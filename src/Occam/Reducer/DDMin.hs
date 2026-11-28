-- | Generic statement delta-debugging and graph-free statement ddmin.
module Occam.Reducer.DDMin (
    CandidateVerdict (..),
    Feedback (..),
    newFeedback,
    CandidateBuilder,
    plainCandidate,
    searchStatementsWithCert,
    searchDDMinWithCert,
    statementDdmin,
    statementDdminWithOrder,
    statementDdminWithParser,
    statementDdminWithCert,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HashSet qualified as HashSet
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Occam.Certificate (
    EarlyRejectCertifier,
    EarlyRejectTracker,
    newEarlyRejectTracker,
 )
import Occam.Core.Oracle (OracleOutcome (..), oracleAccepts)
import Occam.Core.Types (ResultTrace, Statement (..), StatementId (..))
import Occam.Facts (StatementUnit (..))
import Occam.Metrics (FamilyTag (..))
import Occam.Parser.Types (SQLDialect)
import Occam.Pool (
    OraclePool,
    poolMetricsStore,
    poolOracle,
    reportCandidateCount,
    testOrderedGenerated,
 )
import Occam.Reducer.Config (
    StatementLane (..),
    StatementSearchPolicy (..),
 )
import Occam.Reducer.DDMin.Core (
    CandidateBuilder,
    CandidateVerdict (..),
    DDMinEnv (..),
    DDMinOutcome,
    Feedback (..),
    LaneState (..),
    SeedToken (..),
    classifyDDMinSeed,
    commitSeedAcceptance,
    emptyLaneState,
    newFeedback,
    plainCandidate,
    recordDDMinVerdict,
 )
import Occam.Reducer.DDMin.Ranked (buildRankedSchedule)
import Occam.Reducer.Statement (
    cutRemovedStatementInts,
    partitionChunks,
    partitionStatementsByWeights,
    statementIdsInt,
    statementSQL,
    statementSpanBytes,
 )
import Occam.Reducer.Support (
    StatementParser,
    buildCertifierTracked,
    parseStatementsFast,
 )

searchStatementsWithCert ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    Bool ->
    StatementSearchPolicy ->
    CandidateBuilder ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO [Statement]
searchStatementsWithCert tracker cert closureAware policy buildCandidate pool baseSQL allStatements initialStatements = do
    feedback <- newFeedback
    searchDDMinWithCert
        tracker
        cert
        closureAware
        policy
        feedback
        buildCandidate
        pool
        baseSQL
        allStatements
        initialStatements

searchDDMinWithCert ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    Bool ->
    StatementSearchPolicy ->
    Feedback ->
    CandidateBuilder ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO [Statement]
searchDDMinWithCert tracker cert closureAware policy feedback buildCandidate pool baseSQL allStatements initial =
    ddminLoop env 2 emptyLaneState initialLen initial
    where
        effectivePolicy = case policy of
            StatementRankedFallback _ -> StatementByteWeighted
            other -> other
        env =
            DDMinEnv
                { ddTracker = tracker
                , ddCert = cert
                , ddClosureAware = closureAware
                , ddPolicy = effectivePolicy
                , ddFeedback = feedback
                , ddBuildCandidate = buildCandidate
                , ddPool = pool
                , ddBaseSQL = baseSQL
                , ddAllStatements = allStatements
                }
        initialLen = BS.length (cutRemovedStatementInts baseSQL allStatements (statementIdsInt initial))

ddminLoop :: DDMinEnv -> Int -> LaneState -> Int -> [Statement] -> IO [Statement]
ddminLoop _ _ _ _ [] = pure []
ddminLoop env granularity laneState currentLen current =
    tryDDMinSchedule env laneState current currentWeights currentLen schedule
    where
        n = min (length current) (max 1 granularity)
        currentInts = statementIdsInt current
        stmtWithWeights = [(stmt, statementSpanBytes stmt) | stmt <- current]
        currentWeights = IntMap.fromList [(unStatementId (stmtId stmt), w) | (stmt, w) <- stmtWithWeights]
        totalSpanBytes = sum (map snd stmtWithWeights)
        chunks count = case ddPolicy env of
            StatementByteWeighted -> partitionStatementsByWeights count stmtWithWeights totalSpanBytes
            StatementRankedWith _ -> partitionStatementsByWeights count stmtWithWeights totalSpanBytes
            _ -> partitionChunks count current
        lanes count =
            let groups = chunks count
                resume = ddPolicy env == StatementByteWeighted
                orderedGroups
                    | resume
                    , Just (anchor, _) <- lsResumeAnchor laneState =
                        -- Keep every candidate in the pass, wrapping back to earlier groups.
                        let (before, after) = span (all ((<= anchor) . stmtId)) groups
                         in after ++ before
                    | otherwise = groups
             in ( (2, False, tryDDMinSubsets env current currentInts currentLen groups)
                , (max 2 (count - 1), resume, tryDDMinComplements env current currentInts currentLen orderedGroups)
                )
        counts = ddminGranularities (length current) n
        schedule = case ddPolicy env of
            StatementRankedWith cfg ->
                buildRankedSchedule
                    env
                    cfg
                    laneState
                    current
                    currentInts
                    currentWeights
                    currentLen
                    (map chunks counts)
            StatementSubsetFirst ->
                concatMap (\count -> let (subset, complement) = lanes count in [subset, complement]) counts
            _ ->
                concatMap (\count -> let (subset, complement) = lanes count in [complement, subset]) counts

ddminGranularities :: Int -> Int -> [Int]
ddminGranularities limit n
    | n < limit = n : ddminGranularities limit (min limit (2 * n))
    | otherwise = [n]

tryDDMinSchedule ::
    DDMinEnv ->
    LaneState ->
    [Statement] ->
    IntMap.IntMap Int ->
    Int ->
    [(Int, Bool, IO DDMinOutcome)] ->
    IO [Statement]
tryDDMinSchedule _ _ current _ _ [] = pure current
tryDDMinSchedule env laneState current currentWeights currentLen ((next, resume, search) : rest) = do
    reduced <- search
    case reduced of
        Nothing -> tryDDMinSchedule env laneState current currentWeights currentLen rest
        Just (acceptedLane, accepted, saved) ->
            ddminLoop env next nextState (currentLen - saved) accepted
            where
                removedInts = statementIdsInt current `IntSet.difference` statementIdsInt accepted
                anchor
                    | resume
                    , Just (removedId, remaining) <- IntSet.minView removedInts
                    , IntSet.null remaining =
                        Just (StatementId removedId, IntMap.findWithDefault 0 removedId currentWeights)
                    | otherwise = Nothing
                nextStreak
                    | lsLastAcceptedLane laneState == Just acceptedLane = lsLaneStreak laneState + 1
                    | otherwise = 1
                nextState =
                    LaneState
                        { lsLastAcceptedLane = Just acceptedLane
                        , lsLaneStreak = nextStreak
                        , lsResumeAnchor = anchor
                        }

tryDDMinSubsets ::
    DDMinEnv -> [Statement] -> IntSet.IntSet -> Int -> [[Statement]] -> IO DDMinOutcome
tryDDMinSubsets env current currentInts currentLen chunks =
    tryDDMinSeeds
        env
        LaneSubset
        TagStatementSubset
        current
        currentInts
        currentLen
        (map statementIdsInt chunks)

tryDDMinComplements ::
    DDMinEnv -> [Statement] -> IntSet.IntSet -> Int -> [[Statement]] -> IO DDMinOutcome
tryDDMinComplements env current currentInts currentLen chunks =
    tryDDMinSeeds
        env
        LaneComplement
        TagStatementComplement
        current
        currentInts
        currentLen
        [currentInts `IntSet.difference` statementIdsInt chunk | chunk <- chunks]

tryDDMinSeeds ::
    DDMinEnv ->
    StatementLane ->
    FamilyTag ->
    [Statement] ->
    IntSet.IntSet ->
    Int ->
    [IntSet.IntSet] ->
    IO DDMinOutcome
tryDDMinSeeds env lane tag current currentInts currentLen seeds = do
    seenProjections <- newIORef HashSet.empty
    pendingBytes <- newIORef HashSet.empty
    cachedAcceptance <- newIORef Nothing
    let pool = ddPool env
        next [] = pure Nothing
        next (seed : rest) = do
            token <- classifyDDMinSeed env (Just seenProjections) tag current currentInts currentLen seed
            case token of
                Nothing -> next rest
                Just (SeedAccepted kept saved) ->
                    writeIORef cachedAcceptance (Just (kept, saved)) >> pure Nothing
                Just (SeedFresh kept count bytes) -> do
                    pending <- readIORef pendingBytes
                    if HashSet.member bytes pending
                        then next rest
                        else do
                            writeIORef pendingBytes (HashSet.insert bytes pending)
                            reportCandidateCount pool bytes count
                            pure (Just ((tag, bytes, kept), rest))
        retire _ kept bytes (outcome, _) = do
            modifyIORef' pendingBytes (HashSet.delete bytes)
            recordDDMinVerdict env bytes outcome
            pure (if outcome == OracleAccepted then Just (kept, currentLen - BS.length bytes) else Nothing)
    tested <- testOrderedGenerated pool next retire Nothing seeds
    accepted <- maybe (readIORef cachedAcceptance) (pure . Just) tested
    traverse (commitSeedAcceptance pool lane tag) accepted

-- | Graph-free statement ddmin over original top-level statement byte spans.
statementDdmin :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
statementDdmin = statementDdminWithOrder StatementSubsetFirst

statementDdminWithOrder ::
    StatementSearchPolicy -> SQLDialect -> OraclePool -> ByteString -> IO ByteString
statementDdminWithOrder policy dialect pool =
    statementDdminWithParser policy (parseStatementsFast (poolMetricsStore pool) dialect) pool

statementDdminWithParser ::
    StatementSearchPolicy -> StatementParser -> OraclePool -> ByteString -> IO ByteString
statementDdminWithParser policy parse pool initialSQL = do
    tracker <- newEarlyRejectTracker
    statementDdminWithCert False tracker Nothing policy parse pool initialSQL

statementDdminWithCert ::
    Bool ->
    EarlyRejectTracker ->
    Maybe ResultTrace ->
    StatementSearchPolicy ->
    StatementParser ->
    OraclePool ->
    ByteString ->
    IO ByteString
statementDdminWithCert earlyReject tracker mTrace policy parse pool initialSQL = do
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    if not initialOK
        then pure initialSQL
        else do
            (_, units) <- parse initialSQL
            let allStatements = map unitStatement units
            cert <- buildCertifierTracked earlyReject tracker initialSQL units mTrace
            reduced <-
                searchStatementsWithCert
                    tracker
                    cert
                    False
                    policy
                    plainCandidate
                    pool
                    initialSQL
                    allStatements
                    allStatements
            pure (statementSQL initialSQL allStatements reduced)

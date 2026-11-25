-- | Interesting-prefix binary search and dependency-guided backward slicing.
module Occam.Reducer.Slice (
    findInterestingPrefix,
    findInterestingPrefixThrough,
    findInterestingPrefixThroughWithCert,
    sliceWithAnalysis,
) where

import Control.Applicative (empty, (<|>))
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Occam.Certificate (
    EarlyRejectCertifier,
    EarlyRejectTracker,
    checkEarlyRejectionBytesTracked,
    checkEarlyRejectionIdsTracked,
    emptyEarlyRejectCertifier,
    newEarlyRejectTracker,
 )
import Occam.Core (
    BarrierScope (GlobalScope),
    ResultStatement (..),
    ScopedBarrier (barrierScope),
    SemanticFacts (factBarriers),
    Span (..),
    Statement (..),
    StatementId (..),
    oracleAccepts,
 )
import Occam.Dependency (DependencyGraph (..))
import Occam.Logger (Logger, logDebug', logInfo')
import Occam.Metrics (recordGeneratedSkipped)
import Occam.Pool (OraclePool, poolMetricsStore, poolOracle, reportCandidateCount)
import Occam.Proposals (buildRelaxedGraph)
import Occam.Reducer.Statement (
    cutRemovedStatementInts,
    statementIdsInt,
    statementSQL,
 )
import Occam.Reducer.Support (
    SQLAnalysis (..),
    hoistMaybe,
    lastMay,
    rejectIfEarly,
    timeStatementBuild,
 )
import Occam.Slicer (backwardSliceFast, backwardSliceManyFast)

-- | Binary search for the shortest interesting prefix of statements.
findInterestingPrefix ::
    OraclePool -> ByteString -> [Statement] -> IO (Maybe (Statement, ByteString, Int), Int)
findInterestingPrefix pool baseSQL statements =
    findInterestingPrefixThrough pool baseSQL statements (length statements)

findInterestingPrefixThrough ::
    OraclePool -> ByteString -> [Statement] -> Int -> IO (Maybe (Statement, ByteString, Int), Int)
findInterestingPrefixThrough pool baseSQL statements limit = do
    tracker <- newEarlyRejectTracker
    findInterestingPrefixThroughWithCert tracker emptyEarlyRejectCertifier pool baseSQL statements limit

findInterestingPrefixThroughWithCert ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    Int ->
    IO (Maybe (Statement, ByteString, Int), Int)
findInterestingPrefixThroughWithCert tracker cert pool baseSQL statements limit =
    go 1 limit Nothing 0
    where
        go !low !high best !tests
            | low > high = pure (best, tests)
            | otherwise = do
                let mid = low + (high - low) `div` 2
                    prefix = take mid statements
                probeInterestingPrefix tracker cert pool baseSQL statements prefix >>= \case
                    Nothing -> go (mid + 1) high best tests
                    Just (True, candidate) ->
                        let nextBest = maybe best (\stmt -> Just (stmt, candidate, mid)) (lastMay prefix)
                         in go low (mid - 1) nextBest (tests + 1)
                    Just (False, _) -> go (mid + 1) high best (tests + 1)

probeInterestingPrefix ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO (Maybe (Bool, ByteString))
probeInterestingPrefix tracker cert pool baseSQL statements prefix = runMaybeT do
    let store = poolMetricsStore pool
    rejectIfEarly store =<< liftIO (checkEarlyRejectionIdsTracked tracker cert (statementIdsInt prefix))
    candidate <- liftIO $ timeStatementBuild store (statementSQL baseSQL statements prefix)
    rejectIfEarly store =<< liftIO (checkEarlyRejectionBytesTracked tracker cert False candidate)
    liftIO do
        reportCandidateCount pool candidate (length prefix)
        ok <- oracleAccepts (poolOracle pool) candidate
        pure (ok, candidate)

data SliceEnv = SliceEnv
    { seTracker :: !EarlyRejectTracker
    , seCert :: !EarlyRejectCertifier
    , seLogger :: !Logger
    , sePool :: !OraclePool
    , seAnalysis :: !SQLAnalysis
    , seSQL :: !ByteString
    }

-- | Slice SQL statements using a precomputed dependency analysis.
sliceWithAnalysis ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    Logger ->
    OraclePool ->
    SQLAnalysis ->
    Maybe ResultStatement ->
    ByteString ->
    IO (ByteString, Maybe SQLAnalysis, Maybe [Statement])
sliceWithAnalysis tracker cert logger pool analysis observedResult sql =
    case seedTarget of
        Nothing -> keepSlice sql Nothing
        Just seedStatement -> do
            let (conservativeRetained, conservativeBytes) = sliceFor graph seedStatement
                useRelaxed =
                    BS.length conservativeBytes >= BS.length sql
                        && graphHasGlobalBarrier graph
                        && isJust resultTarget
                primary
                    | useRelaxed = sliceFor proposalGraph seedStatement
                    | otherwise = (conservativeRetained, conservativeBytes)
                primaryStrategy
                    | useRelaxed = "result-statement-without-global-barriers"
                    | otherwise = seedStrategy
            primaryAccepted <- trySliceSeed env primary
            (seedAccepted, (seedRetained, seedCandidate), strategy) <-
                if useRelaxed && not primaryAccepted
                    then tryRelaxedOpaqueSeed env proposalGraph seedStatement primary
                    else pure (primaryAccepted, primary, primaryStrategy)
            let prefixLimit = case (seedAccepted, resultTarget) of
                    (True, Just (index, _)) -> index
                    _ -> length statements
                initialBest = if seedAccepted then Just (seedCandidate, seedRetained) else Nothing
            better <- tryPrefixSlice env prefixLimit initialBest
            case better of
                Just (result, retained)
                    | Just result /= fmap fst initialBest -> keepSlice result (Just retained)
                _
                    | seedAccepted -> do
                        accepted <- acceptBackwardSlice env strategy seedStatement seedRetained seedCandidate
                        keepSlice accepted (Just seedRetained)
                    | otherwise -> keepSlice sql Nothing
    where
        env = SliceEnv tracker cert logger pool analysis sql
        statements = analyzedStatements analysis
        graph = analyzedGraph analysis
        resultTarget = do
            ResultStatement count index <- observedResult
            -- The engine and frontend can recognize different statement boundaries.
            guard (count == length statements && index >= 0)
            (index,) <$> listToMaybe (drop index statements)
        (seedTarget, seedStrategy) = case resultTarget of
            Just (_, stmt) -> (Just stmt, "result-statement")
            Nothing -> (lastMay statements, "last-statement")
        keepSlice result retained =
            pure (result, if result == sql then Just analysis else Nothing, retained)
        -- Use the relaxed graph only to propose candidate bytes. A global
        -- barrier can carry real effects, so the oracle still decides.
        proposalGraph = buildRelaxedGraph (analyzedSession analysis) statements graph
        sliceFor dependencyGraph target =
            renderSliceUnits sql statements (backwardSliceFast (unStatementId (stmtId target)) dependencyGraph)

renderSliceUnits :: ByteString -> [Statement] -> IntSet.IntSet -> ([Statement], ByteString)
renderSliceUnits sql statements sliceInts =
    ( filter (\s -> IntSet.member (unStatementId (stmtId s)) sliceInts) statements
    , cutRemovedStatementInts sql statements sliceInts
    )

trySliceSeed :: SliceEnv -> ([Statement], ByteString) -> IO Bool
trySliceSeed env (retained, candidateRaw) = fmap (fromMaybe False) . runMaybeT $ do
    let pool = sePool env
        store = poolMetricsStore pool
        keptInts = statementIdsInt retained
    rejectIfEarly store =<< liftIO (checkEarlyRejectionIdsTracked (seTracker env) (seCert env) keptInts)
    candidate <- liftIO $ timeStatementBuild store candidateRaw
    guard (BS.length candidate < BS.length (seSQL env))
    rejectIfEarly store
        =<< liftIO (checkEarlyRejectionBytesTracked (seTracker env) (seCert env) False candidate)
    liftIO do
        reportCandidateCount pool candidate (length retained)
        oracleAccepts (poolOracle pool) candidate

tryRelaxedOpaqueSeed ::
    SliceEnv ->
    DependencyGraph ->
    Statement ->
    ([Statement], ByteString) ->
    IO (Bool, ([Statement], ByteString), String)
tryRelaxedOpaqueSeed env proposalGraph seedStatement primary = do
    let statements = analyzedStatements (seAnalysis env)
        graph = analyzedGraph (seAnalysis env)
        opaqueRoots =
            IntSet.fromList
                [ unStatementId (stmtId stmt)
                | stmt <- statements
                , Just facts <- [Map.lookup (stmtId stmt) (graphFacts graph)]
                , any ((== GlobalScope) . barrierScope) (factBarriers facts)
                ]
        roots = IntSet.insert (unStatementId (stmtId seedStatement)) opaqueRoots
        retainingOpaque =
            renderSliceUnits (seSQL env) statements (backwardSliceManyFast roots proposalGraph)
    accepted <-
        if snd retainingOpaque == snd primary
            then pure False
            else trySliceSeed env retainingOpaque
    pure (accepted, retainingOpaque, "result-statement-with-opaque-roots")

tryPrefixSlice ::
    SliceEnv ->
    Int ->
    Maybe (ByteString, [Statement]) ->
    IO (Maybe (ByteString, [Statement]))
tryPrefixSlice env prefixLimit bestCandidate
    | Just (best, _) <- bestCandidate
    , prefixSlicesCannotImprove (seSQL env) statements prefixLimit (BS.length best) = do
        recordGeneratedSkipped (poolMetricsStore pool) "prefix-search-cannot-improve"
        pure bestCandidate
    | otherwise = fmap (<|> bestCandidate) . runMaybeT $ do
        (maybeTarget, prefixTests) <-
            liftIO $
                findInterestingPrefixThroughWithCert
                    (seTracker env)
                    (seCert env)
                    pool
                    (seSQL env)
                    statements
                    prefixLimit
        (targetStatement, prefixSQL, prefixLength) <- hoistMaybe maybeTarget
        let (retained, candidateRaw) =
                renderSliceUnits
                    (seSQL env)
                    statements
                    (backwardSliceFast (unStatementId (stmtId targetStatement)) (analyzedGraph (seAnalysis env)))
        candidate <- liftIO $ timeStatementBuild (poolMetricsStore pool) candidateRaw
        let bestLen = maybe (BS.length (seSQL env)) (BS.length . fst) bestCandidate
        when (BS.length candidate >= bestLen) do
            liftIO $ recordGeneratedSkipped (poolMetricsStore pool) "prefix-slice-cannot-improve"
            empty
        let reusedPrefixResult = candidate == prefixSQL
        ok <-
            liftIO $
                if reusedPrefixResult
                    then pure True
                    else do
                        reportCandidateCount pool candidate (length retained)
                        oracleAccepts (poolOracle pool) candidate
        liftIO $
            logDebug'
                (seLogger env)
                "reducer"
                "Prefix oracle result considered for backward slice"
                [ ("prefix_tests", toJSON prefixTests)
                , ("prefix_length", toJSON prefixLength)
                , ("prefix_result_reused", toJSON reusedPrefixResult)
                ]
        guard ok
        accepted <-
            liftIO $ acceptBackwardSlice env "first-interesting-prefix" targetStatement retained candidate
        pure (accepted, retained)
    where
        pool = sePool env
        statements = analyzedStatements (seAnalysis env)

-- Every backward slice retains its root and therefore its source bytes through
-- the next statement start. If no root can beat the incumbent, the prefix
-- search cannot contribute an improvement.
prefixSlicesCannotImprove :: ByteString -> [Statement] -> Int -> Int -> Bool
prefixSlicesCannotImprove sql statements limit incumbent =
    and (zipWith (<=) (0 : starts) (starts ++ [BS.length sql]))
        && all (>= incumbent) (take limit (zipWith (-) ends starts))
    where
        starts = map (spanStartByte . stmtSpan) statements
        ends = drop 1 starts ++ [BS.length sql]

acceptBackwardSlice :: SliceEnv -> String -> Statement -> [Statement] -> ByteString -> IO ByteString
acceptBackwardSlice env strategy target retained candidate = do
    let StatementId targetId = stmtId target
    logInfo'
        (seLogger env)
        "reducer"
        "Backward slicing accepted"
        [ ("seed_strategy", toJSON strategy)
        , ("target_statement_id", toJSON targetId)
        , ("statements_before", toJSON (length (analyzedStatements (seAnalysis env))))
        , ("statements_after", toJSON (length retained))
        , ("bytes_before", toJSON (BS.length (seSQL env)))
        , ("bytes_after", toJSON (BS.length candidate))
        ]
    pure candidate

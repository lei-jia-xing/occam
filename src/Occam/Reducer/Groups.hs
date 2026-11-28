-- | Oracle-gated tiered noncontiguous deletion group proposals.
module Occam.Reducer.Groups (
    proposeGroupDeletionsWithAnalysis,
) where

import Control.Applicative (empty)
import Control.Monad (guard, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.HashSet qualified as HashSet
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Occam.Certificate (
    EarlyRejectCertifier,
    EarlyRejectTracker,
    checkEarlyRejectionBytesTracked,
    checkEarlyRejectionIdsTracked,
 )
import Occam.Core.Oracle (OracleOutcome (..))
import Occam.Core.Types (ResultStatement, Statement (..), StatementId (..))
import Occam.Logger (Logger, logInfo')
import Occam.Metrics (FamilyTag (..), recordAcceptedBytesSaved)
import Occam.Pool (
    OraclePool,
    poolMetricsStore,
    reportCandidateCount,
    testOrderedTagged,
 )
import Occam.Proposals (
    DeletionProposalGroup (..),
    GroupProposalPlan (..),
    extractDeletionProposalPlanWithExtraTx,
    isNoncontiguousIn,
 )
import Occam.Reducer.Statement (cutRemovedStatementInts, statementIdsInt)
import Occam.Reducer.Support (
    SQLAnalysis (..),
    recordEarlyRejectSkip,
    rejectIfEarly,
    timeTaggedStatementBuild,
 )

data GroupProposalEnv = GroupProposalEnv
    { gpTracker :: !EarlyRejectTracker
    , gpCert :: !EarlyRejectCertifier
    , gpLogger :: !Logger
    , gpPool :: !OraclePool
    , gpBaseSQL :: !ByteString
    , gpStatements :: ![Statement]
    , gpCurrentInts :: !(IORef IntSet.IntSet)
    , gpCurrentSQL :: !(IORef ByteString)
    , gpSeen :: !(IORef (HashSet.HashSet ByteString))
    }

{- | Probe tiered noncontiguous deletion proposals derived from dependency
components, relation lifecycles, and transaction/savepoint boundaries.
Rejected groups leave their members eligible for contiguous search and audit.
-}
proposeGroupDeletionsWithAnalysis ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    Logger ->
    OraclePool ->
    SQLAnalysis ->
    Maybe ResultStatement ->
    [(T.Text, IntSet.IntSet)] ->
    ByteString ->
    IO ByteString
proposeGroupDeletionsWithAnalysis tracker cert logger pool analysis observedResult extraTxBlocks sql
    | length statements < 2 = pure sql
    | otherwise = do
        currentIntsRef <- newIORef (statementIdsInt statements)
        currentSQLRef <- newIORef sql
        seenRef <- newIORef (HashSet.singleton sql)
        let env = GroupProposalEnv tracker cert logger pool sql statements currentIntsRef currentSQLRef seenRef
            plan =
                extractDeletionProposalPlanWithExtraTx
                    (analyzedSession analysis)
                    statements
                    (analyzedGraph analysis)
                    observedResult
                    extraTxBlocks
        probeProposalTier env 8 False "weak-group" (planWeakGroups plan)
        probeProposalTier env 6 False "weak-tx-block" (planWeakTxBlocks plan)
        probeProposalTier env 3 True "strong-tx-block" (planStrongTxBlocks plan)
        probeProposalTier env 8 False "tx-wrapper" (planTxWrappers plan)
        probeProposalTier env 4 True "indirect-relation" (planIndirectRelations plan)
        readIORef currentSQLRef
    where
        statements = analyzedStatements analysis

tryDeleteProposalGroup :: GroupProposalEnv -> T.Text -> T.Text -> IntSet.IntSet -> IO Bool
tryDeleteProposalGroup env kind label removed = fmap (fromMaybe False) . runMaybeT $ do
    let pool = gpPool env
        store = poolMetricsStore pool
    currentInts <- liftIO $ readIORef (gpCurrentInts env)
    currentSQL <- liftIO $ readIORef (gpCurrentSQL env)
    let effectiveRemoved = removed `IntSet.intersection` currentInts
        keptInts = currentInts `IntSet.difference` effectiveRemoved
    guard (not (IntSet.null effectiveRemoved) && not (IntSet.null keptInts))
    rejectIfEarly store =<< liftIO (checkEarlyRejectionIdsTracked (gpTracker env) (gpCert env) keptInts)
    candidateSQL <-
        liftIO $
            timeTaggedStatementBuild
                store
                TagStatementComplement
                (cutRemovedStatementInts (gpBaseSQL env) (gpStatements env) keptInts)
    seen <- liftIO $ readIORef (gpSeen env)
    guard (BS.length candidateSQL < BS.length currentSQL && not (HashSet.member candidateSQL seen))
    earlyBytes <-
        liftIO $ checkEarlyRejectionBytesTracked (gpTracker env) (gpCert env) False candidateSQL
    for_ earlyBytes \reason -> do
        liftIO do
            recordEarlyRejectSkip store reason
            modifyIORef' (gpSeen env) (HashSet.insert candidateSQL)
        empty
    liftIO $
        testProposalGroupCandidate
            env
            kind
            label
            currentInts
            currentSQL
            effectiveRemoved
            keptInts
            candidateSQL

testProposalGroupCandidate ::
    GroupProposalEnv ->
    T.Text ->
    T.Text ->
    IntSet.IntSet ->
    ByteString ->
    IntSet.IntSet ->
    IntSet.IntSet ->
    ByteString ->
    IO Bool
testProposalGroupCandidate env kind label currentInts currentSQL effectiveRemoved keptInts candidateSQL = do
    let pool = gpPool env
    reportCandidateCount pool candidateSQL (IntSet.size keptInts)
    outcomes <- testOrderedTagged pool [(TagStatementComplement, candidateSQL)]
    case outcomes of
        (OracleAccepted, _) : _ -> do
            let currentStmts =
                    filter (\s -> IntSet.member (unStatementId (stmtId s)) currentInts) (gpStatements env)
                noncontig = isNoncontiguousIn currentStmts effectiveRemoved
                saved = BS.length currentSQL - BS.length candidateSQL
            recordAcceptedBytesSaved (poolMetricsStore pool) TagStatementComplement saved
            writeIORef (gpCurrentInts env) keptInts
            writeIORef (gpCurrentSQL env) candidateSQL
            modifyIORef' (gpSeen env) (HashSet.insert candidateSQL)
            logInfo'
                (gpLogger env)
                "reducer"
                "Dependency group deletion accepted"
                [ ("group_kind", toJSON kind)
                , ("group_label", toJSON label)
                , ("noncontiguous", toJSON noncontig)
                , ("statements_removed", toJSON (IntSet.size effectiveRemoved))
                , ("bytes_saved", toJSON saved)
                ]
            pure True
        _ -> do
            modifyIORef' (gpSeen env) (HashSet.insert candidateSQL)
            pure False

splitGroupsByBytes :: [DeletionProposalGroup] -> ([DeletionProposalGroup], [DeletionProposalGroup])
splitGroupsByBytes [] = ([], [])
splitGroupsByBytes [x] = ([x], [])
splitGroupsByBytes gs =
    let (l, r) = go 0 [] gs
     in if null r then splitAt (length gs `div` 2) gs else (l, r)
    where
        target = max 1 (sum (map proposalBytes gs) `div` 2)
        go _ accGroups [] = (reverse accGroups, [])
        go _ accGroups [x] = (reverse (x : accGroups), [])
        go !acc accGroups (x : xs)
            | acc + proposalBytes x >= target && not (null accGroups) = (reverse accGroups, x : xs)
            | otherwise = go (acc + proposalBytes x) (x : accGroups) xs

probeProposalTier ::
    GroupProposalEnv -> Int -> Bool -> T.Text -> [DeletionProposalGroup] -> IO ()
probeProposalTier env maxMisses stopOnBothHalvesFail tierKind rawGroups = do
    currentInts <- readIORef (gpCurrentInts env)
    let active =
            filter
                (not . IntSet.null . IntSet.intersection currentInts . proposalStatements)
                rawGroups
    case active of
        [] -> pure ()
        [single] -> void (tryDeleteProposalGroup env tierKind (proposalLabel single) (proposalStatements single))
        _ -> do
            let allRemoved = IntSet.unions (map proposalStatements active)
            acceptedAll <- tryDeleteProposalGroup env (tierKind <> "-union") "union" allRemoved
            unless acceptedAll $ do
                missesRef <- newIORef (1 :: Int)
                bisectProposalGroups env missesRef maxMisses stopOnBothHalvesFail tierKind active

bisectProposalGroups ::
    GroupProposalEnv -> IORef Int -> Int -> Bool -> T.Text -> [DeletionProposalGroup] -> IO ()
bisectProposalGroups env missesRef maxMisses stopOnBothHalvesFail tierKind gs = do
    misses <- readIORef missesRef
    when (misses < maxMisses) $ case gs of
        [] -> pure ()
        [g] -> do
            ok <- tryDeleteProposalGroup env tierKind (proposalLabel g) (proposalStatements g)
            recordProposalAttempt missesRef ok
        _ -> bisectSplit gs
    where
        bisectSplit groups = do
            let (leftGs, rightGs) = splitGroupsByBytes groups
                leftSet = IntSet.unions (map proposalStatements leftGs)
                rightSet = IntSet.unions (map proposalStatements rightGs)
            okLeft <- tryDeleteProposalGroup env (tierKind <> "-bisect") "left-half" leftSet
            recordProposalAttempt missesRef okLeft
            if okLeft
                then recurse rightGs
                else tryRightHalf leftGs rightGs rightSet
        tryRightHalf leftGs rightGs rightSet = do
            misses <- readIORef missesRef
            when (misses < maxMisses) $ do
                okRight <- tryDeleteProposalGroup env (tierKind <> "-bisect") "right-half" rightSet
                recordProposalAttempt missesRef okRight
                if okRight
                    then recurse leftGs
                    else unless stopOnBothHalvesFail $ do
                        when (length leftGs > 1) (recurse leftGs)
                        when (length rightGs > 1) (recurse rightGs)
        recurse = bisectProposalGroups env missesRef maxMisses stopOnBothHalvesFail tierKind

recordProposalAttempt :: IORef Int -> Bool -> IO ()
recordProposalAttempt missesRef True = writeIORef missesRef 0
recordProposalAttempt missesRef False = modifyIORef' missesRef (+ 1)

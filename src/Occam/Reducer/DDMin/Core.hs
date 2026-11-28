-- | Shared DDMin environment, memoized verdicts, lane state, and seed classification.
module Occam.Reducer.DDMin.Core (
    CandidateBuilder,
    CandidateVerdict (..),
    DDMinEnv (..),
    DDMinOutcome,
    Feedback (..),
    LaneState (..),
    SeedToken (..),
    checkAndInsertSeenProjection,
    classifyDDMinSeed,
    commitSeedAcceptance,
    dedupIntSets,
    emptyLaneState,
    newFeedback,
    plainCandidate,
    recordDDMinVerdict,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (evalContT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntSet qualified as IntSet
import Occam.Certificate (
    EarlyRejectCertifier,
    EarlyRejectTracker,
    checkEarlyRejectionBytesTracked,
    checkEarlyRejectionIdsTracked,
 )
import Occam.Core.Oracle (OracleOutcome (..))
import Occam.Core.Types (Statement (..), StatementId (..))
import Occam.Metrics (
    FamilyTag (..),
    recordAcceptedBytesSaved,
    recordGeneratedSkipped,
 )
import Occam.Pool (
    OraclePool,
    poolMetricsStore,
 )
import Occam.Reducer.Config (
    StatementLane (..),
    StatementSearchPolicy (..),
 )
import Occam.Reducer.Statement (cutRemovedStatementInts)
import Occam.Reducer.Support (
    earlyExit,
    recordEarlyRejectSkip,
    timeTaggedStatementBuild,
 )

data CandidateVerdict
    = CandidateAccepted
    | CandidateRejected
    deriving (Eq, Show)

-- | Verdict memo keyed by exact candidate bytes.
newtype Feedback = Feedback
    { fbEvaluated :: IORef (HashMap ByteString CandidateVerdict)
    }

newFeedback :: IO Feedback
newFeedback = Feedback <$> newIORef HashMap.empty

type CandidateBuilder = IntSet.IntSet -> IntSet.IntSet -> IO (Maybe IntSet.IntSet)

plainCandidate :: CandidateBuilder
plainCandidate _ keptInts = pure (Just keptInts)

-- | Classified seeds: previously accepted seeds commit; fresh seeds test in order.
data SeedToken
    = SeedAccepted ![Statement] !Int
    | SeedFresh [Statement] !Int !ByteString

-- | Per-pass adaptive scheduling state carried across accepted reductions.
data LaneState = LaneState
    { lsLastAcceptedLane :: !(Maybe StatementLane)
    , lsLaneStreak :: !Int
    , lsResumeAnchor :: !(Maybe (StatementId, Int))
    }

emptyLaneState :: LaneState
emptyLaneState = LaneState Nothing 0 Nothing

data DDMinEnv = DDMinEnv
    { ddTracker :: !EarlyRejectTracker
    , ddCert :: !EarlyRejectCertifier
    , ddClosureAware :: !Bool
    , ddPolicy :: !StatementSearchPolicy
    , ddFeedback :: !Feedback
    , ddBuildCandidate :: !CandidateBuilder
    , ddPool :: !OraclePool
    , ddBaseSQL :: !ByteString
    , ddAllStatements :: ![Statement]
    }

-- | Result yielded by a DDMin scheduling pass: winning lane, surviving statements, and bytes saved.
type DDMinOutcome = Maybe (StatementLane, [Statement], Int)

-- | Deduplicate a list of 'IntSet's while preserving first-seen order.
dedupIntSets :: [IntSet.IntSet] -> [IntSet.IntSet]
dedupIntSets = go HashSet.empty
    where
        go _ [] = []
        go seen (s : rest)
            | HashSet.member s seen = go seen rest
            | otherwise = s : go (HashSet.insert s seen) rest

-- | Record saved-byte telemetry when accepting an already-validated seed candidate.
commitSeedAcceptance ::
    OraclePool ->
    StatementLane ->
    FamilyTag ->
    ([Statement], Int) ->
    IO (StatementLane, [Statement], Int)
commitSeedAcceptance pool lane tag (kept, saved) = do
    recordAcceptedBytesSaved (poolMetricsStore pool) tag saved
    pure (lane, kept, saved)

-- | Memoize an oracle outcome for @sql@ in the DDMin run's feedback table.
recordDDMinVerdict :: DDMinEnv -> ByteString -> OracleOutcome -> IO ()
recordDDMinVerdict env sql outcome =
    modifyIORef' (fbEvaluated (ddFeedback env)) (HashMap.insert sql verdict)
    where
        verdict = if outcome == OracleAccepted then CandidateAccepted else CandidateRejected

-- | Project, early-reject, deduplicate, and classify one candidate seed before oracle testing.
classifyDDMinSeed ::
    DDMinEnv ->
    Maybe (IORef (HashSet.HashSet IntSet.IntSet)) ->
    FamilyTag ->
    [Statement] ->
    IntSet.IntSet ->
    Int ->
    IntSet.IntSet ->
    IO (Maybe SeedToken)
classifyDDMinSeed env maybeSeen tag current currentInts currentLen seedInts
    | seedInts == currentInts = pure Nothing
    | otherwise = evalContT do
        let pool = ddPool env
            store = poolMetricsStore pool
            feedback = ddFeedback env
        projection <- liftIO $ ddBuildCandidate env currentInts seedInts
        projected <- case projection of
            Nothing -> earlyExit Nothing
            Just p
                | p == currentInts -> do
                    liftIO $ recordGeneratedSkipped store "closure-projection-no-shrink"
                    earlyExit Nothing
                | otherwise -> pure p
        dupInWindow <- liftIO $ checkAndInsertSeenProjection maybeSeen projected
        when dupInWindow do
            liftIO do
                recordGeneratedSkipped store "projected-window-duplicate"
                when (projected /= seedInts) $
                    recordGeneratedSkipped store "closure-projection-duplicate"
            earlyExit Nothing

        earlyId <- liftIO $ checkEarlyRejectionIdsTracked (ddTracker env) (ddCert env) projected
        for_ earlyId \reason -> do
            liftIO $ recordEarlyRejectSkip store reason
            earlyExit Nothing

        let kept = filter (\s -> IntSet.member (unStatementId (stmtId s)) projected) current
        candidateSQL <-
            liftIO $
                timeTaggedStatementBuild
                    store
                    tag
                    (cutRemovedStatementInts (ddBaseSQL env) (ddAllStatements env) projected)
        let saved = currentLen - BS.length candidateSQL
        when (saved <= 0) do
            liftIO $
                when (projected /= seedInts) $
                    recordGeneratedSkipped store "closure-projection-no-shrink"
            earlyExit Nothing

        evaluated <- liftIO $ readIORef (fbEvaluated feedback)
        case HashMap.lookup candidateSQL evaluated of
            Just CandidateAccepted -> earlyExit (Just (SeedAccepted kept saved))
            Just CandidateRejected -> do
                liftIO do
                    recordGeneratedSkipped store "duplicate-bytes"
                    when (projected /= seedInts) $
                        recordGeneratedSkipped store "closure-projection-duplicate"
                earlyExit Nothing
            Nothing -> pure ()

        earlyBytes <-
            liftIO $ checkEarlyRejectionBytesTracked (ddTracker env) (ddCert env) False candidateSQL
        for_ earlyBytes \reason -> do
            liftIO do
                recordEarlyRejectSkip store reason
                modifyIORef' (fbEvaluated feedback) (HashMap.insert candidateSQL CandidateRejected)
            earlyExit Nothing

        let count = IntSet.size (IntSet.intersection currentInts projected)
        pure (Just (SeedFresh kept count candidateSQL))

-- | Atomically check and record @projected@ in a per-window seen set to avoid duplicate projection tests.
checkAndInsertSeenProjection ::
    Maybe (IORef (HashSet.HashSet IntSet.IntSet)) -> IntSet.IntSet -> IO Bool
checkAndInsertSeenProjection Nothing _ = pure False
checkAndInsertSeenProjection (Just seenRef) projected = do
    seen <- readIORef seenRef
    if HashSet.member projected seen
        then pure True
        else writeIORef seenRef (HashSet.insert projected seen) >> pure False

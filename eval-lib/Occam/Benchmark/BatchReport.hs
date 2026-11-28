{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Typed aggregation and decision-log summaries for Occam batch evaluations.
module Occam.Benchmark.BatchReport (
    AggregatedReport (..),
    BatchJobResult (..),
    BatchOverview (..),
    BatchRunRecord (..),
    DecisionSummary (..),
    GroupSummary (..),
    MetricSummary (..),
    MetricsSubset (..),
    ReplayVerification (..),
    SignatureGroupSummary (..),
    SignatureProfileStratified (..),
    StatsEvalMetrics (..),
    StatsSnapshot (..),
    aggregateGroup,
    aggregateOutDir,
    aggregateReport,
    collectRunRecord,
    collectRunRecordIO,
    metricSummary,
    summarizeDecisionLog,
    summarizeDecisionLogBytes,
) where

import Control.Exception.Safe (IOException, try)
import Data.Aeson (
    FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    eitherDecodeStrict',
    encode,
    object,
    withObject,
    (.!=),
    (.:?),
    (.=),
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable qualified as Foldable
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Occam.AtomicFile (writeAtomicLazyBytes)
import Occam.Benchmark.Metrics (computeAttemptAuc, engineOverheadMicros)
import Occam.Evaluation (EvaluationPoint (..))
import Occam.Harness.Checkpoints (
    BatchJobResult (..),
    DecisionSummary (..),
    JobState (..),
    emptyDecisionSummary,
    readJsonIfExists,
 )
import Occam.Harness.Replay (QualificationResult (..), ReplayVerification (..))
import Occam.Harness.Signature (CrashSignature (..), categoryName)
import Occam.Metrics (MetricsSnapshot)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    makeAbsolute,
 )
import System.FilePath ((</>))

-- | Non-empty summary statistics over numeric values.
data MetricSummary = MetricSummary
    { msCount :: !Int
    , msMedian :: !Double
    , msMean :: !Double
    , msMin :: !Double
    , msMax :: !Double
    }
    deriving (Eq, Show)

instance ToJSON MetricSummary where
    toJSON ms =
        object
            [ "n" .= msCount ms
            , "median" .= msMedian ms
            , "mean" .= msMean ms
            , "min" .= msMin ms
            , "max" .= msMax ms
            ]

instance FromJSON MetricSummary where
    parseJSON = withObject "MetricSummary" \obj ->
        MetricSummary
            <$> obj .:? "n" .!= 0
            <*> obj .:? "median" .!= 0.0
            <*> obj .:? "mean" .!= 0.0
            <*> obj .:? "min" .!= 0.0
            <*> obj .:? "max" .!= 0.0

-- | Lenient @evaluation_curve@ entry supporting partial test fixtures and legacy camelCase.
data CurvePointInput = CurvePointInput
    { elapsedSeconds :: !Double
    , callIndex :: !Int
    , candidateBytes :: !(Maybe Int)
    , candidateStatements :: !(Maybe Int)
    , cacheHit :: !Bool
    , accepted :: !Bool
    }
    deriving (Eq, Show)

instance FromJSON CurvePointInput where
    parseJSON = withObject "CurvePointInput" \obj ->
        CurvePointInput
            <$> fmap (fromMaybe 0.0) (lookupField obj "point_elapsed_seconds" "pointElapsedSeconds")
            <*> fmap (fromMaybe 0) (lookupField obj "point_call_index" "pointCallIndex")
            <*> lookupField obj "point_candidate_bytes" "pointCandidateBytes"
            <*> lookupField obj "point_candidate_statements" "pointCandidateStatements"
            <*> fmap (fromMaybe False) (lookupField obj "point_cache_hit" "pointCacheHit")
            <*> fmap (fromMaybe False) (lookupField obj "point_accepted" "pointAccepted")

toEvaluationPoint :: Int -> CurvePointInput -> EvaluationPoint
toEvaluationPoint fallbackBytes pt =
    EvaluationPoint
        { elapsedSeconds = pt.elapsedSeconds
        , callIndex = pt.callIndex
        , candidateBytes = fromMaybe fallbackBytes pt.candidateBytes
        , candidateStatements = pt.candidateStatements
        , cacheHit = pt.cacheHit
        , accepted = pt.accepted
        }

-- | Lenient subset of @EvaluationMetrics@ needed for batch reporting.
data StatsEvalMetrics = StatsEvalMetrics
    { semOracleSeconds :: !(Maybe Double)
    , semResetSeconds :: !(Maybe Double)
    , semAuditSeconds :: !(Maybe Double)
    , semValidationSeconds :: !(Maybe Double)
    , semValidationPassed :: !(Maybe Bool)
    , semMinimalityClaim :: !(Maybe Text)
    , semCurve :: ![CurvePointInput]
    , semCostAttribution :: !(Maybe MetricsSnapshot)
    }
    deriving (Eq, Show)

instance FromJSON StatsEvalMetrics where
    parseJSON = withObject "StatsEvalMetrics" \obj -> do
        mCurve <- lookupField obj "evaluation_curve" "evaluationCurve"
        mCost <- lookupField obj "evaluation_cost_attribution" "evaluationCostAttribution"
        StatsEvalMetrics
            <$> lookupField obj "evaluation_oracle_seconds" "evaluationOracleSeconds"
            <*> lookupField obj "evaluation_reset_seconds" "evaluationResetSeconds"
            <*> lookupField obj "evaluation_audit_seconds" "evaluationAuditSeconds"
            <*> lookupField obj "evaluation_validation_seconds" "evaluationValidationSeconds"
            <*> lookupField obj "evaluation_validation_passed" "evaluationValidationPassed"
            <*> lookupField obj "evaluation_minimality_claim" "evaluationMinimalityClaim"
            <*> pure (fromMaybe [] mCurve)
            <*> pure mCost

-- | Lenient subset of Occam @stats.json@ supporting both full runs and minimal test fixtures.
data StatsSnapshot = StatsSnapshot
    { stStatus :: !(Maybe Text)
    , stInitialBytes :: !(Maybe Int)
    , stReducedBytes :: !(Maybe Int)
    , stReductionRatio :: !(Maybe Double)
    , stInitialStatements :: !(Maybe Int)
    , stReducedStatements :: !(Maybe Int)
    , stOracleExecutions :: !(Maybe Int)
    , stOracleAttempts :: !(Maybe Int)
    , stCacheHits :: !(Maybe Int)
    , stCacheMisses :: !(Maybe Int)
    , stCacheJoins :: !(Maybe Int)
    , stCacheHitRatio :: !(Maybe Double)
    , stCancelledObservations :: !(Maybe Int)
    , stElapsedSeconds :: !(Maybe Double)
    , stEvaluationBudget :: !(Maybe Object)
    , stEvaluationMetrics :: !(Maybe StatsEvalMetrics)
    }
    deriving (Eq, Show)

instance FromJSON StatsSnapshot where
    parseJSON = withObject "StatsSnapshot" \obj ->
        StatsSnapshot
            <$> obj .:? "status"
            <*> obj .:? "initial_bytes"
            <*> obj .:? "reduced_bytes"
            <*> obj .:? "reduction_ratio"
            <*> obj .:? "initial_statements"
            <*> obj .:? "reduced_statements"
            <*> obj .:? "oracle_executions"
            <*> obj .:? "oracle_attempts"
            <*> obj .:? "cache_hits"
            <*> obj .:? "cache_misses"
            <*> obj .:? "cache_joins"
            <*> obj .:? "cache_hit_ratio"
            <*> obj .:? "cancelled_observations"
            <*> obj .:? "elapsed_seconds"
            <*> obj .:? "evaluation_budget"
            <*> obj .:? "evaluation_metrics"

-- | Normalized per-run record emitted inside @aggregated_stats.json@.
data BatchRunRecord = BatchRunRecord
    { recJobId :: !Text
    , recCaseId :: !Text
    , recDbms :: !Text
    , recDialect :: !Text
    , recProfile :: !Text
    , recRepeat :: !Int
    , recJobState :: !JobState
    , recStatus :: !(Maybe Text)
    , recIsCompleted :: !Bool
    , recIsCensored :: !Bool
    , recInputQualified :: !Bool
    , recBaselineSignature :: !(Maybe CrashSignature)
    , recStratificationSignature :: !Text
    , recInitialBytes :: !(Maybe Int)
    , recReducedBytes :: !(Maybe Int)
    , recReductionRatio :: !(Maybe Double)
    , recInitialStatements :: !(Maybe Int)
    , recReducedStatements :: !(Maybe Int)
    , recStatementReductionRatio :: !(Maybe Double)
    , recOracleExecutions :: !(Maybe Int)
    , recOracleAttempts :: !(Maybe Int)
    , recCacheHits :: !(Maybe Int)
    , recCacheMisses :: !(Maybe Int)
    , recCacheJoins :: !(Maybe Int)
    , recCacheHitRatio :: !(Maybe Double)
    , recCancelledObservations :: !(Maybe Int)
    , recElapsedSeconds :: !(Maybe Double)
    , recOracleSeconds :: !(Maybe Double)
    , recResetSeconds :: !(Maybe Double)
    , recAuditSeconds :: !(Maybe Double)
    , recValidationSeconds :: !(Maybe Double)
    , recValidationPassed :: !(Maybe Bool)
    , recMinimalityClaim :: !(Maybe Text)
    , recAttemptAuc :: !(Maybe Double)
    , recEngineOverheadUs :: !(Maybe Int)
    , recEvaluationJobs :: !(Maybe Int)
    , recReducedReplayRunsRequired :: !Int
    , recReducedReplayRunsPassed :: !(Maybe Int)
    , recReducedReplayAllPassed :: !(Maybe Bool)
    , recReducedReplayConsistentSignature :: !(Maybe Bool)
    , recReducedReplayMatchesBaseline :: !(Maybe Bool)
    , recReducedReplaySlippageCount :: !(Maybe Int)
    , recReducedReplayObservedSignatures :: !(Maybe [Text])
    , recOracleDecisionsSummary :: !(Maybe DecisionSummary)
    }
    deriving (Eq, Show)

instance ToJSON BatchRunRecord where
    toJSON r =
        object
            [ "job_id" .= recJobId r
            , "case_id" .= recCaseId r
            , "dbms" .= recDbms r
            , "dialect" .= recDialect r
            , "profile" .= recProfile r
            , "repeat" .= recRepeat r
            , "job_state" .= recJobState r
            , "status" .= recStatus r
            , "is_completed" .= recIsCompleted r
            , "is_censored" .= recIsCensored r
            , "input_qualified" .= recInputQualified r
            , "baseline_signature" .= recBaselineSignature r
            , "stratification_signature" .= recStratificationSignature r
            , "initial_bytes" .= recInitialBytes r
            , "reduced_bytes" .= recReducedBytes r
            , "reduction_ratio" .= recReductionRatio r
            , "initial_statements" .= recInitialStatements r
            , "reduced_statements" .= recReducedStatements r
            , "statement_reduction_ratio" .= recStatementReductionRatio r
            , "oracle_executions" .= recOracleExecutions r
            , "oracle_attempts" .= recOracleAttempts r
            , "cache_hits" .= recCacheHits r
            , "cache_misses" .= recCacheMisses r
            , "cache_joins" .= recCacheJoins r
            , "cache_hit_ratio" .= recCacheHitRatio r
            , "cancelled_observations" .= recCancelledObservations r
            , "elapsed_seconds" .= recElapsedSeconds r
            , "oracle_seconds" .= recOracleSeconds r
            , "reset_seconds" .= recResetSeconds r
            , "audit_seconds" .= recAuditSeconds r
            , "validation_seconds" .= recValidationSeconds r
            , "validation_passed" .= recValidationPassed r
            , "minimality_claim" .= recMinimalityClaim r
            , "attempt_auc" .= recAttemptAuc r
            , "engine_overhead_us" .= recEngineOverheadUs r
            , "evaluation_jobs" .= recEvaluationJobs r
            , "reduced_replay_runs_required" .= recReducedReplayRunsRequired r
            , "reduced_replay_runs_passed" .= recReducedReplayRunsPassed r
            , "reduced_replay_all_passed" .= recReducedReplayAllPassed r
            , "reduced_replay_consistent_signature" .= recReducedReplayConsistentSignature r
            , "reduced_replay_matches_baseline" .= recReducedReplayMatchesBaseline r
            , "reduced_replay_slippage_count" .= recReducedReplaySlippageCount r
            , "reduced_replay_observed_signatures" .= recReducedReplayObservedSignatures r
            , "oracle_decisions_summary" .= recOracleDecisionsSummary r
            ]

-- | Distribution summaries for one subset of runs (e.g. @completed_only@ or @all_with_checkpoints@).
data MetricsSubset = MetricsSubset
    { subCount :: !Int
    , subReducedBytes :: !(Maybe MetricSummary)
    , subReductionRatio :: !(Maybe MetricSummary)
    , subReducedStatements :: !(Maybe MetricSummary)
    , subStatementReductionRatio :: !(Maybe MetricSummary)
    , subOracleExecutions :: !(Maybe MetricSummary)
    , subOracleAttempts :: !(Maybe MetricSummary)
    , subCacheHitRatio :: !(Maybe MetricSummary)
    , subElapsedSeconds :: !(Maybe MetricSummary)
    , subOracleSeconds :: !(Maybe MetricSummary)
    , subResetSeconds :: !(Maybe MetricSummary)
    , subAuditSeconds :: !(Maybe MetricSummary)
    , subValidationSeconds :: !(Maybe MetricSummary)
    , subAttemptAuc :: !(Maybe MetricSummary)
    , subEngineOverheadUs :: !(Maybe MetricSummary)
    }
    deriving (Eq, Show)

instance ToJSON MetricsSubset where
    toJSON s =
        object
            [ "count" .= subCount s
            , "reduced_bytes" .= subReducedBytes s
            , "reduction_ratio" .= subReductionRatio s
            , "reduced_statements" .= subReducedStatements s
            , "statement_reduction_ratio" .= subStatementReductionRatio s
            , "oracle_executions" .= subOracleExecutions s
            , "oracle_attempts" .= subOracleAttempts s
            , "cache_hit_ratio" .= subCacheHitRatio s
            , "elapsed_seconds" .= subElapsedSeconds s
            , "oracle_seconds" .= subOracleSeconds s
            , "reset_seconds" .= subResetSeconds s
            , "audit_seconds" .= subAuditSeconds s
            , "validation_seconds" .= subValidationSeconds s
            , "attempt_auc" .= subAttemptAuc s
            , "engine_overhead_us" .= subEngineOverheadUs s
            ]

-- | Aggregate statistics across a group of runs (profile, DBMS, or signature slice).
data GroupSummary = GroupSummary
    { gsTotalRuns :: !Int
    , gsCompletedRuns :: !Int
    , gsCensoredRuns :: !Int
    , gsErrorOrFailedRuns :: !Int
    , gsSkippedUnstableRuns :: !Int
    , gsValidationPassedRuns :: !Int
    , gsMinimality1MinimalRuns :: !Int
    , gsReducedReplayCheckedRuns :: !Int
    , gsReducedReplay5of5Runs :: !Int
    , gsReducedReplayZeroSlippageRuns :: !Int
    , gsReducedReplaySlippedRuns :: !Int
    , gsCompletedOnly :: !MetricsSubset
    , gsAllWithCheckpoints :: !MetricsSubset
    }
    deriving (Eq, Show)

instance ToJSON GroupSummary where
    toJSON g =
        object
            [ "total_runs" .= gsTotalRuns g
            , "completed_runs" .= gsCompletedRuns g
            , "censored_runs" .= gsCensoredRuns g
            , "error_or_failed_runs" .= gsErrorOrFailedRuns g
            , "skipped_unstable_runs" .= gsSkippedUnstableRuns g
            , "validation_passed_runs" .= gsValidationPassedRuns g
            , "minimality_1_minimal_runs" .= gsMinimality1MinimalRuns g
            , "reduced_replay_checked_runs" .= gsReducedReplayCheckedRuns g
            , "reduced_replay_5of5_runs" .= gsReducedReplay5of5Runs g
            , "reduced_replay_zero_slippage_runs" .= gsReducedReplayZeroSlippageRuns g
            , "reduced_replay_slipped_runs" .= gsReducedReplaySlippedRuns g
            , "completed_only" .= gsCompletedOnly g
            , "all_with_checkpoints" .= gsAllWithCheckpoints g
            ]

-- | Stratified summary for one unique bug signature across profiles.
data SignatureGroupSummary = SignatureGroupSummary
    { sgsSignatureId :: !(Maybe Text)
    , sgsSignatureHash :: !(Maybe Text)
    , sgsCategory :: !(Maybe Text)
    , sgsDbms :: !(Maybe Text)
    , sgsCaseIds :: ![Text]
    , sgsCaseCount :: !Int
    , sgsOverall :: !GroupSummary
    , sgsByProfile :: !(Map Text GroupSummary)
    }
    deriving (Eq, Show)

instance ToJSON SignatureGroupSummary where
    toJSON s =
        object
            [ "signature_id" .= sgsSignatureId s
            , "signature_hash" .= sgsSignatureHash s
            , "category" .= sgsCategory s
            , "dbms" .= sgsDbms s
            , "case_ids" .= sgsCaseIds s
            , "case_count" .= sgsCaseCount s
            , "overall" .= sgsOverall s
            , "by_profile" .= sgsByProfile s
            ]

-- | Macro-summary across unique signatures for a single reduction profile.
data SignatureProfileStratified = SignatureProfileStratified
    { spsSignaturesObserved :: !Int
    , spsSignaturesWithCompletedRuns :: !Int
    , spsSignaturesZeroSlippageAllRuns :: !Int
    , spsCompletedSignatureMedianReductionRatio :: !(Maybe MetricSummary)
    , spsCompletedSignatureMedianReducedBytes :: !(Maybe MetricSummary)
    , spsCompletedSignatureMedianOracleAttempts :: !(Maybe MetricSummary)
    , spsCompletedSignatureMedianAttemptAuc :: !(Maybe MetricSummary)
    , spsCompletedSignatureMedianElapsedSeconds :: !(Maybe MetricSummary)
    }
    deriving (Eq, Show)

instance ToJSON SignatureProfileStratified where
    toJSON s =
        object
            [ "signatures_observed" .= spsSignaturesObserved s
            , "signatures_with_completed_runs" .= spsSignaturesWithCompletedRuns s
            , "signatures_zero_slippage_all_runs" .= spsSignaturesZeroSlippageAllRuns s
            , "completed_signature_median_reduction_ratio" .= spsCompletedSignatureMedianReductionRatio s
            , "completed_signature_median_reduced_bytes" .= spsCompletedSignatureMedianReducedBytes s
            , "completed_signature_median_oracle_attempts" .= spsCompletedSignatureMedianOracleAttempts s
            , "completed_signature_median_attempt_auc" .= spsCompletedSignatureMedianAttemptAuc s
            , "completed_signature_median_elapsed_seconds" .= spsCompletedSignatureMedianElapsedSeconds s
            ]

-- | Corpus-wide qualification and run counts.
data BatchOverview = BatchOverview
    { ovCasesInRuns :: !Int
    , ovQualifiedCasesEvaluated :: !Int
    , ovInputStableCases :: !Int
    , ovInputUnstableCases :: !Int
    , ovUniqueQualifiedCrashSignatures :: !Int
    , ovTotalJobRuns :: !Int
    , ovUniqueStratificationGroups :: !Int
    }
    deriving (Eq, Show)

instance ToJSON BatchOverview where
    toJSON ov =
        object
            [ "cases_in_runs" .= ovCasesInRuns ov
            , "qualified_cases_evaluated" .= ovQualifiedCasesEvaluated ov
            , "input_stable_cases" .= ovInputStableCases ov
            , "input_unstable_cases" .= ovInputUnstableCases ov
            , "unique_qualified_crash_signatures" .= ovUniqueQualifiedCrashSignatures ov
            , "total_job_runs" .= ovTotalJobRuns ov
            , "unique_stratification_groups" .= ovUniqueStratificationGroups ov
            ]

-- | Top-level batch evaluation report persisted to @aggregated_stats.json@.
data AggregatedReport = AggregatedReport
    { aggSchemaVersion :: !Int
    , aggGeneratedAt :: !Text
    , aggOutDir :: !FilePath
    , aggOverview :: !BatchOverview
    , aggQualifications :: !(Map Text QualificationResult)
    , aggByProfile :: !(Map Text GroupSummary)
    , aggByDbms :: !(Map Text GroupSummary)
    , aggBySignature :: !(Map Text SignatureGroupSummary)
    , aggSignatureStratifiedByProfile :: !(Map Text SignatureProfileStratified)
    , aggRuns :: ![BatchRunRecord]
    }
    deriving (Eq, Show)

instance ToJSON AggregatedReport where
    toJSON agg =
        object
            [ "schema_version" .= aggSchemaVersion agg
            , "generated_at" .= aggGeneratedAt agg
            , "out_dir" .= aggOutDir agg
            , "overview" .= aggOverview agg
            , "qualifications" .= aggQualifications agg
            , "by_profile" .= aggByProfile agg
            , "by_dbms" .= aggByDbms agg
            , "by_signature" .= aggBySignature agg
            , "signature_stratified_by_profile" .= aggSignatureStratifiedByProfile agg
            , "runs" .= aggRuns agg
            ]

lookupField :: (FromJSON a) => Object -> Text -> Text -> Parser (Maybe a)
lookupField obj snake camel = do
    mSnake <- obj .:? Key.fromText snake
    case mSnake of
        Just val -> pure (Just val)
        Nothing -> obj .:? Key.fromText camel

-- | Read and summarize an @oracle_decisions.jsonl@ log file.
summarizeDecisionLog :: FilePath -> IO DecisionSummary
summarizeDecisionLog path
    | null path = pure emptyDecisionSummary
    | otherwise = do
        exists <- doesFileExist path
        if not exists
            then pure emptyDecisionSummary
            else do
                res <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
                pure (either (const emptyDecisionSummary) summarizeDecisionLogBytes res)

-- | Purely summarize JSONL bytes from @oracle_decisions.jsonl@.
summarizeDecisionLogBytes :: BS.ByteString -> DecisionSummary
summarizeDecisionLogBytes raw =
    let rawLines = T.lines (TE.decodeUtf8With TEE.lenientDecode raw)
        (ds, revSigs, _) = Foldable.foldl' step (emptyDecisionSummary, [], Set.empty) rawLines
     in ds {dsUniqueFaultSignatures = reverse revSigs}
    where
        step acc@(ds, revSigs, seen) rawLine =
            let stripped = T.strip rawLine
             in if T.null stripped
                    then acc
                    else case eitherDecodeStrict' (TE.encodeUtf8 stripped) :: Either String Object of
                        Left _ -> acc
                        Right rec ->
                            let totalCalls = dsTotalCalls ds + 1
                                acceptedDelta = if kmText rec "verdict" == Just "accepted" then 1 else 0
                                (cfDelta, hnfDelta, meDelta) = case kmText rec "classification" of
                                    Just "confirmed_fault" -> (1, 0, 0)
                                    Just "healthy_no_fault" -> (0, 1, 0)
                                    Just "missing_evidence" -> (0, 0, 1)
                                    _ -> (0, 0, 0)
                                slipDelta = case KM.lookup "slippage" rec of
                                    Just (Object sObj) | KM.lookup "slipped" sObj == Just (Bool True) -> 1
                                    _ -> 0
                                (revSigs', seen') = case KM.lookup "crash_signature" rec of
                                    Just (Object cObj) -> case kmText cObj "id" of
                                        Just sid
                                            | not (T.null sid) && Set.notMember sid seen ->
                                                (sid : revSigs, Set.insert sid seen)
                                        _ -> (revSigs, seen)
                                    _ -> (revSigs, seen)
                                ds' =
                                    ds
                                        { dsTotalCalls = totalCalls
                                        , dsAcceptedCalls = dsAcceptedCalls ds + acceptedDelta
                                        , dsConfirmedFaults = dsConfirmedFaults ds + cfDelta
                                        , dsHealthyNoFault = dsHealthyNoFault ds + hnfDelta
                                        , dsMissingEvidence = dsMissingEvidence ds + meDelta
                                        , dsSlippageEvents = dsSlippageEvents ds + slipDelta
                                        }
                             in (ds', revSigs', seen')

kmText :: Object -> Key.Key -> Maybe Text
kmText obj k = case KM.lookup k obj of
    Just (String t) -> Just t
    _ -> Nothing

-- | Compute count, median, mean, min, and max for a list of numeric observations.
metricSummary :: [Double] -> Maybe MetricSummary
metricSummary [] = Nothing
metricSummary values =
    let sorted = sort values
        n = length sorted
        (mn, mx) = listBounds sorted
        med
            | odd n = fromMaybe 0.0 (elemAt (n `div` 2) sorted)
            | otherwise =
                let mid = n `div` 2
                    lo = fromMaybe 0.0 (elemAt (mid - 1) sorted)
                    hi = fromMaybe 0.0 (elemAt mid sorted)
                 in (lo + hi) / 2.0
     in Just
            MetricSummary
                { msCount = n
                , msMedian = med
                , msMean = sum sorted / fromIntegral n
                , msMin = mn
                , msMax = mx
                }
    where
        elemAt i xs = case drop i xs of
            (y : _) -> Just y
            [] -> Nothing

        listBounds [] = (0.0, 0.0)
        listBounds (x : xs) = Foldable.foldl' (\(lo, hi) v -> (min lo v, max hi v)) (x, x) xs

-- | Load @stats.json@ if available and normalize one job result into a run record.
collectRunRecordIO :: BatchJobResult -> IO BatchRunRecord
collectRunRecordIO jobRes = do
    mStats <- case bjrStatsPath jobRes of
        Just path | not (null path) -> readJsonIfExists path
        _ -> pure Nothing
    pure (collectRunRecord jobRes mStats)

-- | Purely combine @job_result.json@ and optional @stats.json@ into a run record.
collectRunRecord :: BatchJobResult -> Maybe StatsSnapshot -> BatchRunRecord
collectRunRecord jobRes mStats =
    let mEm = stEvaluationMetrics =<< mStats
        evalStatus = firstNonEmpty [stStatus =<< mStats, bjrEvalStatus jobRes]
        initBytes = stInitialBytes =<< mStats
        initBytesVal = if bjrInitialBytes jobRes > 0 then Just (bjrInitialBytes jobRes) else initBytes
        redBytes = stReducedBytes =<< mStats
        redRatio = stReductionRatio =<< mStats
        initStmts = stInitialStatements =<< mStats
        redStmts = stReducedStatements =<< mStats
        stmtRatio = case (initStmts, redStmts) of
            (Just iS, Just rS)
                | iS > 0 ->
                    Just ((fromIntegral (iS - rS) / fromIntegral iS) * 100.0)
            _ -> Nothing
        attempts = stOracleAttempts =<< mStats
        fallbackBytes = fromMaybe 0 initBytesVal
        curve = maybe [] (map (toEvaluationPoint fallbackBytes) . semCurve) mEm
        peakIdx = Foldable.foldl' (\acc pt -> max acc pt.callIndex) 0 curve
        totalAucCalls = max (fromMaybe 0 attempts) peakIdx
        attemptAuc = case (mStats, initBytesVal) of
            (Just _, Just ib) | ib /= 0 -> Just (computeAttemptAuc ib curve totalAucCalls)
            _ -> Nothing
        overheadUs = engineOverheadMicros (semCostAttribution =<< mEm)
        validationPassed = semValidationPassed =<< mEm
        isCompleted = evalStatus == Just "completed" && fromMaybe False validationPassed
        isCensored = evalStatus == Just "timeout" || evalStatus == Just "execution-budget"

        hasReplay = maybe False ((> 0) . rvRunsRequired) (bjrReducedReplayVerification jobRes)
        mReplay = if hasReplay then bjrReducedReplayVerification jobRes else Nothing

        dbmsLabel = if T.null (bjrDbms jobRes) then "unknown" else bjrDbms jobRes
        stratSig = case csId <$> bjrBaselineSignature jobRes of
            Just sid | not (T.null sid) -> dbmsLabel <> "::" <> sid
            _ -> dbmsLabel <> "::unqualified:" <> bjrCaseId jobRes

        evalJobs = case stEvaluationBudget =<< mStats of
            Just budgetObj -> case KM.lookup "evaluation_jobs" budgetObj of
                Just (Number sci) -> Just (round sci)
                _ -> Nothing
            Nothing -> Nothing
     in BatchRunRecord
            { recJobId = bjrJobId jobRes
            , recCaseId = bjrCaseId jobRes
            , recDbms = bjrDbms jobRes
            , recDialect = bjrDialect jobRes
            , recProfile = bjrProfile jobRes
            , recRepeat = bjrRepeat jobRes
            , recJobState = bjrState jobRes
            , recStatus = evalStatus
            , recIsCompleted = isCompleted
            , recIsCensored = isCensored
            , recInputQualified = bjrInputQualified jobRes
            , recBaselineSignature = bjrBaselineSignature jobRes
            , recStratificationSignature = stratSig
            , recInitialBytes = initBytesVal
            , recReducedBytes = redBytes
            , recReductionRatio = redRatio
            , recInitialStatements = initStmts
            , recReducedStatements = redStmts
            , recStatementReductionRatio = stmtRatio
            , recOracleExecutions = stOracleExecutions =<< mStats
            , recOracleAttempts = attempts
            , recCacheHits = stCacheHits =<< mStats
            , recCacheMisses = stCacheMisses =<< mStats
            , recCacheJoins = stCacheJoins =<< mStats
            , recCacheHitRatio = stCacheHitRatio =<< mStats
            , recCancelledObservations = stCancelledObservations =<< mStats
            , recElapsedSeconds = stElapsedSeconds =<< mStats
            , recOracleSeconds = semOracleSeconds =<< mEm
            , recResetSeconds = semResetSeconds =<< mEm
            , recAuditSeconds = semAuditSeconds =<< mEm
            , recValidationSeconds = semValidationSeconds =<< mEm
            , recValidationPassed = validationPassed
            , recMinimalityClaim = semMinimalityClaim =<< mEm
            , recAttemptAuc = attemptAuc
            , recEngineOverheadUs = overheadUs
            , recEvaluationJobs = evalJobs
            , recReducedReplayRunsRequired = maybe 0 rvRunsRequired mReplay
            , recReducedReplayRunsPassed = rvRunsPassed <$> mReplay
            , recReducedReplayAllPassed = rvAllPassed <$> mReplay
            , recReducedReplayConsistentSignature = rvConsistentSignature <$> mReplay
            , recReducedReplayMatchesBaseline = mReplay >>= rvMatchesBaseline
            , recReducedReplaySlippageCount = rvSlippageCount <$> mReplay
            , recReducedReplayObservedSignatures =
                if hasReplay then rvObservedSignatureIds <$> mReplay else Just []
            , recOracleDecisionsSummary = bjrOracleDecisionsSummary jobRes
            }

firstNonEmpty :: [Maybe Text] -> Maybe Text
firstNonEmpty [] = Nothing
firstNonEmpty (Just t : rest)
    | not (T.null t) = Just t
    | otherwise = firstNonEmpty rest
firstNonEmpty (Nothing : rest) = firstNonEmpty rest

-- | Aggregate a slice of run records into completed/censored/replay metrics.
aggregateGroup :: [BatchRunRecord] -> GroupSummary
aggregateGroup records =
    let completed = filter recIsCompleted records
        censored = filter recIsCensored records
        withCheckpoints =
            filter
                (\record -> isJust (recReducedBytes record) && (recIsCompleted record || recIsCensored record))
                records
        replayChecked = filter ((> 0) . recReducedReplayRunsRequired) records
        replayPassed = filter ((== Just True) . recReducedReplayAllPassed) replayChecked
        zeroSlippageReplay =
            filter
                ( \record ->
                    recReducedReplayAllPassed record == Just True
                        && recReducedReplayMatchesBaseline record == Just True
                )
                replayChecked
        slippedReplay = filter ((> 0) . fromMaybe 0 . recReducedReplaySlippageCount) replayChecked
     in GroupSummary
            { gsTotalRuns = length records
            , gsCompletedRuns = length completed
            , gsCensoredRuns = length censored
            , gsErrorOrFailedRuns =
                length
                    [ record
                    | record <- records
                    , not (recIsCompleted record)
                    , not (recIsCensored record)
                    , recJobState record /= JobSkippedUnstableInput
                    ]
            , gsSkippedUnstableRuns =
                length (filter ((== JobSkippedUnstableInput) . recJobState) records)
            , gsValidationPassedRuns =
                length (filter ((== Just True) . recValidationPassed) records)
            , gsMinimality1MinimalRuns =
                length (filter ((== Just "statement-deletion-1-minimal") . recMinimalityClaim) records)
            , gsReducedReplayCheckedRuns = length replayChecked
            , gsReducedReplay5of5Runs = length replayPassed
            , gsReducedReplayZeroSlippageRuns = length zeroSlippageReplay
            , gsReducedReplaySlippedRuns = length slippedReplay
            , gsCompletedOnly = aggregateMetricsSubset completed
            , gsAllWithCheckpoints = aggregateMetricsSubset withCheckpoints
            }

aggregateMetricsSubset :: [BatchRunRecord] -> MetricsSubset
aggregateMetricsSubset subset =
    MetricsSubset
        { subCount = length subset
        , subReducedBytes = metricSummary (mapMaybe (fmap fromIntegral . recReducedBytes) subset)
        , subReductionRatio = metricSummary (mapMaybe recReductionRatio subset)
        , subReducedStatements = metricSummary (mapMaybe (fmap fromIntegral . recReducedStatements) subset)
        , subStatementReductionRatio = metricSummary (mapMaybe recStatementReductionRatio subset)
        , subOracleExecutions = metricSummary (mapMaybe (fmap fromIntegral . recOracleExecutions) subset)
        , subOracleAttempts = metricSummary (mapMaybe (fmap fromIntegral . recOracleAttempts) subset)
        , subCacheHitRatio = metricSummary (mapMaybe recCacheHitRatio subset)
        , subElapsedSeconds = metricSummary (mapMaybe recElapsedSeconds subset)
        , subOracleSeconds = metricSummary (mapMaybe recOracleSeconds subset)
        , subResetSeconds = metricSummary (mapMaybe recResetSeconds subset)
        , subAuditSeconds = metricSummary (mapMaybe recAuditSeconds subset)
        , subValidationSeconds = metricSummary (mapMaybe recValidationSeconds subset)
        , subAttemptAuc = metricSummary (mapMaybe recAttemptAuc subset)
        , subEngineOverheadUs = metricSummary (mapMaybe (fmap fromIntegral . recEngineOverheadUs) subset)
        }

-- | Purely construct the full @AggregatedReport@ from qualifications and run records.
aggregateReport ::
    UTCTime -> FilePath -> Map Text QualificationResult -> [BatchRunRecord] -> AggregatedReport
aggregateReport now outAbs qualifications runRecords =
    let nonEmptyOr d t = if T.null t then d else t
        profileGroups = groupByKey (nonEmptyOr "unknown" . recProfile) runRecords
        byProfile = Map.map aggregateGroup profileGroups
        dbmsGroups = groupByKey (nonEmptyOr "unknown" . recDbms) runRecords
        byDbms = Map.map aggregateGroup dbmsGroups
        sigGroups = groupByKey recStratificationSignature runRecords
        bySignature = Map.map buildSignatureGroup sigGroups
        sigStratifiedByProfile =
            Map.fromSet (buildSignatureStratified bySignature) (Map.keysSet profileGroups)

        qualVals = Map.elems qualifications
        stableCount = length (filter qrQualified qualVals)
        uniqueQualSigs =
            Set.fromList
                [ (qrDbms qr, csId sig)
                | qr <- qualVals
                , qrQualified qr
                , Just sig <- [qrBaselineSignature qr]
                , not (T.null (csId sig))
                ]
        casesInRuns = Set.size (Set.fromList (filter (not . T.null) (map recCaseId runRecords)))
        generatedAt = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00" now)
        overview =
            BatchOverview
                { ovCasesInRuns = casesInRuns
                , ovQualifiedCasesEvaluated = Map.size qualifications
                , ovInputStableCases = stableCount
                , ovInputUnstableCases = Map.size qualifications - stableCount
                , ovUniqueQualifiedCrashSignatures = Set.size uniqueQualSigs
                , ovTotalJobRuns = length runRecords
                , ovUniqueStratificationGroups = Map.size bySignature
                }
     in AggregatedReport
            { aggSchemaVersion = 1
            , aggGeneratedAt = generatedAt
            , aggOutDir = outAbs
            , aggOverview = overview
            , aggQualifications = qualifications
            , aggByProfile = byProfile
            , aggByDbms = byDbms
            , aggBySignature = bySignature
            , aggSignatureStratifiedByProfile = sigStratifiedByProfile
            , aggRuns = runRecords
            }
    where
        groupByKey :: (Ord k) => (a -> k) -> [a] -> Map k [a]
        groupByKey keyFn =
            Map.map reverse . Foldable.foldl' (\acc x -> Map.insertWith (++) (keyFn x) [x] acc) Map.empty

        nonEmptyTxt d t = if T.null t then d else t

        buildSignatureGroup recs =
            let sigProfGroups = groupByKey (nonEmptyTxt "unknown" . recProfile) recs
                caseIds = Set.toAscList (Set.fromList (filter (not . T.null) (map recCaseId recs)))
                firstDbms = case recs of
                    (r : _) | not (T.null (recDbms r)) -> Just (recDbms r)
                    _ -> Nothing
                mFirstSig = listToMaybe (mapMaybe recBaselineSignature recs)
             in SignatureGroupSummary
                    { sgsSignatureId = csId <$> mFirstSig
                    , sgsSignatureHash = csHash <$> mFirstSig
                    , sgsCategory = categoryName . csCategory <$> mFirstSig
                    , sgsDbms = firstDbms
                    , sgsCaseIds = caseIds
                    , sgsCaseCount = length caseIds
                    , sgsOverall = aggregateGroup recs
                    , sgsByProfile = Map.map aggregateGroup sigProfGroups
                    }

        buildSignatureStratified bySig prof =
            let sigEntries = Map.elems bySig
                observed = length [() | s <- sigEntries, Map.member prof (sgsByProfile s)]
                (withComp, zeroSlip, ratios, bytes, attempts, aucs, elapseds) =
                    Foldable.foldl' (stepSig prof) (0, 0, [], [], [], [], []) sigEntries
             in SignatureProfileStratified
                    { spsSignaturesObserved = observed
                    , spsSignaturesWithCompletedRuns = withComp
                    , spsSignaturesZeroSlippageAllRuns = zeroSlip
                    , spsCompletedSignatureMedianReductionRatio = metricSummary ratios
                    , spsCompletedSignatureMedianReducedBytes = metricSummary bytes
                    , spsCompletedSignatureMedianOracleAttempts = metricSummary attempts
                    , spsCompletedSignatureMedianAttemptAuc = metricSummary aucs
                    , spsCompletedSignatureMedianElapsedSeconds = metricSummary elapseds
                    }

        stepSig prof acc@(withComp, zeroSlip, ratios, bytes, attempts, aucs, elapseds) sigEntry =
            case Map.lookup prof (sgsByProfile sigEntry) of
                Nothing -> acc
                Just pAgg ->
                    let comp = gsCompletedOnly pAgg
                        (withComp', ratios', bytes', attempts', aucs', elapseds')
                            | subCount comp > 0 =
                                ( withComp + 1
                                , maybeCons (msMedian <$> subReductionRatio comp) ratios
                                , maybeCons (msMedian <$> subReducedBytes comp) bytes
                                , maybeCons (msMedian <$> subOracleAttempts comp) attempts
                                , maybeCons (msMedian <$> subAttemptAuc comp) aucs
                                , maybeCons (msMedian <$> subElapsedSeconds comp) elapseds
                                )
                            | otherwise = (withComp, ratios, bytes, attempts, aucs, elapseds)
                        zeroSlip'
                            | gsReducedReplayCheckedRuns pAgg > 0
                                && gsReducedReplayZeroSlippageRuns pAgg == gsReducedReplayCheckedRuns pAgg =
                                zeroSlip + 1
                            | otherwise = zeroSlip
                     in (withComp', zeroSlip', ratios', bytes', attempts', aucs', elapseds')

        maybeCons (Just x) xs = x : xs
        maybeCons Nothing xs = xs

-- | Aggregate all runs and qualifications under @outDir@ and atomically write @aggregated_stats.json@.
aggregateOutDir :: FilePath -> IO AggregatedReport
aggregateOutDir outDir = do
    outAbs <- makeAbsolute outDir
    let qualRoot = outAbs </> "qualified"
        runsRoot = outAbs </> "runs"
    qualifications <- loadQualifications qualRoot
    jobResults <- loadJobResults runsRoot
    runRecords <- mapM collectRunRecordIO jobResults
    now <- getCurrentTime
    let agg = aggregateReport now outAbs qualifications runRecords
        aggPath = outAbs </> "aggregated_stats.json"
    writeAtomicLazyBytes aggPath (encode agg <> BSL.pack [10])
    pure agg

loadQualifications :: FilePath -> IO (Map Text QualificationResult)
loadQualifications qualRoot = do
    exists <- doesDirectoryExist qualRoot
    if not exists
        then pure Map.empty
        else do
            dirs <- sort <$> listDirectory qualRoot
            pairs <- catMaybes <$> mapM loadOne dirs
            pure (Map.fromList pairs)
    where
        loadOne caseDir = do
            let qPath = qualRoot </> caseDir </> "input_replay.json"
            mQr <- readJsonIfExists qPath
            pure $ case mQr of
                Just qr ->
                    let cid = if T.null (qrCaseId qr) then T.pack caseDir else qrCaseId qr
                     in Just (cid, qr)
                Nothing -> Nothing

loadJobResults :: FilePath -> IO [BatchJobResult]
loadJobResults runsRoot = do
    exists <- doesDirectoryExist runsRoot
    if not exists
        then pure []
        else do
            dirs <- sort <$> listDirectory runsRoot
            catMaybes <$> mapM loadOne dirs
    where
        loadOne jobDir = readJsonIfExists (runsRoot </> jobDir </> "job_result.json")

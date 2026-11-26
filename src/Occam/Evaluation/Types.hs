module Occam.Evaluation.Types (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationObserver,
    EvaluationPoint (..),
    EvaluationProgress (..),
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    MinimalityClaim (..),
    ReductionAction,
    ReductionActionResult (..),
    Stats (..),
    Ticks,
    evaluationStatusName,
    metricPolicyForReduction,
    minimalityClaimName,
    parseEvaluationStatusName,
    parseMinimalityClaimName,
    parseSQLDialect,
    seconds,
    sqlDialectName,
    ticks,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
    withText,
 )
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Occam.Json (normalizeJsonFieldNames, snakeCaseOptions)
import Occam.Logger (Logger)
import Occam.Metrics (MetricPolicy (..), MetricsSnapshot)
import Occam.Oracle (
    OracleObservation,
    OracleObservationDetails,
    OracleStatus,
 )
import Occam.Parser (SQLDialect (..), dialectName, lookupDialectByName)
import Occam.Pool (OraclePool)
import Occam.Reducer (
    ColumnMode (..),
    ReductionConfig (..),
    ReductionEvidence,
    StageReporter,
    columnModeName,
    reductionSearchPolicy,
 )

data EvaluationConfig = EvaluationConfig
    { evaluationCacheEnabled :: !Bool
    , evaluationJobs :: !Int
    , evaluationTotalTimeoutMicros :: !(Maybe Int)
    , evaluationMaxOracleExecutions :: !(Maybe Int)
    , evaluationPreflightMicros :: !Int
    , evaluationPreflightAttempts :: !Int
    , evaluationPreflightResetMicros :: !(Maybe Int)
    , evaluationPreflightOracleMicros :: !(Maybe Int)
    , evaluationInitialStatements :: !(Maybe Int)
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvaluationConfig where
    toJSON = genericToJSON snakeCaseOptions

instance FromJSON EvaluationConfig where
    parseJSON = genericParseJSON snakeCaseOptions . normalizeJsonFieldNames

data EvaluationStatus
    = EvaluationCompleted
    | EvaluationTimeout
    | EvaluationExecutionBudget
    | EvaluationError
    | EvaluationValidationFailed
    | EvaluationValidationIncomplete
    deriving (Eq, Ord, Show)

evaluationStatusName :: EvaluationStatus -> Text
evaluationStatusName EvaluationCompleted = "completed"
evaluationStatusName EvaluationTimeout = "timeout"
evaluationStatusName EvaluationExecutionBudget = "execution-budget"
evaluationStatusName EvaluationError = "error"
evaluationStatusName EvaluationValidationFailed = "validation-failed"
evaluationStatusName EvaluationValidationIncomplete = "validation-incomplete"

parseEvaluationStatusName :: Text -> Maybe EvaluationStatus
parseEvaluationStatusName "completed" = Just EvaluationCompleted
parseEvaluationStatusName "timeout" = Just EvaluationTimeout
parseEvaluationStatusName "execution-budget" = Just EvaluationExecutionBudget
parseEvaluationStatusName "error" = Just EvaluationError
parseEvaluationStatusName "validation-failed" = Just EvaluationValidationFailed
parseEvaluationStatusName "validation-incomplete" = Just EvaluationValidationIncomplete
parseEvaluationStatusName _ = Nothing

instance ToJSON EvaluationStatus where
    toJSON :: EvaluationStatus -> Value
    toJSON = toJSON . evaluationStatusName

instance FromJSON EvaluationStatus where
    parseJSON = withText "EvaluationStatus" \name ->
        maybe (fail ("unknown evaluation status: " <> T.unpack name)) pure (parseEvaluationStatusName name)

data EvaluationProgress
    = ProgressStage !Text
    | ProgressCandidateTested !Int !Bool !Bool
    | ProgressCandidateAccepted !Int !(Maybe Int)
    deriving (Eq, Show)

data MinimalityClaim
    = StatementDeletionOneMinimal
    | MinimalityNotEstablished
    deriving (Eq, Ord, Show)

minimalityClaimName :: MinimalityClaim -> Text
minimalityClaimName StatementDeletionOneMinimal = "statement-deletion-1-minimal"
minimalityClaimName MinimalityNotEstablished = "not-established"

parseMinimalityClaimName :: Text -> Maybe MinimalityClaim
parseMinimalityClaimName "statement-deletion-1-minimal" = Just StatementDeletionOneMinimal
parseMinimalityClaimName "not-established" = Just MinimalityNotEstablished
parseMinimalityClaimName _ = Nothing

instance ToJSON MinimalityClaim where
    toJSON :: MinimalityClaim -> Value
    toJSON = toJSON . minimalityClaimName

instance FromJSON MinimalityClaim where
    parseJSON = withText "MinimalityClaim" \name ->
        maybe (fail ("unknown minimality claim: " <> T.unpack name)) pure (parseMinimalityClaimName name)

parseSQLDialect :: String -> Either String SQLDialect
parseSQLDialect value =
    case lookupDialectByName (T.pack value) of
        Just dialect -> Right dialect
        Nothing ->
            Left ("Expected one of: " ++ T.unpack (T.intercalate ", " (map dialectName [minBound .. maxBound])))

-- | Canonical CLI dialect spelling.
sqlDialectName :: SQLDialect -> Text
sqlDialectName = dialectName

data EvaluationPoint = EvaluationPoint
    { pointElapsedSeconds :: !Double
    , pointCallIndex :: !Int
    , pointCandidateBytes :: !Int
    , pointCandidateStatements :: !(Maybe Int)
    , pointCacheHit :: !Bool
    , pointAccepted :: !Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvaluationPoint where
    toJSON = genericToJSON snakeCaseOptions
    toEncoding = genericToEncoding snakeCaseOptions

instance FromJSON EvaluationPoint where
    parseJSON = genericParseJSON snakeCaseOptions . normalizeJsonFieldNames

data EvaluationMetrics = EvaluationMetrics
    { evaluationStatus :: !EvaluationStatus
    , evaluationFailure :: !(Maybe Text)
    , evaluationCacheUsed :: !Bool
    , evaluationBudget :: !EvaluationConfig
    , evaluationTotalSeconds :: !Double
    , evaluationReductionSeconds :: !Double
    , evaluationOracleSeconds :: !Double
    , evaluationResetSeconds :: !Double
    , evaluationPreflightResetSeconds :: !(Maybe Double)
    , evaluationPreflightOracleSeconds :: !(Maybe Double)
    , evaluationOracleAttempts :: !Int
    , evaluationDatabaseObservations :: !Int
    , evaluationCompletedOracleProcesses :: !Int
    , evaluationAcceptedObservations :: !Int
    , evaluationRejectedObservations :: !Int
    , evaluationTimedOutObservations :: !Int
    , evaluationResetFailures :: !Int
    , evaluationOracleErrors :: !Int
    , evaluationCancelledObservations :: !Int
    , evaluationOracleOutcomes :: !(Map OracleStatus Int)
    , evaluationCacheHits :: !Int
    , evaluationCacheMisses :: !Int
    , evaluationCacheJoins :: !Int
    , evaluationValidationSeconds :: !Double
    , evaluationValidationOracleSeconds :: !Double
    , evaluationValidationResetSeconds :: !Double
    , evaluationValidationObservations :: !Int
    , evaluationValidationDatabaseObservations :: !Int
    , evaluationValidationErrors :: !Int
    , evaluationValidationOutcomes :: !(Map OracleStatus Int)
    , evaluationValidationPassed :: !Bool
    , evaluationAuditSeconds :: !Double
    , evaluationAuditChecks :: !Int
    , evaluationAuditCompleted :: !Bool
    , evaluationAuditSkipped :: !Bool
    , evaluationMinimalityClaim :: !MinimalityClaim
    , evaluationCurve :: ![EvaluationPoint]
    , evaluationStages :: ![Value]
    , evaluationCostAttribution :: !(Maybe MetricsSnapshot)
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvaluationMetrics where
    toJSON = genericToJSON snakeCaseOptions
    toEncoding = genericToEncoding snakeCaseOptions

instance FromJSON EvaluationMetrics where
    parseJSON = genericParseJSON snakeCaseOptions . normalizeJsonFieldNames

data EvaluationResult = EvaluationResult
    { evaluatedSQL :: !ByteString
    , evaluatedStatements :: !(Maybe Int)
    , evaluationMetrics :: !EvaluationMetrics
    }
    deriving (Eq, Show)

-- | Deterministic reduction run statistics persisted to @stats.json@.
data Stats = Stats
    { initial_bytes :: !Int
    , reduced_bytes :: !Int
    , reduction_ratio :: !Double
    , initial_statements :: !Int
    , reduced_statements :: !Int
    , oracle_executions :: !Int
    , oracle_attempts :: !Int
    , initial_oracle_observations :: !Int
    , cache_hits :: !Int
    , cache_misses :: !Int
    , cache_joins :: !Int
    , cache_hit_ratio :: !Double
    , cancelled_observations :: !Int
    , status :: !EvaluationStatus
    , reduction_settings :: !Value
    , evaluation_budget :: !EvaluationConfig
    , evaluation_metrics :: !EvaluationMetrics
    , initial_oracle_seconds :: !Double
    , elapsed_seconds :: !Double
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON)

instance ToJSON Stats where
    toJSON = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

-- | Reducer result containing reduced SQL and optional fixed-point evidence.
data ReductionActionResult = ReductionActionResult
    { actionSQL :: !ByteString
    , actionEvidence :: !(Maybe ReductionEvidence)
    }
    deriving (Eq, Show)

type ReductionAction = StageReporter -> Logger -> OraclePool -> IO ReductionActionResult

-- | All inputs for one evaluated reduction run.
type EvaluationObserver = IO () -> ByteString -> IO OracleObservationDetails

data EvaluationRequest oracle = EvaluationRequest
    { requestOnProgress :: !(EvaluationProgress -> IO ())
    , requestMetricsPolicy :: !(Maybe MetricPolicy)
    , requestConfig :: !EvaluationConfig
    , requestDialect :: !SQLDialect
    , requestOracleConfig :: !oracle
    , requestPreflightObservation :: !(Maybe OracleObservation)
    , requestLogger :: !Logger
    , requestInitialSQL :: !ByteString
    , requestCheckpoint :: !FilePath
    , requestAction :: !ReductionAction
    , requestCollectCurve :: !Bool
    }

type Ticks = Integer

ticks :: IO Ticks
ticks = toInteger <$> getMonotonicTimeNSec

seconds :: Ticks -> Ticks -> Double
seconds start end = fromIntegral (end - start) / 1000000000

-- | Build a MetricPolicy for the given reduction configuration.
metricPolicyForReduction :: ReductionConfig -> Int -> Either Text MetricPolicy
metricPolicyForReduction config jobs
    | not (reductionIntraStatement config) && reductionColumnMode config /= ColumnOff =
        Left "column-mode requires --intra-statement"
    | otherwise =
        Right
            MetricPolicy
                { metricSearchPolicy = reductionSearchPolicy config
                , metricColumnMode = columnModeName (reductionColumnMode config)
                , metricJobs = jobs
                }

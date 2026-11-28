-- | Configuration, progress, metrics, and minimality types for bounded reduction evaluation.
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
    Options (..),
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
import Occam.Json (camelToSnake, normalizeJsonFieldNames, snakeCaseOptions)
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

evaluationJsonOptions :: Options
evaluationJsonOptions =
    snakeCaseOptions
        { fieldLabelModifier = \field ->
            "evaluation_" <> camelToSnake (if field == "runStatus" then "status" else field)
        }

pointJsonOptions :: Options
pointJsonOptions =
    snakeCaseOptions {fieldLabelModifier = ("point_" <>) . camelToSnake}

-- | Cache, worker-count, time-budget, and max-execution limits for one evaluation session.
data EvaluationConfig = EvaluationConfig
    { cacheEnabled :: !Bool
    , jobs :: !Int
    , totalTimeoutMicros :: !(Maybe Int)
    , maxOracleExecutions :: !(Maybe Int)
    , preflightMicros :: !Int
    , preflightAttempts :: !Int
    , preflightResetMicros :: !(Maybe Int)
    , preflightOracleMicros :: !(Maybe Int)
    , initialStatements :: !(Maybe Int)
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvaluationConfig where
    toJSON = genericToJSON evaluationJsonOptions

instance FromJSON EvaluationConfig where
    parseJSON = genericParseJSON evaluationJsonOptions . normalizeJsonFieldNames

-- | Terminal status explaining how an evaluation run concluded.
data EvaluationStatus
    = EvaluationCompleted
    | EvaluationTimeout
    | EvaluationExecutionBudget
    | EvaluationError
    | EvaluationValidationFailed
    | EvaluationValidationIncomplete
    deriving (Eq, Ord, Show)

-- | Render an 'EvaluationStatus' as its lowercase kebab-case wire name.
evaluationStatusName :: EvaluationStatus -> Text
evaluationStatusName EvaluationCompleted = "completed"
evaluationStatusName EvaluationTimeout = "timeout"
evaluationStatusName EvaluationExecutionBudget = "execution-budget"
evaluationStatusName EvaluationError = "error"
evaluationStatusName EvaluationValidationFailed = "validation-failed"
evaluationStatusName EvaluationValidationIncomplete = "validation-incomplete"

-- | Parse a kebab-case 'EvaluationStatus' string.
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

-- | Incremental progress event pushed to UI \/ observer callbacks during reduction.
data EvaluationProgress
    = ProgressStage !Text
    | ProgressCandidateTested !Int !Bool !Bool
    | ProgressCandidateAccepted !Int !(Maybe Int)
    deriving (Eq, Show)

-- | Minimality guarantee achieved by the completed reduction run.
data MinimalityClaim
    = StatementDeletionOneMinimal
    | MinimalityNotEstablished
    deriving (Eq, Ord, Show)

-- | Render a 'MinimalityClaim' as its kebab-case serialization token.
minimalityClaimName :: MinimalityClaim -> Text
minimalityClaimName StatementDeletionOneMinimal = "statement-deletion-1-minimal"
minimalityClaimName MinimalityNotEstablished = "not-established"

-- | Parse a kebab-case 'MinimalityClaim' token.
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

-- | Parse a CLI or config dialect name string into 'SQLDialect'.
parseSQLDialect :: String -> Either String SQLDialect
parseSQLDialect value =
    case lookupDialectByName (T.pack value) of
        Just dialect -> Right dialect
        Nothing ->
            Left ("Expected one of: " ++ T.unpack (T.intercalate ", " (map dialectName [minBound .. maxBound])))

-- | Canonical CLI dialect spelling.
sqlDialectName :: SQLDialect -> Text
sqlDialectName = dialectName

-- | Single accepted or evaluated candidate sample along the size-vs-time reduction curve.
data EvaluationPoint = EvaluationPoint
    { elapsedSeconds :: !Double
    , callIndex :: !Int
    , candidateBytes :: !Int
    , candidateStatements :: !(Maybe Int)
    , cacheHit :: !Bool
    , accepted :: !Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvaluationPoint where
    toJSON = genericToJSON pointJsonOptions
    toEncoding = genericToEncoding pointJsonOptions

instance FromJSON EvaluationPoint where
    parseJSON = genericParseJSON pointJsonOptions . normalizeJsonFieldNames

-- | Aggregate resource, cache, timing, and minimality telemetry for one evaluation run.
data EvaluationMetrics = EvaluationMetrics
    { runStatus :: !EvaluationStatus
    , failure :: !(Maybe Text)
    , cacheUsed :: !Bool
    , budget :: !EvaluationConfig
    , totalSeconds :: !Double
    , reductionSeconds :: !Double
    , oracleSeconds :: !Double
    , resetSeconds :: !Double
    , preflightResetSeconds :: !(Maybe Double)
    , preflightOracleSeconds :: !(Maybe Double)
    , oracleAttempts :: !Int
    , databaseObservations :: !Int
    , completedOracleProcesses :: !Int
    , acceptedObservations :: !Int
    , rejectedObservations :: !Int
    , timedOutObservations :: !Int
    , resetFailures :: !Int
    , oracleErrors :: !Int
    , cancelledObservations :: !Int
    , oracleOutcomes :: !(Map OracleStatus Int)
    , cacheHits :: !Int
    , cacheMisses :: !Int
    , cacheJoins :: !Int
    , validationSeconds :: !Double
    , validationOracleSeconds :: !Double
    , validationResetSeconds :: !Double
    , validationObservations :: !Int
    , validationDatabaseObservations :: !Int
    , validationErrors :: !Int
    , validationOutcomes :: !(Map OracleStatus Int)
    , validationPassed :: !Bool
    , auditSeconds :: !Double
    , auditChecks :: !Int
    , auditCompleted :: !Bool
    , auditSkipped :: !Bool
    , minimalityClaim :: !MinimalityClaim
    , curve :: ![EvaluationPoint]
    , stages :: ![Value]
    , costAttribution :: !(Maybe MetricsSnapshot)
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvaluationMetrics where
    toJSON = genericToJSON evaluationJsonOptions
    toEncoding = genericToEncoding evaluationJsonOptions

instance FromJSON EvaluationMetrics where
    parseJSON = genericParseJSON evaluationJsonOptions . normalizeJsonFieldNames

-- | Final output produced by running a reduction under bounded evaluation.
data EvaluationResult = EvaluationResult
    { sql :: !ByteString
    , statements :: !(Maybe Int)
    , metrics :: !EvaluationMetrics
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
    { candidateSQL :: !ByteString
    , evidence :: !(Maybe ReductionEvidence)
    }
    deriving (Eq, Show)

-- | Parameterized reduction callback invoked once a bounded 'OraclePool' is ready.
type ReductionAction = StageReporter -> Logger -> OraclePool -> IO ReductionActionResult

-- | Observer callback executing a candidate with an on-start hook for timing attribution.
type EvaluationObserver = IO () -> ByteString -> IO OracleObservationDetails

-- | Full request bundle configuring an evaluated reduction run across dialects and oracles.
data EvaluationRequest oracle = EvaluationRequest
    { onProgress :: !(EvaluationProgress -> IO ())
    , metricsPolicy :: !(Maybe MetricPolicy)
    , config :: !EvaluationConfig
    , dialect :: !SQLDialect
    , oracleConfig :: !oracle
    , preflightObservation :: !(Maybe OracleObservation)
    , logger :: !Logger
    , initialSQL :: !ByteString
    , checkpoint :: !FilePath
    , action :: !ReductionAction
    , collectCurve :: !Bool
    }

-- | Monotonic timestamp measured in nanoseconds.
type Ticks = Integer

-- | Read the current monotonic clock in nanoseconds.
ticks :: IO Ticks
ticks = toInteger <$> getMonotonicTimeNSec

-- | Convert a start/end 'Ticks' pair into elapsed seconds.
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

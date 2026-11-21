module Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationPoint (..),
    EvaluationProgress (..),
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    MinimalityClaim (..),
    ReductionActionResult (..),
    Stats (..),
    evaluationStatusName,
    metricPolicyForReduction,
    minimalityClaimName,
    parseSQLDialect,
    runEvaluatedReduction,
    runEvaluatedReductionWithProgress,
    runEvaluationRequest,
    sqlDialectName,
) where

import Control.Exception.Safe (
    SomeException,
    displayException,
    finally,
    fromException,
    mask_,
    try,
 )
import Control.Monad (when)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    genericParseJSON,
    genericToJSON,
    object,
    withText,
    (.=),
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Occam.AtomicFile (writeAtomicBytes)
import Occam.Cache (CacheStats (..))
import Occam.Core (OracleOutcome (..))
import Occam.Json (normalizeJsonFieldNames, snakeCaseOptions)
import Occam.Logger (Logger (..), logDebug', logInfo')
import Occam.Metrics (
    MetricPolicy (..),
    MetricsSnapshot,
    MetricsStore,
    newMetricsStore,
    snapshotMetrics,
 )
import Occam.Oracle (
    OracleConfig (..),
    OracleContext (..),
    OracleObservation (..),
    OracleObservationDetails (..),
    OracleReapFailed (..),
    OracleStatus (..),
    classifyOracleObservation,
    observationOutcome,
    observeExternalOracleDetailedWithContext,
    oracleObservationMatches,
    resolveOracleContext,
 )
import Occam.Parser (SQLDialect (..), countStatements, dialectName)
import Occam.Pool (
    ExecutionBudgetReached (..),
    OraclePool,
    PoolConfig (..),
    closeOraclePool,
    mapPoolRetire,
    newOraclePool,
    poolAttempts,
    poolCacheStats,
    poolCancelled,
 )
import Occam.Reducer (
    AuditStats (..),
    ColumnMode (..),
    ReductionConfig (..),
    ReductionEvidence (..),
    ReductionStage (..),
    StageEvent (..),
    StageReporter,
    columnModeName,
    reductionSearchPolicy,
    reductionStageName,
    statementAuditWithLogger,
 )
import System.Timeout (timeout)

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
parseSQLDialect value = case map toLower value of
    "generic" -> Right GenericSQL
    "postgres" -> Right PostgreSQL
    "postgresql" -> Right PostgreSQL
    "mysql" -> Right MySQL
    "sqlite" -> Right SQLite
    "duckdb" -> Right DuckDB
    "clickhouse" -> Right ClickHouse
    "oracle" -> Right Oracle
    "ansi" -> Right Ansi
    "bigquery" -> Right BigQuery
    "databricks" -> Right Databricks
    "hive" -> Right Hive
    "mssql" -> Right MsSql
    "sqlserver" -> Right MsSql
    "redshift" -> Right Redshift
    "snowflake" -> Right Snowflake
    "spark" -> Right Spark
    "sparksql" -> Right Spark
    "teradata" -> Right Teradata
    _ ->
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

instance FromJSON EvaluationMetrics where
    parseJSON = genericParseJSON snakeCaseOptions . normalizeJsonFieldNames

data EvaluationResult = EvaluationResult
    { evaluatedSQL :: !ByteString
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
    deriving anyclass (ToJSON, FromJSON)

-- | Reducer result containing reduced SQL and optional fixed-point evidence.
data ReductionActionResult = ReductionActionResult
    { actionSQL :: !ByteString
    , actionEvidence :: !(Maybe ReductionEvidence)
    }
    deriving (Eq, Show)

type ReductionAction = StageReporter -> Logger -> OraclePool -> IO ReductionActionResult

-- | All inputs for one evaluated reduction run.
data EvaluationRequest = EvaluationRequest
    { requestOnProgress :: !(EvaluationProgress -> IO ())
    , requestMetricsPolicy :: !(Maybe MetricPolicy)
    , requestConfig :: !EvaluationConfig
    , requestDialect :: !SQLDialect
    , requestOracleConfig :: !OracleConfig
    , requestPreflightObservation :: !(Maybe OracleObservation)
    , requestLogger :: !Logger
    , requestInitialSQL :: !ByteString
    , requestCheckpoint :: !FilePath
    , requestAction :: !ReductionAction
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

-- | Execute an evaluated reduction workflow.
runEvaluatedReduction ::
    Maybe MetricPolicy ->
    EvaluationConfig ->
    SQLDialect ->
    OracleConfig ->
    Maybe OracleObservation ->
    Logger ->
    ByteString ->
    FilePath ->
    ReductionAction ->
    IO EvaluationResult
runEvaluatedReduction = runEvaluatedReductionWithProgress (\_ -> pure ())

runEvaluatedReductionWithProgress ::
    (EvaluationProgress -> IO ()) ->
    Maybe MetricPolicy ->
    EvaluationConfig ->
    SQLDialect ->
    OracleConfig ->
    Maybe OracleObservation ->
    Logger ->
    ByteString ->
    FilePath ->
    ReductionAction ->
    IO EvaluationResult
runEvaluatedReductionWithProgress onProgress metricsPolicy config dialect oracleCfg preflightObservation logger initialSQL checkpoint action =
    runEvaluationRequest
        EvaluationRequest
            { requestOnProgress = onProgress
            , requestMetricsPolicy = metricsPolicy
            , requestConfig = config
            , requestDialect = dialect
            , requestOracleConfig = oracleCfg
            , requestPreflightObservation = preflightObservation
            , requestLogger = logger
            , requestInitialSQL = initialSQL
            , requestCheckpoint = checkpoint
            , requestAction = action
            }

runEvaluationRequest :: EvaluationRequest -> IO EvaluationResult
runEvaluationRequest EvaluationRequest {..} = do
    let onProgress = requestOnProgress
        metricsPolicy = requestMetricsPolicy
        config = requestConfig
        dialect = requestDialect
        oracleCfg = requestOracleConfig
        preflightObservation = requestPreflightObservation
        parentLogger = requestLogger
        initialSQL = requestInitialSQL
        checkpoint = requestCheckpoint
        action = requestAction
    when (evaluationJobs config <= 0) $
        ioError (userError ("Evaluation jobs must be positive, got " ++ show (evaluationJobs config)))
    startedAt <- ticks
    let preflightMicros = max 0 (toInteger (evaluationPreflightMicros config))
        totalStart = startedAt - preflightMicros * 1000
    seedCounts <- case evaluationInitialStatements config of
        Just count -> pure (Map.singleton initialSQL count)
        Nothing -> do
            counted <- countStatements dialect initialSQL
            pure $ case counted of
                Left _ -> Map.empty
                Right count -> Map.singleton initialSQL count
    state <- newIORef (initialEvalState initialSQL seedCounts)
    writeAtomicBytes checkpoint initialSQL
    oracleCtx <- resolveOracleContext oracleCfg
    let oracleTimeout = max 1 (toInteger (oracleTimeoutMicros oracleCfg))
        validationSlack = max 1000 (min 10000 (oracleTimeout `div` 100))
        perValidationReserve = oracleTimeout + validationSlack
        callsAvailable = case evaluationMaxOracleExecutions config of
            Nothing -> 2
            Just limit ->
                min 2 (max 0 (limit - max 0 (evaluationPreflightAttempts config)))
        timeAvailable = fromInteger $ case evaluationTotalTimeoutMicros config of
            Nothing -> 2
            Just limit ->
                let remaining = max 0 (toInteger limit - preflightMicros)
                 in min 2 (remaining `div` perValidationReserve)
        validationCallLimit = min callsAvailable timeAvailable
        validationReserve = case evaluationTotalTimeoutMicros config of
            Nothing -> 0
            Just limit ->
                min
                    (max 0 (toInteger limit - preflightMicros))
                    (toInteger validationCallLimit * perValidationReserve)
        totalDeadline =
            (\limit -> totalStart + max 0 (toInteger limit) * 1000)
                <$> evaluationTotalTimeoutMicros config
        reductionCallLimit =
            (\limit -> max 0 (limit - max 0 (evaluationPreflightAttempts config) - validationCallLimit))
                <$> evaluationMaxOracleExecutions config
        precomputedObservations =
            maybe Map.empty (Map.singleton initialSQL) preflightObservation
    reductionStart <- ticks
    metricsStore <- traverse newMetricsStore metricsPolicy
    pool <-
        newOraclePool
            PoolConfig
                { poolJobs = evaluationJobs config
                , poolCacheEnabled = evaluationCacheEnabled config
                , poolPermits = reductionCallLimit
                , poolPrecomputed = precomputedObservations
                , poolMetrics = metricsStore
                , poolExecute =
                    observeExternalOracleDetailedWithContext
                        (modifyEvalState state (\s -> s {evalDatabase = evalDatabase s + 1}))
                        oracleCfg
                        oracleCtx
                , poolBill = recordReductionObservation state
                , poolRetire = retireCandidate onProgress reductionStart checkpoint state
                , poolReportCount =
                    Just
                        (\bytes count -> modifyEvalState state (\s -> s {evalCounts = Map.insert bytes count (evalCounts s)}))
                }
    let env =
            EvalEnv
                { envDialect = dialect
                , envOracleCfg = oracleCfg
                , envOracleCtx = oracleCtx
                , envLogger = parentLogger
                , envReductionStart = reductionStart
                , envTotalDeadline = totalDeadline
                , envValidationCallLimit = validationCallLimit
                , envValidationReserve = validationReserve
                , envPerValidationReserve = perValidationReserve
                }
        reporter = evalStageReporter onProgress state
    (reductionOutcome, reductionSeconds, costAttribution) <-
        runReductionPhase env state pool metricsStore reporter action
    (status, failure, skipValidation) <- case reductionOutcome of
        Right (Just _) -> pure (EvaluationCompleted, Nothing, False)
        Right Nothing -> pure (EvaluationTimeout, Just "Shared wall-clock budget expired during reduction", False)
        Left err
            | Just (ExecutionBudgetReached _) <- fromException err ->
                pure (EvaluationExecutionBudget, Nothing, False)
            | Just OracleReapFailed <- fromException err ->
                pure
                    ( EvaluationError
                    , Just "Oracle process group could not be reaped; refusing further oracle executions"
                    , True
                    )
            | otherwise -> pure (EvaluationError, Just (T.pack (displayException err)), False)
    preValidation <- readIORef state
    let acceptedFinal = evalAccepted preValidation
        auditResult = evalAuditResult preValidation
        finalSQL = fromMaybe acceptedFinal auditResult
        auditResultIsFinal =
            case auditResult of
                Just result -> result == finalSQL
                Nothing -> False
    validationPassed <- runValidationPhase env state finalSQL skipValidation
    cacheStats <- poolCacheStats pool
    attempts <- poolAttempts pool
    cancelled <- poolCancelled pool
    st <- readIORef state
    let curve = reverse (evalCurve st)
        stages = reverse (evalStages st)
        database = evalDatabase st
        completed = evalCompleted st
        acceptedCount = evalAcceptedCount st
        rejectedCount = evalRejectedCount st
        timedOut = evalTimedOut st
        resetFailures = evalResetFailures st
        errors = evalErrors st
        outcomes = evalOutcomes st
        validationObservations = evalValidationObs st
        validationDatabase = evalValidationDatabase st
        validationErrors = evalValidationErrors st
        validationRejected = evalValidationRejected st
        validationOutcomes = evalValidationOutcomes st
        oracleSeconds = evalOracleSeconds st
        resetSeconds = evalResetSeconds st
        validationOracleSeconds = evalValidationOracleSeconds st
        validationResetSeconds = evalValidationResetSeconds st
        validationSeconds = evalValidationSeconds st
        auditSeconds = evalAuditSeconds st
        auditChecks = evalAuditChecks st
        auditCompleted = evalAuditCompleted st
        auditSkipped = evalAuditSkipped st
    totalEnd <- ticks
    let finalStatus
            | status /= EvaluationCompleted = status
            | not validationPassed && validationRejected = EvaluationValidationFailed
            | not validationPassed && validationObservations < 2 = EvaluationValidationIncomplete
            | not validationPassed = EvaluationValidationFailed
            | otherwise = EvaluationCompleted
        finalFailure =
            case failure of
                Just err -> Just err
                Nothing
                    | validationRejected ->
                        Just "Final candidate failed target validation"
                Nothing
                    | not validationPassed && validationObservations < 2 ->
                        Just "Repeated final validation did not complete within the shared limits"
                Nothing
                    | not validationPassed ->
                        Just "Final candidate failed repeated target validation"
                Nothing -> Nothing
        metrics =
            EvaluationMetrics
                { evaluationStatus = finalStatus
                , evaluationFailure = finalFailure
                , evaluationCacheUsed = evaluationCacheEnabled config
                , evaluationBudget = config
                , evaluationTotalSeconds = seconds totalStart totalEnd
                , evaluationReductionSeconds = reductionSeconds
                , evaluationOracleSeconds = oracleSeconds
                , evaluationResetSeconds = resetSeconds
                , evaluationPreflightResetSeconds =
                    fmap ((/ 1000000) . fromIntegral) (evaluationPreflightResetMicros config)
                , evaluationPreflightOracleSeconds =
                    fmap ((/ 1000000) . fromIntegral) (evaluationPreflightOracleMicros config)
                , evaluationOracleAttempts = attempts
                , evaluationDatabaseObservations = database
                , evaluationCompletedOracleProcesses = completed
                , evaluationAcceptedObservations = acceptedCount
                , evaluationRejectedObservations = rejectedCount
                , evaluationTimedOutObservations = timedOut
                , evaluationResetFailures = resetFailures
                , evaluationOracleErrors = errors
                , evaluationCancelledObservations = cancelled
                , evaluationOracleOutcomes = outcomes
                , evaluationCacheHits = cacheHits cacheStats
                , evaluationCacheMisses = cacheMisses cacheStats
                , evaluationCacheJoins = cacheJoins cacheStats
                , evaluationValidationSeconds = validationSeconds
                , evaluationValidationOracleSeconds = validationOracleSeconds
                , evaluationValidationResetSeconds = validationResetSeconds
                , evaluationValidationObservations = validationObservations
                , evaluationValidationDatabaseObservations = validationDatabase
                , evaluationValidationErrors = validationErrors
                , evaluationValidationOutcomes = validationOutcomes
                , evaluationValidationPassed = validationPassed
                , evaluationAuditSeconds = auditSeconds
                , evaluationAuditChecks = auditChecks
                , evaluationAuditCompleted = auditCompleted
                , evaluationAuditSkipped = auditSkipped
                , evaluationMinimalityClaim =
                    if validationPassed && auditCompleted && auditResultIsFinal
                        then StatementDeletionOneMinimal
                        else MinimalityNotEstablished
                , evaluationCurve = curve
                , evaluationStages = stages
                , evaluationCostAttribution = costAttribution
                }
    pure EvaluationResult {evaluatedSQL = finalSQL, evaluationMetrics = metrics}

-- | Mutable reduction state shared across all execution phases.
data EvalState = EvalState
    { evalAccepted :: !ByteString
    , evalDatabase :: !Int
    , evalCompleted :: !Int
    , evalAcceptedCount :: !Int
    , evalRejectedCount :: !Int
    , evalTimedOut :: !Int
    , evalResetFailures :: !Int
    , evalErrors :: !Int
    , evalOutcomes :: !(Map OracleStatus Int)
    , evalOracleSeconds :: !Double
    , evalResetSeconds :: !Double
    , evalValidationObs :: !Int
    , evalValidationDatabase :: !Int
    , evalValidationErrors :: !Int
    , evalValidationOutcomes :: !(Map OracleStatus Int)
    , evalValidationRejected :: !Bool
    , evalValidationOracleSeconds :: !Double
    , evalValidationResetSeconds :: !Double
    , evalValidationSeconds :: !Double
    , evalCurve :: ![EvaluationPoint]
    , evalStages :: ![Value]
    , evalCounts :: !(Map ByteString Int)
    , evalAuditSeconds :: !Double
    , evalAuditChecks :: !Int
    , evalAuditInitialAccepted :: !Bool
    , evalAuditCompleted :: !Bool
    , evalAuditSkipped :: !Bool
    , evalAuditUnchanged :: !Bool
    , evalAuditResult :: !(Maybe ByteString)
    }
    deriving (Eq, Show)

initialEvalState :: ByteString -> Map ByteString Int -> EvalState
initialEvalState initialSQL seedCounts =
    EvalState
        { evalAccepted = initialSQL
        , evalDatabase = 0
        , evalCompleted = 0
        , evalAcceptedCount = 0
        , evalRejectedCount = 0
        , evalTimedOut = 0
        , evalResetFailures = 0
        , evalErrors = 0
        , evalOutcomes = Map.empty
        , evalOracleSeconds = 0
        , evalResetSeconds = 0
        , evalValidationObs = 0
        , evalValidationDatabase = 0
        , evalValidationErrors = 0
        , evalValidationOutcomes = Map.empty
        , evalValidationRejected = False
        , evalValidationOracleSeconds = 0
        , evalValidationResetSeconds = 0
        , evalValidationSeconds = 0
        , evalCurve = []
        , evalStages = []
        , evalCounts = seedCounts
        , evalAuditSeconds = 0
        , evalAuditChecks = 0
        , evalAuditInitialAccepted = False
        , evalAuditCompleted = False
        , evalAuditSkipped = False
        , evalAuditUnchanged = False
        , evalAuditResult = Nothing
        }

modifyEvalState :: IORef EvalState -> (EvalState -> EvalState) -> IO ()
modifyEvalState ref f = atomicModifyIORef' ref (\s -> (f s, ()))

-- | Static phase environment: handles, clocks, and budgets fixed at setup.
data EvalEnv = EvalEnv
    { envDialect :: !SQLDialect
    , envOracleCfg :: !OracleConfig
    , envOracleCtx :: !OracleContext
    , envLogger :: !Logger
    , envReductionStart :: !Ticks
    , envTotalDeadline :: !(Maybe Ticks)
    , envValidationCallLimit :: !Int
    , envValidationReserve :: !Integer
    , envPerValidationReserve :: !Integer
    }

boundedTimeout :: Integer -> Int
boundedTimeout micros =
    fromInteger (max 0 (min micros (toInteger (maxBound :: Int))))

-- | Execute the main reduction action and audit phase under budget constraints.
runReductionPhase ::
    EvalEnv ->
    IORef EvalState ->
    OraclePool ->
    Maybe MetricsStore ->
    StageReporter ->
    ReductionAction ->
    IO (Either SomeException (Maybe ReductionActionResult), Double, Maybe MetricsSnapshot)
runReductionPhase env state pool metricsStore reporter action = do
    reductionBudget <- case envTotalDeadline env of
        Nothing -> pure Nothing
        Just deadline -> do
            now <- ticks
            let remaining = max 0 ((deadline - now) `div` 1000)
                available = max 0 (remaining - envValidationReserve env)
            pure (Just available)
    outcome <-
        try $ case reductionBudget of
            Nothing -> Just <$> (runAction `finally` closeOraclePool pool)
            Just micros -> timeout (boundedTimeout micros) (runAction `finally` closeOraclePool pool)
    reductionEnd <- ticks
    let reductionSeconds = seconds (envReductionStart env) reductionEnd
    -- Snapshot cost attribution after workers are drained.
    costAttribution <- traverse snapshotMetrics metricsStore
    pure (outcome, reductionSeconds, costAttribution)
    where
        runAction = do
            result <- action reporter (envLogger env) pool
            runAuditPhase env state pool reporter (actionEvidence result)
            pure result

-- | Run statement-deletion audit to confirm 1-minimality on the accepted result.
runAuditPhase ::
    EvalEnv -> IORef EvalState -> OraclePool -> StageReporter -> Maybe ReductionEvidence -> IO ()
runAuditPhase env state pool reporter evidence = do
    auditInput <- evalAccepted <$> readIORef state
    case evidence of
        Just ReductionEvidence {..}
            | evidenceAuditMinimal && evidenceBytes == auditInput -> do
                modifyEvalState state \s ->
                    s
                        { evalAuditSeconds = 0
                        , evalAuditCompleted = True
                        , evalAuditSkipped = True
                        , evalAuditUnchanged = True
                        , evalAuditInitialAccepted = True
                        , evalAuditResult = Just auditInput
                        }
                logDebug'
                    (envLogger env)
                    "audit"
                    "Shared statement audit skipped (already confirmed 1-minimal)"
                    [ ("audit_skipped", toJSON True)
                    , ("audit_minimality_confirmed", toJSON True)
                    ]
        _ -> do
            auditStart <- ticks
            let auditPool =
                    mapPoolRetire
                        ( \retire index candidate outcome hit -> do
                            priorChecks <- evalAuditChecks <$> readIORef state
                            modifyEvalState state (\s -> s {evalAuditChecks = evalAuditChecks s + 1})
                            when (priorChecks == 0) $
                                modifyEvalState state (\s -> s {evalAuditInitialAccepted = outcome == OracleAccepted})
                            retire index candidate outcome hit
                        )
                        pool
                finishAudit = do
                    auditEnd <- ticks
                    let elapsed = seconds auditStart auditEnd
                    st <- readIORef state
                    let complete = evalAuditCompleted st
                        checks = evalAuditChecks st
                        unchanged = evalAuditUnchanged st
                    modifyEvalState state (\s -> s {evalAuditSeconds = elapsed})
                    logInfo'
                        (envLogger env)
                        "audit"
                        "Shared statement audit finished"
                        [ ("elapsed_seconds", toJSON elapsed)
                        , ("candidate_checks", toJSON checks)
                        , ("completed", toJSON complete)
                        , ("candidate_unchanged", toJSON unchanged)
                        , ("audit_minimality_confirmed", toJSON complete)
                        , ("skipped", toJSON False)
                        ]
            reporter (StageStarted StageStatementAudit)
            result <-
                ( do
                    (audited, stats) <-
                        statementAuditWithLogger (envLogger env) (envDialect env) auditPool auditInput
                    initialAccepted <- evalAuditInitialAccepted <$> readIORef state
                    modifyEvalState state \s ->
                        s
                            { evalAuditCompleted = auditMinimalityConfirmed stats
                            , evalAuditUnchanged = initialAccepted && audited == auditInput
                            , evalAuditResult = Just audited
                            }
                    pure audited
                )
                    `finally` finishAudit
            auditEnd <- ticks
            reporter
                ( StageFinished
                    StageStatementAudit
                    (seconds auditStart auditEnd)
                    (BS.length auditInput)
                    (BS.length result)
                )

-- | Validation phase: repeat the final check twice within the shared limits.
runValidationPhase :: EvalEnv -> IORef EvalState -> ByteString -> Bool -> IO Bool
runValidationPhase env state finalSQL skipValidation
    | skipValidation = pure False
    | otherwise = do
        first <- validateOnce finalSQL
        second <- validateOnce finalSQL
        pure (first == Just OracleAccepted && second == Just OracleAccepted)
    where
        validateOnce candidate = do
            prior <- evalValidationObs <$> readIORef state
            if prior >= envValidationCallLimit env
                then pure Nothing
                else do
                    start <- ticks
                    let remainingMicros = (`div` 1000) . subtract start <$> envTotalDeadline env
                    case remainingMicros of
                        Just remaining | remaining < envPerValidationReserve env -> pure Nothing
                        _ -> do
                            modifyEvalState state (\s -> s {evalValidationObs = evalValidationObs s + 1})
                            let observe =
                                    observeExternalOracleDetailedWithContext
                                        (modifyEvalState state (\s -> s {evalValidationDatabase = evalValidationDatabase s + 1}))
                                        (envOracleCfg env)
                                        (envOracleCtx env)
                                        candidate
                            result <-
                                case remainingMicros of
                                    Nothing -> Just <$> observe
                                    Just remaining ->
                                        timeout (boundedTimeout (max 0 remaining)) observe
                            end <- ticks
                            modifyEvalState
                                state
                                (\s -> s {evalValidationSeconds = evalValidationSeconds s + seconds start end})
                            case result of
                                Nothing -> do
                                    let observation = OracleTimedOut
                                    modifyEvalState state (\s -> s {evalValidationErrors = evalValidationErrors s + 1})
                                    recordValidationOutcome state observation
                                    pure (Just OracleInconclusive)
                                Just profile -> do
                                    let observation = detailedObservation profile
                                        micros = fromIntegral
                                    modifyEvalState
                                        state
                                        ( \s ->
                                            s
                                                { evalValidationResetSeconds =
                                                    evalValidationResetSeconds s + micros (resetElapsedMicros profile) / 1000000
                                                }
                                        )
                                    modifyEvalState
                                        state
                                        ( \s ->
                                            s
                                                { evalValidationOracleSeconds =
                                                    evalValidationOracleSeconds s + maybe 0 micros (oracleElapsedMicros profile) / 1000000
                                                }
                                        )
                                    recordValidationOutcome state observation
                                    case observationOutcome observation of
                                        OracleAccepted -> pure (Just OracleAccepted)
                                        OracleRejected -> do
                                            modifyEvalState state (\s -> s {evalValidationRejected = True})
                                            pure (Just OracleRejected)
                                        OracleInconclusive -> do
                                            modifyEvalState state (\s -> s {evalValidationErrors = evalValidationErrors s + 1})
                                            pure (Just OracleInconclusive)

-- | Atomically record worker observation metrics.
recordReductionObservation :: IORef EvalState -> OracleObservationDetails -> IO ()
recordReductionObservation state profile = do
    let observation = detailedObservation profile
        micros = fromIntegral
        resetDelta = micros (resetElapsedMicros profile) / 1000000
        oracleDelta = maybe 0 (\oracleMicros -> fromIntegral oracleMicros / 1000000) (oracleElapsedMicros profile)
        outcome = classifyOracleObservation observation
    modifyEvalState state \s ->
        let billed =
                s
                    { evalResetSeconds = evalResetSeconds s + resetDelta
                    , evalOracleSeconds = evalOracleSeconds s + oracleDelta
                    , evalOutcomes = Map.insertWith (+) outcome 1 (evalOutcomes s)
                    }
         in case observation of
                OracleFinished _ ->
                    billed
                        { evalCompleted = evalCompleted s + 1
                        , evalAcceptedCount =
                            evalAcceptedCount s
                                + if oracleObservationMatches observation then 1 else 0
                        , evalRejectedCount =
                            evalRejectedCount s
                                + if oracleObservationMatches observation then 0 else 1
                        }
                OracleTimedOut ->
                    billed {evalTimedOut = evalTimedOut s + 1, evalErrors = evalErrors s + 1}
                OracleResetFailed ->
                    billed {evalResetFailures = evalResetFailures s + 1, evalErrors = evalErrors s + 1}
                OracleIOFailed _ -> billed {evalErrors = evalErrors s + 1}

recordValidationOutcome :: IORef EvalState -> OracleObservation -> IO ()
recordValidationOutcome state observation =
    modifyEvalState
        state
        ( \s ->
            s
                { evalValidationOutcomes =
                    Map.insertWith (+) (classifyOracleObservation observation) 1 (evalValidationOutcomes s)
                }
        )

-- | Update progress curve and checkpoints upon candidate retirement.
retireCandidate ::
    (EvaluationProgress -> IO ()) ->
    Ticks ->
    FilePath ->
    IORef EvalState ->
    Int ->
    ByteString ->
    OracleOutcome ->
    Bool ->
    IO ()
retireCandidate onProgress reductionStart checkpoint state callIndex candidate outcome hit = do
    let accepted = outcome == OracleAccepted
    -- Retrieve cached statement count for accepted candidate.
    statements <-
        if accepted
            then Map.lookup candidate . evalCounts <$> readIORef state
            else pure Nothing
    onProgress (ProgressCandidateTested (callIndex + 1) hit accepted)
    when accepted $ do
        smaller <- checkpointIfSmaller state checkpoint candidate
        when smaller $
            onProgress (ProgressCandidateAccepted (BS.length candidate) statements)
    now <- ticks
    modifyEvalState state \s ->
        s
            { evalCurve =
                EvaluationPoint
                    { pointElapsedSeconds = seconds reductionStart now
                    , pointCallIndex = callIndex + 1
                    , pointCandidateBytes = BS.length candidate
                    , pointCandidateStatements = statements
                    , pointCacheHit = hit
                    , pointAccepted = accepted
                    }
                    : evalCurve s
            }

-- | Route reducer stage events to UI progress and per-stage summaries.
evalStageReporter :: (EvaluationProgress -> IO ()) -> IORef EvalState -> StageReporter
evalStageReporter onProgress state = \case
    StageStarted stage -> onProgress (ProgressStage (reductionStageName stage))
    StageFinished stage elapsed before after ->
        modifyEvalState
            state
            (\s -> s {evalStages = stageSummary stage elapsed before after : evalStages s})

stageSummary :: ReductionStage -> Double -> Int -> Int -> Value
stageSummary stage elapsed before after =
    object
        [ "stage" .= reductionStageName stage
        , "seconds" .= elapsed
        , "bytes_before" .= before
        , "bytes_after" .= after
        ]

checkpointIfSmaller :: IORef EvalState -> FilePath -> ByteString -> IO Bool
checkpointIfSmaller state path candidate = do
    current <- evalAccepted <$> readIORef state
    if BS.length candidate < BS.length current
        then mask_ $ do
            writeAtomicBytes path candidate
            modifyEvalState state (\s -> s {evalAccepted = candidate})
            pure True
        else pure False

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationPoint (..),
    EvaluationResult (..),
    parseSearchStrategy,
    searchStrategyName,
    parseSQLDialect,
    sqlDialectName,
    runEvaluatedReduction,
) where

import Control.Exception (
    AsyncException,
    Exception,
    displayException,
    finally,
    fromException,
    onException,
    throwIO,
    try,
 )
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, Value, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Occam.Cache (CacheStats (..), memoizeOracleWith)
import Occam.Logger (Logger (..), logInfo')
import Occam.Oracle (
    OracleConfig (..),
    OracleObservation (..),
    OracleObservationDetails (..),
    classifyOracleObservation,
    observeExternalOracleDetailedWith,
    oracleObservationMatches,
 )
import Occam.Parser (SQLDialect (..), countStatements)
import Occam.Reducer (
    Oracle,
    SearchStrategy (..),
    statementAudit,
 )
import System.Directory (renameFile)
import System.Timeout (timeout)

data EvaluationConfig = EvaluationConfig
    { evaluationCacheEnabled :: !Bool
    , evaluationTotalTimeoutMicros :: !(Maybe Int)
    , evaluationMaxOracleExecutions :: !(Maybe Int)
    , evaluationPreflightMicros :: !Int
    , evaluationPreflightAttempts :: !Int
    , evaluationPreflightResetMicros :: !(Maybe Int)
    , evaluationPreflightOracleMicros :: !(Maybe Int)
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvaluationConfig
instance FromJSON EvaluationConfig

parseSearchStrategy :: String -> Either String SearchStrategy
parseSearchStrategy value = case map toLower value of
    "count-dd" -> Right CountDD
    "weighted-dd" -> Right WeightedDD
    "cdd" -> Right CDD
    "dominator-dd" -> Right DominatorDD
    "dominator" -> Right DominatorDD
    _ -> Left "Expected count-dd, weighted-dd, cdd, or dominator-dd"

searchStrategyName :: SearchStrategy -> Text
searchStrategyName CountDD = "count-dd"
searchStrategyName WeightedDD = "weighted-dd"
searchStrategyName CDD = "cdd"
searchStrategyName DominatorDD = "dominator-dd"
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
        Left
            "Expected generic, postgresql, mysql, sqlite, duckdb, clickhouse, oracle, ansi, bigquery, databricks, hive, mssql, redshift, snowflake, spark, or teradata"

sqlDialectName :: SQLDialect -> Text
sqlDialectName GenericSQL = "generic"
sqlDialectName PostgreSQL = "postgresql"
sqlDialectName MySQL = "mysql"
sqlDialectName SQLite = "sqlite"
sqlDialectName DuckDB = "duckdb"
sqlDialectName ClickHouse = "clickhouse"
sqlDialectName Oracle = "oracle"
sqlDialectName Ansi = "ansi"
sqlDialectName BigQuery = "bigquery"
sqlDialectName Databricks = "databricks"
sqlDialectName Hive = "hive"
sqlDialectName MsSql = "mssql"
sqlDialectName Redshift = "redshift"
sqlDialectName Snowflake = "snowflake"
sqlDialectName Spark = "spark"
sqlDialectName Teradata = "teradata"

data EvaluationPoint = EvaluationPoint
    { pointElapsedSeconds :: !Double
    , pointCallIndex :: !Int
    , pointCandidateBytes :: !Int
    , pointCandidateStatements :: !Int
    , pointCandidateTokens :: !(Maybe Int)
    , pointCacheHit :: !Bool
    , pointAccepted :: !Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvaluationPoint
instance FromJSON EvaluationPoint

data EvaluationMetrics = EvaluationMetrics
    { evaluationStatus :: !Text
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
    , evaluationOracleOutcomes :: !(Map Text Int)
    , evaluationCacheHits :: !Int
    , evaluationCacheMisses :: !Int
    , evaluationValidationRepeats :: !Int
    , evaluationValidationSeconds :: !Double
    , evaluationValidationOracleSeconds :: !Double
    , evaluationValidationResetSeconds :: !Double
    , evaluationValidationObservations :: !Int
    , evaluationValidationDatabaseObservations :: !Int
    , evaluationValidationErrors :: !Int
    , evaluationValidationOutcomes :: !(Map Text Int)
    , evaluationValidationPassed :: !Bool
    , evaluationAuditSeconds :: !Double
    , evaluationAuditChecks :: !Int
    , evaluationAuditCompleted :: !Bool
    , evaluationMinimalityClaim :: !Text
    , evaluationCurve :: ![EvaluationPoint]
    , evaluationStages :: ![Value]
    }
    deriving (Eq, Show, Generic)

instance ToJSON EvaluationMetrics
instance FromJSON EvaluationMetrics

data EvaluationResult = EvaluationResult
    { evaluatedSQL :: !ByteString
    , evaluationMetrics :: !EvaluationMetrics
    }

newtype ExecutionBudgetReached = ExecutionBudgetReached ()
    deriving (Show)

instance Exception ExecutionBudgetReached

type ReductionAction = Logger -> Oracle -> IO ByteString

type Ticks = Integer

ticks :: IO Ticks
ticks = toInteger <$> getMonotonicTimeNSec

seconds :: Ticks -> Ticks -> Double
seconds start end = fromIntegral (end - start) / 1000000000

{- | Run one reducer with shared budgets/cache/audit policy, uncached repeated
final checks, and an atomically persisted best accepted result.
-}
runEvaluatedReduction ::
    EvaluationConfig ->
    SQLDialect ->
    OracleConfig ->
    Logger ->
    ByteString ->
    FilePath ->
    ReductionAction ->
    IO EvaluationResult
runEvaluatedReduction config dialect oracleCfg parentLogger initialSQL checkpoint action = do
    startedAt <- ticks
    let preflightMicros = max 0 (toInteger (evaluationPreflightMicros config))
        totalStart = startedAt - preflightMicros * 1000
    acceptedRef <- newIORef initialSQL
    attemptsRef <- newIORef (0 :: Int)
    databaseRef <- newIORef (0 :: Int)
    completedRef <- newIORef (0 :: Int)
    acceptedCountRef <- newIORef (0 :: Int)
    rejectedCountRef <- newIORef (0 :: Int)
    timedOutRef <- newIORef (0 :: Int)
    resetFailureRef <- newIORef (0 :: Int)
    errorsRef <- newIORef (0 :: Int)
    outcomesRef <- newIORef Map.empty
    oracleSecondsRef <- newIORef (0.0 :: Double)
    resetSecondsRef <- newIORef (0.0 :: Double)
    validationObsRef <- newIORef (0 :: Int)
    validationDatabaseRef <- newIORef (0 :: Int)
    validationErrorsRef <- newIORef (0 :: Int)
    validationOutcomesRef <- newIORef Map.empty
    validationRejectedRef <- newIORef False
    validationOracleSecondsRef <- newIORef (0.0 :: Double)
    validationResetSecondsRef <- newIORef (0.0 :: Double)
    validationSecondsRef <- newIORef (0.0 :: Double)
    curveRef <- newIORef ([] :: [EvaluationPoint])
    stagesRef <- newIORef ([] :: [Value])
    callIndexRef <- newIORef (0 :: Int)
    auditSecondsRef <- newIORef (0.0 :: Double)
    auditChecksRef <- newIORef (0 :: Int)
    auditInitialAcceptedRef <- newIORef False
    auditCompletedRef <- newIORef False
    auditUnchangedRef <- newIORef False
    auditInputRef <- newIORef initialSQL
    writeCheckpoint initialSQL

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
        boundedTimeout micros =
            fromInteger (max 0 (min micros (toInteger (maxBound :: Int))))
        recordOutcome outcomes observation =
            modifyIORef' outcomes $
                Map.insertWith (+) (T.pack (show (classifyOracleObservation observation))) 1
        recordInterrupted = do
            modifyIORef' timedOutRef (+ 1)
            modifyIORef' errorsRef (+ 1)
            modifyIORef' outcomesRef (Map.insertWith (+) "interrupted" 1)
        recordReductionObservation profile = do
            let observation = detailedObservation profile
                micros = fromIntegral
            modifyIORef' resetSecondsRef (+ micros (resetElapsedMicros profile) / 1000000)
            case oracleElapsedMicros profile of
                Just oracleMicros -> modifyIORef' oracleSecondsRef (+ micros oracleMicros / 1000000)
                Nothing -> pure ()
            recordOutcome outcomesRef observation
            case observation of
                OracleFinished _ -> do
                    modifyIORef' completedRef (+ 1)
                    if oracleObservationMatches observation
                        then modifyIORef' acceptedCountRef (+ 1)
                        else modifyIORef' rejectedCountRef (+ 1)
                OracleTimedOut -> do
                    modifyIORef' timedOutRef (+ 1)
                    modifyIORef' errorsRef (+ 1)
                OracleResetFailed -> do
                    modifyIORef' resetFailureRef (+ 1)
                    modifyIORef' errorsRef (+ 1)
                OracleIOFailed _ -> modifyIORef' errorsRef (+ 1)
    let runObservation candidate = do
            attempts <- readIORef attemptsRef
            case reductionCallLimit of
                Just limit | attempts >= limit -> throwIO (ExecutionBudgetReached ())
                _ -> pure ()
            modifyIORef' attemptsRef (+ 1)
            profile <-
                observeExternalOracleDetailedWith
                    (modifyIORef' databaseRef (+ 1))
                    oracleCfg
                    candidate
                    `onException` recordInterrupted
            recordReductionObservation profile
            pure (detailedObservation profile)
    (cachedObservation, getCacheStats) <-
        memoizeOracleWith
            (evaluationCacheEnabled config)
            isCompleted
            runObservation
    reductionStart <- ticks
    let measuredOracle persistCheckpoint candidate = do
            before <- getCacheStats
            callIndex <- readIORef callIndexRef
            modifyIORef' callIndexRef (+ 1)
            statementsResult <- countStatements dialect candidate
            statements <- case statementsResult of
                Left err -> ioError (userError ("SQL parser failed while counting statements: " ++ show err))
                Right count -> pure count
            result <- cachedObservation candidate
            after <- getCacheStats
            let hit = cacheHits after > cacheHits before
                accepted = oracleObservationMatches result
            when (persistCheckpoint && accepted) $ checkpointIfSmaller acceptedRef checkpoint candidate
            now <- ticks
            modifyIORef' curveRef $ \points ->
                EvaluationPoint
                    { pointElapsedSeconds = seconds reductionStart now
                    , pointCallIndex = callIndex + 1
                    , pointCandidateBytes = BS.length candidate
                    , pointCandidateStatements = statements
                    , pointCandidateTokens = Nothing
                    , pointCacheHit = hit
                    , pointAccepted = accepted
                    }
                    : points
            pure accepted
        captureLogger =
            parentLogger
                { loggerAction = \level component message metadata -> do
                    modifyIORef' stagesRef (event level component message metadata :)
                    loggerAction parentLogger level component message metadata
                }
        runAction = do
            reduced <- action captureLogger (measuredOracle True)
            auditInput <- readIORef acceptedRef
            writeIORef auditInputRef auditInput
            auditStart <- ticks
            let auditOracle candidate = do
                    priorChecks <- readIORef auditChecksRef
                    modifyIORef' auditChecksRef (+ 1)
                    accepted <- measuredOracle False candidate
                    when (priorChecks == 0) (writeIORef auditInitialAcceptedRef accepted)
                    pure accepted
                finishAudit = do
                    auditEnd <- ticks
                    let elapsed = seconds auditStart auditEnd
                    writeIORef auditSecondsRef elapsed
                    complete <- readIORef auditCompletedRef
                    checks <- readIORef auditChecksRef
                    unchanged <- readIORef auditUnchangedRef
                    logInfo'
                        captureLogger
                        "occam::audit"
                        "Shared statement audit finished"
                        [ ("elapsed_seconds", toJSON elapsed)
                        , ("candidate_checks", toJSON checks)
                        , ("completed", toJSON complete)
                        , ("candidate_unchanged", toJSON unchanged)
                        , ("minimality_claim", toJSON ("not-established" :: Text))
                        ]
            _ <-
                ( do
                    result <- statementAudit dialect auditOracle auditInput
                    initialAccepted <- readIORef auditInitialAcceptedRef
                    writeIORef auditCompletedRef True
                    writeIORef auditUnchangedRef (initialAccepted && result == auditInput)
                    pure result
                )
                    `finally` finishAudit
            pure reduced

    reductionBudget <- case totalDeadline of
        Nothing -> pure Nothing
        Just deadline -> do
            now <- ticks
            let remaining = max 0 ((deadline - now) `div` 1000)
                available = max 0 (remaining - validationReserve)
            pure (Just available)
    reductionOutcome <-
        try $ case reductionBudget of
            Nothing -> Just <$> runAction
            Just micros -> timeout (boundedTimeout micros) runAction
    reductionEnd <- ticks
    let reductionSeconds = seconds reductionStart reductionEnd
    (status, failure) <- case reductionOutcome of
        Right (Just _) -> pure ("completed", Nothing)
        Right Nothing -> pure ("timeout", Just "Shared wall-clock budget expired during reduction")
        Left err
            | Just (ExecutionBudgetReached _) <- fromException err -> pure ("execution-budget", Nothing)
            | Just asyncErr <- fromException err -> throwIO (asyncErr :: AsyncException)
            | otherwise -> pure ("error", Just (T.pack (displayException err)))

    finalSQL <- readIORef acceptedRef
    let validateOnce candidate = do
            prior <- readIORef validationObsRef
            if prior >= validationCallLimit
                then pure Nothing
                else do
                    start <- ticks
                    let remainingMicros = (`div` 1000) . subtract start <$> totalDeadline
                    case remainingMicros of
                        Just remaining | remaining < perValidationReserve -> pure Nothing
                        _ -> do
                            modifyIORef' validationObsRef (+ 1)
                            result <-
                                case remainingMicros of
                                    Nothing ->
                                        Just
                                            <$> observeExternalOracleDetailedWith (modifyIORef' validationDatabaseRef (+ 1)) oracleCfg candidate
                                    Just remaining ->
                                        timeout
                                            (boundedTimeout (max 0 remaining))
                                            (observeExternalOracleDetailedWith (modifyIORef' validationDatabaseRef (+ 1)) oracleCfg candidate)
                            end <- ticks
                            modifyIORef' validationSecondsRef (+ seconds start end)
                            case result of
                                Nothing -> do
                                    let observation = OracleTimedOut
                                    modifyIORef' validationErrorsRef (+ 1)
                                    recordOutcome validationOutcomesRef observation
                                    pure (Just False)
                                Just profile -> do
                                    let observation = detailedObservation profile
                                        micros = fromIntegral
                                    modifyIORef' validationResetSecondsRef (+ micros (resetElapsedMicros profile) / 1000000)
                                    modifyIORef' validationOracleSecondsRef (+ maybe 0 micros (oracleElapsedMicros profile) / 1000000)
                                    recordOutcome validationOutcomesRef observation
                                    case observation of
                                        OracleFinished _ -> do
                                            let accepted = oracleObservationMatches observation
                                            unless accepted (writeIORef validationRejectedRef True)
                                            pure (Just accepted)
                                        OracleTimedOut -> modifyIORef' validationErrorsRef (+ 1) >> pure (Just False)
                                        OracleResetFailed -> modifyIORef' validationErrorsRef (+ 1) >> pure (Just False)
                                        OracleIOFailed _ -> modifyIORef' validationErrorsRef (+ 1) >> pure (Just False)
        validateTwice candidate = do
            first <- validateOnce candidate
            second <- validateOnce candidate
            pure (first == Just True && second == Just True)
    validationPassed <- validateTwice finalSQL
    cacheStats <- getCacheStats
    curve <- reverse <$> readIORef curveRef
    stages <- reverse <$> readIORef stagesRef
    attempts <- readIORef attemptsRef
    database <- readIORef databaseRef
    completed <- readIORef completedRef
    acceptedCount <- readIORef acceptedCountRef
    rejectedCount <- readIORef rejectedCountRef
    timedOut <- readIORef timedOutRef
    resetFailures <- readIORef resetFailureRef
    errors <- readIORef errorsRef
    outcomes <- readIORef outcomesRef
    validationObservations <- readIORef validationObsRef
    validationDatabase <- readIORef validationDatabaseRef
    validationErrors <- readIORef validationErrorsRef
    validationRejected <- readIORef validationRejectedRef
    validationOutcomes <- readIORef validationOutcomesRef
    oracleSeconds <- readIORef oracleSecondsRef
    resetSeconds <- readIORef resetSecondsRef
    validationOracleSeconds <- readIORef validationOracleSecondsRef
    validationResetSeconds <- readIORef validationResetSecondsRef
    validationSeconds <- readIORef validationSecondsRef
    auditSeconds <- readIORef auditSecondsRef
    auditChecks <- readIORef auditChecksRef
    auditCompleted <- readIORef auditCompletedRef
    auditUnchanged <- readIORef auditUnchangedRef
    auditInput <- readIORef auditInputRef
    totalEnd <- ticks
    let finalStatus
            | status /= "completed" = status
            | not validationPassed && validationRejected = "validation-failed"
            | not validationPassed && validationObservations < 2 = "validation-incomplete"
            | not validationPassed = "validation-failed"
            | otherwise = "completed"
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
                , evaluationOracleOutcomes = outcomes
                , evaluationCacheHits = cacheHits cacheStats
                , evaluationCacheMisses = cacheMisses cacheStats
                , evaluationValidationRepeats = validationObservations
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
                , evaluationMinimalityClaim =
                    if validationPassed && auditCompleted && auditUnchanged && finalSQL == auditInput
                        then "statement-deletion-1-minimal"
                        else "not-established"
                , evaluationCurve = curve
                , evaluationStages = stages
                }
    pure EvaluationResult {evaluatedSQL = finalSQL, evaluationMetrics = metrics}
    where
        writeCheckpoint sql = do
            let temporary = checkpoint ++ ".tmp"
            BS.writeFile temporary sql
            renameFile temporary checkpoint

        checkpointIfSmaller acceptedRef path candidate = do
            current <- readIORef acceptedRef
            when (BS.length candidate < BS.length current) $ do
                let temporary = path ++ ".tmp"
                BS.writeFile temporary candidate
                renameFile temporary path
                writeIORef acceptedRef candidate

event :: (Show a) => a -> Text -> Text -> [(Text, Value)] -> Value
event level component message metadata =
    object
        [ "level" .= show level
        , "component" .= component
        , "message" .= message
        , "metadata" .= object [(Key.fromText key, value) | (key, value) <- metadata]
        ]

isCompleted :: OracleObservation -> Bool
isCompleted OracleFinished {} = True
isCompleted _ = False

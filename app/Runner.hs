-- | CLI reduction orchestration connecting oracle preparation, reduction strategies, and outputs.
module Runner (
    runOccam,
) where

import Control.Monad (when)
import Data.Aeson (toJSON)
import Data.ByteString qualified as BS
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Certificate (FailingPrefixCache, newFailingPrefixCache)
import Occam.Core (ResultTrace, SessionInit)
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationObserver,
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    ReductionActionResult (..),
    Stats (..),
    metricPolicyForReduction,
    runEvaluationRequestWithNativeValidation,
    runEvaluationRequestWithObserver,
    sqlDialectName,
 )
import Occam.Logger (
    LogConfig (..),
    LogFormat (..),
    LogLevel (..),
    Logger,
    logError,
    logInfo',
    logWarn,
    withLogger,
 )
import Occam.Metrics (MetricPolicy (..))
import Occam.Oracle (
    OracleObservationDetails (..),
    observeExternalOracleDetailedWithContext,
    prepareOracleWithContext,
    resolveOracleContext,
    shellOracleConfig,
    withCandidateWorkdir,
 )
import Occam.Oracle.Inline (newResultTraceTracker, prepareResultOracleWithValidation)
import Occam.Parser (
    ParsedSQL (..),
    SQLDialect (..),
    StatementUnit,
    defaultParseOptions,
    parseSQL,
    spanOnlyOptions,
 )
import Occam.Pool (OraclePool, poolMetricsStore)
import Occam.Reducer (
    AuditStats (..),
    ColumnMode (..),
    ReductionConfig (..),
    ReductionEvidence (..),
    ReductionResult (..),
    ReductionStage (..),
    StageEvent (..),
    StageReporter,
    StatementSearchPolicy,
    columnModeName,
    defaultReductionConfigFor,
    parseStatementsFast,
    reduceSQLWithResultTraceFull,
    reuseLatestSQLWithSeed,
    statementAuditWithParser,
    statementDdminWithParser,
 )
import Occam.UI (
    FinishSummary (..),
    UI (..),
    UIConfig (..),
    newUI,
 )
import Options (
    ReductionStrategy (..),
    Specified (..),
    ValidatedOptions (..),
    getSpecified,
    resolveColor,
    strategyName,
 )
import Output (
    computeSessionStats,
    countStatementsForCLI,
    dumpStatementFacts,
    exitCliError,
    exitTimeout,
    readInputBytes,
    resolveOutputTargets,
    writeReductionOutputs,
 )
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr)
import System.Timeout (timeout)

-- | Initialized oracle callbacks, preflight timing, budget config, and optional result trace lookup.
data PreparedOracle = PreparedOracle
    { poInitialStart :: !UTCTime
    , poInitialEnd :: !UTCTime
    , poPreflightDetails :: !OracleObservationDetails
    , poObserver :: !EvaluationObserver
    , poValidate :: !EvaluationObserver
    , poBudgetConfig :: !EvaluationConfig
    , poFailPrefixCache :: !(Maybe FailingPrefixCache)
    , poLookupTrace :: !(BS.ByteString -> IO (Maybe ResultTrace))
    }

resolveLogLevel :: ValidatedOptions -> LogLevel
resolveLogLevel ValidatedOptions {..}
    | valQuiet = LevelError
    | valVerbose || getSpecified valLogLevel == LevelDebug = LevelDebug
    | otherwise = case valLogLevel of
        Explicit lvl -> lvl
        Default defLvl
            | valLogFormat == FormatJSON -> LevelInfo
            | otherwise -> defLvl

-- | Run the full CLI reduction pipeline from validated options.
runOccam :: ValidatedOptions -> IO ()
runOccam opts@ValidatedOptions {..} = do
    isStderrTTY <- hIsTerminalDevice stderr
    useColor <- resolveColor valColor isStderrTTY
    let logCfg =
            LogConfig
                { logMinLevel = resolveLogLevel opts
                , logFormat = valLogFormat
                , logUseColor = useColor
                }
    withLogger logCfg (\logger -> runOccamWithLogger logger useColor opts)

runOccamWithLogger :: Logger -> Bool -> ValidatedOptions -> IO ()
runOccamWithLogger logger useColor opts@ValidatedOptions {..} = do
    when valDumpFacts do
        dumpStatementFacts valDialect valDumpFactsFkOn valInputFile
        exitSuccess

    oracleCmd <- if valResultOracle then pure Nothing else Just <$> requireOracleCommand valOracleCmd
    (writeToStdout, targetMinimalPath, statsPath) <-
        resolveOutputTargets valInputFile valOut valInPlace valStatsFile
    let spanOnly = valStrategy /= StrategyFull
    (initialSQL, initialStmts, initialBytes, initialParsed) <-
        loadInitialInput logger valDialect True spanOnly valInputFile

    ui <-
        newUI
            UIConfig
                { uiTargetFile = valInputFile
                , uiInitialBytes = initialBytes
                , uiInitialStmts = initialStmts
                , uiDialect = sqlDialectName valDialect
                , uiQuiet = valQuiet
                , uiVerbose = valVerbose || getSpecified valLogLevel == LevelDebug
                , uiUseColor = useColor
                }

    let timeoutMicros = round (valTimeoutSecs * 1000000)
        reductionConfig = buildReductionConfigFromOptions opts
        configuredBudget = buildBudgetConfig valCache valJobs valBudgetSecs valMaxOracleExecutions initialStmts

    logInfo'
        logger
        "cli"
        "Starting reduction session"
        [ ("input_file", toJSON valInputFile)
        , ("initial_bytes", toJSON initialBytes)
        , ("initial_statements", toJSON initialStmts)
        , ("oracle_cmd", toJSON oracleCmd)
        , ("jobs", toJSON valJobs)
        , ("strategy", toJSON (strategyName valStrategy))
        ]

    prepared <-
        prepareSessionOracle opts oracleCmd timeoutMicros reductionConfig configuredBudget initialSQL

    logInfo'
        logger
        "oracle"
        "Initial predicate check passed"
        [ ("initial_statements", toJSON initialStmts)
        , ("initial_bytes", toJSON initialBytes)
        ]

    when (valJobs > 1 && isNothing valResetCmd && not valResultOracle) do
        logWarn
            logger
            "cli"
            "running with --jobs > 1 without --reset may lead to shared database state contamination"

    metricsPolicy <- resolveMetricsPolicy logger valStatsFile valStrategy reductionConfig valJobs
    let reductionAction =
            buildReductionAction
                valStrategy
                initialParsed
                (poFailPrefixCache prepared)
                (poLookupTrace prepared)
                reductionConfig
                initialSQL

    result <-
        withCandidateWorkdir $ \_ -> do
            let runEvaluation
                    | isNothing oracleCmd =
                        runEvaluationRequestWithNativeValidation
                            timeoutMicros
                            (poObserver prepared)
                            (poValidate prepared)
                    | otherwise =
                        runEvaluationRequestWithObserver timeoutMicros (poObserver prepared)
            runEvaluation
                EvaluationRequest
                    { requestOnProgress = uiOnProgress ui
                    , requestMetricsPolicy = metricsPolicy
                    , requestConfig = poBudgetConfig prepared
                    , requestDialect = valDialect
                    , requestOracleConfig = ()
                    , requestPreflightObservation = Just (detailedObservation (poPreflightDetails prepared))
                    , requestLogger = logger
                    , requestInitialSQL = initialSQL
                    , requestCheckpoint = ""
                    , requestAction = reductionAction
                    , requestCollectCurve = isJust valStatsFile
                    }

    finalizeReductionSession
        logger
        ui
        opts
        prepared
        reductionConfig
        initialSQL
        initialBytes
        initialStmts
        writeToStdout
        targetMinimalPath
        statsPath
        result

finalizeReductionSession ::
    Logger ->
    UI ->
    ValidatedOptions ->
    PreparedOracle ->
    ReductionConfig ->
    BS.ByteString ->
    Int ->
    Int ->
    Bool ->
    FilePath ->
    FilePath ->
    EvaluationResult ->
    IO ()
finalizeReductionSession logger ui ValidatedOptions {..} prepared reductionConfig initialSQL initialBytes initialStmts writeToStdout targetMinimalPath statsPath result = do
    let reducedSQL = evaluatedSQL result
        evaluation = evaluationMetrics result
    reducedStmts <-
        case evaluatedStatements result of
            Just count -> pure count
            Nothing
                | reducedSQL == initialSQL -> pure initialStmts
                | otherwise -> countStatementsForCLI logger valDialect reducedSQL

    let stats =
            computeSessionStats
                initialBytes
                initialStmts
                reducedSQL
                reducedStmts
                (poInitialStart prepared)
                (poInitialEnd prepared)
                valCache
                reductionConfig
                valStrategy
                (poBudgetConfig prepared)
                evaluation

    writeReductionOutputs
        logger
        valInputFile
        valInPlace
        writeToStdout
        targetMinimalPath
        statsPath
        initialSQL
        reducedSQL
        stats
        evaluation

    let reducedBytes = BS.length reducedSQL
    logInfo'
        logger
        "cli"
        "Reduction session finished"
        [ ("status", toJSON (evaluationStatus evaluation))
        , ("initial_statements", toJSON initialStmts)
        , ("reduced_statements", toJSON reducedStmts)
        , ("initial_bytes", toJSON initialBytes)
        , ("reduced_bytes", toJSON reducedBytes)
        , ("database_observations", toJSON (oracle_executions stats))
        , ("cache_hits", toJSON (cache_hits stats))
        , ("cache_misses", toJSON (cache_misses stats))
        , ("elapsed_seconds", toJSON (evaluationTotalSeconds evaluation))
        ]

    uiFinish
        ui
        FinishSummary
            { finishMetrics = evaluation
            , finishInitialBytes = initialBytes
            , finishReducedBytes = reducedBytes
            , finishInitialStmts = initialStmts
            , finishReducedStmts = reducedStmts
            , finishMinimalPath = targetMinimalPath
            , finishStatsPath = statsPath
            }
    exitEvaluationStatus evaluation

prepareSessionOracle ::
    ValidatedOptions ->
    Maybe String ->
    Int ->
    ReductionConfig ->
    EvaluationConfig ->
    BS.ByteString ->
    IO PreparedOracle
prepareSessionOracle ValidatedOptions {..} oracleCmd timeoutMicros reductionConfig configuredBudget initialSQL = do
    let needTraces = valStrategy == StrategyFull && (valSlicing || reductionEarlyReject reductionConfig)
    (rememberTrace, lookupTrace) <- newResultTraceTracker needTraces
    failPrefixCache <-
        if isNothing oracleCmd && valStrategy == StrategyFull && reductionEarlyReject reductionConfig
            then Just <$> newFailingPrefixCache
            else pure Nothing
    let totalBudgetMicros = round . (* 1000000) <$> valBudgetSecs :: Maybe Integer
    (initialStart, initialEnd, preflightDetails, observer, validate, preflightMicros) <-
        runPreflight totalBudgetMicros $ case oracleCmd of
            Nothing ->
                prepareResultOracleWithValidation
                    needTraces
                    failPrefixCache
                    rememberTrace
                    valDialect
                    timeoutMicros
                    initialSQL
            Just command -> do
                let config = shellOracleConfig command timeoutMicros valResetCmd
                context <- resolveOracleContext config
                prepared <- prepareOracleWithContext config context initialSQL
                pure $ case prepared of
                    Left err -> Left err
                    Right details ->
                        let observe started = observeExternalOracleDetailedWithContext started config context
                         in Right (details, observe, observe)
    let budgetConfig =
            configuredBudget
                { evaluationPreflightMicros = preflightMicros
                , evaluationPreflightAttempts = 1
                , evaluationPreflightResetMicros = Just (resetElapsedMicros preflightDetails)
                , evaluationPreflightOracleMicros = oracleElapsedMicros preflightDetails
                }
    pure
        PreparedOracle
            { poInitialStart = initialStart
            , poInitialEnd = initialEnd
            , poPreflightDetails = preflightDetails
            , poObserver = observer
            , poValidate = validate
            , poBudgetConfig = budgetConfig
            , poFailPrefixCache = failPrefixCache
            , poLookupTrace = lookupTrace
            }

resolveMetricsPolicy ::
    Logger ->
    Maybe FilePath ->
    ReductionStrategy ->
    ReductionConfig ->
    Int ->
    IO (Maybe MetricPolicy)
resolveMetricsPolicy _ Nothing _ _ _ = pure Nothing
resolveMetricsPolicy logger (Just _) StrategyFull reductionConfig jobs =
    case metricPolicyForReduction reductionConfig jobs of
        Left err -> logError logger "cli" err >> exitCliError
        Right pol -> pure (Just pol)
resolveMetricsPolicy _ (Just _) strategy _ jobs =
    pure (Just (MetricPolicy (strategyName strategy) (columnModeName ColumnOff) jobs))

buildReductionAction ::
    ReductionStrategy ->
    Maybe (BS.ByteString, (SessionInit, [StatementUnit])) ->
    Maybe FailingPrefixCache ->
    (BS.ByteString -> IO (Maybe ResultTrace)) ->
    ReductionConfig ->
    BS.ByteString ->
    StageReporter ->
    Logger ->
    OraclePool ->
    IO ReductionActionResult
buildReductionAction valStrategy initialParsed failPrefixCache lookupTrace reductionConfig initialSQL reporter reducerLogger pool =
    case valStrategy of
        StrategyFull -> do
            ReductionResult sql evidence <-
                reduceSQLWithResultTraceFull
                    initialParsed
                    failPrefixCache
                    lookupTrace
                    reporter
                    reductionConfig
                    reducerLogger
                    pool
                    initialSQL
            pure (ReductionActionResult sql evidence)
        StrategyDdminOnly -> do
            parse <-
                reuseLatestSQLWithSeed
                    initialParsed
                    (parseStatementsFast (poolMetricsStore pool) (reductionDialect reductionConfig))
            sql <-
                statementDdminWithParser
                    (reductionStatementSearch reductionConfig)
                    parse
                    pool
                    initialSQL
            pure (ReductionActionResult sql Nothing)
        StrategyAuditOnly -> do
            parse <-
                reuseLatestSQLWithSeed
                    initialParsed
                    (parseStatementsFast (poolMetricsStore pool) (reductionDialect reductionConfig))
            auditStart <- toInteger <$> getMonotonicTimeNSec
            reporter (StageStarted StageStatementAudit)
            (sql, stats) <- statementAuditWithParser reducerLogger parse pool initialSQL
            auditEnd <- toInteger <$> getMonotonicTimeNSec
            let elapsed = fromIntegral (auditEnd - auditStart) / 1000000000 :: Double
            reporter (StageFinished StageStatementAudit elapsed (BS.length initialSQL) (BS.length sql))
            pure (ReductionActionResult sql (Just (ReductionEvidence sql (auditMinimalityConfirmed stats))))

exitEvaluationStatus :: EvaluationMetrics -> IO a
exitEvaluationStatus evaluation = case evaluationStatus evaluation of
    EvaluationCompleted
        | evaluationValidationPassed evaluation -> exitSuccess
        | otherwise -> exitFailure
    EvaluationTimeout -> exitTimeout
    EvaluationExecutionBudget -> exitTimeout
    _ -> exitFailure

requireOracleCommand :: Maybe String -> IO String
requireOracleCommand (Just command) = pure command
requireOracleCommand Nothing = do
    hPutStrLn stderr "error: specify --oracle COMMAND or --result-oracle"
    hPutStrLn stderr " note: specify predicate with -c or --oracle (e.g. -c './check.sh')"
    exitCliError

loadInitialInput ::
    Logger ->
    SQLDialect ->
    Bool ->
    Bool ->
    FilePath ->
    IO (BS.ByteString, Int, Int, Maybe (BS.ByteString, (SessionInit, [StatementUnit])))
loadInitialInput logger dialect reuseInitialParse spanOnly inputFile = do
    sql <- readInputBytes (Just logger) inputFile
    if not reuseInitialParse
        then do
            statements <- countStatementsForCLI logger dialect sql
            pure (sql, statements, BS.length sql, Nothing)
        else do
            let options = if spanOnly then spanOnlyOptions dialect else defaultParseOptions dialect
            parsed <- parseSQL options sql
            case parsed of
                Left err -> do
                    let message = "SQL parser failed while counting statements: " <> T.pack (show err)
                    logError logger "parser" message
                    hPutStrLn stderr ("error: " ++ T.unpack message)
                    exitFailure
                Right p ->
                    pure
                        ( sql
                        , length (parsedUnits p)
                        , BS.length sql
                        , Just (sql, (parsedInitialSession p, parsedUnits p))
                        )

buildReductionConfigFromOptions :: ValidatedOptions -> ReductionConfig
buildReductionConfigFromOptions ValidatedOptions {..} =
    buildReductionConfig
        valSlicing
        valClosure
        valStatementOrder
        valIntraStatement
        valLiteralBatches
        valAstBatching
        valAdaptiveValues
        valColumnMode
        valUnrestricted
        valDialect

buildReductionConfig ::
    Bool ->
    Bool ->
    StatementSearchPolicy ->
    Bool ->
    Bool ->
    Bool ->
    Bool ->
    ColumnMode ->
    Bool ->
    SQLDialect ->
    ReductionConfig
buildReductionConfig slicing closure statementOrder intra literalBatches astBatching adaptiveValues columnMode unrestricted dialect =
    (defaultReductionConfigFor dialect)
        { reductionSlicing = slicing
        , reductionClosure = closure
        , reductionStatementSearch = statementOrder
        , reductionIntraStatement = intra
        , reductionLiteralBatches = literalBatches
        , reductionAstBatching = astBatching
        , reductionAdaptiveValues = adaptiveValues
        , reductionColumnMode = columnMode
        , reductionUnrestricted = unrestricted
        }

buildBudgetConfig ::
    Bool -> Int -> Maybe Double -> Maybe Int -> Int -> EvaluationConfig
buildBudgetConfig cacheEnabled jobs budgetSecs maxExecutions initialStmts =
    EvaluationConfig
        { evaluationCacheEnabled = cacheEnabled
        , evaluationJobs = jobs
        , evaluationTotalTimeoutMicros = round . (* 1000000) <$> budgetSecs
        , evaluationMaxOracleExecutions = maxExecutions
        , evaluationPreflightMicros = 0
        , evaluationPreflightAttempts = 0
        , evaluationPreflightResetMicros = Nothing
        , evaluationPreflightOracleMicros = Nothing
        , evaluationInitialStatements = Just initialStmts
        }

runPreflight ::
    Maybe Integer ->
    IO (Either T.Text (OracleObservationDetails, EvaluationObserver, EvaluationObserver)) ->
    IO (UTCTime, UTCTime, OracleObservationDetails, EvaluationObserver, EvaluationObserver, Int)
runPreflight totalBudgetMicros prepare = do
    initialStart <- getCurrentTime
    preflightStart <- toInteger <$> getMonotonicTimeNSec
    prepared <- runWithinBudget preflightStart totalBudgetMicros prepare
    (details, observer, validate) <- case prepared of
        Nothing -> do
            hPutStrLn stderr "error: initial predicate check exceeded the total budget"
            exitTimeout
        Just (Left err) -> do
            hPutStrLn stderr ("error: initial predicate check failed: " ++ T.unpack err)
            exitFailure
        Just (Right checked) -> pure checked
    preflightEnd <- toInteger <$> getMonotonicTimeNSec
    initialEnd <- getCurrentTime
    let micros =
            fromInteger
                (min (toInteger (maxBound :: Int)) (max 0 ((preflightEnd - preflightStart) `div` 1000)))
    pure (initialStart, initialEnd, details, observer, validate, micros)

runWithinBudget :: Integer -> Maybe Integer -> IO a -> IO (Maybe a)
runWithinBudget _ Nothing act = Just <$> act
runWithinBudget started (Just totalMicros) act = do
    now <- toInteger <$> getMonotonicTimeNSec
    let remaining = max 0 (totalMicros - (now - started) `div` 1000)
        timeoutMicros = fromInteger (min remaining (toInteger (maxBound :: Int)))
    timeout timeoutMicros act

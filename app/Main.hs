module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, Value, encode, object, toJSON, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Occam qualified
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    metricPolicyForReduction,
    parseSQLDialect,
    runEvaluatedReductionWithProgress,
    sqlDialectName,
 )
import Occam.Logger (
    LogConfig (..),
    LogFormat (..),
    LogLevel (..),
    Logger,
    logError,
    logInfo,
    logInfo',
    withLogger,
 )
import Occam.Oracle (
    OracleConfig (..),
    OracleObservationDetails (..),
    prepareOracle,
    withCandidateWorkdir,
 )
import Occam.Parser (SQLDialect (..), countStatements)
import Occam.Reducer (
    ColumnMode (..),
    ReductionConfig (..),
    columnModeName,
    defaultReductionConfig,
    reduceSQLWithConfig,
 )
import Occam.UI (
    UI (..),
    UIConfig (..),
    newUI,
 )
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
    bashCompleter,
    completer,
    eitherReader,
    execParser,
    flag',
    fullDesc,
    header,
    help,
    helper,
    info,
    infoOption,
    long,
    metavar,
    option,
    optional,
    progDesc,
    short,
    showDefault,
    strArgument,
    strOption,
    switch,
    value,
    (<**>),
 )
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
 )
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdout)
import System.Timeout (timeout)

data Options = Options
    { optInputFile :: !FilePath
    , optOracleCmd :: !String
    , optOutDir :: !FilePath
    , optResetCmd :: !(Maybe String)
    , optTimeoutSecs :: !Double
    , optLogLevel :: !LogLevel
    , optLogFormat :: !LogFormat
    , optVerbose :: !Bool
    , optQuiet :: !Bool
    , optBudgetSecs :: !(Maybe Double)
    , optMaxOracleExecutions :: !(Maybe Int)
    , optJobs :: !Int
    , optReductionOptions :: !ReductionOptions
    }
    deriving (Eq, Show)

data ReductionOptions = ReductionOptions
    { optSlicing :: !Bool
    , optClosure :: !Bool
    , optIntraStatement :: !Bool
    , optColumnMode :: !ColumnMode
    , optUnrestricted :: !Bool
    , optCache :: !Bool
    , optDialect :: !SQLDialect
    }
    deriving (Eq, Show)

data Stats = Stats
    { initial_bytes :: !Int
    , reduced_bytes :: !Int
    , reduction_ratio :: !Double
    , initial_statements :: !Int
    , reduced_statements :: !Int
    , initial_tokens :: !(Maybe Int)
    , reduced_tokens :: !(Maybe Int)
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
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

optionsParser :: Parser Options
optionsParser =
    Options
        <$> strArgument
            ( metavar "INPUT.sql"
                <> help "Path to fuzz SQL input, or '-' for stdin"
                <> completer (bashCompleter "file")
            )
        <*> strOption
            ( long "oracle"
                <> metavar "COMMAND"
                <> help "Interestingness predicate command (SQL on stdin; exit 0 accepts)"
            )
        <*> strOption
            ( long "out"
                <> short 'o'
                <> metavar "DIR"
                <> value "occam-out"
                <> showDefault
                <> help "Directory where reduced artifacts will be written, or '-' for stdout"
                <> completer (bashCompleter "directory")
            )
        <*> optional
            ( strOption
                ( long "reset"
                    <> metavar "COMMAND"
                    <> help "Lightweight state reset command (e.g. dropdb/createdb)"
                )
            )
        <*> option
            auto
            ( long "timeout"
                <> metavar "SECONDS"
                <> value 10.0
                <> showDefault
                <> help "Per-execution timeout in seconds"
            )
        <*> option
            (eitherReader parseLogLevel)
            ( long "log-level"
                <> metavar "LEVEL"
                <> value LevelInfo
                <> help "Log severity level (debug, info, warn, error; default: info)"
            )
        <*> option
            (eitherReader parseLogFormat)
            ( long "log-format"
                <> metavar "FORMAT"
                <> value FormatText
                <> help "Log output format (text or json; default: text)"
            )
        <*> switch
            ( long "verbose"
                <> help "Enable verbose debug output (equivalent to --log-level debug)"
            )
        <*> switch
            ( long "quiet"
                <> short 'q'
                <> help "Suppress info logs and banners (equivalent to --log-level error)"
            )
        <*> optional
            ( option
                auto
                ( long "budget"
                    <> metavar "SECONDS"
                    <> help
                        "Shared wall-clock cap including the initial predicate check, reduction, audit, and repeated final validation"
                )
            )
        <*> optional
            ( option
                auto
                ( long "max-oracle-executions"
                    <> metavar "N"
                    <> help "Maximum uncached oracle attempts including the initial check and final validation"
                )
            )
        <*> option
            (eitherReader parseJobs)
            ( long "jobs"
                <> metavar "N"
                <> value 1
                <> showDefault
                <> help
                    "Maximum concurrent oracle/reset executions (default 1). Declares the predicate AND reset support N concurrent independent calls with independent database state; Occam enforces at most N in flight but provides no database isolation"
            )
        <*> reductionOptionsParser

reductionOptionsParser :: Parser ReductionOptions
reductionOptionsParser =
    ReductionOptions
        <$> toggle True "slicing" "no-slicing" "dependency slicing"
        <*> toggle True "closure" "no-closure" "dependency closure"
        <*> toggle False "intra-statement" "no-intra-statement" "intra-statement reductions"
        <*> option
            (eitherReader parseColumnMode)
            ( long "column-mode"
                <> metavar "MODE"
                <> value ColumnOff
                <> help
                    "Column co-reduction listing: off, single, or atomic (default: off; non-off requires --intra-statement)"
            )
        <*> toggle True "unrestricted" "no-unrestricted" "unrestricted reductions"
        <*> toggle True "cache" "no-cache" "candidate caching"
        <*> option
            (eitherReader parseSQLDialect)
            ( long "dialect"
                <> metavar "DIALECT"
                <> value GenericSQL
                <> showDefault
                <> help
                    "SQL dependency dialect: generic, postgresql, mysql, sqlite, duckdb, clickhouse, oracle, ansi, bigquery, databricks, hive, mssql, redshift, snowflake, spark, or teradata"
            )

toggle :: Bool -> String -> String -> String -> Parser Bool
toggle defaultValue enableName disableName description =
    flag' True (long enableName <> help ("Enable " ++ description))
        <|> flag' False (long disableName <> help ("Disable " ++ description))
        <|> pure defaultValue

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case map toLower s of
    "debug" -> Right LevelDebug
    "info" -> Right LevelInfo
    "warn" -> Right LevelWarn
    "warning" -> Right LevelWarn
    "error" -> Right LevelError
    _ -> Left "Expected debug, info, warn, or error"

parseLogFormat :: String -> Either String LogFormat
parseLogFormat s = case map toLower s of
    "text" -> Right FormatText
    "json" -> Right FormatJSON
    _ -> Left "Expected text or json"

{- | Positive job count parsed as 'Integer' first so huge inputs range-fail
instead of overflowing 'Int'.
-}
parseJobs :: String -> Either String Int
parseJobs s = case reads s of
    [(n, "")]
        | n >= (1 :: Integer)
        , n <= toInteger (maxBound :: Int) ->
            Right (fromInteger n)
    _ -> Left "Expected a positive --jobs value"

parseColumnMode :: String -> Either String ColumnMode
parseColumnMode s = case map toLower s of
    "off" -> Right ColumnOff
    "single" -> Right ColumnSingle
    "atomic" -> Right ColumnAtomic
    _ -> Left "Expected off, single, or atomic"

versionOption :: Parser (a -> a)
versionOption =
    infoOption
        ("Occam v" ++ Occam.version)
        ( long "version"
            <> short 'v'
            <> help "Show version information"
        )

optsInfo :: ParserInfo Options
optsInfo =
    info
        (optionsParser <**> versionOption <**> helper)
        ( fullDesc
            <> progDesc "Occam: Dependency-Aware SQL Test Case Minimizer"
            <> header ("Occam v" ++ Occam.version ++ " - Multi-statement SQL reducer")
        )

main :: IO ()
main = execParser optsInfo >>= runOccam

runOccam :: Options -> IO ()
runOccam opts@Options {..} = do
    let effectiveLevel
            | optQuiet = LevelError
            | optVerbose = LevelDebug
            | optLogFormat == FormatJSON = optLogLevel
            | otherwise = LevelWarn
        effectiveFormat = optLogFormat
        logCfg =
            LogConfig
                { logMinLevel = effectiveLevel
                , logFormat = effectiveFormat
                , logUseColor = True
                }
        runWithLogger logger = runOccamWithLogger logger opts
    withLogger logCfg runWithLogger

{- | Reduction checkpoint location. Stdout runs get a per-run temporary
directory (cleaned on exit); directory runs checkpoint the output file.
-}
withCheckpointPath :: Bool -> FilePath -> (FilePath -> IO a) -> IO a
withCheckpointPath writeToStdout minimalPath action
    | not writeToStdout = action minimalPath
    | otherwise = withCandidateWorkdir (\tmpDir -> action (tmpDir </> "occam-checkpoint.sql"))

runOccamWithLogger :: Logger -> Options -> IO ()
runOccamWithLogger logger Options {..} = do
    let dialect = optDialect optReductionOptions
    let finitePositive seconds = seconds > 0 && not (isNaN seconds || isInfinite seconds)
        maxSeconds = fromIntegral (maxBound :: Int) / 1000000
        finiteTimeout seconds =
            finitePositive seconds && seconds <= maxSeconds && seconds * 1000000 >= 1
        validBudget =
            finiteTimeout optTimeoutSecs
                && maybe True finiteTimeout optBudgetSecs
                && maybe True (> 0) optMaxOracleExecutions
                && optJobs > 0
    unless validBudget do
        hPutStrLn stderr "Error: timeout, budget, execution, and job limits must be positive"
        exitFailure

    let columnMode = optColumnMode optReductionOptions
    when (columnMode /= ColumnOff && not (optIntraStatement optReductionOptions)) do
        hPutStrLn stderr "Error: --column-mode single|atomic requires --intra-statement"
        exitFailure

    initialSQL <-
        if optInputFile == "-"
            then BS.getContents
            else do
                fileExists <- doesFileExist optInputFile
                unless fileExists do
                    let msg = "cannot open '" ++ optInputFile ++ "': No such file or directory"
                    logError logger "occam::cli" (T.pack msg)
                    hPutStrLn stderr ("occam: error: " ++ msg)
                    exitFailure
                BS.readFile optInputFile

    initialStmts <- countStatementsForCLI logger dialect initialSQL
    let initialBytes = BS.length initialSQL

    ui <-
        newUI
            UIConfig
                { uiTargetFile = optInputFile
                , uiInitialBytes = initialBytes
                , uiInitialStmts = initialStmts
                , uiDialect = sqlDialectName dialect
                , uiQuiet = optQuiet
                , uiVerbose = optVerbose || optLogLevel == LevelDebug
                , uiUseColor = True
                }

    let baseOracleConfig =
            OracleConfig
                { oracleCommand = "sh"
                , oracleArgs = ["-c", optOracleCmd]
                , oracleTimeoutMicros = round (optTimeoutSecs * 1000000)
                , oracleResetCommand = optResetCmd
                }
        reductionConfig = toReductionConfig optReductionOptions
        configuredBudget =
            EvaluationConfig
                { evaluationCacheEnabled = optCache optReductionOptions
                , evaluationJobs = optJobs
                , evaluationTotalTimeoutMicros = round . (* 1000000) <$> optBudgetSecs
                , evaluationMaxOracleExecutions = optMaxOracleExecutions
                , evaluationPreflightMicros = 0
                , evaluationPreflightAttempts = 0
                , evaluationPreflightResetMicros = Nothing
                , evaluationPreflightOracleMicros = Nothing
                }
    logInfo'
        logger
        "occam::cli"
        "Starting reduction session"
        [ ("input_file", toJSON optInputFile)
        , ("initial_bytes", toJSON initialBytes)
        , ("initial_statements", toJSON initialStmts)
        , ("oracle_cmd", toJSON optOracleCmd)
        , ("evaluation_budget", toJSON configuredBudget)
        , ("reduction_settings", reductionSettings (optCache optReductionOptions) reductionConfig)
        ]

    initialStart <- getCurrentTime
    preflightStart <- toInteger <$> getMonotonicTimeNSec
    let totalBudgetMicros = round . (* 1000000) <$> optBudgetSecs :: Maybe Integer
    prepared <-
        runWithinBudget
            preflightStart
            totalBudgetMicros
            (prepareOracle baseOracleConfig initialSQL)
    preflightDetails <- case prepared of
        Nothing -> do
            hPutStrLn stderr "occam: error: initial predicate check exceeded the total budget"
            exitFailure
        Just (Left err) -> do
            hPutStrLn stderr ("occam: error: initial predicate check failed: " ++ T.unpack err)
            hPutStrLn stderr "occam: note: oracle command rejected the original unreduced SQL input"
            exitFailure
        Just (Right details) -> pure details
    preflightEnd <- toInteger <$> getMonotonicTimeNSec
    initialEnd <- getCurrentTime
    let oracleCfg = baseOracleConfig
        preflightMicros =
            fromInteger
                (min (toInteger (maxBound :: Int)) (max 0 ((preflightEnd - preflightStart) `div` 1000)))
        budgetConfig =
            configuredBudget
                { evaluationPreflightMicros = preflightMicros
                , evaluationPreflightAttempts = 1
                , evaluationPreflightResetMicros = Just (resetElapsedMicros preflightDetails)
                , evaluationPreflightOracleMicros = oracleElapsedMicros preflightDetails
                }
    logInfo logger "occam::oracle" "Initial predicate check passed"

    let writeToStdout = optOutDir == "-"
        minimalPath = if writeToStdout then "" else optOutDir </> "minimal.sql"
        statsPath = if writeToStdout then "" else optOutDir </> "stats.json"

    unless writeToStdout (createDirectoryIfMissing True optOutDir)
    logInfo logger "occam::reducer" "Running hierarchical reduction engine..."

    metricsPolicy <- case metricPolicyForReduction reductionConfig optJobs of
        Left err -> do
            hPutStrLn stderr ("occam: error: " ++ T.unpack err)
            exitFailure
        Right policy -> pure (Just policy)
    result <-
        withCheckpointPath writeToStdout minimalPath $ \checkpointPath ->
            runEvaluatedReductionWithProgress
                (uiOnProgress ui)
                metricsPolicy
                budgetConfig
                dialect
                oracleCfg
                (Just (detailedObservation preflightDetails))
                logger
                initialSQL
                checkpointPath
                (\reducerLogger oracle -> reduceSQLWithConfig reductionConfig reducerLogger oracle initialSQL)
    let reducedSQL = evaluatedSQL result
        evaluation = evaluationMetrics result
        totalHits = evaluationCacheHits evaluation
        totalMisses = evaluationCacheMisses evaluation
        totalLookups = totalHits + totalMisses
        hitRatio =
            if totalLookups > 0
                then fromIntegral totalHits / fromIntegral totalLookups
                else 0.0
    reducedStmts <- countStatementsForCLI logger dialect reducedSQL
    let reducedBytes = BS.length reducedSQL
        ratio =
            if initialBytes > 0
                then (1.0 - (fromIntegral reducedBytes / fromIntegral initialBytes :: Double)) * 100.0
                else 0.0
        initialOracleSeconds = realToFrac (diffUTCTime initialEnd initialStart)
        totalOracleExecutions =
            evaluationPreflightAttempts budgetConfig
                + evaluationDatabaseObservations evaluation
                + evaluationValidationDatabaseObservations evaluation
        totalOracleAttempts =
            evaluationPreflightAttempts budgetConfig
                + evaluationOracleAttempts evaluation
                + evaluationValidationObservations evaluation
        stats =
            Stats
                { initial_bytes = initialBytes
                , reduced_bytes = reducedBytes
                , reduction_ratio = ratio
                , initial_statements = initialStmts
                , reduced_statements = reducedStmts
                , initial_tokens = Nothing
                , reduced_tokens = Nothing
                , oracle_executions = totalOracleExecutions
                , oracle_attempts = totalOracleAttempts
                , initial_oracle_observations = evaluationPreflightAttempts budgetConfig
                , cache_hits = totalHits
                , cache_misses = totalMisses
                , cache_joins = evaluationCacheJoins evaluation
                , cache_hit_ratio = hitRatio
                , cancelled_observations = evaluationCancelledObservations evaluation
                , status = evaluationStatus evaluation
                , reduction_settings = reductionSettings (optCache optReductionOptions) reductionConfig
                , evaluation_budget = budgetConfig
                , evaluation_metrics = evaluation
                , initial_oracle_seconds = initialOracleSeconds
                , elapsed_seconds = evaluationTotalSeconds evaluation
                }
    unless writeToStdout do
        BS.writeFile minimalPath reducedSQL
        BSL.writeFile statsPath (encode stats)

    isStdoutTTY <- hIsTerminalDevice stdout
    when (writeToStdout || not isStdoutTTY) do
        BS.putStr reducedSQL
        hFlush stdout

    logInfo'
        logger
        "occam::cli"
        "Reduction session finished"
        [ ("status", toJSON (evaluationStatus evaluation))
        , ("initial_statements", toJSON initialStmts)
        , ("reduced_statements", toJSON reducedStmts)
        , ("initial_bytes", toJSON initialBytes)
        , ("reduced_bytes", toJSON reducedBytes)
        , ("database_observations", toJSON totalOracleExecutions)
        , ("cache_hits", toJSON totalHits)
        , ("cache_misses", toJSON totalMisses)
        , ("elapsed_seconds", toJSON (evaluationTotalSeconds evaluation))
        ]

    uiFinish ui evaluation initialBytes reducedBytes initialStmts reducedStmts minimalPath statsPath
    unless
        (evaluationStatus evaluation == EvaluationCompleted && evaluationValidationPassed evaluation)
        exitFailure

runWithinBudget :: Integer -> Maybe Integer -> IO a -> IO (Maybe a)
runWithinBudget _ Nothing action = Just <$> action
runWithinBudget started (Just totalMicros) action = do
    now <- toInteger <$> getMonotonicTimeNSec
    let remaining = max 0 (totalMicros - (now - started) `div` 1000)
        timeoutMicros = fromInteger (min remaining (toInteger (maxBound :: Int)))
    timeout timeoutMicros action

countStatementsForCLI :: Logger -> SQLDialect -> BS.ByteString -> IO Int
countStatementsForCLI logger dialect sql = do
    result <- countStatements dialect sql
    case result of
        Left err -> do
            let message = "SQL parser failed while counting statements: " <> T.pack (show err)
            logError logger "occam::parser" message
            hPutStrLn stderr ("occam: error: " ++ T.unpack message)
            exitFailure
        Right count -> pure count

toReductionConfig :: ReductionOptions -> ReductionConfig
toReductionConfig ReductionOptions {..} =
    defaultReductionConfig
        { reductionSlicing = optSlicing
        , reductionClosure = optClosure
        , reductionIntraStatement = optIntraStatement
        , reductionColumnMode = optColumnMode
        , reductionUnrestricted = optUnrestricted
        , reductionDialect = optDialect
        }

reductionSettings :: Bool -> ReductionConfig -> Value
reductionSettings cacheEnabled ReductionConfig {..} =
    object
        [ "slicing" .= reductionSlicing
        , "closure" .= reductionClosure
        , "intra_statement" .= reductionIntraStatement
        , "column_mode" .= columnModeName reductionColumnMode
        , "unrestricted" .= reductionUnrestricted
        , "cache" .= cacheEnabled
        , "dialect" .= sqlDialectName reductionDialect
        ]

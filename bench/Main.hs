module Main (main) where

import Benchmark (
    BenchmarkMetric (..),
    Profile,
    parseProfile,
    profileCacheEnabled,
    profileName,
    runProfileBenchmark,
 )
import Control.Monad (unless, when)
import Data.Aeson (encode, toJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Occam qualified
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationStatus (..),
    parseSQLDialect,
 )
import Occam.Logger (
    LogConfig (..),
    LogFormat (..),
    LogLevel (..),
    Logger,
    logError,
    logInfo',
    withLogger,
 )
import Occam.Oracle (
    OracleConfig (..),
    OracleObservationDetails (..),
    prepareOracle,
 )
import Occam.Parser (SQLDialect (..))
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
    eitherReader,
    execParser,
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
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

data Options = Options
    { optInputFile :: !FilePath
    , optOracleCmd :: !String
    , optOutDir :: !FilePath
    , optResetCmd :: !(Maybe String)
    , optTimeoutSecs :: !Double
    , optProfile :: !Profile
    , optLogLevel :: !String
    , optLogFormat :: !String
    , optVerbose :: !Bool
    , optQuiet :: !Bool
    , optBudgetSecs :: !(Maybe Double)
    , optMaxOracleExecutions :: !(Maybe Int)
    , optDialect :: !SQLDialect
    }
    deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser =
    Options
        <$> strArgument
            ( metavar "INPUT.sql"
                <> help "Path to fuzz SQL input, including schema and setup statements"
            )
        <*> strOption
            ( long "oracle"
                <> metavar "COMMAND"
                <> help "Interestingness predicate command (SQL on stdin; exit 0 accepts)"
            )
        <*> strOption
            ( long "out"
                <> metavar "DIR"
                <> value "occam-eval-out"
                <> showDefault
                <> help "Directory where benchmark artifacts will be written"
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
            (eitherReader parseProfile)
            ( long "profile"
                <> metavar "PROFILE"
                <> help "Run one internal profile (e.g. full, graph-off, statement-ddmin)"
            )
        <*> strOption
            ( long "log-level"
                <> metavar "LEVEL"
                <> value "info"
                <> showDefault
                <> help "Log severity level (debug, info, warn, error)"
            )
        <*> strOption
            ( long "log-format"
                <> metavar "FORMAT"
                <> value "text"
                <> showDefault
                <> help "Log output format (text or json)"
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
                    <> help "Shared wall-clock cap including initial verification, reduction, audit, and final validation"
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
            (eitherReader parseSQLDialect)
            ( long "dialect"
                <> metavar "DIALECT"
                <> value GenericSQL
                <> showDefault
                <> help
                    "SQL dependency dialect: generic, postgresql, mysql, sqlite, duckdb, clickhouse, oracle, ansi, bigquery, databricks, hive, mssql, redshift, snowflake, spark, or teradata"
            )

parseLogLevel :: String -> LogLevel
parseLogLevel s = case map toLower s of
    "debug" -> LevelDebug
    "warn" -> LevelWarn
    "warning" -> LevelWarn
    "error" -> LevelError
    _ -> LevelInfo

parseLogFormat :: String -> LogFormat
parseLogFormat s = case map toLower s of
    "json" -> FormatJSON
    _ -> FormatText
versionOption :: Parser (a -> a)
versionOption =
    infoOption
        ("Occam-Eval v" ++ Occam.version)
        ( long "version"
            <> short 'v'
            <> help "Show version information"
        )

optsInfo :: ParserInfo Options
optsInfo =
    info
        (optionsParser <**> versionOption <**> helper)
        ( fullDesc
            <> progDesc "Occam-Eval: Run one internal reducer control or ablation"
            <> header ("Occam-Eval v" ++ Occam.version ++ " - Isolated internal profile worker")
        )

main :: IO ()
main = execParser optsInfo >>= runEval

runEval :: Options -> IO ()
runEval opts@Options {..} = do
    let effectiveLevel
            | optQuiet = LevelError
            | optVerbose = LevelDebug
            | otherwise = parseLogLevel optLogLevel
        effectiveFormat = parseLogFormat optLogFormat
        logCfg =
            LogConfig
                { logMinLevel = effectiveLevel
                , logFormat = effectiveFormat
                , logUseColor = True
                }
        runWithLogger logger = runEvalWithLogger logger opts
    withLogger logCfg runWithLogger

runEvalWithLogger :: Logger -> Options -> IO ()
runEvalWithLogger logger Options {..} = do
    let finiteTimeout seconds =
            seconds > 0
                && not (isNaN seconds || isInfinite seconds)
                && seconds <= fromIntegral (maxBound :: Int) / 1000000
                && seconds * 1000000 >= 1
        validLimits =
            finiteTimeout optTimeoutSecs
                && maybe True finiteTimeout optBudgetSecs
                && maybe True (> 0) optMaxOracleExecutions
    unless validLimits $
        failWith "Error: timeout, budget, and execution limits must be positive"

    exists <- doesFileExist optInputFile
    unless exists do
        logError logger "occam::eval" ("Error: Input file does not exist: " <> T.pack optInputFile)
        failWith $ "Error: Input file does not exist: " ++ optInputFile
    initialSQL <- BS.readFile optInputFile
    when (not optQuiet && parseLogFormat optLogFormat == FormatText) do
        putStrLn "======================================================================"
        putStrLn ("Occam-Eval v" ++ Occam.version ++ " - Internal Profile Worker")
        putStrLn $ "  Input SQL:      " ++ optInputFile
        putStrLn $ "  Output Dir:     " ++ optOutDir
        putStrLn $ "  Oracle Command: " ++ optOracleCmd
        case optResetCmd of
            Just reset -> putStrLn $ "  Reset Command:  " ++ reset
            Nothing -> pure ()
        putStrLn $ "  Timeout:        " ++ show optTimeoutSecs ++ "s per oracle call"
        case optBudgetSecs of
            Just budget -> putStrLn $ "  Total Budget:   " ++ show budget ++ "s per profile"
            Nothing -> pure ()
        putStrLn "======================================================================"

    let baseOracleConfig =
            OracleConfig
                { oracleCommand = "sh"
                , oracleArgs = ["-c", optOracleCmd]
                , oracleTimeoutMicros = round (optTimeoutSecs * 1000000)
                , oracleResetCommand = optResetCmd
                }
        totalBudgetMicros = round . (* 1000000) <$> optBudgetSecs :: Maybe Integer
    preflightStart <- toInteger <$> getMonotonicTimeNSec
    prepared <-
        runWithinBudget
            preflightStart
            totalBudgetMicros
            (prepareOracle baseOracleConfig initialSQL)
    preflightDetails <- case prepared of
        Nothing -> failWith "Error: Initial predicate check exceeded the total budget"
        Just (Left err) -> failWith ("Error: " ++ T.unpack err)
        Just (Right details) -> pure details
    preflightEnd <- toInteger <$> getMonotonicTimeNSec
    let preflightMicros =
            fromInteger
                (min (toInteger (maxBound :: Int)) (max 0 ((preflightEnd - preflightStart) `div` 1000)))
        evaluationConfig =
            EvaluationConfig
                { evaluationCacheEnabled = profileCacheEnabled optProfile
                , evaluationTotalTimeoutMicros = round . (* 1000000) <$> optBudgetSecs
                , evaluationMaxOracleExecutions = optMaxOracleExecutions
                , evaluationPreflightMicros = preflightMicros
                , evaluationPreflightAttempts = 1
                , evaluationPreflightResetMicros = Just (resetElapsedMicros preflightDetails)
                , evaluationPreflightOracleMicros = oracleElapsedMicros preflightDetails
                }

    logInfo'
        logger
        "occam::eval"
        "Initial predicate check passed"
        [ ("input_file", toJSON optInputFile)
        , ("oracle_cmd", toJSON optOracleCmd)
        , ("timeout_secs", toJSON optTimeoutSecs)
        , ("evaluation_budget", toJSON evaluationConfig)
        ]
    createDirectoryIfMissing True optOutDir

    logInfo'
        logger
        "occam::eval"
        "Running one internal profile"
        [("profile", toJSON (profileName optProfile))]
    metric <-
        runProfileBenchmark
            optProfile
            optDialect
            evaluationConfig
            baseOracleConfig
            logger
            initialSQL
            optOutDir
    let jsonData = encode [metric]
    BSL.writeFile (optOutDir </> "benchmark.json") jsonData
    logInfo'
        logger
        "occam::eval"
        "Internal profile result written"
        [("out_file", toJSON (optOutDir </> "benchmark.json"))]

    when (not optQuiet && parseLogFormat optLogFormat == FormatText) do
        putStrLn "----------------------------------------------------------------------"
        putStrLn "Internal Profile Result"
        putStrLn "======================================================================"
        putStrLn "Internal Profile Metrics (JSON)"
        putStrLn "======================================================================"
    BSL.putStr jsonData
    putStrLn ""
    when (not optQuiet && parseLogFormat optLogFormat == FormatText) do
        putStrLn "======================================================================"
        putStrLn $ "  Artifact: " ++ (optOutDir </> "benchmark.json")
        putStrLn "======================================================================"
    unless (completedAndValidated metric) $
        failWith "Error: selected profile did not complete repeated target validation"

completedAndValidated :: BenchmarkMetric -> Bool
completedAndValidated metric =
    evaluationStatus (metricEvaluationMetrics metric) == EvaluationCompleted
        && evaluationValidationPassed (metricEvaluationMetrics metric)

runWithinBudget :: Integer -> Maybe Integer -> IO a -> IO (Maybe a)
runWithinBudget _ Nothing action = Just <$> action
runWithinBudget started (Just totalMicros) action = do
    now <- toInteger <$> getMonotonicTimeNSec
    let remaining = max 0 (totalMicros - (now - started) `div` 1000)
        timeoutMicros = fromInteger (min remaining (toInteger (maxBound :: Int)))
    timeout timeoutMicros action

failWith :: String -> IO a
failWith message = hPutStrLn stderr message >> exitFailure

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Benchmark (BenchmarkMetric (..), runComparisonBenchmark, runSingleAlgorithmBenchmark)
import Control.Applicative ((<|>))
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
    logInfo,
    logInfo',
    withLogger,
 )
import Occam.Oracle (
    OracleConfig (..),
    OracleObservationDetails (..),
    prepareOracle,
 )
import Occam.Parser (SQLDialect (..))
import Occam.Reducer (ReductionConfig (..), defaultReductionConfig)
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
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
    , optBaseline :: !(Maybe Int)
    , optLogLevel :: !String
    , optLogFormat :: !String
    , optVerbose :: !Bool
    , optQuiet :: !Bool
    , optBudgetSecs :: !(Maybe Double)
    , optMaxOracleExecutions :: !(Maybe Int)
    , optReductionOptions :: !ReductionOptions
    }
    deriving (Eq, Show)

data ReductionOptions = ReductionOptions
    { optSlicing :: !Bool
    , optClosure :: !Bool
    , optIntraStatement :: !Bool
    , optUnrestricted :: !Bool
    , optCache :: !Bool
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
        <*> optional
            ( option
                auto
                ( long "baseline"
                    <> metavar "N"
                    <> help "Run only specific baseline (1: Sequential, 2: Statement ddmin)"
                )
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
        <*> reductionOptionsParser

reductionOptionsParser :: Parser ReductionOptions
reductionOptionsParser =
    ReductionOptions
        <$> toggle True "slicing" "no-slicing" "dependency slicing"
        <*> toggle True "closure" "no-closure" "dependency closure"
        <*> toggle True "intra-statement" "no-intra-statement" "intra-statement reductions"
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
            <> progDesc "Occam-Eval: Comparative Evaluation Tool for Occam vs Baselines"
            <> header ("Occam-Eval v" ++ Occam.version ++ " - Benchmark runner for SQL test case reduction")
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
    withLogger logCfg $ \logger -> runEvalWithLogger logger opts

runEvalWithLogger :: Logger -> Options -> IO ()
runEvalWithLogger logger Options {..} = do
    when (maybe False (`notElem` [1, 2]) optBaseline) $
        failWith "Error: baseline must be 1 (Sequential) or 2 (Statement ddmin)"
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
    unless exists $ do
        logError logger "occam::eval" ("Error: Input file does not exist: " <> T.pack optInputFile)
        failWith $ "Error: Input file does not exist: " ++ optInputFile
    initialSQL <- BS.readFile optInputFile
    when (not optQuiet && parseLogFormat optLogFormat == FormatText) $ do
        putStrLn "======================================================================"
        putStrLn ("Occam-Eval v" ++ Occam.version ++ " - Comparative Evaluation Runner")
        putStrLn "======================================================================"
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
                { evaluationCacheEnabled = optCache optReductionOptions
                , evaluationTotalTimeoutMicros = round . (* 1000000) <$> optBudgetSecs
                , evaluationMaxOracleExecutions = optMaxOracleExecutions
                , evaluationPreflightMicros = preflightMicros
                , evaluationPreflightAttempts = 1
                , evaluationPreflightResetMicros = Just (resetElapsedMicros preflightDetails)
                , evaluationPreflightOracleMicros = oracleElapsedMicros preflightDetails
                }
        reductionConfig = toReductionConfig optReductionOptions

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

    metrics <- case optBaseline of
        Just baseline -> do
            logInfo'
                logger
                "occam::eval"
                "Running single algorithm benchmark"
                [("baseline_idx", toJSON baseline)]
            metric <-
                runSingleAlgorithmBenchmark
                    (optDialect optReductionOptions)
                    baseline
                    evaluationConfig
                    baseOracleConfig
                    logger
                    initialSQL
                    optOutDir
            reducedSQL <- BS.readFile (metricSQLArtifact metric)
            BS.writeFile (optOutDir </> "minimal.sql") reducedSQL
            pure [metric]
        Nothing -> do
            logInfo logger "occam::eval" "Running comparative reducer and ablation profiles"
            runComparisonBenchmark evaluationConfig baseOracleConfig logger reductionConfig initialSQL optOutDir
    let jsonData = encode metrics
    BSL.writeFile (optOutDir </> "benchmark.json") jsonData
    logInfo'
        logger
        "occam::eval"
        "Benchmark evaluation finished"
        [("out_file", toJSON (optOutDir </> "benchmark.json"))]

    when (not optQuiet && parseLogFormat optLogFormat == FormatText) $ do
        putStrLn "----------------------------------------------------------------------"
        putStrLn "Benchmark Complete!"
        putStrLn "======================================================================"
        putStrLn "Comparative Benchmark Results (JSON)"
        putStrLn "======================================================================"
    BSL.putStr jsonData
    putStrLn ""
    when (not optQuiet && parseLogFormat optLogFormat == FormatText) $ do
        putStrLn "======================================================================"
        putStrLn $ "  Artifact: " ++ (optOutDir </> "benchmark.json")
        putStrLn "======================================================================"
    unless (all completedAndValidated metrics) $
        failWith "Error: one or more benchmark profiles did not complete repeated target validation"

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

toReductionConfig :: ReductionOptions -> ReductionConfig
toReductionConfig ReductionOptions {..} =
    defaultReductionConfig
        { reductionSlicing = optSlicing
        , reductionClosure = optClosure
        , reductionIntraStatement = optIntraStatement
        , reductionUnrestricted = optUnrestricted
        , reductionDialect = optDialect
        }

failWith :: String -> IO a
failWith message = hPutStrLn stderr message >> exitFailure

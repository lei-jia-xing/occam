{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Benchmark (runComparisonBenchmark, runSingleAlgorithmBenchmark)
import Control.Monad (unless, when)
import Data.Aeson (encode, toJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import Data.Text qualified as T
import Occam qualified
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
import Occam.Oracle (OracleConfig (..), runExternalOracle)
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
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
    }
    deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser =
    Options
        <$> strArgument
            ( metavar "INPUT.sql"
                <> help "Path to input SQL crash reproduction script"
            )
        <*> strOption
            ( long "oracle"
                <> metavar "COMMAND"
                <> help "Target crash oracle command (e.g. script or binary)"
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
                    <> help "Run only specific baseline (1: Sequential, 2: Naive ddmin)"
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
            <> header ("Occam-Eval v" ++ Occam.version ++ " - Benchmark runner for SQL crash reduction")
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
    exists <- doesFileExist optInputFile
    unless exists $ do
        logError logger "occam::eval" ("Error: Input file does not exist: " <> T.pack optInputFile)
        hPutStrLn stderr $ "Error: Input file does not exist: " ++ optInputFile
        exitFailure

    initialSQL <- BS.readFile optInputFile

    when (not optQuiet && parseLogFormat optLogFormat == FormatText) $ do
        putStrLn "======================================================================"
        putStrLn ("Occam-Eval v" ++ Occam.version ++ " - Comparative Evaluation Runner")
        putStrLn "======================================================================"
        putStrLn $ "  Input SQL:      " ++ optInputFile
        putStrLn $ "  Output Dir:     " ++ optOutDir
        putStrLn $ "  Oracle Command: " ++ optOracleCmd
        case optResetCmd of
            Just r -> putStrLn $ "  Reset Command:  " ++ r
            Nothing -> pure ()
        putStrLn $ "  Timeout:        " ++ show optTimeoutSecs ++ "s"
        putStrLn "======================================================================"

    logInfo'
        logger
        "occam::eval"
        "Initialized evaluation runner"
        [ ("input_file", toJSON optInputFile)
        , ("oracle_cmd", toJSON optOracleCmd)
        , ("timeout_secs", toJSON optTimeoutSecs)
        ]

    let timeoutMicros = round (optTimeoutSecs * 1000000)
        baseOracleCfg =
            OracleConfig
                { oracleCommand = "sh"
                , oracleArgs = ["-c", optOracleCmd]
                , oracleTimeoutMicros = timeoutMicros
                , oracleResetCommand = optResetCmd
                , oracleExpectedCrash = Nothing
                }

    -- 1. Initial verification: make sure the input triggers the crash
    logInfo logger "occam::eval" "Verifying initial crash reproduction against oracle..."
    initialOk <- runExternalOracle baseOracleCfg initialSQL
    unless initialOk $ do
        logError logger "occam::eval" "Initial input does not reproduce target crash!"
        hPutStrLn stderr "Error: Initial input does not reproduce target crash!"
        exitFailure
    logInfo logger "occam::eval" "Initial crash verified successfully"

    -- 2. Run benchmark: single baseline or comparative Occam vs B1 vs B2
    createDirectoryIfMissing True optOutDir
    jsonData <- case optBaseline of
        Just b -> do
            logInfo' logger "occam::eval" "Running single algorithm benchmark" [("baseline_idx", toJSON b)]
            (metric, reducedSQL) <- runSingleAlgorithmBenchmark b (runExternalOracle baseOracleCfg) initialSQL
            BS.writeFile (optOutDir </> "minimal.sql") reducedSQL
            pure (encode [metric])
        Nothing -> do
            logInfo
                logger
                "occam::eval"
                "Running comparative benchmark: Occam vs B1 (Sequential) vs B2 (Naive ddmin)..."
            metrics <- runComparisonBenchmark (runExternalOracle baseOracleCfg) initialSQL
            pure (encode metrics)
    BSL.writeFile (optOutDir </> "benchmark.json") jsonData
    logInfo'
        logger
        "occam::eval"
        "Benchmark evaluation finished"
        [("out_file", toJSON (optOutDir </> "benchmark.json"))]

    -- 3. Print results JSON
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

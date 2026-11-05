{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Benchmark (formatCSV, formatMarkdownTable, runComparisonBenchmark)
import Control.Monad (unless)
import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text.IO qualified as TIO
import Occam qualified
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
runEval Options {..} = do
    exists <- doesFileExist optInputFile
    unless exists $ do
        hPutStrLn stderr $ "Error: Input file does not exist: " ++ optInputFile
        exitFailure

    initialSQL <- BS.readFile optInputFile

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
    putStr "Verifying initial crash reproduction... "
    initialOk <- runExternalOracle baseOracleCfg initialSQL
    unless initialOk $ do
        putStrLn "FAILED"
        hPutStrLn stderr "Error: Initial input does not reproduce target crash!"
        exitFailure
    putStrLn "OK (Reproduced)"

    -- 2. Run comparative benchmark: Occam vs B1 vs B2
    putStrLn "Running comparative benchmark: Occam vs B1 (Sequential) vs B2 (Naive ddmin)..."
    metrics <- runComparisonBenchmark (runExternalOracle baseOracleCfg) initialSQL
    createDirectoryIfMissing True optOutDir
    let mdTable = formatMarkdownTable metrics
        csvData = formatCSV metrics
        jsonData = encode metrics
    TIO.writeFile (optOutDir </> "benchmark.md") mdTable
    TIO.writeFile (optOutDir </> "benchmark.csv") csvData
    BSL.writeFile (optOutDir </> "benchmark.json") jsonData

    -- 3. Print results table
    putStrLn "----------------------------------------------------------------------"
    putStrLn "Benchmark Complete!"
    putStrLn "======================================================================"
    putStrLn "Comparative Benchmark Results"
    putStrLn "======================================================================"
    TIO.putStr mdTable
    putStrLn "======================================================================"
    putStrLn $ "  Artifacts: " ++ (optOutDir </> "benchmark.md")
    putStrLn $ "             " ++ (optOutDir </> "benchmark.csv")
    putStrLn $ "             " ++ (optOutDir </> "benchmark.json")
    putStrLn "======================================================================"

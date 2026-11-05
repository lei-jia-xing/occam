{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text.IO qualified as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Occam qualified
import Occam.Benchmark (formatCSV, formatMarkdownTable, runComparisonBenchmark)
import Occam.Cache (CacheStats (..), memoizeOracle)
import Occam.Oracle (
    OracleConfig (..),
    runExternalOracle,
 )
import Occam.Reducer (reduceSQL)
import Occam.TreeSitter (countStatements)
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
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getPermissions,
    setOwnerExecutable,
    setPermissions,
 )
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

data Options = Options
    { optInputFile :: !FilePath
    , optOracleCmd :: !String
    , optOutDir :: !FilePath
    , optResetCmd :: !(Maybe String)
    , optTimeoutSecs :: !Double
    , optFrames :: !Int
    , optBenchmark :: !Bool
    }
    deriving (Eq, Show)

data Stats = Stats
    { initial_bytes :: !Int
    , reduced_bytes :: !Int
    , reduction_ratio :: !Double
    , initial_statements :: !Int
    , reduced_statements :: !Int
    , oracle_executions :: !Int
    , cache_hits :: !Int
    , cache_misses :: !Int
    , cache_hit_ratio :: !Double
    , elapsed_seconds :: !Double
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

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
                <> value "occam-out"
                <> showDefault
                <> help "Directory where reduced artifacts will be written"
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
            auto
            ( long "frames"
                <> metavar "N"
                <> value 3
                <> showDefault
                <> help "Top-N stack frame functions required for crash equivalence"
            )
        <*> switch
            ( long "benchmark"
                <> help "Run comparative benchmark: Occam vs B1 (Sequential) vs B2 (Naive ddmin)"
            )

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
            <> progDesc "Occam: Dependency-Aware SQL Crash Minimizer"
            <> header ("Occam v" ++ Occam.version ++ " - Semantic multi-statement crash reducer")
        )

main :: IO ()
main = execParser optsInfo >>= runOccam

runOccam :: Options -> IO ()
runOccam Options {..} = do
    fileExists <- doesFileExist optInputFile
    unless fileExists $ do
        hPutStrLn stderr ("Error: Input file not found: " ++ optInputFile)
        exitFailure

    initialSQL <- BS.readFile optInputFile
    initialStmts <- countStatements initialSQL
    let initialBytes = BS.length initialSQL

    putStrLn "======================================================================"
    putStrLn $ "Occam v" ++ Occam.version ++ " starting reduction"
    putStrLn "======================================================================"
    putStrLn $
        "  Target SQL:   "
            ++ optInputFile
            ++ " ("
            ++ show initialBytes
            ++ " bytes, "
            ++ show initialStmts
            ++ " stmts)"
    putStrLn $ "  Oracle Cmd:   " ++ optOracleCmd
    putStrLn $ "  Artifact Dir: " ++ optOutDir
    putStrLn "----------------------------------------------------------------------"

    -- Setup oracle configuration
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

    if optBenchmark
        then do
            putStrLn "Running comparative benchmark: Occam vs B1 (Sequential) vs B2 (Naive ddmin)..."
            metrics <- runComparisonBenchmark (runExternalOracle baseOracleCfg) initialSQL
            createDirectoryIfMissing True optOutDir
            let mdTable = formatMarkdownTable metrics
                csvData = formatCSV metrics
                jsonData = encode metrics
            TIO.writeFile (optOutDir </> "benchmark.md") mdTable
            TIO.writeFile (optOutDir </> "benchmark.csv") csvData
            BSL.writeFile (optOutDir </> "benchmark.json") jsonData
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
        else do
            (oracle, getCacheStats) <- memoizeOracle (runExternalOracle baseOracleCfg)
            -- 2. Run hierarchical reduction engine
            putStrLn "Running hierarchical reduction engine (Levels 1-4)..."
            startTime <- getCurrentTime
            reducedSQL <- reduceSQL oracle initialSQL
            endTime <- getCurrentTime
            -- 3. Gather metrics
            cacheStats <- getCacheStats
            let totalHits = cacheHits cacheStats
                totalMisses = cacheMisses cacheStats
                totalCalls = totalMisses
                totalLookups = totalHits + totalMisses
                hitRatio =
                    if totalLookups > 0
                        then fromIntegral totalHits / fromIntegral totalLookups
                        else 0.0
            reducedStmts <- countStatements reducedSQL
            let reducedBytes = BS.length reducedSQL
                elapsedSecs = realToFrac (diffUTCTime endTime startTime) :: Double
                ratio =
                    if initialBytes > 0
                        then (1.0 - (fromIntegral reducedBytes / fromIntegral initialBytes :: Double)) * 100.0
                        else 0.0

            -- 4. Export artifacts
            createDirectoryIfMissing True optOutDir

            -- minimal.sql
            let minimalPath = optOutDir </> "minimal.sql"
            BS.writeFile minimalPath reducedSQL

            -- replay.sh
            let replayPath = optOutDir </> "replay.sh"
                replayScript =
                    unlines
                        [ "#!/usr/bin/env bash"
                        , "set -euo pipefail"
                        , "DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\""
                        , "echo \"==> Replaying minimal.sql against oracle...\""
                        , optOracleCmd ++ " < \"$DIR/minimal.sql\""
                        ]
            writeFile replayPath replayScript
            perms <- getPermissions replayPath
            setPermissions replayPath (setOwnerExecutable True perms)

            -- stats.json
            let statsPath = optOutDir </> "stats.json"
                stats =
                    Stats
                        { initial_bytes = initialBytes
                        , reduced_bytes = reducedBytes
                        , reduction_ratio = ratio
                        , initial_statements = initialStmts
                        , reduced_statements = reducedStmts
                        , oracle_executions = totalCalls
                        , cache_hits = totalHits
                        , cache_misses = totalMisses
                        , cache_hit_ratio = hitRatio
                        , elapsed_seconds = elapsedSecs
                        }
            BSL.writeFile statsPath (encode stats)

            -- 5. Print summary table
            putStrLn "----------------------------------------------------------------------"
            putStrLn "Reduction Complete!"
            putStrLn "======================================================================"
            putStrLn "Occam Reduction Summary"
            putStrLn "======================================================================"
            putStrLn $ "  Statements:   " ++ show initialStmts ++ " -> " ++ show reducedStmts
            putStrLn $
                "  Bytes:        "
                    ++ show initialBytes
                    ++ " B -> "
                    ++ show reducedBytes
                    ++ " B (-"
                    ++ showFFloat (Just 1) ratio "%"
                    ++ ")"
            putStrLn $ "  Oracle Calls: " ++ show totalCalls
            putStrLn $ "  Cache Hits:   " ++ show totalHits
            putStrLn $ "  Cache Misses: " ++ show totalMisses
            putStrLn $ "  Hit Ratio:    " ++ showFFloat (Just 1) (hitRatio * 100.0) "%"
            putStrLn $ "  Elapsed Time: " ++ showFFloat (Just 3) elapsedSecs "s"
            putStrLn $ "  Artifacts:    " ++ minimalPath
            putStrLn $ "                " ++ replayPath
            putStrLn $ "                " ++ statsPath
            putStrLn "======================================================================"

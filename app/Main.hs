{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, encode, toJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Occam qualified
import Occam.Cache (CacheStats (..), memoizeOracle)
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
    runExternalOracle,
 )
import Occam.Reducer (reduceSQLWithLogger)
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
    , optLogLevel :: !String
    , optLogFormat :: !String
    , optVerbose :: !Bool
    , optQuiet :: !Bool
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
runOccam opts@Options {..} = do
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
    withLogger logCfg $ \logger -> runOccamWithLogger logger opts

runOccamWithLogger :: Logger -> Options -> IO ()
runOccamWithLogger logger Options {..} = do
    fileExists <- doesFileExist optInputFile
    unless fileExists $ do
        logError logger "occam::cli" ("Error: Input file not found: " <> T.pack optInputFile)
        hPutStrLn stderr ("Error: Input file not found: " ++ optInputFile)
        exitFailure

    initialSQL <- BS.readFile optInputFile
    initialStmts <- countStatements initialSQL
    let initialBytes = BS.length initialSQL

    when (not optQuiet && parseLogFormat optLogFormat == FormatText) $ do
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

    logInfo'
        logger
        "occam::cli"
        "Starting reduction session"
        [ ("input_file", toJSON optInputFile)
        , ("initial_bytes", toJSON initialBytes)
        , ("initial_statements", toJSON initialStmts)
        , ("oracle_cmd", toJSON optOracleCmd)
        ]

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
    logInfo logger "occam::oracle" "Verifying initial crash reproduction against oracle..."
    initialOk <- runExternalOracle baseOracleCfg initialSQL
    unless initialOk $ do
        logError logger "occam::oracle" "Initial input does not reproduce target crash!"
        hPutStrLn stderr "Error: Initial input does not reproduce target crash!"
        exitFailure
    logInfo logger "occam::oracle" "Initial crash verified successfully"

    (oracle, getCacheStats) <- memoizeOracle (runExternalOracle baseOracleCfg)
    -- 2. Run hierarchical reduction engine
    logInfo logger "occam::reducer" "Running hierarchical reduction engine (Levels 1-4)..."
    startTime <- getCurrentTime
    reducedSQL <- reduceSQLWithLogger logger oracle initialSQL
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
    logInfo'
        logger
        "occam::cli"
        "Reduction session completed"
        [ ("initial_statements", toJSON initialStmts)
        , ("reduced_statements", toJSON reducedStmts)
        , ("initial_bytes", toJSON initialBytes)
        , ("reduced_bytes", toJSON reducedBytes)
        , ("reduction_ratio", toJSON ratio)
        , ("oracle_calls", toJSON totalCalls)
        , ("cache_hits", toJSON totalHits)
        , ("cache_misses", toJSON totalMisses)
        , ("elapsed_seconds", toJSON elapsedSecs)
        ]

    when (not optQuiet && parseLogFormat optLogFormat == FormatText) $ do
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

{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import Occam qualified
import Occam.Oracle (
    OracleConfig (..),
    runExternalOracle,
 )
import Occam.Reducer (reduceSQL)
import Occam.TreeSitter (extractStatements, parseSQL, withParser)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getPermissions,
    setOwnerExecutable,
    setPermissions,
 )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

data CLIConfig = CLIConfig
    { cliInputFile :: !FilePath
    , cliOracleCmd :: !String
    , cliOutDir :: !FilePath
    , cliResetCmd :: !(Maybe String)
    , cliTimeoutSecs :: !Double
    , cliFrames :: !Int
    }
    deriving (Eq, Show)

defaultConfig :: CLIConfig
defaultConfig =
    CLIConfig
        { cliInputFile = ""
        , cliOracleCmd = ""
        , cliOutDir = "./occam-out"
        , cliResetCmd = Nothing
        , cliTimeoutSecs = 5.0
        , cliFrames = 3
        }

parseArgs :: [String] -> Either String (Maybe CLIConfig)
parseArgs args
    | "--help" `elem` args || "-h" `elem` args = Right Nothing
    | "--version" `elem` args || "-v" `elem` args = Right Nothing
    | otherwise = go args defaultConfig
    where
        go [] cfg
            | null (cliInputFile cfg) = Left "Missing required input SQL file."
            | null (cliOracleCmd cfg) = Left "Missing required --oracle command."
            | otherwise = Right (Just cfg)
        go ("--oracle" : cmd : rest) cfg = go rest cfg {cliOracleCmd = cmd}
        go ("--out" : dir : rest) cfg = go rest cfg {cliOutDir = dir}
        go ("--reset" : cmd : rest) cfg = go rest cfg {cliResetCmd = Just cmd}
        go ("--timeout" : sec : rest) cfg =
            case reads sec of
                [(s, "")] -> go rest cfg {cliTimeoutSecs = s}
                _ -> Left $ "Invalid timeout value: " ++ sec
        go ("--frames" : n : rest) cfg =
            case reads n of
                [(f, "")] -> go rest cfg {cliFrames = f}
                _ -> Left $ "Invalid frames value: " ++ n
        go (arg : rest) cfg
            | "-" `isPrefixOf` arg = Left $ "Unrecognized option: " ++ arg
            | null (cliInputFile cfg) = go rest cfg {cliInputFile = arg}
            | otherwise = Left $ "Unexpected positional argument: " ++ arg

printHelp :: IO ()
printHelp = do
    putStrLn $ "Occam v" ++ Occam.version ++ ": Dependency-Aware SQL Crash Minimizer"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "  occam <input.sql> --oracle <oracle_cmd> [options]"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  --oracle <cmd>       Target oracle command (executable or script) [REQUIRED]"
    putStrLn "  --out <dir>          Directory to write artifacts (default: ./occam-out)"
    putStrLn "  --reset <cmd>        Lightweight state reset command (e.g. dropdb/createdb)"
    putStrLn "  --timeout <seconds>  Per-execution timeout in seconds (default: 5.0)"
    putStrLn "  --frames <N>         Number of top stack frames for crash equivalence (default: 3)"
    putStrLn "  --version, -v        Display version"
    putStrLn "  --help, -h           Display this help message"

countStatements :: BS.ByteString -> IO Int
countStatements sql = withParser $ \p -> do
    mTree <- parseSQL p sql
    case mTree of
        Nothing -> pure 0
        Just tree -> length <$> extractStatements tree sql

main :: IO ()
main = do
    args <- getArgs
    if "--version" `elem` args || "-v" `elem` args
        then putStrLn ("Occam v" ++ Occam.version) >> exitSuccess
        else
            if "--help" `elem` args || "-h" `elem` args || null args
                then printHelp >> exitSuccess
                else case parseArgs args of
                    Left err -> do
                        hPutStrLn stderr ("Error: " ++ err)
                        hPutStrLn stderr "Run 'occam --help' for usage."
                        exitFailure
                    Right Nothing -> printHelp >> exitSuccess
                    Right (Just cfg) -> runOccam cfg

runOccam :: CLIConfig -> IO ()
runOccam CLIConfig {..} = do
    fileExists <- doesFileExist cliInputFile
    unless fileExists $ do
        hPutStrLn stderr ("Error: Input file not found: " ++ cliInputFile)
        exitFailure

    initialSQL <- BS.readFile cliInputFile
    initialStmts <- countStatements initialSQL
    let initialBytes = BS.length initialSQL

    putStrLn "======================================================================"
    putStrLn $ "Occam v" ++ Occam.version ++ " starting reduction"
    putStrLn "======================================================================"
    putStrLn $
        "  Target SQL:   "
            ++ cliInputFile
            ++ " ("
            ++ show initialBytes
            ++ " bytes, "
            ++ show initialStmts
            ++ " stmts)"
    putStrLn $ "  Oracle Cmd:   " ++ cliOracleCmd
    putStrLn $ "  Artifact Dir: " ++ cliOutDir
    putStrLn "----------------------------------------------------------------------"

    -- Setup oracle and counter
    oracleCallsRef <- newIORef (0 :: Int)
    let timeoutMicros = round (cliTimeoutSecs * 1000000)
        baseOracleCfg =
            OracleConfig
                { oracleCommand = "sh"
                , oracleArgs = ["-c", cliOracleCmd]
                , oracleTimeoutMicros = timeoutMicros
                , oracleResetCommand = cliResetCmd
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

    let oracle sql = do
            modifyIORef' oracleCallsRef (+ 1)
            runExternalOracle baseOracleCfg sql

    -- 2. Run hierarchical reduction engine
    putStrLn "Running hierarchical reduction engine (Levels 1-4)..."
    startTime <- getCurrentTime
    reducedSQL <- reduceSQL oracle initialSQL
    endTime <- getCurrentTime

    -- 3. Gather metrics
    totalCalls <- readIORef oracleCallsRef
    reducedStmts <- countStatements reducedSQL
    let reducedBytes = BS.length reducedSQL
        elapsedSecs = realToFrac (diffUTCTime endTime startTime) :: Double
        ratio =
            if initialBytes > 0
                then (1.0 - (fromIntegral reducedBytes / fromIntegral initialBytes :: Double)) * 100.0
                else 0.0

    -- 4. Export artifacts
    createDirectoryIfMissing True cliOutDir

    -- minimal.sql
    let minimalPath = cliOutDir </> "minimal.sql"
    BS.writeFile minimalPath reducedSQL

    -- replay.sh
    let replayPath = cliOutDir </> "replay.sh"
        replayScript =
            unlines
                [ "#!/usr/bin/env bash"
                , "set -euo pipefail"
                , "DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\""
                , "echo \"==> Replaying minimal.sql against oracle...\""
                , cliOracleCmd ++ " < \"$DIR/minimal.sql\""
                ]
    writeFile replayPath replayScript
    perms <- getPermissions replayPath
    setPermissions replayPath (setOwnerExecutable True perms)

    -- stats.json
    let statsPath = cliOutDir </> "stats.json"
        statsJSON =
            concat
                [ "{\n"
                , "  \"initial_bytes\": " ++ show initialBytes ++ ",\n"
                , "  \"reduced_bytes\": " ++ show reducedBytes ++ ",\n"
                , "  \"reduction_ratio\": " ++ showFFloat (Just 2) ratio "" ++ ",\n"
                , "  \"initial_statements\": " ++ show initialStmts ++ ",\n"
                , "  \"reduced_statements\": " ++ show reducedStmts ++ ",\n"
                , "  \"oracle_executions\": " ++ show totalCalls ++ ",\n"
                , "  \"elapsed_seconds\": " ++ showFFloat (Just 3) elapsedSecs "" ++ "\n"
                , "}\n"
                ]
    writeFile statsPath statsJSON

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
    putStrLn $ "  Elapsed Time: " ++ showFFloat (Just 3) elapsedSecs "s"
    putStrLn $ "  Artifacts:    " ++ minimalPath
    putStrLn $ "                " ++ replayPath
    putStrLn $ "                " ++ statsPath
    putStrLn "======================================================================"

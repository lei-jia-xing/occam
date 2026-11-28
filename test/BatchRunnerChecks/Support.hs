-- | Shared local process, JSON, and filesystem helpers for harness tests.
module BatchRunnerChecks.Support (
    withTempDir,
    writeExec,
    isolatedEnvWithBin,
    requireBinary,
    runHarnessCli,
    runHarnessJson,
    readStrictText,
    readJsonFile,
    asObject,
    overviewOf,
    readAggregatedStats,
    assertRunCounts,
    asArray,
    getIntField,
    getBoolField,
    getTextField,
    getObjField,
    shellSingleQuote,
    waitForFile,
    writeMockPgReady,
) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (finally)
import Control.Monad (void, when)
import Data.Aeson (Object, Value (..), decodeStrict')
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Harness.Config (resolveHarnessConfigPath)
import Occam.Harness.DbmsExec (resolveRepoBinary)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import Test.Tasty.HUnit (assertEqual, assertFailure)

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    dir <- mkdtemp ("/tmp" </> prefix <> "-")
    action dir `finally` removeDirectoryRecursive dir

writeExec :: FilePath -> FilePath -> String -> IO FilePath
writeExec dir name body = do
    createDirectoryIfMissing True dir
    let path = dir </> name
    -- Write in a single-threaded child process so concurrent Haskell fork()
    -- calls never inherit an O_WRONLY descriptor to this script (Linux ETXTBSY).
    (ec, _out, err) <-
        readCreateProcessWithExitCode
            (proc "/bin/sh" ["-c", "cat > \"$1\" && chmod 0755 \"$1\"", "write-exec", path])
            body
    when (ec /= ExitSuccess) $
        assertFailure ("Failed to write mock executable " <> path <> ": " <> err)
    pure path

isolatedEnvWithBin :: FilePath -> IO [(String, String)]
isolatedEnvWithBin binDir = do
    parentEnv <- getEnvironment
    cfgPath <- resolveHarnessConfigPath Nothing
    let keepKeys =
            [ "HOME"
            , "TMPDIR"
            , "LANG"
            , "LC_ALL"
            , "LD_LIBRARY_PATH"
            , "OCCAM_PG_QUERY_LIB"
            , "OCCAM_DUCKDB_LIB"
            , "OCCAM_BIN"
            , "OCCAM_HARNESS_BIN"
            ]
        kept = [(k, v) | (k, v) <- parentEnv, k `elem` keepKeys]
        oldPath = fromMaybe "/usr/bin:/bin" (lookup "PATH" parentEnv)
    pure (("PATH", binDir <> ":" <> oldPath) : ("OCCAM_HARNESS_JSON", cfgPath) : kept)

requireBinary :: String -> String -> IO FilePath
requireBinary envVar binName = do
    eBin <- resolveRepoBinary Nothing envVar binName
    either assertFailure pure eBin

runHarnessCli :: [(String, String)] -> [String] -> IO (ExitCode, String, String)
runHarnessCli envList args = do
    harnessBin <- requireBinary "OCCAM_HARNESS_BIN" "occam-harness"
    readCreateProcessWithExitCode (proc harnessBin args) {env = Just envList} ""

runHarnessJson :: [(String, String)] -> [String] -> IO (ExitCode, Value, String, String)
runHarnessJson envList args = do
    (ec, outStr, errStr) <- runHarnessCli envList args
    let nonEmptyLines = filter (not . all (`elem` (" \t\r" :: String))) (lines outStr)
        parsed = case reverse nonEmptyLines of
            (lastLine : _) -> fromMaybe Null (decodeStrict' (TE.encodeUtf8 (T.pack lastLine)))
            [] -> Null
    pure (ec, parsed, outStr, errStr)

readStrictText :: FilePath -> IO String
readStrictText path = T.unpack . TE.decodeUtf8 <$> BS.readFile path

readJsonFile :: FilePath -> IO Value
readJsonFile path = do
    raw <- BS.readFile path
    case decodeStrict' raw of
        Just val -> pure val
        Nothing -> assertFailure ("Failed to decode JSON at " <> path)

asObject :: String -> Value -> IO Object
asObject _ (Object obj) = pure obj
asObject ctx other = assertFailure (ctx <> ": expected JSON object, got " <> show other)

overviewOf :: String -> Value -> IO Object
overviewOf ctx val =
    asObject ctx val
        >>= maybe (assertFailure (ctx <> ": missing overview")) pure . getObjField "overview"

readAggregatedStats :: FilePath -> IO (Object, Int, Int, Int)
readAggregatedStats outDir = do
    let aggPath = outDir </> "aggregated_stats.json"
    agg <- asObject "aggregated_stats.json" =<< readJsonFile aggPath
    ov <-
        maybe (assertFailure "missing overview in aggregated_stats.json") pure (getObjField "overview" agg)
    byProf <-
        maybe
            (assertFailure "missing by_profile in aggregated_stats.json")
            pure
            (getObjField "by_profile" agg)
    let profileObjs = [o | Object o <- KM.elems byProf]
        sumField k = sum [fromMaybe 0 (getIntField k o) | o <- profileObjs]
    pure (ov, sumField "completed_runs", sumField "censored_runs", sumField "skipped_unstable_runs")

assertRunCounts :: String -> FilePath -> Value -> Int -> Int -> Int -> Int -> IO ()
assertRunCounts ctx outDir summaryVal totalExp compExp censExp skipExp = do
    ovStdout <- overviewOf (ctx <> " stdout") summaryVal
    assertEqual
        (ctx <> " stdout total_job_runs")
        (Just totalExp)
        (getIntField "total_job_runs" ovStdout)
    (ovDisk, compRuns, censRuns, skipRuns) <- readAggregatedStats outDir
    assertEqual (ctx <> " disk total_job_runs") (Just totalExp) (getIntField "total_job_runs" ovDisk)
    assertEqual (ctx <> " completed_runs") compExp compRuns
    assertEqual (ctx <> " censored_runs") censExp censRuns
    assertEqual (ctx <> " skipped_unstable_runs") skipExp skipRuns

asArray :: String -> Value -> IO [Value]
asArray _ (Array arr) = pure (foldr (:) [] arr)
asArray ctx other = assertFailure (ctx <> ": expected JSON array, got " <> show other)

getIntField :: Text -> Object -> Maybe Int
getIntField k obj = case KM.lookup (Key.fromText k) obj of
    Just (Number n) -> Just (round n)
    _ -> Nothing

getBoolField :: Text -> Object -> Maybe Bool
getBoolField k obj = case KM.lookup (Key.fromText k) obj of
    Just (Bool b) -> Just b
    _ -> Nothing

getTextField :: Text -> Object -> Maybe Text
getTextField k obj = case KM.lookup (Key.fromText k) obj of
    Just (String t) -> Just t
    _ -> Nothing

getObjField :: Text -> Object -> Maybe Object
getObjField k obj = case KM.lookup (Key.fromText k) obj of
    Just (Object sub) -> Just sub
    _ -> Nothing

shellSingleQuote :: FilePath -> String
shellSingleQuote p = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) p <> "'"

waitForFile :: FilePath -> Int -> IO Bool
waitForFile path timeoutMs = go 0
    where
        go elapsed
            | elapsed >= timeoutMs = doesFileExist path
            | otherwise = do
                ok <- doesFileExist path
                if ok
                    then pure True
                    else do
                        threadDelay 25_000
                        go (elapsed + 25)

writeMockPgReady :: FilePath -> IO ()
writeMockPgReady dir = void (writeExec dir "pg_isready" "#!/bin/sh\nexit 0\n")

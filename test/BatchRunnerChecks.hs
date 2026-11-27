-- | End-to-end and API checks for native collect, N-replay, and batch orchestration.
module BatchRunnerChecks (
    tests,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception.Safe (finally)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (Object, Value (..), decodeStrict')
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.IORef (newIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (diffUTCTime, getCurrentTime)
import Occam.Harness.Config (HarnessConfig, loadHarnessConfig, resolveHarnessConfigPath)
import Occam.Harness.DbmsExec (
    ConnectionOverrides (..),
    emptyConnectionOverrides,
    resolveRepoBinary,
 )
import Occam.Harness.Replay (
    ReplayConfig (..),
    ReplayVerification (..),
    defaultReplayConfig,
    runReplayVerification,
 )
import Occam.Harness.Signature (CrashSignature (..), DbmsKind (..))
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    removeDirectoryRecursive,
    removeFile,
 )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import System.Posix.Signals (sigINT, signalProcess)
import System.Posix.Temp (mkdtemp)
import System.Process (
    CreateProcess (..),
    StdStream (CreatePipe),
    createProcess,
    getPid,
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Harness Collect, Replay & Batch Runner"
        [ testCase
            "collect imports squirrel & sqlancer artifacts and handles cur-log flags"
            testCollectSources
        , testCase
            "collect --verify filters unstable inputs, dedups signatures, overwrites logs, and validates timeout"
            testCollectVerify
        , testCase
            "Replay API verifies consistency, slippage drift, missing baseline, and live IORef cancellation"
            testReplayApiAndCancel
        , testCase
            "batch --dry-run plans real corpus (71 -> 1278) and validates unsafe occam-jobs / flags"
            testBatchDryRunAndValidation
        , testCase
            "batch --containers drives external workers without docker run or rm"
            testBatchExternalContainers
        , testCase
            "batch managed workers qualify, catch slippage, clean up containers, and resume with zero docker calls"
            testBatchManagedWorkersAndResume
        , testCase
            "batch managed container recreate updates mapped port after reset failure (15432 -> 15433)"
            testBatchManagedRecreatePort
        , testCase
            "batch --local-embedded with real occam covers all 6 profiles, budgets, aggregate-only, and hash resume"
            testBatchLocalEmbeddedRealOccam
        , testCase
            "batch --local-embedded handles SIGINT (exit 130) and resumes cleanly to 2/2 completed"
            testBatchLocalEmbeddedSigintResume
        ]

-- ---------------------------------------------------------------------------
-- Shared local test helpers
-- ---------------------------------------------------------------------------

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    dir <- mkdtemp ("/tmp" </> prefix <> "-")
    action dir `finally` removeDirectoryRecursive dir

writeExec :: FilePath -> FilePath -> String -> IO FilePath
writeExec dir name body = do
    createDirectoryIfMissing True dir
    let path = dir </> name
    writeFile path body
    setFileMode path 0o755
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
    agg <- asObject "aggregated_stats.json" =<< readJsonFile (outDir </> "aggregated_stats.json")
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

-- ---------------------------------------------------------------------------
-- 1. collect & --verify tests
-- ---------------------------------------------------------------------------

testCollectSources :: IO ()
testCollectSources = withTempDir "occam-collect-src" \td -> do
    envList <- isolatedEnvWithBin (td </> "empty-bin")
    let sqIn = td </> "squirrel_in"
        sqOut = td </> "squirrel_out"
    createDirectoryIfMissing True (sqIn </> "worker_0" </> "crashes")
    createDirectoryIfMissing True (sqIn </> "worker_1" </> "crashes")
    writeFile (sqIn </> "worker_0" </> "crashes" </> "README.txt") "ignore me"
    writeFile (sqIn </> "worker_0" </> "crashes" </> ".hidden") "SELECT 0;"
    writeFile (sqIn </> "worker_0" </> "crashes" </> "id:000001") "SELECT 1;\n"
    writeFile (sqIn </> "worker_1" </> "crashes" </> "id:000002") "SELECT 1;\n"
    writeFile (sqIn </> "worker_1" </> "crashes" </> "id:000003") "SELECT 2;"

    (ecSq, sumSq, outSq, errSq) <-
        runHarnessJson
            envList
            [ "collect"
            , "--source"
            , "squirrel"
            , "--dbms"
            , "postgres"
            , "--input-dir"
            , sqIn
            , "--out-dir"
            , sqOut
            ]
    assertEqual ("squirrel collect failed: " <> outSq <> errSq) ExitSuccess ecSq
    sumSqObj <- asObject "squirrel summary" sumSq
    assertEqual "squirrel raw candidates" (Just 3) (getIntField "raw_candidates_discovered" sumSqObj)
    assertEqual
        "squirrel unique byte candidates"
        (Just 2)
        (getIntField "unique_byte_candidates" sumSqObj)
    assertEqual "squirrel verified false" (Just False) (getBoolField "verified" sumSqObj)

    sqManifest <- asObject "squirrel manifest" =<< readJsonFile (sqOut </> "manifest.json")
    cases <-
        asArray "squirrel cases"
            =<< maybe (assertFailure "missing cases") pure (KM.lookup "cases" sqManifest)
    assertEqual "squirrel deduped case count" 2 (length cases)
    forM_ cases \caseVal -> do
        cObj <- asObject "squirrel case" caseVal
        assertEqual "unverified qualified null" (Just Null) (KM.lookup "qualified" cObj)
        assertEqual "unverified baseline null" (Just Null) (KM.lookup "baseline_signature" cObj)

    let slIn = td </> "sqlancer_in"
        slOut1 = td </> "sqlancer_out1"
        slOut2 = td </> "sqlancer_out2"
    createDirectoryIfMissing True (slIn </> "database0")
    writeFile (slIn </> "database0" </> "exc.log") "SELECT 10;\njava.lang.AssertionError: boom\n"
    writeFile (slIn </> "database0" </> "database0-cur.log") "\\c test;\nSELECT 20;\n"
    writeFile (slIn </> "database0" </> "database0-plan.log") "SELECT 999;\n"

    (ecSl1, _, _, errSl1) <-
        runHarnessJson
            envList
            [ "collect"
            , "--source"
            , "sqlancer"
            , "--dbms"
            , "postgres"
            , "--input-dir"
            , slIn
            , "--out-dir"
            , slOut1
            ]
    assertEqual ("sqlancer primary failed: " <> errSl1) ExitSuccess ecSl1
    mSl1 <- asObject "sl1 manifest" =<< readJsonFile (slOut1 </> "manifest.json")
    sl1Sum <- maybe (assertFailure "missing summary") pure (getObjField "summary" mSl1)
    assertEqual "primary excludes cur log" (Just 1) (getIntField "raw_candidates_discovered" sl1Sum)

    (ecSl2, _, _, errSl2) <-
        runHarnessJson
            envList
            [ "collect"
            , "--source"
            , "sqlancer"
            , "--dbms"
            , "postgres"
            , "--input-dir"
            , slIn
            , "--out-dir"
            , slOut2
            , "--include-cur-logs"
            ]
    assertEqual ("sqlancer include-cur failed: " <> errSl2) ExitSuccess ecSl2
    mSl2 <- asObject "sl2 manifest" =<< readJsonFile (slOut2 </> "manifest.json")
    sl2Sum <- maybe (assertFailure "missing summary") pure (getObjField "summary" mSl2)
    assertEqual "include-cur keeps both" (Just 2) (getIntField "raw_candidates_discovered" sl2Sum)

testCollectVerify :: IO ()
testCollectVerify = withTempDir "occam-collect-verify" \td -> do
    let binDir = td </> "bin"
        rawIn = td </> "raw"
        outDedup = td </> "out_dedup"
        pgLog = td </> "pg.log"
        unstableCount = td </> "c4.count"
    createDirectoryIfMissing True rawIn
    writeFile pgLog ""
    writeFile (rawIn </> "c1.sql") "SELECT /* sigA1 */ 1;\n"
    writeFile (rawIn </> "c2.sql") "SELECT /* sigA2 */ 2;\n"
    writeFile (rawIn </> "c3.sql") "SELECT /* sigB */ 3;\n"
    writeFile (rawIn </> "c4.sql") "SELECT /* unstable */ 4;\n"

    writeMockPgReady binDir
    _ <-
        writeExec
            binDir
            "psql"
            ( unlines
                [ "#!/bin/sh"
                , "for arg in \"$@\"; do"
                , "  if [ \"$arg\" = \"-lqt\" ]; then"
                , "    printf ' occam_test | postgres\\n'"
                , "    exit 0"
                , "  fi"
                , "done"
                , "case \" $* \" in"
                , "  *\"-d template1\"*) exit 0 ;;"
                , "esac"
                , "sql=$(cat)"
                , "case \"$sql\" in"
                , "  *sigA*)"
                , "    printf '2026-01-01 00:00:00 UTC [10] TRAP: FailedAssertion(\"okA\", File: \"exec.c\", Line: 10)\\n' >> "
                    <> shellSingleQuote pgLog
                , "    printf 'server closed the connection unexpectedly\\n' >&2"
                , "    exit 2"
                , "    ;;"
                , "  *sigB*)"
                , "    printf '2026-01-01 00:00:00 UTC [11] TRAP: FailedAssertion(\"okB\", File: \"exec.c\", Line: 11)\\n' >> "
                    <> shellSingleQuote pgLog
                , "    printf 'server closed the connection unexpectedly\\n' >&2"
                , "    exit 2"
                , "    ;;"
                , "  *unstable*)"
                , "    n=0"
                , "    if [ -f "
                    <> shellSingleQuote unstableCount
                    <> " ]; then n=$(cat "
                    <> shellSingleQuote unstableCount
                    <> "); fi"
                , "    n=$((n + 1))"
                , "    printf '%s' \"$n\" > " <> shellSingleQuote unstableCount
                , "    if [ \"$n\" -eq 1 ]; then"
                , "      printf '2026-01-01 00:00:00 UTC [12] TRAP: FailedAssertion(\"flaky\", File: \"exec.c\", Line: 12)\\n' >> "
                    <> shellSingleQuote pgLog
                , "      printf 'server closed the connection unexpectedly\\n' >&2"
                , "      exit 2"
                , "    fi"
                , "    exit 0"
                , "    ;;"
                , "esac"
                , "exit 0"
                ]
            )

    envList <- isolatedEnvWithBin binDir
    let baseArgs extra =
            [ "collect"
            , "--source"
            , "raw"
            , "--dbms"
            , "postgres"
            , "--input-dir"
            , rawIn
            , "--out-dir"
            , outDedup
            , "--verify"
            , "--replays"
            , "3"
            , "--log-file"
            , pgLog
            ]
                <> extra

    (ec1, sum1, out1, err1) <- runHarnessJson envList (baseArgs [])
    assertEqual ("collect --verify failed: " <> out1 <> err1) ExitSuccess ec1
    sObj1 <- asObject "verify summary" sum1
    assertEqual "rejected c4 count" (Just 1) (getIntField "rejected_unstable_or_noncrashing" sObj1)
    assertEqual "collapsed sigA duplicate" (Just 1) (getIntField "signature_duplicates_collapsed" sObj1)
    assertEqual "unique signatures" (Just 2) (getIntField "unique_signatures" sObj1)
    assertEqual "manifest count after dedup" (Just 2) (getIntField "manifest_case_count" sObj1)

    m1 <- asObject "verify manifest" =<< readJsonFile (outDedup </> "manifest.json")
    kept1 <- asArray "kept cases" =<< maybe (assertFailure "missing cases") pure (KM.lookup "cases" m1)
    assertEqual "2 cases kept after sig dedup" 2 (length kept1)
    rej1 <-
        asArray "rejected cases"
            =<< maybe (assertFailure "missing rejected_cases") pure (KM.lookup "rejected_cases" m1)
    assertEqual "1 rejected unstable case" 1 (length rej1)

    let decLog1 = outDedup </> "verify_logs" </> "c1_decisions.jsonl"
    decLines1 <- length . lines <$> readStrictText decLog1
    assertEqual "3 replay decision lines initially" 3 decLines1

    -- Rerun on same out-dir with --no-dedup-by-signature: verify_logs must be overwritten cleanly!
    writeFile unstableCount "0"
    (ec2, sum2, _, err2) <- runHarnessJson envList (baseArgs ["--no-dedup-by-signature"])
    assertEqual ("collect --verify rerun failed: " <> err2) ExitSuccess ec2
    sObj2 <- asObject "verify rerun summary" sum2
    assertEqual "manifest count without sig dedup" (Just 3) (getIntField "manifest_case_count" sObj2)
    assertEqual "collapsed count zero" (Just 0) (getIntField "signature_duplicates_collapsed" sObj2)
    decLines2 <- length . lines <$> readStrictText decLog1
    assertEqual "verify_logs overwritten cleanly (still 3 lines)" 3 decLines2

    -- Invalid --probe-timeout values fail validation cleanly (exit 2)
    forM_ ["--probe-timeout=0", "--probe-timeout=-1", "--probe-timeout=nan", "--probe-timeout=inf"] \badFlag -> do
        (ecBad, _, errBad) <- runHarnessCli envList (baseArgs [badFlag])
        assertBool ("expected failure for " <> badFlag <> ", got 0: " <> errBad) (ecBad /= ExitSuccess)

-- ---------------------------------------------------------------------------
-- 2. Replay API tests
-- ---------------------------------------------------------------------------

testReplayApiAndCancel :: IO ()
testReplayApiAndCancel = withTempDir "occam-replay-api" \td -> do
    cfg <- loadDefaultHarnessConfig
    let modeFile = td </> "mode.txt"
        callCount = td </> "calls.txt"
        engDrift = td </> "sqlite_drift"
        engSleep = td </> "sqlite_sleep"
    _ <-
        writeExec
            td
            "sqlite_drift"
            ( unlines
                [ "#!/bin/sh"
                , "mode=$(cat " <> shellSingleQuote modeFile <> ")"
                , "n=0"
                , "if [ -f "
                    <> shellSingleQuote callCount
                    <> " ]; then n=$(cat "
                    <> shellSingleQuote callCount
                    <> "); fi"
                , "n=$((n + 1))"
                , "printf '%s' \"$n\" > " <> shellSingleQuote callCount
                , "if [ \"$mode\" = \"consistent\" ] || [ \"$n\" -eq 1 ]; then"
                , "  printf '==1==ERROR: AddressSanitizer: SEGV on unknown address\\n    #0 0x100 in func_alpha /src/a.c:10\\n' >&2"
                , "else"
                , "  printf '==2==ERROR: AddressSanitizer: SEGV on unknown address\\n    #0 0x200 in func_beta /src/b.c:20\\n' >&2"
                , "fi"
                , "exit 134"
                ]
            )
    _ <-
        writeExec
            td
            "sqlite_sleep"
            "#!/bin/sh\nexec sleep 15\n"

    let mkCfg bin baseSig =
            (defaultReplayConfig DbmsSqlite (emptyConnectionOverrides {coEngineBin = Just bin}))
                { rcBaselineSignature = baseSig
                , rcRejectOnSlippage = False
                , rcProbeTimeout = 10.0
                , rcResetBeforeRun = False
                , rcRequireConsistent = True
                }

    -- Consistent, no baseline -> rvMatchesBaseline Nothing
    writeFile modeFile "consistent"
    removeIfExists callCount
    rvCons <- runReplayVerification cfg Nothing (mkCfg engDrift Nothing) 3 "SELECT 1;"
    assertEqual "allPassed" True (rvAllPassed rvCons)
    assertEqual "consistentSignature" True (rvConsistentSignature rvCons)
    assertEqual "no baseline -> matchesBaseline Nothing" Nothing (rvMatchesBaseline rvCons)
    baseSigId <- case rvBaselineSignature rvCons of
        Just s -> pure (csId s)
        Nothing -> assertFailure "expected baseline signature from consistent replay"

    -- Slippage / signature drift across 2 runs against baseSigId
    writeFile modeFile "drift"
    removeIfExists callCount
    rvDrift <- runReplayVerification cfg Nothing (mkCfg engDrift (Just baseSigId)) 2 "SELECT 1;"
    assertEqual "drift still passes crashes" True (rvAllPassed rvDrift)
    assertEqual "drift consistentSignature False" False (rvConsistentSignature rvDrift)
    assertEqual "drift matchesBaseline Just False" (Just False) (rvMatchesBaseline rvDrift)
    assertEqual "drift slippage count 1" 1 (rvSlippageCount rvDrift)

    -- Pre-set IORef stop flag cancels immediately with zero runs
    stopRef1 <- newIORef True
    rvPreCancel <- runReplayVerification cfg (Just stopRef1) (mkCfg engSleep Nothing) 3 "SELECT 1;"
    assertEqual "pre-cancel interrupted" True (rvInterrupted rvPreCancel)
    assertEqual "pre-cancel zero runs" 0 (length (rvRuns rvPreCancel))

    -- Live cancellation during sleep 15 subprocess completes quickly (< 5s)
    stopRef2 <- newIORef False
    tStart <- getCurrentTime
    _ <- forkIO do
        threadDelay 200_000
        writeIORef stopRef2 True
    rvLiveCancel <- runReplayVerification cfg (Just stopRef2) (mkCfg engSleep Nothing) 2 "SELECT 1;"
    tEnd <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime tEnd tStart) :: Double
    assertEqual "live cancel interrupted" True (rvInterrupted rvLiveCancel)
    assertBool ("live cancel took too long: " <> show elapsed) (elapsed < 5.0)
    where
        removeIfExists p = do
            ok <- doesFileExist p
            when ok (removeFile p)

loadDefaultHarnessConfig :: IO HarnessConfig
loadDefaultHarnessConfig = do
    cfgPath <- resolveHarnessConfigPath Nothing
    either assertFailure pure =<< loadHarnessConfig cfgPath

-- ---------------------------------------------------------------------------
-- 3. batch --dry-run & validation tests
-- ---------------------------------------------------------------------------

testBatchDryRunAndValidation :: IO ()
testBatchDryRunAndValidation = withTempDir "occam-batch-dry" \td -> do
    envList <- isolatedEnvWithBin (td </> "empty-bin")
    let rawSql = td </> "sample.sql"
    writeFile rawSql "SELECT 1;\n"

    -- Full 71-case thesis corpus dry run -> 1278 jobs
    (ecThesis, valThesis, outThesis, errThesis) <-
        runHarnessJson
            envList
            [ "batch"
            , "--dry-run"
            , "--corpus-dir"
            , "benchmarks/real_fuzz_corpus"
            , "--plan"
            , "stage2-thesis"
            , "--out-dir"
            , td </> "thesis_out"
            ]
    assertEqual ("thesis dry-run failed: " <> outThesis <> errThesis) ExitSuccess ecThesis
    tObj <- asObject "thesis dry-run" valThesis
    assertEqual "71 corpus cases" (Just 71) (getIntField "case_count" tObj)
    assertEqual "1278 thesis jobs" (Just 1278) (getIntField "total_jobs" tObj)

    -- Overrides adjust planned jobs
    (ecOv, valOv, _, errOv) <-
        runHarnessJson
            envList
            [ "batch"
            , "--dry-run"
            , "--corpus-dir"
            , "benchmarks/real_fuzz_corpus"
            , "--plan"
            , "stage2-thesis"
            , "--repeats"
            , "1"
            , "--jobs"
            , "2"
            , "--profiles"
            , "Ours-S,ddmin-only"
            , "--out-dir"
            , td </> "ov_out"
            ]
    assertEqual ("override dry-run failed: " <> errOv) ExitSuccess ecOv
    ovObj <- asObject "override dry-run" valOv
    assertEqual "override 142 jobs" (Just 142) (getIntField "total_jobs" ovObj)

    -- --occam-jobs 2 rejected on postgres unless --allow-unsafe-occam-jobs or sqlite --local-embedded
    (ecUnsafe, _, _) <-
        runHarnessCli
            envList
            [ "batch"
            , "--dry-run"
            , "--input"
            , rawSql
            , "--dbms"
            , "postgres"
            , "--occam-jobs"
            , "2"
            , "--out-dir"
            , td </> "unsafe_out"
            ]
    assertBool "--occam-jobs 2 on postgres must fail" (ecUnsafe /= ExitSuccess)

    (ecAllowUnsafe, _, errAllow) <-
        runHarnessCli
            envList
            [ "batch"
            , "--dry-run"
            , "--input"
            , rawSql
            , "--dbms"
            , "postgres"
            , "--occam-jobs"
            , "2"
            , "--allow-unsafe-occam-jobs"
            , "--out-dir"
            , td </> "allow_unsafe_out"
            ]
    assertEqual ("--allow-unsafe-occam-jobs failed: " <> errAllow) ExitSuccess ecAllowUnsafe

    (ecSqliteJobs, _, errSqJobs) <-
        runHarnessCli
            envList
            [ "batch"
            , "--dry-run"
            , "--local-embedded"
            , "--input"
            , rawSql
            , "--dbms"
            , "sqlite"
            , "--occam-jobs"
            , "2"
            , "--out-dir"
            , td </> "sq_jobs_out"
            ]
    assertEqual ("sqlite --occam-jobs 2 failed: " <> errSqJobs) ExitSuccess ecSqliteJobs

    -- --jobs 2 with 1 external container fails
    (ecFewExt, _, _) <-
        runHarnessCli
            envList
            [ "batch"
            , "--dry-run"
            , "--input"
            , rawSql
            , "--dbms"
            , "postgres"
            , "--containers"
            , "ext0@127.0.0.1:25431"
            , "--jobs"
            , "2"
            , "--out-dir"
            , td </> "few_ext_out"
            ]
    assertBool "--jobs 2 with 1 container must fail" (ecFewExt /= ExitSuccess)

    -- Bad numeric flags & missing manifest rejected cleanly
    forM_
        [ ["--jobs", "0"]
        , ["--repeats", "0"]
        , ["--replays=-1"]
        , ["--probe-timeout", "0"]
        , ["--probe-timeout=-1"]
        , ["--probe-timeout", "nan"]
        ]
        \badOpt -> do
            (ecBad, _, _) <-
                runHarnessCli
                    envList
                    ( [ "batch"
                      , "--dry-run"
                      , "--local-embedded"
                      , "--input"
                      , rawSql
                      , "--dbms"
                      , "sqlite"
                      , "--out-dir"
                      , td </> "bad_out"
                      ]
                        <> badOpt
                    )
            assertBool ("expected validation error for " <> show badOpt) (ecBad /= ExitSuccess)

-- ---------------------------------------------------------------------------
-- 4. batch external --containers tests
-- ---------------------------------------------------------------------------

testBatchExternalContainers :: IO ()
testBatchExternalContainers = withTempDir "occam-batch-ext" \td -> do
    let binDir = td </> "bin"
        dockerLog = td </> "docker.log"
        pgLogW0 = td </> "ext_pg0.log"
        pgLogW1 = td </> "ext_pg1.log"
        sqlA = td </> "case_a.sql"
        sqlB = td </> "case_b.sql"
        outDir = td </> "out"
    writeFile dockerLog ""
    writeFile pgLogW0 ""
    writeFile pgLogW1 ""
    writeFile sqlA "SELECT 1;\nSELECT 2;\n"
    writeFile sqlB "SELECT 3;\nSELECT 4;\n"

    writeMockPgReady binDir
    _ <-
        writeExec
            binDir
            "docker"
            ( unlines
                [ "#!/bin/sh"
                , "printf '%s\\n' \"$*\" >> " <> shellSingleQuote dockerLog
                , "case \"$1\" in"
                , "  inspect)"
                , "    printf 'true\\n'"
                , "    exit 0"
                , "    ;;"
                , "  port)"
                , "    printf '127.0.0.1:25432\\n'"
                , "    exit 0"
                , "    ;;"
                , "  exec)"
                , "    cname=\"$2\"; shift 2"
                , "    logf=" <> shellSingleQuote pgLogW1
                , "    case \"$cname\" in *0) logf=" <> shellSingleQuote pgLogW0 <> " ;; esac"
                , "    case \"$1\" in"
                , "      stat)"
                , "        sz=$(stat -c '%s %i' \"$logf\" 2>/dev/null || printf '0 1')"
                , "        printf '%s\\n' \"$sz\""
                , "        exit 0"
                , "        ;;"
                , "      tail)"
                , "        tail -c \"$3\" \"$logf\""
                , "        exit 0"
                , "        ;;"
                , "    esac"
                , "    ;;"
                , "  logs)"
                , "    exit 0"
                , "    ;;"
                , "esac"
                , "exit 0"
                ]
            )

    _ <-
        writeExec
            binDir
            "psql"
            ( unlines
                [ "#!/bin/sh"
                , "port=''"
                , "prev=''"
                , "for arg in \"$@\"; do"
                , "  if [ \"$prev\" = \"-p\" ]; then port=\"$arg\"; fi"
                , "  prev=\"$arg\""
                , "done"
                , "logf=" <> shellSingleQuote pgLogW1
                , "if [ \"$port\" = \"25431\" ]; then logf=" <> shellSingleQuote pgLogW0 <> "; fi"
                , "case \" $* \" in"
                , "  *\"-d template1\"*) exit 0 ;;"
                , "esac"
                , "sql=$(cat)"
                , "case \"$sql\" in"
                , "  *SELECT*)"
                , "    printf '2026-01-01 00:00:00 UTC [42] TRAP: FailedAssertion(\"ext\", File: \"exec.c\", Line: 42)\\n' >> \"$logf\""
                , "    printf 'server closed the connection unexpectedly\\n' >&2"
                , "    exit 2"
                , "    ;;"
                , "esac"
                , "exit 0"
                ]
            )

    mockOccam <-
        writeExec
            binDir
            "mock-occam"
            ( unlines
                [ "#!/bin/sh"
                , "in_sql=\"$1\"; shift"
                , "out_sql=\"\"; stats_path=\"\"; oracle_cmd=\"\""
                , "while [ \"$#\" -gt 0 ]; do"
                , "  case \"$1\" in"
                , "    --out) out_sql=\"$2\"; shift 2 ;;"
                , "    --stats) stats_path=\"$2\"; shift 2 ;;"
                , "    --oracle) oracle_cmd=\"$2\"; shift 2 ;;"
                , "    *) shift ;;"
                , "  esac"
                , "done"
                , "sh -c \"$oracle_cmd\" < \"$in_sql\" >/dev/null 2>&1 || exit 2"
                , "head -n 1 \"$in_sql\" > \"$out_sql\""
                , "printf '{\"status\":\"completed\",\"oracle_calls\":1,\"evaluation_metrics\":{\"evaluation_validation_passed\":true,\"reduced_bytes\":10,\"compression_ratio\":0.5}}\\n' > \"$stats_path\""
                ]
            )

    envList <- isolatedEnvWithBin binDir
    (ec, summary, outStr, errStr) <-
        runHarnessJson
            envList
            [ "batch"
            , "--inputs"
            , sqlA
            , sqlB
            , "--dbms"
            , "postgres"
            , "--containers"
            , "ext_pg0@127.0.0.1:25431,ext_pg1"
            , "--jobs"
            , "2"
            , "--repeats"
            , "1"
            , "--replays"
            , "2"
            , "--profiles"
            , "Ours-S"
            , "--occam-bin"
            , mockOccam
            , "--out-dir"
            , outDir
            ]
    assertEqual ("external batch failed: " <> outStr <> errStr) ExitSuccess ec
    assertRunCounts "external" outDir summary 2 2 0 0

    dockerCalls <- lines <$> readFile dockerLog
    assertBool ("unexpected docker run in external mode: " <> unlines dockerCalls) $
        not (any (\l -> "run " `isInfixOf` (" " <> l) && take 4 l == "run ") dockerCalls)
    assertBool ("unexpected docker rm in external mode: " <> unlines dockerCalls) $
        not (any (\l -> take 3 l == "rm ") dockerCalls)
    assertBool ("expected docker port resolution for ext_pg1: " <> unlines dockerCalls) $
        any ("port ext_pg1" `isInfixOf`) dockerCalls

-- ---------------------------------------------------------------------------
-- 5. batch managed workers, --resume zero docker, and recreate port update
-- ---------------------------------------------------------------------------

testBatchManagedWorkersAndResume :: IO ()
testBatchManagedWorkersAndResume = withTempDir "occam-batch-managed" \td -> do
    let binDir = td </> "bin"
        dockerLog = td </> "docker.log"
        pgLogW0 = td </> "managed_w0.log"
        pgLogW1 = td </> "managed_w1.log"
        unstableCounter = td </> "unstable.count"
        occamCalls = td </> "occam_calls.log"
        casesDir = td </> "cases"
        outDir = td </> "out"
    createDirectoryIfMissing True casesDir
    writeFile dockerLog ""
    writeFile pgLogW0 ""
    writeFile pgLogW1 ""
    writeFile occamCalls ""

    writeFile (casesDir </> "c_ok.sql") "SELECT 'ok';\nSELECT 'ok_drop';\n"
    writeFile (casesDir </> "c_slip.sql") "SELECT 'slip_orig';\nSELECT 'slip_drop';\n"
    writeFile (casesDir </> "c_unstable.sql") "SELECT 'unstable';\n"

    writeMockPgReady binDir
    _ <-
        writeExec
            binDir
            "docker"
            ( unlines
                [ "#!/bin/sh"
                , "printf '%s\\n' \"$*\" >> " <> shellSingleQuote dockerLog
                , "case \"$1\" in"
                , "  run)"
                , "    printf 'container-id-123\\n'"
                , "    exit 0"
                , "    ;;"
                , "  port)"
                , "    case \"$2\" in"
                , "      *-w0) printf '127.0.0.1:26430\\n' ;;"
                , "      *) printf '127.0.0.1:26431\\n' ;;"
                , "    esac"
                , "    exit 0"
                , "    ;;"
                , "  exec)"
                , "    cname=\"$2\"; shift 2"
                , "    logf=" <> shellSingleQuote pgLogW1
                , "    case \"$cname\" in *-w0) logf=" <> shellSingleQuote pgLogW0 <> " ;; esac"
                , "    case \"$1\" in"
                , "      stat)"
                , "        stat -c '%s %i' \"$logf\" 2>/dev/null || printf '0 1\\n'"
                , "        exit 0"
                , "        ;;"
                , "      tail)"
                , "        tail -c \"$3\" \"$logf\""
                , "        exit 0"
                , "        ;;"
                , "    esac"
                , "    ;;"
                , "esac"
                , "exit 0"
                ]
            )

    _ <-
        writeExec
            binDir
            "psql"
            ( unlines
                [ "#!/bin/sh"
                , "port=''"
                , "prev=''"
                , "for arg in \"$@\"; do"
                , "  if [ \"$prev\" = \"-p\" ]; then port=\"$arg\"; fi"
                , "  prev=\"$arg\""
                , "done"
                , "logf=" <> shellSingleQuote pgLogW1
                , "if [ \"$port\" = \"26430\" ]; then logf=" <> shellSingleQuote pgLogW0 <> "; fi"
                , "case \" $* \" in"
                , "  *\"-d template1\"*) exit 0 ;;"
                , "esac"
                , "sql=$(cat)"
                , "case \"$sql\" in"
                , "  *unstable*)"
                , "    n=0"
                , "    if [ -f "
                    <> shellSingleQuote unstableCounter
                    <> " ]; then n=$(cat "
                    <> shellSingleQuote unstableCounter
                    <> "); fi"
                , "    n=$((n + 1))"
                , "    printf '%s' \"$n\" > " <> shellSingleQuote unstableCounter
                , "    if [ \"$n\" -eq 1 ]; then"
                , "      printf '2026-01-01 00:00:00 UTC [1] TRAP: FailedAssertion(\"u\", File: \"exec.c\", Line: 1)\\n' >> \"$logf\""
                , "      printf 'server closed the connection unexpectedly\\n' >&2"
                , "      exit 2"
                , "    fi"
                , "    exit 0"
                , "    ;;"
                , "  *slip_reduced*)"
                , "    printf '2026-01-01 00:00:00 UTC [2] TRAP: FailedAssertion(\"beta\", File: \"exec.c\", Line: 2)\\n' >> \"$logf\""
                , "    printf 'server closed the connection unexpectedly\\n' >&2"
                , "    exit 2"
                , "    ;;"
                , "  *slip_orig*)"
                , "    printf '2026-01-01 00:00:00 UTC [3] TRAP: FailedAssertion(\"alpha\", File: \"exec.c\", Line: 3)\\n' >> \"$logf\""
                , "    printf 'server closed the connection unexpectedly\\n' >&2"
                , "    exit 2"
                , "    ;;"
                , "  *ok*)"
                , "    printf '2026-01-01 00:00:00 UTC [4] TRAP: FailedAssertion(\"alpha\", File: \"exec.c\", Line: 4)\\n' >> \"$logf\""
                , "    printf 'server closed the connection unexpectedly\\n' >&2"
                , "    exit 2"
                , "    ;;"
                , "esac"
                , "exit 0"
                ]
            )

    mockOccam <-
        writeExec
            binDir
            "mock-occam"
            ( unlines
                [ "#!/bin/sh"
                , "in_sql=\"$1\"; shift"
                , "printf '%s\\n' \"$in_sql\" >> " <> shellSingleQuote occamCalls
                , "out_sql=\"\"; stats_path=\"\""
                , "while [ \"$#\" -gt 0 ]; do"
                , "  case \"$1\" in"
                , "    --out) out_sql=\"$2\"; shift 2 ;;"
                , "    --stats) stats_path=\"$2\"; shift 2 ;;"
                , "    *) shift ;;"
                , "  esac"
                , "done"
                , "case \"$in_sql\" in"
                , "  *c_slip*) printf 'SELECT '\''slip_reduced'\'';\\n' > \"$out_sql\" ;;"
                , "  *) printf 'SELECT '\''ok'\'';\\n' > \"$out_sql\" ;;"
                , "esac"
                , "printf '{\"status\":\"completed\",\"oracle_calls\":1,\"evaluation_metrics\":{\"evaluation_validation_passed\":true,\"reduced_bytes\":15,\"compression_ratio\":0.5}}\\n' > \"$stats_path\""
                ]
            )

    envList <- isolatedEnvWithBin binDir
    let runBatchArgs =
            [ "batch"
            , "--corpus-dir"
            , casesDir
            , "--dbms"
            , "postgres"
            , "--jobs"
            , "2"
            , "--repeats"
            , "1"
            , "--replays"
            , "3"
            , "--profiles"
            , "Ours-S"
            , "--run-tag"
            , "testmng"
            , "--occam-bin"
            , mockOccam
            , "--out-dir"
            , outDir
            ]

    (ec1, sum1, out1, err1) <- runHarnessJson envList runBatchArgs
    assertEqual ("managed batch failed: " <> out1 <> err1) ExitSuccess ec1
    assertRunCounts "managed" outDir sum1 3 2 0 1

    occamCalled <- readStrictText occamCalls
    assertBool "c_unstable must not invoke occam" (not ("c_unstable" `isInfixOf` occamCalled))

    -- Check slippage detected on c_slip reduced_replay_verification
    slipJob <-
        asObject "c_slip job"
            =<< readJsonFile (outDir </> "runs" </> "c_slip__Ours-S__r1" </> "job_result.json")
    slipRv <-
        maybe
            (assertFailure "missing reduced replay")
            pure
            (getObjField "reduced_replay_verification" slipJob)
    assertEqual
        "c_slip reduced replay matches_baseline False"
        (Just False)
        (getBoolField "matches_baseline" slipRv)

    -- Check docker run & rm cleanup ran for spawned containers
    dLog <- readStrictText dockerLog
    assertBool
        ("expected managed worker container run: " <> dLog)
        ("occam-eval-testmng-w" `isInfixOf` dLog)
    assertBool
        ("expected docker rm -f cleanup: " <> dLog)
        ("rm -f occam-eval-testmng-w" `isInfixOf` dLog)
    assertBool ("expected host bind mount -v passed to docker run: " <> dLog) ("-v " `isInfixOf` dLog)

    -- Re-run (--resume default): must make ZERO docker calls!
    writeFile dockerLog ""
    (ecRes, _, _, errRes) <- runHarnessJson envList runBatchArgs
    assertEqual ("managed resume failed: " <> errRes) ExitSuccess ecRes
    dLogAfter <- readStrictText dockerLog
    assertEqual "resume must make zero docker calls" "" dLogAfter

testBatchManagedRecreatePort :: IO ()
testBatchManagedRecreatePort = withTempDir "occam-batch-recreate" \td -> do
    let binDir = td </> "bin"
        dockerRunsFile = td </> "docker_runs.txt"
        pgResetCount = td </> "pg_resets.txt"
        pgLog = td </> "rec_pg.log"
        occamArgsLog = td </> "occam_args.txt"
        sqlCase = td </> "case_r.sql"
        outDir = td </> "out"
    writeFile pgLog ""
    writeFile sqlCase "SELECT 1;\n"

    writeMockPgReady binDir
    _ <-
        writeExec
            binDir
            "docker"
            ( unlines
                [ "#!/bin/sh"
                , "case \"$1\" in"
                , "  run)"
                , "    n=0"
                , "    if [ -f "
                    <> shellSingleQuote dockerRunsFile
                    <> " ]; then n=$(cat "
                    <> shellSingleQuote dockerRunsFile
                    <> "); fi"
                , "    n=$((n + 1))"
                , "    printf '%s' \"$n\" > " <> shellSingleQuote dockerRunsFile
                , "    printf 'cid_%s\\n' \"$n\""
                , "    exit 0"
                , "    ;;"
                , "  port)"
                , "    n=$(cat " <> shellSingleQuote dockerRunsFile <> " 2>/dev/null || printf '1')"
                , "    if [ \"$n\" -ge 2 ]; then"
                , "      printf '127.0.0.1:15433\\n'"
                , "    else"
                , "      printf '127.0.0.1:15432\\n'"
                , "    fi"
                , "    exit 0"
                , "    ;;"
                , "  exec)"
                , "    shift 2"
                , "    case \"$1\" in"
                , "      stat) stat -c '%s %i' " <> shellSingleQuote pgLog <> " 2>/dev/null || printf '0 1\\n' ;;"
                , "      tail) tail -c \"$3\" " <> shellSingleQuote pgLog <> " ;;"
                , "    esac"
                , "    exit 0"
                , "    ;;"
                , "esac"
                , "exit 0"
                ]
            )

    _ <-
        writeExec
            binDir
            "psql"
            ( unlines
                [ "#!/bin/sh"
                , "for arg in \"$@\"; do"
                , "  if [ \"$arg\" = \"-lqt\" ]; then"
                , "    printf ' occam_test | postgres\\n'"
                , "    exit 0"
                , "  fi"
                , "done"
                , "case \" $* \" in"
                , "  *\"-d template1\"*)"
                , "    n=0"
                , "    if [ -f "
                    <> shellSingleQuote pgResetCount
                    <> " ]; then n=$(cat "
                    <> shellSingleQuote pgResetCount
                    <> "); fi"
                , "    n=$((n + 1))"
                , "    printf '%s' \"$n\" > " <> shellSingleQuote pgResetCount
                , "    # Fail the second reset (in runJobOnWorker after initial startup) when on old port 15432"
                , "    if [ \"$n\" -eq 2 ]; then"
                , "      case \" $* \" in"
                , "        *\"-p 15432 \"*)"
                , "          printf 'connection refused\\n' >&2"
                , "          exit 1"
                , "          ;;"
                , "      esac"
                , "    fi"
                , "    exit 0"
                , "    ;;"
                , "esac"
                , "cat >/dev/null"
                , "printf '2026-01-01 00:00:00 UTC [7] TRAP: FailedAssertion(\"rec\", File: \"exec.c\", Line: 7)\\n' >> "
                    <> shellSingleQuote pgLog
                , "printf 'server closed the connection unexpectedly\\n' >&2"
                , "exit 2"
                ]
            )

    mockOccam <-
        writeExec
            binDir
            "mock-occam"
            ( unlines
                [ "#!/bin/sh"
                , "printf '%s\\n' \"$*\" > " <> shellSingleQuote occamArgsLog
                , "in_sql=\"$1\"; shift"
                , "out_sql=\"\"; stats_path=\"\""
                , "while [ \"$#\" -gt 0 ]; do"
                , "  case \"$1\" in"
                , "    --out) out_sql=\"$2\"; shift 2 ;;"
                , "    --stats) stats_path=\"$2\"; shift 2 ;;"
                , "    *) shift ;;"
                , "  esac"
                , "done"
                , "cp \"$in_sql\" \"$out_sql\""
                , "printf '{\"status\":\"completed\",\"oracle_calls\":1,\"evaluation_metrics\":{\"evaluation_validation_passed\":true,\"reduced_bytes\":9,\"compression_ratio\":1.0}}\\n' > \"$stats_path\""
                ]
            )

    envList <- isolatedEnvWithBin binDir
    (ec, _, outStr, errStr) <-
        runHarnessJson
            envList
            [ "batch"
            , "--input"
            , sqlCase
            , "--dbms"
            , "postgres"
            , "--skip-input-replay"
            , "--replays"
            , "1"
            , "--jobs"
            , "1"
            , "--repeats"
            , "1"
            , "--container-wait"
            , "5"
            , "--profiles"
            , "Ours-S"
            , "--run-tag"
            , "testrec"
            , "--occam-bin"
            , mockOccam
            , "--out-dir"
            , outDir
            ]
    assertEqual ("recreate batch failed: " <> outStr <> errStr) ExitSuccess ec
    dockerRuns <- readFile dockerRunsFile
    assertEqual "container recreated once (2 docker runs)" "2" (take 1 dockerRuns)
    occamCmd <- readFile occamArgsLog
    assertBool
        ("expected updated port 15433 in occam command args: " <> occamCmd)
        ("'--port' '15433'" `isInfixOf` occamCmd)

-- ---------------------------------------------------------------------------
-- 6. REAL occam + --local-embedded E2E tests
-- ---------------------------------------------------------------------------

writeCrashSqliteMock :: FilePath -> String -> FilePath -> Maybe FilePath -> IO FilePath
writeCrashSqliteMock dir name sleepSecs mCounterFile =
    writeExec
        dir
        name
        ( unlines
            [ "#!/bin/sh"
            , if null sleepSecs then "" else "sleep " <> sleepSecs
            , case mCounterFile of
                Nothing -> ""
                Just cf ->
                    unlines
                        [ "n=0"
                        , "if [ -f " <> shellSingleQuote cf <> " ]; then n=$(cat " <> shellSingleQuote cf <> "); fi"
                        , "n=$((n + 1))"
                        , "printf '%s' \"$n\" > " <> shellSingleQuote cf
                        ]
            , "sql=$(cat)"
            , "case \"$sql\" in"
            , "  *999*)"
            , "    printf '==999==ERROR: AddressSanitizer: SEGV on unknown address\\n    #0 0x42 in sq_crash /src/vdbe.c:42\\n' >&2"
            , "    exit 134"
            , "    ;;"
            , "esac"
            , "exit 0"
            ]
        )

testBatchLocalEmbeddedRealOccam :: IO ()
testBatchLocalEmbeddedRealOccam = withTempDir "occam-batch-e2e" \td -> do
    occamBin <- requireBinary "OCCAM_BIN" "occam"
    let binDir = td </> "bin"
        engCounter = td </> "eng.calls"
        engBin = td </> "bin" </> "mock-sqlite3"
        sqlBug = td </> "bug_e2e.sql"
        outProfiles = td </> "out_profiles"
        outExecCap = td </> "out_execcap"
        outBudget = td </> "out_budget"
        outSkipUnstable = td </> "out_skip_unstable"
    _ <- writeCrashSqliteMock binDir "mock-sqlite3" "" (Just engCounter)
    writeFile
        sqlBug
        ( unlines
            [ "CREATE TABLE t(a INT);"
            , "INSERT INTO t VALUES (1);"
            , "INSERT INTO t VALUES (2);"
            , "SELECT 999 FROM t;"
            ]
        )

    envList <- isolatedEnvWithBin binDir

    -- A) Full 6-profile real occam matrix
    (ec6, sum6, out6, err6) <-
        runHarnessJson
            envList
            [ "batch"
            , "--local-embedded"
            , "--engine-bin"
            , engBin
            , "--occam-bin"
            , occamBin
            , "--input"
            , sqlBug
            , "--dbms"
            , "sqlite"
            , "--expected-signature"
            , "AddressSanitizer"
            , "--profiles"
            , "Ours-S,ddmin-only,audit-only,Ours-SI,Ours-Full,Ours-NoDep"
            , "--repeats"
            , "1"
            , "--replays"
            , "2"
            , "--jobs"
            , "2"
            , "--out-dir"
            , outProfiles
            ]
    assertEqual ("6-profile E2E failed: " <> out6 <> err6) ExitSuccess ec6
    assertRunCounts "6-profile" outProfiles sum6 6 6 0 0
    reducedTxt <- readFile (outProfiles </> "runs" </> "bug_e2e__Ours-S__r1" </> "minimal.sql")
    assertBool
        ("expected reduced SQL to preserve crash trigger 999: " <> reducedTxt)
        ("999" `isInfixOf` reducedTxt)
    assertBool "expected aggregated_stats.json"
        =<< doesFileExist (outProfiles </> "aggregated_stats.json")

    -- B) --aggregate-only regenerates aggregated_stats.json after deletion
    removeFile (outProfiles </> "aggregated_stats.json")
    (ecAgg, _, _, errAgg) <-
        runHarnessJson envList ["batch", "--aggregate-only", "--out-dir", outProfiles]
    assertEqual ("--aggregate-only failed: " <> errAgg) ExitSuccess ecAgg
    assertBool "regenerated aggregated_stats.json"
        =<< doesFileExist (outProfiles </> "aggregated_stats.json")

    -- C) run_config_hash checkpoint invalidation on resume (--timeout changed 30 -> 25)
    callsBeforeResume <- read <$> readStrictText engCounter :: IO Int
    (ecResSame, _, _, errResSame) <-
        runHarnessJson
            envList
            [ "batch"
            , "--local-embedded"
            , "--engine-bin"
            , engBin
            , "--occam-bin"
            , occamBin
            , "--input"
            , sqlBug
            , "--dbms"
            , "sqlite"
            , "--expected-signature"
            , "AddressSanitizer"
            , "--profiles"
            , "Ours-S,ddmin-only,audit-only,Ours-SI,Ours-Full,Ours-NoDep"
            , "--repeats"
            , "1"
            , "--replays"
            , "2"
            , "--jobs"
            , "2"
            , "--out-dir"
            , outProfiles
            ]
    assertEqual ("same-hash resume failed: " <> errResSame) ExitSuccess ecResSame
    callsAfterSame <- read <$> readStrictText engCounter :: IO Int
    assertEqual "identical run_config_hash makes zero engine calls" callsBeforeResume callsAfterSame

    (ecResChanged, _, _, errResChanged) <-
        runHarnessJson
            envList
            [ "batch"
            , "--local-embedded"
            , "--engine-bin"
            , engBin
            , "--occam-bin"
            , occamBin
            , "--input"
            , sqlBug
            , "--dbms"
            , "sqlite"
            , "--expected-signature"
            , "AddressSanitizer"
            , "--profiles"
            , "Ours-S"
            , "--repeats"
            , "1"
            , "--replays"
            , "2"
            , "--timeout"
            , "25"
            , "--out-dir"
            , outProfiles
            ]
    assertEqual ("changed-hash resume failed: " <> errResChanged) ExitSuccess ecResChanged
    callsAfterChanged <- read <$> readStrictText engCounter :: IO Int
    assertBool "changed run_config_hash must re-execute job" (callsAfterChanged > callsAfterSame)

    -- D) --max-oracle-executions cap -> JobCensored
    (ecCap, sumCap, _, errCap) <-
        runHarnessJson
            envList
            [ "batch"
            , "--local-embedded"
            , "--engine-bin"
            , engBin
            , "--occam-bin"
            , occamBin
            , "--input"
            , sqlBug
            , "--dbms"
            , "sqlite"
            , "--expected-signature"
            , "AddressSanitizer"
            , "--profiles"
            , "Ours-S"
            , "--repeats"
            , "1"
            , "--replays"
            , "1"
            , "--max-oracle-executions"
            , "2"
            , "--out-dir"
            , outExecCap
            ]
    assertEqual ("max-oracle-executions failed: " <> errCap) ExitSuccess ecCap
    assertRunCounts "cap" outExecCap sumCap 1 0 1 0
    capJob <-
        asObject "cap job"
            =<< readJsonFile (outExecCap </> "runs" </> "bug_e2e__Ours-S__r1" </> "job_result.json")
    assertEqual
        "cap eval_status execution-budget"
        (Just "execution-budget")
        (getTextField "eval_status" capJob)

    -- E) --budget timeout censorship -> JobCensored
    engSlow <- writeCrashSqliteMock binDir "mock-sqlite3-slow" "0.15" Nothing
    let sqlBig = td </> "bug_big.sql"
    writeFile
        sqlBig
        ( unlines
            ( ["SELECT " <> show i <> ";" | i <- [1 .. 64 :: Int]]
                <> ["SELECT 999;"]
            )
        )
    (ecBud, sumBud, _, errBud) <-
        runHarnessJson
            envList
            [ "batch"
            , "--local-embedded"
            , "--engine-bin"
            , engSlow
            , "--occam-bin"
            , occamBin
            , "--input"
            , sqlBig
            , "--dbms"
            , "sqlite"
            , "--expected-signature"
            , "AddressSanitizer"
            , "--skip-input-replay"
            , "--profiles"
            , "ddmin-only"
            , "--repeats"
            , "1"
            , "--replays"
            , "1"
            , "--budget"
            , "1.0"
            , "--timeout"
            , "5"
            , "--out-dir"
            , outBudget
            ]
    assertEqual ("budget E2E failed: " <> errBud) ExitSuccess ecBud
    assertRunCounts "budget" outBudget sumBud 1 0 1 0
    budJob <-
        asObject "budget job"
            =<< readJsonFile (outBudget </> "runs" </> "bug_big__ddmin-only__r1" </> "job_result.json")
    assertEqual "budget eval_status timeout" (Just "timeout") (getTextField "eval_status" budJob)

    -- F) --skip-input-replay + --replays 5 on unstable input runs reduction anyway
    let flakyCount = td </> "flaky.count"
    engFlaky <-
        writeExec
            binDir
            "mock-sqlite3-flaky"
            ( unlines
                [ "#!/bin/sh"
                , "n=0"
                , "if [ -f "
                    <> shellSingleQuote flakyCount
                    <> " ]; then n=$(cat "
                    <> shellSingleQuote flakyCount
                    <> "); fi"
                , "n=$((n + 1))"
                , "printf '%s' \"$n\" > " <> shellSingleQuote flakyCount
                , "sql=$(cat)"
                , "case \"$sql\" in"
                , "  *999*)"
                , "    case \"$sql\" in"
                , "      *CREATE*) if [ \"$n\" -eq 2 ]; then exit 0; fi ;;"
                , "    esac"
                , "    printf '==1==ERROR: AddressSanitizer: heap-buffer-overflow\\n    #0 0x1 in f /a.c:1\\n' >&2"
                , "    exit 134"
                , "    ;;"
                , "esac"
                , "exit 0"
                ]
            )
    (ecSkip, _, _, errSkip) <-
        runHarnessJson
            envList
            [ "batch"
            , "--local-embedded"
            , "--engine-bin"
            , engFlaky
            , "--occam-bin"
            , occamBin
            , "--input"
            , sqlBug
            , "--dbms"
            , "sqlite"
            , "--expected-signature"
            , "AddressSanitizer"
            , "--skip-input-replay"
            , "--replays"
            , "5"
            , "--profiles"
            , "Ours-S"
            , "--repeats"
            , "1"
            , "--out-dir"
            , outSkipUnstable
            ]
    assertEqual ("skip-input-replay failed: " <> errSkip) ExitSuccess ecSkip
    qualObj <-
        asObject "skip qual"
            =<< readJsonFile (outSkipUnstable </> "qualified" </> "bug_e2e" </> "input_replay.json")
    assertEqual "skipped True" (Just True) (getBoolField "skipped" qualObj)
    assertEqual "qualified True" (Just True) (getBoolField "qualified" qualObj)
    skipJob <-
        asObject "skip job"
            =<< readJsonFile (outSkipUnstable </> "runs" </> "bug_e2e__Ours-S__r1" </> "job_result.json")
    assertEqual
        "ran_occam True despite skip-input-replay"
        (Just True)
        (getBoolField "ran_occam" skipJob)
    skipRv <-
        maybe
            (assertFailure "missing reduced replay")
            pure
            (getObjField "reduced_replay_verification" skipJob)
    assertEqual "reduced replay honors --replays 5" (Just 5) (getIntField "runs_required" skipRv)

testBatchLocalEmbeddedSigintResume :: IO ()
testBatchLocalEmbeddedSigintResume = withTempDir "occam-batch-sigint" \td -> do
    occamBin <- requireBinary "OCCAM_BIN" "occam"
    harnessBin <- requireBinary "OCCAM_HARNESS_BIN" "occam-harness"
    let binDir = td </> "bin"
        caseBStarted = td </> "case_b.started"
        sqlA = td </> "case_a.sql"
        sqlB = td </> "case_b.sql"
        outDir = td </> "out_sigint"
    writeFile sqlA "CREATE TABLE a(x INT);\nSELECT 999;\n"
    writeFile sqlB "CREATE TABLE b(y INT);\nSELECT 888;\n"

    engBin <-
        writeExec
            binDir
            "mock-sqlite3-sig"
            ( unlines
                [ "#!/bin/sh"
                , "sql=$(cat)"
                , "case \"$sql\" in"
                , "  *888*)"
                , "    printf 'started\\n' > " <> shellSingleQuote caseBStarted
                , "    sleep 2"
                , "    printf '==888==ERROR: AddressSanitizer: SEGV\\n    #0 0x8 in f /b.c:1\\n' >&2"
                , "    exit 134"
                , "    ;;"
                , "  *999*)"
                , "    printf '==999==ERROR: AddressSanitizer: SEGV\\n    #0 0x9 in g /a.c:1\\n' >&2"
                , "    exit 134"
                , "    ;;"
                , "esac"
                , "exit 0"
                ]
            )

    envList <- isolatedEnvWithBin binDir
    let batchArgs =
            [ "batch"
            , "--local-embedded"
            , "--engine-bin"
            , engBin
            , "--occam-bin"
            , occamBin
            , "--inputs"
            , sqlA
            , sqlB
            , "--dbms"
            , "sqlite"
            , "--expected-signature"
            , "AddressSanitizer"
            , "--skip-input-replay"
            , "--replays"
            , "1"
            , "--jobs"
            , "1"
            , "--repeats"
            , "1"
            , "--profiles"
            , "Ours-S"
            , "--out-dir"
            , outDir
            ]

    (_, Just hOut, Just hErr, ph) <-
        createProcess
            (proc harnessBin batchArgs)
                { env = Just envList
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    startedB <- waitForFile caseBStarted 15_000
    unless startedB $ do
        _ <- waitForProcess ph
        assertFailure "case_b never started before timeout"
    mPid <- getPid ph
    case mPid of
        Nothing -> assertFailure "missing child pid"
        Just pid -> signalProcess sigINT pid
    ecSig <- waitForProcess ph
    outBytesSig <- BS.hGetContents hOut
    _ <- BS.hGetContents hErr
    assertEqual "SIGINT exits with 130" (ExitFailure 130) ecSig
    case decodeStrict' outBytesSig of
        Just (Object sigObj) ->
            assertEqual "SIGINT stdout interrupted True" (Just True) (getBoolField "interrupted" sigObj)
        _ -> pure ()

    (ovPartial, compPartial, _, _) <- readAggregatedStats outDir
    assertEqual "1 job in partial overview" (Just 1) (getIntField "total_job_runs" ovPartial)
    assertEqual "1 completed job saved before SIGINT" 1 compPartial

    -- Resume finishes cleanly to 2/2 completed
    removeFile caseBStarted
    engFast <-
        writeExec binDir "mock-sqlite3-sig" $
            unlines
                [ "#!/bin/sh"
                , "sql=$(cat)"
                , "case \"$sql\" in"
                , "  *888*|*999*) printf '==1==ERROR: AddressSanitizer: SEGV\\n    #0 0x1 in f /a.c:1\\n' >&2; exit 134 ;;"
                , "esac"
                , "exit 0"
                ]
    let resumeArgs =
            map (\a -> if a == engBin then engFast else a) batchArgs
    (ecRes, sumRes, outRes, errRes) <- runHarnessJson envList resumeArgs
    assertEqual ("SIGINT resume failed: " <> outRes <> errRes) ExitSuccess ecRes
    assertRunCounts "sigint resume" outDir sumRes 2 2 0 0

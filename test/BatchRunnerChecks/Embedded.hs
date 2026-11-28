-- | Embedded SQLite batch end-to-end and signal/resume checks.
module BatchRunnerChecks.Embedded (
    testBatchLocalEmbeddedRealOccam,
    testBatchLocalEmbeddedSigintResume,
) where

import Control.Monad (unless)
import Data.Aeson (Value (Object), decodeStrict')
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Signals (sigINT, signalProcess)
import System.Process (
    CreateProcess (..),
    StdStream (CreatePipe),
    createProcess,
    getPid,
    proc,
    waitForProcess,
 )
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)

import BatchRunnerChecks.Support

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

-- | Managed-worker lifecycle, resume, and port-refresh checks.
module BatchRunnerChecks.Managed (
    testBatchManagedWorkersAndResume,
    testBatchManagedRecreatePort,
) where

import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)

import BatchRunnerChecks.Support

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

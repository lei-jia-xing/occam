-- | Checks for batches using externally managed containers.
module BatchRunnerChecks.External (testBatchExternalContainers) where

import Data.List (isInfixOf)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Tasty.HUnit (assertBool, assertEqual)

import BatchRunnerChecks.Support

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

    dockerCalls <- lines <$> readStrictText dockerLog
    assertBool ("unexpected docker run in external mode: " <> unlines dockerCalls) $
        not (any (\l -> "run " `isInfixOf` (" " <> l) && take 4 l == "run ") dockerCalls)
    assertBool ("unexpected docker rm in external mode: " <> unlines dockerCalls) $
        not (any (\l -> take 3 l == "rm ") dockerCalls)
    assertBool ("expected docker port resolution for ext_pg1: " <> unlines dockerCalls) $
        any ("port ext_pg1" `isInfixOf`) dockerCalls

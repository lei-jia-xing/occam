-- | Collect and verification workflow checks.
module BatchRunnerChecks.Collect (testCollectSources, testCollectVerify) where

import Control.Monad (forM_)
import Data.Aeson (Value (Null))
import Data.Aeson.KeyMap qualified as KM
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)

import BatchRunnerChecks.Support

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

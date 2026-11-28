-- | Batch dry-run planning and argument validation checks.
module BatchRunnerChecks.Plan (testBatchDryRunAndValidation) where

import Control.Monad (forM_)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Tasty.HUnit (assertBool, assertEqual)

import BatchRunnerChecks.Support

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

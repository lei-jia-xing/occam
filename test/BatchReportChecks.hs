module BatchReportChecks (tests) where

import Control.Exception.Safe (finally)
import Data.Aeson (Value (..), decodeStrict', encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Benchmark.BatchReport (
    AggregatedReport (..),
    BatchOverview (..),
    DecisionSummary (..),
    GroupSummary (..),
    MetricSummary (..),
    MetricsSubset (..),
    SignatureProfileStratified (..),
    aggregateOutDir,
    metricSummary,
    renderSummaryMarkdown,
    summarizeDecisionLog,
    summarizeDecisionLogBytes,
 )
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationPoint (..),
    EvaluationStatus (..),
    MinimalityClaim (..),
    Stats (..),
 )
import Occam.Harness.Cli (HarnessCommand (..), parseHarnessArgs)
import Occam.Harness.Config (
    HarnessConfig (..),
    SquirrelRenderOptions (..),
    loadHarnessConfig,
    lookupSquirrelSpec,
    renderSquirrelYaml,
    resolveHarnessConfigPath,
    squirrelFieldText,
 )
import Occam.Harness.SqlancerSanitize (
    SqlancerMeta (..),
    SqlancerSanitizeResult (..),
    sanitizeSqlancerLog,
 )
import Occam.Metrics (
    FamilyCounters (..),
    FamilyTag (..),
    MetricPolicy (..),
    MetricsSnapshot (..),
    ParseKind (..),
    emptyFamilyCounters,
 )
import Options.Applicative (ParserResult (..))
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    findExecutable,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Batch Reporting, Harness Config & Sanitization"
        [ testCase "summarizeDecisionLogBytes counts outcomes and deduplicates signatures in order" do
            let raw =
                    BSC.unlines
                        [ ""
                        , "not valid json"
                        , "123"
                        , "{\"verdict\":\"accepted\",\"classification\":\"confirmed_fault\",\"slippage\":{\"slipped\":false},\"crash_signature\":{\"id\":\"sig-b\"}}"
                        , "{\"verdict\":\"rejected\",\"classification\":\"healthy_no_fault\",\"slippage\":{\"slipped\":true}}"
                        , "{\"verdict\":\"accepted\",\"classification\":\"confirmed_fault\",\"crash_signature\":{\"id\":\"sig-a\"}}"
                        , "{\"verdict\":\"rejected\",\"classification\":\"missing_evidence\",\"crash_signature\":{\"id\":\"sig-b\"}}"
                        ]
                summary = summarizeDecisionLogBytes raw
            assertEqual "total_calls" 4 (dsTotalCalls summary)
            assertEqual "accepted_calls" 2 (dsAcceptedCalls summary)
            assertEqual "confirmed_faults" 2 (dsConfirmedFaults summary)
            assertEqual "healthy_no_fault" 1 (dsHealthyNoFault summary)
            assertEqual "missing_evidence" 1 (dsMissingEvidence summary)
            assertEqual "slippage_events" 1 (dsSlippageEvents summary)
            assertEqual "unique_fault_signatures" ["sig-b", "sig-a"] (dsUniqueFaultSignatures summary)
        , testCase "summarizeDecisionLog handles missing path" do
            summary <- summarizeDecisionLog "/nonexistent/oracle_decisions.jsonl"
            assertEqual "empty summary" (DecisionSummary 0 0 0 0 0 0 []) summary
        , testCase "metricSummary computes median/mean/min/max for odd and even counts" do
            assertEqual "empty list" Nothing (metricSummary [])
            assertEqual
                "odd list"
                (Just (MetricSummary 3 20.0 20.0 10.0 30.0))
                (metricSummary [30.0, 10.0, 20.0])
            assertEqual
                "even list"
                (Just (MetricSummary 4 25.0 30.0 10.0 60.0))
                (metricSummary [60.0, 10.0, 30.0, 20.0])
        , testCase "aggregateOutDir computes stratified metrics, full Stats decoding, and Markdown" do
            withTempDir "occam-batch-report" \outDir -> do
                let writeJson p val = do
                        createDirectoryIfMissing True (takeDirectory p)
                        BSL.writeFile p (encode val)
                writeJson (outDir </> "qualified" </> "c1" </> "input_replay.json") $
                    object
                        [ "case_id" .= ("c1" :: T.Text)
                        , "dbms" .= ("sqlite" :: T.Text)
                        , "qualified" .= True
                        , "baseline_signature_id" .= ("sig-A" :: T.Text)
                        ]
                writeJson (outDir </> "qualified" </> "c2" </> "input_replay.json") $
                    object
                        [ "case_id" .= ("c2" :: T.Text)
                        , "dbms" .= ("sqlite" :: T.Text)
                        , "qualified" .= True
                        , "baseline_signature_id" .= ("sig-A" :: T.Text)
                        ]
                writeJson (outDir </> "qualified" </> "c3" </> "input_replay.json") $
                    object
                        [ "case_id" .= ("c3" :: T.Text)
                        , "dbms" .= ("postgres" :: T.Text)
                        , "qualified" .= True
                        , "baseline_signature_id" .= ("sig-B" :: T.Text)
                        ]
                writeJson (outDir </> "qualified" </> "c_bad" </> "input_replay.json") $
                    object
                        [ "case_id" .= ("c_bad" :: T.Text)
                        , "dbms" .= ("postgres" :: T.Text)
                        , "qualified" .= False
                        ]

                let statsDir = outDir </> "runs"
                    mkFullStats path st redB ratio attempts elapsed = do
                        let snap =
                                MetricsSnapshot
                                    { snapshotPolicy = MetricPolicy "full" "off" 1
                                    , snapshotFamilies =
                                        Map.singleton TagStatement (emptyFamilyCounters {candidateBuildMicros = 40})
                                    , snapshotGeneratedSkipped = Map.empty
                                    , snapshotParseFfiMicros = Map.singleton ParseStatementOnly 60
                                    , snapshotParseDecodeMicros = Map.singleton ParseStatementOnly 15
                                    }
                            budget =
                                EvaluationConfig
                                    { evaluationCacheEnabled = True
                                    , evaluationJobs = 2
                                    , evaluationTotalTimeoutMicros = Nothing
                                    , evaluationMaxOracleExecutions = Nothing
                                    , evaluationPreflightMicros = 0
                                    , evaluationPreflightAttempts = 0
                                    , evaluationPreflightResetMicros = Nothing
                                    , evaluationPreflightOracleMicros = Nothing
                                    , evaluationInitialStatements = Just 10
                                    }
                            em =
                                EvaluationMetrics
                                    { evaluationStatus = st
                                    , evaluationFailure = Nothing
                                    , evaluationCacheUsed = True
                                    , evaluationBudget = budget
                                    , evaluationTotalSeconds = elapsed
                                    , evaluationReductionSeconds = elapsed
                                    , evaluationOracleSeconds = 1.0
                                    , evaluationResetSeconds = 0.2
                                    , evaluationPreflightResetSeconds = Nothing
                                    , evaluationPreflightOracleSeconds = Nothing
                                    , evaluationOracleAttempts = attempts
                                    , evaluationDatabaseObservations = attempts
                                    , evaluationCompletedOracleProcesses = attempts
                                    , evaluationAcceptedObservations = 2
                                    , evaluationRejectedObservations = max 0 (attempts - 2)
                                    , evaluationTimedOutObservations = 0
                                    , evaluationResetFailures = 0
                                    , evaluationOracleErrors = 0
                                    , evaluationCancelledObservations = 0
                                    , evaluationOracleOutcomes = Map.empty
                                    , evaluationCacheHits = 1
                                    , evaluationCacheMisses = 1
                                    , evaluationCacheJoins = 0
                                    , evaluationValidationSeconds = 0.1
                                    , evaluationValidationOracleSeconds = 0.1
                                    , evaluationValidationResetSeconds = 0.0
                                    , evaluationValidationObservations = 1
                                    , evaluationValidationDatabaseObservations = 1
                                    , evaluationValidationErrors = 0
                                    , evaluationValidationOutcomes = Map.empty
                                    , evaluationValidationPassed = st == EvaluationCompleted
                                    , evaluationAuditSeconds = 0.05
                                    , evaluationAuditChecks = 1
                                    , evaluationAuditCompleted = True
                                    , evaluationAuditSkipped = False
                                    , evaluationMinimalityClaim =
                                        if st == EvaluationCompleted
                                            then StatementDeletionOneMinimal
                                            else MinimalityNotEstablished
                                    , evaluationCurve =
                                        [ EvaluationPoint 0.5 1 200 (Just 10) False True
                                        , EvaluationPoint 1.0 2 redB (Just 2) False True
                                        ]
                                    , evaluationStages = []
                                    , evaluationCostAttribution = Just snap
                                    }
                            stats =
                                Stats
                                    { initial_bytes = 200
                                    , reduced_bytes = redB
                                    , reduction_ratio = ratio
                                    , initial_statements = 10
                                    , reduced_statements = 2
                                    , oracle_executions = attempts
                                    , oracle_attempts = attempts
                                    , initial_oracle_observations = 1
                                    , cache_hits = 1
                                    , cache_misses = 1
                                    , cache_joins = 0
                                    , cache_hit_ratio = 0.5
                                    , cancelled_observations = 0
                                    , status = st
                                    , reduction_settings = object []
                                    , evaluation_budget = budget
                                    , evaluation_metrics = em
                                    , initial_oracle_seconds = 0.1
                                    , elapsed_seconds = elapsed
                                    }
                        writeJson path stats

                mkFullStats (statsDir </> "j1" </> "stats.json") EvaluationCompleted 60 70.0 4 2.0
                mkFullStats (statsDir </> "j2" </> "stats.json") EvaluationCompleted 40 80.0 6 3.0
                mkFullStats (statsDir </> "j3" </> "stats.json") EvaluationCompleted 100 50.0 8 4.0
                mkFullStats (statsDir </> "j4" </> "stats.json") EvaluationTimeout 180 10.0 10 10.0

                let writeJob :: String -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> FilePath -> IO ()
                    writeJob jid cid dbms prof sig stateTxt statsFile =
                        writeJson (statsDir </> jid </> "job_result.json") $
                            object
                                [ "job_id" .= jid
                                , "case_id" .= cid
                                , "dbms" .= dbms
                                , "dialect" .= dbms
                                , "profile" .= prof
                                , "repeat" .= (1 :: Int)
                                , "state" .= stateTxt
                                , "eval_status" .= (Nothing :: Maybe T.Text)
                                , "initial_bytes" .= (200 :: Int)
                                , "input_qualified" .= True
                                , "baseline_signature_id" .= sig
                                , "stats_path" .= statsFile
                                , "reduced_replay_verification"
                                    .= object
                                        [ "runs_required" .= (5 :: Int)
                                        , "runs_passed" .= (5 :: Int)
                                        , "all_passed" .= True
                                        , "consistent_signature" .= True
                                        , "matches_baseline" .= True
                                        , "slippage_count" .= (0 :: Int)
                                        , "observed_signature_ids" .= [sig]
                                        ]
                                ]
                writeJob "j1" "c1" "sqlite" "Ours-S" "sig-A" "completed" (statsDir </> "j1" </> "stats.json")
                writeJob "j2" "c2" "sqlite" "Ours-S" "sig-A" "completed" (statsDir </> "j2" </> "stats.json")
                writeJob "j3" "c3" "postgres" "Ours-S" "sig-B" "completed" (statsDir </> "j3" </> "stats.json")
                writeJob "j4" "c3" "postgres" "Ours-S" "sig-B" "censored" (statsDir </> "j4" </> "stats.json")

                agg <- aggregateOutDir outDir
                assertEqual "qualified evaluated" 4 (ovQualifiedCasesEvaluated (aggOverview agg))
                assertEqual "stable inputs" 3 (ovInputStableCases (aggOverview agg))
                assertEqual "unique qualified sigs" 2 (ovUniqueQualifiedCrashSignatures (aggOverview agg))
                case Map.lookup "Ours-S" (aggByProfile agg) of
                    Nothing -> assertFailure "Missing Ours-S profile summary"
                    Just prof -> do
                        assertEqual "completed runs" 3 (gsCompletedRuns prof)
                        assertEqual "censored runs" 1 (gsCensoredRuns prof)
                        assertEqual "completed count" 3 (subCount (gsCompletedOnly prof))
                        assertEqual
                            "completed median reduced_bytes"
                            (Just 60.0)
                            (msMedian <$> subReducedBytes (gsCompletedOnly prof))
                        assertEqual
                            "completed median engine_overhead_us"
                            (Just 115.0)
                            (msMedian <$> subEngineOverheadUs (gsCompletedOnly prof))
                        assertEqual
                            "checkpoints count"
                            4
                            (subCount (gsAllWithCheckpoints prof))
                        assertEqual
                            "checkpoints max reduced_bytes"
                            (Just 180.0)
                            (msMax <$> subReducedBytes (gsAllWithCheckpoints prof))
                case Map.lookup "Ours-S" (aggSignatureStratifiedByProfile agg) of
                    Nothing -> assertFailure "Missing signature-stratified Ours-S"
                    Just strat -> do
                        assertEqual "signatures observed" 2 (spsSignaturesObserved strat)
                        assertEqual "signatures completed" 2 (spsSignaturesWithCompletedRuns strat)
                        assertEqual
                            "macro-median reduced_bytes"
                            (Just 75.0)
                            (msMedian <$> spsCompletedSignatureMedianReducedBytes strat)

                let md = renderSummaryMarkdown agg
                assertBool
                    "Markdown heading present"
                    ("# Occam Horizontal Batch Evaluation Summary" `T.isInfixOf` md)
                assertBool "Markdown Ours-S row present" ("| `Ours-S` | 3 / 4 | 1 |" `T.isInfixOf` md)
                aggExists <- doesFileExist (outDir </> "aggregated_stats.json")
                mdExists <- doesFileExist (outDir </> "summary.md")
                assertBool "aggregated_stats.json created" aggExists
                assertBool "summary.md created" mdExists
        , testCase "aggregateOutDir decodes minimal partial curve points cleanly" do
            withTempDir "occam-partial-curve" \outDir -> do
                let rdir = outDir </> "runs" </> "j_partial"
                createDirectoryIfMissing True rdir
                BSL.writeFile
                    (rdir </> "stats.json")
                    ( encode $
                        object
                            [ "status" .= ("completed" :: T.Text)
                            , "initial_bytes" .= (100 :: Int)
                            , "reduced_bytes" .= (50 :: Int)
                            , "reduction_ratio" .= (50.0 :: Double)
                            , "oracle_attempts" .= (4 :: Int)
                            , "evaluation_metrics"
                                .= object
                                    [ "evaluation_status" .= ("completed" :: T.Text)
                                    , "evaluation_validation_passed" .= True
                                    , "evaluation_curve"
                                        .= [ object
                                                [ "point_call_index" .= (1 :: Int)
                                                , "point_candidate_bytes" .= (50 :: Int)
                                                , "point_accepted" .= True
                                                ]
                                           ]
                                    ]
                            ]
                    )
                BSL.writeFile
                    (rdir </> "job_result.json")
                    ( encode $
                        object
                            [ "job_id" .= ("j_partial" :: T.Text)
                            , "case_id" .= ("c_p" :: T.Text)
                            , "dbms" .= ("postgres" :: T.Text)
                            , "profile" .= ("Ours-S" :: T.Text)
                            , "repeat" .= (1 :: Int)
                            , "state" .= ("completed" :: T.Text)
                            , "stats_path" .= (rdir </> "stats.json")
                            ]
                    )
                agg <- aggregateOutDir outDir
                case Map.lookup "Ours-S" (aggByProfile agg) of
                    Nothing -> assertFailure "Missing Ours-S profile"
                    Just prof -> do
                        assertEqual "completed runs" 1 (gsCompletedRuns prof)
                        assertEqual
                            "AUC with partial point"
                            (Just 0.5)
                            (msMedian <$> subAttemptAuc (gsCompletedOnly prof))
        , testCase "Harness.Config decodes real config/generated/harness.json and renders SQUIRREL YAML" do
            cfgPath <- resolveHarnessConfigPath Nothing
            exists <- doesFileExist cfgPath
            assertBool ("harness.json missing at " ++ cfgPath) exists
            eCfg <- loadHarnessConfig cfgPath
            hc <- either (\e -> assertFailure e >> error "unreachable") pure eCfg
            assertBool "dbms non-empty" (not (null (hcDbms hc)))
            assertBool "profiles non-empty" (not (null (hcProfiles hc)))
            assertBool "stage2_thesis plan present" (Map.member "stage2_thesis" (hcPlans hc))
            let mariadbSpec = lookupSquirrelSpec hc "mariadb"
            assertEqual
                "mariadb target_bin_rel"
                (Just "build/db_driver")
                (mariadbSpec >>= (`squirrelFieldText` "target_bin_rel"))
            let opts =
                    SquirrelRenderOptions
                        { sroDbms = "mariadb"
                        , sroSquirrelHome = "/opt/squirrel"
                        , sroHost = "127.0.0.1"
                        , sroPort = 3306
                        , sroUser = "root"
                        , sroPassword = "p'w"
                        , sroSocket = "/tmp/mysql.sock"
                        , sroDbName = "occam_test"
                        , sroServerCmd = "/usr/sbin/mariadbd"
                        }
            case renderSquirrelYaml hc opts of
                Left err -> assertFailure (T.unpack err)
                Right yamlTxt -> do
                    assertBool "mariadb renders db: mysql" ("db: mysql\n" `T.isInfixOf` yamlTxt)
                    assertBool "single quotes escaped" ("passwd: 'p''w'\n" `T.isInfixOf` yamlTxt)
                    assertBool "required list formatted" ("required: [\"init_lib\", " `T.isInfixOf` yamlTxt)
                    assertBool
                        "init_lib joined"
                        ("init_lib: '/opt/squirrel/data/fuzz_root/mysql_init_lib'" `T.isInfixOf` yamlTxt)
            assertBool
                "unknown squirrel DBMS fails"
                (either (const True) (const False) (renderSquirrelYaml hc opts {sroDbms = "oracle"}))
        , testCase "sanitizeSqlancerLog strips stacktraces, psql \\c, and DB lifecycle statements" do
            let raw =
                    BSC.unlines
                        [ "-- Time: 2026-01-01 00:00:00"
                        , "-- Database: testdb1"
                        , "-- Database version: 17.2"
                        , "-- seed value: 42"
                        , "java.lang.AssertionError: boom"
                        , "-- sqlancer.IgnoreMeException: noisy"
                        , "Exception in thread \"main\" java.lang.RuntimeException: fail"
                        , "\tat app//com.example.Main.run(Main.java:10)"
                        , "Caused by: java.io.IOException: io"
                        , "Suppressed: java.lang.IllegalStateException: sup"
                        , "\t... 14 more"
                        , "DROP DATABASE IF EXISTS testdb1;"
                        , "DROP DATABASE FORCE IF EXISTS database0"
                        , "CREATE DATABASE IF NOT EXISTS `database0` CHARACTER SET utf8;"
                        , "CREATE DATABASE database12;"
                        , "\\c database0"
                        , "USE \"testdb1\";"
                        , "CREATE TABLE t1(c INT);"
                        , "SELECT * FROM t1;"
                        ]
                res = sanitizeSqlancerLog raw
                sqlTxt = TE.decodeUtf8 (sanitizedSql res)
                meta = sanitizedMeta res
            assertEqual "meta time" (Just "2026-01-01 00:00:00") (metaTime meta)
            assertEqual "meta db" (Just "testdb1") (metaDatabase meta)
            assertEqual "meta version" (Just "17.2") (metaDatabaseVersion meta)
            assertEqual "meta seed" (Just "42") (metaSeedValue meta)
            assertBool "preserves comments" ("-- Database: testdb1" `T.isInfixOf` sqlTxt)
            assertBool "preserves SQL" ("CREATE TABLE t1(c INT);\nSELECT * FROM t1;\n" `T.isInfixOf` sqlTxt)
            assertBool "strips java frame" (not ("AssertionError" `T.isInfixOf` sqlTxt))
            assertBool "strips module frame" (not ("Main.java" `T.isInfixOf` sqlTxt))
            assertBool "strips psql \\c" (not ("\\c database0" `T.isInfixOf` sqlTxt))
            assertBool "strips DROP DATABASE" (not ("DROP DATABASE" `T.isInfixOf` sqlTxt))
            assertBool "strips CREATE DATABASE" (not ("CREATE DATABASE" `T.isInfixOf` sqlTxt))
            assertBool "strips USE" (not ("USE \"testdb1\"" `T.isInfixOf` sqlTxt))
        , testCase "occam-harness CLI parser and binary subcommands execute cleanly" do
            case parseHarnessArgs ["check-config", "config/generated/harness.json"] of
                Success (CmdCheckConfig (Just "config/generated/harness.json")) -> pure ()
                other -> assertFailure ("Unexpected parse result: " ++ show other)
            mHarnessBin <- resolveHarnessBinary
            cfgPath <- resolveHarnessConfigPath Nothing
            (code1, out1, err1) <- runHarnessCLI mHarnessBin ["check-config", cfgPath] ""
            assertEqual ("check-config failed: " ++ err1) ExitSuccess code1
            assertBool "check-config output" ("Valid harness config:" `isInfixOf` out1)

            (code2, out2, err2) <-
                runHarnessCLI
                    mHarnessBin
                    [ "render-squirrel-yaml"
                    , "--config"
                    , cfgPath
                    , "--dbms"
                    , "sqlite"
                    , "--squirrel-home"
                    , "/opt/sq"
                    ]
                    ""
            assertEqual ("render-squirrel-yaml failed: " ++ err2) ExitSuccess code2
            assertBool "sqlite pragma rendered" ("pragma: '/opt/sq/data/fuzz_root/pragma'" `isInfixOf` out2)

            (code3, out3, err3) <-
                runHarnessCLI
                    mHarnessBin
                    ["sanitize-sqlancer", "--json"]
                    "-- Database: db9\nCREATE DATABASE db9;\nSELECT 1;\n"
            assertEqual ("sanitize-sqlancer failed: " ++ err3) ExitSuccess code3
            case decodeStrict' (BSC.pack out3) :: Maybe Value of
                Just (Object obj) -> case KM.lookup "sql" obj of
                    Just (String s) -> assertEqual "sanitized JSON sql" "-- Database: db9\nSELECT 1;\n" s
                    other -> assertFailure ("Missing sql field: " ++ show other)
                _ -> assertFailure ("Invalid sanitize JSON output: " ++ out3)
        ]

resolveHarnessBinary :: IO (Maybe FilePath)
resolveHarnessBinary = do
    mEnv <- lookupEnv "OCCAM_HARNESS_BIN"
    case mEnv of
        Just path | not (null path) -> pure (Just path)
        _ -> do
            mOccam <- lookupEnv "OCCAM_BIN"
            exePath <- getExecutablePath
            let occamSibs = case mOccam of
                    Just occPath
                        | not (null occPath) ->
                            [takeDirectory (takeDirectory occPath) </> "occam-harness" </> "occam-harness"]
                    _ -> []
                exeDir = takeDirectory exePath
                exeSibs =
                    [ takeDirectory (takeDirectory exeDir) </> "occam-harness" </> "occam-harness"
                    , iterate takeDirectory exeDir !! 4
                        </> "x"
                        </> "occam-harness"
                        </> "build"
                        </> "occam-harness"
                        </> "occam-harness"
                    , iterate takeDirectory exeDir !! 5
                        </> "x"
                        </> "occam-harness"
                        </> "opt"
                        </> "build"
                        </> "occam-harness"
                        </> "occam-harness"
                    ]
                candidates = occamSibs <> exeSibs <> ["dist/build/occam-harness/occam-harness"]
            firstExisting candidates >>= \case
                Just found -> pure (Just found)
                Nothing -> findExecutable "occam-harness"
    where
        firstExisting [] = pure Nothing
        firstExisting (c : cs) = do
            ex <- doesFileExist c
            if ex then pure (Just c) else firstExisting cs

runHarnessCLI :: Maybe FilePath -> [String] -> String -> IO (ExitCode, String, String)
runHarnessCLI Nothing cliArgs stdinData =
    readProcessWithExitCode "cabal" (["run", "-v0", "occam-harness", "--"] ++ cliArgs) stdinData
runHarnessCLI (Just bin) cliArgs stdinData =
    readProcessWithExitCode bin cliArgs stdinData

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    sysTmp <- getTemporaryDirectory
    let dir = sysTmp </> prefix
    createDirectoryIfMissing True dir
    action dir `finally` removeDirectoryRecursive dir

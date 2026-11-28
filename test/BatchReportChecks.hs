{-# LANGUAGE OverloadedRecordDot #-}

module BatchReportChecks (tests) where

import Control.Exception.Safe (finally)
import Control.Monad (forM_)
import Data.Aeson (Value (..), decode, decodeStrict', encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
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
import Occam.Harness.BarrierCensus (barrierCensus)
import Occam.Harness.BatchInputs (BatchInputOptions (..), BatchInputSource (..), loadBatchInputs)
import Occam.Harness.Cli (HarnessCommand (..), parseHarnessArgs)
import Occam.Harness.Collect (ArtifactSource (..), collectArtifacts, defaultCollectOptions)
import Occam.Harness.Config (
    DbmsSpec (..),
    ExperimentPlan (..),
    HarnessConfig (..),
    OracleDefaults (..),
    ReductionProfile (..),
    SquirrelRenderOptions (..),
    loadExperimentPlan,
    loadHarnessConfig,
    lookupDbmsSpec,
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
        , testCase "batch inputs resolve image precedence and keep normalized case metadata" do
            withTempDir "occam-batch-inputs-check" \outDir -> do
                config <- either assertFailure pure =<< loadHarnessConfig "config/generated/harness.json"
                forM_ ["a", "b", "c", "d"] \name -> BSC.writeFile (outDir </> name <> ".sql") "SELECT 1;"
                let entry cid dbms version =
                        object
                            [ "id" .= (cid :: T.Text)
                            , "dbms" .= (dbms :: T.Text)
                            , "version" .= (version :: T.Text)
                            , "file" .= (T.unpack cid <> ".sql")
                            , "image" .= ("metadata-image" :: T.Text)
                            ]
                    inputs = [entry "a" "sqlite3" "1", entry "b" "sqlite" "1", entry "c" "sqlite" "2", entry "d" "duckdb" "1"]
                    source = outDir </> "manifest.json"
                    options =
                        BatchInputOptions
                            (ManifestFile source)
                            ""
                            ""
                            ""
                            ""
                            ["sqlite=dbms-image", "sqlite@1=version-image", "case:a=case-image"]
                BSL.writeFile source (encode (object ["cases" .= inputs]))
                report <- either assertFailure (pure . toJSON) =<< loadBatchInputs config options
                case report of
                    Array rows ->
                        assertEqual
                            "case ordering, normalized DBMS, and image overrides"
                            [ [String "d", String "duckdb", String "metadata-image"]
                            , [String "a", String "sqlite", String "case-image"]
                            , [String "c", String "sqlite", String "dbms-image"]
                            , [String "b", String "sqlite", String "version-image"]
                            ]
                            [ [value | key <- ["id", "dbms", "image"], Just value <- [KM.lookup key row]]
                            | Object row <- toList rows
                            ]
                    other -> assertFailure ("Unexpected batch inputs: " <> show other)
        , testCase "raw batch inputs preserve dotfile stems and duplicate suffixes" do
            withTempDir "occam-batch-raw-inputs-check" \outDir -> do
                config <- either assertFailure pure =<< loadHarnessConfig "config/generated/harness.json"
                let hidden = outDir </> ".sql"
                    throughParent = outDir </> "absent" </> ".." </> ".sql"
                    options = BatchInputOptions (SqlFiles (hidden :| [throughParent])) "sqlite" "" "" "" []
                BSC.writeFile hidden "SELECT 1;"
                report <- either assertFailure (pure . toJSON) =<< loadBatchInputs config options
                case report of
                    Array rows ->
                        assertEqual
                            "legacy paths and job ID stems"
                            [(Just (String "sql"), Just (toJSON hidden)), (Just (String "sql_2"), Just (toJSON hidden))]
                            [(KM.lookup "id" row, KM.lookup "file" row) | Object row <- toList rows]
                    other -> assertFailure ("Unexpected raw batch inputs: " <> show other)
        , testCase "collect publishes batch manifests without changing earlier SQL snapshots" do
            withTempDir "occam-collect-check" \inputDir -> do
                config <- either assertFailure pure =<< loadHarnessConfig "config/generated/harness.json"
                let output = inputDir </> "collected"
                    first = inputDir </> "a.sql"
                    second = inputDir </> "b.sql"
                    options = defaultCollectOptions Raw "sqlite3" inputDir output
                    manifestPath = output </> "manifest.json"
                    batchOptions = BatchInputOptions (ManifestFile manifestPath) "" "" "" "" []
                    importCase count = do
                        summary <- either assertFailure pure =<< collectArtifacts config options
                        case summary of
                            Object fields -> do
                                assertEqual
                                    "input discovery count"
                                    (Just (Number count))
                                    (KM.lookup "raw_candidates_discovered" fields)
                                assertEqual "manifest case count" (Just (Number 1)) (KM.lookup "manifest_case_count" fields)
                            other -> assertFailure ("Unexpected collection summary: " <> show other)
                        cases <- either assertFailure pure =<< loadBatchInputs config batchOptions
                        case map toJSON cases of
                            [Object item] -> do
                                assertEqual "case ID" (Just (String "a")) (KM.lookup "id" item)
                                assertEqual "normalized DBMS" (Just (String "sqlite")) (KM.lookup "dbms" item)
                                case KM.lookup "file" item of
                                    Just (String path) -> pure (T.unpack path)
                                    other -> assertFailure ("Missing case path: " <> show other)
                            other -> assertFailure ("Unexpected batch inventory: " <> show other)
                BSC.writeFile first "SELECT 1;"
                BSC.writeFile second "SELECT 1;"
                old <- importCase 2
                BSC.writeFile first "SELECT 2;"
                BSC.writeFile second " \n"
                latest <- importCase 1
                assertBool "new SQL receives a different path" (old /= latest)
                assertEqual "previous SQL stays readable" "SELECT 1;" =<< BSC.readFile old
                assertEqual "new manifest points to changed SQL" "SELECT 2;" =<< BSC.readFile latest
        , testCase "experiment plan loading preserves named plans and fills nested JSON defaults" do
            withTempDir "occam-experiment-plan-check" \outDir -> do
                let configPath = "config/generated/harness.json"
                    planPath = outDir </> "partial.json"
                config <- either assertFailure pure =<< loadHarnessConfig configPath
                named <- either assertFailure pure =<< loadExperimentPlan configPath config "stage2-thesis"
                assertEqual "named plan repeats" 3 named.repeats
                BSC.writeFile planPath "{\"repeats\":2,\"budget_secs\":0.25,\"oracle\":{\"stack_frames\":7}}"
                custom <- either assertFailure pure =<< loadExperimentPlan configPath config (T.pack planPath)
                assertEqual "custom repeats" 2 custom.repeats
                assertEqual "custom budget" (Just 0.25) custom.budgetSecs
                assertEqual "default worker count" 1 custom.workerJobs
                assertEqual "custom stack frames" 7 custom.oracle.stackFrames
                assertEqual "default slippage setting" False custom.oracle.rejectOnSlippage
        , testCase "barrier census reports syntax fallback fences and normalizes dialect aliases" do
            withTempDir "occam-barrier-census-check" \outDir -> do
                BSC.writeFile (outDir </> "valid.sql") "SELECT 1;"
                BSC.writeFile (outDir </> "valid.meta.json") "{\"dialect\":\"sqlite\"}"
                BSC.writeFile (outDir </> "invalid.sql") "SELECT FROM;"
                BSC.writeFile
                    (outDir </> "invalid.meta.json")
                    "{\"id\":\"bad-sql\",\"target\":{\"dbms\":\"postgres\"}}"
                result <- barrierCensus outDir
                case result of
                    Right (Object obj) -> do
                        case KM.lookup "overall" obj of
                            Just (Object overall) -> do
                                assertEqual "two fixture cases" (Just (Number 2)) (KM.lookup "case_count" overall)
                                assertEqual "one syntax error" (Just (Number 1)) (KM.lookup "syntax_errors" overall)
                                assertEqual "one global fence" (Just (Number 1)) (KM.lookup "cases_with_global_fences" overall)
                                assertEqual
                                    "fallback barrier counted by reason"
                                    (Just (object ["SyntaxErrorBarrier" .= (1 :: Int)]))
                                    (KM.lookup "declared_by_reason" overall)
                            other -> assertFailure ("Unexpected overall census: " <> show other)
                        case KM.lookup "by_dialect" obj of
                            Just (Object byDialect) ->
                                assertBool "normalizes the postgres alias" (KM.member "postgresql" byDialect)
                            other -> assertFailure ("Unexpected dialect summaries: " <> show other)
                        case KM.lookup "cases" obj of
                            Just (Array cases) ->
                                assertEqual
                                    "IDs match batch metadata and filename fallback"
                                    [Just (String "bad-sql"), Just (String "valid")]
                                    [KM.lookup "case_id" row | Object row <- toList cases]
                            other -> assertFailure ("Unexpected census cases: " <> show other)
                    other -> assertFailure ("Unexpected barrier census: " <> show other)
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
        , testCase "aggregateOutDir decodes full Stats and writes JSON with stratified metrics" do
            withTempDir "occam-batch-report" \outDir -> do
                let writeJson p val = do
                        createDirectoryIfMissing True (takeDirectory p)
                        BSL.writeFile p (encode val)
                let sampleSig sid =
                        object
                            [ "id" .= (sid :: T.Text)
                            , "hash" .= (sid <> "_hash")
                            , "category" .= ("assertion" :: T.Text)
                            , "frames" .= ([] :: [T.Text])
                            , "top_frames" .= (5 :: Int)
                            ]
                    emptyReplay qualifiedFlag sig =
                        object
                            [ "runs_required" .= (5 :: Int)
                            , "runs_passed" .= (if qualifiedFlag then 5 else 0 :: Int)
                            , "all_passed" .= qualifiedFlag
                            , "consistent_signature" .= qualifiedFlag
                            , "baseline_signature" .= sig
                            , "matches_baseline" .= (Nothing :: Maybe Bool)
                            , "slippage_count" .= (0 :: Int)
                            , "observed_signatures" .= maybeToList sig
                            , "observed_signature_ids" .= ([] :: [T.Text])
                            , "interrupted" .= False
                            , "runs" .= ([] :: [Value])
                            ]
                writeJson (outDir </> "qualified" </> "c1" </> "input_replay.json") $
                    object
                        [ "case_id" .= ("c1" :: T.Text)
                        , "dbms" .= ("sqlite" :: T.Text)
                        , "qualified" .= True
                        , "baseline_signature" .= sampleSig "sig-A"
                        , "replay" .= emptyReplay True (Just (sampleSig "sig-A"))
                        ]
                writeJson (outDir </> "qualified" </> "c2" </> "input_replay.json") $
                    object
                        [ "case_id" .= ("c2" :: T.Text)
                        , "dbms" .= ("sqlite" :: T.Text)
                        , "qualified" .= True
                        , "baseline_signature" .= sampleSig "sig-A"
                        , "replay" .= emptyReplay True (Just (sampleSig "sig-A"))
                        ]
                writeJson (outDir </> "qualified" </> "c3" </> "input_replay.json") $
                    object
                        [ "case_id" .= ("c3" :: T.Text)
                        , "dbms" .= ("postgres" :: T.Text)
                        , "qualified" .= True
                        , "baseline_signature" .= sampleSig "sig-B"
                        , "replay" .= emptyReplay True (Just (sampleSig "sig-B"))
                        ]
                writeJson (outDir </> "qualified" </> "c_bad" </> "input_replay.json") $
                    object
                        [ "case_id" .= ("c_bad" :: T.Text)
                        , "dbms" .= ("postgres" :: T.Text)
                        , "qualified" .= False
                        , "baseline_signature" .= (Nothing :: Maybe Value)
                        , "replay" .= emptyReplay False (Nothing :: Maybe Value)
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
                                    { cacheEnabled = True
                                    , jobs = 2
                                    , totalTimeoutMicros = Nothing
                                    , maxOracleExecutions = Nothing
                                    , preflightMicros = 0
                                    , preflightAttempts = 0
                                    , preflightResetMicros = Nothing
                                    , preflightOracleMicros = Nothing
                                    , initialStatements = Just 10
                                    }
                            em =
                                EvaluationMetrics
                                    { runStatus = st
                                    , failure = Nothing
                                    , cacheUsed = True
                                    , budget = budget
                                    , totalSeconds = elapsed
                                    , reductionSeconds = elapsed
                                    , oracleSeconds = 1.0
                                    , resetSeconds = 0.2
                                    , preflightResetSeconds = Nothing
                                    , preflightOracleSeconds = Nothing
                                    , oracleAttempts = attempts
                                    , databaseObservations = attempts
                                    , completedOracleProcesses = attempts
                                    , acceptedObservations = 2
                                    , rejectedObservations = max 0 (attempts - 2)
                                    , timedOutObservations = 0
                                    , resetFailures = 0
                                    , oracleErrors = 0
                                    , cancelledObservations = 0
                                    , oracleOutcomes = Map.empty
                                    , cacheHits = 1
                                    , cacheMisses = 1
                                    , cacheJoins = 0
                                    , validationSeconds = 0.1
                                    , validationOracleSeconds = 0.1
                                    , validationResetSeconds = 0.0
                                    , validationObservations = 1
                                    , validationDatabaseObservations = 1
                                    , validationErrors = 0
                                    , validationOutcomes = Map.empty
                                    , validationPassed = st == EvaluationCompleted
                                    , auditSeconds = 0.05
                                    , auditChecks = 1
                                    , auditCompleted = True
                                    , auditSkipped = False
                                    , minimalityClaim =
                                        if st == EvaluationCompleted
                                            then StatementDeletionOneMinimal
                                            else MinimalityNotEstablished
                                    , curve =
                                        [ EvaluationPoint 0.5 1 200 (Just 10) False True
                                        , EvaluationPoint 1.0 2 redB (Just 2) False True
                                        ]
                                    , stages = []
                                    , costAttribution = Just snap
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
                                , "baseline_signature" .= sampleSig sig
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

                report <- BSL.readFile (outDir </> "aggregated_stats.json")
                assertEqual "persisted report" (Just (toJSON agg)) (decode report)
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
        , testCase "compiled harness DBMS specs and reducer presets agree" do
            let generated = "config" </> "generated"
            config <- either assertFailure pure =<< loadHarnessConfig (generated </> "harness.json")
            assertEqual
                "six DBMS IDs"
                ["postgres", "mysql", "mariadb", "clickhouse", "sqlite", "duckdb"]
                (map (\spec -> spec.identifier) config.dbms)
            assertEqual
                "PostgreSQL CLI dialect"
                (Just "postgresql")
                ((\spec -> spec.occamDialect) <$> lookupDbmsSpec config "postgres")
            assertEqual
                "MariaDB CLI dialect"
                (Just "mariadb")
                ((\spec -> spec.occamDialect) <$> lookupDbmsSpec config "mariadb")
            assertEqual
                "built-in profile names"
                ["audit-only", "ddmin-only", "ours-s", "ours-si", "ours-full", "ours-nodep"]
                (map (\profile -> profile.key) config.profiles)
            forM_ config.profiles \profile -> do
                let key = T.unpack profile.key
                preset <- BSL.readFile (generated </> "profiles" </> (key <> ".json"))
                assertEqual (key <> " JSON preset") (Just profile.config) (decode preset)
                assertBool (key <> " strategy flag") ("--strategy" `elem` profile.cliFlags)
        , testCase "Harness.Config decodes real config/generated/harness.json and renders SQUIRREL YAML" do
            cfgPath <- resolveHarnessConfigPath Nothing
            exists <- doesFileExist cfgPath
            assertBool ("harness.json missing at " ++ cfgPath) exists
            eCfg <- loadHarnessConfig cfgPath
            hc <- either (\e -> assertFailure e >> error "unreachable") pure eCfg
            assertBool "dbms non-empty" (not (null hc.dbms))
            assertBool "profiles non-empty" (not (null hc.profiles))
            assertBool "stage2_thesis plan present" (Map.member "stage2_thesis" hc.plans)
            let mariadbSpec = lookupSquirrelSpec hc "mariadb"
            assertEqual
                "mariadb target_bin_rel"
                (Just "build/db_driver")
                (mariadbSpec >>= (`squirrelFieldText` "target_bin_rel"))
            let opts =
                    SquirrelRenderOptions
                        { dbms = "mariadb"
                        , squirrelHome = "/opt/squirrel"
                        , host = "127.0.0.1"
                        , port = 3306
                        , user = "root"
                        , password = "p'w"
                        , socket = "/tmp/mysql.sock"
                        , dbName = "occam_test"
                        , serverCmd = "/usr/sbin/mariadbd"
                        }
                oracleOpts =
                    SquirrelRenderOptions
                        { dbms = "oracle"
                        , squirrelHome = "/opt/squirrel"
                        , host = "127.0.0.1"
                        , port = 3306
                        , user = "root"
                        , password = "p'w"
                        , socket = "/tmp/mysql.sock"
                        , dbName = "occam_test"
                        , serverCmd = "/usr/sbin/mariadbd"
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
                ( either
                    (const True)
                    (const False)
                    (renderSquirrelYaml hc oracleOpts)
                )
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

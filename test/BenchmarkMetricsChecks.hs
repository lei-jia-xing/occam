module BenchmarkMetricsChecks (tests) where

import Data.Aeson (Value (..), eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Occam.Benchmark.Metrics (
    RunSummary (..),
    Stats (..),
    computeAttemptAuc,
    engineOverheadMicros,
    generateReportForDirectory,
    missingStatsSummary,
    renderSummaryJson,
 )
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationPoint (..),
    EvaluationStatus (..),
    MinimalityClaim (..),
 )
import Occam.Metrics (
    FamilyCounters (..),
    FamilyTag (..),
    MetricPolicy (..),
    MetricsSnapshot (..),
    ParseKind (..),
    emptyFamilyCounters,
 )
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
    testGroup
        "Benchmark Metrics & Reporting"
        [ testCase "computeAttemptAuc returns 1.0 for empty or invalid inputs" do
            assertEqual "zero initial bytes" 1.0 (computeAttemptAuc 0 [] 10)
            assertEqual "negative attempts" 1.0 (computeAttemptAuc 1000 [] 0)
        , testCase "computeAttemptAuc integrates discrete AUC correctly" do
            let curve =
                    [ EvaluationPoint 1.0 1 1000 Nothing False True
                    , EvaluationPoint 2.0 2 500 Nothing False True
                    , EvaluationPoint 3.0 3 100 Nothing False False
                    , EvaluationPoint 4.0 4 200 Nothing False True
                    ]
                auc = computeAttemptAuc 1000 curve 4
            assertBool "AUC matches (1 + 0.5 + 0.5 + 0.2) / 4 = 0.55" (abs (auc - 0.55) < 1e-6)
        , testCase "engineOverheadMicros aggregates FFI, decode, and candidateBuild" do
            let snapshot =
                    MetricsSnapshot
                        { snapshotPolicy = MetricPolicy "ddmin" "none" 1
                        , snapshotFamilies =
                            Map.fromList
                                [ (TagStatement, emptyFamilyCounters {candidateBuildMicros = 75})
                                ]
                        , snapshotGeneratedSkipped = Map.empty
                        , snapshotParseFfiMicros =
                            Map.fromList [(ParseStatementOnly, 100), (ParseSyntaxChunks, 50)]
                        , snapshotParseDecodeMicros =
                            Map.fromList [(ParseStatementOnly, 25)]
                        }
            assertEqual "engine overhead sum" (Just 250) (engineOverheadMicros (Just snapshot))
        , testCase "renderSummaryJson outputs structured 4D schema JSON" do
            let run1 =
                    RunSummary
                        { runName = "caseA__default__r1"
                        , runCase = Just "caseA"
                        , runArm = Just "default"
                        , runRep = Just "r1"
                        , runNameWarning = Nothing
                        , runReducedBytes = Just 200
                        , runReducedStatements = Just 2
                        , runReductionRatio = Just 0.2
                        , runOracleAttempts = Just 4
                        , runAccepted = Just 2
                        , runCacheHits = Just 3
                        , runAttemptAuc = Just 0.55
                        , runEngineOverheadUs = Just 250
                        , runElapsedSeconds = Just 7.25
                        , runStatus = "completed"
                        }
                run2 = missingStatsSummary "caseA__intra__r1"
                encoded = BSL.toStrict (encode (renderSummaryJson [run1, run2]))
            assertBool "has runs array" (BSC.pack "\"runs\"" `BS.isInfixOf` encoded)
            assertBool "has run1" (BSC.pack "caseA__default__r1" `BS.isInfixOf` encoded)
            assertBool "has missing-stats" (BSC.pack "missing-stats" `BS.isInfixOf` encoded)
        , testCase "run name parsing keeps extra separators and flags malformed names" do
            let extended = missingStatsSummary "caseA__default__r1__post"
                malformed = missingStatsSummary "caseA__default"
            assertEqual "case parsed" (Just "caseA") (runCase extended)
            assertEqual "arm parsed" (Just "default") (runArm extended)
            assertEqual "extra separators stay in repetition" (Just "r1__post") (runRep extended)
            assertEqual "valid name has no warning" Nothing (runNameWarning extended)
            assertEqual "malformed case is not guessed" Nothing (runCase malformed)
            assertEqual "malformed arm is not guessed" Nothing (runArm malformed)
            assertEqual "malformed rep is not guessed" Nothing (runRep malformed)
            assertBool "malformed name carries warning" (maybe False (not . T.null) (runNameWarning malformed))
        , testCase "generateReportForDirectory scans run directories and writes reports" do
            tempDir <- getTemporaryDirectory
            let testDir = tempDir </> "occam-test-benchmark-metrics"
                runDir1 = testDir </> "caseA__default__r1"
                runDir2 = testDir </> "caseA__intra__r1"
            createDirectoryIfMissing True runDir1
            createDirectoryIfMissing True runDir2
            let budget = sampleEvaluationBudget
                metrics = sampleEvaluationMetrics
                stats =
                    Stats
                        { initial_bytes = 1000
                        , reduced_bytes = 200
                        , reduction_ratio = 80.0
                        , initial_statements = 10
                        , reduced_statements = 2
                        , oracle_executions = 4
                        , oracle_attempts = 4
                        , initial_oracle_observations = 0
                        , cache_hits = 3
                        , cache_misses = 1
                        , cache_joins = 0
                        , cache_hit_ratio = 0.75
                        , cancelled_observations = 0
                        , status = EvaluationCompleted
                        , reduction_settings = Null
                        , evaluation_budget = budget
                        , evaluation_metrics = metrics
                        , initial_oracle_seconds = 0.01
                        , elapsed_seconds = 7.25
                        }
            BSL.writeFile (runDir1 </> "stats.json") (encode stats)
            runs <- generateReportForDirectory testDir
            assertEqual "scanned 2 runs" 2 (length runs)
            summaryExists <- doesFileExist (testDir </> "summary.json")
            reportExists <- doesFileExist (testDir </> "report.md")
            assertBool "summary.json created" summaryExists
            assertBool "report.md not created" (not reportExists)
            removeDirectoryRecursive testDir
        , testCase "evaluation and metrics emit snake_case JSON keys" $ do
            let encoded = BSL.toStrict (encode sampleEvaluationMetrics)
                hasKey key = BSC.pack ("\"" ++ key ++ "\"") `BS.isInfixOf` encoded
            mapM_
                (\key -> assertBool ("missing snake_case key: " ++ key) (hasKey key))
                [ "evaluation_status"
                , "evaluation_total_seconds"
                , "evaluation_curve"
                , "point_call_index"
                , "evaluation_cost_attribution"
                , "snapshot_policy"
                , "snapshot_families"
                , "snapshot_generated_skipped"
                , "snapshot_parse_ffi_micros"
                , "physical_starts"
                , "tag"
                , "counters"
                ]
            assertBool "legacy camelCase evaluation key leaked" (not (hasKey "evaluationStatus"))
            assertBool "legacy camelCase snapshot key leaked" (not (hasKey "snapshotPolicy"))
        , testCase "legacy camelCase evaluation and metrics JSON still decodes" $ do
            let legacyPoint =
                    BSC.pack
                        "{\"pointElapsedSeconds\":1.5,\"pointCallIndex\":3,\"pointCandidateBytes\":100,\"pointCandidateStatements\":null,\"pointCacheHit\":false,\"pointAccepted\":true}"
                expectedPoint = EvaluationPoint 1.5 3 100 Nothing False True
                legacyConfig =
                    BSC.pack
                        "{\"evaluationCacheEnabled\":true,\"evaluationJobs\":1,\"evaluationTotalTimeoutMicros\":null,\"evaluationMaxOracleExecutions\":null,\"evaluationPreflightMicros\":0,\"evaluationPreflightAttempts\":0,\"evaluationPreflightResetMicros\":null,\"evaluationPreflightOracleMicros\":null,\"evaluationInitialStatements\":10}"
            assertEqual
                "legacy point decodes"
                (Right expectedPoint)
                (eitherDecodeStrict' legacyPoint :: Either String EvaluationPoint)
            assertEqual
                "legacy config decodes"
                (Right sampleEvaluationBudget)
                (eitherDecodeStrict' legacyConfig :: Either String EvaluationConfig)
            let legacySnapshot =
                    BSC.pack
                        "{\"snapshotPolicy\":{\"metricSearchPolicy\":\"ddmin\",\"metricColumnMode\":\"none\",\"metricJobs\":1},\"snapshotFamilies\":[{\"tag\":{\"kind\":\"statement\"},\"counters\":{\"physicalStarts\":2,\"retiredAccept\":1,\"retiredReject\":0,\"retiredInconclusive\":0,\"physicalCompletions\":0,\"completedUnselected\":0,\"completedUnselectedInteractionWallMicros\":0,\"canceled\":0,\"canceledInteractionWallMicros\":0,\"failed\":0,\"failedInteractionWallMicros\":0,\"candidateBuildMicros\":0,\"oracleMicros\":null,\"resetMicros\":null}}],\"snapshotGeneratedSkipped\":{},\"snapshotParseFfiMicros\":{},\"snapshotParseDecodeMicros\":{}}"
            assertEqual
                "legacy metrics snapshot decodes"
                (Right sampleMetricsSnapshot)
                (eitherDecodeStrict' legacySnapshot :: Either String MetricsSnapshot)
        ]

sampleEvaluationBudget :: EvaluationConfig
sampleEvaluationBudget =
    EvaluationConfig
        { cacheEnabled = True
        , jobs = 1
        , totalTimeoutMicros = Nothing
        , maxOracleExecutions = Nothing
        , preflightMicros = 0
        , preflightAttempts = 0
        , preflightResetMicros = Nothing
        , preflightOracleMicros = Nothing
        , initialStatements = Just 10
        }

sampleMetricsSnapshot :: MetricsSnapshot
sampleMetricsSnapshot =
    MetricsSnapshot
        { snapshotPolicy = MetricPolicy "ddmin" "none" 1
        , snapshotFamilies =
            Map.fromList
                [ (TagStatement, emptyFamilyCounters {physicalStarts = 2, retiredAccept = 1})
                ]
        , snapshotGeneratedSkipped = Map.empty
        , snapshotParseFfiMicros = Map.empty
        , snapshotParseDecodeMicros = Map.empty
        }

sampleEvaluationMetrics :: EvaluationMetrics
sampleEvaluationMetrics =
    EvaluationMetrics
        { runStatus = EvaluationCompleted
        , failure = Nothing
        , cacheUsed = True
        , budget = sampleEvaluationBudget
        , totalSeconds = 7.25
        , reductionSeconds = 7.25
        , oracleSeconds = 5.0
        , resetSeconds = 0.0
        , preflightResetSeconds = Nothing
        , preflightOracleSeconds = Nothing
        , oracleAttempts = 4
        , databaseObservations = 4
        , completedOracleProcesses = 4
        , acceptedObservations = 2
        , rejectedObservations = 2
        , timedOutObservations = 0
        , resetFailures = 0
        , oracleErrors = 0
        , cancelledObservations = 0
        , oracleOutcomes = Map.empty
        , cacheHits = 3
        , cacheMisses = 1
        , cacheJoins = 0
        , validationSeconds = 0.0
        , validationOracleSeconds = 0.0
        , validationResetSeconds = 0.0
        , validationObservations = 0
        , validationDatabaseObservations = 0
        , validationErrors = 0
        , validationOutcomes = Map.empty
        , validationPassed = True
        , auditSeconds = 0.0
        , auditChecks = 0
        , auditCompleted = True
        , auditSkipped = False
        , minimalityClaim = MinimalityNotEstablished
        , curve =
            [ EvaluationPoint 1.0 1 1000 Nothing False True
            , EvaluationPoint 2.0 2 500 Nothing False True
            , EvaluationPoint 3.0 3 100 Nothing False False
            , EvaluationPoint 4.0 4 200 Nothing False True
            ]
        , stages = []
        , costAttribution = Just sampleMetricsSnapshot
        }

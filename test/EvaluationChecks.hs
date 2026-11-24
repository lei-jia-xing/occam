module EvaluationChecks (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Aeson (decode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Occam.Core (OracleOutcome (..))
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    MinimalityClaim (..),
    ReductionActionResult (..),
    metricPolicyForReduction,
    parseSQLDialect,
    runEvaluatedReduction,
    runEvaluationRequest,
    sqlDialectName,
 )
import Occam.Facts (ParserEditCategory (..))
import Occam.Logger (nullLogger)
import Occam.Metrics (
    FamilyCounters (..),
    FamilyTag (..),
    MetricsSnapshot (..),
 )
import Occam.Oracle (
    OracleConfig (..),
    OracleObservationDetails (..),
    prepareOracle,
    shellOracleConfig,
 )
import Occam.Parser (SQLDialect (..), countStatements, dialectCode, dialectName)
import Occam.Pool (poolLiveWorkers, poolOracle, testOrdered, withPredicatePool)
import Occam.Reducer (
    ReductionConfig (..),
    ReductionEvidence (..),
    defaultReductionConfig,
    reduceSQL,
    reduceSQLWithConfig,
 )
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import TestSupport (withSentinelPath)

tests :: TestTree
tests =
    testGroup
        "Evaluator"
        [ testCase "execution caps reserve two uncached final checks" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = (predicateOracle 500000 "exit 0") {oracleResetCommand = Just "sleep 0.01"}
                config = evaluationConfig False Nothing (Just 3)
                action _ _ pool = do
                    _ <- poolOracle pool candidate
                    _ <- poolOracle pool candidate
                    pure (ReductionActionResult candidate Nothing)
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    Nothing
                    config
                    GenericSQL
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = evaluationMetrics result
            assertEqual "accepted checkpoint survives execution cap" candidate (evaluatedSQL result)
            assertEqual "one reduction oracle call allowed" 1 (evaluationOracleAttempts metrics)
            assertEqual "two validation calls reserved" 2 (evaluationValidationObservations metrics)
            assertBool "last repeated target validation passes" (evaluationValidationPassed metrics)
            assertEqual "budget exhaustion is reported" EvaluationExecutionBudget (evaluationStatus metrics)
            assertEqual "uncached profile records no hits" 0 (evaluationCacheHits metrics)
            assertEqual
                "outcome category counts every real reduction attempt"
                1
                (sum (Map.elems (evaluationOracleOutcomes metrics)))
            assertEqual
                "validation outcomes remain separate"
                2
                (sum (Map.elems (evaluationValidationOutcomes metrics)))
            assertBool "reset process time is reported separately" (evaluationResetSeconds metrics > 0)
            assertBool "validation reset time is separate" (evaluationValidationResetSeconds metrics > 0)
        , testCase "coarse-to-fine search checkpoints a structural edit within finite budget" $ do
            let initial = "INSERT INTO t VALUES (123456);\nSELECT x FROM t WHERE x = 42;\n"
                candidate = "INSERT INTO t VALUES (123456);\nSELECT x FROM t;\n"
                predicate =
                    "sql=$(cat); case \"$sql\" in "
                        ++ "'INSERT INTO t VALUES (123456);\nSELECT x FROM t WHERE x = 42;'|"
                        ++ "'INSERT INTO t VALUES (123456);\nSELECT x FROM t;') exit 0 ;; "
                        ++ "*) exit 1 ;; esac"
                oracleCfg = predicateOracle 500000 predicate
                config = evaluationConfig False Nothing (Just 8)
                reducerConfig =
                    defaultReductionConfig
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionIntraStatement = True
                        , reductionUnrestricted = False
                        , reductionDialect = SQLite
                        }
                action _ _ pool =
                    ReductionActionResult <$> reduceSQLWithConfig reducerConfig nullLogger pool initial <*> pure Nothing
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction Nothing config SQLite oracleCfg Nothing nullLogger initial checkpoint action
                saved <- BS.readFile checkpoint
                assertEqual "the structural candidate is persisted" candidate saved
                pure value
            let metrics = evaluationMetrics result
            assertEqual "the accepted structural SQL is returned" candidate (evaluatedSQL result)
            assertEqual
                "further search stops only at the explicit budget"
                EvaluationExecutionBudget
                (evaluationStatus metrics)
            assertBool "the accepted checkpoint passes final validation" (evaluationValidationPassed metrics)
        , testCase "syntax reduction checkpoints both fine edits before budget exhaustion" $ do
            let initial = "INSERT INTO t VALUES (123456, 654321);\nSELECT x FROM t WHERE flag;\n"
                candidate = "INSERT INTO t VALUES (0, 0);\nSELECT x FROM t WHERE flag;\n"
                predicate =
                    "sql=$(cat); case \"$sql\" in "
                        ++ "'INSERT INTO t VALUES (123456, 654321);\nSELECT x FROM t WHERE flag;'|"
                        ++ "'INSERT INTO t VALUES (0, 654321);\nSELECT x FROM t WHERE flag;'|"
                        ++ "'INSERT INTO t VALUES (123456, 0);\nSELECT x FROM t WHERE flag;'|"
                        ++ "'INSERT INTO t VALUES (0, 0);\nSELECT x FROM t WHERE flag;') exit 0 ;; "
                        ++ "*) exit 1 ;; esac"
                oracleCfg = predicateOracle 500000 predicate
                config = evaluationConfig False Nothing (Just 9)
                reducerConfig =
                    defaultReductionConfig
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionIntraStatement = True
                        , reductionUnrestricted = False
                        , reductionDialect = SQLite
                        }
                action _ _ pool =
                    ReductionActionResult <$> reduceSQLWithConfig reducerConfig nullLogger pool initial <*> pure Nothing
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction Nothing config SQLite oracleCfg Nothing nullLogger initial checkpoint action
                saved <- BS.readFile checkpoint
                assertEqual "both fine edits are checkpointed before coarse replay" candidate saved
                pure value
            let metrics = evaluationMetrics result
            assertEqual "both fine edits are returned" candidate (evaluatedSQL result)
            assertEqual
                "the next search attempt exhausts the explicit budget"
                EvaluationExecutionBudget
                (evaluationStatus metrics)
            assertBool "the accepted checkpoint passes final validation" (evaluationValidationPassed metrics)
        , testCase "finite oracle budget reaches both later same-category literal reductions" $ do
            let initial = "INSERT INTO t VALUES (42, 123456, 654321);\nSELECT x FROM t WHERE flag;"
                afterFirst = "INSERT INTO t VALUES (42, 0, 654321);\nSELECT x FROM t WHERE flag;"
                afterSecond = "INSERT INTO t VALUES (42, 123456, 0);\nSELECT x FROM t WHERE flag;"
                afterBoth = "INSERT INTO t VALUES (42, 0, 0);\nSELECT x FROM t WHERE flag;"
                shellQuote sql =
                    "'" ++ concatMap escape (BSC.unpack sql) ++ "'"
                escape character
                    | character == '\'' = "'\\''"
                    | otherwise = [character]
                predicate =
                    "sql=$(cat); case \"$sql\" in "
                        ++ shellQuote initial
                        ++ "|"
                        ++ shellQuote afterFirst
                        ++ "|"
                        ++ shellQuote afterSecond
                        ++ "|"
                        ++ shellQuote afterBoth
                        ++ ") exit 0 ;; *) exit 1 ;; esac"
                oracleCfg = predicateOracle 500000 predicate
                config = evaluationConfig False Nothing (Just 11)
                reducerConfig =
                    defaultReductionConfig
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionIntraStatement = True
                        , reductionUnrestricted = False
                        , reductionDialect = SQLite
                        }
                action _ _ pool =
                    ReductionActionResult <$> reduceSQLWithConfig reducerConfig nullLogger pool initial <*> pure Nothing
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction Nothing config SQLite oracleCfg Nothing nullLogger initial checkpoint action
                saved <- BS.readFile checkpoint
                assertEqual "both later literal reductions are checkpointed" afterBoth saved
                pure value
            let metrics = evaluationMetrics result
            assertEqual "both later values are zeroed before the cap" afterBoth (evaluatedSQL result)
            assertEqual
                "the explicit oracle cap interrupts only further search"
                EvaluationExecutionBudget
                (evaluationStatus metrics)
            assertBool "the accepted checkpoint passes final validation" (evaluationValidationPassed metrics)
        , testCase "preflight exhaustion reports execution budget and keeps the accepted checkpoint" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
            assertBool "candidate is a smaller acceptable reduction" (BS.length candidate < BS.length initial)
            preflight <- prepareOracle oracleCfg initial
            case preflight of
                Left _ -> assertBool "initial SQL passes its real preflight oracle" False
                Right details -> do
                    let config =
                            (evaluationConfig False (Just 30000000) (Just 1))
                                { evaluationPreflightMicros = totalElapsedMicros details
                                , evaluationPreflightAttempts = 1
                                , evaluationPreflightResetMicros = Just (resetElapsedMicros details)
                                , evaluationPreflightOracleMicros = oracleElapsedMicros details
                                }
                        action _ _ _ = fail "reduction must not start after preflight exhausts the execution cap"
                    result <- withCheckpoint $ \checkpoint -> do
                        value <-
                            runEvaluatedReduction
                                Nothing
                                config
                                GenericSQL
                                oracleCfg
                                Nothing
                                nullLogger
                                initial
                                checkpoint
                                action
                        saved <- BS.readFile checkpoint
                        assertEqual "exhausted budget cannot replace accepted SQL" initial saved
                        pure value
                    let metrics = evaluationMetrics result
                    assertEqual
                        "an exhausted execution cap is not a wall timeout"
                        EvaluationExecutionBudget
                        (evaluationStatus metrics)
                    assertEqual "preflight consumed the only allowed execution" 0 (evaluationOracleAttempts metrics)
                    assertEqual "no reducer subprocess started" 0 (evaluationDatabaseObservations metrics)
                    assertEqual "no final validations were scheduled" 0 (evaluationValidationObservations metrics)
                    assertEqual
                        "no final validation subprocess started"
                        0
                        (evaluationValidationDatabaseObservations metrics)
                    assertEqual "last accepted SQL remains the initial input" initial (evaluatedSQL result)
        , testCase "timeouts keep the last accepted checkpoint and make no 1-minimal claim" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig True (Just 2200000) Nothing
                action _ _ pool = do
                    _ <- poolOracle pool candidate
                    threadDelay 2500000
                    pure (ReductionActionResult candidate Nothing)
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    Nothing
                    config
                    GenericSQL
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = evaluationMetrics result
            assertEqual "checkpoint remains the last accepted SQL" candidate (evaluatedSQL result)
            assertEqual "timeout is not reported as completion" EvaluationTimeout (evaluationStatus metrics)
            assertBool "final candidate still receives repeated validation" (evaluationValidationPassed metrics)
            assertEqual "audit did not finish" False (evaluationAuditCompleted metrics)
            assertEqual
                "truncated reductions are not called minimal"
                MinimalityNotEstablished
                (evaluationMinimalityClaim metrics)
        , testCase "wall-budget cancellation counts an oracle process already started" $ do
            let initial = "SELECT 1;\n"
                oracleCfg = predicateOracle 2000000 "sleep 2"
                config = evaluationConfig False (Just 400000) Nothing
                action _ _ pool = do
                    _ <- poolOracle pool initial
                    pure (ReductionActionResult initial Nothing)
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    Nothing
                    config
                    GenericSQL
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = evaluationMetrics result
            assertEqual "in-flight oracle attempt is counted" 1 (evaluationOracleAttempts metrics)
            assertEqual "oracle process start survives cancellation" 1 (evaluationDatabaseObservations metrics)
            assertEqual
                "timeout prevents a minimality claim"
                MinimalityNotEstablished
                (evaluationMinimalityClaim metrics)
        , testCase "audit removals replace the selected reduction result" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig True Nothing Nothing
                action _ _ pool = do
                    _ <- poolOracle pool candidate
                    pure (ReductionActionResult candidate Nothing)
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction
                        Nothing
                        config
                        GenericSQL
                        oracleCfg
                        Nothing
                        nullLogger
                        initial
                        checkpoint
                        action
                saved <- BS.readFile checkpoint
                assertEqual "audit result is checkpointed" "" saved
                pure value
            let metrics = evaluationMetrics result
            assertEqual "audit result becomes the selected reduction" "" (evaluatedSQL result)
            assertBool "audit completed" (evaluationAuditCompleted metrics)
            assertEqual
                "audit fixed point is claimed minimal"
                StatementDeletionOneMinimal
                (evaluationMinimalityClaim metrics)
            assertBool "audit result passed independent final validation" (evaluationValidationPassed metrics)
        , testCase "shared audit evidence marks the audit as skipped, not zero-duration" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig True Nothing Nothing
                action _ _ pool = do
                    _ <- poolOracle pool candidate
                    pure (ReductionActionResult candidate (Just (ReductionEvidence candidate True)))
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    Nothing
                    config
                    GenericSQL
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = evaluationMetrics result
            assertBool "shared evidence audit is skipped" (evaluationAuditSkipped metrics)
            assertBool "skipped audit still counts as completed" (evaluationAuditCompleted metrics)
            assertEqual "skipped audit performs no checks" 0 (evaluationAuditChecks metrics)
            assertEqual "skipped audit reports zero duration" 0.0 (evaluationAuditSeconds metrics)
            assertEqual
                "shared evidence preserves the 1-minimal claim"
                StatementDeletionOneMinimal
                (evaluationMinimalityClaim metrics)
        , testCase "parser transport failures preserve the last accepted checkpoint" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                invalidUtf8 = BS.pack [255]
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig False Nothing Nothing
                action _ _ pool = do
                    _ <- poolOracle pool candidate
                    bytes <-
                        withPredicatePool 1 False (const (pure True)) (\inner -> reduceSQL SQLite inner invalidUtf8)
                    pure (ReductionActionResult bytes Nothing)
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction Nothing config SQLite oracleCfg Nothing nullLogger initial checkpoint action
                saved <- BS.readFile checkpoint
                assertEqual "failed parser input cannot overwrite the accepted checkpoint" candidate saved
                pure value
            let metrics = evaluationMetrics result
            assertEqual "parser failure is reported" EvaluationError (evaluationStatus metrics)
            assertEqual "evaluation returns the last accepted SQL" candidate (evaluatedSQL result)
        , testCase "dialect flags parse to their dependency dialect" $ do
            assertEqual "generic dialect" (Right GenericSQL) (parseSQLDialect "generic")
            assertEqual "PostgreSQL dialect" (Right PostgreSQL) (parseSQLDialect "POSTGRESQL")
            assertEqual "MySQL dialect" (Right MySQL) (parseSQLDialect "mysql")
            assertEqual "SQLite dialect" (Right SQLite) (parseSQLDialect "sqlite")
            assertEqual "DuckDB dialect" (Right DuckDB) (parseSQLDialect "duckdb")
            assertEqual "ClickHouse dialect" (Right ClickHouse) (parseSQLDialect "clickhouse")
            assertEqual "Oracle dialect" (Right Oracle) (parseSQLDialect "oracle")
            assertEqual "Ansi dialect" (Right Ansi) (parseSQLDialect "ansi")
            assertEqual "BigQuery dialect" (Right BigQuery) (parseSQLDialect "bigquery")
            assertEqual "Databricks dialect" (Right Databricks) (parseSQLDialect "databricks")
            assertEqual "Hive dialect" (Right Hive) (parseSQLDialect "hive")
            assertEqual "MsSql dialect" (Right MsSql) (parseSQLDialect "mssql")
            assertEqual "SqlServer alias" (Right MsSql) (parseSQLDialect "sqlserver")
            assertEqual "Redshift dialect" (Right Redshift) (parseSQLDialect "redshift")
            assertEqual "Snowflake dialect" (Right Snowflake) (parseSQLDialect "snowflake")
            assertEqual "Spark dialect" (Right Spark) (parseSQLDialect "spark")
            assertEqual "SparkSql alias" (Right Spark) (parseSQLDialect "sparksql")
            assertEqual "Teradata dialect" (Right Teradata) (parseSQLDialect "teradata")
            assertEqual
                "invalid dialect rejected"
                True
                (case parseSQLDialect "invalid" of Left _ -> True; Right _ -> False)
            assertEqual "dialect serialized using CLI spelling" "postgresql" (sqlDialectName PostgreSQL)
            assertEqual "DuckDB serialized using CLI spelling" "duckdb" (sqlDialectName DuckDB)
            assertEqual "ClickHouse serialized using CLI spelling" "clickhouse" (sqlDialectName ClickHouse)
            assertEqual "Oracle serialized using CLI spelling" "oracle" (sqlDialectName Oracle)
            assertEqual "Ansi serialized using CLI spelling" "ansi" (sqlDialectName Ansi)
            assertEqual "BigQuery serialized using CLI spelling" "bigquery" (sqlDialectName BigQuery)
            assertEqual "Databricks serialized using CLI spelling" "databricks" (sqlDialectName Databricks)
            assertEqual "Hive serialized using CLI spelling" "hive" (sqlDialectName Hive)
            assertEqual "MsSql serialized using CLI spelling" "mssql" (sqlDialectName MsSql)
            assertEqual "Redshift serialized using CLI spelling" "redshift" (sqlDialectName Redshift)
            assertEqual "Snowflake serialized using CLI spelling" "snowflake" (sqlDialectName Snowflake)
            assertEqual "Spark serialized using CLI spelling" "spark" (sqlDialectName Spark)
            assertEqual "Teradata serialized using CLI spelling" "teradata" (sqlDialectName Teradata)
        , testCase "all selected dialects round-trip name, parse, and code" $ do
            let dialects = [minBound .. maxBound] :: [SQLDialect]
            assertEqual "sixteen selected dialects" 16 (length dialects)
            mapM_
                ( \dialect -> do
                    let name = dialectName dialect
                    assertEqual ("CLI spelling " ++ show dialect) name (sqlDialectName dialect)
                    assertEqual ("parse round-trip " ++ show dialect) (Right dialect) (parseSQLDialect (T.unpack name))
                )
                dialects
            assertEqual "distinct FFI codes 0..15" [0 .. 15] (sort (map dialectCode dialects))
            counts <- mapM (`countStatements` "SELECT 1; SELECT 2;") dialects
            assertEqual "every dialect counts two statements over FFI" (replicate 16 (Right 2)) counts
        , testCase "speculative losers are cancelled without timeout or error inflation" $
            withSentinelPath $ \sentinel -> do
                let acceptSQL = "SELECT 1;\n"
                    sleepSQL = "SELECT SLEEP;\n"
                    oracleCfg =
                        predicateOracle
                            30000000
                            ( "sql=$(cat); case \"$sql\" in *SLEEP*) (sleep 2; printf leaked > "
                                ++ shellQuotePath sentinel
                                ++ ") & wait;; *) exit 0;; esac"
                            )
                    config = (evaluationConfig True Nothing Nothing) {evaluationJobs = 4}
                    action _ _ pool = do
                        outcomes <- testOrdered pool [acceptSQL, sleepSQL]
                        case outcomes of
                            [(OracleAccepted, _)] -> pure (ReductionActionResult acceptSQL Nothing)
                            _ -> assertFailure "ordered head accept was lost to speculation"
                result <- withCheckpoint $ \checkpoint ->
                    runEvaluatedReduction
                        Nothing
                        config
                        GenericSQL
                        oracleCfg
                        Nothing
                        nullLogger
                        acceptSQL
                        checkpoint
                        action
                let metrics = evaluationMetrics result
                assertEqual "slow loser must not time out" 0 (evaluationTimedOutObservations metrics)
                assertEqual "cancelled work is not an error" 0 (evaluationOracleErrors metrics)
                assertBool "loser cancellation was not accounted" (evaluationCancelledObservations metrics >= 1)
                threadDelay 2500000
                leaked <- doesFileExist sentinel
                assertBool "a cancelled loser descendant survived the process-group cleanup" (not leaked)
        , testCase "reduction drains all workers before final validation" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = (evaluationConfig True Nothing Nothing) {evaluationJobs = 4}
            liveAtEndRef <- newIORef (1 :: Int)
            let action _ _ pool = do
                    _ <- testOrdered pool [candidate, initial]
                    live <- poolLiveWorkers pool
                    writeIORef liveAtEndRef live
                    pure (ReductionActionResult candidate Nothing)
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    Nothing
                    config
                    GenericSQL
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = evaluationMetrics result
            liveAtEnd <- readIORef liveAtEndRef
            assertEqual "workers outlived the reduction action" 0 liveAtEnd
            assertEqual "drained run did not complete" EvaluationCompleted (evaluationStatus metrics)
            assertEqual "final validation did not run twice" 2 (evaluationValidationObservations metrics)
            assertBool "final validation failed" (evaluationValidationPassed metrics)
        , testCase "parallel jobs complete a small end-to-end reduction" $ do
            let initial = "SELECT 1;\nSELECT 2;\nSELECT 3;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = (evaluationConfig True Nothing Nothing) {evaluationJobs = 4}
                reducerConfig = defaultReductionConfig {reductionDialect = GenericSQL}
                action _ _ pool =
                    ReductionActionResult <$> reduceSQLWithConfig reducerConfig nullLogger pool initial <*> pure Nothing
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    Nothing
                    config
                    GenericSQL
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = evaluationMetrics result
            assertEqual "parallel reduction lost the empty fixed point" "" (evaluatedSQL result)
            assertEqual "parallel run did not complete" EvaluationCompleted (evaluationStatus metrics)
            assertBool "parallel result failed final validation" (evaluationValidationPassed metrics)
        , testCase "evaluation request record runs the same evaluation" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig False Nothing Nothing
                action _ _ pool = do
                    _ <- poolOracle pool candidate
                    pure (ReductionActionResult candidate (Just (ReductionEvidence candidate True)))
            result <- withCheckpoint $ \checkpoint ->
                runEvaluationRequest
                    EvaluationRequest
                        { requestOnProgress = \_ -> pure ()
                        , requestMetricsPolicy = Nothing
                        , requestConfig = config
                        , requestDialect = GenericSQL
                        , requestOracleConfig = oracleCfg
                        , requestPreflightObservation = Nothing
                        , requestLogger = nullLogger
                        , requestInitialSQL = initial
                        , requestCheckpoint = checkpoint
                        , requestAction = action
                        }
            let metrics = evaluationMetrics result
            assertEqual "request run returns the accepted candidate" candidate (evaluatedSQL result)
            assertEqual "request run completes" EvaluationCompleted (evaluationStatus metrics)
            assertEqual "request run performs two final checks" 2 (evaluationValidationObservations metrics)
            assertBool "request run validates the final candidate" (evaluationValidationPassed metrics)
        , testCase "cost attribution is captured on budget exhaustion" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig False Nothing (Just 3)
                reducerConfig = defaultReductionConfig {reductionDialect = GenericSQL}
                action _ _ pool = do
                    _ <- poolOracle pool candidate
                    _ <- poolOracle pool candidate
                    pure (ReductionActionResult candidate Nothing)
            policy <- case metricPolicyForReduction reducerConfig 1 of
                Left err -> assertFailure ("test policy rejected: " ++ show err)
                Right p -> pure p
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    (Just policy)
                    config
                    GenericSQL
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = evaluationMetrics result
            assertEqual "budget exhaustion is reported" EvaluationExecutionBudget (evaluationStatus metrics)
            assertEqual "one reduction oracle call allowed" 1 (evaluationOracleAttempts metrics)
            case evaluationCostAttribution metrics of
                Nothing -> assertFailure "cost section missing on budget end"
                Just snapshot -> case Map.lookup TagUnknown (snapshotFamilies snapshot) of
                    Nothing -> assertFailure "untagged pool work missing a family"
                    Just counters -> do
                        assertEqual "one physical worker started" 1 (physicalStarts counters)
                        assertEqual "one ordered accept retired" 1 (retiredAccept counters)
            assertEqual "cost section round-trips through JSON" (Just metrics) (decode (encode metrics))
        , testCase "cost attribution is captured on wall-clock timeout" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 5000000 "exit 0"
                config = evaluationConfig True (Just 4000000) Nothing
                reducerConfig = defaultReductionConfig {reductionDialect = GenericSQL}
                action _ _ pool = do
                    _ <- poolOracle pool candidate
                    threadDelay 30000000
                    pure (ReductionActionResult candidate Nothing)
            policy <- case metricPolicyForReduction reducerConfig 1 of
                Left err -> assertFailure ("test policy rejected: " ++ show err)
                Right p -> pure p
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    (Just policy)
                    config
                    GenericSQL
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = evaluationMetrics result
            assertEqual "timeout is not reported as completion" EvaluationTimeout (evaluationStatus metrics)
            assertEqual "one reduction call ran before the stall" 1 (evaluationOracleAttempts metrics)
            case evaluationCostAttribution metrics of
                Nothing -> assertFailure "cost section missing after drained timeout"
                Just snapshot -> case Map.lookup TagUnknown (snapshotFamilies snapshot) of
                    Nothing -> assertFailure "untagged pool work missing from drained snapshot"
                    Just counters -> do
                        assertEqual "physical start preserved across timeout" 1 (physicalStarts counters)
                        assertEqual "ordered accept preserved across timeout" 1 (retiredAccept counters)
        , testCase "inconclusive oracle error keeps attributed counts and a validated checkpoint" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                sleeping = "SELECT SLEEP;\n"
                oracleCfg =
                    predicateOracle
                        2000000
                        "sql=$(cat); case \"$sql\" in *SLEEP*) sleep 10;; *) exit 0;; esac"
                config = evaluationConfig True Nothing Nothing
                reducerConfig = defaultReductionConfig {reductionDialect = GenericSQL}
                action _ _ pool = do
                    _ <- poolOracle pool candidate
                    _ <- poolOracle pool sleeping
                    pure (ReductionActionResult sleeping Nothing)
            policy <- case metricPolicyForReduction reducerConfig 1 of
                Left err -> assertFailure ("test policy rejected: " ++ show err)
                Right p -> pure p
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    (Just policy)
                    config
                    GenericSQL
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = evaluationMetrics result
            assertEqual "inconclusive oracle is reported as an error" EvaluationError (evaluationStatus metrics)
            assertEqual "last accepted checkpoint survives the error" candidate (evaluatedSQL result)
            assertBool "accepted checkpoint passes final validation" (evaluationValidationPassed metrics)
            case evaluationCostAttribution metrics of
                Nothing -> assertFailure "cost section missing on inconclusive error"
                Just snapshot -> case Map.lookup TagUnknown (snapshotFamilies snapshot) of
                    Nothing -> assertFailure "untagged pool work missing a family"
                    Just counters -> do
                        assertEqual "ordered accept retired before the error" 1 (retiredAccept counters)
                        assertEqual "inconclusive retirement attributed" 1 (retiredInconclusive counters)
        , testCase "family tags encode structurally and reject legacy string keys" $ do
            let tags =
                    [ TagUnknown
                    , TagStatement
                    , TagSyntax EditList "remove-values_row"
                    , TagBundle "remove-column-bundle"
                    ]
            assertEqual "family tags round-trip" (Just tags) (decode (encode tags))
            assertEqual
                "legacy string tag is rejected"
                (Nothing :: Maybe FamilyTag)
                (decode "\"syntax:list:remove-values_row\"")
        ]

evaluationConfig :: Bool -> Maybe Int -> Maybe Int -> EvaluationConfig
evaluationConfig cacheEnabled wallBudget callBudget =
    EvaluationConfig
        { evaluationCacheEnabled = cacheEnabled
        , evaluationJobs = 1
        , evaluationTotalTimeoutMicros = wallBudget
        , evaluationMaxOracleExecutions = callBudget
        , evaluationPreflightMicros = 0
        , evaluationPreflightAttempts = 0
        , evaluationPreflightResetMicros = Nothing
        , evaluationPreflightOracleMicros = Nothing
        , evaluationInitialStatements = Nothing
        }

predicateOracle :: Int -> String -> OracleConfig
predicateOracle timeoutMicros command =
    shellOracleConfig command timeoutMicros Nothing

withCheckpoint :: (FilePath -> IO a) -> IO a
withCheckpoint action = do
    directory <- getTemporaryDirectory
    (checkpoint, handle) <- openTempFile directory "occam-evaluation-checkpoint"
    hClose handle
    removeFile checkpoint
    action checkpoint `finally` mapM_ removeIfExists [checkpoint, checkpoint ++ ".tmp"]
    where
        removeIfExists path = do
            exists <- doesFileExist path
            when exists (removeFile path)

shellQuotePath :: String -> String
shellQuotePath value = "'" ++ concatMap quote value ++ "'"
    where
        quote '\'' = "'\\''"
        quote character = [character]

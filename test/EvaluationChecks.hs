{-# LANGUAGE OverloadedStrings #-}

module EvaluationChecks (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationResult (..),
    parseSQLDialect,
    runEvaluatedReduction,
    sqlDialectName,
 )
import Occam.Logger (nullLogger)
import Occam.Oracle (
    OracleConfig (..),
    OracleObservationDetails (..),
    prepareOracle,
 )
import Occam.Parser (SQLDialect (..))
import Occam.Reducer (reduceSQL)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
    testGroup
        "Evaluator"
        [ testCase "execution caps reserve two uncached final checks" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = (predicateOracle 500000 "exit 0") {oracleResetCommand = Just "sleep 0.01"}
                config = evaluationConfig False Nothing (Just 3)
                action _ oracle = do
                    _ <- oracle candidate
                    _ <- oracle candidate
                    pure candidate
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction config GenericSQL oracleCfg Nothing nullLogger initial checkpoint action
            let metrics = evaluationMetrics result
            assertEqual "accepted checkpoint survives execution cap" candidate (evaluatedSQL result)
            assertEqual "one reduction oracle call allowed" 1 (evaluationOracleAttempts metrics)
            assertEqual "two validation calls reserved" 2 (evaluationValidationRepeats metrics)
            assertBool "last repeated target validation passes" (evaluationValidationPassed metrics)
            assertEqual "budget exhaustion is reported" "execution-budget" (evaluationStatus metrics)
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
                        action _ oracle = oracle candidate >> pure candidate
                    result <- withCheckpoint $ \checkpoint -> do
                        value <-
                            runEvaluatedReduction config GenericSQL oracleCfg Nothing nullLogger initial checkpoint action
                        saved <- BS.readFile checkpoint
                        assertEqual "exhausted budget cannot replace accepted SQL" initial saved
                        pure value
                    let metrics = evaluationMetrics result
                    assertEqual
                        "an exhausted execution cap is not a wall timeout"
                        "execution-budget"
                        (evaluationStatus metrics)
                    assertEqual "preflight consumed the only allowed execution" 0 (evaluationOracleAttempts metrics)
                    assertEqual "no reducer subprocess started" 0 (evaluationDatabaseObservations metrics)
                    assertEqual "no final validations were scheduled" 0 (evaluationValidationRepeats metrics)
                    assertEqual
                        "no final validation subprocess started"
                        0
                        (evaluationValidationDatabaseObservations metrics)
                    assertEqual "last accepted SQL remains the initial input" initial (evaluatedSQL result)
        , testCase "timeouts keep the last accepted checkpoint and make no 1-minimal claim" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 100000 "exit 0"
                config = evaluationConfig True (Just 300000) Nothing
                action _ oracle = do
                    _ <- oracle candidate
                    threadDelay 2000000
                    pure candidate
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction config GenericSQL oracleCfg Nothing nullLogger initial checkpoint action
            let metrics = evaluationMetrics result
            assertEqual "checkpoint remains the last accepted SQL" candidate (evaluatedSQL result)
            assertEqual "timeout is not reported as completion" "timeout" (evaluationStatus metrics)
            assertBool "final candidate still receives repeated validation" (evaluationValidationPassed metrics)
            assertEqual "audit did not finish" False (evaluationAuditCompleted metrics)
            assertEqual
                "truncated reductions are not called minimal"
                "not-established"
                (evaluationMinimalityClaim metrics)
        , testCase "wall-budget cancellation counts an oracle process already started" $ do
            let initial = "SELECT 1;\n"
                oracleCfg = predicateOracle 100000 "sleep 2"
                config = evaluationConfig False (Just 300000) Nothing
                action _ oracle = do
                    _ <- oracle initial
                    pure initial
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction config GenericSQL oracleCfg Nothing nullLogger initial checkpoint action
            let metrics = evaluationMetrics result
            assertEqual "in-flight oracle attempt is counted" 1 (evaluationOracleAttempts metrics)
            assertEqual "oracle process start survives cancellation" 1 (evaluationDatabaseObservations metrics)
            assertEqual
                "timeout prevents a minimality claim"
                "not-established"
                (evaluationMinimalityClaim metrics)
        , testCase "audit removals replace the selected reduction result" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig True Nothing Nothing
                action _ oracle = do
                    _ <- oracle candidate
                    pure candidate
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction config GenericSQL oracleCfg Nothing nullLogger initial checkpoint action
                saved <- BS.readFile checkpoint
                assertEqual "audit result is checkpointed" "" saved
                pure value
            let metrics = evaluationMetrics result
            assertEqual "audit result becomes the selected reduction" "" (evaluatedSQL result)
            assertBool "audit completed" (evaluationAuditCompleted metrics)
            assertEqual
                "audit fixed point is claimed minimal"
                "statement-deletion-1-minimal"
                (evaluationMinimalityClaim metrics)
            assertBool "audit result passed independent final validation" (evaluationValidationPassed metrics)
        , testCase "parser transport failures preserve the last accepted checkpoint" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                invalidUtf8 = BS.pack [255]
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig False Nothing Nothing
                action _ oracle = do
                    _ <- oracle candidate
                    reduceSQL SQLite (const (pure True)) invalidUtf8
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction config SQLite oracleCfg Nothing nullLogger initial checkpoint action
                saved <- BS.readFile checkpoint
                assertEqual "failed parser input cannot overwrite the accepted checkpoint" candidate saved
                pure value
            let metrics = evaluationMetrics result
            assertEqual "parser failure is reported" "error" (evaluationStatus metrics)
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
        ]

evaluationConfig :: Bool -> Maybe Int -> Maybe Int -> EvaluationConfig
evaluationConfig cacheEnabled wallBudget callBudget =
    EvaluationConfig
        { evaluationCacheEnabled = cacheEnabled
        , evaluationTotalTimeoutMicros = wallBudget
        , evaluationMaxOracleExecutions = callBudget
        , evaluationPreflightMicros = 0
        , evaluationPreflightAttempts = 0
        , evaluationPreflightResetMicros = Nothing
        , evaluationPreflightOracleMicros = Nothing
        }

predicateOracle :: Int -> String -> OracleConfig
predicateOracle timeoutMicros command =
    OracleConfig
        { oracleCommand = "/bin/sh"
        , oracleArgs = ["-c", command]
        , oracleTimeoutMicros = timeoutMicros
        , oracleResetCommand = Nothing
        }

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

{-# LANGUAGE OverloadedStrings #-}

module EvaluationChecks (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Occam.Dependency (SQLDialect (..))
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationResult (..),
    parseSQLDialect,
    runEvaluatedReduction,
    sqlDialectName,
 )
import Occam.Logger (nullLogger)
import Occam.Oracle (OracleConfig (..))
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
                runEvaluatedReduction config oracleCfg nullLogger initial checkpoint action
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
                runEvaluatedReduction config oracleCfg nullLogger initial checkpoint action
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
                runEvaluatedReduction config oracleCfg nullLogger initial checkpoint action
            let metrics = evaluationMetrics result
            assertEqual "in-flight oracle attempt is counted" 1 (evaluationOracleAttempts metrics)
            assertEqual "oracle process start survives cancellation" 1 (evaluationDatabaseObservations metrics)
            assertEqual
                "timeout prevents a minimality claim"
                "not-established"
                (evaluationMinimalityClaim metrics)
        , testCase "audit probes do not replace the selected reduction result" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig True Nothing Nothing
                action _ oracle = do
                    _ <- oracle candidate
                    pure candidate
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction config oracleCfg nullLogger initial checkpoint action
            let metrics = evaluationMetrics result
            assertEqual "audit cannot erase the candidate" candidate (evaluatedSQL result)
            assertBool "audit completed" (evaluationAuditCompleted metrics)
            assertEqual "audit found a further reduction" "not-established" (evaluationMinimalityClaim metrics)
            assertBool "candidate passed independent final validation" (evaluationValidationPassed metrics)
        , testCase "dialect flags parse to their dependency dialect" $ do
            assertEqual "generic dialect" (Right GenericSQL) (parseSQLDialect "generic")
            assertEqual "PostgreSQL dialect" (Right PostgreSQL) (parseSQLDialect "POSTGRESQL")
            assertEqual "MySQL dialect" (Right MySQL) (parseSQLDialect "mysql")
            assertEqual "SQLite dialect" (Right SQLite) (parseSQLDialect "sqlite")
            assertEqual
                "invalid dialect rejected"
                True
                (case parseSQLDialect "oracle" of Left _ -> True; Right _ -> False)
            assertEqual "dialect serialized using CLI spelling" "postgresql" (sqlDialectName PostgreSQL)
        ]

evaluationConfig :: Bool -> Maybe Int -> Maybe Int -> EvaluationConfig
evaluationConfig cacheEnabled wallBudget callBudget =
    EvaluationConfig
        { evaluationCacheEnabled = cacheEnabled
        , evaluationTotalTimeoutMicros = wallBudget
        , evaluationMaxOracleExecutions = callBudget
        , evaluationPreflightMicros = 0
        , evaluationPreflightAttempts = 0
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

{-# LANGUAGE OverloadedRecordDot #-}

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
    runEvaluationRequestWithNativeValidation,
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
    OracleObservation (..),
    OracleObservationDetails (..),
    prepareOracle,
    shellOracleConfig,
 )
import Occam.Parser (
    DialectProfile (..),
    SQLDialect (..),
    allDialectProfiles,
    countStatements,
    dialectName,
 )
import Occam.Pool (observationDetails, poolLiveWorkers, poolOracle, testOrdered, withPredicatePool)
import Occam.Reducer (
    ReductionConfig (..),
    ReductionEvidence (..),
    defaultReductionConfigFor,
    reduceSQL,
    reduceSQLWithConfig,
 )
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
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
                    SQLite
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = result.metrics
            assertEqual "accepted checkpoint survives execution cap" candidate result.sql
            assertEqual "one reduction oracle call allowed" 1 metrics.oracleAttempts
            assertEqual "two validation calls reserved" 2 metrics.validationObservations
            assertBool "last repeated target validation passes" metrics.validationPassed
            assertEqual "budget exhaustion is reported" EvaluationExecutionBudget metrics.runStatus
            assertEqual "uncached profile records no hits" 0 metrics.cacheHits
            assertEqual
                "outcome category counts every real reduction attempt"
                1
                (sum (Map.elems metrics.oracleOutcomes))
            assertEqual
                "validation outcomes remain separate"
                2
                (sum (Map.elems metrics.validationOutcomes))
            assertBool "reset process time is reported separately" (metrics.resetSeconds > 0)
            assertBool "validation reset time is separate" (metrics.validationResetSeconds > 0)
        , testCase "native validation uses the independent observer for both final probes" $ do
            let initial = "SELECT 1; SELECT 2;"
                candidate = "SELECT 2;"
                observe started _ = started >> pure (observationDetails (OracleFinished ExitSuccess))
                validate started _ = started >> pure (observationDetails (OracleFinished (ExitFailure 1)))
            result <-
                runEvaluationRequestWithNativeValidation 500000 observe validate $
                    EvaluationRequest
                        { onProgress = const (pure ())
                        , metricsPolicy = Nothing
                        , config = evaluationConfig True Nothing (Just 3)
                        , dialect = SQLite
                        , oracleConfig = ()
                        , preflightObservation = Just (OracleFinished ExitSuccess)
                        , logger = nullLogger
                        , initialSQL = initial
                        , checkpoint = ""
                        , collectCurve = False
                        , action = \_ _ pool -> do
                            outcome <- poolOracle pool candidate
                            assertEqual "reduction accepts its candidate" OracleAccepted outcome
                            pure (ReductionActionResult candidate (Just (ReductionEvidence candidate True)))
                        }
            let metrics = result.metrics
            assertEqual
                "validation bypasses the cached acceptance"
                EvaluationValidationFailed
                metrics.runStatus
            assertEqual
                "both validations executed independently"
                2
                metrics.validationDatabaseObservations
            assertEqual
                "rejected validation suppresses minimality"
                MinimalityNotEstablished
                metrics.minimalityClaim
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
                    (defaultReductionConfigFor SQLite)
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionIntraStatement = True
                        , reductionUnrestricted = False
                        }
                action _ _ pool =
                    ReductionActionResult <$> reduceSQLWithConfig reducerConfig nullLogger pool initial <*> pure Nothing
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction Nothing config SQLite oracleCfg Nothing nullLogger initial checkpoint action
                saved <- BS.readFile checkpoint
                assertEqual "the structural candidate is persisted" candidate saved
                pure value
            let metrics = result.metrics
            assertEqual "the accepted structural SQL is returned" candidate result.sql
            assertEqual
                "further search stops only at the explicit budget"
                EvaluationExecutionBudget
                metrics.runStatus
            assertBool "the accepted checkpoint passes final validation" metrics.validationPassed
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
                    (defaultReductionConfigFor SQLite)
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionIntraStatement = True
                        , reductionUnrestricted = False
                        }
                action _ _ pool =
                    ReductionActionResult <$> reduceSQLWithConfig reducerConfig nullLogger pool initial <*> pure Nothing
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction Nothing config SQLite oracleCfg Nothing nullLogger initial checkpoint action
                saved <- BS.readFile checkpoint
                assertEqual "both fine edits are checkpointed before coarse replay" candidate saved
                pure value
            let metrics = result.metrics
            assertEqual "both fine edits are returned" candidate result.sql
            assertEqual
                "the next search attempt exhausts the explicit budget"
                EvaluationExecutionBudget
                metrics.runStatus
            assertBool "the accepted checkpoint passes final validation" metrics.validationPassed
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
                    (defaultReductionConfigFor SQLite)
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionIntraStatement = True
                        , reductionUnrestricted = False
                        }
                action _ _ pool =
                    ReductionActionResult <$> reduceSQLWithConfig reducerConfig nullLogger pool initial <*> pure Nothing
            result <- withCheckpoint $ \checkpoint -> do
                value <-
                    runEvaluatedReduction Nothing config SQLite oracleCfg Nothing nullLogger initial checkpoint action
                saved <- BS.readFile checkpoint
                assertEqual "both later literal reductions are checkpointed" afterBoth saved
                pure value
            let metrics = result.metrics
            assertEqual "both later values are zeroed before the cap" afterBoth result.sql
            assertEqual
                "the explicit oracle cap interrupts only further search"
                EvaluationExecutionBudget
                metrics.runStatus
            assertBool "the accepted checkpoint passes final validation" metrics.validationPassed
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
                                { preflightMicros = totalElapsedMicros details
                                , preflightAttempts = 1
                                , preflightResetMicros = Just (resetElapsedMicros details)
                                , preflightOracleMicros = oracleElapsedMicros details
                                }
                        action _ _ _ = fail "reduction must not start after preflight exhausts the execution cap"
                    result <- withCheckpoint $ \checkpoint -> do
                        value <-
                            runEvaluatedReduction
                                Nothing
                                config
                                SQLite
                                oracleCfg
                                Nothing
                                nullLogger
                                initial
                                checkpoint
                                action
                        saved <- BS.readFile checkpoint
                        assertEqual "exhausted budget cannot replace accepted SQL" initial saved
                        pure value
                    let metrics = result.metrics
                    assertEqual
                        "an exhausted execution cap is not a wall timeout"
                        EvaluationExecutionBudget
                        metrics.runStatus
                    assertEqual "preflight consumed the only allowed execution" 0 metrics.oracleAttempts
                    assertEqual "no reducer subprocess started" 0 metrics.databaseObservations
                    assertEqual "no final validations were scheduled" 0 metrics.validationObservations
                    assertEqual
                        "no final validation subprocess started"
                        0
                        metrics.validationDatabaseObservations
                    assertEqual "last accepted SQL remains the initial input" initial result.sql
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
                    SQLite
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = result.metrics
            assertEqual "checkpoint remains the last accepted SQL" candidate result.sql
            assertEqual "timeout is not reported as completion" EvaluationTimeout metrics.runStatus
            assertBool "final candidate still receives repeated validation" metrics.validationPassed
            assertEqual "audit did not finish" False metrics.auditCompleted
            assertEqual
                "truncated reductions are not called minimal"
                MinimalityNotEstablished
                metrics.minimalityClaim
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
                    SQLite
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = result.metrics
            assertEqual "in-flight oracle attempt is counted" 1 metrics.oracleAttempts
            assertEqual "oracle process start survives cancellation" 1 metrics.databaseObservations
            assertEqual
                "timeout prevents a minimality claim"
                MinimalityNotEstablished
                metrics.minimalityClaim
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
                        SQLite
                        oracleCfg
                        Nothing
                        nullLogger
                        initial
                        checkpoint
                        action
                saved <- BS.readFile checkpoint
                assertEqual "audit result is checkpointed" "" saved
                pure value
            let metrics = result.metrics
            assertEqual "audit result becomes the selected reduction" "" result.sql
            assertBool "audit completed" metrics.auditCompleted
            assertEqual
                "audit fixed point is claimed minimal"
                StatementDeletionOneMinimal
                metrics.minimalityClaim
            assertBool "audit result passed independent final validation" metrics.validationPassed
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
                    SQLite
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = result.metrics
            assertBool "shared evidence audit is skipped" metrics.auditSkipped
            assertBool "skipped audit still counts as completed" metrics.auditCompleted
            assertEqual "skipped audit performs no checks" 0 metrics.auditChecks
            assertEqual "skipped audit reports zero duration" 0.0 metrics.auditSeconds
            assertEqual
                "shared evidence preserves the 1-minimal claim"
                StatementDeletionOneMinimal
                metrics.minimalityClaim
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
            let metrics = result.metrics
            assertEqual "parser failure is reported" EvaluationError metrics.runStatus
            assertEqual "evaluation returns the last accepted SQL" candidate result.sql
        , testCase "dialect flags parse to their dependency dialect" $ do
            assertEqual "PostgreSQL dialect" (Right PostgreSQL) (parseSQLDialect "POSTGRESQL")
            assertEqual "postgres alias" (Right PostgreSQL) (parseSQLDialect "postgres")
            assertEqual "MySQL dialect" (Right MySQL) (parseSQLDialect "mysql")
            assertEqual "MariaDB dialect" (Right MariaDB) (parseSQLDialect "mariadb")
            assertEqual "SQLite dialect" (Right SQLite) (parseSQLDialect "sqlite")
            assertEqual "DuckDB dialect" (Right DuckDB) (parseSQLDialect "duckdb")
            assertEqual "ClickHouse dialect" (Right ClickHouse) (parseSQLDialect "clickhouse")
            assertEqual
                "removed generic dialect rejected"
                True
                (case parseSQLDialect "generic" of Left _ -> True; Right _ -> False)
            assertEqual
                "removed oracle dialect rejected"
                True
                (case parseSQLDialect "oracle" of Left _ -> True; Right _ -> False)
            assertEqual
                "invalid dialect rejected"
                True
                (case parseSQLDialect "invalid" of Left _ -> True; Right _ -> False)
            assertEqual "dialect serialized using CLI spelling" "postgresql" (sqlDialectName PostgreSQL)
            assertEqual "MySQL serialized using CLI spelling" "mysql" (sqlDialectName MySQL)
            assertEqual "MariaDB serialized using CLI spelling" "mariadb" (sqlDialectName MariaDB)
            assertEqual "SQLite serialized using CLI spelling" "sqlite" (sqlDialectName SQLite)
            assertEqual "DuckDB serialized using CLI spelling" "duckdb" (sqlDialectName DuckDB)
            assertEqual "ClickHouse serialized using CLI spelling" "clickhouse" (sqlDialectName ClickHouse)
        , testCase "all selected dialects round-trip name, parse, and profile" $ do
            let dialects = [minBound .. maxBound] :: [SQLDialect]
            assertEqual "six active dialects" 6 (length dialects)
            mapM_
                ( \dialect -> do
                    let name = dialectName dialect
                    assertEqual ("CLI spelling " ++ show dialect) name (sqlDialectName dialect)
                    assertEqual ("parse round-trip " ++ show dialect) (Right dialect) (parseSQLDialect (T.unpack name))
                )
                dialects
            assertEqual
                "all dialect profiles registered uniquely"
                dialects
                (sort (map dialectId allDialectProfiles))
            counts <- mapM (`countStatements` "SELECT 1; SELECT 2;") dialects
            assertEqual "every dialect counts two statements" (replicate 6 (Right 2)) counts
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
                    config = (evaluationConfig True Nothing Nothing) {jobs = 4}
                    action _ _ pool = do
                        outcomes <- testOrdered pool [acceptSQL, sleepSQL]
                        case outcomes of
                            [(OracleAccepted, _)] -> pure (ReductionActionResult acceptSQL Nothing)
                            _ -> assertFailure "ordered head accept was lost to speculation"
                result <- withCheckpoint $ \checkpoint ->
                    runEvaluatedReduction
                        Nothing
                        config
                        SQLite
                        oracleCfg
                        Nothing
                        nullLogger
                        acceptSQL
                        checkpoint
                        action
                let metrics = result.metrics
                assertEqual "slow loser must not time out" 0 metrics.timedOutObservations
                assertEqual "cancelled work is not an error" 0 metrics.oracleErrors
                assertBool "loser cancellation was not accounted" (metrics.cancelledObservations >= 1)
                threadDelay 2500000
                leaked <- doesFileExist sentinel
                assertBool "a cancelled loser descendant survived the process-group cleanup" (not leaked)
        , testCase "reduction drains all workers before final validation" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = (evaluationConfig True Nothing Nothing) {jobs = 4}
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
                    SQLite
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = result.metrics
            liveAtEnd <- readIORef liveAtEndRef
            assertEqual "workers outlived the reduction action" 0 liveAtEnd
            assertEqual "drained run did not complete" EvaluationCompleted metrics.runStatus
            assertEqual "final validation did not run twice" 2 metrics.validationObservations
            assertBool "final validation failed" metrics.validationPassed
        , testCase "parallel jobs complete a small end-to-end reduction" $ do
            let initial = "SELECT 1;\nSELECT 2;\nSELECT 3;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = (evaluationConfig True Nothing Nothing) {jobs = 4}
                reducerConfig = defaultReductionConfigFor SQLite
                action _ _ pool =
                    ReductionActionResult <$> reduceSQLWithConfig reducerConfig nullLogger pool initial <*> pure Nothing
            result <- withCheckpoint $ \checkpoint ->
                runEvaluatedReduction
                    Nothing
                    config
                    SQLite
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = result.metrics
            assertEqual "parallel reduction lost the empty fixed point" "" result.sql
            assertEqual "parallel run did not complete" EvaluationCompleted metrics.runStatus
            assertBool "parallel result failed final validation" metrics.validationPassed
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
                        { onProgress = \_ -> pure ()
                        , metricsPolicy = Nothing
                        , config = config
                        , dialect = SQLite
                        , oracleConfig = oracleCfg
                        , preflightObservation = Nothing
                        , logger = nullLogger
                        , initialSQL = initial
                        , checkpoint = checkpoint
                        , action = action
                        , collectCurve = True
                        }
            let metrics = result.metrics
            assertEqual "request run returns the accepted candidate" candidate result.sql
            assertEqual "request run completes" EvaluationCompleted metrics.runStatus
            assertEqual "request run performs two final checks" 2 metrics.validationObservations
            assertBool "request run validates the final candidate" metrics.validationPassed
        , testCase "cost attribution is captured on budget exhaustion" $ do
            let initial = "SELECT 1;\nSELECT 2;\n"
                candidate = "SELECT 2;\n"
                oracleCfg = predicateOracle 500000 "exit 0"
                config = evaluationConfig False Nothing (Just 3)
                reducerConfig = defaultReductionConfigFor SQLite
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
                    SQLite
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = result.metrics
            assertEqual "budget exhaustion is reported" EvaluationExecutionBudget metrics.runStatus
            assertEqual "one reduction oracle call allowed" 1 metrics.oracleAttempts
            case metrics.costAttribution of
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
                reducerConfig = defaultReductionConfigFor SQLite
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
                    SQLite
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = result.metrics
            assertEqual "timeout is not reported as completion" EvaluationTimeout metrics.runStatus
            assertEqual "one reduction call ran before the stall" 1 metrics.oracleAttempts
            case metrics.costAttribution of
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
                reducerConfig = defaultReductionConfigFor SQLite
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
                    SQLite
                    oracleCfg
                    Nothing
                    nullLogger
                    initial
                    checkpoint
                    action
            let metrics = result.metrics
            assertEqual "inconclusive oracle is reported as an error" EvaluationError metrics.runStatus
            assertEqual "last accepted checkpoint survives the error" candidate result.sql
            assertBool "accepted checkpoint passes final validation" metrics.validationPassed
            case metrics.costAttribution of
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
        { cacheEnabled = cacheEnabled
        , jobs = 1
        , totalTimeoutMicros = wallBudget
        , maxOracleExecutions = callBudget
        , preflightMicros = 0
        , preflightAttempts = 0
        , preflightResetMicros = Nothing
        , preflightOracleMicros = Nothing
        , initialStatements = Nothing
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

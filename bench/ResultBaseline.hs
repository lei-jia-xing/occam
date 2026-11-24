module ResultBaseline (
    runResultBaseline,
    runSearchBaseline,
    runLiteralBatchBaseline,
    runValuesScheduleBaseline,
    runCombinedSyntaxBaseline,
    runStatementYieldBaseline,
    runStatementOrderBaseline,
    runStatementWeightBaseline,
    runStatementRankedBaseline,
    runStatementRankedGraphFreeBaseline,
    runReparseBaseline,
    runWorkerBaseline,
) where

import Control.Monad (forM, forM_, mapAndUnzipM, unless, (>=>))
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (decodeUtf8)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Occam.AtomicFile (writeAtomicLazyBytes)
import Occam.Core (OracleOutcome (OracleRejected), ResultStatement (..))
import Occam.Dependency (analyzeStatements, graphHasGlobalBarrier)
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationProgress (..),
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    ReductionActionResult (..),
    metricPolicyForReduction,
    runEvaluationRequestWithObserver,
    sqlDialectName,
 )
import Occam.Logger (nullLogger)
import Occam.Oracle (
    OracleObservationDetails (..),
    observationOutcome,
    oracleObservationMatches,
    withCandidateWorkdir,
 )
import Occam.Oracle.Inline (newResultStatementTracker, prepareResultOracle)
import Occam.Parser (
    ParsedSQL (..),
    SQLDialect (..),
    countStatements,
    defaultParseOptions,
    parseSQL,
 )
import Occam.Reducer (
    ReductionConfig (..),
    ReductionResult (..),
    StatementSearchPolicy (..),
    defaultReductionConfig,
    reduceSQLWithResultSlicing,
 )
import System.Exit (die)
import System.FilePath ((</>))
import System.Info qualified as System

data Fixture = Fixture
    { fixtureName :: String
    , fixtureDialect :: SQLDialect
    , fixtureInput :: ByteString
    , fixtureExpected :: ByteString
    , fixtureChanged :: ByteString
    , fixtureLocation :: ResultStatement
    , fixtureFence :: Bool
    }

fixtures :: [Fixture]
fixtures =
    [ Fixture
        "sqlite-trailing-ddl"
        SQLite
        "CREATE TABLE noise(x INT); SELECT 42 AS answer; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 43 AS answer;"
        (ResultStatement 3 1)
        False
    , Fixture
        "sqlite-savepoint"
        SQLite
        "CREATE TABLE data(x INT); INSERT INTO data VALUES (42); SAVEPOINT s; UPDATE data SET x = 0; ROLLBACK TO s; RELEASE s; SELECT x AS answer FROM data; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 0 AS answer;"
        (ResultStatement 8 6)
        False
    , Fixture
        "sqlite-opaque-aggregate"
        SQLite
        "CREATE TABLE noise(x INT); CREATE TABLE data(x INT); INSERT INTO data VALUES (19), (23); SELECT sum(x) AS answer FROM data; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 42 AS different;"
        (ResultStatement 5 3)
        True
    , Fixture
        "duckdb-trailing-ddl"
        DuckDB
        "CREATE TABLE noise(x INT); SELECT [1, 2] AS earlier; SELECT 42 AS answer; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 42::BIGINT AS answer;"
        (ResultStatement 4 2)
        False
    , Fixture
        "duckdb-empty-result"
        DuckDB
        "SELECT 7 AS earlier; SELECT 42 AS answer WHERE FALSE; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer WHERE FALSE;"
        "SELECT 42 AS different WHERE FALSE;"
        (ResultStatement 3 1)
        False
    ]

searchFixtures :: [Fixture]
searchFixtures =
    [ Fixture
        "removable-values"
        SQLite
        "CREATE TABLE t(x INT); INSERT INTO t VALUES (42), (1111), (2222), (3333), (4444); SELECT x AS answer FROM t WHERE x = 42; CREATE TABLE noise(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 0 AS answer;"
        (ResultStatement 4 2)
        False
    , Fixture
        "required-values-and-independent-literals"
        SQLite
        "CREATE TABLE t(x INT, noise TEXT); INSERT INTO t VALUES (40, 'first'), (2, 'second'); SELECT sum(x) AS answer FROM t; CREATE TABLE noise(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 40 AS answer;"
        (ResultStatement 4 2)
        True
    , Fixture
        "coupled-literals"
        SQLite
        "CREATE TABLE t(x INT); INSERT INTO t VALUES (1234), (1234); SELECT max(x) - min(x) AS answer, count(*) AS n FROM t; CREATE TABLE noise(x INT);"
        "SELECT 0 AS answer, 2 AS n;"
        "SELECT 1 AS answer, 2 AS n;"
        (ResultStatement 4 2)
        True
    ]
        ++ map requiredValuesFixture [8, 64]
        ++ map removableValuesFixture [8, 64]

requiredValuesFixture :: Int -> Fixture
requiredValuesFixture size =
    let rows = intercalate ", " ["(" ++ show i ++ ", 'reducible text')" | i <- [1 .. size]]
        expected = BSC.pack ("SELECT " ++ show (sum [1 .. size]) ++ " AS answer;")
     in Fixture
            ("required-values-" ++ show size)
            SQLite
            ( "CREATE TABLE t(x INT, noise TEXT); INSERT INTO t VALUES "
                <> BSC.pack rows
                <> "; SELECT sum(x) AS answer FROM t; CREATE TABLE noise(x INT);"
            )
            expected
            "SELECT 0 AS answer;"
            (ResultStatement 4 2)
            True

removableValuesFixture :: Int -> Fixture
removableValuesFixture size =
    let rows = intercalate ", " ("(42)" : ["(" ++ show i ++ ")" | i <- [1001 .. 999 + size]])
     in Fixture
            ("removable-values-" ++ show size)
            SQLite
            ( "CREATE TABLE t(x INT); INSERT INTO t VALUES "
                <> BSC.pack rows
                <> "; SELECT x AS answer FROM t WHERE x = 42; CREATE TABLE noise(x INT);"
            )
            "SELECT 42 AS answer;"
            "SELECT 0 AS answer;"
            (ResultStatement 4 2)
            False

statementFixtures :: [Fixture]
statementFixtures =
    [ statementFixture False fenced size
    | size <- [8, 32]
    , fenced <- [False, True]
    ]

statementFixture :: Bool -> Bool -> Int -> Fixture
statementFixture skewed fenced size =
    let inputRow i =
            "INSERT INTO t VALUES ("
                ++ show i
                ++ "); CREATE TABLE noise_"
                ++ show i
                ++ if skewed && i `mod` 8 == 0
                    then "(x TEXT DEFAULT '" ++ replicate 1024 'x' ++ "');"
                    else "(x INT);"
        query = if fenced then "SELECT sum(x) AS answer FROM t;" else "SELECT x AS answer FROM t ORDER BY x;"
        reference
            | fenced = "SELECT " ++ show (sum [1 .. size]) ++ " AS answer;"
            | otherwise =
                "WITH t(x) AS (VALUES "
                    ++ intercalate ", " ["(" ++ show i ++ ")" | i <- [1 .. size]]
                    ++ ") "
                    ++ query
     in Fixture
            ( (if skewed then "skewed-" else "")
                ++ (if fenced then "fenced-statements-" else "precise-statements-")
                ++ show size
            )
            SQLite
            (BSC.pack (unlines ("CREATE TABLE t(x INT);" : map inputRow [1 .. size] ++ [query])))
            (BSC.pack reference)
            "SELECT 0 AS answer;"
            (ResultStatement (2 * size + 2) (2 * size + 1))
            fenced

data Arm = Arm
    { armName :: String
    , armConfig :: ReductionConfig -> ReductionConfig
    , armObservedResult :: Bool
    , armJobs :: Int
    }

serialArm :: String -> (ReductionConfig -> ReductionConfig) -> Bool -> Arm
serialArm name configure observed = Arm name configure observed 1

probeTimeout :: Int
probeTimeout = 5000000

require :: (Show err) => String -> Either err a -> IO a
require label = either (die . ((label ++ ": ") ++) . show) pure

check :: String -> Bool -> IO ()
check message condition = unless condition (die message)

runResultBaseline :: Int -> FilePath -> IO ()
runResultBaseline pairs out =
    runComparison
        pairs
        out
        (Just 60000000)
        (Just 256)
        []
        (serialArm "last-statement" id False, serialArm "observed-result" id True)
        fixtures

runSearchBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runSearchBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "statement-only" id True
        , serialArm "intra-statement" (\config -> config {reductionIntraStatement = True}) True
        )
        (searchFixtures ++ fixtures)

runLiteralBatchBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runLiteralBatchBaseline =
    runSyntaxPolicyBaseline
        []
        "literal-singles"
        "literal-batches"
        (\config -> config {reductionLiteralBatches = True})

runValuesScheduleBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runValuesScheduleBaseline =
    runSyntaxPolicyBaseline
        []
        "values-coarse-to-fine"
        "values-defer-fine"
        (\config -> config {reductionAdaptiveValues = True})

runCombinedSyntaxBaseline ::
    (ByteString, ByteString, ByteString) -> Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runCombinedSyntaxBaseline (scale, deep, views) =
    runSyntaxPolicyBaseline
        [ Fixture
            "scale-064"
            SQLite
            scale
            "SELECT 64 AS \"count(*)\", 2080 AS \"sum(id)\";"
            "SELECT 0 AS answer;"
            (ResultStatement 6 5)
            True
        , Fixture
            "deep-expressions"
            SQLite
            deep
            "SELECT 1 AS metric_id, 1056.7 AS calibrated_signal UNION ALL SELECT 2, -1 UNION ALL SELECT 3, 85027.5;"
            "SELECT 0 AS answer;"
            (ResultStatement 3 2)
            True
        , Fixture
            "views-and-aggregates"
            SQLite
            views
            "SELECT 'Software' AS product_cat, 2 AS distinct_orders, 750 AS total_revenue;"
            "SELECT 0 AS answer;"
            (ResultStatement 4 3)
            True
        ]
        "syntax-current"
        "syntax-batches-and-deferred-values"
        (\config -> config {reductionLiteralBatches = True, reductionAdaptiveValues = True})

runStatementYieldBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementYieldBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "graph-free-ddmin"
            (\config -> config {reductionSlicing = False, reductionClosure = False})
            False
        , serialArm "current-statement-search" id True
        )
        statementFixtures

runStatementOrderBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementOrderBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "statement-subset-first"
            (\config -> config {reductionStatementSearch = StatementSubsetFirst})
            True
        , serialArm
            "statement-complement-first"
            (\config -> config {reductionStatementSearch = StatementComplementFirst})
            True
        )
        (statementFixtures ++ searchFixtures ++ fixtures)

runStatementWeightBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementWeightBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "statement-equal-count"
            (\config -> config {reductionStatementSearch = StatementComplementFirst})
            True
        , serialArm
            "statement-equal-bytes"
            (\config -> config {reductionStatementSearch = StatementByteWeighted})
            True
        )
        (statementFixtures ++ [statementFixture True fenced 32 | fenced <- [False, True]])

runStatementRankedBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementRankedBaseline =
    runRankedComparison
        id
        ( statementFixtures
            ++ [statementFixture False True 128, statementFixture True True 32]
            ++ searchFixtures
            ++ fixtures
        )

runStatementRankedGraphFreeBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementRankedGraphFreeBaseline =
    runRankedComparison
        (\config -> config {reductionSlicing = False, reductionClosure = False})
        (statementFixtures ++ [statementFixture False True 128, statementFixture True True 32] ++ fixtures)

runRankedComparison ::
    (ReductionConfig -> ReductionConfig) ->
    [Fixture] ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runRankedComparison configure cases pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "statement-byte-weighted" configure True
        , serialArm
            "statement-ranked"
            (\config -> (configure config) {reductionStatementSearch = StatementRanked})
            True
        )
        cases

optimizedSyntax :: ReductionConfig -> ReductionConfig
optimizedSyntax config =
    config
        { reductionIntraStatement = True
        , reductionAdaptiveValues = True
        , reductionLiteralBatches = True
        }

runReparseBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runReparseBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "incremental-syntax-reparse" optimizedSyntax True
        , serialArm
            "full-syntax-reparse"
            (\config -> (optimizedSyntax config) {reductionFullSyntaxReparse = True})
            True
        )
        (searchFixtures ++ fixtures)

runWorkerBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runWorkerBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "one-oracle-worker" optimizedSyntax True
        , Arm "four-oracle-workers" optimizedSyntax True 4
        )
        (searchFixtures ++ filter ((== SQLite) . fixtureDialect) fixtures)

searchCensoredStatuses :: Maybe Int -> Maybe Int -> [EvaluationStatus]
searchCensoredStatuses wallLimit callLimit =
    [EvaluationTimeout | Just _ <- [wallLimit]] ++ [EvaluationExecutionBudget | Just _ <- [callLimit]]

runSyntaxPolicyBaseline ::
    [Fixture] ->
    String ->
    String ->
    (ReductionConfig -> ReductionConfig) ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runSyntaxPolicyBaseline extraFixtures control treatment policy pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        (serialArm control intra True, serialArm treatment (policy . intra) True)
        (searchFixtures ++ fixtures ++ extraFixtures)
    where
        intra config = config {reductionIntraStatement = True}

runComparison ::
    Int -> FilePath -> Maybe Int -> Maybe Int -> [EvaluationStatus] -> (Arm, Arm) -> [Fixture] -> IO ()
runComparison pairs out wallLimit callLimit censoredStatuses arms casesToRun = do
    capabilities <- getNumCapabilities
    (cases, failures) <-
        mapAndUnzipM (runFixture pairs wallLimit callLimit censoredStatuses arms) casesToRun
    writeAtomicLazyBytes out $
        encode $
            object
                [ "schema_version" .= (1 :: Int)
                , "pairs_per_fixture" .= pairs
                , "capabilities" .= capabilities
                , "os" .= System.os
                , "arch" .= System.arch
                , "oracle_timeout_micros" .= probeTimeout
                , "failures" .= concat failures
                , "cases" .= cases
                ]

    forM_ (listToMaybe (concat failures)) $ \failure ->
        die (failure ++ " (samples written to " ++ out ++ ")")

runFixture ::
    Int -> Maybe Int -> Maybe Int -> [EvaluationStatus] -> (Arm, Arm) -> Fixture -> IO (Value, [String])
runFixture pairs wallLimit callLimit censoredStatuses (control, treatment) Fixture {..} = do
    let config = defaultReductionConfig {reductionDialect = fixtureDialect}
        prepare remember sql = prepareResultOracle remember fixtureDialect probeTimeout sql >>= require fixtureName
        assert = check . ((fixtureName ++ ": ") ++)
    parsed <- parseSQL (defaultParseOptions fixtureDialect) fixtureInput >>= require fixtureName
    let initialCount = length (parsedUnits parsed)
        graph = analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed)
    assert "unexpected global fence classification" (graphHasGlobalBarrier graph == fixtureFence)
    locationRef <- newIORef Nothing
    (_, inputObserver) <- prepare (\_ -> writeIORef locationRef . Just) fixtureInput
    location <- readIORef locationRef
    assert "unexpected engine result location" (location == Just fixtureLocation)
    assert
        "engine/frontend statement count mismatch"
        (resultStatementCount fixtureLocation == initialCount)
    (_, expectedObserver) <- prepare (\_ _ -> pure ()) fixtureExpected
    let observation observe sql = detailedObservation <$> observe (pure ()) sql
        matches observe sql = oracleObservationMatches <$> observation observe sql
    matches expectedObserver fixtureInput >>= assert "input disagrees with the reference result"
    matches inputObserver fixtureExpected >>= assert "oracle rejected the reference result"
    let checkRejected =
            observation inputObserver >=> \observed ->
                assert
                    "expected rejection for changed result or trailing execution error"
                    (observationOutcome observed == OracleRejected)
    mapM_ checkRejected [fixtureChanged, fixtureExpected <> " DROP TABLE missing;"]
    let run arm = withCandidateWorkdir $ \dir -> do
            let reductionConfig = armConfig arm config
            policy <- require fixtureName (metricPolicyForReduction reductionConfig (armJobs arm))
            -- Both arms collect location metadata, including when the policy ignores it.
            (remember, locate) <- newResultStatementTracker True
            start <- getMonotonicTimeNSec
            (preflight, observer) <- prepare remember fixtureInput
            end <- getMonotonicTimeNSec
            physicalCalls <- newIORef (1 :: Int)
            accepted <- newIORef []
            let observed started = observer $ do
                    atomicModifyIORef' physicalCalls (\calls -> (calls + 1, ()))
                    started
                progress (ProgressCandidateAccepted bytes statements) = do
                    calls <- readIORef physicalCalls
                    now <- getMonotonicTimeNSec
                    let seconds = fromIntegral (now - end) / 1000000000 :: Double
                    modifyIORef'
                        accepted
                        ( object
                            ["physical_calls" .= calls, "seconds" .= seconds, "bytes" .= bytes, "statements" .= statements]
                            :
                        )
                progress _ = pure ()
                select sql = do
                    candidateLocation <- locate sql
                    pure (if armObservedResult arm then candidateLocation else Nothing)
            result <-
                runEvaluationRequestWithObserver
                    probeTimeout
                    observed
                    EvaluationRequest
                        { requestOnProgress = progress
                        , requestMetricsPolicy = Just policy
                        , requestConfig =
                            EvaluationConfig
                                { evaluationCacheEnabled = True
                                , evaluationJobs = armJobs arm
                                , evaluationTotalTimeoutMicros = wallLimit
                                , evaluationMaxOracleExecutions = callLimit
                                , evaluationPreflightMicros = fromIntegral ((end - start) `div` 1000)
                                , evaluationPreflightAttempts = 1
                                , evaluationPreflightResetMicros = Just (resetElapsedMicros preflight)
                                , evaluationPreflightOracleMicros = oracleElapsedMicros preflight
                                , evaluationInitialStatements = Just initialCount
                                }
                        , requestDialect = fixtureDialect
                        , requestOracleConfig = ()
                        , requestPreflightObservation = Just (detailedObservation preflight)
                        , requestLogger = nullLogger
                        , requestInitialSQL = fixtureInput
                        , requestCheckpoint = dir </> "checkpoint.sql"
                        , requestAction = \report logger pool -> do
                            ReductionResult sql evidence <-
                                reduceSQLWithResultSlicing select report reductionConfig logger pool fixtureInput
                            pure (ReductionActionResult sql evidence)
                        }
            let sql = evaluatedSQL result
                metrics = evaluationMetrics result
                label = armName arm ++ ": "
                completed = evaluationStatus metrics == EvaluationCompleted
                censored = evaluationStatus metrics `elem` censoredStatuses
            statements <- countStatements fixtureDialect sql >>= require fixtureName
            referenceAccepted <- matches expectedObserver sql
            executions <- readIORef physicalCalls
            curve <- reverse <$> readIORef accepted
            let report =
                    object
                        [ "arm" .= armName arm
                        , "reduced_sql" .= decodeUtf8 sql
                        , "reduced_bytes" .= BS.length sql
                        , "reduced_statements" .= statements
                        , "reference_accepted" .= referenceAccepted
                        , "physical_oracle_executions" .= executions
                        , "best_accepted_curve" .= curve
                        , "evaluation_metrics" .= metrics
                        ]
                failures =
                    [ fixtureName ++ ": " ++ label ++ message
                    | (passed, message) <-
                        [
                            ( completed || censored
                            , "evaluation status " ++ show (evaluationStatus metrics, evaluationFailure metrics)
                            )
                        ,
                            ( evaluationTimedOutObservations metrics == 0 && evaluationOracleErrors metrics == 0
                            , "a probe timed out or failed"
                            )
                        , (censored || evaluationAuditCompleted metrics, "statement minimality was not confirmed")
                        ,
                            ( evaluationValidationPassed metrics && evaluationValidationDatabaseObservations metrics == 2
                            , "expected two fresh, successful final validations"
                            )
                        , (referenceAccepted, "final SQL disagrees with the reference result")
                        ,
                            ( executions
                                == 1 + evaluationDatabaseObservations metrics + evaluationValidationDatabaseObservations metrics
                            , "physical execution accounting disagrees with evaluator"
                            )
                        , (maybe True (executions <=) callLimit, "physical execution budget exceeded")
                        , (censored || BS.length sql < BS.length fixtureInput, "fixture should shrink")
                        ]
                    , not passed
                    ]
            pure (report, failures)
    -- NOTE: Warm both arms once. Keep cold-start effects out of measured samples.
    warmups <- mapM run [control, treatment]
    samples <- forM [1 .. pairs] $ \pair -> do
        let arms = if odd pair then [control, treatment] else [treatment, control]
        (runs, failures) <- mapAndUnzipM run arms
        pure (object ["pair" .= pair, "runs" .= runs], concat failures)
    let report =
            object
                [ "name" .= fixtureName
                , "dialect" .= sqlDialectName fixtureDialect
                , "input_sql" .= decodeUtf8 fixtureInput
                , "expected_sql" .= decodeUtf8 fixtureExpected
                , "initial_bytes" .= BS.length fixtureInput
                , "initial_statements" .= initialCount
                , "engine_result_index" .= resultStatementIndex fixtureLocation
                , "global_fence" .= fixtureFence
                , "warmups" .= map fst warmups
                , "samples" .= map fst samples
                ]
    pure (report, concatMap snd warmups ++ concatMap snd samples)

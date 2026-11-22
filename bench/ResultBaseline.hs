module ResultBaseline (runResultBaseline) where

import Control.Monad (forM, forM_, mapAndUnzipM, unless, (>=>))
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
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

data Arm = LastStatement | ObservedResult
    deriving (Eq, Show)

armName :: Arm -> String
armName LastStatement = "last-statement"
armName ObservedResult = "observed-result"

probeTimeout, totalTimeout, executionLimit :: Int
probeTimeout = 5000000
totalTimeout = 60000000
executionLimit = 256

require :: (Show err) => String -> Either err a -> IO a
require label = either (die . ((label ++ ": ") ++) . show) pure

check :: String -> Bool -> IO ()
check message condition = unless condition (die message)

runResultBaseline :: Int -> FilePath -> IO ()
runResultBaseline pairs out = do
    capabilities <- getNumCapabilities
    (cases, failures) <- mapAndUnzipM (runFixture pairs) fixtures
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

runFixture :: Int -> Fixture -> IO (Value, [String])
runFixture pairs Fixture {..} = do
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
    policy <- require fixtureName (metricPolicyForReduction config 1)
    let run arm = withCandidateWorkdir $ \dir -> do
            -- Both arms collect location metadata. Only the seed selector differs.
            (remember, locate) <- newResultStatementTracker True
            start <- getMonotonicTimeNSec
            (preflight, observer) <- prepare remember fixtureInput
            end <- getMonotonicTimeNSec
            let select sql = do
                    candidateLocation <- locate sql
                    pure $ case arm of
                        LastStatement -> Nothing
                        ObservedResult -> candidateLocation
            result <-
                runEvaluationRequestWithObserver
                    probeTimeout
                    observer
                    EvaluationRequest
                        { requestOnProgress = const (pure ())
                        , requestMetricsPolicy = Just policy
                        , requestConfig =
                            EvaluationConfig
                                { evaluationCacheEnabled = True
                                , evaluationJobs = 1
                                , evaluationTotalTimeoutMicros = Just totalTimeout
                                , evaluationMaxOracleExecutions = Just executionLimit
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
                                reduceSQLWithResultSlicing select report config logger pool fixtureInput
                            pure (ReductionActionResult sql evidence)
                        }
            let sql = evaluatedSQL result
                metrics = evaluationMetrics result
                label = armName arm ++ ": "
            statements <- countStatements fixtureDialect sql >>= require fixtureName
            referenceAccepted <- matches expectedObserver sql
            let report =
                    object
                        [ "arm" .= armName arm
                        , "reduced_sql" .= decodeUtf8 sql
                        , "reduced_bytes" .= BS.length sql
                        , "reduced_statements" .= statements
                        , "reference_accepted" .= referenceAccepted
                        , "evaluation_metrics" .= metrics
                        ]
                failures =
                    [ fixtureName ++ ": " ++ label ++ message
                    | (passed, message) <-
                        [
                            ( evaluationStatus metrics == EvaluationCompleted
                            , "evaluation status " ++ show (evaluationStatus metrics, evaluationFailure metrics)
                            )
                        ,
                            ( evaluationTimedOutObservations metrics == 0 && evaluationOracleErrors metrics == 0
                            , "a probe timed out or failed"
                            )
                        , (evaluationAuditCompleted metrics, "statement minimality was not confirmed")
                        ,
                            ( evaluationValidationPassed metrics && evaluationValidationDatabaseObservations metrics == 2
                            , "expected two fresh, successful final validations"
                            )
                        , (referenceAccepted, "final SQL disagrees with the reference result")
                        , (BS.length sql < BS.length fixtureInput, "fixture should shrink")
                        ]
                    , not passed
                    ]
            pure (report, failures)
    -- NOTE: Warm both arms once. Keep cold-start effects out of measured samples.
    warmups <- mapM run [LastStatement, ObservedResult]
    samples <- forM [1 .. pairs] $ \pair -> do
        let arms = if odd pair then [LastStatement, ObservedResult] else [ObservedResult, LastStatement]
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

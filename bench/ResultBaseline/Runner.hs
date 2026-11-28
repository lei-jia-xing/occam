{-# LANGUAGE OverloadedRecordDot #-}

-- | Shared paired-baseline execution and result-oracle checks.
module ResultBaseline.Runner (
    Arm (..),
    serialArm,
    probeTimeout,
    require,
    check,
    searchCensoredStatuses,
    runComparison,
) where

import Control.Monad (forM, forM_, mapAndUnzipM, unless, when, (>=>))
import Data.Aeson (Value (Bool, Number), encode, object, (.=))
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (decodeUtf8)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Occam.AtomicFile (writeAtomicLazyBytes)
import Occam.Core (OracleOutcome (OracleRejected), ResultStatement (..), traceResultStatement)
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
    runEvaluationRequestWithNativeValidation,
    sqlDialectName,
 )
import Occam.Logger (Logger (..), defaultLogConfig)
import Occam.Oracle (
    OracleObservationDetails (..),
    observationOutcome,
    oracleObservationMatches,
    withCandidateWorkdir,
 )
import Occam.Oracle.Inline (newResultTraceTracker, prepareResultOracleWithValidation)
import Occam.Parser (ParsedSQL (..), countStatements, defaultParseOptions, parseSQL)
import Occam.Reducer (
    ReductionConfig,
    ReductionResult (..),
    defaultReductionConfigFor,
    reduceSQLWithResultTrace,
 )
import ResultBaseline.Fixtures (Fixture (..))
import System.Exit (die)
import System.FilePath ((</>))
import System.Info qualified as System

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

searchCensoredStatuses :: Maybe Int -> Maybe Int -> [EvaluationStatus]
searchCensoredStatuses wallLimit callLimit =
    [EvaluationTimeout | Just _ <- [wallLimit]] ++ [EvaluationExecutionBudget | Just _ <- [callLimit]]
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
    let config = defaultReductionConfigFor fixtureDialect
        prepare remember sql =
            prepareResultOracleWithValidation True Nothing remember fixtureDialect probeTimeout sql
                >>= require fixtureName
        assert = check . ((fixtureName ++ ": ") ++)
    parsed <- parseSQL (defaultParseOptions fixtureDialect) fixtureInput >>= require fixtureName
    let initialCount = length (parsedUnits parsed)
        graph = analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed)
    assert "unexpected global fence classification" (graphHasGlobalBarrier graph == fixtureFence)
    locationRef <- newIORef Nothing
    (_, inputObserver, _) <-
        prepare (\_ trace -> writeIORef locationRef (Just (traceResultStatement trace))) fixtureInput
    location <- readIORef locationRef
    assert "unexpected engine result location" (location == Just fixtureLocation)
    assert
        "engine/frontend statement count mismatch"
        (resultStatementCount fixtureLocation == initialCount)
    (_, _, expectedObserver) <- prepare (\_ _ -> pure ()) fixtureExpected
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
            -- Both arms collect trace metadata, including when the policy ignores it.
            (remember, lookupTrace) <- newResultTraceTracker True
            start <- getMonotonicTimeNSec
            (preflight, observer, validate) <- prepare remember fixtureInput
            end <- getMonotonicTimeNSec
            physicalCalls <- newIORef (1 :: Int)
            accepted <- newIORef []
            fallbackActivations <- newIORef (0 :: Int)
            fallbackSkips <- newIORef (0 :: Int)
            groupNoncontigAccepts <- newIORef (0 :: Int)
            groupContigAccepts <- newIORef (0 :: Int)
            earlyRejectCheckedRef <- newIORef (0 :: Int)
            earlyRejectExactRef <- newIORef (0 :: Int)
            earlyRejectNoResRef <- newIORef (0 :: Int)
            earlyRejectNanosRef <- newIORef (0 :: Integer)
            let counted observe started = observe $ do
                    atomicModifyIORef' physicalCalls (\calls -> (calls + 1, ()))
                    started
                getNumField :: (Eq k, Integral a) => k -> [(k, Value)] -> a
                getNumField key fields = case lookup key fields of
                    Just (Number n) -> round n
                    _ -> 0
                telemetryLogger =
                    Logger
                        defaultLogConfig
                        ( \_ _ msg fields -> do
                            when (msg == "Ranked statement fallback activated") $
                                modifyIORef' fallbackActivations (+ 1)
                            when (msg == "Ranked statement fallback skipped") $
                                modifyIORef' fallbackSkips (+ 1)
                            when (msg == "Dependency group deletion accepted") $
                                case lookup "noncontiguous" fields of
                                    Just (Bool True) -> modifyIORef' groupNoncontigAccepts (+ 1)
                                    _ -> modifyIORef' groupContigAccepts (+ 1)
                            when (msg == "Early rejection certificate summary") $ do
                                modifyIORef' earlyRejectCheckedRef (+ getNumField "checked" fields)
                                modifyIORef' earlyRejectExactRef (+ getNumField "exact_prefix_rejects" fields)
                                modifyIORef' earlyRejectNoResRef (+ getNumField "no_result_rejects" fields)
                                modifyIORef' earlyRejectNanosRef (+ getNumField "check_nanos" fields)
                        )
                        (pure ())
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
                    candidateTrace <- lookupTrace sql
                    pure (if armObservedResult arm then candidateTrace else Nothing)
            result <-
                runEvaluationRequestWithNativeValidation
                    probeTimeout
                    (counted observer)
                    (counted validate)
                    EvaluationRequest
                        { onProgress = progress
                        , metricsPolicy = Just policy
                        , config =
                            EvaluationConfig
                                { cacheEnabled = True
                                , jobs = armJobs arm
                                , totalTimeoutMicros = wallLimit
                                , maxOracleExecutions = callLimit
                                , preflightMicros = fromIntegral ((end - start) `div` 1000)
                                , preflightAttempts = 1
                                , preflightResetMicros = Just (resetElapsedMicros preflight)
                                , preflightOracleMicros = oracleElapsedMicros preflight
                                , initialStatements = Just initialCount
                                }
                        , dialect = fixtureDialect
                        , oracleConfig = ()
                        , preflightObservation = Just (detailedObservation preflight)
                        , logger = telemetryLogger
                        , initialSQL = fixtureInput
                        , checkpoint = dir </> "checkpoint.sql"
                        , action = \report logger pool -> do
                            ReductionResult sql evidence <-
                                reduceSQLWithResultTrace select report reductionConfig logger pool fixtureInput
                            pure (ReductionActionResult sql evidence)
                        , collectCurve = True
                        }
            let sql = result.sql
                metrics = result.metrics
                label = armName arm ++ ": "
                completed = metrics.runStatus == EvaluationCompleted
                censored = metrics.runStatus `elem` censoredStatuses
            statements <- countStatements fixtureDialect sql >>= require fixtureName
            referenceAccepted <- matches expectedObserver sql
            executions <- readIORef physicalCalls
            curve <- reverse <$> readIORef accepted
            activations <- readIORef fallbackActivations
            skips <- readIORef fallbackSkips
            noncontigAccepts <- readIORef groupNoncontigAccepts
            contigAccepts <- readIORef groupContigAccepts
            erChecked <- readIORef earlyRejectCheckedRef
            erExact <- readIORef earlyRejectExactRef
            erNoRes <- readIORef earlyRejectNoResRef
            erNanos <- readIORef earlyRejectNanosRef
            let report =
                    object
                        [ "arm" .= armName arm
                        , "reduced_sql" .= decodeUtf8 sql
                        , "reduced_bytes" .= BS.length sql
                        , "reduced_statements" .= statements
                        , "reference_accepted" .= referenceAccepted
                        , "physical_oracle_executions" .= executions
                        , "ranked_fallback_activations" .= activations
                        , "ranked_fallback_skips" .= skips
                        , "group_noncontiguous_accepts" .= noncontigAccepts
                        , "group_contiguous_accepts" .= contigAccepts
                        , "early_reject_checked" .= erChecked
                        , "early_reject_exact_prefix" .= erExact
                        , "early_reject_no_result" .= erNoRes
                        , "early_reject_check_micros" .= (fromIntegral erNanos / (1000.0 :: Double))
                        , "best_accepted_curve" .= curve
                        , "evaluation_metrics" .= metrics
                        ]
                failures =
                    [ fixtureName ++ ": " ++ label ++ message
                    | (passed, message) <-
                        [
                            ( completed || censored
                            , "evaluation status " ++ show (metrics.runStatus, metrics.failure)
                            )
                        ,
                            ( metrics.timedOutObservations == 0 && metrics.oracleErrors == 0
                            , "a probe timed out or failed"
                            )
                        , (censored || metrics.auditCompleted, "statement minimality was not confirmed")
                        ,
                            ( metrics.validationPassed && metrics.validationDatabaseObservations == 2
                            , "expected two fresh, successful final validations"
                            )
                        , (referenceAccepted, "final SQL disagrees with the reference result")
                        ,
                            ( executions
                                == 1 + metrics.databaseObservations + metrics.validationDatabaseObservations
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

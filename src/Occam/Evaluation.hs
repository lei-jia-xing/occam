module Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationObserver,
    EvaluationPoint (..),
    EvaluationProgress (..),
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    MinimalityClaim (..),
    ReductionActionResult (..),
    Stats (..),
    evaluationStatusName,
    metricPolicyForReduction,
    minimalityClaimName,
    parseSQLDialect,
    runEvaluatedReduction,
    runEvaluatedReductionWithProgress,
    runEvaluationRequest,
    runEvaluationRequestWithObserver,
    runEvaluationRequestWithNativeObserver,
    runEvaluationRequestWithNativeValidation,
    sqlDialectName,
) where

import Control.Exception.Safe (displayException, fromException, toException)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Occam.AtomicFile (writeAtomicBytes)
import Occam.Cache (CacheStats (..), nativeCandidateKey)
import Occam.Evaluation.Execution (
    EvalEnv (..),
    EvalState (..),
    evalStageReporter,
    initialEvalState,
    modifyEvalState,
    recordReductionObservation,
    retireCandidate,
    runReductionPhase,
    runValidationPhase,
 )
import Occam.Evaluation.Types (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationObserver,
    EvaluationPoint (..),
    EvaluationProgress (..),
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    MinimalityClaim (..),
    ReductionAction,
    ReductionActionResult (..),
    Stats (..),
    evaluationStatusName,
    metricPolicyForReduction,
    minimalityClaimName,
    parseSQLDialect,
    seconds,
    sqlDialectName,
    ticks,
 )
import Occam.Logger (Logger)
import Occam.Metrics (MetricPolicy, newMetricsStore, snapshotMetrics)
import Occam.Oracle (
    OracleConfig (..),
    OracleObservation,
    OracleReapFailed (..),
    observeExternalOracleDetailedWithContext,
    resolveOracleContext,
 )
import Occam.Parser (SQLDialect, countStatements, nativeOracleCacheKey)
import Occam.Pool (
    ExecutionBudgetReached (..),
    PoolConfig (..),
    closeOraclePool,
    newOraclePoolWithCacheKey,
    poolAttempts,
    poolCacheStats,
    poolCancelled,
    withAdaptiveLookahead,
    withCandidateCountCleanup,
 )

-- | Execute an evaluated reduction workflow.
runEvaluatedReduction ::
    Maybe MetricPolicy ->
    EvaluationConfig ->
    SQLDialect ->
    OracleConfig ->
    Maybe OracleObservation ->
    Logger ->
    ByteString ->
    FilePath ->
    ReductionAction ->
    IO EvaluationResult
runEvaluatedReduction = runEvaluatedReductionWithProgress (\_ -> pure ())

runEvaluatedReductionWithProgress ::
    (EvaluationProgress -> IO ()) ->
    Maybe MetricPolicy ->
    EvaluationConfig ->
    SQLDialect ->
    OracleConfig ->
    Maybe OracleObservation ->
    Logger ->
    ByteString ->
    FilePath ->
    ReductionAction ->
    IO EvaluationResult
runEvaluatedReductionWithProgress onProgress metricsPolicy config dialect oracleCfg preflightObservation logger initialSQL checkpoint action =
    runEvaluationRequest
        EvaluationRequest
            { requestOnProgress = onProgress
            , requestMetricsPolicy = metricsPolicy
            , requestConfig = config
            , requestDialect = dialect
            , requestOracleConfig = oracleCfg
            , requestPreflightObservation = preflightObservation
            , requestLogger = logger
            , requestInitialSQL = initialSQL
            , requestCheckpoint = checkpoint
            , requestAction = action
            , requestCollectCurve = True
            }

runEvaluationRequest :: EvaluationRequest OracleConfig -> IO EvaluationResult
runEvaluationRequest request = do
    let config = requestOracleConfig request
    context <- resolveOracleContext config
    let observe started = observeExternalOracleDetailedWithContext started config context
    runEvaluationRequestWithObserver (oracleTimeoutMicros config) observe request

runEvaluationRequestWithObserver ::
    Int -> EvaluationObserver -> EvaluationRequest oracle -> IO EvaluationResult
runEvaluationRequestWithObserver micros observe =
    runEvaluationRequestWithKey False pure micros observe observe

runEvaluationRequestWithNativeObserver ::
    Int -> EvaluationObserver -> EvaluationRequest oracle -> IO EvaluationResult
runEvaluationRequestWithNativeObserver micros observe =
    runEvaluationRequestWithNativeValidation micros observe observe

runEvaluationRequestWithNativeValidation ::
    Int -> EvaluationObserver -> EvaluationObserver -> EvaluationRequest oracle -> IO EvaluationResult
runEvaluationRequestWithNativeValidation micros observe validate request =
    runEvaluationRequestWithKey
        True
        (nativeCandidateKey (nativeOracleCacheKey (requestDialect request)))
        micros
        observe
        validate
        request

runEvaluationRequestWithKey ::
    Bool ->
    (ByteString -> IO ByteString) ->
    Int ->
    EvaluationObserver ->
    EvaluationObserver ->
    EvaluationRequest oracle ->
    IO EvaluationResult
runEvaluationRequestWithKey adaptive candidateKey timeoutMicros observe validate EvaluationRequest {..} = do
    let onProgress = requestOnProgress
        metricsPolicy = requestMetricsPolicy
        config = requestConfig
        dialect = requestDialect
        preflightObservation = requestPreflightObservation
        parentLogger = requestLogger
        initialSQL = requestInitialSQL
        checkpoint = requestCheckpoint
        action = requestAction
        collectCurve = requestCollectCurve
    when (evaluationJobs config <= 0) $
        ioError (userError ("Evaluation jobs must be positive, got " ++ show (evaluationJobs config)))
    startedAt <- ticks
    let preflightMicros = max 0 (toInteger (evaluationPreflightMicros config))
        totalStart = startedAt - preflightMicros * 1000
    seedCounts <- case evaluationInitialStatements config of
        Just count -> pure (Map.singleton initialSQL count)
        Nothing -> do
            counted <- countStatements dialect initialSQL
            pure $ case counted of
                Left _ -> Map.empty
                Right count -> Map.singleton initialSQL count
    state <- newIORef (initialEvalState initialSQL seedCounts)
    unless (null checkpoint) $ writeAtomicBytes checkpoint initialSQL
    let oracleTimeout = max 1 (toInteger timeoutMicros)
        validationSlack = max 1000 (min 10000 (oracleTimeout `div` 100))
        perValidationReserve = oracleTimeout + validationSlack
        remainingCalls =
            (\limit -> max 0 (limit - max 0 (evaluationPreflightAttempts config)))
                <$> evaluationMaxOracleExecutions config
        callsToReserve = maybe 2 (min 2 . max 0 . subtract 1) remainingCalls
        timeToReserve = fromInteger $ case evaluationTotalTimeoutMicros config of
            Nothing -> 2
            Just limit ->
                let remaining = toInteger limit - preflightMicros
                 in min 2 (max 0 (remaining - oracleTimeout) `div` perValidationReserve)
        validationCallReserve = min callsToReserve timeToReserve
        validationReserve = case evaluationTotalTimeoutMicros config of
            Nothing -> 0
            Just _ -> toInteger validationCallReserve * perValidationReserve
        totalDeadline =
            (\limit -> totalStart + max 0 (toInteger limit) * 1000)
                <$> evaluationTotalTimeoutMicros config
        reductionCallLimit = subtract validationCallReserve <$> remainingCalls
        precomputedObservations =
            maybe Map.empty (Map.singleton initialSQL) preflightObservation
    reductionStart <- ticks
    metricsStore <- traverse newMetricsStore metricsPolicy
    rawPool <-
        newOraclePoolWithCacheKey
            candidateKey
            PoolConfig
                { poolJobs = evaluationJobs config
                , poolCacheEnabled = evaluationCacheEnabled config
                , poolPermits = reductionCallLimit
                , poolPrecomputed = precomputedObservations
                , poolMetrics = metricsStore
                , poolExecute =
                    observe (modifyEvalState state (\s -> s {evalDatabase = evalDatabase s + 1}))
                , poolBill = recordReductionObservation state
                , poolRetire = retireCandidate onProgress reductionStart checkpoint state collectCurve
                , poolReportCount =
                    Just
                        (\bytes count -> modifyEvalState state (\s -> s {evalCounts = Map.insert bytes count (evalCounts s)}))
                }
    configuredPool <- if adaptive then withAdaptiveLookahead rawPool else pure rawPool
    let pool =
            withCandidateCountCleanup (modifyEvalState state (\s -> s {evalCounts = seedCounts})) configuredPool
        env =
            EvalEnv
                { envDialect = dialect
                , envValidate = validate
                , envLogger = parentLogger
                , envReductionStart = reductionStart
                , envTotalDeadline = totalDeadline
                , envValidationReserve = validationReserve
                }
        reporter = evalStageReporter onProgress state
    (reductionOutcome, reductionSeconds, costAttribution) <-
        if remainingCalls == Just 0
            then do
                closeOraclePool pool
                costs <- if collectCurve then traverse snapshotMetrics metricsStore else pure Nothing
                pure (Left (toException (ExecutionBudgetReached ())), 0, costs)
            else runReductionPhase env state pool metricsStore collectCurve reporter action
    (status, failure, skipValidation) <- case reductionOutcome of
        Right (Just _) -> pure (EvaluationCompleted, Nothing, False)
        Right Nothing -> pure (EvaluationTimeout, Just "Shared wall-clock budget expired during reduction", False)
        Left err
            | Just (ExecutionBudgetReached _) <- fromException err ->
                pure (EvaluationExecutionBudget, Nothing, False)
            | Just OracleReapFailed <- fromException err ->
                pure
                    ( EvaluationError
                    , Just "Oracle process group could not be reaped; refusing further oracle executions"
                    , True
                    )
            | otherwise -> pure (EvaluationError, Just (T.pack (displayException err)), False)
    preValidation <- readIORef state
    let acceptedFinal = evalAccepted preValidation
        auditResult = evalAuditResult preValidation
        finalSQL = fromMaybe acceptedFinal auditResult
        auditResultIsFinal =
            case auditResult of
                Just result -> result == finalSQL
                Nothing -> False
    attempts <- poolAttempts pool
    let validationCallLimit = maybe 2 (min 2 . max 0 . subtract attempts) remainingCalls
    validationPassed <- runValidationPhase env state finalSQL skipValidation validationCallLimit
    cacheStats <- poolCacheStats pool
    cancelled <- poolCancelled pool
    st <- readIORef state
    let curve = reverse (evalCurve st)
        stages = reverse (evalStages st)
        database = evalDatabase st
        completed = evalCompleted st
        acceptedCount = evalAcceptedCount st
        rejectedCount = evalRejectedCount st
        timedOut = evalTimedOut st
        resetFailures = evalResetFailures st
        errors = evalErrors st
        outcomes = evalOutcomes st
        validationObservations = evalValidationObs st
        validationDatabase = evalValidationDatabase st
        validationErrors = evalValidationErrors st
        validationRejected = evalValidationRejected st
        validationOutcomes = evalValidationOutcomes st
        oracleSeconds = evalOracleSeconds st
        resetSeconds = evalResetSeconds st
        validationOracleSeconds = evalValidationOracleSeconds st
        validationResetSeconds = evalValidationResetSeconds st
        validationSeconds = evalValidationSeconds st
        auditSeconds = evalAuditSeconds st
        auditChecks = evalAuditChecks st
        auditCompleted = evalAuditCompleted st
        auditSkipped = evalAuditSkipped st
    totalEnd <- ticks
    let finalStatus
            | status /= EvaluationCompleted = status
            | not validationPassed && validationRejected = EvaluationValidationFailed
            | not validationPassed && validationObservations < 2 = EvaluationValidationIncomplete
            | not validationPassed = EvaluationValidationFailed
            | otherwise = EvaluationCompleted
        finalFailure =
            case failure of
                Just err -> Just err
                Nothing
                    | validationRejected ->
                        Just "Final candidate failed target validation"
                Nothing
                    | not validationPassed && validationObservations < 2 ->
                        Just "Repeated final validation did not complete within the shared limits"
                Nothing
                    | not validationPassed ->
                        Just "Final candidate failed repeated target validation"
                Nothing -> Nothing
        metrics =
            EvaluationMetrics
                { evaluationStatus = finalStatus
                , evaluationFailure = finalFailure
                , evaluationCacheUsed = evaluationCacheEnabled config
                , evaluationBudget = config
                , evaluationTotalSeconds = seconds totalStart totalEnd
                , evaluationReductionSeconds = reductionSeconds
                , evaluationOracleSeconds = oracleSeconds
                , evaluationResetSeconds = resetSeconds
                , evaluationPreflightResetSeconds =
                    fmap ((/ 1000000) . fromIntegral) (evaluationPreflightResetMicros config)
                , evaluationPreflightOracleSeconds =
                    fmap ((/ 1000000) . fromIntegral) (evaluationPreflightOracleMicros config)
                , evaluationOracleAttempts = attempts
                , evaluationDatabaseObservations = database
                , evaluationCompletedOracleProcesses = completed
                , evaluationAcceptedObservations = acceptedCount
                , evaluationRejectedObservations = rejectedCount
                , evaluationTimedOutObservations = timedOut
                , evaluationResetFailures = resetFailures
                , evaluationOracleErrors = errors
                , evaluationCancelledObservations = cancelled
                , evaluationOracleOutcomes = outcomes
                , evaluationCacheHits = cacheHits cacheStats
                , evaluationCacheMisses = cacheMisses cacheStats
                , evaluationCacheJoins = cacheJoins cacheStats
                , evaluationValidationSeconds = validationSeconds
                , evaluationValidationOracleSeconds = validationOracleSeconds
                , evaluationValidationResetSeconds = validationResetSeconds
                , evaluationValidationObservations = validationObservations
                , evaluationValidationDatabaseObservations = validationDatabase
                , evaluationValidationErrors = validationErrors
                , evaluationValidationOutcomes = validationOutcomes
                , evaluationValidationPassed = validationPassed
                , evaluationAuditSeconds = auditSeconds
                , evaluationAuditChecks = auditChecks
                , evaluationAuditCompleted = auditCompleted
                , evaluationAuditSkipped = auditSkipped
                , evaluationMinimalityClaim =
                    if validationPassed && auditCompleted && auditResultIsFinal
                        then StatementDeletionOneMinimal
                        else MinimalityNotEstablished
                , evaluationCurve = if collectCurve then curve else []
                , evaluationStages = stages
                , evaluationCostAttribution = costAttribution
                }
    pure
        EvaluationResult
            { evaluatedSQL = finalSQL
            , evaluatedStatements =
                if finalSQL == evalAccepted st
                    then evalAcceptedStatements st
                    else Nothing
            , evaluationMetrics = metrics
            }

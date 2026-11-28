{-# LANGUAGE OverloadedRecordDot #-}

-- | Budgeted reduction session orchestration, preflight checking, and validation phases.
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

import Control.Exception (SomeException)
import Control.Exception.Safe (displayException, fromException, toException)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Occam.AtomicFile (writeAtomicBytes)
import Occam.Cache (
    CacheStats (cacheHits, cacheJoins, cacheMisses),
    nativeCandidateKey,
 )
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

-- | Execute an evaluated reduction session while streaming 'EvaluationProgress' callbacks.
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
            { onProgress = onProgress
            , metricsPolicy = metricsPolicy
            , config = config
            , dialect = dialect
            , oracleConfig = oracleCfg
            , preflightObservation = preflightObservation
            , logger = logger
            , initialSQL = initialSQL
            , checkpoint = checkpoint
            , action = action
            , collectCurve = True
            }

-- | Run an 'EvaluationRequest' against an external subprocess oracle configuration.
runEvaluationRequest :: EvaluationRequest OracleConfig -> IO EvaluationResult
runEvaluationRequest request = do
    let config = request.oracleConfig
    context <- resolveOracleContext config
    let observe started = observeExternalOracleDetailedWithContext started config context
    runEvaluationRequestWithObserver (oracleTimeoutMicros config) observe request

-- | Run an 'EvaluationRequest' with a caller-supplied 'EvaluationObserver'.
runEvaluationRequestWithObserver ::
    Int -> EvaluationObserver -> EvaluationRequest oracle -> IO EvaluationResult
runEvaluationRequestWithObserver micros observe =
    runEvaluationRequestWithKey pure micros observe observe

-- | Run an 'EvaluationRequest' using an in-process native observer and native cache-key canonicalization.
runEvaluationRequestWithNativeObserver ::
    Int -> EvaluationObserver -> EvaluationRequest oracle -> IO EvaluationResult
runEvaluationRequestWithNativeObserver micros observe =
    runEvaluationRequestWithNativeValidation micros observe observe

-- | Like 'runEvaluationRequestWithNativeObserver', supplying a separate clean observer for final validation.
runEvaluationRequestWithNativeValidation ::
    Int -> EvaluationObserver -> EvaluationObserver -> EvaluationRequest oracle -> IO EvaluationResult
runEvaluationRequestWithNativeValidation micros observe validate request =
    runEvaluationRequestWithKey
        (nativeCandidateKey (nativeOracleCacheKey request.dialect))
        micros
        observe
        validate
        request

runEvaluationRequestWithKey ::
    (ByteString -> IO ByteString) ->
    Int ->
    EvaluationObserver ->
    EvaluationObserver ->
    EvaluationRequest oracle ->
    IO EvaluationResult
runEvaluationRequestWithKey candidateKey timeoutMicros observe validate request = do
    let onProgress = request.onProgress
        metricsPolicy = request.metricsPolicy
        config = request.config
        dialect = request.dialect
        preflightObservation = request.preflightObservation
        parentLogger = request.logger
        initialSQL = request.initialSQL
        checkpoint = request.checkpoint
        action = request.action
        collectCurve = request.collectCurve
    when (config.jobs <= 0) $
        ioError (userError ("Evaluation jobs must be positive, got " ++ show config.jobs))
    startedAt <- ticks
    let preflightMicros = max 0 (toInteger config.preflightMicros)
        totalStart = startedAt - preflightMicros * 1000
    seedCounts <- seedStatementCounts dialect config.initialStatements initialSQL
    state <- newIORef (initialEvalState initialSQL seedCounts)
    unless (null checkpoint) $ writeAtomicBytes checkpoint initialSQL
    let oracleTimeout = max 1 (toInteger timeoutMicros)
        validationSlack = max 1000 (min 10000 (oracleTimeout `div` 100))
        perValidationReserve = oracleTimeout + validationSlack
        remainingCalls =
            (\limit -> max 0 (limit - max 0 config.preflightAttempts))
                <$> config.maxOracleExecutions
        callsToReserve = maybe 2 (min 2 . max 0 . subtract 1) remainingCalls
        timeToReserve = fromInteger $ case config.totalTimeoutMicros of
            Nothing -> 2
            Just limit ->
                let remaining = toInteger limit - preflightMicros
                 in min 2 (max 0 (remaining - oracleTimeout) `div` perValidationReserve)
        validationCallReserve = min callsToReserve timeToReserve
        validationReserve = case config.totalTimeoutMicros of
            Nothing -> 0
            Just _ -> toInteger validationCallReserve * perValidationReserve
        totalDeadline =
            (\limit -> totalStart + max 0 (toInteger limit) * 1000)
                <$> config.totalTimeoutMicros
        reductionCallLimit = subtract validationCallReserve <$> remainingCalls
        precomputedObservations =
            maybe Map.empty (Map.singleton initialSQL) preflightObservation
    reductionStart <- ticks
    metricsStore <- traverse newMetricsStore metricsPolicy
    rawPool <-
        newOraclePoolWithCacheKey
            candidateKey
            PoolConfig
                { poolJobs = config.jobs
                , poolCacheEnabled = config.cacheEnabled
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
    let pool =
            withCandidateCountCleanup (modifyEvalState state (\s -> s {evalCounts = seedCounts})) rawPool
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
    let (status, failure, skipValidation) = classifyReductionOutcome reductionOutcome
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
    let finalStatus =
            finalEvaluationStatus status validationPassed validationRejected validationObservations
        finalFailure =
            finalEvaluationFailure failure validationRejected validationPassed validationObservations
        metrics =
            EvaluationMetrics
                { runStatus = finalStatus
                , failure = finalFailure
                , cacheUsed = config.cacheEnabled
                , budget = config
                , totalSeconds = seconds totalStart totalEnd
                , reductionSeconds = reductionSeconds
                , oracleSeconds = oracleSeconds
                , resetSeconds = resetSeconds
                , preflightResetSeconds =
                    fmap ((/ 1000000) . fromIntegral) config.preflightResetMicros
                , preflightOracleSeconds =
                    fmap ((/ 1000000) . fromIntegral) config.preflightOracleMicros
                , oracleAttempts = attempts
                , databaseObservations = database
                , completedOracleProcesses = completed
                , acceptedObservations = acceptedCount
                , rejectedObservations = rejectedCount
                , timedOutObservations = timedOut
                , resetFailures = resetFailures
                , oracleErrors = errors
                , cancelledObservations = cancelled
                , oracleOutcomes = outcomes
                , cacheHits = cacheStats.cacheHits
                , cacheMisses = cacheStats.cacheMisses
                , cacheJoins = cacheStats.cacheJoins
                , validationSeconds = validationSeconds
                , validationOracleSeconds = validationOracleSeconds
                , validationResetSeconds = validationResetSeconds
                , validationObservations = validationObservations
                , validationDatabaseObservations = validationDatabase
                , validationErrors = validationErrors
                , validationOutcomes = validationOutcomes
                , validationPassed = validationPassed
                , auditSeconds = auditSeconds
                , auditChecks = auditChecks
                , auditCompleted = auditCompleted
                , auditSkipped = auditSkipped
                , minimalityClaim =
                    if validationPassed && auditCompleted && auditResultIsFinal
                        then StatementDeletionOneMinimal
                        else MinimalityNotEstablished
                , curve = if collectCurve then curve else []
                , stages = stages
                , costAttribution = costAttribution
                }
    pure
        EvaluationResult
            { sql = finalSQL
            , statements =
                if finalSQL == evalAccepted st
                    then evalAcceptedStatements st
                    else Nothing
            , metrics = metrics
            }
seedStatementCounts :: SQLDialect -> Maybe Int -> ByteString -> IO (Map.Map ByteString Int)
seedStatementCounts dialect maybeCount initialSQL = case maybeCount of
    Just count -> pure (Map.singleton initialSQL count)
    Nothing -> do
        counted <- countStatements dialect initialSQL
        pure $ case counted of
            Left _ -> Map.empty
            Right count -> Map.singleton initialSQL count

classifyReductionOutcome ::
    Either SomeException (Maybe result) ->
    (EvaluationStatus, Maybe Text, Bool)
classifyReductionOutcome outcome = case outcome of
    Right (Just _) -> (EvaluationCompleted, Nothing, False)
    Right Nothing -> (EvaluationTimeout, Just "Shared wall-clock budget expired during reduction", False)
    Left err
        | Just (ExecutionBudgetReached _) <- fromException err ->
            (EvaluationExecutionBudget, Nothing, False)
        | Just OracleReapFailed <- fromException err ->
            ( EvaluationError
            , Just "Oracle process group could not be reaped; refusing further oracle executions"
            , True
            )
        | otherwise -> (EvaluationError, Just (T.pack (displayException err)), False)

finalEvaluationStatus :: EvaluationStatus -> Bool -> Bool -> Int -> EvaluationStatus
finalEvaluationStatus status validationPassed validationRejected validationObservations
    | status /= EvaluationCompleted = status
    | not validationPassed && validationRejected = EvaluationValidationFailed
    | not validationPassed && validationObservations < 2 = EvaluationValidationIncomplete
    | not validationPassed = EvaluationValidationFailed
    | otherwise = EvaluationCompleted

finalEvaluationFailure :: Maybe Text -> Bool -> Bool -> Int -> Maybe Text
finalEvaluationFailure failure validationRejected validationPassed validationObservations =
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

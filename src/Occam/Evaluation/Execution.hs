{-# LANGUAGE OverloadedRecordDot #-}

-- | Stateful execution loop for reduction, validation, and post-reduction statement auditing.
module Occam.Evaluation.Execution (
    EvalEnv (..),
    EvalState (..),
    boundedTimeout,
    evalStageReporter,
    initialEvalState,
    modifyEvalState,
    recordReductionObservation,
    retireCandidate,
    runReductionPhase,
    runValidationPhase,
) where

import Control.Exception.Safe (SomeException, finally, mask_, try)
import Control.Monad (guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Occam.AtomicFile (writeAtomicBytes)
import Occam.Core.Oracle (OracleOutcome (..))
import Occam.Evaluation.Types (
    EvaluationObserver,
    EvaluationPoint (..),
    EvaluationProgress (..),
    ReductionAction,
    ReductionActionResult (..),
    Ticks,
    seconds,
    ticks,
 )
import Occam.Logger (Logger, logDebug', logInfo')
import Occam.Metrics (MetricsSnapshot, MetricsStore, snapshotMetrics)
import Occam.Oracle (
    OracleObservation (..),
    OracleObservationDetails (..),
    OracleStatus (..),
    classifyOracleObservation,
    observationOutcome,
    oracleObservationMatches,
 )
import Occam.Parser (SQLDialect)
import Occam.Pool (OraclePool, closeOraclePool, mapPoolRetire)
import Occam.Reducer (
    AuditStats (..),
    ReductionEvidence (..),
    ReductionStage (..),
    StageEvent (..),
    StageReporter,
    reductionStageName,
    statementAuditWithLogger,
 )
import System.Timeout (timeout)

-- | Mutable reduction state shared across all execution phases.
data EvalState = EvalState
    { evalAccepted :: !ByteString
    , evalAcceptedStatements :: !(Maybe Int)
    , evalDatabase :: !Int
    , evalCompleted :: !Int
    , evalAcceptedCount :: !Int
    , evalRejectedCount :: !Int
    , evalTimedOut :: !Int
    , evalResetFailures :: !Int
    , evalErrors :: !Int
    , evalOutcomes :: !(Map OracleStatus Int)
    , evalOracleSeconds :: !Double
    , evalResetSeconds :: !Double
    , evalValidationObs :: !Int
    , evalValidationDatabase :: !Int
    , evalValidationErrors :: !Int
    , evalValidationOutcomes :: !(Map OracleStatus Int)
    , evalValidationRejected :: !Bool
    , evalValidationOracleSeconds :: !Double
    , evalValidationResetSeconds :: !Double
    , evalValidationSeconds :: !Double
    , evalCurve :: ![EvaluationPoint]
    , evalStages :: ![Value]
    , evalCounts :: !(Map ByteString Int)
    , evalAuditSeconds :: !Double
    , evalAuditChecks :: !Int
    , evalAuditInitialAccepted :: !Bool
    , evalAuditCompleted :: !Bool
    , evalAuditSkipped :: !Bool
    , evalAuditUnchanged :: !Bool
    , evalAuditResult :: !(Maybe ByteString)
    }
    deriving (Eq, Show)

-- | Construct initial 'EvalState' seeded with the original SQL input and cached statement counts.
initialEvalState :: ByteString -> Map ByteString Int -> EvalState
initialEvalState initialSQL seedCounts =
    EvalState
        { evalAccepted = initialSQL
        , evalAcceptedStatements = Map.lookup initialSQL seedCounts
        , evalDatabase = 0
        , evalCompleted = 0
        , evalAcceptedCount = 0
        , evalRejectedCount = 0
        , evalTimedOut = 0
        , evalResetFailures = 0
        , evalErrors = 0
        , evalOutcomes = Map.empty
        , evalOracleSeconds = 0
        , evalResetSeconds = 0
        , evalValidationObs = 0
        , evalValidationDatabase = 0
        , evalValidationErrors = 0
        , evalValidationOutcomes = Map.empty
        , evalValidationRejected = False
        , evalValidationOracleSeconds = 0
        , evalValidationResetSeconds = 0
        , evalValidationSeconds = 0
        , evalCurve = []
        , evalStages = []
        , evalCounts = seedCounts
        , evalAuditSeconds = 0
        , evalAuditChecks = 0
        , evalAuditInitialAccepted = False
        , evalAuditCompleted = False
        , evalAuditSkipped = False
        , evalAuditUnchanged = False
        , evalAuditResult = Nothing
        }

-- | Atomically apply a pure state transition to the shared 'EvalState' reference.
modifyEvalState :: IORef EvalState -> (EvalState -> EvalState) -> IO ()
modifyEvalState ref f = atomicModifyIORef' ref (\s -> (f s, ()))

-- | Static phase environment: handles, clocks, and budgets fixed at setup.
data EvalEnv = EvalEnv
    { envDialect :: !SQLDialect
    , envValidate :: !EvaluationObserver
    , envLogger :: !Logger
    , envReductionStart :: !Ticks
    , envTotalDeadline :: !(Maybe Ticks)
    , envValidationReserve :: !Integer
    }

-- | Clamp a microsecond budget safely into the @Int@ range expected by 'System.Timeout.timeout'.
boundedTimeout :: Integer -> Int
boundedTimeout micros =
    fromInteger (max 0 (min micros (toInteger (maxBound :: Int))))

-- | Execute the main reduction action and audit phase under budget constraints.
runReductionPhase ::
    EvalEnv ->
    IORef EvalState ->
    OraclePool ->
    Maybe MetricsStore ->
    Bool ->
    StageReporter ->
    ReductionAction ->
    IO (Either SomeException (Maybe ReductionActionResult), Double, Maybe MetricsSnapshot)
runReductionPhase env state pool metricsStore collectCurve reporter action = do
    reductionBudget <- case envTotalDeadline env of
        Nothing -> pure Nothing
        Just deadline -> do
            now <- ticks
            let remaining = max 0 ((deadline - now) `div` 1000)
                available = max 0 (remaining - envValidationReserve env)
            pure (Just available)
    outcome <-
        try $ case reductionBudget of
            Nothing -> Just <$> (runAction `finally` closeOraclePool pool)
            Just micros -> timeout (boundedTimeout micros) (runAction `finally` closeOraclePool pool)
    reductionEnd <- ticks
    let reductionSeconds = seconds (envReductionStart env) reductionEnd
    -- Snapshot cost attribution after workers are drained when requested.
    costAttribution <- if collectCurve then traverse snapshotMetrics metricsStore else pure Nothing
    pure (outcome, reductionSeconds, costAttribution)
    where
        runAction = do
            result <- action reporter (envLogger env) pool
            runAuditPhase env state pool reporter result.evidence
            pure result

-- | Run statement-deletion audit to confirm 1-minimality on the accepted result.
runAuditPhase ::
    EvalEnv -> IORef EvalState -> OraclePool -> StageReporter -> Maybe ReductionEvidence -> IO ()
runAuditPhase env state pool reporter evidence = do
    auditInput <- evalAccepted <$> readIORef state
    case evidence of
        Just ReductionEvidence {..}
            | evidenceAuditMinimal && evidenceBytes == auditInput ->
                skipAuditPhase env state auditInput
        _ -> executeAuditPhase env state pool reporter auditInput

skipAuditPhase :: EvalEnv -> IORef EvalState -> ByteString -> IO ()
skipAuditPhase env state auditInput = do
    modifyEvalState state \s ->
        s
            { evalAuditSeconds = 0
            , evalAuditCompleted = True
            , evalAuditSkipped = True
            , evalAuditUnchanged = True
            , evalAuditInitialAccepted = True
            , evalAuditResult = Just auditInput
            }
    logDebug'
        (envLogger env)
        "audit"
        "Shared statement audit skipped (already confirmed 1-minimal)"
        [ ("audit_skipped", toJSON True)
        , ("audit_minimality_confirmed", toJSON True)
        ]

executeAuditPhase ::
    EvalEnv -> IORef EvalState -> OraclePool -> StageReporter -> ByteString -> IO ()
executeAuditPhase env state pool reporter auditInput = do
    auditStart <- ticks
    let auditPool = mapPoolRetire recordAuditRetire pool
    reporter (StageStarted StageStatementAudit)
    result <- runAuditBody auditPool `finally` finishAudit auditStart
    auditEnd <- ticks
    reporter
        ( StageFinished
            StageStatementAudit
            (seconds auditStart auditEnd)
            (BS.length auditInput)
            (BS.length result)
        )
    where
        recordAuditRetire retire index candidate outcome hit = do
            priorChecks <- evalAuditChecks <$> readIORef state
            modifyEvalState state (\s -> s {evalAuditChecks = evalAuditChecks s + 1})
            when (priorChecks == 0) $
                modifyEvalState state (\s -> s {evalAuditInitialAccepted = outcome == OracleAccepted})
            retire index candidate outcome hit

        runAuditBody auditPool = do
            (audited, stats) <-
                statementAuditWithLogger (envLogger env) (envDialect env) auditPool auditInput
            initialAccepted <- evalAuditInitialAccepted <$> readIORef state
            modifyEvalState state \s ->
                s
                    { evalAuditCompleted = auditMinimalityConfirmed stats
                    , evalAuditUnchanged = initialAccepted && audited == auditInput
                    , evalAuditResult = Just audited
                    }
            pure audited

        finishAudit auditStart = do
            auditEnd <- ticks
            let elapsed = seconds auditStart auditEnd
            st <- readIORef state
            modifyEvalState state (\s -> s {evalAuditSeconds = elapsed})
            logInfo'
                (envLogger env)
                "audit"
                "Shared statement audit finished"
                [ ("elapsed_seconds", toJSON elapsed)
                , ("candidate_checks", toJSON (evalAuditChecks st))
                , ("completed", toJSON (evalAuditCompleted st))
                , ("candidate_unchanged", toJSON (evalAuditUnchanged st))
                , ("audit_minimality_confirmed", toJSON (evalAuditCompleted st))
                , ("skipped", toJSON False)
                ]

-- | Validation phase: repeat the final check twice within the shared limits.
runValidationPhase :: EvalEnv -> IORef EvalState -> ByteString -> Bool -> Int -> IO Bool
runValidationPhase env state finalSQL skipValidation callLimit
    | skipValidation = pure False
    | otherwise = do
        first <- validateOnce env state callLimit finalSQL
        second <- validateOnce env state callLimit finalSQL
        pure (first == Just OracleAccepted && second == Just OracleAccepted)

validateOnce :: EvalEnv -> IORef EvalState -> Int -> ByteString -> IO (Maybe OracleOutcome)
validateOnce env state callLimit candidate = runMaybeT do
    prior <- liftIO $ evalValidationObs <$> readIORef state
    guard (prior < callLimit)
    start <- liftIO ticks
    let remainingMicros = (`div` 1000) . subtract start <$> envTotalDeadline env
    guard (maybe True (> 0) remainingMicros)
    liftIO $ executeValidationAttempt env state candidate start remainingMicros

executeValidationAttempt ::
    EvalEnv -> IORef EvalState -> ByteString -> Ticks -> Maybe Ticks -> IO OracleOutcome
executeValidationAttempt env state candidate start remainingMicros = do
    modifyEvalState state (\s -> s {evalValidationObs = evalValidationObs s + 1})
    let observe =
            envValidate
                env
                (modifyEvalState state (\s -> s {evalValidationDatabase = evalValidationDatabase s + 1}))
                candidate
    result <- case remainingMicros of
        Nothing -> Just <$> observe
        Just remaining -> timeout (boundedTimeout (max 0 remaining)) observe
    end <- ticks
    modifyEvalState
        state
        (\s -> s {evalValidationSeconds = evalValidationSeconds s + seconds start end})
    recordValidationAttempt state result

recordValidationAttempt :: IORef EvalState -> Maybe OracleObservationDetails -> IO OracleOutcome
recordValidationAttempt state Nothing = do
    modifyEvalState state (\s -> s {evalValidationErrors = evalValidationErrors s + 1})
    recordValidationOutcome state OracleTimedOut
    pure OracleInconclusive
recordValidationAttempt state (Just profile) = do
    let observation = detailedObservation profile
        micros = fromIntegral
    modifyEvalState state \s ->
        s
            { evalValidationResetSeconds =
                evalValidationResetSeconds s + micros (resetElapsedMicros profile) / 1000000
            , evalValidationOracleSeconds =
                evalValidationOracleSeconds s + maybe 0 micros (oracleElapsedMicros profile) / 1000000
            }
    recordValidationOutcome state observation
    case observationOutcome observation of
        OracleAccepted -> pure OracleAccepted
        OracleRejected -> do
            modifyEvalState state (\s -> s {evalValidationRejected = True})
            pure OracleRejected
        OracleInconclusive -> do
            modifyEvalState state (\s -> s {evalValidationErrors = evalValidationErrors s + 1})
            pure OracleInconclusive

-- | Atomically record worker observation metrics.
recordReductionObservation :: IORef EvalState -> OracleObservationDetails -> IO ()
recordReductionObservation state profile = do
    let observation = detailedObservation profile
        micros = fromIntegral
        resetDelta = micros (resetElapsedMicros profile) / 1000000
        oracleDelta = maybe 0 (\oracleMicros -> fromIntegral oracleMicros / 1000000) (oracleElapsedMicros profile)
        outcome = classifyOracleObservation observation
        acceptedDelta = if oracleObservationMatches observation then 1 else 0
        rejectedDelta = 1 - acceptedDelta
    modifyEvalState state \s ->
        let billed =
                s
                    { evalResetSeconds = evalResetSeconds s + resetDelta
                    , evalOracleSeconds = evalOracleSeconds s + oracleDelta
                    , evalOutcomes = Map.insertWith (+) outcome 1 (evalOutcomes s)
                    }
         in case observation of
                OracleFinished _ ->
                    billed
                        { evalCompleted = evalCompleted s + 1
                        , evalAcceptedCount = evalAcceptedCount s + acceptedDelta
                        , evalRejectedCount = evalRejectedCount s + rejectedDelta
                        }
                OracleTimedOut ->
                    billed {evalTimedOut = evalTimedOut s + 1, evalErrors = evalErrors s + 1}
                OracleResetFailed ->
                    billed {evalResetFailures = evalResetFailures s + 1, evalErrors = evalErrors s + 1}
                OracleIOFailed _ -> billed {evalErrors = evalErrors s + 1}

recordValidationOutcome :: IORef EvalState -> OracleObservation -> IO ()
recordValidationOutcome state observation =
    modifyEvalState
        state
        ( \s ->
            s
                { evalValidationOutcomes =
                    Map.insertWith (+) (classifyOracleObservation observation) 1 (evalValidationOutcomes s)
                }
        )

-- | Update progress curve and checkpoints upon candidate retirement.
retireCandidate ::
    (EvaluationProgress -> IO ()) ->
    Ticks ->
    FilePath ->
    IORef EvalState ->
    Bool ->
    Int ->
    ByteString ->
    OracleOutcome ->
    Bool ->
    IO ()
retireCandidate onProgress reductionStart checkpoint state collectCurve callIndex candidate outcome hit = do
    let accepted = outcome == OracleAccepted
    -- Retrieve cached statement count for accepted candidate.
    statements <-
        if accepted
            then Map.lookup candidate . evalCounts <$> readIORef state
            else pure Nothing
    onProgress (ProgressCandidateTested (callIndex + 1) hit accepted)
    when accepted $ do
        smaller <- checkpointIfSmaller state checkpoint candidate statements
        when smaller $
            onProgress (ProgressCandidateAccepted (BS.length candidate) statements)
    extendCurve <-
        if collectCurve
            then do
                now <- ticks
                pure
                    ( EvaluationPoint
                        { elapsedSeconds = seconds reductionStart now
                        , callIndex = callIndex + 1
                        , candidateBytes = BS.length candidate
                        , candidateStatements = statements
                        , cacheHit = hit
                        , accepted = accepted
                        }
                        :
                    )
            else pure id
    modifyEvalState state \s ->
        s {evalCounts = Map.delete candidate (evalCounts s), evalCurve = extendCurve (evalCurve s)}

-- | Route reducer stage events to UI progress and per-stage summaries.
evalStageReporter :: (EvaluationProgress -> IO ()) -> IORef EvalState -> StageReporter
evalStageReporter onProgress state = \case
    StageStarted stage -> onProgress (ProgressStage (reductionStageName stage))
    StageFinished stage elapsed before after ->
        modifyEvalState
            state
            (\s -> s {evalStages = stageSummary stage elapsed before after : evalStages s})

stageSummary :: ReductionStage -> Double -> Int -> Int -> Value
stageSummary stage elapsed before after =
    object
        [ "stage" .= reductionStageName stage
        , "seconds" .= elapsed
        , "bytes_before" .= before
        , "bytes_after" .= after
        ]

checkpointIfSmaller :: IORef EvalState -> FilePath -> ByteString -> Maybe Int -> IO Bool
checkpointIfSmaller state path candidate statements = do
    current <- evalAccepted <$> readIORef state
    if BS.length candidate < BS.length current
        then mask_ $ do
            unless (null path) $ writeAtomicBytes path candidate
            modifyEvalState state (\s -> s {evalAccepted = candidate, evalAcceptedStatements = statements})
            pure True
        else pure False

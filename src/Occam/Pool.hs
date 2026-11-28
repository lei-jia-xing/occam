-- | Speculative sliding-window parallel and ordered-serial oracle execution pool.
module Occam.Pool (
    OraclePool,
    PoolConfig (..),
    WindowOutcome,
    ExecutionBudgetReached (..),
    newOraclePool,
    newOraclePoolWithCacheKey,
    closeOraclePool,
    withOraclePool,
    withPredicatePool,
    testOrdered,
    testOrderedTagged,
    testOrderedGenerated,
    withAdaptiveLookahead,
    poolOracle,
    poolMetricsStore,
    mapPoolRetire,
    withCandidateCountCleanup,
    poolCacheStats,
    poolAttempts,
    poolCancelled,
    poolLiveWorkers,
    poolJobCount,
    reportCandidateCount,
    observationDetails,
    predicateDetails,
) where

import Control.Concurrent (
    MVar,
    killThread,
    modifyMVar_,
    myThreadId,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
 )
import Control.Exception (
    AsyncException (..),
    SomeException,
    bracket_,
    finally,
    fromException,
    mask,
    mask_,
    throwIO,
    toException,
    try,
    uninterruptibleMask_,
 )
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Cache (
    CacheKey,
    CacheMode (..),
    LookupResult (..),
    cacheInsertDoneKey,
    cacheLookupKey,
 )
import Occam.Core.Oracle (Oracle, OracleInconclusiveError (..), OracleOutcome (..))
import Occam.Metrics (
    FamilyTag (..),
    recordCanceled,
    recordFailed,
    recordPhysicalCompletionAndTiming,
    recordPhysicalStart,
    recordRetired,
 )
import Occam.Oracle (
    OracleObservation,
    OracleObservationDetails (..),
    OracleReapFailed (..),
    observationOutcome,
 )
import Occam.Pool.Types (
    ExecutionBudgetReached (..),
    OraclePool (..),
    PoolConfig (..),
    WindowOutcome,
    WorkerEntry (..),
    admitObservation,
    checkReapFailure,
    closeOraclePool,
    mapPoolRetire,
    newOraclePool,
    newOraclePoolWithCacheKey,
    observationDetails,
    outcomeVerdict,
    poolAttempts,
    poolCacheStats,
    poolCancelled,
    poolJobCount,
    poolLiveWorkers,
    poolMetricsStore,
    predicateDetails,
    reportCandidateCount,
    reservePermit,
    wallSince,
    withAdaptiveLookahead,
    withCandidateCountCleanup,
    withOraclePool,
    withPredicatePool,
 )
import Occam.Pool.Window (runSlidingWindow)

-- | Window-1 callback through the same ordered path; throws like 'testOrdered'.
poolOracle :: OraclePool -> Oracle
poolOracle pool candidate = do
    outcomes <- testOrdered pool [candidate]
    case outcomes of
        [(outcome, _)] -> pure outcome
        _ -> ioError (userError "OraclePool window-1 retired no outcome")

{- | Test candidates sequentially or speculatively up to concurrency limit.
Returns outcomes up to the first acceptance, or all outcomes if all reject.
-}
testOrdered :: OraclePool -> [ByteString] -> IO [WindowOutcome]
testOrdered pool = testOrderedTagged pool . map (TagUnknown,)

-- | Like 'testOrdered', but tracks telemetry and costs per 'FamilyTag'.
testOrderedTagged :: OraclePool -> [(FamilyTag, ByteString)] -> IO [WindowOutcome]
testOrderedTagged _ [] = pure []
testOrderedTagged pool candidates =
    reverse <$> testOrderedGenerated pool next collect [] candidates
    where
        next [] = pure Nothing
        next ((tag, sql) : rest) = pure (Just ((tag, sql, ()), rest))
        collect results () _ outcome = pure (outcome : results)

{- | Pull SQL only when the ordered window can use it. The producer state is
private to the window. Fold over the retired probes in order.
-}
testOrderedGenerated ::
    OraclePool ->
    (state -> IO (Maybe ((FamilyTag, ByteString, payload), state))) ->
    (result -> payload -> ByteString -> WindowOutcome -> IO result) ->
    result ->
    state ->
    IO result
testOrderedGenerated pool next retire initialResult initial =
    bracket_ (takeMVar (poolGate pool)) (putMVar (poolGate pool) ()) $
        (checkReapFailure pool >> dispatch) `finally` poolCleanupCounts pool
    where
        dispatch
            | poolJobsCount pool == 1 = runSerialOrdered pool next retire initialResult initial
            | otherwise = runSlidingWindow pool next retire initialResult initial

runSerialOrdered ::
    OraclePool ->
    (state -> IO (Maybe ((FamilyTag, ByteString, payload), state))) ->
    (result -> payload -> ByteString -> WindowOutcome -> IO result) ->
    result ->
    state ->
    IO result
runSerialOrdered pool next retire initialResult initial = do
    tid <- myThreadId
    done <- newEmptyMVar
    cancelAck <- newEmptyMVar
    stateRef <- newIORef (0 :: Int)
    let cancelWorker = mask_ $ do
            killIt <- atomicModifyIORef' stateRef $ \s -> if s == 1 then (2, True) else (s, False)
            when killIt $ killThread tid `finally` putMVar cancelAck ()
        cleanup = do
            modifyMVar_ (poolRegistry pool) (pure . Map.delete tid)
            putMVar done ()
    mask \restore -> do
        modifyMVar_ (poolRegistry pool) (pure . Map.insert tid (WorkerEntry done cancelWorker))
        restore (goSerial cancelAck stateRef initial initialResult) `finally` cleanup
    where
        goSerial cancelAck stateRef pending acc =
            next pending >>= \case
                Nothing -> pure acc
                Just ((tag, candidate, payload), rest) -> do
                    (observation, hit) <- runSerialCandidate pool cancelAck stateRef tag candidate
                    retireSerial cancelAck stateRef tag candidate payload observation hit rest acc

        retireSerial cancelAck stateRef tag candidate payload observation hit rest acc = do
            let outcome = observationOutcome observation
            recordRetired (poolMetricsData pool) tag (outcomeVerdict outcome)
            index <- atomicModifyIORef' (poolIndexRef pool) (\n -> (n + 1, n))
            poolRetireFn pool index candidate outcome hit
            !acc' <- retire acc payload candidate (outcome, hit)
            case outcome of
                OracleAccepted -> pure acc'
                OracleInconclusive -> throwIO OracleInconclusiveError
                OracleRejected -> goSerial cancelAck stateRef rest acc'

runSerialCandidate ::
    OraclePool -> MVar () -> IORef Int -> FamilyTag -> ByteString -> IO (OracleObservation, Bool)
runSerialCandidate pool cancelAck stateRef tag candidate = do
    key <- poolCandidateKey pool candidate
    look <- cacheLookupKey (poolCacheData pool) (poolCacheModeData pool) key
    case look of
        LookupHit observation -> pure (observation, True)
        LookupJoin _ -> ioError (userError "OraclePool serial path encountered in-flight join")
        LookupMiss -> do
            profile <- executeSerialMiss pool cancelAck stateRef tag key candidate
            pure (detailedObservation profile, False)

executeSerialMiss ::
    OraclePool ->
    MVar () ->
    IORef Int ->
    FamilyTag ->
    CacheKey ->
    ByteString ->
    IO OracleObservationDetails
executeSerialMiss pool cancelAck stateRef tag key candidate =
    mask $ \restore -> do
        reserved <- reservePermit (poolPermitsVar pool)
        unless reserved $ throwIO (ExecutionBudgetReached ())
        startNs <- maybe (pure 0) (const getMonotonicTimeNSec) (poolMetricsData pool)
        atomicModifyIORef' (poolLiveRef pool) (\n -> (n + 1, ()))
        atomicModifyIORef' (poolAttemptsRef pool) (\n -> (n + 1, ()))
        recordPhysicalStart (poolMetricsData pool) tag
        writeIORef stateRef 1
        outcome <- try (restore (poolExecuteFn pool candidate))
        cancelling <- atomicModifyIORef' stateRef (\s -> (0, s == 2))
        cancelResult <-
            if cancelling
                then Just <$> (try (readMVar cancelAck) :: IO (Either SomeException ()))
                else pure Nothing
        settled <-
            uninterruptibleMask_ $
                settleSerialOutcome pool tag key startNs outcome cancelResult
                    `finally` atomicModifyIORef' (poolLiveRef pool) (\n -> (n - 1, ()))
        either throwIO pure settled

settleSerialOutcome ::
    OraclePool ->
    FamilyTag ->
    CacheKey ->
    Word64 ->
    Either SomeException OracleObservationDetails ->
    Maybe (Either SomeException ()) ->
    IO (Either SomeException OracleObservationDetails)
settleSerialOutcome pool tag key startNs outcome cancelResult = case (outcome, cancelResult) of
    (Left err, _) -> recordFailure err >> pure (Left err)
    (Right _, Just (Left err)) -> recordFailure err >> pure (Left err)
    (Right _, Just (Right ())) -> do
        wall <- maybe (pure 0) (const (wallSince startNs)) (poolMetricsData pool)
        recordCancelled wall
        pure (Left (toException ThreadKilled))
    (Right details, Nothing) -> do
        poolBillFn pool details
        let observation = detailedObservation details
        when (poolCacheModeData pool == CacheEnabled) $
            for_ (admitObservation observation) (cacheInsertDoneKey (poolCacheData pool) key)
        recordPhysicalCompletionAndTiming
            (poolMetricsData pool)
            tag
            (oracleElapsedMicros details)
            (Just (resetElapsedMicros details))
        pure (Right details)
    where
        recordCancelled wall = do
            atomicModifyIORef' (poolCancelledRef pool) (\n -> (n + 1, ()))
            recordCanceled (poolMetricsData pool) tag wall
        recordFailure err = do
            for_ (fromException err) \OracleReapFailed -> writeIORef (poolReapFailed pool) True
            wall <- maybe (pure 0) (const (wallSince startNs)) (poolMetricsData pool)
            case fromException err of
                Just ThreadKilled -> recordCancelled wall
                _ -> recordFailed (poolMetricsData pool) tag wall

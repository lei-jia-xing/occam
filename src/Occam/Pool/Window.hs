-- | Sliding-window concurrent scheduler enforcing lookahead limits and serial retirement.
module Occam.Pool.Window (
    runSlidingWindow,
) where

import Control.Concurrent (
    MVar,
    forkIOWithUnmask,
    killThread,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    tryTakeMVar,
 )
import Control.Concurrent.QSem (signalQSem, waitQSem)
import Control.Exception (
    AsyncException (..),
    SomeException,
    bracket_,
    fromException,
    mask,
    mask_,
    onException,
    throwIO,
    try,
 )
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Cache (
    CacheKey,
    CacheMode (..),
    LookupResult (..),
    cacheCompleteInflightKey,
    cacheInstallInflightKey,
    cacheLookupKey,
 )
import Occam.Core.Oracle (OracleInconclusiveError (..), OracleOutcome (..))
import Occam.Metrics (
    FamilyTag (..),
    recordCanceled,
    recordCompletedUnselected,
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
    TimedObservation (..),
    WindowOutcome,
    WindowTask (..),
    WorkerEntry (..),
    admitObservation,
    checkReapFailure,
    outcomeVerdict,
    recordLookaheadRun,
    refundPermit,
    reservePermit,
    slidingLookahead,
    wallSince,
 )

type ActiveTask payload = (FamilyTag, ByteString, payload, Int, WindowTask)

{- | Bounded sliding-window ordered scheduler: keeps up to @jobs@ candidates in
flight simultaneously while strictly retiring decisions in input order and
immediately launching the next candidate on each head rejection.
-}
runSlidingWindow ::
    OraclePool ->
    (state -> IO (Maybe ((FamilyTag, ByteString, payload), state))) ->
    (result -> payload -> ByteString -> WindowOutcome -> IO result) ->
    result ->
    state ->
    IO result
runSlidingWindow pool next retire initialResult initial = mask \restore -> do
    activeRef <- newIORef Seq.empty
    retiredThrough <- newIORef (-1 :: Int)
    firstDecisiveRef <- newIORef (Nothing :: Maybe Int)
    width <- slidingLookahead pool
    let markDecisive !ord =
            atomicModifyIORef' firstDecisiveRef \prev -> (Just (maybe ord (min ord) prev), ())
        settleRemaining = do
            remSeq <- readIORef activeRef
            rt <- readIORef retiredThrough
            writeIORef activeRef Seq.empty
            settleAll pool [(ord, task) | (_, _, _, ord, task) <- toList remSeq] rt
        fill !nextOrd Nothing = pure (nextOrd, Nothing)
        fill !nextOrd (Just pending) = do
            cur <- readIORef activeRef
            decisive <- readIORef firstDecisiveRef
            if Seq.length cur >= width || maybe False (< nextOrd) decisive
                then pure (nextOrd, Just pending)
                else pullNext cur nextOrd pending
        pullNext active !nextOrd pending =
            restore (next pending) >>= \case
                Nothing -> pure (nextOrd, Nothing)
                Just ((tag, candidate, payload), rest) -> do
                    published <- readIORef firstDecisiveRef
                    if maybe False (< nextOrd) published
                        then pure (nextOrd, Nothing)
                        else do
                            task <- launchTask pool active nextOrd tag candidate markDecisive
                            modifyIORef' activeRef (Seq.|> (tag, candidate, payload, nextOrd, task))
                            fill (nextOrd + 1) (Just rest)
        finish accepted fresh prefix = do
            checkReapFailure pool
            mapM_ (recordLookaheadRun pool accepted) fresh
            pure prefix
        go !nextOrd pending prefix !fresh = do
            (nextOrd', pending') <- fill nextOrd pending
            cur <- readIORef activeRef
            case Seq.viewl cur of
                Seq.EmptyL -> finish False fresh prefix
                (tag, candidate, payload, ordinal, task) Seq.:< restSeq -> do
                    (observation, hit) <- restore (resolveSlidingTask task)
                    let outcome = observationOutcome observation
                        fresh' = case fresh of
                            Just n | not hit -> let !count = n + 1 in Just count
                            _ -> Nothing
                    -- Commit retirement before invoking callback and retire this worker cleanly.
                    recordRetired (poolMetricsData pool) tag (outcomeVerdict outcome)
                    writeIORef retiredThrough ordinal
                    writeIORef activeRef restSeq
                    settleOne pool ordinal (ordinal, task)
                    index <- atomicModifyIORef' (poolIndexRef pool) (\n -> (n + 1, n))
                    restore (poolRetireFn pool index candidate outcome hit)
                    !prefix' <- restore (retire prefix payload candidate (outcome, hit))
                    case outcome of
                        OracleAccepted -> do
                            settleRemaining
                            finish True fresh' prefix'
                        OracleInconclusive -> settleRemaining >> throwIO OracleInconclusiveError
                        OracleRejected -> do
                            atomicModifyIORef' firstDecisiveRef (\m -> if m == Just ordinal then (Nothing, ()) else (m, ()))
                            go nextOrd' pending' prefix' fresh'
    go 0 (Just initial) initialResult (0 <$ poolLookahead pool) `onException` settleRemaining

resolveSlidingTask :: WindowTask -> IO (OracleObservation, Bool)
resolveSlidingTask task = case task of
    TaskImmediate observation hit -> pure (observation, hit)
    TaskJoined result -> readObservation True result
    TaskDenied -> throwIO (ExecutionBudgetReached ())
    TaskPending _ result _ _ _ _ -> readObservation False result
    where
        readObservation hit result =
            readMVar result >>= \case
                Right timed -> pure (detailedObservation (timedDetails timed), hit)
                Left err -> throwIO err

launchTask ::
    OraclePool ->
    Seq.Seq (ActiveTask payload) ->
    Int ->
    FamilyTag ->
    ByteString ->
    (Int -> IO ()) ->
    IO WindowTask
launchTask pool active ordinal tag candidate markDecisive = do
    key <- poolCandidateKey pool candidate
    look <- cacheLookupKey (poolCacheData pool) (poolCacheModeData pool) key
    case look of
        LookupHit observation -> do
            when (observationOutcome observation /= OracleRejected) (markDecisive ordinal)
            pure (TaskImmediate observation True)
        LookupJoin owner ->
            -- Active ordinals are contiguous. The in-flight entry belongs to
            -- an active worker, and its result remains readable after retirement.
            case Seq.lookup (owner - (ordinal - Seq.length active)) active of
                Just (_, _, _, ownerOrdinal, TaskPending _ result _ _ _ _)
                    | ownerOrdinal == owner -> pure (TaskJoined result)
                _ -> ioError (userError "OraclePool join owner is not an active worker")
        LookupMiss -> launchFreshTask pool ordinal tag key candidate markDecisive

launchFreshTask ::
    OraclePool -> Int -> FamilyTag -> CacheKey -> ByteString -> (Int -> IO ()) -> IO WindowTask
launchFreshTask pool ordinal tag key candidate markDecisive = do
    reserved <- reservePermit (poolPermitsVar pool)
    if not reserved
        then markDecisive ordinal >> pure TaskDenied
        else do
            when (poolCacheModeData pool == CacheEnabled) $
                cacheInstallInflightKey (poolCacheData pool) key ordinal
            spawnPendingTask pool ordinal tag key candidate markDecisive `onException` rollbackLaunch
    where
        rollbackLaunch = do
            when (poolCacheModeData pool == CacheEnabled) $
                cacheCompleteInflightKey (poolCacheData pool) key ordinal Nothing
            refundPermit (poolPermitsVar pool)

spawnPendingTask ::
    OraclePool -> Int -> FamilyTag -> CacheKey -> ByteString -> (Int -> IO ()) -> IO WindowTask
spawnPendingTask pool ordinal tag key candidate markDecisive = do
    result <- newEmptyMVar
    done <- newEmptyMVar
    spent <- newMVar ()
    startRef <- newIORef Nothing
    mask_ $ do
        tid <- forkIOWithUnmask $ \unmask -> do
            outcome <- try (unmask (runWorker pool tag candidate key ordinal startRef))
            mask_ $ publishOutcome pool tag key ordinal startRef result done markDecisive outcome
        atomicModifyIORef' (poolLiveRef pool) (\n -> (n + 1, ()))
        modifyMVar_ (poolRegistry pool) (pure . Map.insert tid (WorkerEntry done (killThread tid)))
        pure (TaskPending tid result done spent tag startRef)

runWorker ::
    OraclePool ->
    FamilyTag ->
    ByteString ->
    CacheKey ->
    Int ->
    IORef (Maybe Word64) ->
    IO TimedObservation
runWorker pool tag candidate key ordinal startRef =
    bracket_ (waitQSem (poolSlots pool)) (signalQSem (poolSlots pool)) $ do
        startNs <- case poolMetricsData pool of
            Nothing -> pure 0
            Just _ -> getMonotonicTimeNSec
        mask_ $ do
            atomicModifyIORef' (poolAttemptsRef pool) (\n -> (n + 1, ()))
            writeIORef startRef (Just startNs)
            recordPhysicalStart (poolMetricsData pool) tag
        profile <- poolExecuteFn pool candidate
        mask_ $ do
            poolBillFn pool profile
            let observation = detailedObservation profile
            when (poolCacheModeData pool == CacheEnabled) $
                cacheCompleteInflightKey (poolCacheData pool) key ordinal (admitObservation observation)
            wallMicros <- case poolMetricsData pool of
                Nothing -> pure 0
                Just _ -> wallSince startNs
            pure (TimedObservation profile wallMicros)

-- | Masked final publication: every worker publishes exactly once, always.
publishOutcome ::
    OraclePool ->
    FamilyTag ->
    CacheKey ->
    Int ->
    IORef (Maybe Word64) ->
    MVar (Either SomeException TimedObservation) ->
    MVar () ->
    (Int -> IO ()) ->
    Either SomeException TimedObservation ->
    IO ()
publishOutcome pool tag key ordinal startRef result done markDecisive outcome = mask_ $ do
    case outcome of
        Right timed -> do
            let details = timedDetails timed
            recordPhysicalCompletionAndTiming
                (poolMetricsData pool)
                tag
                (oracleElapsedMicros details)
                (Just (resetElapsedMicros details))
            let obs = detailedObservation details
            when (observationOutcome obs /= OracleRejected) (markDecisive ordinal)
            putMVar result (Right timed)
        Left err -> do
            markDecisive ordinal
            case fromException err of
                Just OracleReapFailed -> writeIORef (poolReapFailed pool) True
                Nothing -> pure ()
            started <- readIORef startRef
            case started of
                Nothing -> refundPermit (poolPermitsVar pool)
                Just _ -> pure ()
            when (poolCacheModeData pool == CacheEnabled) $
                cacheCompleteInflightKey (poolCacheData pool) key ordinal Nothing
            -- Record worker metrics at thread termination.
            wall <- case (poolMetricsData pool, started) of
                (Just _, Just begin) -> wallSince begin
                _ -> pure 0
            case fromException err of
                Just ThreadKilled -> do
                    atomicModifyIORef' (poolCancelledRef pool) (\n -> (n + 1, ()))
                    recordCanceled (poolMetricsData pool) tag wall
                _ -> recordFailed (poolMetricsData pool) tag wall
            putMVar result (Left err)
    atomicModifyIORef' (poolLiveRef pool) (\n -> (n - 1, ()))
    putMVar done ()

-- | Cancel unretired tasks and join all workers in the window.
settleAll :: OraclePool -> [(Int, WindowTask)] -> Int -> IO ()
settleAll pool launched retiredThrough = mapM_ (settleOne pool retiredThrough) launched

settleOne :: OraclePool -> Int -> (Int, WindowTask) -> IO ()
settleOne pool retiredThrough (ordinal, TaskPending tid result done spent tag _) = do
    first <- tryTakeMVar spent
    case first of
        Nothing -> readMVar done
        Just () -> do
            when (ordinal > retiredThrough) (killThread tid)
            readMVar done
            published <- readMVar result
            case published of
                Right timed
                    | ordinal <= retiredThrough -> pure ()
                    | otherwise ->
                        recordCompletedUnselected
                            (poolMetricsData pool)
                            tag
                            (timedExecWallMicros timed)
                Left _ -> pure ()
            modifyMVar_ (poolRegistry pool) (pure . Map.delete tid)
settleOne _ _ _ = pure ()

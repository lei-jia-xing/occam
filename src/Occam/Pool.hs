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
    ThreadId,
    forkIOWithUnmask,
    killThread,
    modifyMVar,
    modifyMVar_,
    myThreadId,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryTakeMVar,
 )
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (
    AsyncException (..),
    Exception,
    SomeException,
    bracket_,
    finally,
    fromException,
    mask,
    mask_,
    onException,
    throwIO,
    toException,
    try,
    uninterruptibleMask_,
 )
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Foldable (for_, toList)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Cache (
    Cache,
    CacheAdmission (..),
    CacheKey,
    CacheMode (..),
    CacheStats,
    LookupResult (..),
    cacheCompleteInflightKey,
    cacheInsertDoneKey,
    cacheInstallInflightKey,
    cacheLookupKey,
    cacheStats,
    makeKey,
    newCache,
    observationAdmission,
 )
import Occam.Core (Oracle, OracleInconclusiveError (..), OracleOutcome (..))
import Occam.Metrics (
    FamilyTag (..),
    MetricsStore,
    RetiredVerdict (..),
    recordCanceled,
    recordCompletedUnselected,
    recordFailed,
    recordPhysicalCompletionAndTiming,
    recordPhysicalStart,
    recordRetired,
 )
import Occam.Oracle (
    OracleObservation (..),
    OracleObservationDetails (..),
    OracleReapFailed (..),
    observationOutcome,
 )
import System.Exit (ExitCode (..))

data ExecutionBudgetReached = ExecutionBudgetReached ()
    deriving (Show)

instance Exception ExecutionBudgetReached

data PoolConfig = PoolConfig
    { poolJobs :: !Int
    , poolCacheEnabled :: !Bool
    , poolPermits :: !(Maybe Int)
    , poolPrecomputed :: !(Map ByteString OracleObservation)
    , poolMetrics :: !(Maybe MetricsStore)
    , poolExecute :: !(ByteString -> IO OracleObservationDetails)
    , poolBill :: !(OracleObservationDetails -> IO ())
    , poolRetire :: !(Int -> ByteString -> OracleOutcome -> Bool -> IO ())
    , poolReportCount :: !(Maybe (ByteString -> Int -> IO ()))
    }

-- | Retired decision in candidate order: outcome plus cache-hit flag.
type WindowOutcome = (OracleOutcome, Bool)

data WorkerEntry = WorkerEntry
    { entryDone :: !(MVar ())
    , entryCancel :: !(IO ())
    }

data OraclePool = OraclePool
    { poolJobsCount :: !Int
    , poolGate :: !(MVar ())
    , poolSlots :: !QSem
    , poolPermitsVar :: !(MVar (Maybe Int))
    , poolCacheData :: !Cache
    , poolCandidateKey :: !(ByteString -> IO CacheKey)
    , poolCacheModeData :: !CacheMode
    , poolMetricsData :: !(Maybe MetricsStore)
    , poolExecuteFn :: !(ByteString -> IO OracleObservationDetails)
    , poolBillFn :: !(OracleObservationDetails -> IO ())
    , poolRetireFn :: !(Int -> ByteString -> OracleOutcome -> Bool -> IO ())
    , poolCleanupCounts :: !(IO ())
    , poolReapFailed :: !(IORef Bool)
    , poolReportCountFn :: !(Maybe (ByteString -> Int -> IO ()))
    , poolRegistry :: !(MVar (Map ThreadId WorkerEntry))
    , poolIndexRef :: !(IORef Int)
    , poolAttemptsRef :: !(IORef Int)
    , poolCancelledRef :: !(IORef Int)
    , poolLiveRef :: !(IORef Int)
    , poolAcceptStreak :: !(Maybe (IORef Int))
    }

newOraclePool :: PoolConfig -> IO OraclePool
newOraclePool = newOraclePoolWithCacheKey pure

newOraclePoolWithCacheKey :: (ByteString -> IO ByteString) -> PoolConfig -> IO OraclePool
newOraclePoolWithCacheKey candidateKey PoolConfig {..}
    | poolJobs <= 0 = ioError (userError ("OraclePool jobs must be positive, got " ++ show poolJobs))
    | otherwise = do
        gate <- newMVar ()
        slots <- newQSem poolJobs
        permits <- newMVar poolPermits
        let keyOf = if poolCacheEnabled then candidateKey else pure
        precomputed <-
            traverse (\(sql, observation) -> (,observation) <$> keyOf sql) (Map.toList poolPrecomputed)
        cache <- newCache (Map.fromList precomputed)
        registry <- newMVar Map.empty
        index <- newIORef 0
        attempts <- newIORef 0
        cancelled <- newIORef 0
        live <- newIORef 0
        reapFailed <- newIORef False
        pure
            OraclePool
                { poolJobsCount = poolJobs
                , poolGate = gate
                , poolSlots = slots
                , poolPermitsVar = permits
                , poolCacheData = cache
                , poolCandidateKey = fmap makeKey . keyOf
                , poolCacheModeData = if poolCacheEnabled then CacheEnabled else CacheDisabled
                , poolMetricsData = poolMetrics
                , poolExecuteFn = poolExecute
                , poolBillFn = poolBill
                , poolRetireFn = poolRetire
                , poolCleanupCounts = pure ()
                , poolReapFailed = reapFailed
                , poolReportCountFn = poolReportCount
                , poolRegistry = registry
                , poolIndexRef = index
                , poolAttemptsRef = attempts
                , poolCancelledRef = cancelled
                , poolLiveRef = live
                , poolAcceptStreak = Nothing
                }

{- | Native probes can sweep through a succession of immediately accepted
candidates. After two head acceptances, start the next window serially and
restore the full width on its first retirement that rejects.
-}
withAdaptiveLookahead :: OraclePool -> IO OraclePool
withAdaptiveLookahead pool
    | poolJobsCount pool <= 1 = pure pool
    | otherwise = do
        streak <- newIORef 0
        pure pool {poolAcceptStreak = Just streak}

-- | Cancel and join all active workers in the pool.
closeOraclePool :: OraclePool -> IO ()
closeOraclePool pool = drain >> checkReapFailure pool
    where
        drain = do
            snapshot <- readMVar (poolRegistry pool)
            unless (Map.null snapshot) $ do
                mapM_ entryCancel (Map.elems snapshot)
                mapM_ (readMVar . entryDone) (Map.elems snapshot)
                mapM_ (deregWorker pool) (Map.keys snapshot)
                drain

checkReapFailure :: OraclePool -> IO ()
checkReapFailure pool = do
    failed <- readIORef (poolReapFailed pool)
    when failed (throwIO OracleReapFailed)

deregWorker :: OraclePool -> ThreadId -> IO ()
deregWorker pool tid = modifyMVar_ (poolRegistry pool) (pure . Map.delete tid)

withOraclePool :: PoolConfig -> (OraclePool -> IO a) -> IO a
withOraclePool config action = do
    pool <- newOraclePool config
    action pool `onException` closeOraclePool pool <* closeOraclePool pool

-- | In-process predicate pool for tests.
withPredicatePool :: Int -> Bool -> (ByteString -> IO Bool) -> (OraclePool -> IO a) -> IO a
withPredicatePool jobs cacheEnabled predicate =
    withOraclePool
        PoolConfig
            { poolJobs = jobs
            , poolCacheEnabled = cacheEnabled
            , poolPermits = Nothing
            , poolPrecomputed = mempty
            , poolMetrics = Nothing
            , poolExecute = predicateDetails predicate
            , poolBill = const (pure ())
            , poolRetire = \_ _ _ _ -> pure ()
            , poolReportCount = Nothing
            }

-- | Window-1 callback through the same ordered path; throws like 'testOrdered'.
poolOracle :: OraclePool -> Oracle
poolOracle pool candidate = do
    outcomes <- testOrdered pool [candidate]
    case outcomes of
        [(outcome, _)] -> pure outcome
        _ -> ioError (userError "OraclePool window-1 retired no outcome")

-- | Shared metrics store of the pool, if present.
poolMetricsStore :: OraclePool -> Maybe MetricsStore
poolMetricsStore = poolMetricsData

-- | Report a candidate's known statement count prior to testing.
reportCandidateCount :: OraclePool -> ByteString -> Int -> IO ()
reportCandidateCount pool bytes count = case poolReportCountFn pool of
    Nothing -> pure ()
    Just report -> report bytes count

-- | Elapsed microseconds since the given start timestamp.
wallSince :: Word64 -> IO Int
wallSince begin = do
    now <- getMonotonicTimeNSec
    pure (fromIntegral ((now - begin) `div` 1000))

-- | Derive a pool sharing slots, permits, cache, gate, and registry.
mapPoolRetire ::
    ( (Int -> ByteString -> OracleOutcome -> Bool -> IO ()) ->
      (Int -> ByteString -> OracleOutcome -> Bool -> IO ())
    ) ->
    OraclePool ->
    OraclePool
mapPoolRetire f pool = pool {poolRetireFn = f (poolRetireFn pool)}

withCandidateCountCleanup :: IO () -> OraclePool -> OraclePool
withCandidateCountCleanup cleanup pool = pool {poolCleanupCounts = cleanup}

poolJobCount :: OraclePool -> Int
poolJobCount = poolJobsCount

poolCacheStats :: OraclePool -> IO CacheStats
poolCacheStats = cacheStats . poolCacheData

poolAttempts :: OraclePool -> IO Int
poolAttempts = readIORef . poolAttemptsRef

poolCancelled :: OraclePool -> IO Int
poolCancelled = readIORef . poolCancelledRef

poolLiveWorkers :: OraclePool -> IO Int
poolLiveWorkers = readIORef . poolLiveRef

observationDetails :: OracleObservation -> OracleObservationDetails
observationDetails observation =
    OracleObservationDetails
        { detailedObservation = observation
        , totalElapsedMicros = 0
        , resetElapsedMicros = 0
        , oracleElapsedMicros = Nothing
        }

predicateDetails :: (ByteString -> IO Bool) -> ByteString -> IO OracleObservationDetails
predicateDetails predicate candidate = do
    accepted <- predicate candidate
    pure
        (observationDetails (if accepted then OracleFinished ExitSuccess else OracleFinished (ExitFailure 1)))

-- | Finished worker observation with its measured execution duration in microseconds.
data TimedObservation = TimedObservation
    { timedDetails :: !OracleObservationDetails
    , timedExecWallMicros :: !Int
    }

data WindowTask
    = TaskImmediate !OracleObservation !Bool
    | TaskJoined !Int
    | TaskDenied
    | TaskPending
        !ThreadId
        !(MVar (Either SomeException TimedObservation))
        !(MVar ())
        !(MVar ())
        !FamilyTag
        !(IORef (Maybe Word64))

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
        jobs = poolJobsCount pool
        dispatch
            | jobs == 1 = runSerial
            | otherwise = runSlidingWindow pool next retire initialResult initial

        runSerial = do
            tid <- myThreadId
            done <- newEmptyMVar
            cancelAck <- newEmptyMVar
            stateRef <- newIORef (0 :: Int)
            let cancelWorker = mask_ $ do
                    killIt <- atomicModifyIORef' stateRef $ \s ->
                        if s == 1 then (2, True) else (s, False)
                    when killIt $
                        killThread tid `finally` putMVar cancelAck ()
            modifyMVar_ (poolRegistry pool) (pure . Map.insert tid (WorkerEntry done cancelWorker))
            let cleanup = do
                    modifyMVar_ (poolRegistry pool) (pure . Map.delete tid)
                    putMVar done ()
            goSerial tid cancelAck stateRef initial initialResult `finally` cleanup

        goSerial tid cancelAck stateRef pending acc =
            next pending >>= \case
                Nothing -> pure acc
                Just ((tag, candidate, payload), rest) ->
                    runSerialCandidate tid cancelAck stateRef tag candidate payload rest acc

        runSerialCandidate tid cancelAck stateRef tag candidate payload rest acc = do
            key <- poolCandidateKey pool candidate
            look <- cacheLookupKey (poolCacheData pool) (poolCacheModeData pool) key
            case look of
                LookupHit observation -> retireSerial tid cancelAck stateRef tag candidate payload observation True rest acc
                LookupJoin _ -> ioError (userError "OraclePool serial path encountered in-flight join")
                LookupMiss -> do
                    profile <- mask $ \restore -> do
                        reserved <- reservePermit (poolPermitsVar pool)
                        unless reserved $ throwIO (ExecutionBudgetReached ())
                        startNs <- case poolMetricsData pool of
                            Nothing -> pure 0
                            Just _ -> getMonotonicTimeNSec
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
                        let recordCancelled wall = do
                                atomicModifyIORef' (poolCancelledRef pool) (\n -> (n + 1, ()))
                                recordCanceled (poolMetricsData pool) tag wall
                            recordFailure err = do
                                case fromException err of
                                    Just OracleReapFailed -> writeIORef (poolReapFailed pool) True
                                    Nothing -> pure ()
                                wall <- case poolMetricsData pool of
                                    Nothing -> pure 0
                                    Just _ -> wallSince startNs
                                case fromException err of
                                    Just ThreadKilled -> recordCancelled wall
                                    _ -> recordFailed (poolMetricsData pool) tag wall
                        settled <-
                            uninterruptibleMask_ $
                                ( case (outcome, cancelResult) of
                                    (Left err, _) -> recordFailure err >> pure (Left err)
                                    (Right _, Just (Left err)) -> recordFailure err >> pure (Left err)
                                    (Right _, Just (Right ())) -> do
                                        wall <- case poolMetricsData pool of
                                            Nothing -> pure 0
                                            Just _ -> wallSince startNs
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
                                )
                                    `finally` atomicModifyIORef' (poolLiveRef pool) (\n -> (n - 1, ()))
                        either throwIO pure settled
                    retireSerial
                        tid
                        cancelAck
                        stateRef
                        tag
                        candidate
                        payload
                        (detailedObservation profile)
                        False
                        rest
                        acc

        retireSerial tid cancelAck stateRef tag candidate payload observation hit rest acc = do
            let outcome = observationOutcome observation
                verdict = case outcome of
                    OracleAccepted -> RetiredAccept
                    OracleRejected -> RetiredReject
                    OracleInconclusive -> RetiredInconclusive
            recordRetired (poolMetricsData pool) tag verdict
            index <- atomicModifyIORef' (poolIndexRef pool) (\n -> (n + 1, n))
            poolRetireFn pool index candidate outcome hit
            !acc' <- retire acc payload candidate (outcome, hit)
            case outcome of
                OracleAccepted -> pure acc'
                OracleInconclusive -> throwIO OracleInconclusiveError
                OracleRejected -> goSerial tid cancelAck stateRef rest acc'

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
    let maxWidth = poolJobsCount pool
        lookahead = case poolAcceptStreak pool of
            Nothing -> pure maxWidth
            Just streak -> do
                accepts <- readIORef streak
                pure (if accepts >= 2 then 1 else maxWidth)
    activeRef <- newIORef Seq.empty
    retiredThrough <- newIORef (-1 :: Int)
    firstDecisiveRef <- newIORef (Nothing :: Maybe Int)
    let markDecisive !ord =
            atomicModifyIORef'
                firstDecisiveRef
                \case
                    Nothing -> (Just ord, ())
                    Just prev -> (Just (min prev ord), ())
        settleRemaining = do
            remSeq <- readIORef activeRef
            rt <- readIORef retiredThrough
            writeIORef activeRef Seq.empty
            settleAll pool [(ord, task) | (_, _, _, ord, task) <- toList remSeq] rt
        fill !nextOrd Nothing = pure (nextOrd, Nothing)
        fill !nextOrd (Just pending) = do
            cur <- readIORef activeRef
            width <- lookahead
            decisive <- readIORef firstDecisiveRef
            if Seq.length cur >= width || maybe False (< nextOrd) decisive
                then pure (nextOrd, Just pending)
                else
                    restore (next pending) >>= \case
                        Nothing -> pure (nextOrd, Nothing)
                        Just ((tag, candidate, payload), rest) -> do
                            published <- readIORef firstDecisiveRef
                            if maybe False (< nextOrd) published
                                then pure (nextOrd, Nothing)
                                else do
                                    task <- launchTask pool nextOrd tag candidate markDecisive
                                    modifyIORef' activeRef (Seq.|> (tag, candidate, payload, nextOrd, task))
                                    fill (nextOrd + 1) (Just rest)
        go !nextOrd pending prefix earlier = do
            (nextOrd', pending') <- fill nextOrd pending
            cur <- readIORef activeRef
            case Seq.viewl cur of
                Seq.EmptyL -> pure prefix
                (tag, candidate, payload, ordinal, task) Seq.:< restSeq -> do
                    (observation, hit) <- restore (resolveSlidingTask earlier task)
                    let outcome = observationOutcome observation
                        verdict = case outcome of
                            OracleAccepted -> RetiredAccept
                            OracleRejected -> RetiredReject
                            OracleInconclusive -> RetiredInconclusive
                    -- Commit retirement before invoking callback and retire this worker cleanly.
                    recordRetired (poolMetricsData pool) tag verdict
                    for_ (poolAcceptStreak pool) $ \streak ->
                        modifyIORef' streak (\prior -> if outcome == OracleAccepted then min 2 (prior + 1) else 0)
                    writeIORef retiredThrough ordinal
                    writeIORef activeRef restSeq
                    settleOne pool ordinal (ordinal, task)
                    index <- atomicModifyIORef' (poolIndexRef pool) (\n -> (n + 1, n))
                    restore (poolRetireFn pool index candidate outcome hit)
                    !prefix' <- restore (retire prefix payload candidate (outcome, hit))
                    let
                        -- A join is created only while its owner is in the
                        -- same window, so no remaining task can need an older
                        -- result after retiring this ordinal.
                        earlier' = IntMap.insert ordinal observation (IntMap.delete (ordinal - maxWidth) earlier)
                    case outcome of
                        OracleAccepted -> do
                            settleRemaining
                            pure prefix'
                        OracleInconclusive -> do
                            settleRemaining
                            throwIO OracleInconclusiveError
                        OracleRejected -> do
                            atomicModifyIORef' firstDecisiveRef (\m -> if m == Just ordinal then (Nothing, ()) else (m, ()))
                            go nextOrd' pending' prefix' earlier'
    outcomes <- go 0 (Just initial) initialResult IntMap.empty `onException` settleRemaining
    checkReapFailure pool
    pure outcomes

resolveSlidingTask ::
    IntMap.IntMap OracleObservation ->
    WindowTask ->
    IO (OracleObservation, Bool)
resolveSlidingTask earlier task = case task of
    TaskImmediate observation hit -> pure (observation, hit)
    TaskJoined owner -> case IntMap.lookup owner earlier of
        Just observation -> pure (observation, True)
        Nothing -> ioError (userError "OraclePool join owner retired out of order")
    TaskDenied -> throwIO (ExecutionBudgetReached ())
    TaskPending _ result _ _ _ _ -> do
        published <- readMVar result
        case published of
            Right timed -> pure (detailedObservation (timedDetails timed), False)
            Left err -> throwIO err

launchTask :: OraclePool -> Int -> FamilyTag -> ByteString -> (Int -> IO ()) -> IO WindowTask
launchTask pool ordinal tag candidate markDecisive = do
    key <- poolCandidateKey pool candidate
    look <- cacheLookupKey (poolCacheData pool) (poolCacheModeData pool) key
    case look of
        LookupHit observation -> do
            when (observationOutcome observation /= OracleRejected) (markDecisive ordinal)
            pure (TaskImmediate observation True)
        LookupJoin owner -> pure (TaskJoined owner)
        LookupMiss -> do
            reserved <- reservePermit (poolPermitsVar pool)
            if not reserved
                then do
                    markDecisive ordinal
                    pure TaskDenied
                else do
                    when (poolCacheModeData pool == CacheEnabled) $
                        cacheInstallInflightKey (poolCacheData pool) key ordinal
                    ( do
                            result <- newEmptyMVar
                            done <- newEmptyMVar
                            spent <- newMVar ()
                            startRef <- newIORef Nothing
                            mask_ $ do
                                tid <-
                                    forkIOWithUnmask $ \unmask -> do
                                        outcome <- try (unmask (runWorker pool tag candidate key ordinal startRef))
                                        mask_ $ publishOutcome pool tag key ordinal startRef result done markDecisive outcome
                                atomicModifyIORef' (poolLiveRef pool) (\n -> (n + 1, ()))
                                modifyMVar_ (poolRegistry pool) (pure . Map.insert tid (WorkerEntry done (killThread tid)))
                                pure (TaskPending tid result done spent tag startRef)
                        )
                        `onException` ( do
                                            when (poolCacheModeData pool == CacheEnabled) $
                                                cacheCompleteInflightKey (poolCacheData pool) key ordinal Nothing
                                            refundPermit (poolPermitsVar pool)
                                      )

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

admitObservation :: OracleObservation -> Maybe OracleObservation
admitObservation observation = case observationAdmission observation of
    CacheAdmit -> Just observation
    CacheReject -> Nothing

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

reservePermit :: MVar (Maybe Int) -> IO Bool
reservePermit permits = modifyMVar permits reserve
    where
        reserve Nothing = pure (Nothing, True)
        reserve (Just n)
            | n > 0 = pure (Just (n - 1), True)
            | otherwise = pure (Just 0, False)

refundPermit :: MVar (Maybe Int) -> IO ()
refundPermit permits =
    uninterruptibleMask_ $
        modifyMVar_ permits \current -> pure $ case current of
            Nothing -> Nothing
            Just n -> Just (n + 1)

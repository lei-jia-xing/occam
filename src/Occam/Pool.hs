module Occam.Pool (
    OraclePool,
    PoolConfig (..),
    WindowOutcome,
    ExecutionBudgetReached (..),
    newOraclePool,
    closeOraclePool,
    withOraclePool,
    withPredicatePool,
    testOrdered,
    testOrderedTagged,
    poolOracle,
    poolMetricsStore,
    mapPoolRetire,
    poolCacheStats,
    poolAttempts,
    poolCancelled,
    poolLiveWorkers,
    poolJobCount,
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
    fromException,
    mask_,
    onException,
    throwIO,
    try,
 )
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Cache (
    Cache,
    CacheAdmission (..),
    CacheMode (..),
    CacheStats,
    LookupResult (..),
    cacheCompleteInflight,
    cacheInstallInflight,
    cacheLookup,
    cacheRemoveInflight,
    cacheStats,
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
    recordOracleTiming,
    recordPhysicalCompletion,
    recordPhysicalStart,
    recordRetired,
 )
import Occam.Oracle (
    OracleObservation (..),
    OracleObservationDetails (..),
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
    }

-- | Retired decision in candidate order: outcome plus cache-hit flag.
type WindowOutcome = (OracleOutcome, Bool)

data WindowDecision = WindowAccept | WindowReject
    deriving (Eq, Show)

data OraclePool = OraclePool
    { poolJobsCount :: !Int
    , poolGate :: !(MVar ())
    , poolSlots :: !QSem
    , poolPermitsVar :: !(MVar (Maybe Int))
    , poolCacheData :: !Cache
    , poolCacheModeData :: !CacheMode
    , poolPrecomputedData :: !(Map ByteString OracleObservation)
    , poolMetricsData :: !(Maybe MetricsStore)
    , poolExecuteFn :: !(ByteString -> IO OracleObservationDetails)
    , poolBillFn :: !(OracleObservationDetails -> IO ())
    , poolRetireFn :: !(Int -> ByteString -> OracleOutcome -> Bool -> IO ())
    , poolRegistry :: !(MVar (Map ThreadId (MVar ())))
    , poolIndexRef :: !(IORef Int)
    , poolAttemptsRef :: !(IORef Int)
    , poolCancelledRef :: !(IORef Int)
    , poolLiveRef :: !(IORef Int)
    }

newOraclePool :: PoolConfig -> IO OraclePool
newOraclePool PoolConfig {..}
    | poolJobs <= 0 = ioError (userError ("OraclePool jobs must be positive, got " ++ show poolJobs))
    | otherwise = do
        gate <- newMVar ()
        slots <- newQSem poolJobs
        permits <- newMVar poolPermits
        cache <- newCache poolPrecomputed
        registry <- newMVar Map.empty
        index <- newIORef 0
        attempts <- newIORef 0
        cancelled <- newIORef 0
        live <- newIORef 0
        pure
            OraclePool
                { poolJobsCount = poolJobs
                , poolGate = gate
                , poolSlots = slots
                , poolPermitsVar = permits
                , poolCacheData = cache
                , poolCacheModeData = if poolCacheEnabled then CacheEnabled else CacheDisabled
                , poolPrecomputedData = poolPrecomputed
                , poolMetricsData = poolMetrics
                , poolExecuteFn = poolExecute
                , poolBillFn = poolBill
                , poolRetireFn = poolRetire
                , poolRegistry = registry
                , poolIndexRef = index
                , poolAttemptsRef = attempts
                , poolCancelledRef = cancelled
                , poolLiveRef = live
                }

{- | Cancel and join every owned worker, then return once none remain. Windows
drain themselves before 'testOrdered' returns or throws, so this normally
observes an empty registry; it still terminates stray work instead of polling
for voluntary completion.
-}
closeOraclePool :: OraclePool -> IO ()
closeOraclePool pool = do
    snapshot <- readMVar (poolRegistry pool)
    unless (Map.null snapshot) $ do
        mapM_ killThread (Map.keys snapshot)
        mapM_ readMVar (Map.elems snapshot)
        mapM_ (deregWorker pool) (Map.keys snapshot)
        closeOraclePool pool

deregWorker :: OraclePool -> ThreadId -> IO ()
deregWorker pool tid = modifyMVar_ (poolRegistry pool) (pure . Map.delete tid)

withOraclePool :: PoolConfig -> (OraclePool -> IO a) -> IO a
withOraclePool config action = do
    pool <- newOraclePool config
    action pool `onException` closeOraclePool pool <* closeOraclePool pool

{- | In-process predicate pool for search-logic tests. Unlimited permits, no
billing or retirement effects; cache mode is explicit.
-}
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
            }

-- | Window-1 callback through the same ordered path; throws like 'testOrdered'.
poolOracle :: OraclePool -> Oracle
poolOracle pool candidate = do
    outcomes <- testOrdered pool [candidate]
    case outcomes of
        [(outcome, _)] -> pure outcome
        _ -> ioError (userError "OraclePool window-1 retired no outcome")

{- | The pool's shared cost store, if any. Single source of truth: reducers
read it through the pool instead of carrying their own pointer.
-}
poolMetricsStore :: OraclePool -> Maybe MetricsStore
poolMetricsStore = poolMetricsData

{- | Interaction wall from a worker start stamp to now, in microseconds. The
caller guarantees measurement is on; unmeasured paths never clock.
-}
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

{- | A finished worker observation with its measured execution wall: worker
start (after slot acquisition, so no queueing) through worker end.
Interaction wall, never CPU.
-}
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

{- | Test candidates in decision order with bounded speculation.

The input is consumed in job-sized chunks, so an early decision never forces
the rest of the list and a long input never spawns unbounded threads. Within
each chunk the scheduler reserves cache entries and execution permits strictly
in candidate order before forking, so an early head can never lose its permit
to later lookahead. At most 'poolJobs' physical executions run concurrently
(pool-global); a slot is held from execution start through process cleanup.
Retirement walks ordinals in order: the first acceptance commits, the first
inconclusive throws 'OracleInconclusiveError', and the first budget denial
throws 'ExecutionBudgetReached'. Every window drains fully before the call
returns or throws. Later losers are cancelled and reaped; their completed
physical work stays billed, while killed tasks count as cancelled, never as
timeouts or errors.

Concurrent 'testOrdered' calls serialize on the shared pool gate; derived
pools share it. The call is not reentrant.

Returns the retired prefix: outcomes through the first acceptance, or every
outcome when all candidates reject.
-}

-- | Untagged 'testOrderedTagged': every candidate retires as 'TagUnknown'.
testOrdered :: OraclePool -> [ByteString] -> IO [WindowOutcome]
testOrdered pool = testOrderedTagged pool . map (TagUnknown,)

{- | Tagged sibling sharing the identical launch/retire core: the single
scheduler engine. Tags travel launch to publish/retire/settle, so mixed
windows attribute retired decisions, physical starts/completions,
finished losers, and canceled walls per family.
-}
testOrderedTagged :: OraclePool -> [(FamilyTag, ByteString)] -> IO [WindowOutcome]
testOrderedTagged _ [] = pure []
testOrderedTagged pool candidates =
    bracket_ (takeMVar (poolGate pool)) (putMVar (poolGate pool) ()) (goChunks candidates [])
    where
        jobs = poolJobsCount pool
        goChunks [] acc = pure (reverse acc)
        goChunks rest acc = do
            let (window, remaining) = splitAt jobs rest
            (outcomes, decision) <- testWindow pool window
            let acc' = reverse outcomes ++ acc
            case decision of
                WindowAccept -> pure (reverse acc')
                WindowReject -> goChunks remaining acc'

testWindow :: OraclePool -> [(FamilyTag, ByteString)] -> IO ([WindowOutcome], WindowDecision)
testWindow pool tagged = do
    launched <- launchWindow pool tagged
    -- Highest retired window ordinal (-1 while none). Ordered retirement is a
    -- prefix, so cleanup classifies finished tasks against this single
    -- number instead of allocating a set; async cleanup reads the actual
    -- prefix rather than assuming none retired.
    retiredThrough <- newIORef (-1)
    let items = [(tag, candidate, (ordinal, task)) | ((tag, candidate), (ordinal, task)) <- zip tagged launched]
    retireWindow pool items retiredThrough
        `onException` (readIORef retiredThrough >>= settleAll pool launched)

launchWindow :: OraclePool -> [(FamilyTag, ByteString)] -> IO [(Int, WindowTask)]
launchWindow pool window = mask_ (go 0 [] window)
    where
        go _ acc [] = pure (reverse acc)
        go ordinal acc ((tag, candidate) : rest) = do
            task <- launchTask pool ordinal tag candidate `onException` settleAll pool acc (-1)
            go (ordinal + 1) ((ordinal, task) : acc) rest

launchTask :: OraclePool -> Int -> FamilyTag -> ByteString -> IO WindowTask
launchTask pool ordinal tag candidate = do
    look <-
        cacheLookup (poolCacheData pool) (poolCacheModeData pool) (poolPrecomputedData pool) candidate
    case look of
        LookupHit observation -> pure (TaskImmediate observation True)
        LookupJoin owner -> pure (TaskJoined owner)
        LookupMiss -> do
            reserved <- reservePermit (poolPermitsVar pool)
            if not reserved
                then pure TaskDenied
                else do
                    when (poolCacheModeData pool == CacheEnabled) $
                        cacheInstallInflight (poolCacheData pool) candidate ordinal
                    ( do
                            result <- newEmptyMVar
                            done <- newEmptyMVar
                            spent <- newMVar ()
                            startRef <- newIORef Nothing
                            mask_ $ do
                                tid <-
                                    forkIOWithUnmask $ \unmask -> do
                                        outcome <- try (unmask (runWorker pool tag candidate ordinal startRef))
                                        mask_ $ publishOutcome pool tag candidate ordinal startRef result done outcome
                                atomicModifyIORef' (poolLiveRef pool) (\n -> (n + 1, ()))
                                modifyMVar_ (poolRegistry pool) (pure . Map.insert tid done)
                                pure (TaskPending tid result done spent tag startRef)
                        )
                        `onException` ( do
                                            when (poolCacheModeData pool == CacheEnabled) $
                                                cacheRemoveInflight (poolCacheData pool) candidate ordinal
                                            refundPermit (poolPermitsVar pool)
                                      )

runWorker ::
    OraclePool -> FamilyTag -> ByteString -> Int -> IORef (Maybe Word64) -> IO TimedObservation
runWorker pool tag candidate ordinal startRef =
    bracket_ (waitQSem (poolSlots pool)) (signalQSem (poolSlots pool)) $ do
        startNs <- case poolMetricsData pool of
            Nothing -> pure 0
            Just _ -> getMonotonicTimeNSec
        mask_ $ do
            atomicModifyIORef' (poolAttemptsRef pool) (\n -> (n + 1, ()))
            -- A bare marker when unmeasured (permit refund only); a real stamp
            -- when measured. No clock runs without a store.
            writeIORef startRef (Just startNs)
            recordPhysicalStart (poolMetricsData pool) tag
        profile <- poolExecuteFn pool candidate
        mask_ $ do
            poolBillFn pool profile
            let observation = detailedObservation profile
            when (poolCacheModeData pool == CacheEnabled) $
                cacheCompleteInflight (poolCacheData pool) candidate ordinal (admitObservation observation)
            wallMicros <- case poolMetricsData pool of
                Nothing -> pure 0
                Just _ -> wallSince startNs
            pure (TimedObservation profile wallMicros)

-- | Masked final publication: every worker publishes exactly once, always.
publishOutcome ::
    OraclePool ->
    FamilyTag ->
    ByteString ->
    Int ->
    IORef (Maybe Word64) ->
    MVar (Either SomeException TimedObservation) ->
    MVar () ->
    Either SomeException TimedObservation ->
    IO ()
publishOutcome pool tag candidate ordinal startRef result done outcome = mask_ $ do
    case outcome of
        Right timed -> do
            recordPhysicalCompletion (poolMetricsData pool) tag
            let details = timedDetails timed
            recordOracleTiming
                (poolMetricsData pool)
                tag
                (oracleElapsedMicros details)
                (Just (resetElapsedMicros details))
            putMVar result (Right timed)
        Left err -> do
            started <- readIORef startRef
            case started of
                Nothing -> refundPermit (poolPermitsVar pool)
                Just _ -> pure ()
            when (poolCacheModeData pool == CacheEnabled) $
                cacheRemoveInflight (poolCacheData pool) candidate ordinal
            -- Attribute exactly once, here at worker end: a failure observed
            -- in settle would also charge all queue/retirement wait after the
            -- thread already ended. ThreadKilled is a genuine cancel; anything
            -- else is failed work, never a cancellation.
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

retireWindow ::
    OraclePool ->
    [(FamilyTag, ByteString, (Int, WindowTask))] ->
    IORef Int ->
    IO ([WindowOutcome], WindowDecision)
retireWindow pool items retiredThrough = go items [] IntMap.empty
    where
        launched = [(ordinal, task) | (_, _, (ordinal, task)) <- items]
        settleThrough = readIORef retiredThrough >>= settleAll pool launched
        go [] prefix _ = do
            settleThrough
            pure (reverse prefix, WindowReject)
        go ((tag, candidate, (ordinal, task)) : rest) prefix earlier = do
            (observation, hit) <- resolveTask pool launched retiredThrough earlier task
            let outcome = observationOutcome observation
                verdict = case outcome of
                    OracleAccepted -> RetiredAccept
                    OracleRejected -> RetiredReject
                    OracleInconclusive -> RetiredInconclusive
            -- The decision commits here, masked, before the user callback can
            -- throw: async cleanup always reads the actual retired prefix.
            mask_ $ do
                recordRetired (poolMetricsData pool) tag verdict
                writeIORef retiredThrough ordinal
            index <- atomicModifyIORef' (poolIndexRef pool) (\n -> (n + 1, n))
            poolRetireFn pool index candidate outcome hit
            let prefix' = (outcome, hit) : prefix
                earlier' = IntMap.insert ordinal observation earlier
            case outcome of
                OracleAccepted -> do
                    settleThrough
                    pure (reverse prefix', WindowAccept)
                OracleInconclusive -> do
                    settleThrough
                    throwIO OracleInconclusiveError
                OracleRejected -> go rest prefix' earlier'

resolveTask ::
    OraclePool ->
    [(Int, WindowTask)] ->
    IORef Int ->
    IntMap.IntMap OracleObservation ->
    WindowTask ->
    IO (OracleObservation, Bool)
resolveTask pool launched retiredThrough earlier task = case task of
    TaskImmediate observation hit -> pure (observation, hit)
    TaskJoined owner -> case IntMap.lookup owner earlier of
        Just observation -> pure (observation, True)
        Nothing -> ioError (userError "OraclePool join owner retired out of order")
    TaskDenied -> do
        settleThrough
        throwIO (ExecutionBudgetReached ())
    TaskPending _ result _ _ _ _ -> do
        published <- readMVar result
        case published of
            Right timed -> pure (detailedObservation (timedDetails timed), False)
            Left err -> do
                settleThrough
                throwIO err
    where
        settleThrough = readIORef retiredThrough >>= settleAll pool launched

{- | Settle every window task exactly once. Finished tasks at or below the
retired prefix join silently (their physical work is already billed); finished
tasks beyond it attribute their measured wall as losers. Unpublished tasks
only join and deregister here: their cancel/failed attribution already
happened at publication (worker end), so settle never charges queue or
retirement wait after the fact. Joins use persistent 'readMVar' and per-task
spent flags, so settling is idempotent and safe to repeat from exception
handlers; attribution happens only on the first settle.
-}
settleAll :: OraclePool -> [(Int, WindowTask)] -> Int -> IO ()
settleAll pool launched retiredThrough = mapM_ settleOne launched
    where
        settleOne (ordinal, TaskPending tid result done spent tag _) = do
            first <- tryTakeMVar spent
            case first of
                Nothing -> readMVar done
                Just () -> do
                    killThread tid
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
        settleOne _ = pure ()

reservePermit :: MVar (Maybe Int) -> IO Bool
reservePermit permits = modifyMVar permits reserve
    where
        reserve Nothing = pure (Nothing, True)
        reserve (Just n)
            | n > 0 = pure (Just (n - 1), True)
            | otherwise = pure (Just 0, False)

refundPermit :: MVar (Maybe Int) -> IO ()
refundPermit permits = modifyMVar_ permits \current -> pure $ case current of
    Nothing -> Nothing
    Just n -> Just (n + 1)

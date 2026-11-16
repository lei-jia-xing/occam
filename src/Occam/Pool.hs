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
    poolOracle,
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
import Control.Exception (Exception, SomeException, bracket_, mask_, onException, throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

data WindowTask
    = TaskImmediate !OracleObservation !Bool
    | TaskJoined !Int
    | TaskDenied
    | TaskPending !ThreadId !(MVar (Either SomeException OracleObservation)) !(MVar ()) !(MVar ())

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
testOrdered :: OraclePool -> [ByteString] -> IO [WindowOutcome]
testOrdered _ [] = pure []
testOrdered pool candidates =
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

testWindow :: OraclePool -> [ByteString] -> IO ([WindowOutcome], WindowDecision)
testWindow pool window = do
    launched <- launchWindow pool window
    let tasks = map snd launched
    retireWindow pool (zip window tasks) `onException` settleAll pool tasks

launchWindow :: OraclePool -> [ByteString] -> IO [(Int, WindowTask)]
launchWindow pool window = mask_ (go 0 [] window)
    where
        go _ acc [] = pure (reverse acc)
        go ordinal acc (candidate : rest) = do
            task <- launchTask pool ordinal candidate `onException` settleAll pool (map snd acc)
            go (ordinal + 1) ((ordinal, task) : acc) rest

launchTask :: OraclePool -> Int -> ByteString -> IO WindowTask
launchTask pool ordinal candidate = do
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
                            startedRef <- newIORef False
                            mask_ $ do
                                tid <-
                                    forkIOWithUnmask $ \unmask -> do
                                        outcome <- try (unmask (runWorker pool candidate ordinal startedRef))
                                        mask_ $ publishOutcome pool candidate ordinal startedRef result done outcome
                                atomicModifyIORef' (poolLiveRef pool) (\n -> (n + 1, ()))
                                modifyMVar_ (poolRegistry pool) (pure . Map.insert tid done)
                                pure (TaskPending tid result done spent)
                        )
                        `onException` ( do
                                            when (poolCacheModeData pool == CacheEnabled) $
                                                cacheRemoveInflight (poolCacheData pool) candidate ordinal
                                            refundPermit (poolPermitsVar pool)
                                      )

runWorker :: OraclePool -> ByteString -> Int -> IORef Bool -> IO OracleObservation
runWorker pool candidate ordinal startedRef =
    bracket_ (waitQSem (poolSlots pool)) (signalQSem (poolSlots pool)) $ do
        mask_ $ do
            started <- readIORef startedRef
            unless started $ do
                atomicModifyIORef' (poolAttemptsRef pool) (\n -> (n + 1, ()))
                atomicModifyIORef' startedRef (const (True, ()))
        profile <- poolExecuteFn pool candidate
        mask_ $ do
            poolBillFn pool profile
            let observation = detailedObservation profile
            when (poolCacheModeData pool == CacheEnabled) $
                cacheCompleteInflight (poolCacheData pool) candidate ordinal (admitObservation observation)
            pure observation

-- | Masked final publication: every worker publishes exactly once, always.
publishOutcome ::
    OraclePool ->
    ByteString ->
    Int ->
    IORef Bool ->
    MVar (Either SomeException OracleObservation) ->
    MVar () ->
    Either SomeException OracleObservation ->
    IO ()
publishOutcome pool candidate ordinal startedRef result done outcome = mask_ $ do
    case outcome of
        Right observation -> putMVar result (Right observation)
        Left err -> do
            started <- readIORef startedRef
            unless started $ refundPermit (poolPermitsVar pool)
            when (poolCacheModeData pool == CacheEnabled) $
                cacheRemoveInflight (poolCacheData pool) candidate ordinal
            putMVar result (Left err)
    atomicModifyIORef' (poolLiveRef pool) (\n -> (n - 1, ()))
    putMVar done ()

admitObservation :: OracleObservation -> Maybe OracleObservation
admitObservation observation = case observationAdmission observation of
    CacheAdmit -> Just observation
    CacheReject -> Nothing

retireWindow :: OraclePool -> [(ByteString, WindowTask)] -> IO ([WindowOutcome], WindowDecision)
retireWindow pool window = go 0 window [] IntMap.empty
    where
        tasks = map snd window
        go _ [] prefix _ = do
            settleAll pool tasks
            pure (reverse prefix, WindowReject)
        go ordinal ((candidate, task) : rest) prefix earlier = do
            (observation, hit) <- resolveTask pool tasks earlier task
            let outcome = observationOutcome observation
            index <- atomicModifyIORef' (poolIndexRef pool) (\n -> (n + 1, n))
            poolRetireFn pool index candidate outcome hit
            let prefix' = (outcome, hit) : prefix
                earlier' = IntMap.insert ordinal observation earlier
            case outcome of
                OracleAccepted -> do
                    settleAll pool tasks
                    pure (reverse prefix', WindowAccept)
                OracleInconclusive -> do
                    settleAll pool tasks
                    throwIO OracleInconclusiveError
                OracleRejected -> go (ordinal + 1) rest prefix' earlier'

resolveTask ::
    OraclePool ->
    [WindowTask] ->
    IntMap.IntMap OracleObservation ->
    WindowTask ->
    IO (OracleObservation, Bool)
resolveTask pool tasks earlier task = case task of
    TaskImmediate observation hit -> pure (observation, hit)
    TaskJoined owner -> case IntMap.lookup owner earlier of
        Just observation -> pure (observation, True)
        Nothing -> ioError (userError "OraclePool join owner retired out of order")
    TaskDenied -> do
        settleAll pool tasks
        throwIO (ExecutionBudgetReached ())
    TaskPending _ result _ _ -> do
        published <- readMVar result
        case published of
            Right observation -> pure (observation, False)
            Left err -> do
                settleAll pool tasks
                throwIO err

{- | Settle every window task exactly once. Completed tasks join silently (their
physical work is already billed); killed tasks count as cancelled. Joins use
persistent 'readMVar' and per-task spent flags, so settling is idempotent and
safe to repeat from exception handlers.
-}
settleAll :: OraclePool -> [WindowTask] -> IO ()
settleAll pool = mapM_ settleOne
    where
        settleOne (TaskPending tid result done spent) = do
            first <- tryTakeMVar spent
            case first of
                Nothing -> readMVar done
                Just () -> do
                    killThread tid
                    readMVar done
                    published <- readMVar result
                    case published of
                        Right _ -> pure ()
                        Left _ -> atomicModifyIORef' (poolCancelledRef pool) (\n -> (n + 1, ()))
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

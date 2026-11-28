module Occam.Pool.Types (
    ExecutionBudgetReached (..),
    OraclePool (..),
    PoolConfig (..),
    TimedObservation (..),
    WindowOutcome,
    WindowTask (..),
    WorkerEntry (..),
    admitObservation,
    checkReapFailure,
    closeOraclePool,
    deregWorker,
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
    refundPermit,
    reportCandidateCount,
    reservePermit,
    recordLookaheadRun,
    slidingLookahead,
    wallSince,
    withAdaptiveLookahead,
    withCandidateCountCleanup,
    withOraclePool,
    withPredicatePool,
) where

import Control.Concurrent (
    MVar,
    ThreadId,
    modifyMVar,
    modifyMVar_,
    newMVar,
    readMVar,
 )
import Control.Concurrent.QSem (QSem, newQSem)
import Control.Exception (
    Exception,
    SomeException,
    bracket,
    throwIO,
    uninterruptibleMask_,
 )
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Cache (
    Cache,
    CacheAdmission (..),
    CacheKey,
    CacheMode (..),
    CacheStats,
    cacheStats,
    makeKey,
    newCache,
    observationAdmission,
 )
import Occam.Core (OracleOutcome (..))
import Occam.Metrics (
    FamilyTag (..),
    MetricsStore,
    RetiredVerdict (..),
 )
import Occam.Oracle (
    OracleObservation (..),
    OracleObservationDetails (..),
    OracleReapFailed (..),
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
    , poolLookahead :: !(Maybe (IORef Lookahead))
    }

-- | Finished worker observation with its measured execution duration in microseconds.
data TimedObservation = TimedObservation
    { timedDetails :: !OracleObservationDetails
    , timedExecWallMicros :: !Int
    }

data WindowTask
    = TaskImmediate !OracleObservation !Bool
    | TaskJoined !(MVar (Either SomeException TimedObservation))
    | TaskDenied
    | TaskPending
        !ThreadId
        !(MVar (Either SomeException TimedObservation))
        !(MVar ())
        !(MVar ())
        !FamilyTag
        !(IORef (Maybe Word64))

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
                , poolLookahead = Nothing
                }

data Lookahead = Lookahead
    { lookaheadWidth :: !Int
    , lookaheadCosts :: !(Vector.Vector Integer)
    , lookaheadRuns :: !Integer
    , lookaheadAccepts :: !Integer
    , lookaheadAcceptedLength :: !Integer
    }

-- | Adapt the width using the observed stopping positions of fresh runs.
withAdaptiveLookahead :: OraclePool -> IO OraclePool
withAdaptiveLookahead pool
    | poolJobsCount pool <= 1 = pure pool
    | otherwise = do
        state <- newIORef (Lookahead (poolJobsCount pool) Vector.empty 0 0 0)
        pure pool {poolLookahead = Just state}

slidingLookahead :: OraclePool -> IO Int
slidingLookahead pool = case poolLookahead pool of
    Nothing -> pure (poolJobsCount pool)
    Just ref -> lookaheadWidth <$> readIORef ref

{- | Learn the cost of a constant width from completed, uncached runs.
A run of n retirements takes ceil(n / w) idealized waves. Acceptance can
leave the last wave partially useful, while exhaustion leaves no extra
work. Charge one unit per wave and one per speculative execution.
-}
recordLookaheadRun :: OraclePool -> Bool -> Int -> IO ()
recordLookaheadRun pool accepted retired =
    when (retired > 0) $ for_ (poolLookahead pool) $ \ref ->
        modifyIORef' ref update
    where
        update state@Lookahead {..} =
            let known = Vector.length lookaheadCosts
                limit = min (poolJobsCount pool) (max retired known)
                costAt index =
                    let width = index + 1
                        waves = toInteger ((retired - 1) `quot` width + 1)
                        unused = if accepted then waves * toInteger width - toInteger retired else 0
                        -- A previously untracked width fits each old run in one wave.
                        prior
                            | index < known = lookaheadCosts Vector.! index
                            | otherwise = lookaheadRuns + lookaheadAccepts * toInteger width - lookaheadAcceptedLength
                     in prior + waves + unused
                costs = Vector.generate limit costAt
                minimize incumbent index cost
                    | cost < costs Vector.! incumbent = index
                    | otherwise = incumbent
                best = Vector.ifoldl' minimize 0 costs
             in state
                    { lookaheadWidth = best + 1
                    , lookaheadCosts = costs
                    , lookaheadRuns = lookaheadRuns + 1
                    , lookaheadAccepts = lookaheadAccepts + if accepted then 1 else 0
                    , lookaheadAcceptedLength = lookaheadAcceptedLength + if accepted then toInteger retired else 0
                    }

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
withOraclePool config = bracket (newOraclePool config) closeOraclePool

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

outcomeVerdict :: OracleOutcome -> RetiredVerdict
outcomeVerdict OracleAccepted = RetiredAccept
outcomeVerdict OracleRejected = RetiredReject
outcomeVerdict OracleInconclusive = RetiredInconclusive

admitObservation :: OracleObservation -> Maybe OracleObservation
admitObservation observation = case observationAdmission observation of
    CacheAdmit -> Just observation
    CacheReject -> Nothing

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

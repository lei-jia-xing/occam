module Occam.Cache (
    CacheMode (..),
    CacheAdmission (..),
    CacheStats (..),
    CacheEntry (..),
    Cache (..),
    newCache,
    cacheLookup,
    cacheInstallInflight,
    cacheCompleteInflight,
    cacheRemoveInflight,
    cacheStats,
    LookupResult (..),
    observationAdmission,
) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Occam.Oracle (OracleObservation (..))

data CacheMode = CacheEnabled | CacheDisabled
    deriving (Eq, Show)

data CacheAdmission = CacheAdmit | CacheReject
    deriving (Eq, Show)

data CacheStats = CacheStats
    { cacheHits :: !Int
    , cacheMisses :: !Int
    , cacheJoins :: !Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON CacheStats
instance FromJSON CacheStats

{- | An in-flight entry records its owner's window ordinal. The scheduler
reserves entries in candidate order, so an owner always retires before its
joiners and joiners resolve from the owner's retired result without blocking.
-}
data CacheEntry
    = CacheDone !OracleObservation
    | CacheInflight !Int
    deriving (Eq, Show)

data Cache = Cache
    { cacheEntries :: !(MVar (Map ByteString CacheEntry))
    , cacheHitsRef :: !(IORef Int)
    , cacheMissesRef :: !(IORef Int)
    , cacheJoinsRef :: !(IORef Int)
    }

data LookupResult
    = LookupHit !OracleObservation
    | LookupJoin !Int
    | LookupMiss
    deriving (Eq, Show)

newCache :: Map ByteString OracleObservation -> IO Cache
newCache precomputed = do
    entries <- newMVar (Map.map CacheDone precomputed)
    hits <- newIORef 0
    misses <- newIORef 0
    joins <- newIORef 0
    pure
        Cache {cacheEntries = entries, cacheHitsRef = hits, cacheMissesRef = misses, cacheJoinsRef = joins}

{- | Ordered cache admission check. Precomputed entries are established facts,
served in either mode; they count as hits only when memoization is enabled so
@--no-cache@ metrics stay honest. Fresh hits and joins count only when enabled;
a disabled cache reports every fresh candidate as a miss so each one executes
physically, preserving @--no-cache@ semantics.
-}
cacheLookup ::
    Cache -> CacheMode -> Map ByteString OracleObservation -> ByteString -> IO LookupResult
cacheLookup cache mode precomputed sql = case Map.lookup sql precomputed of
    Just res -> case mode of
        CacheDisabled -> pure (LookupHit res)
        CacheEnabled -> do
            atomicModifyIORef' (cacheHitsRef cache) (\n -> (n + 1, ()))
            pure (LookupHit res)
    Nothing -> case mode of
        CacheDisabled -> do
            atomicModifyIORef' (cacheMissesRef cache) (\n -> (n + 1, ()))
            pure LookupMiss
        CacheEnabled -> do
            entries <- readMVar (cacheEntries cache)
            case Map.lookup sql entries of
                Just (CacheDone res) -> do
                    atomicModifyIORef' (cacheHitsRef cache) (\n -> (n + 1, ()))
                    pure (LookupHit res)
                Just (CacheInflight owner) -> do
                    atomicModifyIORef' (cacheJoinsRef cache) (\n -> (n + 1, ()))
                    pure (LookupJoin owner)
                Nothing -> do
                    atomicModifyIORef' (cacheMissesRef cache) (\n -> (n + 1, ()))
                    pure LookupMiss

cacheInstallInflight :: Cache -> ByteString -> Int -> IO ()
cacheInstallInflight cache sql ordinal =
    modifyMVar_ (cacheEntries cache) (pure . Map.insert sql (CacheInflight ordinal))

{- | Publish a completed miss. Admitted observations are retained; rejected
ones (timeouts, reset failures, I/O failures) are removed so a later window
re-observes them physically. Only the owning ordinal may complete its entry.
-}
cacheCompleteInflight :: Cache -> ByteString -> Int -> Maybe OracleObservation -> IO ()
cacheCompleteInflight cache sql ordinal admitted =
    modifyMVar_ (cacheEntries cache) \entries -> case Map.lookup sql entries of
        Just (CacheInflight owner)
            | owner == ordinal -> pure $ case admitted of
                Just res -> Map.insert sql (CacheDone res) entries
                Nothing -> Map.delete sql entries
        _ -> pure entries

-- | Drop an in-flight entry owned by the given ordinal (worker cancellation).
cacheRemoveInflight :: Cache -> ByteString -> Int -> IO ()
cacheRemoveInflight cache sql ordinal = cacheCompleteInflight cache sql ordinal Nothing

cacheStats :: Cache -> IO CacheStats
cacheStats cache = do
    hits <- readIORef (cacheHitsRef cache)
    misses <- readIORef (cacheMissesRef cache)
    joins <- readIORef (cacheJoinsRef cache)
    pure CacheStats {cacheHits = hits, cacheMisses = misses, cacheJoins = joins}

{- | Only completed process observations are authoritative; timeouts, reset
failures, and I/O failures are re-observed physically on later windows.
-}
observationAdmission :: OracleObservation -> CacheAdmission
observationAdmission OracleFinished {} = CacheAdmit
observationAdmission _ = CacheReject

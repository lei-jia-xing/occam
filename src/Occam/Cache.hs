module Occam.Cache (
    CacheMode (..),
    CacheAdmission (..),
    CacheStats (..),
    CacheEntry (..),
    CacheKey (..),
    Cache (..),
    newCache,
    makeKey,
    makeKeyWithFingerprint,
    nativeCandidateKey,
    cacheLookup,
    cacheInstallInflight,
    cacheCompleteInflight,
    cacheInsertDone,
    cacheRemoveInflight,
    cacheLookupKey,
    cacheInstallInflightKey,
    cacheCompleteInflightKey,
    cacheInsertDoneKey,
    cacheStats,
    LookupResult (..),
    observationAdmission,
) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable (..))
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

-- | Cache key storing a precalculated candidate fingerprint alongside exact bytes.
data CacheKey = CacheKey
    { keyFingerprint :: {-# UNPACK #-} !Int
    , keyBytes :: !ByteString
    }

instance Eq CacheKey where
    {-# INLINE (==) #-}
    CacheKey fp1 b1 == CacheKey fp2 b2 = fp1 == fp2 && b1 == b2

instance Hashable CacheKey where
    {-# INLINE hashWithSalt #-}
    hashWithSalt s (CacheKey fp _) = hashWithSalt s fp
    {-# INLINE hash #-}
    hash (CacheKey fp _) = fp

{-# INLINE makeKey #-}
makeKey :: ByteString -> CacheKey
makeKey bs = makeKeyWithFingerprint (hash bs) bs

{-# INLINE makeKeyWithFingerprint #-}
makeKeyWithFingerprint :: Int -> ByteString -> CacheKey
makeKeyWithFingerprint = CacheKey

-- | Cache entry state: precomputed, completed, or in-flight with owner ordinal.
data CacheEntry
    = CachePrecomputed !OracleObservation
    | CacheDone !OracleObservation
    | CacheInflight !Int
    deriving (Eq, Show)

data Cache = Cache
    { cacheEntries :: !(MVar (HashMap CacheKey CacheEntry))
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
    entries <-
        newMVar (HashMap.fromList [(makeKey k, CachePrecomputed v) | (k, v) <- Map.toList precomputed])
    hits <- newIORef 0
    misses <- newIORef 0
    joins <- newIORef 0
    pure
        Cache {cacheEntries = entries, cacheHitsRef = hits, cacheMissesRef = misses, cacheJoinsRef = joins}

{- | Reserve the NUL-prefixed namespace for lexer-certified native keys. Escape
a leading NUL on an exact key so even invalid SQL cannot alias this namespace.
-}
nativeCandidateKey :: (ByteString -> IO (Maybe ByteString)) -> ByteString -> IO ByteString
nativeCandidateKey prove sql = do
    canonical <- prove sql
    pure $ case canonical of
        Just key -> BS.append "\0\1" key
        Nothing
            | BS.isPrefixOf (BS.singleton 0) sql -> BS.cons 0 sql
            | otherwise -> sql

-- | Look up candidate bytes in the cache according to the cache mode.
cacheLookup :: Cache -> CacheMode -> ByteString -> IO LookupResult
cacheLookup cache mode = cacheLookupKey cache mode . makeKey

cacheLookupKey :: Cache -> CacheMode -> CacheKey -> IO LookupResult
cacheLookupKey cache mode !k = do
    entries <- readMVar (cacheEntries cache)
    case mode of
        CacheDisabled -> case HashMap.lookup k entries of
            Just (CachePrecomputed res) -> pure (LookupHit res)
            _ -> do
                atomicModifyIORef' (cacheMissesRef cache) (\n -> (n + 1, ()))
                pure LookupMiss
        CacheEnabled -> case HashMap.lookup k entries of
            Just (CachePrecomputed res) -> countHit res
            Just (CacheDone res) -> countHit res
            Just (CacheInflight owner) -> do
                atomicModifyIORef' (cacheJoinsRef cache) (\n -> (n + 1, ()))
                pure (LookupJoin owner)
            Nothing -> do
                atomicModifyIORef' (cacheMissesRef cache) (\n -> (n + 1, ()))
                pure LookupMiss
    where
        countHit res = do
            atomicModifyIORef' (cacheHitsRef cache) (\n -> (n + 1, ()))
            pure (LookupHit res)

cacheInstallInflight :: Cache -> ByteString -> Int -> IO ()
cacheInstallInflight cache = cacheInstallInflightKey cache . makeKey

cacheInstallInflightKey :: Cache -> CacheKey -> Int -> IO ()
cacheInstallInflightKey cache !key ordinal =
    modifyMVar_ (cacheEntries cache) \entries ->
        pure $! HashMap.insert key (CacheInflight ordinal) entries

-- | Directly insert a completed observation into the cache (serial path).
cacheInsertDone :: Cache -> ByteString -> OracleObservation -> IO ()
cacheInsertDone cache = cacheInsertDoneKey cache . makeKey

cacheInsertDoneKey :: Cache -> CacheKey -> OracleObservation -> IO ()
cacheInsertDoneKey cache !key res =
    modifyMVar_ (cacheEntries cache) \entries ->
        pure $! HashMap.insert key (CacheDone res) entries

-- | Complete an in-flight entry with an admitted observation or remove it.
cacheCompleteInflight :: Cache -> ByteString -> Int -> Maybe OracleObservation -> IO ()
cacheCompleteInflight cache = cacheCompleteInflightKey cache . makeKey

cacheCompleteInflightKey :: Cache -> CacheKey -> Int -> Maybe OracleObservation -> IO ()
cacheCompleteInflightKey cache !key ordinal admitted =
    modifyMVar_ (cacheEntries cache) \entries -> case HashMap.lookup key entries of
        Just (CacheInflight owner)
            | owner == ordinal -> case admitted of
                Just res -> pure $! HashMap.insert key (CacheDone res) entries
                Nothing -> pure $! HashMap.delete key entries
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

-- | Admit only finished executions into the cache.
observationAdmission :: OracleObservation -> CacheAdmission
observationAdmission OracleFinished {} = CacheAdmit
observationAdmission _ = CacheReject

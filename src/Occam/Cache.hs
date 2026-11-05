{-# LANGUAGE DeriveGeneric #-}

module Occam.Cache (
    CacheStats (..),
    memoizeOracle,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Occam.Reducer (Oracle)

data CacheStats = CacheStats
    { cacheHits :: !Int
    , cacheMisses :: !Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON CacheStats
instance FromJSON CacheStats

{- | Wraps an Oracle with an in-memory memoization cache.
If a candidate SQL was already evaluated, returns the cached result and increments cacheHits.
Otherwise, calls the underlying Oracle, caches the result, and increments cacheMisses.
-}
memoizeOracle :: Oracle -> IO (Oracle, IO CacheStats)
memoizeOracle oracle = do
    cacheRef <- newIORef (Map.empty :: Map ByteString Bool)
    hitsRef <- newIORef (0 :: Int)
    missesRef <- newIORef (0 :: Int)
    let cachedOracle sql = do
            cache <- readIORef cacheRef
            case Map.lookup sql cache of
                Just res -> do
                    modifyIORef' hitsRef (+ 1)
                    pure res
                Nothing -> do
                    res <- oracle sql
                    modifyIORef' missesRef (+ 1)
                    modifyIORef' cacheRef (Map.insert sql res)
                    pure res
        getStats = do
            hits <- readIORef hitsRef
            misses <- readIORef missesRef
            pure
                CacheStats
                    { cacheHits = hits
                    , cacheMisses = misses
                    }
    pure (cachedOracle, getStats)

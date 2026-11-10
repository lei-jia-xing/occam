{-# LANGUAGE DeriveGeneric #-}

module Occam.Cache (
    CacheStats (..),
    memoizeOracle,
    memoizeOracleWith,
) where

import Control.Monad (when)
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

-- | Memoize completed results. Failed actions are counted as misses but never cached.
memoizeOracle :: Oracle -> IO (Oracle, IO CacheStats)
memoizeOracle = memoizeOracleWith True Map.empty (const True)

{- | Memoize completed results, starting from a set of observations that are
already known to be authoritative. Precomputed entries are served even when
memoization is disabled: they are established facts, not speculative cache
entries, and re-running them would only duplicate work. They are not counted as
cache hits while memoization is disabled, so @--no-cache@ metrics stay honest.
-}
memoizeOracleWith ::
    Bool ->
    Map ByteString a ->
    (a -> Bool) ->
    (ByteString -> IO a) ->
    IO (ByteString -> IO a, IO CacheStats)
memoizeOracleWith enabled precomputed cacheResult oracle = do
    cacheRef <- newIORef precomputed
    hitsRef <- newIORef (0 :: Int)
    missesRef <- newIORef (0 :: Int)
    let cachedOracle sql
            | not enabled =
                case Map.lookup sql precomputed of
                    Just res -> pure res
                    Nothing -> do
                        modifyIORef' missesRef (+ 1)
                        oracle sql
            | otherwise = do
                cache <- readIORef cacheRef
                case Map.lookup sql cache of
                    Just res -> do
                        modifyIORef' hitsRef (+ 1)
                        pure res
                    Nothing -> do
                        modifyIORef' missesRef (+ 1)
                        res <- oracle sql
                        when (cacheResult res) $
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

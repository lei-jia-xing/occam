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
memoizeOracle = memoizeOracleWith True (const True)

memoizeOracleWith ::
    Bool ->
    (a -> Bool) ->
    (ByteString -> IO a) ->
    IO (ByteString -> IO a, IO CacheStats)
memoizeOracleWith enabled cacheResult oracle = do
    cacheRef <- newIORef Map.empty
    hitsRef <- newIORef (0 :: Int)
    missesRef <- newIORef (0 :: Int)
    let cachedOracle sql
            | not enabled = do
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

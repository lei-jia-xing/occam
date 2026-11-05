{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Benchmark (
    BenchmarkMetric (..),
    runComparisonBenchmark,
    runSingleAlgorithmBenchmark,
) where

import Baseline (runNaiveDdmin, runSequentialDrop)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Occam.Reducer (Oracle, reduceSQL)
import Occam.TreeSitter (countStatements)

data BenchmarkMetric = BenchmarkMetric
    { metricAlgorithm :: !Text
    , metricInitialBytes :: !Int
    , metricReducedBytes :: !Int
    , metricByteReduction :: !Double
    , metricInitialStmts :: !Int
    , metricReducedStmts :: !Int
    , metricStmtReduction :: !Double
    , metricOracleCalls :: !Int
    , metricTimeSeconds :: !Double
    }
    deriving (Eq, Show, Generic)

instance ToJSON BenchmarkMetric
instance FromJSON BenchmarkMetric

-- | Run a single reducer algorithm and collect metrics along with reduced output.
runOne ::
    Int ->
    Int ->
    Oracle ->
    ByteString ->
    (Text, Oracle -> ByteString -> IO ByteString) ->
    IO (BenchmarkMetric, ByteString)
runOne initialBytes initialStmts oracle sql (name, runAlg) = do
    counter <- newIORef 0
    let countedOracle cand = do
            modifyIORef' counter (+ 1)
            oracle cand
    t0 <- getCurrentTime
    reducedSQL <- runAlg countedOracle sql
    t1 <- getCurrentTime
    calls <- readIORef counter
    reducedStmts <- countStatements reducedSQL
    let reducedBytes = BS.length reducedSQL
        timeSecs = realToFrac (diffUTCTime t1 t0) :: Double
        byteRed =
            if initialBytes > 0
                then (1.0 - (fromIntegral reducedBytes / fromIntegral initialBytes :: Double)) * 100.0
                else 0.0
        stmtRed =
            if initialStmts > 0
                then (1.0 - (fromIntegral reducedStmts / fromIntegral initialStmts :: Double)) * 100.0
                else 0.0
        metric =
            BenchmarkMetric
                { metricAlgorithm = name
                , metricInitialBytes = initialBytes
                , metricReducedBytes = reducedBytes
                , metricByteReduction = byteRed
                , metricInitialStmts = initialStmts
                , metricReducedStmts = reducedStmts
                , metricStmtReduction = stmtRed
                , metricOracleCalls = calls
                , metricTimeSeconds = timeSecs
                }
    pure (metric, reducedSQL)

-- | Run comparative benchmark: Occam vs B1 (Sequential) vs B2 (Naive ddmin).
runComparisonBenchmark :: Oracle -> ByteString -> IO [BenchmarkMetric]
runComparisonBenchmark oracle sql = do
    let initialBytes = BS.length sql
    initialStmts <- countStatements sql
    let configs =
            [ ("Occam", reduceSQL)
            , ("B1 (Sequential)", runSequentialDrop)
            , ("B2 (Naive ddmin)", runNaiveDdmin)
            ]
    mapM (fmap fst . runOne initialBytes initialStmts oracle sql) configs

-- | Run a single benchmark algorithm by baseline index (1: Sequential, 2: Naive ddmin).
runSingleAlgorithmBenchmark :: Int -> Oracle -> ByteString -> IO (BenchmarkMetric, ByteString)
runSingleAlgorithmBenchmark baselineIdx oracle sql = do
    let initialBytes = BS.length sql
    initialStmts <- countStatements sql
    let cfg = case baselineIdx of
            1 -> ("B1 (Sequential)", runSequentialDrop)
            2 -> ("B2 (Naive ddmin)", runNaiveDdmin)
            _ -> ("Occam", reduceSQL)
    runOne initialBytes initialStmts oracle sql cfg

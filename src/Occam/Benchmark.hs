{-# LANGUAGE DeriveGeneric #-}

module Occam.Benchmark (
    BenchmarkMetric (..),
    runComparisonBenchmark,
    formatMarkdownTable,
    formatCSV,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Occam.Baseline (runNaiveDdmin, runSequentialDrop)
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
    mapM (runOne initialBytes initialStmts) configs
    where
        runOne initialBytes initialStmts (name, runAlg) = do
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
            pure
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

-- | Render a Markdown comparison table.
formatMarkdownTable :: [BenchmarkMetric] -> Text
formatMarkdownTable metrics =
    T.unlines
        ( [ "| Algorithm | Initial Bytes | Reduced Bytes | Byte Red. % | Initial Stmts | Reduced Stmts | Stmt Red. % | Oracle Invocations | Time (s) |"
          , "| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"
          ]
            ++ map formatRow metrics
        )
    where
        formatRow BenchmarkMetric {..} =
            "| "
                <> T.intercalate
                    " | "
                    [ metricAlgorithm
                    , showT metricInitialBytes
                    , showT metricReducedBytes
                    , fmtPct metricByteReduction
                    , showT metricInitialStmts
                    , showT metricReducedStmts
                    , fmtPct metricStmtReduction
                    , showT metricOracleCalls
                    , fmtSecs metricTimeSeconds
                    ]
                <> " |"

        showT = T.pack . show
        fmtPct v = T.pack (showFFloat (Just 1) v "%")
        fmtSecs v = T.pack (showFFloat (Just 3) v "")

-- | Emit standard CSV for plotting and data analysis.
formatCSV :: [BenchmarkMetric] -> Text
formatCSV metrics =
    T.unlines
        ( "Algorithm,Initial Bytes,Reduced Bytes,Byte Red. %,Initial Stmts,Reduced Stmts,Stmt Red. %,Oracle Invocations,Time (s)"
            : map formatRow metrics
        )
    where
        formatRow BenchmarkMetric {..} =
            T.intercalate
                ","
                [ metricAlgorithm
                , showT metricInitialBytes
                , showT metricReducedBytes
                , fmtPct metricByteReduction
                , showT metricInitialStmts
                , showT metricReducedStmts
                , fmtPct metricStmtReduction
                , showT metricOracleCalls
                , fmtSecs metricTimeSeconds
                ]

        showT = T.pack . show
        fmtPct v = T.pack (showFFloat (Just 2) v "")
        fmtSecs v = T.pack (showFFloat (Just 4) v "")

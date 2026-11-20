module Occam.Benchmark.Metrics (
    Stats (..),
    RunSummary (..),
    computeAttemptAuc,
    engineOverheadMicros,
    summarizeStats,
    missingStatsSummary,
    summarizeDirectory,
    renderMarkdownReport,
    renderSummaryJson,
    writeReport,
    generateReportForDirectory,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    eitherDecodeStrict',
    encode,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Occam.Evaluation (
    EvaluationMetrics (..),
    EvaluationPoint (..),
    Stats (..),
    evaluationStatusName,
 )
import Occam.Metrics (FamilyCounters (..), MetricsSnapshot (..))
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.FilePath ((</>))

-- | 4-dimension summary of a single benchmark run.
data RunSummary = RunSummary
    { runName :: !Text
    , runCase :: !(Maybe Text)
    , runArm :: !(Maybe Text)
    , runRep :: !(Maybe Text)
    , runReducedBytes :: !(Maybe Int)
    , runReducedStatements :: !(Maybe Int)
    , runReductionRatio :: !(Maybe Double)
    , runOracleAttempts :: !(Maybe Int)
    , runAccepted :: !(Maybe Int)
    , runCacheHits :: !(Maybe Int)
    , runAttemptAuc :: !(Maybe Double)
    , runEngineOverheadUs :: !(Maybe Int)
    , runElapsedSeconds :: !(Maybe Double)
    , runStatus :: !Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON RunSummary where
    toJSON RunSummary {..} =
        object
            [ "run" .= runName
            , "case" .= runCase
            , "arm" .= runArm
            , "rep" .= runRep
            , "reduced_bytes" .= runReducedBytes
            , "reduced_statements" .= runReducedStatements
            , "reduction_ratio" .= runReductionRatio
            , "oracle_attempts" .= runOracleAttempts
            , "accepted" .= runAccepted
            , "cache_hits" .= runCacheHits
            , "attempt_auc" .= runAttemptAuc
            , "engine_overhead_us" .= runEngineOverheadUs
            , "elapsed_seconds" .= runElapsedSeconds
            , "status" .= runStatus
            ]

instance FromJSON RunSummary where
    parseJSON = withObject "RunSummary" \o ->
        RunSummary
            <$> o .: "run"
            <*> o .:? "case"
            <*> o .:? "arm"
            <*> o .:? "rep"
            <*> o .:? "reduced_bytes"
            <*> o .:? "reduced_statements"
            <*> o .:? "reduction_ratio"
            <*> o .:? "oracle_attempts"
            <*> o .:? "accepted"
            <*> o .:? "cache_hits"
            <*> o .:? "attempt_auc"
            <*> o .:? "engine_overhead_us"
            <*> o .:? "elapsed_seconds"
            <*> o .: "status"

{- | Normalized area under the attempt-versus-size evaluation curve over @1..total@.
Lower is faster/better. At attempt @i@, the current best size is the minimum
accepted size up to that call index.
-}
computeAttemptAuc :: Int -> [EvaluationPoint] -> Int -> Double
computeAttemptAuc initialBytes curve totalAttempts
    | initialBytes <= 0 || totalAttempts <= 0 = 1.0
    | otherwise =
        let bestAt =
                IntMap.fromListWith
                    min
                    [ (pointCallIndex p, pointCandidateBytes p)
                    | p <- curve
                    , pointAccepted p
                    ]
            step (!best, !area) i =
                let !curBest = maybe best (min best) (IntMap.lookup i bestAt)
                 in (curBest, area + (fromIntegral curBest / fromIntegral initialBytes))
            (_, totalArea) = foldl' step (initialBytes, 0.0) [1 .. totalAttempts]
         in totalArea / fromIntegral totalAttempts

-- | Parse FFI + decode + candidate-build micros from cost attribution.
engineOverheadMicros :: Maybe MetricsSnapshot -> Int
engineOverheadMicros Nothing = 0
engineOverheadMicros (Just MetricsSnapshot {..}) =
    let ffi = sum (Map.elems snapshotParseFfiMicros)
        decode = sum (Map.elems snapshotParseDecodeMicros)
        candidateBuild = sum [candidateBuildMicros c | c <- Map.elems snapshotFamilies]
     in ffi + decode + candidateBuild

parseRunParts :: Text -> (Maybe Text, Maybe Text, Maybe Text)
parseRunParts name =
    case T.splitOn "__" name of
        [c, a, r] -> (Just c, Just a, Just r)
        _ -> (Nothing, Nothing, Nothing)

-- | Summarize a successful run with its full stats.
summarizeStats :: Text -> Stats -> RunSummary
summarizeStats name stats =
    let (c, a, r) = parseRunParts name
        m = evaluation_metrics stats
        curve = evaluationCurve m
        peak = maximum (0 : [pointCallIndex p | p <- curve])
        total = max (oracle_attempts stats) peak
        auc = computeAttemptAuc (initial_bytes stats) curve total
        overhead = engineOverheadMicros (evaluationCostAttribution m)
        elapsed =
            let rSecs = evaluationReductionSeconds m
             in if rSecs > 0 then rSecs else elapsed_seconds stats
     in RunSummary
            { runName = name
            , runCase = c
            , runArm = a
            , runRep = r
            , runReducedBytes = Just (reduced_bytes stats)
            , runReducedStatements = Just (reduced_statements stats)
            , runReductionRatio = Just (reduction_ratio stats)
            , runOracleAttempts = Just (oracle_attempts stats)
            , runAccepted = Just (evaluationAcceptedObservations m)
            , runCacheHits = Just (cache_hits stats)
            , runAttemptAuc = Just auc
            , runEngineOverheadUs = Just overhead
            , runElapsedSeconds = Just elapsed
            , runStatus = evaluationStatusName (status stats)
            }

-- | Summary for a missing or corrupt run directory.
missingStatsSummary :: Text -> RunSummary
missingStatsSummary name =
    let (c, a, r) = parseRunParts name
     in RunSummary
            { runName = name
            , runCase = c
            , runArm = a
            , runRep = r
            , runReducedBytes = Nothing
            , runReducedStatements = Nothing
            , runReductionRatio = Nothing
            , runOracleAttempts = Nothing
            , runAccepted = Nothing
            , runCacheHits = Nothing
            , runAttemptAuc = Nothing
            , runEngineOverheadUs = Nothing
            , runElapsedSeconds = Nothing
            , runStatus = "missing-stats"
            }

-- | Scan a runs directory and produce summaries for each child directory.
summarizeDirectory :: FilePath -> IO [RunSummary]
summarizeDirectory dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else do
            entries <- sort <$> listDirectory dir
            results <- mapM processChild entries
            pure (catMaybes results)
    where
        processChild name = do
            let childPath = dir </> name
            isDir <- doesDirectoryExist childPath
            if not isDir
                then pure Nothing
                else do
                    let statsFile = childPath </> "stats.json"
                    hasStats <- doesFileExist statsFile
                    if not hasStats
                        then pure (Just (missingStatsSummary (T.pack name)))
                        else do
                            content <- BS.readFile statsFile
                            case eitherDecodeStrict' content of
                                Left _ -> pure (Just (missingStatsSummary (T.pack name)))
                                Right stats -> pure (Just (summarizeStats (T.pack name) stats))

-- | Format runs as a GitHub Flavored Markdown table matching Tier 3 4D schema.
renderMarkdownReport :: [RunSummary] -> Text
renderMarkdownReport runs =
    let columns =
            [ "run"
            , "reduced_bytes"
            , "reduced_statements"
            , "reduction_ratio"
            , "oracle_attempts"
            , "accepted"
            , "cache_hits"
            , "attempt_auc"
            , "engine_overhead_us"
            , "elapsed_seconds"
            , "status"
            ]
        header = "| " <> T.intercalate " | " columns <> " |"
        sep = "|" <> T.intercalate "|" (replicate (length columns) "---") <> "|"
        formatFloat x = T.pack (showFFloat (Just 4) x "")
        renderRow RunSummary {..} =
            let formatField name = case name of
                    "run" -> runName
                    "reduced_bytes" -> maybe "None" (T.pack . show) runReducedBytes
                    "reduced_statements" -> maybe "None" (T.pack . show) runReducedStatements
                    "reduction_ratio" -> maybe "None" formatFloat runReductionRatio
                    "oracle_attempts" -> maybe "None" (T.pack . show) runOracleAttempts
                    "accepted" -> maybe "None" (T.pack . show) runAccepted
                    "cache_hits" -> maybe "None" (T.pack . show) runCacheHits
                    "attempt_auc" -> maybe "None" formatFloat runAttemptAuc
                    "engine_overhead_us" -> maybe "None" (T.pack . show) runEngineOverheadUs
                    "elapsed_seconds" -> maybe "None" formatFloat runElapsedSeconds
                    "status" -> runStatus
                    _ -> ""
             in "| " <> T.intercalate " | " (map formatField columns) <> " |"
        lines' =
            [ "# Benchmark report"
            , ""
            , header
            , sep
            ]
                ++ map renderRow runs
     in T.unlines lines'

-- | Encode runs into summary JSON payload format @{\"runs\": [...]}@.
renderSummaryJson :: [RunSummary] -> Value
renderSummaryJson runs =
    object ["runs" .= runs]

-- | Write @summary.json@ and @report.md@ to the target directory.
writeReport :: FilePath -> [RunSummary] -> IO ()
writeReport runDir runs = do
    let summaryPath = runDir </> "summary.json"
        reportPath = runDir </> "report.md"
    BSL.writeFile summaryPath (encode (renderSummaryJson runs))
    TIO.writeFile reportPath (renderMarkdownReport runs)

-- | Scan directory, summarize all runs, and write @summary.json@ and @report.md@.
generateReportForDirectory :: FilePath -> IO [RunSummary]
generateReportForDirectory dir = do
    runs <- summarizeDirectory dir
    writeReport dir runs
    pure runs

module Occam.Benchmark.Metrics (
    Stats (..),
    RunSummary (..),
    computeAttemptAuc,
    engineOverheadMicros,
    summarizeStats,
    missingStatsSummary,
    summarizeDirectory,
    renderSummaryJson,
    writeReport,
    generateReportForDirectory,
) where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
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
import GHC.Generics (Generic)
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
    , runNameWarning :: !(Maybe Text)
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
            , "name_warning" .= runNameWarning
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
            <*> o .:? "name_warning"
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

-- | Normalized area under the attempt-versus-size evaluation curve over @1..total@.
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
engineOverheadMicros :: Maybe MetricsSnapshot -> Maybe Int
engineOverheadMicros Nothing = Nothing
engineOverheadMicros (Just MetricsSnapshot {..}) =
    let ffi = sum (Map.elems snapshotParseFfiMicros)
        decode = sum (Map.elems snapshotParseDecodeMicros)
        candidateBuild = sum [candidateBuildMicros c | c <- Map.elems snapshotFamilies]
     in Just (ffi + decode + candidateBuild)

data RunNameParts = RunNameParts
    { partCase :: !(Maybe Text)
    , partArm :: !(Maybe Text)
    , partRep :: !(Maybe Text)
    , partWarning :: !(Maybe Text)
    }
    deriving stock (Eq, Show)

-- | Split @case__arm__rep@ run identifier into components.
parseRunParts :: Text -> RunNameParts
parseRunParts name =
    case T.splitOn "__" name of
        c : a : r : rest
            | not (any T.null [c, a, r]) ->
                RunNameParts
                    { partCase = Just c
                    , partArm = Just a
                    , partRep = Just (T.intercalate "__" (r : rest))
                    , partWarning = Nothing
                    }
        _ ->
            RunNameParts
                { partCase = Nothing
                , partArm = Nothing
                , partRep = Nothing
                , partWarning = Just ("expected case__arm__rep, got " <> T.pack (show name))
                }

-- | Summarize a successful run with its full stats.
summarizeStats :: Text -> Stats -> RunSummary
summarizeStats name stats =
    let parts = parseRunParts name
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
            , runCase = partCase parts
            , runArm = partArm parts
            , runRep = partRep parts
            , runNameWarning = partWarning parts
            , runReducedBytes = Just (reduced_bytes stats)
            , runReducedStatements = Just (reduced_statements stats)
            , runReductionRatio = Just (reduction_ratio stats)
            , runOracleAttempts = Just (oracle_attempts stats)
            , runAccepted = Just (evaluationAcceptedObservations m)
            , runCacheHits = Just (cache_hits stats)
            , runAttemptAuc = Just auc
            , runEngineOverheadUs = overhead
            , runElapsedSeconds = Just elapsed
            , runStatus = evaluationStatusName (status stats)
            }

-- | Summary for a missing or corrupt run directory.
missingStatsSummary :: Text -> RunSummary
missingStatsSummary name =
    let parts = parseRunParts name
     in RunSummary
            { runName = name
            , runCase = partCase parts
            , runArm = partArm parts
            , runRep = partRep parts
            , runNameWarning = partWarning parts
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
            catMaybes <$> mapM processChild entries
    where
        processChild name = runMaybeT do
            let childPath = dir </> name
            guard =<< liftIO (doesDirectoryExist childPath)
            liftIO $ readRunSummary (T.pack name) (childPath </> "stats.json")

readRunSummary :: Text -> FilePath -> IO RunSummary
readRunSummary runNameText statsFile = do
    hasStats <- doesFileExist statsFile
    if not hasStats
        then pure (missingStatsSummary runNameText)
        else do
            content <- BS.readFile statsFile
            pure $
                either
                    (const (missingStatsSummary runNameText))
                    (summarizeStats runNameText)
                    (eitherDecodeStrict' content)

-- | Encode runs into summary JSON payload format @{\"runs\": [...]}@.
renderSummaryJson :: [RunSummary] -> Value
renderSummaryJson runs =
    object ["runs" .= runs]

-- | Write @summary.json@ to the target directory.
writeReport :: FilePath -> [RunSummary] -> IO ()
writeReport runDir runs = do
    let summaryPath = runDir </> "summary.json"
    BSL.writeFile summaryPath (encode (renderSummaryJson runs))

-- | Scan directory, summarize all runs, and write @summary.json@.
generateReportForDirectory :: FilePath -> IO [RunSummary]
generateReportForDirectory dir = do
    runs <- summarizeDirectory dir
    writeReport dir runs
    pure runs

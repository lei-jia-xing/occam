-- | Per-family speculation, parse latency, and cancellation telemetry store and snapshot types.
module Occam.Metrics (
    MetricPolicy (..),
    FamilyTag (..),
    ParseKind (..),
    RetiredVerdict (..),
    MetricsStore,
    MetricsSnapshot (..),
    FamilyCounters (..),
    emptyFamilyCounters,
    newMetricsStore,
    snapshotMetrics,
    recordPhysicalStart,
    recordRetired,
    recordPhysicalCompletion,
    recordCompletedUnselected,
    recordCanceled,
    recordFailed,
    recordGeneratedSkipped,
    recordParseTiming,
    recordCandidateBuildTiming,
    recordAcceptedBytesSaved,
    recordOracleTiming,
    recordPhysicalCompletionAndTiming,
) where

import Data.Aeson (
    FromJSON (..),
    FromJSONKey (..),
    FromJSONKeyFunction (..),
    ToJSON (..),
    ToJSONKey (..),
    ToJSONKeyFunction (..),
    genericParseJSON,
    genericToJSON,
    object,
    withObject,
    withText,
    (.:),
    (.=),
 )
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Occam.Facts (ParserEditCategory)
import Occam.Json (normalizeJsonFieldNames, snakeCaseOptions)

-- | Reducer configuration metadata recorded into serialized metrics snapshots.
data MetricPolicy = MetricPolicy
    { metricSearchPolicy :: !Text
    , metricColumnMode :: !Text
    , metricJobs :: !Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON MetricPolicy where
    toJSON = genericToJSON snakeCaseOptions

instance FromJSON MetricPolicy where
    parseJSON = genericParseJSON snakeCaseOptions . normalizeJsonFieldNames

-- | Candidate generator family label used to attribute speculation and cancellation cost.
data FamilyTag
    = TagUnknown
    | TagStatement
    | TagStatementSubset
    | TagStatementComplement
    | TagSyntax !ParserEditCategory !Text
    | TagBundle !Text
    deriving (Eq, Ord, Show, Generic)

instance ToJSON FamilyTag where
    toJSON TagUnknown = object ["kind" .= ("unknown" :: Text)]
    toJSON TagStatement = object ["kind" .= ("statement" :: Text)]
    toJSON TagStatementSubset = object ["kind" .= ("statement-subset" :: Text)]
    toJSON TagStatementComplement = object ["kind" .= ("statement-complement" :: Text)]
    toJSON (TagSyntax category label) =
        object
            [ "kind" .= ("syntax" :: Text)
            , "category" .= category
            , "label" .= label
            ]
    toJSON (TagBundle label) =
        object
            [ "kind" .= ("bundle" :: Text)
            , "label" .= label
            ]

instance FromJSON FamilyTag where
    parseJSON = withObject "FamilyTag" \objectValue -> do
        kind <- objectValue .: "kind"
        case (kind :: Text) of
            "unknown" -> pure TagUnknown
            "statement" -> pure TagStatement
            "statement-subset" -> pure TagStatementSubset
            "statement-complement" -> pure TagStatementComplement
            "syntax" -> TagSyntax <$> objectValue .: "category" <*> objectValue .: "label"
            "bundle" -> TagBundle <$> objectValue .: "label"
            _ -> fail ("unknown metrics family kind: " <> T.unpack kind)

-- | Category of a parser invocation recorded for parse latency telemetry.
data ParseKind = ParseStatementOnly | ParseSyntaxChunks | ParseColumnIR
    deriving (Eq, Ord, Show, Generic)

kindKey :: ParseKind -> Text
kindKey ParseStatementOnly = "statement-only"
kindKey ParseSyntaxChunks = "syntax-chunks"
kindKey ParseColumnIR = "column-ir"

parseKindKey :: Text -> Maybe ParseKind
parseKindKey "statement-only" = Just ParseStatementOnly
parseKindKey "syntax-chunks" = Just ParseSyntaxChunks
parseKindKey "column-ir" = Just ParseColumnIR
parseKindKey _ = Nothing

instance ToJSON ParseKind where
    toJSON = toJSON . kindKey

instance FromJSON ParseKind where
    parseJSON = withText "ParseKind" \name ->
        maybe (fail ("unknown metrics parse kind: " <> T.unpack name)) pure (parseKindKey name)

instance ToJSONKey ParseKind where
    toJSONKey :: ToJSONKeyFunction ParseKind
    toJSONKey =
        ToJSONKeyText
            (Key.fromText . kindKey)
            (Encoding.text . kindKey)

instance FromJSONKey ParseKind where
    fromJSONKey :: FromJSONKeyFunction ParseKind
    fromJSONKey = FromJSONKeyTextParser \name ->
        maybe (fail ("unknown metrics parse kind: " <> T.unpack name)) pure (parseKindKey name)

-- | Outcome classification recorded when a candidate retires in window order.
data RetiredVerdict = RetiredAccept | RetiredReject | RetiredInconclusive
    deriving (Eq, Show)

-- | Accumulated execution counts and microsecond timings for a single 'FamilyTag'.
data FamilyCounters = FamilyCounters
    { physicalStarts :: !Int
    , retiredAccept :: !Int
    , retiredReject :: !Int
    , retiredInconclusive :: !Int
    , physicalCompletions :: !Int
    , completedUnselected :: !Int
    , completedUnselectedInteractionWallMicros :: !Int
    , canceled :: !Int
    , canceledInteractionWallMicros :: !Int
    , failed :: !Int
    , failedInteractionWallMicros :: !Int
    , candidateBuildMicros :: !Int
    , acceptedBytesSaved :: !(Maybe Int)
    , oracleMicros :: !(Maybe Int)
    , resetMicros :: !(Maybe Int)
    }
    deriving (Eq, Show, Generic)

instance ToJSON FamilyCounters where
    toJSON = genericToJSON snakeCaseOptions

instance FromJSON FamilyCounters where
    parseJSON = genericParseJSON snakeCaseOptions . normalizeJsonFieldNames

-- | Family entry in JSON wire format.
data FamilyEntry = FamilyEntry
    { familyEntryTag :: !FamilyTag
    , familyEntryCounters :: !FamilyCounters
    }
    deriving (Eq, Show, Generic)

instance ToJSON FamilyEntry where
    toJSON FamilyEntry {familyEntryTag, familyEntryCounters} =
        object
            [ "tag" .= familyEntryTag
            , "counters" .= familyEntryCounters
            ]

instance FromJSON FamilyEntry where
    parseJSON = withObject "FamilyEntry" \objectValue ->
        FamilyEntry
            <$> objectValue .: "tag"
            <*> objectValue .: "counters"

-- | Immutable snapshot of all per-family counters and parse timings emitted into run telemetry.
data MetricsSnapshot = MetricsSnapshot
    { snapshotPolicy :: !MetricPolicy
    , snapshotFamilies :: !(Map FamilyTag FamilyCounters)
    , snapshotGeneratedSkipped :: !(Map Text Int)
    , snapshotParseFfiMicros :: !(Map ParseKind Int)
    , snapshotParseDecodeMicros :: !(Map ParseKind Int)
    }
    deriving (Eq, Show, Generic)

instance ToJSON MetricsSnapshot where
    toJSON MetricsSnapshot {..} =
        object
            [ "snapshot_policy" .= snapshotPolicy
            , "snapshot_families"
                .= [ FamilyEntry tag counters
                   | (tag, counters) <- Map.toList snapshotFamilies
                   ]
            , "snapshot_generated_skipped" .= snapshotGeneratedSkipped
            , "snapshot_parse_ffi_micros" .= snapshotParseFfiMicros
            , "snapshot_parse_decode_micros" .= snapshotParseDecodeMicros
            ]

instance FromJSON MetricsSnapshot where
    parseJSON = withObject "MetricsSnapshot" parseSnapshot . normalizeJsonFieldNames
        where
            parseSnapshot objectValue =
                MetricsSnapshot
                    <$> objectValue .: "snapshot_policy"
                    <*> (Map.fromList . map entryPair <$> objectValue .: "snapshot_families")
                    <*> objectValue .: "snapshot_generated_skipped"
                    <*> objectValue .: "snapshot_parse_ffi_micros"
                    <*> objectValue .: "snapshot_parse_decode_micros"
            entryPair entry = (familyEntryTag entry, familyEntryCounters entry)

-- | Mutable per-run metrics and telemetry store.
data MetricsStore = MetricsStore
    { storePolicy :: !MetricPolicy
    , storeFamilies :: !(IORef (Map FamilyTag FamilyCounters))
    , storeSkipped :: !(IORef (Map Text Int))
    , storeParseFfi :: !(IORef (Map ParseKind Int))
    , storeParseDecode :: !(IORef (Map ParseKind Int))
    }

-- | Allocate an empty 'MetricsStore' parameterized by the reduction's 'MetricPolicy'.
newMetricsStore :: MetricPolicy -> IO MetricsStore
newMetricsStore policy =
    MetricsStore policy
        <$> newIORef Map.empty
        <*> newIORef Map.empty
        <*> newIORef Map.empty
        <*> newIORef Map.empty

-- | Capture an immutable snapshot of all metrics in the store.
snapshotMetrics :: MetricsStore -> IO MetricsSnapshot
snapshotMetrics MetricsStore {..} =
    MetricsSnapshot storePolicy
        <$> readIORef storeFamilies
        <*> readIORef storeSkipped
        <*> readIORef storeParseFfi
        <*> readIORef storeParseDecode

-- | Zero-initialized 'FamilyCounters' value.
emptyFamilyCounters :: FamilyCounters
emptyFamilyCounters =
    FamilyCounters
        { physicalStarts = 0
        , retiredAccept = 0
        , retiredReject = 0
        , retiredInconclusive = 0
        , physicalCompletions = 0
        , completedUnselected = 0
        , completedUnselectedInteractionWallMicros = 0
        , canceled = 0
        , canceledInteractionWallMicros = 0
        , failed = 0
        , failedInteractionWallMicros = 0
        , candidateBuildMicros = 0
        , acceptedBytesSaved = Nothing
        , oracleMicros = Nothing
        , resetMicros = Nothing
        }

bumpFamily :: MetricsStore -> FamilyTag -> (FamilyCounters -> FamilyCounters) -> IO ()
bumpFamily store tag adjust = atomicModifyIORef' (storeFamilies store) \counts ->
    let updated = adjust (Map.findWithDefault emptyFamilyCounters tag counts)
     in updated `seq` (Map.insert tag updated counts, ())

bumpKey :: (Ord k) => IORef (Map k Int) -> k -> Int -> IO ()
bumpKey ref key amount = atomicModifyIORef' ref \counts ->
    let total = Map.findWithDefault 0 key counts + amount
     in total `seq` (Map.insert key total counts, ())

withMetricsStore :: Maybe MetricsStore -> (MetricsStore -> IO ()) -> IO ()
withMetricsStore Nothing _ = pure ()
withMetricsStore (Just store) action = action store

-- | Increment the physical execution attempt counter for a candidate family.
recordPhysicalStart :: Maybe MetricsStore -> FamilyTag -> IO ()
recordPhysicalStart metricsStore tag =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters -> counters {physicalStarts = physicalStarts counters + 1}

-- | Record the final retired verdict (accept, reject, or inconclusive) for a candidate family.
recordRetired :: Maybe MetricsStore -> FamilyTag -> RetiredVerdict -> IO ()
recordRetired metricsStore tag verdict =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters -> case verdict of
            RetiredAccept -> counters {retiredAccept = retiredAccept counters + 1}
            RetiredReject -> counters {retiredReject = retiredReject counters + 1}
            RetiredInconclusive ->
                counters {retiredInconclusive = retiredInconclusive counters + 1}

-- | Increment the completed physical execution counter for a candidate family.
recordPhysicalCompletion :: Maybe MetricsStore -> FamilyTag -> IO ()
recordPhysicalCompletion metricsStore tag =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters -> counters {physicalCompletions = physicalCompletions counters + 1}

-- | Record a speculative candidate that finished execution after an earlier winner already retired.
recordCompletedUnselected :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordCompletedUnselected metricsStore tag wallMicros =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters ->
            counters
                { completedUnselected = completedUnselected counters + 1
                , completedUnselectedInteractionWallMicros = loserWall counters + wallMicros
                }
    where
        loserWall = completedUnselectedInteractionWallMicros

-- | Record a speculative candidate cancelled in flight and accumulate its wall time.
recordCanceled :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordCanceled metricsStore tag wallMicros =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters ->
            counters
                { canceled = canceled counters + 1
                , canceledInteractionWallMicros = canceledInteractionWallMicros counters + wallMicros
                }

-- | Record a candidate whose execution raised an unexpected error and accumulate its wall time.
recordFailed :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordFailed metricsStore tag wallMicros =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters ->
            counters
                { failed = failed counters + 1
                , failedInteractionWallMicros = failedInteractionWallMicros counters + wallMicros
                }

-- | Increment the count of candidate generation skips keyed by skip reason.
recordGeneratedSkipped :: Maybe MetricsStore -> Text -> IO ()
recordGeneratedSkipped metricsStore reason =
    withMetricsStore metricsStore \store -> bumpKey (storeSkipped store) reason 1

-- | Accumulate frontend parse and decode durations for the given 'ParseKind'.
recordParseTiming :: Maybe MetricsStore -> ParseKind -> Int -> Int -> IO ()
recordParseTiming metricsStore kind ffiMicros decodeMicros =
    withMetricsStore metricsStore \store -> do
        bumpKey (storeParseFfi store) kind ffiMicros
        bumpKey (storeParseDecode store) kind decodeMicros

-- | Accumulate candidate-building duration in microseconds for a candidate family.
recordCandidateBuildTiming :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordCandidateBuildTiming metricsStore tag micros =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters -> counters {candidateBuildMicros = candidateBuildMicros counters + micros}

-- | Accumulate total UTF-8 bytes eliminated by accepted candidates in a family.
recordAcceptedBytesSaved :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordAcceptedBytesSaved metricsStore tag bytes =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters ->
            counters {acceptedBytesSaved = Just (fromMaybe 0 (acceptedBytesSaved counters) + bytes)}

-- | Record measured oracle and reset execution durations.
recordOracleTiming :: Maybe MetricsStore -> FamilyTag -> Maybe Int -> Maybe Int -> IO ()
recordOracleTiming (Just _) _ Nothing Nothing = pure ()
recordOracleTiming metricsStore tag mOracle mReset =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters ->
            counters
                { oracleMicros = case mOracle of
                    Nothing -> oracleMicros counters
                    Just n -> Just (maybe n (+ n) (oracleMicros counters))
                , resetMicros = case mReset of
                    Nothing -> resetMicros counters
                    Just n -> Just (maybe n (+ n) (resetMicros counters))
                }

-- | Atomically record both physical completion and oracle timings in a single update.
recordPhysicalCompletionAndTiming ::
    Maybe MetricsStore -> FamilyTag -> Maybe Int -> Maybe Int -> IO ()
recordPhysicalCompletionAndTiming metricsStore tag mOracle mReset =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters ->
            counters
                { physicalCompletions = physicalCompletions counters + 1
                , oracleMicros = case mOracle of
                    Nothing -> oracleMicros counters
                    Just n -> Just (maybe n (+ n) (oracleMicros counters))
                , resetMicros = case mReset of
                    Nothing -> resetMicros counters
                    Just n -> Just (maybe n (+ n) (resetMicros counters))
                }

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
    recordOracleTiming,
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
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Occam.Facts (ParserEditCategory)
import Occam.Json (normalizeJsonFieldNames, snakeCaseOptions)

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

data FamilyTag
    = TagUnknown
    | TagStatement
    | TagSyntax !ParserEditCategory !Text
    | TagBundle !Text
    deriving (Eq, Ord, Show, Generic)

instance ToJSON FamilyTag where
    toJSON TagUnknown = object ["kind" .= ("unknown" :: Text)]
    toJSON TagStatement = object ["kind" .= ("statement" :: Text)]
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
            "syntax" -> TagSyntax <$> objectValue .: "category" <*> objectValue .: "label"
            "bundle" -> TagBundle <$> objectValue .: "label"
            _ -> fail ("unknown metrics family kind: " <> T.unpack kind)

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

data RetiredVerdict = RetiredAccept | RetiredReject | RetiredInconclusive
    deriving (Eq, Show)

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

recordPhysicalStart :: Maybe MetricsStore -> FamilyTag -> IO ()
recordPhysicalStart metricsStore tag =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters -> counters {physicalStarts = physicalStarts counters + 1}

recordRetired :: Maybe MetricsStore -> FamilyTag -> RetiredVerdict -> IO ()
recordRetired metricsStore tag verdict =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters -> case verdict of
            RetiredAccept -> counters {retiredAccept = retiredAccept counters + 1}
            RetiredReject -> counters {retiredReject = retiredReject counters + 1}
            RetiredInconclusive ->
                counters {retiredInconclusive = retiredInconclusive counters + 1}

recordPhysicalCompletion :: Maybe MetricsStore -> FamilyTag -> IO ()
recordPhysicalCompletion metricsStore tag =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters -> counters {physicalCompletions = physicalCompletions counters + 1}

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

recordCanceled :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordCanceled metricsStore tag wallMicros =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters ->
            counters
                { canceled = canceled counters + 1
                , canceledInteractionWallMicros = canceledInteractionWallMicros counters + wallMicros
                }

recordFailed :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordFailed metricsStore tag wallMicros =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters ->
            counters
                { failed = failed counters + 1
                , failedInteractionWallMicros = failedInteractionWallMicros counters + wallMicros
                }

recordGeneratedSkipped :: Maybe MetricsStore -> Text -> IO ()
recordGeneratedSkipped metricsStore reason =
    withMetricsStore metricsStore \store -> bumpKey (storeSkipped store) reason 1

recordParseTiming :: Maybe MetricsStore -> ParseKind -> Int -> Int -> IO ()
recordParseTiming metricsStore kind ffiMicros decodeMicros =
    withMetricsStore metricsStore \store -> do
        bumpKey (storeParseFfi store) kind ffiMicros
        bumpKey (storeParseDecode store) kind decodeMicros

recordCandidateBuildTiming :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordCandidateBuildTiming metricsStore tag micros =
    withMetricsStore metricsStore \store ->
        bumpFamily store tag \counters -> counters {candidateBuildMicros = candidateBuildMicros counters + micros}

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

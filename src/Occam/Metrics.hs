module Occam.Metrics (
    MetricPolicy (..),
    FamilyTag (..),
    ParseKind (..),
    RetiredVerdict (..),
    MetricsStore,
    MetricsSnapshot (..),
    FamilyCounters (..),
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
    withText,
 )
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Occam.Facts (ParserEditCategory, editCategoryName, parseEditCategoryName)

data MetricPolicy = MetricPolicy
    { metricSearchPolicy :: !Text
    , metricColumnMode :: !Text
    , metricJobs :: !Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON MetricPolicy
instance FromJSON MetricPolicy

data FamilyTag
    = TagUnknown
    | TagStatement
    | TagSyntax !ParserEditCategory !Text
    | TagBundle !Text
    deriving (Eq, Ord, Show, Generic)

familyTagKey :: FamilyTag -> Text
familyTagKey TagUnknown = "unknown"
familyTagKey TagStatement = "statement"
familyTagKey (TagSyntax category label) = "syntax:" <> editCategoryName category <> ":" <> label
familyTagKey (TagBundle label) = "bundle:" <> label

parseFamilyTagKey :: Text -> Maybe FamilyTag
parseFamilyTagKey "unknown" = Just TagUnknown
parseFamilyTagKey "statement" = Just TagStatement
parseFamilyTagKey key
    | Just rest <- T.stripPrefix "syntax:" key =
        case T.breakOn ":" rest of
            (category, colonLabel)
                | not (T.null category)
                , Just label <- T.stripPrefix ":" colonLabel
                , not (T.null label)
                , Just parsed <- parseEditCategoryName category ->
                    Just (TagSyntax parsed label)
            _ -> Nothing
    | Just label <- T.stripPrefix "bundle:" key =
        if T.null label then Nothing else Just (TagBundle label)
    | otherwise = Nothing

instance ToJSON FamilyTag where
    toJSON = toJSON . familyTagKey

instance FromJSON FamilyTag where
    parseJSON = withText "FamilyTag" \name ->
        maybe (fail ("unknown metrics family tag: " <> T.unpack name)) pure (parseFamilyTagKey name)

instance ToJSONKey FamilyTag where
    toJSONKey :: ToJSONKeyFunction FamilyTag
    toJSONKey =
        ToJSONKeyText
            (Key.fromText . familyTagKey)
            (Encoding.text . familyTagKey)

instance FromJSONKey FamilyTag where
    fromJSONKey :: FromJSONKeyFunction FamilyTag
    fromJSONKey = FromJSONKeyTextParser \name ->
        maybe (fail ("unknown metrics family tag: " <> T.unpack name)) pure (parseFamilyTagKey name)

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

instance ToJSON FamilyCounters
instance FromJSON FamilyCounters

data MetricsSnapshot = MetricsSnapshot
    { snapshotPolicy :: !MetricPolicy
    , snapshotFamilies :: !(Map FamilyTag FamilyCounters)
    , snapshotGeneratedSkipped :: !(Map Text Int)
    , snapshotParseFfiMicros :: !(Map ParseKind Int)
    , snapshotParseDecodeMicros :: !(Map ParseKind Int)
    }
    deriving (Eq, Show, Generic)

instance ToJSON MetricsSnapshot
instance FromJSON MetricsSnapshot

{- | Mutable per-run cost store. One owner (Evaluation) creates it, threads it
into the pool config, and snapshots it after the pool drains; record sites
only add counts. Holds counters plus bounded tag/policy text; no event log, no SQL.
-}
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

{- | The single end hook. Call after the pool drains, on every end status;
concurrent records during the snapshot itself are not atomic across maps.
-}
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

recordPhysicalStart :: Maybe MetricsStore -> FamilyTag -> IO ()
recordPhysicalStart Nothing _ = pure ()
recordPhysicalStart (Just store) tag =
    bumpFamily store tag \counters -> counters {physicalStarts = physicalStarts counters + 1}

recordRetired :: Maybe MetricsStore -> FamilyTag -> RetiredVerdict -> IO ()
recordRetired Nothing _ _ = pure ()
recordRetired (Just store) tag RetiredAccept =
    bumpFamily store tag \counters -> counters {retiredAccept = retiredAccept counters + 1}
recordRetired (Just store) tag RetiredReject =
    bumpFamily store tag \counters -> counters {retiredReject = retiredReject counters + 1}
recordRetired (Just store) tag RetiredInconclusive =
    bumpFamily store tag \counters ->
        counters {retiredInconclusive = retiredInconclusive counters + 1}

recordPhysicalCompletion :: Maybe MetricsStore -> FamilyTag -> IO ()
recordPhysicalCompletion Nothing _ = pure ()
recordPhysicalCompletion (Just store) tag =
    bumpFamily store tag \counters -> counters {physicalCompletions = physicalCompletions counters + 1}

recordCompletedUnselected :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordCompletedUnselected Nothing _ _ = pure ()
recordCompletedUnselected (Just store) tag wallMicros =
    bumpFamily store tag \counters ->
        counters
            { completedUnselected = completedUnselected counters + 1
            , completedUnselectedInteractionWallMicros = loserWall counters + wallMicros
            }
    where
        loserWall = completedUnselectedInteractionWallMicros

recordCanceled :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordCanceled Nothing _ _ = pure ()
recordCanceled (Just store) tag wallMicros =
    bumpFamily store tag \counters ->
        counters
            { canceled = canceled counters + 1
            , canceledInteractionWallMicros = canceledInteractionWallMicros counters + wallMicros
            }

recordFailed :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordFailed Nothing _ _ = pure ()
recordFailed (Just store) tag wallMicros =
    bumpFamily store tag \counters ->
        counters
            { failed = failed counters + 1
            , failedInteractionWallMicros = failedInteractionWallMicros counters + wallMicros
            }

recordGeneratedSkipped :: Maybe MetricsStore -> Text -> IO ()
recordGeneratedSkipped Nothing _ = pure ()
recordGeneratedSkipped (Just store) reason = bumpKey (storeSkipped store) reason 1

recordParseTiming :: Maybe MetricsStore -> ParseKind -> Int -> Int -> IO ()
recordParseTiming Nothing _ _ _ = pure ()
recordParseTiming (Just store) kind ffiMicros decodeMicros = do
    bumpKey (storeParseFfi store) kind ffiMicros
    bumpKey (storeParseDecode store) kind decodeMicros

recordCandidateBuildTiming :: Maybe MetricsStore -> FamilyTag -> Int -> IO ()
recordCandidateBuildTiming Nothing _ _ = pure ()
recordCandidateBuildTiming (Just store) tag micros =
    bumpFamily store tag \counters -> counters {candidateBuildMicros = candidateBuildMicros counters + micros}

{- | Add measured oracle/reset micros. Sums cover available completed
profiles ONLY: 'Nothing' stays unknown (never folded to zero), canceled
work bills separate interaction wall. Neither sum is total CPU or all
interaction cost.
-}
recordOracleTiming :: Maybe MetricsStore -> FamilyTag -> Maybe Int -> Maybe Int -> IO ()
recordOracleTiming Nothing _ _ _ = pure ()
recordOracleTiming (Just _) _ Nothing Nothing = pure ()
recordOracleTiming (Just store) tag mOracle mReset =
    bumpFamily store tag \counters ->
        counters
            { oracleMicros = case mOracle of
                Nothing -> oracleMicros counters
                Just n -> Just (maybe n (+ n) (oracleMicros counters))
            , resetMicros = case mReset of
                Nothing -> resetMicros counters
                Just n -> Just (maybe n (+ n) (resetMicros counters))
            }

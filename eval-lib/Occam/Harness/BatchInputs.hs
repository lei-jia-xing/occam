-- | Resolve input files and metadata for a batch without executing the SQL.
module Occam.Harness.BatchInputs (
    Attributes (..),
    BatchCase (..),
    BatchInputOptions (..),
    BatchInputSource (..),
    batchCaseOverview,
    loadBatchInputs,
) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (IOException, try)
import Control.Monad (guard, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Aeson (
    Object,
    ToJSON (..),
    Value (..),
    eitherDecodeStrict',
    encode,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (toList, traverse_)
import Data.List (mapAccumL, sort, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Occam.Harness.Config (DbmsSpec (..), HarnessConfig, lookupDbmsSpec)
import Occam.Harness.Corpus (
    absolutePath,
    fileStem,
    normalizeDbms,
    sqlSha256,
    uniqueCaseId,
    validCaseId,
 )
import Occam.Harness.Signature (CrashSignature)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (
    isAbsolute,
    takeDirectory,
    takeExtension,
    (<.>),
    (</>),
 )

-- | Origin of SQL seed inputs for a batch benchmark run.
data BatchInputSource
    = CorpusDirectory !FilePath
    | ManifestFile !FilePath
    | SqlFiles !(NonEmpty FilePath)
    deriving (Eq, Show)

-- | Input discovery and target-selection flags supplied to 'loadBatchInputs'.
data BatchInputOptions = BatchInputOptions
    { inputSource :: !BatchInputSource
    , inputDbms :: !Text
    , inputCaseIds :: !Text
    , inputExpectedSignature :: !Text
    , inputImage :: !Text
    , inputImageOverrides :: ![Text]
    }
    deriving (Eq, Show)

data Entry = Entry !Text !FilePath !Object

-- | Normalized manifest/directory attributes for a corpus test case.
data Attributes = Attributes
    { attrDbms :: !(Maybe Text)
    , attrDialect :: !(Maybe Text)
    , attrVersion :: !Text
    , attrImage :: !Text
    , attrSignature :: !(Maybe Text)
    , attrBaseline :: !(Maybe CrashSignature)
    }

-- | Fully resolved benchmark case ready for planning and crash-oracle evaluation.
data BatchCase = BatchCase
    { batchCaseId :: !Text
    , batchFile :: !FilePath
    , batchByteCount :: !Int
    , batchSqlSha256 :: !Text
    , batchDbms :: !DbmsSpec
    , batchDialect :: !Text
    , batchImage :: !Text
    , batchSignature :: !Text
    , batchAttributes :: !Attributes
    , batchMetadata :: !Object
    }

instance ToJSON BatchCase where
    toJSON item =
        let attrs = batchAttributes item
         in object
                [ "id" .= batchCaseId item
                , "file" .= batchFile item
                , "byte_count" .= batchByteCount item
                , "sql_sha256" .= batchSqlSha256 item
                , "dbms" .= dbmsId (batchDbms item)
                , "dialect" .= batchDialect item
                , "image" .= batchImage item
                , "version" .= attrVersion attrs
                , "meta_image" .= attrImage attrs
                , "expected_signature" .= batchSignature item
                , "baseline_signature" .= attrBaseline attrs
                , "meta" .= batchMetadata item
                ]

-- | Stable subset of the case inventory recorded in public batch plans.
batchCaseOverview :: BatchCase -> Value
batchCaseOverview item =
    object
        [ "id" .= batchCaseId item
        , "dbms" .= dbmsId (batchDbms item)
        , "dialect" .= batchDialect item
        , "image" .= batchImage item
        , "file" .= batchFile item
        , "byte_count" .= batchByteCount item
        , "expected_signature" .= batchSignature item
        ]

type ReadInputs = ExceptT String IO

-- | Read existing files and resolve the batch's input selection in execution order.
loadBatchInputs :: HarnessConfig -> BatchInputOptions -> IO (Either String [BatchCase])
loadBatchInputs cfg options = do
    result <- try (runExceptT load) :: IO (Either IOException (Either String [BatchCase]))
    pure (either (Left . show) id result)
    where
        source = inputSource options
        dbmsFilter = nonEmpty (normalizeDbms (inputDbms options))
        caseFilter = Set.fromList (filter (not . T.null) (map T.strip (T.splitOn "," (inputCaseIds options))))
        selected cid = Set.null caseFilter || Set.member cid caseFilter
        load = do
            traverse_ (requireDbms cfg) dbmsFilter
            overrides <- Map.fromList <$> traverse parseImageOverride (inputImageOverrides options)
            entries <- case source of
                CorpusDirectory dir -> readCorpus dir
                ManifestFile path -> readManifest path
                SqlFiles paths -> do
                    when (isNothing dbmsFilter) (throwE "--dbms is required when specifying raw --inputs SQL files")
                    readSqlFiles paths
            cases <-
                traverse
                    (resolveEntry overrides)
                    [entry | entry@(Entry cid _ _) <- entries, selected cid || explicitFiles source]
            pure (sortOn sortKey (catMaybes cases))

        resolveEntry overrides (Entry cid path metadata) = do
            attrs <- except $ first ((path <> ": ") <>) (parseEither (attributes source) metadata)
            dbms <-
                requireDbms cfg . normalizeDbms
                    =<< maybe
                        (throwE ("Cannot infer DBMS for '" <> path <> "'; pass --dbms or add dialect metadata"))
                        pure
                        (attrDbms attrs <|> dbmsFilter)
            if maybe True (== dbmsId dbms) dbmsFilter
                then do
                    exists <- lift (doesFileExist path)
                    unless exists (throwE ("SQL file not found: " <> path))
                    sql <- lift (BS.readFile path)
                    let image = resolveImage (inputImage options) overrides cid attrs dbms
                        dialect = fromMaybe (dbmsOccamDialect dbms) (attrDialect attrs)
                        signature = fromMaybe (inputExpectedSignature options) (attrSignature attrs)
                    pure
                        ( Just
                            (BatchCase cid path (BS.length sql) (sqlSha256 sql) dbms dialect image signature attrs metadata)
                        )
                else pure Nothing

        sortKey item = (dbmsId (batchDbms item), batchImage item, batchCaseId item)

explicitFiles :: BatchInputSource -> Bool
explicitFiles (SqlFiles _) = True
explicitFiles _ = False

readCorpus :: FilePath -> ReadInputs [Entry]
readCorpus dir = do
    root <- lift (absolutePath dir)
    files <- lift (sort <$> listDirectory root)
    entries <- traverse (readEntry root) (filter ((== ".sql") . takeExtension) files)
    checkUniqueIds root (catMaybes entries)
    where
        readEntry root name = do
            let path = root </> name
                stem = fileStem name
                sidecar = root </> stem <.> "meta.json"
            exists <- lift (doesFileExist path)
            if exists
                then do
                    hasMetadata <- lift (doesFileExist sidecar)
                    metadata <-
                        if hasMetadata then readJson sidecar (withObject "case metadata" pure) else pure KM.empty
                    Just <$> makeEntry sidecar (T.pack stem) path metadata
                else pure Nothing

readManifest :: FilePath -> ReadInputs [Entry]
readManifest path = do
    absolute <- lift (absolutePath path)
    entries <- readJson absolute parseManifest >>= traverse (readEntry absolute)
    checkUniqueIds absolute entries
    where
        parseManifest = \case
            Array items -> traverse parseItem (toList items)
            value ->
                withObject
                    "manifest"
                    (\obj -> if KM.member "cases" obj then obj .: "cases" >>= traverse parseItem else pure [])
                    value
        parseItem = withObject "manifest entry" pure
        readEntry absolute item = do
            raw <- except (first ((absolute <> ": ") <>) (parseEither (.: "file") item))
            when (T.null (T.strip raw)) (throwE ("Manifest entry missing a file path in " <> absolute))
            let filename = T.unpack raw
            sql <-
                if isAbsolute filename
                    then pure filename
                    else lift (absolutePath (takeDirectory absolute </> filename))
            makeEntry absolute (T.pack (fileStem filename)) sql item

readSqlFiles :: NonEmpty FilePath -> ReadInputs [Entry]
readSqlFiles paths = do
    absolute <- lift (traverse absolutePath (toList paths))
    pure (snd (mapAccumL makeRawEntry Set.empty absolute))
    where
        makeRawEntry used path =
            let cid = uniqueCaseId used (T.pack (fileStem path))
             in (Set.insert cid used, Entry cid path KM.empty)

makeEntry :: FilePath -> Text -> FilePath -> Object -> ReadInputs Entry
makeEntry source fallback path metadata = do
    cid <- fromMaybe fallback <$> except (first ((source <> ": ") <>) (parseEither (.:? "id") metadata))
    unless (validCaseId cid) (throwE ("Unsafe or empty case id " <> show cid <> " in " <> source))
    pure (Entry cid path metadata)

checkUniqueIds :: FilePath -> [Entry] -> ReadInputs [Entry]
checkUniqueIds source entries = go Set.empty entries >> pure entries
    where
        go _ [] = pure ()
        go seen (Entry cid _ _ : rest) = do
            when (Set.member cid seen) (throwE ("Duplicate case id " <> show cid <> " in " <> source))
            go (Set.insert cid seen) rest

readJson :: FilePath -> (Value -> Parser a) -> ReadInputs a
readJson path parser = do
    bytes <- lift (BS.readFile path)
    except $ first ((path <> ": ") <>) (eitherDecodeStrict' bytes >>= parseEither parser)

attributes :: BatchInputSource -> Object -> Parser Attributes
attributes (ManifestFile _) meta = do
    dbms <- optionalText meta "dbms"
    dialect <- optionalText meta "dialect"
    baseline <- meta .:? "baseline_signature"
    pure
        ( Attributes
            dbms
            dialect
            (text meta "version")
            (text meta "image")
            (scalar meta "expected_signature")
            baseline
        )
attributes _ meta = do
    target <- maybe (pure KM.empty) (withObject "target" pure) (KM.lookup "target" meta >>= present)
    targetDbms <- optionalText target "dbms"
    dialect <- optionalText meta "dialect"
    let signature = case KM.lookup "crash_signature" meta of
            Just (Object info) -> scalar info "expected_signature"
            _ -> Nothing
    pure
        ( Attributes
            (targetDbms <|> dialect)
            Nothing
            (text target "version")
            (text target "image")
            signature
            Nothing
        )

optionalText :: Object -> Key -> Parser (Maybe Text)
optionalText obj key = (>>= nonEmpty) <$> obj .:? key

text :: Object -> Key -> Text
text obj key = fromMaybe "" (scalar obj key)

scalar :: Object -> Key -> Maybe Text
scalar obj key =
    KM.lookup key obj >>= present >>= \case
        String value -> Just value
        Bool True -> Just "True"
        value -> Just (decodeUtf8 (BSL.toStrict (encode value)))

present :: Value -> Maybe Value
present value = do
    guard $ case value of
        Null -> False
        Bool boolean -> boolean
        String raw -> not (T.null raw)
        Number number -> number /= 0
        Array items -> not (null items)
        Object obj -> not (KM.null obj)
    pure value

nonEmpty :: Text -> Maybe Text
nonEmpty value = value <$ guard (not (T.null value))

requireDbms :: HarnessConfig -> Text -> ReadInputs DbmsSpec
requireDbms cfg name = maybe (throwE ("Unsupported DBMS: " <> T.unpack name)) pure (lookupDbmsSpec cfg name)

parseImageOverride :: Text -> ReadInputs (Text, Text)
parseImageOverride raw =
    let (key, suffix) = T.breakOn "=" raw
     in case (nonEmpty (T.strip key), nonEmpty (T.strip (T.drop 1 suffix))) of
            (Just name, Just image) -> pure (name, image)
            _ -> throwE ("Invalid --image-map " <> show raw <> "; expected KEY=IMAGE")

resolveImage :: Text -> Map Text Text -> Text -> Attributes -> DbmsSpec -> Text
resolveImage explicit overrides cid attrs dbms =
    let name = dbmsId dbms
        versionKey = (\version -> name <> "@" <> version) <$> nonEmpty (attrVersion attrs)
        keys = [Just ("case:" <> cid), versionKey, Just name]
        candidates =
            nonEmpty explicit : [key >>= (`Map.lookup` overrides) | key <- keys] <> [nonEmpty (attrImage attrs)]
     in fromMaybe (dbmsContainerImage dbms) (listToMaybe (catMaybes candidates))

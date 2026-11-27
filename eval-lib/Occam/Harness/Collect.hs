-- | Import existing SQL artifacts into an Occam manifest with optional N-replay verification.
module Occam.Harness.Collect (
    ArtifactSource (..),
    CollectOptions (..),
    collectArtifacts,
    defaultCollectOptions,
) where

import Control.Exception.Safe (IOException, try)
import Control.Monad (foldM, forM, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (Object, ToJSON (..), Value (..), decodeStrict', encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List (isPrefixOf, isSuffixOf, partition, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.AtomicFile (writeAtomicBytes, writeAtomicLazyBytes)
import Occam.Harness.Config (DbmsSpec (..), HarnessConfig, lookupDbmsSpec)
import Occam.Harness.Corpus (absolutePath, fileStem, normalizeDbms, sqlSha256, uniqueCaseId)
import Occam.Harness.DbmsExec (ConnectionOverrides, emptyConnectionOverrides)
import Occam.Harness.Replay (
    ReplayConfig (..),
    ReplayEndpoint (..),
    ReplayVerification (..),
    defaultReplayConfig,
    runReplayVerification,
 )
import Occam.Harness.Signature (CrashSignature (..), DbmsKind, parseDbmsKind)
import Occam.Harness.SqlancerSanitize (
    SqlancerSanitizeResult (..),
    metaToObject,
    sanitizeSqlancerLog,
 )
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    pathIsSymbolicLink,
 )
import System.FilePath (makeRelative, takeDirectory, takeFileName, (<.>), (</>))

data ArtifactSource = Raw | Sqlsmith | Sqlancer | Squirrel
    deriving (Eq, Show)

sourceName :: ArtifactSource -> Text
sourceName Raw = "raw"
sourceName Sqlsmith = "sqlsmith"
sourceName Sqlancer = "sqlancer"
sourceName Squirrel = "squirrel"

data CollectOptions = CollectOptions
    { collectSource :: !ArtifactSource
    , collectDbms :: !Text
    , collectInputDir :: !FilePath
    , collectOutputDir :: !FilePath
    , collectExcludedDir :: !(Maybe FilePath)
    , collectIncludeCurrentLogs :: !Bool
    , collectFallbackCurrentLogs :: !Bool
    , collectExpectedSignature :: !Text
    , collectVerify :: !Bool
    , collectReplays :: !Int
    , collectDedupBySignature :: !Bool
    , collectConnection :: !ConnectionOverrides
    , collectLogFile :: !(Maybe FilePath)
    , collectContainerLog :: !(Maybe FilePath)
    , collectAllowStderrOnly :: !Bool
    , collectAllowPsqlMeta :: !Bool
    , collectProbeTimeout :: !Double
    , collectResetBeforeVerify :: !Bool
    , collectStackFrames :: !Int
    }
    deriving (Eq, Show)

defaultCollectOptions :: ArtifactSource -> Text -> FilePath -> FilePath -> CollectOptions
defaultCollectOptions source dbms inputDir outputDir =
    CollectOptions
        { collectSource = source
        , collectDbms = dbms
        , collectInputDir = inputDir
        , collectOutputDir = outputDir
        , collectExcludedDir = Nothing
        , collectIncludeCurrentLogs = False
        , collectFallbackCurrentLogs = True
        , collectExpectedSignature = ""
        , collectVerify = False
        , collectReplays = 5
        , collectDedupBySignature = True
        , collectConnection = emptyConnectionOverrides
        , collectLogFile = Nothing
        , collectContainerLog = Nothing
        , collectAllowStderrOnly = False
        , collectAllowPsqlMeta = False
        , collectProbeTimeout = 15.0
        , collectResetBeforeVerify = True
        , collectStackFrames = 5
        }

data Candidate = Candidate !Text !BS.ByteString !Object

data CollectedCase = CollectedCase
    { collectedId :: !Text
    , collectedDbms :: !DbmsSpec
    , collectedType :: !ArtifactSource
    , collectedHash :: !Text
    , collectedPath :: !FilePath
    , collectedRelative :: !FilePath
    , collectedOrigin :: !FilePath
    , collectedBytes :: !Int
    , collectedSignature :: !Text
    , collectedMetadata :: !Object
    , collectedDuplicateCount :: !Int
    , collectedBaselineSignature :: !(Maybe CrashSignature)
    , collectedQualified :: !(Maybe Bool)
    , collectedReplay :: !(Maybe ReplayVerification)
    , collectedSigDuplicates :: ![Text]
    }

instance ToJSON CollectedCase where
    toJSON item =
        object
            [ "id" .= collectedId item
            , "dbms" .= dbmsId (collectedDbms item)
            , "dialect" .= dbmsOccamDialect (collectedDbms item)
            , "file" .= collectedPath item
            , "relative_file" .= collectedRelative item
            , "source_type" .= sourceName (collectedType item)
            , "source_path" .= collectedOrigin item
            , "sql_sha256" .= collectedHash item
            , "byte_count" .= collectedBytes item
            , "expected_signature" .= collectedSignature item
            , "baseline_signature" .= collectedBaselineSignature item
            , "qualified" .= collectedQualified item
            , "replay_verification" .= collectedReplay item
            , "provenance" .= collectedMetadata item
            , "byte_duplicate_count" .= collectedDuplicateCount item
            , "signature_duplicates" .= collectedSigDuplicates item
            ]

data Collection = Collection
    { byHash :: !(Map.Map Text CollectedCase)
    , hashesInReverseOrder :: ![Text]
    , usedIds :: !(Set.Set Text)
    , candidatesSeen :: !Int
    }

type Import = ExceptT String IO

-- | Write content-addressed cases, optionally run N-replay verification, then publish manifest.json atomically.
collectArtifacts :: HarnessConfig -> CollectOptions -> IO (Either String Value)
collectArtifacts cfg options = do
    result <- try (runExceptT collect) :: IO (Either IOException (Either String Value))
    pure (either (Left . show) id result)
    where
        source = collectSource options
        collect = do
            when (collectReplays options <= 0) (throwE "--replays must be a positive integer")
            when (collectStackFrames options <= 0) (throwE "--stack-frames must be a positive integer")
            let pt = collectProbeTimeout options
            when
                (isNaN pt || isInfinite pt || pt <= 0)
                (throwE "--probe-timeout must be a finite positive number")
            dbms <-
                maybe (throwE ("Unsupported DBMS: " <> T.unpack (collectDbms options))) pure $
                    lookupDbmsSpec cfg (normalizeDbms (collectDbms options))
            dbmsKind <-
                maybe (throwE ("Unsupported DBMS kind: " <> T.unpack (dbmsId dbms))) pure $
                    parseDbmsKind (dbmsId dbms)
            input <- lift (absolutePath (collectInputDir options))
            output <- lift (absolutePath (collectOutputDir options))
            exists <- lift (doesDirectoryExist input)
            unless exists (throwE ("Input directory does not exist: " <> input))
            canonicalInput <- lift (canonicalizePath input)
            canonicalOutput <- lift (canonicalizePath output)
            excluded <- lift (traverse canonicalizePath (collectExcludedDir options))
            let outputDirs = canonicalOutput : maybeToList excluded
            when (canonicalInput `elem` outputDirs) (throwE "Input and output directories must be different")
            files <- lift (walkFiles outputDirs input)
            let chosen = selectArtifacts options dbms files
            imported <- foldM (importFile input output dbms) (Collection Map.empty [] Set.empty 0) chosen
            let byteUnique = reverse (mapMaybe (`Map.lookup` byHash imported) (hashesInReverseOrder imported))
                rawSeen = candidatesSeen imported
            (finalCases, rejectedVals, summary) <-
                if collectVerify options
                    then lift (verifyAndDedup cfg dbmsKind dbms source output rawSeen byteUnique options)
                    else
                        pure
                            ( byteUnique
                            , []
                            , collectionSummary rawSeen (length byteUnique) 0 False 0 0 0 dbms source
                            )
            let manifest =
                    object
                        [ "schema_version" .= (1 :: Int)
                        , "dbms" .= dbmsId dbms
                        , "source" .= sourceName source
                        , "cases" .= finalCases
                        , "rejected_cases" .= rejectedVals
                        , "summary" .= Object summary
                        ]
                path = output </> "manifest.json"
            lift $ do
                createDirectoryIfMissing True (output </> "cases")
                writeAtomicLazyBytes path (encode manifest <> BSL.pack [10])
            pure (Object (KM.insert "manifest_path" (toJSON path) summary))

        importFile input output dbms collection path = do
            candidate <- lift (readCandidate input source path)
            case candidate of
                Nothing -> pure collection
                Just (Candidate name payload meta) -> do
                    let digest = sqlSha256 payload
                        observed = collection {candidatesSeen = candidatesSeen collection + 1}
                    case Map.lookup digest (byHash observed) of
                        Just previous ->
                            pure
                                observed
                                    { byHash =
                                        Map.insert
                                            digest
                                            (previous {collectedDuplicateCount = collectedDuplicateCount previous + 1})
                                            (byHash observed)
                                    }
                        Nothing -> do
                            let cid = uniqueCaseId (usedIds observed) name
                                relative = "cases" </> T.unpack digest <.> "sql"
                                artifactPath = output </> relative
                                signature
                                    | T.null (collectExpectedSignature options) = signatureFrom meta
                                    | otherwise = collectExpectedSignature options
                                item =
                                    CollectedCase
                                        cid
                                        dbms
                                        source
                                        digest
                                        artifactPath
                                        relative
                                        path
                                        (BS.length payload)
                                        signature
                                        meta
                                        0
                                        Nothing
                                        Nothing
                                        Nothing
                                        []
                            store artifactPath payload
                            pure
                                observed
                                    { byHash = Map.insert digest item (byHash observed)
                                    , hashesInReverseOrder = digest : hashesInReverseOrder observed
                                    , usedIds = Set.insert cid (usedIds observed)
                                    }

verifyAndDedup ::
    HarnessConfig ->
    DbmsKind ->
    DbmsSpec ->
    ArtifactSource ->
    FilePath ->
    Int ->
    [CollectedCase] ->
    CollectOptions ->
    IO ([CollectedCase], [Value], Object)
verifyAndDedup cfg dbmsKind dbms source output rawSeen byteUnique options = do
    let verifyDir = output </> "verify_logs"
    createDirectoryIfMissing True verifyDir
    (revOrder, byKeptId, rejectedRev, collapsedCount, seenSigs) <-
        foldM (stepOne verifyDir) ([], Map.empty, [], 0, Map.empty) byteUnique
    let kept = reverse (mapMaybe (`Map.lookup` byKeptId) revOrder)
        uniqCount = Map.size seenSigs
        rejectedCount = length rejectedRev
        summary0 =
            collectionSummary
                rawSeen
                (length byteUnique)
                (collectReplays options)
                True
                rejectedCount
                collapsedCount
                uniqCount
                dbms
                source
        summary = KM.insert "manifest_case_count" (toJSON (length kept)) summary0
    pure (kept, reverse rejectedRev, summary)
    where
        stepOne verifyDir (revOrder, byKeptId, rejectedRev, collapsed, seenSigs) item = do
            sqlBytes <- BS.readFile (collectedPath item)
            let decPath = verifyDir </> T.unpack (collectedId item) <> "_decisions.jsonl"
                rCfg =
                    (defaultReplayConfig dbmsKind (collectConnection options))
                        { rcEndpoint =
                            pure
                                ( Right
                                    ( ReplayEndpoint
                                        (collectConnection options)
                                        (collectLogFile options)
                                        (collectContainerLog options)
                                    )
                                )
                        , rcExpectedSignature = collectedSignature item
                        , rcStackFrames = collectStackFrames options
                        , rcDecisionLog = Just decPath
                        , rcAllowStderrOnly = collectAllowStderrOnly options
                        , rcAllowPsqlMeta = collectAllowPsqlMeta options
                        , rcProbeTimeout = collectProbeTimeout options
                        , rcResetBeforeRun = collectResetBeforeVerify options
                        , rcRequireConsistent = True
                        }
            rv <- runReplayVerification cfg Nothing rCfg (collectReplays options) sqlBytes
            let stable = rvAllPassed rv && rvConsistentSignature rv
            case (stable, rvBaselineSignature rv) of
                (True, Just baseSig) ->
                    let sigKey = csId baseSig
                     in case Map.lookup sigKey seenSigs of
                            Just keeperCid
                                | collectDedupBySignature options ->
                                    let updater prev =
                                            prev {collectedSigDuplicates = collectedSigDuplicates prev <> [collectedId item]}
                                     in pure (revOrder, Map.adjust updater keeperCid byKeptId, rejectedRev, collapsed + 1, seenSigs)
                            _ ->
                                let accepted =
                                        item
                                            { collectedBaselineSignature = Just baseSig
                                            , collectedQualified = Just True
                                            , collectedReplay = Just rv
                                            }
                                    cid = collectedId item
                                 in pure
                                        ( cid : revOrder
                                        , Map.insert cid accepted byKeptId
                                        , rejectedRev
                                        , collapsed
                                        , Map.insertWith (\_ old -> old) sigKey cid seenSigs
                                        )
                _ ->
                    let rejObj =
                            object
                                [ "id" .= collectedId item
                                , "dbms" .= dbmsId dbms
                                , "file" .= collectedPath item
                                , "replay_verification" .= rv
                                ]
                     in pure (revOrder, byKeptId, rejObj : rejectedRev, collapsed, seenSigs)

-- A path that is already present in a published manifest must keep its bytes.
store :: FilePath -> BS.ByteString -> Import ()
store path payload = do
    exists <- lift (doesFileExist path)
    if exists
        then do
            existing <- lift (BS.readFile path)
            unless (existing == payload) (throwE ("Content-addressed SQL file has unexpected bytes: " <> path))
        else lift (writeAtomicBytes path payload)

collectionSummary ::
    Int ->
    Int ->
    Int ->
    Bool ->
    Int ->
    Int ->
    Int ->
    DbmsSpec ->
    ArtifactSource ->
    Object
collectionSummary raw uniqBytes replaysReq isVerified rejCount collapsed uniqSigs dbms source =
    KM.fromList
        [ "source" .= sourceName source
        , "dbms" .= dbmsId dbms
        , "raw_candidates_discovered" .= raw
        , "unique_byte_candidates" .= uniqBytes
        , "verified" .= isVerified
        , "replays_required" .= replaysReq
        , "rejected_unstable_or_noncrashing" .= rejCount
        , "signature_duplicates_collapsed" .= collapsed
        , "unique_signatures" .= uniqSigs
        , "manifest_case_count" .= uniqBytes
        ]

walkFiles :: [FilePath] -> FilePath -> IO [FilePath]
walkFiles excluded = walk
    where
        walk dir = do
            resolved <- canonicalizePath dir
            if resolved `elem` excluded
                then pure []
                else do
                    entries <- sort <$> listDirectory dir
                    classified <- forM entries \name -> do
                        let path = dir </> name
                        directory <- doesDirectoryExist path
                        if directory
                            then do
                                link <- pathIsSymbolicLink path
                                pure ([], [path | not link])
                            else do
                                file <- doesFileExist path
                                pure ([path | file], [])
                    let (files, subdirs) = unzip classified
                    nested <- traverse walk (concat subdirs)
                    pure (concat files <> concat nested)

selectArtifacts :: CollectOptions -> DbmsSpec -> [FilePath] -> [FilePath]
selectArtifacts options dbms files = case collectSource options of
    Squirrel -> filter squirrelFile files
    Sqlancer ->
        let (current, primary) = partition ("-cur.log" `isSuffixOf`) (filter sqlancerFile files)
            keepCurrent = collectIncludeCurrentLogs options || (collectFallbackCurrentLogs options && null primary)
         in primary <> if keepCurrent then current else []
    _ -> filter (".sql" `isSuffixOf`) files
    where
        squirrelFile path =
            takeFileName (takeDirectory path) == "crashes"
                && let name = takeFileName path in name /= "README.txt" && not ("." `isPrefixOf` name)
        sqlancerFile path =
            let name = takeFileName path
                prefix = "sqlancer_" <> T.unpack (dbmsId dbms) <> "_"
             in ".log" `isSuffixOf` name
                    && not (prefix `isPrefixOf` name || any (`isSuffixOf` name) ["-plan.log", "-reduce.log"])

readCandidate :: FilePath -> ArtifactSource -> FilePath -> IO (Maybe Candidate)
readCandidate root source path = do
    raw <- BS.readFile path
    let stem = T.pack (takeDirectoryRelativeStem root path)
        sourceField = "fuzzer" .= sourceName source
        blank = BS.all (\byte -> byte == 32 || (byte >= 9 && byte <= 13)) raw
    case source of
        Sqlancer -> do
            let SqlancerSanitizeResult sql meta = sanitizeSqlancerLog raw
                significant line = let stripped = T.strip line in not (T.null stripped || "--" `T.isPrefixOf` stripped)
                provenance = uncurry KM.insert sourceField (metaToObject meta)
            pure
                ( if any significant (T.lines (TE.decodeUtf8 sql))
                    then Just (Candidate ("sqlancer_" <> stem) sql provenance)
                    else Nothing
                )
        _ | blank -> pure Nothing
        Squirrel -> do
            let worker = T.pack (takeFileName (takeDirectory (takeDirectory path)))
                name = "squirrel_" <> worker <> "_" <> T.pack (takeFileName path)
                sql = if BS.isSuffixOf "\n" raw then raw else BS.snoc raw 10
            pure (Just (Candidate name sql (KM.fromList [sourceField, "worker_id" .= worker])))
        _ -> do
            meta <- readSidecar (takeDirectory path </> fileStem path <.> "meta.json")
            pure (Just (Candidate stem raw (KM.union meta (KM.fromList [sourceField]))))

takeDirectoryRelativeStem :: FilePath -> FilePath -> FilePath
takeDirectoryRelativeStem root path =
    let relative = makeRelative root path
        parent = takeDirectory relative
     in if parent == "." then fileStem relative else parent </> fileStem relative

readSidecar :: FilePath -> IO Object
readSidecar path = do
    result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    pure $ case either (const Nothing) decodeStrict' result of
        Just (Object meta) -> meta
        _ -> KM.empty

signatureFrom :: Object -> Text
signatureFrom meta = case KM.lookup "crash_signature" meta of
    Just (Object info) | Just (String expected) <- KM.lookup "expected_signature" info -> expected
    _ -> ""

-- | Typed loader for Occam's Dhall-compiled @harness.json@ configuration and SQUIRREL YAML renderer.
module Occam.Harness.Config (
    DbmsSpec (..),
    ExperimentPlan (..),
    HarnessConfig (..),
    OracleDefaults (..),
    ReductionProfile (..),
    SquirrelRenderOptions (..),
    SquirrelSpec (..),
    decodeHarnessConfig,
    defaultRenderOptionsForSpec,
    loadHarnessConfig,
    lookupDbmsSpec,
    lookupSquirrelSpec,
    renderSquirrelYaml,
    resolveHarnessConfigPath,
    squirrelFieldText,
) where

import Control.Exception.Safe (IOException, try)
import Data.Aeson (
    FromJSON (..),
    Value,
    eitherDecodeStrict',
    encode,
    withObject,
    (.:),
    (.:?),
 )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Numeric.Natural (Natural)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))

-- | Per-DBMS connection, dialect, and container configuration from Dhall.
data DbmsSpec = DbmsSpec
    { dbmsId :: !Text
    , dbmsOccamDialect :: !Text
    , dbmsSqlancerSubcommand :: !(Maybe Text)
    , dbmsSqlancerPort :: !(Maybe Int)
    , dbmsSquirrelDb :: !(Maybe Text)
    , dbmsContainerImage :: !Text
    , dbmsContainerLog :: !(Maybe Text)
    , dbmsServerBased :: !Bool
    , dbmsDefaultHost :: !(Maybe Text)
    , dbmsDefaultPort :: !(Maybe Int)
    , dbmsDefaultUser :: !(Maybe Text)
    , dbmsDefaultDb :: !Text
    }
    deriving (Eq, Show)

instance FromJSON DbmsSpec where
    parseJSON = withObject "DbmsSpec" \obj ->
        DbmsSpec
            <$> obj .: "id"
            <*> obj .: "occam_dialect"
            <*> obj .:? "sqlancer_subcommand"
            <*> obj .:? "sqlancer_port"
            <*> obj .:? "squirrel_db"
            <*> obj .: "container_image"
            <*> obj .:? "container_log"
            <*> obj .: "server_based"
            <*> obj .:? "default_host"
            <*> obj .:? "default_port"
            <*> obj .:? "default_user"
            <*> obj .: "default_db"

-- | Default oracle verification settings shared by batch scripts.
data OracleDefaults = OracleDefaults
    { odAllowPsqlMeta :: !Bool
    , odAllowStderrOnly :: !Bool
    , odRejectOnSlippage :: !Bool
    , odStackFrames :: !Int
    , odTimeoutSecs :: !Double
    , odVerbose :: !Bool
    }
    deriving (Eq, Show)

instance FromJSON OracleDefaults where
    parseJSON = withObject "OracleDefaults" \obj ->
        OracleDefaults
            <$> obj .: "allow_psql_meta"
            <*> obj .: "allow_stderr_only"
            <*> obj .: "reject_on_slippage"
            <*> obj .: "stack_frames"
            <*> obj .: "timeout_secs"
            <*> obj .: "verbose"

-- | Named batch experiment plan.
data ExperimentPlan = ExperimentPlan
    { planName :: !Text
    , planDbms :: ![Text]
    , planProfiles :: ![Text]
    , planRepeats :: !Int
    , planTimeoutSecs :: !Double
    , planBudgetSecs :: !(Maybe Double)
    , planMaxOracleExecutions :: !(Maybe Natural)
    , planWorkerJobs :: !Int
    , planOccamJobs :: !Int
    , planNoCache :: !Bool
    , planContainerWaitSecs :: !Int
    , planOracle :: !OracleDefaults
    }
    deriving (Eq, Show)

instance FromJSON ExperimentPlan where
    parseJSON = withObject "ExperimentPlan" \obj ->
        ExperimentPlan
            <$> obj .: "name"
            <*> obj .: "dbms"
            <*> obj .: "profiles"
            <*> obj .: "repeats"
            <*> obj .: "timeout_secs"
            <*> obj .:? "budget_secs"
            <*> obj .:? "max_oracle_executions"
            <*> obj .: "worker_jobs"
            <*> obj .: "occam_jobs"
            <*> obj .: "no_cache"
            <*> obj .: "container_wait_secs"
            <*> obj .: "oracle"

-- | Named reduction profile with both CLI flags and Occam JSON patch.
data ReductionProfile = ReductionProfile
    { profileKey :: !Text
    , profileName :: !Text
    , profileCliFlags :: ![Text]
    , profileConfig :: !Value
    }
    deriving (Eq, Show)

instance FromJSON ReductionProfile where
    parseJSON = withObject "ReductionProfile" \obj ->
        ReductionProfile
            <$> obj .: "key"
            <*> obj .: "name"
            <*> obj .: "cli_flags"
            <*> obj .: "config"

-- | SQUIRREL fuzzing driver configuration template for one DBMS.
data SquirrelSpec = SquirrelSpec
    { sqDbms :: !Text
    , sqDb :: !Text
    , sqMutatorDbms :: !Text
    , sqTargetBinRel :: !Text
    , sqDefaultInputSubdir :: !Text
    , sqRequired :: ![Text]
    , sqShouldExist :: ![Text]
    , sqInitLibRel :: !Text
    , sqDataLibRel :: !(Maybe Text)
    , sqPragmaRel :: !(Maybe Text)
    , sqDefaultHost :: !(Maybe Text)
    , sqDefaultPort :: !(Maybe Int)
    , sqDefaultUser :: !(Maybe Text)
    , sqDefaultDb :: !(Maybe Text)
    , sqDefaultSocket :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromJSON SquirrelSpec where
    parseJSON = withObject "SquirrelSpec" \obj ->
        SquirrelSpec
            <$> obj .: "dbms"
            <*> obj .: "db"
            <*> obj .: "mutator_dbms"
            <*> obj .: "target_bin_rel"
            <*> obj .: "default_input_subdir"
            <*> obj .: "required"
            <*> obj .: "should_exist"
            <*> obj .: "init_lib_rel"
            <*> obj .:? "data_lib_rel"
            <*> obj .:? "pragma_rel"
            <*> obj .:? "default_host"
            <*> obj .:? "default_port"
            <*> obj .:? "default_user"
            <*> obj .:? "default_db"
            <*> obj .:? "default_socket"

-- | Full decoded @config/generated/harness.json@ bundle.
data HarnessConfig = HarnessConfig
    { hcDbms :: ![DbmsSpec]
    , hcOccamDefaults :: !Value
    , hcOracle :: !OracleDefaults
    , hcPlans :: !(Map Text ExperimentPlan)
    , hcProfiles :: ![ReductionProfile]
    , hcSquirrel :: ![SquirrelSpec]
    }
    deriving (Eq, Show)

instance FromJSON HarnessConfig where
    parseJSON = withObject "HarnessConfig" \obj ->
        HarnessConfig
            <$> obj .: "dbms"
            <*> obj .: "occam_defaults"
            <*> obj .: "oracle"
            <*> obj .: "plans"
            <*> obj .: "profiles"
            <*> obj .: "squirrel"

-- | Parameters supplied to SQUIRREL YAML rendering.
data SquirrelRenderOptions = SquirrelRenderOptions
    { sroDbms :: !Text
    , sroSquirrelHome :: !FilePath
    , sroHost :: !Text
    , sroPort :: !Int
    , sroUser :: !Text
    , sroPassword :: !Text
    , sroSocket :: !Text
    , sroDbName :: !Text
    , sroServerCmd :: !Text
    }
    deriving (Eq, Show)

-- | Build render options populated from spec defaults when available.
defaultRenderOptionsForSpec :: SquirrelSpec -> FilePath -> SquirrelRenderOptions
defaultRenderOptionsForSpec spec squirrelHome =
    SquirrelRenderOptions
        { sroDbms = sqDbms spec
        , sroSquirrelHome = squirrelHome
        , sroHost = fromMaybe "" (sqDefaultHost spec)
        , sroPort = fromMaybe 0 (sqDefaultPort spec)
        , sroUser = fromMaybe "" (sqDefaultUser spec)
        , sroPassword = ""
        , sroSocket = fromMaybe "" (sqDefaultSocket spec)
        , sroDbName = fromMaybe "" (sqDefaultDb spec)
        , sroServerCmd = ""
        }

-- | Decode strictly from raw @harness.json@ UTF-8 bytes.
decodeHarnessConfig :: BS.ByteString -> Either String HarnessConfig
decodeHarnessConfig = eitherDecodeStrict'

-- | Load and validate @harness.json@ from disk.
loadHarnessConfig :: FilePath -> IO (Either String HarnessConfig)
loadHarnessConfig path = do
    res <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    pure $ case res of
        Left err -> Left ("Failed to read harness config '" <> path <> "': " <> show err)
        Right raw -> case decodeHarnessConfig raw of
            Left err -> Left ("Invalid harness config '" <> path <> "': " <> err)
            Right config -> Right config

-- | Resolve @harness.json@ via an explicit path, environment override, or a default location.
resolveHarnessConfigPath :: Maybe FilePath -> IO FilePath
resolveHarnessConfigPath (Just p) = pure p
resolveHarnessConfigPath Nothing = do
    mJson <- lookupEnv "OCCAM_HARNESS_JSON"
    mLegacy <- lookupEnv "OCCAM_HARNESS_CONFIG"
    case listToMaybe [path | Just path <- [mJson, mLegacy], not (null path)] of
        Just path -> pure path
        Nothing -> getCurrentDirectory >>= walkParents
    where
        relativePath = "config" </> "generated" </> "harness.json"
        walkParents dir = do
            let candidate = dir </> relativePath
            found <- doesFileExist candidate
            if found
                then pure candidate
                else do
                    let parent = takeDirectory dir
                    if parent == dir
                        then do
                            let installed = "/usr/local/share/occam" </> relativePath
                            installedExists <- doesFileExist installed
                            pure (if installedExists then installed else relativePath)
                        else walkParents parent

-- | Look up a DBMS specification by @id@.
lookupDbmsSpec :: HarnessConfig -> Text -> Maybe DbmsSpec
lookupDbmsSpec hc name = find ((== name) . dbmsId) (hcDbms hc)

-- | Look up a SQUIRREL profile by @dbms@.
lookupSquirrelSpec :: HarnessConfig -> Text -> Maybe SquirrelSpec
lookupSquirrelSpec hc name = find ((== name) . sqDbms) (hcSquirrel hc)

-- | Extract a scalar SQUIRREL template field matching @dhall_config.py squirrel-field@.
squirrelFieldText :: SquirrelSpec -> Text -> Maybe Text
squirrelFieldText spec field = case field of
    "dbms" -> Just (sqDbms spec)
    "db" -> Just (sqDb spec)
    "mutator_dbms" -> Just (sqMutatorDbms spec)
    "target_bin_rel" -> Just (sqTargetBinRel spec)
    "default_input_subdir" -> Just (sqDefaultInputSubdir spec)
    "init_lib_rel" -> Just (sqInitLibRel spec)
    "data_lib_rel" -> sqDataLibRel spec
    "pragma_rel" -> sqPragmaRel spec
    "default_host" -> sqDefaultHost spec
    "default_port" -> T.pack . show <$> sqDefaultPort spec
    "default_user" -> sqDefaultUser spec
    "default_db" -> sqDefaultDb spec
    "default_socket" -> sqDefaultSocket spec
    _ -> Nothing

-- | Render a deterministic YAML config file for SQUIRREL from a typed Dhall profile.
renderSquirrelYaml :: HarnessConfig -> SquirrelRenderOptions -> Either Text Text
renderSquirrelYaml hc opts = case lookupSquirrelSpec hc (sroDbms opts) of
    Nothing -> Left ("Unsupported SQUIRREL DBMS '" <> sroDbms opts <> "'")
    Just spec -> Right (renderWithSpec spec opts)

renderWithSpec :: SquirrelSpec -> SquirrelRenderOptions -> Text
renderWithSpec spec opts =
    let baseLines =
            [ "---"
            , "required: " <> yamlList (sqRequired spec)
            , "should_exist: " <> yamlList (sqShouldExist spec)
            , "init_lib: " <> joinQuote (sqInitLibRel spec)
            ]
                <> optionalLine "data_lib" (sqDataLibRel spec)
                <> optionalLine "pragma" (sqPragmaRel spec)
                <> ["db: " <> sqDb spec]
        dbLines = case sqDb spec of
            "postgresql" ->
                [ "passwd: " <> yamlQuote (sroPassword opts)
                , "user_name: " <> yamlQuote (sroUser opts)
                , "db_name: " <> yamlQuote (sroDbName opts)
                , "host: " <> yamlQuote (sroHost opts)
                , "port: " <> T.pack (show (sroPort opts))
                , "startup_cmd: " <> yamlQuote (sroServerCmd opts)
                ]
            "mysql" ->
                [ "passwd: " <> yamlQuote (sroPassword opts)
                , "user_name: " <> yamlQuote (sroUser opts)
                , "host: " <> yamlQuote (sroHost opts)
                , "port: " <> T.pack (show (sroPort opts))
                , "sock_path: " <> yamlQuote (sroSocket opts)
                , "db_prefix: " <> yamlQuote (sroDbName opts)
                , "startup_cmd: " <> yamlQuote (sroServerCmd opts)
                ]
            _ -> []
     in T.intercalate "\n" (baseLines <> dbLines) <> "\n"
    where
        joinQuote rel = yamlQuote (T.pack (sroSquirrelHome opts </> T.unpack rel))
        optionalLine key = maybe [] (\rel -> [key <> ": " <> joinQuote rel])
        yamlQuote val = "'" <> T.replace "'" "''" val <> "'"
        yamlList items =
            "[" <> T.intercalate ", " (map (decodeUtf8 . BSL.toStrict . encode) items) <> "]"

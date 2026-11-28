-- | Plan a batch and write its public manifest without starting any workers.
module Occam.Harness.BatchPlan (
    BatchPlanOptions (..),
    ExternalContainer (..),
    Job (..),
    PlanOverrides (..),
    PreparedBatchPlan (..),
    Profile (..),
    Settings (..),
    jobId,
    prepareBatchPlan,
    profileFlagsHash,
) where

import Control.Exception.Safe (IOException, try)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (ToJSON (..), Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, getCurrentTime)
import Occam.AtomicFile (writeAtomicLazyBytes)
import Occam.Harness.BatchInputs (
    BatchCase,
    BatchInputOptions,
    batchCaseId,
    batchCaseOverview,
    batchDbms,
    loadBatchInputs,
 )
import Occam.Harness.Config (
    DbmsSpec (..),
    ExperimentPlan (..),
    HarnessConfig (..),
    OracleDefaults (..),
    ReductionProfile (..),
    loadExperimentPlan,
 )
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | High-level options governing batch matrix expansion, replay, and resume behavior.
data BatchPlanOptions = BatchPlanOptions
    { planInputs :: !BatchInputOptions
    , planReference :: !Text
    , planOutputDir :: !FilePath
    , planSelectedProfiles :: !(Maybe Text)
    , planCustomProfiles :: ![Text]
    , planOverrides :: !PlanOverrides
    , planExternalContainers :: !(Maybe Text)
    , planLocalEmbedded :: !Bool
    , planAllowUnsafeJobs :: !Bool
    , planReplays :: !Int
    , planSkipInputReplay :: !Bool
    , planProbeTimeout :: !Double
    , planResume :: !Bool
    , planRerunFailed :: !Bool
    }
    deriving (Eq, Show)

-- | Explicit CLI\/user overrides layered over reference harness defaults.
data PlanOverrides = PlanOverrides
    { overrideRepeats :: !(Maybe Int)
    , overrideWorkers :: !(Maybe Int)
    , overrideOccamJobs :: !(Maybe Int)
    , overrideContainerWait :: !(Maybe Int)
    , overrideTimeout :: !(Maybe Double)
    , overrideBudget :: !(Maybe Double)
    , overrideMaxExecutions :: !(Maybe Integer)
    , overrideNoCache :: !Bool
    , overrideRejectSlippage :: !Bool
    , overrideStackFrames :: !(Maybe Int)
    }
    deriving (Eq, Show)

-- | Resolved worker concurrency, timeouts, caching, and slippage settings for a batch run.
data Settings = Settings
    { workRepeats :: !Int
    , workWorkers :: !Int
    , workOccamJobs :: !Int
    , workContainerWait :: !Int
    , workTimeout :: !Double
    , workBudget :: !(Maybe Double)
    , workMaxExecutions :: !(Maybe Integer)
    , workNoCache :: !Bool
    , workRejectSlippage :: !Bool
    , workStackFrames :: !Int
    }

instance ToJSON Settings where
    toJSON settings =
        object
            [ "jobs" .= workWorkers settings
            , "occam_jobs" .= workOccamJobs settings
            , "container_wait" .= workContainerWait settings
            , "timeout" .= workTimeout settings
            , "budget" .= workBudget settings
            , "max_oracle_executions" .= workMaxExecutions settings
            , "no_cache" .= workNoCache settings
            , "reject_on_slippage" .= workRejectSlippage settings
            , "stack_frames" .= workStackFrames settings
            ]

-- | Named reducer arm (profile name paired with Occam CLI flags).
data Profile = Profile {profile :: !Text, flags :: ![Text]}
    deriving (Eq, Show)

-- | Deterministic 16-hex-char SHA-256 hash identifying a reducer 'Profile' configuration.
profileFlagsHash :: Profile -> Text
profileFlagsHash (Profile name fs) =
    TE.decodeUtf8 (BS.take 16 (Base16.encode (SHA256.hash (BSL.toStrict (encode (name, fs))))))

instance ToJSON Profile where
    toJSON p = object ["name" .= profile p, "flags" .= flags p]

-- | Single scheduled execution unit pairing a case ID, reducer profile, and repetition index.
data Job = Job !Text !Profile !Int
    deriving (Eq, Show)

-- | Canonical string identifier @case__profile__rN@ for a 'Job'.
jobId :: Job -> Text
jobId (Job cid p repetition) = cid <> "__" <> profile p <> "__r" <> T.pack (show repetition)

instance ToJSON Job where
    toJSON job@(Job cid p repetition) =
        object
            [ "job_id" .= jobId job
            , "case_id" .= cid
            , "profile_name" .= profile p
            , "profile_flags" .= flags p
            , "repeat" .= repetition
            ]

-- | Connection specification for an externally managed container instance.
data ExternalContainer = ExternalContainer !Text !Text !(Maybe Int)
    deriving (Eq, Show)

instance ToJSON ExternalContainer where
    toJSON (ExternalContainer name host port) = object ["container_name" .= name, "host" .= host, "port" .= port]

-- | Typed resolved batch plan returned to CLI preview and native Batch runner.
data PreparedBatchPlan = PreparedBatchPlan
    { pbpPlanPath :: !FilePath
    , pbpOutDir :: !FilePath
    , pbpSettings :: !Settings
    , pbpProfiles :: ![Profile]
    , pbpCases :: ![BatchCase]
    , pbpJobs :: ![Job]
    , pbpExternalContainers :: ![ExternalContainer]
    , pbpPreview :: !Value
    }

-- | Write plan.json atomically and return the strongly-typed 'PreparedBatchPlan'.
prepareBatchPlan ::
    FilePath -> HarnessConfig -> BatchPlanOptions -> IO (Either String PreparedBatchPlan)
prepareBatchPlan configPath config options = do
    result <- try (runExceptT prepare) :: IO (Either IOException (Either String PreparedBatchPlan))
    pure (either (Left . show) id result)
    where
        prepare = do
            experiment <- ExceptT (loadExperimentPlan configPath config (planReference options))
            let settings = resolveSettings (planOverrides options) experiment
            except (validateSettings options settings)
            profiles <- except (resolveProfiles config options experiment)
            inputs <- ExceptT (loadBatchInputs config (planInputs options))
            when (null inputs) (throwE "no matching cases found.")
            containers <- except (validateTargets options settings inputs)
            let jobs =
                    [ Job (batchCaseId item) p repetition
                    | item <- inputs
                    , p <- profiles
                    , repetition <- [1 .. workRepeats settings]
                    ]
                ids = map jobId jobs
            unless
                (Set.size (Set.fromList ids) == length ids)
                (throwE "Case and profile names produce duplicate job IDs")
            out <- lift (makeAbsolute (planOutputDir options))
            now <- lift getCurrentTime
            let path = out </> "plan.json"
                preview = previewJSON path settings inputs jobs
            lift $ do
                createDirectoryIfMissing True out
                writeAtomicLazyBytes
                    path
                    (encode (manifestJSON now out options settings profiles inputs jobs) <> BSL.pack [10])
            pure
                PreparedBatchPlan
                    { pbpPlanPath = path
                    , pbpOutDir = out
                    , pbpSettings = settings
                    , pbpProfiles = profiles
                    , pbpCases = inputs
                    , pbpJobs = jobs
                    , pbpExternalContainers = containers
                    , pbpPreview = preview
                    }

previewJSON :: FilePath -> Settings -> [BatchCase] -> [Job] -> Value
previewJSON path settings inputs jobs =
    object
        [ "dry_run" .= True
        , "plan_path" .= path
        , "case_count" .= length inputs
        , "total_jobs" .= length jobs
        , "jobs_workers" .= workWorkers settings
        , "occam_jobs" .= workOccamJobs settings
        ]

manifestJSON ::
    UTCTime -> FilePath -> BatchPlanOptions -> Settings -> [Profile] -> [BatchCase] -> [Job] -> Value
manifestJSON now out options settings profiles inputs jobs =
    object
        [ "schema_version" .= (1 :: Int)
        , "created_at" .= now
        , "out_dir" .= out
        , "jobs_workers" .= workWorkers settings
        , "occam_jobs" .= workOccamJobs settings
        , "replays" .= planReplays options
        , "skip_input_replay" .= planSkipInputReplay options
        , "reject_on_slippage" .= workRejectSlippage settings
        , "resume" .= planResume options
        , "rerun_failed" .= planRerunFailed options
        , "profiles" .= profiles
        , "case_count" .= length inputs
        , "total_jobs" .= length jobs
        , "cases" .= map batchCaseOverview inputs
        , "job_ids" .= map jobId jobs
        ]

resolveSettings :: PlanOverrides -> ExperimentPlan -> Settings
resolveSettings overrides experiment =
    let oracle = planOracle experiment
        pick :: (ExperimentPlan -> a) -> (PlanOverrides -> Maybe a) -> a
        pick defaultValue option = fromMaybe (defaultValue experiment) (option overrides)
     in Settings
            { workRepeats = pick planRepeats overrideRepeats
            , workWorkers = pick planWorkerJobs overrideWorkers
            , workOccamJobs = pick planOccamJobs overrideOccamJobs
            , workContainerWait = pick planContainerWaitSecs overrideContainerWait
            , workTimeout = pick planTimeoutSecs overrideTimeout
            , workBudget = pick planBudgetSecs (fmap Just . overrideBudget)
            , workMaxExecutions =
                pick (fmap toInteger . planMaxOracleExecutions) (fmap Just . overrideMaxExecutions)
            , workNoCache = overrideNoCache overrides || planNoCache experiment
            , workRejectSlippage = overrideRejectSlippage overrides || odRejectOnSlippage oracle
            , workStackFrames = fromMaybe (odStackFrames oracle) (overrideStackFrames overrides)
            }

validateSettings :: BatchPlanOptions -> Settings -> Either String ()
validateSettings options settings = do
    positive "--jobs" (workWorkers settings)
    positive "--occam-jobs" (workOccamJobs settings)
    positive "--repeats" (workRepeats settings)
    when (planReplays options < 0) (Left "--replays must be non-negative")
    positive "--stack-frames" (workStackFrames settings)
    finite "--timeout" (workTimeout settings)
    mapM_ (finite "--budget") (workBudget settings)
    finite "--probe-timeout" (planProbeTimeout options)
    positive "--container-wait" (workContainerWait settings)
    mapM_
        (\count -> when (count <= 0) (Left "--max-oracle-executions must be positive"))
        (workMaxExecutions settings)
    where
        positive label count = when (count <= 0) (Left (label <> " must be a positive integer"))
        finite label seconds =
            when
                (isNaN seconds || isInfinite seconds || seconds <= 0)
                (Left (label <> " must be finite and positive"))

validateTargets :: BatchPlanOptions -> Settings -> [BatchCase] -> Either String [ExternalContainer]
validateTargets options settings inputs = do
    containers <- maybe (Right []) parseContainers (planExternalContainers options)
    let serverCases = filter (dbmsServerBased . batchDbms) inputs
        hasServer = not (null serverCases)
    when
        (planLocalEmbedded options && not (null containers))
        (Left "--local-embedded cannot be used with --containers")
    when (planLocalEmbedded options && hasServer) $
        Left
            ( "--local-embedded supports sqlite/duckdb only, incompatible with server cases: "
                <> show (map batchCaseId serverCases)
            )
    when (workOccamJobs settings > 1 && hasServer && not (planAllowUnsafeJobs options)) $
        Left
            "--occam-jobs > 1 is unsafe for server DBMSes sharing one database per worker. Use --jobs for isolated workers."
    when (hasServer && not (null containers) && workWorkers settings > length containers) $
        Left ("--jobs requires " <> show (workWorkers settings) <> " isolated containers for server DBMSes")
    pure containers

parseContainers :: Text -> Either String [ExternalContainer]
parseContainers raw = do
    let items = splitNames raw
    when (null items) (Left "Empty container specification")
    traverse parse items
    where
        parse item = do
            let (prefix, address) = T.breakOn "@" item
                (name, host, portText)
                    | T.null address = let (label, port) = splitPort prefix in (label, "", port)
                    | otherwise = let (label, port) = splitPort (T.drop 1 address) in (prefix, label, port)
                stripped = T.strip name
            when (T.null stripped) (Left ("Empty container name in " <> show item))
            port <- traverse readPort portText
            pure (ExternalContainer stripped (if T.null (T.strip host) then "127.0.0.1" else T.strip host) port)
        splitPort value = case T.breakOnEnd ":" value of
            ("", _) -> (value, Nothing)
            (prefix, port) -> (T.dropEnd 1 prefix, Just port)
        readPort rawPort = case readMaybe (T.unpack (fromMaybe stripped (T.stripPrefix "+" stripped))) of
            Just port | port > 0 -> Right port
            _ -> Left ("Container port must be positive: " <> show rawPort)
            where
                stripped = T.strip rawPort

resolveProfiles :: HarnessConfig -> BatchPlanOptions -> ExperimentPlan -> Either String [Profile]
resolveProfiles config options experiment = do
    custom <- Map.fromList <$> traverse parseCustom (planCustomProfiles options)
    let builtIn =
            Map.fromList [(profileKey p, Profile (profileName p) (profileCliFlags p)) | p <- hcProfiles config]
        available = Map.union custom builtIn
        defaults =
            if null (planProfiles experiment)
                then ["Ours-S"]
                else concatMap splitNames (planProfiles experiment)
        names = maybe defaults splitNames (planSelectedProfiles options)
        lookupProfile name = case Map.lookup (T.toLower name) available of
            Just p -> Right p
            Nothing ->
                Left
                    ( "Unknown profile "
                        <> show name
                        <> ". Valid profiles: "
                        <> T.unpack (T.intercalate ", " (map profile (Map.elems available)))
                    )
    profiles <- nubBy (\a b -> profile a == profile b) <$> traverse lookupProfile names
    when (null profiles) (Left "At least one reduction profile must be specified")
    pure profiles
    where
        parseCustom raw = do
            let (prefix, suffix) = T.breakOn "=" raw
                name = T.strip prefix
            when (T.null suffix) (Left ("Invalid --profile-spec " <> show raw <> ", expected NAME=FLAGS"))
            when (T.null name || T.any (\c -> c == '/' || c == '\\' || c < ' ') name) $
                Left ("Unsafe or empty profile name: " <> show name)
            args <- splitFlags (T.unpack (T.drop 1 suffix))
            pure (T.toLower name, Profile name args)

splitNames :: Text -> [Text]
splitNames = filter (not . T.null) . map T.strip . T.splitOn ","

-- The legacy --profile-spec splitter is POSIX shlex with comments disabled.
splitFlags :: String -> Either String [Text]
splitFlags = start
    where
        whitespace c = c `elem` (" \t\r\n" :: String)
        start [] = Right []
        start (c : rest) | whitespace c = start rest
        start raw = word [] raw
        word acc [] = Right [finish acc]
        word acc (c : rest)
            | whitespace c = (finish acc :) <$> start rest
            | c == '\'' = quoted '\'' acc rest
            | c == '"' = quoted '"' acc rest
            | c == '\\' = case rest of
                next : remaining -> word (next : acc) remaining
                [] -> Left "No escaped character in --profile-spec"
            | otherwise = word (c : acc) rest
        quoted _ _ [] = Left "No closing quotation in --profile-spec"
        quoted quote acc (c : rest)
            | c == quote = word acc rest
            | quote == '"' && c == '\\' = case rest of
                next : remaining
                    | next == quote || next == '\\' -> quoted quote (next : acc) remaining
                    | otherwise -> quoted quote (next : c : acc) remaining
                [] -> Left "No escaped character in --profile-spec"
            | otherwise = quoted quote (c : acc) rest
        finish = T.pack . reverse

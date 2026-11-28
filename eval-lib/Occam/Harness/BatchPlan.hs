{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
    { inputs :: !BatchInputOptions
    , reference :: !Text
    , outputDir :: !FilePath
    , selectedProfiles :: !(Maybe Text)
    , customProfiles :: ![Text]
    , overrides :: !PlanOverrides
    , externalContainers :: !(Maybe Text)
    , localEmbedded :: !Bool
    , allowUnsafeJobs :: !Bool
    , replays :: !Int
    , skipInputReplay :: !Bool
    , probeTimeout :: !Double
    , resume :: !Bool
    , rerunFailed :: !Bool
    }
    deriving (Eq, Show)

-- | Explicit CLI\/user overrides layered over reference harness defaults.
data PlanOverrides = PlanOverrides
    { repeats :: !(Maybe Int)
    , workers :: !(Maybe Int)
    , occamJobs :: !(Maybe Int)
    , containerWait :: !(Maybe Int)
    , timeout :: !(Maybe Double)
    , budget :: !(Maybe Double)
    , maxExecutions :: !(Maybe Integer)
    , noCache :: !Bool
    , rejectSlippage :: !Bool
    , stackFrames :: !(Maybe Int)
    }
    deriving (Eq, Show)

-- | Resolved worker concurrency, timeouts, caching, and slippage settings for a batch run.
data Settings = Settings
    { repeats :: !Int
    , workers :: !Int
    , occamJobs :: !Int
    , containerWait :: !Int
    , timeout :: !Double
    , budget :: !(Maybe Double)
    , maxExecutions :: !(Maybe Integer)
    , noCache :: !Bool
    , rejectSlippage :: !Bool
    , stackFrames :: !Int
    }

instance ToJSON Settings where
    toJSON settings =
        object
            [ "jobs" .= settings.workers
            , "occam_jobs" .= settings.occamJobs
            , "container_wait" .= settings.containerWait
            , "timeout" .= settings.timeout
            , "budget" .= settings.budget
            , "max_oracle_executions" .= settings.maxExecutions
            , "no_cache" .= settings.noCache
            , "reject_on_slippage" .= settings.rejectSlippage
            , "stack_frames" .= settings.stackFrames
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
    { planPath :: !FilePath
    , outDir :: !FilePath
    , settings :: !Settings
    , profiles :: ![Profile]
    , cases :: ![BatchCase]
    , jobs :: ![Job]
    , externalContainers :: ![ExternalContainer]
    , preview :: !Value
    }

-- | Write plan.json atomically and return the strongly-typed 'PreparedBatchPlan'.
prepareBatchPlan ::
    FilePath -> HarnessConfig -> BatchPlanOptions -> IO (Either String PreparedBatchPlan)
prepareBatchPlan configPath config options = do
    result <- try (runExceptT prepare) :: IO (Either IOException (Either String PreparedBatchPlan))
    pure (either (Left . show) id result)
    where
        prepare = do
            experiment <- ExceptT (loadExperimentPlan configPath config options.reference)
            let settings = resolveSettings options.overrides experiment
            except (validateSettings options settings)
            profiles <- except (resolveProfiles config options experiment)
            inputs <- ExceptT (loadBatchInputs config options.inputs)
            when (null inputs) (throwE "no matching cases found.")
            containers <- except (validateTargets options settings inputs)
            let jobs =
                    [ Job (batchCaseId item) p repetition
                    | item <- inputs
                    , p <- profiles
                    , repetition <- [1 .. settings.repeats]
                    ]
                ids = map jobId jobs
            unless
                (Set.size (Set.fromList ids) == length ids)
                (throwE "Case and profile names produce duplicate job IDs")
            out <- lift (makeAbsolute options.outputDir)
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
                    { planPath = path
                    , outDir = out
                    , settings = settings
                    , profiles = profiles
                    , cases = inputs
                    , jobs = jobs
                    , externalContainers = containers
                    , preview = preview
                    }

previewJSON :: FilePath -> Settings -> [BatchCase] -> [Job] -> Value
previewJSON path settings inputs jobs =
    object
        [ "dry_run" .= True
        , "plan_path" .= path
        , "case_count" .= length inputs
        , "total_jobs" .= length jobs
        , "jobs_workers" .= settings.workers
        , "occam_jobs" .= settings.occamJobs
        ]

manifestJSON ::
    UTCTime -> FilePath -> BatchPlanOptions -> Settings -> [Profile] -> [BatchCase] -> [Job] -> Value
manifestJSON now out options settings profiles inputs jobs =
    object
        [ "schema_version" .= (1 :: Int)
        , "created_at" .= now
        , "out_dir" .= out
        , "jobs_workers" .= settings.workers
        , "occam_jobs" .= settings.occamJobs
        , "replays" .= options.replays
        , "skip_input_replay" .= options.skipInputReplay
        , "reject_on_slippage" .= settings.rejectSlippage
        , "resume" .= options.resume
        , "rerun_failed" .= options.rerunFailed
        , "profiles" .= profiles
        , "case_count" .= length inputs
        , "total_jobs" .= length jobs
        , "cases" .= map batchCaseOverview inputs
        , "job_ids" .= map jobId jobs
        ]

resolveSettings :: PlanOverrides -> ExperimentPlan -> Settings
resolveSettings overrides experiment =
    let oracle = experiment.oracle
        pick :: (ExperimentPlan -> a) -> (PlanOverrides -> Maybe a) -> a
        pick defaultValue option = fromMaybe (defaultValue experiment) (option overrides)
     in Settings
            { repeats = pick (\plan -> plan.repeats) (\options -> options.repeats)
            , workers = pick (\plan -> plan.workerJobs) (\options -> options.workers)
            , occamJobs = pick (\plan -> plan.occamJobs) (\options -> options.occamJobs)
            , containerWait = pick (\plan -> plan.containerWaitSecs) (\options -> options.containerWait)
            , timeout = pick (\plan -> plan.timeoutSecs) (\options -> options.timeout)
            , budget = pick (\plan -> plan.budgetSecs) (\options -> Just <$> options.budget)
            , maxExecutions =
                pick (\plan -> fmap toInteger plan.maxOracleExecutions) (\options -> Just <$> options.maxExecutions)
            , noCache = overrides.noCache || experiment.noCache
            , rejectSlippage = overrides.rejectSlippage || oracle.rejectOnSlippage
            , stackFrames = fromMaybe oracle.stackFrames overrides.stackFrames
            }

validateSettings :: BatchPlanOptions -> Settings -> Either String ()
validateSettings options settings = do
    positive "--jobs" settings.workers
    positive "--occam-jobs" settings.occamJobs
    positive "--repeats" settings.repeats
    when (options.replays < 0) (Left "--replays must be non-negative")
    positive "--stack-frames" settings.stackFrames
    finite "--timeout" settings.timeout
    mapM_ (finite "--budget") settings.budget
    finite "--probe-timeout" options.probeTimeout
    positive "--container-wait" settings.containerWait
    mapM_
        (\count -> when (count <= 0) (Left "--max-oracle-executions must be positive"))
        settings.maxExecutions
    where
        positive label count = when (count <= 0) (Left (label <> " must be a positive integer"))
        finite label seconds =
            when
                (isNaN seconds || isInfinite seconds || seconds <= 0)
                (Left (label <> " must be finite and positive"))

validateTargets :: BatchPlanOptions -> Settings -> [BatchCase] -> Either String [ExternalContainer]
validateTargets options settings inputs = do
    containers <- maybe (Right []) parseContainers options.externalContainers
    let serverCases = filter (\item -> (batchDbms item).serverBased) inputs
        hasServer = not (null serverCases)
    when
        (options.localEmbedded && not (null containers))
        (Left "--local-embedded cannot be used with --containers")
    when (options.localEmbedded && hasServer) $
        Left
            ( "--local-embedded supports sqlite/duckdb only, incompatible with server cases: "
                <> show (map batchCaseId serverCases)
            )
    when (settings.occamJobs > 1 && hasServer && not options.allowUnsafeJobs) $
        Left
            "--occam-jobs > 1 is unsafe for server DBMSes sharing one database per worker. Use --jobs for isolated workers."
    when (hasServer && not (null containers) && settings.workers > length containers) $
        Left ("--jobs requires " <> show settings.workers <> " isolated containers for server DBMSes")
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
    custom <- Map.fromList <$> traverse parseCustom options.customProfiles
    let builtIn =
            Map.fromList [(profile.key, Profile profile.name profile.cliFlags) | profile <- config.profiles]
        available = Map.union custom builtIn
        defaults =
            if null experiment.profiles
                then ["Ours-S"]
                else concatMap splitNames experiment.profiles
        names = maybe defaults splitNames options.selectedProfiles
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

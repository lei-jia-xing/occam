-- | Typed checkpoint validation and shared job/decision summary types for Batch and BatchReport.
module Occam.Harness.Checkpoints (
    BatchJobResult (..),
    DecisionSummary (..),
    JobCheck (..),
    JobState (..),
    emptyDecisionSummary,
    jobStateText,
    parseJobState,
    readJsonIfExists,
    selectReusableJobs,
    selectReusableQualifications,
) where

import Control.Exception.Safe (IOException, try)
import Control.Monad (guard)
import Data.Aeson (
    FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    decodeStrict',
    object,
    withObject,
    (.!=),
    (.:?),
    (.=),
 )
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Harness.BatchInputs (BatchCase (..))
import Occam.Harness.BatchPlan (Job (..), profileFlagsHash)
import Occam.Harness.Replay (QualificationResult (..), ReplayVerification (..))
import Occam.Harness.Signature (CrashSignature)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Lifecycle state of one job persisted to @runs/<job_id>/job_result.json@.
data JobState
    = JobCompleted
    | JobCensored
    | JobSkippedUnstableInput
    | JobFailed
    | JobError
    | JobInfraError
    | JobValidationFailed
    deriving (Eq, Ord, Show)

-- | Render a 'JobState' as its snake_case checkpoint status string.
jobStateText :: JobState -> Text
jobStateText = \case
    JobCompleted -> "completed"
    JobCensored -> "censored"
    JobSkippedUnstableInput -> "skipped_unstable_input"
    JobFailed -> "failed"
    JobError -> "error"
    JobInfraError -> "infra_error"
    JobValidationFailed -> "validation_failed"

-- | Parse a snake_case checkpoint status string into 'JobState'.
parseJobState :: Text -> Maybe JobState
parseJobState = \case
    "completed" -> Just JobCompleted
    "censored" -> Just JobCensored
    "skipped_unstable_input" -> Just JobSkippedUnstableInput
    "failed" -> Just JobFailed
    "error" -> Just JobError
    "infra_error" -> Just JobInfraError
    "validation_failed" -> Just JobValidationFailed
    _ -> Nothing

instance ToJSON JobState where
    toJSON = String . jobStateText

instance FromJSON JobState where
    parseJSON val = do
        txt <- parseJSON val
        maybe (fail ("Unknown job state: " <> T.unpack txt)) pure (parseJobState txt)

-- | Summary counters extracted from an @oracle_decisions.jsonl@ log.
data DecisionSummary = DecisionSummary
    { dsTotalCalls :: !Int
    , dsAcceptedCalls :: !Int
    , dsConfirmedFaults :: !Int
    , dsHealthyNoFault :: !Int
    , dsMissingEvidence :: !Int
    , dsSlippageEvents :: !Int
    , dsUniqueFaultSignatures :: ![Text]
    }
    deriving (Eq, Show)

-- | Initial zeroed 'DecisionSummary' accumulator.
emptyDecisionSummary :: DecisionSummary
emptyDecisionSummary = DecisionSummary 0 0 0 0 0 0 []

instance ToJSON DecisionSummary where
    toJSON ds =
        object
            [ "total_calls" .= dsTotalCalls ds
            , "accepted_calls" .= dsAcceptedCalls ds
            , "confirmed_faults" .= dsConfirmedFaults ds
            , "healthy_no_fault" .= dsHealthyNoFault ds
            , "missing_evidence" .= dsMissingEvidence ds
            , "slippage_events" .= dsSlippageEvents ds
            , "unique_fault_signatures" .= dsUniqueFaultSignatures ds
            ]

instance FromJSON DecisionSummary where
    parseJSON = withObject "DecisionSummary" \obj ->
        DecisionSummary
            <$> obj .:? "total_calls" .!= 0
            <*> obj .:? "accepted_calls" .!= 0
            <*> obj .:? "confirmed_faults" .!= 0
            <*> obj .:? "healthy_no_fault" .!= 0
            <*> obj .:? "missing_evidence" .!= 0
            <*> obj .:? "slippage_events" .!= 0
            <*> obj .:? "unique_fault_signatures" .!= []

-- | Canonical typed per-job record persisted to @runs/<job_id>/job_result.json@.
data BatchJobResult = BatchJobResult
    { bjrJobId :: !Text
    , bjrCaseId :: !Text
    , bjrDbms :: !Text
    , bjrDialect :: !Text
    , bjrProfile :: !Text
    , bjrRepeat :: !Int
    , bjrState :: !JobState
    , bjrEvalStatus :: !(Maybe Text)
    , bjrInitialBytes :: !Int
    , bjrSqlSha256 :: !Text
    , bjrProfileHash :: !Text
    , bjrConfigHash :: !(Maybe Text)
    , bjrInputQualified :: !Bool
    , bjrRanOccam :: !Bool
    , bjrBaselineSignature :: !(Maybe CrashSignature)
    , bjrStatsPath :: !(Maybe FilePath)
    , bjrReducedReplayVerification :: !(Maybe ReplayVerification)
    , bjrOracleDecisionsSummary :: !(Maybe DecisionSummary)
    , bjrExitCode :: !(Maybe Int)
    , bjrElapsedSeconds :: !(Maybe Double)
    , bjrError :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON BatchJobResult where
    toJSON r =
        object
            [ "job_id" .= bjrJobId r
            , "case_id" .= bjrCaseId r
            , "dbms" .= bjrDbms r
            , "dialect" .= bjrDialect r
            , "profile" .= bjrProfile r
            , "repeat" .= bjrRepeat r
            , "state" .= bjrState r
            , "eval_status" .= bjrEvalStatus r
            , "initial_bytes" .= bjrInitialBytes r
            , "sql_sha256" .= bjrSqlSha256 r
            , "profile_hash" .= bjrProfileHash r
            , "run_config_hash" .= bjrConfigHash r
            , "input_qualified" .= bjrInputQualified r
            , "ran_occam" .= bjrRanOccam r
            , "baseline_signature" .= bjrBaselineSignature r
            , "stats_path" .= bjrStatsPath r
            , "reduced_replay_verification" .= bjrReducedReplayVerification r
            , "oracle_decisions_summary" .= bjrOracleDecisionsSummary r
            , "exit_code" .= bjrExitCode r
            , "elapsed_seconds" .= bjrElapsedSeconds r
            , "error" .= bjrError r
            ]

instance FromJSON BatchJobResult where
    parseJSON = withObject "BatchJobResult" \obj ->
        BatchJobResult
            <$> obj .:? "job_id" .!= ""
            <*> obj .:? "case_id" .!= ""
            <*> obj .:? "dbms" .!= ""
            <*> obj .:? "dialect" .!= ""
            <*> obj .:? "profile" .!= ""
            <*> obj .:? "repeat" .!= 1
            <*> obj .:? "state" .!= JobFailed
            <*> obj .:? "eval_status"
            <*> obj .:? "initial_bytes" .!= 0
            <*> obj .:? "sql_sha256" .!= ""
            <*> obj .:? "profile_hash" .!= ""
            <*> obj .:? "run_config_hash"
            <*> obj .:? "input_qualified" .!= False
            <*> obj .:? "ran_occam" .!= False
            <*> obj .:? "baseline_signature"
            <*> obj .:? "stats_path"
            <*> obj .:? "reduced_replay_verification"
            <*> obj .:? "oracle_decisions_summary"
            <*> obj .:? "exit_code"
            <*> obj .:? "elapsed_seconds"
            <*> obj .:? "error"

-- | Job checkpoint check parameters supplied by @Batch@.
data JobCheck = JobCheck
    { jcJobId :: !Text
    , jcJob :: !Job
    , jcCase :: !BatchCase
    , jcConfigHash :: !Text
    , jcAllowUnstable :: !Bool
    , jcSkipInputReplay :: !Bool
    , jcInputQualified :: !Bool
    }

-- | Return reusable prior input qualifications indexed by @case_id@.
selectReusableQualifications ::
    FilePath -> Int -> Int -> Bool -> [BatchCase] -> IO (Map Text QualificationResult)
selectReusableQualifications outDir replays frames skipInputReplay cases =
    Map.fromList . catMaybes <$> traverse checkOne cases
    where
        checkOne bc = do
            let cid = batchCaseId bc
                qPath = outDir </> "qualified" </> T.unpack cid </> "input_replay.json"
            mQr <- readJsonIfExists qPath :: IO (Maybe QualificationResult)
            pure $ do
                qr <- mQr
                guard (qrCaseId qr == cid)
                guard (qrSqlSha256 qr == batchSqlSha256 bc)
                guard (rvRunsRequired (qrReplay qr) == replays)
                guard (qrExpectedSignature qr == batchSignature bc)
                guard (qrStackFrames qr == frames)
                guard (if skipInputReplay then qrQualified qr else not (qrSkipped qr))
                pure (cid, qr)

-- | Return the set of @job_id@s whose prior artifacts remain valid under current settings.
selectReusableJobs :: FilePath -> Bool -> [JobCheck] -> IO (Set Text)
selectReusableJobs outDir rerunFailed checks =
    Set.fromList . catMaybes <$> traverse checkOne checks
    where
        checkOne jc = do
            let jid = jcJobId jc
                Job _ prof _ = jcJob jc
                bc = jcCase jc
                jPath = outDir </> "runs" </> T.unpack jid </> "job_result.json"
            mRes <- readJsonIfExists jPath :: IO (Maybe BatchJobResult)
            case mRes >>= eligible jid (batchSqlSha256 bc) (profileFlagsHash prof) jc of
                Just res -> do
                    ok <-
                        if bjrState res `elem` [JobCompleted, JobCensored]
                            then validStats res
                            else pure True
                    pure (jid <$ guard ok)
                Nothing -> pure Nothing

        eligible jid sqlDigest profDigest jc res = do
            guard (bjrJobId res == jid)
            guard (bjrSqlSha256 res == sqlDigest)
            guard (bjrProfileHash res == profDigest)
            guard (maybe True (== jcConfigHash jc) (bjrConfigHash res))
            guard (resumeState jc (bjrState res))
            pure res

        resumeState jc = \case
            JobSkippedUnstableInput ->
                not (jcAllowUnstable jc || jcSkipInputReplay jc || rerunFailed || jcInputQualified jc)
            JobCompleted -> True
            JobCensored -> True
            st ->
                not rerunFailed && st `elem` [JobError, JobInfraError, JobValidationFailed]

        validStats res = case bjrStatsPath res of
            Just path | not (null path) -> do
                exists <- doesFileExist path
                if exists
                    then isJust <$> (readJsonIfExists path :: IO (Maybe Object))
                    else pure (not (bjrRanOccam res))
            _ -> pure (not (bjrRanOccam res))

readJsonIfExists :: (FromJSON a) => FilePath -> IO (Maybe a)
readJsonIfExists path = do
    exists <- doesFileExist path
    if exists
        then do
            result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
            pure (either (const Nothing) decodeStrict' result)
        else pure Nothing

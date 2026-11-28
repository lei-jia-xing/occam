-- | In-process multi-run crash replay verification for Collect, Qualification, and Reduced SQL checks.
module Occam.Harness.Replay (
    QualificationResult (..),
    ReplayConfig (..),
    ReplayEndpoint (..),
    ReplayRunRecord (..),
    ReplayVerification (..),
    defaultReplayConfig,
    runReplayVerification,
) where

import Control.Exception.Safe (IOException, try)
import Control.Monad (foldM, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (..), runContT)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, withExceptT)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.ByteString qualified as BS
import Data.IORef (IORef, readIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Harness.Config (HarnessConfig)
import Occam.Harness.DbmsExec (ConnectionOverrides (..))
import Occam.Harness.Oracle (
    OracleClassification (..),
    OracleDecision (..),
    OracleOptions (..),
    OracleResult (..),
    OracleVerdict (..),
    defaultOracleOptions,
    probeCrashOracleCancellable,
 )
import Occam.Harness.Reset (ResetOptions (..), resetDb)
import Occam.Harness.Signature (
    CrashSignature (..),
    DbmsKind (..),
    SlippageVerdict (..),
 )
import System.Directory (doesFileExist, removeFile)

-- | Result of a single replay attempt within 'ReplayVerification'.
data ReplayRunRecord = ReplayRunRecord
    { rrRun :: !Int
    , rrAccepted :: !Bool
    , rrClassification :: !OracleClassification
    , rrReason :: !Text
    , rrSignatureId :: !(Maybe Text)
    , rrSignatureHash :: !(Maybe Text)
    , rrSlipped :: !(Maybe Bool)
    }
    deriving (Eq, Show)

instance ToJSON ReplayRunRecord where
    toJSON r =
        object
            [ "run" .= rrRun r
            , "accepted" .= rrAccepted r
            , "classification" .= rrClassification r
            , "reason" .= rrReason r
            , "signature_id" .= rrSignatureId r
            , "signature_hash" .= rrSignatureHash r
            , "slipped" .= rrSlipped r
            ]

instance FromJSON ReplayRunRecord where
    parseJSON = withObject "ReplayRunRecord" \obj ->
        ReplayRunRecord
            <$> obj .: "run"
            <*> obj .: "accepted"
            <*> obj .: "classification"
            <*> obj .: "reason"
            <*> obj .:? "signature_id"
            <*> obj .:? "signature_hash"
            <*> obj .:? "slipped"

-- | Aggregated verdict across N independent replay attempts of a SQL candidate.
data ReplayVerification = ReplayVerification
    { rvRunsRequired :: !Int
    , rvRunsPassed :: !Int
    , rvAllPassed :: !Bool
    , rvConsistentSignature :: !Bool
    , rvBaselineSignature :: !(Maybe CrashSignature)
    , rvMatchesBaseline :: !(Maybe Bool)
    , rvSlippageCount :: !Int
    , rvObservedSignatures :: ![CrashSignature]
    , rvObservedSignatureIds :: ![Text]
    , rvInterrupted :: !Bool
    , rvRuns :: ![ReplayRunRecord]
    }
    deriving (Eq, Show)

instance ToJSON ReplayVerification where
    toJSON rv =
        object
            [ "runs_required" .= rvRunsRequired rv
            , "runs_passed" .= rvRunsPassed rv
            , "all_passed" .= rvAllPassed rv
            , "consistent_signature" .= rvConsistentSignature rv
            , "baseline_signature" .= rvBaselineSignature rv
            , "matches_baseline" .= rvMatchesBaseline rv
            , "slippage_count" .= rvSlippageCount rv
            , "observed_signatures" .= rvObservedSignatures rv
            , "observed_signature_ids" .= rvObservedSignatureIds rv
            , "interrupted" .= rvInterrupted rv
            , "runs" .= rvRuns rv
            ]

instance FromJSON ReplayVerification where
    parseJSON = withObject "ReplayVerification" \obj ->
        ReplayVerification
            <$> obj .:? "runs_required" .!= 0
            <*> obj .:? "runs_passed" .!= 0
            <*> obj .:? "all_passed" .!= False
            <*> obj .:? "consistent_signature" .!= False
            <*> obj .:? "baseline_signature"
            <*> obj .:? "matches_baseline"
            <*> obj .:? "slippage_count" .!= 0
            <*> obj .:? "observed_signatures" .!= []
            <*> obj .:? "observed_signature_ids" .!= []
            <*> obj .:? "interrupted" .!= False
            <*> obj .:? "runs" .!= []

-- | Persisted qualification outcome for one input case at @qualified/<case_id>/input_replay.json@.
data QualificationResult = QualificationResult
    { qrCaseId :: !Text
    , qrDbms :: !DbmsKind
    , qrDialect :: !Text
    , qrSqlSha256 :: !Text
    , qrExpectedSignature :: !Text
    , qrStackFrames :: !Int
    , qrSkipped :: !Bool
    , qrQualified :: !Bool
    , qrBaselineSignature :: !(Maybe CrashSignature)
    , qrSignatureSlippageAgainstBaseline :: !(Maybe Bool)
    , qrReplay :: !ReplayVerification
    }
    deriving (Eq, Show)

instance ToJSON QualificationResult where
    toJSON qr =
        object
            [ "case_id" .= qrCaseId qr
            , "dbms" .= qrDbms qr
            , "dialect" .= qrDialect qr
            , "sql_sha256" .= qrSqlSha256 qr
            , "expected_signature" .= qrExpectedSignature qr
            , "stack_frames" .= qrStackFrames qr
            , "skipped" .= qrSkipped qr
            , "qualified" .= qrQualified qr
            , "baseline_signature" .= qrBaselineSignature qr
            , "signature_slippage_against_baseline" .= qrSignatureSlippageAgainstBaseline qr
            , "replay" .= qrReplay qr
            ]

instance FromJSON QualificationResult where
    parseJSON = withObject "QualificationResult" \obj ->
        QualificationResult
            <$> obj .: "case_id"
            <*> obj .: "dbms"
            <*> obj .:? "dialect" .!= ""
            <*> obj .:? "sql_sha256" .!= ""
            <*> obj .:? "expected_signature" .!= ""
            <*> obj .:? "stack_frames" .!= 5
            <*> obj .:? "skipped" .!= False
            <*> obj .:? "qualified" .!= False
            <*> obj .:? "baseline_signature"
            <*> obj .:? "signature_slippage_against_baseline"
            <*> obj .: "replay"

-- | Endpoint coordinates fetched fresh before each probe (handles container port changes after recreate).
data ReplayEndpoint = ReplayEndpoint
    { reConnection :: !ConnectionOverrides
    , reLogFile :: !(Maybe FilePath)
    , reContainerLog :: !(Maybe FilePath)
    }
    deriving (Eq, Show)

-- | Replay harness configuration passed to 'runReplayVerification'.
data ReplayConfig = ReplayConfig
    { rcDbms :: !DbmsKind
    , rcEndpoint :: !(IO (Either String ReplayEndpoint))
    , rcResetFn :: !(Maybe (IO (Either String ())))
    , rcExpectedSignature :: !Text
    , rcBaselineSignature :: !(Maybe Text)
    , rcRejectOnSlippage :: !Bool
    , rcStackFrames :: !Int
    , rcDecisionLog :: !(Maybe FilePath)
    , rcAllowStderrOnly :: !Bool
    , rcAllowPsqlMeta :: !Bool
    , rcProbeTimeout :: !Double
    , rcMaxWaitReset :: !Int
    , rcResetBeforeRun :: !Bool
    , rcRequireConsistent :: !Bool
    }

-- | Default 'ReplayConfig' targeting the given DBMS and connection overrides.
defaultReplayConfig :: DbmsKind -> ConnectionOverrides -> ReplayConfig
defaultReplayConfig dbms connOv =
    ReplayConfig
        { rcDbms = dbms
        , rcEndpoint = pure (Right (ReplayEndpoint connOv Nothing Nothing))
        , rcResetFn = Nothing
        , rcExpectedSignature = ""
        , rcBaselineSignature = Nothing
        , rcRejectOnSlippage = False
        , rcStackFrames = 5
        , rcDecisionLog = Nothing
        , rcAllowStderrOnly = False
        , rcAllowPsqlMeta = False
        , rcProbeTimeout = 15.0
        , rcMaxWaitReset = 30
        , rcResetBeforeRun = True
        , rcRequireConsistent = True
        }

data ReplayState = ReplayState
    { rsPassed :: !Int
    , rsSlips :: !Int
    , rsSeenIds :: !(Set.Set Text)
    , rsRevSigs :: ![CrashSignature]
    , rsRevRuns :: ![ReplayRunRecord]
    }

-- | Replay @sqlBytes@ @replays@ times using in-process 'resetDb' + 'probeCrashOracleCancellable'.
runReplayVerification ::
    HarnessConfig ->
    Maybe (IORef Bool) ->
    ReplayConfig ->
    Int ->
    BS.ByteString ->
    IO ReplayVerification
runReplayVerification hc mStopRef rcfg replays sqlBytes
    | replays <= 0 =
        pure
            ReplayVerification
                { rvRunsRequired = 0
                , rvRunsPassed = 0
                , rvAllPassed = True
                , rvConsistentSignature = False
                , rvBaselineSignature = Nothing
                , rvMatchesBaseline = Nothing
                , rvSlippageCount = 0
                , rvObservedSignatures = []
                , rvObservedSignatureIds = []
                , rvInterrupted = False
                , rvRuns = []
                }
    | otherwise = do
        removeFileIfExists (rcDecisionLog rcfg)
        let initState = ReplayState 0 0 Set.empty [] []
        (`runContT` (pure . (`finalize` False))) $
            foldM runReplayAttempt initState [1 .. replays]
    where
        hasBaseline = maybe False (not . T.null . T.strip) (rcBaselineSignature rcfg)
        dbms = rcDbms rcfg
        isStopped = maybe (pure False) readIORef mStopRef

        abortNow :: ReplayState -> ContT ReplayVerification IO a
        abortNow st = ContT \_ -> pure (finalize st True)

        abortIfStopped :: ReplayState -> ContT ReplayVerification IO ()
        abortIfStopped st = do
            stopped <- liftIO isStopped
            when stopped (abortNow st)

        runReplayAttempt :: ReplayState -> Int -> ContT ReplayVerification IO ReplayState
        runReplayAttempt st idx = do
            outcome <- runExceptT do
                lift (abortIfStopped st)
                when (rcResetBeforeRun rcfg) $
                    withExceptT (failedRecord idx "reset-failed") (ExceptT (liftIO performReset))
                lift (abortIfStopped st)
                ep <- withExceptT (failedRecord idx "endpoint-error") (ExceptT (liftIO (rcEndpoint rcfg)))
                (wasInterrupted, oRes) <-
                    liftIO (probeCrashOracleCancellable hc mStopRef (oracleOptsFor ep) sqlBytes)
                when wasInterrupted $ lift (abortNow st)
                pure oRes
            pure $ either (recordOnly st) (applyProbeResult st idx) outcome

        performReset :: IO (Either String ())
        performReset = case rcResetFn rcfg of
            Just fn -> fn
            Nothing -> rcEndpoint rcfg >>= either (pure . Left) defaultReset

        defaultReset ep =
            resetDb hc (ResetOptions dbms (reConnection ep) (rcMaxWaitReset rcfg))

        oracleOptsFor ep =
            (defaultOracleOptions dbms)
                { ooConnection = reConnection ep
                , ooExpectedSignature = rcExpectedSignature rcfg
                , ooBaselineSignature = fromMaybe "" (rcBaselineSignature rcfg)
                , ooRejectOnSlippage = rcRejectOnSlippage rcfg
                , ooStackFrames = rcStackFrames rcfg
                , ooLogFile = reLogFile ep
                , ooContainerLog = reContainerLog ep
                , ooDecisionLog = rcDecisionLog rcfg
                , ooTimeoutSeconds = rcProbeTimeout rcfg
                , ooAllowStderrOnly = rcAllowStderrOnly rcfg
                , ooAllowPsqlMeta = rcAllowPsqlMeta rcfg
                , ooVerbose = False
                , ooWorkdir = Nothing
                }

        failedRecord idx prefix err =
            ReplayRunRecord
                { rrRun = idx
                , rrAccepted = False
                , rrClassification = ClassMissingEvidence
                , rrReason = prefix <> ": " <> T.take 200 (T.pack err)
                , rrSignatureId = Nothing
                , rrSignatureHash = Nothing
                , rrSlipped = Nothing
                }

        recordOnly st rec = st {rsRevRuns = rec : rsRevRuns st}

        applyProbeResult st idx oRes =
            let dec = orDecision oRes
                accepted = orExitCode oRes == 0 && odVerdict dec == VerdictAccepted
                mSig = odCrashSignature dec
                mSlipped = svSlipped <$> odSlippage dec
                slippedThis = fromMaybe False mSlipped
                (seenIds', revSigs') = case mSig of
                    Just sig
                        | Set.notMember (csId sig) (rsSeenIds st) ->
                            (Set.insert (csId sig) (rsSeenIds st), sig : rsRevSigs st)
                    _ -> (rsSeenIds st, rsRevSigs st)
                rec =
                    ReplayRunRecord
                        { rrRun = idx
                        , rrAccepted = accepted
                        , rrClassification = odClassification dec
                        , rrReason = odReason dec
                        , rrSignatureId = csId <$> mSig
                        , rrSignatureHash = csHash <$> mSig
                        , rrSlipped = if hasBaseline then mSlipped else Nothing
                        }
             in st
                    { rsPassed = rsPassed st + if accepted then 1 else 0
                    , rsSlips = rsSlips st + if hasBaseline && slippedThis then 1 else 0
                    , rsSeenIds = seenIds'
                    , rsRevSigs = revSigs'
                    , rsRevRuns = rec : rsRevRuns st
                    }

        removeFileIfExists Nothing = pure ()
        removeFileIfExists (Just path) = do
            exists <- doesFileExist path
            when exists $ void (try (removeFile path) :: IO (Either IOException ()))

        finalize st interrupted =
            let obsSigs = reverse (rsRevSigs st)
                obsIds = map csId obsSigs
                allPassed = not interrupted && replays > 0 && rsPassed st == replays
                consistentSig = allPassed && length obsSigs == 1
                chosenBaseline
                    | allPassed && (not (rcRequireConsistent rcfg) || consistentSig) = listToMaybe obsSigs
                    | otherwise = Nothing
                matchesBase
                    | not hasBaseline = Nothing
                    | otherwise =
                        Just (allPassed && (not (rcRequireConsistent rcfg) || consistentSig) && rsSlips st == 0)
             in ReplayVerification
                    { rvRunsRequired = replays
                    , rvRunsPassed = rsPassed st
                    , rvAllPassed = allPassed
                    , rvConsistentSignature = consistentSig
                    , rvBaselineSignature = chosenBaseline
                    , rvMatchesBaseline = matchesBase
                    , rvSlippageCount = rsSlips st
                    , rvObservedSignatures = obsSigs
                    , rvObservedSignatureIds = obsIds
                    , rvInterrupted = interrupted
                    , rvRuns = reverse (rsRevRuns st)
                    }

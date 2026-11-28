{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Oracle verdict, evidence, and configuration types shared by the public API and implementation.
module Occam.Harness.Oracle.Types (
    OracleVerdict (..),
    OracleClassification (..),
    OracleDecision (..),
    OracleResult (..),
    OracleOptions (..),
    OracleOverrides (..),
    LogMode (..),
    defaultOracleOptions,
    emptyOracleOverrides,
    verdictText,
    classificationText,
    logModeLabel,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Harness.DbmsExec (ConnectionOverrides, emptyConnectionOverrides)
import Occam.Harness.Signature (
    Corroboration,
    CrashSignature,
    DbmsKind,
    FaultEvent,
    SlippageVerdict,
 )

-- | Oracle accept/reject decision semantics.
data OracleVerdict
    = VerdictAccepted
    | VerdictRejected
    | VerdictTimeout
    deriving (Eq, Show)

-- | Canonical lowercase string encoding of an 'OracleVerdict' for JSON logs.
verdictText :: OracleVerdict -> Text
verdictText = \case
    VerdictAccepted -> "accepted"
    VerdictRejected -> "rejected"
    VerdictTimeout -> "timeout"

instance ToJSON OracleVerdict where
    toJSON = String . verdictText

instance FromJSON OracleVerdict where
    parseJSON val = do
        txt <- parseJSON val
        case txt of
            "accepted" -> pure VerdictAccepted
            "rejected" -> pure VerdictRejected
            "timeout" -> pure VerdictTimeout
            _ -> fail ("Unknown oracle verdict: " <> T.unpack txt)

-- | Evidence classification distinguishing confirmed faults from benign outcomes and missing evidence.
data OracleClassification
    = ClassConfirmedFault
    | ClassHealthyNoFault
    | ClassMissingEvidence
    deriving (Eq, Show)

-- | Canonical lowercase string encoding of an 'OracleClassification' for JSON logs.
classificationText :: OracleClassification -> Text
classificationText = \case
    ClassConfirmedFault -> "confirmed_fault"
    ClassHealthyNoFault -> "healthy_no_fault"
    ClassMissingEvidence -> "missing_evidence"

instance ToJSON OracleClassification where
    toJSON = String . classificationText

instance FromJSON OracleClassification where
    parseJSON val = do
        txt <- parseJSON val
        case txt of
            "confirmed_fault" -> pure ClassConfirmedFault
            "healthy_no_fault" -> pure ClassHealthyNoFault
            "missing_evidence" -> pure ClassMissingEvidence
            _ -> fail ("Unknown oracle classification: " <> T.unpack txt)

-- | Complete structured verdict emitted to @--decision-log@ and returned to @Replay@.
data OracleDecision = OracleDecision
    { timestamp :: !Text
    -- ^ UTC timestamp when this probe was recorded.
    , dbms :: !DbmsKind
    -- ^ DBMS used for the probe.
    , candidateSha16 :: !Text
    -- ^ First 16 hexadecimal characters of the candidate SQL SHA-256.
    , candidateBytes :: !Int
    -- ^ Candidate SQL length in bytes.
    , expectedSignature :: !Text
    -- ^ Signature required for acceptance, if configured.
    , baselineSignature :: !(Maybe Text)
    -- ^ Optional baseline signature used for slippage checks.
    , workdir :: !(Maybe FilePath)
    -- ^ Optional working directory for the DBMS client process.
    , verdict :: !OracleVerdict
    -- ^ Oracle accept/reject/timeout result.
    , classification :: !OracleClassification
    -- ^ Evidence classification associated with the verdict.
    , reason :: !Text
    -- ^ Stable machine-readable explanation for the verdict.
    , logMode :: !(Maybe Text)
    -- ^ Evidence transport used to observe the DBMS.
    , logFile :: !(Maybe FilePath)
    -- ^ Optional path to the evidence log.
    , retcode :: !(Maybe Int)
    -- ^ DBMS client process exit status, when available.
    , connectionLost :: !(Maybe Bool)
    -- ^ Whether client output indicates a lost connection.
    , regexValid :: !(Maybe Bool)
    -- ^ Whether the configured expected-signature expression is valid.
    , timeoutSeconds :: !(Maybe Double)
    -- ^ Configured client timeout, when a timeout occurred.
    , indicators :: !(Maybe [Text])
    -- ^ Human-readable evidence indicators.
    , faultEvents :: !(Maybe [FaultEvent])
    -- ^ Extracted DBMS fault events.
    , corroboration :: !(Maybe Corroboration)
    -- ^ Evidence corroboration details.
    , crashSignature :: !(Maybe CrashSignature)
    -- ^ Normalized crash signature, when one could be built.
    , slippage :: !(Maybe SlippageVerdict)
    -- ^ Comparison with the configured baseline signature.
    , hint :: !(Maybe Text)
    -- ^ Additional diagnostic hint for the user.
    }
    deriving (Eq, Show)

instance ToJSON OracleDecision where
    toJSON d =
        object $
            [ "ts" .= d.timestamp
            , "dbms" .= d.dbms
            , "candidate_sha256_16" .= d.candidateSha16
            , "candidate_bytes" .= d.candidateBytes
            , "expected_signature" .= d.expectedSignature
            , "workdir" .= d.workdir
            , "verdict" .= d.verdict
            , "classification" .= d.classification
            , "reason" .= d.reason
            ]
                <> opt "baseline_signature" d.baselineSignature
                <> opt "log_mode" d.logMode
                <> opt "log_file" d.logFile
                <> opt "retcode" d.retcode
                <> opt "connection_lost" d.connectionLost
                <> opt "regex_valid" d.regexValid
                <> opt "timeout" d.timeoutSeconds
                <> opt "indicators" d.indicators
                <> opt "fault_events" d.faultEvents
                <> opt "corroboration" d.corroboration
                <> opt "crash_signature" d.crashSignature
                <> opt "slippage" d.slippage
                <> opt "hint" d.hint
        where
            opt k = maybe [] (\v -> [k .= v])

-- | Result of a single crash oracle probe.
data OracleResult = OracleResult
    { orExitCode :: !Int
    -- ^ Exit status returned by the oracle command.
    , orDecision :: !OracleDecision
    -- ^ Structured verdict and evidence details.
    }
    deriving (Eq, Show)

-- | Fully resolved configuration for @probeCrashOracle@.
data OracleOptions = OracleOptions
    { dbms :: !DbmsKind
    -- ^ DBMS this option set targets.
    , connection :: !ConnectionOverrides
    -- ^ Connection details used to resolve the DBMS endpoint.
    , expectedSignature :: !Text
    -- ^ Expected crash signature; empty means no exact signature is required.
    , baselineSignature :: !Text
    -- ^ Baseline signature used for optional slippage rejection.
    , rejectOnSlippage :: !Bool
    -- ^ Reject a confirmed fault when it differs from the baseline.
    , stackFrames :: !Int
    -- ^ Number of stack frames included in signatures.
    , printSignature :: !Bool
    -- ^ Print the normalized crash signature to standard output.
    , logFile :: !(Maybe FilePath)
    -- ^ Optional host path to the DBMS log.
    , containerLog :: !(Maybe FilePath)
    -- ^ Optional path to the DBMS log inside its container.
    , decisionLog :: !(Maybe FilePath)
    -- ^ Optional JSON-lines decision log destination.
    , timeoutSeconds :: !Double
    -- ^ Client process timeout in seconds; zero disables it.
    , allowStderrOnly :: !Bool
    -- ^ Permit stderr-only evidence when no server log is available.
    , allowPsqlMeta :: !Bool
    -- ^ Permit PostgreSQL psql meta-commands in candidate SQL.
    , verbose :: !Bool
    -- ^ Print a concise verdict to standard error.
    , workdir :: !(Maybe FilePath)
    -- ^ Optional working directory for the DBMS client process.
    }
    deriving (Eq, Show)

-- | Default 'OracleOptions' with 5 stack frames, zero timeout, and strict evidence checks.
defaultOracleOptions :: DbmsKind -> OracleOptions
defaultOracleOptions dbms =
    OracleOptions
        { dbms = dbms
        , connection = emptyConnectionOverrides
        , expectedSignature = ""
        , baselineSignature = ""
        , rejectOnSlippage = False
        , stackFrames = 5
        , printSignature = False
        , logFile = Nothing
        , containerLog = Nothing
        , decisionLog = Nothing
        , timeoutSeconds = 0.0
        , allowStderrOnly = False
        , allowPsqlMeta = False
        , verbose = False
        , workdir = Nothing
        }

-- | Partial CLI flags merged on top of environment variables and Dhall defaults.
data OracleOverrides = OracleOverrides
    { dbms :: !(Maybe DbmsKind)
    -- ^ DBMS override; absent values use environment and configuration defaults.
    , connection :: !ConnectionOverrides
    -- ^ Connection overrides supplied by CLI flags.
    , expectedSignature :: !(Maybe Text)
    -- ^ Expected crash signature override.
    , baselineSignature :: !(Maybe Text)
    -- ^ Baseline signature override.
    , rejectOnSlippage :: !Bool
    -- ^ Enable rejection for signature slippage.
    , stackFrames :: !(Maybe Int)
    -- ^ Stack-frame count override.
    , printSignature :: !Bool
    -- ^ Print the normalized crash signature.
    , logFile :: !(Maybe FilePath)
    -- ^ Host log path override.
    , containerLog :: !(Maybe FilePath)
    -- ^ Container log path override.
    , decisionLog :: !(Maybe FilePath)
    -- ^ Decision log path override.
    , timeoutSeconds :: !(Maybe Double)
    -- ^ Client timeout override in seconds.
    , allowStderrOnly :: !Bool
    -- ^ Permit stderr-only evidence.
    , allowPsqlMeta :: !Bool
    -- ^ Permit PostgreSQL psql meta-commands.
    , verbose :: !Bool
    -- ^ Enable verbose verdict reporting.
    }
    deriving (Eq, Show)

-- | Empty 'OracleOverrides' leaving all configuration fields unspecified.
emptyOracleOverrides :: OracleOverrides
emptyOracleOverrides =
    OracleOverrides
        { dbms = Nothing
        , connection = emptyConnectionOverrides
        , expectedSignature = Nothing
        , baselineSignature = Nothing
        , rejectOnSlippage = False
        , stackFrames = Nothing
        , printSignature = False
        , logFile = Nothing
        , containerLog = Nothing
        , decisionLog = Nothing
        , timeoutSeconds = Nothing
        , allowStderrOnly = False
        , allowPsqlMeta = False
        , verbose = False
        }

-- | Active server/container log observation transport for an oracle probe.
data LogMode
    = ModeEmbeddedLocal
    | ModeEmbeddedContainer !Text
    | ModeFile !FilePath
    | ModeContainer !Text !FilePath
    | ModeStderr
    deriving (Eq, Show)

-- | Machine-readable label recorded in @OracleDecision.logMode@.
logModeLabel :: LogMode -> Text
logModeLabel = \case
    ModeEmbeddedLocal -> "embedded_local"
    ModeEmbeddedContainer _ -> "embedded_container"
    ModeFile _ -> "file"
    ModeContainer _ _ -> "container"
    ModeStderr -> "stderr"

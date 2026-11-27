-- | Haskell-native DBMS crash oracle (@occam-harness oracle@ and direct in-process probe API).
module Occam.Harness.Oracle (
    OracleClassification (..),
    OracleDecision (..),
    OracleOptions (..),
    OracleOverrides (..),
    OracleResult (..),
    OracleVerdict (..),
    defaultOracleOptions,
    emptyOracleOverrides,
    probeCrashOracle,
    probeCrashOracleCancellable,
    resolveOracleOptions,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (IOException, try)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import Data.IORef (IORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Occam.Harness.Config (HarnessConfig (..), OracleDefaults (..))
import Occam.Harness.DbmsExec (
    ClientRole (..),
    ConnectionOverrides (..),
    DbmsConnection (..),
    buildDbmsCommand,
    emptyConnectionOverrides,
    resolveDbmsConnection,
    runProcessCapture,
    runProcessCaptureCancellable,
 )
import Occam.Harness.Evidence (
    connectionLostPhrasesForDbms,
    extractDbmsEvidence,
    hasPsqlMetaCommand,
 )
import Occam.Harness.Regex (matchExpectedSignature)
import Occam.Harness.Signature (
    Corroboration (..),
    CrashSignature (..),
    DbmsKind (..),
    EvidenceResult (..),
    FaultEvent (..),
    SlippageVerdict (..),
    buildCrashSignature,
    evaluateSlippage,
    isServerDbmsKind,
    loadBaselineRef,
    parseDbmsKind,
 )
import System.Directory (doesDirectoryExist, getHomeDirectory, makeAbsolute)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((</>))
import System.IO (
    IOMode (ReadMode),
    SeekMode (AbsoluteSeek),
    hPutStrLn,
    hSeek,
    stderr,
    withBinaryFile,
 )
import System.Posix.Files (FileStatus, fileID, fileSize, getFileStatus)
import Text.Read (readMaybe)

maxLogReadBytes :: Integer
maxLogReadBytes = 8 * 1024 * 1024

maxTotalLogChars :: Int
maxTotalLogChars = 32 * 1024 * 1024

logWaitSeconds :: Double
logWaitSeconds = 2.0

logPollIntervalMicros :: Int
logPollIntervalMicros = 50_000

-- | Oracle accept/reject decision semantics.
data OracleVerdict
    = VerdictAccepted
    | VerdictRejected
    | VerdictTimeout
    deriving (Eq, Show)

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
    { odTimestamp :: !Text
    , odDbms :: !DbmsKind
    , odCandidateSha16 :: !Text
    , odCandidateBytes :: !Int
    , odExpectedSignature :: !Text
    , odBaselineSignature :: !(Maybe Text)
    , odWorkdir :: !(Maybe FilePath)
    , odVerdict :: !OracleVerdict
    , odClassification :: !OracleClassification
    , odReason :: !Text
    , odLogMode :: !(Maybe Text)
    , odLogFile :: !(Maybe FilePath)
    , odRetcode :: !(Maybe Int)
    , odConnectionLost :: !(Maybe Bool)
    , odRegexValid :: !(Maybe Bool)
    , odTimeoutSeconds :: !(Maybe Double)
    , odIndicators :: !(Maybe [Text])
    , odFaultEvents :: !(Maybe [FaultEvent])
    , odCorroboration :: !(Maybe Corroboration)
    , odCrashSignature :: !(Maybe CrashSignature)
    , odSlippage :: !(Maybe SlippageVerdict)
    , odHint :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON OracleDecision where
    toJSON d =
        object $
            [ "ts" .= odTimestamp d
            , "dbms" .= odDbms d
            , "candidate_sha256_16" .= odCandidateSha16 d
            , "candidate_bytes" .= odCandidateBytes d
            , "expected_signature" .= odExpectedSignature d
            , "workdir" .= odWorkdir d
            , "verdict" .= odVerdict d
            , "classification" .= odClassification d
            , "reason" .= odReason d
            ]
                <> opt "baseline_signature" (odBaselineSignature d)
                <> opt "log_mode" (odLogMode d)
                <> opt "log_file" (odLogFile d)
                <> opt "retcode" (odRetcode d)
                <> opt "connection_lost" (odConnectionLost d)
                <> opt "regex_valid" (odRegexValid d)
                <> opt "timeout" (odTimeoutSeconds d)
                <> opt "indicators" (odIndicators d)
                <> opt "fault_events" (odFaultEvents d)
                <> opt "corroboration" (odCorroboration d)
                <> opt "crash_signature" (odCrashSignature d)
                <> opt "slippage" (odSlippage d)
                <> opt "hint" (odHint d)
        where
            opt k = maybe [] (\v -> [k .= v])

-- | Result of a single crash oracle probe.
data OracleResult = OracleResult
    { orExitCode :: !Int
    , orDecision :: !OracleDecision
    }
    deriving (Eq, Show)

-- | Fully resolved configuration for @probeCrashOracle@.
data OracleOptions = OracleOptions
    { ooDbms :: !DbmsKind
    , ooConnection :: !ConnectionOverrides
    , ooExpectedSignature :: !Text
    , ooBaselineSignature :: !Text
    , ooRejectOnSlippage :: !Bool
    , ooStackFrames :: !Int
    , ooPrintSignature :: !Bool
    , ooLogFile :: !(Maybe FilePath)
    , ooContainerLog :: !(Maybe FilePath)
    , ooDecisionLog :: !(Maybe FilePath)
    , ooTimeoutSeconds :: !Double
    , ooAllowStderrOnly :: !Bool
    , ooAllowPsqlMeta :: !Bool
    , ooVerbose :: !Bool
    , ooWorkdir :: !(Maybe FilePath)
    }
    deriving (Eq, Show)

defaultOracleOptions :: DbmsKind -> OracleOptions
defaultOracleOptions dbms =
    OracleOptions
        { ooDbms = dbms
        , ooConnection = emptyConnectionOverrides
        , ooExpectedSignature = ""
        , ooBaselineSignature = ""
        , ooRejectOnSlippage = False
        , ooStackFrames = 5
        , ooPrintSignature = False
        , ooLogFile = Nothing
        , ooContainerLog = Nothing
        , ooDecisionLog = Nothing
        , ooTimeoutSeconds = 0.0
        , ooAllowStderrOnly = False
        , ooAllowPsqlMeta = False
        , ooVerbose = False
        , ooWorkdir = Nothing
        }

-- | Partial CLI flags merged on top of environment variables and Dhall defaults.
data OracleOverrides = OracleOverrides
    { ovDbms :: !(Maybe DbmsKind)
    , ovConnection :: !ConnectionOverrides
    , ovExpectedSignature :: !(Maybe Text)
    , ovBaselineSignature :: !(Maybe Text)
    , ovRejectOnSlippage :: !Bool
    , ovStackFrames :: !(Maybe Int)
    , ovPrintSignature :: !Bool
    , ovLogFile :: !(Maybe FilePath)
    , ovContainerLog :: !(Maybe FilePath)
    , ovDecisionLog :: !(Maybe FilePath)
    , ovTimeoutSeconds :: !(Maybe Double)
    , ovAllowStderrOnly :: !Bool
    , ovAllowPsqlMeta :: !Bool
    , ovVerbose :: !Bool
    }
    deriving (Eq, Show)

emptyOracleOverrides :: OracleOverrides
emptyOracleOverrides =
    OracleOverrides
        { ovDbms = Nothing
        , ovConnection = emptyConnectionOverrides
        , ovExpectedSignature = Nothing
        , ovBaselineSignature = Nothing
        , ovRejectOnSlippage = False
        , ovStackFrames = Nothing
        , ovPrintSignature = False
        , ovLogFile = Nothing
        , ovContainerLog = Nothing
        , ovDecisionLog = Nothing
        , ovTimeoutSeconds = Nothing
        , ovAllowStderrOnly = False
        , ovAllowPsqlMeta = False
        , ovVerbose = False
        }

resolveOracleOptions :: HarnessConfig -> OracleOverrides -> IO (Either String OracleOptions)
resolveOracleOptions cfg ov = do
    let defs = hcOracle cfg
    envDbms <- lookupEnv "DBMS"
    dbms <- case ovDbms ov <|> (envDbms >>= parseDbmsKind . T.pack) of
        Just d -> pure (Right d)
        Nothing -> case envDbms of
            Just raw | not (null raw) -> pure (Left ("Unsupported DBMS in DBMS env: " <> raw))
            _ -> pure (Right DbmsPostgres)
    case dbms of
        Left err -> pure (Left err)
        Right dbmsKind -> do
            expSig <- maybe (maybe "" T.pack <$> lookupEnv "EXPECTED_SIGNATURE") pure (ovExpectedSignature ov)
            baseSig <- maybe (maybe "" T.pack <$> lookupEnv "BASELINE_SIGNATURE") pure (ovBaselineSignature ov)
            envReject <- envFlag "ORACLE_REJECT_ON_SLIPPAGE"
            envFrames <- envInt "ORACLE_STACK_FRAMES"
            envTimeout <- envDouble "ORACLE_TIMEOUT"
            envAllowStderr <- envFlag "ORACLE_ALLOW_STDERR_ONLY"
            envAllowMeta <- envFlag "ORACLE_ALLOW_PSQL_META"
            envVerbose <- envFlag "ORACLE_VERBOSE"
            envDecLog <- nonEmptyStr <$> lookupEnv "ORACLE_DECISION_LOG"
            envWorkdir <- nonEmptyStr <$> lookupEnv "OCCAM_WORKDIR"

            let stackFrames = fromMaybe (odStackFrames defs) (ovStackFrames ov <|> envFrames)
                timeoutSecs = fromMaybe (odTimeoutSecs defs) (ovTimeoutSeconds ov <|> envTimeout)
            if isNaN timeoutSecs || isInfinite timeoutSecs || timeoutSecs < 0
                then pure (Left "--timeout must be a finite non-negative number")
                else
                    if stackFrames <= 0
                        then pure (Left "--stack-frames must be a positive integer")
                        else do
                            mLogFile <- traverse expandAbsPath (nonEmptyJust (ovLogFile ov))
                            mDecLog <- traverse expandAbsPath (nonEmptyJust (ovDecisionLog ov) <|> envDecLog)
                            mWorkdir <- case envWorkdir of
                                Just d -> do
                                    isDir <- doesDirectoryExist d
                                    pure (if isDir then Just d else Nothing)
                                Nothing -> pure Nothing
                            pure $
                                Right
                                    OracleOptions
                                        { ooDbms = dbmsKind
                                        , ooConnection = ovConnection ov
                                        , ooExpectedSignature = expSig
                                        , ooBaselineSignature = baseSig
                                        , ooRejectOnSlippage = ovRejectOnSlippage ov || envReject || odRejectOnSlippage defs
                                        , ooStackFrames = stackFrames
                                        , ooPrintSignature = ovPrintSignature ov
                                        , ooLogFile = mLogFile
                                        , ooContainerLog = nonEmptyJust (ovContainerLog ov)
                                        , ooDecisionLog = mDecLog
                                        , ooTimeoutSeconds = timeoutSecs
                                        , ooAllowStderrOnly = ovAllowStderrOnly ov || envAllowStderr || odAllowStderrOnly defs
                                        , ooAllowPsqlMeta = ovAllowPsqlMeta ov || envAllowMeta || odAllowPsqlMeta defs
                                        , ooVerbose = ovVerbose ov || envVerbose || odVerbose defs
                                        , ooWorkdir = mWorkdir
                                        }
    where
        nonEmptyStr (Just s) | not (null s) = Just s
        nonEmptyStr _ = Nothing

        nonEmptyJust (Just s) | not (null s) = Just s
        nonEmptyJust _ = Nothing

        envFlag name = do
            mv <- lookupEnv name
            pure $ maybe False (\v -> map toLower (trimStr v) `elem` ["1", "true", "yes", "on"]) mv

        envInt name = do
            mv <- lookupEnv name
            pure (mv >>= readMaybe)

        envDouble name = do
            mv <- lookupEnv name
            pure (mv >>= readMaybe)

        trimStr = T.unpack . T.strip . T.pack

expandAbsPath :: FilePath -> IO FilePath
expandAbsPath ('~' : '/' : rest) = do
    home <- getHomeDirectory
    makeAbsolute (home </> rest)
expandAbsPath path = makeAbsolute path

data LogMode
    = ModeEmbeddedLocal
    | ModeEmbeddedContainer !Text
    | ModeFile !FilePath
    | ModeContainer !Text !FilePath
    | ModeStderr
    deriving (Eq, Show)

logModeLabel :: LogMode -> Text
logModeLabel = \case
    ModeEmbeddedLocal -> "embedded_local"
    ModeEmbeddedContainer _ -> "embedded_container"
    ModeFile _ -> "file"
    ModeContainer _ _ -> "container"
    ModeStderr -> "stderr"

-- | Run one SQL candidate through the DBMS crash oracle and optionally append to @--decision-log@.
probeCrashOracle :: HarnessConfig -> OracleOptions -> BS.ByteString -> IO OracleResult
probeCrashOracle cfg opts candidateSql =
    snd <$> probeCrashOracleCancellable cfg Nothing opts candidateSql

-- | Interruptible variant of 'probeCrashOracle' supporting an @IORef Bool@ stop flag.
probeCrashOracleCancellable ::
    HarnessConfig ->
    Maybe (IORef Bool) ->
    OracleOptions ->
    BS.ByteString ->
    IO (Bool, OracleResult)
probeCrashOracleCancellable cfg mStopRef opts candidateSql = do
    now <- getCurrentTime
    let tsText = formatIsoUtc now
        sha16 = TE.decodeUtf8 (BS.take 16 (Base16.encode (SHA256.hash candidateSql)))
        baseDec =
            OracleDecision
                { odTimestamp = tsText
                , odDbms = ooDbms opts
                , odCandidateSha16 = sha16
                , odCandidateBytes = BS.length candidateSql
                , odExpectedSignature = ooExpectedSignature opts
                , odBaselineSignature = nonEmptyText (ooBaselineSignature opts)
                , odWorkdir = ooWorkdir opts
                , odVerdict = VerdictRejected
                , odClassification = ClassHealthyNoFault
                , odReason = "uninitialized"
                , odLogMode = Nothing
                , odLogFile = Nothing
                , odRetcode = Nothing
                , odConnectionLost = Nothing
                , odRegexValid = Nothing
                , odTimeoutSeconds = Nothing
                , odIndicators = Nothing
                , odFaultEvents = Nothing
                , odCorroboration = Nothing
                , odCrashSignature = Nothing
                , odSlippage = Nothing
                , odHint = Nothing
                }
        sqlTxt = TE.decodeUtf8With TEE.lenientDecode candidateSql
    if T.null (T.strip sqlTxt)
        then
            finish
                1
                ( baseDec
                    { odVerdict = VerdictRejected
                    , odClassification = ClassHealthyNoFault
                    , odReason = "empty-sql"
                    }
                )
        else
            if ooDbms opts == DbmsPostgres && not (ooAllowPsqlMeta opts) && hasPsqlMetaCommand sqlTxt
                then
                    finish
                        1
                        ( baseDec
                            { odVerdict = VerdictRejected
                            , odClassification = ClassHealthyNoFault
                            , odReason = "psql-meta-command"
                            }
                        )
                else runWithSource baseDec
    where
        emitAndReturn interrupted code dec = do
            emitDecisionLog (ooDecisionLog opts) dec
            whenVerbose (ooVerbose opts) dec
            pure (interrupted, OracleResult code dec)

        finish = emitAndReturn False
        finishInterrupted = emitAndReturn True 130

        runWithSource baseDec = do
            conn <- resolveDbmsConnection cfg (ooDbms opts) (ooConnection opts)
            let internalLog = fromMaybe (connContainerLog conn) (ooContainerLog opts)
            sourceOrErr <- acquireLogSource opts conn internalLog
            case sourceOrErr of
                Left errDec -> finish 1 (errDec baseDec)
                Right (mode, initOffset, initInode, sinceIso) ->
                    executeAndClassify conn mode internalLog initOffset initInode sinceIso baseDec

        executeAndClassify conn mode internalLog initOffset initInode sinceIso baseDec = do
            let modeLabel = logModeLabel mode
                mTimeout =
                    if ooTimeoutSeconds opts > 0
                        then Just (ooTimeoutSeconds opts)
                        else Nothing
            cmdEither <- buildDbmsCommand conn RoleProbe =<< currentEnvMap
            case cmdEither of
                Left err ->
                    finish
                        1
                        ( baseDec
                            { odVerdict = VerdictRejected
                            , odClassification = ClassMissingEvidence
                            , odReason = "execution-error"
                            , odHint = Just (T.pack err)
                            , odLogMode = Just modeLabel
                            }
                        )
                Right (cmd, args, childEnv) -> do
                    res <- tryExec cmd args childEnv mTimeout
                    case res of
                        Left exc ->
                            finish
                                1
                                ( baseDec
                                    { odVerdict = VerdictRejected
                                    , odClassification = ClassMissingEvidence
                                    , odReason = "execution-error"
                                    , odHint = Just (T.pack (show exc))
                                    , odLogMode = Just modeLabel
                                    }
                                )
                        Right (True, _, _, _, _) ->
                            finishInterrupted
                                ( baseDec
                                    { odVerdict = VerdictRejected
                                    , odClassification = ClassMissingEvidence
                                    , odReason = "interrupted"
                                    , odLogMode = Just modeLabel
                                    }
                                )
                        Right (False, True, _, _, _) ->
                            finish
                                124
                                ( baseDec
                                    { odVerdict = VerdictTimeout
                                    , odClassification = ClassMissingEvidence
                                    , odReason = "client-timeout"
                                    , odTimeoutSeconds = Just (ooTimeoutSeconds opts)
                                    , odLogMode = Just modeLabel
                                    }
                                )
                        Right (False, False, retcode, _, stderrBs) -> do
                            let stderrTxt = TE.decodeUtf8With TEE.lenientDecode stderrBs
                            case mode of
                                ModeEmbeddedContainer _
                                    | retcode `elem` [125, 126, 127] ->
                                        finish
                                            1
                                            ( baseDec
                                                { odVerdict = VerdictRejected
                                                , odClassification = ClassMissingEvidence
                                                , odReason = "container-exec-failed"
                                                , odRetcode = Just retcode
                                                , odLogMode = Just modeLabel
                                                , odHint = Just (T.take 512 stderrTxt)
                                                }
                                            )
                                _ ->
                                    evaluateProbeOutcome
                                        opts
                                        mode
                                        modeLabel
                                        internalLog
                                        initOffset
                                        initInode
                                        sinceIso
                                        retcode
                                        stderrTxt
                                        baseDec
                                        finish

        tryExec ::
            FilePath ->
            [String] ->
            [(String, String)] ->
            Maybe Double ->
            IO (Either IOException (Bool, Bool, Int, BS.ByteString, BS.ByteString))
        tryExec cmd args childEnv mTimeout = do
            stopped <- maybe (pure False) readIORef mStopRef
            if stopped
                then pure (Right (True, False, 130, BS.empty, BS.empty))
                else
                    try
                        ( runProcessCaptureCancellable
                            (isJust mStopRef)
                            mStopRef
                            (ooWorkdir opts)
                            (Just childEnv)
                            mTimeout
                            cmd
                            args
                            candidateSql
                        )

evaluateProbeOutcome ::
    OracleOptions ->
    LogMode ->
    Text ->
    FilePath ->
    Integer ->
    Maybe Integer ->
    Text ->
    Int ->
    Text ->
    OracleDecision ->
    (Int -> OracleDecision -> IO (Bool, OracleResult)) ->
    IO (Bool, OracleResult)
evaluateProbeOutcome opts mode modeLabel internalLog initOffset initInode sinceIso retcode stderrTxt baseDec finish = do
    let dbms = ooDbms opts
        connLost = any (`T.isInfixOf` stderrTxt) (connectionLostPhrasesForDbms dbms)
        possibleCrash = connLost || retcode /= 0
        isEmbedded = not (isServerDbmsKind dbms)
    case mode of
        ModeStderr -> evaluateStderrMode dbms connLost retcode stderrTxt
        _ -> do
            (logReadOk, serverLogTxt) <- case mode of
                ModeFile path -> pollLogs possibleCrash (pollFileReader path)
                ModeContainer cname _ -> pollLogs possibleCrash (pollContainerReader cname internalLog sinceIso)
                _ -> pure (True, "")
            if isFileOrContainer mode && not logReadOk && T.null serverLogTxt
                then
                    finish
                        1
                        ( baseDec
                            { odVerdict = VerdictRejected
                            , odClassification = ClassMissingEvidence
                            , odReason = "log-read-failed"
                            , odLogMode = Just modeLabel
                            , odLogFile = modeLogFilePath mode
                            }
                        )
                else classifyFullEvidence dbms isEmbedded connLost retcode serverLogTxt stderrTxt
    where
        expSig = ooExpectedSignature opts
        stackN = ooStackFrames opts

        isFileOrContainer (ModeFile _) = True
        isFileOrContainer (ModeContainer _ _) = True
        isFileOrContainer _ = False

        modeLogFilePath (ModeFile p) = Just p
        modeLogFilePath (ModeContainer _ p) = Just p
        modeLogFilePath _ = Nothing

        pollLogs possibleCrash stepReader = do
            now <- getCurrentTime
            let waitSecs = if possibleCrash then logWaitSeconds else 0.0
                go curOff curInode accFile = do
                    (ok, nxtOff, nxtInode, combinedTxt, nextFile) <- stepReader curOff curInode accFile
                    if not ok
                        then pure (False, combinedTxt)
                        else do
                            let ev = extractDbmsEvidence (ooDbms opts) combinedTxt stderrTxt retcode
                                faults = erFaults ev
                                cands = erCandidateLines ev
                                doneMatch
                                    | T.null expSig = not (null faults)
                                    | otherwise =
                                        not (null faults)
                                            && fst (matchExpectedSignature expSig (T.unlines (map feDetail faults <> cands)))
                            tNow <- getCurrentTime
                            if doneMatch || realToFrac (diffUTCTime tNow now) >= waitSecs
                                then pure (True, combinedTxt)
                                else do
                                    threadDelay logPollIntervalMicros
                                    go nxtOff nxtInode nextFile
            go initOffset initInode ""

        evaluateStderrMode dbms connLost rc errTxt = do
            let ev = extractDbmsEvidence dbms errTxt errTxt rc
                faults = erFaults ev
                cands = erCandidateLines ev
                mSig = buildCrashSignature dbms faults cands rc stackN
            if not (T.null expSig)
                then
                    let (matched, rValid) = matchExpectedSignature expSig errTxt
                     in if matched
                            then
                                finish
                                    0
                                    ( baseDec
                                        { odVerdict = VerdictAccepted
                                        , odClassification = ClassMissingEvidence
                                        , odReason = "stderr-signature-unsafe"
                                        , odRegexValid = Just rValid
                                        , odCrashSignature = mSig
                                        , odLogMode = Just modeLabel
                                        }
                                    )
                            else
                                finish
                                    1
                                    ( baseDec
                                        { odVerdict = VerdictRejected
                                        , odClassification = ClassMissingEvidence
                                        , odReason = "stderr-signature-mismatch"
                                        , odRegexValid = Just rValid
                                        , odCrashSignature = mSig
                                        , odLogMode = Just modeLabel
                                        }
                                    )
                else
                    if connLost || not (null faults)
                        then
                            finish
                                0
                                ( baseDec
                                    { odVerdict = VerdictAccepted
                                    , odClassification = ClassMissingEvidence
                                    , odReason = "stderr-crash-unsafe"
                                    , odFaultEvents = Just faults
                                    , odConnectionLost = Just connLost
                                    , odCrashSignature = mSig
                                    , odLogMode = Just modeLabel
                                    }
                                )
                        else
                            finish
                                1
                                ( baseDec
                                    { odVerdict = VerdictRejected
                                    , odClassification = ClassMissingEvidence
                                    , odReason = "no-crash"
                                    , odConnectionLost = Just connLost
                                    , odLogMode = Just modeLabel
                                    }
                                )

        classifyFullEvidence dbms isEmbedded connLost rc srvTxt errTxt = do
            let ev = extractDbmsEvidence dbms srvTxt errTxt rc
                faults = erFaults ev
                cands = erCandidateLines ev
                corr = erCorroboration ev
                indicators = map feDetail faults
                mSig = buildCrashSignature dbms faults cands rc stackN
            mBaseline <-
                if T.null (T.strip (ooBaselineSignature opts))
                    then pure Nothing
                    else loadBaselineRef (ooBaselineSignature opts)
            let mSlip = evaluateSlippage <$> mBaseline <*> mSig
                slippedReject = ooRejectOnSlippage opts && maybe False svSlipped mSlip
                noCrashReason
                    | corOom corr =
                        if corOomCause corr == Just "oom_kill_signal_9"
                            then "oom-kill"
                            else "oom"
                    | corSqliteAbort corr = "sqlite-abort"
                    | corExternalKill corr = "external-kill"
                    | otherwise = "no-crash"
                (sigMatched, regexOk)
                    | T.null expSig = (True, Nothing)
                    | null faults =
                        let (_, rValid) = matchExpectedSignature expSig ""
                         in (False, Just rValid)
                    | otherwise =
                        let scopeTxt = T.intercalate "\n" (indicators <> cands)
                            (matched, rValid) = matchExpectedSignature expSig scopeTxt
                         in (matched, Just rValid)
                withEvidence dec =
                    dec
                        { odIndicators = Just indicators
                        , odFaultEvents = Just faults
                        , odCorroboration = Just corr
                        , odConnectionLost = Just connLost
                        , odRetcode = Just rc
                        , odRegexValid = regexOk
                        , odCrashSignature = mSig
                        , odSlippage = mSlip
                        , odLogMode = Just modeLabel
                        }
            if not (T.null expSig)
                then
                    if not (not (null faults) && sigMatched)
                        then
                            finish
                                1
                                ( withEvidence
                                    ( baseDec
                                        { odVerdict = VerdictRejected
                                        , odClassification = ClassHealthyNoFault
                                        , odReason = if null faults then noCrashReason else "signature-mismatch"
                                        }
                                    )
                                )
                        else
                            if slippedReject
                                then
                                    finish
                                        1
                                        ( withEvidence
                                            ( baseDec
                                                { odVerdict = VerdictRejected
                                                , odClassification = ClassConfirmedFault
                                                , odReason = "signature-slippage"
                                                }
                                            )
                                        )
                                else
                                    finish
                                        0
                                        ( withEvidence
                                            ( baseDec
                                                { odVerdict = VerdictAccepted
                                                , odClassification = ClassConfirmedFault
                                                , odReason = "crash-signature"
                                                }
                                            )
                                        )
                else
                    if not (null faults)
                        then
                            if slippedReject
                                then
                                    finish
                                        1
                                        ( withEvidence
                                            ( baseDec
                                                { odVerdict = VerdictRejected
                                                , odClassification = ClassConfirmedFault
                                                , odReason = "signature-slippage"
                                                }
                                            )
                                        )
                                else
                                    finish
                                        0
                                        ( withEvidence
                                            ( baseDec
                                                { odVerdict = VerdictAccepted
                                                , odClassification = ClassConfirmedFault
                                                , odReason = "crash-indicator"
                                                }
                                            )
                                        )
                        else
                            if connLost && ooAllowStderrOnly opts && not isEmbedded
                                then
                                    finish
                                        0
                                        ( baseDec
                                            { odVerdict = VerdictAccepted
                                            , odClassification = ClassMissingEvidence
                                            , odReason = "connection-loss-stderr-unsafe"
                                            , odConnectionLost = Just True
                                            , odRetcode = Just rc
                                            , odLogMode = Just modeLabel
                                            }
                                        )
                                else
                                    finish
                                        1
                                        ( baseDec
                                            { odVerdict = VerdictRejected
                                            , odClassification = ClassHealthyNoFault
                                            , odReason = noCrashReason
                                            , odCorroboration = Just corr
                                            , odConnectionLost = Just connLost
                                            , odRetcode = Just rc
                                            , odLogMode = Just modeLabel
                                            }
                                        )

acquireLogSource ::
    OracleOptions ->
    DbmsConnection ->
    FilePath ->
    IO (Either (OracleDecision -> OracleDecision) (LogMode, Integer, Maybe Integer, Text))
acquireLogSource opts conn internalLog = do
    sinceIso <- formatIsoUtc <$> getCurrentTime
    if not (isServerDbmsKind (ooDbms opts))
        then pure $ case connContainer conn of
            Just cname -> Right (ModeEmbeddedContainer cname, 0, Nothing, sinceIso)
            Nothing -> Right (ModeEmbeddedLocal, 0, Nothing, sinceIso)
        else case (ooLogFile opts, connContainer conn) of
            (Just path, mContainer) -> do
                readable <- waitForReadableFile path logWaitSeconds
                if readable
                    then do
                        stRes <- try (getFileStatus path) :: IO (Either IOException FileStatus)
                        case stRes of
                            Right st ->
                                pure (Right (ModeFile path, fromIntegral (fileSize st), Just (fromIntegral (fileID st)), sinceIso))
                            Left _ ->
                                pure $
                                    Left \d ->
                                        d
                                            { odVerdict = VerdictRejected
                                            , odClassification = ClassMissingEvidence
                                            , odReason = "log-stat-failed"
                                            , odLogFile = Just path
                                            }
                    else case mContainer of
                        Just cname -> acquireContainer cname sinceIso
                        Nothing ->
                            pure $
                                Left \d ->
                                    d
                                        { odVerdict = VerdictRejected
                                        , odClassification = ClassMissingEvidence
                                        , odReason = "log-unreadable"
                                        , odLogFile = Just path
                                        , odHint = Just "mount the log readably or pass --container-name"
                                        }
            (Nothing, Just cname) -> acquireContainer cname sinceIso
            (Nothing, Nothing)
                | ooAllowStderrOnly opts -> pure (Right (ModeStderr, 0, Nothing, sinceIso))
                | otherwise ->
                    pure $
                        Left \d ->
                            d
                                { odVerdict = VerdictRejected
                                , odClassification = ClassMissingEvidence
                                , odReason = "log-source-required"
                                , odHint = Just "pass --log-file or --container-name (or --allow-stderr-only)"
                                }
    where
        acquireContainer cname sinceIso = do
            mState <- getContainerLogState cname internalLog
            case mState of
                Just (sz, ino) -> pure (Right (ModeContainer cname internalLog, sz, Just ino, sinceIso))
                Nothing ->
                    pure $
                        Left \d ->
                            d
                                { odVerdict = VerdictRejected
                                , odClassification = ClassMissingEvidence
                                , odReason = "container-log-unavailable"
                                , odLogFile = Just internalLog
                                }

waitForReadableFile :: FilePath -> Double -> IO Bool
waitForReadableFile path waitSecs = do
    start <- getCurrentTime
    go start
    where
        go start = do
            res <- try (withBinaryFile path ReadMode (const (pure ()))) :: IO (Either IOException ())
            case res of
                Right () -> pure True
                Left _ -> do
                    now <- getCurrentTime
                    if realToFrac (diffUTCTime now start) >= waitSecs
                        then pure False
                        else do
                            threadDelay logPollIntervalMicros
                            go start

pollFileReader ::
    FilePath -> Integer -> Maybe Integer -> Text -> IO (Bool, Integer, Maybe Integer, Text, Text)
pollFileReader path curOff curInode accFile = do
    res <- try (getFileStatus path) :: IO (Either IOException FileStatus)
    case res of
        Left _ -> pure (False, curOff, curInode, accFile, accFile)
        Right st -> do
            let curSize = fromIntegral (fileSize st)
                inodeNow = fromIntegral (fileID st)
                offset0 = if maybe False (/= inodeNow) curInode || curSize < curOff then 0 else curOff
            if curSize <= offset0
                then pure (True, offset0, Just inodeNow, accFile, accFile)
                else do
                    let start = max offset0 (curSize - maxLogReadBytes)
                        toRead = fromIntegral (curSize - start)
                    readRes <-
                        try
                            ( withBinaryFile path ReadMode \h -> do
                                hSeek h AbsoluteSeek start
                                bytes <- BS.hGet h toRead
                                pure (TE.decodeUtf8With TEE.lenientDecode bytes)
                            ) ::
                            IO (Either IOException Text)
                    case readRes of
                        Left _ -> pure (False, offset0, Just inodeNow, accFile, accFile)
                        Right chunk ->
                            let acc' = tailText maxTotalLogChars (accFile <> chunk)
                             in pure (True, curSize, Just inodeNow, acc', acc')

getContainerLogState :: Text -> FilePath -> IO (Maybe (Integer, Integer))
getContainerLogState cname internalLog = do
    res <-
        try
            ( runProcessCapture
                Nothing
                Nothing
                (Just 3.0)
                "docker"
                ["exec", T.unpack cname, "stat", "-c", "%s %i", internalLog]
                BS.empty
            ) ::
            IO (Either IOException (Bool, Int, BS.ByteString, BS.ByteString))
    pure $ case res of
        Right (False, 0, outBs, _) ->
            case words (T.unpack (TE.decodeUtf8With TEE.lenientDecode outBs)) of
                [szStr, inoStr] -> (,) <$> readMaybe szStr <*> readMaybe inoStr
                _ -> Nothing
        _ -> Nothing

pollContainerReader ::
    Text ->
    FilePath ->
    Text ->
    Integer ->
    Maybe Integer ->
    Text ->
    IO (Bool, Integer, Maybe Integer, Text, Text)
pollContainerReader cname internalLog sinceIso curOff curInode accFile = do
    mState <- getContainerLogState cname internalLog
    case mState of
        Nothing -> pure (False, curOff, curInode, accFile, accFile)
        Just (curSize, inodeNow) -> do
            let offset0 = if maybe False (/= inodeNow) curInode || curSize < curOff then 0 else curOff
            (okFile, nextFile) <-
                if curSize <= offset0
                    then pure (True, accFile)
                    else do
                        let start = max offset0 (curSize - maxLogReadBytes)
                        res <-
                            try
                                ( runProcessCapture
                                    Nothing
                                    Nothing
                                    (Just 5.0)
                                    "docker"
                                    ["exec", T.unpack cname, "tail", "-c", "+" <> show (start + 1), internalLog]
                                    BS.empty
                                ) ::
                                IO (Either IOException (Bool, Int, BS.ByteString, BS.ByteString))
                        pure $ case res of
                            Right (False, 0, outBs, _) ->
                                (True, tailText maxTotalLogChars (accFile <> TE.decodeUtf8With TEE.lenientDecode outBs))
                            _ -> (False, accFile)
            stdoutLog <- readContainerStdoutLogs cname sinceIso
            let combined =
                    tailText maxTotalLogChars $
                        if T.null stdoutLog
                            then nextFile
                            else if T.null nextFile then stdoutLog else nextFile <> "\n" <> stdoutLog
            pure (okFile, curSize, Just inodeNow, combined, nextFile)

readContainerStdoutLogs :: Text -> Text -> IO Text
readContainerStdoutLogs cname sinceIso = do
    res <-
        try
            ( runProcessCapture
                Nothing
                Nothing
                (Just 5.0)
                "docker"
                ["logs", "--since", T.unpack sinceIso, T.unpack cname]
                BS.empty
            ) ::
            IO (Either IOException (Bool, Int, BS.ByteString, BS.ByteString))
    pure $ case res of
        Right (_, _, outBs, errBs) ->
            let outTxt = TE.decodeUtf8With TEE.lenientDecode outBs
                errTxt = TE.decodeUtf8With TEE.lenientDecode errBs
             in outTxt <> "\n" <> errTxt
        Left _ -> ""

tailText :: Int -> Text -> Text
tailText limit txt
    | T.length txt <= limit = txt
    | otherwise = T.takeEnd limit txt

currentEnvMap :: IO (Map String String)
currentEnvMap = Map.fromList <$> getEnvironment

emitDecisionLog :: Maybe FilePath -> OracleDecision -> IO ()
emitDecisionLog Nothing _ = pure ()
emitDecisionLog (Just path) dec = do
    let line = BSL.toStrict (encode dec) <> BS.singleton 10
    res <- try (BS.appendFile path line) :: IO (Either IOException ())
    case res of
        Right () -> pure ()
        Left exc ->
            hPutStrLn stderr ("[occam-harness oracle] Warning: failed to write " <> path <> ": " <> show exc)

whenVerbose :: Bool -> OracleDecision -> IO ()
whenVerbose False _ = pure ()
whenVerbose True dec =
    hPutStrLn
        stderr
        ( "[occam-harness oracle] "
            <> T.unpack (T.toUpper (verdictText (odVerdict dec)))
            <> " ["
            <> T.unpack (classificationText (odClassification dec))
            <> "]: "
            <> T.unpack (odReason dec)
        )

formatIsoUtc :: UTCTime -> Text
formatIsoUtc = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+00:00"

nonEmptyText :: Text -> Maybe Text
nonEmptyText t = if T.null t then Nothing else Just t

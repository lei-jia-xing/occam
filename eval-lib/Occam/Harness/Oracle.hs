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
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), runContT)
import Control.Monad.Trans.Except (runExceptT, throwE)
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

-- | Default 'OracleOptions' with 5 stack frames, zero timeout, and strict evidence checks.
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

-- | Empty 'OracleOverrides' leaving all configuration fields unspecified.
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

-- | Resolve 'OracleOptions' from CLI overrides, environment variables, and Dhall defaults.
resolveOracleOptions :: HarnessConfig -> OracleOverrides -> IO (Either String OracleOptions)
resolveOracleOptions cfg ov = runExceptT do
    let defs = hcOracle cfg
    envDbms <- liftIO (lookupEnv "DBMS")
    dbmsKind <- case ovDbms ov <|> (envDbms >>= parseDbmsKind . T.pack) of
        Just d -> pure d
        Nothing -> case envDbms of
            Just raw | not (null raw) -> throwE ("Unsupported DBMS in DBMS env: " <> raw)
            _ -> pure DbmsPostgres
    expSig <-
        liftIO $ maybe (maybe "" T.pack <$> lookupEnv "EXPECTED_SIGNATURE") pure (ovExpectedSignature ov)
    baseSig <-
        liftIO $ maybe (maybe "" T.pack <$> lookupEnv "BASELINE_SIGNATURE") pure (ovBaselineSignature ov)
    envReject <- liftIO (envFlag "ORACLE_REJECT_ON_SLIPPAGE")
    envFrames <- liftIO (envRead "ORACLE_STACK_FRAMES")
    envTimeout <- liftIO (envRead "ORACLE_TIMEOUT")
    envAllowStderr <- liftIO (envFlag "ORACLE_ALLOW_STDERR_ONLY")
    envAllowMeta <- liftIO (envFlag "ORACLE_ALLOW_PSQL_META")
    envVerbose <- liftIO (envFlag "ORACLE_VERBOSE")
    envDecLog <- nonEmptyStr <$> liftIO (lookupEnv "ORACLE_DECISION_LOG")
    envWorkdir <- nonEmptyStr <$> liftIO (lookupEnv "OCCAM_WORKDIR")

    let stackFrames = fromMaybe (odStackFrames defs) (ovStackFrames ov <|> envFrames)
        timeoutSecs = fromMaybe (odTimeoutSecs defs) (ovTimeoutSeconds ov <|> envTimeout)
    when (isNaN timeoutSecs || isInfinite timeoutSecs || timeoutSecs < 0) $
        throwE "--timeout must be a finite non-negative number"
    when (stackFrames <= 0) $
        throwE "--stack-frames must be a positive integer"

    mLogFile <- liftIO $ traverse expandAbsPath (nonEmptyStr (ovLogFile ov))
    mDecLog <- liftIO $ traverse expandAbsPath (nonEmptyStr (ovDecisionLog ov) <|> envDecLog)
    mWorkdir <- liftIO case envWorkdir of
        Nothing -> pure Nothing
        Just d -> do
            isDir <- doesDirectoryExist d
            pure (d <$ guard isDir)
    pure
        OracleOptions
            { ooDbms = dbmsKind
            , ooConnection = ovConnection ov
            , ooExpectedSignature = expSig
            , ooBaselineSignature = baseSig
            , ooRejectOnSlippage = ovRejectOnSlippage ov || envReject || odRejectOnSlippage defs
            , ooStackFrames = stackFrames
            , ooPrintSignature = ovPrintSignature ov
            , ooLogFile = mLogFile
            , ooContainerLog = nonEmptyStr (ovContainerLog ov)
            , ooDecisionLog = mDecLog
            , ooTimeoutSeconds = timeoutSecs
            , ooAllowStderrOnly = ovAllowStderrOnly ov || envAllowStderr || odAllowStderrOnly defs
            , ooAllowPsqlMeta = ovAllowPsqlMeta ov || envAllowMeta || odAllowPsqlMeta defs
            , ooVerbose = ovVerbose ov || envVerbose || odVerbose defs
            , ooWorkdir = mWorkdir
            }
    where
        nonEmptyStr m = m >>= \s -> s <$ guard (not (null s))

        envFlag name = do
            mv <- lookupEnv name
            pure $ maybe False (\v -> map toLower (trimStr v) `elem` ["1", "true", "yes", "on"]) mv

        envRead :: (Read a) => String -> IO (Maybe a)
        envRead name = (>>= readMaybe) <$> lookupEnv name

        trimStr = T.unpack . T.strip . T.pack

expandAbsPath :: FilePath -> IO FilePath
expandAbsPath ('~' : '/' : rest) = do
    home <- getHomeDirectory
    makeAbsolute (home </> rest)
expandAbsPath path = makeAbsolute path

-- | Active server/container log observation transport for an oracle probe.
data LogMode
    = ModeEmbeddedLocal
    | ModeEmbeddedContainer !Text
    | ModeFile !FilePath
    | ModeContainer !Text !FilePath
    | ModeStderr
    deriving (Eq, Show)

-- | Machine-readable label recorded in @OracleDecision.odLogMode@.
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

-- | Internal continuation outcome tuple: @(wasInterrupted, exitCode, decision)@.
type ProbeOutcome = (Bool, Int, OracleDecision)

-- | Continuation monad allowing early probe termination without manual CPS threading.
type ProbeM = ContT ProbeOutcome IO

exitProbe :: Bool -> Int -> OracleDecision -> ProbeM a
exitProbe interrupted code dec = ContT \_ -> pure (interrupted, code, dec)

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
        mTimeout = ooTimeoutSeconds opts <$ guard (ooTimeoutSeconds opts > 0)

    (interrupted, code, dec) <- (`runContT` pure) do
        when (T.null (T.strip sqlTxt)) $
            exitProbe False 1 baseDec {odReason = "empty-sql"}
        when (ooDbms opts == DbmsPostgres && not (ooAllowPsqlMeta opts) && hasPsqlMetaCommand sqlTxt) $
            exitProbe False 1 baseDec {odReason = "psql-meta-command"}

        conn <- liftIO (resolveDbmsConnection cfg (ooDbms opts) (ooConnection opts))
        let internalLog = fromMaybe (connContainerLog conn) (ooContainerLog opts)
        (mode, initOffset, initInode, sinceIso) <-
            liftIO (acquireLogSource opts conn internalLog)
                >>= either (\f -> exitProbe False 1 (f baseDec)) pure

        let modeLabel = logModeLabel mode
            execError :: Text -> ProbeM a
            execError decHint =
                exitProbe
                    False
                    1
                    baseDec
                        { odVerdict = VerdictRejected
                        , odClassification = ClassMissingEvidence
                        , odReason = "execution-error"
                        , odHint = Just decHint
                        , odLogMode = Just modeLabel
                        }
        (cmd, args, childEnv) <-
            liftIO (buildDbmsCommand conn RoleProbe =<< currentEnvMap) >>= either (execError . T.pack) pure
        execRes <-
            liftIO (tryExec cmd args childEnv mTimeout) >>= either (execError . T.pack . show) pure
        case execRes of
            (True, _, _, _, _) ->
                exitProbe
                    True
                    130
                    baseDec
                        { odVerdict = VerdictRejected
                        , odClassification = ClassMissingEvidence
                        , odReason = "interrupted"
                        , odLogMode = Just modeLabel
                        }
            (False, True, _, _, _) ->
                exitProbe
                    False
                    124
                    baseDec
                        { odVerdict = VerdictTimeout
                        , odClassification = ClassMissingEvidence
                        , odReason = "client-timeout"
                        , odTimeoutSeconds = Just (ooTimeoutSeconds opts)
                        , odLogMode = Just modeLabel
                        }
            (False, False, retcode, _, stderrBs) -> do
                let stderrTxt = TE.decodeUtf8With TEE.lenientDecode stderrBs
                case mode of
                    ModeEmbeddedContainer _
                        | retcode `elem` [125, 126, 127] ->
                            exitProbe
                                False
                                1
                                baseDec
                                    { odVerdict = VerdictRejected
                                    , odClassification = ClassMissingEvidence
                                    , odReason = "container-exec-failed"
                                    , odRetcode = Just retcode
                                    , odLogMode = Just modeLabel
                                    , odHint = Just (T.take 512 stderrTxt)
                                    }
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

    emitDecisionLog (ooDecisionLog opts) dec
    whenVerbose (ooVerbose opts) dec
    pure (interrupted, OracleResult code dec)
    where
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
    ProbeM ProbeOutcome
evaluateProbeOutcome opts mode modeLabel internalLog initOffset initInode sinceIso retcode stderrTxt baseDec =
    case mode of
        ModeStderr -> pure $ evaluateStderrMode dbms connLost retcode stderrTxt
        _ -> do
            (logReadOk, serverLogTxt) <- liftIO case mode of
                ModeFile path -> pollLogs possibleCrash (pollFileReader path)
                ModeContainer cname _ -> pollLogs possibleCrash (pollContainerReader cname internalLog sinceIso)
                _ -> pure (True, "")
            when (isFileOrContainer mode && not logReadOk && T.null serverLogTxt) $
                exitProbe
                    False
                    1
                    baseDec
                        { odVerdict = VerdictRejected
                        , odClassification = ClassMissingEvidence
                        , odReason = "log-read-failed"
                        , odLogMode = Just modeLabel
                        , odLogFile = modeLogFilePath mode
                        }
            classifyFullEvidence dbms isEmbedded connLost retcode serverLogTxt stderrTxt
    where
        dbms = ooDbms opts
        connLost = any (`T.isInfixOf` stderrTxt) (connectionLostPhrasesForDbms dbms)
        possibleCrash = connLost || retcode /= 0
        isEmbedded = not (isServerDbmsKind dbms)
        expSig = ooExpectedSignature opts
        stackN = ooStackFrames opts

        isFileOrContainer (ModeFile _) = True
        isFileOrContainer (ModeContainer _ _) = True
        isFileOrContainer _ = False

        modeLogFilePath (ModeFile p) = Just p
        modeLogFilePath (ModeContainer _ p) = Just p
        modeLogFilePath _ = Nothing

        pollLogs ::
            Bool ->
            (Integer -> Maybe Integer -> Text -> IO (Bool, Integer, Maybe Integer, Text, Text)) ->
            IO (Bool, Text)
        pollLogs maybeCrash stepReader = do
            now <- getCurrentTime
            let waitSecs = if maybeCrash then logWaitSeconds else 0.0
                go curOff curInode accFile = do
                    (ok, nxtOff, nxtInode, combinedTxt, nextFile) <- stepReader curOff curInode accFile
                    if not ok
                        then pure (False, combinedTxt)
                        else do
                            let ev = extractDbmsEvidence dbms combinedTxt stderrTxt retcode
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

        evaluateStderrMode targetDbms hasConnLost rc errTxt
            | not (T.null expSig) =
                let (matched, rValid) = matchExpectedSignature expSig errTxt
                 in ( False
                    , if matched then 0 else 1
                    , baseDec
                        { odVerdict = if matched then VerdictAccepted else VerdictRejected
                        , odClassification = ClassMissingEvidence
                        , odReason = if matched then "stderr-signature-unsafe" else "stderr-signature-mismatch"
                        , odRegexValid = Just rValid
                        , odCrashSignature = mSig
                        , odLogMode = Just modeLabel
                        }
                    )
            | hasConnLost || not (null faults) =
                ( False
                , 0
                , baseDec
                    { odVerdict = VerdictAccepted
                    , odClassification = ClassMissingEvidence
                    , odReason = "stderr-crash-unsafe"
                    , odFaultEvents = Just faults
                    , odConnectionLost = Just hasConnLost
                    , odCrashSignature = mSig
                    , odLogMode = Just modeLabel
                    }
                )
            | otherwise =
                ( False
                , 1
                , baseDec
                    { odVerdict = VerdictRejected
                    , odClassification = ClassMissingEvidence
                    , odReason = "no-crash"
                    , odConnectionLost = Just hasConnLost
                    , odLogMode = Just modeLabel
                    }
                )
            where
                ev = extractDbmsEvidence targetDbms errTxt errTxt rc
                faults = erFaults ev
                cands = erCandidateLines ev
                mSig = buildCrashSignature targetDbms faults cands rc stackN

        classifyFullEvidence targetDbms embedded hasConnLost rc srvTxt errTxt = do
            let ev = extractDbmsEvidence targetDbms srvTxt errTxt rc
                faults = erFaults ev
                cands = erCandidateLines ev
                corr = erCorroboration ev
                indicators = map feDetail faults
                mSig = buildCrashSignature targetDbms faults cands rc stackN
            mBaseline <-
                liftIO
                    if T.null (T.strip (ooBaselineSignature opts))
                        then pure Nothing
                        else loadBaselineRef (ooBaselineSignature opts)
            let mSlip = evaluateSlippage <$> mBaseline <*> mSig
                slippedReject = ooRejectOnSlippage opts && maybe False svSlipped mSlip
                noCrashReason
                    | corOom corr =
                        if corOomCause corr == Just "oom_kill_signal_9" then "oom-kill" else "oom"
                    | corSqliteAbort corr = "sqlite-abort"
                    | corExternalKill corr = "external-kill"
                    | otherwise = "no-crash"
                (sigMatched, regexOk)
                    | T.null expSig = (True, Nothing)
                    | null faults = (False, Just (snd (matchExpectedSignature expSig "")))
                    | otherwise =
                        let scopeTxt = T.intercalate "\n" (indicators <> cands)
                            (matched, rValid) = matchExpectedSignature expSig scopeTxt
                         in (matched, Just rValid)
                withEvidence dec =
                    dec
                        { odIndicators = Just indicators
                        , odFaultEvents = Just faults
                        , odCorroboration = Just corr
                        , odConnectionLost = Just hasConnLost
                        , odRetcode = Just rc
                        , odRegexValid = regexOk
                        , odCrashSignature = mSig
                        , odSlippage = mSlip
                        , odLogMode = Just modeLabel
                        }

            when (not (T.null expSig) && (null faults || not sigMatched)) $
                exitProbe
                    False
                    1
                    ( withEvidence
                        baseDec
                            { odVerdict = VerdictRejected
                            , odClassification = ClassHealthyNoFault
                            , odReason = if null faults then noCrashReason else "signature-mismatch"
                            }
                    )
            when (T.null expSig && null faults && hasConnLost && ooAllowStderrOnly opts && not embedded) $
                exitProbe
                    False
                    0
                    baseDec
                        { odVerdict = VerdictAccepted
                        , odClassification = ClassMissingEvidence
                        , odReason = "connection-loss-stderr-unsafe"
                        , odConnectionLost = Just True
                        , odRetcode = Just rc
                        , odLogMode = Just modeLabel
                        }
            when (T.null expSig && null faults) $
                exitProbe
                    False
                    1
                    baseDec
                        { odVerdict = VerdictRejected
                        , odClassification = ClassHealthyNoFault
                        , odReason = noCrashReason
                        , odCorroboration = Just corr
                        , odConnectionLost = Just hasConnLost
                        , odRetcode = Just rc
                        , odLogMode = Just modeLabel
                        }
            when slippedReject $
                exitProbe
                    False
                    1
                    ( withEvidence
                        baseDec
                            { odVerdict = VerdictRejected
                            , odClassification = ClassConfirmedFault
                            , odReason = "signature-slippage"
                            }
                    )
            pure
                ( False
                , 0
                , withEvidence
                    baseDec
                        { odVerdict = VerdictAccepted
                        , odClassification = ClassConfirmedFault
                        , odReason = if T.null expSig then "crash-indicator" else "crash-signature"
                        }
                )

-- | Locate and stat the initial crash evidence source (file, container log, or stderr fallback).
acquireLogSource ::
    OracleOptions ->
    DbmsConnection ->
    FilePath ->
    IO (Either (OracleDecision -> OracleDecision) (LogMode, Integer, Maybe Integer, Text))
acquireLogSource opts conn internalLog = runExceptT do
    sinceIso <- formatIsoUtc <$> liftIO getCurrentTime
    if not (isServerDbmsKind (ooDbms opts))
        then pure $ case connContainer conn of
            Just cname -> (ModeEmbeddedContainer cname, 0, Nothing, sinceIso)
            Nothing -> (ModeEmbeddedLocal, 0, Nothing, sinceIso)
        else case (ooLogFile opts, connContainer conn) of
            (Just path, mContainer) -> acquireHostLog path mContainer sinceIso
            (Nothing, Just cname) -> acquireContainer cname sinceIso
            (Nothing, Nothing)
                | ooAllowStderrOnly opts -> pure (ModeStderr, 0, Nothing, sinceIso)
                | otherwise ->
                    throwE \d ->
                        d
                            { odVerdict = VerdictRejected
                            , odClassification = ClassMissingEvidence
                            , odReason = "log-source-required"
                            , odHint = Just "pass --log-file or --container-name (or --allow-stderr-only)"
                            }
    where
        acquireHostLog path mContainer sinceIso = do
            readable <- liftIO (waitForReadableFile path logWaitSeconds)
            if not readable
                then case mContainer of
                    Just cname -> acquireContainer cname sinceIso
                    Nothing ->
                        throwE \d ->
                            d
                                { odVerdict = VerdictRejected
                                , odClassification = ClassMissingEvidence
                                , odReason = "log-unreadable"
                                , odLogFile = Just path
                                , odHint = Just "mount the log readably or pass --container-name"
                                }
                else do
                    stRes <- liftIO (try (getFileStatus path) :: IO (Either IOException FileStatus))
                    case stRes of
                        Right st ->
                            pure (ModeFile path, fromIntegral (fileSize st), Just (fromIntegral (fileID st)), sinceIso)
                        Left _ ->
                            throwE \d ->
                                d
                                    { odVerdict = VerdictRejected
                                    , odClassification = ClassMissingEvidence
                                    , odReason = "log-stat-failed"
                                    , odLogFile = Just path
                                    }

        acquireContainer cname sinceIso = do
            mState <- liftIO (getContainerLogState cname internalLog)
            case mState of
                Just (sz, ino) -> pure (ModeContainer cname internalLog, sz, Just ino, sinceIso)
                Nothing ->
                    throwE \d ->
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

-- | Abstract incremental tail cursor shared between host log files and Docker container logs.
data LogCursor = LogCursor
    { lcStat :: !(IO (Maybe (Integer, Integer)))
    -- ^ Query current @(sizeBytes, inode)@ of the active log target.
    , lcReadSlice :: !(Integer -> Integer -> IO (Maybe Text))
    -- ^ Read up to @count@ bytes starting at byte offset @start@.
    , lcReadExtra :: !(IO Text)
    -- ^ Optional secondary output stream (such as @docker logs --since@).
    }

-- | Poll a 'LogCursor' incrementally from @(curOff, curInode)@, handling log rotation/truncation.
pollLogCursor ::
    LogCursor -> Integer -> Maybe Integer -> Text -> IO (Bool, Integer, Maybe Integer, Text, Text)
pollLogCursor cursor curOff curInode accFile = do
    mState <- lcStat cursor
    case mState of
        Nothing -> combine False curOff curInode accFile
        Just (curSize, inodeNow) -> do
            let offset0 = if maybe False (/= inodeNow) curInode || curSize < curOff then 0 else curOff
            (okFile, nextOff, nextFile) <-
                if curSize <= offset0
                    then pure (True, offset0, accFile)
                    else do
                        let start = max offset0 (curSize - maxLogReadBytes)
                        mChunk <- lcReadSlice cursor start (curSize - start)
                        pure $ case mChunk of
                            Nothing -> (False, offset0, accFile)
                            Just chunk -> (True, curSize, tailText maxTotalLogChars (accFile <> chunk))
            combine okFile nextOff (Just inodeNow) nextFile
    where
        combine ok off mInode nextFile = do
            extra <- lcReadExtra cursor
            let combined =
                    tailText maxTotalLogChars $
                        if T.null extra
                            then nextFile
                            else if T.null nextFile then extra else nextFile <> "\n" <> extra
            pure (ok, off, mInode, combined, nextFile)

pollFileReader ::
    FilePath -> Integer -> Maybe Integer -> Text -> IO (Bool, Integer, Maybe Integer, Text, Text)
pollFileReader path =
    pollLogCursor
        LogCursor
            { lcStat = do
                res <- try (getFileStatus path) :: IO (Either IOException FileStatus)
                pure $ case res of
                    Left _ -> Nothing
                    Right st -> Just (fromIntegral (fileSize st), fromIntegral (fileID st))
            , lcReadSlice = \start count -> do
                res <-
                    try
                        ( withBinaryFile path ReadMode \h -> do
                            hSeek h AbsoluteSeek start
                            bytes <- BS.hGet h (fromIntegral count)
                            pure (TE.decodeUtf8With TEE.lenientDecode bytes)
                        ) ::
                        IO (Either IOException Text)
                pure (either (const Nothing) Just res)
            , lcReadExtra = pure ""
            }

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
pollContainerReader cname internalLog sinceIso =
    pollLogCursor
        LogCursor
            { lcStat = getContainerLogState cname internalLog
            , lcReadSlice = \start _ -> do
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
                    Right (False, 0, outBs, _) -> Just (TE.decodeUtf8With TEE.lenientDecode outBs)
                    _ -> Nothing
            , lcReadExtra = readContainerStdoutLogs cname sinceIso
            }

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

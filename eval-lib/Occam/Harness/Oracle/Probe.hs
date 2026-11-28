{-# LANGUAGE OverloadedRecordDot #-}

-- | Execute candidate SQL and classify the resulting DBMS evidence.
module Occam.Harness.Oracle.Probe (
    probeCrashOracle,
    probeCrashOracleCancellable,
) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (IOException, try)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), runContT)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.IORef (IORef, readIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time (diffUTCTime, getCurrentTime)
import Occam.Harness.Config (HarnessConfig)
import Occam.Harness.DbmsExec (
    ClientRole (..),
    DbmsConnection (..),
    buildDbmsCommand,
    resolveDbmsConnection,
    runProcessCaptureCancellable,
 )
import Occam.Harness.Evidence (
    connectionLostPhrasesForDbms,
    extractDbmsEvidence,
    hasPsqlMetaCommand,
 )
import Occam.Harness.Oracle.Log (
    acquireLogSource,
    currentEnvMap,
    emitDecisionLog,
    formatIsoUtc,
    logPollIntervalMicros,
    logWaitSeconds,
    nonEmptyText,
    pollContainerReader,
    pollFileReader,
    whenVerbose,
 )
import Occam.Harness.Oracle.Types
import Occam.Harness.Regex (matchExpectedSignature)
import Occam.Harness.Signature (
    Corroboration (..),
    DbmsKind (..),
    EvidenceResult (..),
    FaultEvent (..),
    SlippageVerdict (..),
    buildCrashSignature,
    evaluateSlippage,
    isServerDbmsKind,
    loadBaselineRef,
 )

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
                { timestamp = tsText
                , dbms = opts.dbms
                , candidateSha16 = sha16
                , candidateBytes = BS.length candidateSql
                , expectedSignature = opts.expectedSignature
                , baselineSignature = nonEmptyText opts.baselineSignature
                , workdir = opts.workdir
                , verdict = VerdictRejected
                , classification = ClassHealthyNoFault
                , reason = "uninitialized"
                , logMode = Nothing
                , logFile = Nothing
                , retcode = Nothing
                , connectionLost = Nothing
                , regexValid = Nothing
                , timeoutSeconds = Nothing
                , indicators = Nothing
                , faultEvents = Nothing
                , corroboration = Nothing
                , crashSignature = Nothing
                , slippage = Nothing
                , hint = Nothing
                }
        sqlTxt = TE.decodeUtf8With TEE.lenientDecode candidateSql
        mTimeout = opts.timeoutSeconds <$ guard (opts.timeoutSeconds > 0)

    (interrupted, code, dec) <- (`runContT` pure) do
        when (T.null (T.strip sqlTxt)) $
            exitProbe False 1 baseDec {reason = "empty-sql"}
        when (opts.dbms == DbmsPostgres && not opts.allowPsqlMeta && hasPsqlMetaCommand sqlTxt) $
            exitProbe False 1 baseDec {reason = "psql-meta-command"}

        conn <- liftIO (resolveDbmsConnection cfg opts.dbms opts.connection)
        let internalLog = fromMaybe (connContainerLog conn) opts.containerLog
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
                        { verdict = VerdictRejected
                        , classification = ClassMissingEvidence
                        , reason = "execution-error"
                        , hint = Just decHint
                        , logMode = Just modeLabel
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
                        { verdict = VerdictRejected
                        , classification = ClassMissingEvidence
                        , reason = "interrupted"
                        , logMode = Just modeLabel
                        }
            (False, True, _, _, _) ->
                exitProbe
                    False
                    124
                    baseDec
                        { verdict = VerdictTimeout
                        , classification = ClassMissingEvidence
                        , reason = "client-timeout"
                        , timeoutSeconds = Just opts.timeoutSeconds
                        , logMode = Just modeLabel
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
                                    { verdict = VerdictRejected
                                    , classification = ClassMissingEvidence
                                    , reason = "container-exec-failed"
                                    , retcode = Just retcode
                                    , logMode = Just modeLabel
                                    , hint = Just (T.take 512 stderrTxt)
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

    emitDecisionLog opts.decisionLog dec
    whenVerbose opts.verbose dec
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
                            opts.workdir
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
        ModeStderr ->
            pure (evaluateStderrOutcome opts modeLabel retcode stderrTxt baseDec)
        _ -> do
            let dbms = opts.dbms
                connLost = any (`T.isInfixOf` stderrTxt) (connectionLostPhrasesForDbms dbms)
                isEmbedded = not (isServerDbmsKind dbms)
            (logReadOk, serverLogTxt) <-
                liftIO (readServerLog opts mode internalLog initOffset initInode sinceIso retcode stderrTxt)
            when (modeHasFileLog mode && not logReadOk && T.null serverLogTxt) $
                exitProbe
                    False
                    1
                    baseDec
                        { verdict = VerdictRejected
                        , classification = ClassMissingEvidence
                        , reason = "log-read-failed"
                        , logMode = Just modeLabel
                        , logFile = modeLogFilePath mode
                        }
            classifyFullEvidence opts modeLabel isEmbedded connLost retcode serverLogTxt stderrTxt baseDec

modeHasFileLog :: LogMode -> Bool
modeHasFileLog (ModeFile _) = True
modeHasFileLog (ModeContainer _ _) = True
modeHasFileLog _ = False

modeLogFilePath :: LogMode -> Maybe FilePath
modeLogFilePath (ModeFile path) = Just path
modeLogFilePath (ModeContainer _ path) = Just path
modeLogFilePath _ = Nothing

readServerLog ::
    OracleOptions ->
    LogMode ->
    FilePath ->
    Integer ->
    Maybe Integer ->
    Text ->
    Int ->
    Text ->
    IO (Bool, Text)
readServerLog opts mode internalLog initOffset initInode sinceIso retcode stderrTxt =
    case mode of
        ModeFile path -> pollLogs possibleCrash (pollFileReader path)
        ModeContainer cname _ -> pollLogs possibleCrash (pollContainerReader cname internalLog sinceIso)
        _ -> pure (True, "")
    where
        dbms = opts.dbms
        connLost = any (`T.isInfixOf` stderrTxt) (connectionLostPhrasesForDbms dbms)
        possibleCrash = connLost || retcode /= 0
        expectedSignature = opts.expectedSignature

        pollLogs ::
            Bool ->
            (Integer -> Maybe Integer -> Text -> IO (Bool, Integer, Maybe Integer, Text, Text)) ->
            IO (Bool, Text)
        pollLogs maybeCrash stepReader = do
            started <- getCurrentTime
            let waitSecs = if maybeCrash then logWaitSeconds else 0.0
                go curOff curInode accFile = do
                    (ok, nextOffset, nextInode, combinedTxt, nextFile) <-
                        stepReader curOff curInode accFile
                    if not ok
                        then pure (False, combinedTxt)
                        else do
                            let evidence = extractDbmsEvidence dbms combinedTxt stderrTxt retcode
                                faults = erFaults evidence
                                candidates = erCandidateLines evidence
                                doneMatch
                                    | T.null expectedSignature = not (null faults)
                                    | otherwise =
                                        not (null faults)
                                            && fst
                                                ( matchExpectedSignature
                                                    expectedSignature
                                                    (T.unlines (map feDetail faults <> candidates))
                                                )
                            now <- getCurrentTime
                            if doneMatch || realToFrac (diffUTCTime now started) >= waitSecs
                                then pure (True, combinedTxt)
                                else do
                                    threadDelay logPollIntervalMicros
                                    go nextOffset nextInode nextFile
            go initOffset initInode ""

evaluateStderrOutcome ::
    OracleOptions ->
    Text ->
    Int ->
    Text ->
    OracleDecision ->
    ProbeOutcome
evaluateStderrOutcome opts modeLabel retcode stderrTxt baseDec
    | not (T.null expectedSignature) =
        let (matched, regexValid) = matchExpectedSignature expectedSignature stderrTxt
         in ( False
            , if matched then 0 else 1
            , baseDec
                { verdict = if matched then VerdictAccepted else VerdictRejected
                , classification = ClassMissingEvidence
                , reason = if matched then "stderr-signature-unsafe" else "stderr-signature-mismatch"
                , regexValid = Just regexValid
                , crashSignature = crashSignature
                , logMode = Just modeLabel
                }
            )
    | connectionLost || not (null faults) =
        ( False
        , 0
        , baseDec
            { verdict = VerdictAccepted
            , classification = ClassMissingEvidence
            , reason = "stderr-crash-unsafe"
            , faultEvents = Just faults
            , connectionLost = Just connectionLost
            , crashSignature = crashSignature
            , logMode = Just modeLabel
            }
        )
    | otherwise =
        ( False
        , 1
        , baseDec
            { verdict = VerdictRejected
            , classification = ClassMissingEvidence
            , reason = "no-crash"
            , connectionLost = Just connectionLost
            , logMode = Just modeLabel
            }
        )
    where
        dbms = opts.dbms
        expectedSignature = opts.expectedSignature
        connectionLost = any (`T.isInfixOf` stderrTxt) (connectionLostPhrasesForDbms dbms)
        evidence = extractDbmsEvidence dbms stderrTxt stderrTxt retcode
        faults = erFaults evidence
        candidates = erCandidateLines evidence
        crashSignature = buildCrashSignature dbms faults candidates retcode opts.stackFrames

classifyFullEvidence ::
    OracleOptions ->
    Text ->
    Bool ->
    Bool ->
    Int ->
    Text ->
    Text ->
    OracleDecision ->
    ProbeM ProbeOutcome
classifyFullEvidence opts modeLabel embedded connectionLost retcode serverLogTxt stderrTxt baseDec = do
    let dbms = opts.dbms
        expectedSignature = opts.expectedSignature
        evidence = extractDbmsEvidence dbms serverLogTxt stderrTxt retcode
        faults = erFaults evidence
        candidates = erCandidateLines evidence
        corroboration = erCorroboration evidence
        indicators = map feDetail faults
        crashSignature = buildCrashSignature dbms faults candidates retcode opts.stackFrames
    baseline <-
        liftIO
            if T.null (T.strip opts.baselineSignature)
                then pure Nothing
                else loadBaselineRef opts.baselineSignature
    let slippage = evaluateSlippage <$> baseline <*> crashSignature
        rejectSlippage = opts.rejectOnSlippage && maybe False svSlipped slippage
        noCrashReason
            | corOom corroboration =
                if corOomCause corroboration == Just "oom_kill_signal_9" then "oom-kill" else "oom"
            | corSqliteAbort corroboration = "sqlite-abort"
            | corExternalKill corroboration = "external-kill"
            | otherwise = "no-crash"
        (signatureMatched, regexValid)
            | T.null expectedSignature = (True, Nothing)
            | null faults = (False, Just (snd (matchExpectedSignature expectedSignature "")))
            | otherwise =
                let scopeTxt = T.intercalate "\n" (indicators <> candidates)
                    (matched, valid) = matchExpectedSignature expectedSignature scopeTxt
                 in (matched, Just valid)
        withEvidence decision =
            decision
                { indicators = Just indicators
                , faultEvents = Just faults
                , corroboration = Just corroboration
                , connectionLost = Just connectionLost
                , retcode = Just retcode
                , regexValid = regexValid
                , crashSignature = crashSignature
                , slippage = slippage
                , logMode = Just modeLabel
                }

    when (not (T.null expectedSignature) && (null faults || not signatureMatched)) $
        exitProbe
            False
            1
            ( withEvidence
                baseDec
                    { verdict = VerdictRejected
                    , classification = ClassHealthyNoFault
                    , reason = if null faults then noCrashReason else "signature-mismatch"
                    }
            )
    when
        (T.null expectedSignature && null faults && connectionLost && opts.allowStderrOnly && not embedded)
        $ exitProbe
            False
            0
            baseDec
                { verdict = VerdictAccepted
                , classification = ClassMissingEvidence
                , reason = "connection-loss-stderr-unsafe"
                , connectionLost = Just True
                , retcode = Just retcode
                , logMode = Just modeLabel
                }
    when (T.null expectedSignature && null faults) $
        exitProbe
            False
            1
            baseDec
                { verdict = VerdictRejected
                , classification = ClassHealthyNoFault
                , reason = noCrashReason
                , corroboration = Just corroboration
                , connectionLost = Just connectionLost
                , retcode = Just retcode
                , logMode = Just modeLabel
                }
    when rejectSlippage $
        exitProbe
            False
            1
            ( withEvidence
                baseDec
                    { verdict = VerdictRejected
                    , classification = ClassConfirmedFault
                    , reason = "signature-slippage"
                    }
            )
    pure
        ( False
        , 0
        , withEvidence
            baseDec
                { verdict = VerdictAccepted
                , classification = ClassConfirmedFault
                , reason = if T.null expectedSignature then "crash-indicator" else "crash-signature"
                }
        )

{-# LANGUAGE OverloadedRecordDot #-}

-- | Qualification and reduction jobs executed by batch workers.
module Occam.Harness.Batch.Jobs (
    qualifyCaseOnWorker,
    runJobOnWorker,
) where

import Control.Exception.Safe (IOException, try)
import Control.Monad (guard, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), runContT)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time (diffUTCTime, getCurrentTime)
import Occam.AtomicFile (writeAtomicLazyBytes)
import Occam.Benchmark.BatchReport (StatsEvalMetrics (..), StatsSnapshot (..), summarizeDecisionLog)
import Occam.Harness.Batch.Types
import Occam.Harness.Batch.Workers (ensureWorkerTarget, resetWorker, workerEndpoint)
import Occam.Harness.BatchInputs (Attributes (..), BatchCase (..))
import Occam.Harness.BatchPlan (
    BatchPlanOptions (..),
    Job (..),
    PreparedBatchPlan (..),
    Profile (..),
    Settings (..),
    jobId,
    profileFlagsHash,
 )
import Occam.Harness.Checkpoints (BatchJobResult (..), JobState (..))
import Occam.Harness.Config (DbmsSpec (..))
import Occam.Harness.DbmsExec (
    ConnectionOverrides (..),
    emptyConnectionOverrides,
    runProcessCaptureCancellable,
 )
import Occam.Harness.Replay (
    QualificationResult (..),
    ReplayConfig (..),
    ReplayEndpoint (..),
    ReplayVerification (..),
    defaultReplayConfig,
    runReplayVerification,
 )
import Occam.Harness.Signature (CrashSignature (..), DbmsKind (..), dbmsKindName, parseDbmsKind)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    removeDirectoryRecursive,
 )
import System.Environment (getEnvironment)
import System.FilePath ((</>))

abortWorker :: ContT () IO a
abortWorker = ContT \_ -> pure ()

guardNotStopped :: WorkerHandle -> ContT () IO ()
guardNotStopped wh = do
    stopped <- liftIO (readIORef wh.stopRef)
    when stopped abortWorker

-- | Qualify a single batch input via N-replay and write @qualified/<case_id>/input_replay.json@.
qualifyCaseOnWorker :: WorkerHandle -> BatchOptions -> PreparedBatchPlan -> BatchCase -> IO ()
qualifyCaseOnWorker wh opts plan bc = (`runContT` pure) do
    guardNotStopped wh
    let pOpts = opts.plan
        settings = plan.settings
        outDir = plan.outDir
        cid = batchCaseId bc
        qualDir = outDir </> "qualified" </> T.unpack cid
        qualPath = qualDir </> "input_replay.json"
        decLog = qualDir </> "input_replay_decisions.jsonl"
    liftIO (createDirectoryIfMissing True qualDir)
    dbmsKind <- liftIO (requireDbmsKind (batchDbms bc).identifier)

    when pOpts.skipInputReplay do
        let rv =
                ReplayVerification
                    { rvRunsRequired = pOpts.replays
                    , rvRunsPassed = 0
                    , rvAllPassed = True
                    , rvConsistentSignature = True
                    , rvBaselineSignature = attrBaseline (batchAttributes bc)
                    , rvMatchesBaseline = Nothing
                    , rvSlippageCount = 0
                    , rvObservedSignatures = maybeToList (attrBaseline (batchAttributes bc))
                    , rvObservedSignatureIds = maybe [] (\s -> [csId s]) (attrBaseline (batchAttributes bc))
                    , rvInterrupted = False
                    , rvRuns = []
                    }
            qr =
                QualificationResult
                    { qrCaseId = cid
                    , qrDbms = dbmsKind
                    , qrDialect = batchDialect bc
                    , qrSqlSha256 = batchSqlSha256 bc
                    , qrExpectedSignature = batchSignature bc
                    , qrStackFrames = settings.stackFrames
                    , qrSkipped = True
                    , qrQualified = True
                    , qrBaselineSignature = attrBaseline (batchAttributes bc)
                    , qrSignatureSlippageAgainstBaseline = Nothing
                    , qrReplay = rv
                    }
        liftIO (writeAtomicLazyBytes qualPath (encode qr <> BSL.pack [10]))
        abortWorker

    sqlBytes <- liftIO (BS.readFile (batchFile bc))
    liftIO (ensureWorkerTarget wh False dbmsKind (batchDbms bc) (batchImage bc))
    let rCfg =
            (defaultReplayConfig dbmsKind emptyConnectionOverrides)
                { rcEndpoint = workerEndpoint wh dbmsKind
                , rcResetFn = Just (resetWorker wh dbmsKind)
                , rcExpectedSignature = batchSignature bc
                , rcRejectOnSlippage = False
                , rcStackFrames = settings.stackFrames
                , rcDecisionLog = Just decLog
                , rcProbeTimeout = pOpts.probeTimeout
                , rcMaxWaitReset = settings.containerWait
                , rcResetBeforeRun = True
                , rcRequireConsistent = True
                }
    rv <-
        liftIO
            (runReplayVerification wh.harnessConfig (Just wh.stopRef) rCfg pOpts.replays sqlBytes)
    when (rvInterrupted rv) abortWorker
    guardNotStopped wh
    let qualified = rvAllPassed rv && rvConsistentSignature rv
        chosenSig = rvBaselineSignature rv
        metaBase = attrBaseline (batchAttributes bc)
        slipVsMeta = case (chosenSig, metaBase) of
            (Just obs, Just base) -> Just (csId obs /= csId base && csHash obs /= csHash base)
            _ -> Nothing
        qr =
            QualificationResult
                { qrCaseId = cid
                , qrDbms = dbmsKind
                , qrDialect = batchDialect bc
                , qrSqlSha256 = batchSqlSha256 bc
                , qrExpectedSignature = batchSignature bc
                , qrStackFrames = settings.stackFrames
                , qrSkipped = False
                , qrQualified = qualified
                , qrBaselineSignature = chosenSig
                , qrSignatureSlippageAgainstBaseline = slipVsMeta
                , qrReplay = rv
                }
    liftIO (writeAtomicLazyBytes qualPath (encode qr <> BSL.pack [10]))

-- | Construct a baseline 'BatchJobResult' template parameterized by outcome state.
baseJobResult ::
    Job ->
    BatchCase ->
    DbmsKind ->
    Text ->
    Bool ->
    Maybe CrashSignature ->
    JobState ->
    BatchJobResult
baseJobResult j@(Job _ prof repetition) bc dbmsKind cfgHash inputQualified baselineSig state =
    BatchJobResult
        { bjrJobId = jobId j
        , bjrCaseId = batchCaseId bc
        , bjrDbms = dbmsKindName dbmsKind
        , bjrDialect = batchDialect bc
        , bjrProfile = profile prof
        , bjrRepeat = repetition
        , bjrState = state
        , bjrEvalStatus = Nothing
        , bjrInitialBytes = batchByteCount bc
        , bjrSqlSha256 = batchSqlSha256 bc
        , bjrProfileHash = profileFlagsHash prof
        , bjrConfigHash = Just cfgHash
        , bjrInputQualified = inputQualified
        , bjrRanOccam = False
        , bjrBaselineSignature = baselineSig
        , bjrStatsPath = Nothing
        , bjrReducedReplayVerification = Nothing
        , bjrOracleDecisionsSummary = Nothing
        , bjrExitCode = Nothing
        , bjrElapsedSeconds = Nothing
        , bjrError = Nothing
        }

-- | Run one reduction job (reset, @occam@ execution, reduced N-replay verification) and publish @job_result.json@.
runJobOnWorker ::
    WorkerHandle ->
    BatchOptions ->
    PreparedBatchPlan ->
    FilePath ->
    FilePath ->
    BatchCase ->
    Job ->
    Text ->
    Maybe QualificationResult ->
    IO ()
runJobOnWorker wh opts plan occamBin harnessBin bc j@(Job _ prof _) cfgHash mQr = (`runContT` pure) do
    guardNotStopped wh
    let outDir = plan.outDir
        jobDir = outDir </> "runs" </> T.unpack (jobId j)
        jobResPath = jobDir </> "job_result.json"
        pOpts = opts.plan
        inputQualified = maybe True qrQualified mQr
        baselineSig = (mQr >>= qrBaselineSignature) `altMaybe` attrBaseline (batchAttributes bc)
        writeResult res = liftIO (writeAtomicLazyBytes jobResPath (encode res <> BSL.pack [10]))

    dbmsKind <- liftIO (requireDbmsKind (batchDbms bc).identifier)
    liftIO (cleanPartialDir jobDir)
    let mkRes = baseJobResult j bc dbmsKind cfgHash inputQualified baselineSig

    when (not pOpts.skipInputReplay && not inputQualified) do
        writeResult (mkRes JobSkippedUnstableInput)
        abortWorker

    liftIO (ensureWorkerTarget wh False dbmsKind (batchDbms bc) (batchImage bc))
    ep <-
        liftIO
            ( runExceptT do
                withExceptT ("Pre-run reset failed: " <>) (ExceptT (resetWorker wh dbmsKind))
                ExceptT (workerEndpoint wh dbmsKind)
            )
            >>= either
                (\err -> writeResult ((mkRes JobInfraError) {bjrError = Just (T.pack err)}) >> abortWorker)
                pure

    let outStats = jobDir </> "stats.json"
        minimalPath = jobDir </> "minimal.sql"
        decLog = jobDir </> "oracle_decisions.jsonl"
        replayDecLog = jobDir </> "replay_decisions.jsonl"
        stderrLogPath = jobDir </> "occam.stderr.log"
        settings = plan.settings
    oracleCmd <-
        liftIO (buildSubCmd "oracle" ep dbmsKind (Just decLog) (batchSignature bc) baselineSig settings)
    resetCmd <- liftIO (buildSubCmd "reset" ep dbmsKind Nothing "" Nothing settings)
    let occamArgs =
            [ batchFile bc
            , "--oracle"
            , oracleCmd
            , "--reset"
            , resetCmd
            , "--dialect"
            , T.unpack (batchDialect bc)
            , "--timeout"
            , show settings.timeout
            , "--jobs"
            , show settings.occamJobs
            , "--out"
            , minimalPath
            , "--stats"
            , outStats
            ]
                <> maybe [] (\b -> ["--budget", show b]) settings.budget
                <> maybe [] (\m -> ["--max-oracle-executions", show m]) settings.maxExecutions
                <> ["--no-cache" | settings.noCache]
                <> map T.unpack (flags prof)
        hardTimeout = (\b -> b + max 60.0 (settings.timeout * 4.0)) <$> settings.budget
    childEnv <- liftIO isolatedSubprocessEnv
    t0 <- liftIO getCurrentTime
    (interrupted, timedOut, procRc, _, stderrBs) <-
        liftIO $
            runProcessCaptureCancellable
                True
                (Just wh.stopRef)
                Nothing
                (Just childEnv)
                hardTimeout
                occamBin
                occamArgs
                BS.empty
    t1 <- liftIO getCurrentTime
    when interrupted abortWorker

    let wallSecs = realToFrac (diffUTCTime t1 t0) :: Double
    liftIO (BS.writeFile stderrLogPath stderrBs)
    mStats <- liftIO (readStatsJson outStats)
    replayRes <- liftIO (runReducedReplay dbmsKind minimalPath replayDecLog baselineSig)
    guardNotStopped wh
    when (maybe False rvInterrupted replayRes) abortWorker

    decSum <- liftIO (summarizeDecisionLog decLog)
    let evalStatus = mStats >>= stStatus
        valPassed = mStats >>= stEvaluationMetrics >>= semValidationPassed
        state
            | evalStatus == Just "completed" && procRc == 0 && fromMaybe True valPassed = JobCompleted
            | evalStatus `elem` [Just "timeout", Just "execution-budget"] = JobCensored
            | evalStatus `elem` [Just "validation-failed", Just "validation-incomplete"]
                || (evalStatus == Just "completed" && valPassed == Just False) =
                JobValidationFailed
            | otherwise = JobError
        errNote
            | timedOut = Just "occam hard wall-clock timeout exceeded"
            | state == JobError =
                Just (T.take 512 (TE.decodeUtf8With TEE.lenientDecode stderrBs))
            | otherwise = Nothing
        res =
            (mkRes state)
                { bjrEvalStatus = evalStatus
                , bjrRanOccam = True
                , bjrStatsPath = outStats <$ guard (isJust mStats)
                , bjrReducedReplayVerification = replayRes
                , bjrOracleDecisionsSummary = Just decSum
                , bjrExitCode = Just procRc
                , bjrElapsedSeconds = Just wallSecs
                , bjrError = errNote
                }
    writeResult res
    where
        altMaybe (Just a) _ = Just a
        altMaybe Nothing b = b

        cleanPartialDir dir = do
            exists <- doesDirectoryExist dir
            when exists $ void (try (removeDirectoryRecursive dir) :: IO (Either IOException ()))
            createDirectoryIfMissing True dir

        runReducedReplay dbmsKind minimalPath replayDecLog baselineSig = runMaybeT do
            let pOpts = opts.plan
                settings = plan.settings
            guard (pOpts.replays > 0)
            liftIO (doesFileExist minimalPath) >>= guard
            sqlBytes <- liftIO (BS.readFile minimalPath)
            guard (not (BS.null sqlBytes))
            let rCfg =
                    (defaultReplayConfig dbmsKind emptyConnectionOverrides)
                        { rcEndpoint = workerEndpoint wh dbmsKind
                        , rcResetFn = Just (resetWorker wh dbmsKind)
                        , rcExpectedSignature = batchSignature bc
                        , rcBaselineSignature = csId <$> baselineSig
                        , rcRejectOnSlippage = False
                        , rcStackFrames = settings.stackFrames
                        , rcDecisionLog = Just replayDecLog
                        , rcProbeTimeout = pOpts.probeTimeout
                        , rcMaxWaitReset = settings.containerWait
                        , rcResetBeforeRun = True
                        , rcRequireConsistent = True
                        }
            liftIO
                (runReplayVerification wh.harnessConfig (Just wh.stopRef) rCfg pOpts.replays sqlBytes)

        readStatsJson path = do
            exists <- doesFileExist path
            if not exists
                then pure Nothing
                else do
                    res <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
                    pure (either (const Nothing) (either (const Nothing) Just . eitherDecodeStrict') res)

        buildSubCmd sub ep dbmsKind mDecLog expSig mBaseSig settings = do
            let ov = reConnection ep
                connArgs =
                    ["--dbms", T.unpack (dbmsKindName dbmsKind)]
                        <> maybe [] (\h -> ["--host", T.unpack h]) (coHost ov)
                        <> maybe [] (\p -> ["--port", show p]) (coPort ov)
                        <> maybe [] (\d -> ["--dbname", T.unpack d]) (coDbName ov)
                        <> maybe [] (\c -> ["--container-name", T.unpack c]) (coContainer ov)
                        <> maybe [] (\b -> ["--engine-bin", b]) (coEngineBin ov)
                roleArgs = case sub of
                    "oracle" ->
                        maybe [] (\lf -> ["--log-file", lf]) (reLogFile ep)
                            <> (if T.null expSig then [] else ["--expected-signature", T.unpack expSig])
                            <> maybe [] (\bs -> ["--baseline-signature", T.unpack (csId bs)]) mBaseSig
                            <> ["--reject-on-slippage" | settings.rejectSlippage]
                            <> ["--stack-frames", show settings.stackFrames]
                            <> maybe [] (\dl -> ["--decision-log", dl]) mDecLog
                    _ -> []
            pure
                (unwords (map shellQuote (harnessBin : sub : "--config" : opts.configPath : connArgs <> roleArgs)))

isolatedSubprocessEnv :: IO [(String, String)]
isolatedSubprocessEnv =
    filter (\(k, _) -> k `Set.notMember` ambientKeys) <$> getEnvironment
    where
        ambientKeys =
            Set.fromList
                [ "DBMS"
                , "PGHOST"
                , "PGPORT"
                , "PGUSER"
                , "PGPASSWORD"
                , "PGDATABASE"
                , "MYSQL_HOST"
                , "MYSQL_TCP_PORT"
                , "MYSQL_USER"
                , "MYSQL_PWD"
                , "MYSQL_DATABASE"
                , "MYSQL_SOCKET"
                , "MARIADB_HOST"
                , "MARIADB_TCP_PORT"
                , "MARIADB_USER"
                , "MARIADB_PWD"
                , "MARIADB_DATABASE"
                , "MARIADB_UNIX_PORT"
                , "CLICKHOUSE_HOST"
                , "CLICKHOUSE_PORT"
                , "CLICKHOUSE_USER"
                , "CLICKHOUSE_PASSWORD"
                , "CLICKHOUSE_DATABASE"
                , "CLICKHOUSE_DB"
                , "SQLITE_BIN"
                , "DUCKDB_BIN"
                , "CONTAINER_NAME"
                , "TEMPLATE_DB"
                , "EXPECTED_SIGNATURE"
                , "BASELINE_SIGNATURE"
                , "ORACLE_REJECT_ON_SLIPPAGE"
                , "ORACLE_STACK_FRAMES"
                , "ORACLE_TIMEOUT"
                , "ORACLE_VERBOSE"
                , "ORACLE_ALLOW_STDERR_ONLY"
                , "ORACLE_ALLOW_PSQL_META"
                , "ORACLE_DECISION_LOG"
                , "OCCAM_WORKDIR"
                ]
requireDbmsKind :: Text -> IO DbmsKind
requireDbmsKind name = case parseDbmsKind name of
    Just k -> pure k
    Nothing -> fail ("Unsupported DBMS: " <> T.unpack name)
shellQuote :: String -> String
shellQuote s = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) s <> "'"

-- | Native Haskell batch orchestrator (@occam-harness batch@): containers, qualification, reduction, and reporting.
module Occam.Harness.Batch (
    BatchOptions (..),
    BatchSummary (..),
    runBatch,
) where

import Control.Concurrent (
    MVar,
    forkIO,
    modifyMVar,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
    threadDelay,
    withMVar,
 )
import Control.Exception.Safe (IOException, SomeException, bracket, catch, finally, try)
import Control.Monad (forM, guard, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT, runContT)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Occam.AtomicFile (writeAtomicLazyBytes)
import Occam.Benchmark.BatchReport (
    AggregatedReport (..),
    StatsEvalMetrics (..),
    StatsSnapshot (..),
    aggregateOutDir,
    summarizeDecisionLog,
 )
import Occam.Harness.BatchInputs (Attributes (..), BatchCase (..))
import Occam.Harness.BatchPlan (
    BatchPlanOptions (..),
    ExternalContainer (..),
    Job (..),
    PreparedBatchPlan (..),
    Profile (..),
    Settings (..),
    jobId,
    prepareBatchPlan,
    profileFlagsHash,
 )
import Occam.Harness.Checkpoints (
    BatchJobResult (..),
    JobCheck (..),
    JobState (..),
    selectReusableJobs,
    selectReusableQualifications,
 )
import Occam.Harness.Config (DbmsSpec (..), HarnessConfig, lookupDbmsSpec)
import Occam.Harness.DbmsExec (
    ConnectionOverrides (..),
    defaultContainerLog,
    emptyConnectionOverrides,
    resolveRepoBinary,
    runProcessCapture,
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
import Occam.Harness.Reset (ResetOptions (..), resetDb)
import Occam.Harness.Signature (
    CrashSignature (..),
    DbmsKind (..),
    dbmsKindName,
    isServerDbmsKind,
    parseDbmsKind,
 )
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    makeAbsolute,
    removeDirectoryRecursive,
 )
import System.Environment (getEnvironment)
import System.FilePath (takeFileName, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), Signal, installHandler, sigINT, sigTERM)
import Text.Read (readMaybe)

-- | Options controlling native batch evaluation.
data BatchOptions = BatchOptions
    { boConfigPath :: !FilePath
    , boPlan :: !BatchPlanOptions
    , boDryRun :: !Bool
    , boAggregateOnly :: !Bool
    , boOccamBin :: !(Maybe FilePath)
    , boEngineBin :: !(Maybe FilePath)
    , boHarnessBin :: !(Maybe FilePath)
    , boRunTag :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | Top-level JSON summary printed by @occam-harness batch@.
data BatchSummary = BatchSummary
    { bsExitCode :: !Int
    , bsPayload :: !Value
    }
    deriving (Eq, Show)

-- | Container coordinates tracked for dynamically spawned batch worker containers.
data ManagedContainer = ManagedContainer
    { mcName :: !Text
    , mcHostLog :: !(Maybe FilePath)
    }

-- | Execution backend assigned to an individual batch worker slot.
data WorkerBackend
    = BackendLocalEmbedded !(Maybe FilePath)
    | BackendExternal !ExternalContainer
    | BackendManaged !(Maybe ManagedContainer)

-- | Resolved endpoint attributes bound to a worker's active DBMS instance.
data WorkerEndpointSpec = WorkerEndpointSpec
    { wesContainer :: !(Maybe Text)
    , wesHostLog :: !(Maybe FilePath)
    , wesHost :: !(Maybe Text)
    , wesPort :: !(Maybe Int)
    , wesDbName :: !(Maybe Text)
    }

-- | Mutable per-worker state protected by an 'MVar' inside 'WorkerHandle'.
data WorkerState = WorkerState
    { wsWorkerId :: !Int
    , wsRunTag :: !Text
    , wsOutDir :: !FilePath
    , wsContainerWait :: !Int
    , wsCurrentDbms :: !(Maybe DbmsKind)
    , wsCurrentImage :: !(Maybe Text)
    , wsBackend :: !WorkerBackend
    , wsEndpoint :: !(Maybe WorkerEndpointSpec)
    }

-- | Concurrent worker handle passed across qualification and reduction tasks.
data WorkerHandle = WorkerHandle
    { whState :: !(MVar WorkerState)
    , whSpawned :: !(MVar (Set Text))
    , whHarnessConfig :: !HarnessConfig
    , whStopRef :: !(IORef Bool)
    }

-- | Run native batch evaluation (dry-run, aggregate-only, or full parallel execution).
runBatch :: HarnessConfig -> BatchOptions -> IO (Either String BatchSummary)
runBatch cfg opts
    | boAggregateOnly opts = do
        outAbs <- makeAbsolute (planOutputDir (boPlan opts))
        agg <- aggregateOutDir outAbs
        let payload =
                object
                    [ "aggregated_stats" .= (outAbs </> "aggregated_stats.json")
                    , "overview" .= aggOverview agg
                    ]
        pure (Right (BatchSummary 0 payload))
    | otherwise = runExceptT do
        plan <- ExceptT (prepareBatchPlan (boConfigPath opts) cfg (boPlan opts))
        if boDryRun opts
            then pure (BatchSummary 0 (pbpPreview plan))
            else do
                (occamBin, harnessBin) <- ExceptT (resolveBatchBinaries opts)
                liftIO (executePreparedBatch cfg opts plan occamBin harnessBin)

-- | Locate the @occam@ and @occam-harness@ binaries required for batch execution.
resolveBatchBinaries :: BatchOptions -> IO (Either String (FilePath, FilePath))
resolveBatchBinaries opts = do
    eOccam <- resolveRepoBinary (boOccamBin opts) "OCCAM_BIN" "occam"
    eHarness <- resolveRepoBinary (boHarnessBin opts) "OCCAM_HARNESS_BIN" "occam-harness"
    pure $ (,) <$> eOccam <*> eHarness

-- | Execute qualification and reduction across the prepared batch plan with signal-safe container cleanup.
executePreparedBatch ::
    HarnessConfig ->
    BatchOptions ->
    PreparedBatchPlan ->
    FilePath ->
    FilePath ->
    IO BatchSummary
executePreparedBatch cfg opts plan occamBin harnessBin = do
    let outDir = pbpOutDir plan
        settings = pbpSettings plan
        pOpts = boPlan opts
    runTag <- resolveRunTag (boRunTag opts)
    stopRef <- newIORef False
    sigNumRef <- newIORef Nothing
    spawnedRef <- newMVar Set.empty
    workerErrsRef <- newIORef []
    evalContT do
        ContT (\k -> withSignalHandlers stopRef sigNumRef (k ()))
        ContT (\k -> k () `finally` cleanupSpawnedContainers spawnedRef)
        liftIO do
            workers <- initWorkers cfg opts plan runTag stopRef spawnedRef
            -- Phase 1: Qualification (lazy container startup!)
            priorQuals <- loadPriorQuals outDir pOpts settings
            let missingCases = filter (\bc -> Map.notMember (batchCaseId bc) priorQuals) (pbpCases plan)
            runWorkerPool stopRef workers workerErrsRef missingCases \wh bc ->
                qualifyCaseOnWorker wh opts plan bc
            stopped1 <- readIORef stopRef
            qualsMap <-
                if stopped1
                    then pure priorQuals
                    else
                        if planResume pOpts
                            then
                                selectReusableQualifications
                                    outDir
                                    (planReplays pOpts)
                                    (workStackFrames settings)
                                    (planSkipInputReplay pOpts)
                                    (pbpCases plan)
                            else loadQualMapFromDisk outDir (pbpCases plan)

            -- Phase 2: Reduction + Reduced Replay Verification
            stopped2 <- readIORef stopRef
            unless stopped2 do
                let qualLookup cid = Map.lookup cid qualsMap
                    caseById = Map.fromList [(batchCaseId bc, bc) | bc <- pbpCases plan]
                    jobItems =
                        [ (j, bc, computeRunConfigHash settings pOpts bc prof)
                        | j@(Job cid prof _) <- pbpJobs plan
                        , Just bc <- [Map.lookup cid caseById]
                        ]
                    jobChecks =
                        [ JobCheck
                            { jcJobId = jobId j
                            , jcJob = j
                            , jcCase = bc
                            , jcConfigHash = cfgHash
                            , jcAllowUnstable = False
                            , jcSkipInputReplay = planSkipInputReplay pOpts
                            , jcInputQualified = maybe False qrQualified (qualLookup (batchCaseId bc))
                            }
                        | (j, bc, cfgHash) <- jobItems
                        ]
                reusableIds <-
                    if planResume pOpts
                        then selectReusableJobs outDir (planRerunFailed pOpts) jobChecks
                        else pure Set.empty
                let pendingJobs = filter (\(j, _, _) -> Set.notMember (jobId j) reusableIds) jobItems
                runWorkerPool stopRef workers workerErrsRef pendingJobs \wh (j, bc, cfgHash) ->
                    runJobOnWorker wh opts plan occamBin harnessBin bc j cfgHash (qualLookup (batchCaseId bc))

    interrupted <- readIORef stopRef
    mSig <- readIORef sigNumRef
    workerErrs <- reverse <$> readIORef workerErrsRef
    let workerExCount = length workerErrs
    agg <- aggregateOutDir outDir
    let code
            | interrupted = 128 + fromMaybe 2 mSig
            | workerExCount > 0 = 1
            | otherwise = 0
        payload =
            object
                [ "out_dir" .= outDir
                , "aggregated_stats" .= (outDir </> "aggregated_stats.json")
                , "overview" .= aggOverview agg
                , "interrupted" .= interrupted
                , "signal" .= mSig
                , "worker_exceptions" .= workerExCount
                , "worker_errors" .= workerErrs
                ]
    pure (BatchSummary code payload)
    where
        resolveRunTag (Just tag) | not (T.null tag) = pure tag
        resolveRunTag _ = do
            pid <- getProcessID
            epoch <- floor <$> getPOSIXTime :: IO Integer
            pure ("b" <> T.pack (show epoch) <> "_" <> T.pack (show pid))

        loadPriorQuals outDir pOpts settings
            | planResume pOpts =
                selectReusableQualifications
                    outDir
                    (planReplays pOpts)
                    (workStackFrames settings)
                    (planSkipInputReplay pOpts)
                    (pbpCases plan)
            | otherwise = pure Map.empty

        loadQualMapFromDisk outDir cases = do
            pairs <- forM cases \bc -> do
                let qPath = outDir </> "qualified" </> T.unpack (batchCaseId bc) </> "input_replay.json"
                mQr <- tryReadJson qPath
                pure ((batchCaseId bc,) <$> mQr)
            pure (Map.fromList (catMaybes pairs))

        tryReadJson :: FilePath -> IO (Maybe QualificationResult)
        tryReadJson p = do
            exists <- doesFileExist p
            if not exists
                then pure Nothing
                else do
                    res <- try (BS.readFile p) :: IO (Either IOException BS.ByteString)
                    pure (either (const Nothing) (either (const Nothing) Just . eitherDecodeStrict') res)

-- | Temporarily install SIGINT/SIGTERM handlers setting @stopRef@ and recording the signal number.
withSignalHandlers :: IORef Bool -> IORef (Maybe Int) -> IO a -> IO a
withSignalHandlers stopRef sigNumRef action =
    bracket install restore (const action)
    where
        install = do
            prevInt <- installHandler sigINT (Catch (onSig 2)) Nothing
            prevTerm <- installHandler sigTERM (Catch (onSig 15)) Nothing
            pure (prevInt, prevTerm)
        restore (prevInt, prevTerm) = do
            void (installHandler sigINT prevInt Nothing)
            void (installHandler sigTERM prevTerm Nothing)
        onSig :: Signal -> IO ()
        onSig n = do
            writeIORef sigNumRef (Just (fromIntegral n))
            writeIORef stopRef True

-- | Initialize isolated 'WorkerHandle' slots mapped to embedded, external, or managed backends.
initWorkers ::
    HarnessConfig ->
    BatchOptions ->
    PreparedBatchPlan ->
    Text ->
    IORef Bool ->
    MVar (Set Text) ->
    IO [WorkerHandle]
initWorkers cfg opts plan runTag stopRef spawnedRef = do
    let settings = pbpSettings plan
        pOpts = boPlan opts
        outDir = pbpOutDir plan
        nWorkers = workWorkers settings
        externals = pbpExternalContainers plan
    forM [0 .. nWorkers - 1] \wid -> do
        let mExt = if wid < length externals then Just (externals !! wid) else Nothing
            backend
                | planLocalEmbedded pOpts = BackendLocalEmbedded (boEngineBin opts)
                | Just ext <- mExt = BackendExternal ext
                | otherwise = BackendManaged Nothing
            st =
                WorkerState
                    { wsWorkerId = wid
                    , wsRunTag = runTag
                    , wsOutDir = outDir
                    , wsContainerWait = workContainerWait settings
                    , wsCurrentDbms = Nothing
                    , wsCurrentImage = Nothing
                    , wsBackend = backend
                    , wsEndpoint = Nothing
                    }
        mv <- newMVar st
        pure (WorkerHandle mv spawnedRef cfg stopRef)

-- | Drain @items@ across parallel worker threads, capturing per-worker exceptions into @errsRef@.
runWorkerPool ::
    IORef Bool ->
    [WorkerHandle] ->
    IORef [Text] ->
    [a] ->
    (WorkerHandle -> a -> IO ()) ->
    IO ()
runWorkerPool _ _ _ [] _ = pure ()
runWorkerPool stopRef workers errsRef items action = do
    queue <- newMVar items
    dones <- forM workers \wh -> do
        done <- newEmptyMVar
        _ <- forkIO $ (loop wh queue `catch` onWorkerErr) `finally` putMVar done ()
        pure done
    mapM_ takeMVar dones
    where
        loop wh queue =
            void $ runMaybeT do
                liftIO (readIORef stopRef) >>= guard . not
                item <- MaybeT $ modifyMVar queue \case
                    [] -> pure ([], Nothing)
                    (x : xs) -> pure (xs, Just x)
                liftIO (action wh item)
                liftIO (loop wh queue)

        onWorkerErr :: SomeException -> IO ()
        onWorkerErr exc = atomicModifyIORef' errsRef (\es -> (T.pack (show exc) : es, ()))

abortWorker :: ContT () IO a
abortWorker = ContT \_ -> pure ()

guardNotStopped :: WorkerHandle -> ContT () IO ()
guardNotStopped wh = do
    stopped <- liftIO (readIORef (whStopRef wh))
    when stopped abortWorker

-- | Qualify a single batch input via N-replay and write @qualified/<case_id>/input_replay.json@.
qualifyCaseOnWorker :: WorkerHandle -> BatchOptions -> PreparedBatchPlan -> BatchCase -> IO ()
qualifyCaseOnWorker wh opts plan bc = (`runContT` pure) do
    guardNotStopped wh
    let pOpts = boPlan opts
        settings = pbpSettings plan
        outDir = pbpOutDir plan
        cid = batchCaseId bc
        qualDir = outDir </> "qualified" </> T.unpack cid
        qualPath = qualDir </> "input_replay.json"
        decLog = qualDir </> "input_replay_decisions.jsonl"
    liftIO (createDirectoryIfMissing True qualDir)
    dbmsKind <- liftIO (requireDbmsKind (dbmsId (batchDbms bc)))

    when (planSkipInputReplay pOpts) do
        let rv =
                ReplayVerification
                    { rvRunsRequired = planReplays pOpts
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
                    , qrStackFrames = workStackFrames settings
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
                , rcStackFrames = workStackFrames settings
                , rcDecisionLog = Just decLog
                , rcProbeTimeout = planProbeTimeout pOpts
                , rcMaxWaitReset = workContainerWait settings
                , rcResetBeforeRun = True
                , rcRequireConsistent = True
                }
    rv <-
        liftIO
            (runReplayVerification (whHarnessConfig wh) (Just (whStopRef wh)) rCfg (planReplays pOpts) sqlBytes)
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
                , qrStackFrames = workStackFrames settings
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
    let outDir = pbpOutDir plan
        jobDir = outDir </> "runs" </> T.unpack (jobId j)
        jobResPath = jobDir </> "job_result.json"
        pOpts = boPlan opts
        inputQualified = maybe True qrQualified mQr
        baselineSig = (mQr >>= qrBaselineSignature) `altMaybe` attrBaseline (batchAttributes bc)
        writeResult res = liftIO (writeAtomicLazyBytes jobResPath (encode res <> BSL.pack [10]))

    dbmsKind <- liftIO (requireDbmsKind (dbmsId (batchDbms bc)))
    liftIO (cleanPartialDir jobDir)
    let mkRes = baseJobResult j bc dbmsKind cfgHash inputQualified baselineSig

    when (not (planSkipInputReplay pOpts) && not inputQualified) do
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
        settings = pbpSettings plan
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
            , show (workTimeout settings)
            , "--jobs"
            , show (workOccamJobs settings)
            , "--out"
            , minimalPath
            , "--stats"
            , outStats
            ]
                <> maybe [] (\b -> ["--budget", show b]) (workBudget settings)
                <> maybe [] (\m -> ["--max-oracle-executions", show m]) (workMaxExecutions settings)
                <> ["--no-cache" | workNoCache settings]
                <> map T.unpack (flags prof)
        hardTimeout = (\b -> b + max 60.0 (workTimeout settings * 4.0)) <$> workBudget settings
    childEnv <- liftIO isolatedSubprocessEnv
    t0 <- liftIO getCurrentTime
    (interrupted, timedOut, procRc, _, stderrBs) <-
        liftIO $
            runProcessCaptureCancellable
                True
                (Just (whStopRef wh))
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
            let pOpts = boPlan opts
                settings = pbpSettings plan
            guard (planReplays pOpts > 0)
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
                        , rcStackFrames = workStackFrames settings
                        , rcDecisionLog = Just replayDecLog
                        , rcProbeTimeout = planProbeTimeout pOpts
                        , rcMaxWaitReset = workContainerWait settings
                        , rcResetBeforeRun = True
                        , rcRequireConsistent = True
                        }
            liftIO
                (runReplayVerification (whHarnessConfig wh) (Just (whStopRef wh)) rCfg (planReplays pOpts) sqlBytes)

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
                            <> ["--reject-on-slippage" | workRejectSlippage settings]
                            <> ["--stack-frames", show (workStackFrames settings)]
                            <> maybe [] (\dl -> ["--decision-log", dl]) mDecLog
                    _ -> []
            pure
                (unwords (map shellQuote (harnessBin : sub : "--config" : boConfigPath opts : connArgs <> roleArgs)))

-- | Ensure the worker's backend (local embedded, external container, or managed container) targets @(dbmsKind, image)@.
ensureWorkerTarget :: WorkerHandle -> Bool -> DbmsKind -> DbmsSpec -> Text -> IO ()
ensureWorkerTarget wh forceRecreate dbmsKind dbmsSpec image =
    modifyMVar_ (whState wh) \st ->
        case wsBackend st of
            BackendLocalEmbedded _ ->
                pure $
                    st
                        { wsCurrentDbms = Just dbmsKind
                        , wsCurrentImage = Just "local-embedded"
                        , wsEndpoint = Nothing
                        }
            BackendExternal (ExternalContainer cname host mPort) -> do
                effPort <-
                    if dbmsServerBased dbmsSpec
                        then case mPort of
                            Just p -> pure (Just p)
                            Nothing -> do
                                let defPort = defaultPortFor dbmsKind dbmsSpec
                                pResolved <- resolveDockerPort cname defPort
                                pure (Just (fromMaybe defPort pResolved))
                        else pure Nothing
                let needWait = forceRecreate || wsCurrentDbms st /= Just dbmsKind
                    ep =
                        WorkerEndpointSpec
                            { wesContainer = Just cname
                            , wesHostLog = Nothing
                            , wesHost = if dbmsServerBased dbmsSpec then Just host else Nothing
                            , wesPort = effPort
                            , wesDbName = if dbmsServerBased dbmsSpec then Just "occam_test" else Nothing
                            }
                    nextSt =
                        st
                            { wsCurrentDbms = Just dbmsKind
                            , wsCurrentImage = Just "external"
                            , wsEndpoint = Just ep
                            }
                when needWait $ do
                    eReady <- resetStateEndpoint (whHarnessConfig wh) dbmsKind nextSt (wsContainerWait st)
                    case eReady of
                        Left err -> fail err
                        Right () -> pure ()
                pure nextSt
            BackendManaged (Just _)
                | not forceRecreate
                    && wsCurrentDbms st == Just dbmsKind
                    && wsCurrentImage st == Just image ->
                    pure st
            BackendManaged _ -> spawnManagedContainer wh st dbmsKind dbmsSpec image

-- | Spawn a fresh worker-scoped Docker container with mapped ports and log bind-mount, then wait until ready.
spawnManagedContainer ::
    WorkerHandle -> WorkerState -> DbmsKind -> DbmsSpec -> Text -> IO WorkerState
spawnManagedContainer wh st0 dbmsKind dbmsSpec image = do
    cleanupManaged (wsBackend st0) (whSpawned wh)
    let cname = "occam-eval-" <> wsRunTag st0 <> "-w" <> T.pack (show (wsWorkerId st0))
        (contLogDir, logFileName) = containerLogDirAndFile dbmsKind
        hostLogDir = wsOutDir st0 </> "workers" </> ("worker_" <> show (wsWorkerId st0)) </> "logs"
        hostLogPath = if null logFileName then Nothing else Just (hostLogDir </> logFileName)
    when (dbmsServerBased dbmsSpec && not (null contLogDir)) do
        createDirectoryIfMissing True hostLogDir
        setFileMode hostLogDir 0o777
    _ <- runProcessCapture Nothing Nothing (Just 20.0) "docker" ["rm", "-f", T.unpack cname] BS.empty
    let defPort = defaultPortFor dbmsKind dbmsSpec
        portArgs =
            if dbmsServerBased dbmsSpec
                then ["-p", "127.0.0.1::" <> show defPort]
                else []
        volArgs =
            if dbmsServerBased dbmsSpec && not (null contLogDir)
                then ["-v", hostLogDir <> ":" <> contLogDir <> ":rw"]
                else []
        runArgs = ["run", "-d", "--name", T.unpack cname] <> portArgs <> volArgs <> [T.unpack image]
    modifyMVar_ (whSpawned wh) (pure . Set.insert cname)
    (_, rc, outBs, errBs) <- runProcessCapture Nothing Nothing (Just 60.0) "docker" runArgs BS.empty
    when (rc /= 0) $
        fail
            ( "Worker "
                <> show (wsWorkerId st0)
                <> " failed to start container "
                <> T.unpack cname
                <> ": "
                <> T.unpack (T.strip (TE.decodeUtf8With TEE.lenientDecode (errBs <> outBs)))
            )
    effPort <-
        if dbmsServerBased dbmsSpec
            then Just <$> waitForPort cname defPort
            else pure Nothing
    let managed = ManagedContainer {mcName = cname, mcHostLog = hostLogPath}
        ep =
            WorkerEndpointSpec
                { wesContainer = Just cname
                , wesHostLog = hostLogPath
                , wesHost = if dbmsServerBased dbmsSpec then Just "127.0.0.1" else Nothing
                , wesPort = effPort
                , wesDbName = if dbmsServerBased dbmsSpec then Just "occam_test" else Nothing
                }
        nextSt =
            st0
                { wsCurrentDbms = Just dbmsKind
                , wsCurrentImage = Just image
                , wsBackend = BackendManaged (Just managed)
                , wsEndpoint = Just ep
                }
    eReady <- resetStateEndpoint (whHarnessConfig wh) dbmsKind nextSt (wsContainerWait st0)
    case eReady of
        Left err -> do
            cleanupManaged (wsBackend nextSt) (whSpawned wh)
            fail err
        Right () -> pure nextSt
    where
        waitForPort cname cport = go 0
            where
                go n
                    | n >= (25 :: Int) =
                        fail ("Worker could not resolve mapped port for " <> T.unpack cname <> ":" <> show cport)
                    | otherwise = do
                        mPort <- resolveDockerPort cname cport
                        case mPort of
                            Just p -> pure p
                            Nothing -> do
                                threadDelay 200_000
                                go (n + 1)

        cleanupManaged (BackendManaged (Just managed)) spawnedMv = do
            let oldCname = mcName managed
            _ <- runProcessCapture Nothing Nothing (Just 20.0) "docker" ["rm", "-f", T.unpack oldCname] BS.empty
            modifyMVar_ spawnedMv (pure . Set.delete oldCname)
        cleanupManaged _ _ = pure ()

-- | Force-remove any Docker containers registered in @spawnedMv@ on batch exit or interruption.
cleanupSpawnedContainers :: MVar (Set Text) -> IO ()
cleanupSpawnedContainers spawnedMv = do
    names <- modifyMVar spawnedMv (\s -> pure (Set.empty, Set.toList s))
    mapM_
        ( \cname ->
            void (runProcessCapture Nothing Nothing (Just 20.0) "docker" ["rm", "-f", T.unpack cname] BS.empty)
        )
        names

-- | Query @docker port@ to obtain the host ephemeral port bound to @cport@ in container @cname@.
resolveDockerPort :: Text -> Int -> IO (Maybe Int)
resolveDockerPort cname cport = do
    (_, rc, outBs, _) <-
        runProcessCapture
            Nothing
            Nothing
            (Just 5.0)
            "docker"
            ["port", T.unpack cname, show cport]
            BS.empty
    if rc /= 0
        then pure Nothing
        else do
            let firstLine = case T.lines (TE.decodeUtf8With TEE.lenientDecode outBs) of
                    (l : _) -> T.strip l
                    [] -> ""
                (_, portTxt) = T.breakOnEnd ":" firstLine
            pure (readMaybe (T.unpack portTxt))

-- | Project active worker backend coordinates into 'ConnectionOverrides' for reset and oracle commands.
connectionOverridesForState :: WorkerState -> ConnectionOverrides
connectionOverridesForState st = case wsBackend st of
    BackendLocalEmbedded mEngBin ->
        emptyConnectionOverrides {coEngineBin = mEngBin}
    _ ->
        let mEp = wsEndpoint st
         in emptyConnectionOverrides
                { coHost = mEp >>= wesHost
                , coPort = mEp >>= wesPort
                , coDbName = mEp >>= wesDbName
                , coContainer = mEp >>= wesContainer
                }

-- | Read the live 'ReplayEndpoint' for the given worker, validating host log visibility.
workerEndpoint :: WorkerHandle -> DbmsKind -> IO (Either String ReplayEndpoint)
workerEndpoint wh _dbmsKind = withMVar (whState wh) \st -> do
    mHostLog <- case wsEndpoint st >>= wesHostLog of
        Just p -> doesFileExist p >>= \ok -> pure (if ok then Just p else Nothing)
        Nothing -> pure Nothing
    pure (Right (ReplayEndpoint (connectionOverridesForState st) mHostLog Nothing))

-- | Reset the worker's database, recreating managed containers if database reset fails.
resetWorker :: WorkerHandle -> DbmsKind -> IO (Either String ())
resetWorker wh dbmsKind = do
    st0 <- withMVar (whState wh) pure
    eRes <- resetStateEndpoint (whHarnessConfig wh) dbmsKind st0 (wsContainerWait st0)
    case eRes of
        Right () -> pure (Right ())
        Left err -> case (wsBackend st0, wsCurrentImage st0) of
            (BackendManaged _, Just img) -> do
                let specFallback =
                        fromMaybe
                            DbmsSpec
                                { dbmsId = dbmsKindName dbmsKind
                                , dbmsOccamDialect = dbmsKindName dbmsKind
                                , dbmsSqlancerSubcommand = Nothing
                                , dbmsSqlancerPort = Nothing
                                , dbmsSquirrelDb = Nothing
                                , dbmsContainerImage = img
                                , dbmsContainerLog = Nothing
                                , dbmsServerBased = isServerDbmsKind dbmsKind
                                , dbmsDefaultHost = Nothing
                                , dbmsDefaultPort = Nothing
                                , dbmsDefaultUser = Nothing
                                , dbmsDefaultDb = "occam_test"
                                }
                            (lookupDbmsSpec (whHarnessConfig wh) (dbmsKindName dbmsKind))
                ensureWorkerTarget wh True dbmsKind specFallback img
                st1 <- withMVar (whState wh) pure
                resetStateEndpoint (whHarnessConfig wh) dbmsKind st1 (wsContainerWait st1)
            _ -> pure (Left err)

defaultPortFor :: DbmsKind -> DbmsSpec -> Int
defaultPortFor dbmsKind dbmsSpec = fromMaybe fallback (dbmsDefaultPort dbmsSpec)
    where
        fallback = case dbmsKind of
            DbmsPostgres -> 5432
            DbmsMySql -> 3306
            DbmsMariaDb -> 3306
            DbmsClickHouse -> 9000
            _ -> 0

resetStateEndpoint :: HarnessConfig -> DbmsKind -> WorkerState -> Int -> IO (Either String ())
resetStateEndpoint hc dbmsKind st maxWait =
    resetDb hc (ResetOptions dbmsKind (connectionOverridesForState st) maxWait)

containerLogDirAndFile :: DbmsKind -> (FilePath, FilePath)
containerLogDirAndFile dbms =
    let full = defaultContainerLog dbms
     in if null full
            then ("", "")
            else (takeDirectoryPath full, takeFileName full)
    where
        takeDirectoryPath p =
            let rev = reverse (dropWhile (== '/') p)
                (_, rest) = break (== '/') rev
             in reverse (dropWhile (== '/') rest)

-- | Compute the deterministic 16-hex SHA-256 cache key governing incremental job resume.
computeRunConfigHash :: Settings -> BatchPlanOptions -> BatchCase -> Profile -> Text
computeRunConfigHash settings pOpts bc prof =
    let payload =
            object
                [ "sql_sha256" .= batchSqlSha256 bc
                , "signature" .= batchSignature bc
                , "dialect" .= batchDialect bc
                , "image" .= batchImage bc
                , "timeout" .= workTimeout settings
                , "budget" .= workBudget settings
                , "max_executions" .= workMaxExecutions settings
                , "no_cache" .= workNoCache settings
                , "reject_slippage" .= workRejectSlippage settings
                , "stack_frames" .= workStackFrames settings
                , "occam_jobs" .= workOccamJobs settings
                , "replays" .= planReplays pOpts
                , "skip_input_replay" .= planSkipInputReplay pOpts
                , "local_embedded" .= planLocalEmbedded pOpts
                , "profile" .= profile prof
                , "flags" .= flags prof
                ]
     in TE.decodeUtf8 (BS.take 16 (Base16.encode (SHA256.hash (BSL.toStrict (encode payload)))))

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

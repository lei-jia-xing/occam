{-# LANGUAGE OverloadedRecordDot #-}

-- | Batch planning, resume selection, and top-level orchestration.
module Occam.Harness.Batch.Runner (runBatch) where

import Control.Concurrent (newMVar)
import Control.Exception.Safe (IOException, bracket, finally, try)
import Control.Monad (forM, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (eitherDecodeStrict', encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Occam.Benchmark.BatchReport (AggregatedReport (..), aggregateOutDir)
import Occam.Harness.Batch.Jobs (qualifyCaseOnWorker, runJobOnWorker)
import Occam.Harness.Batch.Types
import Occam.Harness.Batch.Workers (cleanupSpawnedContainers, initWorkers, runWorkerPool)
import Occam.Harness.BatchInputs (BatchCase (..))
import Occam.Harness.BatchPlan (
    BatchPlanOptions (..),
    Job (..),
    PreparedBatchPlan (..),
    Profile (..),
    Settings (..),
    jobId,
    prepareBatchPlan,
 )
import Occam.Harness.Checkpoints (JobCheck (..), selectReusableJobs, selectReusableQualifications)
import Occam.Harness.Config (HarnessConfig)
import Occam.Harness.DbmsExec (resolveRepoBinary)
import Occam.Harness.Replay (QualificationResult (..))
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), Signal, installHandler, sigINT, sigTERM)

-- | Run native batch evaluation (dry-run, aggregate-only, or full parallel execution).
runBatch :: HarnessConfig -> BatchOptions -> IO (Either String BatchSummary)
runBatch cfg opts
    | opts.aggregateOnly = do
        outAbs <- makeAbsolute opts.plan.outputDir
        agg <- aggregateOutDir outAbs
        let payload =
                object
                    [ "aggregated_stats" .= (outAbs </> "aggregated_stats.json")
                    , "overview" .= aggOverview agg
                    ]
        pure (Right (BatchSummary 0 payload))
    | otherwise = runExceptT do
        plan <- ExceptT (prepareBatchPlan opts.configPath cfg opts.plan)
        if opts.dryRun
            then pure (BatchSummary 0 plan.preview)
            else do
                (occamBin, harnessBin) <- ExceptT (resolveBatchBinaries opts)
                liftIO (executePreparedBatch cfg opts plan occamBin harnessBin)

-- | Locate the @occam@ and @occam-harness@ binaries required for batch execution.
resolveBatchBinaries :: BatchOptions -> IO (Either String (FilePath, FilePath))
resolveBatchBinaries opts = do
    eOccam <- resolveRepoBinary opts.occamBin "OCCAM_BIN" "occam"
    eHarness <- resolveRepoBinary opts.harnessBin "OCCAM_HARNESS_BIN" "occam-harness"
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
    let outDir = plan.outDir
        settings = plan.settings
        pOpts = opts.plan
    runTag <- resolveRunTag opts.runTag
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
            let missingCases = filter (\bc -> Map.notMember (batchCaseId bc) priorQuals) plan.cases
            runWorkerPool stopRef workers workerErrsRef missingCases \wh bc ->
                qualifyCaseOnWorker wh opts plan bc
            stopped1 <- readIORef stopRef
            qualsMap <-
                if stopped1
                    then pure priorQuals
                    else
                        if pOpts.resume
                            then
                                selectReusableQualifications
                                    outDir
                                    pOpts.replays
                                    settings.stackFrames
                                    pOpts.skipInputReplay
                                    plan.cases
                            else loadQualMapFromDisk outDir plan.cases

            -- Phase 2: Reduction + Reduced Replay Verification
            stopped2 <- readIORef stopRef
            unless stopped2 do
                let qualLookup cid = Map.lookup cid qualsMap
                    caseById = Map.fromList [(batchCaseId bc, bc) | bc <- plan.cases]
                    jobItems =
                        [ (j, bc, computeRunConfigHash settings pOpts bc prof)
                        | j@(Job cid prof _) <- plan.jobs
                        , Just bc <- [Map.lookup cid caseById]
                        ]
                    jobChecks =
                        [ JobCheck
                            { jcJobId = jobId j
                            , jcJob = j
                            , jcCase = bc
                            , jcConfigHash = cfgHash
                            , jcAllowUnstable = False
                            , jcSkipInputReplay = pOpts.skipInputReplay
                            , jcInputQualified = maybe False qrQualified (qualLookup (batchCaseId bc))
                            }
                        | (j, bc, cfgHash) <- jobItems
                        ]
                reusableIds <-
                    if pOpts.resume
                        then selectReusableJobs outDir pOpts.rerunFailed jobChecks
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
            | pOpts.resume =
                selectReusableQualifications
                    outDir
                    pOpts.replays
                    settings.stackFrames
                    pOpts.skipInputReplay
                    plan.cases
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

-- | Compute the deterministic 16-hex SHA-256 cache key governing incremental job resume.
computeRunConfigHash :: Settings -> BatchPlanOptions -> BatchCase -> Profile -> Text
computeRunConfigHash settings pOpts bc prof =
    let payload =
            object
                [ "sql_sha256" .= batchSqlSha256 bc
                , "signature" .= batchSignature bc
                , "dialect" .= batchDialect bc
                , "image" .= batchImage bc
                , "timeout" .= settings.timeout
                , "budget" .= settings.budget
                , "max_executions" .= settings.maxExecutions
                , "no_cache" .= settings.noCache
                , "reject_slippage" .= settings.rejectSlippage
                , "stack_frames" .= settings.stackFrames
                , "occam_jobs" .= settings.occamJobs
                , "replays" .= pOpts.replays
                , "skip_input_replay" .= pOpts.skipInputReplay
                , "local_embedded" .= pOpts.localEmbedded
                , "profile" .= profile prof
                , "flags" .= flags prof
                ]
     in TE.decodeUtf8 (BS.take 16 (Base16.encode (SHA256.hash (BSL.toStrict (encode payload)))))

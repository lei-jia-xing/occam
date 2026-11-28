{-# LANGUAGE OverloadedRecordDot #-}

-- | Worker backend and container lifecycle for batch execution.
module Occam.Harness.Batch.Workers (
    initWorkers,
    runWorkerPool,
    ensureWorkerTarget,
    cleanupSpawnedContainers,
    workerEndpoint,
    resetWorker,
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
import Control.Exception.Safe (SomeException, catch, finally)
import Control.Monad (forM, guard, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Occam.Harness.Batch.Types
import Occam.Harness.BatchPlan (
    BatchPlanOptions (..),
    ExternalContainer (..),
    PreparedBatchPlan (..),
    Settings (..),
 )
import Occam.Harness.Config (DbmsSpec (..), HarnessConfig, lookupDbmsSpec)
import Occam.Harness.DbmsExec (
    ConnectionOverrides (..),
    defaultContainerLog,
    emptyConnectionOverrides,
    runProcessCapture,
 )
import Occam.Harness.Replay (ReplayEndpoint (..))
import Occam.Harness.Reset (ResetOptions (..), resetDb)
import Occam.Harness.Signature (DbmsKind (..), dbmsKindName, isServerDbmsKind)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeFileName, (</>))
import System.Posix.Files (setFileMode)
import Text.Read (readMaybe)

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
    let settings = plan.settings
        pOpts = opts.plan
        outDir = plan.outDir
        nWorkers = settings.workers
        externals = plan.externalContainers
    forM [0 .. nWorkers - 1] \wid -> do
        let mExt = if wid < length externals then Just (externals !! wid) else Nothing
            backend
                | pOpts.localEmbedded = BackendLocalEmbedded opts.engineBin
                | Just ext <- mExt = BackendExternal ext
                | otherwise = BackendManaged Nothing
            st =
                WorkerState
                    { workerId = wid
                    , runTag = runTag
                    , outDir = outDir
                    , containerWait = settings.containerWait
                    , currentDbms = Nothing
                    , currentImage = Nothing
                    , backend = backend
                    , endpoint = Nothing
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

-- | Ensure the worker's backend (local embedded, external container, or managed container) targets @(dbmsKind, image)@.
ensureWorkerTarget :: WorkerHandle -> Bool -> DbmsKind -> DbmsSpec -> Text -> IO ()
ensureWorkerTarget wh forceRecreate dbmsKind dbmsSpec image =
    modifyMVar_ wh.state \st ->
        case st.backend of
            BackendLocalEmbedded _ ->
                pure $
                    st
                        { currentDbms = Just dbmsKind
                        , currentImage = Just "local-embedded"
                        , endpoint = Nothing
                        }
            BackendExternal (ExternalContainer cname host mPort) -> do
                effPort <-
                    if dbmsSpec.serverBased
                        then case mPort of
                            Just p -> pure (Just p)
                            Nothing -> do
                                let defPort = defaultPortFor dbmsKind dbmsSpec
                                pResolved <- resolveDockerPort cname defPort
                                pure (Just (fromMaybe defPort pResolved))
                        else pure Nothing
                let needWait = forceRecreate || st.currentDbms /= Just dbmsKind
                    ep =
                        WorkerEndpointSpec
                            { container = Just cname
                            , hostLog = Nothing
                            , host = if dbmsSpec.serverBased then Just host else Nothing
                            , port = effPort
                            , dbName = if dbmsSpec.serverBased then Just "occam_test" else Nothing
                            }
                    nextSt =
                        st
                            { currentDbms = Just dbmsKind
                            , currentImage = Just "external"
                            , endpoint = Just ep
                            }
                when needWait $ do
                    eReady <- resetStateEndpoint wh.harnessConfig dbmsKind nextSt st.containerWait
                    case eReady of
                        Left err -> fail err
                        Right () -> pure ()
                pure nextSt
            BackendManaged (Just _)
                | not forceRecreate
                    && st.currentDbms == Just dbmsKind
                    && st.currentImage == Just image ->
                    pure st
            BackendManaged _ -> spawnManagedContainer wh st dbmsKind dbmsSpec image

-- | Spawn a fresh worker-scoped Docker container with mapped ports and log bind-mount, then wait until ready.
spawnManagedContainer ::
    WorkerHandle -> WorkerState -> DbmsKind -> DbmsSpec -> Text -> IO WorkerState
spawnManagedContainer wh st0 dbmsKind dbmsSpec image = do
    cleanupManaged st0.backend wh.spawned
    let cname = "occam-eval-" <> st0.runTag <> "-w" <> T.pack (show st0.workerId)
        (contLogDir, logFileName) = containerLogDirAndFile dbmsKind
        hostLogDir = st0.outDir </> "workers" </> ("worker_" <> show st0.workerId) </> "logs"
        hostLogPath = if null logFileName then Nothing else Just (hostLogDir </> logFileName)
    when (dbmsSpec.serverBased && not (null contLogDir)) do
        createDirectoryIfMissing True hostLogDir
        setFileMode hostLogDir 0o777
    _ <- runProcessCapture Nothing Nothing (Just 20.0) "docker" ["rm", "-f", T.unpack cname] BS.empty
    let defPort = defaultPortFor dbmsKind dbmsSpec
        portArgs =
            if dbmsSpec.serverBased
                then ["-p", "127.0.0.1::" <> show defPort]
                else []
        volArgs =
            if dbmsSpec.serverBased && not (null contLogDir)
                then ["-v", hostLogDir <> ":" <> contLogDir <> ":rw"]
                else []
        runArgs = ["run", "-d", "--name", T.unpack cname] <> portArgs <> volArgs <> [T.unpack image]
    modifyMVar_ wh.spawned (pure . Set.insert cname)
    (_, rc, outBs, errBs) <- runProcessCapture Nothing Nothing (Just 60.0) "docker" runArgs BS.empty
    when (rc /= 0) $
        fail
            ( "Worker "
                <> show st0.workerId
                <> " failed to start container "
                <> T.unpack cname
                <> ": "
                <> T.unpack (T.strip (TE.decodeUtf8With TEE.lenientDecode (errBs <> outBs)))
            )
    effPort <-
        if dbmsSpec.serverBased
            then Just <$> waitForPort cname defPort
            else pure Nothing
    let managed = ManagedContainer {name = cname, hostLog = hostLogPath}
        ep =
            WorkerEndpointSpec
                { container = Just cname
                , hostLog = hostLogPath
                , host = if dbmsSpec.serverBased then Just "127.0.0.1" else Nothing
                , port = effPort
                , dbName = if dbmsSpec.serverBased then Just "occam_test" else Nothing
                }
        nextSt =
            st0
                { currentDbms = Just dbmsKind
                , currentImage = Just image
                , backend = BackendManaged (Just managed)
                , endpoint = Just ep
                }
    eReady <- resetStateEndpoint wh.harnessConfig dbmsKind nextSt st0.containerWait
    case eReady of
        Left err -> do
            cleanupManaged nextSt.backend wh.spawned
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
            let oldCname = managed.name
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
connectionOverridesForState st = case st.backend of
    BackendLocalEmbedded mEngBin ->
        emptyConnectionOverrides {coEngineBin = mEngBin}
    _ ->
        let mEp = st.endpoint
         in emptyConnectionOverrides
                { coHost = mEp >>= \ep -> ep.host
                , coPort = mEp >>= \ep -> ep.port
                , coDbName = mEp >>= \ep -> ep.dbName
                , coContainer = mEp >>= \ep -> ep.container
                }

-- | Read the live 'ReplayEndpoint' for the given worker, validating host log visibility.
workerEndpoint :: WorkerHandle -> DbmsKind -> IO (Either String ReplayEndpoint)
workerEndpoint wh _dbmsKind = withMVar wh.state \st -> do
    mHostLog <- case st.endpoint >>= \ep -> ep.hostLog of
        Just p -> doesFileExist p >>= \ok -> pure (if ok then Just p else Nothing)
        Nothing -> pure Nothing
    pure (Right (ReplayEndpoint (connectionOverridesForState st) mHostLog Nothing))

-- | Reset the worker's database, recreating managed containers if database reset fails.
resetWorker :: WorkerHandle -> DbmsKind -> IO (Either String ())
resetWorker wh dbmsKind = do
    st0 <- withMVar wh.state pure
    eRes <- resetStateEndpoint wh.harnessConfig dbmsKind st0 st0.containerWait
    case eRes of
        Right () -> pure (Right ())
        Left err -> case (st0.backend, st0.currentImage) of
            (BackendManaged _, Just img) -> do
                let specFallback =
                        fromMaybe
                            DbmsSpec
                                { identifier = dbmsKindName dbmsKind
                                , occamDialect = dbmsKindName dbmsKind
                                , sqlancerSubcommand = Nothing
                                , sqlancerPort = Nothing
                                , squirrelDb = Nothing
                                , containerImage = img
                                , containerLog = Nothing
                                , serverBased = isServerDbmsKind dbmsKind
                                , defaultHost = Nothing
                                , defaultPort = Nothing
                                , defaultUser = Nothing
                                , defaultDb = "occam_test"
                                }
                            (lookupDbmsSpec wh.harnessConfig (dbmsKindName dbmsKind))
                ensureWorkerTarget wh True dbmsKind specFallback img
                st1 <- withMVar wh.state pure
                resetStateEndpoint wh.harnessConfig dbmsKind st1 st1.containerWait
            _ -> pure (Left err)

defaultPortFor :: DbmsKind -> DbmsSpec -> Int
defaultPortFor dbmsKind dbmsSpec = fromMaybe fallback dbmsSpec.defaultPort
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

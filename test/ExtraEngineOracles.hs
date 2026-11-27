{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test-only syntax oracles for ClickHouse 26.8, MySQL 8.4, and MariaDB 11.4.
module ExtraEngineOracles (
    ExtraOracles (..),
    MysqlEngine (..),
    MysqlServer,
    MysqlSyntaxError (..),
    isClickHouseAvailable,
    isMySQLAvailable,
    isMariaDBAvailable,
    clickhouseSyntaxCheck,
    clickhouseSyntaxCheckSingle,
    withExtraOracles,
    mysqlSyntaxCheck,
    mysqlSyntaxCheckIsolated,
    mysqlCrossRejectCheck,
    mysqlClientScriptCheck,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, finally)
import Control.Exception.Safe (SomeException, displayException, try)
import Control.Monad (filterM, guard, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, sortOn)
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CUInt (..), CULong (..))
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, nullPtr)
import Occam.Frontend.Span (sliceSpan)
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Parser.Dialect (mariadbProfile, mysqlProfile)
import System.Directory (
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    executable,
    findExecutable,
    getPermissions,
    getTemporaryDirectory,
    listDirectory,
    removeFile,
    removePathForcibly,
 )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), hClose, openFile, openTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    getProcessExitCode,
    proc,
    terminateProcess,
    waitForProcess,
    withCreateProcess,
 )

#if defined(mingw32_HOST_OS)
import Foreign.Ptr (nullFunPtr)

foreign import ccall "windows.h LoadLibraryA"
    c_LoadLibrary :: CString -> IO (Ptr ())
foreign import ccall "windows.h GetProcAddress"
    c_GetProcAddress :: Ptr () -> CString -> IO (FunPtr ())
#else
import System.Posix.DynamicLinker (
    DL,
    RTLDFlags (..),
    dlclose,
    dlopen,
    dlsym,
 )
#endif

data ExtraOracles = ExtraOracles
    { oracleClickHouse :: !Bool
    , oracleMySQL :: !(Maybe MysqlServer)
    , oracleMariaDB :: !(Maybe MysqlServer)
    }

data MysqlEngine
    = EngineMySQL
    | EngineMariaDB
    deriving (Eq, Ord, Show)

data MysqlSyntaxError = MysqlSyntaxError
    { mysqlErrCode :: !Word32
    , mysqlErrMessage :: !Text
    }
    deriving (Eq, Show)

data MYSQL
data MYSQL_RES

type FnMysqlInit = Ptr MYSQL -> IO (Ptr MYSQL)
type FnMysqlRealConnect =
    Ptr MYSQL ->
    CString ->
    CString ->
    CString ->
    CString ->
    CUInt ->
    CString ->
    CULong ->
    IO (Ptr MYSQL)
type FnMysqlRealQuery = Ptr MYSQL -> CString -> CULong -> IO CInt
type FnMysqlErrno = Ptr MYSQL -> IO CUInt
type FnMysqlError = Ptr MYSQL -> IO CString
type FnMysqlStoreResult = Ptr MYSQL -> IO (Ptr MYSQL_RES)
type FnMysqlFreeResult = Ptr MYSQL_RES -> IO ()
type FnMysqlNextResult = Ptr MYSQL -> IO CInt
type FnMysqlClose = Ptr MYSQL -> IO ()

foreign import ccall "dynamic"
    mkMysqlInit :: FunPtr FnMysqlInit -> FnMysqlInit
foreign import ccall "dynamic"
    mkMysqlRealConnect :: FunPtr FnMysqlRealConnect -> FnMysqlRealConnect
foreign import ccall "dynamic"
    mkMysqlRealQuery :: FunPtr FnMysqlRealQuery -> FnMysqlRealQuery
foreign import ccall "dynamic"
    mkMysqlErrno :: FunPtr FnMysqlErrno -> FnMysqlErrno
foreign import ccall "dynamic"
    mkMysqlError :: FunPtr FnMysqlError -> FnMysqlError
foreign import ccall "dynamic"
    mkMysqlStoreResult :: FunPtr FnMysqlStoreResult -> FnMysqlStoreResult
foreign import ccall "dynamic"
    mkMysqlFreeResult :: FunPtr FnMysqlFreeResult -> FnMysqlFreeResult
foreign import ccall "dynamic"
    mkMysqlNextResult :: FunPtr FnMysqlNextResult -> FnMysqlNextResult
foreign import ccall "dynamic"
    mkMysqlClose :: FunPtr FnMysqlClose -> FnMysqlClose

data MariadbAPI = MariadbAPI
    { apiInit :: !FnMysqlInit
    , apiRealConnect :: !FnMysqlRealConnect
    , apiRealQuery :: !FnMysqlRealQuery
    , apiErrno :: !FnMysqlErrno
    , apiError :: !FnMysqlError
    , apiStoreResult :: !FnMysqlStoreResult
    , apiFreeResult :: !FnMysqlFreeResult
    , apiNextResult :: !FnMysqlNextResult
    , apiClose :: !FnMysqlClose
    }

data MysqlServer = MysqlServer
    { srvEngine :: !MysqlEngine
    , srvApi :: !MariadbAPI
    , srvSocket :: !FilePath
    , srvClientBin :: !(Maybe FilePath)
    , srvTmpDir :: !FilePath
    , srvProcess :: !ProcessHandle
    , srvConnLock :: !(MVar (Ptr MYSQL))
    }

clientMultiStatements :: CULong
clientMultiStatements = 0x00010000

{-# NOINLINE mariadbApiCache #-}
mariadbApiCache :: IORef (Maybe (Either String MariadbAPI))
mariadbApiCache = unsafePerformIO (newIORef Nothing)

{-# NOINLINE mariadbApiLock #-}
mariadbApiLock :: MVar ()
mariadbApiLock = unsafePerformIO (newMVar ())

-- ---------------------------------------------------------------------------
-- Availability checks (strict on configured env vars; False when unset)
-- ---------------------------------------------------------------------------

lookupEnvNonEmpty :: String -> IO (Maybe String)
lookupEnvNonEmpty name = do
    mVal <- lookupEnv name
    pure $ case mVal of
        Just v | not (null v) -> Just v
        _ -> Nothing

requireExecutable :: String -> FilePath -> IO FilePath
requireExecutable label path = do
    exists <- doesFileExist path
    unless exists $
        ioError (userError (label <> " points to missing file: " <> path))
    perms <- getPermissions path
    unless (executable perms) $
        ioError (userError (label <> " is not executable: " <> path))
    pure path

findClickHouseBin :: IO (Maybe FilePath)
findClickHouseBin =
    lookupEnvNonEmpty "OCCAM_CLICKHOUSE_BIN" >>= \case
        Just path -> Just <$> requireExecutable "OCCAM_CLICKHOUSE_BIN" path
        Nothing -> findExecutable "clickhouse"

isClickHouseAvailable :: IO Bool
isClickHouseAvailable = isJust <$> findClickHouseBin

findMySQLBins :: IO (Maybe (FilePath, Maybe FilePath))
findMySQLBins =
    lookupEnvNonEmpty "OCCAM_MYSQLD_BIN" >>= \case
        Nothing -> do
            mCli <- lookupEnvNonEmpty "OCCAM_MYSQL_CLIENT_BIN"
            when (isJust mCli) $
                ioError (userError "OCCAM_MYSQL_CLIENT_BIN is set without OCCAM_MYSQLD_BIN")
            pure Nothing
        Just mysqldRaw -> do
            mysqld <- requireExecutable "OCCAM_MYSQLD_BIN" mysqldRaw
            mCliEnv <- lookupEnvNonEmpty "OCCAM_MYSQL_CLIENT_BIN"
            client <- case mCliEnv of
                Just p -> Just <$> requireExecutable "OCCAM_MYSQL_CLIENT_BIN" p
                Nothing -> do
                    let sibling = takeDirectory mysqld </> "mysql"
                    exists <- doesFileExist sibling
                    pure (if exists then Just sibling else Nothing)
            _ <- requireMariadbAPI
            pure (Just (mysqld, client))

isMySQLAvailable :: IO Bool
isMySQLAvailable = isJust <$> findMySQLBins

findMariaDBBins :: IO (Maybe (FilePath, FilePath, Maybe FilePath))
findMariaDBBins =
    lookupEnvNonEmpty "OCCAM_MARIADBD_BIN" >>= \case
        Nothing -> do
            mInst <- lookupEnvNonEmpty "OCCAM_MARIADB_INSTALL_DB_BIN"
            when (isJust mInst) $
                ioError (userError "OCCAM_MARIADB_INSTALL_DB_BIN is set without OCCAM_MARIADBD_BIN")
            pure Nothing
        Just mariadbdRaw -> do
            mariadbd <- requireExecutable "OCCAM_MARIADBD_BIN" mariadbdRaw
            mInstEnv <- lookupEnvNonEmpty "OCCAM_MARIADB_INSTALL_DB_BIN"
            installDb <- case mInstEnv of
                Just p -> requireExecutable "OCCAM_MARIADB_INSTALL_DB_BIN" p
                Nothing ->
                    requireExecutable
                        "mariadb-install-db (sibling of OCCAM_MARIADBD_BIN)"
                        (takeDirectory mariadbd </> "mariadb-install-db")
            mCliEnv <- lookupEnvNonEmpty "OCCAM_MARIADB_CLIENT_BIN"
            client <- case mCliEnv of
                Just p -> Just <$> requireExecutable "OCCAM_MARIADB_CLIENT_BIN" p
                Nothing -> do
                    let sibling = takeDirectory mariadbd </> "mariadb"
                    exists <- doesFileExist sibling
                    pure (if exists then Just sibling else Nothing)
            _ <- requireMariadbAPI
            pure (Just (mariadbd, installDb, client))

isMariaDBAvailable :: IO Bool
isMariaDBAvailable = isJust <$> findMariaDBBins

-- ---------------------------------------------------------------------------
-- Dynamic loading of libmariadb.so.3 (zero C shim)
-- ---------------------------------------------------------------------------

findMariadbLibPath :: IO (Either String FilePath)
findMariadbLibPath = do
    mExplicit <- lookupEnvNonEmpty "OCCAM_MARIADB_LIB"
    case mExplicit of
        Just path -> do
            exists <- doesFileExist path
            if exists
                then pure (Right path)
                else pure (Left ("OCCAM_MARIADB_LIB points to missing file: " <> path))
        Nothing -> do
            mMariadbd <- lookupEnvNonEmpty "OCCAM_MARIADBD_BIN"
            mDetected <-
                runMaybeT $
                    MaybeT (findSiblingLib mMariadbd)
                        <|> MaybeT findSystemMariadbLib
            pure $ case mDetected of
                Just p -> Right p
                Nothing ->
                    Left
                        "Could not locate libmariadb.so.3. Set OCCAM_MARIADB_LIB or OCCAM_MARIADBD_BIN."
    where
        findSiblingLib Nothing = pure Nothing
        findSiblingLib (Just binPath) = do
            let prefix = takeDirectory (takeDirectory binPath)
                candidates =
                    [ prefix </> "lib" </> libName
                    | libName <- platformLibNames
                    ]
            filterM doesFileExist candidates >>= \case
                p : _ -> pure (Just p)
                [] -> pure Nothing

platformLibNames :: [FilePath]
#if defined(mingw32_HOST_OS)
platformLibNames = ["libmariadb.dll"]
#elif defined(darwin_HOST_OS)
platformLibNames = ["libmariadb.3.dylib", "libmariadb.dylib"]
#else
platformLibNames = ["libmariadb.so.3", "libmariadb.so"]
#endif

findSystemMariadbLib :: IO (Maybe FilePath)
#if defined(mingw32_HOST_OS)
findSystemMariadbLib = pure (Just "libmariadb.dll")
#elif defined(darwin_HOST_OS)
findSystemMariadbLib =
    runMaybeT $
        tryBareLib "libmariadb.3.dylib"
            <|> tryBareLib "libmariadb.dylib"
            <|> findInNixStore "libmariadb.3.dylib"
#else
findSystemMariadbLib =
    runMaybeT $
        tryBareLib "libmariadb.so.3"
            <|> tryBareLib "libmariadb.so"
            <|> findInNixStore "libmariadb.so.3"
#endif

#if !defined(mingw32_HOST_OS)
tryBareLib :: FilePath -> MaybeT IO FilePath
tryBareLib libName = MaybeT do
    mBare <- try (dlopen libName [RTLD_LAZY]) :: IO (Either SomeException DL)
    case mBare of
        Right dl -> dlclose dl >> pure (Just libName)
        Left _ -> pure Nothing

findInNixStore :: FilePath -> MaybeT IO FilePath
findInNixStore soname = do
    let nixStore = "/nix/store"
    guard =<< liftIO (doesDirectoryExist nixStore)
    entries <- liftIO (listDirectory nixStore)
    let matches =
            sortOn Down
                [ nixStore </> e </> "lib" </> soname
                | e <- entries
                , "-mariadb-" `isInfixOf` e
                ]
    MaybeT $
        filterM doesFileExist matches >>= \case
            f : _ -> pure (Just f)
            [] -> pure Nothing
#endif

loadMariadbAPI :: IO (Either String MariadbAPI)
loadMariadbAPI =
    findMariadbLibPath >>= \case
        Left err -> pure (Left err)
        Right libPath -> do
            res <- try (loadSymbols libPath) :: IO (Either SomeException MariadbAPI)
            pure $ case res of
                Left err -> Left (displayException err)
                Right api -> Right api

#if defined(mingw32_HOST_OS)
loadSymbols :: FilePath -> IO MariadbAPI
loadSymbols libPath = withCString libPath \cLib -> do
    hMod <- c_LoadLibrary cLib
    if hMod == nullPtr
        then fail ("LoadLibrary failed for " <> libPath)
        else do
            let getProc :: String -> IO (FunPtr a)
                getProc name = withCString name \cName -> do
                    p <- c_GetProcAddress hMod cName
                    if p == nullFunPtr
                        then fail ("GetProcAddress failed for symbol: " <> name)
                        else pure (castFunPtr p)
            buildAPI getProc
#else
loadSymbols :: FilePath -> IO MariadbAPI
loadSymbols libPath = do
    dl <- dlopen libPath [RTLD_LAZY]
    let getSym :: String -> IO (FunPtr a)
        getSym name = castFunPtr <$> dlsym dl name
    buildAPI getSym
#endif

buildAPI :: (forall a. String -> IO (FunPtr a)) -> IO MariadbAPI
buildAPI getSym = do
    apiInit <- mkMysqlInit <$> getSym "mysql_init"
    apiRealConnect <- mkMysqlRealConnect <$> getSym "mysql_real_connect"
    apiRealQuery <- mkMysqlRealQuery <$> getSym "mysql_real_query"
    apiErrno <- mkMysqlErrno <$> getSym "mysql_errno"
    apiError <- mkMysqlError <$> getSym "mysql_error"
    apiStoreResult <- mkMysqlStoreResult <$> getSym "mysql_store_result"
    apiFreeResult <- mkMysqlFreeResult <$> getSym "mysql_free_result"
    apiNextResult <- mkMysqlNextResult <$> getSym "mysql_next_result"
    apiClose <- mkMysqlClose <$> getSym "mysql_close"
    pure MariadbAPI {..}

getMariadbAPI :: IO (Either String MariadbAPI)
getMariadbAPI =
    withMVar mariadbApiLock \_ -> do
        cached <- readIORef mariadbApiCache
        case cached of
            Just res -> pure res
            Nothing -> do
                res <- loadMariadbAPI
                writeIORef mariadbApiCache (Just res)
                pure res

requireMariadbAPI :: IO MariadbAPI
requireMariadbAPI =
    getMariadbAPI >>= \case
        Right api -> pure api
        Left msg -> ioError (userError ("libmariadb.so.3 unavailable: " <> msg))

-- ---------------------------------------------------------------------------
-- ClickHouse syntax oracle
-- ---------------------------------------------------------------------------

clickhouseSyntaxCheck :: ByteString -> IO (Either Text ())
clickhouseSyntaxCheck = runClickHouseFormat ["--quiet", "--multiquery"]

clickhouseSyntaxCheckSingle :: ByteString -> IO (Either Text ())
clickhouseSyntaxCheckSingle = runClickHouseFormat ["--quiet"]

runClickHouseFormat :: [String] -> ByteString -> IO (Either Text ())
runClickHouseFormat args sql
    | BS.elem 0 sql = pure (Left "NUL byte in SQL input")
    | otherwise = do
        chBin <-
            findClickHouseBin
                >>= maybe
                    (ioError (userError "ClickHouse binary not found; set OCCAM_CLICKHOUSE_BIN"))
                    pure
        (code, _out, err) <- runBinaryStdin chBin ("format" : args) sql
        pure $ case code of
            ExitSuccess -> Right ()
            ExitFailure _ ->
                let decoded = T.strip (TE.decodeUtf8With lenientDecode err)
                 in Left (if T.null decoded then "clickhouse format rejected SQL" else decoded)

-- ---------------------------------------------------------------------------
-- Temporary unix-socket MySQL 8.4 & MariaDB 11.4 lifecycle
-- ---------------------------------------------------------------------------

withExtraOracles :: (ExtraOracles -> IO a) -> IO a
withExtraOracles action = do
    chAvail <- isClickHouseAvailable
    mMySQL <- findMySQLBins
    mMariaDB <- findMariaDBBins
    bracket
        (startServers mMySQL mMariaDB)
        stopServers
        (\(srvMy, srvMa) -> action (ExtraOracles chAvail srvMy srvMa))
    where
        startServers Nothing Nothing = pure (Nothing, Nothing)
        startServers (Just mySpec) Nothing = do
            srvMy <- startMySQLServer mySpec
            pure (Just srvMy, Nothing)
        startServers Nothing (Just maSpec) = do
            srvMa <- startMariaDBServer maSpec
            pure (Nothing, Just srvMa)
        startServers (Just mySpec) (Just maSpec) = do
            vMy <- newEmptyMVar
            vMa <- newEmptyMVar
            _ <- forkIO (try (startMySQLServer mySpec) >>= putMVar vMy)
            _ <- forkIO (try (startMariaDBServer maSpec) >>= putMVar vMa)
            eMy <- takeMVar vMy :: IO (Either SomeException MysqlServer)
            eMa <- takeMVar vMa :: IO (Either SomeException MysqlServer)
            case (eMy, eMa) of
                (Right sMy, Right sMa) -> pure (Just sMy, Just sMa)
                (Right sMy, Left err) -> stopServer sMy >> ioError (userError (displayException err))
                (Left err, Right sMa) -> stopServer sMa >> ioError (userError (displayException err))
                (Left err1, Left err2) ->
                    ioError (userError (displayException err1 <> "; " <> displayException err2))

        stopServers (mSrvMy, mSrvMa) =
            mapM_ stopServer mSrvMy `finally` mapM_ stopServer mSrvMa

createShortTempDir :: String -> IO FilePath
createShortTempDir prefix = do
    hasTmp <- doesDirectoryExist "/tmp"
    base <- if hasTmp then pure "/tmp" else getTemporaryDirectory
    (path, handle) <- openTempFile base prefix
    hClose handle
    removeFile path
    createDirectory path
    pure path

startMySQLServer :: (FilePath, Maybe FilePath) -> IO MysqlServer
startMySQLServer (mysqldBin, mClientBin) = do
    api <- requireMariadbAPI
    tmpDir <- createShortTempDir "occam-mysql"
    let dataDir = tmpDir </> "data"
        sockPath = tmpDir </> "s.sock"
        pidPath = tmpDir </> "p.pid"
        errLog = tmpDir </> "err.log"
        baseDir = takeDirectory (takeDirectory mysqldBin)
    createDirectory dataDir
    let initFailCleanup = removePathForcibly tmpDir
    (initCode, initOut, initErr) <-
        runBinaryStdin
            mysqldBin
            [ "--initialize-insecure"
            , "--datadir=" <> dataDir
            , "--basedir=" <> baseDir
            ]
            BS.empty
    when (initCode /= ExitSuccess) $ do
        initFailCleanup
        ioError
            ( userError
                ( "mysqld --initialize-insecure failed: "
                    <> BSC.unpack (initOut <> initErr)
                )
            )
    launchAndWaitServer
        EngineMySQL
        api
        tmpDir
        sockPath
        errLog
        mClientBin
        mysqldBin
        [ "--datadir=" <> dataDir
        , "--basedir=" <> baseDir
        , "--socket=" <> sockPath
        , "--pid-file=" <> pidPath
        , "--skip-networking"
        , "--skip-log-bin"
        , "--log-error=" <> errLog
        ]

startMariaDBServer :: (FilePath, FilePath, Maybe FilePath) -> IO MysqlServer
startMariaDBServer (mariadbdBin, installDbBin, mClientBin) = do
    api <- requireMariadbAPI
    tmpDir <- createShortTempDir "occam-maria"
    let dataDir = tmpDir </> "data"
        sockPath = tmpDir </> "s.sock"
        pidPath = tmpDir </> "p.pid"
        errLog = tmpDir </> "err.log"
        baseDir = takeDirectory (takeDirectory mariadbdBin)
    createDirectory dataDir
    (initCode, initOut, initErr) <-
        runBinaryStdin
            installDbBin
            [ "--datadir=" <> dataDir
            , "--basedir=" <> baseDir
            , "--auth-root-authentication-method=normal"
            , "--skip-test-db"
            ]
            BS.empty
    when (initCode /= ExitSuccess) $ do
        removePathForcibly tmpDir
        ioError
            ( userError
                ( "mariadb-install-db failed: "
                    <> BSC.unpack (initOut <> initErr)
                )
            )
    launchAndWaitServer
        EngineMariaDB
        api
        tmpDir
        sockPath
        errLog
        mClientBin
        mariadbdBin
        [ "--datadir=" <> dataDir
        , "--basedir=" <> baseDir
        , "--socket=" <> sockPath
        , "--pid-file=" <> pidPath
        , "--skip-networking"
        , "--log-error=" <> errLog
        ]

launchAndWaitServer ::
    MysqlEngine ->
    MariadbAPI ->
    FilePath ->
    FilePath ->
    FilePath ->
    Maybe FilePath ->
    FilePath ->
    [String] ->
    IO MysqlServer
launchAndWaitServer engine api tmpDir sockPath errLog mClientBin binPath args = do
    devNullIn <- openFile "/dev/null" ReadMode
    devNullOut <- openFile "/dev/null" WriteMode
    devNullErr <- openFile "/dev/null" WriteMode
    let cp =
            (proc binPath args)
                { std_in = UseHandle devNullIn
                , std_out = UseHandle devNullOut
                , std_err = UseHandle devNullErr
                }
    (_, _, _, ph) <-
        createProcess cp
            `finally` (hClose devNullIn >> hClose devNullOut >> hClose devNullErr)
    let cleanupOnError = do
            terminateProcess ph
            _ <- waitForProcess ph
            removePathForcibly tmpDir
    eConn <- try (waitForSocketConnect api sockPath errLog ph) :: IO (Either SomeException (Ptr MYSQL))
    case eConn of
        Left err -> cleanupOnError >> ioError (userError (displayException err))
        Right conn -> do
            let setupSql =
                    "CREATE DATABASE IF NOT EXISTS occam_syntax; "
                        <> "USE occam_syntax; "
                        <> "CREATE TABLE t (a INT PRIMARY KEY, b INT); "
                        <> "CREATE TABLE u (a INT PRIMARY KEY, b INT); "
                        <> "SET NAMES utf8mb4;"
            eInit <- execRawQuery [] engine api conn setupSql
            case eInit of
                Left synErr -> do
                    apiClose api conn
                    cleanupOnError
                    ioError (userError ("Failed to initialize occam_syntax: " <> show synErr))
                Right () -> do
                    lock <- newMVar conn
                    pure (MysqlServer engine api sockPath mClientBin tmpDir ph lock)

waitForSocketConnect ::
    MariadbAPI ->
    FilePath ->
    FilePath ->
    ProcessHandle ->
    IO (Ptr MYSQL)
waitForSocketConnect api sockPath errLog ph = go (300 :: Int)
    where
        go 0 = do
            logContents <- readErrorLog errLog
            ioError
                ( userError
                    ( "Timed out waiting for unix socket "
                        <> sockPath
                        <> "\nServer log:\n"
                        <> logContents
                    )
                )
        go remaining = do
            mExit <- getProcessExitCode ph
            case mExit of
                Just ec -> do
                    logContents <- readErrorLog errLog
                    ioError
                        ( userError
                            ( "Database server exited early ("
                                <> show ec
                                <> ") before socket ready:\n"
                                <> logContents
                            )
                        )
                Nothing -> do
                    sockExists <- doesFileExist sockPath
                    if not sockExists
                        then threadDelay 50000 >> go (remaining - 1)
                        else do
                            mConn <- tryConnectSocket api sockPath
                            case mConn of
                                Right conn -> pure conn
                                Left _ -> threadDelay 50000 >> go (remaining - 1)

tryConnectSocket :: MariadbAPI -> FilePath -> IO (Either String (Ptr MYSQL))
tryConnectSocket api sockPath = do
    mysqlPtr <- apiInit api nullPtr
    if mysqlPtr == nullPtr
        then pure (Left "mysql_init returned NULL")
        else withCString "root" \cUser ->
            withCString sockPath \cSock -> do
                connected <-
                    apiRealConnect
                        api
                        mysqlPtr
                        nullPtr
                        cUser
                        nullPtr
                        nullPtr
                        0
                        cSock
                        clientMultiStatements
                if connected /= nullPtr
                    then pure (Right mysqlPtr)
                    else do
                        msg <- peekCString =<< apiError api mysqlPtr
                        apiClose api mysqlPtr
                        pure (Left msg)

readErrorLog :: FilePath -> IO String
readErrorLog path = do
    exists <- doesFileExist path
    if exists
        then BSC.unpack <$> BS.readFile path
        else pure "(no error log)"

stopServer :: MysqlServer -> IO ()
stopServer srv = do
    _ <- try (withMVar (srvConnLock srv) (apiClose (srvApi srv))) :: IO (Either SomeException ())
    _ <- try (terminateProcess (srvProcess srv)) :: IO (Either SomeException ())
    _ <- try (waitForProcess (srvProcess srv)) :: IO (Either SomeException ExitCode)
    removePathForcibly (srvTmpDir srv)

-- ---------------------------------------------------------------------------
-- Query execution & error classification
-- ---------------------------------------------------------------------------

mysqlSyntaxCheck :: MysqlServer -> ByteString -> IO (Either MysqlSyntaxError ())
mysqlSyntaxCheck srv sql =
    withMVar (srvConnLock srv) \conn ->
        execRawQuery [] (srvEngine srv) (srvApi srv) conn sql

mysqlSyntaxCheckIsolated :: MysqlServer -> ByteString -> IO (Either MysqlSyntaxError ())
mysqlSyntaxCheckIsolated = mysqlSyntaxCheckIsolatedWith []

{- | Cross-dialect rejection check for MySQL/MariaDB. On MariaDB, bison resolves
unknown column data types mid-parse (@4161: Unknown data type@) before
reaching trailing clauses such as ClickHouse @PROJECTION@ / @ENGINE=MergeTree@.
-}
mysqlCrossRejectCheck :: MysqlServer -> ByteString -> IO (Either MysqlSyntaxError ())
mysqlCrossRejectCheck srv =
    mysqlSyntaxCheckIsolatedWith
        (case srvEngine srv of EngineMariaDB -> [4161]; EngineMySQL -> [])
        srv

mysqlSyntaxCheckIsolatedWith ::
    [Word32] -> MysqlServer -> ByteString -> IO (Either MysqlSyntaxError ())
mysqlSyntaxCheckIsolatedWith extraCodes srv sql =
    withMVar (srvConnLock srv) \conn -> do
        let engine = srvEngine srv
            api = srvApi srv
            profile = case engine of
                EngineMySQL -> mysqlProfile
                EngineMariaDB -> mariadbProfile
            resetReadOnly =
                when (engine == EngineMySQL) $
                    voidRight
                        (execRawQuery [] engine api conn "SET GLOBAL super_read_only = OFF; SET GLOBAL read_only = OFF;")
            setupIsolated =
                "DROP DATABASE IF EXISTS occam_case; "
                    <> "CREATE DATABASE occam_case; "
                    <> "USE occam_case; "
                    <> "CREATE TABLE t (a INT PRIMARY KEY, b INT); "
                    <> "CREATE TABLE u (a INT PRIMARY KEY, b INT);"
            restoreDefault =
                resetReadOnly
                    >> voidRight
                        (execRawQuery [] engine api conn "ROLLBACK; DROP DATABASE IF EXISTS occam_case; USE occam_syntax;")
        resetReadOnly
        voidRight (execRawQuery [] engine api conn setupIsolated)
        ( case splitSQL profile sql of
                Left lexErr ->
                    pure (Left (MysqlSyntaxError 1064 (T.pack (show lexErr))))
                Right units ->
                    let sqlUnits = [u | u <- units, isNothing (splitUnitDelimiterDirective u)]
                     in runUnitsSequential engine api conn sqlUnits
            )
            `finally` restoreDefault
    where
        voidRight act =
            act >>= \case
                Right () -> pure ()
                Left err ->
                    ioError (userError ("Unexpected setup/cleanup syntax error: " <> show err))
        runUnitsSequential _ _ _ [] = pure (Right ())
        runUnitsSequential engine api conn (u : us) =
            case sliceSpan (splitUnitSpan u) sql of
                Nothing -> pure (Left (MysqlSyntaxError 1064 "invalid unit span"))
                Just unitSql -> do
                    res <- execRawQuery extraCodes engine api conn unitSql
                    case res of
                        Left err -> pure (Left err)
                        Right () -> runUnitsSequential engine api conn us

mysqlClientScriptCheck :: MysqlServer -> ByteString -> IO (Either Text ())
mysqlClientScriptCheck srv scriptSql =
    case srvClientBin srv of
        Nothing ->
            ioError (userError "MySQL/MariaDB client binary unavailable for DELIMITER verification")
        Just clientBin ->
            withMVar (srvConnLock srv) \conn -> do
                let engine = srvEngine srv
                    api = srvApi srv
                    cleanup =
                        execRawQuery [] engine api conn "DROP DATABASE IF EXISTS occam_cli_case; USE occam_syntax;"
                _ <-
                    execRawQuery
                        []
                        engine
                        api
                        conn
                        "DROP DATABASE IF EXISTS occam_cli_case; CREATE DATABASE occam_cli_case;"
                ( do
                        (code, out, err) <-
                            runBinaryStdin
                                clientBin
                                ["-S", srvSocket srv, "-u", "root", "--batch", "occam_cli_case"]
                                scriptSql
                        let combined = out <> err
                        pure $
                            if code /= ExitSuccess || "ERROR " `BS.isInfixOf` combined
                                then Left (T.strip (TE.decodeUtf8With lenientDecode combined))
                                else Right ()
                    )
                    `finally` cleanup

execRawQuery ::
    [Word32] ->
    MysqlEngine ->
    MariadbAPI ->
    Ptr MYSQL ->
    ByteString ->
    IO (Either MysqlSyntaxError ())
execRawQuery extraCodes engine api conn sql
    | BS.null sql = pure (Left (MysqlSyntaxError 1065 "Query was empty"))
    | BS.elem 0 sql = pure (Left (MysqlSyntaxError 1064 "NUL byte in SQL input"))
    | otherwise =
        BS.useAsCStringLen sql \(ptr, len) -> do
            rc <- apiRealQuery api conn ptr (fromIntegral len)
            if rc /= 0
                then classifyCurrentError extraCodes engine api conn
                else drainResults extraCodes engine api conn

drainResults ::
    [Word32] ->
    MysqlEngine ->
    MariadbAPI ->
    Ptr MYSQL ->
    IO (Either MysqlSyntaxError ())
drainResults extraCodes engine api conn = do
    resPtr <- apiStoreResult api conn
    when (resPtr /= nullPtr) (apiFreeResult api resPtr)
    nr <- apiNextResult api conn
    case nr of
        -1 -> pure (Right ())
        0 -> drainResults extraCodes engine api conn
        _ -> classifyCurrentError extraCodes engine api conn

classifyCurrentError ::
    [Word32] ->
    MysqlEngine ->
    MariadbAPI ->
    Ptr MYSQL ->
    IO (Either MysqlSyntaxError ())
classifyCurrentError extraCodes engine api conn = do
    code <- fromIntegral <$> apiErrno api conn
    msg <- T.pack <$> (peekCString =<< apiError api conn)
    when (code >= 2000 && code < 3000) $
        ioError (userError ("MySQL/MariaDB transport error " <> show code <> ": " <> T.unpack msg))
    let isSyntax =
            code `elem` (1064 : 1065 : 1149 : extraCodes)
                || (engine == EngineMySQL && code `elem` [6036, 6037])
    pure $
        if isSyntax
            then Left (MysqlSyntaxError code msg)
            else Right ()

-- ---------------------------------------------------------------------------
-- Subprocess helper (strict ByteString stdin/stdout/stderr, deadlock-free)
-- ---------------------------------------------------------------------------

runBinaryStdin :: FilePath -> [String] -> ByteString -> IO (ExitCode, ByteString, ByteString)
runBinaryStdin binPath args inputBytes =
    let cp =
            (proc binPath args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
     in withCreateProcess cp \mIn mOut mErr ph -> case (mIn, mOut, mErr) of
            (Just hIn, Just hOut, Just hErr) -> do
                outVar <- newEmptyMVar
                errVar <- newEmptyMVar
                _ <- forkIO (BS.hGetContents hOut >>= putMVar outVar)
                _ <- forkIO (BS.hGetContents hErr >>= putMVar errVar)
                BS.hPut hIn inputBytes `finally` hClose hIn
                outBytes <- takeMVar outVar
                errBytes <- takeMVar errVar
                code <- waitForProcess ph
                pure (code, outBytes, errBytes)
            _ -> ioError (userError ("Failed to create pipes for " <> binPath))

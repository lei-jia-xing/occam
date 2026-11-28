-- | Shared DBMS connection resolution, client lookup, and subprocess runner for Reset and Oracle.
module Occam.Harness.DbmsExec (
    ClientRole (..),
    ConnectionOverrides (..),
    DbmsConnection (..),
    buildDbmsCommand,
    containerRunning,
    decodeExitCode,
    defaultContainerLog,
    emptyConnectionOverrides,
    ensureContainerStarted,
    findBinaryIn,
    isSafeSqlIdent,
    resolveDbmsConnection,
    resolveRepoBinary,
    runProcessCapture,
    runProcessCaptureCancellable,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Exception.Safe (IOException, bracketOnError, catch, finally, throwIO, try)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Foldable qualified as Foldable
import Data.IORef (IORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time (addUTCTime, getCurrentTime)
import GHC.IO.Exception (IOErrorType (ResourceBusy))
import Occam.Harness.Config (DbmsSpec (..), HarnessConfig, lookupDbmsSpec)
import Occam.Harness.Signature (DbmsKind (..), dbmsKindName)
import System.Directory (
    doesFileExist,
    findExecutable,
    getHomeDirectory,
    getPermissions,
 )
import System.Directory qualified as Dir
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isPathSeparator, joinPath, splitDirectories, takeDirectory, (</>))
import System.IO (Handle, hClose)
import System.IO.Error (ioeGetErrorType)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess, signalProcessGroup)
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    getPid,
    proc,
    terminateProcess,
    waitForProcess,
 )
import System.Timeout (timeout)
import Text.Read (readMaybe)

-- | Purpose of a DBMS client invocation across reset and oracle probes.
data ClientRole
    = RolePing
    | RoleReset
    | RoleProbe
    deriving (Eq, Show)

-- | Optional CLI overrides before falling back to DBMS environment vars and Dhall specs.
data ConnectionOverrides = ConnectionOverrides
    { coHost :: !(Maybe Text)
    , coPort :: !(Maybe Int)
    , coUser :: !(Maybe Text)
    , coPassword :: !(Maybe Text)
    , coDbName :: !(Maybe Text)
    , coTemplateDb :: !(Maybe Text)
    , coSocket :: !(Maybe FilePath)
    , coContainer :: !(Maybe Text)
    , coEngineBin :: !(Maybe FilePath)
    }
    deriving (Eq, Show)

-- | Empty set of overrides (all values resolved from environment or HarnessConfig).
emptyConnectionOverrides :: ConnectionOverrides
emptyConnectionOverrides =
    ConnectionOverrides
        { coHost = Nothing
        , coPort = Nothing
        , coUser = Nothing
        , coPassword = Nothing
        , coDbName = Nothing
        , coTemplateDb = Nothing
        , coSocket = Nothing
        , coContainer = Nothing
        , coEngineBin = Nothing
        }

-- | Fully resolved target DBMS connection and runtime location.
data DbmsConnection = DbmsConnection
    { connDbms :: !DbmsKind
    , connHost :: !Text
    , connPort :: !Int
    , connUser :: !Text
    , connPassword :: !Text
    , connDbName :: !Text
    , connTemplateDb :: !Text
    , connContainerLog :: !FilePath
    , connSocket :: !(Maybe FilePath)
    , connContainer :: !(Maybe Text)
    , connEngineBin :: !(Maybe FilePath)
    }
    deriving (Eq, Show)

-- | Verify a SQL identifier for engines without psql variable quoting (@^[A-Za-z0-9_]+$@).
isSafeSqlIdent :: Text -> Bool
isSafeSqlIdent txt =
    not (T.null txt)
        && T.all (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c == '_') txt

-- | Resolve connection defaults cleanly using Dhall @DbmsSpec@, DBMS-isolated env vars, and CLI overrides.
resolveDbmsConnection :: HarnessConfig -> DbmsKind -> ConnectionOverrides -> IO DbmsConnection
resolveDbmsConnection cfg dbms overrides = do
    envPairs <- getEnvironment
    let envMap = Map.fromList envPairs
        spec = lookupDbmsSpec cfg (dbmsKindName dbms)
        (hostKeys, portKeys, userKeys, pwdKeys, dbKeys, sockKeys, binKeys) = dbmsEnvKeys dbms
        firstNonEmpty keys = Foldable.asum [nonEmptyText (T.pack v) | k <- keys, Just v <- [Map.lookup k envMap]]
        firstPresent keys = Foldable.asum [T.pack <$> Map.lookup k envMap | k <- keys]
        firstPort keys = Foldable.asum [readPositiveInt v | k <- keys, Just v <- [Map.lookup k envMap]]
        defHost = fromMaybe (fallbackHost dbms) (spec >>= dbmsDefaultHost >>= nonEmptyText)
        defPort = fromMaybe (fallbackPort dbms) (spec >>= dbmsDefaultPort)
        defUser = fromMaybe (fallbackUser dbms) (spec >>= dbmsDefaultUser >>= nonEmptyText)
        defDb = fromMaybe (fallbackDb dbms) (spec >>= nonEmptyText . dbmsDefaultDb)
        defLog = maybe (defaultContainerLog dbms) T.unpack (dbmsContainerLog =<< spec)
    pure
        DbmsConnection
            { connDbms = dbms
            , connHost = fromMaybe defHost ((coHost overrides >>= nonEmptyText) <|> firstNonEmpty hostKeys)
            , connPort = fromMaybe defPort (coPort overrides <|> firstPort portKeys)
            , connUser = fromMaybe defUser ((coUser overrides >>= nonEmptyText) <|> firstNonEmpty userKeys)
            , connPassword = fromMaybe "" (coPassword overrides <|> firstPresent pwdKeys)
            , connDbName = fromMaybe defDb ((coDbName overrides >>= nonEmptyText) <|> firstNonEmpty dbKeys)
            , connTemplateDb =
                fromMaybe "template1" ((coTemplateDb overrides >>= nonEmptyText) <|> firstNonEmpty ["TEMPLATE_DB"])
            , connContainerLog = defLog
            , connSocket = (coSocket overrides >>= nonEmptyPath) <|> (T.unpack <$> firstNonEmpty sockKeys)
            , connContainer = (coContainer overrides >>= nonEmptyText) <|> firstNonEmpty ["CONTAINER_NAME"]
            , connEngineBin = (coEngineBin overrides >>= nonEmptyPath) <|> (T.unpack <$> firstNonEmpty binKeys)
            }
    where
        nonEmptyText t = if T.null t then Nothing else Just t
        nonEmptyPath p = if null p then Nothing else Just p
        readPositiveInt s = case readMaybe s of
            Just n | n > 0 -> Just n
            _ -> Nothing

-- | Default in-container server error/diagnostic log path for a DBMS engine.
defaultContainerLog :: DbmsKind -> FilePath
defaultContainerLog = \case
    DbmsPostgres -> "/var/log/postgresql/postgresql.log"
    DbmsMySql -> "/var/log/mysql/error.log"
    DbmsMariaDb -> "/var/log/mysql/error.log"
    DbmsClickHouse -> "/var/log/clickhouse-server/clickhouse-server.err.log"
    DbmsSqlite -> ""
    DbmsDuckDb -> ""

-- | Default TCP hostname when connecting to a DBMS server without overrides or env vars.
fallbackHost :: DbmsKind -> Text
fallbackHost = \case
    DbmsSqlite -> ""
    DbmsDuckDb -> ""
    _ -> "127.0.0.1"

-- | Default TCP port for a DBMS server, or @0@ for embedded engines.
fallbackPort :: DbmsKind -> Int
fallbackPort = \case
    DbmsPostgres -> 5432
    DbmsMySql -> 3306
    DbmsMariaDb -> 3306
    DbmsClickHouse -> 9000
    DbmsSqlite -> 0
    DbmsDuckDb -> 0

-- | Default client username for a DBMS server, or empty for embedded engines.
fallbackUser :: DbmsKind -> Text
fallbackUser = \case
    DbmsPostgres -> "postgres"
    DbmsMySql -> "root"
    DbmsMariaDb -> "root"
    DbmsClickHouse -> "default"
    DbmsSqlite -> ""
    DbmsDuckDb -> ""

-- | Default database identifier for a DBMS engine (@occam_test@ for servers, @:memory:@ for embedded).
fallbackDb :: DbmsKind -> Text
fallbackDb = \case
    DbmsSqlite -> ":memory:"
    DbmsDuckDb -> ":memory:"
    _ -> "occam_test"

-- | DBMS-specific environment variable lookup keys @(host, port, user, password, db, socket, bin)@.
dbmsEnvKeys :: DbmsKind -> ([String], [String], [String], [String], [String], [String], [String])
dbmsEnvKeys = \case
    DbmsPostgres ->
        (["PGHOST"], ["PGPORT"], ["PGUSER"], ["PGPASSWORD"], ["PGDATABASE"], [], [])
    DbmsMySql ->
        ( ["MYSQL_HOST"]
        , ["MYSQL_TCP_PORT"]
        , ["MYSQL_USER"]
        , ["MYSQL_PWD"]
        , ["MYSQL_DATABASE"]
        , ["MYSQL_SOCKET"]
        , []
        )
    DbmsMariaDb ->
        ( ["MARIADB_HOST", "MYSQL_HOST"]
        , ["MARIADB_TCP_PORT", "MYSQL_TCP_PORT"]
        , ["MARIADB_USER", "MYSQL_USER"]
        , ["MARIADB_PWD", "MYSQL_PWD"]
        , ["MARIADB_DATABASE", "MYSQL_DATABASE"]
        , ["MARIADB_UNIX_PORT", "MYSQL_SOCKET"]
        , []
        )
    DbmsClickHouse ->
        ( ["CLICKHOUSE_HOST"]
        , ["CLICKHOUSE_PORT"]
        , ["CLICKHOUSE_USER"]
        , ["CLICKHOUSE_PASSWORD"]
        , ["CLICKHOUSE_DATABASE", "CLICKHOUSE_DB"]
        , []
        , []
        )
    DbmsSqlite ->
        ([], [], [], [], [], [], ["SQLITE_BIN"])
    DbmsDuckDb ->
        ([], [], [], [], [], [], ["DUCKDB_BIN"])

-- | Find the first executable matching @names@ on @PATH@, or in explicit @candidates@.
findBinaryIn :: [String] -> [FilePath] -> IO (Maybe FilePath)
findBinaryIn names candidates = do
    onPath <- Foldable.asum <$> mapM findExecutable names
    case onPath of
        Just found -> pure (Just found)
        Nothing -> findFirstCandidate candidates

findFirstCandidate :: [FilePath] -> IO (Maybe FilePath)
findFirstCandidate [] = pure Nothing
findFirstCandidate (raw : rest) = do
    path <- expandTilde raw
    ok <- isExecutableFile path
    if ok then pure (Just path) else findFirstCandidate rest

expandTilde :: FilePath -> IO FilePath
expandTilde ('~' : '/' : rest) = (</> rest) <$> getHomeDirectory
expandTilde path = pure path

isExecutableFile :: FilePath -> IO Bool
isExecutableFile path = do
    exists <- doesFileExist path
    if not exists
        then pure False
        else
            either (const False) Dir.executable
                <$> (try (getPermissions path) :: IO (Either IOException Dir.Permissions))

resolveExplicitOr :: Maybe FilePath -> [String] -> [FilePath] -> IO (Maybe FilePath)
resolveExplicitOr (Just p) _ _
    | any isPathSeparator p = do
        p' <- expandTilde p
        ok <- isExecutableFile p'
        pure (if ok then Just p' else Nothing)
    | otherwise = findExecutable p
resolveExplicitOr Nothing names candidates = findBinaryIn names candidates

-- | Construct the client executable, CLI arguments, and child environment map for a DBMS role.
buildDbmsCommand ::
    DbmsConnection ->
    ClientRole ->
    Map String String ->
    IO (Either String (FilePath, [String], [(String, String)]))
buildDbmsCommand conn role baseEnv = case connDbms conn of
    DbmsPostgres -> buildPostgres
    DbmsMySql -> buildMysqlLike False
    DbmsMariaDb -> buildMysqlLike True
    DbmsClickHouse -> buildClickHouse
    DbmsSqlite -> buildEmbedded "sqlite3" ["/usr/local/bin/sqlite3", "/usr/bin/sqlite3"]
    DbmsDuckDb -> buildEmbedded "duckdb" ["~/.local/bin/duckdb", "/usr/local/bin/duckdb", "/usr/bin/duckdb"]
    where
        host = T.unpack (connHost conn)
        portStr = show (connPort conn)
        user = T.unpack (connUser conn)
        pwd = T.unpack (connPassword conn)
        db = T.unpack (connDbName conn)
        tmpl = T.unpack (connTemplateDb conn)

        withPwdEnv key m
            | null pwd = m
            | otherwise = Map.insert key pwd m

        buildPostgres = case role of
            RolePing -> do
                mBin <-
                    findBinaryIn
                        ["pg_isready"]
                        (pgBinCandidates "pg_isready")
                pure $ case mBin of
                    Just bin -> Right (bin, ["-h", host, "-p", portStr, "-U", user], Map.toList baseEnv)
                    Nothing -> Left "pg_isready executable not found"
            RoleReset -> do
                mBin <- findBinaryIn ["psql"] (pgBinCandidates "psql")
                pure $ case mBin of
                    Just bin ->
                        Right
                            ( bin
                            ,
                                [ "-h"
                                , host
                                , "-p"
                                , portStr
                                , "-U"
                                , user
                                , "-d"
                                , "template1"
                                , "-v"
                                , "ON_ERROR_STOP=1"
                                , "-v"
                                , "dbname=" <> db
                                , "-v"
                                , "template=" <> tmpl
                                , "-X"
                                , "-q"
                                ]
                            , Map.toList (withPwdEnv "PGPASSWORD" baseEnv)
                            )
                    Nothing -> Left "psql executable not found"
            RoleProbe -> do
                mBin <- findBinaryIn ["psql"] (pgBinCandidates "psql")
                let bin = fromMaybe "psql" mBin
                pure $
                    Right
                        ( bin
                        , ["-h", host, "-p", portStr, "-U", user, "-d", db, "-v", "ON_ERROR_STOP=0", "-X", "-q"]
                        , Map.toList (withPwdEnv "PGPASSWORD" baseEnv)
                        )

        pgBinCandidates tool =
            [ "~/.cache/occam/postgresql-17.0-debug/install/bin/" <> tool
            , "~/.cache/occam/postgresql-13-debug/install/bin/" <> tool
            , "/usr/local/pgsql/bin/" <> tool
            , "/usr/lib/postgresql/17/bin/" <> tool
            , "/usr/lib/postgresql/16/bin/" <> tool
            , "/usr/lib/postgresql/15/bin/" <> tool
            , "/usr/lib/postgresql/14/bin/" <> tool
            , "/usr/lib/postgresql/13/bin/" <> tool
            , "/usr/lib/postgresql/12/bin/" <> tool
            , "/usr/bin/" <> tool
            ]

        mysqlConnArgs = case connSocket conn of
            Just sock -> ["--socket", sock]
            Nothing -> ["-h", host, "-P", portStr]

        buildMysqlLike isMaria = case role of
            RolePing -> do
                let (names, cands)
                        | isMaria =
                            ( ["mariadb-admin", "mysqladmin"]
                            ,
                                [ "/usr/local/mariadb/bin/mariadb-admin"
                                , "/usr/local/mysql/bin/mariadb-admin"
                                , "/usr/local/mysql/bin/mysqladmin"
                                , "/usr/bin/mariadb-admin"
                                , "/usr/bin/mysqladmin"
                                ]
                            )
                        | otherwise = (["mysqladmin"], ["/usr/local/mysql/bin/mysqladmin", "/usr/bin/mysqladmin"])
                mBin <- findBinaryIn names cands
                pure $ case mBin of
                    Just bin ->
                        Right
                            ( bin
                            , ["--no-defaults"] <> mysqlConnArgs <> ["-u", user, "--silent", "ping"]
                            , Map.toList (withPwdEnv "MYSQL_PWD" baseEnv)
                            )
                    Nothing ->
                        Left
                            ( if isMaria
                                then "mariadb-admin/mysqladmin executable not found"
                                else "mysqladmin executable not found"
                            )
            _ -> do
                let (names, defaultBin, cands)
                        | isMaria =
                            ( ["mariadb", "mysql"]
                            , "mariadb"
                            ,
                                [ "/usr/local/mariadb/bin/mariadb"
                                , "/usr/local/mysql/bin/mariadb"
                                , "/usr/local/mysql/bin/mysql"
                                , "/usr/bin/mariadb"
                                , "/usr/bin/mysql"
                                ]
                            )
                        | otherwise = (["mysql"], "mysql", ["/usr/local/mysql/bin/mysql", "/usr/bin/mysql"])
                mBin <- findBinaryIn names cands
                let bin = fromMaybe defaultBin mBin
                    suffixArgs
                        | role == RoleProbe = ["-u", user, "-D", db, "--force", "-s"]
                        | otherwise = ["-u", user]
                if role == RoleReset && isNothing mBin
                    then
                        pure (Left (if isMaria then "mariadb/mysql executable not found" else "mysql executable not found"))
                    else
                        pure $
                            Right
                                ( bin
                                , ["--no-defaults"] <> ["--binary-mode" | role == RoleProbe] <> mysqlConnArgs <> suffixArgs
                                , Map.toList (withPwdEnv "MYSQL_PWD" baseEnv)
                                )

        buildClickHouse = do
            mBin <-
                findBinaryIn
                    ["clickhouse-client"]
                    ["/usr/bin/clickhouse-client", "/usr/local/bin/clickhouse-client"]
            let envList = Map.toList (withPwdEnv "CLICKHOUSE_PASSWORD" baseEnv)
                suffix = case role of
                    RolePing -> ["--query", "SELECT 1"]
                    RoleReset -> ["--multiquery"]
                    RoleProbe -> ["--database", db, "--multiquery", "--ignore-error"]
            case (mBin, connContainer conn, role) of
                (Just bin, _, _) ->
                    pure $ Right (bin, ["--host", host, "--port", portStr, "--user", user] <> suffix, envList)
                (Nothing, Just cname, _) ->
                    pure $
                        Right
                            ( "docker"
                            , ["exec", "-i"]
                                <> (if null pwd then [] else ["-e", "CLICKHOUSE_PASSWORD"])
                                <> [T.unpack cname, "clickhouse-client", "--user", user]
                                <> suffix
                            , envList
                            )
                (Nothing, Nothing, RoleProbe) ->
                    pure $
                        Right ("clickhouse-client", ["--host", host, "--port", portStr, "--user", user] <> suffix, envList)
                (Nothing, Nothing, _) ->
                    pure (Left "clickhouse-client not found on host and no --container-name fallback available")

        buildEmbedded defaultName cands = case connContainer conn of
            Just cname ->
                pure $
                    Right
                        ( "docker"
                        , ["exec", "-i", T.unpack cname, fromMaybe "occam-exec" (connEngineBin conn)]
                        , Map.toList baseEnv
                        )
            Nothing -> do
                mBin <- resolveExplicitOr (connEngineBin conn) [defaultName] cands
                let bin = fromMaybe defaultName mBin
                    sanEnv =
                        Map.insertWith
                            (\_ old -> old)
                            "ASAN_OPTIONS"
                            "detect_leaks=0:abort_on_error=1:disable_coredump=1:symbolize=1"
                            . Map.insertWith (\_ old -> old) "UBSAN_OPTIONS" "halt_on_error=1:abort_on_error=1:print_stacktrace=1"
                            $ baseEnv
                pure $ Right (bin, ["-batch", ":memory:"], Map.toList sanEnv)

-- | Check whether a Docker container is currently listed as running via @docker ps@.
containerRunning :: Text -> IO Bool
containerRunning cname = do
    (_, rc, outBytes, _) <-
        runProcessCapture
            Nothing
            Nothing
            (Just 10.0)
            "docker"
            ["ps", "--filter", "name=^" <> T.unpack cname <> "$", "--format", "{{.Names}}"]
            BS.empty
    let outTxt = TE.decodeUtf8With TEE.lenientDecode outBytes
    pure (rc == 0 && cname `elem` map T.strip (T.lines outTxt))

-- | Attempt @docker start <cname>@ if @containerRunning@ is False.
ensureContainerStarted :: Text -> IO ()
ensureContainerStarted cname = do
    running <- containerRunning cname
    if running
        then pure ()
        else do
            _ <- runProcessCapture Nothing Nothing (Just 30.0) "docker" ["start", T.unpack cname] BS.empty
            pure ()

-- | Decode POSIX ExitCode into standard shell exit status (signal @s@ -> @128 + s@).
decodeExitCode :: ExitCode -> Int
decodeExitCode ExitSuccess = 0
decodeExitCode (ExitFailure c)
    | c < 0 = 128 - c
    | otherwise = c

-- | Locate an executable by explicit path, env var, sibling of current binary, PATH, or @cabal list-bin@.
resolveRepoBinary :: Maybe FilePath -> String -> String -> IO (Either String FilePath)
resolveRepoBinary (Just explicit) _ binName = runExceptT do
    p' <- liftIO (expandTilde explicit)
    ok <- liftIO (isExecutableFile p')
    if ok
        then pure p'
        else throwE (binName <> " executable not found or not executable: " <> explicit)
resolveRepoBinary Nothing envVar binName = runExceptT do
    mEnv <- liftIO (lookupEnv envVar)
    case mEnv of
        Just p | not (null p) -> do
            p' <- liftIO (expandTilde p)
            ok <- liftIO (isExecutableFile p')
            if ok
                then pure p'
                else throwE (envVar <> " points to missing/non-executable binary: " <> p)
        _ -> do
            mFound <-
                liftIO $
                    runMaybeT $
                        findViaSiblings <|> MaybeT (findExecutable binName) <|> findViaCabal
            maybe
                ( throwE
                    ("Could not locate " <> binName <> " executable; pass --" <> binName <> "-bin or set " <> envVar)
                )
                pure
                mFound
    where
        findViaSiblings = do
            exe <- liftIO getExecutablePath
            let dirs = splitDirectories exe
                candidates =
                    [ takeDirectory exe </> binName
                    , takeDirectory (takeDirectory (takeDirectory exe)) </> binName </> binName
                    , joinPath (replaceSub binName dirs)
                    , "dist" </> "build" </> binName </> binName
                    ]
            MaybeT (findFirstCandidate candidates)

        findViaCabal = do
            (_, rc, outBs, _) <-
                liftIO $
                    runProcessCapture Nothing Nothing (Just 15.0) "cabal" ["list-bin", "exe:" <> binName] BS.empty
            guard (rc == 0)
            let listed =
                    dropWhile
                        isSpace
                        (reverse (dropWhile isSpace (reverse (T.unpack (TE.decodeUtf8With TEE.lenientDecode outBs)))))
            guard (not (null listed))
            ok <- liftIO (isExecutableFile listed)
            guard ok
            pure listed

        replaceSub _ [] = []
        replaceSub target ("t" : _ : rest) = "x" : target : replaceSub target rest
        replaceSub target ("x" : _ : rest) = "x" : target : replaceSub target rest
        replaceSub target [cur, exeName] | cur == exeName = [target, target]
        replaceSub target (x : xs) = x : replaceSub target xs

-- | Execute a subprocess without creating a new process group (preserving parent timeout reaping).
runProcessCapture ::
    Maybe FilePath ->
    Maybe [(String, String)] ->
    Maybe Double ->
    FilePath ->
    [String] ->
    BS.ByteString ->
    IO (Bool, Int, BS.ByteString, BS.ByteString)
runProcessCapture mCwd mEnv mTimeout cmd args inputBytes = do
    (_, timedOut, rc, outBs, errBs) <-
        runProcessCaptureCancellable False Nothing mCwd mEnv mTimeout cmd args inputBytes
    pure (timedOut, rc, outBs, errBs)

-- | Execute a subprocess with optional @IORef Bool@ cancellation flag and optional process-group isolation.
runProcessCaptureCancellable ::
    Bool ->
    Maybe (IORef Bool) ->
    Maybe FilePath ->
    Maybe [(String, String)] ->
    Maybe Double ->
    FilePath ->
    [String] ->
    BS.ByteString ->
    IO (Bool, Bool, Int, BS.ByteString, BS.ByteString)
runProcessCaptureCancellable createNewGroup mStopRef mCwd mEnv mTimeout cmd args inputBytes =
    bracketOnError
        ( createProcessRetryBusy
            (proc cmd args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , cwd = mCwd
                , env = mEnv
                , close_fds = True
                , create_group = createNewGroup
                }
        )
        cleanupHandles
        runWithHandles
    where
        createProcessRetryBusy ::
            CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
        createProcessRetryBusy cp = go (5 :: Int)
            where
                go remaining = do
                    res <- try (createProcess cp)
                    case res of
                        Right handles -> pure handles
                        Left (exc :: IOException)
                            | remaining > 0 && ioeGetErrorType exc == ResourceBusy -> do
                                threadDelay 5000
                                go (remaining - 1)
                            | otherwise -> throwIO exc

        cleanupHandles (mIn, mOut, mErr, ph) = do
            mapM_ closeQuiet [mIn, mOut, mErr]
            reapProcess ph `catch` ignoreIo

        closeQuiet = maybe (pure ()) (\h -> hClose h `catch` ignoreIo)

        ignoreIo :: IOException -> IO ()
        ignoreIo _ = pure ()

        runWithHandles (Just hIn, Just hOut, Just hErr, ph) = do
            outVar <- newEmptyMVar
            errVar <- newEmptyMVar
            exitVar <- newEmptyMVar
            _ <- forkIO $ (BS.hPut hIn inputBytes `catch` ignoreIo) `finally` closeQuiet (Just hIn)
            _ <- forkIO $ do
                bs <- BS.hGetContents hOut `catch` (\(_ :: IOException) -> pure BS.empty)
                putMVar outVar bs
            _ <- forkIO $ do
                bs <- BS.hGetContents hErr `catch` (\(_ :: IOException) -> pure BS.empty)
                putMVar errVar bs
            let posTimeout = case mTimeout of
                    Just s | s > 0 -> Just s
                    _ -> Nothing
            case (mStopRef, posTimeout) of
                (Nothing, Nothing) -> do
                    ec <- waitForProcess ph
                    outBs <- takeMVar outVar
                    errBs <- takeMVar errVar
                    pure (False, False, decodeExitCode ec, outBs, errBs)
                _ -> do
                    _ <- forkIO (waitForProcess ph >>= putMVar exitVar)
                    now <- getCurrentTime
                    let mDeadline = (\s -> addUTCTime (realToFrac s) now) <$> posTimeout
                    outcome <- pollWait exitVar mDeadline
                    case outcome of
                        Right ec -> do
                            outBs <- takeMVar outVar
                            errBs <- takeMVar errVar
                            pure (False, False, decodeExitCode ec, outBs, errBs)
                        Left wasInterrupted -> do
                            reapProcess ph
                            mapM_ closeQuiet [Just hOut, Just hErr]
                            outBs <- fromMaybe BS.empty <$> timeout 50_000 (takeMVar outVar)
                            errBs <- fromMaybe BS.empty <$> timeout 50_000 (takeMVar errVar)
                            pure (wasInterrupted, not wasInterrupted, if wasInterrupted then 130 else 124, outBs, errBs)
        runWithHandles handles = do
            cleanupHandles handles
            pure (False, False, 127, BS.empty, BS.empty)

        pollWait exitVar mDeadline = do
            stopped <- maybe (pure False) readIORef mStopRef
            if stopped
                then pure (Left True)
                else do
                    mEc <- tryTakeMVar exitVar
                    case mEc of
                        Just ec -> pure (Right ec)
                        Nothing -> do
                            timedOut <- case mDeadline of
                                Nothing -> pure False
                                Just deadline -> (>= deadline) <$> getCurrentTime
                            if timedOut
                                then pure (Left False)
                                else do
                                    threadDelay 25_000
                                    pollWait exitVar mDeadline

        reapProcess :: ProcessHandle -> IO ()
        reapProcess ph = do
            mPid <- getPid ph
            if createNewGroup
                then mapM_ (\pid -> signalProcessGroup sigTERM pid `catch` ignoreIo) mPid
                else terminateProcess ph `catch` ignoreIo
            mExited <- timeout 200_000 (waitForProcess ph) `catch` (\(_ :: IOException) -> pure Nothing)
            case mExited of
                Just _ -> pure ()
                Nothing -> do
                    if createNewGroup
                        then mapM_ (\pid -> signalProcessGroup sigKILL pid `catch` ignoreIo) mPid
                        else mapM_ (\pid -> signalProcess sigKILL pid `catch` ignoreIo) mPid
                    _ <- timeout 200_000 (waitForProcess ph) `catch` (\(_ :: IOException) -> pure Nothing)
                    pure ()

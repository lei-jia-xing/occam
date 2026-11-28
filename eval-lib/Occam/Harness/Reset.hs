-- | Haskell-native target DBMS database reset for Occam (@occam-harness reset@).
module Occam.Harness.Reset (
    ResetOptions (..),
    defaultResetOptions,
    resetDb,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Occam.Harness.Config (HarnessConfig)
import Occam.Harness.DbmsExec (
    ClientRole (..),
    ConnectionOverrides,
    DbmsConnection (..),
    buildDbmsCommand,
    containerRunning,
    emptyConnectionOverrides,
    ensureContainerStarted,
    isSafeSqlIdent,
    resolveDbmsConnection,
    runProcessCapture,
 )
import Occam.Harness.Signature (DbmsKind (..), dbmsKindName)
import System.Environment (getEnvironment)

-- | Configuration for @occam-harness reset@.
data ResetOptions = ResetOptions
    { roDbms :: !DbmsKind
    -- ^ Target database engine to reset.
    , roConnection :: !ConnectionOverrides
    -- ^ Host, port, credential, or container overrides merged with defaults.
    , roMaxWaitSeconds :: !Int
    -- ^ Maximum seconds to wait for container/server readiness before failing.
    }
    deriving (Eq, Show)

-- | Default 'ResetOptions' with a 30-second readiness timeout and empty connection overrides.
defaultResetOptions :: DbmsKind -> ResetOptions
defaultResetOptions dbms =
    ResetOptions
        { roDbms = dbms
        , roConnection = emptyConnectionOverrides
        , roMaxWaitSeconds = 30
        }

-- | Reset target database state cleanly before an oracle probe or batch evaluation phase.
resetDb :: HarnessConfig -> ResetOptions -> IO (Either String ())
resetDb cfg opts = runExceptT do
    conn <- liftIO (resolveDbmsConnection cfg (roDbms opts) (roConnection opts))
    case connDbms conn of
        DbmsSqlite -> resetEmbedded conn
        DbmsDuckDb -> resetEmbedded conn
        dbms -> resetServer dbms conn
    where
        maxWait = max 1 (roMaxWaitSeconds opts)
        maxAttempts = maxWait * 5

        waitForReady :: String -> IO Bool -> IO () -> ExceptT String IO ()
        waitForReady timeoutMsg probeReady onRetry = go 1
            where
                go attempt = do
                    ready <- liftIO probeReady
                    unless ready do
                        when (attempt >= maxAttempts) (throwE timeoutMsg)
                        liftIO onRetry
                        liftIO (threadDelay 200_000)
                        go (attempt + 1)

        resetEmbedded conn = case connContainer conn of
            Nothing -> pure ()
            Just cname -> waitForReady (timeoutMsg cname) (probe cname) (ensureContainerStarted cname)
            where
                timeoutMsg cname =
                    "[reset] Error: Embedded container "
                        <> T.unpack cname
                        <> " not ready after "
                        <> show maxWait
                        <> "s"
                probe cname = do
                    running <- containerRunning cname
                    if not running
                        then pure False
                        else do
                            (_, rc, _, _) <-
                                runProcessCapture
                                    Nothing
                                    Nothing
                                    (Just 10.0)
                                    "docker"
                                    ["exec", "-i", T.unpack cname, fromMaybe "occam-exec" (connEngineBin conn)]
                                    "SELECT 1;\n"
                            pure (rc == 0)

        resetServer dbms conn = do
            when (dbms /= DbmsPostgres && not (isSafeSqlIdent (connDbName conn))) $
                throwE
                    ( "[reset] Error: --dbname '"
                        <> T.unpack (connDbName conn)
                        <> "' must match ^[A-Za-z0-9_]+$"
                    )
            envMap <- Map.fromList <$> liftIO getEnvironment
            (pingCmd, pingArgs, pingEnv) <- cmdFor conn RolePing envMap
            (resetCmd, resetArgs, resetEnv) <- cmdFor conn RoleReset envMap
            waitForReady
                (serverTimeoutMsg dbms conn)
                (probePing pingCmd pingArgs pingEnv)
                (mapM_ ensureContainerStarted (connContainer conn))
            let sqlBytes = TE.encodeUtf8 (resetSqlFor conn)
            (_, rc, _, errBs) <-
                liftIO $
                    runProcessCapture
                        Nothing
                        (Just resetEnv)
                        (Just 30.0)
                        resetCmd
                        resetArgs
                        sqlBytes
            when (rc /= 0) $
                throwE
                    ( "[reset] Error: "
                        <> T.unpack (dbmsKindName dbms)
                        <> " reset failed (exit "
                        <> show rc
                        <> "): "
                        <> T.unpack (T.strip (TE.decodeUtf8With TEE.lenientDecode errBs))
                    )

        cmdFor conn role envMap =
            ExceptT (first ("[reset] Error: " <>) <$> buildDbmsCommand conn role envMap)

        probePing pingCmd pingArgs pingEnv = do
            (_, rc, _, _) <-
                runProcessCapture
                    Nothing
                    (Just pingEnv)
                    (Just 5.0)
                    pingCmd
                    pingArgs
                    BS.empty
            pure (rc == 0)

        serverTimeoutMsg dbms conn =
            "[reset] Error: "
                <> T.unpack (dbmsKindName dbms)
                <> " not ready at "
                <> T.unpack (connHost conn)
                <> ":"
                <> show (connPort conn)
                <> " after "
                <> show maxWait
                <> "s"

-- | Engine-specific SQL snippet executed during a clean server database reset.
resetSqlFor :: DbmsConnection -> Text
resetSqlFor conn = case connDbms conn of
    DbmsPostgres ->
        T.unlines
            [ "SELECT pg_terminate_backend(pid)"
            , "FROM pg_stat_activity"
            , "WHERE datname = :'dbname' AND pid <> pg_backend_pid();"
            , ""
            , "DROP DATABASE IF EXISTS :\"dbname\";"
            , "CREATE DATABASE :\"dbname\" TEMPLATE :\"template\";"
            , "GRANT ALL PRIVILEGES ON DATABASE :\"dbname\" TO PUBLIC;"
            ]
    DbmsMySql ->
        T.unlines
            [ "SET GLOBAL super_read_only = OFF;"
            , "SET GLOBAL read_only = OFF;"
            , "DROP DATABASE IF EXISTS `" <> connDbName conn <> "`;"
            , "CREATE DATABASE `" <> connDbName conn <> "`;"
            ]
    DbmsMariaDb ->
        T.unlines
            [ "SET GLOBAL read_only = OFF;"
            , "DROP DATABASE IF EXISTS `" <> connDbName conn <> "`;"
            , "CREATE DATABASE `" <> connDbName conn <> "`;"
            ]
    DbmsClickHouse ->
        T.unlines
            [ "DROP DATABASE IF EXISTS `" <> connDbName conn <> "` SYNC;"
            , "CREATE DATABASE `" <> connDbName conn <> "`;"
            ]
    _ -> ""

-- | Haskell-native target DBMS database reset for Occam (@occam-harness reset@).
module Occam.Harness.Reset (
    ResetOptions (..),
    defaultResetOptions,
    resetDb,
) where

import Control.Concurrent (threadDelay)
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
    , roConnection :: !ConnectionOverrides
    , roMaxWaitSeconds :: !Int
    }
    deriving (Eq, Show)

defaultResetOptions :: DbmsKind -> ResetOptions
defaultResetOptions dbms =
    ResetOptions
        { roDbms = dbms
        , roConnection = emptyConnectionOverrides
        , roMaxWaitSeconds = 30
        }

-- | Reset target database state cleanly before an oracle probe or batch evaluation phase.
resetDb :: HarnessConfig -> ResetOptions -> IO (Either String ())
resetDb cfg opts = do
    conn <- resolveDbmsConnection cfg (roDbms opts) (roConnection opts)
    case connDbms conn of
        DbmsSqlite -> resetEmbedded conn
        DbmsDuckDb -> resetEmbedded conn
        dbms -> resetServer dbms conn
    where
        maxWait = max 1 (roMaxWaitSeconds opts)
        maxAttempts = maxWait * 5

        resetEmbedded conn = case connContainer conn of
            Nothing -> pure (Right ())
            Just cname -> waitLoop 1 \attempt -> do
                running <- containerRunning cname
                if running
                    then do
                        (_, rc, _, _) <-
                            runProcessCapture
                                Nothing
                                Nothing
                                (Just 10.0)
                                "docker"
                                ["exec", "-i", T.unpack cname, fromMaybe "occam-exec" (connEngineBin conn)]
                                "SELECT 1;\n"
                        if rc == 0
                            then pure (Right True)
                            else stepWait attempt Nothing
                    else stepWait attempt (Just cname)
                where
                    stepWait attempt mRestart
                        | attempt >= maxAttempts =
                            pure
                                ( Left
                                    ( "[reset] Error: Embedded container "
                                        <> T.unpack cname
                                        <> " not ready after "
                                        <> show maxWait
                                        <> "s"
                                    )
                                )
                        | otherwise = do
                            mapM_ ensureContainerStarted mRestart
                            threadDelay 200_000
                            pure (Right False)

        resetServer dbms conn
            | dbms /= DbmsPostgres && not (isSafeSqlIdent (connDbName conn)) =
                pure
                    ( Left
                        ( "[reset] Error: --dbname '"
                            <> T.unpack (connDbName conn)
                            <> "' must match ^[A-Za-z0-9_]+$"
                        )
                    )
            | otherwise = do
                envMap <- Map.fromList <$> getEnvironment
                pingSpec <- buildDbmsCommand conn RolePing envMap
                resetSpec <- buildDbmsCommand conn RoleReset envMap
                case (pingSpec, resetSpec) of
                    (Left err, _) -> pure (Left ("[reset] Error: " <> err))
                    (_, Left err) -> pure (Left ("[reset] Error: " <> err))
                    (Right (pingCmd, pingArgs, pingEnv), Right (resetCmd, resetArgs, resetEnv)) -> do
                        readyRes <- waitLoop 1 \attempt -> do
                            (_, rc, _, _) <-
                                runProcessCapture
                                    Nothing
                                    (Just pingEnv)
                                    (Just 5.0)
                                    pingCmd
                                    pingArgs
                                    BS.empty
                            if rc == 0
                                then pure (Right True)
                                else
                                    if attempt >= maxAttempts
                                        then
                                            pure
                                                ( Left
                                                    ( "[reset] Error: "
                                                        <> T.unpack (dbmsKindName dbms)
                                                        <> " not ready at "
                                                        <> T.unpack (connHost conn)
                                                        <> ":"
                                                        <> show (connPort conn)
                                                        <> " after "
                                                        <> show maxWait
                                                        <> "s"
                                                    )
                                                )
                                        else do
                                            mapM_ ensureContainerStarted (connContainer conn)
                                            threadDelay 200_000
                                            pure (Right False)
                        case readyRes of
                            Left err -> pure (Left err)
                            Right () -> do
                                let sqlBytes = TE.encodeUtf8 (resetSqlFor conn)
                                (_, rc, _, errBs) <-
                                    runProcessCapture
                                        Nothing
                                        (Just resetEnv)
                                        (Just 30.0)
                                        resetCmd
                                        resetArgs
                                        sqlBytes
                                if rc == 0
                                    then pure (Right ())
                                    else
                                        pure
                                            ( Left
                                                ( "[reset] Error: "
                                                    <> T.unpack (dbmsKindName dbms)
                                                    <> " reset failed (exit "
                                                    <> show rc
                                                    <> "): "
                                                    <> T.unpack (T.strip (TE.decodeUtf8With TEE.lenientDecode errBs))
                                                )
                                            )

        waitLoop :: Int -> (Int -> IO (Either String Bool)) -> IO (Either String ())
        waitLoop attempt action = do
            res <- action attempt
            case res of
                Left err -> pure (Left err)
                Right True -> pure (Right ())
                Right False -> waitLoop (attempt + 1) action

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

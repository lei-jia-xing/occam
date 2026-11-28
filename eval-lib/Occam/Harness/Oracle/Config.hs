{-# LANGUAGE OverloadedRecordDot #-}

-- | Resolve oracle options from CLI overrides, environment variables, and Dhall defaults.
module Occam.Harness.Oracle.Config (resolveOracleOptions) where

import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Occam.Harness.Config (HarnessConfig (..), OracleDefaults (..))
import Occam.Harness.Oracle.Types (OracleOptions (..), OracleOverrides (..))
import Occam.Harness.Signature (DbmsKind (..), parseDbmsKind)
import System.Directory (doesDirectoryExist, getHomeDirectory, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | Resolve 'OracleOptions' from CLI overrides, environment variables, and Dhall defaults.
resolveOracleOptions :: HarnessConfig -> OracleOverrides -> IO (Either String OracleOptions)
resolveOracleOptions cfg ov = runExceptT do
    let defs = cfg.oracle
    envDbms <- liftIO (lookupEnv "DBMS")
    dbmsKind <- case ov.dbms <|> (envDbms >>= parseDbmsKind . T.pack) of
        Just d -> pure d
        Nothing -> case envDbms of
            Just raw | not (null raw) -> throwE ("Unsupported DBMS in DBMS env: " <> raw)
            _ -> pure DbmsPostgres
    expSig <-
        liftIO $ maybe (maybe "" T.pack <$> lookupEnv "EXPECTED_SIGNATURE") pure ov.expectedSignature
    baseSig <-
        liftIO $ maybe (maybe "" T.pack <$> lookupEnv "BASELINE_SIGNATURE") pure ov.baselineSignature
    envReject <- liftIO (envFlag "ORACLE_REJECT_ON_SLIPPAGE")
    envFrames <- liftIO (envRead "ORACLE_STACK_FRAMES")
    envTimeout <- liftIO (envRead "ORACLE_TIMEOUT")
    envAllowStderr <- liftIO (envFlag "ORACLE_ALLOW_STDERR_ONLY")
    envAllowMeta <- liftIO (envFlag "ORACLE_ALLOW_PSQL_META")
    envVerbose <- liftIO (envFlag "ORACLE_VERBOSE")
    envDecLog <- nonEmptyStr <$> liftIO (lookupEnv "ORACLE_DECISION_LOG")
    envWorkdir <- nonEmptyStr <$> liftIO (lookupEnv "OCCAM_WORKDIR")

    let stackFrames = fromMaybe defs.stackFrames (ov.stackFrames <|> envFrames)
        timeoutSecs = fromMaybe defs.timeoutSecs (ov.timeoutSeconds <|> envTimeout)
    when (isNaN timeoutSecs || isInfinite timeoutSecs || timeoutSecs < 0) $
        throwE "--timeout must be a finite non-negative number"
    when (stackFrames <= 0) $
        throwE "--stack-frames must be a positive integer"

    mLogFile <- liftIO $ traverse expandAbsPath (nonEmptyStr ov.logFile)
    mDecLog <- liftIO $ traverse expandAbsPath (nonEmptyStr (ov.decisionLog <|> envDecLog))
    mWorkdir <- liftIO case envWorkdir of
        Nothing -> pure Nothing
        Just d -> do
            isDir <- doesDirectoryExist d
            pure (d <$ guard isDir)
    pure
        OracleOptions
            { dbms = dbmsKind
            , connection = ov.connection
            , expectedSignature = expSig
            , baselineSignature = baseSig
            , rejectOnSlippage = ov.rejectOnSlippage || envReject || defs.rejectOnSlippage
            , stackFrames = stackFrames
            , printSignature = ov.printSignature
            , logFile = mLogFile
            , containerLog = nonEmptyStr ov.containerLog
            , decisionLog = mDecLog
            , timeoutSeconds = timeoutSecs
            , allowStderrOnly = ov.allowStderrOnly || envAllowStderr || defs.allowStderrOnly
            , allowPsqlMeta = ov.allowPsqlMeta || envAllowMeta || defs.allowPsqlMeta
            , verbose = ov.verbose || envVerbose || defs.verbose
            , workdir = mWorkdir
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

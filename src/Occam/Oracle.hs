{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Occam.Oracle (
    OracleConfig (..),
    executeReset,
    runExternalOracle,
    OracleObservation (..),
    OracleObservationDetails (..),
    OracleStatus (..),
    observeExternalOracle,
    observeExternalOracleDetailed,
    observeExternalOracleDetailedWith,
    oracleObservationMatches,
    classifyOracleObservation,
    prepareOracle,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (
    IOException,
    bracket,
    finally,
    handle,
    mask,
    mask_,
    onException,
    throwIO,
    try,
    uninterruptibleMask_,
 )
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, isPathSeparator, (</>))
#ifdef mingw32_HOST_OS
import System.Directory (createDirectory, findExecutable, getCurrentDirectory, getTemporaryDirectory, makeAbsolute, removeFile, removePathForcibly)
import System.IO (Handle, hClose, hFlush, hSetBinaryMode, openBinaryTempFile)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), proc, shell, terminateProcess, waitForProcess, withCreateProcess)
#else
import System.Directory (findExecutable, getCurrentDirectory, getTemporaryDirectory, makeAbsolute, removePathForcibly)
import System.IO (Handle, hClose, hFlush, hSetBinaryMode)
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), getPid, proc, shell, waitForProcess, withCreateProcess)
#endif
import System.Timeout (timeout)

-- | Run a database/environment reset command in an owned process group.
executeReset :: Maybe String -> IO Bool
executeReset Nothing = pure True
executeReset (Just cmd) =
    handle (\(_ :: IOException) -> pure False) $
        withCandidateWorkdir $ \workDir -> do
            childEnv <- candidateEnvironment workDir
            result <-
                runProcessGroup
                    defaultResetTimeoutMicros
                    ((shell cmd) {env = Just childEnv})
                    BS.empty
            pure $ case result of
                Just (ExitSuccess, _, _) -> True
                _ -> False

data OracleObservationDetails = OracleObservationDetails
    { detailedObservation :: !OracleObservation
    , totalElapsedMicros :: !Int
    , resetElapsedMicros :: !Int
    , oracleElapsedMicros :: !(Maybe Int)
    }
    deriving (Eq, Show)

type CapturedProcess = (ExitCode, ByteString, ByteString)

defaultResetTimeoutMicros :: Int
defaultResetTimeoutMicros = 30000000

maxCapturedOutputBytes :: Int
maxCapturedOutputBytes = 2 * 1024 * 1024

{- | Oracle and reset children get a fresh process group. Ordinary descendants
inherit ownership and are terminated on completion/timeout; children that call
setsid escape it and are outside the supported Linux/POSIX contract.
-}
runProcessGroup :: Int -> CreateProcess -> ByteString -> IO (Maybe CapturedProcess)
runProcessGroup timeoutMicros command input =
    snd <$> runProcessGroupWithStart (pure ()) timeoutMicros command input

runProcessGroupWithStart ::
    IO () ->
    Int ->
    CreateProcess ->
    ByteString ->
    IO (Bool, Maybe CapturedProcess)
runProcessGroupWithStart onStarted timeoutMicros command input
    | timeoutMicros <= 0 = pure (False, Nothing)
    | otherwise =
        mask $ \restore ->
            withCreateProcess process $ \mbIn mbOut mbErr ph ->
                case (mbIn, mbOut, mbErr) of
                    (Just hIn, Just hOut, Just hErr) -> do
                        groupId <- getOwnedGroup ph
                        let cleanup = cleanupOwnedGroup groupId ph
                        result <-
                            ( do
                                onStarted
                                hSetBinaryMode hIn True
                                hSetBinaryMode hOut True
                                hSetBinaryMode hErr True
                                restore $
                                    timeout
                                        timeoutMicros
                                        ( withWorker (readPipeBounded hOut) $ \readOut ->
                                            withWorker (readPipeBounded hErr) $ \readErr ->
                                                withWorker (writePipe hIn input) $ \writeInput -> do
                                                    exitCode <- waitForProcess ph
                                                    writeInput
                                                    out <- readOut
                                                    err <- readErr
                                                    pure (exitCode, out, err)
                                        )
                            )
                                `onException` cleanup
                        cleanup
                        pure (True, result)
                    _ -> ioError (userError "Missing oracle process pipes")
    where
        process =
            command
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , create_group = createProcessGroup
                }
#ifdef mingw32_HOST_OS
createProcessGroup :: Bool
createProcessGroup = False

getOwnedGroup :: ProcessHandle -> IO ()
getOwnedGroup _ = pure ()

cleanupOwnedGroup :: () -> ProcessHandle -> IO ()
cleanupOwnedGroup _ ph = do
    ignoreIOException (terminateProcess ph)
    _ <- timeout 1000000 (waitForProcess ph)
    pure ()
#else
createProcessGroup :: Bool
createProcessGroup = True

getOwnedGroup :: ProcessHandle -> IO (Maybe ProcessID)
getOwnedGroup ph = handle (\(_ :: IOException) -> pure Nothing) (getPid ph)

cleanupOwnedGroup :: Maybe ProcessID -> ProcessHandle -> IO ()
cleanupOwnedGroup mPid ph = mask_ $ do
    case mPid of
        Nothing -> pure ()
        Just pid -> do
            termSent <- tryIOException (signalProcessGroup sigTERM pid)
            case termSent of
                Right () -> uninterruptibleMask_ (threadDelay 50000)
                Left _ -> pure ()
            ignoreIOException (signalProcessGroup sigKILL pid)
    _ <- timeout 1000000 (waitForProcess ph)
    pure ()
#endif

withWorker :: IO a -> (IO a -> IO b) -> IO b
withWorker action use =
    bracket
        ( do
            result <- newEmptyMVar
            tid <- forkFinally action (putMVar result)
            pure (tid, result)
        )
        (killThread . fst)
        (\(_, result) -> use (readMVar result >>= either throwIO pure))

readPipeBounded :: Handle -> IO ByteString
readPipeBounded h = BS.concat . reverse <$> go 0 []
    where
        go retained chunks = do
            chunk <- BS.hGetSome h 32768
            if BS.null chunk
                then pure chunks
                else do
                    let kept = BS.take (maxCapturedOutputBytes - retained) chunk
                    go
                        (retained + BS.length kept)
                        (if BS.null kept then chunks else kept : chunks)

writePipe :: Handle -> ByteString -> IO ()
writePipe h bytes =
    handle
        (\(_ :: IOException) -> pure ())
        ( do
            unless (BS.null bytes) (BS.hPut h bytes)
            hFlush h
        )
        `finally` ignoreIOException (hClose h)

tryIOException :: IO a -> IO (Either IOException a)
tryIOException = try

ignoreIOException :: IO () -> IO ()
ignoreIOException = handle (\(_ :: IOException) -> pure ())

withCandidateWorkdir :: (FilePath -> IO a) -> IO a
withCandidateWorkdir = bracket createWorkdir (ignoreIOException . removePathForcibly)
    where
#ifdef mingw32_HOST_OS
    createWorkdir = do
        tempDir <- getTemporaryDirectory
        (path, h) <- openBinaryTempFile tempDir "occam-candidate-"
        hClose h
        removeFile path
        createDirectory path
        pure path
#else
    createWorkdir = do
        tempDir <- getTemporaryDirectory
        mkdtemp (tempDir </> "occam-candidate-XXXXXX")
#endif

resolveExecutable :: FilePath -> IO FilePath
resolveExecutable executable
    | isAbsolute executable || any isPathSeparator executable = makeAbsolute executable
    | otherwise = do
        found <- findExecutable executable
        maybe (pure executable) makeAbsolute found

candidateEnvironment :: FilePath -> IO [(String, String)]
candidateEnvironment workDir = do
    current <- getEnvironment
    originalDir <- getCurrentDirectory
    let overrides =
            [ ("OCCAM_WORKDIR", workDir)
            , ("OCCAM_ORIGINAL_CWD", originalDir)
            , ("TMPDIR", workDir)
            , ("TMP", workDir)
            , ("TEMP", workDir)
            ]
        keys = map fst overrides
    pure (overrides ++ filter ((`notElem` keys) . fst) current)

elapsedMicros :: Word64 -> Word64 -> Int
elapsedMicros start end = fromIntegral ((end - start) `div` 1000)

observeExternalOracle :: OracleConfig -> ByteString -> IO OracleObservation
observeExternalOracle cfg sql =
    detailedObservation <$> observeExternalOracleDetailed cfg sql

observeExternalOracleDetailed :: OracleConfig -> ByteString -> IO OracleObservationDetails
observeExternalOracleDetailed = observeExternalOracleDetailedWith (pure ())

-- | Count the oracle process only after it has actually been created.
observeExternalOracleDetailedWith ::
    IO () ->
    OracleConfig ->
    ByteString ->
    IO OracleObservationDetails
observeExternalOracleDetailedWith onOracleStarted OracleConfig {..} sql = do
    started <- getMonotonicTimeNSec
    let finish observation resetElapsed oracleElapsed = do
            ended <- getMonotonicTimeNSec
            pure
                OracleObservationDetails
                    { detailedObservation = observation
                    , totalElapsedMicros = elapsedMicros started ended
                    , resetElapsedMicros = resetElapsed
                    , oracleElapsedMicros = oracleElapsed
                    }
        remainingMicros = do
            now <- getMonotonicTimeNSec
            pure (oracleTimeoutMicros - elapsedMicros started now)
    if oracleTimeoutMicros <= 0
        then finish OracleTimedOut 0 Nothing
        else do
            attempt <- tryIOException $ withCandidateWorkdir $ \workDir -> do
                childEnv <- candidateEnvironment workDir
                (resetResult, resetElapsed) <- case oracleResetCommand of
                    Nothing -> pure (Right (Just ExitSuccess), 0)
                    Just command -> do
                        resetStarted <- getMonotonicTimeNSec
                        remaining <- remainingMicros
                        result <-
                            if remaining <= 0
                                then pure (Right Nothing)
                                else
                                    tryIOException $
                                        runProcessGroup
                                            remaining
                                            ((shell command) {env = Just childEnv})
                                            BS.empty
                        resetEnded <- getMonotonicTimeNSec
                        let duration = elapsedMicros resetStarted resetEnded
                        pure
                            ( fmap (fmap (\(code, _, _) -> code)) result
                            , duration
                            )
                case resetResult of
                    Left err -> pure (OracleIOFailed (T.pack (show err)), resetElapsed, Nothing)
                    Right Nothing -> pure (OracleTimedOut, resetElapsed, Nothing)
                    Right (Just code)
                        | code /= ExitSuccess -> pure (OracleResetFailed, resetElapsed, Nothing)
                        | otherwise -> do
                            oracleExecutable <- resolveExecutable oracleCommand
                            oracleStarted <- getMonotonicTimeNSec
                            remaining <- remainingMicros
                            if remaining <= 0
                                then pure (OracleTimedOut, resetElapsed, Nothing)
                                else do
                                    oracleResult <-
                                        tryIOException $
                                            runProcessGroupWithStart
                                                onOracleStarted
                                                remaining
                                                ((proc oracleExecutable oracleArgs) {env = Just childEnv})
                                                sql
                                    oracleEnded <- getMonotonicTimeNSec
                                    let oracleElapsed = elapsedMicros oracleStarted oracleEnded
                                    pure
                                        ( case oracleResult of
                                            Left err -> OracleIOFailed (T.pack (show err))
                                            Right (_, Nothing) -> OracleTimedOut
                                            Right (_, Just (code', out, err))
                                                | Just message <- infrastructureMessage out err ->
                                                    OracleIOFailed message
                                                | otherwise -> OracleFinished code'
                                        , resetElapsed
                                        , case oracleResult of
                                            Right (True, _) -> Just oracleElapsed
                                            _ -> Nothing
                                        )
            case attempt of
                Left err -> finish (OracleIOFailed (T.pack (show err))) 0 Nothing
                Right (observation, resetElapsed, oracleElapsed) ->
                    finish observation resetElapsed oracleElapsed

oracleObservationMatches :: OracleObservation -> Bool
oracleObservationMatches (OracleFinished ExitSuccess) = True
oracleObservationMatches _ = False

data OracleStatus
    = Accepted
    | Rejected
    | TimedOut
    | InfrastructureError
    deriving (Eq, Ord, Show)

classifyOracleObservation :: OracleObservation -> OracleStatus
classifyOracleObservation observation = case observation of
    OracleTimedOut -> TimedOut
    OracleResetFailed -> InfrastructureError
    OracleIOFailed _ -> InfrastructureError
    OracleFinished ExitSuccess -> Accepted
    OracleFinished _ -> Rejected

infrastructureMessage :: ByteString -> ByteString -> Maybe Text
infrastructureMessage stdout stderr = messageIn stdout <|> messageIn stderr
    where
        marker = "OCCAM_INFRASTRUCTURE:"
        messageIn output =
            let (_, rest) = BSC.breakSubstring marker output
                message =
                    T.strip . T.pack . BSC.unpack . BSC.takeWhile (/= '\n') $
                        BS.drop (BS.length marker) rest
             in if BSC.null rest then Nothing else Just message

-- ----------------------------------------------------------------------------
-- External interestingness predicate
-- ----------------------------------------------------------------------------

data OracleConfig = OracleConfig
    { oracleCommand :: !FilePath
    , oracleArgs :: ![String]
    , oracleTimeoutMicros :: !Int
    , oracleResetCommand :: !(Maybe String)
    }
    deriving (Eq, Show)

-- | An observation retains execution failures instead of conflating them with rejection.
data OracleObservation
    = OracleFinished !ExitCode
    | OracleTimedOut
    | OracleResetFailed
    | OracleIOFailed !Text
    deriving (Eq, Show)

-- | Refuse inputs that do not satisfy the supplied predicate before reduction.
prepareOracle :: OracleConfig -> ByteString -> IO (Either Text ())
prepareOracle cfg sql
    | oracleTimeoutMicros cfg <= 0 = pure (Left "--timeout must be positive")
    | otherwise = do
        observation <- observeExternalOracle cfg sql
        pure $ case observation of
            OracleFinished ExitSuccess -> Right ()
            OracleFinished _ -> Left "Initial input does not satisfy the oracle predicate."
            _ -> Left ("Initial oracle failed: " <> T.pack (show observation))

-- | Only an explicit zero exit status accepts a candidate.
runExternalOracle :: OracleConfig -> ByteString -> IO Bool
runExternalOracle cfg sql =
    oracleObservationMatches <$> observeExternalOracle cfg sql

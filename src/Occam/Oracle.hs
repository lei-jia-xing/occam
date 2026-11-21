{-# LANGUAGE CPP #-}

module Occam.Oracle (
    OracleConfig (..),
    shellOracleConfig,
    rawOracleConfig,
    OracleContext (..),
    resolveOracleContext,
    executeReset,
    runExternalOracle,
    OracleObservation (..),
    OracleObservationDetails (..),
    OracleStatus (..),
    observeExternalOracle,
    observeExternalOracleDetailed,
    observeExternalOracleDetailedWith,
    observeExternalOracleDetailedWithContext,
    oracleObservationMatches,
    classifyOracleObservation,
    observationOutcome,
    OracleReapFailed (..),
    prepareOracle,
    withCandidateWorkdir,
) where

import Control.Concurrent (
    forkIOWithUnmask,
    killThread,
    newEmptyMVar,
    putMVar,
    readMVar,
    threadDelay,
 )
import Control.Exception (
    Exception,
    IOException,
    SomeException,
    bracket,
    finally,
    handle,
    mask,
    onException,
    throwIO,
    try,
    uninterruptibleMask_,
 )
import Control.Monad (unless)
import Data.Aeson (
    FromJSON (..),
    FromJSONKey (..),
    FromJSONKeyFunction (..),
    ToJSON (..),
    ToJSONKey (..),
    ToJSONKeyFunction (..),
    Value (String),
    withText,
 )
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (findExecutable, getCurrentDirectory, makeAbsolute)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, isPathSeparator)
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, hSetBinaryMode, withFile)
import System.IO.Temp (withSystemTempDirectory)
#ifdef mingw32_HOST_OS
import Data.Char (toUpper)
import System.Process (
    CmdSpec (..),
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    proc,
    shell,
    terminateProcess,
    waitForProcess,
    withCreateProcess,
 )
#else
import System.Posix.Signals (nullSignal, sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Types (ProcessID)
import System.Process (
    CmdSpec (..),
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    getPid,
    getProcessExitCode,
    proc,
    shell,
    waitForProcess,
    withCreateProcess,
 )
#endif
import Occam.Core (OracleOutcome (..))
import System.IO.Error (isResourceVanishedError)
import System.Timeout (timeout)

-- | Run a database/environment reset command in an owned process group.
executeReset :: Maybe String -> IO Bool
executeReset Nothing = pure True
executeReset (Just cmd) =
    handle (\(_ :: IOException) -> pure False) $
        withCandidateWorkdir \workDir -> do
            childEnv <- candidateEnvironment workDir
            result <-
                runProcessGroup
                    defaultResetTimeoutMicros
                    ((shell cmd) {env = Just childEnv})
                    BS.empty
            pure $ case result of
                Just ExitSuccess -> True
                _ -> False

data OracleObservationDetails = OracleObservationDetails
    { detailedObservation :: !OracleObservation
    , totalElapsedMicros :: !Int
    , resetElapsedMicros :: !Int
    , oracleElapsedMicros :: !(Maybe Int)
    }
    deriving (Eq, Show)

defaultResetTimeoutMicros :: Int
defaultResetTimeoutMicros = 30000000

-- | Thrown when an oracle or reset process group cannot be cleanly reaped.
data OracleReapFailed = OracleReapFailed
    deriving (Show)

instance Exception OracleReapFailed

runProcessGroup :: Int -> CreateProcess -> ByteString -> IO (Maybe ExitCode)
runProcessGroup timeoutMicros command input =
    snd <$> runProcessGroupWithStart (pure ()) timeoutMicros command input

runProcessGroupWithStart ::
    IO () ->
    Int ->
    CreateProcess ->
    ByteString ->
    IO (Bool, Maybe ExitCode)
runProcessGroupWithStart onStarted timeoutMicros command input
    | timeoutMicros <= 0 = pure (False, Nothing)
    | otherwise =
        withNullOutput \nullOut nullErr ->
            mask \restore ->
                withCreateProcess (process nullOut nullErr) \mbIn _ _ ph ->
                    case mbIn of
                        Just hIn -> do
                            groupId <- getOwnedGroup ph
                            let cleanup = cleanupOwnedGroup groupId ph
                            result <-
                                ( do
                                    onStarted
                                    hSetBinaryMode hIn True
                                    restore $
                                        timeout
                                            timeoutMicros
                                            ( withWorker (writePipe hIn input) \writeInput -> do
                                                exitCode <- waitForProcess ph
                                                -- Kill remaining group members before joining the stdin writer.
                                                _ <- cleanup
                                                writeInput
                                                pure exitCode
                                            )
                                            -- Kill the process group on timeout before unwinding.
                                            `onException` cleanup
                                )
                                    `onException` cleanup
                            reaped <- cleanup
                            unless reaped $ throwIO OracleReapFailed
                            pure (True, result)
                        _ -> ioError (userError "Missing oracle process pipes")
    where
        process nullOut nullErr =
            command
                { std_in = CreatePipe
                , std_out = nullOut
                , std_err = nullErr
                , create_group = createProcessGroup
                }

-- | Provide null output streams (/dev/null or NUL) for oracle execution.
#ifdef mingw32_HOST_OS
withNullOutput :: (StdStream -> StdStream -> IO a) -> IO a
withNullOutput use = withFile "NUL" WriteMode \h -> use (UseHandle h) (UseHandle h)
#else
withNullOutput :: (StdStream -> StdStream -> IO a) -> IO a
withNullOutput use = withFile "/dev/null" WriteMode \h -> use (UseHandle h) (UseHandle h)
#endif
#ifdef mingw32_HOST_OS
createProcessGroup :: Bool
createProcessGroup = False

getOwnedGroup :: ProcessHandle -> IO ()
getOwnedGroup _ = pure ()

cleanupOwnedGroup :: () -> ProcessHandle -> IO Bool
cleanupOwnedGroup _ ph = do
    ignoreIOException (terminateProcess ph)
    reaped <- timeout 1000000 (waitForProcess ph)
    pure (isJust reaped)
#else
createProcessGroup :: Bool
createProcessGroup = True

getOwnedGroup :: ProcessHandle -> IO (Maybe ProcessID)
getOwnedGroup ph = handle (\(_ :: IOException) -> pure Nothing) (getPid ph)

-- | Terminate the owned process group and report whether the leader was reaped.
cleanupOwnedGroup :: Maybe ProcessID -> ProcessHandle -> IO Bool
cleanupOwnedGroup mPid ph = mask $ \restore -> do
    case mPid of
        Nothing -> pure ()
        Just pid -> do
            termSent <- tryIOException (signalProcessGroup sigTERM pid)
            case termSent of
                Right () -> uninterruptibleMask_ (waitForGroupTermination pid ph 50)
                Left _ -> pure ()
            ignoreIOException (signalProcessGroup sigKILL pid)
    reaped <- restore (timeout 1000000 (waitForProcess ph))
    pure (isJust reaped)

-- | Wait for process group termination within a short grace period after SIGTERM.
waitForGroupTermination :: ProcessID -> ProcessHandle -> Int -> IO ()
waitForGroupTermination pid ph remaining
    | remaining <= 0 = pure ()
    | otherwise = do
        leaderExited <- getProcessExitCode ph
        case leaderExited of
            Just _ -> pure ()
            Nothing -> do
                groupAlive <- tryIOException (signalProcessGroup nullSignal pid)
                case groupAlive of
                    Left _ -> pure ()
                    Right () ->
                        threadDelay 1000 >> waitForGroupTermination pid ph (remaining - 1)
#endif

-- | Fork an asynchronous helper action ensuring guaranteed cleanup and join.
withWorker :: IO a -> (IO a -> IO b) -> IO b
withWorker action use =
    bracket
        ( do
            result <- newEmptyMVar
            tid <-
                forkIOWithUnmask $ \unmask -> do
                    completed <- tryWorker (unmask action)
                    putMVar result completed
            pure (tid, result)
        )
        ( \(tid, result) -> do
            killThread tid
            _ <- readMVar result
            pure ()
        )
        (\(_, result) -> use (readMVar result >>= either throwIO pure))

tryWorker :: IO a -> IO (Either SomeException a)
tryWorker = try

writePipe :: Handle -> ByteString -> IO ()
writePipe h bytes =
    handle
        (\err -> if isResourceVanishedError err then pure () else throwIO err)
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
withCandidateWorkdir = withSystemTempDirectory "occam-candidate-"

resolveExecutable :: FilePath -> IO FilePath
resolveExecutable executable
    | isAbsolute executable || any isPathSeparator executable = makeAbsolute executable
    | otherwise = do
        found <- findExecutable executable
        maybe (pure executable) makeAbsolute found

candidateOverrideKeys :: [String]
candidateOverrideKeys = ["OCCAM_WORKDIR", "OCCAM_ORIGINAL_CWD", "TMPDIR", "TMP", "TEMP"]

isCandidateOverrideKey :: String -> Bool
#ifdef mingw32_HOST_OS
isCandidateOverrideKey k = map toUpper k `elem` candidateOverrideKeys
#else
isCandidateOverrideKey k = k `elem` candidateOverrideKeys
#endif

candidateOverrides :: FilePath -> FilePath -> [(String, String)]
candidateOverrides workDir originalDir =
    [ ("OCCAM_WORKDIR", workDir)
    , ("OCCAM_ORIGINAL_CWD", originalDir)
    , ("TMPDIR", workDir)
    , ("TMP", workDir)
    , ("TEMP", workDir)
    ]

baseEnvironment :: IO [(String, String)]
baseEnvironment =
    filter (not . isCandidateOverrideKey . fst) <$> getEnvironment

candidateEnvironment :: FilePath -> IO [(String, String)]
candidateEnvironment workDir = do
    base <- baseEnvironment
    originalDir <- getCurrentDirectory
    pure (candidateOverrides workDir originalDir ++ base)

-- | Base launch context resolved once per reduction.
data OracleContext = OracleContext
    { contextCmdSpec :: !CmdSpec
    , contextBaseEnvironment :: ![(String, String)]
    , contextOriginalDir :: !FilePath
    }
    deriving (Eq, Show)

resolveOracleContext :: OracleConfig -> IO OracleContext
resolveOracleContext OracleConfig {..} = do
    spec <- case oracleSpec of
        RawCommand bin args -> do
            resolvedBin <- resolveExecutable bin
            pure (RawCommand resolvedBin args)
        ShellCommand cmd -> pure (ShellCommand cmd)
    base <- baseEnvironment
    originalDir <- getCurrentDirectory
    pure
        OracleContext
            { contextCmdSpec = spec
            , contextBaseEnvironment = base
            , contextOriginalDir = originalDir
            }

candidateEnvironmentWith :: OracleContext -> FilePath -> [(String, String)]
candidateEnvironmentWith OracleContext {..} workDir =
    candidateOverrides workDir contextOriginalDir ++ contextBaseEnvironment

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
observeExternalOracleDetailedWith onOracleStarted cfg sql = do
    context <- resolveOracleContext cfg
    observeExternalOracleDetailedWithContext onOracleStarted cfg context sql

-- | One-shot observation against a pre-resolved 't:OracleContext'.
observeExternalOracleDetailedWithContext ::
    IO () ->
    OracleConfig ->
    OracleContext ->
    ByteString ->
    IO OracleObservationDetails
observeExternalOracleDetailedWithContext onOracleStarted OracleConfig {..} context sql = do
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
            attempt <- tryIOException $ withCandidateWorkdir \workDir -> do
                let childEnv = candidateEnvironmentWith context workDir
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
                        pure (result, duration)
                case resetResult of
                    Left err -> pure (OracleIOFailed (T.pack (show err)), resetElapsed, Nothing)
                    Right Nothing -> pure (OracleTimedOut, resetElapsed, Nothing)
                    Right (Just code)
                        | code /= ExitSuccess -> pure (OracleResetFailed, resetElapsed, Nothing)
                        | otherwise -> do
                            let cmdProcess = case contextCmdSpec context of
                                    ShellCommand cmd -> shell cmd
                                    RawCommand bin args -> proc bin args
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
                                                (cmdProcess {env = Just childEnv})
                                                sql
                                    oracleEnded <- getMonotonicTimeNSec
                                    let oracleElapsed = elapsedMicros oracleStarted oracleEnded
                                    pure
                                        ( case oracleResult of
                                            Left err -> OracleIOFailed (T.pack (show err))
                                            Right (_, Nothing) -> OracleTimedOut
                                            Right (_, Just code') -> OracleFinished code'
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
    | Interrupted
    deriving (Eq, Ord, Show)

oracleStatusName :: OracleStatus -> Text
oracleStatusName Accepted = "accepted"
oracleStatusName Rejected = "rejected"
oracleStatusName TimedOut = "timed-out"
oracleStatusName InfrastructureError = "infrastructure-error"
oracleStatusName Interrupted = "interrupted"

parseOracleStatusName :: Text -> Maybe OracleStatus
parseOracleStatusName "accepted" = Just Accepted
parseOracleStatusName "rejected" = Just Rejected
parseOracleStatusName "timed-out" = Just TimedOut
parseOracleStatusName "infrastructure-error" = Just InfrastructureError
parseOracleStatusName "interrupted" = Just Interrupted
parseOracleStatusName _ = Nothing

instance ToJSON OracleStatus where
    toJSON :: OracleStatus -> Value
    toJSON = String . oracleStatusName

instance FromJSON OracleStatus where
    parseJSON = withText "OracleStatus" \name ->
        maybe (fail ("unknown oracle status: " <> T.unpack name)) pure (parseOracleStatusName name)

instance ToJSONKey OracleStatus where
    toJSONKey :: ToJSONKeyFunction OracleStatus
    toJSONKey =
        ToJSONKeyText
            (Key.fromText . oracleStatusName)
            (Encoding.text . oracleStatusName)

instance FromJSONKey OracleStatus where
    fromJSONKey :: FromJSONKeyFunction OracleStatus
    fromJSONKey = FromJSONKeyTextParser \name ->
        maybe (fail ("unknown oracle status: " <> T.unpack name)) pure (parseOracleStatusName name)

classifyOracleObservation :: OracleObservation -> OracleStatus
classifyOracleObservation observation = case observation of
    OracleTimedOut -> TimedOut
    OracleResetFailed -> InfrastructureError
    OracleIOFailed _ -> InfrastructureError
    OracleFinished ExitSuccess -> Accepted
    OracleFinished _ -> Rejected

-- | Definite process outcomes decide; timeouts and failures stay inconclusive.
observationOutcome :: OracleObservation -> OracleOutcome
observationOutcome observation = case classifyOracleObservation observation of
    Accepted -> OracleAccepted
    Rejected -> OracleRejected
    TimedOut -> OracleInconclusive
    InfrastructureError -> OracleInconclusive
    Interrupted -> OracleInconclusive

-- ----------------------------------------------------------------------------
-- External interestingness predicate
-- ----------------------------------------------------------------------------

data OracleConfig = OracleConfig
    { oracleSpec :: !CmdSpec
    , oracleTimeoutMicros :: !Int
    , oracleResetCommand :: !(Maybe String)
    }
    deriving (Eq, Show)

-- | Construct a shell-based oracle command (portable across POSIX and Windows).
shellOracleConfig :: String -> Int -> Maybe String -> OracleConfig
shellOracleConfig cmd timeoutMicros reset =
    OracleConfig
        { oracleSpec = ShellCommand cmd
        , oracleTimeoutMicros = timeoutMicros
        , oracleResetCommand = reset
        }

-- | Construct a raw binary-based oracle command.
rawOracleConfig :: FilePath -> [String] -> Int -> Maybe String -> OracleConfig
rawOracleConfig bin args timeoutMicros reset =
    OracleConfig
        { oracleSpec = RawCommand bin args
        , oracleTimeoutMicros = timeoutMicros
        , oracleResetCommand = reset
        }

-- | An observation retains execution failures instead of conflating them with rejection.
data OracleObservation
    = OracleFinished !ExitCode
    | OracleTimedOut
    | OracleResetFailed
    | OracleIOFailed !Text
    deriving (Eq, Show)

-- | Refuse inputs that do not satisfy the supplied predicate before reduction.
prepareOracle :: OracleConfig -> ByteString -> IO (Either Text OracleObservationDetails)
prepareOracle cfg sql
    | oracleTimeoutMicros cfg <= 0 = pure (Left "--timeout must be positive")
    | otherwise = do
        details <- observeExternalOracleDetailed cfg sql
        pure $ case detailedObservation details of
            OracleFinished ExitSuccess -> Right details
            OracleFinished _ -> Left "Initial input does not satisfy the oracle predicate."
            _ -> Left ("Initial oracle failed: " <> T.pack (show (detailedObservation details)))

-- | Only an explicit zero exit status accepts a candidate.
runExternalOracle :: OracleConfig -> ByteString -> IO Bool
runExternalOracle cfg sql =
    oracleObservationMatches <$> observeExternalOracle cfg sql

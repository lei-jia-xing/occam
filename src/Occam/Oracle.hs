{-# LANGUAGE CPP #-}

module Occam.Oracle (
    OracleConfig (..),
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
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, isPathSeparator, (</>))
#ifdef mingw32_HOST_OS
import System.Directory (createDirectory, findExecutable, getCurrentDirectory, getTemporaryDirectory, makeAbsolute, removeFile, removePathForcibly)
import System.IO (Handle, hClose, hFlush, hSetBinaryMode, openBinaryTempFile)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), proc, shell, terminateProcess, waitForProcess, withCreateProcess)
#else
import System.Directory (findExecutable, getCurrentDirectory, getTemporaryDirectory, makeAbsolute, removePathForcibly)
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, hSetBinaryMode, withFile)
import System.Posix.Signals (nullSignal, sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), getPid, getProcessExitCode, proc, shell, waitForProcess, withCreateProcess)
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

{- | Oracle and reset children get a fresh process group. Ordinary descendants
inherit ownership and are terminated on completion/timeout; children that call
setsid escape it and are outside the supported Linux/POSIX contract. Process
groups are job control, not a security sandbox or a hard real-time guarantee:
kernel-wedged (SIGKILL-immune, D-state) leaders cannot be reaped, and that
failure aborts loudly via 'OracleReapFailed' instead of claiming cleanup.
-}
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
                                                -- The leader is gone but group members may still hold
                                                -- the stdin pipe open; kill the owned group before
                                                -- joining the writer so a lingering grandchild that
                                                -- inherited stdin cannot stall it.
                                                _ <- cleanup
                                                writeInput
                                                pure exitCode
                                            )
                                            -- On timeout the group must die BEFORE the helper
                                            -- join unwinds: a writer blocked on a full pipe
                                            -- that nobody reads cannot close until its readers
                                            -- are gone. This innermost handler runs first.
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

{- | Null device for the oracle's opaque output streams. The verdict is the
exit code only, so children write to the bit bucket instead of pipes: no
reader threads, no pipe deadlock, no output buffering. POSIX binds
/dev/null explicitly ('NoStream' would close the streams and fail writing
children); Windows uses 'NoStream'.
-}
#ifdef mingw32_HOST_OS
withNullOutput :: (StdStream -> StdStream -> IO a) -> IO a
withNullOutput use = use NoStream NoStream
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

-- | Terminate the owned group and report whether the leader was reaped.
-- Signals stay masked; only the bounded reap wait is interruptible, so the
-- one-second bound is real even for wedged leaders.
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

{- | Give a cooperative process group a short grace period after SIGTERM.
Poll instead of sleeping for the full delay so fast oracles do not pay it on
every invocation.
-}
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

{- | Fork a pipe helper that is always killable and always joined. The helper
runs unmasked ('forkFinally' under 'mask' would inherit 'MaskedInterruptible'
and hostile always-ready I/O could starve its kill); completion is published
masked. Both the user and the release join via persistent 'readMVar'.
-}
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

candidateOverrideKeys :: [String]
candidateOverrideKeys = ["OCCAM_WORKDIR", "OCCAM_ORIGINAL_CWD", "TMPDIR", "TMP", "TEMP"]

candidateOverrides :: FilePath -> FilePath -> [(String, String)]
candidateOverrides workDir originalDir =
    [ ("OCCAM_WORKDIR", workDir)
    , ("OCCAM_ORIGINAL_CWD", originalDir)
    , ("TMPDIR", workDir)
    , ("TMP", workDir)
    , ("TEMP", workDir)
    ]

candidateEnvironment :: FilePath -> IO [(String, String)]
candidateEnvironment workDir = do
    current <- getEnvironment
    originalDir <- getCurrentDirectory
    pure
        (candidateOverrides workDir originalDir ++ filter ((`notElem` candidateOverrideKeys) . fst) current)

{- | Oracle launch context resolved once per reduction: the executable path
plus the process environment minus the per-candidate overrides. Resolving
once skips a PATH walk and an environment snapshot per candidate; the
per-candidate workdirs still vary per probe.
-}
data OracleContext = OracleContext
    { contextExecutable :: !FilePath
    , contextBaseEnvironment :: ![(String, String)]
    , contextOriginalDir :: !FilePath
    }
    deriving (Eq, Show)

resolveOracleContext :: OracleConfig -> IO OracleContext
resolveOracleContext OracleConfig {..} = do
    executable <- resolveExecutable oracleCommand
    current <- getEnvironment
    originalDir <- getCurrentDirectory
    pure
        OracleContext
            { contextExecutable = executable
            , contextBaseEnvironment = filter ((`notElem` candidateOverrideKeys) . fst) current
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

-- | One-shot observation against a pre-resolved 'OracleContext'.
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
                            let oracleExecutable = contextExecutable context
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

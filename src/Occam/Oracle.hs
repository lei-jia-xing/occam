{-# LANGUAGE CPP #-}

module Occam.Oracle (
    OracleConfig (..),
    shellOracleConfig,
    rawOracleConfig,
    parseDirectCommand,
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
    prepareOracleWithContext,
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
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (OracleOutcome (..))
import System.Directory (
    doesFileExist,
    executable,
    findExecutable,
    getCurrentDirectory,
    getPermissions,
    makeAbsolute,
 )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, isPathSeparator)
import System.IO (
    Handle,
    IOMode (ReadMode, WriteMode),
    hClose,
    hFlush,
    hSetBinaryMode,
    withBinaryFile,
    withFile,
 )
import System.IO.Error (isResourceVanishedError)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
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

-- | Run a database/environment reset command in an owned process group.
executeReset :: Maybe String -> IO Bool
executeReset Nothing = pure True
executeReset (Just cmd) =
    handle (\(_ :: IOException) -> pure False) $
        withCandidateWorkdir \workDir -> do
            childEnv <- candidateEnvironment workDir
            spec <- resolveShellCommand cmd
            result <-
                runProcessGroup
                    defaultResetTimeoutMicros
                    ((cmdSpecToProcess spec) {env = Just childEnv})
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

-- | Guaranteed non-blocking pipe write bound (POSIX PIPE_BUF = 4096).
inlinePipeWriteLimit :: Int
inlinePipeWriteLimit = 4096

-- | Thrown when an oracle or reset process group cannot be cleanly reaped.
data OracleReapFailed = OracleReapFailed
    deriving (Show)

instance Exception OracleReapFailed

{- | Parse a shell command string into a direct executable + arguments when it
contains strictly no shell syntax, metacharacters, assignments, or POSIX
builtins. Conservative: returns 'Nothing' for any syntax that requires a shell.
-}
parseDirectCommand :: String -> Maybe (FilePath, [String])
parseDirectCommand cmd
    | any isShellMeta cmd = Nothing
    | otherwise = case splitSpaces cmd of
        [] -> Nothing
        bin : args
            | '=' `elem` bin -> Nothing
            | bin `elem` posixShellBuiltins -> Nothing
            | otherwise -> Just (bin, args)
    where
        isShellMeta c =
            c < ' '
                || c == '\DEL'
                || c `elem` ("|&;()<>$`\\\"'*?[~#{}" :: String)
        splitSpaces = filter (not . null) . go
            where
                go [] = []
                go s =
                    let (tok, rest) = break (== ' ') s
                     in tok : go (dropWhile (== ' ') rest)
        posixShellBuiltins :: [String]
        posixShellBuiltins =
            [ "."
            , ":"
            , "["
            , "!"
            , "alias"
            , "bg"
            , "break"
            , "builtin"
            , "case"
            , "cd"
            , "command"
            , "continue"
            , "do"
            , "done"
            , "echo"
            , "elif"
            , "else"
            , "esac"
            , "eval"
            , "exec"
            , "exit"
            , "export"
            , "false"
            , "fc"
            , "fg"
            , "fi"
            , "for"
            , "function"
            , "getopts"
            , "hash"
            , "if"
            , "in"
            , "jobs"
            , "kill"
            , "let"
            , "local"
            , "printf"
            , "pwd"
            , "read"
            , "readonly"
            , "return"
            , "set"
            , "shift"
            , "source"
            , "test"
            , "then"
            , "time"
            , "times"
            , "trap"
            , "true"
            , "type"
            , "typeset"
            , "ulimit"
            , "umask"
            , "unalias"
            , "unset"
            , "until"
            , "wait"
            , "while"
            ]

cmdSpecToProcess :: CmdSpec -> CreateProcess
cmdSpecToProcess (ShellCommand cmd) = shell cmd
cmdSpecToProcess (RawCommand bin args) = proc bin args

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
                            reapedRef <- newIORef False
                            cleanedRef <- newIORef False
                            let cleanup = do
                                    alreadyCleaned <- atomicModifyIORef' cleanedRef (True,)
                                    unless alreadyCleaned $ do
                                        leaderReaped <- readIORef reapedRef
                                        reaped <- cleanupOwnedGroup leaderReaped groupId ph
                                        unless reaped $ throwIO OracleReapFailed
                            result <-
                                ( do
                                    onStarted
                                    hSetBinaryMode hIn True
                                    restore $
                                        timeout
                                            timeoutMicros
                                            ( if BS.length input <= inlinePipeWriteLimit
                                                then do
                                                    writePipe hIn input
                                                    exitCode <- waitForProcess ph
                                                    writeIORef reapedRef True
                                                    cleanup
                                                    pure exitCode
                                                else withWorker (writePipe hIn input) \writeInput -> do
                                                    exitCode <- waitForProcess ph
                                                    writeIORef reapedRef True
                                                    -- Kill remaining group members before joining the stdin writer.
                                                    cleanup
                                                    writeInput
                                                    pure exitCode
                                            )
                                            -- Kill the process group on timeout before unwinding.
                                            `onException` cleanup
                                )
                                    `onException` cleanup
                            cleanup
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

createProcessGroup :: Bool
createProcessGroup = False

getOwnedGroup :: ProcessHandle -> IO ()
getOwnedGroup _ = pure ()

cleanupOwnedGroup :: Bool -> () -> ProcessHandle -> IO Bool
cleanupOwnedGroup True _ _ = pure True
cleanupOwnedGroup False _ ph = do
    ignoreIOException (terminateProcess ph)
    reaped <- timeout 1000000 (waitForProcess ph)
    pure (isJust reaped)

isCandidateOverrideKey :: String -> Bool
isCandidateOverrideKey k = map toUpper k `elem` candidateOverrideKeys

resolveShellCommand :: String -> IO CmdSpec
resolveShellCommand = pure . ShellCommand
#else
withNullOutput :: (StdStream -> StdStream -> IO a) -> IO a
withNullOutput use = withFile "/dev/null" WriteMode \h -> use (UseHandle h) (UseHandle h)

createProcessGroup :: Bool
createProcessGroup = True

getOwnedGroup :: ProcessHandle -> IO (Maybe ProcessID)
getOwnedGroup ph = handle (\(_ :: IOException) -> pure Nothing) (getPid ph)

-- | Terminate the owned process group and report whether the leader was reaped.
cleanupOwnedGroup :: Bool -> Maybe ProcessID -> ProcessHandle -> IO Bool
cleanupOwnedGroup leaderReaped mPid ph = mask $ \restore -> do
    case mPid of
        Nothing -> pure ()
        Just pid -> do
            alive <-
                if leaderReaped
                    then isRight <$> tryIOException (signalProcessGroup nullSignal pid)
                    else pure True
            whenAlive alive $ do
                termSent <- tryIOException (signalProcessGroup sigTERM pid)
                case termSent of
                    Right () -> uninterruptibleMask_ (waitForGroupTermination leaderReaped pid ph 50)
                    Left _ -> pure ()
                ignoreIOException (signalProcessGroup sigKILL pid)
    if leaderReaped
        then pure True
        else do
            reaped <- restore (timeout 1000000 (waitForProcess ph))
            pure (isJust reaped)
    where
        isRight (Right _) = True
        isRight (Left _) = False
        whenAlive True action = action
        whenAlive False _ = pure ()

-- | Wait for process group termination within a short grace period after SIGTERM.
waitForGroupTermination :: Bool -> ProcessID -> ProcessHandle -> Int -> IO ()
waitForGroupTermination leaderReaped pid ph remaining
    | remaining <= 0 = pure ()
    | otherwise = do
        leaderExited <-
            if leaderReaped
                then pure Nothing
                else getProcessExitCode ph
        case leaderExited of
            Just _ -> pure ()
            Nothing -> do
                groupAlive <- tryIOException (signalProcessGroup nullSignal pid)
                case groupAlive of
                    Left _ -> pure ()
                    Right () ->
                        threadDelay 1000 >> waitForGroupTermination leaderReaped pid ph (remaining - 1)

isCandidateOverrideKey :: String -> Bool
isCandidateOverrideKey k = k `elem` candidateOverrideKeys

-- | Verify a file is directly executable by execve (ELF binary or #! script).
isDirectExecFile :: FilePath -> IO Bool
isDirectExecFile path =
    handle (\(_ :: IOException) -> pure False) $ do
        exists <- doesFileExist path
        if not exists
            then pure False
            else do
                perms <- getPermissions path
                if not (executable perms)
                    then pure False
                    else do
                        header <- withBinaryFile path ReadMode (`BS.hGet` 4)
                        pure (BS.isPrefixOf (BS.pack [0x7f, 0x45, 0x4c, 0x46]) header || BS.isPrefixOf "#!" header)

-- | Upgrade a shell command without shell syntax into a direct RawCommand when
-- the resolved target is an executable ELF binary or #! script.
resolveShellCommand :: String -> IO CmdSpec
resolveShellCommand cmd = case parseDirectCommand cmd of
    Nothing -> pure (ShellCommand cmd)
    Just (bin, args) -> do
        resolved <-
            if isAbsolute bin || any isPathSeparator bin
                then do
                    exists <- doesFileExist bin
                    if exists then Just <$> makeAbsolute bin else pure Nothing
                else findExecutable bin >>= traverse makeAbsolute
        case resolved of
            Just absPath -> do
                direct <- isDirectExecFile absPath
                pure (if direct then RawCommand absPath args else ShellCommand cmd)
            Nothing -> pure (ShellCommand cmd)
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
resolveExecutable executablePath
    | isAbsolute executablePath || any isPathSeparator executablePath = makeAbsolute executablePath
    | otherwise = do
        found <- findExecutable executablePath
        maybe (pure executablePath) makeAbsolute found

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

baseEnvironment :: IO [(String, String)]
baseEnvironment =
    filter (not . isCandidateOverrideKey . fst) <$> getEnvironment

candidateEnvironment :: FilePath -> IO [(String, String)]
candidateEnvironment workDir = do
    base <- baseEnvironment
    originalDir <- getCurrentDirectory
    pure (candidateOverrides workDir originalDir ++ base)

candidateEnvironmentWith :: OracleContext -> FilePath -> [(String, String)]
candidateEnvironmentWith OracleContext {..} workDir =
    candidateOverrides workDir contextOriginalDir ++ contextBaseEnvironment

-- | Base launch context resolved once per reduction.
data OracleContext = OracleContext
    { contextCmdSpec :: !CmdSpec
    , contextResetSpec :: !(Maybe CmdSpec)
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
        ShellCommand cmd -> resolveShellCommand cmd
    resetSpec <- traverse resolveShellCommand oracleResetCommand
    base <- baseEnvironment
    originalDir <- getCurrentDirectory
    pure
        OracleContext
            { contextCmdSpec = spec
            , contextResetSpec = resetSpec
            , contextBaseEnvironment = base
            , contextOriginalDir = originalDir
            }

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
                                else do
                                    let resetProc = maybe (shell command) cmdSpecToProcess (contextResetSpec context)
                                    tryIOException $
                                        runProcessGroup
                                            remaining
                                            (resetProc {env = Just childEnv})
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
                            let cmdProcess = cmdSpecToProcess (contextCmdSpec context)
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
prepareOracle cfg sql = do
    context <- resolveOracleContext cfg
    prepareOracleWithContext cfg context sql

-- | Variant of 'prepareOracle' reusing a pre-resolved 'OracleContext'.
prepareOracleWithContext ::
    OracleConfig -> OracleContext -> ByteString -> IO (Either Text OracleObservationDetails)
prepareOracleWithContext cfg context sql
    | oracleTimeoutMicros cfg <= 0 = pure (Left "--timeout must be positive")
    | otherwise = do
        details <- observeExternalOracleDetailedWithContext (pure ()) cfg context sql
        pure $ case detailedObservation details of
            OracleFinished ExitSuccess -> Right details
            OracleFinished _ -> Left "Initial input does not satisfy the oracle predicate."
            _ -> Left ("Initial oracle failed: " <> T.pack (show (detailedObservation details)))

-- | Only an explicit zero exit status accepts a candidate.
runExternalOracle :: OracleConfig -> ByteString -> IO Bool
runExternalOracle cfg sql =
    oracleObservationMatches <$> observeExternalOracle cfg sql

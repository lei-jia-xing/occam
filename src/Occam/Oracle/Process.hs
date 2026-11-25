{-# LANGUAGE CPP #-}

module Occam.Oracle.Process (
    OracleContext (..),
    OracleReapFailed (..),
    baseEnvironment,
    candidateEnvironment,
    candidateEnvironmentWith,
    cmdSpecToProcess,
    elapsedMicros,
    executeReset,
    parseDirectCommand,
    resolveExecutable,
    resolveShellCommand,
    runProcessGroup,
    runProcessGroupWithStart,
    tryIOException,
    withCandidateWorkdir,
) where

import Control.Concurrent (
    MVar,
    forkIOWithUnmask,
    killThread,
    newEmptyMVar,
    putMVar,
    readMVar,
    threadDelay,
    tryReadMVar,
 )
import Control.Exception (
    Exception,
    IOException,
    SomeException,
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
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
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
import Control.Monad (unless)
import Data.Char (toUpper)
import Data.Maybe (isJust)
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
import Control.Monad (guard, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (fromMaybe, isJust)
import System.Posix.Signals (nullSignal, sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Types (ProcessID)
import System.Process (
    CmdSpec (..),
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    getPid,
    proc,
    shell,
    waitForProcess,
    withCreateProcess,
 )
#endif

defaultResetTimeoutMicros :: Int
defaultResetTimeoutMicros = 30000000

-- | Guaranteed non-blocking pipe write bound (POSIX PIPE_BUF = 4096).
inlinePipeWriteLimit :: Int
inlinePipeWriteLimit = 4096

-- | Thrown when an oracle or reset process group cannot be cleanly reaped.
data OracleReapFailed = OracleReapFailed
    deriving (Show)

instance Exception OracleReapFailed

-- | Base launch context resolved once per reduction.
data OracleContext = OracleContext
    { contextCmdSpec :: !CmdSpec
    , contextResetSpec :: !(Maybe CmdSpec)
    , contextBaseEnvironment :: ![(String, String)]
    , contextOriginalDir :: !FilePath
    }
    deriving (Eq, Show)

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
                    maybe
                        (ioError (userError "Missing oracle process pipes"))
                        (\hIn -> runOwnedProcess restore onStarted timeoutMicros input hIn ph)
                        mbIn
    where
        process nullOut nullErr =
            command
                { std_in = CreatePipe
                , std_out = nullOut
                , std_err = nullErr
                , create_group = createProcessGroup
                }

runOwnedProcess ::
    (IO (Maybe ExitCode) -> IO (Maybe ExitCode)) ->
    IO () ->
    Int ->
    ByteString ->
    Handle ->
    ProcessHandle ->
    IO (Bool, Maybe ExitCode)
runOwnedProcess restore onStarted timeoutMicros input hIn ph = do
    groupId <- getOwnedGroup ph
    reaper <- startProcessReaper ph
    cleanedRef <- newIORef False
    let wait = waitForReaper reaper
        cleanup = mask_ $ do
            alreadyCleaned <- readIORef cleanedRef
            unless alreadyCleaned $ do
                reaped <- cleanupOwnedGroup groupId ph reaper
                unless reaped $ throwIO OracleReapFailed
                writeIORef cleanedRef True
        runCandidate
            | BS.length input <= inlinePipeWriteLimit = do
                writePipe hIn input
                exitCode <- wait
                cleanup
                pure exitCode
            | otherwise = withWorker (writePipe hIn input) \writeInput -> do
                exitCode <- wait
                -- Kill remaining group members before joining the stdin writer.
                cleanup
                writeInput
                pure exitCode
        runGuarded = do
            onStarted
            hSetBinaryMode hIn True
            restore $ timeout timeoutMicros runCandidate `onException` cleanup
    result <- runGuarded `onException` cleanup
    cleanup
    pure (True, result)

-- The owner of waitpid must survive cancellation until System.Process records
-- the exit code in its handle. Cancellation interrupts the MVar reader and
-- kills the process group before joining this reaper.
type ProcessReaper = MVar (Either IOException ExitCode)

startProcessReaper :: ProcessHandle -> IO ProcessReaper
startProcessReaper ph = do
    result <- newEmptyMVar
    _ <- forkIOWithUnmask $ \_ ->
        tryIOException (uninterruptibleMask_ (waitForProcess ph)) >>= putMVar result
    pure result

waitForReaper :: ProcessReaper -> IO ExitCode
waitForReaper reaper = readMVar reaper >>= either throwIO pure

leaderReaped :: ProcessReaper -> IO Bool
leaderReaped reaper = do
    result <- tryReadMVar reaper
    pure $ case result of
        Just (Right _) -> True
        _ -> False

#ifdef mingw32_HOST_OS
withNullOutput :: (StdStream -> StdStream -> IO a) -> IO a
withNullOutput use = withFile "NUL" WriteMode \h -> use (UseHandle h) (UseHandle h)

createProcessGroup :: Bool
createProcessGroup = False

getOwnedGroup :: ProcessHandle -> IO ()
getOwnedGroup _ = pure ()

cleanupOwnedGroup :: () -> ProcessHandle -> ProcessReaper -> IO Bool
cleanupOwnedGroup _ ph reaper = do
    exited <- leaderReaped reaper
    unless exited (ignoreIOException (terminateProcess ph))
    isJust <$> timeout 1000000 (waitForReaper reaper)

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
cleanupOwnedGroup :: Maybe ProcessID -> ProcessHandle -> ProcessReaper -> IO Bool
cleanupOwnedGroup mPid _ reaper = do
    exited <- leaderReaped reaper
    case mPid of
        Nothing -> pure ()
        Just pid -> do
            alive <-
                if exited
                    then isRight <$> tryIOException (signalProcessGroup nullSignal pid)
                    else pure True
            whenAlive alive $ do
                termSent <- tryIOException (signalProcessGroup sigTERM pid)
                case termSent of
                    Right () -> uninterruptibleMask_ (waitForGroupTermination exited pid reaper 50)
                    Left _ -> pure ()
                ignoreIOException (signalProcessGroup sigKILL pid)
    if exited
        then pure True
        else isJust <$> timeout 1000000 (waitForReaper reaper)
    where
        isRight (Right _) = True
        isRight (Left _) = False
        whenAlive True action = action
        whenAlive False _ = pure ()

-- | Wait for process group termination within a short grace period after SIGTERM.
waitForGroupTermination :: Bool -> ProcessID -> ProcessReaper -> Int -> IO ()
waitForGroupTermination alreadyExited pid reaper remaining
    | remaining <= 0 = pure ()
    | otherwise = do
        leaderExited <- if alreadyExited then pure False else leaderReaped reaper
        if leaderExited
            then pure ()
            else do
                groupAlive <- tryIOException (signalProcessGroup nullSignal pid)
                case groupAlive of
                    Left _ -> pure ()
                    Right () ->
                        threadDelay 1000 >> waitForGroupTermination alreadyExited pid reaper (remaining - 1)

isCandidateOverrideKey :: String -> Bool
isCandidateOverrideKey k = k `elem` candidateOverrideKeys

-- | Verify a file is directly executable by execve (ELF binary or #! script).
isDirectExecFile :: FilePath -> IO Bool
isDirectExecFile path =
    handle (\(_ :: IOException) -> pure False) $
        fmap isJust . runMaybeT $ do
            guard =<< liftIO (doesFileExist path)
            perms <- liftIO (getPermissions path)
            guard (executable perms)
            header <- liftIO (withBinaryFile path ReadMode (`BS.hGet` 4))
            guard (BS.isPrefixOf (BS.pack [0x7f, 0x45, 0x4c, 0x46]) header || BS.isPrefixOf "#!" header)

-- | Upgrade a shell command without shell syntax into a direct RawCommand when
-- the resolved target is an executable ELF binary or #! script.
resolveShellCommand :: String -> IO CmdSpec
resolveShellCommand cmd =
    fmap (fromMaybe (ShellCommand cmd)) . runMaybeT $ do
        (bin, args) <- MaybeT (pure (parseDirectCommand cmd))
        absPath <- MaybeT (resolveDirectBin bin)
        guard =<< liftIO (isDirectExecFile absPath)
        pure (RawCommand absPath args)
    where
        resolveDirectBin bin
            | isAbsolute bin || any isPathSeparator bin = do
                exists <- doesFileExist bin
                if exists then Just <$> makeAbsolute bin else pure Nothing
            | otherwise = findExecutable bin >>= traverse makeAbsolute
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

elapsedMicros :: Word64 -> Word64 -> Int
elapsedMicros start end = fromIntegral ((end - start) `div` 1000)

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

type CapturedProcess = (ExitCode, Maybe Text, Maybe Text)

defaultResetTimeoutMicros :: Int
defaultResetTimeoutMicros = 30000000

maxInfrastructureMessageBytes :: Int
maxInfrastructureMessageBytes = 2 * 1024 * 1024

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
                                        ( withWorker (readInfrastructureDiagnostic hOut) $ \readOut ->
                                            withWorker (readInfrastructureDiagnostic hErr) $ \readErr ->
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

readInfrastructureDiagnostic :: Handle -> IO (Maybe Text)
readInfrastructureDiagnostic h = go BS.empty Nothing
    where
        marker = "OCCAM_INFRASTRUCTURE:"
        overlapLimit = BS.length marker - 1

        go :: ByteString -> Maybe ([ByteString], Int, Bool) -> IO (Maybe Text)
        go pending diagnostic = do
            chunk <- BS.hGetSome h 32768
            if BS.null chunk
                then pure (diagnosticText diagnostic)
                else case diagnostic of
                    Just (_, _, True) -> go BS.empty diagnostic
                    Just (pieces, size, False) ->
                        go BS.empty (Just (appendDiagnostic pieces size chunk))
                    Nothing ->
                        case boundaryRemainder pending chunk of
                            Just remainder ->
                                go BS.empty (Just (appendDiagnostic [] 0 (BS.drop remainder chunk)))
                            Nothing ->
                                let (_, rest) = BSC.breakSubstring marker chunk
                                 in if BS.null rest
                                        then go (nextPending pending chunk) Nothing
                                        else
                                            go
                                                BS.empty
                                                (Just (appendDiagnostic [] 0 (BS.drop (BS.length marker) rest)))

        boundaryRemainder :: ByteString -> ByteString -> Maybe Int
        boundaryRemainder pending chunk = seek (min (BS.length pending) overlapLimit)
            where
                seek overlap
                    | overlap <= 0 = Nothing
                    | BS.drop (BS.length pending - overlap) pending == BS.take overlap marker
                        && BS.take (BS.length marker - overlap) chunk == BS.drop overlap marker =
                        Just (BS.length marker - overlap)
                    | otherwise = seek (overlap - 1)

        nextPending :: ByteString -> ByteString -> ByteString
        nextPending pending chunk
            | BS.length chunk >= overlapLimit =
                BS.drop (BS.length chunk - overlapLimit) chunk
            | otherwise =
                let keepFromPending = min (BS.length pending) (overlapLimit - BS.length chunk)
                 in BS.drop (BS.length pending - keepFromPending) pending <> chunk

        appendDiagnostic :: [ByteString] -> Int -> ByteString -> ([ByteString], Int, Bool)
        appendDiagnostic pieces size chunk =
            let (line, newline) = BSC.break (== '\n') chunk
                kept = BS.take (max 0 (maxInfrastructureMessageBytes - size)) line
                newSize = size + BS.length kept
                newPieces = if BS.null kept then pieces else kept : pieces
                complete = not (BS.null newline) || newSize >= maxInfrastructureMessageBytes
             in (newPieces, newSize, complete)

        diagnosticText :: Maybe ([ByteString], Int, Bool) -> Maybe Text
        diagnosticText Nothing = Nothing
        diagnosticText (Just (pieces, _, _)) =
            Just (T.strip . T.pack . BSC.unpack . BS.concat $ reverse pieces)

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
    toJSON = String . oracleStatusName

instance FromJSON OracleStatus where
    parseJSON = withText "OracleStatus" $ \name ->
        maybe (fail ("unknown oracle status: " <> T.unpack name)) pure (parseOracleStatusName name)

instance ToJSONKey OracleStatus where
    toJSONKey =
        ToJSONKeyText
            (Key.fromText . oracleStatusName)
            (Encoding.text . oracleStatusName)

instance FromJSONKey OracleStatus where
    fromJSONKey = FromJSONKeyTextParser $ \name ->
        maybe (fail ("unknown oracle status: " <> T.unpack name)) pure (parseOracleStatusName name)

classifyOracleObservation :: OracleObservation -> OracleStatus
classifyOracleObservation observation = case observation of
    OracleTimedOut -> TimedOut
    OracleResetFailed -> InfrastructureError
    OracleIOFailed _ -> InfrastructureError
    OracleFinished ExitSuccess -> Accepted
    OracleFinished _ -> Rejected

infrastructureMessage :: Maybe Text -> Maybe Text -> Maybe Text
infrastructureMessage = (<|>)

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

{-# LANGUAGE OverloadedRecordDot #-}

-- | Select and incrementally read oracle evidence logs.
module Occam.Harness.Oracle.Log (
    acquireLogSource,
    pollFileReader,
    pollContainerReader,
    currentEnvMap,
    emitDecisionLog,
    whenVerbose,
    formatIsoUtc,
    logWaitSeconds,
    logPollIntervalMicros,
    nonEmptyText,
) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Occam.Harness.DbmsExec (DbmsConnection (..), runProcessCapture)
import Occam.Harness.Oracle.Types
import Occam.Harness.Signature (isServerDbmsKind)
import System.Environment (getEnvironment)
import System.IO (
    IOMode (ReadMode),
    SeekMode (AbsoluteSeek),
    hPutStrLn,
    hSeek,
    stderr,
    withBinaryFile,
 )
import System.Posix.Files (FileStatus, fileID, fileSize, getFileStatus)
import Text.Read (readMaybe)

-- | Format a UTC timestamp for container log cursor requests.
formatIsoUtc :: UTCTime -> Text
formatIsoUtc = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+00:00"

maxLogReadBytes :: Integer
maxLogReadBytes = 8 * 1024 * 1024

maxTotalLogChars :: Int
maxTotalLogChars = 32 * 1024 * 1024

logWaitSeconds :: Double
logWaitSeconds = 2.0

logPollIntervalMicros :: Int
logPollIntervalMicros = 50_000

-- | Locate and stat the initial crash evidence source (file, container log, or stderr fallback).
acquireLogSource ::
    OracleOptions ->
    DbmsConnection ->
    FilePath ->
    IO (Either (OracleDecision -> OracleDecision) (LogMode, Integer, Maybe Integer, Text))
acquireLogSource opts conn internalLog = runExceptT do
    sinceIso <- formatIsoUtc <$> liftIO getCurrentTime
    if not (isServerDbmsKind opts.dbms)
        then pure $ case connContainer conn of
            Just cname -> (ModeEmbeddedContainer cname, 0, Nothing, sinceIso)
            Nothing -> (ModeEmbeddedLocal, 0, Nothing, sinceIso)
        else case (opts.logFile, connContainer conn) of
            (Just path, mContainer) -> acquireHostLog path mContainer sinceIso
            (Nothing, Just cname) -> acquireContainer cname sinceIso
            (Nothing, Nothing)
                | opts.allowStderrOnly -> pure (ModeStderr, 0, Nothing, sinceIso)
                | otherwise ->
                    throwE \d ->
                        d
                            { verdict = VerdictRejected
                            , classification = ClassMissingEvidence
                            , reason = "log-source-required"
                            , hint = Just "pass --log-file or --container-name (or --allow-stderr-only)"
                            }
    where
        acquireHostLog path mContainer sinceIso = do
            readable <- liftIO (waitForReadableFile path logWaitSeconds)
            if not readable
                then case mContainer of
                    Just cname -> acquireContainer cname sinceIso
                    Nothing ->
                        throwE \d ->
                            d
                                { verdict = VerdictRejected
                                , classification = ClassMissingEvidence
                                , reason = "log-unreadable"
                                , logFile = Just path
                                , hint = Just "mount the log readably or pass --container-name"
                                }
                else do
                    stRes <- liftIO (try (getFileStatus path) :: IO (Either IOException FileStatus))
                    case stRes of
                        Right st ->
                            pure (ModeFile path, fromIntegral (fileSize st), Just (fromIntegral (fileID st)), sinceIso)
                        Left _ ->
                            throwE \d ->
                                d
                                    { verdict = VerdictRejected
                                    , classification = ClassMissingEvidence
                                    , reason = "log-stat-failed"
                                    , logFile = Just path
                                    }

        acquireContainer cname sinceIso = do
            mState <- liftIO (getContainerLogState cname internalLog)
            case mState of
                Just (sz, ino) -> pure (ModeContainer cname internalLog, sz, Just ino, sinceIso)
                Nothing ->
                    throwE \d ->
                        d
                            { verdict = VerdictRejected
                            , classification = ClassMissingEvidence
                            , reason = "container-log-unavailable"
                            , logFile = Just internalLog
                            }

waitForReadableFile :: FilePath -> Double -> IO Bool
waitForReadableFile path waitSecs = do
    start <- getCurrentTime
    go start
    where
        go start = do
            res <- try (withBinaryFile path ReadMode (const (pure ()))) :: IO (Either IOException ())
            case res of
                Right () -> pure True
                Left _ -> do
                    now <- getCurrentTime
                    if realToFrac (diffUTCTime now start) >= waitSecs
                        then pure False
                        else do
                            threadDelay logPollIntervalMicros
                            go start

-- | Abstract incremental tail cursor shared between host log files and Docker container logs.
data LogCursor = LogCursor
    { lcStat :: !(IO (Maybe (Integer, Integer)))
    -- ^ Query current @(sizeBytes, inode)@ of the active log target.
    , lcReadSlice :: !(Integer -> Integer -> IO (Maybe Text))
    -- ^ Read up to @count@ bytes starting at byte offset @start@.
    , lcReadExtra :: !(IO Text)
    -- ^ Optional secondary output stream (such as @docker logs --since@).
    }

-- | Poll a 'LogCursor' incrementally from @(curOff, curInode)@, handling log rotation/truncation.
pollLogCursor ::
    LogCursor -> Integer -> Maybe Integer -> Text -> IO (Bool, Integer, Maybe Integer, Text, Text)
pollLogCursor cursor curOff curInode accFile = do
    mState <- lcStat cursor
    case mState of
        Nothing -> combine False curOff curInode accFile
        Just (curSize, inodeNow) -> do
            let offset0 = if maybe False (/= inodeNow) curInode || curSize < curOff then 0 else curOff
            (okFile, nextOff, nextFile) <-
                if curSize <= offset0
                    then pure (True, offset0, accFile)
                    else do
                        let start = max offset0 (curSize - maxLogReadBytes)
                        mChunk <- lcReadSlice cursor start (curSize - start)
                        pure $ case mChunk of
                            Nothing -> (False, offset0, accFile)
                            Just chunk -> (True, curSize, tailText maxTotalLogChars (accFile <> chunk))
            combine okFile nextOff (Just inodeNow) nextFile
    where
        combine ok off mInode nextFile = do
            extra <- lcReadExtra cursor
            let combined =
                    tailText maxTotalLogChars $
                        if T.null extra
                            then nextFile
                            else if T.null nextFile then extra else nextFile <> "\n" <> extra
            pure (ok, off, mInode, combined, nextFile)

pollFileReader ::
    FilePath -> Integer -> Maybe Integer -> Text -> IO (Bool, Integer, Maybe Integer, Text, Text)
pollFileReader path =
    pollLogCursor
        LogCursor
            { lcStat = do
                res <- try (getFileStatus path) :: IO (Either IOException FileStatus)
                pure $ case res of
                    Left _ -> Nothing
                    Right st -> Just (fromIntegral (fileSize st), fromIntegral (fileID st))
            , lcReadSlice = \start count -> do
                res <-
                    try
                        ( withBinaryFile path ReadMode \h -> do
                            hSeek h AbsoluteSeek start
                            bytes <- BS.hGet h (fromIntegral count)
                            pure (TE.decodeUtf8With TEE.lenientDecode bytes)
                        ) ::
                        IO (Either IOException Text)
                pure (either (const Nothing) Just res)
            , lcReadExtra = pure ""
            }

getContainerLogState :: Text -> FilePath -> IO (Maybe (Integer, Integer))
getContainerLogState cname internalLog = do
    res <-
        try
            ( runProcessCapture
                Nothing
                Nothing
                (Just 3.0)
                "docker"
                ["exec", T.unpack cname, "stat", "-c", "%s %i", internalLog]
                BS.empty
            ) ::
            IO (Either IOException (Bool, Int, BS.ByteString, BS.ByteString))
    pure $ case res of
        Right (False, 0, outBs, _) ->
            case words (T.unpack (TE.decodeUtf8With TEE.lenientDecode outBs)) of
                [szStr, inoStr] -> (,) <$> readMaybe szStr <*> readMaybe inoStr
                _ -> Nothing
        _ -> Nothing

pollContainerReader ::
    Text ->
    FilePath ->
    Text ->
    Integer ->
    Maybe Integer ->
    Text ->
    IO (Bool, Integer, Maybe Integer, Text, Text)
pollContainerReader cname internalLog sinceIso =
    pollLogCursor
        LogCursor
            { lcStat = getContainerLogState cname internalLog
            , lcReadSlice = \start _ -> do
                res <-
                    try
                        ( runProcessCapture
                            Nothing
                            Nothing
                            (Just 5.0)
                            "docker"
                            ["exec", T.unpack cname, "tail", "-c", "+" <> show (start + 1), internalLog]
                            BS.empty
                        ) ::
                        IO (Either IOException (Bool, Int, BS.ByteString, BS.ByteString))
                pure $ case res of
                    Right (False, 0, outBs, _) -> Just (TE.decodeUtf8With TEE.lenientDecode outBs)
                    _ -> Nothing
            , lcReadExtra = readContainerStdoutLogs cname sinceIso
            }

readContainerStdoutLogs :: Text -> Text -> IO Text
readContainerStdoutLogs cname sinceIso = do
    res <-
        try
            ( runProcessCapture
                Nothing
                Nothing
                (Just 5.0)
                "docker"
                ["logs", "--since", T.unpack sinceIso, T.unpack cname]
                BS.empty
            ) ::
            IO (Either IOException (Bool, Int, BS.ByteString, BS.ByteString))
    pure $ case res of
        Right (_, _, outBs, errBs) ->
            let outTxt = TE.decodeUtf8With TEE.lenientDecode outBs
                errTxt = TE.decodeUtf8With TEE.lenientDecode errBs
             in outTxt <> "\n" <> errTxt
        Left _ -> ""

tailText :: Int -> Text -> Text
tailText limit txt
    | T.length txt <= limit = txt
    | otherwise = T.takeEnd limit txt

nonEmptyText :: Text -> Maybe Text
nonEmptyText t = if T.null t then Nothing else Just t

currentEnvMap :: IO (Map String String)
currentEnvMap = Map.fromList <$> getEnvironment

emitDecisionLog :: Maybe FilePath -> OracleDecision -> IO ()
emitDecisionLog Nothing _ = pure ()
emitDecisionLog (Just path) dec = do
    let line = BSL.toStrict (encode dec) <> BS.singleton 10
    res <- try (BS.appendFile path line) :: IO (Either IOException ())
    case res of
        Right () -> pure ()
        Left exc ->
            hPutStrLn stderr ("[occam-harness oracle] Warning: failed to write " <> path <> ": " <> show exc)

whenVerbose :: Bool -> OracleDecision -> IO ()
whenVerbose False _ = pure ()
whenVerbose True dec =
    hPutStrLn
        stderr
        ( "[occam-harness oracle] "
            <> T.unpack (T.toUpper (verdictText dec.verdict))
            <> " ["
            <> T.unpack (classificationText dec.classification)
            <> "]: "
            <> T.unpack dec.reason
        )

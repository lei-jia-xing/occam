{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Occam.Oracle (
    CrashKind (..),
    StackFrame (..),
    CrashReport (..),
    isEquivalentCrash,
    parseCrashReportJSON,
    crashReportToJSON,
    OracleConfig (..),
    executeReset,
    runExternalOracle,
    runGdbSupervisor,
) where

import Control.Exception (IOException, handle)
import Control.Monad (join, unless)
import Data.Aeson (
    FromJSON (..),
    Object,
    ToJSON (..),
    decodeStrict,
    encode,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.IO (hClose, hFlush)
import System.Process (
    CreateProcess (..),
    StdStream (..),
    proc,
    shell,
    waitForProcess,
    withCreateProcess,
 )
import System.Timeout (timeout)

-- ----------------------------------------------------------------------------
-- 1. Typed CrashReport ADT & Community Aeson Serialization
-- ----------------------------------------------------------------------------

data CrashKind
    = SignalCrash !Int
    | ASanCrash !Text !(Maybe Text)
    | AssertFailure !Text
    | ExitFailureCode !Int
    deriving (Eq, Show, Generic)

data StackFrame = StackFrame
    { frameIndex :: !Int
    , frameFunction :: !Text
    , frameFile :: !(Maybe Text)
    , frameLine :: !(Maybe Int)
    }
    deriving (Eq, Show, Generic)

data CrashReport = CrashReport
    { reportKind :: !CrashKind
    , reportPC :: !(Maybe Word64)
    , reportFrames :: ![StackFrame]
    }
    deriving (Eq, Show, Generic)

instance ToJSON CrashKind where
    toJSON = \case
        SignalCrash sig ->
            object ["kind" .= ("signal" :: Text), "signal" .= sig]
        ASanCrash err mAddr ->
            object $
                [ "kind" .= ("asan" :: Text)
                , "error_type" .= err
                ]
                    ++ maybe [] (\a -> ["address" .= a]) mAddr
        AssertFailure msg ->
            object
                [ "kind" .= ("assert" :: Text)
                , "assertion" .= msg
                ]
        ExitFailureCode code ->
            object
                [ "kind" .= ("exit" :: Text)
                , "code" .= code
                ]

instance FromJSON CrashKind where
    parseJSON = withObject "CrashKind" parseCrashKindFields

parseCrashKindFields :: Object -> Parser CrashKind
parseCrashKindFields o = do
    mSig <- o .:? "signal" :: Parser (Maybe Int)
    mKind <- o .:? "kind" :: Parser (Maybe Text)
    mErrType <- o .:? "error_type" :: Parser (Maybe Text)
    mAsan <- o .:? "asan" :: Parser (Maybe Text)
    mAddr <- o .:? "address" :: Parser (Maybe Text)
    mAssertion <- o .:? "assertion" :: Parser (Maybe Text)
    mAssert <- o .:? "assert" :: Parser (Maybe Text)
    mExitCode <- o .:? "exit_code" :: Parser (Maybe Int)
    mCode <- o .:? "code" :: Parser (Maybe Int)
    case () of
        _
            | Just sig <- mSig -> pure (SignalCrash sig)
            | Just "SIGSEGV" <- mKind -> pure (SignalCrash 11)
            | Just "SIGABRT" <- mKind -> pure (SignalCrash 6)
            | Just "SIGBUS" <- mKind -> pure (SignalCrash 7)
            | Just "SIGFPE" <- mKind -> pure (SignalCrash 8)
            | Just "SIGILL" <- mKind -> pure (SignalCrash 4)
            | Just "signal" <- mKind, Just sig <- mCode -> pure (SignalCrash sig)
            | Just err <- mErrType -> pure (ASanCrash err mAddr)
            | Just err <- mAsan -> pure (ASanCrash err mAddr)
            | Just msg <- mAssertion -> pure (AssertFailure msg)
            | Just msg <- mAssert -> pure (AssertFailure msg)
            | Just code <- mExitCode -> pure (ExitFailureCode code)
            | Just "exit" <- mKind, Just code <- mCode -> pure (ExitFailureCode code)
            | otherwise -> fail "Unknown or invalid crash kind in JSON"

instance ToJSON StackFrame where
    toJSON StackFrame {..} =
        object $
            [ "index" .= frameIndex
            , "function" .= frameFunction
            ]
                ++ maybe [] (\f -> ["file" .= f]) frameFile
                ++ maybe [] (\l -> ["line" .= l]) frameLine

instance FromJSON StackFrame where
    parseJSON = withObject "StackFrame" $ \o ->
        StackFrame
            <$> o .:? "index" .!= 0
            <*> o .: "function"
            <*> o .:? "file"
            <*> o .:? "line"

instance ToJSON CrashReport where
    toJSON CrashReport {..} =
        object $
            kindFields reportKind
                ++ ["frames" .= reportFrames]
                ++ maybe [] (\pc -> ["pc" .= pc]) reportPC
        where
            kindFields = \case
                SignalCrash sig ->
                    ["kind" .= ("signal" :: Text), "signal" .= sig]
                ASanCrash err mAddr ->
                    ["kind" .= ("asan" :: Text), "error_type" .= err]
                        ++ maybe [] (\a -> ["address" .= a]) mAddr
                AssertFailure msg ->
                    ["kind" .= ("assert" :: Text), "assertion" .= msg]
                ExitFailureCode code ->
                    ["kind" .= ("exit" :: Text), "code" .= code]

instance FromJSON CrashReport where
    parseJSON = withObject "CrashReport" $ \o -> do
        kind <- parseCrashKindFields o
        pc <- o .:? "pc"
        frames <- o .:? "frames" .!= []
        pure
            CrashReport
                { reportKind = kind
                , reportPC = pc
                , reportFrames = frames
                }

-- | Parse a JSON ByteString into a typed CrashReport.
parseCrashReportJSON :: ByteString -> Maybe CrashReport
parseCrashReportJSON = decodeStrict

-- | Serialize a CrashReport to a JSON ByteString.
crashReportToJSON :: CrashReport -> ByteString
crashReportToJSON = BSL.toStrict . encode

-- ----------------------------------------------------------------------------
-- 2. Semantic Equivalence Checker
-- ----------------------------------------------------------------------------

{- | Check whether two crash reports represent the same crash.
Checks that crash kinds match and top-N stack frame functions match.
-}
isEquivalentCrash :: Int -> CrashReport -> CrashReport -> Bool
isEquivalentCrash n r1 r2 =
    reportKind r1 == reportKind r2
        && take n (map frameFunction (reportFrames r1))
            == take n (map frameFunction (reportFrames r2))

-- ----------------------------------------------------------------------------
-- 3. Debugger Supervisor
-- ----------------------------------------------------------------------------

{- | Run target under GDB, extracting typed CrashReport via Aeson JSON dump.
Process execution is protected against timeouts without leaving zombie processes.
-}
runGdbSupervisor ::
    FilePath ->
    [String] ->
    ByteString ->
    Int ->
    IO (Maybe CrashReport)
runGdbSupervisor targetProg targetArgs sql timeoutMicros =
    handle (\(_ :: IOException) -> pure Nothing) $ do
        let gdbPythonScript =
                "import gdb, json\n\
                \try:\n\
                \    gdb.execute('run')\n\
                \except Exception:\n\
                \    pass\n\
                \try:\n\
                \    frame = gdb.newest_frame()\n\
                \    pc = frame.pc()\n\
                \    frames = []\n\
                \    cur = frame\n\
                \    idx = 0\n\
                \    while cur and idx < 20:\n\
                \        sal = cur.find_sal()\n\
                \        frames.append({\n\
                \            'index': idx,\n\
                \            'function': cur.name() or '???',\n\
                \            'file': sal.symtab.filename if sal and sal.symtab else None,\n\
                \            'line': sal.line if sal and sal.line > 0 else None\n\
                \        })\n\
                \        cur = cur.older()\n\
                \        idx += 1\n\
                \    report = {'kind': 'signal', 'signal': 11, 'pc': pc, 'frames': frames}\n\
                \    print('---JSON_REPORT_BEGIN---')\n\
                \    print(json.dumps(report))\n\
                \except Exception:\n\
                \    pass\n"
            gdbArgs =
                ["--batch", "-q", "-nx", "-ex", "set confirm off", "-ex", "set pagination off"]
                    ++ ["-ex", "file " ++ targetProg]
                    ++ (if null targetArgs then [] else ["-ex", "set args " ++ unwords targetArgs])
                    ++ ["-ex", "python " ++ gdbPythonScript]
            cp =
                (proc "gdb" gdbArgs)
                    { std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
        mRes <- timeout timeoutMicros $
            withCreateProcess cp $ \mbIn mbOut _ ph -> case (mbIn, mbOut) of
                (Just hIn, Just hOut) -> do
                    handle (\(_ :: IOException) -> pure ()) $ do
                        unless (BSC.null sql) $ do
                            BSC.hPutStr hIn sql
                            hFlush hIn
                        hClose hIn
                    outBytes <- BSC.hGetContents hOut
                    _ <- waitForProcess ph
                    pure $ extractJsonReport outBytes
                _ -> pure Nothing
        pure (join mRes)
    where
        extractJsonReport bs =
            let marker = "---JSON_REPORT_BEGIN---\n"
             in case BSC.breakSubstring marker bs of
                    (_, rest)
                        | not (BSC.null rest) ->
                            let jsonBS = BSC.drop (BSC.length marker) rest
                             in parseCrashReportJSON (BSC.takeWhile (/= '\n') jsonBS)
                    _ -> parseCrashReportJSON bs

-- ----------------------------------------------------------------------------
-- 4. Lightweight State Reset Mechanism
-- ----------------------------------------------------------------------------

-- | Run a database/environment reset command.
executeReset :: Maybe String -> IO Bool
executeReset Nothing = pure True
executeReset (Just cmd) =
    handle (\(_ :: IOException) -> pure False) $
        withCreateProcess (shell cmd) $ \_ _ _ ph -> do
            exitCode <- waitForProcess ph
            pure (exitCode == ExitSuccess)

-- ----------------------------------------------------------------------------
-- 5. External Oracle Runner
-- ----------------------------------------------------------------------------

data OracleConfig = OracleConfig
    { oracleCommand :: !FilePath
    , oracleArgs :: ![String]
    , oracleTimeoutMicros :: !Int
    , oracleResetCommand :: !(Maybe String)
    , oracleExpectedCrash :: !(Maybe (Int, CrashReport))
    }
    deriving (Eq, Show)

-- | Run an external script/oracle with timeout and optional reset.
runExternalOracle :: OracleConfig -> ByteString -> IO Bool
runExternalOracle OracleConfig {..} sql = do
    resetOk <- executeReset oracleResetCommand
    if not resetOk
        then pure False
        else do
            let cp =
                    (proc oracleCommand oracleArgs)
                        { std_in = CreatePipe
                        , std_out = CreatePipe
                        , std_err = Inherit
                        }
            mResult <- timeout oracleTimeoutMicros $
                withCreateProcess cp $ \mbIn mbOut _ ph -> case (mbIn, mbOut) of
                    (Just hIn, Just hOut) -> do
                        handle (\(_ :: IOException) -> pure ()) $ do
                            BSC.hPutStr hIn sql
                            hFlush hIn
                            hClose hIn
                        outBytes <- BSC.hGetContents hOut
                        exitCode <- waitForProcess ph
                        pure (exitCode, outBytes)
                    _ -> pure (ExitFailure (-1), BSC.empty)
            case mResult of
                Nothing -> pure False
                Just (exitCode, stdoutBytes) ->
                    case oracleExpectedCrash of
                        Just (topN, expectedReport) ->
                            case parseCrashReportJSON stdoutBytes of
                                Just actualReport ->
                                    pure (isEquivalentCrash topN expectedReport actualReport)
                                Nothing ->
                                    pure (isCrashExitCode exitCode)
                        Nothing ->
                            case parseCrashReportJSON stdoutBytes of
                                Just _ -> pure True
                                Nothing -> pure (isCrashExitCode exitCode)

isCrashExitCode :: ExitCode -> Bool
isCrashExitCode ExitSuccess = True
isCrashExitCode (ExitFailure code) = code < 0 || code >= 128

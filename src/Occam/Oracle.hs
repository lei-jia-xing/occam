module Occam.Oracle (
    CrashKind (..),
    StackFrame (..),
    CrashReport (..),
    isEquivalentCrash,
    parseCrashReportJSON,
    crashReportToJSON,
    parseGdbMiOutput,
    OracleConfig (..),
    executeReset,
    runExternalOracle,
    runGdbSupervisor,
) where

import Control.Exception (IOException, handle)
import Control.Monad (join, unless)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Numeric (readHex)
import System.Exit (ExitCode (..))
import System.IO (hClose, hFlush, hGetLine, hIsEOF, hPutStrLn)
import System.Process (
    CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    shell,
    waitForProcess,
 )
import System.Timeout (timeout)

-- ----------------------------------------------------------------------------
-- 1. Typed CrashReport ADT
-- ----------------------------------------------------------------------------

data CrashKind
    = SignalCrash !Int
    | ASanCrash !Text
    | AssertCrash !Text
    | UnknownCrash
    deriving (Eq, Show)

data StackFrame = StackFrame
    { frameIndex :: !Int
    , frameFunction :: !Text
    , frameFile :: !(Maybe Text)
    , frameLine :: !(Maybe Int)
    }
    deriving (Eq, Show)

data CrashReport = CrashReport
    { reportKind :: !CrashKind
    , reportPC :: !(Maybe Word64)
    , reportFrames :: ![StackFrame]
    }
    deriving (Eq, Show)

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
-- 3. Structured JSON Oracle Contract
-- ----------------------------------------------------------------------------

data JVal
    = JObj ![(Text, JVal)]
    | JArr ![JVal]
    | JStr !Text
    | JNum !Double
    | JBool !Bool
    | JNull
    deriving (Eq, Show)

-- | Minimal robust JSON parser for CrashReport JSON.
parseJSON :: Text -> Maybe JVal
parseJSON input = case parseValue (T.strip input) of
    Just (v, rest) | T.null (T.strip rest) -> Just v
    _ -> Nothing
    where
        skipWs = T.dropWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

        parseValue s =
            let s' = skipWs s
             in case T.uncons s' of
                    Nothing -> Nothing
                    Just ('"', _) -> parseString s'
                    Just ('{', _) -> parseObject s'
                    Just ('[', _) -> parseArray s'
                    Just ('t', _) -> if "true" `T.isPrefixOf` s' then Just (JBool True, T.drop 4 s') else Nothing
                    Just ('f', _) -> if "false" `T.isPrefixOf` s' then Just (JBool False, T.drop 5 s') else Nothing
                    Just ('n', _) -> if "null" `T.isPrefixOf` s' then Just (JNull, T.drop 4 s') else Nothing
                    Just (c, _) | c == '-' || isDigit c -> parseNumber s'
                    _ -> Nothing

        parseString s = case T.uncons s of
            Just ('"', rest) -> go rest T.empty
            _ -> Nothing
            where
                go t acc = case T.uncons t of
                    Nothing -> Nothing
                    Just ('"', rest) -> Just (JStr acc, rest)
                    Just ('\\', rest) -> case T.uncons rest of
                        Just ('"', r) -> go r (T.snoc acc '"')
                        Just ('\\', r) -> go r (T.snoc acc '\\')
                        Just ('/', r) -> go r (T.snoc acc '/')
                        Just ('n', r) -> go r (T.snoc acc '\n')
                        Just ('t', r) -> go r (T.snoc acc '\t')
                        Just ('r', r) -> go r (T.snoc acc '\r')
                        Just (other, r) -> go r (T.snoc acc other)
                        Nothing -> Nothing
                    Just (c, rest) -> go rest (T.snoc acc c)

        parseNumber s =
            let (numStr, rest) =
                    T.span
                        (\c -> isDigit c || c == '-' || c == '.' || c == 'e' || c == 'E' || c == '+')
                        s
             in case reads (T.unpack numStr) of
                    [(val, "")] -> Just (JNum val, rest)
                    _ -> Nothing

        parseArray s = case T.uncons s of
            Just ('[', rest) ->
                let rest' = skipWs rest
                 in case T.uncons rest' of
                        Just (']', r) -> Just (JArr [], r)
                        _ -> parseElements rest' []
            _ -> Nothing
            where
                parseElements cur acc = do
                    (val, afterVal) <- parseValue cur
                    let afterWs = skipWs afterVal
                    case T.uncons afterWs of
                        Just (',', next) -> parseElements (skipWs next) (val : acc)
                        Just (']', r) -> Just (JArr (reverse (val : acc)), r)
                        _ -> Nothing

        parseObject s = case T.uncons s of
            Just ('{', rest) ->
                let rest' = skipWs rest
                 in case T.uncons rest' of
                        Just ('}', r) -> Just (JObj [], r)
                        _ -> parsePairs rest' []
            _ -> Nothing
            where
                parsePairs cur acc = do
                    (JStr key, afterKey) <- parseString (skipWs cur)
                    let afterColon = skipWs afterKey
                    case T.uncons afterColon of
                        Just (':', restVal) -> do
                            (val, afterV) <- parseValue restVal
                            let afterSep = skipWs afterV
                            case T.uncons afterSep of
                                Just (',', next) -> parsePairs (skipWs next) ((key, val) : acc)
                                Just ('}', r) -> Just (JObj (reverse ((key, val) : acc)), r)
                                _ -> Nothing
                        _ -> Nothing

lookupStr :: Text -> [(Text, JVal)] -> Maybe Text
lookupStr k obj = case lookup k obj of
    Just (JStr s) -> Just s
    _ -> Nothing

lookupInt :: Text -> [(Text, JVal)] -> Maybe Int
lookupInt k obj = case lookup k obj of
    Just (JNum n) -> Just (round n)
    _ -> Nothing

lookupWord64 :: Text -> [(Text, JVal)] -> Maybe Word64
lookupWord64 k obj = case lookup k obj of
    Just (JNum n) -> Just (round n)
    _ -> Nothing

lookupArr :: Text -> [(Text, JVal)] -> Maybe [JVal]
lookupArr k obj = case lookup k obj of
    Just (JArr a) -> Just a
    _ -> Nothing

-- | Parse a JSON ByteString into a typed CrashReport.
parseCrashReportJSON :: ByteString -> Maybe CrashReport
parseCrashReportJSON bs = do
    txt <- either (const Nothing) Just (TE.decodeUtf8' bs)
    jval <- parseJSON txt
    case jval of
        JObj obj -> parseCrashReportObj obj
        _ -> Nothing

parseCrashReportObj :: [(Text, JVal)] -> Maybe CrashReport
parseCrashReportObj obj = do
    let mSig = lookupInt "signal" obj
        mKindStr = lookupStr "kind" obj
        mAssert = lookupStr "assert" obj
        mAsan = lookupStr "asan" obj
        kind = case (mSig, mKindStr, mAssert, mAsan) of
            (Just sig, _, _, _) -> SignalCrash sig
            (_, Just "SIGSEGV", _, _) -> SignalCrash 11
            (_, Just "SIGABRT", _, _) -> SignalCrash 6
            (_, Just "SIGBUS", _, _) -> SignalCrash 7
            (_, Just "SIGFPE", _, _) -> SignalCrash 8
            (_, Just "SIGILL", _, _) -> SignalCrash 4
            (_, Just k, _, _) | "ASAN" `T.isInfixOf` T.toUpper k -> ASanCrash k
            (_, Just k, _, _) | "ASSERT" `T.isInfixOf` T.toUpper k -> AssertCrash k
            (_, _, Just msg, _) -> AssertCrash msg
            (_, _, _, Just msg) -> ASanCrash msg
            _ -> UnknownCrash
        mPC = lookupWord64 "pc" obj
        frames = case lookupArr "frames" obj of
            Just arr -> mapMaybe parseFrame arr
            Nothing -> []
    pure
        CrashReport
            { reportKind = kind
            , reportPC = mPC
            , reportFrames = frames
            }

parseFrame :: JVal -> Maybe StackFrame
parseFrame (JObj obj) = do
    func <- lookupStr "function" obj
    let idx = fromMaybe 0 (lookupInt "index" obj)
        mFile = lookupStr "file" obj
        mLine = lookupInt "line" obj
    pure
        StackFrame
            { frameIndex = idx
            , frameFunction = func
            , frameFile = mFile
            , frameLine = mLine
            }
parseFrame _ = Nothing

-- | Serialize a CrashReport to a JSON ByteString.
crashReportToJSON :: CrashReport -> ByteString
crashReportToJSON CrashReport {..} =
    BSC.pack $
        "{\"kind\":\""
            ++ kindStr
            ++ "\""
            ++ sigField
            ++ pcField
            ++ ",\"frames\":["
            ++ intercalate "," (map frameToJSON reportFrames)
            ++ "]}"
    where
        (kindStr, sigField) = case reportKind of
            SignalCrash s -> ("SignalCrash", ",\"signal\":" ++ show s)
            ASanCrash msg -> ("ASanCrash", ",\"asan\":\"" ++ escapeJSON (T.unpack msg) ++ "\"")
            AssertCrash msg -> ("AssertCrash", ",\"assert\":\"" ++ escapeJSON (T.unpack msg) ++ "\"")
            UnknownCrash -> ("UnknownCrash", "")
        pcField = case reportPC of
            Just pc -> ",\"pc\":" ++ show pc
            Nothing -> ""
        frameToJSON StackFrame {..} =
            "{\"index\":"
                ++ show frameIndex
                ++ ",\"function\":\""
                ++ escapeJSON (T.unpack frameFunction)
                ++ "\""
                ++ maybe "" (\f -> ",\"file\":\"" ++ escapeJSON (T.unpack f) ++ "\"") frameFile
                ++ maybe "" (\l -> ",\"line\":" ++ show l) frameLine
                ++ "}"
        escapeJSON = concatMap (\case '"' -> "\\\""; '\\' -> "\\\\"; c -> [c])

-- ----------------------------------------------------------------------------
-- 4. GDB-MI Output Parser & Debugger Supervisor
-- ----------------------------------------------------------------------------

extractField :: Text -> Text -> Maybe Text
extractField key txt =
    let pat = key <> "=\""
     in case T.breakOn pat txt of
            (_, rest)
                | not (T.null rest) ->
                    let after = T.drop (T.length pat) rest
                        val = T.takeWhile (/= '"') after
                     in Just val
            _ -> Nothing

parseHexPC :: Text -> Maybe Word64
parseHexPC txt =
    let clean = if "0x" `T.isPrefixOf` txt || "0X" `T.isPrefixOf` txt then T.drop 2 txt else txt
     in case readHex (T.unpack clean) of
            [(val, "")] -> Just val
            _ -> Nothing

parseGdbFrame :: Text -> Maybe StackFrame
parseGdbFrame frameText = do
    func <- extractField "func" frameText
    let mIdx =
            extractField "level" frameText >>= \s -> case reads (T.unpack s) of
                [(n, "")] -> Just n
                _ -> Nothing
        idx = fromMaybe 0 mIdx
        mFile = extractField "file" frameText
        mLine =
            extractField "line" frameText >>= \s -> case reads (T.unpack s) of
                [(n, "")] -> Just n
                _ -> Nothing
    pure
        StackFrame
            { frameIndex = idx
            , frameFunction = func
            , frameFile = mFile
            , frameLine = mLine
            }

-- | Parse GDB-MI text containing *stopped and/or stack=[frame={...},...] into a CrashReport.
parseGdbMiOutput :: Text -> Maybe CrashReport
parseGdbMiOutput miText = do
    let linesList = T.lines miText
        stoppedLine = case filter ("*stopped" `T.isInfixOf`) linesList of
            (l : _) -> Just l
            [] -> Nothing

    -- Verify that a crash/signal stop actually occurred
    stop <- stoppedLine
    let reason = extractField "reason" stop
        sigName = extractField "signal-name" stop
        sigNum = case sigName of
            Just "SIGSEGV" -> Just 11
            Just "SIGABRT" -> Just 6
            Just "SIGBUS" -> Just 7
            Just "SIGFPE" -> Just 8
            Just "SIGILL" -> Just 4
            Just other -> case T.filter isDigit other of
                s | not (T.null s) -> Just (read (T.unpack s))
                _ -> Just 11
            Nothing ->
                if reason == Just "signal-received"
                    then Just 11
                    else Nothing

    kind <- case sigNum of
        Just n -> Just (SignalCrash n)
        Nothing ->
            if reason == Just "exited-signalled"
                then Just (SignalCrash 11)
                else Nothing

    -- Extract frames (deduplicate by frameIndex, e.g. *stopped frame vs ^done,stack=[...])
    let frameChunks = splitOnFrame miText
        allFrames = mapMaybe parseGdbFrame frameChunks
        frames = Map.elems (Map.fromList [(frameIndex f, f) | f <- allFrames])
    -- Extract PC from first frame addr
    let mPC = case frameChunks of
            (f : _) -> extractField "addr" f >>= parseHexPC
            [] -> Nothing

    pure
        CrashReport
            { reportKind = kind
            , reportPC = mPC
            , reportFrames = frames
            }
    where
        splitOnFrame t = case T.breakOn "frame={" t of
            (_, rest)
                | not (T.null rest) ->
                    let after = T.drop 7 rest
                        (body, tailRest) = T.breakOn "}" after
                     in body : splitOnFrame tailRest
            _ -> []

-- | Run target under GDB-MI, wait for crash/stop, extract CrashReport.
runGdbSupervisor ::
    FilePath ->
    [String] ->
    ByteString ->
    Int ->
    IO (Maybe CrashReport)
runGdbSupervisor targetProg targetArgs sql timeoutMicros =
    handle (\(_ :: IOException) -> pure Nothing) $ do
        let gdbArgs = ["-q", "--nx", "-interpreter=mi2", "--args", targetProg] ++ targetArgs
        mRes <- timeout timeoutMicros $ do
            (Just hIn, Just hOut, _, ph) <-
                createProcess
                    (proc "gdb" gdbArgs)
                        { std_in = CreatePipe
                        , std_out = CreatePipe
                        , std_err = CreatePipe
                        }
            hPutStrLn hIn "-exec-run"
            hFlush hIn

            unless (BSC.null sql) $ do
                BSC.hPutStrLn hIn sql
                hFlush hIn

            outText <- readMiOutput hIn hOut ph
            pure (parseGdbMiOutput outText)
        pure (join mRes)
    where
        readMiOutput hIn hOut ph = do
            output <- readLines hIn hOut []
            _ <- waitForProcess ph
            pure (T.unlines (reverse output))

        readLines hIn hOut acc = do
            isEof <- hIsEOF hOut
            if isEof
                then pure acc
                else do
                    line <- T.pack <$> hGetLine hOut
                    let acc' = line : acc
                    if "*stopped" `T.isPrefixOf` line
                        then do
                            hPutStrLn hIn "-stack-list-frames 0 10"
                            hPutStrLn hIn "-gdb-exit"
                            hFlush hIn
                            drainLines hOut acc'
                        else
                            if "^exit" `T.isPrefixOf` line || "*stopped,reason=\"exited" `T.isPrefixOf` line
                                then pure acc'
                                else readLines hIn hOut acc'

        drainLines hOut acc = do
            isEof <- hIsEOF hOut
            if isEof
                then pure acc
                else do
                    line <- T.pack <$> hGetLine hOut
                    drainLines hOut (line : acc)

-- ----------------------------------------------------------------------------
-- 5. Lightweight State Reset Mechanism
-- ----------------------------------------------------------------------------

-- | Run a database/environment reset command.
executeReset :: Maybe String -> IO Bool
executeReset Nothing = pure True
executeReset (Just cmd) = do
    (_, _, _, ph) <- createProcess (shell cmd)
    exitCode <- waitForProcess ph
    pure (exitCode == ExitSuccess)

-- ----------------------------------------------------------------------------
-- 6. External Oracle Runner
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
            mResult <- timeout oracleTimeoutMicros runProcessWithStdin
            case mResult of
                Nothing -> pure False
                Just (exitCode, stdoutBytes) ->
                    case oracleExpectedCrash of
                        Just (topN, expectedReport) ->
                            case parseCrashReportJSON stdoutBytes of
                                Just actualReport ->
                                    pure (isEquivalentCrash topN expectedReport actualReport)
                                Nothing ->
                                    pure (exitCode /= ExitSuccess)
                        Nothing ->
                            pure (exitCode /= ExitSuccess || isJust (parseCrashReportJSON stdoutBytes))
    where
        runProcessWithStdin = do
            (Just hIn, Just hOut, _, ph) <-
                createProcess
                    (proc oracleCommand oracleArgs)
                        { std_in = CreatePipe
                        , std_out = CreatePipe
                        , std_err = Inherit
                        }
            BSC.hPutStr hIn sql
            hClose hIn
            outBytes <- BSC.hGetContents hOut
            exitCode <- waitForProcess ph
            pure (exitCode, outBytes)

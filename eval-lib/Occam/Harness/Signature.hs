-- | Strongly-typed crash evidence, stack-frame normalization, and RQ3 signature classification.
module Occam.Harness.Signature (
    AssertionDetails (..),
    BaselineRef (..),
    Corroboration (..),
    CrashCategory (..),
    CrashSignature (..),
    DbmsKind (..),
    EvidenceResult (..),
    FaultEvent (..),
    FaultPayload (..),
    InternalFaultKind (..),
    SanitizerTool (..),
    SlippageMatchType (..),
    SlippageVerdict (..),
    buildCrashSignature,
    categoryName,
    crashSignals,
    dbmsKindName,
    decodeTerminationSignal,
    emptyCorroboration,
    evaluateSlippage,
    externalOrOomSignals,
    extractStackFrames,
    isServerDbmsKind,
    loadBaselineRef,
    normalizeSourcePath,
    normalizeVolatileTokens,
    parseDbmsKind,
    parseStackFrame,
) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (IOException, try)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (
    FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    decodeStrict',
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Char (isAlphaNum, isDigit, isHexDigit, isSpace)
import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)
import Text.Read (readMaybe)

-- | Supported target DBMS engines.
data DbmsKind
    = DbmsPostgres
    | DbmsMySql
    | DbmsMariaDb
    | DbmsClickHouse
    | DbmsSqlite
    | DbmsDuckDb
    deriving (Eq, Ord, Show, Enum, Bounded)

dbmsKindName :: DbmsKind -> Text
dbmsKindName = \case
    DbmsPostgres -> "postgres"
    DbmsMySql -> "mysql"
    DbmsMariaDb -> "mariadb"
    DbmsClickHouse -> "clickhouse"
    DbmsSqlite -> "sqlite"
    DbmsDuckDb -> "duckdb"

parseDbmsKind :: Text -> Maybe DbmsKind
parseDbmsKind raw = case T.toLower (T.strip raw) of
    "postgres" -> Just DbmsPostgres
    "postgresql" -> Just DbmsPostgres
    "pgsql" -> Just DbmsPostgres
    "mysql" -> Just DbmsMySql
    "mariadb" -> Just DbmsMariaDb
    "clickhouse" -> Just DbmsClickHouse
    "sqlite" -> Just DbmsSqlite
    "sqlite3" -> Just DbmsSqlite
    "duckdb" -> Just DbmsDuckDb
    _ -> Nothing

isServerDbmsKind :: DbmsKind -> Bool
isServerDbmsKind DbmsSqlite = False
isServerDbmsKind DbmsDuckDb = False
isServerDbmsKind _ = True

instance ToJSON DbmsKind where
    toJSON = String . dbmsKindName

instance FromJSON DbmsKind where
    parseJSON val = do
        txt <- parseJSON val
        maybe (fail ("Unknown DBMS: " <> T.unpack txt)) pure (parseDbmsKind txt)

-- | Parsed source/line/function/expression extracted from a DBMS assertion failure.
data AssertionDetails = AssertionDetails
    { assertFile :: !(Maybe Text)
    , assertLine :: !(Maybe Int)
    , assertFunction :: !(Maybe Text)
    , assertExpression :: !(Maybe Text)
    }
    deriving (Eq, Show)

data InternalFaultKind
    = KindDuckDbInternal
    | KindClickHouseLogical
    | KindDuckDbInvalidated
    deriving (Eq, Ord, Show)

data SanitizerTool = ToolAsan | ToolUbsan
    deriving (Eq, Ord, Show)

-- | Strongly-typed payload carried by a single fault event (no re-parsing via regexes).
data FaultPayload
    = PayloadAssertion !AssertionDetails
    | PayloadSignal !Int !(Maybe Text) !(Maybe Int)
    | PayloadBackendPanic !Text
    | PayloadInternal !InternalFaultKind !Text
    | PayloadSanitizer !(Maybe SanitizerTool)
    | PayloadSupervisorExit !Int !(Maybe Int)
    | PayloadChildException !Int !Text
    | PayloadChildExit !Int !Int
    | PayloadCrashRecovery
    deriving (Eq, Show)

-- | One fault indicator observed in DBMS logs or isolated process stderr.
data FaultEvent = FaultEvent
    { feDbms :: !DbmsKind
    , feKind :: !Text
    , feDetail :: !Text
    , fePayload :: !FaultPayload
    , fePid :: !(Maybe Int)
    , feChildPid :: !(Maybe Int)
    , feComponent :: !(Maybe Text)
    , feEventCode :: !(Maybe Text)
    , feSubsystem :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON FaultEvent where
    toJSON fe =
        object $
            [ "kind" .= feKind fe
            , "detail" .= feDetail fe
            ]
                <> maybeField "pid" (fePid fe)
                <> maybeField "child_pid" (feChildPid fe)
                <> maybeField "component" (feComponent fe)
                <> maybeField "event_code" (feEventCode fe)
                <> maybeField "subsystem" (feSubsystem fe)
                <> payloadFields (fePayload fe)
        where
            maybeField k = maybe [] (\v -> [k .= v])
            payloadFields = \case
                PayloadAssertion ad ->
                    maybeField "file" (assertFile ad)
                        <> maybeField "line" (assertLine ad)
                        <> maybeField "function" (assertFunction ad)
                        <> maybeField "assertion" (assertExpression ad)
                PayloadSignal sig mName mExit ->
                    ["signal" .= sig]
                        <> maybeField "signal_name" mName
                        <> maybeField "exit_status" mExit
                PayloadBackendPanic _ -> []
                PayloadInternal _ msg -> ["message" .= msg | not (T.null msg)]
                PayloadSanitizer _ -> []
                PayloadSupervisorExit st mSig ->
                    ["exit_status" .= st] <> maybeField "signal" mSig
                PayloadChildException _ exc -> ["exception" .= exc]
                PayloadChildExit _ code -> ["exit_code" .= code]
                PayloadCrashRecovery -> []

-- | Client/process corroboration flags (never standalone server crash proof).
data Corroboration = Corroboration
    { corConnectionLost :: !Bool
    , corSqlstate57p02 :: !Bool
    , corBackendPids :: ![Int]
    , corPostmasterChildPids :: ![Int]
    , corSupervisorExitCodes :: ![Int]
    , corRetcode :: !(Maybe Int)
    , corSignal :: !(Maybe Int)
    , corSqliteAbort :: !Bool
    , corDatabaseInvalidated :: !Bool
    , corOom :: !Bool
    , corOomCause :: !(Maybe Text)
    , corExternalKill :: !Bool
    }
    deriving (Eq, Show)

emptyCorroboration :: Corroboration
emptyCorroboration =
    Corroboration
        { corConnectionLost = False
        , corSqlstate57p02 = False
        , corBackendPids = []
        , corPostmasterChildPids = []
        , corSupervisorExitCodes = []
        , corRetcode = Nothing
        , corSignal = Nothing
        , corSqliteAbort = False
        , corDatabaseInvalidated = False
        , corOom = False
        , corOomCause = Nothing
        , corExternalKill = False
        }

instance ToJSON Corroboration where
    toJSON c =
        object
            [ "connection_lost" .= corConnectionLost c
            , "sqlstate_57p02" .= corSqlstate57p02 c
            , "backend_pids" .= corBackendPids c
            , "postmaster_child_pids" .= corPostmasterChildPids c
            , "supervisor_exit_codes" .= corSupervisorExitCodes c
            , "retcode" .= corRetcode c
            , "signal" .= corSignal c
            , "sqlite_abort" .= corSqliteAbort c
            , "database_invalidated" .= corDatabaseInvalidated c
            , "oom" .= corOom c
            , "oom_cause" .= corOomCause c
            , "external_kill" .= corExternalKill c
            ]

-- | Complete structured output from parsing one probe's server logs and/or stderr.
data EvidenceResult = EvidenceResult
    { erFaults :: ![FaultEvent]
    , erCandidateLines :: ![Text]
    , erCorroboration :: !Corroboration
    }
    deriving (Eq, Show)

-- | Normalized crash category for RQ3 slippage and deduplication.
data CrashCategory
    = CatAsan
    | CatUbsan
    | CatAssertion
    | CatInternalError
    | CatLogicalError
    | CatPanic
    | CatSignal
    | CatCrashRecovery
    | CatAbnormalExit
    deriving (Eq, Ord, Show)

categoryName :: CrashCategory -> Text
categoryName = \case
    CatAsan -> "asan"
    CatUbsan -> "ubsan"
    CatAssertion -> "assertion"
    CatInternalError -> "internal_error"
    CatLogicalError -> "logical_error"
    CatPanic -> "panic"
    CatSignal -> "signal"
    CatCrashRecovery -> "crash_recovery"
    CatAbnormalExit -> "abnormal_exit"

parseCategoryName :: Text -> Maybe CrashCategory
parseCategoryName = \case
    "asan" -> Just CatAsan
    "ubsan" -> Just CatUbsan
    "assertion" -> Just CatAssertion
    "internal_error" -> Just CatInternalError
    "logical_error" -> Just CatLogicalError
    "panic" -> Just CatPanic
    "signal" -> Just CatSignal
    "crash_recovery" -> Just CatCrashRecovery
    "abnormal_exit" -> Just CatAbnormalExit
    _ -> Nothing

instance ToJSON CrashCategory where
    toJSON = String . categoryName

instance FromJSON CrashCategory where
    parseJSON val = do
        txt <- parseJSON val
        maybe (fail ("Unknown crash category: " <> T.unpack txt)) pure (parseCategoryName txt)

-- | Structured RQ3 crash signature shared across Oracle, Replay, Collect, Batch, and BatchReport.
data CrashSignature = CrashSignature
    { csId :: !Text
    , csHash :: !Text
    , csCategory :: !CrashCategory
    , csSignal :: !(Maybe Text)
    , csSignalNum :: !(Maybe Int)
    , csAssertion :: !(Maybe Text)
    , csAsanError :: !(Maybe Text)
    , csFile :: !(Maybe Text)
    , csLine :: !(Maybe Int)
    , csFunction :: !(Maybe Text)
    , csExitCode :: !(Maybe Int)
    , csFrames :: ![Text]
    , csTopFrames :: !Int
    }
    deriving (Eq, Show)

instance ToJSON CrashSignature where
    toJSON cs =
        object
            [ "id" .= csId cs
            , "hash" .= csHash cs
            , "category" .= csCategory cs
            , "signal" .= csSignal cs
            , "signal_num" .= csSignalNum cs
            , "assertion" .= csAssertion cs
            , "asan_error" .= csAsanError cs
            , "file" .= csFile cs
            , "line" .= csLine cs
            , "function" .= csFunction cs
            , "exit_code" .= csExitCode cs
            , "frames" .= csFrames cs
            , "top_frames" .= csTopFrames cs
            ]

instance FromJSON CrashSignature where
    parseJSON = withObject "CrashSignature" \obj ->
        CrashSignature
            <$> obj .: "id"
            <*> obj .: "hash"
            <*> obj .: "category"
            <*> obj .:? "signal"
            <*> obj .:? "signal_num"
            <*> obj .:? "assertion"
            <*> obj .:? "asan_error"
            <*> obj .:? "file"
            <*> obj .:? "line"
            <*> obj .:? "function"
            <*> obj .:? "exit_code"
            <*> (fromMaybe [] <$> obj .:? "frames")
            <*> (fromMaybe 5 <$> obj .:? "top_frames")

data BaselineRef = BaselineRef
    { brRaw :: !Text
    , brId :: !(Maybe Text)
    , brHash :: !(Maybe Text)
    , brCategory :: !(Maybe Text)
    }
    deriving (Eq, Show)

data SlippageMatchType
    = MatchExactId
    | MatchExactHash
    | MatchMismatch
    deriving (Eq, Show)

matchTypeName :: SlippageMatchType -> Text
matchTypeName = \case
    MatchExactId -> "exact_id"
    MatchExactHash -> "exact_hash"
    MatchMismatch -> "mismatch"

data SlippageVerdict = SlippageVerdict
    { svBaseline :: !Text
    , svObservedId :: !Text
    , svObservedHash :: !Text
    , svSlipped :: !Bool
    , svMatchType :: !SlippageMatchType
    , svCategoryChanged :: !(Maybe Bool)
    }
    deriving (Eq, Show)

instance ToJSON SlippageVerdict where
    toJSON sv =
        object
            [ "baseline" .= svBaseline sv
            , "observed_id" .= svObservedId sv
            , "observed_hash" .= svObservedHash sv
            , "slipped" .= svSlipped sv
            , "match_type" .= matchTypeName (svMatchType sv)
            , "category_changed" .= svCategoryChanged sv
            ]

-- | Signals classified as actual DBMS crashes vs external/OOM termination.
crashSignals :: Set.Set Int
crashSignals = Set.fromList [4, 5, 6, 7, 8, 11, 16, 31]

externalOrOomSignals :: Set.Set Int
externalOrOomSignals = Set.fromList [9, 15]

signalNames :: Map Int Text
signalNames =
    Map.fromList
        [ (4, "SIGILL")
        , (5, "SIGTRAP")
        , (6, "SIGABRT")
        , (7, "SIGBUS")
        , (8, "SIGFPE")
        , (9, "SIGKILL")
        , (11, "SIGSEGV")
        , (13, "SIGPIPE")
        , (15, "SIGTERM")
        , (16, "SIGSTKFLT")
        , (31, "SIGSYS")
        ]

signalLabel :: Int -> Text
signalLabel n = fromMaybe ("SIG" <> T.pack (show n)) (Map.lookup n signalNames)

-- | Decode negative or shell-convention (>128) process termination signal codes.
decodeTerminationSignal :: Int -> Maybe (Int, Text)
decodeTerminationSignal retcode
    | retcode < 0 = let sig = negate retcode in Just (sig, signalLabel sig)
    | retcode > 128 && retcode <= 192 = let sig = retcode - 128 in Just (sig, signalLabel sig)
    | otherwise = Nothing

-- | Normalize build-specific paths while preserving @src/@, @storage/@, and @sql/@ relative tails when deep.
normalizeSourcePath :: Bool -> Text -> Maybe Text
normalizeSourcePath keepSrcRel rawPath =
    let cleaned = T.strip (T.replace "\\" "/" rawPath)
        noDot = fromMaybe cleaned (T.stripPrefix "./" cleaned)
        baseName = T.pack (takeFileName (T.unpack noDot))
     in if T.null noDot
            then Nothing
            else Just (if keepSrcRel then fromMaybe baseName (findMarker noDot) else baseName)
    where
        markers = ["/src/", "/storage/", "/sql/"]
        prefixes = ["src/", "storage/", "sql/"]

        findMarker path =
            findInfix markers path <|> findStart prefixes path

        findInfix [] _ = Nothing
        findInfix (m : ms) path = case T.breakOn m path of
            (_, match)
                | not (T.null match) ->
                    let rel = T.drop 1 match
                     in if hasSubdir rel then Just rel else findInfix ms path
            _ -> findInfix ms path

        findStart [] _ = Nothing
        findStart (p : ps) path
            | p `T.isPrefixOf` path && hasSubdir path = Just path
            | otherwise = findStart ps path

        hasSubdir p = case T.breakOn "/" p of
            (_, rest) -> "/" `T.isInfixOf` T.drop 1 rest

-- | Scrub ASLR hex addresses, UUIDs, PIDs, and redundant whitespace.
normalizeVolatileTokens :: Text -> Text
normalizeVolatileTokens = T.unwords . T.words . replaceTokens
    where
        replaceTokens txt
            | T.null txt = ""
            | Just rest <- stripHex txt = "0xADDR" <> replaceTokens rest
            | Just rest <- stripUuid txt = "<UUID>" <> replaceTokens rest
            | Just rest <- stripPid txt = "PID <N>" <> replaceTokens rest
            | otherwise = case T.uncons txt of
                Nothing -> ""
                Just (c, tl) ->
                    let (chunk, tailTxt) = T.break (\ch -> ch == '0' || ch == 'P' || isHexDigit ch) tl
                     in T.cons c chunk <> replaceTokens tailTxt

        stripHex t = case T.stripPrefix "0x" t of
            Just after0x ->
                let digits = T.takeWhile isHexDigit after0x
                 in if T.null digits then Nothing else Just (T.drop (T.length digits) after0x)
            Nothing -> Nothing

        stripUuid t =
            let cand = T.take 36 t
                after = T.drop 36 t
                beforeOk = True
                afterOk = case T.uncons after of
                    Nothing -> True
                    Just (c, _) -> not (isAlphaNum c || c == '_')
             in if T.length cand == 36 && beforeOk && afterOk && isUuidShape (T.unpack cand)
                    then Just after
                    else Nothing

        isUuidShape cs =
            length cs == 36
                && all isHexDigit (take 8 cs)
                && cs `charAt` 8 == '-'
                && all isHexDigit (slice 9 4 cs)
                && cs `charAt` 13 == '-'
                && all isHexDigit (slice 14 4 cs)
                && cs `charAt` 18 == '-'
                && all isHexDigit (slice 19 4 cs)
                && cs `charAt` 23 == '-'
                && all isHexDigit (drop 24 cs)
            where
                slice off len = take len . drop off
                charAt xs i = case drop i xs of
                    (x : _) -> x
                    [] -> '\0'

        stripPid t = do
            afterKw <- T.stripPrefix "PID" t
            let ws = T.takeWhile isSpace afterKw
            if T.null ws
                then Nothing
                else do
                    let afterWs = T.drop (T.length ws) afterKw
                        digits = T.takeWhile isDigit afterWs
                        afterDigits = T.drop (T.length digits) afterWs
                    if T.null digits || maybe False ((\c -> isAlphaNum c || c == '_') . fst) (T.uncons afterDigits)
                        then Nothing
                        else Just afterDigits

wrapperPrefixes :: [Text]
wrapperPrefixes =
    [ "__asan"
    , "__ubsan"
    , "__lsan"
    , "__msan"
    , "__tsan"
    , "__sanitizer"
    , "__interceptor_"
    , "__gi_raise"
    , "__gi_abort"
    , "__gi___assert_fail"
    , "__assert_fail"
    , "__pthread_kill"
    , "pthread_kill"
    , "__restore_rt"
    , "handle_fatal_signal"
    , "my_print_stacktrace"
    , "print_stacktrace"
    , "ut_dbg_assertion_failed"
    , "basedaemon"
    , "poco::signalhandler"
    , "signalhandler"
    , "stacktrace::"
    , "db::abortonfailedassertion"
    , "__libc_start_"
    , "libpthread.so"
    , "libc.so"
    , "libasan.so"
    , "libubsan.so"
    , "linux-vdso.so"
    ]

wrapperExactFuncs :: Set.Set Text
wrapperExactFuncs = Set.fromList ["abort", "raise", "gsignal", "killpg", "_start"]

isWrapperFrame :: Text -> Bool
isWrapperFrame frame =
    let (rawFunc, rawMod) = T.breakOn "@" frame
        funcPart = T.toLower (T.strip rawFunc)
        modPart = T.toLower (T.strip (T.drop 1 rawMod))
     in ("libasan.so" `T.isPrefixOf` modPart || "libubsan.so" `T.isPrefixOf` modPart)
            || Set.member funcPart wrapperExactFuncs
            || any (`T.isPrefixOf` funcPart) wrapperPrefixes

-- | Parse one stack frame line across ASan, ClickHouse BaseDaemon, glibc backtrace(), and MySQL formats.
parseStackFrame :: Text -> Maybe Text
parseStackFrame rawLine =
    let line = T.strip rawLine
     in if T.null line
            then Nothing
            else
                parseAsanFrame line
                    <|> parseClickHouseFrame line
                    <|> parseGlibcBtFrame line
                    <|> parseMysqlBtFileFrame line

parseAsanFrame :: Text -> Maybe Text
parseAsanFrame line = do
    afterHash <- T.stripPrefix "#" line
    let frameNum = T.takeWhile isDigit afterHash
    if T.null frameNum
        then Nothing
        else do
            afterNum <- skipWs1 (T.drop (T.length frameNum) afterHash)
            after0x <- T.stripPrefix "0x" afterNum
            let hexAddr = T.takeWhile isHexDigit after0x
            if T.null hexAddr
                then Nothing
                else do
                    afterAddr <- skipWs1 (T.drop (T.length hexAddr) after0x)
                    case T.stripPrefix "in" afterAddr >>= skipWs1 of
                        Just rest -> parseAsanIn rest
                        Nothing -> parseAsanModOnly afterAddr
    where
        parseAsanIn rest =
            let cleaned = stripBuildId rest
             in parseAsanFileLoc cleaned <|> parseAsanModSuffix cleaned

        parseAsanFileLoc rest =
            case T.breakOnEnd " " rest of
                (funcSp, locTok)
                    | not (T.null funcSp) -> do
                        let func = T.unwords (T.words funcSp)
                        (srcPath, lineNo) <- parseSrcColonLine locTok
                        normFile <- normalizeSourcePath True srcPath
                        pure (func <> "@" <> normFile <> ":" <> lineNo)
                _ -> Nothing

        parseAsanModSuffix rest = do
            let s = T.stripEnd rest
            withoutClose <- T.stripSuffix ")" s
            let (beforeOpen, modPart) = T.breakOnEnd " (" withoutClose
            if T.null beforeOpen || T.null modPart
                then Nothing
                else do
                    let func = T.unwords (T.words (T.dropEnd 2 beforeOpen))
                    pure (func <> "@" <> formatModBase modPart)

        parseAsanModOnly rest = do
            let s = T.stripEnd (stripBuildId rest)
            afterOpen <- T.stripPrefix "(" s
            modPart <- T.stripSuffix ")" afterOpen
            if T.null modPart then Nothing else pure (formatModBase modPart)

        stripBuildId txt = case T.breakOnEnd " (BuildId: " txt of
            (prefix, suffix)
                | not (T.null prefix) && ")" `T.isSuffixOf` suffix ->
                    T.stripEnd (T.dropEnd 11 prefix)
            _ -> txt

        formatModBase modTxt =
            let (pathPart, plusOff) = T.breakOn "+" (T.strip modTxt)
                baseMod = T.pack (takeFileName (T.unpack pathPart))
             in baseMod <> plusOff

parseSrcColonLine :: Text -> Maybe (Text, Text)
parseSrcColonLine tok =
    let cleanTok = stripTrailingCol tok
        (fPart, lineRest) = T.breakOnEnd ":" cleanTok
     in if T.null fPart || T.null lineRest || not (T.all isDigit lineRest)
            then Nothing
            else
                let srcFile = T.dropEnd 1 fPart
                 in if hasValidSrcExt srcFile && validLocPath srcFile
                        then Just (srcFile, lineRest)
                        else Nothing
    where
        stripTrailingCol t = case T.breakOnEnd ":" t of
            (pref, suf)
                | not (T.null pref) && not (T.null suf) && T.all isDigit suf ->
                    case T.breakOnEnd ":" (T.dropEnd 1 pref) of
                        (p2, prevDigits)
                            | not (T.null p2) && not (T.null prevDigits) && T.all isDigit prevDigits ->
                                T.dropEnd 1 pref
                        _ -> t
            _ -> t

        validLocPath p =
            "/" `T.isPrefixOf` p
                || "./" `T.isPrefixOf` p
                || "../" `T.isPrefixOf` p
                || T.all (\c -> isAlphaNum c || c `elem` ("_./-" :: String)) p

hasValidSrcExt :: Text -> Bool
hasValidSrcExt p =
    any
        (`T.isSuffixOf` p)
        [ ".c"
        , ".cc"
        , ".cpp"
        , ".cxx"
        , ".h"
        , ".hpp"
        , ".hh"
        , ".rs"
        , ".y"
        , ".l"
        , ".tcc"
        , ".ipp"
        , ".inc"
        , ".inl"
        ]

parseClickHouseFrame :: Text -> Maybe Text
parseClickHouseFrame line =
    let chMsg = stripChPrefix line
        (numPrefix, afterNum) = spanClickHouseNum chMsg
     in if T.null numPrefix
            then Nothing
            else do
                afterDot <- T.stripPrefix "." afterNum >>= skipWs1
                let body = fromMaybe afterDot (T.stripPrefix "inlined from" afterDot >>= skipWs1)
                    withoutAddr = stripAtAddr body
                parseWithFile withoutAddr <|> parseFuncOnly withoutAddr
    where
        stripChPrefix s
            | Just rest <- stripChTimestampLine s = rest
            | Just rest <- T.stripPrefix "<Fatal> " s = case T.breakOn ":" rest of
                (_, after) | not (T.null after) -> T.strip (T.drop 1 after)
                _ -> s
            | otherwise = s

        spanClickHouseNum s =
            let d1 = T.takeWhile isDigit s
                r1 = T.drop (T.length d1) s
             in case T.stripPrefix "." r1 of
                    Just r2 ->
                        let d2 = T.takeWhile isDigit r2
                         in if T.null d2 then (d1, r1) else (d1 <> "." <> d2, T.drop (T.length d2) r2)
                    Nothing -> (d1, r1)

        stripAtAddr txt = case T.breakOnEnd " @ 0x" txt of
            (pref, suf)
                | not (T.null pref) ->
                    let afterAddr = T.dropWhile isHexDigit suf
                     in if T.null afterAddr || " in " `T.isPrefixOf` afterAddr
                            then T.stripEnd (T.dropEnd 5 pref)
                            else txt
            _ -> txt

        parseWithFile body = case T.breakOn ": " body of
            (fileAndLine, colonRest)
                | not (T.null colonRest) -> do
                    let func = T.unwords (T.words (T.drop 2 colonRest))
                    (srcPath, lineNo) <- splitFileLine fileAndLine
                    if T.any isSpace srcPath || T.null func
                        then Nothing
                        else do
                            normFile <- normalizeSourcePath True srcPath
                            pure (func <> "@" <> normFile <> ":" <> lineNo)
            _ -> Nothing

        splitFileLine tok = case T.breakOnEnd ":" tok of
            (pref, digits)
                | not (T.null pref) && not (T.null digits) && T.all isDigit digits ->
                    Just (T.dropEnd 1 pref, digits)
            _ -> Nothing

        parseFuncOnly rawFunc =
            let func = T.unwords (T.words rawFunc)
             in if T.null func
                    then Nothing
                    else case T.breakOn "(+" func of
                        (modPart, offPart)
                            | "/" `T.isPrefixOf` func && not (T.null offPart) ->
                                Just (T.pack (takeFileName (T.unpack modPart)) <> "+" <> T.dropEnd 1 (T.drop 2 offPart))
                        _ -> Just func

stripChTimestampLine :: Text -> Maybe Text
stripChTimestampLine s
    | T.length s >= 19
        && isChDate (T.unpack (T.take 19 s)) = do
        let (_, openB) = T.breakOn "[" s
        if T.null openB
            then Nothing
            else do
                let (_, closeB) = T.breakOn "]" openB
                    afterB = T.dropWhile isSpace (T.drop 1 closeB)
                afterBrace <- T.stripPrefix "{" afterB
                let (_, closeQ) = T.breakOn "}" afterBrace
                if T.null closeQ
                    then Nothing
                    else do
                        let afterQ = T.dropWhile isSpace (T.drop 1 closeQ)
                        afterLt <- T.stripPrefix "<" afterQ
                        let (lvl, closeGt) = T.breakOn ">" afterLt
                        if T.null lvl || T.null closeGt
                            then Nothing
                            else do
                                let (_, colon) = T.breakOn ":" (T.drop 1 closeGt)
                                if T.null colon then Nothing else Just (T.strip (T.drop 1 colon))
    | otherwise = Nothing
    where
        isChDate cs = case splitAt 4 cs of
            (yr, sep1 : rest1) ->
                length yr == 4
                    && all isDigit yr
                    && (sep1 == '-' || sep1 == '.')
                    && case drop 5 rest1 of
                        (sep2 : _) -> sep2 == 'T' || sep2 == ' '
                        [] -> False
            _ -> False

parseGlibcBtFrame :: Text -> Maybe Text
parseGlibcBtFrame line = do
    withoutBracket <- T.stripSuffix "]" line
    let (prefix, addr) = T.breakOnEnd " [0x" withoutBracket
        (compactPrefix, compactAddr) = T.breakOnEnd "[0x" withoutBracket
        chosen
            | not (T.null prefix) && validHex addr = Just (T.stripEnd (T.dropEnd 4 prefix))
            | not (T.null compactPrefix) && validHex compactAddr = Just (T.stripEnd (T.dropEnd 3 compactPrefix))
            | otherwise = Nothing
    body <- chosen
    withoutClose <- T.stripSuffix ")" body
    let (binPart, symRaw) = T.breakOn "(" withoutClose
    if T.null binPart || T.null symRaw || T.any (\c -> isSpace c || c == ':') binPart
        then Nothing
        else do
            let sym = T.strip (T.drop 1 symRaw)
                binBase = T.pack (takeFileName (T.unpack binPart))
            pure $
                if T.null sym
                    then binBase
                    else
                        if "+0x" `T.isPrefixOf` sym
                            then binBase <> sym
                            else stripTrailingOffset sym
    where
        validHex t = not (T.null t) && T.all isHexDigit t
        stripTrailingOffset s = case T.breakOnEnd "+0x" s of
            (pref, suf)
                | not (T.null pref) && validHex suf -> T.strip (T.dropEnd 3 pref)
            _ -> s

parseMysqlBtFileFrame :: Text -> Maybe Text
parseMysqlBtFileFrame line = do
    withoutClose <- T.stripSuffix ")" line
    let (locPart, funcRaw) = T.breakOn "(" withoutClose
    if T.null locPart || T.null funcRaw
        then Nothing
        else do
            (srcFile, lineNo) <- splitSrcLine locPart
            if T.any (\c -> isSpace c || c == '(' || c == ')') srcFile || not (hasMysqlExt srcFile)
                then Nothing
                else do
                    normFile <- normalizeSourcePath True srcFile
                    pure (T.strip (T.drop 1 funcRaw) <> "@" <> normFile <> ":" <> lineNo)
    where
        splitSrcLine tok = case T.breakOnEnd ":" tok of
            (p, d) | not (T.null p) && not (T.null d) && T.all isDigit d -> Just (T.dropEnd 1 p, d)
            _ -> Nothing
        hasMysqlExt f = any (`T.isSuffixOf` f) [".c", ".cc", ".cpp", ".cxx", ".h", ".hpp", ".y", ".l"]

-- | Extract up to N deduplicated non-wrapper stack frames from raw lines.
extractStackFrames :: Int -> [Text] -> [Text]
extractStackFrames topN linesList =
    let rawFrames = reverse (fst (Foldable.foldl' step ([], Set.empty) linesList))
        filtered = filter (not . isWrapperFrame) rawFrames
        chosen = if null filtered then rawFrames else filtered
     in take (max 1 topN) chosen
    where
        step acc@(frames, seen) ln = case parseStackFrame ln of
            Just f | Set.notMember f seen -> (f : frames, Set.insert f seen)
            _ -> acc

-- | Construct a canonical SHA-256 RQ3 crash signature from typed fault events and frames.
buildCrashSignature :: DbmsKind -> [FaultEvent] -> [Text] -> Int -> Int -> Maybe CrashSignature
buildCrashSignature _ [] _ _ _ = Nothing
buildCrashSignature dbms faults candLines retcode topFrames =
    let linesAll = map feDetail faults <> candLines
        frames = extractStackFrames topFrames linesAll
        (mFaultSig, mFaultExit) = foldFaultSignals faults
        (mSigNum, mSigName) = case mFaultSig of
            Just (s, name) -> (Just s, Just name)
            Nothing -> case decodeTerminationSignal retcode of
                Just (s, name) | Set.member s crashSignals -> (Just s, Just name)
                _ -> (Nothing, Nothing)
        (asanErr, ubsanErr, asanLoc, ubsanLoc, ubsanFunc) = scanSanitizerLines linesAll
        hasAssertion = any isAssertionFault faults
        assertionDetails = findFirstAssertion faults
        internalFault = findFirstInternal faults
        panicFault = findFirstPanic faults
        childExc = findChildException faults
        dbInvalid = any (\f -> feKind f == "duckdb_database_invalidated") faults
        pgCrashRec = any (\f -> feKind f == "pg_postmaster_crash_recovery") faults

        (cat, finalSigNum, finalSigName, finalFile, finalLine, finalFunc, finalAssert, sigId) =
            case asanErr of
                Just aErr
                    | not (hasAssertion && (aErr == "ABRT" || aErr == "DEADLYSIGNAL")) ->
                        let loc = listHead frames <|> asanLoc
                            parts = ["asan", aErr] <> maybe [] pure loc
                         in (CatAsan, mSigNum, mSigName, Nothing, Nothing, Nothing, Nothing, T.intercalate ":" parts)
                _ -> case ubsanErr of
                    Just uErr
                        | not hasAssertion ->
                            let (uFile, uLine) = fromMaybe (Nothing, Nothing) ubsanLoc
                                fallbackLoc = case (uFile, uLine) of
                                    (Just f, Just l) -> Just (f <> ":" <> T.pack (show l))
                                    _ -> asanLoc
                                loc = listHead frames <|> fallbackLoc
                                parts = ["ubsan", uErr] <> maybe [] pure loc
                             in (CatUbsan, mSigNum, mSigName, uFile, uLine, Nothing, Nothing, T.intercalate ":" parts)
                    _
                        | hasAssertion ->
                            let (fName, lNum, fnName, aExpr) = assertionDetails
                                (sNum, sName) = case mSigNum of
                                    Nothing | dbms /= DbmsClickHouse -> (Just 6, Just "SIGABRT")
                                    _ -> (mSigNum, mSigName)
                                idParts =
                                    ["assertion"]
                                        <> maybe [] pure fName
                                        <> maybe [] (pure . T.pack . show) lNum
                                        <> maybe [] pure fnName
                                        <> maybe [] pure aExpr
                             in (CatAssertion, sNum, sName, fName, lNum, fnName, aExpr, T.intercalate ":" idParts)
                        | Just (iKind, iMsg) <- internalFault ->
                            let c = if iKind == KindDuckDbInternal then CatInternalError else CatLogicalError
                                expr = normalizeVolatileTokens iMsg
                                sid = if T.null expr then categoryName c else categoryName c <> ":" <> expr
                             in (c, mSigNum, mSigName, Nothing, Nothing, Nothing, Just expr, sid)
                        | Just pMsg <- panicFault ->
                            let body = T.strip (fromMaybe pMsg (T.stripPrefix "PANIC:" pMsg))
                                expr = normalizeVolatileTokens body
                             in (CatPanic, mSigNum, mSigName, Nothing, Nothing, Nothing, Just expr, "panic:" <> expr)
                        | Just sNum <- mSigNum
                        , Just sName <- mSigName ->
                            let sid =
                                    if null frames
                                        then "signal:" <> sName
                                        else "signal:" <> sName <> ":" <> T.intercalate "|" frames
                             in (CatSignal, Just sNum, Just sName, Nothing, Nothing, Nothing, Nothing, sid)
                        | Just exc <- childExc ->
                            let sName = "EXC_" <> T.toUpper exc
                             in (CatSignal, Nothing, Just sName, Nothing, Nothing, Nothing, Nothing, "signal:" <> sName)
                        | dbInvalid ->
                            ( CatInternalError
                            , Nothing
                            , Nothing
                            , Nothing
                            , Nothing
                            , Nothing
                            , Just "database invalidated"
                            , "internal_error:database invalidated"
                            )
                        | pgCrashRec ->
                            ( CatCrashRecovery
                            , Nothing
                            , Nothing
                            , Nothing
                            , Nothing
                            , Nothing
                            , Nothing
                            , "crash_recovery:postmaster"
                            )
                        | otherwise ->
                            let codeVal = fromMaybe retcode mFaultExit
                             in ( CatAbnormalExit
                                , Nothing
                                , Nothing
                                , Nothing
                                , Nothing
                                , Nothing
                                , Nothing
                                , "abnormal_exit:" <> T.pack (show codeVal)
                                )

        sigHash = T.take 16 (decodeUtf8 (Base16.encode (SHA256.hash (encodeUtf8 sigId))))
     in Just
            CrashSignature
                { csId = sigId
                , csHash = sigHash
                , csCategory = cat
                , csSignal = finalSigName
                , csSignalNum = finalSigNum
                , csAssertion = finalAssert
                , csAsanError = asanErr <|> ubsanErr
                , csFile = finalFile
                , csLine = finalLine
                , csFunction = finalFunc <|> ubsanFunc
                , csExitCode = mFaultExit
                , csFrames = frames
                , csTopFrames = topFrames
                }
    where
        listHead [] = Nothing
        listHead (x : _) = Just x

        foldFaultSignals = Foldable.foldl' step (Nothing, Nothing)
            where
                step (mSig, mEx) fe = case fePayload fe of
                    PayloadSignal s _ mEs -> (mSig <|> Just (s, signalLabel s), mEx <|> mEs)
                    PayloadSupervisorExit st mS -> (mSig <|> fmap (\s -> (s, signalLabel s)) mS, mEx <|> Just st)
                    PayloadChildExit _ code -> (mSig, mEx <|> Just code)
                    _ -> (mSig, mEx)

        isAssertionFault fe = case fePayload fe of
            PayloadAssertion _ -> True
            _ -> False

        findFirstAssertion = Foldable.foldl' merge (Nothing, Nothing, Nothing, Nothing)
            where
                merge acc@(f0, l0, fn0, ex0) fe = case fePayload fe of
                    PayloadAssertion ad ->
                        ( f0 <|> assertFile ad
                        , l0 <|> assertLine ad
                        , fn0 <|> assertFunction ad
                        , ex0 <|> fmap (T.unwords . T.words) (assertExpression ad)
                        )
                    _ -> acc

        findFirstInternal = Foldable.foldl' go Nothing
            where
                go acc fe =
                    acc <|> case fePayload fe of
                        PayloadInternal k msg | k /= KindDuckDbInvalidated -> Just (k, msg)
                        _ -> Nothing

        findFirstPanic = Foldable.foldl' go Nothing
            where
                go acc fe =
                    acc <|> case fePayload fe of
                        PayloadBackendPanic msg -> Just msg
                        _ -> Nothing

        findChildException = Foldable.foldl' go Nothing
            where
                go acc fe =
                    acc <|> case fePayload fe of
                        PayloadChildException _ exc -> Just exc
                        _ -> Nothing

scanSanitizerLines ::
    [Text] -> (Maybe Text, Maybe Text, Maybe Text, Maybe (Maybe Text, Maybe Int), Maybe Text)
scanSanitizerLines = Foldable.foldl' step (Nothing, Nothing, Nothing, Nothing, Nothing)
    where
        step (aErr, uErr, aLoc, uLoc, uFunc) rawLine =
            let trimmed = T.strip rawLine
                aErr' = aErr <|> parseAsanKind rawLine
                (sAsan, sUbsan, sLoc, sUbsanFunc) = parseSummary trimmed
                (rtLoc, rtMsg) = parseUbsanRuntime trimmed
             in ( aErr' <|> sAsan
                , uErr <|> sUbsan <|> rtMsg
                , aLoc <|> sLoc
                , uLoc <|> rtLoc
                , uFunc <|> sUbsanFunc
                )

        parseAsanKind ln = case T.breakOn "AddressSanitizer:" ln of
            (_, rest)
                | not (T.null rest) ->
                    let kind = T.takeWhile (\c -> not (isSpace c) && c /= ':') (T.dropWhile isSpace (T.drop 17 rest))
                     in if T.null kind then Nothing else Just kind
            _ -> Nothing

        parseSummary s = case T.stripPrefix "SUMMARY:" s of
            Nothing -> (Nothing, Nothing, Nothing, Nothing)
            Just rest ->
                let afterWs = T.dropWhile isSpace rest
                 in case T.stripPrefix "AddressSanitizer:" afterWs of
                        Just afterTool -> extractSummary ToolAsan afterTool
                        Nothing -> case T.stripPrefix "UndefinedBehaviorSanitizer:" afterWs of
                            Just afterTool -> extractSummary ToolUbsan afterTool
                            Nothing -> (Nothing, Nothing, Nothing, Nothing)

        extractSummary tool afterTool =
            let stripped = T.dropWhile isSpace afterTool
                (kind, afterKind) = T.break isSpace stripped
                (locPart, funcPart) = case T.breakOn " in " (T.dropWhile isSpace afterKind) of
                    (l, f) | not (T.null f) -> (T.strip l, Just (T.strip (T.drop 4 f)))
                    _ -> (T.strip afterKind, Nothing)
                formattedLoc = case T.splitOn ":" locPart of
                    (p : lnTxt : _)
                        | not (T.null lnTxt) && T.all isDigit lnTxt -> do
                            nf <- normalizeSourcePath True p
                            let base = nf <> ":" <> lnTxt
                            pure (maybe base (\fn -> fn <> "@" <> base) funcPart)
                    _ -> Nothing
             in if T.null kind
                    then (Nothing, Nothing, Nothing, Nothing)
                    else case tool of
                        ToolAsan -> (Just kind, Nothing, formattedLoc, Nothing)
                        ToolUbsan -> (Nothing, Just kind, formattedLoc, funcPart)

        parseUbsanRuntime s = case T.breakOn ": runtime error: " s of
            (locPrefix, afterColon)
                | not (T.null afterColon) ->
                    let msg = T.drop 17 afterColon
                        kind = normalizeVolatileTokens (T.strip (fst (T.breakOn ":" msg)))
                        parts = T.splitOn ":" locPrefix
                     in case reverse parts of
                            (colOrLine : lineOrFile : revRest)
                                | not (T.null colOrLine) && T.all isDigit colOrLine ->
                                    if length parts >= 3 && not (T.null lineOrFile) && T.all isDigit lineOrFile
                                        then
                                            let f = normalizeSourcePath False (T.intercalate ":" (reverse revRest))
                                                l = readMaybe (T.unpack lineOrFile)
                                             in (Just (f, l), Just kind)
                                        else
                                            let f = normalizeSourcePath False (T.intercalate ":" (reverse (lineOrFile : revRest)))
                                                l = readMaybe (T.unpack colOrLine)
                                             in (Just (f, l), Just kind)
                            _ -> (Nothing, Nothing)
            _ -> (Nothing, Nothing)

-- | Load a baseline reference from inline JSON, signature ID/hash string, or a JSON/JSONL file.
loadBaselineRef :: Text -> IO (Maybe BaselineRef)
loadBaselineRef raw =
    let trimmed = T.strip raw
     in if T.null trimmed
            then pure Nothing
            else do
                isFile <- doesPathExist trimmed
                if isFile
                    then do
                        res <- try (BS.readFile (T.unpack trimmed)) :: IO (Either IOException BS.ByteString)
                        case res of
                            Right bytes -> case parseBaselineContent trimmed bytes of
                                Just br -> pure (Just br)
                                Nothing -> pure (Just (literalBaseline trimmed))
                            Left _ -> pure (Just (literalBaseline trimmed))
                    else
                        pure $
                            if "{" `T.isPrefixOf` trimmed
                                then Just (fromMaybe (literalBaseline trimmed) (decodeJsonBaseline trimmed (encodeUtf8 trimmed)))
                                else Just (literalBaseline trimmed)
    where
        doesPathExist p = doesFileExist (T.unpack p)

        literalBaseline txt =
            let cat = case T.breakOn ":" txt of
                    (c, rest) | not (T.null rest) -> Just c
                    _ -> Nothing
             in BaselineRef txt (Just txt) (Just txt) cat

        parseBaselineContent orig bytes =
            decodeJsonBaseline orig bytes
                <|> Foldable.foldl'
                    (\acc ln -> acc <|> decodeJsonBaseline orig (encodeUtf8 (T.strip ln)))
                    Nothing
                    (T.lines (decodeUtf8 bytes))

decodeJsonBaseline :: Text -> BS.ByteString -> Maybe BaselineRef
decodeJsonBaseline orig bytes = do
    val <- decodeStrict' bytes
    case val of
        Object obj -> extractSigDict orig obj
        _ -> Nothing

extractSigDict :: Text -> Object -> Maybe BaselineRef
extractSigDict raw origObj =
    let targetObj = case KM.lookup "crash_signature" origObj of
            Just (Object inner) -> inner
            _ -> origObj
        mId = kmText "id" targetObj
        mHash = kmText "hash" targetObj
        mCat = kmText "category" targetObj
     in if isJust mId || isJust mHash
            then Just (BaselineRef raw mId mHash mCat)
            else Nothing
    where
        kmText k obj = case KM.lookup k obj of
            Just (String t) -> Just t
            _ -> Nothing

-- | Compare an observed signature against a baseline reference for RQ3 slippage detection.
evaluateSlippage :: BaselineRef -> CrashSignature -> SlippageVerdict
evaluateSlippage base obs =
    let obsId = csId obs
        obsHash = csHash obs
        obsCat = categoryName (csCategory obs)
        (matchType, slipped) = case brId base of
            Just bid | not (T.null bid) && obsId == bid -> (MatchExactId, False)
            _ -> case brHash base of
                Just bhash | not (T.null bhash) && obsHash == bhash -> (MatchExactHash, False)
                _ -> (MatchMismatch, True)
        catChanged = fmap (/= obsCat) (brCategory base)
     in SlippageVerdict
            { svBaseline = brRaw base
            , svObservedId = obsId
            , svObservedHash = obsHash
            , svSlipped = slipped
            , svMatchType = matchType
            , svCategoryChanged = catChanged
            }

skipWs1 :: Text -> Maybe Text
skipWs1 txt = case T.uncons txt of
    Just (c, _) | isSpace c -> Just (T.dropWhile isSpace txt)
    _ -> Nothing

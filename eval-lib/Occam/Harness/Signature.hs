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
import Data.Char (isDigit, isSpace)
import Data.Foldable qualified as Foldable
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Occam.Harness.Signature.StackFrame
import System.Directory (doesFileExist)
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

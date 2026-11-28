-- | Shared line splitting, sanitizer/assertion parsers, and fault builders for DBMS crash evidence.
module Occam.Harness.Evidence.Common (
    EmbeddedSanitizerContext (..),
    applyEmbeddedProcessSignal,
    clickHouseConnectionLostPhrases,
    connectionLostPhrasesForDbms,
    embeddedSanitizerContext,
    hasAsanInLine,
    hasPsqlMetaCommand,
    looksLikeBacktraceLine,
    makeFault,
    mariadbConnectionLostPhrases,
    matchesDateTemplate,
    mysqlConnectionLostPhrases,
    parseAsanBanner,
    parseAsanSummary,
    parseBsdAssertion,
    parseCAssertion,
    parseUbsanRuntime,
    pgConnectionLostPhrases,
    pgCrashShutdownPhrases,
    skipWs1,
    splitLinesPy,
) where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Harness.Signature (
    AssertionDetails (..),
    DbmsKind (..),
    FaultEvent (..),
    FaultPayload (..),
    crashSignals,
    dbmsKindName,
    decodeTerminationSignal,
    externalOrOomSignals,
    normalizeSourcePath,
    parseStackFrame,
 )
import Occam.Harness.Signature.StackFrame (skipWs1)
import Text.Read (readMaybe)

-- | Stderr phrases indicating a dropped PostgreSQL client connection.
pgConnectionLostPhrases :: [Text]
pgConnectionLostPhrases =
    [ "server closed the connection unexpectedly"
    , "connection to server was lost"
    , "could not receive data from server"
    ]

-- | PostgreSQL SQLSTATE\/stderr markers indicating backend termination after another worker crashed.
pgCrashShutdownPhrases :: [Text]
pgCrashShutdownPhrases =
    [ "57P02"
    , "terminating connection because of crash of another server process"
    ]

-- | Stderr phrases indicating a dropped MySQL client connection.
mysqlConnectionLostPhrases :: [Text]
mysqlConnectionLostPhrases =
    [ "Lost connection to MySQL server"
    , "MySQL server has gone away"
    , "ERROR 2006"
    , "ERROR 2013"
    ]

-- | Stderr phrases indicating a dropped MariaDB client connection.
mariadbConnectionLostPhrases :: [Text]
mariadbConnectionLostPhrases =
    mysqlConnectionLostPhrases
        <> [ "Lost connection to server"
           , "Server has gone away"
           ]

-- | Stderr phrases indicating a dropped ClickHouse client connection.
clickHouseConnectionLostPhrases :: [Text]
clickHouseConnectionLostPhrases =
    [ "Code: 210"
    , "Code: 32"
    , "Connection reset by peer"
    , "Attempt to read after eof"
    , "Unexpected end of file"
    , "Cannot read from socket"
    , "Connection refused"
    , "DB::NetException"
    ]

-- | Return client connection-loss stderr markers for a given DBMS engine.
connectionLostPhrasesForDbms :: DbmsKind -> [Text]
connectionLostPhrasesForDbms = \case
    DbmsPostgres -> pgConnectionLostPhrases
    DbmsMySql -> mysqlConnectionLostPhrases
    DbmsMariaDb -> mariadbConnectionLostPhrases
    DbmsClickHouse -> clickHouseConnectionLostPhrases
    DbmsSqlite -> []
    DbmsDuckDb -> []

-- | Reject PostgreSQL @psql@ backslash meta-commands when @--allow-psql-meta@ is disabled.
hasPsqlMetaCommand :: Text -> Bool
hasPsqlMetaCommand = any (T.isPrefixOf "\\" . T.stripStart) . splitLinesPy

-- | Split text along CR\/CRLF\/LF boundaries matching Python @str.splitlines()@ trailing-newline rules.
splitLinesPy :: Text -> [Text]
splitLinesPy raw
    | T.null raw = []
    | otherwise =
        let normalized = T.replace "\r\n" "\n" raw
            crNorm = T.replace "\r" "\n" normalized
            withoutEnd = fromMaybe crNorm (T.stripSuffix "\n" crNorm)
         in T.splitOn "\n" withoutEnd

-- | True when a trimmed diagnostic line resembles a debugger, sanitizer, or C++ stack frame.
looksLikeBacktraceLine :: Text -> Bool
looksLikeBacktraceLine rawLine =
    let s = T.strip rawLine
     in case parseStackFrame s of
            Nothing -> False
            Just _ ->
                T.isPrefixOf "#" s
                    || " [0x" `T.isInfixOf` s
                    || "[0x" `T.isInfixOf` s
                    || isMysqlBt s
    where
        isMysqlBt line =
            case T.stripSuffix ")" line of
                Just body ->
                    let (loc, fn) = T.breakOn "(" body
                     in not (T.null fn) && any (\ext -> (ext <> ":") `T.isInfixOf` loc) srcExts
                Nothing -> False
        srcExts = [".c", ".cc", ".cpp", ".cxx", ".h", ".hpp", ".y", ".l"]

-- | Return 'True' when a log line contains an inline @AddressSanitizer:@ diagnostic banner.
hasAsanInLine :: Text -> Bool
hasAsanInLine ln = case T.breakOn "AddressSanitizer:" ln of
    (_, rest)
        | not (T.null rest) ->
            let tok = T.takeWhile (\c -> not (isSpace c) && c /= ':') (T.dropWhile isSpace (T.drop 17 rest))
             in not (T.null tok)
    _ -> False

-- | Parse an @==PID==ERROR: AddressSanitizer: <kind>@ header line to extract the sanitizer fault kind.
parseAsanBanner :: Text -> Maybe Text
parseAsanBanner s =
    let rest0 = case T.stripPrefix "==" s of
            Just r ->
                let digits = T.takeWhile isDigit r
                    r1 = T.drop (T.length digits) r
                 in case T.stripPrefix "==ERROR:" r1 <|> T.stripPrefix "== ERROR:" r1 of
                        Just afterErr | not (T.null digits) -> T.dropWhile isSpace afterErr
                        _ -> s
            Nothing -> s
     in case T.stripPrefix "AddressSanitizer:" rest0 of
            Just afterColon ->
                let kind = T.takeWhile (\c -> not (isSpace c) && c /= ':') (T.dropWhile isSpace afterColon)
                 in if T.null kind then Nothing else Just kind
            Nothing -> Nothing

-- | Extract the trailing summary payload from a @SUMMARY: AddressSanitizer:@ or @UndefinedBehaviorSanitizer:@ line.
parseAsanSummary :: Text -> Maybe Text
parseAsanSummary s = case T.stripPrefix "SUMMARY:" s of
    Nothing -> Nothing
    Just rest ->
        let afterWs = T.dropWhile isSpace rest
         in if "AddressSanitizer:" `T.isPrefixOf` afterWs || "UndefinedBehaviorSanitizer:" `T.isPrefixOf` afterWs
                then Just afterWs
                else Nothing

-- | Parse a @file:line:col: runtime error: ...@ UBSan diagnostic line into its message text.
parseUbsanRuntime :: Text -> Maybe Text
parseUbsanRuntime s = case T.breakOn ": runtime error: " s of
    (locPrefix, rest)
        | not (T.null rest) ->
            let msg = T.strip (T.drop 17 rest)
                parts = T.splitOn ":" locPrefix
             in case reverse parts of
                    (colOrLine : lineOrFile : _)
                        | not (T.null colOrLine)
                            && T.all isDigit colOrLine
                            && not (T.null lineOrFile)
                            && not (T.null msg) ->
                            Just msg
                    _ -> Nothing
    _ -> Nothing

-- | Parse a glibc\/GNU-style @Assertion \`expr' failed.@ diagnostic (optionally anchored to start of text).
parseCAssertion :: Bool -> Text -> Maybe AssertionDetails
parseCAssertion anchored line =
    let (beforeAssert, rest) = T.breakOn "Assertion `" line
     in if T.null rest
            then Nothing
            else do
                let afterTick = T.drop 11 rest
                expr <- splitAssertExpr afterTick
                let (mFile, mLine, mFunc) = parseOptionalPrefix beforeAssert
                if anchored && not (T.null (T.stripEnd beforeAssert)) && isNothingPref mFile
                    then Nothing
                    else pure (AssertionDetails mFile mLine mFunc (Just (T.unwords (T.words expr))))
    where
        isNothingPref Nothing = True
        isNothingPref (Just _) = False

        splitAssertExpr s = case T.breakOn "' failed." s of
            (ex, suf) | suf == "' failed." -> Just ex
            _ -> case T.breakOn "' failed" s of
                (ex, suf) | suf == "' failed" -> Just ex
                _ -> Nothing

        parseOptionalPrefix pref
            | T.null (T.strip pref) = (Nothing, Nothing, Nothing)
            | otherwise = case T.breakOn ": " pref of
                (progTok, afterProg)
                    | not (T.null afterProg) && not (T.any (\c -> isSpace c || c == ':') progTok) ->
                        parseFileLineFunc (T.drop 2 afterProg)
                _ -> parseFileLineFunc pref

        parseFileLineFunc s =
            let trimmed = T.stripEnd s
                noColon = fromMaybe trimmed (T.stripSuffix ":" trimmed)
             in case T.breakOnEnd ": " noColon of
                    (fileLinePrefix, funcTok)
                        | not (T.null fileLinePrefix) ->
                            let flTok = T.stripEnd (T.dropEnd 2 fileLinePrefix)
                             in case T.breakOnEnd ":" flTok of
                                    (fPref, lineDigits)
                                        | not (T.null fPref)
                                            && not (T.null lineDigits)
                                            && T.all isDigit lineDigits ->
                                            ( normalizeSourcePath False (T.dropEnd 1 fPref)
                                            , readMaybe (T.unpack lineDigits)
                                            , Just (T.strip funcTok)
                                            )
                                    _ -> (Nothing, Nothing, Nothing)
                    _ -> (Nothing, Nothing, Nothing)

-- | Parse a BSD\/musl\/macOS @Assertion failed: (expr), function fn, file f, line N.@ diagnostic line.
parseBsdAssertion :: Text -> Maybe AssertionDetails
parseBsdAssertion line = do
    afterHead <- T.stripPrefix "Assertion failed:" (T.strip line)
    let afterWs = T.dropWhile isSpace afterHead
    afterOpen <- T.stripPrefix "(" afterWs
    (expr, afterClose) <- splitOn "), function " afterOpen
    (funcName, afterFunc) <- splitOn ", file " afterClose
    (fName, afterFile) <- splitOn ", line " afterFunc
    let digitsRaw = T.stripEnd afterFile
        digits = fromMaybe digitsRaw (T.stripSuffix "." digitsRaw)
    if not (T.null digits) && T.all isDigit digits
        then
            Just
                AssertionDetails
                    { assertFile = normalizeSourcePath False (T.strip fName)
                    , assertLine = readMaybe (T.unpack digits)
                    , assertFunction = Just (T.strip funcName)
                    , assertExpression = Just (T.unwords (T.words expr))
                    }
        else Nothing
    where
        splitOn delim s = case T.breakOn delim s of
            (before, rest) | not (T.null rest) -> Just (before, T.drop (T.length delim) rest)
            _ -> Nothing

-- | Construct a baseline 'FaultEvent' with optional metadata fields cleared.
makeFault :: DbmsKind -> Text -> Text -> FaultPayload -> FaultEvent
makeFault dbms kind detail payload =
    FaultEvent
        { feDbms = dbms
        , feKind = kind
        , feDetail = detail
        , fePayload = payload
        , fePid = Nothing
        , feChildPid = Nothing
        , feComponent = Nothing
        , feEventCode = Nothing
        , feSubsystem = Nothing
        }

-- | Shared sanitizer/signal corroboration context computed across embedded engine stderr streams.
data EmbeddedSanitizerContext = EmbeddedSanitizerContext
    { escSigInfo :: !(Maybe (Int, Text))
    , escHasCrashSig :: !Bool
    , escHasSanCorr :: !Bool
    , escHasAbrtSig :: !Bool
    }

-- | Summarize termination signal and corroborated ASan diagnostics from embedded engine output lines.
embeddedSanitizerContext :: [Text] -> Int -> EmbeddedSanitizerContext
embeddedSanitizerContext rawLines retcode =
    let mSigInfo = decodeTerminationSignal retcode
        hasCrashSig = maybe False (\(s, _) -> Set.member s crashSignals) mSigInfo
        hasAsanB = any (isJust . parseAsanBanner . T.strip) rawLines
        hasAsanF = any (looksLikeBacktraceLine . T.strip) rawLines
        hasSanCorr = hasAsanB && hasAsanF
        hasAsanAbrt = hasSanCorr && any ("AddressSanitizer: ABRT" `T.isInfixOf`) rawLines
        hasAbrtSig = fmap fst mSigInfo == Just 6 || hasAsanAbrt
     in EmbeddedSanitizerContext mSigInfo hasCrashSig hasSanCorr hasAbrtSig

-- | Fold embedded process termination signals (crash vs external\/OOM kill) into accumulated fault evidence.
applyEmbeddedProcessSignal ::
    DbmsKind ->
    Text ->
    Text ->
    Maybe (Int, Text) ->
    [FaultEvent] ->
    [Text] ->
    Bool ->
    Maybe Text ->
    ([FaultEvent], [Text], Bool, Maybe Text, Bool)
applyEmbeddedProcessSignal dbms sig9CausePrefix sig9Cause mSigInfo faults0 cands0 oom0 oomCause0 = case mSigInfo of
    Just (sig, sName)
        | Set.member sig crashSignals ->
            let detail = sig9CausePrefix <> " terminated by signal " <> T.pack (show sig) <> " (" <> sName <> ")"
                f =
                    makeFault
                        dbms
                        (dbmsKindName dbms <> "_process_signal")
                        detail
                        (PayloadSignal sig (Just sName) Nothing)
             in (faults0 <> [f], cands0 <> [detail], oom0, oomCause0, False)
        | sig == 9 -> (faults0, cands0, True, oomCause0 <|> Just sig9Cause, True)
        | Set.member sig externalOrOomSignals -> (faults0, cands0, oom0, oomCause0, True)
    _ -> (faults0, cands0, oom0, oomCause0, False)

-- | Match compact fixed-width date/timestamp templates ('D'=digit, 'S'=space, 'W'=T/space, '.'=-/.).
matchesDateTemplate :: String -> Text -> Bool
matchesDateTemplate tmpl txt = go tmpl (T.unpack (T.take (length tmpl) txt))
    where
        go [] _ = True
        go _ [] = False
        go (t : ts) (c : cs) = matchChar t c && go ts cs
        matchChar 'D' c = isDigit c
        matchChar 'S' c = isSpace c
        matchChar 'W' c = c == 'T' || c == ' '
        matchChar '.' c = c == '-' || c == '.'
        matchChar x c = x == c

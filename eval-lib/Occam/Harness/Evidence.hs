-- | Pure DBMS server-log and process-stderr crash evidence extractors (anti-spoofing).
module Occam.Harness.Evidence (
    clickHouseConnectionLostPhrases,
    connectionLostPhrasesForDbms,
    extractClickHouseEvidence,
    extractDbmsEvidence,
    extractDuckDbEvidence,
    extractMariadbEvidence,
    extractMysqlEvidence,
    extractPostgresEvidence,
    extractSqliteEvidence,
    hasPsqlMetaCommand,
    looksLikeBacktraceLine,
    mariadbConnectionLostPhrases,
    mysqlConnectionLostPhrases,
    pgConnectionLostPhrases,
) where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isAsciiUpper, isDigit, isHexDigit, isSpace)
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Harness.Signature (
    AssertionDetails (..),
    Corroboration (..),
    DbmsKind (..),
    EvidenceResult (..),
    FaultEvent (..),
    FaultPayload (..),
    InternalFaultKind (..),
    SanitizerTool (..),
    crashSignals,
    dbmsKindName,
    decodeTerminationSignal,
    emptyCorroboration,
    externalOrOomSignals,
    normalizeSourcePath,
    parseStackFrame,
 )
import Text.Read (readMaybe)

pgConnectionLostPhrases :: [Text]
pgConnectionLostPhrases =
    [ "server closed the connection unexpectedly"
    , "connection to server was lost"
    , "could not receive data from server"
    ]

pgCrashShutdownPhrases :: [Text]
pgCrashShutdownPhrases =
    [ "57P02"
    , "terminating connection because of crash of another server process"
    ]

mysqlConnectionLostPhrases :: [Text]
mysqlConnectionLostPhrases =
    [ "Lost connection to MySQL server"
    , "MySQL server has gone away"
    , "ERROR 2006"
    , "ERROR 2013"
    ]

mariadbConnectionLostPhrases :: [Text]
mariadbConnectionLostPhrases =
    mysqlConnectionLostPhrases
        <> [ "Lost connection to server"
           , "Server has gone away"
           ]

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

connectionLostPhrasesForDbms :: DbmsKind -> [Text]
connectionLostPhrasesForDbms = \case
    DbmsPostgres -> pgConnectionLostPhrases
    DbmsMySql -> mysqlConnectionLostPhrases
    DbmsMariaDb -> mariadbConnectionLostPhrases
    DbmsClickHouse -> clickHouseConnectionLostPhrases
    DbmsSqlite -> []
    DbmsDuckDb -> []

hasPsqlMetaCommand :: Text -> Bool
hasPsqlMetaCommand = any (T.isPrefixOf "\\" . T.stripStart) . splitLinesPy

-- | Dispatch evidence extraction to the dialect-specific anti-spoofing extractor.
extractDbmsEvidence :: DbmsKind -> Text -> Text -> Int -> EvidenceResult
extractDbmsEvidence dbms serverLogText stderrText retcode = case dbms of
    DbmsPostgres -> extractPostgresEvidence serverLogText stderrText
    DbmsMySql -> extractMysqlEvidence serverLogText stderrText
    DbmsMariaDb -> extractMariadbEvidence serverLogText stderrText
    DbmsClickHouse -> extractClickHouseEvidence serverLogText stderrText
    DbmsSqlite -> extractSqliteEvidence stderrText retcode
    DbmsDuckDb -> extractDuckDbEvidence stderrText retcode

-- ---------------------------------------------------------------------------
-- Shared C/BSD assertion & sanitizer line parsers
-- ---------------------------------------------------------------------------

splitLinesPy :: Text -> [Text]
splitLinesPy raw
    | T.null raw = []
    | otherwise =
        let normalized = T.replace "\r\n" "\n" raw
            crNorm = T.replace "\r" "\n" normalized
            withoutEnd = fromMaybe crNorm (T.stripSuffix "\n" crNorm)
         in T.splitOn "\n" withoutEnd

skipWs1 :: Text -> Maybe Text
skipWs1 txt = case T.span isSpace txt of
    (ws, rest) | not (T.null ws) -> Just rest
    _ -> Nothing

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

hasAsanInLine :: Text -> Bool
hasAsanInLine ln = case T.breakOn "AddressSanitizer:" ln of
    (_, rest)
        | not (T.null rest) ->
            let tok = T.takeWhile (\c -> not (isSpace c) && c /= ':') (T.dropWhile isSpace (T.drop 17 rest))
             in not (T.null tok)
    _ -> False

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

parseAsanSummary :: Text -> Maybe Text
parseAsanSummary s = case T.stripPrefix "SUMMARY:" s of
    Nothing -> Nothing
    Just rest ->
        let afterWs = T.dropWhile isSpace rest
         in if "AddressSanitizer:" `T.isPrefixOf` afterWs || "UndefinedBehaviorSanitizer:" `T.isPrefixOf` afterWs
                then Just afterWs
                else Nothing

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

-- ---------------------------------------------------------------------------
-- 1. PostgreSQL evidence extractor
-- ---------------------------------------------------------------------------

extractPostgresEvidence :: Text -> Text -> EvidenceResult
extractPostgresEvidence serverLogText stderrText =
    let rawLines = splitLinesPy serverLogText
        init57p02 = any (`T.isInfixOf` stderrText) pgCrashShutdownPhrases
        (revFaults, revCands, backPids, childPids, final57p02) = go rawLines False [] [] Set.empty Set.empty init57p02
        corr =
            emptyCorroboration
                { corConnectionLost = any (`T.isInfixOf` stderrText) pgConnectionLostPhrases
                , corSqlstate57p02 = final57p02
                , corBackendPids = Set.toAscList backPids
                , corPostmasterChildPids = Set.toAscList childPids
                }
     in EvidenceResult (reverse revFaults) (reverse revCands) corr
    where
        go [] _ faults cands bPids cPids s57 = (faults, cands, bPids, cPids, s57)
        go (rawLine : rest) inExcl faults cands bPids cPids s57
            | T.null (T.strip rawLine) = go rest inExcl faults cands bPids cPids s57
            | isPgCsvStart rawLine && "," `T.isInfixOf` rawLine =
                let (csvRowTxt, restAfterCsv) = accumulateCsv rawLine rest
                 in case parsePgCsvRow csvRowTxt of
                        Just (pid, sev, sqlstate, msg) ->
                            let s57' = s57 || sqlstate == "57P02"
                             in if isPgExcludedSeverity sev msg
                                    then go restAfterCsv False faults cands bPids cPids s57'
                                    else
                                        let lineTxt = sev <> ": " <> msg
                                         in processEntry restAfterCsv False (Just pid) sev msg lineTxt faults cands bPids cPids s57'
                        Nothing -> processStderr rawLine rest inExcl faults cands bPids cPids s57
            | otherwise = processStderr rawLine rest inExcl faults cands bPids cPids s57

        accumulateCsv buf [] = (buf, [])
        accumulateCsv buf (nxt : tl)
            | odd (T.count "\"" buf) = accumulateCsv (buf <> "\n" <> nxt) tl
            | otherwise = (buf, nxt : tl)

        processStderr rawLine rest inExcl faults cands bPids cPids s57
            | "\t" `T.isPrefixOf` rawLine =
                if inExcl
                    then go rest True faults cands bPids cPids s57
                    else go rest False faults (T.strip rawLine : cands) bPids cPids s57
            | otherwise =
                let stripped = T.strip rawLine
                 in case parsePgStderrLine stripped of
                        Just (mPid, sev, msg) ->
                            if isPgExcludedSeverity sev msg
                                then go rest True faults cands bPids cPids s57
                                else
                                    let s57' =
                                            s57
                                                || ( sev == "FATAL"
                                                        && ( "57P02" `T.isInfixOf` msg
                                                                || "terminating connection because of crash of another server process" `T.isInfixOf` msg
                                                           )
                                                   )
                                        lineTxt = sev <> ": " <> msg
                                     in processEntry rest False mPid sev msg lineTxt faults cands bPids cPids s57'
                        Nothing -> processEntry rest False Nothing "" stripped stripped faults cands bPids cPids s57

        processEntry rest inExcl mPid sev msg lineTxt faults cands bPids cPids s57 =
            let cands' = lineTxt : cands
             in case parsePgFault mPid sev msg lineTxt of
                    Just (fault, addBackPid, addChildPid) ->
                        let bPids' = maybe bPids (`Set.insert` bPids) addBackPid
                            cPids' = maybe cPids (`Set.insert` cPids) addChildPid
                         in go rest inExcl (fault : faults) cands' bPids' cPids' s57
                    Nothing -> go rest inExcl faults cands' bPids cPids s57

parsePgFault :: Maybe Int -> Text -> Text -> Text -> Maybe (FaultEvent, Maybe Int, Maybe Int)
parsePgFault mPid sev msg lineTxt
    | Just ad <- parsePgTrap lineTxt =
        Just
            ( (makeFault DbmsPostgres "pg_assertion_trap" lineTxt (PayloadAssertion ad)) {fePid = mPid}
            , mPid
            , Nothing
            )
    | "TRAP:" `T.isInfixOf` lineTxt && "FailedAssertion(" `T.isInfixOf` lineTxt =
        let emptyAd = AssertionDetails Nothing Nothing Nothing (Just lineTxt)
         in Just
                ( (makeFault DbmsPostgres "pg_assertion_trap" lineTxt (PayloadAssertion emptyAd)) {fePid = mPid}
                , mPid
                , Nothing
                )
    | sev == "PANIC" || "PANIC:" `T.isPrefixOf` lineTxt =
        let body = T.strip (fromMaybe lineTxt (T.stripPrefix "PANIC:" lineTxt))
         in Just
                ( (makeFault DbmsPostgres "pg_backend_panic" lineTxt (PayloadBackendPanic body)) {fePid = mPid}
                , mPid
                , Nothing
                )
    | Just (cPid, sig) <- parsePgChildSignal msg =
        let f =
                (makeFault DbmsPostgres "pg_postmaster_child_signal" lineTxt (PayloadSignal sig Nothing Nothing))
                    { feChildPid = Just cPid
                    }
         in Just (f, Nothing, Just cPid)
    | Just (cPid, exc) <- parsePgChildException msg =
        let f =
                (makeFault DbmsPostgres "pg_postmaster_child_exception" lineTxt (PayloadChildException cPid exc))
                    { feChildPid = Just cPid
                    }
         in Just (f, Nothing, Just cPid)
    | Just (cPid, code) <- parsePgChildExit msg =
        let f =
                (makeFault DbmsPostgres "pg_postmaster_child_exit" lineTxt (PayloadChildExit cPid code))
                    { feChildPid = Just cPid
                    }
         in Just (f, Nothing, Just cPid)
    | "terminating any other active server processes" `T.isInfixOf` msg =
        Just
            ( makeFault DbmsPostgres "pg_postmaster_crash_recovery" lineTxt PayloadCrashRecovery
            , Nothing
            , Nothing
            )
    | hasAsanInLine lineTxt || isJust (parseUbsanRuntime msg) || isJust (parseUbsanRuntime lineTxt) =
        let tool = if hasAsanInLine lineTxt then Just ToolAsan else Just ToolUbsan
         in Just
                ( (makeFault DbmsPostgres "pg_asan_fault" lineTxt (PayloadSanitizer tool)) {fePid = mPid}
                , Nothing
                , Nothing
                )
    | otherwise = Nothing

isPgExcludedSeverity :: Text -> Text -> Bool
isPgExcludedSeverity sev msg =
    (sev == "LOG" && isNonFaultLogMsg msg)
        || sev `elem` ["STATEMENT", "DETAIL", "HINT", "CONTEXT", "QUERY", "ERROR", "WARNING", "NOTICE", "INFO"]
        || "DEBUG" `T.isPrefixOf` sev
    where
        isNonFaultLogMsg m =
            any (`T.isPrefixOf` m) ["statement:", "duration:"]
                || hasCmdColon "execute " m
                || hasCmdColon "parse " m
        hasCmdColon p m = case T.stripPrefix p m of
            Just rest ->
                let tok = T.takeWhile (not . isSpace) rest
                 in not (T.null tok) && ":" `T.isPrefixOf` T.drop (T.length tok) rest
            Nothing -> False

isPgCsvStart :: Text -> Bool
isPgCsvStart = matchesDateTemplate "DDDD-DD-DDSDD:DD:DD"

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

parsePgCsvRow :: Text -> Maybe (Int, Text, Text, Text)
parsePgCsvRow record = do
    cols <- parseCsvFields record
    case drop 3 cols of
        (pidCol : rest1) -> case drop 7 rest1 of
            (sevCol : sqlCol : msgCol : _) -> do
                let pidTxt = T.strip pidCol
                    sev = T.toUpper (T.strip sevCol)
                    sqlstate = T.toUpper (T.strip sqlCol)
                pid <- if not (T.null pidTxt) && T.all isDigit pidTxt then readMaybe (T.unpack pidTxt) else Nothing
                if sev `elem` pgSeverities
                    then Just (pid, sev, sqlstate, msgCol)
                    else Nothing
            _ -> Nothing
        _ -> Nothing
    where
        pgSeverities =
            [ "LOG"
            , "FATAL"
            , "PANIC"
            , "ERROR"
            , "WARNING"
            , "NOTICE"
            , "INFO"
            , "DEBUG1"
            , "DEBUG2"
            , "DEBUG3"
            , "DEBUG4"
            , "DEBUG5"
            , "TRAP"
            ]

parseCsvFields :: Text -> Maybe [Text]
parseCsvFields txt = go [] "" False (T.unpack txt)
    where
        go fields cur False [] = Just (reverse (T.pack (reverse cur) : fields))
        go fields cur True [] = Just (reverse (T.pack (reverse cur) : fields))
        go fields cur False (',' : cs) = go (T.pack (reverse cur) : fields) "" False cs
        go fields cur False ('"' : cs)
            | null cur = go fields "" True cs
            | otherwise = go fields ('"' : cur) False cs
        go fields cur False (c : cs) = go fields (c : cur) False cs
        go fields cur True ('"' : '"' : cs) = go fields ('"' : cur) True cs
        go fields cur True ('"' : cs) = go fields cur False cs
        go fields cur True (c : cs) = go fields (c : cur) True cs

parsePgStderrLine :: Text -> Maybe (Maybe Int, Text, Text)
parsePgStderrLine line = do
    let afterTs = stripPgTs line
        (mPid, afterPid) = stripPgPid afterTs
        afterCtx = stripPgContext afterPid
    (sev, msg) <- splitPgSeverity afterCtx
    pure (mPid, sev, msg)
    where
        stripPgTs txt
            | isPgCsvStart txt =
                let afterDate = T.drop 19 txt
                    afterMicros = case T.stripPrefix "." afterDate of
                        Just r -> T.dropWhile isDigit r
                        Nothing -> afterDate
                    afterWs1 = T.dropWhile isSpace afterMicros
                    (tzOrBracket, afterTz) = T.break isSpace afterWs1
                 in if "[" `T.isPrefixOf` tzOrBracket || isPgSeverityWord tzOrBracket
                        then afterWs1
                        else T.dropWhile isSpace afterTz
            | otherwise = txt

        stripPgPid txt = case T.stripPrefix "[" txt of
            Just r ->
                let digits = T.takeWhile isDigit r
                    afterD = T.drop (T.length digits) r
                 in case T.stripPrefix "]" afterD of
                        Just afterB
                            | not (T.null digits) ->
                                (readMaybe (T.unpack digits), T.dropWhile isSpace afterB)
                        _ -> (Nothing, txt)
            Nothing -> (Nothing, txt)

        stripPgContext txt =
            let (tok, rest) = T.break isSpace txt
             in if "@" `T.isInfixOf` tok && not (T.null rest) && not (isPgSeverityWord tok)
                    then T.dropWhile isSpace rest
                    else txt

        splitPgSeverity txt = case T.breakOn ":" txt of
            (sevRaw, rest)
                | not (T.null rest) && isPgSeverityWord (sevRaw <> ":") ->
                    Just (sevRaw, T.dropWhile isSpace (T.drop 1 rest))
            _ -> Nothing

        isPgSeverityWord s =
            let w = fromMaybe s (T.stripSuffix ":" s)
             in w
                    `elem` [ "TRAP"
                           , "PANIC"
                           , "FATAL"
                           , "LOG"
                           , "ERROR"
                           , "WARNING"
                           , "NOTICE"
                           , "INFO"
                           , "STATEMENT"
                           , "DETAIL"
                           , "HINT"
                           , "CONTEXT"
                           , "QUERY"
                           ]
                    || (T.isPrefixOf "DEBUG" w && T.all isDigit (T.drop 5 w))

parsePgTrap :: Text -> Maybe AssertionDetails
parsePgTrap txt = case T.breakOn "TRAP:" txt of
    (_, afterTrap)
        | not (T.null afterTrap) -> do
            let afterWs = T.dropWhile isSpace (T.drop 5 afterTrap)
            afterOpen <- T.stripPrefix "FailedAssertion(\"" afterWs
            (expr, afterFileKw) <- case T.breakOn "\", File: \"" afterOpen of
                (e, r) | not (T.null r) -> Just (e, T.drop 10 r)
                _ -> Nothing
            (fName, afterLineKw) <- case T.breakOn "\", Line: " afterFileKw of
                (f, r) | not (T.null r) -> Just (f, T.drop 9 r)
                _ -> Nothing
            let digits = T.takeWhile isDigit afterLineKw
            lNum <- if T.null digits then Nothing else readMaybe (T.unpack digits)
            pure
                AssertionDetails
                    { assertFile = normalizeSourcePath False fName
                    , assertLine = Just lNum
                    , assertFunction = Nothing
                    , assertExpression = Just (T.unwords (T.words expr))
                    }
    _ -> Nothing

parsePgChildSignal :: Text -> Maybe (Int, Int)
parsePgChildSignal msg = do
    (cPid, rest) <- parsePgChildPrefix msg
    afterPhrase <- T.stripPrefix ") was terminated by signal " rest
    let digits = T.takeWhile isDigit afterPhrase
    sig <- if T.null digits then Nothing else readMaybe (T.unpack digits)
    pure (cPid, sig)

parsePgChildException :: Text -> Maybe (Int, Text)
parsePgChildException msg = do
    (cPid, rest) <- parsePgChildPrefix msg
    afterPhrase <- T.stripPrefix ") was terminated by exception 0x" rest
    let hexDigits = T.takeWhile isHexDigit afterPhrase
    if T.null hexDigits then Nothing else pure (cPid, "0x" <> hexDigits)

parsePgChildExit :: Text -> Maybe (Int, Int)
parsePgChildExit msg = do
    (cPid, rest) <- parsePgChildPrefix msg
    afterPhrase <- T.stripPrefix ") exited with exit code " rest
    let digits = T.takeWhile isDigit afterPhrase
    code <- if T.null digits then Nothing else readMaybe (T.unpack digits)
    if code > 0 then Just (cPid, code) else Nothing

parsePgChildPrefix :: Text -> Maybe (Int, Text)
parsePgChildPrefix msg = case T.breakOn "server process (PID " msg of
    (_, rest)
        | not (T.null rest) ->
            let after = T.drop 20 rest
                digits = T.takeWhile isDigit after
             in do
                    pid <- if T.null digits then Nothing else readMaybe (T.unpack digits)
                    pure (pid, T.drop (T.length digits) after)
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- 2. MySQL / MariaDB evidence extractor
-- ---------------------------------------------------------------------------

extractMysqlEvidence :: Text -> Text -> EvidenceResult
extractMysqlEvidence = extractMysqlFamilyEvidence DbmsMySql

extractMariadbEvidence :: Text -> Text -> EvidenceResult
extractMariadbEvidence = extractMysqlFamilyEvidence DbmsMariaDb

extractMysqlFamilyEvidence :: DbmsKind -> Text -> Text -> EvidenceResult
extractMysqlFamilyEvidence dbms serverLogText stderrText =
    let (revFaults, revCands, revSups) = go (splitLinesPy serverLogText) False False [] [] []
        corr =
            emptyCorroboration
                { corConnectionLost = any (`T.isInfixOf` stderrText) (connectionLostPhrasesForDbms dbms)
                , corSupervisorExitCodes = reverse revSups
                }
     in EvidenceResult (reverse revFaults) (reverse revCands) corr
    where
        go [] _ _ faults cands sups = (faults, cands, sups)
        go (rawLine : rest) inEcho inExcl faults cands sups
            | T.null (T.strip rawLine) = go rest False False faults cands sups
            | otherwise =
                let line = T.strip rawLine
                 in case parseMysqlSupervisorExit line of
                        Just status ->
                            let sups' = status : sups
                                (faults', cands')
                                    | status /= 0 =
                                        let mSig = fst <$> decodeTerminationSignal status
                                            f =
                                                makeFault
                                                    dbms
                                                    (dbmsKindName dbms <> "_supervisor_abnormal_exit")
                                                    line
                                                    (PayloadSupervisorExit status mSig)
                                         in (f : faults, line : cands)
                                    | otherwise = (faults, cands)
                             in go rest False False faults' cands' sups'
                        Nothing -> case parseMysqlLogHeader dbms line of
                            Just (level, mCode, mSubsys, msg) ->
                                if level `elem` ["Note", "Warning"]
                                    || "Aborted connection " `T.isInfixOf` msg && hasAbortedDigits msg
                                    || mCode `elem` [Just "MY-010914", Just "MY-010926"]
                                    then go rest False True faults cands sups
                                    else classifyLine rest line msg mCode mSubsys faults cands sups
                            Nothing
                                | isMysqlQueryEcho line -> go rest True False faults cands sups
                                | inEcho ->
                                    if any (`T.isPrefixOf` line) ["Connection ID", "Status:", "The manual page", "Optimizer switch"]
                                        then classifyUnprefixed rest False inExcl line faults cands sups
                                        else go rest True inExcl faults cands sups
                                | inExcl -> go rest False True faults cands sups
                                | otherwise -> classifyUnprefixed rest False False line faults cands sups

        classifyUnprefixed rest _ _ line faults cands sups
            | hasAbortedConn line = go rest False True faults cands sups
            | otherwise = classifyLine rest line line Nothing Nothing faults cands sups

        classifyLine rest line msg mCode mSubsys faults cands sups =
            let cands' = line : cands
                mFault = parseMysqlFault dbms line msg mCode mSubsys
             in go rest False False (maybe faults (: faults) mFault) cands' sups

parseMysqlSupervisorExit :: Text -> Maybe Int
parseMysqlSupervisorExit line =
    parseSuffix "mysqld-debug terminated (exit status: "
        <|> parseSuffix "mariadbd-debug terminated (exit status: "
        <|> parseSuffix "mysqld terminated (exit status: "
        <|> parseSuffix "mariadbd terminated (exit status: "
    where
        parseSuffix marker = case T.breakOn marker line of
            (_, rest)
                | not (T.null rest) ->
                    let after = T.drop (T.length marker) rest
                        digits = T.takeWhile isDigit after
                     in case T.stripPrefix ")" (T.drop (T.length digits) after) of
                            Just _ | not (T.null digits) -> readMaybe (T.unpack digits)
                            _ -> Nothing
            _ -> Nothing

parseMysqlLogHeader :: DbmsKind -> Text -> Maybe (Text, Maybe Text, Maybe Text, Text)
parseMysqlLogHeader dbms line = do
    afterTs <- case dbms of
        DbmsMariaDb -> stripMariadbTs line
        _ -> stripMysql80Ts line
    let (mTid, afterTid) = stripThreadId afterTs
    _ <- mTid <|> Just ()
    (level, afterLevel) <- readBracketToken afterTid
    if T.all isAlpha level && not (T.null level)
        then do
            let (mCode, afterCode) = readMyCode afterLevel
                afterSubLvl = case dbms of
                    DbmsMariaDb -> stripSublevel afterCode
                    _ -> afterCode
                (mSubsys, msg) = readBracketOpt afterSubLvl
            pure (level, mCode, mSubsys, msg)
        else Nothing
    where
        stripMysql80Ts s = case T.break isSpace s of
            (tsTok, rest)
                | isIso8601Z tsTok && not (T.null rest) -> Just (T.dropWhile isSpace rest)
            _ -> Nothing

        stripMariadbTs s =
            let tsLen
                    | isMysqlTs19 s = Just 19
                    | isMariaShortTs s = Just 15
                    | otherwise = Nothing
             in case tsLen of
                    Nothing -> Nothing
                    Just baseLen ->
                        let rest = T.drop baseLen s
                            afterSub = case T.stripPrefix "." rest of
                                Just r -> T.dropWhile isDigit r
                                Nothing -> rest
                            afterZ = fromMaybe afterSub (T.stripPrefix "Z" afterSub)
                         in skipWs1 afterZ

        isIso8601Z tok =
            T.length tok >= 20
                && T.isSuffixOf "Z" tok
                && matchesDateTemplate "DDDD-DD-DDTDD:DD:DD" tok

        isMysqlTs19 = matchesDateTemplate "DDDD-DD-DDWDD:DD:DD"

        isMariaShortTs = matchesDateTemplate "DDDDDD DD:DD:DD"

        stripThreadId s = case T.uncons s of
            Just ('[', _) -> (Nothing, s)
            _ -> case T.break isSpace s of
                (tok, rest)
                    | not (T.null rest) && (T.all isDigit tok || isHexPrefix tok) ->
                        (Just (), T.dropWhile isSpace rest)
                _ -> (Nothing, s)

        isHexPrefix t = case T.stripPrefix "0x" t of
            Just h -> not (T.null h) && T.all isHexDigit h
            Nothing -> False

        readBracketToken s = do
            afterOpen <- T.stripPrefix "[" s
            let (tok, afterClose) = T.break (== ']') afterOpen
            rest <- T.stripPrefix "]" afterClose
            pure (tok, T.dropWhile isSpace rest)

        readMyCode s = case readBracketToken s of
            Just (tok, rest)
                | "MY-" `T.isPrefixOf` tok && T.length tok == 9 && T.all isDigit (T.drop 3 tok) ->
                    (Just tok, rest)
            _ -> (Nothing, s)

        stripSublevel s = case readBracketToken s of
            Just (tok, rest) | tok `elem` ["FATAL", "ERROR", "Note", "Warning"] -> rest
            _ -> s

        readBracketOpt s = case readBracketToken s of
            Just (tok, rest) | not (T.null tok) -> (Just tok, rest)
            _ -> (Nothing, s)

isMysqlQueryEcho :: Text -> Bool
isMysqlQueryEcho line = case T.stripPrefix "Query" line of
    Nothing -> False
    Just rest ->
        let afterWs = T.dropWhile isSpace rest
         in case T.stripPrefix "(" afterWs of
                Just inP ->
                    let hx = T.takeWhile isHexOrX inP
                        afterHx = T.drop (T.length hx) inP
                     in not (T.null hx) && "):" `T.isPrefixOf` afterHx
                Nothing -> False
    where
        isHexOrX c = isHexDigit c || c == 'x' || c == 'X'

hasAbortedConn :: Text -> Bool
hasAbortedConn msg = hasAbortedDigits msg && "Aborted connection " `T.isInfixOf` msg

hasAbortedDigits :: Text -> Bool
hasAbortedDigits msg = case T.breakOn "Aborted connection " msg of
    (_, rest)
        | not (T.null rest) ->
            let digits = T.takeWhile isDigit (T.drop 19 rest)
             in not (T.null digits)
    _ -> False

parseMysqlFault :: DbmsKind -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe FaultEvent
parseMysqlFault dbms line msg mCode mSubsys
    | Just sig <- parseMysqlSignal line =
        Just (baseEvent (dbmsKindName dbms <> "_server_signal") (PayloadSignal sig Nothing Nothing))
    | Just ad <- parseMysqlAssertion msg =
        Just (baseEvent (dbmsKindName dbms <> "_assertion_failure") (PayloadAssertion ad))
    | hasAsanInLine line || isJust (parseUbsanRuntime msg) || isJust (parseUbsanRuntime line) =
        let tool = if hasAsanInLine line then Just ToolAsan else Just ToolUbsan
         in Just (baseEvent (dbmsKindName dbms <> "_asan_fault") (PayloadSanitizer tool))
    | otherwise = Nothing
    where
        baseEvent kind payload =
            (makeFault dbms kind line payload)
                { feEventCode = mCode
                , feSubsystem = mSubsys
                }

parseMysqlSignal :: Text -> Maybe Int
parseMysqlSignal line =
    afterMarker "mysqld got signal "
        <|> afterMarker "mariadbd got signal "
        <|> afterMarker "Fatal signal "
        <|> afterMarker "Got signal "
    where
        afterMarker m = case T.breakOn m line of
            (_, rest)
                | not (T.null rest) ->
                    let d = T.takeWhile isDigit (T.drop (T.length m) rest)
                     in if T.null d then Nothing else readMaybe (T.unpack d)
            _ -> Nothing

parseMysqlAssertion :: Text -> Maybe AssertionDetails
parseMysqlAssertion msg =
    parseAssertFailureColon msg
        <|> parseInnoFileLine msg
        <|> parseFailingAssert msg
        <|> parseCAssertion False msg
        <|> parseBsdAssertion msg
    where
        parseAssertFailureColon s = case T.breakOn "Assertion failure:" s of
            (_, rest)
                | not (T.null rest) ->
                    let after = T.dropWhile isSpace (T.drop 18 rest)
                        (fPart, rest1) = T.break (== ':') after
                        (lDigits, rest2) = T.break (== ':') (T.drop 1 rest1)
                     in if not (T.null fPart)
                            && not (T.any isSpace fPart)
                            && not (T.null lDigits)
                            && T.all isDigit lDigits
                            && ":" `T.isPrefixOf` rest2
                            then
                                Just
                                    AssertionDetails
                                        { assertFile = normalizeSourcePath False fPart
                                        , assertLine = readMaybe (T.unpack lDigits)
                                        , assertFunction = Nothing
                                        , assertExpression = Just (T.unwords (T.words (T.drop 1 rest2)))
                                        }
                            else Nothing
            _ -> Nothing

        parseInnoFileLine s = case T.breakOn "Assertion failure " s of
            (_, rest) | not (T.null rest) -> do
                let after = T.dropWhile isSpace (T.drop 18 rest)
                afterIn <- T.stripPrefix "in " after
                let afterThread = case T.stripPrefix "thread " afterIn of
                        Just rt ->
                            let afterTid = T.dropWhile (not . isSpace) rt
                                afterWs = T.dropWhile isSpace afterTid
                             in fromMaybe afterIn (T.stripPrefix "in " afterWs)
                        Nothing -> afterIn
                afterFileKw <- T.stripPrefix "file " afterThread
                let fName = T.takeWhile (not . isSpace) afterFileKw
                afterWs <- skipWs1 (T.drop (T.length fName) afterFileKw)
                afterLineKw <- T.stripPrefix "line " afterWs
                let digits = T.takeWhile isDigit afterLineKw
                lNum <- if T.null digits then Nothing else readMaybe (T.unpack digits)
                pure
                    AssertionDetails
                        { assertFile = normalizeSourcePath False fName
                        , assertLine = Just lNum
                        , assertFunction = Nothing
                        , assertExpression = Nothing
                        }
            _ -> Nothing

        parseFailingAssert s = case T.breakOn "Failing assertion:" s of
            (_, rest)
                | not (T.null rest) ->
                    let expr = T.unwords (T.words (T.drop 18 rest))
                     in if T.null expr
                            then Nothing
                            else Just (AssertionDetails Nothing Nothing Nothing (Just expr))
            _ -> Nothing

-- ---------------------------------------------------------------------------
-- 3. ClickHouse evidence extractor
-- ---------------------------------------------------------------------------

extractClickHouseEvidence :: Text -> Text -> EvidenceResult
extractClickHouseEvidence serverLogText stderrText =
    let (revFaults, revCands, revSups) = go (splitLinesPy serverLogText) False False [] [] []
        corr =
            emptyCorroboration
                { corConnectionLost = any (`T.isInfixOf` stderrText) clickHouseConnectionLostPhrases
                , corSupervisorExitCodes = reverse revSups
                }
     in EvidenceResult (reverse revFaults) (reverse revCands) corr
    where
        go [] _ _ faults cands sups = (faults, cands, sups)
        go (rawLine : rest) inExcl inFatal faults cands sups
            | T.null (T.strip rawLine) = go rest inExcl inFatal faults cands sups
            | otherwise =
                let line = T.strip rawLine
                 in case parseClickHouseSupervisorExit line of
                        Just status ->
                            let sups' = status : sups
                                (faults', cands')
                                    | status /= 0 =
                                        let mSig = fst <$> decodeTerminationSignal status
                                            f =
                                                makeFault
                                                    DbmsClickHouse
                                                    "clickhouse_supervisor_abnormal_exit"
                                                    line
                                                    (PayloadSupervisorExit status mSig)
                                         in (f : faults, line : cands)
                                    | otherwise = (faults, cands)
                             in go rest False False faults' cands' sups'
                        Nothing -> case parseClickHouseLogLine line of
                            Just (lvl, comp, msg0) ->
                                if lvl /= "Fatal"
                                    then go rest True False faults cands sups
                                    else
                                        let (fullMsg, rest') = accumulateQueryEcho msg0 rest
                                         in if T.null fullMsg || T.all (== '#') fullMsg
                                                then go rest' False True faults cands sups
                                                else
                                                    let cleanMsg = stripClickHouseQueryEcho fullMsg
                                                     in if T.null cleanMsg
                                                            then go rest' False True faults cands sups
                                                            else
                                                                let cleanLine =
                                                                        if T.null comp
                                                                            then "<Fatal> : " <> cleanMsg
                                                                            else "<Fatal> " <> comp <> ": " <> cleanMsg
                                                                    cands' = cleanLine : cands
                                                                    mFault = classifyClickHouseMsg comp cleanLine cleanMsg
                                                                 in go rest' False True (maybe faults (: faults) mFault) cands' sups
                            Nothing
                                | inExcl -> go rest True False faults cands sups
                                | isJust (parseAsanBanner line)
                                    || isJust (parseAsanSummary line)
                                    || isJust (parseUbsanRuntime line)
                                    || isJust (parseCAssertion True line)
                                    || isJust (parseBsdAssertion line)
                                    || looksLikeBacktraceLine line
                                    || (inFatal && isJust (parseStackFrame line)) ->
                                    let cands' = line : cands
                                        mFault = classifyClickHouseMsg "" line line
                                     in go rest False inFatal (maybe faults (: faults) mFault) cands' sups
                                | otherwise -> go rest False inFatal faults cands sups

        accumulateQueryEcho msg0 linesAfter
            | "(query:" `T.isInfixOf` msg0
                && ( (not (hasQuerySignalSuffix msg0) && not (hasQueryEndSuffix msg0))
                        || hasBasedaemonQueryContinuation linesAfter
                   ) =
                accLoop msg0 linesAfter
            | otherwise = (msg0, linesAfter)

        accLoop cur [] = (cur, [])
        accLoop cur (rawNxt : tl) =
            let nxt = T.strip rawNxt
             in if isJust (parseClickHouseLogLine nxt) || isJust (parseClickHouseSupervisorExit nxt)
                    then (cur, rawNxt : tl)
                    else
                        let cur' = cur <> "\n" <> nxt
                         in if (hasQuerySignalSuffix cur' || hasQueryEndSuffix cur')
                                && not (hasBasedaemonQueryContinuation tl)
                                then (cur', tl)
                                else accLoop cur' tl

        classifyClickHouseMsg comp cleanLine cleanMsg
            | Just sig <- parseClickHouseSignal cleanMsg =
                Just
                    ( (makeFault DbmsClickHouse "clickhouse_server_signal" cleanLine (PayloadSignal sig Nothing Nothing))
                        { feComponent = compOpt
                        }
                    )
            | Just logicMsg <- parseClickHouseLogicalError cleanMsg =
                Just
                    ( ( makeFault
                            DbmsClickHouse
                            "clickhouse_logical_error"
                            cleanLine
                            (PayloadInternal KindClickHouseLogical logicMsg)
                      )
                        { feComponent = compOpt
                        }
                    )
            | Just ad <-
                parseChAssertion cleanMsg <|> parseCAssertion True cleanMsg <|> parseBsdAssertion cleanMsg =
                Just
                    ( (makeFault DbmsClickHouse "clickhouse_assertion_failure" cleanLine (PayloadAssertion ad))
                        { feComponent = compOpt
                        }
                    )
            | hasAsanInLine cleanMsg || isJust (parseUbsanRuntime cleanMsg) =
                let tool = if hasAsanInLine cleanMsg then Just ToolAsan else Just ToolUbsan
                 in Just
                        ( (makeFault DbmsClickHouse "clickhouse_asan_fault" cleanLine (PayloadSanitizer tool))
                            { feComponent = compOpt
                            }
                        )
            | otherwise = Nothing
            where
                compOpt = if T.null comp then Nothing else Just comp

parseClickHouseSupervisorExit :: Text -> Maybe Int
parseClickHouseSupervisorExit line = case T.breakOn "clickhouse-server terminated (exit status: " line of
    (_, rest)
        | not (T.null rest) ->
            let after = T.drop 42 rest
                digits = T.takeWhile isDigit after
             in case T.stripPrefix ")" (T.drop (T.length digits) after) of
                    Just _ | not (T.null digits) -> readMaybe (T.unpack digits)
                    _ -> Nothing
    _ -> Nothing

parseClickHouseLogLine :: Text -> Maybe (Text, Text, Text)
parseClickHouseLogLine line = do
    let (tsTok, afterTs) = splitChTs line
    if T.null tsTok
        then Nothing
        else do
            afterOpenB <- T.stripPrefix "[" afterTs
            let (tid, afterTid) = T.break (== ']') afterOpenB
            afterCloseB <- T.stripPrefix "]" afterTid >>= skipWs1
            if T.null (T.strip tid) || not (T.all isDigit (T.strip tid))
                then Nothing
                else do
                    afterOpenBrace <- T.stripPrefix "{" afterCloseB
                    let (_qid, afterQid) = T.break (== '}') afterOpenBrace
                    afterCloseBrace <- T.stripPrefix "}" afterQid >>= skipWs1
                    afterLt <- T.stripPrefix "<" afterCloseBrace
                    let (lvl, afterLvl) = T.break (== '>') afterLt
                    afterGt <- T.stripPrefix ">" afterLvl
                    if T.null lvl || not (T.all isAlpha lvl)
                        then Nothing
                        else
                            let afterWs = T.dropWhile isSpace afterGt
                             in case T.breakOn ":" afterWs of
                                    (comp, afterColon)
                                        | not (T.null afterColon) ->
                                            Just (lvl, T.strip comp, T.strip (T.drop 1 afterColon))
                                    _ -> Nothing
    where
        splitChTs s
            | matchesDateTemplate "DDDD.DD.DDWDD:DD:DD" s =
                let after19 = T.drop 19 s
                    afterDot = case T.stripPrefix "." after19 of
                        Just r -> T.dropWhile isDigit r
                        Nothing -> after19
                    afterZ = fromMaybe afterDot (T.stripPrefix "Z" afterDot)
                 in ("ts", T.dropWhile isSpace afterZ)
            | otherwise = ("", s)

hasBasedaemonQueryContinuation :: [Text] -> Bool
hasBasedaemonQueryContinuation [] = False
hasBasedaemonQueryContinuation (raw : tl) =
    let nxt = T.strip raw
     in if T.null nxt
            then hasBasedaemonQueryContinuation tl
            else
                not (isJust (parseClickHouseLogLine nxt) || isJust (parseClickHouseSupervisorExit nxt))
                    && (isClosingQuerySignal nxt || hasBasedaemonQueryContinuation tl)
    where
        isClosingQuerySignal s = case T.stripPrefix ")" s of
            Just rest ->
                let trimmed = T.strip rest
                 in T.null trimmed || isJust (parseReceivedSignalPhrase trimmed)
            Nothing -> case T.breakOn ") Received signal " s of
                (_, r) | not (T.null r) -> isJust (parseReceivedSignalPhrase (T.drop 2 r))
                _ -> False

hasQuerySignalSuffix :: Text -> Bool
hasQuerySignalSuffix msg = isJust (splitQuerySignal msg)

hasQueryEndSuffix :: Text -> Bool
hasQueryEndSuffix msg =
    "(query:" `T.isInfixOf` msg && T.isSuffixOf ")" (T.stripEnd msg)

stripClickHouseQueryEcho :: Text -> Text
stripClickHouseQueryEcho msg = case splitQuerySignal msg of
    Just (beforeQuery, sigPhrase) ->
        T.strip (beforeQuery <> " " <> sigPhrase)
    Nothing -> case T.breakOn "(query:" msg of
        (beforeQuery, rest)
            | not (T.null rest) && T.isSuffixOf ")" (T.stripEnd rest) ->
                T.strip beforeQuery
        _ -> msg

splitQuerySignal :: Text -> Maybe (Text, Text)
splitQuerySignal msg = case T.breakOn "(query:" msg of
    (beforeQuery, afterQuery)
        | not (T.null afterQuery) -> findLastClosingSig (T.stripEnd afterQuery)
        where
            findLastClosingSig txt = case T.breakOnEnd ") Received signal " txt of
                (pref, suf)
                    | not (T.null pref) ->
                        let sigTxt = "Received signal " <> suf
                         in if isValidClosingSig suf
                                then Just (beforeQuery, sigTxt)
                                else Nothing
                _ -> Nothing
            isValidClosingSig s =
                let (headTok, parenRest) = T.break (== '(') s
                    headValid = not (T.null headTok) && T.all (\c -> isAlphaNum c || c `elem` (" _-" :: String)) headTok
                 in case T.stripPrefix "(" parenRest of
                        Just inP ->
                            let digits = T.takeWhile isDigit inP
                             in headValid && not (T.null digits) && T.drop (T.length digits) inP == ")"
                        Nothing -> headValid
    _ -> Nothing

parseReceivedSignalPhrase :: Text -> Maybe Int
parseReceivedSignalPhrase txt = do
    after <- T.stripPrefix "Received signal " txt
    case T.break (== '(') after of
        (_desc, afterParen)
            | not (T.null afterParen) ->
                let digits = T.takeWhile isDigit (T.drop 1 afterParen)
                 in if not (T.null digits) && ")" `T.isPrefixOf` T.drop (T.length digits) (T.drop 1 afterParen)
                        then readMaybe (T.unpack digits)
                        else Nothing
        _ ->
            let digits = T.takeWhile isDigit after
             in if T.null digits then Nothing else readMaybe (T.unpack digits)

parseClickHouseSignal :: Text -> Maybe Int
parseClickHouseSignal msg = case T.breakOn "Received signal " msg of
    (_, rest) | not (T.null rest) -> parseReceivedSignalPhrase rest
    _ -> Nothing

parseClickHouseLogicalError :: Text -> Maybe Text
parseClickHouseLogicalError msg = case T.breakOn "Logical error:" msg of
    (_, rest)
        | not (T.null rest) ->
            let body = T.strip (T.drop 14 rest)
             in if T.null body then Nothing else Just body
    _ -> Nothing

parseChAssertion :: Text -> Maybe AssertionDetails
parseChAssertion msg
    | "chassert" `T.isInfixOf` msg && "failed" `T.isInfixOf` msg =
        Just (AssertionDetails Nothing Nothing Nothing (Just (T.unwords (T.words msg))))
    | Just rest <- findSuffix "Assertion failure:" msg <|> findSuffix "Assertion failure " msg =
        Just (AssertionDetails Nothing Nothing Nothing (Just (T.unwords (T.words rest))))
    | otherwise = Nothing
    where
        findSuffix p s = case T.breakOn p s of
            (_, r) | not (T.null r) -> Just (T.drop (T.length p) r)
            _ -> Nothing

-- ---------------------------------------------------------------------------
-- 4. SQLite evidence extractor
-- ---------------------------------------------------------------------------

extractSqliteEvidence :: Text -> Int -> EvidenceResult
extractSqliteEvidence stderrText retcode =
    let mSigInfo = decodeTerminationSignal retcode
        hasCrashSig = maybe False (\(s, _) -> Set.member s crashSignals) mSigInfo
        rawLines = stripSqliteErrorBlocks (splitLinesPy stderrText)
        hasAsanB = any (isJust . parseAsanBanner . T.strip) rawLines
        hasAsanF = any (looksLikeBacktraceLine . T.strip) rawLines
        hasSanCorr = hasAsanB && hasAsanF
        hasAsanAbrt = hasSanCorr && any ("AddressSanitizer: ABRT" `T.isInfixOf`) rawLines
        hasAbrtSig = fmap fst mSigInfo == Just 6 || hasAsanAbrt

        scan [] _ faults cands = (reverse faults, reverse cands)
        scan (rawLine : rest) inErr faults cands =
            let stripped = T.strip rawLine
             in if T.null stripped
                    then scan rest inErr faults cands
                    else
                        if isSqliteErrorStart stripped
                            then scan rest True faults cands
                            else
                                if isSqliteCaretEnd rawLine
                                    then scan rest False faults cands
                                    else
                                        if looksLikeBacktraceLine stripped && (hasCrashSig || hasSanCorr || not inErr)
                                            then scan rest False faults (stripped : cands)
                                            else
                                                if "  " `T.isPrefixOf` rawLine && inErr
                                                    then scan rest True faults cands
                                                    else case parseCAssertion True stripped <|> parseBsdAssertion stripped of
                                                        Just ad ->
                                                            if hasAbrtSig
                                                                then
                                                                    let f = makeFault DbmsSqlite "sqlite_assertion_failure" stripped (PayloadAssertion ad)
                                                                     in scan rest False (f : faults) (stripped : cands)
                                                                else scan rest inErr faults cands
                                                        Nothing
                                                            | isJust (parseAsanBanner stripped) ->
                                                                if hasCrashSig || hasSanCorr
                                                                    then
                                                                        let f = makeFault DbmsSqlite "sqlite_asan_fault" stripped (PayloadSanitizer (Just ToolAsan))
                                                                         in scan rest False (f : faults) (stripped : cands)
                                                                    else scan rest inErr faults cands
                                                            | isJust (parseAsanSummary stripped) ->
                                                                if hasCrashSig || hasSanCorr
                                                                    then scan rest False faults (stripped : cands)
                                                                    else scan rest inErr faults cands
                                                            | isJust (parseUbsanRuntime stripped) ->
                                                                if hasCrashSig || not inErr
                                                                    then
                                                                        let f = makeFault DbmsSqlite "sqlite_asan_fault" stripped (PayloadSanitizer (Just ToolUbsan))
                                                                         in scan rest False (f : faults) (stripped : cands)
                                                                    else scan rest inErr faults cands
                                                            | otherwise -> scan rest inErr faults cands

        (faults0, cands0) = scan rawLines False [] []
        sqliteAbort = retcode == 4 && null faults0 && not hasCrashSig
        (faults1, cands1, oom, oomCause, extKill) = case mSigInfo of
            Just (sig, sName)
                | Set.member sig crashSignals ->
                    let detail = "sqlite3 terminated by signal " <> T.pack (show sig) <> " (" <> sName <> ")"
                        f = makeFault DbmsSqlite "sqlite_process_signal" detail (PayloadSignal sig (Just sName) Nothing)
                     in (faults0 <> [f], cands0 <> [detail], False, Nothing, False)
                | sig == 9 -> (faults0, cands0, True, Just "signal_9_kill", True)
                | Set.member sig externalOrOomSignals -> (faults0, cands0, False, Nothing, True)
            _ -> (faults0, cands0, False, Nothing, False)
        corr =
            emptyCorroboration
                { corRetcode = Just retcode
                , corSignal = fst <$> mSigInfo
                , corSqliteAbort = sqliteAbort
                , corOom = oom
                , corOomCause = oomCause
                , corExternalKill = extKill
                }
     in EvidenceResult faults1 cands1 corr

stripSqliteErrorBlocks :: [Text] -> [Text]
stripSqliteErrorBlocks [] = []
stripSqliteErrorBlocks (rawLine : rest)
    | not (isSqliteErrorStart (T.strip rawLine)) = rawLine : stripSqliteErrorBlocks rest
    | Just afterCaret <- findCaretBlock (hasOpenNearQuote rawLine) rest =
        stripSqliteErrorBlocks afterCaret
    | hasOpenNearQuote rawLine
    , Just afterUnclosed <- findClosedQuoteBlock rest =
        stripSqliteErrorBlocks afterUnclosed
    | otherwise =
        rawLine : stripSqliteErrorBlocks (drop2SpaceEchoes rest)
    where
        hasOpenNearQuote ln = ": near \"" `T.isInfixOf` ln && odd (T.count "\"" ln)

        findCaretBlock _ [] = Nothing
        findCaretBlock inQ (nxt : tl)
            | isSqliteCaretEnd nxt = Just tl
            | not inQ
                && not ("  " `T.isPrefixOf` nxt)
                && ( isSqliteErrorStart stripped
                        || isJust (parseAsanBanner stripped)
                        || isJust (parseAsanSummary stripped)
                        || looksLikeBacktraceLine stripped
                   ) =
                Nothing
            | otherwise =
                let inQ' =
                        if (inQ || "  " `T.isPrefixOf` nxt) && odd (T.count "\"" nxt)
                            then not inQ
                            else inQ
                 in findCaretBlock inQ' tl
            where
                stripped = T.strip nxt

        findClosedQuoteBlock [] = Nothing
        findClosedQuoteBlock (nxt : tl)
            | odd (T.count "\"" nxt) = Just (drop2SpaceEchoes tl)
            | otherwise = findClosedQuoteBlock tl

        drop2SpaceEchoes (nxt : tl)
            | "  " `T.isPrefixOf` nxt && not (looksLikeBacktraceLine (T.strip nxt)) =
                drop2SpaceEchoes tl
        drop2SpaceEchoes xs = xs

isSqliteErrorStart :: Text -> Bool
isSqliteErrorStart stripped =
    let rest0 = fromMaybe stripped (T.stripPrefix "Parse " stripped <|> T.stripPrefix "Runtime " stripped)
        afterErr = T.stripPrefix "Error" rest0 <|> T.stripPrefix "error" rest0
     in case afterErr of
            Nothing -> False
            Just r ->
                let afterLine = case T.stripPrefix " near line " r of
                        Just rLine ->
                            let digits = T.takeWhile isDigit rLine
                             in if T.null digits then r else T.drop (T.length digits) rLine
                        Nothing -> r
                 in case T.stripPrefix ":" afterLine of
                        Just _ -> True
                        Nothing -> False

isSqliteCaretEnd :: Text -> Bool
isSqliteCaretEnd rawLine =
    let s = T.strip rawLine
     in fromMaybe False (checkPrefix "^---" "error here" s <|> checkSuffix "---^" "error here" s)
    where
        checkPrefix pre tailTxt s = do
            afterPre <- T.stripPrefix pre s
            pure (T.strip afterPre == tailTxt)
        checkSuffix suf headTxt s = do
            beforeSuf <- T.stripSuffix suf s
            pure (T.strip beforeSuf == headTxt)

-- ---------------------------------------------------------------------------
-- 5. DuckDB evidence extractor
-- ---------------------------------------------------------------------------

extractDuckDbEvidence :: Text -> Int -> EvidenceResult
extractDuckDbEvidence stderrText retcode =
    let mSigInfo = decodeTerminationSignal retcode
        hasCrashSig = maybe False (\(s, _) -> Set.member s crashSignals) mSigInfo
        rawLines = splitLinesPy stderrText
        hasAsanB = any (isJust . parseAsanBanner . T.strip) rawLines
        hasAsanF = any (looksLikeBacktraceLine . T.strip) rawLines
        hasSanCorr = hasAsanB && hasAsanF
        hasAsanAbrt = hasSanCorr && any ("AddressSanitizer: ABRT" `T.isInfixOf`) rawLines
        hasAbrtSig = fmap fst mSigInfo == Just 6 || hasAsanAbrt

        go [] faults cands oom oomCause dbInv = (reverse faults, reverse cands, oom, oomCause, dbInv)
        go (rawLine : rest) faults cands oom oomCause dbInv
            | isDuckDbLineEcho (T.strip rawLine) =
                go (consumeDuckDbLineEcho rawLine rest) faults cands oom oomCause dbInv
            | T.null (T.strip rawLine) =
                go rest faults cands oom oomCause dbInv
            | "Out of Memory Error:" `T.isPrefixOf` T.strip rawLine =
                go rest faults cands True (Just "duckdb_oom_error") dbInv
            | isDuckDbNonfatalError (T.strip rawLine) =
                go (consumeDuckDbNonfatalError (T.strip rawLine) rest) faults cands oom oomCause dbInv
            | looksLikeBacktraceLine (T.strip rawLine) =
                go rest faults (T.strip rawLine : cands) oom oomCause dbInv
            | Just inlineMsg <- stripInternalError (T.strip rawLine) =
                let (parts, rest') = collectInternalBody (maybe [] pure (nonEmpty inlineMsg)) rest
                    body = T.strip (T.unwords parts)
                    detail = if T.null body then "INTERNAL Error" else "INTERNAL Error: " <> body
                    f = makeFault DbmsDuckDb "duckdb_internal_error" detail (PayloadInternal KindDuckDbInternal body)
                 in go rest' (f : faults) (detail : cands) oom oomCause dbInv
            | Just inlineMsg <- stripFatalInvalidated (T.strip rawLine) =
                let (parts, rest') = collectFatalBody (maybe [] pure (nonEmpty inlineMsg)) rest
                    body = T.strip (T.unwords parts)
                    lowBody = T.toLower body
                    triggered = "invalidated" `T.isInfixOf` lowBody || "fatal error" `T.isInfixOf` lowBody || not (T.null body)
                 in if triggered
                        then
                            let detail = if T.null body then "FATAL Error" else "FATAL Error: " <> body
                                f =
                                    makeFault
                                        DbmsDuckDb
                                        "duckdb_database_invalidated"
                                        detail
                                        (PayloadInternal KindDuckDbInvalidated body)
                             in go rest' (f : faults) (detail : cands) oom oomCause True
                        else go rest' faults cands oom oomCause dbInv
            | Just ad <- parseCAssertion True (T.strip rawLine) <|> parseBsdAssertion (T.strip rawLine) =
                if hasAbrtSig
                    then
                        let stripped = T.strip rawLine
                            f = makeFault DbmsDuckDb "duckdb_assertion_failure" stripped (PayloadAssertion ad)
                         in go rest (f : faults) (stripped : cands) oom oomCause dbInv
                    else go rest faults cands oom oomCause dbInv
            | isJust (parseAsanBanner (T.strip rawLine)) =
                if hasCrashSig || hasSanCorr
                    then
                        let stripped = T.strip rawLine
                            f = makeFault DbmsDuckDb "duckdb_asan_fault" stripped (PayloadSanitizer (Just ToolAsan))
                         in go rest (f : faults) (stripped : cands) oom oomCause dbInv
                    else go rest faults cands oom oomCause dbInv
            | isJust (parseAsanSummary (T.strip rawLine)) =
                if hasCrashSig || hasSanCorr
                    then go rest faults (T.strip rawLine : cands) oom oomCause dbInv
                    else go rest faults cands oom oomCause dbInv
            | isJust (parseUbsanRuntime (T.strip rawLine)) =
                let stripped = T.strip rawLine
                    f = makeFault DbmsDuckDb "duckdb_asan_fault" stripped (PayloadSanitizer (Just ToolUbsan))
                 in go rest (f : faults) (stripped : cands) oom oomCause dbInv
            | otherwise = go rest faults cands oom oomCause dbInv

        (faults0, cands0, oom0, oomCause0, dbInv0) = go rawLines [] [] False Nothing False
        (faults1, cands1, oom1, oomCause1, extKill) = case mSigInfo of
            Just (sig, sName)
                | Set.member sig crashSignals ->
                    let detail = "duckdb terminated by signal " <> T.pack (show sig) <> " (" <> sName <> ")"
                        f = makeFault DbmsDuckDb "duckdb_process_signal" detail (PayloadSignal sig (Just sName) Nothing)
                     in (faults0 <> [f], cands0 <> [detail], oom0, oomCause0, False)
                | sig == 9 -> (faults0, cands0, True, oomCause0 <|> Just "oom_kill_signal_9", True)
                | Set.member sig externalOrOomSignals -> (faults0, cands0, oom0, oomCause0, True)
            _ -> (faults0, cands0, oom0, oomCause0, False)
        corr =
            emptyCorroboration
                { corRetcode = Just retcode
                , corSignal = fst <$> mSigInfo
                , corDatabaseInvalidated = dbInv0
                , corOom = oom1
                , corOomCause = oomCause1
                , corExternalKill = extKill
                }
     in EvidenceResult faults1 cands1 corr
    where
        nonEmpty t = if T.null t then Nothing else Just t

        stripInternalError s = T.strip <$> T.stripPrefix "INTERNAL Error:" s
        stripFatalInvalidated s = T.strip <$> T.stripPrefix "FATAL Error:" s

        collectInternalBody acc [] = (reverse acc, [])
        collectInternalBody acc (rawNxt : tl) =
            let nxt = T.strip rawNxt
             in if T.null nxt
                    || isDuckDbLineEcho nxt
                    || isJust (stripFatalInvalidated nxt)
                    || isDuckDbNonfatalError nxt
                    || "Out of Memory Error:" `T.isPrefixOf` nxt
                    || nxt == "Stack Trace:"
                    || looksLikeBacktraceLine nxt
                    then (reverse acc, rawNxt : tl)
                    else
                        let acc' =
                                if any (`T.isPrefixOf` nxt) ["This error signals an assertion failure", "For more information, see"]
                                    then acc
                                    else nxt : acc
                         in collectInternalBody acc' tl

        collectFatalBody acc [] = (reverse acc, [])
        collectFatalBody acc (rawNxt : tl) =
            let nxt = T.strip rawNxt
             in if T.null nxt
                    || isDuckDbLineEcho nxt
                    || isJust (stripInternalError nxt)
                    || isDuckDbNonfatalError nxt
                    || "Out of Memory Error:" `T.isPrefixOf` nxt
                    then (reverse acc, rawNxt : tl)
                    else collectFatalBody (nxt : acc) tl

scanDuckDbErrorDelimiters :: Text -> (Bool, Bool, Int) -> (Bool, Bool, Int)
scanDuckDbErrorDelimiters line = go Nothing (T.unpack line)
    where
        go _ [] acc = acc
        go prevChar (ch : cs) (inDq, inSq, parens)
            | inDq = go (Just ch) cs (ch /= '"', inSq, parens)
            | inSq =
                let inSq' = ch /= '\'' || isContraction prevChar cs
                 in go (Just ch) cs (False, inSq', parens)
            | ch == '"' = go (Just ch) cs (True, False, parens)
            | ch == '\'' = go (Just ch) cs (False, not (isContraction prevChar cs), parens)
            | ch == '(' = go (Just ch) cs (False, False, parens + 1)
            | ch == ')' = go (Just ch) cs (False, False, max 0 (parens - 1))
            | otherwise = go (Just ch) cs (False, False, parens)

        isContraction prevChar cs = case (prevChar, cs) of
            (Just p, nxt : _) -> isAlpha p && isAlpha nxt
            _ -> False

consumeDuckDbLineEcho :: Text -> [Text] -> [Text]
consumeDuckDbLineEcho header linesAfter =
    let (inDq0, inSq0, _) = scanDuckDbErrorDelimiters header (False, False, 0)
     in fromMaybe linesAfter (loop inDq0 inSq0 linesAfter)
    where
        loop _ _ [] = Nothing
        loop inDq inSq (nxt : tl)
            | isDuckDbCaretEnd nxt = Just tl
            | not inDq
                && not inSq
                && ( T.null nxtStripped
                        || isDuckDbLineEcho nxtStripped
                        || isDuckDbNonfatalError nxtStripped
                        || "Out of Memory Error:" `T.isPrefixOf` nxtStripped
                        || "INTERNAL Error:" `T.isPrefixOf` nxtStripped
                        || "FATAL Error:" `T.isPrefixOf` nxtStripped
                        || isJust (parseAsanBanner nxtStripped)
                        || isJust (parseAsanSummary nxtStripped)
                        || isJust (parseUbsanRuntime nxtStripped)
                        || isJust (parseCAssertion True nxtStripped)
                        || isJust (parseBsdAssertion nxtStripped)
                        || looksLikeBacktraceLine nxtStripped
                        || any
                            (`T.isPrefixOf` nxtStripped)
                            ["This error signals an assertion failure", "Possible solutions:", "Stack Trace:"]
                   ) =
                Nothing
            | otherwise =
                let (inDq', inSq', _) = scanDuckDbErrorDelimiters nxt (inDq, inSq, 0)
                 in loop inDq' inSq' tl
            where
                nxtStripped = T.strip nxt

consumeDuckDbNonfatalError :: Text -> [Text] -> [Text]
consumeDuckDbNonfatalError header linesAfter =
    let isUnquotedCat = isDuckDbCatalogWithName header
        (inDq0, inSq0, parens0, sawCat0)
            | isUnquotedCat = (False, False, 0, isDuckDbCatalogEnd header)
            | otherwise =
                let (d, sq, p) = scanDuckDbErrorDelimiters header (False, False, 0)
                 in (d, sq, p, True)
     in case pass1LineEcho isUnquotedCat inDq0 inSq0 parens0 sawCat0 linesAfter of
            Just afterLineCaret -> afterLineCaret
            Nothing ->
                let afterUnclosed = pass2Unclosed isUnquotedCat inDq0 inSq0 parens0 sawCat0 linesAfter
                 in dropDuckDbHints afterUnclosed
    where
        pass1LineEcho _ _ _ _ _ [] = Nothing
        pass1LineEcho unqDq inDq inSq parens sawCat (nxt : tl)
            | isDuckDbLineEcho s = Just (consumeDuckDbLineEcho nxt tl)
            | not inDq
                && not inSq
                && parens == 0
                && sawCat
                && ( any
                        (`T.isPrefixOf` s)
                        [ "This error signals an assertion failure"
                        , "Possible solutions:"
                        , "Stack Trace:"
                        , "For more information, see"
                        ]
                        || isDuckDbNonfatalError s
                   ) =
                Nothing
            | unqDq && not sawCat =
                pass1LineEcho unqDq inDq inSq parens (isDuckDbCatalogEnd s) tl
            | otherwise =
                let (d', sq', p') = scanDuckDbErrorDelimiters s (inDq, inSq, parens)
                 in pass1LineEcho unqDq d' sq' p' sawCat tl
            where
                s = T.strip nxt

        pass2Unclosed _ _ _ _ _ [] = []
        pass2Unclosed unqDq inDq inSq parens sawCat remaining@(nxt : tl)
            | not (inDq || inSq || parens > 0 || (unqDq && not sawCat)) = remaining
            | not inDq
                && not inSq
                && parens == 0
                && sawCat
                && ( any
                        (`T.isPrefixOf` s)
                        [ "This error signals an assertion failure"
                        , "Possible solutions:"
                        , "Stack Trace:"
                        , "For more information, see"
                        ]
                        || isJust (parseAsanBanner s)
                        || "FATAL Error:" `T.isPrefixOf` s
                        || "INTERNAL Error:" `T.isPrefixOf` s
                        || "Out of Memory Error:" `T.isPrefixOf` s
                        || isDuckDbNonfatalError s
                   ) =
                remaining
            | unqDq && not sawCat =
                pass2Unclosed unqDq inDq inSq parens (isDuckDbCatalogEnd s) tl
            | otherwise =
                let (d', sq', p') = scanDuckDbErrorDelimiters s (inDq, inSq, parens)
                 in pass2Unclosed unqDq d' sq' p' sawCat tl
            where
                s = T.strip nxt

        dropDuckDbHints xs = case span (T.null . T.strip) xs of
            (_, y : ys)
                | any (`T.isPrefixOf` T.strip y) ["Did you mean", "Candidate bindings:", "Run '.help'", "See also "] ->
                    dropDuckDbHints ys
            _ -> xs

isDuckDbLineEcho :: Text -> Bool
isDuckDbLineEcho s = case T.stripPrefix "LINE " s of
    Just r ->
        let d = T.takeWhile isDigit r
         in not (T.null d) && ":" `T.isPrefixOf` T.drop (T.length d) r
    Nothing -> False

isDuckDbCaretEnd :: Text -> Bool
isDuckDbCaretEnd raw =
    let s = T.strip raw
     in not (T.null s) && T.all (== '^') s

isDuckDbNonfatalError :: Text -> Bool
isDuckDbNonfatalError s
    | any (`T.isPrefixOf` s) ["INTERNAL Error:", "FATAL Error:", "Out of Memory Error:"] = False
    | otherwise = case T.breakOn " Error:" s of
        (lbl, rest)
            | not (T.null rest) ->
                case T.uncons lbl of
                    Just (c, tl) ->
                        isAsciiUpper c && T.all (\ch -> isAlpha ch || ch == ' ') tl
                    Nothing -> False
        _ -> False

isDuckDbCatalogWithName :: Text -> Bool
isDuckDbCatalogWithName s = case T.stripPrefix "Catalog Error:" s of
    Just rest -> hasWordWithName (T.dropWhile isSpace rest)
    Nothing -> False
    where
        hasWordWithName txt =
            elem "with name" (windows (T.words txt))
                || " with name " `T.isInfixOf` (" " <> txt <> " ")
        windows (x : y : zs) = (x <> " " <> y) : windows (y : zs)
        windows _ = []

isDuckDbCatalogEnd :: Text -> Bool
isDuckDbCatalogEnd s =
    let body = T.stripEnd s
        trimmed = fromMaybe body (T.stripSuffix "!" body <|> T.stripSuffix "." body)
     in any (`T.isSuffixOf` trimmed) ["does not exist", "already exists", "not found"]

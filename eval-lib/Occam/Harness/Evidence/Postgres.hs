-- | PostgreSQL server log and stderr crash evidence extractor.
module Occam.Harness.Evidence.Postgres (
    extractPostgresEvidence,
) where

import Data.Char (isDigit, isHexDigit, isSpace)
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Harness.Evidence.Common (
    hasAsanInLine,
    makeFault,
    matchesDateTemplate,
    parseUbsanRuntime,
    pgConnectionLostPhrases,
    pgCrashShutdownPhrases,
    splitLinesPy,
 )
import Occam.Harness.Signature (
    AssertionDetails (..),
    Corroboration (..),
    DbmsKind (..),
    EvidenceResult (..),
    FaultEvent (..),
    FaultPayload (..),
    SanitizerTool (..),
    emptyCorroboration,
    normalizeSourcePath,
 )
import Text.Read (readMaybe)

-- | Extract PostgreSQL server log faults (assertions, PANICs, postmaster child exits, ASan/UBSan) and stderr corroboration.
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
            | T.null (T.strip rawLine) =
                go rest inExcl faults cands bPids cPids s57
            | isPgCsvStart rawLine && "," `T.isInfixOf` rawLine
            , let (csvRowTxt, restAfterCsv) = accumulateCsv rawLine rest
            , Just (pid, sev, sqlstate, msg) <- parsePgCsvRow csvRowTxt =
                let s57' = s57 || sqlstate == "57P02"
                 in if isPgExcludedSeverity sev msg
                        then go restAfterCsv False faults cands bPids cPids s57'
                        else
                            processEntry
                                restAfterCsv
                                False
                                (Just pid)
                                sev
                                msg
                                (sev <> ": " <> msg)
                                faults
                                cands
                                bPids
                                cPids
                                s57'
            | "\t" `T.isPrefixOf` rawLine =
                if inExcl
                    then go rest True faults cands bPids cPids s57
                    else go rest False faults (T.strip rawLine : cands) bPids cPids s57
            | Just (mPid, sev, msg) <- parsePgStderrLine stripped =
                if isPgExcludedSeverity sev msg
                    then go rest True faults cands bPids cPids s57
                    else
                        processEntry
                            rest
                            False
                            mPid
                            sev
                            msg
                            (sev <> ": " <> msg)
                            faults
                            cands
                            bPids
                            cPids
                            (s57 || isFatal57p02 sev msg)
            | otherwise =
                processEntry rest False Nothing "" stripped stripped faults cands bPids cPids s57
            where
                stripped = T.strip rawLine

        isFatal57p02 sev msg =
            sev == "FATAL"
                && ( "57P02" `T.isInfixOf` msg
                        || "terminating connection because of crash of another server process" `T.isInfixOf` msg
                   )

        accumulateCsv buf [] = (buf, [])
        accumulateCsv buf (nxt : tl)
            | odd (T.count "\"" buf) = accumulateCsv (buf <> "\n" <> nxt) tl
            | otherwise = (buf, nxt : tl)

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

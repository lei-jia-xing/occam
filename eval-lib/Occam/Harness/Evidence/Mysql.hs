-- | MySQL and MariaDB server log crash evidence extractors.
module Occam.Harness.Evidence.Mysql (
    extractMariadbEvidence,
    extractMysqlEvidence,
) where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isDigit, isHexDigit, isSpace)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Harness.Evidence.Common (
    connectionLostPhrasesForDbms,
    hasAsanInLine,
    makeFault,
    matchesDateTemplate,
    parseBsdAssertion,
    parseCAssertion,
    parseUbsanRuntime,
    skipWs1,
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
    dbmsKindName,
    decodeTerminationSignal,
    emptyCorroboration,
    normalizeSourcePath,
 )
import Text.Read (readMaybe)

-- | Extract MySQL server log faults (assertions, InnoDB/signal backtraces, supervisor exits).
extractMysqlEvidence :: Text -> Text -> EvidenceResult
extractMysqlEvidence = extractMysqlFamilyEvidence DbmsMySql

-- | Extract MariaDB server log faults (assertions, signal backtraces, supervisor exits).
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
            | T.null (T.strip rawLine) =
                go rest False False faults cands sups
            | Just status <- parseMysqlSupervisorExit line =
                recordSupervisorExit rest line status faults cands sups
            | Just (level, mCode, mSubsys, msg) <- parseMysqlLogHeader dbms line =
                if isIgnoredMysqlHeader level mCode msg
                    then go rest False True faults cands sups
                    else classifyLine rest line msg mCode mSubsys faults cands sups
            | isMysqlQueryEcho line =
                go rest True False faults cands sups
            | inEcho
                && any (`T.isPrefixOf` line) ["Connection ID", "Status:", "The manual page", "Optimizer switch"] =
                classifyUnprefixed rest False inExcl line faults cands sups
            | inEcho =
                go rest True inExcl faults cands sups
            | inExcl =
                go rest False True faults cands sups
            | otherwise =
                classifyUnprefixed rest False False line faults cands sups
            where
                line = T.strip rawLine

        recordSupervisorExit rest line status faults cands sups
            | status /= 0 =
                let mSig = fst <$> decodeTerminationSignal status
                    f =
                        makeFault
                            dbms
                            (dbmsKindName dbms <> "_supervisor_abnormal_exit")
                            line
                            (PayloadSupervisorExit status mSig)
                 in go rest False False (f : faults) (line : cands) (status : sups)
            | otherwise = go rest False False faults cands (status : sups)

        isIgnoredMysqlHeader level mCode msg =
            level `elem` ["Note", "Warning"]
                || ("Aborted connection " `T.isInfixOf` msg && hasAbortedDigits msg)
                || mCode `elem` [Just "MY-010914", Just "MY-010926"]

        classifyUnprefixed rest _ _ line faults cands sups
            | hasAbortedConn line = go rest False True faults cands sups
            | otherwise = classifyLine rest line line Nothing Nothing faults cands sups

        classifyLine rest line msg mCode mSubsys faults cands sups =
            let mFault = parseMysqlFault dbms line msg mCode mSubsys
             in go rest False False (maybe faults (: faults) mFault) (line : cands) sups

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

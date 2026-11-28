-- | ClickHouse server log crash evidence extractor.
module Occam.Harness.Evidence.ClickHouse (
    extractClickHouseEvidence,
) where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Harness.Evidence.Common (
    clickHouseConnectionLostPhrases,
    hasAsanInLine,
    looksLikeBacktraceLine,
    makeFault,
    matchesDateTemplate,
    parseAsanBanner,
    parseAsanSummary,
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
    InternalFaultKind (..),
    SanitizerTool (..),
    decodeTerminationSignal,
    emptyCorroboration,
    parseStackFrame,
 )
import Text.Read (readMaybe)

-- | Extract ClickHouse server log faults (Logical error, Signal, ASan, supervisor exit) while stripping query echoes.
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
            | T.null line =
                go rest inExcl inFatal faults cands sups
            | Just status <- parseClickHouseSupervisorExit line =
                recordSupervisorExit rest line status faults cands sups
            | Just (lvl, comp, msg0) <- parseClickHouseLogLine line =
                if lvl /= "Fatal"
                    then go rest True False faults cands sups
                    else processFatalLogLine rest comp msg0 faults cands sups
            | inExcl =
                go rest True False faults cands sups
            | isUnprefixedFaultLine inFatal line =
                let mFault = classifyClickHouseMsg "" line line
                 in go rest False inFatal (maybe faults (: faults) mFault) (line : cands) sups
            | otherwise =
                go rest False inFatal faults cands sups
            where
                line = T.strip rawLine

        recordSupervisorExit rest line status faults cands sups
            | status /= 0 =
                let mSig = fst <$> decodeTerminationSignal status
                    f =
                        makeFault
                            DbmsClickHouse
                            "clickhouse_supervisor_abnormal_exit"
                            line
                            (PayloadSupervisorExit status mSig)
                 in go rest False False (f : faults) (line : cands) (status : sups)
            | otherwise = go rest False False faults cands (status : sups)

        processFatalLogLine rest comp msg0 faults cands sups
            | T.null fullMsg || T.all (== '#') fullMsg || T.null cleanMsg =
                go rest' False True faults cands sups
            | otherwise =
                let cleanLine
                        | T.null comp = "<Fatal> : " <> cleanMsg
                        | otherwise = "<Fatal> " <> comp <> ": " <> cleanMsg
                    mFault = classifyClickHouseMsg comp cleanLine cleanMsg
                 in go rest' False True (maybe faults (: faults) mFault) (cleanLine : cands) sups
            where
                (fullMsg, rest') = accumulateQueryEcho msg0 rest
                cleanMsg = stripClickHouseQueryEcho fullMsg

        isUnprefixedFaultLine inFatal line =
            isJust (parseAsanBanner line)
                || isJust (parseAsanSummary line)
                || isJust (parseUbsanRuntime line)
                || isJust (parseCAssertion True line)
                || isJust (parseBsdAssertion line)
                || looksLikeBacktraceLine line
                || (inFatal && isJust (parseStackFrame line))

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

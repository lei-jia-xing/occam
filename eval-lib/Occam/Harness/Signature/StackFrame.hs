-- | POSIX termination signal decoding, stack-frame parsing, and volatile-token normalization.
module Occam.Harness.Signature.StackFrame (
    crashSignals,
    externalOrOomSignals,
    signalLabel,
    decodeTerminationSignal,
    normalizeSourcePath,
    normalizeVolatileTokens,
    parseStackFrame,
    skipWs1,
    extractStackFrames,
) where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isDigit, isHexDigit, isSpace)
import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (takeFileName)

-- | Signals classified as actual DBMS crashes vs external/OOM termination.
crashSignals :: Set.Set Int
crashSignals = Set.fromList [4, 5, 6, 7, 8, 11, 16, 31]

-- | Signals associated with external kills or out-of-memory termination.
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

-- | Render a POSIX signal number to its symbolic name (e.g. @"SIGSEGV"@).
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

-- | Consume one or more leading whitespace characters, returning 'Nothing' when unpadded.
skipWs1 :: Text -> Maybe Text
skipWs1 txt = case T.span isSpace txt of
    (ws, rest) | not (T.null ws) -> Just rest
    _ -> Nothing

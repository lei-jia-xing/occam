-- | DuckDB process stderr and exit-signal crash evidence extractor.
module Occam.Harness.Evidence.DuckDB (
    extractDuckDbEvidence,
) where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAsciiUpper, isDigit, isSpace)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Harness.Evidence.Common (
    EmbeddedSanitizerContext (..),
    applyEmbeddedProcessSignal,
    embeddedSanitizerContext,
    looksLikeBacktraceLine,
    makeFault,
    parseAsanBanner,
    parseAsanSummary,
    parseBsdAssertion,
    parseCAssertion,
    parseUbsanRuntime,
    splitLinesPy,
 )
import Occam.Harness.Signature (
    Corroboration (..),
    DbmsKind (..),
    EvidenceResult (..),
    FaultPayload (..),
    InternalFaultKind (..),
    SanitizerTool (..),
    emptyCorroboration,
 )

-- | Extract DuckDB crash evidence (INTERNAL/FATAL Error, assertions, ASan/UBSan, signal/OOM) while skipping nonfatal blocks.
extractDuckDbEvidence :: Text -> Int -> EvidenceResult
extractDuckDbEvidence stderrText retcode =
    let rawLines = splitLinesPy stderrText
        ctx = embeddedSanitizerContext rawLines retcode
        hasCrashSig = escHasCrashSig ctx
        hasSanCorr = escHasSanCorr ctx
        hasAbrtSig = escHasAbrtSig ctx

        go [] faults cands oom oomCause dbInv = (reverse faults, reverse cands, oom, oomCause, dbInv)
        go (rawLine : rest) faults cands oom oomCause dbInv
            | isDuckDbLineEcho stripped =
                go (consumeDuckDbLineEcho rawLine rest) faults cands oom oomCause dbInv
            | T.null stripped =
                go rest faults cands oom oomCause dbInv
            | "Out of Memory Error:" `T.isPrefixOf` stripped =
                go rest faults cands True (Just "duckdb_oom_error") dbInv
            | isDuckDbNonfatalError stripped =
                go (consumeDuckDbNonfatalError stripped rest) faults cands oom oomCause dbInv
            | looksLikeBacktraceLine stripped =
                go rest faults (stripped : cands) oom oomCause dbInv
            | Just inlineMsg <- stripInternalError stripped =
                let (parts, rest') = collectInternalBody (maybe [] pure (nonEmpty inlineMsg)) rest
                    body = T.strip (T.unwords parts)
                    detail = if T.null body then "INTERNAL Error" else "INTERNAL Error: " <> body
                    f = makeFault DbmsDuckDb "duckdb_internal_error" detail (PayloadInternal KindDuckDbInternal body)
                 in go rest' (f : faults) (detail : cands) oom oomCause dbInv
            | Just inlineMsg <- stripFatalInvalidated stripped =
                let (parts, rest') = collectFatalBody (maybe [] pure (nonEmpty inlineMsg)) rest
                    body = T.strip (T.unwords parts)
                    lowBody = T.toLower body
                    triggered = "invalidated" `T.isInfixOf` lowBody || "fatal error" `T.isInfixOf` lowBody || not (T.null body)
                 in if not triggered
                        then go rest' faults cands oom oomCause dbInv
                        else
                            let detail = if T.null body then "FATAL Error" else "FATAL Error: " <> body
                                f =
                                    makeFault
                                        DbmsDuckDb
                                        "duckdb_database_invalidated"
                                        detail
                                        (PayloadInternal KindDuckDbInvalidated body)
                             in go rest' (f : faults) (detail : cands) oom oomCause True
            | Just ad <- parseCAssertion True stripped <|> parseBsdAssertion stripped
            , hasAbrtSig =
                let f = makeFault DbmsDuckDb "duckdb_assertion_failure" stripped (PayloadAssertion ad)
                 in go rest (f : faults) (stripped : cands) oom oomCause dbInv
            | isJust (parseAsanBanner stripped) && (hasCrashSig || hasSanCorr) =
                let f = makeFault DbmsDuckDb "duckdb_asan_fault" stripped (PayloadSanitizer (Just ToolAsan))
                 in go rest (f : faults) (stripped : cands) oom oomCause dbInv
            | isJust (parseAsanSummary stripped) && (hasCrashSig || hasSanCorr) =
                go rest faults (stripped : cands) oom oomCause dbInv
            | isJust (parseUbsanRuntime stripped) =
                let f = makeFault DbmsDuckDb "duckdb_asan_fault" stripped (PayloadSanitizer (Just ToolUbsan))
                 in go rest (f : faults) (stripped : cands) oom oomCause dbInv
            | otherwise =
                go rest faults cands oom oomCause dbInv
            where
                stripped = T.strip rawLine

        (faults0, cands0, oom0, oomCause0, dbInv0) = go rawLines [] [] False Nothing False
        (faults1, cands1, oom1, oomCause1, extKill) =
            applyEmbeddedProcessSignal
                DbmsDuckDb
                "duckdb"
                "oom_kill_signal_9"
                (escSigInfo ctx)
                faults0
                cands0
                oom0
                oomCause0
        corr =
            emptyCorroboration
                { corRetcode = Just retcode
                , corSignal = fst <$> escSigInfo ctx
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

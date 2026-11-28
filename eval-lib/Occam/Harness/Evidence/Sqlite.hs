-- | SQLite process stderr and exit-signal crash evidence extractor.
module Occam.Harness.Evidence.Sqlite (
    extractSqliteEvidence,
) where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
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
    SanitizerTool (..),
    emptyCorroboration,
 )

-- | Extract SQLite crash evidence (C/BSD assertions, ASan/UBSan, signal termination, SQLITE_ABORT) while stripping error echoes.
extractSqliteEvidence :: Text -> Int -> EvidenceResult
extractSqliteEvidence stderrText retcode =
    let rawLines = stripSqliteErrorBlocks (splitLinesPy stderrText)
        ctx = embeddedSanitizerContext rawLines retcode
        hasCrashSig = escHasCrashSig ctx
        hasSanCorr = escHasSanCorr ctx
        hasAbrtSig = escHasAbrtSig ctx

        scan [] _ faults cands = (reverse faults, reverse cands)
        scan (rawLine : rest) inErr faults cands
            | T.null stripped =
                scan rest inErr faults cands
            | isSqliteErrorStart stripped =
                scan rest True faults cands
            | isSqliteCaretEnd rawLine =
                scan rest False faults cands
            | looksLikeBacktraceLine stripped && (hasCrashSig || hasSanCorr || not inErr) =
                scan rest False faults (stripped : cands)
            | "  " `T.isPrefixOf` rawLine && inErr =
                scan rest True faults cands
            | Just ad <- parseCAssertion True stripped <|> parseBsdAssertion stripped =
                if hasAbrtSig
                    then
                        let f = makeFault DbmsSqlite "sqlite_assertion_failure" stripped (PayloadAssertion ad)
                         in scan rest False (f : faults) (stripped : cands)
                    else scan rest inErr faults cands
            | isJust (parseAsanBanner stripped) && (hasCrashSig || hasSanCorr) =
                let f = makeFault DbmsSqlite "sqlite_asan_fault" stripped (PayloadSanitizer (Just ToolAsan))
                 in scan rest False (f : faults) (stripped : cands)
            | isJust (parseAsanSummary stripped) && (hasCrashSig || hasSanCorr) =
                scan rest False faults (stripped : cands)
            | isJust (parseUbsanRuntime stripped) && (hasCrashSig || not inErr) =
                let f = makeFault DbmsSqlite "sqlite_asan_fault" stripped (PayloadSanitizer (Just ToolUbsan))
                 in scan rest False (f : faults) (stripped : cands)
            | otherwise =
                scan rest inErr faults cands
            where
                stripped = T.strip rawLine

        (faults0, cands0) = scan rawLines False [] []
        sqliteAbort = retcode == 4 && null faults0 && not hasCrashSig
        (faults1, cands1, oom, oomCause, extKill) =
            applyEmbeddedProcessSignal
                DbmsSqlite
                "sqlite3"
                "signal_9_kill"
                (escSigInfo ctx)
                faults0
                cands0
                False
                Nothing
        corr =
            emptyCorroboration
                { corRetcode = Just retcode
                , corSignal = fst <$> escSigInfo ctx
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

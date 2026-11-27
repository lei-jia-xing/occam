-- | Top-level statement splitter; keeps compound routine/trigger/CASE blocks whole.
module Occam.Frontend.Splitter (
    SplitUnit (..),
    splitSQL,
    splitTokens,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Occam.Frontend.Lexer (
    Keyword (..),
    LexError (..),
    LexErrorKind (..),
    LexResult (..),
    LiteralKind (..),
    SpannedToken,
    Token (..),
    WordToken (..),
    asciiEqCI,
    isTriviaToken,
    lexSQL,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..), unsafeSliceSpan)
import Occam.Parser.Dialect (DialectProfile (..), LexicalProfile (..))
import Occam.Parser.Types (SQLDialect (..))

-- | A top-level statement slice produced prior to full grammar parsing.
data SplitUnit = SplitUnit
    { splitUnitSpan :: !ByteSpan
    , splitUnitDelimiterSpan :: !(Maybe ByteSpan)
    , splitUnitTokens :: !(Vector SpannedToken)
    , splitUnitLexError :: !(Maybe LexError)
    , splitUnitDelimiterDirective :: !(Maybe (Located ByteString))
    }
    deriving (Eq, Show)

data BlockKind
    = BlockBegin
    | BlockCase
    | BlockIf
    | BlockLoop
    | BlockWhile
    | BlockRepeat
    | BlockFor
    deriving (Eq, Show)

data HeaderKind
    = HeaderNone
    | HeaderCreate
    | HeaderAlter
    | HeaderAllowCompound
    | HeaderDisallowCompound
    deriving (Eq, Show)

data SplitState = SplitState
    { ssParenDepth :: !Int
    , ssBracketDepth :: !Int
    , ssBraceDepth :: !Int
    , ssBlockStack :: ![BlockKind]
    , ssHeaderKind :: !HeaderKind
    , ssStmtStartTok :: !(Maybe Int)
    , ssLastNonTriviaTok :: !(Maybe Int)
    , ssPrevNonTrivia :: !(Maybe Token)
    , ssDelimiter :: !ByteString
    , ssLastTopLevelEndIdx :: !Int
    , ssAcc :: ![SplitUnit]
    }

-- | Lex and split @sql@ into top-level statement units under @profile@.
splitSQL :: DialectProfile -> ByteString -> Either LexError [SplitUnit]
splitSQL profile sql = splitTokens profile sql (lexSQL profile sql)

-- | Split a pre-lexed 'LexResult' into top-level statement units.
splitTokens :: DialectProfile -> ByteString -> LexResult -> Either LexError [SplitUnit]
splitTokens profile sql (LexResult vec mErr) = case mErr of
    Just err@LexError {lexErrorKind = LexInvalidUtf8} -> Left err
    _ -> Right (runSplit profile sql vec mErr)

runSplit ::
    DialectProfile ->
    ByteString ->
    Vector SpannedToken ->
    Maybe LexError ->
    [SplitUnit]
runSplit profile sql vec mErr = go 0 initState
    where
        !n = V.length vec
        !sqlLen = BS.length sql
        !lp = dialectLexical profile
        !dialect = dialectId profile
        !supportsDelimiter = dialect == MySQL || dialect == MariaDB
        !compoundEnabled = lexCompoundBlocks lp

        initState =
            SplitState
                { ssParenDepth = 0
                , ssBracketDepth = 0
                , ssBraceDepth = 0
                , ssBlockStack = []
                , ssHeaderKind = HeaderNone
                , ssStmtStartTok = Nothing
                , ssLastNonTriviaTok = Nothing
                , ssPrevNonTrivia = Nothing
                , ssDelimiter = ";"
                , ssLastTopLevelEndIdx = 0
                , ssAcc = []
                }

        {-# INLINE tokAt #-}
        tokAt !i = vec V.! i

        nextNonTriviaFrom !k
            | k >= n = Nothing
            | isTriviaToken (locValue (tokAt k)) = nextNonTriviaFrom (k + 1)
            | otherwise = Just k

        nextNonTriviaTok !k = fmap (locValue . tokAt) (nextNonTriviaFrom k)

        nextTwoNonTriviaToks !k = case nextNonTriviaFrom k of
            Nothing -> (Nothing, Nothing)
            Just i1 -> (Just (locValue (tokAt i1)), nextNonTriviaTok (i1 + 1))

        {-# INLINE atTopLevel #-}
        atTopLevel st = outsideGrouping st && null (ssBlockStack st)

        {-# INLINE outsideGrouping #-}
        outsideGrouping st =
            ssParenDepth st == 0 && ssBracketDepth st == 0 && ssBraceDepth st == 0

        go !i !st
            | i >= n = finalize st
            | isTriviaToken tok = go (i + 1) st
            | otherwise = case ssStmtStartTok st of
                Nothing -> handleBetweenStatements i tok st
                Just sIdx -> handleInStatement False i tok sIdx st
            where
                tok = locValue (tokAt i)

        handleBetweenStatements !i !tok !st
            | ssDelimiter st == ";" && tok == TokSemicolon =
                go (i + 1) st {ssLastTopLevelEndIdx = i + 1}
            | ssDelimiter st /= ";"
            , Just (!nextIdx, _) <- matchCustomDelimiter i (ssDelimiter st) =
                go nextIdx st {ssLastTopLevelEndIdx = nextIdx}
            | supportsDelimiter
            , isDelimiterWord tok
            , Just (!delimEndByte, !newDelim, !afterArgIdx) <- parseDelimiterDirective i =
                let !startByte = spanStart (locSpan (tokAt i))
                    !unit =
                        SplitUnit
                            { splitUnitSpan = ByteSpan startByte delimEndByte
                            , splitUnitDelimiterSpan = Nothing
                            , splitUnitTokens = V.slice i (afterArgIdx - i) vec
                            , splitUnitLexError = Nothing
                            , splitUnitDelimiterDirective =
                                Just (L (ByteSpan (delimEndByte - BS.length newDelim) delimEndByte) newDelim)
                            }
                 in go
                        afterArgIdx
                        st
                            { ssDelimiter = newDelim
                            , ssLastTopLevelEndIdx = afterArgIdx
                            , ssAcc = unit : ssAcc st
                            }
            | otherwise =
                let !hdr = case tok of
                        TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwCreate} -> HeaderCreate
                        TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwAlter} -> HeaderAlter
                        _ -> HeaderDisallowCompound
                    !stStarted =
                        st
                            { ssHeaderKind = hdr
                            , ssStmtStartTok = Just i
                            , ssLastNonTriviaTok = Just i
                            }
                 in handleInStatement True i tok i stStarted

        handleInStatement !isFirst !i !tok !sIdx !st
            | ssDelimiter st == ";"
            , tok == TokSemicolon
            , atTopLevel st =
                emitUnit sIdx (locSpan (tokAt i)) Nothing (i + 1) st
            | let delimiter = ssDelimiter st
            , delimiter /= ";"
            , outsideGrouping st =
                case matchCustomDelimiter i delimiter of
                    Just (nextIdx, delimSpan) -> emitUnit sIdx delimSpan Nothing nextIdx st
                    Nothing -> case attachedDelimiter i tok delimiter of
                        Just delimSpan -> emitUnit sIdx delimSpan (Just i) (i + 1) st
                        Nothing -> stepToken isFirst i tok st
            | otherwise = stepToken isFirst i tok st

        -- A client delimiter can be part of a lexically valid bare identifier.
        -- Keep the ordinary tokenization until the delimiter is actually active.
        attachedDelimiter !i tok delimiter = case tok of
            TokWord WordToken {wordQuote = Nothing, wordRaw = raw}
                | BS.length raw > BS.length delimiter
                , BS.isSuffixOf delimiter raw ->
                    let !wordEnd = spanEnd (locSpan (tokAt i))
                     in Just (ByteSpan (wordEnd - BS.length delimiter) wordEnd)
            _ -> Nothing

        emitUnit !sIdx !delimSpan !clippedWord !nextIdx !st =
            let !eIdx = fromMaybe sIdx (ssLastNonTriviaTok st)
                !startByte = spanStart (locSpan (tokAt sIdx))
                (!endByte, !tokens) = case clippedWord of
                    Nothing -> (spanEnd (locSpan (tokAt eIdx)), V.slice sIdx (eIdx - sIdx + 1) vec)
                    Just i ->
                        let !end = spanStart delimSpan
                            !wordStart = spanStart (locSpan (tokAt i))
                            prefix = unsafeSliceSpan (ByteSpan wordStart end) sql
                            offset (L (ByteSpan lo hi) tok) = L (ByteSpan (wordStart + lo) (wordStart + hi)) tok
                         in (end, V.slice sIdx (i - sIdx) vec V.++ V.map offset (lexTokens (lexSQL profile prefix)))
                !unit =
                    SplitUnit
                        { splitUnitSpan = ByteSpan startByte endByte
                        , splitUnitDelimiterSpan = Just delimSpan
                        , splitUnitTokens = tokens
                        , splitUnitLexError = Nothing
                        , splitUnitDelimiterDirective = Nothing
                        }
             in go
                    nextIdx
                    st
                        { ssParenDepth = 0
                        , ssBracketDepth = 0
                        , ssBraceDepth = 0
                        , ssBlockStack = []
                        , ssHeaderKind = HeaderNone
                        , ssStmtStartTok = Nothing
                        , ssLastNonTriviaTok = Nothing
                        , ssPrevNonTrivia = Nothing
                        , ssLastTopLevelEndIdx = nextIdx
                        , ssAcc = unit : ssAcc st
                        }

        stepToken !isFirst !i !tok !st = case tok of
            TokLParen -> advance (i + 1) tok st {ssParenDepth = ssParenDepth st + 1}
            TokRParen -> advance (i + 1) tok st {ssParenDepth = max 0 (ssParenDepth st - 1)}
            TokLBracket -> advance (i + 1) tok st {ssBracketDepth = ssBracketDepth st + 1}
            TokRBracket -> advance (i + 1) tok st {ssBracketDepth = max 0 (ssBracketDepth st - 1)}
            TokLBrace -> advance (i + 1) tok st {ssBraceDepth = ssBraceDepth st + 1}
            TokRBrace -> advance (i + 1) tok st {ssBraceDepth = max 0 (ssBraceDepth st - 1)}
            TokWord wt@WordToken {wordQuote = Nothing, wordKeyword = mKw}
                | compoundEnabled
                , ssParenDepth st == 0
                , ssBracketDepth st == 0
                , ssBraceDepth st == 0 ->
                    let !stHdr = updateHeaderState isFirst wt mKw st
                     in case mKw of
                            Just kw
                                | ssPrevNonTrivia stHdr /= Just TokDot ->
                                    stepCompoundKeyword isFirst i tok kw stHdr
                            _ -> advance (i + 1) tok stHdr
            _ -> advance (i + 1) tok st

        updateHeaderState !isFirst wt mKw st
            | isFirst = st
            | not (null (ssBlockStack st)) = st
            | otherwise = case ssHeaderKind st of
                HeaderCreate -> case mKw of
                    Just KwTrigger -> st {ssHeaderKind = HeaderAllowCompound}
                    Just KwProcedure -> st {ssHeaderKind = HeaderAllowCompound}
                    Just KwFunction -> st {ssHeaderKind = HeaderAllowCompound}
                    Nothing | asciiEqCI (wordRaw wt) "EVENT" -> st {ssHeaderKind = HeaderAllowCompound}
                    Just kw | isNonCompoundCreateTarget kw -> st {ssHeaderKind = HeaderDisallowCompound}
                    Nothing | isNonCompoundCreateWord (wordRaw wt) -> st {ssHeaderKind = HeaderDisallowCompound}
                    _ -> st
                HeaderAlter -> case mKw of
                    Nothing | asciiEqCI (wordRaw wt) "EVENT" -> st {ssHeaderKind = HeaderAllowCompound}
                    Nothing | asciiEqCI (wordRaw wt) "DEFINER" -> st
                    Just kw | isNonCompoundCreateTarget kw -> st {ssHeaderKind = HeaderDisallowCompound}
                    Nothing | isNonCompoundCreateWord (wordRaw wt) -> st {ssHeaderKind = HeaderDisallowCompound}
                    _ -> st
                _ -> st

        isNonCompoundCreateTarget kw =
            kw
                `elem` [ KwTable
                       , KwView
                       , KwIndex
                       , KwSchema
                       , KwDatabase
                       , KwSequence
                       , KwRule
                       , KwConversion
                       , KwStatistics
                       ]

        isNonCompoundCreateWord raw =
            any
                (asciiEqCI raw)
                [ "POLICY"
                , "TYPE"
                , "DOMAIN"
                , "EXTENSION"
                , "AGGREGATE"
                , "OPERATOR"
                , "CAST"
                , "FOREIGN"
                , "USER"
                , "ROLE"
                , "TABLESPACE"
                , "COLLATION"
                , "PUBLICATION"
                , "SUBSCRIPTION"
                , "TEXT"
                ]

        {-# INLINE advance #-}
        advance !nextIdx !lastTok !st =
            go
                nextIdx
                st
                    { ssLastNonTriviaTok = Just (nextIdx - 1)
                    , ssPrevNonTrivia = Just lastTok
                    }

        stepCompoundKeyword !isFirst !i !tok !kw !st
            | kw == KwEnd = handleEnd i tok st
            | otherwise =
                let !stack' = maybe (ssBlockStack st) (: ssBlockStack st) (compoundBlockToPush isFirst i kw st)
                 in advance (i + 1) tok st {ssBlockStack = stack'}

        compoundBlockToPush !isFirst !i !kw !st = case kw of
            KwBegin ->
                BlockBegin
                    <$ guard
                        ( if null stack
                            then
                                (isFirst && dialect == MariaDB && not (isTransactionBegin (i + 1)))
                                    || ( ssHeaderKind st == HeaderAllowCompound
                                            && (not (isBeginExcludedPrev prev) || startsBodyAfterAs prev (i + 1))
                                       )
                                    || isBeginAtomic (i + 1)
                            else isCompoundStmtStart prev
                        )
            KwCase ->
                BlockCase
                    <$ guard ((isFirst || not (null stack)) && not (isPrevKeyword KwAs prev))
            KwIf -> BlockIf <$ guard (nestedStart && not (isDdlOrFuncIf (i + 1)))
            KwLoop -> BlockLoop <$ guard nestedStart
            KwWhile -> BlockWhile <$ guard nestedStart
            KwRepeat -> BlockRepeat <$ guard (nestedStart && nextNonTriviaTok (i + 1) /= Just TokLParen)
            KwFor -> BlockFor <$ guard (nestedStart && not (isClauseFor (i + 1)))
            _ -> Nothing
            where
                stack = ssBlockStack st
                prev = ssPrevNonTrivia st
                nestedStart = not (null stack) && isCompoundStmtStart prev

        isPrevKeyword expected = \case
            Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) -> kw == expected
            _ -> False

        isBeginExcludedPrev mPrev =
            isPrevKeyword KwAs mPrev || isPrevKeyword KwOn mPrev

        startsBodyAfterAs prev !afterIdx =
            isPrevKeyword KwAs prev && case nextNonTriviaTok afterIdx of
                Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) ->
                    kw
                        `elem` [KwBegin, KwEnd, KwFor, KwIf, KwInsert, KwLoop, KwReturn, KwSelect, KwUpdate, KwDelete, KwWhile]
                Just (TokWord WordToken {wordQuote = Nothing, wordRaw}) -> asciiEqCI "DECLARE" wordRaw
                _ -> False

        isBeginAtomic !afterIdx = case nextNonTriviaTok afterIdx of
            Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwAtomic}) -> True
            _ -> False

        isCompoundStmtStart = \case
            Just TokSemicolon -> True
            Just TokColon -> True
            Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) ->
                kw `elem` [KwBegin, KwThen, KwElse, KwDo, KwLoop, KwRepeat, KwAtomic]
            _ -> False

        isTransactionBegin !afterIdx = case nextTwoNonTriviaToks afterIdx of
            (Nothing, _) -> True
            (Just TokSemicolon, _) -> True
            (Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}), mNext2) ->
                kw
                    `elem` [ KwTransaction
                           , KwWork
                           , KwDeferred
                           , KwImmediate
                           , KwExclusive
                           , KwIsolation
                           , KwRead
                           , KwDeferrable
                           ]
                    || (kw == KwNot && isUnquotedKw KwDeferrable mNext2)
            _ -> False
            where
                isUnquotedKw expected = \case
                    Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) -> kw == expected
                    _ -> False

        isDdlOrFuncIf !afterIdx = case nextTwoNonTriviaToks afterIdx of
            (Just TokLParen, _) -> True
            (Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwExists}), _) -> True
            ( Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwNot})
                , Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwExists})
                ) -> True
            _ -> False

        isClauseFor !afterIdx = case nextNonTriviaTok afterIdx of
            Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) ->
                kw `elem` [KwEach, KwUpdate, KwShare, KwKey, KwNo, KwSystemTime]
            _ -> False

        handleEnd !i !tok !st = case ssBlockStack st of
            top : rest
                | Just nextIdx <- nextNonTriviaFrom (i + 1)
                , TokWord WordToken {wordQuote = Nothing, wordKeyword = Just nextKw} <- locValue (tokAt nextIdx)
                , closesBlock nextKw top
                , startsClosure || top `elem` [BlockCase, BlockRepeat] ->
                    advance (nextIdx + 1) (locValue (tokAt nextIdx)) st {ssBlockStack = rest}
                | top == BlockCase || (top == BlockBegin && startsClosure) ->
                    advance (i + 1) tok st {ssBlockStack = rest}
            _ -> advance (i + 1) tok st
            where
                startsClosure = isCompoundStmtStart (ssPrevNonTrivia st)

        closesBlock kw block = case kw of
            KwIf -> block == BlockIf
            KwLoop -> block == BlockLoop || (dialect == PostgreSQL && block `elem` [BlockWhile, BlockFor])
            KwWhile -> block == BlockWhile
            KwRepeat -> block == BlockRepeat
            KwFor -> block == BlockFor
            KwCase -> block == BlockCase
            _ -> False

        matchCustomDelimiter !i !delim = do
            guard (not (BS.null delim))
            guard (not (isQuotedLitOrIdent (locValue (tokAt i))))
            let !startByte = spanStart (locSpan (tokAt i))
                !dLen = BS.length delim
                !endByte = startByte + dLen
            guard (endByte <= sqlLen)
            guard (BS.isPrefixOf delim (BSU.unsafeDrop startByte sql))
            nextIdx <- consumeDelimTokens i endByte
            pure (nextIdx, ByteSpan startByte endByte)

        isQuotedLitOrIdent (TokLiteral LitNumber _) = False
        isQuotedLitOrIdent (TokLiteral {}) = True
        isQuotedLitOrIdent (TokWord WordToken {wordQuote = Just _}) = True
        isQuotedLitOrIdent _ = False

        consumeDelimTokens !k !targetEnd = do
            guard (k < n)
            guard (not (isTriviaToken (locValue (tokAt k))))
            let !e = spanEnd (locSpan (tokAt k))
            if e == targetEnd
                then pure (k + 1)
                else guard (e < targetEnd) *> consumeDelimTokens (k + 1) targetEnd

        isDelimiterWord (TokWord WordToken {wordQuote = Nothing, wordRaw = raw}) =
            BS.length raw == 9 && asciiEqCI raw "DELIMITER"
        isDelimiterWord _ = False

        parseDelimiterDirective !i = do
            let !afterKw = spanEnd (locSpan (tokAt i))
                !argStart = skipHorizSpace afterKw
            guard (argStart > afterKw && argStart < sqlLen)
            let !argEnd = scanDelimArg argStart
            guard (argEnd > argStart)
            let !newDelim = unsafeSliceSpan (ByteSpan argStart argEnd) sql
                !afterArg = skipToksBefore argEnd (i + 1)
            pure (argEnd, newDelim, afterArg)

        skipHorizSpace !p
            | p < sqlLen && (b == 32 || b == 9) = skipHorizSpace (p + 1)
            | otherwise = p
            where
                b = BSU.unsafeIndex sql p

        scanDelimArg !p
            | p < sqlLen && b /= 32 && b /= 9 && b /= 10 && b /= 13 = scanDelimArg (p + 1)
            | otherwise = p
            where
                b = BSU.unsafeIndex sql p

        skipToksBefore !bytePos !k
            | k < n && spanStart (locSpan (tokAt k)) < bytePos = skipToksBefore bytePos (k + 1)
            | otherwise = k

        finalize !st = case mErr of
            Just err ->
                let !tailIdx = ssLastTopLevelEndIdx st
                    !mFirstSubstantive = nextNonTriviaFrom tailIdx
                    !errStart = case mFirstSubstantive of
                        Just firstIdx -> spanStart (locSpan (tokAt firstIdx))
                        Nothing -> lexErrorOffset err
                    !errToks = case mFirstSubstantive of
                        Just firstIdx -> V.drop firstIdx vec
                        Nothing -> V.empty
                    !errUnit =
                        SplitUnit
                            { splitUnitSpan = ByteSpan errStart sqlLen
                            , splitUnitDelimiterSpan = Nothing
                            , splitUnitTokens = errToks
                            , splitUnitLexError = Just err
                            , splitUnitDelimiterDirective = Nothing
                            }
                 in reverse (errUnit : ssAcc st)
            Nothing -> case (ssStmtStartTok st, ssLastNonTriviaTok st) of
                (Just sIdx, Just eIdx) ->
                    let !startByte = spanStart (locSpan (tokAt sIdx))
                        !endByte = spanEnd (locSpan (tokAt eIdx))
                        !unit =
                            SplitUnit
                                { splitUnitSpan = ByteSpan startByte endByte
                                , splitUnitDelimiterSpan = Nothing
                                , splitUnitTokens = V.slice sIdx (eIdx - sIdx + 1) vec
                                , splitUnitLexError = Nothing
                                , splitUnitDelimiterDirective = Nothing
                                }
                     in reverse (unit : ssAcc st)
                _ -> reverse (ssAcc st)

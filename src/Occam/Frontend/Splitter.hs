-- | Top-level statement splitter; keeps compound routine/trigger/CASE blocks whole.
module Occam.Frontend.Splitter (
    SplitUnit (..),
    splitSQL,
    splitTokens,
    countSQLStatements,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector (Vector)
import Data.Vector qualified as V
import Occam.Frontend.Lexer (
    Keyword (..),
    LexError (..),
    LexErrorKind (..),
    LexResult (..),
    SpannedToken,
    Token (..),
    WordToken (..),
    asciiEqCI,
    isTriviaToken,
    lexSQL,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..), unsafeSliceSpan)
import Occam.Frontend.Splitter.Delimiter (
    attachedDelimiter,
    isDelimiterWord,
    matchCustomDelimiter,
    parseDelimiterDirective,
 )
import Occam.Parser.Dialect (DialectProfile (..), LexicalProfile (..))
import Occam.Parser.Types (SQLDialect (..))

isNonCompoundCreateTarget :: Keyword -> Bool
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

isNonCompoundCreateWord :: ByteString -> Bool
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

isPrevKeyword :: Keyword -> Maybe Token -> Bool
isPrevKeyword expected = \case
    Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) -> kw == expected
    _ -> False

isBeginExcludedPrev :: Maybe Token -> Bool
isBeginExcludedPrev mPrev =
    isPrevKeyword KwAs mPrev || isPrevKeyword KwOn mPrev

isCompoundStmtStart :: Maybe Token -> Bool
isCompoundStmtStart = \case
    Just TokSemicolon -> True
    Just TokColon -> True
    Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) ->
        kw `elem` [KwBegin, KwThen, KwElse, KwDo, KwLoop, KwRepeat, KwAtomic]
    _ -> False

isTransactionBegin :: (Maybe Token, Maybe Token) -> Bool
isTransactionBegin (first, second) = case first of
    Nothing -> True
    Just TokSemicolon -> True
    Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) ->
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
            || (kw == KwNot && splitIsUnquotedKeyword KwDeferrable second)
    _ -> False

splitIsUnquotedKeyword :: Keyword -> Maybe Token -> Bool
splitIsUnquotedKeyword expected = \case
    Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) -> kw == expected
    _ -> False

isDdlOrFuncIf :: (Maybe Token, Maybe Token) -> Bool
isDdlOrFuncIf (first, second) = case first of
    Just TokLParen -> True
    Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwExists}) -> True
    Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwNot}) ->
        splitIsUnquotedKeyword KwExists second
    _ -> False

isClauseFor :: Maybe Token -> Bool
isClauseFor = \case
    Just (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}) ->
        kw `elem` [KwEach, KwUpdate, KwShare, KwKey, KwNo, KwSystemTime]
    _ -> False

closesBlock :: SQLDialect -> Keyword -> BlockKind -> Bool
closesBlock dialect kw block = case kw of
    KwIf -> block == BlockIf
    KwLoop -> block == BlockLoop || (dialect == PostgreSQL && block `elem` [BlockWhile, BlockFor])
    KwWhile -> block == BlockWhile
    KwRepeat -> block == BlockRepeat
    KwFor -> block == BlockFor
    KwCase -> block == BlockCase
    _ -> False

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

data SplitState a = SplitState
    { ssParenDepth :: !Int
    , ssBracketDepth :: !Int
    , ssBraceDepth :: !Int
    , ssBlockStack :: ![BlockKind]
    , ssHeaderKind :: !HeaderKind
    , ssStmtStartTok :: !Int
    -- ^ Negative when there is no current statement.
    , ssLastNonTriviaTok :: !Int
    , ssPrevNonTrivia :: !(Maybe Token)
    , ssDelimiter :: !ByteString
    , ssLastTopLevelEndIdx :: !Int
    , ssAcc :: !a
    }

-- | Lex and split @sql@ into top-level statement units under @profile@.
splitSQL :: DialectProfile -> ByteString -> Either LexError [SplitUnit]
splitSQL profile sql = splitTokens profile sql (lexSQL profile sql)

-- | Split a pre-lexed 'LexResult' into top-level statement units.
splitTokens :: DialectProfile -> ByteString -> LexResult -> Either LexError [SplitUnit]
splitTokens profile sql (LexResult vec mErr) = case mErr of
    Just err@LexError {lexErrorKind = LexInvalidUtf8} -> Left err
    _ -> Right (reverse (runSplit profile sql vec mErr (flip (:)) (flip (:)) []))

-- | Count the same statement units as 'splitSQL', excluding client directives.
countSQLStatements :: DialectProfile -> ByteString -> Either LexError Int
countSQLStatements profile sql = case lexSQL profile sql of
    LexResult _ (Just err@LexError {lexErrorKind = LexInvalidUtf8}) -> Left err
    LexResult vec mErr ->
        Right $! runSplit profile sql vec mErr (\total _ -> total + 1) const 0

runSplit ::
    DialectProfile ->
    ByteString ->
    Vector SpannedToken ->
    Maybe LexError ->
    (a -> SplitUnit -> a) ->
    (a -> SplitUnit -> a) ->
    a ->
    a
runSplit profile sql vec mErr emitStatement emitDirective initial = go 0 initState
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
                , ssStmtStartTok = -1
                , ssLastNonTriviaTok = -1
                , ssPrevNonTrivia = Nothing
                , ssDelimiter = ";"
                , ssLastTopLevelEndIdx = 0
                , ssAcc = initial
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
            | ssStmtStartTok st < 0 = handleBetweenStatements i tok st
            | otherwise = handleInStatement False i tok (ssStmtStartTok st) st
            where
                tok = locValue (tokAt i)

        handleBetweenStatements !i !tok !st
            | ssDelimiter st == ";" && tok == TokSemicolon =
                go (i + 1) st {ssLastTopLevelEndIdx = i + 1}
            | ssDelimiter st /= ";"
            , Just (!nextIdx, _) <- matchCustomDelimiter sql sqlLen vec n i (ssDelimiter st) =
                go nextIdx st {ssLastTopLevelEndIdx = nextIdx}
            | supportsDelimiter
            , isDelimiterWord tok
            , Just (!delimEndByte, !newDelim, !afterArgIdx) <- parseDelimiterDirective sql sqlLen vec n i =
                let !startByte = spanStart (locSpan (tokAt i))
                    unit =
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
                            , ssAcc = emitDirective (ssAcc st) unit
                            }
            | otherwise =
                let !hdr = case tok of
                        TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwCreate} -> HeaderCreate
                        TokWord WordToken {wordQuote = Nothing, wordKeyword = Just KwAlter} -> HeaderAlter
                        _ -> HeaderDisallowCompound
                    !stStarted =
                        st
                            { ssHeaderKind = hdr
                            , ssStmtStartTok = i
                            , ssLastNonTriviaTok = i
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
                case matchCustomDelimiter sql sqlLen vec n i delimiter of
                    Just (nextIdx, delimSpan) -> emitUnit sIdx delimSpan Nothing nextIdx st
                    Nothing -> case attachedDelimiter tok (locSpan (tokAt i)) delimiter of
                        Just delimSpan -> emitUnit sIdx delimSpan (Just i) (i + 1) st
                        Nothing -> stepToken isFirst i tok st
            | otherwise = stepToken isFirst i tok st

        emitUnit !sIdx !delimSpan !clippedWord !nextIdx !st =
            let unit =
                    let !eIdx = max sIdx (ssLastNonTriviaTok st)
                        !startByte = spanStart (locSpan (tokAt sIdx))
                        (!endByte, !tokens) = case clippedWord of
                            Nothing -> (spanEnd (locSpan (tokAt eIdx)), V.slice sIdx (eIdx - sIdx + 1) vec)
                            Just i ->
                                let !end = spanStart delimSpan
                                    !wordStart = spanStart (locSpan (tokAt i))
                                    prefix = unsafeSliceSpan (ByteSpan wordStart end) sql
                                    offset (L (ByteSpan lo hi) tok) = L (ByteSpan (wordStart + lo) (wordStart + hi)) tok
                                 in (end, V.slice sIdx (i - sIdx) vec V.++ V.map offset (lexTokens (lexSQL profile prefix)))
                     in SplitUnit
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
                        , ssStmtStartTok = -1
                        , ssLastNonTriviaTok = -1
                        , ssPrevNonTrivia = Nothing
                        , ssLastTopLevelEndIdx = nextIdx
                        , ssAcc = emitStatement (ssAcc st) unit
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

        {-# INLINE advance #-}
        advance !nextIdx !lastTok !st =
            go
                nextIdx
                st
                    { ssLastNonTriviaTok = nextIdx - 1
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
                                (isFirst && dialect == MariaDB && not (isTransactionBegin (nextTwoNonTriviaToks (i + 1))))
                                    || ( ssHeaderKind st == HeaderAllowCompound
                                            && (not (isBeginExcludedPrev prev) || startsBodyAfterAs prev (i + 1))
                                       )
                                    || isBeginAtomic (i + 1)
                            else isCompoundStmtStart prev
                        )
            KwCase ->
                BlockCase
                    <$ guard ((isFirst || not (null stack)) && not (isPrevKeyword KwAs prev))
            KwIf -> BlockIf <$ guard (nestedStart && not (isDdlOrFuncIf (nextTwoNonTriviaToks (i + 1))))
            KwLoop -> BlockLoop <$ guard nestedStart
            KwWhile -> BlockWhile <$ guard nestedStart
            KwRepeat -> BlockRepeat <$ guard (nestedStart && nextNonTriviaTok (i + 1) /= Just TokLParen)
            KwFor -> BlockFor <$ guard (nestedStart && not (isClauseFor (nextNonTriviaTok (i + 1))))
            _ -> Nothing
            where
                stack = ssBlockStack st
                prev = ssPrevNonTrivia st
                nestedStart = not (null stack) && isCompoundStmtStart prev

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

        handleEnd !i !tok !st = case ssBlockStack st of
            top : rest
                | Just nextIdx <- nextNonTriviaFrom (i + 1)
                , TokWord WordToken {wordQuote = Nothing, wordKeyword = Just nextKw} <- locValue (tokAt nextIdx)
                , closesBlock dialect nextKw top
                , startsClosure || top `elem` [BlockCase, BlockRepeat] ->
                    advance (nextIdx + 1) (locValue (tokAt nextIdx)) st {ssBlockStack = rest}
                | top == BlockCase || (top == BlockBegin && startsClosure) ->
                    advance (i + 1) tok st {ssBlockStack = rest}
            _ -> advance (i + 1) tok st
            where
                startsClosure = isCompoundStmtStart (ssPrevNonTrivia st)

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
                    errUnit =
                        SplitUnit
                            { splitUnitSpan = ByteSpan errStart sqlLen
                            , splitUnitDelimiterSpan = Nothing
                            , splitUnitTokens = errToks
                            , splitUnitLexError = Just err
                            , splitUnitDelimiterDirective = Nothing
                            }
                 in emitStatement (ssAcc st) errUnit
            Nothing
                | let sIdx = ssStmtStartTok st
                      eIdx = ssLastNonTriviaTok st
                , sIdx >= 0
                , eIdx >= 0 ->
                    let !startByte = spanStart (locSpan (tokAt sIdx))
                        !endByte = spanEnd (locSpan (tokAt eIdx))
                        unit =
                            SplitUnit
                                { splitUnitSpan = ByteSpan startByte endByte
                                , splitUnitDelimiterSpan = Nothing
                                , splitUnitTokens = V.slice sIdx (eIdx - sIdx + 1) vec
                                , splitUnitLexError = Nothing
                                , splitUnitDelimiterDirective = Nothing
                                }
                     in emitStatement (ssAcc st) unit
                | otherwise -> ssAcc st

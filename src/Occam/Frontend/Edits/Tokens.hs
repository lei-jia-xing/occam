-- | Indexed token and parenthesis queries over a statement unit's token stream.
module Occam.Frontend.Edits.Tokens (
    TokenIndex (..),
    buildTokenIndex,

    -- * Whitespace boundary helpers
    isAsciiSpace,
    precedingSpace,
    precedingWhitespace,

    -- * Token predicates
    isSignificantToken,
    isWordCI,
    isKeywordTokenCI,
    isCommaToken,
    isLParenToken,
    isRParenToken,
    isSemicolonToken,

    -- * Indexed lookups
    partitionPoint,
    nextSignificant,
    previousSignificant,
    matchingParentheses,
    enclosingParentheses,
    findComma,
    findKeywordBetween,
    keywordBefore,
    topLevelKeyword,
    clauseEndAfter,
    followingWord,
    wordsBetween,
    keywordPairEnd,
    groupingSetsEnd,
    tableBodyItem,
    firstWordInRange,
    columnOptionsSpan,
    lastSignificantEnd,
    querySourceScope,
    selectScope,
    valuesRowSpans,
    duplicateTreatmentRange,
) where

import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isNothing)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)
import Occam.Frontend.Lexer (
    SpannedToken,
    Token (..),
    WordToken (..),
    asciiEqCI,
    isTriviaToken,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..))

-- | Precomputed token vector and matching parenthesis spans for a statement unit.
data TokenIndex = TokenIndex
    { tiTokens :: !(Vector SpannedToken)
    -- ^ Unit tokens in source order (spans shifted into 'ctxSource' coordinates).
    , tiParens :: !(Vector (Maybe ByteSpan))
    -- ^ For each '(' or ')' token index, the full @[openStart, closeEnd)@ span of its matched pair.
    }

-- | Build a 'TokenIndex' from a unit's token slice, shifting spans by @shift@.
buildTokenIndex :: Int -> Vector SpannedToken -> TokenIndex
buildTokenIndex shift rawTokens =
    TokenIndex
        { tiTokens = toks
        , tiParens = parens
        }
    where
        toks =
            if shift == 0
                then rawTokens
                else V.map (\(L (ByteSpan s e) t) -> L (ByteSpan (s + shift) (e + shift)) t) rawTokens
        !n = V.length toks
        parens = runST do
            arr <- MV.replicate n Nothing
            let go !i !stack
                    | i >= n = pure ()
                    | otherwise =
                        let L (ByteSpan s e) tok = V.unsafeIndex toks i
                         in case tok of
                                TokLParen -> go (i + 1) ((i, s) : stack)
                                TokRParen -> case stack of
                                    (openIdx, openStart) : rest -> do
                                        let !pair = Just (ByteSpan openStart e)
                                        MV.unsafeWrite arr openIdx pair
                                        MV.unsafeWrite arr i pair
                                        go (i + 1) rest
                                    [] -> go (i + 1) []
                                TokSemicolon -> go (i + 1) []
                                _ -> go (i + 1) stack
            go 0 []
            V.unsafeFreeze arr

-- ---------------------------------------------------------------------------
-- Whitespace boundary helpers
-- ---------------------------------------------------------------------------

-- | True for ASCII space, tab, carriage return, or line feed.
{-# INLINE isAsciiSpace #-}
isAsciiSpace :: Word8 -> Bool
isAsciiSpace b = b == 32 || b == 9 || b == 13 || b == 10

-- | Step back over a single preceding ASCII whitespace character (or @\\r\\n@ pair).
precedingSpace :: ByteString -> Int -> Int
precedingSpace src start
    | start >= 2 && BS.index src (start - 2) == 13 && BS.index src (start - 1) == 10 = start - 2
    | start > 0 && isAsciiSpace (BS.index src (start - 1)) = start - 1
    | otherwise = start

-- | Step back over all immediately preceding ASCII whitespace characters.
precedingWhitespace :: ByteString -> Int -> Int
precedingWhitespace src = go
    where
        go !pos
            | pos > 0 && isAsciiSpace (BS.index src (pos - 1)) = go (pos - 1)
            | otherwise = pos

-- ---------------------------------------------------------------------------
-- Token predicates
-- ---------------------------------------------------------------------------

-- | True for non-trivia tokens (matching Rust's @!TokenKind::Whitespace@).
{-# INLINE isSignificantToken #-}
isSignificantToken :: SpannedToken -> Bool
isSignificantToken (L _ tok) = not (isTriviaToken tok)

-- | True when a token is an unquoted word matching @expected@ case-insensitively.
{-# INLINE isWordCI #-}
isWordCI :: ByteString -> SpannedToken -> Bool
isWordCI expected (L _ (TokWord wt)) =
    isNothing (wordQuote wt) && asciiEqCI expected (wordRaw wt)
isWordCI _ _ = False

-- | True when a token's raw source text matches @expected@ case-insensitively (unquoted word or operator).
{-# INLINE isKeywordTokenCI #-}
isKeywordTokenCI :: ByteString -> SpannedToken -> Bool
isKeywordTokenCI = isWordCI

{-# INLINE isCommaToken #-}
isCommaToken :: SpannedToken -> Bool
isCommaToken (L _ TokComma) = True
isCommaToken _ = False

{-# INLINE isLParenToken #-}
isLParenToken :: SpannedToken -> Bool
isLParenToken (L _ TokLParen) = True
isLParenToken _ = False

{-# INLINE isRParenToken #-}
isRParenToken :: SpannedToken -> Bool
isRParenToken (L _ TokRParen) = True
isRParenToken _ = False

{-# INLINE isSemicolonToken #-}
isSemicolonToken :: SpannedToken -> Bool
isSemicolonToken (L _ TokSemicolon) = True
isSemicolonToken _ = False

-- ---------------------------------------------------------------------------
-- Binary search & scanning primitives
-- ---------------------------------------------------------------------------

{- | First index @i@ in @[0, V.length vec]@ where @predicate (vec ! i)@ is 'False'.
Requires @predicate@ to be monotone (@True@ prefix followed by @False@ suffix).
-}
partitionPoint :: (SpannedToken -> Bool) -> Vector SpannedToken -> Int
partitionPoint p vec = go 0 (V.length vec)
    where
        go !lo !hi
            | lo >= hi = lo
            | otherwise =
                let !mid = lo + ((hi - lo) `div` 2)
                 in if p (V.unsafeIndex vec mid)
                        then go (mid + 1) hi
                        else go lo mid

-- | Next non-trivia token at or after @startIdx@ whose start offset is strictly below @scopeEnd@.
nextSignificant :: TokenIndex -> Int -> Int -> Maybe (Int, SpannedToken)
nextSignificant ti startIdx scopeEnd = go startIdx
    where
        toks = tiTokens ti
        !n = V.length toks
        go !i
            | i >= n = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in if spanStart (locSpan tok) >= scopeEnd
                        then Nothing
                        else
                            if isSignificantToken tok
                                then Just (i, tok)
                                else go (i + 1)

-- | Nearest non-trivia token ending at or before @byteOffset@.
previousSignificant :: TokenIndex -> Int -> Maybe (Int, SpannedToken)
previousSignificant ti byteOffset = go (before - 1)
    where
        toks = tiTokens ti
        !before = partitionPoint (\t -> spanEnd (locSpan t) <= byteOffset) toks
        go !i
            | i < 0 = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in if isSignificantToken tok
                        then Just (i, tok)
                        else go (i - 1)

-- | Matching parenthesis pair enclosing @[start, end)@ with opening paren at or after @after@.
matchingParentheses :: TokenIndex -> Int -> Int -> Int -> Maybe ByteSpan
matchingParentheses ti start end after =
    case scanBack (before - 1) of
        Just pair -> Just pair
        Nothing
            | after < start -> Nothing
            | otherwise -> scanForward first
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        !before = partitionPoint (\t -> spanStart (locSpan t) <= start) toks
        !first = partitionPoint (\t -> spanStart (locSpan t) < after) toks

        scanBack !i
            | i < 0 = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in if spanStart (locSpan tok) < after
                        then Nothing
                        else case (locValue tok, V.unsafeIndex parens i) of
                            (TokLParen, Just pair) | spanEnd pair >= end -> Just pair
                            _ -> scanBack (i - 1)

        scanForward !i
            | i >= n = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in if spanStart (locSpan tok) > end
                        then Nothing
                        else case (locValue tok, V.unsafeIndex parens i) of
                            (TokLParen, Just pair) | spanEnd pair >= end -> Just pair
                            _ -> scanForward (i + 1)

-- | Outermost matched parenthesis pair strictly starting before @start@ and ending at or after @end@.
enclosingParentheses :: TokenIndex -> Int -> Int -> Maybe (Int, Int)
enclosingParentheses ti start end = go 0
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        go !i
            | i >= n = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                    !tokStart = spanStart (locSpan tok)
                 in if tokStart >= start
                        then Nothing
                        else case (locValue tok, V.unsafeIndex parens i) of
                            (TokLParen, Just pair)
                                | spanEnd pair >= end -> Just (tokStart, spanEnd pair)
                            _ -> go (i + 1)

-- | Locate a top-level comma token within @[start, end)@, skipping balanced parentheses.
findComma :: TokenIndex -> Int -> Int -> Maybe (Int, Int)
findComma ti start end = go initialIdx
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        !initialIdx = partitionPoint (\t -> spanStart (locSpan t) < start) toks
        go !i
            | i >= n = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                    ByteSpan s e = locSpan tok
                 in if s >= end
                        then Nothing
                        else case locValue tok of
                            TokLParen -> case V.unsafeIndex parens i of
                                Just pair
                                    | spanEnd pair <= end ->
                                        go (partitionPoint (\t -> spanEnd (locSpan t) <= spanEnd pair) toks)
                                _ -> Nothing
                            TokComma | e <= end -> Just (s, e)
                            _ -> go (i + 1)

-- | Find the first significant token matching @kw@ within @[start, end)@.
findKeywordBetween :: TokenIndex -> ByteString -> Int -> Int -> Maybe (Int, SpannedToken)
findKeywordBetween ti kw start end = go initialIdx
    where
        toks = tiTokens ti
        !initialIdx = partitionPoint (\t -> spanStart (locSpan t) < start) toks
        go !idx = case nextSignificant ti idx end of
            Nothing -> Nothing
            Just (nextIdx, tok)
                | isWordCI kw tok -> Just (nextIdx, tok)
                | otherwise -> go (nextIdx + 1)

-- | Find the last occurrence of unquoted keyword @kw@ with @scopeStart <= start <= bodyStart@.
keywordBefore :: TokenIndex -> ByteString -> Int -> Int -> Maybe SpannedToken
keywordBefore ti kw scopeStart bodyStart = go (V.length toks - 1)
    where
        toks = tiTokens ti
        go !i
            | i < 0 = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                    !s = spanStart (locSpan tok)
                 in if s >= scopeStart && s <= bodyStart && isWordCI kw tok
                        then Just tok
                        else go (i - 1)

{- | Find the last top-level occurrence of unquoted keyword @kw@ in @(scopeStart, scopeEnd)@,
skipping parenthesized groups. Returns 'Nothing' if a paren group extends beyond @scopeEnd@.
-}
topLevelKeyword :: TokenIndex -> ByteString -> (Int, Int) -> Maybe SpannedToken
topLevelKeyword ti kw (scopeStart, scopeEnd) = go initialIdx Nothing
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        !initialIdx = partitionPoint (\t -> spanStart (locSpan t) < scopeStart) toks
        go !i !found
            | i >= n = found
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in if spanStart (locSpan tok) >= scopeEnd
                        then found
                        else case locValue tok of
                            TokLParen -> case V.unsafeIndex parens i of
                                Just pair
                                    | spanEnd pair <= scopeEnd ->
                                        go (partitionPoint (\t -> spanEnd (locSpan t) <= spanEnd pair) toks) found
                                _ -> Nothing
                            _ ->
                                let !found' = if isWordCI kw tok then Just tok else found
                                 in go (i + 1) found'

{- | Locate the byte boundary immediately following a clause body, skipping balanced
parentheses and stopping at @)@, @;@, or any keyword in @followingClauses@.
-}
clauseEndAfter :: ByteString -> TokenIndex -> Int -> Int -> [ByteString] -> Maybe Int
clauseEndAfter src ti bodyEnd scopeEnd followingClauses
    | bodyEnd > scopeEnd = Nothing
    | otherwise = go initialIdx
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        !initialIdx = partitionPoint (\t -> spanStart (locSpan t) < bodyEnd) toks
        go !i
            | i >= n = Just (precedingSpace src scopeEnd)
            | otherwise =
                let tok = V.unsafeIndex toks i
                    !s = spanStart (locSpan tok)
                 in if s >= scopeEnd
                        then Just (precedingSpace src scopeEnd)
                        else case locValue tok of
                            TokLParen -> case V.unsafeIndex parens i of
                                Just pair
                                    | spanEnd pair <= scopeEnd ->
                                        go (partitionPoint (\t -> spanEnd (locSpan t) <= spanEnd pair) toks)
                                _ -> Nothing
                            TokRParen -> Just (precedingSpace src s)
                            TokSemicolon -> Just (precedingSpace src s)
                            _
                                | any (`isWordCI` tok) followingClauses ->
                                    Just (precedingSpace src s)
                                | otherwise -> go (i + 1)

-- | Match the next significant unquoted word @word@, returning @(nextIdx, tokenEnd)@.
followingWord :: TokenIndex -> Int -> Int -> ByteString -> Maybe (Int, Int)
followingWord ti idx scopeEnd expected = do
    (foundIdx, tok) <- nextSignificant ti idx scopeEnd
    if isWordCI expected tok
        then Just (foundIdx + 1, spanEnd (locSpan tok))
        else Nothing

-- | Verify that the significant tokens between @start@ and @end@ are exactly @words@ (case-insensitive).
wordsBetween :: ByteString -> TokenIndex -> Int -> Int -> [ByteString] -> Bool
wordsBetween src ti start end = go initialIdx
    where
        toks = tiTokens ti
        !initialIdx = partitionPoint (\t -> spanEnd (locSpan t) <= start) toks
        go !idx [] = case nextSignificant ti idx end of
            Nothing -> True
            Just _ -> False
        go !idx (w : ws) = case nextSignificant ti idx end of
            Nothing -> False
            Just (foundIdx, tok) ->
                let ByteSpan s e = locSpan tok
                    raw = BS.take (e - s) (BS.drop s src)
                 in asciiEqCI w raw && go (foundIdx + 1) ws

-- | End offset of two consecutive unquoted keywords @first second@ in @[after, scopeEnd)@.
keywordPairEnd :: TokenIndex -> ByteString -> ByteString -> Int -> Int -> Maybe Int
keywordPairEnd ti first second after scopeEnd = go initialIdx
    where
        toks = tiTokens ti
        !n = V.length toks
        !initialIdx = partitionPoint (\t -> spanStart (locSpan t) < after) toks
        go !i
            | i >= n = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in if spanStart (locSpan tok) >= scopeEnd
                        then Nothing
                        else
                            if isWordCI first tok
                                then case nextSignificant ti (i + 1) scopeEnd of
                                    Just (_, nextTok)
                                        | isWordCI second nextTok -> Just (spanEnd (locSpan nextTok))
                                    _ -> go (i + 1)
                                else go (i + 1)

-- | End offset of @GROUPING SETS (...)@ in @[after, scopeEnd)@.
groupingSetsEnd :: TokenIndex -> Int -> Int -> Maybe Int
groupingSetsEnd ti after scopeEnd = go initialIdx
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        !initialIdx = partitionPoint (\t -> spanStart (locSpan t) < after) toks
        go !i
            | i >= n = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in if spanStart (locSpan tok) >= scopeEnd
                        then Nothing
                        else
                            if isWordCI "GROUPING" tok
                                then case nextSignificant ti (i + 1) scopeEnd of
                                    Just (setsIdx, setsTok)
                                        | isWordCI "SETS" setsTok ->
                                            case nextSignificant ti (setsIdx + 1) scopeEnd of
                                                Just (openIdx, L _ TokLParen) ->
                                                    case V.unsafeIndex parens openIdx of
                                                        Just pair | spanEnd pair <= scopeEnd -> Just (spanEnd pair)
                                                        _ -> Nothing
                                                _ -> go (i + 1)
                                    _ -> go (i + 1)
                                else go (i + 1)

-- | Locate the top-level comma-separated item span in @[bodyStart, bodyEnd)@ containing @target@.
tableBodyItem :: TokenIndex -> Int -> Int -> Int -> Maybe (Int, Int)
tableBodyItem ti bodyStart bodyEnd target = go initialIdx bodyStart
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        !initialIdx = partitionPoint (\t -> spanEnd (locSpan t) <= bodyStart) toks
        go !i !itemStart
            | i >= n = Just (itemStart, bodyEnd)
            | otherwise =
                let tok = V.unsafeIndex toks i
                    ByteSpan s e = locSpan tok
                 in if s >= bodyEnd
                        then Just (itemStart, bodyEnd)
                        else case locValue tok of
                            TokLParen -> case V.unsafeIndex parens i of
                                Just pair
                                    | spanEnd pair <= bodyEnd ->
                                        go (partitionPoint (\t -> spanStart (locSpan t) < spanEnd pair) toks) itemStart
                                _ -> Nothing
                            TokComma
                                | s < target -> go (i + 1) e
                                | otherwise -> Just (itemStart, s)
                            _ -> go (i + 1) itemStart

-- | First word token completely inside @[start, end)@.
firstWordInRange :: TokenIndex -> Int -> Int -> Maybe ByteSpan
firstWordInRange ti start end = go initialIdx
    where
        toks = tiTokens ti
        !n = V.length toks
        !initialIdx = partitionPoint (\t -> spanStart (locSpan t) < start) toks
        go !i
            | i >= n = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                    sp@(ByteSpan s e) = locSpan tok
                 in if s >= end
                        then Nothing
                        else case locValue tok of
                            TokWord _ | s >= start && e <= end -> Just sp
                            _ -> go (i + 1)

{- | Span covering all column options/constraints after @typeEnd@ up to the next
top-level @,@, @)@, or @;@ (matching Rust @column_options_span@).
-}
columnOptionsSpan :: TokenIndex -> Int -> Maybe (Int, Int)
columnOptionsSpan ti typeEnd =
    let !colEnd = go initialIdx typeEnd
     in if colEnd > typeEnd then Just (typeEnd, colEnd) else Nothing
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        !initialIdx = partitionPoint (\t -> spanStart (locSpan t) < typeEnd) toks
        go !i !colEnd
            | i >= n = colEnd
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in case locValue tok of
                        TokComma -> colEnd
                        TokRParen -> colEnd
                        TokSemicolon -> colEnd
                        TokLParen -> case V.unsafeIndex parens i of
                            Just pair ->
                                go (partitionPoint (\t -> spanEnd (locSpan t) <= spanEnd pair) toks) (spanEnd pair)
                            Nothing -> go (i + 1) (spanEnd (locSpan tok))
                        _ -> go (i + 1) (spanEnd (locSpan tok))

-- | End byte offset of the last non-trivia, non-semicolon token in the unit, or @fallback@.
lastSignificantEnd :: TokenIndex -> Int -> Int
lastSignificantEnd ti fallback = go (V.length toks - 1)
    where
        toks = tiTokens ti
        go !i
            | i < 0 = fallback
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in case locValue tok of
                        TokTrivia _ -> go (i - 1)
                        TokSemicolon -> go (i - 1)
                        _ -> spanEnd (locSpan tok)

{- | Compute the source scope @(scopeStart, scopeEnd)@ for a 'Query' span,
matching Rust @query_source_scope@.
-}
querySourceScope :: Int -> Int -> TokenIndex -> (Int, Int) -> Maybe (Int, Int)
querySourceScope unitStart unitEnd ti (qStart, qEnd)
    | qStart < unitStart || qEnd > unitEnd = Nothing
    | otherwise = case scanEnclosing (before - 1) of
        Just inner -> Just inner
        Nothing -> scanForward before
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        !before = partitionPoint (\t -> spanStart (locSpan t) < qStart) toks

        scanEnclosing !i
            | i < 0 = Nothing
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in case (locValue tok, V.unsafeIndex parens i) of
                        (TokLParen, Just pair)
                            | spanStart pair >= unitStart && spanEnd pair <= unitEnd && spanEnd pair >= qEnd ->
                                Just (spanStart pair + 1, spanEnd pair - 1)
                        _ -> scanEnclosing (i - 1)

        scanForward !i
            | i >= n = Just (qStart, unitEnd)
            | otherwise =
                let tok = V.unsafeIndex toks i
                 in case locValue tok of
                        TokSemicolon -> Just (qStart, spanStart (locSpan tok))
                        TokRParen -> Just (qStart, spanStart (locSpan tok))
                        TokLParen -> case V.unsafeIndex parens i of
                            Just pair ->
                                scanForward (partitionPoint (\t -> spanEnd (locSpan t) <= spanEnd pair) toks)
                            Nothing -> Nothing
                        _ -> scanForward (i + 1)

-- | Compute the source scope @(scopeStart, scopeEnd)@ for a 'SelectCore', matching Rust @select_scope@.
selectScope :: Int -> TokenIndex -> Int -> Maybe (Int, Int)
selectScope unitEnd ti selectStart = scanForward initialIdx
    where
        toks = tiTokens ti
        parens = tiParens ti
        !n = V.length toks
        !end0 = lastSignificantEnd ti unitEnd
        !initialIdx = partitionPoint (\t -> spanStart (locSpan t) < selectStart) toks

        scanForward !i
            | i >= n = Just (selectStart, end0)
            | otherwise =
                let tok = V.unsafeIndex toks i
                    !s = spanStart (locSpan tok)
                 in if s >= end0
                        then Just (selectStart, end0)
                        else case locValue tok of
                            TokLParen -> case V.unsafeIndex parens i of
                                Just pair ->
                                    scanForward (partitionPoint (\t -> spanEnd (locSpan t) <= spanEnd pair) toks)
                                Nothing -> scanForward (i + 1)
                            TokRParen -> Just (selectStart, s)
                            TokSemicolon -> Just (selectStart, s)
                            _
                                | isWordCI "UNION" tok || isWordCI "EXCEPT" tok || isWordCI "INTERSECT" tok ->
                                    Just (selectStart, s)
                                | otherwise -> scanForward (i + 1)

{- | Locate exact @(start, end)@ spans for every row in a @VALUES (...)@ list,
handling optional MySQL @ROW(...)@ prefixes and empty rows.
-}
valuesRowSpans :: TokenIndex -> Int -> Int -> Int -> Maybe [(Int, Int)]
valuesRowSpans ti valuesEnd scopeEnd rowCount
    | rowCount <= 0 = Just []
    | otherwise = go (partitionPoint (\t -> spanStart (locSpan t) < valuesEnd) toks) rowCount []
    where
        toks = tiTokens ti
        parens = tiParens ti

        go _ 0 acc = Just (reverse acc)
        go !idx !remaining acc = do
            (tokIdx, tok) <- nextSignificant ti idx scopeEnd
            (rowStart, pair) <- case locValue tok of
                TokLParen -> do
                    p <- V.unsafeIndex parens tokIdx
                    Just (spanStart p, p)
                _
                    | isWordCI "ROW" tok -> do
                        (parenIdx, pTok) <- nextSignificant ti (tokIdx + 1) scopeEnd
                        case locValue pTok of
                            TokLParen -> do
                                p <- V.unsafeIndex parens parenIdx
                                Just (spanStart (locSpan tok), p)
                            _ -> Nothing
                    | otherwise -> Nothing
            let !pairEnd = spanEnd pair
            if pairEnd > scopeEnd
                then Nothing
                else
                    let !afterPairIdx = partitionPoint (\t -> spanEnd (locSpan t) <= pairEnd) toks
                     in if remaining == 1
                            then Just (reverse ((rowStart, pairEnd) : acc))
                            else do
                                (commaIdx, commaTok) <- nextSignificant ti afterPairIdx scopeEnd
                                case locValue commaTok of
                                    TokComma -> go (commaIdx + 1) (remaining - 1) ((rowStart, pairEnd) : acc)
                                    _ -> Nothing

-- | Span of a @DISTINCT@ or @ALL@ duplicate-treatment keyword immediately inside @func(...)@.
duplicateTreatmentRange :: TokenIndex -> Int -> Int -> Maybe (Int, Int)
duplicateTreatmentRange ti nameEnd unitEnd = do
    let !startIdx = partitionPoint (\t -> spanStart (locSpan t) < nameEnd) toks
    (parenIdx, L _ TokLParen) <- nextSignificant ti startIdx unitEnd
    (kwIdx, kwTok) <- nextSignificant ti (parenIdx + 1) unitEnd
    if isWordCI "DISTINCT" kwTok || isWordCI "ALL" kwTok
        then do
            (_, nextTok) <- nextSignificant ti (kwIdx + 1) unitEnd
            Just (spanStart (locSpan kwTok), spanStart (locSpan nextTok))
        else Nothing
    where
        toks = tiTokens ti

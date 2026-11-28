-- | Byte-oriented SQL scanning implementation.
module Occam.Frontend.Lexer.Scan (
    lexSQL,
    lexSQLStrict,
) where

import Control.Monad.ST (ST, runST)
import Data.Bits (unsafeShiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (chr, isAlpha, isAlphaNum)
import Data.HashMap.Strict (HashMap)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)
import Occam.Frontend.Lexer.Keyword (Keyword (..), keywordTableFor, lookupKeyword)
import Occam.Frontend.Lexer.Token (
    LexError (..),
    LexErrorKind (..),
    LexResult (..),
    LiteralKind (..),
    QuoteKind (..),
    SpannedToken,
    Token (..),
    TriviaKind (..),
    WordToken (..),
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..), unsafeSliceSpan, validateUtf8)
import Occam.Parser.Dialect (DialectProfile (..), LexicalProfile (..))
import Occam.Parser.Types (SQLDialect (..))

-- | Strict lexer returning 'Left' on any lexical error (including invalid UTF-8).
lexSQLStrict :: DialectProfile -> ByteString -> Either LexError (Vector SpannedToken)
lexSQLStrict profile bs =
    let res = lexSQL profile bs
     in case lexError res of
            Just err -> Left err
            Nothing -> Right (lexTokens res)

{- | Single-pass UTF-8 'ByteString' lexer driven by 'DialectProfile'.
On invalid UTF-8, returns empty tokens and 'LexInvalidUtf8' at the offending start byte.
On an unterminated literal/comment/identifier, returns all valid preceding tokens
alongside 'Just LexError' pointing to the opening byte offset of the unterminated construct.
-}
lexSQL :: DialectProfile -> ByteString -> LexResult
lexSQL profile bs = case validateUtf8 bs of
    Left badOffset ->
        LexResult
            V.empty
            (Just (LexError LexInvalidUtf8 badOffset "SQL input is not valid UTF-8"))
    Right () -> runST (scanAll profile kwTable bs)
    where
        !kwTable = keywordTableFor profile

scanAll :: forall s. DialectProfile -> HashMap ByteString Keyword -> ByteString -> ST s LexResult
scanAll profile kwTable bs = do
    let !len = BS.length bs
        !lp = dialectLexical profile
        !initCap = max 16 (min 16384 (len `div` 3))
    bufRef <- MV.new initCap >>= newSTRef
    sizeRef <- newSTRef (0 :: Int)

    let pushTok !tok = do
            buf <- readSTRef bufRef
            idx <- readSTRef sizeRef
            let !cap = MV.length buf
            buf' <-
                if idx < cap
                    then pure buf
                    else do
                        grown <- MV.grow buf cap
                        writeSTRef bufRef grown
                        pure grown
            MV.write buf' idx tok
            writeSTRef sizeRef (idx + 1)

        finishWith err = do
            buf <- readSTRef bufRef
            sz <- readSTRef sizeRef
            vec <- V.unsafeFreeze (MV.slice 0 sz buf)
            pure (LexResult vec err)

        previousToken = do
            sz <- readSTRef sizeRef
            if sz == 0
                then pure (TokTrivia TriviaWhitespace)
                else do
                    buf <- readSTRef bufRef
                    locValue <$> MV.unsafeRead buf (sz - 1)

        {-# INLINE emit #-}
        emit !s !e !tok = pushTok (L (ByteSpan s e) tok)

        {-# INLINE emitSpan #-}
        emitSpan !s !e !tok = emit s e tok *> loop e

        {-# INLINE emitOp #-}
        emitOp !s !k = emitSpan s (s + k) (TokOperator (slice s (s + k)))

        failLex kind off msg = finishWith (Just (LexError kind off msg))

        scanEmit ::
            Int ->
            LexErrorKind ->
            Text ->
            Maybe Int ->
            (Int -> Token) ->
            ST s LexResult
        scanEmit startOff errKind errMsg mEnd mkTok = case mEnd of
            Just endPos -> emitSpan startOff endPos (mkTok endPos)
            Nothing -> failLex errKind startOff errMsg

        {-# INLINE slice #-}
        slice !s !e = unsafeSliceSpan (ByteSpan s e) bs

        {-# INLINE at #-}
        at !idx = BSU.unsafeIndex bs idx

        {-# INLINE hasByteAt #-}
        hasByteAt !idx !b = idx < len && at idx == b

        {-# INLINE byteAt #-}
        byteAt !idx
            | idx < len = Just (at idx)
            | otherwise = Nothing

        loop :: Int -> ST s LexResult
        loop !i
            | i >= len = finishWith Nothing
            | isAsciiWhitespace b0 =
                let !endPos = scanWhile isAsciiWhitespace (i + 1)
                 in emitSpan i endPos (TokTrivia TriviaWhitespace)
            -- Line comments: --
            | b0 == 45 && hasByteAt (i + 1) 45 =
                emitSpan i (scanLineComment (i + 2)) (TokTrivia TriviaLineComment)
            -- Hash line comments: # (MySQL / MariaDB)
            | b0 == 35 && lexHashComments lp =
                emitSpan i (scanLineComment (i + 1)) (TokTrivia TriviaLineComment)
            -- Conditional & block comments: /*! ... */ and /* ... */
            | b0 == 47 && hasByteAt (i + 1) 42 =
                if lexConditionalComments lp && hasByteAt (i + 2) 33
                    then
                        scanEmit
                            i
                            LexUnterminatedComment
                            "unterminated conditional block comment"
                            (scanSimpleBlockComment (i + 3))
                            (const (TokTrivia TriviaConditionalComment))
                    else
                        let mEnd =
                                if lexNestedBlockComments lp
                                    then scanNestedBlockComment (1 :: Int) (i + 2)
                                    else scanSimpleBlockComment (i + 2)
                         in scanEmit
                                i
                                LexUnterminatedComment
                                "unterminated block comment"
                                mEnd
                                (const (TokTrivia TriviaBlockComment))
            -- Prefix strings: E'...', X'...', B'...', N'...'
            | (b0 == 69 || b0 == 101) && lexEscapeStrings lp && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated escape string literal"
                    (scanSingleQuoted True (i + 2))
                    (TokLiteral LitEscapeString . slice i)
            | (b0 == 88 || b0 == 120) && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated hex string literal"
                    (scanSingleQuoted False (i + 2))
                    (TokLiteral LitHexString . slice i)
            | (b0 == 66 || b0 == 98) && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated bit string literal"
                    (scanSingleQuoted False (i + 2))
                    (TokLiteral LitBitString . slice i)
            | (b0 == 78 || b0 == 110) && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated national string literal"
                    (scanSingleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 2))
                    (TokLiteral LitNationalString . slice i)
            -- Single-quoted string literal
            | b0 == 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated single-quoted string literal"
                    (scanSingleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 1))
                    (TokLiteral LitString . slice i)
            -- Double-quoted identifier
            | b0 == 34 =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated double-quoted identifier"
                    (scanDoubleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 1))
                    (\e -> TokWord (WordToken (slice i e) (Just QuoteDouble) Nothing))
            -- Backtick-quoted identifier (when enabled)
            | b0 == 96 && lexBacktickQuotes lp =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated backtick-quoted identifier"
                    (scanBacktickQuoted (i + 1))
                    (\e -> TokWord (WordToken (slice i e) (Just QuoteBacktick) Nothing))
            -- Bracket-quoted identifier (SQLite)
            | b0 == 91 && lexBracketQuotes lp =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated bracket-quoted identifier"
                    (scanBracketQuoted (i + 1))
                    (\e -> TokWord (WordToken (slice i e) (Just QuoteBracket) Nothing))
            -- Dollar-quoting $$tag$$ or $1 / $name parameters
            | b0 == 36 = scanDollar i
            -- User / system variables (@user / @@sys) or SQLite @param or operators
            | b0 == 64 = scanAt i
            -- Numbers (starting with digit or '.' followed by digit)
            | isAsciiDigit b0 = do
                fieldIndex <-
                    if i > 0 && hasByteAt (i - 1) 46
                        then (== TokDot) <$> previousToken
                        else pure False
                let !endPos = if fieldIndex then scanWhile isAsciiDigit (i + 1) else scanNumber i
                emitSpan i endPos (TokLiteral LitNumber (slice i endPos))
            | b0 == 46
            , Just b1 <- byteAt (i + 1)
            , isAsciiDigit b1 = do
                prev <- previousToken
                if blocksLeadingDot prev
                    then emitSpan i (i + 1) TokDot
                    else
                        let !endPos = scanNumberFraction (i + 1)
                         in emitSpan i endPos (TokLiteral LitNumber (slice i endPos))
            -- Unquoted identifier or keyword (ASCII start)
            | isAsciiIdentStart b0 =
                let !endPos = scanWordCont (i + 1)
                    !raw = slice i endPos
                    !mKw = lookupKeyword kwTable raw
                 in emitSpan i endPos (TokWord (WordToken raw Nothing mKw))
            -- Unquoted Unicode identifier (non-ASCII alphabetic lead byte)
            | b0 >= 0x80 && isUtf8IdentStart i =
                let !charLen = utf8CharLen b0
                    !endPos = scanWordCont (i + charLen)
                    !raw = slice i endPos
                 in emitSpan i endPos (TokWord (WordToken raw Nothing Nothing))
            -- Operators and punctuation
            | otherwise = scanPunctuation i b0
            where
                b0 = at i

        scanWhile p !idx
            | idx < len && p (at idx) = scanWhile p (idx + 1)
            | otherwise = idx

        scanLineComment !idx
            | idx >= len = len
            | at idx == 10 = idx
            | otherwise = scanLineComment (idx + 1)

        scanSimpleBlockComment !idx =
            let !rest = BSU.unsafeDrop idx bs
                !(before, match) = BS.breakSubstring "*/" rest
             in if BS.null match
                    then Nothing
                    else Just (idx + BS.length before + 2)

        scanNestedBlockComment !depth !idx
            | idx >= len = Nothing
            | at idx == 47 && hasByteAt (idx + 1) 42 =
                scanNestedBlockComment (depth + 1) (idx + 2)
            | at idx == 42 && hasByteAt (idx + 1) 47 =
                if depth == 1
                    then Just (idx + 2)
                    else scanNestedBlockComment (depth - 1) (idx + 2)
            | otherwise = scanNestedBlockComment depth (idx + 1)

        scanDelimited !closeByte !allowDoubled !allowBackslash = go
            where
                go !idx
                    | idx >= len = Nothing
                    | b == closeByte =
                        if allowDoubled && hasByteAt (idx + 1) closeByte
                            then go (idx + 2)
                            else Just (idx + 1)
                    | allowBackslash && b == 92 =
                        if idx + 1 >= len
                            then Nothing
                            else go (idx + 1 + utf8CharLen (at (idx + 1)))
                    | otherwise = go (idx + utf8CharLen b)
                    where
                        b = at idx

        scanSingleQuoted !allowBackslash = scanDelimited 39 True allowBackslash
        scanDoubleQuoted !allowBackslash = scanDelimited 34 True allowBackslash
        scanBacktickQuoted = scanDelimited 96 True False
        scanBracketQuoted = scanDelimited 93 False False

        scanDollar !i
            | lexDollarQuoting lp && hasByteAt (i + 1) 36 =
                scanDollarBody i (i + 2) (slice i (i + 2))
            | lexDollarQuoting lp
            , Just b1 <- byteAt (i + 1)
            , isAsciiDigit b1 =
                let !endPos = scanWhile isAsciiDigit (i + 1)
                 in if lexDollarParameters lp
                        then emitSpan i endPos (TokParam (slice i endPos))
                        else emitOp i 1
            | lexDollarQuoting lp && i + 1 < len && isDollarTagStart (i + 1) =
                let !tagStartLen = utf8CharLen (at (i + 1))
                    !tagEnd = scanDollarTagCont (i + 1 + tagStartLen)
                 in if hasByteAt tagEnd 36
                        then scanDollarBody i (tagEnd + 1) (slice i (tagEnd + 1))
                        else
                            if lexDollarParameters lp && (lexNamedParameters lp || dialectId profile == DuckDB)
                                then emitSpan i tagEnd (TokParam (slice i tagEnd))
                                else emitOp i 1
            | lexDollarParameters lp
            , Just b1 <- byteAt (i + 1)
            , isParamIdentCont b1 =
                let !endPos = scanWhile isParamIdentCont (i + 1)
                 in emitSpan i endPos (TokParam (slice i endPos))
            | otherwise = emitOp i 1

        scanDollarBody !openOffset !bodyStart !delim =
            let !rest = BSU.unsafeDrop bodyStart bs
                !(before, match) = BS.breakSubstring delim rest
             in if BS.null match
                    then failLex LexUnterminatedDollarString openOffset "unterminated dollar-quoted string literal"
                    else
                        let !endPos = bodyStart + BS.length before + BS.length delim
                         in emitSpan openOffset endPos (TokLiteral LitDollarString (slice openOffset endPos))

        isDollarTagStart !idx =
            let !b = at idx
             in isAsciiIdentStart b || (b >= 0x80 && isUtf8IdentStart idx)

        scanDollarTagCont !idx
            | idx >= len = idx
            | isAsciiIdentContNoDollar (at idx) = scanDollarTagCont (idx + 1)
            | at idx >= 0x80 && isUtf8IdentCont idx = scanDollarTagCont (idx + utf8CharLen (at idx))
            | otherwise = idx

        scanAt !i
            | lexUserVariables lp && hasByteAt (i + 1) 64 =
                let !nameStart = i + 2
                 in case scanSysVar nameStart of
                        Just endPos
                            | endPos == nameStart -> emitOp i 2
                            | otherwise -> emitSpan i endPos (TokSysVar (slice i endPos))
                        Nothing -> failLex LexUnterminatedIdent i "unterminated quoted system variable"
            | lexUserVariables lp && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated quoted user variable"
                    (scanSingleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 2))
                    (TokUserVar . slice i)
            | lexUserVariables lp && hasByteAt (i + 1) 34 =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated quoted user variable"
                    (scanDoubleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 2))
                    (TokUserVar . slice i)
            | lexUserVariables lp && hasByteAt (i + 1) 96 =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated quoted user variable"
                    (scanBacktickQuoted (i + 2))
                    (TokUserVar . slice i)
            | lexUserVariables lp
            , Just b1 <- byteAt (i + 1)
            , isUserVarStart b1 =
                let !endPos = scanWhile isUserVarCont (i + 2)
                 in emitSpan i endPos (TokUserVar (slice i endPos))
            | lexUserVariables lp = emitOp i 1
            | lexNamedParameters lp
            , Just b1 <- byteAt (i + 1)
            , isAsciiIdentStart b1 =
                let !endPos = scanWhile isParamIdentCont (i + 1)
                 in emitSpan i endPos (TokParam (slice i endPos))
            | hasByteAt (i + 1) 45 && hasByteAt (i + 2) 64 = emitOp i 3
            | hasByteAt (i + 1) 64 || hasByteAt (i + 1) 62 = emitOp i 2
            | otherwise = emitOp i 1

        scanSysVar !idx
            | lexBacktickQuotes lp && hasByteAt idx 96 = do
                end <- scanBacktickQuoted (idx + 1)
                if hasByteAt end 46
                    then scanSysVar (end + 1)
                    else Just end
            | otherwise =
                let !end = scanWhile isSysVarCont idx
                 in if end > idx && hasByteAt (end - 1) 46 && lexBacktickQuotes lp && hasByteAt end 96
                        then scanSysVar end
                        else Just end

        scanNumber !i
            | at i == 48
            , hasByteAt (i + 1) 120 || hasByteAt (i + 1) 88
            , Just b2 <- byteAt (i + 2)
            , isAsciiHexDigit b2 =
                scanWhile isAsciiHexDigit (i + 2)
            | at i == 48
            , hasByteAt (i + 1) 98 || hasByteAt (i + 1) 66
            , Just b2 <- byteAt (i + 2)
            , isAsciiBinDigit b2 =
                scanWhile isAsciiBinDigit (i + 2)
            | otherwise =
                let !intEnd = scanWhile isAsciiDigit (i + 1)
                 in if hasByteAt intEnd 46 && not (hasByteAt (intEnd + 1) 46)
                        then scanNumberExponent (scanWhile isAsciiDigit (intEnd + 1))
                        else scanNumberExponent intEnd

        scanNumberFraction !digitStart =
            scanNumberExponent (scanWhile isAsciiDigit digitStart)

        scanNumberExponent !idx
            | hasByteAt idx 101 || hasByteAt idx 69 =
                let !afterExp =
                        if hasByteAt (idx + 1) 43 || hasByteAt (idx + 1) 45
                            then idx + 2
                            else idx + 1
                 in case byteAt afterExp of
                        Just b | isAsciiDigit b -> scanWhile isAsciiDigit afterExp
                        _ -> idx
            | otherwise = idx

        scanWordCont !idx
            | idx >= len = idx
            | isAsciiIdentCont (at idx) = scanWordCont (idx + 1)
            | at idx >= 0x80 && isUtf8IdentCont idx = scanWordCont (idx + utf8CharLen (at idx))
            | otherwise = idx

        isUtf8IdentStart !idx =
            let !(ch, _) = decodeUtf8Char bs idx
             in isAlpha ch

        isUtf8IdentCont !idx =
            let !(ch, _) = decodeUtf8Char bs idx
             in isAlphaNum ch || ch == '_'

        scanPunctuation !i !b0 = case b0 of
            40 -> emitSpan i (i + 1) TokLParen
            41 -> emitSpan i (i + 1) TokRParen
            91 -> emitSpan i (i + 1) TokLBracket
            93 -> emitSpan i (i + 1) TokRBracket
            123 -> emitSpan i (i + 1) TokLBrace
            125 -> emitSpan i (i + 1) TokRBrace
            44 -> emitSpan i (i + 1) TokComma
            59 -> emitSpan i (i + 1) TokSemicolon
            43 -> emitSpan i (i + 1) TokPlus
            47 -> emitSpan i (i + 1) TokSlash
            46
                | hasByteAt (i + 1) 58 -> emitSpan i (i + 2) TokDotColon
                | hasByteAt (i + 1) 46 -> emitOp i 2
                | otherwise -> emitSpan i (i + 1) TokDot
            58
                | hasByteAt (i + 1) 58 -> emitSpan i (i + 2) TokDoubleColon
                | hasByteAt (i + 1) 61 -> emitSpan i (i + 2) TokColonEq
                | lexNamedParameters lp
                , Just b1 <- byteAt (i + 1)
                , isAsciiIdentStart b1 ->
                    let !endPos = scanWhile isParamIdentCont (i + 1)
                     in emitSpan i endPos (TokParam (slice i endPos))
                | otherwise -> emitSpan i (i + 1) TokColon
            45
                | hasByteAt (i + 1) 62 && hasByteAt (i + 2) 62 -> emitSpan i (i + 3) TokDoubleArrow
                | hasByteAt (i + 1) 62 -> emitSpan i (i + 2) TokArrow
                | otherwise -> emitSpan i (i + 1) TokMinus
            42
                | hasByteAt (i + 1) 42 -> emitOp i 2
                | otherwise -> emitSpan i (i + 1) TokStar
            61
                | hasByteAt (i + 1) 61 || hasByteAt (i + 1) 62 -> emitOp i 2
                | otherwise -> emitSpan i (i + 1) TokEq
            60
                -- <=>, <<=, <<, <=, <>, <@, <->, <
                | (hasByteAt (i + 1) 61 && hasByteAt (i + 2) 62)
                    || (hasByteAt (i + 1) 60 && hasByteAt (i + 2) 61)
                    || (hasByteAt (i + 1) 45 && hasByteAt (i + 2) 62) ->
                    emitOp i 3
                | hasByteAt (i + 1) 61
                    || hasByteAt (i + 1) 62
                    || hasByteAt (i + 1) 60
                    || hasByteAt (i + 1) 64 ->
                    emitOp i 2
                | otherwise -> emitOp i 1
            62
                -- >>=, >>, >=, >
                | hasByteAt (i + 1) 62 && hasByteAt (i + 2) 61 -> emitOp i 3
                | hasByteAt (i + 1) 61 || hasByteAt (i + 1) 62 -> emitOp i 2
                | otherwise -> emitOp i 1
            33
                -- !~~*, !~*, !=, !~, !~~, !!, !
                | hasByteAt (i + 1) 126 && hasByteAt (i + 2) 126 && hasByteAt (i + 3) 42 -> emitOp i 4
                | (hasByteAt (i + 1) 126 && hasByteAt (i + 2) 42)
                    || (hasByteAt (i + 1) 126 && hasByteAt (i + 2) 126) ->
                    emitOp i 3
                | hasByteAt (i + 1) 61 || hasByteAt (i + 1) 126 || hasByteAt (i + 1) 33 -> emitOp i 2
                | otherwise -> emitOp i 1
            126
                -- ~~*, ~*, ~~, ~
                | hasByteAt (i + 1) 126 && hasByteAt (i + 2) 42 -> emitOp i 3
                | hasByteAt (i + 1) 42 || hasByteAt (i + 1) 126 -> emitOp i 2
                | otherwise -> emitOp i 1
            124
                -- \||/, ||, |/, |
                | hasByteAt (i + 1) 124 && hasByteAt (i + 2) 47 -> emitOp i 3
                | hasByteAt (i + 1) 124 || hasByteAt (i + 1) 47 -> emitOp i 2
                | otherwise -> emitOp i 1
            38
                | hasByteAt (i + 1) 38 -> emitOp i 2
                | otherwise -> emitOp i 1
            35
                -- #>>, #>, #-, # (only reachable when lexHashComments is False)
                | hasByteAt (i + 1) 62 && hasByteAt (i + 2) 62 -> emitOp i 3
                | hasByteAt (i + 1) 62 || hasByteAt (i + 1) 45 -> emitOp i 2
                | otherwise -> emitOp i 1
            63
                -- ?|, ?& (PostgreSQL JSONB) or ?123 (SQLite param) or ?
                | lexQuestionOperators lp && (hasByteAt (i + 1) 124 || hasByteAt (i + 1) 38) ->
                    emitOp i 2
                | lexNumberedQuestionParameters lp
                , Just b1 <- byteAt (i + 1)
                , isAsciiDigit b1 ->
                    let !endPos = scanWhile isAsciiDigit (i + 1)
                     in emitSpan i endPos (TokParam (slice i endPos))
                | otherwise -> emitSpan i (i + 1) TokQuestion
            37 -> emitOp i 1
            94 -> emitOp i 1
            _ ->
                let !endPos = min len (i + utf8CharLen b0)
                 in emitSpan i endPos (TokUnknown (slice i endPos))

        blocksLeadingDot = \case
            TokWord WordToken {wordKeyword = Just kw} ->
                kw
                    `notElem` [ KwSelect
                              , KwDefault
                              , KwDistinct
                              , KwAll
                              , KwWhere
                              , KwPrewhere
                              , KwHaving
                              , KwQualify
                              , KwWhen
                              , KwThen
                              , KwElse
                              , KwBy
                              , KwLimit
                              , KwOffset
                              , KwReturning
                              , KwOn
                              , KwAnd
                              , KwOr
                              , KwXor
                              , KwNot
                              , KwBetween
                              , KwLike
                              ]
            TokWord _ -> True
            TokLiteral LitNumber _ -> True
            TokRParen -> True
            TokRBracket -> True
            TokRBrace -> True
            TokDot -> True
            _ -> False
    loop 0

{-# INLINE isAsciiWhitespace #-}
isAsciiWhitespace :: Word8 -> Bool
isAsciiWhitespace b = b == 32 || b == 9 || b == 10 || b == 13 || b == 12

{-# INLINE isAsciiDigit #-}
isAsciiDigit :: Word8 -> Bool
isAsciiDigit b = b >= 48 && b <= 57

{-# INLINE isAsciiHexDigit #-}
isAsciiHexDigit :: Word8 -> Bool
isAsciiHexDigit b =
    (b >= 48 && b <= 57) || (b >= 65 && b <= 70) || (b >= 97 && b <= 102)

{-# INLINE isAsciiBinDigit #-}
isAsciiBinDigit :: Word8 -> Bool
isAsciiBinDigit b = b == 48 || b == 49

{-# INLINE isAsciiAlpha #-}
isAsciiAlpha :: Word8 -> Bool
isAsciiAlpha b = (b >= 65 && b <= 90) || (b >= 97 && b <= 122)

{-# INLINE isAsciiIdentStart #-}
isAsciiIdentStart :: Word8 -> Bool
isAsciiIdentStart b = isAsciiAlpha b || b == 95

{-# INLINE isAsciiIdentContNoDollar #-}
isAsciiIdentContNoDollar :: Word8 -> Bool
isAsciiIdentContNoDollar b = isAsciiIdentStart b || isAsciiDigit b

{-# INLINE isAsciiIdentCont #-}
isAsciiIdentCont :: Word8 -> Bool
isAsciiIdentCont b = isAsciiIdentContNoDollar b || b == 36

{-# INLINE isParamIdentCont #-}
isParamIdentCont :: Word8 -> Bool
isParamIdentCont = isAsciiIdentContNoDollar

{-# INLINE isUserVarStart #-}
isUserVarStart :: Word8 -> Bool
isUserVarStart b = isAsciiAlpha b || isAsciiDigit b || b == 95 || b == 36

{-# INLINE isUserVarCont #-}
isUserVarCont :: Word8 -> Bool
isUserVarCont b = isUserVarStart b || b == 46

{-# INLINE isSysVarCont #-}
isSysVarCont :: Word8 -> Bool
isSysVarCont b = isAsciiAlpha b || isAsciiDigit b || b == 95 || b == 46

{-# INLINE utf8CharLen #-}
utf8CharLen :: Word8 -> Int
utf8CharLen b
    | b < 0x80 = 1
    | b < 0xe0 = 2
    | b < 0xf0 = 3
    | otherwise = 4

-- | Decode a single UTF-8 codepoint at a validated UTF-8 boundary.
{-# INLINE decodeUtf8Char #-}
decodeUtf8Char :: ByteString -> Int -> (Char, Int)
decodeUtf8Char bs idx
    | b0 < 0x80 = (chr (fromIntegral b0), 1)
    | b0 < 0xe0 =
        let !cp = (fromIntegral (b0 .&. 0x1f) `unsafeShiftL` 6) .|. cont 1
         in (chr cp, 2)
    | b0 < 0xf0 =
        let !cp =
                (fromIntegral (b0 .&. 0x0f) `unsafeShiftL` 12)
                    .|. (cont 1 `unsafeShiftL` 6)
                    .|. cont 2
         in (chr cp, 3)
    | otherwise =
        let !cp =
                (fromIntegral (b0 .&. 0x07) `unsafeShiftL` 18)
                    .|. (cont 1 `unsafeShiftL` 12)
                    .|. (cont 2 `unsafeShiftL` 6)
                    .|. cont 3
         in (chr cp, 4)
    where
        !b0 = BSU.unsafeIndex bs idx
        {-# INLINE cont #-}
        cont !off = fromIntegral (BSU.unsafeIndex bs (idx + off) .&. 0x3f)

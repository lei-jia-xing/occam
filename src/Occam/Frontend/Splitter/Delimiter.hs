-- | Byte-precise custom-delimiter parsing and matching for the SQL splitter.
module Occam.Frontend.Splitter.Delimiter (
    attachedDelimiter,
    isDelimiterWord,
    matchCustomDelimiter,
    parseDelimiterDirective,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Vector (Vector)
import Data.Vector qualified as V
import Occam.Frontend.Lexer (
    LiteralKind (..),
    SpannedToken,
    Token (..),
    WordToken (..),
    asciiEqCI,
    isTriviaToken,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..), unsafeSliceSpan)

{- | Detect a custom delimiter attached to the suffix of a valid unquoted identifier.
The delimiter is clipped only when it is active.
-}
attachedDelimiter :: Token -> ByteSpan -> ByteString -> Maybe ByteSpan
attachedDelimiter !tok tokSpan !delimiter = case tok of
    TokWord WordToken {wordQuote = Nothing, wordRaw = raw}
        | BS.length raw > BS.length delimiter
        , BS.isSuffixOf delimiter raw ->
            let !wordEnd = spanEnd tokSpan
             in Just (ByteSpan (wordEnd - BS.length delimiter) wordEnd)
    _ -> Nothing

-- | Identify an unquoted @DELIMITER@ directive word.
isDelimiterWord :: Token -> Bool
isDelimiterWord (TokWord WordToken {wordQuote = Nothing, wordRaw = raw}) =
    BS.length raw == 9 && asciiEqCI raw "DELIMITER"
isDelimiterWord _ = False

-- | Match delimiter bytes at a token start, consuming only complete tokens.
matchCustomDelimiter ::
    ByteString -> Int -> Vector SpannedToken -> Int -> Int -> ByteString -> Maybe (Int, ByteSpan)
matchCustomDelimiter sql !sqlLen vec !n !i !delim = do
    guard (not (BS.null delim))
    let tok = vec V.! i
    guard (not (isQuotedLitOrIdent (locValue tok)))
    let !startByte = spanStart (locSpan tok)
        !dLen = BS.length delim
        !endByte = startByte + dLen
    guard (endByte <= sqlLen)
    guard (BS.isPrefixOf delim (BSU.unsafeDrop startByte sql))
    nextIdx <- consumeDelimTokens vec n i endByte
    pure (nextIdx, ByteSpan startByte endByte)

-- | Parse a @DELIMITER@ argument and return its byte end, contents, and token end.
parseDelimiterDirective ::
    ByteString -> Int -> Vector SpannedToken -> Int -> Int -> Maybe (Int, ByteString, Int)
parseDelimiterDirective sql !sqlLen vec !n !i = do
    let !afterKw = spanEnd (locSpan (vec V.! i))
        !argStart = skipHorizSpace sql sqlLen afterKw
    guard (argStart > afterKw && argStart < sqlLen)
    let !argEnd = scanDelimArg sql sqlLen argStart
    guard (argEnd > argStart)
    let !newDelim = unsafeSliceSpan (ByteSpan argStart argEnd) sql
        !afterArg = skipToksBefore vec n argEnd (i + 1)
    pure (argEnd, newDelim, afterArg)

isQuotedLitOrIdent :: Token -> Bool
isQuotedLitOrIdent (TokLiteral LitNumber _) = False
isQuotedLitOrIdent (TokLiteral {}) = True
isQuotedLitOrIdent (TokWord WordToken {wordQuote = Just _}) = True
isQuotedLitOrIdent _ = False

consumeDelimTokens :: Vector SpannedToken -> Int -> Int -> Int -> Maybe Int
consumeDelimTokens vec !n !k !targetEnd = do
    guard (k < n)
    guard (not (isTriviaToken (locValue (vec V.! k))))
    let !e = spanEnd (locSpan (vec V.! k))
    if e == targetEnd
        then pure (k + 1)
        else guard (e < targetEnd) *> consumeDelimTokens vec n (k + 1) targetEnd

skipHorizSpace :: ByteString -> Int -> Int -> Int
skipHorizSpace sql !sqlLen !p
    | p < sqlLen && (b == 32 || b == 9) = skipHorizSpace sql sqlLen (p + 1)
    | otherwise = p
    where
        b = BSU.unsafeIndex sql p

scanDelimArg :: ByteString -> Int -> Int -> Int
scanDelimArg sql !sqlLen !p
    | p < sqlLen && b /= 32 && b /= 9 && b /= 10 && b /= 13 = scanDelimArg sql sqlLen (p + 1)
    | otherwise = p
    where
        b = BSU.unsafeIndex sql p

skipToksBefore :: Vector SpannedToken -> Int -> Int -> Int -> Int
skipToksBefore vec !n !bytePos !k
    | k < n && spanStart (locSpan (vec V.! k)) < bytePos = skipToksBefore vec n bytePos (k + 1)
    | otherwise = k

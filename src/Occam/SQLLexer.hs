module Occam.SQLLexer (SQLToken (..), lexSQL, sqlSemicolons) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core (Span (..))

data SQLToken
    = WordToken !T.Text
    | IdentifierToken !T.Text
    | SemicolonToken !Span
    deriving (Eq, Show)

lexSQL :: ByteString -> [SQLToken]
lexSQL = scanSQL True

sqlSemicolons :: ByteString -> [Span]
sqlSemicolons = foldr collect [] . scanSQL False
    where
        collect (SemicolonToken semicolonSpan) spans = semicolonSpan : spans
        collect _ spans = spans

scanSQL :: Bool -> ByteString -> [SQLToken]
scanSQL emitWords source = scan 0
    where
        sourceLength = BS.length source

        scan position
            | position >= sourceLength = []
            | isSpaceByte (at position) = scan (position + 1)
            | pairAt position 45 45 = scan (skipLine (position + 2))
            | pairAt position 47 42 = scan (skipBlock 1 (position + 2))
            | at position == 69 || at position == 101
            , position + 1 < sourceLength
            , at (position + 1) == 39 =
                scan (skipQuoted 39 True (position + 2))
            | at position == 39 = scan (skipQuoted 39 False (position + 1))
            | Just closer <- quoteCloser (at position) =
                if emitWords
                    then
                        let (name, next) = readIdentifier closer (position + 1) []
                         in IdentifierToken (TE.decodeUtf8Lenient (BS.pack (reverse name))) : scan next
                    else scan (skipQuotedIdentifier closer (position + 1))
            | at position == 36
            , Just (afterDelimiter, delimiter) <- dollarDelimiter position =
                scan (skipDollarString afterDelimiter delimiter)
            | at position == 59 =
                SemicolonToken (Span position (position + 1)) : scan (position + 1)
            | isWordByte (at position) =
                let end = skipWhile isWordByte position
                 in if emitWords
                        then
                            let word = TE.decodeUtf8Lenient (BS.take (end - position) (BS.drop position source))
                             in WordToken (T.toLower word) : scan end
                        else scan end
            | otherwise = scan (position + 1)

        at = BS.index source

        pairAt position first second =
            position + 1 < sourceLength && at position == first && at (position + 1) == second

        skipLine position
            | position >= sourceLength || at position == 10 || at position == 13 = position
            | otherwise = skipLine (position + 1)

        skipBlock :: Int -> Int -> Int
        skipBlock _ position | position >= sourceLength = sourceLength
        skipBlock depth position
            | pairAt position 47 42 = skipBlock (depth + 1) (position + 2)
            | pairAt position 42 47 =
                if depth == 1
                    then position + 2
                    else skipBlock (depth - 1) (position + 2)
            | otherwise = skipBlock depth (position + 1)

        skipQuoted quote escapeBackslash position
            | position >= sourceLength = sourceLength
            | at position == quote && pairAt position quote quote =
                skipQuoted quote escapeBackslash (position + 2)
            | at position == quote = position + 1
            | escapeBackslash && at position == 92 && position + 1 < sourceLength =
                skipQuoted quote escapeBackslash (position + 2)
            | otherwise = skipQuoted quote escapeBackslash (position + 1)

        skipQuotedIdentifier closer position
            | position >= sourceLength = sourceLength
            | at position == closer && pairAt position closer closer =
                skipQuotedIdentifier closer (position + 2)
            | at position == closer = position + 1
            | otherwise = skipQuotedIdentifier closer (position + 1)

        readIdentifier closer position accumulated
            | position >= sourceLength = (accumulated, sourceLength)
            | at position == closer && pairAt position closer closer =
                readIdentifier closer (position + 2) (closer : accumulated)
            | at position == closer = (accumulated, position + 1)
            | otherwise = readIdentifier closer (position + 1) (at position : accumulated)

        dollarDelimiter position =
            let tagStart = position + 1
                tagEnd
                    | tagStart < sourceLength && at tagStart == 36 = tagStart
                    | tagStart < sourceLength && isDollarTagStart (at tagStart) =
                        skipWhile isDollarTagByte tagStart
                    | otherwise = sourceLength
             in if tagEnd < sourceLength && at tagEnd == 36
                    then
                        let afterDelimiter = tagEnd + 1
                            delimiter = BS.take (afterDelimiter - position) (BS.drop position source)
                         in Just (afterDelimiter, delimiter)
                    else Nothing

        skipDollarString position delimiter =
            let (before, rest) = BS.breakSubstring delimiter (BS.drop position source)
             in if BS.null rest
                    then sourceLength
                    else position + BS.length before + BS.length delimiter

        skipWhile predicate position
            | position < sourceLength && predicate (at position) = skipWhile predicate (position + 1)
            | otherwise = position

        isSpaceByte byte = byte == 9 || byte == 10 || byte == 11 || byte == 12 || byte == 13 || byte == 32

        isWordByte byte =
            isAsciiLetter byte
                || (byte >= 48 && byte <= 57)
                || byte == 95
                || byte == 36
                || byte >= 128

        isAsciiLetter byte = (byte >= 65 && byte <= 90) || (byte >= 97 && byte <= 122)
        isDollarTagStart byte = isAsciiLetter byte || byte == 95
        isDollarTagByte byte = isDollarTagStart byte || (byte >= 48 && byte <= 57)

        quoteCloser 34 = Just 34
        quoteCloser 96 = Just 96
        quoteCloser 91 = Just 93
        quoteCloser _ = Nothing

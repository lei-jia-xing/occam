-- | Single-pass UTF-8 lexer that retains trivia and exact byte spans.
module Occam.Frontend.Lexer (
    Keyword (..),
    QuoteKind (..),
    WordToken (..),
    LiteralKind (..),
    TriviaKind (..),
    Token (..),
    SpannedToken,
    LexErrorKind (..),
    LexError (..),
    LexResult (..),
    lexSQL,
    lexSQLStrict,
    isTriviaToken,
    isWhitespaceToken,
    isCommentToken,
    isKeywordToken,
    keywordText,
    asciiUpperBS,
    asciiEqCI,
) where

import Occam.Frontend.Lexer.Keyword (Keyword (..), asciiEqCI, asciiUpperBS, keywordText)
import Occam.Frontend.Lexer.Scan (lexSQL, lexSQLStrict)
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
    isCommentToken,
    isKeywordToken,
    isTriviaToken,
    isWhitespaceToken,
 )

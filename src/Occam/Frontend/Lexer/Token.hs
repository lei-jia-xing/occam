-- | Token and lex-result types shared by the lexer and its consumers.
module Occam.Frontend.Lexer.Token (
    QuoteKind (..),
    WordToken (..),
    LiteralKind (..),
    TriviaKind (..),
    Token (..),
    SpannedToken,
    LexErrorKind (..),
    LexError (..),
    LexResult (..),
    isTriviaToken,
    isWhitespaceToken,
    isCommentToken,
    isKeywordToken,
) where

import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Text (Text)
import Data.Vector (Vector)
import Occam.Frontend.Lexer.Keyword (Keyword)
import Occam.Frontend.Span (Located)

-- | Identifier quote style.
data QuoteKind
    = QuoteDouble
    | QuoteBacktick
    | QuoteBracket
    deriving (Eq, Ord, Show)

-- | An identifier or keyword token, retaining its original raw slice and quote style.
data WordToken = WordToken
    { wordRaw :: !ByteString
    , wordQuote :: !(Maybe QuoteKind)
    , wordKeyword :: !(Maybe Keyword)
    }
    deriving (Eq, Ord, Show)

-- | Literal token classification.
data LiteralKind
    = LitNumber
    | LitString
    | LitEscapeString
    | LitDollarString
    | LitHexString
    | LitBitString
    | LitNationalString
    deriving (Eq, Ord, Show, Data)

-- | Trivia classification preserved in the token stream.
data TriviaKind
    = TriviaWhitespace
    | TriviaLineComment
    | TriviaBlockComment
    | TriviaConditionalComment
    deriving (Eq, Ord, Show)

-- | Lexical token kind.
data Token
    = TokTrivia !TriviaKind
    | TokWord !WordToken
    | TokLiteral !LiteralKind !ByteString
    | TokParam !ByteString
    | TokUserVar !ByteString
    | TokSysVar !ByteString
    | TokLParen
    | TokRParen
    | TokLBracket
    | TokRBracket
    | TokLBrace
    | TokRBrace
    | TokComma
    | TokSemicolon
    | TokDot
    | TokDoubleColon
    | TokDotColon
    | TokColonEq
    | TokColon
    | TokArrow
    | TokDoubleArrow
    | TokPlus
    | TokMinus
    | TokStar
    | TokSlash
    | TokEq
    | TokQuestion
    | TokOperator !ByteString
    | TokUnknown !ByteString
    deriving (Eq, Ord, Show)

-- | Token plus its exact source span.
type SpannedToken = Located Token

-- | Lexical failure class.
data LexErrorKind
    = LexInvalidUtf8
    | LexUnterminatedString
    | LexUnterminatedDollarString
    | LexUnterminatedIdent
    | LexUnterminatedComment
    deriving (Eq, Ord, Show)

-- | Lexical failure at the byte offset where the construct starts.
data LexError = LexError
    { lexErrorKind :: !LexErrorKind
    , lexErrorOffset :: !Int
    , lexErrorMessage :: !Text
    }
    deriving (Eq, Ord, Show)

-- | Tokens scanned before an error, plus the first error if any.
data LexResult = LexResult
    { lexTokens :: !(Vector SpannedToken)
    , lexError :: !(Maybe LexError)
    }
    deriving (Eq, Show)

-- | True for whitespace, comments, and conditional comments.
{-# INLINE isTriviaToken #-}
isTriviaToken :: Token -> Bool
isTriviaToken (TokTrivia _) = True
isTriviaToken _ = False

-- | True only for whitespace trivia.
{-# INLINE isWhitespaceToken #-}
isWhitespaceToken :: Token -> Bool
isWhitespaceToken (TokTrivia TriviaWhitespace) = True
isWhitespaceToken _ = False

-- | True for line, block, and conditional comments.
{-# INLINE isCommentToken #-}
isCommentToken :: Token -> Bool
isCommentToken (TokTrivia TriviaWhitespace) = False
isCommentToken (TokTrivia _) = True
isCommentToken _ = False

-- | True for an unquoted keyword token equal to @expected@.
{-# INLINE isKeywordToken #-}
isKeywordToken :: Keyword -> Token -> Bool
isKeywordToken expected (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just found}) =
    found == expected
isKeywordToken _ _ = False

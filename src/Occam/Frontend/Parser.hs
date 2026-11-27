-- | Dialect-dispatched parser producing 'ParsedScript' with exact spans.
module Occam.Frontend.Parser (
    ParseErrorKind (..),
    ParseError (..),
    ParsedScript (..),
    parseUnit,
    parseScript,
    parseScriptUnits,
    module Occam.Frontend.AST,
) where

import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, isNothing)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Occam.Frontend.AST
import Occam.Frontend.Lexer (
    LexError (..),
    SpannedToken,
    Token (..),
    isTriviaToken,
 )
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.Query (parseQuery)
import Occam.Frontend.Parser.Stmt (parseStatement)
import Occam.Frontend.Span (ByteSpan (..), Located (..), spanEnd, spanStart)
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Parser.Dialect (DialectProfile)

{- | Parse a single split unit.  Leading/trailing trivia and a trailing
statement terminator are excluded from the returned statement span.
-}
parseUnit :: DialectProfile -> SplitUnit -> Either ParseError (Located Statement)
parseUnit profile unit = do
    result <- parseUnitStatement profile unit
    case result of
        Just statement -> Right statement
        Nothing ->
            let s = spanStart (splitUnitSpan unit)
             in Left (ParseError ParseUnexpectedEnd (ByteSpan s s) "empty SQL statement")

parseUnitStatement :: DialectProfile -> SplitUnit -> Either ParseError (Maybe (Located Statement))
parseUnitStatement profile unit
    | Just lexErr <- splitUnitLexError unit = Left (lexErrorToParseError lexErr)
    | Just delimiter <- splitUnitDelimiterDirective unit =
        Right (Just (L (splitUnitSpan unit) (StatementDelimiter delimiter)))
    | V.null tokens = Right Nothing
    | otherwise = Just <$> runUnitParser profile tokens (spanEnd (splitUnitSpan unit))
    where
        tokens = statementTokens (splitUnitTokens unit)

{- | Lex, split, and parse every SQL statement in @sql@ under @profile@.
Recognized client delimiter directives are consumed by the splitter.
-}
parseScript :: DialectProfile -> ByteString -> Either ParseError ParsedScript
parseScript profile sql = do
    units <- either (Left . lexErrorToParseError) Right (splitSQL profile sql)
    ParsedScript . catMaybes
        <$> traverse (parseUnitStatement profile) (filter (isNothing . splitUnitDelimiterDirective) units)

{- | Keep each unit and its own parse result, including recognized client directives.
Invalid UTF-8 is a script-wide error. A trailing lexical failure belongs to its unit.
-}
parseScriptUnits ::
    DialectProfile -> ByteString -> Either LexError [(SplitUnit, Either ParseError (Located Statement))]
parseScriptUnits profile sql = map (\unit -> (unit, parseUnit profile unit)) <$> splitSQL profile sql

runUnitParser ::
    DialectProfile -> Vector SpannedToken -> Int -> Either ParseError (Located Statement)
runUnitParser profile tokens endOffset =
    runParser profile tokens endOffset parseQuery parseStatement parsedStatement

parsedStatement :: P (Located Statement)
parsedStatement = parseStatement <* expectStatementEnd

statementTokens :: Vector SpannedToken -> Vector SpannedToken
statementTokens toks = V.unsafeSlice 0 (endIndex (V.length withoutTrivia)) withoutTrivia
    where
        withoutTrivia = V.filter (not . isTriviaToken . locValue) toks
        endIndex !n
            | n > 0 && locValue (V.unsafeIndex withoutTrivia (n - 1)) == TokSemicolon = endIndex (n - 1)
            | otherwise = n

lexErrorToParseError :: LexError -> ParseError
lexErrorToParseError lexErr =
    ParseError
        { parseErrorKind = ParseLexicalError
        , parseErrorSpan = ByteSpan off off
        , parseErrorMessage = lexErrorMessage lexErr
        }
    where
        off = lexErrorOffset lexErr

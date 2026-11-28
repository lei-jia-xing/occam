-- | Shared parser assertion and fixture helpers for parser regression suites.
module ParserChecks.Support (
    parseStatements,
    parseSingle,
    parseSelect,
    singleSelectExpr,
    assertSlice,
    assertColumn,
    assertNumberLiteral,
) where

import Data.ByteString (ByteString)
import Occam.Frontend.Parser
import Occam.Frontend.Span (ByteSpan (..), Located (..), sliceSpan)
import Occam.Parser.Dialect (DialectProfile (..))
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure)

parseStatements :: DialectProfile -> ByteString -> IO [Located Statement]
parseStatements profile sql =
    case parseScript profile sql of
        Left err -> assertFailure ("unexpected parse error: " <> show err)
        Right (ParsedScript stmts) -> pure stmts

parseSingle :: DialectProfile -> ByteString -> IO (Located Statement)
parseSingle profile sql = do
    stmts <- parseStatements profile sql
    case stmts of
        [stmt] -> pure stmt
        _ -> assertFailure ("expected exactly one statement, got " <> show (length stmts))

parseSelect :: DialectProfile -> ByteString -> IO SelectCore
parseSelect profile sql = do
    stmt <- parseSingle profile sql
    case stmt of
        L _ (StatementQuery query) ->
            case locValue (queryBody (locValue query)) of
                QueryBodySelect core -> pure core
                other -> assertFailure ("expected a SELECT query body, got " <> show other)
        other -> assertFailure ("expected a query statement, got " <> show other)

singleSelectExpr :: DialectProfile -> ByteString -> IO (Located Expr)
singleSelectExpr profile sql = do
    core <- parseSelect profile sql
    case selectItems core of
        [item] -> pure (selectItemExpr (locValue item))
        items -> assertFailure ("expected one projection item, got " <> show (length items))

assertSlice :: ByteString -> ByteSpan -> ByteString -> Assertion
assertSlice sql sp expected =
    case sliceSpan sp sql of
        Just actual -> assertEqual ("slice at " <> show sp) expected actual
        Nothing -> assertFailure ("span " <> show sp <> " is not valid for the input")

assertColumn :: ByteString -> Located Expr -> Assertion
assertColumn expected expr =
    case locValue expr of
        ExprColumn (L _ (Name [L _ part])) ->
            assertEqual "column name" expected (namePartRaw part)
        other -> assertFailure ("expected column " <> show expected <> ", got " <> show other)

assertNumberLiteral :: ByteString -> Located Expr -> Assertion
assertNumberLiteral expected expr =
    case locValue expr of
        ExprLiteral (Literal LitNumber raw) -> assertEqual "number literal" expected raw
        other -> assertFailure ("expected number literal " <> show expected <> ", got " <> show other)

module FrontendFacadeChecks (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import DependencyChecks.Support (analyzeParsed, factsFor, has, ref)
import Occam.Core (
    BarrierScope (..),
    BeginPolicy (..),
    Ident (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    SessionEffect (..),
    StatementId (..),
    TxEffect (..),
 )
import Occam.Parser
import Occam.Slicer (backwardSlice)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Haskell frontend through the product parser"
        [ testCase "PostgreSQL TABLE retains its definition and writes" $ do
            let sql =
                    "CREATE TABLE \"Input\" (x INT); INSERT INTO \"Input\" VALUES (1); TABLE \"Input\";"
            graph <- analyzeParsed PostgreSQL sql
            let slice = backwardSlice (StatementId 3) graph
            assertBool "TABLE reads all rows" (all (\sid -> has (StatementId sid) slice) [1, 2])
            parsed <- parseOrFail (spanOnlyOptions PostgreSQL) sql
            assertEqual
                "span-only parse agrees on validity"
                (replicate 3 Nothing)
                (map unitParseError (parsedUnits parsed))
        , testCase "SQLite PRAGMA boolean forms update FK checks" $ do
            let sql =
                    "PRAGMA main.foreign_keys = ON; PRAGMA foreign_keys(no); PRAGMA foreign_keys;"
            parsed <- parseOrFail (defaultParseOptions SQLite) sql
            facts <- traverse unitSemantics (parsedUnits parsed)
            assertEqual
                "the last PRAGMA reads the setting"
                [[SetSqliteForeignKeys True], [SetSqliteForeignKeys False], []]
                (map factSession facts)
        , testCase "PostgreSQL transaction modes and quoted savepoints survive lowering" $ do
            let sql = "BEGIN READ ONLY; SAVEPOINT \"Mixed\"; ROLLBACK TO \"Mixed\"; COMMIT AND CHAIN;"
            parsed <- parseOrFail (defaultParseOptions PostgreSQL) sql
            facts <- traverse unitSemantics (parsedUnits parsed)
            assertEqual "begin policy" [[TxBegin BeginWarn], [], [], []] (map factPreTx facts)
            assertEqual
                "transaction effects"
                [ [TxSetReadOnly True]
                , [TxSavepoint (Ident "Mixed")]
                , [TxRollbackTo (Ident "Mixed")]
                , [TxCommit True]
                ]
                (map factPostTx facts)
        , testCase "ClickHouse OPTIMIZE scopes dependencies to the target relation" $ do
            let sql =
                    "CREATE TABLE dst (id Int32) ENGINE = Memory; SELECT 1; OPTIMIZE TABLE dst FINAL; SELECT id FROM dst;"
            graph <- analyzeParsed ClickHouse sql
            let slice = backwardSlice (StatementId 4) graph
            assertEqual
                "only the target relation is fenced"
                [RelationScope (ref ["dst"])]
                (map barrierScope (factBarriers (factsFor (StatementId 3) graph)))
            assertBool "keep target and optimization" (all (\sid -> has (StatementId sid) slice) [1, 3])
            assertBool "independent statement stays out of the slice" (not (has (StatementId 2) slice))
        , testCase "ClickHouse PREWHERE and SETTINGS use complete absolute byte spans" $ do
            let sql =
                    encodeUtf8
                        "SELECT '前置'; SELECT 'PREWHERE' AS café FROM t PREWHERE /* 条件 */ x > 0 WHERE y > 0 SETTINGS max_threads = 1, max_block_size = 8;"
            assertClauseEdit ClickHouse sql "remove-prewhere" "PREWHERE /* 条件 */ x > 0"
            assertClauseEdit ClickHouse sql "remove-settings" "SETTINGS max_threads = 1, max_block_size = 8"
        , testCase "DuckDB QUALIFY edit preserves the following ORDER BY" $ do
            let sql = encodeUtf8 "SELECT '前置'; SELECT x FROM t QUALIFY row_number() OVER () > 0 ORDER BY x;"
            assertClauseEdit DuckDB sql "remove-qualify" "QUALIFY row_number() OVER () > 0"
        ]

parseOrFail :: ParseOptions -> ByteString -> IO ParsedSQL
parseOrFail options sql = parseSQL options sql >>= either (assertFailure . show) pure

unitSemantics :: StatementUnit -> IO SemanticFacts
unitSemantics unit =
    case (unitParseError unit, unitFacts unit) of
        (Nothing, Just facts) -> pure (factSemantics facts)
        _ -> assertFailure (show unit)

assertClauseEdit :: SQLDialect -> ByteString -> Text -> Text -> IO ()
assertClauseEdit dialect sql label clause = do
    let options = (defaultParseOptions dialect) {parseSyntaxOnly = True, parseSyntaxEdits = True}
    parsed <- parseOrFail options sql
    let edits = concat [factEdits facts | unit <- parsedUnits parsed, Just facts <- [unitFacts unit]]
    case find ((== label) . parserEditLabel) edits of
        Nothing -> assertFailure ("missing edit " <> show label)
        Just edit -> do
            let start = parserEditStart edit
                end = parserEditEnd edit
                candidate = BS.take start sql <> parserEditReplacement edit <> BS.drop end sql
            assertEqual
                "keyword, in-clause trivia and body"
                (encodeUtf8 clause)
                (BS.take (end - start) (BS.drop start sql))
            assertEqual "clause deletion" "" (parserEditReplacement edit)
            reparsed <- parseOrFail options candidate
            assertEqual
                "both statements still parse"
                [Nothing, Nothing]
                (map unitParseError (parsedUnits reparsed))

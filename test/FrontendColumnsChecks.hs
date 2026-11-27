module FrontendColumnsChecks (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core (Ident (..))
import Occam.Facts (
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    defaultParseOptions,
    parseSQL,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Pure-Haskell frontend columns"
        [ columnEditsTests
        , assembleColumnsTests
        ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseAndAssemble :: SQLDialect -> ByteString -> IO ([StatementUnit], [ColumnTarget])
parseAndAssemble dialect sql = do
    let opts =
            (defaultParseOptions dialect)
                { parseColumnIR = True
                , parseColumnBundles = True
                , parseSyntaxEdits = True
                }
    res <- parseSQL opts sql
    case res of
        Left err -> assertFailure ("parseSQL failed: " <> show err)
        Right parsed -> pure (parsedUnits parsed, parsedColumns parsed)

findTarget :: [ColumnTarget] -> Text -> IO ColumnTarget
findTarget targets col = case find ((== Ident col) . targetColumn) targets of
    Just t -> pure t
    Nothing -> assertFailure ("missing target for column: " <> T.unpack col)

siteSlices :: ByteString -> ColumnTarget -> [ByteString]
siteSlices sql tgt =
    [ BSC.take (siteEnd s - siteStart s) (BSC.drop (siteStart s) sql)
    | s <- targetSites tgt
    ]

applySites :: ByteString -> ColumnTarget -> ByteString
applySites sql tgt = go 0 (targetSites tgt)
    where
        go cursor [] = BSC.drop cursor sql
        go cursor (s : rest) =
            BSC.take (siteStart s - cursor) (BSC.drop cursor sql) <> go (siteEnd s) rest

hsColumnEdits :: SQLDialect -> Bool -> ByteString -> IO [(Text, ByteString)]
hsColumnEdits dialect columnIr sql = do
    let opts =
            (defaultParseOptions dialect)
                { parseColumnIR = columnIr
                , parseSyntaxEdits = True
                }
    res <- parseSQL opts sql
    case res of
        Left err -> assertFailure ("parseSQL failed: " <> show err)
        Right parsed ->
            pure
                [ (parserEditLabel e, sliceEdit (parserEditStart e) (parserEditEnd e))
                | u <- parsedUnits parsed
                , Just f <- [unitFacts u]
                , e <- factEdits f
                , parserEditCategory e == EditColumn
                ]
    where
        sliceEdit s e = BSC.take (e - s) (BSC.drop s sql)

-- ---------------------------------------------------------------------------
-- columnEdits tests
-- ---------------------------------------------------------------------------

columnEditsTests :: TestTree
columnEditsTests =
    testGroup
        "columnEdits"
        [ testCase "gated off when parseColumnIR and parseColumnBundles are False" do
            let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);"
            edits <- hsColumnEdits SQLite False sql
            assertEqual "no EditColumn edits when gated off" [] edits
        , testCase "CREATE TABLE first, middle, last comma gating and singleton" do
            let sql = "CREATE TABLE t(a INT, b INT, c INT);\nCREATE TABLE s(x INT);"
            edits <- hsColumnEdits SQLite True sql
            assertEqual
                "create_def edits"
                [ ("remove-column_create_def", "a INT,")
                , ("remove-column_create_def", ", b INT")
                , ("remove-column_create_def", ", c INT")
                ]
                edits
        , testCase "CREATE TABLE single column with table constraint emits edit" do
            let sql = "CREATE TABLE t(a INT, PRIMARY KEY(a));"
            edits <- hsColumnEdits SQLite True sql
            assertEqual
                "column before constraint gets trailing comma"
                [("remove-column_create_def", "a INT,")]
                edits
        , testCase "CREATE TABLE preserves inline comments before comma" do
            let sql = "CREATE TABLE t(a INT /* keep */ , b INT);"
            edits <- hsColumnEdits SQLite True sql
            assertEqual
                "preceding whitespace only"
                [ ("remove-column_create_def", "a INT /* keep */ ,")
                , ("remove-column_create_def", " , b INT")
                ]
                edits
        , testCase "INSERT explicit column list and multi-row VALUES edits" do
            let sql = "INSERT INTO t(a, b, c) VALUES (1, 2, 3), (4, 5, 6);"
            edits <- hsColumnEdits SQLite True sql
            assertEqual
                "insert and values edits"
                [ ("remove-column_insert_item", "a, ")
                , ("remove-column_insert_item", ", b")
                , ("remove-column_insert_item", ", c")
                , ("remove-column_values_item", "1, ")
                , ("remove-column_values_item", ", 2")
                , ("remove-column_values_item", ", 3")
                , ("remove-column_values_item", "4, ")
                , ("remove-column_values_item", ", 5")
                , ("remove-column_values_item", ", 6")
                ]
                edits
        , testCase "singleton INSERT column list and singleton VALUES row emit no edits" do
            let sql = "INSERT INTO t(a) VALUES (1);"
            edits <- hsColumnEdits SQLite True sql
            assertEqual "no edits for singleton lists" [] edits
        , testCase
            "tainted INSERT (ON CONFLICT / RETURNING / OR IGNORE) keeps insert_item but drops values_item"
            do
                let sql1 = "INSERT INTO t(a, b) VALUES (1, 2) ON CONFLICT DO NOTHING;"
                    sql2 = "INSERT INTO t(a, b) VALUES (1, 2) RETURNING a;"
                    sql3 = "INSERT OR IGNORE INTO t(a, b) VALUES (1, 2);"
                forM_ [sql1, sql2, sql3] \sql -> do
                    edits <- hsColumnEdits SQLite True sql
                    assertEqual
                        "only insert_item edits survive taint"
                        [ ("remove-column_insert_item", "a, ")
                        , ("remove-column_insert_item", ", b")
                        ]
                        edits
        , testCase "MySQL VALUES ROW(...) emits values_item edits" do
            let sql = "INSERT INTO t(a, b) VALUES ROW(1, 2), ROW(3, 4);"
            edits <- hsColumnEdits MySQL True sql
            assertEqual
                "MySQL ROW() edits"
                [ ("remove-column_insert_item", "a, ")
                , ("remove-column_insert_item", ", b")
                , ("remove-column_values_item", "1, ")
                , ("remove-column_values_item", ", 2")
                , ("remove-column_values_item", "3, ")
                , ("remove-column_values_item", ", 4")
                ]
                edits
        , testCase "EXPLAIN ANALYZE unwraps inner CREATE/INSERT for columnEdits" do
            let sql = "EXPLAIN ANALYZE CREATE TABLE t(a INT, b INT);"
            edits <- hsColumnEdits PostgreSQL True sql
            assertEqual
                "EXPLAIN ANALYZE unwraps"
                [ ("remove-column_create_def", "a INT,")
                , ("remove-column_create_def", ", b INT")
                ]
                edits
        ]

-- ---------------------------------------------------------------------------
-- assembleColumns unit tests
-- ---------------------------------------------------------------------------

assembleColumnsTests :: TestTree
assembleColumnsTests =
    testGroup
        "assembleColumns"
        [ testCase "explicit bundle forms exact joint sites" do
            let sql =
                    "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 10), (3, 20);\nSELECT a FROM t ORDER BY a;"
            (_, targets) <- parseAndAssemble SQLite sql
            assertEqual "two targets" 2 (length targets)
            tgtA <- findTarget targets "a"
            tgtB <- findTarget targets "b"
            assertEqual "a blocked" (Just "column-read-elsewhere") (targetBlocked tgtA)
            assertEqual "b unblocked" Nothing (targetBlocked tgtB)
            assertEqual "b ordinal" 1 (targetOrdinal tgtB)
            assertEqual
                "b site kinds"
                [CreateDefSite, InsertListSite, ValuesItemSite, ValuesItemSite]
                (map siteKind (targetSites tgtB))
            assertEqual "b slices" [", b INT", ", b", ", 10", ", 20"] (siteSlices sql tgtB)
            assertEqual
                "b applied"
                "CREATE TABLE t(a INT);\nINSERT INTO t(a) VALUES (1), (3);\nSELECT a FROM t ORDER BY a;"
                (applySites sql tgtB)
        , testCase "implicit bundle binds by definition order" do
            let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t VALUES (1, 10), (3, 20);"
            (_, targets) <- parseAndAssemble SQLite sql
            tgtB <- findTarget targets "b"
            assertEqual "b unblocked" Nothing (targetBlocked tgtB)
            assertEqual "b slices" [", b INT", ", 10", ", 20"] (siteSlices sql tgtB)
            assertEqual
                "b applied"
                "CREATE TABLE t(a INT);\nINSERT INTO t VALUES (1), (3);"
                (applySites sql tgtB)
        , testCase "constraint blocks only constrained column" do
            let sql = "CREATE TABLE t(a INT, b INT PRIMARY KEY);\nINSERT INTO t(a, b) VALUES (1, 2);"
            (_, targets) <- parseAndAssemble SQLite sql
            tgtA <- findTarget targets "a"
            tgtB <- findTarget targets "b"
            assertEqual "a unblocked" Nothing (targetBlocked tgtA)
            assertEqual "b blocked" (Just "column-constraint-ref") (targetBlocked tgtB)
        , testCase "star projection blocks all columns" do
            let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT * FROM t;"
            (_, targets) <- parseAndAssemble SQLite sql
            tgtA <- findTarget targets "a"
            tgtB <- findTarget targets "b"
            assertEqual "a star" (Just "column-star-projection") (targetBlocked tgtA)
            assertEqual "b star" (Just "column-star-projection") (targetBlocked tgtB)
        , testCase "mixed star and explicit read still blocks as star" do
            let sql =
                    "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT t1.*, t2.a FROM t AS t1 JOIN t AS t2 ON t1.a = t2.a;"
            (_, targets) <- parseAndAssemble SQLite sql
            tgtA <- findTarget targets "a"
            tgtB <- findTarget targets "b"
            assertEqual "a star" (Just "column-star-projection") (targetBlocked tgtA)
            assertEqual "b star" (Just "column-star-projection") (targetBlocked tgtB)
        , testCase "parse error unit blocks everything" do
            let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nGARBAGE (((;"
            (_, targets) <- parseAndAssemble SQLite sql
            tgtA <- findTarget targets "a"
            tgtB <- findTarget targets "b"
            assertEqual "a blocked" (Just "column-unknown-source") (targetBlocked tgtA)
            assertEqual "b blocked" (Just "column-unknown-source") (targetBlocked tgtB)
        , testCase "parent FK blocks referenced column only; implicit blocks whole parent" do
            let sql1 =
                    "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT REFERENCES t(b));\nINSERT INTO t(a, b) VALUES (1, 2);"
            (_, t1) <- parseAndAssemble SQLite sql1
            assertEqual "a free" Nothing . targetBlocked =<< findTarget t1 "a"
            assertEqual "b dep" (Just "column-dependent-object") . targetBlocked =<< findTarget t1 "b"

            let sql2 =
                    "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT, FOREIGN KEY (x) REFERENCES t);\nINSERT INTO t(a, b) VALUES (1, 2);"
            (_, t2) <- parseAndAssemble SQLite sql2
            assertEqual "a dep" (Just "column-dependent-object") . targetBlocked =<< findTarget t2 "a"
            assertEqual "b dep" (Just "column-dependent-object") . targetBlocked =<< findTarget t2 "b"
        , testCase "rename chain two hops binds each segment" do
            let sql =
                    "CREATE TABLE t(a INT, b INT);\n\
                    \INSERT INTO t(a, b) VALUES (1, 2);\n\
                    \ALTER TABLE t RENAME TO u;\n\
                    \INSERT INTO u(a, b) VALUES (3, 4);\n\
                    \ALTER TABLE u RENAME TO v;\n\
                    \INSERT INTO v(a, b) VALUES (5, 6);"
            (_, targets) <- parseAndAssemble SQLite sql
            tgtB <- findTarget targets "b"
            assertEqual "b unblocked" Nothing (targetBlocked tgtB)
            assertEqual "7 sites" [1, 2, 2, 4, 4, 6, 6] (map siteStmt (targetSites tgtB))
            assertEqual
                "applied across renames"
                "CREATE TABLE t(a INT);\n\
                \INSERT INTO t(a) VALUES (1);\n\
                \ALTER TABLE t RENAME TO u;\n\
                \INSERT INTO u(a) VALUES (3);\n\
                \ALTER TABLE u RENAME TO v;\n\
                \INSERT INTO v(a) VALUES (5);"
                (applySites sql tgtB)
        , testCase "alter add/drop/rename column narrows to affected columns with explicit inserts" do
            let sql =
                    "CREATE TABLE t(a INT, b INT, c INT);\n\
                    \ALTER TABLE t RENAME COLUMN c TO d;\n\
                    \INSERT INTO t(a, b, d) VALUES (1, 2, 3);\n\
                    \SELECT a FROM t ORDER BY a;"
            (_, targets) <- parseAndAssemble SQLite sql
            assertEqual "b free" Nothing . targetBlocked =<< findTarget targets "b"
            assertEqual "c gen" (Just "column-generation-ambiguous") . targetBlocked =<< findTarget targets "c"
            assertEqual "a read" (Just "column-read-elsewhere") . targetBlocked =<< findTarget targets "a"
        , testCase "sibling qualification ambiguity blocks touched columns" do
            let sql1 = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT b FROM main.t;"
            (_, t1) <- parseAndAssemble SQLite sql1
            assertEqual "b amb" (Just "column-ambiguous-binding") . targetBlocked =<< findTarget t1 "b"
            assertEqual "a free" Nothing . targetBlocked =<< findTarget t1 "a"

            let sql2 =
                    "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nINSERT INTO main.t VALUES (3, 4);"
            (_, t2) <- parseAndAssemble SQLite sql2
            assertEqual "a amb" (Just "column-ambiguous-binding") . targetBlocked =<< findTarget t2 "a"
            assertEqual "b amb" (Just "column-ambiguous-binding") . targetBlocked =<< findTarget t2 "b"
        , testCase "arity mismatch and ragged rows block as span-incomplete" do
            let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t VALUES (1, 10), (3);"
            (_, targets) <- parseAndAssemble SQLite sql
            assertEqual "a span" (Just "column-span-incomplete") . targetBlocked =<< findTarget targets "a"
            assertEqual "b span" (Just "column-span-incomplete") . targetBlocked =<< findTarget targets "b"
        , testCase "returning and select source block as insert-shape" do
            let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2) RETURNING a;"
            (_, targets) <- parseAndAssemble SQLite sql
            assertEqual "a shape" (Just "column-insert-shape") . targetBlocked =<< findTarget targets "a"
            assertEqual "b shape" (Just "column-insert-shape") . targetBlocked =<< findTarget targets "b"
        , testCase "transaction on unrelated table does not block clean table" do
            let sql =
                    "BEGIN;\nCREATE TABLE other(x INT);\nINSERT INTO other VALUES (1);\nCOMMIT;\n\
                    \CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT a FROM t ORDER BY a;"
            (_, targets) <- parseAndAssemble SQLite sql
            tgtB <- findTarget targets "b"
            assertEqual "b free" Nothing (targetBlocked tgtB)
            assertEqual "3 sites" 3 (length (targetSites tgtB))
        , testCase "PostgreSQL alter column USING and ROLLBACK AND CHAIN" do
            let sql1 =
                    "CREATE TABLE t(a INT, b INT, c INT);\nALTER TABLE t ALTER COLUMN a TYPE BIGINT USING (a + b);\nINSERT INTO t(a, b, c) VALUES (1, 2, 3);"
            (_, t1) <- parseAndAssemble PostgreSQL sql1
            assertEqual "a gen" (Just "column-generation-ambiguous") . targetBlocked =<< findTarget t1 "a"
            assertEqual "b gen" (Just "column-generation-ambiguous") . targetBlocked =<< findTarget t1 "b"
            assertEqual "c free" Nothing . targetBlocked =<< findTarget t1 "c"

            let sql2 =
                    "CREATE TABLE t(a INT, b INT);\nBEGIN;\nROLLBACK AND CHAIN;\nINSERT INTO t(a, b) VALUES (1, 2);\nCOMMIT;\nSELECT a FROM t ORDER BY a;"
            (_, t2) <- parseAndAssemble PostgreSQL sql2
            assertEqual "b tx opaque" (Just "column-unknown-source") . targetBlocked =<< findTarget t2 "b"
        ]

-- | Span, precedence, error-span, and dialect accept/reject parser regressions.
module ParserChecks (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Frontend.Parser
import Occam.Frontend.Span (ByteSpan (..), Located (..), spanEnd, spanStart)
import Occam.Parser.Dialect (
    DialectProfile (..),
    clickhouseProfile,
    duckdbProfile,
    mariadbProfile,
    mysqlProfile,
    postgresqlProfile,
    sqliteProfile,
 )
import ParserChecks.AstAndScriptChecks (
    dialectAstTests,
    grammarGapTests,
    realFuzzCorpusTests,
    reviewedRegressions,
    scriptTests,
 )
import ParserChecks.Support
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Pure-Haskell Parser"
        [ spanTests
        , expressionTests
        , errorTests
        , dialectTests
        , dialectAstTests
        , reviewedRegressions
        , grammarGapTests
        , scriptTests
        , realFuzzCorpusTests
        ]

-- ---------------------------------------------------------------------------
-- Exact spans
-- ---------------------------------------------------------------------------

spanTests :: TestTree
spanTests =
    testGroup
        "exact ByteSpan coverage"
        [ testCase "statement span excludes trivia and the unit delimiter" $ do
            let sql = BSC.pack "  \n\tSELECT 1  "
            stmt <- parseSingle sqliteProfile sql
            assertEqual "leading trivia and trailing spaces excluded" (ByteSpan 4 12) (locSpan stmt)
            assertSlice sql (locSpan stmt) "SELECT 1"
        , testCase "comments between tokens do not move statement or expression spans" $ do
            let sql = BSC.pack "SELECT /* c */ a +  b FROM t;"
            stmt <- parseSingle sqliteProfile sql
            assertEqual "statement span" (ByteSpan 0 28) (locSpan stmt)
            assertSlice sql (locSpan stmt) "SELECT /* c */ a +  b FROM t"
            expr <- singleSelectExpr sqliteProfile sql
            assertEqual "expression span starts at the first non-trivia token" (ByteSpan 15 21) (locSpan expr)
            assertSlice sql (locSpan expr) "a +  b"
        , testCase "comment inside an expression is included in that expression's span" $ do
            let sql = BSC.pack "SELECT a /* mid */ +  b FROM t;"
            expr <- singleSelectExpr sqliteProfile sql
            assertEqual "expression span" (ByteSpan 7 23) (locSpan expr)
            assertSlice sql (locSpan expr) "a /* mid */ +  b"
        , testCase "UTF-8 trivia and literals keep byte-accurate spans" $ do
            let lead = TE.encodeUtf8 (T.pack "-- 注释\n")
                sql = lead <> TE.encodeUtf8 (T.pack "SELECT 'é' AS x;")
                expectedStmt = TE.encodeUtf8 (T.pack "SELECT 'é' AS x")
            stmt <- parseSingle sqliteProfile sql
            assertEqual "statement byte offset after comment" (BS.length lead) (spanStart (locSpan stmt))
            assertEqual
                "statement byte length"
                (BS.length expectedStmt)
                (spanEnd (locSpan stmt) - spanStart (locSpan stmt))
            assertSlice sql (locSpan stmt) expectedStmt
            expr <- singleSelectExpr sqliteProfile sql
            case locValue expr of
                ExprLiteral (Literal LitString raw) -> do
                    assertEqual "string literal raw bytes" (TE.encodeUtf8 (T.pack "'é'")) raw
                    assertSlice sql (locSpan expr) (TE.encodeUtf8 (T.pack "'é'"))
                other -> assertFailure ("expected a string literal, got " <> show other)
        , testCase "nested parenthesized expression spans are exact" $ do
            let sql = BSC.pack "SELECT ((a)) FROM t;"
            outer <- singleSelectExpr sqliteProfile sql
            assertEqual "outer paren span" (ByteSpan 7 12) (locSpan outer)
            assertSlice sql (locSpan outer) "((a))"
            case locValue outer of
                ExprParen inner -> do
                    assertEqual "inner paren span" (ByteSpan 8 11) (locSpan inner)
                    assertSlice sql (locSpan inner) "(a)"
                    case locValue inner of
                        ExprParen column -> do
                            assertEqual "column span" (ByteSpan 9 10) (locSpan column)
                            assertSlice sql (locSpan column) "a"
                        other -> assertFailure ("expected nested paren, got " <> show other)
                other -> assertFailure ("expected paren expression, got " <> show other)
        , testCase "casts and subscripts end at their own closing token" $ do
            let sql = BSC.pack "SELECT (a + b)::int, arr[1:2] FROM t;"
            core <- parseSelect postgresqlProfile sql
            case selectItems core of
                [firstItem, secondItem] -> do
                    let firstExpr = selectItemExpr (locValue firstItem)
                    assertEqual "cast span" (ByteSpan 7 19) (locSpan firstExpr)
                    assertSlice sql (locSpan firstExpr) "(a + b)::int"
                    let secondExpr = selectItemExpr (locValue secondItem)
                    assertEqual "subscript span" (ByteSpan 21 29) (locSpan secondExpr)
                    assertSlice sql (locSpan secondExpr) "arr[1:2]"
                items -> assertFailure ("expected two projection items, got " <> show (length items))
        , testCase "multi-statement script spans stay inside their own statements" $ do
            let sql =
                    BSC.pack
                        "-- head\nCREATE TABLE t (a INTEGER);\nINSERT INTO t VALUES (1); -- tail\nSELECT /*x*/ a FROM t;"
            stmts <- parseStatements sqliteProfile sql
            case stmts of
                [createStmt, insertStmt, selectStmt] -> do
                    assertEqual "CREATE span" (ByteSpan 8 34) (locSpan createStmt)
                    assertSlice sql (locSpan createStmt) "CREATE TABLE t (a INTEGER)"
                    assertEqual "INSERT span" (ByteSpan 36 60) (locSpan insertStmt)
                    assertSlice sql (locSpan insertStmt) "INSERT INTO t VALUES (1)"
                    assertEqual "SELECT span" (ByteSpan 70 91) (locSpan selectStmt)
                    assertSlice sql (locSpan selectStmt) "SELECT /*x*/ a FROM t"
                    case locValue createStmt of
                        StatementCreate create ->
                            case locValue (createObject (locValue create)) of
                                CreateTableObject table ->
                                    assertEqual
                                        "created table name"
                                        "t"
                                        (namePartRaw (headNamePart (createTableName table)))
                                other -> assertFailure ("expected CREATE TABLE, got " <> show other)
                        other -> assertFailure ("expected CREATE statement, got " <> show other)
                _ -> assertFailure ("expected three statements, got " <> show (length stmts))
        ]
    where
        headNamePart (L _ (Name (L _ part : _))) = part
        headNamePart _ = error "headNamePart: empty name"

-- ---------------------------------------------------------------------------
-- Precedence and associativity
-- ---------------------------------------------------------------------------

expressionTests :: TestTree
expressionTests =
    testGroup
        "expression precedence and associativity"
        [ testCase "OR binds less tightly than AND" $ do
            expr <- singleSelectExpr postgresqlProfile (BSC.pack "SELECT a OR b AND c FROM t;")
            case locValue expr of
                ExprBinary OpOr left right -> do
                    assertColumn "a" left
                    case locValue right of
                        ExprBinary OpAnd mid innerRight -> do
                            assertColumn "b" mid
                            assertColumn "c" innerRight
                        other -> assertFailure ("expected AND on the right of OR, got " <> show other)
                other -> assertFailure ("expected OR at the root, got " <> show other)
        , testCase "multiplication binds more tightly than addition" $ do
            expr <- singleSelectExpr postgresqlProfile (BSC.pack "SELECT a + b * c FROM t;")
            case locValue expr of
                ExprBinary OpAdd left right -> do
                    assertColumn "a" left
                    case locValue right of
                        ExprBinary OpMul mid innerRight -> do
                            assertColumn "b" mid
                            assertColumn "c" innerRight
                        other -> assertFailure ("expected multiplication on the right, got " <> show other)
                other -> assertFailure ("expected addition at the root, got " <> show other)
        , testCase "NOT binds less tightly than comparison" $ do
            expr <- singleSelectExpr postgresqlProfile (BSC.pack "SELECT NOT a = b FROM t;")
            case locValue expr of
                ExprUnary UnaryNot inner ->
                    case locValue inner of
                        ExprBinary OpEq left right -> assertColumn "a" left >> assertColumn "b" right
                        other -> assertFailure ("expected NOT applied to equality, got " <> show other)
                other -> assertFailure ("expected unary NOT at the root, got " <> show other)
        , testCase "unary minus applies to the whole :: cast" $ do
            expr <- singleSelectExpr postgresqlProfile (BSC.pack "SELECT -x::int FROM t;")
            case locValue expr of
                ExprUnary UnaryNegate inner ->
                    case locValue inner of
                        ExprCast CastDoubleColon operand typeName -> do
                            assertColumn "x" operand
                            case typeNameParts (locValue typeName) of
                                [L _ part] -> assertEqual "cast type" "int" (namePartRaw part)
                                parts -> assertFailure ("expected one type name part, got " <> show (length parts))
                        other -> assertFailure ("expected a cast under unary minus, got " <> show other)
                other -> assertFailure ("expected unary minus at the root, got " <> show other)
        , testCase "BETWEEN consumes its own AND without swallowing the outer conjunction" $ do
            expr <- singleSelectExpr postgresqlProfile (BSC.pack "SELECT a BETWEEN 1 AND 2 AND b FROM t;")
            case locValue expr of
                ExprBinary OpAnd left right -> do
                    case locValue left of
                        ExprBetween False subject low high -> do
                            assertColumn "a" subject
                            assertNumberLiteral "1" low
                            assertNumberLiteral "2" high
                        other -> assertFailure ("expected BETWEEN as the left conjunct, got " <> show other)
                    assertColumn "b" right
                other -> assertFailure ("expected AND at the root, got " <> show other)
        , testCase "IN list parses values as operands" $ do
            expr <- singleSelectExpr postgresqlProfile (BSC.pack "SELECT a IN (1,2,3) FROM t;")
            case locValue expr of
                ExprInList False subject items -> do
                    assertColumn "a" subject
                    assertEqual "IN list length" 3 (length items)
                other -> assertFailure ("expected IN list, got " <> show other)
        , testCase "arithmetic is left associative" $ do
            expr <- singleSelectExpr postgresqlProfile (BSC.pack "SELECT a - b - c FROM t;")
            case locValue expr of
                ExprBinary OpSub left right -> do
                    case locValue left of
                        ExprBinary OpSub a b -> assertColumn "a" a >> assertColumn "b" b
                        other -> assertFailure ("expected left-nested subtraction, got " <> show other)
                    assertColumn "c" right
                other -> assertFailure ("expected subtraction at the root, got " <> show other)
        , testCase "IS NOT NULL and IS DISTINCT FROM are predicates, not binary tokens" $ do
            core <-
                parseSelect postgresqlProfile (BSC.pack "SELECT a IS NOT NULL, b IS DISTINCT FROM c FROM t;")
            case selectItems core of
                [firstItem, secondItem] -> do
                    case locValue (selectItemExpr (locValue firstItem)) of
                        ExprIsNull True subject -> assertColumn "a" subject
                        other -> assertFailure ("expected IS NOT NULL, got " <> show other)
                    case locValue (selectItemExpr (locValue secondItem)) of
                        ExprIsDistinct False left right -> assertColumn "b" left >> assertColumn "c" right
                        other -> assertFailure ("expected IS DISTINCT FROM, got " <> show other)
                items -> assertFailure ("expected two projection items, got " <> show (length items))
        ]

-- ---------------------------------------------------------------------------
-- Error spans
-- ---------------------------------------------------------------------------

errorTests :: TestTree
errorTests =
    testGroup
        "spanned errors"
        [ testCase "SELECT FROM points at FROM" $ do
            let sql = BSC.pack "SELECT FROM"
            case parseScript sqliteProfile sql of
                Left err -> do
                    assertEqual "kind" ParseUnexpectedToken (parseErrorKind err)
                    assertEqual "span" (ByteSpan 7 11) (parseErrorSpan err)
                    assertSlice sql (parseErrorSpan err) "FROM"
                Right _ -> assertFailure "expected SELECT FROM to fail"
        , testCase "a dangling token points at the token" $ do
            let sql = BSC.pack "SELECT 1 2"
            case parseScript sqliteProfile sql of
                Left err -> do
                    assertEqual "kind" ParseUnexpectedToken (parseErrorKind err)
                    assertEqual "span" (ByteSpan 9 10) (parseErrorSpan err)
                    assertSlice sql (parseErrorSpan err) "2"
                    assertBool
                        "message mentions the trailing token"
                        ("trailing" `T.isInfixOf` parseErrorMessage err)
                Right _ -> assertFailure "expected a dangling-token error"
        , testCase "missing FROM target reports end of input at a zero-width span" $ do
            let sql = BSC.pack "SELECT * FROM"
            case parseScript sqliteProfile sql of
                Left err -> do
                    assertEqual "kind" ParseUnexpectedEnd (parseErrorKind err)
                    assertEqual "span" (ByteSpan 13 13) (parseErrorSpan err)
                Right _ -> assertFailure "expected a missing-target error"
        , testCase "unterminated literal is a lexical error with the opening offset" $ do
            let sql = BSC.pack "SELECT 'unterminated"
            case parseScript sqliteProfile sql of
                Left err -> do
                    assertEqual "kind" ParseLexicalError (parseErrorKind err)
                    assertEqual "span" (ByteSpan 7 7) (parseErrorSpan err)
                Right _ -> assertFailure "expected an unterminated literal error"
        , testCase "invalid UTF-8 is a lexical error at the invalid byte" $ do
            let sql = BS.pack [0x53, 0x45, 0x4c, 0x45, 0x43, 0x54, 0x20, 0xff]
            case parseScript sqliteProfile sql of
                Left err -> do
                    assertEqual "kind" ParseLexicalError (parseErrorKind err)
                    assertEqual "offset" (ByteSpan 7 7) (parseErrorSpan err)
                Right _ -> assertFailure "expected invalid UTF-8 to fail"
        , testCase "raw garbage after SELECT reports the projection error at FROM" $ do
            let sql = BSC.pack "SELECT FROM WHERE SYNTAX;"
            case parseScript sqliteProfile sql of
                Left err -> do
                    assertEqual "kind" ParseUnexpectedToken (parseErrorKind err)
                    assertEqual "span" (ByteSpan 7 11) (parseErrorSpan err)
                    assertSlice sql (parseErrorSpan err) "FROM"
                Right _ -> assertFailure "expected raw garbage to fail"
        , testCase "unknown statement heads are rejected, not swallowed" $ do
            let sql = BSC.pack "FROBNICATE t;"
            case parseScript sqliteProfile sql of
                Left err -> do
                    assertEqual "kind" ParseUnsupported (parseErrorKind err)
                    assertEqual "span" (ByteSpan 0 10) (parseErrorSpan err)
                Right _ -> assertFailure "expected an unsupported-statement error"
        ]

-- ---------------------------------------------------------------------------
-- Dialect accept / reject
-- ---------------------------------------------------------------------------

dialectTests :: TestTree
dialectTests =
    testGroup
        "dialect strictness"
        [ testGroup
            "accepted forms"
            [ acceptCase
                "PostgreSQL CREATE STATISTICS"
                postgresqlProfile
                "CREATE STATISTICS s (ndistinct) ON a, b FROM t;"
            , acceptCase
                "PostgreSQL DROP CONVERSION"
                postgresqlProfile
                "DROP CONVERSION c;"
            , acceptCase
                "PostgreSQL multiword type with arguments"
                postgresqlProfile
                "SELECT CAST(x AS CHARACTER VARYING(10)), y::double precision, z::int[] FROM t;"
            , acceptCase
                "PostgreSQL standalone TABLE query"
                postgresqlProfile
                "TABLE t ORDER BY a;"
            , acceptCase
                "SQLite bare PRAGMA value with schema qualification"
                sqliteProfile
                "PRAGMA main.foreign_keys = ON;"
            , acceptCase
                "SQLite schema-qualified VACUUM ... INTO"
                sqliteProfile
                "VACUUM main INTO 'backup.db';"
            , acceptCase
                "SQLite CREATE VIRTUAL TABLE"
                sqliteProfile
                "CREATE VIRTUAL TABLE t USING fts5(content, tokenize = 'porter');"
            , acceptCase
                "SQLite RETURNING"
                sqliteProfile
                "CREATE TEMP TABLE t (a INT); INSERT INTO t VALUES (1) RETURNING a;"
            , acceptCase
                "SQLite UPDATE ... FROM"
                sqliteProfile
                "CREATE TEMP TABLE t (a INT); CREATE TEMP TABLE u (a INT); UPDATE t SET a = u.a FROM u WHERE t.a = u.a;"
            , acceptCase
                "SQLite ALTER TABLE DROP COLUMN"
                sqliteProfile
                "CREATE TEMP TABLE t (a INT, b TEXT); ALTER TABLE t DROP COLUMN b;"
            , acceptCase
                "SQLite ALTER TABLE DROP without COLUMN"
                sqliteProfile
                "CREATE TEMP TABLE t (a INT, b TEXT); ALTER TABLE t DROP b;"
            , acceptCase
                "SQLite ALTER TABLE ADD and RENAME without COLUMN"
                sqliteProfile
                "CREATE TEMP TABLE t (a INT); ALTER TABLE t ADD b TEXT; ALTER TABLE t RENAME b TO c; ALTER TABLE t RENAME TO t2;"
            , acceptCase
                "SQLite STRICT table"
                sqliteProfile
                "CREATE TABLE t (a INT) STRICT;"
            , acceptCase
                "SQLite MATERIALIZED CTE hint"
                sqliteProfile
                "WITH c AS MATERIALIZED (SELECT 1 AS a) SELECT a FROM c;"
            , acceptCase
                "SQLite RIGHT and FULL JOIN"
                sqliteProfile
                "SELECT * FROM t RIGHT JOIN u ON t.a = u.a; SELECT * FROM t FULL JOIN u ON t.a = u.a;"
            , acceptCase
                "MySQL BINARY column attribute and index prefix length"
                mysqlProfile
                "CREATE TABLE t (b VARCHAR(10) BINARY, KEY idx (b(5)));"
            , acceptCase
                "MySQL multi-table ANALYZE"
                mysqlProfile
                "ANALYZE TABLE a, b;"
            , acceptCase
                "MySQL FULLTEXT index"
                mysqlProfile
                "CREATE FULLTEXT INDEX fi ON t (c);"
            , acceptCase
                "MySQL INTERSECT and EXCEPT"
                mysqlProfile
                "SELECT 1 INTERSECT SELECT 1; SELECT 1 EXCEPT SELECT 2;"
            , acceptCase
                "MySQL ALTER TABLE ADD COLUMN with parenthesized definitions"
                mysqlProfile
                "ALTER TABLE t ADD COLUMN (a INT, b TEXT);"
            , acceptCase
                "MySQL ALTER TABLE DROP INDEX"
                mysqlProfile
                "ALTER TABLE t DROP INDEX i;"
            , acceptCase
                "MySQL PREPARE ... FROM string"
                mysqlProfile
                "PREPARE s FROM 'SELECT 1';"
            , acceptCase
                "MariaDB PREPARE ... FROM string"
                mariadbProfile
                "PREPARE s FROM 'SELECT 1';"
            , acceptCase
                "PostgreSQL CREATE POLICY"
                postgresqlProfile
                "CREATE POLICY p ON t AS RESTRICTIVE FOR SELECT TO r USING (true) WITH CHECK (true);"
            , acceptCase
                "PostgreSQL CREATE RULE"
                postgresqlProfile
                "CREATE RULE r AS ON UPDATE TO t WHERE a > 0 DO INSTEAD NOTHING;"
            , acceptCase
                "PostgreSQL CREATE SEQUENCE"
                postgresqlProfile
                "CREATE SEQUENCE s START WITH 1 INCREMENT BY 2;"
            , acceptCase
                "PostgreSQL CREATE FUNCTION with opaque body"
                postgresqlProfile
                "CREATE FUNCTION f(a int) RETURNS int AS $$ SELECT 1 $$ LANGUAGE sql;"
            , acceptCase
                "PostgreSQL MERGE"
                postgresqlProfile
                "MERGE INTO t USING u ON t.id = u.id WHEN MATCHED THEN UPDATE SET a = 1 WHEN NOT MATCHED THEN INSERT (a) VALUES (1);"
            , acceptCase
                "PostgreSQL CREATE OR REPLACE TRIGGER"
                postgresqlProfile
                "CREATE OR REPLACE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE f();"
            , acceptCase
                "DuckDB MERGE"
                duckdbProfile
                "MERGE INTO t USING u ON t.id = u.id WHEN MATCHED THEN UPDATE SET a = 1 WHEN NOT MATCHED THEN INSERT (a) VALUES (1);"
            , acceptCase
                "DuckDB CREATE MACRO"
                duckdbProfile
                "CREATE MACRO add(a, b := 1) AS a + b;"
            , acceptCase
                "DuckDB PIVOT"
                duckdbProfile
                "PIVOT t ON a IN (1, 2) GROUP BY b;"
            , acceptCase
                "DuckDB UNPIVOT"
                duckdbProfile
                "UNPIVOT t ON a INTO NAME n VALUE v;"
            , acceptCase
                "DuckDB ASOF RIGHT JOIN"
                duckdbProfile
                "SELECT * FROM t ASOF RIGHT JOIN u ON t.a = u.a;"
            , acceptCase
                "DuckDB E'...' escape literal"
                duckdbProfile
                "SELECT E'a\\n';"
            , acceptCase
                "DuckDB parenthesized FROM (VALUES ...) with WHERE"
                duckdbProfile
                "SELECT * FROM (VALUES (1),(2)) AS v(x) WHERE x > 1;"
            , acceptCase
                "ClickHouse CREATE FUNCTION with opaque body"
                clickhouseProfile
                "CREATE FUNCTION f AS (x) -> x + 1;"
            , acceptCase
                "ClickHouse materialized view ENGINE and TO"
                clickhouseProfile
                "CREATE MATERIALIZED VIEW mv ENGINE = MergeTree() ORDER BY id TO t AS SELECT 1;"
            , acceptCase
                "ClickHouse materialized view SETTINGS before AS"
                clickhouseProfile
                "CREATE MATERIALIZED VIEW mv ENGINE = MergeTree() SETTINGS index_granularity = 8192 AS SELECT 1;"
            , acceptCase
                "ClickHouse ALTER TABLE ... MATERIALIZE TTL"
                clickhouseProfile
                "ALTER TABLE t MATERIALIZE TTL;"
            , acceptCase
                "ClickHouse ATTACH and DETACH"
                clickhouseProfile
                "DETACH TABLE IF EXISTS t;"
            , acceptCase
                "ClickHouse query SETTINGS"
                clickhouseProfile
                "SELECT 1 SETTINGS max_threads = 1;"
            , acceptCase
                "ClickHouse Array(JSON(...)) type"
                clickhouseProfile
                "CREATE TABLE t (a Array(JSON(max_dynamic_paths=100)));"
            , acceptCase
                "ClickHouse data.:Int64 typed path"
                clickhouseProfile
                "SELECT data.:Int64 FROM t;"
            , acceptCase
                "ClickHouse ARRAY JOIN []"
                clickhouseProfile
                "SELECT * FROM t ARRAY JOIN [];"
            , acceptCase
                "ClickHouse two-arg CAST and lambda"
                clickhouseProfile
                "SELECT arrayMap(x -> CAST(x, 'Int64'), arr) FROM t;"
            , acceptCase
                "ClickHouse OPTIMIZE TABLE FINAL and DETACH SYNC"
                clickhouseProfile
                "OPTIMIZE TABLE t FINAL; DETACH TABLE t SYNC;"
            , acceptCase
                "PostgreSQL empty column list CREATE TABLE, RLS, PARTITION BY/OF, TABLESAMPLE, NOTIFY, GROUPING SETS"
                postgresqlProfile
                "CREATE TEMP TABLE empty_t (); ALTER TABLE v0 NO FORCE ROW LEVEL SECURITY; CREATE TABLE p (a INT) PARTITION BY RANGE (a); CREATE TABLE p1 PARTITION OF p FOR VALUES FROM (1) TO (10); SELECT * FROM p TABLESAMPLE BERNOULLI (10) GROUP BY GROUPING SETS ((a), ROLLUP (a)); NOTIFY chan;"
            , acceptCase
                "ClickHouse GROUPING SETS, ROLLUP, and CUBE"
                clickhouseProfile
                "SELECT a FROM t GROUP BY GROUPING SETS ((a), ()), ROLLUP (a), CUBE (a);"
            , acceptCase
                "SQLite empty IN () and REPLACE INTO"
                sqliteProfile
                "SELECT 1 IN (); REPLACE INTO t VALUES (1);"
            , acceptCase
                "MySQL charset introducer, SET NAMES, ALGORITHM VIEW, RENAME TABLE, USER VAR :=, CHAR ARRAY"
                mysqlProfile
                "SET NAMES 'utf8mb4'; CREATE ALGORITHM = TEMPTABLE VIEW v AS SELECT _utf32 'a'; RENAME TABLE t1 TO t2; SELECT CAST(j AS CHAR(64) ARRAY), (@x := 1) FROM t;"
            , acceptCase
                "MariaDB USING HASH index and ALTER ALGORITHM=INPLACE"
                mariadbProfile
                "CREATE UNIQUE INDEX i1 USING HASH ON t (c ASC); ALTER TABLE t ADD d INT, ALGORITHM=INPLACE;"
            , acceptCase
                "DuckDB TEMP VIEW, TEMP SEQUENCE, OR REPLACE MACRO, POSITION(... IN ...)"
                duckdbProfile
                "CREATE TEMP VIEW v AS SELECT 1; CREATE TEMP SEQUENCE s; CREATE OR REPLACE MACRO f(x) AS x + 1; SELECT position('a' IN 'bar');"
            , acceptCase
                "MariaDB standalone TABLE query"
                mariadbProfile
                "TABLE t ORDER BY a;"
            , acceptCase
                "DuckDB parenthesized view body"
                duckdbProfile
                "CREATE VIEW v AS (SELECT 1);"
            , acceptCase
                "DuckDB ATTACH IF NOT EXISTS"
                duckdbProfile
                "ATTACH IF NOT EXISTS 'file.db' AS file_db;"
            , acceptCase
                "DuckDB trailing comma in SELECT list"
                duckdbProfile
                "SELECT a, FROM t;"
            , acceptCase
                "MySQL ALTER TABLE ADD INDEX with name"
                mysqlProfile
                "ALTER TABLE t ADD INDEX idx (a);"
            , acceptCase
                "SQLite LIMIT offset, count"
                sqliteProfile
                "SELECT 1 LIMIT 1, 2;"
            , acceptCase
                "PostgreSQL FETCH FIRST"
                postgresqlProfile
                "SELECT 1 FETCH FIRST 1 ROW ONLY;"
            , acceptCase
                "ClickHouse DISTINCT ON"
                clickhouseProfile
                "SELECT DISTINCT ON (a) a FROM t;"
            , acceptCase
                "ClickHouse CTE column lists"
                clickhouseProfile
                "WITH cte(n) AS (SELECT 1) SELECT n FROM cte;"
            , acceptCase
                "ClickHouse trailing comma in SELECT list"
                clickhouseProfile
                "SELECT a, FROM t;"
            , acceptCase
                "MySQL INSERT ... VALUES ROW(...)"
                mysqlProfile
                "INSERT INTO t VALUES ROW(1), ROW(2);"
            , acceptCase
                "PostgreSQL GENERATED AS IDENTITY with sequence options"
                postgresqlProfile
                "CREATE TABLE t (id BIGINT GENERATED ALWAYS AS IDENTITY (START WITH 10 INCREMENT BY 2) PRIMARY KEY);"
            , acceptCase
                "DuckDB dynamic PIVOT without IN list"
                duckdbProfile
                "PIVOT t ON quarter USING sum(amount) GROUP BY region;"
            , acceptCase
                "ClickHouse LEFT ASOF JOIN"
                clickhouseProfile
                "SELECT * FROM a LEFT ASOF JOIN b ON a.k = b.k AND a.ts >= b.ts;"
            , acceptCase
                "MySQL PREPARE FROM string literal and user variable"
                mysqlProfile
                "PREPARE s1 FROM 'SELECT 1'; PREPARE s2 FROM @q;"
            ]
        , testGroup
            "rejected forms"
            [ rejectCase
                "PostgreSQL rejects CREATE TEMP POLICY"
                postgresqlProfile
                "CREATE TEMP POLICY p ON t;"
                ParseUnsupported
                (Just (ByteSpan 7 11))
            , rejectCase
                "SQLite rejects CREATE OR REPLACE VIEW"
                sqliteProfile
                "CREATE OR REPLACE VIEW v AS SELECT 1;"
                ParseUnsupported
                Nothing
            , rejectCase
                "PostgreSQL rejects CREATE OR REPLACE TABLE"
                postgresqlProfile
                "CREATE OR REPLACE TABLE t (a INT);"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects DROP ... CASCADE"
                sqliteProfile
                "DROP TABLE IF EXISTS t CASCADE;"
                ParseUnsupported
                Nothing
            , rejectCase
                "DuckDB rejects DROP TRIGGER"
                duckdbProfile
                "DROP TRIGGER IF EXISTS tr;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects DROP FUNCTION"
                sqliteProfile
                "DROP FUNCTION IF EXISTS f;"
                ParseUnsupported
                Nothing
            , rejectCase
                "DuckDB rejects empty IN ()"
                duckdbProfile
                "SELECT 1 IN ();"
                ParseUnsupported
                Nothing
            , rejectCase
                "DuckDB rejects empty CREATE TABLE t ()"
                duckdbProfile
                "CREATE TABLE t ();"
                ParseUnsupported
                Nothing
            , rejectCase
                "DuckDB rejects TRUNCATE ... RESTART IDENTITY"
                duckdbProfile
                "TRUNCATE TABLE t RESTART IDENTITY;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects SHOW TABLES"
                sqliteProfile
                "SHOW TABLES;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects TABLESAMPLE"
                sqliteProfile
                "SELECT a FROM t TABLESAMPLE SYSTEM (10);"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects GROUPING SETS"
                sqliteProfile
                "SELECT a FROM t GROUP BY GROUPING SETS ((a), ());"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects ROLLUP"
                sqliteProfile
                "SELECT a FROM t GROUP BY ROLLUP (a);"
                ParseUnsupported
                Nothing
            , rejectCase
                "MySQL rejects GROUPING SETS"
                mysqlProfile
                "SELECT a FROM t GROUP BY GROUPING SETS ((a), ());"
                ParseUnsupported
                Nothing
            , rejectCase
                "MariaDB rejects CUBE"
                mariadbProfile
                "SELECT a FROM t GROUP BY CUBE (a);"
                ParseUnsupported
                Nothing
            , rejectCase
                "DuckDB rejects SAVEPOINT"
                duckdbProfile
                "SAVEPOINT s;"
                ParseUnsupported
                (Just (ByteSpan 0 9))
            , rejectCase
                "DuckDB rejects FOREIGN KEY ... ON DELETE CASCADE"
                duckdbProfile
                "CREATE TABLE t (a INT, FOREIGN KEY (a) REFERENCES u(a) ON DELETE CASCADE);"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects MySQL ENGINE table options"
                sqliteProfile
                "CREATE TABLE t (a INT) ENGINE=InnoDB;"
                ParseUnsupported
                Nothing
            , rejectCase
                "MySQL rejects ClickHouse-style view ENGINE/TO"
                mysqlProfile
                "CREATE VIEW v TO x AS SELECT 1;"
                ParseUnsupported
                Nothing
            , rejectCase
                "MySQL rejects materialized views"
                mysqlProfile
                "CREATE MATERIALIZED VIEW mv AS SELECT 1;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects CREATE STATISTICS"
                sqliteProfile
                "CREATE STATISTICS s ON a FROM t;"
                ParseUnsupported
                Nothing
            , rejectCase
                "PostgreSQL rejects CREATE VIRTUAL TABLE"
                postgresqlProfile
                "CREATE VIRTUAL TABLE t USING fts5(a);"
                ParseUnsupported
                Nothing
            , rejectCase
                "PostgreSQL rejects PIVOT"
                postgresqlProfile
                "PIVOT t ON a;"
                ParseUnsupported
                Nothing
            , rejectCase
                "MySQL rejects RETURNING"
                mysqlProfile
                "INSERT INTO t VALUES (1) RETURNING a;"
                ParseUnsupported
                Nothing
            , rejectCase
                "MySQL rejects CREATE SEQUENCE"
                mysqlProfile
                "CREATE SEQUENCE s;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects MERGE"
                sqliteProfile
                "MERGE INTO t USING u ON t.a = u.a WHEN MATCHED THEN DELETE;"
                ParseUnsupported
                Nothing
            , rejectCase
                "ClickHouse rejects SAVEPOINT"
                clickhouseProfile
                "SAVEPOINT s;"
                ParseUnsupported
                (Just (ByteSpan 0 9))
            , rejectCase
                "MySQL requires ROW(...) in VALUES statements"
                mysqlProfile
                "VALUES (1), (2);"
                ParseUnexpectedToken
                Nothing
            , rejectCase
                "MySQL rejects CREATE OR REPLACE PROCEDURE"
                mysqlProfile
                "CREATE OR REPLACE PROCEDURE p() BEGIN SELECT 1; END;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects CREATE INDEX ... INCLUDE"
                sqliteProfile
                "CREATE TABLE t (a INT, b INT); CREATE INDEX i ON t (a) INCLUDE (b);"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects ALTER TABLE ... ADD CONSTRAINT"
                sqliteProfile
                "CREATE TABLE t (a INT); ALTER TABLE t ADD CONSTRAINT uq UNIQUE (a);"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects FOR UPDATE"
                sqliteProfile
                "SELECT * FROM t FOR UPDATE;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects LATERAL"
                sqliteProfile
                "SELECT * FROM t, LATERAL (SELECT 1) AS x;"
                ParseUnsupported
                Nothing
            , rejectCase
                "MariaDB rejects LATERAL"
                mariadbProfile
                "SELECT * FROM t, LATERAL (SELECT 1) AS x;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects ClickHouse FINAL"
                sqliteProfile
                "SELECT * FROM t FINAL;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects ClickHouse GLOBAL JOIN"
                sqliteProfile
                "SELECT * FROM t GLOBAL JOIN u ON t.a = u.a;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects DELETE ... USING"
                sqliteProfile
                "DELETE FROM t USING u;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects POSITION(... IN ...)"
                sqliteProfile
                "SELECT position('a' IN 'bar');"
                ParseUnsupported
                Nothing
            , rejectCase
                "DuckDB rejects a dangling SELECT-list comma"
                duckdbProfile
                "SELECT a,"
                ParseUnexpectedEnd
                (Just (ByteSpan 9 9))
            , rejectCase
                "PostgreSQL rejects LIMIT offset, count"
                postgresqlProfile
                "SELECT 1 LIMIT 1, 2;"
                ParseUnsupported
                Nothing
            , rejectCase
                "SQLite rejects FETCH FIRST"
                sqliteProfile
                "SELECT 1 FETCH FIRST 1 ROW ONLY;"
                ParseUnsupported
                Nothing
            , rejectCase
                "MySQL rejects unnamed CREATE INDEX"
                mysqlProfile
                "CREATE INDEX ON t (a);"
                ParseUnsupported
                Nothing
            , rejectCase
                "PostgreSQL rejects a trailing SELECT-list comma"
                postgresqlProfile
                "SELECT a, FROM t;"
                ParseUnexpectedToken
                Nothing
            , rejectCase
                "SQLite rejects ON CONFLICT ON CONSTRAINT"
                sqliteProfile
                "INSERT INTO t (a) VALUES (1) ON CONFLICT ON CONSTRAINT pk DO NOTHING;"
                ParseUnsupported
                Nothing
            , rejectCase
                "DuckDB rejects ON CONFLICT ON CONSTRAINT"
                duckdbProfile
                "INSERT INTO t (a) VALUES (1) ON CONFLICT ON CONSTRAINT pk DO NOTHING;"
                ParseUnsupported
                Nothing
            , rejectCase
                "MySQL rejects PREPARE FROM general expression"
                mysqlProfile
                "PREPARE s FROM CONCAT('SELECT ', 1);"
                ParseUnsupported
                Nothing
            , rejectCase
                "MariaDB rejects multi-table DELETE ... USING ... RETURNING"
                mariadbProfile
                "DELETE FROM a USING b WHERE a.id = b.id RETURNING id;"
                ParseUnsupported
                Nothing
            , rejectCase
                "ClickHouse rejects ASOF RIGHT JOIN"
                clickhouseProfile
                "SELECT * FROM a ASOF RIGHT JOIN b ON a.ts >= b.ts;"
                ParseUnsupported
                Nothing
            , rejectCase
                "ClickHouse rejects LATERAL subquery"
                clickhouseProfile
                "SELECT * FROM t, LATERAL (SELECT 1) AS x;"
                ParseUnsupported
                Nothing
            , rejectCase
                "PostgreSQL rejects ClickHouse lambda CREATE FUNCTION"
                postgresqlProfile
                "CREATE FUNCTION f AS (x) -> x * 2;"
                ParseUnexpectedToken
                Nothing
            , rejectCase
                "MariaDB rejects MySQL-only SECONDARY_ENGINE table option"
                mariadbProfile
                "CREATE TABLE t (id INT) ENGINE=InnoDB SECONDARY_ENGINE=NULL;"
                ParseUnsupported
                Nothing
            , acceptCase
                "MySQL, MariaDB, and ClickHouse accept DROP TEMPORARY TABLE"
                mysqlProfile
                "DROP TEMPORARY TABLE IF EXISTS t;"
            , acceptCase
                "MariaDB accepts DROP TEMPORARY SEQUENCE"
                mariadbProfile
                "DROP TEMPORARY SEQUENCE IF EXISTS s;"
            , acceptCase
                "ClickHouse accepts DROP TEMPORARY VIEW"
                clickhouseProfile
                "DROP TEMPORARY VIEW IF EXISTS v;"
            , rejectCase
                "MySQL rejects DROP TEMPORARY VIEW"
                mysqlProfile
                "DROP TEMPORARY VIEW IF EXISTS v;"
                ParseUnsupported
                Nothing
            , rejectCase
                "PostgreSQL rejects DROP TEMPORARY TABLE"
                postgresqlProfile
                "DROP TEMPORARY TABLE IF EXISTS t;"
                ParseUnsupported
                Nothing
            ]
        ]
    where
        acceptCase :: String -> DialectProfile -> String -> TestTree
        acceptCase name profile sql =
            testCase name $ do
                stmts <- parseStatements profile (BSC.pack sql)
                assertBool (name <> ": expected a parsed statement") (not (null stmts))

        rejectCase ::
            String ->
            DialectProfile ->
            String ->
            ParseErrorKind ->
            Maybe ByteSpan ->
            TestTree
        rejectCase name profile sql expectedKind expectedSpan =
            testCase name $
                case parseScript profile (BSC.pack sql) of
                    Left err -> do
                        assertEqual (name <> ": kind") expectedKind (parseErrorKind err)
                        case expectedSpan of
                            Just sp -> assertEqual (name <> ": span") sp (parseErrorSpan err)
                            Nothing ->
                                assertBool
                                    (name <> ": error span must cover a token")
                                    (spanStart (parseErrorSpan err) < spanEnd (parseErrorSpan err))
                    Right _ -> assertFailure (name <> ": expected a spanned parse error")

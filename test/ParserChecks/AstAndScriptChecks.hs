-- | Dialect-specific AST shape, regression, script, and corpus parser checks.
module ParserChecks.AstAndScriptChecks (
    dialectAstTests,
    reviewedRegressions,
    grammarGapTests,
    scriptTests,
    realFuzzCorpusTests,
) where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.List (isPrefixOf, sort)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import FrontendAstInvariants (validateStatementInvariants)
import Occam.Frontend.Parser
import Occam.Frontend.Span (ByteSpan (..), Located (..))
import Occam.Frontend.Splitter (splitSQL)
import Occam.Parser.Dialect (
    DialectFeature (..),
    DialectProfile (..),
    clickhouseProfile,
    duckdbProfile,
    mariadbProfile,
    mysqlProfile,
    postgresqlProfile,
    sqliteProfile,
 )
import ParserChecks.Support
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- ---------------------------------------------------------------------------
-- Dialect-specific AST shapes
-- ---------------------------------------------------------------------------

dialectAstTests :: TestTree
dialectAstTests =
    testGroup
        "dialect-specific AST shapes"
        [ testCase "MySQL index prefix is a structured IndexKeyPart, not a function call" $ do
            stmt <-
                parseSingle
                    mysqlProfile
                    (BSC.pack "CREATE TABLE t (b VARCHAR(10) BINARY, KEY idx (b(5)));")
            case stmt of
                L _ (StatementCreate create) ->
                    case locValue (createObject (locValue create)) of
                        CreateTableObject table ->
                            case createTableElements table of
                                [L _ (TableElementColumn column), L _ (TableElementConstraint constraint)] -> do
                                    let columnDef = locValue column
                                    assertBool
                                        "column BINARY attribute"
                                        (any (isBinaryConstraint . locValue) (columnConstraints columnDef))
                                    case columnType columnDef of
                                        Just (L _ typeName) ->
                                            assertEqual
                                                "VARCHAR argument count"
                                                1
                                                (length (typeNameArgs typeName))
                                        Nothing -> assertFailure "expected a VARCHAR column type"
                                    case locValue constraint of
                                        TableConstraintIndex _ keys ->
                                            case keys of
                                                [L _ key] -> do
                                                    case locValue (indexKeyExpr key) of
                                                        ExprColumn (L _ (Name [L _ part])) ->
                                                            assertEqual "indexed column" "b" (namePartRaw part)
                                                        other ->
                                                            assertFailure
                                                                ("expected index column expression, got " <> show other)
                                                    case indexKeyPrefixLength key of
                                                        Just prefixExpr -> assertNumberLiteral "5" prefixExpr
                                                        Nothing -> assertFailure "expected a structured prefix length"
                                                _ -> assertFailure ("expected one key part, got " <> show (length keys))
                                        other -> assertFailure ("expected a KEY table constraint, got " <> show other)
                                _ -> assertFailure "expected a column element followed by a KEY element"
                        other -> assertFailure ("expected CREATE TABLE, got " <> show other)
                other -> assertFailure ("expected a CREATE statement, got " <> show other)
        , testCase "ClickHouse data.:Int64 is a typed path cast node" $ do
            expr <- singleSelectExpr clickhouseProfile (BSC.pack "SELECT data.:Int64 FROM t;")
            case locValue expr of
                ExprCast CastDotColon operand typeName -> do
                    assertColumn "data" operand
                    case typeNameParts (locValue typeName) of
                        [L _ part] -> assertEqual "cast type" "Int64" (namePartRaw part)
                        parts -> assertFailure ("expected one type part, got " <> show (length parts))
                other -> assertFailure ("expected a dot-colon cast, got " <> show other)
        , testCase "SQLite PRAGMA keeps the qualified name and bare value" $ do
            stmt <- parseSingle sqliteProfile (BSC.pack "PRAGMA main.foreign_keys = ON;")
            case stmt of
                L _ (StatementPragma (L _ (PragmaStmt (L _ (Name nameParts')) value))) -> do
                    assertEqual "qualified pragma name parts" 2 (length nameParts')
                    case value of
                        Just (PragmaBare (L _ raw)) -> assertEqual "bare pragma value" "ON" raw
                        other -> assertFailure ("expected a bare PRAGMA value, got " <> show other)
                other -> assertFailure ("expected a PRAGMA statement, got " <> show other)
        , testCase "DuckDB E'...' literal keeps its escape-literal lexical kind" $ do
            expr <- singleSelectExpr duckdbProfile (BSC.pack "SELECT E'a\\n';")
            case locValue expr of
                ExprLiteral (Literal LitEscapeString raw) -> assertEqual "escape literal spelling" "E'a\\n'" raw
                other -> assertFailure ("expected an escape string literal, got " <> show other)
        , testCase "ClickHouse ARRAY JOIN produces a join node with an array target" $ do
            core <- parseSelect clickhouseProfile (BSC.pack "SELECT * FROM t ARRAY JOIN [];")
            case selectFrom core of
                Just (FromClause [L _ tableRef]) ->
                    case tableRefJoins tableRef of
                        [L _ join] -> do
                            assertEqual "join kind" (JoinArray False) (joinKind join)
                            case joinTarget join of
                                JoinTargetArray items -> assertEqual "array join item count" 1 (length items)
                                other -> assertFailure ("expected an array join target, got " <> show other)
                        joins -> assertFailure ("expected one join clause, got " <> show (length joins))
                other -> assertFailure ("expected one FROM item, got " <> show other)
        , testCase "DuckDB PIVOT keeps ON, IN, and GROUP BY columns separate" $ do
            stmt <- parseSingle duckdbProfile (BSC.pack "PIVOT t ON a IN (1, 2) GROUP BY b;")
            case stmt of
                L _ (StatementPivot (L _ pivot)) -> do
                    assertEqual "PIVOT flag" False (pivotIsUnpivot pivot)
                    assertEqual "pivot ON count" 1 (length (pivotOn pivot))
                    assertEqual "pivot IN count" 2 (length (pivotIn pivot))
                    assertEqual "pivot GROUP BY count" 1 (length (pivotGroupBy pivot))
                other -> assertFailure ("expected a PIVOT statement, got " <> show other)
        , testCase "PostgreSQL TABLESAMPLE keeps amount, seed, and exact spans" $ do
            let sql = BSC.pack "SELECT a FROM t TABLESAMPLE SYSTEM (10) REPEATABLE (42);"
            core <- parseSelect postgresqlProfile sql
            case selectFrom core of
                Just (FromClause [L _ tableRef]) ->
                    case tableRefSample tableRef of
                        Just (L sampleSpan (TableSample method amount percent repeatable _offset)) -> do
                            assertEqual "sample span" (ByteSpan 16 55) sampleSpan
                            assertSlice sql sampleSpan "TABLESAMPLE SYSTEM (10) REPEATABLE (42)"
                            assertEqual "method" "SYSTEM" (locValue method)
                            assertEqual "method span" (ByteSpan 28 34) (locSpan method)
                            assertEqual "percent flag" False percent
                            assertEqual "amount span" (ByteSpan 36 38) (locSpan amount)
                            assertSlice sql (locSpan amount) "10"
                            case repeatable of
                                Just seed -> do
                                    assertEqual "seed span" (ByteSpan 52 54) (locSpan seed)
                                    assertSlice sql (locSpan seed) "42"
                                Nothing -> assertFailure "expected a REPEATABLE seed"
                        Nothing -> assertFailure "expected a TABLESAMPLE clause"
                other -> assertFailure ("expected one FROM item, got " <> show other)
        , testCase "PostgreSQL TABLESAMPLE percent suffix is structural" $ do
            let sql = BSC.pack "SELECT a FROM t TABLESAMPLE BERNOULLI (50%) REPEATABLE (42);"
            core <- parseSelect postgresqlProfile sql
            case selectFrom core of
                Just (FromClause [L _ tableRef]) ->
                    case tableRefSample tableRef of
                        Just (L sampleSpan (TableSample method amount percent repeatable _offset)) -> do
                            assertEqual "sample span" (ByteSpan 16 59) sampleSpan
                            assertSlice sql sampleSpan "TABLESAMPLE BERNOULLI (50%) REPEATABLE (42)"
                            assertEqual "method" "BERNOULLI" (locValue method)
                            assertEqual "method span" (ByteSpan 28 37) (locSpan method)
                            assertEqual "percent flag" True percent
                            assertEqual "amount span" (ByteSpan 39 41) (locSpan amount)
                            assertSlice sql (locSpan amount) "50"
                            case repeatable of
                                Just seed -> assertSlice sql (locSpan seed) "42"
                                Nothing -> assertFailure "expected a REPEATABLE seed"
                        Nothing -> assertFailure "expected a TABLESAMPLE clause"
                other -> assertFailure ("expected one FROM item, got " <> show other)
        , testCase "GROUPING SETS keeps the empty set as GroupByParens []" $ do
            let sql = BSC.pack "SELECT a FROM t GROUP BY GROUPING SETS ((a), ());"
            core <- parseSelect postgresqlProfile sql
            case selectGroupBy core of
                [ L
                        setsSpan
                        ( GroupByGroupingSets
                                [ L firstSpan (GroupByParens [L innerSpan (GroupByExpr inner)])
                                    , L emptySpan (GroupByParens [])
                                    ]
                            )
                    ] -> do
                        assertEqual "grouping sets span" (ByteSpan 25 48) setsSpan
                        assertSlice sql setsSpan "GROUPING SETS ((a), ())"
                        assertEqual "first set span" (ByteSpan 40 43) firstSpan
                        assertSlice sql firstSpan "(a)"
                        assertEqual "inner expression span" (ByteSpan 41 42) innerSpan
                        assertColumn "a" inner
                        assertEqual "empty set span" (ByteSpan 45 47) emptySpan
                        assertSlice sql emptySpan "()"
                other -> assertFailure ("unexpected GROUP BY shape: " <> show other)
        , testCase "ROLLUP and CUBE are structural comma-separated group-by items" $ do
            let sql = BSC.pack "SELECT a FROM t GROUP BY ROLLUP (a, b), CUBE (a, b);"
            core <- parseSelect duckdbProfile sql
            case selectGroupBy core of
                [ L rollupSpan (GroupByRollup [L aSpan (GroupByExpr a), L bSpan (GroupByExpr b)])
                    , L cubeSpan (GroupByCube [L cSpan (GroupByExpr c), L dSpan (GroupByExpr d)])
                    ] -> do
                        assertEqual "rollup span" (ByteSpan 25 38) rollupSpan
                        assertSlice sql rollupSpan "ROLLUP (a, b)"
                        assertEqual "rollup a span" (ByteSpan 33 34) aSpan
                        assertColumn "a" a
                        assertEqual "rollup b span" (ByteSpan 36 37) bSpan
                        assertColumn "b" b
                        assertEqual "cube span" (ByteSpan 40 51) cubeSpan
                        assertSlice sql cubeSpan "CUBE (a, b)"
                        assertEqual "cube a span" (ByteSpan 46 47) cSpan
                        assertColumn "a" c
                        assertEqual "cube b span" (ByteSpan 49 50) dSpan
                        assertColumn "b" d
                other -> assertFailure ("unexpected ROLLUP/CUBE shape: " <> show other)
        , testCase "ALTER TABLE ADD INDEX keeps the index name" $ do
            stmt <- parseSingle mysqlProfile (BSC.pack "ALTER TABLE t ADD INDEX idx (a);")
            case stmt of
                L _ (StatementAlter (L _ alter)) ->
                    case alterActions alter of
                        [L _ (AlterAddIndex mName keys)] -> do
                            case mName of
                                Just (L _ part) -> assertEqual "index name" "idx" (namePartRaw part)
                                Nothing -> assertFailure "expected an index name"
                            assertEqual "key count" 1 (length keys)
                        other -> assertFailure ("unexpected ALTER actions: " <> show other)
                other -> assertFailure ("expected ALTER TABLE, got " <> show other)
        , testCase "DuckDB ATTACH IF NOT EXISTS records the flag and alias" $ do
            stmt <- parseSingle duckdbProfile (BSC.pack "ATTACH IF NOT EXISTS 'file.db' AS file_db;")
            case locValue stmt of
                StatementAttach (L _ (AttachDatabase _ alias ifNotExists)) -> do
                    assertEqual "if-not-exists flag" True ifNotExists
                    case alias of
                        Just (L _ part) -> assertEqual "alias" "file_db" (namePartRaw part)
                        Nothing -> assertFailure "expected an ATTACH alias"
                other -> assertFailure ("expected ATTACH, got " <> show other)
        , testCase "DuckDB parenthesized view body is parsed as a query" $ do
            stmt <- parseSingle duckdbProfile (BSC.pack "CREATE VIEW v AS (SELECT 1);")
            case locValue stmt of
                StatementCreate (L _ create) ->
                    case locValue (createObject create) of
                        CreateViewObject view ->
                            case createViewBody view of
                                ViewBodyQuery _ -> pure ()
                                other -> assertFailure ("expected a query view body, got " <> show other)
                        other -> assertFailure ("expected CREATE VIEW, got " <> show other)
                other -> assertFailure ("expected CREATE, got " <> show other)
        ]
    where
        isBinaryConstraint ColumnConstraintBinary = True
        isBinaryConstraint _ = False

reviewedRegressions :: TestTree
reviewedRegressions =
    testGroup
        "frontend audit regressions"
        [ testCase "type suffixes preserve order, quotes and spans" $ do
            forM_
                [ (postgresqlProfile, "TIMESTAMP WITHOUT TIME ZONE", ["WITHOUT", "TIME", "ZONE"])
                , (postgresqlProfile, "TEXT COLLATE \"C\"", ["COLLATE", "\"C\""])
                , (mysqlProfile, "INT UNSIGNED ZEROFILL", ["UNSIGNED", "ZEROFILL"])
                , (mysqlProfile, "VARCHAR(10) CHARACTER SET utf8mb4", ["CHARACTER", "SET", "utf8mb4"])
                ]
                $ \(profile, ty, expected) -> do
                    let sql = "SELECT CAST(x AS " <> ty <> ");"
                    expression <- singleSelectExpr profile sql
                    case locValue expression of
                        ExprCast _ _ (L _ typeName) -> do
                            let suffix = typeNameSuffix typeName
                            assertEqual "suffix words" expected (map (namePartRaw . locValue) suffix)
                            forM_ suffix $ \part -> assertSlice sql (locSpan part) (namePartRaw (locValue part))
                        other -> assertFailure (show other)
            assertRejected postgresqlProfile "SELECT CAST(x AS TIMESTAMP WITH TIME TYPO);"
        , testCase "client directives are classified by the splitter" $ do
            let sql = "DELIMITER $$\nCOMMIT$$\nDELIMITER ;\nSELECT 2;"
            rows <- either (assertFailure . show) pure (parseScriptUnits mysqlProfile sql)
            case map snd rows of
                [ Right (L _ (StatementDelimiter (L argSpan "$$")))
                    , Right (L _ (StatementTransaction _))
                    , Right (L _ (StatementDelimiter (L _ ";")))
                    , Right (L _ (StatementQuery _))
                    ] ->
                        assertSlice sql argSpan "$$"
                other -> assertFailure (show other)
            assertEqual "only SQL statements in parseScript" 2 . length =<< parseStatements mysqlProfile sql
            assertEqual "empty units under a custom delimiter" 1 . length
                =<< parseStatements mysqlProfile "DELIMITER $$\n;$$\nSELECT 1$$"
            mapM_ (assertRejected mysqlProfile) ["DELIMITER", "DELIMITER;", "DELIMITER \nSELECT 1;"]
        , testCase "per-unit API preserves valid statements around syntax errors" $ do
            let sql = "SELECT 1; SELECT * FROM; SELECT 2; @'broken"
            rows <- either (assertFailure . show) pure (parseScriptUnits mysqlProfile sql)
            case map snd rows of
                [Right first, Left failure, Right third, Left lexical] -> do
                    assertSlice sql (locSpan first) "SELECT 1"
                    assertSlice sql (locSpan third) "SELECT 2"
                    assertEqual "parse error" ParseUnexpectedEnd (parseErrorKind failure)
                    assertEqual "lexical error" ParseLexicalError (parseErrorKind lexical)
                other -> assertFailure (show other)
            assertRejected mysqlProfile sql
        , testCase "parenthesized set queries and scalar operands are distinguished" $ do
            let sub = "((SELECT 1) UNION (SELECT 2))"
            scalar <- singleSelectExpr postgresqlProfile ("SELECT " <> sub)
            case locValue scalar of
                ExprSubquery _ -> pure ()
                other -> assertFailure (show other)
            predicate <- singleSelectExpr postgresqlProfile ("SELECT x IN " <> sub)
            case locValue predicate of
                ExprInSubquery {} -> assertSlice ("SELECT x IN " <> sub) (locSpan predicate) ("x IN " <> sub)
                other -> assertFailure (show other)
            _ <- parseSingle postgresqlProfile ("SELECT * FROM (" <> sub <> ") AS t")
            _ <- parseSingle postgresqlProfile "INSERT INTO t (SELECT 1) RETURNING *;"
            _ <- parseSingle postgresqlProfile "INSERT INTO t (a) ((SELECT 1) UNION (SELECT 2));"
            add <- singleSelectExpr postgresqlProfile "SELECT (((SELECT 1) UNION SELECT 2) + 3)"
            case locValue add of
                ExprParen (L _ (ExprBinary OpAdd (L _ (ExprSubquery _)) _)) -> pure ()
                other -> assertFailure (show other)
        , testCase "DuckDB FROM-first queries work in subqueries and INSERT" $ do
            mapM_
                (parseSingle duckdbProfile)
                [ "SELECT (FROM t)"
                , "SELECT * FROM (FROM t) s"
                , "SELECT x IN (FROM t)"
                , "INSERT INTO t FROM u"
                , "FROM t GROUP BY a"
                ]
            query <-
                parseSingle
                    duckdbProfile
                    "FROM t SELECT a, sum(b) WHERE a > 0 GROUP BY a HAVING sum(b) > 1 QUALIFY row_number() OVER () > 0"
            case locValue query of
                StatementQuery (L _ Query {queryBody = L _ (QueryBodyFrom core)}) -> do
                    assertEqual "projection count" 2 (length (selectItems core))
                    assertEqual "GROUP BY count" 1 (length (selectGroupBy core))
                    assertBool
                        "WHERE, HAVING and QUALIFY preserved"
                        (Nothing `notElem` [selectWhere core, selectHaving core, locValue <$> selectQualify core])
                other -> assertFailure (show other)
            assertRejected postgresqlProfile "SELECT (FROM t)"
        , testCase "operator precedence follows the active dialect" $ do
            forM_ [mysqlProfile, mariadbProfile] $ \profile -> do
                expr <- singleSelectExpr profile "SELECT a OR b XOR c AND d"
                case locValue expr of
                    ExprBinary OpOr _ (L _ (ExprBinary OpXor _ (L _ (ExprBinary OpAnd _ _)))) -> pure ()
                    other -> assertFailure (show other)
                bits <- singleSelectExpr profile "SELECT 1 | 2 & 3 << 4 + 5"
                case locValue bits of
                    ExprBinary
                        OpBitOr
                        _
                        (L _ (ExprBinary OpBitAnd _ (L _ (ExprBinary OpShiftLeft _ (L _ (ExprBinary OpAdd _ _)))))) -> pure ()
                    other -> assertFailure (show other)
                bitXor <- singleSelectExpr profile "SELECT 1 * 2 ^ 3 ^ 4"
                case locValue bitXor of
                    ExprBinary OpMul _ (L _ (ExprBinary OpBitXor (L _ (ExprBinary OpBitXor _ _)) _)) -> pure ()
                    other -> assertFailure (show other)
                assertRejected profile "SELECT a = NOT b = c"
            forM_ [postgresqlProfile, sqliteProfile, duckdbProfile] $ \profile -> do
                expr <- singleSelectExpr profile "SELECT 1 & 2 + 3"
                case locValue expr of
                    ExprBinary OpBitAnd _ (L _ (ExprBinary OpAdd _ _)) -> pure ()
                    other -> assertFailure (show other)
                negation <- singleSelectExpr profile "SELECT a = NOT b = c AND d"
                case locValue negation of
                    ExprBinary OpAnd (L _ (ExprBinary OpEq _ (L _ (ExprUnary UnaryNot (L _ (ExprBinary OpEq _ _)))))) _ -> pure ()
                    other -> assertFailure (show other)
        , testCase "expression predicates and postfixes keep their syntax in the AST" $ do
            unknown <- singleSelectExpr postgresqlProfile "SELECT a IS NOT UNKNOWN"
            case locValue unknown of
                ExprIsUnknown True _ -> pure ()
                other -> assertFailure (show other)
            star <- singleSelectExpr postgresqlProfile "SELECT (f()).*"
            case locValue star of
                ExprFieldStar _ -> pure ()
                other -> assertFailure (show other)
            tuple <- singleSelectExpr clickhouseProfile "SELECT tup.1.2 FROM t"
            case locValue tuple of
                ExprFieldIndex (L _ (ExprFieldIndex _ (L _ "1"))) (L _ "2") -> pure ()
                other -> assertFailure (show other)
            forM_ [sqliteProfile, duckdbProfile] $ \profile -> do
                decimal <- singleSelectExpr profile "SELECT.1"
                assertNumberLiteral ".1" decimal
                _ <- parseSingle profile "SELECT CASE WHEN.1 THEN.2 ELSE.3 END"
                pure ()
            assertRejected duckdbProfile "SELECT tup.1 FROM t"
            assertRejected duckdbProfile "SELECT (r).* FROM t"
        , testCase "transaction modes are validated and commas are preserved as separators" $ do
            stmt <-
                parseSingle
                    postgresqlProfile
                    "START TRANSACTION ISOLATION LEVEL SERIALIZABLE, READ ONLY, NOT DEFERRABLE"
            case locValue stmt of
                StatementTransaction (L _ tx) ->
                    assertEqual
                        "mode grouping"
                        [ TransactionUnmodeledMode "ISOLATION LEVEL SERIALIZABLE"
                        , TransactionReadOnly True
                        , TransactionUnmodeledMode "NOT DEFERRABLE"
                        ]
                        (transactionModes tx)
                other -> assertFailure (show other)
            _ <- parseSingle mysqlProfile "START TRANSACTION WITH CONSISTENT SNAPSHOT, READ ONLY"
            _ <- parseSingle sqliteProfile "BEGIN IMMEDIATE TRANSACTION"
            mapM_
                (assertRejected postgresqlProfile)
                ["BEGIN FOO BAR SELECT 1;", "BEGIN \"READ\";", "START TRANSACTION READ ONLY,"]
        , testCase "COPY and EXPLAIN preserve case-sensitive option values" $ do
            copy <- parseSingle postgresqlProfile "COPY t FROM 'data.txt' (NULL 'null', ENCODING 'utf8')"
            case locValue copy of
                StatementCopy (L _ options) ->
                    assertEqual
                        "COPY values"
                        [Just "'null'", Just "'utf8'"]
                        (map (fmap locValue . copyOptionValue . locValue) (copyOptions options))
                other -> assertFailure (show other)
            explain <- parseSingle postgresqlProfile "EXPLAIN (FORMAT 'json') SELECT 1"
            case locValue explain of
                StatementExplain (L _ options) ->
                    assertEqual
                        "EXPLAIN value"
                        [Just "'json'"]
                        (map (fmap locValue . explainOptionValue . locValue) (explainOptions options))
                other -> assertFailure (show other)
        , testCase "MySQL family preserves both SET variable kinds and joined DML targets" $ do
            forM_ [mysqlProfile, mariadbProfile] $ \profile -> do
                stmt <- parseSingle profile "SET @foreign_key_checks = 0, @@`foreign_key_checks` = 0"
                case locValue stmt of
                    StatementSet (L _ set) -> case map (setItemTarget . locValue) (setItems set) of
                        [SetTargetUserVar _, SetTargetSysVar _] -> pure ()
                        other -> assertFailure (show other)
                    other -> assertFailure (show other)
                update <- parseSingle profile "UPDATE t1 a JOIN t2 b ON a.id=b.id, t3 c SET a.x=c.x"
                case locValue update of
                    StatementUpdate (L _ UpdateStmt {updateTargets = FromClause [L _ first, L _ second]}) -> do
                        assertEqual "first target join count" 1 (length (tableRefJoins first))
                        assertEqual
                            "second target alias"
                            (Just "c")
                            (namePartRaw . locValue . aliasName . locValue <$> tableRefAlias second)
                    other -> assertFailure (show other)
                forM_ ["DELETE FROM t1.*, t2 USING t1 JOIN t2 ON t1.id=t2.id", "DELETE t1.* FROM t1"] $ \sql -> do
                    delete <- parseSingle profile sql
                    case locValue delete of
                        StatementDelete (L _ DeleteStmt {deleteTargets = L _ (DeleteTarget _ True) : _}) -> pure ()
                        other -> assertFailure (show other)
        , testCase "MERGE preserves DEFAULT VALUES and RETURNING" $ do
            forM_ [postgresqlProfile, duckdbProfile] $ \profile -> do
                stmt <-
                    parseSingle
                        profile
                        "MERGE INTO t USING u ON t.id = u.id WHEN NOT MATCHED THEN INSERT DEFAULT VALUES RETURNING *"
                case locValue stmt of
                    StatementMerge (L _ merge) -> do
                        assertEqual
                            "actions"
                            [MergeInsertDefaultValues]
                            (map (mergeClauseAction . locValue) (mergeClauses merge))
                        assertEqual "RETURNING length" 1 (length (mergeReturning merge))
                    other -> assertFailure (show other)
        , testCase "DDL validates routine bodies, trigger events and ALTER actions" $ do
            mapM_
                (assertRejected postgresqlProfile)
                [ "CREATE FUNCTION f() RETURNS int AS;"
                , "CREATE TRIGGER tr BEFORE ON t EXECUTE FUNCTION f()"
                , "CREATE TRIGGER tr BEFORE INSERT OR ON t EXECUTE FUNCTION f()"
                , "ALTER TABLE t ALTER COLUMN c FOOBAR"
                ]
            _ <-
                parseSingle
                    postgresqlProfile
                    "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH STATEMENT EXECUTE FUNCTION f()"
            _ <- parseSingle postgresqlProfile "ALTER TABLE t ALTER COLUMN c SET DATA TYPE int"
            _ <- parseSingle mysqlProfile "ALTER TABLE t DROP KEY primary"
            _ <- parseSingle postgresqlProfile "DROP MATERIALIZED VIEW mv"
            _ <- parseSingle clickhouseProfile "DROP VIEW mv"
            assertRejected clickhouseProfile "DROP MATERIALIZED VIEW mv"
            assertRejected mysqlProfile "ALTER TABLE t DROP KEY PRIMARY KEY"
        , testCase "DDL preserves SQLite conflicts, MySQL index metadata and foreign key timing" $ do
            columns <-
                parseSingle
                    sqliteProfile
                    "CREATE TABLE t (a INTEGER PRIMARY KEY ASC ON CONFLICT REPLACE, b TEXT UNIQUE ON CONFLICT IGNORE)"
            case tableElements columns of
                [TableElementColumn (L _ a), TableElementColumn (L _ b)] -> do
                    case map locValue (columnConstraints a ++ columnConstraints b) of
                        [ ColumnConstraintOnConflict
                                (ColumnConstraintPrimaryKey (Just SortAscending))
                                (L _ ColumnConflictReplace)
                            , ColumnConstraintOnConflict ColumnConstraintUnique (L _ ColumnConflictIgnore)
                            ] -> pure ()
                        other -> assertFailure (show other)
                other -> assertFailure (show other)
            indexes <-
                parseSingle
                    mysqlProfile
                    "CREATE TABLE t (a INT, CONSTRAINT c1 UNIQUE KEY idx (a), UNIQUE USING BTREE (a))"
            case drop 1 (tableElements indexes) of
                [ TableElementConstraint (L _ (TableConstraintUnique named _))
                    , TableElementConstraint (L _ (TableConstraintUnique method _))
                    ] -> do
                        assertEqual "constraint name" (Just "c1") (namePartRaw . locValue <$> constraintName named)
                        assertEqual "index name" (Just "idx") (namePartRaw . locValue <$> constraintIndexName named)
                        assertEqual "index method" (Just "BTREE") (namePartRaw . locValue <$> constraintIndexMethod method)
                other -> assertFailure (show other)
            fk <-
                parseSingle
                    postgresqlProfile
                    "CREATE TABLE t (a INT REFERENCES p(id) MATCH FULL ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED NOT NULL)"
            case tableElements fk of
                [TableElementColumn (L _ col)] -> case map locValue (columnConstraints col) of
                    [ColumnConstraintReferences (L _ ref), ColumnConstraintNotNull] -> do
                        assertEqual "match" (Just "FULL") (namePartRaw . locValue <$> foreignKeyMatch ref)
                        assertEqual "deferrable" (Just True) (foreignKeyDeferrable ref)
                        assertEqual "initially" (Just ForeignKeyInitiallyDeferred) (foreignKeyInitially ref)
                    other -> assertFailure (show other)
                other -> assertFailure (show other)
        , testCase "feature gates use the active profile and distinguish SELECT INTO destinations" $ do
            pg <- parseSelect postgresqlProfile "SELECT a INTO TEMP TABLE new_table FROM old_table"
            case selectInto pg of
                Just (L _ (SelectInto _ SelectIntoTemporary True)) -> pure ()
                other -> assertFailure (show other)
            forM_ ["SELECT a INTO @v FROM t", "SELECT a FROM t LIMIT 1 INTO @v"] $ \sql -> do
                my <- parseSelect mysqlProfile sql
                case selectInto my of
                    Just (L _ (SelectIntoUserVars [L _ "@v"])) -> pure ()
                    other -> assertFailure (show other)
            let custom =
                    postgresqlProfile
                        { dialectFeatures = Set.delete FeatureSelectIntoCreatesTable (dialectFeatures postgresqlProfile)
                        }
            assertRejected custom "SELECT 1 INTO t"
        ]
    where
        assertRejected profile sql = case parseScript profile sql of
            Left _ -> pure ()
            Right other -> assertFailure ("expected a parse error for " <> show sql <> ", got " <> show other)
        tableElements (L _ (StatementCreate (L _ CreateStmt {createObject = L _ (CreateTableObject table)}))) =
            map locValue (createTableElements table)
        tableElements _ = []

-- ---------------------------------------------------------------------------
-- Grammar-gap regressions (AST structure, spans, and dialect gating)
-- ---------------------------------------------------------------------------

grammarGapTests :: TestTree
grammarGapTests =
    testGroup
        "grammar-gap productions"
        [ testCase "SQLite INSERT OR <action> and dialect gating" $ do
            forM_
                [ ("INSERT OR IGNORE INTO t (a) VALUES (1)", InsertOrIgnore)
                , ("INSERT OR REPLACE INTO t (a) VALUES (1)", InsertOrReplace)
                , ("INSERT OR ABORT INTO t (a) VALUES (1)", InsertOrAbort)
                , ("INSERT OR FAIL INTO t (a) VALUES (1)", InsertOrFail)
                , ("INSERT OR ROLLBACK INTO t (a) VALUES (1)", InsertOrRollback)
                ]
                $ \(sql, expectedAction) -> do
                    stmt <- parseSingleChecked sqliteProfile sql
                    case locValue stmt of
                        StatementInsert (L _ ins) ->
                            assertEqual
                                ("INSERT OR action for " <> show sql)
                                (Just expectedAction)
                                (locValue <$> insertOrAction ins)
                        other -> assertFailure ("expected InsertStmt, got " <> show other)
            assertRejected postgresqlProfile "INSERT OR IGNORE INTO t (a) VALUES (1)"
            assertRejected mysqlProfile "INSERT OR IGNORE INTO t (a) VALUES (1)"
        , testCase "MySQL 8.0 INSERT ... VALUES (...) AS alias [(cols)] ON DUPLICATE KEY UPDATE" $ do
            stmt <-
                parseSingleChecked
                    mysqlProfile
                    "INSERT INTO t (a, b) VALUES (1, 2) AS new(x, y) ON DUPLICATE KEY UPDATE a = new.x + c"
            case locValue stmt of
                StatementInsert (L _ ins) -> do
                    case insertValuesAlias ins of
                        Just (L _ (Alias (L _ np) cols)) -> do
                            assertEqual "row alias" "new" (namePartRaw np)
                            assertEqual "row alias cols" ["x", "y"] (map (namePartRaw . locValue) cols)
                        Nothing -> assertFailure "expected insertValuesAlias"
                    assertBool "on duplicate key" (maybe False (not . null) (insertOnDuplicate ins))
                other -> assertFailure ("expected InsertStmt, got " <> show other)
            assertRejected
                mariadbProfile
                "INSERT INTO t (a) VALUES (1) AS new ON DUPLICATE KEY UPDATE a = new.a"
        , testCase "DuckDB struct literals and TRY_CAST" $ do
            stmt <- parseSingleChecked duckdbProfile "SELECT {'a': 1, 'b': 2}, {}, TRY_CAST(col AS INT) FROM t"
            case locValue stmt of
                StatementQuery (L _ q) -> case locValue (queryBody q) of
                    QueryBodySelect core -> case map (locValue . selectItemExpr . locValue) (selectItems core) of
                        [ExprStruct fields, ExprStruct [], ExprCast CastTryFunction _ _] ->
                            assertEqual "struct field count" 2 (length fields)
                        other -> assertFailure ("unexpected DuckDB projections: " <> show other)
                    other -> assertFailure ("expected SELECT, got " <> show other)
                other -> assertFailure ("expected query, got " <> show other)
            assertRejected postgresqlProfile "SELECT {'a': 1}"
            assertRejected postgresqlProfile "SELECT TRY_CAST(col AS INT) FROM t"
        , testCase "PostgreSQL SET ROLE, SET SESSION AUTHORIZATION, SET TRANSACTION SNAPSHOT" $ do
            forM_
                [ ("SET ROLE analyst", SetScopeNone, "ROLE")
                , ("SET LOCAL ROLE NONE", SetScopeLocal, "ROLE")
                , ("SET SESSION AUTHORIZATION admin", SetScopeNone, "SESSION AUTHORIZATION")
                , ("SET LOCAL SESSION AUTHORIZATION DEFAULT", SetScopeLocal, "SESSION AUTHORIZATION")
                , ("SET SESSION SESSION AUTHORIZATION admin", SetScopeSession, "SESSION AUTHORIZATION")
                ]
                $ \(sql, expectedScope, expectedTarget) -> do
                    stmt <- parseSingleChecked postgresqlProfile sql
                    case locValue stmt of
                        StatementSet (L _ setStmt) -> do
                            assertEqual ("scope for " <> show sql) expectedScope (setScope setStmt)
                            case map (setItemTarget . locValue) (setItems setStmt) of
                                [SetTargetSpecial (L _ raw)] -> assertEqual "special target" expectedTarget raw
                                other -> assertFailure ("unexpected SET target: " <> show other)
                        other -> assertFailure ("expected SetStmt, got " <> show other)
            snap <- parseSingleChecked postgresqlProfile "SET TRANSACTION SNAPSHOT '00000003-0000001A-1'"
            case locValue snap of
                StatementSet (L _ setStmt) ->
                    assertEqual
                        "snapshot mode"
                        ["SNAPSHOT '00000003-0000001A-1'"]
                        (map locValue (setTransactionModes setStmt))
                other -> assertFailure ("expected SetStmt, got " <> show other)
            assertRejected mysqlProfile "SET TRANSACTION SNAPSHOT 'snap'"
        , testCase "RESET statements across PostgreSQL, DuckDB, and MySQL/MariaDB rejections" $ do
            r1 <- parseSingleChecked postgresqlProfile "RESET ROLE"
            assertEqual "RESET ROLE" (StatementReset (L (ByteSpan 0 10) ResetRole)) (locValue r1)
            r2 <- parseSingleChecked postgresqlProfile "RESET SESSION AUTHORIZATION"
            case locValue r2 of
                StatementReset (L _ ResetSessionAuthorization) -> pure ()
                other -> assertFailure ("expected ResetSessionAuthorization, got " <> show other)
            r3 <- parseSingleChecked postgresqlProfile "RESET ALL"
            case locValue r3 of
                StatementReset (L _ ResetAll) -> pure ()
                other -> assertFailure ("expected ResetAll, got " <> show other)
            r4 <- parseSingleChecked postgresqlProfile "RESET search_path"
            case locValue r4 of
                StatementReset (L _ (ResetTarget (SetTargetName _))) -> pure ()
                other -> assertFailure ("expected ResetTarget SetTargetName, got " <> show other)
            _ <- parseSingleChecked duckdbProfile "RESET ALL"
            forM_ [mysqlProfile, mariadbProfile, sqliteProfile] $ \prof -> do
                assertRejected prof "RESET foreign_key_checks"
                assertRejected prof "RESET ALL"
            assertRejected mysqlProfile "RESET @search_path"
            assertRejected mysqlProfile "RESET @@global.foreign_key_checks"
        , testCase "PostgreSQL ALTER TABLE USING INDEX, INHERIT/NO INHERIT, OWNER TO, SET LOGGED/UNLOGGED" $ do
            pk <- parseSingleChecked postgresqlProfile "ALTER TABLE s.t ADD PRIMARY KEY USING INDEX t_idx"
            case alterActionsOf pk of
                [AlterAddConstraint (L _ (TableConstraintUsingIndex Nothing True (L _ idx)))] ->
                    assertEqual "index name" "t_idx" (namePartRaw idx)
                other -> assertFailure ("unexpected USING INDEX action: " <> show other)
            uq <-
                parseSingleChecked
                    postgresqlProfile
                    "ALTER TABLE t ADD CONSTRAINT c1 UNIQUE USING INDEX u_idx DEFERRABLE"
            case alterActionsOf uq of
                [AlterAddConstraint (L _ (TableConstraintUsingIndex (Just (L _ cname)) False (L _ idx)))] -> do
                    assertEqual "constraint name" "c1" (namePartRaw cname)
                    assertEqual "index name" "u_idx" (namePartRaw idx)
                other -> assertFailure ("unexpected UNIQUE USING INDEX action: " <> show other)
            inh <- parseSingleChecked postgresqlProfile "ALTER TABLE c INHERIT s.p"
            case alterActionsOf inh of
                [AlterInherit _] -> pure ()
                other -> assertFailure ("expected AlterInherit, got " <> show other)
            noInh <- parseSingleChecked postgresqlProfile "ALTER TABLE c NO INHERIT p"
            case alterActionsOf noInh of
                [AlterNoInherit _] -> pure ()
                other -> assertFailure ("expected AlterNoInherit, got " <> show other)
            own <- parseSingleChecked postgresqlProfile "ALTER TABLE t OWNER TO app"
            case alterActionsOf own of
                [AlterOwnerTo _] -> pure ()
                other -> assertFailure ("expected AlterOwnerTo, got " <> show other)
            logged <- parseSingleChecked postgresqlProfile "ALTER TABLE t SET LOGGED"
            assertEqual "SET LOGGED" [AlterSetLogged True] (alterActionsOf logged)
            unlogged <- parseSingleChecked postgresqlProfile "ALTER TABLE t SET UNLOGGED"
            assertEqual "SET UNLOGGED" [AlterSetLogged False] (alterActionsOf unlogged)
            assertRejected mysqlProfile "ALTER TABLE c INHERIT p"
            assertRejected mysqlProfile "ALTER TABLE t SET LOGGED"
        , testCase "PostgreSQL ALTER COLUMN TYPE USING, ADD GENERATED IDENTITY, and OPTIONS" $ do
            ty <- parseSingleChecked postgresqlProfile "ALTER TABLE t ALTER COLUMN a TYPE BIGINT USING (a + b)"
            case alterActionsOf ty of
                [AlterAlterColumn _ (AlterType _ (Just _))] -> pure ()
                other -> assertFailure ("expected AlterType with USING, got " <> show other)
            ident <-
                parseSingleChecked
                    postgresqlProfile
                    "ALTER TABLE t ALTER COLUMN a ADD GENERATED ALWAYS AS IDENTITY (START WITH 10)"
            case alterActionsOf ident of
                [AlterAlterColumn _ (AlterAddIdentity True opts)] ->
                    assertEqual "identity option count" 1 (length opts)
                other -> assertFailure ("expected AlterAddIdentity, got " <> show other)
            opts <-
                parseSingleChecked postgresqlProfile "ALTER TABLE t ALTER COLUMN a OPTIONS (ADD x 'y', DROP z)"
            case alterActionsOf opts of
                [AlterAlterColumn _ (AlterColumnOptions [L _ o1, L _ o2])] -> do
                    assertEqual "opt1 action" (Just "ADD") (locValue <$> genericOptionAction o1)
                    assertEqual "opt1 name" "x" (namePartRaw (locValue (genericOptionName o1)))
                    assertEqual "opt2 action" (Just "DROP") (locValue <$> genericOptionAction o2)
                    assertEqual "opt2 val" Nothing (genericOptionValue o2)
                other -> assertFailure ("expected AlterColumnOptions, got " <> show other)
        , testCase "MySQL/MariaDB ALTER TABLE ADD/MODIFY/CHANGE COLUMN FIRST/AFTER" $ do
            modStmt <- parseSingleChecked mysqlProfile "ALTER TABLE t MODIFY COLUMN a INT AFTER b"
            case alterActionsOf modStmt of
                [AlterModifyColumn [_] (Just (L _ (ColumnPositionAfter (L _ col))))] ->
                    assertEqual "AFTER col" "b" (namePartRaw col)
                other -> assertFailure ("expected AlterModifyColumn AFTER, got " <> show other)
            chgStmt <- parseSingleChecked mysqlProfile "ALTER TABLE t CHANGE COLUMN a a INT FIRST"
            case alterActionsOf chgStmt of
                [AlterChangeColumn _ _ (Just (L _ ColumnPositionFirst))] -> pure ()
                other -> assertFailure ("expected AlterChangeColumn FIRST, got " <> show other)
            addStmt <- parseSingleChecked mariadbProfile "ALTER TABLE t ADD COLUMN c TEXT AFTER a"
            case alterActionsOf addStmt of
                [AlterAddColumn False [_] (Just (L _ (ColumnPositionAfter (L _ col))))] ->
                    assertEqual "ADD AFTER col" "a" (namePartRaw col)
                other -> assertFailure ("expected AlterAddColumn AFTER, got " <> show other)
            assertRejected mysqlProfile "CREATE TABLE t (a INT FIRST)"
        , testCase "ClickHouse EPHEMERAL column default kind" $ do
            stmt <-
                parseSingleChecked
                    clickhouseProfile
                    "CREATE TABLE t (a Int32 EPHEMERAL, b Int32 EPHEMERAL 1, c Int32 DEFAULT 2) ENGINE = Memory"
            case tableElements stmt of
                [TableElementColumn (L _ a), TableElementColumn (L _ b), TableElementColumn _] -> do
                    assertEqual "a EPHEMERAL" [ColumnConstraintEphemeral Nothing] (map locValue (columnConstraints a))
                    case map locValue (columnConstraints b) of
                        [ColumnConstraintEphemeral (Just _)] -> pure ()
                        other -> assertFailure ("expected EPHEMERAL expr, got " <> show other)
                other -> assertFailure ("unexpected elements: " <> show other)
            assertRejected postgresqlProfile "CREATE TABLE t (a INT EPHEMERAL)"
        , testCase "MySQL LOCK TABLES / UNLOCK TABLES and GRANT / REVOKE" $ do
            lockStmt <- parseSingleChecked mysqlProfile "LOCK TABLES t WRITE, u AS u1 READ LOCAL"
            case locValue lockStmt of
                StatementLockTables (L _ (LockTablesStmt [L _ i1, L _ i2])) -> do
                    assertEqual "i1 type" "WRITE" (locValue (lockTableType i1))
                    assertEqual "i2 type" "READ LOCAL" (locValue (lockTableType i2))
                other -> assertFailure ("expected LockTablesStmt, got " <> show other)
            unlockStmt <- parseSingleChecked mysqlProfile "UNLOCK TABLES"
            assertEqual "UNLOCK TABLES" StatementUnlockTables (locValue unlockStmt)
            grantStmt <- parseSingleChecked mysqlProfile "GRANT SELECT ON t TO u"
            case locValue grantStmt of
                StatementGrant (L _ g) -> do
                    assertEqual "not revoke" False (grantIsRevoke g)
                    assertEqual "privs" ["SELECT"] (map locValue (grantPrivileges g))
                    assertEqual "grantees" 1 (length (grantGrantees g))
                other -> assertFailure ("expected GrantStmt, got " <> show other)
            revokeStmt <- parseSingleChecked postgresqlProfile "REVOKE SELECT, INSERT ON TABLE t FROM u CASCADE"
            case locValue revokeStmt of
                StatementGrant (L _ g) -> do
                    assertEqual "is revoke" True (grantIsRevoke g)
                    assertEqual "privs" ["SELECT", "INSERT"] (map locValue (grantPrivileges g))
                other -> assertFailure ("expected GrantStmt, got " <> show other)
            assertRejected sqliteProfile "LOCK TABLES t WRITE"
            assertRejected sqliteProfile "GRANT SELECT ON t TO u"
        , testCase "PostgreSQL CREATE TABLE AS WITH [NO] DATA and nested data-modifying CTEs" $ do
            ctas <-
                parseSingleChecked postgresqlProfile "CREATE TABLE t AS SELECT pg_catalog.nextval('s') WITH NO DATA"
            case locValue ctas of
                StatementCreate (L _ CreateStmt {createObject = L _ (CreateTableObject ct)}) ->
                    assertEqual "WITH NO DATA" (Just False) (createTableWithData ct)
                other -> assertFailure ("expected CreateTable, got " <> show other)
            cte <-
                parseSingleChecked
                    postgresqlProfile
                    "WITH outer_cte AS (WITH inner_cte AS (INSERT INTO t VALUES (1) RETURNING id) INSERT INTO t VALUES (2) RETURNING id) INSERT INTO t VALUES (3)"
            case locValue cte of
                StatementInsert (L _ ins) -> case insertWith ins of
                    Just (L _ (WithClause False [L _ outerCte])) ->
                        case locValue (cteBody outerCte) of
                            CteStatement (L _ (StatementInsert _)) -> pure ()
                            other -> assertFailure ("expected nested CteStatement Insert, got " <> show other)
                    other -> assertFailure ("expected outer WITH clause, got " <> show other)
                other -> assertFailure ("expected InsertStmt, got " <> show other)
        , testCase "Ordered-set aggregates WITHIN GROUP (ORDER BY ...) and MariaDB OVER requirement" $ do
            forM_ [postgresqlProfile, duckdbProfile] $ \prof -> do
                stmt <- parseSingleChecked prof "SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a DESC) FROM t"
                case locValue stmt of
                    StatementQuery (L _ q) -> case locValue (queryBody q) of
                        QueryBodySelect core -> case map (locValue . selectItemExpr . locValue) (selectItems core) of
                            [ExprCall (L _ fc)] ->
                                assertEqual "within group order items" 1 (length (functionWithinGroup fc))
                            other -> assertFailure ("expected ExprCall, got " <> show other)
                        other -> assertFailure ("expected SELECT, got " <> show other)
                    other -> assertFailure ("expected Query, got " <> show other)
            mariaStmt <-
                parseSingleChecked
                    mariadbProfile
                    "SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a DESC) OVER () FROM t"
            case locValue mariaStmt of
                StatementQuery (L _ q) -> case locValue (queryBody q) of
                    QueryBodySelect core -> case map (locValue . selectItemExpr . locValue) (selectItems core) of
                        [ExprCall (L _ fc)] -> do
                            assertEqual "MariaDB within group order items" 1 (length (functionWithinGroup fc))
                            assertBool "MariaDB within group OVER present" (isJust (functionOver fc))
                        other -> assertFailure ("expected ExprCall, got " <> show other)
                    other -> assertFailure ("expected SELECT, got " <> show other)
                other -> assertFailure ("expected Query, got " <> show other)
            assertRejected mariadbProfile "SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a DESC) FROM t"
            forM_ [sqliteProfile, mysqlProfile, clickhouseProfile] $ \prof -> do
                assertRejected prof "SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a DESC) FROM t"
                assertRejected prof "SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a DESC) OVER () FROM t"
        , testCase "PostgreSQL FROM ONLY t and FROM t * markers" $ do
            onlyStmt <- parseSingleChecked postgresqlProfile "SELECT * FROM ONLY t"
            case firstTableRef onlyStmt of
                Just tr -> do
                    assertEqual "tableRefOnly" True (tableRefOnly tr)
                    assertEqual "tableRefIncludeDescendants" False (tableRefIncludeDescendants tr)
                    assertEqual "no spurious alias" Nothing (tableRefAlias tr)
                Nothing -> assertFailure "expected TableRef"
            starStmt <- parseSingleChecked postgresqlProfile "SELECT * FROM t *"
            case firstTableRef starStmt of
                Just tr -> do
                    assertEqual "tableRefOnly" False (tableRefOnly tr)
                    assertEqual "tableRefIncludeDescendants" True (tableRefIncludeDescendants tr)
                Nothing -> assertFailure "expected TableRef"
            assertRejected mysqlProfile "SELECT * FROM t *"
        , testCase "MySQL/MariaDB/SQLite UPDATE and DELETE with ORDER BY and LIMIT" $ do
            upd <- parseSingleChecked mysqlProfile "UPDATE t SET a = 1 ORDER BY id LIMIT 5"
            case locValue upd of
                StatementUpdate (L _ u) -> do
                    assertEqual "update ORDER BY count" 1 (length (updateOrderBy u))
                    assertBool "update LIMIT present" (isJust (updateLimit u))
                other -> assertFailure ("expected UpdateStmt, got " <> show other)
            del <- parseSingleChecked sqliteProfile "DELETE FROM t WHERE id = 10 ORDER BY id LIMIT 5"
            case locValue del of
                StatementDelete (L _ d) -> do
                    assertEqual "delete ORDER BY count" 1 (length (deleteOrderBy d))
                    assertBool "delete LIMIT present" (isJust (deleteLimit d))
                other -> assertFailure ("expected DeleteStmt, got " <> show other)
            assertRejected postgresqlProfile "UPDATE t SET a = 1 LIMIT 1"
            assertRejected postgresqlProfile "DELETE FROM t ORDER BY id LIMIT 5"
        , testCase "PostgreSQL CREATE AGGREGATE and CREATE INDEX function call vs MySQL prefix" $ do
            agg <-
                parseSingleChecked postgresqlProfile "CREATE AGGREGATE count(int) (SFUNC = int4pl, STYPE = int)"
            case locValue agg of
                StatementCreate (L _ CreateStmt {createObject = L _ (CreateAggregateObject ca)}) -> do
                    assertEqual "aggregate param count" 1 (length (createAggregateParams ca))
                    assertEqual "aggregate option count" 2 (length (createAggregateOptions ca))
                other -> assertFailure ("expected CreateAggregateObject, got " <> show other)
            pgIdx <- parseSingleChecked postgresqlProfile "CREATE INDEX sk ON t (b(1))"
            case locValue pgIdx of
                StatementCreate (L _ CreateStmt {createObject = L _ (CreateIndexObject ci)}) ->
                    case map locValue (createIndexKeys ci) of
                        [IndexKeyPart (L _ (ExprCall _)) Nothing Nothing Nothing] -> pure ()
                        other -> assertFailure ("expected PostgreSQL functional index call, got " <> show other)
                other -> assertFailure ("expected CreateIndex, got " <> show other)
            myIdx <- parseSingleChecked mysqlProfile "CREATE INDEX sk ON t (b(1))"
            case locValue myIdx of
                StatementCreate (L _ CreateStmt {createObject = L _ (CreateIndexObject ci)}) ->
                    case map locValue (createIndexKeys ci) of
                        [IndexKeyPart (L _ (ExprColumn _)) (Just _) Nothing Nothing] -> pure ()
                        other -> assertFailure ("expected MySQL index prefix length, got " <> show other)
                other -> assertFailure ("expected CreateIndex, got " <> show other)
            assertRejected mysqlProfile "CREATE INDEX sk ON t (b(1.5))"
            assertRejected mysqlProfile "CREATE INDEX sk ON t (f(b))"
            assertRejected mariadbProfile "CREATE INDEX sk ON t ((lower(b)))"
            _ <- parseSingleChecked mysqlProfile "CREATE INDEX sk ON t ((lower(b)))"
            pure ()
        ]
    where
        parseSingleChecked profile sql = do
            stmt <- parseSingle profile sql
            validateStatementInvariants sql stmt
            pure stmt
        assertRejected profile sql = case parseScript profile sql of
            Left _ -> pure ()
            Right other -> assertFailure ("expected a parse error for " <> show sql <> ", got " <> show other)
        alterActionsOf (L _ (StatementAlter (L _ alt))) = map locValue (alterActions alt)
        alterActionsOf _ = []
        tableElements (L _ (StatementCreate (L _ CreateStmt {createObject = L _ (CreateTableObject table)}))) =
            map locValue (createTableElements table)
        tableElements _ = []
        firstTableRef (L _ (StatementQuery (L _ q))) = case locValue (queryBody q) of
            QueryBodySelect core -> case selectFrom core of
                Just (FromClause (L _ tr : _)) -> Just tr
                _ -> Nothing
            _ -> Nothing
        firstTableRef _ = Nothing

-- ---------------------------------------------------------------------------
-- Scripts, split units, determinism
-- ---------------------------------------------------------------------------

scriptTests :: TestTree
scriptTests =
    testGroup
        "script parsing"
        [ testCase "multi-statement SQLite script preserves comments and semicolons" $ do
            let sql =
                    BSC.pack
                        "-- lead\nCREATE TABLE t (a INTEGER);\nINSERT INTO t VALUES (1); /* gap */\nSELECT /*x*/ a FROM t;\n-- end\n"
            stmts <- parseStatements sqliteProfile sql
            assertEqual "statement count" 3 (length stmts)
            case stmts of
                [createStmt, insertStmt, selectStmt] -> do
                    assertSlice sql (locSpan createStmt) "CREATE TABLE t (a INTEGER)"
                    assertSlice sql (locSpan insertStmt) "INSERT INTO t VALUES (1)"
                    assertSlice sql (locSpan selectStmt) "SELECT /*x*/ a FROM t"
                _ -> assertFailure "expected exactly three statements"
        , testCase "empty and comment-only scripts parse to zero statements" $ do
            assertEqual "empty input" (Right (ParsedScript [])) (parseScript sqliteProfile "")
            assertEqual
                "comment-only input"
                (Right (ParsedScript []))
                (parseScript sqliteProfile "-- only a comment\n/* and a block */\n;\n")
        , testCase "parseUnit consumes exactly one split unit" $ do
            let sql = BSC.pack "SELECT 1; -- c\nSELECT 2;"
            case splitSQL sqliteProfile sql of
                Left err -> assertFailure ("unexpected split error: " <> show err)
                Right units ->
                    case units of
                        [firstUnit, secondUnit] -> do
                            firstStmt <- either (assertFailure . show) pure (parseUnit sqliteProfile firstUnit)
                            secondStmt <- either (assertFailure . show) pure (parseUnit sqliteProfile secondUnit)
                            assertEqual "first unit span" (ByteSpan 0 8) (locSpan firstStmt)
                            assertEqual "second unit span" (ByteSpan 15 23) (locSpan secondStmt)
                            assertSlice sql (locSpan firstStmt) "SELECT 1"
                            assertSlice sql (locSpan secondStmt) "SELECT 2"
                        _ -> assertFailure ("expected two split units, got " <> show (length units))
        , testCase "script parsing is deterministic" $ do
            let sql =
                    BSC.pack
                        "WITH x AS (SELECT 1) SELECT * FROM x UNION ALL SELECT 2;\nSELECT count(*) FROM t;\n"
            assertEqual "repeat parse" (parseScript duckdbProfile sql) (parseScript duckdbProfile sql)
        , testCase "semicolon-only gaps do not create empty statements" $ do
            stmts <- parseStatements sqliteProfile (BSC.pack ";; SELECT 1;;; SELECT 2;;")
            assertEqual "statement count" 2 (length stmts)
        ]

-- ---------------------------------------------------------------------------
-- Real fuzz corpus
-- ---------------------------------------------------------------------------

corpusProfileFor :: FilePath -> Maybe DialectProfile
corpusProfileFor file
    | "pg_" `isPrefixOf` file = Just postgresqlProfile
    | "sqlite_" `isPrefixOf` file = Just sqliteProfile
    | "mysql_" `isPrefixOf` file = Just mysqlProfile
    | "mariadb_" `isPrefixOf` file = Just mariadbProfile
    | "duckdb_" `isPrefixOf` file = Just duckdbProfile
    | "clickhouse_" `isPrefixOf` file = Just clickhouseProfile
    | otherwise = Nothing

realFuzzCorpusTests :: TestTree
realFuzzCorpusTests =
    testCase "all 71 benchmarks/real_fuzz_corpus/*.sql parse cleanly" $ do
        let dir = "benchmarks/real_fuzz_corpus"
        exists <- doesDirectoryExist dir
        assertBool (dir <> " directory must exist") exists
        entries <- sort . filter ((== ".sql") . takeExtension) <$> listDirectory dir
        assertEqual "expected 71 corpus files" 71 (length entries)
        forM_ entries $ \name -> do
            profile <-
                maybe
                    (assertFailure ("unknown dialect prefix for corpus file " <> name))
                    pure
                    (corpusProfileFor name)
            sql <- BS.readFile (dir </> name)
            case parseScript profile sql of
                Right (ParsedScript stmts) ->
                    assertBool (name <> ": expected at least one parsed statement") (not (null stmts))
                Left err ->
                    assertFailure (name <> ": parseScript failed with " <> show err)

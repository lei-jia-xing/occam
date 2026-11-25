module DependencyChecks.CoreChecks (checks) where

import Data.ByteString.Char8 qualified as BSC
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import DependencyChecks.Support (
    analyze,
    analyzeParsed,
    analyzeWith,
    defaultSessionInit,
    definedColumns,
    dependenciesFor,
    factsFor,
    has,
    hasRead,
    hasStatementRead,
    hasWrite,
    isTruncateKind,
    objectName,
    opsFor,
    readSources,
    readsFor,
    ref,
    renamesFor,
    tableDefinition,
    writesFor,
 )
import Occam.Core (
    BarrierScope (..),
    BeginPolicy (..),
    CatalogOp (..),
    ColumnDef (..),
    ConflictAction (..),
    DataWrite (..),
    Ident (..),
    IndexDef (..),
    IndexRef (..),
    IndexSchemaPlacement (..),
    ReadFact (..),
    ReadMode (..),
    RenameOp (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    StatementId (..),
    TableDef (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEvent (..),
    TriggerRef (..),
    TriggerTiming (..),
    TxEffect (..),
    WriteKind (..),
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements, buildGraph)
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    ServerConfig (..),
    StatementUnit (..),
    defaultParseOptions,
    parseSQL,
 )
import Occam.Slicer (backwardSlice)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import TestSupport (emptySemanticFacts)

checks :: [TestTree]
checks =
    [ testCase "schema collisions and quoted PostgreSQL identities stay distinct" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE sales.\"Order.Item\" (id INT);"
                    , "CREATE TABLE archive.\"Order.Item\" (id INT);"
                    , "SELECT * FROM sales.\"Order.Item\";"
                    , "SELECT * FROM sales.\"order.item\";"
                    , "CREATE TABLE alpha.items (id INT);"
                    , "CREATE TABLE beta.items (id INT);"
                    , "SELECT * FROM items;"
                    , "SELECT * FROM alpha.items;"
                    ]
        graph <- analyze PostgreSQL sql
        let readDeps = dependenciesFor (StatementId 3) graph
            wrongCaseReads = dependenciesFor (StatementId 4) graph
            unqualifiedReads = dependenciesFor (StatementId 7) graph
            qualifiedReads = dependenciesFor (StatementId 8) graph
        assertBool
            "quoted dots stay inside one identifier and quoted PostgreSQL case is preserved"
            (isJust (tableDefinition ["sales", "Order.Item"] (opsFor (StatementId 1) graph)))
        assertBool
            "qualified read depends on only its schema's table"
            (has (StatementId 1) readDeps && not (has (StatementId 2) readDeps))
        assertBool
            "quoted case mismatch does not bind to the stored object"
            (not (has (StatementId 1) wrongCaseReads))
        assertBool
            "unknown search_path conservatively includes both schemas"
            (has (StatementId 5) unqualifiedReads && has (StatementId 6) unqualifiedReads)
        assertBool
            "qualified read excludes the colliding schema"
            (has (StatementId 5) qualifiedReads && not (has (StatementId 6) qualifiedReads))
    , testCase "unresolved column qualifiers do not bind to every table" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE actual (id INT);"
                    , "SELECT missing.id FROM actual;"
                    ]
        graph <- analyze GenericSQL sql
        let readFacts = readsFor (StatementId 2) graph
        assertBool
            "an unresolved qualifier emits no column use"
            (all (null . readColumns) readFacts)
        assertBool
            "the table-level read is still recorded"
            (hasRead ["actual"] (opsFor (StatementId 2) graph))
        assertBool
            "the query still depends on the table definition"
            (has (StatementId 1) (dependenciesFor (StatementId 2) graph))
    , testCase "invalid SQL units retain conservative dependency barriers" $ do
        let sql = BSC.pack "SELECT FROM ; CREATE TABLE t (id INT); SELECT id FROM t;"
        result <- parseSQL (defaultParseOptions SQLite) sql
        parsed <- case result of
            Left err -> assertFailure ("SQL parser transport error: " ++ show err)
            Right value -> pure value
        case parsedUnits parsed of
            [invalid, _, _] -> do
                assertBool "invalid SQL has no parser facts" (isNothing (unitFacts invalid))
                assertBool "invalid SQL exposes its parse error" (isJust (unitParseError invalid))
                let graph =
                        analyzeStatements
                            (parsedInitialSession parsed)
                            (parsedUnits parsed)
                assertBool "invalid SQL is counted as a conservative barrier" (graphBarrierStatements graph > 0)
            units -> assertFailure ("expected three parser units, got " ++ show (length units))
    , testCase "SQLite and MySQL identifier folding is case-insensitive" $ do
        sqliteGraph <-
            analyze
                SQLite
                (BSC.unlines ["CREATE TABLE main.\"MiXeD\" (id INT);", "SELECT * FROM main.mixed;"])
        let mysqlOptions =
                (defaultParseOptions MySQL)
                    { parseServerConfig =
                        (parseServerConfig (defaultParseOptions MySQL))
                            { serverLowerCaseTableNames = 1
                            }
                    }
        mysqlGraph <-
            analyzeWith
                mysqlOptions
                (BSC.unlines ["CREATE TABLE `Db`.`MiXeD` (id INT);", "SELECT * FROM db.mixed;"])
        assertBool
            "SQLite quoted identifier case folds"
            (has (StatementId 1) (dependenciesFor (StatementId 2) sqliteGraph))
        assertBool
            "MySQL backtick identifier case folds when lower_case_table_names = 1"
            (has (StatementId 1) (dependenciesFor (StatementId 2) mysqlGraph))
    , testCase "nested CTEs shadow base tables lexically" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE base (id INT);"
                    , "CREATE TABLE inside_cte (id INT);"
                    , "CREATE TABLE nested_cte (id INT);"
                    , "WITH base AS (SELECT id FROM inside_cte)"
                    , "SELECT id FROM base WHERE EXISTS ("
                    , "  WITH base AS (SELECT id FROM nested_cte)"
                    , "  SELECT id FROM base"
                    , ");"
                    ]
        graph <- analyze PostgreSQL sql
        let deps = dependenciesFor (StatementId 4) graph
        assertEqual
            "CTE references are not external table reads"
            (Set.fromList ["inside_cte", "nested_cte"])
            (readSources (StatementId 4) graph)
        assertBool
            "the nested query does not bind to the shadowed base table"
            (not (has (StatementId 1) deps))
        assertBool
            "both CTE definitions remain dependencies"
            (has (StatementId 2) deps && has (StatementId 3) deps)
    , testCase "DROP and CREATE start a fresh table and column generation" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (old_col INT);"
                    , "INSERT INTO t VALUES (1);"
                    , "DROP TABLE t;"
                    , "CREATE TABLE t (new_col INT);"
                    , "SELECT new_col FROM t;"
                    ]
        graph <- analyze SQLite sql
        let deps = dependenciesFor (StatementId 5) graph
        assertBool "query uses the recreated generation" (has (StatementId 4) deps)
        assertBool "old data writes do not leak into the new generation" (not (has (StatementId 2) deps))
        assertBool
            "old column definitions do not leak into the new generation"
            (not (has (StatementId 1) deps))
    , testCase "table rename preserves its schema and generation" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE accounting.old_name (id INT);"
                    , "INSERT INTO accounting.old_name VALUES (1);"
                    , "ALTER TABLE accounting.old_name RENAME TO new_name;"
                    , "SELECT id FROM accounting.new_name;"
                    ]
        graph <- analyze PostgreSQL sql
        assertBool
            "rename target retains the source schema"
            ( RenameRelation
                (ref ["accounting", "old_name"])
                (objectName ["accounting", "new_name"])
                IndexTableSchema
                `elem` renamesFor (StatementId 3) graph
            )
        assertBool
            "later query resolves against the renamed generation"
            (hasRead ["accounting", "new_name"] (opsFor (StatementId 4) graph))
        assertBool
            "later query depends on rename"
            (has (StatementId 3) (dependenciesFor (StatementId 4) graph))
    , testCase "temporary tables shadow unqualified names, not explicit schemas" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE main.items (id INT);"
                    , "CREATE TABLE temp.items (id INT);"
                    , "SELECT * FROM items;"
                    , "SELECT * FROM main.items;"
                    ]
        graph <- analyze SQLite sql
        let unqualified = dependenciesFor (StatementId 3) graph
            qualified = dependenciesFor (StatementId 4) graph
        assertBool
            "unqualified lookup uses the temporary object"
            (has (StatementId 2) unqualified && not (has (StatementId 1) unqualified))
        assertBool
            "explicit schema lookup excludes the temporary object"
            (has (StatementId 1) qualified && not (has (StatementId 2) qualified))
    , testCase "INSERT SELECT writes its target and only reads its source" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE destination (id INT);"
                    , "CREATE TABLE source (id INT);"
                    , "INSERT INTO destination (id) SELECT id FROM source;"
                    , "SELECT id FROM source;"
                    ]
        graph <- analyze PostgreSQL sql
        let writes = writesFor (StatementId 3) graph
            deps = dependenciesFor (StatementId 3) graph
        assertBool
            "only the insert target is modified"
            ( hasWrite
                ["destination"]
                (InsertWrite ConflictError)
                (opsFor (StatementId 3) graph)
            )
        assertEqual
            "the SELECT input is the only source read"
            (Set.singleton "source")
            (readSources (StatementId 3) graph)
        assertEqual
            "only explicit target columns are written"
            [Ident "id"]
            (concatMap dataWriteColumns writes)
        assertEqual
            "source reads do not depend on the insert"
            (Set.singleton (StatementId 2))
            (dependenciesFor (StatementId 4) graph)
        assertBool
            "both relation definitions are necessary"
            (has (StatementId 1) deps && has (StatementId 2) deps)
    , testCase "TRUNCATE records a table write" $ do
        let sql = BSC.unlines ["CREATE TABLE items (id INT);", "TRUNCATE TABLE items;"]
        graph <- analyze GenericSQL sql
        let writes = writesFor (StatementId 2) graph
        assertBool
            "TRUNCATE modifies its target table"
            ( any
                (\write -> dataWriteTarget write == ref ["items"] && isTruncateKind (dataWriteKind write))
                writes
            )
        assertBool
            "TRUNCATE depends on the table definition"
            (has (StatementId 1) (dependenciesFor (StatementId 2) graph))
    , testCase "savepoint rollback restores reaching writes and session settings fence state" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (x INT);"
                    , "BEGIN;"
                    , "INSERT INTO t VALUES (1);"
                    , "SAVEPOINT s;"
                    , "UPDATE t SET x = 2;"
                    , "ROLLBACK TO SAVEPOINT s;"
                    , "SELECT * FROM t;"
                    , "ROLLBACK;"
                    , "SELECT * FROM t;"
                    , "SET search_path TO tenant;"
                    , "SELECT * FROM t;"
                    ]
        graph <- analyze PostgreSQL sql
        let savepointTx = factPostTx (factsFor (StatementId 6) graph)
            beginTx = factPreTx (factsFor (StatementId 2) graph)
            rollbackTx = factPostTx (factsFor (StatementId 8) graph)
            afterSavepointRollback = dependenciesFor (StatementId 7) graph
            afterTransactionRollback = dependenciesFor (StatementId 9) graph
            afterSessionChange = dependenciesFor (StatementId 11) graph
        assertBool
            "BEGIN is recorded as a pre-transaction effect"
            (TxBegin BeginWarn `elem` beginTx)
        assertBool
            "ROLLBACK is recorded as a post-transaction effect"
            (TxRollbackAll `elem` rollbackTx)
        assertBool
            "ROLLBACK TO is recognized from this statement, not the whole script"
            (TxRollbackTo (Ident "s") `elem` savepointTx)
        assertBool
            "savepoint snapshot retains the earlier write"
            (has (StatementId 3) afterSavepointRollback)
        assertBool
            "savepoint rollback removes the later write from reaching state"
            (not (has (StatementId 5) afterSavepointRollback))
        assertBool
            "full rollback removes transaction writes from reaching state"
            ( not (has (StatementId 3) afterTransactionRollback)
                && not (has (StatementId 5) afterTransactionRollback)
            )
        assertBool
            "session setting is a barrier for later statements"
            (has (StatementId 10) afterSessionChange)
    , testCase "unknown statement syntax becomes a conservative global barrier" $ do
        let sql = BSC.unlines ["CREATE TABLE t (id INT);", "CREATE EXTENSION hstore;", "SELECT * FROM t;"]
        graph <- analyze PostgreSQL sql
        assertBool
            "unknown statement depends on all preceding state"
            (has (StatementId 1) (dependenciesFor (StatementId 2) graph))
        assertBool
            "the unknown statement is a global barrier"
            (any ((== GlobalScope) . barrierScope) (factBarriers (factsFor (StatementId 2) graph)))
        assertBool
            "following reads remain fenced by unknown effects"
            (has (StatementId 2) (dependenciesFor (StatementId 3) graph))
        assertBool
            "barrier metrics include the unsupported operation"
            (graphBarrierStatements graph > 0 && graphBarrierActions graph > 0)
    , testCase "opaque effects preserve hidden sources and scalar writes" $
        mapM_
            ( \(dialect, operation, relation) -> do
                graph <-
                    analyzeParsed dialect $
                        BSC.unlines
                            [ "CREATE TABLE t (a INT);"
                            , operation
                            , "INSERT INTO t VALUES (42);"
                            , "SELECT 0;"
                            , "SELECT * FROM " <> relation <> ";"
                            ]
                let readDeps = backwardSlice (StatementId 5) graph
                assertBool
                    ("hidden reads retain the opaque operation: " ++ BSC.unpack operation)
                    (has (StatementId 2) readDeps)
                assertBool
                    ("hidden reads retain writes after the opaque operation: " ++ BSC.unpack operation)
                    (has (StatementId 3) readDeps)
            )
            -- INHERITS is fully modeled rather than opaque.
            [ (PostgreSQL, "CREATE EXTENSION hstore;", "hidden_view")
            , (PostgreSQL, "GRANT SELECT ON t TO PUBLIC;", "hidden_view")
            , (PostgreSQL, "SELECT mutate_t();", "t")
            , (SQLite, "PRAGMA recursive_triggers=0;", "hidden_view")
            ]
    , testCase "view reads retain indexes created after the view" $ do
        graph <-
            analyze SQLite $
                BSC.unlines
                    [ "CREATE TABLE t (x INT);"
                    , "CREATE VIEW v AS SELECT * FROM t;"
                    , "CREATE INDEX ix ON t (x);"
                    , "INSERT INTO t VALUES (42);"
                    , "SELECT * FROM v;"
                    ]
        assertEqual
            "view sources include their current data and index state"
            (Set.fromList [StatementId 1, StatementId 2, StatementId 3, StatementId 4])
            (dependenciesFor (StatementId 5) graph)
    , testCase "unmodeled writes through known views fence subsequent reads" $ do
        graph <-
            analyze
                PostgreSQL
                "CREATE TABLE t (x INT); CREATE VIEW v AS SELECT * FROM t; INSERT INTO v VALUES (42); SELECT * FROM t;"
        assertEqual
            "view writes retain the known source and view"
            (Set.fromList [StatementId 1, StatementId 2])
            (dependenciesFor (StatementId 3) graph)
        assertEqual
            "base-table reads retain the conservative view write"
            (Set.fromList [StatementId 1, StatementId 3])
            (dependenciesFor (StatementId 4) graph)
        assertBool "catalog-sensitive view writes count as barriers" (graphBarrierStatements graph > 0)
    , testCase "transaction rollback restores column and index ownership" $ do
        graph <-
            analyze
                SQLite
                "CREATE TABLE t (x INT); CREATE INDEX ix ON t (x); BEGIN; ALTER TABLE t RENAME TO u; ROLLBACK; SELECT x FROM t;"
        assertEqual
            "the restored table and index, not the rename, reach the read directly"
            (Set.fromList [StatementId 1, StatementId 2, StatementId 5])
            (dependenciesFor (StatementId 6) graph)
    , testCase "table writes retain related indexes, triggers, and column definitions" $ do
        let table =
                TableDef
                    (objectName ["t"])
                    False
                    True
                    [ColumnDef (Ident "id") False False False []]
                    []
                    []
                    Nothing
                    Nothing
                    []
                    Nothing
                    False
                    False
            index =
                IndexDef
                    (NamedIndex (objectName ["ix_t_id"]))
                    (ref ["t"])
                    [Ident "id"]
                    False
                    False
                    False
                    False
                    IndexTableSchema
            trigger =
                TriggerDef
                    (NamedTrigger (objectName ["tr_t"]))
                    (ref ["t"])
                    TriggerAfter
                    [TriggerInsert]
                    False
                    []
                    (TriggerInline [])
                    Nothing
                    False
                    False
                    False
            graph =
                buildGraph
                    defaultSessionInit
                    [ (StatementId 1, emptySemanticFacts {factOps = [DefineTableOp table]})
                    , (StatementId 2, emptySemanticFacts {factOps = [DefineIndexOp index]})
                    , (StatementId 3, emptySemanticFacts {factOps = [DefineTriggerOp trigger]})
                    ,
                        ( StatementId 4
                        , emptySemanticFacts
                            { factOps =
                                [ WriteDataOp
                                    (DataWrite (ref ["t"]) (InsertWrite ConflictError) [Ident "id"] [] [] False)
                                ]
                            }
                        )
                    ,
                        ( StatementId 5
                        , emptySemanticFacts
                            { factOps = [ReadOp (ReadFact (ref ["t"]) ReadData [Ident "id"] False)]
                            }
                        )
                    ]
            writeDeps = dependenciesFor (StatementId 4) graph
            readDeps = dependenciesFor (StatementId 5) graph
        assertBool
            "table writes depend on index and trigger definitions"
            (has (StatementId 2) writeDeps && has (StatementId 3) writeDeps)
        assertBool
            "reads depend on index and latest column reaching definition"
            (has (StatementId 2) readDeps && has (StatementId 4) readDeps)
    , testCase "DuckDB dependency extraction handles table references and closures" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE duck_src (id INT);"
                    , "CREATE TABLE duck_target AS SELECT * FROM duck_src;"
                    , "CREATE TABLE duck_other (id INT);"
                    , "SELECT id FROM duck_target;"
                    ]
        graph <- analyze DuckDB sql
        let depsS2 = dependenciesFor (StatementId 2) graph
            depsS4 = dependenciesFor (StatementId 4) graph
        assertBool "duck_target creation depends on duck_src" (has (StatementId 1) depsS2)
        assertBool "SELECT from duck_target depends on duck_target" (has (StatementId 2) depsS4)
        assertBool
            "SELECT from duck_target has no dependency on duck_other"
            (not (has (StatementId 3) depsS4))
    , testCase "ClickHouse dependency extraction preserves unquoted case and engine dependencies" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE EventsLog (EventId UInt64) ENGINE = MergeTree() ORDER BY EventId;"
                    , "CREATE TABLE OtherLog (OtherId UInt64) ENGINE = MergeTree() ORDER BY OtherId;"
                    , "SELECT EventId FROM EventsLog;"
                    ]
        graph <- analyze ClickHouse sql
        let depsS3 = dependenciesFor (StatementId 3) graph
        assertEqual
            "ClickHouse preserves unquoted table and column identifier case"
            ["EventId"]
            (definedColumns ["EventsLog"] (opsFor (StatementId 1) graph))
        assertBool "SELECT EventId depends on EventsLog" (has (StatementId 1) depsS3)
        assertBool "SELECT EventId has no dependency on OtherLog" (not (has (StatementId 2) depsS3))
    , testCase "Oracle dependency extraction handles tables, DUAL, and identifier casing" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE dept (\"DeptId\" NUMBER(10), DeptName VARCHAR2(50));"
                    , "CREATE TABLE other (id NUMBER(10));"
                    , "SELECT \"DeptId\", DeptName FROM dept;"
                    , "SELECT 1 FROM dual;"
                    ]
        graph <- analyze Oracle sql
        let depsS3 = dependenciesFor (StatementId 3) graph
            depsS4 = dependenciesFor (StatementId 4) graph
        assertEqual
            "Oracle folds unquoted identifiers to upper case and preserves quoted identifier case"
            ["DeptId", "DEPTNAME"]
            (definedColumns ["DEPT"] (opsFor (StatementId 1) graph))
        assertBool "SELECT from dept depends on dept" (has (StatementId 1) depsS3)
        assertBool "SELECT from dept does not depend on other" (not (has (StatementId 2) depsS3))
        assertBool
            "SELECT from dual records a read on dual"
            (hasRead ["DUAL"] (opsFor (StatementId 4) graph))
        assertBool
            "SELECT from dual does not depend on other tables"
            (not (has (StatementId 1) depsS4 || has (StatementId 2) depsS4))
    , testCase "MsSql dependency extraction handles bracketed identifiers and schema qualification" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE [dbo].[users] ([UserId] INT, [UserName] NVARCHAR(50));"
                    , "CREATE TABLE [dbo].[other] ([id] INT);"
                    , "SELECT [UserId], [UserName] FROM [dbo].[users];"
                    ]
        graph <- analyze MsSql sql
        let depsS3 = dependenciesFor (StatementId 3) graph
        assertEqual
            "MsSql folds case-insensitive bracketed identifiers"
            ["userid", "username"]
            (definedColumns ["dbo", "users"] (opsFor (StatementId 1) graph))
        assertBool "SELECT from users depends on users" (has (StatementId 1) depsS3)
        assertBool "SELECT from users does not depend on other" (not (has (StatementId 2) depsS3))
    , testCase "triggers do not create global serialization barriers across unrelated tables" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t_target (id INT);"
                    , "CREATE TABLE t_log (msg TEXT);"
                    , "CREATE TABLE t_unrelated (x INT);"
                    , "CREATE TRIGGER tr_target AFTER INSERT ON t_target BEGIN INSERT INTO t_log VALUES ('new'); END;"
                    , "SELECT x FROM t_unrelated;"
                    ]
        graph <- analyze SQLite sql
        let unrelatedDeps = dependenciesFor (StatementId 5) graph
        assertBool "SELECT from unrelated depends on unrelated" (has (StatementId 3) unrelatedDeps)
        assertBool
            "SELECT from unrelated does not depend on trigger"
            (not (has (StatementId 4) unrelatedDeps))
        assertBool
            "SELECT from unrelated does not depend on t_target"
            (not (has (StatementId 1) unrelatedDeps))
        assertBool
            "SELECT from unrelated does not depend on t_log"
            (not (has (StatementId 2) unrelatedDeps))
    , testCase "transactions do not create global serialization barriers across unrelated tables" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t_unrelated (id INT);"
                    , "BEGIN;"
                    , "CREATE TABLE t_in_tx (val INT);"
                    , "INSERT INTO t_in_tx VALUES (42);"
                    , "COMMIT;"
                    , "SELECT id FROM t_unrelated;"
                    ]
        graph <- analyze PostgreSQL sql
        let queryDeps = dependenciesFor (StatementId 6) graph
        assertBool "Query depends on t_unrelated" (has (StatementId 1) queryDeps)
        assertBool "Query does not depend on BEGIN" (not (has (StatementId 2) queryDeps))
        assertBool "Query does not depend on t_in_tx" (not (has (StatementId 3) queryDeps))
        assertBool "Query does not depend on INSERT" (not (has (StatementId 4) queryDeps))
        assertBool "Query does not depend on COMMIT" (not (has (StatementId 5) queryDeps))
    , testCase "ON COMMIT DELETE ROWS clears write dependencies after rollback and commit" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TEMP TABLE t (x INT) ON COMMIT DELETE ROWS;"
                    , "BEGIN;"
                    , "INSERT INTO t VALUES (1);"
                    , "ROLLBACK;"
                    , "BEGIN;"
                    , "INSERT INTO t VALUES (2);"
                    , "SELECT x FROM t;"
                    , "COMMIT;"
                    , "SELECT x FROM t;"
                    ]
        graph <- analyze PostgreSQL sql
        let beforeCommit = dependenciesFor (StatementId 7) graph
            afterCommit = dependenciesFor (StatementId 9) graph
        assertBool "the active write is needed before commit" (has (StatementId 6) beforeCommit)
        assertBool "the active write is cleared on commit" (not (has (StatementId 6) afterCommit))
        assertBool "the temporary table still exists" (has (StatementId 1) afterCommit)
    , testCase "MERGE statement extracts precise writes on target and reads on source" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE target_tbl (id INT, val INT);"
                    , "CREATE TABLE source_tbl (id INT, val INT);"
                    , "CREATE TABLE untouched_tbl (id INT);"
                    , "MERGE INTO target_tbl USING source_tbl ON target_tbl.id = source_tbl.id WHEN MATCHED THEN UPDATE SET val = source_tbl.val;"
                    ]
        graph <- analyze GenericSQL sql
        let mergeDeps = dependenciesFor (StatementId 4) graph
        assertBool
            "MERGE has a write on target_tbl"
            (hasWrite ["target_tbl"] MergeWrite (opsFor (StatementId 4) graph))
        assertBool
            "MERGE has a read on source_tbl"
            (hasStatementRead ["source_tbl"] (StatementId 4) graph)
        assertBool "MERGE depends on target_tbl definition" (has (StatementId 1) mergeDeps)
        assertBool "MERGE depends on source_tbl definition" (has (StatementId 2) mergeDeps)
        assertBool "MERGE does not depend on untouched_tbl" (not (has (StatementId 3) mergeDeps))
    ]

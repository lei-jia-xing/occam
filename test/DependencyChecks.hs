{-# LANGUAGE OverloadedStrings #-}

module DependencyChecks (tests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, maybeToList)
import Data.Set qualified as Set
import Occam.Core (DependencyType (..), ObjectAction (..), ObjectKind (..), StatementId (..))
import Occam.Dependency (DependencyGraph (..), analyzeStatements, buildGraph)
import Occam.Parser (ParsedSQL (..), SQLDialect (..), StatementUnit (..), parseSQL)
import Test.QuickCheck (Gen, Property, arbitrary, chooseInt, counterexample, forAll, listOf, resize)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "Dependency analysis"
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
            let names = actionsFor (StatementId 1) graph
                readDeps = dependenciesFor (StatementId 3) graph
                wrongCaseReads = dependenciesFor (StatementId 4) graph
                unqualifiedReads = dependenciesFor (StatementId 7) graph
                qualifiedReads = dependenciesFor (StatementId 8) graph
            assertBool
                "quoted dots stay inside one identifier and quoted PostgreSQL case is preserved"
                (ObjectAction TableObject DefDep "sales.Order\\.Item" `elem` names)
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
        , testCase "invalid SQL units retain conservative dependency barriers" $ do
            let sql = BSC.pack "SELECT FROM ; CREATE TABLE t (id INT); SELECT id FROM t;"
            result <- parseSQL SQLite sql
            parsed <- case result of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right value -> pure value
            case parsedUnits parsed of
                [invalid, _, _] -> do
                    assertBool "invalid SQL has no parser facts" (isNothing (unitFacts invalid))
                    assertBool "invalid SQL exposes its parse error" (isJust (unitParseError invalid))
                    let graph = analyzeStatements (parsedUnits parsed)
                    assertBool "invalid SQL is counted as a conservative barrier" (graphBarrierStatements graph > 0)
                units -> assertFailure ("expected three parser units, got " ++ show (length units))
        , testCase "SQLite and MySQL identifier folding is case-insensitive" $ do
            sqliteGraph <-
                analyze
                    SQLite
                    (BSC.unlines ["CREATE TABLE main.\"MiXeD\" (id INT);", "SELECT * FROM main.mixed;"])
            mysqlGraph <-
                analyze
                    MySQL
                    (BSC.unlines ["CREATE TABLE `Db`.`MiXeD` (id INT);", "SELECT * FROM db.mixed;"])
            assertBool
                "SQLite quoted identifier case folds"
                (has (StatementId 1) (dependenciesFor (StatementId 2) sqliteGraph))
            assertBool
                "MySQL backtick identifier case folds"
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
            let queryActions = actionsFor (StatementId 4) graph
                tableReads =
                    Set.fromList
                        [ actionName action
                        | action <- queryActions
                        , actionKind action == TableObject
                        , actionType action == UseDep
                        ]
                deps = dependenciesFor (StatementId 4) graph
            assertEqual
                "CTE references are not external table reads"
                (Set.fromList ["inside_cte", "nested_cte"])
                tableReads
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
            let renameActions = actionsFor (StatementId 3) graph
                queryActions = actionsFor (StatementId 4) graph
            assertBool
                "rename target retains the source schema"
                (ObjectAction TableObject RenameToDep "accounting.new_name" `elem` renameActions)
            assertBool
                "later query resolves against the renamed generation"
                (ObjectAction TableObject UseDep "accounting.new_name" `elem` queryActions)
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
            let actions = actionsFor (StatementId 3) graph
                tableActions = [action | action <- actions, actionKind action == TableObject]
                writes = Set.fromList [actionName action | action <- tableActions, actionType action == ModDep]
                readTables = Set.fromList [actionName action | action <- tableActions, actionType action == UseDep]
                columnWrites =
                    Set.fromList
                        [ actionName action
                        | action <- actions
                        , actionKind action == ColumnObject
                        , actionType action == ModDep
                        ]
                deps = dependenciesFor (StatementId 3) graph
            assertEqual "only the insert target is modified" (Set.singleton "destination") writes
            assertEqual "the SELECT input is a source read" (Set.singleton "source") readTables
            assertEqual
                "only explicit target columns are written"
                (Set.singleton "destination\x1fid")
                columnWrites
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
            let actions = actionsFor (StatementId 2) graph
            assertBool
                "TRUNCATE modifies its target table"
                (ObjectAction TableObject ModDep "items" `elem` actions)
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
            let savepointActions = actionsFor (StatementId 6) graph
                afterSavepointRollback = dependenciesFor (StatementId 7) graph
                afterTransactionRollback = dependenciesFor (StatementId 9) graph
                afterSessionChange = dependenciesFor (StatementId 11) graph
            assertBool
                "ROLLBACK TO is recognized from this statement, not the whole script"
                (ObjectAction TransactionObject TxRollbackToDep "s" `elem` savepointActions)
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
        , testCase "releasing an outer SQLite savepoint commits its writes" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (x INT);"
                        , "SAVEPOINT s;"
                        , "INSERT INTO t VALUES (1);"
                        , "RELEASE s;"
                        , "ROLLBACK;"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze SQLite sql
            let releaseActions = actionsFor (StatementId 4) graph
            assertBool
                "RELEASE is extracted from its statement"
                (ObjectAction TransactionObject TxReleaseDep "s" `elem` releaseActions)
            assertBool
                "released savepoint writes remain in the catalog"
                (has (StatementId 3) (dependenciesFor (StatementId 6) graph))
        , testCase "unknown schema syntax becomes a conservative sequence barrier" $ do
            let sql = BSC.unlines ["CREATE TABLE t (id INT);", "CREATE SCHEMA extra;", "SELECT * FROM t;"]
            graph <- analyze PostgreSQL sql
            assertBool
                "unknown statement depends on all preceding state"
                (has (StatementId 1) (dependenciesFor (StatementId 2) graph))
            assertBool
                "following reads remain fenced by unknown effects"
                (has (StatementId 2) (dependenciesFor (StatementId 3) graph))
            assertBool
                "barrier metrics include the unsupported schema operation"
                (graphBarrierStatements graph > 0 && graphBarrierActions graph > 0)
        , testCase "temporary definitions preserve permanent column and index ownership" $ do
            graph <-
                analyze SQLite $
                    BSC.unlines
                        [ "CREATE TABLE main.t (x INT);"
                        , "CREATE INDEX ix ON t (x);"
                        , "CREATE TEMP TABLE t (x INT);"
                        , "SELECT * FROM main.t;"
                        , "SELECT x FROM main.t;"
                        ]
            assertBool
                "index name is not confused with its target table"
                (ObjectAction IndexObject DefDep "ix" `elem` actionsFor (StatementId 2) graph)
            assertEqual
                "index creation reads its table"
                (Set.singleton (StatementId 1))
                (dependenciesFor (StatementId 2) graph)
            let expected = Set.fromList [StatementId 1, StatementId 2]
            assertEqual
                "explicit permanent lookup retains its index"
                expected
                (dependenciesFor (StatementId 4) graph)
            assertEqual
                "temporary columns do not replace permanent columns"
                expected
                (dependenciesFor (StatementId 5) graph)
        , testCase "dropping a temporary table reveals permanent reaching writes" $ do
            graph <-
                analyze SQLite $
                    BSC.unlines
                        [ "CREATE TABLE main.t (x INT);"
                        , "INSERT INTO main.t VALUES (42);"
                        , "CREATE TEMP TABLE t (x INT);"
                        , "DROP TABLE t;"
                        , "SELECT x FROM t;"
                        ]
            assertEqual
                "the dropped shadow does not hide the live permanent object"
                (Set.fromList [StatementId 1, StatementId 2, StatementId 4])
                (dependenciesFor (StatementId 5) graph)
        , testCase "temporary rename does not redirect a bound permanent view" $ do
            graph <-
                analyze SQLite $
                    BSC.unlines
                        [ "CREATE TABLE main.t (x INT);"
                        , "CREATE VIEW v AS SELECT * FROM t;"
                        , "CREATE TEMP TABLE t (x INT);"
                        , "ALTER TABLE t RENAME TO u;"
                        , "INSERT INTO main.t VALUES (42);"
                        , "SELECT * FROM v;"
                        ]
            assertEqual
                "the view remains bound to its definition-time source"
                (Set.fromList [StatementId 1, StatementId 2, StatementId 5])
                (dependenciesFor (StatementId 6) graph)
        , testCase "qualified recreation retires an unqualified dropped generation" $ do
            graph <-
                analyze SQLite $
                    BSC.unlines
                        [ "CREATE TABLE \"t.part\" (old_col INT);"
                        , "DROP TABLE \"t.part\";"
                        , "CREATE TABLE main.\"t.part\" (new_col INT);"
                        , "SELECT new_col FROM main.\"t.part\";"
                        ]
            assertEqual
                "only the new definition reaches the qualified query"
                (Set.singleton (StatementId 3))
                (dependenciesFor (StatementId 4) graph)
        , testCase "nested-only CTE aliases do not shadow an outer table read" $ do
            graph <-
                analyze PostgreSQL $
                    BSC.unlines
                        [ "CREATE TABLE base (id INT);"
                        , "CREATE TABLE src (id INT);"
                        , "SELECT * FROM base WHERE EXISTS (WITH base AS (SELECT id FROM src) SELECT 1 FROM base);"
                        ]
            assertEqual
                "both outer and nested external sources remain visible"
                (Set.fromList [StatementId 1, StatementId 2])
                (dependenciesFor (StatementId 3) graph)
        , testCase "a nonrecursive CTE body can read its same-named base table" $ do
            graph <-
                analyze PostgreSQL "CREATE TABLE t (id INT); WITH t AS (SELECT id FROM t) SELECT id FROM t;"
            assertEqual
                "a CTE alias is not visible in its own nonrecursive body"
                (Set.singleton (StatementId 1))
                (dependenciesFor (StatementId 2) graph)
        , testCase "quoted dotted CTE aliases retain their identity" $ do
            graph <-
                analyze
                    PostgreSQL
                    "CREATE TABLE \"a.b\" (id INT); WITH \"a.b\" AS (SELECT 42 AS id) SELECT id FROM \"a.b\";"
            assertEqual
                "the quoted CTE shadows the quoted base table"
                Set.empty
                (dependenciesFor (StatementId 2) graph)
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
        , testCase "savepoint rollback restores a dropped schema generation and its data" $ do
            graph <-
                analyze SQLite $
                    BSC.unlines
                        [ "CREATE TABLE t (old_col INT);"
                        , "CREATE INDEX ix ON t (old_col);"
                        , "INSERT INTO t VALUES (42);"
                        , "SAVEPOINT s;"
                        , "DROP TABLE t;"
                        , "CREATE TABLE t (new_col INT);"
                        , "INSERT INTO t VALUES (99);"
                        , "ROLLBACK TO s;"
                        , "SELECT old_col FROM t;"
                        , "RELEASE s;"
                        ]
            assertEqual
                "only the restored generation and rollback reach the read directly"
                (Set.fromList [StatementId 1, StatementId 2, StatementId 3, StatementId 8])
                (dependenciesFor (StatementId 9) graph)
        , testCase "table writes retain related indexes, triggers, and column definitions" $ do
            let table = ObjectAction TableObject DefDep "t"
                column = ObjectAction ColumnObject DefDep "t\x1fid"
                index = ObjectAction IndexObject DefDep "ix_t_id"
                trigger = ObjectAction TriggerObject DefDep "tr_t"
                graph =
                    buildGraph
                        [ (StatementId 1, [table, column])
                        , (StatementId 2, [ObjectAction TableObject UseDep "t", index])
                        , (StatementId 3, [ObjectAction TableObject UseDep "t", trigger])
                        , (StatementId 4, [ObjectAction TableObject ModDep "t", ObjectAction ColumnObject ModDep "t\x1fid"])
                        , (StatementId 5, [ObjectAction TableObject UseDep "t", ObjectAction ColumnObject UseDep "t\x1fid"])
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
            let actionsS1 = actionsFor (StatementId 1) graph
                depsS3 = dependenciesFor (StatementId 3) graph
            assertBool
                "ClickHouse preserves unquoted table and column identifier case"
                ( ObjectAction TableObject DefDep "EventsLog" `elem` actionsS1
                    && ObjectAction ColumnObject DefDep "EventsLog\USEventId" `elem` actionsS1
                )
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
            let actionsS1 = actionsFor (StatementId 1) graph
                actionsS4 = actionsFor (StatementId 4) graph
                depsS3 = dependenciesFor (StatementId 3) graph
                depsS4 = dependenciesFor (StatementId 4) graph
            assertBool
                "Oracle normalizes unquoted identifiers to lower case and preserves quoted identifier case"
                ( ObjectAction TableObject DefDep "dept" `elem` actionsS1
                    && ObjectAction ColumnObject DefDep "dept\USDeptId" `elem` actionsS1
                    && ObjectAction ColumnObject DefDep "dept\USdeptname" `elem` actionsS1
                )
            assertBool "SELECT from dept depends on dept" (has (StatementId 1) depsS3)
            assertBool "SELECT from dept does not depend on other" (not (has (StatementId 2) depsS3))
            assertBool
                "SELECT from dual records UseDep on dual"
                (ObjectAction TableObject UseDep "dual" `elem` actionsS4)
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
            let actionsS1 = actionsFor (StatementId 1) graph
                depsS3 = dependenciesFor (StatementId 3) graph
            assertBool
                "MsSql captures DefDep for bracketed table and columns"
                ( ObjectAction TableObject DefDep "dbo.users" `elem` actionsS1
                    && ObjectAction ColumnObject DefDep "dbo.users\USUserId" `elem` actionsS1
                )
            assertBool "SELECT from users depends on users" (has (StatementId 1) depsS3)
            assertBool "SELECT from users does not depend on other" (not (has (StatementId 2) depsS3))
        , testCase "session SET does not create false dependencies on prior unrelated tables" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE unneeded_tbl (id INT);"
                        , "CREATE TABLE needed_tbl (id INT, val INT);"
                        , "SET enable_seqscan = off;"
                        , "SELECT val FROM needed_tbl;"
                        ]
            graph <- analyze PostgreSQL sql
            let setDeps = dependenciesFor (StatementId 3) graph
                queryDeps = dependenciesFor (StatementId 4) graph
            assertBool "SET does not depend on unneeded_tbl" (not (has (StatementId 1) setDeps))
            assertBool "SET does not depend on needed_tbl" (not (has (StatementId 2) setDeps))
            assertBool "Query depends on needed_tbl" (has (StatementId 2) queryDeps)
            assertBool "Query does not depend on unneeded_tbl through SET" (not (has (StatementId 1) queryDeps))
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
        , testCase "MERGE statement extracts precise ModDep on target and UseDep on source" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE target_tbl (id INT, val INT);"
                        , "CREATE TABLE source_tbl (id INT, val INT);"
                        , "CREATE TABLE untouched_tbl (id INT);"
                        , "MERGE INTO target_tbl USING source_tbl ON target_tbl.id = source_tbl.id WHEN MATCHED THEN UPDATE SET val = source_tbl.val;"
                        ]
            graph <- analyze GenericSQL sql
            let mergeDeps = dependenciesFor (StatementId 4) graph
                actions = actionsFor (StatementId 4) graph
            assertBool
                "MERGE has ModDep on target_tbl"
                (ObjectAction TableObject ModDep "target_tbl" `elem` actions)
            assertBool
                "MERGE has UseDep on source_tbl"
                (ObjectAction TableObject UseDep "source_tbl" `elem` actions)
            assertBool "MERGE depends on target_tbl definition" (has (StatementId 1) mergeDeps)
            assertBool "MERGE depends on source_tbl definition" (has (StatementId 2) mergeDeps)
            assertBool "MERGE does not depend on untouched_tbl" (not (has (StatementId 3) mergeDeps))
        , testCase "cascading foreign keys propagate delete writes to child tables" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (id INT, p_id INT REFERENCES parent(id) ON DELETE CASCADE);"
                        , "CREATE TABLE unrelated (x INT);"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT id FROM child;"
                        , "SELECT x FROM unrelated;"
                        ]
            graph <- analyze GenericSQL sql
            let childQueryDeps = dependenciesFor (StatementId 5) graph
                unrelatedQueryDeps = dependenciesFor (StatementId 6) graph
            assertBool "Child query depends on parent deletion via cascade" (has (StatementId 4) childQueryDeps)
            assertBool "Child query depends on child definition" (has (StatementId 2) childQueryDeps)
            assertBool
                "Unrelated query does not depend on parent deletion"
                (not (has (StatementId 4) unrelatedQueryDeps))
            assertBool "Unrelated query depends on unrelated table" (has (StatementId 3) unrelatedQueryDeps)
        , testCase "multi-level cascading foreign keys propagate to grandchildren" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE grand_parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE parent (id INT PRIMARY KEY, gp_id INT REFERENCES grand_parent(id) ON DELETE CASCADE);"
                        , "CREATE TABLE child (id INT, p_id INT REFERENCES parent(id) ON DELETE CASCADE);"
                        , "DELETE FROM grand_parent WHERE id = 10;"
                        , "SELECT id FROM child;"
                        ]
            graph <- analyze GenericSQL sql
            let childQueryDeps = dependenciesFor (StatementId 5) graph
            assertBool
                "Child query depends on grand_parent deletion via transitive cascade"
                (has (StatementId 4) childQueryDeps)
            assertBool "Child query depends on child definition" (has (StatementId 3) childQueryDeps)
        , testCase "non-cascading foreign keys do not propagate writes to child tables" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (id INT, p_id INT REFERENCES parent(id));"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT id FROM child;"
                        ]
            graph <- analyze GenericSQL sql
            let childQueryDeps = dependenciesFor (StatementId 4) graph
            assertBool
                "Child query does not depend on parent deletion without cascade"
                (not (has (StatementId 3) childQueryDeps))
            assertBool "Child query depends on child definition" (has (StatementId 2) childQueryDeps)
        , testCase "temporary table with ON COMMIT DROP is dropped upon commit" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE persistent_tbl (id INT);"
                        , "BEGIN;"
                        , "CREATE TEMPORARY TABLE temp_drop (id INT) ON COMMIT DROP;"
                        , "COMMIT;"
                        , "SELECT id FROM persistent_tbl;"
                        ]
            graph <- analyze PostgreSQL sql
            let queryDeps = dependenciesFor (StatementId 5) graph
            assertBool "Query depends on persistent table" (has (StatementId 1) queryDeps)
            assertBool "Query does not depend on dropped temp table" (not (has (StatementId 3) queryDeps))
        , testCase "temporary table created inside a rolled-back transaction is not visible after rollback" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE persistent_tbl (id INT);"
                        , "BEGIN;"
                        , "CREATE TEMPORARY TABLE temp_tbl (id INT);"
                        , "ROLLBACK;"
                        , "SELECT id FROM persistent_tbl;"
                        ]
            graph <- analyze SQLite sql
            let queryDeps = dependenciesFor (StatementId 5) graph
            assertBool "Query depends on persistent table" (has (StatementId 1) queryDeps)
            assertBool
                "Query does not depend on rolled-back temp table definition"
                (not (has (StatementId 3) queryDeps))
        , testProperty
            "generated SQLite catalog transitions match a table snapshot model"
            prop_catalogTransitions
        ]

data CatalogPlan = CatalogPlan
    { planTarget :: Int
    , planReverseInitial :: Bool
    , planTransactionWrites :: [Int]
    , planSavepointWrites :: [Int]
    , planAfterRollbackWrites :: [Int]
    , planReleaseWrites :: [Int]
    , planTransactionWritesFirst :: Bool
    , planSavepointWritesFirst :: Bool
    , planReleaseWritesFirst :: Bool
    }
    deriving (Show)

genCatalogPlan :: Gen CatalogPlan
genCatalogPlan =
    CatalogPlan
        <$> chooseInt (0, 1)
        <*> arbitrary
        <*> boundedWrites
        <*> boundedWrites
        <*> boundedWrites
        <*> boundedWrites
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    where
        boundedWrites :: Gen [Int]
        boundedWrites = resize 3 (listOf (chooseInt (0, 1)))

type TableSnapshot = Map.Map Int (StatementId, Maybe StatementId)

data CatalogTrace = CatalogTrace
    { traceStatements :: ![String]
    , traceNextStatement :: !Int
    , traceTables :: !TableSnapshot
    , traceReads :: ![(StatementId, Set.Set StatementId)]
    , traceControls :: !(Set.Set StatementId)
    }

emptyTrace :: CatalogTrace
emptyTrace = CatalogTrace [] 1 Map.empty [] Set.empty

emit :: String -> CatalogTrace -> (StatementId, CatalogTrace)
emit sql trace =
    let sid = StatementId (traceNextStatement trace)
     in ( sid
        , trace
            { traceStatements = sql : traceStatements trace
            , traceNextStatement = traceNextStatement trace + 1
            }
        )

control :: String -> CatalogTrace -> CatalogTrace
control sql trace =
    let (sid, trace') = emit sql trace
     in trace' {traceControls = Set.insert sid (traceControls trace')}

tableName :: Int -> String
tableName 0 = "a"
tableName _ = "b"

createTable :: Int -> CatalogTrace -> CatalogTrace
createTable table trace =
    let (sid, trace') = emit ("CREATE TABLE " ++ tableName table ++ " (id INTEGER);") trace
     in trace' {traceTables = Map.insert table (sid, Nothing) (traceTables trace')}

writeTable :: Int -> CatalogTrace -> CatalogTrace
writeTable table trace =
    let (sid, trace') = emit ("INSERT INTO " ++ tableName table ++ " VALUES (1);") trace
     in trace'
            { traceTables =
                Map.adjust (\(definition, _) -> (definition, Just sid)) table (traceTables trace')
            }

dropTable :: Int -> CatalogTrace -> CatalogTrace
dropTable table trace =
    let (_, trace') = emit ("DROP TABLE " ++ tableName table ++ ";") trace
     in trace' {traceTables = Map.delete table (traceTables trace')}

readTable :: Int -> CatalogTrace -> CatalogTrace
readTable table trace =
    let (sid, trace') = emit ("SELECT * FROM " ++ tableName table ++ ";") trace
     in trace'
            { traceReads =
                (sid, tableDependencies table (traceTables trace')) : traceReads trace'
            }

tableDependencies :: Int -> TableSnapshot -> Set.Set StatementId
tableDependencies table tables =
    case Map.lookup table tables of
        Nothing -> Set.empty
        Just (definition, lastWrite) ->
            Set.fromList (definition : maybeToList lastWrite)

writeMany :: [Int] -> CatalogTrace -> CatalogTrace
writeMany tables trace = foldl' (flip writeTable) trace tables

readBoth :: Int -> Int -> CatalogTrace -> CatalogTrace
readBoth first second = readTable second . readTable first

mutateBlock :: Int -> Int -> [Int] -> Bool -> CatalogTrace -> CatalogTrace
mutateBlock target other extraWrites extrasFirst trace =
    let beforeChange = if extrasFirst then writeMany extraWrites trace else trace
        changed =
            writeTable
                other
                (writeTable target (createTable target (dropTable target beforeChange)))
     in if extrasFirst then changed else writeMany extraWrites changed

catalogTrace :: CatalogPlan -> CatalogTrace
catalogTrace plan =
    releaseSavepoint (rollbackSavepoint (rollbackTransaction initial))
    where
        target = planTarget plan
        other = 1 - target
        initialOrder = if planReverseInitial plan then [1, 0] else [0, 1]
        initial =
            foldl'
                (\trace table -> writeTable table (createTable table trace))
                emptyTrace
                initialOrder

        rollbackTransaction trace =
            let snapshot = traceTables trace
                changed =
                    readBoth target other $
                        mutateBlock
                            target
                            other
                            (planTransactionWrites plan)
                            (planTransactionWritesFirst plan)
                            (control "BEGIN;" trace)
                restored = (control "ROLLBACK;" changed) {traceTables = snapshot}
             in readBoth target other restored

        rollbackSavepoint trace =
            let snapshot = traceTables trace
                changed =
                    readBoth target other $
                        mutateBlock
                            target
                            other
                            (planSavepointWrites plan)
                            (planSavepointWritesFirst plan)
                            (control "SAVEPOINT s;" trace)
                restored = (control "ROLLBACK TO SAVEPOINT s;" changed) {traceTables = snapshot}
                afterRollback = readBoth target other restored
                released =
                    control
                        "RELEASE SAVEPOINT s;"
                        (writeMany (planAfterRollbackWrites plan) afterRollback)
             in readBoth target other released

        releaseSavepoint trace =
            let changed =
                    mutateBlock
                        target
                        other
                        (planReleaseWrites plan)
                        (planReleaseWritesFirst plan)
                        (control "SAVEPOINT r;" trace)
             in readBoth target other (control "RELEASE SAVEPOINT r;" changed)

prop_catalogTransitions :: Property
prop_catalogTransitions =
    forAll genCatalogPlan $ \plan ->
        let trace = catalogTrace plan
            sql = BSC.unlines (map BSC.pack (reverse (traceStatements trace)))
            expected = reverse (traceReads trace)
            actual graph =
                [ (sid, Set.difference (dependenciesFor sid graph) (traceControls trace))
                | (sid, _) <- expected
                ]
         in counterexample
                ("SQL trace:\n" ++ BSC.unpack sql ++ "\nexpected: " ++ show expected)
                ( monadicIO $ do
                    graph <- run (analyze SQLite sql)
                    monitor (counterexample ("actual data dependencies: " ++ show (actual graph)))
                    assert (actual graph == expected)
                )

analyze :: SQLDialect -> ByteString -> IO DependencyGraph
analyze dialect sql = do
    result <- parseSQL dialect sql
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right parsed -> pure (analyzeStatements (parsedUnits parsed))

actionsFor :: StatementId -> DependencyGraph -> [ObjectAction]
actionsFor sid graph = Map.findWithDefault [] sid (graphActions graph)

dependenciesFor :: StatementId -> DependencyGraph -> Set.Set StatementId
dependenciesFor sid graph = Map.findWithDefault Set.empty sid (graphEdges graph)

has :: StatementId -> Set.Set StatementId -> Bool
has = Set.member

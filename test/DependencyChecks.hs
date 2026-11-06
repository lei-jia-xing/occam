{-# LANGUAGE OverloadedStrings #-}

module DependencyChecks (tests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Occam.Core (Statement (..), StatementId (..))
import Occam.Dependency
import Occam.TreeSitter (extractStatementNodes, withParsedSQL, withParser)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

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
                reads = dependenciesFor (StatementId 3) graph
                wrongCaseReads = dependenciesFor (StatementId 4) graph
                unqualifiedReads = dependenciesFor (StatementId 7) graph
                qualifiedReads = dependenciesFor (StatementId 8) graph
            assertBool
                "quoted dots stay inside one identifier and quoted PostgreSQL case is preserved"
                (ObjectAction TableObject DefDep "sales.Order\\.Item" `elem` names)
            assertBool
                "qualified read depends on only its schema's table"
                (has (StatementId 1) reads && not (has (StatementId 2) reads))
            assertBool
                "quoted case mismatch does not bind to the stored object"
                (not (has (StatementId 1) wrongCaseReads))
            assertBool
                "unknown search_path conservatively includes both schemas"
                (has (StatementId 5) unqualifiedReads && has (StatementId 6) unqualifiedReads)
            assertBool
                "qualified read excludes the colliding schema"
                (has (StatementId 5) qualifiedReads && not (has (StatementId 6) qualifiedReads))
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
                        , "CREATE TEMP TABLE items (id INT);"
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
                        , "INSERT INTO destination SELECT id FROM source;"
                        ]
            graph <- analyze PostgreSQL sql
            let actions = actionsFor (StatementId 3) graph
                tableActions = [action | action <- actions, actionKind action == TableObject]
                writes = Set.fromList [actionName action | action <- tableActions, actionType action == ModDep]
                reads = Set.fromList [actionName action | action <- tableActions, actionType action == UseDep]
                deps = dependenciesFor (StatementId 3) graph
            assertEqual "only the insert target is modified" (Set.singleton "destination") writes
            assertEqual "the SELECT input is a source read" (Set.singleton "source") reads
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
                (any ((== "__tx_rollback_to__\x1es") . actionName) savepointActions)
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
                (ObjectAction BarrierObject BarrierDep "__tx_release__\x1es" `elem` releaseActions)
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
        ]

analyze :: SQLDialect -> ByteString -> IO DependencyGraph
analyze dialect sql = withParser $ \parser -> do
    parsed <- withParsedSQL parser sql $ \tree -> do
        statementNodes <- extractStatementNodes tree sql
        analyzeStatementsWithDialect
            dialect
            [(stmtId statement, node) | (statement, node) <- statementNodes]
            sql
    case parsed of
        Nothing -> assertFailure "SQL script did not parse"
        Just graph -> pure graph

actionsFor :: StatementId -> DependencyGraph -> [ObjectAction]
actionsFor sid graph = Map.findWithDefault [] sid (graphActions graph)

dependenciesFor :: StatementId -> DependencyGraph -> Set.Set StatementId
dependenciesFor sid graph = Map.findWithDefault Set.empty sid (graphEdges graph)

has :: StatementId -> Set.Set StatementId -> Bool
has = Set.member

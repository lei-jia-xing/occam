module DependencyChecks (tests) where

import Data.Bifunctor qualified as Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
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
    Name (..),
    NameRef (..),
    ReadFact (..),
    ReadMode (..),
    RenameOp (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    SessionInit (..),
    StatementId (..),
    TableDef (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEvent (..),
    TriggerRef (..),
    TriggerTiming (..),
    TxEffect (..),
    WriteKind (..),
    nameText,
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
import Test.QuickCheck (Gen, Property, arbitrary, chooseInt, counterexample, forAll, listOf, resize)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

-- ----------------------------------------------------------------------------
-- Fact helpers
-- ----------------------------------------------------------------------------

emptySemanticFacts :: SemanticFacts
emptySemanticFacts = SemanticFacts [] [] [] [] []

factsFor :: StatementId -> DependencyGraph -> SemanticFacts
factsFor sid graph = Map.findWithDefault emptySemanticFacts sid (graphFacts graph)

opsFor :: StatementId -> DependencyGraph -> [CatalogOp]
opsFor sid = factOps . factsFor sid

ref :: [Text] -> NameRef
ref parts = NameRef (Name (map Ident parts)) False

objectName :: [Text] -> Name
objectName = Name . map Ident

tableDefinition :: [Text] -> [CatalogOp] -> Maybe TableDef
tableDefinition parts ops =
    listToMaybe
        [ definition
        | DefineTableOp definition <- ops
        , tableName definition == objectName parts
        ]

definedColumns :: [Text] -> [CatalogOp] -> [Text]
definedColumns parts ops =
    maybe [] (map (identText . columnName) . tableColumns) (tableDefinition parts ops)

readsFor :: StatementId -> DependencyGraph -> [ReadFact]
readsFor sid graph = [readFact | ReadOp readFact <- opsFor sid graph]

writesFor :: StatementId -> DependencyGraph -> [DataWrite]
writesFor sid graph = [write | WriteDataOp write <- opsFor sid graph]

allReadFacts :: StatementId -> DependencyGraph -> [ReadFact]
allReadFacts sid graph =
    readsFor sid graph
        ++ concatMap dataWriteReads (writesFor sid graph)

hasStatementRead :: [Text] -> StatementId -> DependencyGraph -> Bool
hasStatementRead parts sid graph =
    any
        (\readFact -> readSource readFact == ref parts)
        (allReadFacts sid graph)

renamesFor :: StatementId -> DependencyGraph -> [RenameOp]
renamesFor sid graph = [rename | RenameOp rename <- opsFor sid graph]

hasRead :: [Text] -> [CatalogOp] -> Bool
hasRead parts ops =
    any
        (\readFact -> readSource readFact == ref parts)
        [readFact | ReadOp readFact <- ops]

hasWrite :: [Text] -> WriteKind -> [CatalogOp] -> Bool
hasWrite parts kind ops =
    any
        (\write -> dataWriteTarget write == ref parts && dataWriteKind write == kind)
        [write | WriteDataOp write <- ops]

readSources :: StatementId -> DependencyGraph -> Set.Set Text
readSources sid graph =
    Set.fromList
        [ nameText (nameRefName (readSource readFact))
        | readFact <- allReadFacts sid graph
        ]

isTruncateKind :: WriteKind -> Bool
isTruncateKind TruncateWrite {} = True
isTruncateKind _ = False

defaultSessionInit :: SessionInit
defaultSessionInit = SessionInit [] [] Nothing True True Map.empty

-- ----------------------------------------------------------------------------
-- Tests
-- ----------------------------------------------------------------------------

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
                [ (PostgreSQL, "CREATE EXTENSION hstore;", "hidden_view")
                , (PostgreSQL, "GRANT SELECT ON t TO PUBLIC;", "hidden_view")
                , (PostgreSQL, "CREATE TABLE hidden_view (b INT) INHERITS (t);", "hidden_view")
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
                                        (DataWrite (ref ["t"]) (InsertWrite ConflictError) [Ident "id"] [] [])
                                    ]
                                }
                            )
                        ,
                            ( StatementId 5
                            , emptySemanticFacts
                                { factOps = [ReadOp (ReadFact (ref ["t"]) ReadData [Ident "id"])]
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
        , testCase "DROP COLUMN CASCADE drops views that read the column" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b INT);"
                        , "CREATE VIEW v AS SELECT a FROM t;"
                        , "ALTER TABLE t DROP COLUMN a CASCADE;"
                        , "SELECT * FROM v;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the dependent view is dropped with the column"
                (not (has (StatementId 2) (dependenciesFor (StatementId 4) graph)))
        , testCase "DROP COLUMN drops indexes on that column but a rejected RESTRICT keeps them" $ do
            let cascadeSql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, v INT);"
                        , "CREATE INDEX t_id_idx ON t (id);"
                        , "ALTER TABLE t DROP COLUMN id CASCADE;"
                        , "SELECT * FROM t;"
                        ]
                renamedSql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, v INT);"
                        , "CREATE INDEX t_id_idx ON t (id);"
                        , "ALTER TABLE t RENAME COLUMN id TO id2;"
                        , "ALTER TABLE t DROP COLUMN id2 CASCADE;"
                        , "SELECT * FROM t;"
                        ]
                restrictSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT, v INT);"
                        , "CREATE INDEX parent_id_idx ON parent (id);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE);"
                        , "ALTER TABLE parent DROP COLUMN id;"
                        , "SELECT * FROM parent;"
                        ]
            cascadeGraph <- analyze PostgreSQL cascadeSql
            renamedGraph <- analyze PostgreSQL renamedSql
            restrictGraph <- analyze PostgreSQL restrictSql
            let cascadeDeps = dependenciesFor (StatementId 4) cascadeGraph
                renamedDeps = dependenciesFor (StatementId 5) renamedGraph
                restrictDeps = dependenciesFor (StatementId 5) restrictGraph
            assertBool
                "the index on the dropped column is gone"
                (not (has (StatementId 2) cascadeDeps))
            assertBool
                "an index follows a column rename before the drop"
                (not (has (StatementId 2) renamedDeps))
            assertBool
                "a rejected RESTRICT leaves the index and table intact"
                (has (StatementId 2) restrictDeps)
        , testCase "USE database constrains unqualified names to the current database" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE db1.t (x INT);"
                        , "CREATE TABLE db2.t (x INT);"
                        , "USE db1;"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze MySQL sql
            let readDeps = dependenciesFor (StatementId 4) graph
            assertBool
                "unqualified read resolves only in the current database"
                (has (StatementId 1) readDeps && not (has (StatementId 2) readDeps))
        , testCase "a view source created later is resolved when the view is read" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE VIEW v AS SELECT * FROM later;"
                        , "CREATE TABLE later (x INT);"
                        , "SELECT * FROM v;"
                        ]
            graph <- analyze PostgreSQL sql
            let viewReadDeps = dependenciesFor (StatementId 3) graph
            assertBool
                "view read depends on the view definition"
                (has (StatementId 1) viewReadDeps)
            assertBool
                "view read follows the unresolved source to its later definition"
                (has (StatementId 2) viewReadDeps)
        , testCase "MySQL DROP SCHEMA cascades through its contents" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE db.t (x INT);"
                        , "DROP SCHEMA db;"
                        , "SELECT * FROM db.t;"
                        ]
            graph <- analyze MySQL sql
            assertBool
                "the table is gone with the schema"
                (not (has (StatementId 1) (dependenciesFor (StatementId 3) graph)))
        , testCase "DROP CONSTRAINT CASCADE removes foreign keys that depend on it" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT, CONSTRAINT parent_pk PRIMARY KEY (id));"
                        , "CREATE TABLE child (pid INT);"
                        , "ALTER TABLE child ADD CONSTRAINT child_fk FOREIGN KEY (pid) REFERENCES parent(id);"
                        , "INSERT INTO parent VALUES (1);"
                        , "ALTER TABLE parent DROP CONSTRAINT parent_pk CASCADE;"
                        , "INSERT INTO child VALUES (1);"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the cascaded drop also removes the dependent foreign-key parent check"
                (not (has (StatementId 4) (dependenciesFor (StatementId 6) graph)))
        , testCase "SQLite foreign keys default to disabled" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE);"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
                enabledSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE);"
                        , "PRAGMA foreign_keys = 1;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
            defaultGraph <- analyze SQLite sql
            enabledGraph <- analyze SQLite enabledSql
            let defaultDeps = dependenciesFor (StatementId 4) defaultGraph
                enabledDeps' = dependenciesFor (StatementId 5) enabledGraph
            assertBool
                "the SQLite default does not cascade the parent delete"
                (not (has (StatementId 3) defaultDeps))
            assertBool
                "an explicit PRAGMA foreign_keys = 1 restores cascading"
                (has (StatementId 4) enabledDeps')
        , testCase "TABLE query bodies retain external sources without capturing CTE shadows" $
            mapM_
                ( \(query, expectedWrites) -> do
                    graph <-
                        analyzeParsed PostgreSQL $
                            BSC.unlines
                                [ "CREATE TABLE t (a INT);"
                                , "CREATE TABLE u (a INT);"
                                , "INSERT INTO t VALUES (1);"
                                , "INSERT INTO u VALUES (2);"
                                , query
                                ]
                    let deps = backwardSlice (StatementId 5) graph
                    assertBool
                        ("TABLE query reaches exactly its source writes: " ++ BSC.unpack query)
                        (all (\sid -> has (StatementId sid) deps == (sid `elem` expectedWrites)) [3, 4])
                )
                [ ("(TABLE t);", [3])
                , ("SELECT * FROM (TABLE t) AS q;", [3])
                , ("(TABLE t) UNION ALL TABLE u;", [3, 4])
                , ("WITH u AS (TABLE t) TABLE u;", [3])
                , ("CREATE TABLE copied AS TABLE t;", [3])
                ]
        , testCase "whole-row projections retain nonpredicate columns without broadening other sources" $
            mapM_
                ( \(query, readsWholeRow) -> do
                    graph <-
                        analyzeParsed PostgreSQL $
                            BSC.unlines
                                [ "CREATE TEMP TABLE t (a INT, b INT);"
                                , "CREATE TABLE u (a INT, b INT);"
                                , "INSERT INTO t VALUES (1, 0);"
                                , "INSERT INTO u VALUES (1, 0);"
                                , "UPDATE t SET b = 1;"
                                , "UPDATE u SET b = 2;"
                                , query
                                ]
                    let deps = backwardSlice (StatementId 7) graph
                    assertEqual
                        ("whole-row projection retains updates outside its predicate: " ++ BSC.unpack query)
                        readsWholeRow
                        (has (StatementId 5) deps)
                    assertBool
                        "an unrelated or join-key-only source does not contribute its other-column update"
                        (not (has (StatementId 6) deps))
                )
                [ ("(TABLE t) ORDER BY a;", True)
                , ("SELECT * FROM t WHERE a = 1;", True)
                , ("SELECT t.* FROM t JOIN u ON t.a = u.a WHERE t.a = 1;", True)
                , ("SELECT t.a FROM t JOIN u ON t.a = u.a WHERE t.a = 1;", False)
                , ("SELECT q.* FROM pg_temp.t AS q WHERE a = 1;", True)
                ]
        , testCase "SELECT INTO creates a data snapshot only in table-target dialects" $
            mapM_
                ( \dialect -> do
                    graph <-
                        analyzeParsed dialect $
                            BSC.unlines
                                [ "CREATE TABLE src (a INT);"
                                , "CREATE TABLE unrelated (a INT);"
                                , "INSERT INTO src VALUES (1);"
                                , "SELECT a INTO copied FROM src;"
                                , "INSERT INTO src VALUES (2);"
                                , "INSERT INTO unrelated VALUES (3);"
                                , "SELECT a FROM copied;"
                                ]
                    let deps = backwardSlice (StatementId 7) graph
                    assertBool
                        "copied rows retain their source definition and initial data"
                        (all (\sid -> has (StatementId sid) deps) [1, 3, 4])
                    assertBool
                        "copied rows do not become a live view or a global fence"
                        (all (\sid -> not (has (StatementId sid) deps)) [2, 5, 6])
                )
                [PostgreSQL, Redshift, MsSql]
        , testCase "CTAS NO DATA binds schema while data copies remain isolated snapshots" $
            mapM_
                ( \(suffix, keepsInitialData) -> do
                    graph <-
                        analyzeParsed PostgreSQL $
                            BSC.unlines
                                [ "CREATE TABLE src (a INT);"
                                , "INSERT INTO src VALUES (1);"
                                , "CREATE TABLE copied AS SELECT a FROM src" <> suffix <> ";"
                                , "INSERT INTO src VALUES (2);"
                                , "SELECT a FROM copied;"
                                , "INSERT INTO copied VALUES (3);"
                                , "SELECT a FROM copied;"
                                ]
                    let deps = backwardSlice (StatementId 5) graph
                    assertBool
                        "copy retains its own creation and source schema"
                        (has (StatementId 1) deps && has (StatementId 3) deps)
                    assertEqual
                        "only populated creation consumes initial source rows"
                        keepsInitialData
                        (has (StatementId 2) deps)
                    assertBool
                        "later source rows do not enter the copy"
                        (not (has (StatementId 4) deps))
                    let modifiedDeps = backwardSlice (StatementId 7) graph
                    assertBool
                        "writing the snapshot retains its own rows without reopening live source data"
                        (has (StatementId 6) modifiedDeps && not (has (StatementId 4) modifiedDeps))
                )
                [("", True), (" WITH NO DATA", False)]
        , testCase "materialized refresh captures source rows at refresh time only" $ do
            graph <-
                analyzeParsed PostgreSQL $
                    BSC.unlines
                        [ "CREATE TABLE src (a INT);"
                        , "CREATE MATERIALIZED VIEW mv AS SELECT a FROM src;"
                        , "INSERT INTO src VALUES (1);"
                        , "REFRESH MATERIALIZED VIEW mv WITH DATA;"
                        , "INSERT INTO src VALUES (2);"
                        , "SELECT a FROM mv;"
                        , "REFRESH MATERIALIZED VIEW mv WITH NO DATA;"
                        , "SELECT a FROM mv;"
                        ]
            let populatedDeps = backwardSlice (StatementId 6) graph
                emptyDeps = backwardSlice (StatementId 8) graph
            assertBool
                "populated refresh retains the refresh-time source write"
                (has (StatementId 3) populatedDeps && has (StatementId 4) populatedDeps)
            assertBool
                "materialized rows do not follow later source writes"
                (not (has (StatementId 5) populatedDeps))
            assertBool
                "NO DATA refresh retains creation and its schema transition"
                (all (\sid -> has (StatementId sid) emptyDeps) [1, 2, 7])
            assertBool
                "NO DATA refresh discards previous refresh data dependencies"
                (all (\sid -> not (has (StatementId sid) emptyDeps)) [3, 4, 5])
        , testCase "ambiguous destructive targets preserve possible state across later writes" $ do
            graph <-
                analyzeParsed PostgreSQL $
                    BSC.unlines
                        [ "CREATE SCHEMA a;"
                        , "CREATE SCHEMA b;"
                        , "CREATE TABLE a.t (x INT);"
                        , "CREATE TABLE b.t (x INT);"
                        , "INSERT INTO a.t VALUES (1);"
                        , "INSERT INTO b.t VALUES (2);"
                        , "SET search_path TO a, b;"
                        , "DROP TABLE t;"
                        , "INSERT INTO b.t VALUES (3);"
                        , "SELECT x FROM b.t;"
                        ]
            let deps = backwardSlice (StatementId 10) graph
            assertBool
                "an unresolved destructive choice cannot erase either candidate or later writes"
                (all (\sid -> has (StatementId sid) deps) [3, 4, 5, 6, 8, 9])
        , testCase "unmodeled dialect DML effects fence hidden target consumers" $
            mapM_
                ( \(dialect, operation) -> do
                    graph <-
                        analyzeParsed dialect $
                            BSC.unlines
                                [ "CREATE TABLE src (a INT);"
                                , "CREATE TABLE sink (a INT);"
                                , "INSERT INTO src VALUES (1);"
                                , operation
                                , "SELECT a FROM sink;"
                                ]
                    let deps = backwardSlice (StatementId 5) graph
                    assertBool
                        "hidden effects retain operation and its preceding state"
                        (all (\sid -> has (StatementId sid) deps) [1, 2, 3, 4])
                )
                [ (Hive, "INSERT OVERWRITE TABLE src SELECT a FROM sink;")
                , (MySQL, "INSERT INTO src PARTITION (p0) SELECT a FROM sink;")
                , (Hive, "INSERT INTO src PARTITION (a) SELECT a FROM sink;")
                , (Snowflake, "INSERT ALL INTO src VALUES (1) INTO sink VALUES (2) SELECT 1;")
                , (MsSql, "UPDATE src SET a = 2 OUTPUT inserted.a INTO sink;")
                ]
        , testCase "plain view definitions bind schema while later reads follow data" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT);"
                        , "INSERT INTO t VALUES (1);"
                        , "CREATE VIEW v AS SELECT a FROM t;"
                        , "SELECT a FROM v;"
                        , "CREATE MATERIALIZED VIEW mv AS SELECT a FROM t;"
                        , "CREATE TABLE copy AS SELECT a FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            let viewDefinitionDeps = dependenciesFor (StatementId 3) graph
                viewReadDeps = dependenciesFor (StatementId 4) graph
                materializedDeps = dependenciesFor (StatementId 5) graph
                tableCopyDeps = dependenciesFor (StatementId 6) graph
            assertBool
                "the view definition depends on the table definition"
                (has (StatementId 1) viewDefinitionDeps)
            assertBool
                "the view definition does not depend on earlier row data"
                (not (has (StatementId 2) viewDefinitionDeps))
            assertBool
                "reading the view depends on the view definition"
                (has (StatementId 3) viewReadDeps)
            assertBool
                "reading the view follows its source data"
                (has (StatementId 2) viewReadDeps)
            assertBool
                "a materialized view definition reads source data"
                (has (StatementId 2) materializedDeps)
            assertBool
                "CREATE TABLE AS reads source data"
                (has (StatementId 2) tableCopyDeps)
            assertEqual
                "CREATE TABLE AS derives columns from the projection"
                ["a"]
                (definedColumns ["copy"] (opsFor (StatementId 6) graph))
        , testCase "sequence reads depend on the sequence definition" $ do
            let postgresSql =
                    BSC.unlines
                        [ "CREATE SEQUENCE s;"
                        , "SELECT nextval('s');"
                        ]
                oracleSql =
                    BSC.unlines
                        [ "CREATE SEQUENCE s;"
                        , "SELECT s.NEXTVAL FROM DUAL;"
                        ]
            postgresGraph <- analyze PostgreSQL postgresSql
            oracleGraph <- analyze Oracle oracleSql
            assertBool
                "nextval depends on the sequence definition"
                (has (StatementId 1) (dependenciesFor (StatementId 2) postgresGraph))
            assertBool
                "Oracle NEXTVAL depends on the sequence definition"
                (has (StatementId 1) (dependenciesFor (StatementId 2) oracleGraph))
        , testCase "constraint-coupled columns share write dependencies" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b INT, c INT, PRIMARY KEY (a, b));"
                        , "INSERT INTO t VALUES (1, 2, 3);"
                        , "UPDATE t SET a = 4;"
                        , "SELECT b FROM t;"
                        , "SELECT c FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "a read of another primary-key column depends on the update"
                (has (StatementId 3) (dependenciesFor (StatementId 4) graph))
            assertBool
                "a read of an unrelated column does not depend on the update"
                (not (has (StatementId 3) (dependenciesFor (StatementId 5) graph)))
        , testProperty
            "generated SQLite catalog transitions match a table snapshot model"
            prop_catalogTransitions
        ]

-- ----------------------------------------------------------------------------
-- Snapshot-model property
-- ----------------------------------------------------------------------------

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

type TableSnapshot = Map.Map Int (StatementId, Set.Set StatementId)

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

snapshotTableName :: Int -> String
snapshotTableName 0 = "a"
snapshotTableName _ = "b"

createTable :: Int -> CatalogTrace -> CatalogTrace
createTable table trace =
    let (sid, trace') = emit ("CREATE TABLE " ++ snapshotTableName table ++ " (id INTEGER);") trace
     in trace' {traceTables = Map.insert table (sid, Set.empty) (traceTables trace')}

writeTable :: Int -> CatalogTrace -> CatalogTrace
writeTable table trace =
    let (sid, trace') = emit ("INSERT INTO " ++ snapshotTableName table ++ " VALUES (1);") trace
     in trace'
            { traceTables =
                Map.adjust
                    (Bifunctor.second (Set.insert sid))
                    table
                    (traceTables trace')
            }

dropTable :: Int -> CatalogTrace -> CatalogTrace
dropTable table trace =
    let (_, trace') = emit ("DROP TABLE " ++ snapshotTableName table ++ ";") trace
     in trace' {traceTables = Map.delete table (traceTables trace')}

readTable :: Int -> CatalogTrace -> CatalogTrace
readTable table trace =
    let (sid, trace') = emit ("SELECT * FROM " ++ snapshotTableName table ++ ";") trace
     in trace'
            { traceReads =
                (sid, tableDependencies table (traceTables trace')) : traceReads trace'
            }

tableDependencies :: Int -> TableSnapshot -> Set.Set StatementId
tableDependencies table tables =
    case Map.lookup table tables of
        Nothing -> Set.empty
        Just (definition, writes) -> Set.insert definition writes

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

analyzeParsed :: SQLDialect -> ByteString -> IO DependencyGraph
analyzeParsed dialect sql = do
    result <- parseSQL (defaultParseOptions dialect) sql
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right parsed -> do
            assertBool
                ("expected supported " ++ show dialect ++ " syntax:\n" ++ BSC.unpack sql)
                (all (isJust . unitFacts) (parsedUnits parsed))
            pure (analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed))

analyze :: SQLDialect -> ByteString -> IO DependencyGraph
analyze dialect = analyzeWith (defaultParseOptions dialect)

analyzeWith :: ParseOptions -> ByteString -> IO DependencyGraph
analyzeWith options sql = do
    result <- parseSQL options sql
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right parsed ->
            pure (analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed))

dependenciesFor :: StatementId -> DependencyGraph -> Set.Set StatementId
dependenciesFor sid graph = Map.findWithDefault Set.empty sid (graphEdges graph)

has :: StatementId -> Set.Set StatementId -> Bool
has = Set.member

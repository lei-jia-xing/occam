module DependencyChecks (tests) where

import Data.Bifunctor qualified as Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core (
    BarrierScope (..),
    BeginPolicy (..),
    CatalogOp (..),
    ColumnDef (..),
    ConflictAction (..),
    Constraint (..),
    DataWrite (..),
    DeleteScope (..),
    Ident (..),
    IndexDef (..),
    IndexRef (..),
    IndexSchemaPlacement (..),
    Name (..),
    NameRef (..),
    OracleOutcome (..),
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
    UpdateScope (..),
    WriteKind (..),
    nameText,
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements, buildGraph, cascadeWrites)
import Occam.Oracle.DuckDB (
    DuckDBError (..),
    duckdbContractOracleTimeout,
    executeDuckDBCandidateDetailedTimeout,
    generateDuckDBContractTimeout,
 )
import Occam.Oracle.Sqlite (
    SqliteError (..),
    executeSqliteCandidateDetailedTimeout,
    generateSqliteContractTimeout,
    sqliteContractOracleTimeout,
 )
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    ServerConfig (..),
    StatementUnit (..),
    defaultParseOptions,
    parseSQL,
 )
import Occam.Reducer (cutRemovedStatementIds)
import Occam.Slicer (backwardSlice)
import Test.QuickCheck (Gen, Property, arbitrary, chooseInt, counterexample, forAll, listOf, resize)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport (emptySemanticFacts)

-- ----------------------------------------------------------------------------
-- Fact helpers
-- ----------------------------------------------------------------------------

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

isNamedIndex :: Text -> Constraint -> Bool
isNamedIndex wanted (IndexConstraint (Just (Ident name)) _ _) = wanted == name
isNamedIndex _ _ = False

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
isAttach :: CatalogOp -> Bool
isAttach (AttachPartitionOp _) = True
isAttach _ = False

isDetach :: CatalogOp -> Bool
isDetach (DetachPartitionOp _) = True
isDetach _ = False

isAttachInherit :: CatalogOp -> Bool
isAttachInherit (AttachInheritOp _) = True
isAttachInherit _ = False

isDetachInherit :: CatalogOp -> Bool
isDetachInherit (DetachInheritOp _) = True
isDetachInherit _ = False

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
        [ nativeSliceChecks
        , testCase "schema collisions and quoted PostgreSQL identities stay distinct" $ do
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
        , testCase
            "transitive view column lineage distinguishes referenced from unreferenced base table columns on UPDATE"
            $ do
                let sql =
                        BSC.unlines
                            [ "CREATE TABLE t (a INT, b INT);"
                            , "CREATE VIEW v1 AS SELECT a, b FROM t;"
                            , "CREATE VIEW v2 AS SELECT a FROM v1;"
                            , "UPDATE t SET b = 1;"
                            , "UPDATE t SET a = 1;"
                            , "SELECT a FROM v2;"
                            ]
                graph <- analyze PostgreSQL sql
                let v2QueryDeps = dependenciesFor (StatementId 6) graph
                assertBool
                    "query on v2.a does not depend on UPDATE to unused column b"
                    (not (has (StatementId 4) v2QueryDeps))
                assertBool "query on v2.a depends on UPDATE to used column a" (has (StatementId 5) v2QueryDeps)
        , testCase
            "transitive view column lineage prevents false dependency on DROP COLUMN of unreferenced column"
            $ do
                let sql =
                        BSC.unlines
                            [ "CREATE TABLE t (a INT, b INT, c INT);"
                            , "CREATE VIEW v1 AS SELECT a, b FROM t;"
                            , "CREATE VIEW v2 AS SELECT a FROM v1;"
                            , "ALTER TABLE t DROP COLUMN c CASCADE;"
                            , "SELECT * FROM v2;"
                            ]
                graph <- analyze PostgreSQL sql
                let alterDeps = dependenciesFor (StatementId 4) graph
                    v2QueryDeps = dependenciesFor (StatementId 5) graph
                assertBool "DROP COLUMN c does not depend on v1 definition" (not (has (StatementId 2) alterDeps))
                assertBool "DROP COLUMN c does not depend on v2 definition" (not (has (StatementId 3) alterDeps))
                assertBool "unaffected view v2 is not dropped by CASCADE" (has (StatementId 3) v2QueryDeps)
        , testCase "transitive column aliasing across views correctly tracks source column" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b INT);"
                        , "CREATE VIEW v1 AS SELECT a AS x, b AS y FROM t;"
                        , "CREATE VIEW v2 AS SELECT x AS z FROM v1;"
                        , "UPDATE t SET b = 10;"
                        , "UPDATE t SET a = 20;"
                        , "SELECT z FROM v2;"
                        ]
            graph <- analyze PostgreSQL sql
            let v2QueryDeps = dependenciesFor (StatementId 6) graph
            assertBool
                "query on aliased column z does not depend on UPDATE to unaliased unused column b"
                (not (has (StatementId 4) v2QueryDeps))
            assertBool
                "query on aliased column z depends on UPDATE to underlying column a"
                (has (StatementId 5) v2QueryDeps)
        , testCase "view condition column in WHERE clause creates dependency on filtered column" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b INT, c INT);"
                        , "CREATE VIEW v1 AS SELECT a FROM t WHERE b > 0;"
                        , "CREATE VIEW v2 AS SELECT a FROM v1;"
                        , "UPDATE t SET c = 10;"
                        , "UPDATE t SET b = 20;"
                        , "SELECT a FROM v2;"
                        ]
            graph <- analyze PostgreSQL sql
            let v2QueryDeps = dependenciesFor (StatementId 6) graph
            assertBool
                "query on v2 does not depend on unrelated column c"
                (not (has (StatementId 4) v2QueryDeps))
            assertBool
                "query on v2 depends on condition column b in intermediate view"
                (has (StatementId 5) v2QueryDeps)
        , testCase "view with unexpanded SELECT * falls back to conservative dependencies" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b INT);"
                        , "CREATE VIEW v1 AS SELECT * FROM t;"
                        , "CREATE VIEW v2 AS SELECT a FROM v1;"
                        , "UPDATE t SET b = 10;"
                        , "SELECT a FROM v2;"
                        ]
            graph <- analyze PostgreSQL sql
            let v2QueryDeps = dependenciesFor (StatementId 5) graph
            assertBool
                "query through SELECT * view conservatively depends on column b update"
                (has (StatementId 4) v2QueryDeps)
        , testCase "multi-table join in view isolates column updates by table source" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t1 (id INT, a INT, b INT);"
                        , "CREATE TABLE t2 (id INT, c INT, d INT);"
                        , "CREATE VIEW v1 AS SELECT t1.a AS col_a, t2.c AS col_c FROM t1 JOIN t2 ON t1.id = t2.id;"
                        , "CREATE VIEW v2 AS SELECT col_a FROM v1;"
                        , "UPDATE t2 SET d = 1;"
                        , "UPDATE t1 SET b = 1;"
                        , "UPDATE t2 SET id = 1;"
                        , "UPDATE t1 SET a = 1;"
                        , "SELECT col_a FROM v2;"
                        ]
            graph <- analyze PostgreSQL sql
            let v2QueryDeps = dependenciesFor (StatementId 9) graph
            assertBool "v2 does not depend on t2.d update" (not (has (StatementId 5) v2QueryDeps))
            assertBool "v2 does not depend on t1.b update" (not (has (StatementId 6) v2QueryDeps))
            assertBool "v2 depends on join condition column t2.id update" (has (StatementId 7) v2QueryDeps)
            assertBool "v2 depends on projected column t1.a update" (has (StatementId 8) v2QueryDeps)
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
        , testCase "MySQL prefix index does not trip unknown statement barrier" $ do
            let sql = "CREATE TEMPORARY TABLE t1(i INT, b BLOB, PRIMARY KEY pk(i), INDEX sk(b(1)));"
            graph <- analyze MySQL sql
            let barriers = factBarriers (factsFor (StatementId 1) graph)
            assertEqual "MySQL index prefix has no barriers" [] barriers
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
        , testCase "SQLite FK cascade chain stays quiet when enforcement is off (03 shapes)" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE dept (did INTEGER PRIMARY KEY, dname TEXT NOT NULL);"
                        , "CREATE TABLE emp (eid INTEGER PRIMARY KEY, did INTEGER NOT NULL REFERENCES dept(did) ON DELETE CASCADE ON UPDATE CASCADE, ename TEXT NOT NULL);"
                        , "CREATE TABLE task (tid INTEGER PRIMARY KEY, eid INTEGER NOT NULL REFERENCES emp(eid) ON DELETE CASCADE, title TEXT NOT NULL);"
                        , "INSERT INTO dept VALUES (1, 'eng'), (2, 'ops');"
                        , "INSERT INTO emp VALUES (10, 1, 'amy'), (20, 2, 'bob');"
                        , "INSERT INTO task VALUES (100, 10, 'build'), (200, 20, 'deploy');"
                        , "UPDATE dept SET did = 7 WHERE did = 2;"
                        , "SELECT * FROM emp;"
                        , "DELETE FROM dept WHERE did = 1;"
                        , "SELECT * FROM emp;"
                        , "SELECT * FROM task;"
                        ]
            graph <- analyze SQLite sql
            let afterUpdate = dependenciesFor (StatementId 8) graph
                afterDeleteEmp = dependenciesFor (StatementId 10) graph
                afterDeleteTask = dependenciesFor (StatementId 11) graph
            assertBool
                "ordinary child reads still resolve without enforcement"
                (has (StatementId 5) afterUpdate)
            assertBool
                "the SQLite default does not cascade the parent update"
                (not (has (StatementId 7) afterUpdate))
            assertBool
                "the SQLite default does not cascade the parent delete to the child"
                (not (has (StatementId 9) afterDeleteEmp))
            assertBool
                "the SQLite default does not cascade the parent delete transitively"
                (not (has (StatementId 9) afterDeleteTask))
        , testCase "SQLite FK cascade chain fires when enforcement is on (03 shapes)" $ do
            let schema =
                    [ "CREATE TABLE dept (did INTEGER PRIMARY KEY, dname TEXT NOT NULL);"
                    , "CREATE TABLE emp (eid INTEGER PRIMARY KEY, did INTEGER NOT NULL REFERENCES dept(did) ON DELETE CASCADE ON UPDATE CASCADE, ename TEXT NOT NULL);"
                    , "CREATE TABLE task (tid INTEGER PRIMARY KEY, eid INTEGER NOT NULL REFERENCES emp(eid) ON DELETE CASCADE, title TEXT NOT NULL);"
                    ]
                body =
                    [ "INSERT INTO dept VALUES (1, 'eng'), (2, 'ops');"
                    , "INSERT INTO emp VALUES (10, 1, 'amy'), (20, 2, 'bob');"
                    , "INSERT INTO task VALUES (100, 10, 'build'), (200, 20, 'deploy');"
                    , "UPDATE dept SET did = 7 WHERE did = 2;"
                    , "SELECT * FROM emp;"
                    , "SELECT * FROM task;"
                    , "DELETE FROM dept WHERE did = 1;"
                    , "SELECT * FROM emp;"
                    , "SELECT * FROM task;"
                    ]
                pragmaSql = BSC.unlines (schema ++ ["PRAGMA foreign_keys = 1;"] ++ body)
                plainSql = BSC.unlines (schema ++ body)
                enforcedOptions =
                    (defaultParseOptions SQLite)
                        { parseServerConfig =
                            (parseServerConfig (defaultParseOptions SQLite))
                                { serverForeignKeyChecks = Just True
                                }
                        }
            pragmaGraph <- analyze SQLite pragmaSql
            enforcedGraph <- analyzeWith enforcedOptions plainSql
            -- With the PRAGMA at statement 4, UPDATE is 8 and DELETE is 11;
            -- without it (server-enforced initial state), UPDATE is 7 and DELETE is 10.
            let check graph update delete empAfterUpdate taskAfterUpdate empAfterDelete taskAfterDelete = do
                    assertBool
                        "the parent update cascades to the direct child"
                        (has update (dependenciesFor empAfterUpdate graph))
                    assertBool
                        "the update cascade stops where the child key is untouched"
                        (not (has update (dependenciesFor taskAfterUpdate graph)))
                    assertBool
                        "the parent delete cascades to the direct child"
                        (has delete (dependenciesFor empAfterDelete graph))
                    assertBool
                        "the parent delete cascades transitively down the chain"
                        (has delete (dependenciesFor taskAfterDelete graph))
            check
                pragmaGraph
                (StatementId 8)
                (StatementId 11)
                (StatementId 9)
                (StatementId 10)
                (StatementId 12)
                (StatementId 13)
            check
                enforcedGraph
                (StatementId 7)
                (StatementId 10)
                (StatementId 8)
                (StatementId 9)
                (StatementId 11)
                (StatementId 12)
        , testCase "--dump-facts cascade expansion follows modeled enforcement" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE dept (did INTEGER PRIMARY KEY, dname TEXT NOT NULL);"
                        , "CREATE TABLE emp (eid INTEGER PRIMARY KEY, did INTEGER NOT NULL REFERENCES dept(did) ON DELETE CASCADE ON UPDATE CASCADE, ename TEXT NOT NULL);"
                        , "CREATE TABLE task (tid INTEGER PRIMARY KEY, eid INTEGER NOT NULL REFERENCES emp(eid) ON DELETE CASCADE, title TEXT NOT NULL);"
                        , "INSERT INTO dept VALUES (1, 'eng'), (2, 'ops');"
                        , "INSERT INTO emp VALUES (10, 1, 'amy'), (20, 2, 'bob');"
                        , "INSERT INTO task VALUES (100, 10, 'build'), (200, 20, 'deploy');"
                        , "UPDATE dept SET did = 7 WHERE did = 2;"
                        , "SELECT * FROM emp;"
                        , "SELECT * FROM task;"
                        , "DELETE FROM dept WHERE did = 1;"
                        , "SELECT * FROM emp;"
                        , "SELECT * FROM task;"
                        ]
                enforcedOptions =
                    (defaultParseOptions SQLite)
                        { parseServerConfig =
                            (parseServerConfig (defaultParseOptions SQLite))
                                { serverForeignKeyChecks = Just True
                                }
                        }
                parse options = do
                    result <- parseSQL options sql
                    case result of
                        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                        Right parsed -> pure parsed
            defaultParsed <- parse (defaultParseOptions SQLite)
            enforcedParsed <- parse enforcedOptions
            let defaultExtras = cascadeWrites (parsedInitialSession defaultParsed) (parsedUnits defaultParsed)
                enforcedExtras = cascadeWrites (parsedInitialSession enforcedParsed) (parsedUnits enforcedParsed)
                targets sid =
                    [ nameText (nameRefName (dataWriteTarget write))
                    | write <- Map.findWithDefault [] sid enforcedExtras
                    ]
            assertBool
                "the SQLite default expands no cascade writes"
                (Map.null defaultExtras)
            assertEqual
                "only the parent UPDATE and DELETE expand"
                [StatementId 7, StatementId 10]
                (Map.keys enforcedExtras)
            assertEqual
                "the parent update cascades to the direct child"
                ["emp"]
                (targets (StatementId 7))
            case Map.findWithDefault [] (StatementId 7) enforcedExtras of
                [write] -> do
                    assertEqual
                        "the cascaded update keeps its kind"
                        (UpdateWrite UpdateFilteredRows)
                        (dataWriteKind write)
                    assertEqual
                        "the cascaded update writes the child key"
                        ["did"]
                        (map identText (dataWriteColumns write))
                writes -> assertFailure ("expected one cascaded update, got " ++ show (length writes))
            assertEqual
                "the parent delete cascades down the chain"
                ["emp", "task"]
                (targets (StatementId 10))
            assertBool
                "cascaded deletes are limited to matching child rows"
                ( all
                    (\write -> dataWriteKind write == DeleteWrite DeleteFilteredRows)
                    (Map.findWithDefault [] (StatementId 10) enforcedExtras)
                )
        , testCase "schema-qualified tables with the same base name cascade independently" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA a;"
                        , "CREATE SCHEMA b;"
                        , "CREATE TABLE a.p (id INT PRIMARY KEY);"
                        , "CREATE TABLE b.p (id INT PRIMARY KEY);"
                        , "CREATE TABLE a.c (pid INT REFERENCES a.p(id) ON UPDATE CASCADE);"
                        , "CREATE TABLE b.c (pid INT REFERENCES b.p(id) ON UPDATE CASCADE);"
                        , "UPDATE a.p SET id = 7 WHERE id = 1;"
                        , "UPDATE b.p SET id = 8 WHERE id = 1;"
                        ]
                options =
                    (defaultParseOptions PostgreSQL)
                        { parseServerConfig =
                            (parseServerConfig (defaultParseOptions PostgreSQL))
                                { serverForeignKeyChecks = Just True
                                }
                        }
            parseRes <- parseSQL options sql
            parsed <- case parseRes of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right value -> pure value
            let extras = cascadeWrites (parsedInitialSession parsed) (parsedUnits parsed)
                targets sid =
                    [ nameText (nameRefName (dataWriteTarget write))
                    | write <- Map.findWithDefault [] sid extras
                    ]
            assertEqual "a.p cascades only to a.c" ["a.c"] (targets (StatementId 7))
            assertEqual "b.p cascades only to b.c" ["b.c"] (targets (StatementId 8))
        , testCase "DROP SCHEMA CASCADE removes the dropped schema's cascade edges" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA s;"
                        , "CREATE TABLE s.parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE s.child (pid INT REFERENCES s.parent(id) ON UPDATE CASCADE);"
                        , "DROP SCHEMA s CASCADE;"
                        , "CREATE TABLE s.parent (id INT PRIMARY KEY);"
                        , "UPDATE s.parent SET id = 7 WHERE id = 1;"
                        ]
                options =
                    (defaultParseOptions PostgreSQL)
                        { parseServerConfig =
                            (parseServerConfig (defaultParseOptions PostgreSQL))
                                { serverForeignKeyChecks = Just True
                                }
                        }
            parseRes <- parseSQL options sql
            parsed <- case parseRes of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right value -> pure value
            let extras = cascadeWrites (parsedInitialSession parsed) (parsedUnits parsed)
                targets =
                    [ nameText (nameRefName (dataWriteTarget write))
                    | write <- Map.findWithDefault [] (StatementId 6) extras
                    ]
            assertEqual
                "the recreated table does not cascade into tables from the dropped schema"
                []
                targets
        , testCase "cascade visits the same child table for distinct update column sets" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p (id INT PRIMARY KEY, alt INT UNIQUE);"
                        , "CREATE TABLE c (a INT REFERENCES p(id) ON UPDATE CASCADE, b INT REFERENCES p(alt) ON UPDATE CASCADE);"
                        , "CREATE TABLE g (x INT REFERENCES c(b) ON UPDATE CASCADE);"
                        , "UPDATE p SET id = 7, alt = 8 WHERE id = 1;"
                        ]
                options =
                    (defaultParseOptions PostgreSQL)
                        { parseServerConfig =
                            (parseServerConfig (defaultParseOptions PostgreSQL))
                                { serverForeignKeyChecks = Just True
                                }
                        }
            parseRes <- parseSQL options sql
            parsed <- case parseRes of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right value -> pure value
            let extras = cascadeWrites (parsedInitialSession parsed) (parsedUnits parsed)
                targets =
                    [ nameText (nameRefName (dataWriteTarget write))
                    | write <- Map.findWithDefault [] (StatementId 4) extras
                    ]
            assertBool "the first FK path reaches c" ("c" `elem` targets)
            assertBool "the second FK path reaches g through c(b)" ("g" `elem` targets)
        , testCase "MySQL SET foreign_key_checks dynamically controls foreign key cascading" $ do
            let baseSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT, FOREIGN KEY (pid) REFERENCES parent(id) ON DELETE CASCADE);"
                        , "SET foreign_key_checks = 0;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
                enabledSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT, FOREIGN KEY (pid) REFERENCES parent(id) ON DELETE CASCADE);"
                        , "SET @@foreign_key_checks = 1;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
            disabledGraph <- analyze MySQL baseSql
            enabledGraph <- analyze MySQL enabledSql
            let disabledDeps = dependenciesFor (StatementId 5) disabledGraph
                enabledDeps = dependenciesFor (StatementId 5) enabledGraph
            assertBool
                "SET foreign_key_checks = 0 disables cascade dependency"
                (not (has (StatementId 4) disabledDeps))
            assertBool
                "SET @@foreign_key_checks = 1 enables cascade dependency"
                (has (StatementId 4) enabledDeps)
        , testCase "MySQL SET @@global.foreign_key_checks leaves the session cascade intact" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT, FOREIGN KEY (pid) REFERENCES parent(id) ON DELETE CASCADE);"
                        , "SET @@global.foreign_key_checks = 0;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
            graph <- analyze MySQL sql
            let setDeps = dependenciesFor (StatementId 3) graph
                readDeps = dependenciesFor (StatementId 5) graph
            assertBool
                "the global setting does not disable the session cascade"
                (has (StatementId 4) readDeps)
            assertBool
                "the global setting carries no fence"
                (not (has (StatementId 1) setDeps || has (StatementId 2) setDeps))
        , testCase "MySQL SET @foreign_key_checks stays fenced as a user variable" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT, FOREIGN KEY (pid) REFERENCES parent(id) ON DELETE CASCADE);"
                        , "SET @foreign_key_checks = 0;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
            graph <- analyze MySQL sql
            let setDeps = dependenciesFor (StatementId 3) graph
                readDeps = dependenciesFor (StatementId 5) graph
            assertBool
                "the user variable does not disable the session cascade"
                (has (StatementId 4) readDeps)
            assertBool
                "the user variable keeps its fence"
                (has (StatementId 1) setDeps && has (StatementId 2) setDeps)
        , testCase "MySQL SET @autocommit stays fenced as a user variable" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT PRIMARY KEY);"
                        , "SET @autocommit = 0;"
                        , "INSERT INTO t VALUES (1);"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze MySQL sql
            let setDeps = dependenciesFor (StatementId 2) graph
                readDeps = dependenciesFor (StatementId 4) graph
            assertBool
                "the user variable records no session effect"
                (null (factSession (factsFor (StatementId 2) graph)))
            assertBool
                "the user variable keeps its fence"
                (has (StatementId 1) setDeps)
            assertBool
                "ordinary dependencies still resolve"
                (has (StatementId 3) readDeps)
        , testCase "MySQL SET @@autocommit maps without a fence" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT PRIMARY KEY);"
                        , "SET @@autocommit = 0;"
                        , "INSERT INTO t VALUES (1);"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze MySQL sql
            let setDeps = dependenciesFor (StatementId 2) graph
                writeDeps = dependenciesFor (StatementId 3) graph
            assertBool
                "the system spelling carries no fence"
                (not (has (StatementId 1) setDeps))
            assertBool
                "later statements keep the setting in the session chain"
                (has (StatementId 2) writeDeps)
        , testCase "SET @search_path stays fenced as a user variable" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT);"
                        , "SET @search_path = 'public';"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze MySQL sql
            let setDeps = dependenciesFor (StatementId 2) graph
            assertBool
                "the user variable records no session effect"
                (null (factSession (factsFor (StatementId 2) graph)))
            assertBool
                "the user variable keeps its fence"
                (has (StatementId 1) setDeps)
        , testCase "RESET @@global stays fenced" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT PRIMARY KEY);"
                        , "RESET @@global.foreign_key_checks;"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze MySQL sql
            let resetDeps = dependenciesFor (StatementId 2) graph
            assertBool
                "the @@ RESET records no session effect"
                (null (factSession (factsFor (StatementId 2) graph)))
            assertBool
                "the @@ RESET keeps its fence"
                (has (StatementId 1) resetDeps)
        , testCase "SQLite storage pragmas record without fencing" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (x INT);"
                        , "PRAGMA page_size = 4096;"
                        , "INSERT INTO t VALUES (1);"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze SQLite sql
            let pragmaDeps = dependenciesFor (StatementId 2) graph
                writeDeps = dependenciesFor (StatementId 3) graph
                readDeps = dependenciesFor (StatementId 4) graph
            assertBool
                "the inert pragma carries no fence"
                (not (has (StatementId 1) pragmaDeps))
            assertBool
                "later statements keep the pragma in the session chain"
                (has (StatementId 2) writeDeps)
            assertBool
                "ordinary dependencies still resolve"
                (has (StatementId 3) readDeps)
        , testCase "bare PRAGMA flag reads lift without a fence" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (x INT);"
                        , "PRAGMA recursive_triggers;"
                        , "INSERT INTO t VALUES (1);"
                        , "SELECT * FROM t;"
                        ]
                fencedSql =
                    BSC.unlines
                        [ "CREATE TABLE t (x INT);"
                        , "PRAGMA recursive_triggers = 0;"
                        , "INSERT INTO t VALUES (1);"
                        ]
            graph <- analyze SQLite sql
            fencedGraph <- analyze SQLite fencedSql
            let pragmaDeps = dependenciesFor (StatementId 2) graph
                writeDeps = dependenciesFor (StatementId 3) graph
                readDeps = dependenciesFor (StatementId 4) graph
            assertBool
                "the bare read carries no fence"
                (not (has (StatementId 1) pragmaDeps))
            assertBool
                "the bare read records no session effect"
                (null (factSession (factsFor (StatementId 2) graph)))
            assertBool
                "the bare read stays out of the session chain"
                (not (has (StatementId 2) writeDeps))
            assertBool
                "ordinary dependencies still resolve"
                (has (StatementId 3) readDeps)
            assertBool
                "the assignment keeps its fence"
                (has (StatementId 1) (dependenciesFor (StatementId 2) fencedGraph))
        , testCase "PostgreSQL COPY FROM writes to relation and COPY TO reads from relation" $ do
            let copyFromSql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT PRIMARY KEY);"
                        , "COPY t FROM '/tmp/data.csv';"
                        , "SELECT * FROM t;"
                        ]
                copyToSql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT PRIMARY KEY);"
                        , "INSERT INTO t VALUES (1);"
                        , "COPY t TO '/tmp/data.csv';"
                        ]
            fromGraph <- analyzeParsed PostgreSQL copyFromSql
            toGraph <- analyzeParsed PostgreSQL copyToSql
            let fromDeps = dependenciesFor (StatementId 3) fromGraph
                toDeps = dependenciesFor (StatementId 3) toGraph
            assertBool
                "SELECT reads from COPY FROM"
                (has (StatementId 2) fromDeps)
            assertBool
                "COPY TO reads from INSERT"
                (has (StatementId 2) toDeps)
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
        , testCase "SQLite virtual table barrier lowering isolates unrelated virtual tables" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE VIRTUAL TABLE vt0 USING fts5(c0);"
                        , "CREATE VIRTUAL TABLE vt1 USING fts4(c0);"
                        , "INSERT INTO vt1(c0) VALUES(0);"
                        , "BEGIN;"
                        , "UPDATE vt1 SET c0 = 0;"
                        , "INSERT INTO vt1(c0) VALUES (0), (0);"
                        , "UPDATE vt0 SET c0 = 0;"
                        , "INSERT INTO vt1(c0) VALUES (0);"
                        , "UPDATE vt1 SET c0 = 0;"
                        , "INSERT INTO vt1(vt1) VALUES('automerge=1');"
                        , "UPDATE vt1 SET c0 = 0;"
                        , "DROP TABLE vt1;"
                        ]
            graph <- analyzeParsed SQLite sql
            -- Statement 1: CREATE VIRTUAL TABLE vt0
            let facts1 = factsFor (StatementId 1) graph
            assertBool "statement 1 defines table vt0" (isJust (tableDefinition ["vt0"] (factOps facts1)))
            assertBool
                "statement 1 scopes barrier to relation vt0 instead of global"
                (any (\b -> barrierScope b == RelationScope (ref ["vt0"])) (factBarriers facts1))
            assertBool
                "statement 1 has no global barrier"
                (not (any (\b -> barrierScope b == GlobalScope) (factBarriers facts1)))

            -- Statement 2: CREATE VIRTUAL TABLE vt1
            let facts2 = factsFor (StatementId 2) graph
            assertBool "statement 2 defines table vt1" (isJust (tableDefinition ["vt1"] (factOps facts2)))
            assertBool
                "statement 2 scopes barrier to relation vt1 instead of global"
                (any (\b -> barrierScope b == RelationScope (ref ["vt1"])) (factBarriers facts2))

            -- Slicing Statement 12 (DROP TABLE vt1) should retain vt1 operations and exclude vt0 (statements 1 and 7)
            let deps12 = backwardSlice (StatementId 12) graph
            assertBool "vt1 drop reaches vt1 creation" (has (StatementId 2) deps12)
            assertBool "vt1 drop reaches vt1 initial insert" (has (StatementId 3) deps12)
            assertBool
                "vt1 drop does not depend on unrelated vt0 virtual table creation"
                (not (has (StatementId 1) deps12))
            assertBool "vt1 drop does not depend on unrelated vt0 update" (not (has (StatementId 7) deps12))

            -- Slicing Statement 7 (UPDATE vt0) should reach vt0 creation and not reach vt1 operations
            let deps7 = backwardSlice (StatementId 7) graph
            assertBool "vt0 update reaches vt0 creation" (has (StatementId 1) deps7)
            assertBool "vt0 update does not depend on vt1 creation" (not (has (StatementId 2) deps7))
            assertBool "vt0 update does not depend on vt1 initial write" (not (has (StatementId 3) deps7))
            assertBool "vt0 update does not depend on preceding vt1 update" (not (has (StatementId 5) deps7))
            assertBool "vt0 update does not depend on preceding vt1 insert" (not (has (StatementId 6) deps7))
            assertBool
                "vt0 update does not depend on any vt1 operations"
                (not (any (\s -> has (StatementId s) deps7) [2, 3, 5, 6, 8, 9, 10, 11, 12]))
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
        , testCase "sqlite abs projections fence with a global barrier" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE unrelated (id INT);"
                        , "CREATE TABLE t (a INT, b INT);"
                        , "SELECT abs(a) FROM t;"
                        ]
            graph <- analyze SQLite sql
            let deps = dependenciesFor (StatementId 3) graph
            assertBool
                "the fenced projection still chains every prior statement"
                (has (StatementId 1) deps && has (StatementId 2) deps)
            assertEqual
                "argument reads survive the restored fence"
                ["a"]
                [identText column | readFact <- readsFor (StatementId 3) graph, column <- readColumns readFact]
        , testCase "unknown function calls still fence with a global barrier" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE unrelated (id INT);"
                        , "CREATE TABLE t (a INT, b INT);"
                        , "SELECT frobnicate(a) FROM t;"
                        ]
            graph <- analyze SQLite sql
            let deps = dependenciesFor (StatementId 3) graph
            assertBool
                "the unknown call still chains every prior statement"
                (has (StatementId 1) deps && has (StatementId 2) deps)
        , testCase "generic coalesce group projections keep reads without a global fence" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE unrelated (id INT);"
                        , "CREATE TABLE t (a INT, b INT);"
                        , "SELECT a, coalesce(b, 0) FROM t GROUP BY a;"
                        ]
            graph <- analyze GenericSQL sql
            let deps = dependenciesFor (StatementId 3) graph
            assertBool
                "the generic projection depends on its table definition"
                (has (StatementId 2) deps)
            assertBool
                "no global fence chains the unrelated table"
                (not (has (StatementId 1) deps))
            assertEqual
                "grouping and argument reads survive the lifted fence"
                ["a", "b"]
                [identText column | readFact <- readsFor (StatementId 3) graph, column <- readColumns readFact]
        , testCase "filtered aggregate projections fence with a global barrier" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE unrelated (id INT);"
                        , "CREATE TABLE t (a INT, b INT);"
                        , "SELECT sum(a) FILTER (WHERE b > 0) FROM t;"
                        ]
            graph <- analyze SQLite sql
            let deps = dependenciesFor (StatementId 3) graph
            assertBool
                "the filtered projection still chains every prior statement"
                (has (StatementId 1) deps && has (StatementId 2) deps)
            assertEqual
                "argument and condition reads survive the restored fence"
                ["a", "b"]
                [identText column | readFact <- readsFor (StatementId 3) graph, column <- readColumns readFact]
        , testCase "scalar FILTER still fences with a global barrier" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE unrelated (id INT);"
                        , "CREATE TABLE t (a INT, b INT);"
                        , "SELECT abs(a) FILTER (WHERE b > 0) FROM t;"
                        ]
            graph <- analyze SQLite sql
            let deps = dependenciesFor (StatementId 3) graph
            assertBool
                "the scalar FILTER still chains every prior statement"
                (has (StatementId 1) deps && has (StatementId 2) deps)
        , testCase "generic keyword coalesce keeps reads without a global fence" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE unrelated (id INT);"
                        , "CREATE TABLE t (a INT, b INT);"
                        , "SELECT COALESCE(a, b) FROM t;"
                        ]
            graph <- analyze GenericSQL sql
            let deps = dependenciesFor (StatementId 3) graph
            assertBool
                "the generic projection depends on its table definition"
                (has (StatementId 2) deps)
            assertBool
                "no global fence chains the unrelated table"
                (not (has (StatementId 1) deps))
            assertEqual
                "argument reads survive the lifted fence"
                ["a", "b"]
                [identText column | readFact <- readsFor (StatementId 3) graph, column <- readColumns readFact]
        , testCase "mysql index prefixes carry no routine fence and keep the index" $ do
            let sql =
                    BSC.unlines
                        ["CREATE TEMPORARY TABLE t1 (i INT, b BLOB, PRIMARY KEY pk (i), INDEX sk (b(1)));"]
            graph <- analyze MySQL sql
            assertBool
                "the index prefix carries no routine fence"
                (null (factBarriers (factsFor (StatementId 1) graph)))
            let constraints =
                    maybe [] tableConstraints (tableDefinition ["t1"] (opsFor (StatementId 1) graph))
            assertBool
                "the index definition survives the lifted fence"
                (any (isNamedIndex "sk") constraints)
        , testCase "partitioned parent reads depend on child definitions and writes" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p (a INT) PARTITION BY RANGE (a);"
                        , "CREATE TABLE c PARTITION OF p FOR VALUES FROM (1) TO (10);"
                        , "INSERT INTO c VALUES (5);"
                        , "SELECT * FROM p;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the partition parent carries no routine fence"
                (null (factBarriers (factsFor (StatementId 1) graph)))
            assertBool
                "the partition child carries no DDL fence"
                (null (factBarriers (factsFor (StatementId 2) graph)))
            assertEqual
                "the child fact binds its partition parent"
                (Just (ref ["p"]))
                (tableDefinition ["c"] (opsFor (StatementId 2) graph) >>= tablePartitionParent)
            assertEqual
                "the parent read sees the child definition and write"
                (Set.fromList [StatementId 1, StatementId 2, StatementId 3])
                (dependenciesFor (StatementId 4) graph)
        , testCase "inheritance children route parent reads like partitions" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p (a INT);"
                        , "CREATE TABLE c (a INT) INHERITS (p);"
                        , "INSERT INTO c VALUES (5);"
                        , "SELECT * FROM p;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the inheriting child carries no DDL fence"
                (null (factBarriers (factsFor (StatementId 2) graph)))
            assertEqual
                "the parent read sees the child definition and write"
                (Set.fromList [StatementId 1, StatementId 2, StatementId 3])
                (dependenciesFor (StatementId 4) graph)
        , testCase "ONLY skips partition children on reads and writes" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p (a INT) PARTITION BY RANGE (a);"
                        , "CREATE TABLE c PARTITION OF p FOR VALUES FROM (1) TO (10);"
                        , "INSERT INTO c VALUES (5);"
                        , "SELECT * FROM ONLY p;"
                        , "UPDATE ONLY p SET a = 6;"
                        , "SELECT * FROM c;"
                        ]
            graph <- analyze PostgreSQL sql
            let onlyRead = dependenciesFor (StatementId 4) graph
                childRead = dependenciesFor (StatementId 6) graph
            assertBool
                "an ONLY parent read keeps the parent definition"
                (has (StatementId 1) onlyRead)
            assertBool
                "an ONLY parent read skips the children"
                (not (has (StatementId 2) onlyRead) && not (has (StatementId 3) onlyRead))
            assertBool
                "the child read keeps its own definition and write"
                (has (StatementId 2) childRead && has (StatementId 3) childRead)
            assertBool
                "an ONLY parent write is not recorded on the child"
                (not (has (StatementId 5) childRead))
        , testCase "parent writes route into partition children" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p (a INT) PARTITION BY RANGE (a);"
                        , "CREATE TABLE c PARTITION OF p FOR VALUES FROM (1) TO (10);"
                        , "INSERT INTO p VALUES (5);"
                        , "SELECT * FROM c;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the routed parent write reaches direct child reads"
                (has (StatementId 3) (dependenciesFor (StatementId 4) graph))
        , testCase "INHERITS INSERT lands parent-only while other writes fan out" $ do
            let insertSql =
                    BSC.unlines
                        [ "CREATE TABLE p (a INT);"
                        , "CREATE TABLE c (a INT) INHERITS (p);"
                        , "INSERT INTO p VALUES (5);"
                        , "SELECT * FROM c;"
                        , "SELECT * FROM p;"
                        ]
                updateSql =
                    BSC.unlines
                        [ "CREATE TABLE p (a INT);"
                        , "CREATE TABLE c (a INT) INHERITS (p);"
                        , "UPDATE p SET a = 6;"
                        , "SELECT * FROM c;"
                        ]
            insertGraph <- analyze PostgreSQL insertSql
            updateGraph <- analyze PostgreSQL updateSql
            let childRead = dependenciesFor (StatementId 4) insertGraph
                parentRead = dependenciesFor (StatementId 5) insertGraph
            assertBool
                "the child read keeps its own definition"
                (has (StatementId 2) childRead)
            assertBool
                "the parent INSERT is not recorded on the INHERITS child"
                (not (has (StatementId 3) childRead))
            assertBool
                "the parent read still sees the child definition and the INSERT"
                (has (StatementId 1) parentRead && has (StatementId 2) parentRead && has (StatementId 3) parentRead)
            assertBool
                "the parent UPDATE still fans out to the INHERITS child"
                (has (StatementId 3) (dependenciesFor (StatementId 4) updateGraph))
        , testCase "attach establishes routing and detach breaks it" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p (a INT) PARTITION BY RANGE (a);"
                        , "CREATE TABLE c (a INT);"
                        , "ALTER TABLE p ATTACH PARTITION c FOR VALUES FROM (1) TO (10);"
                        , "INSERT INTO c VALUES (5);"
                        , "SELECT * FROM p;"
                        , "ALTER TABLE p DETACH PARTITION c;"
                        , "SELECT * FROM p;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "attach and detach emit no fence"
                ( null (factBarriers (factsFor (StatementId 3) graph))
                    && null (factBarriers (factsFor (StatementId 6) graph))
                )
            assertBool
                "attach emits a partition link op"
                (any isAttach (opsFor (StatementId 3) graph))
            assertBool
                "detach emits a partition unlink op"
                (any isDetach (opsFor (StatementId 6) graph))
            assertEqual
                "the pre-detach read sees the attach and the child"
                (Set.fromList [StatementId 1, StatementId 2, StatementId 3, StatementId 4])
                (dependenciesFor (StatementId 5) graph)
            let postDetach = dependenciesFor (StatementId 7) graph
            assertBool
                "the post-detach read keeps the parent and the routing changes"
                ( has (StatementId 1) postDetach
                    && has (StatementId 3) postDetach
                    && has (StatementId 6) postDetach
                )
            assertBool
                "the post-detach read drops the detached child"
                (not (has (StatementId 2) postDetach) && not (has (StatementId 4) postDetach))
        , testCase "OUTPUT INTO adds a write edge to the target table" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT);"
                        , "CREATE TABLE t2 (a INT);"
                        , "INSERT INTO t (a) OUTPUT inserted.a INTO t2 SELECT a FROM t;"
                        , "SELECT * FROM t2;"
                        ]
            graph <- analyze MsSql sql
            assertBool
                "OUTPUT INTO a real table carries no fence"
                (null (factBarriers (factsFor (StatementId 3) graph)))
            assertEqual
                "the host and OUTPUT writes are both recorded"
                (Set.fromList ["t", "t2"])
                ( Set.fromList
                    [nameText (nameRefName (dataWriteTarget write)) | write <- writesFor (StatementId 3) graph]
                )
            assertBool
                "later reads of the OUTPUT target retain the statement"
                (has (StatementId 3) (dependenciesFor (StatementId 4) graph))
        , testCase "OUTPUT INTO a table variable fences at relation scope" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT);"
                        , "UPDATE t SET a = 1 OUTPUT deleted.a INTO @var;"
                        ]
            graph <- analyze MsSql sql
            assertEqual
                "the table variable is fenced without a global barrier"
                [RelationScope (ref ["@var"])]
                [barrierScope barrier | barrier <- factBarriers (factsFor (StatementId 2) graph)]
            assertBool
                "the host write is still recorded"
                (hasWrite ["t"] (UpdateWrite UpdateAllRows) (opsFor (StatementId 2) graph))
        , testCase "OUTPUT without INTO adds no edge and no fence" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT);"
                        , "DELETE FROM t OUTPUT deleted.*;"
                        ]
            graph <- analyze MsSql sql
            assertBool
                "a read-only OUTPUT projection carries no fence"
                (null (factBarriers (factsFor (StatementId 2) graph)))
            assertEqual
                "no extra write edge is recorded"
                1
                (length (writesFor (StatementId 2) graph))
        , testCase "INHERIT adds and NO INHERIT drops inheritance parents" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p1 (a INT);"
                        , "CREATE TABLE p2 (a INT);"
                        , "CREATE TABLE p3 (a INT);"
                        , "CREATE TABLE c (a INT) INHERITS (p1, p2);"
                        , "INSERT INTO c VALUES (1);"
                        , "SELECT * FROM p3;"
                        , "ALTER TABLE c INHERIT p3;"
                        , "SELECT * FROM p3;"
                        , "ALTER TABLE c NO INHERIT p2;"
                        , "SELECT * FROM p2;"
                        , "SELECT * FROM p1;"
                        , "SELECT * FROM p3;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "inherit and no-inherit emit no fence"
                ( null (factBarriers (factsFor (StatementId 7) graph))
                    && null (factBarriers (factsFor (StatementId 9) graph))
                )
            assertBool
                "INHERIT emits an inherit link op"
                (any isAttachInherit (opsFor (StatementId 7) graph))
            assertBool
                "NO INHERIT emits an inherit unlink op"
                (any isDetachInherit (opsFor (StatementId 9) graph))
            assertEqual
                "the pre-INHERIT read sees only its parent"
                (Set.fromList [StatementId 3])
                (dependenciesFor (StatementId 6) graph)
            assertEqual
                "the post-INHERIT read sees the link and the child"
                (Set.fromList [StatementId 3, StatementId 4, StatementId 5, StatementId 7])
                (dependenciesFor (StatementId 8) graph)
            assertEqual
                "dropping an INHERITS-origin parent removes the child"
                (Set.fromList [StatementId 2, StatementId 9])
                (dependenciesFor (StatementId 10) graph)
            assertEqual
                "the untouched parent still sees the child"
                (Set.fromList [StatementId 1, StatementId 4, StatementId 5])
                (dependenciesFor (StatementId 11) graph)
            assertEqual
                "the INHERIT-added parent still sees the child"
                (Set.fromList [StatementId 3, StatementId 4, StatementId 5, StatementId 7])
                (dependenciesFor (StatementId 12) graph)
        , testCase "INSERT into an INHERIT-added parent lands parent-only" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p (a INT);"
                        , "CREATE TABLE c (a INT);"
                        , "ALTER TABLE c INHERIT p;"
                        , "INSERT INTO p VALUES (5);"
                        , "SELECT * FROM c;"
                        , "SELECT * FROM p;"
                        , "UPDATE p SET a = 6;"
                        , "SELECT * FROM c;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "INHERIT emits no fence"
                (null (factBarriers (factsFor (StatementId 3) graph)))
            assertBool
                "INHERIT emits an inherit link op"
                (any isAttachInherit (opsFor (StatementId 3) graph))
            assertEqual
                "the child read misses the parent-only INSERT (a partition-misfiled link would route it here)"
                (Set.fromList [StatementId 2])
                (dependenciesFor (StatementId 5) graph)
            assertEqual
                "the parent read sees the child and the INSERT"
                (Set.fromList [StatementId 1, StatementId 2, StatementId 3, StatementId 4])
                (dependenciesFor (StatementId 6) graph)
            assertBool
                "non-INSERT parent writes still fan out to INHERIT-added children"
                (has (StatementId 7) (dependenciesFor (StatementId 8) graph))
        , testCase "upsert conflict targets and SET sources read the target row" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE item_inventory (sku VARCHAR(32) PRIMARY KEY, item_name VARCHAR(64) NOT NULL, quantity INT NOT NULL);"
                        , "INSERT INTO item_inventory (sku, item_name, quantity) VALUES ('SKU-001', 'Widget Alpha', 30) ON CONFLICT (sku) DO UPDATE SET quantity = item_inventory.quantity + EXCLUDED.quantity;"
                        , "INSERT INTO item_inventory (sku, item_name, quantity) VALUES ('SKU-002', 'Gadget Beta', 20) ON CONFLICT (sku) DO NOTHING;"
                        ]
            graph <- analyze SQLite sql
            assertBool
                "the upsert writes with the conflict-update action"
                (hasWrite ["item_inventory"] (InsertWrite ConflictUpdate) (opsFor (StatementId 2) graph))
            assertEqual
                "the arbiter probe and SET sources read only the target"
                (Set.singleton "item_inventory")
                (readSources (StatementId 2) graph)
            assertBool
                "the arbiter and SET-source columns are covered"
                ( Set.fromList ["sku", "quantity"]
                    `Set.isSubsetOf` Set.fromList
                        [identText column | readFact <- allReadFacts (StatementId 2) graph, column <- readColumns readFact]
                )
            assertBool
                "DO NOTHING keeps the conflict-ignore action"
                (hasWrite ["item_inventory"] (InsertWrite ConflictIgnore) (opsFor (StatementId 3) graph))
            assertEqual
                "DO NOTHING reads only the arbiter column"
                (Set.singleton "sku")
                ( Set.fromList
                    [identText column | readFact <- allReadFacts (StatementId 3) graph, column <- readColumns readFact]
                )
        , testCase "mysql duplicate-key probe and SET sources read the target row" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE unrelated (id INT);"
                        , "CREATE TABLE t (a INT, b INT, c INT);"
                        , "INSERT INTO t (a) VALUES (1) ON DUPLICATE KEY UPDATE a = VALUES(b);"
                        , "INSERT INTO t (a, b) VALUES (1, 2) ON DUPLICATE KEY UPDATE a = b + c;"
                        ]
            graph <- analyze MySQL sql
            assertBool
                "the VALUES upsert writes with the conflict-update action"
                (hasWrite ["t"] (InsertWrite ConflictUpdate) (opsFor (StatementId 3) graph))
            assertEqual
                "the VALUES probe reads only the target"
                (Set.singleton "t")
                (readSources (StatementId 3) graph)
            assertEqual
                "VALUES(b) is proposed-row only: the probe covers just the key"
                (Set.singleton "a")
                ( Set.fromList
                    [identText column | readFact <- allReadFacts (StatementId 3) graph, column <- readColumns readFact]
                )
            assertBool
                "the VALUES marker carries no routine fence"
                (null (factBarriers (factsFor (StatementId 3) graph)))
            let deps3 = dependenciesFor (StatementId 3) graph
            assertBool
                "the VALUES upsert depends on its table definition"
                (has (StatementId 2) deps3)
            assertBool
                "no global fence chains the unrelated table"
                (not (has (StatementId 1) deps3))
            assertBool
                "the SET-source upsert writes with the conflict-update action"
                (hasWrite ["t"] (InsertWrite ConflictUpdate) (opsFor (StatementId 4) graph))
            assertBool
                "probe keys and SET sources are covered"
                ( Set.fromList ["a", "b", "c"]
                    `Set.isSubsetOf` Set.fromList
                        [identText column | readFact <- allReadFacts (StatementId 4) graph, column <- readColumns readFact]
                )
            assertBool
                "the SET-source upsert carries no fence"
                (null (factBarriers (factsFor (StatementId 4) graph)))
        , testCase "unconditional UPDATE overwrites prior updates on the same column (DWE)" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT, b INT);"
                        , "INSERT INTO t VALUES (1, 10, 20);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET a = 2;"
                        , "SELECT a FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            assertEqual
                "S3 is UpdateWrite UpdateAllRows"
                (Just (UpdateWrite UpdateAllRows))
                (listToMaybe [dataWriteKind w | WriteDataOp w <- opsFor (StatementId 3) graph])
            assertEqual
                "S4 is UpdateWrite UpdateAllRows"
                (Just (UpdateWrite UpdateAllRows))
                (listToMaybe [dataWriteKind w | WriteDataOp w <- opsFor (StatementId 4) graph])
            assertBool
                "S4 does not depend on S3"
                (not (has (StatementId 3) (dependenciesFor (StatementId 4) graph)))
            assertBool
                "S4 strictly preserves S2 (INSERT) dependency"
                (has (StatementId 2) (dependenciesFor (StatementId 4) graph))
            assertBool
                "S5 read does not depend on S3"
                (not (has (StatementId 3) (dependenciesFor (StatementId 5) graph)))
            assertBool
                "S5 read depends on S4 (latest update)"
                (has (StatementId 4) (dependenciesFor (StatementId 5) graph))
            assertBool
                "S5 read strictly preserves S2 (INSERT)"
                (has (StatementId 2) (dependenciesFor (StatementId 5) graph))
            let slice = backwardSlice (StatementId 5) graph
            assertBool "backward slice excludes dead update S3" (not (has (StatementId 3) slice))
            assertBool
                "backward slice includes S1, S2, S4, S5"
                (Set.fromList [StatementId 1, StatementId 2, StatementId 4, StatementId 5] `Set.isSubsetOf` slice)
        , testCase "unconditional UPDATE does not overwrite prior update on a distinct column" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT, b INT);"
                        , "INSERT INTO t VALUES (1, 10, 20);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET b = 2;"
                        , "SELECT a, b FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            let slice = backwardSlice (StatementId 5) graph
            assertBool
                "reading both columns requires both updates"
                (has (StatementId 3) slice && has (StatementId 4) slice)
        , testCase "filtered UPDATE does not overwrite prior update (requires both)" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1 WHERE id = 1;"
                        , "UPDATE t SET a = 2 WHERE id = 2;"
                        , "SELECT a FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            assertEqual
                "filtered S3 is UpdateWrite UpdateFilteredRows"
                (Just (UpdateWrite UpdateFilteredRows))
                (listToMaybe [dataWriteKind w | WriteDataOp w <- opsFor (StatementId 3) graph])
            let slice = backwardSlice (StatementId 5) graph
            assertBool
                "filtered updates both reach subsequent read"
                (has (StatementId 3) slice && has (StatementId 4) slice)
        , testCase "UPDATE reading the updated column retains dependency on prior write" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET a = a + 1;"
                        , "SELECT a FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "self-referencing update S4 depends on S3"
                (has (StatementId 3) (dependenciesFor (StatementId 4) graph))
            let slice = backwardSlice (StatementId 5) graph
            assertBool "backward slice retains S3" (has (StatementId 3) slice)
        , testCase "UPDATE with LIMIT is classified as UpdateFilteredRows and retains prior update" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET a = 2 LIMIT 1;"
                        , "SELECT a FROM t;"
                        ]
            graph <- analyze MySQL sql
            assertEqual
                "S4 with LIMIT is UpdateFilteredRows"
                (Just (UpdateWrite UpdateFilteredRows))
                (listToMaybe [dataWriteKind w | WriteDataOp w <- opsFor (StatementId 4) graph])
            let slice = backwardSlice (StatementId 5) graph
            assertBool
                "backward slice retains prior update S3 when S4 has LIMIT"
                (has (StatementId 3) slice)
        , testCase "unconditional UPDATE with subquery reading target retains read dependency on prior writes" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET a = (SELECT 1 FROM t);"
                        , "SELECT a FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            assertEqual
                "S4 is UpdateWrite UpdateAllRows"
                (Just (UpdateWrite UpdateAllRows))
                (listToMaybe [dataWriteKind w | WriteDataOp w <- opsFor (StatementId 4) graph])
            assertBool
                "subquery reading target makes S4 depend on S3"
                (has (StatementId 3) (dependenciesFor (StatementId 4) graph))
            let slice = backwardSlice (StatementId 5) graph
            assertBool "backward slice retains S3" (has (StatementId 3) slice)
        , testCase "SELECT COUNT(*) does not depend on prior UPDATE statements on the table" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET a = 2 WHERE id = 1;"
                        , "SELECT COUNT(*) FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            let slice = backwardSlice (StatementId 5) graph
            assertBool "COUNT(*) read S5 does not depend on UPDATE S3" (not (has (StatementId 3) slice))
            assertBool "COUNT(*) read S5 does not depend on UPDATE S4" (not (has (StatementId 4) slice))
            assertBool "COUNT(*) read S5 preserves CREATE TABLE S1" (has (StatementId 1) slice)
            assertBool "COUNT(*) read S5 preserves INSERT S2" (has (StatementId 2) slice)
        , testCase "PostgreSQL quoted COUNT lookalikes preserve preceding UPDATEs" $ do
            let script call =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT);"
                        , "INSERT INTO t VALUES (10);"
                        , "UPDATE t SET a = 1;"
                        , "SELECT " <> call <> " FROM t;"
                        ]
                sliceFor call = backwardSlice (StatementId 4) <$> analyze PostgreSQL (script call)
            builtin <- sliceFor "\"pg_catalog\".COUNT(1)"
            assertBool "builtin only reads cardinality" (not (has (StatementId 3) builtin))
            mapM_
                ( \call -> do
                    slice <- sliceFor call
                    assertBool (BSC.unpack call <> " retains the preceding UPDATE") (has (StatementId 3) slice)
                )
                ["\"COUNT\"(1)", "\"PG_CATALOG\".count(1)", "pg_catalog.\"COUNT\"(1)"]
        , testCase "SELECT COUNT(*) does not depend on prior UPDATE even if subsequent INSERT exists" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "INSERT INTO t VALUES (2, 20);"
                        , "SELECT COUNT(*) FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            let slice = backwardSlice (StatementId 5) graph
            assertBool "COUNT(*) read S5 does not depend on UPDATE S3" (not (has (StatementId 3) slice))
            assertBool "COUNT(*) read S5 preserves INSERT S2" (has (StatementId 2) slice)
            assertBool "COUNT(*) read S5 preserves INSERT S4" (has (StatementId 4) slice)
        , testCase "SELECT COUNT(col) and COUNT(*) with WHERE preserve dependencies on prior UPDATE" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "SELECT COUNT(a) FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            let slice = backwardSlice (StatementId 4) graph
            assertBool "COUNT(col) depends on prior update on col" (has (StatementId 3) slice)

            let sqlFiltered =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "SELECT COUNT(*) FROM t WHERE a > 0;"
                        ]
            graphFiltered <- analyze PostgreSQL sqlFiltered
            let sliceFiltered = backwardSlice (StatementId 4) graphFiltered
            assertBool "COUNT(*) with WHERE depends on prior update on col" (has (StatementId 3) sliceFiltered)
        , testCase "Pure existence checks do not depend on prior UPDATE statements on the table" $ do
            let sqlLimit =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "SELECT 1 FROM t LIMIT 1;"
                        ]
            graphLimit <- analyze PostgreSQL sqlLimit
            let sliceLimit = backwardSlice (StatementId 4) graphLimit
            assertBool "SELECT 1 LIMIT 1 does not depend on UPDATE S3" (not (has (StatementId 3) sliceLimit))
            assertBool "SELECT 1 LIMIT 1 preserves INSERT S2" (has (StatementId 2) sliceLimit)

            let sqlExists =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "SELECT EXISTS (SELECT 1 FROM t);"
                        ]
            graphExists <- analyze PostgreSQL sqlExists
            let sliceExists = backwardSlice (StatementId 4) graphExists
            assertBool "EXISTS check does not depend on UPDATE S3" (not (has (StatementId 3) sliceExists))
            assertBool "EXISTS check preserves INSERT S2" (has (StatementId 2) sliceExists)
        , testProperty
            "generated SQLite catalog transitions match a table snapshot model"
            prop_catalogTransitions
        ]

nativeSliceChecks :: TestTree
nativeSliceChecks =
    testGroup
        "Execute raw dependency slices"
        [ testCase "SQLite savepoint rollback retains the committed value" $
            checkNativeSlice
                SQLite
                [ "CREATE TABLE noise (x INT);"
                , "CREATE TABLE t (x INT);"
                , "INSERT INTO t VALUES (42);"
                , "SAVEPOINT s;"
                , "UPDATE t SET x = 99;"
                , "ROLLBACK TO s;"
                , "RELEASE s;"
                , "SELECT x AS answer FROM t;"
                ]
                "SELECT 42 AS answer;"
                [1]
                [(3, "SELECT 0 AS answer WHERE 0;")]
        , testCase "SQLite trigger write keeps the trigger and firing insert" $
            checkNativeSlice
                SQLite
                [ "CREATE TABLE noise (x INT);"
                , "CREATE TABLE input (x INT);"
                , "CREATE TABLE audit (x INT);"
                , "CREATE TRIGGER log_insert AFTER INSERT ON input BEGIN INSERT INTO audit VALUES (NEW.x); END;"
                , "INSERT INTO input VALUES (42);"
                , "SELECT x AS answer FROM audit;"
                ]
                "SELECT 42 AS answer;"
                [1]
                [(4, "SELECT 0 AS answer WHERE 0;"), (5, "SELECT 0 AS answer WHERE 0;")]
        , testCase "SQLite foreign-key cascade keeps enforcement and parent delete" $
            checkNativeSlice
                SQLite
                [ "CREATE TABLE noise (x INT);"
                , "CREATE TABLE parent (id INT PRIMARY KEY);"
                , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE);"
                , "PRAGMA foreign_keys = 1;"
                , "INSERT INTO parent VALUES (42);"
                , "INSERT INTO child VALUES (42);"
                , "DELETE FROM parent WHERE id = 42;"
                , "SELECT pid AS answer FROM child;"
                ]
                "SELECT 0 AS answer WHERE 0;"
                [1]
                [(4, "SELECT 42 AS answer;"), (7, "SELECT 42 AS answer;")]
        , sqliteForeignKeyBoundaryChecks
        , generatedNativeSliceChecks
        , testCase "DuckDB transaction rollback retains the committed value" $
            checkNativeSlice
                DuckDB
                [ "CREATE TABLE noise (x INT);"
                , "CREATE TABLE t (x INT);"
                , "INSERT INTO t VALUES (42);"
                , "BEGIN TRANSACTION;"
                , "UPDATE t SET x = 99;"
                , "ROLLBACK;"
                , "SELECT x AS answer FROM t;"
                ]
                "SELECT 42 AS answer;"
                [1]
                [(3, "SELECT 0 AS answer WHERE FALSE;")]
        ]

generatedNativeSliceChecks :: TestTree
generatedNativeSliceChecks =
    testGroup
        "Generated native traces"
        [ testGroup
            "Consecutive transactions"
            [ testCase (show (dialect, keepFirst, keepSecond)) $
                checkNativeSlice
                    dialect
                    (transactionInput keepFirst keepSecond)
                    (transactionResult keepFirst keepSecond)
                    [1]
                    []
            | dialect <- [SQLite, DuckDB]
            , keepFirst <- [False, True]
            , keepSecond <- [False, True]
            ]
        , testGroup
            "Primary key conflict after a transaction"
            [ testCase (show dialect) $
                checkNativeConstraintSlice
                    dialect
                    [ noise
                    , "CREATE TABLE t (x INT PRIMARY KEY);"
                    , "BEGIN TRANSACTION;"
                    , "INSERT INTO t VALUES (1);"
                    , "COMMIT;"
                    , "INSERT INTO t VALUES (1);"
                    ]
            | dialect <- [SQLite, DuckDB]
            ]
        , testCase "SQLite cascade preserves the payload of surviving children" $
            checkNativeSlice
                SQLite
                [ noise
                , "PRAGMA foreign_keys = 1;"
                , "CREATE TABLE parent (id INT PRIMARY KEY);"
                , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE, payload INT);"
                , "INSERT INTO parent VALUES (1), (2);"
                , "INSERT INTO child VALUES (1, 0), (2, 0);"
                , "UPDATE child SET payload = 42;"
                , "DELETE FROM parent WHERE id = 1;"
                , "SELECT payload FROM child;"
                ]
                "SELECT 42 AS payload;"
                [1]
                [(7, "SELECT 0 AS payload;")]
        , testGroup
            "SQLite savepoints with cascades and triggers"
            [ testCase (show (reuseName, undoOuter, auditOnly)) $
                checkNativeSlice
                    SQLite
                    (savepointInput reuseName undoOuter ++ [observer auditOnly])
                    (savepointResult undoOuter auditOnly)
                    [1]
                    []
            | (reuseName, undoOuter) <- [(False, False), (False, True), (True, False)]
            , auditOnly <- [False, True]
            ]
        ]
    where
        noise = "CREATE TABLE noise (x INT);"
        finish True = "COMMIT;"
        finish False = "ROLLBACK;"
        transactionInput keepFirst keepSecond =
            [ noise
            , "CREATE TABLE t (x INT PRIMARY KEY);"
            , "INSERT INTO t VALUES (1);"
            , "BEGIN TRANSACTION;"
            , "INSERT INTO t VALUES (2);"
            , finish keepFirst
            , "BEGIN TRANSACTION;"
            , "DELETE FROM t WHERE x = 1;"
            , "INSERT INTO t VALUES (3);"
            , finish keepSecond
            , "SELECT x FROM t ORDER BY x;"
            ]
        transactionResult keepFirst keepSecond =
            "WITH results(x) AS (VALUES (1), (2), (3)) SELECT x FROM results WHERE "
                <> BSC.intercalate
                    " OR "
                    (["x = 1" | not keepSecond] ++ ["x = 2" | keepFirst] ++ ["x = 3" | keepSecond])
                <> " ORDER BY x;"
        savepointInput reuseName undoOuter =
            [ noise
            , "PRAGMA foreign_keys = 1;"
            , "CREATE TABLE parent (id INT PRIMARY KEY);"
            , "CREATE TABLE child (id INT REFERENCES parent(id) ON DELETE CASCADE);"
            , "CREATE TABLE audit (id INT);"
            , "CREATE TRIGGER log_delete AFTER DELETE ON child BEGIN INSERT INTO audit VALUES (OLD.id); END;"
            , "INSERT INTO parent VALUES (1), (2);"
            , "INSERT INTO child VALUES (1), (2);"
            , "SAVEPOINT outer_sp;"
            , "DELETE FROM parent WHERE id = 1;"
            , "SAVEPOINT " <> inner <> ";"
            , "DELETE FROM parent WHERE id = 2;"
            , "ROLLBACK TO " <> (if undoOuter then "outer_sp" else inner) <> ";"
            ]
                ++ ["RELEASE " <> inner <> ";" | not undoOuter]
                ++ ["RELEASE outer_sp;"]
            where
                inner = if reuseName then "outer_sp" else "inner_sp"
        observer True = "SELECT id FROM audit ORDER BY id;"
        observer False =
            "SELECT 'audit' AS source, id FROM audit UNION ALL SELECT 'child', id FROM child UNION ALL SELECT 'parent', id FROM parent ORDER BY source, id;"
        savepointResult True True = "SELECT 0 AS id WHERE FALSE;"
        savepointResult False True = "SELECT 1 AS id;"
        savepointResult True False =
            "SELECT 'child' AS source, 1 AS id UNION ALL SELECT 'child', 2 UNION ALL SELECT 'parent', 1 UNION ALL SELECT 'parent', 2;"
        savepointResult False False =
            "SELECT 'audit' AS source, 1 AS id UNION ALL SELECT 'child', 2 UNION ALL SELECT 'parent', 2;"

sqliteForeignKeyBoundaryChecks :: TestTree
sqliteForeignKeyBoundaryChecks =
    testGroup
        "SQLite foreign-key PRAGMA transaction boundaries"
        [ testGroup
            label
            [ testCase "ignores disabling enforcement inside the transaction" $ do
                let input =
                        schema
                            ++ [ "PRAGMA foreign_keys = ON;"
                               , opener
                               , "PRAGMA main.foreign_keys(OFF);"
                               , deleteParent
                               , closer
                               , readChild
                               ]
                checkNativeSlice SQLite input emptyChild [1, 8] [(6, presentChild), (9, presentChild)]
                checkCascade input 9 True
            , testCase "ignored enable does not persist after the transaction" $ do
                let input = schema ++ [opener, "PRAGMA foreign_keys(YES);", closer, deleteParent, readChild]
                checkNativeSlice SQLite input presentChild [1, 7, 9] [(5, emptyChild)]
                checkCascade input 9 False
            , testCase "enabling after the transaction retains the exit boundary" $ do
                let input =
                        schema
                            ++ [opener, "PRAGMA foreign_keys = 0;", closer, "PRAGMA foreign_keys = 1;", deleteParent, readChild]
                checkNativeSlice SQLite input emptyChild [1, 7] [(8, presentChild), (9, presentChild)]
                checkCascade input 10 True
            ]
        | (label, opener, closer) <-
            [ ("BEGIN", "BEGIN;", "COMMIT;")
            , ("outer SAVEPOINT", "SAVEPOINT s;", "RELEASE s;")
            ]
        ]
    where
        schema =
            [ "CREATE TABLE noise (x INT);"
            , "CREATE TABLE parent (id INT PRIMARY KEY);"
            , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE);"
            , "INSERT INTO parent VALUES (42);"
            , "INSERT INTO child VALUES (42);"
            ]
        deleteParent = "DELETE FROM parent WHERE id = 42;"
        readChild = "SELECT pid AS answer FROM child;"
        presentChild = "SELECT 42 AS answer;"
        emptyChild = "SELECT 0 AS answer WHERE 0;"
        checkCascade input sid expected = do
            parsed <-
                parseSQL (defaultParseOptions SQLite) (BSC.unlines input) >>= either (assertFailure . show) pure
            let cascades = cascadeWrites (parsedInitialSession parsed) (parsedUnits parsed)
            assertEqual
                "cascade diagnostics agree with enforcement"
                expected
                (Map.member (StatementId sid) cascades)

-- The native APIs currently expose execution errors as text. Match the
-- constraint diagnostic locally so an unrelated SQL error cannot pass.
checkNativeConstraintSlice :: SQLDialect -> [ByteString] -> IO ()
checkNativeConstraintSlice dialect input = do
    (retained, render) <- nativeSlice dialect input
    assertBool "irrelevant statement retained" (Set.notMember (StatementId 1) retained)
    checkConflict (BSC.unlines input)
    checkConflict (render retained)
    where
        checkConflict sql = do
            conflict <-
                case dialect of
                    DuckDB -> do
                        outcome <- executeDuckDBCandidateDetailedTimeout 5000000 sql
                        pure $ case outcome of
                            Left (DuckDBExecutionError msg) | "Duplicate key" `T.isInfixOf` msg -> Right ()
                            _ -> Left (show outcome)
                    _ -> do
                        outcome <- executeSqliteCandidateDetailedTimeout 5000000 sql
                        pure $ case outcome of
                            Left (SqliteExecutionError msg) | "UNIQUE constraint failed: t.x" `T.isInfixOf` msg -> Right ()
                            _ -> Left (show outcome)
            either
                (assertFailure . ((BSC.unpack sql <> "\nExpected a primary key conflict, got ") <>))
                pure
                conflict

nativeSlice ::
    SQLDialect -> [ByteString] -> IO (Set.Set StatementId, Set.Set StatementId -> ByteString)
nativeSlice dialect input = do
    let sql = BSC.unlines input
    parsed <- parseSQL (defaultParseOptions dialect) sql >>= either (assertFailure . show) pure
    let units = parsedUnits parsed
        statements = map unitStatement units
        graph = analyzeStatements (parsedInitialSession parsed) units
        retained = backwardSlice (StatementId (length input)) graph
    assertEqual "the frontend recognizes every fixture statement" (length input) (length statements)
    pure (retained, cutRemovedStatementIds sql statements)

checkNativeSlice ::
    SQLDialect -> [ByteString] -> ByteString -> [Int] -> [(Int, ByteString)] -> IO ()
checkNativeSlice dialect input reference irrelevant omissions = do
    let sql = BSC.unlines input
        micros = 5000000
        prepare expected =
            case dialect of
                DuckDB ->
                    fmap (duckdbContractOracleTimeout micros) <$> generateDuckDBContractTimeout micros expected
                _ ->
                    fmap (sqliteContractOracleTimeout micros) <$> generateSqliteContractTimeout micros expected
        checkResult oracle label candidate = do
            verdict <- oracle candidate
            assertEqual (label <> ": " <> BSC.unpack candidate) OracleAccepted verdict
    oracle <- prepare reference >>= either (assertFailure . show) pure
    (retained, render) <- nativeSlice dialect input
    checkResult oracle "original result" sql
    mapM_
        ( \sid ->
            assertBool
                ("irrelevant statement retained: " <> show sid)
                (Set.notMember (StatementId sid) retained)
        )
        irrelevant
    checkResult oracle "raw backward slice result" (render retained)
    mapM_
        ( \(sid, expected) -> do
            let omitted = StatementId sid
                candidate = render (Set.delete omitted retained)
            assertBool ("necessary statement omitted: " <> show sid) (Set.member omitted retained)
            different <- prepare expected >>= either (assertFailure . show) pure
            checkResult different ("omitting statement " <> show sid <> " changes the result") candidate
            verdict <- oracle candidate
            assertEqual "the original result contract detects the missing dependency" OracleRejected verdict
        )
        omissions

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

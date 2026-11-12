module DependencyChecks (tests) where

import Data.Bifunctor qualified as Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Occam.Core (
    AlterChange (..),
    AlterTable (..),
    BarrierScope (..),
    BeginPolicy (..),
    CatalogOp (..),
    ColumnDef (..),
    ConflictAction (..),
    Constraint (..),
    DataWrite (..),
    Ident (..),
    IndexDef (..),
    IndexRef (..),
    IndexRole (..),
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

indexDefinitions :: [CatalogOp] -> [IndexDef]
indexDefinitions ops = [definition | DefineIndexOp definition <- ops]

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
        , testCase "SET ROLE changes the $user search-path entry" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA alice;"
                        , "CREATE SCHEMA bob;"
                        , "SET search_path TO \"$user\", public;"
                        , "SET ROLE alice;"
                        , "CREATE TABLE t (x INT);"
                        , "SET ROLE bob;"
                        , "CREATE TABLE t (x INT);"
                        , "SET ROLE alice;"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            let readDeps = dependenciesFor (StatementId 9) graph
            assertBool
                "the read resolves to the table created while the role was alice"
                (has (StatementId 5) readDeps && not (has (StatementId 7) readDeps))
        , testCase "RESET ROLE restores current_user from session_user" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA alice;"
                        , "CREATE SCHEMA bob;"
                        , "SET search_path TO \"$user\", public;"
                        , "SET SESSION AUTHORIZATION bob;"
                        , "SET ROLE alice;"
                        , "CREATE TABLE t (x INT);"
                        , "RESET ROLE;"
                        , "CREATE TABLE t (x INT);"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            let readDeps = dependenciesFor (StatementId 9) graph
            assertBool
                "the read resolves to the table created after resetting to the session user"
                (has (StatementId 8) readDeps && not (has (StatementId 6) readDeps))
        , testCase "RESET SESSION AUTHORIZATION restores the authenticated user" $ do
            let options =
                    (defaultParseOptions PostgreSQL)
                        { parseServerConfig =
                            (parseServerConfig (defaultParseOptions PostgreSQL))
                                { serverCurrentUser = Just "carol"
                                }
                        }
                sql =
                    BSC.unlines
                        [ "CREATE SCHEMA carol;"
                        , "CREATE SCHEMA bob;"
                        , "SET search_path TO \"$user\", public;"
                        , "SET SESSION AUTHORIZATION bob;"
                        , "CREATE TABLE t (x INT);"
                        , "RESET SESSION AUTHORIZATION;"
                        , "CREATE TABLE t (x INT);"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyzeWith options sql
            let readDeps = dependenciesFor (StatementId 8) graph
            assertBool
                "the read resolves to the table created by the authenticated user"
                (has (StatementId 7) readDeps && not (has (StatementId 5) readDeps))
        , testCase "quoted current_user preserves identifier case" $ do
            let options =
                    (defaultParseOptions PostgreSQL)
                        { parseServerConfig =
                            (parseServerConfig (defaultParseOptions PostgreSQL))
                                { serverCurrentUser = Just "\"MixedUser\""
                                }
                        }
                sql =
                    BSC.unlines
                        [ "CREATE SCHEMA \"MixedUser\";"
                        , "SET search_path TO \"$user\";"
                        , "CREATE TABLE t (id INT);"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyzeWith options sql
            assertBool
                "the $user entry resolves to the case-preserved authenticated user"
                (has (StatementId 3) (dependenciesFor (StatementId 4) graph))
        , testCase "quoted namespace_prefix preserves identifier case" $ do
            let options =
                    (defaultParseOptions PostgreSQL)
                        { parseServerConfig =
                            (parseServerConfig (defaultParseOptions PostgreSQL))
                                { serverNamespacePrefix = ["\"MixedSchema\""]
                                }
                        }
                sql =
                    BSC.unlines
                        [ "CREATE SCHEMA \"MixedSchema\";"
                        , "CREATE TABLE t (id INT);"
                        , "SELECT * FROM \"MixedSchema\".t;"
                        ]
            graph <- analyzeWith options sql
            assertBool
                "the namespace prefix resolves to the case-preserved schema"
                (has (StatementId 2) (dependenciesFor (StatementId 3) graph))
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
        , testCase "relation renames carry the frontend index placement" $ do
            let oracleSql =
                    BSC.unlines
                        [ "CREATE SCHEMA s;"
                        , "CREATE TABLE s.t (a INT);"
                        , "ALTER TABLE s.t RENAME TO u;"
                        ]
                postgresSql =
                    BSC.unlines
                        [ "CREATE SCHEMA s;"
                        , "CREATE TABLE s.t (a INT);"
                        , "ALTER TABLE s.t RENAME TO u;"
                        ]
            oracleGraph <- analyze Oracle oracleSql
            postgresGraph <- analyze PostgreSQL postgresSql
            assertBool
                "Oracle renames record current-schema index placement"
                ( RenameRelation
                    (ref ["S", "T"])
                    (objectName ["S", "U"])
                    IndexCurrentSchema
                    `elem` renamesFor (StatementId 3) oracleGraph
                )
            assertBool
                "PostgreSQL renames record table-schema index placement"
                ( RenameRelation
                    (ref ["s", "t"])
                    (objectName ["s", "u"])
                    IndexTableSchema
                    `elem` renamesFor (StatementId 3) postgresGraph
                )
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
            assertBool
                "RELEASE is extracted from its statement"
                (TxReleaseSavepoint (Ident "s") `elem` factPostTx (factsFor (StatementId 4) graph))
            assertBool
                "released savepoint writes remain in the catalog"
                (has (StatementId 3) (dependenciesFor (StatementId 6) graph))
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
                ( any
                    (\definition -> indexRef definition == NamedIndex (objectName ["ix"]))
                    (indexDefinitions (opsFor (StatementId 2) graph))
                )
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
        , testCase "column and table renames update trigger body references" $ do
            let columnSql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT);"
                        , "CREATE TABLE log (x INT);"
                        , "CREATE TRIGGER tr AFTER INSERT ON t BEGIN UPDATE log SET x = 1; END;"
                        , "ALTER TABLE log RENAME COLUMN x TO y;"
                        , "INSERT INTO t (a) VALUES (1);"
                        , "SELECT y FROM log;"
                        ]
                tableSql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT);"
                        , "CREATE TABLE log (x INT);"
                        , "CREATE TRIGGER tr AFTER INSERT ON t BEGIN UPDATE log SET x = 1; END;"
                        , "ALTER TABLE log RENAME TO log_new;"
                        , "INSERT INTO t (a) VALUES (1);"
                        , "SELECT x FROM log_new;"
                        ]
            columnGraph <- analyze SQLite columnSql
            tableGraph <- analyze SQLite tableSql
            let columnDeps = dependenciesFor (StatementId 6) columnGraph
                tableDeps = dependenciesFor (StatementId 6) tableGraph
            assertBool
                "the trigger writes the renamed column"
                (has (StatementId 5) columnDeps)
            assertBool
                "the trigger writes the renamed table"
                (has (StatementId 5) tableDeps)
        , testCase "table rename updates foreign-key parents and unresolved view sources" $ do
            let fkSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE);"
                        , "ALTER TABLE parent RENAME TO parent_new;"
                        , "DELETE FROM parent_new WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
                viewSql =
                    BSC.unlines
                        [ "CREATE VIEW v AS SELECT * FROM old_name;"
                        , "CREATE TABLE old_name (x INT);"
                        , "ALTER TABLE old_name RENAME TO new_name;"
                        , "SELECT * FROM v;"
                        ]
            fkGraph <- analyze PostgreSQL fkSql
            viewGraph <- analyze PostgreSQL viewSql
            let childDeps = dependenciesFor (StatementId 5) fkGraph
                viewDeps = dependenciesFor (StatementId 4) viewGraph
            assertBool
                "the renamed parent still cascades to the child"
                (has (StatementId 4) childDeps)
            assertBool
                "the unresolved view source follows the table rename"
                (has (StatementId 2) viewDeps && has (StatementId 3) viewDeps)
        , testCase "PostgreSQL unqualified index names use the table schema" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA s1;"
                        , "CREATE SCHEMA s2;"
                        , "SET search_path TO s1, s2;"
                        , "CREATE TABLE s2.t (id INT);"
                        , "CREATE INDEX idx ON s2.t (id);"
                        , "DROP INDEX s2.idx;"
                        , "SELECT * FROM s2.t;"
                        ]
            graph <- analyze PostgreSQL sql
            let readDeps = dependenciesFor (StatementId 7) graph
            assertBool
                "the index was created in the table schema and dropped by its qualified name"
                (not (has (StatementId 5) readDeps))
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
        , testCase "DROP COLUMN CASCADE keeps views that read other columns" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b INT);"
                        , "CREATE VIEW v AS SELECT b FROM t;"
                        , "ALTER TABLE t DROP COLUMN a CASCADE;"
                        , "SELECT * FROM v;"
                        ]
            graph <- analyze PostgreSQL sql
            let viewDeps = dependenciesFor (StatementId 4) graph
            assertBool
                "the unaffected view survives the column drop"
                (has (StatementId 2) viewDeps)
            assertBool
                "the surviving view still reaches the altered table"
                (has (StatementId 3) viewDeps)
        , testCase "column renames keep view column dependencies current" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b INT);"
                        , "CREATE VIEW v AS SELECT a FROM t;"
                        , "ALTER TABLE t RENAME COLUMN a TO c;"
                        , "ALTER TABLE t DROP COLUMN c CASCADE;"
                        , "SELECT * FROM v;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the view follows the renamed column and is dropped with it"
                (not (has (StatementId 2) (dependenciesFor (StatementId 5) graph)))
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
        , testCase "DROP TABLE CASCADE follows unresolved view sources" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE VIEW v AS SELECT * FROM later;"
                        , "CREATE TABLE later (x INT);"
                        , "DROP TABLE later CASCADE;"
                        , "SELECT * FROM v;"
                        ]
            graph <- analyze PostgreSQL sql
            let viewReadDeps = dependenciesFor (StatementId 4) graph
            assertBool
                "the dependent view is dropped with its newly resolvable source"
                (not (has (StatementId 1) viewReadDeps))
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
        , testCase "DROP COLUMN CASCADE removes cross-table foreign keys but RESTRICT keeps them" $ do
            let cascadeSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE);"
                        , "ALTER TABLE parent DROP COLUMN id CASCADE;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
                restrictSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE);"
                        , "ALTER TABLE parent DROP COLUMN id;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
            cascadeGraph <- analyze GenericSQL cascadeSql
            restrictGraph <- analyze GenericSQL restrictSql
            let cascadeDeps = dependenciesFor (StatementId 5) cascadeGraph
                restrictDeps = dependenciesFor (StatementId 5) restrictGraph
            assertBool
                "child read still depends on its definition"
                (has (StatementId 2) cascadeDeps)
            assertBool
                "removed foreign key no longer cascades the parent delete"
                (not (has (StatementId 4) cascadeDeps))
            assertBool
                "RESTRICT keeps the cross-table foreign key"
                (has (StatementId 4) restrictDeps)
        , testCase "VALIDATE CONSTRAINT depends on current parent data" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT);"
                        , "ALTER TABLE child ADD CONSTRAINT fk FOREIGN KEY (pid) REFERENCES parent(id) NOT VALID;"
                        , "INSERT INTO parent VALUES (1);"
                        , "ALTER TABLE child VALIDATE CONSTRAINT fk;"
                        ]
            graph <- analyze PostgreSQL sql
            let validationDeps = dependenciesFor (StatementId 5) graph
            assertBool
                "validation depends on the parent insert"
                (has (StatementId 4) validationDeps)
            assertBool
                "validation depends on the child definition"
                (has (StatementId 2) validationDeps)
        , testCase "unnamed constraints from separate ALTER changes keep distinct identities" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p1 (id INT PRIMARY KEY);"
                        , "CREATE TABLE p2 (id INT PRIMARY KEY);"
                        , "CREATE TABLE c (x INT);"
                        , "ALTER TABLE c ADD COLUMN a INT REFERENCES p1(id) ON DELETE CASCADE, ADD COLUMN b INT REFERENCES p2(id) ON DELETE CASCADE;"
                        , "DELETE FROM p1 WHERE id = 1;"
                        , "SELECT * FROM c;"
                        ]
            graph <- analyze PostgreSQL sql
            let childDeps = dependenciesFor (StatementId 6) graph
            assertBool
                "both unnamed foreign keys survive and the first still cascades"
                (has (StatementId 5) childDeps)
        , testCase "USING INDEX constraints retain the index columns for column drops" $ do
            -- Keep this as an extraction smoke test; the schema-resolution test
            -- below observes the resolved constraint through column coupling.
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT);"
                        , "CREATE UNIQUE INDEX t_id_idx ON t (id);"
                        , "ALTER TABLE t ADD CONSTRAINT t_pk PRIMARY KEY USING INDEX t_id_idx;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "PRIMARY KEY USING INDEX is extracted as a typed constraint"
                ( case opsFor (StatementId 3) graph of
                    [AlterTableOp alter] ->
                        case alterTableChanges alter of
                            [ AddConstraint
                                    (UsingIndexConstraint (Just (Ident "t_pk")) (Ident "t_id_idx") PrimaryIndex IndexTableSchema)
                                ] -> True
                            _ -> False
                    _ -> False
                )
        , testCase "USING INDEX resolves in the altered table's schema" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA s;"
                        , "SET search_path TO public;"
                        , "CREATE TABLE s.t (a INT, b INT);"
                        , "CREATE UNIQUE INDEX t_ab_idx ON s.t (a, b);"
                        , "ALTER TABLE s.t ADD PRIMARY KEY USING INDEX t_ab_idx;"
                        , "UPDATE s.t SET a = 1;"
                        , "SELECT b FROM s.t;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "a read of the coupled column depends on the update through the schema-local index constraint"
                (has (StatementId 6) (dependenciesFor (StatementId 7) graph))
        , testCase "USING INDEX resolves an Oracle current-schema index" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA s;"
                        , "CREATE TABLE s.t (a INT, b INT);"
                        , "CREATE UNIQUE INDEX idx ON s.t (a, b);"
                        , "ALTER TABLE s.t ADD PRIMARY KEY USING INDEX idx;"
                        , "UPDATE s.t SET a = 1;"
                        , "SELECT b FROM s.t;"
                        ]
            graph <- analyze Oracle sql
            assertBool
                "a read of the coupled column depends on the update through the current-schema index constraint"
                (has (StatementId 5) (dependenciesFor (StatementId 6) graph))
        , testCase "DROP SCHEMA RESTRICT keeps a non-empty schema intact" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA s;"
                        , "CREATE TABLE s.t (x INT);"
                        , "DROP SCHEMA s;"
                        , "CREATE SCHEMA IF NOT EXISTS s;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the rejected schema drop leaves the schema definition reachable"
                (has (StatementId 1) (dependenciesFor (StatementId 4) graph))
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
        , testCase "DROP SCHEMA CASCADE updates unqualified name resolution" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA s;"
                        , "CREATE SCHEMA public;"
                        , "CREATE TABLE s.t (x INT);"
                        , "CREATE TABLE public.t (x INT);"
                        , "SET search_path TO s, public;"
                        , "DROP SCHEMA s CASCADE;"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            let readDeps = dependenciesFor (StatementId 7) graph
            assertBool
                "the read now resolves to public.t"
                (has (StatementId 4) readDeps)
            assertBool
                "the read depends on the schema drop that changed resolution"
                (has (StatementId 6) readDeps)
        , testCase "DROP TABLE CASCADE updates unqualified name resolution" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE SCHEMA s;"
                        , "CREATE SCHEMA public;"
                        , "CREATE TABLE s.t (x INT);"
                        , "CREATE TABLE public.t (x INT);"
                        , "SET search_path TO s, public;"
                        , "DROP TABLE s.t CASCADE;"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            let readDeps = dependenciesFor (StatementId 7) graph
            assertBool
                "the read now resolves to public.t"
                (has (StatementId 4) readDeps)
            assertBool
                "the read depends on the table drop that changed resolution"
                (has (StatementId 6) readDeps)
        , testCase "DROP CONSTRAINT RESTRICT keeps foreign keys that depend on it" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT, CONSTRAINT parent_pk PRIMARY KEY (id));"
                        , "CREATE TABLE child (pid INT);"
                        , "ALTER TABLE child ADD CONSTRAINT child_fk FOREIGN KEY (pid) REFERENCES parent(id);"
                        , "INSERT INTO parent VALUES (1);"
                        , "ALTER TABLE parent DROP CONSTRAINT parent_pk;"
                        , "INSERT INTO child VALUES (1);"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the rejected drop leaves the foreign-key parent check in place"
                (has (StatementId 4) (dependenciesFor (StatementId 6) graph))
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
        , testCase "DROP CONSTRAINT CASCADE keeps foreign keys on other unique columns" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (a INT, b INT, CONSTRAINT parent_a UNIQUE (a), CONSTRAINT parent_b UNIQUE (b));"
                        , "CREATE TABLE child (x INT);"
                        , "ALTER TABLE child ADD CONSTRAINT child_fk FOREIGN KEY (x) REFERENCES parent(a);"
                        , "INSERT INTO parent VALUES (1, 2);"
                        , "ALTER TABLE parent DROP CONSTRAINT parent_b CASCADE;"
                        , "INSERT INTO child VALUES (1);"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the foreign key references parent_a and survives the drop of parent_b"
                (has (StatementId 4) (dependenciesFor (StatementId 6) graph))
        , testCase "DROP TABLE RESTRICT keeps dependents when they still exist" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE base (x INT);"
                        , "CREATE VIEW v AS SELECT * FROM base;"
                        , "DROP TABLE base;"
                        , "SELECT * FROM base;"
                        , "SELECT * FROM v;"
                        ]
            graph <- analyze PostgreSQL sql
            let baseDeps = dependenciesFor (StatementId 4) graph
                viewDeps = dependenciesFor (StatementId 5) graph
            assertBool
                "restricted table drop leaves the table definition reachable"
                (has (StatementId 1) baseDeps)
            assertBool
                "view still reaches the retained base table"
                (has (StatementId 1) viewDeps && has (StatementId 2) viewDeps)
        , testCase "MsSql nested BEGIN counts commits and rolls back the outer transaction" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (x INT);"
                        , "BEGIN;"
                        , "INSERT INTO t VALUES (1);"
                        , "BEGIN;"
                        , "INSERT INTO t VALUES (2);"
                        , "COMMIT;"
                        , "SELECT * FROM t;"
                        , "ROLLBACK;"
                        , "SELECT * FROM t;"
                        ]
            graph <- analyze MsSql sql
            let afterInnerCommit = dependenciesFor (StatementId 7) graph
                afterRollback = dependenciesFor (StatementId 9) graph
            assertBool
                "inner COMMIT does not commit the outer transaction"
                (has (StatementId 3) afterInnerCommit && has (StatementId 5) afterInnerCommit)
            assertBool
                "outer ROLLBACK removes both inserts"
                ( not (has (StatementId 3) afterRollback)
                    && not (has (StatementId 5) afterRollback)
                )
            assertBool
                "table definition remains after rollback"
                (has (StatementId 1) afterRollback)
        , testCase "DROP COLUMN removes self-referential foreign keys" $ do
            let cascadeSql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT PRIMARY KEY, parent_id INT REFERENCES t(id) ON UPDATE CASCADE);"
                        , "ALTER TABLE t DROP COLUMN id CASCADE;"
                        , "UPDATE t SET id = 2;"
                        , "SELECT parent_id FROM t;"
                        ]
                restrictSql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT PRIMARY KEY, parent_id INT REFERENCES t(id) ON UPDATE CASCADE);"
                        , "ALTER TABLE t DROP COLUMN id;"
                        , "UPDATE t SET id = 2;"
                        , "SELECT parent_id FROM t;"
                        ]
            cascadeGraph <- analyze GenericSQL cascadeSql
            restrictGraph <- analyze GenericSQL restrictSql
            let cascadeDeps = dependenciesFor (StatementId 4) cascadeGraph
                restrictDeps = dependenciesFor (StatementId 4) restrictGraph
            assertBool
                "the dropped self-reference no longer cascades onto parent_id"
                (not (has (StatementId 3) cascadeDeps))
            assertBool
                "RESTRICT keeps the self-referential cascade"
                (has (StatementId 3) restrictDeps)
        , testCase "renaming a parent column updates foreign-key parent columns" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id) ON UPDATE CASCADE);"
                        , "ALTER TABLE parent RENAME COLUMN id TO new_id;"
                        , "UPDATE parent SET new_id = 2;"
                        , "SELECT * FROM child;"
                        ]
            graph <- analyze GenericSQL sql
            let childDeps = dependenciesFor (StatementId 5) graph
            assertBool
                "ON UPDATE CASCADE still fires after the parent column rename"
                (has (StatementId 4) childDeps)
        , testCase "renaming a child column leaves foreign-key parent columns alone" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id) ON UPDATE CASCADE);"
                        , "ALTER TABLE child RENAME COLUMN pid TO cid;"
                        , "UPDATE parent SET id = 2;"
                        , "SELECT * FROM child;"
                        ]
            graph <- analyze GenericSQL sql
            let childDeps = dependenciesFor (StatementId 5) graph
            assertBool
                "ON UPDATE CASCADE still fires when only the child column is renamed"
                (has (StatementId 4) childDeps)
        , testCase "RESET foreign_key_checks restores the configured initial value" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id) ON DELETE CASCADE);"
                        , "SET foreign_key_checks = 1;"
                        , "RESET foreign_key_checks;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
                disabledOptions =
                    (defaultParseOptions MySQL)
                        { parseServerConfig =
                            (parseServerConfig (defaultParseOptions MySQL))
                                { serverForeignKeyChecks = Just False
                                }
                        }
            disabledGraph <- analyzeWith disabledOptions sql
            enabledGraph <- analyze MySQL sql
            let disabledDeps = dependenciesFor (StatementId 6) disabledGraph
                enabledDeps = dependenciesFor (StatementId 6) enabledGraph
            assertBool
                "reset restores the initial disabled state, so the delete does not cascade"
                (not (has (StatementId 5) disabledDeps))
            assertBool
                "the default enabled state keeps the cascade after reset"
                (has (StatementId 5) enabledDeps)
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
        , testCase "multi-table UPDATE assigns unqualified columns to the owning table" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT);"
                        , "CREATE TABLE t2 (b INT);"
                        , "UPDATE t1 JOIN t2 ON t1.a = t2.b SET b = 1;"
                        , "SELECT a FROM t1;"
                        , "SELECT b FROM t2;"
                        ]
            graph <- analyze MySQL sql
            assertBool
                "the table without the assigned column is not written"
                (not (has (StatementId 3) (dependenciesFor (StatementId 4) graph)))
            assertBool
                "the table that owns the unqualified column is written"
                (has (StatementId 3) (dependenciesFor (StatementId 5) graph))
        , testCase "foreign keys with omitted parent columns follow the primary key" $ do
            let cascadeSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT REFERENCES parent ON DELETE CASCADE);"
                        , "ALTER TABLE parent DROP COLUMN id CASCADE;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
                restrictSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT REFERENCES parent ON DELETE CASCADE);"
                        , "ALTER TABLE parent DROP COLUMN id;"
                        , "DELETE FROM parent WHERE id = 1;"
                        , "SELECT * FROM child;"
                        ]
            cascadeGraph <- analyze PostgreSQL cascadeSql
            restrictGraph <- analyze PostgreSQL restrictSql
            let cascadeDeps = dependenciesFor (StatementId 5) cascadeGraph
                restrictDeps = dependenciesFor (StatementId 5) restrictGraph
            assertBool
                "CASCADE removes the implicit primary-key reference"
                (not (has (StatementId 4) cascadeDeps))
            assertBool
                "RESTRICT keeps the implicit primary-key reference"
                (has (StatementId 4) restrictDeps)
        , testCase "validated foreign key addition depends on parent data" $ do
            let validatedSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT);"
                        , "INSERT INTO parent VALUES (1);"
                        , "ALTER TABLE child ADD CONSTRAINT fk FOREIGN KEY (pid) REFERENCES parent(id);"
                        ]
                notValidSql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT);"
                        , "INSERT INTO parent VALUES (1);"
                        , "ALTER TABLE child ADD CONSTRAINT fk FOREIGN KEY (pid) REFERENCES parent(id) NOT VALID;"
                        ]
            validatedGraph <- analyze PostgreSQL validatedSql
            notValidGraph <- analyze PostgreSQL notValidSql
            assertBool
                "a validated foreign key scans existing parent rows"
                (has (StatementId 3) (dependenciesFor (StatementId 4) validatedGraph))
            assertBool
                "NOT VALID defers the parent scan"
                (not (has (StatementId 3) (dependenciesFor (StatementId 4) notValidGraph)))
        , testCase "TRUNCATE without CASCADE depends on referencing tables" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (pid INT REFERENCES parent(id));"
                        , "TRUNCATE parent;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the truncate depends on the table that references the parent"
                (has (StatementId 2) (dependenciesFor (StatementId 3) graph))
        , testCase "trigger functions keep their definition dependency" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE FUNCTION f() RETURNS trigger AS $$ BEGIN RETURN NEW; END; $$ LANGUAGE plpgsql;"
                        , "CREATE TABLE t (id INT);"
                        , "CREATE TRIGGER tr AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION f();"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "the trigger definition depends on the function definition"
                (has (StatementId 1) (dependenciesFor (StatementId 3) graph))
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
        , testCase "constraint coupling is transitively closed" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b INT, c INT, UNIQUE (a, b), CHECK (b > c));"
                        , "INSERT INTO t VALUES (1, 2, 3);"
                        , "UPDATE t SET a = 4;"
                        , "SELECT c FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "a read of a column coupled through two constraints depends on the update"
                (has (StatementId 3) (dependenciesFor (StatementId 4) graph))
        , testCase "self-referential foreign keys couple child and parent columns" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT PRIMARY KEY, parent_id INT REFERENCES t(id));"
                        , "INSERT INTO t VALUES (1, NULL);"
                        , "UPDATE t SET id = 2;"
                        , "SELECT parent_id FROM t;"
                        ]
            graph <- analyze PostgreSQL sql
            assertBool
                "a read of the child column depends on the update of the parent column"
                (has (StatementId 3) (dependenciesFor (StatementId 4) graph))
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

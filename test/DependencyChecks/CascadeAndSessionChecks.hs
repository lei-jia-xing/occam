module DependencyChecks.CascadeAndSessionChecks (checks) where

import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import DependencyChecks.Support (
    analyze,
    analyzeParsed,
    analyzeWith,
    dependenciesFor,
    factsFor,
    has,
 )
import Occam.Core (
    BarrierScope (..),
    DataWrite (..),
    DeleteScope (..),
    Ident (..),
    NameRef (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    StatementId (..),
    UpdateScope (..),
    WriteKind (..),
    nameText,
 )
import Occam.Dependency (cascadeWrites)
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    ServerConfig (..),
    defaultParseOptions,
    parseSQL,
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

checks :: [TestTree]
checks =
    [ testCase "cascading foreign keys propagate delete writes to child tables" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                    , "CREATE TABLE child (id INT, p_id INT REFERENCES parent(id) ON DELETE CASCADE);"
                    , "CREATE TABLE unrelated (x INT);"
                    , "DELETE FROM parent WHERE id = 1;"
                    , "SELECT id FROM child;"
                    , "SELECT x FROM unrelated;"
                    ]
        graph <- analyze PostgreSQL sql
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
        graph <- analyze PostgreSQL sql
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
        graph <- analyze PostgreSQL sql
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
            barriers = factBarriers (factsFor (StatementId 3) graph)
        assertBool
            "the user variable does not disable the session cascade"
            (has (StatementId 4) readDeps)
        assertBool
            "the user variable carries UserVariableScope"
            (any (\b -> barrierScope b == UserVariableScope (Ident "@foreign_key_checks")) barriers)
        assertBool
            "the user variable does not chain unrelated tables"
            (not (has (StatementId 1) setDeps) && not (has (StatementId 2) setDeps))
    , testCase "MySQL SET @autocommit stays fenced as a user variable" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (id INT PRIMARY KEY);"
                    , "SET @autocommit = 0;"
                    , "INSERT INTO t VALUES (1);"
                    , "SELECT * FROM t;"
                    , "SELECT @autocommit;"
                    ]
        graph <- analyze MySQL sql
        let setDeps = dependenciesFor (StatementId 2) graph
            readDeps = dependenciesFor (StatementId 4) graph
            varReadDeps = dependenciesFor (StatementId 5) graph
            barriers = factBarriers (factsFor (StatementId 2) graph)
        assertBool
            "the user variable records no session effect"
            (null (factSession (factsFor (StatementId 2) graph)))
        assertBool
            "the user variable carries UserVariableScope"
            (any (\b -> barrierScope b == UserVariableScope (Ident "@autocommit")) barriers)
        assertBool
            "the user variable does not chain unrelated table definitions"
            (not (has (StatementId 1) setDeps) && not (has (StatementId 2) readDeps))
        assertBool
            "ordinary dependencies still resolve"
            (has (StatementId 3) readDeps)
        assertBool
            "later reader of @autocommit depends on SET @autocommit"
            (has (StatementId 2) varReadDeps)
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
                    , "SELECT @search_path;"
                    ]
        graph <- analyze MySQL sql
        let setDeps = dependenciesFor (StatementId 2) graph
            readDeps = dependenciesFor (StatementId 3) graph
            varReadDeps = dependenciesFor (StatementId 4) graph
            barriers = factBarriers (factsFor (StatementId 2) graph)
        assertBool
            "the user variable records no session effect"
            (null (factSession (factsFor (StatementId 2) graph)))
        assertBool
            "the user variable carries UserVariableScope"
            (any (\b -> barrierScope b == UserVariableScope (Ident "@search_path")) barriers)
        assertBool
            "the user variable does not chain unrelated statements"
            (not (has (StatementId 1) setDeps) && not (has (StatementId 2) readDeps))
        assertBool
            "later reader of @search_path depends on SET @search_path"
            (has (StatementId 2) varReadDeps)
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
    ]

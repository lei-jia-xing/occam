module DependencyChecks.ViewAndBarrierChecks (checks) where

import Data.ByteString.Char8 qualified as BSC
import Data.Maybe (isJust)
import DependencyChecks.Support (
    analyze,
    analyzeParsed,
    definedColumns,
    dependenciesFor,
    factsFor,
    has,
    isNamedIndex,
    opsFor,
    readsFor,
    ref,
    tableDefinition,
 )
import Occam.Core (
    BarrierScope (..),
    Ident (..),
    ReadFact (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    StatementId (..),
    TableDef (..),
 )
import Occam.Parser (SQLDialect (..))
import Occam.Slicer (backwardSlice)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

checks :: [TestTree]
checks =
    [ testCase "TABLE query bodies retain external sources without capturing CTE shadows" $
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
            [PostgreSQL]
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
            [ (MySQL, "INSERT INTO src PARTITION (p0) SELECT a FROM sink;")
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
        postgresGraph <- analyze PostgreSQL postgresSql
        assertBool
            "nextval depends on the sequence definition"
            (has (StatementId 1) (dependenciesFor (StatementId 2) postgresGraph))
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
    , testCase "coalesce group projections fence conservatively while keeping reads" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE unrelated (id INT);"
                    , "CREATE TABLE t (a INT, b INT);"
                    , "SELECT a, coalesce(b, 0) FROM t GROUP BY a;"
                    ]
        graph <- analyze PostgreSQL sql
        let deps = dependenciesFor (StatementId 3) graph
        assertBool
            "the fenced projection chains prior statements conservatively"
            (has (StatementId 1) deps && has (StatementId 2) deps)
        assertEqual
            "grouping and argument reads survive the conservative fence"
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
    , testCase "keyword COALESCE fences conservatively while keeping reads" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE unrelated (id INT);"
                    , "CREATE TABLE t (a INT, b INT);"
                    , "SELECT COALESCE(a, b) FROM t;"
                    ]
        graph <- analyze SQLite sql
        let deps = dependenciesFor (StatementId 3) graph
        assertBool
            "the fenced projection chains prior statements conservatively"
            (has (StatementId 1) deps && has (StatementId 2) deps)
        assertEqual
            "argument reads survive the conservative fence"
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
    , testCase "mysql user variable expressions chain without polluting table reads or unrelated tables" $ do
        let sql =
                BSC.unlines
                    [ "SET @f = _utf32 'a';"
                    , "CREATE TABLE unrelated (x INT);"
                    , "CREATE TABLE t1 (a INT);"
                    , "SELECT 1 FROM t1 WHERE @f<>'3761-03-28' XOR (@f:='5570-12-30') > a;"
                    ]
        graph <- analyze MySQL sql
        let queryDeps = dependenciesFor (StatementId 4) graph
        assertBool
            "user variable reader depends on SET @f and table definition"
            (has (StatementId 1) queryDeps && has (StatementId 3) queryDeps)
        assertBool
            "user variable fence does not chain unrelated tables"
            (not (has (StatementId 2) queryDeps))
        assertEqual
            "user variable is excluded from table column reads"
            ["a"]
            [identText column | readFact <- readsFor (StatementId 4) graph, column <- readColumns readFact]
    , testCase "postgresql bare ANALYZE carries a global fence" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (a INT);"
                    , "ANALYZE;"
                    , "SELECT a FROM t;"
                    ]
        graph <- analyze PostgreSQL sql
        let analyzeDeps = dependenciesFor (StatementId 2) graph
            readDeps = dependenciesFor (StatementId 3) graph
            barriers = factBarriers (factsFor (StatementId 2) graph)
        assertBool
            "bare ANALYZE emits GlobalScope barrier"
            (any (\b -> barrierScope b == GlobalScope) barriers)
        assertBool
            "global barrier fences preceding and following statements"
            (has (StatementId 1) analyzeDeps && has (StatementId 2) readDeps)
    , testCase "mysql SET user variable subquery retains referenced table without unrelated fence" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE unrelated (x INT);"
                    , "CREATE TABLE t (a INT);"
                    , "INSERT INTO t VALUES (1);"
                    , "SET @x = (SELECT a FROM t);"
                    , "SELECT @x;"
                    ]
        graph <- analyze MySQL sql
        let setDeps = dependenciesFor (StatementId 4) graph
            readDeps = dependenciesFor (StatementId 5) graph
        assertBool
            "SET @x = (SELECT a FROM t) depends on t DDL and DML"
            (has (StatementId 2) setDeps && has (StatementId 3) setDeps)
        assertBool
            "SET @x = (SELECT a FROM t) does not depend on unrelated table"
            (not (has (StatementId 1) setDeps))
        assertBool
            "SELECT @x depends on SET @x"
            (has (StatementId 4) readDeps)
    ]

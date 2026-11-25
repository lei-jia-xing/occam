module DependencyChecks.PartitionAndDweChecks (checks) where

import Data.ByteString.Char8 qualified as BSC
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import DependencyChecks.SnapshotModelChecks (prop_catalogTransitions)
import DependencyChecks.Support (
    allReadFacts,
    analyze,
    dependenciesFor,
    factsFor,
    has,
    hasWrite,
    isAttach,
    isAttachInherit,
    isDetach,
    isDetachInherit,
    opsFor,
    readSources,
    ref,
    tableDefinition,
    writesFor,
 )
import Occam.Core (
    BarrierScope (..),
    CatalogOp (..),
    ConflictAction (..),
    DataWrite (..),
    Ident (..),
    NameRef (..),
    ReadFact (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    StatementId (..),
    TableDef (..),
    UpdateScope (..),
    WriteKind (..),
    nameText,
 )
import Occam.Dependency (DependencyGraph (..))
import Occam.Parser (SQLDialect (..))
import Occam.Slicer (backwardSlice)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

checks :: [TestTree]
checks =
    [ testCase "partitioned parent reads depend on child definitions and writes" $ do
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
    , testCase "Online transitive reduction keeps O(N) edges while preserving exact backward slices" $ do
        let inserts = ["INSERT INTO t VALUES (" <> BSC.pack (show i) <> ", 10);" | i <- [1 .. 200 :: Int]]
            insertSql =
                BSC.unlines
                    ("CREATE TABLE t (id INT, a INT);" : inserts ++ ["SELECT COUNT(*) FROM t;"])
        insertGraph <- analyze PostgreSQL insertSql
        let countSlice = backwardSlice (StatementId 202) insertGraph
        assertBool
            ("expected O(N) edge count for 200 inserts (< 600), got " ++ show (graphEdgeCount insertGraph))
            (graphEdgeCount insertGraph < 600)
        assertEqual
            "COUNT(*) backward slice retains CREATE TABLE, all 200 INSERTs, and itself"
            (Set.fromList (map StatementId [1 .. 202]))
            countSlice

        let updates =
                [ "UPDATE t SET a = " <> BSC.pack (show i) <> " WHERE id = 1;"
                | i <- [1 .. 200 :: Int]
                ]
            updateSql =
                BSC.unlines
                    ( "CREATE TABLE t (id INT, a INT);"
                        : "INSERT INTO t VALUES (1, 10);"
                        : updates
                        ++ ["SELECT a FROM t;"]
                    )
        updateGraph <- analyze PostgreSQL updateSql
        let readSlice = backwardSlice (StatementId 203) updateGraph
        assertBool
            ( "expected O(N) edge count for 200 filtered updates (< 1000), got "
                ++ show (graphEdgeCount updateGraph)
            )
            (graphEdgeCount updateGraph < 1000)
        assertEqual
            "SELECT a backward slice retains CREATE TABLE, INSERT, all 200 UPDATEs, and itself"
            (Set.fromList (map StatementId [1 .. 203]))
            readSlice
    , testProperty
        "generated SQLite catalog transitions match a table snapshot model"
        prop_catalogTransitions
    ]

module FrontendSemanticsChecks (tests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.IntSet qualified as IntSet
import Occam.Core (
    AlterChange (..),
    AlterTable (..),
    BarrierReason (..),
    BarrierScope (..),
    CatalogOp (..),
    ColumnDef (..),
    ConflictAction (..),
    Constraint (..),
    DataWrite (..),
    DeleteScope (..),
    Ident (..),
    IndexUniqueness (..),
    Name (..),
    NameRef (..),
    ReadFact (..),
    ReadMode (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    SequenceDef (..),
    SequenceWrite (..),
    SessionEffect (..),
    SessionScope (..),
    SessionValue (..),
    SourceColumn (..),
    TableDef (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    TriggerEvent (..),
    TriggerTiming (..),
    TxEffect (..),
    ViewColumnLineage (..),
    ViewDef (..),
    WriteKind (..),
 )
import Occam.Dependency (analyzeStatements)
import Occam.Facts (StatementFacts (..), StatementUnit (..))
import Occam.Frontend.Pipeline (parseFrontend)
import Occam.Parser (
    ParsedSQL (..),
    SQLDialect (..),
    defaultParseOptions,
 )
import Occam.Slicer (backwardSliceFast)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Pure-Haskell frontend semantics"
        [ viewAndLineageTests
        , triggerTests
        , sequenceAndPureTableTests
        , mysqlAndMariaDbTests
        , sqlitePragmaAndVirtualTableTests
        , postgresRulePartitionAndOnlyTests
        , duckdbSemanticsTests
        , clickhouseSemanticsTests
        ]

parseUnits :: SQLDialect -> ByteString -> IO [StatementUnit]
parseUnits dialect sql =
    case parseFrontend (defaultParseOptions dialect) sql of
        Left err -> assertFailure ("parseFrontend failed: " <> show err)
        Right parsed -> pure (parsedUnits parsed)

parseSingleFacts :: SQLDialect -> ByteString -> IO SemanticFacts
parseSingleFacts dialect sql = do
    units <- parseUnits dialect sql
    case units of
        [unit] -> case (unitParseError unit, unitFacts unit) of
            (Just err, _) -> assertFailure ("unexpected parse error: " <> show err)
            (Nothing, Nothing) -> assertFailure "expected unitFacts"
            (Nothing, Just sf) -> pure (factSemantics sf)
        other -> assertFailure ("expected 1 unit, got " <> show (length other))

tableDefs :: SemanticFacts -> [TableDef]
tableDefs facts = [t | DefineTableOp t <- factOps facts]

viewDefs :: SemanticFacts -> [ViewDef]
viewDefs facts = [v | DefineViewOp v <- factOps facts]

triggerDefs :: SemanticFacts -> [TriggerDef]
triggerDefs facts = [t | DefineTriggerOp t <- factOps facts]

readFacts :: SemanticFacts -> [ReadFact]
readFacts facts = [r | ReadOp r <- factOps facts]

writeFacts :: SemanticFacts -> [DataWrite]
writeFacts facts = [w | WriteDataOp w <- factOps facts]

seqWrites :: SemanticFacts -> [SequenceWrite]
seqWrites facts = [sw | WriteSequenceOp sw <- factOps facts]

plainRef :: [Ident] -> NameRef
plainRef parts = NameRef (Name parts) False

-- ---------------------------------------------------------------------------
-- 1. Views & Column Lineage
-- ---------------------------------------------------------------------------

viewAndLineageTests :: TestTree
viewAndLineageTests =
    testGroup
        "Views and column lineage"
        [ testCase "CREATE VIEW records schema reads, projection columns, lineage, and condition columns" do
            facts <-
                parseSingleFacts
                    PostgreSQL
                    "CREATE VIEW v (out_a, out_b) AS SELECT t.a, t.b + 1 FROM t WHERE t.c > 0 ORDER BY t.d;"
            assertEqual "no barriers" [] (factBarriers facts)
            case viewDefs facts of
                [v] -> do
                    assertEqual "view name" (Name [Ident "v"]) (viewName v)
                    assertEqual "not materialized" False (viewMaterialized v)
                    assertEqual "output columns" [Ident "out_a", Ident "out_b"] (viewColumns v)
                    assertEqual "source mode" [ReadSchema] (map readMode (viewSources v))
                    assertEqual
                        "lineage extracted"
                        [ ViewColumnLineage (Ident "out_a") (Just [SourceColumn (plainRef [Ident "t"]) (Ident "a")])
                        , ViewColumnLineage (Ident "out_b") (Just [SourceColumn (plainRef [Ident "t"]) (Ident "b")])
                        ]
                        (viewColumnLineage v)
                    assertEqual
                        "condition columns"
                        [ SourceColumn (plainRef [Ident "t"]) (Ident "c")
                        , SourceColumn (plainRef [Ident "t"]) (Ident "d")
                        ]
                        (viewConditionColumns v)
                other -> assertFailure ("expected 1 view def, got " <> show other)
        , testCase "CREATE MATERIALIZED VIEW reads source data" do
            facts <-
                parseSingleFacts
                    PostgreSQL
                    "CREATE MATERIALIZED VIEW mv AS SELECT id FROM src;"
            assertEqual "no barriers" [] (factBarriers facts)
            case viewDefs facts of
                [v] -> do
                    assertBool "materialized" (viewMaterialized v)
                    assertEqual "source mode" [ReadData] (map readMode (viewSources v))
                other -> assertFailure ("expected 1 matview def, got " <> show other)
        , testCase "CREATE OR REPLACE VIEW sets orReplace flag" do
            facts <-
                parseSingleFacts
                    PostgreSQL
                    "CREATE OR REPLACE VIEW v AS SELECT a FROM t;"
            case viewDefs facts of
                [v] -> assertBool "orReplace" (viewOrReplace v)
                other -> assertFailure ("expected 1 view def, got " <> show other)
        ]

-- ---------------------------------------------------------------------------
-- 2. Triggers
-- ---------------------------------------------------------------------------

triggerTests :: TestTree
triggerTests =
    testGroup
        "Triggers"
        [ testCase "SQLite CREATE TRIGGER analyzes inline body DML and reads" do
            facts <-
                parseSingleFacts
                    SQLite
                    "CREATE TRIGGER tr AFTER INSERT ON src BEGIN INSERT INTO audit VALUES (NEW.id); END;"
            assertEqual "no barriers" [] (factBarriers facts)
            case triggerDefs facts of
                [tr] -> do
                    assertEqual "timing" TriggerAfter (triggerTiming tr)
                    assertEqual "events" [TriggerInsert] (triggerEvents tr)
                    assertEqual "table" (plainRef [Ident "src"]) (triggerTable tr)
                    case triggerBody tr of
                        TriggerInline effs -> assertBool "non-empty inline effects" (not (null effs))
                        other -> assertFailure ("expected TriggerInline, got " <> show other)
                other -> assertFailure ("expected 1 trigger def, got " <> show other)
        , testCase "PostgreSQL CREATE TRIGGER EXECUTE FUNCTION produces TriggerFunction on target table" do
            facts <-
                parseSingleFacts
                    PostgreSQL
                    "CREATE TRIGGER tr BEFORE UPDATE ON t FOR EACH ROW EXECUTE FUNCTION audit_fn();"
            case triggerDefs facts of
                [tr] -> do
                    assertEqual "timing" TriggerBefore (triggerTiming tr)
                    assertEqual "events" [TriggerUpdate []] (triggerEvents tr)
                    assertEqual "function body" (TriggerFunction (Name [Ident "audit_fn"]) []) (triggerBody tr)
                other -> assertFailure ("expected 1 trigger def, got " <> show other)
        , testCase "MySQL trigger with DDL in body emits UnsupportedTriggerBodyBarrier inside inline effects" do
            facts <-
                parseSingleFacts
                    MySQL
                    "CREATE TRIGGER tr AFTER INSERT ON src FOR EACH ROW BEGIN CREATE TABLE tmp (x INT); END;"
            case triggerDefs facts of
                [tr] -> case triggerBody tr of
                    TriggerInline effs ->
                        assertBool
                            "contains UnsupportedTriggerBodyBarrier"
                            (any isUnsupportedTriggerBarrier effs)
                    other -> assertFailure ("expected TriggerInline, got " <> show other)
                other -> assertFailure ("expected 1 trigger def, got " <> show other)
        ]
    where
        isUnsupportedTriggerBarrier = \case
            TriggerBarrier b -> barrierReason b == UnsupportedTriggerBodyBarrier
            _ -> False

-- ---------------------------------------------------------------------------
-- 3. Sequences, Pure-Table Gating, Routine DDL, Deferred Calls
-- ---------------------------------------------------------------------------

sequenceAndPureTableTests :: TestTree
sequenceAndPureTableTests =
    testGroup
        "Sequences, pure-table gating, and deferred calls"
        [ testCase "PostgreSQL pg_catalog.nextval/setval/currval extract non-transactional sequence ops" do
            fNext <- parseSingleFacts PostgreSQL "SELECT pg_catalog.nextval('s');"
            assertEqual "no barriers" [] (factBarriers fNext)
            assertEqual "nextval write" [SequenceWrite (plainRef [Ident "s"]) False] (seqWrites fNext)

            fQuoted <- parseSingleFacts PostgreSQL "SELECT \"pg_catalog\".\"nextval\"('s');"
            assertEqual "quoted nextval no barriers" [] (factBarriers fQuoted)
            assertEqual "quoted nextval write" [SequenceWrite (plainRef [Ident "s"]) False] (seqWrites fQuoted)

            fCurr <- parseSingleFacts PostgreSQL "SELECT pg_catalog.currval('s');"
            assertEqual "currval no barriers" [] (factBarriers fCurr)
            assertEqual "currval read" [ReadFact (plainRef [Ident "s"]) ReadData [] False] (readFacts fCurr)

            fSet <- parseSingleFacts PostgreSQL "SELECT pg_catalog.setval('s', 42, true);"
            assertEqual "setval no barriers" [] (factBarriers fSet)
            assertEqual "setval write" [SequenceWrite (plainRef [Ident "s"]) False] (seqWrites fSet)
        , testCase "unqualified nextval fences conservatively on PostgreSQL" do
            facts <- parseSingleFacts PostgreSQL "SELECT nextval('s');"
            assertEqual "no sequence writes" [] (seqWrites facts)
            assertBool
                "global unknown statement barrier"
                ( any
                    (\b -> barrierScope b == GlobalScope && barrierReason b == UnknownStatementBarrier)
                    (factBarriers facts)
                )
        , testCase "CREATE SEQUENCE is transactional on PostgreSQL and non-transactional on MariaDB" do
            fPg <- parseSingleFacts PostgreSQL "CREATE SEQUENCE s1;"
            assertEqual
                "PG sequence def"
                [DefineSequenceOp (SequenceDef (Name [Ident "s1"]) False True)]
                (factOps fPg)
            fMaria <- parseSingleFacts MariaDB "CREATE SEQUENCE s1;"
            assertEqual
                "MariaDB sequence def"
                [DefineSequenceOp (SequenceDef (Name [Ident "s1"]) False False)]
                (factOps fMaria)
        , testCase "routine DDL disables COUNT(*) pure-table specialization across the script" do
            unitsClean <-
                parseUnits
                    PostgreSQL
                    ( BSC.unlines
                        [ "CREATE TABLE t (a INT);"
                        , "SELECT COUNT(*) FROM t;"
                        ]
                    )
            case map (fmap factSemantics . unitFacts) unitsClean of
                [Just _, Just fClean] -> do
                    assertEqual "COUNT(*) without routine DDL has no barrier" [] (factBarriers fClean)
                    assertEqual
                        "COUNT(*) without routine DDL uses ReadRowCount"
                        [ReadRowCount]
                        (map readMode (readFacts fClean))
                other -> assertFailure ("unexpected unit count: " <> show (length other))

            unitsWithRoutine <-
                parseUnits
                    PostgreSQL
                    ( BSC.unlines
                        [ "CREATE TABLE t (a INT);"
                        , "CREATE AGGREGATE count(int) (SFUNC = int4pl, STYPE = int);"
                        , "SELECT COUNT(*) FROM t;"
                        ]
                    )
            case map (fmap factSemantics . unitFacts) unitsWithRoutine of
                [Just _, Just _, Just fAfter] ->
                    assertBool "COUNT(*) with routine DDL is fenced" (not (null (factBarriers fAfter)))
                other -> assertFailure ("unexpected unit count: " <> show (length other))
        , testCase "DEFAULT CURRENT_TIMESTAMP fences deferred execution on SQLite, PostgreSQL, and MySQL" do
            mapM_
                ( \dialect -> do
                    facts <- parseSingleFacts dialect "CREATE TABLE t (x TIMESTAMP DEFAULT CURRENT_TIMESTAMP);"
                    assertBool
                        ("expected routine barrier for " <> show dialect)
                        ( any
                            (\b -> barrierScope b == GlobalScope && barrierReason b == UnknownStatementBarrier)
                            (factBarriers facts)
                        )
                )
                [SQLite, PostgreSQL, MySQL]
        , testCase "plain EXPLAIN uses ReadSchema without executing; EXPLAIN ANALYZE executes" do
            fPlain <- parseSingleFacts PostgreSQL "EXPLAIN SELECT pg_catalog.nextval('s') FROM t;"
            assertEqual "plain EXPLAIN has no sequence writes" [] (seqWrites fPlain)
            assertEqual "plain EXPLAIN reads schema" [ReadSchema] (map readMode (readFacts fPlain))

            fAnalyze <- parseSingleFacts PostgreSQL "EXPLAIN ANALYZE SELECT pg_catalog.nextval('s') FROM t;"
            assertEqual
                "EXPLAIN ANALYZE executes nextval"
                [SequenceWrite (plainRef [Ident "s"]) False]
                (seqWrites fAnalyze)
        ]

-- ---------------------------------------------------------------------------
-- 4. MySQL & MariaDB Semantics
-- ---------------------------------------------------------------------------

mysqlAndMariaDbTests :: TestTree
mysqlAndMariaDbTests =
    testGroup
        "MySQL and MariaDB semantics"
        [ testCase "user variables are scoped to UserVariableScope and excluded from table reads" do
            facts <- parseSingleFacts MySQL "SELECT @x := a FROM t WHERE b = @y;"
            assertEqual
                "user variable barriers"
                [ ScopedBarrier
                    (UserVariableScope (Ident "@x"))
                    UnsupportedExpressionBarrier
                    (Just "user variable @x")
                , ScopedBarrier
                    (UserVariableScope (Ident "@y"))
                    UnsupportedExpressionBarrier
                    (Just "user variable @y")
                ]
                (factBarriers facts)
            assertEqual
                "no global barriers"
                False
                (any (\b -> barrierScope b == GlobalScope) (factBarriers facts))
            assertEqual "reads only t" [plainRef [Ident "t"]] (map readSource (readFacts facts))
        , testCase "double-quoted SET string literal unquotes cleanly on MySQL" do
            facts <- parseSingleFacts MySQL "SET optimizer_switch=\"semijoin=off\";"
            assertEqual
                "unquoted session text"
                [SetVariable ScopeSession (Name [Ident "optimizer_switch"]) (SessionText "semijoin=off")]
                (factSession facts)
        , testCase "implicit commits fire on permanent DDL and not on CREATE TEMPORARY TABLE" do
            fPerm <- parseSingleFacts MySQL "CREATE TABLE t (a INT);"
            assertEqual "pre commit" [TxCommit False] (factPreTx fPerm)
            assertEqual "post commit" [TxCommit False] (factPostTx fPerm)

            fTemp <- parseSingleFacts MySQL "CREATE TEMPORARY TABLE t (a INT);"
            assertEqual "no pre commit on temp" [] (factPreTx fTemp)
            assertEqual "no post commit on temp" [] (factPostTx fTemp)
        , testCase "MySQL ON DUPLICATE KEY UPDATE lifts VALUES(col) marker and tracks stored reads" do
            facts <-
                parseSingleFacts
                    MySQL
                    "INSERT INTO t (id, v) VALUES (1, 2) ON DUPLICATE KEY UPDATE v = VALUES(v) + old_v;"
            assertEqual "no barriers" [] (factBarriers facts)
            case writeFacts facts of
                [w] -> do
                    assertEqual "target" (plainRef [Ident "t"]) (dataWriteTarget w)
                    assertBool
                        "reads stored column old_v"
                        ( any
                            (\r -> readSource r == plainRef [Ident "t"] && Ident "old_v" `elem` readColumns r)
                            (dataWriteReads w)
                        )
                other -> assertFailure ("expected 1 write, got " <> show other)
        , testCase
            "MySQL and MariaDB multi-table UPDATE writes all assigned targets and records qualified column reads"
            do
                mapM_
                    ( \dialect -> do
                        facts <-
                            parseSingleFacts
                                dialect
                                "UPDATE t1 JOIN t2 ON t1.id = t2.id SET t1.a = 1, t2.b = 2;"
                        assertEqual "no barriers" [] (factBarriers facts)
                        assertEqual
                            "targets"
                            [plainRef [Ident "t1"], plainRef [Ident "t2"]]
                            (map dataWriteTarget (writeFacts facts))
                        case writeFacts facts of
                            w1 : _ ->
                                assertEqual
                                    "qualified join columns recorded"
                                    [[Ident "id"], [Ident "id"]]
                                    (map readColumns (dataWriteReads w1))
                            [] -> assertFailure "expected writes"
                    )
                    [MySQL, MariaDB]
        , testCase "MariaDB/MySQL inline KEY sets notNull+PK and named prefix INDEX lifts routine barrier" do
            fKey <- parseSingleFacts MariaDB "CREATE TABLE t (id INT KEY, a VARCHAR(16), INDEX sk (a(4)));"
            assertEqual "no barriers" [] (factBarriers fKey)
            case tableDefs fKey of
                [t] -> do
                    assertEqual "id is not null" [True, False] (map columnNotNull (tableColumns t))
                    assertEqual
                        "constraints include PK and named index"
                        [ PrimaryKeyConstraint Nothing [Ident "id"]
                        , IndexConstraint (Just (Ident "sk")) [Ident "a"] IndexNonUnique
                        ]
                        (tableConstraints t)
                other -> assertFailure ("expected 1 table def, got " <> show other)
        , testCase "MySQL MODIFY COLUMN AUTO_INCREMENT emits OtherChange to keep column fence" do
            facts <- parseSingleFacts MySQL "ALTER TABLE t MODIFY COLUMN a INT AUTO_INCREMENT;"
            assertEqual "no barriers" [] (factBarriers facts)
            assertEqual
                "OtherChange"
                [ AlterTableOp (AlterTable (plainRef [Ident "t"]) [OtherChange "MODIFY COLUMN a INT AUTO_INCREMENT"])
                ]
                (factOps facts)
        , testCase "MariaDB and MySQL ALTER TABLE ALGORITHM=INPLACE scopes barrier to target relation" do
            mapM_
                ( \dialect -> do
                    facts <- parseSingleFacts dialect "ALTER TABLE t ADD d INT, ALGORITHM=INPLACE;"
                    assertEqual
                        "scoped relation barrier"
                        [ ScopedBarrier (RelationScope (plainRef [Ident "t"])) UnknownAlterBarrier (Just "ALGORITHM=INPLACE")
                        ]
                        (factBarriers facts)
                )
                [MariaDB, MySQL]
            -- End-to-end dependency check: ALGORITHM=INPLACE keeps INSERT on t, drops unrelated u.
            let script =
                    BSC.unlines
                        [ "CREATE TABLE u (x INT);"
                        , "INSERT INTO u VALUES (1);"
                        , "CREATE TABLE t (a BLOB) ENGINE=InnoDB;"
                        , "INSERT INTO t VALUES ('a');"
                        , "ALTER TABLE t ADD d INT, ALGORITHM=INPLACE;"
                        ]
            case parseFrontend (defaultParseOptions MariaDB) script of
                Left err -> assertFailure (show err)
                Right parsed -> do
                    let graph = analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed)
                        deps = backwardSliceFast 5 graph
                    assertBool "retains CREATE TABLE t" (IntSet.member 3 deps)
                    assertBool "retains INSERT INTO t" (IntSet.member 4 deps)
                    assertBool "isolates unrelated CREATE TABLE u" (not (IntSet.member 1 deps))
                    assertBool "isolates unrelated INSERT INTO u" (not (IntSet.member 2 deps))
        ]

-- ---------------------------------------------------------------------------
-- 5. SQLite PRAGMAs & Virtual Tables
-- ---------------------------------------------------------------------------

sqlitePragmaAndVirtualTableTests :: TestTree
sqlitePragmaAndVirtualTableTests =
    testGroup
        "SQLite PRAGMAs and virtual tables"
        [ testCase "PRAGMA foreign_keys handles boolean spellings and bare reads" do
            fRead <- parseSingleFacts SQLite "PRAGMA foreign_keys;"
            assertEqual "bare read has no session effect" [] (factSession fRead)
            assertEqual "bare read has no barriers" [] (factBarriers fRead)

            fOn <- parseSingleFacts SQLite "PRAGMA foreign_keys = ON;"
            assertEqual "ON sets true" [SetSqliteForeignKeys True] (factSession fOn)
            assertEqual "ON no barriers" [] (factBarriers fOn)

            fOff <- parseSingleFacts SQLite "PRAGMA main.foreign_keys(Off);"
            assertEqual "Off sets false" [SetSqliteForeignKeys False] (factSession fOff)

            fIgnore <- parseSingleFacts SQLite "PRAGMA foreign_key_checks = 1;"
            assertEqual "foreign_key_checks ignored on SQLite" [] (factSession fIgnore)
            assertEqual "foreign_key_checks no barriers" [] (factBarriers fIgnore)
        , testCase "inert storage PRAGMAs record without fence; effectful PRAGMAs stay fenced" do
            fInert <- parseSingleFacts SQLite "PRAGMA journal_mode = 'wal';"
            assertEqual
                "journal_mode recorded"
                [SetVariable ScopeSession (Name [Ident "journal_mode"]) (SessionText "wal")]
                (factSession fInert)
            assertEqual "no barrier" [] (factBarriers fInert)

            fEff <- parseSingleFacts SQLite "PRAGMA recursive_triggers = 1;"
            assertBool
                "recursive_triggers assignment fenced"
                ( any
                    (\b -> barrierScope b == GlobalScope && barrierReason b == UnknownSessionEffectBarrier)
                    (factBarriers fEff)
                )
        , testCase
            "CREATE VIRTUAL TABLE emits FTS3/FTS5 shadow tables linked via partitionParent and isolates unrelated tables"
            do
                fFts3 <- parseSingleFacts SQLite "CREATE VIRTUAL TABLE f USING fts3(a,b);"
                let names = map tableName (tableDefs fFts3)
                assertEqual
                    "fts3 defines main + 5 shadow tables"
                    [ Name [Ident "f"]
                    , Name [Ident "f_content"]
                    , Name [Ident "f_segments"]
                    , Name [Ident "f_segdir"]
                    , Name [Ident "f_docsize"]
                    , Name [Ident "f_stat"]
                    ]
                    names
                assertEqual
                    "shadow tables link to parent f"
                    (replicate 5 (Just (plainRef [Ident "f"])))
                    (map tablePartitionParent (drop 1 (tableDefs fFts3)))
                assertEqual
                    "barrier scoped to relation f"
                    [ ScopedBarrier
                        (RelationScope (plainRef [Ident "f"]))
                        UnknownStatementBarrier
                        (Just "CREATE VIRTUAL TABLE f USING fts3(a,b)")
                    ]
                    (factBarriers fFts3)

                fDbstat <- parseSingleFacts SQLite "CREATE VIRTUAL TABLE vt_no_args USING dbstat;"
                assertEqual
                    "dbstat has no shadow tables"
                    [Name [Ident "vt_no_args"]]
                    (map tableName (tableDefs fDbstat))
        , testCase
            "FTS3 shadow-table dependency slice links INSERT INTO f_segdir to CREATE VIRTUAL TABLE f and INSERT INTO f"
            do
                let script =
                        BSC.unlines
                            [ "CREATE VIRTUAL TABLE other USING fts5(z);"
                            , "INSERT INTO other VALUES ('ignored');"
                            , "CREATE VIRTUAL TABLE f USING fts3(a,b);"
                            , "INSERT INTO f_segdir VALUES (28,0,0,0,'0 0',x'00');"
                            , "INSERT INTO f_segdir VALUES (0,241,0,0,'0 0',x'0001000030310000f1');"
                            , "INSERT INTO f VALUES (0,x'00');"
                            ]
                case parseFrontend (defaultParseOptions SQLite) script of
                    Left err -> assertFailure (show err)
                    Right parsed -> do
                        let graph = analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed)
                            deps = backwardSliceFast 6 graph
                        assertBool "retains CREATE VIRTUAL TABLE f" (IntSet.member 3 deps)
                        assertBool "retains first INSERT INTO f_segdir" (IntSet.member 4 deps)
                        assertBool "retains second INSERT INTO f_segdir" (IntSet.member 5 deps)
                        assertBool "excludes unrelated virtual table other" (not (IntSet.member 1 deps))
                        assertBool "excludes unrelated insert into other" (not (IntSet.member 2 deps))
        ]

-- ---------------------------------------------------------------------------
-- 6. PostgreSQL Rules, Partitioning, ONLY, Writable CTEs, DEFAULT, Constraints
-- ---------------------------------------------------------------------------

postgresRulePartitionAndOnlyTests :: TestTree
postgresRulePartitionAndOnlyTests =
    testGroup
        "PostgreSQL rules, partitioning, ONLY, writable CTEs, and constraints"
        [ testCase "CREATE OR REPLACE RULE scopes barrier to target table and isolates unrelated tables" do
            facts <-
                parseSingleFacts
                    PostgreSQL
                    "CREATE OR REPLACE RULE v1 AS ON INSERT TO v0 DO INSTEAD NOTIFY COMPRESSION;"
            assertEqual
                "schema read on v0"
                [ReadFact (plainRef [Ident "v0"]) ReadSchema [] False]
                (readFacts facts)
            assertEqual
                "relation-scoped barrier on v0"
                [ScopedBarrier (RelationScope (plainRef [Ident "v0"])) UnknownDDLBarrier (Just "CREATE RULE on v0")]
                (factBarriers facts)

            let script =
                    BSC.unlines
                        [ "CREATE TABLE unrelated (x INT);"
                        , "INSERT INTO unrelated VALUES (1);"
                        , "CREATE TABLE v0 (v4 INT, v3 INT UNIQUE);"
                        , "CREATE OR REPLACE RULE v1 AS ON INSERT TO v0 DO INSTEAD NOTIFY COMPRESSION;"
                        , "WITH v2 AS (INSERT INTO v0 VALUES (0, 1)) DELETE FROM v0 WHERE v3 = 1;"
                        ]
            case parseFrontend (defaultParseOptions PostgreSQL) script of
                Left err -> assertFailure (show err)
                Right parsed -> do
                    let graph = analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed)
                        deps = backwardSliceFast 5 graph
                    assertBool "retains CREATE TABLE v0" (IntSet.member 3 deps)
                    assertBool "retains CREATE OR REPLACE RULE v1" (IntSet.member 4 deps)
                    assertBool "excludes unrelated table" (not (IntSet.member 1 deps))
                    assertBool "excludes unrelated insert" (not (IntSet.member 2 deps))
        , testCase "CREATE RULE with cross-table DML action scopes barriers to both target and action table" do
            facts <-
                parseSingleFacts
                    PostgreSQL
                    "CREATE RULE r AS ON INSERT TO t1 DO ALSO INSERT INTO t2 VALUES (1);"
            let scopes = map barrierScope (factBarriers facts)
            assertBool "scopes t1" (RelationScope (plainRef [Ident "t1"]) `elem` scopes)
            assertBool "scopes t2" (RelationScope (plainRef [Ident "t2"]) `elem` scopes)
            assertBool "no global barrier" (GlobalScope `notElem` scopes)
        , testCase "ONLY modifier sets readOnly and dataWriteOnly flags" do
            fSel <- parseSingleFacts PostgreSQL "SELECT a FROM ONLY t;"
            assertEqual "SELECT ONLY" [True] (map readOnly (readFacts fSel))

            fUpd <- parseSingleFacts PostgreSQL "UPDATE ONLY t SET a = 1;"
            assertEqual "UPDATE ONLY" [True] (map dataWriteOnly (writeFacts fUpd))

            fDel <- parseSingleFacts PostgreSQL "DELETE FROM ONLY t WHERE a = 1;"
            assertEqual "DELETE ONLY" [True] (map dataWriteOnly (writeFacts fDel))
        , testCase "writable CTE WITH ... DELETE emits CTE ops before outer DELETE" do
            facts <-
                parseSingleFacts
                    PostgreSQL
                    "WITH v2 AS (INSERT INTO v0 VALUES (0)) DELETE FROM v0 WHERE v3 = 1;"
            assertEqual
                "op order"
                [InsertWrite ConflictError, DeleteWrite DeleteFilteredRows]
                (map dataWriteKind (writeFacts facts))
        , testCase "UPDATE SET col = DEFAULT does not emit phantom default column read" do
            facts <- parseSingleFacts PostgreSQL "UPDATE t0 SET c0 = DEFAULT WHERE c1 > 0;"
            case writeFacts facts of
                [w] ->
                    assertBool
                        "no phantom default column"
                        (all (\r -> Ident "default" `notElem` readColumns r) (dataWriteReads w))
                other -> assertFailure ("expected 1 write, got " <> show other)
        , testCase "inline CONSTRAINT name PRIMARY KEY preserves constraint name" do
            facts <-
                parseSingleFacts
                    PostgreSQL
                    "CREATE TABLE t (id INT CONSTRAINT t_pk PRIMARY KEY, v TEXT NOT NULL);"
            case tableDefs facts of
                [t] ->
                    assertEqual
                        "named PK constraint"
                        [PrimaryKeyConstraint (Just (Ident "t_pk")) [Ident "id"]]
                        (tableConstraints t)
                other -> assertFailure ("expected 1 table def, got " <> show other)
        , testCase "GROUPING SETS with nested ROLLUP/CUBE emits no spurious routine barrier" do
            facts <-
                parseSingleFacts
                    PostgreSQL
                    "SELECT v1 FROM v0 GROUP BY ROLLUP (v1), GROUPING SETS (ROLLUP (v1), CUBE (v1));"
            assertEqual "no barriers" [] (factBarriers facts)
        ]

-- ---------------------------------------------------------------------------
-- 7. DuckDB Semantics
-- ---------------------------------------------------------------------------

duckdbSemanticsTests :: TestTree
duckdbSemanticsTests =
    testGroup
        "DuckDB semantics"
        [ testCase "file table functions extract file paths as ReadOp without barriers" do
            f1 <- parseSingleFacts DuckDB "SELECT * FROM read_parquet('data.parquet') AS p;"
            assertEqual "no barriers" [] (factBarriers f1)
            assertEqual "single file" [plainRef [Ident "data.parquet"]] (map readSource (readFacts f1))

            f2 <- parseSingleFacts DuckDB "SELECT * FROM read_csv(['f1.csv', 'f2.csv']);"
            assertEqual "no barriers" [] (factBarriers f2)
            assertEqual
                "multiple files"
                [plainRef [Ident "f1.csv"], plainRef [Ident "f2.csv"]]
                (map readSource (readFacts f2))

            f3 <- parseSingleFacts DuckDB "SELECT * FROM read_csv('data.csv', delim = '|', nullstr = 'NA');"
            assertEqual "no barriers" [] (factBarriers f3)
            assertEqual "named options file" [plainRef [Ident "data.csv"]] (map readSource (readFacts f3))
        , testCase "GROUP BY ALL does not emit a phantom column named all" do
            facts <- parseSingleFacts DuckDB "SELECT a FROM t GROUP BY ALL;"
            assertEqual "no barriers" [] (factBarriers facts)
            assertEqual
                "only column a"
                [ReadFact (plainRef [Ident "t"]) ReadData [Ident "a"] False]
                (readFacts facts)
        , testCase "USE catalog.schema emits two-part SetNamespace" do
            facts <- parseSingleFacts DuckDB "USE cat.sch;"
            assertEqual "no barriers" [] (factBarriers facts)
            assertEqual
                "SetNamespace"
                [SetNamespace ScopeSession [Ident "cat", Ident "sch"]]
                (factSession facts)
        ]

-- ---------------------------------------------------------------------------
-- 8. ClickHouse Semantics
-- ---------------------------------------------------------------------------

clickhouseSemanticsTests :: TestTree
clickhouseSemanticsTests =
    testGroup
        "ClickHouse semantics"
        [ testCase "OPTIMIZE TABLE ... FINAL and PARTITION ... FINAL scope barrier to target table" do
            f1 <- parseSingleFacts ClickHouse "OPTIMIZE TABLE dst FINAL;"
            assertEqual "reads dst" [ReadFact (plainRef [Ident "dst"]) ReadData [] False] (readFacts f1)
            assertEqual
                "relation-scoped barrier"
                [ScopedBarrier (RelationScope (plainRef [Ident "dst"])) UnknownAlterBarrier (Just "OPTIMIZE TABLE")]
                (factBarriers f1)

            f2 <- parseSingleFacts ClickHouse "OPTIMIZE TABLE dst PARTITION '2020' FINAL;"
            assertEqual
                "partition optimize reads dst"
                [ReadFact (plainRef [Ident "dst"]) ReadData [] False]
                (readFacts f2)
            assertEqual
                "partition optimize scoped barrier"
                [ScopedBarrier (RelationScope (plainRef [Ident "dst"])) UnknownAlterBarrier (Just "OPTIMIZE TABLE")]
                (factBarriers f2)
        , testCase "ARRAY JOIN reads array column from source table instead of phantom table" do
            facts <-
                parseSingleFacts
                    ClickHouse
                    "SELECT id, tag, score FROM events ARRAY JOIN tags AS tag PREWHERE score > 0 WHERE id > 0;"
            assertEqual "no barriers" [] (factBarriers facts)
            assertEqual "single source table" [plainRef [Ident "events"]] (map readSource (readFacts facts))
            case readFacts facts of
                [r] -> assertBool "tags recorded as column" (Ident "tags" `elem` readColumns r)
                other -> assertFailure ("expected 1 read fact, got " <> show other)
        , testCase "ORDER BY tuple() and two-argument CAST(x, 'Type') emit no spurious routine barriers" do
            f1 <-
                parseSingleFacts
                    ClickHouse
                    "CREATE TABLE t (k UInt64, x Nullable(UInt8)) ENGINE = MergeTree ORDER BY (k, CAST(x, 'UInt8'));"
            assertEqual "no barriers on 2-arg CAST in ORDER BY" [] (factBarriers f1)

            f2 <-
                parseSingleFacts
                    ClickHouse
                    "CREATE TABLE t (a Int32) ENGINE = MergeTree ORDER BY tuple();"
            assertEqual "no barriers on ORDER BY tuple()" [] (factBarriers f2)
        ]

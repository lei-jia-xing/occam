{-# LANGUAGE OverloadedStrings #-}

-- | Six-engine live semantic alignment & backward-slice equivalence checks.
module SemanticAlignmentChecks (tests) where

import Data.Aeson (Value (..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import ExtraEngineOracles (
    ExtraOracles (..),
    MysqlServer,
    clickhouseLocalQuery,
    mysqlClientExecIsolated,
 )
import Occam.Core (
    BeginPolicy (..),
    CatalogOp (..),
    Ident (..),
    RenameOp (..),
    SemanticFacts (..),
    SequenceWrite (..),
    Statement (stmtRaw),
    UpdateScope (..),
    WriteKind (..),
    dataWriteKind,
    dataWriteUnqualifiedColumns,
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Facts (StatementFacts (..), StatementUnit (..))
import Occam.Frontend.Pipeline (parseFrontend)
import Occam.Oracle.DuckDB (executeDuckDBCandidate)
import Occam.Oracle.Sqlite (executeSqliteCandidate)
import Occam.Parser (
    ParsedSQL (..),
    SQLDialect (..),
    SemanticsProfile (..),
    defaultParseOptions,
    dialectProfile,
    dialectSemantics,
 )
import Occam.Slicer (backwardSliceFast)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: ExtraOracles -> TestTree
tests extraOracles =
    testGroup
        "Multi-engine semantic alignment"
        ( [ sqliteSemanticTests
          , duckdbSemanticTests
          , postgresSemanticTests
          ]
            ++ [mysqlSemanticTests srv | Just srv <- [oracleMySQL extraOracles]]
            ++ [mariadbSemanticTests srv | Just srv <- [oracleMariaDB extraOracles]]
            ++ [clickhouseSemanticTests | oracleClickHouse extraOracles]
        )

-- ---------------------------------------------------------------------------
-- Shared slice & inspection helpers
-- ---------------------------------------------------------------------------

parseAndSlice ::
    SQLDialect ->
    ByteString ->
    Int ->
    IO (ParsedSQL, DependencyGraph, IntSet.IntSet, ByteString)
parseAndSlice dialect sql rootIdx =
    case parseFrontend (defaultParseOptions dialect) sql of
        Left err -> assertFailure ("parseFrontend failed: " <> show err)
        Right parsed ->
            let units = parsedUnits parsed
                graph = analyzeStatements (parsedInitialSession parsed) units
                deps = backwardSliceFast rootIdx graph
                kept =
                    [ stmtRaw (unitStatement u)
                    | (idx, u) <- zip [1 ..] units
                    , IntSet.member idx deps
                    ]
                sliceSql = BSC.unlines (map (<> ";") kept)
             in pure (parsed, graph, deps, sliceSql)

singleUnitFacts :: ParsedSQL -> Int -> IO SemanticFacts
singleUnitFacts parsed idx =
    case drop (idx - 1) (parsedUnits parsed) of
        u : _ -> case unitFacts u of
            Just sf -> pure (factSemantics sf)
            Nothing -> assertFailure ("unit #" <> show idx <> " had no SemanticFacts")
        [] -> assertFailure ("missing unit #" <> show idx)

execSqliteOrFail :: ByteString -> IO [[Value]]
execSqliteOrFail sql = do
    res <- executeSqliteCandidate sql
    case res of
        Left err -> assertFailure ("SQLite execution failed: " <> show err <> "\nSQL:\n" <> BSC.unpack sql)
        Right rows -> pure rows

execDuckOrFail :: ByteString -> IO [[Value]]
execDuckOrFail sql = do
    res <- executeDuckDBCandidate sql
    case res of
        Left err -> assertFailure ("DuckDB execution failed: " <> show err <> "\nSQL:\n" <> BSC.unpack sql)
        Right rows -> pure rows

execMysqlOrFail :: String -> MysqlServer -> [ByteString] -> ByteString -> IO T.Text
execMysqlOrFail label srv extraDbs sql = do
    res <- mysqlClientExecIsolated srv extraDbs sql
    case res of
        Left err ->
            assertFailure (label <> " execution failed: " <> T.unpack err <> "\nSQL:\n" <> BSC.unpack sql)
        Right out -> pure (T.strip out)

execClickHouseOrFail :: ByteString -> IO T.Text
execClickHouseOrFail sql = do
    res <- clickhouseLocalQuery sql
    case res of
        Left err ->
            assertFailure ("clickhouse local failed: " <> T.unpack err <> "\nSQL:\n" <> BSC.unpack sql)
        Right out -> pure (T.strip out)

-- ---------------------------------------------------------------------------
-- 1. SQLite 3 in-process live semantic alignment
-- ---------------------------------------------------------------------------

sqliteSemanticTests :: TestTree
sqliteSemanticTests =
    testGroup
        "SQLite 3 live semantic alignment"
        [ testCase "permanent CREATE VIEW ignores TEMP TABLE shadow and slice executes identically" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE s (a INT);"
                        , "INSERT INTO s VALUES (10);"
                        , "CREATE TEMP TABLE s (a INT);"
                        , "INSERT INTO s VALUES (20);"
                        , "CREATE TABLE noise (z INT);"
                        , "CREATE VIEW v AS SELECT a FROM s;"
                        , "SELECT a FROM v;"
                        ]
            fullRows <- execSqliteOrFail sql
            assertEqual "live SQLite permanent view reads permanent s" [[Number 10]] fullRows
            (_, _, deps, sliceSql) <- parseAndSlice SQLite sql 7
            assertBool "retains CREATE TABLE s" (IntSet.member 1 deps)
            assertBool "retains INSERT INTO s VALUES (10)" (IntSet.member 2 deps)
            assertBool "excludes CREATE TEMP TABLE s" (not (IntSet.member 3 deps))
            assertBool "excludes INSERT INTO TEMP s" (not (IntSet.member 4 deps))
            assertBool "excludes unrelated noise" (not (IntSet.member 5 deps))
            assertBool "retains CREATE VIEW v" (IntSet.member 6 deps)
            sliceRows <- execSqliteOrFail sliceSql
            assertEqual "slice produces identical live SQLite result" fullRows sliceRows
        , testCase "DROP TABLE removes TEMP shadow first and keeps permanent table intact" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE s (a INT);"
                        , "INSERT INTO s VALUES (10);"
                        , "CREATE TEMP TABLE s (a INT);"
                        , "INSERT INTO s VALUES (20);"
                        , "CREATE TABLE noise (z INT);"
                        , "DROP TABLE s;"
                        , "SELECT a FROM s;"
                        ]
            fullRows <- execSqliteOrFail sql
            assertEqual "live SQLite keeps permanent s after DROP TABLE s" [[Number 10]] fullRows
            (_, _, deps, sliceSql) <- parseAndSlice SQLite sql 7
            assertBool "excludes unrelated noise" (not (IntSet.member 5 deps))
            sliceRows <- execSqliteOrFail sliceSql
            assertEqual "slice matches live SQLite result" fullRows sliceRows
        , testCase "PRAGMA foreign_keys = ON inside transaction is a no-op" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE p (id INT PRIMARY KEY);"
                        , "INSERT INTO p VALUES (1);"
                        , "CREATE TABLE ch (id INT, pid INT REFERENCES p(id));"
                        , "INSERT INTO ch VALUES (1, 1);"
                        , "CREATE TABLE noise (z INT);"
                        , "BEGIN;"
                        , "PRAGMA foreign_keys = ON;"
                        , "DELETE FROM p WHERE id = 1;"
                        , "COMMIT;"
                        , "SELECT COUNT(*) FROM p;"
                        ]
            fullRows <- execSqliteOrFail sql
            assertEqual
                "live SQLite deletes parent because tx PRAGMA foreign_keys is ignored"
                [[Number 0]]
                fullRows
            (_, _, deps, sliceSql) <- parseAndSlice SQLite sql 10
            assertBool "excludes noise" (not (IntSet.member 5 deps))
            sliceRows <- execSqliteOrFail sliceSql
            assertEqual "slice matches live SQLite result" fullRows sliceRows
        , testCase "semSequenceTransactional is False and semBeginPolicy is BeginError" $ do
            let sem = dialectSemantics (dialectProfile SQLite)
            assertEqual "SQLite sequence transactional" False (semSequenceTransactional sem)
            assertEqual "SQLite BeginPolicy" BeginError (semBeginPolicy sem)
        ]

-- ---------------------------------------------------------------------------
-- 2. DuckDB 1.5.5 in-process live semantic alignment
-- ---------------------------------------------------------------------------

duckdbSemanticTests :: TestTree
duckdbSemanticTests =
    testGroup
        "DuckDB 1.5.5 live semantic alignment"
        [ testCase "nextval is non-transactional across ROLLBACK without GlobalScope fence" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE noise (z INT);"
                        , "CREATE SEQUENCE seq_d;"
                        , "BEGIN;"
                        , "SELECT nextval('seq_d');"
                        , "ROLLBACK;"
                        , "SELECT nextval('seq_d');"
                        ]
            fullRows <- execDuckOrFail sql
            assertEqual "live DuckDB advances sequence across ROLLBACK" [[Number 2]] fullRows
            (parsed, graph, deps, sliceSql) <- parseAndSlice DuckDB sql 6
            assertBool "no GlobalScope fence on DuckDB nextval" (not (graphHasGlobalBarrier graph))
            seqFacts <- singleUnitFacts parsed 4
            assertEqual
                "nextval recorded as non-transactional SequenceWrite"
                [ False
                ]
                [sequenceWriteTransactional sw | WriteSequenceOp sw <- factOps seqFacts]
            assertBool "retains CREATE SEQUENCE seq_d" (IntSet.member 2 deps)
            assertBool "retains first nextval" (IntSet.member 4 deps)
            assertBool "excludes unrelated noise" (not (IntSet.member 1 deps))
            sliceRows <- execDuckOrFail sliceSql
            assertEqual "slice reproduces exact DuckDB sequence state" fullRows sliceRows
        , testCase "PIVOT isolates unrelated tables with zero barriers and identical slice result" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE noise (z INT);"
                        , "INSERT INTO noise VALUES (99);"
                        , "CREATE TABLE sales (yr INT, amt INT);"
                        , "INSERT INTO sales VALUES (2025, 42);"
                        , "PIVOT sales ON yr USING count(*);"
                        ]
            fullRows <- execDuckOrFail sql
            (_, graph, deps, sliceSql) <- parseAndSlice DuckDB sql 5
            assertBool "no GlobalScope fence on PIVOT" (not (graphHasGlobalBarrier graph))
            assertEqual "zero barrier statements" 0 (graphBarrierStatements graph)
            assertBool "retains sales DDL" (IntSet.member 3 deps)
            assertBool "retains sales INSERT" (IntSet.member 4 deps)
            assertBool "excludes noise DDL" (not (IntSet.member 1 deps))
            assertBool "excludes noise INSERT" (not (IntSet.member 2 deps))
            sliceRows <- execDuckOrFail sliceSql
            assertEqual "slice produces identical PIVOT result" fullRows sliceRows
        , testCase "CREATE VIEW resolves shadowing TEMP table dynamically and falls back after DROP" $ do
            let sqlTemp =
                    BSC.unlines
                        [ "CREATE TABLE s (a INT);"
                        , "INSERT INTO s VALUES (10);"
                        , "CREATE TEMP TABLE s (a INT);"
                        , "INSERT INTO s VALUES (20);"
                        , "CREATE TABLE noise (z INT);"
                        , "CREATE VIEW v AS SELECT a FROM s;"
                        , "SELECT a FROM v;"
                        ]
                sqlDrop =
                    BSC.unlines
                        [ "CREATE TABLE s (a INT);"
                        , "INSERT INTO s VALUES (10);"
                        , "CREATE TEMP TABLE s (a INT);"
                        , "INSERT INTO s VALUES (20);"
                        , "CREATE TABLE noise (z INT);"
                        , "CREATE VIEW v AS SELECT a FROM s;"
                        , "DROP TABLE s;"
                        , "SELECT a FROM v;"
                        ]
            rTemp <- execDuckOrFail sqlTemp
            assertEqual "DuckDB view sees TEMP shadow first" [[Number 20]] rTemp
            (_, _, depsT, sliceTemp) <- parseAndSlice DuckDB sqlTemp 7
            assertBool "view slice keeps TEMP table when shadowing" (IntSet.member 3 depsT)
            assertBool "view slice excludes noise" (not (IntSet.member 5 depsT))
            sliceTempRows <- execDuckOrFail sliceTemp
            assertEqual "view slice reproduces TEMP shadow value" rTemp sliceTempRows

            rDrop <- execDuckOrFail sqlDrop
            assertEqual "DuckDB view falls back to permanent s after DROP TABLE s" [[Number 10]] rDrop
            (_, _, depsD, sliceDrop) <- parseAndSlice DuckDB sqlDrop 8
            assertBool "post-drop view slice excludes noise" (not (IntSet.member 5 depsD))
            sliceDropRows <- execDuckOrFail sliceDrop
            assertEqual "post-drop slice reproduces permanent value" rDrop sliceDropRows
        , testCase "DuckDB semBeginPolicy matches nested BEGIN error behavior" $ do
            let sem = dialectSemantics (dialectProfile DuckDB)
            assertEqual "DuckDB BeginPolicy" BeginError (semBeginPolicy sem)
            assertEqual "DuckDB sequence transactional" False (semSequenceTransactional sem)
        ]

-- ---------------------------------------------------------------------------
-- 3. PostgreSQL 18 semantic policy & slice alignment
-- ---------------------------------------------------------------------------

postgresSemanticTests :: TestTree
postgresSemanticTests =
    testGroup
        "PostgreSQL 18 semantic alignment"
        [ testCase "pg_catalog.nextval is non-transactional across ROLLBACK without GlobalScope" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE noise (z INT);"
                        , "CREATE SEQUENCE seq_pg;"
                        , "BEGIN;"
                        , "SELECT pg_catalog.nextval('seq_pg');"
                        , "ROLLBACK;"
                        , "SELECT pg_catalog.nextval('seq_pg');"
                        ]
            (_, graph, deps, _) <- parseAndSlice PostgreSQL sql 6
            assertBool "no GlobalScope fence" (not (graphHasGlobalBarrier graph))
            assertBool "retains CREATE SEQUENCE" (IntSet.member 2 deps)
            assertBool "retains rolled-back nextval" (IntSet.member 4 deps)
            assertBool "excludes noise" (not (IntSet.member 1 deps))
        , testCase "pg_temp persists across SET search_path changes" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TEMP TABLE t_pg (a INT);"
                        , "INSERT INTO t_pg VALUES (42);"
                        , "CREATE SCHEMA s2;"
                        , "SET search_path TO s2;"
                        , "SELECT a FROM t_pg;"
                        ]
            (_, _, deps, _) <- parseAndSlice PostgreSQL sql 5
            assertBool "retains CREATE TEMP TABLE t_pg across search_path change" (IntSet.member 1 deps)
            assertBool "retains INSERT INTO t_pg across search_path change" (IntSet.member 2 deps)
        , testCase "ALTER TABLE RLS and SET UNLOGGED scope barriers to target relation" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE noise (z INT);"
                        , "CREATE TABLE t_rls (a INT);"
                        , "ALTER TABLE t_rls NO FORCE ROW LEVEL SECURITY;"
                        , "ALTER TABLE t_rls SET UNLOGGED;"
                        , "SELECT a FROM t_rls;"
                        ]
            (_, graph, deps, _) <- parseAndSlice PostgreSQL sql 5
            assertBool "no GlobalScope fence on table-scoped ALTER" (not (graphHasGlobalBarrier graph))
            assertBool "retains t_rls" (IntSet.member 2 deps)
            assertBool "retains RLS" (IntSet.member 3 deps)
            assertBool "retains SET UNLOGGED" (IntSet.member 4 deps)
            assertBool "excludes noise" (not (IntSet.member 1 deps))
        ]

-- ---------------------------------------------------------------------------
-- 4. MySQL 8.4 vs live mysqld semantic alignment
-- ---------------------------------------------------------------------------

mysqlSemanticTests :: MysqlServer -> TestTree
mysqlSemanticTests srv =
    testGroup
        "MySQL 8.4 vs mysqld live semantic alignment"
        [ testCase "DROP TEMPORARY TABLE does not implicitly commit whereas DROP TABLE commits" $ do
            let sqlNoCommit =
                    BSC.unlines
                        [ "CREATE TABLE base (a INT) ENGINE=InnoDB;"
                        , "CREATE TEMPORARY TABLE tmp_x (k INT);"
                        , "BEGIN;"
                        , "INSERT INTO base VALUES (1);"
                        , "DROP TEMPORARY TABLE tmp_x;"
                        , "ROLLBACK;"
                        , "SELECT COUNT(*) FROM base;"
                        ]
                sqlCommits =
                    BSC.unlines
                        [ "CREATE TABLE base (a INT) ENGINE=InnoDB;"
                        , "CREATE TABLE perm_x (k INT) ENGINE=InnoDB;"
                        , "BEGIN;"
                        , "INSERT INTO base VALUES (1);"
                        , "DROP TABLE perm_x;"
                        , "ROLLBACK;"
                        , "SELECT COUNT(*) FROM base;"
                        ]
            out0 <- execMysqlOrFail "MySQL DROP TEMPORARY TABLE" srv [] sqlNoCommit
            assertEqual "live mysqld rolls back INSERT after DROP TEMPORARY TABLE" "0" out0
            (_, _, deps0, slice0) <- parseAndSlice MySQL sqlNoCommit 7
            assertBool "excludes DROP TEMPORARY TABLE from SELECT COUNT(*) slice" (not (IntSet.member 5 deps0))
            sliceOut0 <- execMysqlOrFail "MySQL DROP TEMPORARY TABLE slice" srv [] slice0
            assertEqual "slice matches live mysqld result" out0 sliceOut0

            out1 <- execMysqlOrFail "MySQL DROP TABLE" srv [] sqlCommits
            assertEqual "live mysqld commits INSERT when DROP TABLE executes" "1" out1
            (_, _, deps1, slice1) <- parseAndSlice MySQL sqlCommits 7
            assertBool "retains INSERT before implicit-commit DROP TABLE" (IntSet.member 4 deps1)
            assertBool "excludes no-op ROLLBACK after implicit commit" (not (IntSet.member 6 deps1))
            sliceOut1 <- execMysqlOrFail "MySQL DROP TABLE slice" srv [] slice1
            assertEqual "slice reproduces committed count 1" out1 sliceOut1
        , testCase "RENAME TABLE skips TEMPORARY shadow while ALTER TABLE RENAME renames TEMPORARY" $ do
            let sqlRename =
                    BSC.unlines
                        [ "CREATE TABLE r1 (a INT);"
                        , "INSERT INTO r1 VALUES (10);"
                        , "CREATE TEMPORARY TABLE r1 (a INT);"
                        , "INSERT INTO r1 VALUES (20);"
                        , "CREATE TABLE noise (z INT);"
                        , "RENAME TABLE r1 TO r2;"
                        , "SELECT a FROM r2;"
                        ]
                sqlAlterRename =
                    BSC.unlines
                        [ "CREATE TABLE r1 (a INT);"
                        , "INSERT INTO r1 VALUES (10);"
                        , "CREATE TEMPORARY TABLE r1 (a INT);"
                        , "INSERT INTO r1 VALUES (20);"
                        , "CREATE TABLE noise (z INT);"
                        , "ALTER TABLE r1 RENAME TO r2;"
                        , "SELECT a FROM r2;"
                        ]
            outRen <- execMysqlOrFail "MySQL RENAME TABLE" srv [] sqlRename
            assertEqual "live mysqld RENAME TABLE renames permanent r1 -> 10" "10" outRen
            (parsedRen, _, depsRen, sliceRen) <- parseAndSlice MySQL sqlRename 7
            renFacts <- singleUnitFacts parsedRen 6
            assertBool
                "MySQL RENAME TABLE emits RenamePermanentRelation"
                (any (\case RenameOp RenamePermanentRelation {} -> True; _ -> False) (factOps renFacts))
            assertBool "retains permanent r1 DDL" (IntSet.member 1 depsRen)
            assertBool "retains permanent r1 INSERT" (IntSet.member 2 depsRen)
            assertBool "excludes temporary r1 DDL" (not (IntSet.member 3 depsRen))
            assertBool "excludes temporary r1 INSERT" (not (IntSet.member 4 depsRen))
            assertBool "excludes noise" (not (IntSet.member 5 depsRen))
            sliceOutRen <- execMysqlOrFail "MySQL RENAME TABLE slice" srv [] sliceRen
            assertEqual "slice reproduces permanent rename result" outRen sliceOutRen

            outAlt <- execMysqlOrFail "MySQL ALTER RENAME" srv [] sqlAlterRename
            assertEqual "live mysqld ALTER TABLE RENAME renames temporary r1 -> 20" "20" outAlt
            (_, _, depsAlt, sliceAlt) <- parseAndSlice MySQL sqlAlterRename 7
            assertBool "retains temporary r1 DDL" (IntSet.member 3 depsAlt)
            assertBool "retains temporary r1 INSERT" (IntSet.member 4 depsAlt)
            assertBool "excludes permanent r1 DDL" (not (IntSet.member 1 depsAlt))
            assertBool "excludes permanent r1 INSERT" (not (IntSet.member 2 depsAlt))
            sliceOutAlt <- execMysqlOrFail "MySQL ALTER RENAME slice" srv [] sliceAlt
            assertEqual "slice reproduces temporary alter rename result" outAlt sliceOutAlt
        , testCase "TEMPORARY tables are scoped by USE database and accessible qualified" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE DATABASE occam_sem_d1;"
                        , "CREATE DATABASE occam_sem_d2;"
                        , "USE occam_sem_d1;"
                        , "CREATE TEMPORARY TABLE st (a INT);"
                        , "INSERT INTO st VALUES (99);"
                        , "USE occam_sem_d2;"
                        , "CREATE TABLE st (a INT);"
                        , "INSERT INTO st VALUES (11);"
                        , "SELECT a FROM st;"
                        , "SELECT a FROM occam_sem_d1.st;"
                        ]
            outFull <- execMysqlOrFail "MySQL USE temp scope" srv ["occam_sem_d1", "occam_sem_d2"] sql
            assertEqual
                "live mysqld isolates d1 temp from d2 unqual and allows d1.st"
                ["11", "99"]
                (T.lines outFull)
            (_, _, depsUnqual, sliceUnqual) <- parseAndSlice MySQL sql 9
            assertBool
                "unqual SELECT in d2 retains d2 perm st"
                (IntSet.member 7 depsUnqual && IntSet.member 8 depsUnqual)
            assertBool
                "unqual SELECT in d2 excludes d1 temp st"
                (not (IntSet.member 4 depsUnqual || IntSet.member 5 depsUnqual))
            sliceOutUnqual <-
                execMysqlOrFail "MySQL USE d2 slice" srv ["occam_sem_d1", "occam_sem_d2"] sliceUnqual
            assertEqual "d2 slice matches 11" "11" sliceOutUnqual

            (_, _, depsQual, sliceQual) <- parseAndSlice MySQL sql 10
            assertBool
                "qualified SELECT d1.st retains d1 temp st"
                (IntSet.member 4 depsQual && IntSet.member 5 depsQual)
            sliceOutQual <-
                execMysqlOrFail "MySQL USE d1.st slice" srv ["occam_sem_d1", "occam_sem_d2"] sliceQual
            assertEqual "d1.st slice matches 99" "99" sliceOutQual
        ]

-- ---------------------------------------------------------------------------
-- 5. MariaDB 11.4 vs live mariadbd semantic alignment
-- ---------------------------------------------------------------------------

mariadbSemanticTests :: MysqlServer -> TestTree
mariadbSemanticTests srv =
    testGroup
        "MariaDB 11.4 vs mariadbd live semantic alignment"
        [ testCase "DROP TEMPORARY TABLE/SEQUENCE do not commit; CREATE TEMPORARY SEQUENCE commits" $ do
            let sqlDropTemp =
                    BSC.unlines
                        [ "CREATE TABLE base (a INT) ENGINE=InnoDB;"
                        , "CREATE TEMPORARY SEQUENCE tseq;"
                        , "BEGIN;"
                        , "INSERT INTO base VALUES (1);"
                        , "DROP TEMPORARY SEQUENCE tseq;"
                        , "ROLLBACK;"
                        , "SELECT COUNT(*) FROM base;"
                        ]
                sqlCreateTempSeq =
                    BSC.unlines
                        [ "CREATE TABLE base (a INT) ENGINE=InnoDB;"
                        , "BEGIN;"
                        , "INSERT INTO base VALUES (1);"
                        , "CREATE TEMPORARY SEQUENCE tseq;"
                        , "ROLLBACK;"
                        , "SELECT COUNT(*) FROM base;"
                        ]
            outDrop <- execMysqlOrFail "MariaDB DROP TEMPORARY SEQUENCE" srv [] sqlDropTemp
            assertEqual "live mariadbd does not commit on DROP TEMPORARY SEQUENCE" "0" outDrop
            (_, _, depsDrop, sliceDrop) <- parseAndSlice MariaDB sqlDropTemp 7
            assertBool "excludes DROP TEMPORARY SEQUENCE" (not (IntSet.member 5 depsDrop))
            sliceOutDrop <- execMysqlOrFail "MariaDB DROP TEMPORARY SEQUENCE slice" srv [] sliceDrop
            assertEqual "slice matches 0" outDrop sliceOutDrop

            outCreate <- execMysqlOrFail "MariaDB CREATE TEMPORARY SEQUENCE" srv [] sqlCreateTempSeq
            assertEqual "live mariadbd commits on CREATE TEMPORARY SEQUENCE" "1" outCreate
            (_, _, depsCreate, sliceCreate) <- parseAndSlice MariaDB sqlCreateTempSeq 6
            assertBool "retains INSERT committed by CREATE TEMPORARY SEQUENCE" (IntSet.member 3 depsCreate)
            assertBool "excludes no-op ROLLBACK after implicit commit" (not (IntSet.member 5 depsCreate))
            sliceOutCreate <- execMysqlOrFail "MariaDB CREATE TEMPORARY SEQUENCE slice" srv [] sliceCreate
            assertEqual "slice matches 1" outCreate sliceOutCreate
        , testCase "RENAME TABLE renames shadowing TEMPORARY table (diverging from MySQL)" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE r1 (a INT);"
                        , "INSERT INTO r1 VALUES (10);"
                        , "CREATE TEMPORARY TABLE r1 (a INT);"
                        , "INSERT INTO r1 VALUES (20);"
                        , "CREATE TABLE noise (z INT);"
                        , "RENAME TABLE r1 TO r2;"
                        , "SELECT a FROM r2;"
                        ]
            outFull <- execMysqlOrFail "MariaDB RENAME TABLE" srv [] sql
            assertEqual "live mariadbd RENAME TABLE renames temporary r1 -> 20" "20" outFull
            (parsed, _, deps, sliceSql) <- parseAndSlice MariaDB sql 7
            renFacts <- singleUnitFacts parsed 6
            assertBool
                "MariaDB RENAME TABLE emits RenameRelation (not RenamePermanentRelation)"
                (any (\case RenameOp RenameRelation {} -> True; _ -> False) (factOps renFacts))
            assertBool "retains temporary r1 DDL" (IntSet.member 3 deps)
            assertBool "retains temporary r1 INSERT" (IntSet.member 4 deps)
            assertBool "excludes permanent r1 DDL" (not (IntSet.member 1 deps))
            assertBool "excludes permanent r1 INSERT" (not (IntSet.member 2 deps))
            assertBool "excludes noise" (not (IntSet.member 5 deps))
            sliceOut <- execMysqlOrFail "MariaDB RENAME TABLE slice" srv [] sliceSql
            assertEqual "slice reproduces MariaDB temporary rename result" outFull sliceOut
        , testCase "TEMPORARY tables are scoped by USE database and accessible qualified" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE DATABASE occam_sem_d1;"
                        , "CREATE DATABASE occam_sem_d2;"
                        , "USE occam_sem_d1;"
                        , "CREATE TEMPORARY TABLE st (a INT);"
                        , "INSERT INTO st VALUES (99);"
                        , "USE occam_sem_d2;"
                        , "SELECT a FROM occam_sem_d1.st;"
                        ]
            outFull <- execMysqlOrFail "MariaDB qualified temp" srv ["occam_sem_d1", "occam_sem_d2"] sql
            assertEqual "live mariadbd reads d1.st after USE d2" "99" outFull
            (_, _, deps, sliceSql) <- parseAndSlice MariaDB sql 7
            assertBool "retains d1 temp DDL and INSERT" (IntSet.member 4 deps && IntSet.member 5 deps)
            sliceOut <-
                execMysqlOrFail "MariaDB qualified temp slice" srv ["occam_sem_d1", "occam_sem_d2"] sliceSql
            assertEqual "slice reads 99" outFull sliceOut
        ]

-- ---------------------------------------------------------------------------
-- 6. ClickHouse 26.8 vs live clickhouse local semantic alignment
-- ---------------------------------------------------------------------------

clickhouseSemanticTests :: TestTree
clickhouseSemanticTests =
    testGroup
        "ClickHouse 26.8 vs clickhouse local live semantic alignment"
        [ testCase "ALTER TABLE ... UPDATE tracks write/column reads without GlobalScope" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE noise (z Int32) ENGINE = Memory;"
                        , "CREATE TABLE items (id Int32, v Int32) ENGINE = MergeTree ORDER BY id;"
                        , "INSERT INTO items VALUES (1, 10), (2, 20);"
                        , "ALTER TABLE items UPDATE v = v + 5 WHERE id = 1 SETTINGS mutations_sync = 1;"
                        , "SELECT v FROM items WHERE id = 1;"
                        ]
            outFull <- execClickHouseOrFail sql
            assertEqual "live ClickHouse ALTER UPDATE yields 15" "15" outFull
            (parsed, graph, deps, sliceSql) <- parseAndSlice ClickHouse sql 5
            assertBool "no GlobalScope fence on ALTER UPDATE" (not (graphHasGlobalBarrier graph))
            updFacts <- singleUnitFacts parsed 4
            assertEqual
                "ALTER UPDATE records UpdateFilteredRows write and column v"
                [(UpdateWrite UpdateFilteredRows, [Ident "v"])]
                [ (dataWriteKind dw, dataWriteUnqualifiedColumns dw)
                | WriteDataOp dw <- factOps updFacts
                ]
            assertBool "retains items DDL" (IntSet.member 2 deps)
            assertBool "retains items INSERT" (IntSet.member 3 deps)
            assertBool "retains ALTER TABLE items UPDATE" (IntSet.member 4 deps)
            assertBool "excludes noise table" (not (IntSet.member 1 deps))
            sliceOut <- execClickHouseOrFail sliceSql
            assertEqual "slice reproduces ClickHouse ALTER UPDATE output 15" outFull sliceOut
        , testCase "DETACH TABLE / ATTACH TABLE isolates unrelated noise without GlobalScope" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE noise (z Int32) ENGINE = Memory;"
                        , "CREATE DATABASE db_ch_sem;"
                        , "CREATE TABLE db_ch_sem.t (a Int32) ENGINE = MergeTree ORDER BY a;"
                        , "INSERT INTO db_ch_sem.t VALUES (7);"
                        , "DETACH TABLE db_ch_sem.t SYNC;"
                        , "ATTACH TABLE db_ch_sem.t;"
                        , "SELECT a FROM db_ch_sem.t;"
                        ]
            outFull <- execClickHouseOrFail sql
            assertEqual "live ClickHouse DETACH/ATTACH keeps row 7" "7" outFull
            (_, graph, deps, sliceSql) <- parseAndSlice ClickHouse sql 7
            assertBool
                "no GlobalScope fence on ClickHouse DETACH/ATTACH TABLE"
                (not (graphHasGlobalBarrier graph))
            assertBool "retains DB DDL" (IntSet.member 2 deps)
            assertBool "retains table DDL" (IntSet.member 3 deps)
            assertBool "retains INSERT" (IntSet.member 4 deps)
            assertBool "retains DETACH" (IntSet.member 5 deps)
            assertBool "retains ATTACH" (IntSet.member 6 deps)
            assertBool "excludes noise" (not (IntSet.member 1 deps))
            sliceOut <- execClickHouseOrFail sliceSql
            assertEqual "slice reproduces DETACH/ATTACH output 7" outFull sliceOut
        , testCase "CREATE MATERIALIZED VIEW ... TO retains src, dst, and INSERT INTO src" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE src (k UInt32) ENGINE = Memory;"
                        , "CREATE TABLE dst (k UInt32) ENGINE = Memory;"
                        , "CREATE MATERIALIZED VIEW mv TO dst AS SELECT k * 10 AS k FROM src;"
                        , "INSERT INTO src VALUES (1), (2);"
                        , "SELECT k FROM dst ORDER BY k;"
                        ]
            outFull <- execClickHouseOrFail sql
            assertEqual "live ClickHouse MV TO routes rows into dst" ["10", "20"] (T.lines outFull)
            (_, _, deps, sliceSql) <- parseAndSlice ClickHouse sql 5
            assertBool "retains src" (IntSet.member 1 deps)
            assertBool "retains dst" (IntSet.member 2 deps)
            assertBool "retains mv" (IntSet.member 3 deps)
            assertBool "retains INSERT INTO src" (IntSet.member 4 deps)
            sliceOut <- execClickHouseOrFail sliceSql
            assertEqual "slice reproduces MV TO rows" outFull sliceOut
        ]

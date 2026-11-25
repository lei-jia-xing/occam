module SqliteOracleChecks (tests) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import Data.Aeson (Value (..))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Occam.Core (OracleOutcome (..), ResultStatement (..), ResultTrace (..))
import Occam.Oracle (OracleObservation (..))
import Occam.Oracle.Sqlite (
    executeSqliteCandidate,
    generateSqliteContract,
    generateSqliteContractWithResultStatement,
    generateSqliteContractWithTrace,
    prepareSqliteContractObserver,
    splitStatements,
    sqliteContractObservation,
    sqliteContractObservationTimeout,
    sqliteContractOracle,
 )
import Occam.Parser (SQLDialect (..))
import Occam.Pool (withPredicatePool)
import Occam.Reducer (statementAudit)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "In-process SQLite Oracle"
        [ testCase "splitStatements splits statements with sqlite3_complete" $ do
            let sql = "CREATE TABLE t (id INT);\n-- comment\nINSERT INTO t VALUES (1);\nSELECT * FROM t;"
            stmts <- splitStatements sql
            assertEqual "three statements" 3 (length stmts)
        , testCase "executeSqliteCandidate executes query and returns rowset" $ do
            let sql = "CREATE TABLE t (a INT, b TEXT);\nINSERT INTO t VALUES (1, 'hello');\nSELECT * FROM t;"
            res <- executeSqliteCandidate sql
            assertEqual "rowset matches" (Right [[Number 1, String "hello"]]) res
        , testCase "sqliteContractOracle accepts matching and rejects invalid SQL" $ do
            let sql = "CREATE TABLE t (a INT, b TEXT);\nINSERT INTO t VALUES (1, 'hello');\nSELECT * FROM t;"
            contractRes <- generateSqliteContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    let oracle = sqliteContractOracle contract
                    o1 <- oracle sql
                    assertEqual "original accepts" OracleAccepted o1
                    o2 <- oracle "SYNTAX ERROR;"
                    assertEqual "syntax error rejects" OracleRejected o2
                    o3 <- oracle "SELECT 42;"
                    assertEqual "mismatched query rejects" OracleRejected o3
                    o4 <- oracle ""
                    assertEqual "empty candidate rejects" OracleRejected o4
        , testCase "SQLite checkpoints bind result evidence to the expected contract" $ do
            let prefix = "SELECT 1 AS n; CREATE TABLE a(i); CREATE TABLE b(i); CREATE TABLE c(i);"
                baseline = prefix <> " CREATE TABLE d(i); CREATE TABLE e(i); CREATE TABLE f(i); CREATE TABLE g(i);"
            one <- generateSqliteContract "SELECT 1 AS n;" >>= either (assertFailure . T.unpack) pure
            two <- generateSqliteContract "SELECT 2 AS n;" >>= either (assertFailure . T.unpack) pure
            warm <- sqliteContractOracle one baseline
            assertEqual "warm the checkpoint under the first contract" OracleAccepted warm
            mismatch <- sqliteContractOracle two prefix
            assertEqual "the checkpoint cannot satisfy the second contract" OracleRejected mismatch
            match <- sqliteContractOracle one prefix
            assertEqual "the original contract still accepts" OracleAccepted match
        , testCase "SQLite enforces a shared deadline across short statements" $ do
            contract <- generateSqliteContract "SELECT 1 AS n;" >>= either (assertFailure . T.unpack) pure
            let candidate = BSC.concat (replicate 12000 "SELECT 1 AS n;\n")
            BS.length candidate `seq` pure ()
            delayed <- sqliteContractObservationTimeout 1000 contract candidate
            assertEqual "the sequence exceeds its deadline" OracleTimedOut delayed
            clean <- sqliteContractOracle contract "SELECT 1 AS n;"
            assertEqual "the next observation starts clean" OracleAccepted clean
        , testCase "SQLite preserves an empty final result and its column names" $ do
            let sql = "SELECT 1 AS earlier; SELECT 2 AS answer WHERE FALSE;"
            contractRes <- generateSqliteContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    let oracle = sqliteContractOracle contract
                    empty <- oracle "SELECT 2 AS answer WHERE FALSE;"
                    dropped <- oracle "SELECT 1 AS earlier;"
                    renamed <- oracle "SELECT 2 AS other WHERE FALSE;"
                    noResult <- oracle "CREATE TABLE x (a INT);"
                    assertEqual "earlier result is irrelevant" OracleAccepted empty
                    assertEqual "dropping final result changes the observation" OracleRejected dropped
                    assertEqual "renaming an empty result column changes the observation" OracleRejected renamed
                    assertEqual "a script without a result set is distinct" OracleRejected noResult
        , testCase "SQLite locates its final result before trailing DDL and ignores empty statements" $ do
            result <-
                generateSqliteContractWithResultStatement
                    1000000
                    ";; SELECT 1 AS answer; CREATE TABLE tail (x INT); -- end"
            assertEqual
                "the result belongs to the first of two statements"
                (Right (ResultStatement 2 0))
                (fmap snd result)
        , testCase "SQLite compares value storage types without lossy JSON coercion" $ do
            result <- generateSqliteContract "SELECT x'ff' AS blob_value, 1.0 AS float_value;"
            case result of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    changed <- sqliteContractOracle contract "SELECT x'ff' AS blob_value, 1 AS float_value;"
                    assertEqual "REAL and INTEGER remain different" OracleRejected changed
        , testCase "SQLite interrupts a long-running inline query" $ do
            result <- generateSqliteContract "SELECT 1 AS a;"
            case result of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    observed <-
                        sqliteContractObservationTimeout
                            20000
                            contract
                            "WITH RECURSIVE t(i) AS (VALUES (0) UNION ALL SELECT i+1 FROM t WHERE i<1000000000) SELECT sum(i) AS a FROM t;"
                    assertEqual "candidate reaches its per-probe timeout" OracleTimedOut observed
        , testCase "sqliteContractObservation rejects candidate SQL errors" $ do
            let sql = "CREATE TABLE t (a INT);\nSELECT * FROM t;"
            contractRes <- generateSqliteContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    observation <- sqliteContractObservation contract "SYNTAX ERROR;"
                    assertEqual "SQL error is a definite rejection" (OracleFinished (ExitFailure 1)) observation
        , testCase "sqliteContractOracle treats non-SQL execution faults as inconclusive" $ do
            let sql = "CREATE TABLE t (a INT);\nSELECT * FROM t;"
                invalidUtf8 = BS.pack [0x53, 0x45, 0x4c, 0x45, 0x43, 0x54, 0x20, 0xff, 0x3b]
            contractRes <- generateSqliteContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    outcome <- sqliteContractOracle contract invalidUtf8
                    assertEqual "infrastructure fault is inconclusive" OracleInconclusive outcome
                    observation <- sqliteContractObservation contract invalidUtf8
                    case observation of
                        OracleIOFailed err ->
                            assertBool "infrastructure detail is preserved" (not (T.null err))
                        other ->
                            assertFailure ("expected OracleIOFailed, got " ++ show other)
        , testCase "in-process reduction removes dead noise table" $ do
            let sql =
                    "CREATE TABLE noise (x INT);\nINSERT INTO noise VALUES (99);\nCREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\nSELECT count(*) FROM t;"
            contractRes <- generateSqliteContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    let oracle = sqliteContractOracle contract
                    reduced <- withPredicatePool 1 True (fmap (== OracleAccepted) . oracle) $ \pool ->
                        statementAudit SQLite pool sql
                    assertBool "noise removed" (not (BSC.isInfixOf "noise" reduced))
                    assertBool "table t retained" (BSC.isInfixOf "t" reduced)
                    oReduced <- oracle reduced
                    assertEqual "reduced SQL satisfies oracle" OracleAccepted oReduced
        , testCase "pooled SQLite handles isolate tables, temp objects, PRAGMAs, and open transactions" $ do
            -- 1. Mutate main schema + AUTOINCREMENT, PRAGMAs, and leave an open transaction
            r1 <-
                executeSqliteCandidate
                    "PRAGMA foreign_keys = ON; PRAGMA recursive_triggers = ON; PRAGMA user_version = 99; BEGIN; CREATE TABLE t(id INTEGER PRIMARY KEY AUTOINCREMENT, v INT); INSERT INTO t(v) VALUES (7);"
            assertEqual "first script executes" (Right []) r1

            -- 2. Verify PRAGMAs, main tables, and autocommit were cleanly reset
            r2 <-
                executeSqliteCandidate
                    "PRAGMA foreign_keys;"
            assertEqual "foreign_keys reset to OFF" (Right [[Number 0]]) r2
            r3 <-
                executeSqliteCandidate
                    "PRAGMA user_version;"
            assertEqual "user_version reset to 0" (Right [[Number 0]]) r3
            r4 <-
                executeSqliteCandidate
                    "SELECT count(*) FROM sqlite_master;"
            assertEqual "main schema reset to empty" (Right [[Number 0]]) r4

            -- 3. Touch TEMP database, then verify TEMP does not leak to subsequent call
            r5 <-
                executeSqliteCandidate
                    "CREATE TEMP TABLE tmp_leak(x INT); INSERT INTO tmp_leak VALUES (1); SELECT x FROM tmp_leak;"
            assertEqual "temp table query works" (Right [[Number 1]]) r5
            r6 <-
                executeSqliteCandidate
                    "SELECT x FROM tmp_leak;"
            assertBool "temp table must not leak to next pooled execution" (isLeft r6)

            -- 4. Mid-script SQL error must leave next execution clean
            r7 <-
                executeSqliteCandidate
                    "CREATE TABLE residual(x INT); SELECT * FROM nonexistent_table;"
            assertBool "mid-script error rejects" (isLeft r7)
            r8 <-
                executeSqliteCandidate
                    "SELECT * FROM residual;"
            assertBool "residual table from failed script must not leak" (isLeft r8)

            -- 5. DML connection counters (last_insert_rowid/changes/total_changes) must not leak
            r9 <-
                executeSqliteCandidate
                    "CREATE TABLE t_dml(id INTEGER PRIMARY KEY, v INT); INSERT INTO t_dml(v) VALUES (10), (20);"
            assertEqual "DML setup executes" (Right []) r9
            r10 <-
                executeSqliteCandidate
                    "SELECT last_insert_rowid(), changes(), total_changes();"
            assertEqual
                "connection DML counters remain zero on subsequent execution"
                (Right [[Number 0, Number 0, Number 0]])
                r10

            -- 6. Normal views/triggers stay valid, and unpoolable PRAGMAs/ATTACH execute without leaking
            r11 <-
                executeSqliteCandidate
                    "CREATE TABLE tv(x INT); INSERT INTO tv VALUES (1); CREATE VIEW v AS SELECT x FROM tv; SELECT x FROM v;"
            assertEqual "normal view executes cleanly" (Right [[Number 1]]) r11
            r12 <-
                executeSqliteCandidate
                    "PRAGMA case_sensitive_like = ON; SELECT 'a' LIKE 'A';"
            assertEqual "non-allowlisted PRAGMA executes cleanly" (Right [[Number 0]]) r12
            r13 <-
                executeSqliteCandidate
                    "SELECT 'a' LIKE 'A';"
            assertEqual "case_sensitive_like must not leak to next execution" (Right [[Number 1]]) r13
            r14 <-
                executeSqliteCandidate
                    "ATTACH ':memory:' AS aux; CREATE TABLE aux.a(x INT); INSERT INTO aux.a VALUES (5); SELECT x FROM aux.a;"
            assertEqual "ATTACH executes cleanly without leaking" (Right [[Number 5]]) r14
        , testCase "pooled SQLite handles remain isolated under concurrent threads" $ do
            let workers = 8 :: Int
                rounds = 15 :: Int
            dones <- forM [0 .. workers - 1] $ \wid -> do
                done <- newEmptyMVar
                _ <- forkIO $ do
                    res <- try @SomeException $ forM_ [0 .. rounds - 1] $ \r -> do
                        let tag = wid * 100 + r
                            sql1 =
                                BSC.pack
                                    ( "CREATE TABLE c(id INTEGER PRIMARY KEY, x INT); INSERT INTO c(x) VALUES ("
                                        ++ show tag
                                        ++ "); SELECT x, last_insert_rowid() FROM c;"
                                    )
                        o1 <- executeSqliteCandidate sql1
                        assertEqual
                            "concurrent DML query returns expected row and rowid"
                            (Right [[Number (fromIntegral tag), Number 1]])
                            o1
                        o2 <-
                            executeSqliteCandidate
                                "SELECT count(*) FROM sqlite_master UNION ALL SELECT total_changes();"
                        assertEqual
                            "concurrent fresh query observes clean schema and zero total_changes"
                            (Right [[Number 0], [Number 0]])
                            o2
                    putMVar done res
                pure done
            forM_ dones $ \done -> do
                res <- takeMVar done
                case res of
                    Left err -> assertFailure ("concurrent SQLite pool error: " ++ show err)
                    Right () -> pure ()
        , testCase
            "SQLite binary matcher and prefix checkpoints preserve exact types, DML counters, and failing prefixes"
            $ do
                -- 1. Binary matcher distinguishes -0.0 vs +0.0, empty vs non-empty blobs, NULL, text, and int.
                let typedSql = "SELECT 1 AS i, -0.0 AS f, 'hi' AS s, X'' AS b0, X'00ff' AS b1, NULL AS n;"
                typedRes <- generateSqliteContract typedSql
                case typedRes of
                    Left err -> assertFailure (T.unpack err)
                    Right contract -> do
                        assertEqual "exact typed row matches" OracleAccepted =<< sqliteContractOracle contract typedSql
                        assertEqual
                            "+0.0 differs from -0.0"
                            OracleRejected
                            =<< sqliteContractOracle
                                contract
                                "SELECT 1 AS i, 0.0 AS f, 'hi' AS s, X'' AS b0, X'00ff' AS b1, NULL AS n;"
                        assertEqual
                            "non-empty blob differs from empty blob"
                            OracleRejected
                            =<< sqliteContractOracle
                                contract
                                "SELECT 1 AS i, -0.0 AS f, 'hi' AS s, X'00' AS b0, X'00ff' AS b1, NULL AS n;"
                -- 2. Prefix checkpoints (>= 8 stmts) preserve exact results, DML counters, and failing-prefix capture.
                let setup =
                        "CREATE TABLE t(id INTEGER PRIMARY KEY, x INT);"
                            : ["INSERT INTO t(x) VALUES (" <> BSC.pack (show i) <> ");" | i <- [1 .. 20 :: Int]]
                    script = BSC.unlines (setup ++ ["SELECT sum(x), last_insert_rowid() FROM t;"])
                contractRes <- generateSqliteContractWithTrace 500000 script
                case contractRes of
                    Left err -> assertFailure (T.unpack err)
                    Right (contract, trace) -> do
                        assertBool
                            "script is deterministic and >= 8 stmts"
                            (traceDeterministic trace && traceStatementCount trace >= 22)
                        failsRef <- newIORef ([] :: [BS.ByteString])
                        let observe = prepareSqliteContractObserver 500000 contract (\p -> modifyIORef' failsRef (p :))
                        (obsOk, mTrOk) <- observe script
                        assertEqual
                            "checkpointed exact match with last_insert_rowid() succeeds"
                            (OracleFinished ExitSuccess)
                            obsOk
                        assertBool "checkpointed exact match preserves trace" (isJust mTrOk)
                        -- Missing CREATE TABLE causes SQL error on first INSERT; failing prefix must be recorded.
                        let broken = BSC.unlines (drop 1 setup ++ ["SELECT sum(x), last_insert_rowid() FROM t;"])
                        (obsBad, _) <- observe broken
                        assertEqual "missing table rejects cleanly" (OracleFinished (ExitFailure 1)) obsBad
                        recorded <- readIORef failsRef
                        assertBool "deterministic semicolon-terminated failing prefix recorded" (not (null recorded))
                        assertBool
                            "recorded prefix is a prefix of the broken script"
                            (all (`BS.isPrefixOf` broken) recorded)
        ]
    where
        isLeft (Left _) = True
        isLeft (Right _) = False

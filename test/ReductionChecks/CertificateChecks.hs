module ReductionChecks.CertificateChecks (checks) where

import Control.Monad (forM_, when)
import Data.Aeson (Value (..))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.IntSet qualified as IntSet
import Data.Maybe (isJust)
import Occam.Certificate (
    EarlyRejectCertifier (..),
    buildEarlyRejectCertifier,
    checkEarlyRejection,
 )
import Occam.Core (
    EarlyRejectReason (..),
    OracleOutcome (..),
    ResultStatement (..),
    ResultTrace (..),
    Statement (..),
    StatementId (..),
 )
import Occam.Facts (StatementUnit (..))
import Occam.Logger (Logger (..), defaultLogConfig, nullLogger)
import Occam.Oracle (OracleObservationDetails (..), observationOutcome)
import Occam.Oracle.DuckDB (
    defaultDuckDBCandidateTimeoutMicros,
    duckdbContractOracle,
    generateDuckDBContractWithTrace,
    isDuckDBAvailable,
 )
import Occam.Oracle.Inline (newResultTraceTracker, prepareResultOracleWithTrace)
import Occam.Oracle.Sqlite (
    SqliteError (..),
    executeSqliteCandidateDetailedTimeout,
    generateSqliteContract,
    generateSqliteContractWithTrace,
    sqliteContractOracle,
 )
import Occam.Parser (ParsedSQL (..), SQLDialect (..), defaultParseOptions, parseSQL)
import Occam.Reducer (
    ReductionConfig (..),
    ReductionResult (..),
    cutRemovedStatementInts,
    defaultReductionConfig,
    reduceSQLWithConfig,
    reduceSQLWithResultSlicing,
    reduceSQLWithResultTrace,
 )
import ReductionChecks.Support (outcomePool, predicatePool)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

checks :: [TestTree]
checks =
    [ testCase "SQLite releases statements after a timeout during execution" $ do
        let query =
                "WITH RECURSIVE r(x) AS (VALUES (1) UNION ALL SELECT x + 1 FROM r WHERE x < 1000000000) SELECT sum(x) FROM r;"
        forM_ [query, "CREATE TABLE t AS " <> query] $ \sql -> do
            outcome <- executeSqliteCandidateDetailedTimeout 50000 sql
            assertEqual "long execution reaches its timeout" (Left SqliteTimeout) outcome
        recovered <- executeSqliteCandidateDetailedTimeout 1000000 "SELECT 42;"
        assertEqual "subsequent SQLite run succeeds" (Right [[Number 42]]) recovered
    , testCase
        "Early-rejection exact-prefix certificate distinguishes mismatching and matching SQLite prefixes"
        $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t(x INT);"
                        , "INSERT INTO t VALUES (1);"
                        , "INSERT INTO t VALUES (42) RETURNING x AS answer;"
                        , "CREATE TABLE noise(y INT);"
                        , "SELECT 0 AS answer;"
                        , "SELECT 42 AS answer;"
                        ]
            contractRes <- generateSqliteContractWithTrace 500000 sql
            (contract, trace) <- either (assertFailure . show) pure contractRes
            parsed <- parseSQL (defaultParseOptions SQLite) sql >>= either (assertFailure . show) pure
            let units = parsedUnits parsed
                stmts = map unitStatement units
                cert = buildEarlyRejectCertifier sql units (Just trace)
                prefixKept n = IntSet.fromList [1 .. n]
                prefixBytes n = cutRemovedStatementInts sql stmts (prefixKept n)
            assertBool "deterministic SQLite trace expected" (traceDeterministic trace)
            assertEqual
                "prefix 1 (CREATE TABLE) is certified rejected"
                (Just EarlyRejectExactPrefix)
                (checkEarlyRejection cert (prefixKept 1) (prefixBytes 1))
            assertEqual
                "prefix 2 (CREATE + INSERT) is certified rejected"
                (Just EarlyRejectExactPrefix)
                (checkEarlyRejection cert (prefixKept 2) (prefixBytes 2))
            assertEqual
                "prefix 3 (matching RETURNING) must NOT be rejected"
                Nothing
                (checkEarlyRejection cert (prefixKept 3) (prefixBytes 3))
            assertEqual
                "prefix 4 (matching RETURNING + DDL) must NOT be rejected"
                Nothing
                (checkEarlyRejection cert (prefixKept 4) (prefixBytes 4))
            assertEqual
                "prefix 5 (overwriting SELECT 0) is certified rejected"
                (Just EarlyRejectExactPrefix)
                (checkEarlyRejection cert (prefixKept 5) (prefixBytes 5))
            forM_ [1 .. 5] $ \n -> do
                let kept = prefixKept n
                    cand = prefixBytes n
                    certRes = checkEarlyRejection cert kept cand
                outcome <- sqliteContractOracle contract cand
                when (isJust certRes) $
                    assertEqual
                        ("certified prefix " ++ show n ++ " must be rejected by native SQLite")
                        OracleRejected
                        outcome
            outcome3 <- sqliteContractOracle contract (prefixBytes 3)
            outcome4 <- sqliteContractOracle contract (prefixBytes 4)
            assertEqual "prefix 3 actually matches native SQLite" OracleAccepted outcome3
            assertEqual "prefix 4 actually matches native SQLite" OracleAccepted outcome4
    , testCase "Nondeterministic SQLite scripts disable exact-prefix certification" $ do
        let scripts =
                [ "CREATE TABLE t(x INT); SELECT random();"
                , "CREATE TABLE t(x INT); SELECT datetime('now');"
                , "CREATE TABLE t(x INT); SELECT CURRENT_TIMESTAMP;"
                , "CREATE TABLE t(ts TEXT DEFAULT CURRENT_TIMESTAMP); INSERT INTO t DEFAULT VALUES; SELECT ts FROM t; SELECT 1;"
                ]
        forM_ scripts $ \sql -> do
            contractRes <- generateSqliteContractWithTrace 500000 sql
            (_, trace) <- either (assertFailure . show) pure contractRes
            parsed <- parseSQL (defaultParseOptions SQLite) sql >>= either (assertFailure . show) pure
            let cert = buildEarlyRejectCertifier sql (parsedUnits parsed) (Just trace)
            assertBool
                ("expected traceDeterministic=False for " ++ BSC.unpack sql)
                (not (traceDeterministic trace))
            assertBool
                ("expected empty exact-prefix byte lengths for " ++ BSC.unpack sql)
                (IntSet.null (certExactPrefixByteLens cert))
            assertBool
                ("expected empty exact-prefix lengths for " ++ BSC.unpack sql)
                (IntSet.null (certExactPrefixLengths cert))
    , testCase
        "Zero-result allowlist excludes all potentially result-bearing statements on SQLite and DuckDB"
        $ do
            let sqliteSql =
                    BSC.unlines
                        [ "CREATE TABLE t(x INT);"
                        , "CREATE TABLE ctas AS SELECT 1 AS x;"
                        , "INSERT INTO t VALUES (1);"
                        , "INSERT INTO t VALUES (2) RETURNING x;"
                        , "UPDATE t SET x = 3;"
                        , "UPDATE t SET x = 4 RETURNING x;"
                        , "DELETE FROM t WHERE x = 1;"
                        , "DELETE FROM t WHERE x = 4 RETURNING x;"
                        , "PRAGMA foreign_keys = ON;"
                        , "PRAGMA foreign_keys;"
                        , "EXPLAIN SELECT 1;"
                        , "SELECT 42 AS answer;"
                        ]
            contractRes <- generateSqliteContractWithTrace 500000 sqliteSql
            (_, trace) <- either (assertFailure . show) pure contractRes
            parsed <- parseSQL (defaultParseOptions SQLite) sqliteSql >>= either (assertFailure . show) pure
            let cert = buildEarlyRejectCertifier sqliteSql (parsedUnits parsed) (Just trace)
                guaranteed = certGuaranteedNonResult cert
            -- 1: CREATE TABLE, 3: INSERT, 5: UPDATE, 7: DELETE are allowlisted.
            -- 2: CTAS, 4/6/8: RETURNING, 9/10: PRAGMA, 11: EXPLAIN, 12: SELECT are excluded.
            assertEqual "SQLite guaranteed non-result set" (IntSet.fromList [1, 3, 5, 7]) guaranteed
            hasDuck <- isDuckDBAvailable
            when hasDuck $ do
                let duckSql =
                        BSC.unlines
                            [ "CREATE TABLE t(x INT);"
                            , "CREATE TABLE ctas AS SELECT 1 AS x;"
                            , "INSERT INTO t VALUES (1);"
                            , "INSERT INTO t VALUES (2) RETURNING x;"
                            , "UPDATE t SET x = 3;"
                            , "UPDATE t SET x = 4 RETURNING x;"
                            , "DELETE FROM t WHERE x = 1;"
                            , "DELETE FROM t WHERE x = 4 RETURNING x;"
                            , "CREATE VIEW v AS SELECT 1 AS x;"
                            , "CREATE INDEX idx_t ON t(x);"
                            , "ALTER TABLE t ADD COLUMN y INT;"
                            , "BEGIN;"
                            , "COMMIT;"
                            , "DROP VIEW v;"
                            , "EXPLAIN SELECT 1;"
                            , "SELECT 42 AS answer;"
                            ]
                duckRes <- generateDuckDBContractWithTrace defaultDuckDBCandidateTimeoutMicros duckSql
                (duckContract, duckTrace) <- either (assertFailure . show) pure duckRes
                duckParsed <- parseSQL (defaultParseOptions DuckDB) duckSql >>= either (assertFailure . show) pure
                let duckCert = buildEarlyRejectCertifier duckSql (parsedUnits duckParsed) (Just duckTrace)
                    duckGuaranteed = certGuaranteedNonResult duckCert
                    duckStmts = map unitStatement (parsedUnits duckParsed)
                assertEqual
                    "DuckDB guaranteed non-result set"
                    (IntSet.fromList [1, 3, 5, 7, 9, 10, 11, 12, 13, 14])
                    duckGuaranteed
                -- Verify every allowlisted statement individually rejects on native DuckDB.
                forM_ (IntSet.toList duckGuaranteed) $ \sid -> do
                    let cand = cutRemovedStatementInts duckSql duckStmts (IntSet.singleton sid)
                    outcome <- duckdbContractOracle duckContract cand
                    assertEqual
                        ("allowlisted DuckDB statement " ++ show sid ++ " must reject natively")
                        OracleRejected
                        outcome
    , testCase "Exhaustive native soundness validation on SQLite and DuckDB finds zero false rejections" $ do
        let allSubsets ids =
                map IntSet.fromList (foldr (\x acc -> acc ++ map (x :) acc) [[]] ids)
            sqliteScripts =
                [ BSC.unlines
                    [ "CREATE TABLE t(x INT);"
                    , "CREATE TRIGGER tr AFTER INSERT ON t BEGIN SELECT 1; END;"
                    , "INSERT INTO t VALUES (42);"
                    , "SAVEPOINT s;"
                    , "UPDATE t SET x = 0;"
                    , "ROLLBACK TO s;"
                    , "RELEASE s;"
                    , "SELECT x AS answer FROM t;"
                    ]
                , BSC.unlines
                    [ "PRAGMA foreign_keys = ON;"
                    , "CREATE TABLE p(id INT PRIMARY KEY);"
                    , "CREATE TABLE c(pid INT REFERENCES p(id));"
                    , "INSERT INTO p VALUES (1);"
                    , "INSERT INTO c VALUES (1) RETURNING pid AS answer;"
                    , "SELECT 0 AS answer;"
                    , "SELECT 1 AS answer;"
                    ]
                , BSC.unlines
                    [ "CREATE TABLE a(x INT); -- trailing comment"
                    , "CREATE VIEW v AS SELECT 42 AS answer;"
                    , "INSERT INTO a VALUES (1);"
                    , "CREATE TABLE b AS SELECT x FROM a;"
                    , "SELECT answer FROM v WHERE 0;"
                    ]
                , BSC.unlines
                    [ "CREATE TABLE t(x INT);"
                    , "CREATE TABLE u(y INT);"
                    , "INSERT INTO u VALUES (10);"
                    , "ALTER TABLE u RENAME TO t2;"
                    , "WITH t AS (SELECT 1 AS x) SELECT x + (SELECT y FROM t2) AS answer FROM t;"
                    ]
                , BSC.unlines
                    [ "CREATE TABLE t(x INT);"
                    , "BEGIN;"
                    , "INSERT INTO t VALUES (42);"
                    , "COMMIT;"
                    , "DROP TABLE IF EXISTS missing_tbl;"
                    , "SELECT x AS answer FROM t;"
                    ]
                ]
        sqliteCheckedRef <- newIORef (0 :: Int)
        forM_ sqliteScripts $ \sql -> do
            contractRes <- generateSqliteContractWithTrace 500000 sql
            (contract, trace) <- either (assertFailure . show) pure contractRes
            parsed <- parseSQL (defaultParseOptions SQLite) sql >>= either (assertFailure . show) pure
            let units = parsedUnits parsed
                stmts = map unitStatement units
                cert = buildEarlyRejectCertifier sql units (Just trace)
                ids = map (unStatementId . stmtId) stmts
            forM_ (allSubsets ids) $ \kept -> do
                let cand = cutRemovedStatementInts sql stmts kept
                case checkEarlyRejection cert kept cand of
                    Nothing -> pure ()
                    Just reason -> do
                        modifyIORef' sqliteCheckedRef (+ 1)
                        outcome <- sqliteContractOracle contract cand
                        assertEqual
                            ( "Soundness violation on SQLite ("
                                ++ show reason
                                ++ ") for kept="
                                ++ show (IntSet.toList kept)
                                ++ " sql="
                                ++ BSC.unpack cand
                            )
                            OracleRejected
                            outcome
        sqliteChecked <- readIORef sqliteCheckedRef
        assertBool
            ("expected >= 100 SQLite certified subsets validated, got " ++ show sqliteChecked)
            (sqliteChecked >= 100)
        hasDuck <- isDuckDBAvailable
        when hasDuck $ do
            let duckScripts =
                    [ BSC.unlines
                        [ "CREATE TABLE t(x INT);"
                        , "CREATE TABLE n(y INT);"
                        , "INSERT INTO t VALUES (40);"
                        , "INSERT INTO t VALUES (2);"
                        , "INSERT INTO n VALUES (99);"
                        , "SELECT sum(x) AS answer FROM t;"
                        ]
                    , BSC.unlines
                        [ "CREATE TABLE t(x INT);"
                        , "INSERT INTO t VALUES (7) RETURNING x AS answer;"
                        , "CREATE TABLE ctas AS SELECT 1 AS z;"
                        , "SELECT 42 AS answer WHERE FALSE;"
                        , "CREATE TABLE tail(a INT);"
                        ]
                    ]
            duckCheckedRef <- newIORef (0 :: Int)
            forM_ duckScripts $ \sql -> do
                duckRes <- generateDuckDBContractWithTrace defaultDuckDBCandidateTimeoutMicros sql
                (contract, trace) <- either (assertFailure . show) pure duckRes
                parsed <- parseSQL (defaultParseOptions DuckDB) sql >>= either (assertFailure . show) pure
                let units = parsedUnits parsed
                    stmts = map unitStatement units
                    cert = buildEarlyRejectCertifier sql units (Just trace)
                    ids = map (unStatementId . stmtId) stmts
                forM_ (allSubsets ids) $ \kept -> do
                    let cand = cutRemovedStatementInts sql stmts kept
                    case checkEarlyRejection cert kept cand of
                        Nothing -> pure ()
                        Just reason -> do
                            modifyIORef' duckCheckedRef (+ 1)
                            outcome <- duckdbContractOracle contract cand
                            assertEqual
                                ( "Soundness violation on DuckDB ("
                                    ++ show reason
                                    ++ ") for kept="
                                    ++ show (IntSet.toList kept)
                                    ++ " sql="
                                    ++ BSC.unpack cand
                                )
                                OracleRejected
                                outcome
            duckChecked <- readIORef duckCheckedRef
            assertBool
                ("expected >= 30 DuckDB certified subsets validated, got " ++ show duckChecked)
                (duckChecked >= 30)
    , testCase "Early rejection preserves exact reduced SQL and cuts physical oracle calls on SQLite" $ do
        let inputRow i =
                "INSERT INTO t VALUES ("
                    ++ show (i :: Int)
                    ++ "); CREATE TABLE noise_"
                    ++ show i
                    ++ "(x INT);"
            sql =
                BSC.pack $
                    unlines
                        ( "CREATE TABLE t(x INT);"
                            : map inputRow [1 .. 16]
                            ++ ["SELECT sum(x) AS answer FROM t;"]
                        )
            runWith earlyReject = do
                (remember, lookupTrace) <- newResultTraceTracker True
                prepared <- prepareResultOracleWithTrace remember SQLite 500000 sql
                (_, observer) <- either (assertFailure . show) pure prepared
                callsRef <- newIORef (0 :: Int)
                certHitsRef <- newIORef (0 :: Int)
                let logger =
                        Logger
                            defaultLogConfig
                            ( \_ _ msg fields ->
                                when (msg == "Early rejection certificate summary") $ do
                                    let getInt k = case lookup k fields of
                                            Just (Number n) -> round n
                                            _ -> (0 :: Int)
                                    modifyIORef' certHitsRef (+ (getInt "exact_prefix_rejects" + getInt "no_result_rejects"))
                            )
                            (pure ())
                    decide cand = do
                        modifyIORef' callsRef (+ 1)
                        details <- observer (pure ()) cand
                        pure (observationOutcome (detailedObservation details))
                pool <- outcomePool decide
                let cfg =
                        defaultReductionConfig
                            { reductionDialect = SQLite
                            , reductionEarlyReject = earlyReject
                            }
                ReductionResult out _ <-
                    reduceSQLWithResultTrace lookupTrace (const (pure ())) cfg logger pool sql
                calls <- readIORef callsRef
                hits <- readIORef certHitsRef
                pure (out, calls, hits)
        (outOff, callsOff, hitsOff) <- runWith False
        (outOn, callsOn, hitsOn) <- runWith True
        assertEqual "early rejection must preserve exact reduced SQL" outOff outOn
        assertEqual "disabled early rejection must record zero certificate hits" 0 hitsOff
        assertBool ("expected certificate hits > 0, got " ++ show hitsOn) (hitsOn > 0)
        assertBool
            ( "expected strictly fewer oracle calls with early rejection: on="
                ++ show callsOn
                ++ " off="
                ++ show callsOff
            )
            (callsOn < callsOff)
    , testCase
        "Synthetic non-result oracle via reduceSQLWithResultSlicing never activates early rejection"
        $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE noise(x INT);"
                        , "CREATE TABLE keep(x INT);"
                        , "INSERT INTO keep VALUES (1);"
                        ]
            pool <-
                predicatePool
                    (\cand -> BSC.isInfixOf "CREATE TABLE keep" cand && BSC.isInfixOf "INSERT INTO keep" cand)
            let locate _ = pure (Just (ResultStatement 3 2))
                cfg =
                    defaultReductionConfig
                        { reductionDialect = SQLite
                        , reductionEarlyReject = True
                        }
            ReductionResult out _ <-
                reduceSQLWithResultSlicing locate (const (pure ())) cfg nullLogger pool sql
            assertBool "must preserve keep table" (BSC.isInfixOf "CREATE TABLE keep" out)
            assertBool "must preserve keep insert" (BSC.isInfixOf "INSERT INTO keep" out)
            assertBool "must remove noise table" (not (BSC.isInfixOf "noise" out))
    , testCase
        "Column-arity certificate skips outer projection deletions while preserving inner CTE pruning"
        $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t(a INT, b INT, c INT);"
                        , "INSERT INTO t VALUES (1, 2, 3);"
                        , "WITH cte AS (SELECT a, b, c, 99 AS unused_cte_col FROM t) SELECT a, b, c FROM cte;"
                        ]
                runWith earlyReject = do
                    (remember, lookupTrace) <- newResultTraceTracker True
                    prepared <- prepareResultOracleWithTrace remember SQLite 500000 sql
                    (_, observer) <- either (assertFailure . show) pure prepared
                    callsRef <- newIORef (0 :: Int)
                    arityHitsRef <- newIORef (0 :: Int)
                    let logger =
                            Logger
                                defaultLogConfig
                                ( \_ _ msg fields ->
                                    when (msg == "Early rejection certificate summary") $ do
                                        let getInt k = case lookup k fields of
                                                Just (Number n) -> round n
                                                _ -> (0 :: Int)
                                        modifyIORef' arityHitsRef (+ getInt "column_arity_rejects")
                                )
                                (pure ())
                        decide cand = do
                            modifyIORef' callsRef (+ 1)
                            details <- observer (pure ()) cand
                            pure (observationOutcome (detailedObservation details))
                    pool <- outcomePool decide
                    let cfg =
                            defaultReductionConfig
                                { reductionDialect = SQLite
                                , reductionIntraStatement = True
                                , reductionEarlyReject = earlyReject
                                }
                    ReductionResult out _ <-
                        reduceSQLWithResultTrace lookupTrace (const (pure ())) cfg logger pool sql
                    calls <- readIORef callsRef
                    hits <- readIORef arityHitsRef
                    pure (out, calls, hits)
            (outOff, callsOff, hitsOff) <- runWith False
            (outOn, callsOn, hitsOn) <- runWith True
            assertEqual "column-arity certificate preserves exact reduced SQL" outOff outOn
            assertEqual "disabled early rejection records zero arity hits" 0 hitsOff
            assertBool ("expected column_arity_rejects > 0, got " ++ show hitsOn) (hitsOn > 0)
            assertBool "inner CTE still pruned unused column" (not ("unused_cte_col" `BSC.isInfixOf` outOn))
            assertBool
                ("expected strictly fewer oracle calls: on=" ++ show callsOn ++ " off=" ++ show callsOff)
                (callsOn < callsOff)
    , testCase "Deferred literal leaf span-shift matches full syntax reparse exactly on multi-row VALUES" $ do
        let rows =
                [ "(" <> BSC.pack (show i) <> ", 'payload_string_value_" <> BSC.pack (show i) <> "')"
                | i <- [1 .. 24 :: Int]
                ]
            sql =
                BSC.unlines
                    [ "CREATE TABLE t(id INT, label TEXT);"
                    , "INSERT INTO t VALUES " <> BS.intercalate ", " rows <> ";"
                    , "SELECT sum(id), count(*) FROM t;"
                    ]
        contractRes <- generateSqliteContract sql
        contract <- either (assertFailure . show) pure contractRes
        let runWith fullReparse = do
                pool <- outcomePool (sqliteContractOracle contract)
                let cfg =
                        defaultReductionConfig
                            { reductionDialect = SQLite
                            , reductionIntraStatement = True
                            , reductionFullSyntaxReparse = fullReparse
                            }
                reduceSQLWithConfig cfg nullLogger pool sql
        fullOut <- runWith True
        fastOut <- runWith False
        assertEqual "deferred literal leaf shift matches full reparse byte-for-byte" fullOut fastOut
        verdict <- sqliteContractOracle contract fastOut
        assertEqual "deferred literal output preserves SQLite contract" OracleAccepted verdict
    ]

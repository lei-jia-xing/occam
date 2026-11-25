module DependencyChecks.NativeSliceChecks (nativeSliceChecks) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Occam.Core (OracleOutcome (..), StatementId (..))
import Occam.Dependency (analyzeStatements, cascadeWrites)
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
    ParsedSQL (..),
    SQLDialect (..),
    StatementUnit (..),
    defaultParseOptions,
    parseSQL,
 )
import Occam.Reducer (cutRemovedStatementIds)
import Occam.Slicer (backwardSlice)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

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

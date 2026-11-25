module DuckDBOracleChecks (tests) where

import Data.Aeson (Value (..), eitherDecodeStrict', encode)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Occam.Core (OracleOutcome (..), ResultStatement (..))
import Occam.Oracle (OracleObservation (..))
import Occam.Oracle.DuckDB (
    DuckDBContract (..),
    duckdbContractObservation,
    duckdbContractObservationTimeout,
    duckdbContractOracle,
    executeDuckDBCandidate,
    generateDuckDBContract,
    generateDuckDBContractWithResultStatement,
    generateDuckDBContractWithTrace,
    isDuckDBAvailable,
    prepareDuckDBContractObserverWithReplay,
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
        "In-process DuckDB Oracle"
        [ testCase "executeDuckDBCandidate executes query, handles CREATE MACRO and returns rowset" $ do
            available <- isDuckDBAvailable
            assertBool "DuckDB library must be detected and available" available
            let sql =
                    "CREATE MACRO add_ten(x) AS x + 10;\n\
                    \CREATE TABLE items (id INT, name TEXT, active BOOLEAN);\n\
                    \INSERT INTO items VALUES (add_ten(32), 'widget', true);\n\
                    \SELECT id, name, active FROM items;"
            res <- executeDuckDBCandidate sql
            assertEqual "rowset matches" (Right [[Number 42, String "widget", Bool True]]) res
        , testCase "duckdbContractOracle accepts matching and rejects invalid SQL" $ do
            available <- isDuckDBAvailable
            assertBool "DuckDB library must be detected and available" available
            let sql =
                    "CREATE TABLE items (id INT, name TEXT);\n\
                    \INSERT INTO items VALUES (1, 'widget');\n\
                    \SELECT * FROM items;"
            contractRes <- generateDuckDBContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    let oracle = duckdbContractOracle contract
                    o1 <- oracle sql
                    assertEqual "original accepts" OracleAccepted o1
                    o2 <- oracle "SYNTAX ERROR;"
                    assertEqual "syntax error rejects" OracleRejected o2
                    o3 <- oracle "SELECT 99;"
                    assertEqual "mismatched query rejects" OracleRejected o3
                    o4 <- oracle ""
                    assertEqual "empty candidate rejects" OracleRejected o4
        , testCase "DuckDB preserves an empty final result and its column names" $ do
            let sql = "SELECT 1 AS earlier; SELECT 2 AS answer WHERE FALSE;"
            contractRes <- generateDuckDBContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    let oracle = duckdbContractOracle contract
                    empty <- oracle "SELECT 2 AS answer WHERE FALSE;"
                    dropped <- oracle "SELECT 1 AS earlier;"
                    renamed <- oracle "SELECT 2 AS other WHERE FALSE;"
                    noResult <- oracle "CREATE TABLE x (a INT);"
                    assertEqual "earlier result is irrelevant" OracleAccepted empty
                    assertEqual "dropping final result changes the observation" OracleRejected dropped
                    assertEqual "renaming an empty result column changes the observation" OracleRejected renamed
                    assertEqual "a script without a result set is distinct" OracleRejected noResult
        , testCase "DuckDB compares decimals exactly and preserves precision and scale" $ do
            let source =
                    "SELECT 1.23::DECIMAL(4,2) AS small, -98765.4321::DECIMAL(9,4) AS medium, "
                        <> "123456789.123456789::DECIMAL(18,9) AS large, "
                        <> "-123456789012345678901234567890.12345678::DECIMAL(38,8) AS wide;"
                changed =
                    "SELECT 1.23::DECIMAL(4,2) AS small, -98765.4321::DECIMAL(9,4) AS medium, "
                        <> "123456789.123456789::DECIMAL(18,9) AS large, "
                        <> "-123456789012345678901234567890.12345679::DECIMAL(38,8) AS wide;"
            contract <- generateDuckDBContract source >>= either (assertFailure . T.unpack) pure
            assertEqual
                "contract survives JSON round trip"
                (Right contract)
                (eitherDecodeStrict' (BSL.toStrict (encode contract)))
            exact <- duckdbContractOracle contract source
            different <- duckdbContractOracle contract changed
            assertEqual "all four integer storage widths match" OracleAccepted exact
            assertEqual "one unit of the last place differs" OracleRejected different
            empty <-
                generateDuckDBContract "SELECT NULL::DECIMAL(4,2) AS n WHERE FALSE;"
                    >>= either (assertFailure . T.unpack) pure
            wrongWidth <- duckdbContractOracle empty "SELECT NULL::DECIMAL(5,2) AS n WHERE FALSE;"
            wrongScale <- duckdbContractOracle empty "SELECT NULL::DECIMAL(4,1) AS n WHERE FALSE;"
            assertEqual "empty columns retain precision" OracleRejected wrongWidth
            assertEqual "empty columns retain scale" OracleRejected wrongScale
        , testCase "DuckDB decodes only the last result set and still executes trailing statements" $ do
            let sql = "SELECT [1, 2] AS ignored; SELECT 42 AS answer; CREATE TABLE tail (x INT);"
            result <- generateDuckDBContractWithResultStatement 1000000 sql
            case result of
                Left err -> assertFailure (T.unpack err)
                Right (contract, location) -> do
                    assertEqual "result statement precedes trailing DDL" (ResultStatement 3 1) location
                    matched <- duckdbContractOracle contract "SELECT 42 AS answer;"
                    failed <- duckdbContractOracle contract "SELECT 42 AS answer; DROP TABLE nonexistent;"
                    changedType <- duckdbContractOracle contract "SELECT [1, 2] AS answer;"
                    assertEqual
                        "an unsupported type cannot match the supported contract type"
                        OracleRejected
                        changedType
                    assertEqual "unsupported intermediate types do not affect matching" OracleAccepted matched
                    assertEqual "trailing execution errors reject the candidate" OracleRejected failed
            unsupported <- generateDuckDBContract "SELECT 42 AS answer; SELECT [1, 2] AS answer;"
            case unsupported of
                Left err ->
                    assertBool
                        "the final result still needs lossless decoding"
                        ("Unsupported DuckDB result column types" `T.isInfixOf` err)
                Right _ -> assertFailure "accepted an unsupported final result type"
        , testCase "DuckDB preserves typed rows and string bytes after a NUL" $ do
            result <- generateDuckDBContract "SELECT 'a' || chr(0) || 'b' AS s, NULL::BIGINT AS v;"
            case result of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    changed <- duckdbContractOracle contract "SELECT 'a' || chr(0) || 'c' AS s, NULL::BIGINT AS v;"
                    retyped <- duckdbContractOracle contract "SELECT 'a' || chr(0) || 'b' AS s, NULL::INTEGER AS v;"
                    assertEqual "preserve bytes following the NUL" OracleRejected changed
                    assertEqual "preserve column types for NULL values" OracleRejected retyped
        , testCase "DuckDB native matcher compares exact typed values across chunks" $ do
            let query suffix =
                    "SELECT i, i % 2 = 0 AS p, (i % 128 - 64)::TINYINT AS s, "
                        <> "170141183460469231731687303715884105727::HUGEINT AS h, "
                        <> "'340282366920938463463374607431768211455'::UHUGEINT AS uh, "
                        <> "'18446744073709551615'::UBIGINT AS u, "
                        <> "'NaN'::FLOAT AS f, CAST('-0.0' AS DOUBLE) AS d, "
                        <> "from_hex('00FF') AS b, 'a long string' || chr(0) || i AS str, "
                        <> "CASE WHEN i % 65 = 0 THEN NULL ELSE i END AS nullable "
                        <> "FROM range(2100) t(i) "
                        <> suffix
            contract <- generateDuckDBContractWithResultStatement 2000000 (query "ORDER BY i;")
            case contract of
                Left err -> assertFailure (T.unpack err)
                Right (expected, _) -> do
                    let observe = duckdbContractObservationTimeout 2000000 expected
                    assertEqual "original rows match" (OracleFinished ExitSuccess) =<< observe (query "ORDER BY i;")
                    assertEqual "row order is part of the contract" (OracleFinished (ExitFailure 1))
                        =<< observe (query "ORDER BY i DESC;")
                    assertEqual "a missing row in the last chunk differs" (OracleFinished (ExitFailure 1))
                        =<< observe (query "ORDER BY i LIMIT 2099;")
        , testCase "DuckDB reuses a deterministic prefix without changing traces or failure boundaries" $ do
            let prefix =
                    BSC.unlines $
                        "CREATE TABLE replay_t (a INT);"
                            : ["INSERT INTO replay_t VALUES (" <> BSC.pack (show i) <> ");" | i <- [1 .. 12 :: Int]]
                original = prefix <> "SELECT SUM(a) AS total_a FROM replay_t;"
                equivalent = prefix <> "SELECT SUM(a) AS total_a\nFROM replay_t;"
                wrong = prefix <> "SELECT COUNT(a) AS total_a FROM replay_t;"
                failing = prefix <> "SELECT SUM(absent_a) AS total_a FROM replay_t;"
            contract <- generateDuckDBContractWithTrace 2000000 original
            case contract of
                Left err -> assertFailure (T.unpack err)
                Right (expected, originalTrace) -> do
                    failures <- newIORef []
                    observe <-
                        prepareDuckDBContractObserverWithReplay
                            2000000
                            expected
                            original
                            (\p -> modifyIORef' failures (p :))
                    assertEqual
                        "first probe matches with complete source ordinals"
                        (OracleFinished ExitSuccess, Just originalTrace)
                        =<< observe original
                    assertEqual
                        "exact prefix restores the same result and ordinals"
                        (OracleFinished ExitSuccess, Just originalTrace)
                        =<< observe equivalent
                    assertEqual "a different aggregate rejects" (OracleFinished (ExitFailure 1), Nothing)
                        =<< observe wrong
                    assertEqual "missing column rejects" (OracleFinished (ExitFailure 1), Nothing)
                        =<< observe (failing <> " SELECT 7 AS another;")
                    assertEqual "failure evidence ends at the failing statement" [failing] =<< readIORef failures
                    assertEqual "a mutating suffix uses a fresh database" (OracleFinished (ExitFailure 1), Nothing)
                        =<< observe (prefix <> "INSERT INTO replay_t VALUES (100); SELECT SUM(a) AS total_a FROM replay_t;")
                    assertEqual "the checkpoint remains pristine" (OracleFinished ExitSuccess, Just originalTrace)
                        =<< observe original
        , testCase "DuckDB interrupts a long-running inline query" $ do
            result <- generateDuckDBContract "SELECT 1 AS a;"
            case result of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    observed <-
                        duckdbContractObservationTimeout
                            20000
                            contract
                            "SELECT sum(sin(i)) AS a FROM range(1000000000) t(i);"
                    assertEqual "candidate reaches its per-probe timeout" OracleTimedOut observed
        , testCase "duckdbContractObservation rejects candidate SQL errors" $ do
            available <- isDuckDBAvailable
            assertBool "DuckDB library must be detected and available" available
            let sql = "CREATE TABLE t (a INT);\nSELECT * FROM t;"
            contractRes <- generateDuckDBContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    observation <- duckdbContractObservation contract "SYNTAX ERROR;"
                    assertEqual "SQL error is a definite rejection" (OracleFinished (ExitFailure 1)) observation
        , testCase "in-process DuckDB reduction removes dead noise table and dead macro" $ do
            available <- isDuckDBAvailable
            assertBool "DuckDB library must be detected and available" available
            let sql =
                    "CREATE MACRO unused_macro(x) AS x * 2;\n\
                    \CREATE TABLE noise (val INT);\n\
                    \INSERT INTO noise VALUES (999);\n\
                    \CREATE MACRO live_macro(x) AS x + 1;\n\
                    \CREATE TABLE target (a INT);\n\
                    \INSERT INTO target VALUES (live_macro(41));\n\
                    \SELECT * FROM target;"
            contractRes <- generateDuckDBContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    let oracle = duckdbContractOracle contract
                    reduced <- withPredicatePool 1 True (fmap (== OracleAccepted) . oracle) $ \pool ->
                        statementAudit DuckDB pool sql
                    assertBool "noise removed" (not (BSC.isInfixOf "noise" reduced))
                    assertBool "unused_macro removed" (not (BSC.isInfixOf "unused_macro" reduced))
                    assertBool "target retained" (BSC.isInfixOf "target" reduced)
                    oReduced <- oracle reduced
                    assertEqual "reduced SQL satisfies oracle" OracleAccepted oReduced
        , testCase "DuckDB table generator generate_series works in contract execution" $ do
            available <- isDuckDBAvailable
            assertBool "DuckDB library must be detected and available" available
            let sql = "SELECT sum(v) FROM generate_series(1, 5) AS t(v);"
            contractRes <- generateDuckDBContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    assertEqual "contract rowset matches" [[Number 15]] (contractExpectedRows contract)
                    let oracle = duckdbContractOracle contract
                    o1 <- oracle sql
                    assertEqual "original accepts" OracleAccepted o1
        ]

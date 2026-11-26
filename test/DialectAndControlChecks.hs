module DialectAndControlChecks (
    phase3Tests,
    phase7Tests,
    phase8Tests,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Occam.Core (Statement (stmtRaw), StatementId (..), oracleAccepts)
import Occam.Dependency (DependencyGraph, analyzeStatements)
import Occam.Logger (nullLogger)
import Occam.Parser (
    CaseFolding (..),
    DialectFeature (..),
    DialectProfile (..),
    IdentifierProfile (..),
    LexicalProfile (..),
    ParsedSQL (..),
    SQLDialect (..),
    StatementUnit (..),
    allDialectProfiles,
    applyCaseFolding,
    defaultParseOptions,
    dialectHasFeature,
    dialectProfile,
    parseSQL,
 )
import Occam.Pool (poolOracle)
import Occam.Reducer (
    ReductionConfig (..),
    defaultReductionConfigFor,
    isValidClosure,
    reduceSQLWithConfig,
    statementAudit,
    statementDdmin,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import TestSupport (testPredicatePool)

withGraph :: SQLDialect -> ByteString -> (DependencyGraph -> IO ()) -> IO ()
withGraph dialect source action = do
    result <- parseSQL (defaultParseOptions dialect) source
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right parsed -> action (analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed))

phase3Tests :: TestTree
phase3Tests =
    testGroup
        "Phase 3: Hierarchical Reducer"
        [ testCase "Closure validity checking" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0 ORDER BY a LIMIT 1;"
                        ]
            withGraph SQLite script $ \graph -> do
                assertBool
                    "table, data, index, and query form a valid conservative closure"
                    (isValidClosure graph (Set.fromList [StatementId 1, StatementId 3, StatementId 4, StatementId 5]))
                assertBool
                    "omitting the table definition leaves a dangling dependency"
                    (not (isValidClosure graph (Set.fromList [StatementId 3, StatementId 4, StatementId 5])))
                assertBool
                    "omitting index state leaves a dangling dependency"
                    (not (isValidClosure graph (Set.fromList [StatementId 1, StatementId 3, StatementId 5])))
        , testCase "End-to-end multi-statement reduction" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                        ]
                predicate sql = do
                    let hasT1 = BSC.isInfixOf "CREATE TABLE t1" sql
                        hasIns = BSC.isInfixOf "INSERT INTO t1" sql
                        hasSel = BSC.isInfixOf "SELECT" sql && BSC.isInfixOf "WHERE a > 0" sql
                    pure (hasT1 && hasIns && hasSel)
                config = (defaultReductionConfigFor SQLite) {reductionIntraStatement = True}
            pool <- testPredicatePool predicate
            reduced <- reduceSQLWithConfig config nullLogger pool script
            ok <- oracleAccepts (poolOracle pool) reduced
            assertBool "Reduced SQL failed to satisfy the crash oracle" ok
            assertBool "Table t2 was not pruned" (not (BSC.isInfixOf "t2" reduced))
            assertBool "Index i1 was not pruned" (not (BSC.isInfixOf "i1" reduced))
            assertBool "ORDER BY clause was not pruned" (not (BSC.isInfixOf "ORDER BY" reduced))
            assertBool "LIMIT clause was not pruned" (not (BSC.isInfixOf "LIMIT" reduced))
            assertBool "Expression 1 = 1 was not simplified" (not (BSC.isInfixOf "1 = 1" reduced))
            assertBool "Operator AND was not simplified" (not (BSC.isInfixOf "AND" reduced))
            assertBool "Missing required CREATE TABLE t1" (BSC.isInfixOf "CREATE TABLE t1" reduced)
            assertBool "Missing required INSERT INTO t1" (BSC.isInfixOf "INSERT INTO t1" reduced)
            assertBool "Missing required WHERE a > 0" (BSC.isInfixOf "WHERE a > 0" reduced)
        ]

phase7Tests :: TestTree
phase7Tests =
    testGroup
        "Selected frontend dialects"
        ( map (\dialect -> check dialect "SELECT 1" "SELECT 2") [minBound .. maxBound]
            ++ [ check PostgreSQL "SELECT $$a;b$$" "SELECT 2"
               , check
                    SQLite
                    "CREATE TRIGGER trg AFTER INSERT ON t BEGIN UPDATE t SET a = 1; UPDATE t SET a = 2; END"
                    "SELECT 2"
               , check MySQL "CREATE TABLE `t` (`c` CHAR(8), INDEX (`c`(4)))" "SELECT 2"
               , check MariaDB "CREATE TABLE `t` (`c` CHAR(8), INDEX (`c`(4)))" "SELECT 2"
               , check DuckDB "SELECT * FROM range(3) QUALIFY row_number() OVER () = 1" "SELECT 2"
               , check
                    ClickHouse
                    "CREATE TABLE t (a UInt32) ENGINE = MergeTree ORDER BY tuple()"
                    "SELECT 2"
               , testCase "DialectProfile covers all 6 active dialects and distinct capabilities" $ do
                    assertEqual "profile count" 6 (length allDialectProfiles)
                    assertEqual
                        "every Enum dialect maps to itself"
                        [minBound .. maxBound]
                        (map (dialectId . dialectProfile) [minBound .. maxBound])
                    assertBool
                        "PostgreSQL enables dollar quoting"
                        (lexDollarQuoting (dialectLexical (dialectProfile PostgreSQL)))
                    assertBool
                        "MariaDB has sequence DDL while MySQL does not"
                        ( dialectHasFeature MariaDB FeatureSequenceDDL
                            && not (dialectHasFeature MySQL FeatureSequenceDDL)
                        )
                    assertEqual
                        "SQLite folds ASCII only"
                        "t_É"
                        (applyCaseFolding (identUnquotedFolding (dialectIdentifiers (dialectProfile SQLite))) "T_É")
                    assertEqual
                        "ClickHouse preserves identifier case"
                        FoldPreserve
                        (identUnquotedFolding (dialectIdentifiers (dialectProfile ClickHouse)))
               ]
        )
    where
        check dialect first second =
            testCase (show dialect ++ ": " ++ BSC.unpack first) $ do
                let source = first <> "; " <> second <> ";"
                result <- parseSQL (defaultParseOptions dialect) source
                case result of
                    Left err -> assertFailure (show err)
                    Right parsed -> do
                        let units = parsedUnits parsed
                        assertEqual "statement text and boundaries" [first, second] (map (stmtRaw . unitStatement) units)
                        assertEqual "parse errors" [] (mapMaybe unitParseError units)

phase8Tests :: TestTree
phase8Tests =
    testGroup
        "Internal statement controls"
        [ testCase "statement deletion removes dead statements and satisfies oracle" $
            mapM_
                ( \(name, reduce) -> do
                    let script =
                            BSC.unlines
                                [ "CREATE TABLE t1 (a INT, b TEXT);"
                                , "CREATE TABLE t2 (c INT);"
                                , "INSERT INTO t1 VALUES (1, 'x');"
                                , "CREATE TABLE t3 (d INT);"
                                , "SELECT * FROM t1 WHERE a > 0;"
                                ]
                        predicate sql = do
                            let hasT1 = BSC.isInfixOf "CREATE TABLE t1" sql
                                hasIns = BSC.isInfixOf "INSERT INTO t1" sql
                                hasSel = BSC.isInfixOf "SELECT * FROM t1 WHERE a > 0" sql
                            pure (hasT1 && hasIns && hasSel)
                    pool <- testPredicatePool predicate
                    reduced <- reduce SQLite pool script
                    accepted <- oracleAccepts (poolOracle pool) reduced
                    assertBool (name ++ " must satisfy oracle") accepted
                    assertBool (name ++ " should remove dead table t2") (not (BSC.isInfixOf "t2" reduced))
                    assertBool (name ++ " should remove dead table t3") (not (BSC.isInfixOf "t3" reduced))
                    assertBool (name ++ " must retain t1, its insert, and its query") $
                        all (`BSC.isInfixOf` reduced) ["CREATE TABLE t1", "INSERT INTO t1", "SELECT * FROM t1 WHERE a > 0"]
                )
                [ ("statement audit", statementAudit)
                , ("statement ddmin", statementDdmin)
                ]
        ]

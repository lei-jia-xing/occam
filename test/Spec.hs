{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Baseline (runNaiveDdmin, runSequentialDrop)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Graph qualified as Graph
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (subsequences)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import DependencyChecks qualified
import EvaluationChecks qualified
import Fuzz (generativeFuzzTests)
import MutationalFuzz (mutationalFuzzTests)
import Occam.Cache (CacheStats (..), memoizeOracle, memoizeOracleWith)
import Occam.Core (
    ActionTarget (..),
    DependencyType (..),
    ObjectAction (..),
    ObjectKind (..),
    ObjectName (..),
    StatementId (..),
    targetObjectName,
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements, buildGraph)
import Occam.Logger (nullLogger)
import Occam.Parser (ParsedSQL (..), SQLDialect (..), parseSQL)
import Occam.Reducer
import Occam.Slicer
import OracleChecks qualified
import ReductionChecks qualified
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
 )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck (
    Arbitrary (..),
    Gen,
    Property,
    chooseInt,
    counterexample,
    elements,
    oneof,
    resize,
    sublistOf,
    vectorOf,
 )
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Occam Test Suite"
        [ phase1Tests
        , phase2Tests
        , phase3Tests
        , phase5Tests
        , phase6Tests
        , phase7Tests
        , phase8Tests
        , DependencyChecks.tests
        , EvaluationChecks.tests
        , OracleChecks.tests
        , ReductionChecks.tests
        , propertyTests
        , generativeFuzzTests
        , mutationalFuzzTests
        ]

-- ----------------------------------------------------------------------------
-- Phase 1: SQL frontend
-- ----------------------------------------------------------------------------

phase1Tests :: TestTree
phase1Tests =
    testGroup
        "SQL frontend"
        [ testCase "same-line UTF-8 projection surgery preserves retained bytes and CRLF" $ do
            let kept = TE.encodeUtf8 "SELECT 'café' AS kept, a FROM"
                sql = kept <> " (SELECT 42 AS a,99 AS b);\r\n"
                expected = kept <> " (SELECT 42 AS a);\r\n"
                oracle = oracleFromPredicate $ \candidate -> pure (BS.isInfixOf kept candidate && BSC.isInfixOf "42 AS a" candidate)
                config =
                    defaultReductionConfig
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionIntraStatement = True
                        , reductionUnrestricted = False
                        , reductionDialect = GenericSQL
                        }
            reduced <- reduceSQLWithConfig config nullLogger oracle sql
            assertEqual "unused projection removal leaves UTF-8 SQL and CRLF untouched" expected reduced
        ]

-- ----------------------------------------------------------------------------
-- Phase 2: Dependency Analysis
-- ----------------------------------------------------------------------------

phase2Tests :: TestTree
phase2Tests =
    testGroup
        "Phase 2: Dependency Analysis"
        [ testCase "Construct dependency DAG and verify non-dependency of irrelevant tables" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0;"
                        ]
            withGraph GenericSQL script $ \graph -> do
                let depsS3 = Map.findWithDefault Set.empty (StatementId 3) (graphEdges graph)
                    depsS5 = Map.findWithDefault Set.empty (StatementId 5) (graphEdges graph)
                assertBool "s3 should not depend on s2 (t2)" (not (Set.member (StatementId 2) depsS3))
                assertBool "s5 should not depend on s2 (t2)" (not (Set.member (StatementId 2) depsS5))
        , testCase "Reads base tables through CTEs, subqueries, and UNION branches" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE base_a (id INT);"
                        , "CREATE TABLE base_b (id INT);"
                        , "CREATE TABLE base_c (id INT);"
                        , "WITH cte_union AS ("
                        , "  SELECT id FROM base_a"
                        , "  UNION ALL"
                        , "  SELECT id FROM base_b"
                        , ")"
                        , "SELECT id FROM cte_union"
                        , "WHERE EXISTS (SELECT 1 FROM base_c WHERE base_c.id = cte_union.id);"
                        ]
            withGraph GenericSQL script $ \graph -> do
                let queryId = StatementId 4
                    queryActions = Map.findWithDefault [] queryId (graphActions graph)
                    uses =
                        Set.fromList
                            [ name
                            | action <- queryActions
                            , actionType action == UseDep
                            , actionKind action == TableObject
                            , Just name <- [targetObjectName (actionTarget action)]
                            ]
                    deps = Map.findWithDefault Set.empty queryId (graphEdges graph)
                assertEqual
                    "Only external base tables are query reads"
                    (Set.fromList [ObjectName ["base_a"], ObjectName ["base_b"], ObjectName ["base_c"]])
                    uses
                assertEqual
                    "Query depends on each referenced base table definition"
                    (Set.fromList (map StatementId [1, 2, 3]))
                    deps
        , testCase "Backward slicing from crashing statement" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0;"
                        ]
            withGraph GenericSQL script $ \graph -> do
                let slice = backwardSlice (StatementId 5) graph
                assertBool
                    "CRITICAL: Statement 2 (t2) should have been pruned by slice"
                    (not (Set.member (StatementId 2) slice))
                assertBool "Statement 1 (CREATE TABLE t1) must be in slice" (Set.member (StatementId 1) slice)
                assertBool "Statement 5 (Crash trigger) must be in slice" (Set.member (StatementId 5) slice)
        ]

-- ----------------------------------------------------------------------------
-- Phase 3: Hierarchical Reducer
-- ----------------------------------------------------------------------------

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
            withGraph GenericSQL script $ \graph -> do
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
                oracle = oracleFromPredicate $ \sql -> do
                    let hasT1 = BSC.isInfixOf "CREATE TABLE t1" sql
                        hasIns = BSC.isInfixOf "INSERT INTO t1" sql
                        hasSel = BSC.isInfixOf "SELECT" sql && BSC.isInfixOf "WHERE a > 0" sql
                    pure (hasT1 && hasIns && hasSel)
            reduced <- reduceSQL GenericSQL oracle script
            ok <- oracleAccepts oracle reduced
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
        , testCase "Expression simplification unit test" $ do
            let complexExprSQL = "SELECT * FROM t1 WHERE (a + 10) > 0 AND 1 = 1;"
                exprOracle sql = pure (BSC.isInfixOf "a" sql && BSC.isInfixOf "> 0" sql)
            simplifiedExpr <- simplifyExpressions GenericSQL (oracleFromPredicate exprOracle) complexExprSQL
            assertBool "Failed to simplify 1 = 1" (not (BSC.isInfixOf "1 = 1" simplifiedExpr))
            assertBool "Failed to simplify + 10" (not (BSC.isInfixOf "10" simplifiedExpr))
        ]

-- ----------------------------------------------------------------------------
-- Phase 5: CLI
-- ----------------------------------------------------------------------------

getOccamBinary :: IO FilePath
getOccamBinary = do
    mPath <- findExecutable "occam"
    case mPath of
        Just p -> pure p
        Nothing -> do
            let localPath = "dist-newstyle/build/x86_64-linux/ghc-9.10.3/occam-0.1.0.0/x/occam/build/occam/occam"
            exists <- doesFileExist localPath
            if exists
                then pure localPath
                else pure "cabal"

runOccamCLI :: [String] -> IO (ExitCode, String, String)
runOccamCLI cliArgs = do
    bin <- getOccamBinary
    if bin == "cabal"
        then readProcessWithExitCode "cabal" (["run", "-v0", "occam", "--"] ++ cliArgs) ""
        else readProcessWithExitCode bin cliArgs ""

getOccamEvalBinary :: IO FilePath
getOccamEvalBinary = do
    mPath <- findExecutable "occam-eval"
    case mPath of
        Just p -> pure p
        Nothing -> do
            let localPath =
                    "dist-newstyle/build/x86_64-linux/ghc-9.10.3/occam-0.1.0.0/x/occam-eval/build/occam-eval/occam-eval"
            exists <- doesFileExist localPath
            if exists
                then pure localPath
                else pure "cabal"

runOccamEvalCLI :: [String] -> IO (ExitCode, String, String)
runOccamEvalCLI cliArgs = do
    bin <- getOccamEvalBinary
    if bin == "cabal"
        then readProcessWithExitCode "cabal" (["run", "-v0", "occam-eval", "--"] ++ cliArgs) ""
        else readProcessWithExitCode bin cliArgs ""

withCLIInput :: BS.ByteString -> (FilePath -> FilePath -> IO a) -> IO a
withCLIInput sql action =
    bracket create cleanup $ \input -> do
        BS.writeFile input sql
        action input (input ++ ".out")
    where
        create = do
            directory <- getTemporaryDirectory
            (path, handle) <- openTempFile directory "occam-cli-"
            hClose handle
            pure path
        cleanup path = do
            removeFile path
            exists <- doesDirectoryExist (path ++ ".out")
            when exists (removeDirectoryRecursive (path ++ ".out"))

phase5Tests :: TestTree
phase5Tests =
    testGroup
        "CLI predicate contract"
        [ testCase "CLI and evaluator refuse rejected or signaled initial predicates" $
            mapM_
                ( \(runner, extra) ->
                    mapM_
                        ( \command -> withCLIInput "SELECT 1;" $ \input outDir -> do
                            (code, _, _) <- runner ([input, "--oracle", command, "--out", outDir, "--quiet"] ++ extra)
                            assertBool ("Noninteresting input was accepted: " ++ command) (code /= ExitSuccess)
                            exists <- doesDirectoryExist outDir
                            assertBool "Reduction started before the initial predicate accepted" (not exists)
                        )
                        ["false", "exit 139", "kill -TERM $$"]
                )
                [(runOccamCLI, []), (runOccamEvalCLI, ["--baseline", "2"])]
        , testCase "CLI and evaluator shrink SQL while preserving the supplied predicate" $ do
            let initialSQL =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                        ]
                predicate =
                    "python3 -c \"import sys; s=sys.stdin.read(); sys.exit(0 if 'WHERE a > 0' in s and 'CREATE TABLE t1' in s and 'INSERT INTO t1' in s else 1)\""
            mapM_
                ( \(runner, extra) -> withCLIInput initialSQL $ \input outDir -> do
                    (code, _, err) <- runner ([input, "--oracle", predicate, "--out", outDir, "--quiet"] ++ extra)
                    assertEqual ("Reduction failed: " ++ err) ExitSuccess code
                    reduced <- BSC.readFile (outDir </> "minimal.sql")
                    (verified, _, _) <- readProcessWithExitCode "sh" ["-c", predicate] (BSC.unpack reduced)
                    assertEqual "Reduced SQL lost the target property" ExitSuccess verified
                    assertBool "No SQL reduction occurred" (BS.length reduced < BS.length initialSQL)
                )
                [(runOccamCLI, []), (runOccamEvalCLI, ["--baseline", "2"])]
        ]

-- ----------------------------------------------------------------------------
-- Phase 6: Candidate Memoization
-- ----------------------------------------------------------------------------

phase6Tests :: TestTree
phase6Tests =
    testGroup
        "Phase 6: Candidate Memoization"
        [ testCase "memoizeOracle caches repeated queries and tracks hits/misses" $ do
            callCountRef <- newIORef (0 :: Int)
            let mockOracle sql = do
                    modifyIORef' callCountRef (+ 1)
                    pure (BSC.isInfixOf "CRASH" sql)

            (cachedOracle, getStats) <- memoizeOracle (oracleFromPredicate mockOracle)

            -- 1. Initial queries (all misses)
            r1 <- oracleAccepts cachedOracle "SELECT * FROM t1 WHERE CRASH;"
            assertEqual "First query should return True" True r1
            r2 <- oracleAccepts cachedOracle "SELECT 1;"
            assertEqual "Second query should return False" False r2

            calls1 <- readIORef callCountRef
            assertEqual "Underlying oracle should be called 2 times" 2 calls1

            stats1 <- getStats
            assertEqual "cacheHits should be 0" 0 (cacheHits stats1)
            assertEqual "cacheMisses should be 2" 2 (cacheMisses stats1)

            -- 2. Repeated queries (all hits)
            r3 <- oracleAccepts cachedOracle "SELECT * FROM t1 WHERE CRASH;"
            assertEqual "Cached query 1 should return True" True r3
            r4 <- oracleAccepts cachedOracle "SELECT 1;"
            assertEqual "Cached query 2 should return False" False r4
            r5 <- oracleAccepts cachedOracle "SELECT * FROM t1 WHERE CRASH;"
            assertEqual "Cached query 1 (second time) should return True" True r5

            calls2 <- readIORef callCountRef
            assertEqual "Underlying oracle should NOT be re-invoked on cache hits" 2 calls2

            stats2 <- getStats
            assertEqual "cacheHits should be 3" 3 (cacheHits stats2)
            assertEqual "cacheMisses should remain 2" 2 (cacheMisses stats2)

            -- 3. Another new query (miss)
            r6 <- oracleAccepts cachedOracle "SELECT 2;"
            assertEqual "New query should return False" False r6

            calls3 <- readIORef callCountRef
            assertEqual "Underlying oracle should be called for new query" 3 calls3

            stats3 <- getStats
            assertEqual "cacheHits should be 3" 3 (cacheHits stats3)
            assertEqual "cacheMisses should be 3" 3 (cacheMisses stats3)
        , testCase "precomputed observations are served even without memoization" $ do
            callCountRef <- newIORef (0 :: Int)
            let mockOracle _ = do
                    modifyIORef' callCountRef (+ 1)
                    pure False
            (cachedOracle, getStats) <-
                memoizeOracleWith
                    False
                    (Map.singleton "SELECT 1;" True)
                    (const True)
                    mockOracle
            seeded <- cachedOracle "SELECT 1;"
            assertEqual "precomputed observation is returned" True seeded
            callsAfterSeed <- readIORef callCountRef
            assertEqual "precomputed observation does not invoke the oracle" 0 callsAfterSeed
            repeated <- cachedOracle "SELECT 1;"
            assertEqual "precomputed observation remains available" True repeated
            callsAfterRepeat <- readIORef callCountRef
            assertEqual "repeated precomputed lookup does not invoke the oracle" 0 callsAfterRepeat
            unseeded <- cachedOracle "SELECT 2;"
            assertEqual "unseeded query reaches the oracle" False unseeded
            callsAfterMiss <- readIORef callCountRef
            assertEqual "unseeded query is counted as a miss" 1 callsAfterMiss
            stats <- getStats
            assertEqual "precomputed lookups are not cache hits when disabled" 0 (cacheHits stats)
            assertEqual "only the real query is a miss" 1 (cacheMisses stats)
        , testCase "inconclusive outcomes are not cached as authoritative" $ do
            callCountRef <- newIORef (0 :: Int)
            let outcomeOracle _ = do
                    modifyIORef' callCountRef (+ 1)
                    pure OracleInconclusive
            (cachedOracle, getStats) <- memoizeOracle outcomeOracle
            first <- cachedOracle "SELECT 1;"
            assertEqual "first call stays inconclusive" OracleInconclusive first
            second <- cachedOracle "SELECT 1;"
            assertEqual "second call stays inconclusive" OracleInconclusive second
            calls <- readIORef callCountRef
            assertEqual "inconclusive result is re-observed" 2 calls
            stats <- getStats
            assertEqual "inconclusive results are misses" 2 (cacheMisses stats)
            assertEqual "inconclusive results are not hits" 0 (cacheHits stats)
        , testCase "Late-query slicing completes within a linear oracle allowance" $ do
            let required =
                    [ "CREATE TABLE needed (v INT);"
                    , "INSERT INTO needed VALUES (42);"
                    , "SELECT v FROM needed;"
                    ]
                noise = [BSC.pack ("CREATE TABLE unused_" ++ show n ++ " (v INT);") | n <- [1 .. 64 :: Int]]
                statements = take 2 required ++ noise ++ drop 2 required
                sql = BSC.unlines statements
                allowance = length statements + 16
            calls <- newIORef (0 :: Int)
            let check candidate = do
                    modifyIORef' calls (+ 1)
                    count <- readIORef calls
                    when (count > allowance) $
                        assertFailure
                            ( "Linear prefix search exceeded its oracle allowance: "
                                ++ show count
                                ++ " > "
                                ++ show allowance
                            )
                    pure (all (`BSC.isInfixOf` candidate) required)
            (oracle, _) <- memoizeOracle (oracleFromPredicate check)
            reduced <-
                reduceSQLWithConfig
                    defaultReductionConfig {reductionIntraStatement = False, reductionUnrestricted = False}
                    nullLogger
                    oracle
                    sql
            assertEqual "Unrelated setup is removed by a linear prefix scan" (BSC.unlines required) reduced
        ]

-- ----------------------------------------------------------------------------
-- Phase 7: Dialect Hardening
-- ----------------------------------------------------------------------------

phase7Tests :: TestTree
phase7Tests =
    testGroup
        "Phase 7: Dialect Hardening"
        [ testCase "PG schema-qualified table: public.orders" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE public.orders (id INT);"
                        , "SELECT * FROM public.orders;"
                        ]
            withGraph PostgreSQL script $ \graph -> do
                let depsS2 = Map.findWithDefault Set.empty (StatementId 2) (graphEdges graph)
                assertBool "SELECT should depend on CREATE TABLE public.orders" (Set.member (StatementId 1) depsS2)
        , testCase "MySQL backticks: `items`" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE `items` (id INT);"
                        , "INSERT INTO `items` VALUES (1);"
                        ]
            withGraph MySQL script $ \graph -> do
                let depsS2 = Map.findWithDefault Set.empty (StatementId 2) (graphEdges graph)
                assertBool "INSERT should depend on CREATE TABLE `items`" (Set.member (StatementId 1) depsS2)
        , testCase "Unquoted mixed-case PostgreSQL name matches quoted lowercase" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE Customers (id INT);"
                        , "SELECT * FROM \"customers\";"
                        ]
            withGraph PostgreSQL script $ \graph -> do
                let depsS2 = Map.findWithDefault Set.empty (StatementId 2) (graphEdges graph)
                assertBool
                    "PostgreSQL folds unquoted identifiers to lowercase"
                    (Set.member (StatementId 1) depsS2)
        , testCase "MySQL ON DUPLICATE KEY UPDATE records ModDep" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE `items` (id INT, val INT);"
                        , "INSERT INTO `items` VALUES (1, 2) ON DUPLICATE KEY UPDATE `val` = VALUES(`val`);"
                        ]
            withGraph MySQL script $ \graph -> do
                let actionsS2 = Map.findWithDefault [] (StatementId 2) (graphActions graph)
                    depsS2 = Map.findWithDefault Set.empty (StatementId 2) (graphEdges graph)
                assertBool
                    "INSERT ... ON DUPLICATE KEY UPDATE should record ModDep on items"
                    ( any
                        ( \act ->
                            actionKind act == TableObject
                                && actionType act == ModDep
                                && actionTarget act == ObjectTarget (ObjectName ["items"])
                        )
                        actionsS2
                    )
                assertBool
                    "INSERT ... ON DUPLICATE KEY UPDATE should depend on CREATE TABLE"
                    (Set.member (StatementId 1) depsS2)
        , testCase "SQLite bracket identifiers: [products]" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE [products] (id INT, name TEXT);"
                        , "INSERT INTO [products] VALUES (1, 'widget');"
                        , "SELECT * FROM [products];"
                        ]
            withGraph SQLite script $ \graph -> do
                let sliceS3 = backwardSlice (StatementId 3) graph
                assertBool
                    "Backward slice from SELECT should reach CREATE TABLE [products]"
                    (Set.member (StatementId 1) sliceS3)
        , testCase "SQLite schema-qualified table: main.products" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE main.products (id INT, name TEXT);"
                        , "INSERT INTO main.products VALUES (1, 'widget');"
                        , "SELECT * FROM main.products;"
                        ]
            withGraph SQLite script $ \graph -> do
                let depsS2 = Map.findWithDefault Set.empty (StatementId 2) (graphEdges graph)
                    depsS3 = Map.findWithDefault Set.empty (StatementId 3) (graphEdges graph)
                assertBool "INSERT should depend on CREATE TABLE main.products" (Set.member (StatementId 1) depsS2)
                assertBool "SELECT should depend on CREATE TABLE main.products" (Set.member (StatementId 1) depsS3)
        , testCase "SQLite CREATE VIEW and SELECT from view" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (id INT, val TEXT);"
                        , "CREATE VIEW v1 AS SELECT * FROM t1;"
                        , "SELECT * FROM v1;"
                        ]
            withGraph SQLite script $ \graph -> do
                let depsS2 = Map.findWithDefault Set.empty (StatementId 2) (graphEdges graph)
                    depsS3 = Map.findWithDefault Set.empty (StatementId 3) (graphEdges graph)
                assertBool "CREATE VIEW should depend on CREATE TABLE t1" (Set.member (StatementId 1) depsS2)
                assertBool "SELECT from view should depend on CREATE VIEW v1" (Set.member (StatementId 2) depsS3)
        , testCase "SQLite CREATE TABLE t2 AS SELECT * FROM t1 maintains dependencies" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (id INT);"
                        , "CREATE TABLE t2 AS SELECT * FROM t1;"
                        , "SELECT * FROM t1;"
                        ]
            withGraph SQLite script $ \graph -> do
                let depsS2 = Map.findWithDefault Set.empty (StatementId 2) (graphEdges graph)
                    depsS3 = Map.findWithDefault Set.empty (StatementId 3) (graphEdges graph)
                assertBool
                    "CREATE TABLE t2 AS SELECT should depend on t1 (Stmt 1)"
                    (Set.member (StatementId 1) depsS2)
                assertBool
                    "SELECT * FROM t1 must still depend on CREATE TABLE t1 (Stmt 1)"
                    (Set.member (StatementId 1) depsS3)
        , testCase "SQLite foreign key REFERENCES parent(id)" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE parent (id INT PRIMARY KEY);"
                        , "CREATE TABLE child (id INT, parent_id INT REFERENCES parent(id));"
                        ]
            withGraph SQLite script $ \graph -> do
                let depsS2 = Map.findWithDefault Set.empty (StatementId 2) (graphEdges graph)
                assertBool
                    "CREATE TABLE child should depend on CREATE TABLE parent"
                    (Set.member (StatementId 1) depsS2)
        , testCase "SQLite TRUNCATE TABLE records UseDep and ModDep" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE items (id INT);"
                        , "TRUNCATE TABLE items;"
                        ]
            withGraph SQLite script $ \graph -> do
                let actsS2 = Map.findWithDefault [] (StatementId 2) (graphActions graph)
                    depsS2 = Map.findWithDefault Set.empty (StatementId 2) (graphEdges graph)
                assertBool
                    "TRUNCATE should have ModDep on items"
                    (any (\a -> actionType a == ModDep && actionTarget a == ObjectTarget (ObjectName ["items"])) actsS2)
                assertBool "TRUNCATE should depend on CREATE TABLE items" (Set.member (StatementId 1) depsS2)
        , testCase "DuckDB dialect parses range and list comprehension syntax" $ do
            let script = "SELECT * FROM range(10); SELECT [x + 1 FOR x IN [1, 2, 3]];"
            result <- parseSQL DuckDB script
            case result of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right parsed ->
                    assertEqual "DuckDB parses two statement units" 2 (length (parsedUnits parsed))
        , testCase "ClickHouse dialect parses numbers and engine clauses" $ do
            let script = "CREATE TABLE t (id UInt64) ENGINE = MergeTree() ORDER BY id; SELECT number FROM numbers(5);"
            result <- parseSQL ClickHouse script
            case result of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right parsed ->
                    assertEqual "ClickHouse parses two statement units" 2 (length (parsedUnits parsed))
        , testCase "Oracle dialect parses DUAL, outer join (+), and hierarchical queries" $ do
            let script =
                    "SELECT 1 FROM dual;\n\
                    \CREATE TABLE emp (id NUMBER(10) PRIMARY KEY, name VARCHAR2(100));\n\
                    \SELECT a.id, b.name FROM a, b WHERE a.id = b.id(+);\n\
                    \SELECT id, name FROM emp START WITH mgr_id IS NULL CONNECT BY PRIOR id = mgr_id;"
            result <- parseSQL Oracle script
            case result of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right parsed ->
                    assertEqual "Oracle parses four statement units" 4 (length (parsedUnits parsed))
        , testCase "MsSql dialect parses bracketed identifiers and TOP clauses" $ do
            let script = "SELECT TOP 10 [id], [val] FROM [dbo].[my_table];\nSELECT 1;"
            result <- parseSQL MsSql script
            case result of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right parsed ->
                    assertEqual "MsSql parses two statement units" 2 (length (parsedUnits parsed))
        , testCase "BigQuery dialect parses backtick qualified identifiers" $ do
            let script = "SELECT `x` FROM `project.dataset.table`;\nSELECT 42;"
            result <- parseSQL BigQuery script
            case result of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right parsed ->
                    assertEqual "BigQuery parses two statement units" 2 (length (parsedUnits parsed))
        , testCase "Snowflake dialect parses double-quoted identifiers" $ do
            let script = "SELECT \"ColA\" FROM \"Db\".\"Schema\".\"MyTable\";\nSELECT 1;"
            result <- parseSQL Snowflake script
            case result of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right parsed ->
                    assertEqual "Snowflake parses two statement units" 2 (length (parsedUnits parsed))
        ]

withGraph :: SQLDialect -> BS.ByteString -> (DependencyGraph -> IO ()) -> IO ()
withGraph dialect source action = do
    result <- parseSQL dialect source
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right parsed -> action (analyzeStatements (parsedUnits parsed))

-- ----------------------------------------------------------------------------
-- Phase 8: Baselines & Benchmark Runner
-- ----------------------------------------------------------------------------

phase8Tests :: TestTree
phase8Tests =
    testGroup
        "Phase 8: Baselines & Benchmark Runner"
        [ testCase "B1 (Sequential Drop) removes dead statements and satisfies oracle" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE TABLE t3 (d INT);"
                        , "SELECT * FROM t1 WHERE a > 0;"
                        ]
                oracle = oracleFromPredicate $ \sql -> do
                    let hasT1 = BSC.isInfixOf "CREATE TABLE t1" sql
                        hasIns = BSC.isInfixOf "INSERT INTO t1" sql
                        hasSel = BSC.isInfixOf "SELECT * FROM t1 WHERE a > 0" sql
                    pure (hasT1 && hasIns && hasSel)
            reduced <- runSequentialDrop GenericSQL oracle script
            ok <- oracleAccepts oracle reduced
            assertBool "B1 reduced SQL must satisfy oracle" ok
            assertBool "Dead table t2 should be removed by B1" (not (BSC.isInfixOf "t2" reduced))
            assertBool "Dead table t3 should be removed by B1" (not (BSC.isInfixOf "t3" reduced))
            assertBool "Table t1 must be retained" (BSC.isInfixOf "CREATE TABLE t1" reduced)
            assertBool "Insert into t1 must be retained" (BSC.isInfixOf "INSERT INTO t1" reduced)
            assertBool "Select query must be retained" (BSC.isInfixOf "SELECT * FROM t1" reduced)
        , testCase "B2 (Naive ddmin) removes dead statements and satisfies oracle" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE TABLE t3 (d INT);"
                        , "SELECT * FROM t1 WHERE a > 0;"
                        ]
                oracle = oracleFromPredicate $ \sql -> do
                    let hasT1 = BSC.isInfixOf "CREATE TABLE t1" sql
                        hasIns = BSC.isInfixOf "INSERT INTO t1" sql
                        hasSel = BSC.isInfixOf "SELECT * FROM t1 WHERE a > 0" sql
                    pure (hasT1 && hasIns && hasSel)
            reduced <- runNaiveDdmin GenericSQL oracle script
            ok <- oracleAccepts oracle reduced
            assertBool "B2 reduced SQL must satisfy oracle" ok
            assertBool "Dead table t2 should be removed by B2" (not (BSC.isInfixOf "t2" reduced))
            assertBool "Dead table t3 should be removed by B2" (not (BSC.isInfixOf "t3" reduced))
            assertBool "Table t1 must be retained" (BSC.isInfixOf "CREATE TABLE t1" reduced)
            assertBool "Insert into t1 must be retained" (BSC.isInfixOf "INSERT INTO t1" reduced)
            assertBool "Select query must be retained" (BSC.isInfixOf "SELECT * FROM t1" reduced)
        ]

-- ----------------------------------------------------------------------------
-- Property Tests: Invariants of Reducer & DAG
-- ----------------------------------------------------------------------------

data ArbitraryAction = ArbitraryAction !ObjectKind !DependencyType !T.Text
    deriving (Eq, Show)

instance Arbitrary ArbitraryAction where
    arbitrary = do
        k <- elements [TableObject, ColumnObject, IndexObject, ViewObject, BarrierObject]
        t <- elements [DefDep, UseDep, ModDep, BarrierDep]
        name <- elements ["t1", "t2", "t3", "v1", "c1", "__barrier__"]
        pure (ArbitraryAction k t name)

toObjectAction :: ArbitraryAction -> ObjectAction
toObjectAction (ArbitraryAction k t name) = ObjectAction k t (ObjectTarget (ObjectName [name]))

newtype SmallActionGraph = SmallActionGraph [[ArbitraryAction]]
    deriving (Show)

instance Arbitrary SmallActionGraph where
    arbitrary = do
        statementCount <- chooseInt (2, 8)
        SmallActionGraph <$> vectorOf statementCount (resize 8 arbitrary)
    shrink (SmallActionGraph statements) =
        [SmallActionGraph smaller | smaller <- shrink statements, not (null smaller)]

buildActionGraph :: [[ArbitraryAction]] -> DependencyGraph
buildActionGraph rawActions =
    buildGraph
        [ (StatementId sid, map toObjectAction actions)
        | (sid, actions) <- zip [1 ..] rawActions
        ]

-- Data.Graph independently computes reachability along each statement's predecessors.
backwardReachabilityOracle ::
    DependencyGraph ->
    Map.Map StatementId (Set.Set StatementId)
backwardReachabilityOracle dependencyGraph =
    let nodes =
            [ (sid, sid, Set.toList (Map.findWithDefault Set.empty sid (graphEdges dependencyGraph)))
            | sid <- Set.toList (graphStmts dependencyGraph)
            ]
        (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges nodes
        reachableFrom target =
            case vertexFromKey target of
                Nothing -> Set.empty
                Just vertex ->
                    Set.fromList
                        [ reached
                        | reachedVertex <- Graph.reachable graph vertex
                        , let (_, reached, _) = nodeFromVertex reachedVertex
                        ]
     in Map.fromSet reachableFrom (graphStmts dependencyGraph)

-- Prop 1: DAG Acyclicity
prop_dagAcyclicity :: [[ArbitraryAction]] -> Property
prop_dagAcyclicity rawActions =
    let graph = buildActionGraph rawActions
        edges = Map.toList (graphEdges graph)
        strictlyBackwards (StatementId j, preds) =
            all (\(StatementId i) -> i < j) (Set.toList preds)
     in counterexample
            ("Graph edges violate acyclicity: " ++ show edges)
            (all strictlyBackwards edges)

-- Prop 2: Backward slices exactly match graph reachability
prop_backwardSliceSoundness :: SmallActionGraph -> Property
prop_backwardSliceSoundness (SmallActionGraph rawActions) =
    let graph = buildActionGraph rawActions
        expectedByTarget = backwardReachabilityOracle graph
        mismatches =
            [ (target, Set.difference expected actual, Set.difference actual expected)
            | target <- Set.toList (graphStmts graph)
            , let actual = backwardSlice target graph
                  expected = Map.findWithDefault Set.empty target expectedByTarget
            , actual /= expected
            ]
     in counterexample
            ( "Backward slices differ (target, missing ancestors, unrelated extras): "
                ++ show mismatches
                ++ "; graph edges: "
                ++ show (graphEdges graph)
            )
            (null mismatches)

-- Prop 2b: Set closure exactly matches reachability for every valid root subset
prop_closeSetReachability :: SmallActionGraph -> Property
prop_closeSetReachability (SmallActionGraph rawActions) =
    let graph = buildActionGraph rawActions
        expectedByTarget = backwardReachabilityOracle graph
        rootSets = map Set.fromList (subsequences (Set.toList (graphStmts graph)))
        mismatches =
            [ (roots, actual, expected)
            | roots <- rootSets
            , let actual = closeSet graph roots
                  expected =
                    Set.unions
                        [ Map.findWithDefault Set.empty root expectedByTarget
                        | root <- Set.toList roots
                        ]
            , actual /= expected
            ]
     in counterexample
            ("closeSet differs from reachability (roots, actual, expected): " ++ show mismatches)
            (null mismatches)

-- Prop 3: Reduction Monotonicity
data TestOracle
    = OracleAlwaysTrue
    | OracleAlwaysFalse
    | OracleContains BSC.ByteString
    | OracleMinLength Int
    deriving (Show)

instance Arbitrary TestOracle where
    arbitrary =
        oneof
            [ pure OracleAlwaysTrue
            , pure OracleAlwaysFalse
            , OracleContains <$> elements ["t1", "t2", "SELECT", "WHERE", "crash", "missing"]
            , OracleMinLength <$> chooseInt (0, 100)
            ]

runTestOracle :: TestOracle -> Oracle
runTestOracle = oracleFromPredicate . runTestPredicate

runTestPredicate :: TestOracle -> BS.ByteString -> IO Bool
runTestPredicate OracleAlwaysTrue = const (pure True)
runTestPredicate OracleAlwaysFalse = const (pure False)
runTestPredicate (OracleContains needle) = \s -> pure (needle `BSC.isInfixOf` s)
runTestPredicate (OracleMinLength minLen) = \s -> pure (BS.length s >= minLen)

newtype TestScript = TestScript BSC.ByteString
    deriving (Show)

instance Arbitrary TestScript where
    arbitrary = do
        let pool =
                [ "CREATE TABLE t1 (a INT, b TEXT);"
                , "CREATE TABLE t2 (c INT);"
                , "INSERT INTO t1 VALUES (1, 'x');"
                , "INSERT INTO t2 VALUES (2);"
                , "SELECT * FROM t1 WHERE (a + 0) > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                , "SELECT * FROM t2 WHERE c > 0;"
                , "BEGIN;\nINSERT INTO t1 VALUES (3, 'y');\nCOMMIT;"
                ]
        lines_ <- sublistOf pool
        extra <- arbitrary :: Gen String
        let full = BSC.unlines (map BSC.pack lines_ ++ [TE.encodeUtf8 (T.pack extra) | not (null extra)])
        pure (TestScript full)

prop_reductionMonotonicity :: TestScript -> TestOracle -> Property
prop_reductionMonotonicity (TestScript script) testOracle = monadicIO $ do
    reduced <- run (reduceSQL SQLite (runTestOracle testOracle) script)
    assert (BS.length reduced <= BS.length script)

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests (QuickCheck)"
        [ testProperty "Prop 1: DAG Acyclicity (j -> i implies i < j)" prop_dagAcyclicity
        , testProperty
            "Prop 2: Backward Slice Soundness (exact predecessor reachability)"
            prop_backwardSliceSoundness
        , testProperty
            "Prop 2b: Set closure matches reachability for every valid root subset"
            prop_closeSetReachability
        , testProperty
            "Prop 3: Reduction Monotonicity (len reduced <= len initial)"
            prop_reductionMonotonicity
        ]

module Main (main) where

import Benchmark (BenchmarkMetric (..))
import ColumnChecks qualified
import Control.Exception (SomeException, bracket, try)
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isSpace)
import Data.Graph qualified as Graph
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, subsequences)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import DependencyChecks qualified
import EvaluationChecks qualified
import Fuzz (generativeFuzzTests)
import MutationalFuzz (mutationalFuzzTests)
import Occam.Cache (
    CacheStats (..),
 )
import Occam.Core (
    CatalogOp (..),
    ColumnDef (..),
    ConflictAction (..),
    DataWrite (..),
    Ident (..),
    Name (..),
    NameRef (..),
    OracleInconclusiveError,
    OracleOutcome (..),
    ReadFact (..),
    ReadMode (..),
    SemanticFacts (..),
    SessionInit (..),
    StatementId (..),
    TableDef (..),
    WriteKind (..),
    oracleAccepts,
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements, buildGraph)
import Occam.Evaluation (EvaluationConfig (..), EvaluationMetrics (..), EvaluationStatus (..))
import Occam.Logger (nullLogger)
import Occam.Oracle (OracleObservation (..), OracleObservationDetails)
import Occam.Parser (ParsedSQL (..), SQLDialect (..), defaultParseOptions, dialectName, parseSQL)
import Occam.Pool (
    OraclePool,
    PoolConfig (..),
    newOraclePool,
    observationDetails,
    poolCacheStats,
    poolOracle,
    predicateDetails,
    testOrdered,
 )
import Occam.Reducer
import Occam.Slicer
import OracleChecks qualified
import PoolChecks qualified
import PoolMetricsChecks qualified
import ReductionChecks qualified
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
 )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcess, readProcessWithExitCode)
import Test.QuickCheck (
    Arbitrary (..),
    Property,
    chooseInt,
    counterexample,
    elements,
    oneof,
    resize,
    vectorOf,
 )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

emptySemanticFacts :: SemanticFacts
emptySemanticFacts = SemanticFacts [] [] [] [] []

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Occam Test Suite"
        [ phase3Tests
        , phase5Tests
        , phase6Tests
        , phase7Tests
        , phase8Tests
        , DependencyChecks.tests
        , EvaluationChecks.tests
        , OracleChecks.tests
        , PoolChecks.tests
        , PoolMetricsChecks.tests
        , ReductionChecks.tests
        , ColumnChecks.tests
        , propertyTests
        , generativeFuzzTests
        , mutationalFuzzTests
        ]

-- ----------------------------------------------------------------------------
-- Hierarchical reducer
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
                predicate sql = do
                    let hasT1 = BSC.isInfixOf "CREATE TABLE t1" sql
                        hasIns = BSC.isInfixOf "INSERT INTO t1" sql
                        hasSel = BSC.isInfixOf "SELECT" sql && BSC.isInfixOf "WHERE a > 0" sql
                    pure (hasT1 && hasIns && hasSel)
                config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
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

-- ----------------------------------------------------------------------------
-- Phase 5: CLI
-- ----------------------------------------------------------------------------

getOccamBinary :: IO FilePath
getOccamBinary = resolveBinary "OCCAM_BIN" "occam"

-- | Env override, then PATH, then `cabal list-bin`; "cabal" means `cabal run`.
resolveBinary :: String -> String -> IO FilePath
resolveBinary envVar name = do
    mEnv <- lookupEnv envVar
    case mEnv of
        Just p -> do
            exists <- doesFileExist p
            if exists then pure p else fromPath
        Nothing -> fromPath
    where
        fromPath = do
            mPath <- findExecutable name
            maybe fromListBin pure mPath
        fromListBin = do
            mPath <- tryListBin name
            maybe (pure "cabal") pure mPath

tryListBin :: String -> IO (Maybe FilePath)
tryListBin name = do
    result <- try (readProcess "cabal" ["list-bin", name] "") :: IO (Either SomeException String)
    case result of
        Left _ -> pure Nothing
        Right out -> do
            let path = stripEnd out
            exists <- doesFileExist path
            pure (if exists then Just path else Nothing)

runOccamCLI :: [String] -> IO (ExitCode, String, String)
runOccamCLI cliArgs = do
    bin <- getOccamBinary
    if bin == "cabal"
        then readProcessWithExitCode "cabal" (["run", "-v0", "occam", "--"] ++ cliArgs) ""
        else readProcessWithExitCode bin cliArgs ""

getOccamEvalBinary :: IO FilePath
getOccamEvalBinary = resolveBinary "OCCAM_EVAL_BIN" "occam-eval"

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

stripEnd :: String -> String
stripEnd = reverse . dropWhile isSpace . reverse

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
                [(runOccamCLI, []), (runOccamEvalCLI, ["--profile", "statement-ddmin"])]
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
                [(runOccamCLI, []), (runOccamEvalCLI, ["--profile", "statement-ddmin"])]
        , testCase "selected internal profiles own their preflight and execution budget" $
            withCLIInput "SELECT 1; SELECT 2;" $ \input outRoot ->
                withCLIInput "" $ \calls _ -> do
                    let predicate = "cat >/dev/null; printf x >> " ++ show calls
                    mapM_
                        ( \profile -> do
                            let outDir = outRoot </> profile
                            (code, _, _) <-
                                runOccamEvalCLI
                                    [ input
                                    , "--profile"
                                    , profile
                                    , "--oracle"
                                    , predicate
                                    , "--max-oracle-executions"
                                    , "1"
                                    , "--out"
                                    , outDir
                                    , "--quiet"
                                    ]
                            assertBool "Execution-budget exhaustion must remain nonzero" (code /= ExitSuccess)
                            encoded <- BS.readFile (outDir </> "benchmark.json")
                            case eitherDecodeStrict' encoded :: Either String [BenchmarkMetric] of
                                Right [metric] -> do
                                    let evaluation = metricEvaluationMetrics metric
                                    assertEqual "Wrong selected profile" (T.pack profile) (metricAlgorithm metric)
                                    assertEqual "Not an external baseline" "internal" (metricExperimentLine metric)
                                    assertEqual
                                        "Own preflight consumes this run's cap"
                                        1
                                        (evaluationPreflightAttempts (evaluationBudget evaluation))
                                    assertEqual "Budget result was lost" EvaluationExecutionBudget (evaluationStatus evaluation)
                                    assertEqual
                                        "No reduction call remains after preflight"
                                        0
                                        (evaluationDatabaseObservations evaluation)
                                    assertBool "An unchecked checkpoint is not validated" (not (evaluationValidationPassed evaluation))
                                other -> assertFailure ("Expected one profile result, got " ++ show other)
                            checkpoint <- BS.readFile (outDir </> "minimal.sql")
                            assertEqual "Accepted input checkpoint was lost" "SELECT 1; SELECT 2;" checkpoint
                        )
                        ["statement-ddmin", "graph-off"]
                    observed <- BSC.readFile calls
                    assertEqual "Each child must physically execute its own preflight" "xx" observed
        , testCase "CLI and evaluator surface --jobs help and reject invalid counts" $ do
            mapM_
                ( \(runner, extra) -> do
                    (helpCode, helpOut, _) <- runner ("--help" : extra)
                    assertEqual "help must exit zero" ExitSuccess helpCode
                    assertBool "help hides --jobs" ("--jobs" `elem` words helpOut)
                    mapM_
                        ( \jobs -> withCLIInput "SELECT 1;" $ \input outDir -> do
                            (code, _, _) <-
                                runner ([input, "--oracle", "exit 0", "--out", outDir, "--jobs", jobs, "--quiet"] ++ extra)
                            assertBool ("invalid --jobs was accepted: " ++ jobs) (code /= ExitSuccess)
                            exists <- doesDirectoryExist outDir
                            assertBool "reduction started with invalid --jobs" (not exists)
                        )
                        ["0", "-1", "abc"]
                )
                [(runOccamCLI, []), (runOccamEvalCLI, ["--profile", "statement-ddmin"])]
        , testCase "CLI rejects bad column modes before any oracle call" $
            withCLIInput "SELECT 1;" $ \input outDir -> do
                (helpCode, helpOut, _) <- runOccamCLI ["--help"]
                assertEqual "help must exit zero" ExitSuccess helpCode
                assertBool "help hides --column-mode" ("--column-mode" `elem` words helpOut)
                mapM_
                    ( \mode -> do
                        (code, _, _) <-
                            runOccamCLI [input, "--oracle", "exit 0", "--out", outDir, "--column-mode", mode, "--quiet"]
                        assertBool ("bad --column-mode was accepted: " ++ mode) (code /= ExitSuccess)
                        exists <- doesDirectoryExist outDir
                        assertBool "reduction started with bad --column-mode" (not exists)
                    )
                    ["bogus", "single-atomic"]
        , testCase "CLI requires --intra-statement for column modes and starts no oracle" $
            withCLIInput "SELECT 1;" $ \input outDir ->
                withCLIInput "" $ \calls _ -> do
                    let oracle = "cat >/dev/null; printf x >> " ++ show calls
                    mapM_
                        ( \mode -> do
                            (code, _, err) <-
                                runOccamCLI [input, "--oracle", oracle, "--out", outDir, "--column-mode", mode, "--quiet"]
                            assertBool ("column mode without intra-statement was accepted: " ++ mode) (code /= ExitSuccess)
                            assertBool "wrong rejection reason" ("--intra-statement" `elem` words err)
                            exists <- doesDirectoryExist outDir
                            assertBool "reduction started without --intra-statement" (not exists)
                        )
                        ["single", "atomic"]
                    observed <- BSC.readFile calls
                    assertEqual "oracle ran before validation" "" observed
        , testCase "help advertises all selected dialects" $ do
            (helpCode, helpOut, _) <- runOccamCLI ["--help"]
            assertEqual "help must exit zero" ExitSuccess helpCode
            mapM_
                ( \dialect ->
                    let name = T.unpack (dialectName dialect)
                     in assertBool ("--help hides dialect: " ++ name) (name `isInfixOf` helpOut)
                )
                ([minBound .. maxBound] :: [SQLDialect])
        , testCase "--out - keeps stdout pure SQL under --verbose" $
            withCLIInput "CREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\nINSERT INTO t VALUES (2);\n" $ \input _ -> do
                (code, out, err) <- runOccamCLI [input, "--oracle", "grep -q INSERT", "--out", "-", "--verbose"]
                assertEqual ("reduction failed: " ++ err) ExitSuccess code
                assertBool
                    "stdout polluted with log output"
                    (not (any (`isInfixOf` out) ["[DEBUG]", "[INFO ", "[WARN ", "[ERROR]"]))
                assertBool "stdout missing reduced SQL" ("INSERT" `isInfixOf` out)
                assertBool "expected logs on stderr" (not (null err))
        , testCase "--log-level debug emits logs in text mode" $
            withCLIInput "CREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\n" $ \input _ -> do
                (code, _, err) <-
                    runOccamCLI [input, "--oracle", "grep -q INSERT", "--out", "-", "--log-level", "debug"]
                assertEqual ("reduction failed: " ++ err) ExitSuccess code
                -- No component emits debug-level messages yet, so INFO visibility is the
                -- observable proof the flag took effect (text mode used to force WARN).
                assertBool "text mode ignored --log-level debug" ("[INFO " `isInfixOf` err)
        ]

-- ----------------------------------------------------------------------------
-- Phase 6: Candidate Memoization
-- ----------------------------------------------------------------------------

phase6Tests :: TestTree
phase6Tests =
    testGroup
        "Phase 6: Candidate Memoization"
        [ testCase "ordered pool caches repeated queries and tracks hits/misses" $ do
            callCountRef <- newIORef (0 :: Int)
            let mockOracle sql = do
                    modifyIORef' callCountRef (+ 1)
                    pure (observationDetails (boolObservation (BSC.isInfixOf "CRASH" sql)))
            pool <- testCountingPool 1 mockOracle
            let oracle = poolOracle pool

            -- 1. Initial queries (all misses)
            r1 <- oracleAccepts oracle "SELECT * FROM t1 WHERE CRASH;"
            assertEqual "First query should return True" True r1
            r2 <- oracleAccepts oracle "SELECT 1;"
            assertEqual "Second query should return False" False r2

            calls1 <- readIORef callCountRef
            assertEqual "Underlying oracle should be called 2 times" 2 calls1

            stats1 <- poolCacheStats pool
            assertEqual "cacheHits should be 0" 0 (cacheHits stats1)
            assertEqual "cacheMisses should be 2" 2 (cacheMisses stats1)

            -- 2. Repeated queries (all hits)
            r3 <- oracleAccepts oracle "SELECT * FROM t1 WHERE CRASH;"
            assertEqual "Cached query 1 should return True" True r3
            r4 <- oracleAccepts oracle "SELECT 1;"
            assertEqual "Cached query 2 should return False" False r4
            r5 <- oracleAccepts oracle "SELECT * FROM t1 WHERE CRASH;"
            assertEqual "Cached query 1 (second time) should return True" True r5

            calls2 <- readIORef callCountRef
            assertEqual "Underlying oracle should NOT be re-invoked on cache hits" 2 calls2

            stats2 <- poolCacheStats pool
            assertEqual "cacheHits should be 3" 3 (cacheHits stats2)
            assertEqual "cacheMisses should remain 2" 2 (cacheMisses stats2)

            -- 3. Another new query (miss)
            r6 <- oracleAccepts oracle "SELECT 2;"
            assertEqual "New query should return False" False r6

            calls3 <- readIORef callCountRef
            assertEqual "Underlying oracle should be called for new query" 3 calls3

            stats3 <- poolCacheStats pool
            assertEqual "cacheHits should be 3" 3 (cacheHits stats3)
            assertEqual "cacheMisses should be 3" 3 (cacheMisses stats3)
        , testCase "inconclusive outcomes are not cached as authoritative" $ do
            callCountRef <- newIORef (0 :: Int)
            let outcomeOracle _ = do
                    modifyIORef' callCountRef (+ 1)
                    pure (observationDetails OracleTimedOut)
            pool <- testCountingPool 1 outcomeOracle
            let oracle = poolOracle pool
                expectInconclusive label = do
                    result <- try (oracle "SELECT 1;") :: IO (Either OracleInconclusiveError OracleOutcome)
                    case result of
                        Left _ -> pure ()
                        Right _ -> assertFailure ("inconclusive outcome was returned as definite: " ++ label)
            expectInconclusive "first"
            expectInconclusive "second"
            calls <- readIORef callCountRef
            assertEqual "inconclusive result is re-observed" 2 calls
            stats <- poolCacheStats pool
            assertEqual "inconclusive results are misses" 2 (cacheMisses stats)
            assertEqual "inconclusive results are not hits" 0 (cacheHits stats)
        , testCase "concurrent duplicate candidates single-flight one physical execution" $ do
            callCountRef <- newIORef (0 :: Int)
            let mockOracle _ = do
                    modifyIORef' callCountRef (+ 1)
                    pure (observationDetails (OracleFinished (ExitFailure 1)))
            pool <- testCountingPool 4 mockOracle
            outcomes <- testOrdered pool (replicate 4 "SELECT 1;")
            assertEqual "every duplicate retires" 4 (length outcomes)
            assertBool "shared verdict rejects" (all ((== OracleRejected) . fst) outcomes)
            calls <- readIORef callCountRef
            assertEqual "duplicates share one physical execution" 1 calls
            stats <- poolCacheStats pool
            assertEqual "one miss" 1 (cacheMisses stats)
            -- Whether a later duplicate joins the in-flight owner or hits its
            -- completed entry depends on scheduling; either way it never
            -- re-executes physically.
            assertEqual
                "every duplicate shared without re-executing"
                3
                (cacheHits stats + cacheJoins stats)
        ]

-- ----------------------------------------------------------------------------
-- Selected frontend dialects
-- ----------------------------------------------------------------------------

phase7Tests :: TestTree
phase7Tests =
    testGroup
        "Selected frontend dialects"
        [ testCase "every selected dialect parses statement boundaries" $
            mapM_
                ( \dialect -> do
                    result <- parseSQL (defaultParseOptions dialect) "SELECT 1; SELECT 2;"
                    case result of
                        Left err -> assertFailure (show dialect ++ " parser transport error: " ++ show err)
                        Right parsed ->
                            assertEqual
                                (show dialect ++ " parses two statement units")
                                2
                                (length (parsedUnits parsed))
                )
                [ GenericSQL
                , Ansi
                , Teradata
                , PostgreSQL
                , Redshift
                , MySQL
                , SQLite
                , DuckDB
                , ClickHouse
                , Oracle
                , Snowflake
                , Hive
                , Spark
                , Databricks
                , BigQuery
                , MsSql
                ]
        ]

withGraph :: SQLDialect -> BS.ByteString -> (DependencyGraph -> IO ()) -> IO ()
withGraph dialect source action = do
    result <- parseSQL (defaultParseOptions dialect) source
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right parsed -> action (analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed))

{- | Throwaway pools for unit tests. Workers always join before 'testOrdered'
returns, so tests never need to close these.
-}
testPredicatePool :: (BS.ByteString -> IO Bool) -> IO OraclePool
testPredicatePool predicate =
    newOraclePool
        PoolConfig
            { poolJobs = 1
            , poolCacheEnabled = False
            , poolPermits = Nothing
            , poolPrecomputed = Map.empty
            , poolMetrics = Nothing
            , poolExecute = predicateDetails predicate
            , poolBill = const (pure ())
            , poolRetire = \_ _ _ _ -> pure ()
            }

testCountingPool :: Int -> (BS.ByteString -> IO OracleObservationDetails) -> IO OraclePool
testCountingPool jobs execute =
    newOraclePool
        PoolConfig
            { poolJobs = jobs
            , poolCacheEnabled = True
            , poolPermits = Nothing
            , poolPrecomputed = Map.empty
            , poolMetrics = Nothing
            , poolExecute = execute
            , poolBill = const (pure ())
            , poolRetire = \_ _ _ _ -> pure ()
            }

boolObservation :: Bool -> OracleObservation
boolObservation accepted =
    if accepted then OracleFinished ExitSuccess else OracleFinished (ExitFailure 1)

-- ----------------------------------------------------------------------------
-- Phase 8: Internal statement controls
-- ----------------------------------------------------------------------------

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
                    reduced <- reduce GenericSQL pool script
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

-- ----------------------------------------------------------------------------
-- Property Tests: Invariants of Reducer & DAG
-- ----------------------------------------------------------------------------

data ArbitraryFact
    = DefineTableFact !T.Text
    | WriteTableFact !T.Text
    | ReadTableFact !T.Text
    deriving (Eq, Show)

instance Arbitrary ArbitraryFact where
    arbitrary = do
        name <- elements ["t1", "t2", "t3"]
        oneof
            [ pure (DefineTableFact name)
            , pure (WriteTableFact name)
            , pure (ReadTableFact name)
            ]

factOp :: ArbitraryFact -> CatalogOp
factOp fact =
    case fact of
        DefineTableFact name ->
            DefineTableOp
                TableDef
                    { tableName = Name [Ident name]
                    , tableTemporary = False
                    , tableTransactional = True
                    , tableColumns = [ColumnDef (Ident "id") False False False []]
                    , tableConstraints = []
                    , tableSources = []
                    , tableLike = Nothing
                    , tableOnCommit = Nothing
                    , tableIfNotExists = False
                    , tableOrReplace = False
                    }
        WriteTableFact name ->
            WriteDataOp
                DataWrite
                    { dataWriteTarget = NameRef (Name [Ident name]) False
                    , dataWriteKind = InsertWrite ConflictError
                    , dataWriteColumns = []
                    , dataWriteUnqualifiedColumns = []
                    , dataWriteReads = []
                    }
        ReadTableFact name ->
            ReadOp
                ReadFact
                    { readSource = NameRef (Name [Ident name]) False
                    , readMode = ReadData
                    , readColumns = []
                    }

newtype SmallFactGraph = SmallFactGraph [[ArbitraryFact]]
    deriving (Show)

instance Arbitrary SmallFactGraph where
    arbitrary = do
        statementCount <- chooseInt (2, 8)
        SmallFactGraph <$> vectorOf statementCount (resize 8 arbitrary)
    shrink (SmallFactGraph statements) =
        [SmallFactGraph smaller | smaller <- shrink statements, not (null smaller)]

buildFactGraph :: [[ArbitraryFact]] -> DependencyGraph
buildFactGraph rawFacts =
    buildGraph
        defaultSessionInit
        [ ( StatementId sid
          , emptySemanticFacts {factOps = map factOp facts}
          )
        | (sid, facts) <- zip [1 ..] rawFacts
        ]

defaultSessionInit :: SessionInit
defaultSessionInit = SessionInit [] [] Nothing True True Map.empty

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
prop_dagAcyclicity :: [[ArbitraryFact]] -> Property
prop_dagAcyclicity rawFacts =
    let graph = buildFactGraph rawFacts
        edges = Map.toList (graphEdges graph)
        strictlyBackwards (StatementId j, preds) =
            all (\(StatementId i) -> i < j) (Set.toList preds)
     in counterexample
            ("Graph edges violate acyclicity: " ++ show edges)
            (all strictlyBackwards edges)

-- Prop 2: Backward slices exactly match graph reachability
prop_backwardSliceSoundness :: SmallFactGraph -> Property
prop_backwardSliceSoundness (SmallFactGraph rawFacts) =
    let graph = buildFactGraph rawFacts
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
prop_closeSetReachability :: SmallFactGraph -> Property
prop_closeSetReachability (SmallFactGraph rawFacts) =
    let graph = buildFactGraph rawFacts
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
        ]

module Main (main) where

import ColumnChecks qualified
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forM, forM_, when)
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeStrict', encode, withObject, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isSpace)
import Data.Graph qualified as Graph
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.IntSet qualified as IntSet
import Data.List (isInfixOf, subsequences)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import DependencyChecks qualified
import EvaluationChecks qualified
import Fuzz (generativeFuzzTests)
import MutationalFuzz (mutationalFuzzTests)
import Occam.Benchmark.Metrics (
    RunSummary (..),
    Stats (..),
    computeAttemptAuc,
    engineOverheadMicros,
    generateReportForDirectory,
    missingStatsSummary,
    renderSummaryJson,
 )
import Occam.Cache (
    CacheAdmission (..),
    CacheMode (..),
    CacheStats (..),
    LookupResult (..),
    cacheCompleteInflight,
    cacheInstallInflight,
    cacheLookup,
    cacheRemoveInflight,
    cacheStats,
    newCache,
    observationAdmission,
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
    ResultStatement (..),
    ResultTrace (..),
    SemanticFacts (..),
    SessionInit (..),
    Statement (stmtRaw),
    StatementId (..),
    TableDef (..),
    WriteKind (..),
    oracleAccepts,
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements, buildGraph)
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationPoint (..),
    EvaluationStatus (..),
    MinimalityClaim (..),
 )
import Occam.Logger (nullLogger)
import Occam.Metrics (
    FamilyCounters (..),
    FamilyTag (..),
    MetricPolicy (..),
    MetricsSnapshot (..),
    ParseKind (..),
    emptyFamilyCounters,
 )
import Occam.Oracle (OracleObservation (..), OracleObservationDetails)
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
import Occam.Parser (
    ParsedSQL (..),
    SQLDialect (..),
    StatementUnit (..),
    defaultParseOptions,
    dialectName,
    nativeOracleCacheKey,
    parseSQL,
 )
import Occam.Pool (
    OraclePool,
    PoolConfig (..),
    newOraclePool,
    observationDetails,
    poolCacheStats,
    poolOracle,
    predicateDetails,
    testOrdered,
    withPredicatePool,
 )
import Occam.Reducer
import Occam.Slicer
import OracleChecks qualified
import PoolChecks qualified
import PoolMetricsChecks qualified
import ReductionChecks qualified
import System.Directory (
    createDirectoryIfMissing,
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
import System.Process (
    CreateProcess (..),
    StdStream (..),
    proc,
    readCreateProcessWithExitCode,
    readProcess,
    readProcessWithExitCode,
    shell,
    terminateProcess,
    waitForProcess,
    withCreateProcess,
 )
import System.Timeout (timeout)
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
import TestSupport (emptySemanticFacts)

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
        , sqliteOracleTests
        , duckdbOracleTests
        , benchmarkMetricsTests
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
runOccamCLI cliArgs = runOccamCLIWithStdin cliArgs ""

runOccamCLIWithStdin :: [String] -> String -> IO (ExitCode, String, String)
runOccamCLIWithStdin cliArgs stdinData = do
    bin <- getOccamBinary
    if bin == "cabal"
        then readProcessWithExitCode "cabal" (["run", "-v0", "occam", "--"] ++ cliArgs) stdinData
        else readProcessWithExitCode bin cliArgs stdinData

withCLIInput :: BS.ByteString -> (FilePath -> FilePath -> IO a) -> IO a
withCLIInput sql action =
    bracket create cleanup $ \input -> do
        BS.writeFile input sql
        action input (input ++ ".min.sql")
    where
        create = do
            directory <- getTemporaryDirectory
            (path, handle) <- openTempFile directory "occam-cli-"
            hClose handle
            pure path
        cleanup path = do
            removeFile path
            exists <- doesFileExist (path ++ ".min.sql")
            when exists (removeFile (path ++ ".min.sql"))

stripEnd :: String -> String
stripEnd = reverse . dropWhile isSpace . reverse

-- | Minimal probe into the CLI's stats.json (whose Stats type lives in app/).
data CliStats = CliStats
    { cliStatus :: !EvaluationStatus
    , cliBudget :: !EvaluationConfig
    , cliMetrics :: !EvaluationMetrics
    , cliStrategy :: !T.Text
    }

instance FromJSON CliStats where
    parseJSON = withObject "CliStats" $ \stats -> do
        settings <- stats .: "reduction_settings"
        strategy <- withObject "reduction_settings" (.: "strategy") settings
        CliStats
            <$> stats .: "status"
            <*> stats .: "evaluation_budget"
            <*> stats .: "evaluation_metrics"
            <*> pure strategy

findPythonExecutable :: IO String
findPythonExecutable = do
    mPy3 <- findExecutable "python3"
    case mPy3 of
        Just _ -> pure "python3"
        Nothing -> do
            mPy <- findExecutable "python"
            case mPy of
                Just _ -> pure "python"
                Nothing -> pure "python3"

phase5Tests :: TestTree
phase5Tests =
    testGroup
        "CLI predicate contract"
        [ testCase "CLI refuses rejected or signaled initial predicates" $
            mapM_
                ( \extra ->
                    mapM_
                        ( \command -> withCLIInput "SELECT 1;" $ \input outFile -> do
                            (code, _, _) <- runOccamCLI ([input, "--oracle", command, "--out", outFile, "--quiet"] ++ extra)
                            assertBool ("Noninteresting input was accepted: " ++ command) (code /= ExitSuccess)
                            exists <- doesFileExist outFile
                            assertBool "Reduction started before the initial predicate accepted" (not exists)
                        )
                        ["false", "exit 139", "kill -TERM $$"]
                )
                [[], ["--strategy", "ddmin-only"]]
        , testCase "CLI shrinks SQL while preserving the supplied predicate" $ do
            py <- findPythonExecutable
            let initialSQL =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                        ]
                predicate =
                    py
                        ++ " -c \"import sys; s=sys.stdin.read(); sys.exit(0 if 'WHERE a > 0' in s and 'CREATE TABLE t1' in s and 'INSERT INTO t1' in s else 1)\""
            mapM_
                ( \extra -> withCLIInput initialSQL $ \input outFile -> do
                    (code, _, err) <- runOccamCLI ([input, "--oracle", predicate, "--out", outFile, "--quiet"] ++ extra)
                    assertEqual ("Reduction failed: " ++ err) ExitSuccess code
                    reduced <- BSC.readFile outFile
                    (verified, _, _) <- readCreateProcessWithExitCode (shell predicate) (BSC.unpack reduced)
                    assertEqual "Reduced SQL lost the target property" ExitSuccess verified
                    assertBool "No SQL reduction occurred" (BS.length reduced < BS.length initialSQL)
                )
                [[], ["--strategy", "ddmin-only"]]
        , testCase "selected strategies own their preflight and execution budget" $
            withCLIInput "SELECT 1; SELECT 2;" $ \input outRoot ->
                withCLIInput "" $ \calls _ -> do
                    let predicate = "cat >/dev/null; printf x >> " ++ show calls
                    mapM_
                        ( \(extra, expectedStrategy) -> do
                            let outFile = outRoot ++ "-" ++ T.unpack expectedStrategy ++ ".sql"
                                statsFile = outRoot ++ "-" ++ T.unpack expectedStrategy ++ ".json"
                            (code, _, _) <-
                                runOccamCLI
                                    ( [ input
                                      , "--oracle"
                                      , predicate
                                      , "--max-oracle-executions"
                                      , "1"
                                      , "--out"
                                      , outFile
                                      , "--stats"
                                      , statsFile
                                      , "--quiet"
                                      ]
                                        ++ extra
                                    )
                            assertBool "Execution-budget exhaustion must remain nonzero" (code /= ExitSuccess)
                            encoded <- BS.readFile statsFile
                            case eitherDecodeStrict' encoded :: Either String CliStats of
                                Right stats -> do
                                    let evaluation = cliMetrics stats
                                    assertEqual "Wrong selected strategy" expectedStrategy (cliStrategy stats)
                                    assertEqual
                                        "Own preflight consumes this run's cap"
                                        1
                                        (evaluationPreflightAttempts (cliBudget stats))
                                    assertEqual "Budget result was lost" EvaluationExecutionBudget (evaluationStatus evaluation)
                                    assertEqual
                                        "Top-level status was lost"
                                        EvaluationExecutionBudget
                                        (cliStatus stats)
                                    assertEqual
                                        "No reduction call remains after preflight"
                                        0
                                        (evaluationDatabaseObservations evaluation)
                                    assertBool "An unchecked checkpoint is not validated" (not (evaluationValidationPassed evaluation))
                                Left err -> assertFailure ("stats.json did not decode: " ++ err)
                            checkpoint <- BS.readFile outFile
                            assertEqual "Accepted input checkpoint was lost" "SELECT 1; SELECT 2;" checkpoint
                            outExists <- doesFileExist outFile
                            when outExists (removeFile outFile)
                            statsExists <- doesFileExist statsFile
                            when statsExists (removeFile statsFile)
                        )
                        [(["--strategy", "ddmin-only"], "ddmin-only"), (["--no-slicing", "--no-closure"], "full")]
                    observed <- BSC.readFile calls
                    assertEqual "Each run must physically execute its own preflight" "xx" observed
        , testCase "CLI surfaces --jobs help and rejects invalid counts" $ do
            mapM_
                ( \extra -> do
                    (helpCode, helpOut, _) <- runOccamCLI ("--help" : extra)
                    assertEqual "help must exit zero" ExitSuccess helpCode
                    assertBool "help hides --jobs" ("--jobs" `isInfixOf` helpOut)
                    mapM_
                        ( \jobs -> withCLIInput "SELECT 1;" $ \input outFile -> do
                            (code, _, _) <-
                                runOccamCLI ([input, "--oracle", "exit 0", "--out", outFile, "--jobs", jobs, "--quiet"] ++ extra)
                            assertBool ("invalid --jobs was accepted: " ++ jobs) (code /= ExitSuccess)
                            exists <- doesFileExist outFile
                            assertBool "reduction started with invalid --jobs" (not exists)
                        )
                        ["0", "-1", "abc"]
                )
                [[], ["--strategy", "ddmin-only"]]
        , testCase "CLI rejects bad column modes before any oracle call" $
            withCLIInput "SELECT 1;" $ \input outFile -> do
                (helpCode, helpOut, _) <- runOccamCLI ["--help"]
                assertEqual "help must exit zero" ExitSuccess helpCode
                assertBool "help hides --column-mode" ("--column-mode" `elem` words helpOut)
                mapM_
                    ( \mode -> do
                        (code, _, _) <-
                            runOccamCLI [input, "--oracle", "exit 0", "--out", outFile, "--column-mode", mode, "--quiet"]
                        assertBool ("bad --column-mode was accepted: " ++ mode) (code /= ExitSuccess)
                        exists <- doesFileExist outFile
                        assertBool "reduction started with bad --column-mode" (not exists)
                    )
                    ["bogus", "single-atomic"]
        , testCase "CLI requires --intra-statement for column modes and starts no oracle" $
            withCLIInput "SELECT 1;" $ \input outFile ->
                withCLIInput "" $ \calls _ -> do
                    let oracle = "cat >/dev/null; printf x >> " ++ show calls
                    mapM_
                        ( \mode -> do
                            (code, _, err) <-
                                runOccamCLI [input, "--oracle", oracle, "--out", outFile, "--column-mode", mode, "--quiet"]
                            assertBool ("column mode without intra-statement was accepted: " ++ mode) (code /= ExitSuccess)
                            assertBool "wrong rejection reason" ("--intra-statement" `elem` words err)
                            exists <- doesFileExist outFile
                            assertBool "reduction started without --intra-statement" (not exists)
                        )
                        ["single", "atomic"]
                    observed <- BSC.readFile calls
                    assertEqual "oracle ran before validation" "" observed
        , testCase "help starts with a compact usage line and points to the dialect list" $ do
            (helpCode, helpOut, _) <- runOccamCLI ["--help"]
            assertEqual "help must exit zero" ExitSuccess helpCode
            assertEqual
                "usage and options should appear before any explanatory text"
                ["Usage: occam INPUT.sql (--oracle COMMAND | --result-oracle) [OPTIONS]", "", "Available options:"]
                (take 3 (lines helpOut))
            assertEqual
                "input, oracle, and output should be listed first"
                ["INPUT.sql", "-c,--oracle", "-o,--out"]
                (map (takeWhile (/= ' ') . dropWhile (== ' ')) (take 3 (drop 3 (lines helpOut))))
            assertBool "missing dialect list pointer" ("See --list-dialects" `isInfixOf` helpOut)
            (listCode, dialects, _) <- runOccamCLI ["--list-dialects"]
            assertEqual "dialect list must exit zero" ExitSuccess listCode
            mapM_
                ( \dialect ->
                    let name = T.unpack (dialectName dialect)
                     in assertBool ("--list-dialects hides dialect: " ++ name) (name `elem` words dialects)
                )
                ([minBound .. maxBound] :: [SQLDialect])
        , testCase "native result oracle removes irrelevant statements through the CLI" $
            mapM_
                ( \(dialect, sql) ->
                    withCLIInput sql $ \input _ -> do
                        (code, out, err) <- runOccamCLI [input, "--dialect", dialect, "--result-oracle", "--quiet"]
                        assertEqual err ExitSuccess code
                        assertEqual "only the result-producing statement remains" "SELECT 42 AS answer;" out
                )
                [ ("sqlite", "CREATE TABLE noise (x INT);SELECT 42 AS answer;CREATE TABLE tail (x INT);")
                , ("duckdb", "SELECT [1,2] AS ignored;SELECT 42 AS answer;CREATE TABLE tail (x INT);")
                ]
        , testCase "default stdout includes the best reduction under a two-call budget" $
            withCLIInput "SELECT 1; SELECT 2;" $ \input _ -> do
                let predicate = "grep -q 'SELECT 2'"
                (code, out, _) <-
                    runOccamCLI [input, "-c", predicate, "--max-oracle-executions", "2", "--quiet"]
                assertBool "budget exhaustion must remain nonzero" (code /= ExitSuccess)
                assertEqual "preflight must leave one call for reduction" "SELECT 2;" out
        , testCase "--out - keeps stdout pure SQL under --verbose" $
            withCLIInput "CREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\nINSERT INTO t VALUES (2);\n" $ \input _ -> do
                (code, out, _) <- runOccamCLI [input, "--oracle", "grep -q INSERT", "--out", "-", "--verbose"]
                assertEqual "reduction failed" ExitSuccess code
                assertBool "stdout missing reduced SQL" ("INSERT" `isInfixOf` out)
        , testCase "--dump-facts emits per-statement JSON and stays hidden from --help" $
            withCLIInput "CREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\n" $ \input _ -> do
                (helpCode, helpOut, _) <- runOccamCLI ["--help"]
                assertEqual "help must exit zero" ExitSuccess helpCode
                assertBool "--dump-facts leaked into --help" (not ("dump-facts" `isInfixOf` helpOut))
                (code, out, err) <- runOccamCLI [input, "--dump-facts", "--dialect", "sqlite"]
                assertEqual ("--dump-facts failed: " ++ err) ExitSuccess code
                case eitherDecodeStrict' (BSC.pack out) :: Either String Value of
                    Left e -> assertFailure ("--dump-facts output is not JSON: " ++ e)
                    Right _ -> assertBool "missing statements key" ("\"statements\"" `isInfixOf` out)
        , testCase "informational actions exit 0 without input file" $ do
            (dCode, dOut, _) <- runOccamCLI ["--list-dialects"]
            assertEqual "--list-dialects exit code" ExitSuccess dCode
            assertBool "missing sqlite in --list-dialects" ("sqlite" `isInfixOf` dOut)
            assertBool "missing postgresql in --list-dialects" ("postgresql" `isInfixOf` dOut)

            (mCode, mOut, _) <- runOccamCLI ["--list-column-modes"]
            assertEqual "--list-column-modes exit code" ExitSuccess mCode
            assertBool "missing single in --list-column-modes" ("single" `isInfixOf` mOut)
            assertBool "missing atomic in --list-column-modes" ("atomic" `isInfixOf` mOut)

            (sCode, sOut, _) <- runOccamCLI ["--list-strategies"]
            assertEqual "--list-strategies exit code" ExitSuccess sCode
            assertBool "missing full in --list-strategies" ("full" `isInfixOf` sOut)
            assertBool "missing ddmin-only in --list-strategies" ("ddmin-only" `isInfixOf` sOut)

            (pCode, pOut, _) <- runOccamCLI ["--print-defaults"]
            assertEqual "--print-defaults exit code" ExitSuccess pCode
            case eitherDecodeStrict' (BSC.pack pOut) :: Either String (Map.Map String Value) of
                Left e -> assertFailure ("--print-defaults is not JSON: " ++ e)
                Right defaults -> do
                    assertEqual "default timeout" (Just (Number 10)) (Map.lookup "timeout" defaults)
                    assertEqual "default jobs" (Just (Number 1)) (Map.lookup "jobs" defaults)
                    assertEqual "default dialect" (Just (String "generic")) (Map.lookup "dialect" defaults)
                    assertEqual "default strategy" (Just (String "full")) (Map.lookup "strategy" defaults)
                    assertEqual
                        "default statement_order"
                        (Just (String "ranked-fallback"))
                        (Map.lookup "statement_order" defaults)
                    assertEqual "default literal_batches" (Just (Bool False)) (Map.lookup "literal_batches" defaults)
                    assertEqual "default ast_batching" (Just (Bool False)) (Map.lookup "ast_batching" defaults)
                    assertEqual "default adaptive_values" (Just (Bool False)) (Map.lookup "adaptive_values" defaults)
                    assertEqual "default log level" (Just (String "warn")) (Map.lookup "log_level" defaults)
                    assertEqual "json log level" (Just (String "info")) (Map.lookup "log_level_json" defaults)
        , testCase "--completion generates valid shell scripts and supports dynamic completion" $ do
            (bCode, bOut, _) <- runOccamCLI ["--completion", "bash"]
            assertEqual "--completion bash exit code" ExitSuccess bCode
            assertBool "bash completion missing function" ("_occam()" `isInfixOf` bOut)
            assertBool "bash completion missing complete" ("-F _occam" `isInfixOf` bOut)
            assertBool "bash completion missing dynamic hook" ("--bash-completion-index" `isInfixOf` bOut)

            (zCode, zOut, _) <- runOccamCLI ["--completion", "zsh"]
            assertEqual "--completion zsh exit code" ExitSuccess zCode
            assertBool "zsh completion missing compdef" ("#compdef occam" `isInfixOf` zOut)
            assertBool "zsh completion missing compadd" ("compadd" `isInfixOf` zOut)
            assertBool "zsh completion missing dynamic hook" ("--bash-completion-enriched" `isInfixOf` zOut)

            (fCode, fOut, _) <- runOccamCLI ["--completion", "fish"]
            assertEqual "--completion fish exit code" ExitSuccess fCode
            assertBool "fish completion missing complete command" ("--command occam" `isInfixOf` fOut)
            assertBool "fish completion missing function" ("_occam" `isInfixOf` fOut)
            assertBool "fish completion missing dynamic hook" ("--bash-completion-enriched" `isInfixOf` fOut)

            -- Dynamic option completion
            (cCode, cOut, _) <-
                runOccamCLI
                    ["--bash-completion-index", "1", "--bash-completion-word", "occam", "--bash-completion-word", "--"]
            assertEqual "completion options exit code" ExitSuccess cCode
            assertBool "completion includes --print-defaults" ("--print-defaults" `isInfixOf` cOut)
            assertBool "completion includes --dialect" ("--dialect" `isInfixOf` cOut)
            assertBool "completion includes --strategy" ("--strategy" `isInfixOf` cOut)
            assertBool "completion includes --completion" ("--completion" `isInfixOf` cOut)

            -- Dynamic argument completion (completeWith)
            (dCode, dOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--dialect"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "dialect completion exit code" ExitSuccess dCode
            assertBool "completion includes postgresql" ("postgresql" `isInfixOf` dOut)
            assertBool "completion includes mysql" ("mysql" `isInfixOf` dOut)
            assertBool "completion includes sqlite" ("sqlite" `isInfixOf` dOut)

            (sCode, sOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--strategy"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "strategy completion exit code" ExitSuccess sCode
            assertBool "completion includes full" ("full" `isInfixOf` sOut)
            assertBool "completion includes ddmin-only" ("ddmin-only" `isInfixOf` sOut)
            assertBool "completion includes audit-only" ("audit-only" `isInfixOf` sOut)

            (mCode, mOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--completion"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "shell completion exit code" ExitSuccess mCode
            assertBool "completion includes bash" ("bash" `isInfixOf` mOut)
            assertBool "completion includes zsh" ("zsh" `isInfixOf` mOut)
            assertBool "completion includes fish" ("fish" `isInfixOf` mOut)

            (colCode, colOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--color"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "color completion exit code" ExitSuccess colCode
            assertBool "completion includes auto" ("auto" `isInfixOf` colOut)
            assertBool "completion includes always" ("always" `isInfixOf` colOut)
            assertBool "completion includes never" ("never" `isInfixOf` colOut)

            (llCode, llOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--log-level"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "log-level completion exit code" ExitSuccess llCode
            assertBool "completion includes debug" ("debug" `isInfixOf` llOut)
            assertBool "completion includes info" ("info" `isInfixOf` llOut)
            assertBool "completion includes warn" ("warn" `isInfixOf` llOut)
            assertBool "completion includes error" ("error" `isInfixOf` llOut)

            (lfCode, lfOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--log-format"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "log-format completion exit code" ExitSuccess lfCode
            assertBool "completion includes text" ("text" `isInfixOf` lfOut)
            assertBool "completion includes json" ("json" `isInfixOf` lfOut)

            (cmCode, cmOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--column-mode"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "column-mode completion exit code" ExitSuccess cmCode
            assertBool "completion includes off" ("off" `isInfixOf` cmOut)
            assertBool "completion includes single" ("single" `isInfixOf` cmOut)
            assertBool "completion includes atomic" ("atomic" `isInfixOf` cmOut)

            -- Shell completion case-insensitivity and invalid shell rejection
            (upperCode, upperOut, _) <- runOccamCLI ["--completion", "BASH"]
            assertEqual "--completion BASH exit code" ExitSuccess upperCode
            assertBool "case-insensitive completion missing function" ("_occam()" `isInfixOf` upperOut)

            (invCode, _, invErr) <- runOccamCLI ["--completion", "powershell"]
            assertEqual "--completion powershell exit failure" (ExitFailure 1) invCode
            assertBool "invalid shell error message" ("Expected 'bash', 'zsh', or 'fish'" `isInfixOf` invErr)
        , testCase "CLI requires in-place mode to overwrite the input" $
            withCLIInput "SELECT 1; SELECT 2;" $ \input _ -> do
                (code, _, err) <- runOccamCLI [input, "-c", "exit 0", "--out", input]
                assertEqual ("--out input must exit 2: " ++ err) (ExitFailure 2) code
                inputAfter <- BS.readFile input
                assertEqual "input file changed" "SELECT 1; SELECT 2;" inputAfter
        , testCase "CLI preserves input and backup permissions for in-place output" $
            withCLIInput "SELECT 1; SELECT 2;" $ \input _ -> do
                let backup = input ++ ".bak"
                BS.writeFile backup "old backup"
                flip finally (removeFile backup) $
                    OracleChecks.checkInPlacePermissions input backup $ do
                        (code, _, err) <-
                            runOccamCLI [input, "--in-place", "--dialect", "sqlite", "--result-oracle", "--quiet"]
                        assertEqual err ExitSuccess code
                        original <- BS.readFile backup
                        assertEqual "backup contains the original input" "SELECT 1; SELECT 2;" original
        , testCase "CLI rejects --stats paths that collide with input or output" $
            withCLIInput "SELECT 1;" $ \input outFile -> do
                (inputCode, _, inputErr) <-
                    runOccamCLI [input, "-c", "exit 0", "--stats", input]
                assertEqual ("--stats input must exit 2: " ++ inputErr) (ExitFailure 2) inputCode
                inputAfter <- BS.readFile input
                assertEqual "input was overwritten by stats" "SELECT 1;" inputAfter

                (outputCode, _, outputErr) <-
                    runOccamCLI [input, "-c", "exit 0", "--out", outFile, "--stats", outFile]
                assertEqual ("--stats output must exit 2: " ++ outputErr) (ExitFailure 2) outputCode

                let backup = input ++ ".bak"
                (backupCode, _, backupErr) <-
                    runOccamCLI [input, "-i", "-c", "exit 0", "--stats", backup]
                assertEqual ("--stats backup must exit 2: " ++ backupErr) (ExitFailure 2) backupCode
                backupExists <- doesFileExist backup
                when backupExists (removeFile backup)
        , testCase "CLI strictly rejects conflicting arguments with exit code 2" $ do
            (code1, _, err1) <- runOccamCLI ["input.sql", "--verbose", "--quiet", "-c", "exit 0"]
            assertEqual "verbose + quiet must exit 2" (ExitFailure 2) code1
            assertBool
                "wrong error for verbose + quiet"
                ("Cannot specify both --verbose and --quiet" `isInfixOf` err1)

            (code2, _, err2) <- runOccamCLI ["input.sql", "-i", "-o", "out.sql", "-c", "exit 0"]
            assertEqual "in-place + out must exit 2" (ExitFailure 2) code2
            assertBool
                "wrong error for in-place + out"
                ("Cannot specify both --in-place and --out" `isInfixOf` err2)

            (code3, _, err3) <- runOccamCLI ["-", "-i", "-c", "exit 0"]
            assertEqual "in-place on stdin must exit 2" (ExitFailure 2) code3
            assertBool "wrong error for in-place on stdin" ("Cannot modify stdin in place" `isInfixOf` err3)

            (code4, _, err4) <-
                runOccamCLI ["input.sql", "--strategy", "ddmin-only", "--slicing", "-c", "exit 0"]
            assertEqual "ddmin-only + slicing must exit 2" (ExitFailure 2) code4
            assertBool
                "wrong error for ddmin-only + slicing"
                ("--slicing conflicts with --strategy ddmin-only" `isInfixOf` err4)

            (code5, _, err5) <-
                runOccamCLI ["input.sql", "--strategy", "audit-only", "--intra-statement", "-c", "exit 0"]
            assertEqual "audit-only + intra-statement must exit 2" (ExitFailure 2) code5
            assertBool
                "wrong error for audit-only + intra-statement"
                ("--intra-statement conflicts with --strategy audit-only" `isInfixOf` err5)

            (code6, _, err6) <-
                runOccamCLI ["input.sql", "--strategy", "ddmin-only", "--intra-statement", "-c", "exit 0"]
            assertEqual "ddmin-only + intra-statement must exit 2" (ExitFailure 2) code6
            assertBool
                "wrong error for ddmin-only + intra-statement"
                ("--intra-statement conflicts with --strategy ddmin-only" `isInfixOf` err6)

            (code7, _, err7) <-
                runOccamCLI ["input.sql", "--strategy", "audit-only", "--unrestricted", "-c", "exit 0"]
            assertEqual "audit-only + unrestricted must exit 2" (ExitFailure 2) code7
            assertBool
                "wrong error for audit-only + unrestricted"
                ("--unrestricted conflicts with --strategy audit-only" `isInfixOf` err7)

            (code8, _, err8) <-
                runOccamCLI
                    ["input.sql", "--strategy", "audit-only", "--statement-order", "complement-first", "-c", "exit 0"]
            assertEqual "audit-only + statement-order must exit 2" (ExitFailure 2) code8
            assertBool
                "wrong error for audit-only + statement-order"
                ("--statement-order conflicts with --strategy audit-only" `isInfixOf` err8)

            (code9, _, err9) <-
                runOccamCLI ["input.sql", "--literal-batches", "-c", "exit 0"]
            assertEqual "literal-batches without intra-statement must exit 2" (ExitFailure 2) code9
            assertBool
                "wrong error for literal-batches without intra-statement"
                ("--literal-batches requires --intra-statement" `isInfixOf` err9)

            (code10, _, err10) <-
                runOccamCLI ["input.sql", "--adaptive-values", "-c", "exit 0"]
            assertEqual "adaptive-values without intra-statement must exit 2" (ExitFailure 2) code10
            assertBool
                "wrong error for adaptive-values without intra-statement"
                ("--adaptive-values requires --intra-statement" `isInfixOf` err10)

            (code11, _, err11) <-
                runOccamCLI ["input.sql", "--ast-batching", "-c", "exit 0"]
            assertEqual "ast-batching without intra-statement must exit 2" (ExitFailure 2) code11
            assertBool
                "wrong error for ast-batching without intra-statement"
                ("--ast-batching requires --intra-statement" `isInfixOf` err11)
        , testCase "CLI exposes search policy flags in completion and stats" $
            withCLIInput "SELECT 1234, 1234, 9999, 9999;" $ \input outFile -> do
                let statsFile = outFile ++ ".stats.json"
                (ordCompCode, ordCompOut, _) <-
                    runOccamCLI
                        [ "--bash-completion-index"
                        , "2"
                        , "--bash-completion-word"
                        , "occam"
                        , "--bash-completion-word"
                        , "--statement-order"
                        , "--bash-completion-word"
                        , ""
                        ]
                assertEqual "statement-order completion exit code" ExitSuccess ordCompCode
                assertBool "completion includes ranked-fallback" ("ranked-fallback" `isInfixOf` ordCompOut)
                assertBool "completion includes ranked" ("ranked" `isInfixOf` ordCompOut)
                assertBool "completion includes subset-first" ("subset-first" `isInfixOf` ordCompOut)
                assertBool "completion includes complement-first" ("complement-first" `isInfixOf` ordCompOut)
                assertBool "completion includes byte-weighted" ("byte-weighted" `isInfixOf` ordCompOut)

                (code, _, err) <-
                    runOccamCLI
                        [ input
                        , "-c"
                        , "sql=$(cat); [ \"$sql\" = 'SELECT 1234, 1234, 9999, 9999;' ] || [ \"$sql\" = 'SELECT 0, 0, 9999, 9999;' ]"
                        , "--dialect"
                        , "sqlite"
                        , "--statement-order"
                        , "complement-first"
                        , "--intra-statement"
                        , "--literal-batches"
                        , "--adaptive-values"
                        , "--out"
                        , outFile
                        , "--stats"
                        , statsFile
                        , "--quiet"
                        ]
                assertEqual ("CLI policy run failed: " ++ err) ExitSuccess code
                reduced <- BS.readFile outFile
                assertEqual "literal-batches coupled shrink works through CLI" "SELECT 0, 0, 9999, 9999;" reduced
                statsBytes <- BS.readFile statsFile
                case eitherDecodeStrict' statsBytes :: Either String Value of
                    Left e -> assertFailure ("stats.json did not decode: " ++ e)
                    Right val -> do
                        assertBool "stats missing statement_order" ("\"complement-first\"" `BSC.isInfixOf` statsBytes)
                        assertBool "stats missing literal_batches" ("\"literal_batches\":true" `BSC.isInfixOf` statsBytes)
                        assertBool "stats missing adaptive_values" ("\"adaptive_values\":true" `BSC.isInfixOf` statsBytes)
                        let _ = val
                        pure ()
                removeFile statsFile
        , testCase "CLI rejects missing input files with exit code 1" $ do
            (code, _, err) <- runOccamCLI ["nonexistent-test-file-never-created.sql", "-c", "exit 0"]
            assertEqual "missing input file must exit 1" (ExitFailure 1) code
            assertBool "error message mentions missing file" ("No such file or directory" `isInfixOf` err)
        , testCase "--quiet suppresses jobs without reset warning" $
            withCLIInput "CREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\n" $ \input _ -> do
                (code, _, err) <- runOccamCLI [input, "-c", "grep -q INSERT", "-j", "2", "--quiet", "--out", "-"]
                assertEqual "reduction should succeed" ExitSuccess code
                assertBool "warning should be suppressed by --quiet" (not ("state contamination" `isInfixOf` err))
        , testCase "CLI reads stdin only with explicit '-' and rejects omitted input" $ do
            (okCode, okOut, okErr) <-
                runOccamCLIWithStdin
                    ["-", "--dialect", "sqlite", "--result-oracle", "--quiet"]
                    "CREATE TABLE noise (x INT);SELECT 42 AS answer;"
            assertEqual ("explicit '-' stdin reduction failed: " ++ okErr) ExitSuccess okCode
            assertEqual "explicit '-' reduced SQL mismatch" "SELECT 42 AS answer;" okOut

            (missCode, _, missErr) <-
                runOccamCLIWithStdin ["-c", "exit 0", "--quiet"] "SELECT 1;"
            assertBool "omitted input must be rejected even when stdin is piped" (missCode /= ExitSuccess)
            assertBool "missing input error should name INPUT.sql" ("Missing: INPUT.sql" `isInfixOf` missErr)
        , testCase "CLI rejects --stats - stdout collision before reading stdin" $ do
            bin <- getOccamBinary
            let baseProc =
                    if bin == "cabal"
                        then proc "cabal" ["run", "-v0", "occam", "--", "-", "-c", "exit 0", "--stats", "-"]
                        else proc bin ["-", "-c", "exit 0", "--stats", "-"]
                cliProc =
                    baseProc
                        { std_in = CreatePipe
                        , std_out = CreatePipe
                        , std_err = CreatePipe
                        }
            withCreateProcess cliProc $ \mIn _ mErr ph -> do
                -- Intentionally keep stdin open without EOF; validation must fail fast.
                res <- timeout 10000000 (waitForProcess ph)
                case res of
                    Nothing -> do
                        terminateProcess ph
                        _ <- waitForProcess ph
                        assertFailure "CLI blocked on stdin before rejecting --stats - collision"
                    Just code -> do
                        errText <- maybe (pure "") (fmap BSC.unpack . BSC.hGetContents) mErr
                        assertEqual ("--stats - with stdout must exit 2: " ++ errText) (ExitFailure 2) code
                        assertBool
                            "wrong error for --stats - with stdout"
                            ("--stats - cannot be combined" `isInfixOf` errText)
                forM_ mIn hClose
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
            -- Duplicates join in-flight owner or hit completed cache without re-executing.
            assertEqual
                "every duplicate shared without re-executing"
                3
                (cacheHits stats + cacheJoins stats)
        , testCase "native cache keys preserve literal tokens and source-dependent SQL" $ do
            forM_ [SQLite, DuckDB] $ \dialect -> do
                let key = nativeOracleCacheKey dialect
                a <- key "SELECT 'a b' AS note, - 23 AS score;"
                assertBool "an explicitly named literal SELECT is canonicalizable" (isJust a)
                assertEqual "only ordinary inter-token whitespace is removed" a
                    =<< key "\nSELECT\t'a b' AS note,-23 AS score ; "
                b <- key "SELECT 'ab' AS note, -23 AS score;"
                assertBool "literal bytes remain distinct" (isJust b && a /= b)
                assertEqual "comments require exact bytes" Nothing =<< key "SELECT /* a */ 2 AS note;"
                assertEqual "derived column names require exact bytes" Nothing =<< key "SELECT 1 + 2;"
                assertEqual "source-reading functions require exact bytes" Nothing
                    =<< key "SELECT current_query() AS note;"
                withSemicolon <- key "SELECT 'a;''b' AS note ;"
                assertBool "quoted semicolon remains eligible" (isJust withSemicolon)
                assertEqual "quoted semicolon keeps exact token bytes" withSemicolon
                    =<< key "SELECT\t'a;''b' AS note;"
                assertEqual "multiple statements require exact bytes" Nothing
                    =<< key "SELECT 1 AS n; SELECT 2 AS n;"
        , testCase "direct cacheLookup and newCache behavior" $ do
            let precompKey = BSC.pack "SELECT 42;"
                precompObs = OracleFinished ExitSuccess
                precomputed = Map.singleton precompKey precompObs
            cache <- newCache precomputed

            -- Hit on precomputed with CacheEnabled records a hit
            r1 <- cacheLookup cache CacheEnabled precompKey
            assertEqual "precomputed hit enabled" (LookupHit precompObs) r1
            s1 <- cacheStats cache
            assertEqual "one hit" 1 (cacheHits s1)
            assertEqual "zero misses" 0 (cacheMisses s1)

            -- Hit on precomputed with CacheDisabled does NOT record a hit
            r2 <- cacheLookup cache CacheDisabled precompKey
            assertEqual "precomputed hit disabled" (LookupHit precompObs) r2
            s2 <- cacheStats cache
            assertEqual "hits unchanged" 1 (cacheHits s2)

            -- Fresh query with CacheDisabled records a miss
            let freshKey = BSC.pack "SELECT 99;"
            r3 <- cacheLookup cache CacheDisabled freshKey
            assertEqual "fresh query disabled" LookupMiss r3
            s3 <- cacheStats cache
            assertEqual "one miss" 1 (cacheMisses s3)

            -- Fresh query with CacheEnabled records a miss
            r4 <- cacheLookup cache CacheEnabled freshKey
            assertEqual "fresh query enabled" LookupMiss r4
            s4 <- cacheStats cache
            assertEqual "two misses" 2 (cacheMisses s4)
        , testCase "direct in-flight lifecycle, join reservation, and completion" $ do
            cache <- newCache Map.empty
            let candidate = BSC.pack "SELECT candidate;"
                obs = OracleFinished ExitSuccess

            -- 1. Install candidate in-flight for worker ordinal 10
            cacheInstallInflight cache candidate 10
            r1 <- cacheLookup cache CacheEnabled candidate
            assertEqual "in-flight query returns join" (LookupJoin 10) r1
            s1 <- cacheStats cache
            assertEqual "joins incremented" 1 (cacheJoins s1)

            -- 2. Wrong ordinal cannot complete the in-flight entry
            cacheCompleteInflight cache candidate 99 (Just obs)
            r2 <- cacheLookup cache CacheEnabled candidate
            assertEqual "still in-flight after mismatched owner complete" (LookupJoin 10) r2
            s2 <- cacheStats cache
            assertEqual "joins incremented again" 2 (cacheJoins s2)

            -- 3. Correct ordinal completes the in-flight entry
            cacheCompleteInflight cache candidate 10 (Just obs)
            r3 <- cacheLookup cache CacheEnabled candidate
            assertEqual "completed entry returns hit" (LookupHit obs) r3
            s3 <- cacheStats cache
            assertEqual "hits incremented" 1 (cacheHits s3)

            -- 4. Complete on non-inflight candidate does nothing
            let nonInflight = BSC.pack "SELECT never_inflight;"
            cacheCompleteInflight cache nonInflight 10 (Just obs)
            r4 <- cacheLookup cache CacheEnabled nonInflight
            assertEqual "non-inflight complete leaves candidate as miss" LookupMiss r4
        , testCase "direct cacheRemoveInflight cancels and evicts in-flight entry" $ do
            cache <- newCache Map.empty
            let candidate = BSC.pack "SELECT to_cancel;"
            cacheInstallInflight cache candidate 5
            r1 <- cacheLookup cache CacheEnabled candidate
            assertEqual "installed" (LookupJoin 5) r1
            cacheRemoveInflight cache candidate 5
            r2 <- cacheLookup cache CacheEnabled candidate
            assertEqual "evicted after cancellation" LookupMiss r2
        , testCase "observationAdmission filters valid results" $ do
            assertEqual "success is admitted" CacheAdmit (observationAdmission (OracleFinished ExitSuccess))
            assertEqual
                "exit failure is admitted"
                CacheAdmit
                (observationAdmission (OracleFinished (ExitFailure 1)))
            assertEqual "timeout is rejected" CacheReject (observationAdmission OracleTimedOut)
            assertEqual "reset failure is rejected" CacheReject (observationAdmission OracleResetFailed)
            assertEqual
                "io failure is rejected"
                CacheReject
                (observationAdmission (OracleIOFailed (T.pack "fail")))
        , testCase "concurrent HAMT cache inserts and lookups" $ do
            cache <- newCache Map.empty
            let workerCount = 8
                itemsPerWorker = 30
                obs = OracleFinished ExitSuccess
            doneVars <- sequence [newEmptyMVar | _ <- [1 .. workerCount]]
            forM_ (zip [0 .. workerCount - 1] doneVars) $ \(wId, doneVar) ->
                forkIO $ do
                    forM_ [0 .. itemsPerWorker - 1] $ \i -> do
                        let key = BSC.pack ("SELECT " ++ show wId ++ ", " ++ show i ++ ";")
                        cacheInstallInflight cache key wId
                        cacheCompleteInflight cache key wId (Just obs)
                        res <- cacheLookup cache CacheEnabled key
                        assertEqual "concurrent lookup hit" (LookupHit obs) res
                    putMVar doneVar ()
            forM_ doneVars takeMVar
            stats <- cacheStats cache
            assertEqual "all completed lookups hit" (workerCount * itemsPerWorker) (cacheHits stats)
        ]

-- ----------------------------------------------------------------------------
-- Selected frontend dialects
-- ----------------------------------------------------------------------------

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
               , check DuckDB "SELECT * FROM range(3) QUALIFY row_number() OVER () = 1" "SELECT 2"
               , check MsSql "SELECT TOP (1) [a] FROM [t]" "SELECT 2"
               , check Oracle "SELECT seq.NEXTVAL FROM dual" "SELECT 2 FROM dual"
               , check BigQuery "SELECT * EXCEPT (a) FROM `project.dataset.t`" "SELECT 2"
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

withGraph :: SQLDialect -> BS.ByteString -> (DependencyGraph -> IO ()) -> IO ()
withGraph dialect source action = do
    result <- parseSQL (defaultParseOptions dialect) source
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right parsed -> action (analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed))

-- | Create a test oracle pool backed by an in-process predicate.
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
            , poolReportCount = Nothing
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
            , poolReportCount = Nothing
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
-- In-process SQLite Oracle Tests
-- ----------------------------------------------------------------------------

sqliteOracleTests :: TestTree
sqliteOracleTests =
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

-- ----------------------------------------------------------------------------
-- In-process DuckDB Oracle Tests
-- ----------------------------------------------------------------------------

duckdbOracleTests :: TestTree
duckdbOracleTests =
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

-- ----------------------------------------------------------------------------
-- Benchmark Metrics & Reporting
-- ----------------------------------------------------------------------------

benchmarkMetricsTests :: TestTree
benchmarkMetricsTests =
    testGroup
        "Benchmark Metrics & Reporting"
        [ testCase "computeAttemptAuc returns 1.0 for empty or invalid inputs" do
            assertEqual "zero initial bytes" 1.0 (computeAttemptAuc 0 [] 10)
            assertEqual "negative attempts" 1.0 (computeAttemptAuc 1000 [] 0)
        , testCase "computeAttemptAuc integrates discrete AUC correctly" do
            let curve =
                    [ EvaluationPoint 1.0 1 1000 Nothing False True
                    , EvaluationPoint 2.0 2 500 Nothing False True
                    , EvaluationPoint 3.0 3 100 Nothing False False
                    , EvaluationPoint 4.0 4 200 Nothing False True
                    ]
                auc = computeAttemptAuc 1000 curve 4
            assertBool "AUC matches (1 + 0.5 + 0.5 + 0.2) / 4 = 0.55" (abs (auc - 0.55) < 1e-6)
        , testCase "engineOverheadMicros aggregates FFI, decode, and candidateBuild" do
            let snapshot =
                    MetricsSnapshot
                        { snapshotPolicy = MetricPolicy "ddmin" "none" 1
                        , snapshotFamilies =
                            Map.fromList
                                [ (TagStatement, emptyFamilyCounters {candidateBuildMicros = 75})
                                ]
                        , snapshotGeneratedSkipped = Map.empty
                        , snapshotParseFfiMicros =
                            Map.fromList [(ParseStatementOnly, 100), (ParseSyntaxChunks, 50)]
                        , snapshotParseDecodeMicros =
                            Map.fromList [(ParseStatementOnly, 25)]
                        }
            assertEqual "engine overhead sum" (Just 250) (engineOverheadMicros (Just snapshot))
        , testCase "renderSummaryJson outputs structured 4D schema JSON" do
            let run1 =
                    RunSummary
                        { runName = "caseA__default__r1"
                        , runCase = Just "caseA"
                        , runArm = Just "default"
                        , runRep = Just "r1"
                        , runNameWarning = Nothing
                        , runReducedBytes = Just 200
                        , runReducedStatements = Just 2
                        , runReductionRatio = Just 0.2
                        , runOracleAttempts = Just 4
                        , runAccepted = Just 2
                        , runCacheHits = Just 3
                        , runAttemptAuc = Just 0.55
                        , runEngineOverheadUs = Just 250
                        , runElapsedSeconds = Just 7.25
                        , runStatus = "completed"
                        }
                run2 = missingStatsSummary "caseA__intra__r1"
                encoded = BSL.toStrict (encode (renderSummaryJson [run1, run2]))
            assertBool "has runs array" (BSC.pack "\"runs\"" `BS.isInfixOf` encoded)
            assertBool "has run1" (BSC.pack "caseA__default__r1" `BS.isInfixOf` encoded)
            assertBool "has missing-stats" (BSC.pack "missing-stats" `BS.isInfixOf` encoded)
        , testCase "run name parsing keeps extra separators and flags malformed names" do
            let extended = missingStatsSummary "caseA__default__r1__post"
                malformed = missingStatsSummary "caseA__default"
            assertEqual "case parsed" (Just "caseA") (runCase extended)
            assertEqual "arm parsed" (Just "default") (runArm extended)
            assertEqual "extra separators stay in repetition" (Just "r1__post") (runRep extended)
            assertEqual "valid name has no warning" Nothing (runNameWarning extended)
            assertEqual "malformed case is not guessed" Nothing (runCase malformed)
            assertEqual "malformed arm is not guessed" Nothing (runArm malformed)
            assertEqual "malformed rep is not guessed" Nothing (runRep malformed)
            assertBool "malformed name carries warning" (maybe False (not . T.null) (runNameWarning malformed))
        , testCase "generateReportForDirectory scans run directories and writes reports" do
            tempDir <- getTemporaryDirectory
            let testDir = tempDir </> "occam-test-benchmark-metrics"
                runDir1 = testDir </> "caseA__default__r1"
                runDir2 = testDir </> "caseA__intra__r1"
            createDirectoryIfMissing True runDir1
            createDirectoryIfMissing True runDir2
            let budget = sampleEvaluationBudget
                metrics = sampleEvaluationMetrics
                stats =
                    Stats
                        { initial_bytes = 1000
                        , reduced_bytes = 200
                        , reduction_ratio = 80.0
                        , initial_statements = 10
                        , reduced_statements = 2
                        , oracle_executions = 4
                        , oracle_attempts = 4
                        , initial_oracle_observations = 0
                        , cache_hits = 3
                        , cache_misses = 1
                        , cache_joins = 0
                        , cache_hit_ratio = 0.75
                        , cancelled_observations = 0
                        , status = EvaluationCompleted
                        , reduction_settings = Null
                        , evaluation_budget = budget
                        , evaluation_metrics = metrics
                        , initial_oracle_seconds = 0.01
                        , elapsed_seconds = 7.25
                        }
            BSL.writeFile (runDir1 </> "stats.json") (encode stats)
            runs <- generateReportForDirectory testDir
            assertEqual "scanned 2 runs" 2 (length runs)
            summaryExists <- doesFileExist (testDir </> "summary.json")
            reportExists <- doesFileExist (testDir </> "report.md")
            assertBool "summary.json created" summaryExists
            assertBool "report.md not created" (not reportExists)
            removeDirectoryRecursive testDir
        , testCase "evaluation and metrics emit snake_case JSON keys" $ do
            let encoded = BSL.toStrict (encode sampleEvaluationMetrics)
                hasKey key = BSC.pack ("\"" ++ key ++ "\"") `BS.isInfixOf` encoded
            mapM_
                (\key -> assertBool ("missing snake_case key: " ++ key) (hasKey key))
                [ "evaluation_status"
                , "evaluation_total_seconds"
                , "evaluation_curve"
                , "point_call_index"
                , "evaluation_cost_attribution"
                , "snapshot_policy"
                , "snapshot_families"
                , "snapshot_generated_skipped"
                , "snapshot_parse_ffi_micros"
                , "physical_starts"
                , "tag"
                , "counters"
                ]
            assertBool "legacy camelCase evaluation key leaked" (not (hasKey "evaluationStatus"))
            assertBool "legacy camelCase snapshot key leaked" (not (hasKey "snapshotPolicy"))
        , testCase "legacy camelCase evaluation and metrics JSON still decodes" $ do
            let legacyPoint =
                    BSC.pack
                        "{\"pointElapsedSeconds\":1.5,\"pointCallIndex\":3,\"pointCandidateBytes\":100,\"pointCandidateStatements\":null,\"pointCacheHit\":false,\"pointAccepted\":true}"
                expectedPoint = EvaluationPoint 1.5 3 100 Nothing False True
                legacyConfig =
                    BSC.pack
                        "{\"evaluationCacheEnabled\":true,\"evaluationJobs\":1,\"evaluationTotalTimeoutMicros\":null,\"evaluationMaxOracleExecutions\":null,\"evaluationPreflightMicros\":0,\"evaluationPreflightAttempts\":0,\"evaluationPreflightResetMicros\":null,\"evaluationPreflightOracleMicros\":null,\"evaluationInitialStatements\":10}"
            assertEqual
                "legacy point decodes"
                (Right expectedPoint)
                (eitherDecodeStrict' legacyPoint :: Either String EvaluationPoint)
            assertEqual
                "legacy config decodes"
                (Right sampleEvaluationBudget)
                (eitherDecodeStrict' legacyConfig :: Either String EvaluationConfig)
            let legacySnapshot =
                    BSC.pack
                        "{\"snapshotPolicy\":{\"metricSearchPolicy\":\"ddmin\",\"metricColumnMode\":\"none\",\"metricJobs\":1},\"snapshotFamilies\":[{\"tag\":{\"kind\":\"statement\"},\"counters\":{\"physicalStarts\":2,\"retiredAccept\":1,\"retiredReject\":0,\"retiredInconclusive\":0,\"physicalCompletions\":0,\"completedUnselected\":0,\"completedUnselectedInteractionWallMicros\":0,\"canceled\":0,\"canceledInteractionWallMicros\":0,\"failed\":0,\"failedInteractionWallMicros\":0,\"candidateBuildMicros\":0,\"oracleMicros\":null,\"resetMicros\":null}}],\"snapshotGeneratedSkipped\":{},\"snapshotParseFfiMicros\":{},\"snapshotParseDecodeMicros\":{}}"
            assertEqual
                "legacy metrics snapshot decodes"
                (Right sampleMetricsSnapshot)
                (eitherDecodeStrict' legacySnapshot :: Either String MetricsSnapshot)
        ]

sampleEvaluationBudget :: EvaluationConfig
sampleEvaluationBudget =
    EvaluationConfig
        { evaluationCacheEnabled = True
        , evaluationJobs = 1
        , evaluationTotalTimeoutMicros = Nothing
        , evaluationMaxOracleExecutions = Nothing
        , evaluationPreflightMicros = 0
        , evaluationPreflightAttempts = 0
        , evaluationPreflightResetMicros = Nothing
        , evaluationPreflightOracleMicros = Nothing
        , evaluationInitialStatements = Just 10
        }

sampleMetricsSnapshot :: MetricsSnapshot
sampleMetricsSnapshot =
    MetricsSnapshot
        { snapshotPolicy = MetricPolicy "ddmin" "none" 1
        , snapshotFamilies =
            Map.fromList
                [ (TagStatement, emptyFamilyCounters {physicalStarts = 2, retiredAccept = 1})
                ]
        , snapshotGeneratedSkipped = Map.empty
        , snapshotParseFfiMicros = Map.empty
        , snapshotParseDecodeMicros = Map.empty
        }

sampleEvaluationMetrics :: EvaluationMetrics
sampleEvaluationMetrics =
    EvaluationMetrics
        { evaluationStatus = EvaluationCompleted
        , evaluationFailure = Nothing
        , evaluationCacheUsed = True
        , evaluationBudget = sampleEvaluationBudget
        , evaluationTotalSeconds = 7.25
        , evaluationReductionSeconds = 7.25
        , evaluationOracleSeconds = 5.0
        , evaluationResetSeconds = 0.0
        , evaluationPreflightResetSeconds = Nothing
        , evaluationPreflightOracleSeconds = Nothing
        , evaluationOracleAttempts = 4
        , evaluationDatabaseObservations = 4
        , evaluationCompletedOracleProcesses = 4
        , evaluationAcceptedObservations = 2
        , evaluationRejectedObservations = 2
        , evaluationTimedOutObservations = 0
        , evaluationResetFailures = 0
        , evaluationOracleErrors = 0
        , evaluationCancelledObservations = 0
        , evaluationOracleOutcomes = Map.empty
        , evaluationCacheHits = 3
        , evaluationCacheMisses = 1
        , evaluationCacheJoins = 0
        , evaluationValidationSeconds = 0.0
        , evaluationValidationOracleSeconds = 0.0
        , evaluationValidationResetSeconds = 0.0
        , evaluationValidationObservations = 0
        , evaluationValidationDatabaseObservations = 0
        , evaluationValidationErrors = 0
        , evaluationValidationOutcomes = Map.empty
        , evaluationValidationPassed = True
        , evaluationAuditSeconds = 0.0
        , evaluationAuditChecks = 0
        , evaluationAuditCompleted = True
        , evaluationAuditSkipped = False
        , evaluationMinimalityClaim = MinimalityNotEstablished
        , evaluationCurve =
            [ EvaluationPoint 1.0 1 1000 Nothing False True
            , EvaluationPoint 2.0 2 500 Nothing False True
            , EvaluationPoint 3.0 3 100 Nothing False False
            , EvaluationPoint 4.0 4 200 Nothing False True
            ]
        , evaluationStages = []
        , evaluationCostAttribution = Just sampleMetricsSnapshot
        }

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
                    , tablePartitionParent = Nothing
                    , tableInheritParents = []
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
                    , dataWriteOnly = False
                    }
        ReadTableFact name ->
            ReadOp
                ReadFact
                    { readSource = NameRef (Name [Ident name]) False
                    , readMode = ReadData
                    , readColumns = []
                    , readOnly = False
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
        predecessors = graphFastEdges graph
        successors = successorIndex predecessors
        asInts = IntSet.fromList . map unStatementId . Set.toList
        allIds = asInts (graphStmts graph)
        mismatches =
            [ (roots, actual, boundaryActual, expected)
            | roots <- rootSets
            , let actual = closeSet graph roots
                  expected =
                    Set.unions
                        [ Map.findWithDefault Set.empty root expectedByTarget
                        | root <- Set.toList roots
                        ]
                  kept = asInts roots
                  boundaryActual =
                    closeFromBoundary predecessors successors kept (allIds `IntSet.difference` kept)
            , actual /= expected || boundaryActual /= asInts expected
            ]
     in counterexample
            ("closeSet differs from reachability (roots, actual, boundary actual, expected): " ++ show mismatches)
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

module Main (main) where

import ColumnChecks qualified
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeStrict', encode, withObject, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
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
    SemanticFacts (..),
    SessionInit (..),
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
    duckdbContractOracle,
    executeDuckDBCandidate,
    generateDuckDBContract,
    isDuckDBAvailable,
 )
import Occam.Oracle.Sqlite (
    executeSqliteCandidate,
    generateSqliteContract,
    splitStatements,
    sqliteContractObservation,
    sqliteContractOracle,
 )
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
import System.Process (readCreateProcessWithExitCode, readProcess, readProcessWithExitCode, shell)
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
runOccamCLI cliArgs = do
    bin <- getOccamBinary
    if bin == "cabal"
        then readProcessWithExitCode "cabal" (["run", "-v0", "occam", "--"] ++ cliArgs) ""
        else readProcessWithExitCode bin cliArgs ""

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
                ["Usage: occam INPUT.sql --oracle COMMAND [OPTIONS]", "", "Available options:"]
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
        , testCase "CLI rejects missing input files with exit code 1" $ do
            (code, _, err) <- runOccamCLI ["nonexistent-test-file-never-created.sql", "-c", "exit 0"]
            assertEqual "missing input file must exit 1" (ExitFailure 1) code
            assertBool "error message mentions missing file" ("No such file or directory" `isInfixOf` err)
        , testCase "--quiet suppresses jobs without reset warning" $
            withCLIInput "CREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\n" $ \input _ -> do
                (code, _, err) <- runOccamCLI [input, "-c", "grep -q INSERT", "-j", "2", "--quiet", "--out", "-"]
                assertEqual "reduction should succeed" ExitSuccess code
                assertBool "warning should be suppressed by --quiet" (not ("state contamination" `isInfixOf` err))
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
        , testCase "sqliteContractObservation keeps execution errors distinct from rejection" $ do
            let sql = "CREATE TABLE t (a INT);\nSELECT * FROM t;"
            contractRes <- generateSqliteContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    observation <- sqliteContractObservation contract "SYNTAX ERROR;"
                    case observation of
                        OracleIOFailed err ->
                            assertBool "error detail is preserved" (not (T.null err))
                        other ->
                            assertFailure ("expected OracleIOFailed, got " ++ show other)
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
        ]

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
        , testCase "duckdbContractObservation keeps execution errors distinct from rejection" $ do
            available <- isDuckDBAvailable
            assertBool "DuckDB library must be detected and available" available
            let sql = "CREATE TABLE t (a INT);\nSELECT * FROM t;"
            contractRes <- generateDuckDBContract sql
            case contractRes of
                Left err -> assertFailure (T.unpack err)
                Right contract -> do
                    observation <- duckdbContractObservation contract "SYNTAX ERROR;"
                    case observation of
                        OracleIOFailed err ->
                            assertBool "error detail is preserved" (not (T.null err))
                        other ->
                            assertFailure ("expected OracleIOFailed, got " ++ show other)
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

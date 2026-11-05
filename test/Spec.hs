{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Char8 qualified as BSC
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Occam.Core (Span (..), Statement (..), StatementId (..))
import Occam.Dependency
import Occam.Oracle
import Occam.Reducer
import Occam.Slicer
import Occam.TreeSitter
import System.Directory (
    doesFileExist,
    findExecutable,
    removeDirectoryRecursive,
    removeFile,
 )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Occam Test Suite"
        [ phase1Tests
        , phase2Tests
        , phase3Tests
        , phase4Tests
        , phase5Tests
        ]

-- ----------------------------------------------------------------------------
-- Phase 1: Tree-sitter
-- ----------------------------------------------------------------------------

phase1Tests :: TestTree
phase1Tests =
    testGroup
        "Phase 1: Tree-sitter"
        [ testCase "Root node & multi-statement extraction" $ do
            let sampleSQL =
                    BSC.unlines
                        [ "CREATE TABLE users (id INT, name TEXT);"
                        , "INSERT INTO users VALUES (1, 'alice');"
                        , "SELECT * FROM users WHERE id = 1;"
                        ]
            withParser $ \parser -> do
                res <- withParsedSQL parser sampleSQL $ \tree -> do
                    root <- rootNode tree
                    rType <- nodeType root
                    assertEqual "Expected root node type 'program'" "program" rType
                    stmts <- extractStatements tree sampleSQL
                    assertEqual "Expected 3 statements" 3 (length stmts)
                case res of
                    Nothing -> assertFailure "Failed to parse sample SQL"
                    Just () -> pure ()
        , testCase "Byte-span cutting and replacement surgery" $ do
            let sampleSQL =
                    BSC.unlines
                        [ "CREATE TABLE users (id INT, name TEXT);"
                        , "INSERT INTO users VALUES (1, 'alice');"
                        , "SELECT * FROM users WHERE id = 1;"
                        ]
            withParser $ \parser -> do
                res <- withParsedSQL parser sampleSQL $ \tree -> do
                    stmts <- extractStatements tree sampleSQL
                    let stmt2 = stmts !! 1
                        Span sStart sEnd = stmtSpan stmt2
                        cutSQL = cutSpan sStart sEnd sampleSQL
                    assertBool "Cut statement still present in cutSQL" (not (BSC.isInfixOf "INSERT INTO" cutSQL))

                    case stmts of
                        (stmt1 : _) -> do
                            let Span s1Start s1End = stmtSpan stmt1
                                replacedSQL = replaceSpan s1Start s1End "CREATE TABLE t (x INT);" sampleSQL
                            assertBool "Replacement failed" (BSC.isInfixOf "CREATE TABLE t (x INT);" replacedSQL)
                        [] -> assertFailure "Expected non-empty statements"
                case res of
                    Nothing -> assertFailure "Failed to parse sample SQL"
                    Just () -> pure ()
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
            withParser $ \parser -> do
                res <- withParsedSQL parser script $ \tree -> do
                    pairs <- extractStatementNodes tree script
                    let indexedNodes = [(stmtId s, n) | (s, n) <- pairs]
                    graph <- analyzeStatements indexedNodes script

                    let depsS3 = Map.findWithDefault Set.empty (StatementId 3) (graphEdges graph)
                        depsS5 = Map.findWithDefault Set.empty (StatementId 5) (graphEdges graph)
                    assertBool "s3 should not depend on s2 (t2)" (not (Set.member (StatementId 2) depsS3))
                    assertBool "s5 should not depend on s2 (t2)" (not (Set.member (StatementId 2) depsS5))
                case res of
                    Nothing -> assertFailure "Failed to parse script for dependency test"
                    Just () -> pure ()
        , testCase "Backward slicing from crashing statement" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0;"
                        ]
            withParser $ \parser -> do
                res <- withParsedSQL parser script $ \tree -> do
                    pairs <- extractStatementNodes tree script
                    let indexedNodes = [(stmtId s, n) | (s, n) <- pairs]
                    graph <- analyzeStatements indexedNodes script
                    let slice = backwardSlice (StatementId 5) graph
                    assertBool
                        "CRITICAL: Statement 2 (t2) should have been pruned by slice"
                        (not (Set.member (StatementId 2) slice))
                    assertBool "Statement 1 (CREATE TABLE t1) must be in slice" (Set.member (StatementId 1) slice)
                    assertBool "Statement 5 (Crash trigger) must be in slice" (Set.member (StatementId 5) slice)
                case res of
                    Nothing -> assertFailure "Failed to parse script for slicing test"
                    Just () -> pure ()
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
            withParser $ \parser -> do
                res <- withParsedSQL parser script $ \tree -> do
                    pairs <- extractStatementNodes tree script
                    let indexed = [(stmtId s, n) | (s, n) <- pairs]
                    graph <- analyzeStatements indexed script
                    assertBool
                        "{s1, s3, s5} should be a valid closure"
                        (isValidClosure graph (Set.fromList [StatementId 1, StatementId 3, StatementId 5]))
                    assertBool
                        "{s3, s5} should be invalid (dangling s1)"
                        (not (isValidClosure graph (Set.fromList [StatementId 3, StatementId 5])))
                case res of
                    Nothing -> assertFailure "Failed to parse script for closure check"
                    Just () -> pure ()
        , testCase "End-to-end multi-statement reduction" $ do
            let script =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                        ]
                oracle sql = do
                    let hasT1 = BSC.isInfixOf "CREATE TABLE t1" sql
                        hasIns = BSC.isInfixOf "INSERT INTO t1" sql
                        hasSel = BSC.isInfixOf "SELECT" sql && BSC.isInfixOf "WHERE a > 0" sql
                    pure (hasT1 && hasIns && hasSel)
            reduced <- reduceSQL oracle script
            ok <- oracle reduced
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
            simplifiedExpr <- simplifyExpressions exprOracle complexExprSQL
            assertBool "Failed to simplify 1 = 1" (not (BSC.isInfixOf "1 = 1" simplifiedExpr))
            assertBool "Failed to simplify + 10" (not (BSC.isInfixOf "10" simplifiedExpr))
        ]

-- ----------------------------------------------------------------------------
-- Phase 4: Crash Oracle
-- ----------------------------------------------------------------------------

phase4Tests :: TestTree
phase4Tests =
    testGroup
        "Phase 4: Crash Oracle"
        [ testCase "CrashReport semantic equivalence" $ do
            let frame0 = StackFrame 0 "ExecScan" (Just "execScan.c") (Just 42)
                frame1 = StackFrame 1 "standard_ExecutorRun" (Just "execMain.c") (Just 300)
                frame2 = StackFrame 2 "PortalRun" (Just "pquery.c") (Just 780)
                frameAlt = StackFrame 0 "errfinish" (Just "elog.c") (Just 100)

                reportA = CrashReport (SignalCrash 11) (Just 0x401000) [frame0, frame1, frame2]
                reportB = CrashReport (SignalCrash 11) (Just 0x401050) [frame0, frame1, frame2]
                reportDiffSig = CrashReport (SignalCrash 6) (Just 0x401000) [frame0, frame1, frame2]
                reportDiffFrame = CrashReport (SignalCrash 11) (Just 0x401000) [frameAlt, frame1, frame2]

            assertBool "Expected reportA and reportB to be equivalent" (isEquivalentCrash 3 reportA reportB)
            assertBool
                "Expected different signals to not match"
                (not (isEquivalentCrash 3 reportA reportDiffSig))
            assertBool
                "Expected different top frame to not match"
                (not (isEquivalentCrash 3 reportA reportDiffFrame))
        , testCase "JSON contract parsing and round-trip" $ do
            let jsonPayload =
                    BSC.pack
                        "{\"kind\":\"SIGSEGV\",\"signal\":11,\"pc\":4198400,\"frames\":[\
                        \{\"index\":0,\"function\":\"ExecScan\",\"file\":\"execScan.c\",\"line\":42},\
                        \{\"index\":1,\"function\":\"standard_ExecutorRun\",\"file\":\"execMain.c\",\"line\":300}\
                        \]}"
            case parseCrashReportJSON jsonPayload of
                Nothing -> assertFailure "Failed to parse crash report JSON"
                Just parsed -> do
                    assertEqual "Expected SignalCrash 11" (SignalCrash 11) (reportKind parsed)
                    assertEqual "Expected PC" (Just 4198400) (reportPC parsed)
                    assertEqual "Expected 2 frames" 2 (length (reportFrames parsed))
                    case reportFrames parsed of
                        (f0 : _) -> do
                            assertEqual "Expected function ExecScan" "ExecScan" (frameFunction f0)
                            assertEqual "Expected file execScan.c" (Just "execScan.c") (frameFile f0)
                            assertEqual "Expected line 42" (Just 42) (frameLine f0)
                        [] -> assertFailure "Expected non-empty frames"

            let frame0 = StackFrame 0 "ExecScan" (Just "execScan.c") (Just 42)
                reportA = CrashReport (SignalCrash 11) (Just 0x401000) [frame0]
                rtJSON = crashReportToJSON reportA
            case parseCrashReportJSON rtJSON of
                Nothing -> assertFailure "Failed to parse round-tripped JSON"
                Just rtReport ->
                    assertBool "Round-tripped CrashReport did not match" (isEquivalentCrash 1 reportA rtReport)
        , testCase "GDB-MI output parsing" $ do
            let gdbMiSample =
                    T.unlines
                        [ "*stopped,reason=\"signal-received\",signal-name=\"SIGSEGV\",signal-meaning=\"Segmentation fault\",frame={level=\"0\",addr=\"0x0000000000401000\",func=\"ExecScan\",file=\"execScan.c\",line=\"42\"}"
                        , "^done,stack=[frame={level=\"0\",addr=\"0x0000000000401000\",func=\"ExecScan\",file=\"execScan.c\",line=\"42\"},frame={level=\"1\",addr=\"0x0000000000402000\",func=\"standard_ExecutorRun\",file=\"execMain.c\",line=\"300\"}]"
                        ]
            case parseGdbMiOutput gdbMiSample of
                Nothing -> assertFailure "Failed to parse GDB-MI sample output"
                Just miReport -> do
                    assertEqual "Expected SignalCrash 11" (SignalCrash 11) (reportKind miReport)
                    assertEqual "Expected 2 stack frames" 2 (length (reportFrames miReport))
                    case reportFrames miReport of
                        (f0 : _) -> assertEqual "Expected top function ExecScan" "ExecScan" (frameFunction f0)
                        [] -> assertFailure "Expected non-empty frames"
        , testCase "Lightweight reset mechanism" $ do
            resNone <- executeReset Nothing
            assertBool "executeReset Nothing should return True" resNone
            resTrue <- executeReset (Just "true")
            assertBool "executeReset (Just 'true') should return True" resTrue
            resFalse <- executeReset (Just "false")
            assertBool "executeReset (Just 'false') should return False" (not resFalse)
        , testCase "External oracle runner & bug slippage prevention" $ do
            let frame0 = StackFrame 0 "ExecScan" (Just "execScan.c") (Just 42)
                frameAlt = StackFrame 0 "errfinish" (Just "elog.c") (Just 100)
                reportA = CrashReport (SignalCrash 11) (Just 0x401000) [frame0]
                reportDiff = CrashReport (SignalCrash 11) (Just 0x401000) [frameAlt]
                mockScript =
                    "echo '{\"kind\":\"SIGSEGV\",\"signal\":11,\"frames\":[{\"index\":0,\"function\":\"ExecScan\"}]}'"
                cfgMatch =
                    OracleConfig
                        { oracleCommand = "sh"
                        , oracleArgs = ["-c", mockScript]
                        , oracleTimeoutMicros = 2000000
                        , oracleResetCommand = Nothing
                        , oracleExpectedCrash = Just (1, reportA)
                        }
                cfgMismatch = cfgMatch {oracleExpectedCrash = Just (1, reportDiff)}

            matched <- runExternalOracle cfgMatch "SELECT 1;"
            assertBool "runExternalOracle should have matched" matched
            slipped <- runExternalOracle cfgMismatch "SELECT 1;"
            assertBool "runExternalOracle should reject mismatch" (not slipped)
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

phase5Tests :: TestTree
phase5Tests =
    testGroup
        "Phase 5: CLI"
        [ testCase "CLI --version output" $ do
            (verExit, verOut, _) <- runOccamCLI ["--version"]
            assertEqual "occam --version failed" ExitSuccess verExit
            assertBool "occam --version output missing 'Occam v'" ("Occam v" `isInfixOf` verOut)
        , testCase "CLI --help output" $ do
            (helpExit, helpOut, _) <- runOccamCLI ["--help"]
            assertEqual "occam --help failed" ExitSuccess helpExit
            assertBool
                "occam --help output missing 'Usage:'"
                ("Usage:" `isInfixOf` helpOut && "occam" `isInfixOf` helpOut)
        , testCase "CLI non-reproducing input error" $ do
            let tmpSQL = "test_cli_input.sql"
                tmpOut = "test_cli_out"
            BSC.writeFile tmpSQL "SELECT 1;"
            (badExit, _, badErr) <- runOccamCLI [tmpSQL, "--oracle", "false", "--out", tmpOut]
            assertBool "Expected failure on non-reproducing input" (badExit /= ExitSuccess)
            assertBool
                "Expected error message in stderr"
                ("Error: Initial input does not reproduce target crash!" `isInfixOf` badErr)
            removeFile tmpSQL
        , testCase "CLI end-to-end reduction and artifact generation" $ do
            let tmpSQL = "test_cli_e2e.sql"
                tmpOut = "test_cli_e2e_out"
                sampleCrashSQL =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                        ]
            BSC.writeFile tmpSQL sampleCrashSQL
            let pythonOracle =
                    "python3 -c \"import sys; s=sys.stdin.read(); sys.exit(0 if 'WHERE a > 0' in s and 'CREATE TABLE t1' in s and 'INSERT INTO t1' in s else 1)\""
            (okExit, okOut, okErr) <- runOccamCLI [tmpSQL, "--oracle", pythonOracle, "--out", tmpOut]
            assertEqual ("CLI reduction failed: " ++ okErr ++ "\nStdout: " ++ okOut) ExitSuccess okExit

            minExists <- doesFileExist (tmpOut </> "minimal.sql")
            assertBool "minimal.sql not created" minExists
            replayExists <- doesFileExist (tmpOut </> "replay.sh")
            assertBool "replay.sh not created" replayExists
            statsExists <- doesFileExist (tmpOut </> "stats.json")
            assertBool "stats.json not created" statsExists

            minSQL <- BSC.readFile (tmpOut </> "minimal.sql")
            assertBool "minimal.sql contains unpruned t2" (not (BSC.isInfixOf "t2" minSQL))
            assertBool "minimal.sql contains unpruned ORDER BY" (not (BSC.isInfixOf "ORDER BY" minSQL))
            assertBool "minimal.sql contains unsimplified 1 = 1" (not (BSC.isInfixOf "1 = 1" minSQL))
            assertBool "minimal.sql missing required WHERE a > 0" (BSC.isInfixOf "WHERE a > 0" minSQL)

            removeFile tmpSQL
            removeDirectoryRecursive tmpOut
        ]

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless, when)
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
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
    putStrLn "=================================================="
    putStrLn "Running Occam Test Suite (Phases 1-5)"
    putStrLn "=================================================="

    testPhase1TreeSitter
    testPhase2DependencyAnalysis
    testPhase3Reducer
    testPhase4CrashOracle
    testPhase5CLI
    putStrLn "\n=================================================="
    putStrLn "All Occam tests PASSED successfully!"
    putStrLn "=================================================="

testPhase1TreeSitter :: IO ()
testPhase1TreeSitter = do
    putStrLn "\n--- [Phase 1] Tree-sitter Surgery Verification ---"
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
            when (rType /= "program") $
                fail "Expected root node type to be 'program'"

            stmts <- extractStatements tree sampleSQL
            when (length stmts /= 3) $
                fail ("Expected 3 statements, got " ++ show (length stmts))

            -- Cut statement 2
            let stmt2 = stmts !! 1
                Span sStart sEnd = stmtSpan stmt2
                cutSQL = cutSpan sStart sEnd sampleSQL

            when (BSC.isInfixOf "INSERT INTO" cutSQL) $
                fail "Cut statement still present in cutSQL!"

            -- Replace statement 1
            stmt1 <- case stmts of
                (s : _) -> pure s
                [] -> fail "Expected non-empty statement list"
            let Span s1Start s1End = stmtSpan stmt1
                replacedSQL = replaceSpan s1Start s1End "CREATE TABLE t (x INT);" sampleSQL

            unless (BSC.isInfixOf "CREATE TABLE t (x INT);" replacedSQL) $
                fail "Replacement failed!"

        case res of
            Nothing -> do
                putStrLn "Failed to parse sample SQL"
                exitFailure
            Just () ->
                putStrLn "Phase 1: Tree-sitter byte surgery passed."

testPhase2DependencyAnalysis :: IO ()
testPhase2DependencyAnalysis = do
    putStrLn "\n--- [Phase 2] Semantic Dependency & Slicing Verification ---"
    -- Classical 5-statement crashing scenario from research proposal section 5.2:
    -- s1: Def(t1)
    -- s2: Def(t2)  <- completely irrelevant!
    -- s3: Mod(t1)
    -- s4: Use(t1)
    -- s5: Use(t1)  <- crash trigger
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
            root <- rootNode tree
            namedStmts <- nodeNamedChildren root

            let indexedNodes = zip (map StatementId [1 ..]) namedStmts
            graph <- analyzeStatements indexedNodes script

            putStrLn "Dependency DAG edges (predecessors):"
            forM_ (Map.toList (graphEdges graph)) $ \(sid, preds) ->
                putStrLn $ "  " ++ show sid ++ " depends on: " ++ show (Set.toList preds)

            -- Statement 2 (CREATE TABLE t2) must have NO incoming dependencies from s3, s4, s5
            let depsS3 = Map.findWithDefault Set.empty (StatementId 3) (graphEdges graph)
                depsS5 = Map.findWithDefault Set.empty (StatementId 5) (graphEdges graph)

            when (Set.member (StatementId 2) depsS3) $
                fail "s3 should not depend on s2 (t2)!"
            when (Set.member (StatementId 2) depsS5) $
                fail "s5 should not depend on s2 (t2)!"

            -- Backward slicing from crashing statement (s5)
            let slice = backwardSlice (StatementId 5) graph
            putStrLn $ "Backward slice from Statement 5 (Crash): " ++ show (Set.toList slice)

            when (Set.member (StatementId 2) slice) $
                fail "CRITICAL: Statement 2 (irrelevant table t2) should have been pruned by slice!"

            unless (Set.member (StatementId 1) slice) $
                fail "Statement 1 (CREATE TABLE t1) must be in slice!"

            unless (Set.member (StatementId 5) slice) $
                fail "Statement 5 (Crash trigger) must be in slice!"

            putStrLn "Phase 2: Semantic dependency analysis & backward slicing successfully passed!"

        case res of
            Nothing -> do
                putStrLn "Failed to parse script for dependency test"
                exitFailure
            Just () -> pure ()

testPhase3Reducer :: IO ()
testPhase3Reducer = do
    putStrLn "\n--- [Phase 3] Hierarchical Reduction Engine Verification ---"
    let script =
            BSC.unlines
                [ "CREATE TABLE t1 (a INT, b TEXT);"
                , "CREATE TABLE t2 (c INT);"
                , "INSERT INTO t1 VALUES (1, 'x');"
                , "CREATE INDEX i1 ON t1 (a);"
                , "SELECT * FROM t1 WHERE a > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                ]

    -- 1. Closure validity check
    withParser $ \parser -> do
        res <- withParsedSQL parser script $ \tree -> do
            pairs <- extractStatementNodes tree script
            let indexed = [(stmtId s, n) | (s, n) <- pairs]
            graph <- analyzeStatements indexed script

            -- {s1, s3, s5} is valid (s5 -> s3 -> s1 -> empty)
            unless (isValidClosure graph (Set.fromList [StatementId 1, StatementId 3, StatementId 5])) $
                fail "Expected {s1, s3, s5} to be a valid closure!"

            -- {s3, s5} is invalid (s3 needs s1, which is missing)
            when (isValidClosure graph (Set.fromList [StatementId 3, StatementId 5])) $
                fail "Expected {s3, s5} to be an invalid closure due to dangling dependency on s1!"

            putStrLn "Phase 3: Closure validity check passed."
        case res of
            Nothing -> fail "Failed to parse script for closure check"
            Just () -> pure ()

    -- 2. End-to-end reduceSQL test
    -- Simulated crash oracle: requires t1 definition, t1 insert, and select with WHERE a > 0
    let oracle :: Oracle
        oracle sql = do
            let hasCreateTableT1 = BSC.isInfixOf "CREATE TABLE t1" sql
                hasInsertT1 = BSC.isInfixOf "INSERT INTO t1" sql
                hasSelectWhere = BSC.isInfixOf "SELECT" sql && BSC.isInfixOf "WHERE a > 0" sql
            pure (hasCreateTableT1 && hasInsertT1 && hasSelectWhere)

    reduced <- reduceSQL oracle script
    putStrLn "Reduced SQL output:"
    putStrLn "--------------------------------------------------"
    putStr (BSC.unpack reduced)
    putStrLn "--------------------------------------------------"

    -- Verify that the oracle still triggers on the reduced output
    ok <- oracle reduced
    unless ok $
        fail "Reduced SQL failed to satisfy the crash oracle!"

    -- Verify that table t2 is completely pruned
    when (BSC.isInfixOf "t2" reduced) $
        fail "CRITICAL: Table t2 was not pruned from reduced SQL!"

    -- Verify that redundant index i1 is pruned
    when (BSC.isInfixOf "i1" reduced) $
        fail "CRITICAL: Redundant index i1 was not pruned!"

    -- Verify that redundant ORDER BY clause is pruned
    when (BSC.isInfixOf "ORDER BY" reduced) $
        fail "CRITICAL: Optional ORDER BY clause was not pruned!"

    -- Verify that redundant LIMIT clause is pruned
    when (BSC.isInfixOf "LIMIT" reduced) $
        fail "CRITICAL: Optional LIMIT clause was not pruned!"

    -- Verify that redundant expression AND 1 = 1 is simplified
    when (BSC.isInfixOf "1 = 1" reduced) $
        fail "CRITICAL: Redundant expression '1 = 1' was not simplified!"
    when (BSC.isInfixOf "AND" reduced) $
        fail "CRITICAL: Redundant operator 'AND' was not simplified!"
    -- Verify that necessary statements and clauses are retained
    unless (BSC.isInfixOf "CREATE TABLE t1" reduced) $
        fail "Missing required CREATE TABLE t1!"
    unless (BSC.isInfixOf "INSERT INTO t1" reduced) $
        fail "Missing required INSERT INTO t1!"
    unless (BSC.isInfixOf "WHERE a > 0" reduced) $
        fail "Missing required WHERE a > 0 clause!"

    -- 3. Expression simplification unit test on arithmetic & nested expressions
    let complexExprSQL = "SELECT * FROM t1 WHERE (a + 10) > 0 AND 1 = 1;"
        exprOracle sql = pure (BSC.isInfixOf "a" sql && BSC.isInfixOf "> 0" sql)
    simplifiedExpr <- simplifyExpressions exprOracle complexExprSQL
    putStrLn $ "Simplified expression SQL: " ++ BSC.unpack simplifiedExpr
    when (BSC.isInfixOf "1 = 1" simplifiedExpr) $
        fail "Failed to simplify '1 = 1' in expression!"
    when (BSC.isInfixOf "10" simplifiedExpr) $
        fail "Failed to simplify '+ 10' in expression!"

    putStrLn "Phase 3: Hierarchical reduction engine tests passed!"

testPhase4CrashOracle :: IO ()
testPhase4CrashOracle = do
    putStrLn "\n--- [Phase 4] Semantic Crash Oracle & Environment Verification ---"

    -- 1. Equivalence checker test
    let frame0 = StackFrame 0 "ExecScan" (Just "execScan.c") (Just 42)
        frame1 = StackFrame 1 "standard_ExecutorRun" (Just "execMain.c") (Just 300)
        frame2 = StackFrame 2 "PortalRun" (Just "pquery.c") (Just 780)
        frameAlt = StackFrame 0 "errfinish" (Just "elog.c") (Just 100)

        reportA = CrashReport (SignalCrash 11) (Just 0x401000) [frame0, frame1, frame2]
        reportB = CrashReport (SignalCrash 11) (Just 0x401050) [frame0, frame1, frame2]
        reportDiffSig = CrashReport (SignalCrash 6) (Just 0x401000) [frame0, frame1, frame2]
        reportDiffFrame = CrashReport (SignalCrash 11) (Just 0x401000) [frameAlt, frame1, frame2]

    unless (isEquivalentCrash 3 reportA reportB) $
        fail "Expected reportA and reportB to be equivalent (same signal, same top-3 frames)!"

    when (isEquivalentCrash 3 reportA reportDiffSig) $
        fail "Expected reportA and reportDiffSig to NOT be equivalent (different signals)!"

    when (isEquivalentCrash 3 reportA reportDiffFrame) $
        fail "Expected reportA and reportDiffFrame to NOT be equivalent (different top frame)!"

    putStrLn "Phase 4: CrashReport equivalence checking passed."

    -- 2. JSON contract parser test
    let jsonPayload =
            BSC.pack
                "{\"kind\":\"SIGSEGV\",\"signal\":11,\"pc\":4198400,\"frames\":[\
                \{\"index\":0,\"function\":\"ExecScan\",\"file\":\"execScan.c\",\"line\":42},\
                \{\"index\":1,\"function\":\"standard_ExecutorRun\",\"file\":\"execMain.c\",\"line\":300}\
                \]}"

    case parseCrashReportJSON jsonPayload of
        Nothing -> fail "Failed to parse standard crash report JSON!"
        Just parsed -> do
            when (reportKind parsed /= SignalCrash 11) $
                fail "JSON parser failed to extract SignalCrash 11!"
            when (reportPC parsed /= Just 4198400) $
                fail "JSON parser failed to extract PC!"
            when (length (reportFrames parsed) /= 2) $
                fail "JSON parser failed to extract 2 frames!"
            case reportFrames parsed of
                (f0 : _) -> do
                    when (frameFunction f0 /= "ExecScan") $
                        fail "JSON parser failed to extract function name!"
                    when (frameFile f0 /= Just "execScan.c") $
                        fail "JSON parser failed to extract file name!"
                    when (frameLine f0 /= Just 42) $
                        fail "JSON parser failed to extract line number!"
                [] -> fail "Expected non-empty frames"

    -- Test JSON round-trip
    let rtJSON = crashReportToJSON reportA
    case parseCrashReportJSON rtJSON of
        Nothing -> fail "Failed to parse round-tripped CrashReport JSON!"
        Just rtReport ->
            unless (isEquivalentCrash 3 reportA rtReport) $
                fail "Round-tripped CrashReport did not match original!"

    putStrLn "Phase 4: Structured JSON contract parsing & round-trip passed."

    -- 3. GDB-MI parser test
    let gdbMiSample =
            T.unlines
                [ "*stopped,reason=\"signal-received\",signal-name=\"SIGSEGV\",signal-meaning=\"Segmentation fault\",frame={level=\"0\",addr=\"0x0000000000401000\",func=\"ExecScan\",file=\"execScan.c\",line=\"42\"}"
                , "^done,stack=[frame={level=\"0\",addr=\"0x0000000000401000\",func=\"ExecScan\",file=\"execScan.c\",line=\"42\"},frame={level=\"1\",addr=\"0x0000000000402000\",func=\"standard_ExecutorRun\",file=\"execMain.c\",line=\"300\"}]"
                ]

    case parseGdbMiOutput gdbMiSample of
        Nothing -> fail "Failed to parse GDB-MI sample output!"
        Just miReport -> do
            when (reportKind miReport /= SignalCrash 11) $
                fail "GDB-MI parser failed to extract SignalCrash 11!"
            when (length (reportFrames miReport) /= 2) $
                fail "GDB-MI parser failed to extract stack frames!"
            case reportFrames miReport of
                (f0 : _) ->
                    when (frameFunction f0 /= "ExecScan") $
                        fail "GDB-MI parser failed to extract top function!"
                [] -> fail "Expected non-empty frames from GDB-MI"

    putStrLn "Phase 4: GDB-MI structured output parsing passed."

    -- 4. Reset mechanism test
    resetNone <- executeReset Nothing
    unless resetNone $
        fail "executeReset Nothing should return True!"

    resetTrue <- executeReset (Just "true")
    unless resetTrue $
        fail "executeReset (Just 'true') should return True!"

    resetFalse <- executeReset (Just "false")
    when resetFalse $
        fail "executeReset (Just 'false') should return False!"

    putStrLn "Phase 4: Lightweight reset mechanism passed."

    -- 5. External Oracle integration test
    let mockScript =
            "echo '{\"kind\":\"SIGSEGV\",\"signal\":11,\"frames\":[{\"index\":0,\"function\":\"ExecScan\"},{\"index\":1,\"function\":\"standard_ExecutorRun\"},{\"index\":2,\"function\":\"PortalRun\"}]}'"
        oracleMatchConfig =
            OracleConfig
                { oracleCommand = "sh"
                , oracleArgs = ["-c", mockScript]
                , oracleTimeoutMicros = 2000000
                , oracleResetCommand = Nothing
                , oracleExpectedCrash = Just (3, reportA)
                }

    matched <- runExternalOracle oracleMatchConfig "SELECT 1;"
    unless matched $
        fail "runExternalOracle should have succeeded with matching crash report!"

    -- Mismatched expected crash (bug slippage prevention)
    let oracleMismatchConfig =
            oracleMatchConfig
                { oracleExpectedCrash = Just (3, reportDiffFrame)
                }
    slipped <- runExternalOracle oracleMismatchConfig "SELECT 1;"
    when slipped $
        fail "runExternalOracle should have rejected mismatched crash to prevent bug slippage!"

    putStrLn "Phase 4: External oracle runner & bug slippage prevention passed!"

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

testPhase5CLI :: IO ()
testPhase5CLI = do
    putStrLn "\n--- [Phase 5] CLI Interface & Artifact Packaging Verification ---"

    -- 1. Test --version
    (verExit, verOut, _) <- runOccamCLI ["--version"]
    when (verExit /= ExitSuccess) $
        fail "occam --version failed!"
    unless ("Occam v" `isInfixOf` verOut) $
        fail "occam --version output missing 'Occam v'!"
    putStrLn "Phase 5: CLI --version passed."

    -- 2. Test --help
    (helpExit, helpOut, _) <- runOccamCLI ["--help"]
    when (helpExit /= ExitSuccess) $
        fail "occam --help failed!"
    unless ("Usage:" `isInfixOf` helpOut && "occam" `isInfixOf` helpOut) $
        fail "occam --help output missing 'Usage:' and 'occam'!"
    putStrLn "Phase 5: CLI --help passed."

    -- 3. Test non-reproducing initial input error
    let tmpSQL = "test_cli_input.sql"
        tmpOut = "test_cli_out"
    BSC.writeFile tmpSQL "SELECT 1;"

    (badExit, _, badErr) <- runOccamCLI [tmpSQL, "--oracle", "false", "--out", tmpOut]
    when (badExit == ExitSuccess) $
        fail "Expected occam to fail when initial crash does not reproduce!"
    unless ("Error: Initial input does not reproduce target crash!" `isInfixOf` badErr) $
        fail $
            "Expected error message not found in stderr: " ++ badErr
    putStrLn "Phase 5: Non-reproducing input error check passed."

    -- 4. Test end-to-end reduction run with artifact verification
    let sampleCrashSQL =
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
    when (okExit /= ExitSuccess) $
        fail $
            "occam CLI reduction failed: " ++ okErr ++ "\nStdout: " ++ okOut

    -- Verify artifacts exist
    minExists <- doesFileExist (tmpOut </> "minimal.sql")
    unless minExists $
        fail "Artifact minimal.sql was not created!"

    replayExists <- doesFileExist (tmpOut </> "replay.sh")
    unless replayExists $
        fail "Artifact replay.sh was not created!"

    statsExists <- doesFileExist (tmpOut </> "stats.json")
    unless statsExists $
        fail "Artifact stats.json was not created!"

    -- Verify contents of minimal.sql
    minSQL <- BSC.readFile (tmpOut </> "minimal.sql")
    when (BSC.isInfixOf "t2" minSQL) $
        fail "minimal.sql contains unpruned table t2!"
    when (BSC.isInfixOf "ORDER BY" minSQL) $
        fail "minimal.sql contains unpruned ORDER BY clause!"
    when (BSC.isInfixOf "1 = 1" minSQL) $
        fail "minimal.sql contains unsimplified expression 1 = 1!"
    unless (BSC.isInfixOf "WHERE a > 0" minSQL) $
        fail "minimal.sql missing required WHERE a > 0!"

    -- Clean up temporary test files
    removeFile tmpSQL
    removeDirectoryRecursive tmpOut

    putStrLn "Phase 5: CLI end-to-end reduction & artifact generation passed!"

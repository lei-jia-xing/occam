{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Occam.Core (Span (..), Statement (..), StatementId (..))
import Occam.Dependency
import Occam.Slicer
import Occam.TreeSitter
import System.Exit (exitFailure)

main :: IO ()
main = do
    putStrLn "=================================================="
    putStrLn "Running Occam Test Suite (Phase 1 & Phase 2)"
    putStrLn "=================================================="

    testPhase1TreeSitter
    testPhase2DependencyAnalysis

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

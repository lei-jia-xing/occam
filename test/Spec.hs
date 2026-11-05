{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless, when)
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Occam.Core (Span (..), Statement (..))
import Occam.TreeSitter
import System.Exit (exitFailure)

main :: IO ()
main = do
    putStrLn "Running Occam Tree-sitter integration test..."

    let sampleSQL =
            BSC.unlines
                [ "CREATE TABLE users (id INT, name TEXT);"
                , "INSERT INTO users VALUES (1, 'alice');"
                , "SELECT * FROM users WHERE id = 1;"
                ]

    withParser $ \parser -> do
        res <- withParsedSQL parser sampleSQL $ \tree -> do
            -- 1. Check Root Node
            root <- rootNode tree
            rType <- nodeType root
            putStrLn $ "Root type: " ++ T.unpack rType
            when (rType /= "program") $
                fail "Expected root node type to be 'program'"

            -- 2. Extract multi-statements
            stmts <- extractStatements tree sampleSQL
            putStrLn $ "Extracted " ++ show (length stmts) ++ " statements:"
            mapM_ (\s -> putStrLn ("  [" ++ show (stmtId s) ++ "] " ++ BSC.unpack (stmtRaw s))) stmts

            when (length stmts /= 3) $
                fail ("Expected 3 statements, got " ++ show (length stmts))

            -- 3. Verify Byte-Span Surgery: Cut statement 2 (INSERT)
            let stmt2 = stmts !! 1
                Span sStart sEnd = stmtSpan stmt2
                cutSQL = cutSpan sStart sEnd sampleSQL

            putStrLn "\n--- SQL after cutting statement 2 ---"
            BSC.putStr cutSQL
            putStrLn "------------------------------------"

            when (BSC.isInfixOf "INSERT INTO" cutSQL) $
                fail "Cut statement still present in cutSQL!"

            -- 4. Verify Byte-Span Surgery: Replace statement 1 with a simplified table
            stmt1 <- case stmts of
                (s : _) -> pure s
                [] -> fail "Expected non-empty statement list"
            let Span s1Start s1End = stmtSpan stmt1
                replacedSQL = replaceSpan s1Start s1End "CREATE TABLE t (x INT);" sampleSQL

            putStrLn "\n--- SQL after replacing statement 1 ---"
            BSC.putStr replacedSQL
            putStrLn "--------------------------------------"

            unless (BSC.isInfixOf "CREATE TABLE t (x INT);" replacedSQL) $
                fail "Replacement failed!"

        case res of
            Nothing -> do
                putStrLn "Failed to parse sample SQL"
                exitFailure
            Just () ->
                putStrLn "\nAll Tree-sitter tests PASSED!"

{-# LANGUAGE OverloadedStrings #-}

module ReductionChecks (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Occam.Core (StatementId (..))
import Occam.Dependency (DependencyGraph (..), SQLDialect (..), analyzeStatementsWithDialect)
import Occam.Logger (nullLogger)
import Occam.Reducer (
    ReductionConfig (..),
    SearchStrategy (..),
    defaultReductionConfig,
    reduceSQLWithConfig,
    statementAudit,
    statementDdmin,
 )
import Occam.TreeSitter (countStatements, extractStatementUnits, withParsedSQL, withParser)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Reducer checks"
        [ testCase "Same-named unused table is not a false dependency" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE needed (id INT);"
                        , "CREATE TABLE unrelated (id INT);"
                        , "SELECT needed.id FROM needed;"
                        ]
            withParser $ \parser -> do
                result <- withParsedSQL parser sql $ \tree -> do
                    units <- extractStatementUnits tree sql
                    analyzeStatementsWithDialect SQLite units sql
                case result of
                    Nothing -> assertFailure "Dependency fixture did not parse"
                    Just graph -> do
                        let dependencies = Map.findWithDefault Set.empty (StatementId 3) (graphEdges graph)
                        assertBool "query should depend on its referenced table" (Set.member (StatementId 1) dependencies)
                        assertBool
                            "query must not depend on an unrelated table with the same column name"
                            (Set.notMember (StatementId 2) dependencies)
        , testCase "Configured SQL dialect changes dependency closure" $ do
            let sql = BSC.pack "CREATE TABLE \"MiXeD\" (id INT);\nSELECT id FROM \"mixed\";\n"
                oracle candidate = pure (BSC.isInfixOf "SELECT id" candidate)
                config dialect =
                    defaultReductionConfig
                        { reductionSlicing = False
                        , reductionClosure = True
                        , reductionIntraStatement = False
                        , reductionUnrestricted = False
                        , reductionDialect = dialect
                        }
            postgres <- reduceSQLWithConfig (config PostgreSQL) nullLogger oracle sql
            sqlite <- reduceSQLWithConfig (config SQLite) nullLogger oracle sql
            assertEqual
                "PostgreSQL quoted identifiers retain case"
                (BSC.pack "SELECT id FROM \"mixed\";\n")
                postgres
            assertEqual "SQLite folds quoted identifiers for closure" sql sqlite
        , testCase "Graph-free and dependency-aware modes run identical intra-statement passes" $ do
            let sql = "SELECT 100 + 200;"
                oracle candidate = pure (BSC.isInfixOf "SELECT" candidate)
                shared =
                    defaultReductionConfig
                        { reductionSlicing = False
                        , reductionIntraStatement = True
                        , reductionUnrestricted = False
                        }
            graphFree <- reduceSQLWithConfig (shared {reductionClosure = False}) nullLogger oracle sql
            closure <- reduceSQLWithConfig (shared {reductionClosure = True}) nullLogger oracle sql
            audited <-
                reduceSQLWithConfig
                    (shared {reductionClosure = True, reductionUnrestricted = True})
                    nullLogger
                    oracle
                    sql
            assertBool
                "intra-statement rewriting must make strict progress"
                (BS.length graphFree < BS.length sql)
            assertEqual "disabling dependency closure leaves the rewrite pass unchanged" graphFree closure
            assertEqual
                "the unrestricted fallback/audit composition leaves this result unchanged"
                closure
                audited
        , testCase "Weighted and CDD searches preserve the predicate-required statement" $ do
            let target = BSC.concat ["SELECT 'sentinel", BS.replicate 256 120, "';\n"]
                sql = BSC.concat ["SELECT 1;\nSELECT 2;\nSELECT 3;\nSELECT 4;\n", target]
                oracle candidate = pure (BSC.isInfixOf "sentinel" candidate)
            weighted <- statementDdmin WeightedDD oracle sql
            cdd <- statementDdmin CDD oracle sql
            assertBool
                "weighted search keeps the predicate-required statement"
                (BSC.isInfixOf "sentinel" weighted)
            assertBool "CDD keeps the predicate-required statement" (BSC.isInfixOf "sentinel" cdd)
            assertBool "weighted search removes irrelevant statements" (BS.length weighted < BS.length sql)
            assertBool "CDD removes irrelevant statements" (BS.length cdd < BS.length sql)
            assertBool
                "weighted search removes the unrelated first statement"
                (not (BSC.isInfixOf "SELECT 1" weighted))
            assertBool "CDD removes the unrelated first statement" (not (BSC.isInfixOf "SELECT 1" cdd))
        , testCase "Statement audit reaches a fixed point, permits empty output, and preserves bytes" $ do
            let sql = BSC.unlines ["SELECT 101;", "SELECT 202;", "SELECT 303;"]
                ac = BSC.pack "SELECT 101;\nSELECT 303;\n"
                c = BSC.pack "SELECT 303;\n"
                oracle candidate = pure (candidate == sql || candidate == ac || candidate == c)
            fixedPoint <- statementAudit oracle sql
            assertEqual "a statement becomes removable only after an earlier deletion" c fixedPoint
            empty <- statementAudit (const (pure True)) "SELECT 7;"
            assertEqual "the empty candidate is not prohibited" BS.empty empty
            let bytes = BSC.pack "SELECT 11;\r\nSELECT 22;\n"
                firstStatement = BSC.pack "SELECT 11;\r\n"
                keepFirst candidate = pure (BSC.isInfixOf "SELECT 11" candidate)
            byteResult <- statementDdmin CountDD keepFirst bytes
            assertEqual "retained source bytes, including CRLF, are unchanged" firstStatement byteResult
        , testCase "Statement surgery preserves compound triggers and UTF-8 offsets" $ do
            let prefix = TE.encodeUtf8 "-- café\n"
                unused = BSC.pack "CREATE TABLE unused (id INT);\n"
                trigger =
                    TE.encodeUtf8
                        "CREATE TRIGGER tr AFTER INSERT ON t BEGIN UPDATE t SET x = CASE WHEN x = 'a;b' THEN 'c;d' ELSE x END; INSERT INTO audit VALUES ('x;y'); END;"
                target = TE.encodeUtf8 "SELECT $tag$semi;colon$tag$;\n"
                sql = prefix <> unused <> trigger <> "\n" <> target <> BSC.pack "SELECT 9;\n"
                expected = prefix <> trigger <> "\n" <> target
                oracle candidate = pure (BS.isInfixOf trigger candidate && BS.isInfixOf target candidate)
            statementCount <- countStatements sql
            assertEqual "table, trigger, target, and following SELECT are four source units" 4 statementCount
            reduced <- statementDdmin CountDD oracle sql
            assertEqual "the trigger is one source statement and retained bytes are untouched" expected reduced
        , testCase "Statement counting respects quoted, escaped, and commented semicolons" $ do
            let sql =
                    TE.encodeUtf8
                        "SELECT /*; block */ 'a;b'; -- line; comment\nSELECT [g;h]; SELECT $tag$semi;colon$tag$; SELECT E'a\\';semi';"
            count <- countStatements sql
            assertEqual "each SQL source statement remains one unit" 4 count
        ]

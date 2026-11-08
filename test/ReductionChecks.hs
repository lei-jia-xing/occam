{-# LANGUAGE OverloadedStrings #-}

module ReductionChecks (tests) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Occam.Core (StatementId (..))
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Logger (nullLogger)
import Occam.Parser (ParsedSQL (..), SQLDialect (..), countStatements, parseSQL)
import Occam.Reducer (
    ReductionConfig (..),
    SearchStrategy (..),
    defaultReductionConfig,
    reduceSQL,
    reduceSQLWithConfig,
    statementAudit,
    statementDdmin,
 )
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
            result <- parseSQL SQLite sql
            case result of
                Left err -> assertFailure ("SQL parser transport error: " ++ show err)
                Right parsed -> do
                    let graph = analyzeStatements (parsedUnits parsed)
                        dependencies = Map.findWithDefault Set.empty (StatementId 3) (graphEdges graph)
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
            weighted <- statementDdmin GenericSQL WeightedDD oracle sql
            cdd <- statementDdmin GenericSQL CDD oracle sql
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
                accepted = BSC.pack "SELECT 101;\nSELECT 303;\n"
                final = BSC.pack "SELECT 303;\n"
                oracle candidate = pure (candidate == sql || candidate == accepted || candidate == final)
            fixedPoint <- statementAudit GenericSQL oracle sql
            assertEqual "a statement becomes removable only after an earlier deletion" final fixedPoint
            empty <- statementAudit GenericSQL (const (pure True)) "SELECT 7;"
            assertEqual "the empty candidate is not prohibited" BS.empty empty
            let bytes = BSC.pack "SELECT 11;\r\nSELECT 22;\n"
                firstStatement = BSC.pack "SELECT 11;\r\n"
                keepFirst candidate = pure (BSC.isInfixOf "SELECT 11" candidate)
            byteResult <- statementDdmin GenericSQL CountDD keepFirst bytes
            assertEqual "retained source bytes, including CRLF, are unchanged" firstStatement byteResult
        , testCase "CTE removal preserves a required sibling and untouched source bytes" $ do
            let prefix = TE.encodeUtf8 "-- café\n"
                kept = "kept AS (SELECT 42 AS value)"
                query = "SELECT value FROM kept;\r\n"
                sql = prefix <> "WITH unused AS (SELECT 1), " <> kept <> " " <> query
                oracle candidate = pure (BS.isInfixOf kept candidate && BS.isInfixOf query candidate)
            reduced <- syntaxOnly SQLite oracle sql
            assertBool "the unnecessary CTE is removed" (not (BSC.isInfixOf "unused" reduced))
            assertBool "the required CTE is unchanged" (BS.isInfixOf kept reduced)
            assertBool "the Unicode prefix is untouched" (BS.isPrefixOf prefix reduced)
            assertBool "the query and CRLF ending are untouched" (BS.isSuffixOf query reduced)
            withoutCTEs <-
                syntaxOnly SQLite (pure . BSC.isInfixOf "SELECT 42;") "WITH unused AS (SELECT 1) SELECT 42;"
            assertBool
                "the final unnecessary CTE and WITH keyword can disappear"
                (not (BSC.isInfixOf "WITH" withoutCTEs))
        , testCase "VALUES row reduction excludes the INSERT target column list" $ do
            let sql = "INSERT INTO t(a,b) VALUES (1,2),(42,43);"
                oracle candidate = pure (BSC.isInfixOf "t(a,b)" candidate && BSC.isInfixOf "(42,43)" candidate)
            reduced <- syntaxOnly SQLite oracle sql
            assertEqual
                "only the irrelevant VALUES row and its comma are removed"
                "INSERT INTO t(a,b) VALUES (42,43);"
                reduced
        , testCase "unused projection terms are removed inside a subquery" $ do
            let sql = "SELECT a FROM (SELECT 42 AS a,99 AS b);\r\n"
                oracle candidate = pure (BSC.isInfixOf "SELECT a FROM" candidate && BSC.isInfixOf "42 AS a" candidate)
            reduced <- syntaxOnly SQLite oracle sql
            assertEqual
                "the unused projection and comma disappear without reformatting"
                "SELECT a FROM (SELECT 42 AS a);\r\n"
                reduced
        , testCase "OFFSET can disappear while a required LIMIT remains" $ do
            let sql = "SELECT a FROM t LIMIT 1 OFFSET 0;\r\n"
                oracle candidate = pure (BSC.isInfixOf "SELECT a FROM t" candidate && BSC.isInfixOf "LIMIT 1" candidate)
            reduced <- syntaxOnly SQLite oracle sql
            assertEqual
                "LIMIT and the original ending survive OFFSET removal"
                "SELECT a FROM t LIMIT 1;\r\n"
                reduced
        , testCase "literal scalar subqueries collapse using their exact source span" $ do
            let prefix = TE.encodeUtf8 "-- café\n"
                sql = prefix <> "SELECT (SELECT 42);\r\n"
            reduced <- syntaxOnly SQLite (pure . BSC.isInfixOf "42") sql
            assertEqual "outer Unicode and CRLF bytes remain untouched" (prefix <> "SELECT 42;\r\n") reduced
            tableQuery <- syntaxOnly SQLite (pure . BSC.isInfixOf "42") "SELECT * FROM (SELECT 42);"
            assertBool
                "table subqueries are not replaced by scalar literals"
                (not (BSC.isInfixOf "FROM 42" tableQuery))
        , testCase "Statement surgery preserves compound SQLite triggers and UTF-8 offsets" $ do
            let prefix = TE.encodeUtf8 "-- café\n"
                unused = BSC.pack "CREATE TABLE unused (id INT);\n"
                trigger =
                    TE.encodeUtf8
                        "CREATE TRIGGER tr AFTER INSERT ON t BEGIN UPDATE t SET x = CASE WHEN x = 'a;b' THEN 'c;d' ELSE x END; INSERT INTO audit VALUES ('x;y'); END;"
                target = TE.encodeUtf8 "SELECT 'semi;colon';\r\n"
                sql = prefix <> unused <> trigger <> "\r\n" <> target <> BSC.pack "SELECT 9;\n"
                expected = prefix <> trigger <> "\r\n" <> target
                oracle candidate = pure (BS.isInfixOf trigger candidate && BS.isInfixOf target candidate)
            statementCount <- countOrFail SQLite sql
            assertEqual "table, trigger, target, and following SELECT are four source units" 4 statementCount
            reduced <- statementDdmin SQLite CountDD oracle sql
            assertEqual "the trigger is one source statement and retained bytes are untouched" expected reduced
        , testCase "Statement counting follows each selected SQL dialect" $ do
            sqliteCount <- countOrFail SQLite "SELECT /*; block */ 'a;b'; -- line; comment\nSELECT [g;h];"
            postgresCount <-
                countOrFail PostgreSQL "SELECT $tag$semi;colon$tag$; -- line; comment\nSELECT E'a\\';semi';"
            mysqlCount <- countOrFail MySQL "SELECT `g;h`; -- line; comment\nSELECT 'a;b';"
            assertEqual "SQLite quoted/comment semicolons stay within units" 2 sqliteCount
            assertEqual "PostgreSQL dollar and escaped strings stay within units" 2 postgresCount
            assertEqual "MySQL backticks and strings stay within units" 2 mysqlCount
        , testCase "invalid SQL still counts its raw statement units; transport errors stay explicit" $ do
            invalidCount <- countOrFail SQLite "SELECT FROM ; SELECT 42;"
            assertEqual "invalid syntax does not turn statement count into zero" 2 invalidCount
            invalidUtf8 <- countStatements SQLite (BS.pack [255])
            case invalidUtf8 of
                Left _ -> pure ()
                Right count -> assertFailure ("invalid UTF-8 unexpectedly counted as " ++ show count)
        , testCase "reducer propagates parser transport failures" $ do
            let invalidUtf8 = BS.pack [255]
                acceptingOracle candidate = pure (candidate == invalidUtf8)
            result <-
                try (reduceSQL SQLite acceptingOracle invalidUtf8) ::
                    IO (Either IOException ByteString)
            case result of
                Left _ -> pure ()
                Right _ -> assertFailure "invalid UTF-8 returned as a successful reduction"
        ]

countOrFail :: SQLDialect -> ByteString -> IO Int
countOrFail dialect sql = do
    result <- countStatements dialect sql
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right count -> pure count

syntaxOnly :: SQLDialect -> (BS.ByteString -> IO Bool) -> BS.ByteString -> IO BS.ByteString
syntaxOnly dialect =
    reduceSQLWithConfig
        defaultReductionConfig
            { reductionSlicing = False
            , reductionClosure = False
            , reductionUnrestricted = False
            , reductionIntraStatement = True
            , reductionDialect = dialect
            }
        nullLogger

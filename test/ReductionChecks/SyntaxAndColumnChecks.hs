module ReductionChecks.SyntaxAndColumnChecks (checks) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (elemIndex)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Occam.Core (
    OracleInconclusiveError,
    OracleOutcome (..),
 )
import Occam.Facts (
    ParserEdit (..),
    ParserEditCategory (..),
    StatementUnit (..),
 )
import Occam.Logger (nullLogger)
import Occam.Parser (
    ParsedSQL (..),
    ParserError (..),
    ParserErrorKind (..),
    SQLDialect (..),
    countStatements,
    defaultParseOptions,
    parseSQL,
 )
import Occam.Reducer (
    ColumnMode (..),
    ReductionConfig (..),
    applyParserEdits,
    defaultReductionConfig,
    findInterestingPrefix,
    reduceSQL,
    reduceSQLWithConfig,
    selectIndependentEdits,
    statementAudit,
    statementDdmin,
 )
import ReductionChecks.Support (countOrFail, jobsPool, outcomePool, predicatePool, syntaxOnly)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

checks :: [TestTree]
checks =
    [ testCase "Refreshed syntax edits agree with full reparse across statement boundaries" $ do
        let cases =
                [ (SQLite, "CREATE TABLE t(a INT NOT NULL);\r\n-- retained\r\nSELECT a, 123456 FROM t WHERE a > 0;")
                ,
                    ( PostgreSQL
                    , "SET search_path TO public; CREATE TABLE t(a INT NOT NULL); SELECT a, 123456 FROM t WHERE a > 0;"
                    )
                ,
                    ( DuckDB
                    , "CREATE MACRO identity_value(x) AS x; CREATE TABLE t(a INT NOT NULL); SELECT a, 123456 FROM t WHERE a > 0;"
                    )
                ]
            run fullReparse dialect original = do
                candidates <- newIORef []
                pool <- outcomePool $ \candidate -> do
                    modifyIORef' candidates (candidate :)
                    let accepted = all (`BSC.isInfixOf` candidate) ["CREATE TABLE t(", "SELECT a", "FROM t"]
                    pure (if accepted then OracleAccepted else OracleRejected)
                let config =
                        defaultReductionConfig
                            { reductionDialect = dialect
                            , reductionIntraStatement = True
                            , reductionLiteralBatches = True
                            , reductionAdaptiveValues = True
                            , reductionFullSyntaxReparse = fullReparse
                            }
                reduced <- reduceSQLWithConfig config nullLogger pool original
                trace <- reverse <$> readIORef candidates
                pure (reduced, trace)
        mapM_
            ( \(dialect, original) -> do
                incremental@(reduced, _) <- run False dialect original
                full <- run True dialect original
                assertBool (show dialect <> " applies an edit") (BS.length reduced < BS.length original)
                assertEqual (show dialect <> " candidate bytes and endpoint") full incremental
            )
            cases
    , testCase "Trailing SELECT clause shifts preserve the full syntax-reparse candidate order" $ do
        let original =
                "SELECT 123456 AS a, 6583 AS b WHERE 25 > 12 ORDER BY a, b LIMIT 10; SELECT 12345 AS c;"
            withoutLimit =
                "SELECT 123456 AS a, 6583 AS b WHERE 25 > 12 ORDER BY a, b; SELECT 12345 AS c;"
            withoutOrder =
                "SELECT 123456 AS a, 6583 AS b WHERE 25 > 12; SELECT 12345 AS c;"
            afterLiteralChange =
                "SELECT 123456 AS a, 6583 AS b WHERE 25 > 12; SELECT 0 AS c;"
            accepted = [original, withoutLimit, withoutOrder, afterLiteralChange]
            run fullReparse = do
                candidates <- newIORef []
                pool <- outcomePool $ \candidate -> do
                    modifyIORef' candidates (candidate :)
                    pure (if candidate `elem` accepted then OracleAccepted else OracleRejected)
                let cfg =
                        defaultReductionConfig
                            { reductionDialect = SQLite
                            , reductionIntraStatement = True
                            , reductionFullSyntaxReparse = fullReparse
                            }
                reduced <- reduceSQLWithConfig cfg nullLogger pool original
                trace <- reverse <$> readIORef candidates
                pure (reduced, trace)
        fast@(endpoint, _) <- run False
        assertEqual "both trailing clauses and the later literal are removed" afterLiteralChange endpoint
        assertEqual "same ordered candidate bytes" fast =<< run True
    , testCase "applyParserEdits and selectIndependentEdits enforce disjoint non-gluing boundaries" $ do
        let sql = "CREATE TABLE t (a VARCHAR(20) NOT NULL, b BIGINT DEFAULT 1);"
            -- a: VARCHAR(20) [18..29) -> "INT", NOT NULL [29..38) -> ""
            -- b: BIGINT [42..48) -> "INT", DEFAULT 1 [48..58) -> ""
            eTypeA = ParserEdit 18 29 "INT" EditType "simplify-data_type" False False False False
            eTypeAText = ParserEdit 18 29 "TEXT" EditType "simplify-data_type" False False False False
            eConsA = ParserEdit 29 38 "" EditConstraint "remove-column_constraint" False False False False
            eTypeB = ParserEdit 42 48 "INT" EditType "simplify-data_type" False False False False
            eConsB = ParserEdit 48 58 "" EditConstraint "remove-column_constraint" False False False False
            gluedA = ParserEdit 18 23 "AA" EditExpression "test" False False False False
            gluedB = ParserEdit 23 29 "BB" EditExpression "test" False False False False
        assertEqual
            "applyParserEdits applies adjacent replacement + deletion cleanly"
            (Just "CREATE TABLE t (a INT, b INT);")
            (applyParserEdits sql [eConsB, eTypeA, eTypeB, eConsA])
        assertEqual
            "applyParserEdits rejects overlapping edits"
            Nothing
            (applyParserEdits sql [eTypeA, eTypeAText])
        assertEqual
            "applyParserEdits rejects gluing non-empty adjacent replacements"
            Nothing
            (applyParserEdits sql [gluedA, gluedB])
        assertEqual
            "applyParserEdits rejects out-of-bounds spans"
            Nothing
            (applyParserEdits sql [ParserEdit 0 999 "" EditClause "oob" False False False False])
        let selected = selectIndependentEdits [eTypeAText, eTypeA, gluedA, gluedB, eTypeB]
        assertEqual
            "selectIndependentEdits picks highest-savings non-overlapping non-gluing set"
            [eTypeA, eTypeB]
            selected
    , testCase "Independent AST edit batching cuts calls and subdivides rejected batches across dialects" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t1 (a VARCHAR(50) NOT NULL DEFAULT 'abc', b BIGINT NOT NULL, PRIMARY KEY (a));"
                    , "CREATE TABLE t2 (c VARCHAR(50) NOT NULL, d DECIMAL(10, 2) DEFAULT 0);"
                    , "SELECT a, c FROM t1 JOIN t2 ON t1.a = t2.c WHERE 1 = 1 ORDER BY a LIMIT 5;"
                    ]
            accepted sqlCandidate =
                BSC.isInfixOf "PRIMARY KEY (a)" sqlCandidate
                    && BSC.isInfixOf "CREATE TABLE t1" sqlCandidate
                    && BSC.isInfixOf "CREATE TABLE t2" sqlCandidate
                    && BSC.isInfixOf "SELECT" sqlCandidate
                    && BSC.isInfixOf "WHERE 1 = 1" sqlCandidate
        mapM_
            ( \dialect -> do
                controlCalls <- newIORef (0 :: Int)
                controlPool <- outcomePool $ \c -> do
                    modifyIORef' controlCalls (+ 1)
                    pure (if accepted c then OracleAccepted else OracleRejected)
                let controlCfg =
                        defaultReductionConfig
                            { reductionDialect = dialect
                            , reductionSlicing = False
                            , reductionClosure = False
                            , reductionUnrestricted = False
                            , reductionIntraStatement = True
                            }
                reducedControl <- reduceSQLWithConfig controlCfg nullLogger controlPool sql
                nControl <- readIORef controlCalls

                batchCalls <- newIORef (0 :: Int)
                batchPool <- outcomePool $ \c -> do
                    modifyIORef' batchCalls (+ 1)
                    pure (if accepted c then OracleAccepted else OracleRejected)
                let batchCfg = controlCfg {reductionAstBatching = True}
                reducedBatch <- reduceSQLWithConfig batchCfg nullLogger batchPool sql
                nBatch <- readIORef batchCalls

                assertBool (show dialect <> " preserves PRIMARY KEY") (BSC.isInfixOf "PRIMARY KEY (a)" reducedBatch)
                assertBool (show dialect <> " removes NOT NULL") (not (BSC.isInfixOf "NOT NULL" reducedBatch))
                assertBool (show dialect <> " removes DEFAULT") (not (BSC.isInfixOf "DEFAULT" reducedBatch))
                assertBool (show dialect <> " removes ORDER BY") (not (BSC.isInfixOf "ORDER BY" reducedBatch))
                assertBool (show dialect <> " removes LIMIT") (not (BSC.isInfixOf "LIMIT" reducedBatch))
                assertBool
                    (show dialect <> " batching reduces oracle calls (" ++ show nBatch ++ " vs " ++ show nControl ++ ")")
                    (nBatch < nControl)
                assertBool
                    (show dialect <> " batching shrinks at least as much as control")
                    (BS.length reducedBatch <= BS.length reducedControl)
            )
            [SQLite, PostgreSQL, MySQL, DuckDB]
    , testCase "ColumnAtomic refreshes bundles cleanly after EditList and AST batching in same statement" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (a INT, b VARCHAR(40) NOT NULL);"
                    , "INSERT INTO t(a, b) VALUES (1, 'keep'), (2, 'drop');"
                    , "SELECT a FROM t ORDER BY a;"
                    ]
            accepted candidate =
                BSC.isInfixOf "CREATE TABLE t" candidate
                    && BSC.isInfixOf "INSERT INTO t" candidate
                    && BSC.isInfixOf "(1" candidate
                    && BSC.isInfixOf "SELECT a FROM t" candidate
        pool <- predicatePool accepted
        let cfg =
                defaultReductionConfig
                    { reductionDialect = SQLite
                    , reductionSlicing = False
                    , reductionClosure = False
                    , reductionUnrestricted = False
                    , reductionIntraStatement = True
                    , reductionAstBatching = True
                    , reductionColumnMode = ColumnAtomic
                    }
        reduced <- reduceSQLWithConfig cfg nullLogger pool sql
        assertBool "accepted output remains valid" (accepted reduced)
        assertBool "unused column b removed by atomic bundle" (not (BSC.isInfixOf "'keep'" reduced))
        assertBool "redundant ORDER BY removed" (not (BSC.isInfixOf "ORDER BY" reduced))
        assertBool "unused row (2, 'drop') removed" (not (BSC.isInfixOf "'drop'" reduced))
    , testCase "Intra-statement rewrites reduce UPDATE assignments, WHERE clauses, and DELETE clauses" $ do
        pool <-
            predicatePool
                ( \candidate ->
                    BSC.isInfixOf "UPDATE" candidate
                        && BSC.isInfixOf "target_col = 42" candidate
                        && BSC.isInfixOf "DELETE" candidate
                )
        let sql =
                BSC.unlines
                    [ "UPDATE t SET noise_col = 999, target_col = 42, other_noise = 888 WHERE dead_cond = 1;"
                    , "DELETE FROM t WHERE dead_delete_cond = 1;"
                    ]
            config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "keeps target assignment" (BSC.isInfixOf "target_col = 42" reduced)
        assertBool "removes noise_col assignment" (not (BSC.isInfixOf "noise_col" reduced))
        assertBool "removes other_noise assignment" (not (BSC.isInfixOf "other_noise" reduced))
        assertBool "removes dead update where clause" (not (BSC.isInfixOf "dead_cond" reduced))
        assertBool "removes dead delete where clause" (not (BSC.isInfixOf "dead_delete_cond" reduced))
        assertBool "keeps DELETE statement" (BSC.isInfixOf "DELETE FROM t;" reduced)
    , testCase "Intra-statement rewrites reduce compound queries across set operations" $ do
        pool <- predicatePool (BSC.isInfixOf "42")
        let sql =
                "SELECT 999 UNION ALL (SELECT 888) UNION ALL SELECT 42;"
            config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "keeps target value" (BSC.isInfixOf "42" reduced)
        assertBool "removes noise branch 999" (not (BSC.isInfixOf "999" reduced))
        assertBool "removes noise branch 888" (not (BSC.isInfixOf "888" reduced))
        assertEqual "reduces to clean single query" "SELECT 42;" reduced
    , testCase "Intra-statement rewrites reduce DISTINCT, GROUP BY, ORDER BY, and window specifications" $ do
        pool <- predicatePool (BSC.isInfixOf "42")
        let sql =
                "SELECT DISTINCT sum(42) OVER (PARTITION BY noise_part ORDER BY noise_ord ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) FROM t GROUP BY a, noise_grp ORDER BY x, noise_order;"
            config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "keeps target value" (BSC.isInfixOf "42" reduced)
        assertBool "removes distinct modifier" (not (BSC.isInfixOf "DISTINCT" reduced))
        assertBool "removes noise partition" (not (BSC.isInfixOf "noise_part" reduced))
        assertBool "removes noise window order" (not (BSC.isInfixOf "noise_ord" reduced))
        assertBool "removes noise frame" (not (BSC.isInfixOf "ROWS BETWEEN" reduced))
        assertBool "removes noise group by item" (not (BSC.isInfixOf "noise_grp" reduced))
        assertBool "removes noise query order by item" (not (BSC.isInfixOf "noise_order" reduced))
    , testCase "Intra-statement rewrites unwrap CAST expressions" $ do
        pool <- predicatePool (BSC.isInfixOf "42")
        let sql = "SELECT CAST(42 AS VARCHAR(255));"
            config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertEqual "unwraps cast to inner expression" "SELECT 42;" reduced
    , testCase "Intra-statement rewrites unwrap CASE expressions to matching branch" $ do
        pool <- predicatePool (BSC.isInfixOf "42")
        let sql = "SELECT CASE WHEN cond THEN 42 ELSE 99 END;"
            config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertEqual "unwraps case expression to inner branch" "SELECT 42;" reduced
    , testCase "Interleaving statement audit and intra-statement rewrites reaches a global fixed point" $ do
        pool <-
            predicatePool (\candidate -> BSC.isInfixOf "main_tbl" candidate && BSC.isInfixOf "42" candidate)
        let sql =
                BSC.unlines
                    [ "CREATE TABLE helper (hid INT);"
                    , "INSERT INTO helper VALUES (999);"
                    , "CREATE TABLE main_tbl (id INT);"
                    , "INSERT INTO main_tbl VALUES (42);"
                    , "SELECT id FROM main_tbl WHERE id = 42 AND (SELECT COUNT(*) FROM helper) > 0;"
                    ]
            config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "keeps main table" (BSC.isInfixOf "main_tbl" reduced)
        assertBool "keeps target value" (BSC.isInfixOf "42" reduced)
        assertBool "eliminates helper table via interleaved audit" (not (BSC.isInfixOf "helper" reduced))
        assertBool "eliminates helper insert via interleaved audit" (not (BSC.isInfixOf "999" reduced))
    , testCase "Statement audit reaches a fixed point, permits empty output, and preserves bytes" $ do
        let sql = BSC.unlines ["SELECT 101;", "SELECT 202;", "SELECT 303;"]
            accepted = BSC.pack "SELECT 101;\nSELECT 303;\n"
            final = BSC.pack "SELECT 303;\n"
        pool <-
            predicatePool (\candidate -> candidate == sql || candidate == accepted || candidate == final)
        fixedPoint <- statementAudit GenericSQL pool sql
        assertEqual "a statement becomes removable only after an earlier deletion" final fixedPoint
        emptyPool <- predicatePool (const True)
        empty <- statementAudit GenericSQL emptyPool "SELECT 7;"
        assertEqual "the empty candidate is not prohibited" BS.empty empty
        keepPool <- predicatePool (BSC.isInfixOf "SELECT 11")
        let bytes = BSC.pack "SELECT 11;\r\nSELECT 22;\n"
            firstStatement = BSC.pack "SELECT 11;\r\n"
        byteResult <- statementDdmin GenericSQL keepPool bytes
        assertEqual "retained source bytes, including CRLF, are unchanged" firstStatement byteResult
    , testCase "statement audit distinguishes identical statements with different trailing comments" $ do
        let kept = "SELECT 1; -- A\n"
            input = kept <> "SELECT 1; -- B\n"
        forM_ [1, 4] $ \jobs -> do
            pool <- jobsPool jobs (BS.isInfixOf "-- A")
            reduced <- statementAudit SQLite pool input
            assertEqual "audit finds the interesting deletion" kept reduced
    , testCase "statement deletion removes exactly its own terminator and separator" $ do
        pool <-
            predicatePool
                (\candidate -> BSC.isInfixOf "SELECT 2" candidate && BSC.isInfixOf "SELECT 3" candidate)
        let sql = BSC.pack "SELECT 1;\r\n\r\nSELECT 2;\r\nSELECT 3;\r\n"
        reduced <- statementDdmin GenericSQL pool sql
        assertEqual
            "the removed statement takes its CRLF and blank separator with it"
            (BSC.pack "SELECT 2;\r\nSELECT 3;\r\n")
            reduced
    , testCase "CTE removal preserves a required sibling and untouched source bytes" $ do
        let prefix = TE.encodeUtf8 "-- café\n"
            kept = "kept AS (SELECT 42 AS value)"
            query = "SELECT value FROM kept;\r\n"
            sql = prefix <> "WITH unused AS (SELECT 1), " <> kept <> " " <> query
        pool <- predicatePool (\candidate -> BS.isInfixOf kept candidate && BS.isInfixOf query candidate)
        reduced <- syntaxOnly SQLite pool sql
        assertBool "the unnecessary CTE is removed" (not (BSC.isInfixOf "unused" reduced))
        assertBool "the required CTE is unchanged" (BS.isInfixOf kept reduced)
        assertBool "the Unicode prefix is untouched" (BS.isPrefixOf prefix reduced)
        assertBool "the query and CRLF ending are untouched" (BS.isSuffixOf query reduced)
        secondPool <- predicatePool (BSC.isInfixOf "SELECT 42;")
        withoutCTEs <-
            syntaxOnly
                SQLite
                secondPool
                "WITH unused AS (SELECT 1) SELECT 42;"
        assertBool
            "the final unnecessary CTE and WITH keyword can disappear"
            (not (BSC.isInfixOf "WITH" withoutCTEs))
    , testCase "VALUES row reduction excludes the INSERT target column list" $ do
        pool <-
            predicatePool
                (\candidate -> BSC.isInfixOf "t(a,b)" candidate && BSC.isInfixOf "(42,43)" candidate)
        let sql = "INSERT INTO t(a,b) VALUES (1,2),(42,43);"
        reduced <- syntaxOnly SQLite pool sql
        assertEqual
            "only the irrelevant VALUES row and its comma are removed"
            "INSERT INTO t(a,b) VALUES (42,43);"
            reduced
    , testCase "VALUES chunk deletion removes contiguous rows with exact separators" $ do
        pool <- predicatePool (BSC.isInfixOf "(42)")
        let prefix = TE.encodeUtf8 "-- café\n"
            sql = prefix <> "INSERT INTO t VALUES (1),(2),(3),(42);\r\n"
            expected = prefix <> "INSERT INTO t VALUES (42);\r\n"
        reduced <- syntaxOnly SQLite pool sql
        assertEqual
            "contiguous chunk deletions and singles leave exact separators and untouched bytes"
            expected
            reduced
    , testCase "unused projection terms are removed inside a subquery" $ do
        pool <-
            predicatePool
                (\candidate -> BSC.isInfixOf "SELECT a FROM" candidate && BSC.isInfixOf "42 AS a" candidate)
        let sql = "SELECT a FROM (SELECT 42 AS a,99 AS b);\r\n"
        reduced <- syntaxOnly SQLite pool sql
        assertEqual
            "the unused projection and comma disappear without reformatting"
            "SELECT a FROM (SELECT 42 AS a);\r\n"
            reduced
    , testCase "OFFSET can disappear while a required LIMIT remains" $ do
        pool <-
            predicatePool
                (\candidate -> BSC.isInfixOf "SELECT a FROM t" candidate && BSC.isInfixOf "LIMIT 1" candidate)
        let sql = "SELECT a FROM t LIMIT 1 OFFSET 0;\r\n"
        reduced <- syntaxOnly SQLite pool sql
        assertEqual
            "LIMIT and the original ending survive OFFSET removal"
            "SELECT a FROM t LIMIT 1;\r\n"
            reduced
    , testCase "literal scalar subqueries collapse using their exact source span" $ do
        pool <- predicatePool (BSC.isInfixOf "42")
        let prefix = TE.encodeUtf8 "-- café\n"
            sql = prefix <> "SELECT (SELECT 42);\r\n"
        reduced <- syntaxOnly SQLite pool sql
        assertEqual "outer Unicode and CRLF bytes remain untouched" (prefix <> "SELECT 42;\r\n") reduced
        tableQuery <- syntaxOnly SQLite pool "SELECT * FROM (SELECT 42);"
        assertBool
            "table subqueries are not replaced by scalar literals"
            (not (BSC.isInfixOf "FROM 42" tableQuery))
    , testCase "SQL search retries an FK-blocked parent row after child reduction" $ do
        let prefix = TE.encodeUtf8 "-- café\n"
            script parentValues childValues =
                prefix
                    <> BSC.pack
                        ( "PRAGMA foreign_keys = ON;\r\n"
                            ++ "CREATE TABLE parent (id INTEGER PRIMARY KEY);\r\n"
                            ++ "CREATE TABLE child (pid INTEGER REFERENCES parent(id));\r\n"
                            ++ "INSERT INTO parent VALUES "
                            ++ parentValues
                            ++ ";\r\n"
                            ++ "INSERT INTO child VALUES "
                            ++ childValues
                            ++ ";\r\n"
                            ++ "SELECT p.id FROM parent p JOIN child c ON c.pid = p.id WHERE p.id = 1;\r\n"
                        )
            initial = script "(1), (2)" "(1), (2)"
            blockedParent = script "(1)" "(1), (2)"
            afterChild = script "(1), (2)" "(1)"
            expected = script "(1)" "(1)"
            -- A deterministic external predicate accepts these three complete valid scripts.
            accepted = Set.fromList [initial, afterChild, expected]
        observations <- newIORef ([] :: [(ByteString, Bool)])
        pool <- outcomePool $ \candidate -> do
            let wasAccepted = Set.member candidate accepted
            modifyIORef' observations ((candidate, wasAccepted) :)
            pure (if wasAccepted then OracleAccepted else OracleRejected)
        reduced <- syntaxOnly SQLite pool initial
        seen <- reverse <$> readIORef observations
        let observedSQL = map fst seen
        assertEqual "the later parent deletion is accepted after its child" expected reduced
        assertEqual "the parent deletion is initially rejected" (Just False) (lookup blockedParent seen)
        assertEqual "the child deletion is accepted" (Just True) (lookup afterChild seen)
        assertEqual "the enabled parent deletion is accepted" (Just True) (lookup expected seen)
        let positions =
                ( elemIndex blockedParent observedSQL
                , elemIndex afterChild observedSQL
                , elemIndex expected observedSQL
                )
        case positions of
            (Just blockedAt, Just childAt, Just parentAt) -> do
                assertBool "the blocked parent candidate comes before child removal" (blockedAt < childAt)
                assertBool "child removal precedes the newly enabled parent removal" (childAt < parentAt)
            _ -> assertFailure "the oracle did not observe the three complete-script states"
    , testCase "Statement surgery preserves compound SQLite triggers and UTF-8 offsets" $ do
        let prefix = TE.encodeUtf8 "-- café\n"
            unused = BSC.pack "CREATE TABLE unused (id INT);\n"
            trigger =
                TE.encodeUtf8
                    "CREATE TRIGGER tr AFTER INSERT ON t BEGIN UPDATE t SET x = CASE WHEN x = 'a;b' THEN 'c;d' ELSE x END; INSERT INTO audit VALUES ('x;y'); END;"
            target = TE.encodeUtf8 "SELECT 'semi;colon';\r\n"
            sql = prefix <> unused <> trigger <> "\r\n" <> target <> BSC.pack "SELECT 9;\n"
            expected = prefix <> trigger <> "\r\n" <> target
        pool <-
            predicatePool (\candidate -> BS.isInfixOf trigger candidate && BS.isInfixOf target candidate)
        statementCount <- countOrFail SQLite sql
        assertEqual "table, trigger, target, and following SELECT are four source units" 4 statementCount
        reduced <- statementDdmin SQLite pool sql
        assertEqual "the trigger is one source statement and retained bytes are untouched" expected reduced
    , testCase "Statement counting follows each selected SQL dialect" $ do
        sqliteCount <- countOrFail SQLite "SELECT /*; block */ 'a;b'; -- line; comment\nSELECT [g;h];"
        postgresCount <-
            countOrFail PostgreSQL "SELECT $tag$semi;colon$tag$; -- line; comment\nSELECT E'a\\';semi';"
        mysqlCount <- countOrFail MySQL "SELECT `g;h`; -- line; comment\nSELECT 'a;b';"
        assertEqual "SQLite quoted/comment semicolons stay within units" 2 sqliteCount
        assertEqual "PostgreSQL dollar and escaped strings stay within units" 2 postgresCount
        assertEqual "MySQL backticks and strings stay within units" 2 mysqlCount
        duckdbCount <- countOrFail DuckDB "SELECT [1, 2, 3]; -- comment\nSELECT * FROM range(5);"
        clickhouseCount <- countOrFail ClickHouse "SELECT number FROM numbers(10); -- comment\nSELECT 1;"
        oracleCount <- countOrFail Oracle "SELECT 1 FROM dual; -- comment\nSELECT 2 FROM dual;"
        ansiCount <- countOrFail Ansi "SELECT 1; -- comment\nSELECT 2;"
        mssqlCount <- countOrFail MsSql "SELECT [a;b]; -- comment\nSELECT TOP 1 * FROM [t];"
        snowflakeCount <- countOrFail Snowflake "SELECT \"a;b\"; -- comment\nSELECT 2;"
        bigqueryCount <- countOrFail BigQuery "SELECT `a;b`; -- comment\nSELECT 2;"
        assertEqual "DuckDB statements stay within units" 2 duckdbCount
        assertEqual "ClickHouse statements stay within units" 2 clickhouseCount
        assertEqual "Oracle statements stay within units" 2 oracleCount
        assertEqual "Ansi statements stay within units" 2 ansiCount
        assertEqual "MsSql statements stay within units" 2 mssqlCount
        assertEqual "Snowflake statements stay within units" 2 snowflakeCount
        assertEqual "BigQuery statements stay within units" 2 bigqueryCount
    , testCase "inconclusive oracle outcomes abort reduction" $ do
        pool <- outcomePool (\_ -> pure OracleInconclusive)
        result <-
            try (reduceSQL GenericSQL pool "SELECT 1;") ::
                IO (Either OracleInconclusiveError ByteString)
        case result of
            Left _ -> pure ()
            Right _ -> assertFailure "inconclusive oracle outcome was treated as a definite decision"
    , testCase "invalid SQL still counts its raw statement units; transport errors stay explicit" $ do
        invalidCount <- countOrFail SQLite "SELECT FROM ; SELECT 42;"
        assertEqual "invalid syntax does not turn statement count into zero" 2 invalidCount
        invalidUtf8 <- countStatements SQLite (BS.pack [255])
        case invalidUtf8 of
            Left err -> assertEqual "invalid UTF-8 has a typed parser error" ParserInvalidUtf8 (parserErrorKind err)
            Right count -> assertFailure ("invalid UTF-8 unexpectedly counted as " ++ show count)
    , testCase "reducer propagates parser transport failures" $ do
        pool <- predicatePool (== BS.pack [255])
        let invalidUtf8 = BS.pack [255]
        result <-
            try (reduceSQL SQLite pool invalidUtf8) ::
                IO (Either IOException ByteString)
        case result of
            Left _ -> pure ()
            Right _ -> assertFailure "invalid UTF-8 returned as a successful reduction"
    , testCase "findInterestingPrefix binary search handles empty, 1-stmt, tail, middle, and no-match" $ do
        let mkSQL n = BSC.unlines [BSC.pack ("SELECT " ++ show i ++ ";") | i <- [1 .. n]]
        -- Case 1: Empty list
        emptyPool <- predicatePool (const True)
        (resEmpty, testsEmpty) <- findInterestingPrefix emptyPool BS.empty []
        assertEqual "empty statements returns Nothing" Nothing resEmpty
        assertEqual "empty statements does 0 tests" 0 testsEmpty

        -- Case 2: 1-statement matching
        onePool <- predicatePool (const True)
        parsed1 <- parseSQL (defaultParseOptions SQLite) "SELECT 1;"
        case parsed1 of
            Left err -> assertFailure (show err)
            Right p1 -> do
                let stmts1 = map unitStatement (parsedUnits p1)
                (res1, tests1) <- findInterestingPrefix onePool "SELECT 1;" stmts1
                assertBool "1-stmt matches" (isJust res1)
                assertEqual "1-stmt does 1 test" 1 tests1

        -- Case 3: 21 statements with tail crash (prefix >= 21)
        tailPool <- predicatePool (BSC.isInfixOf "SELECT 21;")
        let sql21 = mkSQL (21 :: Int)
        parsed21 <- parseSQL (defaultParseOptions SQLite) sql21
        case parsed21 of
            Left err -> assertFailure (show err)
            Right p21 -> do
                let stmts21 = map unitStatement (parsedUnits p21)
                (resTail, testsTail) <- findInterestingPrefix tailPool sql21 stmts21
                case resTail of
                    Nothing -> assertFailure "failed to find tail crash prefix"
                    Just (_, _, prefixLen) -> do
                        assertEqual "tail crash is at prefix 21" 21 prefixLen
                        assertBool ("binary search takes <= 5 tests (took " ++ show testsTail ++ ")") (testsTail <= 5)

        -- Case 4: 21 statements with middle crash (prefix >= 7)
        midPool <- predicatePool (BSC.isInfixOf "SELECT 7;")
        case parsed21 of
            Left err -> assertFailure (show err)
            Right p21 -> do
                let stmts21 = map unitStatement (parsedUnits p21)
                (resMid, testsMid) <- findInterestingPrefix midPool sql21 stmts21
                case resMid of
                    Nothing -> assertFailure "failed to find middle crash prefix"
                    Just (_, _, prefixLen) -> do
                        assertEqual "middle crash is at prefix 7" 7 prefixLen
                        assertBool ("binary search takes <= 5 tests (took " ++ show testsMid ++ ")") (testsMid <= 5)

        -- Case 5: 21 statements with no match
        noPool <- predicatePool (const False)
        case parsed21 of
            Left err -> assertFailure (show err)
            Right p21 -> do
                let stmts21 = map unitStatement (parsedUnits p21)
                (resNo, testsNo) <- findInterestingPrefix noPool sql21 stmts21
                assertEqual "no match returns Nothing" Nothing resNo
                assertBool ("binary search takes <= 5 tests (took " ++ show testsNo ++ ")") (testsNo <= 5)
    , testCase "syntax search reaches a late clause after earlier same-category rejections" $ do
        let statements =
                [ let value = BSC.pack (show index)
                   in "SELECT " <> value <> " WHERE " <> value <> " = " <> value <> ";"
                | index <- [1 :: Int .. 42]
                ]
            earlier = init statements
            sql = BSC.unlines statements
            expected = BSC.unlines (earlier ++ ["SELECT 42;"])
            accepts candidate =
                candidate == sql
                    || ( all (`BSC.isInfixOf` candidate) earlier
                            && BSC.isInfixOf "SELECT 42;" candidate
                       )
        rejected <- newIORef (0 :: Int)
        pool <- outcomePool $ \candidate ->
            if accepts candidate
                then pure OracleAccepted
                else do
                    modifyIORef' rejected (+ 1)
                    pure OracleRejected
        reduced <- syntaxOnly SQLite pool sql
        rejectionCount <- readIORef rejected
        assertEqual "the final clause is removed after earlier clauses remain" expected reduced
        assertBool
            "the earlier same-category candidates were rejected"
            (rejectionCount >= length earlier)
    , testCase "safe clauses (ORDER BY / LIMIT) are prioritized before WHERE removal" $ do
        let sql = "SELECT * FROM t WHERE a = 1 ORDER BY id LIMIT 5;\r\n"
        probes <- newIORef ([] :: [ByteString])
        pool <- outcomePool $ \candidate -> do
            modifyIORef' probes (candidate :)
            if BSC.isInfixOf "WHERE a = 1" candidate
                then pure OracleAccepted
                else pure OracleRejected
        reduced <- syntaxOnly SQLite pool sql
        assertEqual
            "ORDER BY and LIMIT are removed while WHERE is preserved"
            "SELECT * FROM t WHERE a = 1;\r\n"
            reduced
        seen <- reverse <$> readIORef probes
        let withoutWhere = "SELECT * FROM t ORDER BY id LIMIT 5;\r\n"
            withoutLimit = "SELECT * FROM t WHERE a = 1 ORDER BY id;\r\n"
            withoutOrder = "SELECT * FROM t WHERE a = 1 LIMIT 5;\r\n"
        let whereIdx = elemIndex withoutWhere seen
            limitIdx = elemIndex withoutLimit seen
            orderIdx = elemIndex withoutOrder seen
        assertBool "safe clause candidate is tested before WHERE removal" $
            case (whereIdx, limitIdx, orderIdx) of
                (Just w, Just l, _) -> l < w
                (Just w, _, Just o) -> o < w
                (Nothing, Just _, _) -> True
                (Nothing, _, Just _) -> True
                _ -> False
    , testCase "expression unwrapping is prioritized before destructive constant replacements" $ do
        let sql = "SELECT * FROM t WHERE (a = 1);\r\n"
        probes <- newIORef ([] :: [ByteString])
        pool <- outcomePool $ \candidate -> do
            modifyIORef' probes (candidate :)
            if BSC.isInfixOf "a = 1" candidate
                then pure OracleAccepted
                else pure OracleRejected
        reduced <- syntaxOnly SQLite pool sql
        assertEqual
            "parentheses are unwrapped"
            "SELECT * FROM t WHERE a = 1;\r\n"
            reduced
        seen <- reverse <$> readIORef probes
        let unwrapCandidate = "SELECT * FROM t WHERE a = 1;\r\n"
        assertBool "unwrap candidate was tested" (unwrapCandidate `elem` seen)
    , testCase "statement ddmin re-tests a removal enabled by a later shrink (jobs 1 and 4)" $ do
        -- Candidate memo is keyed by exact bytes only to allow removals enabled by later shrinks.
        let sql = BSC.unlines ["SELECT 0;", "SELECT 1;", "SELECT 0;", "SELECT 2;", "SELECT 42;"]
            expected = BSC.pack "SELECT 1;\nSELECT 42;\n"
            interesting candidate =
                BSC.isInfixOf "SELECT 1;" candidate
                    && BSC.isInfixOf "SELECT 42;" candidate
                    && ( not (BSC.isInfixOf "SELECT 2;" candidate)
                            || length (filter (== "SELECT 0;") (BSC.lines candidate)) >= 2
                       )
        mapM_
            ( \jobs -> do
                pool <- jobsPool jobs interesting
                reduced <- statementDdmin GenericSQL pool sql
                assertEqual ("ddmin reaches C,M with jobs=" ++ show jobs) expected reduced
            )
            [1, 4]
    , testCase "jobs 1 and 4 agree on audit, ddmin, and syntax fixed points" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t1 (a INT, b TEXT);"
                    , "CREATE TABLE t2 (c INT);"
                    , "INSERT INTO t1 VALUES (1, 'x');"
                    , "CREATE INDEX i1 ON t1 (a);"
                    , "SELECT * FROM t1 WHERE a > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                    ]
            interesting candidate =
                BSC.isInfixOf "CREATE TABLE t1" candidate
                    && BSC.isInfixOf "INSERT INTO t1" candidate
                    && BSC.isInfixOf "WHERE a > 0" candidate
        mapM_
            ( \(name, reduce) -> do
                serialPool <- jobsPool 1 interesting
                serial <- reduce serialPool sql
                widePool <- jobsPool 4 interesting
                wide <- reduce widePool sql
                assertEqual (name ++ " agrees across job counts") serial wide
            )
            [ ("statement audit", statementAudit GenericSQL)
            , ("statement ddmin", statementDdmin GenericSQL)
            , ("syntax only", syntaxOnly GenericSQL)
            ]
    , testCase "dead earlier update on a column is pruned by dependency slice" $ do
        pool <-
            predicatePool
                ( \candidate ->
                    BSC.isInfixOf "SELECT a FROM t;" candidate
                        && BSC.isInfixOf "a = 2" candidate
                )
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (id INT, a INT);"
                    , "INSERT INTO t VALUES (1, 10);"
                    , "UPDATE t SET a = 1;"
                    , "UPDATE t SET a = 2;"
                    , "SELECT a FROM t;"
                    ]
        let config =
                defaultReductionConfig
                    { reductionUnrestricted = False
                    , reductionIntraStatement = False
                    }
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "dead earlier update (a = 1) must be eliminated" (not (BSC.isInfixOf "a = 1" reduced))
        assertBool "effective update (a = 2) must be preserved" (BSC.isInfixOf "a = 2" reduced)
        assertBool "insert must be preserved for row existence" (BSC.isInfixOf "INSERT INTO t" reduced)
    , testCase "dead earlier update on a column is pruned during full reduction" $ do
        pool <-
            predicatePool
                ( \candidate ->
                    BSC.isInfixOf "INSERT INTO t" candidate
                        && BSC.isInfixOf "SELECT a FROM t;" candidate
                        && BSC.isInfixOf "a = 2" candidate
                )
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (id INT, a INT);"
                    , "INSERT INTO t VALUES (1, 10);"
                    , "UPDATE t SET a = 1;"
                    , "UPDATE t SET a = 2;"
                    , "SELECT a FROM t;"
                    ]
        reduced <- reduceSQL GenericSQL pool sql
        assertBool "dead earlier update (a = 1) must be eliminated" (not (BSC.isInfixOf "a = 1" reduced))
        assertBool "effective update (a = 2) must be preserved" (BSC.isInfixOf "a = 2" reduced)
        assertBool "insert must be preserved" (BSC.isInfixOf "INSERT INTO t" reduced)
    , testCase "updates to distinct columns read by query are both preserved during reduction" $ do
        pool <-
            predicatePool
                ( \candidate ->
                    BSC.isInfixOf "SELECT a, b FROM t;" candidate
                        && BSC.isInfixOf "a = 1" candidate
                        && BSC.isInfixOf "b = 2" candidate
                )
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (id INT, a INT, b INT);"
                    , "INSERT INTO t VALUES (1, 10, 20);"
                    , "UPDATE t SET a = 1;"
                    , "UPDATE t SET b = 2;"
                    , "SELECT a, b FROM t;"
                    ]
        reduced <- reduceSQL GenericSQL pool sql
        assertBool
            "both updates must be preserved"
            (BSC.isInfixOf "a = 1" reduced && BSC.isInfixOf "b = 2" reduced)
    , testCase "UPDATE on table is pruned during reduction when query only reads COUNT(*)" $ do
        pool <-
            predicatePool
                ( \candidate ->
                    BSC.isInfixOf "INSERT INTO t" candidate
                        && BSC.isInfixOf "SELECT COUNT(*) FROM t;" candidate
                )
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (id INT, a INT);"
                    , "INSERT INTO t VALUES (1, 10);"
                    , "UPDATE t SET a = 1;"
                    , "UPDATE t SET a = 2;"
                    , "SELECT COUNT(*) FROM t;"
                    ]
        reduced <- reduceSQL GenericSQL pool sql
        assertBool "all updates must be eliminated" (not (BSC.isInfixOf "UPDATE t" reduced))
        assertBool "insert must be preserved" (BSC.isInfixOf "INSERT INTO t" reduced)
        assertBool "count query must be preserved" (BSC.isInfixOf "SELECT COUNT(*) FROM t;" reduced)
    , testCase "update with LIMIT does not eliminate earlier update in dependency slice" $ do
        pool <-
            predicatePool
                ( \candidate ->
                    BSC.isInfixOf "SELECT a FROM t;" candidate
                        && BSC.isInfixOf "a = 2" candidate
                )
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (id INT, a INT);"
                    , "INSERT INTO t VALUES (1, 10);"
                    , "UPDATE t SET a = 1;"
                    , "UPDATE t SET a = 2 LIMIT 1;"
                    , "SELECT a FROM t;"
                    ]
        let config =
                defaultReductionConfig
                    { reductionDialect = MySQL
                    , reductionUnrestricted = False
                    , reductionIntraStatement = False
                    }
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "earlier update (a = 1) must be preserved" (BSC.isInfixOf "a = 1" reduced)
        assertBool "effective update (a = 2) must be preserved" (BSC.isInfixOf "a = 2" reduced)
        assertBool "insert must be preserved" (BSC.isInfixOf "INSERT INTO t" reduced)
    , testCase
        "atomic column reduction succeeds on clean table despite transaction block on another table"
        $ do
            pool <-
                predicatePool
                    ( \candidate ->
                        BSC.isInfixOf "CREATE TABLE t" candidate
                            && BSC.isInfixOf "a INT" candidate
                            && BSC.isInfixOf "SELECT a FROM t" candidate
                            && BSC.isInfixOf "10" candidate
                            && BSC.isInfixOf "other" candidate
                    )
            let sql =
                    BSC.unlines
                        [ "BEGIN;"
                        , "CREATE TABLE other (x INT);"
                        , "INSERT INTO other VALUES (1);"
                        , "COMMIT;"
                        , "CREATE TABLE t (a INT, b INT);"
                        , "INSERT INTO t (a, b) VALUES (1, 10);"
                        , "SELECT a FROM t;"
                        ]
            let config =
                    defaultReductionConfig
                        { reductionDialect = SQLite
                        , reductionSlicing = False
                        , reductionClosure = False
                        , reductionIntraStatement = True
                        , reductionColumnMode = ColumnAtomic
                        }
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertBool "keeps required table t with column a" (BSC.isInfixOf "a INT" reduced)
            assertBool "unneeded column b is dropped" (not (BSC.isInfixOf "b INT" reduced))
            assertBool "keeps other table in transaction" (BSC.isInfixOf "other" reduced)
    , testCase "atomic column reduction succeeds on clean table despite scoped barrier on another table" $ do
        pool <-
            predicatePool
                ( \candidate ->
                    BSC.isInfixOf "CREATE TABLE t" candidate
                        && BSC.isInfixOf "a INT" candidate
                        && BSC.isInfixOf "SELECT a FROM t" candidate
                        && BSC.isInfixOf "10" candidate
                        && BSC.isInfixOf "CREATE VIRTUAL TABLE vt" candidate
                )
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (a INT, b INT);"
                    , "INSERT INTO t (a, b) VALUES (1, 10);"
                    , "SELECT a FROM t;"
                    , "CREATE VIRTUAL TABLE vt USING fts5(content);"
                    ]
        let config =
                defaultReductionConfig
                    { reductionDialect = SQLite
                    , reductionSlicing = False
                    , reductionClosure = False
                    , reductionIntraStatement = True
                    , reductionColumnMode = ColumnAtomic
                    }
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "keeps required table t with column a" (BSC.isInfixOf "a INT" reduced)
        assertBool "unneeded column b is dropped" (not (BSC.isInfixOf "b INT" reduced))
        assertBool "keeps scoped barrier" (BSC.isInfixOf "CREATE VIRTUAL TABLE vt" reduced)
    ]

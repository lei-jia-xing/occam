-- | Real and regression corpus alignment across database parsers.
module SyntaxAlignmentChecks.Corpus (realFuzzCorpusAlignment, regressionCorpusAlignment) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite3 qualified as Sqlite
import ExtraEngineOracles (
    ExtraOracles (..),
    MysqlServer,
    clickhouseSyntaxCheck,
    clickhouseSyntaxCheckSingle,
    mysqlSyntaxCheckIsolated,
 )
import FrontendCorpusChecks (CorpusCase (..), loadRealFuzzCorpus, loadRegressionCases)
import Occam.Frontend.Parser (ParsedScript (..), parseScript)
import Occam.Frontend.Span (sliceSpan)
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Parser (SQLDialect (..))
import Occam.Parser.Dialect (
    DialectProfile,
    clickhouseProfile,
    duckdbProfile,
    mariadbProfile,
    mysqlProfile,
    postgresqlProfile,
    sqliteProfile,
 )
import PgQueryOracle (PgSyntaxError (..), pgQuerySplitCheck)
import SyntaxAlignmentChecks.Engine (parseCorpusStmts, sqlitePrepareSyntaxCheck, withDuckDBConn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- ---------------------------------------------------------------------------
-- Real fuzz corpus alignment (benchmarks/real_fuzz_corpus/*.sql)
-- ---------------------------------------------------------------------------
realFuzzCorpusAlignment :: Bool -> ExtraOracles -> TestTree
realFuzzCorpusAlignment pgAvail extraOracles =
    testGroup
        "real_fuzz_corpus alignment"
        ( [ testCase "PostgreSQL (12 cases) match libpg_query acceptance and statement count" $ do
                cases <- either assertFailure pure =<< loadRealFuzzCorpus "benchmarks/real_fuzz_corpus"
                let pgCases = filter ((== PostgreSQL) . corpusDialect) cases
                assertEqual "PostgreSQL real_fuzz_corpus count" 12 (length pgCases)
                forM_ pgCases \item -> do
                    occamCount <- parseCorpusStmts postgresqlProfile item
                    pgRes <- pgQuerySplitCheck (corpusSql item)
                    case pgRes of
                        Left err ->
                            assertFailure
                                ( T.unpack (corpusId item)
                                    <> ": libpg_query rejected PG corpus case at pos "
                                    <> show (pgErrCursorPos err)
                                    <> ": "
                                    <> T.unpack (pgErrMessage err)
                                )
                        Right pgCount ->
                            assertEqual
                                (T.unpack (corpusId item) <> ": PG statement count")
                                occamCount
                                pgCount
          | pgAvail
          ]
            ++ [ testCase "DuckDB (10 cases) accepted by both Occam and duckdb_extract_statements" $ do
                    cases <- either assertFailure pure =<< loadRealFuzzCorpus "benchmarks/real_fuzz_corpus"
                    let duckCases = filter ((== DuckDB) . corpusDialect) cases
                    assertEqual "DuckDB real_fuzz_corpus count" 10 (length duckCases)
                    withDuckDBConn \extract ->
                        forM_ duckCases \item -> do
                            occamCount <- parseCorpusStmts duckdbProfile item
                            duckRes <- extract (corpusSql item)
                            case duckRes of
                                Left err ->
                                    assertFailure
                                        (T.unpack (corpusId item) <> ": duckdb_extract_statements rejected DuckDB corpus case: " <> err)
                                Right duckCount ->
                                    -- Dynamic PIVOT without IN (...) expands in DuckDB's extractor;
                                    -- otherwise counts must match directly.
                                    if "PIVOT" `BS.isInfixOf` corpusSql item || "pivot" `BS.isInfixOf` corpusSql item
                                        then
                                            assertBool
                                                (T.unpack (corpusId item) <> ": DuckDB count must be >= Occam count")
                                                (duckCount >= occamCount)
                                        else
                                            assertEqual
                                                (T.unpack (corpusId item) <> ": DuckDB statement count")
                                                occamCount
                                                duckCount
               , testCase "SQLite (12 cases) parse with zero SQLite syntax errors" $ do
                    cases <- either assertFailure pure =<< loadRealFuzzCorpus "benchmarks/real_fuzz_corpus"
                    let sqliteCases = filter ((== SQLite) . corpusDialect) cases
                    assertEqual "SQLite real_fuzz_corpus count" 12 (length sqliteCases)
                    forM_ sqliteCases \item -> do
                        _ <- parseCorpusStmts sqliteProfile item
                        bracket (Sqlite.open ":memory:") Sqlite.close \db ->
                            case splitSQL sqliteProfile (corpusSql item) of
                                Left lexErr ->
                                    assertFailure (T.unpack (corpusId item) <> ": splitSQL failed: " <> show lexErr)
                                Right units ->
                                    forM_ (zip [1 :: Int ..] units) \(idx, unit) ->
                                        case sliceSpan (splitUnitSpan unit) (corpusSql item) of
                                            Nothing ->
                                                assertFailure (T.unpack (corpusId item) <> ": invalid split span")
                                            Just unitSql -> do
                                                res <- sqlitePrepareSyntaxCheck db unitSql
                                                case res of
                                                    Left synErr ->
                                                        assertFailure
                                                            ( T.unpack (corpusId item)
                                                                <> " stmt #"
                                                                <> show idx
                                                                <> ": SQLite syntax error: "
                                                                <> T.unpack synErr
                                                                <> "\nSQL: "
                                                                <> BSC.unpack unitSql
                                                            )
                                                    Right () -> pure ()
               ]
            ++ [ testCase "ClickHouse (10 cases) accepted by both Occam and clickhouse format" $ do
                    cases <- either assertFailure pure =<< loadRealFuzzCorpus "benchmarks/real_fuzz_corpus"
                    let chCases = filter ((== ClickHouse) . corpusDialect) cases
                    assertEqual "ClickHouse real_fuzz_corpus count" 10 (length chCases)
                    forM_ chCases \item -> do
                        _ <- parseCorpusStmts clickhouseProfile item
                        case splitSQL clickhouseProfile (corpusSql item) of
                            Left lexErr ->
                                assertFailure (T.unpack (corpusId item) <> ": splitSQL failed: " <> show lexErr)
                            Right units ->
                                forM_ (zip [1 :: Int ..] units) \(idx, unit) ->
                                    case sliceSpan (splitUnitSpan unit) (corpusSql item) of
                                        Nothing ->
                                            assertFailure (T.unpack (corpusId item) <> ": invalid split span")
                                        Just unitSql -> do
                                            res <- clickhouseSyntaxCheckSingle unitSql
                                            case res of
                                                Left synErr ->
                                                    assertFailure
                                                        ( T.unpack (corpusId item)
                                                            <> " stmt #"
                                                            <> show idx
                                                            <> ": ClickHouse syntax error: "
                                                            <> T.unpack synErr
                                                            <> "\nSQL: "
                                                            <> BSC.unpack unitSql
                                                        )
                                                Right () -> pure ()
               | oracleClickHouse extraOracles
               ]
            ++ [ testCase "MySQL 8.4 (15 cases) parse with zero MySQL syntax errors" $ do
                    cases <- either assertFailure pure =<< loadRealFuzzCorpus "benchmarks/real_fuzz_corpus"
                    let mysqlCases = filter ((== MySQL) . corpusDialect) cases
                    assertEqual "MySQL real_fuzz_corpus count" 15 (length mysqlCases)
                    forM_ mysqlCases \item -> do
                        _ <- parseCorpusStmts mysqlProfile item
                        res <- mysqlSyntaxCheckIsolated srv (corpusSql item)
                        case res of
                            Left synErr ->
                                assertFailure
                                    (T.unpack (corpusId item) <> ": MySQL syntax error: " <> show synErr)
                            Right () -> pure ()
               | Just srv <- [oracleMySQL extraOracles]
               ]
            ++ [ testCase "MariaDB 11.4 (12 cases) parse with zero unexpected MariaDB syntax errors" $ do
                    cases <- either assertFailure pure =<< loadRealFuzzCorpus "benchmarks/real_fuzz_corpus"
                    let mariaCases = filter ((== MariaDB) . corpusDialect) cases
                    assertEqual "MariaDB real_fuzz_corpus count" 12 (length mariaCases)
                    forM_ mariaCases \item -> do
                        _ <- parseCorpusStmts mariadbProfile item
                        if corpusId item == "mariadb_mdev22462_in_to_exists_assert"
                            then do
                                -- Squirrel MDEV-22462 triggers on statement #3; statement #5
                                -- emits `RANK(v1) OVER w` (MariaDB RANK() takes 0 args).
                                -- Verify statements 1..4 parse cleanly in MariaDB 11.4.
                                let prefixSql =
                                        BSC.unlines (take 4 (BSC.lines (corpusSql item)))
                                res <- mysqlSyntaxCheckIsolated srv prefixSql
                                case res of
                                    Left synErr ->
                                        assertFailure
                                            (T.unpack (corpusId item) <> " prefix: MariaDB syntax error: " <> show synErr)
                                    Right () -> pure ()
                            else do
                                res <- mysqlSyntaxCheckIsolated srv (corpusSql item)
                                case res of
                                    Left synErr ->
                                        assertFailure
                                            (T.unpack (corpusId item) <> ": MariaDB syntax error: " <> show synErr)
                                    Right () -> pure ()
               | Just srv <- [oracleMariaDB extraOracles]
               ]
        )

-- ---------------------------------------------------------------------------
-- 2. Regression corpus alignment (test/seeds/regression_cases.sql)
-- ---------------------------------------------------------------------------

regressionCorpusAlignment :: Bool -> ExtraOracles -> TestTree
regressionCorpusAlignment pgAvail extraOracles =
    testGroup
        "regression_cases.sql alignment"
        ( [ testCase "PostgreSQL regression cases match libpg_query acceptance and statement count" $ do
                cases <- either assertFailure pure =<< loadRegressionCases "test/seeds/regression_cases.sql"
                let pgCases = filter ((== PostgreSQL) . corpusDialect) cases
                assertBool "PostgreSQL regression cases non-empty" (not (null pgCases))
                mismatches <- collectPgMismatches pgCases
                assertEqual
                    ("PostgreSQL regression mismatches (" <> show (length mismatches) <> "):\n" <> unlines mismatches)
                    []
                    mismatches
          | pgAvail
          ]
            ++ [ testCase "DuckDB regression cases match duckdb_extract_statements acceptance and count" $ do
                    cases <- either assertFailure pure =<< loadRegressionCases "test/seeds/regression_cases.sql"
                    let duckCases = filter ((== DuckDB) . corpusDialect) cases
                    assertBool "DuckDB regression cases non-empty" (not (null duckCases))
                    mismatches <- withDuckDBConn (`collectDuckMismatches` duckCases)
                    assertEqual
                        ("DuckDB regression mismatches (" <> show (length mismatches) <> "):\n" <> unlines mismatches)
                        []
                        mismatches
               ]
            ++ [ testCase "ClickHouse regression cases match clickhouse format acceptance" $ do
                    cases <- either assertFailure pure =<< loadRegressionCases "test/seeds/regression_cases.sql"
                    let chCases = filter ((== ClickHouse) . corpusDialect) cases
                    assertBool "ClickHouse regression cases non-empty" (not (null chCases))
                    mismatches <- collectClickHouseMismatches chCases
                    assertEqual
                        ("ClickHouse regression mismatches (" <> show (length mismatches) <> "):\n" <> unlines mismatches)
                        []
                        mismatches
               | oracleClickHouse extraOracles
               ]
            ++ [ testCase "MySQL 8.4 regression cases match mysqld acceptance" $ do
                    cases <- either assertFailure pure =<< loadRegressionCases "test/seeds/regression_cases.sql"
                    let myCases = filter ((== MySQL) . corpusDialect) cases
                    assertBool "MySQL regression cases non-empty" (not (null myCases))
                    mismatches <- collectMysqlFamilyMismatches "MySQL 8.4" mysqlProfile srv myCases
                    assertEqual
                        ("MySQL 8.4 regression mismatches (" <> show (length mismatches) <> "):\n" <> unlines mismatches)
                        []
                        mismatches
               | Just srv <- [oracleMySQL extraOracles]
               ]
            ++ [ testCase "MariaDB 11.4 regression cases match mariadbd acceptance" $ do
                    cases <- either assertFailure pure =<< loadRegressionCases "test/seeds/regression_cases.sql"
                    let mariaTagged = filter ((== MariaDB) . corpusDialect) cases
                        sharedMyCases =
                            [ c
                            | c <- cases
                            , corpusDialect c == MySQL
                            , corpusId c `notElem` mysqlOnlyRegressionIds
                            ]
                        toCheck = mariaTagged ++ sharedMyCases
                    assertBool "MariaDB regression check cases non-empty" (not (null toCheck))
                    mismatches <- collectMysqlFamilyMismatches "MariaDB 11.4" mariadbProfile srv toCheck
                    assertEqual
                        ("MariaDB 11.4 regression mismatches (" <> show (length mismatches) <> "):\n" <> unlines mismatches)
                        []
                        mismatches
               | Just srv <- [oracleMariaDB extraOracles]
               ]
        )

mysqlOnlyRegressionIds :: [Text]
mysqlOnlyRegressionIds =
    [ "rust:tests:mysql_duplicate_key_reads_cover_keys_and_set_sources:4" -- INSERT ... VALUES ... AS new
    , "rust:tests:stored_calls_fence_conservatively:5" -- MySQL rejects unparenthesized DEFAULT abs(5); MariaDB allows it
    , "rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:8" -- MySQL ((lower(b))) functional index
    , "rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:9" -- MySQL ((f(b))) functional index
    , "rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:14" -- MySQL rejects unparenthesized DEFAULT f(1); MariaDB allows it
    , "rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:11" -- MariaDB aborts mid-parse SP lookup (1327: TEMPORARY) before rejecting @x
    ]

collectPgMismatches :: [CorpusCase] -> IO [String]
collectPgMismatches [] = pure []
collectPgMismatches (item : rest) = do
    let sql = corpusSql item
        cid = T.unpack (corpusId item)
        occamRes = parseScript postgresqlProfile sql
    pgRes <- pgQuerySplitCheck sql
    let here = case (occamRes, pgRes) of
            (Right (ParsedScript stmts), Right pgCount)
                | length stmts == pgCount -> []
                | otherwise ->
                    [ cid
                        <> ": count mismatch occam="
                        <> show (length stmts)
                        <> " pg="
                        <> show pgCount
                        <> " sql="
                        <> show sql
                    ]
            (Left _, Left _) -> []
            (Right (ParsedScript stmts), Left err) ->
                [ cid
                    <> ": OVER-ACCEPTED by Occam ("
                    <> show (length stmts)
                    <> " stmts), rejected by libpg_query: "
                    <> T.unpack (pgErrMessage err)
                    <> " | sql="
                    <> show sql
                ]
            (Left err, Right pgCount) ->
                [ cid
                    <> ": UNDER-ACCEPTED by Occam ("
                    <> show err
                    <> "), accepted by libpg_query ("
                    <> show pgCount
                    <> " stmts) | sql="
                    <> show sql
                ]
    (here ++) <$> collectPgMismatches rest

collectDuckMismatches :: (ByteString -> IO (Either String Int)) -> [CorpusCase] -> IO [String]
collectDuckMismatches _ [] = pure []
collectDuckMismatches extract (item : rest) = do
    let sql = corpusSql item
        cid = T.unpack (corpusId item)
        occamRes = parseScript duckdbProfile sql
    duckRes <- extract sql
    let here = case (occamRes, duckRes) of
            (Right (ParsedScript stmts), Right duckCount)
                | length stmts == duckCount -> []
                | otherwise ->
                    [ cid
                        <> ": count mismatch occam="
                        <> show (length stmts)
                        <> " duckdb="
                        <> show duckCount
                        <> " sql="
                        <> show sql
                    ]
            (Left _, Left _) -> []
            (Right (ParsedScript stmts), Left err) ->
                [ cid
                    <> ": OVER-ACCEPTED by Occam ("
                    <> show (length stmts)
                    <> " stmts), rejected by DuckDB: "
                    <> err
                    <> " | sql="
                    <> show sql
                ]
            (Left err, Right duckCount) ->
                [ cid
                    <> ": UNDER-ACCEPTED by Occam ("
                    <> show err
                    <> "), accepted by DuckDB ("
                    <> show duckCount
                    <> " stmts) | sql="
                    <> show sql
                ]
    (here ++) <$> collectDuckMismatches extract rest

collectClickHouseMismatches :: [CorpusCase] -> IO [String]
collectClickHouseMismatches [] = pure []
collectClickHouseMismatches (item : rest) = do
    let sql = corpusSql item
        cid = T.unpack (corpusId item)
        occamRes = parseScript clickhouseProfile sql
    chRes <- clickhouseSyntaxCheck sql
    let here = case (occamRes, chRes) of
            (Right _, Right ()) -> []
            (Left _, Left _) -> []
            (Right (ParsedScript stmts), Left err) ->
                [ cid
                    <> ": OVER-ACCEPTED by Occam ("
                    <> show (length stmts)
                    <> " stmts), rejected by ClickHouse: "
                    <> T.unpack err
                    <> " | sql="
                    <> show sql
                ]
            (Left err, Right ()) ->
                [ cid
                    <> ": UNDER-ACCEPTED by Occam ("
                    <> show err
                    <> "), accepted by ClickHouse | sql="
                    <> show sql
                ]
    (here ++) <$> collectClickHouseMismatches rest

collectMysqlFamilyMismatches ::
    String -> DialectProfile -> MysqlServer -> [CorpusCase] -> IO [String]
collectMysqlFamilyMismatches _ _ _ [] = pure []
collectMysqlFamilyMismatches label profile srv (item : rest) = do
    let sql = corpusSql item
        cid = T.unpack (corpusId item)
        occamRes = parseScript profile sql
    srvRes <- mysqlSyntaxCheckIsolated srv sql
    let here = case (occamRes, srvRes) of
            (Right _, Right ()) -> []
            (Left _, Left _) -> []
            (Right (ParsedScript stmts), Left err) ->
                [ cid
                    <> ": OVER-ACCEPTED by Occam ("
                    <> show (length stmts)
                    <> " stmts), rejected by "
                    <> label
                    <> ": "
                    <> show err
                    <> " | sql="
                    <> show sql
                ]
            (Left err, Right ()) ->
                [ cid
                    <> ": UNDER-ACCEPTED by Occam ("
                    <> show err
                    <> "), accepted by "
                    <> label
                    <> " | sql="
                    <> show sql
                ]
    (here ++) <$> collectMysqlFamilyMismatches label profile srv rest

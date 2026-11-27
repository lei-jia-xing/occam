{- | Multi-engine in-process syntax alignment & differential syntax fuzzing
against PostgreSQL (libpg_query 18), DuckDB (libduckdb), and SQLite (direct-sqlite).
-}
module SyntaxAlignmentChecks (tests) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, try)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.SQLite3 qualified as Sqlite
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import FrontendCorpusChecks (CorpusCase (..), loadRealFuzzCorpus, loadRegressionCases)
import Occam.Frontend.Parser (ParsedScript (..), parseScript)
import Occam.Frontend.Span (sliceSpan)
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Oracle.DuckDB.FFI (
    DuckDBAPI (..),
    DuckdbConnection,
    DuckdbDatabase,
    closeDatabase,
    getDuckDBAPI,
    openFreshDatabase,
 )
import Occam.Parser (SQLDialect (..))
import Occam.Parser.Dialect (
    DialectProfile,
    duckdbProfile,
    postgresqlProfile,
    sqliteProfile,
 )
import PgQueryOracle (PgSyntaxError (..), isPgQueryAvailable, pgQueryParseCheck, pgQuerySplitCheck)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (
    Gen,
    Property,
    counterexample,
    elements,
    forAll,
    oneof,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: IO TestTree
tests = do
    pgAvail <- isPgQueryAvailable
    pure $
        testGroup
            "Multi-engine syntax alignment"
            [ realFuzzCorpusAlignment pgAvail
            , regressionCorpusAlignment pgAvail
            , differentialFuzzTests pgAvail
            ]

-- ---------------------------------------------------------------------------
-- 1. Real fuzz corpus alignment (benchmarks/real_fuzz_corpus/*.sql)
-- ---------------------------------------------------------------------------

realFuzzCorpusAlignment :: Bool -> TestTree
realFuzzCorpusAlignment pgAvail =
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
        )

-- ---------------------------------------------------------------------------
-- 2. Regression corpus alignment (test/seeds/regression_cases.sql)
-- ---------------------------------------------------------------------------

regressionCorpusAlignment :: Bool -> TestTree
regressionCorpusAlignment pgAvail =
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
        )

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

-- ---------------------------------------------------------------------------
-- 3. QuickCheck differential syntax fuzzing (PG, DuckDB, SQLite)
-- ---------------------------------------------------------------------------

differentialFuzzTests :: Bool -> TestTree
differentialFuzzTests pgAvail =
    testGroup
        "QuickCheck differential syntax fuzzing"
        ( ( if pgAvail
                then
                    [ testProperty "PostgreSQL vs libpg_query: JOIN combinations" (propPgDiff genJoinSql)
                    , testProperty
                        "PostgreSQL vs libpg_query: CREATE modifiers & objects"
                        (propPgDiff (genCreateSql PostgreSQL))
                    , testProperty "PostgreSQL vs libpg_query: row-locking clauses" (propPgDiff genRowLockSql)
                    , testProperty "PostgreSQL vs libpg_query: window & aggregate clauses" (propPgDiff genWindowAggSql)
                    , testProperty "PostgreSQL vs libpg_query: DML tails" (propPgDiff (genDmlTailSql True))
                    , testProperty
                        "PostgreSQL vs libpg_query: expressions & lexical edge cases"
                        (propPgDiff genExprLexSql)
                    ]
                else []
          )
            ++ [ testProperty "DuckDB vs duckdb_extract_statements: JOIN combinations" (propDuckDiff genJoinSql)
               , testProperty
                    "DuckDB vs duckdb_extract_statements: CREATE modifiers & objects"
                    (propDuckDiff (genCreateSql DuckDB))
               , testProperty "DuckDB vs duckdb_extract_statements: row-locking clauses" (propDuckDiff genRowLockSql)
               , testProperty
                    "DuckDB vs duckdb_extract_statements: window & aggregate clauses"
                    (propDuckDiff genWindowAggSql)
               , testProperty "DuckDB vs duckdb_extract_statements: DML tails" (propDuckDiff (genDmlTailSql True))
               , testProperty
                    "DuckDB vs duckdb_extract_statements: expressions & lexical edge cases"
                    (propDuckDiff genExprLexSql)
               , testProperty "SQLite vs sqlite3_prepare: JOIN combinations" (propSqliteDiff genJoinSql)
               , testProperty
                    "SQLite vs sqlite3_prepare: CREATE modifiers & objects"
                    (propSqliteDiff (genCreateSql SQLite))
               , testProperty "SQLite vs sqlite3_prepare: row-locking clauses" (propSqliteDiff genRowLockSql)
               , testProperty
                    "SQLite vs sqlite3_prepare: window & aggregate clauses"
                    (propSqliteDiff genWindowAggSql)
               , testProperty "SQLite vs sqlite3_prepare: DML tails" (propSqliteDiff (genDmlTailSql False))
               , testProperty
                    "SQLite vs sqlite3_prepare: expressions & lexical edge cases"
                    (propSqliteDiff genExprLexSql)
               ]
        )

propPgDiff :: Gen ByteString -> Property
propPgDiff gen = forAll gen \sql ->
    let occamOk = case parseScript postgresqlProfile sql of
            Right _ -> True
            Left _ -> False
        pgRes = unsafePerformIO (pgQueryParseCheck sql)
        pgOk = case pgRes of
            Right () -> True
            Left _ -> False
     in counterexample
            ( "PostgreSQL differential mismatch:\n  SQL: "
                <> BSC.unpack sql
                <> "\n  Occam: "
                <> show (parseScript postgresqlProfile sql)
                <> "\n  libpg_query: "
                <> show pgRes
            )
            (occamOk == pgOk)

propDuckDiff :: Gen ByteString -> Property
propDuckDiff gen = forAll gen \sql ->
    let occamOk = case parseScript duckdbProfile sql of
            Right _ -> True
            Left _ -> False
        duckRes = unsafePerformIO (duckdbExtractOne sql)
        duckOk = case duckRes of
            Right _ -> True
            Left _ -> False
     in counterexample
            ( "DuckDB differential mismatch:\n  SQL: "
                <> BSC.unpack sql
                <> "\n  Occam: "
                <> show (parseScript duckdbProfile sql)
                <> "\n  DuckDB: "
                <> show duckRes
            )
            (occamOk == duckOk)

propSqliteDiff :: Gen ByteString -> Property
propSqliteDiff gen = forAll gen \sql ->
    let occamOk = case parseScript sqliteProfile sql of
            Right _ -> True
            Left _ -> False
        sqliteRes = unsafePerformIO (sqliteSyntaxCheckWithSchema sql)
        sqliteOk = case sqliteRes of
            Right () -> True
            Left _ -> False
     in counterexample
            ( "SQLite differential mismatch:\n  SQL: "
                <> BSC.unpack sql
                <> "\n  Occam: "
                <> show (parseScript sqliteProfile sql)
                <> "\n  SQLite: "
                <> show sqliteRes
            )
            (occamOk == sqliteOk)

-- ---------------------------------------------------------------------------
-- Targeted SQL grammar surface generators
-- ---------------------------------------------------------------------------

genJoinSql :: Gen ByteString
genJoinSql = do
    aliasPrefix <- elements ["", " AS t1"]
    joinPrefix <-
        elements
            [ "JOIN"
            , "INNER JOIN"
            , "LEFT JOIN"
            , "LEFT OUTER JOIN"
            , "RIGHT JOIN"
            , "FULL OUTER JOIN"
            , "CROSS JOIN"
            , "NATURAL JOIN"
            , "NATURAL INNER JOIN"
            , "NATURAL LEFT JOIN"
            , "NATURAL RIGHT JOIN"
            , "NATURAL FULL JOIN"
            , "ASOF JOIN"
            , "ASOF LEFT JOIN"
            , "ASOF RIGHT JOIN"
            ]
    let lhsRef = if BS.null aliasPrefix then "t" else "t1"
    cond <-
        elements
            [ ""
            , " ON " <> lhsRef <> ".a = u.a"
            , " USING (a)"
            ]
    pure ("SELECT 1 FROM t" <> aliasPrefix <> " " <> joinPrefix <> " u" <> cond <> ";")

genCreateSql :: SQLDialect -> Gen ByteString
genCreateSql dialect = do
    -- DuckDB's PG-derived gram.y accepts UNLOGGED in OptTemp even though DuckDB
    -- rejects unlogged relations at bind time; PG gram.y also shares OptTemp on VIEW.
    modifier <-
        elements $
            ["", "TEMP ", "TEMPORARY ", "OR REPLACE "]
                ++ ["UNLOGGED " | dialect `elem` [PostgreSQL, SQLite]]
    obj <-
        elements $
            [ "TABLE t_new (a INT, b TEXT)"
            , "INDEX idx_new ON t (a)"
            , "SEQUENCE seq_new"
            , "SCHEMA s_new"
            , "STATISTICS st_new ON a, b FROM t"
            , "POLICY p_new ON t FOR SELECT USING (a > 0)"
            , "RULE r_new AS ON INSERT TO t DO NOTHING"
            , "MACRO m_new(x) AS x + 1"
            ]
                ++ ["VIEW v_new AS SELECT a FROM t" | not (dialect == PostgreSQL && modifier == "UNLOGGED ")]
    pure ("CREATE " <> modifier <> obj <> ";")

genRowLockSql :: Gen ByteString
genRowLockSql = do
    orderPart <- elements ["", " ORDER BY a"]
    limitPart <- elements ["", " LIMIT 1"]
    lockClause <-
        elements
            [ ""
            , " FOR UPDATE"
            , " FOR SHARE"
            , " FOR NO KEY UPDATE"
            , " FOR KEY SHARE"
            , " FOR UPDATE NOWAIT"
            , " FOR UPDATE SKIP LOCKED"
            , " FOR SHARE OF t"
            ]
    pure ("SELECT a FROM t" <> orderPart <> limitPart <> lockClause <> ";")

genWindowAggSql :: Gen ByteString
genWindowAggSql =
    oneof
        [ do
            filterPart <- elements ["", " FILTER (WHERE a > 0)"]
            overPart <-
                elements
                    [ ""
                    , " OVER ()"
                    , " OVER (PARTITION BY a ORDER BY b DESC NULLS LAST)"
                    , " OVER (ORDER BY a ASC NULLS FIRST)"
                    ]
            pure ("SELECT count(a)" <> filterPart <> overPart <> " FROM t;")
        , do
            nulls <- elements ["", " NULLS FIRST", " NULLS LAST"]
            pure ("SELECT a, b FROM t ORDER BY a DESC" <> nulls <> ";")
        , do
            qualify <- elements ["", " QUALIFY rn = 1"]
            pure ("SELECT a, row_number() OVER (ORDER BY a) AS rn FROM t" <> qualify <> ";")
        , pure "SELECT percentile_disc(0.5) WITHIN GROUP (ORDER BY a) FROM t;"
        ]

genDmlTailSql :: Bool -> Gen ByteString
genDmlTailSql includeUpdateOrderLimit =
    oneof
        [ do
            conflict <-
                elements
                    [ ""
                    , " ON CONFLICT DO NOTHING"
                    , " ON CONFLICT (a) DO UPDATE SET b = excluded.b"
                    ]
            ret <- elements ["", " RETURNING a"]
            pure ("INSERT INTO t (a, b) VALUES (1, 2)" <> conflict <> ret <> ";")
        , do
            fromPart <- elements ["", " FROM u"]
            wherePart <- elements ["", " WHERE t.a = 1"]
            orderLimit <-
                if includeUpdateOrderLimit
                    then elements ["", " ORDER BY a LIMIT 1"]
                    else pure ""
            ret <- elements ["", " RETURNING a"]
            pure ("UPDATE t SET b = 2" <> fromPart <> wherePart <> orderLimit <> ret <> ";")
        , do
            usingPart <- elements ["", " USING u"]
            wherePart <- elements ["", " WHERE t.a = 1"]
            orderLimit <-
                if includeUpdateOrderLimit
                    then elements ["", " ORDER BY a LIMIT 1"]
                    else pure ""
            ret <- elements ["", " RETURNING a"]
            pure ("DELETE FROM t" <> usingPart <> wherePart <> orderLimit <> ret <> ";")
        ]

genExprLexSql :: Gen ByteString
genExprLexSql =
    elements
        [ "SELECT CAST(a AS INT) FROM t;"
        , "SELECT TRY_CAST(a AS INT) FROM t;"
        , "SELECT a::INT FROM t;"
        , "SELECT $tag$hello 'world'$tag$;"
        , "SELECT $$plain 'dollar'$$;"
        , "SELECT E'line\\nbreak' || 'x';"
        , "SELECT /* outer /* inner */ still comment */ 1;"
        , "SELECT a || b FROM t;"
        , "SELECT DISTINCT ON (a) a, b FROM t ORDER BY a, b;"
        , "SELECT a FROM t LIMIT 1 OFFSET 0;"
        , "SELECT a FROM t LIMIT 0, 1;"
        , "FROM t SELECT a;"
        ]

-- ---------------------------------------------------------------------------
-- Engine helpers
-- ---------------------------------------------------------------------------

parseCorpusStmts :: DialectProfile -> CorpusCase -> IO Int
parseCorpusStmts profile item =
    case parseScript profile (corpusSql item) of
        Left err ->
            assertFailure (T.unpack (corpusId item) <> ": Occam parseScript failed: " <> show err)
        Right (ParsedScript stmts) -> pure (length stmts)

withDuckDBConn :: ((ByteString -> IO (Either String Int)) -> IO a) -> IO a
withDuckDBConn action = do
    api <- getDuckDBAPI >>= either (assertFailure . ("DuckDB API unavailable: " <>) . show) pure
    bracket (openFreshDatabase api) (closeDatabase api) \database ->
        alloca \connectionPtr -> do
            status <- apiConnect api database connectionPtr
            if status /= 0
                then assertFailure "DuckDB connection failed"
                else do
                    connection <- peek connectionPtr
                    bracket (pure connection) (\_ -> apiDisconnect api connectionPtr) (action . duckdbExtractOnConn api)

duckdbExtractOnConn :: DuckDBAPI -> DuckdbConnection -> ByteString -> IO (Either String Int)
duckdbExtractOnConn api conn sql =
    alloca \extractedPtr -> do
        count <- BSC.useAsCString sql \sqlPtr ->
            apiExtractStatements api conn sqlPtr extractedPtr
        extracted <- peek extractedPtr
        bracket (pure extracted) (\_ -> apiDestroyExtracted api extractedPtr) \_ -> do
            errorPtr <- apiExtractStatementsError api extracted
            if errorPtr == nullPtr
                then pure (Right (fromIntegral count))
                else Left <$> peekCString errorPtr

{-# NOINLINE sharedDuckDBLock #-}
sharedDuckDBLock :: MVar (DuckDBAPI, DuckdbDatabase, DuckdbConnection)
sharedDuckDBLock = unsafePerformIO do
    api <- getDuckDBAPI >>= either (ioError . userError . ("DuckDB unavailable: " <>) . show) pure
    db <- openFreshDatabase api
    conn <- alloca \ptr -> do
        st <- apiConnect api db ptr
        if st /= 0 then ioError (userError "DuckDB connect failed") else peek ptr
    newMVar (api, db, conn)

duckdbExtractOne :: ByteString -> IO (Either String Int)
duckdbExtractOne sql =
    withMVar sharedDuckDBLock \(api, _, conn) ->
        duckdbExtractOnConn api conn sql

{-# NOINLINE sharedSqliteLock #-}
sharedSqliteLock :: MVar Sqlite.Database
sharedSqliteLock = unsafePerformIO do
    db <- Sqlite.open ":memory:"
    Sqlite.exec
        db
        "CREATE TABLE t (a INT PRIMARY KEY, b INT); CREATE TABLE u (a INT PRIMARY KEY, b INT);"
    newMVar db

sqliteSyntaxCheckWithSchema :: ByteString -> IO (Either Text ())
sqliteSyntaxCheckWithSchema sql =
    withMVar sharedSqliteLock \db -> do
        res <-
            try
                ( bracket (Sqlite.prepare db (TE.decodeUtf8 sql)) Sqlite.finalize \_ ->
                    pure ()
                ) ::
                IO (Either Sqlite.SQLError ())
        pure $ case res of
            Right () -> Right ()
            Left err ->
                let details = Sqlite.sqlErrorDetails err
                    lower = T.toLower details
                    isCatalogOnly =
                        any
                            (`T.isInfixOf` lower)
                            [ "no such column"
                            , "no such function"
                            , "no such table"
                            ]
                 in if isCatalogOnly then Right () else Left details

sqlitePrepareSyntaxCheck :: Sqlite.Database -> ByteString -> IO (Either Text ())
sqlitePrepareSyntaxCheck db unitSql = do
    res <-
        try
            ( bracket (Sqlite.prepare db (TE.decodeUtf8 unitSql)) Sqlite.finalize \_ ->
                pure ()
            ) ::
            IO (Either Sqlite.SQLError ())
    pure $ case res of
        Right () -> Right ()
        Left err ->
            let details = Sqlite.sqlErrorDetails err
                lower = T.toLower details
                isSyntax =
                    any
                        (`T.isInfixOf` lower)
                        [ "syntax error"
                        , "incomplete input"
                        , "unrecognized token"
                        , "natural join"
                        , "cannot have both on and using"
                        ]
             in if isSyntax then Left details else Right ()

module ReductionChecks.BatchAndDeferralChecks (checks) where

import Control.Exception (try)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (elemIndex)
import Occam.Core (
    OracleInconclusiveError,
    OracleOutcome (..),
 )
import Occam.Facts (
    ParserEdit (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Logger (nullLogger)
import Occam.Oracle.DuckDB (
    duckdbContractOracleTimeout,
    generateDuckDBContractWithTrace,
    isDuckDBAvailable,
 )
import Occam.Oracle.Sqlite (
    generateSqliteContract,
    sqliteContractOracle,
 )
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    defaultParseOptions,
    parseSQL,
 )
import Occam.Reducer (
    LiteralDeferralMode (..),
    ReductionConfig (..),
    ReductionResult (..),
    ReductionStage (..),
    StageEvent (..),
    defaultReductionConfigFor,
    reduceSQLWithConfig,
    reduceSQLWithResultTrace,
 )
import ReductionChecks.Support (outcomePool)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

checks :: [TestTree]
checks =
    [ testCase
        "Distributed cross-statement literal batching cuts calls and preserves output across SQLite and DuckDB"
        $ do
            let mkScript colType n =
                    BSC.pack $
                        unlines $
                            ("CREATE TABLE t (a INT, x " ++ colType ++ ");")
                                : [ "INSERT INTO t VALUES ("
                                        ++ show i
                                        ++ ", 'payload_"
                                        ++ show (1000 + i)
                                        ++ "');"
                                  | i <- [1 .. n :: Int]
                                  ]
                                ++ ["SELECT count(*) AS n FROM t;"]
                sqliteSql = mkScript "TEXT" 36
            sqliteContract <- either (assertFailure . show) pure =<< generateSqliteContract sqliteSql
            let runSqlite cfg = do
                    inSyntaxRef <- newIORef False
                    syntaxCallsRef <- newIORef (0 :: Int)
                    let report (StageStarted StageIntraStatementRewrites) = writeIORef inSyntaxRef True
                        report (StageFinished StageIntraStatementRewrites _ _ _) = writeIORef inSyntaxRef False
                        report _ = pure ()
                    pool <- outcomePool $ \cand -> do
                        inSyn <- readIORef inSyntaxRef
                        when inSyn (modifyIORef' syntaxCallsRef (+ 1))
                        sqliteContractOracle sqliteContract cand
                    ReductionResult out _ <-
                        reduceSQLWithResultTrace (const (pure Nothing)) report cfg nullLogger pool sqliteSql
                    calls <- readIORef syntaxCallsRef
                    pure (out, calls)
                baseCfg =
                    (defaultReductionConfigFor SQLite)
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionUnrestricted = False
                        , reductionIntraStatement = True
                        , reductionDistributedLiteralBatches = False
                        , reductionDeferLiteralVariants = DeferLiteralsOff
                        }
                distCfg = baseCfg {reductionDistributedLiteralBatches = True}
            (outOff, callsOff) <- runSqlite baseCfg
            (outDist, callsDist) <- runSqlite distCfg
            (outLitBatch, callsLitBatch) <- runSqlite distCfg {reductionLiteralBatches = True}
            (outAstBatch, callsAstBatch) <- runSqlite distCfg {reductionAstBatching = True}
            assertEqual "distributed batching matches single-edit SQLite output" outOff outDist
            assertEqual "--literal-batches matches single-edit SQLite output" outOff outLitBatch
            assertEqual "--ast-batching matches single-edit SQLite output" outOff outAstBatch
            assertBool
                ( "distributed batching cuts SQLite syntax calls sharply ("
                    ++ show callsDist
                    ++ " vs "
                    ++ show callsOff
                    ++ ")"
                )
                (callsDist * 4 < callsOff)
            assertBool
                ( "--literal-batches keeps cross-statement speedup ("
                    ++ show callsLitBatch
                    ++ " vs "
                    ++ show callsOff
                    ++ ")"
                )
                (callsLitBatch * 4 < callsOff)
            assertBool
                ( "--ast-batching keeps cross-statement speedup ("
                    ++ show callsAstBatch
                    ++ " vs "
                    ++ show callsOff
                    ++ ")"
                )
                (callsAstBatch * 4 < callsOff)
            hasDuck <- isDuckDBAvailable
            when hasDuck $ do
                let duckSql = mkScript "VARCHAR" 32
                    duckTimeoutMicros = 2000000
                (duckContract, _) <-
                    either (assertFailure . show) pure
                        =<< generateDuckDBContractWithTrace duckTimeoutMicros duckSql
                let runDuck cfg = do
                        inSyntaxRef <- newIORef False
                        syntaxCallsRef <- newIORef (0 :: Int)
                        let report (StageStarted StageIntraStatementRewrites) = writeIORef inSyntaxRef True
                            report (StageFinished StageIntraStatementRewrites _ _ _) = writeIORef inSyntaxRef False
                            report _ = pure ()
                        pool <- outcomePool $ \cand -> do
                            inSyn <- readIORef inSyntaxRef
                            when inSyn (modifyIORef' syntaxCallsRef (+ 1))
                            duckdbContractOracleTimeout duckTimeoutMicros duckContract cand
                        ReductionResult out _ <-
                            reduceSQLWithResultTrace (const (pure Nothing)) report cfg nullLogger pool duckSql
                        calls <- readIORef syntaxCallsRef
                        pure (out, calls)
                    duckBase = baseCfg {reductionDialect = DuckDB}
                    duckDist = distCfg {reductionDialect = DuckDB}
                (duckOff, duckCallsOff) <- runDuck duckBase
                (duckOn, duckCallsOn) <- runDuck duckDist
                assertEqual "distributed batching matches single-edit DuckDB output" duckOff duckOn
                assertBool
                    ( "distributed batching cuts DuckDB syntax calls sharply ("
                        ++ show duckCallsOn
                        ++ " vs "
                        ++ show duckCallsOff
                        ++ ")"
                    )
                    (duckCallsOn * 4 < duckCallsOff)
    , testCase
        "Distributed literal batching handles typed, nonzero, head-required, and multi-column scripts"
        $ do
            let typedSql =
                    BSC.pack $
                        unlines $
                            "CREATE TABLE t (a INT, x);"
                                : "INSERT INTO t VALUES (0, 0);"
                                : [ "INSERT INTO t VALUES ("
                                        ++ show i
                                        ++ ", 'payload_"
                                        ++ show (1000 + i)
                                        ++ "');"
                                  | i <- [1 .. 32 :: Int]
                                  ]
                                ++ ["SELECT count(*) AS n, sum(typeof(x) = 'text') AS ok FROM t;"]
                nonzeroSql =
                    BSC.pack $
                        unlines $
                            "CREATE TABLE t (x INT);"
                                : "INSERT INTO t VALUES (0);"
                                : replicate 32 "INSERT INTO t VALUES (1000);"
                                ++ ["SELECT count(*) AS n, sum(CASE WHEN x > 0 THEN 1 ELSE 0 END) AS ok FROM t;"]
                first16Sql =
                    BSC.pack $
                        unlines $
                            "CREATE TABLE t (a INT, x TEXT);"
                                : [ "INSERT INTO t VALUES ("
                                        ++ show i
                                        ++ ", 'payload_"
                                        ++ show (1000 + i)
                                        ++ "');"
                                  | i <- [1 .. 40 :: Int]
                                  ]
                                ++ ["SELECT count(*) AS n, sum(CASE WHEN a <= 16 THEN length(x) ELSE 0 END) AS s FROM t;"]
                multiColSql =
                    BSC.pack $
                        unlines $
                            "CREATE TABLE t (a INT, x TEXT);"
                                : [ "INSERT INTO t VALUES ("
                                        ++ show i
                                        ++ ", 'payload_"
                                        ++ show (1000 + i)
                                        ++ "');"
                                  | i <- [1 .. 36 :: Int]
                                  ]
                                ++ ["SELECT sum(a) AS s FROM t;"]
                cases =
                    [ ("typed", typedSql)
                    , ("nonzero", nonzeroSql)
                    , ("first16", first16Sql)
                    , ("multi-col", multiColSql)
                    ]
            forM_ cases $ \(label, sql) -> do
                contract <- either (assertFailure . show) pure =<< generateSqliteContract sql
                let runDist distOn = do
                        callsRef <- newIORef (0 :: Int)
                        pool <- outcomePool $ \cand -> do
                            modifyIORef' callsRef (+ 1)
                            sqliteContractOracle contract cand
                        let cfg =
                                (defaultReductionConfigFor SQLite)
                                    { reductionSlicing = False
                                    , reductionClosure = False
                                    , reductionUnrestricted = False
                                    , reductionIntraStatement = True
                                    , reductionDistributedLiteralBatches = distOn
                                    , reductionDeferLiteralVariants = DeferLiteralsZeroOne
                                    }
                        out <- reduceSQLWithConfig cfg nullLogger pool sql
                        calls <- readIORef callsRef
                        pure (out, calls)
                (outOff, callsOff) <- runDist False
                (outOn, callsOn) <- runDist True
                if label == "first16"
                    then
                        assertEqual
                            (label ++ " distributed output matches single-edit minimum size")
                            (BS.length outOff)
                            (BS.length outOn)
                    else assertEqual (label ++ " distributed output matches singles") outOff outOn
                verdict <- sqliteContractOracle contract outOn
                assertEqual (label ++ " output preserves SQLite contract") OracleAccepted verdict
                assertBool
                    (label ++ " distributed batching cuts calls (" ++ show callsOn ++ " vs " ++ show callsOff ++ ")")
                    (callsOn < callsOff)
    , testCase
        "Incremental distributed literal batch refresh matches full syntax reparse trace across dialects"
        $ do
            let sql =
                    BSC.pack $
                        unlines $
                            "CREATE TABLE t (a INT, x);"
                                : [ "INSERT INTO t VALUES ("
                                        ++ show i
                                        ++ ", 'payload_"
                                        ++ show (1000 + i)
                                        ++ "');"
                                  | i <- [1 .. 36 :: Int]
                                  ]
                                ++ ["SELECT sum(a) FROM t;"]
                keepKeys = [BSC.pack ("VALUES (" ++ show i ++ ",") | i <- [1 .. 16 :: Int]]
                accepted cand =
                    BSC.isInfixOf "CREATE TABLE t" cand
                        && BSC.isInfixOf "SELECT sum(a)" cand
                        && all (`BSC.isInfixOf` cand) keepKeys
                        && length (filter (BSC.isPrefixOf "INSERT INTO t VALUES") (BSC.lines cand)) == 36
            forM_ [SQLite, PostgreSQL, MySQL, DuckDB] $ \dialect ->
                forM_ [False, True] $ \astBatching -> do
                    let run fullReparse = do
                            traceRef <- newIORef ([] :: [ByteString])
                            pool <- outcomePool $ \cand -> do
                                modifyIORef' traceRef (cand :)
                                pure (if accepted cand then OracleAccepted else OracleRejected)
                            let cfg =
                                    (defaultReductionConfigFor dialect)
                                        { reductionSlicing = False
                                        , reductionClosure = False
                                        , reductionUnrestricted = False
                                        , reductionIntraStatement = True
                                        , reductionDistributedLiteralBatches = True
                                        , reductionAstBatching = astBatching
                                        , reductionDeferLiteralVariants = DeferLiteralsZeroOne
                                        , reductionFullSyntaxReparse = fullReparse
                                        }
                            reduced <- reduceSQLWithConfig cfg nullLogger pool sql
                            trace <- reverse <$> readIORef traceRef
                            pure (reduced, trace)
                    fast@(reducedFast, _) <- run False
                    full <- run True
                    assertBool
                        (show dialect ++ " astBatch=" ++ show astBatching ++ " shrinks SQL")
                        (BS.length reducedFast < BS.length sql)
                    assertEqual
                        (show dialect ++ " astBatch=" ++ show astBatching ++ " exact ordered trace parity")
                        full
                        fast
    , testCase
        "Literal boundary and deferred follow-up edits match full reparse trace across dialects"
        $ do
            let sql =
                    BSC.unlines
                        [ "SELECT'payload_tight'FROM t;"
                        , "SELECT DATE '2020-01-01', 'alpha_one', 12345, 'beta_two';"
                        ]
                accepted cand =
                    BSC.isInfixOf "FROM t;" cand
                        && BSC.isInfixOf "SELECT DATE " cand
                        && (BSC.isInfixOf "'alpha_one'" cand || BSC.isInfixOf "'0'" cand)
                        && (BSC.isInfixOf "12345" cand || BSC.isInfixOf "NULL" cand)
                        && length (BSC.lines cand) == 2
            forM_ [SQLite, PostgreSQL, MySQL, DuckDB] $ \dialect -> do
                let run fullReparse = do
                        traceRef <- newIORef ([] :: [ByteString])
                        pool <- outcomePool $ \cand -> do
                            modifyIORef' traceRef (cand :)
                            pure (if accepted cand then OracleAccepted else OracleRejected)
                        let cfg =
                                (defaultReductionConfigFor dialect)
                                    { reductionSlicing = False
                                    , reductionClosure = False
                                    , reductionUnrestricted = False
                                    , reductionIntraStatement = True
                                    , reductionDeferLiteralVariants = DeferLiteralsZeroOne
                                    , reductionFullSyntaxReparse = fullReparse
                                    }
                        reduced <- reduceSQLWithConfig cfg nullLogger pool sql
                        trace <- reverse <$> readIORef traceRef
                        pure (reduced, trace)
                fast@(reducedFast, _) <- run False
                full <- run True
                assertBool
                    (show dialect ++ " shrinks boundary/deferred literals")
                    (BS.length reducedFast < BS.length sql)
                assertBool (show dialect ++ " reaches deferred '0'") (BSC.isInfixOf "'0'" reducedFast)
                assertBool (show dialect ++ " reaches deferred NULL") (BSC.isInfixOf "NULL" reducedFast)
                assertEqual (show dialect ++ " boundary/deferred trace parity") full fast
    , testCase "Deferred literal variants test primary replacements first and preserve interacting edits" $ do
        let multiCycleSql =
                "CREATE TABLE t (a INT, b INT, tag TEXT); INSERT INTO t VALUES (100, 200, 'Acme Corp'), (300, 400, 'Beta LLC'); SELECT a, tag FROM t WHERE a >= 100 AND b > 0 ORDER BY a LIMIT 2;"
            nonzeroSql =
                "CREATE TABLE t (a INT, b INT); INSERT INTO t VALUES (100, 200), (300, 400); SELECT count(*) AS n FROM t WHERE a > 0 AND b > 0;"
            quote0Sql =
                "CREATE TABLE t (x TEXT); INSERT INTO t VALUES ('payload_alpha'), ('payload_beta'); SELECT count(*) AS n FROM t WHERE x <> '' AND typeof(x) = 'text';"
            unlockSql =
                "CREATE TABLE t (a INT, x TEXT); INSERT INTO t VALUES (100, 'payload_alpha'); SELECT count(*) AS n FROM t WHERE (x = '0' OR x = 'payload_alpha') AND a > 0;"
            runSqlite sql deferMode = do
                contract <- either (assertFailure . show) pure =<< generateSqliteContract sql
                inSyntaxRef <- newIORef False
                traceRef <- newIORef ([] :: [ByteString])
                let report (StageStarted StageIntraStatementRewrites) = writeIORef inSyntaxRef True
                    report (StageFinished StageIntraStatementRewrites _ _ _) = writeIORef inSyntaxRef False
                    report _ = pure ()
                pool <- outcomePool $ \cand -> do
                    inSyn <- readIORef inSyntaxRef
                    when inSyn (modifyIORef' traceRef (cand :))
                    sqliteContractOracle contract cand
                let cfg =
                        (defaultReductionConfigFor SQLite)
                            { reductionSlicing = False
                            , reductionClosure = False
                            , reductionUnrestricted = False
                            , reductionIntraStatement = True
                            , reductionDistributedLiteralBatches = False
                            , reductionDeferLiteralVariants = deferMode
                            }
                ReductionResult out _ <-
                    reduceSQLWithResultTrace (const (pure Nothing)) report cfg nullLogger pool sql
                trace <- reverse <$> readIORef traceRef
                verdict <- sqliteContractOracle contract out
                assertEqual "output satisfies SQLite contract" OracleAccepted verdict
                pure (out, trace)
        (mcOff, mcTraceOff) <- runSqlite multiCycleSql DeferLiteralsOff
        (mcOn, mcTraceOn) <- runSqlite multiCycleSql DeferLiteralsZeroOne
        assertEqual "multi-cycle deferred literals match byte-for-byte" mcOff mcOn
        assertBool
            ( "deferred literal variants cut multi-cycle syntax calls ("
                ++ show (length mcTraceOn)
                ++ " vs "
                ++ show (length mcTraceOff)
                ++ ")"
            )
            (length mcTraceOn < length mcTraceOff)
        let firstPrimaryIdx = elemIndex True (map (BSC.isInfixOf "(0, 200") mcTraceOn)
            firstDeferredIdx = elemIndex True (map (BSC.isInfixOf "NULL") mcTraceOn)
        case (firstPrimaryIdx, firstDeferredIdx) of
            (Just pIdx, Just dIdx) ->
                assertBool "primary replacement is tested before deferred NULL" (pIdx < dIdx)
            (Just _, Nothing) -> pure ()
            _ -> assertFailure "expected primary replacement candidate in trace"
        forM_ [("nonzero", nonzeroSql), ("quote0", quote0Sql), ("unlock", unlockSql)] $ \(label, sql) -> do
            (outOff, _) <- runSqlite sql DeferLiteralsOff
            (outOn, _) <- runSqlite sql DeferLiteralsZeroOne
            assertEqual (label ++ " interacting deferred literal matches exhaustive output") outOff outOn
        (unlockOut, _) <- runSqlite unlockSql DeferLiteralsZeroOne
        assertBool
            "deferred '0' shrinks payload_alpha and collapses OR branch"
            (not (BSC.isInfixOf "payload_alpha" unlockOut))
    , testCase
        "Distributed literal batching defers low-yield empty-string edits under DeferLiteralsZeroOne"
        $ do
            let sql =
                    BSC.pack $
                        unlines $
                            "CREATE TABLE t (a INT, x);"
                                : replicate 32 "INSERT INTO t VALUES (1000, '');"
                                ++ ["SELECT count(*) AS n, group_concat(typeof(x)) AS ok FROM t;"]
                countMatches needle = go (0 :: Int)
                    where
                        go !acc bs =
                            case BSC.breakSubstring needle bs of
                                (_, rest)
                                    | BSC.null rest -> acc
                                    | otherwise -> go (acc + 1) (BSC.drop (BS.length needle) rest)
            contract <- either (assertFailure . show) pure =<< generateSqliteContract sql
            traceRef <- newIORef ([] :: [ByteString])
            pool <- outcomePool $ \cand -> do
                modifyIORef' traceRef (cand :)
                sqliteContractOracle contract cand
            let cfg =
                    (defaultReductionConfigFor SQLite)
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionUnrestricted = False
                        , reductionIntraStatement = True
                        , reductionDistributedLiteralBatches = True
                        , reductionDeferLiteralVariants = DeferLiteralsZeroOne
                        }
            out <- reduceSQLWithConfig cfg nullLogger pool sql
            trace <- reverse <$> readIORef traceRef
            assertBool
                "never batches multiple low-yield '' -> 0 edits ahead of deferral"
                (not (any (\c -> countMatches "VALUES (0, 0)" c >= 2) trace))
            assertBool
                "reduces primary 1000 -> 0 while preserving ''"
                (BSC.isInfixOf "(0, '')" out && not (BSC.isInfixOf "1000" out))
            verdict <- sqliteContractOracle contract out
            assertEqual "output preserves SQLite contract" OracleAccepted verdict
    , testCase
        "Recursive predicate flag roundtrips through binary FFI and scopes only to recursive members"
        $ do
            let sql =
                    BSC.unlines
                        [ "WITH RECURSIVE"
                        , "  h(emp_id, manager_id) AS ("
                        , "    SELECT emp_id, manager_id FROM emp WHERE manager_id IS NULL"
                        , "    UNION ALL"
                        , "    SELECT e.emp_id, e.manager_id FROM emp e JOIN h ON e.manager_id = h.emp_id WHERE e.active = 1"
                        , "  ),"
                        , "  dept AS (SELECT emp_id FROM h WHERE emp_id > 0)"
                        , "SELECT * FROM dept WHERE emp_id > 0;"
                        ]
                opts = (defaultParseOptions SQLite) {parseSyntaxEdits = True}
            parsed <- either (assertFailure . show) pure =<< parseSQL opts sql
            let edits = concatMap (maybe [] factEdits . unitFacts) (parsedUnits parsed)
                snippet e = BS.take (parserEditEnd e - parserEditStart e) (BS.drop (parserEditStart e) sql)
                recEdits = filter parserEditRecursivePredicate edits
                nonRecEdits = filter (not . parserEditRecursivePredicate) edits
            assertBool
                "recursive member JOIN ON predicate has recursive_predicate=True"
                (any (\e -> "e.manager_id = h.emp_id" `BSC.isInfixOf` snippet e) recEdits)
            assertBool
                "recursive member WHERE predicate has recursive_predicate=True"
                (any (\e -> "e.active = 1" `BSC.isInfixOf` snippet e) recEdits)
            assertBool
                "remove-join is not flagged as recursive_predicate"
                (all (\e -> parserEditLabel e /= "remove-join") recEdits)
            assertBool
                "base member WHERE is not flagged as recursive_predicate"
                (not (any (\e -> "manager_id IS NULL" `BSC.isInfixOf` snippet e) recEdits))
            assertBool
                "downstream non-recursive CTE and outer WHERE are not flagged as recursive_predicate"
                (not (any (\e -> "emp_id > 0" `BSC.isInfixOf` snippet e) recEdits))
            assertBool
                "non-recursive edits exist alongside recursive ones"
                (any (\e -> "emp_id > 0" `BSC.isInfixOf` snippet e) nonRecEdits)
    , testCase
        "Deferred recursive predicates retire other edits first while preserving inconclusive timeout status"
        $ do
            let sql =
                    BSC.unlines
                        [ "WITH RECURSIVE h(emp_id, manager_id) AS ("
                        , "  SELECT emp_id, manager_id FROM emp WHERE manager_id IS NULL"
                        , "  UNION ALL"
                        , "  SELECT e.emp_id, e.manager_id FROM emp e JOIN h ON e.manager_id = h.emp_id WHERE e.active = 1"
                        , ")"
                        , "SELECT emp_id, 'long_department_label', 'long_employee_name' FROM h;"
                        ]
                runWith deferRec = do
                    bestRef <- newIORef sql
                    pool <- outcomePool $ \cand ->
                        if not (BSC.isInfixOf "e.manager_id = h.emp_id" cand)
                            && BSC.isInfixOf "JOIN h ON" cand
                            then pure OracleInconclusive
                            else
                                if BSC.isInfixOf "WITH RECURSIVE h" cand
                                    && BSC.isInfixOf "WHERE manager_id IS NULL" cand
                                    && BSC.isInfixOf "JOIN h ON e.manager_id = h.emp_id WHERE e.active = 1" cand
                                    && BSC.isInfixOf "FROM h;" cand
                                    then do
                                        prev <- readIORef bestRef
                                        when (BS.length cand < BS.length prev) (writeIORef bestRef cand)
                                        pure OracleAccepted
                                    else pure OracleRejected
                    let cfg =
                            (defaultReductionConfigFor SQLite)
                                { reductionSlicing = False
                                , reductionClosure = False
                                , reductionUnrestricted = False
                                , reductionIntraStatement = True
                                , reductionDeferRecursivePredicates = deferRec
                                }
                    res <-
                        try (reduceSQLWithConfig cfg nullLogger pool sql) ::
                            IO (Either OracleInconclusiveError ByteString)
                    best <- readIORef bestRef
                    pure (res, best)
            (resOff, bestOff) <- runWith False
            (resOn, bestOn) <- runWith True
            case resOff of
                Left _ -> pure ()
                Right _ -> assertFailure "expected OracleInconclusiveError when recursive deferral is off"
            case resOn of
                Left _ -> pure ()
                Right _ -> assertFailure "deferred recursive timeout must preserve OracleInconclusiveError"
            assertBool
                "deferred mode retires non-recursive string literals before recursive timeout"
                ( not (BSC.isInfixOf "long_department_label" bestOn)
                    && not (BSC.isInfixOf "long_employee_name" bestOn)
                )
            assertBool
                ( "deferred checkpoint is strictly smaller ("
                    ++ show (BS.length bestOn)
                    ++ " vs "
                    ++ show (BS.length bestOff)
                    ++ ")"
                )
                (BS.length bestOn < BS.length bestOff)
    , testCase "Deferred recursive predicates still simplify and complete on terminating recursive CTEs" $ do
        let sqliteSql =
                "CREATE TABLE edges (u INT, v INT, note TEXT); INSERT INTO edges VALUES (1, 2, 'alpha_edge'), (2, 3, 'beta_edge'); WITH RECURSIVE reach(n) AS (SELECT 1 UNION SELECT e.v FROM edges e JOIN reach r ON e.u = r.n AND e.v > 0) SELECT sum(n) AS answer FROM reach;"
        sqliteContract <- either (assertFailure . show) pure =<< generateSqliteContract sqliteSql
        let runSqlite deferRec = do
                pool <- outcomePool (sqliteContractOracle sqliteContract)
                let cfg =
                        (defaultReductionConfigFor SQLite)
                            { reductionIntraStatement = True
                            , reductionDeferRecursivePredicates = deferRec
                            }
                reduceSQLWithConfig cfg nullLogger pool sqliteSql
        sqliteOff <- runSqlite False
        sqliteOn <- runSqlite True
        assertEqual "SQLite terminating recursive CTE matches non-deferred output" sqliteOff sqliteOn
        assertBool
            "SQLite redundant recursive JOIN conjunct is removed"
            (not (BSC.isInfixOf "e.v > 0" sqliteOn))
        sqliteVerdict <- sqliteContractOracle sqliteContract sqliteOn
        assertEqual "SQLite output satisfies contract" OracleAccepted sqliteVerdict
        hasDuck <- isDuckDBAvailable
        when hasDuck $ do
            let duckSql =
                    "CREATE TABLE edges (u INT, v INT, note VARCHAR); INSERT INTO edges VALUES (1, 2, 'alpha_edge'), (2, 3, 'beta_edge'); WITH RECURSIVE reach(n) AS (SELECT 1 UNION SELECT e.v FROM edges e JOIN reach r ON e.u = r.n AND e.v > 0) SELECT sum(n) AS answer FROM reach;"
                duckTimeoutMicros = 2000000
            (duckContract, _) <-
                either (assertFailure . show) pure
                    =<< generateDuckDBContractWithTrace duckTimeoutMicros duckSql
            let runDuck deferRec = do
                    pool <- outcomePool (duckdbContractOracleTimeout duckTimeoutMicros duckContract)
                    let cfg =
                            (defaultReductionConfigFor DuckDB)
                                { reductionIntraStatement = True
                                , reductionDeferRecursivePredicates = deferRec
                                }
                    reduceSQLWithConfig cfg nullLogger pool duckSql
            duckOff <- runDuck False
            duckOn <- runDuck True
            assertEqual "DuckDB terminating recursive CTE matches non-deferred output" duckOff duckOn
            assertBool
                "DuckDB redundant recursive JOIN conjunct is removed"
                (not (BSC.isInfixOf "e.v > 0" duckOn))
            duckVerdict <- duckdbContractOracleTimeout duckTimeoutMicros duckContract duckOn
            assertEqual "DuckDB output satisfies contract" OracleAccepted duckVerdict
    ]

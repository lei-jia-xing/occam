module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (forM, forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.List (intercalate, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Occam.Cache (
    CacheMode (..),
    cacheCompleteInflight,
    cacheInstallInflight,
    cacheLookup,
    cacheLookupKey,
    makeKey,
    newCache,
 )
import Occam.Core (
    OracleOutcome (..),
    SemanticFacts (..),
    Span (..),
    Statement (..),
    StatementId (..),
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    MinimalityClaim (..),
    ReductionActionResult (..),
    runEvaluationRequestWithNativeValidation,
 )
import Occam.Facts (StatementFacts (..), StatementUnit (..))
import Occam.Logger (nullLogger)
import Occam.Metrics (MetricPolicy (..), ParseKind (..))
import Occam.Oracle (OracleObservation (..), OracleObservationDetails (..))
import Occam.Oracle.DuckDB (
    DuckDBContract,
    duckdbContractOracle,
    generateDuckDBContract,
    isDuckDBAvailable,
 )
import Occam.Oracle.Inline (prepareResultOracleWithValidation)
import Occam.Oracle.Sqlite (SqliteContract, generateSqliteContract, sqliteContractOracle)
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    defaultParseOptions,
    nativeOracleCacheKey,
    parseSQLTimed,
 )
import Occam.Pool (poolMetricsStore, withPredicatePool)
import Occam.Reducer (
    AuditStats (..),
    ReductionConfig (..),
    ReductionEvidence (..),
    ReductionResult (..),
    closeSet,
    cutRemovedStatementIds,
    defaultReductionConfig,
    parseStatementsFast,
    reduceSQLWithConfig,
    reduceSQLWithEvidence,
    replaceSpan,
    statementAuditWithLogger,
    statementAuditWithParser,
 )
import Occam.Slicer (backwardSlice)
import Paths_occam qualified
import ResultBaseline (
    runCombinedSyntaxBaseline,
    runDeferLiteralVariants01Baseline,
    runDeferLiteralVariantsBaseline,
    runDeferRecursiveBaseline,
    runDistributedCap16Vs32Baseline,
    runDistributedCap8Vs16Baseline,
    runDistributedLiteralBatchBaseline,
    runDistributedMin16Vs32Baseline,
    runDistributedMisses1Vs2Baseline,
    runDistributedMisses2Vs3Baseline,
    runDistributedRefreshBaseline,
    runEarlyRejectBaseline,
    runGroupProposalsBaseline,
    runGroupProposalsNoLocBaseline,
    runLiteralBatchBaseline,
    runReparseBaseline,
    runResultBaseline,
    runSearchBaseline,
    runStatementBoundedPrefClosureBaseline,
    runStatementFallbackBaseline,
    runStatementFallbackNoLocBaseline,
    runStatementInterleaveBaseline,
    runStatementLanePrefBaseline,
    runStatementLanePrefQuota2Baseline,
    runStatementLanePrefQuota8Baseline,
    runStatementOrderBaseline,
    runStatementProjectedRankBaseline,
    runStatementProjectionScoreBaseline,
    runStatementRankBaseline,
    runStatementRankedBaseline,
    runStatementRankedGraphFreeBaseline,
    runStatementRankedResumeBaseline,
    runStatementRankedResumeTiesBaseline,
    runStatementResumeMonoBaseline,
    runStatementWeightBaseline,
    runStatementWindowCrossVsLaneBaseline,
    runStatementWindowPrefetchBaseline,
    runStatementWindowSize16Baseline,
    runStatementWindowSize4Baseline,
    runStatementWindowWithinLaneBaseline,
    runStatementYieldBaseline,
    runValuesScheduleBaseline,
    runWorkerBaseline,
 )
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), die)
import System.FilePath (takeBaseName, (</>))
import Test.Tasty.Bench (
    bcompare,
    bench,
    bgroup,
    defaultMain,
    whnf,
    whnfIO,
 )
import Text.Read (readMaybe)

genScale :: Int -> ByteString
genScale n =
    BSC.pack $
        "CREATE TABLE noise_a(id INTEGER PRIMARY KEY, grp INT, val TEXT);\n"
            ++ "INSERT INTO noise_a(id, grp, val) VALUES "
            ++ rowsA
            ++ ";\n"
            ++ "CREATE TABLE noise_b(id INTEGER PRIMARY KEY, a_id INT REFERENCES noise_a(id), tag TEXT);\n"
            ++ "INSERT INTO noise_b(id, a_id, tag) VALUES "
            ++ rowsB
            ++ ";\n"
            ++ "CREATE INDEX idx_noise_b_tag ON noise_b(tag);\n"
            ++ "SELECT count(*), sum(id) FROM noise_a;\n"
    where
        rowsA =
            intercalate
                ", "
                ["(" ++ show i ++ ", " ++ show (i `mod` 8) ++ ", 'v" ++ show (i `mod` 13) ++ "')" | i <- [1 .. n]]
        rowsB =
            intercalate
                ", "
                [ "(" ++ show i ++ ", " ++ show ((i * 7) `mod` n + 1) ++ ", 't" ++ show (i `mod` 5) ++ "')"
                | i <- [1 .. n]
                ]

{- | Audit workloads that respectively accept every early deletion and
reject every deletion. The latter includes wide trivia in the source spans.
-}
auditRemovable :: Int -> ByteString
auditRemovable count =
    BSC.unlines ["SELECT " <> BSC.pack (show value) <> ";" | value <- [1 .. count]]

auditRequiredWide :: Int -> ByteString
auditRequiredWide count =
    BSC.unlines $
        "CREATE TABLE t(x);"
            : ["INSERT INTO t VALUES(" <> BSC.pack (show value) <> "); -- " <> padding | value <- [1 .. count]]
            ++ ["SELECT count(*) AS n FROM t;"]
    where
        padding = BS.replicate 440 120

runNativeAudit :: ByteString -> ByteString -> IO Int
runNativeAudit sql expected = do
    (preflight, observe, validate) <-
        prepareResultOracleWithValidation False Nothing (\_ _ -> pure ()) SQLite timeoutMicros sql
            >>= either (die . T.unpack) pure
    result <-
        runEvaluationRequestWithNativeValidation timeoutMicros observe validate $
            EvaluationRequest
                { requestOnProgress = \_ -> pure ()
                , requestMetricsPolicy = Just (MetricPolicy "audit-only" "off" 1)
                , requestConfig =
                    EvaluationConfig
                        { evaluationCacheEnabled = True
                        , evaluationJobs = 1
                        , evaluationTotalTimeoutMicros = Nothing
                        , evaluationMaxOracleExecutions = Nothing
                        , evaluationPreflightMicros = fromMaybe 0 (oracleElapsedMicros preflight)
                        , evaluationPreflightAttempts = 1
                        , evaluationPreflightResetMicros = Just 0
                        , evaluationPreflightOracleMicros = oracleElapsedMicros preflight
                        , evaluationInitialStatements = Nothing
                        }
                , requestDialect = SQLite
                , requestOracleConfig = ()
                , requestPreflightObservation = Just (detailedObservation preflight)
                , requestLogger = nullLogger
                , requestInitialSQL = sql
                , requestCheckpoint = ""
                , requestAction = \_ logger pool -> do
                    (reduced, stats) <-
                        statementAuditWithParser logger (parseStatementsFast (poolMetricsStore pool) SQLite) pool sql
                    pure
                        (ReductionActionResult reduced (Just (ReductionEvidence reduced (auditMinimalityConfirmed stats))))
                , requestCollectCurve = True
                }
    let metrics = evaluationMetrics result
    unless
        ( evaluatedSQL result == expected
            && evaluationStatus metrics == EvaluationCompleted
            && evaluationValidationPassed metrics
            && evaluationMinimalityClaim metrics == StatementDeletionOneMinimal
        )
        (die "occam-bench: product audit failed its endpoint or validation check")
    pure (BS.length (evaluatedSQL result))
    where
        timeoutMicros = 5000000

corpusCases :: [FilePath]
corpusCases =
    [ "01_cte_recursive_analytics.sql"
    , "02_transaction_savepoint_churn.sql"
    , "03_deep_expressions_polymorphic.sql"
    , "04_multitable_diamond_dag.sql"
    , "05_upsert_conflict.sql"
    , "06_views_and_aggregates.sql"
    , "07_column_explicit.sql"
    , "08_column_implicit.sql"
    , "09_column_star.sql"
    ]

locateCorpusFile :: FilePath -> IO FilePath
locateCorpusFile name = do
    let direct = "benchmarks/corpus" </> name
    directExists <- doesFileExist direct
    if directExists
        then pure direct
        else do
            dataDirFile <-
                catch
                    (Paths_occam.getDataFileName ("benchmarks/corpus" </> name))
                    (\(_ :: IOException) -> pure direct)
            dataExists <- doesFileExist dataDirFile
            if dataExists
                then pure dataDirFile
                else
                    die
                        ( "[BENCHMARK ERROR] Required corpus file not found: "
                            ++ name
                            ++ " (checked "
                            ++ direct
                            ++ " and "
                            ++ dataDirFile
                            ++ ")"
                        )

loadCorpusCase :: FilePath -> IO (String, ByteString, SqliteContract)
loadCorpusCase name = do
    path <- locateCorpusFile name
    sql <- BS.readFile path
    cRes <- generateSqliteContract sql
    case cRes of
        Left err ->
            die ("[BENCHMARK ERROR] Failed to generate SQLite contract for " ++ name ++ ": " ++ T.unpack err)
        Right contract -> pure (takeBaseName name, sql, contract)

-- | Eight dependency-linked statements per table: DDL, writes, and reads.
tableBlock :: Int -> [ByteString]
tableBlock i =
    map
        BSC.pack
        [ "CREATE TABLE t" ++ s ++ " (a INT PRIMARY KEY, b TEXT);"
        , "INSERT INTO t" ++ s ++ " (a, b) VALUES (1, 'x');"
        , "INSERT INTO t" ++ s ++ " (a, b) VALUES (2, 'y');"
        , "SELECT a, b FROM t" ++ s ++ " WHERE a > 1 ORDER BY a;"
        , "UPDATE t" ++ s ++ " SET b = 'z' WHERE a = 1;"
        , "DELETE FROM t" ++ s ++ " WHERE a = 2;"
        , "SELECT COUNT(*) FROM t" ++ s ++ ";"
        , "INSERT INTO t" ++ s ++ " (a, b) VALUES (3, 'w');"
        ]
    where
        s = show i

syntheticSQL :: Int -> ByteString
syntheticSQL tables = BSC.unlines (concatMap tableBlock [0 .. tables - 1])

-- | Parser payload per tier, mirroring the reducer's own option sets.
optionsForKind :: ParseKind -> ParseOptions
optionsForKind ParseStatementOnly = defaultParseOptions GenericSQL
optionsForKind ParseSyntaxChunks =
    (defaultParseOptions GenericSQL) {parseChunkEdits = True, parseSyntaxEdits = True}
optionsForKind ParseColumnIR =
    (defaultParseOptions GenericSQL)
        { parseChunkEdits = True
        , parseSyntaxEdits = True
        , parseColumnIR = True
        , parseColumnBundles = True
        }

parseUnitsOrDie :: ParseKind -> ByteString -> IO ParsedSQL
parseUnitsOrDie kind sql = do
    parsed <- parseSQLTimed Nothing kind (optionsForKind kind) sql
    case parsed of
        Left err -> die ("occam-bench: synthetic SQL failed to parse: " ++ show err)
        Right ok -> pure ok

-- | Spine-plus-payload checksum so laziness cannot hide decode work.
parsedWorkload :: ParsedSQL -> Int
parsedWorkload parsed = sum (map unitWork (parsedUnits parsed))
    where
        unitWork unit =
            let Span start end = stmtSpan (unitStatement unit)
             in (end - start) + case unitFacts unit of
                    Nothing -> 0
                    Just facts -> length (factOps (factSemantics facts)) + length (factEdits facts)

graphWorkload :: DependencyGraph -> Int
graphWorkload graph = Map.foldl' (\total deps -> total + Set.size deps) 0 (graphEdges graph)

-- | Strict byte-sum checksum over spliced chunks.
byteChecksum :: ByteString -> Int
byteChecksum = BS.foldl' (\total w -> total + fromIntegral w) 0

-- | Eight-id window rotating with the iteration so every repetition re-slices.
windowIds :: [StatementId] -> Int -> Set.Set StatementId
windowIds ids i = Set.fromList (take 8 (drop ((i * 8) `mod` (length ids - 8)) ids))

checkCount :: Int -> ParsedSQL -> IO ()
checkCount expected parsed =
    let actual = length (parsedUnits parsed)
     in unless (actual == expected) $
            die ("occam-bench: expected " ++ show expected ++ " statements, parsed " ++ show actual)

-- NOTE: The tasty-bench audit groups time a direct reducer/audit probe.
-- Use --result-baseline for the shared evaluation path and its validation evidence.

-- | In-process reduction path for SQLite contract benchmarking.
runInProcessReductionPath :: SqliteContract -> ByteString -> IO ByteString
runInProcessReductionPath contract sql = do
    reduced <-
        withPredicatePool 1 True (fmap (== OracleAccepted) . sqliteContractOracle contract) $ \pool -> do
            ReductionResult reducedSql _evidence <-
                reduceSQLWithEvidence
                    (\_ -> pure ())
                    (defaultReductionConfig {reductionDialect = SQLite})
                    nullLogger
                    pool
                    sql
            fst <$> statementAuditWithLogger nullLogger SQLite pool reducedSql
    valRes <- sqliteContractOracle contract reduced
    unless (valRes == OracleAccepted) $
        die "Final validation failed"
    pure reduced

-- | In-process reduction path for DuckDB contract benchmarking.
runInProcessDuckDBReductionPath :: DuckDBContract -> ByteString -> IO ByteString
runInProcessDuckDBReductionPath contract sql = do
    reduced <-
        withPredicatePool 1 True (fmap (== OracleAccepted) . duckdbContractOracle contract) $ \pool -> do
            ReductionResult reducedSql _evidence <-
                reduceSQLWithEvidence
                    (\_ -> pure ())
                    (defaultReductionConfig {reductionDialect = DuckDB})
                    nullLogger
                    pool
                    sql
            fst <$> statementAuditWithLogger nullLogger DuckDB pool reducedSql
    valRes <- duckdbContractOracle contract reduced
    unless (valRes == OracleAccepted) $
        die "Final DuckDB validation failed"
    pure reduced

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--result-baseline", count, out]
            | Just pairs <- readMaybe count
            , pairs >= 2
            , even pairs ->
                runResultBaseline pairs out
        "--result-baseline" : _ -> die "Usage: occam-bench --result-baseline EVEN_PAIRS OUTPUT.json (at least 2)"
        [mode, count, budget, out]
            | Just run <- lookup mode searchModes
            , Just pairs <- readMaybe count
            , pairs >= 2
            , even pairs
            , Just (wallLimit, callLimit) <- parseSearchLimits budget ->
                run pairs wallLimit callLimit out
        mode : _
            | mode `elem` map fst searchModes ->
                die
                    ("Usage: occam-bench " ++ mode ++ " EVEN_PAIRS (unlimited|CALLS >= 4|wall:SECONDS >= 20) OUTPUT.json")
        _ -> runTimingBenchmarks

searchModes :: [(String, Int -> Maybe Int -> Maybe Int -> FilePath -> IO ())]
searchModes =
    [ ("--search-baseline", runSearchBaseline)
    , ("--literal-batch-baseline", runLiteralBatchBaseline)
    , ("--values-schedule-baseline", runValuesScheduleBaseline)
    , ("--combined-syntax-baseline", runCombinedWithCorpus)
    , ("--statement-yield-baseline", runStatementYieldBaseline)
    , ("--statement-order-baseline", runStatementOrderBaseline)
    , ("--statement-weight-baseline", runStatementWeightBaseline)
    , ("--statement-ranked-baseline", runStatementRankedBaseline)
    , ("--statement-ranked-graph-free-baseline", runStatementRankedGraphFreeBaseline)
    , ("--statement-rank-ablation", runStatementRankBaseline)
    , ("--statement-interleave-ablation", runStatementInterleaveBaseline)
    , ("--statement-ranked-resume-ablation", runStatementRankedResumeBaseline)
    , ("--statement-ranked-resume-ties-ablation", runStatementRankedResumeTiesBaseline)
    , ("--statement-projected-rank-baseline", runStatementProjectedRankBaseline)
    , ("--statement-projection-score-baseline", runStatementProjectionScoreBaseline)
    , ("--statement-window-prefetch-baseline", runStatementWindowPrefetchBaseline)
    , ("--statement-window-within-lane-baseline", runStatementWindowWithinLaneBaseline)
    , ("--statement-window-cross-vs-lane-baseline", runStatementWindowCrossVsLaneBaseline)
    , ("--statement-window-size-4-baseline", runStatementWindowSize4Baseline)
    , ("--statement-window-size-16-baseline", runStatementWindowSize16Baseline)
    , ("--statement-bounded-pref-closure-baseline", runStatementBoundedPrefClosureBaseline)
    , ("--statement-resume-mono-baseline", runStatementResumeMonoBaseline)
    , ("--statement-lane-pref-baseline", runStatementLanePrefBaseline)
    , ("--statement-lane-pref-quota-2-baseline", runStatementLanePrefQuota2Baseline)
    , ("--statement-lane-pref-quota-8-baseline", runStatementLanePrefQuota8Baseline)
    , ("--statement-fallback-baseline", runStatementFallbackBaseline)
    , ("--statement-fallback-noloc-baseline", runStatementFallbackNoLocBaseline)
    , ("--group-proposals-baseline", runGroupProposalsBaseline)
    , ("--group-proposals-noloc-baseline", runGroupProposalsNoLocBaseline)
    , ("--early-reject-baseline", runEarlyRejectBaseline)
    , ("--reparse-baseline", runReparseBaseline)
    , ("--worker-baseline", runWorkerBaseline)
    , ("--distributed-literal-batch-baseline", runDistributedLiteralBatchBaseline)
    , ("--distributed-refresh-baseline", runDistributedRefreshBaseline)
    , ("--distributed-misses-1-vs-2-baseline", runDistributedMisses1Vs2Baseline)
    , ("--distributed-misses-2-vs-3-baseline", runDistributedMisses2Vs3Baseline)
    , ("--distributed-min16-vs-32-baseline", runDistributedMin16Vs32Baseline)
    , ("--distributed-cap8-vs-16-baseline", runDistributedCap8Vs16Baseline)
    , ("--distributed-cap16-vs-32-baseline", runDistributedCap16Vs32Baseline)
    , ("--defer-literal-variants-baseline", runDeferLiteralsWithCorpus runDeferLiteralVariantsBaseline)
    ,
        ( "--defer-literal-variants-01-baseline"
        , runDeferLiteralsWithCorpus runDeferLiteralVariants01Baseline
        )
    , ("--defer-recursive-baseline", runDeferRecursiveBaseline)
    ]

runDeferLiteralsWithCorpus ::
    ( (ByteString, ByteString, ByteString, ByteString, ByteString, ByteString) ->
      Int ->
      Maybe Int ->
      Maybe Int ->
      FilePath ->
      IO ()
    ) ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runDeferLiteralsWithCorpus runner pairs wallLimit callLimit out = do
    txChurn <- locateCorpusFile "02_transaction_savepoint_churn.sql" >>= BS.readFile
    deep <- locateCorpusFile "03_deep_expressions_polymorphic.sql" >>= BS.readFile
    diamond <- locateCorpusFile "04_multitable_diamond_dag.sql" >>= BS.readFile
    upsert <- locateCorpusFile "05_upsert_conflict.sql" >>= BS.readFile
    views <- locateCorpusFile "06_views_and_aggregates.sql" >>= BS.readFile
    runner (txChurn, deep, diamond, upsert, views, genScale 64) pairs wallLimit callLimit out

runCombinedWithCorpus :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runCombinedWithCorpus pairs wallLimit callLimit out = do
    deep <- locateCorpusFile "03_deep_expressions_polymorphic.sql" >>= BS.readFile
    views <- locateCorpusFile "06_views_and_aggregates.sql" >>= BS.readFile
    runCombinedSyntaxBaseline (genScale 64, deep, views) pairs wallLimit callLimit out

parseSearchLimits :: String -> Maybe (Maybe Int, Maybe Int)
parseSearchLimits "unlimited" = Just (Nothing, Nothing)
parseSearchLimits budget
    | Just raw <- stripPrefix "wall:" budget = do
        seconds <- readMaybe raw
        if seconds >= 20 && seconds <= maxBound `div` 1000000
            then Just (Just (seconds * 1000000), Nothing)
            else Nothing
    | otherwise = do
        calls <- readMaybe budget
        if calls >= 4 then Just (Nothing, Just calls) else Nothing

runTimingBenchmarks :: IO ()
runTimingBenchmarks = do
    let sql64 = syntheticSQL 8
        sql512 = syntheticSQL 64
    -- Pre-parse once for the DAG/surgery stages; parsing itself is timed below.
    parsed64 <- parseUnitsOrDie ParseStatementOnly sql64
    parsed512 <- parseUnitsOrDie ParseStatementOnly sql512
    checkCount 64 parsed64
    checkCount 512 parsed512
    let units64 = parsedUnits parsed64
        units512 = parsedUnits parsed512
        sess64 = parsedInitialSession parsed64
        sess512 = parsedInitialSession parsed512
        graph64 = analyzeStatements sess64 units64
        graph512 = analyzeStatements sess512 units512
        ids64 = map (stmtId . unitStatement) units64
        ids512 = map (stmtId . unitStatement) units512
        targets64 = drop (length ids64 - 8) ids64
        targets512 = drop (length ids512 - 8) ids512
        stmts512 = map unitStatement units512
        keptEven = Set.fromList [stmtId stmt | (i, stmt) <- zip [0 :: Int ..] stmts512, even i]
    cache <- newCache Map.empty
    let candidateKey = BSC.pack "SELECT * FROM t0 WHERE a = 1;"
        nonExistentKey = BSC.pack "SELECT * FROM non_existent WHERE x = 999;"
        obs = OracleFinished ExitSuccess
    -- Populate cache with diverse candidate entries so HAMT depth is realistic
    forM_ [0 .. 511 :: Int] $ \i -> do
        let key = BSC.pack ("SELECT a, b FROM t" ++ show i ++ " WHERE a > " ++ show i ++ ";")
        cacheInstallInflight cache key i
        cacheCompleteInflight cache key i (Just obs)
    cacheInstallInflight cache candidateKey 512
    cacheCompleteInflight cache candidateKey 512 (Just obs)
    largeCacheCases <- forM [4096, 32768, 131072] $ \size -> do
        let stored = BS.replicate size 120
            hit = BS.copy stored
            miss = BS.snoc (BS.init stored) 121
        largeCache <- newCache (Map.singleton stored obs)
        pure (size, largeCache, hit, miss, makeKey hit)
    let sqlScale64 = genScale 64
    contractRes <- generateSqliteContract sqlScale64
    contractScale64 <- case contractRes of
        Left err -> die ("Failed to generate contract for scale 64: " ++ show err)
        Right c -> pure c
    let oracleScale64 = sqliteContractOracle contractScale64
    loadedCorpus <- mapM loadCorpusCase corpusCases
    hasDuckDB <- isDuckDBAvailable
    mDuckDbBench <-
        if hasDuckDB
            then do
                let duckdbSql =
                        "CREATE MACRO m_add(x) AS x + 1;\n\
                        \CREATE TABLE noise_tbl (v INT);\n\
                        \INSERT INTO noise_tbl VALUES (10), (20);\n\
                        \CREATE TABLE real_tbl (id INT, val TEXT);\n\
                        \INSERT INTO real_tbl VALUES (m_add(41), 'result');\n\
                        \SELECT id, val FROM real_tbl;"
                cRes <- generateDuckDBContract duckdbSql
                case cRes of
                    Right c ->
                        pure
                            [ bgroup
                                "reduction-duckdb-validation"
                                [ bench "macro-and-table-reduction" $ whnfIO (runInProcessDuckDBReductionPath c duckdbSql)
                                ]
                            ]
                    Left _ -> pure []
            else pure []
    defaultMain
        ( [ bgroup
                "parse"
                [ bgroup
                    "64"
                    [ bench "stmt-only" $ whnfIO (parsedWorkload <$> parseUnitsOrDie ParseStatementOnly sql64)
                    , bcompare "$NF == \"stmt-only\" && $(NF-1) == \"64\"" $
                        bench "syntax-chunks" $
                            whnfIO (parsedWorkload <$> parseUnitsOrDie ParseSyntaxChunks sql64)
                    , bcompare "$NF == \"stmt-only\" && $(NF-1) == \"64\"" $
                        bench "column-ir" $
                            whnfIO (parsedWorkload <$> parseUnitsOrDie ParseColumnIR sql64)
                    ]
                , bgroup
                    "512"
                    [ bench "stmt-only" $ whnfIO (parsedWorkload <$> parseUnitsOrDie ParseStatementOnly sql512)
                    , bcompare "$NF == \"stmt-only\" && $(NF-1) == \"512\"" $
                        bench "syntax-chunks" $
                            whnfIO (parsedWorkload <$> parseUnitsOrDie ParseSyntaxChunks sql512)
                    , bcompare "$NF == \"stmt-only\" && $(NF-1) == \"512\"" $
                        bench "column-ir" $
                            whnfIO (parsedWorkload <$> parseUnitsOrDie ParseColumnIR sql512)
                    ]
                ]
          , bgroup
                "dag"
                [ bgroup
                    "analyze"
                    [ bench "64" $ whnf (graphWorkload . analyzeStatements sess64) units64
                    , bcompare "$NF == \"64\" && $(NF-1) == \"analyze\"" $
                        bench "512" $
                            whnf (graphWorkload . analyzeStatements sess512) units512
                    ]
                , bgroup
                    "backward-slice"
                    [ bench "64" $ whnf (Set.size . flip backwardSlice graph64) (last targets64)
                    , bcompare "$NF == \"64\" && $(NF-1) == \"backward-slice\"" $
                        bench "512" $
                            whnf (Set.size . flip backwardSlice graph512) (last targets512)
                    ]
                , bgroup
                    "close-set"
                    [ bench "64" $ whnf (Set.size . closeSet graph64) (windowIds ids64 0)
                    , bcompare "$NF == \"64\" && $(NF-1) == \"close-set\"" $
                        bench "512" $
                            whnf (Set.size . closeSet graph512) (windowIds ids512 0)
                    ]
                ]
          , bgroup
                "surgery"
                [ bench "cut-removed" $ whnf (byteChecksum . cutRemovedStatementIds sql512 stmts512) keptEven
                , bcompare "$NF == \"cut-removed\" && $(NF-1) == \"surgery\"" $
                    bench "replace-span" $
                        whnf
                            (\start -> byteChecksum (replaceSpan start (start + 896) ("/* trimmed */" :: ByteString) sql512))
                            (128 :: Int)
                ]
          , bgroup
                "cache"
                ( [ bench "hit" $ whnfIO (cacheLookup cache CacheEnabled candidateKey)
                  , bcompare "$NF == \"hit\" && $(NF-1) == \"cache\"" $
                        bench "miss" $
                            whnfIO (cacheLookup cache CacheEnabled nonExistentKey)
                  , bench "native-key-multi-statement" $ whnfIO (nativeOracleCacheKey SQLite (auditRemovable 512))
                  ]
                    ++ [ bgroup
                            (show size ++ "-bytes")
                            [ bench "hit" $ whnfIO (cacheLookup largeCache CacheEnabled hit)
                            , bench "prepared-hit" $ whnfIO (cacheLookupKey largeCache CacheEnabled prepared)
                            , bench "miss" $ whnfIO (cacheLookup largeCache CacheEnabled miss)
                            ]
                       | (size, largeCache, hit, miss, prepared) <- largeCacheCases
                       ]
                )
          , bgroup
                "audit-product"
                [ bench "accept-frequently" $ whnfIO (runNativeAudit (auditRemovable 512) "SELECT 512;\n")
                , let sql = auditRequiredWide 512
                   in bench "reject-all-wide" $ whnfIO (runNativeAudit sql sql)
                ]
          , bgroup
                "reduction"
                [ bgroup
                    "scale-064"
                    [ bench "default" $
                        whnfIO $
                            withPredicatePool 1 True (fmap (== OracleAccepted) . oracleScale64) $ \pool ->
                                reduceSQLWithConfig
                                    (defaultReductionConfig {reductionDialect = SQLite})
                                    nullLogger
                                    pool
                                    sqlScale64
                    , bcompare "$NF == \"default\" && $(NF-1) == \"scale-064\"" $
                        bench "intra-statement" $
                            whnfIO $
                                withPredicatePool 1 True (fmap (== OracleAccepted) . oracleScale64) $ \pool ->
                                    reduceSQLWithConfig
                                        (defaultReductionConfig {reductionDialect = SQLite, reductionIntraStatement = True})
                                        nullLogger
                                        pool
                                        sqlScale64
                    ]
                , bgroup
                    "corpus"
                    [ bgroup
                        name
                        ( bench
                            "default"
                            ( whnfIO $
                                withPredicatePool 1 True (fmap (== OracleAccepted) . sqliteContractOracle contract) $ \pool ->
                                    reduceSQLWithConfig
                                        (defaultReductionConfig {reductionDialect = SQLite})
                                        nullLogger
                                        pool
                                        sql
                            )
                            : [ bcompare ("$NF == \"default\" && $(NF-1) == \"" ++ name ++ "\"") $
                                    bench "intra-statement" $
                                        whnfIO $
                                            withPredicatePool 1 True (fmap (== OracleAccepted) . sqliteContractOracle contract) $ \pool ->
                                                reduceSQLWithConfig
                                                    (defaultReductionConfig {reductionDialect = SQLite, reductionIntraStatement = True})
                                                    nullLogger
                                                    pool
                                                    sql
                              | name /= "01_cte_recursive_analytics"
                              ]
                        )
                    | (name, sql, contract) <- loadedCorpus
                    ]
                ]
          , bgroup
                "reduction-audit-validation"
                [ bench name $ whnfIO (runInProcessReductionPath contract sql)
                | (name, sql, contract) <- loadedCorpus
                ]
          ]
            ++ mDuckDbBench
        )

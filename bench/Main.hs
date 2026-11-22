module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Occam.Cache (
    CacheMode (..),
    cacheCompleteInflight,
    cacheInstallInflight,
    cacheLookup,
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
import Occam.Facts (StatementFacts (..), StatementUnit (..))
import Occam.Logger (nullLogger)
import Occam.Metrics (ParseKind (..))
import Occam.Oracle (OracleObservation (..))
import Occam.Oracle.DuckDB (
    DuckDBContract,
    duckdbContractOracle,
    generateDuckDBContract,
    isDuckDBAvailable,
 )
import Occam.Oracle.Sqlite (SqliteContract, generateSqliteContract, sqliteContractOracle)
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    defaultParseOptions,
    parseSQLTimed,
 )
import Occam.Pool (withPredicatePool)
import Occam.Reducer (
    ReductionConfig (..),
    ReductionResult (..),
    closeSet,
    cutRemovedStatementIds,
    defaultReductionConfig,
    reduceSQLWithConfig,
    reduceSQLWithEvidence,
    replaceSpan,
    statementAuditWithLogger,
 )
import Occam.Slicer (backwardSlice)
import Paths_occam qualified
import ResultBaseline (runResultBaseline)
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
        _ -> runTimingBenchmarks

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
                [ bench "hit" $ whnfIO (cacheLookup cache CacheEnabled candidateKey)
                , bcompare "$NF == \"hit\" && $(NF-1) == \"cache\"" $
                    bench "miss" $
                        whnfIO (cacheLookup cache CacheEnabled nonExistentKey)
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

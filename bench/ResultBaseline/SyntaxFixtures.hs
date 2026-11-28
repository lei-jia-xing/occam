-- | INSERT, distributed-batching, and deferral fixture cohorts.
module ResultBaseline.SyntaxFixtures (
    insertRowsFixture,
    wideInsertsFixture,
    distributedBatchFixtures,
    sqliteDistributedFixtures,
    deferLiteralFixtures,
    recursiveTerminatingFixtures,
)
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Occam.Core (ResultStatement (..))
import Occam.Parser (SQLDialect (..))
import ResultBaseline.Fixtures (Fixture (..), requiredValuesFixture)

insertRowsFixture :: SQLDialect -> String -> Int -> String -> Fixture
insertRowsFixture dialect mode size querySuffix =
    let isTyped = mode == "all" && not (null querySuffix)
        colDecl = case (dialect, isTyped) of
            (DuckDB, _) -> "x VARCHAR"
            (_, True) -> "x"
            (_, False) -> "x TEXT"
        sentinelRow = case (dialect, isTyped) of
            (_, False) -> []
            (DuckDB, True) -> ["INSERT INTO t VALUES (0, '0');"]
            (_, True) -> ["INSERT INTO t VALUES (0, 0);"]
        inserts =
            sentinelRow
                ++ [ "INSERT INTO t VALUES ("
                        ++ show i
                        ++ ", 'payload "
                        ++ replicate (6 - length (show i)) '0'
                        ++ show i
                        ++ "');"
                   | i <- [1 .. size]
                   ]
        totalStmts = length inserts + 2
        rowCount = length inserts
        sql =
            BSC.pack $
                unlines $
                    ("CREATE TABLE t (a INT, " ++ colDecl ++ ");") : inserts ++ [query]
        (query, expected, fence) = case (mode, null querySuffix) of
            ("all", True) ->
                ( "SELECT count(*) AS n FROM t;"
                , case dialect of
                    DuckDB -> "SELECT " ++ show rowCount ++ "::BIGINT AS n;"
                    _ -> "SELECT " ++ show rowCount ++ " AS n;"
                , False
                )
            ("all", False) -> case dialect of
                DuckDB ->
                    ( "SELECT count(*) AS n, sum(CASE WHEN x <> '0' THEN 1 ELSE 0 END) AS ok FROM t;"
                    , "SELECT " ++ show rowCount ++ "::BIGINT AS n, " ++ show size ++ "::HUGEINT AS ok;"
                    , True
                    )
                _ ->
                    ( "SELECT count(*) AS n, sum(typeof(x) = 'text') AS ok FROM t;"
                    , "SELECT " ++ show rowCount ++ " AS n, " ++ show size ++ " AS ok;"
                    , True
                    )
            ("half", _) ->
                let halfSum = sum [14 :: Int | i <- [1 .. size], even i]
                 in ( "SELECT count(*) AS n, sum(CASE WHEN a % 2 = 0 THEN length(x) ELSE 0 END) AS s FROM t;"
                    , case dialect of
                        DuckDB ->
                            "SELECT " ++ show size ++ "::BIGINT AS n, " ++ show halfSum ++ "::HUGEINT AS s;"
                        _ -> "SELECT " ++ show size ++ " AS n, " ++ show halfSum ++ " AS s;"
                    , True
                    )
            ("none", _) ->
                let fullSum = size * 14
                 in ( "SELECT count(*) AS n, sum(length(x)) AS s FROM t;"
                    , case dialect of
                        DuckDB ->
                            "SELECT " ++ show size ++ "::BIGINT AS n, " ++ show fullSum ++ "::HUGEINT AS s;"
                        _ -> "SELECT " ++ show size ++ " AS n, " ++ show fullSum ++ " AS s;"
                    , True
                    )
            ("first16", _) ->
                let headSum = min 16 size * 14
                 in ( "SELECT count(*) AS n, sum(CASE WHEN a <= 16 THEN length(x) ELSE 0 END) AS s FROM t;"
                    , case dialect of
                        DuckDB ->
                            "SELECT " ++ show size ++ "::BIGINT AS n, " ++ show headSum ++ "::HUGEINT AS s;"
                        _ -> "SELECT " ++ show size ++ " AS n, " ++ show headSum ++ " AS s;"
                    , True
                    )
            ("multi-col", _) ->
                let sumA = sum [1 .. size]
                 in ( "SELECT sum(a) AS s FROM t;"
                    , case dialect of
                        DuckDB -> "SELECT " ++ show sumA ++ "::HUGEINT AS s;"
                        _ -> "SELECT " ++ show sumA ++ " AS s;"
                    , True
                    )
            _ -> error ("unknown insertRowsFixture mode: " ++ mode)
        prefix = case dialect of
            DuckDB -> "duckdb-"
            _ -> "sqlite-"
     in Fixture
            (prefix ++ "insert-" ++ show size ++ "-" ++ mode ++ if null querySuffix then "" else "-typed")
            dialect
            sql
            (BSC.pack expected)
            "SELECT 0 AS n;"
            (ResultStatement totalStmts (totalStmts - 1))
            fence

wideInsertsFixture :: Int -> Fixture
wideInsertsFixture size =
    let pad = replicate 440 'x'
        inserts = ["INSERT INTO t VALUES(" ++ show i ++ "); -- " ++ pad | i <- [0 .. size - 1]]
     in Fixture
            ("sqlite-required-inserts-" ++ show size ++ "-wide")
            SQLite
            (BSC.pack (unlines ("CREATE TABLE t(x);" : inserts ++ ["SELECT count(*) AS n FROM t;"])))
            (BSC.pack ("SELECT " ++ show size ++ " AS n;"))
            "SELECT 0 AS n;"
            (ResultStatement (size + 2) (size + 1))
            False

coupledInsertFixture :: Int -> Fixture
coupledInsertFixture size =
    Fixture
        ("sqlite-insert-" ++ show size ++ "-coupled")
        SQLite
        ( BSC.pack $
            unlines $
                "CREATE TABLE t (x INT);"
                    : replicate size "INSERT INTO t VALUES (1234);"
                    ++ ["SELECT max(x) - min(x) AS diff, count(*) AS n FROM t;"]
        )
        (BSC.pack ("SELECT 0 AS diff, " ++ show size ++ " AS n;"))
        "SELECT 1 AS diff, 0 AS n;"
        (ResultStatement (size + 2) (size + 1))
        True

nonzeroInsertFixture :: SQLDialect -> Int -> Fixture
nonzeroInsertFixture dialect size =
    Fixture
        ((if dialect == DuckDB then "duckdb-" else "sqlite-") ++ "insert-" ++ show size ++ "-nonzero")
        dialect
        ( BSC.pack $
            unlines $
                "CREATE TABLE t (x INT);"
                    : "INSERT INTO t VALUES (0);"
                    : replicate size "INSERT INTO t VALUES (1000);"
                    ++ ["SELECT count(*) AS n, sum(CASE WHEN x > 0 THEN 1 ELSE 0 END) AS ok FROM t;"]
        )
        ( BSC.pack $ case dialect of
            DuckDB -> "SELECT " ++ show (size + 1) ++ "::BIGINT AS n, " ++ show size ++ "::HUGEINT AS ok;"
            _ -> "SELECT " ++ show (size + 1) ++ " AS n, " ++ show size ++ " AS ok;"
        )
        "SELECT 0 AS n, 0 AS ok;"
        (ResultStatement (size + 3) (size + 2))
        True

distributedBatchFixtures :: [Fixture]
distributedBatchFixtures =
    sqliteDistributedFixtures ++ duckdbDistributedFixtures

sqliteDistributedFixtures :: [Fixture]
sqliteDistributedFixtures =
    [ insertRowsFixture SQLite "all" 32 ""
    , insertRowsFixture SQLite "half" 32 ""
    , insertRowsFixture SQLite "none" 32 ""
    , insertRowsFixture SQLite "all" 64 ""
    , insertRowsFixture SQLite "all" 64 " WHERE typeof(x) = 'text'"
    , insertRowsFixture SQLite "first16" 64 ""
    , insertRowsFixture SQLite "multi-col" 64 ""
    , coupledInsertFixture 32
    , nonzeroInsertFixture SQLite 32
    , wideInsertsFixture 128
    ]

duckdbDistributedFixtures :: [Fixture]
duckdbDistributedFixtures =
    [ insertRowsFixture DuckDB "all" 32 ""
    , insertRowsFixture DuckDB "all" 32 " WHERE x <> '0'"
    , insertRowsFixture DuckDB "multi-col" 32 ""
    , nonzeroInsertFixture DuckDB 32
    ]

interactingLiteralFixtures :: [Fixture]
interactingLiteralFixtures =
    [ Fixture
        "sqlite-interacting-nonzero-pair"
        SQLite
        "CREATE TABLE t (a INT, b INT); INSERT INTO t VALUES (100, 200), (300, 400); SELECT count(*) AS n FROM t WHERE a > 0 AND b > 0;"
        "SELECT 2 AS n;"
        "SELECT 0 AS n;"
        (ResultStatement 3 2)
        False
    , Fixture
        "sqlite-interacting-string-quote0"
        SQLite
        "CREATE TABLE t (x TEXT); INSERT INTO t VALUES ('payload_alpha'), ('payload_beta'); SELECT count(*) AS n FROM t WHERE x <> '' AND typeof(x) = 'text';"
        "SELECT 2 AS n;"
        "SELECT 0 AS n;"
        (ResultStatement 3 2)
        True
    , Fixture
        "sqlite-interacting-unlock-clause"
        SQLite
        "CREATE TABLE t (a INT, b INT); INSERT INTO t VALUES (100, 200), (0, 200); SELECT count(*) AS n FROM t WHERE a = 1 OR a = 100;"
        "SELECT 1 AS n;"
        "SELECT 0 AS n;"
        (ResultStatement 3 2)
        False
    , Fixture
        "duckdb-interacting-nonzero"
        DuckDB
        "CREATE TABLE t (a INT, b INT); INSERT INTO t VALUES (100, 200), (300, 400); SELECT count(*) AS n FROM t WHERE a > 0 AND b > 0;"
        "SELECT 2::BIGINT AS n;"
        "SELECT 0::BIGINT AS n;"
        (ResultStatement 3 2)
        False
    ]

recursiveTerminatingFixtures :: [Fixture]
recursiveTerminatingFixtures =
    [ Fixture
        "sqlite-recursive-terminating"
        SQLite
        "CREATE TABLE edges (u INT, v INT, note TEXT); INSERT INTO edges VALUES (1, 2, 'alpha_edge'), (2, 3, 'beta_edge'); WITH RECURSIVE reach(n) AS (SELECT 1 UNION SELECT e.v FROM edges e JOIN reach r ON e.u = r.n AND e.v > 0) SELECT sum(n) AS answer FROM reach;"
        "SELECT 6 AS answer;"
        "SELECT 0 AS answer;"
        (ResultStatement 3 2)
        True
    , Fixture
        "duckdb-recursive-terminating"
        DuckDB
        "CREATE TABLE edges (u INT, v INT, note VARCHAR); INSERT INTO edges VALUES (1, 2, 'alpha_edge'), (2, 3, 'beta_edge'); WITH RECURSIVE reach(n) AS (SELECT 1 UNION SELECT e.v FROM edges e JOIN reach r ON e.u = r.n AND e.v > 0) SELECT sum(n) AS answer FROM reach;"
        "SELECT 6::HUGEINT AS answer;"
        "SELECT 0::HUGEINT AS answer;"
        (ResultStatement 3 2)
        True
    ]
deferLiteralFixtures ::
    (ByteString, ByteString, ByteString, ByteString, ByteString, ByteString) -> [Fixture]
deferLiteralFixtures (txChurn, deep, diamond, upsert, views, scale64) =
    [ Fixture
        "02_transaction_savepoint_churn"
        SQLite
        txChurn
        "SELECT 101 AS account_id, 'Acme Corp' AS owner_name, 45000 AS balance, 'premium' AS risk_tier UNION ALL SELECT 102, 'Beta LLC', 17300, 'standard';"
        "SELECT 0 AS account_id;"
        (ResultStatement 12 11)
        False
    , Fixture
        "03_deep_expressions_polymorphic"
        SQLite
        deep
        "SELECT 1 AS metric_id, 1056.7 AS calibrated_signal UNION ALL SELECT 2, -1 UNION ALL SELECT 3, 85027.5;"
        "SELECT 0 AS answer;"
        (ResultStatement 3 2)
        True
    , Fixture
        "04_multitable_diamond_dag"
        SQLite
        diamond
        "SELECT 1001 AS sink_id, 'east-cluster' AS cluster_name, 95 AS left_score, 250.5 AS right_weight, 'passed' AS verdict;"
        "SELECT 0 AS sink_id;"
        (ResultStatement 9 8)
        False
    , Fixture
        "05_upsert_conflict"
        SQLite
        upsert
        "SELECT 'SKU-001' AS sku, 'Widget Alpha' AS item_name, 80 AS quantity;"
        "SELECT 'x' AS sku;"
        (ResultStatement 4 3)
        True
    , Fixture
        "06_views_and_aggregates"
        SQLite
        views
        "SELECT 'Software' AS product_cat, 2 AS distinct_orders, 750 AS total_revenue;"
        "SELECT 0 AS answer;"
        (ResultStatement 4 3)
        True
    , Fixture
        "scale-064"
        SQLite
        scale64
        "SELECT 64 AS \"count(*)\", 2080 AS \"sum(id)\";"
        "SELECT 0 AS answer;"
        (ResultStatement 6 5)
        True
    , requiredValuesFixture 64
    , insertRowsFixture SQLite "half" 32 ""
    , insertRowsFixture SQLite "none" 32 ""
    ]
        ++ interactingLiteralFixtures

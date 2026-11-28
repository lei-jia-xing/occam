-- | Reusable result-oracle fixture cases for result and statement-search baselines.
module ResultBaseline.Fixtures (
    Fixture (..),
    fixtures,
    searchFixtures,
    columnModeCohort,
    requiredValuesFixture,
    statementFixtures,
    statementFixture,
    closureRankedCohort,
    fallbackQualificationCohort,
    fallbackNoLocCohort,
    groupProposalCohort,
    earlyRejectCohort,
)
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.List (intercalate)
import Occam.Core (ResultStatement (..))
import Occam.Parser (SQLDialect (..))

data Fixture = Fixture
    { fixtureName :: String
    , fixtureDialect :: SQLDialect
    , fixtureInput :: ByteString
    , fixtureExpected :: ByteString
    , fixtureChanged :: ByteString
    , fixtureLocation :: ResultStatement
    , fixtureFence :: Bool
    }

fixtures :: [Fixture]
fixtures =
    [ Fixture
        "sqlite-trailing-ddl"
        SQLite
        "CREATE TABLE noise(x INT); SELECT 42 AS answer; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 43 AS answer;"
        (ResultStatement 3 1)
        False
    , Fixture
        "sqlite-savepoint"
        SQLite
        "CREATE TABLE data(x INT); INSERT INTO data VALUES (42); SAVEPOINT s; UPDATE data SET x = 0; ROLLBACK TO s; RELEASE s; SELECT x AS answer FROM data; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 0 AS answer;"
        (ResultStatement 8 6)
        False
    , Fixture
        "sqlite-opaque-aggregate"
        SQLite
        "CREATE TABLE noise(x INT); CREATE TABLE data(x INT); INSERT INTO data VALUES (19), (23); SELECT sum(x) AS answer FROM data; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 42 AS different;"
        (ResultStatement 5 3)
        True
    , Fixture
        "duckdb-trailing-ddl"
        DuckDB
        "CREATE TABLE noise(x INT); SELECT [1, 2] AS earlier; SELECT 42 AS answer; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 42::BIGINT AS answer;"
        (ResultStatement 4 2)
        False
    , Fixture
        "duckdb-empty-result"
        DuckDB
        "SELECT 7 AS earlier; SELECT 42 AS answer WHERE FALSE; CREATE TABLE tail(x INT);"
        "SELECT 42 AS answer WHERE FALSE;"
        "SELECT 42 AS different WHERE FALSE;"
        (ResultStatement 3 1)
        False
    ]

searchFixtures :: [Fixture]
searchFixtures =
    [ Fixture
        "removable-values"
        SQLite
        "CREATE TABLE t(x INT); INSERT INTO t VALUES (42), (1111), (2222), (3333), (4444); SELECT x AS answer FROM t WHERE x = 42; CREATE TABLE noise(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 0 AS answer;"
        (ResultStatement 4 2)
        False
    , Fixture
        "required-values-and-independent-literals"
        SQLite
        "CREATE TABLE t(x INT, noise TEXT); INSERT INTO t VALUES (40, 'first'), (2, 'second'); SELECT sum(x) AS answer FROM t; CREATE TABLE noise(x INT);"
        "SELECT 42 AS answer;"
        "SELECT 40 AS answer;"
        (ResultStatement 4 2)
        True
    , Fixture
        "coupled-literals"
        SQLite
        "CREATE TABLE t(x INT); INSERT INTO t VALUES (1234), (1234); SELECT max(x) - min(x) AS answer, count(*) AS n FROM t; CREATE TABLE noise(x INT);"
        "SELECT 0 AS answer, 2 AS n;"
        "SELECT 1 AS answer, 2 AS n;"
        (ResultStatement 4 2)
        True
    ]
        ++ map requiredValuesFixture [8, 64]
        ++ map removableValuesFixture [8, 64]

columnModeCohort :: [Fixture]
columnModeCohort =
    [ columnFixture
        "column-explicit-joint"
        SQLite
        [ "CREATE TABLE t(a INT, b INT);"
        , "INSERT INTO t(a, b) VALUES (1, 10), (2, 20);"
        , "SELECT a FROM t ORDER BY a;"
        ]
        "SELECT 1 AS a UNION ALL SELECT 2 AS a;"
        "SELECT 0 AS a;"
    , columnFixture
        "column-implicit-joint"
        SQLite
        [ "CREATE TABLE t(a INT, b INT);"
        , "INSERT INTO t VALUES (1, 10), (2, 20);"
        , "SELECT a FROM t ORDER BY a;"
        ]
        "SELECT 1 AS a UNION ALL SELECT 2 AS a;"
        "SELECT 0 AS a;"
    , columnFixture
        "column-star"
        SQLite
        [ "CREATE TABLE t(a INT, b INT);"
        , "INSERT INTO t VALUES (1, 10), (2, 20);"
        , "SELECT * FROM t ORDER BY a;"
        ]
        "SELECT 1 AS a, 10 AS b UNION ALL SELECT 2 AS a, 20 AS b;"
        "SELECT 1 AS a, 10 AS b;"
    , columnFixture
        "column-wide-explicit"
        SQLite
        [ "CREATE TABLE t(a INT, b INT, c INT, d INT, e INT, f INT, g INT, h INT);"
        , "INSERT INTO t VALUES (1, 2, 3, 4, 5, 6, 7, 8), (2, 3, 4, 5, 6, 7, 8, 9);"
        , "SELECT a FROM t ORDER BY a;"
        ]
        "SELECT 1 AS a UNION ALL SELECT 2 AS a;"
        "SELECT 0 AS a;"
    , fencedColumnFixture
        "column-fenced-aggregate"
        SQLite
        [ "CREATE TABLE t(a INT, b INT, c INT);"
        , "INSERT INTO t VALUES (1, 10, 100), (2, 20, 200);"
        , "SELECT sum(a) AS answer FROM t;"
        ]
        "SELECT 3 AS answer;"
        "SELECT 0 AS answer;"
    , columnFixture
        "column-duckdb-explicit"
        DuckDB
        [ "CREATE TABLE t(a INTEGER, b INTEGER);"
        , "INSERT INTO t(a, b) VALUES (1, 10), (2, 20);"
        , "SELECT a FROM t ORDER BY a;"
        ]
        "SELECT 1::INTEGER AS a UNION ALL SELECT 2::INTEGER AS a;"
        "SELECT 0::INTEGER AS a;"
    ]
    where
        columnFixture name dialect input expected changed =
            Fixture
                name
                dialect
                (BSC.pack (unlines input))
                (BSC.pack expected)
                (BSC.pack changed)
                (ResultStatement 3 2)
                False
        fencedColumnFixture name dialect input expected changed =
            Fixture
                name
                dialect
                (BSC.pack (unlines input))
                (BSC.pack expected)
                (BSC.pack changed)
                (ResultStatement 3 2)
                True

requiredValuesFixture :: Int -> Fixture
requiredValuesFixture size =
    let rows = intercalate ", " ["(" ++ show i ++ ", 'reducible text')" | i <- [1 .. size]]
        expected = BSC.pack ("SELECT " ++ show (sum [1 .. size]) ++ " AS answer;")
     in Fixture
            ("required-values-" ++ show size)
            SQLite
            ( "CREATE TABLE t(x INT, noise TEXT); INSERT INTO t VALUES "
                <> BSC.pack rows
                <> "; SELECT sum(x) AS answer FROM t; CREATE TABLE noise(x INT);"
            )
            expected
            "SELECT 0 AS answer;"
            (ResultStatement 4 2)
            True

removableValuesFixture :: Int -> Fixture
removableValuesFixture size =
    let rows = intercalate ", " ("(42)" : ["(" ++ show i ++ ")" | i <- [1001 .. 999 + size]])
     in Fixture
            ("removable-values-" ++ show size)
            SQLite
            ( "CREATE TABLE t(x INT); INSERT INTO t VALUES "
                <> BSC.pack rows
                <> "; SELECT x AS answer FROM t WHERE x = 42; CREATE TABLE noise(x INT);"
            )
            "SELECT 42 AS answer;"
            "SELECT 0 AS answer;"
            (ResultStatement 4 2)
            False

statementFixtures :: [Fixture]
statementFixtures =
    [ statementFixture False fenced size
    | size <- [8, 32]
    , fenced <- [False, True]
    ]

statementFixture :: Bool -> Bool -> Int -> Fixture
statementFixture skewed fenced size =
    let inputRow i =
            "INSERT INTO t VALUES ("
                ++ show i
                ++ "); CREATE TABLE noise_"
                ++ show i
                ++ if skewed && i `mod` 8 == 0
                    then "(x TEXT DEFAULT '" ++ replicate 1024 'x' ++ "');"
                    else "(x INT);"
        query = if fenced then "SELECT sum(x) AS answer FROM t;" else "SELECT x AS answer FROM t ORDER BY x;"
        reference
            | fenced = "SELECT " ++ show (sum [1 .. size]) ++ " AS answer;"
            | otherwise =
                "WITH t(x) AS (VALUES "
                    ++ intercalate ", " ["(" ++ show i ++ ")" | i <- [1 .. size]]
                    ++ ") "
                    ++ query
     in Fixture
            ( (if skewed then "skewed-" else "")
                ++ (if fenced then "fenced-statements-" else "precise-statements-")
                ++ show size
            )
            SQLite
            (BSC.pack (unlines ("CREATE TABLE t(x INT);" : map inputRow [1 .. size] ++ [query])))
            (BSC.pack reference)
            "SELECT 0 AS answer;"
            (ResultStatement (2 * size + 2) (2 * size + 1))
            fenced

shortNoiseFixture :: Bool -> Int -> Fixture
shortNoiseFixture fenced size =
    let inputRow i =
            "INSERT INTO ledger(entry_value, note) VALUES ("
                ++ show i
                ++ ", 'required-payload-record-padding-0000'); CREATE TABLE n_"
                ++ show i
                ++ "(a);"
        query
            | fenced = "SELECT sum(entry_value) AS answer FROM ledger;"
            | otherwise = "SELECT entry_value AS answer FROM ledger ORDER BY entry_value;"
        reference
            | fenced = "SELECT " ++ show (sum [1 .. size]) ++ " AS answer;"
            | otherwise =
                "WITH ledger(entry_value) AS (VALUES "
                    ++ intercalate ", " ["(" ++ show i ++ ")" | i <- [1 .. size]]
                    ++ ") "
                    ++ query
     in Fixture
            ((if fenced then "short-noise-fenced-" else "short-noise-precise-") ++ show size)
            SQLite
            ( BSC.pack
                (unlines ("CREATE TABLE ledger(entry_value INT, note TEXT);" : map inputRow [1 .. size] ++ [query]))
            )
            (BSC.pack reference)
            "SELECT 0 AS answer;"
            (ResultStatement (2 * size + 2) (2 * size + 1))
            fenced

txNoopBlocksFixture :: Bool -> Int -> Fixture
txNoopBlocksFixture fenced size =
    let block _ =
            [ "BEGIN;"
            , "UPDATE t SET x = x + 1;"
            , "UPDATE t SET x = x - 1;"
            , "COMMIT;"
            ]
        query = if fenced then "SELECT sum(x) AS answer FROM t;" else "SELECT x AS answer FROM t;"
        stmts =
            ["CREATE TABLE t(x INT);", "INSERT INTO t VALUES (42);"]
                ++ concatMap block [1 .. size]
                ++ [query]
        total = 4 * size + 3
     in Fixture
            ((if fenced then "tx-noop-blocks-fenced-" else "tx-noop-blocks-precise-") ++ show size)
            SQLite
            (BSC.pack (unlines stmts))
            "SELECT 42 AS answer;"
            "SELECT 0 AS answer;"
            (ResultStatement total (total - 1))
            fenced

txNoopUpdatesFixture :: Bool -> Int -> Fixture
txNoopUpdatesFixture fenced size =
    let pair _ =
            [ "UPDATE t SET x = x + 1;"
            , "UPDATE t SET x = x - 1;"
            ]
        query = if fenced then "SELECT sum(x) AS answer FROM t;" else "SELECT x AS answer FROM t;"
        stmts =
            ["CREATE TABLE t(x INT);", "INSERT INTO t VALUES (42);"]
                ++ concatMap pair [1 .. size]
                ++ [query]
        total = 2 * size + 3
     in Fixture
            ((if fenced then "tx-noop-updates-fenced-" else "tx-noop-updates-precise-") ++ show size)
            SQLite
            (BSC.pack (unlines stmts))
            "SELECT 42 AS answer;"
            "SELECT 0 AS answer;"
            (ResultStatement total (total - 1))
            fenced

txSavepointMixFixture :: Bool -> Int -> Fixture
txSavepointMixFixture fenced size =
    let block i =
            [ "SAVEPOINT s_" ++ show i ++ ";"
            , "UPDATE t SET x = 0;"
            , "ROLLBACK TO s_" ++ show i ++ ";"
            , "RELEASE s_" ++ show i ++ ";"
            , "BEGIN;"
            , "UPDATE t SET x = x + 1;"
            , "UPDATE t SET x = x - 1;"
            , "COMMIT;"
            , "CREATE TABLE noise_" ++ show i ++ "(a INT);"
            ]
        query = if fenced then "SELECT sum(x) AS answer FROM t;" else "SELECT x AS answer FROM t;"
        stmts =
            ["CREATE TABLE t(x INT);", "INSERT INTO t VALUES (42);"]
                ++ concatMap block [1 .. size]
                ++ [query]
        total = 9 * size + 3
     in Fixture
            ((if fenced then "tx-savepoint-mix-fenced-" else "tx-savepoint-mix-precise-") ++ show size)
            SQLite
            (BSC.pack (unlines stmts))
            "SELECT 42 AS answer;"
            "SELECT 0 AS answer;"
            (ResultStatement total (total - 1))
            fenced

subsetOnlyReturningFixture :: Fixture
subsetOnlyReturningFixture =
    let insertA :: Int -> String
        insertA i = "INSERT INTO a(x) VALUES (" ++ show i ++ ") RETURNING 0 AS answer;"
        stmts =
            ["CREATE TABLE a(x INT, pad TEXT DEFAULT 'aaaaaaaaaaaaaaaaaaaa');"]
                ++ map insertA [1 .. 3]
                ++ [ "CREATE TABLE k(x INT, pad TEXT DEFAULT 'kkkkkkkkkkkk');"
                   , "INSERT INTO k(x) VALUES (42) RETURNING x AS answer;"
                   ]
                ++ map insertA [4 .. 12]
                ++ ["SELECT sum(a.x) + (SELECT x FROM k) - 78 AS answer FROM a;"]
     in Fixture
            "subset-only-returning-16"
            SQLite
            (BSC.pack (unlines stmts))
            "SELECT 42 AS answer;"
            "SELECT 0 AS answer;"
            (ResultStatement 16 15)
            True

scatteredTablesFixture :: Bool -> Int -> Fixture
scatteredTablesFixture fenced size =
    let phase1 i =
            ["CREATE TABLE noise_" ++ show i ++ "(k INT, v TEXT);", "INSERT INTO t VALUES (" ++ show i ++ ");"]
        phase2 i = ["INSERT INTO noise_" ++ show i ++ " VALUES (" ++ show i ++ ", 'row');"]
        phase3 i = ["CREATE INDEX idx_noise_" ++ show i ++ " ON noise_" ++ show i ++ "(k);"]
        phase4 i = ["UPDATE noise_" ++ show i ++ " SET v = 'updated';"]
        query = if fenced then "SELECT sum(x) AS answer FROM t;" else "SELECT x AS answer FROM t ORDER BY x;"
        reference
            | fenced = "SELECT " ++ show (sum [1 .. size]) ++ " AS answer;"
            | otherwise =
                "WITH t(x) AS (VALUES "
                    ++ intercalate ", " ["(" ++ show i ++ ")" | i <- [1 .. size]]
                    ++ ") "
                    ++ query
        stmts =
            ["CREATE TABLE t(x INT);"]
                ++ concatMap phase1 [1 .. size]
                ++ concatMap phase2 [1 .. size]
                ++ concatMap phase3 [1 .. size]
                ++ concatMap phase4 [1 .. size]
                ++ [query]
        total = 5 * size + 2
     in Fixture
            ((if fenced then "scattered-tables-fenced-" else "scattered-tables-precise-") ++ show size)
            SQLite
            (BSC.pack (unlines stmts))
            (BSC.pack reference)
            "SELECT 0 AS answer;"
            (ResultStatement total (total - 1))
            fenced

scatteredDeadReadFixture :: Int -> Fixture
scatteredDeadReadFixture size =
    let creates i = ["CREATE TABLE shadow_" ++ show i ++ "(x INT);", "INSERT INTO t VALUES (" ++ show i ++ ");"]
        copies i = ["INSERT INTO shadow_" ++ show i ++ " SELECT x FROM t;"]
        indexes i = ["CREATE INDEX idx_shadow_" ++ show i ++ " ON shadow_" ++ show i ++ "(x);"]
        stmts =
            ["CREATE TABLE t(x INT);"]
                ++ concatMap creates [1 .. size]
                ++ concatMap copies [1 .. size]
                ++ concatMap indexes [1 .. size]
                ++ ["SELECT sum(x) AS answer FROM t;"]
        total = 4 * size + 2
     in Fixture
            ("scattered-dead-read-" ++ show size)
            SQLite
            (BSC.pack (unlines stmts))
            (BSC.pack ("SELECT " ++ show (sum [1 .. size]) ++ " AS answer;"))
            "SELECT 0 AS answer;"
            (ResultStatement total (total - 1))
            True

noncontiguousTxWrapperFkFixture :: Int -> Fixture
noncontiguousTxWrapperFkFixture wrappers =
    let block i =
            [ "BEGIN;"
            , "INSERT INTO p VALUES (" ++ show (2 * i - 1) ++ "), (" ++ show (2 * i) ++ ");"
            , "INSERT INTO c VALUES (" ++ show (2 * i - 1) ++ "), (" ++ show (2 * i) ++ ");"
            , "COMMIT;"
            ]
        stmts =
            [ "CREATE TABLE p(id INT PRIMARY KEY);"
            , "CREATE TABLE c(pid INT REFERENCES p(id) ON DELETE CASCADE);"
            ]
                ++ concatMap block [1 .. wrappers]
                ++ [ "PRAGMA foreign_keys = ON;"
                   , "DELETE FROM p WHERE id = 1;"
                   , "SELECT count(*) AS answer FROM c;"
                   , "CREATE TABLE tail(x INT);"
                   ]
        total = 4 * wrappers + 6
        expectedCount = 2 * wrappers - 1
     in Fixture
            ("noncontiguous-tx-wrapper-fk-" ++ show wrappers)
            SQLite
            (BSC.pack (unlines stmts))
            (BSC.pack ("SELECT " ++ show expectedCount ++ " AS answer;"))
            "SELECT 0 AS answer;"
            (ResultStatement total (total - 2))
            False

requiredTxBlocksFixture :: Int -> Fixture
requiredTxBlocksFixture size =
    let block i =
            [ "BEGIN;"
            , "INSERT INTO t VALUES (" ++ show i ++ ");"
            , "COMMIT;"
            ]
        stmts =
            ["CREATE TABLE t(x INT);"]
                ++ concatMap block [1 .. size]
                ++ ["SELECT sum(x) AS answer FROM t;"]
        total = 3 * size + 2
     in Fixture
            ("required-tx-blocks-fenced-" ++ show size)
            SQLite
            (BSC.pack (unlines stmts))
            (BSC.pack ("SELECT " ++ show (sum [1 .. size]) ++ " AS answer;"))
            "SELECT 0 AS answer;"
            (ResultStatement total (total - 1))
            True

closureRankedCohort :: [Fixture]
closureRankedCohort =
    statementFixtures
        ++ [statementFixture False False 128, statementFixture True False 32, statementFixture True True 32]
        ++ fixtures

fallbackQualificationCohort :: [Fixture]
fallbackQualificationCohort =
    statementFixtures
        ++ [ statementFixture False False 128
           , statementFixture False True 128
           , statementFixture True True 32
           , shortNoiseFixture True 32
           , txNoopUpdatesFixture False 16
           , txNoopUpdatesFixture True 16
           , txNoopBlocksFixture False 16
           , txNoopBlocksFixture False 32
           , txNoopBlocksFixture True 16
           , txSavepointMixFixture False 12
           , txSavepointMixFixture True 12
           , subsetOnlyReturningFixture
           ]
        ++ fixtures

fallbackNoLocCohort :: [Fixture]
fallbackNoLocCohort =
    [ statementFixture False False 32
    , statementFixture False True 32
    , statementFixture False True 128
    , statementFixture True True 32
    , shortNoiseFixture True 32
    , txNoopUpdatesFixture False 16
    , txNoopUpdatesFixture True 16
    , txNoopBlocksFixture False 16
    , txNoopBlocksFixture True 16
    , txSavepointMixFixture True 12
    , subsetOnlyReturningFixture
    ]
        ++ fixtures
groupProposalCohort :: [Fixture]
groupProposalCohort =
    [ statementFixture False False 32
    , statementFixture False True 32
    , statementFixture False True 128
    , skewedFenced32
    , scatteredTablesFixture False 16
    , scatteredTablesFixture True 16
    , scatteredDeadReadFixture 8
    , noncontiguousTxWrapperFkFixture 1
    , noncontiguousTxWrapperFkFixture 4
    , txNoopBlocksFixture False 16
    , txNoopBlocksFixture True 16
    , txSavepointMixFixture True 12
    , requiredTxBlocksFixture 16
    , subsetOnlyReturningFixture
    ]
        ++ fixtures
    where
        skewedFenced32 = statementFixture True True 32

duckdbFencedFixture :: Int -> Fixture
duckdbFencedFixture size =
    let inputRow i =
            "INSERT INTO t VALUES ("
                ++ show i
                ++ "); CREATE TABLE noise_"
                ++ show i
                ++ "(x INT);"
        query = "SELECT sum(x) AS answer FROM t;"
        reference = "SELECT " ++ show (sum [1 .. size]) ++ "::HUGEINT AS answer;"
     in Fixture
            ("duckdb-fenced-statements-" ++ show size)
            DuckDB
            (BSC.pack (unlines ("CREATE TABLE t(x INT);" : map inputRow [1 .. size] ++ [query])))
            (BSC.pack reference)
            "SELECT 0::HUGEINT AS answer;"
            (ResultStatement (2 * size + 2) (2 * size + 1))
            True

earlyRejectCohort :: [Fixture]
earlyRejectCohort =
    [ statementFixture False False 32
    , statementFixture False True 32
    , statementFixture False True 128
    , statementFixture True True 32
    , scatteredTablesFixture False 16
    , scatteredTablesFixture True 16
    , txNoopBlocksFixture True 16
    , txSavepointMixFixture True 12
    , noncontiguousTxWrapperFkFixture 1
    , noncontiguousTxWrapperFkFixture 4
    , subsetOnlyReturningFixture
    , duckdbFencedFixture 16
    ]
        ++ fixtures

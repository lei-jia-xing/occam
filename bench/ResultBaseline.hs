module ResultBaseline (
    runResultBaseline,
    runSearchBaseline,
    runLiteralBatchBaseline,
    runValuesScheduleBaseline,
    runCombinedSyntaxBaseline,
    runStatementYieldBaseline,
    runStatementOrderBaseline,
    runStatementWeightBaseline,
    runStatementRankedBaseline,
    runStatementRankedGraphFreeBaseline,
    runStatementRankBaseline,
    runStatementInterleaveBaseline,
    runStatementRankedResumeBaseline,
    runStatementRankedResumeTiesBaseline,
    runStatementProjectedRankBaseline,
    runStatementProjectionScoreBaseline,
    runStatementWindowPrefetchBaseline,
    runStatementWindowWithinLaneBaseline,
    runStatementWindowCrossVsLaneBaseline,
    runStatementWindowSize4Baseline,
    runStatementWindowSize16Baseline,
    runStatementBoundedPrefClosureBaseline,
    runStatementResumeMonoBaseline,
    runStatementLanePrefBaseline,
    runStatementLanePrefQuota2Baseline,
    runStatementLanePrefQuota8Baseline,
    runStatementFallbackBaseline,
    runStatementFallbackNoLocBaseline,
    runGroupProposalsBaseline,
    runGroupProposalsNoLocBaseline,
    runEarlyRejectBaseline,
    runReparseBaseline,
    runWorkerBaseline,
    runDistributedLiteralBatchBaseline,
    runDistributedRefreshBaseline,
    runDistributedMisses1Vs2Baseline,
    runDistributedMisses2Vs3Baseline,
    runDistributedMin16Vs32Baseline,
    runDistributedCap8Vs16Baseline,
    runDistributedCap16Vs32Baseline,
    runDeferLiteralVariantsBaseline,
    runDeferLiteralVariants01Baseline,
    runDeferRecursiveBaseline,
) where

import Control.Monad (forM, forM_, mapAndUnzipM, unless, when, (>=>))
import Data.Aeson (Value (Bool, Number), encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (decodeUtf8)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Occam.AtomicFile (writeAtomicLazyBytes)
import Occam.Core (OracleOutcome (OracleRejected), ResultStatement (..), traceResultStatement)
import Occam.Dependency (analyzeStatements, graphHasGlobalBarrier)
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationProgress (..),
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    ReductionActionResult (..),
    metricPolicyForReduction,
    runEvaluationRequestWithNativeValidation,
    sqlDialectName,
 )
import Occam.Logger (Logger (..), defaultLogConfig)
import Occam.Oracle (
    OracleObservationDetails (..),
    observationOutcome,
    oracleObservationMatches,
    withCandidateWorkdir,
 )
import Occam.Oracle.Inline (newResultTraceTracker, prepareResultOracleWithValidation)
import Occam.Parser (
    ParsedSQL (..),
    SQLDialect (..),
    countStatements,
    defaultParseOptions,
    parseSQL,
 )
import Occam.Reducer (
    DistributedBatchConfig (..),
    LaneOrdering (..),
    LaneScheduling (..),
    LiteralDeferralMode (..),
    ProjectionWindow (..),
    ProjectionWindowOrder (..),
    RankedScheduleConfig (..),
    ReductionConfig (..),
    ReductionResult (..),
    SingletonResume (..),
    StatementSearchPolicy (..),
    defaultDistributedBatchConfig,
    defaultRankedScheduleConfig,
    defaultReductionConfig,
    reduceSQLWithResultTrace,
    statementRanked,
 )
import System.Exit (die)
import System.FilePath ((</>))
import System.Info qualified as System

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

data Arm = Arm
    { armName :: String
    , armConfig :: ReductionConfig -> ReductionConfig
    , armObservedResult :: Bool
    , armJobs :: Int
    }

serialArm :: String -> (ReductionConfig -> ReductionConfig) -> Bool -> Arm
serialArm name configure observed = Arm name configure observed 1

probeTimeout :: Int
probeTimeout = 5000000

require :: (Show err) => String -> Either err a -> IO a
require label = either (die . ((label ++ ": ") ++) . show) pure

check :: String -> Bool -> IO ()
check message condition = unless condition (die message)

runResultBaseline :: Int -> FilePath -> IO ()
runResultBaseline pairs out =
    runComparison
        pairs
        out
        (Just 60000000)
        (Just 256)
        []
        (serialArm "last-statement" id False, serialArm "observed-result" id True)
        fixtures

runSearchBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runSearchBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "statement-only" id True
        , serialArm "intra-statement" (\config -> config {reductionIntraStatement = True}) True
        )
        (searchFixtures ++ fixtures)

runLiteralBatchBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runLiteralBatchBaseline =
    runSyntaxPolicyBaseline
        []
        "literal-singles"
        "literal-batches"
        (\config -> config {reductionLiteralBatches = True})

runValuesScheduleBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runValuesScheduleBaseline =
    runSyntaxPolicyBaseline
        []
        "values-coarse-to-fine"
        "values-defer-fine"
        (\config -> config {reductionAdaptiveValues = True})

runCombinedSyntaxBaseline ::
    (ByteString, ByteString, ByteString) -> Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runCombinedSyntaxBaseline (scale, deep, views) =
    runSyntaxPolicyBaseline
        [ Fixture
            "scale-064"
            SQLite
            scale
            "SELECT 64 AS \"count(*)\", 2080 AS \"sum(id)\";"
            "SELECT 0 AS answer;"
            (ResultStatement 6 5)
            True
        , Fixture
            "deep-expressions"
            SQLite
            deep
            "SELECT 1 AS metric_id, 1056.7 AS calibrated_signal UNION ALL SELECT 2, -1 UNION ALL SELECT 3, 85027.5;"
            "SELECT 0 AS answer;"
            (ResultStatement 3 2)
            True
        , Fixture
            "views-and-aggregates"
            SQLite
            views
            "SELECT 'Software' AS product_cat, 2 AS distinct_orders, 750 AS total_revenue;"
            "SELECT 0 AS answer;"
            (ResultStatement 4 3)
            True
        ]
        "syntax-current"
        "syntax-batches-and-deferred-values"
        (\config -> config {reductionLiteralBatches = True, reductionAdaptiveValues = True})

runStatementYieldBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementYieldBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "graph-free-ddmin"
            (\config -> config {reductionSlicing = False, reductionClosure = False})
            False
        , serialArm "current-statement-search" id True
        )
        statementFixtures

runStatementOrderBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementOrderBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "statement-subset-first"
            (\config -> config {reductionStatementSearch = StatementSubsetFirst})
            True
        , serialArm
            "statement-complement-first"
            (\config -> config {reductionStatementSearch = StatementComplementFirst})
            True
        )
        (statementFixtures ++ searchFixtures ++ fixtures)

runStatementWeightBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementWeightBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "statement-equal-count"
            (\config -> config {reductionStatementSearch = StatementComplementFirst})
            True
        , serialArm
            "statement-equal-bytes"
            (\config -> config {reductionStatementSearch = StatementByteWeighted})
            True
        )
        (statementFixtures ++ [statementFixture True fenced 32 | fenced <- [False, True]])

runStatementRankedBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementRankedBaseline =
    runRankedComparison
        id
        ( statementFixtures
            ++ [statementFixture False True 128, statementFixture True True 32]
            ++ searchFixtures
            ++ fixtures
        )

runStatementRankedGraphFreeBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementRankedGraphFreeBaseline =
    runRankedComparison
        (\config -> config {reductionSlicing = False, reductionClosure = False})
        (statementFixtures ++ [statementFixture False True 128, statementFixture True True 32] ++ fixtures)

runRankedComparison ::
    (ReductionConfig -> ReductionConfig) ->
    [Fixture] ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runRankedComparison configure cases pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "statement-byte-weighted"
            (\config -> (configure config) {reductionStatementSearch = StatementByteWeighted})
            True
        , serialArm
            "statement-ranked"
            (\config -> (configure config) {reductionStatementSearch = statementRanked})
            True
        )
        cases

rankedPolicy :: (RankedScheduleConfig -> RankedScheduleConfig) -> StatementSearchPolicy
rankedPolicy f = StatementRankedWith (f defaultRankedScheduleConfig)

boundedPrefConfig :: Int -> RankedScheduleConfig
boundedPrefConfig quota =
    defaultRankedScheduleConfig
        { rankedLaneScheduling = LaneBoundedPreference quota
        , rankedSingletonResume = ResumeWeightMonotonic
        , rankedFallbackMinStatements = 8
        , rankedFallbackMaxShrinkPct = 10
        }

projectionCohort :: [Fixture]
projectionCohort =
    statementFixtures
        ++ [statementFixture False False 128, statementFixture True False 32, statementFixture True True 32]
        ++ fixtures

lanePreferenceCohort :: [Fixture]
lanePreferenceCohort =
    statementFixtures
        ++ [ statementFixture False True 128
           , statementFixture True True 32
           , shortNoiseFixture True 32
           , shortNoiseFixture True 64
           , txNoopBlocksFixture False 16
           , txSavepointMixFixture False 12
           , subsetOnlyReturningFixture
           ]
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

runStatementRankBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementRankBaseline =
    runRankedAblation (rankedPolicy (\c -> c {rankedLaneOrdering = LaneOrderPartition}))

runStatementInterleaveBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementInterleaveBaseline =
    runRankedAblation (rankedPolicy (\c -> c {rankedLaneScheduling = LaneSerial}))

runStatementRankedResumeBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementRankedResumeBaseline =
    runRankedAblation (rankedPolicy (\c -> c {rankedSingletonResume = ResumeCursorOnly}))

runStatementRankedResumeTiesBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementRankedResumeTiesBaseline =
    runRankedAblation (rankedPolicy (\c -> c {rankedSingletonResume = ResumeWeightStableTies}))

runStatementProjectedRankBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementProjectedRankBaseline =
    runProjectionPair
        "estimated-bytes"
        statementRanked
        "projected-bytes"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderCrossProjected}))

runStatementProjectionScoreBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementProjectionScoreBaseline =
    runProjectionPair
        "window-estimated-bytes"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderCrossEstimated}))
        "projected-bytes"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderCrossProjected}))

runStatementWindowPrefetchBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementWindowPrefetchBaseline =
    runProjectionPair
        "unwindowed"
        statementRanked
        "window-8-preserve"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderPreserve}))

runStatementWindowWithinLaneBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementWindowWithinLaneBaseline =
    runProjectionPair
        "window-8-preserve"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderPreserve}))
        "window-8-within-lane-projected"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderWithinLaneProjected}))

runStatementWindowCrossVsLaneBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementWindowCrossVsLaneBaseline =
    runProjectionPair
        "window-8-cross-projected"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderCrossProjected}))
        "window-8-within-lane-projected"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderWithinLaneProjected}))

runStatementWindowSize4Baseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementWindowSize4Baseline =
    runProjectionPair
        "window-4-within-lane-projected"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 4 WindowOrderWithinLaneProjected}))
        "window-8-within-lane-projected"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderWithinLaneProjected}))

runStatementWindowSize16Baseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementWindowSize16Baseline =
    runProjectionPair
        "window-16-within-lane-projected"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 16 WindowOrderWithinLaneProjected}))
        "window-8-within-lane-projected"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderWithinLaneProjected}))

runStatementBoundedPrefClosureBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementBoundedPrefClosureBaseline =
    runProjectionPair
        "window-8-cross-projected"
        (rankedPolicy (\c -> c {rankedProjectionWindow = ProjectionWindow 8 WindowOrderCrossProjected}))
        "bounded-lane-pref-4"
        (StatementRankedWith (boundedPrefConfig 4))

runStatementResumeMonoBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementResumeMonoBaseline =
    runGraphFreePair
        "resume-stable-ties"
        (rankedPolicy (\c -> c {rankedSingletonResume = ResumeWeightStableTies}))
        "resume-weight-monotonic"
        (rankedPolicy (\c -> c {rankedSingletonResume = ResumeWeightMonotonic}))
        lanePreferenceCohort

runStatementLanePrefBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementLanePrefBaseline =
    runGraphFreePair
        "fixed-resume-ties"
        (rankedPolicy (\c -> c {rankedSingletonResume = ResumeWeightStableTies}))
        "bounded-lane-pref-4"
        (StatementRankedWith (boundedPrefConfig 4))
        lanePreferenceCohort

runStatementLanePrefQuota2Baseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementLanePrefQuota2Baseline =
    runGraphFreePair
        "bounded-lane-pref-2"
        (StatementRankedWith (boundedPrefConfig 2))
        "bounded-lane-pref-4"
        (StatementRankedWith (boundedPrefConfig 4))
        lanePreferenceCohort

runStatementLanePrefQuota8Baseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementLanePrefQuota8Baseline =
    runGraphFreePair
        "bounded-lane-pref-8"
        (StatementRankedWith (boundedPrefConfig 8))
        "bounded-lane-pref-4"
        (StatementRankedWith (boundedPrefConfig 4))
        lanePreferenceCohort

runStatementFallbackBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementFallbackBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "statement-byte-weighted"
            ( \config -> config {reductionGroupProposals = False, reductionStatementSearch = StatementByteWeighted}
            )
            True
        , serialArm
            "statement-ranked-fallback"
            ( \config ->
                config
                    { reductionGroupProposals = False
                    , reductionStatementSearch = StatementRankedFallback (boundedPrefConfig 4)
                    }
            )
            True
        )
        fallbackQualificationCohort

runStatementFallbackNoLocBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementFallbackNoLocBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "statement-byte-weighted-noloc"
            ( \config -> config {reductionGroupProposals = False, reductionStatementSearch = StatementByteWeighted}
            )
            False
        , serialArm
            "statement-ranked-fallback-noloc"
            ( \config ->
                config
                    { reductionGroupProposals = False
                    , reductionStatementSearch = StatementRankedFallback (boundedPrefConfig 4)
                    }
            )
            False
        )
        fallbackNoLocCohort

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

runGroupProposalsBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runGroupProposalsBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "without-group-proposals"
            (\config -> config {reductionGroupProposals = False})
            True
        , serialArm
            "with-group-proposals"
            (\config -> config {reductionGroupProposals = True})
            True
        )
        groupProposalCohort

runGroupProposalsNoLocBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runGroupProposalsNoLocBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "without-group-proposals-noloc"
            (\config -> config {reductionGroupProposals = False})
            False
        , serialArm
            "with-group-proposals-noloc"
            (\config -> config {reductionGroupProposals = True})
            False
        )
        groupProposalCohort

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

runEarlyRejectBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runEarlyRejectBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "without-early-reject"
            (\config -> config {reductionEarlyReject = False})
            True
        , serialArm
            "with-early-reject"
            (\config -> config {reductionEarlyReject = True})
            True
        )
        earlyRejectCohort

runProjectionPair ::
    String ->
    StatementSearchPolicy ->
    String ->
    StatementSearchPolicy ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runProjectionPair controlName control treatmentName treatment pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        (arm controlName control, arm treatmentName treatment)
        projectionCohort
    where
        arm name policy =
            serialArm
                name
                (\config -> config {reductionSlicing = False, reductionStatementSearch = policy})
                True

runGraphFreePair ::
    String ->
    StatementSearchPolicy ->
    String ->
    StatementSearchPolicy ->
    [Fixture] ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runGraphFreePair controlName control treatmentName treatment cases pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        (arm controlName control, arm treatmentName treatment)
        cases
    where
        arm name policy =
            serialArm
                name
                ( \config ->
                    config
                        { reductionSlicing = False
                        , reductionClosure = False
                        , reductionStatementSearch = policy
                        }
                )
                True

runRankedAblation :: StatementSearchPolicy -> Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runRankedAblation control =
    runGraphFreePair
        "control"
        control
        "ranked-interleaved"
        statementRanked
        (statementFixtures ++ [statementFixture False True 128, statementFixture True True 32] ++ fixtures)

optimizedSyntax :: ReductionConfig -> ReductionConfig
optimizedSyntax config =
    config
        { reductionIntraStatement = True
        , reductionAdaptiveValues = True
        , reductionLiteralBatches = True
        }

runReparseBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runReparseBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "incremental-syntax-reparse" optimizedSyntax True
        , serialArm
            "full-syntax-reparse"
            (\config -> (optimizedSyntax config) {reductionFullSyntaxReparse = True})
            True
        )
        (searchFixtures ++ fixtures)

runWorkerBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runWorkerBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "one-oracle-worker" optimizedSyntax True
        , Arm "four-oracle-workers" optimizedSyntax True 4
        )
        (searchFixtures ++ filter ((== SQLite) . fixtureDialect) fixtures)

searchCensoredStatuses :: Maybe Int -> Maybe Int -> [EvaluationStatus]
searchCensoredStatuses wallLimit callLimit =
    [EvaluationTimeout | Just _ <- [wallLimit]] ++ [EvaluationExecutionBudget | Just _ <- [callLimit]]

runSyntaxPolicyBaseline ::
    [Fixture] ->
    String ->
    String ->
    (ReductionConfig -> ReductionConfig) ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runSyntaxPolicyBaseline extraFixtures control treatment policy pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        (serialArm control intra True, serialArm treatment (policy . intra) True)
        (searchFixtures ++ fixtures ++ extraFixtures)
    where
        intra config = config {reductionIntraStatement = True}

plainSyntax :: ReductionConfig -> ReductionConfig
plainSyntax config =
    config
        { reductionIntraStatement = True
        , reductionLiteralBatches = False
        , reductionDistributedLiteralBatches = False
        , reductionDeferLiteralVariants = DeferLiteralsOff
        , reductionDeferRecursivePredicates = False
        , reductionAstBatching = False
        , reductionAdaptiveValues = False
        }

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

runDistributedLiteralBatchBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runDistributedLiteralBatchBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "distributed-off" plainSyntax True
        , serialArm
            "distributed-on"
            (\c -> (plainSyntax c) {reductionDistributedLiteralBatches = True})
            True
        )
        distributedBatchFixtures

runDistributedRefreshBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runDistributedRefreshBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm
            "distributed-full-reparse"
            (\c -> (plainSyntax c) {reductionDistributedLiteralBatches = True, reductionFullSyntaxReparse = True})
            True
        , serialArm
            "distributed-incremental-refresh"
            ( \c -> (plainSyntax c) {reductionDistributedLiteralBatches = True, reductionFullSyntaxReparse = False}
            )
            True
        )
        [ wideInsertsFixture 512
        , insertRowsFixture SQLite "all" 512 ""
        , insertRowsFixture SQLite "all" 64 " WHERE typeof(x) = 'text'"
        , insertRowsFixture SQLite "multi-col" 64 ""
        , insertRowsFixture DuckDB "all" 32 ""
        , insertRowsFixture DuckDB "all" 32 " WHERE x <> '0'"
        ]

runDistributedConfigComparison ::
    String ->
    DistributedBatchConfig ->
    String ->
    DistributedBatchConfig ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runDistributedConfigComparison nameA cfgA nameB cfgB pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm nameA (mk cfgA) True
        , serialArm nameB (mk cfgB) True
        )
        sqliteDistributedFixtures
    where
        mk cfg c =
            (plainSyntax c)
                { reductionDistributedLiteralBatches = True
                , reductionDistributedBatchConfig = cfg
                }

runDistributedMisses1Vs2Baseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runDistributedMisses1Vs2Baseline =
    runDistributedConfigComparison
        "dist-misses-1"
        defaultDistributedBatchConfig {distributedMaxMisses = 1}
        "dist-misses-2"
        defaultDistributedBatchConfig {distributedMaxMisses = 2}

runDistributedMisses2Vs3Baseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runDistributedMisses2Vs3Baseline =
    runDistributedConfigComparison
        "dist-misses-2"
        defaultDistributedBatchConfig {distributedMaxMisses = 2}
        "dist-misses-3"
        defaultDistributedBatchConfig {distributedMaxMisses = 3}

runDistributedMin16Vs32Baseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runDistributedMin16Vs32Baseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "dist-min-16" (mk defaultDistributedBatchConfig {distributedMinStatements = 16}) True
        , serialArm "dist-min-32" (mk defaultDistributedBatchConfig {distributedMinStatements = 32}) True
        )
        ( [ insertRowsFixture SQLite "all" 16 ""
          , insertRowsFixture SQLite "half" 16 ""
          , insertRowsFixture SQLite "none" 16 ""
          , insertRowsFixture SQLite "multi-col" 16 ""
          ]
            ++ sqliteDistributedFixtures
        )
    where
        mk cfg c =
            (plainSyntax c)
                { reductionDistributedLiteralBatches = True
                , reductionDistributedBatchConfig = cfg
                }

runDistributedCap8Vs16Baseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runDistributedCap8Vs16Baseline =
    runDistributedConfigComparison
        "dist-cap-8"
        defaultDistributedBatchConfig {distributedMaxBatchSize = 8}
        "dist-cap-16"
        defaultDistributedBatchConfig {distributedMaxBatchSize = 16}

runDistributedCap16Vs32Baseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runDistributedCap16Vs32Baseline =
    runDistributedConfigComparison
        "dist-cap-16"
        defaultDistributedBatchConfig {distributedMaxBatchSize = 16}
        "dist-cap-32"
        defaultDistributedBatchConfig {distributedMaxBatchSize = 32}

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

runDeferLiteralVariantsBaseline ::
    (ByteString, ByteString, ByteString, ByteString, ByteString, ByteString) ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runDeferLiteralVariantsBaseline corpus pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "defer-literals-off" plainSyntax True
        , serialArm
            "defer-literals-primary-one"
            (\c -> (plainSyntax c) {reductionDeferLiteralVariants = DeferLiteralsPrimaryOne})
            True
        )
        (deferLiteralFixtures corpus)

runDeferLiteralVariants01Baseline ::
    (ByteString, ByteString, ByteString, ByteString, ByteString, ByteString) ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runDeferLiteralVariants01Baseline corpus pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "defer-literals-off" plainSyntax True
        , serialArm
            "defer-literals-zero-one"
            (\c -> (plainSyntax c) {reductionDeferLiteralVariants = DeferLiteralsZeroOne})
            True
        )
        (deferLiteralFixtures corpus)

runDeferRecursiveBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runDeferRecursiveBaseline pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        ( serialArm "defer-recursive-off" plainSyntax True
        , serialArm
            "defer-recursive-on"
            (\c -> (plainSyntax c) {reductionDeferRecursivePredicates = True})
            True
        )
        recursiveTerminatingFixtures

runComparison ::
    Int -> FilePath -> Maybe Int -> Maybe Int -> [EvaluationStatus] -> (Arm, Arm) -> [Fixture] -> IO ()
runComparison pairs out wallLimit callLimit censoredStatuses arms casesToRun = do
    capabilities <- getNumCapabilities
    (cases, failures) <-
        mapAndUnzipM (runFixture pairs wallLimit callLimit censoredStatuses arms) casesToRun
    writeAtomicLazyBytes out $
        encode $
            object
                [ "schema_version" .= (1 :: Int)
                , "pairs_per_fixture" .= pairs
                , "capabilities" .= capabilities
                , "os" .= System.os
                , "arch" .= System.arch
                , "oracle_timeout_micros" .= probeTimeout
                , "failures" .= concat failures
                , "cases" .= cases
                ]

    forM_ (listToMaybe (concat failures)) $ \failure ->
        die (failure ++ " (samples written to " ++ out ++ ")")

runFixture ::
    Int -> Maybe Int -> Maybe Int -> [EvaluationStatus] -> (Arm, Arm) -> Fixture -> IO (Value, [String])
runFixture pairs wallLimit callLimit censoredStatuses (control, treatment) Fixture {..} = do
    let config = defaultReductionConfig {reductionDialect = fixtureDialect}
        prepare remember sql =
            prepareResultOracleWithValidation True Nothing remember fixtureDialect probeTimeout sql
                >>= require fixtureName
        assert = check . ((fixtureName ++ ": ") ++)
    parsed <- parseSQL (defaultParseOptions fixtureDialect) fixtureInput >>= require fixtureName
    let initialCount = length (parsedUnits parsed)
        graph = analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed)
    assert "unexpected global fence classification" (graphHasGlobalBarrier graph == fixtureFence)
    locationRef <- newIORef Nothing
    (_, inputObserver, _) <-
        prepare (\_ trace -> writeIORef locationRef (Just (traceResultStatement trace))) fixtureInput
    location <- readIORef locationRef
    assert "unexpected engine result location" (location == Just fixtureLocation)
    assert
        "engine/frontend statement count mismatch"
        (resultStatementCount fixtureLocation == initialCount)
    (_, _, expectedObserver) <- prepare (\_ _ -> pure ()) fixtureExpected
    let observation observe sql = detailedObservation <$> observe (pure ()) sql
        matches observe sql = oracleObservationMatches <$> observation observe sql
    matches expectedObserver fixtureInput >>= assert "input disagrees with the reference result"
    matches inputObserver fixtureExpected >>= assert "oracle rejected the reference result"
    let checkRejected =
            observation inputObserver >=> \observed ->
                assert
                    "expected rejection for changed result or trailing execution error"
                    (observationOutcome observed == OracleRejected)
    mapM_ checkRejected [fixtureChanged, fixtureExpected <> " DROP TABLE missing;"]
    let run arm = withCandidateWorkdir $ \dir -> do
            let reductionConfig = armConfig arm config
            policy <- require fixtureName (metricPolicyForReduction reductionConfig (armJobs arm))
            -- Both arms collect trace metadata, including when the policy ignores it.
            (remember, lookupTrace) <- newResultTraceTracker True
            start <- getMonotonicTimeNSec
            (preflight, observer, validate) <- prepare remember fixtureInput
            end <- getMonotonicTimeNSec
            physicalCalls <- newIORef (1 :: Int)
            accepted <- newIORef []
            fallbackActivations <- newIORef (0 :: Int)
            fallbackSkips <- newIORef (0 :: Int)
            groupNoncontigAccepts <- newIORef (0 :: Int)
            groupContigAccepts <- newIORef (0 :: Int)
            earlyRejectCheckedRef <- newIORef (0 :: Int)
            earlyRejectExactRef <- newIORef (0 :: Int)
            earlyRejectNoResRef <- newIORef (0 :: Int)
            earlyRejectNanosRef <- newIORef (0 :: Integer)
            let counted observe started = observe $ do
                    atomicModifyIORef' physicalCalls (\calls -> (calls + 1, ()))
                    started
                getNumField :: (Eq k, Integral a) => k -> [(k, Value)] -> a
                getNumField key fields = case lookup key fields of
                    Just (Number n) -> round n
                    _ -> 0
                telemetryLogger =
                    Logger
                        defaultLogConfig
                        ( \_ _ msg fields -> do
                            when (msg == "Ranked statement fallback activated") $
                                modifyIORef' fallbackActivations (+ 1)
                            when (msg == "Ranked statement fallback skipped") $
                                modifyIORef' fallbackSkips (+ 1)
                            when (msg == "Dependency group deletion accepted") $
                                case lookup "noncontiguous" fields of
                                    Just (Bool True) -> modifyIORef' groupNoncontigAccepts (+ 1)
                                    _ -> modifyIORef' groupContigAccepts (+ 1)
                            when (msg == "Early rejection certificate summary") $ do
                                modifyIORef' earlyRejectCheckedRef (+ getNumField "checked" fields)
                                modifyIORef' earlyRejectExactRef (+ getNumField "exact_prefix_rejects" fields)
                                modifyIORef' earlyRejectNoResRef (+ getNumField "no_result_rejects" fields)
                                modifyIORef' earlyRejectNanosRef (+ getNumField "check_nanos" fields)
                        )
                        (pure ())
                progress (ProgressCandidateAccepted bytes statements) = do
                    calls <- readIORef physicalCalls
                    now <- getMonotonicTimeNSec
                    let seconds = fromIntegral (now - end) / 1000000000 :: Double
                    modifyIORef'
                        accepted
                        ( object
                            ["physical_calls" .= calls, "seconds" .= seconds, "bytes" .= bytes, "statements" .= statements]
                            :
                        )
                progress _ = pure ()
                select sql = do
                    candidateTrace <- lookupTrace sql
                    pure (if armObservedResult arm then candidateTrace else Nothing)
            result <-
                runEvaluationRequestWithNativeValidation
                    probeTimeout
                    (counted observer)
                    (counted validate)
                    EvaluationRequest
                        { requestOnProgress = progress
                        , requestMetricsPolicy = Just policy
                        , requestConfig =
                            EvaluationConfig
                                { evaluationCacheEnabled = True
                                , evaluationJobs = armJobs arm
                                , evaluationTotalTimeoutMicros = wallLimit
                                , evaluationMaxOracleExecutions = callLimit
                                , evaluationPreflightMicros = fromIntegral ((end - start) `div` 1000)
                                , evaluationPreflightAttempts = 1
                                , evaluationPreflightResetMicros = Just (resetElapsedMicros preflight)
                                , evaluationPreflightOracleMicros = oracleElapsedMicros preflight
                                , evaluationInitialStatements = Just initialCount
                                }
                        , requestDialect = fixtureDialect
                        , requestOracleConfig = ()
                        , requestPreflightObservation = Just (detailedObservation preflight)
                        , requestLogger = telemetryLogger
                        , requestInitialSQL = fixtureInput
                        , requestCheckpoint = dir </> "checkpoint.sql"
                        , requestAction = \report logger pool -> do
                            ReductionResult sql evidence <-
                                reduceSQLWithResultTrace select report reductionConfig logger pool fixtureInput
                            pure (ReductionActionResult sql evidence)
                        , requestCollectCurve = True
                        }
            let sql = evaluatedSQL result
                metrics = evaluationMetrics result
                label = armName arm ++ ": "
                completed = evaluationStatus metrics == EvaluationCompleted
                censored = evaluationStatus metrics `elem` censoredStatuses
            statements <- countStatements fixtureDialect sql >>= require fixtureName
            referenceAccepted <- matches expectedObserver sql
            executions <- readIORef physicalCalls
            curve <- reverse <$> readIORef accepted
            activations <- readIORef fallbackActivations
            skips <- readIORef fallbackSkips
            noncontigAccepts <- readIORef groupNoncontigAccepts
            contigAccepts <- readIORef groupContigAccepts
            erChecked <- readIORef earlyRejectCheckedRef
            erExact <- readIORef earlyRejectExactRef
            erNoRes <- readIORef earlyRejectNoResRef
            erNanos <- readIORef earlyRejectNanosRef
            let report =
                    object
                        [ "arm" .= armName arm
                        , "reduced_sql" .= decodeUtf8 sql
                        , "reduced_bytes" .= BS.length sql
                        , "reduced_statements" .= statements
                        , "reference_accepted" .= referenceAccepted
                        , "physical_oracle_executions" .= executions
                        , "ranked_fallback_activations" .= activations
                        , "ranked_fallback_skips" .= skips
                        , "group_noncontiguous_accepts" .= noncontigAccepts
                        , "group_contiguous_accepts" .= contigAccepts
                        , "early_reject_checked" .= erChecked
                        , "early_reject_exact_prefix" .= erExact
                        , "early_reject_no_result" .= erNoRes
                        , "early_reject_check_micros" .= (fromIntegral erNanos / (1000.0 :: Double))
                        , "best_accepted_curve" .= curve
                        , "evaluation_metrics" .= metrics
                        ]
                failures =
                    [ fixtureName ++ ": " ++ label ++ message
                    | (passed, message) <-
                        [
                            ( completed || censored
                            , "evaluation status " ++ show (evaluationStatus metrics, evaluationFailure metrics)
                            )
                        ,
                            ( evaluationTimedOutObservations metrics == 0 && evaluationOracleErrors metrics == 0
                            , "a probe timed out or failed"
                            )
                        , (censored || evaluationAuditCompleted metrics, "statement minimality was not confirmed")
                        ,
                            ( evaluationValidationPassed metrics && evaluationValidationDatabaseObservations metrics == 2
                            , "expected two fresh, successful final validations"
                            )
                        , (referenceAccepted, "final SQL disagrees with the reference result")
                        ,
                            ( executions
                                == 1 + evaluationDatabaseObservations metrics + evaluationValidationDatabaseObservations metrics
                            , "physical execution accounting disagrees with evaluator"
                            )
                        , (maybe True (executions <=) callLimit, "physical execution budget exceeded")
                        , (censored || BS.length sql < BS.length fixtureInput, "fixture should shrink")
                        ]
                    , not passed
                    ]
            pure (report, failures)
    -- NOTE: Warm both arms once. Keep cold-start effects out of measured samples.
    warmups <- mapM run [control, treatment]
    samples <- forM [1 .. pairs] $ \pair -> do
        let arms = if odd pair then [control, treatment] else [treatment, control]
        (runs, failures) <- mapAndUnzipM run arms
        pure (object ["pair" .= pair, "runs" .= runs], concat failures)
    let report =
            object
                [ "name" .= fixtureName
                , "dialect" .= sqlDialectName fixtureDialect
                , "input_sql" .= decodeUtf8 fixtureInput
                , "expected_sql" .= decodeUtf8 fixtureExpected
                , "initial_bytes" .= BS.length fixtureInput
                , "initial_statements" .= initialCount
                , "engine_result_index" .= resultStatementIndex fixtureLocation
                , "global_fence" .= fixtureFence
                , "warmups" .= map fst warmups
                , "samples" .= map fst samples
                ]
    pure (report, concatMap snd warmups ++ concatMap snd samples)

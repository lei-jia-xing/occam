-- | Baseline runners for result selection, syntax policy, reparsing, and worker count.
module ResultBaseline.Basic (
    runResultBaseline,
    runSearchBaseline,
    runLiteralBatchBaseline,
    runValuesScheduleBaseline,
    runCombinedSyntaxBaseline,
    runReparseBaseline,
    runWorkerBaseline,
) where

import Data.ByteString (ByteString)
import Occam.Core (ResultStatement (..))
import Occam.Parser (SQLDialect (..))
import Occam.Reducer (ReductionConfig (..))
import ResultBaseline.Fixtures (Fixture (..), fixtures, searchFixtures)
import ResultBaseline.Runner (Arm (..), runComparison, searchCensoredStatuses, serialArm)
import ResultBaseline.Syntax (runSyntaxPolicyBaseline)

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

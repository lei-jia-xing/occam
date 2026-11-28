-- | Syntax, distributed literal-batch, and deferred-literal baseline runners.
module ResultBaseline.Syntax (
    runSyntaxPolicyBaseline,
    runDistributedLiteralBatchBaseline,
    runDistributedRefreshBaseline,
    runDistributedMisses1Vs2Baseline,
    runDistributedMisses2Vs3Baseline,
    runDistributedMin16Vs32Baseline,
    runDistributedCap8Vs16Baseline,
    runDistributedCap16Vs32Baseline,
    runDeferLiteralVariants01Baseline,
    runDeferRecursiveBaseline,
) where

import Data.ByteString (ByteString)
import Occam.Parser (SQLDialect (..))

import Occam.Reducer (
    DistributedBatchConfig (..),
    LiteralDeferralMode (..),
    ReductionConfig (..),
    defaultDistributedBatchConfig,
 )
import ResultBaseline.Fixtures (Fixture (..), fixtures, searchFixtures)
import ResultBaseline.Runner (runComparison, searchCensoredStatuses, serialArm)
import ResultBaseline.SyntaxFixtures

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

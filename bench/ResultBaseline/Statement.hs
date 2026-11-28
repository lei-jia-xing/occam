-- | Statement-search, ranked scheduling, fallback, proposal, and early-reject baselines.
module ResultBaseline.Statement (
    runStatementYieldBaseline,
    runStatementOrderBaseline,
    runStatementWeightBaseline,
    runStatementRankedBaseline,
    runStatementRankedGraphFreeBaseline,
    runStatementBoundedPrefClosureBaseline,
    runStatementFallbackBaseline,
    runStatementFallbackNoLocBaseline,
    runGroupProposalsBaseline,
    runGroupProposalsNoLocBaseline,
    runEarlyRejectBaseline,
) where

import Occam.Reducer (
    ReductionConfig (..),
    StatementSearchPolicy (..),
    statementRanked,
 )
import ResultBaseline.Fixtures
import ResultBaseline.Runner (runComparison, searchCensoredStatuses, serialArm)

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

runStatementBoundedPrefClosureBaseline :: Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
runStatementBoundedPrefClosureBaseline =
    runClosurePair
        "statement-byte-weighted"
        StatementByteWeighted
        "statement-ranked"
        statementRanked

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
                    , reductionStatementSearch = StatementRankedFallback
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
                    , reductionStatementSearch = StatementRankedFallback
                    }
            )
            False
        )
        fallbackNoLocCohort
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

runClosurePair ::
    String ->
    StatementSearchPolicy ->
    String ->
    StatementSearchPolicy ->
    Int ->
    Maybe Int ->
    Maybe Int ->
    FilePath ->
    IO ()
runClosurePair controlName control treatmentName treatment pairs wallLimit callLimit out =
    runComparison
        pairs
        out
        wallLimit
        callLimit
        (searchCensoredStatuses wallLimit callLimit)
        (arm controlName control, arm treatmentName treatment)
        closureRankedCohort
    where
        arm name policy =
            serialArm
                name
                (\config -> config {reductionSlicing = False, reductionStatementSearch = policy})
                True

-- | Statement-search, ranked scheduling, fallback, proposal, and early-reject baselines.
module ResultBaseline.Statement (
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
) where

import Occam.Reducer (
    LaneOrdering (..),
    LaneScheduling (..),
    ProjectionWindow (..),
    ProjectionWindowOrder (..),
    RankedScheduleConfig (..),
    ReductionConfig (..),
    SingletonResume (..),
    StatementSearchPolicy (..),
    defaultRankedScheduleConfig,
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

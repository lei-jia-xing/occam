-- | Configuration, search policies, stage events, and result types for the reducer.
module Occam.Reducer.Config (
    ColumnMode (..),
    columnModeName,
    syntaxPolicyName,
    StatementLane (..),
    LaneOrdering (..),
    SingletonResume (..),
    LaneScheduling (..),
    ProjectionWindowOrder (..),
    ProjectionWindow (..),
    RankedScheduleConfig (..),
    defaultRankedScheduleConfig,
    convergedRankedScheduleConfig,
    rankedSchedulePolicyTag,
    shouldActivateRankedFallback,
    StatementSearchPolicy (..),
    statementRanked,
    statementRankedFallback,
    statementSearchPolicyName,
    LiteralDeferralMode (..),
    DistributedBatchConfig (..),
    defaultDistributedBatchConfig,
    ReductionConfig (..),
    defaultReductionConfig,
    reductionSearchPolicy,
    literalBatchPolicy,
    distributedLiteralBatchPolicy,
    deferLiteralVariantsPolicy,
    deferRecursivePredicatesPolicy,
    astBatchPolicy,
    valuesChunkPolicy,
    fullReparsePolicy,
    ReductionStage (..),
    reductionStageName,
    stageLogName,
    StageEvent (..),
    StageReporter,
    nullStageReporter,
    ReductionEvidence (..),
    ReductionResult (..),
) where

import Data.ByteString (ByteString)
import Data.Text qualified as T
import Occam.Parser.Types (SQLDialect, defaultSQLDialect)

-- | Column co-reduction mode: disabled, single-site, or atomic multi-site bundles.
data ColumnMode = ColumnOff | ColumnSingle | ColumnAtomic
    deriving (Eq, Show)

columnModeName :: ColumnMode -> T.Text
columnModeName ColumnOff = "off"
columnModeName ColumnSingle = "single"
columnModeName ColumnAtomic = "atomic"

syntaxPolicyName :: ColumnMode -> T.Text
syntaxPolicyName ColumnOff = "phase-forward-sweep-list-chunks"
syntaxPolicyName ColumnSingle = "phase-forward-sweep-list-chunks+column-single"
syntaxPolicyName ColumnAtomic = "phase-forward-sweep-list-chunks+column-atomic"

-- | Total policy description for the given reduction configuration.
reductionSearchPolicy :: ReductionConfig -> T.Text
reductionSearchPolicy config =
    syntax
        <> statement
        <> groupProposalsPolicy (reductionGroupProposals config)
        <> earlyRejectPolicy (reductionEarlyReject config)
    where
        syntax
            | not (reductionIntraStatement config) = syntaxPolicyName ColumnOff
            | otherwise =
                syntaxPolicyName (reductionColumnMode config)
                    <> literalBatchPolicy (reductionLiteralBatches config)
                    <> distributedLiteralBatchPolicy (reductionDistributedLiteralBatches config)
                    <> deferLiteralVariantsPolicy (reductionDeferLiteralVariants config)
                    <> deferRecursivePredicatesPolicy (reductionDeferRecursivePredicates config)
                    <> astBatchPolicy (reductionAstBatching config)
                    <> valuesChunkPolicy (reductionAdaptiveValues config)
                    <> fullReparsePolicy (reductionFullSyntaxReparse config)
        statement = case reductionStatementSearch config of
            StatementSubsetFirst -> ""
            StatementComplementFirst -> "+complement-first"
            StatementByteWeighted -> "+complement-first-byte-weighted"
            StatementRankedWith cfg -> "+" <> rankedSchedulePolicyTag cfg
            StatementRankedFallback cfg ->
                "+fallback-"
                    <> rankedSchedulePolicyTag cfg
                    <> "-m"
                    <> T.pack (show (rankedFallbackMinStatements cfg))
                    <> "-s"
                    <> T.pack (show (rankedFallbackMaxShrinkPct cfg))

groupProposalsPolicy :: Bool -> T.Text
groupProposalsPolicy False = ""
groupProposalsPolicy True = "+group-proposals"

earlyRejectPolicy :: Bool -> T.Text
earlyRejectPolicy False = ""
earlyRejectPolicy True = "+early-reject"

literalBatchPolicy :: Bool -> T.Text
literalBatchPolicy False = ""
literalBatchPolicy True = "+literal-batches"

distributedLiteralBatchPolicy :: Bool -> T.Text
distributedLiteralBatchPolicy False = ""
distributedLiteralBatchPolicy True = "+distributed-literal-batches"

deferLiteralVariantsPolicy :: LiteralDeferralMode -> T.Text
deferLiteralVariantsPolicy DeferLiteralsOff = ""
deferLiteralVariantsPolicy DeferLiteralsPrimaryOne = "+defer-literal-variants"
deferLiteralVariantsPolicy DeferLiteralsZeroOne = "+defer-literal-variants-01"

deferRecursivePredicatesPolicy :: Bool -> T.Text
deferRecursivePredicatesPolicy False = ""
deferRecursivePredicatesPolicy True = "+defer-recursive-predicates"

astBatchPolicy :: Bool -> T.Text
astBatchPolicy False = ""
astBatchPolicy True = "+ast-batches"

valuesChunkPolicy :: Bool -> T.Text
valuesChunkPolicy False = ""
valuesChunkPolicy True = "+defer-fine-values-chunks"

fullReparsePolicy :: Bool -> T.Text
fullReparsePolicy False = ""
fullReparsePolicy True = "+full-reparse"

-- | Candidate family lane inside ranked statement scheduling.
data StatementLane
    = LaneComplement
    | LaneSingleton
    | LaneSubset
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | How candidates are ordered inside each lane before cross-lane merging.
data LaneOrdering
    = LaneOrderPartition
    | LaneOrderEstimatedBytes
    deriving (Eq, Show)

-- | How single-statement deletion resumes after an accepted singleton shrink.
data SingletonResume
    = ResumeNone
    | ResumeCursorOnly
    | ResumeWeightStableTies
    | ResumeWeightMonotonic
    deriving (Eq, Show)

-- | How the three candidate lanes share probe slots across each pass.
data LaneScheduling
    = LaneSerial
    | LaneRoundRobin
    | LaneBoundedPreference !Int
    deriving (Eq, Show)

-- | Candidate ordering within a fixed-size closure-projection lookahead window.
data ProjectionWindowOrder
    = WindowOrderPreserve
    | WindowOrderCrossEstimated
    | WindowOrderCrossProjected
    | WindowOrderWithinLaneProjected
    deriving (Eq, Show)

-- | Optional bounded lookahead window for closure projection before probing.
data ProjectionWindow
    = NoProjectionWindow
    | ProjectionWindow !Int !ProjectionWindowOrder
    deriving (Eq, Show)

-- | Orthogonal configuration for generalized ranked statement scheduling.
data RankedScheduleConfig = RankedScheduleConfig
    { rankedLaneOrdering :: !LaneOrdering
    , rankedLaneScheduling :: !LaneScheduling
    , rankedSingletonResume :: !SingletonResume
    , rankedProjectionWindow :: !ProjectionWindow
    , rankedFallbackMinStatements :: !Int
    , rankedFallbackMaxShrinkPct :: !Int
    }
    deriving (Eq, Show)

defaultRankedScheduleConfig :: RankedScheduleConfig
defaultRankedScheduleConfig =
    RankedScheduleConfig
        { rankedLaneOrdering = LaneOrderEstimatedBytes
        , rankedLaneScheduling = LaneRoundRobin
        , rankedSingletonResume = ResumeNone
        , rankedProjectionWindow = NoProjectionWindow
        , rankedFallbackMinStatements = 8
        , rankedFallbackMaxShrinkPct = 0
        }

rankedSchedulePolicyTag :: RankedScheduleConfig -> T.Text
rankedSchedulePolicyTag cfg = base <> resumeTag <> windowTag
    where
        base = case (rankedLaneOrdering cfg, rankedLaneScheduling cfg) of
            (LaneOrderPartition, _) -> "interleaved-statements"
            (LaneOrderEstimatedBytes, LaneSerial) -> "ranked-lanes"
            (LaneOrderEstimatedBytes, LaneRoundRobin) -> "ranked-statements"
            (LaneOrderEstimatedBytes, LaneBoundedPreference q) ->
                "ranked-pref-" <> T.pack (show q)
        resumeTag = case rankedSingletonResume cfg of
            ResumeNone -> ""
            ResumeCursorOnly -> "-resume"
            ResumeWeightStableTies -> "-resume-ties"
            ResumeWeightMonotonic -> "-resume-mono"
        windowTag = case rankedProjectionWindow cfg of
            NoProjectionWindow -> ""
            ProjectionWindow w order ->
                "-win"
                    <> T.pack (show w)
                    <> case order of
                        WindowOrderPreserve -> "-preserve"
                        WindowOrderCrossEstimated -> "-est"
                        WindowOrderCrossProjected -> "-proj"
                        WindowOrderWithinLaneProjected -> "-lane-proj"

-- | Decide whether post-slicing fallback should switch from byte-weighted ddmin to ranked search.
shouldActivateRankedFallback :: RankedScheduleConfig -> Bool -> Int -> Int -> Int -> Bool
shouldActivateRankedFallback cfg slicingEnabled initialBytes slicedBytes remainingStmts =
    slicingEnabled
        && remainingStmts >= max 1 (rankedFallbackMinStatements cfg)
        && initialBytes > 0
        && removedBytes * 100 <= initialBytes * max 0 (rankedFallbackMaxShrinkPct cfg)
    where
        removedBytes = max 0 (initialBytes - slicedBytes)

data StatementSearchPolicy
    = StatementSubsetFirst
    | StatementComplementFirst
    | StatementByteWeighted
    | StatementRankedWith !RankedScheduleConfig
    | StatementRankedFallback !RankedScheduleConfig
    deriving (Eq, Show)

convergedRankedScheduleConfig :: RankedScheduleConfig
convergedRankedScheduleConfig =
    defaultRankedScheduleConfig
        { rankedLaneScheduling = LaneBoundedPreference 4
        , rankedSingletonResume = ResumeWeightMonotonic
        , rankedFallbackMinStatements = 8
        , rankedFallbackMaxShrinkPct = 10
        }

statementRanked :: StatementSearchPolicy
statementRanked = StatementRankedWith defaultRankedScheduleConfig

statementRankedFallback :: StatementSearchPolicy
statementRankedFallback = StatementRankedFallback convergedRankedScheduleConfig

statementSearchPolicyName :: StatementSearchPolicy -> T.Text
statementSearchPolicyName StatementSubsetFirst = "subset-first"
statementSearchPolicyName StatementComplementFirst = "complement-first"
statementSearchPolicyName StatementByteWeighted = "byte-weighted"
statementSearchPolicyName (StatementRankedWith cfg)
    | cfg == convergedRankedScheduleConfig = "ranked"
    | otherwise = rankedSchedulePolicyTag cfg
statementSearchPolicyName (StatementRankedFallback cfg)
    | cfg == convergedRankedScheduleConfig = "ranked-fallback"
    | otherwise =
        "fallback-"
            <> rankedSchedulePolicyTag cfg
            <> "-m"
            <> T.pack (show (rankedFallbackMinStatements cfg))
            <> "-s"
            <> T.pack (show (rankedFallbackMaxShrinkPct cfg))

-- | Which literal replacement variants run in the regular sweep before stalling.
data LiteralDeferralMode
    = DeferLiteralsOff
    | DeferLiteralsPrimaryOne
    | DeferLiteralsZeroOne
    deriving (Eq, Show)

-- | Thresholds and rejection limit for cross-statement literal batching.
data DistributedBatchConfig = DistributedBatchConfig
    { distributedMinStatements :: !Int
    , distributedMaxBatchSize :: !Int
    , distributedMaxMisses :: !Int
    }
    deriving (Eq, Show)

defaultDistributedBatchConfig :: DistributedBatchConfig
defaultDistributedBatchConfig =
    DistributedBatchConfig
        { distributedMinStatements = 32
        , distributedMaxBatchSize = 16
        , distributedMaxMisses = 3
        }

-- | Reduction pipeline configuration options.
data ReductionConfig = ReductionConfig
    { reductionSlicing :: !Bool
    , reductionGroupProposals :: !Bool
    , reductionEarlyReject :: !Bool
    , reductionClosure :: !Bool
    , reductionStatementSearch :: !StatementSearchPolicy
    , reductionIntraStatement :: !Bool
    , reductionLiteralBatches :: !Bool
    , reductionDistributedLiteralBatches :: !Bool
    , reductionDistributedBatchConfig :: !DistributedBatchConfig
    , reductionDeferLiteralVariants :: !LiteralDeferralMode
    , reductionDeferRecursivePredicates :: !Bool
    , reductionAstBatching :: !Bool
    , reductionAdaptiveValues :: !Bool
    , reductionFullSyntaxReparse :: !Bool
    , reductionUnrestricted :: !Bool
    , reductionDialect :: !SQLDialect
    , reductionColumnMode :: !ColumnMode
    }
    deriving (Eq, Show)

-- | Default reduction pipeline configuration.
defaultReductionConfig :: ReductionConfig
defaultReductionConfig =
    ReductionConfig
        { reductionSlicing = True
        , reductionGroupProposals = True
        , reductionEarlyReject = True
        , reductionClosure = True
        , reductionStatementSearch = statementRankedFallback
        , reductionIntraStatement = False
        , reductionLiteralBatches = False
        , reductionDistributedLiteralBatches = True
        , reductionDistributedBatchConfig = defaultDistributedBatchConfig
        , reductionDeferLiteralVariants = DeferLiteralsZeroOne
        , reductionDeferRecursivePredicates = True
        , reductionAstBatching = False
        , reductionAdaptiveValues = False
        , reductionFullSyntaxReparse = False
        , reductionUnrestricted = True
        , reductionDialect = defaultSQLDialect
        , reductionColumnMode = ColumnOff
        }

-- | Typed reduction stage, emitted at exact boundaries for progress and summaries.
data ReductionStage
    = StageSlicing
    | StageClosureSearch
    | StageGraphFreeSearch
    | StageUnrestrictedFallback
    | StageStatementAudit
    | StageIntraStatementRewrites
    deriving (Eq, Ord, Show, Enum, Bounded)

reductionStageName :: ReductionStage -> T.Text
reductionStageName StageSlicing = "slicing"
reductionStageName StageClosureSearch = "closure-search"
reductionStageName StageGraphFreeSearch = "graph-free-search"
reductionStageName StageUnrestrictedFallback = "unrestricted-fallback"
reductionStageName StageStatementAudit = "statement-audit"
reductionStageName StageIntraStatementRewrites = "intra-statement-rewrites"

-- | Legacy log-stage labels, kept stable for user-visible logs.
stageLogName :: ReductionStage -> T.Text
stageLogName StageSlicing = "backward-slicing"
stageLogName StageClosureSearch = "closure-search"
stageLogName StageGraphFreeSearch = "graph-free-statement-search"
stageLogName StageUnrestrictedFallback = "unrestricted-fallback"
stageLogName StageStatementAudit = "statement-audit"
stageLogName StageIntraStatementRewrites = "intra-statement-rewrites"

-- | Boundary event: enter names the stage; finish carries seconds and bytes.
data StageEvent
    = StageStarted !ReductionStage
    | StageFinished !ReductionStage !Double !Int !Int
    deriving (Eq, Show)

type StageReporter = StageEvent -> IO ()

nullStageReporter :: StageReporter
nullStageReporter _ = pure ()

-- | Evidence indicating the reduced SQL has satisfied statement-level audit minimality.
data ReductionEvidence = ReductionEvidence
    { evidenceBytes :: !ByteString
    , evidenceAuditMinimal :: !Bool
    }
    deriving (Eq, Show)

data ReductionResult = ReductionResult
    { reductionSQL :: !ByteString
    , reductionEvidence :: !(Maybe ReductionEvidence)
    }
    deriving (Eq, Show)

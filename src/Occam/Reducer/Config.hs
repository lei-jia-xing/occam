-- | Configuration, search policies, stage events, and result types for the reducer.
module Occam.Reducer.Config (
    ColumnMode (..),
    columnModeName,
    syntaxPolicyName,
    StatementLane (..),
    shouldActivateRankedFallback,
    StatementSearchPolicy (..),
    statementRanked,
    statementRankedFallback,
    statementSearchPolicyName,
    LiteralDeferralMode (..),
    DistributedBatchConfig (..),
    defaultDistributedBatchConfig,
    ReductionConfig (..),
    defaultReductionConfigFor,
    reductionSearchPolicy,
    literalBatchPolicy,
    distributedLiteralBatchPolicy,
    deferLiteralVariantsPolicy,
    deferRecursivePredicatesPolicy,
    astBatchPolicy,
    valuesChunkPolicy,
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
import Occam.Parser.Types (SQLDialect)

-- | Column co-reduction mode: disabled or atomic multi-site bundles.
data ColumnMode = ColumnOff | ColumnAtomic
    deriving (Eq, Show)

columnModeName :: ColumnMode -> T.Text
columnModeName ColumnOff = "off"
columnModeName ColumnAtomic = "atomic"

syntaxPolicyName :: ColumnMode -> T.Text
syntaxPolicyName ColumnOff = "phase-forward-sweep-list-chunks"
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
        statement = case reductionStatementSearch config of
            StatementSubsetFirst -> ""
            StatementComplementFirst -> "+complement-first"
            StatementByteWeighted -> "+complement-first-byte-weighted"
            StatementRanked -> "+ranked-pref-4-resume-mono"
            StatementRankedFallback -> "+fallback-ranked-pref-4-resume-mono-m8-s10"

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

-- | Candidate family lane inside ranked statement scheduling.
data StatementLane
    = LaneComplement
    | LaneSingleton
    | LaneSubset
    deriving (Eq, Ord, Show, Enum, Bounded)

{- | Decide whether post-slicing fallback should switch from byte-weighted ddmin
to ranked search after removing at most 10% of the bytes with >= 8 statements left.
-}
shouldActivateRankedFallback :: Bool -> Int -> Int -> Int -> Bool
shouldActivateRankedFallback slicingEnabled initialBytes slicedBytes remainingStmts =
    slicingEnabled
        && remainingStmts >= 8
        && initialBytes > 0
        && removedBytes * 100 <= initialBytes * 10
    where
        removedBytes = max 0 (initialBytes - slicedBytes)

data StatementSearchPolicy
    = StatementSubsetFirst
    | StatementComplementFirst
    | StatementByteWeighted
    | StatementRanked
    | StatementRankedFallback
    deriving (Eq, Show)

statementRanked :: StatementSearchPolicy
statementRanked = StatementRanked

statementRankedFallback :: StatementSearchPolicy
statementRankedFallback = StatementRankedFallback

statementSearchPolicyName :: StatementSearchPolicy -> T.Text
statementSearchPolicyName StatementSubsetFirst = "subset-first"
statementSearchPolicyName StatementComplementFirst = "complement-first"
statementSearchPolicyName StatementByteWeighted = "byte-weighted"
statementSearchPolicyName StatementRanked = "ranked"
statementSearchPolicyName StatementRankedFallback = "ranked-fallback"

-- | Which literal replacement variants run in the regular sweep before stalling.
data LiteralDeferralMode
    = DeferLiteralsOff
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
    , reductionUnrestricted :: !Bool
    , reductionDialect :: !SQLDialect
    , reductionColumnMode :: !ColumnMode
    }
    deriving (Eq, Show)

-- | Default reduction pipeline configuration for a specific SQL dialect.
defaultReductionConfigFor :: SQLDialect -> ReductionConfig
defaultReductionConfigFor dialect =
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
        , reductionUnrestricted = True
        , reductionDialect = dialect
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

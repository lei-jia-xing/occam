-- | SQL test case reduction pipeline combining slicing, ddmin, and intra-statement rewrites.
module Occam.Reducer (
    ReductionConfig (..),
    DistributedBatchConfig (..),
    defaultDistributedBatchConfig,
    LiteralDeferralMode (..),
    StatementLane (..),
    LaneOrdering (..),
    SingletonResume (..),
    LaneScheduling (..),
    ProjectionWindowOrder (..),
    ProjectionWindow (..),
    RankedScheduleConfig (..),
    defaultRankedScheduleConfig,
    convergedRankedScheduleConfig,
    statementRanked,
    statementRankedFallback,
    shouldActivateRankedFallback,
    StatementSearchPolicy (..),
    statementSearchPolicyName,
    ColumnMode (..),
    columnModeName,
    reductionSearchPolicy,
    defaultReductionConfig,
    isValidClosure,
    isValidClosureFast,
    closeSet,
    closeSetFast,
    closureDdmin,
    cutRemovedStatements,
    cutRemovedStatementIds,
    cutRemovedStatementInts,
    statementIds,
    statementIdsInt,
    replaceSpan,
    applyParserEdits,
    selectIndependentEdits,
    independentEditBatches,
    partitionChunks,
    partitionStatementsByBytes,
    partitionStatementsByWeights,
    reduceSQL,
    reduceSQLWithLogger,
    reduceSQLWithConfig,
    reduceSQLWithEvidence,
    reduceSQLWithResultSlicing,
    reduceSQLWithResultTrace,
    reduceSQLWithResultTraceSeeded,
    reduceSQLWithResultTraceFull,
    ReductionResult (..),
    ReductionEvidence (..),
    ReductionStage (..),
    reductionStageName,
    StageEvent (..),
    StageReporter,
    nullStageReporter,
    statementDdmin,
    statementDdminWithOrder,
    statementDdminWithParser,
    statementAudit,
    statementAuditWithLogger,
    statementAuditWithParser,
    parseStatementsFast,
    reuseLatestSQLWithSeed,
    AuditStats (..),
    findInterestingPrefix,
) where

import Control.Applicative (empty, (<|>))
import Control.Exception.Safe (finally)
import Control.Monad (forM, guard, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (partition, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Certificate (
    EarlyRejectCertifier (..),
    EarlyRejectStats (..),
    EarlyRejectTracker,
    FailingPrefixCache,
    buildEarlyRejectCertifier,
    checkEarlyRejectionBytesTracked,
    checkEarlyRejectionIdsTracked,
    checkEarlyRejectionSyntaxEditTracked,
    emptyEarlyRejectCertifier,
    newEarlyRejectTracker,
    newEarlyRejectTrackerWithFailures,
    readEarlyRejectStats,
    recordEarlyRejectBuildNanos,
 )
import Occam.Core (
    BarrierScope (GlobalScope),
    EarlyRejectReason (..),
    Ident (..),
    OracleOutcome (..),
    ResultStatement (..),
    ResultTrace (..),
    ScopedBarrier (barrierScope),
    SemanticFacts (factBarriers),
    SessionInit (..),
    Span (..),
    Statement (..),
    StatementId (..),
    nameText,
    oracleAccepts,
    traceResultStatement,
 )
import Occam.Dependency (
    DependencyGraph (..),
    analyzeStatements,
 )
import Occam.Facts (editCategoryName)
import Occam.Logger (Logger, logDebug', logInfo', logWarn, nullLogger)
import Occam.Metrics (
    FamilyTag (..),
    MetricsStore,
    ParseKind (..),
    recordAcceptedBytesSaved,
    recordCandidateBuildTiming,
    recordGeneratedSkipped,
 )
import Occam.Parser (
    ColumnSite (..),
    ColumnTarget (..),
    ParseOptions (..),
    ParsedSQL (..),
    ParserEdit (..),
    ParserEditCategory (..),
    SQLDialect (..),
    StatementFacts (..),
    StatementUnit (..),
    defaultParseOptions,
    defaultSQLDialect,
    parseSQLTimed,
    spanOnlyOptions,
 )
import Occam.Pool (
    OraclePool,
    poolJobCount,
    poolMetricsStore,
    poolOracle,
    reportCandidateCount,
    testOrderedGenerated,
    testOrderedTagged,
 )
import Occam.Proposals (
    DeletionProposalGroup (..),
    GroupProposalPlan (..),
    buildRelaxedGraph,
    extractDeletionProposalPlanWithExtraTx,
    isNoncontiguousIn,
    projectTransactionBlocks,
 )
import Occam.Slicer (
    backwardSliceFast,
    backwardSliceMany,
    backwardSliceManyFast,
    closeFromBoundary,
    successorIndex,
 )

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

seconds :: Word64 -> Double
seconds ns = fromIntegral ns / 1000000000

-- | Total 'last': the final element, or 'Nothing' for an empty list.
lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [only] = Just only
lastMay (_ : rest) = lastMay rest

logStage :: Logger -> T.Text -> Word64 -> IO ()
logStage logger stage elapsed =
    logDebug'
        logger
        "reducer"
        ("Finished " <> stage)
        [("stage", toJSON stage), ("duration_seconds", toJSON (seconds elapsed))]

-- | Timed stage with typed boundary events. Keeps the legacy log line.
timedReductionStage ::
    StageReporter -> Logger -> ReductionStage -> ByteString -> IO (ByteString, a) -> IO (ByteString, a)
timedReductionStage reporter logger stage input action = do
    reporter (StageStarted stage)
    started <- getMonotonicTimeNSec
    (output, extra) <- action
    finished <- getMonotonicTimeNSec
    let elapsed = finished - started
    logStage logger (stageLogName stage) elapsed
    reporter (StageFinished stage (seconds elapsed) (BS.length input) (BS.length output))
    pure (output, extra)

kindOfOptions :: ParseOptions -> ParseKind
kindOfOptions options
    | parseColumnIR options || parseColumnBundles options = ParseColumnIR
    | parseChunkEdits options = ParseSyntaxChunks
    | otherwise = ParseStatementOnly

parseUnitsOrThrow ::
    Maybe MetricsStore ->
    ParseOptions ->
    ByteString ->
    IO (SessionInit, [StatementUnit], [ColumnTarget])
parseUnitsOrThrow store options sql = do
    parsed <- parseSQLTimed store (kindOfOptions options) options sql
    case parsed of
        Left parserError -> ioError (userError ("SQL parser transport failure: " ++ show parserError))
        Right result -> pure (parsedInitialSession result, parsedUnits result, parsedColumns result)

type StatementParser = ByteString -> IO (SessionInit, [StatementUnit])

parseStatements :: Maybe MetricsStore -> SQLDialect -> StatementParser
parseStatements store dialect sql = do
    (session, units, _) <- parseUnitsOrThrow store (defaultParseOptions dialect) sql
    pure (session, units)

parseStatementsFast :: Maybe MetricsStore -> SQLDialect -> StatementParser
parseStatementsFast store dialect sql = do
    (session, units, _) <- parseUnitsOrThrow store (spanOnlyOptions dialect) sql
    pure (session, units)

-- | Monadic early exit in 'ContT', discarding the continuation and returning immediately.
earlyExit :: (Monad m) => r -> ContT r m a
earlyExit r = ContT (\_ -> pure r)

-- | Lift a pure 'Maybe' into 'MaybeT'.
hoistMaybe :: (Applicative m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- Share the latest analysis within a reduction, keyed by exact SQL bytes.
reuseLatestSQL :: (ByteString -> IO a) -> IO (ByteString -> IO a)
reuseLatestSQL = reuseLatestSQLWithSeed Nothing

reuseLatestSQLWithSeed :: Maybe (ByteString, a) -> (ByteString -> IO a) -> IO (ByteString -> IO a)
reuseLatestSQLWithSeed initialSeed analyze = do
    latest <- newIORef initialSeed
    pure $ \sql -> do
        cached <- readIORef latest
        case cached of
            Just (previous, result) | previous == sql -> pure result
            _ -> do
                result <- analyze sql
                writeIORef latest (Just (sql, result))
                pure result

-- | Parse options for syntax-level rewrites (skips unused semantic dependency extraction).
syntaxParseOptions :: SQLDialect -> ParseOptions
syntaxParseOptions dialect =
    (defaultParseOptions dialect)
        { parseChunkEdits = True
        , parseSyntaxEdits = True
        , parseSyntaxOnly = True
        }

-- | Single-point column sites only: no bundle planning or payload.
columnSingleParseOptions :: SQLDialect -> ParseOptions
columnSingleParseOptions dialect = (syntaxParseOptions dialect) {parseColumnIR = True, parseSyntaxOnly = False}

-- | Full atomic bundles; implies single-point geometry.
columnAtomicParseOptions :: SQLDialect -> ParseOptions
columnAtomicParseOptions dialect =
    (syntaxParseOptions dialect)
        { parseColumnIR = True
        , parseColumnBundles = True
        , parseSyntaxOnly = False
        }

columnParseOptions :: ColumnMode -> SQLDialect -> ParseOptions
columnParseOptions ColumnOff = syntaxParseOptions
columnParseOptions ColumnSingle = columnSingleParseOptions
columnParseOptions ColumnAtomic = columnAtomicParseOptions

-- | Check whether the retained statement set is closed under dependency predecessors.
isValidClosure :: DependencyGraph -> Set StatementId -> Bool
isValidClosure graph kept =
    let !keptInts = IntSet.fromList (map unStatementId (Set.toList kept))
     in isValidClosureFast graph keptInts

-- | Fast bitwise closure check directly over statement integers.
isValidClosureFast :: DependencyGraph -> IntSet.IntSet -> Bool
isValidClosureFast DependencyGraph {..} keptInts
    | IntMap.null graphFastEdges =
        let keptSet = Set.fromList (map StatementId (IntSet.toList keptInts))
         in all (isClosureOk keptSet) (Set.toList keptSet)
    | otherwise = all isClosureOkFast (IntSet.toList keptInts)
    where
        isClosureOk keptSet sid =
            let predecessors = Map.findWithDefault Set.empty sid graphEdges
             in predecessors `Set.isSubsetOf` keptSet

        isClosureOkFast sid =
            let predecessors = IntMap.findWithDefault IntSet.empty sid graphFastEdges
             in predecessors `IntSet.isSubsetOf` keptInts

-- | Transitive backward closure of a set of statement IDs.
closeSet :: DependencyGraph -> Set StatementId -> Set StatementId
closeSet graph roots = backwardSliceMany roots graph

-- | Transitive backward closure of a set of statement integers.
closeSetFast :: DependencyGraph -> IntSet.IntSet -> IntSet.IntSet
closeSetFast graph roots = backwardSliceManyFast roots graph

-- | Cut removed statements from the source SQL, preserving retained bytes and delimiters.
cutRemovedStatementIds :: ByteString -> [Statement] -> Set StatementId -> ByteString
cutRemovedStatementIds baseSQL allStatements keptIds =
    let !keptInts = IntSet.fromList (map unStatementId (Set.toList keptIds))
     in cutRemovedStatementInts baseSQL allStatements keptInts

-- | Fast statement byte surgery directly driven by an 'IntSet.IntSet' of retained statement IDs.
cutRemovedStatementInts :: ByteString -> [Statement] -> IntSet.IntSet -> ByteString
cutRemovedStatementInts baseSQL allStatements keptInts =
    case chunks 0 allStatements of
        [] -> BS.empty
        [single] -> single
        slices -> BS.concat slices
    where
        sourceLength = BS.length baseSQL

        chunks !cursor []
            | cursor < sourceLength = [BS.drop cursor baseSQL]
            | otherwise = []
        chunks !cursor (stmt : rest)
            | IntSet.member (unStatementId (stmtId stmt)) keptInts = chunks cursor rest
            | otherwise =
                let Span start _ = stmtSpan stmt
                    nextStart = case rest of
                        next : _ -> spanStartByte (stmtSpan next)
                        [] -> sourceLength
                    prefix =
                        if start > cursor
                            then (BS.take (start - cursor) (BS.drop cursor baseSQL) :)
                            else id
                 in prefix (chunks nextStart rest)

cutRemovedStatements :: ByteString -> [Statement] -> [Statement] -> ByteString
cutRemovedStatements baseSQL allStatements keptStatements =
    cutRemovedStatementInts baseSQL allStatements (statementIdsInt keptStatements)

-- | Indexed singleton deletion boundaries for a statement snapshot.
data StatementSnapshot = StatementSnapshot
    { snapSQL :: !ByteString
    , snapOffsets :: !(IntMap.IntMap (Int, Int))
    }

buildStatementSnapshot ::
    ByteString -> [Statement] -> IntSet.IntSet -> ByteString -> StatementSnapshot
buildStatementSnapshot baseSQL allStatements keptInts currentSQL =
    let !sourceLength = BS.length baseSQL
        !firstStart = case allStatements of
            s : _ -> spanStartByte (stmtSpan s)
            [] -> 0
        computeSpans [] = []
        computeSpans (s : rest) =
            let Span start _ = stmtSpan s
                nextStart = case rest of
                    n : _ -> spanStartByte (stmtSpan n)
                    [] -> sourceLength
             in (s, nextStart - start) : computeSpans rest

        spans = computeSpans allStatements

        buildOffsets [] !m !_ = m
        buildOffsets ((s, len) : rest) !m !off
            | IntSet.member (unStatementId (stmtId s)) keptInts =
                let !m' = IntMap.insert (unStatementId (stmtId s)) (off, len) m
                 in buildOffsets rest m' (off + len)
            | otherwise = buildOffsets rest m off

        !offsets = buildOffsets spans IntMap.empty firstStart
     in StatementSnapshot currentSQL offsets

renderSingletonDeletion :: StatementSnapshot -> Statement -> Maybe ByteString
renderSingletonDeletion StatementSnapshot {..} stmt =
    case IntMap.lookup (unStatementId (stmtId stmt)) snapOffsets of
        Nothing -> Nothing
        Just (!off, !len) ->
            Just (BS.take off snapSQL <> BS.drop (off + len) snapSQL)

-- | Split a list into at most n approximately equal, contiguous chunks.
partitionChunks :: Int -> [a] -> [[a]]
partitionChunks n xs
    | n <= 1 = [xs]
    | null xs = []
    | otherwise = go n (length xs) xs
    where
        go _ _ [] = []
        go remaining total rest =
            let chunkSize = (total + remaining - 1) `div` remaining
                (chunk, next) = splitAt chunkSize rest
             in chunk : go (remaining - 1) (total - chunkSize) next

{- | Approximate equal-byte contiguous partitions. Reserve a statement for
each remaining group even when a single unusually large statement dominates.
-}
partitionStatementsByBytes :: Int -> [Statement] -> [[Statement]]
partitionStatementsByBytes groups statements =
    partitionStatementsByWeights
        groups
        [(s, statementSpanBytes s) | s <- statements]
        (sum (map statementSpanBytes statements))

partitionStatementsByWeights :: Int -> [(Statement, Int)] -> Int -> [[Statement]]
partitionStatementsByWeights groups statementsWithWeights totalBytes =
    go groups (length statementsWithWeights) totalBytes statementsWithWeights
    where
        go _ _ _ [] = []
        go remaining _ _ rest | remaining <= 1 = [map fst rest]
        go remaining count bytes ((firstStmt, firstWeight) : rest) =
            let target = (bytes + remaining - 1) `div` remaining
                (chunk, next, consumed, weight) =
                    consume target (count - remaining) [firstStmt] (1 :: Int) firstWeight rest
             in chunk : go (remaining - 1) (count - consumed) (bytes - weight) next
        consume target slots taken consumed weight ((nextStmt, nextWeight) : rest)
            | slots > 0
            , weight < target
            , abs (target - weight - nextWeight) <= target - weight =
                consume target (slots - 1) (nextStmt : taken) (consumed + 1) (weight + nextWeight) rest
        consume _ _ taken consumed weight rest = (reverse taken, rest, consumed, weight)

statementSpanBytes :: Statement -> Int
statementSpanBytes statement =
    let Span start end = stmtSpan statement
     in end - start

statementIds :: [Statement] -> Set StatementId
statementIds = Set.fromList . map stmtId

-- | Collect statement IDs as a bitwise 'IntSet.IntSet'.
statementIdsInt :: [Statement] -> IntSet.IntSet
statementIdsInt = IntSet.fromList . map (unStatementId . stmtId)

statementSQL :: ByteString -> [Statement] -> [Statement] -> ByteString
statementSQL = cutRemovedStatements

recordEarlyRejectSkip :: Maybe MetricsStore -> EarlyRejectReason -> IO ()
recordEarlyRejectSkip store EarlyRejectExactPrefix =
    recordGeneratedSkipped store "early-reject-exact-prefix"
recordEarlyRejectSkip store EarlyRejectNoResultStatement =
    recordGeneratedSkipped store "early-reject-no-result"
recordEarlyRejectSkip store EarlyRejectFailingPrefix =
    recordGeneratedSkipped store "early-reject-failing-prefix"
recordEarlyRejectSkip store EarlyRejectMissingDefinition =
    recordGeneratedSkipped store "early-reject-missing-definition"
recordEarlyRejectSkip store EarlyRejectColumnArityMismatch =
    recordGeneratedSkipped store "early-reject-column-arity"

buildCertifierTracked ::
    Bool ->
    EarlyRejectTracker ->
    ByteString ->
    [StatementUnit] ->
    Maybe ResultTrace ->
    IO EarlyRejectCertifier
buildCertifierTracked False _ _ _ _ = pure emptyEarlyRejectCertifier
buildCertifierTracked True _ _ _ Nothing = pure emptyEarlyRejectCertifier
buildCertifierTracked True tracker baseSQL units mTrace = do
    t0 <- getMonotonicTimeNSec
    let !cert = buildEarlyRejectCertifier baseSQL units mTrace
        !_ =
            IntSet.size (certExactPrefixByteLens cert)
                + IntSet.size (certGuaranteedNonResult cert)
                + IntMap.size (certMissingDefReqs cert)
    t1 <- getMonotonicTimeNSec
    recordEarlyRejectBuildNanos tracker (t1 - t0)
    pure cert

logEarlyRejectStats :: Logger -> T.Text -> EarlyRejectTracker -> IO ()
logEarlyRejectStats logger stage tracker = do
    stats <- readEarlyRejectStats tracker
    when (earlyRejectChecked stats > 0 || earlyRejectCheckNanos stats > 0) $
        logInfo'
            logger
            "reducer"
            "Early rejection certificate summary"
            [ ("stage", toJSON stage)
            , ("checked", toJSON (earlyRejectChecked stats))
            , ("exact_prefix_rejects", toJSON (earlyRejectExactPrefix stats))
            , ("no_result_rejects", toJSON (earlyRejectNoResult stats))
            , ("failing_prefix_rejects", toJSON (earlyRejectFailingPrefix stats))
            , ("missing_definition_rejects", toJSON (earlyRejectMissingDef stats))
            , ("column_arity_rejects", toJSON (earlyRejectColumnArity stats))
            , ("check_nanos", toJSON (earlyRejectCheckNanos stats))
            , ("check_micros", toJSON (fromIntegral (earlyRejectCheckNanos stats) / (1000.0 :: Double)))
            ]

-- | Closure-constrained delta debugging.
closureDdmin :: OraclePool -> ByteString -> [Statement] -> DependencyGraph -> IO [Statement]
closureDdmin = closureDdminWithLogger StatementSubsetFirst nullLogger

closureDdminWithLogger ::
    StatementSearchPolicy ->
    Logger ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    DependencyGraph ->
    IO [Statement]
closureDdminWithLogger policy logger pool baseSQL initialStatements graph = do
    tracker <- newEarlyRejectTracker
    closureDdminWithCert
        tracker
        emptyEarlyRejectCertifier
        policy
        logger
        pool
        baseSQL
        initialStatements
        graph

data ClosureEnv = ClosureEnv
    { ceGraph :: !DependencyGraph
    , ceInitialClosed :: !Bool
    , cePredecessors :: !(IntMap.IntMap IntSet.IntSet)
    , ceSuccessors :: !(IntMap.IntMap IntSet.IntSet)
    , ceWeights :: !(IntMap.IntMap Int)
    , ceCache :: !(IORef (HashMap IntSet.IntSet IntSet.IntSet))
    , ceFullRoots :: !(IORef (Maybe (IntSet.IntSet, IntSet.IntSet)))
    , ceStats :: !(IORef ClosureStats)
    }

closureDdminWithCert ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    StatementSearchPolicy ->
    Logger ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    DependencyGraph ->
    IO [Statement]
closureDdminWithCert tracker cert policy logger pool baseSQL initialStatements graph = do
    closureCache <- newIORef HashMap.empty
    fullRootsCache <- newIORef Nothing
    closureStats <- newIORef (ClosureStats 0 0 0 0)
    let predecessors = graphFastEdges graph
        env =
            ClosureEnv
                { ceGraph = graph
                , ceInitialClosed = isValidClosureFast graph (statementIdsInt initialStatements)
                , cePredecessors = predecessors
                , ceSuccessors = successorIndex predecessors
                , ceWeights =
                    IntMap.fromList [(unStatementId (stmtId s), statementSpanBytes s) | s <- initialStatements]
                , ceCache = closureCache
                , ceFullRoots = fullRootsCache
                , ceStats = closureStats
                }
    result <-
        searchStatementsWithCert
            tracker
            cert
            True
            policy
            (buildClosureCandidate env)
            pool
            baseSQL
            initialStatements
            initialStatements
    stats <- readIORef closureStats
    logClosureStats logger stats
    pure result

computeFullSliceRoots ::
    IntMap.IntMap IntSet.IntSet -> DependencyGraph -> IntSet.IntSet -> IntSet.IntSet
computeFullSliceRoots successors graph currentInts = case IntSet.minView sinks of
    Just (sink, rest)
        | IntSet.null rest
        , backwardSliceFast sink graph == currentInts ->
            IntSet.singleton sink
    _ -> IntSet.empty
    where
        sinks =
            IntSet.filter
                (\v -> IntSet.disjoint currentInts (IntMap.findWithDefault IntSet.empty v successors))
                currentInts

lookupFullSliceRoots :: ClosureEnv -> IntSet.IntSet -> IO IntSet.IntSet
lookupFullSliceRoots env currentInts
    | not (ceInitialClosed env) || IntMap.null (cePredecessors env) = pure IntSet.empty
    | otherwise = do
        cached <- readIORef (ceFullRoots env)
        case cached of
            Just (cachedCurrent, roots) | cachedCurrent == currentInts -> pure roots
            _ -> do
                let !roots = computeFullSliceRoots (ceSuccessors env) (ceGraph env) currentInts
                writeIORef (ceFullRoots env) (Just (currentInts, roots))
                pure roots

closeClosureCandidate :: ClosureEnv -> IntSet.IntSet -> IntSet.IntSet -> IntSet.IntSet
closeClosureCandidate env current seed
    | ceInitialClosed env
    , not (IntMap.null (cePredecessors env))
    , let deleted = current `IntSet.difference` seed
    , IntSet.size deleted < IntSet.size seed =
        closeFromBoundary (cePredecessors env) (ceSuccessors env) seed deleted
    | otherwise = closeSetFast (ceGraph env) seed

buildClosureCandidate :: ClosureEnv -> IntSet.IntSet -> IntSet.IntSet -> IO (Maybe IntSet.IntSet)
buildClosureCandidate env currentInts seedInts = evalContT do
    liftIO $ modifyIORef' (ceStats env) (\s -> s {closureRequests = closureRequests s + 1})
    fullRoots <- liftIO $ lookupFullSliceRoots env currentInts
    unless (IntSet.disjoint seedInts fullRoots) (earlyExit (Just currentInts))
    closedInts <- liftIO $ lookupOrComputeClosure env currentInts seedInts
    -- A seed is a subset of the current set. If the initial set was closed,
    -- the seed closure stays inside it, and each accepted closure keeps the
    -- same invariant for the next search pass.
    let contained = ceInitialClosed env || closedInts `IntSet.isSubsetOf` currentInts
        keptInts
            | contained = closedInts
            | otherwise = closedInts `IntSet.intersection` currentInts
        added = keptInts `IntSet.difference` seedInts
        gainedBytes = sum [IntMap.findWithDefault 0 sid (ceWeights env) | sid <- IntSet.toList added]
    liftIO $
        modifyIORef'
            (ceStats env)
            ( \s ->
                s
                    { closureGainStatements = closureGainStatements s + IntSet.size added
                    , closureGainBytes = closureGainBytes s + gainedBytes
                    }
            )
    -- A closed projection containing the seed must contain its entire closure.
    pure (if contained then Just closedInts else Nothing)

lookupOrComputeClosure :: ClosureEnv -> IntSet.IntSet -> IntSet.IntSet -> IO IntSet.IntSet
lookupOrComputeClosure env currentInts seedInts = do
    cached <- readIORef (ceCache env)
    case HashMap.lookup seedInts cached of
        Just result -> do
            modifyIORef' (ceStats env) (\s -> s {closureCacheHits = closureCacheHits s + 1})
            pure result
        Nothing -> do
            let result = closeClosureCandidate env currentInts seedInts
            modifyIORef' (ceCache env) (HashMap.insert seedInts result)
            pure result

logClosureStats :: Logger -> ClosureStats -> IO ()
logClosureStats logger stats
    | gainBytes > 0 || gainStmts > 0 =
        logInfo'
            logger
            "reducer"
            "Closure search accepted reduction"
            [ ("gain_statements", toJSON gainStmts)
            , ("gain_bytes", toJSON gainBytes)
            ]
    | otherwise =
        logDebug'
            logger
            "reducer"
            "Closure search found no further reductions"
            [ ("closure_requests", toJSON (closureRequests stats))
            , ("closure_cache_hits", toJSON (closureCacheHits stats))
            ]
    where
        gainStmts = closureGainStatements stats
        gainBytes = closureGainBytes stats

data ClosureStats = ClosureStats
    { closureRequests :: !Int
    , closureCacheHits :: !Int
    , closureGainStatements :: !Int
    , closureGainBytes :: !Int
    }

data CandidateVerdict
    = CandidateAccepted
    | CandidateRejected
    deriving (Eq, Show)

-- | Verdict memo keyed by exact candidate bytes.
newtype Feedback = Feedback
    { fbEvaluated :: IORef (HashMap ByteString CandidateVerdict)
    }

newFeedback :: IO Feedback
newFeedback = Feedback <$> newIORef HashMap.empty
searchStatementsWithCert ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    Bool ->
    StatementSearchPolicy ->
    CandidateBuilder ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO [Statement]
searchStatementsWithCert tracker cert closureAware policy buildCandidate pool baseSQL allStatements initialStatements = do
    feedback <- newFeedback
    searchDDMinWithCert
        tracker
        cert
        closureAware
        policy
        feedback
        buildCandidate
        pool
        baseSQL
        allStatements
        initialStatements

type CandidateBuilder = IntSet.IntSet -> IntSet.IntSet -> IO (Maybe IntSet.IntSet)

plainCandidate :: CandidateBuilder
plainCandidate _ keptInts = pure (Just keptInts)

-- | Classified seeds: previously accepted seeds commit; fresh seeds test in order.
data SeedToken
    = SeedAccepted ![Statement] !Int
    | SeedFresh [Statement] !Int !ByteString

-- | Per-pass adaptive scheduling state carried across accepted reductions.
data LaneState = LaneState
    { lsLastAcceptedLane :: !(Maybe StatementLane)
    , lsLaneStreak :: !Int
    , lsResumeAnchor :: !(Maybe (StatementId, Int))
    }

emptyLaneState :: LaneState
emptyLaneState = LaneState Nothing 0 Nothing

data WindowCandidate = WindowCandidate
    { wcLane :: !StatementLane
    , wcTag :: !FamilyTag
    , wcEstimatedSaved :: !Int
    , wcProjectedSaved :: !Int
    , wcToken :: !SeedToken
    }

dedupIntSets :: [IntSet.IntSet] -> [IntSet.IntSet]
dedupIntSets = go HashSet.empty
    where
        go _ [] = []
        go seen (s : rest)
            | HashSet.member s seen = go seen rest
            | otherwise = s : go (HashSet.insert s seen) rest

mergeWeightedLanes :: [(Int, [a])] -> [a]
mergeWeightedLanes = go False []
    where
        go _ [] [] = []
        go progressed acc []
            | progressed = go False [] (reverse acc)
            | otherwise = []
        go progressed acc ((q, xs) : rest) = case splitAt (max 1 q) xs of
            ([], _) -> go progressed acc rest
            (taken, remaining) -> taken ++ go True ((q, remaining) : acc) rest

orderWindowCandidates :: ProjectionWindowOrder -> [WindowCandidate] -> [WindowCandidate]
orderWindowCandidates WindowOrderPreserve items = items
orderWindowCandidates WindowOrderCrossEstimated items = sortOn (Down . wcEstimatedSaved) items
orderWindowCandidates WindowOrderCrossProjected items = sortOn (Down . wcProjectedSaved) items
orderWindowCandidates WindowOrderWithinLaneProjected items =
    refill (map wcLane items) sortedC sortedD sortedS
    where
        sortedFor lane = sortOn (Down . wcProjectedSaved) [c | c <- items, wcLane c == lane]
        sortedC = sortedFor LaneComplement
        sortedD = sortedFor LaneSingleton
        sortedS = sortedFor LaneSubset
        refill [] _ _ _ = []
        refill (LaneComplement : rest) (c : cs) ds ss = c : refill rest cs ds ss
        refill (LaneSingleton : rest) cs (d : ds) ss = d : refill rest cs ds ss
        refill (LaneSubset : rest) cs ds (s : ss) = s : refill rest cs ds ss
        refill (_ : rest) cs ds ss = refill rest cs ds ss

data DDMinEnv = DDMinEnv
    { ddTracker :: !EarlyRejectTracker
    , ddCert :: !EarlyRejectCertifier
    , ddClosureAware :: !Bool
    , ddPolicy :: !StatementSearchPolicy
    , ddFeedback :: !Feedback
    , ddBuildCandidate :: !CandidateBuilder
    , ddPool :: !OraclePool
    , ddBaseSQL :: !ByteString
    , ddAllStatements :: ![Statement]
    }

type DDMinOutcome = Maybe (StatementLane, [Statement], Int)
type ScheduledSeed = (StatementLane, FamilyTag, IntSet.IntSet)
type MixedBatchItem = (StatementLane, FamilyTag, [Statement], Int, ByteString)
type MixedBarrier = (StatementLane, FamilyTag, [Statement], Int)

searchDDMinWithCert ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    Bool ->
    StatementSearchPolicy ->
    Feedback ->
    CandidateBuilder ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO [Statement]
searchDDMinWithCert tracker cert closureAware policy feedback buildCandidate pool baseSQL allStatements initial =
    ddminLoop env 2 emptyLaneState initialLen initial
    where
        effectivePolicy = case policy of
            StatementRankedFallback _ -> StatementByteWeighted
            other -> other
        env =
            DDMinEnv
                { ddTracker = tracker
                , ddCert = cert
                , ddClosureAware = closureAware
                , ddPolicy = effectivePolicy
                , ddFeedback = feedback
                , ddBuildCandidate = buildCandidate
                , ddPool = pool
                , ddBaseSQL = baseSQL
                , ddAllStatements = allStatements
                }
        initialLen = BS.length (cutRemovedStatementInts baseSQL allStatements (statementIdsInt initial))

ddminLoop :: DDMinEnv -> Int -> LaneState -> Int -> [Statement] -> IO [Statement]
ddminLoop _ _ _ _ [] = pure []
ddminLoop env granularity laneState currentLen current =
    tryDDMinSchedule env laneState current currentWeights currentLen schedule
    where
        n = min (length current) (max 1 granularity)
        currentInts = statementIdsInt current
        stmtWithWeights = [(stmt, statementSpanBytes stmt) | stmt <- current]
        currentWeights = IntMap.fromList [(unStatementId (stmtId stmt), w) | (stmt, w) <- stmtWithWeights]
        totalSpanBytes = sum (map snd stmtWithWeights)
        chunks count = case ddPolicy env of
            StatementByteWeighted -> partitionStatementsByWeights count stmtWithWeights totalSpanBytes
            StatementRankedWith _ -> partitionStatementsByWeights count stmtWithWeights totalSpanBytes
            _ -> partitionChunks count current
        lanes count =
            let groups = chunks count
                resume = ddPolicy env == StatementByteWeighted
                orderedGroups
                    | resume
                    , Just (anchor, _) <- lsResumeAnchor laneState =
                        -- Keep every candidate in the pass, wrapping back to earlier groups.
                        let (before, after) = span (all ((<= anchor) . stmtId)) groups
                         in after ++ before
                    | otherwise = groups
             in ( (2, False, tryDDMinSubsets env current currentInts currentLen groups)
                , (max 2 (count - 1), resume, tryDDMinComplements env current currentInts currentLen orderedGroups)
                )
        counts = ddminGranularities (length current) n
        schedule = case ddPolicy env of
            StatementRankedWith cfg ->
                buildRankedSchedule
                    env
                    cfg
                    laneState
                    current
                    currentInts
                    currentWeights
                    currentLen
                    (map chunks counts)
            StatementSubsetFirst ->
                concatMap (\count -> let (subset, complement) = lanes count in [subset, complement]) counts
            _ ->
                concatMap (\count -> let (subset, complement) = lanes count in [complement, subset]) counts

ddminGranularities :: Int -> Int -> [Int]
ddminGranularities limit n
    | n < limit = n : ddminGranularities limit (min limit (2 * n))
    | otherwise = [n]

tryDDMinSchedule ::
    DDMinEnv ->
    LaneState ->
    [Statement] ->
    IntMap.IntMap Int ->
    Int ->
    [(Int, Bool, IO DDMinOutcome)] ->
    IO [Statement]
tryDDMinSchedule _ _ current _ _ [] = pure current
tryDDMinSchedule env laneState current currentWeights currentLen ((next, resume, search) : rest) = do
    reduced <- search
    case reduced of
        Nothing -> tryDDMinSchedule env laneState current currentWeights currentLen rest
        Just (acceptedLane, accepted, saved) ->
            ddminLoop env next nextState (currentLen - saved) accepted
            where
                removedInts = statementIdsInt current `IntSet.difference` statementIdsInt accepted
                anchor
                    | resume
                    , Just (removedId, remaining) <- IntSet.minView removedInts
                    , IntSet.null remaining =
                        Just (StatementId removedId, IntMap.findWithDefault 0 removedId currentWeights)
                    | otherwise = Nothing
                nextStreak
                    | lsLastAcceptedLane laneState == Just acceptedLane = lsLaneStreak laneState + 1
                    | otherwise = 1
                nextState =
                    LaneState
                        { lsLastAcceptedLane = Just acceptedLane
                        , lsLaneStreak = nextStreak
                        , lsResumeAnchor = anchor
                        }

-- Unified ranked scheduler: orthogonal lane ordering, lane quota
-- scheduling, singleton resumption, and optional bounded projection window.
buildRankedSchedule ::
    DDMinEnv ->
    RankedScheduleConfig ->
    LaneState ->
    [Statement] ->
    IntSet.IntSet ->
    IntMap.IntMap Int ->
    Int ->
    [[[Statement]]] ->
    [(Int, Bool, IO DDMinOutcome)]
buildRankedSchedule env cfg laneState current currentInts weights currentLen partitions =
    case (ddClosureAware env, rankedProjectionWindow cfg) of
        (True, ProjectionWindow w order) ->
            [ (2, resumes, tryProjectedWindow env w order retainedBytes current currentInts currentLen scheduled)
            ]
        _ ->
            [(2, resumes, tryScheduledStream env current currentInts currentLen scheduled)]
    where
        resumes = rankedSingletonResume cfg /= ResumeNone
        weightOf sid = IntMap.findWithDefault 0 sid weights
        retainedBytes = IntSet.foldl' (\total sid -> total + weightOf sid) 0
        rankLane lane tag seeds =
            let nonTrivial = dedupIntSets [seed | seed <- seeds, not (IntSet.null seed), seed /= currentInts]
                ordered = case rankedLaneOrdering cfg of
                    LaneOrderPartition -> nonTrivial
                    LaneOrderEstimatedBytes -> sortOn retainedBytes nonTrivial
             in [(lane, tag, seed) | seed <- ordered]
        groups = map statementIdsInt (concat partitions)
        complements =
            rankLane
                LaneComplement
                TagStatementComplement
                [currentInts `IntSet.difference` group | group <- groups, IntSet.size group > 1]
        -- Keep smallest subsets first: small subset byte strings remain
        -- invariant across deletions and hit exact-byte memoization.
        subsets = rankLane LaneSubset TagStatementSubset groups
        singletonIds = orderRankedSingletonIds cfg laneState weightOf (IntSet.toList currentInts)
        singletons =
            [(LaneSingleton, TagStatementComplement, IntSet.delete sid currentInts) | sid <- singletonIds]
        laneQueue LaneComplement = complements
        laneQueue LaneSingleton = singletons
        laneQueue LaneSubset = subsets
        scheduled = interleaveRankedLanes cfg laneState resumes laneQueue

orderRankedSingletonIds :: RankedScheduleConfig -> LaneState -> (Int -> Int) -> [Int] -> [Int]
orderRankedSingletonIds cfg laneState weightOf orderedSids =
    case (rankedSingletonResume cfg, lsResumeAnchor laneState) of
        (ResumeCursorOnly, Just (StatementId anchorId, _)) ->
            let (before, after) = span (<= anchorId) orderedSids in after ++ before
        (ResumeWeightStableTies, Just (StatementId anchorId, _)) ->
            let (before, after) = span (<= anchorId) orderedSids
             in sortOn (Down . weightOf) (after ++ before)
        (ResumeWeightMonotonic, Just (StatementId anchorId, anchorWeight)) ->
            let sorted = sortOn (\sid -> (Down (weightOf sid), sid)) orderedSids
                (before, after) =
                    span (\sid -> (Down (weightOf sid), sid) <= (Down anchorWeight, anchorId)) sorted
             in after ++ before
        _ -> case rankedLaneOrdering cfg of
            LaneOrderPartition -> orderedSids
            LaneOrderEstimatedBytes -> sortOn (Down . weightOf) orderedSids

interleaveRankedLanes ::
    RankedScheduleConfig -> LaneState -> Bool -> (StatementLane -> [a]) -> [a]
interleaveRankedLanes cfg laneState resumes laneQueue = case rankedLaneScheduling cfg of
    LaneSerial -> complements ++ subsets ++ singletons
    LaneRoundRobin
        | resumes
        , Just _ <- lsResumeAnchor laneState ->
            mergeWeightedLanes [(1, singletons), (1, complements), (1, subsets)]
        | otherwise ->
            mergeWeightedLanes [(1, complements), (1, subsets), (1, singletons)]
    LaneBoundedPreference rawQuota -> interleaveBoundedPreference (max 1 rawQuota) laneState laneQueue
    where
        complements = laneQueue LaneComplement
        singletons = laneQueue LaneSingleton
        subsets = laneQueue LaneSubset

interleaveBoundedPreference :: Int -> LaneState -> (StatementLane -> [a]) -> [a]
interleaveBoundedPreference maxQuota laneState laneQueue = case lsLastAcceptedLane laneState of
    Nothing ->
        -- Test both halves of the initial 2-way split before singletons/subsets,
        -- then interleave all three lanes fairly.
        let (headC, restC) = splitAt (min 2 maxQuota) (laneQueue LaneComplement)
         in headC ++ mergeWeightedLanes [(1, laneQueue LaneSingleton), (1, laneQueue LaneSubset), (1, restC)]
    Just LaneSingleton ->
        -- Weight-monotonic resumption probes the next singleton first; on a miss,
        -- yield immediately to multi-statement complements and subsets.
        mergeWeightedLanes
            [(1, laneQueue LaneSingleton), (1, laneQueue LaneComplement), (1, laneQueue LaneSubset)]
    Just winLane ->
        -- Coarse winning lanes (Complement / Subset) keep bounded cyclic preference
        -- while preserving guaranteed slots for the other two lanes.
        let winQuota = min maxQuota (1 + lsLaneStreak laneState)
            baseOrder = [LaneComplement, LaneSingleton, LaneSubset]
            others = [(1, laneQueue l) | l <- baseOrder, l /= winLane]
         in mergeWeightedLanes ((winQuota, laneQueue winLane) : others)

-- Unwindowed ranked stream supporting ordered concurrency (`jobs >= 1`).
tryScheduledStream ::
    DDMinEnv -> [Statement] -> IntSet.IntSet -> Int -> [ScheduledSeed] -> IO DDMinOutcome
tryScheduledStream env current currentInts currentLen = pull
    where
        jobs = poolJobCount (ddPool env)
        pull [] = pure Nothing
        pull seeds = collect jobs [] HashSet.empty seeds
        collect _ acc _ [] = testAndContinue (reverse acc) [] Nothing
        collect 0 acc _ rest = testAndContinue (reverse acc) rest Nothing
        collect n acc seen ((lane, tag, seedInts) : rest) = do
            token <- classifyDDMinSeed env Nothing tag current currentInts currentLen seedInts
            case token of
                Nothing -> collect n acc seen rest
                Just (SeedAccepted kept saved) ->
                    testAndContinue (reverse acc) rest (Just (lane, tag, kept, saved))
                Just (SeedFresh kept count sql)
                    | HashSet.member sql seen -> collect n acc seen rest
                    | otherwise ->
                        collect (n - 1) ((lane, tag, kept, count, sql) : acc) (HashSet.insert sql seen) rest
        testAndContinue batch rest barrier =
            resolveMixedBatch env currentLen batch barrier (pull rest)

-- Bound speculative projection and rendering to a fixed window of W seeds.
-- Deduplicates projected IntSets before rendering and isolates window ordering.
tryProjectedWindow ::
    DDMinEnv ->
    Int ->
    ProjectionWindowOrder ->
    (IntSet.IntSet -> Int) ->
    [Statement] ->
    IntSet.IntSet ->
    Int ->
    [ScheduledSeed] ->
    IO DDMinOutcome
tryProjectedWindow env rawSize order retainedBytes current currentInts currentLen = search
    where
        wSize = max 1 rawSize
        jobs = poolJobCount (ddPool env)
        search [] = pure Nothing
        search seeds = do
            let (window, remaining) = splitAt wSize seeds
            seenProjected <- newIORef HashSet.empty
            classified <- forM window (classifyWindowSeed seenProjected)
            runWindow remaining (orderWindowCandidates order (catMaybes classified))
        classifyWindowSeed seenProjected (lane, tag, seed) = do
            token <- classifyDDMinSeed env (Just seenProjected) tag current currentInts currentLen seed
            pure (fmap (mkWindowCandidate lane tag seed) token)
        mkWindowCandidate lane tag seed cand =
            WindowCandidate
                { wcLane = lane
                , wcTag = tag
                , wcEstimatedSaved = currentLen - retainedBytes seed
                , wcProjectedSaved = seedSavedBytes currentLen cand
                , wcToken = cand
                }
        runWindow remaining [] = search remaining
        runWindow remaining items = collectWindow remaining jobs [] HashSet.empty items
        collectWindow remaining _ acc _ [] = testWindow remaining (reverse acc) [] Nothing
        collectWindow remaining 0 acc _ rest = testWindow remaining (reverse acc) rest Nothing
        collectWindow remaining n acc seen (cand : rest) = case wcToken cand of
            SeedAccepted kept saved ->
                testWindow remaining (reverse acc) rest (Just (wcLane cand, wcTag cand, kept, saved))
            SeedFresh kept count sql -> do
                tested <- readIORef (fbEvaluated (ddFeedback env))
                if HashMap.member sql tested || HashSet.member sql seen
                    then do
                        recordGeneratedSkipped (poolMetricsStore (ddPool env)) "projected-window-duplicate"
                        collectWindow remaining n acc seen rest
                    else
                        collectWindow
                            remaining
                            (n - 1)
                            ((wcLane cand, wcTag cand, kept, count, sql) : acc)
                            (HashSet.insert sql seen)
                            rest
        testWindow remaining batch rest barrier =
            resolveMixedBatch env currentLen batch barrier (runWindow remaining rest)

seedSavedBytes :: Int -> SeedToken -> Int
seedSavedBytes _ (SeedAccepted _ saved) = saved
seedSavedBytes currentLen (SeedFresh _ _ sql) = currentLen - BS.length sql

resolveMixedBatch ::
    DDMinEnv -> Int -> [MixedBatchItem] -> Maybe MixedBarrier -> IO DDMinOutcome -> IO DDMinOutcome
resolveMixedBatch env currentLen batch barrier onMiss = do
    accepted <- testMixedBatch env currentLen batch
    case (accepted, barrier) of
        (Just result, _) -> pure (Just result)
        (Nothing, Just (lane, tag, kept, saved)) -> do
            recordAcceptedBytesSaved (poolMetricsStore (ddPool env)) tag saved
            pure (Just (lane, kept, saved))
        (Nothing, Nothing) -> onMiss

tryDDMinSubsets ::
    DDMinEnv -> [Statement] -> IntSet.IntSet -> Int -> [[Statement]] -> IO DDMinOutcome
tryDDMinSubsets env current currentInts currentLen chunks =
    tryDDMinSeeds
        env
        LaneSubset
        TagStatementSubset
        current
        currentInts
        currentLen
        (map statementIdsInt chunks)

tryDDMinComplements ::
    DDMinEnv -> [Statement] -> IntSet.IntSet -> Int -> [[Statement]] -> IO DDMinOutcome
tryDDMinComplements env current currentInts currentLen chunks =
    tryDDMinSeeds
        env
        LaneComplement
        TagStatementComplement
        current
        currentInts
        currentLen
        [currentInts `IntSet.difference` statementIdsInt chunk | chunk <- chunks]

tryDDMinSeeds ::
    DDMinEnv ->
    StatementLane ->
    FamilyTag ->
    [Statement] ->
    IntSet.IntSet ->
    Int ->
    [IntSet.IntSet] ->
    IO DDMinOutcome
tryDDMinSeeds env lane tag current currentInts currentLen seeds = do
    seenProjections <- newIORef HashSet.empty
    pendingBytes <- newIORef HashSet.empty
    cachedAcceptance <- newIORef Nothing
    let pool = ddPool env
        next [] = pure Nothing
        next (seed : rest) = do
            token <- classifyDDMinSeed env (Just seenProjections) tag current currentInts currentLen seed
            case token of
                Nothing -> next rest
                Just (SeedAccepted kept saved) ->
                    writeIORef cachedAcceptance (Just (kept, saved)) >> pure Nothing
                Just (SeedFresh kept count bytes) -> do
                    pending <- readIORef pendingBytes
                    if HashSet.member bytes pending
                        then next rest
                        else do
                            writeIORef pendingBytes (HashSet.insert bytes pending)
                            reportCandidateCount pool bytes count
                            pure (Just ((tag, bytes, kept), rest))
        retire _ kept bytes (outcome, _) = do
            modifyIORef' pendingBytes (HashSet.delete bytes)
            recordDDMinVerdict env bytes outcome
            pure (if outcome == OracleAccepted then Just (kept, currentLen - BS.length bytes) else Nothing)
    tested <- testOrderedGenerated pool next retire Nothing seeds
    accepted <- maybe (readIORef cachedAcceptance) (pure . Just) tested
    traverse (commitSeedAcceptance pool lane tag) accepted

commitSeedAcceptance ::
    OraclePool ->
    StatementLane ->
    FamilyTag ->
    ([Statement], Int) ->
    IO (StatementLane, [Statement], Int)
commitSeedAcceptance pool lane tag (kept, saved) = do
    recordAcceptedBytesSaved (poolMetricsStore pool) tag saved
    pure (lane, kept, saved)

testMixedBatch :: DDMinEnv -> Int -> [MixedBatchItem] -> IO DDMinOutcome
testMixedBatch _ _ [] = pure Nothing
testMixedBatch env currentLen batch = do
    let pool = ddPool env
    mapM_ (\(_, _, _, count, sql) -> reportCandidateCount pool sql count) batch
    outcomes <- testOrderedTagged pool [(tag, sql) | (_, tag, _, _, sql) <- batch]
    let retired = zip batch outcomes
    mapM_ (\((_, _, _, _, sql), (outcome, _)) -> recordDDMinVerdict env sql outcome) retired
    case listToMaybe [candidate | (candidate, (outcome, _)) <- retired, outcome == OracleAccepted] of
        Just (lane, tag, kept, _, sql) -> do
            let saved = currentLen - BS.length sql
            recordAcceptedBytesSaved (poolMetricsStore pool) tag saved
            pure (Just (lane, kept, saved))
        Nothing -> pure Nothing

recordDDMinVerdict :: DDMinEnv -> ByteString -> OracleOutcome -> IO ()
recordDDMinVerdict env sql outcome =
    modifyIORef' (fbEvaluated (ddFeedback env)) (HashMap.insert sql verdict)
    where
        verdict = if outcome == OracleAccepted then CandidateAccepted else CandidateRejected

classifyDDMinSeed ::
    DDMinEnv ->
    Maybe (IORef (HashSet.HashSet IntSet.IntSet)) ->
    FamilyTag ->
    [Statement] ->
    IntSet.IntSet ->
    Int ->
    IntSet.IntSet ->
    IO (Maybe SeedToken)
classifyDDMinSeed env maybeSeen tag current currentInts currentLen seedInts
    | seedInts == currentInts = pure Nothing
    | otherwise = evalContT do
        let pool = ddPool env
            store = poolMetricsStore pool
            feedback = ddFeedback env
        projection <- liftIO $ ddBuildCandidate env currentInts seedInts
        projected <- case projection of
            Nothing -> earlyExit Nothing
            Just p
                | p == currentInts -> do
                    liftIO $ recordGeneratedSkipped store "closure-projection-no-shrink"
                    earlyExit Nothing
                | otherwise -> pure p
        dupInWindow <- liftIO $ checkAndInsertSeenProjection maybeSeen projected
        when dupInWindow do
            liftIO do
                recordGeneratedSkipped store "projected-window-duplicate"
                when (projected /= seedInts) $
                    recordGeneratedSkipped store "closure-projection-duplicate"
            earlyExit Nothing

        earlyId <- liftIO $ checkEarlyRejectionIdsTracked (ddTracker env) (ddCert env) projected
        for_ earlyId \reason -> do
            liftIO $ recordEarlyRejectSkip store reason
            earlyExit Nothing

        let kept = filter (\s -> IntSet.member (unStatementId (stmtId s)) projected) current
        candidateSQL <-
            liftIO $
                timeTaggedStatementBuild
                    store
                    tag
                    (cutRemovedStatementInts (ddBaseSQL env) (ddAllStatements env) projected)
        let saved = currentLen - BS.length candidateSQL
        when (saved <= 0) do
            liftIO $
                when (projected /= seedInts) $
                    recordGeneratedSkipped store "closure-projection-no-shrink"
            earlyExit Nothing

        evaluated <- liftIO $ readIORef (fbEvaluated feedback)
        case HashMap.lookup candidateSQL evaluated of
            Just CandidateAccepted -> earlyExit (Just (SeedAccepted kept saved))
            Just CandidateRejected -> do
                liftIO do
                    recordGeneratedSkipped store "duplicate-bytes"
                    when (projected /= seedInts) $
                        recordGeneratedSkipped store "closure-projection-duplicate"
                earlyExit Nothing
            Nothing -> pure ()

        earlyBytes <-
            liftIO $ checkEarlyRejectionBytesTracked (ddTracker env) (ddCert env) False candidateSQL
        for_ earlyBytes \reason -> do
            liftIO do
                recordEarlyRejectSkip store reason
                modifyIORef' (fbEvaluated feedback) (HashMap.insert candidateSQL CandidateRejected)
            earlyExit Nothing

        let count = IntSet.size (IntSet.intersection currentInts projected)
        pure (Just (SeedFresh kept count candidateSQL))

checkAndInsertSeenProjection ::
    Maybe (IORef (HashSet.HashSet IntSet.IntSet)) -> IntSet.IntSet -> IO Bool
checkAndInsertSeenProjection Nothing _ = pure False
checkAndInsertSeenProjection (Just seenRef) projected = do
    seen <- readIORef seenRef
    if HashSet.member projected seen
        then pure True
        else writeIORef seenRef (HashSet.insert projected seen) >> pure False

-- | Graph-free statement ddmin over original top-level statement byte spans.
statementDdmin :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
statementDdmin = statementDdminWithOrder StatementSubsetFirst

statementDdminWithOrder ::
    StatementSearchPolicy -> SQLDialect -> OraclePool -> ByteString -> IO ByteString
statementDdminWithOrder policy dialect pool =
    statementDdminWithParser policy (parseStatementsFast (poolMetricsStore pool) dialect) pool

statementDdminWithParser ::
    StatementSearchPolicy -> StatementParser -> OraclePool -> ByteString -> IO ByteString
statementDdminWithParser policy parse pool initialSQL = do
    tracker <- newEarlyRejectTracker
    statementDdminWithCert False tracker Nothing policy parse pool initialSQL

statementDdminWithCert ::
    Bool ->
    EarlyRejectTracker ->
    Maybe ResultTrace ->
    StatementSearchPolicy ->
    StatementParser ->
    OraclePool ->
    ByteString ->
    IO ByteString
statementDdminWithCert earlyReject tracker mTrace policy parse pool initialSQL = do
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    if not initialOK
        then pure initialSQL
        else do
            (_, units) <- parse initialSQL
            let allStatements = map unitStatement units
            cert <- buildCertifierTracked earlyReject tracker initialSQL units mTrace
            reduced <-
                searchStatementsWithCert
                    tracker
                    cert
                    False
                    policy
                    plainCandidate
                    pool
                    initialSQL
                    allStatements
                    allStatements
            pure (statementSQL initialSQL allStatements reduced)

-- | Repeated single-statement deletion until reaching a 1-minimal fixed point.
statementAudit :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
statementAudit dialect pool initialSQL = fst <$> statementAuditWithLogger nullLogger dialect pool initialSQL

statementAuditWithLogger ::
    Logger -> SQLDialect -> OraclePool -> ByteString -> IO (ByteString, AuditStats)
statementAuditWithLogger logger dialect pool =
    statementAuditWithParser logger (parseStatementsFast (poolMetricsStore pool) dialect) pool

statementAuditWithParser ::
    Logger -> StatementParser -> OraclePool -> ByteString -> IO (ByteString, AuditStats)
statementAuditWithParser logger parse pool initialSQL = do
    tracker <- newEarlyRejectTracker
    statementAuditWithCert False tracker Nothing logger parse pool initialSQL

data AuditEnv = AuditEnv
    { auditTracker :: !EarlyRejectTracker
    , auditCert :: !EarlyRejectCertifier
    , auditPool :: !OraclePool
    , auditBaseSQL :: !ByteString
    , auditAllStatements :: ![Statement]
    }

type PrevDeletion = Maybe (Int, Int, ByteString)

statementAuditWithCert ::
    Bool ->
    EarlyRejectTracker ->
    Maybe ResultTrace ->
    Logger ->
    StatementParser ->
    OraclePool ->
    ByteString ->
    IO (ByteString, AuditStats)
statementAuditWithCert earlyReject tracker mTrace logger parse pool initialSQL = do
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    outcome <-
        if initialOK
            then runConfirmedAudit
            else pure (initialSQL, AuditStats 0 0 0 False)
    logAuditStats logger (snd outcome)
    pure outcome
    where
        runConfirmedAudit = do
            (_, units) <- parse initialSQL
            let statements = map unitStatement units
                !initialKept = statementIdsInt statements
                !initialCount = length statements
            cert <- buildCertifierTracked earlyReject tracker initialSQL units mTrace
            let env = AuditEnv tracker cert pool initialSQL statements
            auditLoop env initialKept initialCount 0 0 0

-- The audit only removes whole statements, and those statements keep
-- their original byte spans. Parse once, then track the retained set.
auditLoop ::
    AuditEnv -> IntSet.IntSet -> Int -> Int -> Int -> Int -> IO (ByteString, AuditStats)
auditLoop env kept !keptCount passCount attemptCount removalCount
    | keptCount <= 0 || IntSet.null kept =
        pure (renderAuditKept env kept, AuditStats passCount attemptCount removalCount True)
    | otherwise = do
        (nextKept, nextKeptCount, attempts, removals) <- runAuditPass env kept keptCount
        let nextPassCount = passCount + 1
            totalAttempts = attemptCount + attempts
        if removals > 0
            then auditLoop env nextKept nextKeptCount nextPassCount totalAttempts (removalCount + removals)
            else pure (renderAuditKept env nextKept, AuditStats nextPassCount totalAttempts removalCount True)

runAuditPass :: AuditEnv -> IntSet.IntSet -> Int -> IO (IntSet.IntSet, Int, Int, Int)
runAuditPass env kept keptCount =
    go initSnap kept keptCount (auditAllStatements env) 0 0
    where
        !initSnap =
            buildStatementSnapshot (auditBaseSQL env) (auditAllStatements env) kept (renderAuditKept env kept)
        retire (!tested, _) (sid, rest) bytes (outcome, _) =
            pure (tested + 1, if outcome == OracleAccepted then Just (sid, rest, bytes) else Nothing)
        go snap currentKept !currentCount statements !attempts !removals = do
            (tested, accepted) <-
                testOrderedGenerated
                    (auditPool env)
                    (nextAuditCandidate env snap currentKept (currentCount - 1))
                    retire
                    (0, Nothing)
                    (Nothing, statements)
            case accepted of
                Nothing -> pure (currentKept, currentCount, attempts + tested, removals)
                Just (sid, rest, bytes) -> do
                    let nextKept = IntSet.delete sid currentKept
                        nextSnap = buildStatementSnapshot (auditBaseSQL env) (auditAllStatements env) nextKept bytes
                    go nextSnap nextKept (currentCount - 1) rest (attempts + tested) (removals + 1)

nextAuditCandidate ::
    AuditEnv ->
    StatementSnapshot ->
    IntSet.IntSet ->
    Int ->
    (PrevDeletion, [Statement]) ->
    IO (Maybe ((FamilyTag, ByteString, (Int, [Statement])), (PrevDeletion, [Statement])))
nextAuditCandidate _ _ _ _ (_, []) = pure Nothing
nextAuditCandidate env snap currentKept count (previous, statement : rest)
    | IntSet.notMember sid currentKept = nextAuditCandidate env snap currentKept count (previous, rest)
    | otherwise = do
        (nextLast, candidate) <- evaluateAuditCandidate env snap currentKept statement sid previous
        case candidate of
            Nothing -> nextAuditCandidate env snap currentKept count (nextLast, rest)
            Just bytes -> do
                reportCandidateCount (auditPool env) bytes count
                pure (Just ((TagStatement, bytes, (sid, rest)), (nextLast, rest)))
    where
        sid = unStatementId (stmtId statement)

evaluateAuditCandidate ::
    AuditEnv ->
    StatementSnapshot ->
    IntSet.IntSet ->
    Statement ->
    Int ->
    PrevDeletion ->
    IO (PrevDeletion, Maybe ByteString)
evaluateAuditCandidate env snap currentKept statement sid previous = evalContT do
    let store = poolMetricsStore (auditPool env)
        keptAfter = IntSet.delete sid currentKept
    earlyId <- liftIO $ checkEarlyRejectionIdsTracked (auditTracker env) (auditCert env) keptAfter
    for_ earlyId \reason -> do
        liftIO $ recordEarlyRejectSkip store reason
        earlyExit (previous, Nothing)
    (bytes, nextLast) <- liftIO $ renderAuditCandidate env snap statement sid keptAfter previous
    when (BS.length bytes >= BS.length (snapSQL snap)) do
        liftIO $ recordGeneratedSkipped store "nonshrinking"
        earlyExit (nextLast, Nothing)
    earlyBytes <-
        liftIO $ checkEarlyRejectionBytesTracked (auditTracker env) (auditCert env) False bytes
    for_ earlyBytes \reason -> do
        liftIO $ recordEarlyRejectSkip store reason
        earlyExit (nextLast, Nothing)
    pure (nextLast, Just bytes)

renderAuditCandidate ::
    AuditEnv ->
    StatementSnapshot ->
    Statement ->
    Int ->
    IntSet.IntSet ->
    PrevDeletion ->
    IO (ByteString, PrevDeletion)
renderAuditCandidate env snap statement sid keptAfter previous =
    case (previous, IntMap.lookup sid (snapOffsets snap)) of
        (Just (prevOff, prevLen, bytes), Just (off, len))
            | off == prevOff + prevLen
            , len == prevLen
            , deletedBytes off len == deletedBytes prevOff prevLen ->
                pure (bytes, Just (off, len, bytes))
        (_, extent) -> do
            let fallback = renderAuditKept env keptAfter
                raw = fromMaybe fallback (renderSingletonDeletion snap statement)
            bytes <- timeStatementBuild (poolMetricsStore (auditPool env)) raw
            pure (bytes, fmap (\(off, len) -> (off, len, bytes)) extent)
    where
        deletedBytes off len = BS.take len (BS.drop off (snapSQL snap))

renderAuditKept :: AuditEnv -> IntSet.IntSet -> ByteString
renderAuditKept env = cutRemovedStatementInts (auditBaseSQL env) (auditAllStatements env)

data AuditStats = AuditStats
    { auditPasses :: !Int
    , auditAttempts :: !Int
    , auditRemovals :: !Int
    , auditMinimalityConfirmed :: !Bool
    }

logAuditStats :: Logger -> AuditStats -> IO ()
logAuditStats logger stats =
    let removals = auditRemovals stats
     in if removals > 0
            then
                logInfo'
                    logger
                    "reducer"
                    "Statement deletion audit reduced statements"
                    [ ("statements_removed", toJSON removals)
                    , ("audit_attempts", toJSON (auditAttempts stats))
                    , ("statement_1_minimal", toJSON (auditMinimalityConfirmed stats))
                    ]
            else
                logDebug'
                    logger
                    "reducer"
                    "Statement deletion audit confirmed 1-minimality"
                    [ ("audit_attempts", toJSON (auditAttempts stats))
                    , ("statement_1_minimal", toJSON (auditMinimalityConfirmed stats))
                    ]

-- | Binary search for the shortest interesting prefix of statements.
findInterestingPrefix ::
    OraclePool -> ByteString -> [Statement] -> IO (Maybe (Statement, ByteString, Int), Int)
findInterestingPrefix pool baseSQL statements =
    findInterestingPrefixThrough pool baseSQL statements (length statements)

findInterestingPrefixThrough ::
    OraclePool -> ByteString -> [Statement] -> Int -> IO (Maybe (Statement, ByteString, Int), Int)
findInterestingPrefixThrough pool baseSQL statements limit = do
    tracker <- newEarlyRejectTracker
    findInterestingPrefixThroughWithCert tracker emptyEarlyRejectCertifier pool baseSQL statements limit

findInterestingPrefixThroughWithCert ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    Int ->
    IO (Maybe (Statement, ByteString, Int), Int)
findInterestingPrefixThroughWithCert tracker cert pool baseSQL statements limit =
    go 1 limit Nothing 0
    where
        go !low !high best !tests
            | low > high = pure (best, tests)
            | otherwise = do
                let mid = low + (high - low) `div` 2
                    prefix = take mid statements
                probeInterestingPrefix tracker cert pool baseSQL statements prefix >>= \case
                    Nothing -> go (mid + 1) high best tests
                    Just (True, candidate) ->
                        let nextBest = maybe best (\stmt -> Just (stmt, candidate, mid)) (lastMay prefix)
                         in go low (mid - 1) nextBest (tests + 1)
                    Just (False, _) -> go (mid + 1) high best (tests + 1)

rejectIfEarly :: Maybe MetricsStore -> Maybe EarlyRejectReason -> MaybeT IO ()
rejectIfEarly _ Nothing = pure ()
rejectIfEarly store (Just reason) = liftIO (recordEarlyRejectSkip store reason) >> empty

probeInterestingPrefix ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO (Maybe (Bool, ByteString))
probeInterestingPrefix tracker cert pool baseSQL statements prefix = runMaybeT do
    let store = poolMetricsStore pool
    rejectIfEarly store =<< liftIO (checkEarlyRejectionIdsTracked tracker cert (statementIdsInt prefix))
    candidate <- liftIO $ timeStatementBuild store (statementSQL baseSQL statements prefix)
    rejectIfEarly store =<< liftIO (checkEarlyRejectionBytesTracked tracker cert False candidate)
    liftIO do
        reportCandidateCount pool candidate (length prefix)
        ok <- oracleAccepts (poolOracle pool) candidate
        pure (ok, candidate)

-- | Parsed statements plus their dependency graph for one exact candidate byte string.
data SQLAnalysis = SQLAnalysis
    { analyzedUnits :: ![StatementUnit]
    , analyzedStatements :: ![Statement]
    , analyzedGraph :: !DependencyGraph
    , analyzedSession :: !SessionInit
    }

analyzeSQL :: StatementParser -> Logger -> SQLDialect -> ByteString -> IO SQLAnalysis
analyzeSQL parse logger dialect sql = do
    started <- getMonotonicTimeNSec
    (initialSession, units) <- parse sql
    let statements = map unitStatement units
        graph = analyzeStatements initialSession units
    graph `seq` pure ()
    finished <- getMonotonicTimeNSec
    let elapsed = finished - started
        statementCount = length statements
        edgeCount = graphEdgeCount graph
        barrierActions = graphBarrierActions graph
        barrierStatements = graphBarrierStatements graph
        barrierEdges = graphBarrierEdges graph
    logDebug'
        logger
        "reducer"
        "Dependency analysis completed"
        [ ("analysis_seconds", toJSON (seconds elapsed))
        , ("statements", toJSON statementCount)
        , ("dependency_edges", toJSON edgeCount)
        , ("barrier_actions", toJSON barrierActions)
        , ("barrier_statements", toJSON barrierStatements)
        , ("barrier_edges", toJSON barrierEdges)
        , ("dialect", toJSON (T.pack (show dialect)))
        ]
    pure
        SQLAnalysis
            { analyzedUnits = units
            , analyzedStatements = statements
            , analyzedGraph = graph
            , analyzedSession = initialSession
            }

data SliceEnv = SliceEnv
    { seTracker :: !EarlyRejectTracker
    , seCert :: !EarlyRejectCertifier
    , seLogger :: !Logger
    , sePool :: !OraclePool
    , seAnalysis :: !SQLAnalysis
    , seSQL :: !ByteString
    }

-- | Slice SQL statements using a precomputed dependency analysis.
sliceWithAnalysis ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    Logger ->
    OraclePool ->
    SQLAnalysis ->
    Maybe ResultStatement ->
    ByteString ->
    IO (ByteString, Maybe SQLAnalysis, Maybe [Statement])
sliceWithAnalysis tracker cert logger pool analysis observedResult sql =
    case seedTarget of
        Nothing -> keepSlice sql Nothing
        Just seedStatement -> do
            let (conservativeRetained, conservativeBytes) = sliceFor graph seedStatement
                useRelaxed =
                    BS.length conservativeBytes >= BS.length sql
                        && graphHasGlobalBarrier graph
                        && isJust resultTarget
                primary
                    | useRelaxed = sliceFor proposalGraph seedStatement
                    | otherwise = (conservativeRetained, conservativeBytes)
                primaryStrategy
                    | useRelaxed = "result-statement-without-global-barriers"
                    | otherwise = seedStrategy
            primaryAccepted <- trySliceSeed env primary
            (seedAccepted, (seedRetained, seedCandidate), strategy) <-
                if useRelaxed && not primaryAccepted
                    then tryRelaxedOpaqueSeed env proposalGraph seedStatement primary
                    else pure (primaryAccepted, primary, primaryStrategy)
            let prefixLimit = case (seedAccepted, resultTarget) of
                    (True, Just (index, _)) -> index
                    _ -> length statements
                initialBest = if seedAccepted then Just (seedCandidate, seedRetained) else Nothing
            better <- tryPrefixSlice env prefixLimit initialBest
            case better of
                Just (result, retained)
                    | Just result /= fmap fst initialBest -> keepSlice result (Just retained)
                _
                    | seedAccepted -> do
                        accepted <- acceptBackwardSlice env strategy seedStatement seedRetained seedCandidate
                        keepSlice accepted (Just seedRetained)
                    | otherwise -> keepSlice sql Nothing
    where
        env = SliceEnv tracker cert logger pool analysis sql
        statements = analyzedStatements analysis
        graph = analyzedGraph analysis
        resultTarget = do
            ResultStatement count index <- observedResult
            -- The engine and frontend can recognize different statement boundaries.
            guard (count == length statements && index >= 0)
            (index,) <$> listToMaybe (drop index statements)
        (seedTarget, seedStrategy) = case resultTarget of
            Just (_, stmt) -> (Just stmt, "result-statement")
            Nothing -> (lastMay statements, "last-statement")
        keepSlice result retained =
            pure (result, if result == sql then Just analysis else Nothing, retained)
        -- Use the relaxed graph only to propose candidate bytes. A global
        -- barrier can carry real effects, so the oracle still decides.
        proposalGraph = buildRelaxedGraph (analyzedSession analysis) statements graph
        sliceFor dependencyGraph target =
            renderSliceUnits sql statements (backwardSliceFast (unStatementId (stmtId target)) dependencyGraph)

renderSliceUnits :: ByteString -> [Statement] -> IntSet.IntSet -> ([Statement], ByteString)
renderSliceUnits sql statements sliceInts =
    ( filter (\s -> IntSet.member (unStatementId (stmtId s)) sliceInts) statements
    , cutRemovedStatementInts sql statements sliceInts
    )

trySliceSeed :: SliceEnv -> ([Statement], ByteString) -> IO Bool
trySliceSeed env (retained, candidateRaw) = fmap (fromMaybe False) . runMaybeT $ do
    let pool = sePool env
        store = poolMetricsStore pool
        keptInts = statementIdsInt retained
    rejectIfEarly store =<< liftIO (checkEarlyRejectionIdsTracked (seTracker env) (seCert env) keptInts)
    candidate <- liftIO $ timeStatementBuild store candidateRaw
    guard (BS.length candidate < BS.length (seSQL env))
    rejectIfEarly store
        =<< liftIO (checkEarlyRejectionBytesTracked (seTracker env) (seCert env) False candidate)
    liftIO do
        reportCandidateCount pool candidate (length retained)
        oracleAccepts (poolOracle pool) candidate

tryRelaxedOpaqueSeed ::
    SliceEnv ->
    DependencyGraph ->
    Statement ->
    ([Statement], ByteString) ->
    IO (Bool, ([Statement], ByteString), String)
tryRelaxedOpaqueSeed env proposalGraph seedStatement primary = do
    let statements = analyzedStatements (seAnalysis env)
        graph = analyzedGraph (seAnalysis env)
        opaqueRoots =
            IntSet.fromList
                [ unStatementId (stmtId stmt)
                | stmt <- statements
                , Just facts <- [Map.lookup (stmtId stmt) (graphFacts graph)]
                , any ((== GlobalScope) . barrierScope) (factBarriers facts)
                ]
        roots = IntSet.insert (unStatementId (stmtId seedStatement)) opaqueRoots
        retainingOpaque =
            renderSliceUnits (seSQL env) statements (backwardSliceManyFast roots proposalGraph)
    accepted <-
        if snd retainingOpaque == snd primary
            then pure False
            else trySliceSeed env retainingOpaque
    pure (accepted, retainingOpaque, "result-statement-with-opaque-roots")

tryPrefixSlice ::
    SliceEnv ->
    Int ->
    Maybe (ByteString, [Statement]) ->
    IO (Maybe (ByteString, [Statement]))
tryPrefixSlice env prefixLimit bestCandidate
    | Just (best, _) <- bestCandidate
    , prefixSlicesCannotImprove (seSQL env) statements prefixLimit (BS.length best) = do
        recordGeneratedSkipped (poolMetricsStore pool) "prefix-search-cannot-improve"
        pure bestCandidate
    | otherwise = fmap (<|> bestCandidate) . runMaybeT $ do
        (maybeTarget, prefixTests) <-
            liftIO $
                findInterestingPrefixThroughWithCert
                    (seTracker env)
                    (seCert env)
                    pool
                    (seSQL env)
                    statements
                    prefixLimit
        (targetStatement, prefixSQL, prefixLength) <- hoistMaybe maybeTarget
        let (retained, candidateRaw) =
                renderSliceUnits
                    (seSQL env)
                    statements
                    (backwardSliceFast (unStatementId (stmtId targetStatement)) (analyzedGraph (seAnalysis env)))
        candidate <- liftIO $ timeStatementBuild (poolMetricsStore pool) candidateRaw
        let bestLen = maybe (BS.length (seSQL env)) (BS.length . fst) bestCandidate
        when (BS.length candidate >= bestLen) do
            liftIO $ recordGeneratedSkipped (poolMetricsStore pool) "prefix-slice-cannot-improve"
            empty
        let reusedPrefixResult = candidate == prefixSQL
        ok <-
            liftIO $
                if reusedPrefixResult
                    then pure True
                    else do
                        reportCandidateCount pool candidate (length retained)
                        oracleAccepts (poolOracle pool) candidate
        liftIO $
            logDebug'
                (seLogger env)
                "reducer"
                "Prefix oracle result considered for backward slice"
                [ ("prefix_tests", toJSON prefixTests)
                , ("prefix_length", toJSON prefixLength)
                , ("prefix_result_reused", toJSON reusedPrefixResult)
                ]
        guard ok
        accepted <-
            liftIO $ acceptBackwardSlice env "first-interesting-prefix" targetStatement retained candidate
        pure (accepted, retained)
    where
        pool = sePool env
        statements = analyzedStatements (seAnalysis env)

-- Every backward slice retains its root and therefore its source bytes through
-- the next statement start. If no root can beat the incumbent, the prefix
-- search cannot contribute an improvement.
prefixSlicesCannotImprove :: ByteString -> [Statement] -> Int -> Int -> Bool
prefixSlicesCannotImprove sql statements limit incumbent =
    and (zipWith (<=) (0 : starts) (starts ++ [BS.length sql]))
        && all (>= incumbent) (take limit (zipWith (-) ends starts))
    where
        starts = map (spanStartByte . stmtSpan) statements
        ends = drop 1 starts ++ [BS.length sql]

acceptBackwardSlice :: SliceEnv -> String -> Statement -> [Statement] -> ByteString -> IO ByteString
acceptBackwardSlice env strategy target retained candidate = do
    let StatementId targetId = stmtId target
    logInfo'
        (seLogger env)
        "reducer"
        "Backward slicing accepted"
        [ ("seed_strategy", toJSON strategy)
        , ("target_statement_id", toJSON targetId)
        , ("statements_before", toJSON (length (analyzedStatements (seAnalysis env))))
        , ("statements_after", toJSON (length retained))
        , ("bytes_before", toJSON (BS.length (seSQL env)))
        , ("bytes_after", toJSON (BS.length candidate))
        ]
    pure candidate

data GroupProposalEnv = GroupProposalEnv
    { gpTracker :: !EarlyRejectTracker
    , gpCert :: !EarlyRejectCertifier
    , gpLogger :: !Logger
    , gpPool :: !OraclePool
    , gpBaseSQL :: !ByteString
    , gpStatements :: ![Statement]
    , gpCurrentInts :: !(IORef IntSet.IntSet)
    , gpCurrentSQL :: !(IORef ByteString)
    , gpSeen :: !(IORef (HashSet.HashSet ByteString))
    }

{- | Probe tiered noncontiguous deletion proposals derived from dependency
components, relation lifecycles, and transaction/savepoint boundaries.
Rejected groups leave their members eligible for contiguous search and audit.
-}
proposeGroupDeletionsWithAnalysis ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    Logger ->
    OraclePool ->
    SQLAnalysis ->
    Maybe ResultStatement ->
    [(T.Text, IntSet.IntSet)] ->
    ByteString ->
    IO ByteString
proposeGroupDeletionsWithAnalysis tracker cert logger pool analysis observedResult extraTxBlocks sql
    | length statements < 2 = pure sql
    | otherwise = do
        currentIntsRef <- newIORef (statementIdsInt statements)
        currentSQLRef <- newIORef sql
        seenRef <- newIORef (HashSet.singleton sql)
        let env = GroupProposalEnv tracker cert logger pool sql statements currentIntsRef currentSQLRef seenRef
            plan =
                extractDeletionProposalPlanWithExtraTx
                    (analyzedSession analysis)
                    statements
                    (analyzedGraph analysis)
                    observedResult
                    extraTxBlocks
        probeProposalTier env 8 False "weak-group" (planWeakGroups plan)
        probeProposalTier env 6 False "weak-tx-block" (planWeakTxBlocks plan)
        probeProposalTier env 3 True "strong-tx-block" (planStrongTxBlocks plan)
        probeProposalTier env 8 False "tx-wrapper" (planTxWrappers plan)
        probeProposalTier env 4 True "indirect-relation" (planIndirectRelations plan)
        readIORef currentSQLRef
    where
        statements = analyzedStatements analysis

tryDeleteProposalGroup :: GroupProposalEnv -> T.Text -> T.Text -> IntSet.IntSet -> IO Bool
tryDeleteProposalGroup env kind label removed = fmap (fromMaybe False) . runMaybeT $ do
    let pool = gpPool env
        store = poolMetricsStore pool
    currentInts <- liftIO $ readIORef (gpCurrentInts env)
    currentSQL <- liftIO $ readIORef (gpCurrentSQL env)
    let effectiveRemoved = removed `IntSet.intersection` currentInts
        keptInts = currentInts `IntSet.difference` effectiveRemoved
    guard (not (IntSet.null effectiveRemoved) && not (IntSet.null keptInts))
    rejectIfEarly store =<< liftIO (checkEarlyRejectionIdsTracked (gpTracker env) (gpCert env) keptInts)
    candidateSQL <-
        liftIO $
            timeTaggedStatementBuild
                store
                TagStatementComplement
                (cutRemovedStatementInts (gpBaseSQL env) (gpStatements env) keptInts)
    seen <- liftIO $ readIORef (gpSeen env)
    guard (BS.length candidateSQL < BS.length currentSQL && not (HashSet.member candidateSQL seen))
    earlyBytes <-
        liftIO $ checkEarlyRejectionBytesTracked (gpTracker env) (gpCert env) False candidateSQL
    for_ earlyBytes \reason -> do
        liftIO do
            recordEarlyRejectSkip store reason
            modifyIORef' (gpSeen env) (HashSet.insert candidateSQL)
        empty
    liftIO $
        testProposalGroupCandidate
            env
            kind
            label
            currentInts
            currentSQL
            effectiveRemoved
            keptInts
            candidateSQL

testProposalGroupCandidate ::
    GroupProposalEnv ->
    T.Text ->
    T.Text ->
    IntSet.IntSet ->
    ByteString ->
    IntSet.IntSet ->
    IntSet.IntSet ->
    ByteString ->
    IO Bool
testProposalGroupCandidate env kind label currentInts currentSQL effectiveRemoved keptInts candidateSQL = do
    let pool = gpPool env
    reportCandidateCount pool candidateSQL (IntSet.size keptInts)
    outcomes <- testOrderedTagged pool [(TagStatementComplement, candidateSQL)]
    case outcomes of
        (OracleAccepted, _) : _ -> do
            let currentStmts =
                    filter (\s -> IntSet.member (unStatementId (stmtId s)) currentInts) (gpStatements env)
                noncontig = isNoncontiguousIn currentStmts effectiveRemoved
                saved = BS.length currentSQL - BS.length candidateSQL
            recordAcceptedBytesSaved (poolMetricsStore pool) TagStatementComplement saved
            writeIORef (gpCurrentInts env) keptInts
            writeIORef (gpCurrentSQL env) candidateSQL
            modifyIORef' (gpSeen env) (HashSet.insert candidateSQL)
            logInfo'
                (gpLogger env)
                "reducer"
                "Dependency group deletion accepted"
                [ ("group_kind", toJSON kind)
                , ("group_label", toJSON label)
                , ("noncontiguous", toJSON noncontig)
                , ("statements_removed", toJSON (IntSet.size effectiveRemoved))
                , ("bytes_saved", toJSON saved)
                ]
            pure True
        _ -> do
            modifyIORef' (gpSeen env) (HashSet.insert candidateSQL)
            pure False

splitGroupsByBytes :: [DeletionProposalGroup] -> ([DeletionProposalGroup], [DeletionProposalGroup])
splitGroupsByBytes [] = ([], [])
splitGroupsByBytes [x] = ([x], [])
splitGroupsByBytes gs =
    let (l, r) = go 0 [] gs
     in if null r then splitAt (length gs `div` 2) gs else (l, r)
    where
        target = max 1 (sum (map proposalBytes gs) `div` 2)
        go _ accGroups [] = (reverse accGroups, [])
        go _ accGroups [x] = (reverse (x : accGroups), [])
        go !acc accGroups (x : xs)
            | acc + proposalBytes x >= target && not (null accGroups) = (reverse accGroups, x : xs)
            | otherwise = go (acc + proposalBytes x) (x : accGroups) xs

probeProposalTier ::
    GroupProposalEnv -> Int -> Bool -> T.Text -> [DeletionProposalGroup] -> IO ()
probeProposalTier env maxMisses stopOnBothHalvesFail tierKind rawGroups = do
    currentInts <- readIORef (gpCurrentInts env)
    let active =
            filter
                (not . IntSet.null . IntSet.intersection currentInts . proposalStatements)
                rawGroups
    case active of
        [] -> pure ()
        [single] -> void (tryDeleteProposalGroup env tierKind (proposalLabel single) (proposalStatements single))
        _ -> do
            let allRemoved = IntSet.unions (map proposalStatements active)
            acceptedAll <- tryDeleteProposalGroup env (tierKind <> "-union") "union" allRemoved
            unless acceptedAll $ do
                missesRef <- newIORef (1 :: Int)
                bisectProposalGroups env missesRef maxMisses stopOnBothHalvesFail tierKind active

bisectProposalGroups ::
    GroupProposalEnv -> IORef Int -> Int -> Bool -> T.Text -> [DeletionProposalGroup] -> IO ()
bisectProposalGroups env missesRef maxMisses stopOnBothHalvesFail tierKind gs = do
    misses <- readIORef missesRef
    when (misses < maxMisses) $ case gs of
        [] -> pure ()
        [g] -> do
            ok <- tryDeleteProposalGroup env tierKind (proposalLabel g) (proposalStatements g)
            recordProposalAttempt missesRef ok
        _ -> bisectSplit gs
    where
        bisectSplit groups = do
            let (leftGs, rightGs) = splitGroupsByBytes groups
                leftSet = IntSet.unions (map proposalStatements leftGs)
                rightSet = IntSet.unions (map proposalStatements rightGs)
            okLeft <- tryDeleteProposalGroup env (tierKind <> "-bisect") "left-half" leftSet
            recordProposalAttempt missesRef okLeft
            if okLeft
                then recurse rightGs
                else tryRightHalf leftGs rightGs rightSet
        tryRightHalf leftGs rightGs rightSet = do
            misses <- readIORef missesRef
            when (misses < maxMisses) $ do
                okRight <- tryDeleteProposalGroup env (tierKind <> "-bisect") "right-half" rightSet
                recordProposalAttempt missesRef okRight
                if okRight
                    then recurse leftGs
                    else unless stopOnBothHalvesFail $ do
                        when (length leftGs > 1) (recurse leftGs)
                        when (length rightGs > 1) (recurse rightGs)
        recurse = bisectProposalGroups env missesRef maxMisses stopOnBothHalvesFail tierKind

recordProposalAttempt :: IORef Int -> Bool -> IO ()
recordProposalAttempt missesRef True = writeIORef missesRef 0
recordProposalAttempt missesRef False = modifyIORef' missesRef (+ 1)

closureSearchWithAnalysis ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    StatementSearchPolicy ->
    Logger ->
    OraclePool ->
    ByteString ->
    SQLAnalysis ->
    IO ByteString
closureSearchWithAnalysis tracker cert policy logger pool sql analysis =
    let statements = analyzedStatements analysis
     in if null statements
            then pure sql
            else do
                retained <-
                    closureDdminWithCert
                        tracker
                        cert
                        policy
                        logger
                        pool
                        sql
                        statements
                        (analyzedGraph analysis)
                pure (statementSQL sql statements retained)

data ObservedResultEvidence
    = ObservedStatementOnly !ResultStatement
    | ObservedFullTrace !ResultTrace

evidenceResultStatement :: ObservedResultEvidence -> ResultStatement
evidenceResultStatement (ObservedStatementOnly rs) = rs
evidenceResultStatement (ObservedFullTrace tr) = traceResultStatement tr

evidenceResultTrace :: ObservedResultEvidence -> Maybe ResultTrace
evidenceResultTrace (ObservedStatementOnly _) = Nothing
evidenceResultTrace (ObservedFullTrace tr) = Just tr

-- | Reduce SQL test case using default configuration and dialect.
reduceSQL :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
reduceSQL dialect = reduceSQLWithLogger dialect nullLogger

-- | Reduce SQL test case using custom logger and dialect.
reduceSQLWithLogger :: SQLDialect -> Logger -> OraclePool -> ByteString -> IO ByteString
reduceSQLWithLogger dialect =
    reduceSQLWithConfig (defaultReductionConfig {reductionDialect = dialect})

-- | Reduce SQL test case using custom configuration, logger, and oracle pool.
reduceSQLWithConfig :: ReductionConfig -> Logger -> OraclePool -> ByteString -> IO ByteString
reduceSQLWithConfig config logger pool initialSQL =
    reductionSQL <$> reduceSQLWithEvidence nullStageReporter config logger pool initialSQL

reduceSQLWithEvidence ::
    StageReporter -> ReductionConfig -> Logger -> OraclePool -> ByteString -> IO ReductionResult
reduceSQLWithEvidence = reduceSQLWithResultSlicing (const (pure Nothing))

-- | Use the engine's result statement for each observed input to slicing.
reduceSQLWithResultSlicing ::
    (ByteString -> IO (Maybe ResultStatement)) ->
    StageReporter ->
    ReductionConfig ->
    Logger ->
    OraclePool ->
    ByteString ->
    IO ReductionResult
reduceSQLWithResultSlicing resultStatement =
    reduceSQLWithObservedResult (fmap (fmap ObservedStatementOnly) . resultStatement)

-- | Use full native engine result traces for both slicing and sound early-rejection certificates.
reduceSQLWithResultTrace ::
    (ByteString -> IO (Maybe ResultTrace)) ->
    StageReporter ->
    ReductionConfig ->
    Logger ->
    OraclePool ->
    ByteString ->
    IO ReductionResult
reduceSQLWithResultTrace = reduceSQLWithResultTraceSeeded Nothing

-- | Variant of 'reduceSQLWithResultTrace' reusing an existing initial parse when bytes match.
reduceSQLWithResultTraceSeeded ::
    Maybe (ByteString, (SessionInit, [StatementUnit])) ->
    (ByteString -> IO (Maybe ResultTrace)) ->
    StageReporter ->
    ReductionConfig ->
    Logger ->
    OraclePool ->
    ByteString ->
    IO ReductionResult
reduceSQLWithResultTraceSeeded initialParsed = reduceSQLWithResultTraceFull initialParsed Nothing

-- | Full native result-trace entrypoint reusing an initial parse and dynamic failing-prefix cache.
reduceSQLWithResultTraceFull ::
    Maybe (ByteString, (SessionInit, [StatementUnit])) ->
    Maybe FailingPrefixCache ->
    (ByteString -> IO (Maybe ResultTrace)) ->
    StageReporter ->
    ReductionConfig ->
    Logger ->
    OraclePool ->
    ByteString ->
    IO ReductionResult
reduceSQLWithResultTraceFull initialParsed mFailCache resultTrace =
    reduceSQLWithObservedResultSeeded
        initialParsed
        mFailCache
        (fmap (fmap ObservedFullTrace) . resultTrace)

reduceSQLWithObservedResult ::
    (ByteString -> IO (Maybe ObservedResultEvidence)) ->
    StageReporter ->
    ReductionConfig ->
    Logger ->
    OraclePool ->
    ByteString ->
    IO ReductionResult
reduceSQLWithObservedResult = reduceSQLWithObservedResultSeeded Nothing Nothing

reduceSQLWithObservedResultSeeded ::
    Maybe (ByteString, (SessionInit, [StatementUnit])) ->
    Maybe FailingPrefixCache ->
    (ByteString -> IO (Maybe ObservedResultEvidence)) ->
    StageReporter ->
    ReductionConfig ->
    Logger ->
    OraclePool ->
    ByteString ->
    IO ReductionResult
reduceSQLWithObservedResultSeeded initialParsed mFailCache observeResult reporter config logger pool initialSQL = do
    when (reductionColumnMode config /= ColumnOff && not (reductionIntraStatement config)) $
        ioError (userError "reductionColumnMode requires reductionIntraStatement")
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    if not initialOK
        then do
            logWarn logger "reducer" "Initial SQL candidate failed oracle check"
            pure (ReductionResult initialSQL Nothing)
        else do
            parse <-
                reuseLatestSQLWithSeed
                    initialParsed
                    (parseStatements (poolMetricsStore pool) (reductionDialect config))
            analyze <- reuseLatestSQL (analyzeSQL parse logger (reductionDialect config))
            tracker <-
                newEarlyRejectTrackerWithFailures
                    (if reductionEarlyReject config then mFailCache else Nothing)
            loop parse analyze tracker (1 :: Int) initialSQL Nothing False
                `finally` logEarlyRejectStats logger "reduction" tracker
    where
        loop parse analyze tracker iteration currentSQL priorEvidence priorFullFixedPoint = do
            logInfo'
                logger
                "reducer"
                "Starting reduction iteration"
                [ ("iteration", toJSON iteration)
                , ("current_bytes", toJSON (BS.length currentSQL))
                ]
            (nextSQL, evidence, fullFixedPoint) <-
                reduceIterationWithEvidence
                    observeResult
                    tracker
                    reporter
                    config
                    logger
                    pool
                    parse
                    analyze
                    currentSQL
                    priorEvidence
                    priorFullFixedPoint
            if BS.length nextSQL < BS.length currentSQL
                then loop parse analyze tracker (iteration + 1) nextSQL evidence fullFixedPoint
                else do
                    logInfo'
                        logger
                        "reducer"
                        "Rewrite fixed point reached"
                        [ ("total_iterations", toJSON iteration)
                        , ("final_bytes", toJSON (BS.length currentSQL))
                        ]
                    -- The loop keeps the pre-iteration bytes on no shrink, so
                    -- evidence only survives when it attests those exact bytes.
                    let confirmed =
                            if maybe False ((== currentSQL) . evidenceBytes) evidence then evidence else Nothing
                    pure (ReductionResult currentSQL confirmed)

data IterationEnv = IterationEnv
    { iterObserveResult :: ByteString -> IO (Maybe ObservedResultEvidence)
    , iterTracker :: !EarlyRejectTracker
    , iterReporter :: !StageReporter
    , iterConfig :: !ReductionConfig
    , iterLogger :: !Logger
    , iterPool :: !OraclePool
    , iterParse :: !StatementParser
    , iterAnalyze :: ByteString -> IO SQLAnalysis
    }

reduceIterationWithEvidence ::
    (ByteString -> IO (Maybe ObservedResultEvidence)) ->
    EarlyRejectTracker ->
    StageReporter ->
    ReductionConfig ->
    Logger ->
    OraclePool ->
    StatementParser ->
    (ByteString -> IO SQLAnalysis) ->
    ByteString ->
    Maybe ReductionEvidence ->
    Bool ->
    IO (ByteString, Maybe ReductionEvidence, Bool)
reduceIterationWithEvidence observeResult tracker reporter config logger pool parse analyze initialSQL priorEvidence priorFullFixedPoint = do
    (sliced, reusableAnalysis) <- runSlicingAndGroupsPhase env initialSQL priorEvidence
    if priorFullFixedPoint && maybe False ((== sliced) . evidenceBytes) priorEvidence
        then pure (sliced, priorEvidence, True)
        else do
            searchAnalysis <-
                if reductionClosure config && reductionUnrestricted config
                    then Just <$> maybe (analyze sliced) pure reusableAnalysis
                    else pure reusableAnalysis
            let useEarlyUnrestricted =
                    reductionClosure config
                        && reductionUnrestricted config
                        && maybe False (graphHasGlobalBarrier . analyzedGraph) searchAnalysis
            effectiveStatementPolicy <-
                resolveEffectiveStatementPolicy env initialSQL sliced searchAnalysis priorEvidence
            searchInput <-
                if useEarlyUnrestricted
                    then runUnrestrictedSearch env effectiveStatementPolicy sliced
                    else pure sliced
            searched <-
                runPrimaryStatementSearch env effectiveStatementPolicy sliced searchInput searchAnalysis
            fallback <-
                if reductionClosure config
                    && reductionUnrestricted config
                    && (not useEarlyUnrestricted || searched /= searchInput)
                    then runUnrestrictedSearch env effectiveStatementPolicy searched
                    else pure searched
            (out, ev) <- runInterleaveStages env priorEvidence fallback
            pure (out, ev, out == sliced)
    where
        env = IterationEnv observeResult tracker reporter config logger pool parse analyze

runSlicingAndGroupsPhase ::
    IterationEnv -> ByteString -> Maybe ReductionEvidence -> IO (ByteString, Maybe SQLAnalysis)
runSlicingAndGroupsPhase env initialSQL priorEvidence
    | reductionSlicing config =
        timedReductionStage (iterReporter env) (iterLogger env) StageSlicing initialSQL do
            analysis <- iterAnalyze env initialSQL
            observed <- iterObserveResult env initialSQL
            let observedStmt = fmap evidenceResultStatement observed
                observedTrace = observed >>= evidenceResultTrace
            sliceCert <-
                buildCertifierTracked
                    (reductionEarlyReject config)
                    (iterTracker env)
                    initialSQL
                    (analyzedUnits analysis)
                    observedTrace
            (afterSlice, sliceAnalysis, sliceRetained) <-
                sliceWithAnalysis
                    (iterTracker env)
                    sliceCert
                    (iterLogger env)
                    (iterPool env)
                    analysis
                    observedStmt
                    initialSQL
            if reductionGroupProposals config && not (maybe False ((== afterSlice) . evidenceBytes) priorEvidence)
                then
                    runGroupProposalsAfterSlice
                        env
                        analysis
                        observed
                        sliceCert
                        initialSQL
                        afterSlice
                        sliceAnalysis
                        sliceRetained
                else pure (afterSlice, sliceAnalysis)
    | reductionGroupProposals config
    , reductionClosure config
    , not (maybe False ((== initialSQL) . evidenceBytes) priorEvidence) = do
        analysis <- iterAnalyze env initialSQL
        observed <- iterObserveResult env initialSQL
        groupCert <-
            buildCertifierTracked
                (reductionEarlyReject config)
                (iterTracker env)
                initialSQL
                (analyzedUnits analysis)
                (observed >>= evidenceResultTrace)
        afterGroups <-
            proposeGroupDeletionsWithAnalysis
                (iterTracker env)
                groupCert
                (iterLogger env)
                (iterPool env)
                analysis
                (fmap evidenceResultStatement observed)
                []
                initialSQL
        pure (afterGroups, if afterGroups == initialSQL then Just analysis else Nothing)
    | otherwise = pure (initialSQL, Nothing)
    where
        config = iterConfig env

runGroupProposalsAfterSlice ::
    IterationEnv ->
    SQLAnalysis ->
    Maybe ObservedResultEvidence ->
    EarlyRejectCertifier ->
    ByteString ->
    ByteString ->
    Maybe SQLAnalysis ->
    Maybe [Statement] ->
    IO (ByteString, Maybe SQLAnalysis)
runGroupProposalsAfterSlice env analysis observed sliceCert initialSQL afterSlice sliceAnalysis sliceRetained = do
    groupAnalysis <- maybe (iterAnalyze env afterSlice) pure sliceAnalysis
    let observedStmt = fmap evidenceResultStatement observed
        extraTxBlocks = case sliceRetained of
            Just retained
                | afterSlice /= initialSQL ->
                    projectTransactionBlocks
                        (analyzedStatements analysis)
                        (analyzedGraph analysis)
                        observedStmt
                        retained
                        (analyzedStatements groupAnalysis)
            _ -> []
    groupObserved <-
        if afterSlice == initialSQL
            then pure observed
            else iterObserveResult env afterSlice
    groupCert <-
        if afterSlice == initialSQL
            then pure sliceCert
            else
                buildCertifierTracked
                    (reductionEarlyReject (iterConfig env))
                    (iterTracker env)
                    afterSlice
                    (analyzedUnits groupAnalysis)
                    (groupObserved >>= evidenceResultTrace)
    afterGroups <-
        proposeGroupDeletionsWithAnalysis
            (iterTracker env)
            groupCert
            (iterLogger env)
            (iterPool env)
            groupAnalysis
            (fmap evidenceResultStatement groupObserved)
            extraTxBlocks
            afterSlice
    pure (afterGroups, if afterGroups == afterSlice then Just groupAnalysis else Nothing)

resolveEffectiveStatementPolicy ::
    IterationEnv ->
    ByteString ->
    ByteString ->
    Maybe SQLAnalysis ->
    Maybe ReductionEvidence ->
    IO StatementSearchPolicy
resolveEffectiveStatementPolicy env initialSQL sliced searchAnalysis priorEvidence =
    case reductionStatementSearch config of
        StatementRankedFallback rankedCfg -> do
            remainingStmts <- case searchAnalysis of
                Just analysis -> pure (length (analyzedStatements analysis))
                Nothing -> length . snd <$> iterParse env sliced
            let alreadyConfirmed = maybe False ((== sliced) . evidenceBytes) priorEvidence
                activated =
                    not alreadyConfirmed
                        && shouldActivateRankedFallback
                            rankedCfg
                            (reductionSlicing config)
                            (BS.length initialSQL)
                            (BS.length sliced)
                            remainingStmts
                hasGlobalBarrier = maybe False (graphHasGlobalBarrier . analyzedGraph) searchAnalysis
            logInfo'
                (iterLogger env)
                "reducer"
                (if activated then "Ranked statement fallback activated" else "Ranked statement fallback skipped")
                [ ("activated", toJSON activated)
                , ("already_confirmed", toJSON alreadyConfirmed)
                , ("slicing_enabled", toJSON (reductionSlicing config))
                , ("global_barrier", toJSON hasGlobalBarrier)
                , ("bytes_before", toJSON (BS.length initialSQL))
                , ("bytes_after", toJSON (BS.length sliced))
                , ("statements_after", toJSON remainingStmts)
                ]
            pure (if activated then StatementRankedWith rankedCfg else StatementByteWeighted)
        other -> pure other
    where
        config = iterConfig env

runPrimaryStatementSearch ::
    IterationEnv ->
    StatementSearchPolicy ->
    ByteString ->
    ByteString ->
    Maybe SQLAnalysis ->
    IO ByteString
runPrimaryStatementSearch env policy sliced searchInput searchAnalysis
    | reductionClosure config =
        fst <$> timedReductionStage (iterReporter env) (iterLogger env) StageClosureSearch searchInput do
            analysis <- case searchAnalysis of
                Just cached | searchInput == sliced -> pure cached
                _ -> iterAnalyze env searchInput
            mTrace <- lookupIterTraceForCert env searchInput
            cert <-
                buildCertifierTracked
                    (reductionEarlyReject config)
                    (iterTracker env)
                    searchInput
                    (analyzedUnits analysis)
                    mTrace
            out <-
                closureSearchWithAnalysis
                    (iterTracker env)
                    cert
                    policy
                    (iterLogger env)
                    (iterPool env)
                    searchInput
                    analysis
            pure (out, ())
    | otherwise =
        fst <$> timedReductionStage (iterReporter env) (iterLogger env) StageGraphFreeSearch searchInput do
            mTrace <- lookupIterTraceForCert env searchInput
            out <-
                statementDdminWithCert
                    (reductionEarlyReject config)
                    (iterTracker env)
                    mTrace
                    policy
                    (iterParse env)
                    (iterPool env)
                    searchInput
            pure (out, ())
    where
        config = iterConfig env

lookupIterTraceForCert :: IterationEnv -> ByteString -> IO (Maybe ResultTrace)
lookupIterTraceForCert env sql
    | reductionEarlyReject (iterConfig env) = (>>= evidenceResultTrace) <$> iterObserveResult env sql
    | otherwise = pure Nothing

runUnrestrictedSearch :: IterationEnv -> StatementSearchPolicy -> ByteString -> IO ByteString
runUnrestrictedSearch env statementPolicy sql =
    fst <$> timedReductionStage (iterReporter env) (iterLogger env) StageUnrestrictedFallback sql do
        mTrace <- lookupIterTraceForCert env sql
        out <-
            statementDdminWithCert
                (reductionEarlyReject (iterConfig env))
                (iterTracker env)
                mTrace
                statementPolicy
                (iterParse env)
                (iterPool env)
                sql
        pure (out, ())

runInterleaveStages ::
    IterationEnv -> Maybe ReductionEvidence -> ByteString -> IO (ByteString, Maybe ReductionEvidence)
runInterleaveStages env priorEvidence base
    | Just evidence <- priorEvidence
    , evidenceBytes evidence == base =
        -- Prior evidence already confirms fixed point on these bytes.
        pure (base, priorEvidence)
    | otherwise = case (reductionUnrestricted config, reductionIntraStatement config) of
        (True, True) -> do
            (out, minimal) <- runInterleaveFixedPoint env 1 base
            pure (out, Just (ReductionEvidence out minimal))
        (True, False) -> do
            (result, stats) <- runIterAuditStage env base
            pure (result, Just (ReductionEvidence result (auditMinimalityConfirmed stats)))
        (False, True) -> do
            out <- runIterSyntaxStage env base
            pure (out, Just (ReductionEvidence out False))
        (False, False) -> pure (base, Just (ReductionEvidence base False))
    where
        config = iterConfig env

runInterleaveFixedPoint :: IterationEnv -> Int -> ByteString -> IO (ByteString, Bool)
runInterleaveFixedPoint env iter current = do
    (afterAudit, auditStats) <- runIterAuditStage env current
    afterSyntax <- runIterSyntaxStage env afterAudit
    if BS.length afterSyntax < BS.length current
        then do
            logInfo'
                (iterLogger env)
                "reducer"
                "Interleaved reduction step accepted"
                [ ("interleave_iteration", toJSON iter)
                , ("bytes_before", toJSON (BS.length current))
                , ("bytes_after", toJSON (BS.length afterSyntax))
                ]
            runInterleaveFixedPoint env (iter + 1) afterSyntax
        else do
            logInfo'
                (iterLogger env)
                "reducer"
                "Interleaved audit and syntax global fixed point reached"
                [ ("total_interleave_iterations", toJSON iter)
                , ("final_bytes", toJSON (BS.length current))
                ]
            pure (afterSyntax, auditMinimalityConfirmed auditStats)

runIterAuditStage :: IterationEnv -> ByteString -> IO (ByteString, AuditStats)
runIterAuditStage env current =
    timedReductionStage (iterReporter env) (iterLogger env) StageStatementAudit current do
        mTrace <- lookupIterTraceForCert env current
        statementAuditWithCert
            (reductionEarlyReject (iterConfig env))
            (iterTracker env)
            mTrace
            (iterLogger env)
            (iterParse env)
            (iterPool env)
            current

runIterSyntaxStage :: IterationEnv -> ByteString -> IO ByteString
runIterSyntaxStage env current =
    fst <$> timedReductionStage (iterReporter env) (iterLogger env) StageIntraStatementRewrites current do
        out <-
            reduceSyntaxWithDialect
                config
                (iterTracker env)
                (lookupIterTraceForCert env)
                (iterLogger env)
                (columnParseOptions (reductionColumnMode config) (reductionDialect config))
                (iterPool env)
                (const True)
                current
        pure (out, ())
    where
        config = iterConfig env

data SyntaxStats = SyntaxStats
    { syntaxAttempts :: !Int
    , syntaxAccepted :: !Int
    , syntaxElapsed :: !Word64
    }

-- | Canonical syntax-edit category order.
allEditCategories :: [ParserEditCategory]
allEditCategories = [EditClause, EditList, EditColumn, EditConstraint, EditType, EditExpression, EditLiteral]

-- | Read-only configuration shared by the syntax-reduction phases.
data SyntaxContext = SyntaxContext
    { syntaxLogger :: !Logger
    , syntaxStore :: !(Maybe MetricsStore)
    , syntaxPool :: !OraclePool
    , syntaxShouldRun :: ParserEdit -> Bool
    , syntaxMode :: !ColumnMode
    , syntaxLiteralBatches :: !Bool
    , syntaxDistributedLiteralBatches :: !Bool
    , syntaxDistributedBatchConfig :: !DistributedBatchConfig
    , syntaxDeferLiteralVariants :: !LiteralDeferralMode
    , syntaxDeferRecursivePredicates :: !Bool
    , syntaxAstBatching :: !Bool
    , syntaxAdaptiveValues :: !Bool
    , syntaxFullReparse :: !Bool
    , syntaxOptions :: !ParseOptions
    , syntaxEarlyReject :: !Bool
    , syntaxTracker :: !EarlyRejectTracker
    , syntaxCert :: !EarlyRejectCertifier
    , syntaxLookupTrace :: ByteString -> IO (Maybe ResultTrace)
    }

data SyntaxCandidate
    = SingleCandidate !ParserEdit
    | BundleCandidate !ColumnTarget
    | BatchCandidate !ParserEditCategory !T.Text ![ParserEdit]

-- | Clause edit classification in priority order.
data ClauseEditClass = SafeClauseEdit | StructuralClauseEdit | FragileClauseEdit
    deriving (Eq, Ord, Show)

-- | Expression edit classification in priority order.
data ExpressionEditClass = UnwrapExpressionEdit | AggressiveExpressionEdit
    deriving (Eq, Ord, Show)

-- | Clause labels whose removal cannot change predicate truth.
safeClauseLabels :: Set T.Text
safeClauseLabels =
    Set.fromList
        [ "remove-order_by"
        , "remove-limit"
        , "remove-offset"
        , "remove-fetch"
        , "remove-distinct"
        , "simplify-window_spec_empty"
        , "remove-window_partition_by"
        , "remove-window_order_by"
        , "remove-window_frame"
        ]

-- | Predicate-bearing clause labels, attempted after structural clauses.
fragileClauseLabels :: Set T.Text
fragileClauseLabels = Set.singleton "remove-where"

-- | Expression labels that only collapse redundant structure.
unwrappedExpressionLabels :: Set T.Text
unwrappedExpressionLabels =
    Set.fromList
        [ "simplify-parenthesized_expression"
        , "simplify-scalar-subquery"
        , "simplify-cast"
        , "simplify-function_to_argument"
        , "simplify-case_result"
        , "simplify-case_else"
        ]

-- | Operand simplifications that unwrap unless they discard the operand value.
operandSimplifyLabels :: Set T.Text
operandSimplifyLabels =
    Set.fromList
        [ "simplify-unary_expression"
        , "simplify-binary_expression"
        , "simplify-in_list"
        , "simplify-in_subquery"
        , "simplify-between"
        , "simplify-like"
        , "simplify-is_null"
        ]

-- | Prefix used by the frontend for operand-unwrapping edits.
unwrapExpressionPrefix :: T.Text
unwrapExpressionPrefix = "unwrap-"

classifyClauseEdit :: ParserEdit -> ClauseEditClass
classifyClauseEdit edit
    | label `Set.member` safeClauseLabels = SafeClauseEdit
    | label `Set.member` fragileClauseLabels = FragileClauseEdit
    | otherwise = StructuralClauseEdit
    where
        label = parserEditLabel edit

classifyExpressionEdit :: ParserEdit -> ExpressionEditClass
classifyExpressionEdit edit
    | parserEditLowYield edit = AggressiveExpressionEdit
    | label `Set.member` unwrappedExpressionLabels
        || label `Set.member` operandSimplifyLabels
        || unwrapExpressionPrefix `T.isPrefixOf` label =
        UnwrapExpressionEdit
    | otherwise = AggressiveExpressionEdit
    where
        label = parserEditLabel edit

-- | Named rank constants: lower ranks are tested first.
safeClauseRank, structuralClauseRank, fragileClauseRank :: Int
safeClauseRank = 0
structuralClauseRank = 1
fragileClauseRank = 2

unwrappedExpressionRank, aggressiveExpressionRank :: Int
unwrappedExpressionRank = 0
aggressiveExpressionRank = 1

clauseEditRank :: ParserEdit -> Int
clauseEditRank edit = case classifyClauseEdit edit of
    SafeClauseEdit -> safeClauseRank
    StructuralClauseEdit -> structuralClauseRank
    FragileClauseEdit -> fragileClauseRank

expressionEditRank :: ParserEdit -> Int
expressionEditRank edit = case classifyExpressionEdit edit of
    UnwrapExpressionEdit -> unwrappedExpressionRank
    AggressiveExpressionEdit -> aggressiveExpressionRank

-- | Column edits only participate when the column mode is enabled.
syntaxCategoryOrder :: ColumnMode -> [ParserEditCategory]
syntaxCategoryOrder ColumnOff = filter (/= EditColumn) allEditCategories
syntaxCategoryOrder _ = allEditCategories

syntaxCategoryOrderName :: ColumnMode -> T.Text
syntaxCategoryOrderName = T.intercalate "," . map editCategoryName . syntaxCategoryOrder

reduceSyntaxWithDialect ::
    ReductionConfig ->
    EarlyRejectTracker ->
    (ByteString -> IO (Maybe ResultTrace)) ->
    Logger ->
    ParseOptions ->
    OraclePool ->
    (ParserEdit -> Bool) ->
    ByteString ->
    IO ByteString
reduceSyntaxWithDialect config tracker lookupTrace logger options pool shouldRun initialSQL = do
    let mode = reductionColumnMode config
        context =
            SyntaxContext
                { syntaxLogger = logger
                , syntaxStore = poolMetricsStore pool
                , syntaxPool = pool
                , syntaxShouldRun = shouldRun
                , syntaxMode = mode
                , syntaxLiteralBatches = reductionLiteralBatches config
                , syntaxDistributedLiteralBatches = reductionDistributedLiteralBatches config
                , syntaxDistributedBatchConfig = reductionDistributedBatchConfig config
                , syntaxDeferLiteralVariants = reductionDeferLiteralVariants config
                , syntaxDeferRecursivePredicates = reductionDeferRecursivePredicates config
                , syntaxAstBatching = reductionAstBatching config
                , syntaxAdaptiveValues = reductionAdaptiveValues config
                , syntaxFullReparse = reductionFullSyntaxReparse config
                , syntaxOptions = options
                , syntaxEarlyReject = reductionEarlyReject config
                , syntaxTracker = tracker
                , syntaxCert = emptyEarlyRejectCertifier
                , syntaxLookupTrace = lookupTrace
                }
    logDebug'
        logger
        "reducer"
        "Using ordinal-forward syntax candidate schedule"
        [
            ( "search_policy"
            , toJSON
                ( syntaxPolicyName mode
                    <> literalBatchPolicy (reductionLiteralBatches config)
                    <> distributedLiteralBatchPolicy (reductionDistributedLiteralBatches config)
                    <> deferLiteralVariantsPolicy (reductionDeferLiteralVariants config)
                    <> deferRecursivePredicatesPolicy (reductionDeferRecursivePredicates config)
                    <> astBatchPolicy (reductionAstBatching config)
                    <> valuesChunkPolicy (reductionAdaptiveValues config)
                    <> fullReparsePolicy (reductionFullSyntaxReparse config)
                )
            )
        , ("category_order", toJSON (syntaxCategoryOrderName mode))
        ]
    stats <- newIORef (SyntaxStats 0 0 0)
    result <- runSyntaxFixedPoint context stats initialSQL
    logSyntaxStats logger =<< readIORef stats
    pure result

-- | Reparse after each accepted edit and resweep until a full cycle makes no progress.
runSyntaxFixedPoint :: SyntaxContext -> IORef SyntaxStats -> ByteString -> IO ByteString
runSyntaxFixedPoint context stats currentSQL = do
    (startUnits, phases, bundles) <- parseSyntaxSnapshot context currentSQL
    mTrace <- syntaxLookupTrace context currentSQL
    cert <-
        buildCertifierTracked
            (syntaxEarlyReject context)
            (syntaxTracker context)
            currentSQL
            startUnits
            mTrace
    let context' = context {syntaxCert = cert}
    sweepSyntaxCategories
        context'
        stats
        currentSQL
        startUnits
        phases
        bundles
        (syntaxCategoryOrder (syntaxMode context'))
        False

sweepSyntaxCategories ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    [ParserEditCategory] ->
    Bool ->
    IO ByteString
sweepSyntaxCategories context stats currentSQL units phases bundles [] shrunk
    | shrunk = runSyntaxFixedPoint context stats currentSQL
    | otherwise = runDeferredSyntaxPasses context stats currentSQL units phases bundles
sweepSyntaxCategories context stats currentSQL currentUnits phases bundles (category : rest) shrunk = do
    (batchSQL, batchUnits, batchPhases, batchBundles) <-
        runCategoryBatches context stats currentSQL currentUnits phases bundles category
    (nextSQL, nextUnits, nextPhases, nextBundles, categoryShrunk) <-
        exhaustSyntaxCategory context stats batchSQL batchUnits batchPhases batchBundles category
    sweepSyntaxCategories
        context
        stats
        nextSQL
        nextUnits
        nextPhases
        nextBundles
        rest
        (shrunk || categoryShrunk || BS.length batchSQL < BS.length currentSQL)

runDeferredSyntaxPasses ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    IO ByteString
runDeferredSyntaxPasses context stats = go deferredSpecs
    where
        deferredValuesStream buckets _ ordinal =
            map SingleCandidate (drop ordinal (valuesChunkEdits True (phaseCandidates EditList buckets)))
        deferredLiteralsStream buckets _ ordinal =
            map SingleCandidate (bucketCandidatesFrom [DeferredLiterals] buckets ordinal)
        deferredRecursiveStream buckets _ ordinal =
            map SingleCandidate (bucketCandidatesFrom [DeferredRecursivePredicates] buckets ordinal)
        deferredSpecs =
            [(Just EditList, deferredValuesStream) | syntaxAdaptiveValues context]
                ++ [ (Just EditLiteral, deferredLiteralsStream) | syntaxDeferLiteralVariants context /= DeferLiteralsOff
                   ]
                ++ [(Nothing, deferredRecursiveStream) | syntaxDeferRecursivePredicates context]
        go [] sql _ _ _ = pure sql
        go ((mCategory, stream) : more) sql units phases bundles = do
            (nextSQL, nextUnits, nextPhases, nextBundles, accepted) <-
                exhaustSyntaxStream context stats sql units phases bundles mCategory stream
            if accepted
                then runSyntaxFixedPoint context stats nextSQL
                else go more nextSQL nextUnits nextPhases nextBundles

runCategoryBatches ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEditCategory ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget])
runCategoryBatches context stats currentSQL currentUnits phases bundles category
    | syntaxAstBatching context
    , category `elem` [EditClause, EditConstraint, EditType, EditExpression, EditLiteral] =
        exhaustIndependentAstBatches context stats currentSQL currentUnits phases bundles category
    | category == EditLiteral = do
        (distSQL, distUnits, distPhases, distBundles) <-
            if syntaxDistributedLiteralBatches context
                then exhaustDistributedLiteralBatches context stats currentSQL currentUnits phases bundles
                else pure (currentSQL, currentUnits, phases, bundles)
        if syntaxLiteralBatches context
            then exhaustLiteralBatches context stats distSQL distUnits distPhases distBundles
            else pure (distSQL, distUnits, distPhases, distBundles)
    | otherwise = pure (currentSQL, currentUnits, phases, bundles)

-- | Shift statement spans by a byte offset.
shiftStatementUnit :: Int -> StatementUnit -> StatementUnit
shiftStatementUnit delta unit =
    unit
        { unitStatement = shiftStatement delta (unitStatement unit)
        , unitFacts = fmap (shiftStatementFacts delta) (unitFacts unit)
        }

shiftStatement :: Int -> Statement -> Statement
shiftStatement delta stmt =
    stmt {stmtSpan = shiftSpan delta (stmtSpan stmt)}

shiftSpan :: Int -> Span -> Span
shiftSpan delta (Span start end) =
    Span (start + delta) (end + delta)

shiftStatementFacts :: Int -> StatementFacts -> StatementFacts
shiftStatementFacts delta facts =
    facts {factEdits = map (shiftParserEdit delta) (factEdits facts)}

shiftParserEdit :: Int -> ParserEdit -> ParserEdit
shiftParserEdit delta edit =
    edit
        { parserEditStart = parserEditStart edit + delta
        , parserEditEnd = parserEditEnd edit + delta
        }

shiftColumnTarget :: Int -> Int -> ColumnTarget -> ColumnTarget
shiftColumnTarget threshold delta target =
    target {targetSites = map shiftSite (targetSites target)}
    where
        shiftSite site
            | siteStart site >= threshold =
                site
                    { siteStart = siteStart site + delta
                    , siteEnd = siteEnd site + delta
                    }
            | otherwise = site

{- | Parse a single modified statement in isolation after splicing one or more
local edits, then shift its spans back to global coordinates.
-}
parseModifiedUnit ::
    SyntaxContext ->
    ParserEditCategory ->
    StatementUnit ->
    [ParserEdit] ->
    IO (Maybe (StatementUnit, Int))
parseModifiedUnit context category oldUnit edits = runMaybeT do
    let Span start_k end_k = stmtSpan (unitStatement oldUnit)
        oldRaw = stmtRaw (unitStatement oldUnit)
        localEdits = [shiftParserEdit (negate start_k) edit | edit <- edits]
        -- After EditList has completed in the current sweep, any accepted edit
        -- forces a full cycle restart (runSyntaxFixedPoint), so chunk edits are
        -- never consumed for the rest of this cycle.
        singleOptions
            | category `elem` [EditConstraint, EditType, EditExpression, EditLiteral] =
                (syntaxOptions context) {parseChunkEdits = False}
            | otherwise = syntaxOptions context
    newRaw <- hoistMaybe (applyParserEdits oldRaw localEdits)
    let delta = BS.length newRaw - (end_k - start_k)
    parsed <-
        liftIO $
            parseSQLTimed (syntaxStore context) (kindOfOptions singleOptions) singleOptions newRaw
    singleUnit <- hoistMaybe (singleParsedUnit parsed)
    let renamed =
            singleUnit
                { unitStatement = (unitStatement singleUnit) {stmtId = stmtId (unitStatement oldUnit)}
                }
    pure (shiftStatementUnit start_k renamed, delta)
    where
        singleParsedUnit (Right result)
            | [unit] <- parsedUnits result = Just unit
        singleParsedUnit _ = Nothing

-- | Incrementally update syntax snapshot by reparsing only the modified statement.
updateSyntaxSnapshot ::
    SyntaxContext ->
    ParserEditCategory ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEdit ->
    ByteString ->
    IO ([StatementUnit], SyntaxIndex, [ColumnTarget])
updateSyntaxSnapshot context category currentUnits index currentBundles edit =
    updateSyntaxBatchSnapshot context category currentUnits index currentBundles [edit]

{- | Incrementally refresh an accepted single- or multi-edit syntax batch by
reparsing only the touched statements through the Rust parser and shifting
untouched statement spans by the cumulative byte delta, with full reparse fallback.
-}
updateSyntaxBatchSnapshot ::
    SyntaxContext ->
    ParserEditCategory ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    [ParserEdit] ->
    ByteString ->
    IO ([StatementUnit], SyntaxIndex, [ColumnTarget])
updateSyntaxBatchSnapshot context _ _ _ _ _ acceptedBytes
    | syntaxFullReparse context = parseSyntaxSnapshot context acceptedBytes
updateSyntaxBatchSnapshot context category currentUnits index currentBundles batch acceptedBytes = do
    incremental <- runMaybeT do
        guard (category /= EditColumn)
        let sortedEdits = sortOn parserEditStart batch
        guard (validBatch sortedEdits)
        (freshUnits, freshIndex, unitDeltasRev) <-
            updateBatchUnits context category currentBundles 0 0 [] index [] currentUnits sortedEdits
        let freshBundles =
                foldl' (\bs (oldEnd, delta) -> map (shiftColumnTarget oldEnd delta) bs) currentBundles unitDeltasRev
        pure (freshUnits, freshIndex, freshBundles)
    maybe (parseSyntaxSnapshot context acceptedBytes) pure incremental
    where
        validEdit e = parserEditStart e >= 0 && literalSavings e > 0
        validBatch [] = False
        validBatch [e] = validEdit e
        validBatch (a : b : rest) =
            validEdit a
                && ( parserEditEnd a < parserEditStart b
                        || ( parserEditEnd a == parserEditStart b
                                && (BS.null (parserEditReplacement a) || BS.null (parserEditReplacement b))
                           )
                   )
                && validBatch (b : rest)

updateBatchUnits ::
    SyntaxContext ->
    ParserEditCategory ->
    [ColumnTarget] ->
    Int ->
    Int ->
    [StatementUnit] ->
    SyntaxIndex ->
    [(Int, Int)] ->
    [StatementUnit] ->
    [ParserEdit] ->
    MaybeT IO ([StatementUnit], SyntaxIndex, [(Int, Int)])
updateBatchUnits _ _ _ _ _ accUnitsRev accIndex unitDeltasRev [] [] =
    pure (reverse accUnitsRev, accIndex, unitDeltasRev)
updateBatchUnits _ _ _ _ _ _ _ _ [] (_ : _) = empty
updateBatchUnits context category currentBundles !unitIdx !cumDelta accUnitsRev accIndex unitDeltasRev (unit : moreUnits) remEdits
    | null touching =
        let shiftedUnit = if cumDelta == 0 then unit else shiftStatementUnit cumDelta unit
         in updateBatchUnits
                context
                category
                currentBundles
                (unitIdx + 1)
                cumDelta
                (shiftedUnit : accUnitsRev)
                accIndex
                unitDeltasRev
                moreUnits
                restEdits
    | otherwise = do
        guard (all (`containsEdit` unit) touching)
        guard (not (hasAffectedBundleSite currentBundles unit))
        (updatedLocal, unitDelta) <- MaybeT (parseModifiedUnit context category unit touching)
        let finalUnit = if cumDelta == 0 then updatedLocal else shiftStatementUnit cumDelta updatedLocal
            accIndex' = replaceIndexedUnit unitIdx unitDelta (indexUnitEdits context finalUnit) accIndex
            oldEnd = spanEndByte (stmtSpan (unitStatement unit))
            unitDeltasRev' = if unitDelta == 0 then unitDeltasRev else (oldEnd, unitDelta) : unitDeltasRev
        updateBatchUnits
            context
            category
            currentBundles
            (unitIdx + 1)
            (cumDelta + unitDelta)
            (finalUnit : accUnitsRev)
            accIndex'
            unitDeltasRev'
            moreUnits
            restEdits
    where
        Span _ unitEnd = stmtSpan (unitStatement unit)
        (touching, restEdits) = span (\e -> parserEditStart e < unitEnd) remEdits

hasAffectedBundleSite :: [ColumnTarget] -> StatementUnit -> Bool
hasAffectedBundleSite currentBundles u =
    let Span oldStart oldEnd = stmtSpan (unitStatement u)
     in any
            (any (\site -> siteStart site >= oldStart && siteStart site < oldEnd) . targetSites)
            currentBundles

containsEdit :: ParserEdit -> StatementUnit -> Bool
containsEdit edit unit =
    let Span start end = stmtSpan (unitStatement unit)
     in parserEditStart edit >= start && parserEditEnd edit <= end

-- | Parse a full snapshot into units, ordered edit buckets, and unblocked column bundles.
parseSyntaxSnapshot ::
    SyntaxContext ->
    ByteString ->
    IO ([StatementUnit], SyntaxIndex, [ColumnTarget])
parseSyntaxSnapshot context currentSQL = do
    (_, units, columns) <-
        parseUnitsOrThrow (syntaxStore context) (syntaxOptions context) currentSQL
    -- Record skipped metrics for blocked bundles.
    mapM_
        (maybe (pure ()) (recordGeneratedSkipped (syntaxStore context)) . targetBlocked)
        columns
    -- Fold into ordered buckets once; category transitions reuse this snapshot.
    pure
        ( units
        , buildSyntaxIndex context units
        , [target | target <- columns, isNothing (targetBlocked target)]
        )

-- Priority buckets preserve category ordering across statement boundaries.
data EditBucket
    = SafeClauses
    | StructuralClauses
    | FragileClauses
    | Lists
    | Columns
    | Constraints
    | Types
    | UnwrapExpressions
    | AggressiveExpressions
    | PrimaryLiterals
    | DeferredLiterals
    | DeferredRecursivePredicates
    deriving (Eq, Ord, Enum)

type IndexedUnitEdits = IntMap.IntMap (Seq ParserEdit)

{- | A fixed-shape tree of statement units. Branches cache edit counts for
ordinal seeking and the sum of byte changes made below them. Unchanged
right subtrees stay in their original coordinates after an earlier edit.
-}
data SyntaxIndex
    = EmptySyntaxIndex
    | IndexedUnit !Int !IndexedUnitEdits
    | IndexedUnits !Int !Int !(IntMap.IntMap Int) !SyntaxIndex !SyntaxIndex

indexUnitEdits :: SyntaxContext -> StatementUnit -> IndexedUnitEdits
indexUnitEdits context unit =
    IntMap.mapWithKey finalize (fst (foldl' insertEdit (IntMap.empty, Set.empty) filtered))
    where
        edits = maybe [] factEdits (unitFacts unit)
        filtered = filter (syntaxShouldRun context) edits
        deferLiterals = syntaxDeferLiteralVariants context
        deferRecursive = syntaxDeferRecursivePredicates context
        deferredRecKey = fromEnum DeferredRecursivePredicates
        finalize key revEdits
            | key == deferredRecKey = Seq.fromList (sortOn (fromEnum . baseEditBucket) (reverse revEdits))
            | otherwise = Seq.fromList (reverse revEdits)
        insertEdit (!buckets, !seenLitSpans) edit
            | deferRecursive && parserEditRecursivePredicate edit =
                (IntMap.insertWith (++) deferredRecKey [edit] buckets, seenLitSpans)
            | deferLiterals /= DeferLiteralsOff && parserEditCategory edit == EditLiteral =
                if isPrimaryLiteralEdit deferLiterals seenLitSpans edit
                    then
                        ( IntMap.insertWith (++) (fromEnum PrimaryLiterals) [edit] buckets
                        , Set.insert (parserEditStart edit, parserEditEnd edit) seenLitSpans
                        )
                    else (IntMap.insertWith (++) (fromEnum DeferredLiterals) [edit] buckets, seenLitSpans)
            | otherwise =
                (IntMap.insertWith (++) (fromEnum (baseEditBucket edit)) [edit] buckets, seenLitSpans)
        baseEditBucket edit = case parserEditCategory edit of
            EditClause -> case classifyClauseEdit edit of
                SafeClauseEdit -> SafeClauses
                StructuralClauseEdit -> StructuralClauses
                FragileClauseEdit -> FragileClauses
            EditList -> Lists
            EditColumn -> Columns
            EditConstraint -> Constraints
            EditType -> Types
            EditExpression -> case classifyExpressionEdit edit of
                UnwrapExpressionEdit -> UnwrapExpressions
                AggressiveExpressionEdit -> AggressiveExpressions
            EditLiteral -> PrimaryLiterals

buildSyntaxIndex :: SyntaxContext -> [StatementUnit] -> SyntaxIndex
buildSyntaxIndex context = build . Seq.fromList
    where
        build units = case Seq.viewl units of
            Seq.EmptyL -> EmptySyntaxIndex
            unit Seq.:< rest
                | Seq.null rest -> IndexedUnit 0 (indexUnitEdits context unit)
                | otherwise ->
                    let (left, right) = Seq.splitAt (Seq.length units `div` 2) units
                     in joinSyntaxIndex (build left) (build right)

joinSyntaxIndex :: SyntaxIndex -> SyntaxIndex -> SyntaxIndex
joinSyntaxIndex left right =
    IndexedUnits
        (indexUnitCount left + indexUnitCount right)
        (indexDelta left + indexDelta right)
        (IntMap.unionWith (+) (counts left) (counts right))
        left
        right
    where
        counts EmptySyntaxIndex = IntMap.empty
        counts (IndexedUnit _ edits) = IntMap.map Seq.length edits
        counts (IndexedUnits _ _ totals _ _) = totals

indexUnitCount :: SyntaxIndex -> Int
indexUnitCount EmptySyntaxIndex = 0
indexUnitCount IndexedUnit {} = 1
indexUnitCount (IndexedUnits count _ _ _ _) = count

indexDelta :: SyntaxIndex -> Int
indexDelta EmptySyntaxIndex = 0
indexDelta (IndexedUnit shift _) = shift
indexDelta (IndexedUnits _ shift _ _ _) = shift

replaceIndexedUnit :: Int -> Int -> IndexedUnitEdits -> SyntaxIndex -> SyntaxIndex
replaceIndexedUnit unitOrdinal delta edits = update unitOrdinal 0
    where
        update _ _ EmptySyntaxIndex = EmptySyntaxIndex
        update _ prefix (IndexedUnit previous _) =
            let local
                    | prefix == 0 = edits
                    | otherwise = IntMap.map (fmap (shiftParserEdit (negate prefix))) edits
             in IndexedUnit (previous + delta) local
        update position prefix (IndexedUnits _ _ _ left right)
            | position < leftCount =
                joinSyntaxIndex (update position prefix left) right
            | otherwise =
                joinSyntaxIndex left (update (position - leftCount) (prefix + indexDelta left) right)
            where
                leftCount = indexUnitCount left

bucketCandidatesFrom :: [EditBucket] -> SyntaxIndex -> Int -> [ParserEdit]
bucketCandidatesFrom buckets index = visitBuckets buckets
    where
        visitBuckets [] _ = []
        visitBuckets (bucket : rest) ordinal
            | ordinal >= count = visitBuckets rest (ordinal - count)
            | otherwise = visit key ordinal 0 index (visitBuckets rest 0)
            where
                key = fromEnum bucket
                count = bucketCount key index

        bucketCount _ EmptySyntaxIndex = 0
        bucketCount key (IndexedUnit _ edits) = maybe 0 Seq.length (IntMap.lookup key edits)
        bucketCount key (IndexedUnits _ _ totals _ _) = IntMap.findWithDefault 0 key totals

        visit _ _ _ EmptySyntaxIndex rest = rest
        visit key ordinal shift (IndexedUnit _ edits) rest =
            case IntMap.lookup key edits of
                Nothing -> rest
                Just bucket ->
                    foldr
                        (\edit -> (:) (if shift == 0 then edit else shiftParserEdit shift edit))
                        rest
                        (Seq.drop ordinal bucket)
        visit key ordinal shift (IndexedUnits _ _ _ left right) rest
            | ordinal >= leftCount = visit key (ordinal - leftCount) rightShift right rest
            | otherwise = visit key ordinal shift left (visit key 0 rightShift right rest)
            where
                leftCount = bucketCount key left
                rightShift = shift + indexDelta left

phaseCandidatesFrom :: ParserEditCategory -> SyntaxIndex -> Int -> [ParserEdit]
phaseCandidatesFrom category = bucketCandidatesFrom (phaseBuckets category)
    where
        phaseBuckets EditClause = [SafeClauses, StructuralClauses, FragileClauses]
        phaseBuckets EditList = [Lists]
        phaseBuckets EditColumn = [Columns]
        phaseBuckets EditConstraint = [Constraints]
        phaseBuckets EditType = [Types]
        phaseBuckets EditExpression = [UnwrapExpressions, AggressiveExpressions]
        phaseBuckets EditLiteral = [PrimaryLiterals]

phaseCandidates :: ParserEditCategory -> SyntaxIndex -> [ParserEdit]
phaseCandidates category index = phaseCandidatesFrom category index 0

isPrimaryLiteralEdit :: LiteralDeferralMode -> Set (Int, Int) -> ParserEdit -> Bool
isPrimaryLiteralEdit DeferLiteralsOff _ _ = True
isPrimaryLiteralEdit DeferLiteralsPrimaryOne seenLitSpans edit =
    not (Set.member (parserEditStart edit, parserEditEnd edit) seenLitSpans)
isPrimaryLiteralEdit DeferLiteralsZeroOne _ edit =
    not (parserEditLowYield edit)

unitNonRecursiveLiterals :: SyntaxContext -> StatementUnit -> [ParserEdit]
unitNonRecursiveLiterals context unit =
    [ edit
    | facts <- maybe [] pure (unitFacts unit)
    , edit <- factEdits facts
    , parserEditCategory edit == EditLiteral
    , not (syntaxDeferRecursivePredicates context && parserEditRecursivePredicate edit)
    , syntaxShouldRun context edit
    ]

allNonRecursiveLiterals :: SyntaxContext -> [StatementUnit] -> [ParserEdit]
allNonRecursiveLiterals context = concatMap (unitNonRecursiveLiterals context)

{- | Try literal batches before the regular single-edit sweep. A rejection of
a batch leaves its smaller partitions and the single-edit sweep eligible.
-}
exhaustLiteralBatches ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget])
exhaustLiteralBatches context stats sql units phases bundles = search 0 (8 :: Int) candidates
    where
        literals = allNonRecursiveLiterals context units
        candidates =
            [ SingleCandidate (combineLiteralEdits sql first rest)
            | unit <- units
            , group <-
                sortOn (Down . sum . map literalSavings) (literalGroups (stmtSpan (unitStatement unit)) literals)
            , first : rest@(_ : _) <- batchPartitions group
            ]
        search _ misses _ | misses <= 0 = pure (sql, units, phases, bundles)
        search _ _ [] = pure (sql, units, phases, bundles)
        search ordinal misses remaining = do
            let (eligible, deferred) = splitAt misses remaining
            (tested, elapsed, accepted) <- testSyntaxCandidates context units sql ordinal eligible
            modifyIORef' stats (recordSyntaxTest tested elapsed accepted)
            case accepted of
                Just (_, SingleCandidate edit, bytes) -> do
                    logInfo'
                        (syntaxLogger context)
                        "reducer"
                        "Accepted literal batch"
                        [ ("bytes_before", toJSON (BS.length sql))
                        , ("bytes_after", toJSON (BS.length bytes))
                        ]
                    (nextUnits, nextPhases, nextBundles) <-
                        updateSyntaxSnapshot context EditLiteral units phases bundles edit bytes
                    exhaustLiteralBatches context stats bytes nextUnits nextPhases nextBundles
                _ -> search (ordinal + length eligible) (misses - tested) deferred

{- | Group equal replacements while retaining order within each group. This
keeps edits to result-bearing literals from blocking batches of noise values.
-}
literalGroups :: Span -> [ParserEdit] -> [[ParserEdit]]
literalGroups (Span start end) = groupReplacements . filter eligible
    where
        eligible edit =
            parserEditStart edit >= start
                && parserEditEnd edit <= end
                && literalSavings edit > 0
        groupReplacements [] = []
        groupReplacements (first : rest) =
            let (matching, others) = partition ((== parserEditReplacement first) . parserEditReplacement) rest
             in group (first : matching) ++ groupReplacements others
        group [] = []
        group (first : rest) = collect (15 :: Int) [first] rest
        collect capacity edits (next : rest)
            | any (overlaps next) edits = collect capacity edits rest
            | capacity > 0 = collect (capacity - 1) (next : edits) rest
        collect _ edits rest = reverse edits : group rest
        overlaps a b = parserEditStart a < parserEditEnd b && parserEditStart b < parserEditEnd a

literalSavings :: ParserEdit -> Int
literalSavings edit = parserEditEnd edit - parserEditStart edit - BS.length (parserEditReplacement edit)

batchPartitions :: [ParserEdit] -> [[ParserEdit]]
batchPartitions edits@(_ : _ : _) =
    let (left, right) = splitAt (length edits `div` 2) edits
     in edits : (batchPartitions left ++ batchPartitions right)
batchPartitions _ = []

{- | Represent the disjoint replacements as one statement-local patch so the
existing incremental snapshot path can refresh it after oracle acceptance.
-}
combineLiteralEdits :: ByteString -> ParserEdit -> [ParserEdit] -> ParserEdit
combineLiteralEdits sql first rest =
    first
        { parserEditStart = start
        , parserEditEnd = end
        , parserEditLabel = "simplify-literal-batch"
        , parserEditReplacement = BS.concat (reverse parts)
        }
    where
        ordered = sortOn parserEditStart (first : rest)
        start = minimum (map parserEditStart ordered)
        (end, parts) = foldl' append (start, []) ordered
        append (previous, acc) edit =
            ( parserEditEnd edit
            , parserEditReplacement edit : BS.take (parserEditStart edit - previous) (BS.drop previous sql) : acc
            )

-- | Select maximal non-interfering edits with disjoint, non-gluing spans.
selectIndependentEdits :: [ParserEdit] -> [ParserEdit]
selectIndependentEdits =
    sortOn parserEditStart
        . foldl' pick []
        . sortOn (\e -> (Down (literalSavings e), parserEditStart e))
    where
        pick acc edit
            | literalSavings edit <= 0 = acc
            | any (interferes edit) acc = acc
            | otherwise = edit : acc
        interferes a b =
            overlaps a b
                || (adjacent a b && not (BS.null (parserEditReplacement a) || BS.null (parserEditReplacement b)))
        overlaps a b = parserEditStart a < parserEditEnd b && parserEditStart b < parserEditEnd a
        adjacent a b = parserEditEnd a == parserEditStart b || parserEditEnd b == parserEditStart a

-- | Build independent edit batches (>= 2 edits) grouped by semantic rank/category.
independentEditBatches :: ParserEditCategory -> [ParserEdit] -> [[ParserEdit]]
independentEditBatches category edits =
    sortOn (Down . sum . map literalSavings) (filter (\g -> length g >= 2) rawGroups)
    where
        rawGroups = case category of
            EditClause ->
                partitionMax 16 (selectIndependentEdits [e | e <- edits, clauseEditRank e == safeClauseRank])
            EditExpression ->
                partitionMax
                    16
                    (selectIndependentEdits [e | e <- edits, expressionEditRank e == unwrappedExpressionRank])
            EditLiteral ->
                concatMap (partitionMax 16 . selectIndependentEdits) (groupByReplacement edits)
            _ ->
                partitionMax 16 (selectIndependentEdits edits)
        groupByReplacement [] = []
        groupByReplacement (first : rest) =
            let (same, others) = partition ((== parserEditReplacement first) . parserEditReplacement) rest
             in (first : same) : groupByReplacement others
        partitionMax _ [] = []
        partitionMax cap xs =
            let (chunk, rest) = splitAt cap xs
             in chunk : partitionMax cap rest

-- | Apply sorted disjoint parser edits to source bytes, enforcing strict byte shrink.
applyParserEdits :: ByteString -> [ParserEdit] -> Maybe ByteString
applyParserEdits source edits = case sortOn parserEditStart edits of
    [] -> Nothing
    first : rest
        | not (validEdit first) -> Nothing
        | otherwise -> go first rest [parserEditReplacement first, BS.take (parserEditStart first) source]
    where
        sourceLen = BS.length source
        validEdit e =
            let s = parserEditStart e
                eEnd = parserEditEnd e
             in s >= 0 && s < eEnd && eEnd <= sourceLen && BS.length (parserEditReplacement e) < eEnd - s
        go prev [] acc =
            let candidate = BS.concat (reverse (BS.drop (parserEditEnd prev) source : acc))
             in if BS.length candidate < sourceLen then Just candidate else Nothing
        go prev (e : more) acc
            | not (validEdit e) = Nothing
            | parserEditStart e < parserEditEnd prev = Nothing
            | parserEditStart e == parserEditEnd prev
            , not (BS.null (parserEditReplacement prev))
            , not (BS.null (parserEditReplacement e)) =
                Nothing
            | otherwise =
                let gap = BS.take (parserEditStart e - parserEditEnd prev) (BS.drop (parserEditEnd prev) source)
                 in go e more (parserEditReplacement e : gap : acc)

primaryUnitLiterals :: SyntaxContext -> StatementUnit -> [ParserEdit]
primaryUnitLiterals context unit
    | deferLiterals == DeferLiteralsOff = eligible
    | otherwise = reverse (fst (foldl' step ([], Set.empty) eligible))
    where
        deferLiterals = syntaxDeferLiteralVariants context
        eligible = unitNonRecursiveLiterals context unit
        step (!acc, !seenLitSpans) edit
            | isPrimaryLiteralEdit deferLiterals seenLitSpans edit =
                (edit : acc, Set.insert (parserEditStart edit, parserEditEnd edit) seenLitSpans)
            | otherwise = (acc, seenLitSpans)

unitEligibleLiterals :: SyntaxContext -> StatementUnit -> [ParserEdit]
unitEligibleLiterals context unit =
    [ edit
    | edit <- primaryUnitLiterals context unit
    , parserEditLabel edit == "simplify-literal"
    , literalSavings edit > 0
    ]

distributedLiteralGroups ::
    SyntaxContext ->
    Int ->
    Map.Map (ByteString, Bool) IntSet.IntSet ->
    Maybe (ByteString, Bool) ->
    [StatementUnit] ->
    [((ByteString, Bool), [[(Int, ParserEdit)]])]
distributedLiteralGroups context maxBatch rejectedByRepl preferredRepl units =
    sortOn groupOrder nonEmptyGroups
    where
        byRepl = foldl' addUnit Map.empty units
        addUnit groups unit =
            let sid = unStatementId (stmtId (unitStatement unit))
                bestPerRepl =
                    Map.fromListWith
                        (\new old -> if literalSavings new > literalSavings old then new else old)
                        [((parserEditReplacement e, parserEditStringLiteral e), e) | e <- unitEligibleLiterals context unit]
                addEntry acc key edit
                    | IntSet.member sid (Map.findWithDefault IntSet.empty key rejectedByRepl) = acc
                    | otherwise = Map.insertWith (++) key [(sid, edit)] acc
             in Map.foldlWithKey' addEntry groups bestPerRepl

        chunkEdits xs@(_ : _ : _) =
            let (chunk, rest) = splitAt (max 2 maxBatch) xs
             in chunk : chunkEdits rest
        chunkEdits _ = []

        chunkSavings = sum . map (literalSavings . snd)

        nonEmptyGroups =
            [ (key, sortOn (Down . chunkSavings) chunks)
            | (key, revPairs) <- Map.toList byRepl
            , let ordered = sortOn (Down . literalSavings . snd) (reverse revPairs)
                  chunks = chunkEdits ordered
            , not (null chunks)
            ]

        groupOrder (key@(repl, isStr), firstChunk : _) =
            ( if preferredRepl == Just key then (0 :: Int) else 1
            , deferLowYield && any (parserEditLowYield . snd) firstChunk
            , Down (chunkSavings firstChunk)
            , BS.length repl
            , isStr
            , repl
            )
        groupOrder ((repl, isStr), []) = (2, False, Down 0, BS.length repl, isStr, repl)
        deferLowYield = syntaxDeferLiteralVariants context /= DeferLiteralsOff

data BatchProbeOutcome
    = BatchSkipped !(HashSet.HashSet ByteString)
    | BatchRejected !(HashSet.HashSet ByteString)
    | BatchAccepted !ByteString

-- | Build, early-reject check, and test a single syntax batch candidate.
probeSyntaxBatch ::
    SyntaxContext ->
    IORef SyntaxStats ->
    Int ->
    ByteString ->
    HashSet.HashSet ByteString ->
    SyntaxCandidate ->
    IO BatchProbeOutcome
probeSyntaxBatch context stats unitCount sql seenRejected candidate = evalContT do
    (built, buildMicros) <- liftIO $ timeSyntaxCandidateBuild context sql candidate
    candidateBytes <- case built of
        Just b
            | BS.length b < BS.length sql && not (HashSet.member b seenRejected) -> pure b
        _ -> earlyExit (BatchSkipped seenRejected)
    liftIO $ recordCandidateBuildTiming (syntaxStore context) (candidateTag candidate) buildMicros
    earlyBytes <-
        liftIO $
            checkEarlyRejectionBytesTracked (syntaxTracker context) (syntaxCert context) True candidateBytes
    for_ earlyBytes \reason -> do
        liftIO $ recordEarlyRejectSkip (syntaxStore context) reason
        earlyExit (BatchSkipped (HashSet.insert candidateBytes seenRejected))
    liftIO do
        (tested, elapsed, accepted) <-
            testSyntaxWindow context unitCount [(0, candidate, candidateBytes)]
        modifyIORef' stats (recordSyntaxTest tested elapsed accepted)
        case accepted of
            Just (_, _, bytes) -> do
                logAcceptedSyntaxEdit context candidate sql bytes
                pure (BatchAccepted bytes)
            Nothing -> pure (BatchRejected (HashSet.insert candidateBytes seenRejected))

{- | Try atomic cross-statement literal batches (at most one literal per statement
per batch, sharing the same replacement) before the single-edit literal sweep.
Refreshes accepted batches by reparsing only the touched statements through the
Rust parser with a full parse fallback.
-}
exhaustDistributedLiteralBatches ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget])
exhaustDistributedLiteralBatches context stats initSQL initUnits initPhases initBundles
    | null (drop (minStmts - 1) (filter (not . null . unitEligibleLiterals context) initUnits)) =
        pure (initSQL, initUnits, initPhases, initBundles)
    | otherwise = go initSQL initUnits initPhases initBundles HashSet.empty Map.empty Nothing
    where
        cfg = syntaxDistributedBatchConfig context
        minStmts = max 2 (distributedMinStatements cfg)
        maxBatch = max 2 (distributedMaxBatchSize cfg)
        maxMisses = max 1 (distributedMaxMisses cfg)

        go sql units phases bundles seenRejected rejectedByRepl preferredRepl =
            let groups = distributedLiteralGroups context maxBatch rejectedByRepl preferredRepl units
                firstPerGroup = [(k, b) | (k, b : _) <- groups]
                nextChunks = [(k, b) | (k, _ : b : _) <- take 1 groups]
                queue = take 2 firstPerGroup ++ nextChunks
             in search sql units phases bundles seenRejected rejectedByRepl maxMisses queue

        search sql units phases bundles _ _ misses _ | misses <= 0 = pure (sql, units, phases, bundles)
        search sql units phases bundles _ _ _ [] = pure (sql, units, phases, bundles)
        search sql units phases bundles seenRejected rejectedByRepl misses ((key, chunk) : rest) = do
            let batch = map snd chunk
                candidate = BatchCandidate EditLiteral "distributed-literal-batch" batch
            probeSyntaxBatch context stats (length units) sql seenRejected candidate >>= \case
                BatchSkipped seen' ->
                    search sql units phases bundles seen' rejectedByRepl misses rest
                BatchRejected seen' ->
                    let sids = IntSet.fromList (map fst chunk)
                        rejectedByRepl' = Map.insertWith IntSet.union key sids rejectedByRepl
                     in search sql units phases bundles seen' rejectedByRepl' (misses - 1) rest
                BatchAccepted bytes -> do
                    (nextUnits, nextPhases, nextBundles) <-
                        updateSyntaxBatchSnapshot context EditLiteral units phases bundles batch bytes
                    let rejectedByRepl' = Map.filterWithKey (\k _ -> k == key) rejectedByRepl
                    go bytes nextUnits nextPhases nextBundles seenRejected rejectedByRepl' (Just key)

-- | Try independent AST edit batches with DDMin-style recursive subdivision on rejection.
exhaustIndependentAstBatches ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEditCategory ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget])
exhaustIndependentAstBatches context stats initSQL initUnits initPhases initBundles category =
    go initSQL initUnits initPhases initBundles HashSet.empty
    where
        go sql units phases bundles seenRejected =
            let categoryEdits
                    | category == EditLiteral = allNonRecursiveLiterals context units
                    | otherwise = phaseCandidates category phases
                batches = independentEditBatches category categoryEdits
             in search sql units phases bundles seenRejected (8 :: Int) batches

        search sql units phases bundles _ misses _ | misses <= 0 = pure (sql, units, phases, bundles)
        search sql units phases bundles _ _ [] = pure (sql, units, phases, bundles)
        search sql units phases bundles seenRejected misses (batch : rest) = do
            let candidate = BatchCandidate category "independent-ast-batch" batch
            probeSyntaxBatch context stats (length units) sql seenRejected candidate >>= \case
                BatchSkipped seen' ->
                    search sql units phases bundles seen' misses rest
                BatchRejected seen' ->
                    let (left, right) = splitAt (length batch `div` 2) batch
                        subdivisions = [sub | sub <- [left, right], length sub >= 2]
                     in search sql units phases bundles seen' (misses - 1) (subdivisions ++ rest)
                BatchAccepted bytes -> do
                    (nextUnits, nextPhases, nextBundles) <-
                        updateSyntaxBatchSnapshot context category units phases bundles batch bytes
                    go bytes nextUnits nextPhases nextBundles seenRejected

-- | Exhaustively apply edits of one category until no further reductions succeed.
exhaustSyntaxCategory ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEditCategory ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget], Bool)
exhaustSyntaxCategory context stats sql units phases bundles category =
    exhaustSyntaxStream
        context
        stats
        sql
        units
        phases
        bundles
        (Just category)
        (\buckets targets -> initialSyntaxStream context buckets targets category)

exhaustSyntaxStream ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    Maybe ParserEditCategory ->
    (SyntaxIndex -> [ColumnTarget] -> Int -> [SyntaxCandidate]) ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget], Bool)
exhaustSyntaxStream context stats sourceSQL sourceUnits phases bundles mCategory stream =
    sweep False 0 sourceSQL sourceUnits phases bundles (stream phases bundles 0)
    where
        sweep changed _ currentSQL currentUnits currentPhases currentBundles [] =
            pure (currentSQL, currentUnits, currentPhases, currentBundles, changed)
        sweep changed ordinal currentSQL currentUnits currentPhases currentBundles candidates = do
            (tested, elapsed, accepted) <-
                testSyntaxCandidates context currentUnits currentSQL ordinal candidates
            modifyIORef' stats (recordSyntaxTest tested elapsed accepted)
            case accepted of
                Nothing -> pure (currentSQL, currentUnits, currentPhases, currentBundles, changed)
                Just (acceptedOrdinal, candidate, acceptedBytes) -> do
                    logAcceptedSyntaxEdit context candidate currentSQL acceptedBytes
                    (freshUnits, freshPhases, freshBundles) <-
                        refreshAcceptedSyntaxSnapshot
                            context
                            mCategory
                            currentUnits
                            currentPhases
                            currentBundles
                            candidate
                            acceptedBytes
                    let freshCandidates = stream freshPhases freshBundles acceptedOrdinal
                    sweep True acceptedOrdinal acceptedBytes freshUnits freshPhases freshBundles freshCandidates

refreshAcceptedSyntaxSnapshot ::
    SyntaxContext ->
    Maybe ParserEditCategory ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    SyntaxCandidate ->
    ByteString ->
    IO ([StatementUnit], SyntaxIndex, [ColumnTarget])
refreshAcceptedSyntaxSnapshot context mCategory currentUnits currentPhases currentBundles (SingleCandidate edit) acceptedBytes
    | let editCat = fromMaybe (parserEditCategory edit) mCategory
    , editCat /= EditColumn =
        updateSyntaxSnapshot context editCat currentUnits currentPhases currentBundles edit acceptedBytes
refreshAcceptedSyntaxSnapshot context _ _ _ _ _ acceptedBytes =
    parseSyntaxSnapshot context acceptedBytes

-- | Run one window of candidates and return the first accepted candidate.
testSyntaxWindow ::
    SyntaxContext ->
    Int ->
    [(Int, SyntaxCandidate, ByteString)] ->
    IO (Int, Word64, Maybe (Int, SyntaxCandidate, ByteString))
testSyntaxWindow context currentCount windowTasks = do
    mapM_ (\(_, _, bytes) -> reportCandidateCount (syntaxPool context) bytes currentCount) windowTasks
    started <- getMonotonicTimeNSec
    outcomes <-
        testOrderedTagged
            (syntaxPool context)
            [(candidateTag candidate, bytes) | (_, candidate, bytes) <- windowTasks]
    finished <- getMonotonicTimeNSec
    let retired = zip windowTasks outcomes
    pure
        ( length outcomes
        , finished - started
        , listToMaybe
            [ (ordinal, candidate, bytes)
            | ((ordinal, candidate, bytes), (outcome, _)) <- retired
            , outcome == OracleAccepted
            ]
        )

recordSyntaxTest :: Int -> Word64 -> Maybe a -> SyntaxStats -> SyntaxStats
recordSyntaxTest tested elapsed accepted stats =
    stats
        { syntaxAttempts = syntaxAttempts stats + tested
        , syntaxAccepted = syntaxAccepted stats + maybe 0 (const 1) accepted
        , syntaxElapsed = syntaxElapsed stats + elapsed
        }

-- | Initial candidate stream for the given syntax category.
initialSyntaxStream ::
    SyntaxContext ->
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEditCategory ->
    Int ->
    [SyntaxCandidate]
initialSyntaxStream context phases bundles category ordinal
    | category == EditColumn && syntaxMode context == ColumnAtomic =
        map BundleCandidate (drop ordinal bundles)
            ++ map SingleCandidate (phaseCandidatesFrom category phases (max 0 (ordinal - length bundles)))
    | category == EditList && syntaxAdaptiveValues context =
        map SingleCandidate (drop ordinal (valuesChunkEdits False (phaseCandidates category phases)))
    | otherwise = map SingleCandidate (phaseCandidatesFrom category phases ordinal)

-- The frontend emits each VALUES list's chunks coarse-to-fine, then singles.
-- Leave its coarse pair in the regular sweep and revisit the finer candidates
-- only when an entire cycle finds no reductions.
valuesChunkEdits :: Bool -> [ParserEdit] -> [ParserEdit]
valuesChunkEdits _ [] = []
valuesChunkEdits deferred edits@(edit : rest)
    | isChunk edit =
        let (chunks, remaining) = span isChunk edits
            (coarse, fine) = splitAt 2 chunks
         in (if deferred then fine else coarse) ++ valuesChunkEdits deferred remaining
    | deferred = valuesChunkEdits deferred rest
    | otherwise = edit : valuesChunkEdits deferred rest
    where
        isChunk candidate = parserEditLabel candidate == "remove-values_row-chunk"

-- | Build and execute eligible syntax candidates as the ordered pool asks for them.
testSyntaxCandidates ::
    SyntaxContext ->
    [StatementUnit] ->
    ByteString ->
    Int ->
    [SyntaxCandidate] ->
    IO (Int, Word64, Maybe (Int, SyntaxCandidate, ByteString))
testSyntaxCandidates context currentUnits currentSQL startOrdinal candidates = do
    started <- getMonotonicTimeNSec
    (tested, accepted) <-
        testOrderedGenerated pool nextCandidate retire (0, Nothing) (startOrdinal, candidates)
    finished <- getMonotonicTimeNSec
    pure (tested, finished - started, accepted)
    where
        pool = syntaxPool context
        currentCount = length currentUnits
        nextCandidate (_, []) = pure Nothing
        nextCandidate (ordinal, candidate : rest) = do
            let next = (ordinal + 1, rest)
            stepCandidate candidate >>= \case
                Nothing -> nextCandidate next
                Just bytes -> do
                    reportCandidateCount pool bytes currentCount
                    pure (Just ((candidateTag candidate, bytes, (ordinal, candidate)), next))

        retire (!tested, _) (ordinal, candidate) bytes (outcome, _) =
            pure (tested + 1, if outcome == OracleAccepted then Just (ordinal, candidate, bytes) else Nothing)

        stepCandidate candidate = runMaybeT do
            let store = syntaxStore context
            for_ (singleEdit candidate) \edit ->
                rejectIfEarly store
                    =<< liftIO
                        ( checkEarlyRejectionSyntaxEditTracked
                            (syntaxTracker context)
                            currentUnits
                            (syntaxCert context)
                            edit
                        )
            (built, buildMicros) <- liftIO $ timeSyntaxCandidateBuild context currentSQL candidate
            bytes <- case built of
                Nothing -> liftIO (recordGeneratedSkipped store "nonshrinking") >> empty
                Just b -> pure b
            liftIO $ recordCandidateBuildTiming store (candidateTag candidate) buildMicros
            rejectIfEarly store
                =<< liftIO (checkEarlyRejectionBytesTracked (syntaxTracker context) (syntaxCert context) True bytes)
            pure bytes

        singleEdit (SingleCandidate edit) = Just edit
        singleEdit _ = Nothing

-- | Assemble candidate bytes with optional build timing.
timeSyntaxCandidateBuild ::
    SyntaxContext -> ByteString -> SyntaxCandidate -> IO (Maybe ByteString, Int)
timeSyntaxCandidateBuild context currentSQL candidate = case syntaxStore context of
    -- Unmeasured: no clocks, no extra forcing, same engine.
    Nothing -> pure (buildCandidate, 0)
    Just _ -> do
        started <- getMonotonicTimeNSec
        -- Force the assembled bytes before stopping the clock; an O(1) length
        -- touch, never a deepseq.
        let forced = maybe 0 BS.length buildCandidate
        forced `seq` pure ()
        finished <- getMonotonicTimeNSec
        pure (buildCandidate, fromIntegral ((finished - started) `div` 1000))
    where
        buildCandidate = case candidate of
            SingleCandidate edit -> singleBytes currentSQL edit
            BundleCandidate target -> bundleBytes currentSQL target
            BatchCandidate _ _ edits -> applyParserEdits currentSQL edits

logAcceptedSyntaxEdit :: SyntaxContext -> SyntaxCandidate -> ByteString -> ByteString -> IO ()
logAcceptedSyntaxEdit context (SingleCandidate edit) currentSQL acceptedBytes =
    let editStart = parserEditStart edit
        editEnd = parserEditEnd edit
        replacement = parserEditReplacement edit
     in logInfo'
            (syntaxLogger context)
            "reducer"
            "Accepted byte-preserving syntax rewrite"
            [ ("rewrite_kind", toJSON (parserEditLabel edit))
            , ("rewrite_category", toJSON (parserEditCategory edit))
            , ("span_start", toJSON editStart)
            , ("span_end", toJSON editEnd)
            , ("replacement", toJSON (TE.decodeUtf8Lenient replacement))
            , ("bytes_before", toJSON (BS.length currentSQL))
            , ("bytes_after", toJSON (BS.length acceptedBytes))
            ]
logAcceptedSyntaxEdit context (BundleCandidate target) currentSQL acceptedBytes =
    logInfo'
        (syntaxLogger context)
        "reducer"
        "Accepted atomic column co-reduction"
        [ ("rewrite_kind", toJSON ("remove-column-bundle" :: T.Text))
        , ("rewrite_category", toJSON EditColumn)
        , ("table", toJSON (nameText (targetTable target)))
        , ("column", toJSON (identText (targetColumn target)))
        , ("sites", toJSON (length (targetSites target)))
        , ("bytes_before", toJSON (BS.length currentSQL))
        , ("bytes_after", toJSON (BS.length acceptedBytes))
        ]
logAcceptedSyntaxEdit context (BatchCandidate category label edits) currentSQL acceptedBytes =
    logInfo'
        (syntaxLogger context)
        "reducer"
        "Accepted syntax edit batch"
        [ ("rewrite_kind", toJSON label)
        , ("rewrite_category", toJSON category)
        , ("edits_count", toJSON (length edits))
        , ("bytes_before", toJSON (BS.length currentSQL))
        , ("bytes_after", toJSON (BS.length acceptedBytes))
        ]

logSyntaxStats :: Logger -> SyntaxStats -> IO ()
logSyntaxStats logger finalStats =
    let accepted = syntaxAccepted finalStats
     in if accepted > 0
            then
                logInfo'
                    logger
                    "reducer"
                    "Syntax rewrites accepted"
                    [ ("accepted_edits", toJSON accepted)
                    , ("attempts", toJSON (syntaxAttempts finalStats))
                    , ("window_seconds", toJSON (seconds (syntaxElapsed finalStats)))
                    ]
            else
                logDebug'
                    logger
                    "reducer"
                    "Syntax replacement completed with no accepted edits"
                    [ ("attempts", toJSON (syntaxAttempts finalStats))
                    , ("window_seconds", toJSON (seconds (syntaxElapsed finalStats)))
                    ]

replaceSpan :: Int -> Int -> ByteString -> ByteString -> ByteString
replaceSpan start end replacement source
    | BS.null replacement =
        let !prefix = BS.take start source
            !suffix = BS.drop end source
         in BS.append prefix suffix
    | start == 0 =
        BS.append replacement (BS.drop end source)
    | end == BS.length source =
        BS.append (BS.take start source) replacement
    | otherwise =
        BS.concat [BS.take start source, replacement, BS.drop end source]

candidateTag :: SyntaxCandidate -> FamilyTag
candidateTag (SingleCandidate edit) = TagSyntax (parserEditCategory edit) (parserEditLabel edit)
candidateTag (BundleCandidate _) = TagBundle "remove-column-bundle"
candidateTag (BatchCandidate category label _) = TagSyntax category label

singleBytes :: ByteString -> ParserEdit -> Maybe ByteString
singleBytes currentSQL edit
    | BS.length (parserEditReplacement edit) >= parserEditEnd edit - parserEditStart edit = Nothing
    | otherwise =
        Just
            (replaceSpan (parserEditStart edit) (parserEditEnd edit) (parserEditReplacement edit) currentSQL)

bundleBytes :: ByteString -> ColumnTarget -> Maybe ByteString
bundleBytes currentSQL target =
    applySpans [(siteStart site, siteEnd site) | site <- targetSites target] currentSQL

-- | Apply non-overlapping deletion spans to the source bytes in a single pass.
applySpans :: [(Int, Int)] -> ByteString -> Maybe ByteString
applySpans spans source = case spans of
    [] -> Nothing
    (start, end) : rest
        | start < 0 || start >= end || end > sourceLength -> Nothing
        | otherwise -> go rest end [BS.take start source] (end - start)
    where
        sourceLength = BS.length source
        go [] previousEnd acc removed =
            let candidate = BS.concat (reverse (BS.drop previousEnd source : acc))
             in if removed > 0 && BS.length candidate < sourceLength then Just candidate else Nothing
        go ((start, end) : more) previousEnd acc removed
            | start < previousEnd || start >= end || end > sourceLength = Nothing
            | otherwise =
                go
                    more
                    end
                    (BS.take (start - previousEnd) (BS.drop previousEnd source) : acc)
                    (removed + (end - start))

-- | Record statement assembly time if metrics are enabled.
timeStatementBuild :: Maybe MetricsStore -> ByteString -> IO ByteString
timeStatementBuild store = timeTaggedStatementBuild store TagStatement

timeTaggedStatementBuild :: Maybe MetricsStore -> FamilyTag -> ByteString -> IO ByteString
timeTaggedStatementBuild Nothing _ bytes = pure bytes
timeTaggedStatementBuild (Just store) tag bytes = do
    started <- getMonotonicTimeNSec
    BS.length bytes `seq` pure ()
    finished <- getMonotonicTimeNSec
    recordCandidateBuildTiming
        (Just store)
        tag
        (fromIntegral ((finished - started) `div` 1000))
    pure bytes

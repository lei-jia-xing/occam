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
    defaultReductionConfigFor,
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

import Data.ByteString (ByteString)
import Occam.Certificate (FailingPrefixCache)
import Occam.Core.Semantics (SessionInit)
import Occam.Core.Types (ResultStatement, ResultTrace)
import Occam.Logger (Logger, nullLogger)
import Occam.Parser (SQLDialect, StatementUnit)
import Occam.Pool (OraclePool)
import Occam.Reducer.Audit (
    AuditStats (..),
    statementAudit,
    statementAuditWithLogger,
    statementAuditWithParser,
 )
import Occam.Reducer.Closure (closureDdmin)
import Occam.Reducer.Config (
    ColumnMode (..),
    DistributedBatchConfig (..),
    LaneOrdering (..),
    LaneScheduling (..),
    LiteralDeferralMode (..),
    ProjectionWindow (..),
    ProjectionWindowOrder (..),
    RankedScheduleConfig (..),
    ReductionConfig (..),
    ReductionEvidence (..),
    ReductionResult (..),
    ReductionStage (..),
    SingletonResume (..),
    StageEvent (..),
    StageReporter,
    StatementLane (..),
    StatementSearchPolicy (..),
    columnModeName,
    convergedRankedScheduleConfig,
    defaultDistributedBatchConfig,
    defaultRankedScheduleConfig,
    defaultReductionConfigFor,
    nullStageReporter,
    reductionSearchPolicy,
    reductionStageName,
    shouldActivateRankedFallback,
    statementRanked,
    statementRankedFallback,
    statementSearchPolicyName,
 )
import Occam.Reducer.DDMin (
    statementDdmin,
    statementDdminWithOrder,
    statementDdminWithParser,
 )
import Occam.Reducer.Pipeline (
    ObservedResultEvidence (..),
    reduceSQLWithObservedResultSeeded,
 )
import Occam.Reducer.Slice (findInterestingPrefix)
import Occam.Reducer.Statement (
    closeSet,
    closeSetFast,
    cutRemovedStatementIds,
    cutRemovedStatementInts,
    cutRemovedStatements,
    isValidClosure,
    isValidClosureFast,
    partitionChunks,
    partitionStatementsByBytes,
    partitionStatementsByWeights,
    statementIds,
    statementIdsInt,
 )
import Occam.Reducer.Support (
    parseStatementsFast,
    reuseLatestSQLWithSeed,
 )
import Occam.Reducer.Syntax.Snapshot (
    applyParserEdits,
    independentEditBatches,
    replaceSpan,
    selectIndependentEdits,
 )

-- | Reduce SQL test case using default configuration and dialect.
reduceSQL :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
reduceSQL dialect = reduceSQLWithLogger dialect nullLogger

-- | Reduce SQL test case using custom logger and dialect.
reduceSQLWithLogger :: SQLDialect -> Logger -> OraclePool -> ByteString -> IO ByteString
reduceSQLWithLogger dialect =
    reduceSQLWithConfig (defaultReductionConfigFor dialect)

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

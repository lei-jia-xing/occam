-- | Multi-stage reduction loop and stage orchestration pipeline.
module Occam.Reducer.Pipeline (
    ObservedResultEvidence (..),
    evidenceResultStatement,
    evidenceResultTrace,
    reduceSQLWithObservedResultSeeded,
) where

import Control.Exception.Safe (finally)
import Control.Monad (when)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Occam.Certificate (
    EarlyRejectCertifier,
    EarlyRejectTracker,
    FailingPrefixCache,
    newEarlyRejectTrackerWithFailures,
 )
import Occam.Core (
    ResultStatement,
    ResultTrace,
    SessionInit,
    Statement,
    oracleAccepts,
    traceResultStatement,
 )
import Occam.Dependency (DependencyGraph (..))
import Occam.Logger (Logger, logInfo', logWarn)
import Occam.Parser (StatementUnit)
import Occam.Pool (OraclePool, poolMetricsStore, poolOracle)
import Occam.Proposals (projectTransactionBlocks)
import Occam.Reducer.Audit (
    AuditStats (..),
    statementAuditWithCert,
 )
import Occam.Reducer.Closure (closureSearchWithAnalysis)
import Occam.Reducer.Config (
    ColumnMode (..),
    ReductionConfig (..),
    ReductionEvidence (..),
    ReductionResult (..),
    ReductionStage (..),
    StageReporter,
    StatementSearchPolicy (..),
    shouldActivateRankedFallback,
 )
import Occam.Reducer.DDMin (statementDdminWithCert)
import Occam.Reducer.Groups (proposeGroupDeletionsWithAnalysis)
import Occam.Reducer.Slice (sliceWithAnalysis)
import Occam.Reducer.Support (
    SQLAnalysis (..),
    StatementParser,
    analyzeSQL,
    buildCertifierTracked,
    columnParseOptions,
    logEarlyRejectStats,
    parseStatements,
    reuseLatestSQL,
    reuseLatestSQLWithSeed,
    timedReductionStage,
 )
import Occam.Reducer.Syntax (reduceSyntaxWithDialect)

data ObservedResultEvidence
    = ObservedStatementOnly !ResultStatement
    | ObservedFullTrace !ResultTrace

evidenceResultStatement :: ObservedResultEvidence -> ResultStatement
evidenceResultStatement (ObservedStatementOnly rs) = rs
evidenceResultStatement (ObservedFullTrace tr) = traceResultStatement tr

evidenceResultTrace :: ObservedResultEvidence -> Maybe ResultTrace
evidenceResultTrace (ObservedStatementOnly _) = Nothing
evidenceResultTrace (ObservedFullTrace tr) = Just tr

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

sameEvidenceBytes :: ByteString -> Maybe ReductionEvidence -> Bool
sameEvidenceBytes sql = maybe False ((== sql) . evidenceBytes)

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
            let env = IterationEnv observeResult tracker reporter config logger pool parse analyze
            loop env (1 :: Int) initialSQL Nothing False
                `finally` logEarlyRejectStats logger "reduction" tracker
    where
        loop env iteration currentSQL priorEvidence priorFullFixedPoint = do
            logInfo'
                logger
                "reducer"
                "Starting reduction iteration"
                [ ("iteration", toJSON iteration)
                , ("current_bytes", toJSON (BS.length currentSQL))
                ]
            (nextSQL, evidence, fullFixedPoint) <-
                reduceIterationWithEvidence env currentSQL priorEvidence priorFullFixedPoint
            if BS.length nextSQL < BS.length currentSQL
                then loop env (iteration + 1) nextSQL evidence fullFixedPoint
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
                    let confirmed = if sameEvidenceBytes currentSQL evidence then evidence else Nothing
                    pure (ReductionResult currentSQL confirmed)

reduceIterationWithEvidence ::
    IterationEnv ->
    ByteString ->
    Maybe ReductionEvidence ->
    Bool ->
    IO (ByteString, Maybe ReductionEvidence, Bool)
reduceIterationWithEvidence env initialSQL priorEvidence priorFullFixedPoint = do
    (sliced, reusableAnalysis) <- runSlicingAndGroupsPhase env initialSQL priorEvidence
    if priorFullFixedPoint && sameEvidenceBytes sliced priorEvidence
        then pure (sliced, priorEvidence, True)
        else do
            let config = iterConfig env
                unrestrictedClosure = reductionClosure config && reductionUnrestricted config
            searchAnalysis <-
                if unrestrictedClosure
                    then Just <$> maybe (iterAnalyze env sliced) pure reusableAnalysis
                    else pure reusableAnalysis
            let useEarlyUnrestricted =
                    unrestrictedClosure
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
                if unrestrictedClosure && (not useEarlyUnrestricted || searched /= searchInput)
                    then runUnrestrictedSearch env effectiveStatementPolicy searched
                    else pure searched
            (out, ev) <- runInterleaveStages env priorEvidence fallback
            pure (out, ev, out == sliced)

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
            if reductionGroupProposals config && not (sameEvidenceBytes afterSlice priorEvidence)
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
    , not (sameEvidenceBytes initialSQL priorEvidence) = do
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
            remainingStmts <-
                maybe
                    (length . snd <$> iterParse env sliced)
                    (pure . length . analyzedStatements)
                    searchAnalysis
            let alreadyConfirmed = sameEvidenceBytes sliced priorEvidence
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
    | sameEvidenceBytes base priorEvidence =
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

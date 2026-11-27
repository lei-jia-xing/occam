-- | Dependency-closure-constrained delta debugging.
module Occam.Reducer.Closure (
    closureDdmin,
    closureDdminWithLogger,
    closureDdminWithCert,
    closureSearchWithAnalysis,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (evalContT)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Occam.Certificate (
    EarlyRejectCertifier,
    EarlyRejectTracker,
    emptyEarlyRejectCertifier,
    newEarlyRejectTracker,
 )
import Occam.Core (Statement (..), StatementId (..))
import Occam.Dependency (DependencyGraph (..))
import Occam.Logger (Logger, logDebug', logInfo', nullLogger)
import Occam.Pool (OraclePool)
import Occam.Reducer.Config (StatementSearchPolicy (..))
import Occam.Reducer.DDMin (searchStatementsWithCert)
import Occam.Reducer.Statement (
    closeSetFast,
    isValidClosureFast,
    statementIdsInt,
    statementSQL,
    statementSpanBytes,
 )
import Occam.Reducer.Support (SQLAnalysis (..), earlyExit)
import Occam.Slicer (closeFromBoundary, successorIndex)

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
    , ceUseSinkCheck :: !Bool
    , cePredecessors :: !(IntMap.IntMap IntSet.IntSet)
    , ceSuccessors :: !(IntMap.IntMap IntSet.IntSet)
    , ceWeights :: !(IntMap.IntMap Int)
    , ceCache :: !(IORef (HashMap IntSet.IntSet IntSet.IntSet))
    , ceSinks :: !(IORef (Maybe (IntSet.IntSet, IntSet.IntSet)))
    , ceStats :: !(IORef ClosureStats)
    }

data ClosureStats = ClosureStats
    { closureRequests :: !Int
    , closureCacheHits :: !Int
    , closureUnchangedHits :: !Int
    , closureGainStatements :: !Int
    , closureGainBytes :: !Int
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
    sinksCache <- newIORef Nothing
    closureStats <- newIORef (ClosureStats 0 0 0 0 0)
    let predecessors = graphFastEdges graph
        initialIds = statementIdsInt initialStatements
        initialClosed = isValidClosureFast graph initialIds
        env =
            ClosureEnv
                { ceGraph = graph
                , ceInitialClosed = initialClosed
                , ceUseSinkCheck =
                    initialClosed
                        && not (IntMap.null predecessors)
                        && IntSet.foldr (\sid rest -> hasEarlierPredecessors predecessors sid && rest) True initialIds
                , cePredecessors = predecessors
                , ceSuccessors = successorIndex predecessors
                , ceWeights =
                    IntMap.fromList [(unStatementId (stmtId s), statementSpanBytes s) | s <- initialStatements]
                , ceCache = closureCache
                , ceSinks = sinksCache
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

-- Graphs constructed by callers may contain cycles, so check the temporal
-- invariant that the sink criterion uses before applying it to any subset.
hasEarlierPredecessors :: IntMap.IntMap IntSet.IntSet -> Int -> Bool
hasEarlierPredecessors predecessors sid = IntSet.null deps || IntSet.findMax deps < sid
    where
        deps = IntMap.findWithDefault IntSet.empty sid predecessors

lookupCurrentSinks :: ClosureEnv -> IntSet.IntSet -> IO IntSet.IntSet
lookupCurrentSinks env currentInts = do
    cached <- readIORef (ceSinks env)
    case cached of
        Just (cachedCurrent, sinks) | cachedCurrent == currentInts -> pure sinks
        _ -> do
            let !sinks = IntSet.filter hasNoConsumer currentInts
            writeIORef (ceSinks env) (Just (currentInts, sinks))
            pure sinks
    where
        hasNoConsumer sid =
            IntSet.disjoint currentInts (IntMap.findWithDefault IntSet.empty sid (ceSuccessors env))

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
    when (ceUseSinkCheck env) do
        -- See closeSet_same_members_iff_all_sinks in proofs/Proofs/DAG.lean.
        sinks <- liftIO $ lookupCurrentSinks env currentInts
        when (sinks `IntSet.isSubsetOf` seedInts) do
            liftIO $ modifyIORef' (ceStats env) (\s -> s {closureUnchangedHits = closureUnchangedHits s + 1})
            earlyExit (Just currentInts)
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
            (("gain_statements", toJSON gainStmts) : ("gain_bytes", toJSON gainBytes) : counts)
    | otherwise =
        logDebug'
            logger
            "reducer"
            "Closure search found no further reductions"
            counts
    where
        gainStmts = closureGainStatements stats
        gainBytes = closureGainBytes stats
        counts =
            [ ("closure_requests", toJSON (closureRequests stats))
            , ("closure_cache_hits", toJSON (closureCacheHits stats))
            , ("closure_unchanged_hits", toJSON (closureUnchangedHits stats))
            ,
                ( "closure_computations"
                , toJSON (closureRequests stats - closureCacheHits stats - closureUnchangedHits stats)
                )
            ]

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

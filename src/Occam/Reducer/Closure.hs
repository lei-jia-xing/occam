-- | Dependency-closure-constrained delta debugging.
module Occam.Reducer.Closure (
    closureDdmin,
    closureDdminWithLogger,
    closureDdminWithCert,
    closureSearchWithAnalysis,
) where

import Control.Monad (unless)
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
import Occam.Slicer (backwardSliceFast, closeFromBoundary, successorIndex)

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

data ClosureStats = ClosureStats
    { closureRequests :: !Int
    , closureCacheHits :: !Int
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

-- | Dependency graph analysis for SQL statements.
module Occam.Dependency (
    DependencyGraph (..),
    emptyGraph,
    buildGraph,
    analyzeStatements,
    cascadeWrites,
) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (
    BarrierReason (..),
    BarrierScope (..),
    CatalogOp (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    SessionInit,
    Statement (..),
    StatementId (..),
 )
import Occam.Dependency.Alter (
    processAlterTable,
    processAttachInherit,
    processAttachPartition,
    processDetachInherit,
    processDetachPartition,
 )
import Occam.Dependency.Barrier (barrierKeysForScope, lastBarrierDeps, touchersForBarrier)
import Occam.Dependency.Define (
    processDefineIndex,
    processDefineSchema,
    processDefineSequence,
    processDefineTable,
    processDefineTrigger,
    processDefineType,
    processDefineView,
 )
import Occam.Dependency.Drop (processDrop)
import Occam.Dependency.Enforcement (cascadeWrites)
import Occam.Dependency.Rename (processRename)
import Occam.Dependency.Session (applySessionEffects)
import Occam.Dependency.State (
    BarrierKey (..),
    Eval (..),
    ScanState (..),
    addBarrier,
    addDeps,
    evalToState,
    initialEval,
    initialScanState,
 )
import Occam.Dependency.Transaction (
    applyTxEffects,
    ensureFrame,
    ensureFrameForOps,
    finishStatement,
    settleAutocommit,
 )
import Occam.Dependency.Write (processDataWrite, processRead, processRefresh, processSequenceWrite)
import Occam.Facts (StatementFacts (..), StatementUnit (..))

-- | Directed acyclic dependency graph across SQL statements.
data DependencyGraph = DependencyGraph
    { graphStmts :: !(Set StatementId)
    , graphFacts :: !(Map StatementId SemanticFacts)
    , graphEdges :: !(Map StatementId (Set StatementId))
    , graphFastEdges :: !(IntMap IntSet)
    , graphHasGlobalBarrier :: !Bool
    , graphBarrierActions :: !Int
    , graphBarrierStatements :: !Int
    , graphBarrierEdges :: !Int
    , graphEdgeCount :: !Int
    }
    deriving (Eq, Show)

-- | Empty dependency graph.
emptyGraph :: DependencyGraph
emptyGraph =
    DependencyGraph
        { graphStmts = Set.empty
        , graphFacts = Map.empty
        , graphEdges = Map.empty
        , graphFastEdges = IntMap.empty
        , graphHasGlobalBarrier = False
        , graphBarrierActions = 0
        , graphBarrierStatements = 0
        , graphBarrierEdges = 0
        , graphEdgeCount = 0
        }

extractFacts :: StatementUnit -> SemanticFacts
extractFacts unit =
    maybe
        SemanticFacts
            { factOps = []
            , factSession = []
            , factPreTx = []
            , factPostTx = []
            , factBarriers =
                [ ScopedBarrier
                    GlobalScope
                    (if isJust (unitParseError unit) then SyntaxErrorBarrier else UnknownStatementBarrier)
                    Nothing
                ]
            }
        factSemantics
        (unitFacts unit)

-- | Build a dependency graph from statement units and initial session state.
analyzeStatements :: SessionInit -> [StatementUnit] -> DependencyGraph
analyzeStatements initialSession units =
    buildGraph
        initialSession
        [ (stmtId (unitStatement unit), extractFacts unit)
        | unit <- units
        ]

-- | Construct a dependency graph from extracted statement facts.
buildGraph :: SessionInit -> [(StatementId, SemanticFacts)] -> DependencyGraph
buildGraph initialSession stmtFacts =
    let (finalState, edges, barrierEdgeCount, barrierStmtCount, barrierActionCount) =
            foldl' step (initialScanState initialSession, Map.empty, 0, 0, 0) stmtFacts
        fastEdges =
            IntMap.fromList
                [ (unStatementId sid, IntSet.fromList (map unStatementId (Set.toList deps)))
                | (sid, deps) <- Map.toList edges
                ]
     in DependencyGraph
            { graphStmts = Set.fromList (map fst stmtFacts)
            , graphFacts = Map.fromList stmtFacts
            , graphEdges = edges
            , graphFastEdges = fastEdges
            , graphHasGlobalBarrier = Map.member GlobalBarrierKey (scanLastBarriers finalState)
            , graphBarrierActions = barrierActionCount
            , graphBarrierStatements = barrierStmtCount
            , graphBarrierEdges = barrierEdgeCount
            , graphEdgeCount = sum (map Set.size (Map.elems edges))
            }

step ::
    (ScanState, Map StatementId (Set StatementId), Int, Int, Int) ->
    (StatementId, SemanticFacts) ->
    (ScanState, Map StatementId (Set StatementId), Int, Int, Int)
step (priorState, edges, barrierEdgeCount, barrierStmtCount, barrierActionCount) (sid, facts) =
    let
        (state1, _) = ensureFrame priorState
        (state2, preTxDeps) = applyTxEffects sid (factPreTx facts) state1
        state3 = ensureFrameForOps state2
        eval0 = initialEval sid state3
        eval1 = foldl' (processOp sid) eval0 (factOps facts)
        eval1' = foldl' (flip addBarrier) eval1 (factBarriers facts)
        (eval2, sessionTouched) = applySessionEffects sid (factSession facts) eval1'
        state4 = evalToState eval2 state3
        state5 = settleAutocommit sid state3 state4
        (state6, postTxDeps) = applyTxEffects sid (factPostTx facts) state5
        eval3 =
            (addDeps (Set.union preTxDeps postTxDeps) eval2)
                { evalCatalog = scanCatalog state6
                , evalSession = scanSession state6
                }
        (deps, state7, newBarrierEdges, hasBarrier, barrierActions) =
            finalizeStatement sid eval3 sessionTouched priorState state6
        state8 = finishStatement sid state7
        edges' = Map.insert sid deps edges
     in
        ( state8
        , edges'
        , barrierEdgeCount + newBarrierEdges
        , barrierStmtCount + if hasBarrier then 1 else 0
        , barrierActionCount + barrierActions
        )

-- ----------------------------------------------------------------------------
-- Catalog operation dispatch
-- ----------------------------------------------------------------------------

processOp :: StatementId -> Eval -> CatalogOp -> Eval
processOp sid eval op =
    case op of
        DefineTableOp definition -> processDefineTable sid eval definition
        DefineViewOp definition -> processDefineView sid eval definition
        DefineIndexOp definition -> processDefineIndex sid eval definition
        DefineTriggerOp definition -> processDefineTrigger sid eval definition
        DefineSequenceOp definition -> processDefineSequence sid eval definition
        DefineTypeOp definition -> processDefineType sid eval definition
        DefineSchemaOp definition -> processDefineSchema sid eval definition
        ReadOp readFact -> processRead eval readFact
        WriteDataOp write -> processDataWrite sid eval write
        WriteSequenceOp write -> processSequenceWrite sid eval write
        AlterTableOp alter -> processAlterTable sid eval alter
        AttachPartitionOp link -> processAttachPartition sid eval link
        DetachPartitionOp link -> processDetachPartition sid eval link
        AttachInheritOp link -> processAttachInherit sid eval link
        DetachInheritOp link -> processDetachInherit sid eval link
        DropCatalogOp dropOp -> processDrop sid eval dropOp
        RenameOp renameOp -> processRename sid eval renameOp
        RefreshCatalogOp refresh -> processRefresh sid eval refresh

-- ----------------------------------------------------------------------------
-- Statement barrier finalization
-- ----------------------------------------------------------------------------

finalizeStatement ::
    StatementId ->
    Eval ->
    Bool ->
    ScanState ->
    ScanState ->
    (Set StatementId, ScanState, Int, Bool, Int)
finalizeStatement sid eval sessionTouched priorState resolutionState =
    let declared = evalBarriers eval
        resolutionSession = scanSession resolutionState
        resolutionCatalog = scanCatalog resolutionState
        barrierKeys =
            concatMap
                (barrierKeysForScope resolutionSession resolutionCatalog . barrierScope)
                declared
        touchedKeys = Set.toList (evalTouches eval)
        baseDeps =
            lastBarrierDeps
                priorState
                [GlobalBarrierKey, SessionBarrierKey, TransactionBarrierKey]
        touchDeps = lastBarrierDeps priorState touchedKeys
        hasGlobalFence = Map.member GlobalBarrierKey (scanLastBarriers priorState)
        declaredDeps =
            Set.unions
                [ if key == GlobalBarrierKey
                    then if hasGlobalFence then Set.empty else scanPriorStatements priorState
                    else touchersForBarrier priorState key
                | key <- barrierKeys
                ]
        barrierDeps =
            Set.unions [baseDeps, touchDeps, declaredDeps]
        objectDeps = evalDeps eval
        allDeps = Set.delete sid (Set.union objectDeps barrierDeps)
        barrierEdges =
            Set.size (Set.delete sid (Set.difference barrierDeps objectDeps))
        sessionKeys = [SessionBarrierKey | sessionTouched]
        -- Unknown effects serialize subsequent statements.
        inheritedGlobal = [GlobalBarrierKey | hasGlobalFence]
        registrationKeys = barrierKeys ++ sessionKeys ++ inheritedGlobal
        lastBarriers =
            foldl'
                (\current key -> Map.insert key sid current)
                (scanLastBarriers priorState)
                registrationKeys
        !sidSet = Set.singleton sid
        addToucher !acc key = Map.insertWith (\_ old -> Set.insert sid old) key sidSet acc
        touchers1 = Set.foldl' addToucher (scanTouchers priorState) (evalTouches eval)
        touchers = foldl' addToucher touchers1 registrationKeys
        state' =
            resolutionState
                { scanLastBarriers = lastBarriers
                , scanTouchers = touchers
                , scanPriorStatements = Set.insert sid (scanPriorStatements priorState)
                }
        hasBarrier = not (null declared) || sessionTouched
        barrierActions = length declared + if sessionTouched then 1 else 0
     in (allDeps, state', barrierEdges, hasBarrier, barrierActions)

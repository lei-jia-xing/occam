-- | Transaction frames, savepoints, rollback/restore, and on-commit actions.
module Occam.Dependency.Transaction (
    createFrame,
    ensureFrame,
    ensureFrameForOps,
    finishStatement,
    settleAutocommit,
    applyTxEffects,
    applyTxEffect,
    savepoint,
    rollbackTo,
    releaseSavepoint,
    commitFrame,
    rollbackFrame,
    applyOnCommit,
    restoreCatalog,
    restoreRelationMap,
    nonTransactionalRelation,
    stripRelationRestored,
    restoreIndexMap,
    nonTransactionalIndex,
    indexTransactionalIn,
    stripIndexRestored,
    restoreTriggerMap,
    emptyTrigger,
    nonTransactionalTrigger,
    triggerTransactionalIn,
    stripTriggerRestored,
    restoreSequenceMap,
    stripSequenceRestored,
    restoreConstraintMap,
    nonTransactionalConstraint,
    constraintTransactionalIn,
    stripConstraintRestored,
    restoreDefinitionMap,
    stripDefinitionRestored,
) where

import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (
    BeginPolicy (..),
    DropMode (..),
    Ident,
    Name (..),
    OnCommitAction (..),
    SessionValue (..),
    StatementId,
    TriggerBody (..),
    TriggerTiming (..),
    TxEffect (..),
 )
import Occam.Dependency.Drop (removeRelationCascade)
import Occam.Dependency.State (
    Catalog (..),
    ConstraintKey,
    ConstraintState (..),
    DefinitionState (..),
    IndexKey,
    IndexState (..),
    RelationKey (..),
    RelationState (..),
    Savepoint (..),
    ScanState (..),
    SequenceKey,
    SequenceState (..),
    SessionState (..),
    TriggerKey,
    TriggerState (..),
    TxFrame (..),
    clearTransactionSession,
    lookupRelation,
    newRelation,
    rebuildChildRelations,
    rebuildForeignKeys,
    rebuildRelationsByLast,
    transactionActive,
    transactionOpener,
 )

createFrame :: ScanState -> Bool -> Maybe StatementId -> (TxFrame, ScanState)
createFrame state explicit begin =
    let identifier = scanNextFrameId state
        frame =
            TxFrame
                { frameId = identifier
                , frameExplicit = explicit
                , frameNesting = 1
                , frameBegin = begin
                , frameCatalog = scanCatalog state
                , frameSession = scanSession state
                , frameSavepoints = []
                }
     in (frame, state {scanNextFrameId = identifier + 1})

ensureFrame :: ScanState -> (ScanState, TxFrame)
ensureFrame state =
    case scanTransactions state of
        frame : _ -> (state, frame)
        [] ->
            let (frame, state') =
                    createFrame state (not (sessAutocommit (scanSession state))) Nothing
             in (state' {scanTransactions = [frame]}, frame)

ensureFrameForOps :: ScanState -> ScanState
ensureFrameForOps = fst . ensureFrame

finishStatement :: StatementId -> ScanState -> ScanState
finishStatement sid state =
    case scanTransactions state of
        frame : _
            | not (frameExplicit frame)
            , null (frameSavepoints frame)
            , sessAutocommit (scanSession state) ->
                finishStatement sid (commitFrame sid state)
        []
            | not (sessAutocommit (scanSession state)) ->
                let (frame, state') = createFrame state False Nothing
                 in state' {scanTransactions = [frame]}
        _ -> state

settleAutocommit :: StatementId -> ScanState -> ScanState -> ScanState
settleAutocommit sid prior next
    | sessAutocommit (scanSession prior) == sessAutocommit (scanSession next) = next
    | sessAutocommit (scanSession next) =
        case scanTransactions next of
            frame : _ | not (frameExplicit frame) -> commitFrame sid next
            _ -> next
    | otherwise =
        case scanTransactions next of
            [] ->
                let (frame, state') = createFrame next False Nothing
                 in state' {scanTransactions = [frame]}
            _ -> next

{- | Apply transaction controls and retain the statements that make each
control valid when replayed as part of a backward slice.
-}
applyTxEffects :: StatementId -> [TxEffect] -> ScanState -> (ScanState, Set StatementId)
applyTxEffects sid effects state = foldl' step (state, Set.empty) effects
    where
        step (current, deps) effect =
            let next = applyTxEffect sid current effect
                tracked
                    | transactionActive current && not (transactionActive next) =
                        next {scanLastTransactionExit = Just sid}
                    | otherwise = next
             in (tracked, maybe deps (`Set.insert` deps) (prerequisite current effect))
        prerequisite current effect = do
            frame <- listToMaybe (scanTransactions current)
            let saved name = savepointStatement <$> find ((== name) . savepointName) (frameSavepoints frame)
                opener = transactionOpener current
            case effect of
                TxCommit _ -> opener
                TxRollbackAll -> opener
                TxRollbackTo name -> saved name
                TxReleaseSavepoint name -> saved name
                TxSavepoint _ -> opener
                _ -> Nothing

applyTxEffect :: StatementId -> ScanState -> TxEffect -> ScanState
applyTxEffect sid state (TxBegin policy) = applyTxBegin sid policy state
applyTxEffect sid state (TxCommit chain) = applyTxCommit sid chain state
applyTxEffect sid state TxRollbackAll = rollbackFrame sid state
applyTxEffect sid state (TxRollbackTo name) = rollbackTo sid name state
applyTxEffect sid state (TxSavepoint name) = savepoint sid name state
applyTxEffect sid state (TxReleaseSavepoint name) = releaseSavepoint sid name state
applyTxEffect _ state (TxSetReadOnly value) =
    state {scanSession = session {sessTxVariables = txVars'}}
    where
        session = scanSession state
        txVars' = Map.insert "transaction_read_only" (SessionBool value) (sessTxVariables session)

applyTxBegin :: StatementId -> BeginPolicy -> ScanState -> ScanState
applyTxBegin sid policy state = case scanTransactions state of
    [] ->
        let (frame, state') = createFrame state True (Just sid)
         in state' {scanTransactions = [frame]}
    frame : rest
        | policy == BeginCommitActive ->
            -- Commit pending transaction before starting a fresh one under BeginCommitActive.
            applyTxBegin sid BeginIgnore (commitFrame sid state)
        | not (frameExplicit frame) ->
            let nesting = if policy == BeginIgnore then frameNesting frame + 1 else frameNesting frame
                frame' = frame {frameExplicit = True, frameBegin = Just sid, frameNesting = nesting}
             in state {scanTransactions = frame' : rest}
        | policy == BeginIgnore ->
            state {scanTransactions = frame {frameNesting = frameNesting frame + 1} : rest}
        | otherwise -> state

applyTxCommit :: StatementId -> Bool -> ScanState -> ScanState
applyTxCommit sid chain state = case scanTransactions state of
    frame : rest
        | frameNesting frame > 1 ->
            state {scanTransactions = frame {frameNesting = frameNesting frame - 1} : rest}
    _ ->
        let state' = commitFrame sid state
         in if chain
                then
                    let (frame, state'') = createFrame state' True (Just sid)
                     in state'' {scanTransactions = frame : scanTransactions state'}
                else state'

savepoint :: StatementId -> Ident -> ScanState -> ScanState
savepoint sid name state =
    let (state1, frame) = ensureFrame state
        saved =
            Savepoint
                { savepointName = name
                , savepointCatalog = scanCatalog state1
                , savepointSession = scanSession state1
                , savepointStatement = sid
                }
        frame1 = frame {frameSavepoints = saved : frameSavepoints frame}
     in state1 {scanTransactions = frame1 : drop 1 (scanTransactions state1)}

rollbackTo :: StatementId -> Ident -> ScanState -> ScanState
rollbackTo sid name state =
    case scanTransactions state of
        [] -> state
        frame : rest ->
            case break ((== name) . savepointName) (frameSavepoints frame) of
                (_, []) -> state
                (_, saved : older) ->
                    let catalog1 =
                            restoreCatalog sid (savepointCatalog saved) (scanCatalog state)
                        session1 = savepointSession saved
                        frame1 = frame {frameSavepoints = saved : older}
                     in state
                            { scanCatalog = catalog1
                            , scanSession = session1
                            , scanTransactions = frame1 : rest
                            }

releaseSavepoint :: StatementId -> Ident -> ScanState -> ScanState
releaseSavepoint sid name state =
    case scanTransactions state of
        [] -> state
        frame : rest ->
            case break ((== name) . savepointName) (frameSavepoints frame) of
                (_, []) -> state
                (_, _released : older) ->
                    let frame1 = frame {frameSavepoints = older}
                        state1 = state {scanTransactions = frame1 : rest}
                     in if not (frameExplicit frame) && null older
                            then commitFrame sid state1
                            else state1

commitFrame :: StatementId -> ScanState -> ScanState
commitFrame _sid state =
    case scanTransactions state of
        [] -> state
        frame : rest ->
            let catalog1 = applyOnCommit (frameId frame) (scanSession state) (scanCatalog state)
                session1 = clearTransactionSession (scanSession state)
             in state
                    { scanCatalog = catalog1
                    , scanSession = session1
                    , scanTransactions = rest
                    }

rollbackFrame :: StatementId -> ScanState -> ScanState
rollbackFrame sid state =
    case scanTransactions state of
        [] -> state
        frame : rest ->
            let catalog1 = restoreCatalog sid (frameCatalog frame) (scanCatalog state)
                session1 = frameSession frame
             in state
                    { scanCatalog = catalog1
                    , scanSession = session1
                    , scanTransactions = rest
                    }

applyOnCommit :: Int -> SessionState -> Catalog -> Catalog
applyOnCommit transactionId session catalog
    | not (catalogMayHaveOnCommit catalog) = catalog
    | otherwise =
        foldl'
            (flip (removeRelationCascade session DropCascade))
            (catalog {catalogRelations = relations})
            dropKeys
    where
        relations = Map.map clearRowsOnCommit (catalogRelations catalog)
        dropKeys =
            [ key
            | (key, relation) <- Map.toList relations
            , relOnCommit relation == Just OnCommitDrop
            , relOnCommitFrameId relation == Just transactionId
            ]
        clearRowsOnCommit relation
            | relOnCommit relation == Just OnCommitDeleteRows =
                relation
                    { relDataWrites = Set.empty
                    , relCardinalityWrites = Set.empty
                    , relColumnWrites = Map.empty
                    }
            | otherwise = relation

-- ----------------------------------------------------------------------------
-- Transactional rollback
-- ----------------------------------------------------------------------------

restoreCatalog :: StatementId -> Catalog -> Catalog -> Catalog
restoreCatalog sid snapshot current =
    let rels = restoreRelationMap sid (catalogRelations snapshot) (catalogRelations current)
        constraints = restoreConstraintMap sid snapshot current
        fks
            | Map.null (catalogForeignKeys snapshot) && Map.null (catalogForeignKeys current) = Map.empty
            | otherwise = rebuildForeignKeys constraints
        children
            | Set.null (catalogChildRelations snapshot) && Set.null (catalogChildRelations current) = Set.empty
            | otherwise = rebuildChildRelations rels
        relsByLast
            | Map.null rels = Map.empty
            | otherwise = rebuildRelationsByLast rels
     in Catalog
            { catalogRelations = rels
            , catalogMayHaveOnCommit = catalogMayHaveOnCommit snapshot || catalogMayHaveOnCommit current
            , catalogRelationsByLast = relsByLast
            , catalogIndexes =
                restoreIndexMap sid snapshot current
            , catalogTriggers =
                restoreTriggerMap sid snapshot current
            , catalogSequences =
                restoreSequenceMap sid (catalogSequences snapshot) (catalogSequences current)
            , catalogConstraints = constraints
            , catalogForeignKeys = fks
            , catalogChildRelations = children
            , catalogTypes =
                restoreDefinitionMap sid (catalogTypes snapshot) (catalogTypes current)
            , catalogSchemas =
                restoreDefinitionMap sid (catalogSchemas snapshot) (catalogSchemas current)
            , catalogNextUnnamedConstraint =
                max
                    (catalogNextUnnamedConstraint snapshot)
                    (catalogNextUnnamedConstraint current)
            }

-- | Policy for rolling back a catalog map to a previous snapshot.
data RestorePolicy state = RestorePolicy
    { policyMark :: StatementId -> state -> state
    , policyStrip :: state -> state
    , policyKeepsBoth :: state -> state -> Bool
    , policyKeepsSnapshot :: state -> Bool
    , policyKeepsCurrent :: state -> Bool
    }

-- | Roll back a catalog map to a snapshot state according to the given policy.
restoreMap ::
    (Ord key, Eq state) =>
    RestorePolicy state ->
    StatementId ->
    Map key state ->
    Map key state ->
    Map key state
restoreMap policy sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet snapshot) (Map.keysSet current))
        , entry <- restoreKey key
        ]
    where
        restoreKey key =
            case (Map.lookup key snapshot, Map.lookup key current) of
                (Just old, Just new)
                    | policyKeepsBoth policy old new -> [(key, new)]
                    | otherwise -> [(key, markRestored old (Just new))]
                (Just old, Nothing)
                    | policyKeepsSnapshot policy old -> []
                    | otherwise -> [(key, markRestored old Nothing)]
                (Nothing, Just new)
                    | policyKeepsCurrent policy new -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        markRestored old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            policyStrip policy new /= policyStrip policy old
             in if changed
                    then policyMark policy sid old
                    else old

relationRestorePolicy :: RestorePolicy RelationState
relationRestorePolicy =
    RestorePolicy
        { policyMark = \sid state -> state {relRestoredBy = Just sid}
        , policyStrip = stripRelationRestored
        , policyKeepsBoth = nonTransactionalRelation
        , policyKeepsSnapshot = \old -> nonTransactionalRelation old (newRelation (relKind old))
        , policyKeepsCurrent = \new -> nonTransactionalRelation (newRelation (relKind new)) new
        }

restoreRelationMap ::
    StatementId ->
    Map RelationKey RelationState ->
    Map RelationKey RelationState ->
    Map RelationKey RelationState
restoreRelationMap = restoreMap relationRestorePolicy

nonTransactionalRelation :: RelationState -> RelationState -> Bool
nonTransactionalRelation left right =
    not (relTransactional left) || not (relTransactional right)

stripRelationRestored :: RelationState -> RelationState
stripRelationRestored relation = relation {relRestoredBy = Nothing}

indexRestorePolicy :: Catalog -> Catalog -> RestorePolicy IndexState
indexRestorePolicy snapshot current =
    RestorePolicy
        { policyMark = \sid state -> state {indexRestoredBy = Just sid}
        , policyStrip = stripIndexRestored
        , policyKeepsBoth = nonTransactionalIndex snapshot current
        , policyKeepsSnapshot = not . indexTransactionalIn snapshot
        , policyKeepsCurrent = not . indexTransactionalIn current
        }

restoreIndexMap ::
    StatementId ->
    Catalog ->
    Catalog ->
    Map IndexKey IndexState
restoreIndexMap sid snapshot current =
    restoreMap
        (indexRestorePolicy snapshot current)
        sid
        (catalogIndexes snapshot)
        (catalogIndexes current)

nonTransactionalIndex :: Catalog -> Catalog -> IndexState -> IndexState -> Bool
nonTransactionalIndex snapshot current left right =
    not (indexTransactionalIn snapshot left)
        || not (indexTransactionalIn current right)

indexTransactionalIn :: Catalog -> IndexState -> Bool
indexTransactionalIn catalog state =
    case indexStateTable state of
        Nothing -> True
        Just key -> maybe True relTransactional (lookupRelation catalog key)

stripIndexRestored :: IndexState -> IndexState
stripIndexRestored state = state {indexRestoredBy = Nothing}

triggerRestorePolicy :: Catalog -> Catalog -> RestorePolicy TriggerState
triggerRestorePolicy snapshot current =
    RestorePolicy
        { policyMark = \sid state -> state {triggerRestoredBy = Just sid}
        , policyStrip = stripTriggerRestored
        , policyKeepsBoth = nonTransactionalTrigger snapshot current
        , policyKeepsSnapshot = not . triggerTransactionalIn snapshot
        , policyKeepsCurrent = not . triggerTransactionalIn current
        }

restoreTriggerMap ::
    StatementId ->
    Catalog ->
    Catalog ->
    Map TriggerKey TriggerState
restoreTriggerMap sid snapshot current =
    restoreMap
        (triggerRestorePolicy snapshot current)
        sid
        (catalogTriggers snapshot)
        (catalogTriggers current)

emptyTrigger :: TriggerState
emptyTrigger =
    TriggerState Nothing Nothing TriggerBefore [] [] TriggerOpaque Nothing

nonTransactionalTrigger :: Catalog -> Catalog -> TriggerState -> TriggerState -> Bool
nonTransactionalTrigger snapshot current left right =
    not (triggerTransactionalIn snapshot left)
        || not (triggerTransactionalIn current right)

triggerTransactionalIn :: Catalog -> TriggerState -> Bool
triggerTransactionalIn catalog state =
    case triggerStateTable state of
        Nothing -> True
        Just key -> maybe True relTransactional (lookupRelation catalog key)

stripTriggerRestored :: TriggerState -> TriggerState
stripTriggerRestored state = state {triggerRestoredBy = Nothing}

sequenceRestorePolicy :: RestorePolicy SequenceState
sequenceRestorePolicy =
    RestorePolicy
        { policyMark = \sid state -> state {sequenceRestoredBy = Just sid}
        , policyStrip = stripSequenceRestored
        , policyKeepsBoth = \old new ->
            not (sequenceStateTransactional old)
                || not (sequenceStateTransactional new)
        , policyKeepsSnapshot = not . sequenceStateTransactional
        , policyKeepsCurrent = not . sequenceStateTransactional
        }

restoreSequenceMap ::
    StatementId ->
    Map SequenceKey SequenceState ->
    Map SequenceKey SequenceState ->
    Map SequenceKey SequenceState
restoreSequenceMap = restoreMap sequenceRestorePolicy

stripSequenceRestored :: SequenceState -> SequenceState
stripSequenceRestored state = state {sequenceRestoredBy = Nothing}

constraintRestorePolicy :: Catalog -> Catalog -> RestorePolicy ConstraintState
constraintRestorePolicy snapshot current =
    RestorePolicy
        { policyMark = \sid state -> state {constraintRestoredBy = Just sid}
        , policyStrip = stripConstraintRestored
        , policyKeepsBoth = nonTransactionalConstraint snapshot current
        , policyKeepsSnapshot = not . constraintTransactionalIn snapshot
        , policyKeepsCurrent = not . constraintTransactionalIn current
        }

restoreConstraintMap ::
    StatementId ->
    Catalog ->
    Catalog ->
    Map ConstraintKey ConstraintState
restoreConstraintMap sid snapshot current =
    restoreMap
        (constraintRestorePolicy snapshot current)
        sid
        (catalogConstraints snapshot)
        (catalogConstraints current)

nonTransactionalConstraint :: Catalog -> Catalog -> ConstraintState -> ConstraintState -> Bool
nonTransactionalConstraint snapshot current left right =
    not (constraintTransactionalIn snapshot left)
        || not (constraintTransactionalIn current right)

constraintTransactionalIn :: Catalog -> ConstraintState -> Bool
constraintTransactionalIn catalog state =
    maybe True relTransactional (lookupRelation catalog (constraintStateTable state))

stripConstraintRestored :: ConstraintState -> ConstraintState
stripConstraintRestored state = state {constraintRestoredBy = Nothing}

definitionRestorePolicy :: RestorePolicy DefinitionState
definitionRestorePolicy =
    RestorePolicy
        { policyMark = \sid state -> state {definitionRestoredBy = Just sid}
        , policyStrip = stripDefinitionRestored
        , policyKeepsBoth = \_ _ -> False
        , policyKeepsSnapshot = const False
        , policyKeepsCurrent = const False
        }

restoreDefinitionMap ::
    StatementId ->
    Map Name DefinitionState ->
    Map Name DefinitionState ->
    Map Name DefinitionState
restoreDefinitionMap = restoreMap definitionRestorePolicy

stripDefinitionRestored :: DefinitionState -> DefinitionState
stripDefinitionRestored state = state {definitionRestoredBy = Nothing}

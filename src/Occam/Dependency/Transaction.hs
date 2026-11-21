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

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

applyTxEffects :: StatementId -> [TxEffect] -> ScanState -> ScanState
applyTxEffects sid effects state = foldl' (applyTxEffect sid) state effects

applyTxEffect :: StatementId -> ScanState -> TxEffect -> ScanState
applyTxEffect sid state effect =
    case effect of
        TxBegin policy ->
            case scanTransactions state of
                [] ->
                    let (frame, state') = createFrame state True (Just sid)
                     in state' {scanTransactions = [frame]}
                frame : rest
                    | policy == BeginCommitActive ->
                        -- A `START TRANSACTION`-style begin commits the pending
                        -- transaction (including an implicit autocommit-off
                        -- frame) and starts a fresh one.
                        applyTxEffect sid (commitFrame sid state) (TxBegin BeginIgnore)
                    | not (frameExplicit frame) ->
                        state
                            { scanTransactions =
                                frame
                                    { frameExplicit = True
                                    , frameBegin = Just sid
                                    , frameNesting =
                                        if policy == BeginIgnore
                                            then frameNesting frame + 1
                                            else frameNesting frame
                                    }
                                    : rest
                            }
                    | policy == BeginIgnore ->
                        state
                            { scanTransactions =
                                frame {frameNesting = frameNesting frame + 1} : rest
                            }
                    | otherwise -> state
        TxCommit chain ->
            case scanTransactions state of
                frame : rest
                    | frameNesting frame > 1 ->
                        state
                            { scanTransactions =
                                frame {frameNesting = frameNesting frame - 1} : rest
                            }
                _ ->
                    let state' = commitFrame sid state
                     in if chain
                            then
                                let (frame, state'') = createFrame state' True (Just sid)
                                 in state'' {scanTransactions = frame : scanTransactions state'}
                            else state'
        TxRollbackAll -> rollbackFrame sid state
        TxRollbackTo name -> rollbackTo sid name state
        TxSavepoint name -> savepoint sid name state
        TxReleaseSavepoint name -> releaseSavepoint sid name state
        TxSetReadOnly value ->
            state
                { scanSession =
                    (scanSession state)
                        { sessTxVariables =
                            Map.insert
                                "transaction_read_only"
                                (SessionBool value)
                                (sessTxVariables (scanSession state))
                        }
                }

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
commitFrame sid state =
    case scanTransactions state of
        [] -> state
        frame : rest ->
            let catalog1 = applyOnCommit sid (frameId frame) (scanSession state) (scanCatalog state)
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

applyOnCommit :: StatementId -> Int -> SessionState -> Catalog -> Catalog
applyOnCommit sid transactionId session catalog =
    let relations =
            Map.map
                ( \relation ->
                    case relOnCommit relation of
                        Just OnCommitDeleteRows ->
                            relation
                                { relDataWrites = Set.empty
                                , relCardinalityWrites = Set.empty
                                , relColumnWrites = Map.empty
                                }
                        _ -> relation
                )
                (catalogRelations catalog)
        dropKeys =
            [ key
            | (key, relation) <- Map.toList relations
            , relOnCommit relation == Just OnCommitDrop
            , relOnCommitFrameId relation == Just transactionId
            ]
        catalog1 = catalog {catalogRelations = relations}
     in foldl'
            (flip (removeRelationCascade session sid DropCascade))
            catalog1
            dropKeys

-- ----------------------------------------------------------------------------
-- Transactional rollback
-- ----------------------------------------------------------------------------

restoreCatalog :: StatementId -> Catalog -> Catalog -> Catalog
restoreCatalog sid snapshot current =
    Catalog
        { catalogRelations =
            restoreRelationMap sid (catalogRelations snapshot) (catalogRelations current)
        , catalogIndexes =
            restoreIndexMap sid snapshot current
        , catalogTriggers =
            restoreTriggerMap sid snapshot current
        , catalogSequences =
            restoreSequenceMap sid (catalogSequences snapshot) (catalogSequences current)
        , catalogConstraints =
            restoreConstraintMap sid snapshot current
        , catalogTypes =
            restoreDefinitionMap sid (catalogTypes snapshot) (catalogTypes current)
        , catalogSchemas =
            restoreDefinitionMap sid (catalogSchemas snapshot) (catalogSchemas current)
        , catalogNextUnnamedConstraint =
            max
                (catalogNextUnnamedConstraint snapshot)
                (catalogNextUnnamedConstraint current)
        }

restoreRelationMap ::
    StatementId ->
    Map RelationKey RelationState ->
    Map RelationKey RelationState ->
    Map RelationKey RelationState
restoreRelationMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet snapshot) (Map.keysSet current))
        , entry <- restoreRelationKey key
        ]
    where
        restoreRelationKey key =
            case (Map.lookup key snapshot, Map.lookup key current) of
                (Just old, Just new)
                    | nonTransactionalRelation old new -> [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | nonTransactionalRelation old (newRelation (relKind old)) -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | nonTransactionalRelation (newRelation (relKind new)) new -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripRelationRestored new /= stripRelationRestored old
             in if changed
                    then old {relRestoredBy = Just sid}
                    else old

nonTransactionalRelation :: RelationState -> RelationState -> Bool
nonTransactionalRelation left right =
    not (relTransactional left) || not (relTransactional right)

stripRelationRestored :: RelationState -> RelationState
stripRelationRestored relation = relation {relRestoredBy = Nothing}

restoreIndexMap ::
    StatementId ->
    Catalog ->
    Catalog ->
    Map IndexKey IndexState
restoreIndexMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet oldMap) (Map.keysSet newMap))
        , entry <- restoreIndexKey key
        ]
    where
        oldMap = catalogIndexes snapshot
        newMap = catalogIndexes current
        restoreIndexKey key =
            case (Map.lookup key oldMap, Map.lookup key newMap) of
                (Just old, Just new)
                    | nonTransactionalIndex snapshot current old new -> [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | nonTransactionalIndex snapshot current old (IndexState Nothing Nothing [] Nothing) -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | nonTransactionalIndex snapshot current (IndexState Nothing Nothing [] Nothing) new -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripIndexRestored new /= stripIndexRestored old
             in if changed
                    then old {indexRestoredBy = Just sid}
                    else old

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

restoreTriggerMap ::
    StatementId ->
    Catalog ->
    Catalog ->
    Map TriggerKey TriggerState
restoreTriggerMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet oldMap) (Map.keysSet newMap))
        , entry <- restoreTriggerKey key
        ]
    where
        oldMap = catalogTriggers snapshot
        newMap = catalogTriggers current
        restoreTriggerKey key =
            case (Map.lookup key oldMap, Map.lookup key newMap) of
                (Just old, Just new)
                    | nonTransactionalTrigger snapshot current old new -> [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | nonTransactionalTrigger snapshot current old emptyTrigger -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | nonTransactionalTrigger snapshot current emptyTrigger new -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripTriggerRestored new /= stripTriggerRestored old
             in if changed
                    then old {triggerRestoredBy = Just sid}
                    else old

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

restoreSequenceMap ::
    StatementId ->
    Map SequenceKey SequenceState ->
    Map SequenceKey SequenceState ->
    Map SequenceKey SequenceState
restoreSequenceMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet snapshot) (Map.keysSet current))
        , entry <- restoreSequenceKey key
        ]
    where
        restoreSequenceKey key =
            case (Map.lookup key snapshot, Map.lookup key current) of
                (Just old, Just new)
                    | not (sequenceStateTransactional old)
                        || not (sequenceStateTransactional new) ->
                        [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | not (sequenceStateTransactional old) -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | not (sequenceStateTransactional new) -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripSequenceRestored new /= stripSequenceRestored old
             in if changed
                    then old {sequenceRestoredBy = Just sid}
                    else old

stripSequenceRestored :: SequenceState -> SequenceState
stripSequenceRestored state = state {sequenceRestoredBy = Nothing}

restoreConstraintMap ::
    StatementId ->
    Catalog ->
    Catalog ->
    Map ConstraintKey ConstraintState
restoreConstraintMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet oldMap) (Map.keysSet newMap))
        , entry <- restoreConstraintKey key
        ]
    where
        oldMap = catalogConstraints snapshot
        newMap = catalogConstraints current
        restoreConstraintKey key =
            case (Map.lookup key oldMap, Map.lookup key newMap) of
                (Just old, Just new)
                    | nonTransactionalConstraint snapshot current old new -> [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | not (constraintTransactionalIn snapshot old) -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | not (constraintTransactionalIn current new) -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripConstraintRestored new /= stripConstraintRestored old
             in if changed
                    then old {constraintRestoredBy = Just sid}
                    else old

nonTransactionalConstraint :: Catalog -> Catalog -> ConstraintState -> ConstraintState -> Bool
nonTransactionalConstraint snapshot current left right =
    not (constraintTransactionalIn snapshot left)
        || not (constraintTransactionalIn current right)

constraintTransactionalIn :: Catalog -> ConstraintState -> Bool
constraintTransactionalIn catalog state =
    maybe True relTransactional (lookupRelation catalog (constraintStateTable state))

stripConstraintRestored :: ConstraintState -> ConstraintState
stripConstraintRestored state = state {constraintRestoredBy = Nothing}

restoreDefinitionMap ::
    StatementId ->
    Map Name DefinitionState ->
    Map Name DefinitionState ->
    Map Name DefinitionState
restoreDefinitionMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet snapshot) (Map.keysSet current))
        , entry <- restoreDefinitionKey key
        ]
    where
        restoreDefinitionKey key =
            case (Map.lookup key snapshot, Map.lookup key current) of
                (Just old, Just new) -> [(key, mark old (Just new))]
                (Just old, Nothing) -> [(key, mark old Nothing)]
                (Nothing, _) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripDefinitionRestored new /= stripDefinitionRestored old
             in if changed
                    then old {definitionRestoredBy = Just sid}
                    else old

stripDefinitionRestored :: DefinitionState -> DefinitionState
stripDefinitionRestored state = state {definitionRestoredBy = Nothing}

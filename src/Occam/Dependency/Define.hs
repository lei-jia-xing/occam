{-# LANGUAGE OverloadedLabels #-}

-- | Catalog definitions: tables, views, indexes, triggers, sequences, types, schemas.
module Occam.Dependency.Define (
    tableConstraintValues,
    foreignKeyParentDeps,
    resolvedSourceKeys,
    unresolvedSourceRefs,
    processDefineTable,
    processDefineView,
    processDefineIndex,
    triggerBodyDeps,
    dmlEventTarget,
    sequenceWriteDeps,
    processDefineTrigger,
    processDefineSequence,
    processDefineType,
    processDefineSchema,
) where

import Control.Monad.Trans.State.Strict (State, get, modify')
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Occam.Core.Schema (
    ColumnDef (..),
    Constraint (..),
    DmlEvent (..),
    ForeignKey (..),
    IndexDef (..),
    IndexRef (..),
    IndexSchemaPlacement (..),
    Name (..),
    NameRef,
    OnCommitAction (..),
    ReadFact (..),
    ReadMode (..),
    RelationKind (..),
    SchemaDef (..),
    SequenceDef (..),
    SequenceWrite (..),
    TableDef (..),
    TypeDef (..),
    ViewDef (..),
    nameText,
 )
import Occam.Core.Semantics (
    BarrierReason (..),
    BarrierScope (..),
    ScopedBarrier (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
 )
import Occam.Core.Types (StatementId)
import Occam.Dependency.Barrier (
    indexTouchKeys,
    relationTouchKeys,
    schemaTouchKeys,
    sequenceTouchKeys,
    triggerTouchKeys,
    typeTouchKeys,
 )
import Occam.Dependency.Query (
    installConstraints,
    relationAllDeps,
    relationReadDeps,
    schemaPrefixDeps,
 )
import Occam.Dependency.Resolve (
    qualifyDefinitionName,
    qualifyObjectName,
    resolveIndexKeys,
    resolveRelationKeys,
    resolveSequenceKeys,
    resolveTriggerKeys,
    resolveViewSourceKeys,
 )
import Occam.Dependency.State (
    Catalog (..),
    DefinitionState (..),
    Eval (..),
    IndexKey (..),
    IndexState (..),
    RelationKey (..),
    RelationState (..),
    SequenceKey (..),
    SequenceState (..),
    SessionState,
    TriggerKey (..),
    TriggerState (..),
    addBarrier,
    addDeps,
    addTouches,
    ambiguousMutation,
    clearRelationObjects,
    insertRelation,
    lookupRelation,
    modifyRelation,
    newRelation,
 )
import Occam.Dependency.Write (processRead, processReadWith)
import Optics.Core ((%), (%~), (&))

-- Shared definition helpers

-- | Combine table-level constraints with inline column foreign-key constraints.
tableConstraintValues :: TableDef -> [Constraint]
tableConstraintValues definition =
    tableConstraints definition
        ++ [ ForeignKeyConstraint foreignKey
           | column <- tableColumns definition
           , foreignKey <- columnReferences column
           ]

-- | Collect catalog dependencies required to resolve foreign-key parent tables.
foreignKeyParentDeps :: SessionState -> Catalog -> [Constraint] -> Set StatementId
foreignKeyParentDeps session catalog constraints =
    Set.unions
        [ relationReadDeps session catalog parentKey ReadSchema [] False
        | ForeignKeyConstraint foreignKey <- constraints
        , parentKey <- resolveRelationKeys session catalog (foreignKeyParent foreignKey)
        ]

resolvedSourceKeysWith ::
    (SessionState -> Catalog -> NameRef -> [RelationKey]) ->
    SessionState ->
    Catalog ->
    [ReadFact] ->
    Set RelationKey
resolvedSourceKeysWith resolveFn session catalog readFacts =
    Set.fromList
        [ key
        | readFact <- readFacts
        , key <- resolveFn session catalog (readSource readFact)
        , Map.member key (catalogRelations catalog)
        ]

-- | Resolve read-fact sources to currently present relation keys.
resolvedSourceKeys :: SessionState -> Catalog -> [ReadFact] -> Set RelationKey
resolvedSourceKeys = resolvedSourceKeysWith resolveRelationKeys

unresolvedSourceRefsWith ::
    (SessionState -> Catalog -> NameRef -> [RelationKey]) ->
    SessionState ->
    Catalog ->
    [ReadFact] ->
    Set NameRef
unresolvedSourceRefsWith resolveFn session catalog readFacts =
    Set.fromList
        [ readSource readFact
        | readFact <- readFacts
        , null
            [ key
            | key <- resolveFn session catalog (readSource readFact)
            , Map.member key (catalogRelations catalog)
            ]
        ]

-- | Return read-fact names that do not resolve to a present relation.
unresolvedSourceRefs :: SessionState -> Catalog -> [ReadFact] -> Set NameRef
unresolvedSourceRefs = unresolvedSourceRefsWith resolveRelationKeys

-- Tables, views, indexes

-- | Install a table definition, its constraints, source links, and dependencies.
processDefineTable :: StatementId -> TableDef -> State Eval ()
processDefineTable sid definition =
    modify' $ \eval ->
        let session = evalSession eval
            catalog = evalCatalog eval
            key =
                RelationKey
                    (tableTemporary definition)
                    (qualifyDefinitionName session (tableName definition) (tableTemporary definition))
            existing = lookupRelation catalog key
            evalReads = Foldable.foldl' processRead eval (tableSources definition)
            likeDeps =
                maybe
                    Set.empty
                    ( \reference ->
                        Set.unions
                            [ relationReadDeps session catalog source ReadSchema [] False
                            | source <- resolveRelationKeys session catalog reference
                            ]
                    )
                    (tableLike definition)
            existingDeps = maybe Set.empty (const (relationAllDeps session catalog key)) existing
            constraintValues = tableConstraintValues definition
            parentDeps = foreignKeyParentDeps session catalog constraintValues
            eval1 =
                addTouches
                    (relationTouchKeys key)
                    ( addDeps
                        ( likeDeps
                            `Set.union` existingDeps
                            `Set.union` parentDeps
                            `Set.union` schemaPrefixDeps catalog (relationKeyName key)
                        )
                        evalReads
                    )
            unresolved = unresolvedSourceRefs session catalog (tableSources definition)
            eval2 = addUnresolvedSourceBarriers "table" (tableName definition) unresolved eval1
            installTable =
                let catalog1 = insertRelation key (newRelation TableRelation) (clearRelationObjects key catalog)
                    (catalog2, constraintKeys) =
                        installConstraints session sid key constraintValues catalog1
                    onCommitFrame = case tableOnCommit definition of
                        Just OnCommitDrop -> evalFrameId eval
                        _ -> Nothing
                    relation =
                        (newRelation TableRelation)
                            { relDefinition = Just sid
                            , relColumns = Set.fromList (map columnName (tableColumns definition))
                            , relSources = resolvedSourceKeys session catalog (tableSources definition)
                            , relUnresolvedSources = unresolved
                            , relSourceReads = tableSources definition
                            , relPartitionParents = Set.fromList (maybeToList (tablePartitionParent definition))
                            , relInheritParents = Set.fromList (tableInheritParents definition)
                            , relOnCommit = tableOnCommit definition
                            , relOnCommitFrameId = onCommitFrame
                            , relTransactional = tableTransactional definition
                            , relConstraints = constraintKeys
                            }
                 in insertRelation key relation catalog2
         in if tableIfNotExists definition && isJust existing
                then eval2
                else eval2 {evalCatalog = installTable}

-- | Install a view definition and its source/lineage metadata.
processDefineView :: StatementId -> ViewDef -> State Eval ()
processDefineView sid definition =
    modify' $ \eval ->
        let session = evalSession eval
            catalog = evalCatalog eval
            key =
                RelationKey
                    (viewTemporary definition)
                    (qualifyDefinitionName session (viewName definition) (viewTemporary definition))
            resolveSource s = resolveViewSourceKeys s (viewTemporary definition)
            existing = lookupRelation catalog key
            evalReads = Foldable.foldl' (processReadWith resolveSource) eval (viewSources definition)
            existingDeps = maybe Set.empty (const (relationAllDeps session catalog key)) existing
            eval1 =
                addTouches
                    (relationTouchKeys key)
                    (addDeps (existingDeps `Set.union` schemaPrefixDeps catalog (relationKeyName key)) evalReads)
            unresolved = unresolvedSourceRefsWith resolveSource session catalog (viewSources definition)
            eval2 = addUnresolvedSourceBarriers "view" (viewName definition) unresolved eval1
            kind
                | viewMaterialized definition = MaterializedViewRelation
                | otherwise = ViewRelation
            relation =
                (newRelation kind)
                    { relDefinition = Just sid
                    , relColumns = Set.fromList (viewColumns definition)
                    , relSources = resolvedSourceKeysWith resolveSource session catalog (viewSources definition)
                    , relUnresolvedSources = unresolved
                    , relSourceReads = viewSources definition
                    , relColumnLineage = viewColumnLineage definition
                    , relConditionColumns = viewConditionColumns definition
                    }
         in if viewIfNotExists definition && isJust existing
                then eval2
                else eval2 {evalCatalog = insertRelation key relation (clearRelationObjects key catalog)}

-- | Collect view-source barriers after the definition's read dependencies.
addUnresolvedSourceBarriers :: Text -> Name -> Set NameRef -> Eval -> Eval
addUnresolvedSourceBarriers kindLabel objName unresolved eval0 =
    Foldable.foldl'
        (flip addBarrier)
        eval0
        [ ScopedBarrier
            (RelationScope reference)
            UnresolvedViewSourceBarrier
            (Just ("unresolved source of " <> kindLabel <> " " <> nameText objName))
        | reference <- Set.toList unresolved
        ]

-- | Install an index and associate it with its resolved table.
processDefineIndex :: StatementId -> IndexDef -> State Eval ()
processDefineIndex sid definition = do
    eval <- get
    let session = evalSession eval
        catalog = evalCatalog eval
        tableKeys = resolveRelationKeys session catalog (indexTable definition)
        -- Resolve unqualified index in table's schema or current schema.
        indexKeys = case (indexRef definition, indexSchemaPlacement definition, tableKeys) of
            (NamedIndex name@(Name [_]), IndexTableSchema, tableKey : _)
                | length (nameParts (relationKeyName tableKey)) > 1 ->
                    [NamedIndexKey (Name (init (nameParts (relationKeyName tableKey)) ++ nameParts name))]
            _ -> resolveIndexKeys session catalog (indexRef definition)
        tableDeps =
            Set.unions
                [ relationReadDeps session catalog tableKey ReadData (indexColumns definition) False
                | tableKey <- tableKeys
                ]
    case tableKeys of
        _ : _ : _ -> modify' ambiguousMutation
        _ -> case indexKeys of
            [] -> pure ()
            [indexKey] -> installDefinedIndex sid definition catalog tableKeys tableDeps indexKey
            _ -> modify' ambiguousMutation

installDefinedIndex ::
    StatementId -> IndexDef -> Catalog -> [RelationKey] -> Set StatementId -> IndexKey -> State Eval ()
installDefinedIndex sid definition catalog tableKeys tableDeps indexKey =
    modify' $ \current ->
        let existing = Map.lookup indexKey (catalogIndexes (evalCatalog current))
            existingDeps =
                maybe
                    Set.empty
                    (\state -> Set.fromList (catMaybes [indexDefinition state, indexRestoredBy state]))
                    existing
            ownerName = case indexKey of
                NamedIndexKey name -> name
                TableIndexKey table _ -> relationKeyName table
            current1 =
                addTouches
                    (indexTouchKeys indexKey (listToMaybe tableKeys))
                    (addDeps (tableDeps `Set.union` existingDeps `Set.union` schemaPrefixDeps catalog ownerName) current)
            catalog0 =
                maybe
                    (evalCatalog current1)
                    ( \oldKey ->
                        modifyRelation
                            oldKey
                            (\relation -> relation {relIndexes = Set.delete indexKey (relIndexes relation)})
                            (evalCatalog current1)
                    )
                    (existing >>= indexStateTable)
            newState = IndexState (Just sid) (listToMaybe tableKeys) (indexColumns definition) Nothing
            catalog1 = catalog0 {catalogIndexes = Map.insert indexKey newState (catalogIndexes catalog0)}
            catalog2 =
                Foldable.foldl'
                    ( \cat' tableKey ->
                        modifyRelation
                            tableKey
                            (\relation -> relation {relIndexes = Set.insert indexKey (relIndexes relation)})
                            cat'
                    )
                    catalog1
                    tableKeys
         in if indexIfNotExists definition && isJust existing
                then current1
                else current1 {evalCatalog = catalog2}

-- Trigger body dependencies

-- | Collect dependencies referenced by inline trigger effects.
triggerBodyDeps :: SessionState -> Catalog -> TriggerBody -> Set StatementId
triggerBodyDeps session catalog = \case
    TriggerInline effects -> Set.unions (map effectDeps effects)
    TriggerFunction _ _ -> Set.empty
    TriggerOpaque -> Set.empty
    where
        effectDeps = \case
            TriggerDml event ->
                Set.unions
                    [ relationAllDeps session catalog key
                    | key <- resolveRelationKeys session catalog (dmlEventTarget event)
                    ]
            TriggerRead readFact ->
                Set.unions
                    [ relationReadDeps session catalog key (readMode readFact) (readColumns readFact) (readOnly readFact)
                    | key <- resolveRelationKeys session catalog (readSource readFact)
                    ]
            TriggerWriteSequence write ->
                sequenceWriteDeps session catalog (sequenceWriteTarget write)
            TriggerSession _ -> Set.empty
            TriggerBarrier _ -> Set.empty

-- | Return the target relation for any supported DML event.
dmlEventTarget :: DmlEvent -> NameRef
dmlEventTarget = \case
    DmlInsert target _ -> target
    DmlUpdate target _ -> target
    DmlMerge target _ -> target
    DmlDelete target _ -> target
    DmlTruncate target _ -> target

-- | Collect sequence-definition and prior-write dependencies for a sequence target.
sequenceWriteDeps :: SessionState -> Catalog -> NameRef -> Set StatementId
sequenceWriteDeps session catalog target =
    Set.unions
        [ Set.fromList (catMaybes [sequenceDefinition state, sequenceRestoredBy state])
            `Set.union` sequenceWrites state
        | key <- resolveSequenceKeys session catalog target
        , Just state <- [Map.lookup key (catalogSequences catalog)]
        ]

-- Triggers, sequences, types, schemas

-- | Install a trigger and record dependencies from its target, conditions, and body.
processDefineTrigger :: StatementId -> TriggerDef -> State Eval ()
processDefineTrigger sid definition = do
    initial <- get
    let session = evalSession initial
        catalog = evalCatalog initial
        triggerKeys = resolveTriggerKeys session catalog (triggerRef definition)
        tableKeys = resolveRelationKeys session catalog (triggerTable definition)
        tableDeps = Set.unions [relationAllDeps session catalog tableKey | tableKey <- tableKeys]
        referencedDeps =
            maybe
                Set.empty
                ( \reference ->
                    Set.unions
                        [ relationAllDeps session catalog tableKey
                        | tableKey <- resolveRelationKeys session catalog reference
                        ]
                )
                (triggerReferencedTable definition)
        bodyDeps = triggerBodyDeps session catalog (triggerBody definition)
        whenDeps =
            Set.unions
                [ relationReadDeps session catalog key (readMode readFact) (readColumns readFact) (readOnly readFact)
                | readFact <- triggerWhenReads definition
                , key <- resolveRelationKeys session catalog (readSource readFact)
                ]
    case tableKeys of
        _ : _ : _ -> modify' ambiguousMutation
        _ -> do
            modify' $ \eval ->
                let eval0 = case triggerBody definition of
                        TriggerFunction name _ ->
                            -- Opaque function trigger bodies require a barrier.
                            addBarrier
                                ( ScopedBarrier
                                    GlobalScope
                                    UnsupportedTriggerBodyBarrier
                                    (Just ("trigger function " <> nameText name))
                                )
                                eval
                        _ -> eval
                 in addTouches
                        (concatMap (`triggerTouchKeys` listToMaybe tableKeys) triggerKeys)
                        (addDeps (tableDeps `Set.union` referencedDeps `Set.union` bodyDeps `Set.union` whenDeps) eval0)
            case triggerKeys of
                [] -> pure ()
                [triggerKey] -> installDefinedTrigger sid definition catalog tableKeys triggerKey
                _ -> modify' ambiguousMutation

installDefinedTrigger ::
    StatementId -> TriggerDef -> Catalog -> [RelationKey] -> TriggerKey -> State Eval ()
installDefinedTrigger sid definition catalog tableKeys triggerKey =
    modify' $ \current ->
        let existing = Map.lookup triggerKey (catalogTriggers (evalCatalog current))
            existingDeps =
                maybe
                    Set.empty
                    (\state -> Set.fromList (catMaybes [triggerDefinition state, triggerRestoredBy state]))
                    existing
            ownerName = case triggerKey of
                NamedTriggerKey name -> name
                TableTriggerKey table _ -> relationKeyName table
            current1 = addDeps (existingDeps `Set.union` schemaPrefixDeps catalog ownerName) current
            catalog0 =
                maybe
                    (evalCatalog current1)
                    ( \oldKey ->
                        modifyRelation
                            oldKey
                            (\relation -> relation {relTriggers = Set.delete triggerKey (relTriggers relation)})
                            (evalCatalog current1)
                    )
                    (existing >>= triggerStateTable)
            newState =
                TriggerState
                    (Just sid)
                    (listToMaybe tableKeys)
                    (triggerTiming definition)
                    (triggerEvents definition)
                    (triggerWhenReads definition)
                    (triggerBody definition)
                    Nothing
            catalog1 = catalog0 {catalogTriggers = Map.insert triggerKey newState (catalogTriggers catalog0)}
            catalog2 =
                Foldable.foldl'
                    ( \cat' tableKey ->
                        modifyRelation
                            tableKey
                            (\relation -> relation {relTriggers = Set.insert triggerKey (relTriggers relation)})
                            cat'
                    )
                    catalog1
                    tableKeys
         in if triggerIfNotExists definition && isJust existing
                then current1
                else current1 {evalCatalog = catalog2}

-- | Install sequence state and dependencies for a sequence definition.
processDefineSequence :: StatementId -> SequenceDef -> State Eval ()
processDefineSequence sid definition =
    modify' $ \eval ->
        let session = evalSession eval
            catalog = evalCatalog eval
            key =
                SequenceKey
                    (sequenceTemporary definition)
                    (qualifyDefinitionName session (sequenceName definition) (sequenceTemporary definition))
            existing = Map.lookup key (catalogSequences catalog)
            existingDeps =
                maybe
                    Set.empty
                    ( Set.fromList
                        . catMaybes
                        . (\sequenceState -> [sequenceDefinition sequenceState, sequenceRestoredBy sequenceState])
                    )
                    existing
            state =
                SequenceState
                    (Just sid)
                    Set.empty
                    Set.empty
                    Nothing
                    (sequenceTransactional definition)
         in eval
                & addTouches (sequenceTouchKeys key)
                & addDeps (existingDeps `Set.union` schemaPrefixDeps catalog (sequenceKeyName key))
                & (#evalCatalog % #catalogSequences %~ Map.insert key state)

-- | Install a type definition and depend on any prior definition it replaces.
processDefineType :: StatementId -> TypeDef -> State Eval ()
processDefineType sid definition =
    modify' $ \eval ->
        let catalog = evalCatalog eval
            name = qualifyObjectName (evalSession eval) (typeName definition)
            existing = Map.lookup name (catalogTypes catalog)
            existingDeps =
                maybe
                    Set.empty
                    (Set.fromList . catMaybes . (\state -> [definitionStatement state, definitionRestoredBy state]))
                    existing
         in eval
                & addTouches (typeTouchKeys name)
                & addDeps (existingDeps `Set.union` schemaPrefixDeps catalog name)
                & (#evalCatalog % #catalogTypes %~ Map.insert name (DefinitionState (Just sid) Nothing))

-- | Install a schema definition and depend on its previous state.
processDefineSchema :: StatementId -> SchemaDef -> State Eval ()
processDefineSchema sid definition =
    modify' $ \eval ->
        let catalog = evalCatalog eval
            name = schemaName definition
            existing = Map.lookup name (catalogSchemas catalog)
            existingDeps =
                maybe
                    Set.empty
                    (Set.fromList . catMaybes . (\state -> [definitionStatement state, definitionRestoredBy state]))
                    existing
            catalog1 =
                if isJust existing
                    then catalog
                    else
                        catalog
                            { catalogSchemas = Map.insert name (DefinitionState (Just sid) Nothing) (catalogSchemas catalog)
                            }
         in ( eval
                & addTouches (schemaTouchKeys name)
                & addDeps (existingDeps `Set.union` schemaPrefixDeps catalog name)
            )
                { evalCatalog = catalog1
                }

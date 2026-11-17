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

import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (
    BarrierReason (..),
    BarrierScope (..),
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
    ScopedBarrier (..),
    SequenceDef (..),
    SequenceWrite (..),
    StatementId,
    TableDef (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    TypeDef (..),
    ViewDef (..),
    nameText,
 )
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
    mutateResolved,
    newRelation,
 )
import Occam.Dependency.Write (processRead)

-- Shared definition helpers
tableConstraintValues :: TableDef -> [Constraint]
tableConstraintValues definition =
    tableConstraints definition
        ++ [ ForeignKeyConstraint foreignKey
           | column <- tableColumns definition
           , foreignKey <- columnReferences column
           ]

foreignKeyParentDeps :: SessionState -> Catalog -> [Constraint] -> Set StatementId
foreignKeyParentDeps session catalog constraints =
    Set.unions
        [ relationReadDeps session catalog parentKey ReadSchema [] False
        | ForeignKeyConstraint foreignKey <- constraints
        , parentKey <- resolveRelationKeys session catalog (foreignKeyParent foreignKey)
        ]

resolvedSourceKeys :: SessionState -> Catalog -> [ReadFact] -> Set RelationKey
resolvedSourceKeys session catalog readFacts =
    Set.fromList
        [ key
        | readFact <- readFacts
        , key <- resolveRelationKeys session catalog (readSource readFact)
        , Map.member key (catalogRelations catalog)
        ]

unresolvedSourceRefs :: SessionState -> Catalog -> [ReadFact] -> Set NameRef
unresolvedSourceRefs session catalog readFacts =
    Set.fromList
        [ readSource readFact
        | readFact <- readFacts
        , null
            [ key
            | key <- resolveRelationKeys session catalog (readSource readFact)
            , Map.member key (catalogRelations catalog)
            ]
        ]

-- Tables, views, indexes
processDefineTable :: StatementId -> Eval -> TableDef -> Eval
processDefineTable sid eval definition =
    let session = evalSession eval
        catalog = evalCatalog eval
        key =
            RelationKey
                (tableTemporary definition)
                (qualifyDefinitionName session (tableName definition) (tableTemporary definition))
        existing = lookupRelation catalog key
        evalReads = foldl' (processRead sid) eval (tableSources definition)
        likeDeps =
            case tableLike definition of
                Nothing -> Set.empty
                Just reference ->
                    Set.unions
                        [ relationReadDeps session catalog source ReadSchema [] False
                        | source <- resolveRelationKeys session catalog reference
                        ]
        existingDeps =
            if isJust existing
                then relationAllDeps session catalog key
                else Set.empty
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
        eval2 =
            foldl'
                (flip addBarrier)
                eval1
                [ ScopedBarrier
                    (RelationScope reference)
                    UnresolvedViewSourceBarrier
                    (Just ("unresolved source of table " <> nameText (tableName definition)))
                | reference <- Set.toList unresolved
                ]
     in if tableIfNotExists definition && isJust existing
            then eval2
            else
                let catalog1 = insertRelation key (newRelation TableRelation) (clearRelationObjects key catalog)
                    (catalog2, constraintKeys) =
                        installConstraints session sid key constraintValues catalog1
                    relation =
                        (newRelation TableRelation)
                            { relDefinition = Just sid
                            , relColumns =
                                Set.fromList (map columnName (tableColumns definition))
                            , relSources =
                                resolvedSourceKeys session catalog (tableSources definition)
                            , relUnresolvedSources = unresolved
                            , relSourceReads = tableSources definition
                            , -- Stored unresolved and resolved at use, like
                              -- foreign-key parents. Partition and inheritance
                              -- parents stay distinct for `INSERT` routing.
                              relPartitionParents =
                                Set.fromList
                                    (maybeToList (tablePartitionParent definition))
                            , relInheritParents =
                                Set.fromList (tableInheritParents definition)
                            , relOnCommit = tableOnCommit definition
                            , relOnCommitFrameId =
                                case tableOnCommit definition of
                                    Just OnCommitDrop -> evalFrameId eval
                                    _ -> Nothing
                            , relTransactional = tableTransactional definition
                            , relConstraints = constraintKeys
                            }
                    catalog3 = insertRelation key relation catalog2
                 in eval2 {evalCatalog = catalog3}

processDefineView :: StatementId -> Eval -> ViewDef -> Eval
processDefineView sid eval definition =
    let session = evalSession eval
        catalog = evalCatalog eval
        key =
            RelationKey
                (viewTemporary definition)
                (qualifyDefinitionName session (viewName definition) (viewTemporary definition))
        existing = lookupRelation catalog key
        evalReads = foldl' (processRead sid) eval (viewSources definition)
        existingDeps =
            if isJust existing
                then relationAllDeps session catalog key
                else Set.empty
        eval1 =
            addTouches
                (relationTouchKeys key)
                (addDeps (existingDeps `Set.union` schemaPrefixDeps catalog (relationKeyName key)) evalReads)
        unresolved = unresolvedSourceRefs session catalog (viewSources definition)
        eval2 =
            foldl'
                (flip addBarrier)
                eval1
                [ ScopedBarrier
                    (RelationScope reference)
                    UnresolvedViewSourceBarrier
                    (Just ("unresolved source of view " <> nameText (viewName definition)))
                | reference <- Set.toList unresolved
                ]
     in if viewIfNotExists definition && isJust existing
            then eval2
            else
                let kind =
                        if viewMaterialized definition
                            then MaterializedViewRelation
                            else ViewRelation
                    relation =
                        (newRelation kind)
                            { relDefinition = Just sid
                            , relColumns = Set.fromList (viewColumns definition)
                            , relSources =
                                resolvedSourceKeys session catalog (viewSources definition)
                            , relUnresolvedSources = unresolved
                            , relSourceReads = viewSources definition
                            }
                    catalog1 = insertRelation key relation (clearRelationObjects key catalog)
                 in eval2 {evalCatalog = catalog1}

processDefineIndex :: StatementId -> Eval -> IndexDef -> Eval
processDefineIndex sid eval definition =
    let session = evalSession eval
        catalog = evalCatalog eval
        tableKeys = resolveRelationKeys session catalog (indexTable definition)
        -- The frontend records whether an unqualified index name resolves in
        -- the table's schema or in the current schema.
        indexKeys =
            case (indexRef definition, indexSchemaPlacement definition, tableKeys) of
                (NamedIndex name@(Name [_]), IndexTableSchema, tableKey : _)
                    | length (nameParts (relationKeyName tableKey)) > 1 ->
                        [ NamedIndexKey
                            ( Name
                                ( init (nameParts (relationKeyName tableKey))
                                    ++ nameParts name
                                )
                            )
                        ]
                _ -> resolveIndexKeys session catalog (indexRef definition)
        tableDeps =
            Set.unions
                [ relationReadDeps session catalog tableKey ReadData (indexColumns definition) False
                | tableKey <- tableKeys
                ]
        stepKey current indexKey =
            let existing = Map.lookup indexKey (catalogIndexes (evalCatalog current))
                existingDeps =
                    maybe
                        Set.empty
                        (Set.fromList . catMaybes . (\state -> [indexDefinition state, indexRestoredBy state]))
                        existing
                ownerName =
                    case indexKey of
                        NamedIndexKey name -> name
                        TableIndexKey table _ -> relationKeyName table
                current1 =
                    addTouches
                        (indexTouchKeys indexKey (listToMaybe tableKeys))
                        (addDeps (tableDeps `Set.union` existingDeps `Set.union` schemaPrefixDeps catalog ownerName) current)
             in if indexIfNotExists definition && isJust existing
                    then current1
                    else
                        let oldTableKeys =
                                case existing >>= indexStateTable of
                                    Nothing -> []
                                    Just oldTableKey -> [oldTableKey]
                            catalog0 =
                                foldl'
                                    ( \catalog' oldTableKey ->
                                        catalog'
                                            { catalogRelations =
                                                Map.adjust
                                                    (\relation -> relation {relIndexes = Set.delete indexKey (relIndexes relation)})
                                                    oldTableKey
                                                    (catalogRelations catalog')
                                            }
                                    )
                                    (evalCatalog current1)
                                    oldTableKeys
                            catalog1 =
                                catalog0
                                    { catalogIndexes =
                                        Map.insert
                                            indexKey
                                            (IndexState (Just sid) (listToMaybe tableKeys) (indexColumns definition) Nothing)
                                            (catalogIndexes catalog0)
                                    }
                            catalog2 =
                                foldl'
                                    ( \catalog' tableKey ->
                                        catalog'
                                            { catalogRelations =
                                                Map.adjust
                                                    (\relation -> relation {relIndexes = Set.insert indexKey (relIndexes relation)})
                                                    tableKey
                                                    (catalogRelations catalog')
                                            }
                                    )
                                    catalog1
                                    tableKeys
                         in current1 {evalCatalog = catalog2}
     in case tableKeys of
            _ : _ : _ -> ambiguousMutation eval
            _ -> mutateResolved stepKey eval indexKeys

-- Trigger body dependencies
triggerBodyDeps :: SessionState -> Catalog -> TriggerBody -> Set StatementId
triggerBodyDeps session catalog body =
    case body of
        TriggerInline effects ->
            Set.unions (map effectDeps effects)
        TriggerFunction _ _ -> Set.empty
        TriggerOpaque -> Set.empty
    where
        effectDeps effect =
            case effect of
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

dmlEventTarget :: DmlEvent -> NameRef
dmlEventTarget event =
    case event of
        DmlInsert target _ -> target
        DmlUpdate target _ -> target
        DmlMerge target _ -> target
        DmlDelete target _ -> target
        DmlTruncate target _ -> target

sequenceWriteDeps :: SessionState -> Catalog -> NameRef -> Set StatementId
sequenceWriteDeps session catalog target =
    Set.unions
        [ Set.fromList
            ( catMaybes
                [ sequenceDefinition state
                , sequenceRestoredBy state
                ]
            )
            `Set.union` sequenceWrites state
        | key <- resolveSequenceKeys session catalog target
        , Just state <- [Map.lookup key (catalogSequences catalog)]
        ]

-- Triggers, sequences, types, schemas
processDefineTrigger :: StatementId -> Eval -> TriggerDef -> Eval
processDefineTrigger sid eval definition =
    let session = evalSession eval
        catalog = evalCatalog eval
        triggerKeys = resolveTriggerKeys session catalog (triggerRef definition)
        tableKeys = resolveRelationKeys session catalog (triggerTable definition)
        tableDeps =
            Set.unions
                [ relationAllDeps session catalog tableKey
                | tableKey <- tableKeys
                ]
        referencedDeps =
            case triggerReferencedTable definition of
                Nothing -> Set.empty
                Just reference ->
                    Set.unions
                        [ relationAllDeps session catalog tableKey
                        | tableKey <- resolveRelationKeys session catalog reference
                        ]
        bodyDeps = triggerBodyDeps session catalog (triggerBody definition)
        whenDeps =
            Set.unions
                [ relationReadDeps session catalog key (readMode readFact) (readColumns readFact) (readOnly readFact)
                | readFact <- triggerWhenReads definition
                , key <- resolveRelationKeys session catalog (readSource readFact)
                ]
        eval0 =
            case triggerBody definition of
                TriggerFunction name _ ->
                    -- The function body is opaque to the catalog model. Treat
                    -- the trigger definition like an unknown statement so the
                    -- function definition and other prior catalog state are
                    -- retained.
                    addBarrier
                        ( ScopedBarrier
                            GlobalScope
                            UnsupportedTriggerBodyBarrier
                            (Just ("trigger function " <> nameText name))
                        )
                        eval
                _ -> eval
        eval1 =
            addTouches
                (concatMap (`triggerTouchKeys` listToMaybe tableKeys) triggerKeys)
                (addDeps (tableDeps `Set.union` referencedDeps `Set.union` bodyDeps `Set.union` whenDeps) eval0)
        stepKey current triggerKey =
            let existing = Map.lookup triggerKey (catalogTriggers (evalCatalog current))
                existingDeps =
                    maybe
                        Set.empty
                        (Set.fromList . catMaybes . (\state -> [triggerDefinition state, triggerRestoredBy state]))
                        existing
                ownerName =
                    case triggerKey of
                        NamedTriggerKey name -> name
                        TableTriggerKey table _ -> relationKeyName table
                current1 = addDeps (existingDeps `Set.union` schemaPrefixDeps catalog ownerName) current
             in if triggerIfNotExists definition && isJust existing
                    then current1
                    else
                        let oldTableKeys =
                                case existing >>= triggerStateTable of
                                    Nothing -> []
                                    Just oldTableKey -> [oldTableKey]
                            catalog0 =
                                foldl'
                                    ( \catalog' oldTableKey ->
                                        catalog'
                                            { catalogRelations =
                                                Map.adjust
                                                    (\relation -> relation {relTriggers = Set.delete triggerKey (relTriggers relation)})
                                                    oldTableKey
                                                    (catalogRelations catalog')
                                            }
                                    )
                                    (evalCatalog current1)
                                    oldTableKeys
                            catalog1 =
                                catalog0
                                    { catalogTriggers =
                                        Map.insert
                                            triggerKey
                                            ( TriggerState
                                                (Just sid)
                                                (listToMaybe tableKeys)
                                                (triggerTiming definition)
                                                (triggerEvents definition)
                                                (triggerWhenReads definition)
                                                (triggerBody definition)
                                                Nothing
                                            )
                                            (catalogTriggers catalog0)
                                    }
                            catalog2 =
                                foldl'
                                    ( \catalog' tableKey ->
                                        catalog'
                                            { catalogRelations =
                                                Map.adjust
                                                    (\relation -> relation {relTriggers = Set.insert triggerKey (relTriggers relation)})
                                                    tableKey
                                                    (catalogRelations catalog')
                                            }
                                    )
                                    catalog1
                                    tableKeys
                         in current1 {evalCatalog = catalog2}
     in case tableKeys of
            _ : _ : _ -> ambiguousMutation eval
            _ -> mutateResolved stepKey eval1 triggerKeys

processDefineSequence :: StatementId -> Eval -> SequenceDef -> Eval
processDefineSequence sid eval definition =
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
                Nothing
                (sequenceTransactional definition)
        catalog1 =
            catalog
                { catalogSequences = Map.insert key state (catalogSequences catalog)
                }
     in ( addTouches
            (sequenceTouchKeys key)
            (addDeps (existingDeps `Set.union` schemaPrefixDeps catalog (sequenceKeyName key)) eval)
        )
            { evalCatalog = catalog1
            }

processDefineType :: StatementId -> Eval -> TypeDef -> Eval
processDefineType sid eval definition =
    let catalog = evalCatalog eval
        name = qualifyObjectName (evalSession eval) (typeName definition)
        existing = Map.lookup name (catalogTypes catalog)
        existingDeps =
            maybe
                Set.empty
                (Set.fromList . catMaybes . (\state -> [definitionStatement state, definitionRestoredBy state]))
                existing
        catalog1 =
            catalog
                { catalogTypes =
                    Map.insert name (DefinitionState (Just sid) Nothing) (catalogTypes catalog)
                }
     in ( addTouches
            (typeTouchKeys name)
            (addDeps (existingDeps `Set.union` schemaPrefixDeps catalog name) eval)
        )
            { evalCatalog = catalog1
            }

processDefineSchema :: StatementId -> Eval -> SchemaDef -> Eval
processDefineSchema sid eval definition =
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
                        { catalogSchemas =
                            Map.insert name (DefinitionState (Just sid) Nothing) (catalogSchemas catalog)
                        }
     in ( addTouches
            (schemaTouchKeys name)
            (addDeps (existingDeps `Set.union` schemaPrefixDeps catalog name) eval)
        )
            { evalCatalog = catalog1
            }

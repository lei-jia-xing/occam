-- | RENAME: relations and columns plus view, trigger, and key fixups.
module Occam.Dependency.Rename (
    renameConstraintChildColumns,
    processRename,
    moveRelationObjects,
    updateViewSources,
    updateForeignKeyParents,
    updatePartitionParents,
    renameSourceReadColumns,
    renameTriggerColumns,
    renameTriggerRelations,
    updateTriggerRelations,
    renameForeignKeyParentColumn,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Lens.Micro ((%~), (&), (.~))
import Occam.Core (
    Constraint (..),
    DmlEvent (..),
    ForeignKey (..),
    Ident,
    IndexSchemaPlacement (..),
    Name (..),
    NameRef (..),
    ReadFact (..),
    RelationKind (..),
    RenameOp (..),
    SourceColumn (..),
    StatementId,
    TriggerBody (..),
    TriggerEffect (..),
    ViewColumnLineage (..),
 )
import Occam.Dependency.Barrier (relationTouchKeys)
import Occam.Dependency.Query (relationAllDeps, relationConstraintDeps, schemaPrefixDeps)
import Occam.Dependency.Resolve (qualifyRenameName, resolveRelationKeys)
import Occam.Dependency.State (
    Catalog (..),
    ConstraintKey (..),
    ConstraintState (..),
    Eval (..),
    IndexKey (..),
    IndexState (..),
    RelationKey (..),
    RelationState (..),
    SessionState,
    TriggerKey (..),
    TriggerState (..),
    addDeps,
    addTouches,
    catalogConstraintsL,
    catalogIndexesL,
    catalogRelationsL,
    catalogTriggersL,
    deleteRelation,
    evalCatalogL,
    insertRelation,
    lookupRelation,
    mutateResolved,
    newRelation,
    rebuildForeignKeys,
    relColumnLineageL,
    relColumnWritesL,
    relColumnsL,
    relConditionColumnsL,
    relConstraintsL,
    relIndexesL,
    relInheritParentsL,
    relPartitionParentsL,
    relSchemaWritesL,
    relSourceReadsL,
    relSourcesL,
    relTriggersL,
    relUnresolvedSourcesL,
 )

-- Constraint column renames

-- | Rename child columns within a table's constraint definition.
renameConstraintChildColumns :: (Ident -> Ident) -> Constraint -> Constraint
renameConstraintChildColumns rename constraint =
    case constraint of
        PrimaryKeyConstraint name columns ->
            PrimaryKeyConstraint name (map rename columns)
        UniqueConstraint name columns ->
            UniqueConstraint name (map rename columns)
        ForeignKeyConstraint foreignKey ->
            ForeignKeyConstraint
                foreignKey
                    { foreignKeyChildColumns = map rename (foreignKeyChildColumns foreignKey)
                    }
        CheckConstraint name columns notValid ->
            CheckConstraint name (map rename columns) notValid
        IndexConstraint name columns unique ->
            IndexConstraint name (map rename columns) unique
        UsingIndexConstraint name index primary placement ->
            UsingIndexConstraint name index primary placement
        OtherConstraint name -> OtherConstraint name

-- RENAME
processRename :: StatementId -> Eval -> RenameOp -> Eval
processRename sid eval renameOp =
    case renameOp of
        RenameRelation from to placement ->
            let session = evalSession eval
                catalog = evalCatalog eval
                keys = filter (`Map.member` catalogRelations catalog) (resolveRelationKeys session catalog from)
                stepKey current oldKey =
                    let currentCatalog = evalCatalog current
                        relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog oldKey)
                        deps =
                            relationAllDeps session currentCatalog oldKey
                                `Set.union` relationConstraintDeps currentCatalog relation
                                `Set.union` schemaPrefixDeps currentCatalog (relationKeyName newKey)
                        newKey =
                            RelationKey
                                (relationKeyTemp oldKey)
                                (qualifyRenameName (relationKeyName oldKey) to)
                        (catalog1, relation1) =
                            moveRelationObjects placement sid oldKey newKey relation currentCatalog
                        relation2 =
                            relation1
                                { relSchemaWrites = Set.insert sid (relSchemaWrites relation1)
                                }
                        catalog2 =
                            insertRelation newKey relation2 (deleteRelation oldKey catalog1)
                                & updateTriggerRelations session currentCatalog oldKey newKey
                                & updateViewSources session currentCatalog oldKey newKey
                                & updateForeignKeyParents session currentCatalog oldKey newKey
                                & updatePartitionParents session currentCatalog oldKey newKey
                     in current
                            & addTouches (relationTouchKeys newKey ++ relationTouchKeys oldKey)
                            & addDeps deps
                            & evalCatalogL .~ catalog2
             in mutateResolved stepKey eval keys
        RenameColumnOp table from to ->
            let session = evalSession eval
                catalog = evalCatalog eval
                keys = filter (`Map.member` catalogRelations catalog) (resolveRelationKeys session catalog table)
                stepKey current key =
                    let currentCatalog = evalCatalog current
                        relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog key)
                        deps =
                            relationAllDeps session currentCatalog key
                                `Set.union` relationConstraintDeps currentCatalog relation
                        renameColumn column
                            | column == from = to
                            | otherwise = column
                        relation1 =
                            relation
                                & relColumnsL %~ Set.insert to . Set.delete from
                                & relColumnWritesL
                                    %~ ( \writesMap ->
                                            case Map.lookup from writesMap of
                                                Nothing -> writesMap
                                                Just writes ->
                                                    Map.insertWith
                                                        Set.union
                                                        to
                                                        writes
                                                        (Map.delete from writesMap)
                                       )
                                & relSchemaWritesL %~ Set.insert sid
                                & relColumnLineageL
                                    %~ map
                                        ( \lineage ->
                                            if lineageColumn lineage == from
                                                then lineage {lineageColumn = to}
                                                else lineage
                                        )
                        catalog1 =
                            currentCatalog
                                & catalogConstraintsL
                                    %~ Map.map
                                        ( \state ->
                                            let childRenamed =
                                                    if constraintStateTable state == key
                                                        then
                                                            state
                                                                { constraintValue =
                                                                    renameConstraintChildColumns
                                                                        renameColumn
                                                                        (constraintValue state)
                                                                }
                                                        else state
                                             in renameForeignKeyParentColumn
                                                    session
                                                    currentCatalog
                                                    key
                                                    renameColumn
                                                    childRenamed
                                        )
                                & catalogIndexesL
                                    %~ Map.map
                                        ( \state ->
                                            if indexStateTable state == Just key
                                                then state {indexStateColumns = map renameColumn (indexStateColumns state)}
                                                else state
                                        )
                                & catalogTriggersL
                                    %~ Map.map
                                        (renameTriggerColumns session currentCatalog key renameColumn)
                                & catalogRelationsL
                                    %~ Map.map
                                        (renameSourceReadColumns session currentCatalog key renameColumn)
                        catalog2 =
                            if Map.null (catalogForeignKeys catalog1)
                                then catalog1
                                else catalog1 {catalogForeignKeys = rebuildForeignKeys (catalogConstraints catalog1)}
                     in current
                            & addTouches (relationTouchKeys key)
                            & addDeps deps
                            & evalCatalogL .~ insertRelation key relation1 catalog2
             in mutateResolved stepKey eval keys

moveRelationObjects ::
    IndexSchemaPlacement ->
    StatementId ->
    RelationKey ->
    RelationKey ->
    RelationState ->
    Catalog ->
    (Catalog, RelationState)
moveRelationObjects placement _sid oldKey newKey relation catalog =
    let (catalog1, newIndexes) =
            foldl' (moveIndex placement oldKey newKey) (catalog, Set.empty) (Set.toList (relIndexes relation))
        (catalog2, newTriggers) =
            foldl' (moveTrigger oldKey newKey) (catalog1, Set.empty) (Set.toList (relTriggers relation))
        (catalog3, newConstraints) =
            foldl' (moveConstraint oldKey newKey) (catalog2, Set.empty) (Set.toList (relConstraints relation))
        relation1 =
            relation
                & relIndexesL .~ newIndexes
                & relTriggersL .~ newTriggers
                & relConstraintsL .~ newConstraints
     in (catalog3, relation1)
    where
        moveIndex schemaPlacement _old new (current, keys) indexKey =
            case Map.lookup indexKey (catalogIndexes current) of
                Nothing -> (current, keys)
                Just state ->
                    let newIndexKey =
                            case indexKey of
                                NamedIndexKey name ->
                                    case schemaPlacement of
                                        IndexTableSchema ->
                                            -- Table-schema placement moves an unqualified index into table schema.
                                            case (nameParts name, nameParts (relationKeyName new)) of
                                                ([_], newParts)
                                                    | length newParts > 1 ->
                                                        NamedIndexKey (Name (init newParts ++ nameParts name))
                                                _ -> NamedIndexKey name
                                        IndexCurrentSchema -> NamedIndexKey name
                                TableIndexKey _ ident -> TableIndexKey new ident
                        state1 = state {indexStateTable = Just new}
                     in ( current
                            & catalogIndexesL %~ Map.insert newIndexKey state1 . Map.delete indexKey
                        , Set.insert newIndexKey keys
                        )
        moveTrigger _old new (current, keys) triggerKey =
            case Map.lookup triggerKey (catalogTriggers current) of
                Nothing -> (current, keys)
                Just state ->
                    let newTriggerKey =
                            case triggerKey of
                                NamedTriggerKey name -> NamedTriggerKey name
                                TableTriggerKey _ ident -> TableTriggerKey new ident
                        state1 = state {triggerStateTable = Just new}
                     in ( current
                            & catalogTriggersL %~ Map.insert newTriggerKey state1 . Map.delete triggerKey
                        , Set.insert newTriggerKey keys
                        )
        moveConstraint _old new (current, keys) constraintKey =
            case Map.lookup constraintKey (catalogConstraints current) of
                Nothing -> (current, keys)
                Just state ->
                    let newConstraintKey = ConstraintKey new (constraintKeyIdentity constraintKey)
                        state1 = state {constraintStateTable = new}
                        fks' = case constraintValue state1 of
                            ForeignKeyConstraint fk ->
                                Map.insert newConstraintKey (state1, fk) (Map.delete constraintKey (catalogForeignKeys current))
                            _ -> catalogForeignKeys current
                     in ( current
                            & catalogConstraintsL %~ Map.insert newConstraintKey state1 . Map.delete constraintKey
                            & (\c -> c {catalogForeignKeys = fks'})
                        , Set.insert newConstraintKey keys
                        )

updateViewSources :: SessionState -> Catalog -> RelationKey -> RelationKey -> Catalog -> Catalog
updateViewSources session resolutionCatalog oldKey newKey catalog =
    catalog
        & catalogRelationsL
            %~ Map.map
                ( \relation ->
                    relation
                        & relSourcesL
                            %~ (\srcs -> if Set.member oldKey srcs then Set.insert newKey (Set.delete oldKey srcs) else srcs)
                        & relUnresolvedSourcesL %~ Set.map updateUnresolved
                        & relSourceReadsL %~ map updateRead
                        & relColumnLineageL %~ map updateLineage
                        & relConditionColumnsL %~ map updateSourceCol
                )
    where
        newReference = NameRef (relationKeyName newKey) (relationKeyTemp newKey)
        resolvesToOldKey ref =
            oldKey `elem` resolveRelationKeys session resolutionCatalog ref
        updateSourceCol sc
            | resolvesToOldKey (sourceColumnRelation sc) =
                sc {sourceColumnRelation = newReference}
            | otherwise = sc
        updateLineage lineage =
            lineage
                { lineageSources = fmap (map updateSourceCol) (lineageSources lineage)
                }
        updateUnresolved reference
            | resolveRelationKeys session resolutionCatalog reference == [oldKey] =
                newReference
            | otherwise = reference
        updateRead readFact
            | resolvesToOldKey (readSource readFact) =
                readFact {readSource = newReference}
            | otherwise = readFact

updateForeignKeyParents ::
    SessionState ->
    Catalog ->
    RelationKey ->
    RelationKey ->
    Catalog ->
    Catalog
updateForeignKeyParents session resolutionCatalog oldKey newKey catalog
    | Map.null (catalogForeignKeys catalog) = catalog
    | otherwise =
        let constraints' = Map.map adjust (catalogConstraints catalog)
         in catalog
                { catalogConstraints = constraints'
                , catalogForeignKeys = rebuildForeignKeys constraints'
                }
    where
        adjust state =
            case constraintValue state of
                ForeignKeyConstraint foreignKey
                    | resolveRelationKeys session resolutionCatalog (foreignKeyParent foreignKey) == [oldKey] ->
                        state
                            { constraintValue =
                                ForeignKeyConstraint
                                    foreignKey
                                        { foreignKeyParent =
                                            NameRef (relationKeyName newKey) (relationKeyTemp newKey)
                                        }
                            }
                _ -> state

updatePartitionParents ::
    SessionState ->
    Catalog ->
    RelationKey ->
    RelationKey ->
    Catalog ->
    Catalog
updatePartitionParents session resolutionCatalog oldKey newKey catalog =
    catalog & catalogRelationsL %~ Map.map adjust
    where
        newReference = NameRef (relationKeyName newKey) (relationKeyTemp newKey)
        adjust relation =
            relation
                & relPartitionParentsL %~ rewrite
                & relInheritParentsL %~ rewrite
        rewrite =
            Set.map
                ( \reference ->
                    if resolveRelationKeys session resolutionCatalog reference == [oldKey]
                        then newReference
                        else reference
                )

renameSourceReadColumns ::
    SessionState ->
    Catalog ->
    RelationKey ->
    (Ident -> Ident) ->
    RelationState ->
    RelationState
renameSourceReadColumns session catalog key rename relation =
    relation
        & relSourceReadsL %~ map renameRead
        & relColumnLineageL %~ map renameLineage
        & relConditionColumnsL %~ map renameSourceCol
    where
        resolvesToKey ref = key `elem` resolveRelationKeys session catalog ref
        renameSourceCol sc
            | resolvesToKey (sourceColumnRelation sc) =
                sc {sourceColumnName = rename (sourceColumnName sc)}
            | otherwise = sc
        renameLineage lineage =
            lineage
                { lineageSources = fmap (map renameSourceCol) (lineageSources lineage)
                }
        renameRead readFact
            | resolvesToKey (readSource readFact) =
                readFact {readColumns = map rename (readColumns readFact)}
            | otherwise = readFact

-- | Rewrite read facts and inline DML events carried by a trigger.
renameTriggerReadsAndBody ::
    (ReadFact -> ReadFact) ->
    (DmlEvent -> DmlEvent) ->
    TriggerState ->
    TriggerState
renameTriggerReadsAndBody renameRead renameEvent state =
    state
        { triggerStateWhenReads = map renameRead (triggerStateWhenReads state)
        , triggerStateBody = renameBody (triggerStateBody state)
        }
    where
        renameEffect effect =
            case effect of
                TriggerDml event -> TriggerDml (renameEvent event)
                TriggerRead readFact -> TriggerRead (renameRead readFact)
                other -> other
        renameBody body =
            case body of
                TriggerInline effects -> TriggerInline (map renameEffect effects)
                other -> other

renameTriggerColumns ::
    SessionState ->
    Catalog ->
    RelationKey ->
    (Ident -> Ident) ->
    TriggerState ->
    TriggerState
renameTriggerColumns session catalog key rename =
    renameTriggerReadsAndBody renameRead renameEvent
    where
        resolvesToKey reference =
            resolveRelationKeys session catalog reference == [key]
        renameRead readFact
            | resolvesToKey (readSource readFact) =
                readFact {readColumns = map rename (readColumns readFact)}
            | otherwise = readFact
        renameEvent event =
            case event of
                DmlInsert target columns
                    | resolvesToKey target -> DmlInsert target (map rename columns)
                DmlUpdate target columns
                    | resolvesToKey target -> DmlUpdate target (map rename columns)
                DmlMerge target columns
                    | resolvesToKey target -> DmlMerge target (map rename columns)
                other -> other

renameTriggerRelations ::
    SessionState ->
    Catalog ->
    RelationKey ->
    RelationKey ->
    TriggerState ->
    TriggerState
renameTriggerRelations session resolutionCatalog oldKey newKey =
    renameTriggerReadsAndBody renameRead renameEvent
    where
        newReference = NameRef (relationKeyName newKey) (relationKeyTemp newKey)
        resolvesToOld reference =
            resolveRelationKeys session resolutionCatalog reference == [oldKey]
        renameRead readFact
            | resolvesToOld (readSource readFact) = readFact {readSource = newReference}
            | otherwise = readFact
        renameEvent event =
            case event of
                DmlInsert target columns
                    | resolvesToOld target -> DmlInsert newReference columns
                DmlUpdate target columns
                    | resolvesToOld target -> DmlUpdate newReference columns
                DmlMerge target columns
                    | resolvesToOld target -> DmlMerge newReference columns
                DmlDelete target allRows
                    | resolvesToOld target -> DmlDelete newReference allRows
                DmlTruncate target cascade
                    | resolvesToOld target -> DmlTruncate newReference cascade
                other -> other

updateTriggerRelations ::
    SessionState ->
    Catalog ->
    RelationKey ->
    RelationKey ->
    Catalog ->
    Catalog
updateTriggerRelations session resolutionCatalog oldKey newKey catalog =
    catalog
        { catalogTriggers =
            Map.map
                (renameTriggerRelations session resolutionCatalog oldKey newKey)
                (catalogTriggers catalog)
        }

renameForeignKeyParentColumn ::
    SessionState ->
    Catalog ->
    RelationKey ->
    (Ident -> Ident) ->
    ConstraintState ->
    ConstraintState
renameForeignKeyParentColumn session catalog key rename state =
    case constraintValue state of
        ForeignKeyConstraint foreignKey
            | key `elem` resolveRelationKeys session catalog (foreignKeyParent foreignKey) ->
                state
                    { constraintValue =
                        ForeignKeyConstraint
                            foreignKey
                                { foreignKeyParentColumns =
                                    map rename (foreignKeyParentColumns foreignKey)
                                }
                    }
        _ -> state

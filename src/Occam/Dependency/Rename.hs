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

import Data.Foldable qualified as Foldable
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
import Occam.Dependency.Resolve (
    qualifyRenameName,
    resolvePermanentRelationKeys,
    resolveRelationKeys,
 )
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
processRename sid eval (RenameRelation from to placement) =
    renameRelationWith sid resolveRelationKeys eval from to placement
processRename sid eval (RenamePermanentRelation from to placement) =
    renameRelationWith sid resolvePermanentRelationKeys eval from to placement
processRename sid eval (RenameColumnOp table from to) =
    mutateResolved stepKey eval keys
    where
        session = evalSession eval
        catalog = evalCatalog eval
        keys = filter (`Map.member` catalogRelations catalog) (resolveRelationKeys session catalog table)
        renameColumn column
            | column == from = to
            | otherwise = column
        stepKey current key =
            current
                & addTouches (relationTouchKeys key)
                & addDeps deps
                & evalCatalogL .~ insertRelation key relation1 catalog1
            where
                currentCatalog = evalCatalog current
                relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog key)
                deps =
                    relationAllDeps session currentCatalog key
                        `Set.union` relationConstraintDeps currentCatalog relation
                relation1 = renameRelationColumns from to sid relation
                catalog1 = renameCatalogColumns session currentCatalog key renameColumn

renameRelationWith ::
    StatementId ->
    (SessionState -> Catalog -> NameRef -> [RelationKey]) ->
    Eval ->
    NameRef ->
    Name ->
    IndexSchemaPlacement ->
    Eval
renameRelationWith sid resolveFn eval from to placement =
    mutateResolved stepKey eval keys
    where
        session = evalSession eval
        catalog = evalCatalog eval
        keys = filter (`Map.member` catalogRelations catalog) (resolveFn session catalog from)
        stepKey current oldKey =
            current
                & addTouches (relationTouchKeys newKey ++ relationTouchKeys oldKey)
                & addDeps deps
                & evalCatalogL .~ catalog2
            where
                currentCatalog = evalCatalog current
                relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog oldKey)
                newKey = RelationKey (relationKeyTemp oldKey) (qualifyRenameName (relationKeyName oldKey) to)
                deps =
                    relationAllDeps session currentCatalog oldKey
                        `Set.union` relationConstraintDeps currentCatalog relation
                        `Set.union` schemaPrefixDeps currentCatalog (relationKeyName newKey)
                (catalog1, relation1) =
                    moveRelationObjects placement sid oldKey newKey relation currentCatalog
                relation2 = relation1 {relSchemaWrites = Set.insert sid (relSchemaWrites relation1)}
                catalog2 =
                    insertRelation newKey relation2 (deleteRelation oldKey catalog1)
                        & updateTriggerRelations session currentCatalog oldKey newKey
                        & updateViewSources session currentCatalog oldKey newKey
                        & updateForeignKeyParents session currentCatalog oldKey newKey
                        & updatePartitionParents session currentCatalog oldKey newKey

renameRelationColumns :: Ident -> Ident -> StatementId -> RelationState -> RelationState
renameRelationColumns from to sid relation =
    relation
        & relColumnsL %~ Set.insert to . Set.delete from
        & relColumnWritesL %~ renameWrites
        & relSchemaWritesL %~ Set.insert sid
        & relColumnLineageL %~ map renameLineage
    where
        renameWrites writesMap =
            maybe
                writesMap
                (\writes -> Map.insertWith Set.union to writes (Map.delete from writesMap))
                (Map.lookup from writesMap)
        renameLineage lineage
            | lineageColumn lineage == from = lineage {lineageColumn = to}
            | otherwise = lineage

renameCatalogColumns ::
    SessionState -> Catalog -> RelationKey -> (Ident -> Ident) -> Catalog
renameCatalogColumns session currentCatalog key renameColumn
    | Map.null (catalogForeignKeys catalog1) = catalog1
    | otherwise = catalog1 {catalogForeignKeys = rebuildForeignKeys (catalogConstraints catalog1)}
    where
        catalog1 =
            currentCatalog
                & catalogConstraintsL %~ Map.map renameConstraint
                & catalogIndexesL %~ Map.map renameIndex
                & catalogTriggersL %~ Map.map (renameTriggerColumns session currentCatalog key renameColumn)
                & catalogRelationsL %~ Map.map (renameSourceReadColumns session currentCatalog key renameColumn)
        renameConstraint state =
            renameForeignKeyParentColumn session currentCatalog key renameColumn childRenamed
            where
                childRenamed
                    | constraintStateTable state == key =
                        state {constraintValue = renameConstraintChildColumns renameColumn (constraintValue state)}
                    | otherwise = state
        renameIndex state
            | indexStateTable state == Just key =
                state {indexStateColumns = map renameColumn (indexStateColumns state)}
            | otherwise = state

moveRelationObjects ::
    IndexSchemaPlacement ->
    StatementId ->
    RelationKey ->
    RelationKey ->
    RelationState ->
    Catalog ->
    (Catalog, RelationState)
moveRelationObjects placement _sid _oldKey newKey relation catalog =
    (catalog3, relation1)
    where
        (catalog1, newIndexes) =
            Foldable.foldl'
                (moveRelationIndex placement newKey)
                (catalog, Set.empty)
                (Set.toList (relIndexes relation))
        (catalog2, newTriggers) =
            Foldable.foldl'
                (moveRelationTrigger newKey)
                (catalog1, Set.empty)
                (Set.toList (relTriggers relation))
        (catalog3, newConstraints) =
            Foldable.foldl'
                (moveRelationConstraint newKey)
                (catalog2, Set.empty)
                (Set.toList (relConstraints relation))
        relation1 =
            relation
                & relIndexesL .~ newIndexes
                & relTriggersL .~ newTriggers
                & relConstraintsL .~ newConstraints

moveRelationIndex ::
    IndexSchemaPlacement ->
    RelationKey ->
    (Catalog, Set.Set IndexKey) ->
    IndexKey ->
    (Catalog, Set.Set IndexKey)
moveRelationIndex schemaPlacement new (current, keys) indexKey
    | Just state <- Map.lookup indexKey (catalogIndexes current) =
        ( current
            & catalogIndexesL %~ Map.insert newIndexKey (state {indexStateTable = Just new}) . Map.delete indexKey
        , Set.insert newIndexKey keys
        )
    | otherwise = (current, keys)
    where
        newIndexKey = renameIndexKey schemaPlacement new indexKey

renameIndexKey :: IndexSchemaPlacement -> RelationKey -> IndexKey -> IndexKey
renameIndexKey _ new (TableIndexKey _ ident) = TableIndexKey new ident
renameIndexKey IndexTableSchema new (NamedIndexKey name)
    | [_] <- nameParts name
    , newParts@(_ : _ : _) <- nameParts (relationKeyName new) =
        NamedIndexKey (Name (init newParts ++ nameParts name))
renameIndexKey _ _ key = key

moveRelationTrigger ::
    RelationKey ->
    (Catalog, Set.Set TriggerKey) ->
    TriggerKey ->
    (Catalog, Set.Set TriggerKey)
moveRelationTrigger new (current, keys) triggerKey
    | Just state <- Map.lookup triggerKey (catalogTriggers current) =
        ( current
            & catalogTriggersL
                %~ Map.insert newTriggerKey (state {triggerStateTable = Just new}) . Map.delete triggerKey
        , Set.insert newTriggerKey keys
        )
    | otherwise = (current, keys)
    where
        newTriggerKey = case triggerKey of
            NamedTriggerKey name -> NamedTriggerKey name
            TableTriggerKey _ ident -> TableTriggerKey new ident

moveRelationConstraint ::
    RelationKey ->
    (Catalog, Set.Set ConstraintKey) ->
    ConstraintKey ->
    (Catalog, Set.Set ConstraintKey)
moveRelationConstraint new (current, keys) constraintKey
    | Just state <- Map.lookup constraintKey (catalogConstraints current) =
        let newConstraintKey = ConstraintKey new (constraintKeyIdentity constraintKey)
            state1 = state {constraintStateTable = new}
            fks' = case constraintValue state1 of
                ForeignKeyConstraint fk ->
                    Map.insert newConstraintKey (state1, fk) (Map.delete constraintKey (catalogForeignKeys current))
                _ -> catalogForeignKeys current
            catalog' =
                current
                    { catalogConstraints =
                        Map.insert newConstraintKey state1 (Map.delete constraintKey (catalogConstraints current))
                    , catalogForeignKeys = fks'
                    }
         in (catalog', Set.insert newConstraintKey keys)
    | otherwise = (current, keys)

updateViewSources :: SessionState -> Catalog -> RelationKey -> RelationKey -> Catalog -> Catalog
updateViewSources session resolutionCatalog oldKey newKey catalog =
    catalog & catalogRelationsL %~ Map.map updateRelation
    where
        newReference = NameRef (relationKeyName newKey) (relationKeyTemp newKey)
        resolvesToOldKey ref = oldKey `elem` resolveRelationKeys session resolutionCatalog ref
        updateSources srcs
            | Set.member oldKey srcs = Set.insert newKey (Set.delete oldKey srcs)
            | otherwise = srcs
        updateRelation relation =
            relation
                & relSourcesL %~ updateSources
                & relUnresolvedSourcesL %~ Set.map updateUnresolved
                & relSourceReadsL %~ map updateRead
                & relColumnLineageL %~ map updateLineage
                & relConditionColumnsL %~ map updateSourceCol
        updateSourceCol sc
            | resolvesToOldKey (sourceColumnRelation sc) = sc {sourceColumnRelation = newReference}
            | otherwise = sc
        updateLineage lineage =
            lineage {lineageSources = fmap (map updateSourceCol) (lineageSources lineage)}
        updateUnresolved reference
            | resolveRelationKeys session resolutionCatalog reference == [oldKey] = newReference
            | otherwise = reference
        updateRead readFact
            | resolvesToOldKey (readSource readFact) = readFact {readSource = newReference}
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
        newRef = NameRef (relationKeyName newKey) (relationKeyTemp newKey)
        adjust state
            | ForeignKeyConstraint foreignKey <- constraintValue state
            , resolveRelationKeys session resolutionCatalog (foreignKeyParent foreignKey) == [oldKey] =
                state {constraintValue = ForeignKeyConstraint (foreignKey {foreignKeyParent = newRef})}
            | otherwise = state

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
renameForeignKeyParentColumn session catalog key rename state
    | ForeignKeyConstraint foreignKey <- constraintValue state
    , key `elem` resolveRelationKeys session catalog (foreignKeyParent foreignKey) =
        let fk' = foreignKey {foreignKeyParentColumns = map rename (foreignKeyParentColumns foreignKey)}
         in state {constraintValue = ForeignKeyConstraint fk'}
    | otherwise = state

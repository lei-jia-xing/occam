-- | ALTER TABLE: column, constraint, index, and partition/inheritance-routing changes.
module Occam.Dependency.Alter (
    constraintColumnsOf,
    introducedForeignKeysOf,
    dropColumnDependentViews,
    viewDependsOnColumn,
    processAlterTable,
    processAttachPartition,
    processDetachPartition,
    processAttachInherit,
    processDetachInherit,
    applyAlterChange,
    renameConstraintName,
    validateConstraintValue,
    indexKeyName,
) where

import Control.Monad.Trans.State.Strict (State, get, modify')
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core.Schema (
    AlterChange (..),
    AlterTable (..),
    ColumnDef (..),
    Constraint (..),
    ConstraintValidity (..),
    DropMode (..),
    ForeignKey (..),
    Ident (..),
    NameRef (..),
    PartitionLink (..),
    ReadMode (..),
    RelationKind (..),
 )
import Occam.Core.Types (StatementId)
import Occam.Dependency.Barrier (relationTouchKeys)
import Occam.Dependency.Drop (removeRelationCascade)
import Occam.Dependency.Query (
    allForeignKeys,
    constraintColumnsOf,
    installConstraints,
    referencedParentColumns,
    relationAllDeps,
    relationConstraintDeps,
    relationDefinitionDeps,
    relationReadDeps,
    viewSourceRequirements,
 )
import Occam.Dependency.Resolve (resolveRelationKeys)
import Occam.Dependency.State (
    Catalog (..),
    ConstraintIdentity (..),
    ConstraintKey (..),
    ConstraintState (..),
    Eval (..),
    IndexKey (..),
    IndexState (..),
    RelationKey (..),
    RelationState (..),
    SessionState,
    addDeps,
    addTouches,
    ambiguousMutation,
    deleteConstraint,
    deleteIndex,
    insertRelation,
    lookupRelation,
    nameLast,
    newRelation,
 )

-- Constraint columns

-- ALTER TABLE

-- | Find foreign keys introduced by ADD CONSTRAINT or ADD COLUMN changes.
introducedForeignKeysOf :: AlterChange -> [ForeignKey]
introducedForeignKeysOf change =
    case change of
        AddConstraint (ForeignKeyConstraint foreignKey) -> [foreignKey]
        AddColumn column constraints ->
            [foreignKey | ForeignKeyConstraint foreignKey <- constraints]
                ++ columnReferences column
        _ -> []

-- | Find views dependent on the dropped column via column lineage.
dropColumnDependentViews :: SessionState -> Catalog -> RelationKey -> Ident -> [RelationKey]
dropColumnDependentViews session catalog tableKey column =
    [ viewKey
    | viewKey <- Map.keys (catalogRelations catalog)
    , viewKey /= tableKey
    , viewDependsOnColumn session catalog Set.empty tableKey column viewKey
    ]

-- | Test transitive view lineage for dependence on one source column.
viewDependsOnColumn ::
    SessionState ->
    Catalog ->
    Set RelationKey ->
    RelationKey ->
    Ident ->
    RelationKey ->
    Bool
viewDependsOnColumn session catalog visited tableKey column viewKey =
    viewTransitiveDependsOn session catalog visited tableKey column viewKey Nothing

viewTransitiveDependsOn ::
    SessionState ->
    Catalog ->
    Set RelationKey ->
    RelationKey ->
    Ident ->
    RelationKey ->
    Maybe (Set Ident) ->
    Bool
viewTransitiveDependsOn session catalog visited tableKey column currentKey requestedCols
    | Set.member currentKey visited = False
    | otherwise =
        case Map.lookup currentKey (catalogRelations catalog) of
            Just relation
                | relKind relation `elem` [ViewRelation, MaterializedViewRelation] ->
                    let visited' = Set.insert currentKey visited
                        reqs = viewSourceRequirements session catalog (relationKeyTemp currentKey) relation requestedCols
                     in any (checkSource visited') (Map.toList reqs)
                | otherwise ->
                    currentKey == tableKey
                        && case requestedCols of
                            Nothing -> True
                            Just cols -> Set.member column cols
            Nothing -> False
    where
        checkSource visited' (sourceKey, neededCols)
            | sourceKey == tableKey =
                case neededCols of
                    Nothing -> True
                    Just cols -> Set.member column cols
            | otherwise =
                viewTransitiveDependsOn session catalog visited' tableKey column sourceKey neededCols

-- | Apply ALTER TABLE changes to each unambiguous target and record dependencies.
processAlterTable :: StatementId -> AlterTable -> State Eval ()
processAlterTable sid alter = do
    eval <- get
    let session = evalSession eval
        catalog = evalCatalog eval
        keys = resolveRelationKeys session catalog (alterTableTarget alter)
        changes = alterTableChanges alter
        introducedForeignKeys = concatMap introducedForeignKeysOf changes
        foreignKeyDeps =
            Set.unions
                [ relationReadDeps
                    session
                    catalog
                    parentKey
                    (if foreignKeyNotValid foreignKey then ReadSchema else ReadData)
                    []
                    False
                | foreignKey <- introducedForeignKeys
                , parentKey <- resolveRelationKeys session catalog (foreignKeyParent foreignKey)
                ]
    case keys of
        _ : _ : _ -> modify' ambiguousMutation
        [] -> pure ()
        [key] -> alterRelationKey session sid changes foreignKeyDeps key

alterRelationKey ::
    SessionState -> StatementId -> [AlterChange] -> Set StatementId -> RelationKey -> State Eval ()
alterRelationKey session sid changes foreignKeyDeps key =
    modify' $ \current ->
        let currentCatalog = evalCatalog current
            relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog key)
            validationDeps =
                Set.unions
                    [ relationReadDeps session currentCatalog parentKey ReadData [] False
                    | ValidateConstraint name <- changes
                    , constraintKey <- Set.toList (relConstraints relation)
                    , constraintKeyIdentity constraintKey == NamedConstraint name
                    , Just state <- [Map.lookup constraintKey (catalogConstraints currentCatalog)]
                    , ForeignKeyConstraint foreignKey <- [constraintValue state]
                    , parentKey <- resolveRelationKeys session currentCatalog (foreignKeyParent foreignKey)
                    ]
            dropColumnViewDeps =
                Set.unions
                    [ relationAllDeps session currentCatalog viewKey
                        `Set.union` relationConstraintDeps currentCatalog viewRelation
                    | DropColumn column _ <- changes
                    , viewKey <- dropColumnDependentViews session currentCatalog key column
                    , Just viewRelation <- [lookupRelation currentCatalog viewKey]
                    ]
            deps =
                relationAllDeps session currentCatalog key
                    `Set.union` relationConstraintDeps currentCatalog relation
                    `Set.union` foreignKeyDeps
                    `Set.union` validationDeps
                    `Set.union` dropColumnViewDeps
            current1 = addTouches (relationTouchKeys key) (addDeps deps current)
            (catalog1, relation1) = Foldable.foldl' (applyAlterChange session sid key) (currentCatalog, relation) changes
            relation2 = relation1 {relSchemaWrites = Set.insert sid (relSchemaWrites relation1)}
         in current1 {evalCatalog = insertRelation key relation2 catalog1}

-- | Process @ALTER TABLE ... ATTACH PARTITION@.
processAttachPartition :: StatementId -> PartitionLink -> State Eval ()
processAttachPartition sid link = attachDetachPartition sid link True

-- | Process @ALTER TABLE ... DETACH PARTITION@.
processDetachPartition :: StatementId -> PartitionLink -> State Eval ()
processDetachPartition sid link = attachDetachPartition sid link False

attachDetachPartition :: StatementId -> PartitionLink -> Bool -> State Eval ()
attachDetachPartition =
    attachDetachLink relPartitionParents (\relation links -> relation {relPartitionParents = links})

-- | Process @ALTER TABLE ... INHERIT@.
processAttachInherit :: StatementId -> PartitionLink -> State Eval ()
processAttachInherit sid link = attachDetachInherit sid link True

-- | Process @ALTER TABLE ... NO INHERIT@.
processDetachInherit :: StatementId -> PartitionLink -> State Eval ()
processDetachInherit sid link = attachDetachInherit sid link False

attachDetachInherit :: StatementId -> PartitionLink -> Bool -> State Eval ()
attachDetachInherit =
    attachDetachLink relInheritParents (\relation links -> relation {relInheritParents = links})

-- | Attach or detach a parent-child routing link.
attachDetachLink ::
    (RelationState -> Set NameRef) ->
    (RelationState -> Set NameRef -> RelationState) ->
    StatementId ->
    PartitionLink ->
    Bool ->
    State Eval ()
attachDetachLink getLinks setLinks sid link isAttach = do
    eval <- get
    let session = evalSession eval
        catalog = evalCatalog eval
        existing reference =
            filter (`Map.member` catalogRelations catalog) (resolveRelationKeys session catalog reference)
        pairs =
            [ (parentKey, childKey)
            | parentKey <- existing (linkParent link)
            , childKey <- existing (linkChild link)
            ]
        step (parentKey, childKey) =
            modify' $ \current ->
                let currentCatalog = evalCatalog current
                    child = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog childKey)
                    parent = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog parentKey)
                    deps =
                        relationDefinitionDeps currentCatalog parentKey
                            `Set.union` relationDefinitionDeps currentCatalog childKey
                    current1 =
                        addTouches
                            (relationTouchKeys parentKey ++ relationTouchKeys childKey)
                            (addDeps deps current)
                    updatedLinks
                        | isAttach = Set.insert (linkParent link) (getLinks child)
                        | otherwise =
                            Set.filter
                                (\reference -> parentKey `notElem` resolveRelationKeys session currentCatalog reference)
                                (getLinks child)
                    parent1 = parent {relSchemaWrites = Set.insert sid (relSchemaWrites parent)}
                    catalog1 =
                        insertRelation
                            childKey
                            (setLinks child updatedLinks)
                            (insertRelation parentKey parent1 currentCatalog)
                 in current1 {evalCatalog = catalog1}
    case pairs of
        _ : _ : _ -> modify' ambiguousMutation
        _ -> Foldable.foldlM (\() pair -> step pair) () pairs

-- | Apply one normalized alteration to a relation and its catalog objects.
applyAlterChange ::
    SessionState ->
    StatementId ->
    RelationKey ->
    (Catalog, RelationState) ->
    AlterChange ->
    (Catalog, RelationState)
applyAlterChange session sid key current change =
    case change of
        AddColumn column constraints ->
            applyAddColumn session sid key current column constraints
        DropColumn column cascade ->
            applyDropColumn session key current column cascade
        AddConstraint constraint ->
            applyAddConstraint session sid key current constraint
        DropConstraint name cascade ->
            applyDropConstraint session key current name cascade
        RenameConstraint from to ->
            applyRenameConstraint key current from to
        ValidateConstraint name ->
            applyValidateConstraint current name
        DropIndexChange name ->
            applyDropIndexChange current name
        AlterColumn column _ _ _ ->
            applyAlterColumn current column
        OtherChange _ -> current

applyAddColumn ::
    SessionState ->
    StatementId ->
    RelationKey ->
    (Catalog, RelationState) ->
    ColumnDef ->
    [Constraint] ->
    (Catalog, RelationState)
applyAddColumn session sid key (catalog, relation) column constraints =
    let allConstraints =
            constraints
                ++ [ForeignKeyConstraint foreignKey | foreignKey <- columnReferences column]
        (catalog1, keys) = installConstraints session sid key allConstraints catalog
     in ( catalog1
        , relation
            { relColumns = Set.insert (columnName column) (relColumns relation)
            , relConstraints = Set.union keys (relConstraints relation)
            }
        )

applyDropColumn ::
    SessionState ->
    RelationKey ->
    (Catalog, RelationState) ->
    Ident ->
    Bool ->
    (Catalog, RelationState)
applyDropColumn session key (catalog, relation) column cascade =
    let isParentColumn foreignKey =
            column `elem` referencedParentColumns session catalog foreignKey
                && key
                    `elem` resolveRelationKeys
                        session
                        catalog
                        (foreignKeyParent foreignKey)
        -- Internal table constraints on the dropped column.
        sameTableKeys =
            [ constraintKey
            | constraintKey <- Set.toList (relConstraints relation)
            , Just state <- [Map.lookup constraintKey (catalogConstraints catalog)]
            , let constraint = constraintValue state
            , column `elem` constraintColumnsOf constraint
                || case constraint of
                    ForeignKeyConstraint foreignKey -> isParentColumn foreignKey
                    _ -> False
            ]
        sameTableIndexes =
            [ indexKey
            | indexKey <- Set.toList (relIndexes relation)
            , Just state <- [Map.lookup indexKey (catalogIndexes catalog)]
            , indexStateTable state == Just key
            , column `elem` indexStateColumns state
            ]
        -- External foreign keys referencing this column as a parent.
        crossTableKeys =
            [ constraintKey
            | (constraintKey, state, foreignKey) <- allForeignKeys catalog
            , constraintStateTable state /= key
            , isParentColumn foreignKey
            ]
        -- Self-referential foreign keys on the dropped column.
        selfParentKeys =
            [ constraintKey
            | (constraintKey, state, foreignKey) <- allForeignKeys catalog
            , constraintStateTable state == key
            , isParentColumn foreignKey
            ]
        -- Views that depend on the dropped column.
        dependentViews = dropColumnDependentViews session catalog key column
        hasExternalDependents =
            not (null crossTableKeys)
                || not (null selfParentKeys)
                || not (null dependentViews)
        removedKeys =
            sameTableKeys
                ++ if cascade
                    then crossTableKeys ++ selfParentKeys
                    else []
        catalog1 =
            if cascade
                then
                    Foldable.foldl'
                        (flip (removeRelationCascade session DropCascade))
                        catalog
                        dependentViews
                else catalog
     in if not cascade && hasExternalDependents
            then (catalog, relation)
            else
                ( Foldable.foldl'
                    (flip deleteIndex)
                    (Foldable.foldl' (flip deleteConstraint) catalog1 removedKeys)
                    sameTableIndexes
                , relation
                    { relColumns = Set.delete column (relColumns relation)
                    , relColumnWrites = Map.delete column (relColumnWrites relation)
                    , relConstraints = relConstraints relation `Set.difference` Set.fromList removedKeys
                    , relIndexes = relIndexes relation `Set.difference` Set.fromList sameTableIndexes
                    }
                )

applyAddConstraint ::
    SessionState ->
    StatementId ->
    RelationKey ->
    (Catalog, RelationState) ->
    Constraint ->
    (Catalog, RelationState)
applyAddConstraint session sid key (catalog, relation) constraint =
    let (catalog1, keys) = installConstraints session sid key [constraint] catalog
     in (catalog1, relation {relConstraints = Set.union keys (relConstraints relation)})

applyDropConstraint ::
    SessionState ->
    RelationKey ->
    (Catalog, RelationState) ->
    Ident ->
    Bool ->
    (Catalog, RelationState)
applyDropConstraint session key (catalog, relation) name cascade =
    let matching =
            [ constraintKey
            | constraintKey <- Set.toList (relConstraints relation)
            , constraintKeyIdentity constraintKey == NamedConstraint name
            ]
        -- Target columns for dropped primary key or unique constraints.
        dropTarget droppedState =
            case constraintValue droppedState of
                PrimaryKeyConstraint _ columns ->
                    Just (True, Set.fromList columns)
                UniqueConstraint _ columns ->
                    Just (False, Set.fromList columns)
                _ -> Nothing
        dependsOn (primary, targetColumns) keyValue =
            let parentColumns = Set.fromList (foreignKeyParentColumns keyValue)
             in key
                    `elem` resolveRelationKeys
                        session
                        catalog
                        (foreignKeyParent keyValue)
                    && if Set.null parentColumns
                        then primary
                        else parentColumns == targetColumns
        dependentKeys =
            Set.toList . Set.fromList $
                [ foreignKey
                | droppedKey <- matching
                , Just droppedState <- [Map.lookup droppedKey (catalogConstraints catalog)]
                , Just target <- [dropTarget droppedState]
                , not (Set.null (snd target))
                , (foreignKey, _, keyValue) <- allForeignKeys catalog
                , foreignKey `notElem` matching
                , dependsOn target keyValue
                ]
        removedKeys = matching ++ dependentKeys
        catalog1 = Foldable.foldl' (flip deleteConstraint) catalog removedKeys
     in if not cascade && not (null dependentKeys)
            then (catalog, relation)
            else
                ( catalog1
                , relation
                    { relConstraints =
                        Set.difference (relConstraints relation) (Set.fromList removedKeys)
                    }
                )

applyRenameConstraint ::
    RelationKey ->
    (Catalog, RelationState) ->
    Ident ->
    Ident ->
    (Catalog, RelationState)
applyRenameConstraint key (catalog, relation) from to =
    ( Foldable.foldl' renameOne catalog matching
    , relation
        { relConstraints =
            Set.union newKeys (Set.difference (relConstraints relation) (Set.fromList matching))
        }
    )
    where
        matching =
            [ constraintKey
            | constraintKey <- Set.toList (relConstraints relation)
            , constraintKeyIdentity constraintKey == NamedConstraint from
            ]
        newKeys = Set.fromList [ConstraintKey key (NamedConstraint to) | _ <- matching]
        renameOne catalog' constraintKey
            | Just state <- Map.lookup constraintKey (catalogConstraints catalog') =
                let newKey = ConstraintKey key (NamedConstraint to)
                    newState =
                        state
                            { constraintStateTable = key
                            , constraintValue = renameConstraintName to (constraintValue state)
                            }
                    fks' = case constraintValue newState of
                        ForeignKeyConstraint fk ->
                            Map.insert newKey (newState, fk) (Map.delete constraintKey (catalogForeignKeys catalog'))
                        _ -> catalogForeignKeys catalog'
                 in catalog'
                        { catalogConstraints =
                            Map.insert newKey newState (Map.delete constraintKey (catalogConstraints catalog'))
                        , catalogForeignKeys = fks'
                        }
            | otherwise = catalog'

applyValidateConstraint ::
    (Catalog, RelationState) ->
    Ident ->
    (Catalog, RelationState)
applyValidateConstraint (catalog, relation) name =
    (Foldable.foldl' validateOne catalog matching, relation)
    where
        matching =
            [ constraintKey
            | constraintKey <- Set.toList (relConstraints relation)
            , constraintKeyIdentity constraintKey == NamedConstraint name
            ]
        validateState state =
            state {constraintValue = validateConstraintValue (constraintValue state)}
        validateFk (st, fk) = (validateState st, fk {foreignKeyNotValid = False})
        validateOne current constraintKey =
            current
                { catalogConstraints = Map.adjust validateState constraintKey (catalogConstraints current)
                , catalogForeignKeys = Map.adjust validateFk constraintKey (catalogForeignKeys current)
                }

applyDropIndexChange ::
    (Catalog, RelationState) ->
    Ident ->
    (Catalog, RelationState)
applyDropIndexChange (catalog, relation) name =
    let indexKeys =
            [ indexKey
            | indexKey <- Set.toList (relIndexes relation)
            , indexKeyName indexKey == Just name
            ]
        catalog1 = Foldable.foldl' (flip deleteIndex) catalog indexKeys
     in ( catalog1
        , relation
            { relIndexes =
                Set.difference (relIndexes relation) (Set.fromList indexKeys)
            }
        )

applyAlterColumn :: (Catalog, RelationState) -> Ident -> (Catalog, RelationState)
applyAlterColumn (catalog, relation) column =
    (catalog, relation {relColumns = Set.insert column (relColumns relation)})

-- | Replace a constraint's name while preserving its normalized definition.
renameConstraintName :: Ident -> Constraint -> Constraint
renameConstraintName name constraint =
    case constraint of
        PrimaryKeyConstraint _ columns -> PrimaryKeyConstraint (Just name) columns
        UniqueConstraint _ columns -> UniqueConstraint (Just name) columns
        ForeignKeyConstraint foreignKey ->
            ForeignKeyConstraint foreignKey {foreignKeyName = Just name}
        CheckConstraint _ columns notValid ->
            CheckConstraint (Just name) columns notValid
        IndexConstraint _ columns unique ->
            IndexConstraint (Just name) columns unique
        UsingIndexConstraint _ index primary placement ->
            UsingIndexConstraint (Just name) index primary placement
        OtherConstraint _ -> OtherConstraint (Just name)

-- | Mark a validated foreign key or check constraint valid.
validateConstraintValue :: Constraint -> Constraint
validateConstraintValue constraint =
    case constraint of
        ForeignKeyConstraint foreignKey ->
            ForeignKeyConstraint foreignKey {foreignKeyNotValid = False}
        CheckConstraint name columns _ ->
            CheckConstraint name columns ConstraintValid
        other -> other

-- | Return the explicit name carried by either index-key representation.
indexKeyName :: IndexKey -> Maybe Ident
indexKeyName indexKey =
    case indexKey of
        NamedIndexKey name -> nameLast name
        TableIndexKey _ name -> Just name

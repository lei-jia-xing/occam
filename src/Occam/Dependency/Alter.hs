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

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro ((%~), (&), (.~))
import Occam.Core (
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
    StatementId,
 )
import Occam.Dependency.Barrier (relationTouchKeys)
import Occam.Dependency.Drop (removeRelationCascade)
import Occam.Dependency.Query (
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
    RelationKey,
    RelationState (..),
    SessionState,
    addDeps,
    addTouches,
    deleteConstraint,
    deleteIndex,
    evalCatalogL,
    insertRelation,
    lookupRelation,
    mutateResolved,
    nameLast,
    newRelation,
    relColumnWritesL,
    relColumnsL,
    relConstraintsL,
    relIndexesL,
    relSchemaWritesL,
 )

-- Constraint columns

-- ALTER TABLE
introducedForeignKeysOf :: AlterChange -> [ForeignKey]
introducedForeignKeysOf change =
    case change of
        AddConstraint (ForeignKeyConstraint foreignKey) -> [foreignKey]
        AddColumn column constraints ->
            [foreignKey | ForeignKeyConstraint foreignKey <- constraints]
                ++ columnReferences column
        _ -> []

{- | Views that read the dropped column, directly or through another view,
tracked precisely via column lineage across view layers.
-}
dropColumnDependentViews :: SessionState -> Catalog -> RelationKey -> Ident -> [RelationKey]
dropColumnDependentViews session catalog tableKey column =
    [ viewKey
    | viewKey <- Map.keys (catalogRelations catalog)
    , viewKey /= tableKey
    , viewDependsOnColumn session catalog Set.empty tableKey column viewKey
    ]

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
                        reqs = viewSourceRequirements session catalog relation requestedCols
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

processAlterTable :: StatementId -> Eval -> AlterTable -> Eval
processAlterTable sid eval alter =
    let session = evalSession eval
        catalog = evalCatalog eval
        keys = resolveRelationKeys session catalog (alterTableTarget alter)
        changes = alterTableChanges alter
        introducedForeignKeys =
            concatMap introducedForeignKeysOf changes
        foreignKeyDeps =
            Set.unions
                [ relationReadDeps
                    session
                    catalog
                    parentKey
                    ( if foreignKeyNotValid foreignKey
                        then ReadSchema
                        else ReadData
                    )
                    []
                    False
                | foreignKey <- introducedForeignKeys
                , parentKey <- resolveRelationKeys session catalog (foreignKeyParent foreignKey)
                ]
        stepKey current key =
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
                        , parentKey <-
                            resolveRelationKeys
                                session
                                currentCatalog
                                (foreignKeyParent foreignKey)
                        ]
                dropColumnViewDeps =
                    Set.unions
                        [ Set.unions
                            [ relationAllDeps session currentCatalog viewKey
                                `Set.union` relationConstraintDeps currentCatalog viewRelation
                            | viewKey <- dropColumnDependentViews session currentCatalog key column
                            , Just viewRelation <- [lookupRelation currentCatalog viewKey]
                            ]
                        | DropColumn column _ <- changes
                        ]
                deps =
                    relationAllDeps session currentCatalog key
                        `Set.union` relationConstraintDeps currentCatalog relation
                        `Set.union` foreignKeyDeps
                        `Set.union` validationDeps
                        `Set.union` dropColumnViewDeps
                current1 =
                    addTouches
                        (relationTouchKeys key)
                        (addDeps deps current)
                (catalog1, relation1) =
                    foldl'
                        (applyAlterChange session sid key)
                        (currentCatalog, relation)
                        changes
                relation2 =
                    relation1 & relSchemaWritesL %~ Set.insert sid
             in current1 & evalCatalogL .~ insertRelation key relation2 catalog1
     in mutateResolved stepKey eval keys

{- | `ALTER TABLE parent ATTACH PARTITION child`: route the child's rows
through the parent from here on. The parent records a schema write so
later parent reads depend on the attach.
-}
processAttachPartition :: StatementId -> Eval -> PartitionLink -> Eval
processAttachPartition sid eval link = attachDetachPartition sid eval link True

{- | `ALTER TABLE parent DETACH PARTITION child`: break routing. Earlier
statements keep their snapshot edges; later parent reads exclude the
child but depend on the detach itself through the parent schema write.
-}
processDetachPartition :: StatementId -> Eval -> PartitionLink -> Eval
processDetachPartition sid eval link = attachDetachPartition sid eval link False

attachDetachPartition :: StatementId -> Eval -> PartitionLink -> Bool -> Eval
attachDetachPartition =
    attachDetachLink relPartitionParents (\relation links -> relation {relPartitionParents = links})

{- | `ALTER TABLE child INHERIT parent` and `NO INHERIT`: the inheritance
link set mirrors partition routing but keeps `INSERT` parent-only.
-}
processAttachInherit :: StatementId -> Eval -> PartitionLink -> Eval
processAttachInherit sid eval link = attachDetachInherit sid eval link True

processDetachInherit :: StatementId -> Eval -> PartitionLink -> Eval
processDetachInherit sid eval link = attachDetachInherit sid eval link False

attachDetachInherit :: StatementId -> Eval -> PartitionLink -> Bool -> Eval
attachDetachInherit =
    attachDetachLink relInheritParents (\relation links -> relation {relInheritParents = links})

{- | Shared attach/detach engine for partition and inheritance links. The two
callers differ only in which link set they read and write.
-}
attachDetachLink ::
    (RelationState -> Set NameRef) ->
    (RelationState -> Set NameRef -> RelationState) ->
    StatementId ->
    Eval ->
    PartitionLink ->
    Bool ->
    Eval
attachDetachLink getLinks setLinks sid eval link isAttach =
    let session = evalSession eval
        catalog = evalCatalog eval
        existing reference =
            filter
                (`Map.member` catalogRelations catalog)
                (resolveRelationKeys session catalog reference)
        pairs =
            [ (parentKey, childKey)
            | parentKey <- existing (linkParent link)
            , childKey <- existing (linkChild link)
            ]
        step current (parentKey, childKey) =
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
                updatedLinks =
                    if isAttach
                        then Set.insert (linkParent link) (getLinks child)
                        else
                            Set.filter
                                ( \reference ->
                                    parentKey
                                        `notElem` resolveRelationKeys session currentCatalog reference
                                )
                                (getLinks child)
                parent1 = parent & relSchemaWritesL %~ Set.insert sid
                catalog1 =
                    insertRelation
                        childKey
                        (setLinks child updatedLinks)
                        (insertRelation parentKey parent1 currentCatalog)
             in current1 & evalCatalogL .~ catalog1
     in mutateResolved step eval pairs

applyAlterChange ::
    SessionState ->
    StatementId ->
    RelationKey ->
    (Catalog, RelationState) ->
    AlterChange ->
    (Catalog, RelationState)
applyAlterChange session sid key (catalog, relation) change =
    case change of
        AddColumn column constraints ->
            let allConstraints =
                    constraints
                        ++ [ForeignKeyConstraint foreignKey | foreignKey <- columnReferences column]
                (catalog1, keys) = installConstraints session sid key allConstraints catalog
             in ( catalog1
                , relation
                    & relColumnsL %~ Set.insert (columnName column)
                    & relConstraintsL %~ Set.union keys
                )
        DropColumn column cascade ->
            let isParentColumn foreignKey =
                    column `elem` referencedParentColumns session catalog foreignKey
                        && key
                            `elem` resolveRelationKeys
                                session
                                catalog
                                (foreignKeyParent foreignKey)
                -- Indexes and constraints inside this table are always dropped
                -- with the column, even without CASCADE.
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
                -- Foreign keys in other tables that reference this column as
                -- a parent column are external dependents. Without CASCADE the
                -- server rejects the drop, so the whole catalog must stay
                -- unchanged; with CASCADE the constraints are removed.
                crossTableKeys =
                    [ constraintKey
                    | (constraintKey, state) <- Map.toList (catalogConstraints catalog)
                    , constraintStateTable state /= key
                    , ForeignKeyConstraint foreignKey <- [constraintValue state]
                    , isParentColumn foreignKey
                    ]
                -- A self-referential foreign key still makes the dropped
                -- column a referenced parent column. It must block a RESTRICT
                -- drop just like a foreign key from another table.
                selfParentKeys =
                    [ constraintKey
                    | (constraintKey, state) <- Map.toList (catalogConstraints catalog)
                    , constraintStateTable state == key
                    , ForeignKeyConstraint foreignKey <- [constraintValue state]
                    , isParentColumn foreignKey
                    ]
                -- Views retain the columns they read from each source, so a
                -- view that only uses other columns does not block the drop.
                -- A source with no retained read information is treated as
                -- dependent, which is the sound over-approximation.
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
                            foldl'
                                (flip (removeRelationCascade session sid DropCascade))
                                catalog
                                dependentViews
                        else catalog
             in if not cascade && hasExternalDependents
                    then (catalog, relation)
                    else
                        ( foldl'
                            (flip deleteIndex)
                            (foldl' (flip deleteConstraint) catalog1 removedKeys)
                            sameTableIndexes
                        , relation
                            & relColumnsL %~ Set.delete column
                            & relColumnWritesL %~ Map.delete column
                            & relConstraintsL %~ (`Set.difference` Set.fromList removedKeys)
                            & relIndexesL %~ (`Set.difference` Set.fromList sameTableIndexes)
                        )
        AddConstraint constraint ->
            let (catalog1, keys) = installConstraints session sid key [constraint] catalog
             in ( catalog1
                , relation & relConstraintsL %~ Set.union keys
                )
        DropConstraint name cascade ->
            let matching =
                    [ constraintKey
                    | constraintKey <- Set.toList (relConstraints relation)
                    , constraintKeyIdentity constraintKey == NamedConstraint name
                    ]
                -- Only primary-key and unique constraints can be referenced by
                -- a foreign key. The referenced column set decides whether a
                -- given foreign key depends on the dropped constraint; an
                -- omitted parent column list means the parent primary key.
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
                        , (foreignKey, state) <- Map.toList (catalogConstraints catalog)
                        , foreignKey `notElem` matching
                        , ForeignKeyConstraint keyValue <- [constraintValue state]
                        , dependsOn target keyValue
                        ]
                removedKeys = matching ++ dependentKeys
                catalog1 = foldl' (flip deleteConstraint) catalog removedKeys
             in if not cascade && not (null dependentKeys)
                    then (catalog, relation)
                    else
                        ( catalog1
                        , relation
                            { relConstraints =
                                Set.difference (relConstraints relation) (Set.fromList removedKeys)
                            }
                        )
        RenameConstraint from to ->
            let matching =
                    [ constraintKey
                    | constraintKey <- Set.toList (relConstraints relation)
                    , constraintKeyIdentity constraintKey == NamedConstraint from
                    ]
                renameOne catalog' constraintKey =
                    case Map.lookup constraintKey (catalogConstraints catalog') of
                        Nothing -> catalog'
                        Just state ->
                            let newKey = ConstraintKey key (NamedConstraint to)
                                newState =
                                    state
                                        { constraintStateTable = key
                                        , constraintValue = renameConstraintName to (constraintValue state)
                                        }
                             in catalog'
                                    { catalogConstraints =
                                        Map.insert
                                            newKey
                                            newState
                                            (Map.delete constraintKey (catalogConstraints catalog'))
                                    }
                catalog1 = foldl' renameOne catalog matching
                newKeys = Set.fromList [ConstraintKey key (NamedConstraint to) | _ <- matching]
             in ( catalog1
                , relation
                    { relConstraints =
                        Set.union
                            newKeys
                            (Set.difference (relConstraints relation) (Set.fromList matching))
                    }
                )
        ValidateConstraint name ->
            let matching =
                    [ constraintKey
                    | constraintKey <- Set.toList (relConstraints relation)
                    , constraintKeyIdentity constraintKey == NamedConstraint name
                    ]
                catalog1 =
                    foldl'
                        ( \current constraintKey ->
                            current
                                { catalogConstraints =
                                    Map.adjust
                                        ( \state ->
                                            state
                                                { constraintValue =
                                                    validateConstraintValue (constraintValue state)
                                                }
                                        )
                                        constraintKey
                                        (catalogConstraints current)
                                }
                        )
                        catalog
                        matching
             in (catalog1, relation)
        DropIndexChange name ->
            let indexKeys =
                    [ indexKey
                    | indexKey <- Set.toList (relIndexes relation)
                    , indexKeyName indexKey == Just name
                    ]
                catalog1 = foldl' (flip deleteIndex) catalog indexKeys
             in ( catalog1
                , relation
                    { relIndexes =
                        Set.difference (relIndexes relation) (Set.fromList indexKeys)
                    }
                )
        AlterColumn column _Detail _Referenced _Repositions ->
            (catalog, relation {relColumns = Set.insert column (relColumns relation)})
        OtherChange _ -> (catalog, relation)

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

{- | Clear @NOT VALID@ after a successful @VALIDATE CONSTRAINT@. Only foreign
key and check constraints carry that flag.
-}
validateConstraintValue :: Constraint -> Constraint
validateConstraintValue constraint =
    case constraint of
        ForeignKeyConstraint foreignKey ->
            ForeignKeyConstraint foreignKey {foreignKeyNotValid = False}
        CheckConstraint name columns _ ->
            CheckConstraint name columns ConstraintValid
        other -> other

indexKeyName :: IndexKey -> Maybe Ident
indexKeyName indexKey =
    case indexKey of
        NamedIndexKey name -> nameLast name
        TableIndexKey _ name -> Just name

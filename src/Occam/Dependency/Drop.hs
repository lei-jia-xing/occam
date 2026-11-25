-- | DROP: objects, relation cascades, and schema contents.
module Occam.Dependency.Drop (
    processDrop,
    markResolutionChange,
    dropOne,
    removeRelationCascade,
    SchemaContents (..),
    schemaContents,
    schemaContentsEmpty,
    schemaContentsDeps,
    removeSchemaCascade,
    indexKeyInSchema,
    triggerKeyInSchema,
    schemaContains,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro ((%~), (&), (.~))
import Occam.Core (
    DropMode (..),
    DropObject (..),
    DropOp (..),
    ForeignKey (..),
    IndexRef,
    Name (..),
    NameRef,
    RelationKind (..),
    StatementId,
    TriggerRef,
 )
import Occam.Dependency.Barrier (
    indexTouchKeys,
    relationTouchKeys,
    schemaTouchKeys,
    sequenceTouchKeys,
    triggerTouchKeys,
    typeTouchKeys,
 )
import Occam.Dependency.Query (allForeignKeys, relationAllDeps, relationConstraintDeps)
import Occam.Dependency.Resolve (
    qualifyObjectName,
    relationFallbackKey,
    resolveIndexKeys,
    resolveRelationKeys,
    resolveSequenceKeys,
    resolveTriggerKeys,
 )
import Occam.Dependency.State (
    BarrierKey (..),
    Catalog (..),
    ConstraintKey,
    ConstraintState (..),
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
    addDeps,
    addTouch,
    addTouches,
    catalogRelationsL,
    catalogSchemasL,
    catalogTypesL,
    clearRelationObjects,
    deleteConstraint,
    deleteIndex,
    deleteSequence,
    deleteTrigger,
    evalCatalogL,
    lookupRelation,
    modifyRelation,
    mutateResolved,
    nameLast,
    nameSuffix,
    newRelation,
    relIndexesL,
    relSchemaWritesL,
    relTriggersL,
 )

processDrop :: StatementId -> Eval -> DropOp -> Eval
processDrop sid eval dropOp =
    foldl' (dropOne sid (dropMode dropOp)) eval (dropObjects dropOp)

-- | Statement dependencies recorded by a definition and its last rollback.
definitionDeps :: Maybe StatementId -> Maybe StatementId -> Set StatementId
definitionDeps definition restoredBy =
    Set.fromList (catMaybes [definition, restoredBy])

definitionStateDeps :: DefinitionState -> Set StatementId
definitionStateDeps state =
    definitionDeps (definitionStatement state) (definitionRestoredBy state)

-- | Record a schema write on relations whose unqualified resolution might be affected by the drop.
markResolutionChange :: StatementId -> RelationKey -> Catalog -> Catalog
markResolutionChange sid dropped catalog =
    catalog & catalogRelationsL %~ Map.mapWithKey adjust
    where
        droppedName = relationKeyName dropped
        adjust key relation
            | key /= dropped
            , nameLast droppedName == nameLast (relationKeyName key)
                || nameSuffix droppedName (relationKeyName key)
                || nameSuffix (relationKeyName key) droppedName =
                relation & relSchemaWritesL %~ Set.insert sid
            | otherwise = relation

dropOne :: StatementId -> DropMode -> Eval -> DropObject -> Eval
dropOne sid mode eval (DropRelation reference _kind) =
    dropRelation sid mode eval reference
dropOne _ _ eval (DropIndex reference) =
    dropIndex eval reference
dropOne _ _ eval (DropTrigger reference) =
    dropTrigger eval reference
dropOne _ _ eval (DropSequence reference) =
    dropSequence eval reference
dropOne _ _ eval (DropType name) =
    dropType eval name
dropOne sid mode eval (DropSchema name) =
    dropSchema sid mode eval name

dropRelation :: StatementId -> DropMode -> Eval -> NameRef -> Eval
dropRelation sid mode eval reference
    | null existing = addTouch (RelationBarrierKey (relationFallbackKey session reference)) eval
    | otherwise = mutateResolved stepKey eval existing
    where
        session = evalSession eval
        catalog = evalCatalog eval
        keys = resolveRelationKeys session catalog reference
        existing = filter (`Map.member` catalogRelations catalog) keys
        stepKey current key =
            let currentCatalog = evalCatalog current
                relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog key)
                deps =
                    relationAllDeps session currentCatalog key
                        `Set.union` relationConstraintDeps currentCatalog relation
             in current
                    & addTouches (relationTouchKeys key)
                    & addDeps deps
                    & evalCatalogL %~ (removeRelationCascade session mode key . markResolutionChange sid key)

dropIndex :: Eval -> IndexRef -> Eval
dropIndex eval reference = mutateResolved stepKey eval keys
    where
        session = evalSession eval
        keys = resolveIndexKeys session (evalCatalog eval) reference
        stepKey current key
            | Just state <- Map.lookup key (catalogIndexes (evalCatalog current)) =
                let currentCatalog = evalCatalog current
                    tableDeps = maybe Set.empty (relationAllDeps session currentCatalog) (indexStateTable state)
                    deps = definitionDeps (indexDefinition state) (indexRestoredBy state) `Set.union` tableDeps
                    catalog1 = deleteIndex key currentCatalog
                    catalog2 =
                        maybe
                            catalog1
                            (\tableKey -> modifyRelation tableKey (relIndexesL %~ Set.delete key) catalog1)
                            (indexStateTable state)
                 in current
                        & addTouches (indexTouchKeys key (indexStateTable state))
                        & addDeps deps
                        & evalCatalogL .~ catalog2
            | otherwise = current

dropTrigger :: Eval -> TriggerRef -> Eval
dropTrigger eval reference = mutateResolved stepKey eval keys
    where
        session = evalSession eval
        keys = resolveTriggerKeys session (evalCatalog eval) reference
        stepKey current key
            | Just state <- Map.lookup key (catalogTriggers (evalCatalog current)) =
                let currentCatalog = evalCatalog current
                    tableDeps = maybe Set.empty (relationAllDeps session currentCatalog) (triggerStateTable state)
                    deps = definitionDeps (triggerDefinition state) (triggerRestoredBy state) `Set.union` tableDeps
                    catalog1 = deleteTrigger key currentCatalog
                    catalog2 =
                        maybe
                            catalog1
                            (\tableKey -> modifyRelation tableKey (relTriggersL %~ Set.delete key) catalog1)
                            (triggerStateTable state)
                 in current
                        & addTouches (triggerTouchKeys key (triggerStateTable state))
                        & addDeps deps
                        & evalCatalogL .~ catalog2
            | otherwise = current

dropSequence :: Eval -> NameRef -> Eval
dropSequence eval reference = mutateResolved stepKey eval keys
    where
        keys = resolveSequenceKeys (evalSession eval) (evalCatalog eval) reference
        stepKey current key
            | Just state <- Map.lookup key (catalogSequences (evalCatalog current)) =
                let deps =
                        definitionDeps (sequenceDefinition state) (sequenceRestoredBy state)
                            `Set.union` sequenceWrites state
                 in current
                        & addTouches (sequenceTouchKeys key)
                        & addDeps deps
                        & evalCatalogL %~ deleteSequence key
            | otherwise = current

dropType :: Eval -> Name -> Eval
dropType eval name =
    eval
        & addTouches (typeTouchKeys key)
        & addDeps deps
        & evalCatalogL . catalogTypesL %~ Map.delete key
    where
        catalog = evalCatalog eval
        key = qualifyObjectName (evalSession eval) name
        deps = maybe Set.empty definitionStateDeps (Map.lookup key (catalogTypes catalog))

dropSchema :: StatementId -> DropMode -> Eval -> Name -> Eval
dropSchema sid mode eval key =
    eval
        & addTouches (schemaTouchKeys key)
        & addDeps deps
        & evalCatalogL .~ catalog1
    where
        catalog = evalCatalog eval
        session = evalSession eval
        -- Schema names are not resolved through search_path.
        contents = schemaContents catalog key
        schemaDeps = maybe Set.empty definitionStateDeps (Map.lookup key (catalogSchemas catalog))
        deps = schemaDeps `Set.union` schemaContentsDeps session catalog contents
        catalog1 = case mode of
            DropCascade ->
                foldl'
                    (flip (markResolutionChange sid))
                    (removeSchemaCascade session key catalog)
                    (contentsRelations contents)
            DropRestrict
                -- A non-empty schema makes the server reject the drop, so the catalog must stay unchanged.
                | schemaContentsEmpty contents -> catalog & catalogSchemasL %~ Map.delete key
                | otherwise -> catalog

removeRelationCascade :: SessionState -> DropMode -> RelationKey -> Catalog -> Catalog
removeRelationCascade session mode = go Set.empty
    where
        go seen currentKey catalog
            | Set.member currentKey seen = catalog
            | DropRestrict <- mode
            , not (null dependentViews) || not (null foreignKeys) =
                -- The server rejects the drop while dependents exist.
                catalog
            | DropRestrict <- mode =
                clearRelationObjects currentKey catalog
            | otherwise =
                let catalog1 = foldl' (flip (go seen')) catalog dependentViews
                    catalog2 = foldl' (flip deleteConstraint) catalog1 foreignKeys
                 in clearRelationObjects currentKey catalog2
            where
                seen' = Set.insert currentKey seen
                dependentViews = dependentViewKeys session catalog currentKey
                foreignKeys = dependentForeignKeys session catalog currentKey

dependentViewKeys :: SessionState -> Catalog -> RelationKey -> [RelationKey]
dependentViewKeys session catalog currentKey =
    [ viewKey
    | (viewKey, relation) <- Map.toList (catalogRelations catalog)
    , viewKey /= currentKey
    , relKind relation `elem` [ViewRelation, MaterializedViewRelation]
    , currentKey `Set.member` relSources relation
        || any
            (elem currentKey . resolveRelationKeys session catalog)
            (Set.toList (relUnresolvedSources relation))
    ]

dependentForeignKeys :: SessionState -> Catalog -> RelationKey -> [ConstraintKey]
dependentForeignKeys session catalog currentKey =
    [ constraintKey
    | (constraintKey, state, foreignKey) <- allForeignKeys catalog
    , constraintStateTable state /= currentKey
    , currentKey `elem` resolveRelationKeys session catalog (foreignKeyParent foreignKey)
    ]

data SchemaContents = SchemaContents
    { contentsRelations :: ![RelationKey]
    , contentsSequences :: ![SequenceKey]
    , contentsIndexes :: ![IndexKey]
    , contentsTriggers :: ![TriggerKey]
    , contentsTypes :: ![Name]
    , contentsSchemas :: ![Name]
    }

schemaContents :: Catalog -> Name -> SchemaContents
schemaContents catalog schema =
    SchemaContents
        { contentsRelations =
            [ key
            | key <- Map.keys (catalogRelations catalog)
            , schemaContains schema (relationKeyName key)
            ]
        , contentsSequences =
            [ key
            | key <- Map.keys (catalogSequences catalog)
            , schemaContains schema (sequenceKeyName key)
            ]
        , contentsIndexes =
            [ key
            | key <- Map.keys (catalogIndexes catalog)
            , indexKeyInSchema schema key
            ]
        , contentsTriggers =
            [ key
            | key <- Map.keys (catalogTriggers catalog)
            , triggerKeyInSchema schema key
            ]
        , contentsTypes =
            [ name
            | name <- Map.keys (catalogTypes catalog)
            , schemaContains schema name
            ]
        , contentsSchemas =
            [ name
            | name <- Map.keys (catalogSchemas catalog)
            , schemaContains schema name
            ]
        }

schemaContentsEmpty :: SchemaContents -> Bool
schemaContentsEmpty contents =
    null (contentsRelations contents)
        && null (contentsSequences contents)
        && null (contentsIndexes contents)
        && null (contentsTriggers contents)
        && null (contentsTypes contents)
        && null (contentsSchemas contents)

schemaContentsDeps :: SessionState -> Catalog -> SchemaContents -> Set StatementId
schemaContentsDeps session catalog contents =
    Set.unions
        [ relationAllDeps session catalog relationKey
            `Set.union` relationConstraintDeps catalog relation
        | relationKey <- contentsRelations contents
        , Just relation <- [lookupRelation catalog relationKey]
        ]
        `Set.union` Set.unions
            [ definitionDeps (sequenceDefinition state) (sequenceRestoredBy state)
                `Set.union` sequenceWrites state
            | sequenceKey <- contentsSequences contents
            , Just state <- [Map.lookup sequenceKey (catalogSequences catalog)]
            ]
        `Set.union` Set.unions
            [ definitionDeps (indexDefinition state) (indexRestoredBy state)
                `Set.union` maybe
                    Set.empty
                    (relationAllDeps session catalog)
                    (indexStateTable state)
            | indexKey <- contentsIndexes contents
            , Just state <- [Map.lookup indexKey (catalogIndexes catalog)]
            ]
        `Set.union` Set.unions
            [ definitionDeps (triggerDefinition state) (triggerRestoredBy state)
                `Set.union` maybe
                    Set.empty
                    (relationAllDeps session catalog)
                    (triggerStateTable state)
            | triggerKey <- contentsTriggers contents
            , Just state <- [Map.lookup triggerKey (catalogTriggers catalog)]
            ]
        `Set.union` Set.unions
            [ definitionStateDeps state
            | typeName <- contentsTypes contents
            , Just state <- [Map.lookup typeName (catalogTypes catalog)]
            ]
        `Set.union` Set.unions
            [ definitionStateDeps state
            | schemaName <- contentsSchemas contents
            , Just state <- [Map.lookup schemaName (catalogSchemas catalog)]
            ]

removeSchemaCascade :: SessionState -> Name -> Catalog -> Catalog
removeSchemaCascade session schema catalog =
    let contents = schemaContents catalog schema
        catalog1 =
            foldl'
                (flip (removeRelationCascade session DropCascade))
                catalog
                (contentsRelations contents)
        catalog2 = foldl' (flip deleteSequence) catalog1 (contentsSequences contents)
        catalog3 = foldl' (flip deleteIndex) catalog2 (contentsIndexes contents)
        catalog4 = foldl' (flip deleteTrigger) catalog3 (contentsTriggers contents)
     in catalog4
            & catalogTypesL %~ (\types -> foldl' (flip Map.delete) types (contentsTypes contents))
            & catalogSchemasL %~ (\schemas -> foldl' (flip Map.delete) schemas (contentsSchemas contents))

indexKeyInSchema :: Name -> IndexKey -> Bool
indexKeyInSchema schema key =
    case key of
        NamedIndexKey name -> schemaContains schema name
        TableIndexKey relation _ -> schemaContains schema (relationKeyName relation)

triggerKeyInSchema :: Name -> TriggerKey -> Bool
triggerKeyInSchema schema key =
    case key of
        NamedTriggerKey name -> schemaContains schema name
        TableTriggerKey relation _ -> schemaContains schema (relationKeyName relation)

schemaContains :: Name -> Name -> Bool
schemaContains (Name schemaParts) (Name objectParts) =
    take (length schemaParts) objectParts == schemaParts

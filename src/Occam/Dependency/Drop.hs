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

import Control.Monad.Trans.State.Strict (State, get, modify')
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core.Schema (
    DropMode (..),
    DropObject (..),
    DropOp (..),
    ForeignKey (..),
    IndexRef,
    Name (..),
    NameRef,
    RelationKind (..),
    TriggerRef,
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
    SessionState (sessViewsBoundToRelations),
    TriggerKey (..),
    TriggerState (..),
    addDeps,
    addTouch,
    addTouches,
    ambiguousMutation,
    clearRelationObjects,
    deleteConstraint,
    deleteIndex,
    deleteSequence,
    deleteTrigger,
    lookupRelation,
    modifyRelation,
    nameLast,
    nameSuffix,
    newRelation,
 )

-- | Apply a DROP operation to each object in source order.
processDrop :: StatementId -> DropOp -> State Eval ()
processDrop sid dropOp =
    Foldable.foldlM
        (\() object -> dropOne sid (dropMode dropOp) object)
        ()
        (dropObjects dropOp)

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
    catalog
        { catalogRelations = Map.mapWithKey adjust (catalogRelations catalog)
        }
    where
        droppedName = relationKeyName dropped
        adjust key relation
            | key /= dropped
            , nameLast droppedName == nameLast (relationKeyName key)
                || nameSuffix droppedName (relationKeyName key)
                || nameSuffix (relationKeyName key) droppedName =
                relation {relSchemaWrites = Set.insert sid (relSchemaWrites relation)}
            | otherwise = relation

-- | Apply one resolved DROP object, respecting its dependency mode.
dropOne :: StatementId -> DropMode -> DropObject -> State Eval ()
dropOne sid mode object =
    case object of
        DropRelation reference _kind -> dropRelation sid mode reference
        DropIndex reference -> dropIndex reference
        DropTrigger reference -> dropTrigger reference
        DropSequence reference -> dropSequence reference
        DropType name -> dropType name
        DropSchema name -> dropSchema sid mode name

dropRelation :: StatementId -> DropMode -> NameRef -> State Eval ()
dropRelation sid mode reference = do
    eval <- get
    let session = evalSession eval
        catalog = evalCatalog eval
        keys = resolveRelationKeys session catalog reference
        existing = filter (`Map.member` catalogRelations catalog) keys
    case existing of
        [] -> modify' (addTouch (RelationBarrierKey (relationFallbackKey session reference)))
        _ : _ : _ -> modify' ambiguousMutation
        [key] ->
            modify' $ \current ->
                let currentCatalog = evalCatalog current
                    relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog key)
                    deps =
                        relationAllDeps session currentCatalog key
                            `Set.union` relationConstraintDeps currentCatalog relation
                    catalog1 = removeRelationCascade session mode key (markResolutionChange sid key currentCatalog)
                 in (addTouches (relationTouchKeys key) (addDeps deps current))
                        { evalCatalog = catalog1
                        }

dropIndex :: IndexRef -> State Eval ()
dropIndex reference = do
    eval <- get
    let session = evalSession eval
        keys = resolveIndexKeys session (evalCatalog eval) reference
    case keys of
        _ : _ : _ -> modify' ambiguousMutation
        [] -> pure ()
        [key] ->
            modify' $ \current ->
                case Map.lookup key (catalogIndexes (evalCatalog current)) of
                    Nothing -> current
                    Just state ->
                        let currentCatalog = evalCatalog current
                            tableDeps = maybe Set.empty (relationAllDeps session currentCatalog) (indexStateTable state)
                            deps = definitionDeps (indexDefinition state) (indexRestoredBy state) `Set.union` tableDeps
                            catalog1 = deleteIndex key currentCatalog
                            catalog2 =
                                maybe
                                    catalog1
                                    ( \tableKey ->
                                        modifyRelation
                                            tableKey
                                            (\relation -> relation {relIndexes = Set.delete key (relIndexes relation)})
                                            catalog1
                                    )
                                    (indexStateTable state)
                         in (addTouches (indexTouchKeys key (indexStateTable state)) (addDeps deps current))
                                { evalCatalog = catalog2
                                }

dropTrigger :: TriggerRef -> State Eval ()
dropTrigger reference = do
    eval <- get
    let session = evalSession eval
        keys = resolveTriggerKeys session (evalCatalog eval) reference
    case keys of
        _ : _ : _ -> modify' ambiguousMutation
        [] -> pure ()
        [key] ->
            modify' $ \current ->
                case Map.lookup key (catalogTriggers (evalCatalog current)) of
                    Nothing -> current
                    Just state ->
                        let currentCatalog = evalCatalog current
                            tableDeps = maybe Set.empty (relationAllDeps session currentCatalog) (triggerStateTable state)
                            deps = definitionDeps (triggerDefinition state) (triggerRestoredBy state) `Set.union` tableDeps
                            catalog1 = deleteTrigger key currentCatalog
                            catalog2 =
                                maybe
                                    catalog1
                                    ( \tableKey ->
                                        modifyRelation
                                            tableKey
                                            (\relation -> relation {relTriggers = Set.delete key (relTriggers relation)})
                                            catalog1
                                    )
                                    (triggerStateTable state)
                         in (addTouches (triggerTouchKeys key (triggerStateTable state)) (addDeps deps current))
                                { evalCatalog = catalog2
                                }

dropSequence :: NameRef -> State Eval ()
dropSequence reference =
    modify' $ \eval ->
        let catalog = evalCatalog eval
            keys = resolveSequenceKeys (evalSession eval) catalog reference
         in case keys of
                _ : _ : _ -> ambiguousMutation eval
                [] -> eval
                [key] ->
                    case Map.lookup key (catalogSequences catalog) of
                        Nothing -> eval
                        Just state ->
                            let deps =
                                    definitionDeps (sequenceDefinition state) (sequenceRestoredBy state)
                                        `Set.union` sequenceWrites state
                             in (addTouches (sequenceTouchKeys key) (addDeps deps eval))
                                    { evalCatalog = deleteSequence key catalog
                                    }

dropType :: Name -> State Eval ()
dropType name =
    modify' $ \eval ->
        let catalog = evalCatalog eval
            key = qualifyObjectName (evalSession eval) name
            deps = maybe Set.empty definitionStateDeps (Map.lookup key (catalogTypes catalog))
            eval1 = addTouches (typeTouchKeys key) (addDeps deps eval)
         in eval1 {evalCatalog = catalog {catalogTypes = Map.delete key (catalogTypes catalog)}}

dropSchema :: StatementId -> DropMode -> Name -> State Eval ()
dropSchema sid mode key =
    modify' $ \eval ->
        let catalog = evalCatalog eval
            session = evalSession eval
            -- Schema names are not resolved through search_path.
            contents = schemaContents catalog key
            schemaDeps = maybe Set.empty definitionStateDeps (Map.lookup key (catalogSchemas catalog))
            deps = schemaDeps `Set.union` schemaContentsDeps session catalog contents
            catalog1 = case mode of
                DropCascade ->
                    Foldable.foldl'
                        (flip (markResolutionChange sid))
                        (removeSchemaCascade session key catalog)
                        (contentsRelations contents)
                DropRestrict
                    -- A non-empty schema makes the server reject the drop, so the catalog must stay unchanged.
                    | schemaContentsEmpty contents ->
                        catalog {catalogSchemas = Map.delete key (catalogSchemas catalog)}
                    | otherwise -> catalog
            eval1 = addTouches (schemaTouchKeys key) (addDeps deps eval)
         in eval1 {evalCatalog = catalog1}

-- | Remove a relation and dependent objects, retaining them when RESTRICT rejects the drop.
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
                let catalog1 = Foldable.foldl' (flip (go seen')) catalog dependentViews
                    catalog2 = Foldable.foldl' (flip deleteConstraint) catalog1 foreignKeys
                 in clearRelationObjects currentKey catalog2
            where
                seen' = Set.insert currentKey seen
                dependentViews
                    | sessViewsBoundToRelations session = dependentViewKeys session catalog currentKey
                    | otherwise = []
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

-- | Catalog objects selected by a schema drop, grouped by object kind.
data SchemaContents = SchemaContents
    { contentsRelations :: ![RelationKey]
    , contentsSequences :: ![SequenceKey]
    , contentsIndexes :: ![IndexKey]
    , contentsTriggers :: ![TriggerKey]
    , contentsTypes :: ![Name]
    , contentsSchemas :: ![Name]
    }

-- | Collect tracked catalog objects whose names lie within the schema prefix.
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

-- | True when a schema-contents snapshot contains no tracked objects.
schemaContentsEmpty :: SchemaContents -> Bool
schemaContentsEmpty contents =
    null (contentsRelations contents)
        && null (contentsSequences contents)
        && null (contentsIndexes contents)
        && null (contentsTriggers contents)
        && null (contentsTypes contents)
        && null (contentsSchemas contents)

-- | Collect definition, write, and relation dependencies of schema contents.
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

-- | Remove all tracked objects under a schema, including dependent relations.
removeSchemaCascade :: SessionState -> Name -> Catalog -> Catalog
removeSchemaCascade session schema catalog =
    let contents = schemaContents catalog schema
        catalog1 =
            Foldable.foldl'
                (flip (removeRelationCascade session DropCascade))
                catalog
                (contentsRelations contents)
        catalog2 = Foldable.foldl' (flip deleteSequence) catalog1 (contentsSequences contents)
        catalog3 = Foldable.foldl' (flip deleteIndex) catalog2 (contentsIndexes contents)
        catalog4 = Foldable.foldl' (flip deleteTrigger) catalog3 (contentsTriggers contents)
     in catalog4
            { catalogTypes =
                Foldable.foldl' (flip Map.delete) (catalogTypes catalog4) (contentsTypes contents)
            , catalogSchemas =
                Foldable.foldl' (flip Map.delete) (catalogSchemas catalog4) (contentsSchemas contents)
            }

-- | Test whether an index key's owning name or table belongs to a schema prefix.
indexKeyInSchema :: Name -> IndexKey -> Bool
indexKeyInSchema schema key =
    case key of
        NamedIndexKey name -> schemaContains schema name
        TableIndexKey relation _ -> schemaContains schema (relationKeyName relation)

-- | Test whether a trigger key's owning name or table belongs to a schema prefix.
triggerKeyInSchema :: Name -> TriggerKey -> Bool
triggerKeyInSchema schema key =
    case key of
        NamedTriggerKey name -> schemaContains schema name
        TableTriggerKey relation _ -> schemaContains schema (relationKeyName relation)

-- | True when the object name begins with all identifier parts of the schema name.
schemaContains :: Name -> Name -> Bool
schemaContains (Name schemaParts) (Name objectParts) =
    take (length schemaParts) objectParts == schemaParts

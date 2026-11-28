-- | Pure catalog dependency queries shared by catalog-operation handlers.
module Occam.Dependency.Query (
    relationDefinitionDeps,
    relationColumnWriteDeps,
    coupledWriteColumns,
    constraintColumnsOf,
    referencedParentColumns,
    relationIndexDeps,
    relationTriggerDeps,
    childrenOf,
    partitionChildrenOf,
    inheritChildrenOf,
    viewSourceRequirements,
    relationReadDeps,
    relationWriteDeps,
    relationAllDeps,
    allForeignKeys,
    constraintNameIdent,
    installConstraints,
    resolveUsingIndex,
    namespaceDeps,
    schemaPrefixDeps,
    relationConstraintDeps,
) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core.Schema (
    Constraint (..),
    DeleteScope (..),
    ForeignKey (..),
    Ident,
    IndexRef (..),
    IndexRole (..),
    IndexSchemaPlacement (..),
    Name (..),
    NameRef (..),
    ReadFact (..),
    ReadMode (..),
    RelationKind (..),
    SourceColumn (..),
    UpdateScope (..),
    ViewColumnLineage (..),
    WriteKind (..),
 )
import Occam.Core.Types (StatementId)
import Occam.Dependency.Resolve (resolveIndexKeys, resolveRelationKeys, resolveViewSourceKeys)
import Occam.Dependency.State (
    Catalog (..),
    ConstraintIdentity (..),
    ConstraintKey (..),
    ConstraintState (..),
    DefinitionState (..),
    IndexState (..),
    RelationKey (..),
    RelationState (..),
    SessionState,
    TriggerState (..),
    lookupRelation,
    newRelation,
 )

-- Relation dependency queries

-- | Statement IDs that defined, schema-wrote, or restored @key@.
relationDefinitionDeps :: Catalog -> RelationKey -> Set StatementId
relationDefinitionDeps catalog key =
    case lookupRelation catalog key of
        Nothing -> Set.empty
        Just relation ->
            Set.fromList
                ( catMaybes
                    [ relDefinition relation
                    , relRestoredBy relation
                    ]
                )
                `Set.union` relSchemaWrites relation

-- | Dependency edges from writes touching any column in @columns@ (transitive over composite constraints\/FKs).
relationColumnWriteDeps ::
    SessionState -> Catalog -> RelationKey -> RelationState -> [Ident] -> Set StatementId
relationColumnWriteDeps session catalog key relation columns
    | null columns = Set.unions (Map.elems (relColumnWrites relation))
    | otherwise =
        Set.unions
            [ Map.findWithDefault Set.empty column (relColumnWrites relation)
            | column <- Set.toList (coupledWriteColumns session catalog key columns)
            ]

-- | Expand written columns through associated composite keys and constraints.
coupledWriteColumns :: SessionState -> Catalog -> RelationKey -> [Ident] -> Set Ident
coupledWriteColumns _ _ _ [] = Set.empty
coupledWriteColumns session catalog key columns = expand (Set.fromList columns)
    where
        relation = fromMaybe (newRelation TableRelation) (lookupRelation catalog key)
        localSets =
            [ constraintColumnsOf (constraintValue state)
            | constraintKey <- Set.toList (relConstraints relation)
            , Just state <- [Map.lookup constraintKey (catalogConstraints catalog)]
            ]
        candidates = map Set.fromList (localSets ++ coupledForeignKeyColumnSets session catalog key)
        expand current
            | expanded == current = current
            | otherwise = expand expanded
            where
                next =
                    Set.unions
                        [ candidate
                        | candidate <- candidates
                        , not (Set.null (Set.intersection current candidate))
                        ]
                expanded = Set.union current next

coupledForeignKeyColumnSets :: SessionState -> Catalog -> RelationKey -> [[Ident]]
coupledForeignKeyColumnSets session catalog key =
    [ fkCols state foreignKey
    | (_, state, foreignKey) <- allForeignKeys catalog
    , key `elem` resolveRelationKeys session catalog (foreignKeyParent foreignKey)
    ]
    where
        fkCols state foreignKey
            | constraintStateTable state == key =
                foreignKeyChildColumns foreignKey ++ referencedParentColumns session catalog foreignKey
            | otherwise = referencedParentColumns session catalog foreignKey

-- | All local column names explicitly referenced by a 'Constraint'.
constraintColumnsOf :: Constraint -> [Ident]
constraintColumnsOf constraint =
    case constraint of
        PrimaryKeyConstraint _ columns -> columns
        UniqueConstraint _ columns -> columns
        ForeignKeyConstraint foreignKey -> foreignKeyChildColumns foreignKey
        CheckConstraint _ columns _ -> columns
        IndexConstraint _ columns _ -> columns
        UsingIndexConstraint {} -> []
        OtherConstraint _ -> []

-- | Columns referenced in the parent table of a foreign key.
referencedParentColumns :: SessionState -> Catalog -> ForeignKey -> [Ident]
referencedParentColumns session catalog foreignKey
    | not (null (foreignKeyParentColumns foreignKey)) =
        foreignKeyParentColumns foreignKey
    | otherwise =
        concat
            [ keyColumns
            | parentKey <-
                resolveRelationKeys
                    session
                    catalog
                    (foreignKeyParent foreignKey)
            , (_, state) <- Map.toList (catalogConstraints catalog)
            , constraintStateTable state == parentKey
            , PrimaryKeyConstraint _ keyColumns <- [constraintValue state]
            ]

-- | Dependency edges for all indexes currently attached to @relation@.
relationIndexDeps :: Catalog -> RelationState -> Set StatementId
relationIndexDeps catalog relation =
    Set.unions
        [ Set.fromList (catMaybes [indexDefinition index, indexRestoredBy index])
        | key <- Set.toList (relIndexes relation)
        , Just index <- [Map.lookup key (catalogIndexes catalog)]
        ]

-- | Dependency edges for all triggers currently attached to @relation@.
relationTriggerDeps :: Catalog -> RelationState -> Set StatementId
relationTriggerDeps catalog relation =
    Set.unions
        [ Set.fromList (catMaybes [triggerDefinition trigger, triggerRestoredBy trigger])
        | key <- Set.toList (relTriggers relation)
        , Just trigger <- [Map.lookup key (catalogTriggers catalog)]
        ]

-- | Partition and inheritance children of a parent relation.
childrenOf :: SessionState -> Catalog -> RelationKey -> [RelationKey]
childrenOf session catalog key =
    childrenWith
        session
        catalog
        key
        (\relation -> relPartitionParents relation `Set.union` relInheritParents relation)

-- | Partition children only (@PARTITION OF@, @ATTACH PARTITION@).
partitionChildrenOf :: SessionState -> Catalog -> RelationKey -> [RelationKey]
partitionChildrenOf session catalog key =
    childrenWith session catalog key relPartitionParents

-- | Inheritance children only (@INHERITS@).
inheritChildrenOf :: SessionState -> Catalog -> RelationKey -> [RelationKey]
inheritChildrenOf session catalog key =
    childrenWith session catalog key relInheritParents

childrenWith ::
    SessionState -> Catalog -> RelationKey -> (RelationState -> Set.Set NameRef) -> [RelationKey]
childrenWith session catalog key parents
    | Set.null (catalogChildRelations catalog) = []
    | otherwise =
        [ childKey
        | childKey <- Set.toList (catalogChildRelations catalog)
        , childKey /= key
        , Just relation <- [Map.lookup childKey (catalogRelations catalog)]
        , not (Set.null (parents relation))
        , key
            `elem` concatMap
                (resolveRelationKeys session catalog)
                (Set.toList (parents relation))
        ]

viewSourceRequirements ::
    SessionState ->
    Catalog ->
    Bool ->
    RelationState ->
    Maybe (Set Ident) ->
    Map.Map RelationKey (Maybe (Set Ident))
viewSourceRequirements session catalog viewIsTemp relation requestedCols
    | null (relColumnLineage relation) = conservativeViewReqs session catalog viewIsTemp relation
    | otherwise =
        maybe
            (conservativeViewReqs session catalog viewIsTemp relation)
            (buildLineageReqs session catalog viewIsTemp relation . concat)
            (traverse lookupSources cols)
    where
        lineageMap = Map.fromList [(lineageColumn l, l) | l <- relColumnLineage relation]
        cols = maybe (map lineageColumn (relColumnLineage relation)) Set.toList requestedCols
        lookupSources col = Map.lookup col lineageMap >>= lineageSources

combineViewReq :: Maybe (Set Ident) -> Maybe (Set Ident) -> Maybe (Set Ident)
combineViewReq (Just a) (Just b) = Just (a `Set.union` b)
combineViewReq _ _ = Nothing

resolvedViewUnresolvedSources :: SessionState -> Catalog -> Bool -> RelationState -> [RelationKey]
resolvedViewUnresolvedSources session catalog viewIsTemp relation =
    [ sourceKey
    | reference <- Set.toList (relUnresolvedSources relation)
    , sourceKey <- resolveViewSourceKeys session viewIsTemp catalog reference
    , Map.member sourceKey (catalogRelations catalog)
    ]

conservativeViewReqs ::
    SessionState -> Catalog -> Bool -> RelationState -> Map.Map RelationKey (Maybe (Set Ident))
conservativeViewReqs session catalog viewIsTemp relation =
    Set.foldl' insertWildcard fromReads allSourceKeys
    where
        colsOf readFact
            | null (readColumns readFact) = Nothing
            | otherwise = Just (Set.fromList (readColumns readFact))
        fromReads =
            Map.fromListWith
                combineViewReq
                [ (sourceKey, colsOf readFact)
                | readFact <- relSourceReads relation
                , sourceKey <- resolveViewSourceKeys session viewIsTemp catalog (readSource readFact)
                ]
        allSourceKeys =
            relSources relation
                `Set.union` Set.fromList (resolvedViewUnresolvedSources session catalog viewIsTemp relation)
        insertWildcard m k
            | Map.member k m = m
            | otherwise = Map.insert k Nothing m

buildLineageReqs ::
    SessionState ->
    Catalog ->
    Bool ->
    RelationState ->
    [SourceColumn] ->
    Map.Map RelationKey (Maybe (Set Ident))
buildLineageReqs session catalog viewIsTemp relation collectedSCs =
    Foldable.foldl' addSourceCol initMap (collectedSCs ++ relConditionColumns relation)
    where
        baseMap = Map.fromList [(sourceKey, Just Set.empty) | sourceKey <- Set.toList (relSources relation)]
        unresolvedBase =
            Map.fromList
                [ (sourceKey, Just Set.empty)
                | sourceKey <- resolvedViewUnresolvedSources session catalog viewIsTemp relation
                ]
        initMap = baseMap `Map.union` unresolvedBase
        addSourceCol m sc =
            let keys = resolveViewSourceKeys session viewIsTemp catalog (sourceColumnRelation sc)
                col = sourceColumnName sc
             in Foldable.foldl' (\acc k -> Map.insertWith combineViewReq k (Just (Set.singleton col)) acc) m keys

-- | Transitive dependency edges for reading @key@ (including views and child partitions unless @only@).
relationReadDeps ::
    SessionState -> Catalog -> RelationKey -> ReadMode -> [Ident] -> Bool -> Set StatementId
relationReadDeps session catalog key mode columns only =
    go Set.empty key mode initialCols
    where
        initialCols
            | null columns = Nothing
            | otherwise = Just (Set.fromList columns)
        go seen current currentMode currentCols
            | Set.member current seen = Set.empty
            | Just relation <- lookupRelation catalog current =
                let seen' = Set.insert current seen
                    base = relationDefinitionDeps catalog current `Set.union` relationIndexDeps catalog relation
                    dataDeps = readModeDataDeps session catalog current relation currentMode currentCols
                    sourceDeps = viewReadDeps seen' current relation currentCols
                    childDeps = childReadDeps seen' current currentMode currentCols
                 in base `Set.union` dataDeps `Set.union` sourceDeps `Set.union` childDeps
            | otherwise = Set.empty
        -- View sources are evaluated with ReadData for column dependencies.
        viewReadDeps seen' viewKey relation currentCols
            | relKind relation /= ViewRelation = Set.empty
            | otherwise =
                Set.unions
                    [ go seen' sourceKey ReadData neededCols
                    | (sourceKey, neededCols) <-
                        Map.toList (viewSourceRequirements session catalog (relationKeyTemp viewKey) relation currentCols)
                    , Map.member sourceKey (catalogRelations catalog)
                    ]
        -- Child relations contribute rows to parent reads unless ONLY is set.
        childReadDeps seen' current currentMode currentCols
            | only = Set.empty
            | otherwise =
                Set.unions [go seen' child currentMode currentCols | child <- childrenOf session catalog current]

readModeDataDeps ::
    SessionState ->
    Catalog ->
    RelationKey ->
    RelationState ->
    ReadMode ->
    Maybe (Set.Set Ident) ->
    Set.Set StatementId
readModeDataDeps session catalog current relation mode cols = case mode of
    ReadSchema -> Set.empty
    ReadRowCount -> relCardinalityWrites relation
    ReadRowExistence -> relCardinalityWrites relation
    ReadData -> case cols of
        Nothing ->
            relDataWrites relation
                `Set.union` Set.unions (Map.elems (relColumnWrites relation))
        Just cs
            | Set.null cs -> relDataWrites relation
            | otherwise ->
                relDataWrites relation
                    `Set.union` relationColumnWriteDeps session catalog current relation (Set.toList cs)

-- | Transitive dependency edges required to perform a DML write on @key@.
relationWriteDeps ::
    SessionState -> Catalog -> RelationKey -> WriteKind -> [Ident] -> Set StatementId
relationWriteDeps session catalog key kind columns
    | Just relation <- lookupRelation catalog key =
        let base =
                relationDefinitionDeps catalog key
                    `Set.union` relationIndexDeps catalog relation
                    `Set.union` relationTriggerDeps catalog relation
            writes = writeKindDataDeps session catalog key relation kind columns
            sourceDeps = viewWriteSourceDeps session catalog relation
         in base `Set.union` writes `Set.union` sourceDeps
    | otherwise = Set.empty

writeKindDataDeps ::
    SessionState -> Catalog -> RelationKey -> RelationState -> WriteKind -> [Ident] -> Set StatementId
writeKindDataDeps session catalog key relation kind columns = case kind of
    TruncateWrite {} -> Set.empty
    InsertWrite _ -> relCardinalityWrites relation
    DeleteWrite DeleteAllRows -> relCardinalityWrites relation
    DeleteWrite DeleteFilteredRows ->
        relCardinalityWrites relation
            `Set.union` relationColumnWriteDeps session catalog key relation columns
    UpdateWrite UpdateAllRows
        | null columns -> relDataWrites relation
        | otherwise -> relCardinalityWrites relation
    UpdateWrite UpdateFilteredRows ->
        relCardinalityWrites relation
            `Set.union` relationColumnWriteDeps session catalog key relation columns
    MergeWrite ->
        relDataWrites relation `Set.union` relationColumnWriteDeps session catalog key relation columns

viewWriteSourceDeps :: SessionState -> Catalog -> RelationState -> Set StatementId
viewWriteSourceDeps session catalog relation
    | relKind relation /= ViewRelation = Set.empty
    | otherwise = Set.unions (resolvedDeps ++ unresolvedDeps)
    where
        resolvedDeps =
            [ relationReadDeps session catalog source ReadData [] False
            | source <- Set.toList (relSources relation)
            ]
        unresolvedDeps =
            [ relationReadDeps session catalog sourceKey ReadData [] False
            | reference <- Set.toList (relUnresolvedSources relation)
            , sourceKey <- resolveRelationKeys session catalog reference
            , Map.member sourceKey (catalogRelations catalog)
            ]

relationAllDeps :: SessionState -> Catalog -> RelationKey -> Set StatementId
relationAllDeps session catalog key =
    relationReadDeps session catalog key ReadData [] False
        `Set.union` relationWriteDeps session catalog key (DeleteWrite DeleteFilteredRows) []

-- Foreign keys and constraint names
allForeignKeys :: Catalog -> [(ConstraintKey, ConstraintState, ForeignKey)]
allForeignKeys catalog =
    [ (key, state, foreignKey)
    | (key, (state, foreignKey)) <- Map.toList (catalogForeignKeys catalog)
    ]

constraintNameIdent :: Constraint -> Maybe Ident
constraintNameIdent constraint =
    case constraint of
        PrimaryKeyConstraint name _ -> name
        UniqueConstraint name _ -> name
        ForeignKeyConstraint foreignKey -> foreignKeyName foreignKey
        CheckConstraint name _ _ -> name
        IndexConstraint name _ _ -> name
        UsingIndexConstraint name _ _ _ -> name
        OtherConstraint name -> name

-- Constraint installation
installConstraints ::
    SessionState ->
    StatementId ->
    RelationKey ->
    [Constraint] ->
    Catalog ->
    (Catalog, Set ConstraintKey)
installConstraints session sid key constraints catalog =
    let (catalog', keys, _) =
            Foldable.foldl' install (catalog, Set.empty, catalogNextUnnamedConstraint catalog) constraints
     in (catalog', keys)
    where
        install (current, keys, next) constraint =
            let resolved = resolveUsingIndex session current key constraint
                (identity, next') =
                    case constraintNameIdent resolved of
                        Just name -> (NamedConstraint name, next)
                        Nothing -> (UnnamedConstraint sid next, next + 1)
                constraintKey = ConstraintKey key identity
                state = ConstraintState key (Just sid) resolved Nothing
                fks' = case resolved of
                    ForeignKeyConstraint fk -> Map.insert constraintKey (state, fk) (catalogForeignKeys current)
                    _ -> catalogForeignKeys current
             in ( current
                    { catalogConstraints = Map.insert constraintKey state (catalogConstraints current)
                    , catalogForeignKeys = fks'
                    , catalogNextUnnamedConstraint = next'
                    }
                , Set.insert constraintKey keys
                , next'
                )

-- | Resolve @PRIMARY KEY / UNIQUE USING INDEX@ constraints using the index's columns.
resolveUsingIndex :: SessionState -> Catalog -> RelationKey -> Constraint -> Constraint
resolveUsingIndex session catalog tableKey (UsingIndexConstraint name index role placement)
    | null columns = OtherConstraint name
    | role == PrimaryIndex = PrimaryKeyConstraint name columns
    | otherwise = UniqueConstraint name columns
    where
        unqualifiedIndexKeys = resolveIndexKeys session catalog (NamedIndex (Name [index]))
        tableName = relationKeyName tableKey
        -- Resolve unqualified index in table's schema or current schema.
        tableSchemaIndexKeys = case nameParts tableName of
            _schema : _table : _ ->
                resolveIndexKeys session catalog (NamedIndex (Name (init (nameParts tableName) ++ [index])))
            _ -> []
        indexKeys = case placement of
            IndexTableSchema | not (null tableSchemaIndexKeys) -> tableSchemaIndexKeys
            _ -> unqualifiedIndexKeys
        columns =
            concat
                [ indexStateColumns state
                | indexKey <- indexKeys
                , Just state <- [Map.lookup indexKey (catalogIndexes catalog)]
                ]
resolveUsingIndex _ _ _ other = other

-- | Resolve namespace prefix dependencies from catalog schemas.
namespaceDeps :: Catalog -> [Ident] -> Set StatementId
namespaceDeps catalog parts =
    Set.fromList
        [ sid
        | count <- [1 .. length parts]
        , Just state <- [Map.lookup (Name (take count parts)) (catalogSchemas catalog)]
        , sid <- catMaybes [definitionStatement state, definitionRestoredBy state]
        ]

schemaPrefixDeps :: Catalog -> Name -> Set StatementId
schemaPrefixDeps catalog (Name parts) =
    namespaceDeps catalog (take (length parts - 1) parts)

-- Constraint dependencies
relationConstraintDeps :: Catalog -> RelationState -> Set StatementId
relationConstraintDeps catalog relation =
    Set.unions
        [ Set.fromList
            ( catMaybes
                [ constraintDefinition state
                , constraintRestoredBy state
                ]
            )
        | key <- Set.toList (relConstraints relation)
        , Just state <- [Map.lookup key (catalogConstraints catalog)]
        ]

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

import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (
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
    StatementId,
    UpdateScope (..),
    ViewColumnLineage (..),
    WriteKind (..),
 )
import Occam.Dependency.Resolve (resolveIndexKeys, resolveRelationKeys)
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
coupledWriteColumns session catalog key columns
    | null columns = Set.empty
    | otherwise =
        let written = Set.fromList columns
            relation = fromMaybe (newRelation TableRelation) (lookupRelation catalog key)
            fks = allForeignKeys catalog
            fkSets =
                if null fks
                    then []
                    else
                        [ foreignKeyChildColumns foreignKey
                            ++ referencedParentColumns session catalog foreignKey
                        | (_, state, foreignKey) <- fks
                        , constraintStateTable state == key
                        , key
                            `elem` resolveRelationKeys
                                session
                                catalog
                                (foreignKeyParent foreignKey)
                        ]
                            ++ [ referencedParentColumns session catalog foreignKey
                               | (_, state, foreignKey) <- fks
                               , constraintStateTable state /= key
                               , key
                                    `elem` resolveRelationKeys
                                        session
                                        catalog
                                        (foreignKeyParent foreignKey)
                               ]
            localSets =
                [ constraintColumnsOf (constraintValue state)
                | constraintKey <- Set.toList (relConstraints relation)
                , Just state <- [Map.lookup constraintKey (catalogConstraints catalog)]
                ]
                    ++ fkSets
            candidates = map Set.fromList localSets
            expand current =
                let next =
                        Set.unions
                            [ candidate
                            | candidate <- candidates
                            , not (Set.null (Set.intersection current candidate))
                            ]
                    expanded = Set.union current next
                 in if expanded == current then current else expand expanded
         in expand written

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

relationIndexDeps :: Catalog -> RelationState -> Set StatementId
relationIndexDeps catalog relation =
    Set.unions
        [ Set.fromList (catMaybes [indexDefinition index, indexRestoredBy index])
        | key <- Set.toList (relIndexes relation)
        , Just index <- [Map.lookup key (catalogIndexes catalog)]
        ]

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
    RelationState ->
    Maybe (Set Ident) ->
    Map.Map RelationKey (Maybe (Set Ident))
viewSourceRequirements session catalog relation requestedCols =
    if null (relColumnLineage relation)
        then conservativeReqs
        else case requestedCols of
            Nothing ->
                requirementsForColumns (map lineageColumn (relColumnLineage relation))
            Just cols ->
                requirementsForColumns (Set.toList cols)
    where
        lineageMap =
            Map.fromList
                [(lineageColumn l, l) | l <- relColumnLineage relation]

        combineReq Nothing _ = Nothing
        combineReq _ Nothing = Nothing
        combineReq (Just a) (Just b) = Just (a `Set.union` b)

        conservativeReqs =
            let fromReads =
                    Map.fromListWith
                        combineReq
                        [ ( sourceKey
                          , if null (readColumns readFact)
                                then Nothing
                                else Just (Set.fromList (readColumns readFact))
                          )
                        | readFact <- relSourceReads relation
                        , sourceKey <- resolveRelationKeys session catalog (readSource readFact)
                        ]
                allSourceKeys =
                    relSources relation
                        `Set.union` Set.fromList
                            [ sourceKey
                            | reference <- Set.toList (relUnresolvedSources relation)
                            , sourceKey <- resolveRelationKeys session catalog reference
                            , Map.member sourceKey (catalogRelations catalog)
                            ]
             in Set.foldl'
                    (\m k -> if Map.member k m then m else Map.insert k Nothing m)
                    fromReads
                    allSourceKeys

        requirementsForColumns cols =
            let lookupCol col = Map.lookup col lineageMap
                lineages = map lookupCol cols
                hasUnknown = any isUnknown lineages
                isUnknown Nothing = True
                isUnknown (Just l) = isNothing (lineageSources l)
             in if hasUnknown
                    then conservativeReqs
                    else
                        let collectedSCs =
                                concat
                                    [ scs
                                    | Just l <- lineages
                                    , Just scs <- [lineageSources l]
                                    ]
                            allSCs = collectedSCs ++ relConditionColumns relation
                            baseMap =
                                Map.fromList
                                    [ (sourceKey, Just Set.empty)
                                    | sourceKey <- Set.toList (relSources relation)
                                    ]
                            unresolvedBase =
                                Map.fromList
                                    [ (sourceKey, Just Set.empty)
                                    | reference <- Set.toList (relUnresolvedSources relation)
                                    , sourceKey <- resolveRelationKeys session catalog reference
                                    , Map.member sourceKey (catalogRelations catalog)
                                    ]
                            initMap = baseMap `Map.union` unresolvedBase
                         in foldl' addSourceCol initMap allSCs

        addSourceCol m sc =
            let keys = resolveRelationKeys session catalog (sourceColumnRelation sc)
                col = sourceColumnName sc
             in foldl'
                    ( \acc k ->
                        Map.insertWith
                            combineReq
                            k
                            (Just (Set.singleton col))
                            acc
                    )
                    m
                    keys

relationReadDeps ::
    SessionState -> Catalog -> RelationKey -> ReadMode -> [Ident] -> Bool -> Set StatementId
relationReadDeps session catalog key mode columns only =
    go Set.empty key mode initialCols
    where
        initialCols =
            if null columns
                then Nothing
                else Just (Set.fromList columns)
        go seen current currentMode currentCols
            | Set.member current seen = Set.empty
            | otherwise =
                case lookupRelation catalog current of
                    Nothing -> Set.empty
                    Just relation ->
                        let seen' = Set.insert current seen
                            base =
                                relationDefinitionDeps catalog current
                                    `Set.union` relationIndexDeps catalog relation
                            dataDeps =
                                case currentMode of
                                    ReadData ->
                                        case currentCols of
                                            Nothing ->
                                                relDataWrites relation
                                                    `Set.union` Set.unions (Map.elems (relColumnWrites relation))
                                            Just cols
                                                | Set.null cols ->
                                                    relDataWrites relation
                                                | otherwise ->
                                                    relDataWrites relation
                                                        `Set.union` relationColumnWriteDeps
                                                            session
                                                            catalog
                                                            current
                                                            relation
                                                            (Set.toList cols)
                                    ReadRowCount ->
                                        relCardinalityWrites relation
                                    ReadRowExistence ->
                                        relCardinalityWrites relation
                                    ReadSchema -> Set.empty
                            -- View sources are evaluated with ReadData for column dependencies.
                            sourceDeps =
                                if relKind relation == ViewRelation
                                    then
                                        let reqs = viewSourceRequirements session catalog relation currentCols
                                         in Set.unions
                                                [ go seen' sourceKey ReadData neededCols
                                                | (sourceKey, neededCols) <- Map.toList reqs
                                                , Map.member sourceKey (catalogRelations catalog)
                                                ]
                                    else Set.empty
                            -- Child relations contribute rows to parent reads unless ONLY is set.
                            childDeps =
                                if only
                                    then Set.empty
                                    else
                                        Set.unions
                                            [ go seen' child currentMode currentCols
                                            | child <- childrenOf session catalog current
                                            ]
                         in base
                                `Set.union` dataDeps
                                `Set.union` sourceDeps
                                `Set.union` childDeps

relationWriteDeps ::
    SessionState -> Catalog -> RelationKey -> WriteKind -> [Ident] -> Set StatementId
relationWriteDeps session catalog key kind columns =
    case lookupRelation catalog key of
        Nothing -> Set.empty
        Just relation ->
            let base =
                    relationDefinitionDeps catalog key
                        `Set.union` relationIndexDeps catalog relation
                        `Set.union` relationTriggerDeps catalog relation
                writes =
                    case kind of
                        TruncateWrite {} -> Set.empty
                        InsertWrite _ ->
                            relCardinalityWrites relation
                        DeleteWrite DeleteAllRows ->
                            relCardinalityWrites relation
                        DeleteWrite DeleteFilteredRows ->
                            relCardinalityWrites relation
                                `Set.union` relationColumnWriteDeps session catalog key relation columns
                        UpdateWrite UpdateAllRows
                            | null columns ->
                                relDataWrites relation
                            | otherwise ->
                                relCardinalityWrites relation
                        UpdateWrite UpdateFilteredRows ->
                            relCardinalityWrites relation
                                `Set.union` relationColumnWriteDeps session catalog key relation columns
                        MergeWrite ->
                            relDataWrites relation
                                `Set.union` relationColumnWriteDeps session catalog key relation columns
                sourceDeps =
                    Set.unions
                        [ relationReadDeps session catalog source ReadData [] False
                        | relKind relation == ViewRelation
                        , source <- Set.toList (relSources relation)
                        ]
                unresolvedDeps =
                    Set.unions
                        [ relationReadDeps session catalog sourceKey ReadData [] False
                        | relKind relation == ViewRelation
                        , reference <- Set.toList (relUnresolvedSources relation)
                        , sourceKey <- resolveRelationKeys session catalog reference
                        , Map.member sourceKey (catalogRelations catalog)
                        ]
             in base `Set.union` writes `Set.union` sourceDeps `Set.union` unresolvedDeps

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
            foldl' install (catalog, Set.empty, catalogNextUnnamedConstraint catalog) constraints
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
                    { catalogConstraints =
                        Map.insert constraintKey state (catalogConstraints current)
                    , catalogForeignKeys = fks'
                    , catalogNextUnnamedConstraint = next'
                    }
                , Set.insert constraintKey keys
                , next'
                )

-- | Resolve @PRIMARY KEY / UNIQUE USING INDEX@ constraints using the index's columns.
resolveUsingIndex :: SessionState -> Catalog -> RelationKey -> Constraint -> Constraint
resolveUsingIndex session catalog tableKey constraint =
    case constraint of
        UsingIndexConstraint name index role placement ->
            let unqualifiedIndexKeys =
                    resolveIndexKeys session catalog (NamedIndex (Name [index]))
                tableName = relationKeyName tableKey
                -- Resolve unqualified index in table's schema or current schema.
                tableSchemaIndexKeys =
                    case nameParts tableName of
                        _schema : _table : _ ->
                            resolveIndexKeys
                                session
                                catalog
                                (NamedIndex (Name (init (nameParts tableName) ++ [index])))
                        _ -> []
                indexKeys =
                    case placement of
                        IndexTableSchema ->
                            case tableSchemaIndexKeys of
                                [] -> unqualifiedIndexKeys
                                keys -> keys
                        IndexCurrentSchema -> unqualifiedIndexKeys
                columns =
                    concat
                        [ indexStateColumns state
                        | indexKey <- indexKeys
                        , Just state <- [Map.lookup indexKey (catalogIndexes catalog)]
                        ]
             in if null columns
                    then OtherConstraint name
                    else
                        if role == PrimaryIndex
                            then PrimaryKeyConstraint name columns
                            else UniqueConstraint name columns
        other -> other

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

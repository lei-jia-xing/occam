-- | Catalog keys, relation/object state records, pure catalog operations, and write signatures.
module Occam.Dependency.State.Catalog (
    -- Canonical catalog keys
    RelationKey (..),
    IndexKey (..),
    TriggerKey (..),
    SequenceKey (..),
    ConstraintIdentity (..),
    ConstraintKey (..),
    ObjectKey (..),
    BarrierKey (..),
    -- Catalog state
    RelationState (..),
    IndexState (..),
    TriggerState (..),
    SequenceState (..),
    ConstraintState (..),
    DefinitionState (..),
    Catalog (..),
    emptyCatalog,
    newRelation,
    -- Pure catalog operations
    lookupRelation,
    insertRelation,
    deleteRelation,
    deleteIndex,
    deleteTrigger,
    deleteSequence,
    deleteConstraint,
    clearRelationObjects,
    rebuildForeignKeys,
    rebuildChildRelations,
    rebuildRelationsByLast,
    -- Write signatures and name utilities
    WriteSignature (..),
    writeSignature,
    nameSuffix,
    nameLast,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (
    Constraint (..),
    DeleteScope,
    ForeignKey,
    Ident,
    Name (..),
    NameRef,
    OnCommitAction,
    ReadFact,
    RelationKind,
    SourceColumn (..),
    StatementId,
    TriggerBody,
    TriggerEvent,
    TriggerTiming,
    ViewColumnLineage (..),
    WriteKind (..),
 )

-- Canonical catalog keys
data RelationKey = RelationKey
    { relationKeyTemp :: !Bool
    , relationKeyName :: !Name
    }
    deriving (Eq, Ord, Show)

data IndexKey
    = NamedIndexKey !Name
    | TableIndexKey !RelationKey !Ident
    deriving (Eq, Ord, Show)

data TriggerKey
    = NamedTriggerKey !Name
    | TableTriggerKey !RelationKey !Ident
    deriving (Eq, Ord, Show)

data SequenceKey = SequenceKey
    { sequenceKeyTemp :: !Bool
    , sequenceKeyName :: !Name
    }
    deriving (Eq, Ord, Show)

data ConstraintIdentity
    = NamedConstraint !Ident
    | UnnamedConstraint !StatementId !Int
    deriving (Eq, Ord, Show)

data ConstraintKey = ConstraintKey
    { constraintKeyTable :: !RelationKey
    , constraintKeyIdentity :: !ConstraintIdentity
    }
    deriving (Eq, Ord, Show)

data ObjectKey
    = BarrierRelationKey !RelationKey
    | BarrierIndexKey !IndexKey
    | BarrierTriggerKey !TriggerKey
    | BarrierSequenceKey !SequenceKey
    | BarrierTypeKey !Name
    | BarrierSchemaKey !Name
    | BarrierConstraintKey !ConstraintKey
    deriving (Eq, Ord, Show)

data BarrierKey
    = GlobalBarrierKey
    | SessionBarrierKey
    | TransactionBarrierKey
    | RelationBarrierKey !RelationKey
    | SchemaBarrierKey !Name
    | ObjectBarrierKey !ObjectKey
    deriving (Eq, Ord, Show)

-- Catalog state
data RelationState = RelationState
    { relKind :: !RelationKind
    , relDefinition :: !(Maybe StatementId)
    , relColumns :: !(Set Ident)
    , relDataWrites :: !(Set StatementId)
    , relCardinalityWrites :: !(Set StatementId)
    , relColumnWrites :: !(Map Ident (Set StatementId))
    , relSchemaWrites :: !(Set StatementId)
    , relRestoredBy :: !(Maybe StatementId)
    , relSources :: !(Set RelationKey)
    , relUnresolvedSources :: !(Set NameRef)
    , -- The reads that defined a view or table source, retaining the columns
      -- used so column drops can distinguish dependent from unaffected views.
      relSourceReads :: ![ReadFact]
    , relPartitionParents :: !(Set NameRef)
    -- ^ Partition parent references (`PARTITION OF`, `ATTACH PARTITION`).
    , relInheritParents :: !(Set NameRef)
    , relOnCommit :: !(Maybe OnCommitAction)
    , relOnCommitFrameId :: !(Maybe Int)
    , relTransactional :: !Bool
    , relIndexes :: !(Set IndexKey)
    , relTriggers :: !(Set TriggerKey)
    , relConstraints :: !(Set ConstraintKey)
    , relColumnLineage :: ![ViewColumnLineage]
    , relConditionColumns :: ![SourceColumn]
    }
    deriving (Eq, Show)

data IndexState = IndexState
    { indexDefinition :: !(Maybe StatementId)
    , indexStateTable :: !(Maybe RelationKey)
    , indexStateColumns :: ![Ident]
    , indexRestoredBy :: !(Maybe StatementId)
    }
    deriving (Eq, Show)

data TriggerState = TriggerState
    { triggerDefinition :: !(Maybe StatementId)
    , triggerStateTable :: !(Maybe RelationKey)
    , triggerStateTiming :: !TriggerTiming
    , triggerStateEvents :: ![TriggerEvent]
    , triggerStateWhenReads :: ![ReadFact]
    , triggerStateBody :: !TriggerBody
    , triggerRestoredBy :: !(Maybe StatementId)
    }
    deriving (Eq, Show)

data SequenceState = SequenceState
    { sequenceDefinition :: !(Maybe StatementId)
    , sequenceWrites :: !(Set StatementId)
    , sequenceRestoredBy :: !(Maybe StatementId)
    , sequenceStateTransactional :: !Bool
    }
    deriving (Eq, Show)

data ConstraintState = ConstraintState
    { constraintStateTable :: !RelationKey
    , constraintDefinition :: !(Maybe StatementId)
    , constraintValue :: !Constraint
    , constraintRestoredBy :: !(Maybe StatementId)
    }
    deriving (Eq, Show)

data DefinitionState = DefinitionState
    { definitionStatement :: !(Maybe StatementId)
    , definitionRestoredBy :: !(Maybe StatementId)
    }
    deriving (Eq, Show)

data Catalog = Catalog
    { catalogRelations :: !(Map RelationKey RelationState)
    , -- True if this catalog or a restored snapshot may contain ON COMMIT actions.
      catalogMayHaveOnCommit :: !Bool
    , catalogRelationsByLast :: !(Map Ident (Set RelationKey))
    , catalogIndexes :: !(Map IndexKey IndexState)
    , catalogTriggers :: !(Map TriggerKey TriggerState)
    , catalogSequences :: !(Map SequenceKey SequenceState)
    , catalogConstraints :: !(Map ConstraintKey ConstraintState)
    , catalogForeignKeys :: !(Map ConstraintKey (ConstraintState, ForeignKey))
    , catalogChildRelations :: !(Set RelationKey)
    , catalogTypes :: !(Map Name DefinitionState)
    , catalogSchemas :: !(Map Name DefinitionState)
    , catalogNextUnnamedConstraint :: !Int
    -- ^ Monotonic serial for naming unnamed constraints.
    }
    deriving (Eq, Show)

emptyCatalog :: Catalog
emptyCatalog =
    Catalog
        { catalogRelations = Map.empty
        , catalogMayHaveOnCommit = False
        , catalogRelationsByLast = Map.empty
        , catalogIndexes = Map.empty
        , catalogTriggers = Map.empty
        , catalogSequences = Map.empty
        , catalogConstraints = Map.empty
        , catalogForeignKeys = Map.empty
        , catalogChildRelations = Set.empty
        , catalogTypes = Map.empty
        , catalogSchemas = Map.empty
        , catalogNextUnnamedConstraint = 0
        }

newRelation :: RelationKind -> RelationState
newRelation kind =
    RelationState
        { relKind = kind
        , relDefinition = Nothing
        , relColumns = Set.empty
        , relDataWrites = Set.empty
        , relCardinalityWrites = Set.empty
        , relColumnWrites = Map.empty
        , relSchemaWrites = Set.empty
        , relRestoredBy = Nothing
        , relSources = Set.empty
        , relUnresolvedSources = Set.empty
        , relSourceReads = []
        , relPartitionParents = Set.empty
        , relInheritParents = Set.empty
        , relOnCommit = Nothing
        , relOnCommitFrameId = Nothing
        , relTransactional = True
        , relIndexes = Set.empty
        , relTriggers = Set.empty
        , relConstraints = Set.empty
        , relColumnLineage = []
        , relConditionColumns = []
        }

-- Pure catalog operations
lookupRelation :: Catalog -> RelationKey -> Maybe RelationState
lookupRelation catalog key = Map.lookup key (catalogRelations catalog)

insertRelation :: RelationKey -> RelationState -> Catalog -> Catalog
insertRelation key relation catalog =
    let isChild = not (Set.null (relPartitionParents relation) && Set.null (relInheritParents relation))
        children = catalogChildRelations catalog
        children'
            | isChild = Set.insert key children
            | Set.null children = children
            | otherwise = Set.delete key children
        (mbExisting, rels') =
            Map.insertLookupWithKey (\_ new _ -> new) key relation (catalogRelations catalog)
        byLast'
            | Nothing <- mbExisting
            , not (relationKeyTemp key)
            , Just ident <- nameLast (relationKeyName key) =
                Map.insertWith Set.union ident (Set.singleton key) (catalogRelationsByLast catalog)
            | otherwise = catalogRelationsByLast catalog
     in catalog
            { catalogRelations = rels'
            , catalogMayHaveOnCommit = catalogMayHaveOnCommit catalog || isJust (relOnCommit relation)
            , catalogRelationsByLast = byLast'
            , catalogChildRelations = children'
            }

deleteRelation :: RelationKey -> Catalog -> Catalog
deleteRelation key catalog =
    let children = catalogChildRelations catalog
        children' = if Set.null children then children else Set.delete key children
        byLast = catalogRelationsByLast catalog
        byLast'
            | not (relationKeyTemp key)
            , not (Map.null byLast)
            , Just ident <- nameLast (relationKeyName key) =
                Map.update removeKey ident byLast
            | otherwise = byLast
        removeKey keys =
            let keys' = Set.delete key keys
             in if Set.null keys' then Nothing else Just keys'
     in catalog
            { catalogRelations = Map.delete key (catalogRelations catalog)
            , catalogRelationsByLast = byLast'
            , catalogChildRelations = children'
            }

deleteIndex :: IndexKey -> Catalog -> Catalog
deleteIndex key catalog =
    catalog {catalogIndexes = Map.delete key (catalogIndexes catalog)}

deleteTrigger :: TriggerKey -> Catalog -> Catalog
deleteTrigger key catalog =
    catalog {catalogTriggers = Map.delete key (catalogTriggers catalog)}

deleteSequence :: SequenceKey -> Catalog -> Catalog
deleteSequence key catalog =
    catalog {catalogSequences = Map.delete key (catalogSequences catalog)}

deleteConstraint :: ConstraintKey -> Catalog -> Catalog
deleteConstraint key catalog =
    let fks = catalogForeignKeys catalog
        fks' = if Map.null fks then fks else Map.delete key fks
     in catalog
            { catalogConstraints = Map.delete key (catalogConstraints catalog)
            , catalogForeignKeys = fks'
            , catalogRelations =
                Map.adjust
                    (\relation -> relation {relConstraints = Set.delete key (relConstraints relation)})
                    (constraintKeyTable key)
                    (catalogRelations catalog)
            }

rebuildForeignKeys ::
    Map ConstraintKey ConstraintState -> Map ConstraintKey (ConstraintState, ForeignKey)
rebuildForeignKeys =
    Map.mapMaybe $ \state -> case constraintValue state of
        ForeignKeyConstraint fk -> Just (state, fk)
        _ -> Nothing

rebuildChildRelations :: Map RelationKey RelationState -> Set RelationKey
rebuildChildRelations =
    Map.foldlWithKey'
        ( \acc key relation ->
            if Set.null (relPartitionParents relation) && Set.null (relInheritParents relation)
                then acc
                else Set.insert key acc
        )
        Set.empty

rebuildRelationsByLast :: Map RelationKey RelationState -> Map Ident (Set RelationKey)
rebuildRelationsByLast =
    Map.foldlWithKey'
        ( \acc key _ ->
            if relationKeyTemp key
                then acc
                else case nameLast (relationKeyName key) of
                    Nothing -> acc
                    Just ident -> Map.insertWith Set.union ident (Set.singleton key) acc
        )
        Map.empty

clearRelationObjects :: RelationKey -> Catalog -> Catalog
clearRelationObjects key catalog =
    case lookupRelation catalog key of
        Nothing -> catalog
        Just relation ->
            let withoutIndexes =
                    foldl' (flip deleteIndex) catalog (Set.toList (relIndexes relation))
                withoutTriggers =
                    foldl' (flip deleteTrigger) withoutIndexes (Set.toList (relTriggers relation))
                withoutConstraints =
                    foldl' (flip deleteConstraint) withoutTriggers (Set.toList (relConstraints relation))
             in deleteRelation key withoutConstraints

-- Write signatures
data WriteSignature
    = InsertSignature
    | UpdateSignature !(Set Ident)
    | DeleteSignature !DeleteScope
    | MergeSignature
    | TruncateSignature
    deriving (Eq, Ord, Show)

writeSignature :: WriteKind -> [Ident] -> WriteSignature
writeSignature kind columns =
    case kind of
        InsertWrite _ -> InsertSignature
        UpdateWrite _ -> UpdateSignature (Set.fromList columns)
        DeleteWrite scope -> DeleteSignature scope
        MergeWrite -> MergeSignature
        TruncateWrite {} -> TruncateSignature

-- Name utilities
nameSuffix :: Name -> Name -> Bool
nameSuffix (Name requested) (Name stored) =
    length requested <= length stored
        && drop (length stored - length requested) stored == requested

nameLast :: Name -> Maybe Ident
nameLast (Name parts) =
    case parts of
        [] -> Nothing
        xs -> Just (last xs)

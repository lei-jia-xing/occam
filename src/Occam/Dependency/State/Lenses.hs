-- | Lenses for catalog, relation, and sequence state records.
module Occam.Dependency.State.Lenses (
    catalogRelationsL,
    catalogIndexesL,
    catalogTriggersL,
    catalogSequencesL,
    catalogConstraintsL,
    catalogTypesL,
    catalogSchemasL,
    catalogNextUnnamedConstraintL,
    relColumnsL,
    relDefinitionL,
    relTransactionalL,
    relOnCommitL,
    relOnCommitFrameIdL,
    relDataWritesL,
    relCardinalityWritesL,
    relColumnWritesL,
    relSchemaWritesL,
    relConstraintsL,
    relIndexesL,
    relTriggersL,
    relSourcesL,
    relUnresolvedSourcesL,
    relSourceReadsL,
    relPartitionParentsL,
    relInheritParentsL,
    relColumnLineageL,
    relConditionColumnsL,
    sequenceWritesL,
    sequenceNonTxWritesL,
    modifyRelation,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Lens.Micro (Lens', lens, (%~), (&))
import Occam.Core.Schema (
    Ident,
    Name,
    NameRef,
    OnCommitAction,
    ReadFact,
    SourceColumn,
    ViewColumnLineage,
 )
import Occam.Core.Types (StatementId)
import Occam.Dependency.State.Catalog (
    Catalog (..),
    ConstraintKey,
    ConstraintState,
    DefinitionState,
    IndexKey,
    IndexState,
    RelationKey,
    RelationState (..),
    SequenceKey,
    SequenceState (..),
    TriggerKey,
    TriggerState,
 )

-- | Lens for relations inside a catalog.
catalogRelationsL :: Lens' Catalog (Map RelationKey RelationState)
catalogRelationsL = lens catalogRelations (\c r -> c {catalogRelations = r})

-- | Lens for indexes inside a catalog.
catalogIndexesL :: Lens' Catalog (Map IndexKey IndexState)
catalogIndexesL = lens catalogIndexes (\c i -> c {catalogIndexes = i})

-- | Lens for triggers inside a catalog.
catalogTriggersL :: Lens' Catalog (Map TriggerKey TriggerState)
catalogTriggersL = lens catalogTriggers (\c t -> c {catalogTriggers = t})

-- | Lens for sequences inside a catalog.
catalogSequencesL :: Lens' Catalog (Map SequenceKey SequenceState)
catalogSequencesL = lens catalogSequences (\c s -> c {catalogSequences = s})

-- | Lens for constraints inside a catalog.
catalogConstraintsL :: Lens' Catalog (Map ConstraintKey ConstraintState)
catalogConstraintsL = lens catalogConstraints (\c k -> c {catalogConstraints = k})

-- | Lens for types inside a catalog.
catalogTypesL :: Lens' Catalog (Map Name DefinitionState)
catalogTypesL = lens catalogTypes (\c t -> c {catalogTypes = t})

-- | Lens for schemas inside a catalog.
catalogSchemasL :: Lens' Catalog (Map Name DefinitionState)
catalogSchemasL = lens catalogSchemas (\c s -> c {catalogSchemas = s})

-- | Lens for catalog next unnamed constraint counter.
catalogNextUnnamedConstraintL :: Lens' Catalog Int
catalogNextUnnamedConstraintL = lens catalogNextUnnamedConstraint (\c n -> c {catalogNextUnnamedConstraint = n})

-- | Lens for columns in a relation state.
relColumnsL :: Lens' RelationState (Set Ident)
relColumnsL = lens relColumns (\r c -> r {relColumns = c})

-- | Lens for definition statement in a relation state.
relDefinitionL :: Lens' RelationState (Maybe StatementId)
relDefinitionL = lens relDefinition (\r d -> r {relDefinition = d})

-- | Lens for transactional flag in a relation state.
relTransactionalL :: Lens' RelationState Bool
relTransactionalL = lens relTransactional (\r t -> r {relTransactional = t})

-- | Lens for on-commit action in a relation state.
relOnCommitL :: Lens' RelationState (Maybe OnCommitAction)
relOnCommitL = lens relOnCommit (\r a -> r {relOnCommit = a})

-- | Lens for on-commit frame id in a relation state.
relOnCommitFrameIdL :: Lens' RelationState (Maybe Int)
relOnCommitFrameIdL = lens relOnCommitFrameId (\r f -> r {relOnCommitFrameId = f})

-- | Lens for data writes in a relation state.
relDataWritesL :: Lens' RelationState (Set StatementId)
relDataWritesL = lens relDataWrites (\r w -> r {relDataWrites = w})

-- | Lens for cardinality writes in a relation state.
relCardinalityWritesL :: Lens' RelationState (Set StatementId)
relCardinalityWritesL = lens relCardinalityWrites (\r w -> r {relCardinalityWrites = w})

-- | Lens for column writes in a relation state.
relColumnWritesL :: Lens' RelationState (Map Ident (Set StatementId))
relColumnWritesL = lens relColumnWrites (\r w -> r {relColumnWrites = w})

-- | Lens for schema writes in a relation state.
relSchemaWritesL :: Lens' RelationState (Set StatementId)
relSchemaWritesL = lens relSchemaWrites (\r w -> r {relSchemaWrites = w})

-- | Lens for constraints in a relation state.
relConstraintsL :: Lens' RelationState (Set ConstraintKey)
relConstraintsL = lens relConstraints (\r k -> r {relConstraints = k})

-- | Lens for indexes in a relation state.
relIndexesL :: Lens' RelationState (Set IndexKey)
relIndexesL = lens relIndexes (\r i -> r {relIndexes = i})

-- | Lens for triggers in a relation state.
relTriggersL :: Lens' RelationState (Set TriggerKey)
relTriggersL = lens relTriggers (\r t -> r {relTriggers = t})

-- | Lens for sources in a relation state.
relSourcesL :: Lens' RelationState (Set RelationKey)
relSourcesL = lens relSources (\r s -> r {relSources = s})

-- | Lens for unresolved sources in a relation state.
relUnresolvedSourcesL :: Lens' RelationState (Set NameRef)
relUnresolvedSourcesL = lens relUnresolvedSources (\r s -> r {relUnresolvedSources = s})

-- | Lens for source reads in a relation state.
relSourceReadsL :: Lens' RelationState [ReadFact]
relSourceReadsL = lens relSourceReads (\r s -> r {relSourceReads = s})

-- | Lens for partition parents in a relation state.
relPartitionParentsL :: Lens' RelationState (Set NameRef)
relPartitionParentsL = lens relPartitionParents (\r p -> r {relPartitionParents = p})

-- | Lens for inheritance parents in a relation state.
relInheritParentsL :: Lens' RelationState (Set NameRef)
relInheritParentsL = lens relInheritParents (\r p -> r {relInheritParents = p})

-- | Lens for column lineage in a relation state.
relColumnLineageL :: Lens' RelationState [ViewColumnLineage]
relColumnLineageL = lens relColumnLineage (\r l -> r {relColumnLineage = l})

-- | Lens for condition columns in a relation state.
relConditionColumnsL :: Lens' RelationState [SourceColumn]
relConditionColumnsL = lens relConditionColumns (\r c -> r {relConditionColumns = c})

-- | Lens for sequence writes in a sequence state.
sequenceWritesL :: Lens' SequenceState (Set StatementId)
sequenceWritesL = lens sequenceWrites (\s w -> s {sequenceWrites = w})

-- | Lens for non-transactional sequence writes preserved across rollback.
sequenceNonTxWritesL :: Lens' SequenceState (Set StatementId)
sequenceNonTxWritesL = lens sequenceNonTxWrites (\s w -> s {sequenceNonTxWrites = w})

-- | Modify a relation state within a catalog if present.
modifyRelation :: RelationKey -> (RelationState -> RelationState) -> Catalog -> Catalog
modifyRelation key f cat = cat & catalogRelationsL %~ Map.adjust f key

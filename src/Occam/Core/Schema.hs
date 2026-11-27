-- | Core SQL schema, DDL, DML, and identifier types.
module Occam.Core.Schema (
    Ident (..),
    Name (..),
    NameRef (..),
    nameText,
    namePartsText,
    ReadMode (..),
    ReadFact (..),
    RelationKind (..),
    ConstraintValidity (..),
    IndexUniqueness (..),
    IndexRole (..),
    IndexSchemaPlacement (..),
    Constraint (..),
    ForeignKey (..),
    ReferentialAction (..),
    OnCommitAction (..),
    ColumnDef (..),
    TableDef (..),
    SourceColumn (..),
    ViewColumnLineage (..),
    ViewDef (..),
    IndexRef (..),
    IndexDef (..),
    TriggerRef (..),
    ConstraintRef (..),
    SequenceDef (..),
    TypeDef (..),
    SchemaDef (..),
    ConflictAction (..),
    UpdateScope (..),
    DeleteScope (..),
    TruncateBehavior (..),
    TruncateIdentity (..),
    WriteKind (..),
    DataWrite (..),
    SequenceWrite (..),
    AlterChange (..),
    AlterTable (..),
    PartitionLink (..),
    DropMode (..),
    DropObject (..),
    DropOp (..),
    RenameOp (..),
    RefreshOp (..),
    DmlEvent (..),
) where

import Data.Aeson (ToJSON (..), Value (..))
import Data.Text (Text)
import Data.Text qualified as T

newtype Ident = Ident {identText :: Text}
    deriving (Eq, Ord, Show)

newtype Name = Name {nameParts :: [Ident]}
    deriving (Eq, Ord, Show)

nameText :: Name -> Text
nameText = T.intercalate "." . namePartsText

namePartsText :: Name -> [Text]
namePartsText (Name parts) = map identText parts

data NameRef = NameRef
    { nameRefName :: !Name
    , nameRefTemp :: !Bool
    }
    deriving (Eq, Ord, Show)

data ReadMode = ReadData | ReadSchema | ReadRowCount | ReadRowExistence
    deriving (Eq, Ord, Show)

data ReadFact = ReadFact
    { readSource :: !NameRef
    , readMode :: !ReadMode
    , readColumns :: ![Ident]
    , readOnly :: !Bool
    -- ^ PostgreSQL @ONLY@: skip partition/inheritance children.
    }
    deriving (Eq, Show)

data RelationKind = TableRelation | ViewRelation | MaterializedViewRelation
    deriving (Eq, Ord, Show)

data ConstraintValidity = ConstraintValid | ConstraintNotValid
    deriving (Eq, Ord, Show)

data IndexUniqueness = IndexNonUnique | IndexUnique
    deriving (Eq, Ord, Show)

data IndexRole = SecondaryIndex | PrimaryIndex
    deriving (Eq, Ord, Show)

data IndexSchemaPlacement = IndexTableSchema | IndexCurrentSchema
    deriving (Eq, Ord, Show)

-- | A table constraint definition.
data Constraint
    = PrimaryKeyConstraint !(Maybe Ident) ![Ident]
    | UniqueConstraint !(Maybe Ident) ![Ident]
    | ForeignKeyConstraint !ForeignKey
    | CheckConstraint !(Maybe Ident) ![Ident] !ConstraintValidity
    | IndexConstraint !(Maybe Ident) ![Ident] !IndexUniqueness
    | UsingIndexConstraint !(Maybe Ident) !Ident !IndexRole !IndexSchemaPlacement
    | OtherConstraint !(Maybe Ident)
    deriving (Eq, Show)

data ForeignKey = ForeignKey
    { foreignKeyName :: !(Maybe Ident)
    , foreignKeyChildColumns :: ![Ident]
    , foreignKeyParent :: !NameRef
    , foreignKeyParentColumns :: ![Ident]
    , foreignKeyOnDelete :: !ReferentialAction
    , foreignKeyOnUpdate :: !ReferentialAction
    , foreignKeyNotValid :: !Bool
    , foreignKeyDeferrable :: !Bool
    }
    deriving (Eq, Show)

data ReferentialAction = NoAction | Restrict | Cascade | SetNull | SetDefault
    deriving (Eq, Ord, Show)

data OnCommitAction = OnCommitDeleteRows | OnCommitDrop
    deriving (Eq, Ord, Show)

data ColumnDef = ColumnDef
    { columnName :: !Ident
    , columnNotNull :: !Bool
    , columnHasDefault :: !Bool
    , columnGenerated :: !Bool
    , columnReferences :: ![ForeignKey]
    }
    deriving (Eq, Show)

data TableDef = TableDef
    { tableName :: !Name
    , tableTemporary :: !Bool
    , tableTransactional :: !Bool
    , tableColumns :: ![ColumnDef]
    , tableConstraints :: ![Constraint]
    , tableSources :: ![ReadFact]
    , tableLike :: !(Maybe NameRef)
    , tablePartitionParent :: !(Maybe NameRef)
    -- ^ PostgreSQL @PARTITION OF@ parent.
    , tableInheritParents :: ![NameRef]
    -- ^ PostgreSQL @INHERITS@ parents.
    , tableOnCommit :: !(Maybe OnCommitAction)
    , tableIfNotExists :: !Bool
    , tableOrReplace :: !Bool
    }
    deriving (Eq, Show)

data SourceColumn = SourceColumn
    { sourceColumnRelation :: !NameRef
    , sourceColumnName :: !Ident
    }
    deriving (Eq, Ord, Show)

data ViewColumnLineage = ViewColumnLineage
    { lineageColumn :: !Ident
    , lineageSources :: !(Maybe [SourceColumn])
    }
    deriving (Eq, Show)

data ViewDef = ViewDef
    { viewName :: !Name
    , viewTemporary :: !Bool
    , viewMaterialized :: !Bool
    , viewColumns :: ![Ident]
    , viewSources :: ![ReadFact]
    , viewIfNotExists :: !Bool
    , viewOrReplace :: !Bool
    , viewColumnLineage :: ![ViewColumnLineage]
    , viewConditionColumns :: ![SourceColumn]
    }
    deriving (Eq, Show)

data IndexRef
    = NamedIndex !Name
    | TableIndex !NameRef !Ident
    deriving (Eq, Ord, Show)

data IndexDef = IndexDef
    { indexRef :: !IndexRef
    , indexTable :: !NameRef
    , indexColumns :: ![Ident]
    , indexUnique :: !Bool
    , indexPredicate :: !Bool
    , indexExpression :: !Bool
    , indexIfNotExists :: !Bool
    , indexSchemaPlacement :: !IndexSchemaPlacement
    }
    deriving (Eq, Show)

data TriggerRef
    = NamedTrigger !Name
    | TableTrigger !NameRef !Ident
    deriving (Eq, Ord, Show)

data ConstraintRef = ConstraintRef
    { constraintTable :: !NameRef
    , constraintRefName :: !Ident
    }
    deriving (Eq, Ord, Show)

data UpdateScope = UpdateAllRows | UpdateFilteredRows
    deriving (Eq, Ord, Show)

data DeleteScope = DeleteAllRows | DeleteFilteredRows
    deriving (Eq, Ord, Show)

data TruncateBehavior = TruncateRestrict | TruncateCascade
    deriving (Eq, Ord, Show)

data TruncateIdentity = ContinueIdentity | RestartIdentity
    deriving (Eq, Ord, Show)

data DmlEvent
    = DmlInsert !NameRef ![Ident]
    | DmlUpdate !NameRef ![Ident]
    | DmlMerge !NameRef ![Ident]
    | DmlDelete !NameRef !DeleteScope
    | DmlTruncate !NameRef !TruncateBehavior
    deriving (Eq, Show)

data SequenceDef = SequenceDef
    { sequenceName :: !Name
    , sequenceTemporary :: !Bool
    , sequenceTransactional :: !Bool
    }
    deriving (Eq, Show)

newtype TypeDef = TypeDef {typeName :: Name}
    deriving (Eq, Show)

newtype SchemaDef = SchemaDef {schemaName :: Name}
    deriving (Eq, Show)

data ConflictAction = ConflictError | ConflictIgnore | ConflictReplace | ConflictUpdate
    deriving (Eq, Ord, Show)

data WriteKind
    = InsertWrite !ConflictAction
    | UpdateWrite !UpdateScope
    | DeleteWrite !DeleteScope
    | MergeWrite
    | TruncateWrite !TruncateBehavior !TruncateIdentity
    deriving (Eq, Show)

data DataWrite = DataWrite
    { dataWriteTarget :: !NameRef
    , dataWriteKind :: !WriteKind
    , dataWriteColumns :: ![Ident]
    , dataWriteUnqualifiedColumns :: ![Ident]
    , dataWriteReads :: ![ReadFact]
    , dataWriteOnly :: !Bool
    -- ^ PostgreSQL @ONLY@ on the target: skip routing into children.
    }
    deriving (Eq, Show)

data SequenceWrite = SequenceWrite
    { sequenceWriteTarget :: !NameRef
    , sequenceWriteTransactional :: !Bool
    }
    deriving (Eq, Show)

data AlterChange
    = AddColumn !ColumnDef ![Constraint]
    | DropColumn !Ident !Bool
    | AddConstraint !Constraint
    | DropConstraint !Ident !Bool
    | RenameConstraint !Ident !Ident
    | ValidateConstraint !Ident
    | DropIndexChange !Ident
    | AlterColumn !Ident !Text ![Ident] !Bool
    | OtherChange !Text
    deriving (Eq, Show)

data AlterTable = AlterTable
    { alterTableTarget :: !NameRef
    , alterTableChanges :: ![AlterChange]
    }
    deriving (Eq, Show)

-- | Parent/child routing link for partitions and table inheritance.
data PartitionLink = PartitionLink
    { linkParent :: !NameRef
    , linkChild :: !NameRef
    }
    deriving (Eq, Show)

data DropMode = DropRestrict | DropCascade
    deriving (Eq, Ord, Show)

data DropObject
    = DropRelation !NameRef !RelationKind
    | DropIndex !IndexRef
    | DropTrigger !TriggerRef
    | DropSequence !NameRef
    | DropType !Name
    | DropSchema !Name
    deriving (Eq, Ord, Show)

data DropOp = DropOp
    { dropObjects :: ![DropObject]
    , dropMode :: !DropMode
    }
    deriving (Eq, Show)

data RenameOp
    = RenameRelation !NameRef !Name !IndexSchemaPlacement
    | RenameColumnOp !NameRef !Ident !Ident
    deriving (Eq, Show)

data RefreshOp = RefreshOp
    { refreshTarget :: !NameRef
    , refreshSourceMode :: !ReadMode
    }
    deriving (Eq, Show)

instance ToJSON Ident where
    toJSON (Ident value) = String value

instance ToJSON Name where
    toJSON (Name parts) = toJSON parts

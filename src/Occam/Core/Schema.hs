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

-- | Unquoted or normalized SQL identifier text.
newtype Ident = Ident {identText :: Text}
    deriving (Eq, Ord, Show)

-- | Qualified SQL object name consisting of one or more schema\/object 'Ident' parts.
newtype Name = Name {nameParts :: [Ident]}
    deriving (Eq, Ord, Show)

-- | Format a 'Name' as a dot-separated string (e.g. @"schema.table"@).
nameText :: Name -> Text
nameText = T.intercalate "." . namePartsText

-- | Extract the raw 'Text' segments of a 'Name'.
namePartsText :: Name -> [Text]
namePartsText (Name parts) = map identText parts

-- | Catalog object reference paired with whether temporary-object lookup applies.
data NameRef = NameRef
    { nameRefName :: !Name
    , nameRefTemp :: !Bool
    }
    deriving (Eq, Ord, Show)

-- | Kind of table read performed by a statement, determining whether data writes or only schema\/cardinality affect it.
data ReadMode = ReadData | ReadSchema | ReadRowCount | ReadRowExistence
    deriving (Eq, Ord, Show)

-- | Relation read observation produced during statement semantic analysis.
data ReadFact = ReadFact
    { readSource :: !NameRef
    , readMode :: !ReadMode
    , readColumns :: ![Ident]
    , readOnly :: !Bool
    -- ^ PostgreSQL @ONLY@: skip partition/inheritance children.
    }
    deriving (Eq, Show)

-- | Storage\/definition class of a catalog relation.
data RelationKind = TableRelation | ViewRelation | MaterializedViewRelation
    deriving (Eq, Ord, Show)

-- | Validation status of a table constraint (PostgreSQL @NOT VALID@ vs enforced).
data ConstraintValidity = ConstraintValid | ConstraintNotValid
    deriving (Eq, Ord, Show)

-- | Uniqueness mode of an index definition.
data IndexUniqueness = IndexNonUnique | IndexUnique
    deriving (Eq, Ord, Show)

-- | Role distinguishing primary-key backing indexes from secondary indexes.
data IndexRole = SecondaryIndex | PrimaryIndex
    deriving (Eq, Ord, Show)

-- | Dialect rule governing which schema houses a newly created or renamed index.
data IndexSchemaPlacement = IndexTableSchema | IndexCurrentSchema
    deriving (Eq, Ord, Show)

-- | A table constraint definition modeled for column and foreign-key coupling.
data Constraint
    = PrimaryKeyConstraint !(Maybe Ident) ![Ident]
    | UniqueConstraint !(Maybe Ident) ![Ident]
    | ForeignKeyConstraint !ForeignKey
    | CheckConstraint !(Maybe Ident) ![Ident] !ConstraintValidity
    | IndexConstraint !(Maybe Ident) ![Ident] !IndexUniqueness
    | UsingIndexConstraint !(Maybe Ident) !Ident !IndexRole !IndexSchemaPlacement
    | OtherConstraint !(Maybe Ident)
    deriving (Eq, Show)

-- | Foreign-key relationship linking child columns on a table to parent columns.
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

-- | Referential integrity action for @ON DELETE@ \/ @ON UPDATE@ foreign-key clauses.
data ReferentialAction = NoAction | Restrict | Cascade | SetNull | SetDefault
    deriving (Eq, Ord, Show)

-- | Temporary-table end-of-transaction lifecycle behavior (@ON COMMIT@).
data OnCommitAction = OnCommitDeleteRows | OnCommitDrop
    deriving (Eq, Ord, Show)

-- | Normalized column metadata tracked inside the catalog.
data ColumnDef = ColumnDef
    { columnName :: !Ident
    , columnNotNull :: !Bool
    , columnHasDefault :: !Bool
    , columnGenerated :: !Bool
    , columnReferences :: ![ForeignKey]
    }
    deriving (Eq, Show)

-- | Normalized table creation metadata extracted from @CREATE TABLE@ or @SELECT ... INTO@.
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

-- | Single qualified source column referenced in a view output expression.
data SourceColumn = SourceColumn
    { sourceColumnRelation :: !NameRef
    , sourceColumnName :: !Ident
    }
    deriving (Eq, Ord, Show)

-- | Column-level lineage mapping an output view column to its upstream source columns.
data ViewColumnLineage = ViewColumnLineage
    { lineageColumn :: !Ident
    , lineageSources :: !(Maybe [SourceColumn])
    }
    deriving (Eq, Show)

-- | Normalized view or materialized-view metadata including sources and output lineage.
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

-- | Catalog reference to a standalone named index or table-scoped index.
data IndexRef
    = NamedIndex !Name
    | TableIndex !NameRef !Ident
    deriving (Eq, Ord, Show)

-- | Metadata extracted from @CREATE INDEX@ or inline index declarations.
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

-- | Catalog reference to a standalone named trigger or table-scoped trigger.
data TriggerRef
    = NamedTrigger !Name
    | TableTrigger !NameRef !Ident
    deriving (Eq, Ord, Show)

-- | Target table and constraint name for @DROP CONSTRAINT@ or @ALTER TABLE@.
data ConstraintRef = ConstraintRef
    { constraintTable :: !NameRef
    , constraintRefName :: !Ident
    }
    deriving (Eq, Ord, Show)

-- | Scope of an @UPDATE@ statement (unfiltered table rewrite vs conditional modification).
data UpdateScope = UpdateAllRows | UpdateFilteredRows
    deriving (Eq, Ord, Show)

-- | Scope of a @DELETE@ statement (full wipe vs conditional delete).
data DeleteScope = DeleteAllRows | DeleteFilteredRows
    deriving (Eq, Ord, Show)

-- | Restrict vs cascade dependency behavior on @TRUNCATE@.
data TruncateBehavior = TruncateRestrict | TruncateCascade
    deriving (Eq, Ord, Show)

-- | Sequence identity handling on @TRUNCATE@ (@CONTINUE IDENTITY@ vs @RESTART IDENTITY@).
data TruncateIdentity = ContinueIdentity | RestartIdentity
    deriving (Eq, Ord, Show)

-- | Normalized DML operation referenced inside trigger bodies and write facts.
data DmlEvent
    = DmlInsert !NameRef ![Ident]
    | DmlUpdate !NameRef ![Ident]
    | DmlMerge !NameRef ![Ident]
    | DmlDelete !NameRef !DeleteScope
    | DmlTruncate !NameRef !TruncateBehavior
    deriving (Eq, Show)

-- | Metadata extracted from @CREATE [TEMP] SEQUENCE@.
data SequenceDef = SequenceDef
    { sequenceName :: !Name
    , sequenceTemporary :: !Bool
    , sequenceTransactional :: !Bool
    }
    deriving (Eq, Show)

-- | Wrapper naming a user-defined SQL type declared by @CREATE TYPE@ \/ @CREATE DOMAIN@.
newtype TypeDef = TypeDef {typeName :: Name}
    deriving (Eq, Show)

-- | Wrapper naming a schema declared by @CREATE SCHEMA@.
newtype SchemaDef = SchemaDef {schemaName :: Name}
    deriving (Eq, Show)

-- | Conflict-handling policy on @INSERT@ or SQLite\/MySQL @REPLACE@ \/ @ON CONFLICT@.
data ConflictAction = ConflictError | ConflictIgnore | ConflictReplace | ConflictUpdate
    deriving (Eq, Ord, Show)

-- | Classification of a table data write (@INSERT@, @UPDATE@, @DELETE@, @MERGE@, @TRUNCATE@).
data WriteKind
    = InsertWrite !ConflictAction
    | UpdateWrite !UpdateScope
    | DeleteWrite !DeleteScope
    | MergeWrite
    | TruncateWrite !TruncateBehavior !TruncateIdentity
    deriving (Eq, Show)

-- | Full DML write fact including target relation, written columns, and embedded read facts.
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

-- | Sequence state mutation incurred by sequence manipulation calls or DDL.
data SequenceWrite = SequenceWrite
    { sequenceWriteTarget :: !NameRef
    , sequenceWriteTransactional :: !Bool
    }
    deriving (Eq, Show)

-- | Individual catalog mutation inside an @ALTER TABLE@ statement.
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

-- | Parsed @ALTER TABLE@ action list targeting a single relation.
data AlterTable = AlterTable
    { alterTableTarget :: !NameRef
    , alterTableChanges :: ![AlterChange]
    }
    deriving (Eq, Show)

{- | Parent/child routing link for partitions and table inheritance.
| Partition attachment edge connecting a partitioned parent and partition child table.
-}
data PartitionLink = PartitionLink
    { linkParent :: !NameRef
    , linkChild :: !NameRef
    }
    deriving (Eq, Show)

-- | Dependency-drop policy (@RESTRICT@ vs @CASCADE@).
data DropMode = DropRestrict | DropCascade
    deriving (Eq, Ord, Show)

-- | Target catalog object named in a @DROP@ statement.
data DropObject
    = DropRelation !NameRef !RelationKind
    | DropIndex !IndexRef
    | DropTrigger !TriggerRef
    | DropSequence !NameRef
    | DropType !Name
    | DropSchema !Name
    deriving (Eq, Ord, Show)

-- | Batch @DROP@ catalog operation pairing dropped targets with cascade\/temporary flags.
data DropOp = DropOp
    { dropObjects :: ![DropObject]
    , dropMode :: !DropMode
    }
    deriving (Eq, Show)

-- | Catalog rename operation for a relation or table column.
data RenameOp
    = RenameRelation !NameRef !Name !IndexSchemaPlacement
    | RenamePermanentRelation !NameRef !Name !IndexSchemaPlacement
    | RenameColumnOp !NameRef !Ident !Ident
    deriving (Eq, Show)

-- | Materialized-view refresh operation targeting a view name.
data RefreshOp = RefreshOp
    { refreshTarget :: !NameRef
    , refreshSourceMode :: !ReadMode
    }
    deriving (Eq, Show)

instance ToJSON Ident where
    toJSON (Ident value) = String value

instance ToJSON Name where
    toJSON (Name parts) = toJSON parts

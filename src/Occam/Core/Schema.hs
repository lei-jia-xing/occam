-- | Core SQL schema, DDL, DML, and identifier types with native JSON codecs.
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

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    withArray,
    withBool,
    withObject,
    withText,
    (.!=),
    (.:),
    (.:?),
 )
import Data.Foldable (toList)
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

-- ----------------------------------------------------------------------------
-- Native-parser JSON wire format
-- ----------------------------------------------------------------------------

instance FromJSON Ident where
    parseJSON = withText "Ident" (pure . Ident)

instance ToJSON Ident where
    toJSON (Ident value) = String value

instance FromJSON Name where
    parseJSON = withArray "Name" (fmap Name . traverse parseJSON . toList)

instance ToJSON Name where
    toJSON (Name parts) = toJSON parts

instance FromJSON NameRef where
    parseJSON = withObject "NameRef" \objectValue ->
        NameRef
            <$> objectValue .: "name"
            <*> objectValue .: "temp"

instance FromJSON ReadMode where
    parseJSON = withText "ReadMode" \name ->
        case name of
            "data" -> pure ReadData
            "schema" -> pure ReadSchema
            "row_count" -> pure ReadRowCount
            "row_existence" -> pure ReadRowExistence
            _ -> fail ("unknown read mode: " <> T.unpack name)

instance FromJSON ReadFact where
    parseJSON = withObject "ReadFact" \objectValue ->
        ReadFact
            <$> objectValue .: "source"
            <*> objectValue .: "mode"
            <*> objectValue .: "columns"
            <*> objectValue .: "only"

instance FromJSON RelationKind where
    parseJSON = withText "RelationKind" \name ->
        case name of
            "table" -> pure TableRelation
            "view" -> pure ViewRelation
            "materialized_view" -> pure MaterializedViewRelation
            _ -> fail ("unknown relation kind: " <> T.unpack name)

instance FromJSON ReferentialAction where
    parseJSON = withText "ReferentialAction" \name ->
        case name of
            "no_action" -> pure NoAction
            "restrict" -> pure Restrict
            "cascade" -> pure Cascade
            "set_null" -> pure SetNull
            "set_default" -> pure SetDefault
            _ -> fail ("unknown referential action: " <> T.unpack name)

instance FromJSON OnCommitAction where
    parseJSON = withText "OnCommitAction" \name ->
        case name of
            "delete_rows" -> pure OnCommitDeleteRows
            "drop" -> pure OnCommitDrop
            _ -> fail ("unknown ON COMMIT action: " <> T.unpack name)

instance FromJSON ConstraintValidity where
    parseJSON = withBool "ConstraintValidity" \notValid ->
        pure (if notValid then ConstraintNotValid else ConstraintValid)

instance FromJSON IndexUniqueness where
    parseJSON = withBool "IndexUniqueness" \unique ->
        pure (if unique then IndexUnique else IndexNonUnique)

instance FromJSON IndexRole where
    parseJSON = withBool "IndexRole" \primary ->
        pure (if primary then PrimaryIndex else SecondaryIndex)

instance FromJSON IndexSchemaPlacement where
    parseJSON = withText "IndexSchemaPlacement" \name ->
        case name of
            "table_schema" -> pure IndexTableSchema
            "current_schema" -> pure IndexCurrentSchema
            _ -> fail ("unknown index schema placement: " <> T.unpack name)

instance FromJSON UpdateScope where
    parseJSON = withBool "UpdateScope" \allRows ->
        pure (if allRows then UpdateAllRows else UpdateFilteredRows)

instance FromJSON DeleteScope where
    parseJSON = withBool "DeleteScope" \allRows ->
        pure (if allRows then DeleteAllRows else DeleteFilteredRows)

instance FromJSON TruncateBehavior where
    parseJSON = withBool "TruncateBehavior" \cascade ->
        pure (if cascade then TruncateCascade else TruncateRestrict)

instance FromJSON TruncateIdentity where
    parseJSON = withBool "TruncateIdentity" \restart ->
        pure (if restart then RestartIdentity else ContinueIdentity)

instance FromJSON ColumnDef where
    parseJSON = withObject "ColumnDef" \objectValue ->
        ColumnDef
            <$> objectValue .: "name"
            <*> objectValue .: "not_null"
            <*> objectValue .: "has_default"
            <*> objectValue .: "generated"
            <*> objectValue .: "references"

instance FromJSON ForeignKey where
    parseJSON = withObject "ForeignKey" \objectValue ->
        ForeignKey
            <$> objectValue .: "name"
            <*> objectValue .: "child_columns"
            <*> objectValue .: "parent"
            <*> objectValue .: "parent_columns"
            <*> objectValue .: "on_delete"
            <*> objectValue .: "on_update"
            <*> objectValue .: "not_valid"
            <*> objectValue .: "deferrable"

instance FromJSON Constraint where
    parseJSON = withObject "Constraint" \objectValue -> do
        tag <- objectValue .: "constraint"
        case tag of
            "primary_key" ->
                PrimaryKeyConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "columns"
            "unique" ->
                UniqueConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "columns"
            "foreign_key" -> ForeignKeyConstraint <$> parseJSON (Object objectValue)
            "check" ->
                CheckConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "columns"
                    <*> objectValue .: "not_valid"
            "index" ->
                IndexConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "columns"
                    <*> objectValue .: "unique"
            "using_index" ->
                UsingIndexConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "index"
                    <*> objectValue .: "primary"
                    <*> objectValue .: "schema_placement"
            "other" -> OtherConstraint <$> objectValue .: "name"
            _ -> fail ("unknown constraint kind: " <> T.unpack tag)

instance FromJSON TableDef where
    parseJSON = withObject "TableDef" \objectValue ->
        TableDef
            <$> objectValue .: "name"
            <*> objectValue .: "temporary"
            <*> objectValue .: "transactional"
            <*> objectValue .: "columns"
            <*> objectValue .: "constraints"
            <*> objectValue .: "sources"
            <*> objectValue .: "like"
            <*> objectValue .: "partition_parent"
            <*> objectValue .: "inherit_parents"
            <*> objectValue .: "on_commit"
            <*> objectValue .: "if_not_exists"
            <*> objectValue .: "or_replace"

instance FromJSON SourceColumn where
    parseJSON = withObject "SourceColumn" \objectValue ->
        SourceColumn
            <$> objectValue .: "source"
            <*> objectValue .: "column"

instance FromJSON ViewColumnLineage where
    parseJSON = withObject "ViewColumnLineage" \objectValue ->
        ViewColumnLineage
            <$> objectValue .: "column"
            <*> objectValue .:? "sources"

instance FromJSON ViewDef where
    parseJSON = withObject "ViewDef" \objectValue ->
        ViewDef
            <$> objectValue .: "name"
            <*> objectValue .: "temporary"
            <*> objectValue .: "materialized"
            <*> objectValue .: "columns"
            <*> objectValue .: "sources"
            <*> objectValue .: "if_not_exists"
            <*> objectValue .: "or_replace"
            <*> (objectValue .:? "column_lineage" .!= [])
            <*> (objectValue .:? "condition_columns" .!= [])

instance FromJSON IndexRef where
    parseJSON = withObject "IndexRef" \objectValue -> do
        tag <- objectValue .: "scope"
        case tag of
            "named" -> NamedIndex <$> objectValue .: "name"
            "table" -> TableIndex <$> objectValue .: "table" <*> objectValue .: "name"
            _ -> fail ("unknown index scope: " <> T.unpack tag)

instance FromJSON IndexDef where
    parseJSON = withObject "IndexDef" \objectValue ->
        IndexDef
            <$> objectValue .: "index"
            <*> objectValue .: "table"
            <*> objectValue .: "columns"
            <*> objectValue .: "unique"
            <*> objectValue .: "predicate"
            <*> objectValue .: "expression"
            <*> objectValue .: "if_not_exists"
            <*> objectValue .: "schema_placement"

instance FromJSON TriggerRef where
    parseJSON = withObject "TriggerRef" \objectValue -> do
        tag <- objectValue .: "scope"
        case tag of
            "named" -> NamedTrigger <$> objectValue .: "name"
            "table" -> TableTrigger <$> objectValue .: "table" <*> objectValue .: "name"
            _ -> fail ("unknown trigger scope: " <> T.unpack tag)

instance FromJSON ConstraintRef where
    parseJSON = withObject "ConstraintRef" \objectValue ->
        ConstraintRef
            <$> objectValue .: "table"
            <*> objectValue .: "name"

instance FromJSON DmlEvent where
    parseJSON = withObject "DmlEvent" \objectValue -> do
        tag <- objectValue .: "event"
        case tag of
            "insert" -> DmlInsert <$> objectValue .: "target" <*> objectValue .: "columns"
            "update" -> DmlUpdate <$> objectValue .: "target" <*> objectValue .: "columns"
            "merge" -> DmlMerge <$> objectValue .: "target" <*> objectValue .: "columns"
            "delete" -> DmlDelete <$> objectValue .: "target" <*> objectValue .: "all"
            "truncate" -> DmlTruncate <$> objectValue .: "target" <*> objectValue .: "cascade"
            _ -> fail ("unknown DML event: " <> T.unpack tag)

instance FromJSON SequenceDef where
    parseJSON = withObject "SequenceDef" \objectValue ->
        SequenceDef
            <$> objectValue .: "name"
            <*> objectValue .: "temporary"
            <*> objectValue .: "transactional"

instance FromJSON TypeDef where
    parseJSON = withObject "TypeDef" \objectValue -> TypeDef <$> objectValue .: "name"

instance FromJSON SchemaDef where
    parseJSON = withObject "SchemaDef" \objectValue -> SchemaDef <$> objectValue .: "name"

instance FromJSON ConflictAction where
    parseJSON = withText "ConflictAction" \name ->
        case name of
            "error" -> pure ConflictError
            "ignore" -> pure ConflictIgnore
            "replace" -> pure ConflictReplace
            "update" -> pure ConflictUpdate
            _ -> fail ("unknown conflict action: " <> T.unpack name)

instance FromJSON WriteKind where
    parseJSON = withObject "WriteKind" \objectValue -> do
        tag <- objectValue .: "kind"
        case tag of
            "insert" -> InsertWrite <$> objectValue .: "conflict"
            "update" -> UpdateWrite <$> objectValue .: "all"
            "delete" -> DeleteWrite <$> objectValue .: "all"
            "merge" -> pure MergeWrite
            "truncate" ->
                TruncateWrite
                    <$> objectValue .: "cascade"
                    <*> objectValue .: "restart_identity"
            _ -> fail ("unknown write kind: " <> T.unpack tag)

instance FromJSON DataWrite where
    parseJSON = withObject "DataWrite" \objectValue ->
        DataWrite
            <$> objectValue .: "target"
            <*> objectValue .: "kind"
            <*> objectValue .: "columns"
            <*> objectValue .: "unqualified_columns"
            <*> objectValue .: "reads"
            <*> objectValue .: "only"

instance FromJSON SequenceWrite where
    parseJSON = withObject "SequenceWrite" \objectValue ->
        SequenceWrite
            <$> objectValue .: "sequence"
            <*> objectValue .: "transactional"

instance FromJSON AlterChange where
    parseJSON = withObject "AlterChange" \objectValue -> do
        tag <- objectValue .: "change"
        case tag of
            "add_column" ->
                AddColumn
                    <$> objectValue .: "column"
                    <*> objectValue .: "constraints"
            "drop_column" -> DropColumn <$> objectValue .: "column" <*> objectValue .: "cascade"
            "add_constraint" -> AddConstraint <$> objectValue .: "constraint"
            "drop_constraint" -> DropConstraint <$> objectValue .: "name" <*> objectValue .: "cascade"
            "rename_constraint" -> RenameConstraint <$> objectValue .: "from" <*> objectValue .: "to"
            "validate_constraint" -> ValidateConstraint <$> objectValue .: "name"
            "drop_index" -> DropIndexChange <$> objectValue .: "name"
            "alter_column" ->
                AlterColumn
                    <$> objectValue .: "column"
                    <*> objectValue .: "detail"
                    <*> objectValue .: "referenced"
                    <*> objectValue .: "repositions"
            "other" -> OtherChange <$> objectValue .: "detail"
            _ -> fail ("unknown ALTER TABLE change: " <> T.unpack tag)

instance FromJSON AlterTable where
    parseJSON = withObject "AlterTable" \objectValue ->
        AlterTable
            <$> objectValue .: "table"
            <*> objectValue .: "changes"

instance FromJSON PartitionLink where
    parseJSON = withObject "PartitionLink" \objectValue ->
        PartitionLink
            <$> objectValue .: "parent"
            <*> objectValue .: "child"

instance FromJSON DropMode where
    parseJSON = withText "DropMode" \name ->
        case name of
            "restrict" -> pure DropRestrict
            "cascade" -> pure DropCascade
            _ -> fail ("unknown drop mode: " <> T.unpack name)

instance FromJSON DropObject where
    parseJSON = withObject "DropObject" \objectValue -> do
        tag <- objectValue .: "object"
        case tag of
            "relation" -> DropRelation <$> objectValue .: "name" <*> objectValue .: "kind"
            "index" -> DropIndex <$> objectValue .: "index"
            "trigger" -> DropTrigger <$> objectValue .: "trigger"
            "sequence" -> DropSequence <$> objectValue .: "name"
            "type" -> DropType <$> objectValue .: "name"
            "schema" -> DropSchema <$> objectValue .: "name"
            _ -> fail ("unknown drop object: " <> T.unpack tag)

instance FromJSON DropOp where
    parseJSON = withObject "DropOp" \objectValue ->
        DropOp
            <$> objectValue .: "objects"
            <*> objectValue .: "mode"

instance FromJSON RenameOp where
    parseJSON = withObject "RenameOp" \objectValue -> do
        tag <- objectValue .: "rename"
        case tag of
            "relation" ->
                RenameRelation
                    <$> objectValue .: "from"
                    <*> objectValue .: "to"
                    <*> objectValue .: "index_schema_placement"
            "column" ->
                RenameColumnOp
                    <$> objectValue .: "table"
                    <*> objectValue .: "from"
                    <*> objectValue .: "to"
            _ -> fail ("unknown rename kind: " <> T.unpack tag)

instance FromJSON RefreshOp where
    parseJSON = withObject "RefreshOp" \objectValue ->
        RefreshOp <$> objectValue .: "target" <*> objectValue .: "source_mode"

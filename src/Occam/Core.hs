module Occam.Core (
    StatementId (..),
    Statement (..),
    Span (..),
    Ident (..),
    Name (..),
    NameRef (..),
    nameText,
    namePartsText,
    SearchPathEntry (..),
    SearchPathValue (..),
    SessionInit (..),
    ReadMode (..),
    ReadFact (..),
    RelationKind (..),
    ConstraintValidity (..),
    IndexUniqueness (..),
    IndexRole (..),
    Constraint (..),
    ForeignKey (..),
    ReferentialAction (..),
    OnCommitAction (..),
    ColumnDef (..),
    TableDef (..),
    ViewDef (..),
    IndexRef (..),
    IndexDef (..),
    IndexSchemaPlacement (..),
    TriggerRef (..),
    ConstraintRef (..),
    TriggerTiming (..),
    TriggerEvent (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    SequenceDef (..),
    TypeDef (..),
    SchemaDef (..),
    ConflictAction (..),
    DeleteScope (..),
    TruncateBehavior (..),
    TruncateIdentity (..),
    WriteKind (..),
    DataWrite (..),
    SequenceWrite (..),
    AlterChange (..),
    AlterTable (..),
    DropMode (..),
    DropObject (..),
    RenameOp (..),
    RefreshOp (..),
    CatalogOp (..),
    SessionScope (..),
    SessionValue (..),
    SessionEffect (..),
    BeginPolicy (..),
    TxEffect (..),
    BarrierReason (..),
    ObjectRef (..),
    BarrierScope (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    DmlEvent (..),
    DropOp (..),
) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T

newtype StatementId = StatementId Int
    deriving (Eq, Ord, Show)

data Span = Span
    { spanStartByte :: !Int
    , spanEndByte :: !Int
    }
    deriving (Eq, Show)

data Statement = Statement
    { stmtId :: !StatementId
    , stmtSpan :: !Span
    , stmtRaw :: !ByteString
    }
    deriving (Eq, Show)

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

data SearchPathEntry
    = SearchPathName !Name
    | SearchPathCurrentUser
    deriving (Eq, Ord, Show)

data SearchPathValue
    = SearchPathExplicit ![SearchPathEntry]
    | SearchPathDefault
    deriving (Eq, Ord, Show)

data SessionInit = SessionInit
    { sessionNamespacePrefix :: ![Ident]
    , sessionSearchPath :: ![SearchPathEntry]
    , sessionCurrentUser :: !(Maybe Ident)
    , sessionAutocommit :: !Bool
    , sessionForeignKeyChecks :: !Bool
    , sessionVariables :: !(Map Text Text)
    }
    deriving (Eq, Show)

data ReadMode = ReadData | ReadSchema
    deriving (Eq, Ord, Show)

data ReadFact = ReadFact
    { readSource :: !NameRef
    , readMode :: !ReadMode
    , readColumns :: ![Ident]
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

{- | A table constraint. The leading 'Maybe Ident' on most constructors is the
optional constraint name; the following list holds the constrained columns.
-}
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
    , tableOnCommit :: !(Maybe OnCommitAction)
    , tableIfNotExists :: !Bool
    , tableOrReplace :: !Bool
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

data IndexSchemaPlacement = IndexTableSchema | IndexCurrentSchema
    deriving (Eq, Ord, Show)

data TriggerRef
    = NamedTrigger !Name
    | TableTrigger !NameRef !Ident
    deriving (Eq, Ord, Show)

data ConstraintRef = ConstraintRef
    { constraintTable :: !NameRef
    , constraintRefName :: !Ident
    }
    deriving (Eq, Ord, Show)

data TriggerTiming = TriggerBefore | TriggerAfter | TriggerInsteadOf
    deriving (Eq, Ord, Show)

data TriggerEvent
    = TriggerInsert
    | TriggerUpdate ![Ident]
    | TriggerDelete
    | TriggerTruncate
    deriving (Eq, Ord, Show)

data TriggerBody
    = TriggerInline ![TriggerEffect]
    | TriggerFunction !Name ![Text]
    | TriggerOpaque
    deriving (Eq, Show)

data TriggerEffect
    = TriggerDml !DmlEvent
    | TriggerRead !ReadFact
    | TriggerWriteSequence !SequenceWrite
    | TriggerSession !SessionEffect
    | TriggerBarrier !ScopedBarrier
    deriving (Eq, Show)

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
    | UpdateWrite
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
    | AlterColumn !Ident !Text
    | OtherChange !Text
    deriving (Eq, Show)

data AlterTable = AlterTable
    { alterTableTarget :: !NameRef
    , alterTableChanges :: ![AlterChange]
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

data RenameOp
    = RenameRelation !NameRef !Name !IndexSchemaPlacement
    | RenameColumnOp !NameRef !Ident !Ident
    deriving (Eq, Show)

data RefreshOp = RefreshOp
    { refreshTarget :: !NameRef
    , refreshSourceMode :: !ReadMode
    }
    deriving (Eq, Show)

data CatalogOp
    = DefineTableOp !TableDef
    | DefineViewOp !ViewDef
    | DefineIndexOp !IndexDef
    | DefineTriggerOp !TriggerDef
    | DefineSequenceOp !SequenceDef
    | DefineTypeOp !TypeDef
    | DefineSchemaOp !SchemaDef
    | ReadOp !ReadFact
    | WriteDataOp !DataWrite
    | WriteSequenceOp !SequenceWrite
    | AlterTableOp !AlterTable
    | DropCatalogOp !DropOp
    | RenameOp !RenameOp
    | RefreshCatalogOp !RefreshOp
    deriving (Eq, Show)

data DropOp = DropOp
    { dropObjects :: ![DropObject]
    , dropMode :: !DropMode
    }
    deriving (Eq, Show)

data TriggerDef = TriggerDef
    { triggerRef :: !TriggerRef
    , triggerTable :: !NameRef
    , triggerTiming :: !TriggerTiming
    , triggerEvents :: ![TriggerEvent]
    , triggerForEachRow :: !Bool
    , triggerWhenReads :: ![ReadFact]
    , triggerBody :: !TriggerBody
    , triggerReferencedTable :: !(Maybe NameRef)
    , triggerTemporary :: !Bool
    , triggerIfNotExists :: !Bool
    , triggerOrReplace :: !Bool
    }
    deriving (Eq, Show)

data SessionScope = ScopeSession | ScopeTransaction | ScopeLocal | ScopeGlobal
    deriving (Eq, Ord, Show)

data SessionValue
    = SessionText !Text
    | SessionBool !Bool
    | SessionInteger !Integer
    | SessionNames ![Name]
    | SessionDefault
    deriving (Eq, Show)

data SessionEffect
    = SetVariable !SessionScope !Name !SessionValue
    | SetSearchPath !SessionScope !SearchPathValue
    | SetNamespace !SessionScope ![Ident]
    | -- | @SET SESSION AUTHORIZATION@: changes session_user and current_user.
      SetSessionUser !SessionScope !Ident
    | -- | @RESET SESSION AUTHORIZATION@: restores both to the authenticated user.
      ResetSessionUser !SessionScope
    | -- | @SET ROLE@: changes current_user only.
      SetCurrentUser !SessionScope !Ident
    | -- | @RESET ROLE@: restores current_user from session_user.
      ResetCurrentUser !SessionScope
    | SetAutocommit !SessionScope !Bool
    | SetForeignKeyChecks !SessionScope !Bool
    | {- | Restore @foreign_keys@/@foreign_key_checks@ to the session's initial
      configuration as resolved by the frontend.
      -}
      ResetForeignKeyChecks !SessionScope
    | ResetVariable !SessionScope !Name
    | ResetAll !SessionScope
    deriving (Eq, Show)

data BeginPolicy = BeginError | BeginWarn | BeginCommitActive | BeginIgnore
    deriving (Eq, Ord, Show)

data TxEffect
    = TxBegin !BeginPolicy
    | TxCommit !Bool
    | TxRollbackAll
    | TxRollbackTo !Ident
    | TxSavepoint !Ident
    | TxReleaseSavepoint !Ident
    | TxSetReadOnly !Bool
    deriving (Eq, Show)

data BarrierReason
    = SyntaxErrorBarrier
    | UnknownStatementBarrier
    | UnknownRelationBarrier
    | UnknownDDLBarrier
    | UnknownDMLBarrier
    | UnknownDropBarrier
    | UnknownAlterBarrier
    | UnknownViewBarrier
    | UnknownIndexBarrier
    | UnknownTriggerBarrier
    | UnknownTruncateBarrier
    | UnknownSessionEffectBarrier
    | UnsupportedExpressionBarrier
    | UnsupportedConflictClauseBarrier
    | UnsupportedTriggerBodyBarrier
    | UnsupportedTransactionBarrier
    | UnresolvedViewSourceBarrier
    | WriteThroughViewBarrier
    | OpaqueTriggerBodyBarrier
    | UnmodeledRelationSourceBarrier
    deriving (Eq, Ord, Show)

data ObjectRef
    = ObjectRelation !NameRef
    | ObjectIndex !IndexRef
    | ObjectTrigger !TriggerRef
    | ObjectSequence !NameRef
    | ObjectType !Name
    | ObjectSchema !Name
    | ObjectConstraint !ConstraintRef
    deriving (Eq, Ord, Show)

data BarrierScope
    = GlobalScope
    | RelationScope !NameRef
    | SchemaScope !Name
    | ObjectScope !ObjectRef
    | SessionStateScope
    | TransactionStateScope
    deriving (Eq, Ord, Show)

data ScopedBarrier = ScopedBarrier
    { barrierScope :: !BarrierScope
    , barrierReason :: !BarrierReason
    , barrierDetail :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show)

data SemanticFacts = SemanticFacts
    { factOps :: ![CatalogOp]
    , factSession :: ![SessionEffect]
    , factPreTx :: ![TxEffect]
    , factPostTx :: ![TxEffect]
    , factBarriers :: ![ScopedBarrier]
    }
    deriving (Eq, Show)

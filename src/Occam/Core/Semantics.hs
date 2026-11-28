-- | Semantic facts, session/transaction effects, triggers, and barriers.
module Occam.Core.Semantics (
    SearchPathEntry (..),
    SearchPathValue (..),
    SessionInit (..),
    SessionScope (..),
    SessionValue (..),
    SessionEffect (..),
    BeginPolicy (..),
    TxEffect (..),
    BarrierReason (..),
    ObjectRef (..),
    BarrierScope (..),
    ScopedBarrier (..),
    TriggerTiming (..),
    TriggerEvent (..),
    TriggerBody (..),
    TriggerEffect (..),
    TriggerDef (..),
    CatalogOp (..),
    SemanticFacts (..),
) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Occam.Core.Schema (
    AlterTable,
    ConstraintRef,
    DataWrite,
    DmlEvent,
    DropOp,
    Ident,
    IndexDef,
    IndexRef,
    Name,
    NameRef,
    PartitionLink,
    ReadFact,
    RefreshOp,
    RenameOp,
    SchemaDef,
    SequenceDef,
    SequenceWrite,
    TableDef,
    TriggerRef,
    TypeDef,
    ViewDef,
 )

-- | Single entry in a @search_path@ configuration (schema name or @$user@).
data SearchPathEntry
    = SearchPathName !Name
    | SearchPathCurrentUser
    deriving (Eq, Ord, Show)

-- | Explicit @search_path@ entry list or reset to dialect default.
data SearchPathValue
    = SearchPathExplicit ![SearchPathEntry]
    | SearchPathDefault
    deriving (Eq, Ord, Show)

-- | Initial session configuration captured before analyzing statements in a SQL seed.
data SessionInit = SessionInit
    { sessionNamespacePrefix :: ![Ident]
    , sessionSearchPath :: ![SearchPathEntry]
    , sessionCurrentUser :: !(Maybe Ident)
    , sessionAutocommit :: !Bool
    , sessionForeignKeyChecks :: !Bool
    , sessionVariables :: !(Map Text Text)
    , sessionTempInNamespace :: !Bool
    , sessionPermViewIgnoresTemp :: !Bool
    , sessionViewsBoundToRelations :: !Bool
    }
    deriving (Eq, Show)

-- | Phase when a SQL trigger fires relative to the triggering DML event.
data TriggerTiming = TriggerBefore | TriggerAfter | TriggerInsteadOf
    deriving (Eq, Ord, Show)

-- | DML event kind and optional column list that fires a trigger.
data TriggerEvent
    = TriggerInsert
    | TriggerUpdate ![Ident]
    | TriggerDelete
    | TriggerTruncate
    deriving (Eq, Ord, Show)

-- | Body of a trigger analyzed for inline effects, invoked procedure, or conservative opaque fallback.
data TriggerBody
    = TriggerInline ![TriggerEffect]
    | TriggerFunction !Name ![Text]
    | TriggerOpaque
    deriving (Eq, Show)

-- | Dependency-relevant effect extracted inside an inline trigger body.
data TriggerEffect
    = TriggerDml !DmlEvent
    | TriggerRead !ReadFact
    | TriggerWriteSequence !SequenceWrite
    | TriggerSession !SessionEffect
    | TriggerBarrier !ScopedBarrier
    deriving (Eq, Show)

-- | Full metadata for a trigger definition extracted from @CREATE TRIGGER@.
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

-- | DDL or schema catalog operation emitted by a single SQL statement.
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
    | AttachPartitionOp !PartitionLink
    | DetachPartitionOp !PartitionLink
    | AttachInheritOp !PartitionLink
    | DetachInheritOp !PartitionLink
    | DropCatalogOp !DropOp
    | RenameOp !RenameOp
    | RefreshCatalogOp !RefreshOp
    deriving (Eq, Show)

-- | Persistence scope of a @SET@ \/ @RESET@ session mutation.
data SessionScope = ScopeSession | ScopeTransaction | ScopeLocal | ScopeGlobal
    deriving (Eq, Ord, Show)

-- | Normalized value assigned to a session configuration parameter.
data SessionValue
    = SessionText !Text
    | SessionBool !Bool
    | SessionInteger !Integer
    | SessionNames ![Name]
    | SessionDefault
    deriving (Eq, Show)

-- | Session-state mutation extracted from @SET@, @USE@, @PRAGMA@, or authorization statements.
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
    | SetSqliteForeignKeys !Bool
    | -- | Restore foreign key checks to the session's initial setting.
      ResetForeignKeyChecks !SessionScope
    | ResetVariable !SessionScope !Name
    | ResetAll !SessionScope
    deriving (Eq, Show)

-- | Dialect policy when @BEGIN@ executes while a transaction is already active.
data BeginPolicy = BeginError | BeginWarn | BeginCommitActive | BeginIgnore
    deriving (Eq, Ord, Show)

-- | Transaction-control effect modeled during dependency analysis.
data TxEffect
    = TxBegin !BeginPolicy
    | TxCommit !Bool
    | TxRollbackAll
    | TxRollbackTo !Ident
    | TxSavepoint !Ident
    | TxReleaseSavepoint !Ident
    | TxSetReadOnly !Bool
    deriving (Eq, Show)

-- | Diagnostic reason categorizing why a statement introduced an ordering barrier.
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

-- | Reference to a named catalog object protected or invalidated by a 'BarrierScope'.
data ObjectRef
    = ObjectRelation !NameRef
    | ObjectIndex !IndexRef
    | ObjectTrigger !TriggerRef
    | ObjectSequence !NameRef
    | ObjectType !Name
    | ObjectSchema !Name
    | ObjectConstraint !ConstraintRef
    deriving (Eq, Ord, Show)

-- | Granularity at which a conservative ordering barrier restricts statement deletion or reordering.
data BarrierScope
    = GlobalScope
    | RelationScope !NameRef
    | SchemaScope !Name
    | ObjectScope !ObjectRef
    | SessionStateScope
    | TransactionStateScope
    | UserVariableScope !Ident
    deriving (Eq, Ord, Show)

-- | Ordering barrier paired with its diagnostic 'BarrierReason' and optional detail message.
data ScopedBarrier = ScopedBarrier
    { barrierScope :: !BarrierScope
    , barrierReason :: !BarrierReason
    , barrierDetail :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show)

-- | Complete static dependency facts extracted for one parsed SQL statement.
data SemanticFacts = SemanticFacts
    { factOps :: ![CatalogOp]
    , factSession :: ![SessionEffect]
    , factPreTx :: ![TxEffect]
    , factPostTx :: ![TxEffect]
    , factBarriers :: ![ScopedBarrier]
    }
    deriving (Eq, Show)

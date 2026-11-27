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
    | SetSqliteForeignKeys !Bool
    | -- | Restore foreign key checks to the session's initial setting.
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
    | UserVariableScope !Ident
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

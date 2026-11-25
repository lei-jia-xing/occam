-- | Semantic facts, session/transaction effects, triggers, and barriers with JSON codecs.
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

import Data.Aeson (
    FromJSON (..),
    Value (..),
    withObject,
    withText,
    (.:),
 )
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
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

-- ----------------------------------------------------------------------------
-- Native-parser JSON wire format
-- ----------------------------------------------------------------------------

instance FromJSON SemanticFacts where
    parseJSON = withObject "SemanticFacts" \objectValue ->
        SemanticFacts
            <$> objectValue .: "ops"
            <*> objectValue .: "session"
            <*> objectValue .: "pre_tx"
            <*> objectValue .: "post_tx"
            <*> objectValue .: "barriers"

instance FromJSON SearchPathEntry where
    parseJSON = withObject "SearchPathEntry" \objectValue -> do
        tag <- objectValue .: "entry"
        case tag of
            "name" -> SearchPathName <$> objectValue .: "name"
            "current_user" -> pure SearchPathCurrentUser
            _ -> fail ("unknown search path entry: " <> T.unpack tag)

instance FromJSON SearchPathValue where
    parseJSON = withObject "SearchPathValue" \objectValue -> do
        tag <- objectValue .: "path"
        case tag of
            "explicit" -> SearchPathExplicit <$> objectValue .: "entries"
            "default" -> pure SearchPathDefault
            _ -> fail ("unknown search path value: " <> T.unpack tag)

instance FromJSON SessionInit where
    parseJSON = withObject "SessionInit" \objectValue ->
        SessionInit
            <$> objectValue .: "namespace_prefix"
            <*> objectValue .: "search_path"
            <*> objectValue .: "current_user"
            <*> objectValue .: "autocommit"
            <*> objectValue .: "foreign_key_checks"
            <*> objectValue .: "variables"

instance FromJSON TriggerTiming where
    parseJSON = withText "TriggerTiming" \name ->
        case name of
            "before" -> pure TriggerBefore
            "after" -> pure TriggerAfter
            "instead_of" -> pure TriggerInsteadOf
            _ -> fail ("unknown trigger timing: " <> T.unpack name)

instance FromJSON TriggerEvent where
    parseJSON = withObject "TriggerEvent" \objectValue -> do
        tag <- objectValue .: "event"
        case tag of
            "insert" -> pure TriggerInsert
            "update" -> TriggerUpdate <$> objectValue .: "columns"
            "delete" -> pure TriggerDelete
            "truncate" -> pure TriggerTruncate
            _ -> fail ("unknown trigger event: " <> T.unpack tag)

instance FromJSON TriggerBody where
    parseJSON = withObject "TriggerBody" \objectValue -> do
        tag <- objectValue .: "body"
        case tag of
            "inline" -> TriggerInline <$> objectValue .: "effects"
            "function" -> TriggerFunction <$> objectValue .: "name" <*> objectValue .: "args"
            "opaque" -> pure TriggerOpaque
            _ -> fail ("unknown trigger body: " <> T.unpack tag)

instance FromJSON TriggerEffect where
    parseJSON = withObject "TriggerEffect" \objectValue -> do
        tag <- objectValue .: "kind"
        case tag of
            "dml" -> TriggerDml <$> objectValue .: "event"
            "read" -> TriggerRead <$> objectValue .: "read"
            "write_sequence" -> TriggerWriteSequence <$> objectValue .: "write"
            "session" -> TriggerSession <$> objectValue .: "effect"
            "barrier" -> TriggerBarrier <$> objectValue .: "barrier"
            _ -> fail ("unknown trigger effect: " <> T.unpack tag)

instance FromJSON TriggerDef where
    parseJSON = withObject "TriggerDef" \objectValue ->
        TriggerDef
            <$> objectValue .: "trigger"
            <*> objectValue .: "table"
            <*> objectValue .: "timing"
            <*> objectValue .: "events"
            <*> objectValue .: "for_each_row"
            <*> objectValue .: "when_reads"
            <*> objectValue .: "body"
            <*> objectValue .: "referenced_table"
            <*> objectValue .: "temporary"
            <*> objectValue .: "if_not_exists"
            <*> objectValue .: "or_replace"

instance FromJSON CatalogOp where
    parseJSON = withObject "CatalogOp" \objectValue -> do
        tag <- objectValue .: "op"
        case tag of
            "define_table" -> DefineTableOp <$> parseJSON (Object objectValue)
            "define_view" -> DefineViewOp <$> parseJSON (Object objectValue)
            "define_index" -> DefineIndexOp <$> parseJSON (Object objectValue)
            "define_trigger" -> DefineTriggerOp <$> parseJSON (Object objectValue)
            "define_sequence" -> DefineSequenceOp <$> parseJSON (Object objectValue)
            "define_type" -> DefineTypeOp <$> parseJSON (Object objectValue)
            "define_schema" -> DefineSchemaOp <$> parseJSON (Object objectValue)
            "read" -> ReadOp <$> parseJSON (Object objectValue)
            "write_data" -> WriteDataOp <$> parseJSON (Object objectValue)
            "write_sequence" -> WriteSequenceOp <$> parseJSON (Object objectValue)
            "alter_table" -> AlterTableOp <$> parseJSON (Object objectValue)
            "attach_partition" -> AttachPartitionOp <$> parseJSON (Object objectValue)
            "detach_partition" -> DetachPartitionOp <$> parseJSON (Object objectValue)
            "attach_inherit" -> AttachInheritOp <$> parseJSON (Object objectValue)
            "detach_inherit" -> DetachInheritOp <$> parseJSON (Object objectValue)
            "drop" -> DropCatalogOp <$> parseJSON (Object objectValue)
            "rename" -> RenameOp <$> parseJSON (Object objectValue)
            "refresh" -> RefreshCatalogOp <$> parseJSON (Object objectValue)
            _ -> fail ("unknown catalog operation: " <> T.unpack tag)

instance FromJSON SessionScope where
    parseJSON = withText "SessionScope" \name ->
        case name of
            "session" -> pure ScopeSession
            "transaction" -> pure ScopeTransaction
            "local" -> pure ScopeLocal
            "global" -> pure ScopeGlobal
            _ -> fail ("unknown session scope: " <> T.unpack name)

instance FromJSON SessionValue where
    parseJSON = withObject "SessionValue" \objectValue -> do
        tag <- objectValue .: "kind"
        case tag of
            "text" -> SessionText <$> objectValue .: "value"
            "bool" -> SessionBool <$> objectValue .: "value"
            "integer" -> SessionInteger <$> objectValue .: "value"
            "names" -> SessionNames <$> objectValue .: "value"
            "default" -> pure SessionDefault
            _ -> fail ("unknown session value: " <> T.unpack tag)

instance FromJSON SessionEffect where
    parseJSON = withObject "SessionEffect" \objectValue -> do
        tag <- objectValue .: "effect"
        case tag of
            "set_variable" ->
                SetVariable
                    <$> objectValue .: "scope"
                    <*> objectValue .: "name"
                    <*> objectValue .: "value"
            "set_search_path" ->
                SetSearchPath
                    <$> objectValue .: "scope"
                    <*> objectValue .: "path"
            "set_namespace" ->
                SetNamespace
                    <$> objectValue .: "scope"
                    <*> objectValue .: "namespace"
            "set_session_user" ->
                SetSessionUser
                    <$> objectValue .: "scope"
                    <*> objectValue .: "user"
            "reset_session_user" -> ResetSessionUser <$> objectValue .: "scope"
            "set_current_user" ->
                SetCurrentUser
                    <$> objectValue .: "scope"
                    <*> objectValue .: "user"
            "reset_current_user" -> ResetCurrentUser <$> objectValue .: "scope"
            "set_autocommit" ->
                SetAutocommit
                    <$> objectValue .: "scope"
                    <*> objectValue .: "value"
            "set_foreign_key_checks" ->
                SetForeignKeyChecks
                    <$> objectValue .: "scope"
                    <*> objectValue .: "value"
            "set_sqlite_foreign_keys" -> SetSqliteForeignKeys <$> objectValue .: "value"
            "reset_foreign_key_checks" ->
                ResetForeignKeyChecks <$> objectValue .: "scope"
            "reset_variable" ->
                ResetVariable
                    <$> objectValue .: "scope"
                    <*> objectValue .: "name"
            "reset_all" -> ResetAll <$> objectValue .: "scope"
            _ -> fail ("unknown session effect: " <> T.unpack tag)

instance FromJSON BeginPolicy where
    parseJSON = withText "BeginPolicy" \name ->
        case name of
            "error" -> pure BeginError
            "warn" -> pure BeginWarn
            "commit_active" -> pure BeginCommitActive
            "ignore" -> pure BeginIgnore
            _ -> fail ("unknown BEGIN policy: " <> T.unpack name)

instance FromJSON TxEffect where
    parseJSON = withObject "TxEffect" \objectValue -> do
        tag <- objectValue .: "effect"
        case tag of
            "begin" -> TxBegin <$> objectValue .: "if_active"
            "commit" -> TxCommit <$> objectValue .: "chain"
            "rollback_all" -> pure TxRollbackAll
            "rollback_to" -> TxRollbackTo <$> objectValue .: "savepoint"
            "savepoint" -> TxSavepoint <$> objectValue .: "name"
            "release_savepoint" -> TxReleaseSavepoint <$> objectValue .: "name"
            "set_read_only" -> TxSetReadOnly <$> objectValue .: "value"
            _ -> fail ("unknown transaction effect: " <> T.unpack tag)

instance FromJSON BarrierReason where
    parseJSON = withText "BarrierReason" \name ->
        case name of
            "syntax_error" -> pure SyntaxErrorBarrier
            "unknown_statement" -> pure UnknownStatementBarrier
            "unknown_relation" -> pure UnknownRelationBarrier
            "unknown_ddl" -> pure UnknownDDLBarrier
            "unknown_dml" -> pure UnknownDMLBarrier
            "unknown_drop" -> pure UnknownDropBarrier
            "unknown_alter" -> pure UnknownAlterBarrier
            "unknown_view" -> pure UnknownViewBarrier
            "unknown_index" -> pure UnknownIndexBarrier
            "unknown_trigger" -> pure UnknownTriggerBarrier
            "unknown_truncate" -> pure UnknownTruncateBarrier
            "unknown_session_effect" -> pure UnknownSessionEffectBarrier
            "unsupported_expression" -> pure UnsupportedExpressionBarrier
            "unsupported_conflict_clause" -> pure UnsupportedConflictClauseBarrier
            "unsupported_trigger_body" -> pure UnsupportedTriggerBodyBarrier
            "unsupported_transaction" -> pure UnsupportedTransactionBarrier
            "unresolved_view_source" -> pure UnresolvedViewSourceBarrier
            "write_through_view" -> pure WriteThroughViewBarrier
            "opaque_trigger_body" -> pure OpaqueTriggerBodyBarrier
            "unmodeled_relation_source" -> pure UnmodeledRelationSourceBarrier
            _ -> fail ("unknown barrier reason: " <> T.unpack name)

instance FromJSON ObjectRef where
    parseJSON = withObject "ObjectRef" \objectValue -> do
        tag <- objectValue .: "object"
        case tag of
            "relation" -> ObjectRelation <$> objectValue .: "relation"
            "index" -> ObjectIndex <$> objectValue .: "index"
            "trigger" -> ObjectTrigger <$> objectValue .: "trigger"
            "sequence" -> ObjectSequence <$> objectValue .: "sequence"
            "type" -> ObjectType <$> objectValue .: "name"
            "schema" -> ObjectSchema <$> objectValue .: "name"
            "constraint" -> ObjectConstraint <$> objectValue .: "constraint"
            _ -> fail ("unknown object reference: " <> T.unpack tag)

instance FromJSON BarrierScope where
    parseJSON = withObject "BarrierScope" \objectValue -> do
        tag <- objectValue .: "scope"
        case tag of
            "global" -> pure GlobalScope
            "relation" -> RelationScope <$> objectValue .: "relation"
            "schema" -> SchemaScope <$> objectValue .: "schema"
            "object" -> ObjectScope <$> objectValue .: "object"
            "session_state" -> pure SessionStateScope
            "transaction_state" -> pure TransactionStateScope
            _ -> fail ("unknown barrier scope: " <> T.unpack tag)

instance FromJSON ScopedBarrier where
    parseJSON = withObject "ScopedBarrier" \objectValue ->
        ScopedBarrier
            <$> objectValue .: "scope"
            <*> objectValue .: "reason"
            <*> objectValue .: "detail"

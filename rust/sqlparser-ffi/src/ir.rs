use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParseConfig {
    pub dialect: crate::DialectKind,
    #[serde(default)]
    pub server: ServerConfig,
}

/// Server/session configuration that is independent of a particular SQL text.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(default)]
pub struct ServerConfig {
    /// Current database/schema namespace, e.g. MySQL `USE db`. Each element is
    /// one namespace part; a part may be double-quoted to preserve case.
    pub namespace_prefix: Vec<String>,
    /// Ordered schema search path. Each element is one schema entry; elements
    /// are not split on commas. A single element may be double-quoted to
    /// preserve case. An empty path means "unknown" and is treated
    /// conservatively by the graph builder.
    pub search_path: Vec<String>,
    /// Initial current user. An unquoted name follows the dialect's relation
    /// folding; a double-quoted name preserves case.
    pub current_user: Option<String>,
    #[serde(default = "default_true")]
    pub autocommit: bool,
    /// Initial `foreign_key_checks`/`foreign_keys` session setting. `None`
    /// uses the dialect's server default.
    pub foreign_key_checks: Option<bool>,
    pub variables: BTreeMap<String, String>,
    /// MySQL `lower_case_table_names`: 0 = case-sensitive, 1 = stored lower,
    /// 2 = stored as written but compared lower-case.
    pub lower_case_table_names: u8,
    /// Explicit per-class overrides. `None` uses the dialect default.
    pub table_names_case_sensitive: Option<bool>,
    pub column_names_case_sensitive: Option<bool>,
    pub quoted_identifiers_case_sensitive: Option<bool>,
}

fn default_true() -> bool {
    true
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            namespace_prefix: Vec::new(),
            search_path: Vec::new(),
            current_user: None,
            autocommit: true,
            foreign_key_checks: None,
            variables: BTreeMap::new(),
            lower_case_table_names: 0,
            table_names_case_sensitive: None,
            column_names_case_sensitive: None,
            quoted_identifiers_case_sensitive: None,
        }
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[serde(transparent)]
pub struct Ident(pub String);

#[derive(Clone, Debug, Default, Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[serde(transparent)]
pub struct Name(pub Vec<Ident>);

impl Name {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn last(&self) -> Option<&Ident> {
        self.0.last()
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct NameRef {
    pub name: Name,
    pub temp: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(tag = "entry", rename_all = "snake_case")]
pub enum SearchPathEntry {
    Name { name: Name },
    CurrentUser,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(tag = "path", rename_all = "snake_case")]
pub enum SearchPathValue {
    Explicit { entries: Vec<SearchPathEntry> },
    Default,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SessionInit {
    pub namespace_prefix: Vec<Ident>,
    pub search_path: Vec<SearchPathEntry>,
    pub current_user: Option<Ident>,
    pub autocommit: bool,
    /// Concrete initial value after applying the dialect's server default.
    pub foreign_key_checks: bool,
    pub variables: BTreeMap<String, String>,
}

#[derive(Clone, Debug, Serialize, Default)]
pub struct NativeFacts {
    pub ops: Vec<CatalogOp>,
    pub session: Vec<SessionEffect>,
    pub pre_tx: Vec<TxEffect>,
    pub post_tx: Vec<TxEffect>,
    pub barriers: Vec<ScopedBarrier>,
    pub edits: Vec<crate::NativeEdit>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum CatalogOp {
    DefineTable(TableDef),
    DefineView(ViewDef),
    DefineIndex(IndexDef),
    DefineTrigger(TriggerDef),
    DefineSequence(SequenceDef),
    DefineType(TypeDef),
    DefineSchema(SchemaDef),
    Read(ReadFact),
    WriteData(DataWrite),
    WriteSequence(SequenceWrite),
    AlterTable(AlterTable),
    Drop(DropOp),
    Rename(RenameOp),
    Refresh(RefreshOp),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TableDef {
    pub name: Name,
    pub temporary: bool,
    pub transactional: bool,
    pub columns: Vec<ColumnDef>,
    pub constraints: Vec<Constraint>,
    pub sources: Vec<ReadFact>,
    pub like: Option<NameRef>,
    pub on_commit: Option<OnCommitAction>,
    pub if_not_exists: bool,
    pub or_replace: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ColumnDef {
    pub name: Ident,
    pub not_null: bool,
    pub has_default: bool,
    pub generated: bool,
    pub references: Vec<ForeignKey>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "constraint", rename_all = "snake_case")]
pub enum Constraint {
    PrimaryKey {
        name: Option<Ident>,
        columns: Vec<Ident>,
    },
    Unique {
        name: Option<Ident>,
        columns: Vec<Ident>,
    },
    ForeignKey(ForeignKey),
    Check {
        name: Option<Ident>,
        columns: Vec<Ident>,
        not_valid: bool,
    },
    Index {
        name: Option<Ident>,
        columns: Vec<Ident>,
        unique: bool,
    },
    /// PostgreSQL `PRIMARY KEY USING INDEX` / `UNIQUE USING INDEX`.
    /// The index is resolved to its columns by the graph builder.
    UsingIndex {
        name: Option<Ident>,
        index: Ident,
        primary: bool,
        /// Where an unqualified index name is resolved.
        schema_placement: IndexSchemaPlacement,
    },
    Other {
        name: Option<Ident>,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ForeignKey {
    pub name: Option<Ident>,
    pub child_columns: Vec<Ident>,
    pub parent: NameRef,
    pub parent_columns: Vec<Ident>,
    pub on_delete: ReferentialAction,
    pub on_update: ReferentialAction,
    pub not_valid: bool,
    pub deferrable: bool,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ReferentialAction {
    NoAction,
    Restrict,
    Cascade,
    SetNull,
    SetDefault,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum OnCommitAction {
    DeleteRows,
    Drop,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ViewDef {
    pub name: Name,
    pub temporary: bool,
    pub materialized: bool,
    pub columns: Vec<Ident>,
    pub sources: Vec<ReadFact>,
    pub if_not_exists: bool,
    pub or_replace: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IndexDef {
    pub index: IndexRef,
    pub table: NameRef,
    pub columns: Vec<Ident>,
    pub unique: bool,
    pub predicate: bool,
    pub expression: bool,
    pub if_not_exists: bool,
    /// Where an unqualified schema-scoped index name lives.
    pub schema_placement: IndexSchemaPlacement,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum IndexSchemaPlacement {
    /// The index is created in the schema of the indexed table (PostgreSQL,
    /// SQLite, DuckDB, Redshift).
    TableSchema,
    /// The index is created in the current schema/user schema (Oracle).
    CurrentSchema,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[serde(tag = "scope", rename_all = "snake_case")]
pub enum IndexRef {
    Named { name: Name },
    Table { table: NameRef, name: Ident },
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[serde(tag = "scope", rename_all = "snake_case")]
pub enum TriggerRef {
    Named { name: Name },
    Table { table: NameRef, name: Ident },
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ConstraintRef {
    pub table: NameRef,
    pub name: Ident,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TriggerDef {
    pub trigger: TriggerRef,
    pub table: NameRef,
    pub timing: TriggerTiming,
    pub events: Vec<TriggerEvent>,
    pub for_each_row: bool,
    /// Reads performed by the trigger's `WHEN` condition. The condition may
    /// suppress firing; the graph conservatively assumes the trigger fires.
    pub when_reads: Vec<ReadFact>,
    pub body: TriggerBody,
    pub referenced_table: Option<NameRef>,
    pub temporary: bool,
    pub if_not_exists: bool,
    pub or_replace: bool,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum TriggerTiming {
    Before,
    After,
    InsteadOf,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum TriggerEvent {
    Insert,
    Update { columns: Vec<Ident> },
    Delete,
    Truncate,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "body", rename_all = "snake_case")]
pub enum TriggerBody {
    Inline { effects: Vec<TriggerEffect> },
    Function { name: Name, args: Vec<String> },
    Opaque,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TriggerEffect {
    Dml { event: DmlEvent },
    Read { read: ReadFact },
    WriteSequence { write: SequenceWrite },
    Session { effect: SessionEffect },
    Barrier { barrier: ScopedBarrier },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SequenceDef {
    pub name: Name,
    pub temporary: bool,
    pub transactional: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TypeDef {
    pub name: Name,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SchemaDef {
    pub name: Name,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ReadMode {
    Data,
    Schema,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReadFact {
    pub source: NameRef,
    pub mode: ReadMode,
    pub columns: Vec<Ident>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DataWrite {
    pub target: NameRef,
    pub kind: WriteKind,
    pub columns: Vec<Ident>,
    /// Assigned columns without a table qualifier. With multiple UPDATE
    /// targets, the database resolves these against the target schemas; the
    /// graph builder drops candidates that do not own the column.
    #[serde(default)]
    pub unqualified_columns: Vec<Ident>,
    pub reads: Vec<ReadFact>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum WriteKind {
    Insert {
        conflict: ConflictAction,
    },
    Update,
    Delete {
        all: bool,
    },
    Merge,
    Truncate {
        cascade: bool,
        restart_identity: bool,
    },
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ConflictAction {
    Error,
    Ignore,
    Replace,
    Update,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SequenceWrite {
    pub sequence: NameRef,
    pub transactional: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AlterTable {
    pub table: NameRef,
    pub changes: Vec<AlterChange>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "change", rename_all = "snake_case")]
pub enum AlterChange {
    AddColumn {
        column: ColumnDef,
        /// Column-level PRIMARY KEY/UNIQUE/CHECK constraints parsed from the
        /// column definition, in source order.
        constraints: Vec<Constraint>,
    },
    DropColumn {
        column: Ident,
        cascade: bool,
    },
    AddConstraint {
        constraint: Constraint,
    },
    DropConstraint {
        name: Ident,
        cascade: bool,
    },
    RenameConstraint {
        from: Ident,
        to: Ident,
    },
    ValidateConstraint {
        name: Ident,
    },
    DropIndex {
        name: Ident,
    },
    AlterColumn {
        column: Ident,
        detail: String,
    },
    Other {
        detail: String,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DropOp {
    pub objects: Vec<DropObject>,
    pub mode: DropMode,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum DropMode {
    Restrict,
    Cascade,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(tag = "object", rename_all = "snake_case")]
pub enum DropObject {
    Relation { name: NameRef, kind: RelationKind },
    Index { index: IndexRef },
    Trigger { trigger: TriggerRef },
    Sequence { name: NameRef },
    Type { name: Name },
    Schema { name: Name },
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum RelationKind {
    Table,
    View,
    MaterializedView,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(tag = "rename", rename_all = "snake_case")]
pub enum RenameOp {
    Relation {
        from: NameRef,
        to: Name,
        /// Where an unqualified index owned by the table is resolved.
        index_schema_placement: IndexSchemaPlacement,
    },
    Column {
        table: NameRef,
        from: Ident,
        to: Ident,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RefreshOp {
    pub target: NameRef,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum DmlEvent {
    Insert {
        target: NameRef,
        columns: Vec<Ident>,
    },
    Update {
        target: NameRef,
        columns: Vec<Ident>,
    },
    Merge {
        target: NameRef,
        columns: Vec<Ident>,
    },
    Delete {
        target: NameRef,
        all: bool,
    },
    Truncate {
        target: NameRef,
        cascade: bool,
    },
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum SessionScope {
    Session,
    Transaction,
    Local,
    /// Server-wide setting. It does not mutate the current session's modeled
    /// state, but it is still a session-state fact for dependency tracking.
    Global,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "effect", rename_all = "snake_case")]
pub enum SessionEffect {
    SetVariable {
        scope: SessionScope,
        name: Name,
        value: SessionValue,
    },
    SetSearchPath {
        scope: SessionScope,
        path: SearchPathValue,
    },
    SetNamespace {
        scope: SessionScope,
        namespace: Vec<Ident>,
    },
    /// `SET SESSION AUTHORIZATION`: changes session_user and current_user.
    SetSessionUser {
        scope: SessionScope,
        user: Ident,
    },
    /// `RESET SESSION AUTHORIZATION`: restores both to the authenticated user.
    ResetSessionUser {
        scope: SessionScope,
    },
    /// `SET ROLE`: changes current_user only.
    SetCurrentUser {
        scope: SessionScope,
        user: Ident,
    },
    /// `RESET ROLE`: restores current_user from session_user.
    ResetCurrentUser {
        scope: SessionScope,
    },
    SetAutocommit {
        scope: SessionScope,
        value: bool,
    },
    SetForeignKeyChecks {
        scope: SessionScope,
        value: bool,
    },
    /// Restore the session's initial `foreign_keys`/`foreign_key_checks`
    /// configuration. The initial value is dialect- and server-dependent.
    ResetForeignKeyChecks {
        scope: SessionScope,
    },
    ResetVariable {
        scope: SessionScope,
        name: Name,
    },
    ResetAll {
        scope: SessionScope,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SessionValue {
    Text { value: String },
    Bool { value: bool },
    Integer { value: i64 },
    Names { value: Vec<Name> },
    Default,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum BeginPolicy {
    Error,
    Warn,
    CommitActive,
    Ignore,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "effect", rename_all = "snake_case")]
pub enum TxEffect {
    Begin { if_active: BeginPolicy },
    Commit { chain: bool },
    RollbackAll,
    RollbackTo { savepoint: Ident },
    Savepoint { name: Ident },
    ReleaseSavepoint { name: Ident },
    SetReadOnly { value: bool },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ScopedBarrier {
    pub scope: BarrierScope,
    pub reason: BarrierReason,
    pub detail: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "scope", rename_all = "snake_case")]
pub enum BarrierScope {
    Global,
    Relation { relation: NameRef },
    Schema { schema: Name },
    Object { object: ObjectRef },
    SessionState,
    TransactionState,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "object", rename_all = "snake_case")]
pub enum ObjectRef {
    Relation { relation: NameRef },
    Index { index: IndexRef },
    Trigger { trigger: TriggerRef },
    Sequence { sequence: NameRef },
    Type { name: Name },
    Schema { name: Name },
    Constraint { constraint: ConstraintRef },
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum BarrierReason {
    SyntaxError,
    UnknownStatement,
    UnknownRelation,
    UnknownDdl,
    UnknownDml,
    UnknownDrop,
    UnknownAlter,
    UnknownView,
    UnknownIndex,
    UnknownTrigger,
    UnknownTruncate,
    UnknownSessionEffect,
    UnsupportedExpression,
    UnsupportedConflictClause,
    UnsupportedTriggerBody,
    UnsupportedTransaction,
    UnresolvedViewSource,
    WriteThroughView,
    OpaqueTriggerBody,
    UnmodeledRelationSource,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum IdentifierClass {
    Relation,
    Column,
    Other,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CaseFolding {
    Lower,
    Upper,
    Preserve,
}

#[derive(Clone, Debug)]
pub struct IdentifierRules {
    pub unquoted: CaseFolding,
    pub quoted: CaseFolding,
    pub relation_unquoted: Option<CaseFolding>,
    pub relation_quoted: Option<CaseFolding>,
    pub column_unquoted: Option<CaseFolding>,
    pub column_quoted: Option<CaseFolding>,
}

impl IdentifierRules {
    pub fn for_dialect(dialect: crate::DialectKind, server: &ServerConfig) -> Self {
        use crate::DialectKind::*;
        let (unquoted, quoted) = match dialect {
            PostgreSql | Redshift => (CaseFolding::Lower, CaseFolding::Preserve),
            Oracle | Ansi | Snowflake | Teradata => (CaseFolding::Upper, CaseFolding::Preserve),
            ClickHouse => (CaseFolding::Preserve, CaseFolding::Preserve),
            // These dialects are case-preserving but compare identifiers
            // case-insensitively, so folding both classes to lower-case models
            // their matching semantics.
            DuckDB | MsSql | SQLite | BigQuery | Hive | Spark | Databricks | MySql | Generic => {
                (CaseFolding::Lower, CaseFolding::Lower)
            }
        };
        let mut rules = Self {
            unquoted,
            quoted,
            relation_unquoted: None,
            relation_quoted: None,
            column_unquoted: None,
            column_quoted: None,
        };
        if dialect == BigQuery {
            rules.relation_unquoted = Some(CaseFolding::Preserve);
            rules.relation_quoted = Some(CaseFolding::Preserve);
            rules.column_unquoted = Some(CaseFolding::Lower);
            rules.column_quoted = Some(CaseFolding::Lower);
        }
        if dialect == MySql {
            let relation = match server.lower_case_table_names {
                0 => CaseFolding::Preserve,
                _ => CaseFolding::Lower,
            };
            rules.relation_unquoted = Some(relation);
            rules.relation_quoted = Some(relation);
        }
        if let Some(case_sensitive) = server.table_names_case_sensitive {
            let folding = if case_sensitive {
                CaseFolding::Preserve
            } else {
                CaseFolding::Lower
            };
            rules.relation_unquoted = Some(folding);
            rules.relation_quoted = Some(folding);
        }
        if let Some(case_sensitive) = server.column_names_case_sensitive {
            let folding = if case_sensitive {
                CaseFolding::Preserve
            } else {
                CaseFolding::Lower
            };
            rules.column_unquoted = Some(folding);
            rules.column_quoted = Some(folding);
        }
        if let Some(case_sensitive) = server.quoted_identifiers_case_sensitive {
            rules.quoted = if case_sensitive {
                CaseFolding::Preserve
            } else {
                CaseFolding::Lower
            };
        }
        rules
    }

    pub fn fold(&self, class: IdentifierClass, quoted: bool) -> CaseFolding {
        let specific = match (class, quoted) {
            (IdentifierClass::Relation, true) => self.relation_quoted,
            (IdentifierClass::Relation, false) => self.relation_unquoted,
            (IdentifierClass::Column, true) => self.column_quoted,
            (IdentifierClass::Column, false) => self.column_unquoted,
            (IdentifierClass::Other, _) => None,
        };
        specific.unwrap_or(if quoted { self.quoted } else { self.unquoted })
    }
}

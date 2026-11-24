//! Binary serialization protocol for FFI data exchange between Rust and Occam (Haskell).
//!
//! Encodes [`NativeResponse`] into a compact binary format starting with the `OCCM\x02` magic
//! header, followed by initial session state, parsed statement units (including full binary
//! semantic facts and candidate syntactic edits), and atomic column reduction targets.

use crate::columns::{ColumnSite, ColumnSiteKind, ColumnTarget};
use crate::ir::{
    AlterChange, AlterTable, BarrierReason, BarrierScope, BeginPolicy, CatalogOp, ColumnDef,
    ConflictAction, Constraint, ConstraintRef, DataWrite, DmlEvent, DropMode, DropObject, DropOp,
    ForeignKey, Ident, IndexDef, IndexRef, IndexSchemaPlacement, Name, NameRef, NativeFacts,
    ObjectRef, OnCommitAction, PartitionLink, ReadFact, ReadMode, ReferentialAction, RefreshOp,
    RelationKind, RenameOp, ScopedBarrier, SearchPathEntry, SearchPathValue, SequenceWrite,
    SessionEffect, SessionInit, SessionScope, SessionValue, SourceColumn, TableDef, TriggerBody,
    TriggerDef, TriggerEffect, TriggerEvent, TriggerRef, TriggerTiming, TxEffect,
    ViewColumnLineage, ViewDef, WriteKind,
};
use crate::{NativeEdit, NativeEditCategory, NativeResponse, NativeUnit};

/// 5-byte magic header prefix (`b"OCCM\x02"`) identifying the Occam binary response format version 2.
pub const OCCM_MAGIC: &[u8; 5] = b"OCCM\x02";

/// A byte buffer builder for serializing primitive types and structured payloads in little-endian order.
pub struct BinaryWriter {
    buf: Vec<u8>,
}

impl Default for BinaryWriter {
    fn default() -> Self {
        Self::new()
    }
}

impl BinaryWriter {
    /// Creates a new `BinaryWriter` preallocated with initial capacity.
    pub fn new() -> Self {
        Self {
            buf: Vec::with_capacity(4096),
        }
    }

    #[inline]
    pub fn write_u8(&mut self, val: u8) {
        self.buf.push(val);
    }

    #[inline]
    pub fn write_bool(&mut self, val: bool) {
        self.buf.push(if val { 1 } else { 0 });
    }

    #[inline]
    pub fn write_u32(&mut self, val: u32) {
        self.buf.extend_from_slice(&val.to_le_bytes());
    }

    #[inline]
    pub fn write_i64(&mut self, val: i64) {
        self.buf.extend_from_slice(&val.to_le_bytes());
    }

    #[inline]
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.write_u32(bytes.len() as u32);
        self.buf.extend_from_slice(bytes);
    }

    #[inline]
    pub fn write_str(&mut self, s: &str) {
        self.write_bytes(s.as_bytes());
    }

    #[inline]
    pub fn write_opt_str(&mut self, opt: Option<&str>) {
        match opt {
            None => self.write_u8(0),
            Some(s) => {
                self.write_u8(1);
                self.write_str(s);
            }
        }
    }

    #[inline]
    pub fn write_vec<T>(&mut self, items: &[T], mut f: impl FnMut(&mut Self, &T)) {
        self.write_u32(items.len() as u32);
        for item in items {
            f(self, item);
        }
    }

    #[inline]
    pub fn write_opt<T>(&mut self, opt: Option<&T>, f: impl FnOnce(&mut Self, &T)) {
        match opt {
            None => self.write_u8(0),
            Some(val) => {
                self.write_u8(1);
                f(self, val);
            }
        }
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.buf
    }
}

fn write_ident(w: &mut BinaryWriter, ident: &Ident) {
    w.write_str(&ident.0);
}

fn write_name(w: &mut BinaryWriter, name: &Name) {
    w.write_vec(&name.0, write_ident);
}

fn write_name_ref(w: &mut BinaryWriter, nr: &NameRef) {
    write_name(w, &nr.name);
    w.write_bool(nr.temp);
}

fn write_search_path_entry(w: &mut BinaryWriter, entry: &SearchPathEntry) {
    match entry {
        SearchPathEntry::CurrentUser => {
            w.write_u8(0);
        }
        SearchPathEntry::Name { name } => {
            w.write_u8(1);
            write_name(w, name);
        }
    }
}

fn write_search_path_value(w: &mut BinaryWriter, path: &SearchPathValue) {
    match path {
        SearchPathValue::Default => w.write_u8(0),
        SearchPathValue::Explicit { entries } => {
            w.write_u8(1);
            w.write_vec(entries, write_search_path_entry);
        }
    }
}

fn write_session_init(w: &mut BinaryWriter, session: &SessionInit) {
    w.write_vec(&session.namespace_prefix, write_ident);
    w.write_vec(&session.search_path, write_search_path_entry);
    w.write_opt(session.current_user.as_ref(), write_ident);
    w.write_bool(session.autocommit);
    w.write_bool(session.foreign_key_checks);

    w.write_u32(session.variables.len() as u32);
    for (k, v) in &session.variables {
        w.write_str(k);
        w.write_str(v);
    }
}

fn write_read_mode(w: &mut BinaryWriter, mode: ReadMode) {
    w.write_u8(match mode {
        ReadMode::Data => 0,
        ReadMode::Schema => 1,
        ReadMode::RowCount => 2,
        ReadMode::RowExistence => 3,
    });
}

fn write_read_fact(w: &mut BinaryWriter, fact: &ReadFact) {
    write_name_ref(w, &fact.source);
    write_read_mode(w, fact.mode);
    w.write_vec(&fact.columns, write_ident);
    w.write_bool(fact.only);
}

fn write_relation_kind(w: &mut BinaryWriter, kind: RelationKind) {
    w.write_u8(match kind {
        RelationKind::Table => 0,
        RelationKind::View => 1,
        RelationKind::MaterializedView => 2,
    });
}

fn write_referential_action(w: &mut BinaryWriter, action: ReferentialAction) {
    w.write_u8(match action {
        ReferentialAction::NoAction => 0,
        ReferentialAction::Restrict => 1,
        ReferentialAction::Cascade => 2,
        ReferentialAction::SetNull => 3,
        ReferentialAction::SetDefault => 4,
    });
}

fn write_on_commit_action(w: &mut BinaryWriter, action: OnCommitAction) {
    w.write_u8(match action {
        OnCommitAction::DeleteRows => 0,
        OnCommitAction::Drop => 1,
    });
}

fn write_index_schema_placement(w: &mut BinaryWriter, placement: IndexSchemaPlacement) {
    w.write_u8(match placement {
        IndexSchemaPlacement::TableSchema => 0,
        IndexSchemaPlacement::CurrentSchema => 1,
    });
}

fn write_foreign_key(w: &mut BinaryWriter, fk: &ForeignKey) {
    w.write_opt(fk.name.as_ref(), write_ident);
    w.write_vec(&fk.child_columns, write_ident);
    write_name_ref(w, &fk.parent);
    w.write_vec(&fk.parent_columns, write_ident);
    write_referential_action(w, fk.on_delete);
    write_referential_action(w, fk.on_update);
    w.write_bool(fk.not_valid);
    w.write_bool(fk.deferrable);
}

fn write_constraint(w: &mut BinaryWriter, c: &Constraint) {
    match c {
        Constraint::PrimaryKey { name, columns } => {
            w.write_u8(0);
            w.write_opt(name.as_ref(), write_ident);
            w.write_vec(columns, write_ident);
        }
        Constraint::Unique { name, columns } => {
            w.write_u8(1);
            w.write_opt(name.as_ref(), write_ident);
            w.write_vec(columns, write_ident);
        }
        Constraint::ForeignKey(fk) => {
            w.write_u8(2);
            write_foreign_key(w, fk);
        }
        Constraint::Check {
            name,
            columns,
            not_valid,
        } => {
            w.write_u8(3);
            w.write_opt(name.as_ref(), write_ident);
            w.write_vec(columns, write_ident);
            w.write_bool(*not_valid);
        }
        Constraint::Index {
            name,
            columns,
            unique,
        } => {
            w.write_u8(4);
            w.write_opt(name.as_ref(), write_ident);
            w.write_vec(columns, write_ident);
            w.write_bool(*unique);
        }
        Constraint::UsingIndex {
            name,
            index,
            primary,
            schema_placement,
        } => {
            w.write_u8(5);
            w.write_opt(name.as_ref(), write_ident);
            write_ident(w, index);
            w.write_bool(*primary);
            write_index_schema_placement(w, *schema_placement);
        }
        Constraint::Other { name } => {
            w.write_u8(6);
            w.write_opt(name.as_ref(), write_ident);
        }
    }
}

fn write_column_def(w: &mut BinaryWriter, col: &ColumnDef) {
    write_ident(w, &col.name);
    w.write_bool(col.not_null);
    w.write_bool(col.has_default);
    w.write_bool(col.generated);
    w.write_vec(&col.references, write_foreign_key);
}

fn write_table_def(w: &mut BinaryWriter, table: &TableDef) {
    write_name(w, &table.name);
    w.write_bool(table.temporary);
    w.write_bool(table.transactional);
    w.write_vec(&table.columns, write_column_def);
    w.write_vec(&table.constraints, write_constraint);
    w.write_vec(&table.sources, write_read_fact);
    w.write_opt(table.like.as_ref(), write_name_ref);
    w.write_opt(table.partition_parent.as_ref(), write_name_ref);
    w.write_vec(&table.inherit_parents, write_name_ref);
    w.write_opt(table.on_commit.as_ref(), |w, a| {
        write_on_commit_action(w, *a)
    });
    w.write_bool(table.if_not_exists);
    w.write_bool(table.or_replace);
}

fn write_source_column(w: &mut BinaryWriter, sc: &SourceColumn) {
    write_name_ref(w, &sc.source);
    write_ident(w, &sc.column);
}

fn write_view_column_lineage(w: &mut BinaryWriter, lin: &ViewColumnLineage) {
    write_ident(w, &lin.column);
    w.write_opt(lin.sources.as_ref(), |w, srcs| {
        w.write_vec(srcs, write_source_column);
    });
}

fn write_view_def(w: &mut BinaryWriter, view: &ViewDef) {
    write_name(w, &view.name);
    w.write_bool(view.temporary);
    w.write_bool(view.materialized);
    w.write_vec(&view.columns, write_ident);
    w.write_vec(&view.sources, write_read_fact);
    w.write_bool(view.if_not_exists);
    w.write_bool(view.or_replace);
    w.write_vec(&view.column_lineage, write_view_column_lineage);
    w.write_vec(&view.condition_columns, write_source_column);
}

fn write_index_ref(w: &mut BinaryWriter, iref: &IndexRef) {
    match iref {
        IndexRef::Named { name } => {
            w.write_u8(0);
            write_name(w, name);
        }
        IndexRef::Table { table, name } => {
            w.write_u8(1);
            write_name_ref(w, table);
            write_ident(w, name);
        }
    }
}

fn write_index_def(w: &mut BinaryWriter, idx: &IndexDef) {
    write_index_ref(w, &idx.index);
    write_name_ref(w, &idx.table);
    w.write_vec(&idx.columns, write_ident);
    w.write_bool(idx.unique);
    w.write_bool(idx.predicate);
    w.write_bool(idx.expression);
    w.write_bool(idx.if_not_exists);
    write_index_schema_placement(w, idx.schema_placement);
}

fn write_trigger_ref(w: &mut BinaryWriter, tref: &TriggerRef) {
    match tref {
        TriggerRef::Named { name } => {
            w.write_u8(0);
            write_name(w, name);
        }
        TriggerRef::Table { table, name } => {
            w.write_u8(1);
            write_name_ref(w, table);
            write_ident(w, name);
        }
    }
}

fn write_constraint_ref(w: &mut BinaryWriter, cref: &ConstraintRef) {
    write_name_ref(w, &cref.table);
    write_ident(w, &cref.name);
}

fn write_trigger_timing(w: &mut BinaryWriter, timing: TriggerTiming) {
    w.write_u8(match timing {
        TriggerTiming::Before => 0,
        TriggerTiming::After => 1,
        TriggerTiming::InsteadOf => 2,
    });
}

fn write_trigger_event(w: &mut BinaryWriter, event: &TriggerEvent) {
    match event {
        TriggerEvent::Insert => w.write_u8(0),
        TriggerEvent::Update { columns } => {
            w.write_u8(1);
            w.write_vec(columns, write_ident);
        }
        TriggerEvent::Delete => w.write_u8(2),
        TriggerEvent::Truncate => w.write_u8(3),
    }
}

fn write_dml_event(w: &mut BinaryWriter, event: &DmlEvent) {
    match event {
        DmlEvent::Insert { target, columns } => {
            w.write_u8(0);
            write_name_ref(w, target);
            w.write_vec(columns, write_ident);
        }
        DmlEvent::Update { target, columns } => {
            w.write_u8(1);
            write_name_ref(w, target);
            w.write_vec(columns, write_ident);
        }
        DmlEvent::Merge { target, columns } => {
            w.write_u8(2);
            write_name_ref(w, target);
            w.write_vec(columns, write_ident);
        }
        DmlEvent::Delete { target, all } => {
            w.write_u8(3);
            write_name_ref(w, target);
            w.write_bool(*all);
        }
        DmlEvent::Truncate { target, cascade } => {
            w.write_u8(4);
            write_name_ref(w, target);
            w.write_bool(*cascade);
        }
    }
}

fn write_sequence_write(w: &mut BinaryWriter, sw: &SequenceWrite) {
    write_name_ref(w, &sw.sequence);
    w.write_bool(sw.transactional);
}

fn write_session_scope(w: &mut BinaryWriter, scope: SessionScope) {
    w.write_u8(match scope {
        SessionScope::Session => 0,
        SessionScope::Transaction => 1,
        SessionScope::Local => 2,
        SessionScope::Global => 3,
    });
}

fn write_session_value(w: &mut BinaryWriter, val: &SessionValue) {
    match val {
        SessionValue::Text { value } => {
            w.write_u8(0);
            w.write_str(value);
        }
        SessionValue::Bool { value } => {
            w.write_u8(1);
            w.write_bool(*value);
        }
        SessionValue::Integer { value } => {
            w.write_u8(2);
            w.write_i64(*value);
        }
        SessionValue::Names { value } => {
            w.write_u8(3);
            w.write_vec(value, write_name);
        }
        SessionValue::Default => w.write_u8(4),
    }
}

fn write_session_effect(w: &mut BinaryWriter, effect: &SessionEffect) {
    match effect {
        SessionEffect::SetVariable { scope, name, value } => {
            w.write_u8(0);
            write_session_scope(w, *scope);
            write_name(w, name);
            write_session_value(w, value);
        }
        SessionEffect::SetSearchPath { scope, path } => {
            w.write_u8(1);
            write_session_scope(w, *scope);
            write_search_path_value(w, path);
        }
        SessionEffect::SetNamespace { scope, namespace } => {
            w.write_u8(2);
            write_session_scope(w, *scope);
            w.write_vec(namespace, write_ident);
        }
        SessionEffect::SetSessionUser { scope, user } => {
            w.write_u8(3);
            write_session_scope(w, *scope);
            write_ident(w, user);
        }
        SessionEffect::ResetSessionUser { scope } => {
            w.write_u8(4);
            write_session_scope(w, *scope);
        }
        SessionEffect::SetCurrentUser { scope, user } => {
            w.write_u8(5);
            write_session_scope(w, *scope);
            write_ident(w, user);
        }
        SessionEffect::ResetCurrentUser { scope } => {
            w.write_u8(6);
            write_session_scope(w, *scope);
        }
        SessionEffect::SetAutocommit { scope, value } => {
            w.write_u8(7);
            write_session_scope(w, *scope);
            w.write_bool(*value);
        }
        SessionEffect::SetForeignKeyChecks { scope, value } => {
            w.write_u8(8);
            write_session_scope(w, *scope);
            w.write_bool(*value);
        }
        SessionEffect::SetSqliteForeignKeys { value } => {
            w.write_u8(9);
            w.write_bool(*value);
        }
        SessionEffect::ResetForeignKeyChecks { scope } => {
            w.write_u8(10);
            write_session_scope(w, *scope);
        }
        SessionEffect::ResetVariable { scope, name } => {
            w.write_u8(11);
            write_session_scope(w, *scope);
            write_name(w, name);
        }
        SessionEffect::ResetAll { scope } => {
            w.write_u8(12);
            write_session_scope(w, *scope);
        }
    }
}

fn write_begin_policy(w: &mut BinaryWriter, policy: BeginPolicy) {
    w.write_u8(match policy {
        BeginPolicy::Error => 0,
        BeginPolicy::Warn => 1,
        BeginPolicy::CommitActive => 2,
        BeginPolicy::Ignore => 3,
    });
}

fn write_tx_effect(w: &mut BinaryWriter, effect: &TxEffect) {
    match effect {
        TxEffect::Begin { if_active } => {
            w.write_u8(0);
            write_begin_policy(w, *if_active);
        }
        TxEffect::Commit { chain } => {
            w.write_u8(1);
            w.write_bool(*chain);
        }
        TxEffect::RollbackAll => w.write_u8(2),
        TxEffect::RollbackTo { savepoint } => {
            w.write_u8(3);
            write_ident(w, savepoint);
        }
        TxEffect::Savepoint { name } => {
            w.write_u8(4);
            write_ident(w, name);
        }
        TxEffect::ReleaseSavepoint { name } => {
            w.write_u8(5);
            write_ident(w, name);
        }
        TxEffect::SetReadOnly { value } => {
            w.write_u8(6);
            w.write_bool(*value);
        }
    }
}

fn write_object_ref(w: &mut BinaryWriter, obj: &ObjectRef) {
    match obj {
        ObjectRef::Relation { relation } => {
            w.write_u8(0);
            write_name_ref(w, relation);
        }
        ObjectRef::Index { index } => {
            w.write_u8(1);
            write_index_ref(w, index);
        }
        ObjectRef::Trigger { trigger } => {
            w.write_u8(2);
            write_trigger_ref(w, trigger);
        }
        ObjectRef::Sequence { sequence } => {
            w.write_u8(3);
            write_name_ref(w, sequence);
        }
        ObjectRef::Type { name } => {
            w.write_u8(4);
            write_name(w, name);
        }
        ObjectRef::Schema { name } => {
            w.write_u8(5);
            write_name(w, name);
        }
        ObjectRef::Constraint { constraint } => {
            w.write_u8(6);
            write_constraint_ref(w, constraint);
        }
    }
}

fn write_barrier_scope(w: &mut BinaryWriter, scope: &BarrierScope) {
    match scope {
        BarrierScope::Global => w.write_u8(0),
        BarrierScope::Relation { relation } => {
            w.write_u8(1);
            write_name_ref(w, relation);
        }
        BarrierScope::Schema { schema } => {
            w.write_u8(2);
            write_name(w, schema);
        }
        BarrierScope::Object { object } => {
            w.write_u8(3);
            write_object_ref(w, object);
        }
        BarrierScope::SessionState => w.write_u8(4),
        BarrierScope::TransactionState => w.write_u8(5),
    }
}

fn write_barrier_reason(w: &mut BinaryWriter, reason: BarrierReason) {
    w.write_u8(match reason {
        BarrierReason::SyntaxError => 0,
        BarrierReason::UnknownStatement => 1,
        BarrierReason::UnknownRelation => 2,
        BarrierReason::UnknownDdl => 3,
        BarrierReason::UnknownDml => 4,
        BarrierReason::UnknownDrop => 5,
        BarrierReason::UnknownAlter => 6,
        BarrierReason::UnknownView => 7,
        BarrierReason::UnknownIndex => 8,
        BarrierReason::UnknownTrigger => 9,
        BarrierReason::UnknownTruncate => 10,
        BarrierReason::UnknownSessionEffect => 11,
        BarrierReason::UnsupportedExpression => 12,
        BarrierReason::UnsupportedConflictClause => 13,
        BarrierReason::UnsupportedTriggerBody => 14,
        BarrierReason::UnsupportedTransaction => 15,
        BarrierReason::UnresolvedViewSource => 16,
        BarrierReason::WriteThroughView => 17,
        BarrierReason::OpaqueTriggerBody => 18,
        BarrierReason::UnmodeledRelationSource => 19,
    });
}

fn write_scoped_barrier(w: &mut BinaryWriter, b: &ScopedBarrier) {
    write_barrier_scope(w, &b.scope);
    write_barrier_reason(w, b.reason);
    w.write_opt_str(b.detail.as_deref());
}

fn write_trigger_effect(w: &mut BinaryWriter, effect: &TriggerEffect) {
    match effect {
        TriggerEffect::Dml { event } => {
            w.write_u8(0);
            write_dml_event(w, event);
        }
        TriggerEffect::Read { read } => {
            w.write_u8(1);
            write_read_fact(w, read);
        }
        TriggerEffect::WriteSequence { write } => {
            w.write_u8(2);
            write_sequence_write(w, write);
        }
        TriggerEffect::Session { effect } => {
            w.write_u8(3);
            write_session_effect(w, effect);
        }
        TriggerEffect::Barrier { barrier } => {
            w.write_u8(4);
            write_scoped_barrier(w, barrier);
        }
    }
}

fn write_trigger_body(w: &mut BinaryWriter, body: &TriggerBody) {
    match body {
        TriggerBody::Inline { effects } => {
            w.write_u8(0);
            w.write_vec(effects, write_trigger_effect);
        }
        TriggerBody::Function { name, args } => {
            w.write_u8(1);
            write_name(w, name);
            w.write_vec(args, |w, s| w.write_str(s));
        }
        TriggerBody::Opaque => w.write_u8(2),
    }
}

fn write_trigger_def(w: &mut BinaryWriter, trig: &TriggerDef) {
    write_trigger_ref(w, &trig.trigger);
    write_name_ref(w, &trig.table);
    write_trigger_timing(w, trig.timing);
    w.write_vec(&trig.events, write_trigger_event);
    w.write_bool(trig.for_each_row);
    w.write_vec(&trig.when_reads, write_read_fact);
    write_trigger_body(w, &trig.body);
    w.write_opt(trig.referenced_table.as_ref(), write_name_ref);
    w.write_bool(trig.temporary);
    w.write_bool(trig.if_not_exists);
    w.write_bool(trig.or_replace);
}

fn write_conflict_action(w: &mut BinaryWriter, action: ConflictAction) {
    w.write_u8(match action {
        ConflictAction::Error => 0,
        ConflictAction::Ignore => 1,
        ConflictAction::Replace => 2,
        ConflictAction::Update => 3,
    });
}

fn write_write_kind(w: &mut BinaryWriter, kind: &WriteKind) {
    match kind {
        WriteKind::Insert { conflict } => {
            w.write_u8(0);
            write_conflict_action(w, *conflict);
        }
        WriteKind::Update { all } => {
            w.write_u8(1);
            w.write_bool(*all);
        }
        WriteKind::Delete { all } => {
            w.write_u8(2);
            w.write_bool(*all);
        }
        WriteKind::Merge => w.write_u8(3),
        WriteKind::Truncate {
            cascade,
            restart_identity,
        } => {
            w.write_u8(4);
            w.write_bool(*cascade);
            w.write_bool(*restart_identity);
        }
    }
}

fn write_data_write(w: &mut BinaryWriter, dw: &DataWrite) {
    write_name_ref(w, &dw.target);
    write_write_kind(w, &dw.kind);
    w.write_vec(&dw.columns, write_ident);
    w.write_vec(&dw.unqualified_columns, write_ident);
    w.write_vec(&dw.reads, write_read_fact);
    w.write_bool(dw.only);
}

fn write_alter_change(w: &mut BinaryWriter, change: &AlterChange) {
    match change {
        AlterChange::AddColumn {
            column,
            constraints,
        } => {
            w.write_u8(0);
            write_column_def(w, column);
            w.write_vec(constraints, write_constraint);
        }
        AlterChange::DropColumn { column, cascade } => {
            w.write_u8(1);
            write_ident(w, column);
            w.write_bool(*cascade);
        }
        AlterChange::AddConstraint { constraint } => {
            w.write_u8(2);
            write_constraint(w, constraint);
        }
        AlterChange::DropConstraint { name, cascade } => {
            w.write_u8(3);
            write_ident(w, name);
            w.write_bool(*cascade);
        }
        AlterChange::RenameConstraint { from, to } => {
            w.write_u8(4);
            write_ident(w, from);
            write_ident(w, to);
        }
        AlterChange::ValidateConstraint { name } => {
            w.write_u8(5);
            write_ident(w, name);
        }
        AlterChange::DropIndex { name } => {
            w.write_u8(6);
            write_ident(w, name);
        }
        AlterChange::AlterColumn {
            column,
            detail,
            referenced,
            repositions,
        } => {
            w.write_u8(7);
            write_ident(w, column);
            w.write_str(detail);
            w.write_vec(referenced, write_ident);
            w.write_bool(*repositions);
        }
        AlterChange::Other { detail } => {
            w.write_u8(8);
            w.write_str(detail);
        }
    }
}

fn write_alter_table(w: &mut BinaryWriter, at: &AlterTable) {
    write_name_ref(w, &at.table);
    w.write_vec(&at.changes, write_alter_change);
}

fn write_partition_link(w: &mut BinaryWriter, link: &PartitionLink) {
    write_name_ref(w, &link.parent);
    write_name_ref(w, &link.child);
}

fn write_drop_mode(w: &mut BinaryWriter, mode: DropMode) {
    w.write_u8(match mode {
        DropMode::Restrict => 0,
        DropMode::Cascade => 1,
    });
}

fn write_drop_object(w: &mut BinaryWriter, obj: &DropObject) {
    match obj {
        DropObject::Relation { name, kind } => {
            w.write_u8(0);
            write_name_ref(w, name);
            write_relation_kind(w, *kind);
        }
        DropObject::Index { index } => {
            w.write_u8(1);
            write_index_ref(w, index);
        }
        DropObject::Trigger { trigger } => {
            w.write_u8(2);
            write_trigger_ref(w, trigger);
        }
        DropObject::Sequence { name } => {
            w.write_u8(3);
            write_name_ref(w, name);
        }
        DropObject::Type { name } => {
            w.write_u8(4);
            write_name(w, name);
        }
        DropObject::Schema { name } => {
            w.write_u8(5);
            write_name(w, name);
        }
    }
}

fn write_drop_op(w: &mut BinaryWriter, drop: &DropOp) {
    w.write_vec(&drop.objects, write_drop_object);
    write_drop_mode(w, drop.mode);
}

fn write_rename_op(w: &mut BinaryWriter, rename: &RenameOp) {
    match rename {
        RenameOp::Relation {
            from,
            to,
            index_schema_placement,
        } => {
            w.write_u8(0);
            write_name_ref(w, from);
            write_name(w, to);
            write_index_schema_placement(w, *index_schema_placement);
        }
        RenameOp::Column { table, from, to } => {
            w.write_u8(1);
            write_name_ref(w, table);
            write_ident(w, from);
            write_ident(w, to);
        }
    }
}

fn write_refresh_op(w: &mut BinaryWriter, refresh: &RefreshOp) {
    write_name_ref(w, &refresh.target);
    write_read_mode(w, refresh.source_mode);
}

fn write_catalog_op(w: &mut BinaryWriter, op: &CatalogOp) {
    match op {
        CatalogOp::DefineTable(t) => {
            w.write_u8(0);
            write_table_def(w, t);
        }
        CatalogOp::DefineView(v) => {
            w.write_u8(1);
            write_view_def(w, v);
        }
        CatalogOp::DefineIndex(i) => {
            w.write_u8(2);
            write_index_def(w, i);
        }
        CatalogOp::DefineTrigger(tr) => {
            w.write_u8(3);
            write_trigger_def(w, tr);
        }
        CatalogOp::DefineSequence(s) => {
            w.write_u8(4);
            write_name(w, &s.name);
            w.write_bool(s.temporary);
            w.write_bool(s.transactional);
        }
        CatalogOp::DefineType(ty) => {
            w.write_u8(5);
            write_name(w, &ty.name);
        }
        CatalogOp::DefineSchema(sc) => {
            w.write_u8(6);
            write_name(w, &sc.name);
        }
        CatalogOp::Read(r) => {
            w.write_u8(7);
            write_read_fact(w, r);
        }
        CatalogOp::WriteData(dw) => {
            w.write_u8(8);
            write_data_write(w, dw);
        }
        CatalogOp::WriteSequence(sw) => {
            w.write_u8(9);
            write_sequence_write(w, sw);
        }
        CatalogOp::AlterTable(at) => {
            w.write_u8(10);
            write_alter_table(w, at);
        }
        CatalogOp::AttachPartition(link) => {
            w.write_u8(11);
            write_partition_link(w, link);
        }
        CatalogOp::DetachPartition(link) => {
            w.write_u8(12);
            write_partition_link(w, link);
        }
        CatalogOp::AttachInherit(link) => {
            w.write_u8(13);
            write_partition_link(w, link);
        }
        CatalogOp::DetachInherit(link) => {
            w.write_u8(14);
            write_partition_link(w, link);
        }
        CatalogOp::Drop(d) => {
            w.write_u8(15);
            write_drop_op(w, d);
        }
        CatalogOp::Rename(rn) => {
            w.write_u8(16);
            write_rename_op(w, rn);
        }
        CatalogOp::Refresh(rf) => {
            w.write_u8(17);
            write_refresh_op(w, rf);
        }
    }
}

fn category_code(cat: NativeEditCategory) -> u8 {
    match cat {
        NativeEditCategory::Expression => 0,
        NativeEditCategory::Clause => 1,
        NativeEditCategory::Constraint => 2,
        NativeEditCategory::List => 3,
        NativeEditCategory::Column => 4,
        NativeEditCategory::Type => 5,
        NativeEditCategory::Literal => 6,
    }
}

fn write_edit(w: &mut BinaryWriter, edit: &NativeEdit) {
    w.write_u32(edit.start as u32);
    w.write_u32(edit.end as u32);
    w.write_str(&edit.replacement);
    w.write_u8(category_code(edit.category));
    w.write_str(&edit.label);
}

fn write_facts(w: &mut BinaryWriter, facts: &NativeFacts) {
    w.write_vec(&facts.ops, write_catalog_op);
    w.write_vec(&facts.session, write_session_effect);
    w.write_vec(&facts.pre_tx, write_tx_effect);
    w.write_vec(&facts.post_tx, write_tx_effect);
    w.write_vec(&facts.barriers, write_scoped_barrier);
    w.write_vec(&facts.edits, write_edit);
}

fn write_unit(w: &mut BinaryWriter, unit: &NativeUnit) {
    w.write_u32(unit.start as u32);
    w.write_u32(unit.end as u32);
    w.write_opt_str(unit.parse_error.as_deref());
    w.write_bool(unit.cannot_produce_result);
    match &unit.facts {
        None => w.write_u8(0),
        Some(facts) => {
            w.write_u8(1);
            write_facts(w, facts);
        }
    }
}

fn site_kind_code(kind: ColumnSiteKind) -> u8 {
    match kind {
        ColumnSiteKind::CreateDef => 0,
        ColumnSiteKind::InsertListItem => 1,
        ColumnSiteKind::ValuesItem => 2,
    }
}

fn write_column_site(w: &mut BinaryWriter, site: &ColumnSite) {
    w.write_u32(site.stmt as u32);
    w.write_u32(site.start as u32);
    w.write_u32(site.end as u32);
    w.write_u8(site_kind_code(site.kind));
}

fn write_column_target(w: &mut BinaryWriter, target: &ColumnTarget) {
    write_name(w, &target.table);
    write_ident(w, &target.column);
    w.write_u32(target.ordinal as u32);
    w.write_vec(&target.sites, write_column_site);
    w.write_opt_str(target.blocked.as_deref());
}

/// Serializes the entire [`NativeResponse`] payload into pure binary OCCM v2 format.
pub(crate) fn encode_native_response(response: &NativeResponse) -> Vec<u8> {
    let mut w = BinaryWriter::new();
    w.buf.extend_from_slice(OCCM_MAGIC);
    write_session_init(&mut w, &response.initial_session);
    w.write_vec(&response.units, write_unit);
    w.write_vec(&response.columns, write_column_target);
    w.into_bytes()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    #[test]
    fn test_encode_empty_response() {
        let response = NativeResponse {
            initial_session: SessionInit {
                namespace_prefix: Vec::new(),
                search_path: Vec::new(),
                current_user: None,
                autocommit: true,
                foreign_key_checks: true,
                variables: BTreeMap::new(),
            },
            units: Vec::new(),
            columns: Vec::new(),
        };
        let encoded = encode_native_response(&response);
        assert!(encoded.starts_with(OCCM_MAGIC));
        assert!(encoded.len() > 5);
    }
}

//! Precision-bound column reduction facts.
//!
//! The first supported family is one table scope per bundle: a `CREATE TABLE`
//! column definition together with the matching `INSERT` explicit column-list
//! items and per-`VALUES`-row item positions (positional binding for implicit
//! definition-order inserts). All parsing and span extraction happens here in
//! the Rust frontend; the Haskell engine only consumes exact byte spans.
//!
//! Two consumers share one span computation:
//! - single-point edits (one span each, gated ONLY by span/site validity),
//! - atomic bundles (all sites of one column, additionally gated by the
//!   generation/binding fences below).
//!
//! A bundle is emitted only when every site is exact and the table scope has
//! zero fences. Otherwise the column target is emitted with `blocked` set (or
//! omitted when it would duplicate a single), and is never partially applied.
//!
//! Fence boundary (first family, conservative — no generation solver):
//! - Reads elsewhere (projections, `WHERE`, view sources, trigger
//!   conditions/effects, `RETURNING`) fence by exact column match; any star
//!   cover over the table fences regardless of what else the unit reads.
//!   Plain one-source projections that do not read the column never fence.
//! - Writes elsewhere (`UPDATE`/`DELETE`/`MERGE` assignments, trigger-body
//!   DML) fence by exact column match.
//! - Parent-side foreign keys from any table fence the referenced column;
//!   implicit parent columns fence the whole parent table.
//! - Opaque units (parse failures, global/session/transaction/schema
//!   barriers, session effects, transaction effects) can affect any table
//!   without naming it, so any opaque unit blocks every bundle.
//!   Relation-scoped barriers fence only the named table. In particular the
//!   pre-existing sequence/routine analysis marks ANY function call
//!   (including `SUM`, `COUNT`) as a global opaque barrier; that fence is
//!   preserved verbatim — there is deliberately no builtin or fixture
//!   whitelist. First-family proofs use routine-free projections such as
//!   `SELECT a FROM t ORDER BY a`.
//! - Generation ambiguity (zero/non-unique definitions, `ALTER`, renames,
//!   drops, `LIKE`, table sources, `ON COMMIT` actions, temporary tables,
//!   inserts that precede the definition) fences the whole table.
//! - Qualification ambiguity (same trailing name with distinct qualification,
//!   e.g. `t` vs `main.t`, in any role) fences the table: the first family
//!   has no namespace resolver and never assumes two spellings are distinct.

use crate::ir;
use std::collections::{HashMap, HashSet};

/// Emit nothing below this site count: a single-site bundle is byte-identical
/// to its single-point edit, so emitting both would only duplicate candidates.
const MIN_BUNDLE_SITES: usize = 2;

pub const BLOCK_CONSTRAINT: &str = "column-constraint-ref";
pub const BLOCK_READ: &str = "column-read-elsewhere";
pub const BLOCK_STAR: &str = "column-star-projection";
pub const BLOCK_AMBIGUOUS: &str = "column-ambiguous-binding";
pub const BLOCK_UNKNOWN: &str = "column-unknown-source";
pub const BLOCK_GENERATION: &str = "column-generation-ambiguous";
pub const BLOCK_SHAPE: &str = "column-insert-shape";
pub const BLOCK_DEPENDENT: &str = "column-dependent-object";
pub const BLOCK_SPAN: &str = "column-span-incomplete";

/// Per-unit intermediate column data. In-memory only, never serialized; the
/// response carries only the assembled [`ColumnTarget`] list.
#[derive(Clone, Debug, Default)]
pub struct ColumnUnit {
    pub creates: Vec<CreateColumns>,
    pub inserts: Vec<InsertColumns>,
    /// Tables covered by a whole-row use (`SELECT *`, `t.*`, `TABLE t`) in
    /// this unit, resolved with the same matcher as read attribution.
    pub stars: Vec<ir::NameRef>,
    pub unknown_source: bool,
}

/// Canonical column name plus its comma-gated deletion span, in source order.
pub type NamedSpans = Vec<(ir::Ident, Option<(usize, usize)>)>;

/// One `CREATE TABLE` column list in definition order: canonical column name
/// plus the comma-gated whole-definition deletion span (`None` when no exact
/// span can be formed, e.g. a singleton list or an unmappable boundary).
#[derive(Clone, Debug)]
pub struct CreateColumns {
    pub columns: NamedSpans,
}

/// One `INSERT` statement's column geometry.
#[derive(Clone, Debug)]
pub struct InsertColumns {
    pub target: Option<ir::NameRef>,
    /// Explicit column list in source order (`None` = implicit definition
    /// order). Each entry is the canonical name plus its comma-gated deletion
    /// span.
    pub explicit: Option<NamedSpans>,
    /// Per-`VALUES`-row item deletion spans by position (empty unless the
    /// source is a plain `VALUES` list).
    pub rows: Vec<Vec<Option<(usize, usize)>>>,
    pub shape: InsertShape,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InsertShape {
    /// Plain `VALUES` source with plain flags; per-column sites are usable.
    PlainValues,
    /// Non-`VALUES` query source. With an explicit list only the listed
    /// columns fence; without one every column fences (positional binding).
    SelectSource,
    /// `DEFAULT VALUES` form: references no column, fences nothing.
    Neutral,
    /// Overwrite/partition/multi-table/`BY NAME`/assignments/conflict/`ON`/
    /// `RETURNING`/`OUTPUT`, or an unresolvable explicit list: fences every
    /// bundle of the target table.
    Exotic,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct ColumnSite {
    /// 1-based statement id, matching the Haskell `StatementId` numbering.
    pub stmt: usize,
    pub start: usize,
    pub end: usize,
    pub kind: ColumnSiteKind,
}

#[derive(Clone, Copy, Debug, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ColumnSiteKind {
    CreateDef,
    InsertListItem,
    ValuesItem,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct ColumnTarget {
    pub table: ir::Name,
    pub column: ir::Ident,
    /// Ordinal within the single `CREATE TABLE` definition.
    pub ordinal: usize,
    /// Sorted by `(stmt, start)` once at build; never re-sorted downstream.
    pub sites: Vec<ColumnSite>,
    /// `Some` cause means "skipped, never oracle-tested" (see `BLOCK_*`).
    pub blocked: Option<String>,
}

struct UnitView<'a> {
    index: usize,
    start: usize,
    end: usize,
    facts: Option<&'a ir::NativeFacts>,
}

/// Reference material for one unit, precomputed ONCE per assembly (never per
/// column) when column IR is on.
struct UnitRefs<'a> {
    reads: Vec<&'a ir::ReadFact>,
    writes: Vec<(&'a ir::Name, &'a [ir::Ident])>,
    touched: HashSet<ir::Name>,
    stars: &'a [ir::NameRef],
    unknown_source: bool,
    /// Opaque units can affect any table without naming it.
    opaque: bool,
}

/// Every read of a base table in one unit's ops, including reads nested in
/// writes, view/table sources, and trigger conditions/effects.
fn collect_reads(facts: &ir::NativeFacts) -> Vec<&ir::ReadFact> {
    let mut reads = Vec::new();
    for op in &facts.ops {
        match op {
            ir::CatalogOp::Read(read) => reads.push(read),
            ir::CatalogOp::WriteData(write) => reads.extend(write.reads.iter()),
            ir::CatalogOp::DefineTable(table) => reads.extend(table.sources.iter()),
            ir::CatalogOp::DefineView(view) => reads.extend(view.sources.iter()),
            ir::CatalogOp::DefineTrigger(trigger) => {
                reads.extend(trigger.when_reads.iter());
                if let ir::TriggerBody::Inline { effects } = &trigger.body {
                    for effect in effects {
                        if let ir::TriggerEffect::Read { read } = effect {
                            reads.push(read);
                        }
                    }
                }
            }
            _ => {}
        }
    }
    reads
}

/// Non-`INSERT` writes plus trigger-body DML: (target table, assigned columns).
/// `INSERT` targets are handled through their [`InsertColumns`] geometry, not here.
fn collect_writes(facts: &ir::NativeFacts) -> Vec<(&ir::Name, &[ir::Ident])> {
    let mut refs = Vec::new();
    for op in &facts.ops {
        match op {
            ir::CatalogOp::WriteData(write) => {
                if !matches!(write.kind, ir::WriteKind::Insert { .. }) {
                    refs.push((&write.target.name, write.columns.as_slice()));
                }
            }
            ir::CatalogOp::DefineTrigger(trigger) => {
                if let ir::TriggerBody::Inline { effects } = &trigger.body {
                    for effect in effects {
                        if let ir::TriggerEffect::Dml { event } = effect {
                            match event {
                                ir::DmlEvent::Insert { target, columns }
                                | ir::DmlEvent::Update { target, columns }
                                | ir::DmlEvent::Merge { target, columns } => {
                                    refs.push((&target.name, columns.as_slice()));
                                }
                                ir::DmlEvent::Delete { target, .. }
                                | ir::DmlEvent::Truncate { target, .. } => {
                                    refs.push((&target.name, &[][..]));
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    refs
}

/// Tables touched by one unit: used to scope the unknown-source fence to units
/// that can actually observe the table.
fn collect_touched(facts: &ir::NativeFacts, reads: &[&ir::ReadFact]) -> HashSet<ir::Name> {
    let mut touched = HashSet::new();
    for read in reads {
        touched.insert(read.source.name.clone());
    }
    for op in &facts.ops {
        match op {
            ir::CatalogOp::DefineTable(table) => {
                touched.insert(table.name.clone());
            }
            ir::CatalogOp::DefineView(view) => {
                touched.insert(view.name.clone());
            }
            ir::CatalogOp::DefineIndex(index) => {
                touched.insert(index.table.name.clone());
            }
            ir::CatalogOp::WriteData(write) => {
                touched.insert(write.target.name.clone());
            }
            ir::CatalogOp::AlterTable(alter) => {
                touched.insert(alter.table.name.clone());
            }
            ir::CatalogOp::DefineTrigger(trigger) => {
                touched.insert(trigger.table.name.clone());
            }
            _ => {}
        }
    }
    touched
}

fn collect_refs(facts: &ir::NativeFacts) -> UnitRefs<'_> {
    let reads = collect_reads(facts);
    let writes = collect_writes(facts);
    let touched = collect_touched(facts, &reads);
    // Global, session, transaction, and schema barriers — like session and
    // transaction effects, which can change name resolution and snapshots —
    // make the unit opaque: it may affect any table without naming it.
    // `BEGIN`/`ROLLBACK` may carry no separate barrier, so any transaction
    // effect counts; the first family does not model tx snapshots.
    let opaque = !facts.session.is_empty()
        || !facts.pre_tx.is_empty()
        || !facts.post_tx.is_empty()
        || facts.barriers.iter().any(|barrier| {
            matches!(
                barrier.scope,
                ir::BarrierScope::Global
                    | ir::BarrierScope::SessionState
                    | ir::BarrierScope::TransactionState
                    | ir::BarrierScope::Schema { .. }
            )
        });
    UnitRefs {
        reads,
        writes,
        touched,
        stars: facts.column.stars.as_slice(),
        unknown_source: facts.column.unknown_source,
        opaque,
    }
}

/// Generation fences (any `ALTER TABLE`, rename, or drop of the table),
/// dependent fences (any trigger hosted on the table — `WHEN`/body effects
/// are opaque), and relation-scoped barrier fences.
fn collect_table_fences(
    facts: &ir::NativeFacts,
    generation: &mut HashSet<ir::Name>,
    dependent: &mut HashSet<ir::Name>,
    relation: &mut HashSet<ir::Name>,
) {
    for op in &facts.ops {
        match op {
            ir::CatalogOp::AlterTable(alter) => {
                generation.insert(alter.table.name.clone());
            }
            ir::CatalogOp::Rename(rename) => match rename {
                ir::RenameOp::Relation { from, to, .. } => {
                    generation.insert(from.name.clone());
                    generation.insert(to.clone());
                }
                ir::RenameOp::Column { table, .. } => {
                    generation.insert(table.name.clone());
                }
            },
            ir::CatalogOp::Drop(drop) => {
                for object in &drop.objects {
                    if let ir::DropObject::Relation { name, .. } = object {
                        generation.insert(name.name.clone());
                    }
                }
            }
            ir::CatalogOp::DefineTrigger(trigger) => {
                dependent.insert(trigger.table.name.clone());
            }
            _ => {}
        }
    }
    for barrier in &facts.barriers {
        match &barrier.scope {
            ir::BarrierScope::Relation { relation: named } => {
                relation.insert(named.name.clone());
            }
            ir::BarrierScope::Object {
                object: ir::ObjectRef::Relation { relation: named },
            } => {
                relation.insert(named.name.clone());
            }
            _ => {}
        }
    }
}

fn constraint_touches(constraint: &ir::Constraint, column: &ir::Ident) -> bool {
    match constraint {
        ir::Constraint::PrimaryKey { columns, .. }
        | ir::Constraint::Unique { columns, .. }
        | ir::Constraint::Check { columns, .. }
        | ir::Constraint::Index { columns, .. } => columns.contains(column),
        ir::Constraint::ForeignKey(key) => key.child_columns.contains(column),
        // `USING INDEX` names an index, not columns; `Other` is opaque.
        // Both are handled as whole-table fences by the caller.
        ir::Constraint::UsingIndex { .. } | ir::Constraint::Other { .. } => false,
    }
}

fn constraint_is_opaque(constraint: &ir::Constraint) -> bool {
    matches!(
        constraint,
        ir::Constraint::UsingIndex { .. } | ir::Constraint::Other { .. }
    )
}

fn name_in(names: &[ir::NameRef], table: &ir::Name) -> bool {
    names.iter().any(|name| &name.name == table)
}

fn has_duplicate_names(columns: &[ir::ColumnDef]) -> bool {
    let mut seen = HashSet::new();
    columns.iter().any(|column| !seen.insert(&column.name))
}

type TableDefines<'a> = Vec<(usize, &'a ir::TableDef, Option<&'a CreateColumns>)>;

struct TablePlan<'a> {
    name: ir::Name,
    defines: TableDefines<'a>,
}

fn plan_tables<'a>(views: &'a [UnitView<'a>]) -> Vec<TablePlan<'a>> {
    let mut order: Vec<ir::Name> = Vec::new();
    let mut tables: HashMap<ir::Name, TableDefines<'a>> = HashMap::new();
    for view in views {
        let Some(facts) = view.facts else { continue };
        let mut creates = facts.column.creates.iter();
        for op in &facts.ops {
            let ir::CatalogOp::DefineTable(define) = op else {
                continue;
            };
            // Pair positional definition geometry with `DefineTable` ops.
            // Nested merges never carry geometry (nested analysis runs with
            // the column gate off), so at most the top-level definition pairs.
            let geometry = creates.next();
            tables
                .entry(define.name.clone())
                .or_insert_with(|| {
                    order.push(define.name.clone());
                    Vec::new()
                })
                .push((view.index, define, geometry));
        }
    }
    order
        .into_iter()
        .filter_map(|name| {
            tables
                .remove(&name)
                .map(|defines| TablePlan { name, defines })
        })
        .collect()
}

/// Validate bundle sites: unit bounds, UTF-8 boundaries, and strict
/// non-overlap within each statement. Sorted by `(stmt, start)` once here.
fn validate_sites(views: &[UnitView<'_>], source: &str, sites: &mut [ColumnSite]) -> bool {
    sites.sort_by_key(|site| (site.stmt, site.start));
    let mut previous: Option<(usize, usize)> = None;
    for site in sites.iter() {
        if site.start >= site.end {
            return false;
        }
        let Some(view) = views.get(site.stmt.checked_sub(1).unwrap_or(usize::MAX)) else {
            return false;
        };
        if site.start < view.start || site.end > view.end {
            return false;
        }
        if !source.is_char_boundary(site.start) || !source.is_char_boundary(site.end) {
            return false;
        }
        if previous.is_some_and(|(previous_stmt, previous_end)| {
            previous_stmt == site.stmt && previous_end > site.start
        }) {
            return false;
        }
        previous = Some((site.stmt, site.end));
    }
    true
}

struct Assembly<'a> {
    views: Vec<UnitView<'a>>,
    refs: Vec<Option<UnitRefs<'a>>>,
    /// Any parse failure, opaque unit, or resolution-changing effect blocks
    /// every bundle: never infer no-effect from no explicit names.
    opaque_all: bool,
    generation_fenced: HashSet<ir::Name>,
    dependent_fenced: HashSet<ir::Name>,
    relation_fenced: HashSet<ir::Name>,
    index_columns: HashMap<ir::Name, Vec<ir::Ident>>,
    index_opaque: HashSet<ir::Name>,
    /// Parent-side foreign-key targets: referenced columns per parent table.
    parent_columns: HashMap<ir::Name, HashSet<ir::Ident>>,
    /// Parents referenced with implicit (unknown) columns: fence the whole
    /// parent table, including self-references.
    parent_opaque: HashSet<ir::Name>,
    /// Tables with same-trailing-name, distinctly-qualified references
    /// anywhere (e.g. `t` vs `main.t`): no namespace resolver exists, so any
    /// such spelling fences the table as ambiguous.
    ambiguous_tables: HashSet<ir::Name>,
}

/// Reference fences (reads, writes, stars, unknown sources) for one column.
/// Returns the blocking cause, if any. Only exact column matches and actual
/// star covers fence; plain projections that avoid the column never do.
fn reference_fence(
    assembly: &Assembly<'_>,
    table: &ir::Name,
    column: &ir::Ident,
) -> Option<&'static str> {
    for refs in assembly.refs.iter().flatten() {
        // Star cover fences independently of what else the unit reads: a
        // `SELECT *` mixed with explicit expressions still observes every
        // column. Empty-column reads without cover (`SELECT 1`, bare
        // `DELETE`) observe none and never fence.
        if name_in(refs.stars, table) {
            return Some(BLOCK_STAR);
        }
        for read in &refs.reads {
            if read.source.name != *table {
                continue;
            }
            if read.columns.contains(column) {
                return Some(BLOCK_READ);
            }
        }
        for (target, columns) in &refs.writes {
            if *target == table && columns.contains(column) {
                return Some(BLOCK_READ);
            }
        }
        if refs.unknown_source && refs.touched.contains(table) {
            return Some(BLOCK_UNKNOWN);
        }
    }
    None
}

/// Append one insert's contribution for one column to the target's sites, or
/// return the fence that blocks the bundle. Row arity is validated against
/// the whole binding (explicit list length, or `CREATE TABLE` arity for
/// implicit inserts) before any site is trusted.
fn insert_contribution(
    insert: &InsertColumns,
    column: &ir::Ident,
    ordinal: usize,
    create_arity: usize,
    stmt: usize,
    sites: &mut Vec<ColumnSite>,
) -> Result<(), &'static str> {
    match insert.shape {
        InsertShape::Exotic => Err(BLOCK_SHAPE),
        InsertShape::Neutral => Ok(()),
        InsertShape::SelectSource => match &insert.explicit {
            // A `SELECT` source with an explicit list only constrains the
            // listed columns; anything else is untouched by this insert.
            Some(list) if list.iter().any(|(name, _)| name == column) => Err(BLOCK_SHAPE),
            // Implicit `SELECT` binds every column positionally.
            None => Err(BLOCK_SHAPE),
            _ => Ok(()),
        },
        InsertShape::PlainValues => {
            if insert.rows.is_empty() {
                // A `VALUES` source always carries rows; an empty set means
                // the geometry failed and no positional claim is sound.
                return Err(BLOCK_SPAN);
            }
            match &insert.explicit {
                Some(list) => {
                    let positions: Vec<usize> = list
                        .iter()
                        .enumerate()
                        .filter(|(_, (name, _))| name == column)
                        .map(|(position, _)| position)
                        .collect();
                    if positions.is_empty() {
                        return Ok(());
                    }
                    if positions.len() > 1 {
                        return Err(BLOCK_AMBIGUOUS);
                    }
                    if !insert.rows.iter().all(|row| row.len() == list.len()) {
                        return Err(BLOCK_SPAN);
                    }
                    let position = positions[0];
                    let Some((start, end)) = list[position].1 else {
                        return Err(BLOCK_SPAN);
                    };
                    sites.push(ColumnSite {
                        stmt,
                        start,
                        end,
                        kind: ColumnSiteKind::InsertListItem,
                    });
                    for row in &insert.rows {
                        let Some(Some((start, end))) = row.get(position) else {
                            return Err(BLOCK_SPAN);
                        };
                        sites.push(ColumnSite {
                            stmt,
                            start: *start,
                            end: *end,
                            kind: ColumnSiteKind::ValuesItem,
                        });
                    }
                    Ok(())
                }
                None => {
                    if !insert.rows.iter().all(|row| row.len() == create_arity) {
                        return Err(BLOCK_SPAN);
                    }
                    for row in &insert.rows {
                        let Some(Some((start, end))) = row.get(ordinal) else {
                            return Err(BLOCK_SPAN);
                        };
                        sites.push(ColumnSite {
                            stmt,
                            start: *start,
                            end: *end,
                            kind: ColumnSiteKind::ValuesItem,
                        });
                    }
                    Ok(())
                }
            }
        }
    }
}

fn blocked_target(
    table: &ir::Name,
    column: ir::Ident,
    ordinal: usize,
    sites: Vec<ColumnSite>,
    cause: &str,
) -> ColumnTarget {
    ColumnTarget {
        table: table.clone(),
        column,
        ordinal,
        sites,
        blocked: Some(cause.to_string()),
    }
}

fn column_target(
    assembly: &Assembly<'_>,
    source: &str,
    table: &ir::Name,
    define: &(usize, &ir::TableDef, Option<&CreateColumns>),
    ordinal: usize,
    create_dup_names: bool,
) -> Option<ColumnTarget> {
    let (unit_index, define, geometry) = *define;
    // Geometry and op columns are built from the same AST list; a pairing
    // failure still yields a blocked target so the skip is counted, never a
    // bundle.
    let defined = &define.columns[ordinal];
    let column = defined.name.clone();
    let create_span = geometry
        .and_then(|geometry| geometry.columns.get(ordinal))
        .filter(|(name, _)| *name == column)
        .and_then(|(_, span)| *span);
    let mut sites = Vec::new();
    if let Some((start, end)) = create_span {
        sites.push(ColumnSite {
            stmt: unit_index + 1,
            start,
            end,
            kind: ColumnSiteKind::CreateDef,
        });
    }
    if assembly.opaque_all {
        return Some(blocked_target(table, column, ordinal, sites, BLOCK_UNKNOWN));
    }
    let mut blocked: Option<&str> = None;
    if create_span.is_none() {
        blocked = Some(BLOCK_SPAN);
    }
    if create_dup_names {
        blocked = blocked.or(Some(BLOCK_AMBIGUOUS));
    }
    // Whole-definition fences.
    if define.like.is_some()
        || !define.sources.is_empty()
        || define.on_commit.is_some()
        || define.temporary
    {
        blocked = blocked.or(Some(BLOCK_GENERATION));
    }
    if assembly.ambiguous_tables.contains(table) {
        blocked = blocked.or(Some(BLOCK_AMBIGUOUS));
    }
    if assembly.generation_fenced.contains(table) {
        blocked = blocked.or(Some(BLOCK_GENERATION));
    }
    if assembly.relation_fenced.contains(table) {
        blocked = blocked.or(Some(BLOCK_UNKNOWN));
    }
    if assembly.dependent_fenced.contains(table) {
        blocked = blocked.or(Some(BLOCK_DEPENDENT));
    }
    if assembly.index_opaque.contains(table) {
        blocked = blocked.or(Some(BLOCK_DEPENDENT));
    }
    if define.constraints.iter().any(constraint_is_opaque) {
        blocked = blocked.or(Some(BLOCK_CONSTRAINT));
    }
    // Per-column fences.
    if defined.has_default
        || defined.generated
        || !defined.references.is_empty()
        || define
            .constraints
            .iter()
            .any(|c| constraint_touches(c, &column))
    {
        blocked = blocked.or(Some(BLOCK_CONSTRAINT));
    }
    if assembly
        .index_columns
        .get(table)
        .is_some_and(|indexed| indexed.contains(&column))
    {
        blocked = blocked.or(Some(BLOCK_DEPENDENT));
    }
    if assembly.parent_opaque.contains(table) {
        blocked = blocked.or(Some(BLOCK_DEPENDENT));
    }
    if assembly
        .parent_columns
        .get(table)
        .is_some_and(|parents| parents.contains(&column))
    {
        blocked = blocked.or(Some(BLOCK_DEPENDENT));
    }
    if blocked.is_none() {
        blocked = reference_fence(assembly, table, &column);
    }
    // Insert contributions, in unit order. The single definition must precede
    // every matched insert; anything earlier cannot bind to it.
    if blocked.is_none() {
        let create_arity = define.columns.len();
        for view in &assembly.views {
            let Some(facts) = view.facts else { continue };
            for insert in &facts.column.inserts {
                let Some(target) = &insert.target else {
                    continue;
                };
                if target.name != *table {
                    continue;
                }
                if view.index < unit_index {
                    blocked = Some(BLOCK_GENERATION);
                    break;
                }
                if let Err(cause) = insert_contribution(
                    insert,
                    &column,
                    ordinal,
                    create_arity,
                    view.index + 1,
                    &mut sites,
                ) {
                    blocked = Some(cause);
                    break;
                }
            }
            if blocked.is_some() {
                break;
            }
        }
    }
    if blocked.is_none() && !validate_sites(&assembly.views, source, &mut sites) {
        blocked = Some(BLOCK_SPAN);
    }
    // A surviving single-site bundle is byte-identical to its single-point
    // edit; the single covers it, so no bundle is emitted.
    if blocked.is_none() && sites.len() < MIN_BUNDLE_SITES {
        return None;
    }
    Some(ColumnTarget {
        table: table.clone(),
        column,
        ordinal,
        sites,
        blocked: blocked.map(str::to_string),
    })
}

/// Assemble response-level column targets. Tables plan in first-appearance
/// order of their `CREATE TABLE`; columns in definition order. Deterministic.
/// Reference material is precomputed once per assembly, never per column.
pub fn assemble_columns(units: &[crate::NativeUnit], source: &str) -> Vec<ColumnTarget> {
    let views: Vec<UnitView<'_>> = units
        .iter()
        .enumerate()
        .map(|(index, unit)| UnitView {
            index,
            start: unit.start,
            end: unit.end,
            facts: unit.facts.as_ref(),
        })
        .collect();
    let mut refs: Vec<Option<UnitRefs<'_>>> = Vec::with_capacity(views.len());
    let mut opaque_all = false;
    let mut generation_fenced = HashSet::new();
    let mut dependent_fenced = HashSet::new();
    let mut relation_fenced = HashSet::new();
    let mut index_columns: HashMap<ir::Name, Vec<ir::Ident>> = HashMap::new();
    let mut index_opaque = HashSet::new();
    let mut parent_columns: HashMap<ir::Name, HashSet<ir::Ident>> = HashMap::new();
    let mut parent_opaque = HashSet::new();
    let mut relation_names: HashSet<ir::Name> = HashSet::new();
    for view in &views {
        let Some(facts) = view.facts else {
            refs.push(None);
            // A unit without facts (parse failure) may do anything.
            opaque_all = true;
            continue;
        };
        let unit = collect_refs(facts);
        opaque_all |= unit.opaque;
        for read in &unit.reads {
            relation_names.insert(read.source.name.clone());
        }
        for (target, _) in &unit.writes {
            relation_names.insert((*target).clone());
        }
        for insert in &facts.column.inserts {
            if let Some(target) = &insert.target {
                relation_names.insert(target.name.clone());
            }
        }
        refs.push(Some(unit));
        collect_table_fences(
            facts,
            &mut generation_fenced,
            &mut dependent_fenced,
            &mut relation_fenced,
        );
        for op in &facts.ops {
            match op {
                ir::CatalogOp::DefineIndex(index) => {
                    relation_names.insert(index.table.name.clone());
                    if index.expression || index.predicate {
                        index_opaque.insert(index.table.name.clone());
                    } else {
                        index_columns
                            .entry(index.table.name.clone())
                            .or_default()
                            .extend(index.columns.iter().cloned());
                    }
                }
                ir::CatalogOp::DefineTable(table) => {
                    relation_names.insert(table.name.clone());
                    // Parent-side foreign keys live on OTHER tables (and
                    // self-references): fence the referenced column, or the
                    // whole parent when its columns are implicit.
                    let mut parents = Vec::new();
                    for column in &table.columns {
                        parents.extend(column.references.iter());
                    }
                    for constraint in &table.constraints {
                        if let ir::Constraint::ForeignKey(key) = constraint {
                            parents.push(key);
                        }
                    }
                    for key in parents {
                        relation_names.insert(key.parent.name.clone());
                        if key.parent_columns.is_empty() {
                            parent_opaque.insert(key.parent.name.clone());
                        } else {
                            parent_columns
                                .entry(key.parent.name.clone())
                                .or_default()
                                .extend(key.parent_columns.iter().cloned());
                        }
                    }
                }
                ir::CatalogOp::DefineTrigger(trigger) => {
                    relation_names.insert(trigger.table.name.clone());
                }
                ir::CatalogOp::AlterTable(alter) => {
                    relation_names.insert(alter.table.name.clone());
                }
                ir::CatalogOp::Drop(drop) => {
                    for object in &drop.objects {
                        if let ir::DropObject::Relation { name, .. } = object {
                            relation_names.insert(name.name.clone());
                        }
                    }
                }
                ir::CatalogOp::Rename(rename) => match rename {
                    ir::RenameOp::Relation { from, to, .. } => {
                        relation_names.insert(from.name.clone());
                        relation_names.insert(to.clone());
                    }
                    ir::RenameOp::Column { table, .. } => {
                        relation_names.insert(table.name.clone());
                    }
                },
                _ => {}
            }
        }
    }
    // Same trailing name, distinct qualification, in any role: without a
    // namespace resolver no spelling can be proven distinct, so every such
    // table fences as ambiguous. Computed once per assembly.
    let mut ambiguous_tables = HashSet::new();
    for first in &relation_names {
        for second in &relation_names {
            if first != second && first.last() == second.last() {
                ambiguous_tables.insert(first.clone());
            }
        }
    }
    let assembly = Assembly {
        views,
        refs,
        opaque_all,
        generation_fenced,
        dependent_fenced,
        relation_fenced,
        index_columns,
        index_opaque,
        parent_columns,
        parent_opaque,
        ambiguous_tables,
    };
    let mut targets = Vec::new();
    for plan in plan_tables(&assembly.views) {
        // Exactly one plain definition binds every insert unambiguously;
        // anything else fences the whole table.
        let unique = plan.defines.len() == 1;
        for (unit_index, define, geometry) in &plan.defines {
            let arity = define.columns.len();
            let create_dup_names = has_duplicate_names(&define.columns);
            for ordinal in 0..arity {
                if !unique {
                    let column = define.columns[ordinal].name.clone();
                    let mut sites = Vec::new();
                    if let Some((_, Some((start, end)))) = geometry
                        .as_ref()
                        .and_then(|create| create.columns.get(ordinal))
                    {
                        sites.push(ColumnSite {
                            stmt: unit_index + 1,
                            start: *start,
                            end: *end,
                            kind: ColumnSiteKind::CreateDef,
                        });
                    }
                    targets.push(blocked_target(
                        &plan.name,
                        column,
                        ordinal,
                        sites,
                        BLOCK_GENERATION,
                    ));
                    continue;
                }
                if let Some(target) = column_target(
                    &assembly,
                    source,
                    &plan.name,
                    &(*unit_index, *define, *geometry),
                    ordinal,
                    create_dup_names,
                ) {
                    targets.push(target);
                }
            }
        }
    }
    targets
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DialectKind, ir::ParseConfig, ir::ServerConfig};

    fn parse_both(sql: &str) -> (Vec<crate::NativeUnit>, Vec<ColumnTarget>) {
        let units = crate::parser::parse_units(
            sql,
            &ParseConfig {
                dialect: DialectKind::SQLite,
                server: ServerConfig::default(),
                chunk_edits: false,
                column_ir: true,
                column_bundles: true,
                syntax_edits: true,
            },
        )
        .expect("parse");
        let targets = assemble_columns(&units, sql);
        (units, targets)
    }

    fn target<'a>(targets: &'a [ColumnTarget], column: &str) -> &'a ColumnTarget {
        targets
            .iter()
            .find(|target| target.column.0 == column)
            .unwrap_or_else(|| panic!("no target for column {column}"))
    }

    fn slices<'a>(sql: &'a str, target: &ColumnTarget) -> Vec<&'a str> {
        target
            .sites
            .iter()
            .map(|site| &sql[site.start..site.end])
            .collect()
    }

    /// Apply bundle sites to the source exactly like the Haskell assembler:
    /// sorted, non-overlapping, single splice.
    fn apply(sql: &str, target: &ColumnTarget) -> String {
        let mut sites = target.sites.clone();
        sites.sort_by_key(|site| (site.stmt, site.start));
        let mut out = String::new();
        let mut cursor = 0;
        for site in &sites {
            assert!(cursor <= site.start, "overlapping sites");
            out.push_str(&sql[cursor..site.start]);
            cursor = site.end;
        }
        out.push_str(&sql[cursor..]);
        out
    }

    fn column_singles(units: &[crate::NativeUnit]) -> Vec<(String, String)> {
        units
            .iter()
            .filter_map(|unit| unit.facts.as_ref())
            .flat_map(|facts| facts.edits.iter())
            .filter(|edit| edit.category == crate::NativeEditCategory::Column)
            .map(|edit| (edit.label.to_string(), edit.replacement.clone()))
            .collect()
    }

    const EXPLICIT: &str = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 10), (3, 20);\nSELECT a FROM t ORDER BY a;";

    #[test]
    fn explicit_bundle_forms_exact_joint_sites() {
        let (_, targets) = parse_both(EXPLICIT);
        assert_eq!(targets.len(), 2, "one bundle per column");
        let dropped = target(&targets, "b");
        assert_eq!(dropped.blocked, None);
        assert_eq!(dropped.ordinal, 1);
        assert_eq!(dropped.sites.len(), 4);
        assert_eq!(
            slices(EXPLICIT, dropped),
            vec![", b INT", ", b", ", 10", ", 20"]
        );
        assert_eq!(
            apply(EXPLICIT, dropped),
            "CREATE TABLE t(a INT);\nINSERT INTO t(a) VALUES (1), (3);\nSELECT a FROM t ORDER BY a;"
        );
        // `a` is read by the projection, so its bundle fences without repair.
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_READ));
    }

    #[test]
    fn implicit_bundle_binds_by_definition_order() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t VALUES (1, 10), (3, 20);";
        let (_, targets) = parse_both(sql);
        let dropped = target(&targets, "b");
        assert_eq!(dropped.blocked, None);
        assert_eq!(dropped.sites.len(), 3);
        assert_eq!(slices(sql, dropped), vec![", b INT", ", 10", ", 20"]);
        assert_eq!(
            apply(sql, dropped),
            "CREATE TABLE t(a INT);\nINSERT INTO t VALUES (1), (3);"
        );
    }

    #[test]
    fn constraint_blocks_only_the_constrained_column() {
        let sql = "CREATE TABLE t(a INT, b INT PRIMARY KEY);\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_CONSTRAINT)
        );
        assert_eq!(target(&targets, "a").blocked, None);
    }

    #[test]
    fn read_elsewhere_blocks_without_repair() {
        let sql =
            "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT b FROM t;";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "b").blocked.as_deref(), Some(BLOCK_READ));
        assert_eq!(target(&targets, "a").blocked, None);
    }

    #[test]
    fn star_projection_blocks() {
        let sql =
            "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT * FROM t;";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_STAR));
        assert_eq!(target(&targets, "b").blocked.as_deref(), Some(BLOCK_STAR));
    }

    #[test]
    fn routine_call_keeps_global_opaque_fence() {
        // `SUM` is legitimately a global opaque barrier in native facts; no
        // whitelist exists to enable it, so every bundle blocks as unknown.
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT SUM(a) FROM t;";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_UNKNOWN)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_UNKNOWN)
        );
    }

    #[test]
    fn parse_error_unit_blocks_everything() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nGARBAGE (((;";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_UNKNOWN)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_UNKNOWN)
        );
    }

    #[test]
    fn parent_fk_blocks_referenced_column() {
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT REFERENCES t(b));\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_DEPENDENT)
        );
        assert_eq!(target(&targets, "a").blocked, None);
    }

    #[test]
    fn implicit_parent_fk_blocks_whole_parent() {
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT, FOREIGN KEY (x) REFERENCES t);\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_DEPENDENT)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_DEPENDENT)
        );
    }

    #[test]
    fn insert_before_create_cannot_bind() {
        let sql = "INSERT INTO t(a, b) VALUES (1, 2);\nCREATE TABLE t(a INT, b INT);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
    }

    #[test]
    fn drop_fences_the_table() {
        let sql =
            "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nDROP TABLE t;";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
    }

    #[test]
    fn arity_mismatch_blocks_listed_columns() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1);";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_SPAN));
        assert_eq!(target(&targets, "b").blocked.as_deref(), Some(BLOCK_SPAN));
    }

    #[test]
    fn ragged_rows_block_implicit_binding() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t VALUES (1, 10), (3);";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_SPAN));
        assert_eq!(target(&targets, "b").blocked.as_deref(), Some(BLOCK_SPAN));
    }

    #[test]
    fn select_source_fences_only_listed_columns() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a) SELECT a FROM u;";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_SHAPE));
        // `b` is untouched by the insert, but its bundle would be a lone
        // `CREATE` site, which is identical to its single and not emitted.
        assert!(targets.iter().all(|target| target.column.0 != "b"));
    }

    #[test]
    fn singleton_list_emits_no_item_sites() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a) VALUES (1);";
        let (units, targets) = parse_both(sql);
        assert!(targets.iter().all(|target| target.column.0 != "b"));
        // Span-only singles still exist for the `CREATE` definition.
        let singles = column_singles(&units);
        assert!(
            singles
                .iter()
                .any(|(label, _)| label == "remove-column_create_def")
        );
        assert!(
            !singles
                .iter()
                .any(|(label, _)| label == "remove-column_insert_item"),
            "singleton list has no deletable item"
        );
    }

    #[test]
    fn singles_survive_bundle_fences() {
        let sql = "CREATE TABLE t(a INT, b INT PRIMARY KEY);\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (units, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_CONSTRAINT)
        );
        let singles = column_singles(&units);
        assert!(
            singles
                .iter()
                .any(|(label, _)| label == "remove-column_create_def")
        );
        assert!(
            singles
                .iter()
                .any(|(label, _)| label == "remove-column_insert_item")
        );
        assert!(
            singles
                .iter()
                .any(|(label, _)| label == "remove-column_values_item")
        );
        assert!(
            singles
                .iter()
                .all(|(_, replacement)| replacement.is_empty())
        );
    }

    #[test]
    fn duplicate_create_names_are_ambiguous() {
        let sql = "CREATE TABLE t(a INT, a INT);\nINSERT INTO t VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        for target in &targets {
            assert_eq!(target.blocked.as_deref(), Some(BLOCK_AMBIGUOUS));
        }
        assert_eq!(targets.len(), 2);
    }

    #[test]
    fn alter_fences_the_table() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ADD COLUMN c INT;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
    }

    #[test]
    fn returning_fences_inserts() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2) RETURNING a;";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_SHAPE));
        assert_eq!(target(&targets, "b").blocked.as_deref(), Some(BLOCK_SHAPE));
    }

    #[test]
    fn first_middle_last_comma_gating() {
        let sql = "CREATE TABLE t(a INT, b INT, c INT);\nINSERT INTO t(a, b, c) VALUES (1, 2, 3);";
        let (_, targets) = parse_both(sql);
        assert_eq!(slices(sql, target(&targets, "a"))[0], "a INT,");
        assert_eq!(slices(sql, target(&targets, "b"))[0], ", b INT");
        assert_eq!(slices(sql, target(&targets, "c"))[0], ", c INT");
        assert_eq!(
            apply(sql, target(&targets, "b")),
            "CREATE TABLE t(a INT, c INT);\nINSERT INTO t(a, c) VALUES (1, 3);"
        );
    }

    #[test]
    fn mixed_qualification_blocks_as_ambiguous() {
        // `main.t` may alias `t`; without a namespace resolver the plan
        // cannot prove the unqualified insert is the only writer.
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nINSERT INTO main.t(a, b) VALUES (3, 4);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_AMBIGUOUS)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_AMBIGUOUS)
        );
    }

    #[test]
    fn transaction_effects_block_as_opaque() {
        // The first family does not model tx snapshots; `BEGIN`/`COMMIT`
        // units carry effects but no separate barrier.
        let sql =
            "BEGIN;\nCREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nCOMMIT;";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_UNKNOWN)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_UNKNOWN)
        );
    }

    #[test]
    fn mixed_star_and_explicit_read_still_blocks() {
        // A star mixed with explicit expressions still observes every column.
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT t1.*, t2.a FROM t AS t1 JOIN t AS t2 ON t1.a = t2.a;";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_STAR));
        assert_eq!(target(&targets, "b").blocked.as_deref(), Some(BLOCK_STAR));
    }

    #[test]
    fn temporary_tables_are_outside_first_family() {
        let sql = "CREATE TEMP TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
    }
}

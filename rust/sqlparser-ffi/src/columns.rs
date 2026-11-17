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
//! Fence boundary (first family + generation solver + namespace resolver):
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
//!   pre-existing sequence/routine analysis marks every non-pure function
//!   call as a global opaque barrier; only the per-dialect pure-builtin
//!   table (`crate::pure`: `abs`, `coalesce`, `concat`, `length`, `lower`,
//!   `nullif`, `round`, `upper` on SQLite/PostgreSQL/MySQL, unqualified and
//!   without window/filter/grouping shapes) lifts that fence. Unknown and
//!   volatile calls, aggregates (including `SUM`, `COUNT`), qualified
//!   calls, and any call in a script containing routine DDL still fence,
//!   so first-family proofs keep routine-free projections such as
//!   `SELECT a FROM t ORDER BY a` whenever a fence-free unit is needed.
//! - Generation account, per table (a single `CREATE TABLE` generation only;
//!   zero/non-unique definitions fence the whole table):
//!   - Operations on other tables (alters, renames, drops) never fence this
//!     table, even when they name an identical column.
//!   - `ADD COLUMN other`, `DROP COLUMN other`, `RENAME COLUMN`
//!     other-to-other, and `ADD CONSTRAINT` without the column lift for
//!     that column: explicit column lists bind by name, so a change
//!     elsewhere in the definition cannot move this column's items.
//!     Anything naming the column fences it.
//!   - `ADD`/`DROP COLUMN` still fence all when any positional (implicit
//!     `VALUES`) insert binds: `FIRST`/`AFTER` placement is not in the IR,
//!     so append is unprovable and ordinals may shift.
//!   - `ALTER COLUMN` narrows per op: no-expression ops (`SET`/`DROP NOT
//!     NULL, `DROP DEFAULT`, pure type change, identity/foreign-table
//!     options) fence the target only; `USING`/`SET DEFAULT` (and
//!     `CHANGE`/`MODIFY` new-definition expressions) fence the target
//!     plus the columns they read; `FIRST`/`AFTER` placement fences all
//!     (ordinals may shift, and the `AFTER` anchor names an uncarried
//!     sibling). `CHANGE`/`MODIFY` definitions with unparsed dialect
//!     tokens or implicit self-referencing parent columns stay opaque
//!     and fence all, as do generated `ADD COLUMN` expressions, opaque
//!     constraints, and name-addressed changes (`DROP/RENAME CONSTRAINT`,
//!     `DROP INDEX`, `VALIDATE`).
//!   - Table renames are followed (sites before the rename bind the old
//!     name, sites after bind the new name) only when unambiguous: one
//!     outgoing rename per name, strictly ordered after the definition, no
//!     cycle or self-rename, no second `CREATE` generation for any followed
//!     name, no rename into the chain from outside, and no rename sharing
//!     its unit with an alter of the generation. Anything else fences all.
//!     Stale uses (a generation name outside its active segment) bind no
//!     sites; fence checks still scan the whole chain.
//!   - Drops are never followed across: any drop of a generation name
//!     fences the whole table. `LIKE`, table sources, `ON COMMIT` actions,
//!     temporary tables, and pre-definition inserts still fence the whole
//!     table exactly as before.
//! - Namespace resolver (no catalog; canonical spellings only):
//!   - Identical canonical spellings (after `IdentifierRules` folding) are
//!     the same table: case/quoting variants that fold together never fence
//!     each other.
//!   - Genuinely distinct spellings with the same trailing name (e.g. `t`
//!     vs `main.t`) may alias, and neither same-nor-distinct is provable
//!     without `search_path`/catalog reasoning (which this layer never
//!     receives: [`assemble_columns`] sees units plus source only). A
//!     sibling spelling in any DDL role (second definition, alter, drop,
//!     rename, index/trigger host) keeps the whole-table fence. Sibling DML
//!     uses narrow to the columns they can touch under the same-table
//!     hypothesis: explicit lists touch their listed columns; implicit or
//!     exotic shapes, stars, unknown sources, and implicit-column
//!     foreign-key parents touch all.

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

/// Dependent fences (any trigger hosted on the table — `WHEN`/body effects
/// are opaque) and relation-scoped barrier fences. Generation fences
/// (`ALTER TABLE`, renames, drops) live in the per-table [`TableScope`].
fn collect_table_fences(
    facts: &ir::NativeFacts,
    dependent: &mut HashSet<ir::Name>,
    relation: &mut HashSet<ir::Name>,
) {
    for op in &facts.ops {
        if let ir::CatalogOp::DefineTrigger(trigger) = op {
            dependent.insert(trigger.table.name.clone());
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

/// One table-rename edge with its defining unit (0-based statement index).
#[derive(Clone, Debug)]
struct RelRename {
    unit: usize,
    from: ir::NameRef,
    to: ir::Name,
}

/// A per-table fence with an all-columns flag plus per-column touches.
#[derive(Debug, Default)]
struct FenceSet {
    all: bool,
    touched: HashSet<ir::Ident>,
}

/// Per-table generation + namespace scope, computed ONCE per plan (never per
/// column) from the assembly's precomputed material.
struct TableScope {
    /// Followed generation names: `[define, rename1, rename2, ...]`.
    /// Longer than one entry only when every rename on the path is
    /// unambiguous (see [`build_chain`]).
    chain: Vec<ir::Name>,
    /// Units of the followed rename edges, parallel to `chain[1..]`,
    /// strictly increasing.
    edge_units: Vec<usize>,
    /// Generation fences ([`BLOCK_GENERATION`]).
    generation: FenceSet,
    /// Sibling-spelling fences ([`BLOCK_AMBIGUOUS`]).
    amb: FenceSet,
}

impl TableScope {
    /// Generation name active at `unit` (0-based): the last rename target
    /// strictly before `unit`, else the define name. A rename in `unit`
    /// itself never rebinds `unit`: single-statement units cannot observe
    /// their own rename, and a rename sharing its unit with an alter of the
    /// generation fences the whole table instead.
    fn active(&self, unit: usize) -> &ir::Name {
        let mut active = &self.chain[0];
        for (edge, to) in self.edge_units.iter().zip(self.chain.iter().skip(1)) {
            if *edge < unit {
                active = to;
            } else {
                break;
            }
        }
        active
    }
}

/// Follow unambiguous table renames from `define` (defined in `define_unit`).
/// Returns the generation chain with its edge units, or `None` when the
/// generation is broken and every column must fence with
/// [`BLOCK_GENERATION`]: a forked generation, a rename at or before the
/// definition, a self-rename or cycle, a rename target colliding with any
/// `CREATE TABLE` name (a second generation for that name), a rename into
/// the chain from outside, a temporary-spelled rename touching the chain
/// (never followed), any drop of a chain name (never followed across a
/// drop), or a rename sharing its unit with an alter of the generation.
fn build_chain(
    assembly: &Assembly<'_>,
    define: &ir::Name,
    define_unit: usize,
) -> Option<(Vec<ir::Name>, Vec<usize>)> {
    let mut chain = vec![define.clone()];
    let mut edge_units = Vec::new();
    let mut visited = HashSet::new();
    visited.insert(define.clone());
    let mut previous = define_unit;
    loop {
        let current = chain.last().expect("non-empty chain").clone();
        let outgoing: Vec<_> = assembly
            .rel_renames
            .iter()
            .filter(|edge| !edge.from.temp && edge.from.name == current)
            .collect();
        if outgoing.is_empty() {
            break;
        }
        if outgoing.len() > 1 {
            return None;
        }
        let first = outgoing[0];
        if first.unit <= previous {
            return None;
        }
        if first.to == current || !visited.insert(first.to.clone()) {
            return None;
        }
        if assembly.define_names.contains(&first.to) {
            return None;
        }
        chain.push(first.to.clone());
        edge_units.push(first.unit);
        previous = first.unit;
    }
    let in_chain = |name: &ir::Name| chain.contains(name);
    for edge in &assembly.rel_renames {
        if edge.from.temp {
            // Temporary renames are never followed; touching the chain
            // fences exactly as the old whole-table rule did.
            if in_chain(&edge.from.name) || in_chain(&edge.to) {
                return None;
            }
            continue;
        }
        // A rename into the chain from outside produces a second generation
        // for that name. (Renames out of the chain are all chain edges:
        // every chain name's outgoing set was checked single above.)
        if in_chain(&edge.to) && !in_chain(&edge.from.name) {
            return None;
        }
    }
    if assembly.drops.iter().any(in_chain) {
        return None;
    }
    // A rename sharing its unit with an alter of the generation: the
    // intra-unit order is unknowable, so the generation is ambiguous.
    for unit in &edge_units {
        let altered = assembly.views.get(*unit).is_some_and(|view| {
            view.facts.is_some_and(|facts| {
                facts.ops.iter().any(|op| {
                    matches!(op, ir::CatalogOp::AlterTable(alter) if in_chain(&alter.table.name))
                })
            })
        });
        if altered {
            return None;
        }
    }
    Some((chain, edge_units))
}

/// Fold one `ALTER TABLE` change targeting the generation into `generation`:
/// - `ADD COLUMN other` (non-generated, classified constraints) and
///   `DROP COLUMN other` lift for that column when no positional (implicit
///   `VALUES`) insert binds: explicit lists bind by name, so an
///   order/arity change elsewhere cannot move this column's items. Any
///   implicit insert fences all (`FIRST`/`AFTER` is not in the IR, so
///   append is unprovable and ordinals may shift).
/// - `ADD CONSTRAINT` lifts column-wise via its classified column lists;
///   opaque constraints fence all, as do self-referencing foreign keys
///   with implicit parent columns.
/// - `ALTER COLUMN` narrows: the target plus the op's referenced columns
///   fence, except `FIRST`/`AFTER` placement fences all (ordinals may
///   shift and the `AFTER` anchor is uncarried). `CHANGE`/`MODIFY`
///   rewrites whose definition is opaque degrade to `Other` at emission.
/// - Generated `ADD COLUMN` expressions and name-addressed changes
///   (`DROP/RENAME CONSTRAINT`, `DROP INDEX`, `VALIDATE`, `Other`) fence
///   all: names do not map to columns here.
fn fold_generation_change(
    chain: &[ir::Name],
    implicit: bool,
    change: &ir::AlterChange,
    generation: &mut FenceSet,
) {
    match change {
        ir::AlterChange::AddColumn {
            column,
            constraints,
        } => {
            generation.touched.insert(column.name.clone());
            if column.generated {
                // A generated expression may read any column; the IR keeps
                // only the flag, not the expression's columns.
                generation.all = true;
            }
            if implicit {
                generation.all = true;
            }
            for constraint in constraints {
                fold_constraint(chain, constraint, generation);
            }
            for reference in &column.references {
                if chain.contains(&reference.parent.name) {
                    if reference.parent_columns.is_empty() {
                        generation.all = true;
                    } else {
                        generation
                            .touched
                            .extend(reference.parent_columns.iter().cloned());
                    }
                }
            }
        }
        ir::AlterChange::DropColumn { column, .. } => {
            generation.touched.insert(column.clone());
            if implicit {
                generation.all = true;
            }
        }
        ir::AlterChange::AddConstraint { constraint } => {
            fold_constraint(chain, constraint, generation);
        }
        ir::AlterChange::AlterColumn {
            column,
            referenced,
            repositions,
            ..
        } => {
            generation.touched.insert(column.clone());
            generation.touched.extend(referenced.iter().cloned());
            if *repositions {
                generation.all = true;
            }
        }
        ir::AlterChange::DropConstraint { .. }
        | ir::AlterChange::DropIndex { .. }
        | ir::AlterChange::RenameConstraint { .. }
        | ir::AlterChange::ValidateConstraint { .. }
        | ir::AlterChange::Other { .. } => {
            generation.all = true;
        }
    }
}

/// Fold a classified constraint on the generation into `generation`. Opaque
/// constraints and self-referencing foreign keys with implicit parent
/// columns fence all; otherwise the named child (and, for self-references,
/// parent) columns fence.
fn fold_constraint(chain: &[ir::Name], constraint: &ir::Constraint, generation: &mut FenceSet) {
    match constraint {
        ir::Constraint::PrimaryKey { columns, .. }
        | ir::Constraint::Unique { columns, .. }
        | ir::Constraint::Check { columns, .. }
        | ir::Constraint::Index { columns, .. } => {
            generation.touched.extend(columns.iter().cloned());
        }
        ir::Constraint::ForeignKey(key) => {
            generation.touched.extend(key.child_columns.iter().cloned());
            if chain.contains(&key.parent.name) {
                if key.parent_columns.is_empty() {
                    generation.all = true;
                } else {
                    generation
                        .touched
                        .extend(key.parent_columns.iter().cloned());
                }
            }
        }
        ir::Constraint::UsingIndex { .. } | ir::Constraint::Other { .. } => {
            generation.all = true;
        }
    }
}

/// Build the per-table scope for `define` (defined in `define_unit`).
/// Generation ops on other tables are ignored; column renames on the chain
/// fence their two endpoints; sibling-spelling ambiguity folds in last.
fn build_scope(assembly: &Assembly<'_>, define: &ir::Name, define_unit: usize) -> TableScope {
    let mut scope = TableScope {
        chain: vec![define.clone()],
        edge_units: Vec::new(),
        generation: FenceSet::default(),
        amb: FenceSet::default(),
    };
    let Some((chain, edge_units)) = build_chain(assembly, define, define_unit) else {
        scope.generation.all = true;
        build_sibling_scope(assembly, &scope.chain, &mut scope.amb);
        return scope;
    };
    scope.chain = chain;
    scope.edge_units = edge_units;
    // Positional inserts bind every column by ordinal; any column-set/order
    // change then fences all (see [`fold_generation_change`]).
    let mut implicit = false;
    for view in &assembly.views {
        let Some(facts) = view.facts else { continue };
        for insert in &facts.column.inserts {
            let binds = insert
                .target
                .as_ref()
                .is_some_and(|target| scope.chain.contains(&target.name));
            if binds && insert.shape == InsertShape::PlainValues && insert.explicit.is_none() {
                implicit = true;
            }
        }
    }
    for view in &assembly.views {
        let Some(facts) = view.facts else { continue };
        for op in &facts.ops {
            match op {
                ir::CatalogOp::AlterTable(alter) if scope.chain.contains(&alter.table.name) => {
                    for change in &alter.changes {
                        fold_generation_change(
                            &scope.chain,
                            implicit,
                            change,
                            &mut scope.generation,
                        );
                    }
                }
                ir::CatalogOp::Rename(ir::RenameOp::Column { table, from, to })
                    if scope.chain.contains(&table.name) =>
                {
                    scope.generation.touched.insert(from.clone());
                    scope.generation.touched.insert(to.clone());
                }
                _ => {}
            }
        }
    }
    build_sibling_scope(assembly, &scope.chain, &mut scope.amb);
    scope
}

/// Fold sibling-spelling ambiguity (same trailing name, distinct canonical
/// spelling, e.g. `t` vs `main.t`) into `amb`. Identical canonical
/// spellings are the same table and never fence here. A sibling in any DDL
/// role fences all; sibling DML uses fence only the columns they can touch
/// under the same-table hypothesis: explicit lists touch their listed
/// columns, while implicit or exotic shapes, stars, unknown sources, and
/// implicit-column foreign-key parents touch all.
fn build_sibling_scope(assembly: &Assembly<'_>, chain: &[ir::Name], amb: &mut FenceSet) {
    let trailings: HashSet<&ir::Ident> = chain.iter().filter_map(|name| name.last()).collect();
    let siblings: HashSet<&ir::Name> = assembly
        .relation_names
        .iter()
        .filter(|name| {
            !chain.contains(name) && name.last().is_some_and(|last| trailings.contains(last))
        })
        .collect();
    if siblings.is_empty() {
        return;
    }
    if assembly
        .ddl_names
        .iter()
        .any(|name| siblings.contains(name))
    {
        amb.all = true;
        return;
    }
    let is_sibling = |name: &ir::Name| siblings.contains(name);
    for refs in assembly.refs.iter().flatten() {
        if refs.stars.iter().any(|star| is_sibling(&star.name)) {
            amb.all = true;
        }
        for read in &refs.reads {
            if is_sibling(&read.source.name) {
                amb.touched.extend(read.columns.iter().cloned());
            }
        }
        for (target, columns) in &refs.writes {
            if is_sibling(target) {
                amb.touched.extend(columns.iter().cloned());
            }
        }
        if refs.unknown_source && refs.touched.iter().any(&is_sibling) {
            amb.all = true;
        }
    }
    for view in &assembly.views {
        let Some(facts) = view.facts else { continue };
        for insert in &facts.column.inserts {
            let Some(target) = &insert.target else {
                continue;
            };
            if !is_sibling(&target.name) {
                continue;
            }
            match insert.shape {
                InsertShape::Exotic => amb.all = true,
                InsertShape::Neutral => {}
                InsertShape::SelectSource | InsertShape::PlainValues => match &insert.explicit {
                    Some(list) => {
                        amb.touched
                            .extend(list.iter().map(|(name, _)| name.clone()));
                    }
                    None => amb.all = true,
                },
            }
        }
    }
    for (parent, columns) in &assembly.parent_columns {
        if is_sibling(parent) {
            amb.touched.extend(columns.iter().cloned());
        }
    }
    if assembly.parent_opaque.iter().any(is_sibling) {
        amb.all = true;
    }
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
    dependent_fenced: HashSet<ir::Name>,
    relation_fenced: HashSet<ir::Name>,
    index_columns: HashMap<ir::Name, Vec<ir::Ident>>,
    index_opaque: HashSet<ir::Name>,
    /// Parent-side foreign-key targets: referenced columns per parent table.
    parent_columns: HashMap<ir::Name, HashSet<ir::Ident>>,
    /// Parents referenced with implicit (unknown) columns: fence the whole
    /// parent table, including self-references.
    parent_opaque: HashSet<ir::Name>,
    /// Every relation name in any role (temp-blind, canonical). Membership
    /// is exactly the old ambiguity input set.
    relation_names: HashSet<ir::Name>,
    /// Relation names in generation/DDL roles (definitions, alters, drops,
    /// renames, index/trigger hosts). A subset of `relation_names`.
    ddl_names: HashSet<ir::Name>,
    /// Every `CREATE TABLE` name (rename-target collision checks).
    define_names: HashSet<ir::Name>,
    /// Table renames in unit order.
    rel_renames: Vec<RelRename>,
    /// Dropped relation names (temp-blind). Any drop of a generation name
    /// breaks the generation.
    drops: HashSet<ir::Name>,
}

/// Reference fences (reads, writes, stars, unknown sources) for one column
/// over its whole generation chain. Returns the blocking cause, if any.
/// Only exact column matches and actual star covers fence; plain
/// projections that avoid the column never do.
fn reference_fence(
    assembly: &Assembly<'_>,
    chain: &[ir::Name],
    column: &ir::Ident,
) -> Option<&'static str> {
    let in_chain = |name: &ir::Name| chain.contains(name);
    for refs in assembly.refs.iter().flatten() {
        // Star cover fences independently of what else the unit reads: a
        // `SELECT *` mixed with explicit expressions still observes every
        // column. Empty-column reads without cover (`SELECT 1`, bare
        // `DELETE`) observe none and never fence.
        if refs.stars.iter().any(|star| in_chain(&star.name)) {
            return Some(BLOCK_STAR);
        }
        for read in &refs.reads {
            if in_chain(&read.source.name) && read.columns.contains(column) {
                return Some(BLOCK_READ);
            }
        }
        for (target, columns) in &refs.writes {
            if in_chain(target) && columns.contains(column) {
                return Some(BLOCK_READ);
            }
        }
        if refs.unknown_source && refs.touched.iter().any(&in_chain) {
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
    scope: &TableScope,
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
    let in_chain = |name: &ir::Name| scope.chain.contains(name);
    if scope.generation.all || scope.generation.touched.contains(&column) {
        blocked = blocked.or(Some(BLOCK_GENERATION));
    }
    if scope.amb.all || scope.amb.touched.contains(&column) {
        blocked = blocked.or(Some(BLOCK_AMBIGUOUS));
    }
    if assembly.relation_fenced.iter().any(&in_chain) {
        blocked = blocked.or(Some(BLOCK_UNKNOWN));
    }
    if assembly.dependent_fenced.iter().any(&in_chain) {
        blocked = blocked.or(Some(BLOCK_DEPENDENT));
    }
    if assembly.index_opaque.iter().any(&in_chain) {
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
    if scope.chain.iter().any(|name| {
        assembly
            .index_columns
            .get(name)
            .is_some_and(|indexed| indexed.contains(&column))
    }) {
        blocked = blocked.or(Some(BLOCK_DEPENDENT));
    }
    if assembly.parent_opaque.iter().any(in_chain) {
        blocked = blocked.or(Some(BLOCK_DEPENDENT));
    }
    if scope.chain.iter().any(|name| {
        assembly
            .parent_columns
            .get(name)
            .is_some_and(|parents| parents.contains(&column))
    }) {
        blocked = blocked.or(Some(BLOCK_DEPENDENT));
    }
    if blocked.is_none() {
        blocked = reference_fence(assembly, &scope.chain, &column);
    }
    // Insert contributions, in unit order. Each insert binds the generation
    // name active at its unit; the single definition must precede every
    // matched insert, and anything earlier cannot bind to it.
    if blocked.is_none() {
        let create_arity = define.columns.len();
        for view in &assembly.views {
            let Some(facts) = view.facts else { continue };
            for insert in &facts.column.inserts {
                let Some(target) = &insert.target else {
                    continue;
                };
                if target.name != *scope.active(view.index) {
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
    let mut dependent_fenced = HashSet::new();
    let mut relation_fenced = HashSet::new();
    let mut index_columns: HashMap<ir::Name, Vec<ir::Ident>> = HashMap::new();
    let mut index_opaque = HashSet::new();
    let mut parent_columns: HashMap<ir::Name, HashSet<ir::Ident>> = HashMap::new();
    let mut parent_opaque = HashSet::new();
    let mut relation_names: HashSet<ir::Name> = HashSet::new();
    let mut ddl_names: HashSet<ir::Name> = HashSet::new();
    let mut define_names: HashSet<ir::Name> = HashSet::new();
    let mut rel_renames: Vec<RelRename> = Vec::new();
    let mut drops: HashSet<ir::Name> = HashSet::new();
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
        collect_table_fences(facts, &mut dependent_fenced, &mut relation_fenced);
        for op in &facts.ops {
            match op {
                ir::CatalogOp::DefineIndex(index) => {
                    relation_names.insert(index.table.name.clone());
                    ddl_names.insert(index.table.name.clone());
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
                    ddl_names.insert(table.name.clone());
                    define_names.insert(table.name.clone());
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
                    ddl_names.insert(trigger.table.name.clone());
                }
                ir::CatalogOp::AlterTable(alter) => {
                    relation_names.insert(alter.table.name.clone());
                    ddl_names.insert(alter.table.name.clone());
                    // `ALTER TABLE u ADD ... REFERENCES t(b)` introduces a
                    // parent-side foreign key exactly like `CREATE TABLE`;
                    // fence the referenced column (or the whole parent when
                    // its columns are implicit), same as above.
                    let mut parents = Vec::new();
                    for change in &alter.changes {
                        match change {
                            ir::AlterChange::AddColumn {
                                column,
                                constraints,
                            } => {
                                parents.extend(column.references.iter());
                                for constraint in constraints {
                                    if let ir::Constraint::ForeignKey(key) = constraint {
                                        parents.push(key);
                                    }
                                }
                            }
                            ir::AlterChange::AddConstraint {
                                constraint: ir::Constraint::ForeignKey(key),
                            } => {
                                parents.push(key);
                            }
                            _ => {}
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
                ir::CatalogOp::Drop(drop) => {
                    for object in &drop.objects {
                        if let ir::DropObject::Relation { name, .. } = object {
                            relation_names.insert(name.name.clone());
                            ddl_names.insert(name.name.clone());
                            drops.insert(name.name.clone());
                        }
                    }
                }
                ir::CatalogOp::Rename(rename) => match rename {
                    ir::RenameOp::Relation { from, to, .. } => {
                        relation_names.insert(from.name.clone());
                        relation_names.insert(to.clone());
                        ddl_names.insert(from.name.clone());
                        ddl_names.insert(to.clone());
                        rel_renames.push(RelRename {
                            unit: view.index,
                            from: from.clone(),
                            to: to.clone(),
                        });
                    }
                    ir::RenameOp::Column { table, .. } => {
                        relation_names.insert(table.name.clone());
                        ddl_names.insert(table.name.clone());
                    }
                },
                _ => {}
            }
        }
    }
    let assembly = Assembly {
        views,
        refs,
        opaque_all,
        dependent_fenced,
        relation_fenced,
        index_columns,
        index_opaque,
        parent_columns,
        parent_opaque,
        relation_names,
        ddl_names,
        define_names,
        rel_renames,
        drops,
    };
    let mut targets = Vec::new();
    for plan in plan_tables(&assembly.views) {
        // Exactly one plain definition binds every insert unambiguously;
        // anything else fences the whole table. The per-table scope
        // (generation chain plus sibling ambiguity) builds once per plan.
        let unique = plan.defines.len() == 1;
        let scope = unique.then(|| build_scope(&assembly, &plan.name, plan.defines[0].0));
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
                    scope.as_ref().expect("unique plan has scope"),
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

    fn parse_dialect(
        sql: &str,
        dialect: DialectKind,
    ) -> (Vec<crate::NativeUnit>, Vec<ColumnTarget>) {
        let units = crate::parser::parse_units(
            sql,
            &ParseConfig {
                dialect,
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

    fn parse_both(sql: &str) -> (Vec<crate::NativeUnit>, Vec<ColumnTarget>) {
        parse_dialect(sql, DialectKind::SQLite)
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
        // `frobnicate` stays outside the pure-builtin tables, so it is
        // legitimately a global opaque barrier in native facts and every
        // bundle blocks as unknown.
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT frobnicate(a) FROM t;";
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
    fn alter_parent_fk_blocks_referenced_column() {
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT);\nALTER TABLE u ADD FOREIGN KEY (x) REFERENCES t(b);\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_DEPENDENT)
        );
        assert_eq!(target(&targets, "a").blocked, None);
    }

    #[test]
    fn alter_implicit_parent_fk_blocks_whole_parent() {
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT);\nALTER TABLE u ADD FOREIGN KEY (x) REFERENCES t;\nINSERT INTO t(a, b) VALUES (1, 2);";
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
    fn alter_add_column_parent_fk_blocks_referenced_column() {
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT);\nALTER TABLE u ADD COLUMN y INT REFERENCES t(b);\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_DEPENDENT)
        );
        assert_eq!(target(&targets, "a").blocked, None);
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
    fn alter_other_table_never_fences() {
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT);\nALTER TABLE u ADD COLUMN y INT;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "a").blocked, None);
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn alter_other_table_same_column_name_never_fences() {
        // Even a whole-table-fencing alter kind on `u` naming `t`'s column
        // never fences `t`: per-table isolation is by table, not by name.
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(a INT);\nALTER TABLE u ALTER COLUMN a SET NOT NULL;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert_eq!(target(&targets, "a").blocked, None);
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn add_column_other_lifts_with_explicit_inserts() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ADD COLUMN c INT;\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT a FROM t ORDER BY a;";
        let (_, targets) = parse_both(sql);
        let dropped = target(&targets, "b");
        assert_eq!(dropped.blocked, None);
        assert_eq!(dropped.sites.len(), 3);
        assert_eq!(slices(sql, dropped), vec![", b INT", ", b", ", 2"]);
        assert_eq!(
            apply(sql, dropped),
            "CREATE TABLE t(a INT);\nALTER TABLE t ADD COLUMN c INT;\nINSERT INTO t(a) VALUES (1);\nSELECT a FROM t ORDER BY a;"
        );
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_READ));
    }

    #[test]
    fn add_column_other_still_fences_implicit_inserts() {
        // `FIRST`/`AFTER` placement is not in the IR, so append is
        // unprovable and positional binding may shift.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ADD COLUMN c INT;\nINSERT INTO t VALUES (1, 2);";
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
    fn add_column_touching_name_fences_only_it() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ADD COLUMN b INT;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "a").blocked, None);
    }

    #[test]
    fn add_column_generated_fences_all() {
        // The generated expression may read any column; the IR keeps only
        // the flag, not the expression's columns.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ADD COLUMN c INT GENERATED ALWAYS AS (a + b);\nINSERT INTO t(a, b) VALUES (1, 2);";
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
    fn add_constraint_without_column_lifts() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ADD CHECK (a > 0);\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn add_constraint_opaque_fences_all() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ADD PRIMARY KEY USING INDEX pk_t;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
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
    fn drop_column_other_lifts_with_explicit_inserts() {
        let sql = "CREATE TABLE t(a INT, b INT, c INT);\nALTER TABLE t DROP COLUMN c;\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT a FROM t ORDER BY a;";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "b").blocked, None);
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_READ));
    }

    #[test]
    fn drop_column_other_still_fences_implicit_inserts() {
        let sql = "CREATE TABLE t(a INT, b INT, c INT);\nALTER TABLE t DROP COLUMN c;\nINSERT INTO t VALUES (1, 2, 3);";
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
    fn drop_column_self_fences_only_it() {
        let sql = "CREATE TABLE t(a INT, b INT, c INT);\nALTER TABLE t DROP COLUMN b;\nINSERT INTO t(a, c) VALUES (1, 3);\nSELECT a FROM t ORDER BY a;";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "c").blocked, None);
    }

    #[test]
    fn rename_column_other_lifts() {
        let sql = "CREATE TABLE t(a INT, b INT, c INT);\nALTER TABLE t RENAME COLUMN c TO d;\nINSERT INTO t(a, b, d) VALUES (1, 2, 3);\nSELECT a FROM t ORDER BY a;";
        let (_, targets) = parse_both(sql);
        let dropped = target(&targets, "b");
        assert_eq!(dropped.blocked, None);
        assert_eq!(slices(sql, dropped), vec![", b INT", ", b", ", 2"]);
        assert_eq!(
            target(&targets, "c").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
    }

    #[test]
    fn rename_column_self_fences_only_it() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t RENAME COLUMN b TO b2;\nINSERT INTO t(a, b2) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "a").blocked, None);
    }

    #[test]
    fn alter_column_no_expr_fences_target_only() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ALTER COLUMN a SET NOT NULL;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn alter_column_pure_type_fences_target_only() {
        // No `USING`: the op names the target only, so siblings lift.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ALTER COLUMN a TYPE BIGINT;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn alter_column_using_fences_target_and_refs() {
        let sql = "CREATE TABLE t(a INT, b INT, c INT);\nALTER TABLE t ALTER COLUMN a TYPE BIGINT USING (a + b);\nINSERT INTO t(a, b, c) VALUES (1, 2, 3);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "c").blocked, None);
    }

    #[test]
    fn alter_column_set_default_fences_target_and_refs() {
        let sql = "CREATE TABLE t(a INT, b INT, c INT);\nALTER TABLE t ALTER COLUMN a SET DEFAULT (b + 1);\nINSERT INTO t(a, b, c) VALUES (1, 2, 3);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "c").blocked, None);
    }

    #[test]
    fn alter_column_identity_fences_target_only() {
        // Sequence options carry no column references here, so only the
        // target fences.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ALTER COLUMN a ADD GENERATED ALWAYS AS IDENTITY (START WITH 10);\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn alter_column_generic_options_fence_target_only() {
        // Foreign-table option keys/values are FDW names, never same-table
        // columns.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ALTER COLUMN a OPTIONS (ADD x 'y');\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn modify_column_plain_fences_target_only() {
        // Generic parses the same MySQL rewrite syntax (`MODIFY`, `CHANGE`,
        // `FIRST`/`AFTER`, `AUTO_INCREMENT`) without MySQL's implicit-commit
        // DDL effects, which fence every bundle as unknown.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t MODIFY COLUMN a INT NOT NULL;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::Generic);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn modify_column_default_ref_fences_target_and_refs() {
        let sql = "CREATE TABLE t(a INT, b INT, c INT);\nALTER TABLE t MODIFY COLUMN a INT DEFAULT (b);\nINSERT INTO t(a, b, c) VALUES (1, 2, 3);";
        let (_, targets) = parse_dialect(sql, DialectKind::Generic);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "c").blocked, None);
    }

    #[test]
    fn modify_column_after_fences_all() {
        // `AFTER` repositions: ordinals may shift and the anchor names an
        // uncarried sibling.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t MODIFY COLUMN a INT AFTER b;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::Generic);
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
    fn change_column_first_fences_all() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t CHANGE COLUMN a a INT FIRST;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::Generic);
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
    fn change_column_rename_narrows_other() {
        // Rename `a` to `a2` plus a plain rewrite: `a` fences as the
        // rename source, `b` is untouched and lifts.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t CHANGE COLUMN a a2 INT NOT NULL;\nINSERT INTO t(a2, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::Generic);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn modify_column_auto_increment_stays_fenced() {
        // `AUTO_INCREMENT` parses to unparsed dialect `Token` text, so
        // absence of column references is unprovable: opaque, fences all.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t MODIFY COLUMN a INT AUTO_INCREMENT;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::Generic);
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
    fn modify_column_self_fk_explicit_narrows() {
        let sql = "CREATE TABLE t(a INT, b INT, c INT);\nALTER TABLE t MODIFY COLUMN a INT REFERENCES t(b);\nINSERT INTO t(a, b, c) VALUES (1, 2, 3);";
        let (_, targets) = parse_dialect(sql, DialectKind::Generic);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_GENERATION)
        );
        assert_eq!(target(&targets, "c").blocked, None);
    }

    #[test]
    fn modify_column_self_fk_implicit_stays_fenced() {
        // Implicit parent columns mean the parent key: unnameable here,
        // so the rewrite stays opaque and fences all.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t MODIFY COLUMN a INT REFERENCES t;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::Generic);
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
    fn drop_constraint_still_fences_all() {
        // Constraint names do not map to columns here.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t DROP CONSTRAINT c;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
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
        // `main.t` may alias `t`, and the sibling insert lists both columns,
        // so neither column's writer set is provable without a catalog.
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

    #[test]
    fn rename_table_followed() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t RENAME TO u;\nINSERT INTO u(a, b) VALUES (1, 2);\nSELECT a FROM u ORDER BY a;";
        let (_, targets) = parse_both(sql);
        let dropped = target(&targets, "b");
        assert_eq!(dropped.blocked, None);
        assert_eq!(dropped.sites.len(), 3);
        assert_eq!(
            dropped
                .sites
                .iter()
                .map(|site| site.stmt)
                .collect::<Vec<_>>(),
            vec![1, 3, 3]
        );
        assert_eq!(slices(sql, dropped), vec![", b INT", ", b", ", 2"]);
        assert_eq!(
            apply(sql, dropped),
            "CREATE TABLE t(a INT);\nALTER TABLE t RENAME TO u;\nINSERT INTO u(a) VALUES (1);\nSELECT a FROM u ORDER BY a;"
        );
        // The post-rename read binds the followed generation.
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_READ));
    }

    #[test]
    fn rename_chain_two_hops_binds_each_segment() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nALTER TABLE t RENAME TO u;\nINSERT INTO u(a, b) VALUES (3, 4);\nALTER TABLE u RENAME TO v;\nINSERT INTO v(a, b) VALUES (5, 6);";
        let (_, targets) = parse_both(sql);
        let dropped = target(&targets, "b");
        assert_eq!(dropped.blocked, None);
        assert_eq!(dropped.sites.len(), 7);
        assert_eq!(
            dropped
                .sites
                .iter()
                .map(|site| site.stmt)
                .collect::<Vec<_>>(),
            vec![1, 2, 2, 4, 4, 6, 6]
        );
        assert_eq!(
            apply(sql, dropped),
            "CREATE TABLE t(a INT);\nINSERT INTO t(a) VALUES (1);\nALTER TABLE t RENAME TO u;\nINSERT INTO u(a) VALUES (3);\nALTER TABLE u RENAME TO v;\nINSERT INTO v(a) VALUES (5);"
        );
    }

    #[test]
    fn rename_stale_insert_binds_no_sites() {
        // The post-rename `INSERT INTO t` cannot name the generation (now
        // `u`), so it contributes no sites and fences nothing.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t RENAME TO u;\nINSERT INTO u(a, b) VALUES (1, 2);\nINSERT INTO t(a, b) VALUES (3, 4);";
        let (_, targets) = parse_both(sql);
        let dropped = target(&targets, "b");
        assert_eq!(dropped.blocked, None);
        assert_eq!(dropped.sites.len(), 3);
        assert_eq!(slices(sql, dropped), vec![", b INT", ", b", ", 2"]);
    }

    #[test]
    fn rename_fork_fences() {
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t RENAME TO u;\nALTER TABLE t RENAME TO v;\nINSERT INTO u(a, b) VALUES (1, 2);";
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
    fn rename_before_create_fences() {
        let sql = "ALTER TABLE t RENAME TO u;\nCREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);";
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
    fn rename_onto_created_name_fences() {
        // `u` already has its own `CREATE` generation; the rename target is
        // a second generation for that name.
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT);\nALTER TABLE t RENAME TO u;\nINSERT INTO u(a, b) VALUES (1, 2);";
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
    fn rename_into_chain_fences() {
        // Two tables renamed onto `u`: a second generation produces the name.
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE x(y INT);\nALTER TABLE t RENAME TO u;\nALTER TABLE x RENAME TO u;\nINSERT INTO u(a, b) VALUES (1, 2);";
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
    fn rename_with_drop_fences() {
        // Drops are never followed across.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t RENAME TO u;\nDROP TABLE u;\nINSERT INTO u(a, b) VALUES (1, 2);";
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
    fn rename_same_unit_alter_fences() {
        // The rename shares its unit with an alter of the generation; the
        // intra-unit order is unknowable. (PostgreSQL rejects this mix at
        // runtime, but the parser accepts it, so the fence must hold.)
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t RENAME TO u, ADD COLUMN c INT;\nINSERT INTO u(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
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
    fn alter_multi_add_other_lifts() {
        // One unit, two `ADD COLUMN other` changes: neither touches `a`/`b`.
        let sql = "CREATE TABLE t(a INT, b INT);\nALTER TABLE t ADD COLUMN c INT, ADD COLUMN d INT;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert_eq!(target(&targets, "a").blocked, None);
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn drop_recreate_fences() {
        // Two `CREATE` generations for one name: the definition is not unique.
        let sql = "CREATE TABLE t(a INT, b INT);\nDROP TABLE t;\nCREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);";
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
    fn identical_spelling_case_variants_form() {
        // SQLite folds unquoted identifiers to lower-case: one table.
        let sql = "CREATE TABLE T(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT A FROM T ORDER BY A;";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "b").blocked, None);
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_READ));
    }

    #[test]
    fn identical_spelling_quoted_variants_form() {
        // SQLite folds quoted identifiers to lower-case too: one table.
        let sql = "CREATE TABLE \"T\"(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT a FROM \"t\" ORDER BY a;";
        let (_, targets) = parse_both(sql);
        assert_eq!(target(&targets, "b").blocked, None);
        assert_eq!(target(&targets, "a").blocked.as_deref(), Some(BLOCK_READ));
    }

    #[test]
    fn ambiguous_read_fences_only_read_column() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT b FROM main.t;";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_AMBIGUOUS)
        );
        assert_eq!(target(&targets, "a").blocked, None);
    }

    #[test]
    fn ambiguous_insert_list_fences_listed_only() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nINSERT INTO main.t(a) VALUES (3);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "a").blocked.as_deref(),
            Some(BLOCK_AMBIGUOUS)
        );
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn ambiguous_implicit_insert_fences_all() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nINSERT INTO main.t VALUES (3, 4);";
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
    fn ambiguous_star_fences_all() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nSELECT * FROM main.t;";
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
    fn ambiguous_ddl_keeps_whole_table() {
        // A sibling spelling in a DDL role keeps the whole-table fence.
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);\nALTER TABLE main.t ADD COLUMN c INT;";
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
    fn ambiguous_fk_parent_fences_named_columns() {
        let sql = "CREATE TABLE t(a INT, b INT);\nCREATE TABLE u(x INT, FOREIGN KEY (x) REFERENCES main.t(b));\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_both(sql);
        assert_eq!(
            target(&targets, "b").blocked.as_deref(),
            Some(BLOCK_AMBIGUOUS)
        );
        assert_eq!(target(&targets, "a").blocked, None);
    }

    #[test]
    fn like_defines_no_bundles() {
        // `LIKE` creates carry no column list under this parser, so the
        // `LIKE` fence is vacuous: the table yields no targets at all while
        // other tables are unaffected.
        let sql = "CREATE TABLE t LIKE u;\nCREATE TABLE s(a INT, b INT);\nINSERT INTO s(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
        assert!(
            targets
                .iter()
                .all(|target| target.table.0 != vec![ir::Ident("t".to_string())])
        );
        assert_eq!(target(&targets, "b").blocked, None);
    }

    #[test]
    fn ctas_sources_still_fence() {
        let sql = "CREATE TABLE u(x INT, y INT);\nCREATE TABLE t(a, b) AS SELECT x, y FROM u;\nINSERT INTO t(a, b) VALUES (1, 2);";
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
    fn on_commit_still_fences() {
        let sql =
            "CREATE TEMP TABLE t(a INT, b INT) ON COMMIT DROP;\nINSERT INTO t(a, b) VALUES (1, 2);";
        let (_, targets) = parse_dialect(sql, DialectKind::PostgreSql);
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

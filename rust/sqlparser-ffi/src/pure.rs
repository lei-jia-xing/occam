//! Generic-only pure-builtin keyword rule.
//!
//! This lifts the blanket "ANY function call ⇒ global opaque barrier" rule
//! for two provably-pure reserved-keyword special forms on `GenericSQL`
//! only: `coalesce` and `nullif`. "Pure" here means ONLY: no hidden writes,
//! no sequence use, no volatile/session effects. It does NOT mean "no
//! reads": reads flow through function arguments and are extracted by the
//! existing expression analysis independently of fencing, so lifting the
//! fence never drops a read edge.
//!
//! Why Generic-only: `GenericSQL` means an unknown engine, so a lift must
//! hold on ANY engine. Only `coalesce` and `nullif` qualify: they are
//! SQL-standard reserved-keyword special forms, so they cannot name user
//! routines on any conforming engine (reserved keywords are not definable
//! as function names), and their semantics — first-non-NULL selection,
//! equality-conditional NULL — admit no writes, sequences, or volatility by
//! definition. Every other name stays fenced on Generic: ordinary names
//! (`abs`, `sum`, ...) might not exist as builtins on some engine and could
//! resolve to user or extension routines with unknown effects (custom
//! aggregates especially are built from arbitrary functions). Only the
//! unquoted keyword spelling lifts: quotes make the name an identifier,
//! which on an unknown engine may name a user routine. Matching is
//! case-insensitive because keywords are case-insensitive on every engine.
//!
//! Fencing stays conservative everywhere else:
//! - Every dialect outside Generic keeps the blanket fence bit-identically:
//!   no engine-specific builtin is ever assumed (`abs` on SQLite may be a
//!   user routine tomorrow; the rule depends on no engine).
//! - Unknown functions fence (the keyword rule is closed; anything not
//!   listed is unmodeled and keeps today's barrier verbatim).
//! - Volatile and sequence-adjacent names (`random`, `nextval`/`currval`/
//!   `setval`/`lastval`, clock calls such as `now`/`current_timestamp`,
//!   `version`, aggregate state functions) are deliberately absent and fence.
//! - Window/grouping machinery (`OVER`, `FILTER`, `WITHIN GROUP`, null
//!   treatment, parametric calls) fences even under a keyword name.
//! - Qualification fences: only unqualified single-part names lift, so no
//!   schema resolution is ever assumed (`myschema.coalesce` may name a user
//!   routine; even `pg_catalog.abs` fences to keep the rule minimal).
//!
//! The rule is consulted in both fence positions: evaluated statements (via
//! [`crate::edits::SequenceVisitor`], covering `SELECT` projections, DML, and
//! other immediately-executed expressions) and stored/deferred expressions
//! (via `has_function`, covering column `DEFAULT`s, `CHECK` constraints, view
//! definitions, index expressions, and trigger `WHEN` clauses). A stored pure
//! expression's later execution is effect-free, so lifting it is consistent
//! with non-function stored expressions, which never fenced. Stored volatile
//! calls (e.g. `DEFAULT CURRENT_TIMESTAMP`) still fence through the rule.
//!
//! User shadowing: a script may `CREATE FUNCTION coalesce(...)` redefining a
//! keyword spelling. The rule is a coarse script-level disable: ANY routine
//! DDL ([`is_routine_ddl`]) or ANY parse error anywhere in the script
//! disables the rule for every unit, restoring today's blanket fence. This
//! is sound and minimal because (a) routine-DDL units independently emit
//! global `UnknownStatement` barriers through the statement catch-all, so
//! the script serializes regardless — per-name precision would be
//! observably equivalent at the dependency-graph level while complicating
//! the analysis; (b) a parse error means unparsed routine DDL cannot be
//! ruled out, and invalid units independently barrier in the Haskell layer.
//! Out-of-script user functions (pre-existing in the target database,
//! invisible to this self-contained script model which has no external
//! routine catalog) shadowing these names are assumed absent.
//!
//! One sqlparser AST-shape artifact lowers as `Expr::Function` and is NOT a
//! rule member: MySQL index length prefixes such as `KEY (b(1))` parse as
//! `Expr::Function`. It is excluded structurally in index-key position only
//! (via `ignored_function_roots`, following the `PARTITION BY RANGE` scheme
//! precedent): a bare `name(N)` with a single unsigned-numeric literal is the
//! length-prefix production — functional key parts require double parens and
//! lower as `Nested` — so the shape is positional, never a name-table lookup.
//! Adding `b` to a name table would be unsound and is never done.
//!
//! Cutback decision (decided 2026-11; no redesign planned):
//!
//! Per-dialect hand-maintained builtin catalogs (SQLite/PostgreSQL/MySQL
//! scalar and aggregate allowlists plus their per-engine doc links) were
//! removed as an unmaintainable trajectory: every entry needed per-engine
//! purity proof plus perpetual upkeep, and growth would multiply that cost
//! per dialect. The Generic reserved-keyword-only rule stands because it
//! depends on no engine.
//!
//! Upstream sqlparser-rs is syntax-only BY DESIGN and will not distinguish
//! functions by semantics (README "Syntax vs Semantics": "only a syntax
//! parser ... tries to avoid applying any SQL semantics"; semantic analysis
//! is left to downstream projects). Function semantics is therefore
//! permanently occam-owned; do not wait on or propose upstream changes.
//!
//! Growth policy: census-driven selection only, with additions batched per
//! dialect documentation rather than one-off names. Generic stays
//! reserved-keyword-only: a lift there must hold on ANY engine.
//!
//! Explicitly deferred — do not build without revisiting this decision:
//! a DBMS execution matrix for function verification (heavy asset), and
//! data-file/catalog-driven tables (code tables suffice at current scale).

use crate::DialectKind;
use crate::ir;
use sqlparser::ast::{Function, ObjectNamePart, Statement};

/// Pure builtins on unknown engines (`GenericSQL`). Only SQL-standard
/// reserved-keyword special forms qualify: they cannot name user routines on
/// any conforming engine. Ordinary names — scalars and aggregates alike —
/// stay fenced on Generic (see the module docs).
const GENERIC_PURE_BUILTINS: &[&str] = &[
    "coalesce", // Keyword first-non-NULL selection; unquoted spellings only.
    // SQL:2023/2016/92 reserved; PG bars it as a function/type name (COALESCE row):
    // https://www.postgresql.org/docs/current/sql-keywords-appendix.html
    "nullif", // Keyword equality-conditional NULL; unquoted spellings only.
             // SQL:2023/2016/92 reserved; PG bars it as a function/type name (NULLIF row):
             // https://www.postgresql.org/docs/current/sql-keywords-appendix.html
];
/// Whether `statement` defines, drops, or alters a callable routine. Any such
/// statement anywhere in the script disables the pure-builtin rule for every
/// unit (see the module docs for why the coarse rule is sound and minimal).
pub(super) fn is_routine_ddl(statement: &Statement) -> bool {
    matches!(
        statement,
        Statement::CreateFunction(_)
            | Statement::CreateProcedure { .. }
            | Statement::CreateMacro { .. }
            | Statement::DropFunction(_)
            | Statement::DropProcedure { .. }
            | Statement::AlterFunction(_)
    )
}

/// Whether `ident` names a member of `table`. Matching folds through
/// [`ir::IdentifierRules`] (never a raw string compare): unquoted names fold
/// per dialect, quoted names match exactly unless the dialect folds quoted
/// identifiers too (SQLite/MySQL/Generic).
fn table_matches(
    table: &[&str],
    ident: &sqlparser::ast::Ident,
    rules: &ir::IdentifierRules,
) -> bool {
    table.iter().any(|name| {
        match rules.fold(ir::IdentifierClass::Other, ident.quote_style.is_some()) {
            ir::CaseFolding::Lower | ir::CaseFolding::Upper => {
                ident.value.eq_ignore_ascii_case(name)
            }
            ir::CaseFolding::Preserve => ident.value == *name,
        }
    })
}

/// Whether `function` is a pure-builtin call that lifts the routine fence.
/// Only the Generic unquoted-keyword rule lifts: Generic dialect plus an
/// unqualified single-part unquoted keyword spelling in the plain call shape.
/// Unknown names, volatile/sequence-adjacent names, aggregates, qualified
/// names, quoted spellings, and window/grouping call shapes all return
/// `false` (fence); every non-Generic dialect returns `false`.
pub(super) fn is_pure_builtin(
    function: &Function,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> bool {
    // Window, grouping, filter, null-treatment, and parametric machinery is
    // never pure, even under a keyword name. ODBC escape syntax
    // ({fn coalesce(x, y)}) names the same function and may lift. Argument
    // shapes (DISTINCT, ORDER BY, bare subqueries) need no gating: arguments
    // are values whose reads expression analysis extracts independently;
    // ordering and duplicate treatment affect values, not effects.
    if function.over.is_some()
        || function.filter.is_some()
        || !function.within_group.is_empty()
        || function.null_treatment.is_some()
        || !matches!(function.parameters, sqlparser::ast::FunctionArguments::None)
    {
        return false;
    }
    // Unqualified single-part names only: any qualification would need schema
    // resolution, so it fences.
    let [ObjectNamePart::Identifier(ident)] = function.name.0.as_slice() else {
        return false;
    };
    // The Generic claim rests on keyword status, and keywords are unquoted
    // by definition: a quoted spelling is an identifier, which on an unknown
    // engine may name a user routine. Case folds through the shared matcher
    // (Generic compares case-insensitively, as every engine does for
    // keywords). Every other dialect keeps the blanket fence.
    match dialect {
        DialectKind::Generic => {
            ident.quote_style.is_none() && table_matches(GENERIC_PURE_BUILTINS, ident, rules)
        }
        _ => false,
    }
}

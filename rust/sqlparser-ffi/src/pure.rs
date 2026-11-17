//! Per-dialect pure-builtin function table.
//!
//! This lifts the blanket "ANY function call ⇒ global opaque barrier" rule for
//! a small allowlist of provably-pure builtins. "Pure" here means ONLY: no
//! hidden writes, no sequence use, no volatile/session effects. It does NOT
//! mean "no reads": reads flow through function arguments and are extracted by
//! the existing expression analysis independently of fencing, so lifting the
//! fence never drops a read edge.
//!
//! Fencing stays conservative everywhere else:
//! - Unknown functions fence (the allowlist is closed; anything not listed is
//!   unmodeled and keeps today's barrier verbatim).
//! - Volatile and sequence-adjacent names (`random`, `nextval`/`currval`/
//!   `setval`/`lastval`, clock calls such as `now`/`current_timestamp`,
//!   `version`, aggregate state functions) are deliberately absent and fence.
//! - Aggregates (`sum`/`count`/`avg`/`min`/`max`) are deliberately absent: the
//!   aggregate path (group reads, DISTINCT/window/filter state, session
//!   settings such as `div_precision_increment` or grouping validity modes)
//!   is not proven free of session interaction, so aggregates stay fenced.
//! - Window/filter/grouping machinery (`OVER`, `FILTER`, `WITHIN GROUP`,
//!   null treatment, parametric calls) fences even under a builtin name.
//! - Qualification fences: only unqualified single-part names lift, so no
//!   schema resolution is ever assumed (`myschema.abs` may name a user
//!   routine; even `pg_catalog.abs` fences to keep the rule minimal).
//! - The table is seeded only for SQLite, PostgreSQL, and MySQL. Every other
//!   dialect keeps the blanket fence bit-identically.
//!
//! The table is consulted in both fence positions: evaluated statements (via
//! [`crate::edits::SequenceVisitor`], covering `SELECT` projections, DML, and
//! other immediately-executed expressions) and stored/deferred expressions
//! (via `has_function`, covering column `DEFAULT`s, `CHECK` constraints, view
//! definitions, index expressions, and trigger `WHEN` clauses). A stored pure
//! expression's later execution is effect-free, so lifting it is consistent
//! with non-function stored expressions, which never fenced. Stored volatile
//! calls (e.g. `DEFAULT CURRENT_TIMESTAMP`) still fence through the allowlist.
//!
//! User shadowing: a script may `CREATE FUNCTION abs(...)` redefining a
//! builtin name. The rule is a coarse script-level disable: ANY routine DDL
//! ([`is_routine_ddl`]) or ANY parse error anywhere in the script disables the
//! table for every unit, restoring today's blanket fence. This is sound and
//! minimal because (a) routine-DDL units independently emit global
//! `UnknownStatement` barriers through the statement catch-all, so the script
//! serializes regardless — per-name precision would be observably equivalent
//! at the dependency-graph level while complicating the analysis; (b) a parse
//! error means unparsed routine DDL cannot be ruled out, and invalid units
//! independently barrier in the Haskell layer. Out-of-script user functions
//! (pre-existing in the target database, invisible to this self-contained
//! script model which has no external routine catalog) shadowing these names
//! are assumed absent.
//!
//! Two sqlparser AST-shape artifacts lower as `Expr::Function` and are NOT
//! table members, so they stay fenced: `PARTITION BY RANGE` parses with
//! `Function('range')`, and MySQL index prefixes such as `KEY (b(1))` parse
//! as `Expr::Function`. They are deliberately not excluded structurally:
//! that would need parent-context tracking in two visitors, and both occur in
//! DDL that fences on other grounds (partition routing, index modeling).
//! Adding `range` or `b` to the name table would be unsound and is never done.

use crate::DialectKind;
use crate::ir;
use sqlparser::ast::{Function, ObjectNamePart, Statement};

/// Allowlisted pure builtins (lowercase canonical spelling). Every entry is a
/// scalar value function common to SQLite, PostgreSQL, and MySQL with no
/// writes, no sequence use, and no volatile/session effects. Scalar names that
/// sqlparser lowers to dedicated expressions instead of `Expr::Function`
/// (`trim` → `Expr::Trim`, `substr`/`substring` → `Expr::Substring`,
/// `ceil`/`floor` → `Expr::Ceil`/`Expr::Floor`, `case` → `Expr::Case`) never
/// fenced and are deliberately absent: there is no fence to lift.
const PURE_BUILTINS: &[&str] = &[
    "abs",      // Absolute value; closed arithmetic over its argument.
    "coalesce", // First-non-NULL value selection over its arguments.
    "concat",   // Variadic string concatenation; deterministic over arguments.
    "length", // String length measure (bytes vs. characters differs by dialect; every spelling is pure).
    "lower",  // Case mapping; deterministic over the argument.
    "nullif", // Equality-conditional NULL; a failed comparison errors rather than writes.
    "round",  // Numeric rounding; deterministic arithmetic.
    "upper",  // Case mapping; deterministic over the argument.
];

/// Whether `statement` defines, drops, or alters a callable routine. Any such
/// statement anywhere in the script disables the pure-builtin table for every
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

/// Whether `function` is a pure-builtin call that lifts the routine fence.
/// Unknown names, volatile/sequence-adjacent names, aggregates, qualified
/// names, and window/filter/grouping call shapes all return `false` (fence).
/// Name matching folds through [`ir::IdentifierRules`] (never a raw string
/// compare): unquoted names fold per dialect, quoted names match exactly
/// unless the dialect folds quoted identifiers too (SQLite/MySQL).
pub(super) fn is_pure_builtin(
    function: &Function,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> bool {
    if !matches!(
        dialect,
        DialectKind::SQLite | DialectKind::PostgreSql | DialectKind::MySql
    ) {
        return false;
    }
    // Window, filter, grouping, null-treatment, and parametric machinery is
    // aggregate/session-adjacent: never pure, even under a builtin name.
    // ODBC escape syntax ({fn abs(x)}) names the same function and may lift.
    // Argument shapes (DISTINCT, ORDER BY, bare subqueries) need no gating:
    // arguments are values whose reads expression analysis extracts
    // independently; ordering and duplicate treatment affect values, not
    // effects.
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
    PURE_BUILTINS.iter().any(|name| {
        match rules.fold(ir::IdentifierClass::Other, ident.quote_style.is_some()) {
            ir::CaseFolding::Lower | ir::CaseFolding::Upper => {
                ident.value.eq_ignore_ascii_case(name)
            }
            ir::CaseFolding::Preserve => ident.value == *name,
        }
    })
}

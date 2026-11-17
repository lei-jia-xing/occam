//! Generic-only pure-builtin keyword rule: `coalesce`/`nullif` lift the
//! routine fence on `GenericSQL` only, for unquoted unqualified plain calls.
//!
//! Soundness: both are SQL-standard reserved keywords, so they cannot name
//! user routines on any conforming engine; pure means no writes, sequences,
//! or volatility; argument reads are extracted independently, so lifting
//! drops no read edges.
//!
//! Consulted in both fence positions: [`crate::edits::SequenceVisitor`] for
//! evaluated statements and `has_function` for stored expressions (`DEFAULT`,
//! `CHECK`, views, index expressions, trigger `WHEN`).
//!
//! Shadowing: any routine DDL ([`is_routine_ddl`]) or any parse error anywhere
//! disables the rule script-wide, restoring the blanket fence.
//!
//! MySQL `KEY (b(1))` parses as `Expr::Function`; excluded structurally in index-key position only.
//!
//! Generic stays reserved-keyword-only: no per-engine builtin catalogs.

use crate::DialectKind;
use crate::ir;
use sqlparser::ast::{Function, ObjectNamePart, Statement};

/// Pure builtins on `GenericSQL`: reserved-keyword forms only; unquoted spellings lift, quoted fence.
const GENERIC_PURE_BUILTINS: &[&str] = &[
    "coalesce", // Keyword first-non-NULL selection; unquoted spellings only.
    // SQL:2023/2016/92 reserved; PG bars it as a function/type name (COALESCE row):
    // https://www.postgresql.org/docs/current/sql-keywords-appendix.html
    "nullif", // Keyword equality-conditional NULL; unquoted spellings only.
             // SQL:2023/2016/92 reserved; PG bars it as a function/type name (NULLIF row):
             // https://www.postgresql.org/docs/current/sql-keywords-appendix.html
];
/// Whether `statement` is routine DDL; any hit disables the rule script-wide.
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

/// Whether `ident` names a member of `table` via [`ir::IdentifierRules`] folding
/// (unquoted folds per dialect; quoted matches exactly unless the dialect folds quoted too).
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

/// Whether `function` lifts the fence: Generic dialect plus unquoted unqualified
/// plain-call keyword shape; all other names, shapes, and dialects return `false`.
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

//! Pure built-in function recognition for `GenericSQL`.
//!
//! Standard reserved keywords (`coalesce`, `nullif`) are recognized as pure built-ins
//! that lift routine execution fences under strict soundness conditions:
//! - **Soundness**: Both are SQL-standard reserved keywords and cannot name user-defined
//!   routines in conforming engines. Being pure functions, they produce no writes or sequence
//!   mutations, and argument expression reads are tracked independently.
//! - **Unquoted requirement**: Only unquoted spellings lift fences. A quoted name (e.g. `"coalesce"`)
//!   is syntactically an identifier that could reference a custom user routine on unknown engines.
//! - **Script-wide shadowing**: Any routine DDL ([`is_routine_ddl`]) or parse error in the script
//!   disables this optimization script-wide, conservatively restoring blanket fences.

use crate::DialectKind;
use crate::ir;
use sqlparser::ast::{Function, ObjectNamePart, Statement};

/// Pure builtins recognized on `GenericSQL`: reserved-keyword forms only.
const GENERIC_PURE_BUILTINS: &[&str] = &["coalesce", "nullif"];

/// Returns true if `statement` defines, drops, or alters a routine.
///
/// Any routine DDL anywhere in a script conservatively disables pure built-in optimizations
/// across all units to prevent unsoundness under potential routine shadowing.
pub(super) fn is_routine_ddl(statement: &Statement) -> bool {
    matches!(
        statement,
        Statement::CreateFunction(_)
            | Statement::CreateAggregate(_)
            | Statement::CreateProcedure { .. }
            | Statement::CreateMacro { .. }
            | Statement::DropFunction(_)
            | Statement::DropProcedure { .. }
            | Statement::AlterFunction(_)
    )
}

/// Checks if `ident` matches any name in `table` under dialect identifier folding rules.
fn table_matches(
    table: &[&str],
    ident: &sqlparser::ast::Ident,
    rules: &ir::IdentifierRules,
) -> bool {
    table.iter().any(|name| {
        match rules.fold(ir::IdentifierClass::Other, ident.quote_style.is_some()) {
            ir::CaseFolding::Lower | ir::CaseFolding::AsciiLower | ir::CaseFolding::Upper => {
                ident.value.eq_ignore_ascii_case(name)
            }
            ir::CaseFolding::Preserve => ident.value == *name,
        }
    })
}

/// Checks if `function` is an unquoted, unqualified pure built-in on `GenericSQL`.
///
/// Returns `true` only when:
/// 1. The call contains no window (`OVER`), `FILTER`, `WITHIN GROUP`, null treatment, or parameters.
/// 2. The function name is an unqualified single identifier.
/// 3. The dialect is `GenericSQL`, the identifier is unquoted, and the name matches a known pure keyword.
pub(super) fn is_pure_builtin(
    function: &Function,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> bool {
    // Functions with window, filter, grouping, or parametric clauses are never pure.
    if function.over.is_some()
        || function.filter.is_some()
        || !function.within_group.is_empty()
        || function.null_treatment.is_some()
        || !matches!(function.parameters, sqlparser::ast::FunctionArguments::None)
    {
        return false;
    }
    // Only unqualified single-part names can be pure keywords; qualified calls require schema resolution.
    let [ObjectNamePart::Identifier(ident)] = function.name.0.as_slice() else {
        return false;
    };
    // Generic dialect lifts unquoted keyword builtins; quoted names and other dialects retain fences.
    match dialect {
        DialectKind::Generic => {
            ident.quote_style.is_none() && table_matches(GENERIC_PURE_BUILTINS, ident, rules)
        }
        _ => false,
    }
}

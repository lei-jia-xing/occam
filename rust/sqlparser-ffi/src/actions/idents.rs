use crate::ir;
use sqlparser::ast::*;

pub(crate) fn canonical_ident(
    ident: &Ident,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> ir::Ident {
    let fold = rules.fold(class, ident.quote_style.is_some());
    ir::Ident(fold.apply(&ident.value))
}

pub(crate) fn canonical_ident_string(
    ident: &Ident,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> String {
    canonical_ident(ident, rules, class).0
}

/// Returns true if `ident` is an unquoted `@`-prefixed variable (`@user` or `@@sys`).
pub(crate) fn is_variable_ident(ident: &Ident) -> bool {
    ident.quote_style.is_none() && ident.value.starts_with('@')
}

/// Returns true if `ident` is an unquoted user-defined variable (`@name`, not `@@sys` or bare `@`).
pub(crate) fn is_user_variable_ident(ident: &Ident) -> bool {
    ident.quote_style.is_none()
        && ident.value.starts_with('@')
        && !ident.value.starts_with("@@")
        && ident.value.len() > 1
}

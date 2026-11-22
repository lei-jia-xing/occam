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

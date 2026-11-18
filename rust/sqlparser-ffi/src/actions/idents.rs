use crate::ir;
use sqlparser::ast::*;

pub(crate) fn canonical_ident(
    ident: &Ident,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> ir::Ident {
    let fold = rules.fold(class, ident.quote_style.is_some());
    let value = match fold {
        ir::CaseFolding::Lower => ident.value.to_lowercase(),
        ir::CaseFolding::Upper => ident.value.to_uppercase(),
        ir::CaseFolding::Preserve => ident.value.clone(),
    };
    ir::Ident(value)
}

pub(crate) fn canonical_ident_string(
    ident: &Ident,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> String {
    canonical_ident(ident, rules, class).0
}

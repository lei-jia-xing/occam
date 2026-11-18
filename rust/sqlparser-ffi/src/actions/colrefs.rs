use super::*;
use crate::ir;
use sqlparser::ast::*;
use std::collections::HashSet;

pub(crate) struct ColumnCollector<'a> {
    rules: &'a ir::IdentifierRules,
    columns: Vec<ir::Ident>,
    seen: HashSet<ir::Ident>,
}

impl Visitor for ColumnCollector<'_> {
    type Break = ();

    fn pre_visit_expr(&mut self, expr: &Expr) -> std::ops::ControlFlow<Self::Break> {
        let column = match expr {
            Expr::Identifier(ident) => Some(canonical_ident(
                ident,
                self.rules,
                ir::IdentifierClass::Column,
            )),
            Expr::CompoundIdentifier(idents) => idents
                .last()
                .map(|ident| canonical_ident(ident, self.rules, ir::IdentifierClass::Column)),
            _ => None,
        };
        if let Some(column) = column
            && self.seen.insert(column.clone())
        {
            self.columns.push(column);
        }
        std::ops::ControlFlow::Continue(())
    }
}

pub(crate) fn expr_columns(expr: &Expr, rules: &ir::IdentifierRules) -> Vec<ir::Ident> {
    visit_columns(expr, rules)
}

/// Expression-position column references under any AST node: the single
/// extractor behind [`expr_columns`], also shared by `ALTER COLUMN` op
/// expressions and `CHANGE`/`MODIFY` new-definition options. Only
/// `Expr::Identifier`/`CompoundIdentifier` positions are collected, so
/// option names, charset/collation names, and tag/policy names (plain
/// `Ident` positions that never denote same-table columns) are ignored;
/// the few same-table references that live outside `Expr` (inline foreign
/// keys, policy `USING` columns) are handled by the caller.
pub(crate) fn visit_columns(
    node: &impl sqlparser::ast::Visit,
    rules: &ir::IdentifierRules,
) -> Vec<ir::Ident> {
    let mut collector = ColumnCollector {
        rules,
        columns: Vec::new(),
        seen: HashSet::new(),
    };
    let _ = node.visit(&mut collector);
    collector.columns
}

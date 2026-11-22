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
        match column {
            Some(column) if self.seen.insert(column.clone()) => {
                self.columns.push(column);
            }
            _ => {}
        }
        std::ops::ControlFlow::Continue(())
    }
}

/// Extracts column identifiers referenced within an expression.
pub(crate) fn expr_columns(expr: &Expr, rules: &ir::IdentifierRules) -> Vec<ir::Ident> {
    visit_columns(expr, rules)
}

/// Extracts column identifiers referenced in expression positions under any AST node.
///
/// Only `Expr::Identifier` and the terminal component of `Expr::CompoundIdentifier` are
/// collected; non-expression positions (such as table options, charset, and collation names)
/// are excluded. Same-table references outside `Expr` (e.g. inline foreign keys) are handled
/// by callers.
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

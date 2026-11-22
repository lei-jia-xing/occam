use super::*;
use crate::DialectKind;
use crate::actions::{SequenceEffect, sequence_expr_effect};
use crate::ir;
use sqlparser::ast::{Expr, Spanned, Visitor};
use sqlparser::tokenizer::Span;
use std::collections::HashSet;

pub(crate) struct SequenceVisitor<'a> {
    dialect: DialectKind,
    rules: &'a crate::ir::IdentifierRules,
    pure_table_enabled: bool,
    pub(crate) writes: Vec<ir::NameRef>,
    pub(crate) reads: Vec<ir::NameRef>,
    pub(crate) unknown: bool,
    seen: HashSet<ir::NameRef>,
    /// AST spans whose function calls never trigger routine fences.
    ///
    /// Specifically used for MySQL `VALUES(col)` proposed-row markers in `ON DUPLICATE KEY UPDATE`
    /// assignments (which parse as `Expr::Function` despite naming no routine). Only populated
    /// when `pure_table_enabled` is true (i.e. no routine DDL in the script shadows the name).
    pub(crate) ignored_function_roots: Vec<Span>,
}

impl<'a> SequenceVisitor<'a> {
    /// Creates a new sequence visitor tracking sequence mutations and unknown routine invocations.
    pub(crate) fn new(
        dialect: DialectKind,
        rules: &'a crate::ir::IdentifierRules,
        pure_table_enabled: bool,
    ) -> Self {
        Self {
            dialect,
            rules,
            pure_table_enabled,
            writes: Vec::new(),
            reads: Vec::new(),
            unknown: false,
            seen: HashSet::new(),
            ignored_function_roots: Vec::new(),
        }
    }
}

impl Visitor for SequenceVisitor<'_> {
    type Break = ();

    fn pre_visit_expr(&mut self, expr: &Expr) -> std::ops::ControlFlow<Self::Break> {
        match sequence_expr_effect(expr, self.dialect, self.rules) {
            Some(SequenceEffect::Write(sequence)) if self.seen.insert(sequence.clone()) => {
                self.writes.push(sequence);
            }
            Some(SequenceEffect::Read(sequence)) => self.reads.push(sequence),
            Some(SequenceEffect::Unknown) => self.unknown = true,
            None if matches!(expr, Expr::Function(_)) => {
                // Only Generic keyword calls and unconditional COUNT calls lift
                // the routine fence; unknown, volatile, sequence-adjacent, and
                // shadowed calls keep it. `VALUES(col)` markers in `ON DUPLICATE KEY UPDATE`
                // never fence (ignored root, populated only when unshadowed).
                let is_unconditional_count = crate::actions::is_unconditional_count_expr(
                    expr,
                    self.dialect,
                    self.rules,
                    self.pure_table_enabled,
                );
                let ignored = is_unconditional_count
                    || AnalysisVisitor::in_ignored_function_root(
                        &self.ignored_function_roots,
                        expr.span(),
                    );
                if !ignored {
                    let pure = self.pure_table_enabled
                        && matches!(expr, Expr::Function(function)
                            if crate::pure::is_pure_builtin(function, self.dialect, self.rules));
                    self.unknown |= !pure;
                }
            }
            _ => {}
        }
        std::ops::ControlFlow::Continue(())
    }
}

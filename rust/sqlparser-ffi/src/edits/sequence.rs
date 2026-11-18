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
    /// Subtrees whose function calls never fence: MySQL `VALUES(col)`
    /// proposed-row markers in `ON DUPLICATE KEY UPDATE` assignments (which
    /// lower as `Expr::Function` but name no routine). Populated by
    /// `sequence_effects_for_statement` only when the pure table is enabled
    /// (no routine-DDL shadowing); shadowed `VALUES` may name a user
    /// routine and keeps fencing.
    pub(crate) ignored_function_roots: Vec<Span>,
}

impl<'a> SequenceVisitor<'a> {
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
                // Only Generic keyword calls lift the routine fence; unknown,
                // volatile, sequence-adjacent, and shadowed calls keep it.
                // `VALUES(col)` markers in `ON DUPLICATE KEY UPDATE` never
                // fence (ignored root, populated only when unshadowed).
                let ignored = AnalysisVisitor::in_ignored_function_root(
                    &self.ignored_function_roots,
                    expr.span(),
                );
                let pure = !ignored
                    && self.pure_table_enabled
                    && matches!(expr, Expr::Function(function)
                        if crate::pure::is_pure_builtin(function, self.dialect, self.rules));
                // Ignored markers are not routines at all (neither pure nor
                // unknown); only non-ignored impure calls fence.
                if !ignored {
                    self.unknown |= !pure;
                }
            }
            _ => {}
        }
        std::ops::ControlFlow::Continue(())
    }
}

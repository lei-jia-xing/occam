use super::*;
use crate::DialectKind;
use crate::actions::{
    canonical_ident_string, is_dml_statement, name_parts, relation_from_parts, relation_ref,
};
use crate::ir;
use crate::source::{ByteSpan, SourceMap, TokenKind, TokenPosition, dialect};
use crate::{NativeEdit, NativeEditCategory};
use sqlparser::ast::{
    AlterTableOperation, Assignment, AssignmentTarget, CastKind, ColumnDef, DataType, Distinct,
    DuplicateTreatment, Expr, FunctionArg, FunctionArgExpr, GroupByExpr, GroupByWithModifier,
    InTarget, LimitClause, ObjectName, OffsetRows, OrderByExpr, Parens, Query, Select, SelectItem,
    SelectItemQualifiedWildcardKind, SetExpr, Spanned, Statement, TableConstraint, TableFactor,
    TableInheritance, TableWithJoins, ValueWithSpan, Visitor, WindowSpec, WindowType,
};
use sqlparser::keywords::Keyword;
use sqlparser::tokenizer::{Span, Token, Tokenizer};
use std::borrow::Cow;

pub(crate) struct AnalysisVisitor<'a> {
    pub(crate) dialect: DialectKind,
    pub(crate) rules: &'a crate::ir::IdentifierRules,
    pure_table_enabled: bool,
    /// Nested DML statements are analyzed separately by `merge_nested`, so
    /// their relations and columns must not leak into the enclosing statement.
    pub(crate) gate_nested_dml: bool,
    statement_depth: usize,
    nested_dml_depth: usize,
    source: &'a str,
    source_map: &'a SourceMap,
    scopes: Vec<CteScope>,
    pub(crate) relations: Vec<RelationUse>,
    pub(crate) unmodeled_relation_spans: Vec<Span>,
    pub(crate) unmodeled_table_source: bool,
    pub(crate) has_function: bool,
    /// Subtrees whose function calls never fence (e.g. partition schemes, index length prefixes).
    pub(crate) ignored_function_roots: Vec<Span>,
    pub(crate) ignored_relation_spans: Vec<Span>,
    pub(crate) ignored_user_var_spans: Vec<Span>,
    pub(crate) select_into_count: usize,
    /// Column reference and the alias scope in which it appeared.
    pub(crate) columns: Vec<(Vec<String>, usize)>,
    pub(crate) whole_rows: Vec<WholeRowUse>,
    pub(crate) aliases: Vec<AliasScope>,
    alias_scope_stack: Vec<usize>,
    /// The `Select`-level alias scope for each open `Query`. Set operations
    /// visit several sibling `Select` nodes, so each one gets its own scope.
    select_scopes: Vec<Option<usize>>,
    /// Alias and factor limits for the next `Query`, set when the query is the
    /// subquery of a derived table.
    pending_derived_limits: Option<(Span, usize, usize)>,
    /// The span, factor position, alias, and `ONLY` marker of the next table
    /// relation name.
    current_relation: Option<(Span, usize, Option<String>, bool)>,
    pub(crate) edits: Vec<NativeEdit>,
    tokens: &'a [TokenPosition],
    unit: ByteSpan,
    chunk_edits: bool,
    syntax_edits: bool,
    top_level_select_ptr: Option<*const Select>,
    recursive_predicate_spans: Vec<(usize, usize)>,
    pub(crate) sequences: Option<SequenceVisitor<'a>>,
    pub(crate) user_vars: Vec<crate::ir::Ident>,
}
impl<'a> AnalysisVisitor<'a> {
    pub(crate) fn new(context: AnalysisContext<'a>) -> Self {
        Self {
            dialect: context.dialect,
            rules: context.rules,
            pure_table_enabled: context.pure_table_enabled,
            gate_nested_dml: true,
            statement_depth: 0,
            nested_dml_depth: 0,
            source: context.source,
            source_map: context.source_map,
            scopes: Vec::new(),
            relations: Vec::new(),
            unmodeled_relation_spans: Vec::new(),
            unmodeled_table_source: false,
            has_function: false,
            ignored_function_roots: Vec::new(),
            ignored_relation_spans: Vec::new(),
            ignored_user_var_spans: Vec::new(),
            select_into_count: 0,
            columns: Vec::new(),
            whole_rows: Vec::new(),
            aliases: vec![AliasScope {
                parent: None,
                aliases: Vec::new(),
                parent_alias_limit: None,
                parent_factor_limit: None,
                factors: 0,
            }],
            alias_scope_stack: vec![0],
            select_scopes: Vec::new(),
            pending_derived_limits: None,
            current_relation: None,
            edits: Vec::new(),
            tokens: context.tokens,
            unit: context.unit,
            chunk_edits: context.chunk_edits,
            syntax_edits: context.syntax_edits,
            top_level_select_ptr: None,
            recursive_predicate_spans: Vec::new(),
            sequences: None,
            user_vars: Vec::new(),
        }
    }

    fn record_table_commands(&mut self, body: &SetExpr) {
        if self.nested_dml_depth > 0 {
            return;
        }
        match body {
            SetExpr::Table(table) => {
                if let Some(name) = &table.table_name {
                    let parts = table
                        .schema_name
                        .iter()
                        .chain(std::iter::once(name))
                        .map(|ident| {
                            ir::Ident(canonical_ident_string(
                                ident,
                                self.rules,
                                ir::IdentifierClass::Relation,
                            ))
                        })
                        .collect();
                    let span = table
                        .schema_name
                        .as_ref()
                        .map_or(name.span, |schema| schema.span.union(&name.span));
                    self.record_relation(Some(relation_from_parts(parts, self.dialect)), span);
                    self.whole_rows.push(WholeRowUse::Source(span));
                } else {
                    self.unmodeled_table_source = true;
                }
            }
            SetExpr::SetOperation { left, right, .. } => {
                self.record_table_commands(left);
                self.record_table_commands(right);
            }
            // Nested Query nodes get their own CTE and alias scope on visit.
            _ => {}
        }
    }

    pub(crate) fn in_ignored_function_root(roots: &[Span], span: Span) -> bool {
        roots
            .iter()
            .any(|root| root.start <= span.start && span.end <= root.end)
    }

    fn record_relation(&mut self, name: Option<ir::NameRef>, span: Span) {
        let is_cte = name.as_ref().is_some_and(|reference| {
            !reference.temp
                && self.is_cte_reference(
                    &reference.name.0,
                    self.mapped_span(span).map_or(0, |span| span.0),
                )
        });
        let (factor, alias, only) = match &self.current_relation {
            Some((relation_span, factor, alias, only)) if *relation_span == span => {
                let result = (*factor, alias.clone(), *only);
                self.current_relation = None;
                result
            }
            _ => (0, None, false),
        };
        self.relations.push(RelationUse {
            name,
            span,
            is_cte,
            scope: self.current_alias_scope(),
            alias,
            factor,
            only,
        });
    }

    fn current_alias_scope(&self) -> usize {
        self.alias_scope_stack.last().copied().unwrap_or(0)
    }

    fn record_alias(&mut self, alias: &sqlparser::ast::TableAlias, target: Option<Vec<String>>) {
        let name = canonical_ident_string(
            &alias.name,
            self.rules,
            crate::ir::IdentifierClass::Relation,
        );
        let scope = self.current_alias_scope();
        if let Some(scope) = self.aliases.get_mut(scope) {
            scope.aliases.push((name, target));
        }
    }

    fn record_alias_opt(
        &mut self,
        alias: Option<&sqlparser::ast::TableAlias>,
        target: Option<Vec<String>>,
    ) {
        if let Some(alias) = alias {
            self.record_alias(alias, target);
        }
    }

    fn record_table_function(
        &mut self,
        name_span: Span,
        kind: TableFunctionKind,
        alias: Option<&sqlparser::ast::TableAlias>,
    ) {
        match kind {
            TableFunctionKind::PureGenerator => {
                self.ignored_relation_spans.push(name_span);
                self.record_alias_opt(alias, None);
            }
            TableFunctionKind::FileTable(paths) => {
                self.ignored_relation_spans.push(name_span);
                for path in &paths {
                    let parts = vec![ir::Ident(path.clone())];
                    self.record_relation(Some(relation_from_parts(parts, self.dialect)), name_span);
                }
                self.record_alias_opt(alias, paths.first().map(|p| vec![p.clone()]));
            }
            TableFunctionKind::Unmodeled => {
                self.unmodeled_relation_spans.push(name_span);
                self.record_alias_opt(alias, None);
            }
        }
    }

    /// Reserve the next table-factor position in the current lexical scope.
    /// FROM items are visited in source order, so these positions also encode
    /// which siblings a LATERAL subquery may reference.
    fn next_factor(&mut self) -> usize {
        let scope = self.current_alias_scope();
        match self.aliases.get_mut(scope) {
            Some(entry) => {
                let factor = entry.factors;
                entry.factors += 1;
                factor
            }
            None => 0,
        }
    }

    fn is_cte_reference(&self, parts: &[ir::Ident], start: usize) -> bool {
        if parts.len() != 1 {
            return false;
        }
        let Some(name) = parts.first() else {
            return false;
        };
        self.scopes.iter().rev().any(|scope| {
            let visible_count = if scope.recursive {
                scope.names.len()
            } else {
                scope
                    .body_spans
                    .iter()
                    .position(|span| span.is_some_and(|(a, b)| a <= start && start < b))
                    .unwrap_or(scope.names.len())
            };
            scope
                .names
                .iter()
                .take(visible_count)
                .any(|candidate| candidate == &name.0)
        })
    }

    fn mapped_span(&self, span: Span) -> Option<(usize, usize)> {
        let range = self.source_map.span(span)?;
        let range = ByteSpan::checked(self.source, range.start, range.end)?;
        Some((range.start, range.end))
    }
    fn dotted_source_span(
        &self,
        mut parts: impl Iterator<Item = Option<Span>>,
    ) -> Option<(usize, usize)> {
        let (start, mut end) = self.mapped_span(parts.next()??)?;
        for part in parts {
            let (next_start, next_end) = self.mapped_span(part?)?;
            let index = self.tokens.partition_point(|token| token.end <= end);
            let (dot_index, dot) = self.next_significant(index, self.source.len())?;
            if self.source.get(dot.start..dot.end)? != "." {
                return None;
            }
            let (_, identifier) = self.next_significant(dot_index + 1, self.source.len())?;
            if identifier.start != next_start {
                return None;
            }
            end = next_end;
        }
        Some((start, end))
    }

    fn object_name_source_span(&self, name: &ObjectName) -> Option<(usize, usize)> {
        self.dotted_source_span(name.0.iter().map(|part| match part {
            sqlparser::ast::ObjectNamePart::Identifier(identifier) => Some(identifier.span),
            sqlparser::ast::ObjectNamePart::Function(_) => None,
        }))
    }

    fn matching_parentheses(
        &self,
        start: usize,
        end: usize,
        after: usize,
    ) -> Option<crate::source::ByteSpan> {
        let before = self.tokens.partition_point(|token| token.start <= start);
        for token in self.tokens[..before].iter().rev() {
            if token.start < after {
                break;
            }
            if matches!(token.kind, TokenKind::LeftParen)
                && let Some(pair) = token.parentheses.filter(|pair| pair.end >= end)
            {
                return Some(pair);
            }
        }
        if after < start {
            return None;
        }
        let first = self.tokens.partition_point(|token| token.start < after);
        self.tokens[first..]
            .iter()
            .take_while(|token| token.start <= end)
            .find_map(|token| {
                (matches!(token.kind, TokenKind::LeftParen)
                    && token.parentheses.is_some_and(|pair| pair.end >= end))
                .then_some(token.parentheses)
                .flatten()
            })
    }

    fn previous_significant(&self, byte_offset: usize) -> Option<TokenPosition> {
        let before = self
            .tokens
            .partition_point(|token| token.end <= byte_offset);
        self.tokens[..before]
            .iter()
            .rev()
            .copied()
            .find(|token| !matches!(token.kind, TokenKind::Whitespace))
    }

    fn following_word(&self, index: &mut usize, word: &str) -> Option<usize> {
        let (found, token) = self.next_significant(*index, self.source.len())?;
        (matches!(token.kind, TokenKind::Word(_))
            && self
                .source
                .get(token.start..token.end)?
                .eq_ignore_ascii_case(word))
        .then(|| {
            *index = found + 1;
            token.end
        })
    }

    fn postfix_span(&self, inner: &Expr, suffix: &[&str]) -> Option<(usize, usize)> {
        let (start, end) = self.expression_source_span(inner)?;
        let mut index = self.tokens.partition_point(|token| token.end <= end);
        let mut suffix_end = end;
        for word in suffix {
            suffix_end = self.following_word(&mut index, word)?;
        }
        Some((start, suffix_end))
    }

    fn words_between(&self, start: usize, end: usize, words: &[&str]) -> Option<()> {
        let mut index = self.tokens.partition_point(|token| token.end <= start);
        for word in words {
            let (found, token) = self.next_significant(index, end)?;
            if !self
                .source
                .get(token.start..token.end)?
                .eq_ignore_ascii_case(word)
            {
                return None;
            }
            index = found + 1;
        }
        self.next_significant(index, end).is_none().then_some(())
    }

    fn binary_operator_span(
        &self,
        left: &Expr,
        right: &Expr,
        words: &[&str],
    ) -> Option<(usize, usize)> {
        let (start, left_end) = self.expression_source_span(left)?;
        let (right_start, end) = self.expression_source_span(right)?;
        self.words_between(left_end, right_start, words)?;
        Some((start, end))
    }
    fn pattern_span(
        &self,
        expr: &Expr,
        pattern: &Expr,
        escape: Option<&Expr>,
        operator: &[&str],
        negated: bool,
        any: bool,
    ) -> Option<(usize, usize)> {
        let (start, expr_end) = self.expression_source_span(expr)?;
        let (pattern_start, pattern_end) = self.expression_source_span(pattern)?;
        let mut words = [""; 4];
        let mut len = 0;
        if negated {
            words[len] = "NOT";
            len += 1;
        }
        for word in operator {
            words[len] = word;
            len += 1;
        }
        if any {
            words[len] = "ANY";
            len += 1;
        }
        self.words_between(expr_end, pattern_start, &words[..len])?;
        if let Some(escape) = escape {
            let (escape_start, end) = self.expression_source_span(escape)?;
            self.words_between(pattern_end, escape_start, &["ESCAPE"])?;
            Some((start, end))
        } else {
            Some((start, pattern_end))
        }
    }

    fn expression_source_span(&self, expr: &Expr) -> Option<(usize, usize)> {
        match expr {
            Expr::BinaryOp { left, right, .. } => {
                let (start, _) = self.expression_source_span(left)?;
                let (_, end) = self.expression_source_span(right)?;
                Some((start, end))
            }
            Expr::UnaryOp { op, expr: inner } => {
                let (start, end) = self.expression_source_span(inner)?;
                let (spelling, postfix) = match *op {
                    sqlparser::ast::UnaryOperator::AtDashAt => ("@-@", false),
                    sqlparser::ast::UnaryOperator::BangNot => ("!", false),
                    sqlparser::ast::UnaryOperator::BitwiseNot => ("~", false),
                    sqlparser::ast::UnaryOperator::DoubleAt => ("@@", false),
                    sqlparser::ast::UnaryOperator::Hash => ("#", false),
                    sqlparser::ast::UnaryOperator::Plus => ("+", false),
                    sqlparser::ast::UnaryOperator::Minus => ("-", false),
                    sqlparser::ast::UnaryOperator::Not => ("NOT", false),
                    sqlparser::ast::UnaryOperator::PGAbs => ("@", false),
                    sqlparser::ast::UnaryOperator::PGCubeRoot => ("||/", false),
                    sqlparser::ast::UnaryOperator::PGPostfixFactorial => ("!", true),
                    sqlparser::ast::UnaryOperator::PGPrefixFactorial => ("!!", false),
                    sqlparser::ast::UnaryOperator::PGSquareRoot => ("|/", false),
                    sqlparser::ast::UnaryOperator::QuestionDash => ("?-", false),
                    sqlparser::ast::UnaryOperator::QuestionPipe => ("?|", false),
                };
                let operator = if postfix {
                    let index = self.tokens.partition_point(|token| token.end <= end);
                    self.next_significant(index, self.source.len())?.1
                } else {
                    self.previous_significant(start)?
                };
                self.source
                    .get(operator.start..operator.end)?
                    .eq_ignore_ascii_case(spelling)
                    .then_some(if postfix {
                        (start, operator.end)
                    } else {
                        (operator.start, end)
                    })
            }
            Expr::Nested(inner) => {
                let range = self.expression_source_span(inner)?;
                let pair = self.matching_parentheses(range.0, range.1, 0)?;
                Some((pair.start, pair.end))
            }
            Expr::Subquery(query) => {
                let range = self.mapped_span(query.span())?;
                let pair = self.matching_parentheses(range.0, range.1, 0)?;
                Some((pair.start, pair.end))
            }
            Expr::Exists { subquery, negated } => {
                let range = self.mapped_span(subquery.span())?;
                let pair = self.matching_parentheses(range.0, range.1, 0)?;
                let (pair_start, pair_end) = (pair.start, pair.end);
                let exists = self.previous_significant(pair_start)?;
                if !self
                    .source
                    .get(exists.start..exists.end)?
                    .eq_ignore_ascii_case("EXISTS")
                {
                    return None;
                }
                let start = if *negated {
                    let not = self.previous_significant(exists.start)?;
                    if !self
                        .source
                        .get(not.start..not.end)?
                        .eq_ignore_ascii_case("NOT")
                    {
                        return None;
                    }
                    not.start
                } else {
                    exists.start
                };
                Some((start, pair_end))
            }
            Expr::IsFalse(inner) => self.postfix_span(inner, &["IS", "FALSE"]),
            Expr::IsNotFalse(inner) => self.postfix_span(inner, &["IS", "NOT", "FALSE"]),
            Expr::IsTrue(inner) => self.postfix_span(inner, &["IS", "TRUE"]),
            Expr::IsNotTrue(inner) => self.postfix_span(inner, &["IS", "NOT", "TRUE"]),
            Expr::IsNull(inner) => self.postfix_span(inner, &["IS", "NULL"]),
            Expr::IsNotNull(inner) => self.postfix_span(inner, &["IS", "NOT", "NULL"]),
            Expr::IsUnknown(inner) => self.postfix_span(inner, &["IS", "UNKNOWN"]),
            Expr::IsNotUnknown(inner) => self.postfix_span(inner, &["IS", "NOT", "UNKNOWN"]),
            Expr::IsDistinctFrom(left, right) => {
                self.binary_operator_span(left, right, &["IS", "DISTINCT", "FROM"])
            }
            Expr::IsNotDistinctFrom(left, right) => {
                self.binary_operator_span(left, right, &["IS", "NOT", "DISTINCT", "FROM"])
            }
            Expr::Between {
                expr,
                low,
                high,
                negated,
            } => {
                let (start, expr_end) = self.expression_source_span(expr)?;
                let (low_start, low_end) = self.expression_source_span(low)?;
                let (high_start, end) = self.expression_source_span(high)?;
                if *negated {
                    self.words_between(expr_end, low_start, &["NOT", "BETWEEN"])?;
                } else {
                    self.words_between(expr_end, low_start, &["BETWEEN"])?;
                }
                self.words_between(low_end, high_start, &["AND"])?;
                Some((start, end))
            }
            Expr::Like {
                negated,
                any,
                expr,
                pattern,
                escape_char,
            } => self.pattern_span(
                expr,
                pattern,
                escape_char.as_deref(),
                &["LIKE"],
                *negated,
                *any,
            ),
            Expr::ILike {
                negated,
                any,
                expr,
                pattern,
                escape_char,
            } => self.pattern_span(
                expr,
                pattern,
                escape_char.as_deref(),
                &["ILIKE"],
                *negated,
                *any,
            ),
            Expr::SimilarTo {
                negated,
                expr,
                pattern,
                escape_char,
            } => {
                let operator = if *negated {
                    &["NOT", "SIMILAR", "TO"][..]
                } else {
                    &["SIMILAR", "TO"][..]
                };
                let (start, pattern_end) = self.binary_operator_span(expr, pattern, operator)?;
                if let Some(escape) = escape_char {
                    let (escape_start, end) = self.expression_source_span(escape)?;
                    self.words_between(pattern_end, escape_start, &["ESCAPE"])?;
                    Some((start, end))
                } else {
                    Some((start, pattern_end))
                }
            }
            Expr::RLike {
                negated,
                expr,
                pattern,
                regexp,
            } => {
                let keyword = if *regexp { "REGEXP" } else { "RLIKE" };
                if *negated {
                    self.binary_operator_span(expr, pattern, &["NOT", keyword])
                } else {
                    self.binary_operator_span(expr, pattern, &[keyword])
                }
            }
            Expr::In {
                expr,
                target: InTarget::Unnest(array_expr),
                negated,
                ..
            } => {
                let words = if *negated {
                    &["NOT", "IN"][..]
                } else {
                    &["IN"][..]
                };
                self.binary_operator_span(expr, array_expr, words)
            }
            Expr::AtTimeZone {
                timestamp,
                time_zone,
            } => self.binary_operator_span(timestamp, time_zone, &["AT", "TIME", "ZONE"]),
            Expr::Collate { expr, collation } => {
                let (start, expr_end) = self.expression_source_span(expr)?;
                let range = self.object_name_source_span(collation)?;
                self.words_between(expr_end, range.0, &["COLLATE"])?;
                Some((start, range.1))
            }
            Expr::In {
                expr,
                target: InTarget::Subquery(subquery),
                ..
            } => {
                let (start, _) = self.expression_source_span(expr)?;
                let range = self.mapped_span(subquery.span())?;
                let opening = self.previous_significant(range.0)?;
                if !matches!(opening.kind, TokenKind::LeftParen) {
                    return None;
                }
                let pair = self.matching_parentheses(range.0, range.1, opening.start)?;
                Some((start, pair.end))
            }
            Expr::In {
                expr,
                target: InTarget::List(list),
                ..
            } => {
                let (start, left_end) = self.expression_source_span(expr)?;
                let (_, last_end) = self.expression_source_span(list.last()?)?;
                let pair = self.matching_parentheses(left_end, last_end, left_end)?;
                Some((start, pair.end))
            }
            Expr::In {
                expr,
                target: InTarget::Table { name, .. },
                ..
            } => {
                let (start, _) = self.expression_source_span(expr)?;
                let range = self.mapped_span(name.span())?;
                Some((start, range.1))
            }
            Expr::Cast {
                kind,
                expr: inner,
                data_type,
                ..
            } => {
                let (start, inner_end) = self.expression_source_span(inner)?;
                let type_range = self.cast_type_span(inner_end, data_type, kind)?;
                match kind {
                    CastKind::DoubleColon => Some((start, type_range.1)),
                    CastKind::Cast | CastKind::TryCast | CastKind::SafeCast => {
                        let opening = self.previous_significant(start)?;
                        if !matches!(opening.kind, TokenKind::LeftParen)
                            || opening.parentheses?.end < type_range.1
                        {
                            return None;
                        }
                        let keyword = self.previous_significant(opening.start)?;
                        let expected = match kind {
                            CastKind::Cast => "CAST",
                            CastKind::TryCast => "TRY_CAST",
                            CastKind::SafeCast => "SAFE_CAST",
                            CastKind::DoubleColon => unreachable!(),
                        };
                        self.source
                            .get(keyword.start..keyword.end)?
                            .eq_ignore_ascii_case(expected)
                            .then_some((keyword.start, opening.parentheses?.end))
                    }
                }
            }
            Expr::Case {
                case_token,
                end_token,
                ..
            } => {
                let start = self.mapped_span(case_token.0.span)?.0;
                let end = self.mapped_span(end_token.0.span)?.1;
                Some((start, end))
            }
            Expr::Tuple(items) => {
                let first = self.expression_source_span(items.first()?)?;
                let last = self.expression_source_span(items.last()?)?;
                let pair = self.matching_parentheses(first.0, last.1, 0)?;
                Some((pair.start, pair.end))
            }
            Expr::Function(function) => {
                if function.uses_odbc_syntax
                    || function.null_treatment.is_some()
                    || !function.within_group.is_empty()
                    || function.filter.is_some()
                {
                    None
                } else {
                    let (start, name_end) = self.object_name_source_span(&function.name)?;
                    let hint = self.mapped_span(function.span())?;
                    let args_end =
                        if matches!(&function.args, sqlparser::ast::FunctionArguments::None)
                            && matches!(
                                &function.parameters,
                                sqlparser::ast::FunctionArguments::None
                            )
                        {
                            name_end
                        } else {
                            self.matching_parentheses(start, hint.1, start)
                                .map(|pair| pair.end)?
                        };
                    if let Some(over) = &function.over {
                        let (_, over_end) = self.window_type_source_span(over, args_end)?;
                        Some((start, over_end))
                    } else {
                        Some((start, args_end))
                    }
                }
            }
            Expr::Identifier(ident) => self.mapped_span(ident.span),
            Expr::CompoundIdentifier(identifiers) => {
                self.dotted_source_span(identifiers.iter().map(|identifier| Some(identifier.span)))
            }
            Expr::Value(_) | Expr::Wildcard(_) => self.mapped_span(expr.span()),
            _ => None,
        }
    }

    fn expression_source_slice(&self, expr: &Expr) -> Option<&'a str> {
        let (start, end) = self.expression_source_span(expr)?;
        self.source.get(start..end)
    }

    fn add_expression_edit(
        &mut self,
        expr: &Expr,
        replacement: &str,
        category: NativeEditCategory,
        label: &'static str,
    ) {
        if let Some((start, end)) = self.expression_source_span(expr) {
            self.add_edit_bytes(start, end, replacement, category, label);
        }
    }

    fn add_edit_bytes(
        &mut self,
        start: usize,
        end: usize,
        replacement: &str,
        category: NativeEditCategory,
        label: impl Into<Cow<'static, str>>,
    ) {
        self.add_edit_bytes_with_flags(start, end, replacement, category, label, false, false);
    }

    fn add_low_yield_edit_bytes(
        &mut self,
        start: usize,
        end: usize,
        replacement: &str,
        category: NativeEditCategory,
        label: impl Into<Cow<'static, str>>,
    ) {
        self.add_edit_bytes_with_flags(start, end, replacement, category, label, false, true);
    }

    #[allow(clippy::too_many_arguments)]
    fn add_edit_bytes_with_flags(
        &mut self,
        start: usize,
        end: usize,
        replacement: &str,
        category: NativeEditCategory,
        label: impl Into<Cow<'static, str>>,
        string_literal: bool,
        low_yield: bool,
    ) {
        // Statement-only parses skip all edit IR: no allocation, validation,
        // or downstream JSON/decode work. Semantic collection continues
        // untouched in the same traversal.
        if !self.syntax_edits {
            return;
        }
        let Some(range) = ByteSpan::checked(self.source, start, end) else {
            return;
        };
        let recursive_predicate = !self.recursive_predicate_spans.is_empty()
            && self
                .recursive_predicate_spans
                .iter()
                .any(|&(s, e)| s <= range.start && range.end <= e);
        self.edits.push(NativeEdit {
            start: range.start,
            end: range.end,
            replacement: replacement.to_owned(),
            category,
            label: label.into(),
            top_level_projection: false,
            string_literal,
            recursive_predicate,
            low_yield,
        });
    }

    fn source_slice(&self, span: Span) -> Option<&'a str> {
        let (start, end) = self.mapped_span(span)?;
        self.source.get(start..end)
    }

    fn keyword_before(
        &self,
        keyword: Keyword,
        scope_start: usize,
        body_start: usize,
    ) -> Option<TokenPosition> {
        self.tokens.iter().rev().copied().find(|token| {
            token.is_keyword(keyword) && token.start >= scope_start && token.start <= body_start
        })
    }

    fn query_source_scope(&self, query: &Query) -> Option<(usize, usize)> {
        let (start, end) = self.mapped_span(query.span())?;
        if start < self.unit.start || end > self.unit.end {
            return None;
        }
        let before = self.tokens.partition_point(|token| token.start < start);
        if let Some(pair) = self.tokens[..before].iter().rev().find_map(|token| {
            matches!(token.kind, TokenKind::LeftParen)
                .then_some(token.parentheses)
                .flatten()
                .filter(|pair| {
                    pair.start >= self.unit.start && pair.end <= self.unit.end && pair.end >= end
                })
        }) {
            return Some((pair.start + 1, pair.end - 1));
        }
        let mut index = before;
        while let Some(token) = self.tokens.get(index).copied() {
            match token.kind {
                TokenKind::Semicolon | TokenKind::Eof | TokenKind::RightParen => {
                    return Some((start, token.start));
                }
                TokenKind::LeftParen => {
                    let pair = token.parentheses?;
                    index = self
                        .tokens
                        .partition_point(|candidate| candidate.end <= pair.end);
                    continue;
                }
                _ => {}
            }
            index += 1;
        }
        Some((start, self.unit.end))
    }

    fn top_level_keyword(
        &self,
        keyword: Keyword,
        (scope_start, scope_end): (usize, usize),
    ) -> Option<TokenPosition> {
        let mut index = self
            .tokens
            .partition_point(|token| token.start < scope_start);
        let mut found = None;
        while let Some(token) = self.tokens.get(index).copied() {
            if token.start >= scope_end {
                break;
            }
            if matches!(token.kind, TokenKind::LeftParen) {
                let pair = token.parentheses?;
                if pair.end > scope_end {
                    return None;
                }
                index = self
                    .tokens
                    .partition_point(|candidate| candidate.end <= pair.end);
                continue;
            }
            if token.is_keyword(keyword) {
                found = Some(token);
            }
            index += 1;
        }
        found
    }

    /// Locates the byte boundary immediately following a clause body.
    ///
    /// Skips balanced parentheses and stops when encountering a boundary token
    /// (right paren, semicolon, EOF) or any keyword in `following_clauses`.
    fn clause_end_after(
        &self,
        body_end: usize,
        scope_end: usize,
        following_clauses: &[Keyword],
    ) -> Option<usize> {
        if body_end > scope_end {
            return None;
        }
        let mut index = self.tokens.partition_point(|token| token.start < body_end);
        while let Some(token) = self.tokens.get(index).copied() {
            if token.start >= scope_end {
                break;
            }
            match token.kind {
                TokenKind::LeftParen => {
                    let pair = token.parentheses?;
                    if pair.end > scope_end {
                        return None;
                    }
                    index = self
                        .tokens
                        .partition_point(|candidate| candidate.end <= pair.end);
                }
                TokenKind::RightParen | TokenKind::Semicolon | TokenKind::Eof => {
                    return Some(preceding_space(self.source.as_bytes(), token.start));
                }
                _ if following_clauses
                    .iter()
                    .any(|keyword| token.is_keyword(*keyword)) =>
                {
                    return Some(preceding_space(self.source.as_bytes(), token.start));
                }
                _ => {
                    index += 1;
                }
            }
        }
        Some(preceding_space(self.source.as_bytes(), scope_end))
    }

    fn clause_edit_complete(
        &mut self,
        keyword: Keyword,
        body: (usize, usize),
        scope: (usize, usize),
        following_clauses: &[Keyword],
        label: &'static str,
    ) {
        let Some(token) = self.keyword_before(keyword, scope.0, body.0) else {
            return;
        };
        let Some(end) = self.clause_end_after(body.1, scope.1, following_clauses) else {
            return;
        };
        self.add_edit_bytes(
            preceding_space(self.source.as_bytes(), token.start),
            end,
            "",
            NativeEditCategory::Clause,
            label,
        );
    }

    fn order_by_source_span(&self, order: &sqlparser::ast::OrderBy) -> Option<(usize, usize)> {
        let sqlparser::ast::OrderByKind::Expressions(expressions) = &order.kind else {
            return None;
        };
        self.order_by_exprs_source_span(expressions)
    }

    fn order_by_exprs_source_span(&self, exprs: &[OrderByExpr]) -> Option<(usize, usize)> {
        let (start, _) = self.expression_source_span(&exprs.first()?.expr)?;
        let (_, end) = self.expression_source_span(&exprs.last()?.expr)?;
        Some((start, end))
    }

    fn assignment_source_span(&self, assignment: &Assignment) -> Option<(usize, usize)> {
        let (start, _) = match &assignment.target {
            AssignmentTarget::ColumnName(name) => self.mapped_span(name.span())?,
            AssignmentTarget::Tuple(names) => {
                let first = names.first()?;
                let first_start = self.mapped_span(first.span())?.0;
                let start = match self.previous_significant(first_start) {
                    Some(token) if matches!(token.kind, TokenKind::LeftParen) => token.start,
                    _ => first_start,
                };
                (start, first_start)
            }
        };
        let (_, end) = self.expression_source_span(&assignment.value)?;
        (start < end).then_some((start, end))
    }

    fn fetch_source_span(
        &self,
        fetch: &sqlparser::ast::Fetch,
        scope: (usize, usize),
    ) -> Option<(usize, usize)> {
        let fetch_token = self.top_level_keyword(Keyword::FETCH, scope)?;
        fetch.quantity.as_ref().map_or_else(
            || Some((fetch_token.end, fetch_token.end)),
            |quantity| self.expression_source_span(quantity),
        )
    }

    fn group_by_source_span(&self, group_by: &GroupByExpr) -> Option<(usize, usize)> {
        let GroupByExpr::Expressions(expressions, _) = group_by else {
            return None;
        };
        let (start, _) = self.expression_source_span(expressions.first()?)?;
        let (_, end) = self.expression_source_span(expressions.last()?)?;
        Some((start, end))
    }

    fn clause_edit_expression(
        &mut self,
        keyword: Keyword,
        body: &Expr,
        scope: Span,
        label: &'static str,
    ) {
        if let Some(range) = self.expression_source_span(body) {
            self.clause_edit_range(keyword, range, scope, label);
        }
    }

    fn clause_edit_range(
        &mut self,
        keyword: Keyword,
        (body_start, body_end): (usize, usize),
        scope: Span,
        label: &'static str,
    ) {
        let Some((scope_start, _)) = self.mapped_span(scope) else {
            return;
        };
        let Some(token) = self.keyword_before(keyword, scope_start, body_start) else {
            return;
        };
        let start = preceding_space(self.source.as_bytes(), token.start);
        self.add_edit_bytes(start, body_end, "", NativeEditCategory::Clause, label);
    }

    fn select_scope(&self, select: &Select) -> Option<(usize, usize)> {
        let (start, _) = self.mapped_span(select.select_token.0.span)?;
        let mut end = self
            .tokens
            .iter()
            .rev()
            .find(|token| {
                !matches!(
                    token.kind,
                    TokenKind::Whitespace | TokenKind::Semicolon | TokenKind::Eof
                )
            })
            .map_or(self.unit.end, |token| token.end);
        let mut index = self.tokens.partition_point(|token| token.start < start);
        while let Some(token) = self.tokens.get(index).copied() {
            if token.start >= end {
                break;
            }
            if matches!(token.kind, TokenKind::LeftParen)
                && let Some(pair) = token.parentheses
            {
                index = self
                    .tokens
                    .partition_point(|candidate| candidate.end <= pair.end);
                continue;
            }
            match token.kind {
                TokenKind::RightParen | TokenKind::Semicolon => {
                    end = token.start;
                    break;
                }
                TokenKind::Word(Keyword::UNION | Keyword::EXCEPT | Keyword::INTERSECT) => {
                    end = token.start;
                    break;
                }
                _ => {}
            }
            index += 1;
        }
        Some((start, end))
    }

    fn group_by_all_bounds(&self, select: &Select) -> Option<(usize, usize, usize)> {
        let (scope_start, scope_end) = self.select_scope(select)?;
        let mut index = self
            .tokens
            .partition_point(|token| token.start < scope_start);
        while let Some(token) = self.tokens.get(index).copied() {
            if token.start >= scope_end {
                break;
            }
            if token.is_keyword(Keyword::GROUP)
                && let Some((by_index, by)) = self.next_significant(index + 1, scope_end)
                && by.is_keyword(Keyword::BY)
                && let Some((_, all)) = self.next_significant(by_index + 1, scope_end)
                && all.is_keyword(Keyword::ALL)
            {
                return Some((token.start, all.end, scope_end));
            }
            if matches!(token.kind, TokenKind::LeftParen)
                && let Some(pair) = token.parentheses
            {
                index = self
                    .tokens
                    .partition_point(|candidate| candidate.end <= pair.end);
                continue;
            }
            index += 1;
        }
        None
    }

    fn next_significant(
        &self,
        mut index: usize,
        scope_end: usize,
    ) -> Option<(usize, TokenPosition)> {
        while let Some(token) = self.tokens.get(index).copied() {
            if token.start >= scope_end {
                return None;
            }
            if !matches!(token.kind, TokenKind::Whitespace) {
                return Some((index, token));
            }
            index += 1;
        }
        None
    }

    fn keyword_pair_end(
        &self,
        first: Keyword,
        second: Keyword,
        after: usize,
        scope_end: usize,
    ) -> Option<usize> {
        let start = self.tokens.partition_point(|token| token.start < after);
        for (index, token) in self.tokens.iter().enumerate().skip(start) {
            if token.start >= scope_end {
                break;
            }
            if token.is_keyword(first)
                && let Some((_, next)) = self.next_significant(index + 1, scope_end)
                && next.is_keyword(second)
            {
                return Some(next.end);
            }
        }
        None
    }

    fn grouping_sets_end(&self, after: usize, scope_end: usize) -> Option<usize> {
        let start = self.tokens.partition_point(|token| token.start < after);
        for (index, token) in self.tokens.iter().enumerate().skip(start) {
            if token.start >= scope_end || !token.is_keyword(Keyword::GROUPING) {
                continue;
            }
            let Some((sets_index, sets)) = self.next_significant(index + 1, scope_end) else {
                continue;
            };
            if !sets.is_keyword(Keyword::SETS) {
                continue;
            }
            let Some((_, open)) = self.next_significant(sets_index + 1, scope_end) else {
                continue;
            };
            if !matches!(open.kind, TokenKind::LeftParen) {
                continue;
            }
            let pair = open.parentheses?;
            return (pair.end <= scope_end).then_some(pair.end);
        }
        None
    }

    fn group_by_all_edit(&mut self, select: &Select, modifiers: &[GroupByWithModifier]) {
        let Some((group_start, mut end, scope_end)) = self.group_by_all_bounds(select) else {
            return;
        };
        for modifier in modifiers {
            let next_end = match modifier {
                GroupByWithModifier::Rollup => {
                    self.keyword_pair_end(Keyword::WITH, Keyword::ROLLUP, end, scope_end)
                }
                GroupByWithModifier::Cube => {
                    self.keyword_pair_end(Keyword::WITH, Keyword::CUBE, end, scope_end)
                }
                GroupByWithModifier::Totals => {
                    self.keyword_pair_end(Keyword::WITH, Keyword::TOTALS, end, scope_end)
                }
                GroupByWithModifier::GroupingSets(_) => self.grouping_sets_end(end, scope_end),
            };
            let Some(next_end) = next_end else {
                return;
            };
            end = next_end;
        }
        self.add_edit_bytes(
            preceding_space(self.source.as_bytes(), group_start),
            end,
            "",
            NativeEditCategory::Clause,
            "remove-group_by",
        );
    }

    fn cte_source_span(&self, cte: &sqlparser::ast::Cte) -> Option<(usize, usize)> {
        let start = self.mapped_span(cte.alias.name.span)?.0;
        let end = cte.from.as_ref().map_or_else(
            || {
                self.mapped_span(cte.closing_paren_token.0.span)
                    .map(|(_, end)| end)
            },
            |from| self.mapped_span(from.span).map(|(_, end)| end),
        )?;
        Some((start, end))
    }
    fn cte_query_source_span(&self, cte: &sqlparser::ast::Cte) -> Option<(usize, usize)> {
        let range = self.mapped_span(cte.query.span())?;
        if self.tokens.is_empty() {
            return Some(range);
        }
        let pair = self.matching_parentheses(range.0, range.1, 0)?;
        Some((pair.start, pair.end))
    }
    fn table_alias_source_span(
        &self,
        alias: &sqlparser::ast::TableAlias,
    ) -> Option<(usize, usize)> {
        let (start, mut end) = self.mapped_span(alias.name.span)?;
        if !alias.columns.is_empty() {
            let first = self.mapped_span(alias.columns.first()?.name.span)?;
            let last = self.mapped_span(alias.columns.last()?.name.span)?;
            let opening = self.previous_significant(first.0)?;
            let pair = opening.parentheses?;
            if !matches!(opening.kind, TokenKind::LeftParen) || pair.end < last.1 {
                return None;
            }
            end = pair.end;
        }
        if let Some(at) = &alias.at {
            let (at_start, at_end) = self.mapped_span(at.span)?;
            let keyword = self.previous_significant(at_start)?;
            if at_start < end
                || !self
                    .source
                    .get(keyword.start..keyword.end)?
                    .eq_ignore_ascii_case("AT")
            {
                return None;
            }
            end = at_end;
        }
        Some((start, end))
    }
    fn wildcard_token_span(
        &self,
        options: &sqlparser::ast::WildcardAdditionalOptions,
    ) -> Option<(usize, usize)> {
        if options.opt_ilike.is_some()
            || options.opt_exclude.is_some()
            || options.opt_except.is_some()
            || options.opt_replace.is_some()
            || options.opt_rename.is_some()
            || options.opt_alias.is_some()
        {
            return None;
        }
        self.mapped_span(options.wildcard_token.0.span)
    }

    fn select_item_source_span(&self, item: &SelectItem) -> Option<(usize, usize)> {
        match item {
            SelectItem::UnnamedExpr(expr) => self.expression_source_span(expr),
            SelectItem::ExprWithAlias { expr, alias } => {
                let (start, _) = self.expression_source_span(expr)?;
                Some((start, self.mapped_span(alias.span)?.1))
            }
            SelectItem::ExprWithAliases { expr, aliases } => {
                let (start, _) = self.expression_source_span(expr)?;
                let first = self.mapped_span(aliases.first()?.span)?;
                let last = self.mapped_span(aliases.last()?.span)?;
                let opening = self.previous_significant(first.0)?;
                let pair = opening.parentheses?;
                (matches!(opening.kind, TokenKind::LeftParen) && pair.end >= last.1)
                    .then_some((start, pair.end))
            }
            SelectItem::QualifiedWildcard(kind, options) => {
                let (start, prefix_end) = match kind {
                    SelectItemQualifiedWildcardKind::ObjectName(name) => {
                        self.object_name_source_span(name)?
                    }
                    SelectItemQualifiedWildcardKind::Expr(expr) => {
                        self.expression_source_span(expr)?
                    }
                };
                let (star_start, star_end) = self.wildcard_token_span(options)?;
                let index = self.tokens.partition_point(|token| token.end <= prefix_end);
                let (dot_index, dot) = self.next_significant(index, star_start)?;
                let (_, wildcard) = self.next_significant(dot_index + 1, star_start + 1)?;
                (self.source.get(dot.start..dot.end)? == "."
                    && wildcard.start == star_start
                    && self.source.get(wildcard.start..wildcard.end)? == "*")
                    .then_some((start, star_end))
            }
            SelectItem::Wildcard(options) => self.wildcard_token_span(options),
        }
    }

    fn function_arg_expr_source_span(&self, arg: &FunctionArgExpr) -> Option<(usize, usize)> {
        match arg {
            FunctionArgExpr::Expr(expr) => self.expression_source_span(expr),
            FunctionArgExpr::QualifiedWildcard(name) => {
                let (start, end) = self.object_name_source_span(name)?;
                let index = self.tokens.partition_point(|token| token.end <= end);
                let (dot_index, dot) = self.next_significant(index, self.source.len())?;
                if self.source.get(dot.start..dot.end)? != "." {
                    return None;
                }
                let (_, wildcard) = self.next_significant(dot_index + 1, self.source.len())?;
                (self.source.get(wildcard.start..wildcard.end)? == "*")
                    .then_some((start, wildcard.end))
            }
            FunctionArgExpr::Wildcard => None,
            FunctionArgExpr::WildcardWithOptions(_) => None,
        }
    }

    fn function_arg_source_span(&self, arg: &FunctionArg) -> Option<(usize, usize)> {
        match arg {
            FunctionArg::Unnamed(arg) => self.function_arg_expr_source_span(arg),
            FunctionArg::Named { name, arg, .. } => {
                let start = self.mapped_span(name.span)?.0;
                Some((start, self.function_arg_expr_source_span(arg)?.1))
            }
            FunctionArg::ExprNamed { name, arg, .. } => {
                let start = self.expression_source_span(name)?.0;
                Some((start, self.function_arg_expr_source_span(arg)?.1))
            }
        }
    }

    fn table_factor_source_span(&self, relation: &TableFactor) -> Option<(usize, usize)> {
        match relation {
            TableFactor::Table {
                name,
                alias,
                args,
                with_hints,
                version,
                with_ordinality,
                partitions,
                json_path,
                sample,
                index_directives,
                inheritance,
            } => {
                if args.is_some()
                    || !with_hints.is_empty()
                    || version.is_some()
                    || *with_ordinality
                    || !partitions.is_empty()
                    || json_path.is_some()
                    || sample.is_some()
                    || !index_directives.is_empty()
                    || inheritance.is_some()
                {
                    return None;
                }
                let (start, name_end) = self.object_name_source_span(name)?;
                let end = alias.as_ref().map_or(Some(name_end), |alias| {
                    self.table_alias_source_span(alias).map(|(_, end)| end)
                })?;
                Some((start, end))
            }
            TableFactor::Derived {
                lateral,
                subquery,
                alias,
                sample,
            } => {
                if sample.is_some() {
                    return None;
                }
                let range = self.mapped_span(subquery.span())?;
                let pair = self.matching_parentheses(range.0, range.1, 0)?;
                let start = if *lateral {
                    let token = self.previous_significant(pair.start)?;
                    self.source
                        .get(token.start..token.end)?
                        .eq_ignore_ascii_case("LATERAL")
                        .then_some(token.start)?
                } else {
                    pair.start
                };
                let end = alias.as_ref().map_or(Some(pair.end), |alias| {
                    self.table_alias_source_span(alias).map(|(_, end)| end)
                })?;
                Some((start, end))
            }
            TableFactor::NestedJoin {
                table_with_joins,
                alias,
            } => {
                let inner = self.table_with_joins_source_span(table_with_joins)?;
                let pair = self.matching_parentheses(inner.0, inner.1, 0)?;
                let end = alias.as_ref().map_or(Some(pair.end), |alias| {
                    self.table_alias_source_span(alias).map(|(_, end)| end)
                })?;
                Some((pair.start, end))
            }
            TableFactor::TableFunction { expr, alias } => {
                let (expr_start, expr_end) = self.expression_source_span(expr)?;
                let opening = self.previous_significant(expr_start)?;
                if !matches!(opening.kind, TokenKind::LeftParen)
                    || opening.parentheses?.end < expr_end
                {
                    return None;
                }
                let keyword = self.previous_significant(opening.start)?;
                if !self
                    .source
                    .get(keyword.start..keyword.end)?
                    .eq_ignore_ascii_case("TABLE")
                {
                    return None;
                }
                let end = alias
                    .as_ref()
                    .map_or(Some(opening.parentheses?.end), |alias| {
                        self.table_alias_source_span(alias).map(|(_, end)| end)
                    })?;
                Some((keyword.start, end))
            }
            _ => None,
        }
    }

    fn table_with_joins_source_span(&self, table: &TableWithJoins) -> Option<(usize, usize)> {
        let (start, mut end) = self.table_factor_source_span(&table.relation)?;
        for join in &table.joins {
            end = self.join_end(join)?;
        }
        Some((start, end))
    }

    fn list_edits<T>(
        &mut self,
        label: &'static str,
        items: &[T],
        source_span: impl Fn(&Self, &T) -> Option<(usize, usize)>,
    ) {
        if items.len() < 2 {
            return;
        }
        let spans: Option<Vec<_>> = items.iter().map(|item| source_span(self, item)).collect();
        if let Some(spans) = spans {
            self.list_edit_ranges(label, &spans);
        }
    }

    fn list_edit_ranges(&mut self, label: &'static str, spans: &[(usize, usize)]) {
        if spans.len() < 2 {
            return;
        }
        self.list_chunk_ranges(label, spans);
        for index in 0..spans.len() {
            let range = if index == 0 {
                (spans[index].0, spans[index + 1].0)
            } else {
                (spans[index - 1].1, spans[index].1)
            };
            if self.find_comma(range.0, range.1).is_some() {
                self.add_edit_bytes(range.0, range.1, "", NativeEditCategory::List, label);
            }
        }
    }

    /// Emits hierarchical chunk deletions for contiguous items in a comma-separated list,
    /// ensuring at least one survivor remains.
    fn list_chunk_ranges(&mut self, label: &'static str, spans: &[(usize, usize)]) {
        if !self.chunk_edits {
            return;
        }
        let total = spans.len();
        // A chunk needs at least two removed items plus a survivor.
        if total < 3 {
            return;
        }
        if !Self::spans_ordered(spans) {
            return;
        }
        let mut level = 1u32;
        while let Some(groups) = 1usize.checked_shl(level) {
            if groups > total {
                break;
            }
            let base = total / groups;
            let extra = total % groups;
            let mut start = 0;
            for group in 0..groups {
                let size = base + usize::from(group < extra);
                if size < 2 {
                    // Group sizes are nonincreasing within a level, so no
                    // later group can emit; skip the remaining size-1 groups.
                    break;
                }
                let end = start + size;
                let (gap, range) = if start == 0 {
                    ((spans[end - 1].1, spans[end].0), (spans[0].0, spans[end].0))
                } else {
                    (
                        (spans[start - 1].1, spans[start].0),
                        (spans[start - 1].1, spans[end - 1].1),
                    )
                };
                if gap.0 < gap.1 && self.find_comma(gap.0, gap.1).is_some() {
                    self.add_edit_bytes(
                        range.0,
                        range.1,
                        "",
                        NativeEditCategory::List,
                        Self::chunk_label(label),
                    );
                }
                start = end;
            }
            level += 1;
        }
    }

    /// One O(n) check per list: every item span is non-empty and the spans
    /// are ordered without overlap. Only gates chunk emission; the
    /// single-item loop runs unchanged regardless.
    fn spans_ordered(spans: &[(usize, usize)]) -> bool {
        spans
            .windows(2)
            .all(|pair| pair[0].0 < pair[0].1 && pair[0].1 <= pair[1].0 && pair[1].0 < pair[1].1)
    }

    /// Static chunk label per list kind; no per-chunk allocation.
    fn chunk_label(label: &'static str) -> &'static str {
        match label {
            "remove-cte" => "remove-cte-chunk",
            "remove-values_row" => "remove-values_row-chunk",
            "remove-select_expression" => "remove-select_expression-chunk",
            "remove-from-relation" => "remove-from-relation-chunk",
            "remove-in_list" => "remove-in_list-chunk",
            "remove-tuple_item" => "remove-tuple_item-chunk",
            "remove-function_argument" => "remove-function_argument-chunk",
            _ => "remove-list-chunk",
        }
    }

    fn find_comma(&self, start: usize, end: usize) -> Option<(usize, usize)> {
        let mut index = self.tokens.partition_point(|token| token.start < start);
        while let Some(token) = self.tokens.get(index).copied() {
            if token.start >= end {
                break;
            }
            if matches!(token.kind, TokenKind::LeftParen) {
                let pair = token.parentheses?;
                if pair.end > end {
                    return None;
                }
                index = self
                    .tokens
                    .partition_point(|candidate| candidate.end <= pair.end);
                continue;
            }
            if matches!(token.kind, TokenKind::Comma) && token.end <= end {
                return Some((token.start, token.end));
            }
            index += 1;
        }
        None
    }

    fn values_row_span(
        &self,
        row: &sqlparser::ast::Parens<Vec<Expr>>,
        explicit_row: bool,
    ) -> Option<(usize, usize)> {
        let (start, _) = self.mapped_span(row.opening_token.0.span)?;
        let (_, end) = self.mapped_span(row.closing_token.0.span)?;
        let opening_index = self.tokens.partition_point(|token| token.start < start);
        let opening = self.tokens.get(opening_index)?;
        let closing_index = self.tokens.partition_point(|token| token.end < end);
        let closing = self.tokens.get(closing_index)?;
        if !matches!(opening.kind, TokenKind::LeftParen)
            || opening.parentheses?.end != end
            || !matches!(closing.kind, TokenKind::RightParen)
            || closing.end != end
        {
            return None;
        }
        let start = if explicit_row {
            let row = self.previous_significant(start)?;
            self.source
                .get(row.start..row.end)?
                .eq_ignore_ascii_case("ROW")
                .then_some(row.start)?
        } else {
            start
        };
        Some((start, end))
    }

    fn set_expr_source_span(&self, expr: &SetExpr) -> Option<(usize, usize)> {
        let (start, end) = match expr {
            SetExpr::Select(select) => {
                let (start, _) = self.mapped_span(select.select_token.0.span)?;
                let (_, end) = self.mapped_span(select.span())?;
                (start, end)
            }
            SetExpr::Query(query) => {
                if let Some(span) = self.mapped_span(query.span()) {
                    span
                } else {
                    self.set_expr_source_span(&query.body)?
                }
            }
            SetExpr::SetOperation { left, right, .. } => {
                let (start, _) = self.set_expr_source_span(left)?;
                let (_, end) = self.set_expr_source_span(right)?;
                (start, end)
            }
            SetExpr::Values(values) => self.mapped_span(values.span())?,
            _ => self.mapped_span(expr.span())?,
        };
        if start < self.unit.start || end > self.unit.end {
            return None;
        }
        let before = self.tokens.partition_point(|token| token.start < start);
        if let Some(pair) = self.tokens[..before].iter().rev().find_map(|token| {
            matches!(token.kind, TokenKind::LeftParen)
                .then_some(token.parentheses)
                .flatten()
                .filter(|pair| pair.end >= end)
        }) {
            return Some((pair.start, pair.end));
        }
        Some((start, end))
    }

    fn set_operation_edits(&mut self, expr: &SetExpr) {
        let SetExpr::SetOperation { left, right, .. } = expr else {
            return;
        };
        if let (Some(left_span), Some(right_span)) = (
            self.set_expr_source_span(left),
            self.set_expr_source_span(right),
        ) {
            if left_span.1 < right_span.1 {
                self.add_edit_bytes(
                    left_span.1,
                    right_span.1,
                    "",
                    NativeEditCategory::Clause,
                    "simplify-set_operation_left",
                );
            }
            if left_span.0 < right_span.0 {
                self.add_edit_bytes(
                    left_span.0,
                    right_span.0,
                    "",
                    NativeEditCategory::Clause,
                    "simplify-set_operation_right",
                );
            }
        }
        self.set_operation_edits(left);
        self.set_operation_edits(right);
    }

    fn window_type_source_span(&self, over: &WindowType, after: usize) -> Option<(usize, usize)> {
        let after_idx = self.tokens.partition_point(|token| token.start < after);
        let (over_idx, over_tok) = self.next_significant(after_idx, self.unit.end)?;
        if !over_tok.is_keyword(Keyword::OVER) {
            return None;
        }
        match over {
            WindowType::WindowSpec(_) => {
                let (_, paren_tok) = self.next_significant(over_idx + 1, self.unit.end)?;
                if !matches!(paren_tok.kind, TokenKind::LeftParen) {
                    return None;
                }
                let pair = paren_tok.parentheses?;
                Some((over_tok.start, pair.end))
            }
            WindowType::NamedWindow(ident) => {
                let (_, end) = self.mapped_span(ident.span)?;
                Some((over_tok.start, end))
            }
        }
    }

    fn find_keyword_between(
        &self,
        kw: Keyword,
        start: usize,
        end: usize,
    ) -> Option<(usize, TokenPosition)> {
        let mut idx = self.tokens.partition_point(|token| token.start < start);
        while let Some((next_idx, tok)) = self.next_significant(idx, end) {
            if tok.is_keyword(kw) {
                return Some((next_idx, tok));
            }
            idx = next_idx + 1;
        }
        None
    }

    fn window_spec_edits(&mut self, spec: &WindowSpec, after: usize) {
        let after_idx = self.tokens.partition_point(|token| token.start < after);
        let Some((over_idx, over_tok)) = self.next_significant(after_idx, self.unit.end) else {
            return;
        };
        if !over_tok.is_keyword(Keyword::OVER) {
            return;
        }
        let Some((_, paren_tok)) = self.next_significant(over_idx + 1, self.unit.end) else {
            return;
        };
        if !matches!(paren_tok.kind, TokenKind::LeftParen) {
            return;
        }
        let Some(pair) = paren_tok.parentheses else {
            return;
        };
        let inside_start = pair.start + 1;
        let inside_end = pair.end - 1;

        if !spec.partition_by.is_empty()
            || !spec.order_by.is_empty()
            || spec.window_frame.is_some()
            || spec.window_name.is_some()
        {
            self.add_edit_bytes(
                inside_start,
                inside_end,
                "",
                NativeEditCategory::Clause,
                "simplify-window_spec_empty",
            );
        }

        let scope = (inside_start, inside_end);
        let mut clauses = Vec::new();
        let frame_keyword = spec.window_frame.as_ref().map(|frame| match frame.units {
            sqlparser::ast::WindowFrameUnits::Rows => Keyword::ROWS,
            sqlparser::ast::WindowFrameUnits::Range => Keyword::RANGE,
            sqlparser::ast::WindowFrameUnits::Groups => Keyword::GROUPS,
        });
        for (keyword, label) in [
            (
                (!spec.partition_by.is_empty()).then_some(Keyword::PARTITION),
                "remove-window_partition_by",
            ),
            (
                (!spec.order_by.is_empty()).then_some(Keyword::ORDER),
                "remove-window_order_by",
            ),
            (frame_keyword, "remove-window_frame"),
        ] {
            if let Some(keyword) = keyword {
                let Some(token) = self.top_level_keyword(keyword, scope) else {
                    return;
                };
                clauses.push((token.start, label));
            }
        }
        if clauses.windows(2).all(|pair| pair[0].0 < pair[1].0) {
            for (index, &(start, label)) in clauses.iter().enumerate() {
                let (start, end) = match clauses.get(index + 1) {
                    Some(next) => (start, next.0),
                    None => (
                        preceding_space(self.source.as_bytes(), start).max(inside_start),
                        inside_end,
                    ),
                };
                self.add_edit_bytes(start, end, "", NativeEditCategory::Clause, label);
            }
        }

        if spec.partition_by.len() > 1 {
            self.list_edits(
                "remove-window_partition_by_expression",
                &spec.partition_by,
                |visitor, expr| visitor.expression_source_span(expr),
            );
        }

        if spec.order_by.len() > 1 {
            self.list_edits(
                "remove-window_order_by_expression",
                &spec.order_by,
                |visitor, item| {
                    (item.options == Default::default() && item.with_fill.is_none())
                        .then(|| visitor.expression_source_span(&item.expr))
                        .flatten()
                },
            );
        }
    }

    /// Proposes removing `DISTINCT`, `ALL`, or `DISTINCT ON (...)` clauses from `SELECT`.
    fn distinct_edits(&mut self, select: &Select) {
        let Some(distinct) = &select.distinct else {
            return;
        };
        let Some((_, select_end)) = self.mapped_span(select.select_token.0.span) else {
            return;
        };
        let limit = select
            .projection
            .first()
            .and_then(|item| self.select_item_source_span(item))
            .map(|(s, _)| s)
            .unwrap_or(self.unit.end);
        match distinct {
            Distinct::Distinct => {
                if let Some((distinct_idx, distinct_tok)) =
                    self.find_keyword_between(Keyword::DISTINCT, select_end, limit)
                    && let Some((_, next_tok)) =
                        self.next_significant(distinct_idx + 1, self.unit.end)
                {
                    self.add_edit_bytes(
                        distinct_tok.start,
                        next_tok.start,
                        "",
                        NativeEditCategory::Clause,
                        "remove-distinct",
                    );
                }
            }
            Distinct::All => {
                if let Some((all_idx, all_tok)) =
                    self.find_keyword_between(Keyword::ALL, select_end, limit)
                    && let Some((_, next_tok)) = self.next_significant(all_idx + 1, self.unit.end)
                {
                    self.add_edit_bytes(
                        all_tok.start,
                        next_tok.start,
                        "",
                        NativeEditCategory::Clause,
                        "remove-distinct",
                    );
                }
            }
            Distinct::On(exprs) => {
                if let Some((distinct_idx, distinct_tok)) =
                    self.find_keyword_between(Keyword::DISTINCT, select_end, limit)
                    && let Some((on_idx, on_tok)) = self.next_significant(distinct_idx + 1, limit)
                    && on_tok.is_keyword(Keyword::ON)
                    && let Some((_, paren_tok)) = self.next_significant(on_idx + 1, limit)
                    && matches!(paren_tok.kind, TokenKind::LeftParen)
                    && let Some(pair) = paren_tok.parentheses
                {
                    let after_idx = self.tokens.partition_point(|t| t.start < pair.end);
                    if let Some((_, next_tok)) = self.next_significant(after_idx, self.unit.end) {
                        self.add_edit_bytes(
                            distinct_tok.start,
                            next_tok.start,
                            "",
                            NativeEditCategory::Clause,
                            "remove-distinct",
                        );
                    }
                }
                if exprs.len() > 1 {
                    self.list_edits("remove-distinct_on_expression", exprs, |visitor, expr| {
                        visitor.expression_source_span(expr)
                    });
                }
            }
        }
    }

    fn data_type_pattern(&self, data_type: &DataType) -> Option<Vec<TokenKind>> {
        let sql = data_type.to_string();
        let type_dialect = dialect(self.dialect);
        let mut tokenizer = Tokenizer::new(type_dialect, &sql).with_unescape(false);
        let mut tokens = Vec::new();
        tokenizer
            .tokenize_with_location_into_buf(&mut tokens)
            .ok()?;
        let pattern: Vec<_> = tokens
            .iter()
            .filter_map(|token| {
                if matches!(token.token, Token::EOF | Token::Whitespace(_)) {
                    None
                } else {
                    TokenPosition::new(token, None)
                        .ok()
                        .map(|position| position.kind)
                }
            })
            .collect();
        (!pattern.is_empty()).then_some(pattern)
    }

    fn token_span_after(&self, pattern: &[TokenKind], after: usize) -> Option<(usize, usize)> {
        let (first_index, first) = self.tokens.iter().enumerate().find(|(_, token)| {
            token.start >= after && !matches!(token.kind, TokenKind::Whitespace)
        })?;
        let mut pattern_index = 0;
        for candidate in &self.tokens[first_index..] {
            if matches!(candidate.kind, TokenKind::Whitespace) {
                continue;
            }
            if candidate.kind != pattern[pattern_index] {
                return None;
            }
            pattern_index += 1;
            if pattern_index == pattern.len() {
                return Some((first.start, candidate.end));
            }
        }
        None
    }

    fn column_type_span(&self, column: &sqlparser::ast::ColumnDef) -> Option<(usize, usize)> {
        let (_, name_end) = self.mapped_span(column.name.span)?;
        self.token_span_after(&self.data_type_pattern(&column.data_type)?, name_end)
    }

    fn column_options_span(&self, column: &sqlparser::ast::ColumnDef) -> Option<(usize, usize)> {
        let (_, type_end) = self.column_type_span(column)?;
        let mut index = self.tokens.partition_point(|token| token.start < type_end);
        let mut col_end = type_end;
        while let Some(token) = self.tokens.get(index).copied() {
            if matches!(
                token.kind,
                TokenKind::Comma | TokenKind::RightParen | TokenKind::Semicolon
            ) {
                break;
            }
            if matches!(token.kind, TokenKind::LeftParen)
                && let Some(pair) = token.parentheses
            {
                col_end = pair.end;
                index = self.tokens.partition_point(|t| t.end <= pair.end);
                continue;
            }
            col_end = token.end;
            index += 1;
        }
        if col_end > type_end {
            Some((type_end, col_end))
        } else {
            None
        }
    }

    /// Outermost parentheses pair that contains the given source range.
    fn enclosing_parentheses(&self, start: usize, end: usize) -> Option<(usize, usize)> {
        self.tokens
            .iter()
            .filter(|token| matches!(token.kind, TokenKind::LeftParen))
            .filter_map(|token| token.parentheses.map(|pair| (token.start, pair.end)))
            .filter(|(open, close)| *open < start && *close >= end)
            .min_by_key(|(open, _)| *open)
    }

    /// Top-level comma-separated item inside a table body that contains `target`.
    fn table_body_item(
        &self,
        body_start: usize,
        body_end: usize,
        target: usize,
    ) -> Option<(usize, usize)> {
        let mut item_start = body_start;
        let mut index = self.tokens.partition_point(|token| token.end <= body_start);
        while let Some(token) = self.tokens.get(index).copied() {
            if token.start >= body_end {
                break;
            }
            match token.kind {
                TokenKind::LeftParen => {
                    let pair = token.parentheses?;
                    if pair.end > body_end {
                        return None;
                    }
                    index = self
                        .tokens
                        .partition_point(|candidate| candidate.start < pair.end);
                }
                TokenKind::Comma if token.start < target => {
                    item_start = token.end;
                    index += 1;
                }
                TokenKind::Comma => return Some((item_start, token.start)),
                _ => index += 1,
            }
        }
        Some((item_start, body_end))
    }

    fn table_constraint_span(&self, tc: &TableConstraint) -> Option<(usize, usize)> {
        let (inner_start, inner_end) = self.mapped_span(tc.span())?;
        let (open, close) = self.enclosing_parentheses(inner_start, inner_end)?;
        let body_start = open + 1;
        let body_end = close.checked_sub(1)?;
        if body_start > body_end {
            return None;
        }
        let (item_start, item_end) = self.table_body_item(body_start, body_end, inner_start)?;
        if item_start > body_start {
            let comma_start = item_start.checked_sub(1)?;
            let edit_start = preceding_whitespace(self.source.as_bytes(), comma_start);
            let edit_end = if item_end < body_end {
                item_end
            } else {
                body_end
            };
            Some((edit_start, edit_end))
        } else if item_end < body_end {
            Some((item_start, item_end + 1))
        } else {
            None
        }
    }

    pub(crate) fn add_column_edit(&mut self, start: usize, end: usize, label: &'static str) {
        // Column spans are derived, never trusted: anything outside the unit
        // is dropped rather than risking a hard parse error downstream.
        if start < self.unit.start || end > self.unit.end {
            return;
        }
        self.add_edit_bytes(start, end, "", NativeEditCategory::Column, label);
    }

    /// Returns the deletion span for a `CREATE TABLE` column definition,
    /// including adjacent comma and whitespace while preserving at least one column.
    pub(crate) fn column_def_span(&self, column: &ColumnDef) -> Option<(usize, usize)> {
        let (inner_start, inner_end) = self.mapped_span(column.name.span)?;
        let (open, close) = self.enclosing_parentheses(inner_start, inner_end)?;
        let body_start = open + 1;
        let body_end = close.checked_sub(1)?;
        if body_start > body_end {
            return None;
        }
        let (item_start, item_end) = self.table_body_item(body_start, body_end, inner_start)?;
        // The column name must be the first significant token of its item;
        // otherwise the target resolved into a neighboring item.
        let index = self.tokens.partition_point(|token| token.end <= item_start);
        let (_, first) = self.next_significant(index, item_end)?;
        if first.start != inner_start {
            return None;
        }
        if item_start > body_start {
            let comma_start = item_start.checked_sub(1)?;
            let edit_start = preceding_whitespace(self.source.as_bytes(), comma_start);
            let edit_end = if item_end < body_end {
                item_end
            } else {
                body_end
            };
            Some((edit_start, edit_end))
        } else if item_end < body_end {
            Some((item_start, item_end + 1))
        } else {
            None
        }
    }

    /// Source spans of an `INSERT` explicit column list, in order.
    pub(crate) fn insert_list_item_spans(
        &self,
        columns: &[ObjectName],
    ) -> Option<Vec<(usize, usize)>> {
        let spans: Option<Vec<(usize, usize)>> = columns
            .iter()
            .map(|column| self.object_name_source_span(column))
            .collect();
        let spans = spans?;
        // All items must sit inside one enclosing pair (the column list).
        if spans.len() >= 2 {
            let (open, close) = self.enclosing_parentheses(spans[0].0, spans[spans.len() - 1].1)?;
            if spans
                .iter()
                .any(|(start, end)| *start <= open || *end >= close)
            {
                return None;
            }
        }
        Some(spans)
    }

    /// Source spans of one `VALUES` row's items, in order. Every item must
    /// resolve and sit inside the row's own parentheses.
    pub(crate) fn values_item_spans(
        &self,
        row: &Parens<Vec<Expr>>,
        explicit_row: bool,
    ) -> Option<Vec<(usize, usize)>> {
        let (row_start, row_end) = self.values_row_span(row, explicit_row)?;
        let mut spans = Vec::with_capacity(row.content.len());
        for item in &row.content {
            let span = self.expression_source_span(item)?;
            if span.0 < row_start || span.1 > row_end {
                return None;
            }
            spans.push(span);
        }
        Some(spans)
    }

    /// Computes comma-gated deletion spans for items in a comma-separated list,
    /// associating the separator with the appropriate item boundary.
    pub(crate) fn comma_delete_spans(
        &self,
        spans: &[(usize, usize)],
    ) -> Vec<Option<(usize, usize)>> {
        if spans.len() < 2 {
            return vec![None; spans.len()];
        }
        (0..spans.len())
            .map(|index| {
                let range = if index == 0 {
                    (spans[index].0, spans[index + 1].0)
                } else {
                    (spans[index - 1].1, spans[index].1)
                };
                if range.0 >= range.1 {
                    return None;
                }
                self.find_comma(range.0, range.1).map(|_| range)
            })
            .collect()
    }

    fn cast_type_span(
        &self,
        inner_end: usize,
        data_type: &DataType,
        kind: &CastKind,
    ) -> Option<(usize, usize)> {
        let pattern = self.data_type_pattern(data_type)?;
        if matches!(kind, CastKind::DoubleColon) {
            let index = self.tokens.partition_point(|token| token.end <= inner_end);
            let operator = self.next_significant(index, self.source.len())?.1;
            if self.source.get(operator.start..operator.end)? != "::" {
                return None;
            }
            return self.token_span_after(&pattern, operator.end);
        }
        for token in self
            .tokens
            .iter()
            .filter(|token| token.start >= inner_end && token.is_keyword(Keyword::AS))
        {
            if let Some(range) = self.token_span_after(&pattern, token.end) {
                return Some(range);
            }
        }
        None
    }
    fn offset_end(&self, offset: &sqlparser::ast::Offset) -> Option<usize> {
        let (_, end) = self.expression_source_span(&offset.value)?;
        if offset.rows == OffsetRows::None {
            return Some(end);
        }
        let next = self
            .tokens
            .iter()
            .find(|token| token.start >= end && !matches!(token.kind, TokenKind::Whitespace))?;
        Some(
            if next.is_keyword(Keyword::ROW) || next.is_keyword(Keyword::ROWS) {
                next.end
            } else {
                end
            },
        )
    }

    fn limit_edits(&mut self, query: &Query, clause: &LimitClause) {
        let Some((scope_start, scope_end)) = self.query_source_scope(query) else {
            return;
        };
        match clause {
            LimitClause::LimitOffset {
                limit,
                offset,
                limit_by,
            } => {
                let limit_range = limit
                    .as_ref()
                    .and_then(|expr| self.expression_source_span(expr));
                let offset_range = offset
                    .as_ref()
                    .and_then(|value| self.expression_source_span(&value.value));
                let Some(limit_by_ranges) = limit_by
                    .iter()
                    .map(|expr| self.expression_source_span(expr))
                    .collect::<Option<Vec<_>>>()
                else {
                    return;
                };
                let limit_by_end = limit_by_ranges.iter().map(|(_, end)| *end).max();
                let limit_anchor = limit_range
                    .map(|(start, _)| start)
                    .or_else(|| limit_by_ranges.first().map(|(start, _)| *start));
                let limit_token = limit_anchor
                    .and_then(|start| self.keyword_before(Keyword::LIMIT, scope_start, start));
                let offset_token = offset_range.and_then(|(start, _)| {
                    self.keyword_before(Keyword::OFFSET, scope_start, start)
                });
                if let Some(token) = limit_token {
                    let mut end = limit_range
                        .map_or(scope_end, |(_, end)| end)
                        .max(limit_by_end.unwrap_or(0));
                    if let Some(offset_token) = offset_token {
                        end = preceding_space(self.source.as_bytes(), offset_token.start);
                    }
                    self.add_edit_bytes(
                        preceding_space(self.source.as_bytes(), token.start),
                        end,
                        "",
                        NativeEditCategory::Clause,
                        "remove-limit",
                    );
                }
                if let (Some(token), Some(offset)) = (offset_token, offset.as_ref())
                    && let Some(end) = self.offset_end(offset)
                {
                    self.add_edit_bytes(
                        preceding_space(self.source.as_bytes(), token.start),
                        end,
                        "",
                        NativeEditCategory::Clause,
                        "remove-offset",
                    );
                }
            }
            LimitClause::OffsetCommaLimit { offset, limit } => {
                let Some((offset_start, offset_end)) = self.expression_source_span(offset) else {
                    return;
                };
                let Some((_, limit_end)) = self.expression_source_span(limit) else {
                    return;
                };
                let Some(token) = self.keyword_before(Keyword::LIMIT, scope_start, offset_start)
                else {
                    return;
                };
                let end = offset_end.max(limit_end);
                self.add_edit_bytes(
                    preceding_space(self.source.as_bytes(), token.start),
                    end,
                    "",
                    NativeEditCategory::Clause,
                    "remove-limit",
                );
            }
        }
    }
    fn join_constraint_end(
        &self,
        constraint: &sqlparser::ast::JoinConstraint,
        relation_end: usize,
    ) -> Option<usize> {
        match constraint {
            sqlparser::ast::JoinConstraint::On(expr) => {
                self.expression_source_span(expr).map(|(_, end)| end)
            }
            sqlparser::ast::JoinConstraint::Using(columns) => {
                let first = self.mapped_span(columns.first()?.span())?;
                let last = self.mapped_span(columns.last()?.span())?;
                self.matching_parentheses(first.0, last.1, relation_end)
                    .map(|pair| pair.end)
            }
            sqlparser::ast::JoinConstraint::UsingAlias(columns, alias) => {
                let first = self.mapped_span(columns.first()?.span())?;
                let last = self.mapped_span(alias.span)?;
                self.matching_parentheses(first.0, last.1, relation_end)
                    .map(|pair| pair.end)
            }
            sqlparser::ast::JoinConstraint::Natural | sqlparser::ast::JoinConstraint::None => {
                Some(relation_end)
            }
        }
    }

    fn join_end(&self, join: &sqlparser::ast::Join) -> Option<usize> {
        let (_, relation_end) = self.table_factor_source_span(&join.relation)?;
        let end = match &join.join_operator {
            sqlparser::ast::JoinOperator::Join(constraint)
            | sqlparser::ast::JoinOperator::Inner(constraint)
            | sqlparser::ast::JoinOperator::Left(constraint)
            | sqlparser::ast::JoinOperator::LeftOuter(constraint)
            | sqlparser::ast::JoinOperator::Right(constraint)
            | sqlparser::ast::JoinOperator::RightOuter(constraint)
            | sqlparser::ast::JoinOperator::FullOuter(constraint)
            | sqlparser::ast::JoinOperator::CrossJoin(constraint)
            | sqlparser::ast::JoinOperator::Semi(constraint)
            | sqlparser::ast::JoinOperator::LeftSemi(constraint)
            | sqlparser::ast::JoinOperator::RightSemi(constraint)
            | sqlparser::ast::JoinOperator::Anti(constraint)
            | sqlparser::ast::JoinOperator::LeftAnti(constraint)
            | sqlparser::ast::JoinOperator::RightAnti(constraint)
            | sqlparser::ast::JoinOperator::StraightJoin(constraint) => {
                self.join_constraint_end(constraint, relation_end)?
            }
            sqlparser::ast::JoinOperator::AsOf {
                match_condition,
                constraint,
            } => self
                .expression_source_span(match_condition)?
                .1
                .max(self.join_constraint_end(constraint, relation_end)?),
            _ => relation_end,
        };
        Some(end.max(relation_end))
    }

    fn add_type_edits(&mut self, range: (usize, usize)) {
        let (start, end) = range;
        if start >= end {
            return;
        }
        for replacement in ["INT", "TEXT", "REAL", "BOOLEAN"] {
            if replacement.len() < end - start {
                self.add_edit_bytes(
                    start,
                    end,
                    replacement,
                    NativeEditCategory::Type,
                    "simplify-data_type",
                );
            }
        }
        if let Some(token) = self.tokens.iter().find(|token| {
            token.start >= start && token.end <= end && matches!(token.kind, TokenKind::Word(_))
        }) && let Some(replacement) = self.source.get(token.start..token.end).map(str::to_owned)
            && replacement.len() < end - start
        {
            self.add_edit_bytes(
                start,
                end,
                &replacement,
                NativeEditCategory::Type,
                "simplify-data_type",
            );
        }
    }

    /// Emits type simplification and constraint removal candidate edits for a column definition.
    ///
    /// Generates candidate edits for:
    /// - Simplifying column type declarations (e.g. integer width or precision reductions)
    /// - Stripping the entire column options span (e.g. constraints, defaults)
    /// - Stripping individual column options/constraints with preceding whitespace
    fn add_column_def_edits(&mut self, column: &ColumnDef) {
        if let Some(range) = self.column_type_span(column) {
            self.add_type_edits(range);
        }
        if let Some((opt_start, opt_end)) = self.column_options_span(column) {
            self.add_edit_bytes(
                opt_start,
                opt_end,
                "",
                NativeEditCategory::Constraint,
                "remove-column_constraint",
            );
        }
        for option in &column.options {
            if let Some((start, end)) = self.mapped_span(option.span()) {
                let edit_start = preceding_whitespace(self.source.as_bytes(), start);
                self.add_edit_bytes(
                    edit_start,
                    end,
                    "",
                    NativeEditCategory::Constraint,
                    "remove-column_constraint",
                );
            }
        }
    }
}

impl Visitor for AnalysisVisitor<'_> {
    type Break = ();

    fn pre_visit_query(&mut self, query: &Query) -> std::ops::ControlFlow<Self::Break> {
        let parent = self.current_alias_scope();
        let (parent_alias_limit, parent_factor_limit) = match self.pending_derived_limits {
            Some((span, alias_limit, factor_limit)) if span == query.span() => {
                self.pending_derived_limits = None;
                (Some(alias_limit), Some(factor_limit))
            }
            _ => (None, None),
        };
        let scope = self.aliases.len();
        self.aliases.push(AliasScope {
            parent: Some(parent),
            aliases: Vec::new(),
            parent_alias_limit,
            parent_factor_limit,
            factors: 0,
        });
        self.alias_scope_stack.push(scope);
        self.select_scopes.push(None);
        if let Some(with) = &query.with {
            let names: Vec<String> = with
                .cte_tables
                .iter()
                .map(|cte| {
                    canonical_ident_string(
                        &cte.alias.name,
                        self.rules,
                        crate::ir::IdentifierClass::Relation,
                    )
                })
                .collect();
            let body_spans = with
                .cte_tables
                .iter()
                .map(|cte| self.cte_query_source_span(cte))
                .collect();
            if self.syntax_edits && with.recursive {
                self.collect_recursive_predicate_spans(&with.cte_tables, &names);
            }
            self.scopes.push(CteScope {
                recursive: with.recursive,
                names,
                body_spans,
            });
            if self.syntax_edits {
                self.list_edits("remove-cte", &with.cte_tables, |visitor, cte| {
                    visitor.cte_source_span(cte)
                });
                let body_start = with
                    .cte_tables
                    .last()
                    .and_then(|cte| self.cte_source_span(cte))
                    .and_then(|(_, end)| {
                        let index = self.tokens.partition_point(|token| token.end <= end);
                        self.next_significant(index, self.source.len())
                            .map(|(_, token)| token.start)
                    });
                if let (Some((with_start, _)), Some(body_start)) =
                    (self.mapped_span(with.with_token.0.span), body_start)
                {
                    self.add_edit_bytes(
                        with_start,
                        body_start,
                        "",
                        NativeEditCategory::List,
                        "remove-with",
                    );
                }
            }
        } else {
            self.scopes.push(CteScope {
                recursive: false,
                names: Vec::new(),
                body_spans: Vec::new(),
            });
        }
        self.record_table_commands(&query.body);
        if !self.syntax_edits {
            return std::ops::ControlFlow::Continue(());
        }
        let query_scope = self.query_source_scope(query);
        if let Some(order) = &query.order_by {
            if let (Some(body), Some(scope)) = (self.order_by_source_span(order), query_scope) {
                self.clause_edit_complete(
                    Keyword::ORDER,
                    body,
                    scope,
                    &[
                        Keyword::LIMIT,
                        Keyword::OFFSET,
                        Keyword::FETCH,
                        Keyword::FOR,
                        Keyword::SETTINGS,
                        Keyword::FORMAT,
                    ],
                    "remove-order_by",
                );
            }
            if let sqlparser::ast::OrderByKind::Expressions(exprs) = &order.kind
                && exprs.len() > 1
            {
                self.list_edits("remove-order_by_expression", exprs, |visitor, expr| {
                    visitor.mapped_span(expr.span())
                });
            }
        }
        if let Some(limit) = &query.limit_clause {
            self.limit_edits(query, limit);
        }
        if let Some(fetch) = &query.fetch
            && let Some(scope) = query_scope
            && let Some(body) = self.fetch_source_span(fetch, scope)
        {
            self.clause_edit_complete(
                Keyword::FETCH,
                body,
                scope,
                &[Keyword::FOR, Keyword::SETTINGS, Keyword::FORMAT],
                "remove-fetch",
            );
        }
        if let SetExpr::Values(values) = query.body.as_ref()
            && values.rows.len() > 1
        {
            let spans: Option<Vec<_>> = values
                .rows
                .iter()
                .map(|row| self.values_row_span(row, values.explicit_row))
                .collect();
            if let Some(spans) = spans {
                self.list_edit_ranges("remove-values_row", &spans);
            }
        }
        self.set_operation_edits(&query.body);
        std::ops::ControlFlow::Continue(())
    }

    fn post_visit_query(&mut self, _query: &Query) -> std::ops::ControlFlow<Self::Break> {
        self.scopes.pop();
        if let Some(Some(select_scope)) = self.select_scopes.pop()
            && self.alias_scope_stack.last() == Some(&select_scope)
        {
            self.alias_scope_stack.pop();
        }
        self.alias_scope_stack.pop();
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_select(&mut self, select: &Select) -> std::ops::ControlFlow<Self::Break> {
        // Each `SELECT` branch of a set operation is a separate lexical scope:
        // aliases from one branch must not resolve columns in a sibling.
        let previous = self.select_scopes.last_mut().and_then(|slot| slot.take());
        if let Some(previous) = previous
            && self.alias_scope_stack.last() == Some(&previous)
        {
            self.alias_scope_stack.pop();
        }
        let parent = self.current_alias_scope();
        if let Some(slot) = self.select_scopes.last_mut() {
            let scope = self.aliases.len();
            self.aliases.push(AliasScope {
                parent: Some(parent),
                aliases: Vec::new(),
                parent_alias_limit: None,
                parent_factor_limit: None,
                factors: 0,
            });
            self.alias_scope_stack.push(scope);
            *slot = Some(scope);
        }
        if self.nested_dml_depth == 0 && select.into.is_some() {
            self.select_into_count += 1;
        }
        if self.nested_dml_depth == 0 {
            for item in &select.projection {
                let qualifier = match item {
                    SelectItem::Wildcard(_) => Vec::new(),
                    SelectItem::QualifiedWildcard(
                        SelectItemQualifiedWildcardKind::ObjectName(name),
                        _,
                    ) => match name_parts(name, self.rules, ir::IdentifierClass::Relation) {
                        Some(parts) => parts.into_iter().map(|part| part.0).collect(),
                        None => {
                            self.unmodeled_table_source = true;
                            continue;
                        }
                    },
                    // Computed row expansion has no proven base-relation
                    // lineage; do not guess which correlated row it exposes.
                    SelectItem::QualifiedWildcard(SelectItemQualifiedWildcardKind::Expr(_), _) => {
                        self.unmodeled_table_source = true;
                        continue;
                    }
                    _ => continue,
                };
                self.whole_rows.push(WholeRowUse::Projection {
                    qualifier,
                    scope: self.current_alias_scope(),
                });
            }
        }
        if !self.syntax_edits {
            return std::ops::ControlFlow::Continue(());
        }
        let is_top_proj = self
            .top_level_select_ptr
            .is_some_and(|ptr| std::ptr::eq(ptr, select as *const Select));
        let edits_before = self.edits.len();
        self.list_edits(
            "remove-select_expression",
            &select.projection,
            |visitor, item| visitor.select_item_source_span(item),
        );
        if is_top_proj {
            for edit in &mut self.edits[edits_before..] {
                edit.top_level_projection = true;
            }
        }
        self.distinct_edits(select);
        self.list_edits("remove-from-relation", &select.from, |visitor, table| {
            visitor.table_with_joins_source_span(table)
        });
        for table in &select.from {
            let mut previous_end = self
                .table_factor_source_span(&table.relation)
                .map(|(_, end)| end);
            for join in &table.joins {
                if let (Some(start), Some(end)) = (previous_end, self.join_end(join))
                    && start < end
                {
                    self.add_edit_bytes(start, end, "", NativeEditCategory::List, "remove-join");
                    previous_end = Some(end);
                }
            }
        }
        if let Some(selection) = &select.selection {
            let edit_index = self.edits.len();
            self.clause_edit_expression(Keyword::WHERE, selection, select.span(), "remove-where");
            if !self.recursive_predicate_spans.is_empty()
                && let Some(sel_span) = self.expression_source_span(selection)
                && self.recursive_predicate_spans.contains(&sel_span)
                && let Some(edit) = self.edits.get_mut(edit_index)
            {
                edit.recursive_predicate = true;
            }
        }
        if let Some(having) = &select.having {
            self.clause_edit_expression(Keyword::HAVING, having, select.span(), "remove-having");
        }
        match &select.group_by {
            GroupByExpr::All(modifiers) => self.group_by_all_edit(select, modifiers),
            GroupByExpr::Expressions(exprs, ..) => {
                if exprs.len() > 1 {
                    self.list_edits("remove-group_by_expression", exprs, |visitor, expr| {
                        visitor.expression_source_span(expr)
                    });
                }
                if let (Some(body), Some(scope)) = (
                    self.group_by_source_span(&select.group_by),
                    self.select_scope(select),
                ) {
                    self.clause_edit_complete(
                        Keyword::GROUP,
                        body,
                        scope,
                        &[
                            Keyword::HAVING,
                            Keyword::WINDOW,
                            Keyword::QUALIFY,
                            Keyword::SORT,
                            Keyword::ORDER,
                            Keyword::LIMIT,
                            Keyword::OFFSET,
                            Keyword::FETCH,
                            Keyword::FOR,
                            Keyword::CONNECT,
                            Keyword::CLUSTER,
                            Keyword::DISTRIBUTE,
                            Keyword::SETTINGS,
                            Keyword::FORMAT,
                        ],
                        "remove-group_by",
                    );
                }
            }
        }
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_statement(&mut self, statement: &Statement) -> std::ops::ControlFlow<Self::Break> {
        let nested_dml = self.statement_depth > 0 && is_dml_statement(statement);
        self.statement_depth += 1;
        if nested_dml && self.gate_nested_dml {
            self.nested_dml_depth += 1;
        }
        if !self.syntax_edits {
            return std::ops::ControlFlow::Continue(());
        }
        if self.statement_depth == 1
            && let Statement::Query(query) = statement
            && let SetExpr::Select(select) = query.body.as_ref()
            && select.into.is_none()
            && select.projection.len() >= 2
            && !select.projection.iter().any(|item| {
                matches!(
                    item,
                    SelectItem::Wildcard(_) | SelectItem::QualifiedWildcard(_, _)
                )
            })
        {
            self.top_level_select_ptr = Some(select.as_ref() as *const Select);
        }
        match statement {
            Statement::CreateTable(create) => {
                for column in &create.columns {
                    self.add_column_def_edits(column);
                }
                for tc in &create.constraints {
                    if let Some((start, end)) = self.table_constraint_span(tc) {
                        self.add_edit_bytes(
                            start,
                            end,
                            "",
                            NativeEditCategory::Constraint,
                            "remove-table_constraint",
                        );
                    }
                }
            }
            Statement::AlterTable(alter) => {
                for operation in &alter.operations {
                    if let AlterTableOperation::AddColumn { column_def, .. } = operation {
                        self.add_column_def_edits(column_def);
                    }
                }
            }
            Statement::Update(update) => {
                self.list_edits(
                    "remove-update_assignment",
                    &update.assignments,
                    |visitor, item| visitor.assignment_source_span(item),
                );
                let mut previous_end = self
                    .table_factor_source_span(&update.table.relation)
                    .map(|(_, end)| end);
                for join in &update.table.joins {
                    if let (Some(start), Some(end)) = (previous_end, self.join_end(join))
                        && start < end
                    {
                        self.add_edit_bytes(
                            start,
                            end,
                            "",
                            NativeEditCategory::List,
                            "remove-join",
                        );
                        previous_end = Some(end);
                    }
                }
                if let Some(selection) = &update.selection {
                    self.clause_edit_expression(
                        Keyword::WHERE,
                        selection,
                        statement.span(),
                        "remove-where",
                    );
                }
                if !update.order_by.is_empty() {
                    if let (Some(body), Some(scope)) = (
                        self.order_by_exprs_source_span(&update.order_by),
                        self.mapped_span(statement.span()),
                    ) {
                        self.clause_edit_complete(
                            Keyword::ORDER,
                            body,
                            scope,
                            &[Keyword::LIMIT, Keyword::RETURNING],
                            "remove-order_by",
                        );
                    }
                    if update.order_by.len() > 1 {
                        self.list_edits(
                            "remove-order_by_expression",
                            &update.order_by,
                            |visitor, expr| visitor.mapped_span(expr.span()),
                        );
                    }
                }
                if let Some(limit) = &update.limit {
                    self.clause_edit_expression(
                        Keyword::LIMIT,
                        limit,
                        statement.span(),
                        "remove-limit",
                    );
                }
                if let Some(returning) = &update.returning {
                    self.list_edits("remove-returning_expression", returning, |visitor, item| {
                        visitor.select_item_source_span(item)
                    });
                    if let (Some((body_start, _)), Some((_, body_end)), Some(scope)) = (
                        returning
                            .first()
                            .and_then(|item| self.select_item_source_span(item)),
                        returning
                            .last()
                            .and_then(|item| self.select_item_source_span(item)),
                        self.mapped_span(statement.span()),
                    ) {
                        self.clause_edit_complete(
                            Keyword::RETURNING,
                            (body_start, body_end),
                            scope,
                            &[],
                            "remove-returning",
                        );
                    }
                }
            }
            Statement::Delete(delete) => {
                if let Some(selection) = &delete.selection {
                    self.clause_edit_expression(
                        Keyword::WHERE,
                        selection,
                        statement.span(),
                        "remove-where",
                    );
                }
                if !delete.order_by.is_empty() {
                    if let (Some(body), Some(scope)) = (
                        self.order_by_exprs_source_span(&delete.order_by),
                        self.mapped_span(statement.span()),
                    ) {
                        self.clause_edit_complete(
                            Keyword::ORDER,
                            body,
                            scope,
                            &[Keyword::LIMIT, Keyword::RETURNING],
                            "remove-order_by",
                        );
                    }
                    if delete.order_by.len() > 1 {
                        self.list_edits(
                            "remove-order_by_expression",
                            &delete.order_by,
                            |visitor, expr| visitor.mapped_span(expr.span()),
                        );
                    }
                }
                if let Some(limit) = &delete.limit {
                    self.clause_edit_expression(
                        Keyword::LIMIT,
                        limit,
                        statement.span(),
                        "remove-limit",
                    );
                }
                if let Some(returning) = &delete.returning {
                    self.list_edits("remove-returning_expression", returning, |visitor, item| {
                        visitor.select_item_source_span(item)
                    });
                    if let (Some((body_start, _)), Some((_, body_end)), Some(scope)) = (
                        returning
                            .first()
                            .and_then(|item| self.select_item_source_span(item)),
                        returning
                            .last()
                            .and_then(|item| self.select_item_source_span(item)),
                        self.mapped_span(statement.span()),
                    ) {
                        self.clause_edit_complete(
                            Keyword::RETURNING,
                            (body_start, body_end),
                            scope,
                            &[],
                            "remove-returning",
                        );
                    }
                }
            }
            _ => {}
        }
        std::ops::ControlFlow::Continue(())
    }

    fn post_visit_statement(
        &mut self,
        statement: &Statement,
    ) -> std::ops::ControlFlow<Self::Break> {
        if self.gate_nested_dml && self.statement_depth > 1 && is_dml_statement(statement) {
            self.nested_dml_depth = self.nested_dml_depth.saturating_sub(1);
        }
        self.statement_depth = self.statement_depth.saturating_sub(1);
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_table_factor(
        &mut self,
        factor: &TableFactor,
    ) -> std::ops::ControlFlow<Self::Break> {
        if self.nested_dml_depth > 0 {
            return std::ops::ControlFlow::Continue(());
        }
        let factor_index = self.next_factor();
        self.current_relation = None;
        match factor {
            TableFactor::Table {
                name,
                alias,
                args: Some(table_args),
                ..
            } => {
                let kind = classify_table_function(name, &table_args.args);
                self.record_table_function(name.span(), kind, alias.as_ref());
            }
            TableFactor::Table {
                name,
                alias,
                inheritance,
                ..
            } => {
                let alias_name = alias.as_ref().map(|alias| {
                    canonical_ident_string(
                        &alias.name,
                        self.rules,
                        crate::ir::IdentifierClass::Relation,
                    )
                });
                let only = matches!(inheritance, Some(TableInheritance::Only { .. }));
                self.current_relation = Some((name.span(), factor_index, alias_name, only));
                let target = alias.as_ref().and_then(|_| {
                    relation_ref(name, self.dialect, self.rules)
                        .map(|reference| reference.name.0.into_iter().map(|part| part.0).collect())
                });
                self.record_alias_opt(alias.as_ref(), target);
            }
            TableFactor::Function {
                name, args, alias, ..
            } => {
                let kind = classify_table_function(name, args);
                self.record_table_function(name.span(), kind, alias.as_ref());
            }
            TableFactor::TableFunction { expr, alias } => match expr {
                Expr::Function(func) => {
                    let args = match &func.args {
                        sqlparser::ast::FunctionArguments::List(list) => &list.args[..],
                        _ => &[],
                    };
                    let kind = classify_table_function(&func.name, args);
                    self.record_table_function(expr.span(), kind, alias.as_ref());
                }
                _ => {
                    self.unmodeled_table_source = true;
                    self.record_alias_opt(alias.as_ref(), None);
                }
            },
            TableFactor::Derived {
                lateral,
                subquery,
                alias,
                ..
            } => {
                let alias_limit = self
                    .aliases
                    .get(self.current_alias_scope())
                    .map_or(0, |scope| scope.aliases.len());
                let factor_limit = if *lateral { factor_index } else { 0 };
                self.pending_derived_limits = Some((subquery.span(), alias_limit, factor_limit));
                if let Some(alias) = alias {
                    self.record_alias(alias, None);
                }
            }
            TableFactor::UNNEST { alias, .. }
            | TableFactor::JsonTable { alias, .. }
            | TableFactor::OpenJsonTable { alias, .. }
            | TableFactor::NestedJoin { alias, .. }
            | TableFactor::Pivot { alias, .. }
            | TableFactor::Unpivot { alias, .. }
            | TableFactor::MatchRecognize { alias, .. }
            | TableFactor::XmlTable { alias, .. }
            | TableFactor::SemanticView { alias, .. } => {
                if let Some(alias) = alias {
                    self.record_alias(alias, None);
                }
            }
            _ => {}
        }
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_relation(&mut self, relation: &ObjectName) -> std::ops::ControlFlow<Self::Break> {
        if self.nested_dml_depth > 0 || self.ignored_relation_spans.contains(&relation.span()) {
            return std::ops::ControlFlow::Continue(());
        }
        self.record_relation(
            relation_ref(relation, self.dialect, self.rules),
            relation.span(),
        );
        std::ops::ControlFlow::Continue(())
    }

    fn post_visit_table_factor(
        &mut self,
        _factor: &TableFactor,
    ) -> std::ops::ControlFlow<Self::Break> {
        if self.nested_dml_depth == 0 {
            self.current_relation = None;
            self.pending_derived_limits = None;
        }
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_expr(&mut self, expr: &Expr) -> std::ops::ControlFlow<Self::Break> {
        if let Some(sequences) = &mut self.sequences {
            let _ = sequences.pre_visit_expr(expr);
        }
        // Expressions inside an ignored root (PostgreSQL `PARTITION BY`
        // schemes, MySQL index length prefixes) never fence: the scheme
        // keyword and the length prefix lower as `Function` but name no
        // routine, and both affect row placement or key shape, not
        // table-level dependencies.
        let ignored = Self::in_ignored_function_root(&self.ignored_function_roots, expr.span());
        self.has_function |= !ignored
            && match expr {
                Expr::Function(function) => {
                    // Stored pure expressions (DEFAULT, CHECK, view/index bodies)
                    // lift the deferred fence like evaluated ones; everything
                    // else keeps it.
                    !(self.pure_table_enabled
                        && crate::pure::is_pure_builtin(function, self.dialect, self.rules))
                }
                Expr::CompoundIdentifier(idents)
                    if matches!(self.dialect, DialectKind::Oracle | DialectKind::Snowflake) =>
                {
                    idents.len() > 1
                        && idents.last().is_some_and(|ident| {
                            ident.value.eq_ignore_ascii_case("nextval")
                                || ident.value.eq_ignore_ascii_case("currval")
                        })
                }
                _ => false,
            };
        if !self.ignored_user_var_spans.contains(&expr.span()) {
            match expr {
                Expr::Identifier(ident) if crate::actions::is_user_variable_ident(ident) => {
                    let var = crate::actions::canonical_ident(
                        ident,
                        self.rules,
                        crate::ir::IdentifierClass::Other,
                    );
                    if !self.user_vars.contains(&var) {
                        self.user_vars.push(var);
                    }
                }
                Expr::CompoundIdentifier(idents)
                    if idents
                        .first()
                        .is_some_and(crate::actions::is_user_variable_ident) =>
                {
                    if let Some(first) = idents.first() {
                        let var = crate::actions::canonical_ident(
                            first,
                            self.rules,
                            crate::ir::IdentifierClass::Other,
                        );
                        if !self.user_vars.contains(&var) {
                            self.user_vars.push(var);
                        }
                    }
                }
                _ => {}
            }
        }
        let record_columns = self.nested_dml_depth == 0;
        let scope = self.current_alias_scope();
        match expr {
            Expr::Identifier(ident) if record_columns => {
                if !crate::actions::is_variable_ident(ident) {
                    self.columns.push((
                        vec![canonical_ident_string(
                            ident,
                            self.rules,
                            crate::ir::IdentifierClass::Column,
                        )],
                        scope,
                    ));
                }
            }
            _ if record_columns && matches!(self.dialect, DialectKind::MySql) => {
                if let Some(ident) = crate::actions::index_length_prefix_ident(expr) {
                    self.columns.push((
                        vec![canonical_ident_string(
                            ident,
                            self.rules,
                            crate::ir::IdentifierClass::Column,
                        )],
                        scope,
                    ));
                }
            }
            Expr::CompoundIdentifier(idents) if record_columns => {
                if !idents
                    .first()
                    .is_some_and(crate::actions::is_variable_ident)
                {
                    let last = idents.len().saturating_sub(1);
                    let parts = idents
                        .iter()
                        .enumerate()
                        .map(|(index, ident)| {
                            let class = if index == last {
                                crate::ir::IdentifierClass::Column
                            } else {
                                crate::ir::IdentifierClass::Relation
                            };
                            canonical_ident_string(ident, self.rules, class)
                        })
                        .collect();
                    self.columns.push((parts, scope));
                }
            }
            _ if !self.syntax_edits => {}
            Expr::BinaryOp { left, right, .. } => {
                if let Some((start, end)) = self.expression_source_span(expr) {
                    for replacement in [
                        self.expression_source_slice(left),
                        self.expression_source_slice(right),
                    ]
                    .into_iter()
                    .flatten()
                    {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-binary_expression",
                        );
                    }
                    for replacement in ["0", "1", "'0'"] {
                        self.add_low_yield_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-binary_expression",
                        );
                    }
                }
            }
            Expr::UnaryOp { expr: operand, .. } => {
                if let Some((start, end)) = self.expression_source_span(expr) {
                    if let Some(replacement) = self.expression_source_slice(operand) {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-unary_expression",
                        );
                    }
                    for replacement in ["0", "1", "'0'"] {
                        self.add_low_yield_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-unary_expression",
                        );
                    }
                }
            }
            Expr::Nested(inner) => {
                if let (Some((start, end)), Some(replacement)) = (
                    self.expression_source_span(expr),
                    self.expression_source_slice(inner),
                ) {
                    self.add_edit_bytes(
                        start,
                        end,
                        replacement,
                        NativeEditCategory::Expression,
                        "simplify-parenthesized_expression",
                    );
                }
            }
            Expr::Exists { .. } => {
                self.add_expression_edit(
                    expr,
                    "0",
                    NativeEditCategory::Expression,
                    "simplify-exists",
                );
            }
            Expr::Subquery(query) => {
                if let Some(literal) = scalar_literal(query)
                    && let (Some((start, end)), Some(replacement)) = (
                        self.expression_source_span(expr),
                        self.source_slice(literal.span()),
                    )
                {
                    self.add_edit_bytes(
                        start,
                        end,
                        replacement,
                        NativeEditCategory::Expression,
                        "simplify-scalar-subquery",
                    );
                }
            }
            Expr::In {
                expr: left, target, ..
            } => {
                // Simplify `x IN (...)` by proposing item removals for lists, or reducing the entire
                // IN expression to the LHS operand or boolean literals (1/0).
                let label = match target {
                    InTarget::List(list) => {
                        self.list_edits("remove-in_list", list, |visitor, item| {
                            visitor.expression_source_span(item)
                        });
                        "simplify-in_list"
                    }
                    _ => "simplify-in_subquery",
                };
                if let Some((start, end)) = self.expression_source_span(expr) {
                    if let Some(replacement) = self.expression_source_slice(left) {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            label,
                        );
                    }
                    for replacement in ["1", "0"] {
                        self.add_low_yield_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            label,
                        );
                    }
                }
            }
            Expr::Between {
                expr: inner,
                low,
                high,
                ..
            } => {
                if let Some((start, end)) = self.expression_source_span(expr) {
                    for candidate in [inner.as_ref(), low.as_ref(), high.as_ref()] {
                        if let Some(replacement) = self.expression_source_slice(candidate) {
                            self.add_edit_bytes(
                                start,
                                end,
                                replacement,
                                NativeEditCategory::Expression,
                                "simplify-between",
                            );
                        }
                    }
                    for replacement in ["1", "0"] {
                        self.add_low_yield_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-between",
                        );
                    }
                }
            }
            Expr::Like { expr: inner, .. } | Expr::ILike { expr: inner, .. } => {
                if let Some((start, end)) = self.expression_source_span(expr) {
                    if let Some(replacement) = self.expression_source_slice(inner) {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-like",
                        );
                    }
                    for replacement in ["1", "0"] {
                        self.add_low_yield_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-like",
                        );
                    }
                }
            }
            Expr::IsNull(inner) | Expr::IsNotNull(inner) => {
                if let Some((start, end)) = self.expression_source_span(expr) {
                    if let Some(replacement) = self.expression_source_slice(inner) {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-is_null",
                        );
                    }
                    for replacement in ["0", "1"] {
                        self.add_low_yield_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-is_null",
                        );
                    }
                }
            }
            Expr::Case {
                conditions,
                else_result,
                ..
            } => {
                if let Some((start, end)) = self.expression_source_span(expr) {
                    for cw in conditions {
                        if let Some(replacement) = self.expression_source_slice(&cw.result) {
                            self.add_edit_bytes(
                                start,
                                end,
                                replacement,
                                NativeEditCategory::Expression,
                                "simplify-case_result",
                            );
                        }
                    }
                    if let Some(else_expr) = else_result
                        && let Some(replacement) = self.expression_source_slice(else_expr)
                    {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-case_else",
                        );
                    }
                    for replacement in ["0", "1"] {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-case_literal",
                        );
                    }
                }
            }
            Expr::Tuple(items) => self.list_edits("remove-tuple_item", items, |visitor, item| {
                visitor.expression_source_span(item)
            }),
            Expr::Function(function) => {
                match &function.args {
                    sqlparser::ast::FunctionArguments::List(arguments) => {
                        // Propose removing duplicate treatment keywords (DISTINCT / ALL) immediately following the open parenthesis.
                        if let Some(treatment) = arguments.duplicate_treatment {
                            let kw = match treatment {
                                DuplicateTreatment::Distinct => Keyword::DISTINCT,
                                DuplicateTreatment::All => Keyword::ALL,
                            };
                            let (_, name_end) = self
                                .object_name_source_span(&function.name)
                                .unwrap_or((0, 0));
                            let start_idx = self.tokens.partition_point(|t| t.start < name_end);
                            if let Some((paren_idx, paren_tok)) =
                                self.next_significant(start_idx, self.unit.end)
                                && matches!(paren_tok.kind, TokenKind::LeftParen)
                                && let Some((kw_idx, kw_tok)) =
                                    self.next_significant(paren_idx + 1, self.unit.end)
                                && kw_tok.is_keyword(kw)
                                && let Some((_, next_tok)) =
                                    self.next_significant(kw_idx + 1, self.unit.end)
                            {
                                self.add_edit_bytes(
                                    kw_tok.start,
                                    next_tok.start,
                                    "",
                                    NativeEditCategory::Clause,
                                    "remove-distinct",
                                );
                            }
                        }
                        self.list_edits(
                            "remove-function_argument",
                            &arguments.args,
                            |visitor, argument| visitor.function_arg_source_span(argument),
                        );
                        if let Some((start, end)) = self.expression_source_span(expr) {
                            for arg in &arguments.args {
                                if let Some(arg_span) = self.function_arg_source_span(arg)
                                    && let Some(replacement) =
                                        self.source.get(arg_span.0..arg_span.1)
                                {
                                    self.add_edit_bytes(
                                        start,
                                        end,
                                        replacement,
                                        NativeEditCategory::Expression,
                                        "simplify-function_to_argument",
                                    );
                                }
                            }
                        }
                    }
                    sqlparser::ast::FunctionArguments::None
                    | sqlparser::ast::FunctionArguments::Subquery(_) => {}
                }
                if let Some(WindowType::WindowSpec(spec)) = &function.over {
                    let (_, name_end) = self
                        .object_name_source_span(&function.name)
                        .unwrap_or((0, 0));
                    let hint = self
                        .mapped_span(function.span())
                        .unwrap_or((0, self.unit.end));
                    let args_end = match (&function.args, &function.parameters) {
                        (
                            sqlparser::ast::FunctionArguments::None,
                            sqlparser::ast::FunctionArguments::None,
                        ) => name_end,
                        _ => self
                            .matching_parentheses(name_end, hint.1, name_end)
                            .map(|pair| pair.end)
                            .unwrap_or(name_end),
                    };
                    self.window_spec_edits(spec, args_end);
                }
                if let Some((start, end)) = self.expression_source_span(expr) {
                    for replacement in ["0", "1"] {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-function_literal",
                        );
                    }
                }
            }
            Expr::Cast {
                kind,
                expr: inner,
                data_type,
                ..
            } => {
                if let Some((_, inner_end)) = self.expression_source_span(inner)
                    && let Some(range) = self.cast_type_span(inner_end, data_type, kind)
                {
                    self.add_type_edits(range);
                }
                if let (Some((start, end)), Some(replacement)) = (
                    self.expression_source_span(expr),
                    self.expression_source_slice(inner),
                ) {
                    self.add_edit_bytes(
                        start,
                        end,
                        replacement,
                        NativeEditCategory::Expression,
                        "simplify-cast",
                    );
                    for lit in ["0", "1"] {
                        self.add_edit_bytes(
                            start,
                            end,
                            lit,
                            NativeEditCategory::Expression,
                            "simplify-cast_literal",
                        );
                    }
                }
            }
            _ => {}
        }
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_value(&mut self, value: &ValueWithSpan) -> std::ops::ControlFlow<Self::Break> {
        if self.syntax_edits {
            self.add_scalar_edit(value);
        }
        std::ops::ControlFlow::Continue(())
    }
}

impl AnalysisVisitor<'_> {
    fn collect_recursive_predicate_spans(
        &mut self,
        ctes: &[sqlparser::ast::Cte],
        names: &[String],
    ) {
        for (index, cte) in ctes.iter().enumerate() {
            self.collect_recursive_query_predicates(&cte.query, &names[index..]);
        }
    }

    fn collect_recursive_query_predicates(&mut self, query: &Query, targets: &[String]) {
        if targets.is_empty() {
            return;
        }
        if let Some(with) = &query.with {
            let shadowed: Vec<String> = with
                .cte_tables
                .iter()
                .map(|cte| {
                    canonical_ident_string(
                        &cte.alias.name,
                        self.rules,
                        crate::ir::IdentifierClass::Relation,
                    )
                })
                .collect();
            let active: Vec<String> = targets
                .iter()
                .filter(|name| !shadowed.iter().any(|s| s == *name))
                .cloned()
                .collect();
            if !active.is_empty() {
                self.collect_recursive_set_expr_predicates(&query.body, &active);
            }
        } else {
            self.collect_recursive_set_expr_predicates(&query.body, targets);
        }
    }

    fn collect_recursive_set_expr_predicates(&mut self, body: &SetExpr, targets: &[String]) {
        match body {
            SetExpr::Select(select) => self.collect_recursive_select_predicates(select, targets),
            SetExpr::Query(inner) => self.collect_recursive_query_predicates(inner, targets),
            SetExpr::SetOperation { left, right, .. } => {
                self.collect_recursive_set_expr_predicates(left, targets);
                self.collect_recursive_set_expr_predicates(right, targets);
            }
            _ => {}
        }
    }

    fn collect_recursive_select_predicates(&mut self, select: &Select, targets: &[String]) {
        if !self.select_from_references_any(&select.from, targets) {
            return;
        }
        for table in &select.from {
            self.collect_table_with_joins_predicates(table, targets);
        }
        if let Some(selection) = &select.selection
            && let Some(span) = self.expression_source_span(selection)
        {
            self.recursive_predicate_spans.push(span);
        }
    }

    fn collect_table_with_joins_predicates(&mut self, table: &TableWithJoins, targets: &[String]) {
        self.collect_table_factor_predicates(&table.relation, targets);
        for join in &table.joins {
            self.collect_join_operator_predicates(&join.join_operator);
            self.collect_table_factor_predicates(&join.relation, targets);
        }
    }

    fn collect_table_factor_predicates(&mut self, factor: &TableFactor, targets: &[String]) {
        match factor {
            TableFactor::NestedJoin {
                table_with_joins, ..
            } => self.collect_table_with_joins_predicates(table_with_joins, targets),
            TableFactor::Derived { subquery, .. } => {
                self.collect_recursive_query_predicates(subquery, targets);
            }
            _ => {}
        }
    }

    fn collect_join_operator_predicates(&mut self, operator: &sqlparser::ast::JoinOperator) {
        match operator {
            sqlparser::ast::JoinOperator::Join(constraint)
            | sqlparser::ast::JoinOperator::Inner(constraint)
            | sqlparser::ast::JoinOperator::Left(constraint)
            | sqlparser::ast::JoinOperator::LeftOuter(constraint)
            | sqlparser::ast::JoinOperator::Right(constraint)
            | sqlparser::ast::JoinOperator::RightOuter(constraint)
            | sqlparser::ast::JoinOperator::FullOuter(constraint)
            | sqlparser::ast::JoinOperator::CrossJoin(constraint)
            | sqlparser::ast::JoinOperator::Semi(constraint)
            | sqlparser::ast::JoinOperator::LeftSemi(constraint)
            | sqlparser::ast::JoinOperator::RightSemi(constraint)
            | sqlparser::ast::JoinOperator::Anti(constraint)
            | sqlparser::ast::JoinOperator::LeftAnti(constraint)
            | sqlparser::ast::JoinOperator::RightAnti(constraint)
            | sqlparser::ast::JoinOperator::StraightJoin(constraint) => {
                self.collect_join_constraint_predicates(constraint);
            }
            sqlparser::ast::JoinOperator::AsOf {
                match_condition,
                constraint,
            } => {
                if let Some(span) = self.expression_source_span(match_condition) {
                    self.recursive_predicate_spans.push(span);
                }
                self.collect_join_constraint_predicates(constraint);
            }
            _ => {}
        }
    }

    fn collect_join_constraint_predicates(&mut self, constraint: &sqlparser::ast::JoinConstraint) {
        if let sqlparser::ast::JoinConstraint::On(expr) = constraint
            && let Some(span) = self.expression_source_span(expr)
        {
            self.recursive_predicate_spans.push(span);
        }
    }

    fn query_references_any(&self, query: &Query, targets: &[String]) -> bool {
        if targets.is_empty() {
            return false;
        }
        if let Some(with) = &query.with {
            let shadowed: Vec<String> = with
                .cte_tables
                .iter()
                .map(|cte| {
                    canonical_ident_string(
                        &cte.alias.name,
                        self.rules,
                        crate::ir::IdentifierClass::Relation,
                    )
                })
                .collect();
            let active: Vec<String> = targets
                .iter()
                .filter(|name| !shadowed.iter().any(|s| s == *name))
                .cloned()
                .collect();
            !active.is_empty() && self.set_expr_references_any(&query.body, &active)
        } else {
            self.set_expr_references_any(&query.body, targets)
        }
    }

    fn set_expr_references_any(&self, body: &SetExpr, targets: &[String]) -> bool {
        match body {
            SetExpr::Select(select) => self.select_from_references_any(&select.from, targets),
            SetExpr::Query(inner) => self.query_references_any(inner, targets),
            SetExpr::SetOperation { left, right, .. } => {
                self.set_expr_references_any(left, targets)
                    || self.set_expr_references_any(right, targets)
            }
            _ => false,
        }
    }

    fn select_from_references_any(&self, from: &[TableWithJoins], targets: &[String]) -> bool {
        from.iter()
            .any(|table| self.table_with_joins_references_any(table, targets))
    }

    fn table_with_joins_references_any(&self, table: &TableWithJoins, targets: &[String]) -> bool {
        self.table_factor_references_any(&table.relation, targets)
            || table
                .joins
                .iter()
                .any(|join| self.table_factor_references_any(&join.relation, targets))
    }

    fn table_factor_references_any(&self, factor: &TableFactor, targets: &[String]) -> bool {
        match factor {
            TableFactor::Table {
                name, args: None, ..
            } => name_parts(name, self.rules, ir::IdentifierClass::Relation).is_some_and(|parts| {
                parts.len() == 1 && targets.iter().any(|target| target == &parts[0].0)
            }),
            TableFactor::NestedJoin {
                table_with_joins, ..
            } => self.table_with_joins_references_any(table_with_joins, targets),
            TableFactor::Derived { subquery, .. } => self.query_references_any(subquery, targets),
            _ => false,
        }
    }

    fn add_scalar_edit(&mut self, value: &ValueWithSpan) {
        let Some((start, end)) = self.mapped_span(value.span) else {
            return;
        };
        let Some(raw) = self.source.get(start..end) else {
            return;
        };
        let is_string = matches!(value.value, sqlparser::ast::Value::SingleQuotedString(_))
            || raw.starts_with('\'');
        let replacements = if is_string {
            [("''", false), ("'0'", true), ("NULL", true), ("0", true)].as_slice()
        } else {
            [
                ("0", false),
                ("1", false),
                ("NULL", true),
                ("FALSE", true),
                ("TRUE", true),
                ("''", true),
            ]
            .as_slice()
        };
        for &(replacement, low_yield) in replacements {
            if replacement.len() < end - start {
                self.add_edit_bytes_with_flags(
                    start,
                    end,
                    replacement,
                    NativeEditCategory::Literal,
                    "simplify-literal",
                    is_string,
                    low_yield,
                );
            }
        }
    }
}

pub(crate) fn scalar_literal(query: &Query) -> Option<&ValueWithSpan> {
    let SetExpr::Select(select) = &*query.body else {
        return None;
    };
    if !select.from.is_empty() || select.selection.is_some() || select.projection.len() != 1 {
        return None;
    }
    let SelectItem::UnnamedExpr(Expr::Value(value)) = &select.projection[0] else {
        return None;
    };
    Some(value)
}

pub(crate) fn preceding_space(source: &[u8], start: usize) -> usize {
    if start >= 2 && source[start - 2..start] == *b"\r\n" {
        start - 2
    } else if start > 0 && matches!(source[start - 1], b' ' | b'\t' | b'\r' | b'\n') {
        start - 1
    } else {
        start
    }
}

pub(crate) fn is_pure_table_generator_name(name: &str) -> bool {
    matches!(
        name.to_ascii_lowercase().as_str(),
        "generate_series"
            | "generate_subscripts"
            | "range"
            | "repeat"
            | "unnest"
            | "numbers"
            | "numbers_mt"
            | "zeros"
            | "zeros_mt"
    )
}

fn builtin_table_function_ident(name: &ObjectName) -> Option<&str> {
    match name.0.as_slice() {
        [sqlparser::ast::ObjectNamePart::Identifier(ident)] => Some(ident.value.as_str()),
        [
            sqlparser::ast::ObjectNamePart::Identifier(schema),
            sqlparser::ast::ObjectNamePart::Identifier(ident),
        ] if schema.value.eq_ignore_ascii_case("pg_catalog") => Some(ident.value.as_str()),
        _ => None,
    }
}

pub(crate) fn is_pure_table_generator(name: &ObjectName) -> bool {
    match builtin_table_function_ident(name) {
        Some(ident) => is_pure_table_generator_name(ident),
        None => false,
    }
}

pub(crate) fn is_file_table_function_name(name: &str) -> bool {
    matches!(
        name.to_ascii_lowercase().as_str(),
        "read_parquet"
            | "read_csv"
            | "read_csv_auto"
            | "read_json"
            | "read_json_auto"
            | "read_ndjson"
            | "read_ndjson_auto"
            | "parquet_scan"
            | "scan_parquet"
            | "glob"
    )
}

pub(crate) fn is_file_table_function(name: &ObjectName) -> bool {
    match builtin_table_function_ident(name) {
        Some(ident) => is_file_table_function_name(ident),
        None => false,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum TableFunctionKind {
    PureGenerator,
    FileTable(Vec<String>),
    Unmodeled,
}

pub(crate) fn classify_table_function(
    name: &ObjectName,
    args: &[sqlparser::ast::FunctionArg],
) -> TableFunctionKind {
    match (is_pure_table_generator(name), is_file_table_function(name)) {
        (true, _) => TableFunctionKind::PureGenerator,
        (_, true) => match extract_paths_from_function_args(args) {
            paths if paths.is_empty() => TableFunctionKind::Unmodeled,
            paths => TableFunctionKind::FileTable(paths),
        },
        _ => TableFunctionKind::Unmodeled,
    }
}

fn is_path_param_name(name: &str) -> bool {
    matches!(
        name.to_ascii_lowercase().as_str(),
        "files" | "filename" | "path" | "paths"
    )
}

pub(crate) fn extract_paths_from_function_args(
    args: &[sqlparser::ast::FunctionArg],
) -> Vec<String> {
    let mut paths = Vec::new();
    if let Some(sqlparser::ast::FunctionArg::Unnamed(sqlparser::ast::FunctionArgExpr::Expr(expr))) =
        args.first()
    {
        collect_string_literals(expr, &mut paths);
    }
    for arg in args {
        match arg {
            sqlparser::ast::FunctionArg::Named {
                name,
                arg: sqlparser::ast::FunctionArgExpr::Expr(expr),
                ..
            } if is_path_param_name(&name.value) => {
                collect_string_literals(expr, &mut paths);
            }
            sqlparser::ast::FunctionArg::ExprNamed {
                name: Expr::Identifier(ident),
                arg: sqlparser::ast::FunctionArgExpr::Expr(expr),
                ..
            } if is_path_param_name(&ident.value) => {
                collect_string_literals(expr, &mut paths);
            }
            _ => {}
        }
    }
    paths
}

fn collect_string_literals(expr: &Expr, paths: &mut Vec<String>) {
    match expr {
        Expr::Value(v) => match &v.value {
            sqlparser::ast::Value::SingleQuotedString(s)
            | sqlparser::ast::Value::DoubleQuotedString(s)
            | sqlparser::ast::Value::TripleSingleQuotedString(s)
            | sqlparser::ast::Value::TripleDoubleQuotedString(s) => {
                paths.push(s.clone());
            }
            _ => {}
        },
        Expr::Array(array) => {
            for elem in &array.elem {
                collect_string_literals(elem, paths);
            }
        }
        _ => {}
    }
}

pub(crate) fn preceding_whitespace(source: &[u8], mut start: usize) -> usize {
    while start > 0 && matches!(source[start - 1], b' ' | b'\t' | b'\r' | b'\n') {
        start -= 1;
    }
    start
}

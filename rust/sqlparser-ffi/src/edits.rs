use crate::actions::{canonical_ident, name_parts};
use crate::source::{dialect, ByteSpan, SourceMap, TokenKind, TokenPosition};
use crate::{DialectKind, NativeEdit, NativeEditCategory};
use sqlparser::ast::{
    AlterTableOperation, CastKind, DataType, Expr, FunctionArg, FunctionArgExpr, GroupByExpr,
    GroupByWithModifier, LimitClause, ObjectName, OffsetRows, Query, Select, SelectItem,
    SelectItemQualifiedWildcardKind, SetExpr, Spanned, Statement, TableConstraint, TableFactor,
    TableWithJoins, ValueWithSpan, Visitor,
};
use sqlparser::keywords::Keyword;
use sqlparser::tokenizer::{Span, Token, Tokenizer};
use std::borrow::Cow;
#[derive(Clone)]
struct CteScope {
    recursive: bool,
    names: Vec<String>,
    body_spans: Vec<Option<(usize, usize)>>,
}

pub(super) struct AnalysisVisitor<'a> {
    pub(super) dialect: DialectKind,
    source: &'a str,
    source_map: &'a SourceMap,
    scopes: Vec<CteScope>,
    pub(super) relations: Vec<(ObjectName, Span, bool)>,
    pub(super) unmodeled_relation_spans: Vec<Span>,
    pub(super) unmodeled_table_source: bool,
    pub(super) columns: Vec<Vec<String>>,
    pub(super) edits: Vec<NativeEdit>,
    tokens: &'a [TokenPosition],
    unit: ByteSpan,
}
impl<'a> AnalysisVisitor<'a> {
    pub(super) fn new(
        dialect: DialectKind,
        source: &'a str,
        source_map: &'a SourceMap,
        tokens: &'a [TokenPosition],
        unit: ByteSpan,
    ) -> Self {
        Self {
            dialect,
            source,
            source_map,
            scopes: Vec::new(),
            relations: Vec::new(),
            unmodeled_relation_spans: Vec::new(),
            unmodeled_table_source: false,
            columns: Vec::new(),
            edits: Vec::new(),
            tokens,
            unit,
        }
    }

    fn is_cte_reference(&self, name: &ObjectName, start: usize) -> bool {
        let Some(parts) = name_parts(name, self.dialect) else {
            return false;
        };
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
                .any(|candidate| candidate == name)
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
            if matches!(token.kind, TokenKind::LeftParen) {
                if let Some(pair) = token.parentheses.filter(|pair| pair.end >= end) {
                    return Some(pair);
                }
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
        let range = match expr {
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
            Expr::InUnnest {
                expr,
                array_expr,
                negated,
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
            Expr::InSubquery { expr, subquery, .. } => {
                let (start, _) = self.expression_source_span(expr)?;
                let range = self.mapped_span(subquery.span())?;
                let opening = self.previous_significant(range.0)?;
                if !matches!(opening.kind, TokenKind::LeftParen) {
                    return None;
                }
                let pair = self.matching_parentheses(range.0, range.1, opening.start)?;
                Some((start, pair.end))
            }
            Expr::InList { expr, list, .. } => {
                let (start, left_end) = self.expression_source_span(expr)?;
                let (_, last_end) = self.expression_source_span(list.last()?)?;
                let pair = self.matching_parentheses(left_end, last_end, left_end)?;
                Some((start, pair.end))
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
                    || function.over.is_some()
                    || !function.within_group.is_empty()
                    || function.filter.is_some()
                {
                    None
                } else {
                    let (start, name_end) = self.object_name_source_span(&function.name)?;
                    let hint = self.mapped_span(function.span())?;
                    if matches!(&function.args, sqlparser::ast::FunctionArguments::None)
                        && matches!(
                            &function.parameters,
                            sqlparser::ast::FunctionArguments::None
                        )
                    {
                        Some((start, name_end))
                    } else {
                        self.matching_parentheses(start, hint.1, start)
                            .map(|pair| (start, pair.end))
                    }
                }
            }
            Expr::Identifier(ident) => self.mapped_span(ident.span),
            Expr::CompoundIdentifier(identifiers) => {
                self.dotted_source_span(identifiers.iter().map(|identifier| Some(identifier.span)))
            }
            Expr::Value(_) | Expr::Wildcard(_) => self.mapped_span(expr.span()),
            _ => None,
        };
        range
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
        let Some(range) = ByteSpan::checked(self.source, start, end) else {
            return;
        };
        self.edits.push(NativeEdit {
            start: range.start,
            end: range.end,
            replacement: replacement.to_owned(),
            category,
            label: label.into(),
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
            if matches!(
                token.kind,
                TokenKind::RightParen | TokenKind::Semicolon | TokenKind::Eof
            ) || following_clauses
                .iter()
                .any(|keyword| token.is_keyword(*keyword))
            {
                return Some(preceding_space(self.source.as_bytes(), token.start));
            }
            index += 1;
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
        let (start, _) = self.expression_source_span(&expressions.first()?.expr)?;
        let (_, end) = self.expression_source_span(&expressions.last()?.expr)?;
        Some((start, end))
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
            if matches!(token.kind, TokenKind::LeftParen) {
                if let Some(pair) = token.parentheses {
                    index = self
                        .tokens
                        .partition_point(|candidate| candidate.end <= pair.end);
                    continue;
                }
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
            if token.is_keyword(Keyword::GROUP) {
                if let Some((by_index, by)) = self.next_significant(index + 1, scope_end) {
                    if by.is_keyword(Keyword::BY) {
                        if let Some((_, all)) = self.next_significant(by_index + 1, scope_end) {
                            if all.is_keyword(Keyword::ALL) {
                                return Some((token.start, all.end, scope_end));
                            }
                        }
                    }
                }
            }
            if matches!(token.kind, TokenKind::LeftParen) {
                if let Some(pair) = token.parentheses {
                    index = self
                        .tokens
                        .partition_point(|candidate| candidate.end <= pair.end);
                    continue;
                }
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
            if token.is_keyword(first) {
                if let Some((_, next)) = self.next_significant(index + 1, scope_end) {
                    if next.is_keyword(second) {
                        return Some(next.end);
                    }
                }
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
                index_hints,
            } => {
                if args.is_some()
                    || !with_hints.is_empty()
                    || version.is_some()
                    || *with_ordinality
                    || !partitions.is_empty()
                    || json_path.is_some()
                    || sample.is_some()
                    || !index_hints.is_empty()
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
            if matches!(token.kind, TokenKind::LeftParen) {
                if let Some(pair) = token.parentheses {
                    col_end = pair.end;
                    index = self.tokens.partition_point(|t| t.end <= pair.end);
                    continue;
                }
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
                if let (Some(token), Some(offset)) = (offset_token, offset.as_ref()) {
                    if let Some(end) = self.offset_end(offset) {
                        self.add_edit_bytes(
                            preceding_space(self.source.as_bytes(), token.start),
                            end,
                            "",
                            NativeEditCategory::Clause,
                            "remove-offset",
                        );
                    }
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
        }) {
            if let Some(replacement) = self.source.get(token.start..token.end).map(str::to_owned) {
                if replacement.len() < end - start {
                    self.add_edit_bytes(
                        start,
                        end,
                        &replacement,
                        NativeEditCategory::Type,
                        "simplify-data_type",
                    );
                }
            }
        }
    }
}

impl Visitor for AnalysisVisitor<'_> {
    type Break = ();

    fn pre_visit_query(&mut self, query: &Query) -> std::ops::ControlFlow<Self::Break> {
        if let Some(with) = &query.with {
            let names = with
                .cte_tables
                .iter()
                .map(|cte| canonical_ident(&cte.alias.name, self.dialect))
                .collect();
            let body_spans = with
                .cte_tables
                .iter()
                .map(|cte| self.cte_query_source_span(cte))
                .collect();
            self.scopes.push(CteScope {
                recursive: with.recursive,
                names,
                body_spans,
            });
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
        } else {
            self.scopes.push(CteScope {
                recursive: false,
                names: Vec::new(),
                body_spans: Vec::new(),
            });
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
        }
        if let Some(limit) = &query.limit_clause {
            self.limit_edits(query, limit);
        }
        if let Some(fetch) = &query.fetch {
            if let Some(scope) = query_scope {
                if let Some(body) = self.fetch_source_span(fetch, scope) {
                    self.clause_edit_complete(
                        Keyword::FETCH,
                        body,
                        scope,
                        &[Keyword::FOR, Keyword::SETTINGS, Keyword::FORMAT],
                        "remove-fetch",
                    );
                }
            }
        }
        if let SetExpr::Values(values) = query.body.as_ref() {
            if values.rows.len() > 1 {
                let spans: Option<Vec<_>> = values
                    .rows
                    .iter()
                    .map(|row| self.values_row_span(row, values.explicit_row))
                    .collect();
                if let Some(spans) = spans {
                    self.list_edit_ranges("remove-values_row", &spans);
                }
            }
        }
        std::ops::ControlFlow::Continue(())
    }

    fn post_visit_query(&mut self, _query: &Query) -> std::ops::ControlFlow<Self::Break> {
        self.scopes.pop();
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_select(&mut self, select: &Select) -> std::ops::ControlFlow<Self::Break> {
        self.list_edits(
            "remove-select_expression",
            &select.projection,
            |visitor, item| visitor.select_item_source_span(item),
        );
        self.list_edits("remove-from-relation", &select.from, |visitor, table| {
            visitor.table_with_joins_source_span(table)
        });
        for table in &select.from {
            let mut previous_end = self
                .table_factor_source_span(&table.relation)
                .map(|(_, end)| end);
            for join in &table.joins {
                if let (Some(start), Some(end)) = (previous_end, self.join_end(join)) {
                    if start < end {
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
            }
        }
        if let Some(selection) = &select.selection {
            self.clause_edit_expression(Keyword::WHERE, selection, select.span(), "remove-where");
        }
        if let Some(having) = &select.having {
            self.clause_edit_expression(Keyword::HAVING, having, select.span(), "remove-having");
        }
        match &select.group_by {
            GroupByExpr::All(modifiers) => self.group_by_all_edit(select, modifiers),
            GroupByExpr::Expressions(..) => {
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
        match statement {
            Statement::CreateTable(create) => {
                for column in &create.columns {
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
                        if let Some(range) = self.column_type_span(column_def) {
                            self.add_type_edits(range);
                        }
                        if let Some((opt_start, opt_end)) = self.column_options_span(column_def) {
                            self.add_edit_bytes(
                                opt_start,
                                opt_end,
                                "",
                                NativeEditCategory::Constraint,
                                "remove-column_constraint",
                            );
                        }
                        for option in &column_def.options {
                            if let Some((start, end)) = self.mapped_span(option.span()) {
                                let edit_start =
                                    preceding_whitespace(self.source.as_bytes(), start);
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
            }
            _ => {}
        }
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_table_factor(
        &mut self,
        factor: &TableFactor,
    ) -> std::ops::ControlFlow<Self::Break> {
        match factor {
            TableFactor::Table {
                name,
                args: Some(_),
                ..
            }
            | TableFactor::Function { name, .. } => {
                self.unmodeled_relation_spans.push(name.span());
            }
            TableFactor::TableFunction { .. } => self.unmodeled_table_source = true,
            _ => {}
        }
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_relation(&mut self, relation: &ObjectName) -> std::ops::ControlFlow<Self::Break> {
        let is_cte = self.is_cte_reference(
            relation,
            self.mapped_span(relation.span()).map_or(0, |span| span.0),
        );
        self.relations
            .push((relation.clone(), relation.span(), is_cte));
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_expr(&mut self, expr: &Expr) -> std::ops::ControlFlow<Self::Break> {
        match expr {
            Expr::Identifier(ident) => self
                .columns
                .push(vec![canonical_ident(ident, self.dialect)]),
            Expr::CompoundIdentifier(idents) => self.columns.push(
                idents
                    .iter()
                    .map(|ident| canonical_ident(ident, self.dialect))
                    .collect(),
            ),
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
                        self.add_edit_bytes(
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
                        self.add_edit_bytes(
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
                if let Some(literal) = scalar_literal(query) {
                    if let (Some((start, end)), Some(replacement)) = (
                        self.expression_source_span(expr),
                        self.source_slice(literal.span()),
                    ) {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-scalar-subquery",
                        );
                    }
                }
            }
            Expr::InList {
                expr: inner, list, ..
            } => {
                self.list_edits("remove-in_list", list, |visitor, item| {
                    visitor.expression_source_span(item)
                });
                if let Some((start, end)) = self.expression_source_span(expr) {
                    if let Some(replacement) = self.expression_source_slice(inner) {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-in_list",
                        );
                    }
                    for replacement in ["1", "0"] {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-in_list",
                        );
                    }
                }
            }
            Expr::InSubquery { expr: inner, .. } => {
                if let Some((start, end)) = self.expression_source_span(expr) {
                    if let Some(replacement) = self.expression_source_slice(inner) {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-in_subquery",
                        );
                    }
                    for replacement in ["1", "0"] {
                        self.add_edit_bytes(
                            start,
                            end,
                            replacement,
                            NativeEditCategory::Expression,
                            "simplify-in_subquery",
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
                        self.add_edit_bytes(
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
                        self.add_edit_bytes(
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
                        self.add_edit_bytes(
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
                    if let Some(else_expr) = else_result {
                        if let Some(replacement) = self.expression_source_slice(else_expr) {
                            self.add_edit_bytes(
                                start,
                                end,
                                replacement,
                                NativeEditCategory::Expression,
                                "simplify-case_else",
                            );
                        }
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
                if let sqlparser::ast::FunctionArguments::List(arguments) = &function.args {
                    self.list_edits(
                        "remove-function_argument",
                        &arguments.args,
                        |visitor, argument| visitor.function_arg_source_span(argument),
                    );
                    if let Some((start, end)) = self.expression_source_span(expr) {
                        for arg in &arguments.args {
                            if let Some(arg_span) = self.function_arg_source_span(arg) {
                                if let Some(replacement) = self.source.get(arg_span.0..arg_span.1) {
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
                if let Some((_, inner_end)) = self.expression_source_span(inner) {
                    if let Some(range) = self.cast_type_span(inner_end, data_type, kind) {
                        self.add_type_edits(range);
                    }
                }
            }
            _ => {}
        }
        std::ops::ControlFlow::Continue(())
    }

    fn pre_visit_value(&mut self, value: &ValueWithSpan) -> std::ops::ControlFlow<Self::Break> {
        self.add_scalar_edit(value);
        std::ops::ControlFlow::Continue(())
    }
}

impl AnalysisVisitor<'_> {
    fn add_scalar_edit(&mut self, value: &ValueWithSpan) {
        let Some((start, end)) = self.mapped_span(value.span) else {
            return;
        };
        let Some(raw) = self.source.get(start..end) else {
            return;
        };
        let replacements = if raw.starts_with('\'') {
            ["''", "'0'", "NULL", "0"].as_slice()
        } else {
            ["0", "1", "NULL", "FALSE", "TRUE", "''"].as_slice()
        };
        for replacement in replacements {
            if replacement.len() < end - start {
                self.add_edit_bytes(
                    start,
                    end,
                    replacement,
                    NativeEditCategory::Literal,
                    "simplify-literal",
                );
            }
        }
    }
}

fn scalar_literal(query: &Query) -> Option<&ValueWithSpan> {
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

fn preceding_space(source: &[u8], start: usize) -> usize {
    if start >= 2 && source[start - 2..start] == *b"\r\n" {
        start - 2
    } else if start > 0 && matches!(source[start - 1], b' ' | b'\t' | b'\r' | b'\n') {
        start - 1
    } else {
        start
    }
}

fn preceding_whitespace(source: &[u8], mut start: usize) -> usize {
    while start > 0 && matches!(source[start - 1], b' ' | b'\t' | b'\r' | b'\n') {
        start -= 1;
    }
    start
}

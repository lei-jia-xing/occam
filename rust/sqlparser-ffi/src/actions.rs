use crate::DialectKind;
use crate::edits::{AnalysisContext, AnalysisVisitor, SequenceVisitor};
use crate::ir;
use sqlparser::ast::*;
use sqlparser::tokenizer::Span;
use std::collections::HashSet;

fn canonical_ident(
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

pub(super) fn canonical_ident_string(
    ident: &Ident,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> String {
    canonical_ident(ident, rules, class).0
}

struct ColumnCollector<'a> {
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

fn expr_columns(expr: &Expr, rules: &ir::IdentifierRules) -> Vec<ir::Ident> {
    let mut collector = ColumnCollector {
        rules,
        columns: Vec::new(),
        seen: HashSet::new(),
    };
    let _ = expr.visit(&mut collector);
    collector.columns
}

pub(super) fn name_parts(
    name: &ObjectName,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> Option<Vec<ir::Ident>> {
    if name.0.is_empty() || name.0.len() > 4 {
        return None;
    }
    name.0
        .iter()
        .map(|part| match part {
            ObjectNamePart::Identifier(ident) => Some(canonical_ident(ident, rules, class)),
            ObjectNamePart::Function(_) => None,
        })
        .collect()
}

fn name_from_object(
    name: &ObjectName,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> Option<ir::Name> {
    Some(ir::Name(name_parts(name, rules, class)?))
}

fn strip_temp_schema(parts: &mut Vec<ir::Ident>, dialect: DialectKind) -> bool {
    if parts.len() < 2 {
        return false;
    }
    let first = parts[0].0.as_str();
    let is_temp = match dialect {
        DialectKind::PostgreSql | DialectKind::Redshift => {
            first == "pg_temp" || first.starts_with("pg_temp_")
        }
        DialectKind::SQLite | DialectKind::DuckDB => first == "temp",
        _ => false,
    };
    if is_temp {
        parts.remove(0);
        true
    } else {
        false
    }
}

fn relation_ref(
    name: &ObjectName,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<ir::NameRef> {
    let mut parts = name_parts(name, rules, ir::IdentifierClass::Relation)?;
    let temp = strip_temp_schema(&mut parts, dialect);
    Some(ir::NameRef {
        name: ir::Name(parts),
        temp,
    })
}

fn definition_name(
    name: &ObjectName,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
    declared_temporary: bool,
) -> Option<(ir::Name, bool)> {
    let mut parts = name_parts(name, rules, ir::IdentifierClass::Relation)?;
    let schema_temp = strip_temp_schema(&mut parts, dialect);
    Some((ir::Name(parts), declared_temporary || schema_temp))
}

/// Split a literal identifier list on `separator`, ignoring separators inside
/// double-quoted identifiers. A doubled quote inside a quoted identifier is an
/// escaped quote.
fn split_identifier_list(value: &str, separator: char) -> Option<Vec<&str>> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut in_quotes = false;
    let mut chars = value.char_indices().peekable();
    while let Some((index, character)) = chars.next() {
        match character {
            '"' => {
                if in_quotes && chars.peek().is_some_and(|(_, next)| *next == '"') {
                    chars.next();
                } else {
                    in_quotes = !in_quotes;
                }
            }
            c if c == separator && !in_quotes => {
                parts.push(&value[start..index]);
                start = index + c.len_utf8();
            }
            _ => {}
        }
    }
    if in_quotes {
        return None;
    }
    parts.push(&value[start..]);
    Some(parts)
}

/// Parse one identifier from a string literal. Unquoted identifiers may not
/// contain whitespace or quotes; quoted identifiers preserve whitespace and
/// use doubled quotes for an embedded quote. Zero-length quoted identifiers
/// are rejected, matching PostgreSQL.
pub(super) fn literal_identifier_part(part: &str) -> Option<(String, bool)> {
    let part = part.trim();
    if part.is_empty() {
        return None;
    }
    if part.starts_with('"') {
        if part.len() < 2 || !part.ends_with('"') {
            return None;
        }
        let mut value = String::new();
        let mut chars = part[1..part.len() - 1].chars();
        while let Some(character) = chars.next() {
            if character == '"' {
                if chars.next() == Some('"') {
                    value.push('"');
                } else {
                    return None;
                }
            } else {
                value.push(character);
            }
        }
        if value.is_empty() {
            return None;
        }
        Some((value, true))
    } else {
        if part.contains('"') || part.chars().any(char::is_whitespace) {
            return None;
        }
        Some((part.to_string(), false))
    }
}

/// Parse the textual relation name accepted by PostgreSQL sequence functions,
/// e.g. `nextval('tenant."MixedCase"."seq"')`.
fn literal_relation_name(value: &str, rules: &ir::IdentifierRules) -> Option<ir::Name> {
    let mut parts = Vec::new();
    for part in split_identifier_list(value, '.')? {
        let (name, quoted) = literal_identifier_part(part)?;
        let ident = if quoted {
            Ident::with_quote('"', name.as_str())
        } else {
            Ident::new(name.as_str())
        };
        parts.push(canonical_ident(
            &ident,
            rules,
            ir::IdentifierClass::Relation,
        ));
    }
    Some(ir::Name(parts))
}

pub(super) fn sequence_ref_from_expr(
    expr: &Expr,
    rules: &ir::IdentifierRules,
) -> Option<ir::NameRef> {
    match expr {
        Expr::Value(value) => match &value.value {
            Value::SingleQuotedString(name)
            | Value::TripleSingleQuotedString(name)
            | Value::EscapedStringLiteral(name) => {
                literal_relation_name(name, rules).map(|name| ir::NameRef { name, temp: false })
            }
            Value::DoubleQuotedString(name) | Value::TripleDoubleQuotedString(name) => {
                let ident = Ident::with_quote('"', name);
                Some(ir::NameRef {
                    name: ir::Name(vec![canonical_ident(
                        &ident,
                        rules,
                        ir::IdentifierClass::Relation,
                    )]),
                    temp: false,
                })
            }
            _ => None,
        },
        Expr::Identifier(ident) => Some(ir::NameRef {
            name: ir::Name(vec![canonical_ident(
                ident,
                rules,
                ir::IdentifierClass::Relation,
            )]),
            temp: false,
        }),
        Expr::CompoundIdentifier(idents) if !idents.is_empty() => Some(ir::NameRef {
            name: ir::Name(
                idents
                    .iter()
                    .map(|ident| canonical_ident(ident, rules, ir::IdentifierClass::Relation))
                    .collect(),
            ),
            temp: false,
        }),
        Expr::Cast { expr, .. } | Expr::Nested(expr) => sequence_ref_from_expr(expr, rules),
        _ => None,
    }
}

pub(super) fn sequence_function_ref(
    function: &Function,
    rules: &ir::IdentifierRules,
) -> Option<ir::NameRef> {
    let name = match function.name.0.last()? {
        ObjectNamePart::Identifier(ident) => ident,
        ObjectNamePart::Function(_) => return None,
    };
    if !matches!(
        name.value.to_ascii_lowercase().as_str(),
        "nextval" | "setval"
    ) {
        return None;
    }
    let arguments = match &function.args {
        FunctionArguments::List(list) => &list.args,
        FunctionArguments::None | FunctionArguments::Subquery(_) => return None,
    };
    let argument = match arguments.first()? {
        FunctionArg::Unnamed(FunctionArgExpr::Expr(expr))
        | FunctionArg::Named {
            arg: FunctionArgExpr::Expr(expr),
            ..
        }
        | FunctionArg::ExprNamed {
            arg: FunctionArgExpr::Expr(expr),
            ..
        } => expr,
        _ => return None,
    };
    sequence_ref_from_expr(argument, rules)
}

/// Oracle exposes sequence state through `seq.NEXTVAL` rather than a function.
pub(super) fn sequence_nextval_ref(
    idents: &[Ident],
    rules: &ir::IdentifierRules,
) -> Option<ir::NameRef> {
    let (last, name) = idents.split_last()?;
    if !last.value.eq_ignore_ascii_case("nextval") || name.is_empty() {
        return None;
    }
    Some(ir::NameRef {
        name: ir::Name(
            name.iter()
                .map(|ident| canonical_ident(ident, rules, ir::IdentifierClass::Relation))
                .collect(),
        ),
        temp: false,
    })
}

/// A sequence state change expressed by an evaluated expression. PostgreSQL
/// and Redshift use `nextval`/`setval`; Oracle and Snowflake use
/// `seq.NEXTVAL`.
pub(super) fn sequence_expr_ref(
    expr: &Expr,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<ir::NameRef> {
    match expr {
        Expr::CompoundIdentifier(idents)
            if matches!(dialect, DialectKind::Oracle | DialectKind::Snowflake) =>
        {
            sequence_nextval_ref(idents, rules)
        }
        Expr::Function(function)
            if matches!(
                dialect,
                DialectKind::PostgreSql | DialectKind::Redshift | DialectKind::Generic
            ) =>
        {
            sequence_function_ref(function, rules)
        }
        _ => None,
    }
}

fn set_expr_output_columns(expr: &SetExpr, rules: &ir::IdentifierRules) -> Option<Vec<ir::Ident>> {
    match expr {
        SetExpr::Select(select) => {
            let mut columns = Vec::new();
            for item in &select.projection {
                let ident = match item {
                    SelectItem::UnnamedExpr(Expr::Identifier(ident)) => ident,
                    SelectItem::UnnamedExpr(Expr::CompoundIdentifier(idents)) => idents.last()?,
                    SelectItem::ExprWithAlias { alias, .. } => alias,
                    // A wildcard or an unnamed computed expression makes the
                    // projection shape unknowable from the AST. Returning a
                    // partial list would let downstream code treat the known
                    // names as a closed-world column set.
                    _ => return None,
                };
                columns.push(canonical_ident(ident, rules, ir::IdentifierClass::Column));
            }
            Some(columns)
        }
        SetExpr::Query(query) => set_expr_output_columns(&query.body, rules),
        SetExpr::SetOperation { left, right, .. } => {
            let columns = set_expr_output_columns(left, rules)?;
            // Set-operation output names come from the left branch, but a
            // mismatched arity is not a closed column set: the server rejects
            // the statement, and downstream analysis must stay conservative.
            if let Some(right_columns) = set_expr_output_columns(right, rules)
                && right_columns.len() != columns.len()
            {
                return None;
            }
            Some(columns)
        }
        _ => None,
    }
}

fn query_output_columns(query: &Query, rules: &ir::IdentifierRules) -> Option<Vec<ir::Ident>> {
    set_expr_output_columns(&query.body, rules)
}

fn qualify_rename(old: &ir::Name, new: ir::Name) -> ir::Name {
    if new.len() == 1 && old.len() > 1 {
        let mut parts = old.0[..old.0.len() - 1].to_vec();
        parts.extend(new.0);
        ir::Name(parts)
    } else {
        new
    }
}

fn last_ident(name: &ir::Name) -> Option<ir::Ident> {
    name.last().cloned()
}

fn barrier(
    scope: ir::BarrierScope,
    reason: ir::BarrierReason,
    detail: Option<String>,
) -> ir::ScopedBarrier {
    ir::ScopedBarrier {
        scope,
        reason,
        detail,
    }
}

fn global_barrier(reason: ir::BarrierReason, detail: Option<String>) -> ir::ScopedBarrier {
    barrier(ir::BarrierScope::Global, reason, detail)
}

fn relation_barrier(
    relation: ir::NameRef,
    reason: ir::BarrierReason,
    detail: Option<String>,
) -> ir::ScopedBarrier {
    barrier(ir::BarrierScope::Relation { relation }, reason, detail)
}

fn session_barrier(reason: ir::BarrierReason, detail: Option<String>) -> ir::ScopedBarrier {
    barrier(ir::BarrierScope::SessionState, reason, detail)
}

fn transaction_barrier(reason: ir::BarrierReason, detail: Option<String>) -> ir::ScopedBarrier {
    barrier(ir::BarrierScope::TransactionState, reason, detail)
}

fn read_fact(source: ir::NameRef, mode: ir::ReadMode, columns: Vec<ir::Ident>) -> ir::ReadFact {
    ir::ReadFact {
        source,
        mode,
        columns,
    }
}

/// Whether a relation declared in `relation_scope` is lexically visible from
/// `column_scope`. The factor position gates unaliased table references, so a
/// non-LATERAL derived table cannot see sibling FROM items and a LATERAL one
/// only sees the items before it.
fn relation_visible(
    relation_scope: usize,
    relation_factor: usize,
    column_scope: usize,
    aliases: &[crate::edits::AliasScope],
) -> bool {
    let mut current = Some(column_scope);
    let mut incoming_limit: Option<usize> = None;
    while let Some(scope) = current {
        if scope == relation_scope {
            return match incoming_limit {
                Some(limit) => relation_factor < limit,
                None => true,
            };
        }
        let Some(entry) = aliases.get(scope) else {
            return false;
        };
        current = entry.parent;
        incoming_limit = entry.parent_factor_limit;
    }
    false
}

/// Resolve a one-part qualifier through the lexical alias scopes starting at
/// `from`. Returns the scope that defined the alias and its target, if any.
/// Alias limits on derived-table scopes hide later or non-LATERAL siblings.
fn resolve_alias(
    qualifier: &str,
    from: usize,
    aliases: &[crate::edits::AliasScope],
) -> Option<(usize, Option<Vec<String>>)> {
    let mut current = Some(from);
    let mut incoming_limit: Option<usize> = None;
    while let Some(scope) = current {
        let entry = aliases.get(scope)?;
        let visible = incoming_limit
            .map(|limit| limit.min(entry.aliases.len()))
            .unwrap_or(entry.aliases.len());
        if let Some((_, target)) = entry.aliases[..visible]
            .iter()
            .rev()
            .find(|(name, _)| name == qualifier)
        {
            return Some((scope, target.clone()));
        }
        current = entry.parent;
        incoming_limit = entry.parent_alias_limit;
    }
    None
}

fn relation_columns(
    relation: &ir::NameRef,
    relation_scope: usize,
    relation_alias: Option<&str>,
    relation_factor: usize,
    columns: &[(Vec<String>, usize)],
    aliases: &[crate::edits::AliasScope],
) -> Vec<ir::Ident> {
    let relation_parts: Vec<&str> = relation.name.0.iter().map(|part| part.0.as_str()).collect();
    let mut result = Vec::new();
    let mut seen = HashSet::new();
    for (parts, column_scope) in columns {
        let Some((column, qualifier)) = parts.split_last() else {
            continue;
        };
        let matches = if qualifier.is_empty() {
            relation_visible(relation_scope, relation_factor, *column_scope, aliases)
        } else if qualifier.len() == 1 {
            match resolve_alias(&qualifier[0], *column_scope, aliases) {
                Some((alias_scope, Some(target))) => {
                    alias_scope == relation_scope
                        && target
                            .iter()
                            .map(String::as_str)
                            .eq(relation_parts.iter().copied())
                }
                Some((_, None)) => false,
                None => {
                    // An alias hides the original relation name in its scope.
                    relation_alias.is_none()
                        && relation_visible(relation_scope, relation_factor, *column_scope, aliases)
                        && relation_parts.last().copied() == Some(qualifier[0].as_str())
                }
            }
        } else {
            relation_alias.is_none()
                && relation_visible(relation_scope, relation_factor, *column_scope, aliases)
                && qualifier
                    .iter()
                    .map(String::as_str)
                    .eq(relation_parts.iter().copied())
        };
        if matches {
            let column = ir::Ident(column.clone());
            if seen.insert(column.clone()) {
                result.push(column);
            }
        }
    }
    result
}

fn visitor_relations(
    collector: &AnalysisVisitor<'_>,
    excluded: &[Span],
) -> Vec<(ir::NameRef, Span, usize, Option<String>, usize)> {
    let mut relations: Vec<(ir::NameRef, Span, usize, Option<String>, usize)> = Vec::new();
    let mut seen: HashSet<(ir::NameRef, usize, Option<String>, usize)> = HashSet::new();
    for relation in &collector.relations {
        if relation.is_cte
            || excluded.contains(&relation.span)
            || collector.unmodeled_relation_spans.contains(&relation.span)
        {
            continue;
        }
        if let Some(reference) = relation_ref(&relation.name, collector.dialect, collector.rules) {
            let key = (
                reference.clone(),
                relation.scope,
                relation.alias.clone(),
                relation.factor,
            );
            if seen.insert(key) {
                relations.push((
                    reference,
                    relation.span,
                    relation.scope,
                    relation.alias.clone(),
                    relation.factor,
                ));
            }
        }
    }
    relations
}

fn has_unmodeled_relation(collector: &AnalysisVisitor<'_>, excluded: &[Span]) -> bool {
    collector.unmodeled_table_source
        || collector
            .unmodeled_relation_spans
            .iter()
            .any(|span| !excluded.contains(span))
        || collector.relations.iter().any(|relation| {
            !relation.is_cte
                && !excluded.contains(&relation.span)
                && relation_ref(&relation.name, collector.dialect, collector.rules).is_none()
        })
}

fn add_unmodeled_barriers(
    facts: &mut ir::NativeFacts,
    collector: &AnalysisVisitor<'_>,
    excluded: &[Span],
) {
    if has_unmodeled_relation(collector, excluded) {
        facts.barriers.push(global_barrier(
            ir::BarrierReason::UnmodeledRelationSource,
            None,
        ));
    }
}

fn visitor_reads(
    collector: &AnalysisVisitor<'_>,
    excluded: &[Span],
    mode: ir::ReadMode,
) -> Vec<ir::ReadFact> {
    visitor_relations(collector, excluded)
        .into_iter()
        .map(|(reference, _, scope, alias, factor)| {
            let columns = relation_columns(
                &reference,
                scope,
                alias.as_deref(),
                factor,
                &collector.columns,
                &collector.aliases,
            );
            read_fact(reference, mode, columns)
        })
        .collect()
}

fn add_reads(facts: &mut ir::NativeFacts, reads: Vec<ir::ReadFact>) {
    facts.ops.extend(reads.into_iter().map(ir::CatalogOp::Read));
}

fn referential_action(action: Option<ReferentialAction>) -> ir::ReferentialAction {
    match action {
        Some(ReferentialAction::Cascade) => ir::ReferentialAction::Cascade,
        Some(ReferentialAction::SetNull) => ir::ReferentialAction::SetNull,
        Some(ReferentialAction::SetDefault) => ir::ReferentialAction::SetDefault,
        Some(ReferentialAction::Restrict) => ir::ReferentialAction::Restrict,
        Some(ReferentialAction::NoAction) | None => ir::ReferentialAction::NoAction,
    }
}

fn foreign_key(
    reference: &ForeignKeyConstraint,
    child_columns: Vec<ir::Ident>,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<ir::ForeignKey> {
    let parent = relation_ref(&reference.foreign_table, dialect, rules)?;
    let parent_columns = reference
        .referred_columns
        .iter()
        .map(|column| canonical_ident(column, rules, ir::IdentifierClass::Column))
        .collect();
    Some(ir::ForeignKey {
        name: reference
            .name
            .as_ref()
            .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
        child_columns,
        parent,
        parent_columns,
        on_delete: referential_action(reference.on_delete),
        on_update: referential_action(reference.on_update),
        not_valid: false,
        deferrable: reference
            .characteristics
            .as_ref()
            .and_then(|characteristics| characteristics.deferrable)
            .unwrap_or(false),
    })
}

fn index_column_ident(column: &IndexColumn, rules: &ir::IdentifierRules) -> Option<ir::Ident> {
    match &column.column.expr {
        Expr::Identifier(ident) => Some(canonical_ident(ident, rules, ir::IdentifierClass::Column)),
        _ => None,
    }
}

fn constraint_from_table(
    constraint: &TableConstraint,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<ir::Constraint> {
    match constraint {
        TableConstraint::PrimaryKey(primary) => Some(ir::Constraint::PrimaryKey {
            name: primary
                .name
                .as_ref()
                .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
            columns: primary
                .columns
                .iter()
                .filter_map(|column| index_column_ident(column, rules))
                .collect(),
        }),
        TableConstraint::Unique(unique) => Some(ir::Constraint::Unique {
            name: unique
                .name
                .as_ref()
                .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
            columns: unique
                .columns
                .iter()
                .filter_map(|column| index_column_ident(column, rules))
                .collect(),
        }),
        TableConstraint::ForeignKey(reference) => {
            let child_columns = reference
                .columns
                .iter()
                .map(|column| canonical_ident(column, rules, ir::IdentifierClass::Column))
                .collect();
            foreign_key(reference, child_columns, dialect, rules).map(ir::Constraint::ForeignKey)
        }
        TableConstraint::Check(check) => Some(ir::Constraint::Check {
            name: check
                .name
                .as_ref()
                .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
            columns: expr_columns(&check.expr, rules),
            not_valid: false,
        }),
        TableConstraint::Index(index) => Some(ir::Constraint::Index {
            name: index
                .name
                .as_ref()
                .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
            columns: index
                .columns
                .iter()
                .filter_map(|column| index_column_ident(column, rules))
                .collect(),
            unique: false,
        }),
        TableConstraint::FulltextOrSpatial(fulltext) => Some(ir::Constraint::Other {
            name: fulltext
                .opt_index_name
                .as_ref()
                .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
        }),
        TableConstraint::Exclude(exclude) => Some(ir::Constraint::Other {
            name: exclude
                .name
                .as_ref()
                .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
        }),
        TableConstraint::PrimaryKeyUsingIndex(using) => Some(ir::Constraint::UsingIndex {
            name: using
                .name
                .as_ref()
                .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
            index: canonical_ident(&using.index_name, rules, ir::IdentifierClass::Other),
            primary: true,
            schema_placement: index_schema_placement(dialect),
        }),
        TableConstraint::UniqueUsingIndex(using) => Some(ir::Constraint::UsingIndex {
            name: using
                .name
                .as_ref()
                .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
            index: canonical_ident(&using.index_name, rules, ir::IdentifierClass::Other),
            primary: false,
            schema_placement: index_schema_placement(dialect),
        }),
    }
}

fn column_definition(
    column: &ColumnDef,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> (ir::ColumnDef, Vec<ir::Constraint>) {
    let name = canonical_ident(&column.name, rules, ir::IdentifierClass::Column);
    let mut not_null = false;
    let mut has_default = false;
    let mut generated = false;
    let mut references = Vec::new();
    let mut constraints = Vec::new();
    for option in &column.options {
        match &option.option {
            ColumnOption::NotNull => not_null = true,
            ColumnOption::Default(_) => has_default = true,
            ColumnOption::Materialized(_)
            | ColumnOption::Alias(_)
            | ColumnOption::Generated { .. }
            | ColumnOption::Identity(_) => {
                generated = true;
                has_default = true;
            }
            // ClickHouse EPHEMERAL columns are user-supplied helper columns:
            // their values are not stored, but they are not generated by the
            // system and may be written explicitly in INSERT.
            ColumnOption::Ephemeral(default) => {
                has_default = default.is_some();
            }
            ColumnOption::PrimaryKey(primary) => {
                not_null = true;
                constraints.push(ir::Constraint::PrimaryKey {
                    name: primary
                        .name
                        .as_ref()
                        .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
                    columns: vec![name.clone()],
                });
            }
            ColumnOption::Unique(unique) => constraints.push(ir::Constraint::Unique {
                name: unique
                    .name
                    .as_ref()
                    .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
                columns: vec![name.clone()],
            }),
            ColumnOption::ForeignKey(reference) => {
                if let Some(reference) = foreign_key(reference, vec![name.clone()], dialect, rules)
                {
                    references.push(reference);
                }
            }
            ColumnOption::Check(check) => constraints.push(ir::Constraint::Check {
                name: check
                    .name
                    .as_ref()
                    .map(|name| canonical_ident(name, rules, ir::IdentifierClass::Other)),
                columns: expr_columns(&check.expr, rules),
                not_valid: false,
            }),
            _ => {}
        }
    }
    (
        ir::ColumnDef {
            name,
            not_null,
            has_default,
            generated,
            references,
        },
        constraints,
    )
}

fn table_definition(
    create: &CreateTable,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
    sources: Vec<ir::ReadFact>,
    like: Option<ir::NameRef>,
) -> Option<ir::TableDef> {
    let (name, temporary) = definition_name(&create.name, dialect, rules, create.temporary)?;
    let mut columns = Vec::new();
    let mut constraints = Vec::new();
    for column in &create.columns {
        let (definition, column_constraints) = column_definition(column, dialect, rules);
        columns.push(definition);
        constraints.extend(column_constraints);
    }
    for constraint in &create.constraints {
        if let Some(constraint) = constraint_from_table(constraint, dialect, rules) {
            constraints.push(constraint);
        }
    }
    // `CREATE TABLE AS SELECT` does not spell out columns in the table
    // definition; its shape comes from the query projection.
    if columns.is_empty()
        && let Some(query) = &create.query
        && let Some(output) = query_output_columns(query, rules)
    {
        columns.extend(output.into_iter().map(|name| ir::ColumnDef {
            name,
            not_null: false,
            has_default: false,
            generated: false,
            references: Vec::new(),
        }));
    }
    Some(ir::TableDef {
        name,
        temporary,
        transactional: table_transactional(dialect, temporary),
        columns,
        constraints,
        sources,
        like,
        on_commit: create
            .on_commit
            .as_ref()
            .and_then(|on_commit| match on_commit {
                OnCommit::DeleteRows => Some(ir::OnCommitAction::DeleteRows),
                OnCommit::Drop => Some(ir::OnCommitAction::Drop),
                OnCommit::PreserveRows => None,
            }),
        if_not_exists: create.if_not_exists,
        or_replace: create.or_replace,
    })
}

fn table_transactional(dialect: DialectKind, temporary: bool) -> bool {
    if dialect == DialectKind::MySql {
        !temporary
    } else {
        !matches!(
            dialect,
            DialectKind::Oracle
                | DialectKind::ClickHouse
                | DialectKind::Hive
                | DialectKind::Spark
                | DialectKind::Databricks
                | DialectKind::Snowflake
        )
    }
}

fn sequence_transactional(dialect: DialectKind) -> bool {
    !matches!(
        dialect,
        DialectKind::Oracle
            | DialectKind::MySql
            | DialectKind::ClickHouse
            | DialectKind::Hive
            | DialectKind::Spark
            | DialectKind::Databricks
            | DialectKind::Snowflake
    )
}

fn index_ref(name: &ir::Name, table: Option<&ir::NameRef>, dialect: DialectKind) -> ir::IndexRef {
    if matches!(dialect, DialectKind::MySql | DialectKind::MsSql)
        && let (Some(table), Some(last)) = (table, last_ident(name))
    {
        return ir::IndexRef::Table {
            table: table.clone(),
            name: last,
        };
    }
    ir::IndexRef::Named { name: name.clone() }
}

fn index_schema_placement(dialect: DialectKind) -> ir::IndexSchemaPlacement {
    match dialect {
        // Oracle creates an index in the schema of its owner, even when the
        // indexed table is in another schema.
        DialectKind::Oracle => ir::IndexSchemaPlacement::CurrentSchema,
        _ => ir::IndexSchemaPlacement::TableSchema,
    }
}

fn trigger_ref(name: &ir::Name, table: &ir::NameRef, dialect: DialectKind) -> ir::TriggerRef {
    if dialect == DialectKind::PostgreSql
        && let Some(last) = last_ident(name)
    {
        return ir::TriggerRef::Table {
            table: table.clone(),
            name: last,
        };
    }
    ir::TriggerRef::Named { name: name.clone() }
}

fn table_object_name(table: &TableObject) -> Option<&ObjectName> {
    match table {
        TableObject::TableName(name) => Some(name),
        TableObject::TableFunction(_) | TableObject::TableQuery(_) => None,
    }
}

fn table_factor_name(factor: &TableFactor) -> Option<&ObjectName> {
    match factor {
        TableFactor::Table {
            name, args: None, ..
        } => Some(name),
        _ => None,
    }
}

fn first_from_table(from: &FromTable) -> Option<&TableWithJoins> {
    let tables = match from {
        FromTable::WithFromKeyword(tables) | FromTable::WithoutKeyword(tables) => tables,
    };
    tables.first()
}

fn assignment_columns(target: &AssignmentTarget, rules: &ir::IdentifierRules) -> Vec<ir::Ident> {
    match target {
        AssignmentTarget::ColumnName(column) => {
            name_from_object(column, rules, ir::IdentifierClass::Column)
                .and_then(|name| last_ident(&name))
                .into_iter()
                .collect()
        }
        AssignmentTarget::Tuple(columns) => columns
            .iter()
            .filter_map(|column| {
                name_from_object(column, rules, ir::IdentifierClass::Column)
                    .and_then(|name| last_ident(&name))
            })
            .collect(),
    }
}

fn insert_conflict(insert: &Insert) -> ir::ConflictAction {
    if insert.replace_into || insert.overwrite {
        return ir::ConflictAction::Replace;
    }
    if insert.ignore {
        return ir::ConflictAction::Ignore;
    }
    if let Some(on_conflict) = &insert.or {
        return match on_conflict {
            SqliteOnConflict::Replace => ir::ConflictAction::Replace,
            SqliteOnConflict::Ignore => ir::ConflictAction::Ignore,
            SqliteOnConflict::Rollback | SqliteOnConflict::Abort | SqliteOnConflict::Fail => {
                ir::ConflictAction::Error
            }
        };
    }
    match &insert.on {
        Some(OnInsert::DuplicateKeyUpdate(_)) => ir::ConflictAction::Update,
        Some(OnInsert::OnConflict(conflict)) => match &conflict.action {
            OnConflictAction::DoNothing => ir::ConflictAction::Ignore,
            OnConflictAction::DoUpdate(_) => ir::ConflictAction::Update,
        },
        None | Some(_) => ir::ConflictAction::Error,
    }
}

fn insert_columns(insert: &Insert, rules: &ir::IdentifierRules) -> Vec<ir::Ident> {
    let mut columns: Vec<ir::Ident> = insert
        .columns
        .iter()
        .filter_map(|column| {
            name_from_object(column, rules, ir::IdentifierClass::Column)
                .and_then(|name| last_ident(&name))
        })
        .collect();
    let mut seen: HashSet<ir::Ident> = columns.iter().cloned().collect();
    let mut push = |column: ir::Ident, columns: &mut Vec<ir::Ident>| {
        if seen.insert(column.clone()) {
            columns.push(column);
        }
    };
    for assignment in &insert.assignments {
        for column in assignment_columns(&assignment.target, rules) {
            push(column, &mut columns);
        }
    }
    match &insert.on {
        Some(OnInsert::DuplicateKeyUpdate(assignments)) => {
            for assignment in assignments {
                for column in assignment_columns(&assignment.target, rules) {
                    push(column, &mut columns);
                }
            }
        }
        Some(OnInsert::OnConflict(conflict)) => {
            if let OnConflictAction::DoUpdate(update) = &conflict.action {
                for assignment in &update.assignments {
                    for column in assignment_columns(&assignment.target, rules) {
                        push(column, &mut columns);
                    }
                }
            }
        }
        None | Some(_) => {}
    }
    columns
}

/// One physical table factor that can be named by a DML statement.
#[derive(Clone, Debug)]
struct TableSource {
    name: ObjectName,
    alias: Option<String>,
    table_parts: Vec<String>,
}

impl TableSource {
    fn matches_qualifier(&self, qualifier: &[String]) -> bool {
        match &self.alias {
            Some(alias) => qualifier.len() == 1 && qualifier[0] == *alias,
            None => {
                qualifier == self.table_parts.as_slice()
                    || (qualifier.len() == 1 && self.table_parts.last() == Some(&qualifier[0]))
            }
        }
    }
}

#[derive(Clone, Debug)]
struct UpdateTarget {
    source: TableSource,
    columns: Vec<ir::Ident>,
    unqualified_columns: Vec<ir::Ident>,
    /// The alias designator in `UPDATE alias SET ... FROM table AS alias`.
    /// It is not a physical relation and must not become a read.
    designator_span: Option<Span>,
}

#[derive(Clone, Debug)]
struct DeleteTarget {
    source: TableSource,
    /// The designator in `DELETE alias FROM table AS alias`. It is excluded
    /// from reads; the corresponding `FROM` factor remains readable.
    designator_span: Option<Span>,
}

fn assignment_column_target(
    name: &ObjectName,
    rules: &ir::IdentifierRules,
) -> Option<(Option<Vec<String>>, ir::Ident)> {
    let mut idents = Vec::new();
    for part in &name.0 {
        match part {
            ObjectNamePart::Identifier(ident) => idents.push(ident),
            ObjectNamePart::Function(_) => return None,
        }
    }
    let (column, qualifier) = idents.split_last()?;
    let qualifier: Vec<String> = qualifier
        .iter()
        .map(|ident| canonical_ident_string(ident, rules, ir::IdentifierClass::Relation))
        .collect();
    Some((
        (!qualifier.is_empty()).then_some(qualifier),
        canonical_ident(column, rules, ir::IdentifierClass::Column),
    ))
}

fn assignment_targets(
    update: &Update,
    rules: &ir::IdentifierRules,
) -> Vec<(Option<Vec<String>>, ir::Ident)> {
    let mut targets = Vec::new();
    for assignment in &update.assignments {
        match &assignment.target {
            AssignmentTarget::ColumnName(name) => {
                if let Some(target) = assignment_column_target(name, rules) {
                    targets.push(target);
                }
            }
            AssignmentTarget::Tuple(names) => {
                for name in names {
                    if let Some(target) = assignment_column_target(name, rules) {
                        targets.push(target);
                    }
                }
            }
        }
    }
    targets
}

fn table_factor_source(factor: &TableFactor, rules: &ir::IdentifierRules) -> Option<TableSource> {
    let name = table_factor_name(factor)?;
    let parts = name_parts(name, rules, ir::IdentifierClass::Relation)?;
    let alias = match factor {
        TableFactor::Table { alias, .. } => alias
            .as_ref()
            .map(|alias| canonical_ident_string(&alias.name, rules, ir::IdentifierClass::Relation)),
        _ => None,
    };
    Some(TableSource {
        name: name.clone(),
        alias,
        table_parts: parts.into_iter().map(|part| part.0).collect(),
    })
}

fn table_with_joins_sources(
    table: &TableWithJoins,
    rules: &ir::IdentifierRules,
) -> Vec<TableSource> {
    let mut sources = Vec::new();
    if let Some(source) = table_factor_source(&table.relation, rules) {
        sources.push(source);
    }
    for join in &table.joins {
        if let Some(source) = table_factor_source(&join.relation, rules) {
            sources.push(source);
        }
    }
    sources
}

fn from_sources(from: &FromTable, rules: &ir::IdentifierRules) -> Vec<TableSource> {
    let tables = match from {
        FromTable::WithFromKeyword(tables) | FromTable::WithoutKeyword(tables) => tables,
    };
    tables
        .iter()
        .flat_map(|table| table_with_joins_sources(table, rules))
        .collect()
}

fn source_from_name(name: &ObjectName, rules: &ir::IdentifierRules) -> Option<TableSource> {
    let parts = name_parts(name, rules, ir::IdentifierClass::Relation)?;
    Some(TableSource {
        name: name.clone(),
        alias: None,
        table_parts: parts.into_iter().map(|part| part.0).collect(),
    })
}

fn find_source<'a>(
    name: &ObjectName,
    sources: &'a [TableSource],
    rules: &ir::IdentifierRules,
) -> Option<&'a TableSource> {
    let parts = name_parts(name, rules, ir::IdentifierClass::Relation)?;
    if parts.len() == 1 {
        let name = &parts[0].0;
        if let Some(source) = sources
            .iter()
            .find(|source| source.alias.as_deref() == Some(name.as_str()))
        {
            return Some(source);
        }
        if let Some(source) = sources
            .iter()
            .find(|source| source.alias.is_none() && source.table_parts.last() == Some(name))
        {
            return Some(source);
        }
    }
    let parts: Vec<&str> = parts.iter().map(|part| part.0.as_str()).collect();
    sources.iter().find(|source| {
        source.alias.is_none()
            && source
                .table_parts
                .iter()
                .map(String::as_str)
                .eq(parts.iter().copied())
    })
}

/// Tables assigned by an `UPDATE`. MySQL multi-table updates can write through
/// joins, so the primary table alone is not enough. Tables that are only read
/// through the join remain read sources and are not returned here. MSSQL's
/// `UPDATE alias SET ... FROM table AS alias` form resolves the alias to the
/// table in `FROM`; the alias itself is not a physical write target.
fn update_targets(update: &Update, rules: &ir::IdentifierRules) -> Vec<UpdateTarget> {
    let from: Vec<TableSource> = update
        .from
        .as_ref()
        .map(|from| match from {
            UpdateTableFromKind::BeforeSet(tables) | UpdateTableFromKind::AfterSet(tables) => {
                tables
                    .iter()
                    .flat_map(|table| table_with_joins_sources(table, rules))
                    .collect()
            }
        })
        .unwrap_or_default();
    let primary = table_factor_source(&update.table.relation, rules);
    let mut targets: Vec<UpdateTarget> = Vec::new();
    let mut resolved_alias = false;
    if let Some(primary) = &primary
        && primary.alias.is_none()
        && primary.table_parts.len() == 1
        && let Some(source) = find_source(&primary.name, &from, rules)
    {
        targets.push(UpdateTarget {
            source: source.clone(),
            columns: Vec::new(),
            unqualified_columns: Vec::new(),
            designator_span: Some(primary.name.span()),
        });
        resolved_alias = true;
    }
    if !resolved_alias {
        if let Some(source) = primary {
            targets.push(UpdateTarget {
                source,
                columns: Vec::new(),
                unqualified_columns: Vec::new(),
                designator_span: None,
            });
        }
        for join in &update.table.joins {
            if let Some(source) = table_factor_source(&join.relation, rules) {
                targets.push(UpdateTarget {
                    source,
                    columns: Vec::new(),
                    unqualified_columns: Vec::new(),
                    designator_span: None,
                });
            }
        }
    }

    let assignments = assignment_targets(update, rules);
    for target in &mut targets {
        for (qualifier, column) in &assignments {
            match qualifier {
                None => {
                    if !target.unqualified_columns.contains(column) {
                        target.unqualified_columns.push(column.clone());
                    }
                }
                Some(qualifier) => {
                    if target.source.matches_qualifier(qualifier)
                        && !target.columns.contains(column)
                    {
                        target.columns.push(column.clone());
                    }
                }
            }
        }
    }
    targets.retain(|target| !target.columns.is_empty() || !target.unqualified_columns.is_empty());
    targets
}

fn delete_targets(delete: &Delete, rules: &ir::IdentifierRules) -> Vec<DeleteTarget> {
    let from = from_sources(&delete.from, rules);
    if delete.tables.is_empty() {
        return first_from_table(&delete.from)
            .and_then(|table| table_factor_source(&table.relation, rules))
            .map(|source| DeleteTarget {
                source,
                designator_span: None,
            })
            .into_iter()
            .collect();
    }
    delete
        .tables
        .iter()
        .filter_map(|name| {
            let source = find_source(name, &from, rules)
                .cloned()
                .or_else(|| source_from_name(name, rules))?;
            Some(DeleteTarget {
                source,
                designator_span: Some(name.span()),
            })
        })
        .collect()
}

fn merge_columns(merge: &Merge, rules: &ir::IdentifierRules) -> Vec<ir::Ident> {
    let mut columns = Vec::new();
    let mut seen = HashSet::new();
    for clause in &merge.clauses {
        match &clause.action {
            MergeAction::Update(update) => {
                if let MergeUpdateKind::Set(assignments) = &update.kind {
                    for assignment in assignments {
                        for column in assignment_columns(&assignment.target, rules) {
                            if seen.insert(column.clone()) {
                                columns.push(column);
                            }
                        }
                    }
                }
            }
            MergeAction::Insert(insert) => {
                for column in &insert.columns {
                    if let Some(column) =
                        name_from_object(column, rules, ir::IdentifierClass::Column)
                            .and_then(|name| last_ident(&name))
                        && seen.insert(column.clone())
                    {
                        columns.push(column);
                    }
                }
            }
            _ => {}
        }
    }
    columns
}

#[derive(Default)]
struct NestedDml {
    statements: Vec<Statement>,
    skip_root: bool,
    dml_depth: usize,
}

pub(super) fn is_dml_statement(statement: &Statement) -> bool {
    matches!(
        statement,
        Statement::Insert(_) | Statement::Update(_) | Statement::Delete(_) | Statement::Merge(_)
    )
}

impl Visitor for NestedDml {
    type Break = ();

    fn pre_visit_statement(&mut self, statement: &Statement) -> std::ops::ControlFlow<Self::Break> {
        if self.skip_root {
            self.skip_root = false;
            return std::ops::ControlFlow::Continue(());
        }
        if is_dml_statement(statement) {
            // Collect only the outermost DML nodes. Recursive analysis of each
            // collected statement discovers its own nested DML, so descending
            // here would analyze deeper statements once per ancestor.
            if self.dml_depth == 0 {
                self.statements.push(statement.clone());
            }
            self.dml_depth += 1;
        }
        std::ops::ControlFlow::Continue(())
    }

    fn post_visit_statement(
        &mut self,
        statement: &Statement,
    ) -> std::ops::ControlFlow<Self::Break> {
        if is_dml_statement(statement) {
            self.dml_depth = self.dml_depth.saturating_sub(1);
        }
        std::ops::ControlFlow::Continue(())
    }
}

fn collect_dml_in_query(query: &Query) -> Vec<Statement> {
    let mut collector = NestedDml::default();
    let _ = query.visit(&mut collector);
    collector.statements
}

fn collect_dml_in_statement(statement: &Statement) -> Vec<Statement> {
    let mut collector = NestedDml {
        skip_root: true,
        ..NestedDml::default()
    };
    let _ = statement.visit(&mut collector);
    collector.statements
}

fn merge_nested(
    facts: &mut ir::NativeFacts,
    statements: Vec<Statement>,
    context: AnalysisContext<'_>,
) {
    for statement in statements {
        let nested = analyze_statement(&statement, context);
        facts.ops.extend(nested.ops);
        facts.session.extend(nested.session);
        facts.pre_tx.extend(nested.pre_tx);
        facts.post_tx.extend(nested.post_tx);
        facts.barriers.extend(nested.barriers);
    }
}

fn trigger_event(event: &TriggerEvent, rules: &ir::IdentifierRules) -> ir::TriggerEvent {
    match event {
        TriggerEvent::Insert => ir::TriggerEvent::Insert,
        TriggerEvent::Update(columns) => ir::TriggerEvent::Update {
            columns: columns
                .iter()
                .map(|column| canonical_ident(column, rules, ir::IdentifierClass::Column))
                .collect(),
        },
        TriggerEvent::Delete => ir::TriggerEvent::Delete,
        TriggerEvent::Truncate => ir::TriggerEvent::Truncate,
    }
}

fn trigger_timing(period: Option<TriggerPeriod>) -> ir::TriggerTiming {
    match period {
        Some(TriggerPeriod::After) => ir::TriggerTiming::After,
        Some(TriggerPeriod::InsteadOf) => ir::TriggerTiming::InsteadOf,
        Some(TriggerPeriod::Before) | Some(TriggerPeriod::For) | None => ir::TriggerTiming::Before,
    }
}

fn trigger_for_each_row(trigger: &CreateTrigger, dialect: DialectKind) -> bool {
    match trigger.trigger_object {
        Some(TriggerObjectKind::For(TriggerObject::Row))
        | Some(TriggerObjectKind::ForEach(TriggerObject::Row)) => true,
        Some(TriggerObjectKind::For(TriggerObject::Statement))
        | Some(TriggerObjectKind::ForEach(TriggerObject::Statement)) => false,
        None => matches!(dialect, DialectKind::MySql | DialectKind::SQLite),
    }
}

fn trigger_effects_from_facts(facts: ir::NativeFacts, effects: &mut Vec<ir::TriggerEffect>) {
    for op in facts.ops {
        match op {
            ir::CatalogOp::Read(read) => effects.push(ir::TriggerEffect::Read { read }),
            ir::CatalogOp::WriteData(write) => {
                for read in &write.reads {
                    effects.push(ir::TriggerEffect::Read { read: read.clone() });
                }
                let event = match write.kind {
                    ir::WriteKind::Insert { .. } => ir::DmlEvent::Insert {
                        target: write.target,
                        columns: write.columns,
                    },
                    ir::WriteKind::Update => ir::DmlEvent::Update {
                        target: write.target,
                        columns: write.columns,
                    },
                    ir::WriteKind::Merge => ir::DmlEvent::Merge {
                        target: write.target,
                        columns: write.columns,
                    },
                    ir::WriteKind::Delete { all } => ir::DmlEvent::Delete {
                        target: write.target,
                        all,
                    },
                    ir::WriteKind::Truncate { cascade, .. } => ir::DmlEvent::Truncate {
                        target: write.target,
                        cascade,
                    },
                };
                effects.push(ir::TriggerEffect::Dml { event });
            }
            ir::CatalogOp::WriteSequence(write) => {
                effects.push(ir::TriggerEffect::WriteSequence { write });
            }
            ir::CatalogOp::DefineTable(_)
            | ir::CatalogOp::DefineView(_)
            | ir::CatalogOp::DefineIndex(_)
            | ir::CatalogOp::DefineTrigger(_)
            | ir::CatalogOp::DefineSequence(_)
            | ir::CatalogOp::DefineType(_)
            | ir::CatalogOp::DefineSchema(_)
            | ir::CatalogOp::AlterTable(_)
            | ir::CatalogOp::Drop(_)
            | ir::CatalogOp::Rename(_)
            | ir::CatalogOp::Refresh(_) => effects.push(ir::TriggerEffect::Barrier {
                barrier: global_barrier(
                    ir::BarrierReason::UnsupportedTriggerBody,
                    Some("DDL in trigger body".to_string()),
                ),
            }),
        }
    }
    for effect in facts.session {
        effects.push(ir::TriggerEffect::Session { effect });
    }
    for barrier in facts.barriers {
        effects.push(ir::TriggerEffect::Barrier { barrier });
    }
    if !facts.pre_tx.is_empty() || !facts.post_tx.is_empty() {
        effects.push(ir::TriggerEffect::Barrier {
            barrier: global_barrier(
                ir::BarrierReason::UnsupportedTriggerBody,
                Some("transaction control in trigger body".to_string()),
            ),
        });
    }
}

fn trigger_statement_effects(
    statement: &Statement,
    context: AnalysisContext<'_>,
    effects: &mut Vec<ir::TriggerEffect>,
) {
    match statement {
        Statement::If(if_statement) => {
            trigger_block_effects(&if_statement.if_block, context, effects);
            for block in &if_statement.elseif_blocks {
                trigger_block_effects(block, context, effects);
            }
            if let Some(block) = &if_statement.else_block {
                trigger_block_effects(block, context, effects);
            }
        }
        Statement::While(while_statement) => {
            trigger_block_effects(&while_statement.while_block, context, effects)
        }
        Statement::Case(case_statement) => {
            for block in &case_statement.when_blocks {
                trigger_block_effects(block, context, effects);
            }
            if let Some(block) = &case_statement.else_block {
                trigger_block_effects(block, context, effects);
            }
        }
        Statement::StartTransaction { statements, .. } if !statements.is_empty() => {
            for statement in statements {
                trigger_statement_effects(statement, context, effects);
            }
        }
        _ => {
            let facts = analyze_statement(statement, context);
            trigger_effects_from_facts(facts, effects);
        }
    }
}

fn trigger_block_effects(
    block: &ConditionalStatementBlock,
    context: AnalysisContext<'_>,
    effects: &mut Vec<ir::TriggerEffect>,
) {
    for statement in block.statements() {
        trigger_statement_effects(statement, context, effects);
    }
}

fn trigger_when_reads(condition: Option<&Expr>, context: AnalysisContext<'_>) -> Vec<ir::ReadFact> {
    let Some(condition) = condition else {
        return Vec::new();
    };
    let mut visitor = AnalysisVisitor::new(context);
    let _ = condition.visit(&mut visitor);
    visitor_reads(&visitor, &[], ir::ReadMode::Data)
}

fn trigger_body(trigger: &CreateTrigger, context: AnalysisContext<'_>) -> ir::TriggerBody {
    if let Some(exec_body) = &trigger.exec_body {
        let name = name_from_object(
            &exec_body.func_desc.name,
            context.rules,
            ir::IdentifierClass::Other,
        );
        let args = exec_body
            .func_desc
            .args
            .as_ref()
            .map(|args| args.iter().map(ToString::to_string).collect())
            .unwrap_or_default();
        return match name {
            Some(name) => ir::TriggerBody::Function { name, args },
            None => ir::TriggerBody::Opaque,
        };
    }
    if let Some(statements) = &trigger.statements {
        let mut effects = Vec::new();
        for statement in statements.statements() {
            trigger_statement_effects(statement, context, &mut effects);
        }
        return ir::TriggerBody::Inline { effects };
    }
    ir::TriggerBody::Opaque
}

fn context_scope(scope: Option<ContextModifier>) -> ir::SessionScope {
    match scope {
        Some(ContextModifier::Local) => ir::SessionScope::Local,
        Some(ContextModifier::Global) => ir::SessionScope::Global,
        Some(ContextModifier::Session) | None => ir::SessionScope::Session,
    }
}

fn pragma_bool(value: &ValueWithSpan) -> Option<bool> {
    match &value.value {
        Value::Boolean(value) => Some(*value),
        Value::Number(value, _) => value.parse::<i64>().ok().map(|value| value != 0),
        Value::SingleQuotedString(value)
        | Value::DoubleQuotedString(value)
        | Value::TripleSingleQuotedString(value)
        | Value::TripleDoubleQuotedString(value) => match value.to_ascii_lowercase().as_str() {
            "on" | "true" | "1" => Some(true),
            "off" | "false" | "0" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

fn expr_bool(expr: &Expr) -> Option<bool> {
    match expr {
        Expr::Value(value) => match &value.value {
            Value::Boolean(value) => Some(*value),
            Value::Number(value, _) => value.parse::<i64>().ok().map(|value| value != 0),
            Value::SingleQuotedString(value)
            | Value::DoubleQuotedString(value)
            | Value::TripleSingleQuotedString(value)
            | Value::TripleDoubleQuotedString(value) => match value.to_ascii_lowercase().as_str() {
                "on" | "true" | "1" => Some(true),
                "off" | "false" | "0" => Some(false),
                _ => None,
            },
            _ => None,
        },
        Expr::Identifier(ident) => match ident.value.to_ascii_lowercase().as_str() {
            "on" | "true" => Some(true),
            "off" | "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

fn expr_value(expr: &Expr) -> ir::SessionValue {
    match expr {
        Expr::Value(value) => match &value.value {
            Value::Boolean(value) => ir::SessionValue::Bool { value: *value },
            Value::Number(value, _) => match value.parse::<i64>() {
                Ok(value) => ir::SessionValue::Integer { value },
                Err(_) => ir::SessionValue::Text {
                    value: value.clone(),
                },
            },
            Value::SingleQuotedString(value)
            | Value::DoubleQuotedString(value)
            | Value::TripleSingleQuotedString(value)
            | Value::TripleDoubleQuotedString(value) => ir::SessionValue::Text {
                value: value.clone(),
            },
            Value::Null => ir::SessionValue::Text {
                value: "NULL".to_string(),
            },
            _ => ir::SessionValue::Text {
                value: value.to_string(),
            },
        },
        Expr::Identifier(ident) => {
            let lower = ident.value.to_ascii_lowercase();
            match lower.as_str() {
                "default" => ir::SessionValue::Default,
                "on" | "true" => ir::SessionValue::Bool { value: true },
                "off" | "false" => ir::SessionValue::Bool { value: false },
                _ => ir::SessionValue::Text {
                    value: ident.value.clone(),
                },
            }
        }
        _ => ir::SessionValue::Text {
            value: expr.to_string(),
        },
    }
}

fn expr_name(expr: &Expr, rules: &ir::IdentifierRules) -> Option<ir::Name> {
    match expr {
        Expr::Identifier(ident) => Some(ir::Name(vec![canonical_ident(
            ident,
            rules,
            ir::IdentifierClass::Relation,
        )])),
        Expr::CompoundIdentifier(idents) => Some(ir::Name(
            idents
                .iter()
                .map(|ident| canonical_ident(ident, rules, ir::IdentifierClass::Relation))
                .collect(),
        )),
        _ => None,
    }
}

fn search_path_name(value: &str, quoted: bool, rules: &ir::IdentifierRules) -> ir::Name {
    let ident = if quoted {
        Ident::with_quote('"', value)
    } else {
        Ident::new(value)
    };
    ir::Name(vec![canonical_ident(
        &ident,
        rules,
        ir::IdentifierClass::Relation,
    )])
}

/// Parse a PostgreSQL configuration-string search path such as
/// `'tenant, "MixedCase"'`. Commas inside double-quoted identifiers do not
/// split entries. Unquoted names follow the relation identifier folding rules.
fn literal_search_path_entries(
    value: &str,
    rules: &ir::IdentifierRules,
) -> Option<Vec<ir::SearchPathEntry>> {
    let mut entries = Vec::new();
    for part in split_identifier_list(value, ',')? {
        // An empty list item denotes no schema and is ignored.
        if part.trim().is_empty() {
            continue;
        }
        let (name, quoted) = literal_identifier_part(part)?;
        if name == "$user" {
            entries.push(ir::SearchPathEntry::CurrentUser);
        } else {
            entries.push(ir::SearchPathEntry::Name {
                name: search_path_name(&name, quoted, rules),
            });
        }
    }
    Some(entries)
}

/// A `search_path` list item. `$user` is a dynamic reference to the current
/// user's schema and must stay symbolic: the current user can change after the
/// `SET search_path`.
fn search_path_entries(
    expr: &Expr,
    rules: &ir::IdentifierRules,
) -> Option<Vec<ir::SearchPathEntry>> {
    match expr {
        Expr::Identifier(ident) if ident.value.is_empty() && ident.quote_style.is_some() => None,
        Expr::Identifier(ident) if ident.value == "$user" => {
            Some(vec![ir::SearchPathEntry::CurrentUser])
        }
        Expr::Identifier(_) | Expr::CompoundIdentifier(_) => {
            expr_name(expr, rules).map(|name| vec![ir::SearchPathEntry::Name { name }])
        }
        Expr::Value(value) => match &value.value {
            Value::Placeholder(placeholder) if placeholder == "$user" => {
                Some(vec![ir::SearchPathEntry::CurrentUser])
            }
            Value::DoubleQuotedString(name) | Value::TripleDoubleQuotedString(name) => {
                if name.is_empty() {
                    Some(Vec::new())
                } else if name == "$user" {
                    Some(vec![ir::SearchPathEntry::CurrentUser])
                } else {
                    Some(vec![ir::SearchPathEntry::Name {
                        name: search_path_name(name, true, rules),
                    }])
                }
            }
            Value::SingleQuotedString(name) | Value::TripleSingleQuotedString(name) => {
                literal_search_path_entries(name, rules)
            }
            _ => None,
        },
        _ => None,
    }
}

fn search_path_value(values: &[Expr], rules: &ir::IdentifierRules) -> Option<ir::SearchPathValue> {
    if let [Expr::Identifier(ident)] = values
        && ident.quote_style.is_none()
        && ident.value.eq_ignore_ascii_case("default")
    {
        return Some(ir::SearchPathValue::Default);
    }
    let mut entries = Vec::new();
    for value in values {
        entries.extend(search_path_entries(value, rules)?);
    }
    Some(ir::SearchPathValue::Explicit { entries })
}

fn set_effects(
    variable: &ObjectName,
    values: &[Expr],
    scope: ir::SessionScope,
    rules: &ir::IdentifierRules,
) -> (Vec<ir::SessionEffect>, Vec<ir::ScopedBarrier>) {
    let Some(name) = name_from_object(variable, rules, ir::IdentifierClass::Other) else {
        return (
            Vec::new(),
            vec![session_barrier(
                ir::BarrierReason::UnknownSessionEffect,
                Some(variable.to_string()),
            )],
        );
    };
    let last = name
        .last()
        .map(|ident| ident.0.to_ascii_lowercase())
        .unwrap_or_default();
    match last.as_str() {
        "search_path" => match search_path_value(values, rules) {
            Some(path) => (
                vec![ir::SessionEffect::SetSearchPath { scope, path }],
                Vec::new(),
            ),
            None => (
                Vec::new(),
                vec![session_barrier(
                    ir::BarrierReason::UnknownSessionEffect,
                    Some("SET search_path with a non-name value".to_string()),
                )],
            ),
        },
        "foreign_keys" | "foreign_key_checks" => {
            let Some(value) = values.first().and_then(expr_bool) else {
                return (
                    Vec::new(),
                    vec![session_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some("SET foreign_keys with a non-boolean value".to_string()),
                    )],
                );
            };
            (
                vec![ir::SessionEffect::SetForeignKeyChecks { scope, value }],
                Vec::new(),
            )
        }
        "autocommit" => {
            let Some(value) = values.first().and_then(expr_bool) else {
                return (
                    Vec::new(),
                    vec![session_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some("SET autocommit with a non-boolean value".to_string()),
                    )],
                );
            };
            (
                vec![ir::SessionEffect::SetAutocommit { scope, value }],
                Vec::new(),
            )
        }
        _ => {
            let value = values
                .first()
                .map(expr_value)
                .unwrap_or(ir::SessionValue::Default);
            (
                vec![ir::SessionEffect::SetVariable { scope, name, value }],
                Vec::new(),
            )
        }
    }
}

fn set_statement_effects(
    set: &Set,
    rules: &ir::IdentifierRules,
) -> (
    Vec<ir::SessionEffect>,
    Vec<ir::ScopedBarrier>,
    Vec<ir::TxEffect>,
) {
    let mut effects = Vec::new();
    let mut barriers = Vec::new();
    let mut transactions = Vec::new();
    match set {
        Set::SingleAssignment {
            scope,
            variable,
            values,
            ..
        } => {
            let (mut set_effects, mut set_barriers) =
                set_effects(variable, values, context_scope(*scope), rules);
            effects.append(&mut set_effects);
            barriers.append(&mut set_barriers);
        }
        Set::ParenthesizedAssignments {
            variables, values, ..
        } => {
            for (variable, value) in variables.iter().zip(values.iter()) {
                let (mut set_effects, mut set_barriers) = set_effects(
                    variable,
                    std::slice::from_ref(value),
                    ir::SessionScope::Session,
                    rules,
                );
                effects.append(&mut set_effects);
                barriers.append(&mut set_barriers);
            }
        }
        Set::MultipleAssignments { assignments } => {
            for assignment in assignments {
                let (mut set_effects, mut set_barriers) = set_effects(
                    &assignment.name,
                    std::slice::from_ref(&assignment.value),
                    context_scope(assignment.scope),
                    rules,
                );
                effects.append(&mut set_effects);
                barriers.append(&mut set_barriers);
            }
        }
        Set::SetSessionAuthorization(authorization) => {
            let scope = context_scope(Some(authorization.scope));
            match &authorization.kind {
                SetSessionAuthorizationParamKind::User(user) => {
                    effects.push(ir::SessionEffect::SetSessionUser {
                        scope,
                        user: canonical_ident(user, rules, ir::IdentifierClass::Other),
                    });
                }
                SetSessionAuthorizationParamKind::Default => {
                    effects.push(ir::SessionEffect::ResetSessionUser { scope });
                }
            }
        }
        Set::SetRole {
            context_modifier,
            role_name,
        } => {
            let scope = context_scope(*context_modifier);
            match role_name {
                Some(role) => effects.push(ir::SessionEffect::SetCurrentUser {
                    scope,
                    user: canonical_ident(role, rules, ir::IdentifierClass::Other),
                }),
                None => effects.push(ir::SessionEffect::ResetCurrentUser { scope }),
            }
        }
        Set::SetTimeZone { local, value } => {
            effects.push(ir::SessionEffect::SetVariable {
                scope: if *local {
                    ir::SessionScope::Local
                } else {
                    ir::SessionScope::Session
                },
                name: ir::Name(vec![ir::Ident("timezone".to_string())]),
                value: expr_value(value),
            });
        }
        Set::SetNames {
            charset_name,
            collation_name,
        } => {
            let value = match collation_name {
                Some(collation) => format!("{} COLLATE {collation}", charset_name.value),
                None => charset_name.value.clone(),
            };
            effects.push(ir::SessionEffect::SetVariable {
                scope: ir::SessionScope::Session,
                name: ir::Name(vec![ir::Ident("names".to_string())]),
                value: ir::SessionValue::Text { value },
            });
        }
        Set::SetNamesDefault {} => effects.push(ir::SessionEffect::ResetVariable {
            scope: ir::SessionScope::Session,
            name: ir::Name(vec![ir::Ident("names".to_string())]),
        }),
        Set::SetSessionParam(kind) => match kind {
            SetSessionParamKind::Generic(generic) => {
                if generic.names.is_empty() {
                    barriers.push(session_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some("SET with no session parameter name".to_string()),
                    ));
                }
                for name in &generic.names {
                    effects.push(ir::SessionEffect::SetVariable {
                        scope: ir::SessionScope::Session,
                        name: ir::Name(vec![ir::Ident(name.clone())]),
                        value: ir::SessionValue::Text {
                            value: generic.value.clone(),
                        },
                    });
                }
            }
            SetSessionParamKind::IdentityInsert(identity) => {
                let Some(object) =
                    name_from_object(&identity.obj, rules, ir::IdentifierClass::Relation)
                else {
                    barriers.push(session_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some(identity.obj.to_string()),
                    ));
                    return (effects, barriers, transactions);
                };
                effects.push(ir::SessionEffect::SetVariable {
                    scope: ir::SessionScope::Session,
                    name: ir::Name(
                        std::iter::once(ir::Ident("identity_insert".to_string()))
                            .chain(object.0)
                            .collect(),
                    ),
                    value: ir::SessionValue::Text {
                        value: identity.value.to_string(),
                    },
                });
            }
            SetSessionParamKind::Offsets(offsets) => {
                effects.push(ir::SessionEffect::SetVariable {
                    scope: ir::SessionScope::Session,
                    name: ir::Name(vec![ir::Ident("offsets".to_string())]),
                    value: ir::SessionValue::Text {
                        value: format!("{:?} {}", offsets.keywords, offsets.value),
                    },
                });
            }
            SetSessionParamKind::Statistics(statistics) => {
                effects.push(ir::SessionEffect::SetVariable {
                    scope: ir::SessionScope::Session,
                    name: ir::Name(vec![ir::Ident("statistics".to_string())]),
                    value: ir::SessionValue::Text {
                        value: format!("{:?} {:?}", statistics.topic, statistics.value),
                    },
                });
            }
        },
        Set::SetTransaction {
            modes,
            snapshot,
            session,
        } => {
            if snapshot.is_some() {
                barriers.push(transaction_barrier(
                    ir::BarrierReason::UnsupportedTransaction,
                    Some("SET TRANSACTION SNAPSHOT".to_string()),
                ));
            }
            for mode in modes {
                match mode {
                    TransactionMode::AccessMode(TransactionAccessMode::ReadOnly) => {
                        if *session {
                            effects.push(ir::SessionEffect::SetVariable {
                                scope: ir::SessionScope::Session,
                                name: ir::Name(vec![ir::Ident(
                                    "default_transaction_read_only".to_string(),
                                )]),
                                value: ir::SessionValue::Bool { value: true },
                            });
                        } else {
                            transactions.push(ir::TxEffect::SetReadOnly { value: true });
                        }
                    }
                    TransactionMode::AccessMode(TransactionAccessMode::ReadWrite) => {
                        if *session {
                            effects.push(ir::SessionEffect::SetVariable {
                                scope: ir::SessionScope::Session,
                                name: ir::Name(vec![ir::Ident(
                                    "default_transaction_read_only".to_string(),
                                )]),
                                value: ir::SessionValue::Bool { value: false },
                            });
                        } else {
                            transactions.push(ir::TxEffect::SetReadOnly { value: false });
                        }
                    }
                    TransactionMode::IsolationLevel(level) => barriers.push(transaction_barrier(
                        ir::BarrierReason::UnsupportedTransaction,
                        Some(format!("ISOLATION LEVEL {level}")),
                    )),
                }
            }
        }
    }
    (effects, barriers, transactions)
}

fn reset_effects(
    reset: &Reset,
    rules: &ir::IdentifierRules,
) -> (Vec<ir::SessionEffect>, Vec<ir::ScopedBarrier>) {
    match reset {
        Reset::ConfigurationParameter(name) => {
            match name_from_object(name, rules, ir::IdentifierClass::Other) {
                Some(name) => {
                    let scope = ir::SessionScope::Session;
                    match name
                        .last()
                        .map(|ident| ident.0.to_ascii_lowercase())
                        .as_deref()
                    {
                        Some("search_path") => (
                            vec![ir::SessionEffect::SetSearchPath {
                                scope,
                                path: ir::SearchPathValue::Default,
                            }],
                            Vec::new(),
                        ),
                        Some("foreign_keys" | "foreign_key_checks") => (
                            vec![ir::SessionEffect::ResetForeignKeyChecks { scope }],
                            Vec::new(),
                        ),
                        Some("current_user" | "role") => (
                            vec![ir::SessionEffect::ResetCurrentUser { scope }],
                            Vec::new(),
                        ),
                        Some("session_authorization") => (
                            vec![ir::SessionEffect::ResetSessionUser { scope }],
                            Vec::new(),
                        ),
                        _ => (
                            vec![ir::SessionEffect::ResetVariable { scope, name }],
                            Vec::new(),
                        ),
                    }
                }
                None => (
                    Vec::new(),
                    vec![session_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some(name.to_string()),
                    )],
                ),
            }
        }
        Reset::SessionAuthorization => (
            vec![ir::SessionEffect::ResetSessionUser {
                scope: ir::SessionScope::Session,
            }],
            Vec::new(),
        ),
        Reset::ALL => (
            vec![ir::SessionEffect::ResetAll {
                scope: ir::SessionScope::Session,
            }],
            Vec::new(),
        ),
    }
}

fn use_effect(
    use_statement: &Use,
    rules: &ir::IdentifierRules,
) -> (Vec<ir::SessionEffect>, Vec<ir::ScopedBarrier>) {
    match use_statement {
        Use::Object(name) | Use::Schema(name) | Use::Database(name) | Use::Catalog(name) => {
            match name_parts(name, rules, ir::IdentifierClass::Relation) {
                Some(namespace) => (
                    vec![ir::SessionEffect::SetNamespace {
                        scope: ir::SessionScope::Session,
                        namespace,
                    }],
                    Vec::new(),
                ),
                None => (
                    Vec::new(),
                    vec![session_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some(name.to_string()),
                    )],
                ),
            }
        }
        Use::Default => (
            vec![ir::SessionEffect::SetNamespace {
                scope: ir::SessionScope::Session,
                namespace: Vec::new(),
            }],
            Vec::new(),
        ),
        Use::Role(name) => (
            vec![ir::SessionEffect::SetVariable {
                scope: ir::SessionScope::Session,
                name: ir::Name(vec![ir::Ident("role".to_string())]),
                value: ir::SessionValue::Text {
                    value: name.to_string(),
                },
            }],
            Vec::new(),
        ),
        Use::Warehouse(name) => (
            vec![ir::SessionEffect::SetVariable {
                scope: ir::SessionScope::Session,
                name: ir::Name(vec![ir::Ident("warehouse".to_string())]),
                value: ir::SessionValue::Text {
                    value: name.to_string(),
                },
            }],
            Vec::new(),
        ),
        Use::SecondaryRoles(_) => (
            Vec::new(),
            vec![session_barrier(
                ir::BarrierReason::UnknownSessionEffect,
                Some("USE SECONDARY ROLES".to_string()),
            )],
        ),
    }
}

fn begin_policy(dialect: DialectKind) -> ir::BeginPolicy {
    match dialect {
        DialectKind::MySql => ir::BeginPolicy::CommitActive,
        DialectKind::SQLite => ir::BeginPolicy::Error,
        DialectKind::MsSql => ir::BeginPolicy::Ignore,
        _ => ir::BeginPolicy::Warn,
    }
}

fn implicit_commit(dialect: DialectKind, statement: &Statement) -> bool {
    let ddl = matches!(
        statement,
        Statement::CreateTable(_)
            | Statement::CreateView(_)
            | Statement::CreateIndex(_)
            | Statement::CreateTrigger(_)
            | Statement::CreateSequence { .. }
            | Statement::CreateType { .. }
            | Statement::CreateSchema { .. }
            | Statement::CreateDatabase { .. }
            | Statement::CreateFunction(_)
            | Statement::CreateProcedure { .. }
            | Statement::CreateRole(_)
            | Statement::CreateUser(_)
            | Statement::AlterUser(_)
            | Statement::AlterRole { .. }
            | Statement::AlterFunction(_)
            | Statement::Drop { .. }
            | Statement::DropTrigger(_)
            | Statement::DropFunction(_)
            | Statement::DropProcedure { .. }
            | Statement::AlterTable(_)
            | Statement::AlterView(_)
            | Statement::AlterIndex { .. }
            | Statement::AlterSchema(_)
            | Statement::RenameTable(_)
            | Statement::Truncate(_)
            | Statement::Comment { .. }
            | Statement::Grant(_)
            | Statement::Revoke(_)
            | Statement::Deny(_)
    );
    match dialect {
        DialectKind::MySql => {
            if let Statement::CreateTable(create) = statement
                && create.temporary
            {
                return false;
            }
            if let Statement::Drop {
                temporary: true, ..
            } = statement
            {
                return false;
            }
            ddl || matches!(
                statement,
                Statement::LockTables { .. }
                    | Statement::UnlockTables
                    | Statement::Analyze(_)
                    | Statement::OptimizeTable { .. }
                    | Statement::Flush { .. }
            )
        }
        DialectKind::Oracle => ddl,
        // ClickHouse, Hive, Spark, Databricks and Snowflake do not expose the
        // MySQL/Oracle style implicit-commit transaction boundary around DDL.
        _ => false,
    }
}

fn drop_mode(object_type: &ObjectType, cascade: bool, dialect: DialectKind) -> ir::DropMode {
    if cascade {
        return ir::DropMode::Cascade;
    }
    match (object_type, dialect) {
        // These DBMSs always drop the namespace contents; there is no
        // RESTRICT form for dropping a schema/database.
        (
            ObjectType::Schema | ObjectType::Database,
            DialectKind::MySql | DialectKind::BigQuery | DialectKind::ClickHouse,
        ) => ir::DropMode::Cascade,
        _ => ir::DropMode::Restrict,
    }
}

fn trigger_ref_for_drop(
    name: &ObjectName,
    table: Option<&ObjectName>,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<ir::TriggerRef> {
    let trigger_name = name_from_object(name, rules, ir::IdentifierClass::Other)?;
    match table {
        Some(table) => {
            let table = relation_ref(table, dialect, rules)?;
            Some(trigger_ref(&trigger_name, &table, dialect))
        }
        None => Some(ir::TriggerRef::Named { name: trigger_name }),
    }
}

fn drop_object_for(
    object_type: &ObjectType,
    name: &ObjectName,
    temporary: bool,
    table: Option<&ObjectName>,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<ir::DropObject> {
    match object_type {
        ObjectType::Table => {
            let mut reference = relation_ref(name, dialect, rules)?;
            if temporary {
                reference.temp = true;
            }
            Some(ir::DropObject::Relation {
                name: reference,
                kind: ir::RelationKind::Table,
            })
        }
        ObjectType::View => Some(ir::DropObject::Relation {
            name: relation_ref(name, dialect, rules)?,
            kind: ir::RelationKind::View,
        }),
        ObjectType::MaterializedView => Some(ir::DropObject::Relation {
            name: relation_ref(name, dialect, rules)?,
            kind: ir::RelationKind::MaterializedView,
        }),
        ObjectType::Index => {
            let index_name = name_from_object(name, rules, ir::IdentifierClass::Other)?;
            let table = table.and_then(|table| relation_ref(table, dialect, rules));
            Some(ir::DropObject::Index {
                index: index_ref(&index_name, table.as_ref(), dialect),
            })
        }
        ObjectType::Sequence => Some(ir::DropObject::Sequence {
            name: relation_ref(name, dialect, rules)?,
        }),
        ObjectType::Type => Some(ir::DropObject::Type {
            name: name_from_object(name, rules, ir::IdentifierClass::Other)?,
        }),
        ObjectType::Schema | ObjectType::Database => Some(ir::DropObject::Schema {
            name: name_from_object(name, rules, ir::IdentifierClass::Other)?,
        }),
        _ => None,
    }
}

fn alter_change(
    operation: &AlterTableOperation,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
    table: &ir::NameRef,
) -> (
    Vec<ir::AlterChange>,
    Vec<ir::CatalogOp>,
    Vec<ir::ScopedBarrier>,
) {
    let mut changes = Vec::new();
    let mut ops = Vec::new();
    let mut barriers = Vec::new();
    match operation {
        AlterTableOperation::AddColumn { column_def, .. } => {
            let (column, constraints) = column_definition(column_def, dialect, rules);
            changes.push(ir::AlterChange::AddColumn {
                column,
                constraints,
            });
        }
        AlterTableOperation::DropColumn {
            column_names,
            drop_behavior,
            ..
        } => {
            for column in column_names {
                changes.push(ir::AlterChange::DropColumn {
                    column: canonical_ident(column, rules, ir::IdentifierClass::Column),
                    cascade: matches!(drop_behavior, Some(DropBehavior::Cascade)),
                });
            }
        }
        AlterTableOperation::RenameColumn {
            old_column_name,
            new_column_name,
        } => {
            ops.push(ir::CatalogOp::Rename(ir::RenameOp::Column {
                table: table.clone(),
                from: canonical_ident(old_column_name, rules, ir::IdentifierClass::Column),
                to: canonical_ident(new_column_name, rules, ir::IdentifierClass::Column),
            }));
        }
        AlterTableOperation::AddConstraint {
            constraint,
            not_valid,
        } => {
            if let Some(mut constraint) = constraint_from_table(constraint, dialect, rules) {
                match &mut constraint {
                    ir::Constraint::ForeignKey(reference) => reference.not_valid = *not_valid,
                    ir::Constraint::Check {
                        not_valid: validity,
                        ..
                    } => *validity = *not_valid,
                    _ => {}
                }
                changes.push(ir::AlterChange::AddConstraint { constraint });
            } else {
                barriers.push(relation_barrier(
                    table.clone(),
                    ir::BarrierReason::UnknownAlter,
                    Some(operation.to_string()),
                ));
            }
        }
        AlterTableOperation::DropConstraint {
            name,
            drop_behavior,
            ..
        } => changes.push(ir::AlterChange::DropConstraint {
            name: canonical_ident(name, rules, ir::IdentifierClass::Other),
            cascade: matches!(drop_behavior, Some(DropBehavior::Cascade)),
        }),
        AlterTableOperation::DropIndex { name } => {
            changes.push(ir::AlterChange::DropIndex {
                name: canonical_ident(name, rules, ir::IdentifierClass::Other),
            });
        }
        AlterTableOperation::AlterColumn { column_name, op } => {
            changes.push(ir::AlterChange::AlterColumn {
                column: canonical_ident(column_name, rules, ir::IdentifierClass::Column),
                detail: op.to_string(),
            });
        }
        AlterTableOperation::ChangeColumn {
            old_name, new_name, ..
        } => {
            let old_name = canonical_ident(old_name, rules, ir::IdentifierClass::Column);
            let new_name = canonical_ident(new_name, rules, ir::IdentifierClass::Column);
            if old_name != new_name {
                ops.push(ir::CatalogOp::Rename(ir::RenameOp::Column {
                    table: table.clone(),
                    from: old_name,
                    to: new_name.clone(),
                }));
            }
            changes.push(ir::AlterChange::AlterColumn {
                column: new_name,
                detail: operation.to_string(),
            });
        }
        AlterTableOperation::ModifyColumn { col_name, .. } => {
            changes.push(ir::AlterChange::AlterColumn {
                column: canonical_ident(col_name, rules, ir::IdentifierClass::Column),
                detail: operation.to_string(),
            });
        }
        AlterTableOperation::RenameConstraint { old_name, new_name } => {
            changes.push(ir::AlterChange::RenameConstraint {
                from: canonical_ident(old_name, rules, ir::IdentifierClass::Other),
                to: canonical_ident(new_name, rules, ir::IdentifierClass::Other),
            })
        }
        AlterTableOperation::RenameTable { table_name } => {
            let name = match table_name {
                RenameTableNameKind::As(name) | RenameTableNameKind::To(name) => name,
            };
            if let Some(name) = name_from_object(name, rules, ir::IdentifierClass::Relation) {
                ops.push(ir::CatalogOp::Rename(ir::RenameOp::Relation {
                    from: table.clone(),
                    to: qualify_rename(&table.name, name),
                    index_schema_placement: index_schema_placement(dialect),
                }));
            } else {
                barriers.push(relation_barrier(
                    table.clone(),
                    ir::BarrierReason::UnknownAlter,
                    Some(operation.to_string()),
                ));
            }
        }
        AlterTableOperation::Refresh { .. } => {
            ops.push(ir::CatalogOp::Refresh(ir::RefreshOp {
                target: table.clone(),
            }));
        }
        AlterTableOperation::ValidateConstraint { name, .. } => {
            changes.push(ir::AlterChange::ValidateConstraint {
                name: canonical_ident(name, rules, ir::IdentifierClass::Other),
            });
        }
        _ => barriers.push(relation_barrier(
            table.clone(),
            ir::BarrierReason::UnknownAlter,
            Some(operation.to_string()),
        )),
    }
    (changes, ops, barriers)
}

/// Whether evaluating the statement advances a sequence. Definition-time
/// expressions (`CREATE TABLE ... DEFAULT nextval(...)`, view definitions,
/// index expressions) are not evaluated, so they do not write sequence state.
/// Sequence state changes caused by evaluating a statement. Definition-time
/// expressions (`CREATE TABLE ... DEFAULT nextval(...)`, view definitions,
/// index expressions) are not evaluated. `ALTER TABLE ... ADD COLUMN ...
/// DEFAULT` is evaluated for existing rows and therefore does advance the
/// sequence; `ALTER COLUMN ... SET DEFAULT` does not.
fn sequence_writes_for_statement(
    statement: &Statement,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Vec<ir::NameRef> {
    let mut visitor = SequenceVisitor::new(dialect, rules);
    match statement {
        Statement::Query(_)
        | Statement::Insert(_)
        | Statement::Update(_)
        | Statement::Delete(_)
        | Statement::Merge(_)
        | Statement::Call(_) => {
            let _ = statement.visit(&mut visitor);
        }
        Statement::CreateTable(create)
            if create
                .with_data
                .as_ref()
                .is_none_or(|with_data| with_data.data) =>
        {
            if let Some(query) = &create.query {
                let _ = query.visit(&mut visitor);
            }
        }
        Statement::CreateView(create) if create.materialized => {
            let _ = create.query.visit(&mut visitor);
        }
        Statement::AlterTable(alter) => {
            for operation in &alter.operations {
                if let AlterTableOperation::AddColumn { column_def, .. } = operation {
                    for option in &column_def.options {
                        if let ColumnOption::Default(expr) = &option.option {
                            let _ = expr.visit(&mut visitor);
                        }
                    }
                }
            }
        }
        _ => {}
    }
    visitor.writes
}

pub(super) fn analyze_statement(
    statement: &Statement,
    context: AnalysisContext<'_>,
) -> ir::NativeFacts {
    // `EXPLAIN ANALYZE` executes the inner statement. Analyze it directly so
    // its writes, implicit commits, sequence advances, and edits are produced
    // exactly once.
    if let Statement::Explain {
        statement,
        analyze: true,
        ..
    } = statement
    {
        return analyze_statement(statement, context);
    }
    let AnalysisContext { dialect, rules, .. } = context;
    let mut visitor = AnalysisVisitor::new(context);
    if matches!(statement, Statement::Explain { .. }) {
        // Plain EXPLAIN only inspects catalog metadata and statistics, but it
        // still references every relation in the explained statement, including
        // relations inside nested data-modifying CTEs.
        visitor.gate_nested_dml = false;
    }
    let _ = statement.visit(&mut visitor);
    let mut facts = ir::NativeFacts::default();
    let transactional = sequence_transactional(dialect);
    for sequence in sequence_writes_for_statement(statement, dialect, rules) {
        facts
            .ops
            .push(ir::CatalogOp::WriteSequence(ir::SequenceWrite {
                sequence,
                transactional,
            }));
    }
    if implicit_commit(dialect, statement) {
        // The statement runs in its own implicit transaction: close any active
        // transaction before it and the implicit one after it.
        facts.pre_tx.push(ir::TxEffect::Commit { chain: false });
        facts.post_tx.push(ir::TxEffect::Commit { chain: false });
    }
    match statement {
        Statement::Query(query) => {
            let nested = collect_dml_in_query(query);
            add_reads(&mut facts, visitor_reads(&visitor, &[], ir::ReadMode::Data));
            add_unmodeled_barriers(&mut facts, &visitor, &[]);
            merge_nested(&mut facts, nested, context);
        }
        Statement::CreateTable(create) => {
            let nested = create
                .query
                .as_ref()
                .map(|query| collect_dml_in_query(query))
                .unwrap_or_default();
            let excluded: Vec<Span> = vec![create.name.span()];
            let mut sources = visitor_reads(&visitor, &excluded, ir::ReadMode::Data);
            let like = create.like.as_ref().and_then(|like| {
                let name = match like {
                    CreateTableLikeKind::Plain(like) | CreateTableLikeKind::Parenthesized(like) => {
                        &like.name
                    }
                };
                relation_ref(name, dialect, rules)
            });
            if let Some(clone) = &create.clone
                && let Some(reference) = relation_ref(clone, dialect, rules)
            {
                sources.push(read_fact(reference, ir::ReadMode::Data, Vec::new()));
            }
            if let Some(inherits) = &create.inherits {
                for parent in inherits {
                    if let Some(reference) = relation_ref(parent, dialect, rules) {
                        sources.push(read_fact(reference, ir::ReadMode::Schema, Vec::new()));
                    }
                }
            }
            if let Some(parent) = &create.partition_of
                && let Some(reference) = relation_ref(parent, dialect, rules)
            {
                sources.push(read_fact(reference, ir::ReadMode::Schema, Vec::new()));
            }
            if let Some(table) = table_definition(create, dialect, rules, sources, like) {
                for constraint in &table.constraints {
                    if let ir::Constraint::ForeignKey(reference) = constraint {
                        facts.ops.push(ir::CatalogOp::Read(read_fact(
                            reference.parent.clone(),
                            ir::ReadMode::Schema,
                            reference.parent_columns.clone(),
                        )));
                    }
                }
                for column in &table.columns {
                    for reference in &column.references {
                        facts.ops.push(ir::CatalogOp::Read(read_fact(
                            reference.parent.clone(),
                            ir::ReadMode::Schema,
                            reference.parent_columns.clone(),
                        )));
                    }
                }
                facts.ops.push(ir::CatalogOp::DefineTable(table));
            } else {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDdl,
                    Some(create.name.to_string()),
                ));
            }
            add_unmodeled_barriers(&mut facts, &visitor, &excluded);
            merge_nested(&mut facts, nested, context);
        }
        Statement::CreateView(create) => {
            let excluded = [create.name.span()];
            // Defining a plain view only binds its source columns; it does not
            // read source rows. Materialized views execute the query eagerly,
            // including any data-modifying CTEs in the definition.
            let source_mode = if create.materialized {
                ir::ReadMode::Data
            } else {
                ir::ReadMode::Schema
            };
            let nested = collect_dml_in_query(&create.query);
            if !create.materialized && !nested.is_empty() {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownView,
                    Some("data-modifying CTE in a non-materialized view".to_string()),
                ));
            }
            let sources = visitor_reads(&visitor, &excluded, source_mode);
            if let Some((name, temporary)) =
                definition_name(&create.name, dialect, rules, create.temporary)
            {
                let columns = if create.columns.is_empty() {
                    query_output_columns(&create.query, rules).unwrap_or_default()
                } else {
                    create
                        .columns
                        .iter()
                        .map(|column| {
                            canonical_ident(&column.name, rules, ir::IdentifierClass::Column)
                        })
                        .collect()
                };
                facts.ops.push(ir::CatalogOp::DefineView(ir::ViewDef {
                    name,
                    temporary,
                    materialized: create.materialized,
                    columns,
                    sources,
                    if_not_exists: create.if_not_exists,
                    or_replace: create.or_replace || create.or_alter,
                }));
                if let Some(target) = &create.to
                    && let Some(target) = relation_ref(target, dialect, rules)
                {
                    facts.barriers.push(relation_barrier(
                        target,
                        ir::BarrierReason::WriteThroughView,
                        Some("CREATE VIEW ... TO".to_string()),
                    ));
                }
            } else {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownView,
                    Some(create.name.to_string()),
                ));
            }
            add_unmodeled_barriers(&mut facts, &visitor, &excluded);
            if create.materialized {
                merge_nested(&mut facts, nested, context);
            }
        }
        Statement::CreateIndex(create) => {
            let table = relation_ref(&create.table_name, dialect, rules);
            let index_name = create
                .name
                .as_ref()
                .and_then(|name| name_from_object(name, rules, ir::IdentifierClass::Other));
            match (index_name, table) {
                (Some(index_name), Some(table)) => {
                    let columns: Vec<ir::Ident> = create
                        .columns
                        .iter()
                        .filter_map(|column| index_column_ident(column, rules))
                        .collect();
                    facts.ops.push(ir::CatalogOp::DefineIndex(ir::IndexDef {
                        index: index_ref(&index_name, Some(&table), dialect),
                        table,
                        columns,
                        unique: create.unique,
                        predicate: create.predicate.is_some(),
                        expression: create
                            .columns
                            .iter()
                            .any(|column| index_column_ident(column, rules).is_none()),
                        if_not_exists: create.if_not_exists,
                        schema_placement: index_schema_placement(dialect),
                    }));
                }
                _ => facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownIndex,
                    Some(create.to_string()),
                )),
            }
            add_unmodeled_barriers(&mut facts, &visitor, &[create.table_name.span()]);
        }
        Statement::CreateTrigger(create) => {
            let table = relation_ref(&create.table_name, dialect, rules);
            let trigger_name = name_from_object(&create.name, rules, ir::IdentifierClass::Other);
            match (trigger_name, table) {
                (Some(trigger_name), Some(table)) => {
                    let trigger = trigger_ref(&trigger_name, &table, dialect);
                    let body = trigger_body(create, context);
                    let when_reads = trigger_when_reads(create.condition.as_ref(), context);
                    facts.ops.push(ir::CatalogOp::DefineTrigger(ir::TriggerDef {
                        trigger,
                        table: table.clone(),
                        timing: trigger_timing(create.period),
                        events: create
                            .events
                            .iter()
                            .map(|event| trigger_event(event, rules))
                            .collect(),
                        for_each_row: trigger_for_each_row(create, dialect),
                        when_reads,
                        body,
                        referenced_table: create
                            .referenced_table_name
                            .as_ref()
                            .and_then(|name| relation_ref(name, dialect, rules)),
                        temporary: create.temporary,
                        if_not_exists: false,
                        or_replace: create.or_replace || create.or_alter,
                    }));
                }
                _ => facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownTrigger,
                    Some(create.name.to_string()),
                )),
            }
        }
        Statement::DropTrigger(drop_trigger) => {
            if let Some(trigger) = trigger_ref_for_drop(
                &drop_trigger.trigger_name,
                drop_trigger.table_name.as_ref(),
                dialect,
                rules,
            ) {
                facts.ops.push(ir::CatalogOp::Drop(ir::DropOp {
                    objects: vec![ir::DropObject::Trigger { trigger }],
                    mode: ir::DropMode::Restrict,
                }));
            } else {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDrop,
                    Some(drop_trigger.to_string()),
                ));
            }
        }
        Statement::Drop {
            object_type,
            names,
            cascade,
            temporary,
            table,
            ..
        } => {
            let mut objects = Vec::new();
            let mut failed = false;
            for name in names {
                match drop_object_for(
                    object_type,
                    name,
                    *temporary,
                    table.as_ref(),
                    dialect,
                    rules,
                ) {
                    Some(object) => objects.push(object),
                    None => failed = true,
                }
            }
            if failed || objects.is_empty() {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDrop,
                    Some(statement.to_string()),
                ));
            } else {
                facts.ops.push(ir::CatalogOp::Drop(ir::DropOp {
                    objects,
                    mode: drop_mode(object_type, *cascade, dialect),
                }));
            }
        }
        Statement::AlterTable(alter) => {
            if let Some(table) = relation_ref(&alter.name, dialect, rules) {
                let mut changes = Vec::new();
                let mut ops = Vec::new();
                let mut barriers = Vec::new();
                for operation in &alter.operations {
                    let (mut op_changes, mut op_ops, mut op_barriers) =
                        alter_change(operation, dialect, rules, &table);
                    changes.append(&mut op_changes);
                    ops.append(&mut op_ops);
                    barriers.append(&mut op_barriers);
                }
                facts.barriers.extend(barriers);
                if !changes.is_empty() {
                    facts
                        .ops
                        .push(ir::CatalogOp::AlterTable(ir::AlterTable { table, changes }));
                }
                facts.ops.extend(ops);
            } else {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownAlter,
                    Some(alter.name.to_string()),
                ));
            }
        }
        Statement::RenameTable(renames) => {
            for rename in renames {
                match (
                    relation_ref(&rename.old_name, dialect, rules),
                    name_from_object(&rename.new_name, rules, ir::IdentifierClass::Relation),
                ) {
                    (Some(from), Some(to)) => {
                        facts
                            .ops
                            .push(ir::CatalogOp::Rename(ir::RenameOp::Relation {
                                from: from.clone(),
                                to: qualify_rename(&from.name, to),
                                index_schema_placement: index_schema_placement(dialect),
                            }));
                    }
                    _ => facts.barriers.push(global_barrier(
                        ir::BarrierReason::UnknownAlter,
                        Some(rename.to_string()),
                    )),
                }
            }
        }
        Statement::AlterView(alter_view) => {
            let view_name = &alter_view.name;
            let columns = match &alter_view.operation {
                sqlparser::ast::AlterViewOperation::As { columns, .. } => columns.clone(),
                _ => Vec::new(),
            };
            let excluded = [view_name.span()];
            let sources = visitor_reads(&visitor, &excluded, ir::ReadMode::Schema);
            if let Some((name, temporary)) = definition_name(view_name, dialect, rules, false) {
                facts.ops.push(ir::CatalogOp::DefineView(ir::ViewDef {
                    name,
                    temporary,
                    materialized: alter_view.materialized,
                    columns: columns
                        .iter()
                        .map(|column| canonical_ident(column, rules, ir::IdentifierClass::Column))
                        .collect(),
                    sources,
                    if_not_exists: false,
                    or_replace: true,
                }));
            } else {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownView,
                    Some(statement.to_string()),
                ));
            }
        }
        Statement::Insert(insert) => {
            let target_name = table_object_name(&insert.table);
            let target = target_name.and_then(|name| relation_ref(name, dialect, rules));
            let nested = collect_dml_in_statement(statement);
            let excluded: Vec<Span> = target_name
                .map(|name| vec![name.span()])
                .unwrap_or_default();
            match target {
                Some(target) => {
                    let reads = visitor_reads(&visitor, &excluded, ir::ReadMode::Data);
                    facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                        target,
                        kind: ir::WriteKind::Insert {
                            conflict: insert_conflict(insert),
                        },
                        columns: insert_columns(insert, rules),
                        unqualified_columns: Vec::new(),
                        reads,
                    }));
                    add_unmodeled_barriers(&mut facts, &visitor, &excluded);
                    merge_nested(&mut facts, nested, context);
                }
                None => facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDml,
                    Some(insert.table.to_string()),
                )),
            }
        }
        Statement::Update(update) => {
            let targets = update_targets(update, rules);
            let resolved: Vec<(ir::NameRef, Vec<ir::Ident>, Vec<ir::Ident>)> = targets
                .iter()
                .filter_map(|target| {
                    relation_ref(&target.source.name, dialect, rules).map(|reference| {
                        (
                            reference,
                            target.columns.clone(),
                            target.unqualified_columns.clone(),
                        )
                    })
                })
                .collect();
            let nested = collect_dml_in_statement(statement);
            // The target table's own rows and columns are read by the SET
            // expressions, WHERE clause, joins, and ON conditions. Only alias
            // designators that are not physical relations are excluded.
            let excluded: Vec<Span> = targets
                .iter()
                .filter_map(|target| target.designator_span)
                .collect();
            if targets.is_empty() {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDml,
                    Some(update.table.to_string()),
                ));
            } else {
                let reads = visitor_reads(&visitor, &excluded, ir::ReadMode::Data);
                for (target, columns, unqualified_columns) in resolved {
                    facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                        target,
                        kind: ir::WriteKind::Update,
                        columns,
                        unqualified_columns,
                        reads: reads.clone(),
                    }));
                }
                add_unmodeled_barriers(&mut facts, &visitor, &excluded);
                merge_nested(&mut facts, nested, context);
            }
        }
        Statement::Delete(delete) => {
            let targets = delete_targets(delete, rules);
            let resolved: Vec<ir::NameRef> = targets
                .iter()
                .filter_map(|target| relation_ref(&target.source.name, dialect, rules))
                .collect();
            let nested = collect_dml_in_statement(statement);
            // A `DELETE` scans the target table, so its name is a real read.
            // Only alias designators (`DELETE a FROM t AS a`) are not physical
            // relations and must not become reads.
            let excluded: Vec<Span> = targets
                .iter()
                .filter_map(|target| target.designator_span)
                .collect();
            if resolved.is_empty() {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDml,
                    Some(delete.from.to_string()),
                ));
            } else {
                let reads = visitor_reads(&visitor, &excluded, ir::ReadMode::Data);
                for target in resolved {
                    facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                        target,
                        kind: ir::WriteKind::Delete {
                            all: delete.selection.is_none(),
                        },
                        columns: Vec::new(),
                        unqualified_columns: Vec::new(),
                        reads: reads.clone(),
                    }));
                }
                add_unmodeled_barriers(&mut facts, &visitor, &excluded);
                merge_nested(&mut facts, nested, context);
            }
        }
        Statement::Merge(merge) => {
            let target_name = table_factor_name(&merge.table);
            let target = target_name.and_then(|name| relation_ref(name, dialect, rules));
            let nested = collect_dml_in_statement(statement);
            // The ON condition and source joins read the target table.
            let excluded: Vec<Span> = Vec::new();
            match target {
                Some(target) => {
                    let reads = visitor_reads(&visitor, &excluded, ir::ReadMode::Data);
                    facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                        target,
                        kind: ir::WriteKind::Merge,
                        columns: merge_columns(merge, rules),
                        unqualified_columns: Vec::new(),
                        reads,
                    }));
                    add_unmodeled_barriers(&mut facts, &visitor, &excluded);
                    merge_nested(&mut facts, nested, context);
                }
                None => facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDml,
                    Some(merge.table.to_string()),
                )),
            }
        }
        Statement::Truncate(truncate) => {
            if truncate.table_names.is_empty() {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownTruncate,
                    Some(truncate.to_string()),
                ));
            }
            for table in &truncate.table_names {
                match relation_ref(&table.name, dialect, rules) {
                    Some(target) => {
                        facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                            target,
                            kind: ir::WriteKind::Truncate {
                                cascade: matches!(truncate.cascade, Some(CascadeOption::Cascade)),
                                restart_identity: matches!(
                                    truncate.identity,
                                    Some(TruncateIdentityOption::Restart)
                                ),
                            },
                            columns: Vec::new(),
                            unqualified_columns: Vec::new(),
                            reads: Vec::new(),
                        }));
                    }
                    None => facts.barriers.push(relation_barrier(
                        ir::NameRef {
                            name: ir::Name(vec![ir::Ident(table.name.to_string())]),
                            temp: false,
                        },
                        ir::BarrierReason::UnknownTruncate,
                        Some(table.to_string()),
                    )),
                }
            }
        }
        Statement::Set(set) => {
            let (effects, barriers, transactions) = set_statement_effects(set, rules);
            facts.session.extend(effects);
            facts.barriers.extend(barriers);
            facts.post_tx.extend(transactions);
        }
        Statement::Reset(reset) => {
            let (effects, barriers) = reset_effects(&reset.reset, rules);
            facts.session.extend(effects);
            facts.barriers.extend(barriers);
        }
        Statement::Use(use_statement) => {
            let (effects, barriers) = use_effect(use_statement, rules);
            facts.session.extend(effects);
            facts.barriers.extend(barriers);
        }
        Statement::Pragma { name, value, .. } => {
            let pragma = name
                .0
                .last()
                .and_then(|part| match part {
                    ObjectNamePart::Identifier(ident) => Some(ident.value.to_ascii_lowercase()),
                    ObjectNamePart::Function(_) => None,
                })
                .unwrap_or_default();
            if matches!(pragma.as_str(), "foreign_keys" | "foreign_key_checks") {
                // `PRAGMA foreign_keys;` without a value is a read-only query.
                // Only an explicit assignment changes the session setting.
                if let Some(value) = value {
                    match pragma_bool(value) {
                        Some(value) => {
                            facts.session.push(ir::SessionEffect::SetForeignKeyChecks {
                                scope: ir::SessionScope::Session,
                                value,
                            });
                        }
                        None => facts.barriers.push(session_barrier(
                            ir::BarrierReason::UnknownSessionEffect,
                            Some(statement.to_string()),
                        )),
                    }
                }
            } else {
                facts.barriers.push(session_barrier(
                    ir::BarrierReason::UnknownSessionEffect,
                    Some(statement.to_string()),
                ));
            }
        }
        Statement::StartTransaction {
            statements, modes, ..
        } => {
            if statements.is_empty() {
                if !modes.is_empty() {
                    for mode in modes {
                        if let TransactionMode::AccessMode(TransactionAccessMode::ReadOnly) = mode {
                            facts
                                .post_tx
                                .push(ir::TxEffect::SetReadOnly { value: true });
                        } else if let TransactionMode::AccessMode(
                            TransactionAccessMode::ReadWrite,
                        ) = mode
                        {
                            facts
                                .post_tx
                                .push(ir::TxEffect::SetReadOnly { value: false });
                        } else {
                            facts.barriers.push(transaction_barrier(
                                ir::BarrierReason::UnsupportedTransaction,
                                Some(mode.to_string()),
                            ));
                        }
                    }
                }
                facts.pre_tx.push(ir::TxEffect::Begin {
                    if_active: begin_policy(dialect),
                });
            } else {
                for statement in statements {
                    let nested = analyze_statement(statement, context);
                    facts.ops.extend(nested.ops);
                    facts.session.extend(nested.session);
                    facts.pre_tx.extend(nested.pre_tx);
                    facts.post_tx.extend(nested.post_tx);
                    facts.barriers.extend(nested.barriers);
                }
            }
        }
        Statement::Commit {
            chain,
            end,
            modifier,
        } => {
            // `END` is a synonym for `COMMIT` in PostgreSQL and SQLite. It is
            // only a block terminator for the T-SQL `END TRY`/`END CATCH`
            // forms, which the parser marks with a modifier.
            let block_terminator = *end
                && matches!(
                    modifier,
                    Some(TransactionModifier::Try | TransactionModifier::Catch)
                );
            if !block_terminator {
                facts.post_tx.push(ir::TxEffect::Commit { chain: *chain });
            }
        }
        Statement::Rollback { chain, savepoint } => match savepoint {
            Some(savepoint) => facts.post_tx.push(ir::TxEffect::RollbackTo {
                savepoint: canonical_ident(savepoint, rules, ir::IdentifierClass::Other),
            }),
            None => {
                facts.post_tx.push(ir::TxEffect::RollbackAll);
                if *chain {
                    facts.post_tx.push(ir::TxEffect::Begin {
                        if_active: ir::BeginPolicy::Ignore,
                    });
                }
            }
        },
        Statement::Savepoint { name } => {
            facts.post_tx.push(ir::TxEffect::Savepoint {
                name: canonical_ident(name, rules, ir::IdentifierClass::Other),
            });
        }
        Statement::ReleaseSavepoint { name } => {
            facts.post_tx.push(ir::TxEffect::ReleaseSavepoint {
                name: canonical_ident(name, rules, ir::IdentifierClass::Other),
            });
        }
        Statement::Explain { .. } => {
            // `EXPLAIN ANALYZE` returned early and was analyzed as the inner
            // statement. Plain EXPLAIN only inspects catalog metadata and
            // statistics, so it must not apply the nested statement's writes,
            // DDL, session effects, or implicit transaction boundaries.
            add_reads(
                &mut facts,
                visitor_reads(&visitor, &[], ir::ReadMode::Schema),
            );
            add_unmodeled_barriers(&mut facts, &visitor, &[]);
        }
        Statement::ExplainTable { table_name, .. } => {
            if let Some(reference) = relation_ref(table_name, dialect, rules) {
                facts.ops.push(ir::CatalogOp::Read(read_fact(
                    reference,
                    ir::ReadMode::Schema,
                    Vec::new(),
                )));
            } else {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownRelation,
                    Some(table_name.to_string()),
                ));
            }
        }
        Statement::CreateSequence {
            name, temporary, ..
        } => {
            if let Some((name, temporary)) = definition_name(name, dialect, rules, *temporary) {
                facts
                    .ops
                    .push(ir::CatalogOp::DefineSequence(ir::SequenceDef {
                        name,
                        temporary,
                        transactional: sequence_transactional(dialect),
                    }));
            } else {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDdl,
                    Some(statement.to_string()),
                ));
            }
        }
        Statement::CreateType { name, .. } => {
            if let Some(name) = name_from_object(name, rules, ir::IdentifierClass::Other) {
                facts
                    .ops
                    .push(ir::CatalogOp::DefineType(ir::TypeDef { name }));
            } else {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDdl,
                    Some(statement.to_string()),
                ));
            }
        }
        Statement::CreateSchema { schema_name, .. } => {
            let name = match schema_name {
                SchemaName::Simple(name) => {
                    name_from_object(name, rules, ir::IdentifierClass::Other)
                }
                SchemaName::NamedAuthorization(name, _) => {
                    name_from_object(name, rules, ir::IdentifierClass::Other)
                }
                SchemaName::UnnamedAuthorization(_) => None,
            };
            match name {
                Some(name) => facts
                    .ops
                    .push(ir::CatalogOp::DefineSchema(ir::SchemaDef { name })),
                None => facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDdl,
                    Some(statement.to_string()),
                )),
            }
        }
        Statement::Comment { .. }
        | Statement::Grant { .. }
        | Statement::Revoke { .. }
        | Statement::Analyze { .. }
        | Statement::Vacuum(_)
        | Statement::Lock(_)
        | Statement::LockTables { .. }
        | Statement::UnlockTables => {}
        _ => facts.barriers.push(global_barrier(
            ir::BarrierReason::UnknownStatement,
            Some(statement.to_string()),
        )),
    }
    // A sequence can be referenced from both the outer statement and a nested
    // DML statement that is analyzed separately (for example a data-modifying
    // CTE). Both belong to the same statement id, so keep one write per
    // sequence.
    let mut seen_sequences: HashSet<ir::NameRef> = HashSet::new();
    facts.ops.retain(|op| match op {
        ir::CatalogOp::WriteSequence(write) => seen_sequences.insert(write.sequence.clone()),
        _ => true,
    });
    facts.edits = visitor.edits;
    facts
}

use super::*;
use crate::DialectKind;
use crate::edits::{AnalysisContext, AnalysisVisitor, SequenceVisitor};
use crate::ir;
use sqlparser::ast::*;
use sqlparser::tokenizer::Span;
use std::collections::{HashMap, HashSet};

pub(crate) fn select_output_columns(
    select: &Select,
    rules: &ir::IdentifierRules,
) -> Option<Vec<ir::Ident>> {
    let mut columns = Vec::new();
    for item in &select.projection {
        let ident = match item {
            SelectItem::UnnamedExpr(Expr::Identifier(ident)) => ident,
            SelectItem::UnnamedExpr(Expr::CompoundIdentifier(idents)) => idents.last()?,
            SelectItem::ExprWithAlias { alias, .. } => alias,
            // A partial projection is not a closed-world column set.
            _ => return None,
        };
        columns.push(canonical_ident(ident, rules, ir::IdentifierClass::Column));
    }
    Some(columns)
}

pub(crate) fn set_expr_output_columns(
    expr: &SetExpr,
    rules: &ir::IdentifierRules,
) -> Option<Vec<ir::Ident>> {
    match expr {
        SetExpr::Select(select) => select_output_columns(select, rules),
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

pub(crate) fn query_output_columns(
    query: &Query,
    rules: &ir::IdentifierRules,
) -> Option<Vec<ir::Ident>> {
    set_expr_output_columns(&query.body, rules)
}

pub(crate) fn select_into_definition(
    select: &Select,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<ir::TableDef> {
    // Other dialects use the same AST for variable assignment. Parser
    // acceptance alone does not establish table-creation semantics.
    if !matches!(
        dialect,
        DialectKind::PostgreSql | DialectKind::Redshift | DialectKind::MsSql
    ) {
        return None;
    }
    let into = select.into.as_ref()?;
    let [target] = into.targets.as_slice() else {
        return None;
    };
    let parts = match target {
        Expr::Identifier(ident) => std::slice::from_ref(ident),
        Expr::CompoundIdentifier(parts) => parts.as_slice(),
        _ => return None,
    };
    if parts
        .iter()
        .any(|ident| ident.quote_style.is_none() && ident.value.starts_with('@'))
    {
        return None;
    }
    if parts.is_empty() || parts.len() > 4 {
        return None;
    }
    let mut parts = parts
        .iter()
        .map(|ident| canonical_ident(ident, rules, ir::IdentifierClass::Relation))
        .collect();
    let schema_temp = strip_temp_schema(&mut parts, dialect);
    let name = ir::Name(parts);
    let temporary = into.temporary || schema_temp;
    Some(ir::TableDef {
        name,
        temporary,
        transactional: table_transactional(dialect, temporary),
        columns: select_output_columns(select, rules)
            .unwrap_or_default()
            .into_iter()
            .map(|name| ir::ColumnDef {
                name,
                not_null: false,
                has_default: false,
                generated: false,
                references: Vec::new(),
            })
            .collect(),
        constraints: Vec::new(),
        sources: Vec::new(),
        like: None,
        partition_parent: None,
        inherit_parents: Vec::new(),
        on_commit: None,
        if_not_exists: false,
        or_replace: false,
    })
}
/// The target of an MSSQL `OUTPUT ... INTO` clause.
pub(crate) enum OutputInto {
    /// A real table: the statement additionally inserts its output rows
    /// into it.
    Table(ir::NameRef),
    /// A table variable (`@var`): no variable catalog exists, so its uses
    /// are coupled through a relation-scoped fence on the variable name
    /// instead of a global barrier. Table variables are batch-scoped and
    /// cannot alias real tables, so only `@var` touches can observe them.
    TableVariable(ir::NameRef),
    /// Anything else (multiple targets, computed targets): unprovable.
    Unmodeled,
}

pub(crate) fn output_into_target(
    into: &SelectInto,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> OutputInto {
    let [target] = into.targets.as_slice() else {
        return OutputInto::Unmodeled;
    };
    let parts = match target {
        Expr::Identifier(ident) => std::slice::from_ref(ident),
        Expr::CompoundIdentifier(parts) => parts.as_slice(),
        _ => return OutputInto::Unmodeled,
    };
    if parts.is_empty() || parts.len() > 4 {
        return OutputInto::Unmodeled;
    }
    let is_variable = parts
        .iter()
        .any(|ident| ident.quote_style.is_none() && ident.value.starts_with('@'));
    let name: Vec<ir::Ident> = parts
        .iter()
        .map(|ident| canonical_ident(ident, rules, ir::IdentifierClass::Relation))
        .collect();
    let reference = relation_from_parts(name, dialect);
    if is_variable {
        OutputInto::TableVariable(reference)
    } else {
        OutputInto::Table(reference)
    }
}

pub(crate) fn qualify_rename(old: &ir::Name, new: ir::Name) -> ir::Name {
    if new.len() == 1 && old.len() > 1 {
        let mut parts = old.0[..old.0.len() - 1].to_vec();
        parts.extend(new.0);
        ir::Name(parts)
    } else {
        new
    }
}

pub(crate) fn last_ident(name: &ir::Name) -> Option<ir::Ident> {
    name.last().cloned()
}

pub(crate) fn barrier(
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

pub(crate) fn global_barrier(
    reason: ir::BarrierReason,
    detail: Option<String>,
) -> ir::ScopedBarrier {
    barrier(ir::BarrierScope::Global, reason, detail)
}

pub(crate) fn read_fact(
    source: ir::NameRef,
    mode: ir::ReadMode,
    columns: Vec<ir::Ident>,
    only: bool,
) -> ir::ReadFact {
    ir::ReadFact {
        source,
        mode,
        columns,
        only,
    }
}

/// Whether a relation declared in `relation_scope` is lexically visible from
/// `column_scope`. The factor position gates unaliased table references, so a
/// non-LATERAL derived table cannot see sibling FROM items and a LATERAL one
/// only sees the items before it.
pub(crate) fn relation_visible(
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
pub(crate) fn resolve_alias<'a>(
    qualifier: &str,
    from: usize,
    aliases: &'a [crate::edits::AliasScope],
) -> Option<(usize, Option<&'a [String]>)> {
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
            return Some((scope, target.as_deref()));
        }
        current = entry.parent;
        incoming_limit = entry.parent_alias_limit;
    }
    None
}

pub(crate) fn relation_qualifier_matches(
    relation: &ir::NameRef,
    usage: &crate::edits::RelationUse,
    qualifier: &[String],
    column_scope: usize,
    aliases: &[crate::edits::AliasScope],
) -> bool {
    if qualifier.is_empty() {
        relation_visible(usage.scope, usage.factor, column_scope, aliases)
    } else if qualifier.len() == 1 {
        match resolve_alias(&qualifier[0], column_scope, aliases) {
            Some((alias_scope, Some(target))) => {
                alias_scope == usage.scope
                    && target.iter().map(String::as_str).eq(relation
                        .name
                        .0
                        .iter()
                        .map(|part| part.0.as_str()))
            }
            Some((_, None)) => false,
            None => {
                usage.alias.is_none()
                    && relation_visible(usage.scope, usage.factor, column_scope, aliases)
                    && relation.name.last().map(|part| part.0.as_str())
                        == Some(qualifier[0].as_str())
            }
        }
    } else {
        usage.alias.is_none()
            && relation_visible(usage.scope, usage.factor, column_scope, aliases)
            && qualifier.iter().map(String::as_str).eq(relation
                .name
                .0
                .iter()
                .map(|part| part.0.as_str()))
    }
}

pub(crate) fn relation_whole_row(
    relation: &ir::NameRef,
    usage: &crate::edits::RelationUse,
    whole_rows: &[crate::edits::WholeRowUse],
    aliases: &[crate::edits::AliasScope],
) -> bool {
    whole_rows.iter().any(|whole_row| match whole_row {
        crate::edits::WholeRowUse::Source(span) => *span == usage.span,
        crate::edits::WholeRowUse::Projection { qualifier, scope } => {
            if qualifier.is_empty() {
                usage.scope == *scope
            } else {
                relation_qualifier_matches(relation, usage, qualifier, *scope, aliases)
            }
        }
    })
}

pub(crate) fn relation_columns(
    relation: &ir::NameRef,
    usage: &crate::edits::RelationUse,
    columns: &[(Vec<String>, usize)],
    whole_rows: &[crate::edits::WholeRowUse],
    aliases: &[crate::edits::AliasScope],
) -> Vec<ir::Ident> {
    if relation_whole_row(relation, usage, whole_rows, aliases) {
        return Vec::new();
    }
    let mut result = Vec::new();
    let mut seen = HashSet::new();
    for (parts, column_scope) in columns {
        let Some((column, qualifier)) = parts.split_last() else {
            continue;
        };
        if relation_qualifier_matches(relation, usage, qualifier, *column_scope, aliases) {
            let column = ir::Ident(column.clone());
            if seen.insert(column.clone()) {
                result.push(column);
            }
        }
    }
    result
}

pub(crate) fn visitor_relations<'a>(
    collector: &'a AnalysisVisitor<'_>,
    excluded: &[Span],
) -> Vec<(&'a ir::NameRef, &'a crate::edits::RelationUse)> {
    let mut relations = Vec::new();
    let mut seen = HashSet::new();
    for relation in &collector.relations {
        if relation.is_cte
            || excluded.contains(&relation.span)
            || collector.unmodeled_relation_spans.contains(&relation.span)
        {
            continue;
        }
        if let Some(reference) = &relation.name {
            let key = (
                reference,
                relation.scope,
                relation.alias.as_deref(),
                relation.factor,
            );
            if seen.insert(key) {
                relations.push((reference, relation));
            }
        }
    }
    relations
}

pub(crate) fn has_unmodeled_relation(collector: &AnalysisVisitor<'_>, excluded: &[Span]) -> bool {
    collector.unmodeled_table_source
        || collector
            .unmodeled_relation_spans
            .iter()
            .any(|span| !excluded.contains(span))
        || collector.relations.iter().any(|relation| {
            !relation.is_cte && !excluded.contains(&relation.span) && relation.name.is_none()
        })
}

pub(crate) fn add_unmodeled_barriers(
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

pub(crate) fn visitor_reads(
    collector: &AnalysisVisitor<'_>,
    excluded: &[Span],
    mode: ir::ReadMode,
) -> Vec<ir::ReadFact> {
    visitor_relations(collector, excluded)
        .into_iter()
        .map(|(reference, usage)| {
            let columns = relation_columns(
                reference,
                usage,
                &collector.columns,
                &collector.whole_rows,
                &collector.aliases,
            );
            read_fact(reference.clone(), mode, columns, usage.only)
        })
        .collect()
}

pub(crate) fn add_reads(facts: &mut ir::NativeFacts, reads: Vec<ir::ReadFact>) {
    facts.ops.extend(reads.into_iter().map(ir::CatalogOp::Read));
}

/// Tables covered by a whole-row use in this statement, resolved with the
/// exact matcher read attribution uses, so star fences stay consistent with
/// empty-column reads.
pub(crate) fn column_stars(collector: &AnalysisVisitor<'_>) -> Vec<ir::NameRef> {
    let mut stars = Vec::new();
    let mut seen = HashSet::new();
    for (reference, usage) in visitor_relations(collector, &[]) {
        if relation_whole_row(reference, usage, &collector.whole_rows, &collector.aliases)
            && seen.insert(reference.clone())
        {
            stars.push(reference.clone());
        }
    }
    stars
}

pub(crate) fn referential_action(action: Option<ReferentialAction>) -> ir::ReferentialAction {
    match action {
        Some(ReferentialAction::Cascade) => ir::ReferentialAction::Cascade,
        Some(ReferentialAction::SetNull) => ir::ReferentialAction::SetNull,
        Some(ReferentialAction::SetDefault) => ir::ReferentialAction::SetDefault,
        Some(ReferentialAction::Restrict) => ir::ReferentialAction::Restrict,
        Some(ReferentialAction::NoAction) | None => ir::ReferentialAction::NoAction,
    }
}

pub(crate) fn foreign_key(
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

pub(crate) fn index_length_prefix_ident(expr: &Expr) -> Option<&Ident> {
    let Expr::Function(function) = expr else {
        return None;
    };
    if function.over.is_some()
        || function.filter.is_some()
        || !function.within_group.is_empty()
        || function.null_treatment.is_some()
        || !matches!(function.parameters, FunctionArguments::None)
    {
        return None;
    }
    let [ObjectNamePart::Identifier(ident)] = function.name.0.as_slice() else {
        return None;
    };
    let FunctionArguments::List(arguments) = &function.args else {
        return None;
    };
    if arguments.duplicate_treatment.is_some()
        || !arguments.clauses.is_empty()
        || arguments.args.len() != 1
    {
        return None;
    }
    let [FunctionArg::Unnamed(FunctionArgExpr::Expr(Expr::Value(length)))] =
        arguments.args.as_slice()
    else {
        return None;
    };
    match &length.value {
        Value::Number(text, _)
            if !text.is_empty() && text.bytes().all(|byte| byte.is_ascii_digit()) =>
        {
            Some(ident)
        }
        _ => None,
    }
}

pub(crate) fn index_column_ident(
    column: &IndexColumn,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<ir::Ident> {
    match &column.column.expr {
        Expr::Identifier(ident) => Some(canonical_ident(ident, rules, ir::IdentifierClass::Column)),
        expr if matches!(dialect, DialectKind::MySql) => index_length_prefix_ident(expr)
            .map(|ident| canonical_ident(ident, rules, ir::IdentifierClass::Column)),
        _ => None,
    }
}

/// Key-part columns of an index-bearing table constraint (`PRIMARY KEY`,
/// `UNIQUE`, `INDEX`/`KEY`, fulltext/spatial). Every other constraint shape
/// carries no key parts.
pub(crate) fn index_constraint_columns(constraint: &TableConstraint) -> &[IndexColumn] {
    match constraint {
        TableConstraint::PrimaryKey(primary) => &primary.columns,
        TableConstraint::Unique(unique) => &unique.columns,
        TableConstraint::Index(index) => &index.columns,
        TableConstraint::FulltextOrSpatial(fulltext) => &fulltext.columns,
        _ => &[],
    }
}

/// Whether `expr` is a MySQL index length prefix (`col(N)`), which sqlparser
/// lowers as `Expr::Function`. The match is structural — position plus the
/// exact prefix shape (single-part name, one unsigned-numeric literal, no
/// window/filter/grouping/parametric machinery) — never a name-table lookup:
/// functional key parts require double parens and lower as `Nested`, so a
/// bare `name(N)` in key position is the length-prefix production, a
/// positional marker rather than a call.
pub(crate) fn is_index_length_prefix(expr: &Expr) -> bool {
    index_length_prefix_ident(expr).is_some()
}

/// Exclude MySQL index length prefixes in `columns` from the deferred routine
/// fence. Real calls in key position (expression indexes, column arguments,
/// qualified names) match no prefix shape and keep fencing.
pub(crate) fn ignore_index_length_prefixes(roots: &mut Vec<Span>, columns: &[IndexColumn]) {
    for column in columns {
        if is_index_length_prefix(&column.column.expr) {
            roots.push(column.column.expr.span());
        }
    }
}

pub(crate) fn constraint_from_table(
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
                .filter_map(|column| index_column_ident(column, dialect, rules))
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
                .filter_map(|column| index_column_ident(column, dialect, rules))
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
                .filter_map(|column| index_column_ident(column, dialect, rules))
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

pub(crate) fn column_definition(
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

pub(crate) fn table_definition(
    create: &CreateTable,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
    sources: Vec<ir::ReadFact>,
    like: Option<ir::NameRef>,
    partition_parent: Option<ir::NameRef>,
    inherit_parents: Vec<ir::NameRef>,
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
        partition_parent,
        inherit_parents,
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

pub(crate) fn table_transactional(dialect: DialectKind, temporary: bool) -> bool {
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

pub(crate) fn sequence_transactional(dialect: DialectKind) -> bool {
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

pub(crate) fn index_ref(
    name: &ir::Name,
    table: Option<&ir::NameRef>,
    dialect: DialectKind,
) -> ir::IndexRef {
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

pub(crate) fn index_schema_placement(dialect: DialectKind) -> ir::IndexSchemaPlacement {
    match dialect {
        // Oracle creates an index in the schema of its owner, even when the
        // indexed table is in another schema.
        DialectKind::Oracle => ir::IndexSchemaPlacement::CurrentSchema,
        _ => ir::IndexSchemaPlacement::TableSchema,
    }
}

pub(crate) fn trigger_ref(
    name: &ir::Name,
    table: &ir::NameRef,
    dialect: DialectKind,
) -> ir::TriggerRef {
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

pub(crate) fn table_object_name(table: &TableObject) -> Option<&ObjectName> {
    match table {
        TableObject::TableName(name) => Some(name),
        TableObject::TableFunction(_) | TableObject::TableQuery(_) => None,
    }
}

pub(crate) fn table_factor_name(factor: &TableFactor) -> Option<&ObjectName> {
    match factor {
        TableFactor::Table {
            name, args: None, ..
        } => Some(name),
        _ => None,
    }
}

pub(crate) fn first_from_table(from: &FromTable) -> Option<&TableWithJoins> {
    let tables = match from {
        FromTable::WithFromKeyword(tables) | FromTable::WithoutKeyword(tables) => tables,
    };
    tables.first()
}

pub(crate) fn assignment_columns(
    target: &AssignmentTarget,
    rules: &ir::IdentifierRules,
) -> Vec<ir::Ident> {
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

pub(crate) fn insert_conflict(insert: &Insert) -> ir::ConflictAction {
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

pub(crate) fn insert_columns(insert: &Insert, rules: &ir::IdentifierRules) -> Vec<ir::Ident> {
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

/// Column references in an `ON CONFLICT` clause that read the stored target
/// row. `EXCLUDED`-qualified references name the proposed row, not stored
/// data, so they contribute no read; anything else (bare or
/// target-qualified) is a target-row read, mirroring plain `UPDATE` where
/// the target stays a readable relation.
pub(crate) struct ConflictReadCollector<'a> {
    rules: &'a ir::IdentifierRules,
    /// Each referenced column, kept only if seen outside an `EXCLUDED`
    /// qualifier at least once.
    stored: HashMap<ir::Ident, bool>,
    order: Vec<ir::Ident>,
}

impl Visitor for ConflictReadCollector<'_> {
    type Break = ();

    fn pre_visit_expr(&mut self, expr: &Expr) -> std::ops::ControlFlow<Self::Break> {
        let (ident, excluded) = match expr {
            Expr::Identifier(ident) => (ident, false),
            Expr::CompoundIdentifier(idents) => match idents.last() {
                Some(last) => {
                    let excluded = idents.first().is_some_and(|first| {
                        first.quote_style.is_none() && first.value.eq_ignore_ascii_case("excluded")
                    });
                    (last, excluded)
                }
                None => return std::ops::ControlFlow::Continue(()),
            },
            _ => return std::ops::ControlFlow::Continue(()),
        };
        let column = canonical_ident(ident, self.rules, ir::IdentifierClass::Column);
        match self.stored.get_mut(&column) {
            Some(seen_stored) => *seen_stored |= !excluded,
            None => {
                self.stored.insert(column.clone(), !excluded);
                self.order.push(column);
            }
        }
        std::ops::ControlFlow::Continue(())
    }
}

pub(crate) fn conflict_expr_columns(expr: &Expr, rules: &ir::IdentifierRules) -> Vec<ir::Ident> {
    let mut collector = ConflictReadCollector {
        rules,
        stored: HashMap::new(),
        order: Vec::new(),
    };
    let _ = expr.visit(&mut collector);
    let ConflictReadCollector { stored, order, .. } = collector;
    order
        .into_iter()
        .filter(|column| stored.get(column).copied().unwrap_or(false))
        .collect()
}

/// Target-row reads implied by an `ON CONFLICT` clause: the arbiter probe
/// reads the conflict-target columns (plus the partial-index predicate, if
/// any), and `DO UPDATE` re-reads the row through its `SET` values and
/// `WHERE` filter. Plain `INSERT` excludes its target from visitor reads, so
/// without this the upsert under-models exactly what the engine authorizes.
pub(crate) fn conflict_read_columns(
    conflict: &OnConflict,
    rules: &ir::IdentifierRules,
) -> Vec<ir::Ident> {
    let mut columns = Vec::new();
    let mut seen = HashSet::new();
    let mut push = |column: ir::Ident| {
        if seen.insert(column.clone()) {
            columns.push(column);
        }
    };
    if let Some(ConflictTarget::Columns {
        columns: arbiter,
        predicate,
    }) = &conflict.conflict_target
    {
        for column in arbiter {
            push(canonical_ident(column, rules, ir::IdentifierClass::Column));
        }
        if let Some(predicate) = predicate {
            for column in conflict_expr_columns(predicate, rules) {
                push(column);
            }
        }
    }
    if let OnConflictAction::DoUpdate(update) = &conflict.action {
        for assignment in &update.assignments {
            for column in conflict_expr_columns(&assignment.value, rules) {
                push(column);
            }
        }
        if let Some(selection) = &update.selection {
            for column in conflict_expr_columns(selection, rules) {
                push(column);
            }
        }
    }
    columns
}

/// Attach `ON CONFLICT` target-row reads to an upsert write's reads,
/// extending the visitor read when the target is already read (e.g. a
/// `DO UPDATE` subquery over the target itself).
pub(crate) fn merge_conflict_reads(
    reads: &mut Vec<ir::ReadFact>,
    target: &ir::NameRef,
    conflict: &OnConflict,
    rules: &ir::IdentifierRules,
) {
    merge_target_reads(reads, target, conflict_read_columns(conflict, rules));
}
/// Whether `function` is the MySQL proposed-row marker `VALUES(col)` in an
/// `ON DUPLICATE KEY UPDATE` assignment. Positional, never a name-table
/// lookup: unqualified single-part unquoted `VALUES` (case-insensitive, like
/// `EXCLUDED`) in the plain call shape. Quoted spellings may name a user
/// routine and never match; exotic shapes (`OVER`/`FILTER`/...) are invalid
/// and stay fenced. Shadowing (`CREATE FUNCTION values`) is handled by the
/// caller via `pure_table_enabled` (no exclusion when shadowed).
pub(crate) fn is_values_marker(function: &Function) -> bool {
    if function.over.is_some()
        || function.filter.is_some()
        || !function.within_group.is_empty()
        || function.null_treatment.is_some()
        || !matches!(function.parameters, sqlparser::ast::FunctionArguments::None)
    {
        return false;
    }
    let [ObjectNamePart::Identifier(ident)] = function.name.0.as_slice() else {
        return false;
    };
    ident.quote_style.is_none() && ident.value.eq_ignore_ascii_case("values")
}
/// Spans of `VALUES(col)` markers in `ON DUPLICATE KEY UPDATE` assignments,
/// excluded from the routine fence (they name no routine). Collected only on
/// MySQL when the pure table is enabled (unshadowed); elsewhere `VALUES`
/// keeps fencing as an unmodeled call.
pub(crate) struct ValuesMarkerCollector {
    spans: Vec<Span>,
}

impl Visitor for ValuesMarkerCollector {
    type Break = ();

    fn pre_visit_expr(&mut self, expr: &Expr) -> std::ops::ControlFlow<Self::Break> {
        if let Expr::Function(function) = expr
            && is_values_marker(function)
        {
            self.spans.push(expr.span());
        }
        std::ops::ControlFlow::Continue(())
    }
}

pub(crate) fn values_marker_spans(assignments: &[Assignment]) -> Vec<Span> {
    let mut collector = ValuesMarkerCollector { spans: Vec::new() };
    for assignment in assignments {
        let _ = assignment.value.visit(&mut collector);
    }
    collector.spans
}

/// Column references in an `ON DUPLICATE KEY UPDATE` assignment value that
/// read the stored target row. `VALUES(col)` args and row-alias-qualified
/// references (`new.col`) name the proposed row, not stored data, so they
/// contribute no read; anything else (bare or target-qualified) is a
/// target-row read, mirroring `ON CONFLICT` (where `EXCLUDED` plays the
/// proposed-row role) and plain `UPDATE` (where the target stays readable).
/// Subquery columns over other tables are over-approximated onto the target
/// (safe: visitor reads still attribute them correctly); row-alias matches
/// are broad (case-insensitive, quoting ignored) since a miss only shrinks
/// the over-approximation while the visitor covers other-table reads.
pub(crate) struct DuplicateReadCollector<'a> {
    rules: &'a ir::IdentifierRules,
    row_alias: Option<&'a str>,
    values_depth: usize,
    stored: HashMap<ir::Ident, bool>,
    order: Vec<ir::Ident>,
}

impl Visitor for DuplicateReadCollector<'_> {
    type Break = ();

    fn pre_visit_expr(&mut self, expr: &Expr) -> std::ops::ControlFlow<Self::Break> {
        if let Expr::Function(function) = expr
            && is_values_marker(function)
        {
            self.values_depth += 1;
            return std::ops::ControlFlow::Continue(());
        }
        let (ident, proposed) = match expr {
            Expr::Identifier(ident) => (ident, self.values_depth > 0),
            Expr::CompoundIdentifier(idents) => match idents.last() {
                Some(last) => {
                    let proposed = self.values_depth > 0
                        || idents.first().is_some_and(|first| {
                            self.row_alias
                                .is_some_and(|alias| first.value.eq_ignore_ascii_case(alias))
                        });
                    (last, proposed)
                }
                None => return std::ops::ControlFlow::Continue(()),
            },
            _ => return std::ops::ControlFlow::Continue(()),
        };
        let column = canonical_ident(ident, self.rules, ir::IdentifierClass::Column);
        match self.stored.get_mut(&column) {
            Some(seen_stored) => *seen_stored |= !proposed,
            None => {
                self.stored.insert(column.clone(), !proposed);
                self.order.push(column);
            }
        }
        std::ops::ControlFlow::Continue(())
    }

    fn post_visit_expr(&mut self, expr: &Expr) -> std::ops::ControlFlow<Self::Break> {
        if let Expr::Function(function) = expr
            && is_values_marker(function)
        {
            self.values_depth = self.values_depth.saturating_sub(1);
        }
        std::ops::ControlFlow::Continue(())
    }
}

pub(crate) fn duplicate_expr_columns(
    expr: &Expr,
    row_alias: Option<&str>,
    rules: &ir::IdentifierRules,
) -> Vec<ir::Ident> {
    let mut collector = DuplicateReadCollector {
        rules,
        row_alias,
        values_depth: 0,
        stored: HashMap::new(),
        order: Vec::new(),
    };
    let _ = expr.visit(&mut collector);
    let DuplicateReadCollector { stored, order, .. } = collector;
    order
        .into_iter()
        .filter(|column| stored.get(column).copied().unwrap_or(false))
        .collect()
}

/// Row alias naming the proposed row in `INSERT ... AS alias
/// ON DUPLICATE KEY UPDATE` (MySQL 8.0.19+). Only single-part aliases
/// resolve; anything else stays `None` (no exclusion, safe over-approx).
pub(crate) fn duplicate_row_alias(insert: &Insert) -> Option<&str> {
    let aliases = insert.insert_alias.as_ref()?;
    let [ObjectNamePart::Identifier(ident)] = aliases.row_alias.0.as_slice() else {
        return None;
    };
    Some(ident.value.as_str())
}

/// Target-row reads implied by `ON DUPLICATE KEY UPDATE`: the duplicate-key
/// probe reads the provided key columns (explicit `INSERT` column list, or
/// `SET` targets for the `INSERT ... SET` form), and the update re-reads
/// the row through its `SET` values (excluding `VALUES()`/row-alias
/// proposed-row refs). No explicit list (e.g. `INSERT ... VALUES` without
/// columns, `INSERT ... SELECT` without a list) leaves the probe vacuous,
/// mirroring `ON CONFLICT` without an arbiter: without a catalog the
/// conflicting unique key(s) are unnameable, and the precedent stays
/// vacuous rather than wildcarding. Residual: provided-list probes may miss
/// default-valued keys (e.g. auto-increment PK omitted from the list);
/// composite keys spanning provided plus default columns are likewise
/// partial. Plain `INSERT` excludes its target from visitor reads, so
/// without this the upsert under-models the stored-row re-read.
pub(crate) fn duplicate_read_columns(
    insert: &Insert,
    rules: &ir::IdentifierRules,
) -> Vec<ir::Ident> {
    let Some(OnInsert::DuplicateKeyUpdate(assignments)) = &insert.on else {
        return Vec::new();
    };
    let mut columns = Vec::new();
    let mut seen = HashSet::new();
    let mut push = |column: ir::Ident| {
        if seen.insert(column.clone()) {
            columns.push(column);
        }
    };
    for column in &insert.columns {
        if let Some(name) = name_from_object(column, rules, ir::IdentifierClass::Column)
            && let Some(last) = last_ident(&name)
        {
            push(last);
        }
    }
    for assignment in &insert.assignments {
        for column in assignment_columns(&assignment.target, rules) {
            push(column);
        }
    }
    let row_alias = duplicate_row_alias(insert);
    for assignment in assignments {
        for column in duplicate_expr_columns(&assignment.value, row_alias, rules) {
            push(column);
        }
    }
    columns
}

/// Attach target-row reads to an upsert write's reads, extending the visitor
/// read when the target is already read (e.g. an update-subquery over the
/// target itself). Shared by `ON CONFLICT` and `ON DUPLICATE KEY UPDATE`.
pub(crate) fn merge_target_reads(
    reads: &mut Vec<ir::ReadFact>,
    target: &ir::NameRef,
    extra: Vec<ir::Ident>,
) {
    if extra.is_empty() {
        return;
    }
    if let Some(existing) = reads
        .iter_mut()
        .find(|read| read.mode == ir::ReadMode::Data && read.source == *target)
    {
        for column in extra {
            if !existing.columns.contains(&column) {
                existing.columns.push(column);
            }
        }
    } else {
        reads.push(read_fact(target.clone(), ir::ReadMode::Data, extra, false));
    }
}

/// Attach `ON DUPLICATE KEY UPDATE` target-row reads to an upsert write's
/// reads (see [`duplicate_read_columns`]).
pub(crate) fn merge_duplicate_reads(
    reads: &mut Vec<ir::ReadFact>,
    target: &ir::NameRef,
    insert: &Insert,
    rules: &ir::IdentifierRules,
) {
    merge_target_reads(reads, target, duplicate_read_columns(insert, rules));
}

/// A plain `VALUES` query source: positional rows with no `WITH` wrapper.
/// Ordering/limits do not move item positions, so only the body shape and
/// the absence of CTEs matter.
pub(crate) fn values_source(query: &Query) -> Option<&Values> {
    if query.with.is_some() {
        return None;
    }
    match query.body.as_ref() {
        SetExpr::Values(values) => Some(values),
        _ => None,
    }
}

/// Column geometry for one `INSERT`: per-unit assembly input plus the raw
/// single-point sites (span validity only — emitted regardless of shape).
/// `force_exotic` marks shapes the `UnknownDml` arm already fenced.
pub(crate) fn insert_column_info(
    visitor: &AnalysisVisitor<'_>,
    insert: &Insert,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
    force_exotic: bool,
) -> (
    crate::columns::InsertColumns,
    Vec<(usize, usize, &'static str)>,
) {
    use crate::columns::{InsertColumns, InsertShape};
    let mut singles = Vec::new();
    let target =
        table_object_name(&insert.table).and_then(|name| relation_ref(name, dialect, rules));
    let mut tainted = force_exotic;
    let explicit = if insert.columns.is_empty() {
        None
    } else {
        let deletions = visitor
            .insert_list_item_spans(&insert.columns)
            .map(|spans| visitor.comma_delete_spans(&spans))
            .unwrap_or_default();
        if deletions.len() != insert.columns.len() {
            tainted = true;
        }
        let mut list = Vec::with_capacity(insert.columns.len());
        for (index, column) in insert.columns.iter().enumerate() {
            let resolved = name_from_object(column, rules, ir::IdentifierClass::Column)
                .and_then(|name| last_ident(&name));
            let span = deletions.get(index).copied().flatten();
            match resolved {
                Some(ident) => {
                    if let Some((start, end)) = span {
                        singles.push((start, end, "remove-column_insert_item"));
                    }
                    list.push((ident, span));
                }
                None => tainted = true,
            }
        }
        Some(list)
    };
    // Positional binding is unsound under any of these; assembly fences the
    // whole target table. Singles above are unaffected (span validity only).
    tainted = tainted
        || insert.overwrite
        || insert.partitioned.is_some()
        || insert.multi_table_insert_type.is_some()
        || !insert.multi_table_into_clauses.is_empty()
        || !insert.multi_table_when_clauses.is_empty()
        || insert.multi_table_else_clause.is_some()
        || insert.by_name
        || !insert.assignments.is_empty()
        || !insert.after_columns.is_empty()
        || insert.output.is_some()
        || insert.returning.is_some()
        || insert.on.is_some()
        || insert_conflict(insert) != ir::ConflictAction::Error;
    let mut rows = Vec::new();
    let shape = if tainted {
        InsertShape::Exotic
    } else {
        match insert.source.as_deref() {
            None => InsertShape::Neutral,
            Some(query) => match values_source(query) {
                Some(values) => {
                    for row in &values.rows {
                        match visitor.values_item_spans(row, values.explicit_row) {
                            Some(items) => {
                                let deletions = visitor.comma_delete_spans(&items);
                                for span in deletions.iter().flatten() {
                                    singles.push((span.0, span.1, "remove-column_values_item"));
                                }
                                rows.push(deletions);
                            }
                            // Unmappable rows contribute no positional claim;
                            // bundles needing them fail arity, singles skip.
                            None => rows.push(Vec::new()),
                        }
                    }
                    InsertShape::PlainValues
                }
                None => InsertShape::SelectSource,
            },
        }
    };
    (
        InsertColumns {
            target,
            explicit,
            rows,
            shape,
        },
        singles,
    )
}

/// Whether a DML table factor excludes partition/inheritance children
/// (`ONLY t`). Anything else (`t *`, unmarked, non-table factors) includes
/// them.
pub(crate) fn table_factor_only(factor: &TableFactor) -> bool {
    matches!(
        factor,
        TableFactor::Table {
            inheritance: Some(TableInheritance::Only { .. }),
            ..
        }
    )
}

/// One physical table factor that can be named by a DML statement.
#[derive(Clone, Debug)]
pub(crate) struct TableSource {
    name: ObjectName,
    alias: Option<String>,
    table_parts: Vec<String>,
    only: bool,
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
pub(crate) struct UpdateTarget {
    source: TableSource,
    columns: Vec<ir::Ident>,
    unqualified_columns: Vec<ir::Ident>,
    /// The alias designator in `UPDATE alias SET ... FROM table AS alias`.
    /// It is not a physical relation and must not become a read.
    designator_span: Option<Span>,
}

#[derive(Clone, Debug)]
pub(crate) struct DeleteTarget {
    source: TableSource,
    /// The designator in `DELETE alias FROM table AS alias`. It is excluded
    /// from reads; the corresponding `FROM` factor remains readable.
    designator_span: Option<Span>,
}

pub(crate) fn assignment_column_target(
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

pub(crate) fn assignment_targets(
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

pub(crate) fn table_factor_source(
    factor: &TableFactor,
    rules: &ir::IdentifierRules,
) -> Option<TableSource> {
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
        only: table_factor_only(factor),
    })
}

pub(crate) fn table_with_joins_sources(
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

pub(crate) fn from_sources(from: &FromTable, rules: &ir::IdentifierRules) -> Vec<TableSource> {
    let tables = match from {
        FromTable::WithFromKeyword(tables) | FromTable::WithoutKeyword(tables) => tables,
    };
    tables
        .iter()
        .flat_map(|table| table_with_joins_sources(table, rules))
        .collect()
}

pub(crate) fn source_from_name(
    name: &ObjectName,
    rules: &ir::IdentifierRules,
) -> Option<TableSource> {
    let parts = name_parts(name, rules, ir::IdentifierClass::Relation)?;
    Some(TableSource {
        name: name.clone(),
        alias: None,
        table_parts: parts.into_iter().map(|part| part.0).collect(),
        only: false,
    })
}

pub(crate) fn find_source<'a>(
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
pub(crate) fn update_targets(update: &Update, rules: &ir::IdentifierRules) -> Vec<UpdateTarget> {
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

pub(crate) fn delete_targets(delete: &Delete, rules: &ir::IdentifierRules) -> Vec<DeleteTarget> {
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

pub(crate) fn merge_columns(merge: &Merge, rules: &ir::IdentifierRules) -> Vec<ir::Ident> {
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
pub(crate) struct NestedDml {
    statements: Vec<Statement>,
    skip_root: bool,
    dml_depth: usize,
}

pub(crate) fn is_dml_statement(statement: &Statement) -> bool {
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

pub(crate) fn collect_dml_in_query(query: &Query) -> Vec<Statement> {
    let mut collector = NestedDml::default();
    let _ = query.visit(&mut collector);
    collector.statements
}

pub(crate) fn collect_dml_in_statement(statement: &Statement) -> Vec<Statement> {
    let mut collector = NestedDml {
        skip_root: true,
        ..NestedDml::default()
    };
    let _ = statement.visit(&mut collector);
    collector.statements
}

pub(crate) fn merge_nested(
    facts: &mut ir::NativeFacts,
    statements: Vec<Statement>,
    context: AnalysisContext<'_>,
) {
    // Nested analyses never collect column geometry: their spans belong to
    // the parent unit but their bindings do not, and merged ops already feed
    // the conservative reference fences.
    let nested_context = AnalysisContext {
        column_ir: false,
        ..context
    };
    for statement in statements {
        let nested = analyze_statement(&statement, nested_context);
        facts.ops.extend(nested.ops);
        facts.session.extend(nested.session);
        facts.pre_tx.extend(nested.pre_tx);
        facts.post_tx.extend(nested.post_tx);
        facts.barriers.extend(nested.barriers);
    }
}

pub(crate) fn trigger_event(event: &TriggerEvent, rules: &ir::IdentifierRules) -> ir::TriggerEvent {
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

pub(crate) fn trigger_timing(period: Option<TriggerPeriod>) -> ir::TriggerTiming {
    match period {
        Some(TriggerPeriod::After) => ir::TriggerTiming::After,
        Some(TriggerPeriod::InsteadOf) => ir::TriggerTiming::InsteadOf,
        Some(TriggerPeriod::Before) | Some(TriggerPeriod::For) | None => ir::TriggerTiming::Before,
    }
}

pub(crate) fn trigger_for_each_row(trigger: &CreateTrigger, dialect: DialectKind) -> bool {
    match trigger.trigger_object {
        Some(TriggerObjectKind::For(TriggerObject::Row))
        | Some(TriggerObjectKind::ForEach(TriggerObject::Row)) => true,
        Some(TriggerObjectKind::For(TriggerObject::Statement))
        | Some(TriggerObjectKind::ForEach(TriggerObject::Statement)) => false,
        None => matches!(dialect, DialectKind::MySql | DialectKind::SQLite),
    }
}

pub(crate) fn trigger_effects_from_facts(
    facts: ir::NativeFacts,
    effects: &mut Vec<ir::TriggerEffect>,
) {
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
            | ir::CatalogOp::AttachPartition(_)
            | ir::CatalogOp::DetachPartition(_)
            | ir::CatalogOp::AttachInherit(_)
            | ir::CatalogOp::DetachInherit(_)
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

pub(crate) fn trigger_statement_effects(
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

pub(crate) fn trigger_block_effects(
    block: &ConditionalStatementBlock,
    context: AnalysisContext<'_>,
    effects: &mut Vec<ir::TriggerEffect>,
) {
    for statement in block.statements() {
        trigger_statement_effects(statement, context, effects);
    }
}

pub(crate) fn trigger_when_reads(
    condition: Option<&Expr>,
    context: AnalysisContext<'_>,
) -> Vec<ir::ReadFact> {
    let Some(condition) = condition else {
        return Vec::new();
    };
    let mut visitor = AnalysisVisitor::new(context);
    let _ = condition.visit(&mut visitor);
    visitor_reads(&visitor, &[], ir::ReadMode::Data)
}

pub(crate) fn trigger_body(
    trigger: &CreateTrigger,
    context: AnalysisContext<'_>,
) -> ir::TriggerBody {
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

pub(crate) fn context_scope(scope: Option<ContextModifier>) -> ir::SessionScope {
    match scope {
        Some(ContextModifier::Local) => ir::SessionScope::Local,
        Some(ContextModifier::Global) => ir::SessionScope::Global,
        Some(ContextModifier::Session) | None => ir::SessionScope::Session,
    }
}

pub(crate) fn pragma_bool(value: &ValueWithSpan) -> Option<bool> {
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

/// Session value of an inert-pragma assignment. Only concrete literals are
/// representable; placeholders and exotic values stay fenced.
pub(crate) fn pragma_session_value(value: &ValueWithSpan) -> Option<ir::SessionValue> {
    match &value.value {
        Value::Boolean(value) => Some(ir::SessionValue::Bool { value: *value }),
        Value::Number(text, _) => text
            .parse::<i64>()
            .ok()
            .map(|value| ir::SessionValue::Integer { value }),
        Value::SingleQuotedString(text)
        | Value::DoubleQuotedString(text)
        | Value::TripleSingleQuotedString(text)
        | Value::TripleDoubleQuotedString(text) => Some(ir::SessionValue::Text {
            value: text.clone(),
        }),
        _ => None,
    }
}

pub(crate) fn expr_bool(expr: &Expr) -> Option<bool> {
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

pub(crate) fn expr_value(expr: &Expr) -> ir::SessionValue {
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

pub(crate) fn expr_name(expr: &Expr, rules: &ir::IdentifierRules) -> Option<ir::Name> {
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

pub(crate) fn search_path_name(value: &str, quoted: bool, rules: &ir::IdentifierRules) -> ir::Name {
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
pub(crate) fn literal_search_path_entries(
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
pub(crate) fn search_path_entries(
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

pub(crate) fn search_path_value(
    values: &[Expr],
    rules: &ir::IdentifierRules,
) -> Option<ir::SearchPathValue> {
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

/// Resolve the scope of a system-variable assignment (`foreign_keys`,
/// `autocommit`, `search_path`).
///
/// Only spellings that provably select the session's system variable map to
/// an effect: a bare name (the statement keyword, if any, selects the
/// scope), `@@name`/`@@session.name` for the session scope, and
/// `@@global.name` for the global scope (which provably leaves the current
/// session untouched, so the evaluator ignores it). Anything else stays
/// fenced: `@name` is a user variable rather than the system variable,
/// other qualifiers name custom variables, and a qualifier contradicting
/// an explicit `GLOBAL`/`SESSION`/`LOCAL` keyword cannot be resolved.
pub(crate) fn system_var_scope(
    name: &ir::Name,
    keyword: Option<ContextModifier>,
) -> Option<ir::SessionScope> {
    match name.0.as_slice() {
        [single] => {
            let single = single.0.as_str();
            let system = single
                .strip_prefix("@@")
                .is_some_and(|rest| !rest.starts_with('@'))
                || !single.starts_with('@');
            system.then(|| context_scope(keyword))
        }
        [qualifier, ..] => {
            let qualifier = qualifier.0.as_str();
            if !qualifier.starts_with("@@") {
                return None;
            }
            match (
                qualifier
                    .trim_start_matches('@')
                    .to_ascii_lowercase()
                    .as_str(),
                keyword,
            ) {
                ("global", None | Some(ContextModifier::Global)) => Some(ir::SessionScope::Global),
                ("session", None | Some(ContextModifier::Session)) => {
                    Some(ir::SessionScope::Session)
                }
                _ => None,
            }
        }
        [] => None,
    }
}

pub(crate) fn set_effects(
    variable: &ObjectName,
    values: &[Expr],
    keyword: Option<ContextModifier>,
    rules: &ir::IdentifierRules,
) -> (Vec<ir::SessionEffect>, Vec<ir::ScopedBarrier>) {
    let scope = context_scope(keyword);
    let Some(name) = name_from_object(variable, rules, ir::IdentifierClass::Other) else {
        return (
            Vec::new(),
            vec![global_barrier(
                ir::BarrierReason::UnknownSessionEffect,
                Some(variable.to_string()),
            )],
        );
    };
    let last = name
        .last()
        .map(|ident| ident.0.to_ascii_lowercase())
        .unwrap_or_default();
    let variable_name = last.trim_start_matches('@');
    match variable_name {
        "search_path" => {
            let Some(scope) = system_var_scope(&name, keyword) else {
                return (
                    Vec::new(),
                    vec![global_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some("SET search_path with an unmodeled scope or qualifier".to_string()),
                    )],
                );
            };
            match search_path_value(values, rules) {
                Some(path) => (
                    vec![ir::SessionEffect::SetSearchPath { scope, path }],
                    Vec::new(),
                ),
                None => (
                    Vec::new(),
                    vec![global_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some("SET search_path with a non-name value".to_string()),
                    )],
                ),
            }
        }
        "foreign_keys" | "foreign_key_checks" => {
            let Some(value) = values.first().and_then(expr_bool) else {
                return (
                    Vec::new(),
                    vec![global_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some("SET foreign_keys with a non-boolean value".to_string()),
                    )],
                );
            };
            let Some(scope) = system_var_scope(&name, keyword) else {
                return (
                    Vec::new(),
                    vec![global_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some("SET foreign_keys with an unmodeled scope or qualifier".to_string()),
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
                    vec![global_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some("SET autocommit with a non-boolean value".to_string()),
                    )],
                );
            };
            let Some(scope) = system_var_scope(&name, keyword) else {
                return (
                    Vec::new(),
                    vec![global_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some("SET autocommit with an unmodeled scope or qualifier".to_string()),
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

pub(crate) fn set_statement_effects(
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
            let (mut set_effects, mut set_barriers) = set_effects(variable, values, *scope, rules);
            effects.append(&mut set_effects);
            barriers.append(&mut set_barriers);
        }
        Set::ParenthesizedAssignments {
            variables, values, ..
        } => {
            for (variable, value) in variables.iter().zip(values.iter()) {
                let (mut set_effects, mut set_barriers) =
                    set_effects(variable, std::slice::from_ref(value), None, rules);
                effects.append(&mut set_effects);
                barriers.append(&mut set_barriers);
            }
        }
        Set::MultipleAssignments { assignments } => {
            for assignment in assignments {
                let (mut set_effects, mut set_barriers) = set_effects(
                    &assignment.name,
                    std::slice::from_ref(&assignment.value),
                    assignment.scope,
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
                    barriers.push(global_barrier(
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
                    barriers.push(global_barrier(
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
                barriers.push(global_barrier(
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
                    TransactionMode::IsolationLevel(level) => barriers.push(global_barrier(
                        ir::BarrierReason::UnsupportedTransaction,
                        Some(format!("ISOLATION LEVEL {level}")),
                    )),
                }
            }
        }
    }
    (effects, barriers, transactions)
}

pub(crate) fn reset_effects(
    reset: &Reset,
    rules: &ir::IdentifierRules,
) -> (Vec<ir::SessionEffect>, Vec<ir::ScopedBarrier>) {
    match reset {
        Reset::ConfigurationParameter(name) => {
            match name_from_object(name, rules, ir::IdentifierClass::Other) {
                Some(name) => {
                    // No real dialect accepts `@`/`@@` spellings in RESET
                    // (PostgreSQL takes a bare configuration parameter; MySQL
                    // has no `RESET var` form at all), so anything with `@`
                    // stays fenced even when the permissive parser accepts it.
                    if name.0.iter().any(|ident| ident.0.starts_with('@')) {
                        return (
                            Vec::new(),
                            vec![global_barrier(
                                ir::BarrierReason::UnknownSessionEffect,
                                Some("RESET with an unmodeled variable spelling".to_string()),
                            )],
                        );
                    }
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
                    vec![global_barrier(
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

pub(crate) fn use_effect(
    use_statement: &Use,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> (Vec<ir::SessionEffect>, Vec<ir::ScopedBarrier>) {
    match use_statement {
        Use::Object(name) | Use::Schema(name) | Use::Database(name) | Use::Catalog(name) => {
            let namespace = name_parts(name, rules, ir::IdentifierClass::Relation).filter(
                |parts| match dialect {
                    DialectKind::MySql | DialectKind::ClickHouse | DialectKind::Hive => {
                        parts.len() == 1 && !matches!(use_statement, Use::Catalog(_))
                    }
                    DialectKind::DuckDB => {
                        parts.len() == 2 && matches!(use_statement, Use::Object(_))
                    }
                    DialectKind::Snowflake => {
                        parts.len() == 2 && matches!(use_statement, Use::Object(_) | Use::Schema(_))
                    }
                    DialectKind::Databricks | DialectKind::Spark => {
                        parts.len() == 2 && !matches!(use_statement, Use::Catalog(_))
                    }
                    _ => false,
                },
            );
            match namespace {
                Some(namespace) => (
                    vec![ir::SessionEffect::SetNamespace {
                        scope: ir::SessionScope::Session,
                        namespace,
                    }],
                    Vec::new(),
                ),
                None => (
                    Vec::new(),
                    vec![global_barrier(
                        ir::BarrierReason::UnknownSessionEffect,
                        Some(name.to_string()),
                    )],
                ),
            }
        }
        Use::Default => (
            Vec::new(),
            vec![global_barrier(
                ir::BarrierReason::UnknownSessionEffect,
                Some("unmodeled default namespace".to_string()),
            )],
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
            vec![global_barrier(
                ir::BarrierReason::UnknownSessionEffect,
                Some("USE SECONDARY ROLES".to_string()),
            )],
        ),
    }
}

pub(crate) fn begin_policy(dialect: DialectKind) -> ir::BeginPolicy {
    match dialect {
        DialectKind::MySql => ir::BeginPolicy::CommitActive,
        DialectKind::SQLite => ir::BeginPolicy::Error,
        DialectKind::MsSql => ir::BeginPolicy::Ignore,
        _ => ir::BeginPolicy::Warn,
    }
}

pub(crate) fn implicit_commit(dialect: DialectKind, statement: &Statement) -> bool {
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

pub(crate) fn drop_mode(
    object_type: &ObjectType,
    cascade: bool,
    dialect: DialectKind,
) -> ir::DropMode {
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

pub(crate) fn trigger_ref_for_drop(
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

pub(crate) fn drop_object_for(
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
            name: name_from_object(name, rules, ir::IdentifierClass::Relation)?,
        }),
        _ => None,
    }
}

/// Same-table columns read by a plain `ALTER COLUMN` op. Every
/// `AlterColumnOperation` variant is classified (exhaustive match: a new
/// upstream variant fails to compile until it is reviewed here):
/// - `SET DEFAULT` / `SET DATA TYPE ... USING`: the expression's columns.
/// - `ADD GENERATED ... AS IDENTITY`: whatever `Expr`-position columns the
///   sequence options carry (numeric by convention, so usually none).
/// - `SET`/`DROP NOT NULL`, `DROP DEFAULT`, foreign-table `OPTIONS`: the
///   target column only (option keys/values are FDW names, never
///   same-table columns), so no referenced columns.
pub(crate) fn alter_op_columns(
    op: &AlterColumnOperation,
    rules: &ir::IdentifierRules,
) -> Vec<ir::Ident> {
    match op {
        AlterColumnOperation::SetDefault { value } => expr_columns(value, rules),
        AlterColumnOperation::SetDataType { using, .. } => using
            .as_ref()
            .map(|using| expr_columns(using, rules))
            .unwrap_or_default(),
        AlterColumnOperation::AddGenerated {
            sequence_options, ..
        } => {
            let mut referenced = Vec::new();
            let mut seen = HashSet::new();
            for option in sequence_options.iter().flatten() {
                for column in visit_columns(option, rules) {
                    if seen.insert(column.clone()) {
                        referenced.push(column);
                    }
                }
            }
            referenced
        }
        AlterColumnOperation::SetNotNull
        | AlterColumnOperation::DropNotNull
        | AlterColumnOperation::DropDefault
        | AlterColumnOperation::GenericOptions { .. } => Vec::new(),
    }
}

/// Build the IR change for a MySQL `CHANGE`/`MODIFY COLUMN` rewrite from
/// its exposed pieces (target column, new-definition options, column
/// position). An unclassifiable definition (unparsed dialect tokens, or a
/// self-referencing inline foreign key with implicit parent columns)
/// degrades to `Other`, keeping the previous whole-table fence.
pub(crate) fn change_modify_change(
    column: ir::Ident,
    options: &[ColumnOption],
    column_position: &Option<MySQLColumnPosition>,
    detail: &str,
    table: &ir::NameRef,
    rules: &ir::IdentifierRules,
) -> ir::AlterChange {
    match change_modify_refs(options, table, rules) {
        Some(referenced) => ir::AlterChange::AlterColumn {
            column,
            detail: detail.to_string(),
            referenced,
            repositions: column_position.is_some(),
        },
        None => ir::AlterChange::Other {
            detail: detail.to_string(),
        },
    }
}

/// Same-table columns read by a `CHANGE`/`MODIFY` new definition, or
/// `None` when the definition is opaque. `Expr`-position references
/// (`DEFAULT`, `CHECK`, generated expressions, `ON UPDATE`, ...) are
/// collected by [`visit_columns`]; the match below handles the only
/// same-table references that live outside `Expr`, and is exhaustive so a
/// new upstream `ColumnOption` variant fails to compile until reviewed:
/// - inline `FOREIGN KEY`: child columns name this table; parent columns
///   fence only when the parent may be this table (same trailing name,
///   covering sibling spellings), and implicit parent columns are then
///   unnameable (opaque, like `ADD CONSTRAINT` self-references);
/// - masking/projection policy `USING` columns name this table's columns;
/// - `DialectSpecific` is unparsed `Token` text (e.g. MySQL
///   `AUTO_INCREMENT`), so absence of column references is unprovable.
///
/// All other variants reference columns only through `Expr` (or not at
/// all: bare names there are constraint/charset/policy/tag names).
pub(crate) fn change_modify_refs(
    options: &[ColumnOption],
    table: &ir::NameRef,
    rules: &ir::IdentifierRules,
) -> Option<Vec<ir::Ident>> {
    let mut referenced = Vec::new();
    let mut seen = HashSet::new();
    let mut push = |column: ir::Ident| {
        if seen.insert(column.clone()) {
            referenced.push(column);
        }
    };
    for option in options {
        for column in visit_columns(option, rules) {
            push(column);
        }
        match option {
            ColumnOption::ForeignKey(key) => {
                for column in &key.columns {
                    push(canonical_ident(column, rules, ir::IdentifierClass::Column));
                }
                if alter_parent_may_be_self(&key.foreign_table, table, rules) {
                    if key.referred_columns.is_empty() {
                        return None;
                    }
                    for column in &key.referred_columns {
                        push(canonical_ident(column, rules, ir::IdentifierClass::Column));
                    }
                }
            }
            ColumnOption::Policy(policy) => {
                let using = match policy {
                    ColumnPolicy::MaskingPolicy(property)
                    | ColumnPolicy::ProjectionPolicy(property) => &property.using_columns,
                };
                for column in using.iter().flatten() {
                    push(canonical_ident(column, rules, ir::IdentifierClass::Column));
                }
            }
            ColumnOption::DialectSpecific(_) => return None,
            ColumnOption::Null
            | ColumnOption::NotNull
            | ColumnOption::Default(_)
            | ColumnOption::Materialized(_)
            | ColumnOption::Ephemeral(_)
            | ColumnOption::Alias(_)
            | ColumnOption::PrimaryKey(_)
            | ColumnOption::Unique(_)
            | ColumnOption::Check(_)
            | ColumnOption::CharacterSet(_)
            | ColumnOption::Collation(_)
            | ColumnOption::Comment(_)
            | ColumnOption::OnUpdate(_)
            | ColumnOption::Generated { .. }
            | ColumnOption::Options(_)
            | ColumnOption::GenericOptions(_)
            | ColumnOption::Identity(_)
            | ColumnOption::OnConflict(_)
            | ColumnOption::Tags(_)
            | ColumnOption::Srid(_)
            | ColumnOption::Invisible => {}
        }
    }
    Some(referenced)
}

/// Whether an inline foreign-key parent may be the altered table itself:
/// identical trailing names after relation folding, which also covers
/// sibling spellings (`t` vs `main.t`) that may alias. An unresolvable
/// parent name conservatively counts as self.
pub(crate) fn alter_parent_may_be_self(
    parent: &ObjectName,
    table: &ir::NameRef,
    rules: &ir::IdentifierRules,
) -> bool {
    match name_parts(parent, rules, ir::IdentifierClass::Relation) {
        Some(parts) => parts.last() == table.name.0.last(),
        None => true,
    }
}

pub(crate) fn alter_change(
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
                barriers.push(global_barrier(
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
                referenced: alter_op_columns(op, rules),
                // Plain `ALTER COLUMN` never repositions: no `FIRST`/`AFTER`
                // exists for it, so ordinals and arity are unchanged.
                repositions: false,
            });
        }
        AlterTableOperation::ChangeColumn {
            old_name,
            new_name,
            options,
            column_position,
            ..
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
            changes.push(change_modify_change(
                new_name,
                options,
                column_position,
                &operation.to_string(),
                table,
                rules,
            ));
        }
        AlterTableOperation::ModifyColumn {
            col_name,
            options,
            column_position,
            ..
        } => {
            changes.push(change_modify_change(
                canonical_ident(col_name, rules, ir::IdentifierClass::Column),
                options,
                column_position,
                &operation.to_string(),
                table,
                rules,
            ));
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
                barriers.push(global_barrier(
                    ir::BarrierReason::UnknownAlter,
                    Some(operation.to_string()),
                ));
            }
        }
        AlterTableOperation::Refresh { .. } => {
            ops.push(ir::CatalogOp::Refresh(ir::RefreshOp {
                target: table.clone(),
                source_mode: ir::ReadMode::Data,
            }));
        }
        AlterTableOperation::ValidateConstraint { name, .. } => {
            changes.push(ir::AlterChange::ValidateConstraint {
                name: canonical_ident(name, rules, ir::IdentifierClass::Other),
            });
        }
        AlterTableOperation::AttachPartitionOf { partition, .. } => {
            // The `FOR VALUES` bound fixes row placement, not table-level
            // dependencies, so it is parsed and ignored like `PARTITION OF`
            // bounds. The link itself is the fact.
            match relation_ref(partition, dialect, rules) {
                Some(child) => ops.push(ir::CatalogOp::AttachPartition(ir::PartitionLink {
                    parent: table.clone(),
                    child,
                })),
                None => barriers.push(global_barrier(
                    ir::BarrierReason::UnknownAlter,
                    Some(operation.to_string()),
                )),
            }
        }
        AlterTableOperation::DetachPartitionOf { partition, .. } => {
            // `CONCURRENTLY` / `FINALIZE` only change how the detach executes,
            // not the routing outcome, so the mode is ignored.
            match relation_ref(partition, dialect, rules) {
                Some(child) => ops.push(ir::CatalogOp::DetachPartition(ir::PartitionLink {
                    parent: table.clone(),
                    child,
                })),
                None => barriers.push(global_barrier(
                    ir::BarrierReason::UnknownAlter,
                    Some(operation.to_string()),
                )),
            }
        }
        AlterTableOperation::Inherit { parent } => {
            // Unlike `ATTACH PARTITION` (named child, target parent), the
            // `INHERIT` target is the child and the operand is the parent.
            match relation_ref(parent, dialect, rules) {
                Some(parent) => ops.push(ir::CatalogOp::AttachInherit(ir::PartitionLink {
                    parent,
                    child: table.clone(),
                })),
                None => barriers.push(global_barrier(
                    ir::BarrierReason::UnknownAlter,
                    Some(operation.to_string()),
                )),
            }
        }
        AlterTableOperation::NoInherit { parent } => match relation_ref(parent, dialect, rules) {
            Some(parent) => ops.push(ir::CatalogOp::DetachInherit(ir::PartitionLink {
                parent,
                child: table.clone(),
            })),
            None => barriers.push(global_barrier(
                ir::BarrierReason::UnknownAlter,
                Some(operation.to_string()),
            )),
        },
        // ClickHouse `ATTACH/DETACH PARTITION` (a partition expression, not a
        // table name) and every other operation keep the blanket fence.
        _ => barriers.push(global_barrier(
            ir::BarrierReason::UnknownAlter,
            Some(operation.to_string()),
        )),
    }
    (changes, ops, barriers)
}

/// Sequence effects caused by evaluating a statement. Definition-time
/// expressions (`CREATE TABLE ... DEFAULT nextval(...)`, view definitions,
/// index expressions) are not evaluated. `ALTER TABLE ... ADD COLUMN ...
/// DEFAULT` is evaluated for existing rows and therefore does advance the
/// sequence; `ALTER COLUMN ... SET DEFAULT` does not.
pub(crate) fn sequence_effects_for_statement<'a>(
    statement: &Statement,
    dialect: DialectKind,
    rules: &'a ir::IdentifierRules,
    pure_table_enabled: bool,
) -> SequenceVisitor<'a> {
    let mut visitor = SequenceVisitor::new(dialect, rules, pure_table_enabled);
    if pure_table_enabled
        && matches!(dialect, DialectKind::MySql)
        && let Statement::Insert(insert) = statement
        && let Some(OnInsert::DuplicateKeyUpdate(assignments)) = &insert.on
    {
        // `VALUES(col)` in this position is the proposed-row marker, not a
        // routine call: exclude it from the fence. Shadowed scripts
        // (`pure_table_enabled == false`) keep fencing, since `VALUES` may
        // then name a user routine.
        visitor
            .ignored_function_roots
            .extend(values_marker_spans(assignments));
    }
    match statement {
        Statement::Query(_)
        | Statement::Insert(_)
        | Statement::Update(_)
        | Statement::Delete(_)
        | Statement::Merge(_)
        | Statement::Call(_)
        | Statement::Copy { .. } => {
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
    visitor
}

pub(crate) fn analyze_statement(
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
    if let Statement::CreateTable(create) = statement
        && let Some(partition_by) = &create.partition_by
    {
        // The partitioning scheme lowers its keyword (`RANGE`/`LIST`/`HASH`)
        // as `Function`, but it names no routine and creates no dependency
        // edge, so the whole scheme subtree is excluded from the
        // deferred-function check.
        visitor.ignored_function_roots.push(partition_by.span());
    }
    if matches!(dialect, DialectKind::MySql) {
        // MySQL index length prefixes (`col(N)`) lower as `Expr::Function`
        // but name no routine; only the prefix shape in key position is
        // excluded, and only on MySQL, where the production exists. On every
        // other dialect `b(1)` in key position keeps fencing as a call.
        match statement {
            Statement::CreateTable(create) => {
                for constraint in &create.constraints {
                    ignore_index_length_prefixes(
                        &mut visitor.ignored_function_roots,
                        index_constraint_columns(constraint),
                    );
                }
            }
            Statement::CreateIndex(create) => {
                ignore_index_length_prefixes(&mut visitor.ignored_function_roots, &create.columns);
            }
            Statement::AlterTable(alter) => {
                for operation in &alter.operations {
                    if let AlterTableOperation::AddConstraint { constraint, .. } = operation {
                        ignore_index_length_prefixes(
                            &mut visitor.ignored_function_roots,
                            index_constraint_columns(constraint),
                        );
                    }
                }
            }
            _ => {}
        }
    }
    let _ = statement.visit(&mut visitor);
    let mut facts = ir::NativeFacts::default();
    // Set only by the inert-pragma branch below, whose recorded variables are
    // provably irrelevant to dependencies and need no fence. Every other
    // variable effect keeps the blanket barrier.
    let mut inert_pragma_effects = false;
    // Sequence value changes survive rollback in PostgreSQL/Redshift even
    // though creation and removal of the sequence itself are transactional.
    let transactional = sequence_transactional(dialect)
        && !matches!(dialect, DialectKind::PostgreSql | DialectKind::Redshift);
    let sequences =
        sequence_effects_for_statement(statement, dialect, rules, context.pure_table_enabled);
    for sequence in sequences.writes {
        facts
            .ops
            .push(ir::CatalogOp::WriteSequence(ir::SequenceWrite {
                sequence,
                transactional,
            }));
    }
    for sequence in sequences.reads {
        facts.ops.push(ir::CatalogOp::Read(read_fact(
            sequence,
            ir::ReadMode::Data,
            Vec::new(),
            false,
        )));
    }
    // Stored expressions are not propagated by the catalog model. Fence their
    // later execution without pretending a definition executes them now.
    let deferred_function = visitor.has_function
        && !matches!(
            statement,
            Statement::Query(_)
                | Statement::Insert(_)
                | Statement::Update(_)
                | Statement::Delete(_)
                | Statement::Merge(_)
                | Statement::Call(_)
                | Statement::Copy { .. }
                | Statement::Explain { .. }
        );
    if sequences.unknown || deferred_function {
        facts.barriers.push(global_barrier(
            ir::BarrierReason::UnknownStatement,
            Some("unmodeled routine or deferred expression effects".to_string()),
        ));
    }
    if visitor.select_into_count > 0
        && !matches!(statement, Statement::Query(_) | Statement::Explain { .. })
    {
        facts.barriers.push(global_barrier(
            ir::BarrierReason::UnknownDml,
            Some("SELECT INTO outside a standalone query".to_string()),
        ));
    }
    // MSSQL `OUTPUT ... INTO`: the output rows are additionally inserted
    // into the target. A real table becomes a second write edge (emitted
    // after the match so it lands after the host write); a table variable
    // has no catalog entry and is fenced at relation scope; `OUTPUT`
    // without `INTO` is a read-only projection with no extra edge.
    let output = match statement {
        Statement::Insert(insert) => insert.output.as_ref(),
        Statement::Update(update) => update.output.as_ref(),
        Statement::Delete(delete) => delete.output.as_ref(),
        Statement::Merge(merge) => merge.output.as_ref(),
        _ => None,
    };
    if implicit_commit(dialect, statement) {
        // The statement runs in its own implicit transaction: close any active
        // transaction before it and the implicit one after it.
        facts.pre_tx.push(ir::TxEffect::Commit { chain: false });
        facts.post_tx.push(ir::TxEffect::Commit { chain: false });
    }
    // Column-reduction geometry (response-level assembly input) plus the raw
    // single-point sites. Collected only when the column gate is on; nested
    // analyses run with the gate off, so each unit holds at most one shape.
    let mut column_unit = crate::columns::ColumnUnit::default();
    let mut column_singles: Vec<(usize, usize, &'static str)> = Vec::new();
    match statement {
        Statement::Query(query) => {
            let nested = collect_dml_in_query(query);
            let reads = visitor_reads(&visitor, &[], ir::ReadMode::Data);
            let into_table = match query.body.as_ref() {
                SetExpr::Select(select) if visitor.select_into_count == 1 => {
                    select_into_definition(select, dialect, rules)
                }
                _ => None,
            };
            if let Some(mut table) = into_table {
                table.sources = reads;
                facts.ops.push(ir::CatalogOp::DefineTable(table));
            } else {
                if visitor.select_into_count > 0 {
                    facts.barriers.push(global_barrier(
                        ir::BarrierReason::UnknownDml,
                        Some(
                            "unmodeled SELECT INTO target, dialect, or nested context".to_string(),
                        ),
                    ));
                }
                add_reads(&mut facts, reads);
            }
            add_unmodeled_barriers(&mut facts, &visitor, &[]);
            merge_nested(&mut facts, nested, context);
        }
        Statement::CreateTable(create) => {
            // NOTE: PostgreSQL `CREATE TABLE` manual: https://www.postgresql.org/docs/current/sql-createtable.html
            // MySQL `CREATE TABLE` manual: https://dev.mysql.com/doc/refman/8.0/en/create-table.html
            // PostgreSQL partitioning and inheritance route child rows
            // through the parent. The attachment is a fact (`partition_parent`
            // / `inherit_parents`); the graph builder expands parent reads
            // over the children and records parent writes on them (`INSERT`
            // fans out to partition children only; plain `INHERITS` inserts
            // land parent-only). `FOR
            // VALUES` bounds and the `PARTITION BY` scheme are parsed and
            // ignored: they fix row placement (which child holds a row), but
            // every edge in the model is table-level, so no edge needs them.
            // Bound expressions are still visited, so routine shapes inside
            // bounds keep their existing fences; only the scheme keyword
            // (which lowers as `Function`) is structurally excluded from the
            // deferred-function check (see `ignored_function_roots`).
            let mut unmodeled_parent = false;
            let partition_parent = create.partition_of.as_ref().and_then(|parent| {
                relation_ref(parent, dialect, rules).or_else(|| {
                    unmodeled_parent = true;
                    None
                })
            });
            let mut inherit_parents = Vec::new();
            if let Some(inherits) = &create.inherits {
                for parent in inherits {
                    match relation_ref(parent, dialect, rules) {
                        Some(reference) => inherit_parents.push(reference),
                        None => unmodeled_parent = true,
                    }
                }
            }
            if unmodeled_parent {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDdl,
                    Some("unmodeled inherited or partitioned table effects".to_string()),
                ));
            }
            let with_data = create
                .with_data
                .as_ref()
                .is_none_or(|with_data| with_data.data);
            let nested = if with_data {
                create
                    .query
                    .as_ref()
                    .map(|query| collect_dml_in_query(query))
                    .unwrap_or_default()
            } else {
                Vec::new()
            };
            let excluded: Vec<Span> = vec![create.name.span()];
            let source_mode = if with_data {
                ir::ReadMode::Data
            } else {
                ir::ReadMode::Schema
            };
            let mut sources = visitor_reads(&visitor, &excluded, source_mode);
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
                sources.push(read_fact(reference, ir::ReadMode::Data, Vec::new(), false));
            }
            // The child definition requires its parents to exist.
            for reference in partition_parent
                .iter()
                .chain(inherit_parents.iter())
                .cloned()
            {
                sources.push(read_fact(
                    reference,
                    ir::ReadMode::Schema,
                    Vec::new(),
                    false,
                ));
            }
            if let Some(table) = table_definition(
                create,
                dialect,
                rules,
                sources,
                like,
                partition_parent,
                inherit_parents,
            ) {
                for constraint in &table.constraints {
                    if let ir::Constraint::ForeignKey(reference) = constraint {
                        facts.ops.push(ir::CatalogOp::Read(read_fact(
                            reference.parent.clone(),
                            ir::ReadMode::Schema,
                            reference.parent_columns.clone(),
                            false,
                        )));
                    }
                }
                for column in &table.columns {
                    for reference in &column.references {
                        facts.ops.push(ir::CatalogOp::Read(read_fact(
                            reference.parent.clone(),
                            ir::ReadMode::Schema,
                            reference.parent_columns.clone(),
                            false,
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
            if context.column_ir {
                let mut columns = Vec::with_capacity(create.columns.len());
                for column in &create.columns {
                    let name = canonical_ident(&column.name, rules, ir::IdentifierClass::Column);
                    let span = visitor.column_def_span(column);
                    if let Some((start, end)) = span {
                        column_singles.push((start, end, "remove-column_create_def"));
                    }
                    columns.push((name, span));
                }
                column_unit
                    .creates
                    .push(crate::columns::CreateColumns { columns });
            }
        }
        Statement::CreateView(create) => {
            // NOTE: PostgreSQL `CREATE VIEW` manual: https://www.postgresql.org/docs/current/sql-createview.html
            // MySQL `CREATE VIEW` manual: https://dev.mysql.com/doc/refman/8.0/en/create-view.html
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
                if create.to.is_some() {
                    facts.barriers.push(global_barrier(
                        ir::BarrierReason::WriteThroughView,
                        Some("unmodeled persistent CREATE VIEW ... TO routing".to_string()),
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
            // NOTE: PostgreSQL `CREATE INDEX` manual: https://www.postgresql.org/docs/current/sql-createindex.html
            // MySQL `CREATE INDEX` manual: https://dev.mysql.com/doc/refman/8.0/en/create-index.html
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
                        .filter_map(|column| index_column_ident(column, dialect, rules))
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
                            .any(|column| index_column_ident(column, dialect, rules).is_none()),
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
            // NOTE: PostgreSQL `CREATE TRIGGER` manual: https://www.postgresql.org/docs/current/sql-createtrigger.html
            // MySQL `CREATE TRIGGER` manual: https://dev.mysql.com/doc/refman/8.0/en/create-trigger.html
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
            // NOTE: PostgreSQL `DROP` manual: https://www.postgresql.org/docs/current/sql-droptable.html
            // MySQL `DROP TABLE` manual: https://dev.mysql.com/doc/refman/8.0/en/drop-table.html
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
            // NOTE: PostgreSQL `ALTER TABLE` manual: https://www.postgresql.org/docs/current/sql-altertable.html
            // MySQL `ALTER TABLE` manual: https://dev.mysql.com/doc/refman/8.0/en/alter-table.html
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
            match &alter_view.operation {
                AlterViewOperation::As { columns, query, .. } => {
                    let excluded = [view_name.span()];
                    let sources = visitor_reads(&visitor, &excluded, ir::ReadMode::Schema);
                    if let Some((name, temporary)) =
                        definition_name(view_name, dialect, rules, false)
                    {
                        let columns = if columns.is_empty() {
                            query_output_columns(query, rules).unwrap_or_default()
                        } else {
                            columns
                                .iter()
                                .map(|column| {
                                    canonical_ident(column, rules, ir::IdentifierClass::Column)
                                })
                                .collect()
                        };
                        facts.ops.push(ir::CatalogOp::DefineView(ir::ViewDef {
                            name,
                            temporary,
                            materialized: alter_view.materialized,
                            columns,
                            sources,
                            if_not_exists: false,
                            or_replace: true,
                        }));
                        add_unmodeled_barriers(&mut facts, &visitor, &excluded);
                    } else {
                        facts.barriers.push(global_barrier(
                            ir::BarrierReason::UnknownView,
                            Some(statement.to_string()),
                        ));
                    }
                }
                AlterViewOperation::RenameTo { new_name } => {
                    match (
                        relation_ref(view_name, dialect, rules),
                        name_from_object(new_name, rules, ir::IdentifierClass::Relation),
                    ) {
                        (Some(from), Some(to)) => {
                            facts
                                .ops
                                .push(ir::CatalogOp::Rename(ir::RenameOp::Relation {
                                    from,
                                    to,
                                    index_schema_placement: index_schema_placement(dialect),
                                }))
                        }
                        _ => facts.barriers.push(global_barrier(
                            ir::BarrierReason::UnknownView,
                            Some(statement.to_string()),
                        )),
                    }
                }
                AlterViewOperation::SetSchema { schema_name } => {
                    let target = relation_ref(view_name, dialect, rules).and_then(|from| {
                        let mut to =
                            name_from_object(schema_name, rules, ir::IdentifierClass::Relation)?;
                        to.0.push(last_ident(&from.name)?);
                        Some((from, to))
                    });
                    match target {
                        Some((from, to)) => {
                            facts
                                .ops
                                .push(ir::CatalogOp::Rename(ir::RenameOp::Relation {
                                    from,
                                    to,
                                    index_schema_placement: index_schema_placement(dialect),
                                }));
                        }
                        None => facts.barriers.push(global_barrier(
                            ir::BarrierReason::UnknownView,
                            Some(statement.to_string()),
                        )),
                    }
                }
                AlterViewOperation::RenameColumn {
                    old_column_name,
                    new_column_name,
                } => {
                    if let Some(table) = relation_ref(view_name, dialect, rules) {
                        facts.ops.push(ir::CatalogOp::Rename(ir::RenameOp::Column {
                            table,
                            from: canonical_ident(
                                old_column_name,
                                rules,
                                ir::IdentifierClass::Column,
                            ),
                            to: canonical_ident(
                                new_column_name,
                                rules,
                                ir::IdentifierClass::Column,
                            ),
                        }));
                    } else {
                        facts.barriers.push(global_barrier(
                            ir::BarrierReason::UnknownView,
                            Some(statement.to_string()),
                        ));
                    }
                }
                _ => facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownView,
                    Some(statement.to_string()),
                )),
            }
        }
        Statement::Insert(insert)
            if insert.overwrite
                || insert.partitioned.is_some()
                || insert.multi_table_insert_type.is_some()
                || !insert.multi_table_into_clauses.is_empty()
                || !insert.multi_table_when_clauses.is_empty()
                || insert.multi_table_else_clause.is_some() =>
        {
            add_reads(&mut facts, visitor_reads(&visitor, &[], ir::ReadMode::Data));
            add_unmodeled_barriers(&mut facts, &visitor, &[]);
            facts.barriers.push(global_barrier(
                ir::BarrierReason::UnknownDml,
                Some("unmodeled overwrite, partition, or multi-table INSERT".to_string()),
            ));
            if context.column_ir {
                let (info, mut singles) =
                    insert_column_info(&visitor, insert, dialect, rules, true);
                column_unit.inserts.push(info);
                column_singles.append(&mut singles);
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
                    let mut reads = visitor_reads(&visitor, &excluded, ir::ReadMode::Data);
                    match &insert.on {
                        Some(OnInsert::OnConflict(conflict)) => {
                            merge_conflict_reads(&mut reads, &target, conflict, rules);
                        }
                        Some(OnInsert::DuplicateKeyUpdate(_)) => {
                            merge_duplicate_reads(&mut reads, &target, insert, rules);
                        }
                        None | Some(_) => {}
                    }
                    facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                        target,
                        kind: ir::WriteKind::Insert {
                            conflict: insert_conflict(insert),
                        },
                        columns: insert_columns(insert, rules),
                        unqualified_columns: Vec::new(),
                        reads,
                        // `INSERT` targets carry no `ONLY` marker.
                        only: false,
                    }));
                    add_unmodeled_barriers(&mut facts, &visitor, &excluded);
                    merge_nested(&mut facts, nested, context);
                }
                None => facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownDml,
                    Some(insert.table.to_string()),
                )),
            }
            if context.column_ir {
                let (info, mut singles) =
                    insert_column_info(&visitor, insert, dialect, rules, false);
                column_unit.inserts.push(info);
                column_singles.append(&mut singles);
            }
        }
        Statement::Update(update) => {
            let targets = update_targets(update, rules);
            let resolved: Vec<(ir::NameRef, Vec<ir::Ident>, Vec<ir::Ident>, bool)> = targets
                .iter()
                .filter_map(|target| {
                    relation_ref(&target.source.name, dialect, rules).map(|reference| {
                        (
                            reference,
                            target.columns.clone(),
                            target.unqualified_columns.clone(),
                            target.source.only,
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
                for (target, columns, unqualified_columns, only) in resolved {
                    facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                        target,
                        kind: ir::WriteKind::Update,
                        columns,
                        unqualified_columns,
                        reads: reads.clone(),
                        only,
                    }));
                }
                add_unmodeled_barriers(&mut facts, &visitor, &excluded);
                merge_nested(&mut facts, nested, context);
            }
        }
        Statement::Delete(delete) => {
            let targets = delete_targets(delete, rules);
            let resolved: Vec<(ir::NameRef, bool)> = targets
                .iter()
                .filter_map(|target| {
                    relation_ref(&target.source.name, dialect, rules)
                        .map(|reference| (reference, target.source.only))
                })
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
                for (target, only) in resolved {
                    facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                        target,
                        kind: ir::WriteKind::Delete {
                            all: delete.selection.is_none(),
                        },
                        columns: Vec::new(),
                        unqualified_columns: Vec::new(),
                        reads: reads.clone(),
                        only,
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
                        // `MERGE` targets carry no `ONLY` marker.
                        only: false,
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
            // NOTE: PostgreSQL `TRUNCATE` manual: https://www.postgresql.org/docs/current/sql-truncate.html
            // MySQL `TRUNCATE TABLE` manual: https://dev.mysql.com/doc/refman/8.0/en/truncate-table.html
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
                            only: table.only,
                        }));
                    }
                    None => facts.barriers.push(global_barrier(
                        ir::BarrierReason::UnknownTruncate,
                        Some(table.to_string()),
                    )),
                }
            }
        }
        Statement::Copy { source, to, .. } => {
            if *to {
                match source {
                    CopySource::Table {
                        table_name,
                        columns,
                    } => {
                        let col_idents = columns
                            .iter()
                            .map(|col| canonical_ident(col, rules, ir::IdentifierClass::Column))
                            .collect();
                        match relation_ref(table_name, dialect, rules) {
                            Some(target) => facts.ops.push(ir::CatalogOp::Read(read_fact(
                                target,
                                ir::ReadMode::Data,
                                col_idents,
                                false,
                            ))),
                            None => facts.barriers.push(global_barrier(
                                ir::BarrierReason::UnknownDml,
                                Some(table_name.to_string()),
                            )),
                        }
                    }
                    CopySource::Query(query) => {
                        let nested = collect_dml_in_query(query);
                        let reads = visitor_reads(&visitor, &[], ir::ReadMode::Data);
                        for read in reads {
                            facts.ops.push(ir::CatalogOp::Read(read));
                        }
                        add_unmodeled_barriers(&mut facts, &visitor, &[]);
                        merge_nested(&mut facts, nested, context);
                    }
                }
            } else {
                match source {
                    CopySource::Table {
                        table_name,
                        columns,
                    } => {
                        let col_idents = columns
                            .iter()
                            .map(|col| canonical_ident(col, rules, ir::IdentifierClass::Column))
                            .collect();
                        match relation_ref(table_name, dialect, rules) {
                            Some(target) => {
                                facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                                    target,
                                    kind: ir::WriteKind::Insert {
                                        conflict: ir::ConflictAction::Error,
                                    },
                                    columns: col_idents,
                                    unqualified_columns: Vec::new(),
                                    reads: Vec::new(),
                                    only: false,
                                }))
                            }
                            None => facts.barriers.push(global_barrier(
                                ir::BarrierReason::UnknownDml,
                                Some(table_name.to_string()),
                            )),
                        }
                    }
                    CopySource::Query(query) => {
                        facts.barriers.push(global_barrier(
                            ir::BarrierReason::UnknownDml,
                            Some(query.to_string()),
                        ));
                    }
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
            let (effects, barriers) = use_effect(use_statement, dialect, rules);
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
                        None => facts.barriers.push(global_barrier(
                            ir::BarrierReason::UnknownSessionEffect,
                            Some(statement.to_string()),
                        )),
                    }
                }
            } else if value.is_none()
                && matches!(pragma.as_str(), "recursive_triggers" | "defer_foreign_keys")
            {
                // A bare `PRAGMA recursive_triggers;` / `PRAGMA
                // defer_foreign_keys;` is a read-only flag query (like `PRAGMA
                // foreign_keys;`): it returns the current setting and mutates
                // nothing. Assignments stay fenced below (they change trigger
                // firing / FK enforcement timing).
            } else if matches!(
                pragma.as_str(),
                "journal_mode" | "synchronous" | "temp_store" | "cache_size" | "page_size"
            ) {
                // Storage/durability knobs: provably irrelevant to reads,
                // writes, trigger firing, and transaction visibility, so they
                // need no UnknownSessionEffect fence. A bare `PRAGMA
                // journal_mode;` is a read-only query (like `PRAGMA
                // foreign_keys;`); an assignment is recorded as an inert
                // session variable so the script keeps its session ordering
                // without serializing on a global fence.
                if let Some(assignment) = value {
                    match pragma_session_value(assignment) {
                        Some(session_value) => {
                            facts.session.push(ir::SessionEffect::SetVariable {
                                scope: ir::SessionScope::Session,
                                name: ir::Name(vec![ir::Ident(pragma.clone())]),
                                value: session_value,
                            });
                            inert_pragma_effects = true;
                        }
                        None => facts.barriers.push(global_barrier(
                            ir::BarrierReason::UnknownSessionEffect,
                            Some(statement.to_string()),
                        )),
                    }
                }
            } else {
                // Assignments to recursive_triggers/defer_foreign_keys and unknown
                // pragmas stay fenced. Boolean literals (`= true`) reach here as
                // `Number(1/0)` via the PRAGMA pre-normalization in parser.rs; the
                // upstream grammar gap is still recorded in TODO.org "Upstream Gaps".
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownSessionEffect,
                    Some(statement.to_string()),
                ));
            }
        }
        Statement::StartTransaction {
            statements, modes, ..
        } => {
            // NOTE: PostgreSQL `BEGIN` manual: https://www.postgresql.org/docs/current/sql-begin.html
            // MySQL `START TRANSACTION` manual: https://dev.mysql.com/doc/refman/8.0/en/commit.html
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
                            facts.barriers.push(global_barrier(
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
            // NOTE: PostgreSQL `COMMIT` manual: https://www.postgresql.org/docs/current/sql-commit.html
            // MySQL `COMMIT` manual: https://dev.mysql.com/doc/refman/8.0/en/commit.html
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
        Statement::Rollback { chain, savepoint } => {
            // NOTE: PostgreSQL `ROLLBACK` manual: https://www.postgresql.org/docs/current/sql-rollback.html
            // MySQL `ROLLBACK` manual: https://dev.mysql.com/doc/refman/8.0/en/commit.html
            match savepoint {
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
            }
        }
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
                    false,
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
        Statement::RefreshMaterializedView(refresh) => {
            // NOTE: PostgreSQL `REFRESH MATERIALIZED VIEW` manual: https://www.postgresql.org/docs/current/sql-refreshmaterializedview.html
            if let Some(target) = relation_ref(&refresh.name, dialect, rules) {
                facts.ops.push(ir::CatalogOp::Refresh(ir::RefreshOp {
                    target,
                    source_mode: if refresh.with_data == Some(false) {
                        ir::ReadMode::Schema
                    } else {
                        ir::ReadMode::Data
                    },
                }));
            } else {
                facts.barriers.push(global_barrier(
                    ir::BarrierReason::UnknownView,
                    Some(refresh.name.to_string()),
                ));
            }
        }
        Statement::Analyze(analyze) => {
            // NOTE: PostgreSQL `ANALYZE` manual: https://www.postgresql.org/docs/current/sql-analyze.html
            // `ANALYZE [ ( option [, ...] ) ] [ table_and_columns [, ...] ]`
            // Collects statistics for the database or the specified table.
            if let Some(name) = &analyze.table_name {
                if let Some(target) = relation_ref(name, dialect, rules) {
                    let cols = analyze
                        .columns
                        .iter()
                        .map(|col| canonical_ident(col, rules, ir::IdentifierClass::Column))
                        .collect();
                    facts.ops.push(ir::CatalogOp::Read(read_fact(
                        target.clone(),
                        ir::ReadMode::Data,
                        cols,
                        false,
                    )));
                    facts.barriers.push(barrier(
                        ir::BarrierScope::Relation { relation: target },
                        ir::BarrierReason::UnknownDdl,
                        Some(format!("ANALYZE on relation {}", name)),
                    ));
                } else {
                    facts.barriers.push(global_barrier(
                        ir::BarrierReason::UnknownRelation,
                        Some(name.to_string()),
                    ));
                }
            }
        }
        Statement::CreateDatabase {
            db_name,
            location: None,
            managed_location: None,
            or_replace: false,
            transient: false,
            clone: None,
            data_retention_time_in_days: None,
            max_data_extension_time_in_days: None,
            external_volume: None,
            catalog: None,
            replace_invalid_characters: None,
            default_ddl_collation: None,
            storage_serialization_policy: None,
            comment: None,
            default_charset: None,
            default_collation: None,
            catalog_sync: None,
            catalog_sync_namespace_mode: None,
            catalog_sync_namespace_flatten_delimiter: None,
            with_tags: None,
            with_contacts: None,
            ..
        } if dialect == DialectKind::MySql => {
            if let Some(name) = name_from_object(db_name, rules, ir::IdentifierClass::Relation) {
                facts
                    .ops
                    .push(ir::CatalogOp::DefineSchema(ir::SchemaDef { name }));
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
                    name_from_object(name, rules, ir::IdentifierClass::Relation)
                }
                SchemaName::NamedAuthorization(name, _) => {
                    name_from_object(name, rules, ir::IdentifierClass::Relation)
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
        // TODO(upstream-sqlparser): Procedural statements, client commands, and dialect extensions
        // currently hit this fallback barrier:
        // 1. PL/pgSQL & PL/SQL: `CREATE FUNCTION/PROCEDURE ... AS $$ ... $$` procedural blocks
        //    (sqlparser stores function bodies as unparsed strings; inner DML dependencies are opaque).
        // 2. MySQL: `DELIMITER //` client command is outside ANSI/ISO SQL lexical grammar.
        // 3. MySQL: User session variables (`@var := expr`) and dynamic `PREPARE .. FROM @var`.
        // 4. SQLite: `CREATE VIRTUAL TABLE ... USING module(...)` and hidden shadow tables (t_data, t_idx).
        // 5. DuckDB: Direct file queries (`SELECT * FROM 'file.parquet'`) and `CREATE MACRO`.
        // 6. MSSQL: T-SQL procedural batches (`DECLARE @x`, `IF..ELSE`). (`OUTPUT ... INTO`
        //    is modeled, not a fallback: real table -> second write edge, `@var` -> relation-
        //    scoped fence, anything else -> global `UnknownDml` fence.)
        _ => facts.barriers.push(global_barrier(
            ir::BarrierReason::UnknownStatement,
            Some(statement.to_string()),
        )),
    }
    if let Some(OutputClause::Output {
        into_table: Some(into),
        ..
    }) = output
    {
        match output_into_target(into, dialect, rules) {
            OutputInto::Table(target) => facts.ops.push(ir::CatalogOp::WriteData(ir::DataWrite {
                target,
                kind: ir::WriteKind::Insert {
                    conflict: ir::ConflictAction::Error,
                },
                columns: Vec::new(),
                unqualified_columns: Vec::new(),
                reads: Vec::new(),
                only: false,
            })),
            OutputInto::TableVariable(variable) => facts.barriers.push(barrier(
                ir::BarrierScope::Relation { relation: variable },
                ir::BarrierReason::UnknownDml,
                Some("unmodeled OUTPUT INTO table variable".to_string()),
            )),
            OutputInto::Unmodeled => facts.barriers.push(global_barrier(
                ir::BarrierReason::UnknownDml,
                Some("unmodeled OUTPUT INTO target".to_string()),
            )),
        }
    }
    // Generic variables are recorded, but their effects on parsing, writes,
    // triggers, and transaction behavior are not represented by the catalog.
    if !inert_pragma_effects
        && facts.session.iter().any(|effect| {
            matches!(
                effect,
                ir::SessionEffect::SetVariable { .. }
                    | ir::SessionEffect::ResetVariable { .. }
                    | ir::SessionEffect::ResetAll { .. }
            )
        })
    {
        facts.barriers.push(global_barrier(
            ir::BarrierReason::UnknownSessionEffect,
            Some("unmodeled session variable effects".to_string()),
        ));
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
    if context.column_ir {
        column_unit.stars = column_stars(&visitor);
        column_unit.unknown_source = visitor.unmodeled_table_source;
        facts.column = column_unit;
        for (start, end, label) in column_singles {
            visitor.add_column_edit(start, end, label);
        }
    }
    facts.edits = visitor.edits;
    facts
}

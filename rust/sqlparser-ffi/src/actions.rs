use crate::edits::AnalysisVisitor;
use crate::source::{ByteSpan, SourceMap, TokenPosition};
use crate::{DialectKind, NativeAction, NativeActionType, NativeFacts, NativeObjectKind};
use sqlparser::ast::{
    AlterTableOperation, AssignmentTarget, ColumnOption, ForeignKeyConstraint, Ident, ObjectName,
    ObjectNamePart, ObjectType, OnCommit, OnInsert, ReferentialAction, RenameTableNameKind,
    Spanned, Statement, TableConstraint, TableFactor, TableObject, Visit,
};
use sqlparser::tokenizer::Span;
use std::collections::BTreeSet;
fn action(
    kind: NativeObjectKind,
    action: NativeActionType,
    name: impl Into<String>,
) -> NativeAction {
    NativeAction {
        kind,
        action,
        name: name.into(),
    }
}

fn barrier(name: &str) -> NativeAction {
    action(NativeObjectKind::Barrier, NativeActionType::Barrier, name)
}

pub(super) fn canonical_ident(ident: &Ident, dialect: DialectKind) -> String {
    let value = match dialect {
        DialectKind::ClickHouse => ident.value.clone(),
        DialectKind::PostgreSql
        | DialectKind::DuckDB
        | DialectKind::Oracle
        | DialectKind::Ansi
        | DialectKind::MsSql
        | DialectKind::Redshift
        | DialectKind::Snowflake
        | DialectKind::Teradata
            if ident.quote_style.is_some() =>
        {
            ident.value.clone()
        }
        _ => ident.value.to_lowercase(),
    };
    escape_part(&value)
}

fn escape_part(part: &str) -> String {
    let mut out = String::with_capacity(part.len());
    for ch in part.chars() {
        if ch == '.' || ch == '\\' {
            out.push('\\');
        }
        out.push(ch);
    }
    out
}

pub(super) fn name_parts(name: &ObjectName, dialect: DialectKind) -> Option<Vec<String>> {
    name.0
        .iter()
        .map(|part| match part {
            ObjectNamePart::Identifier(ident) => Some(canonical_ident(ident, dialect)),
            ObjectNamePart::Function(_) => None,
        })
        .collect()
}

fn canonical_name(name: &ObjectName, dialect: DialectKind) -> Option<String> {
    Some(name_parts(name, dialect)?.join("."))
}

fn last_name_part(name: &ObjectName, dialect: DialectKind) -> Option<String> {
    name_parts(name, dialect)?.pop()
}

fn qualify_table_rename(
    old_name: &ObjectName,
    new_name: &ObjectName,
    dialect: DialectKind,
) -> Option<String> {
    let old_parts = name_parts(old_name, dialect)?;
    let mut new_parts = name_parts(new_name, dialect)?;
    if new_parts.len() == 1 && old_parts.len() > 1 {
        let mut qualified = old_parts[..old_parts.len() - 1].to_vec();
        qualified.append(&mut new_parts);
        Some(qualified.join("."))
    } else {
        Some(new_parts.join("."))
    }
}

fn split_canonical_name(name: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut escaped = false;
    for (index, ch) in name.char_indices() {
        if escaped {
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == '.' {
            parts.push(&name[start..index]);
            start = index + 1;
        }
    }
    parts.push(&name[start..]);
    parts
}

fn column_name(table: &str, column: &str) -> String {
    format!("{table}\u{1f}{column}")
}

fn temp_schema(name: &ObjectName, dialect: DialectKind) -> bool {
    if !matches!(
        dialect,
        DialectKind::SQLite | DialectKind::PostgreSql | DialectKind::DuckDB | DialectKind::Redshift
    ) {
        return false;
    }
    let Some(parts) = name_parts(name, dialect) else {
        return false;
    };
    matches!(parts.first().map(String::as_str), Some("temp" | "pg_temp"))
        || parts
            .first()
            .is_some_and(|part| part.starts_with("pg_temp_"))
}

fn from_name(name: &ObjectName, dialect: DialectKind) -> Option<String> {
    canonical_name(name, dialect)
}

fn add_table_use(actions: &mut BTreeSet<NativeAction>, name: String) {
    actions.insert(action(NativeObjectKind::Table, NativeActionType::Use, name));
}

fn add_foreign_key(
    actions: &mut BTreeSet<NativeAction>,
    dialect: DialectKind,
    reference: &ForeignKeyConstraint,
) {
    if let Some(target) = from_name(&reference.foreign_table, dialect) {
        add_table_use(actions, target.clone());
        let is_cascading = matches!(
            reference.on_delete,
            Some(
                ReferentialAction::Cascade
                    | ReferentialAction::SetNull
                    | ReferentialAction::SetDefault,
            )
        ) || matches!(
            reference.on_update,
            Some(
                ReferentialAction::Cascade
                    | ReferentialAction::SetNull
                    | ReferentialAction::SetDefault,
            )
        );
        if is_cascading {
            actions.insert(action(
                NativeObjectKind::Table,
                NativeActionType::Cascade,
                target,
            ));
        }
    } else {
        actions.insert(barrier("__unknown_relation_barrier__"));
    }
}

fn add_column_actions(
    actions: &mut BTreeSet<NativeAction>,
    tables: &[String],
    columns: &[Vec<String>],
) {
    for parts in columns {
        let Some(column) = parts.last() else {
            continue;
        };
        let qualifier: Vec<_> = parts[..parts.len().saturating_sub(1)]
            .iter()
            .map(String::as_str)
            .collect();
        let matched: Vec<_> = if qualifier.is_empty() {
            tables.to_vec()
        } else {
            let exact: Vec<_> = tables
                .iter()
                .filter(|table| {
                    let table_parts = split_canonical_name(table);
                    table_parts.as_slice() == qualifier.as_slice()
                        || table_parts
                            .last()
                            .zip(qualifier.last())
                            .is_some_and(|(a, b)| a == b)
                })
                .cloned()
                .collect();
            if exact.is_empty() {
                tables.to_vec()
            } else {
                exact
            }
        };
        for table in matched {
            actions.insert(action(
                NativeObjectKind::Column,
                NativeActionType::Use,
                column_name(&table, column),
            ));
        }
    }
}

fn statements_relations(collector: &AnalysisVisitor<'_>, excluded: &[Span]) -> Vec<(String, Span)> {
    let mut relations = BTreeSet::new();
    for (name, span, is_cte) in &collector.relations {
        if *is_cte || excluded.contains(span) || collector.unmodeled_relation_spans.contains(span) {
            continue;
        }
        if let Some(name) = canonical_name(name, collector.dialect) {
            relations.insert((name, *span));
        }
    }
    relations.into_iter().collect()
}

fn relation_names(collector: &AnalysisVisitor<'_>, excluded: &[Span]) -> Vec<String> {
    statements_relations(collector, excluded)
        .into_iter()
        .map(|(name, _)| name)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

fn has_unmodeled_relation(collector: &AnalysisVisitor<'_>, excluded: &[Span]) -> bool {
    collector.unmodeled_table_source
        || collector
            .unmodeled_relation_spans
            .iter()
            .any(|span| !excluded.contains(span))
        || collector.relations.iter().any(|(name, span, is_cte)| {
            !*is_cte
                && !excluded.contains(span)
                && canonical_name(name, collector.dialect).is_none()
        })
}

fn append_column_uses(
    actions: &mut BTreeSet<NativeAction>,
    collector: &AnalysisVisitor<'_>,
    source_tables: &[String],
) {
    add_column_actions(actions, source_tables, &collector.columns);
}

pub(super) fn analyze_statement(
    statement: &Statement,
    dialect: DialectKind,
    source: &str,
    source_map: &SourceMap,
    tokens: &[TokenPosition],
    unit: ByteSpan,
) -> NativeFacts {
    let mut visitor = AnalysisVisitor::new(dialect, source, source_map, tokens, unit);
    let _ = statement.visit(&mut visitor);
    let mut actions = BTreeSet::new();
    let stmt = statement;
    match stmt {
        Statement::Query(_) => {
            let refs = relation_names(&visitor, &[]);
            for table in &refs {
                add_table_use(&mut actions, table.clone());
            }
            append_column_uses(&mut actions, &visitor, &refs);
            if has_unmodeled_relation(&visitor, &[]) {
                actions.insert(barrier("__unknown_relation_barrier__"));
            }
        }
        Statement::CreateTable(create) => {
            if let Some(table) = from_name(&create.name, dialect) {
                let temporary = create.temporary || temp_schema(&create.name, dialect);
                let kind = if temporary {
                    NativeObjectKind::TemporaryTable
                } else {
                    NativeObjectKind::Table
                };
                actions.insert(action(kind, NativeActionType::Def, table.clone()));
                for col in &create.columns {
                    actions.insert(action(
                        NativeObjectKind::Column,
                        NativeActionType::Def,
                        column_name(&table, &canonical_ident(&col.name, dialect)),
                    ));
                }
                let excluded = [create.name.span()];
                let refs = relation_names(&visitor, &excluded);
                for reference in &refs {
                    add_table_use(&mut actions, reference.clone());
                }
                if has_unmodeled_relation(&visitor, &excluded) {
                    actions.insert(barrier("__unknown_relation_barrier__"));
                }

                for column in &create.columns {
                    for option in &column.options {
                        if let ColumnOption::ForeignKey(reference) = &option.option {
                            add_foreign_key(&mut actions, dialect, reference);
                        }
                    }
                }
                for constraint in &create.constraints {
                    if let TableConstraint::ForeignKey(reference) = constraint {
                        add_foreign_key(&mut actions, dialect, reference);
                    }
                }
                match create.on_commit {
                    Some(OnCommit::Drop) => {
                        actions.insert(action(
                            NativeObjectKind::TemporaryTable,
                            NativeActionType::OnCommitDrop,
                            table.clone(),
                        ));
                    }
                    Some(OnCommit::DeleteRows) => {
                        actions.insert(action(
                            NativeObjectKind::TemporaryTable,
                            NativeActionType::OnCommitDeleteRows,
                            table.clone(),
                        ));
                    }
                    _ => {}
                }
                if let Some(like) = &create.like {
                    let like_name = match like {
                        sqlparser::ast::CreateTableLikeKind::Plain(like)
                        | sqlparser::ast::CreateTableLikeKind::Parenthesized(like) => {
                            Some(&like.name)
                        }
                    };
                    if let Some(name) = like_name.and_then(|name| from_name(name, dialect)) {
                        add_table_use(&mut actions, name);
                    } else {
                        actions.insert(barrier("__unknown_relation_barrier__"));
                    }
                }
            } else {
                actions.insert(barrier("__unknown_ddl_barrier__"));
            }
        }
        Statement::CreateView(create) => {
            if let Some(view) = from_name(&create.name, dialect) {
                actions.insert(action(NativeObjectKind::View, NativeActionType::Def, view));
                let excluded = [create.name.span()];
                let refs = relation_names(&visitor, &excluded);
                for reference in &refs {
                    add_table_use(&mut actions, reference.clone());
                }
                append_column_uses(&mut actions, &visitor, &refs);
                if has_unmodeled_relation(&visitor, &excluded) {
                    actions.insert(barrier("__unknown_relation_barrier__"));
                }
            } else {
                actions.insert(barrier("__unknown_view_barrier__"));
            }
        }
        Statement::CreateIndex(create) => {
            if let (Some(index), Some(table)) = (
                create
                    .name
                    .as_ref()
                    .and_then(|name| from_name(name, dialect)),
                from_name(&create.table_name, dialect),
            ) {
                actions.insert(action(
                    NativeObjectKind::Index,
                    NativeActionType::Def,
                    index,
                ));
                add_table_use(&mut actions, table.clone());
                append_column_uses(&mut actions, &visitor, &[table]);
                if has_unmodeled_relation(&visitor, &[create.table_name.span()]) {
                    actions.insert(barrier("__unknown_relation_barrier__"));
                }
            } else {
                actions.insert(barrier("__unknown_index_barrier__"));
            }
        }
        Statement::CreateTrigger(create) => {
            if let (Some(trigger), Some(table)) = (
                from_name(&create.name, dialect),
                from_name(&create.table_name, dialect),
            ) {
                actions.insert(action(
                    NativeObjectKind::Trigger,
                    NativeActionType::Def,
                    trigger,
                ));
                add_table_use(&mut actions, table);
                if let Some(reference) = &create.referenced_table_name {
                    if let Some(reference) = from_name(reference, dialect) {
                        add_table_use(&mut actions, reference);
                    }
                }
                for reference in relation_names(&visitor, &[]) {
                    add_table_use(&mut actions, reference);
                }
            } else {
                actions.insert(barrier("__unknown_trigger_barrier__"));
            }
        }
        Statement::DropTrigger(drop_trigger) => {
            if let Some(trigger) = from_name(&drop_trigger.trigger_name, dialect) {
                actions.insert(action(
                    NativeObjectKind::Trigger,
                    NativeActionType::Drop,
                    trigger,
                ));
                if let Some(table) = &drop_trigger.table_name {
                    if let Some(table) = from_name(table, dialect) {
                        add_table_use(&mut actions, table);
                    }
                }
            } else {
                actions.insert(barrier("__unknown_drop_barrier__"));
            }
        }
        Statement::Drop {
            object_type,
            names,
            temporary,
            ..
        } => {
            let kind = match object_type {
                ObjectType::Table => Some(if *temporary {
                    NativeObjectKind::TemporaryTable
                } else {
                    NativeObjectKind::Table
                }),
                ObjectType::View | ObjectType::MaterializedView => Some(NativeObjectKind::View),
                ObjectType::Index => Some(NativeObjectKind::Index),
                _ => None,
            };
            if let Some(kind) = kind {
                if names.is_empty() {
                    actions.insert(barrier("__unknown_drop_barrier__"));
                } else {
                    for name in names {
                        if let Some(name) = from_name(name, dialect) {
                            actions.insert(action(kind, NativeActionType::Drop, name));
                        } else {
                            actions.insert(barrier("__unknown_drop_barrier__"));
                        }
                    }
                }
            } else {
                actions.insert(barrier("__unknown_stmt_barrier__"));
            }
        }
        Statement::AlterTable(alter) => {
            if let Some(table) = from_name(&alter.name, dialect) {
                actions.insert(action(
                    NativeObjectKind::Table,
                    NativeActionType::Mod,
                    table.clone(),
                ));
                for operation in &alter.operations {
                    match operation {
                        AlterTableOperation::RenameTable { table_name } => {
                            let new = match table_name {
                                RenameTableNameKind::As(name) | RenameTableNameKind::To(name) => {
                                    qualify_table_rename(&alter.name, name, dialect)
                                }
                            };
                            if let Some(new) = new {
                                actions.insert(action(
                                    NativeObjectKind::Table,
                                    NativeActionType::RenameFrom,
                                    table.clone(),
                                ));
                                actions.insert(action(
                                    NativeObjectKind::Table,
                                    NativeActionType::RenameTo,
                                    new,
                                ));
                            } else {
                                actions.insert(barrier("__unknown_alter_barrier__"));
                            }
                        }
                        AlterTableOperation::AddColumn { column_def, .. } => {
                            actions.insert(action(
                                NativeObjectKind::Column,
                                NativeActionType::Def,
                                column_name(&table, &canonical_ident(&column_def.name, dialect)),
                            ));
                        }
                        AlterTableOperation::DropColumn { column_names, .. } => {
                            for col in column_names {
                                actions.insert(action(
                                    NativeObjectKind::Column,
                                    NativeActionType::Drop,
                                    column_name(&table, &canonical_ident(col, dialect)),
                                ));
                            }
                        }
                        AlterTableOperation::RenameColumn {
                            old_column_name,
                            new_column_name,
                        } => {
                            actions.insert(action(
                                NativeObjectKind::Column,
                                NativeActionType::Drop,
                                column_name(&table, &canonical_ident(old_column_name, dialect)),
                            ));
                            actions.insert(action(
                                NativeObjectKind::Column,
                                NativeActionType::Def,
                                column_name(&table, &canonical_ident(new_column_name, dialect)),
                            ));
                        }
                        AlterTableOperation::AddConstraint { constraint, .. } => {
                            if let TableConstraint::ForeignKey(reference) = constraint {
                                add_foreign_key(&mut actions, dialect, reference);
                            }
                        }
                        _ => {
                            actions.insert(barrier("__unknown_alter_barrier__"));
                        }
                    }
                }
            } else {
                actions.insert(barrier("__unknown_alter_barrier__"));
            }
        }
        Statement::Insert(insert) => {
            if let TableObject::TableName(target) = &insert.table {
                if let Some(table) = from_name(target, dialect) {
                    actions.insert(action(
                        NativeObjectKind::Table,
                        NativeActionType::Mod,
                        table.clone(),
                    ));
                    let excluded = [target.span()];
                    let refs = relation_names(&visitor, &excluded);
                    for reference in &refs {
                        add_table_use(&mut actions, reference.clone());
                    }
                    append_column_uses(&mut actions, &visitor, &refs);
                    if has_unmodeled_relation(&visitor, &excluded) {
                        actions.insert(barrier("__unknown_relation_barrier__"));
                    }
                    for column in &insert.columns {
                        add_modified_column(&mut actions, &table, column, dialect);
                    }
                    for assignment in &insert.assignments {
                        add_assignment_columns(&mut actions, &table, &assignment.target, dialect);
                    }
                    if let Some(on_insert) = &insert.on {
                        match on_insert {
                            OnInsert::DuplicateKeyUpdate(assignments) => {
                                for assignment in assignments {
                                    add_assignment_columns(
                                        &mut actions,
                                        &table,
                                        &assignment.target,
                                        dialect,
                                    );
                                }
                            }
                            OnInsert::OnConflict(conflict) => {
                                if let sqlparser::ast::OnConflictAction::DoUpdate(update) =
                                    &conflict.action
                                {
                                    for assignment in &update.assignments {
                                        add_assignment_columns(
                                            &mut actions,
                                            &table,
                                            &assignment.target,
                                            dialect,
                                        );
                                    }
                                }
                            }
                            _ => {
                                actions.insert(barrier("__unknown_dml_barrier__"));
                            }
                        }
                    }
                } else {
                    actions.insert(barrier("__unknown_dml_barrier__"));
                }
            } else {
                actions.insert(barrier("__unknown_dml_barrier__"));
            }
        }
        Statement::Update(update) => {
            if let Some((table, target_span)) = first_table_in(&update.table, dialect) {
                actions.insert(action(
                    NativeObjectKind::Table,
                    NativeActionType::Mod,
                    table.clone(),
                ));
                let excluded = [target_span];
                let refs = relation_names(&visitor, &excluded);
                for reference in &refs {
                    add_table_use(&mut actions, reference.clone());
                }
                let mut column_tables = vec![table.clone()];
                column_tables.extend(refs);
                append_column_uses(&mut actions, &visitor, &column_tables);
                if has_unmodeled_relation(&visitor, &excluded) {
                    actions.insert(barrier("__unknown_relation_barrier__"));
                }
                for assignment in &update.assignments {
                    match &assignment.target {
                        AssignmentTarget::ColumnName(column) => {
                            add_modified_column(&mut actions, &table, column, dialect)
                        }
                        AssignmentTarget::Tuple(columns) => {
                            for column in columns {
                                add_modified_column(&mut actions, &table, column, dialect);
                            }
                        }
                    }
                }
            } else {
                actions.insert(barrier("__unknown_dml_barrier__"));
            }
        }
        Statement::Delete(delete) => {
            if !delete.tables.is_empty() {
                let mut target_spans = Vec::new();
                let mut modified_tables = Vec::new();
                for tbl in &delete.tables {
                    if let Some(name) = from_name(tbl, dialect) {
                        actions.insert(action(
                            NativeObjectKind::Table,
                            NativeActionType::Mod,
                            name.clone(),
                        ));
                        target_spans.push(tbl.span());
                        modified_tables.push(name);
                    }
                }
                let refs = relation_names(&visitor, &target_spans);
                for reference in &refs {
                    add_table_use(&mut actions, reference.clone());
                }
                let mut column_tables = modified_tables;
                column_tables.extend(refs);
                append_column_uses(&mut actions, &visitor, &column_tables);
                if has_unmodeled_relation(&visitor, &target_spans) {
                    actions.insert(barrier("__unknown_relation_barrier__"));
                }
            } else if let Some((table, target_span)) = first_delete_target(&delete.from, dialect) {
                actions.insert(action(
                    NativeObjectKind::Table,
                    NativeActionType::Mod,
                    table.clone(),
                ));
                let excluded = [target_span];
                let refs = relation_names(&visitor, &excluded);
                for reference in &refs {
                    add_table_use(&mut actions, reference.clone());
                }
                let mut column_tables = vec![table];
                column_tables.extend(refs);
                append_column_uses(&mut actions, &visitor, &column_tables);
                if has_unmodeled_relation(&visitor, &excluded) {
                    actions.insert(barrier("__unknown_relation_barrier__"));
                }
            } else {
                actions.insert(barrier("__unknown_dml_barrier__"));
            }
        }
        Statement::Merge(merge) => {
            if let Some((target, target_span)) = table_factor_name(&merge.table, dialect) {
                actions.insert(action(
                    NativeObjectKind::Table,
                    NativeActionType::Mod,
                    target.clone(),
                ));
                let excluded = [target_span];
                let refs = relation_names(&visitor, &excluded);
                for reference in &refs {
                    add_table_use(&mut actions, reference.clone());
                }
                let mut column_tables = vec![target.clone()];
                column_tables.extend(refs);
                append_column_uses(&mut actions, &visitor, &column_tables);
                if has_unmodeled_relation(&visitor, &excluded) {
                    actions.insert(barrier("__unknown_relation_barrier__"));
                }
                for clause in &merge.clauses {
                    if let sqlparser::ast::MergeAction::Update(update) = &clause.action {
                        if let sqlparser::ast::MergeUpdateKind::Set(assignments) = &update.kind {
                            for assignment in assignments {
                                add_assignment_columns(
                                    &mut actions,
                                    &target,
                                    &assignment.target,
                                    dialect,
                                );
                            }
                        }
                    }
                }
            } else {
                actions.insert(barrier("__unknown_dml_barrier__"));
            }
        }
        Statement::Truncate(truncate) => {
            if truncate.table_names.is_empty() {
                actions.insert(barrier("__unknown_truncate_barrier__"));
            }
            for table in &truncate.table_names {
                if let Some(name) = from_name(&table.name, dialect) {
                    actions.insert(action(NativeObjectKind::Table, NativeActionType::Mod, name));
                } else {
                    actions.insert(barrier("__unknown_truncate_barrier__"));
                }
            }
        }
        Statement::Set(set) => match set {
            sqlparser::ast::Set::SingleAssignment { variable, .. } => {
                if let Some(name) = canonical_name(variable, dialect) {
                    actions.insert(action(
                        NativeObjectKind::SessionVariable,
                        NativeActionType::Mod,
                        name,
                    ));
                } else {
                    actions.insert(action(
                        NativeObjectKind::SessionVariable,
                        NativeActionType::Barrier,
                        "",
                    ));
                }
            }
            sqlparser::ast::Set::ParenthesizedAssignments { variables, .. } => {
                for var in variables {
                    if let Some(name) = canonical_name(var, dialect) {
                        actions.insert(action(
                            NativeObjectKind::SessionVariable,
                            NativeActionType::Mod,
                            name,
                        ));
                    }
                }
            }
            sqlparser::ast::Set::MultipleAssignments { assignments } => {
                for assignment in assignments {
                    if let Some(name) = canonical_name(&assignment.name, dialect) {
                        actions.insert(action(
                            NativeObjectKind::SessionVariable,
                            NativeActionType::Mod,
                            name,
                        ));
                    }
                }
            }
            _ => {
                actions.insert(action(
                    NativeObjectKind::SessionVariable,
                    NativeActionType::Barrier,
                    "",
                ));
            }
        },
        Statement::Reset(reset_stmt) => match &reset_stmt.reset {
            sqlparser::ast::Reset::ConfigurationParameter(name) => {
                if let Some(var_name) = canonical_name(name, dialect) {
                    actions.insert(action(
                        NativeObjectKind::SessionVariable,
                        NativeActionType::Mod,
                        var_name,
                    ));
                } else {
                    actions.insert(action(
                        NativeObjectKind::SessionVariable,
                        NativeActionType::Barrier,
                        "",
                    ));
                }
            }
            _ => {
                actions.insert(action(
                    NativeObjectKind::SessionVariable,
                    NativeActionType::Barrier,
                    "",
                ));
            }
        },
        Statement::StartTransaction { .. } => {
            actions.insert(action(
                NativeObjectKind::Transaction,
                NativeActionType::TxBegin,
                "",
            ));
        }
        Statement::Commit { .. } => {
            actions.insert(action(
                NativeObjectKind::Transaction,
                NativeActionType::TxCommit,
                "",
            ));
        }
        Statement::Rollback { savepoint, .. } => {
            if let Some(ident) = savepoint {
                actions.insert(action(
                    NativeObjectKind::Transaction,
                    NativeActionType::TxRollbackTo,
                    canonical_ident(ident, dialect),
                ));
            } else {
                actions.insert(action(
                    NativeObjectKind::Transaction,
                    NativeActionType::TxRollback,
                    "",
                ));
            }
        }
        Statement::Savepoint { name } => {
            actions.insert(action(
                NativeObjectKind::Transaction,
                NativeActionType::TxSavepoint,
                canonical_ident(name, dialect),
            ));
        }
        Statement::ReleaseSavepoint { name } => {
            actions.insert(action(
                NativeObjectKind::Transaction,
                NativeActionType::TxRelease,
                canonical_ident(name, dialect),
            ));
        }
        _ => {
            actions.insert(barrier("__unknown_stmt_barrier__"));
        }
    }
    NativeFacts {
        actions: actions.into_iter().collect(),
        edits: visitor.edits,
    }
}

fn add_modified_column(
    actions: &mut BTreeSet<NativeAction>,
    table: &str,
    column: &ObjectName,
    dialect: DialectKind,
) {
    if let Some(name) = last_name_part(column, dialect) {
        actions.insert(action(
            NativeObjectKind::Column,
            NativeActionType::Mod,
            column_name(table, &name),
        ));
    }
}

fn add_assignment_columns(
    actions: &mut BTreeSet<NativeAction>,
    table: &str,
    target: &AssignmentTarget,
    dialect: DialectKind,
) {
    match target {
        AssignmentTarget::ColumnName(column) => {
            add_modified_column(actions, table, column, dialect)
        }
        AssignmentTarget::Tuple(columns) => {
            for column in columns {
                add_modified_column(actions, table, column, dialect);
            }
        }
    }
}

fn table_factor_name(factor: &TableFactor, dialect: DialectKind) -> Option<(String, Span)> {
    match factor {
        TableFactor::Table { name, .. } => Some((from_name(name, dialect)?, name.span())),
        _ => None,
    }
}

fn first_table_in(
    table: &sqlparser::ast::TableWithJoins,
    dialect: DialectKind,
) -> Option<(String, Span)> {
    match &table.relation {
        TableFactor::Table {
            name, args: None, ..
        } => Some((from_name(name, dialect)?, name.span())),
        _ => None,
    }
}

fn first_delete_target(
    from: &sqlparser::ast::FromTable,
    dialect: DialectKind,
) -> Option<(String, Span)> {
    let tables = match from {
        sqlparser::ast::FromTable::WithFromKeyword(tables)
        | sqlparser::ast::FromTable::WithoutKeyword(tables) => tables,
    };
    tables
        .first()
        .and_then(|table| first_table_in(table, dialect))
}

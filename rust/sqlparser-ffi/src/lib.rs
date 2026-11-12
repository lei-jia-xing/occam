use serde::{Deserialize, Serialize};
use std::borrow::Cow;

mod actions;
mod edits;
mod ffi;
mod ir;
mod parser;
mod source;

pub use ffi::{occam_sql_count, occam_sql_free, occam_sql_parse, occam_sql_parse_error};
pub(crate) use ir::*;

#[cfg(test)]
use parser::{count_units, parse_units};
#[cfg(test)]
use sqlparser::dialect::SQLiteDialect;
#[cfg(test)]
use sqlparser::parser::Parser;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
enum DialectKind {
    #[serde(rename = "generic")]
    Generic,
    #[serde(rename = "postgresql")]
    PostgreSql,
    #[serde(rename = "mysql")]
    MySql,
    #[serde(rename = "sqlite")]
    SQLite,
    #[serde(rename = "duckdb")]
    DuckDB,
    #[serde(rename = "clickhouse")]
    ClickHouse,
    #[serde(rename = "oracle")]
    Oracle,
    #[serde(rename = "ansi")]
    Ansi,
    #[serde(rename = "bigquery")]
    BigQuery,
    #[serde(rename = "databricks")]
    Databricks,
    #[serde(rename = "hive")]
    Hive,
    #[serde(rename = "mssql")]
    MsSql,
    #[serde(rename = "redshift")]
    Redshift,
    #[serde(rename = "snowflake")]
    Snowflake,
    #[serde(rename = "spark")]
    Spark,
    #[serde(rename = "teradata")]
    Teradata,
}

#[derive(Serialize)]
struct NativeResponse {
    initial_session: SessionInit,
    units: Vec<NativeUnit>,
}

#[derive(Serialize)]
struct NativeUnit {
    start: usize,
    end: usize,
    parse_error: Option<String>,
    facts: Option<NativeFacts>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize)]
#[serde(rename_all = "snake_case")]
enum NativeEditCategory {
    Expression,
    Clause,
    Constraint,
    List,
    Type,
    Literal,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
struct NativeEdit {
    start: usize,
    end: usize,
    replacement: String,
    category: NativeEditCategory,
    label: Cow<'static, str>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(sql: &str, dialect: DialectKind) -> Result<Vec<NativeUnit>, String> {
        parse_units(
            sql,
            &ParseConfig {
                dialect,
                server: ServerConfig::default(),
            },
        )
    }

    fn facts(unit: &NativeUnit) -> &NativeFacts {
        unit.facts.as_ref().expect("native facts")
    }

    fn name_text(name: &Name) -> String {
        name.0
            .iter()
            .map(|part| part.0.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }

    fn reads(facts: &NativeFacts) -> Vec<&ReadFact> {
        facts
            .ops
            .iter()
            .filter_map(|op| match op {
                CatalogOp::Read(read) => Some(read),
                _ => None,
            })
            .collect()
    }

    fn writes(facts: &NativeFacts) -> Vec<&DataWrite> {
        facts
            .ops
            .iter()
            .filter_map(|op| match op {
                CatalogOp::WriteData(write) => Some(write),
                _ => None,
            })
            .collect()
    }

    fn table_def(facts: &NativeFacts) -> Option<&TableDef> {
        facts.ops.iter().find_map(|op| match op {
            CatalogOp::DefineTable(table) => Some(table),
            _ => None,
        })
    }

    fn relation_reads(facts: &NativeFacts) -> Vec<String> {
        reads(facts)
            .into_iter()
            .map(|read| name_text(&read.source.name))
            .collect()
    }

    fn mentioned_relations(facts: &NativeFacts) -> Vec<String> {
        let mut names = Vec::new();
        for op in &facts.ops {
            let name = match op {
                CatalogOp::Read(read) => Some(name_text(&read.source.name)),
                CatalogOp::DefineTable(table) => Some(name_text(&table.name)),
                CatalogOp::DefineView(view) => Some(name_text(&view.name)),
                CatalogOp::WriteData(write) => Some(name_text(&write.target.name)),
                CatalogOp::AlterTable(alter) => Some(name_text(&alter.table.name)),
                CatalogOp::Refresh(refresh) => Some(name_text(&refresh.target.name)),
                _ => None,
            };
            if let Some(name) = name {
                names.push(name);
            }
        }
        names
    }

    #[test]
    fn statement_boundaries_and_source_mapping() {
        let sql = "-- café\r\nSELECT 'x;y';\r\nSELECT 2;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert_eq!(&sql[units[0].start..units[0].end], "SELECT 'x;y'");
        assert_eq!(&sql[units[1].start..units[1].end], "SELECT 2");
    }
    #[test]
    fn column_qualifiers_require_a_structural_match() {
        let sql = "SELECT missing.id FROM actual;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        let found = reads(facts(&units[0]));
        assert_eq!(found.len(), 1);
        assert!(found[0].columns.is_empty(), "{:?}", found[0].columns);

        let sql = "SELECT t.id FROM public.t;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        let found = reads(facts(&units[0]));
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].columns, vec![Ident("id".to_string())]);

        let sql = "SELECT other.t.id FROM public.t;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        let found = reads(facts(&units[0]));
        assert_eq!(found.len(), 1);
        assert!(found[0].columns.is_empty(), "{:?}", found[0].columns);
    }

    #[test]
    fn invalid_middle_unit_does_not_hide_later_statement() {
        let sql = "SELECT 1; SELECT FROM; SELECT 3";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 3);
        assert!(units[1].facts.is_none());
        assert!(units[1].parse_error.is_some());
        assert!(units[2].facts.is_some());
    }

    #[test]
    fn parser_and_tokenizer_errors_are_unit_local() {
        let sql = "SELECT 1 FROM; SELECT 2;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert!(units[0].facts.is_none());
        assert!(units[0].parse_error.is_some());
        assert!(units[1].facts.is_some());

        let sql = "SELECT 1; SELECT FROM";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert_eq!(&sql[units[1].start..units[1].end], "SELECT FROM");
        assert!(units[1].facts.is_none());

        let sql = "SELECT 1; SELECT 'unterminated";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert!(units[0].facts.is_some());
        assert_eq!(&sql[units[1].start..units[1].end], "SELECT 'unterminated");
        assert!(units[1].parse_error.is_some());
        assert_eq!(count_units(sql, DialectKind::SQLite).unwrap(), 2);

        let sql = "SELECT 1 FROM; SELECT 'unterminated";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert_eq!(&sql[units[0].start..units[0].end], "SELECT 1 FROM");
        assert_eq!(&sql[units[1].start..units[1].end], "SELECT 'unterminated");
        assert!(units[0].end <= units[1].start);
        assert_eq!(count_units(sql, DialectKind::SQLite).unwrap(), 2);
    }

    #[test]
    fn missing_delimiter_coalesces_through_its_semicolon() {
        let sql = "SELECT 1 CREATE TABLE t (x INT); SELECT 2;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert_eq!(
            &sql[units[0].start..units[0].end],
            "SELECT 1 CREATE TABLE t (x INT)"
        );
        assert!(units[0].facts.is_none());
        assert!(units[0].parse_error.is_some());
        assert!(units[1].facts.is_some());
    }

    #[test]
    fn edits_map_same_line_unicode_and_crlf_to_exact_bytes() {
        let sql = "SELECT 'é', 1 + 2;\r\nSELECT 3;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        let edit = units[0]
            .facts
            .as_ref()
            .unwrap()
            .edits
            .iter()
            .find(|edit| edit.label == "simplify-binary_expression")
            .unwrap();
        assert_eq!(&sql[edit.start..edit.end], "1 + 2");
        assert!(sql.is_char_boundary(edit.start));
        assert!(sql.is_char_boundary(edit.end));
        assert_eq!(facts(&units[1]).ops.len(), 0);
    }

    #[test]
    fn values_types_and_cte_edits_keep_ast_boundaries() {
        let values_sql = "INSERT INTO t(a,b) VALUES (1,2),(42,43);";
        let values = parse(values_sql, DialectKind::SQLite).unwrap();
        let edit = values[0]
            .facts
            .as_ref()
            .unwrap()
            .edits
            .iter()
            .find(|edit| edit.label == "remove-values_row")
            .unwrap();
        let reduced = format!(
            "{}{}{}",
            &values_sql[..edit.start],
            edit.replacement,
            &values_sql[edit.end..]
        );
        assert_eq!(reduced, "INSERT INTO t(a,b) VALUES (42,43);");

        let type_sql = "CREATE TABLE t (id VARCHAR(255) NOT NULL); SELECT CAST(1 AS BIGINT);";
        let types = parse(type_sql, DialectKind::PostgreSql).unwrap();
        let type_slices: Vec<_> = types
            .iter()
            .flat_map(|unit| unit.facts.iter())
            .flat_map(|facts| &facts.edits)
            .filter(|edit| edit.category == NativeEditCategory::Type)
            .map(|edit| &type_sql[edit.start..edit.end])
            .collect();
        assert!(type_slices.contains(&"VARCHAR(255)"), "{type_slices:?}");
        assert!(type_slices.contains(&"BIGINT"));

        let cte_sql =
            "WITH a AS (SELECT id FROM source), b AS (SELECT id FROM a) SELECT id FROM b;";
        let cte = parse(cte_sql, DialectKind::PostgreSql).unwrap();
        assert_eq!(relation_reads(facts(&cte[0])), vec!["source"]);
    }

    #[test]
    fn quoted_dots_and_trigger_drop_keep_canonical_actions() {
        let sql = "CREATE TABLE t (\"a.b\" INT); INSERT INTO t (\"a.b\") VALUES (1); ALTER TABLE \"old.schema\".t RENAME TO \"new.name\"; DROP TRIGGER tr ON t;";
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        let inserted = writes(facts(&units[1]));
        assert_eq!(inserted.len(), 1);
        assert_eq!(inserted[0].columns, vec![Ident("a.b".to_string())]);
        let renamed = facts(&units[2])
            .ops
            .iter()
            .find_map(|op| match op {
                CatalogOp::Rename(rename) => Some(rename),
                _ => None,
            })
            .expect("rename op");
        match renamed {
            RenameOp::Relation { from, to, .. } => {
                assert_eq!(name_text(&from.name), "old.schema.t");
                assert_eq!(name_text(to), "old.schema.new.name");
            }
            other => panic!("unexpected rename: {other:?}"),
        }
        let dropped = facts(&units[3])
            .ops
            .iter()
            .find_map(|op| match op {
                CatalogOp::Drop(drop) => Some(drop),
                _ => None,
            })
            .expect("drop op");
        assert_eq!(
            dropped.objects,
            vec![DropObject::Trigger {
                trigger: TriggerRef::Table {
                    table: NameRef {
                        name: Name(vec![Ident("t".to_string())]),
                        temp: false,
                    },
                    name: Ident("tr".to_string()),
                },
            }]
        );
    }

    #[test]
    fn clause_edits_use_full_clause_boundaries() {
        let sql = "WITH c AS (SELECT x FROM t) SELECT a FROM u LEFT JOIN v ON u.id = v.id WHERE a = 1 GROUP BY a, b ORDER BY b LIMIT 5 OFFSET 2;";
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        let edits = &units[0].facts.as_ref().unwrap().edits;
        let find = |label: &str| edits.iter().find(|edit| edit.label == label).unwrap();
        assert_eq!(
            sql[find("remove-with").start..find("remove-with").end].trim_end(),
            "WITH c AS (SELECT x FROM t)"
        );
        assert_eq!(
            sql[find("remove-join").start..find("remove-join").end].trim_start(),
            "LEFT JOIN v ON u.id = v.id"
        );
        assert_eq!(
            sql[find("remove-group_by").start..find("remove-group_by").end].trim_start(),
            "GROUP BY a, b"
        );
        assert_eq!(
            sql[find("remove-order_by").start..find("remove-order_by").end].trim_start(),
            "ORDER BY b"
        );
        assert_eq!(
            sql[find("remove-limit").start..find("remove-limit").end].trim_start(),
            "LIMIT 5"
        );
        assert_eq!(
            sql[find("remove-offset").start..find("remove-offset").end].trim_start(),
            "OFFSET 2"
        );
    }

    #[test]
    fn compound_trigger_keeps_internal_semicolons_in_one_unit() {
        let sql = "CREATE TRIGGER tr AFTER INSERT ON t BEGIN UPDATE t SET x = 'a;b'; INSERT INTO audit VALUES ('x;y'); END; SELECT 2;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert!(sql[units[0].start..units[0].end].starts_with("CREATE TRIGGER"));
        assert!(sql[units[0].start..units[0].end].ends_with("END"));
        assert!(units[0].facts.is_some());
        assert!(units[1].facts.is_some());
    }

    #[test]
    fn invalid_utf8_is_rejected_at_the_ffi_boundary() {
        let input = [0xff];
        let config = serde_json::to_vec(&ParseConfig {
            dialect: DialectKind::SQLite,
            server: ServerConfig::default(),
        })
        .unwrap();
        let mut output: *mut u8 = std::ptr::null_mut();
        let mut output_len = 0usize;
        let invalid_output = unsafe {
            occam_sql_parse(
                config.as_ptr(),
                config.len(),
                input.as_ptr(),
                input.len(),
                std::ptr::null_mut(),
                &mut output_len,
            )
        };
        assert_eq!(invalid_output, 1);
        assert!(output.is_null());
        let status = unsafe {
            occam_sql_parse(
                config.as_ptr(),
                config.len(),
                input.as_ptr(),
                input.len(),
                &mut output,
                &mut output_len,
            )
        };
        assert_eq!(status, 3);
        assert!(output.is_null());
        assert_eq!(output_len, 0);
    }

    #[test]
    fn conflict_writes_nested_ctes_and_group_by_all_are_handled() {
        let sql = "WITH outer_cte AS (SELECT id FROM outer_src) SELECT id FROM (WITH outer_cte AS (SELECT id FROM inner_src) SELECT id FROM outer_cte) nested JOIN outer_cte ON nested.id = outer_cte.id;";
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        let mut used = relation_reads(facts(&units[0]));
        used.sort();
        assert_eq!(used, vec!["inner_src", "outer_src"]);

        let dynamic = parse(
            "SELECT * FROM generate_series(1, 2) AS n;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        assert!(!facts(&dynamic[0]).barriers.is_empty());
        assert!(!relation_reads(facts(&dynamic[0])).contains(&"generate_series".to_string()));

        for (dialect, insert) in [
            (
                DialectKind::PostgreSql,
                "INSERT INTO t (id) VALUES (1) ON CONFLICT (id) DO UPDATE SET extra = excluded.extra;",
            ),
            (
                DialectKind::MySql,
                "INSERT INTO t (id) VALUES (1) ON DUPLICATE KEY UPDATE extra = VALUES(extra);",
            ),
        ] {
            let units = parse(insert, dialect).unwrap();
            let writes = writes(facts(&units[0]));
            assert_eq!(writes.len(), 1);
            assert!(
                writes[0].columns.contains(&Ident("extra".to_string())),
                "{:?}",
                writes[0].columns
            );
        }

        let group_sql = "SELECT a FROM t GROUP BY ALL;";
        let group = parse(group_sql, DialectKind::Generic).unwrap();
        let edit = group[0]
            .facts
            .as_ref()
            .unwrap()
            .edits
            .iter()
            .find(|edit| edit.label == "remove-group_by")
            .unwrap();
        let reduced = format!(
            "{}{}{}",
            &group_sql[..edit.start],
            edit.replacement,
            &group_sql[edit.end..]
        );
        assert_eq!(reduced, "SELECT a FROM t;");
    }

    #[test]
    fn cte_production_cases_have_native_facts() {
        for (sql, dialect) in [
            (
                "CREATE TABLE base_a (id INT); CREATE TABLE base_b (id INT); CREATE TABLE base_c (id INT); WITH cte_union AS (SELECT id FROM base_a UNION ALL SELECT id FROM base_b) SELECT id FROM cte_union WHERE EXISTS (SELECT 1 FROM base_c WHERE base_c.id = cte_union.id);",
                DialectKind::Generic,
            ),
            (
                "CREATE TABLE base (id INT); CREATE TABLE inside_cte (id INT); CREATE TABLE nested_cte (id INT); WITH base AS (SELECT id FROM inside_cte) SELECT id FROM base WHERE EXISTS (WITH base AS (SELECT id FROM nested_cte) SELECT id FROM base);",
                DialectKind::PostgreSql,
            ),
            (
                "CREATE TABLE t (id INT); WITH t AS (SELECT id FROM t) SELECT id FROM t;",
                DialectKind::PostgreSql,
            ),
            (
                "CREATE TABLE \"a.b\" (id INT); WITH \"a.b\" AS (SELECT 42 AS id) SELECT id FROM \"a.b\";",
                DialectKind::PostgreSql,
            ),
            (
                "-- café\nWITH unused AS (SELECT 1), kept AS (SELECT 42 AS value) SELECT value FROM kept;\r\n",
                DialectKind::SQLite,
            ),
        ] {
            if let Err(error) = parse(sql, dialect) {
                panic!("native fact generation failed for {sql:?}: {error}");
            }
        }
    }

    #[test]
    fn create_table_foreign_keys_emit_parent_uses() {
        let sql = "CREATE TABLE parent (id INT PRIMARY KEY); CREATE TABLE child (parent_id INT REFERENCES parent(id)); CREATE TABLE child2 (parent_id INT, FOREIGN KEY (parent_id) REFERENCES parent(id));";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        for unit in &units[1..] {
            let found = reads(facts(unit));
            assert!(
                found.iter().any(|read| {
                    name_text(&read.source.name) == "parent" && read.mode == ReadMode::Schema
                }),
                "{:?}",
                found
            );
        }
    }

    #[test]
    fn cascading_foreign_keys_and_on_commit_actions() {
        let sql = "CREATE TABLE child (p_id INT REFERENCES parent(id) ON DELETE CASCADE); CREATE TEMPORARY TABLE temp_tbl (x INT) ON COMMIT DROP;";
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        let child = table_def(facts(&units[0])).expect("child definition");
        let foreign_key = child
            .columns
            .iter()
            .flat_map(|column| column.references.iter())
            .next()
            .expect("foreign key");
        assert_eq!(
            foreign_key.parent.name,
            Name(vec![Ident("parent".to_string())])
        );
        assert_eq!(foreign_key.on_delete, ReferentialAction::Cascade);

        let temp = table_def(facts(&units[1])).expect("temp definition");
        assert!(temp.temporary);
        assert_eq!(temp.name, Name(vec![Ident("temp_tbl".to_string())]));
        assert_eq!(temp.on_commit, Some(OnCommitAction::Drop));
    }

    #[test]
    fn clause_and_scalar_edits_cover_parentheses() {
        let sql = "SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id WHERE (col_timestamp_2 IS NOT NULL AND col_timestamp_1 IS NOT NULL);";
        let units = parse(sql, DialectKind::Generic).unwrap();
        let edit = units[0]
            .facts
            .as_ref()
            .unwrap()
            .edits
            .iter()
            .find(|edit| edit.label == "remove-where")
            .unwrap();
        assert_eq!(
            &sql[edit.start..edit.end],
            " WHERE (col_timestamp_2 IS NOT NULL AND col_timestamp_1 IS NOT NULL)"
        );
        let reduced = format!(
            "{}{}{}",
            &sql[..edit.start],
            edit.replacement,
            &sql[edit.end..]
        );
        assert_eq!(
            reduced,
            "SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id;"
        );

        let sql = "SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id WHERE ((col_numeric_1 + 1.0) > 0.0 AND 1 = 1);";
        let units = parse(sql, DialectKind::Generic).unwrap();
        let edit = units[0]
            .facts
            .as_ref()
            .unwrap()
            .edits
            .iter()
            .find(|edit| edit.label == "remove-where")
            .unwrap();
        assert_eq!(
            &sql[edit.start..edit.end],
            " WHERE ((col_numeric_1 + 1.0) > 0.0 AND 1 = 1)"
        );
        let reduced = format!(
            "{}{}{}",
            &sql[..edit.start],
            edit.replacement,
            &sql[edit.end..]
        );
        assert_eq!(
            reduced,
            "SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id;"
        );

        let sql = "-- café\nSELECT (SELECT 42);\r\n";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        let edit = units[0]
            .facts
            .as_ref()
            .unwrap()
            .edits
            .iter()
            .find(|edit| edit.label == "simplify-scalar-subquery")
            .unwrap();
        assert_eq!(&sql[edit.start..edit.end], "(SELECT 42)");
        let reduced = format!(
            "{}{}{}",
            &sql[..edit.start],
            edit.replacement,
            &sql[edit.end..]
        );
        assert_eq!(reduced, "-- café\nSELECT 42;\r\n");
    }

    #[test]
    fn expression_edits_cover_partial_ast_spans_without_malformed_candidates() {
        let sql = "SELECT id, payload FROM target_json WHERE ((((payload = '{\"trigger\": \"crash_segv\"}') OR FALSE) AND (NULLIF(1, 0) IS NOT NULL)) AND (1 = 1)) AND (1 = 1) LIMIT 9999;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        let edits = &units[0].facts.as_ref().unwrap().edits;
        let where_edit = edits
            .iter()
            .find(|edit| edit.label == "remove-where")
            .unwrap();
        assert_eq!(
            &sql[where_edit.start..where_edit.end],
            " WHERE ((((payload = '{\"trigger\": \"crash_segv\"}') OR FALSE) AND (NULLIF(1, 0) IS NOT NULL)) AND (1 = 1)) AND (1 = 1)"
        );
        let reduced = format!(
            "{}{}{}",
            &sql[..where_edit.start],
            where_edit.replacement,
            &sql[where_edit.end..]
        );
        assert_eq!(reduced, "SELECT id, payload FROM target_json LIMIT 9999;");

        for label in [
            "simplify-binary_expression",
            "simplify-parenthesized_expression",
        ] {
            let matching: Vec<_> = edits.iter().filter(|edit| edit.label == label).collect();
            assert!(!matching.is_empty(), "missing {label} candidates");
            for edit in matching {
                let candidate = format!(
                    "{}{}{}",
                    &sql[..edit.start],
                    edit.replacement,
                    &sql[edit.end..]
                );
                assert!(
                    Parser::parse_sql(&SQLiteDialect {}, &candidate).is_ok(),
                    "{label} emitted malformed candidate: {candidate}"
                );
            }
        }
    }
    #[test]
    fn in_subquery_expression_edits_keep_closing_parentheses() {
        let sql = "SELECT * FROM main_tasks WHERE ((((task_id IN (SELECT task_id FROM sub_items WHERE status = 'crashed_worker')) AND NOT FALSE) OR FALSE) AND NOT FALSE) AND NOT FALSE ORDER BY 1 LIMIT 9999;";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        let edits = &units[0].facts.as_ref().unwrap().edits;
        let subquery = "task_id IN (SELECT task_id FROM sub_items WHERE status = 'crashed_worker')";
        let mut simplified_subquery = false;

        for edit in edits
            .iter()
            .filter(|edit| edit.category == NativeEditCategory::Expression)
        {
            let candidate = format!(
                "{}{}{}",
                &sql[..edit.start],
                edit.replacement,
                &sql[edit.end..]
            );
            assert!(
                Parser::parse_sql(&SQLiteDialect {}, &candidate).is_ok(),
                "{} emitted malformed candidate: {candidate}",
                edit.label
            );
            simplified_subquery |=
                edit.label == "simplify-binary_expression" && edit.replacement.contains(subquery);
        }

        assert!(
            simplified_subquery,
            "binary simplification omitted the complete IN-subquery"
        );
    }
    #[test]
    fn postgres_projection_deletion_keeps_cast_and_dollar_quote_boundaries() {
        let sql = "SELECT (1 + 2)::int AS value, $$crash;marker$$::text AS marker;";
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        let edits = &units[0].facts.as_ref().unwrap().edits;
        let expected = "SELECT $$crash;marker$$::text AS marker;";
        let dialect = sqlparser::dialect::PostgreSqlDialect {};
        let mut removed_first_projection = false;

        for edit in edits
            .iter()
            .filter(|edit| edit.label == "remove-select_expression")
        {
            let candidate = format!(
                "{}{}{}",
                &sql[..edit.start],
                edit.replacement,
                &sql[edit.end..]
            );
            assert!(
                Parser::parse_sql(&dialect, &candidate).is_ok(),
                "{} emitted malformed candidate: {candidate}",
                edit.label
            );
            removed_first_projection |= candidate == expected;
        }

        assert!(
            removed_first_projection,
            "first projection deletion missing"
        );
    }

    #[test]
    fn final_statement_edits_stop_before_following_comments() {
        let sql = "SELECT sensor_id, avg(reading) FROM metric_series WHERE (((((1 = 1) AND ((1 = 1) AND ((1 = 1) AND (sensor_id = 1)))) AND NOT FALSE) AND (NULLIF(1, 0) IS NOT NULL)) AND (1 = 1)) AND (NULLIF(1, 0) IS NOT NULL) GROUP BY sensor_id ORDER BY 1;\n/* fuzz_mutation_noise */\n";
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 1);
        let final_unit = units.last().unwrap();
        let edits = &final_unit.facts.as_ref().unwrap().edits;
        let order_by = edits
            .iter()
            .find(|edit| edit.label == "remove-order_by")
            .unwrap();
        assert_eq!(order_by.end, final_unit.end);
        assert_eq!(&sql[order_by.end..], ";\n/* fuzz_mutation_noise */\n");
    }
    #[test]
    fn window_function_candidates_do_not_cut_off_over() {
        let sql = "SELECT 1 FROM t WHERE 1 < sum(x) OVER (PARTITION BY y) AND TRUE;";
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        let edits = &units[0].facts.as_ref().unwrap().edits;
        let literal_edit = edits
            .iter()
            .find(|edit| {
                edit.category == NativeEditCategory::Literal
                    && &sql[edit.start..edit.end] == "TRUE"
                    && edit.replacement == "0"
            })
            .unwrap();
        let literal_candidate = format!(
            "{}{}{}",
            &sql[..literal_edit.start],
            literal_edit.replacement,
            &sql[literal_edit.end..]
        );
        assert_eq!(
            literal_candidate,
            "SELECT 1 FROM t WHERE 1 < sum(x) OVER (PARTITION BY y) AND 0;"
        );

        for edit in edits {
            let candidate = format!(
                "{}{}{}",
                &sql[..edit.start],
                edit.replacement,
                &sql[edit.end..]
            );
            assert!(
                Parser::parse_sql(&sqlparser::dialect::PostgreSqlDialect {}, &candidate).is_ok(),
                "{} emitted malformed candidate: {candidate}",
                edit.label
            );
        }
    }

    #[test]
    fn duckdb_dialect_parsing_and_statement_boundaries() {
        let sql = "SELECT range(5);\nSELECT {'a': 1, 'b': 2};\nCREATE TABLE t (\"QuotedCol\" INT, unquoted_col INT);";
        assert_eq!(count_units(sql, DialectKind::DuckDB), Ok(3));
        let mut ffi_count = 0;
        let status = unsafe { occam_sql_count(sql.as_ptr(), sql.len(), 4, &mut ffi_count) };
        assert_eq!(status, 0);
        assert_eq!(ffi_count, 3);

        let units = parse(sql, DialectKind::DuckDB).unwrap();
        assert_eq!(units.len(), 3);
        assert_eq!(&sql[units[0].start..units[0].end], "SELECT range(5)");
        assert_eq!(
            &sql[units[1].start..units[1].end],
            "SELECT {'a': 1, 'b': 2}"
        );
        assert_eq!(
            &sql[units[2].start..units[2].end],
            "CREATE TABLE t (\"QuotedCol\" INT, unquoted_col INT)"
        );

        let facts = facts(&units[2]);
        let table = table_def(facts).expect("duckdb table definition");
        let columns: Vec<_> = table
            .columns
            .iter()
            .map(|column| column.name.0.as_str())
            .collect();
        assert_eq!(columns, vec!["quotedcol", "unquoted_col"]);
    }

    #[test]
    fn clickhouse_dialect_parsing_and_statement_boundaries() {
        let sql = "SELECT number FROM numbers(10);\nCREATE TABLE events (EventId UInt64, event_name String) ENGINE = MergeTree ORDER BY EventId;";
        assert_eq!(count_units(sql, DialectKind::ClickHouse), Ok(2));
        let mut ffi_count = 0;
        let status = unsafe { occam_sql_count(sql.as_ptr(), sql.len(), 5, &mut ffi_count) };
        assert_eq!(status, 0);
        assert_eq!(ffi_count, 2);

        let units = parse(sql, DialectKind::ClickHouse).unwrap();
        assert_eq!(units.len(), 2);
        assert_eq!(
            &sql[units[0].start..units[0].end],
            "SELECT number FROM numbers(10)"
        );
        assert_eq!(
            &sql[units[1].start..units[1].end],
            "CREATE TABLE events (EventId UInt64, event_name String) ENGINE = MergeTree ORDER BY EventId"
        );
        let facts = facts(&units[1]);
        // ClickHouse identifiers are case-sensitive: EventId preserves case even unquoted.
        let table = table_def(facts).expect("clickhouse table definition");
        let columns: Vec<_> = table
            .columns
            .iter()
            .map(|column| column.name.0.as_str())
            .collect();
        assert_eq!(columns, vec!["EventId", "event_name"]);
    }

    #[test]
    fn oracle_dialect_parsing_and_statement_boundaries() {
        let sql = "SELECT 42 FROM dual;\nCREATE TABLE employees (id NUMBER(10) PRIMARY KEY, name VARCHAR2(100));\nSELECT a.id, b.name FROM a, b WHERE a.id = b.id(+);\nSELECT id, name FROM employees START WITH mgr_id IS NULL CONNECT BY PRIOR id = mgr_id;";
        assert_eq!(count_units(sql, DialectKind::Oracle), Ok(4));
        let mut ffi_count = 0;
        let status = unsafe { occam_sql_count(sql.as_ptr(), sql.len(), 6, &mut ffi_count) };
        assert_eq!(status, 0);
        assert_eq!(ffi_count, 4);

        let units = parse(sql, DialectKind::Oracle).unwrap();
        assert_eq!(units.len(), 4);
        assert_eq!(&sql[units[0].start..units[0].end], "SELECT 42 FROM dual");
        assert_eq!(
            &sql[units[1].start..units[1].end],
            "CREATE TABLE employees (id NUMBER(10) PRIMARY KEY, name VARCHAR2(100))"
        );
        assert_eq!(
            &sql[units[2].start..units[2].end],
            "SELECT a.id, b.name FROM a, b WHERE a.id = b.id(+)"
        );
        assert_eq!(
            &sql[units[3].start..units[3].end],
            "SELECT id, name FROM employees START WITH mgr_id IS NULL CONNECT BY PRIOR id = mgr_id"
        );

        for unit in &units {
            assert!(unit.facts.is_some());
            assert!(unit.parse_error.is_none());
        }

        let facts0 = facts(&units[0]);
        assert_eq!(relation_reads(facts0), vec!["DUAL"]);

        let facts1 = facts(&units[1]);
        let employees = table_def(facts1).expect("employees definition");
        assert_eq!(employees.name, Name(vec![Ident("EMPLOYEES".to_string())]));
        let columns: Vec<_> = employees
            .columns
            .iter()
            .map(|column| column.name.0.as_str())
            .collect();
        assert_eq!(columns, vec!["ID", "NAME"]);

        let facts2 = facts(&units[2]);
        let mut used = relation_reads(facts2);
        used.sort();
        assert_eq!(used, vec!["A", "B"]);

        let facts3 = facts(&units[3]);
        assert_eq!(relation_reads(facts3), vec!["EMPLOYEES"]);

        let quoted_sql = "CREATE TABLE dept (\"DeptId\" NUMBER(10), DeptName VARCHAR2(50));";
        let quoted_units = parse(quoted_sql, DialectKind::Oracle).unwrap();
        let quoted_facts = facts(&quoted_units[0]);
        let dept = table_def(quoted_facts).expect("dept definition");
        let columns: Vec<_> = dept
            .columns
            .iter()
            .map(|column| column.name.0.as_str())
            .collect();
        assert_eq!(columns, vec!["DeptId", "DEPTNAME"]);
    }

    #[test]
    fn additional_sqlparser_dialects_parsing_and_facts() {
        let test_cases = [
            (
                DialectKind::Ansi,
                7u32,
                "SELECT 1 FROM \"t1\";\nSELECT 2 FROM \"t2\";",
                2,
                "t1",
            ),
            (
                DialectKind::BigQuery,
                8u32,
                "SELECT `id` FROM `proj.dataset.tbl`;\nSELECT 42;",
                2,
                "proj.dataset.tbl",
            ),
            (
                DialectKind::Databricks,
                9u32,
                "CREATE TABLE `events` (`id` INT);\nSELECT `id` FROM `events`;",
                2,
                "events",
            ),
            (
                DialectKind::Hive,
                10u32,
                "CREATE TABLE `logs` (`msg` STRING);\nSELECT `msg` FROM `logs`;",
                2,
                "logs",
            ),
            (
                DialectKind::MsSql,
                11u32,
                "SELECT TOP 10 [id], [name] FROM [dbo].[users];\nSELECT 1;",
                2,
                "dbo.users",
            ),
            (
                DialectKind::Redshift,
                12u32,
                "CREATE TABLE dist_tbl (id INT) DISTKEY(id);\nSELECT id FROM dist_tbl;",
                2,
                "dist_tbl",
            ),
            (
                DialectKind::Snowflake,
                13u32,
                "SELECT \"ColA\" FROM \"MyTable\";\nSELECT 1;",
                2,
                "MyTable",
            ),
            (
                DialectKind::Spark,
                14u32,
                "CREATE TABLE `spark_tbl` (`x` INT);\nSELECT `x` FROM `spark_tbl`;",
                2,
                "spark_tbl",
            ),
            (
                DialectKind::Teradata,
                15u32,
                "SELECT \"id\" FROM \"tera_tbl\";\nSELECT 1;",
                2,
                "tera_tbl",
            ),
        ];

        for (kind, code, sql, expected_count, expected_table) in test_cases {
            assert_eq!(count_units(sql, kind), Ok(expected_count));
            let mut ffi_count = 0;
            let status = unsafe { occam_sql_count(sql.as_ptr(), sql.len(), code, &mut ffi_count) };
            assert_eq!(status, 0);
            assert_eq!(ffi_count, expected_count);

            let units = parse(sql, kind).unwrap();
            assert_eq!(units.len(), expected_count);
            for unit in &units {
                assert!(unit.facts.is_some());
                assert!(unit.parse_error.is_none());
            }

            let first_facts = facts(&units[0]);
            assert!(
                mentioned_relations(first_facts)
                    .iter()
                    .any(|name| name == expected_table),
                "Dialect {:?} should identify table {} in {:?}",
                code,
                expected_table,
                mentioned_relations(first_facts)
            );
        }
    }

    #[test]
    fn identifier_case_folding_follows_dialect_semantics() {
        let oracle_units = parse(
            "CREATE TABLE Dept (DeptName INT, \"QuotedName\" INT);",
            DialectKind::Oracle,
        )
        .unwrap();
        let oracle_facts = facts(&oracle_units[0]);
        let dept = table_def(oracle_facts).expect("oracle table definition");
        assert_eq!(dept.name, Name(vec![Ident("DEPT".to_string())]));
        let columns: Vec<_> = dept
            .columns
            .iter()
            .map(|column| column.name.0.as_str())
            .collect();
        assert_eq!(columns, vec!["DEPTNAME", "QuotedName"]);

        let postgres_units =
            parse("CREATE TABLE Dept (DeptName INT);", DialectKind::PostgreSql).unwrap();
        let postgres_facts = facts(&postgres_units[0]);
        let dept = table_def(postgres_facts).expect("postgres table definition");
        assert_eq!(dept.name, Name(vec![Ident("dept".to_string())]));
    }

    #[test]
    fn mysql_drop_index_records_its_table_use() {
        let units = parse("DROP INDEX ix ON t;", DialectKind::MySql).unwrap();
        let dropped = facts(&units[0])
            .ops
            .iter()
            .find_map(|op| match op {
                CatalogOp::Drop(drop) => Some(drop),
                _ => None,
            })
            .expect("drop op");
        assert_eq!(
            dropped.objects,
            vec![DropObject::Index {
                index: IndexRef::Table {
                    table: NameRef {
                        name: Name(vec![Ident("t".to_string())]),
                        temp: false,
                    },
                    name: Ident("ix".to_string()),
                },
            }]
        );
    }

    #[test]
    fn constraint_stripping_and_deeper_ast_pruning() {
        let sql = "CREATE TABLE t (id INT NOT NULL, name TEXT DEFAULT 'abc', PRIMARY KEY (id), FOREIGN KEY (id) REFERENCES p(id));";
        let units = parse(sql, DialectKind::Generic).unwrap();
        let edits = &units[0].facts.as_ref().unwrap().edits;
        let constraint_edits: Vec<_> = edits
            .iter()
            .filter(|e| e.category == NativeEditCategory::Constraint)
            .collect();
        assert!(
            constraint_edits
                .iter()
                .any(|e| e.label == "remove-column_constraint")
        );
        assert!(
            constraint_edits
                .iter()
                .any(|e| e.label == "remove-table_constraint")
        );

        let expr_sql = "SELECT CASE WHEN a = 1 THEN 42 ELSE 0 END, COALESCE(x, y), z IS NOT NULL, b BETWEEN 1 AND 10 FROM t;";
        let expr_units = parse(expr_sql, DialectKind::Generic).unwrap();
        let expr_edits = &expr_units[0].facts.as_ref().unwrap().edits;
        assert!(expr_edits.iter().any(|e| e.label == "simplify-case_result"));
        assert!(
            expr_edits
                .iter()
                .any(|e| e.label == "simplify-function_to_argument")
        );
        assert!(expr_edits.iter().any(|e| e.label == "simplify-is_null"));
        assert!(expr_edits.iter().any(|e| e.label == "simplify-between"));
    }

    #[test]
    fn search_path_default_and_current_user_stay_symbolic() {
        let units = parse("SET search_path TO DEFAULT;", DialectKind::PostgreSql).unwrap();
        let session = &facts(&units[0]).session;
        assert!(matches!(
            session.as_slice(),
            [SessionEffect::SetSearchPath {
                path: SearchPathValue::Default,
                ..
            }]
        ));

        let units = parse(
            "SET search_path TO \"$user\", public;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let session = &facts(&units[0]).session;
        let entries = match session.as_slice() {
            [
                SessionEffect::SetSearchPath {
                    path: SearchPathValue::Explicit { entries },
                    ..
                },
            ] => entries,
            other => panic!("unexpected session effects: {other:?}"),
        };
        assert_eq!(
            entries,
            &vec![
                SearchPathEntry::CurrentUser,
                SearchPathEntry::Name {
                    name: Name(vec![Ident("public".to_string())])
                }
            ]
        );

        let units = parse(
            "SET search_path = 'tenant, \"Mixed Case\"';",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let session = &facts(&units[0]).session;
        let entries = match session.as_slice() {
            [
                SessionEffect::SetSearchPath {
                    path: SearchPathValue::Explicit { entries },
                    ..
                },
            ] => entries,
            other => panic!("unexpected session effects: {other:?}"),
        };
        assert_eq!(
            entries,
            &vec![
                SearchPathEntry::Name {
                    name: Name(vec![Ident("tenant".to_string())])
                },
                SearchPathEntry::Name {
                    name: Name(vec![Ident("Mixed Case".to_string())])
                }
            ]
        );

        let units = parse(
            "SET search_path = '\"\" , public';",
            DialectKind::PostgreSql,
        )
        .unwrap();
        assert!(facts(&units[0]).session.is_empty());
        assert!(matches!(
            facts(&units[0]).barriers.as_slice(),
            [ScopedBarrier {
                reason: BarrierReason::UnknownSessionEffect,
                ..
            }]
        ));

        let units = parse("SET search_path TO $user, public;", DialectKind::PostgreSql).unwrap();
        let session = &facts(&units[0]).session;
        assert!(matches!(
            session.as_slice(),
            [SessionEffect::SetSearchPath {
                path: SearchPathValue::Explicit { entries },
                ..
            }] if entries == &vec![
                SearchPathEntry::CurrentUser,
                SearchPathEntry::Name {
                    name: Name(vec![Ident("public".to_string())])
                }
            ]
        ));
    }

    #[test]
    fn role_and_session_authorization_are_distinct_session_effects() {
        let units = parse(
            "SET ROLE analyst; RESET ROLE; SET SESSION AUTHORIZATION admin; RESET SESSION AUTHORIZATION;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        assert!(matches!(
            facts(&units[0]).session.as_slice(),
            [SessionEffect::SetCurrentUser { user, .. }] if user == &Ident("analyst".to_string())
        ));
        assert!(matches!(
            facts(&units[1]).session.as_slice(),
            [SessionEffect::ResetCurrentUser { .. }]
        ));
        assert!(matches!(
            facts(&units[2]).session.as_slice(),
            [SessionEffect::SetSessionUser { user, .. }] if user == &Ident("admin".to_string())
        ));
        assert!(matches!(
            facts(&units[3]).session.as_slice(),
            [SessionEffect::ResetSessionUser { .. }]
        ));
    }

    #[test]
    fn mysql_schema_and_database_drops_always_cascade() {
        for sql in ["DROP SCHEMA db;", "DROP DATABASE db;"] {
            let units = parse(sql, DialectKind::MySql).unwrap();
            assert!(matches!(
                facts(&units[0]).ops.as_slice(),
                [CatalogOp::Drop(DropOp {
                    objects,
                    mode: DropMode::Cascade,
                })] if matches!(objects.as_slice(), [DropObject::Schema { .. }])
            ));
        }
    }

    #[test]
    fn qualified_columns_resolve_through_table_aliases() {
        let units = parse("SELECT x.id FROM actual AS x;", DialectKind::SQLite).unwrap();
        let found = reads(facts(&units[0]));
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].columns, vec![Ident("id".to_string())]);

        let units = parse("SELECT other.id FROM actual AS x;", DialectKind::SQLite).unwrap();
        let found = reads(facts(&units[0]));
        assert_eq!(found.len(), 1);
        assert!(found[0].columns.is_empty(), "{:?}", found[0].columns);

        // BigQuery preserves relation-name case but folds column names. The
        // qualifier must use relation folding so it still matches the alias or
        // table name.
        let units = parse(
            "SELECT T.id FROM dataset.MyTable AS T;",
            DialectKind::BigQuery,
        )
        .unwrap();
        let found = reads(facts(&units[0]));
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].columns, vec![Ident("id".to_string())]);

        let units = parse(
            "SELECT dataset.MyTable.id FROM dataset.MyTable;",
            DialectKind::BigQuery,
        )
        .unwrap();
        let found = reads(facts(&units[0]));
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].columns, vec![Ident("id".to_string())]);
    }

    #[test]
    fn named_table_constraints_are_removed_with_their_prefix() {
        let sql = "CREATE TABLE t (id INT, CONSTRAINT pk PRIMARY KEY (id), CONSTRAINT fk FOREIGN KEY (id) REFERENCES p(id), name TEXT);";
        let units = parse(sql, DialectKind::Generic).unwrap();
        let edits = &units[0].facts.as_ref().unwrap().edits;
        let removed: Vec<_> = edits
            .iter()
            .filter(|edit| edit.label == "remove-table_constraint")
            .map(|edit| &sql[edit.start..edit.end])
            .collect();
        assert_eq!(removed.len(), 2);
        assert!(
            removed
                .iter()
                .any(|text| text.contains("CONSTRAINT pk PRIMARY KEY (id)"))
        );
        assert!(
            removed
                .iter()
                .any(|text| { text.contains("CONSTRAINT fk FOREIGN KEY (id) REFERENCES p(id)") })
        );
    }

    #[test]
    fn explain_reads_schema_metadata_unless_analyze_executes() {
        let units = parse("EXPLAIN SELECT * FROM t;", DialectKind::PostgreSql).unwrap();
        let found = reads(facts(&units[0]));
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].mode, ReadMode::Schema);
        assert!(
            facts(&units[0])
                .ops
                .iter()
                .all(|op| matches!(op, CatalogOp::Read(_)))
        );

        let units = parse("EXPLAIN ANALYZE SELECT * FROM t;", DialectKind::PostgreSql).unwrap();
        let found = reads(facts(&units[0]));
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].mode, ReadMode::Data);

        let units = parse("EXPLAIN INSERT INTO t VALUES (1);", DialectKind::PostgreSql).unwrap();
        let nested = facts(&units[0]);
        assert!(writes(nested).is_empty());
        assert!(nested.session.is_empty());
        assert!(nested.pre_tx.is_empty());
        assert!(nested.post_tx.is_empty());
        assert!(nested.ops.iter().all(|op| matches!(op, CatalogOp::Read(_))));

        let units = parse(
            "EXPLAIN WITH d AS (DELETE FROM t RETURNING *) SELECT * FROM d;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let planned = facts(&units[0]);
        assert!(writes(planned).is_empty());
        assert!(planned.pre_tx.is_empty());
        assert!(planned.post_tx.is_empty());
        assert_eq!(relation_reads(planned), vec!["t".to_string()]);
        assert_eq!(reads(planned)[0].mode, ReadMode::Schema);

        let units = parse(
            "EXPLAIN ANALYZE WITH d AS (DELETE FROM t RETURNING *) SELECT * FROM d;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let executed = facts(&units[0]);
        assert_eq!(
            writes(executed)
                .iter()
                .map(|write| name_text(&write.target.name))
                .collect::<Vec<_>>(),
            vec!["t".to_string()]
        );
    }

    #[test]
    fn pragma_foreign_keys_only_changes_state_on_assignment() {
        let units = parse("PRAGMA foreign_keys;", DialectKind::SQLite).unwrap();
        assert!(facts(&units[0]).session.is_empty());
        assert!(facts(&units[0]).barriers.is_empty());

        let units = parse("PRAGMA foreign_keys = 0;", DialectKind::SQLite).unwrap();
        assert!(matches!(
            facts(&units[0]).session.as_slice(),
            [SessionEffect::SetForeignKeyChecks { value: false, .. }]
        ));

        let units = parse("PRAGMA foreign_keys = 'maybe';", DialectKind::SQLite).unwrap();
        assert!(facts(&units[0]).session.is_empty());
        assert!(matches!(
            facts(&units[0]).barriers.as_slice(),
            [ScopedBarrier {
                scope: BarrierScope::SessionState,
                reason: BarrierReason::UnknownSessionEffect,
                ..
            }]
        ));
    }

    #[test]
    fn session_init_resolves_the_dialect_foreign_key_default() {
        let config = |dialect, foreign_key_checks| ParseConfig {
            dialect,
            server: ServerConfig {
                foreign_key_checks,
                ..ServerConfig::default()
            },
        };
        assert!(
            !crate::ffi::session_init(&config(DialectKind::SQLite, None)).foreign_key_checks,
            "SQLite defaults foreign_keys to OFF"
        );
        assert!(
            crate::ffi::session_init(&config(DialectKind::PostgreSql, None)).foreign_key_checks,
            "PostgreSQL defaults foreign_key_checks to ON"
        );
        assert!(
            !crate::ffi::session_init(&config(DialectKind::PostgreSql, Some(false)))
                .foreign_key_checks,
            "an explicit server setting overrides the dialect default"
        );
        assert!(
            crate::ffi::session_init(&config(DialectKind::SQLite, Some(true))).foreign_key_checks,
            "an explicit SQLite setting overrides the dialect default"
        );
    }

    #[test]
    fn using_index_and_rename_carry_index_schema_placement() {
        let using_index = |dialect| {
            let units = parse(
                "CREATE TABLE s.t (a INT); ALTER TABLE s.t ADD PRIMARY KEY USING INDEX t_idx;",
                dialect,
            )
            .unwrap();
            facts(&units[1])
                .ops
                .iter()
                .find_map(|op| match op {
                    CatalogOp::AlterTable(alter) => {
                        alter.changes.iter().find_map(|change| match change {
                            AlterChange::AddConstraint {
                                constraint:
                                    Constraint::UsingIndex {
                                        schema_placement, ..
                                    },
                            } => Some(*schema_placement),
                            _ => None,
                        })
                    }
                    _ => None,
                })
                .expect("using-index constraint")
        };
        assert_eq!(
            using_index(DialectKind::Oracle),
            IndexSchemaPlacement::CurrentSchema
        );
        assert_eq!(
            using_index(DialectKind::PostgreSql),
            IndexSchemaPlacement::TableSchema
        );

        let rename_placement = |dialect| {
            let units = parse(
                "CREATE TABLE s.t (a INT); ALTER TABLE s.t RENAME TO u;",
                dialect,
            )
            .unwrap();
            facts(&units[1])
                .ops
                .iter()
                .find_map(|op| match op {
                    CatalogOp::Rename(RenameOp::Relation {
                        index_schema_placement,
                        ..
                    }) => Some(*index_schema_placement),
                    _ => None,
                })
                .expect("rename op")
        };
        assert_eq!(
            rename_placement(DialectKind::Oracle),
            IndexSchemaPlacement::CurrentSchema
        );
        assert_eq!(
            rename_placement(DialectKind::PostgreSql),
            IndexSchemaPlacement::TableSchema
        );
    }

    #[test]
    fn reset_foreign_keys_restores_the_initial_setting() {
        let units = parse(
            "SET foreign_key_checks = 0; RESET foreign_key_checks;",
            DialectKind::MySql,
        )
        .unwrap();
        assert!(matches!(
            facts(&units[0]).session.as_slice(),
            [SessionEffect::SetForeignKeyChecks { value: false, .. }]
        ));
        assert!(matches!(
            facts(&units[1]).session.as_slice(),
            [SessionEffect::ResetForeignKeyChecks {
                scope: SessionScope::Session
            }]
        ));
    }

    #[test]
    fn use_warehouse_and_role_are_distinct_session_variables() {
        let units = parse(
            "USE WAREHOUSE wh; USE ROLE analyst;",
            DialectKind::Snowflake,
        )
        .unwrap();
        assert!(matches!(
            facts(&units[0]).session.as_slice(),
            [SessionEffect::SetVariable {
                name,
                value: SessionValue::Text { value },
                ..
            }] if name_text(name) == "warehouse" && value == "wh"
        ));
        assert!(matches!(
            facts(&units[1]).session.as_slice(),
            [SessionEffect::SetVariable {
                name,
                value: SessionValue::Text { value },
                ..
            }] if name_text(name) == "role" && value == "analyst"
        ));
    }

    #[test]
    fn clickhouse_ephemeral_columns_are_not_generated_or_defaulted() {
        let units = parse(
            "CREATE TABLE t (a Int32 EPHEMERAL, b Int32 DEFAULT 1) ENGINE = Memory;",
            DialectKind::ClickHouse,
        )
        .unwrap();
        let table = table_def(facts(&units[0])).expect("clickhouse table definition");
        assert_eq!(table.columns.len(), 2);
        assert!(!table.columns[0].generated);
        assert!(!table.columns[0].has_default);
        assert!(table.columns[1].has_default);
    }

    #[test]
    fn change_column_only_renames_when_the_name_changes() {
        let units = parse("ALTER TABLE t CHANGE COLUMN a a INT;", DialectKind::MySql).unwrap();
        assert!(
            !facts(&units[0])
                .ops
                .iter()
                .any(|op| matches!(op, CatalogOp::Rename(RenameOp::Column { .. })))
        );

        let units = parse("ALTER TABLE t CHANGE COLUMN a b INT;", DialectKind::MySql).unwrap();
        assert!(facts(&units[0]).ops.iter().any(|op| matches!(
            op,
            CatalogOp::Rename(RenameOp::Column { from, to, .. })
                if from == &Ident("a".to_string()) && to == &Ident("b".to_string())
        )));
    }

    #[test]
    fn end_commits_a_transaction_but_try_blocks_do_not() {
        let units = parse("BEGIN; END;", DialectKind::PostgreSql).unwrap();
        assert!(matches!(
            facts(&units[1]).post_tx.as_slice(),
            [TxEffect::Commit { chain: false }]
        ));

        let units = parse("END TRY; END CATCH;", DialectKind::MsSql).unwrap();
        for unit in &units {
            assert!(
                !facts(unit)
                    .post_tx
                    .iter()
                    .any(|effect| matches!(effect, TxEffect::Commit { .. })),
                "END TRY/END CATCH must not commit"
            );
        }
    }

    #[test]
    fn multi_table_update_writes_every_assigned_target() {
        let units = parse(
            "UPDATE t1 JOIN t2 ON t1.id = t2.id SET t1.a = 1, t2.b = 2;",
            DialectKind::MySql,
        )
        .unwrap();
        let mut targets: Vec<(String, Vec<Ident>)> = writes(facts(&units[0]))
            .into_iter()
            .map(|write| (name_text(&write.target.name), write.columns.clone()))
            .collect();
        targets.sort_by(|left, right| left.0.cmp(&right.0));
        assert_eq!(
            targets,
            vec![
                ("t1".to_string(), vec![Ident("a".to_string())]),
                ("t2".to_string(), vec![Ident("b".to_string())]),
            ]
        );
    }

    #[test]
    fn create_view_records_projection_columns_and_definition_reads() {
        let units = parse(
            "CREATE VIEW v AS SELECT a, b AS c FROM t;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let view = facts(&units[0])
            .ops
            .iter()
            .find_map(|op| match op {
                CatalogOp::DefineView(view) => Some(view),
                _ => None,
            })
            .expect("view definition");
        assert_eq!(
            view.columns,
            vec![Ident("a".to_string()), Ident("c".to_string())]
        );
        assert_eq!(view.sources.len(), 1);
        assert_eq!(view.sources[0].mode, ReadMode::Schema);
        assert_eq!(
            view.sources[0].columns,
            vec![Ident("a".to_string()), Ident("b".to_string())]
        );

        let units = parse(
            "CREATE MATERIALIZED VIEW mv AS SELECT a FROM t;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let view = facts(&units[0])
            .ops
            .iter()
            .find_map(|op| match op {
                CatalogOp::DefineView(view) => Some(view),
                _ => None,
            })
            .expect("materialized view definition");
        assert_eq!(view.sources[0].mode, ReadMode::Data);
    }

    #[test]
    fn sibling_subquery_aliases_do_not_leak_columns() {
        let units = parse(
            "SELECT (SELECT x.id FROM a AS x), (SELECT x.name FROM b AS x) FROM c;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let found = reads(facts(&units[0]));
        let columns = |table: &str| {
            found
                .iter()
                .find(|read| name_text(&read.source.name) == table)
                .expect("relation read")
                .columns
                .clone()
        };
        assert_eq!(columns("a"), vec![Ident("id".to_string())]);
        assert_eq!(columns("b"), vec![Ident("name".to_string())]);
        assert!(columns("c").is_empty());
    }

    #[test]
    fn correlated_aliases_remain_visible_from_nested_queries() {
        let units = parse(
            "SELECT (SELECT x.id FROM a) FROM b AS x;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let found = reads(facts(&units[0]));
        let columns = |table: &str| {
            found
                .iter()
                .find(|read| name_text(&read.source.name) == table)
                .expect("relation read")
                .columns
                .clone()
        };
        assert!(columns("a").is_empty());
        assert_eq!(columns("b"), vec![Ident("id".to_string())]);
    }

    #[test]
    fn literal_search_path_splits_commas_and_preserves_quotes() {
        let units = parse(
            "SET search_path = 'tenant, \"MixedCase\", $user';",
            DialectKind::PostgreSql,
        )
        .unwrap();
        assert!(matches!(
            facts(&units[0]).session.as_slice(),
            [SessionEffect::SetSearchPath {
                path: SearchPathValue::Explicit { entries },
                ..
            }] if entries == &vec![
                SearchPathEntry::Name {
                    name: Name(vec![Ident("tenant".to_string())])
                },
                SearchPathEntry::Name {
                    name: Name(vec![Ident("MixedCase".to_string())])
                },
                SearchPathEntry::CurrentUser,
            ]
        ));

        for sql in [
            "SET search_path = 'a b, c';",
            "SET search_path = '\"a\" \"b\", c';",
            "SET search_path = 'a\"b\", c';",
            "SET search_path = '\"unterminated, c';",
        ] {
            let units = parse(sql, DialectKind::PostgreSql).unwrap();
            assert!(
                facts(&units[0]).session.is_empty(),
                "invalid search path must not change the session for {sql}"
            );
            assert!(
                matches!(
                    facts(&units[0]).barriers.as_slice(),
                    [ScopedBarrier {
                        reason: BarrierReason::UnknownSessionEffect,
                        ..
                    }]
                ),
                "invalid search path must raise an unknown-session barrier for {sql}"
            );
        }
    }

    #[test]
    fn sequence_literals_use_identifier_parsing() {
        let units = parse(
            "SELECT nextval('tenant.\"MixedSeq\"');",
            DialectKind::PostgreSql,
        )
        .unwrap();
        assert_eq!(
            sequences(facts(&units[0])),
            vec!["tenant.MixedSeq".to_string()]
        );

        for sql in [
            "SELECT nextval('a\"b\"c');",
            "SELECT nextval('foo bar.seq');",
            "SELECT nextval('\"\"');",
        ] {
            let units = parse(sql, DialectKind::PostgreSql).unwrap();
            assert!(
                sequences(facts(&units[0])).is_empty(),
                "invalid sequence literal must not advance a sequence for {sql}"
            );
        }
    }

    #[test]
    fn set_operation_output_columns_require_matching_arity() {
        let units = parse(
            "CREATE VIEW v AS SELECT a FROM t UNION SELECT b FROM u;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let view = facts(&units[0])
            .ops
            .iter()
            .find_map(|op| match op {
                CatalogOp::DefineView(view) => Some(view),
                _ => None,
            })
            .expect("view definition");
        assert_eq!(view.columns, vec![Ident("a".to_string())]);

        let units = parse(
            "CREATE VIEW v AS SELECT a FROM t UNION SELECT b, c FROM u;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let view = facts(&units[0])
            .ops
            .iter()
            .find_map(|op| match op {
                CatalogOp::DefineView(view) => Some(view),
                _ => None,
            })
            .expect("view definition");
        assert!(
            view.columns.is_empty(),
            "a mismatched set-operation arity must not claim a closed column set"
        );
    }

    #[test]
    fn implicit_commit_is_limited_to_mysql_and_oracle() {
        let units = parse("LOCK TABLES t WRITE;", DialectKind::MySql).unwrap();
        assert!(matches!(
            facts(&units[0]).post_tx.as_slice(),
            [TxEffect::Commit { chain: false }]
        ));

        let units = parse("CREATE TABLE t (id INT);", DialectKind::Snowflake).unwrap();
        assert!(facts(&units[0]).post_tx.is_empty());
    }

    #[test]
    fn implicit_commit_covers_grants_and_routines() {
        for (sql, dialect) in [
            ("GRANT SELECT ON t TO u;", DialectKind::MySql),
            ("GRANT SELECT ON t TO u;", DialectKind::Oracle),
            ("CREATE USER u;", DialectKind::Oracle),
            (
                "CREATE PROCEDURE p() AS BEGIN SELECT 1; END;",
                DialectKind::MySql,
            ),
        ] {
            let units = parse(sql, dialect).unwrap_or_else(|error| panic!("{sql}: {error}"));
            let facts = units[0]
                .facts
                .as_ref()
                .unwrap_or_else(|| panic!("{sql} ({dialect:?}) produced no facts"));
            assert!(
                matches!(
                    facts.post_tx.as_slice(),
                    [TxEffect::Commit { chain: false }]
                ),
                "{sql} should implicitly commit"
            );
        }
    }

    #[test]
    fn set_operation_branches_keep_separate_alias_scopes() {
        let units = parse(
            "SELECT x.id FROM a AS x UNION ALL SELECT x.name FROM b AS x;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let found = reads(facts(&units[0]));
        let columns = |table: &str| {
            found
                .iter()
                .find(|read| name_text(&read.source.name) == table)
                .expect("relation read")
                .columns
                .clone()
        };
        assert_eq!(columns("a"), vec![Ident("id".to_string())]);
        assert_eq!(columns("b"), vec![Ident("name".to_string())]);
    }

    #[test]
    fn aliases_hide_the_original_relation_name() {
        let units = parse("SELECT t.a FROM tbl AS x;", DialectKind::PostgreSql).unwrap();
        let found = reads(facts(&units[0]));
        let table = found
            .iter()
            .find(|read| name_text(&read.source.name) == "tbl")
            .expect("relation read");
        assert!(table.columns.is_empty());
    }

    #[test]
    fn derived_table_visibility_follows_lateral() {
        let units = parse(
            "SELECT d.x FROM a, (SELECT a.id AS x) AS d;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let found = reads(facts(&units[0]));
        let columns = |table: &str| {
            found
                .iter()
                .find(|read| name_text(&read.source.name) == table)
                .map(|read| read.columns.clone())
                .unwrap_or_default()
        };
        assert!(
            columns("a").is_empty(),
            "a non-LATERAL derived table cannot see sibling aliases"
        );

        let units = parse(
            "SELECT d.x FROM a, LATERAL (SELECT a.id AS x) AS d;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let found = reads(facts(&units[0]));
        let a = found
            .iter()
            .find(|read| name_text(&read.source.name) == "a")
            .expect("relation read");
        assert_eq!(a.columns, vec![Ident("id".to_string())]);
    }

    #[test]
    fn update_reads_target_rows() {
        let units = parse("UPDATE t SET a = 1 WHERE b = 2;", DialectKind::PostgreSql).unwrap();
        let target_read = writes(facts(&units[0]))
            .into_iter()
            .find(|write| name_text(&write.target.name) == "t")
            .and_then(|write| {
                write
                    .reads
                    .iter()
                    .find(|read| name_text(&read.source.name) == "t")
            })
            .expect("target read");
        assert_eq!(target_read.columns, vec![Ident("b".to_string())]);
    }

    #[test]
    fn delete_reads_target_rows() {
        let units = parse("DELETE FROM t WHERE b = 2;", DialectKind::PostgreSql).unwrap();
        let target_read = writes(facts(&units[0]))
            .into_iter()
            .find(|write| name_text(&write.target.name) == "t")
            .and_then(|write| {
                write
                    .reads
                    .iter()
                    .find(|read| name_text(&read.source.name) == "t")
            })
            .expect("target read");
        assert_eq!(target_read.columns, vec![Ident("b".to_string())]);
    }

    #[test]
    fn merge_reads_target_rows() {
        let units = parse(
            "MERGE INTO t USING s ON t.id = s.id WHEN MATCHED THEN UPDATE SET a = s.a;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let write = writes(facts(&units[0]))
            .into_iter()
            .find(|write| name_text(&write.target.name) == "t")
            .expect("merge target");
        assert!(
            write
                .reads
                .iter()
                .any(|read| name_text(&read.source.name) == "t")
        );
        assert!(
            write
                .reads
                .iter()
                .any(|read| name_text(&read.source.name) == "s")
        );
    }

    #[test]
    fn update_from_resolves_alias_to_physical_table() {
        let units = parse(
            "UPDATE a SET x = 1 FROM actual_table AS a WHERE a.id = 2;",
            DialectKind::MsSql,
        )
        .unwrap();
        let targets: Vec<String> = writes(facts(&units[0]))
            .into_iter()
            .map(|write| name_text(&write.target.name))
            .collect();
        assert_eq!(targets, vec!["actual_table".to_string()]);
        assert!(
            writes(facts(&units[0]))
                .iter()
                .flat_map(|write| write.reads.iter())
                .any(|read| name_text(&read.source.name) == "actual_table")
        );
    }

    #[test]
    fn delete_alias_resolves_to_physical_table() {
        let units = parse(
            "DELETE a FROM actual_table AS a WHERE a.id = 2;",
            DialectKind::MySql,
        )
        .unwrap();
        let writes = writes(facts(&units[0]));
        assert_eq!(
            writes
                .iter()
                .map(|write| name_text(&write.target.name))
                .collect::<Vec<_>>(),
            vec!["actual_table".to_string()]
        );
        assert!(
            writes[0]
                .reads
                .iter()
                .any(|read| name_text(&read.source.name) == "actual_table")
        );
    }

    #[test]
    fn ctas_and_partial_projections_keep_known_columns() {
        let units = parse(
            "CREATE TABLE copy AS SELECT a, b AS c FROM t;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let table = table_def(facts(&units[0])).expect("table definition");
        assert_eq!(
            table
                .columns
                .iter()
                .map(|column| column.name.0.as_str())
                .collect::<Vec<_>>(),
            vec!["a", "c"]
        );

        let units = parse(
            "CREATE VIEW v AS SELECT a, count(*) AS n FROM t;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let view = facts(&units[0])
            .ops
            .iter()
            .find_map(|op| match op {
                CatalogOp::DefineView(view) => Some(view),
                _ => None,
            })
            .expect("view definition");
        assert_eq!(
            view.columns,
            vec![Ident("a".to_string()), Ident("n".to_string())]
        );
    }

    fn sequences(facts: &NativeFacts) -> Vec<String> {
        facts
            .ops
            .iter()
            .filter_map(|op| match op {
                CatalogOp::WriteSequence(write) => Some(name_text(&write.sequence.name)),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn nextval_writes_sequence_state() {
        let units = parse("SELECT nextval('s');", DialectKind::PostgreSql).unwrap();
        assert_eq!(sequences(facts(&units[0])), vec!["s".to_string()]);

        let units = parse("SELECT s.NEXTVAL FROM DUAL;", DialectKind::Oracle).unwrap();
        assert_eq!(sequences(facts(&units[0])), vec!["S".to_string()]);
    }

    #[test]
    fn sequence_state_advances_only_for_evaluated_ddl() {
        for sql in [
            "CREATE MATERIALIZED VIEW mv AS SELECT nextval('s');",
            "CREATE TABLE t AS SELECT nextval('s');",
            "ALTER TABLE t ADD COLUMN c INT DEFAULT nextval('s');",
        ] {
            let units = parse(sql, DialectKind::PostgreSql).unwrap();
            assert_eq!(
                sequences(facts(&units[0])),
                vec!["s".to_string()],
                "expected a sequence write for {sql}"
            );
        }

        for sql in [
            "CREATE VIEW v AS SELECT nextval('s');",
            "CREATE TABLE t AS SELECT nextval('s') WITH NO DATA;",
            "ALTER TABLE t ALTER COLUMN c SET DEFAULT nextval('s');",
        ] {
            let units = parse(sql, DialectKind::PostgreSql).unwrap();
            assert!(
                sequences(facts(&units[0])).is_empty(),
                "expected no sequence write for {sql}"
            );
        }
    }

    #[test]
    fn wildcard_ctas_does_not_record_partial_columns() {
        for sql in [
            "CREATE TABLE t AS SELECT * FROM src;",
            "CREATE TABLE t AS SELECT *, 1 AS extra FROM src;",
        ] {
            let units = parse(sql, DialectKind::PostgreSql).unwrap();
            let table = table_def(facts(&units[0])).expect("table definition");
            assert!(
                table.columns.is_empty(),
                "wildcard projection must not record a partial column set for {sql}"
            );
        }

        let units = parse("CREATE TABLE t (a, b) AS SELECT 1, 2;", DialectKind::SQLite).unwrap();
        let table = table_def(facts(&units[0])).expect("table definition");
        assert_eq!(
            table
                .columns
                .iter()
                .map(|column| column.name.0.as_str())
                .collect::<Vec<_>>(),
            vec!["a", "b"]
        );
    }

    #[test]
    fn nested_dml_relations_are_attributed_only_to_the_nested_write() {
        for sql in [
            "WITH u AS (UPDATE t SET a = 1 FROM other WHERE t.id = other.id RETURNING *) SELECT * FROM u;",
            "WITH m AS (MERGE INTO t USING other ON t.id = other.id WHEN MATCHED THEN UPDATE SET a = 1 RETURNING *) SELECT * FROM m;",
        ] {
            let units = parse(sql, DialectKind::PostgreSql).unwrap();
            let facts = facts(&units[0]);
            assert!(
                relation_reads(facts).is_empty(),
                "nested DML sources must not leak into outer reads for {sql}"
            );
            let found = writes(facts);
            assert_eq!(found.len(), 1, "expected one write for {sql}");
            assert_eq!(name_text(&found[0].target.name), "t");
            let mut write_reads: Vec<String> = found[0]
                .reads
                .iter()
                .map(|read| name_text(&read.source.name))
                .collect();
            write_reads.sort();
            assert_eq!(
                write_reads,
                vec!["other".to_string(), "t".to_string()],
                "the nested write must keep its own reads for {sql}"
            );
        }
    }

    #[test]
    fn create_view_applies_nested_dml_only_when_materialized() {
        let units = parse(
            "CREATE MATERIALIZED VIEW mv AS WITH d AS (DELETE FROM t RETURNING *) SELECT * FROM d;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let materialized = facts(&units[0]);
        assert_eq!(
            writes(materialized)
                .iter()
                .map(|write| name_text(&write.target.name))
                .collect::<Vec<_>>(),
            vec!["t".to_string()]
        );
        assert!(materialized.barriers.is_empty());

        let units = parse(
            "CREATE VIEW v AS WITH d AS (DELETE FROM t RETURNING *) SELECT * FROM d;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let plain = facts(&units[0]);
        assert!(writes(plain).is_empty());
        assert!(matches!(
            plain.barriers.as_slice(),
            [ScopedBarrier {
                reason: BarrierReason::UnknownView,
                ..
            }]
        ));
    }

    #[test]
    fn nested_data_modifying_ctes_are_analyzed_once() {
        let units = parse(
            "WITH outer_cte AS (WITH inner_cte AS (INSERT INTO t VALUES (1) RETURNING id) INSERT INTO t VALUES (2) RETURNING id) INSERT INTO t VALUES (3);",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let targets: Vec<String> = writes(facts(&units[0]))
            .into_iter()
            .map(|write| name_text(&write.target.name))
            .collect();
        assert_eq!(
            targets,
            vec!["t".to_string(), "t".to_string(), "t".to_string()]
        );
    }
}

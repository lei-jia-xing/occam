use serde::Serialize;
use std::borrow::Cow;

mod actions;
mod edits;
mod ffi;
mod parser;
mod source;

pub use ffi::{occam_sql_count, occam_sql_free, occam_sql_parse, occam_sql_parse_error};

#[cfg(test)]
use parser::{count_units, parse_units};
#[cfg(test)]
use sqlparser::dialect::SQLiteDialect;
#[cfg(test)]
use sqlparser::parser::Parser;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum DialectKind {
    Generic,
    PostgreSql,
    MySql,
    SQLite,
    DuckDB,
    ClickHouse,
    Oracle,
    Ansi,
    BigQuery,
    Databricks,
    Hive,
    MsSql,
    Redshift,
    Snowflake,
    Spark,
    Teradata,
}

#[derive(Serialize)]
struct NativeResponse {
    units: Vec<NativeUnit>,
}

#[derive(Serialize)]
struct NativeUnit {
    start: usize,
    end: usize,
    parse_error: Option<String>,
    facts: Option<NativeFacts>,
}

#[derive(Serialize)]
struct NativeFacts {
    actions: Vec<NativeAction>,
    edits: Vec<NativeEdit>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize)]
#[serde(rename_all = "snake_case")]
enum NativeObjectKind {
    Table,
    TemporaryTable,
    Column,
    Index,
    View,
    Trigger,
    Barrier,
    SessionVariable,
    Transaction,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize)]
#[serde(rename_all = "snake_case")]
enum NativeActionType {
    Def,
    Use,
    Mod,
    Drop,
    RenameFrom,
    RenameTo,
    Barrier,
    TxBegin,
    TxCommit,
    TxRollback,
    TxSavepoint,
    TxRollbackTo,
    TxRelease,
    Cascade,
    OnCommitDrop,
    OnCommitDeleteRows,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize)]
struct NativeAction {
    kind: NativeObjectKind,
    action: NativeActionType,
    name: String,
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

    #[test]
    fn statement_boundaries_and_source_mapping() {
        let sql = "-- café\r\nSELECT 'x;y';\r\nSELECT 2;";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert_eq!(&sql[units[0].start..units[0].end], "SELECT 'x;y'");
        assert_eq!(&sql[units[1].start..units[1].end], "SELECT 2");
    }
    #[test]
    fn column_qualifiers_require_a_structural_match() {
        let sql = "SELECT missing.id FROM actual;";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        let actions = &units[0].facts.as_ref().unwrap().actions;
        assert!(!actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column && action.name == "actual\u{1f}id"
        }));

        let sql = "SELECT t.id FROM public.t;";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        let actions = &units[0].facts.as_ref().unwrap().actions;
        assert!(actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column && action.name == "public.t\u{1f}id"
        }));

        let sql = "SELECT other.t.id FROM public.t;";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        let actions = &units[0].facts.as_ref().unwrap().actions;
        assert!(!actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column && action.name == "public.t\u{1f}id"
        }));
    }

    #[test]
    fn invalid_middle_unit_does_not_hide_later_statement() {
        let sql = "SELECT 1; SELECT FROM; SELECT 3";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 3);
        assert!(units[1].facts.is_none());
        assert!(units[1].parse_error.is_some());
        assert!(units[2].facts.is_some());
    }

    #[test]
    fn parser_and_tokenizer_errors_are_unit_local() {
        let sql = "SELECT 1 FROM; SELECT 2;";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert!(units[0].facts.is_none());
        assert!(units[0].parse_error.is_some());
        assert!(units[1].facts.is_some());

        let sql = "SELECT 1; SELECT FROM";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert_eq!(&sql[units[1].start..units[1].end], "SELECT FROM");
        assert!(units[1].facts.is_none());

        let sql = "SELECT 1; SELECT 'unterminated";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert!(units[0].facts.is_some());
        assert_eq!(&sql[units[1].start..units[1].end], "SELECT 'unterminated");
        assert!(units[1].parse_error.is_some());
        assert_eq!(count_units(sql, DialectKind::SQLite).unwrap(), 2);

        let sql = "SELECT 1 FROM; SELECT 'unterminated";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert_eq!(&sql[units[0].start..units[0].end], "SELECT 1 FROM");
        assert_eq!(&sql[units[1].start..units[1].end], "SELECT 'unterminated");
        assert!(units[0].end <= units[1].start);
        assert_eq!(count_units(sql, DialectKind::SQLite).unwrap(), 2);
    }

    #[test]
    fn missing_delimiter_coalesces_through_its_semicolon() {
        let sql = "SELECT 1 CREATE TABLE t (x INT); SELECT 2;";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
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
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
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
        assert_eq!(units[1].facts.as_ref().unwrap().actions.len(), 0);
    }

    #[test]
    fn values_types_and_cte_edits_keep_ast_boundaries() {
        let values_sql = "INSERT INTO t(a,b) VALUES (1,2),(42,43);";
        let values = parse_units(values_sql, DialectKind::SQLite).unwrap();
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
        let types = parse_units(type_sql, DialectKind::PostgreSql).unwrap();
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
        let cte = parse_units(cte_sql, DialectKind::PostgreSql).unwrap();
        let uses: Vec<_> = cte[0]
            .facts
            .as_ref()
            .unwrap()
            .actions
            .iter()
            .filter(|action| {
                action.kind == NativeObjectKind::Table && action.action == NativeActionType::Use
            })
            .map(|action| action.name.as_str())
            .collect();
        assert_eq!(uses, vec!["source"]);
    }

    #[test]
    fn quoted_dots_and_trigger_drop_keep_canonical_actions() {
        let sql = "CREATE TABLE t (\"a.b\" INT); INSERT INTO t (\"a.b\") VALUES (1); ALTER TABLE \"old.schema\".t RENAME TO \"new.name\"; DROP TRIGGER tr ON t;";
        let units = parse_units(sql, DialectKind::PostgreSql).unwrap();
        let inserted = &units[1].facts.as_ref().unwrap().actions;
        assert!(inserted.iter().any(|action| {
            action.kind == NativeObjectKind::Column
                && action.action == NativeActionType::Mod
                && action.name == "t\u{1f}a\\.b"
        }));
        let renamed = &units[2].facts.as_ref().unwrap().actions;
        assert!(renamed.iter().any(|action| {
            action.action == NativeActionType::RenameTo && action.name == "old\\.schema.new\\.name"
        }));
        let dropped = &units[3].facts.as_ref().unwrap().actions;
        assert!(dropped.iter().any(|action| {
            action.kind == NativeObjectKind::Trigger
                && action.action == NativeActionType::Drop
                && action.name == "tr"
        }));
    }

    #[test]
    fn clause_edits_use_full_clause_boundaries() {
        let sql = "WITH c AS (SELECT x FROM t) SELECT a FROM u LEFT JOIN v ON u.id = v.id WHERE a = 1 GROUP BY a, b ORDER BY b LIMIT 5 OFFSET 2;";
        let units = parse_units(sql, DialectKind::PostgreSql).unwrap();
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
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 2);
        assert!(sql[units[0].start..units[0].end].starts_with("CREATE TRIGGER"));
        assert!(sql[units[0].start..units[0].end].ends_with("END"));
        assert!(units[0].facts.is_some());
        assert!(units[1].facts.is_some());
    }

    #[test]
    fn invalid_utf8_is_rejected_at_the_ffi_boundary() {
        let input = [0xff];
        let mut output = std::ptr::null_mut();
        let mut output_len = 0usize;
        let invalid_output = unsafe {
            occam_sql_parse(
                input.as_ptr(),
                input.len(),
                3,
                &mut output,
                std::ptr::null_mut(),
            )
        };
        assert_eq!(invalid_output, 1);
        assert!(output.is_null());
        let status = unsafe {
            occam_sql_parse(input.as_ptr(), input.len(), 3, &mut output, &mut output_len)
        };
        assert_eq!(status, 3);
        assert!(output.is_null());
        assert_eq!(output_len, 0);
    }

    #[test]
    fn conflict_writes_nested_ctes_and_group_by_all_are_handled() {
        let sql = "WITH outer_cte AS (SELECT id FROM outer_src) SELECT id FROM (WITH outer_cte AS (SELECT id FROM inner_src) SELECT id FROM outer_cte) nested JOIN outer_cte ON nested.id = outer_cte.id;";
        let units = parse_units(sql, DialectKind::PostgreSql).unwrap();
        let uses: Vec<_> = units[0]
            .facts
            .as_ref()
            .unwrap()
            .actions
            .iter()
            .filter(|action| {
                action.kind == NativeObjectKind::Table && action.action == NativeActionType::Use
            })
            .map(|action| action.name.as_str())
            .collect();
        assert_eq!(uses, vec!["inner_src", "outer_src"]);

        let dynamic = parse_units(
            "SELECT * FROM generate_series(1, 2) AS n;",
            DialectKind::PostgreSql,
        )
        .unwrap();
        let actions = &dynamic[0].facts.as_ref().unwrap().actions;
        assert!(actions.iter().any(|action| {
            action.kind == NativeObjectKind::Barrier && action.action == NativeActionType::Barrier
        }));
        assert!(!actions.iter().any(|action| {
            action.kind == NativeObjectKind::Table
                && action.action == NativeActionType::Use
                && action.name == "generate_series"
        }));

        for (dialect, insert) in [
            (DialectKind::PostgreSql, "INSERT INTO t (id) VALUES (1) ON CONFLICT (id) DO UPDATE SET extra = excluded.extra;"),
            (DialectKind::MySql, "INSERT INTO t (id) VALUES (1) ON DUPLICATE KEY UPDATE extra = VALUES(extra);"),
        ] {
            let units = parse_units(insert, dialect).unwrap();
            assert!(units[0].facts.as_ref().unwrap().actions.iter().any(|action| {
                action.kind == NativeObjectKind::Column
                    && action.action == NativeActionType::Mod
                    && action.name == "t\u{1f}extra"
            }));
        }

        let group_sql = "SELECT a FROM t GROUP BY ALL;";
        let group = parse_units(group_sql, DialectKind::Generic).unwrap();
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
            if let Err(error) = parse_units(sql, dialect) {
                panic!("native fact generation failed for {sql:?}: {error}");
            }
        }
    }

    #[test]
    fn create_table_foreign_keys_emit_parent_uses() {
        let sql = "CREATE TABLE parent (id INT PRIMARY KEY); CREATE TABLE child (parent_id INT REFERENCES parent(id)); CREATE TABLE child2 (parent_id INT, FOREIGN KEY (parent_id) REFERENCES parent(id));";
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
        for unit in &units[1..] {
            assert!(unit.facts.as_ref().unwrap().actions.iter().any(|action| {
                action.kind == NativeObjectKind::Table
                    && action.action == NativeActionType::Use
                    && action.name == "parent"
            }));
        }
    }

    #[test]
    fn cascading_foreign_keys_and_on_commit_actions() {
        let sql = "CREATE TABLE child (p_id INT REFERENCES parent(id) ON DELETE CASCADE); CREATE TEMPORARY TABLE temp_tbl (x INT) ON COMMIT DROP;";
        let units = parse_units(sql, DialectKind::PostgreSql).unwrap();
        assert!(units[0]
            .facts
            .as_ref()
            .unwrap()
            .actions
            .iter()
            .any(|action| {
                action.kind == NativeObjectKind::Table
                    && action.action == NativeActionType::Cascade
                    && action.name == "parent"
            }));
        assert!(units[1]
            .facts
            .as_ref()
            .unwrap()
            .actions
            .iter()
            .any(|action| {
                action.kind == NativeObjectKind::TemporaryTable
                    && action.action == NativeActionType::OnCommitDrop
                    && action.name == "pg_temp.temp_tbl"
            }));
    }

    #[test]
    fn clause_and_scalar_edits_cover_parentheses() {
        let sql = "SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id WHERE (col_timestamp_2 IS NOT NULL AND col_timestamp_1 IS NOT NULL);";
        let units = parse_units(sql, DialectKind::Generic).unwrap();
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
        let units = parse_units(sql, DialectKind::Generic).unwrap();
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
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
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
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
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
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
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
        let units = parse_units(sql, DialectKind::PostgreSql).unwrap();
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
        let units = parse_units(sql, DialectKind::SQLite).unwrap();
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
        let units = parse_units(sql, DialectKind::PostgreSql).unwrap();
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

        let units = parse_units(sql, DialectKind::DuckDB).unwrap();
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

        let facts = units[2].facts.as_ref().unwrap();
        assert!(facts.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column
                && action.action == NativeActionType::Def
                && action.name == "t\u{1f}QuotedCol"
        }));
        assert!(facts.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column
                && action.action == NativeActionType::Def
                && action.name == "t\u{1f}unquoted_col"
        }));
    }

    #[test]
    fn clickhouse_dialect_parsing_and_statement_boundaries() {
        let sql = "SELECT number FROM numbers(10);\nCREATE TABLE events (EventId UInt64, event_name String) ENGINE = MergeTree ORDER BY EventId;";
        assert_eq!(count_units(sql, DialectKind::ClickHouse), Ok(2));
        let mut ffi_count = 0;
        let status = unsafe { occam_sql_count(sql.as_ptr(), sql.len(), 5, &mut ffi_count) };
        assert_eq!(status, 0);
        assert_eq!(ffi_count, 2);

        let units = parse_units(sql, DialectKind::ClickHouse).unwrap();
        assert_eq!(units.len(), 2);
        assert_eq!(
            &sql[units[0].start..units[0].end],
            "SELECT number FROM numbers(10)"
        );
        assert_eq!(
            &sql[units[1].start..units[1].end],
            "CREATE TABLE events (EventId UInt64, event_name String) ENGINE = MergeTree ORDER BY EventId"
        );
        let facts = units[1].facts.as_ref().unwrap();
        // ClickHouse identifiers are case-sensitive: EventId preserves case even unquoted.
        assert!(facts.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column
                && action.action == NativeActionType::Def
                && action.name == "events\u{1f}EventId"
        }));
        assert!(facts.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column
                && action.action == NativeActionType::Def
                && action.name == "events\u{1f}event_name"
        }));
    }

    #[test]
    fn oracle_dialect_parsing_and_statement_boundaries() {
        let sql = "SELECT 42 FROM dual;\nCREATE TABLE employees (id NUMBER(10) PRIMARY KEY, name VARCHAR2(100));\nSELECT a.id, b.name FROM a, b WHERE a.id = b.id(+);\nSELECT id, name FROM employees START WITH mgr_id IS NULL CONNECT BY PRIOR id = mgr_id;";
        assert_eq!(count_units(sql, DialectKind::Oracle), Ok(4));
        let mut ffi_count = 0;
        let status = unsafe { occam_sql_count(sql.as_ptr(), sql.len(), 6, &mut ffi_count) };
        assert_eq!(status, 0);
        assert_eq!(ffi_count, 4);

        let units = parse_units(sql, DialectKind::Oracle).unwrap();
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

        let facts0 = units[0].facts.as_ref().unwrap();
        assert!(facts0.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Table
                && action.action == NativeActionType::Use
                && action.name == "DUAL"
        }));

        let facts1 = units[1].facts.as_ref().unwrap();
        assert!(facts1.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Table
                && action.action == NativeActionType::Def
                && action.name == "EMPLOYEES"
        }));
        assert!(facts1.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column
                && action.action == NativeActionType::Def
                && action.name == "EMPLOYEES\u{1f}ID"
        }));
        assert!(facts1.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column
                && action.action == NativeActionType::Def
                && action.name == "EMPLOYEES\u{1f}NAME"
        }));

        let facts2 = units[2].facts.as_ref().unwrap();
        assert!(facts2.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Table
                && action.action == NativeActionType::Use
                && action.name == "A"
        }));
        assert!(facts2.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Table
                && action.action == NativeActionType::Use
                && action.name == "B"
        }));

        let facts3 = units[3].facts.as_ref().unwrap();
        assert!(facts3.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Table
                && action.action == NativeActionType::Use
                && action.name == "EMPLOYEES"
        }));

        let quoted_sql = "CREATE TABLE dept (\"DeptId\" NUMBER(10), DeptName VARCHAR2(50));";
        let quoted_units = parse_units(quoted_sql, DialectKind::Oracle).unwrap();
        let quoted_facts = quoted_units[0].facts.as_ref().unwrap();
        assert!(quoted_facts.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column
                && action.action == NativeActionType::Def
                && action.name == "DEPT\u{1f}DeptId"
        }));
        assert!(quoted_facts.actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column
                && action.action == NativeActionType::Def
                && action.name == "DEPT\u{1f}DEPTNAME"
        }));
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

            let units = parse_units(sql, kind).unwrap();
            assert_eq!(units.len(), expected_count);
            for unit in &units {
                assert!(unit.facts.is_some());
                assert!(unit.parse_error.is_none());
            }

            let first_facts = units[0].facts.as_ref().unwrap();
            assert!(
                first_facts.actions.iter().any(|action| {
                    (action.kind == NativeObjectKind::Table
                        || action.kind == NativeObjectKind::TemporaryTable)
                        && action.name == expected_table
                }),
                "Dialect {:?} should identify table {}",
                code,
                expected_table
            );
        }
    }

    #[test]
    fn identifier_case_folding_follows_dialect_semantics() {
        let oracle_units = parse_units(
            "CREATE TABLE Dept (DeptName INT, \"QuotedName\" INT);",
            DialectKind::Oracle,
        )
        .unwrap();
        let oracle_actions = &oracle_units[0].facts.as_ref().unwrap().actions;
        assert!(oracle_actions.iter().any(|action| {
            action.kind == NativeObjectKind::Table
                && action.action == NativeActionType::Def
                && action.name == "DEPT"
        }));
        assert!(oracle_actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column && action.name == "DEPT\u{1f}DEPTNAME"
        }));
        assert!(oracle_actions.iter().any(|action| {
            action.kind == NativeObjectKind::Column && action.name == "DEPT\u{1f}QuotedName"
        }));

        let postgres_units =
            parse_units("CREATE TABLE Dept (DeptName INT);", DialectKind::PostgreSql).unwrap();
        let postgres_actions = &postgres_units[0].facts.as_ref().unwrap().actions;
        assert!(postgres_actions.iter().any(|action| {
            action.kind == NativeObjectKind::Table
                && action.action == NativeActionType::Def
                && action.name == "dept"
        }));
    }

    #[test]
    fn mysql_drop_index_records_its_table_use() {
        let units = parse_units("DROP INDEX ix ON t;", DialectKind::MySql).unwrap();
        let actions = &units[0].facts.as_ref().unwrap().actions;
        assert!(actions.iter().any(|action| {
            action.kind == NativeObjectKind::Index
                && action.action == NativeActionType::Drop
                && action.name == "ix"
        }));
        assert!(actions.iter().any(|action| {
            action.kind == NativeObjectKind::Table
                && action.action == NativeActionType::Use
                && action.name == "t"
        }));
    }

    #[test]
    fn constraint_stripping_and_deeper_ast_pruning() {
        let sql = "CREATE TABLE t (id INT NOT NULL, name TEXT DEFAULT 'abc', PRIMARY KEY (id), FOREIGN KEY (id) REFERENCES p(id));";
        let units = parse_units(sql, DialectKind::Generic).unwrap();
        let edits = &units[0].facts.as_ref().unwrap().edits;
        let constraint_edits: Vec<_> = edits
            .iter()
            .filter(|e| e.category == NativeEditCategory::Constraint)
            .collect();
        assert!(constraint_edits
            .iter()
            .any(|e| e.label == "remove-column_constraint"));
        assert!(constraint_edits
            .iter()
            .any(|e| e.label == "remove-table_constraint"));

        let expr_sql = "SELECT CASE WHEN a = 1 THEN 42 ELSE 0 END, COALESCE(x, y), z IS NOT NULL, b BETWEEN 1 AND 10 FROM t;";
        let expr_units = parse_units(expr_sql, DialectKind::Generic).unwrap();
        let expr_edits = &expr_units[0].facts.as_ref().unwrap().edits;
        assert!(expr_edits.iter().any(|e| e.label == "simplify-case_result"));
        assert!(expr_edits
            .iter()
            .any(|e| e.label == "simplify-function_to_argument"));
        assert!(expr_edits.iter().any(|e| e.label == "simplify-is_null"));
        assert!(expr_edits.iter().any(|e| e.label == "simplify-between"));
    }

    #[test]
    fn named_table_constraints_are_removed_with_their_prefix() {
        let sql = "CREATE TABLE t (id INT, CONSTRAINT pk PRIMARY KEY (id), CONSTRAINT fk FOREIGN KEY (id) REFERENCES p(id), name TEXT);";
        let units = parse_units(sql, DialectKind::Generic).unwrap();
        let edits = &units[0].facts.as_ref().unwrap().edits;
        let removed: Vec<_> = edits
            .iter()
            .filter(|edit| edit.label == "remove-table_constraint")
            .map(|edit| &sql[edit.start..edit.end])
            .collect();
        assert_eq!(removed.len(), 2);
        assert!(removed
            .iter()
            .any(|text| text.contains("CONSTRAINT pk PRIMARY KEY (id)")));
        assert!(removed
            .iter()
            .any(|text| { text.contains("CONSTRAINT fk FOREIGN KEY (id) REFERENCES p(id)") }));
    }
}

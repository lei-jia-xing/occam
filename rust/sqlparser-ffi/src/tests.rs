use super::*;
use crate::parser::{count_units, parse_units};
use sqlparser::dialect::SQLiteDialect;
use sqlparser::parser::Parser;

fn parse(sql: &str, dialect: DialectKind) -> Result<Vec<NativeUnit>, String> {
    parse_units(
        sql,
        &ParseConfig {
            dialect,
            server: ServerConfig::default(),
            chunk_edits: false,
            column_ir: false,
            column_bundles: false,
            syntax_edits: true,
            syntax_only: false,
            span_only: false,
        },
    )
}

fn parse_chunks(sql: &str, dialect: DialectKind) -> Result<Vec<NativeUnit>, String> {
    parse_units(
        sql,
        &ParseConfig {
            dialect,
            server: ServerConfig::default(),
            chunk_edits: true,
            column_ir: false,
            column_bundles: false,
            syntax_edits: true,
            syntax_only: false,
            span_only: false,
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

fn has_routine_barrier(facts: &NativeFacts) -> bool {
    facts.barriers.iter().any(|barrier| {
        matches!(barrier.scope, BarrierScope::Global)
            && matches!(barrier.reason, BarrierReason::UnknownStatement)
    })
}

fn read_column_names(facts: &NativeFacts) -> Vec<String> {
    let mut names: Vec<String> = reads(facts)
        .into_iter()
        .flat_map(|read| read.columns.iter().map(|column| column.0.clone()))
        .collect();
    names.sort();
    names.dedup();
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

    let cte_sql = "WITH a AS (SELECT id FROM source), b AS (SELECT id FROM a) SELECT id FROM b;";
    let cte = parse(cte_sql, DialectKind::PostgreSql).unwrap();
    assert_eq!(relation_reads(facts(&cte[0])), vec!["source"]);
}

#[test]
fn list_chunks_delete_contiguous_values_rows_before_singles() {
    let sql = "INSERT INTO t(a,b) VALUES (1,2),(3,4),(5,6),(42,43);";
    let units = parse_chunks(sql, DialectKind::SQLite).unwrap();
    let edits = &facts(&units[0]).edits;
    let applied: Vec<String> = edits
        .iter()
        .filter(|edit| edit.label == "remove-values_row-chunk")
        .map(|edit| {
            assert!(edit.replacement.is_empty());
            assert!(sql.is_char_boundary(edit.start));
            assert!(sql.is_char_boundary(edit.end));
            format!(
                "{}{}{}",
                &sql[..edit.start],
                edit.replacement,
                &sql[edit.end..]
            )
        })
        .collect();
    assert_eq!(
        applied,
        vec![
            "INSERT INTO t(a,b) VALUES (5,6),(42,43);",
            "INSERT INTO t(a,b) VALUES (1,2),(3,4);",
        ]
    );
    for candidate in &applied {
        let reparsed = parse_chunks(candidate, DialectKind::SQLite).unwrap();
        assert!(reparsed[0].facts.is_some());
    }
    let first_single = edits
        .iter()
        .position(|edit| edit.label == "remove-values_row")
        .unwrap();
    let last_chunk = edits
        .iter()
        .rposition(|edit| edit.label == "remove-values_row-chunk")
        .unwrap();
    assert!(last_chunk < first_single);
    let single = edits
        .iter()
        .find(|edit| edit.label == "remove-values_row")
        .unwrap();
    let reduced = format!(
        "{}{}{}",
        &sql[..single.start],
        single.replacement,
        &sql[single.end..]
    );
    assert_eq!(reduced, "INSERT INTO t(a,b) VALUES (3,4),(5,6),(42,43);");
}

#[test]
fn list_chunks_carry_comments_and_preserve_unicode_crlf() {
    let sql = "-- caf\u{e9}\nSELECT * FROM t WHERE x IN (1, /* drop */ 2, 3);\r\n";
    let units = parse_chunks(sql, DialectKind::SQLite).unwrap();
    let edits = &facts(&units[0]).edits;
    let applied: Vec<String> = edits
        .iter()
        .filter(|edit| edit.label == "remove-in_list-chunk")
        .map(|edit| {
            assert!(sql.is_char_boundary(edit.start));
            assert!(sql.is_char_boundary(edit.end));
            format!(
                "{}{}{}",
                &sql[..edit.start],
                edit.replacement,
                &sql[edit.end..]
            )
        })
        .collect();
    assert_eq!(
        applied,
        vec!["-- caf\u{e9}\nSELECT * FROM t WHERE x IN (3);\r\n"]
    );
    for candidate in &applied {
        assert!(candidate.starts_with("-- caf\u{e9}\n"));
        assert!(candidate.ends_with(";\r\n"));
        let reparsed = parse_chunks(candidate, DialectKind::SQLite).unwrap();
        assert!(reparsed[0].facts.is_some());
    }
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
        chunk_edits: false,
        column_ir: false,
        column_bundles: false,
        syntax_edits: true,
        syntax_only: false,
        span_only: false,
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
    assert!(facts(&dynamic[0]).barriers.is_empty());
    assert!(!relation_reads(facts(&dynamic[0])).contains(&"generate_series".to_string()));

    let unmodeled = parse(
        "SELECT * FROM custom_func(1, 2) AS n;",
        DialectKind::PostgreSql,
    )
    .unwrap();
    assert!(!facts(&unmodeled[0]).barriers.is_empty());

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
fn upsert_conflict_reads_cover_arbiter_and_set_sources() {
    let sql = "INSERT INTO item_inventory (sku, item_name, quantity) VALUES ('SKU-001', 'Widget Alpha', 30) ON CONFLICT (sku) DO UPDATE SET quantity = item_inventory.quantity + EXCLUDED.quantity;";
    let units = parse(sql, DialectKind::SQLite).unwrap();
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    assert!(
        matches!(
            write[0].kind,
            WriteKind::Insert {
                conflict: ConflictAction::Update
            }
        ),
        "{:?}",
        write[0].kind
    );
    assert_eq!(write[0].reads.len(), 1);
    let read = &write[0].reads[0];
    assert_eq!(name_text(&read.source.name), "item_inventory");
    assert_eq!(read.mode, ReadMode::Data);
    let mut columns: Vec<String> = read.columns.iter().map(|column| column.0.clone()).collect();
    columns.sort();
    assert_eq!(columns, vec!["quantity".to_string(), "sku".to_string()]);
    // The proposed row is not stored data: every conflict read stays on
    // the target table.
    assert!(
        write[0]
            .reads
            .iter()
            .all(|read| name_text(&read.source.name) == "item_inventory"),
        "{:?}",
        write[0].reads
    );

    // DO NOTHING still probes the arbiter.
    let sql = "INSERT INTO t (a) VALUES (1) ON CONFLICT (a) DO NOTHING;";
    let units = parse(sql, DialectKind::SQLite).unwrap();
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    assert!(
        matches!(
            write[0].kind,
            WriteKind::Insert {
                conflict: ConflictAction::Ignore
            }
        ),
        "{:?}",
        write[0].kind
    );
    assert_eq!(write[0].reads.len(), 1);
    assert_eq!(name_text(&write[0].reads[0].source.name), "t");
    assert_eq!(write[0].reads[0].columns, vec![Ident("a".to_string())]);

    // DO UPDATE ... WHERE reads its filter columns too.
    let sql = "INSERT INTO t (a, b) VALUES (1, 2) ON CONFLICT (a) DO UPDATE SET b = 1 WHERE c > 0;";
    let units = parse(sql, DialectKind::SQLite).unwrap();
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    let mut columns: Vec<String> = write[0]
        .reads
        .iter()
        .flat_map(|read| read.columns.iter().map(|column| column.0.clone()))
        .collect();
    columns.sort();
    assert_eq!(columns, vec!["a".to_string(), "c".to_string()]);

    // No arbiter, no extra reads; plain INSERTs are untouched.
    for sql in [
        "INSERT INTO t (a) VALUES (1) ON CONFLICT DO NOTHING;",
        "INSERT INTO t (a) VALUES (1);",
        "INSERT OR IGNORE INTO t (a) VALUES (1);",
    ] {
        let units = parse(sql, DialectKind::SQLite).unwrap();
        let write = writes(facts(&units[0]));
        assert_eq!(write.len(), 1, "{sql}");
        assert!(write[0].reads.is_empty(), "{sql}: {:?}", write[0].reads);
    }
}
#[test]
fn mysql_duplicate_key_reads_cover_keys_and_set_sources() {
    // AST shape (sqlparser 0.64, probed): `OnInsert::DuplicateKeyUpdate`
    // holds the `SET` assignments; `VALUES(col)` lowers as a plain
    // `Expr::Function`; `AS new` row aliases lower as `insert_alias`
    // with `new.col` as `CompoundIdentifier`.
    let sql = "INSERT INTO t (id, val) VALUES (1, 2) ON DUPLICATE KEY UPDATE val = t.val + 1;";
    let units = parse(sql, DialectKind::MySql).unwrap();
    assert!(units[0].parse_error.is_none());
    assert!(
        facts(&units[0]).barriers.is_empty(),
        "{:?}",
        facts(&units[0]).barriers
    );
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    assert!(
        matches!(
            write[0].kind,
            WriteKind::Insert {
                conflict: ConflictAction::Update
            }
        ),
        "{:?}",
        write[0].kind
    );
    // Probe (explicit INSERT list) plus SET sources, all on the target.
    assert_eq!(write[0].reads.len(), 1);
    let read = &write[0].reads[0];
    assert_eq!(name_text(&read.source.name), "t");
    assert_eq!(read.mode, ReadMode::Data);
    let mut columns: Vec<String> = read.columns.iter().map(|column| column.0.clone()).collect();
    columns.sort();
    assert_eq!(columns, vec!["id".to_string(), "val".to_string()]);
    assert!(
        write[0].columns.contains(&Ident("val".to_string())),
        "{:?}",
        write[0].columns
    );

    // `VALUES(col)` names the proposed row: excluded from stored reads,
    // and the marker itself carries no routine fence. The probe still
    // covers the provided keys, so reads stay present.
    let sql = "INSERT INTO t (a, b) VALUES (1, 2) ON DUPLICATE KEY UPDATE a = VALUES(b);";
    let units = parse(sql, DialectKind::MySql).unwrap();
    assert!(units[0].parse_error.is_none(), "{sql}");
    assert!(
        facts(&units[0]).barriers.is_empty(),
        "{sql}: {:?}",
        facts(&units[0]).barriers
    );
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    let mut columns: Vec<String> = write[0]
        .reads
        .iter()
        .flat_map(|read| read.columns.iter().map(|column| column.0.clone()))
        .collect();
    columns.sort();
    // Probe keys only; `VALUES(b)` contributes no stored read.
    assert_eq!(columns, vec!["a".to_string(), "b".to_string()], "{sql}");

    // Mixed proposed plus stored: `VALUES(b)` excluded, bare `c` kept.
    let sql = "INSERT INTO t (a, b) VALUES (1, 2) ON DUPLICATE KEY UPDATE a = VALUES(b) + c;";
    let units = parse(sql, DialectKind::MySql).unwrap();
    let write = writes(facts(&units[0]));
    let mut columns: Vec<String> = write[0]
        .reads
        .iter()
        .flat_map(|read| read.columns.iter().map(|column| column.0.clone()))
        .collect();
    columns.sort();
    assert_eq!(
        columns,
        vec!["a".to_string(), "b".to_string(), "c".to_string()],
        "{sql}"
    );

    // Row-alias form (MySQL 8.0.19+): `new.col` is proposed, bare refs
    // are stored.
    let sql = "INSERT INTO t (a, b) VALUES (1, 2) AS new ON DUPLICATE KEY UPDATE a = new.b + c;";
    let units = parse(sql, DialectKind::MySql).unwrap();
    assert!(units[0].parse_error.is_none(), "{sql}");
    let write = writes(facts(&units[0]));
    let mut columns: Vec<String> = write[0]
        .reads
        .iter()
        .flat_map(|read| read.columns.iter().map(|column| column.0.clone()))
        .collect();
    columns.sort();
    assert_eq!(
        columns,
        vec!["a".to_string(), "b".to_string(), "c".to_string()],
        "{sql}"
    );

    // Quoted `VALUES` may name a user routine: it fences and its arg
    // stays a stored read (no proposed-row exclusion). MySQL quotes with
    // backticks (double quotes are string literals, unparseable here).
    let sql = "INSERT INTO t (a) VALUES (1) ON DUPLICATE KEY UPDATE a = `VALUES`(b);";
    let units = parse(sql, DialectKind::MySql).unwrap();
    assert!(units[0].parse_error.is_none(), "{sql}");
    assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    let write = writes(facts(&units[0]));
    let columns: Vec<String> = write[0]
        .reads
        .iter()
        .flat_map(|read| read.columns.iter().map(|column| column.0.clone()))
        .collect();
    assert!(columns.contains(&"b".to_string()), "{sql}: {columns:?}");

    // Shadowed `VALUES` (routine DDL anywhere) keeps fencing.
    let sql = "CREATE FUNCTION values(x INT) RETURNS INT RETURN x; INSERT INTO t (a) VALUES (1) ON DUPLICATE KEY UPDATE a = VALUES(b);";
    let units = parse(sql, DialectKind::MySql).unwrap();
    assert_eq!(units.len(), 2);
    assert!(has_routine_barrier(facts(&units[1])), "{sql}");

    // No explicit column list: probe vacuous (mirroring `ON CONFLICT`
    // without an arbiter); SET sources still modeled.
    let sql = "INSERT INTO t VALUES (1, 2) ON DUPLICATE KEY UPDATE a = b + 1;";
    let units = parse(sql, DialectKind::MySql).unwrap();
    assert!(units[0].parse_error.is_none(), "{sql}");
    let write = writes(facts(&units[0]));
    let mut columns: Vec<String> = write[0]
        .reads
        .iter()
        .flat_map(|read| read.columns.iter().map(|column| column.0.clone()))
        .collect();
    columns.sort();
    assert_eq!(columns, vec!["b".to_string()], "{sql}");

    // Plain INSERTs are untouched.
    let sql = "INSERT INTO t (a) VALUES (1);";
    let units = parse(sql, DialectKind::MySql).unwrap();
    let write = writes(facts(&units[0]));
    assert!(write[0].reads.is_empty(), "{sql}: {:?}", write[0].reads);
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
fn sqlite_foreign_keys_accepts_boolean_keywords_with_original_spans() {
    for (pragma, enabled) in [
        ("PRAGMA foreign_keys = ON", true),
        ("PRAGMA main.foreign_keys(Off)", false),
        ("PRAGMA \"foreign_keys\" /* 外鍵 */ = YeS", true),
        ("PRAGMA foreign_keys(NO)", false),
        ("PRAGMA foreign_keys = TRUE", true),
        ("PRAGMA foreign_keys(FALSE)", false),
        ("PRAGMA foreign_keys = 'yes'", true),
        ("PRAGMA foreign_keys('no')", false),
    ] {
        let sql = format!("SELECT 1; {pragma}; SELECT 2;");
        let units = parse(&sql, DialectKind::SQLite).unwrap();
        assert_eq!(units.len(), 3, "{pragma}");
        assert_eq!(&sql[units[1].start..units[1].end], pragma);
        assert!(facts(&units[1]).barriers.is_empty(), "{pragma}");
        assert!(
            matches!(
                facts(&units[1]).session.as_slice(),
                [SessionEffect::SetSqliteForeignKeys { value }] if *value == enabled
            ),
            "{pragma}"
        );
        assert_eq!(&sql[units[2].start..units[2].end], "SELECT 2");
    }
}

#[test]
fn pragma_foreign_keys_only_changes_state_on_assignment() {
    let units = parse("PRAGMA foreign_keys;", DialectKind::SQLite).unwrap();
    assert!(facts(&units[0]).session.is_empty());
    assert!(facts(&units[0]).barriers.is_empty());

    let units = parse("PRAGMA foreign_keys = 0;", DialectKind::SQLite).unwrap();
    assert!(matches!(
        facts(&units[0]).session.as_slice(),
        [SessionEffect::SetSqliteForeignKeys { value: false }]
    ));

    let units = parse("PRAGMA foreign_key_checks = 1;", DialectKind::SQLite).unwrap();
    assert!(facts(&units[0]).session.is_empty());
    assert!(facts(&units[0]).barriers.is_empty());

    let units = parse("PRAGMA foreign_keys = 'maybe';", DialectKind::SQLite).unwrap();
    assert!(facts(&units[0]).session.is_empty());
    assert!(matches!(
        facts(&units[0]).barriers.as_slice(),
        [ScopedBarrier {
            scope: BarrierScope::Global,
            reason: BarrierReason::UnknownSessionEffect,
            ..
        }]
    ));
}

#[test]
fn unmodeled_state_changes_preserve_global_uncertainty() {
    for (dialect, sql) in [
        (DialectKind::SQLite, "PRAGMA foreign_keys = 1;"),
        (DialectKind::PostgreSql, "SET search_path TO public;"),
    ] {
        let units = parse(sql, dialect).unwrap();
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
    }
    for (dialect, sql) in [
        (DialectKind::SQLite, "PRAGMA recursive_triggers = 0;"),
        (DialectKind::SQLite, "PRAGMA writable_schema = 1;"),
        (
            DialectKind::PostgreSql,
            "SET session_replication_role = replica;",
        ),
        (DialectKind::PostgreSql, "RESET session_replication_role;"),
        (DialectKind::PostgreSql, "RESET ALL;"),
        (DialectKind::PostgreSql, "SET search_path TO 42;"),
        (DialectKind::SQLite, "PRAGMA foreign_keys = ?;"),
        (
            DialectKind::PostgreSql,
            "SET TRANSACTION SNAPSHOT 'snapshot';",
        ),
        (
            DialectKind::PostgreSql,
            "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;",
        ),
        (
            DialectKind::PostgreSql,
            "BEGIN ISOLATION LEVEL SERIALIZABLE;",
        ),
    ] {
        let units = parse(sql, dialect).unwrap();
        assert!(
            facts(&units[0])
                .barriers
                .iter()
                .any(|barrier| { matches!(barrier.scope, BarrierScope::Global) }),
            "{sql}"
        );
    }
}

#[test]
fn session_init_resolves_the_dialect_foreign_key_default() {
    let config = |dialect, foreign_key_checks| ParseConfig {
        dialect,
        server: ServerConfig {
            foreign_key_checks,
            ..ServerConfig::default()
        },
        chunk_edits: false,
        column_ir: false,
        column_bundles: false,
        syntax_edits: true,
        syntax_only: false,
        span_only: false,
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
        !crate::ffi::session_init(&config(DialectKind::PostgreSql, Some(false))).foreign_key_checks,
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
        "SET foreign_key_checks = 0; RESET foreign_key_checks; SET @@foreign_key_checks = 1; SET @@SESSION.foreign_key_checks = 0;",
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
    assert!(matches!(
        facts(&units[2]).session.as_slice(),
        [SessionEffect::SetForeignKeyChecks { value: true, .. }]
    ));
    assert!(matches!(
        facts(&units[3]).session.as_slice(),
        [SessionEffect::SetForeignKeyChecks { value: false, .. }]
    ));
}

#[test]
fn copy_statement_records_read_and_write_facts() {
    let units = parse(
        "COPY t (a, b) FROM '/path/to/data.csv'; COPY t (a, b) TO '/path/to/out.csv'; COPY (SELECT a FROM t) TO STDOUT;",
        DialectKind::PostgreSql,
    )
    .unwrap();
    // COPY FROM t is a WriteData op
    let f0 = facts(&units[0]);
    assert_eq!(f0.barriers.len(), 1);
    assert!(matches!(
        f0.ops.as_slice(),
        [CatalogOp::WriteData(DataWrite { target, kind: WriteKind::Insert { .. }, .. })]
            if name_text(&target.name) == "t"
    ));
    // COPY t TO is a Read op
    let f1 = facts(&units[1]);
    assert_eq!(f1.barriers.len(), 1);
    assert!(matches!(
        f1.ops.as_slice(),
        [CatalogOp::Read(ReadFact { source, .. })]
            if name_text(&source.name) == "t"
    ));
    // COPY (SELECT ..) TO is a Read op from query visitor
    let f2 = facts(&units[2]);
    assert_eq!(f2.barriers.len(), 0);
    assert!(matches!(
        f2.ops.as_slice(),
        [CatalogOp::Read(ReadFact { source, .. })]
            if name_text(&source.name) == "t"
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
fn view_column_lineage_and_condition_columns() {
    let units = parse(
        "CREATE VIEW v AS SELECT t1.a AS col_a, t2.b AS col_b FROM t1 JOIN t2 ON t1.id = t2.id WHERE t1.active = 1;",
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
        vec![Ident("col_a".to_string()), Ident("col_b".to_string())]
    );
    assert_eq!(view.column_lineage.len(), 2);
    assert_eq!(view.column_lineage[0].column, Ident("col_a".to_string()));
    assert_eq!(
        view.column_lineage[0].sources,
        Some(vec![SourceColumn {
            source: ir::NameRef {
                name: ir::Name(vec![Ident("t1".to_string())]),
                temp: false,
            },
            column: Ident("a".to_string()),
        }])
    );
    assert_eq!(view.column_lineage[1].column, Ident("col_b".to_string()));
    assert_eq!(
        view.column_lineage[1].sources,
        Some(vec![SourceColumn {
            source: ir::NameRef {
                name: ir::Name(vec![Ident("t2".to_string())]),
                temp: false,
            },
            column: Ident("b".to_string()),
        }])
    );

    // Condition columns: t1.id, t2.id, t1.active
    assert_eq!(view.condition_columns.len(), 3);
    assert!(view.condition_columns.contains(&SourceColumn {
        source: ir::NameRef {
            name: ir::Name(vec![Ident("t1".to_string())]),
            temp: false,
        },
        column: Ident("id".to_string()),
    }));
    assert!(view.condition_columns.contains(&SourceColumn {
        source: ir::NameRef {
            name: ir::Name(vec![Ident("t2".to_string())]),
            temp: false,
        },
        column: Ident("id".to_string()),
    }));
    assert!(view.condition_columns.contains(&SourceColumn {
        source: ir::NameRef {
            name: ir::Name(vec![Ident("t1".to_string())]),
            temp: false,
        },
        column: Ident("active".to_string()),
    }));

    // Wildcard view falls back to empty lineage
    let wildcard_units = parse(
        "CREATE VIEW v_star AS SELECT * FROM t;",
        DialectKind::PostgreSql,
    )
    .unwrap();
    let star_view = facts(&wildcard_units[0])
        .ops
        .iter()
        .find_map(|op| match op {
            CatalogOp::DefineView(view) => Some(view),
            _ => None,
        })
        .expect("star view definition");
    assert!(star_view.column_lineage.is_empty());
    assert!(star_view.condition_columns.is_empty());
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
        "SELECT pg_catalog.nextval('tenant.\"MixedSeq\"');",
        DialectKind::PostgreSql,
    )
    .unwrap();
    assert_eq!(
        sequences(facts(&units[0])),
        vec!["tenant.MixedSeq".to_string()]
    );

    for sql in [
        "SELECT pg_catalog.nextval('a\"b\"c');",
        "SELECT pg_catalog.nextval('foo bar.seq');",
        "SELECT pg_catalog.nextval('\"\"');",
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
    for sql in [
        "SELECT pg_catalog.nextval('s');",
        "SELECT pg_catalog.nextval('s'::regclass);",
        "SELECT pg_catalog.nextval(CAST(('s') AS TEXT));",
        "SELECT pg_catalog.setval('s', 42);",
    ] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert_eq!(sequences(facts(&units[0])), vec!["s".to_string()], "{sql}");
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
    }

    let units = parse("SELECT s.NEXTVAL FROM DUAL;", DialectKind::Oracle).unwrap();
    assert_eq!(sequences(facts(&units[0])), vec!["S".to_string()]);
}

#[test]
fn sequence_state_reads_and_unknown_calls_are_preserved() {
    for (dialect, sql, name) in [
        (
            DialectKind::PostgreSql,
            "SELECT pg_catalog.currval('s'::regclass);",
            "s",
        ),
        (DialectKind::Oracle, "SELECT s.CURRVAL FROM DUAL;", "S"),
        (DialectKind::Snowflake, "SELECT s.CURRVAL;", "S"),
    ] {
        let units = parse(sql, dialect).unwrap();
        assert!(sequences(facts(&units[0])).is_empty(), "{sql}");
        assert!(
            relation_reads(facts(&units[0])).contains(&name.to_string()),
            "{sql}"
        );
    }
    for sql in [
        "SELECT pg_catalog.nextval(seq_name) FROM seqs;",
        "SELECT pg_catalog.nextval(seqs.seq_name) FROM seqs;",
        "SELECT pg_catalog.nextval(seq_name::regclass) FROM seqs;",
        "SELECT pg_catalog.setval(seq_name::regclass, 42) FROM seqs;",
        "SELECT pg_catalog.currval(seq_name) FROM seqs;",
        "SELECT pg_catalog.lastval();",
        "SELECT pg_catalog.nextval('long_seq'::varchar(1));",
        "SELECT pg_catalog.nextval('s'::custom_type);",
        "SELECT nextval('s');",
        "SELECT currval('s');",
        "SELECT setval('s', 42);",
        "SELECT tenant.nextval('s');",
        "SELECT \"NEXTVAL\"('s');",
        "SELECT mutate_t();",
    ] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        let facts = facts(&units[0]);
        assert!(sequences(facts).is_empty(), "{sql}");
        assert!(
            facts
                .barriers
                .iter()
                .any(|barrier| { matches!(barrier.scope, BarrierScope::Global) }),
            "{sql}"
        );
    }
    for dialect in [DialectKind::Oracle, DialectKind::Snowflake] {
        for sql in [
            "SELECT s.\"NEXTVAL\" FROM s;",
            "SELECT s.\"currval\" FROM s;",
        ] {
            let units = parse(sql, dialect).unwrap();
            let facts = facts(&units[0]);
            assert!(sequences(facts).is_empty(), "{sql}");
            assert!(
                facts
                    .barriers
                    .iter()
                    .any(|barrier| { matches!(barrier.scope, BarrierScope::Global) }),
                "{sql}"
            );
        }
    }
}

#[test]
fn deferred_functions_are_opaque_but_plain_explain_does_not_execute_them() {
    for sql in [
        "CREATE TABLE t (id BIGINT DEFAULT pg_catalog.nextval('s'));",
        "CREATE VIEW v AS SELECT pg_catalog.nextval('s');",
        "CREATE VIEW v AS SELECT mutate_t();",
        "ALTER TABLE t ALTER COLUMN id SET DEFAULT pg_catalog.nextval('s');",
    ] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        let facts = facts(&units[0]);
        assert!(sequences(facts).is_empty(), "{sql}");
        assert!(
            facts
                .barriers
                .iter()
                .any(|barrier| { matches!(barrier.scope, BarrierScope::Global) }),
            "{sql}"
        );
    }
    let units = parse("EXPLAIN SELECT mutate_t();", DialectKind::PostgreSql).unwrap();
    assert!(facts(&units[0]).barriers.is_empty());
    let units = parse(
        "EXPLAIN ANALYZE SELECT mutate_t();",
        DialectKind::PostgreSql,
    )
    .unwrap();
    assert!(
        facts(&units[0])
            .barriers
            .iter()
            .any(|barrier| { matches!(barrier.scope, BarrierScope::Global) })
    );
}

#[test]
fn sequence_state_advances_only_for_evaluated_ddl() {
    for sql in [
        "CREATE MATERIALIZED VIEW mv AS SELECT pg_catalog.nextval('s');",
        "CREATE TABLE t AS SELECT pg_catalog.nextval('s');",
        "ALTER TABLE t ADD COLUMN c INT DEFAULT pg_catalog.nextval('s');",
    ] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert_eq!(
            sequences(facts(&units[0])),
            vec!["s".to_string()],
            "expected a sequence write for {sql}"
        );
    }

    for sql in [
        "CREATE VIEW v AS SELECT pg_catalog.nextval('s');",
        "CREATE TABLE t AS SELECT pg_catalog.nextval('s') WITH NO DATA;",
        "ALTER TABLE t ALTER COLUMN c SET DEFAULT pg_catalog.nextval('s');",
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

#[test]
fn postgres_analyze_table_extracts_relation_and_columns() {
    let sql = "ANALYZE t0 (c0, c2);";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    assert_eq!(units.len(), 1);
    assert!(units[0].parse_error.is_none());
    let facts = units[0].facts.as_ref().expect("expected facts for ANALYZE");
    assert_eq!(facts.barriers.len(), 1);
    assert_eq!(
        facts.barriers[0].scope,
        ir::BarrierScope::Relation {
            relation: ir::NameRef {
                name: ir::Name(vec![ir::Ident("t0".to_string())]),
                temp: false,
            }
        }
    );
    if let ir::CatalogOp::Read(read) = &facts.ops[0] {
        assert_eq!(name_text(&read.source.name), "t0");
        let cols: Vec<String> = read.columns.iter().map(|c| c.0.clone()).collect();
        assert_eq!(cols, vec!["c0".to_string(), "c2".to_string()]);
    } else {
        panic!("expected CatalogOp::Read");
    }

    // Bare ANALYZE; has no parse error and stays globally fenced
    let sql_bare = "ANALYZE;";
    let units_bare = parse(sql_bare, DialectKind::PostgreSql).unwrap();
    assert_eq!(units_bare.len(), 1);
    assert!(units_bare[0].parse_error.is_none());
    let bare_facts = units_bare[0]
        .facts
        .as_ref()
        .expect("expected facts for bare ANALYZE");
    assert!(bare_facts.ops.is_empty());
    assert!(bare_facts.barriers.iter().any(|barrier| {
        matches!(
            barrier,
            ScopedBarrier {
                scope: BarrierScope::Global,
                reason: BarrierReason::UnknownDdl,
                ..
            }
        )
    }));
}

#[test]
fn syntax_edit_gate_drops_only_edits() {
    let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2), (3, 4);\nSELECT a FROM t ORDER BY a;";
    let full = parse(sql, DialectKind::SQLite).expect("parse");
    let gated = parse_units(
        sql,
        &ParseConfig {
            dialect: DialectKind::SQLite,
            server: ServerConfig::default(),
            chunk_edits: false,
            column_ir: false,
            column_bundles: false,
            syntax_edits: false,
            syntax_only: false,
            span_only: false,
        },
    )
    .expect("parse");
    assert_eq!(full.len(), gated.len());
    let edit_count = |units: &[NativeUnit]| {
        units
            .iter()
            .filter_map(|unit| unit.facts.as_ref())
            .map(|facts| facts.edits.len())
            .sum::<usize>()
    };
    assert!(edit_count(&full) > 0, "fixture must carry edits");
    assert_eq!(edit_count(&gated), 0, "gate must drop all edits");
    for (a, b) in full.iter().zip(gated.iter()) {
        assert_eq!((a.start, a.end), (b.start, b.end));
        let fa = facts(a);
        let fb = facts(b);
        // Dependency/semantic facts are identical; only edits differ.
        let shape = |f: &NativeFacts| {
            format!(
                "{:?}",
                (&f.ops, &f.session, &f.pre_tx, &f.post_tx, &f.barriers)
            )
        };
        assert_eq!(shape(fa), shape(fb));
    }
}

#[test]
fn span_only_skips_fact_extraction_and_preserves_spans() {
    let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2), (3, 4);\nSELECT a FROM t ORDER BY a;";
    let full = parse(sql, DialectKind::SQLite).expect("parse");
    let fast = parse_units(
        sql,
        &ParseConfig {
            dialect: DialectKind::SQLite,
            server: ServerConfig::default(),
            chunk_edits: false,
            column_ir: false,
            column_bundles: false,
            syntax_edits: false,
            syntax_only: false,
            span_only: true,
        },
    )
    .expect("span_only parse");
    assert_eq!(full.len(), fast.len());
    for (a, b) in full.iter().zip(fast.iter()) {
        assert_eq!((a.start, a.end), (b.start, b.end));
        assert_eq!(a.cannot_produce_result, b.cannot_produce_result);
        assert_eq!(a.parse_error, b.parse_error);
        assert!(b.facts.is_none(), "span_only must emit None for facts");
    }
}

#[test]
fn former_per_dialect_builtins_stay_fenced() {
    // Cutback: the SQLite/PostgreSQL/MySQL scalar allowlist is gone, so
    // these calls fence on every engine-specific dialect. Reads still
    // flow through arguments independently of fencing.
    let calls = [
        ("abs(a)", vec!["a"]),
        ("coalesce(a, b)", vec!["a", "b"]),
        ("concat(a, b)", vec!["a", "b"]),
        ("length(a)", vec!["a"]),
        ("lower(a)", vec!["a"]),
        ("nullif(a, b)", vec!["a", "b"]),
        ("round(a)", vec!["a"]),
        ("upper(a)", vec!["a"]),
    ];
    for dialect in [
        DialectKind::SQLite,
        DialectKind::PostgreSql,
        DialectKind::MySql,
    ] {
        for (call, columns) in &calls {
            let sql = format!("SELECT {call} FROM t;");
            let units = parse(&sql, dialect).unwrap();
            let unit_facts = facts(&units[0]);
            assert!(
                has_routine_barrier(unit_facts),
                "{sql} on {dialect:?}: expected a routine fence"
            );
            assert_eq!(relation_reads(unit_facts), vec!["t".to_string()], "{sql}");
            assert_eq!(
                read_column_names(unit_facts),
                columns.iter().map(ToString::to_string).collect::<Vec<_>>(),
                "{sql}"
            );
        }
    }
}
#[test]
fn former_per_dialect_aggregates_stay_fenced() {
    // Cutback: the SQLite/PostgreSQL/MySQL aggregate allowlist is gone,
    // so these calls fence on every engine-specific dialect. Reads still
    // flow through arguments independently of fencing.
    let calls = [
        ("avg(a)", vec!["a"]),
        ("count(a)", vec!["a"]),
        ("count(DISTINCT a)", vec!["a"]),
        ("max(a)", vec!["a"]),
        ("min(a)", vec!["a"]),
        ("sum(a)", vec!["a"]),
        ("sum(DISTINCT a)", vec!["a"]),
        ("AVG(a)", vec!["a"]),
        ("MAX(a)", vec!["a"]),
        ("MIN(a)", vec!["a"]),
        ("SUM(a)", vec!["a"]),
    ];
    for dialect in [
        DialectKind::SQLite,
        DialectKind::PostgreSql,
        DialectKind::MySql,
    ] {
        for (call, columns) in &calls {
            let sql = format!("SELECT {call} FROM t;");
            let units = parse(&sql, dialect).unwrap();
            let unit_facts = facts(&units[0]);
            assert!(
                has_routine_barrier(unit_facts),
                "{sql} on {dialect:?}: expected a routine fence"
            );
            assert_eq!(relation_reads(unit_facts), vec!["t".to_string()], "{sql}");
            assert_eq!(
                read_column_names(unit_facts),
                columns.iter().map(ToString::to_string).collect::<Vec<_>>(),
                "{sql}"
            );
        }
        // Unconditional COUNT lifts the routine fence across dialects.
        for sql in ["SELECT count(*) FROM t;", "SELECT COUNT(*) FROM t;"] {
            let units = parse(sql, dialect).unwrap();
            let unit_facts = facts(&units[0]);
            assert!(
                !has_routine_barrier(unit_facts),
                "{sql} on {dialect:?}: unconditional count should lift routine fence"
            );
            assert_eq!(relation_reads(unit_facts), vec!["t".to_string()], "{sql}");
            assert_eq!(read_column_names(unit_facts), Vec::<String>::new(), "{sql}");
        }
        // Grouped projections fence with the grouping columns intact.
        let sql = "SELECT a, sum(b) FROM t GROUP BY a;";
        let units = parse(sql, dialect).unwrap();
        let unit_facts = facts(&units[0]);
        assert!(
            has_routine_barrier(unit_facts),
            "{sql} on {dialect:?}: expected a routine fence"
        );
        assert_eq!(
            read_column_names(unit_facts),
            vec!["a".to_string(), "b".to_string()],
            "{sql}"
        );
    }
    // Quoted spellings fence like every other non-Generic call.
    let units = parse("SELECT \"sum\"(a) FROM t;", DialectKind::PostgreSql).unwrap();
    assert!(has_routine_barrier(facts(&units[0])));
    let units = parse("SELECT \"Sum\"(a) FROM t;", DialectKind::PostgreSql).unwrap();
    assert!(has_routine_barrier(facts(&units[0])));
}

#[test]
fn function_argument_reads_are_extracted_for_fenced_calls() {
    // Reads flow through function arguments independently of fencing: the
    // fence never drops a read edge.
    let units = parse("SELECT a, frobnicate(b) FROM t;", DialectKind::SQLite).unwrap();
    let unit_facts = facts(&units[0]);
    assert!(has_routine_barrier(unit_facts));
    assert_eq!(relation_reads(unit_facts), vec!["t".to_string()]);
    assert_eq!(
        read_column_names(unit_facts),
        vec!["a".to_string(), "b".to_string()]
    );
}

#[test]
fn unknown_and_shaped_calls_still_fence() {
    for dialect in [
        DialectKind::SQLite,
        DialectKind::PostgreSql,
        DialectKind::MySql,
    ] {
        for sql in [
            "SELECT frobnicate(a) FROM t;",
            // Window shapes fence even under formerly-listed names.
            "SELECT abs(a) OVER (PARTITION BY b) FROM t;",
            "SELECT sum(a) OVER (PARTITION BY b) FROM t;",
            "SELECT sum(DISTINCT a) OVER (PARTITION BY b) FROM t;",
        ] {
            let units = parse(sql, dialect).unwrap();
            assert!(
                has_routine_barrier(facts(&units[0])),
                "{sql} on {dialect:?}"
            );
        }
    }
    // Qualification fences even under a builtin name.
    for sql in [
        "SELECT pg_catalog.abs(a) FROM t;",
        "SELECT pg_catalog.sum(a) FROM t;",
    ] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
}

#[test]
fn unproven_aggregate_and_window_shapes_still_fence() {
    for dialect in [
        DialectKind::SQLite,
        DialectKind::PostgreSql,
        DialectKind::MySql,
    ] {
        for sql in [
            // Statistical siblings are not uniform builtins.
            "SELECT stddev(a) FROM t;",
            "SELECT stddev_pop(a) FROM t;",
            "SELECT variance(a) FROM t;",
            "SELECT var_samp(a) FROM t;",
            // String-building aggregates are outside the proven set.
            "SELECT group_concat(a) FROM t;",
            "SELECT string_agg(a, ',') FROM t;",
            // Window-only names fence.
            "SELECT row_number() OVER (ORDER BY a) FROM t;",
            "SELECT rank() OVER (ORDER BY a) FROM t;",
            "SELECT lag(a) OVER (ORDER BY b) FROM t;",
            // Ordered-set shapes fence, even under a formerly-listed aggregate name.
            "SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a) FROM t;",
            "SELECT sum(a) WITHIN GROUP (ORDER BY b) FROM t;",
        ] {
            let units = parse(sql, dialect).unwrap();
            assert!(units[0].parse_error.is_none(), "{sql} on {dialect:?}");
            assert!(
                has_routine_barrier(facts(&units[0])),
                "{sql} on {dialect:?}"
            );
        }
    }
    // FILTER fences on every name and dialect (tested below); scalar
    // FILTER and Generic FILTER fence with the descent proof and the
    // Generic subset. MySQL rejects FILTER entirely (parse error), so
    // no MySQL FILTER case exists.
}
#[test]
fn filter_call_shapes_stay_fenced_with_condition_reads() {
    // Cutback: FILTER fences on every name and dialect (the aggregate
    // FILTER lift is gone with the per-dialect tables). Condition
    // columns are still extracted independently of fencing. MySQL has
    // no FILTER (parse error), so only PostgreSQL and SQLite are
    // observable here.
    let calls = [
        ("avg(a) FILTER (WHERE b > 0)", vec!["a", "b"]),
        ("count(*) FILTER (WHERE b > 0)", vec!["b"]),
        ("count(a) FILTER (WHERE b > 0)", vec!["a", "b"]),
        ("count(DISTINCT a) FILTER (WHERE b > 0)", vec!["a", "b"]),
        ("max(a) FILTER (WHERE b > 0)", vec!["a", "b"]),
        ("min(a) FILTER (WHERE b > 0)", vec!["a", "b"]),
        ("sum(a) FILTER (WHERE b > 0)", vec!["a", "b"]),
        ("sum(DISTINCT a) FILTER (WHERE b > 0)", vec!["a", "b"]),
        ("SUM(a) FILTER (WHERE b > 0)", vec!["a", "b"]),
    ];
    for dialect in [DialectKind::PostgreSql, DialectKind::SQLite] {
        for (call, columns) in &calls {
            let sql = format!("SELECT {call} FROM t;");
            let units = parse(&sql, dialect).unwrap();
            assert!(units[0].parse_error.is_none(), "{sql} on {dialect:?}");
            assert!(
                has_routine_barrier(facts(&units[0])),
                "{sql} on {dialect:?}: expected a routine fence"
            );
            assert_eq!(
                relation_reads(facts(&units[0])),
                vec!["t".to_string()],
                "{sql}"
            );
            assert_eq!(
                read_column_names(facts(&units[0])),
                columns.iter().map(ToString::to_string).collect::<Vec<_>>(),
                "{sql}"
            );
        }
    }
    // Scalar FILTER stays fenced: the clause is aggregate-only, so a
    // scalar FILTER is invalid SQL and never lifts — even though the
    // parser (syntax-only) accepts it.
    for (dialect, sql) in [
        (
            DialectKind::PostgreSql,
            "SELECT abs(a) FILTER (WHERE b > 0) FROM t;",
        ),
        (
            DialectKind::SQLite,
            "SELECT abs(a) FILTER (WHERE b > 0) FROM t;",
        ),
        (
            DialectKind::PostgreSql,
            "SELECT coalesce(a, b) FILTER (WHERE c > 0) FROM t;",
        ),
    ] {
        let units = parse(sql, dialect).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
    // Generic never lifts FILTER (the keyword rule covers plain calls only).
    let units = parse(
        "SELECT coalesce(a, b) FILTER (WHERE c > 0) FROM t;",
        DialectKind::Generic,
    )
    .unwrap();
    assert!(units[0].parse_error.is_none());
    assert!(has_routine_barrier(facts(&units[0])));
    // FILTER with OVER still fences (window gate dominates).
    let units = parse(
        "SELECT sum(a) FILTER (WHERE b > 0) OVER (PARTITION BY c) FROM t;",
        DialectKind::PostgreSql,
    )
    .unwrap();
    assert!(units[0].parse_error.is_none());
    assert!(has_routine_barrier(facts(&units[0])));
}
#[test]
fn filter_and_within_group_condition_columns_are_extracted_while_fenced() {
    // Derived-visit descent proof: sqlparser's `Visit` derive descends
    // into `Function.filter` and `Function.within_group` (upstream
    // `visits_function_clauses_in_source_order` test), so occam's
    // expression analysis extracts condition columns independently of
    // fencing. Every shape here fences (cutback: no FILTER lift
    // remains); this pins the descent on all of them, so
    // read-completeness never regresses silently.
    for (dialect, sql, expected) in [
        (
            DialectKind::PostgreSql,
            "SELECT abs(a) FILTER (WHERE b > 0) FROM t;",
            vec!["a".to_string(), "b".to_string()],
        ),
        (
            DialectKind::SQLite,
            "SELECT abs(a) FILTER (WHERE b > 0) FROM t;",
            vec!["a".to_string(), "b".to_string()],
        ),
        (
            DialectKind::PostgreSql,
            "SELECT sum(a) WITHIN GROUP (ORDER BY b) FROM t;",
            vec!["a".to_string(), "b".to_string()],
        ),
        (
            DialectKind::SQLite,
            "SELECT count(*) FILTER (WHERE b > 0) FROM t;",
            vec!["b".to_string()],
        ),
    ] {
        let units = parse(sql, dialect).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert_eq!(
            relation_reads(facts(&units[0])),
            vec!["t".to_string()],
            "{sql}"
        );
        assert_eq!(read_column_names(facts(&units[0])), expected, "{sql}");
    }
    // Every shape keeps its barrier.
    for (dialect, sql) in [
        (
            DialectKind::PostgreSql,
            "SELECT abs(a) FILTER (WHERE b > 0) FROM t;",
        ),
        (
            DialectKind::PostgreSql,
            "SELECT sum(a) WITHIN GROUP (ORDER BY b) FROM t;",
        ),
        (
            DialectKind::SQLite,
            "SELECT count(*) FILTER (WHERE b > 0) FROM t;",
        ),
    ] {
        let units = parse(sql, dialect).unwrap();
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
}

#[test]
fn generic_keyword_subset_lifts_but_other_names_still_fence() {
    for (call, columns) in [
        ("coalesce(a, b)", vec!["a", "b"]),
        ("COALESCE(a, b)", vec!["a", "b"]),
        ("Coalesce(a, b)", vec!["a", "b"]),
        ("nullif(a, b)", vec!["a", "b"]),
        ("NULLIF(a, b)", vec!["a", "b"]),
    ] {
        let sql = format!("SELECT {call} FROM t;");
        let units = parse(&sql, DialectKind::Generic).unwrap();
        let unit_facts = facts(&units[0]);
        assert!(
            unit_facts.barriers.is_empty(),
            "{sql}: {:?}",
            unit_facts.barriers
        );
        assert_eq!(relation_reads(unit_facts), vec!["t".to_string()], "{sql}");
        assert_eq!(
            read_column_names(unit_facts),
            columns.iter().map(ToString::to_string).collect::<Vec<_>>(),
            "{sql}"
        );
    }
    // Ordinary names — scalars and aggregates alike — still fence on
    // Generic: on an unknown engine they may resolve to user routines.
    for sql in [
        "SELECT abs(a) FROM t;",
        "SELECT concat(a, b) FROM t;",
        "SELECT sum(a) FROM t;",
        "SELECT count(a) FROM t;",
        "SELECT frobnicate(a) FROM t;",
        // Quoted spellings are identifiers, not keywords.
        "SELECT \"coalesce\"(a, b) FROM t;",
        "SELECT \"NULLIF\"(a, b) FROM t;",
    ] {
        let units = parse(sql, DialectKind::Generic).unwrap();
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
    // Unconditional count lifts the routine fence even on Generic.
    let count_star_units = parse("SELECT count(*) FROM t;", DialectKind::Generic).unwrap();
    assert!(!has_routine_barrier(facts(&count_star_units[0])));
    // Every non-Generic dialect keeps the blanket fence bit-identically —
    // cutback: SQLite, PostgreSQL, and MySQL join the always-fence set.
    for (dialect, sql) in [
        (DialectKind::Snowflake, "SELECT abs(a) FROM t;"),
        (DialectKind::Snowflake, "SELECT coalesce(a, b) FROM t;"),
        (DialectKind::DuckDB, "SELECT sum(a) FROM t;"),
        (DialectKind::DuckDB, "SELECT nullif(a, b) FROM t;"),
        (DialectKind::SQLite, "SELECT coalesce(a, b) FROM t;"),
        (DialectKind::SQLite, "SELECT nullif(a, b) FROM t;"),
        (DialectKind::SQLite, "SELECT abs(a) FROM t;"),
        (DialectKind::PostgreSql, "SELECT coalesce(a, b) FROM t;"),
        (DialectKind::PostgreSql, "SELECT nullif(a, b) FROM t;"),
        (DialectKind::PostgreSql, "SELECT sum(a) FROM t;"),
        (DialectKind::MySql, "SELECT coalesce(a, b) FROM t;"),
        (DialectKind::MySql, "SELECT nullif(a, b) FROM t;"),
        (DialectKind::MySql, "SELECT abs(a) FROM t;"),
    ] {
        let units = parse(sql, dialect).unwrap();
        assert!(
            has_routine_barrier(facts(&units[0])),
            "{sql} on {dialect:?}"
        );
    }
}

#[test]
fn volatile_and_sequence_calls_still_fence() {
    for (dialect, calls) in [
        (DialectKind::SQLite, vec!["random()"]),
        (
            DialectKind::PostgreSql,
            vec!["random()", "now()", "version()"],
        ),
        (DialectKind::MySql, vec!["rand()", "now()", "version()"]),
    ] {
        for call in calls {
            let sql = format!("SELECT {call} FROM t;");
            let units = parse(&sql, dialect).unwrap();
            assert!(has_routine_barrier(facts(&units[0])), "{sql}");
        }
    }
    // Unqualified sequence functions fence; only pg_catalog-qualified
    // spellings with valid arity are modeled.
    let units = parse("SELECT nextval('s');", DialectKind::PostgreSql).unwrap();
    assert!(has_routine_barrier(facts(&units[0])));
}

#[test]
fn former_per_dialect_case_folded_names_stay_fenced() {
    // Cutback: no engine-specific dialect lifts any spelling, folded or
    // not. (Generic keyword case-folding — COALESCE/Coalesce/NULLIF — is
    // pinned by the Generic subset test above.)
    for sql in [
        "SELECT ABS(a) FROM t;",
        "SELECT \"abs\"(a) FROM t;",
        "SELECT \"Abs\"(a) FROM t;",
        "SELECT \"ABS\"(a) FROM t;",
    ] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
    for sql in ["SELECT \"ABS\"(a) FROM t;", "SELECT \"Abs\"(a) FROM t;"] {
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
    let units = parse("SELECT `ABS`(a) FROM t;", DialectKind::MySql).unwrap();
    assert!(has_routine_barrier(facts(&units[0])));
}

#[test]
fn shadowed_pure_builtins_still_fence() {
    let create =
        "CREATE FUNCTION abs(x INT) RETURNS INT AS $$ BEGIN RETURN x; END; $$ LANGUAGE plpgsql;";
    let select = "SELECT abs(a) FROM t;";
    // Shadowing disables the rule regardless of definition position.
    for script in [format!("{create} {select}"), format!("{select} {create}")] {
        let units = parse(&script, DialectKind::PostgreSql).unwrap();
        assert_eq!(units.len(), 2);
        for unit in &units {
            assert!(has_routine_barrier(facts(unit)), "{script}");
        }
    }
    let units = parse(
        "DROP FUNCTION abs; SELECT abs(a) FROM t;",
        DialectKind::PostgreSql,
    )
    .unwrap();
    assert_eq!(units.len(), 2);
    assert!(has_routine_barrier(facts(&units[1])));
    // The coarse disable covers the Generic keyword rule too (the
    // PostgreSQL `sum` case below fences unconditionally since the
    // cutback; the Generic `coalesce` case is what pins the disable).
    let units = parse(
        "DROP FUNCTION sum; SELECT sum(a) FROM t;",
        DialectKind::PostgreSql,
    )
    .unwrap();
    assert_eq!(units.len(), 2);
    assert!(has_routine_barrier(facts(&units[1])));
    let units = parse(
        "DROP FUNCTION coalesce; SELECT coalesce(a, b) FROM t;",
        DialectKind::Generic,
    )
    .unwrap();
    assert_eq!(units.len(), 2);
    assert!(has_routine_barrier(facts(&units[1])));
    // Macros are callable routines too: a shadowing macro disables the
    // Generic keyword table (CREATE MACRO parses on Generic/DuckDB only,
    // so Generic is where the disable is observable).
    let units = parse(
        "CREATE MACRO coalesce(a, b) AS a; SELECT coalesce(a, b) FROM t;",
        DialectKind::Generic,
    )
    .unwrap();
    assert_eq!(units.len(), 2);
    assert!(units[0].parse_error.is_none());
    assert!(facts(&units[0]).barriers.is_empty());
    assert!(has_routine_barrier(facts(&units[1])));
    // A parse error anywhere also disables the rule: unparsed routine
    // DDL cannot be ruled out. (This SQLite `abs` case fences
    // unconditionally since the cutback; the Generic cases above pin
    // the disable itself.)
    let units = parse("SELECT abs(a) FROM t; SELECT FROM;", DialectKind::SQLite).unwrap();
    assert_eq!(units.len(), 2);
    assert!(has_routine_barrier(facts(&units[0])));
    assert!(units[1].facts.is_none());
}

#[test]
fn stored_generic_keywords_lift_but_other_stored_calls_still_fence() {
    // The Generic keyword rule lifts in stored positions too.
    for sql in [
        "CREATE TABLE t (x INT DEFAULT coalesce(1, 2));",
        "CREATE VIEW v AS SELECT coalesce(a, b) FROM t;",
    ] {
        let units = parse(sql, DialectKind::Generic).unwrap();
        assert!(
            facts(&units[0]).barriers.is_empty(),
            "{sql}: {:?}",
            facts(&units[0]).barriers
        );
    }
    // Cutback: the per-dialect stored lifts are gone, so these fence.
    for (dialect, sql) in [
        (
            DialectKind::SQLite,
            "CREATE TABLE t (x INT DEFAULT abs(5));",
        ),
        (
            DialectKind::PostgreSql,
            "CREATE TABLE t (x INT DEFAULT coalesce(1, 2));",
        ),
        (DialectKind::MySql, "CREATE TABLE t (x INT DEFAULT abs(5));"),
        (
            DialectKind::PostgreSql,
            "CREATE VIEW v AS SELECT abs(a) FROM t;",
        ),
        (
            DialectKind::PostgreSql,
            "CREATE VIEW v AS SELECT sum(a) FROM t;",
        ),
    ] {
        let units = parse(sql, dialect).unwrap();
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
    for (dialect, sql) in [
        (
            DialectKind::SQLite,
            "CREATE TABLE t (x TIMESTAMP DEFAULT CURRENT_TIMESTAMP);",
        ),
        (
            DialectKind::PostgreSql,
            "CREATE TABLE t (x TIMESTAMP DEFAULT CURRENT_TIMESTAMP);",
        ),
        (
            DialectKind::MySql,
            "CREATE TABLE t (x TIMESTAMP DEFAULT CURRENT_TIMESTAMP);",
        ),
        (
            DialectKind::PostgreSql,
            "CREATE TABLE t (x INT DEFAULT random());",
        ),
    ] {
        let units = parse(sql, dialect).unwrap();
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
}

#[test]
fn mysql_index_prefix_lifts_but_real_index_calls_fence() {
    // MySQL index length prefixes (`b(1)`) lower as Expr::Function but
    // name no routine: structurally excluded in key position only.
    for sql in [
        "CREATE TEMPORARY TABLE t1(i INT, b BLOB, PRIMARY KEY pk(i), INDEX sk(b(1)));",
        "CREATE TABLE t (b BLOB, UNIQUE KEY uk (b(1)));",
        "CREATE TABLE t (b BLOB, PRIMARY KEY (b(10)));",
        "CREATE INDEX sk ON t (b(1));",
        "CREATE INDEX sk ON t (`b`(1));",
        "ALTER TABLE t ADD INDEX sk (b(1));",
    ] {
        let units = parse(sql, DialectKind::MySql).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(
            facts(&units[0]).barriers.is_empty(),
            "{sql}: {:?}",
            facts(&units[0]).barriers
        );
    }
    // The index definition survives the lifted fence.
    let units = parse(
        "CREATE TEMPORARY TABLE t1(i INT, b BLOB, PRIMARY KEY pk(i), INDEX sk(b(1)));",
        DialectKind::MySql,
    )
    .unwrap();
    let table = table_def(facts(&units[0])).expect("index table def");
    assert!(
        table.constraints.iter().any(|constraint| matches!(
            constraint,
            Constraint::Index {
                name: Some(name),
                columns,
                ..
            } if name.0 == "sk" && columns.as_slice() == [ir::Ident("b".to_string())]
        )),
        "{:?}",
        table.constraints
    );
    let units = parse("CREATE INDEX sk ON t (b(1));", DialectKind::MySql).unwrap();
    assert!(
        facts(&units[0]).ops.iter().any(|op| matches!(
            op,
            CatalogOp::DefineIndex(index)
                if name_text(&index.table.name) == "t"
                    && index.columns.as_slice() == [ir::Ident("b".to_string())]
                    && !index.expression
        )),
        "{:?}",
        facts(&units[0]).ops
    );
    // Real calls in key position keep fencing: expression indexes lower
    // as Nested, and column or multi-argument calls match no prefix shape.
    for sql in [
        // Cutback: `lower` is no longer a lifted builtin, so a
        // functional key part calling it fences (name-based, not
        // structural — the `b(1)` exclusion above is untouched).
        "CREATE INDEX sk ON t ((lower(b)));",
        "CREATE INDEX sk ON t ((f(b)));",
        "CREATE INDEX sk ON t (f(b));",
        "CREATE INDEX sk ON t (f(1, 2));",
        "CREATE INDEX sk ON t (b(1.5));",
        "CREATE TABLE t (a INT, b BLOB, INDEX sk (f(b)));",
    ] {
        let units = parse(sql, DialectKind::MySql).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
    // Real calls in non-index positions still fence.
    for sql in [
        "CREATE TABLE t (x INT DEFAULT f(1));",
        "CREATE TABLE t (a INT, CHECK (f(a) > 0));",
    ] {
        let units = parse(sql, DialectKind::MySql).unwrap();
        assert!(has_routine_barrier(facts(&units[0])), "{sql}");
    }
    // The prefix production exists only on MySQL: `b(1)` in key position
    // keeps fencing elsewhere as a call.
    let units = parse("CREATE INDEX sk ON t (b(1));", DialectKind::PostgreSql).unwrap();
    assert!(units[0].parse_error.is_none());
    assert!(has_routine_barrier(facts(&units[0])));
}

#[test]
fn partition_by_scheme_lifts_but_partition_of_binds_parent() {
    // `PARTITION BY RANGE/LIST/HASH (...)` lowers the scheme keyword as
    // Function: structurally excluded from the routine fence because it
    // names no routine and creates no dependency edge.
    for scheme in ["RANGE", "LIST", "HASH"] {
        let sql = format!("CREATE TABLE m (a INT, b INT) PARTITION BY {scheme} (a, b);");
        let units = parse(&sql, DialectKind::PostgreSql).unwrap();
        assert!(
            facts(&units[0]).barriers.is_empty(),
            "{sql}: {:?}",
            facts(&units[0]).barriers
        );
        assert!(table_def(facts(&units[0])).is_some(), "{sql}");
    }
    // `PARTITION OF` binds the child to its parent with no fence.
    let units = parse(
        "CREATE TABLE c PARTITION OF p FOR VALUES FROM (1) TO (10);",
        DialectKind::PostgreSql,
    )
    .unwrap();
    let table = table_def(facts(&units[0])).expect("partition child def");
    assert!(facts(&units[0]).barriers.is_empty());
    assert_eq!(
        table
            .partition_parent
            .as_ref()
            .map(|parent| name_text(&parent.name)),
        Some("p".to_string())
    );
    assert!(table.inherit_parents.is_empty());
    // Routine shapes inside `FOR VALUES` bounds still fence.
    let units = parse(
        "CREATE TABLE c PARTITION OF p FOR VALUES IN (f(1));",
        DialectKind::PostgreSql,
    )
    .unwrap();
    assert!(has_routine_barrier(facts(&units[0])));
}
#[test]
fn inherits_binds_parents_without_fence() {
    let units = parse(
        "CREATE TABLE c (a INT) INHERITS (p1, p2);",
        DialectKind::PostgreSql,
    )
    .unwrap();
    let table = table_def(facts(&units[0])).expect("inherit child def");
    assert!(facts(&units[0]).barriers.is_empty());
    assert!(table.partition_parent.is_none());
    let parents: Vec<String> = table
        .inherit_parents
        .iter()
        .map(|parent| name_text(&parent.name))
        .collect();
    assert_eq!(parents, vec!["p1".to_string(), "p2".to_string()]);
    // An unresolvable parent shape keeps the fence.
    let units = parse(
        "CREATE TABLE c PARTITION OF a.b.c.d.e FOR VALUES IN (1);",
        DialectKind::PostgreSql,
    )
    .unwrap();
    assert!(
        facts(&units[0])
            .barriers
            .iter()
            .any(|barrier| matches!(barrier.reason, BarrierReason::UnknownDdl)),
        "{:?}",
        facts(&units[0]).barriers
    );
}

#[test]
fn attach_detach_partition_emit_link_ops() {
    let units = parse(
        "ALTER TABLE p ATTACH PARTITION c FOR VALUES FROM (1) TO (10);",
        DialectKind::PostgreSql,
    )
    .unwrap();
    assert!(facts(&units[0]).barriers.is_empty());
    assert!(matches!(
        facts(&units[0]).ops.as_slice(),
        [CatalogOp::AttachPartition(link)]
            if name_text(&link.parent.name) == "p" && name_text(&link.child.name) == "c"
    ));
    for sql in [
        "ALTER TABLE p DETACH PARTITION c;",
        "ALTER TABLE p DETACH PARTITION c FINALIZE;",
    ] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
        assert!(
            matches!(
                facts(&units[0]).ops.as_slice(),
                [CatalogOp::DetachPartition(link)]
                    if name_text(&link.parent.name) == "p"
                        && name_text(&link.child.name) == "c"
            ),
            "{sql}"
        );
    }
}

#[test]
fn inherit_no_inherit_emit_link_ops() {
    // Unlike `ATTACH PARTITION` (target parent, named child), the
    // `INHERIT` target is the child and the operand is the parent.
    let units = parse("ALTER TABLE c INHERIT p;", DialectKind::PostgreSql).unwrap();
    assert!(facts(&units[0]).barriers.is_empty());
    assert!(matches!(
        facts(&units[0]).ops.as_slice(),
        [CatalogOp::AttachInherit(link)]
            if name_text(&link.parent.name) == "p" && name_text(&link.child.name) == "c"
    ));
    let units = parse("ALTER TABLE c NO INHERIT p;", DialectKind::PostgreSql).unwrap();
    assert!(facts(&units[0]).barriers.is_empty());
    assert!(matches!(
        facts(&units[0]).ops.as_slice(),
        [CatalogOp::DetachInherit(link)]
            if name_text(&link.parent.name) == "p" && name_text(&link.child.name) == "c"
    ));
    // Schema-qualified operands keep their qualification on the right end.
    let units = parse("ALTER TABLE s.c INHERIT s.p;", DialectKind::PostgreSql).unwrap();
    assert!(facts(&units[0]).barriers.is_empty());
    assert!(matches!(
        facts(&units[0]).ops.as_slice(),
        [CatalogOp::AttachInherit(link)]
            if name_text(&link.parent.name) == "s.p" && name_text(&link.child.name) == "s.c"
    ));
}

#[test]
fn malformed_inherit_and_other_alters_keep_fence() {
    // An unresolvable parent shape fences with no link op.
    let units = parse("ALTER TABLE c INHERIT a.b.c.d.e;", DialectKind::PostgreSql).unwrap();
    assert!(facts(&units[0]).ops.is_empty());
    assert!(
        facts(&units[0])
            .barriers
            .iter()
            .any(|barrier| matches!(barrier.reason, BarrierReason::UnknownAlter))
    );
    // Unmodeled `ALTER TABLE` actions keep the blanket fence.
    for sql in ["ALTER TABLE t OWNER TO app;", "ALTER TABLE t SET LOGGED;"] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert!(
            facts(&units[0])
                .barriers
                .iter()
                .any(|barrier| matches!(barrier.reason, BarrierReason::UnknownAlter)),
            "{sql}"
        );
    }
}

#[test]
fn only_marker_threads_through_reads_and_writes() {
    let units = parse("SELECT * FROM ONLY t;", DialectKind::PostgreSql).unwrap();
    assert_eq!(reads(facts(&units[0])).len(), 1);
    assert!(reads(facts(&units[0]))[0].only);
    for sql in ["SELECT * FROM t;", "SELECT * FROM t *;"] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert_eq!(reads(facts(&units[0])).len(), 1, "{sql}");
        assert!(!reads(facts(&units[0]))[0].only, "{sql}");
    }
    for sql in [
        "UPDATE ONLY t SET a = 1;",
        "DELETE FROM ONLY t;",
        "TRUNCATE ONLY t;",
    ] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert_eq!(writes(facts(&units[0])).len(), 1, "{sql}");
        assert!(writes(facts(&units[0]))[0].only, "{sql}");
    }
    for sql in [
        "UPDATE t SET a = 1;",
        "DELETE FROM t;",
        "TRUNCATE t;",
        "INSERT INTO t VALUES (1);",
    ] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert_eq!(writes(facts(&units[0])).len(), 1, "{sql}");
        assert!(!writes(facts(&units[0]))[0].only, "{sql}");
    }
}

#[test]
fn output_into_table_adds_write_edge() {
    for sql in [
        "INSERT INTO t (a) OUTPUT inserted.a INTO t2 SELECT a FROM s;",
        "UPDATE t SET a = 1 OUTPUT inserted.a INTO t2;",
        "DELETE FROM t OUTPUT deleted.a INTO t2;",
        "MERGE INTO t USING s ON t.id = s.id WHEN MATCHED THEN UPDATE SET a = 1 OUTPUT inserted.a INTO t2;",
    ] {
        let units = parse(sql, DialectKind::MsSql).unwrap();
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
        let targets: Vec<String> = writes(facts(&units[0]))
            .iter()
            .map(|write| name_text(&write.target.name))
            .collect();
        assert_eq!(targets, vec!["t".to_string(), "t2".to_string()], "{sql}");
        assert!(
            matches!(writes(facts(&units[0]))[1].kind, WriteKind::Insert { .. }),
            "{sql}"
        );
    }
    // A qualified target resolves through the same path.
    let units = parse(
        "DELETE FROM t OUTPUT deleted.a INTO dbo.t2;",
        DialectKind::MsSql,
    )
    .unwrap();
    assert!(facts(&units[0]).barriers.is_empty());
    assert_eq!(
        name_text(&writes(facts(&units[0]))[1].target.name),
        "dbo.t2".to_string()
    );
}

#[test]
fn output_into_variable_fences_at_relation_scope() {
    let units = parse(
        "UPDATE t SET a = 1 OUTPUT deleted.a INTO @var;",
        DialectKind::MsSql,
    )
    .unwrap();
    assert_eq!(facts(&units[0]).barriers.len(), 1);
    assert!(matches!(
        facts(&units[0]).barriers.as_slice(),
        [ScopedBarrier {
            scope: BarrierScope::Relation { .. },
            reason: BarrierReason::UnknownDml,
            ..
        }]
    ));
    // The host write is still recorded.
    assert_eq!(writes(facts(&units[0])).len(), 1);
}

#[test]
fn output_without_into_has_no_extra_edge() {
    let units = parse("DELETE FROM t OUTPUT deleted.*;", DialectKind::MsSql).unwrap();
    assert!(facts(&units[0]).barriers.is_empty());
    assert_eq!(writes(facts(&units[0])).len(), 1);
    assert_eq!(reads(facts(&units[0])).len(), 0);
}
#[test]
fn mysql_foreign_key_checks_scope_forms() {
    for (sql, scope, value) in [
        ("SET foreign_key_checks = 0;", SessionScope::Session, false),
        (
            "SET FOREIGN_KEY_CHECKS = OFF;",
            SessionScope::Session,
            false,
        ),
        (
            "SET SESSION foreign_key_checks = 1;",
            SessionScope::Session,
            true,
        ),
        (
            "SET GLOBAL foreign_key_checks = 0;",
            SessionScope::Global,
            false,
        ),
        ("SET @@foreign_key_checks = 1;", SessionScope::Session, true),
        (
            "SET @@session.foreign_key_checks = 0;",
            SessionScope::Session,
            false,
        ),
        (
            "SET @@global.foreign_key_checks = 0;",
            SessionScope::Global,
            false,
        ),
        (
            "SET @@GLOBAL.foreign_key_checks = 1;",
            SessionScope::Global,
            true,
        ),
        (
            "SET foreign_key_checks = TRUE;",
            SessionScope::Session,
            true,
        ),
    ] {
        let units = parse(sql, DialectKind::MySql).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(
            matches!(
                facts(&units[0]).session.as_slice(),
                [SessionEffect::SetForeignKeyChecks {
                    scope: actual_scope,
                    value: actual_value,
                }] if actual_scope == &scope && actual_value == &value
            ),
            "{sql}: {:?}",
            facts(&units[0]).session
        );
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
    }
}

#[test]
fn mysql_foreign_key_checks_unmodeled_spellings_stay_fenced() {
    for sql in [
        "SET foreign_key_checks = @x;",
        "SET @@global.foreign_key_checks = @x;",
        "SET GLOBAL @@session.foreign_key_checks = 0;",
        "SET SESSION @@global.foreign_key_checks = 0;",
        "SET app.foreign_key_checks = 0;",
    ] {
        let units = parse(sql, DialectKind::MySql).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(facts(&units[0]).session.is_empty(), "{sql}");
        assert!(
            facts(&units[0]).barriers.iter().any(|barrier| {
                matches!(
                    barrier,
                    ScopedBarrier {
                        scope: BarrierScope::Global,
                        reason: BarrierReason::UnknownSessionEffect,
                        ..
                    }
                )
            }),
            "{sql}"
        );
    }
    let units = parse("SET @foreign_key_checks = 0;", DialectKind::MySql).unwrap();
    assert!(units[0].parse_error.is_none());
    assert!(facts(&units[0]).session.is_empty());
    assert!(
        facts(&units[0]).barriers.iter().any(|barrier| {
            matches!(
                barrier,
                ScopedBarrier {
                    scope: BarrierScope::UserVariable { name },
                    reason: BarrierReason::UnknownSessionEffect,
                    ..
                } if name.0 == "@foreign_key_checks"
            )
        }),
        "{:?}",
        facts(&units[0]).barriers
    );
    assert!(
        !facts(&units[0])
            .barriers
            .iter()
            .any(|barrier| matches!(barrier.scope, BarrierScope::Global))
    );
    // Out-of-scope variables keep the blanket fence.
    let units = parse("SET @@global.super_read_only = TRUE;", DialectKind::MySql).unwrap();
    assert!(
        facts(&units[0])
            .barriers
            .iter()
            .any(|barrier| { matches!(barrier.reason, BarrierReason::UnknownSessionEffect) })
    );
}

#[test]
fn mysql_autocommit_scope_forms() {
    for (sql, scope, value) in [
        ("SET autocommit = 0;", SessionScope::Session, false),
        ("SET SESSION autocommit = 1;", SessionScope::Session, true),
        ("SET GLOBAL autocommit = 0;", SessionScope::Global, false),
        ("SET @@autocommit = 1;", SessionScope::Session, true),
        (
            "SET @@session.autocommit = 0;",
            SessionScope::Session,
            false,
        ),
        ("SET @@global.autocommit = 0;", SessionScope::Global, false),
    ] {
        let units = parse(sql, DialectKind::MySql).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(
            matches!(
                facts(&units[0]).session.as_slice(),
                [SessionEffect::SetAutocommit {
                    scope: actual_scope,
                    value: actual_value,
                }] if actual_scope == &scope && actual_value == &value
            ),
            "{sql}: {:?}",
            facts(&units[0]).session
        );
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
    }
}

#[test]
fn system_var_user_spellings_stay_fenced() {
    for (sql, expected_var) in [
        ("SET @autocommit = 0;", "@autocommit"),
        ("SET @search_path = 'public';", "@search_path"),
    ] {
        let units = parse(sql, DialectKind::MySql).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(facts(&units[0]).session.is_empty(), "{sql}");
        assert!(
            facts(&units[0]).barriers.iter().any(|barrier| {
                matches!(
                    barrier,
                    ScopedBarrier {
                        scope: BarrierScope::UserVariable { name },
                        reason: BarrierReason::UnknownSessionEffect,
                        ..
                    } if name.0 == expected_var
                )
            }),
            "{sql}: {:?}",
            facts(&units[0]).barriers
        );
        assert!(
            !facts(&units[0])
                .barriers
                .iter()
                .any(|barrier| matches!(barrier.scope, BarrierScope::Global)),
            "{sql}"
        );
    }
    for sql in [
        "SET GLOBAL @@session.autocommit = 0;",
        "SET SESSION @@global.autocommit = 0;",
        "SET app.autocommit = 0;",
        "SET app.search_path = 'public';",
    ] {
        let units = parse(sql, DialectKind::MySql).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(facts(&units[0]).session.is_empty(), "{sql}");
        assert!(
            facts(&units[0]).barriers.iter().any(|barrier| {
                matches!(
                    barrier,
                    ScopedBarrier {
                        scope: BarrierScope::Global,
                        reason: BarrierReason::UnknownSessionEffect,
                        ..
                    }
                )
            }),
            "{sql}"
        );
    }
    // Bare system spellings still map without a fence.
    let units = parse("SET autocommit = 1;", DialectKind::MySql).unwrap();
    assert!(matches!(
        facts(&units[0]).session.as_slice(),
        [SessionEffect::SetAutocommit {
            scope: SessionScope::Session,
            value: true,
        }]
    ));
    assert!(facts(&units[0]).barriers.is_empty());
    let units = parse("SET search_path TO public;", DialectKind::PostgreSql).unwrap();
    assert!(matches!(
        facts(&units[0]).session.as_slice(),
        [SessionEffect::SetSearchPath { .. }]
    ));
    assert!(facts(&units[0]).barriers.is_empty());
}

#[test]
fn reset_at_spellings_stay_fenced() {
    for sql in [
        "RESET @search_path;",
        "RESET @foreign_key_checks;",
        "RESET @@global.foreign_key_checks;",
        "RESET @@session.search_path;",
    ] {
        let units = parse(sql, DialectKind::MySql).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(facts(&units[0]).session.is_empty(), "{sql}");
        assert!(
            facts(&units[0]).barriers.iter().any(|barrier| {
                matches!(
                    barrier,
                    ScopedBarrier {
                        scope: BarrierScope::Global,
                        reason: BarrierReason::UnknownSessionEffect,
                        ..
                    }
                )
            }),
            "{sql}"
        );
    }
    // Bare RESET forms still map without a fence.
    for sql in ["RESET search_path;", "RESET foreign_key_checks;"] {
        let units = parse(sql, DialectKind::PostgreSql).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(!facts(&units[0]).session.is_empty(), "{sql}");
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
    }
}

#[test]
fn sqlite_inert_pragmas_record_without_fence() {
    for (sql, expected_name, expected) in [
        ("PRAGMA synchronous = 2;", "synchronous", 2),
        ("PRAGMA synchronous = 1;", "synchronous", 1),
        ("PRAGMA temp_store = 2;", "temp_store", 2),
        ("PRAGMA cache_size = 1000;", "cache_size", 1000),
        ("PRAGMA page_size = 4096;", "page_size", 4096),
    ] {
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(
            matches!(
                facts(&units[0]).session.as_slice(),
                [SessionEffect::SetVariable { scope, name, value }]
                    if scope == &SessionScope::Session
                        && name_text(name) == *expected_name
                        && matches!(value, SessionValue::Integer { value } if value == &expected)
            ),
            "{sql}: {:?}",
            facts(&units[0]).session
        );
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
    }
    for (sql, expected_name, expected) in [
        ("PRAGMA journal_mode = 'wal';", "journal_mode", "wal"),
        ("PRAGMA synchronous = 'FULL';", "synchronous", "FULL"),
        ("PRAGMA temp_store = 'MEMORY';", "temp_store", "MEMORY"),
        ("PRAGMA page_size = '4096';", "page_size", "4096"),
    ] {
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(
            matches!(
                facts(&units[0]).session.as_slice(),
                [SessionEffect::SetVariable { scope, name, value }]
                    if scope == &SessionScope::Session
                        && name_text(name) == *expected_name
                        && matches!(value, SessionValue::Text { value } if value == expected)
            ),
            "{sql}: {:?}",
            facts(&units[0]).session
        );
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
    }
    // Bare reads are queries: no effect and no fence.
    for sql in ["PRAGMA journal_mode;", "PRAGMA page_size;"] {
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert!(facts(&units[0]).session.is_empty(), "{sql}");
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
    }
}

#[test]
fn sqlite_effectful_and_unknown_pragmas_stay_fenced() {
    for sql in [
        "PRAGMA recursive_triggers = 0;",
        "PRAGMA recursive_triggers = 1;",
        "PRAGMA recursive_triggers(0);",
        "PRAGMA defer_foreign_keys = 0;",
        "PRAGMA defer_foreign_keys = 1;",
        "PRAGMA defer_foreign_keys(1);",
        "PRAGMA ignore_check_constraints = 1;",
        "PRAGMA ignore_check_constraints;",
        "PRAGMA encoding = 'UTF-8';",
        "PRAGMA encoding;",
        "PRAGMA frobnicate = 1;",
        "PRAGMA frobnicate;",
        "PRAGMA integrity_check;",
        "PRAGMA page_size = ?;",
    ] {
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(facts(&units[0]).session.is_empty(), "{sql}");
        assert!(
            facts(&units[0]).barriers.iter().any(|barrier| {
                matches!(
                    barrier,
                    ScopedBarrier {
                        scope: BarrierScope::Global,
                        reason: BarrierReason::UnknownSessionEffect,
                        ..
                    }
                )
            }),
            "{sql}"
        );
    }
}

#[test]
fn sqlite_pure_pragma_reads_lift_without_fence() {
    // Bare flag queries return the current setting and mutate nothing,
    // exactly like `PRAGMA foreign_keys;`.
    for sql in [
        "PRAGMA recursive_triggers;",
        "PRAGMA defer_foreign_keys;",
        "PRAGMA main.recursive_triggers;",
        "PRAGMA foreign_keys;",
    ] {
        let units = parse(sql, DialectKind::SQLite).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql}");
        assert!(facts(&units[0]).session.is_empty(), "{sql}");
        assert!(facts(&units[0]).barriers.is_empty(), "{sql}");
    }
}

#[test]
fn sqlite_create_virtual_table_scopes_barrier_to_relation() {
    let sql = "CREATE VIRTUAL TABLE vt0 USING fts5(c0);";
    let units = parse(sql, DialectKind::SQLite).unwrap();
    assert!(units[0].parse_error.is_none(), "{sql}");
    let f0 = facts(&units[0]);
    assert_eq!(f0.ops.len(), 1);
    match &f0.ops[0] {
        CatalogOp::DefineTable(table) => {
            assert_eq!(name_text(&table.name), "vt0");
            assert!(!table.temporary);
            assert!(table.transactional);
            assert!(!table.if_not_exists);
        }
        other => panic!("expected DefineTable, got {:?}", other),
    }
    assert_eq!(f0.barriers.len(), 1);
    match &f0.barriers[0] {
        ScopedBarrier {
            scope: BarrierScope::Relation { relation },
            reason: BarrierReason::UnknownStatement,
            ..
        } => {
            assert_eq!(name_text(&relation.name), "vt0");
            assert!(!relation.temp);
        }
        other => panic!("expected RelationScope barrier, got {:?}", other),
    }

    let temp_sql = "CREATE VIRTUAL TABLE IF NOT EXISTS temp.vt1 USING fts4(c0, c1);";
    let units = parse(temp_sql, DialectKind::SQLite).unwrap();
    assert!(units[0].parse_error.is_none(), "{temp_sql}");
    let f1 = facts(&units[0]);
    assert_eq!(f1.ops.len(), 1);
    match &f1.ops[0] {
        CatalogOp::DefineTable(table) => {
            assert_eq!(name_text(&table.name), "vt1");
            assert!(table.temporary);
            assert!(table.if_not_exists);
        }
        other => panic!("expected DefineTable, got {:?}", other),
    }
    assert_eq!(f1.barriers.len(), 1);
    match &f1.barriers[0] {
        ScopedBarrier {
            scope: BarrierScope::Relation { relation },
            reason: BarrierReason::UnknownStatement,
            ..
        } => {
            assert_eq!(name_text(&relation.name), "vt1");
            assert!(relation.temp);
        }
        other => panic!("expected RelationScope barrier, got {:?}", other),
    }

    let no_args_sql = "CREATE VIRTUAL TABLE vt_no_args USING dbstat;";
    let units = parse(no_args_sql, DialectKind::SQLite).unwrap();
    assert!(units[0].parse_error.is_none(), "{no_args_sql}");
    let f2 = facts(&units[0]);
    assert_eq!(f2.ops.len(), 1);
    match &f2.ops[0] {
        CatalogOp::DefineTable(table) => {
            assert_eq!(name_text(&table.name), "vt_no_args");
            assert!(!table.temporary);
        }
        other => panic!("expected DefineTable, got {:?}", other),
    }
    assert_eq!(f2.barriers.len(), 1);
    match &f2.barriers[0] {
        ScopedBarrier {
            scope: BarrierScope::Relation { relation },
            reason: BarrierReason::UnknownStatement,
            ..
        } => {
            assert_eq!(name_text(&relation.name), "vt_no_args");
            assert!(!relation.temp);
        }
        other => panic!("expected RelationScope barrier, got {:?}", other),
    }

    let main_sql = "CREATE VIRTUAL TABLE main.vt_main USING fts5(c0);";
    let units = parse(main_sql, DialectKind::SQLite).unwrap();
    assert!(units[0].parse_error.is_none(), "{main_sql}");
    let f3 = facts(&units[0]);
    assert_eq!(f3.ops.len(), 1);
    match &f3.ops[0] {
        CatalogOp::DefineTable(table) => {
            assert_eq!(name_text(&table.name), "main.vt_main");
            assert!(!table.temporary);
        }
        other => panic!("expected DefineTable, got {:?}", other),
    }
    assert_eq!(f3.barriers.len(), 1);
    match &f3.barriers[0] {
        ScopedBarrier {
            scope: BarrierScope::Relation { relation },
            reason: BarrierReason::UnknownStatement,
            ..
        } => {
            assert_eq!(name_text(&relation.name), "main.vt_main");
            assert!(!relation.temp);
        }
        other => panic!("expected RelationScope barrier, got {:?}", other),
    }

    let quoted_sql = r#"CREATE VIRTUAL TABLE "vt spaced" USING fts5("c 0");"#;
    let units = parse(quoted_sql, DialectKind::SQLite).unwrap();
    assert!(units[0].parse_error.is_none(), "{quoted_sql}");
    let f4 = facts(&units[0]);
    assert_eq!(f4.ops.len(), 1);
    match &f4.ops[0] {
        CatalogOp::DefineTable(table) => {
            assert_eq!(name_text(&table.name), "vt spaced");
            assert!(!table.temporary);
        }
        other => panic!("expected DefineTable, got {:?}", other),
    }
    assert_eq!(f4.barriers.len(), 1);
    match &f4.barriers[0] {
        ScopedBarrier {
            scope: BarrierScope::Relation { relation },
            reason: BarrierReason::UnknownStatement,
            ..
        } => {
            assert_eq!(name_text(&relation.name), "vt spaced");
            assert!(!relation.temp);
        }
        other => panic!("expected RelationScope barrier, got {:?}", other),
    }
}

#[test]
fn unconditional_and_filtered_update_scope() {
    let sql = "UPDATE t SET a = 1;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    assert!(
        matches!(write[0].kind, WriteKind::Update { all: true }),
        "{:?}",
        write[0].kind
    );
    assert!(
        write[0].reads.is_empty(),
        "unconditional update without target column references should have no spurious read on target: {:?}",
        write[0].reads
    );

    let sql = "UPDATE t SET a = 1 WHERE b = 2;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    assert!(
        matches!(write[0].kind, WriteKind::Update { all: false }),
        "{:?}",
        write[0].kind
    );
    assert_eq!(write[0].reads.len(), 1);
    assert_eq!(write[0].reads[0].columns, vec![Ident("b".to_string())]);

    let sql = "UPDATE t SET a = 1 FROM u WHERE t.id = u.id;";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    assert!(
        matches!(write[0].kind, WriteKind::Update { all: false }),
        "{:?}",
        write[0].kind
    );

    let sql = "UPDATE t SET a = 1 LIMIT 1;";
    let units = parse(sql, DialectKind::MySql).unwrap();
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    assert!(
        matches!(write[0].kind, WriteKind::Update { all: false }),
        "UPDATE with LIMIT must be filtered, got {:?}",
        write[0].kind
    );

    let sql = "UPDATE t SET a = (SELECT 1 FROM t);";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    assert!(
        !write[0].reads.is_empty(),
        "UPDATE reading from t in a subquery must retain the read: {:?}",
        write[0].reads
    );

    let sql = "UPDATE t SET a = 1 RETURNING a;";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    let write = writes(facts(&units[0]));
    assert_eq!(write.len(), 1);
    assert_eq!(write[0].reads.len(), 1);
    assert_eq!(write[0].reads[0].columns, vec![Ident("a".to_string())]);
}

#[test]
fn row_count_respects_routine_identifier_folding() {
    for (dialect, call, expected) in [
        (DialectKind::PostgreSql, "COUNT(1)", ReadMode::RowCount),
        (DialectKind::PostgreSql, r#""count"(1)"#, ReadMode::RowCount),
        (
            DialectKind::PostgreSql,
            r#""pg_catalog".COUNT(1)"#,
            ReadMode::RowCount,
        ),
        (DialectKind::PostgreSql, r#""COUNT"(1)"#, ReadMode::Data),
        (
            DialectKind::PostgreSql,
            r#""PG_CATALOG".count(1)"#,
            ReadMode::Data,
        ),
        (
            DialectKind::PostgreSql,
            r#"pg_catalog."COUNT"(1)"#,
            ReadMode::Data,
        ),
        (DialectKind::Redshift, r#""COUNT"(1)"#, ReadMode::Data),
        (DialectKind::SQLite, r#""COUNT"(1)"#, ReadMode::RowCount),
        (DialectKind::SQLite, "pg_catalog.count(1)", ReadMode::Data),
    ] {
        let sql = format!("SELECT {call} FROM t;");
        let units = parse(&sql, dialect).unwrap();
        assert!(units[0].parse_error.is_none(), "{sql} on {dialect:?}");
        let query = facts(&units[0]);
        assert_eq!(reads(query)[0].mode, expected, "{sql} on {dialect:?}");
        assert_eq!(
            has_routine_barrier(query),
            expected == ReadMode::Data,
            "{sql} on {dialect:?}"
        );
    }
}

#[test]
fn routine_ddl_disables_count_specialization() {
    let select = "SELECT count(1) FROM t;";
    for ddl in [
        "CREATE FUNCTION count(x INT) RETURNS INT LANGUAGE SQL AS 'SELECT x';",
        "CREATE AGGREGATE count(int) (SFUNC = int4pl, STYPE = int);",
    ] {
        let sql = format!("{ddl} {select}");
        let units = parse(&sql, DialectKind::PostgreSql).unwrap();
        assert_eq!(units.len(), 2, "{sql}");
        assert!(units.iter().all(|unit| unit.parse_error.is_none()), "{sql}");
        let query = facts(&units[1]);
        assert_eq!(reads(query)[0].mode, ReadMode::Data, "{sql}");
        assert!(has_routine_barrier(query), "{sql}");
    }
}

#[test]
fn read_mode_classification_row_count_and_existence() {
    let sql = "SELECT COUNT(*) FROM t;";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts.len(), 1);
    assert_eq!(read_facts[0].mode, ReadMode::RowCount);
    assert_eq!(name_text(&read_facts[0].source.name), "t");
    assert!(read_facts[0].columns.is_empty());

    let sql = "SELECT COUNT(1) AS c FROM t;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts.len(), 1);
    assert_eq!(read_facts[0].mode, ReadMode::RowCount);

    // COUNT(col) reads column data because nulls are filtered
    let sql = "SELECT COUNT(col) FROM t;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts.len(), 1);
    assert_eq!(read_facts[0].mode, ReadMode::Data);

    // COUNT(*) with WHERE reads column data
    let sql = "SELECT COUNT(*) FROM t WHERE x > 1;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts.len(), 1);
    assert_eq!(read_facts[0].mode, ReadMode::Data);

    // COUNT(*) with GROUP BY reads column data
    let sql = "SELECT COUNT(*) FROM t GROUP BY x;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts.len(), 1);
    assert_eq!(read_facts[0].mode, ReadMode::Data);

    // Pure existence check with LIMIT 1
    let sql = "SELECT 1 FROM t LIMIT 1;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts.len(), 1);
    assert_eq!(read_facts[0].mode, ReadMode::RowExistence);

    // Pure existence check with EXISTS
    let sql = "SELECT EXISTS (SELECT 1 FROM t);";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts.len(), 1);
    assert_eq!(read_facts[0].mode, ReadMode::RowExistence);

    let sql = "SELECT EXISTS (SELECT * FROM t);";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts.len(), 1);
    assert_eq!(read_facts[0].mode, ReadMode::RowExistence);

    // COUNT(*) OVER () window function must fall back to Data
    let sql = "SELECT COUNT(*) OVER () FROM t;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts[0].mode, ReadMode::Data);

    // COUNT(NULL) ignores nulls and does not count rows
    let sql = "SELECT COUNT(NULL) FROM t;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts[0].mode, ReadMode::Data);

    // Non-null constant literal in count position is RowCount
    let sql = "SELECT COUNT('hello') FROM t;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts[0].mode, ReadMode::RowCount);

    // Zero limit produces empty results, not pure existence check
    for zero_limit in ["SELECT 1 FROM t LIMIT 0;", "SELECT 1 FROM t LIMIT 00;"] {
        let units = parse(zero_limit, DialectKind::Generic).unwrap();
        let read_facts = reads(facts(&units[0]));
        assert_eq!(read_facts[0].mode, ReadMode::Data, "{zero_limit}");
    }

    // FOR UPDATE locks rows and must fall back to Data
    let sql = "SELECT COUNT(*) FROM t FOR UPDATE;";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts[0].mode, ReadMode::Data);

    let sql = "SELECT 1 FROM t LIMIT 1 FOR UPDATE;";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts[0].mode, ReadMode::Data);

    let sql = "SELECT EXISTS (SELECT 1 FROM t FOR UPDATE);";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    let read_facts = reads(facts(&units[0]));
    assert_eq!(read_facts[0].mode, ReadMode::Data);
}

#[test]
fn test_duckdb_create_macro() {
    let sql = "CREATE MACRO add(a, b) AS a + b;";
    let units = parse(sql, DialectKind::DuckDB).unwrap();
    assert_eq!(units.len(), 1);
    assert!(units[0].parse_error.is_none());
    let unit_facts = facts(&units[0]);
    assert_eq!(unit_facts.barriers, vec![]);
    assert!(reads(unit_facts).is_empty());

    let sql = "CREATE MACRO select_t() AS TABLE SELECT * FROM t;";
    let units = parse(sql, DialectKind::DuckDB).unwrap();
    assert_eq!(units.len(), 1);
    assert!(units[0].parse_error.is_none());
    let unit_facts = facts(&units[0]);
    assert_eq!(unit_facts.barriers, vec![]);
    let read_facts = reads(unit_facts);
    assert_eq!(read_facts.len(), 1);
    assert_eq!(name_text(&read_facts[0].source.name), "t");
}

#[test]
fn test_pure_table_generators() {
    let sql = "SELECT * FROM generate_series(1, 10);";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    assert_eq!(units.len(), 1);
    assert!(units[0].parse_error.is_none());
    let unit_facts = facts(&units[0]);
    assert_eq!(unit_facts.barriers, vec![]);
    assert!(reads(unit_facts).is_empty());

    let sql = "SELECT * FROM pg_catalog.generate_series(1, 10);";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    assert_eq!(units.len(), 1);
    assert!(units[0].parse_error.is_none());
    let unit_facts = facts(&units[0]);
    assert_eq!(unit_facts.barriers, vec![]);
    assert!(reads(unit_facts).is_empty());

    let sql = "SELECT * FROM range(10) AS t;";
    let units = parse(sql, DialectKind::DuckDB).unwrap();
    assert_eq!(units.len(), 1);
    assert!(units[0].parse_error.is_none());
    let unit_facts = facts(&units[0]);
    assert_eq!(unit_facts.barriers, vec![]);
    assert!(reads(unit_facts).is_empty());

    let sql = "SELECT * FROM analytics.generate_series(1, 10);";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    let unit_facts = facts(&units[0]);
    assert!(
        !unit_facts.barriers.is_empty(),
        "Custom schema function should not be treated as built-in pure generator"
    );
}

#[test]
fn test_file_table_functions() {
    let sql = "SELECT * FROM read_parquet('data.parquet') AS p;";
    let units = parse(sql, DialectKind::DuckDB).unwrap();
    assert_eq!(units.len(), 1);
    assert!(units[0].parse_error.is_none());
    let unit_facts = facts(&units[0]);
    assert_eq!(unit_facts.barriers, vec![]);
    let read_facts = reads(unit_facts);
    assert_eq!(read_facts.len(), 1);
    assert_eq!(name_text(&read_facts[0].source.name), "data.parquet");

    let sql = "SELECT * FROM read_csv(['f1.csv', 'f2.csv']);";
    let units = parse(sql, DialectKind::DuckDB).unwrap();
    assert_eq!(units.len(), 1);
    assert!(units[0].parse_error.is_none());
    let unit_facts = facts(&units[0]);
    assert_eq!(unit_facts.barriers, vec![]);
    let read_facts = reads(unit_facts);
    assert_eq!(read_facts.len(), 2);
    assert_eq!(name_text(&read_facts[0].source.name), "f1.csv");
    assert_eq!(name_text(&read_facts[1].source.name), "f2.csv");

    let sql = "SELECT * FROM read_csv('data.csv', delim = '|', nullstr = 'NA');";
    let units = parse(sql, DialectKind::DuckDB).unwrap();
    let unit_facts = facts(&units[0]);
    let read_facts = reads(unit_facts);
    assert_eq!(
        read_facts.len(),
        1,
        "Got reads: {:?}",
        read_facts
            .iter()
            .map(|r| name_text(&r.source.name))
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_update_and_delete_syntax_edits() {
    let sql = "UPDATE t SET a = 1, b = 2, c = 3 WHERE id = 10;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    if units[0].facts.is_none() {
        panic!("parse error: {:?}", units[0].parse_error);
    }
    let edits = &facts(&units[0]).edits;

    let assignment_edits: Vec<String> = edits
        .iter()
        .filter(|edit| edit.label == "remove-update_assignment")
        .map(|edit| {
            format!(
                "{}{}{}",
                &sql[..edit.start],
                edit.replacement,
                &sql[edit.end..]
            )
        })
        .collect();
    assert_eq!(
        assignment_edits,
        vec![
            "UPDATE t SET b = 2, c = 3 WHERE id = 10;",
            "UPDATE t SET a = 1, c = 3 WHERE id = 10;",
            "UPDATE t SET a = 1, b = 2 WHERE id = 10;",
        ]
    );

    let where_edits: Vec<String> = edits
        .iter()
        .filter(|edit| edit.label == "remove-where")
        .map(|edit| {
            format!(
                "{}{}{}",
                &sql[..edit.start],
                edit.replacement,
                &sql[edit.end..]
            )
        })
        .collect();
    assert_eq!(where_edits, vec!["UPDATE t SET a = 1, b = 2, c = 3;"]);

    let sql2 = "UPDATE t SET a = 1 ORDER BY id LIMIT 5;";
    let units2 = parse(sql2, DialectKind::MySql).unwrap();
    let edits2 = &facts(&units2[0]).edits;
    assert!(edits2.iter().any(|e| e.label == "remove-order_by"));
    assert!(edits2.iter().any(|e| e.label == "remove-limit"));

    let sql3 = "UPDATE t SET a = 1 RETURNING a, b;";
    let units3 = parse(sql3, DialectKind::PostgreSql).unwrap();
    let edits3 = &facts(&units3[0]).edits;
    assert!(
        edits3
            .iter()
            .any(|e| e.label == "remove-returning_expression")
    );
    assert!(edits3.iter().any(|e| e.label == "remove-returning"));

    let sql4 = "DELETE FROM t WHERE id = 10 ORDER BY id LIMIT 5;";
    let units4 = parse(sql4, DialectKind::MySql).unwrap();
    let edits4 = &facts(&units4[0]).edits;
    assert!(edits4.iter().any(|e| e.label == "remove-where"));
    assert!(edits4.iter().any(|e| e.label == "remove-order_by"));
    assert!(edits4.iter().any(|e| e.label == "remove-limit"));

    let sql5 = "DELETE FROM t WHERE id = 10 RETURNING id;";
    let units5 = parse(sql5, DialectKind::PostgreSql).unwrap();
    let edits5 = &facts(&units5[0]).edits;
    assert!(edits5.iter().any(|e| e.label == "remove-where"));
    assert!(edits5.iter().any(|e| e.label == "remove-returning"));

    let sql6 = "UPDATE t1 JOIN t2 ON t1.id = t2.id JOIN t3 ON t1.id = t3.id SET t1.a = 1;";
    let units6 = parse(sql6, DialectKind::MySql).unwrap();
    let edits6 = &facts(&units6[0]).edits;
    let join_edits: Vec<_> = edits6.iter().filter(|e| e.label == "remove-join").collect();
    assert_eq!(join_edits.len(), 2);
}

#[test]
fn test_set_operation_syntax_edits() {
    let sql = "SELECT 1 UNION ALL SELECT 2;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let edits = &facts(&units[0]).edits;

    let applied: Vec<String> = edits
        .iter()
        .filter(|edit| {
            edit.label == "simplify-set_operation_left"
                || edit.label == "simplify-set_operation_right"
        })
        .map(|edit| {
            format!(
                "{}{}{}",
                &sql[..edit.start],
                edit.replacement,
                &sql[edit.end..]
            )
        })
        .collect();

    assert_eq!(
        applied,
        vec![
            "SELECT 1;", // simplify-set_operation_left
            "SELECT 2;", // simplify-set_operation_right
        ]
    );

    let sql2 = "(SELECT 1) UNION ALL (SELECT 2);";
    let units2 = parse(sql2, DialectKind::Generic).unwrap();
    let edits2 = &facts(&units2[0]).edits;

    let applied2: Vec<String> = edits2
        .iter()
        .filter(|edit| {
            edit.label == "simplify-set_operation_left"
                || edit.label == "simplify-set_operation_right"
        })
        .map(|edit| {
            format!(
                "{}{}{}",
                &sql2[..edit.start],
                edit.replacement,
                &sql2[edit.end..]
            )
        })
        .collect();

    assert_eq!(applied2, vec!["(SELECT 1);", "(SELECT 2);",]);

    let sql3 = "SELECT 1 UNION ALL SELECT 2 ORDER BY 1 LIMIT 1;";
    let units3 = parse(sql3, DialectKind::Generic).unwrap();
    let edits3 = &facts(&units3[0]).edits;

    let applied3: Vec<String> = edits3
        .iter()
        .filter(|edit| {
            edit.label == "simplify-set_operation_left"
                || edit.label == "simplify-set_operation_right"
        })
        .map(|edit| {
            format!(
                "{}{}{}",
                &sql3[..edit.start],
                edit.replacement,
                &sql3[edit.end..]
            )
        })
        .collect();

    assert_eq!(
        applied3,
        vec![
            "SELECT 1 ORDER BY 1 LIMIT 1;",
            "SELECT 2 ORDER BY 1 LIMIT 1;",
        ]
    );

    let sql4 = "SELECT 999 AS noise UNION ALL (SELECT 888 AS noise) UNION ALL SELECT 42 AS target;";
    let units4 = parse(sql4, DialectKind::Generic).unwrap();
    let edits4 = &facts(&units4[0]).edits;
    let applied4 = edits4
        .iter()
        .filter(|e| e.label.starts_with("simplify-set_operation"))
        .map(|e| {
            let mut s = sql4.to_string();
            s.replace_range(e.start..e.end, &e.replacement);
            s
        })
        .collect::<Vec<_>>();
    assert_eq!(
        applied4,
        vec![
            "SELECT 999 AS noise UNION ALL (SELECT 888 AS noise);",
            "SELECT 42 AS target;",
            "SELECT 999 AS noise UNION ALL SELECT 42 AS target;",
            "(SELECT 888 AS noise) UNION ALL SELECT 42 AS target;",
        ]
    );
}

#[test]
fn test_distinct_and_group_by_syntax_edits() {
    let sql = "SELECT DISTINCT a, b FROM t GROUP BY a, b ORDER BY a, b;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let edits = &facts(&units[0]).edits;

    let distinct_edit = edits.iter().find(|e| e.label == "remove-distinct").unwrap();
    let mut s = sql.to_string();
    s.replace_range(
        distinct_edit.start..distinct_edit.end,
        &distinct_edit.replacement,
    );
    assert_eq!(s, "SELECT a, b FROM t GROUP BY a, b ORDER BY a, b;");

    let group_edits: Vec<_> = edits
        .iter()
        .filter(|e| e.label == "remove-group_by_expression")
        .map(|e| {
            let mut s = sql.to_string();
            s.replace_range(e.start..e.end, &e.replacement);
            s
        })
        .collect();
    assert_eq!(
        group_edits,
        vec![
            "SELECT DISTINCT a, b FROM t GROUP BY b ORDER BY a, b;",
            "SELECT DISTINCT a, b FROM t GROUP BY a ORDER BY a, b;",
        ]
    );

    let order_edits: Vec<_> = edits
        .iter()
        .filter(|e| e.label == "remove-order_by_expression")
        .map(|e| {
            let mut s = sql.to_string();
            s.replace_range(e.start..e.end, &e.replacement);
            s
        })
        .collect();
    assert_eq!(
        order_edits,
        vec![
            "SELECT DISTINCT a, b FROM t GROUP BY a, b ORDER BY b;",
            "SELECT DISTINCT a, b FROM t GROUP BY a, b ORDER BY a;",
        ]
    );

    let sql2 = "SELECT ALL a FROM t;";
    let units2 = parse(sql2, DialectKind::Generic).unwrap();
    let edits2 = &facts(&units2[0]).edits;
    let all_edit = edits2
        .iter()
        .find(|e| e.label == "remove-distinct")
        .unwrap();
    let mut s2 = sql2.to_string();
    s2.replace_range(all_edit.start..all_edit.end, &all_edit.replacement);
    assert_eq!(s2, "SELECT a FROM t;");

    let sql3 = "SELECT DISTINCT ON (a, b) c FROM t;";
    let units3 = parse(sql3, DialectKind::PostgreSql).unwrap();
    let edits3 = &facts(&units3[0]).edits;
    let on_edit = edits3
        .iter()
        .find(|e| e.label == "remove-distinct")
        .unwrap();
    let mut s3 = sql3.to_string();
    s3.replace_range(on_edit.start..on_edit.end, &on_edit.replacement);
    assert_eq!(s3, "SELECT c FROM t;");

    let on_expr_edits: Vec<_> = edits3
        .iter()
        .filter(|e| e.label == "remove-distinct_on_expression")
        .map(|e| {
            let mut s = sql3.to_string();
            s.replace_range(e.start..e.end, &e.replacement);
            s
        })
        .collect();
    assert_eq!(
        on_expr_edits,
        vec![
            "SELECT DISTINCT ON (b) c FROM t;",
            "SELECT DISTINCT ON (a) c FROM t;",
        ]
    );

    let sql4 = "SELECT count(DISTINCT x) FROM t;";
    let units4 = parse(sql4, DialectKind::Generic).unwrap();
    let edits4 = &facts(&units4[0]).edits;
    let count_distinct_edit = edits4
        .iter()
        .find(|e| e.label == "remove-distinct")
        .unwrap();
    let mut s4 = sql4.to_string();
    s4.replace_range(
        count_distinct_edit.start..count_distinct_edit.end,
        &count_distinct_edit.replacement,
    );
    assert_eq!(s4, "SELECT count(x) FROM t;");
}

#[test]
fn test_window_spec_syntax_edits() {
    let sql = "SELECT sum(x) OVER (PARTITION BY a, b ORDER BY c, d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let edits = &facts(&units[0]).edits;

    let empty_edit = edits
        .iter()
        .find(|e| e.label == "simplify-window_spec_empty")
        .unwrap();
    let mut s = sql.to_string();
    s.replace_range(empty_edit.start..empty_edit.end, &empty_edit.replacement);
    assert_eq!(s, "SELECT sum(x) OVER () FROM t;");

    let part_edit = edits
        .iter()
        .find(|e| e.label == "remove-window_partition_by")
        .unwrap();
    let mut s_part = sql.to_string();
    s_part.replace_range(part_edit.start..part_edit.end, &part_edit.replacement);
    assert_eq!(
        s_part,
        "SELECT sum(x) OVER (ORDER BY c, d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;"
    );

    let order_edit = edits
        .iter()
        .find(|e| e.label == "remove-window_order_by")
        .unwrap();
    let mut s_order = sql.to_string();
    s_order.replace_range(order_edit.start..order_edit.end, &order_edit.replacement);
    assert_eq!(
        s_order,
        "SELECT sum(x) OVER (PARTITION BY a, b ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;"
    );

    let frame_edit = edits
        .iter()
        .find(|e| e.label == "remove-window_frame")
        .unwrap();
    let mut s_frame = sql.to_string();
    s_frame.replace_range(frame_edit.start..frame_edit.end, &frame_edit.replacement);
    assert_eq!(
        s_frame,
        "SELECT sum(x) OVER (PARTITION BY a, b ORDER BY c, d) FROM t;"
    );

    let part_expr_edits: Vec<_> = edits
        .iter()
        .filter(|e| e.label == "remove-window_partition_by_expression")
        .map(|e| {
            let mut s = sql.to_string();
            s.replace_range(e.start..e.end, &e.replacement);
            s
        })
        .collect();
    assert_eq!(
        part_expr_edits,
        vec![
            "SELECT sum(x) OVER (PARTITION BY b ORDER BY c, d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;",
            "SELECT sum(x) OVER (PARTITION BY a ORDER BY c, d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;",
        ]
    );

    let order_expr_edits: Vec<_> = edits
        .iter()
        .filter(|e| e.label == "remove-window_order_by_expression")
        .map(|e| {
            let mut s = sql.to_string();
            s.replace_range(e.start..e.end, &e.replacement);
            s
        })
        .collect();
    assert_eq!(
        order_expr_edits,
        vec![
            "SELECT sum(x) OVER (PARTITION BY a, b ORDER BY d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;",
            "SELECT sum(x) OVER (PARTITION BY a, b ORDER BY c ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;",
        ]
    );
}

#[test]
fn lexical_suffix_preserves_prefix_and_disables_pure_builtins() {
    let sql = "SELECT coalesce(1, 2); /* unfinished";
    let units = parse(sql, DialectKind::Generic).unwrap();
    assert_eq!(count_units(sql, DialectKind::Generic), Ok(units.len()));
    assert_eq!(units.len(), 2);
    assert!(!facts(&units[0]).barriers.is_empty());
    assert_eq!(&sql[units[1].start..units[1].end], "/* unfinished");
    assert!(units[1].parse_error.is_some());
}

#[test]
fn test_cast_simplification_syntax_edits() {
    let sql = "SELECT CAST(col AS VARCHAR(255)) FROM t;";
    let units = parse(sql, DialectKind::Generic).unwrap();
    let edits = &facts(&units[0]).edits;
    let cast_edit = edits.iter().find(|e| e.label == "simplify-cast").unwrap();
    let mut s = sql.to_string();
    s.replace_range(cast_edit.start..cast_edit.end, &cast_edit.replacement);
    assert_eq!(s, "SELECT col FROM t;");

    let sql2 = "SELECT col::text FROM t;";
    let units2 = parse(sql2, DialectKind::PostgreSql).unwrap();
    let edits2 = &facts(&units2[0]).edits;
    let cast_edit2 = edits2.iter().find(|e| e.label == "simplify-cast").unwrap();
    let mut s2 = sql2.to_string();
    s2.replace_range(cast_edit2.start..cast_edit2.end, &cast_edit2.replacement);
    assert_eq!(s2, "SELECT col FROM t;");

    let sql3 = "SELECT TRY_CAST(col AS INT) FROM t;";
    let units3 = parse(sql3, DialectKind::Generic).unwrap();
    let edits3 = &facts(&units3[0]).edits;
    let cast_edit3 = edits3.iter().find(|e| e.label == "simplify-cast").unwrap();
    let mut s3 = sql3.to_string();
    s3.replace_range(cast_edit3.start..cast_edit3.end, &cast_edit3.replacement);
    assert_eq!(s3, "SELECT col FROM t;");
}

#[test]
fn sqlite_identifiers_fold_ascii_only() {
    let sql = r#"CREATE TABLE "Å"(x); CREATE TABLE "å"(y); SELECT x FROM "Å";"#;
    let units = parse(sql, DialectKind::SQLite).unwrap();
    assert_eq!(mentioned_relations(facts(&units[0])), ["Å"]);
    assert_eq!(mentioned_relations(facts(&units[1])), ["å"]);
    assert_eq!(relation_reads(facts(&units[2])), ["Å"]);
}

#[test]
fn partial_assignment_span_is_not_used_for_list_deletion() {
    let sql = "UPDATE t SET a = 1, b = sum(c) FILTER (WHERE d > 0)";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    assert!(
        !facts(&units[0])
            .edits
            .iter()
            .any(|edit| edit.label == "remove-update_assignment")
    );
}

#[test]
fn window_partition_deletion_skips_nested_order_by() {
    let sql =
        "SELECT sum(a) OVER (PARTITION BY (SELECT b FROM u ORDER BY b LIMIT 1) ORDER BY c) FROM t";
    let units = parse(sql, DialectKind::PostgreSql).unwrap();
    let edit = facts(&units[0])
        .edits
        .iter()
        .find(|edit| edit.label == "remove-window_partition_by")
        .unwrap();
    let mut candidate = sql.to_owned();
    candidate.replace_range(edit.start..edit.end, &edit.replacement);
    assert_eq!(candidate, "SELECT sum(a) OVER (ORDER BY c) FROM t");
}

#[test]
fn user_variables_are_excluded_from_table_reads_and_scoped() {
    let sql = "SET @f = _utf32 'a'; CREATE TABLE t1 (a INT); SELECT 1 FROM t1 WHERE @f<>'3761-03-28' XOR (@f:='5570-12-30') > a;";
    let units = parse(sql, DialectKind::MySql).unwrap();
    assert_eq!(units.len(), 3);
    assert!(units.iter().all(|unit| unit.parse_error.is_none()));
    assert!(
        facts(&units[0])
            .barriers
            .iter()
            .any(|b| matches!(&b.scope, BarrierScope::UserVariable { name } if name.0 == "@f"))
    );
    assert!(
        !facts(&units[0])
            .barriers
            .iter()
            .any(|b| matches!(b.scope, BarrierScope::Global))
    );
    assert_eq!(relation_reads(facts(&units[2])), ["t1"]);
    assert_eq!(read_column_names(facts(&units[2])), ["a"]);
    assert!(
        facts(&units[2])
            .barriers
            .iter()
            .any(|b| matches!(&b.scope, BarrierScope::UserVariable { name } if name.0 == "@f"))
    );
    assert!(
        !facts(&units[2])
            .barriers
            .iter()
            .any(|b| matches!(b.scope, BarrierScope::Global))
    );

    let mdev_units = parse(
        "SELECT a FROM t WHERE (a,b) in (SELECT @c,@d);",
        DialectKind::MySql,
    )
    .unwrap();
    assert!(mdev_units[0].parse_error.is_none());
    assert_eq!(relation_reads(facts(&mdev_units[0])), ["t"]);
    assert_eq!(read_column_names(facts(&mdev_units[0])), ["a", "b"]);
    assert!(
        facts(&mdev_units[0])
            .barriers
            .iter()
            .any(|b| matches!(&b.scope, BarrierScope::UserVariable { name } if name.0 == "@c"))
    );
    assert!(
        facts(&mdev_units[0])
            .barriers
            .iter()
            .any(|b| matches!(&b.scope, BarrierScope::UserVariable { name } if name.0 == "@d"))
    );
    assert!(
        !facts(&mdev_units[0])
            .barriers
            .iter()
            .any(|b| matches!(b.scope, BarrierScope::Global))
    );

    let set_expr_units = parse("SET @X = @Y + 1;", DialectKind::MySql).unwrap();
    assert!(set_expr_units[0].parse_error.is_none());
    let set_vars: Vec<&str> = facts(&set_expr_units[0])
        .barriers
        .iter()
        .filter_map(|b| match &b.scope {
            BarrierScope::UserVariable { name } => Some(name.0.as_str()),
            _ => None,
        })
        .collect();
    assert!(set_vars.contains(&"@x"), "{set_vars:?}");
    assert!(set_vars.contains(&"@y"), "{set_vars:?}");
    assert!(
        !facts(&set_expr_units[0])
            .barriers
            .iter()
            .any(|b| matches!(b.scope, BarrierScope::Global))
    );

    let sysvar_units = parse("SELECT @@sql_mode FROM t;", DialectKind::MySql).unwrap();
    assert!(sysvar_units[0].parse_error.is_none());
    assert_eq!(relation_reads(facts(&sysvar_units[0])), ["t"]);
    assert!(read_column_names(facts(&sysvar_units[0])).is_empty());

    let into_units = parse("SELECT a INTO @x FROM t;", DialectKind::MySql).unwrap();
    assert!(into_units[0].parse_error.is_none());
    assert_eq!(relation_reads(facts(&into_units[0])), ["t"]);
    assert_eq!(read_column_names(facts(&into_units[0])), ["a"]);
    assert!(
        facts(&into_units[0])
            .barriers
            .iter()
            .any(|b| matches!(&b.scope, BarrierScope::UserVariable { name } if name.0 == "@x"))
    );
    assert!(
        !facts(&into_units[0])
            .barriers
            .iter()
            .any(|b| matches!(b.scope, BarrierScope::Global))
    );

    let multi_into = parse("SELECT a, b INTO @x, @y FROM t1;", DialectKind::MySql).unwrap();
    assert!(multi_into[0].parse_error.is_none());
    assert_eq!(relation_reads(facts(&multi_into[0])), ["t1"]);
    assert_eq!(read_column_names(facts(&multi_into[0])), ["a", "b"]);
    assert!(
        !facts(&multi_into[0])
            .barriers
            .iter()
            .any(|b| matches!(b.scope, BarrierScope::Global))
    );

    for fenced_into in [
        "SELECT a INTO @x FROM (SELECT b INTO t2 FROM t1) s;",
        "SELECT a INTO @x FROM t1 WHERE EXISTS (SELECT b INTO t2 FROM t3);",
        "SELECT a INTO TEMPORARY @x FROM t1;",
    ] {
        let units = parse(fenced_into, DialectKind::MySql).unwrap();
        assert!(units[0].parse_error.is_none(), "{fenced_into}");
        assert!(
            facts(&units[0]).barriers.iter().any(|b| matches!(
                b,
                ScopedBarrier {
                    scope: BarrierScope::Global,
                    reason: BarrierReason::UnknownDml,
                    ..
                }
            )),
            "{fenced_into}: {:?}",
            facts(&units[0]).barriers
        );
    }

    let set_subquery = parse("SET @x = (SELECT a FROM t);", DialectKind::MySql).unwrap();
    assert!(set_subquery[0].parse_error.is_none());
    assert_eq!(relation_reads(facts(&set_subquery[0])), ["t"]);
    assert_eq!(read_column_names(facts(&set_subquery[0])), ["a"]);
    assert!(
        facts(&set_subquery[0])
            .barriers
            .iter()
            .any(|b| matches!(&b.scope, BarrierScope::UserVariable { name } if name.0 == "@x"))
    );
    assert!(
        !facts(&set_subquery[0])
            .barriers
            .iter()
            .any(|b| matches!(b.scope, BarrierScope::Global))
    );

    let set_unmodeled = parse(
        "SET @x = (SELECT a FROM unknown_tvf());",
        DialectKind::MySql,
    )
    .unwrap();
    assert!(set_unmodeled[0].parse_error.is_none());
    assert!(
        facts(&set_unmodeled[0])
            .barriers
            .iter()
            .any(|b| matches!(b.scope, BarrierScope::Global))
    );
}

#[test]
fn clickhouse_empty_tuple_order_by_lifts_routine_fence() {
    let units = parse(
        "CREATE TABLE t0 (c0 UInt64) ENGINE = MergeTree ORDER BY tuple();",
        DialectKind::ClickHouse,
    )
    .unwrap();
    assert!(units[0].parse_error.is_none());
    assert!(
        facts(&units[0]).barriers.is_empty(),
        "{:?}",
        facts(&units[0]).barriers
    );

    let select_units = parse("SELECT tuple();", DialectKind::ClickHouse).unwrap();
    assert!(select_units[0].parse_error.is_none());
    assert!(has_routine_barrier(facts(&select_units[0])));
}

-- Multi-dialect SQL regression corpus extracted from legacy parser unit tests
-- Format: -- @case: <id> / -- @dialects: <dialect> followed by exact SQL bytes until next -- @case: or EOF.
-- @case: rust:tests:statement_boundaries_and_source_mapping:1
-- @dialects: sqlite
-- café
SELECT 'x;y';
SELECT 2;
-- @case: rust:tests:column_qualifiers_require_a_structural_match:1
-- @dialects: sqlite
SELECT missing.id FROM actual;
-- @case: rust:tests:column_qualifiers_require_a_structural_match:2
-- @dialects: sqlite
SELECT t.id FROM public.t;
-- @case: rust:tests:column_qualifiers_require_a_structural_match:3
-- @dialects: sqlite
SELECT other.t.id FROM public.t;
-- @case: rust:tests:invalid_middle_unit_does_not_hide_later_statement:1
-- @dialects: sqlite
SELECT 1; SELECT FROM; SELECT 3
-- @case: rust:tests:parser_and_tokenizer_errors_are_unit_local:1
-- @dialects: sqlite
SELECT 1 FROM; SELECT 2;
-- @case: rust:tests:parser_and_tokenizer_errors_are_unit_local:2
-- @dialects: sqlite
SELECT 1; SELECT FROM
-- @case: rust:tests:parser_and_tokenizer_errors_are_unit_local:3
-- @dialects: sqlite
SELECT 1; SELECT 'unterminated
-- @case: rust:tests:parser_and_tokenizer_errors_are_unit_local:4
-- @dialects: sqlite
SELECT 1 FROM; SELECT 'unterminated
-- @case: rust:tests:missing_delimiter_coalesces_through_its_semicolon:1
-- @dialects: sqlite
SELECT 1 CREATE TABLE t (x INT); SELECT 2;
-- @case: rust:tests:edits_map_same_line_unicode_and_crlf_to_exact_bytes:1
-- @dialects: sqlite
SELECT 'é', 1 + 2;
SELECT 3;
-- @case: rust:tests:values_types_and_cte_edits_keep_ast_boundaries:1
-- @dialects: sqlite
INSERT INTO t(a,b) VALUES (1,2),(42,43);
-- @case: rust:tests:values_types_and_cte_edits_keep_ast_boundaries:2
-- @dialects: postgresql
CREATE TABLE t (id VARCHAR(255) NOT NULL); SELECT CAST(1 AS BIGINT);
-- @case: rust:tests:values_types_and_cte_edits_keep_ast_boundaries:3
-- @dialects: postgresql
WITH a AS (SELECT id FROM source), b AS (SELECT id FROM a) SELECT id FROM b;
-- @case: rust:tests:list_chunks_delete_contiguous_values_rows_before_singles:1
-- @dialects: sqlite
INSERT INTO t(a,b) VALUES (1,2),(3,4),(5,6),(42,43);
-- @case: rust:tests:list_chunks_carry_comments_and_preserve_unicode_crlf:1
-- @dialects: sqlite
-- café
SELECT * FROM t WHERE x IN (1, /* drop */ 2, 3);
-- @case: rust:tests:quoted_dots_and_trigger_drop_keep_canonical_actions:1
-- @dialects: postgresql
CREATE TABLE t ("a.b" INT); INSERT INTO t ("a.b") VALUES (1); ALTER TABLE "old.schema".t RENAME TO "new.name"; DROP TRIGGER tr ON t;
-- @case: rust:tests:clause_edits_use_full_clause_boundaries:1
-- @dialects: postgresql
WITH c AS (SELECT x FROM t) SELECT a FROM u LEFT JOIN v ON u.id = v.id WHERE a = 1 GROUP BY a, b ORDER BY b LIMIT 5 OFFSET 2;
-- @case: rust:tests:compound_trigger_keeps_internal_semicolons_in_one_unit:1
-- @dialects: sqlite
CREATE TRIGGER tr AFTER INSERT ON t BEGIN UPDATE t SET x = 'a;b'; INSERT INTO audit VALUES ('x;y'); END; SELECT 2;
-- @case: rust:tests:conflict_writes_nested_ctes_and_group_by_all_are_handled:1
-- @dialects: postgresql
WITH outer_cte AS (SELECT id FROM outer_src) SELECT id FROM (WITH outer_cte AS (SELECT id FROM inner_src) SELECT id FROM outer_cte) nested JOIN outer_cte ON nested.id = outer_cte.id;
-- @case: rust:tests:conflict_writes_nested_ctes_and_group_by_all_are_handled:2
-- @dialects: postgresql
SELECT * FROM generate_series(1, 2) AS n;
-- @case: rust:tests:conflict_writes_nested_ctes_and_group_by_all_are_handled:3
-- @dialects: postgresql
SELECT * FROM custom_func(1, 2) AS n;
-- @case: rust:tests:conflict_writes_nested_ctes_and_group_by_all_are_handled:4
-- @dialects: duckdb
SELECT a FROM t GROUP BY ALL;
-- @case: rust:tests:conflict_writes_nested_ctes_and_group_by_all_are_handled:5
-- @dialects: postgresql
INSERT INTO t (id) VALUES (1) ON CONFLICT (id) DO UPDATE SET extra = excluded.extra;
-- @case: rust:tests:conflict_writes_nested_ctes_and_group_by_all_are_handled:6
-- @dialects: mysql
INSERT INTO t (id) VALUES (1) ON DUPLICATE KEY UPDATE extra = VALUES(extra);
-- @case: rust:tests:upsert_conflict_reads_cover_arbiter_and_set_sources:1
-- @dialects: sqlite
INSERT INTO item_inventory (sku, item_name, quantity) VALUES ('SKU-001', 'Widget Alpha', 30) ON CONFLICT (sku) DO UPDATE SET quantity = item_inventory.quantity + EXCLUDED.quantity;
-- @case: rust:tests:upsert_conflict_reads_cover_arbiter_and_set_sources:2
-- @dialects: sqlite
INSERT INTO t (a) VALUES (1) ON CONFLICT (a) DO NOTHING;
-- @case: rust:tests:upsert_conflict_reads_cover_arbiter_and_set_sources:3
-- @dialects: sqlite
INSERT INTO t (a, b) VALUES (1, 2) ON CONFLICT (a) DO UPDATE SET b = 1 WHERE c > 0;
-- @case: rust:tests:upsert_conflict_reads_cover_arbiter_and_set_sources:4
-- @dialects: sqlite
INSERT INTO t (a) VALUES (1) ON CONFLICT DO NOTHING;
-- @case: rust:tests:upsert_conflict_reads_cover_arbiter_and_set_sources:5
-- @dialects: sqlite
INSERT INTO t (a) VALUES (1);
-- @case: rust:tests:upsert_conflict_reads_cover_arbiter_and_set_sources:6
-- @dialects: sqlite
INSERT OR IGNORE INTO t (a) VALUES (1);
-- @case: rust:tests:mysql_duplicate_key_reads_cover_keys_and_set_sources:1
-- @dialects: mysql
INSERT INTO t (id, val) VALUES (1, 2) ON DUPLICATE KEY UPDATE val = t.val + 1;
-- @case: rust:tests:mysql_duplicate_key_reads_cover_keys_and_set_sources:2
-- @dialects: mysql
INSERT INTO t (a, b) VALUES (1, 2) ON DUPLICATE KEY UPDATE a = VALUES(b);
-- @case: rust:tests:mysql_duplicate_key_reads_cover_keys_and_set_sources:3
-- @dialects: mysql
INSERT INTO t (a, b) VALUES (1, 2) ON DUPLICATE KEY UPDATE a = VALUES(b) + c;
-- @case: rust:tests:mysql_duplicate_key_reads_cover_keys_and_set_sources:4
-- @dialects: mysql
INSERT INTO t (a, b) VALUES (1, 2) AS new ON DUPLICATE KEY UPDATE a = new.b + c;
-- @case: rust:tests:mysql_duplicate_key_reads_cover_keys_and_set_sources:5
-- @dialects: mysql
INSERT INTO t (a) VALUES (1) ON DUPLICATE KEY UPDATE a = `VALUES`(b);
-- @case: rust:tests:mysql_duplicate_key_reads_cover_keys_and_set_sources:6
-- @dialects: mysql
CREATE FUNCTION values(x INT) RETURNS INT RETURN x; INSERT INTO t (a) VALUES (1) ON DUPLICATE KEY UPDATE a = VALUES(b);
-- @case: rust:tests:mysql_duplicate_key_reads_cover_keys_and_set_sources:7
-- @dialects: mysql
INSERT INTO t VALUES (1, 2) ON DUPLICATE KEY UPDATE a = b + 1;
-- @case: rust:tests:mysql_duplicate_key_reads_cover_keys_and_set_sources:8
-- @dialects: mysql
INSERT INTO t (a) VALUES (1);
-- @case: rust:tests:cte_production_cases_have_native_facts:1
-- @dialects: postgresql
CREATE TABLE base_a (id INT); CREATE TABLE base_b (id INT); CREATE TABLE base_c (id INT); WITH cte_union AS (SELECT id FROM base_a UNION ALL SELECT id FROM base_b) SELECT id FROM cte_union WHERE EXISTS (SELECT 1 FROM base_c WHERE base_c.id = cte_union.id);
-- @case: rust:tests:cte_production_cases_have_native_facts:2
-- @dialects: postgresql
CREATE TABLE base (id INT); CREATE TABLE inside_cte (id INT); CREATE TABLE nested_cte (id INT); WITH base AS (SELECT id FROM inside_cte) SELECT id FROM base WHERE EXISTS (WITH base AS (SELECT id FROM nested_cte) SELECT id FROM base);
-- @case: rust:tests:cte_production_cases_have_native_facts:3
-- @dialects: postgresql
CREATE TABLE t (id INT); WITH t AS (SELECT id FROM t) SELECT id FROM t;
-- @case: rust:tests:cte_production_cases_have_native_facts:4
-- @dialects: postgresql
CREATE TABLE "a.b" (id INT); WITH "a.b" AS (SELECT 42 AS id) SELECT id FROM "a.b";
-- @case: rust:tests:cte_production_cases_have_native_facts:5
-- @dialects: sqlite
-- café
WITH unused AS (SELECT 1), kept AS (SELECT 42 AS value) SELECT value FROM kept;
-- @case: rust:tests:create_table_foreign_keys_emit_parent_uses:1
-- @dialects: sqlite
CREATE TABLE parent (id INT PRIMARY KEY); CREATE TABLE child (parent_id INT REFERENCES parent(id)); CREATE TABLE child2 (parent_id INT, FOREIGN KEY (parent_id) REFERENCES parent(id));
-- @case: rust:tests:cascading_foreign_keys_and_on_commit_actions:1
-- @dialects: postgresql
CREATE TABLE child (p_id INT REFERENCES parent(id) ON DELETE CASCADE); CREATE TEMPORARY TABLE temp_tbl (x INT) ON COMMIT DROP;
-- @case: rust:tests:clause_and_scalar_edits_cover_parentheses:1
-- @dialects: postgresql
SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id WHERE (col_timestamp_2 IS NOT NULL AND col_timestamp_1 IS NOT NULL);
-- @case: rust:tests:clause_and_scalar_edits_cover_parentheses:2
-- @dialects: postgresql
SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id WHERE ((col_numeric_1 + 1.0) > 0.0 AND 1 = 1);
-- @case: rust:tests:clause_and_scalar_edits_cover_parentheses:3
-- @dialects: sqlite
-- café
SELECT (SELECT 42);
-- @case: rust:tests:expression_edits_cover_partial_ast_spans_without_malformed_candidates:1
-- @dialects: sqlite
SELECT id, payload FROM target_json WHERE ((((payload = '{"trigger": "crash_segv"}') OR FALSE) AND (NULLIF(1, 0) IS NOT NULL)) AND (1 = 1)) AND (1 = 1) LIMIT 9999;
-- @case: rust:tests:in_subquery_expression_edits_keep_closing_parentheses:1
-- @dialects: sqlite
SELECT * FROM main_tasks WHERE ((((task_id IN (SELECT task_id FROM sub_items WHERE status = 'crashed_worker')) AND NOT FALSE) OR FALSE) AND NOT FALSE) AND NOT FALSE ORDER BY 1 LIMIT 9999;
-- @case: rust:tests:postgres_projection_deletion_keeps_cast_and_dollar_quote_boundaries:1
-- @dialects: postgresql
SELECT (1 + 2)::int AS value, $$crash;marker$$::text AS marker;
-- @case: rust:tests:final_statement_edits_stop_before_following_comments:1
-- @dialects: sqlite
SELECT sensor_id, avg(reading) FROM metric_series WHERE (((((1 = 1) AND ((1 = 1) AND ((1 = 1) AND (sensor_id = 1)))) AND NOT FALSE) AND (NULLIF(1, 0) IS NOT NULL)) AND (1 = 1)) AND (NULLIF(1, 0) IS NOT NULL) GROUP BY sensor_id ORDER BY 1;
/* fuzz_mutation_noise */
-- @case: rust:tests:window_function_candidates_do_not_cut_off_over:1
-- @dialects: postgresql
SELECT 1 FROM t WHERE 1 < sum(x) OVER (PARTITION BY y) AND TRUE;
-- @case: rust:tests:duckdb_dialect_parsing_and_statement_boundaries:1
-- @dialects: duckdb
SELECT range(5);
SELECT {'a': 1, 'b': 2};
CREATE TABLE t ("QuotedCol" INT, unquoted_col INT);
-- @case: rust:tests:clickhouse_dialect_parsing_and_statement_boundaries:1
-- @dialects: clickhouse
SELECT number FROM numbers(10);
CREATE TABLE events (EventId UInt64, event_name String) ENGINE = MergeTree ORDER BY EventId;
-- @case: rust:tests:identifier_case_folding_follows_dialect_semantics:1
-- @dialects: clickhouse
CREATE TABLE Dept (DeptName Int32, "QuotedName" Int32) ENGINE = Memory;
-- @case: rust:tests:identifier_case_folding_follows_dialect_semantics:2
-- @dialects: postgresql
CREATE TABLE Dept (DeptName INT, "QuotedName" INT);
-- @case: rust:tests:mysql_drop_index_records_its_table_use:1
-- @dialects: mysql
DROP INDEX ix ON t;
-- @case: rust:tests:constraint_stripping_and_deeper_ast_pruning:1
-- @dialects: postgresql
CREATE TABLE t (id INT NOT NULL, name TEXT DEFAULT 'abc', PRIMARY KEY (id), FOREIGN KEY (id) REFERENCES p(id));
-- @case: rust:tests:constraint_stripping_and_deeper_ast_pruning:2
-- @dialects: postgresql
SELECT CASE WHEN a = 1 THEN 42 ELSE 0 END, COALESCE(x, y), z IS NOT NULL, b BETWEEN 1 AND 10 FROM t;
-- @case: rust:tests:search_path_default_and_current_user_stay_symbolic:1
-- @dialects: postgresql
SET search_path TO DEFAULT;
-- @case: rust:tests:search_path_default_and_current_user_stay_symbolic:2
-- @dialects: postgresql
SET search_path TO "$user", public;
-- @case: rust:tests:search_path_default_and_current_user_stay_symbolic:3
-- @dialects: postgresql
SET search_path = 'tenant, "Mixed Case"';
-- @case: rust:tests:search_path_default_and_current_user_stay_symbolic:4
-- @dialects: postgresql
SET search_path = '"" , public';
-- @case: rust:tests:search_path_default_and_current_user_stay_symbolic:5
-- @dialects: postgresql
SET search_path TO $user, public;
-- @case: rust:tests:role_and_session_authorization_are_distinct_session_effects:1
-- @dialects: postgresql
SET ROLE analyst; RESET ROLE; SET SESSION AUTHORIZATION admin; RESET SESSION AUTHORIZATION;
-- @case: rust:tests:mysql_schema_and_database_drops_always_cascade:1
-- @dialects: mysql
DROP SCHEMA db;
-- @case: rust:tests:mysql_schema_and_database_drops_always_cascade:2
-- @dialects: mysql
DROP DATABASE db;
-- @case: rust:tests:qualified_columns_resolve_through_table_aliases:1
-- @dialects: sqlite
SELECT x.id FROM actual AS x;
-- @case: rust:tests:qualified_columns_resolve_through_table_aliases:2
-- @dialects: sqlite
SELECT other.id FROM actual AS x;
-- @case: rust:tests:named_table_constraints_are_removed_with_their_prefix:1
-- @dialects: postgresql
CREATE TABLE t (id INT, CONSTRAINT pk PRIMARY KEY (id), CONSTRAINT fk FOREIGN KEY (id) REFERENCES p(id), name TEXT);
-- @case: rust:tests:explain_reads_schema_metadata_unless_analyze_executes:1
-- @dialects: postgresql
EXPLAIN SELECT * FROM t;
-- @case: rust:tests:explain_reads_schema_metadata_unless_analyze_executes:2
-- @dialects: postgresql
EXPLAIN ANALYZE SELECT * FROM t;
-- @case: rust:tests:explain_reads_schema_metadata_unless_analyze_executes:3
-- @dialects: postgresql
EXPLAIN INSERT INTO t VALUES (1);
-- @case: rust:tests:explain_reads_schema_metadata_unless_analyze_executes:4
-- @dialects: postgresql
EXPLAIN WITH d AS (DELETE FROM t RETURNING *) SELECT * FROM d;
-- @case: rust:tests:explain_reads_schema_metadata_unless_analyze_executes:5
-- @dialects: postgresql
EXPLAIN ANALYZE WITH d AS (DELETE FROM t RETURNING *) SELECT * FROM d;
-- @case: rust:tests:sqlite_foreign_keys_accepts_boolean_keywords_with_original_spans:1
-- @dialects: sqlite
PRAGMA foreign_keys = ON
-- @case: rust:tests:sqlite_foreign_keys_accepts_boolean_keywords_with_original_spans:2
-- @dialects: sqlite
PRAGMA main.foreign_keys(Off)
-- @case: rust:tests:sqlite_foreign_keys_accepts_boolean_keywords_with_original_spans:3
-- @dialects: sqlite
PRAGMA "foreign_keys" /* 外鍵 */ = YeS
-- @case: rust:tests:sqlite_foreign_keys_accepts_boolean_keywords_with_original_spans:4
-- @dialects: sqlite
PRAGMA foreign_keys(NO)
-- @case: rust:tests:sqlite_foreign_keys_accepts_boolean_keywords_with_original_spans:5
-- @dialects: sqlite
PRAGMA foreign_keys = TRUE
-- @case: rust:tests:sqlite_foreign_keys_accepts_boolean_keywords_with_original_spans:6
-- @dialects: sqlite
PRAGMA foreign_keys(FALSE)
-- @case: rust:tests:sqlite_foreign_keys_accepts_boolean_keywords_with_original_spans:7
-- @dialects: sqlite
PRAGMA foreign_keys = 'yes'
-- @case: rust:tests:sqlite_foreign_keys_accepts_boolean_keywords_with_original_spans:8
-- @dialects: sqlite
PRAGMA foreign_keys('no')
-- @case: rust:tests:pragma_foreign_keys_only_changes_state_on_assignment:1
-- @dialects: sqlite
PRAGMA foreign_keys;
-- @case: rust:tests:pragma_foreign_keys_only_changes_state_on_assignment:2
-- @dialects: sqlite
PRAGMA foreign_keys = 0;
-- @case: rust:tests:pragma_foreign_keys_only_changes_state_on_assignment:3
-- @dialects: sqlite
PRAGMA foreign_key_checks = 1;
-- @case: rust:tests:pragma_foreign_keys_only_changes_state_on_assignment:4
-- @dialects: sqlite
PRAGMA foreign_keys = 'maybe';
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:1
-- @dialects: sqlite
PRAGMA foreign_keys = 1;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:2
-- @dialects: postgresql
SET search_path TO public;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:3
-- @dialects: sqlite
PRAGMA recursive_triggers = 0;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:4
-- @dialects: sqlite
PRAGMA writable_schema = 1;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:5
-- @dialects: postgresql
SET session_replication_role = replica;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:6
-- @dialects: postgresql
RESET session_replication_role;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:7
-- @dialects: postgresql
RESET ALL;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:8
-- @dialects: postgresql
SET search_path TO 42;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:9
-- @dialects: sqlite
PRAGMA foreign_keys = ?;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:10
-- @dialects: postgresql
SET TRANSACTION SNAPSHOT 'snapshot';
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:11
-- @dialects: postgresql
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
-- @case: rust:tests:unmodeled_state_changes_preserve_global_uncertainty:12
-- @dialects: postgresql
BEGIN ISOLATION LEVEL SERIALIZABLE;
-- @case: rust:tests:using_index_and_rename_carry_index_schema_placement:1
-- @dialects: postgresql
CREATE TABLE s.t (a INT); ALTER TABLE s.t ADD PRIMARY KEY USING INDEX t_idx;
-- @case: rust:tests:using_index_and_rename_carry_index_schema_placement:2
-- @dialects: postgresql
CREATE TABLE s.t (a INT); ALTER TABLE s.t RENAME TO u;
-- @case: rust:tests:reset_foreign_keys_restores_the_initial_setting:1
-- @dialects: mysql
SET foreign_key_checks = 0; RESET foreign_key_checks; SET @@foreign_key_checks = 1; SET @@SESSION.foreign_key_checks = 0;
-- @case: rust:tests:copy_statement_records_read_and_write_facts:1
-- @dialects: postgresql
COPY t (a, b) FROM '/path/to/data.csv'; COPY t (a, b) TO '/path/to/out.csv'; COPY (SELECT a FROM t) TO STDOUT;
-- @case: rust:tests:clickhouse_ephemeral_columns_are_not_generated_or_defaulted:1
-- @dialects: clickhouse
CREATE TABLE t (a Int32 EPHEMERAL, b Int32 DEFAULT 1) ENGINE = Memory;
-- @case: rust:tests:change_column_only_renames_when_the_name_changes:1
-- @dialects: mysql
ALTER TABLE t CHANGE COLUMN a a INT;
-- @case: rust:tests:change_column_only_renames_when_the_name_changes:2
-- @dialects: mysql
ALTER TABLE t CHANGE COLUMN a b INT;
-- @case: rust:tests:end_commits_a_transaction:1
-- @dialects: postgresql
BEGIN; END;
-- @case: rust:tests:multi_table_update_writes_every_assigned_target:1
-- @dialects: mysql
UPDATE t1 JOIN t2 ON t1.id = t2.id SET t1.a = 1, t2.b = 2;
-- @case: rust:tests:create_view_records_projection_columns_and_definition_reads:1
-- @dialects: postgresql
CREATE VIEW v AS SELECT a, b AS c FROM t;
-- @case: rust:tests:create_view_records_projection_columns_and_definition_reads:2
-- @dialects: postgresql
CREATE MATERIALIZED VIEW mv AS SELECT a FROM t;
-- @case: rust:tests:view_column_lineage_and_condition_columns:1
-- @dialects: postgresql
CREATE VIEW v AS SELECT t1.a AS col_a, t2.b AS col_b FROM t1 JOIN t2 ON t1.id = t2.id WHERE t1.active = 1;
-- @case: rust:tests:view_column_lineage_and_condition_columns:2
-- @dialects: postgresql
CREATE VIEW v_star AS SELECT * FROM t;
-- @case: rust:tests:sibling_subquery_aliases_do_not_leak_columns:1
-- @dialects: postgresql
SELECT (SELECT x.id FROM a AS x), (SELECT x.name FROM b AS x) FROM c;
-- @case: rust:tests:correlated_aliases_remain_visible_from_nested_queries:1
-- @dialects: postgresql
SELECT (SELECT x.id FROM a) FROM b AS x;
-- @case: rust:tests:literal_search_path_splits_commas_and_preserves_quotes:1
-- @dialects: postgresql
SET search_path = 'tenant, "MixedCase", $user';
-- @case: rust:tests:literal_search_path_splits_commas_and_preserves_quotes:2
-- @dialects: postgresql
SET search_path = 'a b, c';
-- @case: rust:tests:literal_search_path_splits_commas_and_preserves_quotes:3
-- @dialects: postgresql
SET search_path = '"a" "b", c';
-- @case: rust:tests:literal_search_path_splits_commas_and_preserves_quotes:4
-- @dialects: postgresql
SET search_path = 'a"b", c';
-- @case: rust:tests:literal_search_path_splits_commas_and_preserves_quotes:5
-- @dialects: postgresql
SET search_path = '"unterminated, c';
-- @case: rust:tests:sequence_literals_use_identifier_parsing:1
-- @dialects: postgresql
SELECT pg_catalog.nextval('tenant."MixedSeq"');
-- @case: rust:tests:sequence_literals_use_identifier_parsing:2
-- @dialects: postgresql
SELECT pg_catalog.nextval('a"b"c');
-- @case: rust:tests:sequence_literals_use_identifier_parsing:3
-- @dialects: postgresql
SELECT pg_catalog.nextval('foo bar.seq');
-- @case: rust:tests:sequence_literals_use_identifier_parsing:4
-- @dialects: postgresql
SELECT pg_catalog.nextval('""');
-- @case: rust:tests:set_operation_output_columns_require_matching_arity:1
-- @dialects: postgresql
CREATE VIEW v AS SELECT a FROM t UNION SELECT b FROM u;
-- @case: rust:tests:set_operation_output_columns_require_matching_arity:2
-- @dialects: postgresql
CREATE VIEW v AS SELECT a FROM t UNION SELECT b, c FROM u;
-- @case: rust:tests:implicit_commit_is_limited_to_mysql:1
-- @dialects: mysql
LOCK TABLES t WRITE;
-- @case: rust:tests:implicit_commit_is_limited_to_mysql:2
-- @dialects: postgresql
CREATE TABLE t (id INT);
-- @case: rust:tests:implicit_commit_covers_grants_and_routines:1
-- @dialects: mysql
GRANT SELECT ON t TO u;
-- @case: rust:tests:implicit_commit_covers_grants_and_routines:2
-- @dialects: mysql
CREATE PROCEDURE p() AS BEGIN SELECT 1; END;
-- @case: rust:tests:set_operation_branches_keep_separate_alias_scopes:1
-- @dialects: postgresql
SELECT x.id FROM a AS x UNION ALL SELECT x.name FROM b AS x;
-- @case: rust:tests:aliases_hide_the_original_relation_name:1
-- @dialects: postgresql
SELECT t.a FROM tbl AS x;
-- @case: rust:tests:derived_table_visibility_follows_lateral:1
-- @dialects: postgresql
SELECT d.x FROM a, (SELECT a.id AS x) AS d;
-- @case: rust:tests:derived_table_visibility_follows_lateral:2
-- @dialects: postgresql
SELECT d.x FROM a, LATERAL (SELECT a.id AS x) AS d;
-- @case: rust:tests:update_reads_target_rows:1
-- @dialects: postgresql
UPDATE t SET a = 1 WHERE b = 2;
-- @case: rust:tests:delete_reads_target_rows:1
-- @dialects: postgresql
DELETE FROM t WHERE b = 2;
-- @case: rust:tests:merge_reads_target_rows:1
-- @dialects: postgresql
MERGE INTO t USING s ON t.id = s.id WHEN MATCHED THEN UPDATE SET a = s.a;
-- @case: rust:tests:delete_alias_resolves_to_physical_table:1
-- @dialects: mysql
DELETE a FROM actual_table AS a WHERE a.id = 2;
-- @case: rust:tests:ctas_and_partial_projections_keep_known_columns:1
-- @dialects: postgresql
CREATE TABLE copy AS SELECT a, b AS c FROM t;
-- @case: rust:tests:ctas_and_partial_projections_keep_known_columns:2
-- @dialects: postgresql
CREATE VIEW v AS SELECT a, count(*) AS n FROM t;
-- @case: rust:tests:nextval_writes_sequence_state:1
-- @dialects: postgresql
SELECT pg_catalog.nextval('s');
-- @case: rust:tests:nextval_writes_sequence_state:2
-- @dialects: postgresql
SELECT pg_catalog.nextval('s'::regclass);
-- @case: rust:tests:nextval_writes_sequence_state:3
-- @dialects: postgresql
SELECT pg_catalog.nextval(CAST(('s') AS TEXT));
-- @case: rust:tests:nextval_writes_sequence_state:4
-- @dialects: postgresql
SELECT pg_catalog.setval('s', 42);
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:1
-- @dialects: postgresql
SELECT pg_catalog.currval('s'::regclass);
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:2
-- @dialects: postgresql
SELECT pg_catalog.nextval(seq_name) FROM seqs;
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:3
-- @dialects: postgresql
SELECT pg_catalog.nextval(seqs.seq_name) FROM seqs;
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:4
-- @dialects: postgresql
SELECT pg_catalog.nextval(seq_name::regclass) FROM seqs;
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:5
-- @dialects: postgresql
SELECT pg_catalog.setval(seq_name::regclass, 42) FROM seqs;
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:6
-- @dialects: postgresql
SELECT pg_catalog.currval(seq_name) FROM seqs;
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:7
-- @dialects: postgresql
SELECT pg_catalog.lastval();
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:8
-- @dialects: postgresql
SELECT pg_catalog.nextval('long_seq'::varchar(1));
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:9
-- @dialects: postgresql
SELECT pg_catalog.nextval('s'::custom_type);
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:10
-- @dialects: postgresql
SELECT nextval('s');
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:11
-- @dialects: postgresql
SELECT currval('s');
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:12
-- @dialects: postgresql
SELECT setval('s', 42);
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:13
-- @dialects: postgresql
SELECT tenant.nextval('s');
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:14
-- @dialects: postgresql
SELECT "NEXTVAL"('s');
-- @case: rust:tests:sequence_state_reads_and_unknown_calls_are_preserved:15
-- @dialects: postgresql
SELECT mutate_t();
-- @case: rust:tests:deferred_functions_are_opaque_but_plain_explain_does_not_execute_them:1
-- @dialects: postgresql
EXPLAIN SELECT mutate_t();
-- @case: rust:tests:deferred_functions_are_opaque_but_plain_explain_does_not_execute_them:2
-- @dialects: postgresql
EXPLAIN ANALYZE SELECT mutate_t();
-- @case: rust:tests:deferred_functions_are_opaque_but_plain_explain_does_not_execute_them:3
-- @dialects: postgresql
CREATE TABLE t (id BIGINT DEFAULT pg_catalog.nextval('s'));
-- @case: rust:tests:deferred_functions_are_opaque_but_plain_explain_does_not_execute_them:4
-- @dialects: postgresql
CREATE VIEW v AS SELECT pg_catalog.nextval('s');
-- @case: rust:tests:deferred_functions_are_opaque_but_plain_explain_does_not_execute_them:5
-- @dialects: postgresql
CREATE VIEW v AS SELECT mutate_t();
-- @case: rust:tests:deferred_functions_are_opaque_but_plain_explain_does_not_execute_them:6
-- @dialects: postgresql
ALTER TABLE t ALTER COLUMN id SET DEFAULT pg_catalog.nextval('s');
-- @case: rust:tests:sequence_state_advances_only_for_evaluated_ddl:1
-- @dialects: postgresql
CREATE MATERIALIZED VIEW mv AS SELECT pg_catalog.nextval('s');
-- @case: rust:tests:sequence_state_advances_only_for_evaluated_ddl:2
-- @dialects: postgresql
CREATE TABLE t AS SELECT pg_catalog.nextval('s');
-- @case: rust:tests:sequence_state_advances_only_for_evaluated_ddl:3
-- @dialects: postgresql
ALTER TABLE t ADD COLUMN c INT DEFAULT pg_catalog.nextval('s');
-- @case: rust:tests:sequence_state_advances_only_for_evaluated_ddl:5
-- @dialects: postgresql
CREATE TABLE t AS SELECT pg_catalog.nextval('s') WITH NO DATA;
-- @case: rust:tests:sequence_state_advances_only_for_evaluated_ddl:6
-- @dialects: postgresql
ALTER TABLE t ALTER COLUMN c SET DEFAULT pg_catalog.nextval('s');
-- @case: rust:tests:wildcard_ctas_does_not_record_partial_columns:1
-- @dialects: sqlite
CREATE TABLE t (a, b) AS SELECT 1, 2;
-- @case: rust:tests:wildcard_ctas_does_not_record_partial_columns:2
-- @dialects: postgresql
CREATE TABLE t AS SELECT * FROM src;
-- @case: rust:tests:wildcard_ctas_does_not_record_partial_columns:3
-- @dialects: postgresql
CREATE TABLE t AS SELECT *, 1 AS extra FROM src;
-- @case: rust:tests:nested_dml_relations_are_attributed_only_to_the_nested_write:1
-- @dialects: postgresql
WITH u AS (UPDATE t SET a = 1 FROM other WHERE t.id = other.id RETURNING *) SELECT * FROM u;
-- @case: rust:tests:nested_dml_relations_are_attributed_only_to_the_nested_write:2
-- @dialects: postgresql
WITH m AS (MERGE INTO t USING other ON t.id = other.id WHEN MATCHED THEN UPDATE SET a = 1 RETURNING *) SELECT * FROM m;
-- @case: rust:tests:create_view_applies_nested_dml_only_when_materialized:1
-- @dialects: postgresql
CREATE MATERIALIZED VIEW mv AS WITH d AS (DELETE FROM t RETURNING *) SELECT * FROM d;
-- @case: rust:tests:create_view_applies_nested_dml_only_when_materialized:2
-- @dialects: postgresql
CREATE VIEW v AS WITH d AS (DELETE FROM t RETURNING *) SELECT * FROM d;
-- @case: rust:tests:nested_data_modifying_ctes_are_analyzed_once:1
-- @dialects: postgresql
WITH outer_cte AS (WITH inner_cte AS (INSERT INTO t VALUES (1) RETURNING id) INSERT INTO t VALUES (2) RETURNING id) INSERT INTO t VALUES (3);
-- @case: rust:tests:postgres_analyze_table_extracts_relation_and_columns:1
-- @dialects: postgresql
ANALYZE t0 (c0, c2);
-- @case: rust:tests:postgres_analyze_table_extracts_relation_and_columns:2
-- @dialects: postgresql
ANALYZE;
-- @case: rust:tests:syntax_edit_gate_drops_only_edits:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2), (3, 4);
SELECT a FROM t ORDER BY a;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:1
-- @dialects: sqlite
SELECT a, sum(b) FROM t GROUP BY a;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:2
-- @dialects: postgresql
SELECT a, sum(b) FROM t GROUP BY a;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:3
-- @dialects: mysql
SELECT a, sum(b) FROM t GROUP BY a;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:4
-- @dialects: postgresql
SELECT "sum"(a) FROM t;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:5
-- @dialects: postgresql
SELECT "Sum"(a) FROM t;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:6
-- @dialects: sqlite
SELECT count(*) FROM t;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:7
-- @dialects: postgresql
SELECT count(*) FROM t;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:8
-- @dialects: mysql
SELECT count(*) FROM t;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:9
-- @dialects: sqlite
SELECT COUNT(*) FROM t;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:10
-- @dialects: postgresql
SELECT COUNT(*) FROM t;
-- @case: rust:tests:former_per_dialect_aggregates_stay_fenced:11
-- @dialects: mysql
SELECT COUNT(*) FROM t;
-- @case: rust:tests:function_argument_reads_are_extracted_for_fenced_calls:1
-- @dialects: sqlite
SELECT a, frobnicate(b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:1
-- @dialects: sqlite
SELECT frobnicate(a) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:2
-- @dialects: postgresql
SELECT frobnicate(a) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:3
-- @dialects: mysql
SELECT frobnicate(a) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:4
-- @dialects: sqlite
SELECT abs(a) OVER (PARTITION BY b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:5
-- @dialects: postgresql
SELECT abs(a) OVER (PARTITION BY b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:6
-- @dialects: mysql
SELECT abs(a) OVER (PARTITION BY b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:7
-- @dialects: sqlite
SELECT sum(a) OVER (PARTITION BY b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:8
-- @dialects: postgresql
SELECT sum(a) OVER (PARTITION BY b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:9
-- @dialects: mysql
SELECT sum(a) OVER (PARTITION BY b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:10
-- @dialects: sqlite
SELECT sum(DISTINCT a) OVER (PARTITION BY b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:11
-- @dialects: postgresql
SELECT sum(DISTINCT a) OVER (PARTITION BY b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:12
-- @dialects: mysql
SELECT sum(DISTINCT a) OVER (PARTITION BY b) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:13
-- @dialects: postgresql
SELECT pg_catalog.abs(a) FROM t;
-- @case: rust:tests:unknown_and_shaped_calls_still_fence:14
-- @dialects: postgresql
SELECT pg_catalog.sum(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:1
-- @dialects: sqlite
SELECT stddev(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:2
-- @dialects: postgresql
SELECT stddev(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:3
-- @dialects: mysql
SELECT stddev(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:4
-- @dialects: sqlite
SELECT stddev_pop(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:5
-- @dialects: postgresql
SELECT stddev_pop(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:6
-- @dialects: mysql
SELECT stddev_pop(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:7
-- @dialects: sqlite
SELECT variance(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:8
-- @dialects: postgresql
SELECT variance(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:9
-- @dialects: mysql
SELECT variance(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:10
-- @dialects: sqlite
SELECT var_samp(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:11
-- @dialects: postgresql
SELECT var_samp(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:12
-- @dialects: mysql
SELECT var_samp(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:13
-- @dialects: sqlite
SELECT group_concat(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:14
-- @dialects: postgresql
SELECT group_concat(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:15
-- @dialects: mysql
SELECT group_concat(a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:16
-- @dialects: sqlite
SELECT string_agg(a, ',') FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:17
-- @dialects: postgresql
SELECT string_agg(a, ',') FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:18
-- @dialects: mysql
SELECT string_agg(a, ',') FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:19
-- @dialects: sqlite
SELECT row_number() OVER (ORDER BY a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:20
-- @dialects: postgresql
SELECT row_number() OVER (ORDER BY a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:21
-- @dialects: mysql
SELECT row_number() OVER (ORDER BY a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:22
-- @dialects: sqlite
SELECT rank() OVER (ORDER BY a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:23
-- @dialects: postgresql
SELECT rank() OVER (ORDER BY a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:24
-- @dialects: mysql
SELECT rank() OVER (ORDER BY a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:25
-- @dialects: sqlite
SELECT lag(a) OVER (ORDER BY b) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:26
-- @dialects: postgresql
SELECT lag(a) OVER (ORDER BY b) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:27
-- @dialects: mysql
SELECT lag(a) OVER (ORDER BY b) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:28
-- @dialects: sqlite
SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:29
-- @dialects: postgresql
SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:30
-- @dialects: mysql
SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:31
-- @dialects: sqlite
SELECT sum(a) WITHIN GROUP (ORDER BY b) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:32
-- @dialects: postgresql
SELECT sum(a) WITHIN GROUP (ORDER BY b) FROM t;
-- @case: rust:tests:unproven_aggregate_and_window_shapes_still_fence:33
-- @dialects: mysql
SELECT sum(a) WITHIN GROUP (ORDER BY b) FROM t;
-- @case: rust:tests:filter_call_shapes_stay_fenced_with_condition_reads:1
-- @dialects: postgresql
SELECT sum(a) FILTER (WHERE b > 0) OVER (PARTITION BY c) FROM t;
-- @case: rust:tests:filter_call_shapes_stay_fenced_with_condition_reads:2
-- @dialects: postgresql
SELECT abs(a) FILTER (WHERE b > 0) FROM t;
-- @case: rust:tests:filter_call_shapes_stay_fenced_with_condition_reads:3
-- @dialects: sqlite
SELECT abs(a) FILTER (WHERE b > 0) FROM t;
-- @case: rust:tests:filter_call_shapes_stay_fenced_with_condition_reads:4
-- @dialects: postgresql
SELECT coalesce(a, b) FILTER (WHERE c > 0) FROM t;
-- @case: rust:tests:filter_and_within_group_condition_columns_are_extracted_while_fenced:4
-- @dialects: sqlite
SELECT count(*) FILTER (WHERE b > 0) FROM t;
-- @case: rust:tests:unproved_functions_fence_conservatively_across_all_dialects:4
-- @dialects: duckdb
SELECT count(*) FROM t;
-- @case: rust:tests:unproved_functions_fence_conservatively_across_all_dialects:5
-- @dialects: clickhouse
SELECT count(*) FROM t;
-- @case: rust:tests:former_per_dialect_case_folded_names_stay_fenced:1
-- @dialects: mysql
SELECT `ABS`(a) FROM t;
-- @case: rust:tests:former_per_dialect_case_folded_names_stay_fenced:2
-- @dialects: postgresql
SELECT ABS(a) FROM t;
-- @case: rust:tests:former_per_dialect_case_folded_names_stay_fenced:3
-- @dialects: postgresql
SELECT "abs"(a) FROM t;
-- @case: rust:tests:former_per_dialect_case_folded_names_stay_fenced:4
-- @dialects: postgresql
SELECT "Abs"(a) FROM t;
-- @case: rust:tests:former_per_dialect_case_folded_names_stay_fenced:5
-- @dialects: postgresql
SELECT "ABS"(a) FROM t;
-- @case: rust:tests:former_per_dialect_case_folded_names_stay_fenced:6
-- @dialects: sqlite
SELECT "ABS"(a) FROM t;
-- @case: rust:tests:former_per_dialect_case_folded_names_stay_fenced:7
-- @dialects: sqlite
SELECT "Abs"(a) FROM t;
-- @case: rust:tests:shadowed_routines_and_parse_errors_still_fence:1
-- @dialects: postgresql
DROP FUNCTION abs; SELECT abs(a) FROM t;
-- @case: rust:tests:shadowed_routines_and_parse_errors_still_fence:2
-- @dialects: postgresql
DROP FUNCTION sum; SELECT sum(a) FROM t;
-- @case: rust:tests:shadowed_routines_and_parse_errors_still_fence:3
-- @dialects: duckdb
CREATE MACRO count(a) AS a; SELECT count(*) FROM t;
-- @case: rust:tests:shadowed_routines_and_parse_errors_still_fence:4
-- @dialects: sqlite
SELECT count(*) FROM t; SELECT FROM;
-- @case: rust:tests:stored_calls_fence_conservatively:1
-- @dialects: sqlite
CREATE TABLE t (x INT DEFAULT abs(5));
-- @case: rust:tests:stored_calls_fence_conservatively:2
-- @dialects: sqlite
CREATE TABLE t (x INT DEFAULT coalesce(1, 2));
-- @case: rust:tests:stored_calls_fence_conservatively:3
-- @dialects: postgresql
CREATE TABLE t (x INT DEFAULT coalesce(1, 2));
-- @case: rust:tests:stored_calls_fence_conservatively:4
-- @dialects: postgresql
CREATE VIEW v AS SELECT coalesce(a, b) FROM t;
-- @case: rust:tests:stored_calls_fence_conservatively:5
-- @dialects: mysql
CREATE TABLE t (x INT DEFAULT abs(5));
-- @case: rust:tests:stored_calls_fence_conservatively:6
-- @dialects: postgresql
CREATE VIEW v AS SELECT abs(a) FROM t;
-- @case: rust:tests:stored_calls_fence_conservatively:7
-- @dialects: postgresql
CREATE VIEW v AS SELECT sum(a) FROM t;
-- @case: rust:tests:stored_calls_fence_conservatively:8
-- @dialects: sqlite
CREATE TABLE t (x TIMESTAMP DEFAULT CURRENT_TIMESTAMP);
-- @case: rust:tests:stored_calls_fence_conservatively:9
-- @dialects: postgresql
CREATE TABLE t (x TIMESTAMP DEFAULT CURRENT_TIMESTAMP);
-- @case: rust:tests:stored_calls_fence_conservatively:10
-- @dialects: mysql
CREATE TABLE t (x TIMESTAMP DEFAULT CURRENT_TIMESTAMP);
-- @case: rust:tests:stored_calls_fence_conservatively:11
-- @dialects: postgresql
CREATE TABLE t (x INT DEFAULT random());
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:1
-- @dialects: mysql
CREATE TEMPORARY TABLE t1(i INT, b BLOB, PRIMARY KEY pk(i), INDEX sk(b(1)));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:2
-- @dialects: mysql
CREATE INDEX sk ON t (b(1));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:3
-- @dialects: postgresql
CREATE INDEX sk ON t (b(1));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:4
-- @dialects: mysql
CREATE TABLE t (b BLOB, UNIQUE KEY uk (b(1)));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:5
-- @dialects: mysql
CREATE TABLE t (b BLOB, PRIMARY KEY (b(10)));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:6
-- @dialects: mysql
CREATE INDEX sk ON t (`b`(1));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:7
-- @dialects: mysql
ALTER TABLE t ADD INDEX sk (b(1));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:8
-- @dialects: mysql
CREATE INDEX sk ON t ((lower(b)));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:9
-- @dialects: mysql
CREATE INDEX sk ON t ((f(b)));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:10
-- @dialects: mysql
CREATE INDEX sk ON t (f(b));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:11
-- @dialects: mysql
CREATE INDEX sk ON t (f(1, 2));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:12
-- @dialects: mysql
CREATE INDEX sk ON t (b(1.5));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:13
-- @dialects: mysql
CREATE TABLE t (a INT, b BLOB, INDEX sk (f(b)));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:14
-- @dialects: mysql
CREATE TABLE t (x INT DEFAULT f(1));
-- @case: rust:tests:mysql_index_prefix_lifts_but_real_index_calls_fence:15
-- @dialects: mysql
CREATE TABLE t (a INT, CHECK (f(a) > 0));
-- @case: rust:tests:partition_by_scheme_lifts_but_partition_of_binds_parent:1
-- @dialects: postgresql
CREATE TABLE c PARTITION OF p FOR VALUES FROM (1) TO (10);
-- @case: rust:tests:partition_by_scheme_lifts_but_partition_of_binds_parent:2
-- @dialects: postgresql
CREATE TABLE c PARTITION OF p FOR VALUES IN (f(1));
-- @case: rust:tests:inherits_binds_parents_without_fence:1
-- @dialects: postgresql
CREATE TABLE c (a INT) INHERITS (p1, p2);
-- @case: rust:tests:inherits_binds_parents_without_fence:2
-- @dialects: postgresql
CREATE TABLE c PARTITION OF a.b.c.d.e FOR VALUES IN (1);
-- @case: rust:tests:attach_detach_partition_emit_link_ops:1
-- @dialects: postgresql
ALTER TABLE p ATTACH PARTITION c FOR VALUES FROM (1) TO (10);
-- @case: rust:tests:attach_detach_partition_emit_link_ops:2
-- @dialects: postgresql
ALTER TABLE p DETACH PARTITION c;
-- @case: rust:tests:attach_detach_partition_emit_link_ops:3
-- @dialects: postgresql
ALTER TABLE p DETACH PARTITION c FINALIZE;
-- @case: rust:tests:inherit_no_inherit_emit_link_ops:1
-- @dialects: postgresql
ALTER TABLE c INHERIT p;
-- @case: rust:tests:inherit_no_inherit_emit_link_ops:2
-- @dialects: postgresql
ALTER TABLE c NO INHERIT p;
-- @case: rust:tests:inherit_no_inherit_emit_link_ops:3
-- @dialects: postgresql
ALTER TABLE s.c INHERIT s.p;
-- @case: rust:tests:malformed_inherit_and_other_alters_keep_fence:1
-- @dialects: postgresql
ALTER TABLE c INHERIT a.b.c.d.e;
-- @case: rust:tests:malformed_inherit_and_other_alters_keep_fence:2
-- @dialects: postgresql
ALTER TABLE t OWNER TO app;
-- @case: rust:tests:malformed_inherit_and_other_alters_keep_fence:3
-- @dialects: postgresql
ALTER TABLE t SET LOGGED;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:1
-- @dialects: postgresql
SELECT * FROM ONLY t;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:2
-- @dialects: postgresql
SELECT * FROM t;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:3
-- @dialects: postgresql
SELECT * FROM t *;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:4
-- @dialects: postgresql
UPDATE ONLY t SET a = 1;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:5
-- @dialects: postgresql
DELETE FROM ONLY t;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:6
-- @dialects: postgresql
TRUNCATE ONLY t;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:7
-- @dialects: postgresql
UPDATE t SET a = 1;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:8
-- @dialects: postgresql
DELETE FROM t;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:9
-- @dialects: postgresql
TRUNCATE t;
-- @case: rust:tests:only_marker_threads_through_reads_and_writes:10
-- @dialects: postgresql
INSERT INTO t VALUES (1);
-- @case: rust:tests:mysql_foreign_key_checks_scope_forms:1
-- @dialects: mysql
SET foreign_key_checks = 0;
-- @case: rust:tests:mysql_foreign_key_checks_scope_forms:2
-- @dialects: mysql
SET FOREIGN_KEY_CHECKS = OFF;
-- @case: rust:tests:mysql_foreign_key_checks_scope_forms:3
-- @dialects: mysql
SET SESSION foreign_key_checks = 1;
-- @case: rust:tests:mysql_foreign_key_checks_scope_forms:4
-- @dialects: mysql
SET GLOBAL foreign_key_checks = 0;
-- @case: rust:tests:mysql_foreign_key_checks_scope_forms:5
-- @dialects: mysql
SET @@foreign_key_checks = 1;
-- @case: rust:tests:mysql_foreign_key_checks_scope_forms:6
-- @dialects: mysql
SET @@session.foreign_key_checks = 0;
-- @case: rust:tests:mysql_foreign_key_checks_scope_forms:7
-- @dialects: mysql
SET @@global.foreign_key_checks = 0;
-- @case: rust:tests:mysql_foreign_key_checks_scope_forms:8
-- @dialects: mysql
SET @@GLOBAL.foreign_key_checks = 1;
-- @case: rust:tests:mysql_foreign_key_checks_scope_forms:9
-- @dialects: mysql
SET foreign_key_checks = TRUE;
-- @case: rust:tests:mysql_foreign_key_checks_unmodeled_spellings_stay_fenced:1
-- @dialects: mysql
SET @foreign_key_checks = 0;
-- @case: rust:tests:mysql_foreign_key_checks_unmodeled_spellings_stay_fenced:2
-- @dialects: mysql
SET @@global.super_read_only = TRUE;
-- @case: rust:tests:mysql_foreign_key_checks_unmodeled_spellings_stay_fenced:3
-- @dialects: mysql
SET foreign_key_checks = @x;
-- @case: rust:tests:mysql_foreign_key_checks_unmodeled_spellings_stay_fenced:4
-- @dialects: mysql
SET @@global.foreign_key_checks = @x;
-- @case: rust:tests:mysql_foreign_key_checks_unmodeled_spellings_stay_fenced:5
-- @dialects: mysql
SET GLOBAL @@session.foreign_key_checks = 0;
-- @case: rust:tests:mysql_foreign_key_checks_unmodeled_spellings_stay_fenced:6
-- @dialects: mysql
SET SESSION @@global.foreign_key_checks = 0;
-- @case: rust:tests:mysql_foreign_key_checks_unmodeled_spellings_stay_fenced:7
-- @dialects: mysql
SET app.foreign_key_checks = 0;
-- @case: rust:tests:mysql_autocommit_scope_forms:1
-- @dialects: mysql
SET autocommit = 0;
-- @case: rust:tests:mysql_autocommit_scope_forms:2
-- @dialects: mysql
SET SESSION autocommit = 1;
-- @case: rust:tests:mysql_autocommit_scope_forms:3
-- @dialects: mysql
SET GLOBAL autocommit = 0;
-- @case: rust:tests:mysql_autocommit_scope_forms:4
-- @dialects: mysql
SET @@autocommit = 1;
-- @case: rust:tests:mysql_autocommit_scope_forms:5
-- @dialects: mysql
SET @@session.autocommit = 0;
-- @case: rust:tests:mysql_autocommit_scope_forms:6
-- @dialects: mysql
SET @@global.autocommit = 0;
-- @case: rust:tests:system_var_user_spellings_stay_fenced:1
-- @dialects: mysql
SET autocommit = 1;
-- @case: rust:tests:system_var_user_spellings_stay_fenced:3
-- @dialects: mysql
SET @autocommit = 0;
-- @case: rust:tests:system_var_user_spellings_stay_fenced:4
-- @dialects: mysql
SET @search_path = 'public';
-- @case: rust:tests:system_var_user_spellings_stay_fenced:5
-- @dialects: mysql
SET GLOBAL @@session.autocommit = 0;
-- @case: rust:tests:system_var_user_spellings_stay_fenced:6
-- @dialects: mysql
SET SESSION @@global.autocommit = 0;
-- @case: rust:tests:system_var_user_spellings_stay_fenced:7
-- @dialects: mysql
SET app.autocommit = 0;
-- @case: rust:tests:system_var_user_spellings_stay_fenced:8
-- @dialects: mysql
SET app.search_path = 'public';
-- @case: rust:tests:reset_at_spellings_stay_fenced:1
-- @dialects: mysql
RESET @search_path;
-- @case: rust:tests:reset_at_spellings_stay_fenced:2
-- @dialects: mysql
RESET @foreign_key_checks;
-- @case: rust:tests:reset_at_spellings_stay_fenced:3
-- @dialects: mysql
RESET @@global.foreign_key_checks;
-- @case: rust:tests:reset_at_spellings_stay_fenced:4
-- @dialects: mysql
RESET @@session.search_path;
-- @case: rust:tests:reset_at_spellings_stay_fenced:5
-- @dialects: postgresql
RESET search_path;
-- @case: rust:tests:reset_at_spellings_stay_fenced:6
-- @dialects: postgresql
RESET foreign_key_checks;
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:1
-- @dialects: sqlite
PRAGMA synchronous = 2;
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:2
-- @dialects: sqlite
PRAGMA synchronous = 1;
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:3
-- @dialects: sqlite
PRAGMA temp_store = 2;
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:4
-- @dialects: sqlite
PRAGMA cache_size = 1000;
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:5
-- @dialects: sqlite
PRAGMA page_size = 4096;
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:6
-- @dialects: sqlite
PRAGMA journal_mode = 'wal';
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:7
-- @dialects: sqlite
PRAGMA synchronous = 'FULL';
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:8
-- @dialects: sqlite
PRAGMA temp_store = 'MEMORY';
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:9
-- @dialects: sqlite
PRAGMA page_size = '4096';
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:10
-- @dialects: sqlite
PRAGMA journal_mode;
-- @case: rust:tests:sqlite_inert_pragmas_record_without_fence:11
-- @dialects: sqlite
PRAGMA page_size;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:2
-- @dialects: sqlite
PRAGMA recursive_triggers = 1;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:3
-- @dialects: sqlite
PRAGMA recursive_triggers(0);
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:4
-- @dialects: sqlite
PRAGMA defer_foreign_keys = 0;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:5
-- @dialects: sqlite
PRAGMA defer_foreign_keys = 1;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:6
-- @dialects: sqlite
PRAGMA defer_foreign_keys(1);
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:7
-- @dialects: sqlite
PRAGMA ignore_check_constraints = 1;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:8
-- @dialects: sqlite
PRAGMA ignore_check_constraints;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:9
-- @dialects: sqlite
PRAGMA encoding = 'UTF-8';
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:10
-- @dialects: sqlite
PRAGMA encoding;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:11
-- @dialects: sqlite
PRAGMA frobnicate = 1;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:12
-- @dialects: sqlite
PRAGMA frobnicate;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:13
-- @dialects: sqlite
PRAGMA integrity_check;
-- @case: rust:tests:sqlite_effectful_and_unknown_pragmas_stay_fenced:14
-- @dialects: sqlite
PRAGMA page_size = ?;
-- @case: rust:tests:sqlite_pure_pragma_reads_lift_without_fence:1
-- @dialects: sqlite
PRAGMA recursive_triggers;
-- @case: rust:tests:sqlite_pure_pragma_reads_lift_without_fence:2
-- @dialects: sqlite
PRAGMA defer_foreign_keys;
-- @case: rust:tests:sqlite_pure_pragma_reads_lift_without_fence:3
-- @dialects: sqlite
PRAGMA main.recursive_triggers;
-- @case: rust:tests:sqlite_create_virtual_table_scopes_barrier_to_relation:1
-- @dialects: sqlite
CREATE VIRTUAL TABLE vt0 USING fts5(c0);
-- @case: rust:tests:sqlite_create_virtual_table_scopes_barrier_to_relation:2
-- @dialects: sqlite
CREATE VIRTUAL TABLE IF NOT EXISTS temp.vt1 USING fts4(c0, c1);
-- @case: rust:tests:sqlite_create_virtual_table_scopes_barrier_to_relation:3
-- @dialects: sqlite
CREATE VIRTUAL TABLE vt_no_args USING dbstat;
-- @case: rust:tests:sqlite_create_virtual_table_scopes_barrier_to_relation:4
-- @dialects: sqlite
CREATE VIRTUAL TABLE main.vt_main USING fts5(c0);
-- @case: rust:tests:sqlite_create_virtual_table_scopes_barrier_to_relation:5
-- @dialects: sqlite
CREATE VIRTUAL TABLE "vt spaced" USING fts5("c 0");
-- @case: rust:tests:unconditional_and_filtered_update_scope:3
-- @dialects: postgresql
UPDATE t SET a = 1 FROM u WHERE t.id = u.id;
-- @case: rust:tests:unconditional_and_filtered_update_scope:4
-- @dialects: mysql
UPDATE t SET a = 1 LIMIT 1;
-- @case: rust:tests:unconditional_and_filtered_update_scope:5
-- @dialects: postgresql
UPDATE t SET a = (SELECT 1 FROM t);
-- @case: rust:tests:unconditional_and_filtered_update_scope:6
-- @dialects: postgresql
UPDATE t SET a = 1 RETURNING a;
-- @case: rust:tests:routine_ddl_disables_count_specialization:1
-- @dialects: postgresql
CREATE FUNCTION count(x INT) RETURNS INT LANGUAGE SQL AS 'SELECT x';
-- @case: rust:tests:routine_ddl_disables_count_specialization:2
-- @dialects: postgresql
CREATE AGGREGATE count(int) (SFUNC = int4pl, STYPE = int);
-- @case: rust:tests:read_mode_classification_row_count_and_existence:2
-- @dialects: postgresql
SELECT COUNT(1) AS c FROM t;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:3
-- @dialects: postgresql
SELECT COUNT(col) FROM t;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:4
-- @dialects: postgresql
SELECT COUNT(*) FROM t WHERE x > 1;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:5
-- @dialects: postgresql
SELECT COUNT(*) FROM t GROUP BY x;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:6
-- @dialects: postgresql
SELECT 1 FROM t LIMIT 1;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:7
-- @dialects: postgresql
SELECT EXISTS (SELECT 1 FROM t);
-- @case: rust:tests:read_mode_classification_row_count_and_existence:8
-- @dialects: postgresql
SELECT EXISTS (SELECT * FROM t);
-- @case: rust:tests:read_mode_classification_row_count_and_existence:9
-- @dialects: postgresql
SELECT COUNT(*) OVER () FROM t;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:10
-- @dialects: postgresql
SELECT COUNT(NULL) FROM t;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:11
-- @dialects: postgresql
SELECT COUNT('hello') FROM t;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:12
-- @dialects: postgresql
SELECT COUNT(*) FROM t FOR UPDATE;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:13
-- @dialects: postgresql
SELECT 1 FROM t LIMIT 1 FOR UPDATE;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:14
-- @dialects: postgresql
SELECT EXISTS (SELECT 1 FROM t FOR UPDATE);
-- @case: rust:tests:read_mode_classification_row_count_and_existence:15
-- @dialects: postgresql
SELECT 1 FROM t LIMIT 0;
-- @case: rust:tests:read_mode_classification_row_count_and_existence:16
-- @dialects: postgresql
SELECT 1 FROM t LIMIT 00;
-- @case: rust:tests:test_duckdb_create_macro:1
-- @dialects: duckdb
CREATE MACRO add(a, b) AS a + b;
-- @case: rust:tests:test_duckdb_create_macro:2
-- @dialects: duckdb
CREATE MACRO select_t() AS TABLE SELECT * FROM t;
-- @case: rust:tests:test_pure_table_generators:1
-- @dialects: postgresql
SELECT * FROM generate_series(1, 10);
-- @case: rust:tests:test_pure_table_generators:2
-- @dialects: postgresql
SELECT * FROM pg_catalog.generate_series(1, 10);
-- @case: rust:tests:test_pure_table_generators:3
-- @dialects: duckdb
SELECT * FROM range(10) AS t;
-- @case: rust:tests:test_pure_table_generators:4
-- @dialects: postgresql
SELECT * FROM analytics.generate_series(1, 10);
-- @case: rust:tests:test_file_table_functions:1
-- @dialects: duckdb
SELECT * FROM read_parquet('data.parquet') AS p;
-- @case: rust:tests:test_file_table_functions:2
-- @dialects: duckdb
SELECT * FROM read_csv(['f1.csv', 'f2.csv']);
-- @case: rust:tests:test_file_table_functions:3
-- @dialects: duckdb
SELECT * FROM read_csv('data.csv', delim = '|', nullstr = 'NA');
-- @case: rust:tests:test_update_and_delete_syntax_edits:1
-- @dialects: postgresql
UPDATE t SET a = 1, b = 2, c = 3 WHERE id = 10;
-- @case: rust:tests:test_update_and_delete_syntax_edits:2
-- @dialects: mysql
UPDATE t SET a = 1 ORDER BY id LIMIT 5;
-- @case: rust:tests:test_update_and_delete_syntax_edits:3
-- @dialects: postgresql
UPDATE t SET a = 1 RETURNING a, b;
-- @case: rust:tests:test_update_and_delete_syntax_edits:4
-- @dialects: mysql
DELETE FROM t WHERE id = 10 ORDER BY id LIMIT 5;
-- @case: rust:tests:test_update_and_delete_syntax_edits:5
-- @dialects: postgresql
DELETE FROM t WHERE id = 10 RETURNING id;
-- @case: rust:tests:test_update_and_delete_syntax_edits:6
-- @dialects: mysql
UPDATE t1 JOIN t2 ON t1.id = t2.id JOIN t3 ON t1.id = t3.id SET t1.a = 1;
-- @case: rust:tests:test_set_operation_syntax_edits:1
-- @dialects: postgresql
SELECT 1 UNION ALL SELECT 2;
-- @case: rust:tests:test_set_operation_syntax_edits:2
-- @dialects: postgresql
(SELECT 1) UNION ALL (SELECT 2);
-- @case: rust:tests:test_set_operation_syntax_edits:3
-- @dialects: postgresql
SELECT 1 UNION ALL SELECT 2 ORDER BY 1 LIMIT 1;
-- @case: rust:tests:test_set_operation_syntax_edits:4
-- @dialects: postgresql
SELECT 999 AS noise UNION ALL (SELECT 888 AS noise) UNION ALL SELECT 42 AS target;
-- @case: rust:tests:test_distinct_and_group_by_syntax_edits:1
-- @dialects: postgresql
SELECT DISTINCT a, b FROM t GROUP BY a, b ORDER BY a, b;
-- @case: rust:tests:test_distinct_and_group_by_syntax_edits:2
-- @dialects: postgresql
SELECT ALL a FROM t;
-- @case: rust:tests:test_distinct_and_group_by_syntax_edits:3
-- @dialects: postgresql
SELECT DISTINCT ON (a, b) c FROM t;
-- @case: rust:tests:test_distinct_and_group_by_syntax_edits:4
-- @dialects: postgresql
SELECT count(DISTINCT x) FROM t;
-- @case: rust:tests:test_window_spec_syntax_edits:1
-- @dialects: postgresql
SELECT sum(x) OVER (PARTITION BY a, b ORDER BY c, d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;
-- @case: rust:tests:lexical_suffix_preserves_prefix_and_disables_count_specialization:1
-- @dialects: postgresql
SELECT count(*) FROM t; /* unfinished
-- @case: rust:tests:test_cast_simplification_syntax_edits:1
-- @dialects: postgresql
SELECT CAST(col AS VARCHAR(255)) FROM t;
-- @case: rust:tests:test_cast_simplification_syntax_edits:2
-- @dialects: postgresql
SELECT col::text FROM t;
-- @case: rust:tests:test_cast_simplification_syntax_edits:3
-- @dialects: duckdb
SELECT TRY_CAST(col AS INT) FROM t;
-- @case: rust:tests:sqlite_identifiers_fold_ascii_only:1
-- @dialects: sqlite
CREATE TABLE "Å"(x); CREATE TABLE "å"(y); SELECT x FROM "Å";
-- @case: rust:tests:partial_assignment_span_is_not_used_for_list_deletion:1
-- @dialects: postgresql
UPDATE t SET a = 1, b = sum(c) FILTER (WHERE d > 0)
-- @case: rust:tests:window_partition_deletion_skips_nested_order_by:1
-- @dialects: postgresql
SELECT sum(a) OVER (PARTITION BY (SELECT b FROM u ORDER BY b LIMIT 1) ORDER BY c) FROM t
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:1
-- @dialects: mysql
SET @f = _utf32 'a'; CREATE TABLE t1 (a INT); SELECT 1 FROM t1 WHERE @f<>'3761-03-28' XOR (@f:='5570-12-30') > a;
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:2
-- @dialects: mysql
SELECT a FROM t WHERE (a,b) in (SELECT @c,@d);
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:3
-- @dialects: mysql
SET @X = @Y + 1;
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:4
-- @dialects: mysql
SELECT @@sql_mode FROM t;
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:5
-- @dialects: mysql
SELECT a INTO @x FROM t;
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:6
-- @dialects: mysql
SELECT a, b INTO @x, @y FROM t1;
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:7
-- @dialects: mysql
SET @x = (SELECT a FROM t);
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:8
-- @dialects: mysql
SET @x = (SELECT a FROM unknown_tvf());
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:9
-- @dialects: mysql
SELECT a INTO @x FROM (SELECT b INTO t2 FROM t1) s;
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:10
-- @dialects: mysql
SELECT a INTO @x FROM t1 WHERE EXISTS (SELECT b INTO t2 FROM t3);
-- @case: rust:tests:user_variables_are_excluded_from_table_reads_and_scoped:11
-- @dialects: mysql
SELECT a INTO TEMPORARY @x FROM t1;
-- @case: rust:tests:clickhouse_empty_tuple_order_by_lifts_routine_fence:1
-- @dialects: clickhouse
CREATE TABLE t0 (c0 UInt64) ENGINE = MergeTree ORDER BY tuple();
-- @case: rust:tests:clickhouse_empty_tuple_order_by_lifts_routine_fence:2
-- @dialects: clickhouse
SELECT tuple();
-- @case: rust:columns:implicit_bundle_binds_by_definition_order:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t VALUES (1, 10), (3, 20);
-- @case: rust:columns:constraint_blocks_only_the_constrained_column:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT PRIMARY KEY);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:read_elsewhere_blocks_without_repair:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT b FROM t;
-- @case: rust:columns:star_projection_blocks:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT * FROM t;
-- @case: rust:columns:routine_call_keeps_global_opaque_fence:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT frobnicate(a) FROM t;
-- @case: rust:columns:parse_error_unit_blocks_everything:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
GARBAGE (((;
-- @case: rust:columns:parent_fk_blocks_referenced_column:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
CREATE TABLE u(x INT REFERENCES t(b));
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:implicit_parent_fk_blocks_whole_parent:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
CREATE TABLE u(x INT, FOREIGN KEY (x) REFERENCES t);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:alter_parent_fk_blocks_referenced_column:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
CREATE TABLE u(x INT);
ALTER TABLE u ADD FOREIGN KEY (x) REFERENCES t(b);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:alter_implicit_parent_fk_blocks_whole_parent:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
CREATE TABLE u(x INT);
ALTER TABLE u ADD FOREIGN KEY (x) REFERENCES t;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:alter_add_column_parent_fk_blocks_referenced_column:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
CREATE TABLE u(x INT);
ALTER TABLE u ADD COLUMN y INT REFERENCES t(b);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:insert_before_create_cannot_bind:1
-- @dialects: sqlite
INSERT INTO t(a, b) VALUES (1, 2);
CREATE TABLE t(a INT, b INT);
-- @case: rust:columns:drop_fences_the_table:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
DROP TABLE t;
-- @case: rust:columns:arity_mismatch_blocks_listed_columns:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1);
-- @case: rust:columns:ragged_rows_block_implicit_binding:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t VALUES (1, 10), (3);
-- @case: rust:columns:select_source_fences_only_listed_columns:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a) SELECT a FROM u;
-- @case: rust:columns:singleton_list_emits_no_item_sites:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a) VALUES (1);
-- @case: rust:columns:duplicate_create_names_are_ambiguous:1
-- @dialects: sqlite
CREATE TABLE t(a INT, a INT);
INSERT INTO t VALUES (1, 2);
-- @case: rust:columns:alter_other_table_never_fences:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
CREATE TABLE u(x INT);
ALTER TABLE u ADD COLUMN y INT;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:alter_other_table_same_column_name_never_fences:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
CREATE TABLE u(a INT);
ALTER TABLE u ALTER COLUMN a SET NOT NULL;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:add_column_other_lifts_with_explicit_inserts:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ADD COLUMN c INT;
INSERT INTO t(a, b) VALUES (1, 2);
SELECT a FROM t ORDER BY a;
-- @case: rust:columns:add_column_other_still_fences_implicit_inserts:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ADD COLUMN c INT;
INSERT INTO t VALUES (1, 2);
-- @case: rust:columns:add_column_touching_name_fences_only_it:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ADD COLUMN b INT;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:add_column_generated_fences_all:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ADD COLUMN c INT GENERATED ALWAYS AS (a + b);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:add_constraint_without_column_lifts:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ADD CHECK (a > 0);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:add_constraint_opaque_fences_all:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ADD PRIMARY KEY USING INDEX pk_t;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:drop_column_other_lifts_with_explicit_inserts:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT, c INT);
ALTER TABLE t DROP COLUMN c;
INSERT INTO t(a, b) VALUES (1, 2);
SELECT a FROM t ORDER BY a;
-- @case: rust:columns:drop_column_other_still_fences_implicit_inserts:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT, c INT);
ALTER TABLE t DROP COLUMN c;
INSERT INTO t VALUES (1, 2, 3);
-- @case: rust:columns:drop_column_self_fences_only_it:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT, c INT);
ALTER TABLE t DROP COLUMN b;
INSERT INTO t(a, c) VALUES (1, 3);
SELECT a FROM t ORDER BY a;
-- @case: rust:columns:rename_column_other_lifts:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT, c INT);
ALTER TABLE t RENAME COLUMN c TO d;
INSERT INTO t(a, b, d) VALUES (1, 2, 3);
SELECT a FROM t ORDER BY a;
-- @case: rust:columns:rename_column_self_fences_only_it:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
ALTER TABLE t RENAME COLUMN b TO b2;
INSERT INTO t(a, b2) VALUES (1, 2);
-- @case: rust:columns:alter_column_no_expr_fences_target_only:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ALTER COLUMN a SET NOT NULL;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:alter_column_pure_type_fences_target_only:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ALTER COLUMN a TYPE BIGINT;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:alter_column_using_fences_target_and_refs:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT, c INT);
ALTER TABLE t ALTER COLUMN a TYPE BIGINT USING (a + b);
INSERT INTO t(a, b, c) VALUES (1, 2, 3);
-- @case: rust:columns:alter_column_set_default_fences_target_and_refs:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT, c INT);
ALTER TABLE t ALTER COLUMN a SET DEFAULT (b + 1);
INSERT INTO t(a, b, c) VALUES (1, 2, 3);
-- @case: rust:columns:alter_column_identity_fences_target_only:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ALTER COLUMN a ADD GENERATED ALWAYS AS IDENTITY (START WITH 10);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:alter_column_generic_options_fence_target_only:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ALTER COLUMN a OPTIONS (ADD x 'y');
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:modify_column_plain_fences_target_only:1
-- @dialects: mysql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t MODIFY COLUMN a INT NOT NULL;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:modify_column_default_ref_fences_target_and_refs:1
-- @dialects: mysql
CREATE TABLE t(a INT, b INT, c INT);
ALTER TABLE t MODIFY COLUMN a INT DEFAULT (b);
INSERT INTO t(a, b, c) VALUES (1, 2, 3);
-- @case: rust:columns:modify_column_after_fences_all:1
-- @dialects: mysql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t MODIFY COLUMN a INT AFTER b;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:change_column_first_fences_all:1
-- @dialects: mysql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t CHANGE COLUMN a a INT FIRST;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:change_column_rename_narrows_other:1
-- @dialects: mysql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t CHANGE COLUMN a a2 INT NOT NULL;
INSERT INTO t(a2, b) VALUES (1, 2);
-- @case: rust:columns:modify_column_auto_increment_stays_fenced:1
-- @dialects: mysql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t MODIFY COLUMN a INT AUTO_INCREMENT;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:modify_column_self_fk_explicit_narrows:1
-- @dialects: mysql
CREATE TABLE t(a INT, b INT, c INT);
ALTER TABLE t MODIFY COLUMN a INT REFERENCES t(b);
INSERT INTO t(a, b, c) VALUES (1, 2, 3);
-- @case: rust:columns:modify_column_self_fk_implicit_stays_fenced:1
-- @dialects: mysql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t MODIFY COLUMN a INT REFERENCES t;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:drop_constraint_still_fences_all:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t DROP CONSTRAINT c;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:returning_fences_inserts:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2) RETURNING a;
-- @case: rust:columns:first_middle_last_comma_gating:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT, c INT);
INSERT INTO t(a, b, c) VALUES (1, 2, 3);
-- @case: rust:columns:mixed_qualification_blocks_as_ambiguous:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
INSERT INTO main.t(a, b) VALUES (3, 4);
-- @case: rust:columns:transaction_effects_block_as_opaque:1
-- @dialects: sqlite
BEGIN;
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
COMMIT;
-- @case: rust:columns:transaction_on_unrelated_table_does_not_block_clean_table:1
-- @dialects: sqlite
BEGIN;
CREATE TABLE other(x INT);
INSERT INTO other VALUES (1);
COMMIT;
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT a FROM t ORDER BY a;
-- @case: rust:columns:transaction_after_clean_table_does_not_block_clean_table:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT a FROM t ORDER BY a;
BEGIN;
INSERT INTO other VALUES (1);
COMMIT;
-- @case: rust:columns:empty_transaction_does_not_block_clean_table:1
-- @dialects: sqlite
BEGIN;
COMMIT;
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT a FROM t ORDER BY a;
-- @case: rust:columns:scoped_barrier_does_not_block_unaffected_table:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT a FROM t ORDER BY a;
CREATE TABLE other(x INT);
SELECT frobnicate(x) FROM other;
-- @case: rust:columns:pragma_foreign_keys_does_not_block_clean_table:1
-- @dialects: sqlite
PRAGMA foreign_keys = 1;
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT a FROM t ORDER BY a;
-- @case: rust:columns:standalone_scalar_routine_does_not_block_clean_table:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT a FROM t ORDER BY a;
SELECT frobnicate(1);
-- @case: rust:columns:rollback_and_chain_keeps_in_transaction_fenced:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
BEGIN;
ROLLBACK AND CHAIN;
INSERT INTO t(a, b) VALUES (1, 2);
COMMIT;
SELECT a FROM t ORDER BY a;
-- @case: rust:columns:mixed_star_and_explicit_read_still_blocks:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT t1.*, t2.a FROM t AS t1 JOIN t AS t2 ON t1.a = t2.a;
-- @case: rust:columns:temporary_tables_are_outside_first_family:1
-- @dialects: sqlite
CREATE TEMP TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:rename_table_followed:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
ALTER TABLE t RENAME TO u;
INSERT INTO u(a, b) VALUES (1, 2);
SELECT a FROM u ORDER BY a;
-- @case: rust:columns:rename_chain_two_hops_binds_each_segment:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
ALTER TABLE t RENAME TO u;
INSERT INTO u(a, b) VALUES (3, 4);
ALTER TABLE u RENAME TO v;
INSERT INTO v(a, b) VALUES (5, 6);
-- @case: rust:columns:rename_stale_insert_binds_no_sites:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
ALTER TABLE t RENAME TO u;
INSERT INTO u(a, b) VALUES (1, 2);
INSERT INTO t(a, b) VALUES (3, 4);
-- @case: rust:columns:rename_fork_fences:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
ALTER TABLE t RENAME TO u;
ALTER TABLE t RENAME TO v;
INSERT INTO u(a, b) VALUES (1, 2);
-- @case: rust:columns:rename_before_create_fences:1
-- @dialects: sqlite
ALTER TABLE t RENAME TO u;
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:rename_onto_created_name_fences:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
CREATE TABLE u(x INT);
ALTER TABLE t RENAME TO u;
INSERT INTO u(a, b) VALUES (1, 2);
-- @case: rust:columns:rename_into_chain_fences:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
CREATE TABLE x(y INT);
ALTER TABLE t RENAME TO u;
ALTER TABLE x RENAME TO u;
INSERT INTO u(a, b) VALUES (1, 2);
-- @case: rust:columns:rename_with_drop_fences:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
ALTER TABLE t RENAME TO u;
DROP TABLE u;
INSERT INTO u(a, b) VALUES (1, 2);
-- @case: rust:columns:rename_same_unit_alter_fences:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t RENAME TO u, ADD COLUMN c INT;
INSERT INTO u(a, b) VALUES (1, 2);
-- @case: rust:columns:alter_multi_add_other_lifts:1
-- @dialects: postgresql
CREATE TABLE t(a INT, b INT);
ALTER TABLE t ADD COLUMN c INT, ADD COLUMN d INT;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:drop_recreate_fences:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
DROP TABLE t;
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:identical_spelling_case_variants_form:1
-- @dialects: sqlite
CREATE TABLE T(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT A FROM T ORDER BY A;
-- @case: rust:columns:identical_spelling_quoted_variants_form:1
-- @dialects: sqlite
CREATE TABLE "T"(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT a FROM "t" ORDER BY a;
-- @case: rust:columns:ambiguous_read_fences_only_read_column:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT b FROM main.t;
-- @case: rust:columns:ambiguous_insert_list_fences_listed_only:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
INSERT INTO main.t(a) VALUES (3);
-- @case: rust:columns:ambiguous_implicit_insert_fences_all:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
INSERT INTO main.t VALUES (3, 4);
-- @case: rust:columns:ambiguous_star_fences_all:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
SELECT * FROM main.t;
-- @case: rust:columns:ambiguous_ddl_keeps_whole_table:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
ALTER TABLE main.t ADD COLUMN c INT;
-- @case: rust:columns:ambiguous_fk_parent_fences_named_columns:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
CREATE TABLE u(x INT, FOREIGN KEY (x) REFERENCES main.t(b));
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:like_defines_no_bundles:1
-- @dialects: postgresql
CREATE TABLE t LIKE u;
CREATE TABLE s(a INT, b INT);
INSERT INTO s(a, b) VALUES (1, 2);
-- @case: rust:columns:ctas_sources_still_fence:1
-- @dialects: sqlite
CREATE TABLE u(x INT, y INT);
CREATE TABLE t(a, b) AS SELECT x, y FROM u;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:columns:on_commit_still_fences:1
-- @dialects: postgresql
CREATE TEMP TABLE t(a INT, b INT) ON COMMIT DROP;
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:ffi:column_payload_gates_independently:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b INT);
INSERT INTO t(a, b) VALUES (1, 2);
-- @case: rust:ffi:syntax_edits_disabled_preserves_spans_and_semantic_facts:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b TEXT);
WITH cte(x) AS (SELECT a FROM t WHERE a > 0) SELECT x FROM cte;
INSERT INTO t VALUES (1, 'ok');
SELECT * FROM t; SELECT 'unterminated
-- @case: rust:ffi:syntax_only_preserves_all_edits_and_skips_semantic_ops:1
-- @dialects: sqlite
CREATE TABLE t(a INT, b TEXT);
INSERT INTO t VALUES (1, 'hello'), (2, 'world');
WITH cte AS (SELECT a, b FROM t WHERE a > 0) SELECT a, b FROM cte ORDER BY a;
-- @case: rust:ffi:top_level_projection_flag_marks_only_outer_wildcard_free_select:1
-- @dialects: sqlite
WITH c AS (SELECT x, y FROM t) SELECT a, b FROM c;
SELECT *, a FROM t;
SELECT a, b FROM t UNION SELECT c, d FROM u;
CREATE VIEW v AS SELECT a, b FROM t;
-- @case: rust:ffi:recursive_predicate_flag_scopes_to_recursive_member_predicates:1
-- @dialects: sqlite
WITH RECURSIVE org AS (
  SELECT emp_id, manager_id, 1 AS depth FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.emp_id, e.manager_id, h.depth + 1 FROM employees e JOIN org h ON e.manager_id = h.emp_id
),
dept AS (SELECT emp_id FROM org JOIN employees d ON org.emp_id = d.emp_id WHERE d.emp_id > 0)
SELECT h.emp_id FROM org h JOIN dept d ON h.emp_id = d.emp_id WHERE h.emp_id > 10;
WITH RECURSIVE cnt(x) AS (VALUES (1) UNION ALL SELECT x + 1 FROM cnt WHERE x < 10) SELECT x FROM cnt;
WITH plain AS (SELECT e.emp_id FROM employees e JOIN employees m ON e.manager_id = m.emp_id) SELECT * FROM plain;
-- @case: rust:ffi:literal_and_expression_flags_classify_in_rust:1
-- @dialects: sqlite
SELECT 'payload_alpha', 10000, a > 0;
-- @case: rust:parser:non_result_allowlist_classifies_ddl_dml_and_excludes_queries_and_returning:1
-- @dialects: sqlite
CREATE TABLE t(x INT);
CREATE TABLE ctas AS SELECT 1;
INSERT INTO t VALUES (1);
INSERT INTO t VALUES (2) RETURNING x;
UPDATE t SET x = 3;
UPDATE t SET x = 4 RETURNING x;
DELETE FROM t WHERE x = 1;
DELETE FROM t WHERE x = 2 RETURNING x;
CREATE INDEX idx_t ON t(x);
BEGIN;
SAVEPOINT s;
ROLLBACK TO s;
RELEASE s;
COMMIT;
PRAGMA foreign_keys = ON;
SELECT x FROM t;
EXPLAIN SELECT 1;

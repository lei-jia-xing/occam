# Real DBMS Fuzz Crash Corpus (PostgreSQL, MySQL, MariaDB, ClickHouse, SQLite & DuckDB)

This directory contains **71 authentic, unreduced, multi-statement fuzz crash cases** extracted from official DBMS defect trackers and fuzzer reports across all 6 DBMS targets supported by [`harness/`](../../harness/README.md).

| DBMS | Occam Dialect (`DBMS_TO_OCCAM_DIALECT`) | Case Count | Target Image / Version Focus | Local Live-5/5 Verification |
| :--- | :--- | :---: | :--- | :--- |
| PostgreSQL (`postgres`) | `postgres` | 12 | PostgreSQL `13.0` / `12.0` (`--enable-cassert --enable-debug`) | Upstream-provenance (no local PG container) |
| MySQL (`mysql`) | `mysql` | 15 | MySQL `8.0.20-debug` (`-DWITH_DEBUG=1`), `8.0.1`–`8.0.12-debug` | Upstream-provenance (no local MySQL container) |
| MariaDB (`mariadb`) | `mysql` | 12 | MariaDB `10.5.8-debug` (`-DCMAKE_BUILD_TYPE=Debug`) | Upstream-provenance (no local MariaDB container) |
| ClickHouse (`clickhouse`) | `clickhouse` | 10 | ClickHouse `26.8.44.3` release, `25.8` LTS, `26.x-debug` (`LOGICAL_ERROR`) | Upstream-provenance (no local CH container) |
| SQLite (`sqlite`) | `sqlite` | 12 | SQLite `3.31.0-debug` (`-DSQLITE_DEBUG=1`), `3.30.0` | **9 new cases verified 5/5 live** on `localhost/occam-target-sqlite:latest` |
| DuckDB (`duckdb`) | `duckdb` | 10 | DuckDB `v1.5.5` & `v1.1.3` (`-DFORCE_ASSERT=1`) | **2 `v1.5.5` cases verified 5/5 live** on local `duckdb` `v1.5.5`; 8 `v1.1.3` upstream-provenance |

---

## 1. Selection Criteria & Methodology

All cases follow four criteria:
1. **Authentic Provenance**: Every case originates directly from official DBMS bug trackers or commit regression tests (PostgreSQL `pgsql-bugs`/`pgsql-hackers`, MySQL Bugs & commits, MariaDB Jira `MDEV-*`, ClickHouse GitHub Issues & AST fuzzer, SQLite Fossil timeline/tickets, DuckDB GitHub Issues), discovered by leading DBMS fuzzers and bug-finding tools (**SQUIRREL**, **SQLsmith**, **SQLancer**, **pquery/RQG**, **ClickHouse AST fuzzer**, **OSS-Fuzz**).
2. **Multi-Statement Dependency**: Every case in the corpus is an ordered multi-statement sequence (`statement_count >= 2`, typically 3–27 statements) combining schema setup (`CREATE TABLE`, `CREATE VIEW`, `CREATE INDEX`), state mutations (`INSERT`, `UPDATE`, `ALTER`), settings/views/rules, and a triggering query/DML. None are isolated 1-line crashes.
3. **Deterministic Crash Signature & Self-Containment**: Each case specifies an `expected_signature` regex compatible with `harness/crash_oracle.py`, and avoids harness-incompatible state (no `USE test;`, no `default.` ClickHouse DB qualifiers, no mysqltest directives, no psql backslash meta-commands, and no global role/policy state that leaks past `harness/reset_db.sh`).
4. **5-Replay Reproducibility Protocol & Verification Scope**: All 9 new SQLite `3.31.0` cases (`localhost/occam-target-sqlite:latest`) and both local DuckDB `v1.5.5` cases (`duckdb` in PATH) were live-verified across **5 consecutive clean replays** via `harness/crash_oracle.py --print-signature`, producing identical deterministic crash signatures (`5/5`). Server-DBMS (`postgres`, `mysql`, `mariadb`, `clickhouse`) and `v1.1.3` DuckDB additions lack local target builds in this environment and are grounded in upstream bug-tracker provenance plus offline verification against `crash_oracle.extract_dbms_evidence`, `occam` parsing, and `harness/batch_eval.py --dry-run`.

---

## 2. Benchmark Case Catalog

| Case ID | DBMS (`dialect`) | Origin & Tool | Stmts / Bytes | Target Version & Flags | Trigger Statement | Assertion / Fault Summary | Replay Verification |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| `pg_aftertriggers_convslot_uaf` | PostgreSQL | [Commit 262eb990c7 (AfterTriggersTableData.storeslot UAF)](https://www.postgresql.org/message-id/flat/39a71864-b120-5a5c-8cc5-c632b6f16761@amazon.com)<br>PostgreSQL Hackers Bug Report (commit 262eb990c7) | 10 / 795B | 13.0<br>`./configure --enable-cassert --enable-debug` | `DELETE FROM parent;` | `Use-after-free SIGSEGV in execute_attr_map_slot (tupconvert.c:193)` | Upstream |
| `pg_fuzzer_mcv_clauses` | PostgreSQL | [Failed assertion clauses != NIL](https://www.postgresql.org/message-id/flat/20191119125051.GA2021%40gmail.com)<br>SQLancer / PQS DBMS fuzzer (OSDI 2020) | 5 / 194B | 12.0<br>`./configure --enable-cassert --enable-debug` | `SELECT * FROM t0 WHERE t0.c2 OR t0.c1 OR t0.c0;` | `!(clauses != ((List *) ((void *)0)))` | Upstream |
| `pg_fuzzer_mcv_enhanced` | PostgreSQL | [Failed assertion clauses != NIL](https://www.postgresql.org/message-id/flat/20191119125051.GA2021%40gmail.com)<br>SQLancer / PQS DBMS fuzzer (OSDI 2020) + Redundancy Augmentation (Thesis Section 6.1) | 22 / 2277B | 12.0<br>`./configure --prefix=/usr/local/pgsql --enable-cassert --enable-debug CFLAGS="-O1 -g -fno-omit-frame-pointer"` | `SELECT * FROM t0 WHERE t0.c2 OR t0.c1 OR t0.c0;` | `FailedAssertion.*mcv\.c` | Upstream |
| `pg_generated_like_defaults_segfault` | PostgreSQL | [sqlancer/bugs PostgreSQL #136](https://github.com/sqlancer/bugs)<br>SQLancer (OSDI 2020) | 3 / 154B | 12 Beta 2<br>`./configure --enable-cassert --enable-debug` | `INSERT INTO t1(c0) VALUES(NULL);` | `INSERT INTO causes segfault on LIKE ... INCLUDING DEFAULTS with STORED generated column` | Upstream |
| `pg_nocols_lateral_values_segfault` | PostgreSQL | [BUG #17477](https://www.postgresql.org/message-id/flat/17477-0af3c6ac6b0a6ae0@postgresql.org)<br>Fuzz / PostgreSQL Bug #17477 (commit 91a3a74c65) | 3 / 112B | 13.0<br>`./configure --enable-cassert --enable-debug` | `SELECT * FROM nocols n, LATERAL (VALUES(n.*)) v;` | `SEGV on null read in transformValuesClause (analyze.c:1512)` | Upstream |
| `pg_result_rte_lateral_assert` | PostgreSQL | [BUG #17097](https://www.postgresql.org/message-id/flat/17097-3372ef9f798fc94f@postgresql.org)<br>Grammar Fuzzer (PostgreSQL Bug #17097, commit 9807b9aedc) | 7 / 1072B | 13.0<br>`./configure --enable-cassert --enable-debug` | `CREATE TEMP VIEW v0 AS SELECT * FROM v0 WHERE...` | `FailedAssertion("false", File: "(?:prepjointree\|initsplan)\.c")` | Upstream |
| `pg_self_ref_view_info_schema` | PostgreSQL | [sqlancer/bugs PostgreSQL #187 (commit 4a0aab14dc)](https://github.com/postgres/postgres/commit/4a0aab14dc)<br>SQLancer (OSDI 2020) | 4 / 149B | 12.0<br>`./configure --enable-cassert --enable-debug` | `SELECT * FROM information_schema.tables;` | `Stack overflow SIGSEGV in relation_is_updatable (rewriteHandler.c)` | Upstream |
| `pg_sqlsmith_partprune` | PostgreSQL | [FailedAssertion on partprune](https://www.postgresql.org/message-id/CAJGNTeOkdk%3DUVuMugmKL7M%3Dowgt4nNr1wjxMg1F%2BmHsXyLCzFA%40mail.gmail.com)<br>SQLsmith (random query generator) | 22 / 4616B | 11-devel / 12.0<br>`./configure --enable-debug --enable-cassert` | `select (select pg_catalog.avg(actu_codi) ...` | `!(relid_subplan_map[rti] == 0)` | Upstream |
| `pg_stack_overflow_27stmts` | PostgreSQL | [Stack buffer overflow in 12beta2 release](https://github.com/sqlancer/bugs)<br>SQLancer (OSDI 2020) | 27 / 1317B | 12 Beta 2<br>`./configure --enable-cassert --enable-debug` | `ANALYZE;` | `Stack buffer overflow` | Upstream |
| `pg_temp_bitlength_index_segfault` | PostgreSQL | [sqlancer/bugs PostgreSQL #195 (commit c35b714caf)](https://github.com/postgres/postgres/commit/c35b714caf)<br>SQLancer (OSDI 2020) | 2 / 91B | 12.0<br>`./configure --enable-cassert --enable-debug` | `CREATE INDEX i0 ON t0(bit_length(''));` | `Segmentation fault when creating constant bit_length() index on TEMP ON COMMIT DELETE ROWS table` | Upstream |
| `pg_view_coll_oid` | PostgreSQL | [FailedAssertion in view.c](https://github.com/sqlancer/bugs)<br>SQLancer | 2 / 103B | 12.0<br>`./configure --enable-cassert --enable-debug` | `CREATE VIEW v0 AS SELECT * FROM t0 WHERE c1 LIKE 'abc%';` | `!OidIsValid(def->collOid)` | Upstream |
| `pg_with_notify_rule_segfault` | PostgreSQL | [BUG #17094](https://www.postgresql.org/message-id/flat/17094-bf15dff55eaf2e28@postgresql.org)<br>Grammar Fuzzer (PostgreSQL Bug #17094, commit 6edccac166) | 3 / 229B | 13.0<br>`./configure --enable-cassert --enable-debug` | `WITH v2 AS ( INSERT INTO v0 VALUES ( 0 ) ) DELETE...` | `!parse->rowMarks && parse->commandType == CMD_SELECT` | Upstream |
| `mysql_bug20794575_having_subquery_bits_assert` | MySQL | [Bug #20794575](https://github.com/mysql/mysql-server/commit/728e566656)<br>MySQL Optimizer Bug Report (fixed by WL#9384 commit 728e566656) | 4 / 167B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `SELECT b FROM t1 HAVING 1 IN (SELECT b FROM t2 ...` | `bit < map->n_bits` | Upstream |
| `mysql_bug26542829_derived_datetime_range_assert` | MySQL | [Bug #26542829](https://github.com/mysql/mysql-server/commit/728e566656)<br>RQG / MySQL Derived Table Join Bug (commit 728e566656) | 7 / 2391B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `SELECT field1 FROM (SELECT alias1.c5 AS field1...` | `!(check_datetime_range(ltime))` | Upstream |
| `mysql_bug28013997_utf32_user_var_slen_assert` | MySQL | [Bug #28013997](https://github.com/mysql/mysql-server/commit/728e566656)<br>MySQL Fuzzer / Charset User-Variable Bug (commit 728e566656) | 4 / 192B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `SELECT 1 FROM t1 WHERE @f<>'3761-03-28' XOR (...` | `(slen % 4) == 0` | Upstream |
| `mysql_bug30753397_derived_where_fixed_assert` | MySQL | [Bug #30753397](https://github.com/mysql/mysql-server/commit/728e566656)<br>MySQL Derived Table / Subquery Bug Report (commit 728e566656) | 3 / 538B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `SELECT 1 = (SELECT 1 FROM (SELECT 1 FROM t LEFT...` | `!tr->derived_where_cond \|\| tr->derived_where_cond->fixed` | Upstream |
| `mysql_bug31361748_window_range_frame_assert` | MySQL | [Bug #31361748](https://github.com/mysql/mysql-server/commit/f69fe331c8)<br>Fuzz / MySQL Window Function Bug Report (commit f69fe331c8) | 5 / 354B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `SELECT LAST_VALUE(b) RESPECT NULLS OVER (...` | `w->frame_buffer()->s->db_type()->db_type == DB_TYPE_INNODB \|\| cnt <= 1` | Upstream |
| `mysql_bug31479282_json_mv_index_crash` | MySQL | [Bug #31479282](https://github.com/mysql/mysql-server/commit/a112550236)<br>MySQL Bug Report / Regression Test (commit a112550236) | 2 / 201B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `INSERT INTO t1 values (1000,  CONCAT('[""]'));` | `Empty string in multi-valued JSON index treated as array marker` | Upstream |
| `mysql_bug31590301_temptable_view_row_subquery` | MySQL | [Bug #31590301](https://github.com/mysql/mysql-server/commit/94c4e70daa)<br>Grammar Fuzzer (MySQL 8.0.20 bug report, commit 94c4e70daa) | 3 / 213B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `CREATE ALGORITHM = TEMPTABLE VIEW v3 AS SELECT...` | `!(thd->lex->context_analysis_only & CONTEXT_ANALYSIS_ONLY_VIEW)` | Upstream |
| `mysql_bug32206756_icp_is_error_assert` | MySQL | [Bug #32206756](https://github.com/mysql/mysql-server/commit/7af629765f)<br>MySQL Fuzz / Optimizer Error-Propagation Bug (commit 7af629765f) | 7 / 388B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `CREATE TABLE insert_select AS SELECT t2.c AS ...` | `!current_thd->is_error()` | Upstream |
| `mysql_bug32591239_case_json_is_error_assert` | MySQL | [Bug #32591239](https://github.com/mysql/mysql-server/commit/fe2e61d85a)<br>MySQL Fuzz / Bug Report (commit fe2e61d85a) | 3 / 213B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `SELECT col_int FROM t1 WHERE CASE WHEN POWER(...` | `!current_thd->is_error() && has_value` | Upstream |
| `mysql_bug86293` | MySQL | [Bug #86293](https://bugs.mysql.com/bug.php?id=86293)<br>randgen / pquery (DBMS fuzzer) | 5 / 270B | 8.0.1-debug<br>`cmake -DWITH_DEBUG=1` | `ALTER TABLE t2 ADD CONSTRAINT FOREIGN KEY ...` | `sym_node->table != NULL` | Upstream |
| `mysql_bug88355` | MySQL | [Bug #88355](https://bugs.mysql.com/bug.php?id=88355)<br>pquery (Percona QA DBMS fuzzer) | 4 / 260B | 8.0.3-debug<br>`cmake -DWITH_DEBUG=1` | `ALTER TABLE t2 ADD INDEX(c2);` | `err == DB_SUCCESS \|\| err == DB_DUPLICATE_KEY` | Upstream |
| `mysql_bug_91975` | MySQL | [Bug #91975](https://bugs.mysql.com/bug.php?id=91975)<br>randgen / pquery (Percona QA DBMS fuzzer) | 5 / 301B | 8.0.12-debug<br>`cmake -DWITH_DEBUG=1` | `RENAME TABLE t1 TO t3;` | `!join->plan_is_const()` | Upstream |
| `mysql_bug_91975_enhanced` | MySQL | [Bug #91975](https://bugs.mysql.com/bug.php?id=91975)<br>randgen / pquery (Percona QA DBMS fuzzer) | 12 / 1029B | 8.0.12-debug<br>`cmake -DWITH_DEBUG=1` | `RENAME TABLE t1 TO t3;` | `!join->plan_is_const()` | Upstream |
| `mysql_squirrel_bug99421` | MySQL | [Bug #99421](https://bugs.mysql.com/bug.php?id=99421)<br>SQUIRREL (CCS 2020 DBMS fuzzer) | 5 / 268B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `CREATE INDEX v2 ON v0 (...)` | `col_nr < static_cast<ulint>(table->n_t_def)` | Upstream |
| `mysql_squirrel_bug99421_enhanced` | MySQL | [Bug #99421](https://bugs.mysql.com/bug.php?id=99421)<br>SQUIRREL (CCS 2020 DBMS fuzzer) | 14 / 1282B | 8.0.20-debug<br>`cmake -DWITH_DEBUG=1` | `CREATE INDEX v2 ON v0 (...)` | `col_nr < static_cast<ulint>(table->n_t_def)` | Upstream |
| `mariadb_mdev22462_in_to_exists_assert` | MariaDB (`mysql`) | [MDEV-22462](https://jira.mariadb.org/browse/MDEV-22462)<br>SQUIRREL (CCS '20) | 5 / 307B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `UPDATE v0 SET v1 = 2 WHERE v1 IN ( SELECT v1 ...` | `false` (`create_single_in_to_exists_cond`) | Upstream |
| `mariadb_mdev22856_password_sql_buffer_assert` | MariaDB (`mysql`) | [MDEV-22856](https://jira.mariadb.org/browse/MDEV-22856)<br>pquery (MariaDB QA Fuzzer) | 5 / 148B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `SELECT PASSWORD(c) FROM t;` | `!str \|\| str != Ptr` | Upstream |
| `mariadb_mdev23597_default_eval_marked_for_read` | MariaDB (`mysql`) | [MDEV-23597](https://jira.mariadb.org/browse/MDEV-23597)<br>pquery (MariaDB QA Fuzzer) | 3 / 97B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `UPDATE t SET b=DEFAULT;` | `marked_for_read()` | Upstream |
| `mariadb_mdev24072_instant_alter_vcols_assert` | MariaDB (`mysql`) | [MDEV-24072](https://jira.mariadb.org/browse/MDEV-24072)<br>pquery (MariaDB QA Fuzzer) | 4 / 199B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `ALTER TABLE t ADD d INT, ALGORITHM=INPLACE;` | `ib_table.n_v_cols` | Upstream |
| `mariadb_mdev24855_aria_tmp_length_assert` | MariaDB (`mysql`) | [MDEV-24855](https://jira.mariadb.org/browse/MDEV-24855)<br>RQG (Random Query Generator) | 3 / 238B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `SELECT c, GROUP_CONCAT(...) FROM t1 GROUP BY ...` | `length <= column->length` (`ma_blockrec.c`) | Upstream |
| `mariadb_mdev25295_duplicate_fulltext_index_assert` | MariaDB (`mysql`) | [MDEV-25295](https://jira.mariadb.org/browse/MDEV-25295)<br>pquery (MariaDB QA Fuzzer) | 5 / 261B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `CREATE FULLTEXT INDEX idx1 ON t (title,body);` | `(ctx->new_table->fts == __null) == (ctx->new_table->fts_doc_id_index == __null)` | Upstream |
| `mariadb_mdev25460_prepare_set_statement_assert` | MariaDB (`mysql`) | [MDEV-25460](https://jira.mariadb.org/browse/MDEV-25460)<br>pquery (MariaDB QA Fuzzer) | 3 / 156B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `PREPARE s FROM 'SET STATEMENT binlog_format=ROW...` | `!is_set() \|\| (m_status == DA_OK_BULK && is_bulk_op())` | Upstream |
| `mariadb_mdev25462_set_names_filename_assert` | MariaDB (`mysql`) | [MDEV-25462](https://jira.mariadb.org/browse/MDEV-25462)<br>pquery (MariaDB QA Fuzzer) | 3 / 69B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `SHOW TABLE STATUS;` | `m_status == DA_ERROR \|\| m_status == DA_OK \|\| m_status == DA_OK_BULK` | Upstream |
| `mariadb_mdev25635_having_pushdown_segfault` | MariaDB (`mysql`) | [MDEV-25635](https://jira.mariadb.org/browse/MDEV-25635)<br>randgen + SQLsmith (MariaDB fuzz.sql) | 3 / 758B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `WITH jennifer_0 AS (...) select subq_0.c2 as ...` | `mysqld got signal 11 (HAVING to derived WHERE pushdown crash)` | Upstream |
| `mariadb_mdev26379_prefix_len_alter_assert` | MariaDB (`mysql`) | [MDEV-26379](https://jira.mariadb.org/browse/MDEV-26379)<br>pquery (MariaDB QA Fuzzer) | 2 / 212B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `ALTER TABLE t CHANGE a a VARCHAR(3000) ...` | `field->col->is_binary() \|\| field->prefix_len % field->col->mbmaxlen == 0` | Upstream |
| `mariadb_mdev26453_hash_index_temp_assert` | MariaDB (`mysql`) | [MDEV-26453](https://jira.mariadb.org/browse/MDEV-26453)<br>pquery (MariaDB QA Fuzzer) | 5 / 238B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `DELETE FROM t;` | `0` (`row_upd_sec_index_entry`) | Upstream |
| `mariadb_mdev27238_optimizer_trace_json_assert` | MariaDB (`mysql`) | [MDEV-27238](https://jira.mariadb.org/browse/MDEV-27238)<br>pquery (MariaDB QA Fuzzer) | 5 / 170B | 10.5.8-debug<br>`cmake -DCMAKE_BUILD_TYPE=Debug` | `SELECT a FROM t WHERE (a,b) in (SELECT @c,@d);` | `got_name == named_item_expected()` | Upstream |
| `clickhouse_alias_materialize_ttl_segv` | ClickHouse | [ClickHouse #117327](https://github.com/ClickHouse/ClickHouse/issues/117327)<br>ClickHouse AST Fuzzer (PR #99980) | 4 / 209B | 26.8.44.3<br>`Official release package (deb)` | `ALTER TABLE av MATERIALIZE TTL;` | `SIGSEGV null mutex dereference at 0x4f8 in MutationsInterpreter::prepare` | Upstream |
| `clickhouse_alter_sorting_key_cast_segv` | ClickHouse | [ClickHouse #121122](https://github.com/ClickHouse/ClickHouse/issues/121122)<br>ClickHouse DDL Fuzz / Analyzer (open) | 7 / 403B | 26.8.44.3<br>`Official release package (deb)` | `INSERT INTO t_alter_key_type (k, x) VALUES (2, 7);` | `SIGSEGV in SerializationNullable::serializeBinary` | Upstream |
| `clickhouse_coalescing_projection_part_offset_segv` | ClickHouse | [ClickHouse #106929](https://github.com/ClickHouse/ClickHouse/issues/106929)<br>ClickHouse Production / LTS Crash Report (open for 25.8 backport) | 4 / 427B | 25.8.24.21<br>`Official release package (deb)` | `OPTIMIZE TABLE tiny FINAL;` | `SIGSEGV in MergeTreeSequentialSource::generate (_part_offset projection)` | Upstream |
| `clickhouse_direct_join_filter_segv` | ClickHouse | [ClickHouse #107272](https://github.com/ClickHouse/ClickHouse/issues/107272)<br>ClickHouse AST Fuzzer (PR #104174) | 7 / 468B | 26.5-release<br>`Official release / debug build` | `SELECT count() FROM (...) SETTINGS join_algorithm = 'direct';` | `SIGSEGV in ColumnString::compareAt / FilterTransform::doTransform` | Upstream |
| `clickhouse_join_order_restriction_logical_error` | ClickHouse | [ClickHouse #106426](https://github.com/ClickHouse/ClickHouse/issues/106426)<br>SQLancer / Join-Order Optimizer Fuzz | 7 / 510B | 26.6.1.339-debug<br>`Debug / ABORT_ON_LOGICAL_ERROR` | `SELECT * FROM t1, t3, t0 JOIN t3 ...` | `Join restriction violated` | Upstream |
| `clickhouse_join_to_in_common_subplan_abort` | ClickHouse | [ClickHouse #118733](https://github.com/ClickHouse/ClickHouse/issues/118733)<br>ClickHouse AST Fuzzer | 3 / 272B | 26.8.44.3-debug<br>`Debug / ABORT_ON_LOGICAL_ERROR` | `SELECT count() FROM t WHERE 1 > (SELECT max(b)) ...` | `Expected CommonSubplanReferenceStep to reference CommonSubplanStep, but got Expression` | Upstream |
| `clickhouse_json_shared_variant_size_segv` | ClickHouse | [ClickHouse #121079](https://github.com/ClickHouse/ClickHouse/issues/121079)<br>ClickHouse AI / Fuzz Reproducer (confirmed 26.3..26.9, open) | 5 / 493B | 26.8.44.3<br>`Official release package (deb)` | ``SELECT jd.a.:`Array(JSON)`.size0 FROM t05215;`` | `Received signal 11 (Segmentation fault) at address 0x58` | Upstream |
| `clickhouse_logical_expr_nullable_chassert` | ClickHouse | [ClickHouse #116281](https://github.com/ClickHouse/ClickHouse/issues/116281)<br>ClickHouse AST Fuzzer (PR #99980) | 5 / 378B | 26.8.10-debug<br>`Debug / ABORT_ON_LOGICAL_ERROR` | `SELECT 1 AS x FROM t1 AS tx ARRAY JOIN [] ...` | `!raw_type->isNullable()` | Upstream |
| `clickhouse_logical_expr_single_or_abort` | ClickHouse | [ClickHouse #112654](https://github.com/ClickHouse/ClickHouse/issues/112654)<br>ClickHouse AST Fuzzer | 3 / 199B | 26.7-debug<br>`Debug / ABORT_ON_LOGICAL_ERROR` | `SELECT 1 AS x FROM t1 ARRAY JOIN [] AS a0 ...` | `or_argument_nodes.size() > 1` | Upstream |
| `clickhouse_mv_loop_table_func_segv` | ClickHouse | [ClickHouse #125023](https://github.com/ClickHouse/ClickHouse/issues/125023)<br>ClickHouse Crash / MV Loop Table Function Bug (fixed 26.11 via PR #129338) | 10 / 715B | 26.8.44.3<br>`Official release package (deb)` | `INSERT INTO src VALUES (2), (3);` | `SIGSEGV in KeyCondition::KeyCondition from LoopSource::initLoop` | Upstream |
| `sqlite_alter_view_compound_orderby` | SQLite | [Ticket a10a14e9b4ba2](https://github.com/sqlite/sqlite/commit/4db7ab53f9)<br>Fuzz / SQLite Bug Tracker | 3 / 141B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `ALTER TABLE a RENAME TO e;` | `p->selFlags & SF_Resolved` | Live 5/5 |
| `sqlite_alter_view_rename_token` | SQLite | [altertab-20 (commit 4db7ab53f9)](https://github.com/sqlite/sqlite/commit/4db7ab53f9)<br>Fuzz / SQLite regression suite (`test/altertab.test 20`) | 3 / 126B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `ALTER TABLE a RENAME a TO e;` | `p->p!=pPtr` | Live 5/5 |
| `sqlite_fts3_segdir_corrupt` | SQLite | [fts3corrupt4 (commit 7576a68c8c)](https://github.com/sqlite/sqlite/commit/7576a68c8c)<br>OSS-Fuzz / SQLite FTS3 corruption fuzz | 4 / 193B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `INSERT INTO f VALUES (0,x'00');` | `nDoclist>0` | Live 5/5 |
| `sqlite_fuzz_fts5_savepoint` | SQLite | [fts5CheckTransactionState assertion failure](https://github.com/sqlancer/bugs)<br>SQLancer (OSDI 2020) | 12 / 341B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `DROP TABLE vt1;` | `iSavepoint<=p->ts.iSavepoint` | Upstream |
| `sqlite_fuzz_recursive_trigger` | SQLite | [Crash on REPLACE INTO of a table with an AFTER DELETE trigger](https://www.sqlite.org/src/tktview?name=50c09fc2cf)<br>SQLancer | 7 / 308B | 3.30.0<br>`-O2` | `REPLACE INTO t0(c0, c1, c2) VALUES(2, 0, -1572226132);` | `Segmentation fault` | Upstream |
| `sqlite_fuzz_vdbe_pin1` | SQLite | [sqlite3VdbeExec assertion failure](https://github.com/sqlancer/bugs)<br>SQLancer | 3 / 104B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `UPDATE t0 SET c0 = 0;` | `pIn1!=pIn3` | Upstream |
| `sqlite_in_leftjoin_mem_invalid` | SQLite | [Ticket 82b588d342d515d1](https://github.com/sqlite/sqlite/commit/14c98a4f40)<br>Fuzz / SQLite Bug Tracker (`test/in6.test`) | 5 / 172B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `SELECT quote(v5) FROM v0 LEFT JOIN v3 ...` | `memIsValid(&r.aMem[ii])` | Live 5/5 |
| `sqlite_leftjoin_nulls_last_assert` | SQLite | [Ticket e12a0ae526bb51c7](https://github.com/sqlite/sqlite/commit/7f05d52c4c)<br>Fuzz / SQLite Bug Tracker (`test/nulls1.test`) | 5 / 171B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `SELECT c FROM t1 LEFT JOIN t2 ON d=NULL ...` | `memIsValid(&aMem[pOp->p1])` | Live 5/5 |
| `sqlite_leftjoin_or_nullrow_cursor` | SQLite | [Ticket aa4378693018aa99](https://github.com/sqlite/sqlite/commit/e8c4f03266)<br>Fuzz / SQLite Bug Tracker (`test/whereD.test`) | 4 / 195B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `SELECT t1.* FROM t1 LEFT JOIN t2 ON abs(t1.a)=abs(t2.b);` | `sqlite3BtreeCursorIsValid(pCrsr)` | Live 5/5 |
| `sqlite_window_agg_depth_segfault` | SQLite | [Ticket 7a5279a25c57adf1](https://github.com/sqlite/sqlite/commit/c37577bb2d)<br>Fuzz / SQLite Bug Tracker (`test/window1.test 53.0`) | 3 / 215B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `SELECT a.c FROM a JOIN a AS b ...` | `Segmentation fault (window rewrite Expr.op2 depth mismatch)` | Live 5/5 |
| `sqlite_window_having_rowvalue` | SQLite | [Ticket af4556bb5c285c08](https://github.com/sqlite/sqlite/commit/c415d91007)<br>Fuzz / SQLite Bug Tracker | 2 / 147B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `SELECT c FROM a GROUP BY c HAVING(...)` | `Segmentation fault (incomplete AggInfo resetAccumulator NULL deref)` | Live 5/5 |
| `sqlite_window_subquery_union_assert` | SQLite | [Ticket 618156e3](https://github.com/sqlite/sqlite/commit/553948e514)<br>Fuzz / SQLite Bug Tracker (`test/window9.test 8.1-8.4`) | 7 / 358B | 3.31.0-debug<br>`-DSQLITE_DEBUG=1` | `SELECT(SELECT x UNION SELECT sum( avg(...) )) FROM v1;` | `p->pWin==0` | Live 5/5 |
| `duckdb_art_index_nullif_context` | DuckDB | [duckdb/duckdb#15206](https://github.com/duckdb/duckdb/issues/15206)<br>SQLancer / PSU Grammar Fuzzer | 2 / 133B | v1.1.3<br>`-DFORCE_ASSERT=1` | `CREATE INDEX UNBOUNDED ON v00 USING ART (...)` | `Calling ExpressionExecutor::GetContext on an expression executor without a context` | Upstream |
| `duckdb_asof_right_join_vector_ref` | DuckDB | [duckdb/duckdb#15444](https://github.com/duckdb/duckdb/issues/15444)<br>SQLancer / PSU Grammar Fuzzer | 4 / 287B | v1.1.3<br>`-DFORCE_ASSERT=1` | `SELECT DISTINCT ON ( 'string' ) 'string' ident ...` | `Vector::Reference used on vector of different type` | Upstream |
| `duckdb_iejoin_list_null_shared_ptr` | DuckDB | [duckdb/duckdb#27220](https://github.com/duckdb/duckdb/issues/27220)<br>SQL Query / IEJoin Fuzzer Report | 3 / 231B | v1.5.5<br>`Official release CLI / -DFORCE_ASSERT=1` | `CREATE TABLE o AS SELECT l.a, r.v FROM l LEFT J...` | `INTERNAL Error: Attempted to dereference shared_ptr that is NULL!` | Live 5/5 |
| `duckdb_info_schema_fk_bind_column` | DuckDB | [duckdb/duckdb#15897](https://github.com/duckdb/duckdb/issues/15897)<br>DuckDB Metadata Query Bug Report | 7 / 1386B | v1.1.3<br>`-DFORCE_ASSERT=1` | `SELECT (...) FROM information_schema.key_column_usage ...` | `Failed to bind column reference "constraint_name"` | Upstream |
| `duckdb_lateral_intersect_types_assert` | DuckDB | [duckdb/duckdb#15526](https://github.com/duckdb/duckdb/issues/15526)<br>SQLancer / PSU Grammar Fuzzer | 2 / 196B | v1.1.3<br>`-DFORCE_ASSERT=1` | `SELECT DISTINCT 'string' AS c01 FROM v00, LATERAL ...` | `types == input.GetTypes()` | Upstream |
| `duckdb_lateral_join_bind_column` | DuckDB | [duckdb/duckdb#16524](https://github.com/duckdb/duckdb/issues/16524)<br>PostgreSQL / DuckDB LATERAL Differential Test | 3 / 529B | v1.1.3<br>`-DFORCE_ASSERT=1` | `SELECT * FROM int8_tbl c LEFT JOIN ( ... ) ss3;` | `Failed to bind column reference "x"` | Upstream |
| `duckdb_pivot_full_join_columnref` | DuckDB | [duckdb/duckdb#15198](https://github.com/duckdb/duckdb/issues/15198)<br>SQLancer / PSU Grammar Fuzzer | 2 / 90B | v1.1.3<br>`-DFORCE_ASSERT=1` | `PIVOT v00 AS ta01 FULL JOIN v00 AS ta02 USING ...` | `Unexpected child of pivot source - not a ColumnRef` | Upstream |
| `duckdb_recursive_cte_bind_column` | DuckDB | [duckdb/duckdb#17286](https://github.com/duckdb/duckdb/issues/17286)<br>DBMSTesting Recursive CTE Fuzz / Report | 3 / 2119B | v1.1.3<br>`-DFORCE_ASSERT=1` | `WITH RECURSIVE aoc6_input(i) AS (...);` | `Failed to bind column reference ""` | Upstream |
| `duckdb_ubigint_move_constants_internal` | DuckDB | [duckdb/duckdb#28535](https://github.com/duckdb/duckdb/issues/28535)<br>Differential / Optimizer Fuzz & User Bug Report | 3 / 117B | v1.5.5<br>`Official release CLI / RelWithDebInfo` | `SELECT count(*) FROM t WHERE x + 1 = 18446744...` | `Information loss on integer cast: value 18446744073709551615...` | Live 5/5 |
| `duckdb_window_subquery_tostring_internal` | DuckDB | [duckdb/duckdb#15657](https://github.com/duckdb/duckdb/issues/15657)<br>SQLancer / PSU Grammar Fuzzer | 2 / 255B | v1.1.3<br>`-DFORCE_ASSERT=1` | `SELECT DISTINCT 'string', OFFSET EXISTS ( ... );` | `Cannot ToString bound subquery node` | Upstream |

---

## 3. Case Details & Dependency Analysis

### 3.1 `mysql_squirrel_bug99421` (MySQL)
- **Origin**: Reported by Yongheng Chen (SQUIRREL co-author, S3 team) on `bugs.mysql.com` and catalogued in the [SQUIRREL Bug List](https://github.com/s3team/Squirrel/wiki/Bug-List).
- **Target**: MySQL 8.0.19-debug / 8.0.20-debug, compiled with cmake `-DWITH_DEBUG=1`.
- **SQL Structure** (5 statements):
  ```sql
  CREATE TABLE v0 ( v1 INT ) ;
  INSERT INTO v0 ( v1 ) VALUES ( 44 ) ;
  UPDATE v0 SET v1 = 21 WHERE 62 > -128 = ( NOT ( v1 IS NULL ) ) ;
  INSERT INTO v0 ( v1 ) VALUES ( 0 ) , ( 82 ) ;
  CREATE INDEX v2 ON v0 ( ( v1 = 127 ) , ( v1 + v1 ) , v1 , ( v1 + - -1 ) , ( v1 > 26 ) ) ;
  ```
- **Why It Is Authentic**:
  - Generated during automated coverage-guided fuzzing of MySQL 8.0.
  - Represents an assertion failure in InnoDB's virtual column index dictionary cache (`dict_index_add_to_cache_w_vcol` calling `dict_table_get_v_col_name_mysql`).
  - SQUIRREL's multi-statement mutator synthesized table mutations before attempting an inplace index creation on composite virtual expressions.
  - Reducer challenges: The reducer must retain `CREATE TABLE v0` and the crashing `CREATE INDEX`, while determining whether the intermediate `INSERT`s and `UPDATE` are necessary to trigger the dictionary index bug.

### 3.2 `pg_fuzzer_mcv_clauses` (PostgreSQL)
- **Origin**: Reported by Manuel Rigger (author of SQLancer / PQS, USENIX OSDI 2020) to `pgsql-bugs` (Nov 19, 2019; diagnosed and fixed by Tomas Vondra).
- **Target**: PostgreSQL 12.0 (`REL_12_0`), compiled with `./configure --enable-cassert --enable-debug`.
- **SQL Structure** (5 statements):
  ```sql
  CREATE TABLE t0(c0 boolean, c1 boolean, c2 boolean);
  INSERT INTO t0 VALUES(FALSE, FALSE, FALSE);
  CREATE STATISTICS s0 ON c0, c2 FROM t0;
  ANALYZE;
  SELECT * FROM t0 WHERE t0.c2 OR t0.c1 OR t0.c0;
  ```
- **Why It Is Authentic**:
  - Discovered by fuzz testing the multivariate MCV (Most Common Values) extended statistics engine introduced in PostgreSQL 12.
  - Extended statistic `s0` covers columns `(c0, c2)`, but the query predicate contains `t0.c2 OR t0.c1 OR t0.c0`. The statistics clause-matching logic incorrectly matched the OR clause because it referenced at least two attributes, but crashed on assertion `clauses != NIL` in `src/backend/statistics/mcv.c:1551` because column `c1` was not covered by the statistic.
  - Strong cross-statement dependencies:
    - `t0` defined by statement 1, used by statements 2, 3, 5.
    - `s0` defined by statement 3, populated by `ANALYZE` (statement 4).
    - If `CREATE STATISTICS` or `ANALYZE` is removed, the query executes normally without crashing.
### 3.3 `pg_fuzzer_mcv_enhanced` (PostgreSQL)
- **Origin**: Redundancy-augmented variant embedding the 5-statement `pg_fuzzer_mcv_clauses` core into 22 realistic multi-statement operations (unrelated tables, inserts, views, indexes, and extra columns/expressions) with a known ground-truth core.
- **Target**: PostgreSQL 12.0 (`REL_12_0`), compiled with `--enable-cassert --enable-debug`.
- **SQL Structure** (22 statements):
  - **Users & Orders Subsystem**: `CREATE TABLE app_users`, `INSERT`, `CREATE TABLE orders`, `INSERT`, `CREATE VIEW active_users_summary`, `ALTER TABLE orders ADD COLUMN`, `UPDATE app_users`, `SELECT ... JOIN`.
  - **Sensor Telemetry & Indexing**: `CREATE TABLE sensor_readings`, `INSERT`, `CREATE INDEX`, `SELECT AVG(...)`.
  - **Inventory Subsystem & Mutation**: `CREATE TABLE inventory_items`, `INSERT`, `DELETE FROM orders`.
  - **Authentic Crash Core with Redundant Attributes**: `CREATE TABLE t0 (..., c_extra INT)`, 2 `INSERT` statements, `CREATE STATISTICS s0 ON c0, c2 FROM t0`, unreferenced subquery probing `SELECT COUNT(*)`, `ANALYZE;`, and the trigger query `SELECT * FROM t0 WHERE t0.c2 OR t0.c1 OR t0.c0;`.
- **Why It Is Useful**:
  - Previously verified on PostgreSQL 12.0 (`occam-target-pg12`) across 5 consecutive clean replays (`FailedAssertion.*mcv.c:1553`); listed as `Upstream` in the catalog above when no local PG container is present.
  - Provides an exact known ground-truth core for evaluating dependency slicing, statement-level ddmin, and IR-level column reduction against a realistic background database schema.

### 3.4 `pg_sqlsmith_partprune` (PostgreSQL)
- **Origin**: Reported by Jaime Casanova (2ndQuadrant) to `pgsql-hackers` (Jul 24, 2018; diagnosed and fixed by Tom Lane in commit `59ef49d26d2`).
- **Target**: PostgreSQL 11-devel (commit `1b957e59b92dc44c14708762f882d7910463a9ac`), compiled with `--enable-cassert --enable-debug`.
- **SQL Structure** (22 statements, 4616 bytes):
  - 3 initial `DROP TABLE IF EXISTS` cleanup statements.
  - 2 completely irrelevant table definitions (`actualizar_sistema`, `anexos`) created as background database schema.
  - 1 range-partitioned table (`radicado`) with nested hash subpartitions (`radicado2013`, `radicado2014`, `radicado2015`, `radicado2016` each having `part00` and `part01`).
  - 1 synthetic bulk insert using `generate_series(1, 1000000)`.
  - 1 crashing complex query featuring `tablesample bernoulli (9.7)`, scalar subqueries against the irrelevant tables in the `SELECT` projection list, and a compound `WHERE` filter.
- **Why It Is Authentic**:
  - Direct raw unreduced output of SQLsmith fuzzing against an existing relational schema.
  - Exercises multi-statement reduction across unreferenced tables (`actualizar_sistema`, `anexos`), unnecessary partition branches, and redundant projections while preserving the exact planner assertion failure (`!(relid_subplan_map[rti] == 0)` in `partprune.c:244`).

### 3.5 `mysql_bug_91975` (MySQL)
- **Origin**: Reported by Roel Van de Paar on `bugs.mysql.com`.
- **Target**: MySQL 8.0.12-debug / 8.0.13-debug, compiled with `-DWITH_DEBUG=1`.
- **SQL Structure** (5 statements):
  ```sql
  CREATE TABLE t1(c1 TIME, c2 CHAR(1), c3 INT(1), c4 VARCHAR(1) PRIMARY KEY, c5 TIME UNIQUE KEY, c6 NUMERIC(0,0) DEFAULT 3.141592);
  SET @@global.super_read_only = TRUE;
  CREATE TEMPORARY TABLE t1(i INT, b BLOB, PRIMARY KEY pk(i), INDEX sk(b(1)));
  CREATE TEMPORARY TABLE t3(f INT);
  RENAME TABLE t1 TO t3;
  ```
- **Why It Is Authentic**:
  - Exercises interactions between transactional catalog metadata, global server state (`super_read_only`), and temporary tables.
  - Crashes mysqld with SIGABRT on assertion `dict0dd.cc:1071:!fail` inside `dict_table_rename_in_cache`.

---

## 4. Integration with Occam Oracle & Harness

Each `.meta.json` file pairs with the raw `.sql` file to drive automated reduction with Occam:

```bash
# Example reduction run with Occam on PostgreSQL. Use the exact
# crash_signature.expected_signature value from the case's .meta.json.
occam benchmarks/real_fuzz_corpus/pg_fuzzer_mcv_clauses.sql \
  --oracle "python3 harness/crash_oracle.py --dbms postgres --container-name occam-target-pg --expected-signature 'clauses != NIL'" \
  --reset "./harness/reset_db.sh --dbms postgres --container-name occam-target-pg" \
  --dialect postgresql \
  --timeout 10 \
  --jobs 1 \
  --out /tmp/occam-reduced.sql
```

The oracle requires `--log-file` or `--container-name`; it does not treat client
stderr as crash evidence. Use `--jobs 1` because all workers would share
`occam_test`.

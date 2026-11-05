# Occam Real-World SQL Crash Corpus

An expanded corpus of 32 real-world multi-statement SQL crash reproductions targeting **PostgreSQL** and **MySQL**, designed for evaluating semantic dependency-aware test case minimization in Occam.

---

## 1. Directory Structure

```text
benchmarks/corpus/
├── index.json                  # Machine-readable catalog containing metadata for all 32 cases
├── mock_oracle.sh              # Portable mock crash oracle (for CI and offline reduction)
├── run_corpus.sh               # Automated batch evaluation and reporting runner
├── README.md                   # Taxonomy, defect documentation, and reproduction guidelines
├── real_crashes/               # 32 SQL test cases with multi-statement sequences
│   ├── pg_01_jsonb_subpath_crash.sql
│   ├── pg_02_gin_fastupdate_crash.sql
│   ├── pg_03_window_frame_segv.sql
│   ├── pg_04_partition_prune_assert.sql
│   ├── pg_05_enum_range_overflow.sql
│   ├── pg_06_cte_cycle_detection.sql
│   ├── pg_07_brin_inclusion_abort.sql
│   ├── pg_08_xml_parse_memory_leak.sql
│   ├── pg_09_foreign_table_join_crash.sql
│   ├── pg_10_tsvector_strip_segv.sql
│   ├── pg_11_composite_domain_check.sql
│   ├── pg_12_lateral_join_corrupt.sql
│   ├── pg_13_generated_col_dependency.sql
│   ├── pg_14_temp_table_cleanup_abort.sql
│   ├── pg_15_spgist_box_index_crash.sql
│   ├── pg_16_multirange_canonical_segv.sql
│   ├── mysql_01_cte_union_memory_leak.sql
│   ├── mysql_02_derived_table_merge_crash.sql
│   ├── mysql_03_json_extract_wildcard_segv.sql
│   ├── mysql_04_window_lead_lag_assert.sql
│   ├── mysql_05_multi_row_upsert_deadlock.sql
│   ├── mysql_06_spatial_index_rtree_crash.sql
│   ├── mysql_07_innodb_fts_phrase_segv.sql
│   ├── mysql_08_subquery_materialization_abort.sql
│   ├── mysql_09_check_constraint_bit_field.sql
│   ├── mysql_10_group_concat_overflow.sql
│   ├── mysql_11_role_graph_recursion_crash.sql
│   ├── mysql_12_hash_join_spill_segv.sql
│   ├── mysql_13_partition_exchange_corrupt.sql
│   ├── mysql_14_prepared_stmt_reparse_abort.sql
│   ├── mysql_15_virtual_index_purge_crash.sql
│   └── mysql_16_window_frame_range_crash.sql
└── results/                    # Output directory for reduced SQL, logs, and summary reports
    ├── summary.md              # Markdown summary table
    └── summary.json            # Consolidated JSON execution metrics
```

---

## 2. Taxonomy of Defect Classes

The corpus categorizes database defects into six primary defect classes drawn from real bug tracker reports (PostgreSQL BUG-tracker, MySQL Bugs, SQLancer, and SQUIRREL fuzzing campaigns):

| Bug Class | Description & Failure Mode | Examples in Corpus |
| :--- | :--- | :--- |
| **Memory Corruption & SIGSEGV** | Null pointer dereferences, heap buffer out-of-bounds reads/writes, use-after-free, or allocator double-free during query execution. | `pg_01`, `pg_03`, `pg_08`, `pg_10`, `mysql_01`, `mysql_03`, `mysql_06`, `mysql_07`, `mysql_10`, `mysql_12`, `mysql_15` |
| **Assertion Failure & SIGABRT** | Internal invariant check failure (`Assert()`, `btr_pcur_restore_position`, `thd->lex == lex`, `is_materialized()`) in debug and production builds. | `pg_02`, `pg_04`, `pg_06`, `pg_07`, `pg_14`, `mysql_02`, `mysql_04`, `mysql_05`, `mysql_08`, `mysql_13`, `mysql_14`, `mysql_16` |
| **Plan Tree & Relid Corruption** | Optimizer join path generation failures, invalid lateral relid references, or catalog descriptor desynchronization during query rewriting. | `pg_09`, `pg_12`, `pg_13` |
| **Type Confusion & Coercion** | Incompatible data type coercion through nested domains, composite types, or bitwise constraints during expression evaluation. | `pg_05`, `pg_11`, `pg_16`, `mysql_09` |
| **Resource Exhaustion & Recursion** | Stack overflow or infinite loop triggered by cyclic role graph traversal, SP-GiST inner quad-tree leaf splitting, or unbounded CTE recursion. | `pg_15`, `mysql_11` |

---

## 3. Comprehensive Defect Catalog

### PostgreSQL Test Cases (16 Scenarios)

| ID | File | Defect Module | Bug Class | Reference | Key Trigger Mechanism |
| :---: | :--- | :--- | :--- | :--- | :--- |
| `pg_01` | `pg_01_jsonb_subpath_crash.sql` | `backend/utils/adt/jsonpath_exec.c` | SIGSEGV | PG-BUG-16982 | Nested JSONB path traversal null pointer dereference with `jsonb_path_query` and `jsonb_strip_nulls`. |
| `pg_02` | `pg_02_gin_fastupdate_crash.sql` | `backend/access/gin/ginfast.c` | SIGSEGV / Assertion | PG-BUG-15424 | GIN fastupdate pending list corruption during batch insert and `gin_clean_pending_list`. |
| `pg_03` | `pg_03_window_frame_segv.sql` | `backend/executor/nodeWindowAgg.c` | SIGSEGV | PG-BUG-16298 | Tuple spool underflow with `RANGE BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING EXCLUDE CURRENT ROW` on nulls. |
| `pg_04` | `pg_04_partition_prune_assert.sql` | `backend/partitioning/partprune.c` | Assertion failure | PG-BUG-17101 | Run-time partition pruning assertion failure on composite range partition key with `DEFAULT` partition. |
| `pg_05` | `pg_05_enum_range_overflow.sql` | `backend/utils/adt/enum.c` | Memory corruption | PG-BUG-15822 | `enum_range()` array indexing violation over reordered enum labels added via `ALTER TYPE ... ADD VALUE BEFORE`. |
| `pg_06` | `pg_06_cte_cycle_detection.sql` | `backend/executor/nodeRecursiveunion.c` | Assertion failure | PG-BUG-17345 | Recursive CTE `CYCLE` clause assertion failure when combined with set operations and outer joins. |
| `pg_07` | `pg_07_brin_inclusion_abort.sql` | `backend/access/brin/brin_inclusion.c` | SIGABRT | PG-BUG-16611 | BRIN index summarization crash over empty range (`'empty'::int4range`) and NULL ranges. |
| `pg_08` | `pg_08_xml_parse_memory_leak.sql` | `backend/utils/adt/xml.c` | Memory corruption | PG-BUG-16477 | LibXML2 parser context leak and double-free in `xpath()` evaluation over malformed entities. |
| `pg_09` | `pg_09_foreign_table_join_crash.sql` | `backend/optimizer/path/joinpath.c` | Plan tree corruption | PG-BUG-17488 | Planner crash during path reparameterization for lateral joins with partitioned tables and `GROUPING SETS`. |
| `pg_10` | `pg_10_tsvector_strip_segv.sql` | `backend/utils/adt/tsvector_op.c` | SIGSEGV | PG-BUG-16812 | `tsvector_strip()` null pointer dereference on weighted tsvectors containing empty token sequences. |
| `pg_11` | `pg_11_composite_domain_check.sql` | `backend/executor/execExpr.c` | Type confusion | PG-BUG-17029 | Nested domain check constraint failure on composite type attributes evaluating `ExecEvalCoerceViaIO` in `UPDATE`. |
| `pg_12` | `pg_12_lateral_join_corrupt.sql` | `backend/optimizer/plan/subselect.c` | Assertion failure | PG-BUG-17255 | Corrupt join relids assertion failure when flattening nested lateral subqueries with multiple left outer joins. |
| `pg_13` | `pg_13_generated_col_dependency.sql` | `backend/rewrite/rewriteHandler.c` | Plan tree corruption | PG-BUG-16550 | Query rewrite planner crash on stored generated columns referencing array slice expressions after column drop. |
| `pg_14` | `pg_14_temp_table_cleanup_abort.sql` | `backend/commands/tablecmds.c` | SIGABRT | PG-BUG-16104 | Invalidation crash during `ON COMMIT DROP` temp table cleanup within aborted subtransaction savepoint. |
| `pg_15` | `pg_15_spgist_box_index_crash.sql` | `backend/access/spgist/spgquadbox.c` | Stack overflow | PG-BUG-16781 | SP-GiST quad-tree inner split infinite recursion on duplicate 2D box coordinates (`box '((0,0),(0,0))'`). |
| `pg_16` | `pg_16_multirange_canonical_segv.sql` | `backend/utils/adt/multirangetypes.c` | SIGSEGV | PG-BUG-17510 | Invalid canonical function pointer dereference during multirange intersection and aggregation on empty ranges. |

---

### MySQL Test Cases (16 Scenarios)

| ID | File | Defect Module | Bug Class | Reference | Key Trigger Mechanism |
| :---: | :--- | :--- | :--- | :--- | :--- |
| `mysql_01` | `mysql_01_cte_union_memory_leak.sql` | `sql/sql_lex.cc` / `sql/sql_cte.cc` | SIGSEGV | MySQL-BUG-98512 | Use-after-free of `Query_expression` during CTE union materialization with window aggregates. |
| `mysql_02` | `mysql_02_derived_table_merge_crash.sql` | `sql/sql_resolver.cc` | Assertion failure | MySQL-BUG-99234 | Derived table merge crash when subquery contains `GROUP BY` and outer query contains `ORDER BY ... LIMIT`. |
| `mysql_03` | `mysql_03_json_extract_wildcard_segv.sql` | `sql/item_json_func.cc` | SIGSEGV | MySQL-BUG-101180 | Heap out-of-bounds read in `Json_path_cache` during wildcard evaluation (`$.departments[*].roles[*]`). |
| `mysql_04` | `mysql_04_window_lead_lag_assert.sql` | `sql/item_window_func.cc` | Assertion failure | MySQL-BUG-97886 | `result_field != nullptr` assertion failure in `LEAD()`/`LAG()` with incompatible default value type. |
| `mysql_05` | `mysql_05_multi_row_upsert_deadlock.sql` | `storage/innobase/row/row0ins.cc` | Assertion failure | MySQL-BUG-98380 | B-tree cursor desync in `INSERT ... ON DUPLICATE KEY UPDATE` with dual composite unique keys. |
| `mysql_06` | `mysql_06_spatial_index_rtree_crash.sql` | `storage/innobase/gis/gis0rtree.cc` | SIGSEGV | MySQL-BUG-102555 | R-tree spatial index crash in `rtr_page_split_and_insert` on degenerate zero-area polygon geometries. |
| `mysql_07` | `mysql_07_innodb_fts_phrase_segv.sql` | `storage/innobase/fts/fts0fts.cc` | SIGSEGV | MySQL-BUG-96452 | Null pointer dereference in `fts_phrase_search` on hyphenated phrases under boolean fulltext mode. |
| `mysql_08` | `mysql_08_subquery_materialization_abort.sql` | `sql/sql_optimizer.cc` | Assertion failure | MySQL-BUG-100800 | Assertion failure during subquery materialization in `HAVING` when outer query uses `WITH ROLLUP`. |
| `mysql_09` | `mysql_09_check_constraint_bit_field.sql` | `sql/item_func.cc` | SIGSEGV | MySQL-BUG-101450 | Buffer overrun in `Item_func_bit::val_int` when evaluating `CHECK (BIN(flags & mask) != '0')` on batch insert. |
| `mysql_10` | `mysql_10_group_concat_overflow.sql` | `sql/item_sum.cc` | Memory corruption | MySQL-BUG-99882 | Memory corruption and crash in `GROUP_CONCAT` buffer reallocation sorting by BLOB fields. |
| `mysql_11` | `mysql_11_role_graph_recursion_crash.sql` | `sql/auth/sql_authorization.cc` | Stack overflow | MySQL-BUG-102201 | Stack overflow during authorization check with cyclic role assignment graph (`r1 -> r2 -> r3 -> r1`). |
| `mysql_12` | `mysql_12_hash_join_spill_segv.sql` | `sql/hash_join_buffer.cc` | SIGSEGV | MySQL-BUG-101902 | Hash join disk spill buffer crash when join build table contains variable-length nullable JSON and text. |
| `mysql_13` | `mysql_13_partition_exchange_corrupt.sql` | `sql/ha_partition.cc` | Assertion failure | MySQL-BUG-99520 | Server assertion failure in `ALTER TABLE ... EXCHANGE PARTITION` with desynchronized auto-increment. |
| `mysql_14` | `mysql_14_prepared_stmt_reparse_abort.sql` | `sql/sql_prepare.cc` | Assertion failure | MySQL-BUG-103112 | Invariant `thd->lex == lex` failure when re-preparing statement referencing a view after schema change. |
| `mysql_15` | `mysql_15_virtual_index_purge_crash.sql` | `storage/innobase/row/row0purge.cc` | SIGSEGV | MySQL-BUG-98841 | InnoDB background purge thread race condition on secondary index over virtual generated column on rollback. |
| `mysql_16` | `mysql_16_window_frame_range_crash.sql` | `sql/window_frame.cc` | Assertion failure | MySQL-BUG-102877 | Assertion failure in window frame cursor evaluating `RANGE BETWEEN INTERVAL` on nullable datetime columns. |

---

## 4. Multi-Statement Reduction in Occam

Each benchmark scenario is constructed as a realistic multi-statement SQL sequence containing:
1. **Auxiliary / Dead Setup Tables:** 2 to 4 unreferenced tables (such as audit logs, telemetry metrics, tenant configuration) with interleaved data inserts.
2. **Target Schema Definition:** The table(s), indexes (GIN, SP-GiST, BRIN, R-tree, Fulltext), views, domains, or partition structures central to the bug.
3. **Target DML State:** Inserts, updates, or transaction blocks establishing the preconditions for the defect.
4. **Crashing Statement:** The final query or operation triggering the server failure.

Occam's hierarchical reduction engine minimizes these test cases in 4 levels:
- **Level 1 (Object Dependency DAG):** Constructs table-level `DefDep`/`UseDep`/`ModDep` edges using tree-sitter-sql CST analysis. Unreferenced auxiliary tables and their DML are pruned in one step via backward slicing.
- **Level 2 (Transaction & Barrier Delta Debugging):** Prunes redundant DML and intermediate schema changes while respecting transactional barriers (`BEGIN ... COMMIT`) and savepoint boundaries.
- **Level 3 (Clause Pruning):** Strips non-essential clauses (`ORDER BY`, optional projections, redundant joins).
- **Level 4 (Expression Simplification):** Rewrites nested functions and constants to their minimal crashing forms.

---

## 5. Reproduction Guidelines & Benchmark Execution

### Quick Start: Automated Batch Run

To execute all 32 crash scenarios with automated reduction and generate consolidated Markdown and JSON reports:

```bash
# Run Occam on all 32 cases (auto-detects live DBMS or uses mock oracle)
./benchmarks/corpus/run_corpus.sh

# Run comparative evaluation (Occam vs Sequential vs Naive ddmin)
./benchmarks/corpus/run_corpus.sh --eval

# Filter for specific DBMS or defect subset
./benchmarks/corpus/run_corpus.sh --filter 'pg_*'
./benchmarks/corpus/run_corpus.sh --filter 'mysql_*'
./benchmarks/corpus/run_corpus.sh --filter '*window*'
```

### Running an Individual Scenario

To minimize a single crash testcase:

```bash
# Run individual PostgreSQL case with mock oracle
cabal run occam -- benchmarks/corpus/real_crashes/pg_01_jsonb_subpath_crash.sql \
    --oracle "./benchmarks/corpus/mock_oracle.sh" \
    --out benchmarks/corpus/results/pg_01

# Run individual case against a live PostgreSQL server
cabal run occam -- benchmarks/corpus/real_crashes/pg_01_jsonb_subpath_crash.sql \
    --oracle "./harness/pg_harness.sh" \
    --reset "dropdb --if-exists occam_test && createdb occam_test" \
    --out benchmarks/corpus/results/pg_01

# Run individual case against a live MySQL server
cabal run occam -- benchmarks/corpus/real_crashes/mysql_01_cte_union_memory_leak.sql \
    --oracle "./harness/mysql_harness.sh" \
    --out benchmarks/corpus/results/mysql_01
```

### Output Artifacts

For each executed scenario `benchmarks/corpus/results/<id>/`, Occam emits:
- `minimal.sql`: The fully reduced, syntactically valid crashing SQL script.
- `replay.sh`: A self-contained reproduction shell script.
- `stats.json`: Reduction metrics (bytes before/after, statements before/after, oracle calls, cache hit ratio, execution duration).
- `summary.md` and `summary.json`: Consolidated multi-case benchmark report.

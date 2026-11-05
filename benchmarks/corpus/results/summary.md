# Occam Real-World Crash Corpus Execution Summary

- **Tool Executed:** `occam`
- **Total Scenarios:** 32
- **Passed:** 32
- **Failed:** 0
- **Overall Byte Reduction:** 35.8%
- **Overall Statement Reduction:** 58.5%

## Per-Scenario Reduction Results

| Scenario ID | DBMS | Status | Stmts (Init -> Red) | Stmt Red % | Bytes (Init -> Red) | Byte Red % | Time (s) |
| :--- | :---: | :---: | :---: | ---: | :---: | ---: | ---: |
| `mysql_01_cte_union_memory_leak` | mysql | **PASSED** | 9 -> 5 | 44.4% | 2282 -> 1663 | 27.1% | 1.789s |
| `mysql_02_derived_table_merge_crash` | mysql | **PASSED** | 9 -> 5 | 44.4% | 2138 -> 1519 | 29.0% | 3.672s |
| `mysql_03_json_extract_wildcard_segv` | mysql | **PASSED** | 7 -> 3 | 57.1% | 2153 -> 1440 | 33.1% | 1.851s |
| `mysql_04_window_lead_lag_assert` | mysql | **PASSED** | 7 -> 3 | 57.1% | 1895 -> 1341 | 29.2% | 1.977s |
| `mysql_05_multi_row_upsert_deadlock` | mysql | **PASSED** | 8 -> 3 | 62.5% | 1929 -> 1387 | 28.1% | 0.799s |
| `mysql_06_spatial_index_rtree_crash` | mysql | **PASSED** | 7 -> 3 | 57.1% | 1979 -> 1481 | 25.2% | 1.491s |
| `mysql_07_innodb_fts_phrase_segv` | mysql | **PASSED** | 7 -> 2 | 71.4% | 1996 -> 935 | 53.2% | 1.598s |
| `mysql_08_subquery_materialization_abort` | mysql | **PASSED** | 9 -> 3 | 66.7% | 2054 -> 951 | 53.7% | 3.968s |
| `mysql_09_check_constraint_bit_field` | mysql | **PASSED** | 7 -> 2 | 71.4% | 1710 -> 1055 | 38.3% | 1.598s |
| `mysql_10_group_concat_overflow` | mysql | **PASSED** | 7 -> 3 | 57.1% | 1565 -> 1095 | 30.0% | 1.97s |
| `mysql_11_role_graph_recursion_crash` | mysql | **PASSED** | 11 -> 7 | 36.4% | 1623 -> 1135 | 30.1% | 3.144s |
| `mysql_12_hash_join_spill_segv` | mysql | **PASSED** | 9 -> 5 | 44.4% | 2222 -> 1676 | 24.6% | 3.19s |
| `mysql_13_partition_exchange_corrupt` | mysql | **PASSED** | 13 -> 6 | 53.8% | 2112 -> 1266 | 40.1% | 2.682s |
| `mysql_14_prepared_stmt_reparse_abort` | mysql | **PASSED** | 12 -> 4 | 66.7% | 1835 -> 930 | 49.3% | 3.05s |
| `mysql_15_virtual_index_purge_crash` | mysql | **PASSED** | 11 -> 6 | 45.5% | 1933 -> 1453 | 24.8% | 4.575s |
| `mysql_16_window_frame_range_crash` | mysql | **PASSED** | 7 -> 2 | 71.4% | 1853 -> 1019 | 45.0% | 2.118s |
| `pg_01_jsonb_subpath_crash` | postgresql | **PASSED** | 9 -> 4 | 55.6% | 2665 -> 1621 | 39.2% | 2.469s |
| `pg_02_gin_fastupdate_crash` | postgresql | **PASSED** | 10 -> 6 | 40.0% | 2040 -> 1349 | 33.9% | 5.136s |
| `pg_03_window_frame_segv` | postgresql | **PASSED** | 7 -> 3 | 57.1% | 2175 -> 1577 | 27.5% | 1.622s |
| `pg_04_partition_prune_assert` | postgresql | **PASSED** | 6 -> 2 | 66.7% | 2150 -> 1618 | 24.7% | 1.156s |
| `pg_05_enum_range_overflow` | postgresql | **PASSED** | 9 -> 5 | 44.4% | 1933 -> 1317 | 31.9% | 2.211s |
| `pg_06_cte_cycle_detection` | postgresql | **PASSED** | 9 -> 3 | 66.7% | 2086 -> 1280 | 38.6% | 3.412s |
| `pg_07_brin_inclusion_abort` | postgresql | **PASSED** | 10 -> 4 | 60.0% | 1814 -> 921 | 49.2% | 3.444s |
| `pg_08_xml_parse_memory_leak` | postgresql | **PASSED** | 8 -> 2 | 75.0% | 2097 -> 976 | 53.5% | 1.976s |
| `pg_09_foreign_table_join_crash` | postgresql | **PASSED** | 6 -> 2 | 66.7% | 2426 -> 1931 | 20.4% | 1.181s |
| `pg_10_tsvector_strip_segv` | postgresql | **PASSED** | 8 -> 3 | 62.5% | 2039 -> 1275 | 37.5% | 2.199s |
| `pg_11_composite_domain_check` | postgresql | **PASSED** | 10 -> 5 | 50.0% | 1848 -> 1191 | 35.6% | 2.557s |
| `pg_12_lateral_join_corrupt` | postgresql | **PASSED** | 11 -> 4 | 63.6% | 1862 -> 1089 | 41.5% | 6.876s |
| `pg_13_generated_col_dependency` | postgresql | **PASSED** | 9 -> 2 | 77.8% | 1814 -> 1012 | 44.2% | 2.641s |
| `pg_14_temp_table_cleanup_abort` | postgresql | **PASSED** | 8 -> 2 | 75.0% | 1489 -> 1005 | 32.5% | 1.215s |
| `pg_15_spgist_box_index_crash` | postgresql | **PASSED** | 8 -> 2 | 75.0% | 1788 -> 909 | 49.2% | 2.299s |
| `pg_16_multirange_canonical_segv` | postgresql | **PASSED** | 7 -> 3 | 57.1% | 1667 -> 1155 | 30.7% | 1.941s |

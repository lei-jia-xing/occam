# Occam Real-World Crash Corpus Execution Summary

- **Tool Executed:** `occam`
- **Total Scenarios:** 32
- **Passed:** 32
- **Failed:** 0
- **Overall Byte Reduction:** 63.3%
- **Overall Statement Reduction:** 88.4%

## Per-Scenario Reduction Results

| Scenario ID | DBMS | Status | Stmts (Init -> Red) | Stmt Red % | Bytes (Init -> Red) | Byte Red % | Time (s) |
| :--- | :---: | :---: | :---: | ---: | :---: | ---: | ---: |
| `mysql_01_cte_union_memory_leak` | mysql | **PASSED** | 9 -> 1 | 88.9% | 2282 -> 750 | 67.1% | 0.117s |
| `mysql_02_derived_table_merge_crash` | mysql | **PASSED** | 9 -> 1 | 88.9% | 2138 -> 734 | 65.7% | 0.121s |
| `mysql_03_json_extract_wildcard_segv` | mysql | **PASSED** | 7 -> 1 | 85.7% | 2153 -> 748 | 65.3% | 0.107s |
| `mysql_04_window_lead_lag_assert` | mysql | **PASSED** | 7 -> 1 | 85.7% | 1895 -> 727 | 61.6% | 0.103s |
| `mysql_05_multi_row_upsert_deadlock` | mysql | **PASSED** | 8 -> 1 | 87.5% | 1929 -> 783 | 59.4% | 0.098s |
| `mysql_06_spatial_index_rtree_crash` | mysql | **PASSED** | 7 -> 1 | 85.7% | 1979 -> 777 | 60.7% | 0.108s |
| `mysql_07_innodb_fts_phrase_segv` | mysql | **PASSED** | 7 -> 1 | 85.7% | 1996 -> 674 | 66.2% | 0.108s |
| `mysql_08_subquery_materialization_abort` | mysql | **PASSED** | 9 -> 1 | 88.9% | 2054 -> 750 | 63.5% | 0.093s |
| `mysql_09_check_constraint_bit_field` | mysql | **PASSED** | 7 -> 1 | 85.7% | 1710 -> 704 | 58.8% | 0.077s |
| `mysql_10_group_concat_overflow` | mysql | **PASSED** | 7 -> 1 | 85.7% | 1565 -> 682 | 56.4% | 0.088s |
| `mysql_11_role_graph_recursion_crash` | mysql | **PASSED** | 11 -> 1 | 90.9% | 1623 -> 665 | 59.0% | 0.098s |
| `mysql_12_hash_join_spill_segv` | mysql | **PASSED** | 9 -> 1 | 88.9% | 2222 -> 714 | 67.9% | 0.084s |
| `mysql_13_partition_exchange_corrupt` | mysql | **PASSED** | 13 -> 1 | 92.3% | 2112 -> 779 | 63.1% | 0.079s |
| `mysql_14_prepared_stmt_reparse_abort` | mysql | **PASSED** | 12 -> 1 | 91.7% | 1835 -> 745 | 59.4% | 0.074s |
| `mysql_15_virtual_index_purge_crash` | mysql | **PASSED** | 11 -> 1 | 90.9% | 1933 -> 845 | 56.3% | 0.106s |
| `mysql_16_window_frame_range_crash` | mysql | **PASSED** | 7 -> 1 | 85.7% | 1853 -> 697 | 62.4% | 0.093s |
| `pg_01_jsonb_subpath_crash` | postgresql | **PASSED** | 9 -> 1 | 88.9% | 2665 -> 713 | 73.2% | 0.081s |
| `pg_02_gin_fastupdate_crash` | postgresql | **PASSED** | 10 -> 1 | 90.0% | 2040 -> 731 | 64.2% | 0.079s |
| `pg_03_window_frame_segv` | postgresql | **PASSED** | 7 -> 1 | 85.7% | 2175 -> 704 | 67.6% | 0.088s |
| `pg_04_partition_prune_assert` | postgresql | **PASSED** | 6 -> 1 | 83.3% | 2150 -> 650 | 69.8% | 0.118s |
| `pg_05_enum_range_overflow` | postgresql | **PASSED** | 9 -> 1 | 88.9% | 1933 -> 709 | 63.3% | 0.089s |
| `pg_06_cte_cycle_detection` | postgresql | **PASSED** | 9 -> 1 | 88.9% | 2086 -> 681 | 67.4% | 0.09s |
| `pg_07_brin_inclusion_abort` | postgresql | **PASSED** | 10 -> 1 | 90.0% | 1814 -> 707 | 61.0% | 0.087s |
| `pg_08_xml_parse_memory_leak` | postgresql | **PASSED** | 8 -> 1 | 87.5% | 2097 -> 696 | 66.8% | 0.099s |
| `pg_09_foreign_table_join_crash` | postgresql | **PASSED** | 6 -> 1 | 83.3% | 2426 -> 656 | 73.0% | 0.095s |
| `pg_10_tsvector_strip_segv` | postgresql | **PASSED** | 8 -> 1 | 87.5% | 2039 -> 730 | 64.2% | 0.097s |
| `pg_11_composite_domain_check` | postgresql | **PASSED** | 10 -> 1 | 90.0% | 1848 -> 722 | 60.9% | 0.079s |
| `pg_12_lateral_join_corrupt` | postgresql | **PASSED** | 11 -> 1 | 90.9% | 1862 -> 727 | 61.0% | 0.097s |
| `pg_13_generated_col_dependency` | postgresql | **PASSED** | 9 -> 1 | 88.9% | 1814 -> 780 | 57.0% | 0.086s |
| `pg_14_temp_table_cleanup_abort` | postgresql | **PASSED** | 8 -> 1 | 87.5% | 1489 -> 738 | 50.4% | 0.09s |
| `pg_15_spgist_box_index_crash` | postgresql | **PASSED** | 8 -> 1 | 87.5% | 1788 -> 767 | 57.1% | 0.09s |
| `pg_16_multirange_canonical_segv` | postgresql | **PASSED** | 7 -> 1 | 85.7% | 1667 -> 710 | 57.4% | 0.096s |

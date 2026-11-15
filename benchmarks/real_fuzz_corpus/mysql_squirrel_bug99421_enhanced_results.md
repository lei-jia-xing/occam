# mysql_squirrel_bug99421_enhanced Comparative Reduction Results

**Input:** `benchmarks/real_fuzz_corpus/mysql_squirrel_bug99421_enhanced.sql` — 1282 bytes, 14 statements.
**Target DBMS:** mysql (occam-target-mysql80).
**Observed Crash Signature:** `Assertion failure: dict0dict\.cc:657:col_nr < static_cast<ulint>\(table->n_t_def\)`.

## Comparative Evaluation Across Reduction Profiles

| Profile | Initial (B / stmts) | Reduced (B / stmts) | Byte Reduction | Stmt Reduction | Oracle Calls (Misses) | Cache Hits | Cache Hit Ratio | Wall Time (s) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Ours-Statement (`no-intra`) | 1282 / 14 | 271 / 2 | 78.86% | 85.71% | 19 | 21 | 52.50% | 75.61 |
| B1 (`sequential`) | 1282 / 14 | 271 / 2 | 78.86% | 85.71% | 16 | 4 | 20.00% | 154.39 |
| B2 (`statement-ddmin`) | 1282 / 14 | 271 / 2 | 78.86% | 85.71% | 20 | 3 | 13.04% | 75.69 |
| Ours-NoDep (`graph-off`) | 1282 / 14 | 219 / 2 | 82.92% | 85.71% | 67 | 29 | 30.21% | 134.33 |

## Clean-State 5/5 Replay Verification

Target Profile: `no-intra` — Status: **PASSED_5_OF_5**

| Replay | Exit Code | Result |
|---:|---:|---|
| 1 | 0 | PASSED |
| 2 | 0 | PASSED |
| 3 | 0 | PASSED |
| 4 | 0 | PASSED |
| 5 | 0 | PASSED |

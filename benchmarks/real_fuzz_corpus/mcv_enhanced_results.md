# Enhanced MCV Crash Reduction Results

**Input:** `pg_fuzzer_mcv_enhanced.sql` — 2,277 bytes, 22 SQL statements. Initial 5/5 replay verification is user-reported; this comparison was run against PostgreSQL 12.0 (`occam-target-pg12`) compiled with `--enable-cassert --enable-debug`.

**Limits:** 15-second oracle timeout, 300-second budget, maximum 200 oracle executions. Candidate evaluations used `reset_db.sh`.

**Observed signature:** `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)`

## Comparative results

| Profile | Initial (B / stmts) | Reduced (B / stmts) | Byte reduction | Statement reduction | Oracle calls / misses | Cache hits | Cache hit ratio | Wall time (s) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Ours-Full (`full`) | 2277 / 22 | 606 / 5 | 73.39% | 77.27% | 139 | 117 | 45.70% | 133.88 |
| B1 (`sequential`) | 2277 / 22 | 620 / 5 | 72.77% | 77.27% | 26 | 8 | 23.53% | 39.45 |
| B2 (`statement-ddmin`) | 2277 / 22 | 620 / 5 | 72.77% | 77.27% | 36 | 2 | 5.26% | 35.45 |
| Ours-NoDep (`graph-off`) | 2277 / 22 | 606 / 5 | 73.39% | 77.27% | 117 | 86 | 42.36% | 113.33 |

Calls/misses are completed uncached predicate processes. Cache hit ratio = hits / (hits + misses). Wall time is externally measured around the full `occam-eval` command.

## Full-profile output

`/tmp/occam-eval-enhanced-full/minimal.sql` is 606 bytes and 5 statements: 73.39% byte reduction and 77.27% statement reduction from the 2,277-byte, 22-statement input.

## Final 5-replay verification

Each replay reset the database first. All five oracle executions returned exit 0 and recorded the identical signature `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)`:

| Replay | Exit code | Result |
|---:|---:|---|
| 1 | 0 | PASS |
| 2 | 0 | PASS |
| 3 | 0 | PASS |
| 4 | 0 | PASS |
| 5 | 0 | PASS |

All profiles reached 5 statements. Full and graph-off also made intra-statement simplifications, reducing to 606 bytes versus 620 bytes for the statement-only controls.

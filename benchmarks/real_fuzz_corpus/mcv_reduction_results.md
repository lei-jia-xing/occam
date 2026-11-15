# Reduction & Baseline Evaluation: `pg_fuzzer_mcv_clauses`

This report documents the end-to-end reduction and baseline comparison of an authentic PostgreSQL crash case (`pg_fuzzer_mcv_clauses.sql`) discovered by SQLancer (OSDI '20) on PostgreSQL 12.0 (`REL_12_0`) compiled with `--enable-cassert --enable-debug`.

---

## 1. Experimental Setup & Protocol

- **Target DBMS**: PostgreSQL 12.0 running in rootless container `occam-target-pg12` with `--enable-cassert --enable-debug`.
- **Target Assertion**: `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` (Signal 6: `SIGABRT`).
- **Occam Oracle**: `harness/crash_oracle.py --expected-signature "FailedAssertion.*mcv\.c"` (exit 0 on matching crash, 1 on normal completion/mismatch, 124 on timeout).
- **Candidate Isolation**: `harness/reset_db.sh` drops and recreates `occam_test` from `template_occam` before every candidate check, terminating lingering sessions and awaiting postmaster crash recovery.
- **Limits**: Per-oracle timeout 15.0s, total profile budget 300.0s, max 200 uncached oracle executions.

---

## 2. 5-Replay Verification Protocol

In compliance with thesis requirements (*"每个用例先连续重放 5 次，全部崩溃且签名一致才入集"*):

### Initial Replay (Unreduced Input)
| Replay # | Candidate State Reset | Oracle Result | Crash Signature Observed |
| :---: | :---: | :---: | :--- |
| 1 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |
| 2 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |
| 3 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |
| 4 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |
| 5 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |

**Result**: **5/5 (100%) deterministic crash reproduction confirmed**.

### Final Replay (Reduced Minimal SQL from `full`)
| Replay # | Candidate State Reset | Oracle Result | Crash Signature Observed |
| :---: | :---: | :---: | :--- |
| 1 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |
| 2 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |
| 3 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |
| 4 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |
| 5 | Success (template clone) | **Exit 0 (Accepted)** | `TRAP: FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` |

**Result**: **5/5 (100%) deterministic crash preservation confirmed on reduced output**.

---

## 3. Comparative Reduction Results

Four profiles evaluated under uniform budget and oracle constraints:
1. **Ours-Full (`full`)**: Complete Occam engine (Dependency slicing + closure + 1-minimality statement audit + intra-statement syntax reduction + memoization).
2. **B1 (`sequential`)**: Sequential statement-by-statement deletion.
3. **B2 (`statement-ddmin`)**: Delta Debugging (DDMin) at the statement granularity.
4. **Ours-NoDep (`graph-off`)**: Graph-free ablation (disabling dependency slicing and closure; retains intra-statement rewrite and 1-minimality audit).

### Performance & Quality Comparison Table

| Profile / Method | Initial Size | Reduced Size | Byte Red. (%) | Stmt Red. (%) | Oracle Calls (Misses) | Cache Hits | Cache Hit Ratio | Wall Time (s) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| **Ours-Full (`full`)** | 194 B / 5 stmts | **191 B / 5 stmts** | **1.55%** | **0.0%** | 90 | **115** | **56.1%** | 88.00s |
| **B1 (`sequential`)** | 194 B / 5 stmts | 194 B / 5 stmts | 0.00% | 0.0% | 6 | 6 | 50.0% | 10.88s |
| **B2 (`statement-ddmin`)** | 194 B / 5 stmts | 194 B / 5 stmts | 0.00% | 0.0% | 15 | 6 | 28.6% | 18.00s |
| **Ours-NoDep (`graph-off`)** | 194 B / 5 stmts | **191 B / 5 stmts** | **1.55%** | **0.0%** | 90 | 97 | 51.9% | 86.31s |

---

## 4. Key Findings & Analysis

1. **Statement-Level Irreducibility (1-Minimal Core)**:
   - All 5 statements in `pg_fuzzer_mcv_clauses.sql` are strictly mandatory to trigger the PostgreSQL planner assertion:
     - `CREATE TABLE t0(...)`: required for relation definition.
     - `INSERT INTO t0 VALUES(...)`: required to populate non-empty data so MCV statistics can be constructed.
     - `CREATE STATISTICS s0 ON c0, c2 FROM t0`: constructs extended multi-column statistics.
     - `ANALYZE`: builds the MCV distribution in `pg_statistic_ext_data`.
     - `SELECT ... WHERE t0.c2 OR t0.c1 OR t0.c0`: triggers faulty multivariate clause matching in `mcv.c:1553`.
   - Both **B1** and **B2** completed with **0% statement reduction** and **0% byte reduction**, unable to simplify the input further.

2. **Intra-Statement Syntax Simplification**:
   - Because Occam implements tree-sitter AST surgery (`intra-statement` passes), **Ours-Full** and **Ours-NoDep** successfully simplified internal SQL literals:
     ```sql
     -- Original
     INSERT INTO t0 VALUES(FALSE, FALSE, FALSE);

     -- Reduced (Ours-Full & Ours-NoDep)
     INSERT INTO t0 VALUES(NULL, NULL, NULL);
     ```
   - Demonstrates that boolean literals could be simplified to `NULL` while still inducing the exact planner crash.

3. **Candidate Memoization Efficiency**:
   - In **Ours-Full**, candidate caching saved **115 redundant database re-executions** (56.1% cache hit ratio), dramatically reducing DBMS recovery load.

4. **Preservation & Soundness**:
   - Both initial and final SQL passed the 5-replay verification with 100% fidelity.
   - Zero bug slippage: the exact assertion `FailedAssertion("!(clauses != ((List *) ((void *)0)))", File: "mcv.c", Line: 1553)` was preserved throughout the reduction trajectory.

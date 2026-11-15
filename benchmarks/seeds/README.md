# High-Quality SQL Fuzzing Seeds

This directory provides canonical, syntactically and semantically rich multi-statement SQL seeds designed for fuzz testing, mutational injection, and database reducer evaluation (Occam, SQUIRREL, SQLsmith).

## Seed Catalog

| Seed File | Primary Focus | Key Language Features |
|:---|:---|:---|
| `01_cte_recursive_analytics.sql` | Recursive CTEs & Window Functions | `WITH RECURSIVE`, hierarchical tree expansion, `ROW_NUMBER() OVER (PARTITION BY ...)` |
| `02_transaction_savepoint_churn.sql` | Transaction Boundaries & DDL Churn | `BEGIN/COMMIT`, `SAVEPOINT`, `ROLLBACK TO SAVEPOINT`, `ALTER TABLE ADD/DROP COLUMN` |
| `03_deep_expressions_polymorphic.sql` | Deep AST Expressions | Nested `CASE WHEN`, `COALESCE`, `NULLIF`, bitwise/arithmetic expressions, type casting |
| `04_multitable_diamond_dag.sql` | Multi-Table Diamond Dependencies | Diamond Def-Use DAG (Root -> Left/Right -> Sink), `FOREIGN KEY ... ON DELETE CASCADE` |
| `05_upsert_conflict.sql` | Upsert & Uniqueness Constraints | `INSERT ... ON CONFLICT DO UPDATE`, `EXCLUDED` pseudo-table |
| `06_views_and_aggregates.sql` | Views & Complex Grouping | `CREATE VIEW`, `GROUP BY ... HAVING`, `COUNT(DISTINCT ...)`, multi-table aggregations |
| `deep_expression_seed.sql` | SQLite Expression Scale Benchmark | Deterministic numeric expressions and rowset validation fixture for `scale_study.py` |

## Usage
- **Haskell Mutational Fuzzer**: Embedded directly as `SeedScenario` in `test/MutationalFuzz.hs` for Property-based fuzzing and reduction recovery.
- **Python Scale Study**: Used by `benchmarks/tools/scale_study.py` for SQLite throughput and scale evaluations.
- **External CLI Fuzzers**: Feedable directly into `occam` via stdin or file arguments.

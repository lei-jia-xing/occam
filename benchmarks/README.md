# Occam Benchmark Corpus (Phase 9)

Real-world multi-statement SQL crash reproduction corpus for evaluating SQL minimizer performance, dependency DAG slicing, and dialect hardening.

---

## 1. Benchmark Scenarios Overview

| File | DBMS Target | Statements | Key Features & Defect Focus |
| :--- | :--- | :---: | :--- |
| `01_pg_json_operator.sql` | **PostgreSQL** (>= 12) | 13 | GIN indexes, nested JSONB, path operators (`->>`, `#>>`, `@>`), 4 dead setup tables |
| `02_mysql_cte_recursive.sql` | **MySQL** (>= 8.0) | 16 | Recursive CTE (`WITH RECURSIVE`), window functions (`ROW_NUMBER`, `SUM`), joins, interleaved DML |
| `03_pg_partial_index.sql` | **PostgreSQL** (>= 11) | 17 | Partial expression indexes, schema evolution (`ALTER TABLE`), index scan defects |
| `04_mysql_duplicate_key.sql` | **MySQL** (>= 8.0) | 11 | `ON DUPLICATE KEY UPDATE`, multiple composite unique keys, batch multi-row inserts |
| `05_multi_table_stress.sql` | **Cross-Platform** | 25 | 5 tables, foreign keys, 2 views, transaction barriers (`BEGIN...COMMIT`), edge-case expressions |

---

## 2. Benchmark Dataset Details

### 01. `01_pg_json_operator.sql`
- **Target DBMS:** PostgreSQL
- **Focus:** JSONB path extraction and GIN indexing corruption.
- **Structure:**
  - 4 irrelevant setup tables (`config_parameters`, `audit_trail`, `user_accounts`, `system_telemetry`) with interleaved data inserts.
  - Target table `event_records` with GIN indexes on `payload` and `metadata`.
  - Crashing query combining containment (`@>`), scalar path extraction (`->>`), and nested JSON path navigation (`#>>`).
- **Reduction Goal:** Eliminate all 4 unreferenced tables and their DML, isolating only `event_records` and the GIN index scan.

### 02. `02_mysql_cte_recursive.sql`
- **Target DBMS:** MySQL
- **Focus:** CTE evaluation crash with window function frame computation.
- **Structure:**
  - Multi-level organizational hierarchy with recursive self-referential CTE.
  - Multi-table joins connecting recursive relations to staff and sales records.
  - Window functions: `ROW_NUMBER() OVER (...)`, `DENSE_RANK() OVER (...)`, `SUM(...) OVER (...)`, `AVG(...) OVER (...)`.
  - Multiple interleaved DML operations on unrelated tables (`app_session_log`, `inventory_stock`, `billing_invoices`).
- **Reduction Goal:** Prune noise DML statements while preserving recursive CTE dependencies and join chains.

### 03. `03_pg_partial_index.sql`
- **Target DBMS:** PostgreSQL
- **Focus:** Partial expression index match and planner predicate inference defects.
- **Structure:**
  - Initial table creation followed by sequential schema migrations (`ALTER TABLE accounts ADD COLUMN ...`).
  - Expression indexes with partial predicates: `(lower(email), (flags & 7)) WHERE status = 'active' AND tier = 'enterprise'`.
  - Queries specifically exercising the indexed expressions and WHERE conditions.
- **Reduction Goal:** Retain the ordered migration chain and partial index definition while slicing unrelated auth tokens and audit entries.

### 04. `04_mysql_duplicate_key.sql`
- **Target DBMS:** MySQL
- **Focus:** Deadlocks and index state corruption during concurrent multi-row upserts with multiple unique keys.
- **Structure:**
  - Primary key `(item_sku, warehouse_id)` alongside composite unique keys `uk_sku_channel` and `uk_warehouse_bin`.
  - Batch multi-row `INSERT ... ON DUPLICATE KEY UPDATE` statements inducing simultaneous conflicts across distinct unique indexes.
  - Interleaved session and audit table modifications.
- **Reduction Goal:** Preserve the multi-row upsert statements triggering key collisions while discarding extraneous logging DML.

### 05. `05_multi_table_stress.sql`
- **Target DBMS:** Cross-Platform (PostgreSQL & MySQL compliant)
- **Focus:** Transaction barrier preservation and deep dependency graph reduction.
- **Structure:**
  - Exactly 25 statements.
  - 5 relational tables (`departments`, `employees`, `projects`, `task_assignments`, `system_journal`) connected via foreign key references.
  - 2 aggregated views (`dept_burn_rate`, `project_cost_summary`).
  - 2 transactional blocks bounded by `BEGIN;` and `COMMIT;` fences.
  - Edge-case arithmetic and conditional expressions: `NULLIF(...)`, `COALESCE(...)`, `CASE WHEN ... BETWEEN ... END`, and bitwise/modulo operations.
- **Reduction Goal:** Verify that transactional fences are never violated during closure delta debugging, and foreign key dependencies remain intact.

---

## 3. DBMS Execution Harnesses

Occam interacts with database targets through standalone JSON crash oracles located in `harness/`:

- **PostgreSQL:** `./harness/pg_harness.sh`
  - Resets state via `dropdb --if-exists` and `createdb`.
  - Executes candidate queries with `psql -v ON_ERROR_STOP=1`.
  - Translates crash signals (SIGSEGV = 11, SIGABRT = 6) to structured JSON reports.
- **MySQL:** `./harness/mysql_harness.sh`
  - Resets state via `DROP DATABASE IF EXISTS` and `CREATE DATABASE`.
  - Executes candidate queries via `mysql` client.
  - Detects server disconnection errors (e.g. `CR_SERVER_LOST`, `ERROR 2013`) and returns typed signal representations.

---

## 4. Reproduction & Benchmark Commands

### Running Individual Benchmarks

```bash
# Benchmark 01 (PostgreSQL)
cabal run occam -- benchmarks/01_pg_json_operator.sql \
    --oracle "./harness/pg_harness.sh" \
    --out "./benchmarks/results/01_pg_json_operator"

# Benchmark 02 (MySQL)
cabal run occam -- benchmarks/02_mysql_cte_recursive.sql \
    --oracle "./harness/mysql_harness.sh" \
    --out "./benchmarks/results/02_mysql_cte_recursive"

# Benchmark 03 (PostgreSQL)
cabal run occam -- benchmarks/03_pg_partial_index.sql \
    --oracle "./harness/pg_harness.sh" \
    --out "./benchmarks/results/03_pg_partial_index"

# Benchmark 04 (MySQL)
cabal run occam -- benchmarks/04_mysql_duplicate_key.sql \
    --oracle "./harness/mysql_harness.sh" \
    --out "./benchmarks/results/04_mysql_duplicate_key"

# Benchmark 05 (Stress)
cabal run occam -- benchmarks/05_multi_table_stress.sql \
    --oracle "./harness/pg_harness.sh" \
    --out "./benchmarks/results/05_multi_table_stress"
```

### Running the Automated Benchmark Suite

To execute the entire corpus sequentially:

```bash
chmod +x benchmarks/run_all.sh
./benchmarks/run_all.sh
```

Reduced SQL outputs, reproduction shell scripts, and execution metrics (`stats.json`) will be emitted to `benchmarks/results/<scenario_name>/`.

# Dialect Semantic Coverage

Occam supports six explicit DBMS dialects (`occam --list-dialects`): `postgresql`, `mysql`, `mariadb`, `sqlite`, `duckdb`, and `clickhouse`. All lexing, statement splitting, parsing, semantic fact extraction, exact-span edit generation, and column IR assembly run natively in the pure-Haskell frontend (`Occam.Frontend.*`). See [Frontend Grammar](FRONTEND_GRAMMAR.md).

**Syntax accepted**, **modeled dependency effects**, and **opaque effects** are distinct outcomes: parser acceptance does not prove that a target DBMS accepts the SQL or that every semantic effect is modeled. Statement-local syntax errors retain their source units and emit `SyntaxErrorBarrier`; invalid UTF-8 and range invariant failures are reported separately as typed `ParserError` values.

---

## 1. Supported Dialects

Recognized AST paths share named table/view/index/trigger/sequence/type/schema definitions, drops, selected column/constraint alterations and renames, DML targets, query sources, lexical CTE/alias scopes, and transaction/session facts. These form a conservative dependency model, not a SQL interpreter.

| Dialects | Shared paths & dialect specializations |
| :--- | :--- |
| `postgresql` | Quoted/unquoted identifier rules, search paths, table-target `SELECT INTO`, transaction/savepoint snapshots, inheritance/partitions, `TABLE t`, `CREATE STATISTICS`, policy/rule DDL, and provable `pg_catalog` sequence calls. |
| `mysql`, `mariadb` | Configurable `lower_case_table_names` for relation/namespace parts; multi-table `UPDATE`/`DELETE` targets; implicit DDL commits; optionless `CREATE DATABASE` and one-level `USE`; distinct first-class `mysql` vs `mariadb` dialect capabilities (`RETURNING`, `CREATE SEQUENCE`, `LATERAL`, `PREPARE ... FROM`). |
| `sqlite`, `duckdb` | ASCII-only/insensitive identifier matching and CTAS snapshots; dedicated SQLite foreign-key and inert storage `PRAGMA` effects plus virtual-table shadow edges (`_segdir`, `_content`); explicit two-part DuckDB `USE catalog.schema`, `PIVOT`, `QUALIFY`, and `CREATE MACRO`. |
| `clickhouse` | Shared named DDL/DML/query extraction with case-preserving identifiers and one-level database namespaces, `PREWHERE`/`SETTINGS`/`OPTIMIZE TABLE` modeling, and `ENGINE`/`ARRAY JOIN` support. Persistent `CREATE VIEW ... TO` routing and partition/overwrite effects remain opaque. |

---

## 2. Modeled Dependency Semantics

- **Whole-row & `TABLE` queries**: `TABLE t`, `(TABLE t)`, nested/CTE/set-operation `TABLE` bodies, `CREATE TABLE copied AS TABLE t`, and `SELECT *` / `t.*` retain whole-row dependencies even alongside explicit predicates or ordering, without over-expanding unrelated qualified sources.
- **Snapshots & materialized views**:
  - Direct standalone `SELECT a INTO copied FROM src` creates a snapshot on PostgreSQL (procedural variables, nested/set-operation `INTO`, and other dialects remain opaque).
  - `CREATE TABLE ... AS ... WITH NO DATA` binds source schema without consuming source rows.
  - CTAS and materialized-view reads/writes do not follow later base-table mutations; ordinary views remain live. `REFRESH MATERIALIZED VIEW` explicitly reads recorded sources, distinguishing `WITH DATA` from `WITH NO DATA`.
- **Schema & namespace moves**: `ALTER VIEW s.v SET SCHEMA dest` preserves lineage at the new name; unqualified renames retain the resolved source schema. Schema-owned definitions, namespace switches, and moves retain schema-generation dependencies. Ambiguous mutation targets are fenced rather than mutating every possible match.
- **Partitioning & inheritance**: `PARTITION OF` / `INHERITS` parents, `ATTACH` / `DETACH PARTITION`, and `ONLY`-aware parent/child reads and writes are modeled.
- **Session & storage settings**:
  - Scoped `foreign_key_checks` assignments (`bare` / `SESSION` / `GLOBAL` / `@@`; `@user` variables and foreign qualifiers still fence).
  - SQLite `PRAGMA foreign_keys` and inert storage pragmas (`journal_mode`, `synchronous`, `temp_store`, `cache_size`, `page_size`); `recursive_triggers` and effectful pragmas still fence.
- **Routines & sequences**:
  - Unproved scalar routines and deferred stored expressions (including sequence defaults) fence conservatively across all dialects while preserving argument column reads.
  - MySQL/MariaDB index length prefixes (`b(1)`) are recognized structurally in key position as first-class AST nodes.
  - PostgreSQL sequence calls are precise only for explicit `pg_catalog.nextval`/`setval`/`currval` with provable literal names (including supported `text`/`regclass` casts). Unqualified/custom calls, dynamic targets, `lastval`, and nontransactional sequence advances inside explicit transactions or with autocommit disabled are fenced.
  - Any script containing routine DDL or a parse error stays fenced.

---

## 3. Explicit Opaque & Parser Boundaries

Unknown global effects establish a persistent fence (see [Architecture](ARCHITECTURE.md#1-system-pipeline)); merely recording an unmodeled identifier is never substituted for modeling its semantics.

- **Opaque forms**: privileges/roles, routine DDL/calls, unsupported session/transaction settings and generic `SET`/`RESET`, partial/default namespace changes, `COPY`/bulk/external-I/O forms, partitioned/multi-table `INSERT` forms, persistent `CREATE VIEW ... TO` routing, unproved table functions/computed row expansion, unsupported view mutations, and option-bearing `CREATE DATABASE`.
- Strict dialect grammar validation ensures constructs invalid for the active dialect emit `SyntaxErrorBarrier` on that statement unit without corrupting neighboring unit boundaries.

# Dialect Semantic Coverage

Occam currently routes parsing through its Rust frontend (`sqlparser-rs` `0.64.0`) while migrating to an in-repo pure-Haskell frontend driven by `Occam.Parser.Dialect.DialectProfile`, and supports the 6 active DBMS dialects (`occam --list-dialects`): `postgresql`, `mysql`, `mariadb`, `sqlite`, `duckdb`, and `clickhouse` (`--dialect` is required; there is no `generic` dialect or implicit default).

**Syntax accepted**, **modeled dependency effects**, and **opaque effects** are distinct outcomes: parser acceptance does not prove that a target DBMS accepts the SQL or that every semantic effect is modeled. Statement-local syntax errors retain their source units; FFI/transport failures are reported separately. Upstream parser bugs and workarounds are tracked in [UPSTREAM_TRACKER.md](UPSTREAM_TRACKER.md).

---

## 1. Supported Dialects

Recognized AST paths share named table/view/index/trigger/sequence/type/schema definitions, drops, selected column/constraint alterations and renames, DML targets, query sources, lexical CTE/alias scopes, and transaction/session facts. These form a conservative dependency model, not a SQL interpreter.

| Dialects | Shared paths & dialect specializations |
| :--- | :--- |
| `postgresql` | Quoted/unquoted identifier rules, search paths, table-target `SELECT INTO`, transaction/savepoint snapshots, inheritance/partitions, and provable `pg_catalog` sequence calls. |
| `mysql`, `mariadb` | Configurable `lower_case_table_names` for relation/namespace parts; multi-table `UPDATE`/`DELETE` targets; implicit DDL commits; optionless `CREATE DATABASE` and one-level `USE`. (During parser migration, `mariadb` profiles distinguish MariaDB capabilities in Haskell while Rust FFI routes both through MySQL parsing.) |
| `sqlite`, `duckdb` | ASCII-only/insensitive identifier matching and CTAS snapshots; dedicated SQLite foreign-key and inert storage `PRAGMA` effects; explicit two-part DuckDB `USE catalog.schema`. |
| `clickhouse` | Shared named DDL/DML/query extraction with case-preserving identifiers and one-level database namespaces. Persistent `CREATE VIEW ... TO` routing and partition/overwrite effects remain opaque. |

---

## 2. Modeled Dependency Semantics

- **Whole-row & `TABLE` queries**: `(TABLE t)`, nested/CTE/set-operation `TABLE` bodies, `CREATE TABLE copied AS TABLE t`, and `SELECT *` / `t.*` retain whole-row dependencies even alongside explicit predicates or ordering, without over-expanding unrelated qualified sources.
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
  - MySQL/MariaDB index length prefixes (`b(1)`) are recognized structurally in key position (see `UP-01` in [UPSTREAM_TRACKER.md](UPSTREAM_TRACKER.md)).
  - PostgreSQL sequence calls are precise only for explicit `pg_catalog.nextval`/`setval`/`currval` with provable literal names (including supported `text`/`regclass` casts). Unqualified/custom calls, dynamic targets, `lastval`, and nontransactional sequence advances inside explicit transactions or with autocommit disabled are fenced.
  - Any script containing routine DDL or a parse error stays fenced.

---

## 3. Explicit Opaque & Parser Boundaries

Unknown global effects establish a persistent fence (see [Architecture](ARCHITECTURE.md#1-system-pipeline)); merely recording an unmodeled identifier is never substituted for modeling its semantics.

- **Opaque forms**: privileges/roles, routine DDL/calls, unsupported session/transaction settings and generic `SET`/`RESET`, partial/default namespace changes, `COPY`/bulk/external-I/O forms, partitioned/multi-table `INSERT` forms, persistent `CREATE VIEW ... TO` routing, unproved table functions/computed row expansion, unsupported view mutations, and option-bearing `CREATE DATABASE`.
- **Upstream parser boundaries**: `sqlparser-rs` `0.64.0` rejects standalone `TABLE t` (though `(TABLE t)` and nested forms parse). Conversely, parser acceptance of `REFRESH` or `ALTER VIEW` on a dialect does not guarantee DBMS support.

# Upstream SQLParser Tracking & Dialect Discrepancies

This document tracks syntax gaps, AST representation mismatches, and parsing limitations in upstream [`sqlparser-rs`](https://github.com/sqlparser-rs/sqlparser-rs) (currently pinned at `0.64.0`), along with Occam's current workarounds and proposed upstream PRs. See [DIALECTS.md](DIALECTS.md) for overall dialect semantic coverage.

---

## 1. Tracking Table

| ID | Dialect | Syntax Construct | Upstream Issue / AST Representation | Occam Impact | Current Workaround in Occam | Upstream PR / Solution Proposal |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **UP-01** | **MySQL** | Prefix Index (`INDEX (col(N))`) | Parsed generically as `Expr::Function` instead of key-part length prefix | Erroneously tripped `visitor.has_function` in DDL, raising `UnknownStatementBarrier` (global fence) | Structural recognition in [`facts.rs`](../rust/sqlparser-ffi/src/actions/facts.rs) via `index_length_prefix_ident` | Add `length: Option<u64>` to `sqlparser::ast::IndexColumn` in `sqlparser-rs` |
| **UP-02** | **SQLite** | Virtual Tables (`CREATE VIRTUAL TABLE ... USING module(...)`) | Module arguments are unparsed strings/idents; schema not extracted | Marked as unmodeled syntax, raising global `UnknownStatementBarrier` and locking subsequent slicing | Lower barrier scope to `RelationScope(name)` in [`facts.rs`](../rust/sqlparser-ffi/src/actions/facts.rs) | Provide structured column parsing for standard SQLite modules (`fts5`, `rtree`) |
| **UP-03** | **MySQL / MSSQL** | User & System Variables (`@var`, `@@var`, `SELECT ... INTO @var`) | Variable scopes (user vs session vs global) are parsed as session vars or expression identifiers lacking user-variable scope distinction | Previously generated `GlobalScope` barriers or polluted table `reads` with `@var` column names | Wired `UserVariableScope` across `SET @var`, `SELECT ... INTO @var`, MSSQL `OUTPUT INTO @var`, and in-expression `@var` refs; excluded `@`/`@@` from table column reads | Refine `sqlparser::ast::Statement::SetVariable` and expression variable nodes |
| **UP-04** | **PostgreSQL** | Dynamic / Dollar-Quoted Routine Bodies | Functions defined in dollar quotes (`$$ ... $$`) contain nested SQL not visited by AST visitor | Functions cannot be statically inspected for read/write effects | Treated as unmodeled routine fence | Pluggable inner statement parser for routine bodies |
| **UP-05** | **PostgreSQL** | Extended Statistics (`CREATE STATISTICS s0 ON ... FROM t0`) | Grammar lacks `STATISTICS` branch in `parse_create`; throws `Expected: an object type after CREATE, found: STATISTICS` | Raises `GlobalScope SyntaxErrorBarrier`, freezing entire script (affects 10 real fuzz units) | Blocked on AST representation; tracked for upstream submission | Add `Statement::CreateStatistics` in `sqlparser-rs` under `PostgreSqlDialect` |
| **UP-06** | **SQLite** | PRAGMA Boolean / Word Literals (`PRAGMA key = true / ON`) | `parse_pragma` value expects `Number` or `SingleQuotedString`; rejects boolean words `true/on/off/false` | Raises `GlobalScope SyntaxErrorBarrier` (tripped in `sqlite_fuzz_recursive_trigger.sql:1`) | Numeric literals (`= 1/0`) work. Occam handles bare boolean words for SQLite `foreign_keys`; other PRAGMAs still depend on upstream parsing | Allow `Word` in `parse_pragma` value in `sqlparser-rs` |
| **UP-07** | **MySQL** | Column Definition `BINARY` Attribute (`col CHAR(1) BINARY`) | `parse_column_def` parses `CHARACTER SET`/`COLLATE` but rejects bare `BINARY` attribute | Raises `GlobalScope SyntaxErrorBarrier` (tripped in `mysql_bug88355.sql:1,3`) | Blocked on AST representation; tracked for upstream submission | Support optional `BINARY` attribute in MySQL column options in `sqlparser-rs` |
| **UP-08** | **ClickHouse** | Materialized View Storage Engine (`CREATE MATERIALIZED VIEW mv ENGINE = ... AS SELECT`) | `parse_create_view` expects `AS` immediately after view name; rejects ClickHouse `ENGINE = ...` | Raises `GlobalScope SyntaxErrorBarrier` on ClickHouse materialized views | Standard views without engine work; engine-bound views blocked upstream | Allow `ENGINE = ...` clause in `parse_create_view` under `ClickHouseDialect` |
| **UP-09** | **ClickHouse** | DDL/Query Clauses & Types (`ALTER ... MATERIALIZE TTL`, `DETACH`/`ATTACH`, `SETTINGS`, `Array(JSON(...))`, `data.:Int64`, `ARRAY JOIN []`) | `ClickHouseDialect` parser rejects ClickHouse `TTL`/query `SETTINGS`, `DETACH`/`ATTACH TABLE`, parameterized `JSON(...)` type args, typed JSON path `.:Type`, and `ARRAY JOIN [...]` | Raises `GlobalScope SyntaxErrorBarrier` across 6 ClickHouse corpus scripts | Tracked upstream only (no local `sqlparser-rs` fork/patch); `ORDER BY tuple()` lifted locally | Extend `ClickHouseDialect` grammar & AST in `sqlparser-rs` |
| **UP-10** | **DuckDB** | `PIVOT`, `ASOF RIGHT JOIN`, `E'...'` Escapes, Parenthesized `FROM` Filters | `DuckDbDialect` lacks top-level `PIVOT`, `ASOF RIGHT JOIN`, `E'\n'` escape mode, and certain subquery `WHERE` forms | Raises `GlobalScope SyntaxErrorBarrier` in 4 DuckDB corpus scripts | Tracked upstream only (no local `sqlparser-rs` fork/patch) | Support `PIVOT`, `ASOF RIGHT JOIN`, and DuckDB escape/subquery forms in `sqlparser-rs` |
| **UP-11** | **MariaDB / MySQL** | `CREATE FULLTEXT INDEX`, `PREPARE ... FROM '...'`, Parenthesized `ADD COLUMN (...)` | `MySqlDialect` rejects `CREATE FULLTEXT INDEX`, string-form `PREPARE s FROM '...'`, and `ALTER TABLE ... ADD COLUMN (...)` | Raises `GlobalScope SyntaxErrorBarrier` in 3 MariaDB corpus scripts | Tracked upstream only (no local `sqlparser-rs` fork/patch) | Extend `MySqlDialect` DDL & `PREPARE` parsing in `sqlparser-rs` |
| **UP-12** | **MySQL** | Multi-Table `ANALYZE TABLE t1, t2` | `parse_analyze` expects a single table / semicolon after first table (`expected a semicolon between statements`) | Raises `GlobalScope SyntaxErrorBarrier` (`mysql_bug26542829`) | Single-table `ANALYZE TABLE t` parses and extracts columns; bare `ANALYZE;` handled locally | Support comma-separated `ANALYZE TABLE` targets in `sqlparser-rs` |
| **UP-13** | **PostgreSQL** | `DROP CONVERSION` | `CONVERSION` is missing from `DROP` object type dispatch | Raises `GlobalScope SyntaxErrorBarrier` (`pg_result_rte_lateral_assert.sql:6`) | Tracked upstream only (no local `sqlparser-rs` fork/patch) | Add `CONVERSION` to `ObjectType` and `parse_drop` in `sqlparser-rs` |

---

## 2. Issue Deep Dives

### UP-01: MySQL Index Length Prefix (`col(N)`)
- **Affects**: MySQL, MariaDB
- **Standard Reference**: MySQL 8.0 Reference Manual § 15.1.15 "CREATE INDEX Statement":
  ```sql
  col_name [(length)] [ASC | DESC]
  ```
  Note that functional key parts require double parentheses `INDEX ((func(col)))` to distinguish them from length prefixes `INDEX (col(10))`.
- **Upstream Representation**:
  `sqlparser-rs`'s `TableConstraint::Index` stores a list of `IndexColumn`:
  ```rust
  pub struct IndexColumn {
      pub column: OrderByExpr,
      pub operator_class: Option<Ident>,
  }
  ```
  Because `OrderByExpr.expr` is an `Expr`, `col(10)` is parsed by `parse_expr()` into `Expr::Function`.
- **Occam Workaround**:
  `index_length_prefix_ident` in [`rust/sqlparser-ffi/src/actions/facts.rs`](../rust/sqlparser-ffi/src/actions/facts.rs) performs structural matching: single identifier name, exactly one unnamed unsigned integer argument, no window/filter/within-group clauses.
- **Corpus Evidence**:
  Resolved false global barrier in [`benchmarks/real_fuzz_corpus/mysql_bug_91975.sql`](../benchmarks/real_fuzz_corpus/mysql_bug_91975.sql).
- **Proposed Upstream PR**:
  Extend `IndexColumn` in `sqlparser::ast`:
  ```rust
  pub struct IndexColumn {
      pub column: OrderByExpr,
      pub operator_class: Option<Ident>,
      pub length: Option<u64>, // Proposed: MySQL key part length prefix
  }
  ```

---

### UP-02: SQLite Virtual Tables (`CREATE VIRTUAL TABLE ... USING`)
- **Affects**: SQLite
- **Syntax Example**:
  ```sql
  CREATE VIRTUAL TABLE vt0 USING fts5(c0, c1);
  ```
- **Upstream Representation**:
  In `sqlparser-rs`:
  ```rust
  Statement::CreateVirtualTable {
      name: ObjectName,
      module_name: Ident,
      module_args: Vec<Ident>,
  }
  ```
  The column definitions are swallowed into unstructured identifier lists or string tokens.
- **Occam Workaround / Plan**:
  Extract the target `name`, register the table in the DDL catalog via `CatalogOp::DefineTable`, and lower the barrier scope to `RelationScope(name)` in [`rust/sqlparser-ffi/src/actions/facts.rs`](../rust/sqlparser-ffi/src/actions/facts.rs).
- **Corpus Evidence**:
  Resolved in [`benchmarks/real_fuzz_corpus/sqlite_fuzz_fts5_savepoint.sql`](../benchmarks/real_fuzz_corpus/sqlite_fuzz_fts5_savepoint.sql), allowing independent virtual tables (e.g. `vt0` vs `vt1`) to be cleanly isolated and sliced away without whole-script global fencing.

---

### UP-05: PostgreSQL Extended Statistics (`CREATE STATISTICS`)
- **Affects**: PostgreSQL 10+
- **Standard Reference**: PostgreSQL Documentation § 14 "CREATE STATISTICS":
  ```sql
  CREATE STATISTICS [ IF NOT EXISTS ] statistics_name
      [ ( statistics_kind [, ...] ) ]
      ON column_name, column_name [, ...]
      FROM table_name
  ```
- **Upstream Representation**:
  `sqlparser-rs`'s `Parser::parse_create` does not recognize keyword `STATISTICS`. It terminates with:
  `Expected: an object type after CREATE, found: STATISTICS`.
- **Occam Impact**:
  Raises `GlobalScope SyntaxErrorBarrier`, pinning every preceding and succeeding statement in multi-statement PostgreSQL fuzz crash scripts.
- **Corpus Evidence**:
  Blocks 10 real fuzz units in:
  - `benchmarks/real_fuzz_corpus/pg_stack_overflow_27stmts.sql` (statements s0–s7)
  - `benchmarks/real_fuzz_corpus/pg_fuzzer_mcv_clauses.sql` (statement 3)
  - `benchmarks/real_fuzz_corpus/pg_fuzzer_mcv_enhanced.sql` (statement 18)
- **Proposed Upstream PR**:
  Add `Statement::CreateStatistics` in `sqlparser-rs`:
  ```rust
  Statement::CreateStatistics {
      name: ObjectName,
      stat_types: Vec<Ident>,
      columns: Vec<Ident>,
      from: ObjectName,
      if_not_exists: bool,
  }
  ```

---

### UP-06: SQLite PRAGMA Word & Boolean Literals (`PRAGMA key = true / ON`)
- **Affects**: SQLite
- **Standard Reference**: SQLite Documentation § PRAGMA Statements:
  Boolean settings accept `yes`/`no`, `on`/`off`, `true`/`false`, or `1`/`0`.
- **Upstream Representation**:
  In `sqlparser-rs`, `Parser::parse_pragma` expects `Number` or `SingleQuotedString` or `?`.
  When encountering word tokens like `true` or `ON`, it rejects with:
  `Expected: a concrete value, found: ON` or `Expected: number or string or ? placeholder, found: true`.
- **Occam Impact**:
  Raises `GlobalScope SyntaxErrorBarrier` at parse boundary.
- **Corpus Evidence**:
  Tripped on statement 1 of `benchmarks/real_fuzz_corpus/sqlite_fuzz_recursive_trigger.sql` (`PRAGMA recursive_triggers = true;`).
- **Proposed Upstream PR**:
  In `sqlparser::parser::Parser::parse_pragma`, allow parsing `Token::Word` and convert boolean keywords to canonical values.

---

### UP-07: MySQL Column Definition `BINARY` Attribute
- **Affects**: MySQL, MariaDB
- **Standard Reference**: MySQL 8.0 Reference Manual § 13.1.20 "CREATE TABLE Statement":
  ```sql
  data_type [BINARY] [CHARACTER SET charset_name] [COLLATE collation_name]
  ```
- **Upstream Representation**:
  `sqlparser-rs` parses `CHARACTER SET` and `COLLATE` in column definitions, but does not accept the standalone `BINARY` attribute keyword on character types. Fails with:
  `Expected: ',' or ')' after column definition, found: BINARY`.
- **Occam Impact**:
  Raises `GlobalScope SyntaxErrorBarrier`.
- **Corpus Evidence**:
  Tripped on statements 1 and 3 in `benchmarks/real_fuzz_corpus/mysql_bug88355.sql`.
- **Proposed Upstream PR**:
  In `sqlparser-rs`, extend MySQL column definition option parser to recognize optional `BINARY` modifier.

---

### UP-08: ClickHouse Materialized View Storage Engine
- **Affects**: ClickHouse
- **Standard Reference**: ClickHouse Documentation § MaterializedView:
  ```sql
  CREATE MATERIALIZED VIEW [IF NOT EXISTS] [db.]table_name [TO [db.]name] [ENGINE = engine] [POPULATE] AS SELECT ...
  ```
- **Upstream Representation**:
  `sqlparser-rs`'s `parse_create_view` expects `AS` immediately after view name, rejecting ClickHouse's `ENGINE = ...` clause with:
  `Expected: AS, found: ENGINE`.
- **Occam Impact**:
  Raises `GlobalScope SyntaxErrorBarrier` on ClickHouse materialized views.
- **Proposed Upstream PR**:
  Under `ClickHouseDialect`, allow `parse_create_view` to parse optional `ENGINE = ...` table engine specification before `AS`.

---

### UP-09: ClickHouse DDL/Query Clauses & Types
- **Affects**: ClickHouse
- **Upstream Errors Observed** (`sqlparser-rs 0.64.0`):
  - `Expected: ADD, RENAME, PARTITION, SWAP, DROP, REPLICA IDENTITY, SET, or SET TBLPROPERTIES after ALTER TABLE, found: MATERIALIZE` (`benchmarks/real_fuzz_corpus/clickhouse_alias_materialize_ttl_segv.sql`)
  - `Expected: an SQL statement, found: DETACH` (`benchmarks/real_fuzz_corpus/clickhouse_alter_sorting_key_cast_segv.sql`)
  - `Expected: ), found: (` on `Array(JSON(...))` and `Expected: an identifier or a '*' after '.', found: :` on `data.:Int64` (`benchmarks/real_fuzz_corpus/clickhouse_json_shared_variant_size_segv.sql`)
  - `Expected: identifier, found: [` on `ARRAY JOIN []` (`benchmarks/real_fuzz_corpus/clickhouse_logical_expr_single_or_abort.sql`, `benchmarks/real_fuzz_corpus/clickhouse_logical_expr_nullable_chassert.sql`)
  - Trailing query/DDL `SETTINGS ...` or `DETACH`/`ATTACH` statements splitting as `expected a semicolon between statements` (`benchmarks/real_fuzz_corpus/clickhouse_coalescing_projection_part_offset_segv.sql`)
- **Occam Status**: Unpatched upstream gap; emits `SyntaxErrorBarrier` per failing unit while keeping `CREATE TABLE ... ORDER BY tuple()` unfenced locally.

---

### UP-10: DuckDB `PIVOT`, `ASOF RIGHT JOIN`, Escape Strings, and Parenthesized `FROM` Filters
- **Affects**: DuckDB
- **Upstream Errors Observed** (`sqlparser-rs 0.64.0`):
  - `Expected: an SQL statement, found: PIVOT` (`benchmarks/real_fuzz_corpus/duckdb_pivot_full_join_columnref.sql`)
  - `Expected: JOIN, found: RIGHT` on `ASOF RIGHT JOIN` (`benchmarks/real_fuzz_corpus/duckdb_asof_right_join_vector_ref.sql`)
  - `Expected: ), found: '\n'` on `E'\n'` escape string literals (`benchmarks/real_fuzz_corpus/duckdb_recursive_cte_bind_column.sql`)
  - `Expected: ), found: WHERE` on `SELECT ... FROM (VALUES ...) WHERE ...` (`benchmarks/real_fuzz_corpus/duckdb_window_subquery_tostring_internal.sql`)
- **Occam Status**: Unpatched upstream gap; emits `SyntaxErrorBarrier` per failing unit.

---

### UP-11: MariaDB / MySQL `FULLTEXT INDEX`, `PREPARE ... FROM '...'`, and Parenthesized `ADD COLUMN (...)`
- **Affects**: MariaDB, MySQL
- **Upstream Errors Observed** (`sqlparser-rs 0.64.0`):
  - `Expected: identifier, found: (` on `ALTER TABLE ... ADD COLUMN (...)` (`benchmarks/real_fuzz_corpus/mariadb_mdev24072_instant_alter_vcols_assert.sql`)
  - `Expected: an object type after CREATE, found: FULLTEXT` (`benchmarks/real_fuzz_corpus/mariadb_mdev25295_duplicate_fulltext_index_assert.sql`)
  - `Expected: AS, found: FROM` on `PREPARE s FROM '...'` (`benchmarks/real_fuzz_corpus/mariadb_mdev25460_prepare_set_statement_assert.sql`)
- **Occam Status**: Unpatched upstream gap; emits `SyntaxErrorBarrier` per failing unit.

---

### UP-12: MySQL Multi-Table `ANALYZE TABLE`
- **Affects**: MySQL
- **Upstream Error Observed** (`sqlparser-rs 0.64.0`):
  - `expected a semicolon between statements` on `ANALYZE TABLE t1, t2` (`benchmarks/real_fuzz_corpus/mysql_bug26542829_derived_datetime_range_assert.sql`)
- **Occam Status**: Single-table `ANALYZE TABLE` parses natively and bare `ANALYZE;` emits `GlobalScope UnknownDDLBarrier`; multi-table lists require upstream `sqlparser-rs` support.

---

### UP-13: PostgreSQL `DROP CONVERSION`
- **Affects**: PostgreSQL
- **Upstream Error Observed** (`sqlparser-rs 0.64.0`):
  - `Expected: ..., found: CONVERSION` (`benchmarks/real_fuzz_corpus/pg_result_rte_lateral_assert.sql`)
- **Occam Status**: Unpatched upstream gap; emits `SyntaxErrorBarrier` for statement 5.

---

## 3. Local Patching Workflow (When & How to Patch `sqlparser-rs`)

When upstream syntax limitations cause **parse failures (syntax errors or AST panics)** rather than mere semantic under-approximations, a local patch on `sqlparser-rs` is preferred over degrading the oracle.

### Patching Protocol:
1. **Clone & Branch**:
   Create a branch on the internal fork or vendor directory:
   ```bash
   git clone https://github.com/sqlparser-rs/sqlparser-rs vendor/sqlparser-rs
   ```
2. **Apply Local Patch**:
   In [`rust/sqlparser-ffi/Cargo.toml`](../rust/sqlparser-ffi/Cargo.toml), add a patch override:
   ```toml
   [patch.crates-io]
   sqlparser = { path = "../../vendor/sqlparser-rs" }
   ```
3. **Upstream Submission**:
   - File an issue upstream referencing the minimal reproduction.
   - Open a PR against `sqlparser-rs/sqlparser-rs:main`.
   - Update this tracking document with the upstream issue/PR number.
4. **Deprecation & Reversion**:
   Once the upstream version is released (e.g. `0.65.0`), bump the version in `Cargo.toml` and remove the `[patch.crates-io]` section.

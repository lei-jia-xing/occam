# Upstream SQLParser Tracking & Dialect Discrepancies

This document tracks syntax gaps, AST representation mismatches, and parsing limitations in upstream [`sqlparser-rs`](https://github.com/sqlparser-rs/sqlparser-rs) (currently pinned at `0.64.0`), along with Occam's current workarounds and proposed upstream PRs.

---

## 1. Tracking Table

| ID | Dialect | Syntax Construct | Upstream Issue / AST Representation | Occam Impact | Current Workaround in Occam | Upstream PR / Solution Proposal |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **UP-01** | **MySQL** | Prefix Index (`INDEX (col(N))`) | Parsed generically as `Expr::Function` instead of key-part length prefix | Erroneously tripped `visitor.has_function` in DDL, raising `UnknownStatementBarrier` (global fence) | Structural recognition in [`rust/sqlparser-ffi/src/actions/facts.rs`](file:///home/hunter/Workspace/occam/rust/sqlparser-ffi/src/actions/facts.rs#L483) via `index_length_prefix_ident` | Add `length: Option<u64>` to `sqlparser::ast::IndexColumn` in `sqlparser-rs` |
| **UP-02** | **SQLite** | Virtual Tables (`CREATE VIRTUAL TABLE ... USING module(...)`) | Module arguments are unparsed strings/idents; schema not extracted | Marked as unmodeled syntax, raising global `UnknownStatementBarrier` and locking subsequent slicing | Lower barrier scope to `RelationScope(name)` in [`facts.rs`](file:///home/hunter/Workspace/occam/rust/sqlparser-ffi/src/actions/facts.rs#L4642) | Provide structured column parsing for standard SQLite modules (`fts5`, `rtree`) |
| **UP-03** | **MySQL** | User & System Variables (`SET @var`, `SET @@var`) | Variable scopes (user vs session vs global) are parsed but lack unified semantics | Generates `UnknownSessionEffectBarrier(scope: Global)` | Scope narrowing: route user variables (`@var`) to `UserVariableScope` | Refine `sqlparser::ast::Statement::SetVariable` variable scoping |
| **UP-04** | **PostgreSQL** | Dynamic / Dollar-Quoted Routine Bodies | Functions defined in dollar quotes (`$$ ... $$`) contain nested SQL not visited by AST visitor | Functions cannot be statically inspected for read/write effects | Treated as unmodeled routine fence | Pluggable inner statement parser for routine bodies |
| **UP-05** | **PostgreSQL** | Extended Statistics (`CREATE STATISTICS s0 ON ... FROM t0`) | Grammar lacks `STATISTICS` branch in `parse_create`; throws `Expected: an object type after CREATE, found: STATISTICS` | Raises `GlobalScope SyntaxErrorBarrier`, freezing entire script (affects 10 real fuzz units) | Blocked on AST representation; tracked for upstream submission | Add `Statement::CreateStatistics` in `sqlparser-rs` under `PostgreSqlDialect` |
| **UP-06** | **SQLite** | PRAGMA Boolean / Word Literals (`PRAGMA key = true / ON`) | `parse_pragma` value expects `Number` or `SingleQuotedString`; rejects boolean words `true/on/off/false` | Raises `GlobalScope SyntaxErrorBarrier` (tripped in `sqlite_fuzz_recursive_trigger.sql:1`) | Numeric literals (`= 1/0`) work. Occam handles bare boolean words for SQLite `foreign_keys`; other PRAGMAs still depend on upstream parsing | Allow `Word` in `parse_pragma` value in `sqlparser-rs` |
| **UP-07** | **MySQL** | Column Definition `BINARY` Attribute (`col CHAR(1) BINARY`) | `parse_column_def` parses `CHARACTER SET`/`COLLATE` but rejects bare `BINARY` attribute | Raises `GlobalScope SyntaxErrorBarrier` (tripped in `mysql_bug88355.sql:1,3`) | Blocked on AST representation; tracked for upstream submission | Support optional `BINARY` attribute in MySQL column options in `sqlparser-rs` |
| **UP-08** | **ClickHouse** | Materialized View Storage Engine (`CREATE MATERIALIZED VIEW mv ENGINE = ... AS SELECT`) | `parse_create_view` expects `AS` immediately after view name; rejects ClickHouse `ENGINE = ...` | Raises `GlobalScope SyntaxErrorBarrier` on ClickHouse materialized views | Standard views without engine work; engine-bound views blocked upstream | Allow `ENGINE = ...` clause in `parse_create_view` under `ClickHouseDialect` |

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
  [`index_length_prefix_ident`](file:///home/hunter/Workspace/occam/rust/sqlparser-ffi/src/actions/facts.rs#L483) performs structural matching: single identifier name, exactly one unnamed unsigned integer argument, no window/filter/within-group clauses.
- **Corpus Evidence**:
  Resolved false global barrier in [`benchmarks/real_fuzz_corpus/mysql_bug_91975.sql`](file:///home/hunter/Workspace/occam/benchmarks/real_fuzz_corpus/mysql_bug_91975.sql).
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
  Extract the target `name`, register the table in the DDL catalog via `CatalogOp::DefineTable`, and lower the barrier scope to `RelationScope(name)` in [`facts.rs`](file:///home/hunter/Workspace/occam/rust/sqlparser-ffi/src/actions/facts.rs#L4646).
- **Corpus Evidence**:
  Resolved in [`benchmarks/real_fuzz_corpus/sqlite_fuzz_fts5_savepoint.sql`](file:///home/hunter/Workspace/occam/benchmarks/real_fuzz_corpus/sqlite_fuzz_fts5_savepoint.sql), allowing independent virtual tables (e.g. `vt0` vs `vt1`) to be cleanly isolated and sliced away without whole-script global fencing.

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

## 3. Local Patching Workflow (When & How to Patch `sqlparser-rs`)

When upstream syntax limitations cause **parse failures (syntax errors or AST panics)** rather than mere semantic under-approximations, a local patch on `sqlparser-rs` is preferred over degrading the oracle.

### Patching Protocol:
1. **Clone & Branch**:
   Create a branch on the internal fork or vendor directory:
   ```bash
   git clone https://github.com/sqlparser-rs/sqlparser-rs vendor/sqlparser-rs
   ```
2. **Apply Local Patch**:
   In [`rust/sqlparser-ffi/Cargo.toml`](file:///home/hunter/Workspace/occam/rust/sqlparser-ffi/Cargo.toml), add a patch override:
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

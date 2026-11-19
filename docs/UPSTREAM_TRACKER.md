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

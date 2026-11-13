# Occam

> *Entia non sunt multiplicanda praeter necessitatem.* (Entities should not be multiplied beyond necessity.)

**Occam** minimizes multi-statement SQL while preserving a supplied interestingness predicate. Existing fuzz SQL, including its schema and setup statements, is the input; collecting real DBMS bugs is not a prerequisite.

## Predicate contract

Pass the complete candidate SQL to an external command on stdin. Exit status 0 accepts it; every nonzero status, including 125 and signal exits, rejects it. Stdout and stderr are opaque and never determine the verdict. Timeouts, process-start/I/O failures, and reset failures are separate control outcomes, not inferred from output text or reserved predicate exit codes. Occam does not interpret crashes or automatically treat failures as interesting.

```bash
cabal run occam -- input.sql \
  --oracle './interesting.sh' \
  --reset './reset-db' \
  --timeout 30 --budget 300 --max-oracle-executions 1000 \
  --out /tmp/occam-result
```

`--reset` is optional. The initial input must pass before reduction. Per-call, total-time, and uncached-execution budgets include the initial and final checks; when budgets permit, Occam performs two uncached final validations. On exhaustion, it preserves the last accepted SQL and reports a nonzero status. `stats.json` reports available measurements; per-candidate statement counts and tokens can be unknown.

`stats.json`'s `evaluation_metrics` keeps reset timings distinct: `evaluationPreflightResetSeconds` (initial/preflight), `evaluationResetSeconds` (reduction), and `evaluationValidationResetSeconds` (final validation); `evaluationPreflightOracleSeconds` records initial predicate time. Sum reset phases once; unavailable optional preflight measurements serialize as `null`. Final-validation wall time is reserved only for calls actually scheduled, so zero scheduled calls reserve zero. If initial preflight consumes the execution cap, Occam reports `execution-budget`, performs no reduction/final calls, and preserves the accepted checkpoint.

## Candidate state

Each predicate call gets a fresh `OCCAM_WORKDIR`, also used for `TMP`, `TMPDIR`, and `TEMP`. The reset command and predicate share it; the original working directory is preserved and exported as `OCCAM_ORIGINAL_CWD`. Occam does not isolate or reset a real database for you: the external predicate/reset must provide the needed database state and isolation. Owned POSIX process groups are cleaned up; descendants that leave the group and full Windows process-tree behavior are outside the verified contract.

## Architecture

- The Rust `sqlparser-rs` frontend receives an explicit dialect and returns statement boundaries and dialect-normalized dependency facts. It emits byte edits only when a node has a complete, exact source range; partial `Spanned` ranges are not treated as whole-node spans. When a compound node lacks a complete range (including some unknown, window, NULL-treatment, or ODBC forms), only recursively verified child edits and statement-level deletion are offered; AST display text is never used to regenerate SQL.
- Haskell dependency analysis tracks recognized schema generations, table-owned columns/indexes, lexical CTE scopes, and transaction/savepoint catalog snapshots. Global unknown effects establish a persistent fence: subsequent statements form a dependency chain so hidden lineage or writes cannot bypass it, adding one inherited fence edge per statement rather than dense all-prior edges. This deliberately sacrifices graph precision for the rest of the script; oracle-gated graph-free reduction remains available.
- Graph-guided search first tests the last statement's backward slice; rejection falls back to linear prefix search without assuming a monotone predicate. Unrestricted deletion and a fixed-point statement audit follow; byte-preserving passes reduce projections, VALUES rows, CTEs, joins, clauses, types, literals, and literal scalar subqueries. These are local guarantees, not a global minimum.
- External predicates define the behavior being preserved; Occam does not launch or configure a DBMS.

## Dialect semantic coverage

The frontend pins `sqlparser-rs` 0.64.0 and selects one of the 16 dialects below. **Syntax accepted**, **modeled dependency effects**, and **opaque effects** are different outcomes: parser acceptance is not evidence that an engine accepts the SQL or that every effect is modeled. Statement-local syntax errors retain their source units; FFI/transport failures remain separate.

Recognized AST paths share named table/view/index/trigger/sequence/type/schema definitions, drops, selected column/constraint alterations and renames, DML targets, query sources, lexical CTE/alias scopes, and transaction/session facts. These are dependency models, not a SQL interpreter. Dialect-specific paths are grouped here:

| Selected dialects | Shared paths and important specializations |
| --- | --- |
| Generic, ANSI, Teradata | Shared AST effect extraction with selected identifier folding; no inferred engine-specific meaning for `SELECT INTO` or `USE`. |
| PostgreSQL, Redshift | Quoted/unquoted identifier rules, search paths, table-target `SELECT INTO`, transaction/savepoint snapshots, and provable qualified sequence calls. |
| MySQL | Configurable `lower_case_table_names` for relation/namespace parts; multi-table UPDATE/DELETE targets; implicit DDL commits; optionless `CREATE DATABASE` and simple one-level `USE`. |
| SQLite, DuckDB | Case-insensitive identifier matching and CTAS snapshots; dedicated SQLite foreign-key PRAGMA effects; explicit two-part DuckDB `USE catalog.schema`. |
| Oracle, Snowflake | Upper-folded unquoted identifiers and sequence pseudocolumn syntax; Oracle implicit DDL commits; complete two-part Snowflake schema selection. |
| ClickHouse, Hive, Spark, Databricks | Shared named DDL/DML/query extraction; simple one-level ClickHouse/Hive namespaces and complete two-part Spark/Databricks namespaces. Persistent routing and partition/overwrite effects below remain opaque. |
| BigQuery | Separate relation/column case rules and shared AST/source extraction; partial namespace changes are not assumed complete. |
| MsSql | Case-insensitive matching, UPDATE-FROM alias resolution, and table-target `SELECT INTO`; `USE database` alone is opaque because the current schema is not proven. |

Newly completed dependency behavior:

- `(TABLE t)`, nested/CTE/set-operation TABLE bodies, and `CREATE TABLE copied AS TABLE t` retain their source rows. TABLE and `SELECT *`/qualified `t.*` retain whole-row dependencies even alongside explicit predicates or ordering, without expanding an unrelated qualified source's columns.
- Direct standalone `SELECT a INTO copied FROM src` creates a snapshot in PostgreSQL/Redshift/MsSql. Procedural variables, nested/set-operation INTO contexts, and other dialect meanings remain opaque.
- CTAS `WITH NO DATA` binds source schema without consuming source rows. CTAS and materialized-view reads/writes no longer follow later base-table data changes; ordinary views remain live. `REFRESH MATERIALIZED VIEW` explicitly reads recorded sources, distinguishing `WITH DATA` from `WITH NO DATA`.
- `ALTER VIEW s.v SET SCHEMA destination` preserves lineage at the new name; unqualified renames retain the resolved source schema. Recognized schema-owned definitions, namespace selection, and moves retain schema-generation dependencies. Ambiguous mutation targets are fenced instead of mutating every possible match.

Unproved scalar routines and deferred stored expressions (including sequence defaults) remain opaque. PostgreSQL/Redshift sequence calls are precise only for explicit `pg_catalog.nextval`/`setval`/`currval` with provable literal names, including supported text/regclass casts; unqualified/custom calls, dynamic targets, and `lastval` remain opaque. Nontransactional sequence advances inside an explicit transaction or with autocommit disabled are fenced: durable values across rollback/drop/recreation are not modeled as transactional rows. Oracle/Snowflake unquoted sequence pseudocolumns retain their distinct syntax; ambiguous quoted members remain opaque.

Other explicit opaque boundaries include privileges/routine statements, unsupported session/transaction settings and generic variable `SET`/`RESET`, partial/default namespace changes, COPY/bulk/external-I/O forms, INSERT overwrite/partition/multi-table forms, DML `OUTPUT INTO`, table inheritance/`PARTITION OF`, persistent `CREATE VIEW ... TO` routing, unproved table functions/computed row expansion, unsupported view changes, and option-bearing database creation. Unknown global effects establish the persistent fence described above; recording a name or setting is not a substitute for modeling its effects.

Parser limits remain independent: the selected frontend rejects standalone `TABLE t`, although `(TABLE t)` and the nested forms above parse; the probed CTAS `WITH NO DATA` form is rejected under Snowflake. Conversely, cross-dialect parser acceptance of REFRESH/ALTER VIEW does not establish DBMS validity.

Proof boundaries: the batch passed 99 dependency tests, a 16-dialect graph smoke, and 640 selected-frontend syntax/fact probes before the whole-row correction. That correction passed six targeted consumer scope/TABLE checks and five native alias/LATERAL checks. The final temporary-schema alias normalization then passed a rebuilt, row-seeded whole-row regression covering five queries; the earlier checks were not rerun. Real SQLite and DuckDB CLI reductions preserved populated CTAS snapshots and passed final validation twice each before these whole-row corrections; these are not execution proofs for the other selected dialects.

## Building

```bash
cabal build
cabal test
```

`nix develop` provides GHC, Cabal, Rust/Cargo, rust-analyzer with matching standard-library sources, Rustfmt, Clippy, Fourmolu, HLint, and HLS. Run `make hooks` once per checkout to enable the repository's pre-commit hook.

The Cabal setup hook builds the locked Rust crate and bundles its static library during ordinary builds; no manual Cargo pre-build is required. `Dockerfile.benchmark` uses a two-stage build and copies only the Occam binaries and benchmark entrypoint into runtime. `benchmarks/docker_run.sh` bind-mounts the checkout at `/workspace`.

The source distribution also includes the Rust crate and `Cargo.lock`; an extracted archive build and an installed-library consumer outside the checkout were verified.

`make check` checks Haskell/Rust formatting, runs HLint, builds all Cabal components with GHC warnings treated as errors, runs the Cabal and Cargo test suites, and runs Rust check/clippy with warnings as errors. The pre-commit hook runs `make fmt-check lint build-check rust-check`: it checks the working tree, including `bench/`, rather than a staged snapshot, and does not execute tests. Missing formatter/linter commands fall back to `nix develop`; unavailable tools or failed checks stop the hook instead of being skipped.

The bounded generated properties check slicing and closure against independent `Data.Graph` reachability for every node and root subset on graphs of at most eight statements, and compare two-table SQLite generation/latest-write snapshots across drop/recreate, full rollback, savepoint rollback+release, and outer release. Catalog comparisons exclude transaction-control barrier IDs; they do not prove nested-savepoint, column/index/view, or general SQL semantics.

See `benchmarks/README.md` for processing existing fuzz SQL and comparing reducers.

## License

MIT License.

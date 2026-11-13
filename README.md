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

The frontend uses the selected Generic, PostgreSQL, MySQL, SQLite, DuckDB, ClickHouse, Oracle, ANSI, BigQuery, Databricks, Hive, MsSql, Redshift, Snowflake, Spark, or Teradata dialect. Statement-local SQL parse errors stay attached to their source units; FFI/transport failures remain separate. Unknown dependency effects stay conservative, and the supplied predicate decides every candidate.

Unproved scalar-function effects and deferred expressions in stored definitions (including sequence defaults) remain opaque, rather than being assumed pure or executed at definition time. PostgreSQL/Redshift sequence calls are precise only for explicit `pg_catalog.nextval`/`setval`/`currval` with provable literal names (including text/regclass casts); unqualified or custom calls, dynamic targets, and `lastval` remain opaque. Oracle/Snowflake unquoted sequence pseudocolumns retain their distinct syntax; ambiguous quoted members remain opaque. Unsupported view changes preserve existing lineage behind a fence; recognized view renames move it rather than replacing it with an empty definition.

Unsupported session/transaction changes and generic variable `SET`/`RESET` effects also establish the global fence: recording a value alone does not model its effects on parsing, triggers, writes, or transaction behavior. Dedicated modeled settings retain their narrower semantics; unknown state changes do not leave the catalog falsely precise.

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

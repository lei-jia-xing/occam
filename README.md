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

`--jobs N` (default `1`, must be positive) bounds concurrent oracle/reset executions and declares that the predicate AND reset support `N` concurrent independent calls with independent database state. The scheduler enforces at most `N` in flight; it never provides database isolation itself. With `--jobs 1` all external interactions serialize on the same scheduler; higher counts only help when the external predicate/reset is genuinely concurrency-safe.

`stats.json`'s `evaluation_metrics` keeps reset timings distinct: `evaluationPreflightResetSeconds` (initial/preflight), `evaluationResetSeconds` (reduction), and `evaluationValidationResetSeconds` (final validation); `evaluationPreflightOracleSeconds` records initial predicate time. Sum reset phases once; unavailable optional preflight measurements serialize as `null`. Final-validation wall time is reserved only for calls actually scheduled, so zero scheduled calls reserve zero. If initial preflight consumes the execution cap, Occam reports `execution-budget`, performs no reduction/final calls, and preserves the accepted checkpoint.

Concurrency accounting lives in `evaluation_metrics`: the job count is `evaluation_budget.evaluationJobs` (no top-level echo); `evaluationCancelledObservations` counts speculatively started tasks killed or dropped without producing an observation (never timeouts or errors); `evaluationCacheJoins` counts duplicate candidates coalesced onto one in-flight physical execution. `evaluationOracleAttempts` counts every physical start including speculation; `evaluationDatabaseObservations` counts actually spawned oracle processes. Top-level `stats.json` echoes cancellations as `cancelled_observations` and joins as `cache_joins`.

## Candidate state

Each predicate call gets a fresh `OCCAM_WORKDIR`, also used for `TMP`, `TMPDIR`, and `TEMP`. The reset command and predicate share it; the original working directory is preserved and exported as `OCCAM_ORIGINAL_CWD`. Occam does not isolate or reset a real database for you: the external predicate/reset must provide the needed database state and isolation. A fresh workdir is filesystem hygiene only and never substitutes for database isolation under `--jobs N`.

Each in-flight execution holds one scheduler slot from reset spawn through oracle execution and process-group cleanup; the slot releases only after the owned worker is fully reaped. On timeout or cancellation the owned POSIX process group is killed before pipe helpers are joined, so a writer blocked on a full pipe nobody reads cannot stall cleanup. If an owned leader cannot be reaped after `SIGKILL` (kernel-wedged, `D`-state), Occam fails loudly instead of claiming cleanup and refuses further oracle executions. Descendants that call `setsid` escape the group, and full Windows process-tree behavior is outside the verified contract; process groups are job control, not a security sandbox.

## Architecture

- The Rust `sqlparser-rs` frontend receives an explicit dialect and returns statement boundaries and dialect-normalized dependency facts. It emits byte edits only when a node has a complete, exact source range; partial `Spanned` ranges are not treated as whole-node spans. When a compound node lacks a complete range (including some unknown, window, NULL-treatment, or ODBC forms), only recursively verified child edits and statement-level deletion are offered; AST display text is never used to regenerate SQL.
- Statement-only parses are facts-only by default: the frontend omits unused per-edit syntax IR entirely (no allocation, JSON, or decode for it) while returning identical statements, boundaries, and dependency facts. The syntax, column, clause-pruning, and expression-simplification passes explicitly opt back into edit IR per call. Candidate construction policy and oracle behavior are unchanged.
- Haskell dependency analysis tracks recognized schema generations, table-owned columns/indexes, lexical CTE scopes, and transaction/savepoint catalog snapshots. Global unknown effects establish a persistent fence: subsequent statements form a dependency chain so hidden lineage or writes cannot bypass it, adding one inherited fence edge per statement rather than dense all-prior edges. This deliberately sacrifices graph precision for the rest of the script; oracle-gated graph-free reduction remains available.
- Graph-guided search tests the last statement's backward slice and probes prefixes with binary search. Prefix probes are a heuristic: for nonmonotone predicates they need not find the earliest accepted prefix; every selected candidate still requires oracle acceptance. Unrestricted deletion and a fixed-point statement audit follow; byte-preserving passes reduce projections, VALUES rows, CTEs, joins, clauses, types, literals, and literal scalar subqueries. These are local guarantees, not a global minimum. Statement deletion uses single-pass retained-slice assembly and step-hoisted candidate rendering (see [Statement surgery benchmarks](benchmarks/README.md#single-pass-retained-span-assembly-and-candidate-hoisting-2026-11-14)).
- External predicates define the behavior being preserved; Occam does not launch or configure a DBMS.
- One ordered scheduler serves every search phase with a hard `--jobs` bound on concurrent physical executions (default 1, same scheduler at 1). Candidates are tested in decision order in bounded windows: cache entries and execution permits are reserved in order before forking, so an early head never loses its permit to later lookahead. Retirement commits the earliest acceptance, surfaces the earliest inconclusive or budget denial, then cancels and reaps losers; completed-but-unselected physical work stays billed while killed tasks count as cancelled. Curve, progress, and checkpoint updates happen only at ordered retirement, and every window drains fully before reparse, the next phase, or serial final validation. Duplicate bytes single-flight onto one execution when caching is on; timeouts, failures, and cancellations are never cached and never convert to rejections.
- The opt-in `--intra-statement` search has no hidden rejection/attempt cutoffs; explicit Evaluation budgets can interrupt oracle calls. It uses `phase-forward-sweep-list-chunks` in stable category order—Clause, List, Constraint, Type, Expression, then Literal—and preserves parser-emitted order within each fresh category snapshot.
  Rejection or nonshrinking advances the ordinal; after an accepted strict shrink, the SQL is reparsed and traversal resumes at the accepted ordinal. Candidates before the cursor, including newly inserted or shifted edits, are revisited from ordinal zero in the next complete cycle. All categories finish before a new cycle, which repeats iff the prior cycle shrank SQL; only a complete unchanged cycle establishes exhaustion of currently generated edits.
  Within the List phase, each comma-separated list first offers coarse-to-fine contiguous chunk deletions (halves, quarters, ...) before its individual single-item deletions; every chunk leaves a survivor, reuses the single-item separator convention, and only the boundary gap is separator-gated. All single-item deletions keep their exact legacy spans and order. Only the syntax pass requests chunks per parse; statement-only parses keep the legacy payload without chunk materialization.
  This forward priority explicitly differs from restart-from-zero scheduling and can change which checkpoint is reached under a finite budget.
  Coverage remains conservative: only complete exact source spans are editable, and every accepted candidate must strictly shrink bytes; equal-length canonicalization such as `1` to `0` remains unsupported. Chunking expands deletions within the existing verified list forms; it does not add AST-node coverage or establish global minimality.
  Column-aware reduction is an explicit `--column-mode off|single|atomic` (default `off`, requires `--intra-statement`). `single` adds one Column category of per-occurrence deletions (a `CREATE TABLE` definition, an `INSERT` list item, or one `VALUES` item) to the same sweep. `atomic` additionally tries one joint candidate per column that deletes its definition together with every matching list item and row position in a single oracle verdict; bundles form only for exact, unambiguous, fence-free table scopes and are otherwise skipped with a recorded cause, never partially applied. Single-point moves are identical across both modes; the three-way comparison is over fixed-point outcomes, not traces.
  Cost attribution is per family and label: retired decisions (including cache hits) are counted separately from physical starts and completions, finished-but-unselected losers carry their measured wall, and canceled or failed work carries interaction wall rather than CPU. Candidate byte-assembly cost is attributed per family for both syntax and statement candidates. Skipped candidates record reasons instead of oracle verdicts. With no metrics store attached, no measurement clocks run.

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

Unproved scalar routines and deferred stored expressions (including sequence defaults) remain opaque, except unqualified pure builtins on SQLite/PostgreSQL/MySQL (eight scalars — `abs`, `coalesce`, `concat`, `length`, `lower`, `nullif`, `round`, `upper` — plus plain-shape `avg`, `count`, `max`, `min`, `sum`) and unquoted `coalesce`/`nullif` on GenericSQL, in evaluated and stored positions; MySQL index length prefixes (`b(1)`) are excluded structurally in key position; other aggregates, volatile/sequence names, qualified calls, window/filter/grouping shapes, and any script containing routine DDL or a parse error stay fenced. PostgreSQL/Redshift sequence calls are precise only for explicit `pg_catalog.nextval`/`setval`/`currval` with provable literal names, including supported text/regclass casts; unqualified/custom calls, dynamic targets, and `lastval` remain opaque. Nontransactional sequence advances inside an explicit transaction or with autocommit disabled are fenced: durable values across rollback/drop/recreation are not modeled as transactional rows. Oracle/Snowflake unquoted sequence pseudocolumns retain their distinct syntax; ambiguous quoted members remain opaque.

Other explicit opaque boundaries include privileges/routine statements, unsupported session/transaction settings and generic variable `SET`/`RESET`, partial/default namespace changes, COPY/bulk/external-I/O forms, INSERT overwrite/partition/multi-table forms, persistent `CREATE VIEW ... TO` routing, unproved table functions/computed row expansion, unsupported view changes, and option-bearing database creation. Modeled session effects are scoped `foreign_key_checks` assignments (bare/`SESSION`/`GLOBAL`/`@@` forms; `@user` variables and foreign qualifiers still fence) and inert SQLite storage-pragma records (`journal_mode`, `synchronous`, `temp_store`, `cache_size`, `page_size`; `recursive_triggers` and effectful pragmas still fence). Partition/inheritance routing (`PARTITION OF`/`INHERITS` parents, `ATTACH`/`DETACH PARTITION`, `ONLY`-aware parent/child reads and writes) and MSSQL `OUTPUT INTO` (real table as a second write edge, `@var` as a relation-scoped fence, anything else fenced) are modeled. Unknown global effects establish the persistent fence described above; merely recording an unmodeled name is not a substitute for modeling its effects.

Parser limits remain independent: the selected frontend rejects standalone `TABLE t`, although `(TABLE t)` and the nested forms above parse; the probed CTAS `WITH NO DATA` form is rejected under Snowflake. Conversely, cross-dialect parser acceptance of REFRESH/ALTER VIEW does not establish DBMS validity.

Proof boundaries: real SQLite and DuckDB CLI reductions passed final validation twice each; these are not execution proofs for the other selected dialects.

## Building

```bash
cabal build
cabal test
```

`nix develop` provides GHC, Cabal, Rust/Cargo, rust-analyzer with matching standard-library sources, Rustfmt, Clippy, Fourmolu, HLint, HLS, and Python 3. Run `make hooks` once per checkout to enable the repository's pre-commit hook.

The Cabal setup hook builds the locked Rust crate during ordinary builds; static (`-f static-rust`) builds additionally bundle it, so no manual Cargo pre-build is required. `Dockerfile.benchmark` uses a two-stage build and copies only the Occam binaries and benchmark entrypoint into runtime. `benchmarks/docker_run.sh` bind-mounts the checkout at `/workspace`.

Dev builds link the Rust frontend dynamically at load time: edit Rust, run `cargo build --locked --release --manifest-path rust/sqlparser-ffi/Cargo.toml --target-dir rust/sqlparser-ffi/target` (as `make build`, `make build-check`, and `make test` do first), then rerun the same binary with zero Haskell rebuild. The loader finds the cdylib via an absolute rpath to `rust/sqlparser-ffi/target/release` baked at configure time, so FFI signature changes still require a Haskell rebuild.
Release builds pass `-f static-rust` for a single self-contained binary (used by `Dockerfile.benchmark` and the release workflow). Windows is static-only in v1.
Bare `cabal build` does not run Cargo when Cabal considers Haskell fresh — use a make target (or run Cargo manually) after Rust edits.

The source distribution also includes the Rust crate and `Cargo.lock`; an extracted archive build and an installed-library consumer outside the checkout were verified.

`make check` checks Haskell/Rust formatting, runs HLint, builds all Cabal components with GHC warnings treated as errors, runs the Cabal and Cargo test suites, runs Rust check/clippy with warnings as errors, and runs the Python benchmark test suite (`make benchmark-test`). The pre-commit hook runs `make fmt-check lint build-check rust-check`: it checks the working tree, including `bench/`, rather than a staged snapshot, and does not execute tests. The Python benchmark suite (`python3 -m unittest discover -s benchmarks`) exercises benchmark tooling, reset contracts, and native reducer wrappers; tests requiring `exe:occam` or `exe:occam-eval` skip automatically if the Cabal targets are not yet built. Missing formatter/linter commands fall back to `nix develop`; unavailable tools or failed checks stop the hook instead of being skipped.

`--no-unrestricted` disables only the reducer's unrestricted fallback search pass.
The final deletion audit is independent of that flag and always runs.

The bounded generated properties check slicing and closure against independent `Data.Graph` reachability for every node and root subset on graphs of at most eight statements, and compare two-table SQLite generation/latest-write snapshots across drop/recreate, full rollback, savepoint rollback+release, and outer release. Catalog comparisons exclude transaction-control barrier IDs; they do not prove nested-savepoint, column/index/view, or general SQL semantics.

See `benchmarks/README.md` for processing existing fuzz SQL and comparing reducers.

## License

MIT License.

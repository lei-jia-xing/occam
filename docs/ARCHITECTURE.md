# Architecture & Execution Contract

This document describes Occam's reduction pipeline, execution scheduler, external predicate contract, `stats.json` schema conventions, and verification boundaries.

---

## 1. System Pipeline

```
SQL input
  │
  ▼
Pure-Haskell Strict Frontend (lexer, statement splitter, dialect LL(k)/Pratt parser, semantic facts, exact-span edits & column IR)
  │
  ▼
Haskell Dependency Analysis (schema generations, scopes, transaction snapshots, fences)
  │
  ▼
Multi-Stage Reducer (slice → closure → ddmin → audit → optional intra-statement)
  │
  ▼
Ordered Bounded-Window Scheduler (cache + single-flight + process-group hygiene)
  │
  ▼
External Predicate (--oracle / --reset)  OR  In-Process Result Oracle (SQLite / DuckDB)
```

### Pure-Haskell Strict Frontend (`src/Occam/Frontend`, `src/Occam/Parser.hs`)

- 100% native Haskell with zero FFI/Cargo/C-toolchain dependencies for SQL parsing, lexing, statement splitting, semantic fact extraction, exact-span edit generation, column IR assembly, and oracle-source proofs.
- Dispatches explicitly on `--dialect` (`postgresql`, `mysql`, `mariadb`, `sqlite`, `duckdb`, `clickhouse`) via `DialectProfile` with zero generic fallback and zero cross-dialect grammar bleed. Client `DELIMITER` directives adjust splitting and are excluded from the SQL statement count.
- **Decoupled statement boundaries & exact-span edits only**: statement splitting runs on the token stream before statement-level grammar parsing, so syntax errors in one unit never corrupt surrounding statement spans. Intra-statement edits carry typed priority tiers (`ClauseTier` / `ExprTier` via `EditTier`) and exact UTF-8 byte spans only—AST pretty-printing is never used to regenerate SQL.
- **Selective pass execution**: statement-level parses omit intra-statement edit IR and column bundles while returning identical statement spans and dependency facts. Syntax, column, clause-pruning, and expression-simplification passes opt into edit/column IR explicitly per call.
- The grammar target is each engine's latest stable release, with no version dimension; see [Frontend Grammar](FRONTEND_GRAMMAR.md) for the module map, span/trivia contract, per-dialect coverage, and seed corpus.

### Dependency Analysis (`src/Occam/Dependency.hs`)

- Tracks recognized schema generations, table-owned columns/indexes, lexical CTE scopes, and transaction/savepoint catalog snapshots.
- **Persistent global fence**: an unmodeled global effect establishes a fence so subsequent statements form a dependency chain that hidden lineage or writes cannot bypass (adding one inherited fence edge per statement rather than dense all-prior edges). This trades graph precision for soundness across the rest of the script.
- When both unrestricted fallback and closure search are enabled, the reducer tries unrestricted deletion first if the current graph contains a global fence.
- See [Dialect Semantic Coverage](DIALECTS.md) for per-dialect modeling details and opaque boundaries.

### Evaluation and effect boundaries (`src/Occam/Evaluation.hs`, `src/Occam/Dependency.hs`)

- Catalog operations use strict `State Eval` from `transformers`, dispatched in `CatalogOp` order; pure read and rename helpers remain pure where useful.
- Keep existing `ExceptT`, `MaybeT`, and `ContT` in their confined IO flows. Their local/shared unlift and continuation scopes are not equivalent to a broad effect runtime.
- `effectful-core` is available in Stackage LTS 24.68, but adopting it adds dependencies and requires local-versus-shared unlift and continuation-scope choices that do not match the current confined transformer flows. No measured performance benefit is asserted; preserve the specialized lexer/parser loops and ordered pool scheduler without adopting `effectful`.

### Reduction Stages (`src/Occam/Reducer.hs`)

1. **Root selection & backward slice**: starts from the last result-producing statement observed by `--result-oracle` when engine and frontend statement counts agree, otherwise from the script's last statement. Tests that statement's backward slice and probes prefixes with binary search (a heuristic for nonmonotone predicates; every selected candidate still requires oracle acceptance).
2. **Closure & statement search**: dependency-guided closure reduction and statement-level ddmin (`--statement-order`, default `ranked-fallback`), using single-pass retained-slice assembly and step-hoisted candidate rendering.
3. **Unrestricted fallback**: tests unrestricted statement deletions when graph edges may over-constrain reduction (`--no-unrestricted` disables only this pass).
4. **Fixed-point statement audit**: runs independently of `--no-unrestricted` to establish statement 1-minimality.
5. **Opt-in intra-statement rewrites (`--intra-statement`)**:
   - Uses `phase-forward-sweep-list-chunks` in stable category order: **Clause → List → Constraint → Type → Expression → Literal**, preserving parser-emitted order within each category snapshot, with no hidden rejection/attempt cutoffs (only explicit budgets interrupt).
   - Rejections or non-shrinking edits advance the cursor; an accepted strict shrink reparses and resumes at the accepted ordinal. Earlier edits (including shifted/new ones) are revisited in the next cycle, repeating until a full unchanged cycle completes.
   - Within **List**, comma-separated lists offer coarse-to-fine contiguous chunk deletions (halves, quarters, ...) leaving at least one survivor before single-item deletions. Only the syntax pass materializes chunks.
   - Conservative by design: every accepted edit must strictly shrink bytes (equal-length canonicalizations such as `1` → `0` are unsupported). Forward priority differs from restart-from-zero scheduling and can reach a different checkpoint under a finite budget.
   - **Column co-reduction (`--column-mode off|single|atomic`, requires `--intra-statement`)**: `single` adds per-occurrence column deletions (`CREATE TABLE` definitions, `INSERT` column items, `VALUES` items); `atomic` also attempts one joint bundle per column across exact, unambiguous, fence-free table scopes (skipping with a recorded cause rather than partially applying).

All stages provide **local minimality guarantees**, not a global minimum.

---

## 2. Ordered Scheduler & Caching (`src/Occam/Pool.hs`, `src/Occam/Cache.hs`)

- Every search phase uses ordered retirement under the `--jobs N` concurrency cap, which defaults to one. The audit and syntax sweeps build candidate SQL as slots become available.
- The scheduler reserves cache entries and execution permits in candidate order before forking. Native probes choose a width at the start of each window from the stopping positions of previous completed runs.
- Retirement commits the earliest acceptance, surfaces the earliest inconclusive result or budget denial, then cancels and reaps losers. Completed-but-unselected physical work remains billed; killed tasks are counted as cancelled.
- When a launched candidate has already published a decisive result, the scheduler leaves later slots empty while it retires the earlier candidates in order.
- Progress, curve, and checkpoint updates occur only at ordered retirement, and every window drains fully before reparse, phase transition, or serial final validation.
- With caching enabled (default), duplicate candidate bytes single-flight onto one in-flight execution (`cache_joins`). Timeouts, errors, and cancellations are never cached and never converted into rejections.
- Native SQLite and DuckDB result oracles can also share a key for ordinary whitespace variations of explicitly aliased literal SELECTs. The Haskell lexer preserves the original token bytes, and the oracle receives the original candidate.

### Adaptive native lookahead

A finished run contributes a sample `(n, accepted)` when all `n` retired candidates used fresh oracle executions. Runs that hit or join the cache, exhaust the execution budget, or end inconclusively leave the estimate unchanged. The configured width is used until the first sample.

In the cost model, `n` ordered decisions take `b = ceil(n / w)` equal-duration waves at width `w`. The model charges `b + γ(bw − n)` when a candidate accepts and `b` when the source exhausts. The acceptance cost prices every unused slot in the final wave as a speculative start. The scheduler uses `γ = 1` and selects the smallest width with minimum cumulative cost. A width larger than every observed run fits every sample in one wave and can only increase the predicted waste. The table therefore stores at most `--jobs` scores and updates them once per completed sample.

With independent candidates, a stationary acceptance probability `p > 0`, and an available tail, the expected number of waves is `1 / (1 − (1 − p)^w)`. Expected cost per retired probe is then `p(1 + γw) / (1 − (1 − p)^w) − γ`. Keeping the stopping positions also covers workloads where the same marginal acceptance rate concentrates at particular ordinals.

For independent, identically distributed completed runs of length at most `N`, let `K` be the configured maximum width and `B = N + γ(K − 1)` bound the loss for any width. At `m` samples, each width's empirical loss has variance at most `B² / (4m)`. Bounding the maximum estimation error by the Euclidean norm and comparing the empirical and true minimizers gives expected excess loss at most `B sqrt(K / m)` for the next sample. Summing over `T` updates gives `O(B sqrt(KT))` regret against the best fixed width in this model. Real sliding windows can also refill before a full wave finishes, and execution durations can change across phases.

---

## 3. Predicate, Budget & Process Hygiene Contract

### External Predicate (`--oracle` / `--reset`)

- Candidate SQL is sent on `stdin`. **Exit `0` accepts**; **every nonzero exit** (including `125` and signal exits) **rejects**.
- `stdout` and `stderr` are opaque and never influence the verdict.
- Timeouts, process-start/I/O failures, and `--reset` failures are distinct control outcomes, never inferred from output text or reserved predicate exit codes. Occam does not interpret crashes or treat failures as interesting automatically.

### Candidate Workdir & Process Groups (`src/Occam/Oracle.hs`)

- Each execution receives a fresh temporary directory exported as `OCCAM_WORKDIR`, `TMP`, `TMPDIR`, and `TEMP` (shared by `--reset` and `--oracle`). The caller's original working directory is exported as `OCCAM_ORIGINAL_CWD`.
- Set the parent process's `TMPDIR` to a writable tmpfs directory to place these fresh per-candidate directories on tmpfs.
- **Database isolation is the caller's responsibility**: `OCCAM_WORKDIR` provides filesystem hygiene only and never isolates a shared database under `--jobs > 1`. With `--jobs N`, both `--oracle` and `--reset` must safely support `N` concurrent independent runs with independent database state.
- Each in-flight worker holds a scheduler slot from reset spawn through oracle exit and process-group reaping. On timeout or cancellation, the owned POSIX process group is killed before pipe helpers are joined so blocked writers cannot stall cleanup.
- If an owned process leader cannot be reaped after `SIGKILL` (e.g., kernel `D`-state), Occam fails loudly and refuses further oracle executions. Descendants calling `setsid` escape process groups, and full Windows process-tree cleanup is outside the verified contract; process groups provide job control, not a security sandbox.

### Budgets & Final Validation (`src/Occam/Evaluation.hs`)

- The initial input must pass before reduction begins.
- Shared `--budget` (wall time) and `--max-oracle-executions` (uncached executions) budgets include initial preflight and final checks.
- After preflight, Occam gives reduction the first available attempt, reserves room for up to two final validation checks when the budget allows, and uses remaining budget for uncached final checks.
- If preflight exhausts the execution cap, Occam reports `execution-budget`, performs no reduction/final calls, and preserves the accepted checkpoint.
- On budget exhaustion at any stage, Occam preserves the last accepted SQL checkpoint and exits nonzero (`124`).

### In-Process Result Oracle (`--result-oracle`)

- Available with `--dialect sqlite` or `--dialect duckdb`; replaces both `--oracle` and `--reset`.
- Evaluates the complete candidate in isolated in-memory state with external data access restricted. It preserves the **last result set** produced by the original script, including ordered rows, column names, and value types. Reduction may reuse an exact deterministic prefix under the same result contract. Each final validation starts with a fresh database and executes the entire SQL without replay.
- Trailing statements after the last result set must still execute without error. Zero-row results still compare column names; scripts without any result set cannot seed a contract. Use deterministic queries and `ORDER BY` to fix row order.
- SQLite starts with foreign keys disabled (matching the reducer's session model). Scripts can enable them via `PRAGMA foreign_keys = ON`. A probe shares one deadline across execution and checkpoint work.
- DuckDB loads its native shared library dynamically (`OCCAM_DUCKDB_LIB` if not on the standard library path). Decimal results compare exact values, precision, and scale. DuckDB reports unsupported types in the compared result set. Earlier ignored result sets may contain other types.

---

## 4. `stats.json` Accounting Conventions

When `--stats FILE` is passed, Occam writes an atomic JSON report:

- **Phase-separated reset timings** (`evaluation_metrics`): `evaluation_preflight_reset_seconds`, `evaluation_reset_seconds`, and `evaluation_validation_reset_seconds` are tracked distinctly (summed once, never double-counted); `evaluation_preflight_oracle_seconds` records initial predicate time; unavailable optional measurements serialize as `null`.
- **Concurrency & caching**:
  - `evaluation_budget.evaluation_jobs` records the configured concurrency (no top-level echo).
  - `evaluation_oracle_attempts` counts every physical start including speculative runs.
  - `evaluation_database_observations` counts spawned oracle processes or started in-process DB observations.
  - `evaluation_cancelled_observations` (echoed top-level as `cancelled_observations`) counts speculatively started tasks killed/dropped without producing an observation (never timeouts or errors).
  - `evaluation_cache_joins` (echoed top-level as `cache_joins`) counts duplicate candidates coalesced onto an in-flight execution.
- **Per-family cost attribution**: `snapshot_families` serializes as an array of `{tag, counters}` records where `tag` carries a structured `kind` discriminator (`unknown`, `statement`, `syntax`, or `bundle`). Retired decisions, physical starts/completions, finished-but-unselected losers, cancelled/failed wall time, candidate byte-assembly cost, and skipped-candidate causes are accounted separately; when no metrics store is attached, no measurement clocks run.

---

## 5. Verification Boundaries

- **Formal proofs (`proofs/`)**: Lean 4 theorems verify DAG acyclicity, backward-slice soundness, reduction monotonicity/termination, and 1-minimality on the abstract model (see [proofs/README.md](../proofs/README.md)).
- **Bounded property tests (`test/`)**: verify slicing and closure against independent `Data.Graph` reachability across all nodes and root subsets on graphs up to 8 statements, and compare two-table SQLite generation/latest-write snapshots across drop/recreate, rollback, savepoint rollback/release, and outer release. These exclude transaction-control barrier IDs and do not prove nested-savepoint, column/index/view, or general SQL semantics.
- **Live engine proofs**: real SQLite and DuckDB CLI reductions passed double final validation; these do not constitute execution proofs for the remaining dialects.

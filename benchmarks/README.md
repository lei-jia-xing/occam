# Fuzz SQL reduction and benchmarks

Occam reduces existing multi-statement fuzz SQL while preserving an external predicate. Include required schema/setup statements in the input. The predicate is the target: exit 0 accepts; every nonzero or signal exit rejects. Output is not parsed for crash signatures. Database reset and isolation are the predicate's responsibility.

## Reduce an existing fuzz input

```bash
./benchmarks/live_fuzz_pipeline.sh \
  --input /path/to/fuzz.sql \
  --oracle './interesting.sh' \
  --reset './reset-db' \
  --dialect postgres \
  --timeout 30 --budget 300 --max-oracle-executions 1000 \
  --out-dir /tmp/occam-runs
```

With `--input`, an explicit `--oracle` command is required. Each candidate, including the initial input, is sent to the predicate on stdin. Exit 0 means interesting; every nonzero exit rejects. `--reset` runs before candidates but does not itself guarantee database isolation. Time, per-call and uncached-execution budgets include initial/final checks; on exhaustion the last accepted SQL is retained and the run reports failure. Each run gets an independent `run_*` directory with `input.sql` and `result.jsonl`; after the initial input is accepted, the reducer also writes `minimal.sql` and `stats.json`. If initial validation rejects or times out before CLI output, those files are absent.

The pipeline can also generate a synthetic stress input when `--input` is omitted:

```bash
./benchmarks/live_fuzz_pipeline.sh --dialect postgres --stmts 100 --case-id pg_01
```

That built-in marker predicate checks SQL text only. It is a reducer-mechanics example, not SQL execution or evidence of a DBMS bug. `--stmts` controls generated size and `--case-id` the marker.

## Experiment-line contract

There are exactly two experiment lines:

- **Internal**: the Occam full reducer and explicitly named internal controls/ablations. Each measured child runs one selected `occam-eval --profile NAME`.
- **External**: independently implemented reducer search against the same exact input, setup, predicate and declared budget. Adapters must execute against qualified DBMS targets and workloads under matching protocols.

### Primary DBMS scope and evaluation strategy (Strategy 3A)

Owner review adopted **Strategy 3A**: primary benchmark evaluation is centered on self-contained, reproducible, deterministic **SQLite** rowset predicates (scale study, internal ablations, and golden fixtures). External heavy DBMS cluster deployments (PostgreSQL, MySQL) are explicitly scoped out of the primary evaluation line to eliminate daemon fragility, socket lifecycle issues, and non-deterministic environment dependencies. Real DBMS crash harvesting is explicitly out of scope; workloads focus strictly on multi-statement DDL/DML reductions with stateful setup and deterministic rowset contracts.

External multi-tool benchmarks (SQLreduce, SQLancer) are optional reference adapters only and are explicitly outside the primary evaluation scope in favor of strictly partitioned internal controls and ablations.

#### SQLite reset and isolation model

- **SQLite**: Isolated per-child template database copy or in-memory execution within `OCCAM_WORKDIR` (`study.sqlite`).
- **Isolation boundaries and lifecycles**: Predicate/reset scripts provide deterministic rowset validation and state resets. Occam provides the per-call `OCCAM_WORKDIR` (with scoped `TMPDIR`/`TMP`/`TEMP`), enforces execution timeouts, guarantees clean POSIX process group teardown on exit or timeout, and immutably retains failures (never promoting partial checkpoints to completed reductions).
- **External DBMS adapters (reference only)**: PostgreSQL and MySQL reset models remain available as reference integration templates but are not part of the active evaluation line.

Execution mode (native CLI or API) is orthogonal to experiment line; this scale-study runner records native mode only. `reduction_track` labels the per-input workload (`whole_script` or `fixed_setup_query`), not a line or profile family. `profile_family` distinguishes statement-only controls from shared-reducer profiles; `intra_statement_edits` records whether that syntax pass is enabled, not universal AST coverage. Pairing requires matching line, input identity, track, validation, resource and count protocol. Profile names/effective settings are comparison sides: controlled ablations intentionally differ in a named mechanism. The runner preserves each selected profile as a separate record but does not form or validate homogeneous cohorts; do not pool cross-line or statement-only/syntax-enabled coverage.

The legacy multi-tool runner and reporter below do not enforce these partitions. Their mixed records are not a line-compliant leaderboard or evidence bundle.

## Internal profile worker

`occam-eval` runs exactly one named internal control or ablation per process.
It does not provide independent external baselines. `--profile` is required;
the numeric `--baseline` selector and implicit all-profile run are removed.

```bash
cabal run occam-eval -- input.sql --profile full \
  --oracle './interesting.sh' --dialect postgres \
  --budget 30 --max-oracle-executions 100 --out /tmp/profile-full
```

Profiles: `full`, `no-slicing`, `no-closure`, `graph-off`, `no-intra`,
`hard-closure`, `no-cache`, `sequential`, `statement-ddmin`,
`statement-ddmin-fixed-point`, and `statement-ddmin-with-syntax`.
`sequential` and `statement-ddmin` are direct statement-only controls.
The other profiles use the shared reducer; the two graph-free fixed-point
profiles disable/enable its intra-statement syntax pass. `profile_family` and
`intra_statement_edits` in scale-study records preserve these distinctions;
inspect the effective settings rather than treating names as coverage claims.

Multiple `--profile` options produce independent child records, not an
automatically homogeneous or paired cohort. Compare full-reducer ablations as
declared mechanism contrasts; keep statement-only controls and syntax-enabled
profiles visibly distinct rather than aggregating their coverage.
Each direct invocation selects exactly one internal profile and writes
`minimal.sql`, `profile.json` and a one-metric `benchmark.json`; use a distinct
output directory per invocation. Scale-study runs give each case × profile ×
repetition a fresh process, cache, counters, budget, physical preflight and
output directory.
`--timeout` is per predicate
execution; `--budget` caps preflight, reduction/audit and final validation;
`--max-oracle-executions` includes preflight and final validation. Reducer
`oracle_calls`, preflight debit and final-validation calls are reported
separately. Budget exhaustion remains a nonzero exit with a retained accepted
checkpoint, never a completed result. Database-state isolation remains the
predicate/reset's responsibility, not a property of process isolation.

## Scale-study fixture and internal profile driver

`tools/scale_study.py` derives SQL from the existing deep-expression golden fuzz fixture, executes it with SQLite to establish expected rowsets, and keeps each input beside its predicate metadata. It varies statement count (up to 4099 total), object count, expression depth and view fan-in separately. The corpus contains 128 related variants plus explicit reset-failure and real SQLite timeout cases; it is not 128 independent bugs or broad dialect coverage.

```bash
python3 benchmarks/tools/scale_study.py prepare /tmp/occam-study --corpus-size 128
python3 benchmarks/tools/scale_study.py profiles /tmp/occam-study/profiles.json \
  --binary "$(env -u GHCRTS cabal list-bin exe:occam-eval)" \
  --profile full --profile graph-off --repetitions 3 \
  --out /tmp/occam-study/profiles-run --timeout 2 --budget 30 \
  --max-oracle-executions 500 --outer-timeout 35
python3 benchmarks/tools/scale_study.py report /tmp/occam-study/profiles-run --base full --target graph-off
```

The internal profile driver (`scale_study.py profiles`)
and partitioned reporter (`partitioned_report.py`) provide the unified control plane for Strategy 3A;
external reducer comparisons are explicitly scoped out of the primary evaluation line.

`--repetitions` is a positive integer (default 1); children run serially in
manifest-case, profile-option, repetition order. The optional outer child
deadline defaults to `budget + max(timeout, 1)` and uses the existing
process-group helper. Timeout cleanup records requested/failed/incomplete or
unknown; normal-exit cleanup is unknown, and a requested signal is not proof
that every descendant exited. Escaped/reparented children are not guaranteed.
Each child receives its own `GHCRTS=-N1` environment without mutating the
runner's environment.

There is no separate warm-up or CPU-affinity pinning; the native cache setting
is recorded from each profile metric, while the OS page cache is not reset.
Each child directory preserves exact `command.json`, opaque `stdout.log` and
`stderr.log`, and any produced `benchmark.json`, `profile.json` or
`minimal.sql` checkpoint; rows retain their raw/expected artifact paths.
`run.json` records run/config/order, Python and native binary identity, actual input
checksums, and configured predicate/reset command fingerprints (not their
transitive scripts or dependencies). JSONL rows carry unique run/child IDs,
command/artifact paths, input/output hashes, structured terminal observations,
and native return code independently. `wall_time_sec` is child-reported;
`outer_wall_time_sec` includes process startup and bounded cleanup, while the
summary's `wall_time_sec` is the observed serial batch makespan. Byte lengths
and hashes use exact bytes; statement counts require nonnegative native typed
`countStatements(dialect)` metrics, and token counts remain null. Missing or
malformed metrics, spawn failures and outer timeouts are retained per child
and do not suppress later children.

`cleanup_reason` records observed helper failures. Linux attempts `/proc`
descendant enumeration and signals the owned POSIX group; other POSIX systems
signal the group without enumeration, and Windows targets only the direct
process. Escaped/reparented descendants are not guaranteed to be killed.

Reset durations are split into preflight, reduction and final-validation
phases; their sum remains null if any phase is unknown. Configured reset-command
call counts are uninstrumented and stay null. With no configured reset command,
zero runner-issued reset calls does not establish that a predicate has no
private reset behavior.

Each predicate observation receives a temporary candidate workdir with scoped
`OCCAM_WORKDIR`/`TMPDIR`; configured reset and predicate run there, and the
directory is removed afterward. For the SQLite study, the file-backed reset
and state signature check qualify that fixture path only. Process/output
separation does not establish isolation for arbitrary server databases,
external state or escaped descendants.

The old `results/2026-11-07/README.md` path is unavailable and does not support
replaying the claimed 90-record evaluation. A bounded local isolation probe is
retained in the gitignored directory
`benchmarks/benchmark_artifacts/profile-isolation-20261114-b/`:

```bash
python3 benchmarks/tools/profile_isolation_probe.py \
  --binary dist-newstyle/build/x86_64-linux/ghc-9.10.3/occam-0.1.0.0/build/occam-eval/occam-eval \
  --out benchmarks/benchmark_artifacts/profile-isolation-20261114-b
```

The retained probe reports `qualified=true`; both repetitions produced
completed, validated native `full` profile rows. It covers one existing
`reset-template` SQLite rowset using per-observation candidate workdirs and
external reset only. This does not establish shared-server restoration,
representative workload evidence, or the full two-line control plane. The
probe helper SHA256 is
`8b8a37fe066f1932f8a3be2bd2fd9196739382af027a881c78ae67f6d79ab4f3`; evidence
is in `probe.json`, `observations.jsonl`, `cleanup.jsonl`, and `runs/` there.
The controller's normal-exit process cleanup status is unknown, not assumed
clean. To rerun, choose a new empty `--out` directory.

Focused orchestration verification reported:
`python3 -m unittest -v benchmarks.test_scale_study` — 5 tests, 11.971s, OK.

## GHC flamegraph

This checks the GHC 9.10 `.prof` → SVG toolchain on one generated SQLite-rowset input; it is not a performance baseline. Install `ghc-prof-flamegraph` separately from project dependencies:

```bash
set -e
TOOL_DIR=$(mktemp -d /tmp/ghc-prof-flamegraph.XXXXXX)
BUILD_DIR=$(mktemp -d /tmp/occam-ghc-profile-build.XXXXXX)
OUT_DIR=$(mktemp -d /tmp/occam-ghc-profile.XXXXXX)
cabal install ghc-prof-flamegraph-0.2.0.1 --installdir="$TOOL_DIR" --overwrite-policy=always
cabal build exe:occam --builddir="$BUILD_DIR" --enable-profiling --enable-executable-profiling \
  --ghc-options='-O2 -fprof-late' --disable-executable-stripping
BIN=$(cabal list-bin exe:occam --builddir="$BUILD_DIR")
python3 benchmarks/tools/scale_study.py prepare "$OUT_DIR/study" --corpus-size 1
ORACLE=$(jq -r '.cases[] | select(.id=="statements-4096") | .oracle_command' "$OUT_DIR/study/scale.json")
set +e
(cd "$OUT_DIR" && env -u GHCRTS "$BIN" +RTS -N1 -P -RTS "$OUT_DIR/study/statements-4096.sql" \
  --oracle "$ORACLE" --dialect sqlite --timeout 1 --budget 10 --max-oracle-executions 100 \
  --log-format json --out "$OUT_DIR/run")
status=$?
set -e
printf 'bounded smoke exit=%s (inspect run/stats.json; timeout is not completion evidence)\n' "$status"
"$TOOL_DIR/ghc-prof-flamegraph" --time "$OUT_DIR/occam.prof" \
  --flamegraph-option=--fonttype=sans-serif -o "$OUT_DIR/cpu-time.svg"
"$TOOL_DIR/ghc-prof-flamegraph" --alloc "$OUT_DIR/occam.prof" \
  --flamegraph-option=--fonttype=sans-serif -o "$OUT_DIR/haskell-allocation.svg"
```

The setup links the same, uninstrumented Rust release archive under Cabal's `_p` library name; this does not profile Rust. GHC time profiles exclude safe foreign-call CPU (`c_parseSQL`), child-oracle time and Rust allocations; allocation is cumulative Haskell heap, not live heap. Treat these graphs as Haskell diagnostics, not whole-process timing.

## Evidence-led diagnostic profiling

To capture symbolized runtime profiles and separately attribute child phase costs without contaminating score runs:

```bash
python3 benchmarks/tools/scale_study.py profiles manifest.json \
  --binary /path/to/profiled/occam-eval \
  --out /path/to/diagnostic-output \
  --profile full --repetitions 2 \
  --diagnostic
```

When `--diagnostic` is passed:
- `run.json` and every record in `results.jsonl` record `run_type: "diagnostic"` (normal runs record `run_type: "score"`) to support line-compliant partitioning; do not mix score and diagnostic runs or rely on the legacy reporter to enforce this boundary.
- The instrumented binary is invoked with `+RTS -N1 -P -po{case_dir}/occam -RTS`, writing symbolized cost-centre profile `{case_dir}/occam.prof`.
- If `ghc-prof-flamegraph` is available, SVG flamegraphs `{case_dir}/cpu-time.svg` and `{case_dir}/haskell-allocation.svg` are generated.
- If Linux `perf` is supported, the run is wrapped with resolved absolute `perf record -o {case_dir}/perf.data -g -- ...` and `{case_dir}/perf-report.txt` is generated with symbol tables.
- Postprocessing renderers and reporters reuse bounded runner process management, capturing structured execution records and raw stderrs into `diagnostic_manifest.json` under `postprocessing`.
- A top-level `diagnostic_status` (`"completed"`, `"partial"`, or `"failed"`) is recorded separately from native SQL reduction completion.
- Child predicate and reset costs are typed and attributed by phase in each record:
  `predicate_preflight_sec`, `predicate_reduction_sec`, `predicate_validation_sec`, `predicate_time_sec`,
  `reset_preflight_sec`, `reset_reduction_sec`, `reset_validation_sec`, `reset_time_sec`, and
  `unattributed_wall_time_sec` (residual wall-clock duration: `wall_time_sec - (predicate_time_sec + reset_time_sec)`; reflects scheduling, RTS runtime overhead, parser execution, and measurement latency, not CPU attribution; set to null with reason on inconsistent timing).
- A per-child `diagnostic_manifest.json` records toolchain versions, binary fingerprints, exact CLI and RTS flags, artifact paths, postprocessor command outcomes, and explicit blind spots disclosure (input checksums and reduction track provenance remain in `run.json` and per-child `results.jsonl` rows).

### Known blind spots and constraints
- **Internal Rust CPU**: GHC cost centres do not resolve internal Rust CPU; safe foreign calls can run outside sampled Haskell capabilities; use native perf with declared coverage.
- **Rust Allocations**: Rust allocations occur via the system/Rust allocator and are completely invisible to the GHC heap profiler.
- **Child Process Isolation**: External predicate (`--oracle`) and reset (`--reset`) commands execute in independent child processes. GHC profilers cover Haskell-side execution, not child process CPU or memory; predicate and reset phase durations are wall-clock times.
- **Linux perf Scope**: Linux `perf` default `-g` inherits the entire native process tree (including child oracle/reset processes and kernel fork/clone routines; symbolized aggregate in `perf-report.txt` is not target-only or Rust-only CPU, and presence of `perf.data` does not prove isolated Rust profiling). Linux `perf` does not observe Haskell heap allocations or green-thread scheduling.
- **Phase Duration Nature**: Typed predicate and reset phase costs (`predicate_*_sec`, `reset_*_sec`) are wall-clock durations measured from the runner/engine perspective, not isolated CPU time.
- **Bounded Case Sample Count**: Low CPU sample counts on small bounded inputs (such as the 47-byte SQLite `reset-template` case) reflect fast execution where startup, reset, and predicate phases dominate, rather than an absence of hotspots.
- **Toolchain Smoke Proof**: Prior flamegraph artifacts (e.g. `ghc-profile-20261113-a` on `statements-4096.sql`) remain toolchain proofs, not a performance baseline.

### Retained diagnostic evidence
Verified diagnostic evidence on the bounded SQLite `reset-template` case is retained at gitignored `benchmarks/benchmark_artifacts/diagnostic-profile-20261114-final2/` (exact commands and verified artifacts in per-child `command.json` and `diagnostic_manifest.json`). Target binary preparation metadata and build logs are recorded in `benchmarks/benchmark_artifacts/profile-prep-20261114-eval/preparation.json` and `cabal-build-v2.log`.

### Statement boundary traversal optimization
- **Bottleneck**: Old GHC profiling (`ghc-profile-20261113-a`) revealed `$wunsafeDrop` in `Occam.Reducer` consuming 79.2% CPU ticks during multi-statement reduction. In `cutRemovedStatements`, boundary lookup used `drop (index + 1) allStatements` for each removed statement, leading to repeated $O(N^2)$ list suffix traversals.
- **Optimization**: Replaced quadratic drop rescans with a single linear adjacent-statement walk (`collectSpans`) computing `nextStart` from the list tail in $O(1)$ per statement.
- **Scope and Ceiling**: Boundary lookup alone improves from $O(N^2)$ suffix rescans to an $O(N)$ adjacent walk. Kept-ID `Set` operations ($O(N \log K)$), removed-span sorting ($O(M \log M)$), and repeated ByteString copying in `cutSpans` remain separate costs. No whole-function $O(N)$ or external DBMS speedup is inferred. Candidate order, reduction semantics, and byte preservation remain exact.
- **Evidence**: Matched V2 before/after measurements across 1024, 4096, and 8192 statements (`benchmarks/benchmark_artifacts/statement-surgery-perf-20261114/final_v2_comparison.json`), using GHC 9.10.3 `-O1`, RTS `-N1 -T`, and 5 repeats per case. Reported values are 5-run arithmetic means of the helper microbenchmark timed region (timed statement surgery plus deep output forcing and FNV-1a64 checksumming, not isolated pure-function CPU). Across all 75 benchmark runs (15 size/pattern cases × 5 repeats), output lengths and FNV-1a64 checksums matched identically; this does not claim byte-for-byte equality across all 75 outputs. Separately, 14 exact ByteString semantics checks and all 184 `occam-test` suite tests passed.

| Statements | Pattern | Process CPU Before → After (ms) | CPU Speedup | Wall Before → After (ms) |
|---|---|---|---|---|
| 8192 | `keep_one` | 274.30 → 92.28 | 2.97x | 275.96 → 93.18 |
| 8192 | `keep_half` | 226.05 → 95.99 | 2.35x | 226.98 → 96.78 |
| 4096 | `keep_one` | 58.06 → 25.27 | 2.30x | 58.21 → 25.53 |
| 4096 | `keep_half` | 49.97 → 19.84 | 2.52x | 50.18 → 19.93 |
| 8192 | `keep_all` | 1.28 → 1.49 | 0.86x | 1.28 → 1.49 |
| 4096 | `keep_all` | 0.63 → 0.72 | 0.87x | 0.63 → 0.72 |
| 8192 | `keep_none` | 153.23 → 0.42 | 364.55x | 153.56 → 0.43 |
| 4096 | `keep_none` | 35.77 → 0.19 | 183.72x | 35.80 → 0.20 |

Small absolute regressions were observed on `keep_all` controls (+14% to +16%), which perform no cuts; the optimization does not generalize to speeding up every pattern. The mechanics-only `keep_none` case performs zero retained output copying, isolating the boundary traversal elimination ceiling.

### End-to-end large-SQL performance closure
- **Workload & Protocol**: Full end-to-end reduction on `statements-512.sql` (515 statements, 20,922 bytes; dialect SQLite) reduced to 3 statements (517 bytes), evaluated against the deterministic SQLite rowset oracle via 3 matched interleaved runs (`B1, C1, B2, C2, B3, C3`). Binaries built in isolated trees under GHC 9.10.3 (`-O1 -threaded -rtsopts`) differing strictly in `cutRemovedStatements` (baseline drop-rescan vs current `collectSpans`), run with `+RTS -N1 -s -RTS`. Raw records in gitignored `benchmarks/benchmark_artifacts/large-sql-e2e-20261114/e2e_comparison.json`.
- **Observed Metrics**:
  - **Occam Process CPU**: Total user CPU improved from 3.182s to 2.614s (1.22x speedup; Mutator CPU 2.892s → 2.360s, 1.23x speedup).
  - **Occam Internal Reduction Wall**: Reduction wall excluding child oracle duration improved from 1.432s to 1.096s (1.31x speedup).
  - **External Process Dominance**: External child oracle subprocess wall clock accounted for 98.19% (baseline, 77.67s / 79.11s) and 98.52% (current, 72.78s / 73.88s) of reduction loop wall time.
  - **Reduction Quality**: All 6 runs completed cleanly with status `completed` (exit code 0), achieving 100% identical outputs (3 statements, 517 bytes).
- **Stopping Decision**: Because external child process execution overwhelmingly dominates end-to-end makespan (~98%) and Occam internal engine overhead is already negligible (~1.10s across 604 reduction steps, ~1.8ms per step), further internal optimization (such as ByteString manipulation in `cutSpans`) is explicitly declined under the performance decision gate. Large-SQL performance evaluation is cleanly closed.
## External experiment line (reference adapters only)

Under Strategy 3A, external reducer comparisons (SQLreduce, SQLancer) are
explicitly scoped out of the primary evaluation line in favor of strictly
partitioned internal controls and ablations. Existing adapter notes below
describe optional reference integration behavior only; they do not form part of
the primary evaluation scope.

If a future scope selects these adapters, SQLancer requires a Git checkout at [`a7d60ee`](https://github.com/sqlancer/sqlancer/tree/a7d60ee450e6a22ca38599c14e984a49773f4745), a JDK, and its built JAR/dependency directory. Set `SQLANCER_SOURCE` and run `mvn -q -Dmaven.test.skip=true package -f "$SQLANCER_SOURCE/pom.xml"` once before measurement; the adapter no longer fetches or rebuilds on each input. This prerequisite inventory is not a selection.

SQLreduce's optional adapter requires a checkout at [`568c8a9`](https://github.com/credativ/sqlreduce/tree/568c8a9a31601ead7128d0b067658b4d5a26821d), exposed through `SQLREDUCE_SOURCE`. An exercised optional environment used pglast 8.7, psycopg2-binary 2.9.13 and PyYAML 6.0.3. These are adapter-specific prerequisites, not an Occam core dependency or external-line decision.

The existing adapter code is wired to invoke upstream search through the common external predicate: SQLancer's StatementReducer/ASTBasedReducer use a virtual JDBC provider, while SQLreduce's AST/rules use a replaced candidate-execution callback. This implementation note is not evidence of a selected or qualified external experiment line, and neither adapter uses its project's native DBMS oracle. SQLreduce accepts complete PostgreSQL scripts; its old single-SELECT/setup/DSN/SQLSTATE adapter flags and reconnect loop are gone. Keep the same input, reset and predicate across any future comparison; `reduction_track` is a per-input label, not a tool-specific input substitution.

Scoped adapter checks have exercised SQLite and PostgreSQL multi-statement predicates; that compatibility is not representative workload or DBMS qualification. SQLreduce rejects non-PostgreSQL dialects as unsupported. SQLancer/JSQLParser coverage is partial and unsupported AST statements are counted. Other dialects, escaped-string modes and procedural bodies need their own evidence. The shared checker distinguishes rejection, timeout, infrastructure failure and execution-budget exhaustion, keeps accepted checkpoints, and does not cache control failures.

Predicate stdout/stderr are opaque, and every raw nonzero predicate exit (including 124/125/126) rejects SQL. Wrapper-owned failures are carried by fresh typed JSONL audit records, not log text. The internal checker translates those records into its explicit 0/1/124/125/126 protocol; both upstream adapters stop on an inconclusive timeout instead of treating it as rejection.

Outer initial/final checks are reported separately. Native execution caps include native preflight/final checks; callback caps cover shared-checker observations, including adapter format probes. Adapter overhead counts toward the wall deadline. Do not present differing internal validation protocols as identical callback counts; compare their scope and final fresh-state checks explicitly.

## Dependency reconstruction versus SQL state slicing

[DRReduce's full paper](https://arxiv.org/html/2605.19412v1) (§§3–3.3) classifies provider/user/conditioner nodes, applies DDMin with typed default-value reconstruction or associated-node deletion, then delegates to Perses. Its evaluated frontends are C/Java through IntelliJ/CLion PSI, not SQL sessions or catalogs. The [public artifact](https://github.com/XYZboom/DRReduceData) contains compiler workloads/predicates/results; its reconstruction-named Python script plots results rather than implementing a SQL reducer.

Occam instead tracks ordered SQL definitions, uses, writes, drops, renames and generations, plus recognized transaction/savepoint catalog snapshots. Conservative barriers retain uncertain state; unrestricted deletion lets the supplied predicate remove false dependencies. Unchanged source bytes are retained, without DRReduce-style typed identifier repair. These are scoped engineering differences, not proof of a novel or universally superior method. DRReduce excludes some C expression reductions and leaves Java inheritance propagation incomplete; neither its compiler timings nor its other-language limitations establish SQL performance. Use the controlled SQL profiles above for SQL-specific evidence.

## Internal ablation reporting

`benchmarks/tools/partitioned_report.py` (and `scale_study.py report`) derives paired deltas only within ONE qualified current run directory (`run.json` + `results.jsonl`). It enforces strict protocol compatibility:
- Pairing requires exact match on `case_id`, `input_sha256`, `reduction_track`, `predicate_command_sha256`, `reset_command_sha256`, `case_contract_sha256`, `repetition`, limits (`oracle_timeout_sec`, `budget_sec`, `max_oracle_executions`, `outer_timeout_sec`), and resource/count scopes. Raw row records must explicitly contain each declared identity key (including explicit `null` for unconfigured reset commands; missing keys fail row validation).
- Native reduction settings must match the declared internal contrast contract:
  * `full` vs `no-cache` (memoization ablation): differs ONLY in candidate cache memoization (`True` for base `full`, `False` for target `no-cache`), with identical active reduction passes (`kind: "shared-reducer"`, `slicing: true`, `closure: true`, `intra_statement: true`, `unrestricted: true`, matching `dialect`).
  * `full` vs `graph-off` (dependency graph ablation): differs ONLY in dependency graph passes (`slicing` and `closure` both `True` for base `full`, both `False` for target `graph-off`), with identical candidate cache (`cache: true`), active intra-statement pass (`intra_statement: true`), unrestricted 1-minimality audit (`unrestricted: true`), shared-reducer kind (`kind: "shared-reducer"`), and matching dialect. Weaker statement-only baselines (`statement-ddmin`, `sequential-deletion`) are strictly rejected.
  * Unknown contrasts, missing/null/malformed settings, asymmetric keys, or non-target setting differences refuse to pair and fail qualification.
- Profiles and child IDs are comparison sides, NOT equality keys.
- Metrics: `oracle_calls` counts spawned predicate observations; `candidate_checks` counts attempted observations (debited before reset). Quality and cost metrics remain in their own units.
- `unattributed_wall_time_sec` is residual wall-clock time minus predicate and reset duration; it reflects runtime overhead, scheduling, and parser latency, NOT CPU attribution.

### Required `case_contract` schema

For qualified case admission, `manifest.json` cases carry an explicit `case_contract` dictionary (with canonical `case_contract_sha256 = hashlib.sha256(json.dumps(case_contract, sort_keys=True).encode("utf-8")).hexdigest()`):
- `effect_scope`: string (e.g. `"candidate-local SQLite file-state control via OCCAM_WORKDIR"`)
- `comparison_policy`: string (e.g. `"exact ordered-row comparison against expected_rows"`)
- `declared_scope`: string (e.g. `"one SQLite file-state control only; not primary DBMS/external tool selection"`)
- `environment`: object with `python_executable`, `python_version`, and `sqlite_version`
- `hashes`: object containing 64-char hex strings for `sql_sha256` (must agree with `input_sha256`), `metadata_sha256`, and `helper_sha256`, plus explicit `template_sha256` (64-char hex string for file-state fixtures requiring template DB restoration, or explicit `null` when no template DB is used, e.g. in-memory or fresh-seed cases)
- Case configuration and raw row records must declare explicit `reset_command_sha256`: 64-char hex string when an external reset command is configured, or explicit `null` when no reset command is configured.

Declaration and fingerprints record declared protocol and effect scope, not automatic oracle certification or DBMS bug reproduction. Running `prepare` alone does not qualify engine evidence.

### Verified full vs no-cache control slice

- **Pass and reduction contract**: Traced and verified from `bench/Benchmark.hs`, `src/Occam/Reducer.hs`, and `src/Occam/Evaluation.hs`. Both profiles execute identical ordered mechanisms: after preflight, an outer shrinking loop runs backward dependency slicing (`reductionSlicing: true`), DAG closure delta debugging (`reductionClosure: true`), unrestricted statement deletion fallback (`reductionUnrestricted: true`), and an inner fixed-point loop (`interleaveFixedPoint`) alternating single-statement audit (`statementAuditWithLogger`) THEN intra-statement syntax rewrites (`reduceSyntaxWithDialect`). `src/Occam/Evaluation.hs` then applies a separate final shared statement audit (`statementAuditWithLogger`), achieving statement deletion 1-minimality only when audit is confirmed (not a global SQL minimum), followed by 2 uncached physical validation checks (`validateTwice`). The named difference is strictly candidate memoization during reduction (`evaluationCacheEnabled: true` in `full` vs `false` in `no-cache`).
- **Case provenance & helper qualification**: Input derived from `test/golden/fuzz_golden_03_deep_expressions.golden.sql` (47 bytes, input SHA256 `12f182722a25864cfc7a0deeca2c8d3ff692a90c35b89d0d200a597fdbe4b3cc`). Comparison semantics enforce exact ordered-row equality against expected rows `[[1, 100.5], [2, 250.75]]`. Standalone helper `sqlite_case_runner.py` is qualified with clean reset, positive control, wrong-row negative control (`rc=1`), syntax-error negative control (`rc=1`), contamination detection (`rc=125`), and clean restoration (`helper_qualification.json`).
- **Execution and reporting commands**:
  The command below records local historical evidence; for new runs, choose a NEW, empty `--out` directory (existing directories are preserved). Local gitignored evidence is not fresh-checkout-ready packaging.
  ```bash
  python3 benchmarks/tools/scale_study.py profiles \
    benchmarks/benchmark_artifacts/internal-ablation-20261114-a/fixture/manifest.json \
    --binary dist-newstyle/build/x86_64-linux/ghc-9.10.3/occam-0.1.0.0/build/occam-eval/occam-eval \
    --out benchmarks/benchmark_artifacts/internal-ablation-20261114-a/runs \
    --profile full --profile no-cache --repetitions 2 \
    --timeout 2 --budget 30 --max-oracle-executions 100

  python3 benchmarks/tools/scale_study.py report \
    benchmarks/benchmark_artifacts/internal-ablation-20261114-a/runs
  python3 benchmarks/tools/scale_study.py report \
    benchmarks/benchmark_artifacts/internal-ablation-20261114-a/runs --json
  ```
- **Evidence and observations**: Retained evidence lives in `benchmarks/benchmark_artifacts/internal-ablation-20261114-a` (local gitignored).
  * Reducer-scope physical predicate observations: 6 in `full` vs 26 in `no-cache` **per repetition**, eliminating exactly 20 uncached physical predicate observations in each repetition (`Δ = +20`).
  * Reducer cache hits: 20 in `full` vs 0 in `no-cache` per repetition (`Δ = -20`).
  * Reduction quality: 100% identical across all 4 children (43 bytes, 1 statement, output SHA256 `97a763885d27f6300f5f1e600dbc6051126d9a54e3a98d076d6c4f6f83a8d544`).
  * Denominators: 2/2 comparison slots paired and completed; 0 exclusions.
- **Scope and non-causal caveats**: Runs executed serially in fixed child order without OS filesystem page cache flushes. Residual wall time (0.04s in `full` vs 0.08–0.10s in `no-cache`) is not CPU attribution or a causal speedup claim. Bounded strictly to self-contained SQLite file-state / memory controls under Strategy 3A; external heavy DBMS cluster deployments and external reducer comparisons are explicitly scoped out.


### Stratified 12-case SQLite pilot methodology and provenance

To evaluate internal mechanism deltas beyond the 1-case micro-control, a stratified 12-case SQLite pilot is configured under Strategy 3A (`benchmarks/benchmark_artifacts/sqlite-pilot-20261114-b/`):
- **Workload provenance and generation**: 12 related SQL cases derived from deep-expression fuzzing fixture `test/golden/fuzz_golden_03_deep_expressions.golden.sql` (source SHA256 `5667a182dfcb360bb8a824c0748b5858f36a149c93870517ea9281f97a87612a`) using Python 3.14 / SQLite 3.54.1; these are selected scale/reset suite variants with explicit controls, not independent bugs or representative of SQLite workloads generally:
  * 10 `whole_script` cases using isolated in-memory execution (`:memory:`) without external reset command (`reset_command_sha256 = null`) or template database (`template_sha256 = null`):
    - Statement count scaling: `statements-64` (64 noise inserts), `statements-512` (515 initial statements; declared budget-stress case).
    - Object breadth: `objects-1` (1 table, 128 inserts), `objects-32` (32 tables, 128 inserts).
    - AST expression depth: `depth-1` (shallow), `depth-8` (moderate), `depth-24` (deep).
    - View dependency density: `density-1` (linear chain fan-in 1), `density-4` (diamond chain fan-in 4).
    - Structural barrier control: `false-barrier` (`SELECT; BEGIN; SELECT; COMMIT;`).
  * 2 `fixed_setup_query` file-state cases executing in candidate-local workdirs (`OCCAM_WORKDIR`):
    - `reset-template`: file-state restored from SQLite template database (`reset_backend: "template"`).
    - `reset-fresh`: file-state re-initialized from seed setup SQL (`reset_backend: "fresh"` with `template_sha256 = null`).
- **Control plane and profile plan**: Evaluates three internal profiles—`full`, `no-cache`, and `graph-off`—across 3 serial repetitions (12 cases × 3 profiles × 3 reps = 108 total child runs).
- **Uniform execution limits & resource protocol**:
  * `--budget 35`: cumulative wall budget per child.
  * `--timeout 2`: per-predicate execution deadline.
  * `--max-oracle-executions 500`: shared cap on preflight, reduction, and validation oracle observations.
  * `--outer-timeout 40`: outer child deadline followed by bounded process-group cleanup (not proof that all descendants or escaped processes exited).
  * `GHCRTS="-N1"`, serial single-job execution (`jobs=1`), inherited CPU affinity.
- **Oracle and helper qualification**: Standalone helper `sqlite_case_runner.py` (SHA256 `27bfbc67bfb3c8ca4bece4a822f7e9d77647065d627e783ef55bb8e7074880f6`) is pre-qualified across all 12 cases (`helper_qualification.json`):
  * Positive identity control (`rc=0`),
  * Negative controls: syntax error (`rc=1`), empty SQL (`rc=1`), wrong rowset values (`rc=1`),
  * Contamination rejection: prior-session tables absent in fresh in-memory connections and corrupted user_version state rejected,
  * Reset recovery: clean state restoration on template and fresh seed backends.
- **Execution order and caveats**: Child processes execute serially in fixed `(case, profile, repetition)` order without flushing the OS page cache between runs. Raw artifacts and runs reside in gitignored local storage (`benchmarks/benchmark_artifacts/sqlite-pilot-20261114-b/`); these generated cases measure reduction and caching mechanics under Strategy 3A, not third-party DBMS bug reproduction, external cluster concurrency, or general SQLite behavior.
- **Observed pilot outcomes and denominators** (`runs/results.summary.json`):
  * Total child runs: 108 (71 completed, 36 timeout, 1 error; serial makespan 2091.58s).
  * `whole_script` track (10 cases × 3 profiles × 3 reps = 90 runs): `full` 18 completed / 11 timeout / 1 error; `no-cache` 11 completed / 19 timeout; `graph-off` 24 completed / 6 timeout.
  * `fixed_setup_query` track (2 cases × 3 profiles × 3 reps = 18 runs): `full` 6/6 completed; `no-cache` 6/6 completed; `graph-off` 6/6 completed.
- **Candidate memoization evidence (`full` vs `no-cache`)**:
  * 17 of 36 comparison slots completed jointly (11 `whole_script`, 6 `fixed_setup_query`; 19 censored due to target timeout).
  * Output identity on completed pairs: 17/17 (100.0%) byte-for-byte identical outputs.
  * Across all 6 completed cases, memoization observed fewer physical database oracle executions per case (median delta: `density-1` +127 calls, `depth-1` +68 calls, `statements-64` +66 calls, `reset-fresh` +20 calls, `reset-template` +20 calls, `false-barrier` +14 calls).
  * In this bounded cohort under the 35s protocol, `full` had fewer noncompleted `whole_script` children than `no-cache` (refer to per-track denominator table above). Disclosing selection bias: evaluating completed-only pairs filters out harder uncached timeout workloads.
- **Dependency graph ablation evidence (`full` vs `graph-off`)**:
  * 24 of 36 comparison slots completed jointly (18 `whole_script`, 6 `fixed_setup_query`; 12 censored: at least one side reported timeout; density-4 rep2 additionally contains the raw infrastructure error).
  * Output identity on completed pairs: 24/24 (100.0%) byte-for-byte identical outputs. Disabling dependency graph passes did not cause quality loss on any completed case.
  * Case-dependent tradeoffs (reporting wins and regressions honestly without pre-framing):
    - On `objects-1` (median delta -84 calls: 137 vs 221), `objects-32` (median delta -47 calls: 174 vs 221), `statements-64` (median delta -27 calls: 130 vs 157; rep 1: full 157 vs graph-off 130, same 517 B), and `density-1` (median delta -14 calls: 115 vs 129), `graph-off` completed with fewer oracle calls. On `statements-512` (declared budget stress) and `depth-8`, `graph-off` completed within budget (151 and 372 calls) while `full` reported timeout/budget exhaustion on all 3 repetitions.
    - On shallow AST expression scaling (`depth-1`), `graph-off` incurred +7 more oracle calls (+5.0%, 148 vs 141 calls) and +2.120s median wall time.
    - On single-statement fixed-setup inputs (`reset-fresh`, `reset-template`), graph passes had zero delta (identical 6 and 6 calls). On the 4-statement whole-script transaction control fixture `false-barrier` (`SELECT; BEGIN; SELECT; COMMIT;`), graph passes also showed zero delta (identical 9 calls).
- **Censored cases and selection bias**:
  * `statements-512` (whole_script, 515 initial stmts): Declared budget-stress case. `full` (3/3) and `no-cache` (3/3) reported timeout/budget exhaustion under the 35s protocol; `graph-off` completed (3/3, 151 calls, wall 18.04s–21.06s, median 18.88s). Both pairs censored due to base timeout.
  * `depth-8` (whole_script, 19 initial stmts): Moderate AST expression depth. `full` (3/3) and `no-cache` (3/3) reported timeout/budget exhaustion under the 35s protocol; `graph-off` completed (3/3, 372 calls, wall 26.89s–27.46s, median 27.20s). Both pairs censored due to base timeout.
  * `depth-24` (whole_script, 19 initial stmts): Deep AST expression depth. All three profiles reported timeout/budget exhaustion (3/3) under the 35s protocol.
  * `density-4` (whole_script, 8 initial stmts): `no-cache` and `graph-off` reported timeout/budget exhaustion (3/3). `full` reported timeout/budget exhaustion on repetitions 1 and 3; repetition 2 exited with `status: "error"` due to a retained process-management/infrastructure error (`stderr.log:1`: `occam-eval: waitForProcess: does not exist (No child processes)`, classified as `OracleIOFailed`/`InfrastructureError` in `Occam.Oracle`, exit code 1, native validation passed true, predicate passed null; exact underlying process cause unestablished without claiming a proven race or reaping cause from a single log). In pair evaluation, the `density-4`/`full`/rep2 slot is labeled timeout in each contrast because its target timed out, while the raw full child is separately an infrastructure error.
  * `objects-1` & `objects-32` (whole_script, 131 initial stmts): Object breadth scaling. `full` and `graph-off` completed 3/3; `no-cache` reported timeout/budget exhaustion (3/3; calls 244–267), censoring `full-vs-no-cache`.
  * `density-1` (whole_script, 8 initial stmts): Linear view chain. `full` and `graph-off` completed 3/3; `no-cache` completed 2/3 and reported timeout/budget exhaustion on repetition 2 at 31.31s (243 calls).
- Full track-separated denominators, case tables, and medians are retained in `benchmarks/benchmark_artifacts/sqlite-pilot-20261114-b/track_summary.{json,md}`.
## Supported partitioned reporting control plane

The supported partitioned report tool is `benchmarks/tools/partitioned_report.py` (also accessible via `python3 benchmarks/tools/scale_study.py report <run_dir>`). It enforces strict partition validation: rejecting cross-line, cross-protocol, cross-track, or mismatched-pass pairs.

```bash
# Memoization ablation: candidate cache enabled vs disabled
python3 benchmarks/tools/partitioned_report.py <run_dir> --base full --target no-cache
python3 benchmarks/tools/partitioned_report.py <run_dir> --base full --target no-cache --json

# Dependency graph ablation: dependency graph passes enabled vs disabled
python3 benchmarks/tools/partitioned_report.py <run_dir> --base full --target graph-off
python3 benchmarks/tools/partitioned_report.py <run_dir> --base full --target graph-off --json
```

Legacy unpartitioned presentation scripts (`benchmarks/report.sh`, `benchmarks/run_all.sh`) have been retired.
Generate a synthetic SQL stress input directly when needed:

```bash
python3 benchmarks/tools/fuzz_generator.py \
  --dialect postgres --stmts 100 --depth 12 --inject pg_01 --out /tmp/fuzz.sql
```

## Appendix: Legacy multi-tool manifest runner (reference only)

### Manifest and output format

The comparison runner accepts a JSON manifest with one shared predicate and per-case SQL, dialect, and optional reset. No bug provenance or crash-signature fields are required; case files may be relative to the manifest or repository.

This legacy runner can dispatch internal and optional external tools in one batch. Do not use mixed selections or its default tool list as a comparison across the two lines; line-specific orchestration/reporting remains a separate cutover. Existing output remains mechanics/history only.

```json
{
  "oracle_command": "./interesting.sh",
  "cases": [
    {
      "id": "fuzz-001",
      "file": "inputs/fuzz-001.sql",
      "dialect": "postgres",
      "reset_command": "./reset-db"
    }
  ]
}
```

Run the built-in reducers without making optional external downloads prerequisites:

```bash
python3 benchmarks/fuzz_corpus/run_docker_benchmark.py \
  --manifest cases.json --tools occam,ddmin \
  --oracle-timeout 30 --budget 300
```

The runner defaults to optional external reducers as well; because it does not enforce line separation, avoid the default or any cross-line tool list for experiment claims. `--out` selects the JSONL result file and `--artifacts-dir` the per-run artifacts. The default manifest uses synthetic text-marker fixtures; historical JSONL records from that corpus are mechanics data, not validated DBMS results.

Each JSONL line stays a per-case/tool record. The runner writes a sibling `<stem>.summary.json` sidecar (`results.summary.json` for `results.jsonl`) with processed/expected counts, validated reductions, status counts, throughput, and actual monotonic batch makespan. Rates use that makespan; `wall_time_sec` on each record is worker time, so summed case time is not batch elapsed time (especially with `--jobs`).

For `occam`/`ddmin`, a normally terminated native process may report a recognized evaluation status from a fresh stats/profile file; outer command timeouts, spawn failures, and missing, stale, or unknown profiles are not overridden. A nonzero native exit is never promoted to `completed`. Successful native exit requires `minimal.sql`; missing output is a `tool-error`, not a successful unchanged result. Partial budget/timeout/error outcomes keep `predicate_passed` unknown; `evaluationFailure` is preferred as the reason when available.

Native dialects are passed explicitly to the CLI; unsupported or absent dialects are not reinterpreted as Generic SQL. Native results also require a complete, recognized typed predicate audit matching the fresh profile's process count. Wrapper timeout, interruption, infrastructure failure, or incomplete audit evidence prevents result promotion and preserves the initially accepted checkpoint.

`evidence_class` and `oracle_kind` come from the manifest (or documented runner defaults); reports group and pair by input/evidence/predicate/dialect/track but do not partition by experiment line, execution mode or internal profile family. These labels do not prove SQL execution or identical predicates. Keep different oracle configurations in distinct input IDs; do not treat legacy multi-tool output as a cross-line leaderboard.

Comparison-runner `reset_calls` and `reset_time_sec` cover audited reducer callbacks only; initial/final validation calls are excluded and reported separately. Incomplete reducer audit data stays `null`. A zero reducer-scope count does not mean no reset was attempted: inspect `oracle_audit_file` for validation-phase attempts and failures. The single-input pipeline records `reset_preflight_sec`, `reset_reduction_sec`, and `reset_validation_sec`; `reset_time_sec` sums these stages once only when all are known, otherwise it is `null`.

`oracle_timeouts` counts reducer-scope observations only: native tools use a fresh `evaluationTimedOutObservations` profile count, callback tools use observed callback/reducer-audit outcomes, and validation-phase audit rows are excluded. If a native count is missing or invalid, it remains `null`.

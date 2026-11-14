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
- **External**: independently implemented reducer search against the same exact input, setup, predicate and declared budget. This line is unselected; the DBMS/workload scope is blocked, and existing optional adapters are not an admitted or verified experiment path.

Execution mode (native CLI or API) is orthogonal to experiment line; this scale-study runner records native mode only. `reduction_track` labels the per-input workload (`whole_script` or `fixed_setup_query`), not a line or profile family. `profile_family` distinguishes statement-only controls from shared-reducer profiles; `intra_statement_edits` records whether that syntax pass is enabled, not universal AST coverage. Pairing requires matching line, input identity, track, validation, resource and count protocol. Profile names/effective settings are comparison sides: controlled ablations intentionally differ in a named mechanism. The runner preserves each selected profile as a separate record but does not form or validate homogeneous cohorts; do not pool cross-line or statement-only/syntax-enabled coverage.

The legacy multi-tool runner and reporter below do not enforce these partitions. Their mixed records are not a line-compliant leaderboard or evidence bundle.

## Legacy multi-tool manifest runner (not line-conforming)
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

`evidence_class` and `oracle_kind` come from the manifest (or documented runner defaults); reports group and pair by input/evidence/predicate/dialect/track but do not partition by experiment line, execution mode or internal profile family. These labels do not prove SQL execution or identical predicates. Keep different oracle configurations in distinct input IDs; do not treat `report.sh` output as a cross-line leaderboard.

Comparison-runner `reset_calls` and `reset_time_sec` cover audited reducer callbacks only; initial/final validation calls are excluded and reported separately. Incomplete reducer audit data stays `null`. A zero reducer-scope count does not mean no reset was attempted: inspect `oracle_audit_file` for validation-phase attempts and failures. The single-input pipeline records `reset_preflight_sec`, `reset_reduction_sec`, and `reset_validation_sec`; `reset_time_sec` sums these stages once only when all are known, otherwise it is `null`.

`oracle_timeouts` counts reducer-scope observations only: native tools use a fresh `evaluationTimedOutObservations` profile count, callback tools use observed callback/reducer-audit outcomes, and validation-phase audit rows are excluded. If a native count is missing or invalid, it remains `null`.


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
GHCRTS=-N1 python3 benchmarks/fuzz_corpus/run_docker_benchmark.py \
  --manifest /tmp/occam-study/scale.json --tools occam,ddmin --jobs 4 \
  --oracle-timeout 2 --budget 30 --max-oracle-executions 500 \
  --out /tmp/occam-study/scale-results.jsonl
GHCRTS=-N1 python3 benchmarks/fuzz_corpus/run_docker_benchmark.py \
  --manifest /tmp/occam-study/corpus.json --tools occam,ddmin --jobs 4 \
  --oracle-timeout 1 --budget 12 --max-oracle-executions 100 \
  --out /tmp/occam-study/corpus-results.jsonl
python3 benchmarks/tools/scale_study.py profiles /tmp/occam-study/profiles.json \
  --binary "$(env -u GHCRTS cabal list-bin exe:occam-eval)" \
  --profile full --profile graph-off --repetitions 3 \
  --out /tmp/occam-study/profiles-run --timeout 2 --budget 30 \
  --max-oracle-executions 500 --outer-timeout 35
```

The two `run_docker_benchmark.py` examples above are legacy batch mechanics,
not a line-conforming experiment cohort. The internal profile driver below is
the current bounded internal slice; external-line selection remains blocked.

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

## External experiment line (unselected)

No external reducer or DBMS/workload scope is selected here. Existing optional
adapter notes below describe implementation behavior only; they do not qualify
an external experiment line or provide a migrated, line-isolated control plane.

If a future scope selects these adapters, SQLancer requires a Git checkout at [`a7d60ee`](https://github.com/sqlancer/sqlancer/tree/a7d60ee450e6a22ca38599c14e984a49773f4745), a JDK, and its built JAR/dependency directory. Set `SQLANCER_SOURCE` and run `mvn -q -Dmaven.test.skip=true package -f "$SQLANCER_SOURCE/pom.xml"` once before measurement; the adapter no longer fetches or rebuilds on each input. This prerequisite inventory is not a selection.

SQLreduce's optional adapter requires a checkout at [`568c8a9`](https://github.com/credativ/sqlreduce/tree/568c8a9a31601ead7128d0b067658b4d5a26821d), exposed through `SQLREDUCE_SOURCE`. An exercised optional environment used pglast 8.7, psycopg2-binary 2.9.13 and PyYAML 6.0.3. These are adapter-specific prerequisites, not an Occam core dependency or external-line decision.

The existing adapter code is wired to invoke upstream search through the common external predicate: SQLancer's StatementReducer/ASTBasedReducer use a virtual JDBC provider, while SQLreduce's AST/rules use a replaced candidate-execution callback. This implementation note is not evidence of a selected or qualified external experiment line, and neither adapter uses its project's native DBMS oracle. SQLreduce accepts complete PostgreSQL scripts; its old single-SELECT/setup/DSN/SQLSTATE adapter flags and reconnect loop are gone. Keep the same input, reset and predicate across any future comparison; `reduction_track` is a per-input label, not a tool-specific input substitution.

Scoped adapter checks have exercised SQLite and PostgreSQL multi-statement predicates; that compatibility is not representative workload or DBMS qualification. SQLreduce rejects non-PostgreSQL dialects as unsupported. SQLancer/JSQLParser coverage is partial and unsupported AST statements are counted. Other dialects, escaped-string modes and procedural bodies need their own evidence. The shared checker distinguishes rejection, timeout, infrastructure failure and execution-budget exhaustion, keeps accepted checkpoints, and does not cache control failures.

Predicate stdout/stderr are opaque, and every raw nonzero predicate exit (including 124/125/126) rejects SQL. Wrapper-owned failures are carried by fresh typed JSONL audit records, not log text. The internal checker translates those records into its explicit 0/1/124/125/126 protocol; both upstream adapters stop on an inconclusive timeout instead of treating it as rejection.

Outer initial/final checks are reported separately. Native execution caps include native preflight/final checks; callback caps cover shared-checker observations, including adapter format probes. Adapter overhead counts toward the wall deadline. Do not present differing internal validation protocols as identical callback counts; compare their scope and final fresh-state checks explicitly.

## Dependency reconstruction versus SQL state slicing

[DRReduce's full paper](https://arxiv.org/html/2605.19412v1) (§§3–3.3) classifies provider/user/conditioner nodes, applies DDMin with typed default-value reconstruction or associated-node deletion, then delegates to Perses. Its evaluated frontends are C/Java through IntelliJ/CLion PSI, not SQL sessions or catalogs. The [public artifact](https://github.com/XYZboom/DRReduceData) contains compiler workloads/predicates/results; its reconstruction-named Python script plots results rather than implementing a SQL reducer.

Occam instead tracks ordered SQL definitions, uses, writes, drops, renames and generations, plus recognized transaction/savepoint catalog snapshots. Conservative barriers retain uncertain state; unrestricted deletion lets the supplied predicate remove false dependencies. Unchanged source bytes are retained, without DRReduce-style typed identifier repair. These are scoped engineering differences, not proof of a novel or universally superior method. DRReduce excludes some C expression reductions and leaves Java inheritance propagation incomplete; neither its compiler timings nor its other-language limitations establish SQL performance. Use the controlled SQL profiles above for SQL-specific evidence.

## Reports and synthetic inputs

`benchmarks/report.sh` formats JSONL records and cannot establish SQL validity or bug reproduction. `--table` is the default; `--summary` groups by evidence class, predicate kind, dialect, reduction track and tool; `--md` emits Markdown. `--json` emits case/tool records as a JSON array; `--csv` emits one case/tool record per CSV row and preserves false separately from unknown. These two data modes do not merge the separate batch-summary sidecar.

```bash
./benchmarks/report.sh --summary
./benchmarks/report.sh --json results.jsonl > results.json
./benchmarks/report.sh --csv results.jsonl > results.csv
cat results.jsonl | ./benchmarks/report.sh --summary
```

Generate a synthetic SQL stress input directly when needed:

```bash
python3 benchmarks/tools/fuzz_generator.py \
  --dialect postgres --stmts 100 --depth 12 --inject pg_01 --out /tmp/fuzz.sql
```

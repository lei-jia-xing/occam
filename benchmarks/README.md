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

## Compare reducers on a manifest

The comparison runner accepts a JSON manifest with one shared predicate and per-case SQL, dialect, and optional reset. No bug provenance or crash-signature fields are required; case files may be relative to the manifest or repository.

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

The runner defaults to optional external reducers as well, so select `--tools occam,ddmin` for a local comparison. `--out` selects the JSONL result file and `--artifacts-dir` the per-run artifacts. The default manifest uses synthetic text-marker fixtures; historical JSONL records from that corpus are mechanics data, not validated DBMS results.

Each JSONL line stays a per-case/tool record. The runner writes a sibling `<stem>.summary.json` sidecar (`results.summary.json` for `results.jsonl`) with processed/expected counts, validated reductions, status counts, throughput, and actual monotonic batch makespan. Rates use that makespan; `wall_time_sec` on each record is worker time, so summed case time is not batch elapsed time (especially with `--jobs`).

For `occam`/`ddmin`, a normally terminated native process may report a recognized evaluation status from a fresh stats/profile file; outer command timeouts, spawn failures, and missing, stale, or unknown profiles are not overridden. A nonzero native exit is never promoted to `completed`. Successful native exit requires `minimal.sql`; missing output is a `tool-error`, not a successful unchanged result. Partial budget/timeout/error outcomes keep `predicate_passed` unknown; `evaluationFailure` is preferred as the reason when available.

`evidence_class` and `oracle_kind` come from the manifest (or documented runner defaults); reports group and pair only matching input IDs, evidence class, predicate kind, dialect and `reduction_track`. The labels do not prove SQL execution or identical predicates. Keep different oracle configurations in distinct input IDs.

Comparison-runner `reset_calls` and `reset_time_sec` cover audited reducer callbacks only; initial/final validation calls are excluded and reported separately. Incomplete reducer audit data stays `null`. A zero reducer-scope count does not mean no reset was attempted: inspect `oracle_audit_file` for validation-phase attempts and failures. The single-input pipeline records `reset_preflight_sec`, `reset_reduction_sec`, and `reset_validation_sec`; `reset_time_sec` sums these stages once only when all are known, otherwise it is `null`.

`oracle_timeouts` counts reducer-scope observations only: native tools use a fresh `evaluationTimedOutObservations` profile count, callback tools use observed callback/reducer-audit outcomes, and validation-phase audit rows are excluded. If a native count is missing or invalid, it remains `null`.


## Reproducible scale and ablation workloads

`tools/p2_scale_study.py` derives SQL from the existing deep-expression golden fuzz fixture, executes it with SQLite to establish expected rowsets, and keeps each input beside its predicate metadata. It varies statement count (up to 4099 total), object count, expression depth and view fan-in separately. The corpus contains 128 related variants plus explicit reset-failure and real SQLite timeout cases; it is not 128 independent bugs or broad dialect coverage.

```bash
python3 benchmarks/tools/p2_scale_study.py prepare /tmp/occam-study --corpus-size 128
GHCRTS=-N1 python3 benchmarks/fuzz_corpus/run_docker_benchmark.py \
  --manifest /tmp/occam-study/scale.json --tools occam,ddmin --jobs 4 \
  --oracle-timeout 2 --budget 30 --max-oracle-executions 500 \
  --out /tmp/occam-study/scale-results.jsonl
GHCRTS=-N1 python3 benchmarks/fuzz_corpus/run_docker_benchmark.py \
  --manifest /tmp/occam-study/corpus.json --tools occam,ddmin --jobs 4 \
  --oracle-timeout 1 --budget 12 --max-oracle-executions 100 \
  --out /tmp/occam-study/corpus-results.jsonl
python3 benchmarks/tools/p2_scale_study.py profiles /tmp/occam-study/profiles.json \
  --binary "$(env -u GHCRTS cabal list-bin exe:occam-eval)" \
  --out /tmp/occam-study/profiles-run --timeout 2 --budget 30 --max-oracle-executions 500
```

Prepare/build native executables first. Workload preparation and profile output refuse nonempty directories rather than overwriting evidence. Profiles run serially with `GHCRTS=-N1`; one physical preflight is shared per input and its budget debit is applied to each profile. Settings and raw stages distinguish statement-only B1/B2/byte-WeightedDD/CDD from shared-pass pipelines and the slicing/closure 2×2 ablation. Do not pool these as identical algorithms or compare their direct-predicate timings to wrapper-based batches.

`resets.json` compares fresh SQLite setup with copying a closed database template, on the same fixed-setup query. The predicate verifies schema, data, indexes, triggers, sequences and `user_version`, and applies `foreign_keys` per connection. This is a SQLite file-state comparison, not a server/container snapshot. No speculative parallel candidate search or incremental graph framework is enabled.

## Optional upstream comparisons

Prepare upstream sources/dependencies **outside** the measured reduction. SQLancer requires a Git checkout at [`a7d60ee`](https://github.com/sqlancer/sqlancer/tree/a7d60ee450e6a22ca38599c14e984a49773f4745), a JDK, and its built JAR/dependency directory. Set `SQLANCER_SOURCE` and run `mvn -q -Dmaven.test.skip=true package -f "$SQLANCER_SOURCE/pom.xml"` once before the comparison; the adapter no longer fetches or rebuilds on each input. It compiles the small Reproducer adapter per invocation, inside the shared deadline.

SQLreduce requires a checkout at [`568c8a9`](https://github.com/credativ/sqlreduce/tree/568c8a9a31601ead7128d0b067658b4d5a26821d), exposed through `SQLREDUCE_SOURCE`. The exercised optional environment used pglast 8.7, psycopg2-binary 2.9.13 and PyYAML 6.0.3. These are benchmark prerequisites, not Occam core dependencies.

Both adapters run the **actual upstream search** through the common external predicate: SQLancer's StatementReducer/ASTBasedReducer use a virtual JDBC provider, and SQLreduce's AST/rules use a replaced candidate-execution callback. This is not either project's native DBMS oracle. SQLreduce accepts complete PostgreSQL scripts; its old single-SELECT/setup/DSN/SQLSTATE adapter flags and reconnect loop are gone. Use the same input, reset and predicate for every compared tool; `reduction_track` is a per-input label, not a tool-specific input substitution.

SQLite and PostgreSQL multi-statement predicates were exercised; SQLreduce rejects non-PostgreSQL dialects as unsupported. SQLancer/JSQLParser coverage is partial and unsupported AST statements are counted. Other dialects, escaped-string modes and procedural bodies need their own compatibility evidence. Parser/normalization changes must preserve the predicate before reductions begin. The shared checker distinguishes rejection, timeout, infrastructure failure and execution-budget exhaustion, keeps accepted checkpoints, and does not cache control failures.

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

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

`evidence_class` and `oracle_kind` come from the manifest (or documented runner defaults); reports group and pair only records with matching labels and input. These labels do not prove a database was executed.

Comparison-runner `reset_calls` and `reset_time_sec` cover audited reducer callbacks only; initial/final validation calls are excluded and reported separately. Incomplete reducer audit data stays `null`. A zero reducer-scope count does not mean no reset was attempted: inspect `oracle_audit_file` for validation-phase attempts and failures. The single-input pipeline records `reset_preflight_sec`, `reset_reduction_sec`, and `reset_validation_sec`; `reset_time_sec` sums these stages once only when all are known, otherwise it is `null`.

`oracle_timeouts` counts reducer-scope observations only: native tools use a fresh `evaluationTimedOutObservations` profile count, callback tools use observed callback/reducer-audit outcomes, and validation-phase audit rows are excluded. If a native count is missing or invalid, it remains `null`.


## Reports and synthetic inputs

`benchmarks/report.sh` formats JSONL records and cannot establish SQL validity or bug reproduction. `--table` is the default, `--summary` groups by evidence class/oracle kind/tool, and `--md` emits Markdown. `--json` emits case/tool records as a JSON array; `--csv` emits one case/tool record per CSV row. These two data modes do not merge the separate batch-summary sidecar.

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

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

## Reports and synthetic inputs

`benchmarks/report.sh` formats JSONL records; it reports recorded fields and cannot establish SQL validity or bug reproduction:

```bash
./benchmarks/report.sh --summary
cat results.jsonl | ./benchmarks/report.sh --summary
```

Generate a synthetic SQL stress input directly when needed:

```bash
python3 benchmarks/tools/fuzz_generator.py \
  --dialect postgres --stmts 100 --depth 12 --inject pg_01 --out /tmp/fuzz.sql
```

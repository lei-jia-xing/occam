# Occam Benchmarks (4 Tiers)

| Tier | What | Where |
| :--- | :--- | :--- |
| 1 | In-process engine microbench, zero subprocesses (`benchmark occam-bench` via `cabal bench`, GHC RTS allocated-bytes gate) | `bench/Main.hs` |
| 2 | Fast single-process SQLite oracle (no wrapper, no disk) | `bench.py oracle` |
| 3 | Deterministic corpus + scale study + AB/BA runner | `bench.py prepare/run/report` |
| 4 | Real DBMS crash reproduction | `real_fuzz_corpus/` + `harness/` |

## Tier 2: SQLite oracle

Reads candidate SQL from stdin, replays it in `:memory:` with
`PRAGMA foreign_keys=ON`, and exits `0` iff the final rowset matches
`contract.json` (plus the `iterdump` state digest when `check_state` is set):

```bash
echo "SELECT 1;" | python3 benchmarks/bench.py oracle case.contract.json
```

## Tier 3: corpus runs

```bash
# Stage the 9 deterministic cases + noise scale cases with contracts.
python3 benchmarks/bench.py prepare --out /tmp/occam-bench --scales 64,256
# AB/BA interleaved reductions (default vs --intra-statement arms).
python3 benchmarks/bench.py run --manifest /tmp/occam-bench/manifest.json \
  --out /tmp/occam-bench-runs --occam occam
# 4-dimension report: quality, search, engine overhead, throughput.
python3 benchmarks/bench.py report /tmp/occam-bench-runs
```

`prepare` qualifies every case with 3 controls (original accepts, empty and
`SYNTAX ERROR;` reject). `run` alternates arm order per repetition to cancel
host drift, and invokes:

```text
occam <sql> --oracle "python3 bench.py oracle <contract>" --dialect sqlite \
  --timeout 2 --budget 30 --out <run_dir> --quiet [arm flags...]
```

`corpus/` holds the 9 curated SQLite cases (CTE analytics, savepoint churn,
deep expressions, diamond DAG, upsert, views, 3 column fixtures). Report
metrics: `reduced_bytes`/`reduced_statements`/`reduction_ratio` (quality),
`oracle_attempts`/`accepted`/`cache_hits`/Attempt-AUC (search),
parse FFI + decode + candidate-build micros (overhead),
`evaluationReductionSeconds`/`status` (throughput). Attempt-AUC is the
normalized area of `best_bytes / initial_bytes` over all attempts (lower is
faster). Extra arms: repeat `--arm NAME=FLAGS`.

## Tier 4: real crashes

Authentic multi-statement PostgreSQL/MySQL/SQLite fuzz crashes with
`.meta.json` provenance + crash signatures (see `real_fuzz_corpus/README.md`).
Generate the exact reduction command from metadata:

```bash
python3 benchmarks/bench.py crash-cmd \
  --meta benchmarks/real_fuzz_corpus/pg_fuzzer_mcv_clauses.meta.json \
  --out /tmp/occam_reduced/
```

This wires `harness/crash_oracle.py` (exit 0 on matching crash signature) and
`harness/reset_db.sh` (clean `occam_test` per candidate) into occam's
`--oracle`/`--reset` contract. Needs a debug/assertion-built target DBMS.

## Tests

```bash
python3 -m unittest discover -s benchmarks
python3 -m unittest discover -s harness -p "test_*.py"
```

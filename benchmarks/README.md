# Occam Benchmarks

Occam benchmark architecture spans four distinct tiers:

| Tier | What | Where |
| :--- | :--- | :--- |
| 1 | In-process engine microbenchmarks (parse, DAG analysis, backward slice, span surgery, HAMT cache) | `bench/Main.hs` via `cabal bench` |
| 2 | In-process native SQLite result oracle (`:memory:`, column names and typed rows) | `Occam.Oracle.Sqlite` |
| 3 | Native corpus and scale timings, plus the paired result-slicing baseline | `bench/Main.hs` (`reduction/scale-064`, `reduction/corpus/*`) |
| 4 | Real DBMS crash reproduction (71 multi-statement cases across 6 DBMS targets) | `benchmarks/real_fuzz_corpus/` + `harness/` |

## Tiers 1–3: Native Haskell Benchmarks (`occam-bench`)

All engine microbenchmarks and corpus reduction runs execute natively in Haskell via `tasty-bench` and `direct-sqlite`:

```bash
# Run all benchmarks (microbenchmarks + full reduction corpus):
cabal bench

# Run only the SQLite reduction corpus benchmarks:
cabal bench --benchmark-options="-p corpus --stdev 20 --timeout 5s"

# Run with GHC RTS allocation memory statistics:
cabal bench --benchmark-options="+RTS -T -RTS"
```

`make bench-product` also exercises the shared native evaluator on two audit
workloads with 512 candidate deletions. One accepts deletions frequently. The
other preserves a row count and rejects all deletions from a wide SQL script.
Both check the exact endpoint, the minimality claim, and two fresh validations.
`make microbench` includes native cache key eligibility and raw versus prepared
cache lookups for 4 KiB, 32 KiB, and 128 KiB keys.

### GHC cost-centre flame graph

Run `make flamegraph` from the pinned development environment. It builds
a profiling executable in `dist-newstyle/flamegraph` and runs SQLite
result-preserving intra-statement reduction on `06_views_and_aggregates.sql`
with one RTS capability. Open `dist-newstyle/flamegraph/occam.prof` in
[Speedscope](https://www.speedscope.app/) and select **Left Heavy**. The same
run writes `stats.json` next to the profile. Set `FLAMEGRAPH_SQL` to profile
another SQLite input.

**NOTE** GHC cost-centre instrumentation changes optimization and excludes time
in safe foreign calls. Use the profile to locate Haskell work, the regular
benchmark for timings, and native CPU sampling for SQLite/DuckDB C calls.
[GHC profiling guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/profiling.html)

### In-Process SQLite Evaluation (`Occam.Oracle.Sqlite`)

Tiers 2 and 3 evaluate candidate SQL directly against an in-memory SQLite instance via `direct-sqlite`, using SQLite-prepared statement boundaries and a per-probe timeout. It checks the
last result set’s column names and typed rows and requires trailing statements to
execute successfully.

### Search policy comparisons

```bash
cabal build bench:occam-bench --offline
bin=$(cabal list-bin bench:occam-bench --offline)
"$bin" --search-baseline 8 unlimited /tmp/search-unlimited.json +RTS -N1 -RTS
"$bin" --search-baseline 8 64 /tmp/search-calls-64.json +RTS -N1 -RTS
"$bin" --search-baseline 8 wall:20 /tmp/search-wall-20.json +RTS -N1 -RTS
```

For `--search-baseline`, the control enables statement reduction and the
treatment adds intra-statement rewrites. Use `--literal-batch-baseline` with
the same arguments to compare single edits against batching with
intra-statement rewrites enabled in both arms. Both use the shared evaluation entry point, a fresh result oracle
and one worker. The cohort includes removable and required VALUES rows,
independent and interacting literals, and both precise and globally fenced
graphs. The JSON records AB/BA order, warmups, phase metrics, physical calls
including preflight and validation, and the improving checkpoint curve.

Use `--values-schedule-baseline` to compare the coarse-to-fine VALUES
schedule with deferred fine chunks. Use `--combined-syntax-baseline`
to compare both experimental policies together with the current schedule.
That mode adds scale-064, deep expressions, and views/aggregates with
explicit reference results.

Use `--statement-yield-baseline` for graph-free ddmin versus the current
statement reducer on 8- and 32-row scripts with interleaved noise statements.
The report includes subset and complement oracle costs, selected byte savings,
and closure projections rejected for duplicate bytes or lack of shrinkage.
Use `--statement-order-baseline` to isolate complement-first ordering while
retaining both search lanes and the normal audit.
Use `--statement-weight-baseline` to compare equal-count and byte-weighted
partitions with complement-first ordering in both arms. This comparison
includes scripts with unusually large noise statements.

Use `--reparse-baseline` to compare single-unit refresh with full script
reparse after an accepted syntax edit, with both experimental syntax
schedules enabled. Use `--worker-baseline` with `+RTS -N4 -RTS` and the
same CPU affinity for both arms to compare one and four ordered workers on
SQLite. Each worker gets a fresh in-memory database. Include speculative
starts and cancellations when comparing physical calls.

The call cap includes a reserve for two final checks. A run that hits a
configured cap retains its status and checkpoint. Successful final checks
and agreement with the reference result are required in every recorded arm,
while completed runs also require a confirmed statement audit. Endpoint
quality and timing comparisons should report capped runs separately.

### Result semantics and dependency slicing baseline

Build in the pinned development environment and run the benchmark directly to
control RTS options and CPU affinity. Pin `OCCAM_DUCKDB_LIB` to the library of the
DuckDB build being measured. Use an available, otherwise idle CPU for `<cpu>`.

```bash
cabal build bench:occam-bench --offline
bin=$(cabal list-bin bench:occam-bench --offline)
OCCAM_DUCKDB_LIB=/path/to/libduckdb.so \
  taskset -c <cpu> "$bin" --result-baseline 12 /tmp/occam-result-baseline.json +RTS -N1 -RTS
```

- A seeds the slice from the last frontend statement. B uses the engine’s last
  result statement when the ordinal and count align. Both use the same tracking,
  oracle, parser, reducer settings, evaluation entry point, and audit policy.
- The runner warms both arms once per case and alternates AB/BA in adjacent
  pairs. It runs one job with caching enabled, a 5-second per-probe timeout, a
  60-second per-arm budget, and a cap of 256 physical oracle executions per arm
  including preflight and final validation. Each arm starts with fresh
  evaluation state and checkpoint. JSON retains all measured runs in execution
  order, plus warmups and failures.
- Fixtures fix the expected result in separate SQL and exercise a result before
  trailing DDL, rolled-back writes, an opaque aggregate fence, a DuckDB
  intermediate result with an unsupported type, and an empty DuckDB final
  result. Controls must reject a result change and a failed trailing statement.
  Both arms must complete, shrink the script, confirm statement minimality,
  pass two fresh final oracle executions, and match the reference SQL result.
  A timeout or oracle error fails the baseline. A missing DuckDB library fails
  fixture setup.
- Compare final bytes and statements and reduction attempts and physical
  database observations per case. For timing, report the median of within-pair
  B/A ratios per case and retain the raw ratios. Keep warmups and unsuccessful
  pairs out of the timing estimate. The JSON has phase timings, cost
  attribution, and an attempt curve for each run. Record the source revision
  and local diff, executable and DuckDB library digests, machine, and CPU
  affinity alongside it when comparing runs across builds.

#### Reference run

One local run used an Intel i7-7700HQ pinned to logical CPU 2, GHC 9.10.3
with the project’s `-O2` settings, DuckDB 1.5.5, and `+RTS -N1`. The
benchmark binary SHA-256 was
`bd974d770ab1da28de610a2314f5e0da381c6f21c0a110cefd22c6bc0b95c83a`.
All 120 measured arms and 10 warmups completed and passed the audit, two final
oracle checks, and reference-result check. A and B produced byte-identical
SQL in every pair. Reduction attempts and physical database observations
were equal within each arm in this run, excluding preflight and validation.

| Fixture | Final bytes / statements | Reduction attempts A → B | Total wall B/A, median (range) |
| :--- | ---: | ---: | ---: |
| SQLite trailing DDL | 21 / 1 | 5 → 3 | 0.75 (0.56–1.16) |
| SQLite savepoint | 86 / 3 | 30 → 30 | 0.99 (0.58–1.66) |
| SQLite opaque aggregate | 97 / 3 | 11 → 11 | 0.96 (0.66–1.36) |
| DuckDB trailing DDL | 21 / 1 | 5 → 4 | 0.90 (0.85–1.02) |
| DuckDB empty result | 33 / 1 | 5 → 3 | 0.73 (0.64–0.86) |

The call counts are stable across repetitions. The elapsed ratios describe
this machine and these fixtures.

#### Notes & Limitations

- The count-mismatch fallback is tested by feeding mismatched counts and invalid ordinals to the reducer and comparing its search trace with A; the timed corpus uses actual engine observations with matching statement counts. The validated contract is the last result set (including row order and column names).
- The legacy `reduction-audit-validation` and `reduction-duckdb-validation` timings validate the SQL returned by their audit, but measure a direct reducer/audit probe with one final oracle call. Use `--result-baseline` to measure the shared evaluation path.
- Remaining measurement gaps: naturally mismatched engine/frontend fixtures, larger inputs, and isolating native DuckDB setup overhead.

## Tier 4: Real DBMS Crash Reproduction

71 authentic multi-statement fuzz crashes across PostgreSQL, MySQL, MariaDB, ClickHouse, SQLite, and DuckDB with `.meta.json` provenance and crash signatures (see [real_fuzz_corpus/README.md](real_fuzz_corpus/README.md) and [harness/README.md](../harness/README.md)).

Run Occam against a target crash:

```bash
occam benchmarks/real_fuzz_corpus/pg_fuzzer_mcv_clauses.sql \
  --oracle "occam-harness oracle --dbms postgres --container-name occam-target-pg --expected-signature 'clauses != NIL'" \
  --reset "occam-harness reset --dbms postgres --container-name occam-target-pg" \
  --dialect postgresql \
  --timeout 10 \
  --jobs 1 \
  --out /tmp/occam-reduced.sql
```

The oracle requires `--log-file` or `--container-name`; client stderr is not
accepted as crash evidence. Use `--jobs 1` because the harness shares one
`occam_test` database.

## Unit & Contract Tests

```bash
# Full Haskell test, lint, harness, and proof suites:
make check

# Crash oracle & harness contract tests:
make harness-check
```

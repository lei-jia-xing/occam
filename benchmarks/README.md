# Occam Benchmarks

Occam benchmark architecture spans four distinct tiers:

| Tier | What | Where |
| :--- | :--- | :--- |
| 1 | In-process engine microbenchmarks (parse, DAG analysis, backward slice, span surgery, HAMT cache) | `bench/Main.hs` via `cabal bench` |
| 2 | In-process native SQLite contract oracle (<0.05ms/probe, `:memory:`, rowset equality) | `Occam.Oracle.Sqlite` |
| 3 | Native corpus & scale reduction benchmarks (AB/BA comparative evaluation of reduction passes) | `bench/Main.hs` (`reduction/scale-064`, `reduction/corpus/*`) |
| 4 | Real DBMS crash reproduction (PostgreSQL/MySQL/SQLite authentic bugs) | `benchmarks/real_fuzz_corpus/` + `harness/` |

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

### In-Process SQLite Evaluation (`Occam.Oracle.Sqlite`)

Tiers 2 and 3 evaluate candidate SQL directly against an in-memory SQLite instance via `direct-sqlite`, verifying:
1. SQL statement completion via `sqlite3_complete`.
2. Execution termination bounded by `Database.SQLite3.interruptibly` (100ms timeout guarding against runaway recursive CTEs).
3. Exact rowset equality and column name alignment against expected contracts without any disk I/O or external subprocess invocation.

## Tier 4: Real DBMS Crash Reproduction

Authentic multi-statement PostgreSQL/MySQL/SQLite fuzz crashes with `.meta.json` provenance and crash signatures (see [real_fuzz_corpus/README.md](real_fuzz_corpus/README.md)).

Run Occam against a target crash:

```bash
occam benchmarks/real_fuzz_corpus/pg_fuzzer_mcv_clauses.sql \
  --oracle "python3 harness/crash_oracle.py --dbms postgres --container-name occam-target-pg --expected-signature 'clauses != NIL'" \
  --reset "./harness/reset_db.sh --dbms postgres --container-name occam-target-pg" \
  --dialect postgres \
  --timeout 10 \
  --jobs 1 \
  --out /tmp/occam_reduced/
```

The oracle requires `--log-file` or `--container-name`; client stderr is not
accepted as crash evidence. Use `--jobs 1` because the harness shares one
`occam_test` database.

## Unit & Contract Tests

```bash
# Haskell & Rust test suites + verification checks:
make check

# Crash oracle contract test:
python3 harness/test_crash_oracle.py
```

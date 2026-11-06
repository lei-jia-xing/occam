# Optional fuzz producers

The optional producer drivers run upstream fuzzers; they do not collect crash reports, reproduce bugs, or reduce SQL. `docker_entrypoint.sh` starts its configured PostgreSQL and MySQL services and dispatches a supplied command (or the default benchmark runner). To reduce existing SQL, use `benchmarks/live_fuzz_pipeline.sh` with a predicate that accepts the target behavior (see `benchmarks/README.md`). Your predicate or reset command must handle real database reset and isolation.

## SQLsmith

`sqlsmith_driver.sh` runs SQLsmith against PostgreSQL using the standard `PGHOST`, `PGPORT`, `PGDATABASE`, and `PGUSER` variables. `--dump-file` asks SQLsmith to write generated queries to a file:

```sh
./harness/sqlsmith_driver.sh --max-queries 1000 --dump-file /tmp/fuzz.sql
./benchmarks/live_fuzz_pipeline.sh --input /tmp/fuzz.sql \
  --oracle './interesting.sh' --dialect postgres
```

## SQLancer

`sqlancer_driver.sh` runs SQLancer against PostgreSQL or MySQL and saves its execution log; it does not extract crash cases or produce a reduction corpus. It fetches and builds a pinned SQLancer checkout on first use. The SQLancer source/build attempt has not completed successfully in this environment, so runtime integration remains unverified.

```sh
./harness/sqlancer_driver.sh postgres --queries 1000 --out-dir /tmp/sqlancer-logs
```

`docker_entrypoint.sh` starts the configured PostgreSQL and MySQL services, then runs the supplied command (or `benchmarks/run_all.sh` when no command is supplied); it does not provide candidate-level database isolation.
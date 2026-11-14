# Optional fuzz producer and container entrypoint

The optional SQLsmith driver runs an upstream fuzzer; it does not collect crash reports, reproduce bugs, or reduce SQL. `docker_entrypoint.sh` starts its configured PostgreSQL and MySQL services and dispatches a supplied command (or an interactive shell). To reduce existing SQL, use `benchmarks/live_fuzz_pipeline.sh` with a predicate that accepts the target behavior (see `benchmarks/README.md`). Your predicate or reset command must handle real database reset and isolation.

## SQLsmith

`sqlsmith_driver.sh` runs SQLsmith against PostgreSQL using the standard `PGHOST`, `PGPORT`, `PGDATABASE`, and `PGUSER` variables. `--dump-file` asks SQLsmith to write generated queries to a file:

```sh
./harness/sqlsmith_driver.sh --max-queries 1000 --dump-file /tmp/fuzz.sql
./benchmarks/live_fuzz_pipeline.sh --input /tmp/fuzz.sql \
  --oracle './interesting.sh' --dialect postgres
```

The SQLancer reducer comparison lives in `benchmarks/upstream/sqlancer_adapter.sh`; it is separate from producing new fuzz input.

`docker_entrypoint.sh` starts the configured PostgreSQL and MySQL services, then runs the supplied command (or starts an interactive shell when no command is supplied); it does not provide candidate-level database isolation.

# Database harnesses

Each harness recreates its configured test database before running the candidate SQL. Use a disposable local database; the defaults are `occam_test`.

Run Occam against PostgreSQL:

```sh
occam crash.sql --oracle "./harness/pg_harness.sh"
```

Run Occam against MySQL:

```sh
occam crash.sql --oracle "./harness/mysql_harness.sh"
```

PostgreSQL uses `PGDATABASE`, `PGHOST`, `PGPORT`, and `PGUSER`; MySQL uses `MYSQL_DATABASE`, `MYSQL_HOST`, and `MYSQL_USER`.

Run Occam against SQLite:

```sh
occam crash.sql --oracle "./harness/sqlite_harness.sh"
```

SQLite uses `SQLITE_DB` (default: `/tmp/occam_sqlite_test.db`) and `SQLITE_BIN` (default: `sqlite3`).

Run Occam against DuckDB:

```sh
occam crash.sql --oracle "./harness/duckdb_harness.sh"
```

DuckDB uses `DUCKDB_DB` (default: `/tmp/occam_duckdb_test.db`) and `DUCKDB_BIN` (default: `duckdb`).
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
# Occam Crash Oracle & Isolation Harness

This directory provides the crash reproduction oracle, target DBMS container definitions, and candidate state reset scripts for validating Occam on authentic multi-statement DBMS crashes across **PostgreSQL**, **MySQL**, **MariaDB**, **ClickHouse**, **SQLite**, and **DuckDB**.

---

## 1. Occam Predicate Contract

Occam communicates with external test harnesses using standard POSIX stream and exit-code semantics:

| Contract Component | Protocol Specification |
| :--- | :--- |
| **`--oracle <cmd>`** | Candidate SQL is piped into `sys.stdin` of `<cmd>`.<br>• **Exit 0**: Accepted / interesting.<br>• **Any nonzero exit**: Rejected. Occam does not reserve predicate exit codes, so **124 is not a timeout signal**; it is just another rejection. |
| **`--reset <cmd>`** | Executed before candidate evaluation when specified.<br>• PostgreSQL: drops and recreates `occam_test` from `template_occam`.<br>• MySQL: clears `super_read_only` / `read_only`, drops and recreates `occam_test`.<br>• MariaDB: clears `read_only`, drops and recreates `occam_test`.<br>• ClickHouse: synchronously drops (`DROP DATABASE ... SYNC`) and recreates `occam_test`.<br>• SQLite / DuckDB: stateless `:memory:` per-probe execution; if `--container-name` is passed, restarts/verifies the idle target container.<br>• Restarts or waits for the server/container after a crash.<br>• **Exit 0**: State cleanly reset and target ready.<br>• **Exit non-zero**: Infrastructure failure. This is the only clean infrastructure-error channel in Occam's predicate contract. |
| **`OCCAM_WORKDIR`** | Occam provides a clean candidate temporary directory in `OCCAM_WORKDIR`, `TMPDIR`, `TMP`, and `TEMP`. The oracle uses it as the client working directory. |

**Timeout ownership.** `crash_oracle.py` disables its own client timeout by default (`--timeout 0`). Under Occam, leave it disabled and let Occam's `--timeout` kill the whole process group. Occam starts its timer before reset, so an internal oracle timeout that fires first would turn an inconclusive candidate into a rejection. Pass `--timeout` only for standalone use.

---

## 2. Crash Evidence and Logs

### Server DBMS Targets (`postgres`, `mysql`, `mariadb`, `clickhouse`)

A verdict is accepted only from the **DBMS server log**. Client stderr is used only to corroborate connection loss; it is never scanned for crash signatures, because clients echo the failing statement in error messages. This prevents candidates containing strings such as `Segmentation fault` from being accepted as crashes.

- Pass `--log-file` (host path) or `--container-name` (read logs with `docker exec` + `docker logs --since`). If neither is given, the oracle rejects with `log-source-required`.
- `--container-log` selects the log path inside the container. Defaults:
  - `postgres`: `/var/log/postgresql/postgresql.log`
  - `mysql` / `mariadb`: `/var/log/mysql/error.log`
  - `clickhouse`: `/var/log/clickhouse-server/clickhouse-server.err.log`
- `--decision-log` appends one JSON verdict record per invocation. This is useful because Occam discards oracle stdout/stderr.
- `--allow-stderr-only` is available for trusted, log-less setups. It is explicitly unsafe and should not be used for reduction.

PostgreSQL log files default to mode `0600`, which is not readable from a non-root host user. The target images keep logs readable (`0644`) across PostgreSQL, MySQL, MariaDB, and ClickHouse. If a bind-mounted log is still unreadable, use `--container-name` instead.

### Embedded DBMS Targets (`sqlite`, `duckdb`)

SQLite and DuckDB execute **statelessly per probe** against `:memory:` in a fresh engine process — either inside a target container via `docker exec -i <container> occam-exec` (when `--container-name` is passed) or directly via local `sqlite3` / `duckdb` (`-batch :memory:`).

- No `--log-file` is needed or used for embedded targets.
- Crash verdicts combine POSIX process termination signals (`SIGABRT`, `SIGSEGV`, `SIGBUS`, `SIGILL`, `SIGFPE`, etc.) with sanitized engine stderr evidence (C `assert()`, ASan/UBSan reports, and DuckDB `INTERNAL Error` / `FATAL Error: database has been invalidated`).
- SQL error echoes (`Parse error` / `Runtime error` / `Error` in SQLite; `Catalog Error` / `Binder Error` / `Parser Error` / `Constraint Error` + `LINE N:` blocks in DuckDB) are filtered out so quoted identifiers cannot spoof crash evidence.
- Non-crash conditions are explicitly distinguished:
  - SQLite `SQLITE_ABORT` (exit `4`) is rejected as `healthy_no_fault` (`reason: "sqlite-abort"`).
  - DuckDB `Out of Memory Error:` and `SIGKILL` (`137` / `-9`) are retained as their own OOM cause (`corroboration.oom = true`, `reason: "oom"` / `"oom-kill"`, `healthy_no_fault`) rather than misclassified as internal errors or crashes.
  - `docker exec` infrastructure failures (`125`, `126`, `127`) are rejected as `missing_evidence` (`reason: "container-exec-failed"`).

---

## 3. Target DBMS Images

### PostgreSQL (`Dockerfile.target-pg`)

Builds PostgreSQL from source (default `REL_13_0`) with `--enable-cassert` and `--enable-debug` (`CFLAGS="-O1 -g -fno-omit-frame-pointer"`).

```bash
docker build -t occam-target-pg -f harness/Dockerfile.target-pg .
# Or a specific tag/branch/SHA:
docker build -t occam-target-pg -f harness/Dockerfile.target-pg --build-arg PG_VERSION=REL_12_0 .

docker run -d --name occam-target-pg --restart unless-stopped \
  -p 127.0.0.1:5432:5432 \
  -v /tmp/occam-pg-log:/var/log/postgresql \
  occam-target-pg
```

Configures `logging_collector = on`, `log_file_mode = 0644`, `template_occam`, and `occam_test`.

### MySQL 8.0 (`Dockerfile.target-mysql`)

Pinned to MySQL Community Server `8.0.20` debug binaries (`mysqld-debug`) on Ubuntu 18.04 (EOL; isolated crash reproduction only).

```bash
docker build -t occam-target-mysql -f harness/Dockerfile.target-mysql .
docker run -d --name occam-target-mysql --restart unless-stopped \
  -p 127.0.0.1:3306:3306 \
  -v /tmp/occam-mysql-log:/var/log/mysql \
  occam-target-mysql
```

The entrypoint supervises `mysqld-debug`, logs to `/var/log/mysql/error.log`, and restarts after assertion aborts or signals.

### MariaDB (`Dockerfile.target-mariadb`)

Builds MariaDB from source (default `10.5.8`) with `CMAKE_BUILD_TYPE=Debug` and optional `SANITIZER=asan`, supervising `mysqld` on port `3306` and `/var/log/mysql/error.log`.

```bash
docker build -t occam-target-mariadb -f harness/Dockerfile.target-mariadb .
docker run -d --name occam-target-mariadb --restart unless-stopped \
  -p 127.0.0.1:3307:3306 \
  -v /tmp/occam-mariadb-log:/var/log/mysql \
  occam-target-mariadb
```

### ClickHouse (`Dockerfile.target-clickhouse`)

Installs pinned official ClickHouse `.deb` packages (default `26.8.44.3`, optional `INSTALL_DBG=1`), supervising `clickhouse-server` on native TCP port `9000` (HTTP `8123`) and `/var/log/clickhouse-server/clickhouse-server.err.log`.

```bash
docker build -t occam-target-clickhouse -f harness/Dockerfile.target-clickhouse .
docker run -d --name occam-target-clickhouse --restart unless-stopped \
  -p 127.0.0.1:9000:9000 \
  -v /tmp/occam-ch-log:/var/log/clickhouse-server \
  occam-target-clickhouse
```

### SQLite (`Dockerfile.target-sqlite`) & DuckDB (`Dockerfile.target-duckdb`)

Both images run `occam-idle` (`target_idle.sh`) and execute probes in fresh `:memory:` processes via `/usr/local/bin/occam-exec` (`target_exec_embedded.sh`), with optional `SANITIZER=asan` (`ASAN_OPTIONS="detect_leaks=0:abort_on_error=1:disable_coredump=1:symbolize=1"`). SQLite supports both release amalgamation zips (`SQLITE_VERSION` / `SQLITE_YEAR`) and Fossil snapshots (`SQLITE_REF`, e.g., `trunk:2020-02-10`).

```bash
docker build -t occam-target-sqlite -f harness/Dockerfile.target-sqlite .
docker run -d --name occam-target-sqlite --restart unless-stopped occam-target-sqlite

docker build -t occam-target-duckdb -f harness/Dockerfile.target-duckdb .
docker run -d --name occam-target-duckdb --restart unless-stopped occam-target-duckdb
```

---

## 4. Crash Oracle (`crash_oracle.py`)

`harness/crash_oracle.py` executes candidate SQL from stdin, collects server logs or embedded engine status/stderr, validates crash signatures, and extracts normalized crash signatures for deduplication and RQ3 slippage analysis.

### Arguments

- `--dbms {postgres,mysql,mariadb,clickhouse,sqlite,duckdb}`: Target DBMS (default: `postgres`, or `DBMS` env).
- `--host`: DBMS host or PostgreSQL unix socket directory (default: `127.0.0.1`, or `PGHOST` / `MYSQL_HOST` / `MARIADB_HOST` / `CLICKHOUSE_HOST`).
- `--port`: DBMS port (default: `5432` postgres, `3306` mysql/mariadb, `9000` clickhouse).
- `--socket`: MySQL/MariaDB unix socket path (overrides `--host`/`--port`).
- `--user`: DBMS user (default: `postgres` / `root` / `default`).
- `--password`: DBMS password (prefer `PGPASSWORD` / `MYSQL_PWD` / `MARIADB_PWD` / `CLICKHOUSE_PASSWORD` env vars).
- `--dbname`: Target test database (default: `occam_test`; `:memory:` for embedded).
- `--engine-bin`: Override embedded engine executable (`sqlite3` / `duckdb` locally, or `occam-exec` inside `--container-name`).
- `--expected-signature`: Regex or literal substring to match in fault evidence (e.g., `TRAP: FailedAssertion` or `subselect.c:2883`). Invalid regexes fall back to literal matching and are marked in the decision log.
- `--log-file`: Host path to the server log (server DBMSes).
- `--container-name`: Docker container name (reads logs for server DBMSes; runs `occam-exec` for embedded DBMSes).
- `--container-log`: Server log path inside the container.
- `--decision-log`: Append JSON verdict records to this file.
- `--baseline-signature`: Baseline normalized crash signature (`id`, `hash`, JSON string, or `.json`/`.jsonl` file path) for RQ3 slippage tracking (`BASELINE_SIGNATURE` env).
- `--reject-on-slippage`: Reject faults (`exit 1`, `classification: "confirmed_fault"`, `reason: "signature-slippage"`) when the observed `crash_signature` differs from `--baseline-signature` (`ORACLE_REJECT_ON_SLIPPAGE` env).
- `--stack-frames N`: Top-N non-wrapper stack frames retained in normalized crash signatures (default: `5`, or `ORACLE_STACK_FRAMES` env).
- `--print-signature`: Print the extracted `crash_signature` JSON object to stdout on fault (off by default; for standalone triaging).
- `--timeout`: Optional standalone client timeout in seconds (default: `0` = disabled). Do not use under Occam; use Occam's `--timeout`.
- `--allow-stderr-only`: Unsafe fallback for trusted, log-less server setups.
- `--allow-psql-meta`: Unsafe; allows psql backslash meta-commands. By default, candidates with line-leading `\` are rejected.
- `--verbose`: Print acceptance/rejection diagnostics to stderr.

### Structured Crash Signatures & Slippage Tracking (RQ3)

Whenever a fault is detected, `crash_oracle.py` builds a deterministic `crash_signature` record in `--decision-log` (and optionally stdout with `--print-signature`), stripping volatile PIDs, timestamps, ASLR `0x...` addresses, build directory prefixes, and signal/sanitizer wrapper frames (`__asan_*`, `handle_fatal_signal`, `BaseDaemon`, `abort`, etc.):

- **`id` / `hash`**: Canonical normalized identifier and 16-char SHA-256 hash across:
  - Sanitizer findings (`asan:<kind>:<top_non_wrapper_frame>` / `ubsan:<kind>:<loc>`)
  - Assertion identities (`assertion:<file>:<line>:<func>:<expr>`)
  - Engine internal/logical/panic errors (`internal_error:<msg>`, `logical_error:<msg>`, `panic:<msg>`)
  - Signals + top-N stack frames (`signal:<SIG>:<frame0>|...|<frameN-1>`)
- **`slippage`**: When `--baseline-signature` is provided, logs `{"baseline", "observed_id", "observed_hash", "slipped", "match_type", "category_changed"}`. Without `--reject-on-slippage`, slippage is measured and logged passively; with `--reject-on-slippage`, any slipped crash is rejected (`reason: "signature-slippage"`).

### Standalone Testing

```bash
# PostgreSQL
python3 harness/crash_oracle.py \
  --dbms postgres --host 127.0.0.1 --port 5432 \
  --log-file /tmp/occam-pg-log/postgresql.log \
  --expected-signature "FailedAssertion.*subselect.c" \
  < candidate.sql

# Embedded SQLite inside target container
python3 harness/crash_oracle.py \
  --dbms sqlite --container-name occam-target-sqlite \
  --expected-signature "sqlite3VdbeExec" \
  --print-signature \
  < sqlite_case.sql
```

---

## 5. State Reset (`reset_db.sh`)

`harness/reset_db.sh` restores target database state before each candidate execution.

- **PostgreSQL**: terminates active connections to `occam_test` and recreates it from `template_occam`.
- **MySQL**: clears `super_read_only` / `read_only` globals (for cases such as `mysql_bug_91975`), then drops and recreates `occam_test`.
- **MariaDB**: clears `read_only` (MariaDB lacks `super_read_only`), then drops and recreates `occam_test`.
- **ClickHouse**: runs `DROP DATABASE IF EXISTS ... SYNC` followed by `CREATE DATABASE ...`.
- **SQLite / DuckDB**: stateless per-probe execution; returns `0` immediately locally, or ensures the target container is running and responsive when `--container-name` is specified.
- If a server/container died from a prior candidate crash, waits for recovery or restarts it within `--max-wait`.

```bash
./harness/reset_db.sh --dbms postgres --container-name occam-target-pg
./harness/reset_db.sh --dbms mariadb --container-name occam-target-mariadb
./harness/reset_db.sh --dbms clickhouse --container-name occam-target-clickhouse
./harness/reset_db.sh --dbms sqlite --container-name occam-target-sqlite
```

**Limitations.** Server resets are database-level, not cluster-level: roles, external extensions, other databases, and non-read-only global variables persist across candidates unless using fresh containers or template datadir restores.

**Concurrency.** Server-based targets are designed for `--jobs 1` when sharing `occam_test`; embedded targets (`sqlite`, `duckdb`) are stateless per probe, though container orchestration should still use per-worker containers or isolated databases when scaling `--jobs`.

---

## 6. End-to-End Occam Reduction Example

```bash
occam /path/to/crash_repro.sql \
  --oracle "python3 harness/crash_oracle.py --dbms postgres --host 127.0.0.1 --port 5432 --log-file /tmp/occam-pg-log/postgresql.log --expected-signature 'TRAP: FailedAssertion'" \
  --reset "./harness/reset_db.sh --dbms postgres --host 127.0.0.1 --port 5432 --container-name occam-target-pg" \
  --dialect postgres \
  --timeout 10 \
  --jobs 1 \
  --out /tmp/occam_reduced/
```

`--timeout 10` belongs to Occam. Do not also pass a smaller `--timeout` to `crash_oracle.py`; a nonzero oracle exit is a rejection, not a timeout.

---

## 7. Security

This harness executes untrusted SQL and is **not a sandbox**.

- Target containers use passwordless/trust authentication; bind published ports to `127.0.0.1`, not all interfaces.
- `crash_oracle.py` rejects line-leading psql backslash meta-commands by default, and passes `--binary-mode` to MySQL/MariaDB clients to disable `system`/`source`. `--allow-psql-meta` disables the psql protection.
- Server-side commands (`COPY ... FROM PROGRAM`, file/table functions) can still execute with DBMS process privileges. Use isolated containers and networks for untrusted corpora.
- Passwords should be supplied through environment variables (`PGPASSWORD`, `MYSQL_PWD`, `MARIADB_PWD`, `CLICKHOUSE_PASSWORD`) rather than CLI `--password`.
- Do not run this against a database you care about.

---

## 8. Unit Tests

```bash
python3 -m unittest discover -s harness -p "test_*.py" -v
# or
make harness-check
```

The suite uses mock DBMS clients, mock embedded binaries, and mock `docker` commands (requiring neither a built `occam` binary nor live DBMS containers) to verify exit-code contracts, server-log-only evidence for `postgres`/`mysql`/`mariadb`/`clickhouse`, `occam-exec`/local execution and anti-spoofing for `sqlite`/`duckdb`, OOM/`SQLITE_ABORT` handling, RQ3 crash signature normalization and slippage tracking, and `reset_db.sh` state resets across all 6 DBMSes.

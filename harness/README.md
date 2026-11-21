# Occam Crash Oracle & Isolation Harness

This directory provides the crash reproduction oracle, target DBMS container definitions, and candidate state reset scripts for validating Occam on authentic PostgreSQL and MySQL multi-statement fuzz crashes. SQLite is not supported by this oracle yet.

---

## 1. Occam Predicate Contract

Occam communicates with external test harnesses using standard POSIX stream and exit-code semantics:

| Contract Component | Protocol Specification |
| :--- | :--- |
| **`--oracle <cmd>`** | Candidate SQL is piped into `sys.stdin` of `<cmd>`.<br>• **Exit 0**: Accepted / interesting.<br>• **Any nonzero exit**: Rejected. Occam does not reserve predicate exit codes, so **124 is not a timeout signal**; it is just another rejection. |
| **`--reset <cmd>`** | Executed before candidate evaluation when specified.<br>• PostgreSQL: drops and recreates `occam_test` from `template_occam`.<br>• MySQL: drops and recreates `occam_test` and clears `read_only` / `super_read_only` globals.<br>• Restarts or waits for the server after a crash.<br>• **Exit 0**: State cleanly reset and target database ready.<br>• **Exit non-zero**: Infrastructure failure. This is the only clean infrastructure-error channel in Occam's predicate contract. |
| **`OCCAM_WORKDIR`** | Occam provides a clean candidate temporary directory in `OCCAM_WORKDIR`, `TMPDIR`, `TMP`, and `TEMP`. The oracle uses it as the client working directory. |

**Timeout ownership.** `crash_oracle.py` disables its own client timeout by default (`--timeout 0`). Under Occam, leave it disabled and let Occam's `--timeout` kill the whole process group. Occam starts its timer before reset, so an internal oracle timeout that fires first would turn an inconclusive candidate into a rejection. Pass `--timeout` only for standalone use.

---

## 2. Crash Evidence and Logs

A verdict is accepted only from the **DBMS server log**. Client stderr is used only to corroborate connection loss; it is never scanned for crash signatures, because psql and mysql echo the failing statement in error messages. This prevents candidates containing strings such as `Segmentation fault` from being accepted as crashes.

- Pass `--log-file` (host path) or `--container-name` (read logs with `docker exec`). If neither is given, the oracle rejects with `log-source-required`.
- `--container-log` selects the log path inside the container. Defaults: `/var/log/postgresql/postgresql.log` for PostgreSQL and `/var/log/mysql/error.log` for MySQL.
- `--decision-log` appends one JSON verdict record per invocation. This is useful because Occam discards oracle stdout/stderr.
- `--allow-stderr-only` is available for trusted, log-less setups. It is explicitly unsafe and should not be used for reduction.

PostgreSQL log files default to mode `0600`, which is not readable from a non-root host user. The target images set `log_file_mode = 0644`; the MySQL entrypoint also keeps its error log at `0644`. If a bind-mounted log is still unreadable, use `--container-name` instead.

---

## 3. Target PostgreSQL Image (`Dockerfile.target-pg`)

A reproducible Dockerfile based on Ubuntu 24.04 that builds PostgreSQL from source with assertion checks (`--enable-cassert`) and debug symbols (`--enable-debug` with `CFLAGS="-O1 -g -fno-omit-frame-pointer"`).

### Building the Image

```bash
# Build default PostgreSQL 13 (REL_13_0)
docker build -t occam-target-pg -f harness/Dockerfile.target-pg .

# Or build a specific tag, branch, or commit:
docker build -t occam-target-pg -f harness/Dockerfile.target-pg \
  --build-arg PG_VERSION=REL_12_0 .
```

`PG_VERSION` is fetched by tag, branch, or commit SHA. A shallow fetch is tried first; a full fetch is used as a fallback for servers that do not allow fetching an arbitrary commit by SHA.

### Running the Target Container

```bash
# Bind the port to localhost only; the container uses trust authentication.
docker run -d --name occam-target-pg \
  --restart unless-stopped \
  -p 127.0.0.1:5432:5432 \
  -v /tmp/occam-pg-log:/var/log/postgresql \
  occam-target-pg

# Verify database cluster is ready
pg_isready -h 127.0.0.1 -p 5432
```

The container configures:
- Unix sockets at `/var/run/postgresql` and `/tmp`.
- `logging_collector = on` logging to `/var/log/postgresql/postgresql.log` with `log_statement = 'all'`, `log_min_messages = 'warning'`, and `log_file_mode = 0644`.
- Template database `template_occam` and pre-created test database `occam_test`.

If the postmaster dies, the container exits; `--restart unless-stopped` or `reset_db.sh --container-name` brings it back.

---

## 4. Target MySQL Image (`Dockerfile.target-mysql`)

The MySQL image is pinned to MySQL Community Server 8.0.20 debug binaries on Ubuntu 18.04. Ubuntu 18.04 is EOL; the image is intended for isolated crash reproduction, not general use. For other MySQL versions, override `MYSQL_BUNDLE_URL` and update the pinned package names in the Dockerfile.

```bash
docker build -t occam-target-mysql -f harness/Dockerfile.target-mysql .
# Optional integrity check:
#   --build-arg MYSQL_BUNDLE_SHA256=<sha256>

docker run -d --name occam-target-mysql \
  --restart unless-stopped \
  -p 127.0.0.1:3306:3306 \
  -v /tmp/occam-mysql-log:/var/log/mysql \
  occam-target-mysql
```

The entrypoint supervises `mysqld-debug` and restarts it after an assertion abort or signal. The error log is written to `/var/log/mysql/error.log` with `--log-error-verbosity=3`.

---

## 5. Crash Oracle (`crash_oracle.py`)

`harness/crash_oracle.py` connects to the target DBMS, runs candidate SQL from stdin, captures DBMS disconnects / backend crashes, reads only newly appended server-log bytes, and verifies the crash signature.

### Arguments

- `--dbms {postgres,mysql}`: Target DBMS (default: `postgres`, or `DBMS` env).
- `--host`: DBMS host or PostgreSQL unix socket directory (default: `127.0.0.1`, or `PGHOST` / `MYSQL_HOST`).
- `--port`: DBMS port (default: 5432 for PostgreSQL, 3306 for MySQL).
- `--socket`: MySQL unix socket path (overrides `--host`/`--port`).
- `--user`: DBMS user (default: `postgres` or `root`).
- `--password`: DBMS password (prefer `PGPASSWORD` / `MYSQL_PWD`; the MySQL client receives it through `MYSQL_PWD`, not argv).
- `--dbname`: Target test database (default: `occam_test`).
- `--expected-signature`: Regex or literal substring to match in the **server log** (e.g. `TRAP: FailedAssertion` or `subselect.c:2883`). Invalid regexes fall back to literal matching and are marked in the decision log.
- `--log-file`: Host path to the server log.
- `--container-name`: Docker container name used to read logs via `docker exec` when the host path is unavailable.
- `--container-log`: Server log path inside the container.
- `--decision-log`: Append JSON verdict records to this file.
- `--timeout`: Optional standalone client timeout in seconds (default: 0 = disabled). Do not use under Occam; use Occam's `--timeout`.
- `--allow-stderr-only`: Unsafe fallback for trusted, log-less setups.
- `--allow-psql-meta`: Unsafe; allows psql backslash meta-commands. By default, candidates with line-leading `\` are rejected.
- `--verbose`: Print acceptance/rejection diagnostics to stderr.

### Standalone Testing

```bash
# Test candidate against a running PostgreSQL container
python3 harness/crash_oracle.py \
  --host 127.0.0.1 \
  --port 5432 \
  --user postgres \
  --dbname occam_test \
  --log-file /tmp/occam-pg-log/postgresql.log \
  --expected-signature "FailedAssertion.*subselect.c" \
  < candidate.sql

echo "Oracle result: $?"
```

---

## 6. State Reset (`reset_db.sh`)

`harness/reset_db.sh` restores the target database before each candidate execution.

- PostgreSQL: terminates active connections to `occam_test` and recreates it from `template_occam`.
- MySQL: clears `read_only` / `super_read_only` globals, then drops and recreates `occam_test`. This is required for cases such as `mysql_bug_91975`, which sets `@@global.super_read_only = TRUE`.
- If the server died from a prior candidate crash, waits for recovery or restarts the container/server.
- Uses DBMS-aware environment defaults (`PGHOST` vs `MYSQL_HOST`, `PGPASSWORD` vs `MYSQL_PWD`) after parsing `--dbms`.

```bash
# Reset PostgreSQL database
./harness/reset_db.sh --dbms postgres --host 127.0.0.1 --port 5432 \
  --container-name occam-target-pg

# Reset MySQL database
./harness/reset_db.sh --dbms mysql --host 127.0.0.1 --port 3306 \
  --container-name occam-target-mysql
```

**Limitations.** The reset is database-level, not cluster-level. PostgreSQL roles, extensions installed outside the database, other databases, and `postgresql.auto.conf` persist; MySQL global variables other than the read-only flags, users, grants, and other databases persist. Use a fresh container or restore the datadir from the image templates for full global-state isolation. MySQL has no `CREATE DATABASE ... TEMPLATE`; `template_occam` is only used by PostgreSQL.

**Concurrency.** This harness is designed for `--jobs 1`. All workers would share the same `occam_test`; a reset from one worker can terminate another worker's connections. Do not use `--jobs > 1` without per-worker databases.

---

## 7. End-to-End Occam Reduction Example

```bash
occam /path/to/crash_repro.sql \
  --oracle "python3 harness/crash_oracle.py --host 127.0.0.1 --port 5432 --log-file /tmp/occam-pg-log/postgresql.log --expected-signature 'TRAP: FailedAssertion'" \
  --reset "./harness/reset_db.sh --dbms postgres --host 127.0.0.1 --port 5432 --container-name occam-target-pg" \
  --dialect postgres \
  --timeout 10 \
  --jobs 1 \
  --out /tmp/occam_reduced/
```

`--timeout 10` belongs to Occam. Do not also pass a smaller `--timeout` to `crash_oracle.py`; a nonzero oracle exit is a rejection, not a timeout.

---

## 8. Security

This harness executes untrusted SQL and is **not a sandbox**.

- PostgreSQL containers use `trust` authentication and a superuser role; bind published ports to `127.0.0.1`, not all interfaces.
- The oracle rejects line-leading psql backslash meta-commands by default, and passes `--binary-mode` to the MySQL client to disable `system`/`source`. `--allow-psql-meta` disables the psql protection.
- Server-side `COPY ... FROM PROGRAM` can still execute commands as the DBMS superuser. Use a non-superuser role and container/network isolation for untrusted corpora.
- MySQL passwords are passed through `MYSQL_PWD`, not `-p<password>` on argv. Prefer environment variables over `--password`.
- Do not run this against a database you care about.

---

## 9. Unit Tests

```bash
python3 -m unittest discover -s harness -p "test_*.py" -v
# or
make harness-check
```

The tests cover exit-code semantics, server-log-only crash evidence, log rotation/flush, decision logging, MySQL client hardening flags, `OCCAM_WORKDIR`, and container log mode. They do not need a built Occam binary or a real DBMS.

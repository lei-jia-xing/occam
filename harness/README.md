# Occam Crash Oracle & Isolation Harness

This directory provides the crash reproduction oracle, target DBMS container definitions, and candidate state isolation scripts for validating Occam on authentic PostgreSQL and MySQL multi-statement fuzz crashes.

---

## 1. Occam Protocol Contract

Occam communicates with external test harnesses using standard POSIX stream and exit-code semantics:

| Contract Component | Protocol Specification |
| :--- | :--- |
| **`--oracle <cmd>`** | Candidate SQL is piped into `sys.stdin` of `<cmd>`.<br>• **Exit 0**: Accepted / interesting (reproduces the target crash with matching signature).<br>• **Exit 1**: Rejected (normal query execution, standard SQL syntax/type error, or mismatched signature).<br>• **Exit 124**: Timeout (query exceeded bounded evaluation threshold). |
| **`--reset <cmd>`** | Executed before candidate evaluation when specified.<br>• Drops and recreates `occam_test` from `template_occam`.<br>• If server crashed during a prior candidate, detects the crash, restarts the server, and waits for connectivity (`pg_isready` / `mysqladmin ping`).<br>• **Exit 0**: State cleanly reset and target database ready.<br>• **Exit non-zero**: Infrastructure failure. |
| **`OCCAM_WORKDIR`** | Occam provides a clean candidate temporary directory in the environment (`OCCAM_WORKDIR`, `TMPDIR`) for candidate isolation. |

---

## 2. Target PostgreSQL Image (`Dockerfile.target-pg`)

A reproducible Dockerfile based on Ubuntu 22.04 that builds PostgreSQL from source with assertion checks (`--enable-cassert`) and debug symbols (`--enable-debug` with `CFLAGS="-O1 -g -fno-omit-frame-pointer"`).

### Building the Image

```bash
# Build default PostgreSQL 13 (REL_13_0)
docker build -t occam-target-pg -f harness/Dockerfile.target-pg .

# Or build a specific version or commit:
docker build -t occam-target-pg -f harness/Dockerfile.target-pg \
  --build-arg PG_VERSION=REL_12_0 .
```

### Running the Target Container

```bash
# Run container exposing port 5432 and mapping server logs to host
docker run -d --name occam-target-pg \
  -p 5432:5432 \
  -v /tmp/occam-pg-log:/var/log/postgresql \
  occam-target-pg

# Verify database cluster is ready
pg_isready -h 127.0.0.1 -p 5432
```

The container automatically configures:
- Unix sockets at `/var/run/postgresql` and `/tmp`.
- `logging_collector = on` logging to `/var/log/postgresql/postgresql.log` with `log_statement = 'all'` and `log_min_messages = 'warning'`.
- Template database `template_occam` and pre-created test database `occam_test`.

---

## 3. Crash Oracle (`crash_oracle.py`)

`harness/crash_oracle.py` connects to the target DBMS, runs candidate SQL from stdin, captures DBMS disconnects / backend crashes, inspects the server log for assertion failures or signals, and verifies the crash signature.

### Arguments

- `--dbms {postgres,mysql}`: Target DBMS (default: `postgres`, or `DBMS` env).
- `--host`: DBMS host or unix socket path (default: `127.0.0.1`, or `PGHOST` / `MYSQL_HOST`).
- `--port`: DBMS port (default: 5432 for PostgreSQL, 3306 for MySQL).
- `--user`: DBMS user (default: `postgres` or `root`).
- `--password`: DBMS password (default: empty, or `PGPASSWORD` / `MYSQL_PWD`).
- `--dbname`: Target test database (default: `occam_test`).
- `--expected-signature`: Substring or regex to match in the crash log (e.g. `TRAP: FailedAssertion` or `subselect.c:2883`).
- `--log-file`: Server log file path (e.g. `/var/log/postgresql/postgresql.log` or host mount).
- `--container-name`: Docker container name (e.g. `occam-target-pg`) for inspecting logs via docker if host path is unavailable.
- `--timeout`: Maximum query execution time in seconds (default: 10.0; exits 124 on timeout).
- `--verbose`: Prints acceptance/rejection diagnostics to stderr.

### Standalone Testing

```bash
# Test candidate against running PostgreSQL container
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

## 4. State Reset Harness (`reset_db.sh`)

`harness/reset_db.sh` restores `occam_test` from `template_occam` before each candidate execution.

- Terminates active connections to `occam_test`.
- Executes `DROP DATABASE IF EXISTS occam_test; CREATE DATABASE occam_test TEMPLATE template_occam;`.
- If the server died from a prior candidate crash, waits for PostgreSQL postmaster recovery or restarts the container/process.

### Usage

```bash
# Reset PostgreSQL database
./harness/reset_db.sh --host 127.0.0.1 --port 5432 --container-name occam-target-pg

# Reset MySQL database
./harness/reset_db.sh --dbms mysql --host 127.0.0.1 --port 3306
```

---

## 5. End-to-End Occam Reduction Example

Run Occam reducing a multi-statement fuzz crash:

```bash
occam /path/to/crash_repro.sql \
  --oracle "python3 harness/crash_oracle.py --host 127.0.0.1 --port 5432 --log-file /tmp/occam-pg-log/postgresql.log --expected-signature 'TRAP: FailedAssertion'" \
  --reset "./harness/reset_db.sh --host 127.0.0.1 --port 5432 --container-name occam-target-pg" \
  --dialect postgres \
  --timeout 10 \
  --out /tmp/occam_reduced/
```

---

## 6. Unit Tests

Unit tests verify oracle exit code semantics (exit 0 on matching crash, exit 1 on normal execution / SQL error / mismatched crash, exit 124 on timeout):

```bash
python3 harness/test_crash_oracle.py
```

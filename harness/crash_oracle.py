#!/usr/bin/env python3
"""
crash_oracle.py - Crash interestingness oracle for Occam multi-statement reduction.

Occam predicate contract:
  - Candidate SQL is piped to sys.stdin.
  - Exit 0: interesting (target DBMS crashed with a matching signature).
  - Any nonzero exit: rejected. Occam does not reserve exit codes for the
    predicate; in particular 124 is NOT treated as a timeout by Occam.

Timeout ownership:
  - Under Occam, leave --timeout at its default (0 = disabled) and let Occam's
    --timeout kill the whole process group. An internal timeout that fires
    first would turn an inconclusive candidate into a rejection.
  - For standalone use, pass --timeout explicitly.

Crash evidence:
  - A crash is accepted only from the DBMS server log (--log-file or
    --container-name). Client stderr is used only to corroborate connection
    loss / transport errors; it is never scanned for crash signatures because
    psql/mysql echo the failing statement in error messages.
  - Evidence extraction is scoped to the pinned server event formats:
      * PostgreSQL 13 (REL_13_0) stderr and CSV logs (postmaster LogChildExit,
        TRAP: FailedAssertion, PANIC, and SQLSTATE 57P02 corroboration).
      * MySQL 8.0 (8.0.20) error logs ([ERROR] [MY-xxxxxx], mysqld signal,
        assertions, and supervisor non-zero mysqld exit status).
  - Verdict classification distinguishes:
      * confirmed_fault: server evidence confirms an explicit DBMS fault
      * healthy_no_fault: server log observed cleanly with no matching fault
      * missing_evidence: server log unavailable, execution error, or unsafe mode

DBMS supported:
  - postgres (default, pinned to PostgreSQL 13 semantics)
  - mysql (pinned to MySQL 8.0 semantics)
"""

import argparse
import csv
import datetime
import hashlib
import io
import json
import os
import re
import shutil
import subprocess
import sys
import time

MAX_LOG_READ_BYTES = 8 * 1024 * 1024
MAX_TOTAL_LOG_BYTES = 32 * 1024 * 1024
LOG_WAIT_SECONDS = 2.0
LOG_POLL_INTERVAL = 0.05

# Client transport disconnect phrases / error codes. Used only for corroboration,
# never as standalone proof of a server crash.
PG_CONNECTION_LOST_PHRASES = [
    "server closed the connection unexpectedly",
    "connection to server was lost",
    "could not receive data from server",
]
PG_CRASH_SHUTDOWN_PHRASES = [
    "57P02",
    "terminating connection because of crash of another server process",
]
MYSQL_CONNECTION_LOST_PHRASES = [
    "Lost connection to MySQL server",
    "MySQL server has gone away",
    "ERROR 2006",
    "ERROR 2013",
]

# PostgreSQL 13 stderr prefix: "%m [%p] %q%u@%d " -> timestamp [pid] [user@db ]SEVERITY: msg
PG_STDERR_LINE_RE = re.compile(
    r"^(?:\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:\s+\S+)?\s+)?"
    r"(?:\[(?P<pid>\d+)\]\s+)?"
    r"(?:(?P<context>\S+@\S+)\s+)?"
    r"(?P<severity>TRAP|PANIC|FATAL|LOG|ERROR|WARNING|NOTICE|INFO|DEBUG\d*|STATEMENT|DETAIL|HINT|CONTEXT|QUERY):\s*"
    r"(?P<message>.*)$"
)
PG_TRAP_RE = re.compile(r"TRAP:\s*FailedAssertion\([^\n]+\)")
PG_CHILD_SIGNAL_RE = re.compile(
    r"server process \(PID (\d+)\) was terminated by signal (\d+)"
)
PG_CHILD_EXCEPTION_RE = re.compile(
    r"server process \(PID (\d+)\) was terminated by exception (0x[0-9a-fA-F]+)"
)
PG_CHILD_EXIT_NONZERO_RE = re.compile(
    r"server process \(PID (\d+)\) exited with exit code ([1-9]\d*)"
)
PG_CRASH_RECOVERY_RE = re.compile(r"terminating any other active server processes")
PG_NON_FAULT_LOG_MSG_RE = re.compile(r"^(?:statement|duration|execute\s+\S+|parse\s+\S+):")

# MySQL 8.0 error log format:
#   2026-11-22T12:00:00.123456Z 0 [ERROR] [MY-013183] [InnoDB] Assertion failure: ...
MYSQL_80_LINE_RE = re.compile(
    r"^(?P<ts>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z)\s+"
    r"(?P<thread_id>\d+)\s+"
    r"\[(?P<level>[A-Za-z]+)\]\s*"
    r"(?:\[(?P<code>MY-\d{6})\]\s*)?"
    r"(?:\[(?P<subsystem>[^\]]+)\]\s*)?"
    r"(?P<message>.*)$"
)
MYSQL_SUPERVISOR_EXIT_RE = re.compile(
    r"mysqld(?:-debug)?\s+terminated\s+\(exit status:\s*(\d+)\)"
)
MYSQL_SIGNAL_RE = re.compile(
    r"(?:mysqld got signal (\d+)|(?:Fatal|Got) signal (\d+))"
)
MYSQL_ASSERTION_RE = re.compile(
    r"(?:Assertion failure:\s+[^\n]+|Assertion `[^']+' failed)"
)
MYSQL_ABORTED_CONNECTION_RE = re.compile(r"Aborted connection \d+")
ASAN_RE = re.compile(r"AddressSanitizer:\s+[^\n]+")


def env_flag(name):
    return os.environ.get(name, "").strip().lower() in {"1", "true", "yes", "on"}


def resolve_binary(name, candidate_paths=None):
    found = shutil.which(name)
    if found:
        return found
    if candidate_paths:
        for path in candidate_paths:
            if os.path.isfile(path) and os.access(path, os.X_OK):
                return path
    return name


def emit_decision(path, payload):
    if not path:
        return
    try:
        with open(path, "a", encoding="utf-8") as handle:
            handle.write(json.dumps(payload, sort_keys=True) + "\n")
    except OSError as exc:
        sys.stderr.write(f"[crash_oracle] Warning: failed to write {path}: {exc}\n")


def wait_for_readable_file(path, timeout):
    deadline = time.monotonic() + timeout
    while True:
        try:
            with open(path, "rb"):
                return True
        except OSError:
            if time.monotonic() >= deadline:
                return False
            time.sleep(LOG_POLL_INTERVAL)


def read_new_log_file(path, offset, inode):
    """Read bytes appended since offset. Detects truncation/inode replacement."""
    try:
        stat = os.stat(path)
    except OSError:
        return "", offset, inode, False

    if (inode is not None and stat.st_ino != inode) or stat.st_size < offset:
        offset = 0
    inode = stat.st_ino
    size = stat.st_size
    if size <= offset:
        return "", offset, inode, True

    start = max(offset, size - MAX_LOG_READ_BYTES)
    try:
        with open(path, "rb") as handle:
            handle.seek(start)
            data = handle.read(size - start)
    except OSError:
        return "", offset, inode, False
    return data.decode("utf-8", errors="replace"), size, inode, True


def get_container_log_state(container_name, internal_log):
    try:
        result = subprocess.run(
            ["docker", "exec", container_name, "stat", "-c", "%s %i", internal_log],
            check=False,
            capture_output=True,
            text=True,
            timeout=3,
        )
        if result.returncode == 0:
            parts = result.stdout.split()
            if len(parts) == 2 and parts[0].isdigit() and parts[1].isdigit():
                return int(parts[0]), int(parts[1])
    except Exception:
        pass
    return None


def read_container_file_log(container_name, internal_log, offset, inode):
    state = get_container_log_state(container_name, internal_log)
    if state is None:
        return "", offset, inode, False
    size, current_inode = state

    if (inode is not None and current_inode != inode) or size < offset:
        offset = 0
    inode = current_inode
    if size <= offset:
        return "", offset, inode, True

    start = max(offset, size - MAX_LOG_READ_BYTES)
    try:
        result = subprocess.run(
            [
                "docker",
                "exec",
                container_name,
                "tail",
                "-c",
                f"+{start + 1}",
                internal_log,
            ],
            check=False,
            capture_output=True,
            text=True,
            timeout=5,
        )
    except Exception:
        return "", offset, inode, False
    if result.returncode != 0:
        return "", offset, inode, False
    return result.stdout or "", size, inode, True


def read_container_stdout_logs(container_name, since_iso):
    try:
        result = subprocess.run(
            ["docker", "logs", "--since", since_iso, container_name],
            check=False,
            capture_output=True,
            text=True,
            timeout=5,
        )
        return (result.stdout or "") + "\n" + (result.stderr or "")
    except Exception as exc:
        sys.stderr.write(f"[crash_oracle] Warning: docker logs failed: {exc}\n")
        return ""


PG_CSV_START_RE = re.compile(r"^\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}")


def _parse_pg_csv_row(record_text):
    """Return (pid, severity, sqlstate, message) if record_text is a PG 13 CSV log row."""
    if not record_text or "," not in record_text:
        return None
    # PG 13 CSV logs have 23 columns (26 in PG 14+); column 3 is process_id,
    # column 11 is error_severity, column 12 is sql_state_code, column 13 is message.
    try:
        row = next(csv.reader(io.StringIO(record_text)))
    except (csv.Error, StopIteration):
        return None
    if len(row) < 14:
        return None
    pid_raw = row[3].strip()
    severity = row[11].strip().upper()
    sqlstate = row[12].strip().upper()
    message = row[13]
    if not pid_raw.isdigit():
        return None
    if severity not in {
        "LOG",
        "FATAL",
        "PANIC",
        "ERROR",
        "WARNING",
        "NOTICE",
        "INFO",
        "DEBUG1",
        "DEBUG2",
        "DEBUG3",
        "DEBUG4",
        "DEBUG5",
        "TRAP",
    }:
        return None
    return int(pid_raw), severity, sqlstate, message


def extract_postgres_evidence(server_log_text, stderr_text):
    """
    Extract PostgreSQL 13 (REL_13_0) server-side fault events and corroboration.
    Excludes SQL statement echoes (`LOG: statement:`, `STATEMENT:`, `ERROR:`),
    tab-indented continuation lines of excluded entries, and multi-line CSV
    statement fields so user queries cannot spoof server crash signatures.
    """
    faults = []
    candidate_lines = []
    backend_pids = set()
    postmaster_child_pids = set()
    sqlstate_57p02 = any(phrase in stderr_text for phrase in PG_CRASH_SHUTDOWN_PHRASES)

    raw_lines = server_log_text.splitlines()
    idx = 0
    in_excluded_entry = False

    while idx < len(raw_lines):
        raw_line = raw_lines[idx]
        idx += 1
        if not raw_line.strip():
            continue

        # Handle multi-line quoted CSV records (e.g. multi-line SQL in LOG: statement:).
        csv_buf = raw_line
        if PG_CSV_START_RE.match(raw_line) and "," in raw_line:
            while csv_buf.count('"') % 2 == 1 and idx < len(raw_lines):
                csv_buf += "\n" + raw_lines[idx]
                idx += 1

        csv_parsed = _parse_pg_csv_row(csv_buf)
        if csv_parsed is not None:
            in_excluded_entry = False
            pid, severity, sqlstate, message = csv_parsed
            if sqlstate == "57P02":
                sqlstate_57p02 = True
            if severity == "LOG" and PG_NON_FAULT_LOG_MSG_RE.match(message):
                continue
            if severity in {"ERROR", "WARNING", "NOTICE", "INFO"} or severity.startswith("DEBUG"):
                continue
            line_text = f"{severity}: {message}"
        else:
            # PostgreSQL elog.c indents continuation lines of a log entry with a tab.
            if raw_line.startswith("\t"):
                if in_excluded_entry:
                    continue
                candidate_lines.append(raw_line.strip())
                continue

            line = raw_line.strip()
            m = PG_STDERR_LINE_RE.match(line)
            if m:
                pid = int(m.group("pid")) if m.group("pid") else None
                severity = m.group("severity")
                message = m.group("message")
                if severity == "LOG" and PG_NON_FAULT_LOG_MSG_RE.match(message):
                    in_excluded_entry = True
                    continue
                if severity in {
                    "STATEMENT",
                    "DETAIL",
                    "HINT",
                    "CONTEXT",
                    "QUERY",
                    "ERROR",
                    "WARNING",
                    "NOTICE",
                    "INFO",
                } or severity.startswith("DEBUG"):
                    in_excluded_entry = True
                    continue
                in_excluded_entry = False
                if severity == "FATAL" and (
                    "57P02" in message
                    or "terminating connection because of crash of another server process"
                    in message
                ):
                    sqlstate_57p02 = True
                line_text = f"{severity}: {message}"
            else:
                in_excluded_entry = False
                pid = None
                severity = ""
                message = line
                line_text = line

        candidate_lines.append(line_text)

        if PG_TRAP_RE.search(line_text):
            if pid is not None:
                backend_pids.add(pid)
            faults.append(
                {
                    "kind": "pg_assertion_trap",
                    "pid": pid,
                    "detail": line_text,
                }
            )
            continue

        if severity == "PANIC" or line_text.startswith("PANIC:"):
            if pid is not None:
                backend_pids.add(pid)
            faults.append(
                {
                    "kind": "pg_backend_panic",
                    "pid": pid,
                    "detail": line_text,
                }
            )
            continue

        m_sig = PG_CHILD_SIGNAL_RE.search(message)
        if m_sig:
            child_pid = int(m_sig.group(1))
            sig = int(m_sig.group(2))
            postmaster_child_pids.add(child_pid)
            faults.append(
                {
                    "kind": "pg_postmaster_child_signal",
                    "child_pid": child_pid,
                    "signal": sig,
                    "detail": line_text,
                }
            )
            continue

        m_exc = PG_CHILD_EXCEPTION_RE.search(message)
        if m_exc:
            child_pid = int(m_exc.group(1))
            postmaster_child_pids.add(child_pid)
            faults.append(
                {
                    "kind": "pg_postmaster_child_exception",
                    "child_pid": child_pid,
                    "exception": m_exc.group(2),
                    "detail": line_text,
                }
            )
            continue

        m_exit = PG_CHILD_EXIT_NONZERO_RE.search(message)
        if m_exit:
            child_pid = int(m_exit.group(1))
            postmaster_child_pids.add(child_pid)
            faults.append(
                {
                    "kind": "pg_postmaster_child_exit",
                    "child_pid": child_pid,
                    "exit_code": int(m_exit.group(2)),
                    "detail": line_text,
                }
            )
            continue

        if PG_CRASH_RECOVERY_RE.search(message):
            faults.append(
                {
                    "kind": "pg_postmaster_crash_recovery",
                    "detail": line_text,
                }
            )
            continue

        if ASAN_RE.search(line_text):
            faults.append(
                {
                    "kind": "pg_asan_fault",
                    "pid": pid,
                    "detail": line_text,
                }
            )

    corroboration = {
        "sqlstate_57p02": sqlstate_57p02,
        "connection_lost": any(p in stderr_text for p in PG_CONNECTION_LOST_PHRASES),
        "backend_pids": sorted(backend_pids),
        "postmaster_child_pids": sorted(postmaster_child_pids),
    }
    return faults, candidate_lines, corroboration


def extract_mysql_evidence(server_log_text, stderr_text):
    """
    Extract MySQL 8.0 (8.0.20) server error log fault events and supervisor exits.
    Excludes [Note]/[Warning] lines, Aborted connection warnings, normal exit
    status 0, and client-only transport errors (2006/2013).
    """
    faults = []
    candidate_lines = []
    supervisor_exits = []

    for raw_line in server_log_text.splitlines():
        line = raw_line.strip()
        if not line:
            continue

        m_sup = MYSQL_SUPERVISOR_EXIT_RE.search(line)
        if m_sup:
            status = int(m_sup.group(1))
            supervisor_exits.append(status)
            if status != 0:
                candidate_lines.append(line)
                faults.append(
                    {
                        "kind": "mysql_supervisor_abnormal_exit",
                        "exit_status": status,
                        "detail": line,
                    }
                )
            continue

        m_log = MYSQL_80_LINE_RE.match(line)
        if m_log:
            level = m_log.group("level")
            event_code = m_log.group("code")
            subsystem = m_log.group("subsystem")
            message = m_log.group("message")
            if level in {"Note", "Warning"}:
                continue
            if MYSQL_ABORTED_CONNECTION_RE.search(message) or event_code in {
                "MY-010914",
                "MY-010926",
            }:
                continue
        else:
            level = ""
            event_code = None
            subsystem = None
            message = line
            if MYSQL_ABORTED_CONNECTION_RE.search(line):
                continue

        candidate_lines.append(line)

        m_sig = MYSQL_SIGNAL_RE.search(line)
        if m_sig:
            sig = int(m_sig.group(1) or m_sig.group(2))
            faults.append(
                {
                    "kind": "mysql_server_signal",
                    "signal": sig,
                    "event_code": event_code,
                    "subsystem": subsystem,
                    "detail": line,
                }
            )
            continue

        if MYSQL_ASSERTION_RE.search(message):
            faults.append(
                {
                    "kind": "mysql_assertion_failure",
                    "event_code": event_code,
                    "subsystem": subsystem,
                    "detail": line,
                }
            )
            continue

        if ASAN_RE.search(line):
            faults.append(
                {
                    "kind": "mysql_asan_fault",
                    "event_code": event_code,
                    "detail": line,
                }
            )
            continue

        # In MySQL 8.0, [ERROR] with an explicit [MY-xxxxxx] server error code from
        # fatal crash reporting (e.g. [MY-013183] InnoDB assertion) is captured above;
        # ordinary SQL/plugin [ERROR] entries are not crashes without signal/assertion/exit.
        _ = level

    corroboration = {
        "connection_lost": any(p in stderr_text for p in MYSQL_CONNECTION_LOST_PHRASES),
        "supervisor_exit_codes": supervisor_exits,
    }
    return faults, candidate_lines, corroboration


def extract_dbms_evidence(dbms, server_log_text, stderr_text):
    if dbms == "mysql":
        return extract_mysql_evidence(server_log_text, stderr_text)
    return extract_postgres_evidence(server_log_text, stderr_text)


def signature_matches(text, signature):
    """Return (matched, regex_valid). Invalid regexes fall back to literal."""
    try:
        return bool(re.search(signature, text, re.IGNORECASE | re.DOTALL)), True
    except re.error:
        return signature.lower() in text.lower(), False


def has_psql_meta_command(sql_bytes):
    text = sql_bytes.decode("utf-8", errors="replace")
    return any(line.lstrip().startswith("\\") for line in text.splitlines())


def main():
    parser = argparse.ArgumentParser(
        description="Occam crash oracle: executes SQL from stdin and validates a DBMS crash."
    )
    parser.add_argument(
        "--dbms",
        choices=["postgres", "mysql"],
        default=os.environ.get("DBMS", "postgres"),
        help="Target database system (default: postgres)",
    )
    parser.add_argument(
        "--host",
        default=None,
        help="Database host or unix socket directory (default: 127.0.0.1)",
    )
    parser.add_argument(
        "--port",
        type=int,
        default=None,
        help="Database port (default: 5432 for postgres, 3306 for mysql)",
    )
    parser.add_argument(
        "--socket",
        default=os.environ.get("MYSQL_SOCKET", ""),
        help="MySQL unix socket path (overrides --host/--port)",
    )
    parser.add_argument(
        "--user",
        default=None,
        help="Database user (default: postgres / root)",
    )
    parser.add_argument(
        "--password",
        default=None,
        help="Database password (prefer PGPASSWORD / MYSQL_PWD environment variables)",
    )
    parser.add_argument(
        "--dbname",
        default=None,
        help="Target database name (default: occam_test)",
    )
    parser.add_argument(
        "--expected-signature",
        default=os.environ.get("EXPECTED_SIGNATURE", ""),
        help="Regex or literal substring of the expected crash signature",
    )
    parser.add_argument(
        "--log-file",
        default=None,
        help="Host path to the DBMS server log (required unless --container-name)",
    )
    parser.add_argument(
        "--container-name",
        default=os.environ.get("CONTAINER_NAME", ""),
        help="Docker container name used to read logs when --log-file is unavailable",
    )
    parser.add_argument(
        "--container-log",
        default="",
        help="Server log path inside the container (defaults per DBMS)",
    )
    parser.add_argument(
        "--decision-log",
        default=os.environ.get("ORACLE_DECISION_LOG", ""),
        help="Append one JSON verdict record per invocation to this file",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=float(os.environ.get("ORACLE_TIMEOUT", "0")),
        help=(
            "Optional client-side timeout in seconds (default: 0 = disabled). "
            "Under Occam leave this disabled and use Occam's --timeout instead."
        ),
    )
    parser.add_argument(
        "--allow-stderr-only",
        action="store_true",
        default=env_flag("ORACLE_ALLOW_STDERR_ONLY"),
        help="UNSAFE: accept connection loss from stderr when no server log is configured",
    )
    parser.add_argument(
        "--allow-psql-meta",
        action="store_true",
        default=env_flag("ORACLE_ALLOW_PSQL_META"),
        help="UNSAFE: allow psql backslash meta-commands in candidate SQL",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        default=env_flag("ORACLE_VERBOSE"),
        help="Print verbose diagnostics to stderr",
    )

    args = parser.parse_args()
    if args.timeout < 0:
        parser.error("--timeout must be non-negative")

    # Resolve DBMS-aware connection defaults.
    if args.host is None:
        args.host = (
            os.environ.get("MYSQL_HOST", "127.0.0.1")
            if args.dbms == "mysql"
            else os.environ.get("PGHOST", "127.0.0.1")
        )
    if args.port is None:
        args.port = (
            int(os.environ.get("MYSQL_TCP_PORT", 3306))
            if args.dbms == "mysql"
            else int(os.environ.get("PGPORT", 5432))
        )
    if args.user is None:
        args.user = (
            os.environ.get("MYSQL_USER", "root")
            if args.dbms == "mysql"
            else os.environ.get("PGUSER", "postgres")
        )
    if args.password is None:
        args.password = (
            os.environ.get("MYSQL_PWD", "")
            if args.dbms == "mysql"
            else os.environ.get("PGPASSWORD", "")
        )
    if args.dbname is None:
        args.dbname = (
            os.environ.get("MYSQL_DATABASE", "occam_test")
            if args.dbms == "mysql"
            else os.environ.get("PGDATABASE", "occam_test")
        )

    log_file = os.path.expanduser(args.log_file) if args.log_file else ""
    if log_file:
        log_file = os.path.abspath(log_file)
    decision_log = os.path.expanduser(args.decision_log) if args.decision_log else ""
    if decision_log:
        decision_log = os.path.abspath(decision_log)
    workdir = os.environ.get("OCCAM_WORKDIR", "")
    if workdir and not os.path.isdir(workdir):
        workdir = ""

    candidate_sql = sys.stdin.buffer.read()
    candidate_sha = hashlib.sha256(candidate_sql).hexdigest()[:16]
    base_payload = {
        "ts": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "dbms": args.dbms,
        "candidate_sha256_16": candidate_sha,
        "candidate_bytes": len(candidate_sql),
        "expected_signature": args.expected_signature,
        "workdir": workdir or None,
    }

    def finish(code, verdict, classification, reason, **extra):
        payload = dict(base_payload)
        payload.update(
            {
                "verdict": verdict,
                "classification": classification,
                "reason": reason,
            }
        )
        payload.update(extra)
        emit_decision(decision_log, payload)
        if args.verbose:
            sys.stderr.write(
                f"[crash_oracle] {verdict.upper()} [{classification}]: {reason}"
                + (f" ({extra})" if extra else "")
                + "\n"
            )
        sys.exit(code)

    if not candidate_sql.strip():
        finish(1, "rejected", "healthy_no_fault", "empty-sql")

    if args.dbms == "postgres" and not args.allow_psql_meta:
        if has_psql_meta_command(candidate_sql):
            finish(1, "rejected", "healthy_no_fault", "psql-meta-command")

    # Build client command.
    env = os.environ.copy()
    if args.dbms == "postgres":
        psql_candidates = [
            os.path.expanduser("~/.cache/occam/postgresql-17.0-debug/install/bin/psql"),
            "/usr/local/pgsql/bin/psql",
            "/usr/lib/postgresql/13/bin/psql",
            "/usr/lib/postgresql/12/bin/psql",
            "/usr/lib/postgresql/14/bin/psql",
            "/usr/lib/postgresql/15/bin/psql",
            "/usr/lib/postgresql/16/bin/psql",
            "/usr/lib/postgresql/17/bin/psql",
            "/usr/bin/psql",
        ]
        binary = resolve_binary("psql", psql_candidates)
        cmd = [
            binary,
            "-h",
            args.host,
            "-p",
            str(args.port),
            "-U",
            args.user,
            "-d",
            args.dbname,
            "-v",
            "ON_ERROR_STOP=0",
            "-X",
            "-q",
        ]
        if args.password:
            env["PGPASSWORD"] = args.password
    else:
        mysql_candidates = [
            "/usr/local/mysql/bin/mysql",
            "/usr/bin/mysql",
        ]
        binary = resolve_binary("mysql", mysql_candidates)
        cmd = [binary, "--no-defaults", "--binary-mode"]
        if args.socket:
            cmd.extend(["--socket", args.socket])
        else:
            cmd.extend(["-h", args.host, "-P", str(args.port)])
        cmd.extend(["-u", args.user, "-D", args.dbname, "--force", "-s"])
        if args.password:
            env["MYSQL_PWD"] = args.password

    # Resolve the log source before executing anything. Silent fallback to
    # stderr would turn infrastructure problems into false verdicts.
    log_mode = ""
    log_offset = 0
    log_inode = None
    internal_log = args.container_log or (
        "/var/log/mysql/error.log"
        if args.dbms == "mysql"
        else "/var/log/postgresql/postgresql.log"
    )

    if log_file:
        if wait_for_readable_file(log_file, LOG_WAIT_SECONDS):
            log_mode = "file"
            try:
                stat = os.stat(log_file)
                log_offset = stat.st_size
                log_inode = stat.st_ino
            except OSError:
                finish(1, "rejected", "missing_evidence", "log-stat-failed", log_file=log_file)
        elif args.container_name:
            log_mode = "container"
        else:
            finish(
                1,
                "rejected",
                "missing_evidence",
                "log-unreadable",
                log_file=log_file,
                hint="mount the log readably or pass --container-name",
            )
    elif args.container_name:
        log_mode = "container"
    elif args.allow_stderr_only:
        log_mode = "stderr"
    else:
        finish(
            1,
            "rejected",
            "missing_evidence",
            "log-source-required",
            hint="pass --log-file or --container-name (or --allow-stderr-only)",
        )

    since_iso = datetime.datetime.now(datetime.timezone.utc).isoformat()
    if log_mode == "container":
        state = get_container_log_state(args.container_name, internal_log)
        if state is None:
            finish(
                1,
                "rejected",
                "missing_evidence",
                "container-log-unavailable",
                container=args.container_name,
                container_log=internal_log,
            )
        log_offset, log_inode = state

    # Execute the candidate. No start_new_session: psql/mysql must stay in
    # Occam's process group so a timeout/cancel reaches the whole tree.
    stderr_text = ""
    retcode = 0
    try:
        process = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            env=env,
            cwd=workdir or None,
        )
        if args.timeout > 0:
            _, stderr_bytes = process.communicate(
                input=candidate_sql, timeout=args.timeout
            )
        else:
            _, stderr_bytes = process.communicate(input=candidate_sql)
        retcode = process.returncode
        stderr_text = (stderr_bytes or b"").decode("utf-8", errors="replace")
    except subprocess.TimeoutExpired:
        try:
            process.kill()
        except OSError:
            pass
        try:
            process.communicate(timeout=1.0)
        except Exception:
            pass
        finish(124, "timeout", "missing_evidence", "client-timeout", timeout=args.timeout)
    except FileNotFoundError:
        finish(1, "rejected", "missing_evidence", "client-not-found", binary=cmd[0])
    except OSError as exc:
        finish(1, "rejected", "missing_evidence", "execution-error", error=str(exc))

    conn_phrases = (
        MYSQL_CONNECTION_LOST_PHRASES if args.dbms == "mysql" else PG_CONNECTION_LOST_PHRASES
    )
    connection_lost = any(phrase in stderr_text for phrase in conn_phrases)
    possible_crash = connection_lost or retcode != 0

    # Collect only new server-log bytes. Poll briefly when a crash is plausible
    # because logging collectors flush asynchronously.
    file_log_text = ""
    stdout_log_text = ""
    server_log_text = ""
    log_read_ok = True
    deadline = time.monotonic() + (LOG_WAIT_SECONDS if possible_crash else 0.0)

    def collect_logs():
        nonlocal log_offset, log_inode, file_log_text, stdout_log_text
        if log_mode == "file":
            chunk, log_offset, log_inode, ok = read_new_log_file(
                log_file, log_offset, log_inode
            )
            if chunk:
                file_log_text = (file_log_text + chunk)[-MAX_TOTAL_LOG_BYTES:]
            return file_log_text, ok
        if log_mode == "container":
            chunk, log_offset, log_inode, ok = read_container_file_log(
                args.container_name, internal_log, log_offset, log_inode
            )
            if chunk:
                file_log_text = (file_log_text + chunk)[-MAX_TOTAL_LOG_BYTES:]
            stdout_log_text = read_container_stdout_logs(args.container_name, since_iso)
            combined = file_log_text
            if stdout_log_text:
                combined = (combined + "\n" + stdout_log_text) if combined else stdout_log_text
            return combined[-MAX_TOTAL_LOG_BYTES:], ok
        return "", True

    if log_mode in ("file", "container"):
        while True:
            server_log_text, ok = collect_logs()
            if not ok:
                log_read_ok = False
                break
            faults, candidate_lines, _ = extract_dbms_evidence(
                args.dbms, server_log_text, stderr_text
            )
            if args.expected_signature:
                if faults:
                    scope_text = "\n".join([f["detail"] for f in faults] + candidate_lines)
                    matched, _ = signature_matches(scope_text, args.expected_signature)
                    if matched:
                        break
            elif faults:
                break
            if time.monotonic() >= deadline:
                break
            time.sleep(LOG_POLL_INTERVAL)

        if not log_read_ok and not server_log_text:
            finish(
                1,
                "rejected",
                "missing_evidence",
                "log-read-failed",
                log_mode=log_mode,
                log_file=log_file or internal_log,
            )

    if log_mode == "stderr":
        stderr_faults, _, _ = extract_dbms_evidence(args.dbms, stderr_text, stderr_text)
        if args.expected_signature:
            matched, regex_valid = signature_matches(stderr_text, args.expected_signature)
            if matched:
                finish(
                    0,
                    "accepted",
                    "missing_evidence",
                    "stderr-signature-unsafe",
                    regex_valid=regex_valid,
                )
            finish(
                1,
                "rejected",
                "missing_evidence",
                "stderr-signature-mismatch",
                regex_valid=regex_valid,
            )
        if connection_lost or stderr_faults:
            finish(
                0,
                "accepted",
                "missing_evidence",
                "stderr-crash-unsafe",
                fault_events=stderr_faults,
                connection_lost=connection_lost,
            )
        finish(
            1,
            "rejected",
            "missing_evidence",
            "no-crash",
            connection_lost=connection_lost,
        )

    faults, candidate_lines, corroboration = extract_dbms_evidence(
        args.dbms, server_log_text, stderr_text
    )
    indicators = [f["detail"] for f in faults]
    signature_found = False
    regex_valid = True
    if args.expected_signature:
        # Require at least one explicit server fault event before classifying as
        # confirmed_fault; when a fault exists, allow matching its detail or
        # accompanying non-echo server log lines (e.g. crash backtrace frames).
        if faults:
            scope_text = "\n".join(indicators + candidate_lines)
            signature_found, regex_valid = signature_matches(scope_text, args.expected_signature)
        else:
            _, regex_valid = signature_matches("", args.expected_signature)

    if args.expected_signature:
        if faults and signature_found:
            finish(
                0,
                "accepted",
                "confirmed_fault",
                "crash-signature",
                indicators=indicators,
                fault_events=faults,
                corroboration=corroboration,
                connection_lost=connection_lost,
                regex_valid=regex_valid,
            )
        finish(
            1,
            "rejected",
            "healthy_no_fault",
            "signature-mismatch" if faults else "no-crash",
            indicators=indicators,
            fault_events=faults,
            corroboration=corroboration,
            connection_lost=connection_lost,
            retcode=retcode,
            regex_valid=regex_valid,
        )

    if faults:
        finish(
            0,
            "accepted",
            "confirmed_fault",
            "crash-indicator",
            indicators=indicators,
            fault_events=faults,
            corroboration=corroboration,
            connection_lost=connection_lost,
            retcode=retcode,
        )

    if connection_lost and args.allow_stderr_only:
        finish(
            0,
            "accepted",
            "missing_evidence",
            "connection-loss-stderr-unsafe",
            connection_lost=True,
            retcode=retcode,
        )

    finish(
        1,
        "rejected",
        "healthy_no_fault",
        "no-crash",
        corroboration=corroboration,
        connection_lost=connection_lost,
        retcode=retcode,
        log_mode=log_mode,
    )


if __name__ == "__main__":
    main()

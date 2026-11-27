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
  - Server DBMS targets (postgres, mysql, mariadb, clickhouse):
      A crash is accepted only from the DBMS server log (--log-file or
      --container-name). Client stderr is used only to corroborate connection
      loss / transport errors; it is never scanned for crash signatures because
      clients echo the failing statement in error messages.
  - Embedded DBMS targets (sqlite, duckdb):
      Every candidate runs in an isolated fresh process against :memory:
      (either `docker exec -i <container> occam-exec` or local CLI binary).
      Verdicts combine process termination signals with sanitized stderr
      fault evidence (assertions, ASan/UBSan, DuckDB INTERNAL/FATAL errors),
      filtering out SQL error echoes and distinguishing OOM / SQLITE_ABORT.
  - Verdict classification distinguishes:
      * confirmed_fault: DBMS evidence confirms an explicit fault
      * healthy_no_fault: execution/logs observed cleanly with no matching fault
      * missing_evidence: server log unavailable, container/exec failure, or unsafe mode
  - RQ3 structured crash signatures:
      Normalized signatures (assertion identity, ASan/UBSan error kind + location,
      or signal + top-N non-wrapper stack frames) are emitted to --decision-log
      for deduplication and bug-slippage tracking (--baseline-signature /
      --reject-on-slippage).

DBMS supported:
  - postgres (default, pinned to PostgreSQL 13 semantics)
  - mysql (pinned to MySQL 8.0 semantics)
  - mariadb (MariaDB 10.5 debug / InnoDB / supervisor semantics)
  - clickhouse (ClickHouse server fatal/BaseDaemon/supervisor semantics)
  - sqlite (embedded SQLite via occam-exec or sqlite3 -batch :memory:)
  - duckdb (embedded DuckDB via occam-exec or duckdb -batch :memory:)
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

_HARNESS_DIR = os.path.dirname(os.path.abspath(__file__))
if _HARNESS_DIR not in sys.path:
    sys.path.insert(0, _HARNESS_DIR)

try:
    from dhall_config import (  # noqa: E402
        EMBEDDED_DBMS,
        ORACLE_DEFAULTS,
        SERVER_DBMS,
        SUPPORTED_DBMS,
    )
except (ImportError, FileNotFoundError):
    EMBEDDED_DBMS = {"sqlite", "duckdb"}
    SERVER_DBMS = {"postgres", "mysql", "mariadb", "clickhouse"}
    SUPPORTED_DBMS = ("postgres", "mysql", "mariadb", "clickhouse", "sqlite", "duckdb")
    ORACLE_DEFAULTS = {
        "timeout_secs": 0.0,
        "stack_frames": 5,
        "reject_on_slippage": False,
        "allow_stderr_only": False,
        "allow_psql_meta": False,
        "verbose": False,
    }

MAX_LOG_READ_BYTES = 8 * 1024 * 1024
MAX_TOTAL_LOG_BYTES = 32 * 1024 * 1024
LOG_WAIT_SECONDS = 2.0
LOG_POLL_INTERVAL = 0.05

SIGNAL_NAMES = {
    4: "SIGILL",
    5: "SIGTRAP",
    6: "SIGABRT",
    7: "SIGBUS",
    8: "SIGFPE",
    9: "SIGKILL",
    11: "SIGSEGV",
    13: "SIGPIPE",
    15: "SIGTERM",
    16: "SIGSTKFLT",
    31: "SIGSYS",
}
CRASH_SIGNALS = {4, 5, 6, 7, 8, 11, 16, 31}
EXTERNAL_OR_OOM_SIGNALS = {9, 15}

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
MARIADB_CONNECTION_LOST_PHRASES = MYSQL_CONNECTION_LOST_PHRASES + [
    "Lost connection to server",
    "Server has gone away",
]
CLICKHOUSE_CONNECTION_LOST_PHRASES = [
    "Code: 210",
    "Code: 32",
    "Connection reset by peer",
    "Attempt to read after eof",
    "Unexpected end of file",
    "Cannot read from socket",
    "Connection refused",
    "DB::NetException",
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
PG_TRAP_STRUCT_RE = re.compile(
    r'TRAP:\s*FailedAssertion\("(?P<expr>.+?)",\s*File:\s*"(?P<file>[^"]+)",\s*Line:\s*(?P<line>\d+)(?:,\s*PID:\s*\d+)?\)'
)
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

# MySQL 8.0 / MariaDB error log formats:
#   MySQL 8.0: 2026-11-22T12:00:00.123456Z 0 [ERROR] [MY-013183] [InnoDB] Assertion failure: ...
#   MariaDB:   2026-11-22 12:00:00 0 [ERROR] [FATAL] InnoDB: Assertion failure in file ... line ...
#   MariaDB2:  261122 12:00:00 [ERROR] mysqld got signal 11 ;
MYSQL_80_LINE_RE = re.compile(
    r"^(?P<ts>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z)\s+"
    r"(?P<thread_id>\d+)\s+"
    r"\[(?P<level>[A-Za-z]+)\]\s*"
    r"(?:\[(?P<code>MY-\d{6})\]\s*)?"
    r"(?:\[(?P<subsystem>[^\]]+)\]\s*)?"
    r"(?P<message>.*)$"
)
MARIADB_LINE_RE = re.compile(
    r"^(?P<ts>(?:\d{4}-\d{2}-\d{2}|\d{6})(?:T|\s+)\d{2}:\d{2}:\d{2}(?:\.\d+)?Z?)\s+"
    r"(?:(?P<thread_id>(?:\d+|0x[0-9a-fA-F]+))\s+)?"
    r"\[(?P<level>[A-Za-z]+)\]\s*"
    r"(?:\[(?P<code>MY-\d{6})\]\s*)?"
    r"(?:\[(?P<sublevel>FATAL|ERROR|Note|Warning)\]\s*)?"
    r"(?:\[(?P<subsystem>[^\]]+)\]\s*)?"
    r"(?P<message>.*)$"
)
MYSQL_SUPERVISOR_EXIT_RE = re.compile(
    r"(?:mysqld|mariadbd)(?:-debug)?\s+terminated\s+\(exit status:\s*(\d+)\)"
)
MYSQL_SIGNAL_RE = re.compile(
    r"(?:(?:mysqld|mariadbd)\s+got\s+signal\s+(\d+)|(?:Fatal|Got)\s+signal\s+(\d+))"
)
MYSQL_ASSERTION_RE = re.compile(
    r"(?:Assertion failure(?::|\s+in\s+(?:thread\s+\S+\s+in\s+)?file)\s+[^\n]+|"
    r"Failing assertion:\s+[^\n]+|"
    r"Assertion `[^']+' failed\.?)"
)
MYSQL_ABORTED_CONNECTION_RE = re.compile(r"Aborted connection \d+")
MYSQL_QUERY_ECHO_RE = re.compile(r"^Query\s*\([0-9a-fA-Fx]+\):")

# ClickHouse server error log format:
#   2026-11-22 12:00:00.123456 [ 25819 ] {qid} <Fatal> BaseDaemon: ...
CH_LOG_LINE_RE = re.compile(
    r"^(?P<ts>\d{4}[.-]\d{2}[.-]\d{2}[T\s]\d{2}:\d{2}:\d{2}(?:\.\d+)?Z?)\s+"
    r"\[\s*(?P<thread_id>\d+)\s*\]\s+"
    r"\{(?P<query_id>[^}]*)\}\s+"
    r"<(?P<level>[A-Za-z]+)>\s*"
    r"(?P<component>[^:]*):\s*(?P<message>.*)$"
)
CH_SUPERVISOR_EXIT_RE = re.compile(
    r"clickhouse-server\s+terminated\s+\(exit status:\s*(\d+)\)"
)
CH_SIGNAL_RE = re.compile(
    r"Received signal\s+(?:(?P<desc>[A-Za-z0-9 _-]+?)\s*\((?P<paren_num>\d+)\)|(?P<num>\d+))"
)
CH_LOGICAL_ERROR_RE = re.compile(r"Logical error:\s*(?P<msg>.+)")
CH_ASSERTION_RE = re.compile(
    r"(?:Assertion `[^']+' failed\.?|chassert[^\n]*failed[^\n]*|Assertion failure(?::|\s+)[^\n]+)"
)
CH_BASEDAEMON_QUERY_STRIP_RE = re.compile(
    r"\(query:\s+.*\)\s+(Received signal\s+[A-Za-z0-9 _-]+(?:\s*\(\d+\))?)\s*$",
    re.DOTALL,
)
CH_BASEDAEMON_QUERY_END_RE = re.compile(r"\(query:\s+.*\)\s*$", re.DOTALL)
CH_FRAME_RE = re.compile(
    r"^\d+(?:\.\d+)?\.\s+(?:inlined from\s+)?(?:(?P<file>[^\s:]+):(?P<line>\d+):\s+)?(?P<func>.+?)(?:\s+@\s+0x[0-9a-fA-F]+(?:\s+in\s+\S+)?)?\s*$"
)

# SQLite & DuckDB embedded patterns
SQLITE_ERROR_START_RE = re.compile(
    r"^(?:(?:Parse|Runtime)\s+)?[Ee]rror(?:\s+near\s+line\s+\d+)?:\s*"
)
SQLITE_NEAR_QUOTE_START_RE = re.compile(r':\s+near\s+"')
SQLITE_CARET_END_RE = re.compile(
    r"^\s*(?:\^---\s*error here|error here\s*---\^)\s*$"
)
C_ASSERTION_RE = re.compile(
    r"^(?:(?P<prog>[^\s:]+):\s+(?P<file>[^:\n]+):(?P<line>\d+):\s+(?P<func>.+?):\s+)?Assertion\s+`(?P<expr>.+?)'\s+failed\.?$"
)
C_ASSERTION_SEARCH_RE = re.compile(
    r"(?:(?P<prog>[^\s:]+):\s+(?P<file>[^:\n\s]+):(?P<line>\d+):\s+(?P<func>.+?):\s+)?Assertion\s+`(?P<expr>.+?)'\s+failed\.?"
)
BSD_ASSERTION_RE = re.compile(
    r"^Assertion failed:\s*\((?P<expr>[^)]+)\),\s*function\s+(?P<func>[^,]+),\s*file\s+(?P<file>[^,]+),\s*line\s+(?P<line>\d+)\.?$"
)
DUCKDB_NONFATAL_ERROR_RE = re.compile(
    r"^(?!INTERNAL Error:|FATAL Error:|Out of Memory Error:)[A-Z][A-Za-z ]+ Error:"
)
DUCKDB_CATALOG_WITH_NAME_RE = re.compile(r"^Catalog Error:\s+.*\bwith name\b")
DUCKDB_CATALOG_END_RE = re.compile(
    r"(?:does not exist|already exists|not found)[!.]?$"
)
DUCKDB_OOM_ERROR_RE = re.compile(r"^Out of Memory Error:")
DUCKDB_LINE_ECHO_RE = re.compile(r"^LINE \d+:")
DUCKDB_CARET_END_RE = re.compile(r"^\s*\^+\s*$")
DUCKDB_INTERNAL_ERROR_RE = re.compile(r"^INTERNAL Error:(?:\s*(?P<msg>.*))?$")
DUCKDB_FATAL_INVALIDATED_RE = re.compile(r"^FATAL Error:(?:\s*(?P<msg>.*))?$")

# Sanitizer & generic backtrace frame patterns
ASAN_RE = re.compile(r"AddressSanitizer:\s+(?P<kind>[^\s:]+)")
ASAN_BANNER_RE = re.compile(r"^(?:==\d+==\s*ERROR:\s*)?AddressSanitizer:\s+(?P<kind>[^\s:]+)")
ASAN_SUMMARY_RE = re.compile(
    r"^SUMMARY:\s*(?P<tool>AddressSanitizer|UndefinedBehaviorSanitizer):\s*(?P<kind>\S+)(?:\s+(?P<loc>\S+))?(?:\s+in\s+(?P<func>.+))?$"
)
UBSAN_RUNTIME_RE = re.compile(
    r"^(?P<file>[^:\n]+):(?P<line>\d+)(?::\d+)?:\s+runtime error:\s+(?P<msg>.+)$"
)
ASAN_FRAME_FILE_RE = re.compile(
    r"^\s*#\d+\s+0x[0-9a-fA-F]+\s+in\s+(?P<func>.+?)\s+(?P<file>(?:(?:/|\.{1,2}/)[^\s:]+|[A-Za-z0-9_./-]+\.(?:c|cc|cpp|cxx|h|hpp|hh|rs|y|l|tcc|ipp|inc|inl)):(?P<line>\d+))(?::\d+)?(?:\s.*)?$"
)
ASAN_FRAME_MOD_RE = re.compile(
    r"^\s*#\d+\s+0x[0-9a-fA-F]+\s+in\s+(?P<func>.+?)\s+\((?P<mod>[^()]+)\)(?:\s+\(BuildId:\s+[^)]+\))?\s*$"
)
ASAN_FRAME_ADDR_RE = re.compile(
    r"^\s*#\d+\s+0x[0-9a-fA-F]+\s+\((?P<mod>[^()]+)\)(?:\s+\(BuildId:\s+[^)]+\))?\s*$"
)
GLIBC_BT_RE = re.compile(
    r"^(?P<bin>(?:/|\./)?[^\s(:]+)\((?P<sym>.*)\)\s*\[0x[0-9a-fA-F]+\]\s*$"
)
MYSQL_BT_FILE_RE = re.compile(
    r"^(?P<file>[^\s:()]+\.(?:c|cc|cpp|cxx|h|hpp|y|l)):(?P<line>\d+)\((?P<func>.+)\)\s*$"
)



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


def resolve_binary_any(names, candidate_paths=None):
    for directory in os.environ.get("PATH", "").split(os.pathsep):
        if not directory:
            continue
        for name in names:
            candidate = os.path.join(directory, name)
            if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
                return candidate
    if candidate_paths:
        for path in candidate_paths:
            if os.path.isfile(path) and os.access(path, os.X_OK):
                return path
    return names[0]


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

        if ASAN_RE.search(line_text) or UBSAN_RUNTIME_RE.match(message) or UBSAN_RUNTIME_RE.match(line_text):
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


def decode_termination_signal(retcode):
    """Return (sig_num, sig_name) if retcode reflects POSIX signal termination."""
    if retcode is None:
        return None
    if retcode < 0:
        sig = -retcode
    elif 128 < retcode <= 192:
        sig = retcode - 128
    else:
        return None
    return sig, SIGNAL_NAMES.get(sig, f"SIG{sig}")


def _extract_mysql_family_evidence(dbms, server_log_text, stderr_text):
    """
    Extract MySQL 8.0 / MariaDB server error log fault events and supervisor exits.
    Excludes [Note]/[Warning] lines (and their continuation lines), Aborted
    connection warnings, crash-dump Query (...) SQL echoes, normal exit status 0,
    and client transport errors.
    """
    faults = []
    candidate_lines = []
    supervisor_exits = []
    line_re = MARIADB_LINE_RE if dbms == "mariadb" else MYSQL_80_LINE_RE
    conn_phrases = (
        MARIADB_CONNECTION_LOST_PHRASES
        if dbms == "mariadb"
        else MYSQL_CONNECTION_LOST_PHRASES
    )
    in_query_echo = False
    in_excluded_entry = False

    for raw_line in server_log_text.splitlines():
        line = raw_line.strip()
        if not line:
            in_query_echo = False
            in_excluded_entry = False
            continue

        m_sup = MYSQL_SUPERVISOR_EXIT_RE.search(line)
        if m_sup:
            in_query_echo = False
            in_excluded_entry = False
            status = int(m_sup.group(1))
            supervisor_exits.append(status)
            if status != 0:
                candidate_lines.append(line)
                fault_entry = {
                    "kind": f"{dbms}_supervisor_abnormal_exit",
                    "exit_status": status,
                    "detail": line,
                }
                sig_info = decode_termination_signal(status)
                if sig_info is not None:
                    fault_entry["signal"] = sig_info[0]
                faults.append(fault_entry)
            continue

        m_log = line_re.match(line)
        if m_log:
            in_query_echo = False
            level = m_log.group("level")
            event_code = m_log.group("code")
            subsystem = m_log.group("subsystem")
            message = m_log.group("message")
            if level in {"Note", "Warning"}:
                in_excluded_entry = True
                continue
            if MYSQL_ABORTED_CONNECTION_RE.search(message) or event_code in {
                "MY-010914",
                "MY-010926",
            }:
                in_excluded_entry = True
                continue
            in_excluded_entry = False
        else:
            if MYSQL_QUERY_ECHO_RE.match(line):
                in_query_echo = True
                in_excluded_entry = False
                continue
            if in_query_echo:
                if line.startswith(
                    ("Connection ID", "Status:", "The manual page", "Optimizer switch")
                ):
                    in_query_echo = False
                else:
                    continue
            if in_excluded_entry:
                continue
            level = ""
            event_code = None
            subsystem = None
            message = line
            if MYSQL_ABORTED_CONNECTION_RE.search(line):
                in_excluded_entry = True
                continue

        candidate_lines.append(line)

        m_sig = MYSQL_SIGNAL_RE.search(line)
        if m_sig:
            sig = int(m_sig.group(1) or m_sig.group(2))
            faults.append(
                {
                    "kind": f"{dbms}_server_signal",
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
                    "kind": f"{dbms}_assertion_failure",
                    "event_code": event_code,
                    "subsystem": subsystem,
                    "detail": line,
                }
            )
            continue

        if (
            ASAN_RE.search(line)
            or UBSAN_RUNTIME_RE.match(message)
            or UBSAN_RUNTIME_RE.match(line)
        ):
            faults.append(
                {
                    "kind": f"{dbms}_asan_fault",
                    "event_code": event_code,
                    "detail": line,
                }
            )
            continue

        _ = level

    corroboration = {
        "connection_lost": any(p in stderr_text for p in conn_phrases),
        "supervisor_exit_codes": supervisor_exits,
    }
    return faults, candidate_lines, corroboration


def extract_mysql_evidence(server_log_text, stderr_text):
    """
    Extract MySQL 8.0 (8.0.20) server error log fault events and supervisor exits.
    Excludes [Note]/[Warning] lines, Aborted connection warnings, normal exit
    status 0, and client-only transport errors (2006/2013).
    """
    return _extract_mysql_family_evidence("mysql", server_log_text, stderr_text)


def extract_mariadb_evidence(server_log_text, stderr_text):
    """
    Extract MariaDB server error log fault events and supervisor exits,
    reusing MySQL-family rules with MariaDB timestamp/InnoDB assertion formats.
    """
    return _extract_mysql_family_evidence("mariadb", server_log_text, stderr_text)


def _has_basedaemon_query_continuation(raw_lines, idx):
    """Return True if unprefixed lines from idx close an open `(query: ...)`."""
    k = idx
    while k < len(raw_lines):
        nxt = raw_lines[k].strip()
        if not nxt:
            k += 1
            continue
        if CH_LOG_LINE_RE.match(nxt) or CH_SUPERVISOR_EXIT_RE.search(nxt):
            return False
        if re.match(
            r"^\)\s*(?:Received signal\s+[A-Za-z0-9 _-]+(?:\s*\(\d+\))?)?\s*$",
            nxt,
        ) or re.search(
            r"\)\s+Received signal\s+[A-Za-z0-9 _-]+(?:\s*\(\d+\))?\s*$",
            nxt,
        ):
            return True
        k += 1
    return False


def _strip_clickhouse_query_echo(message):
    stripped = CH_BASEDAEMON_QUERY_STRIP_RE.sub(r"\1", message)
    if stripped != message:
        return stripped.strip()
    stripped = CH_BASEDAEMON_QUERY_END_RE.sub("", message).strip()
    return stripped


def extract_clickhouse_evidence(server_log_text, stderr_text):
    """
    Extract ClickHouse server fatal/BaseDaemon crash events and supervisor exits.
    Excludes <Information>/<Warning>/<Error> query exception logs (including their
    multi-line SQL echoes and exception stack traces) and strips multi-line
    BaseDaemon `(query: ...)` echoes so candidate SQL cannot spoof signatures.
    """
    faults = []
    candidate_lines = []
    supervisor_exits = []

    raw_lines = server_log_text.splitlines()
    idx = 0
    in_excluded_entry = False
    in_fatal_entry = False

    while idx < len(raw_lines):
        line = raw_lines[idx].strip()
        idx += 1
        if not line:
            continue

        m_sup = CH_SUPERVISOR_EXIT_RE.search(line)
        if m_sup:
            in_excluded_entry = False
            in_fatal_entry = False
            status = int(m_sup.group(1))
            supervisor_exits.append(status)
            if status != 0:
                candidate_lines.append(line)
                fault_entry = {
                    "kind": "clickhouse_supervisor_abnormal_exit",
                    "exit_status": status,
                    "detail": line,
                }
                sig_info = decode_termination_signal(status)
                if sig_info is not None:
                    fault_entry["signal"] = sig_info[0]
                faults.append(fault_entry)
            continue

        m_ch = CH_LOG_LINE_RE.match(line)
        if m_ch:
            level = m_ch.group("level")
            component = m_ch.group("component").strip()
            message = m_ch.group("message").strip()
            if level != "Fatal":
                in_excluded_entry = True
                in_fatal_entry = False
                continue
            in_excluded_entry = False
            in_fatal_entry = True

            # BaseDaemon `(query: ...)` can span multiple lines when candidate SQL
            # has newlines; accumulate until the closing `) [Received signal ...]`
            # before stripping so neither SQL lines nor the signal phrase leak.
            if "(query:" in message and (
                (
                    not CH_BASEDAEMON_QUERY_STRIP_RE.search(message)
                    and not CH_BASEDAEMON_QUERY_END_RE.search(message)
                )
                or _has_basedaemon_query_continuation(raw_lines, idx)
            ):
                while idx < len(raw_lines):
                    nxt_line = raw_lines[idx].strip()
                    if CH_LOG_LINE_RE.match(nxt_line) or CH_SUPERVISOR_EXIT_RE.search(nxt_line):
                        break
                    message += "\n" + nxt_line
                    idx += 1
                    if (
                        CH_BASEDAEMON_QUERY_STRIP_RE.search(message)
                        or CH_BASEDAEMON_QUERY_END_RE.search(message)
                    ) and not _has_basedaemon_query_continuation(raw_lines, idx):
                        break

            if set(message) <= {"#"} or not message:
                continue
            clean_msg = _strip_clickhouse_query_echo(message)
            if not clean_msg:
                continue
            clean_line = f"<Fatal> {component}: {clean_msg}" if component else f"<Fatal> : {clean_msg}"
            candidate_lines.append(clean_line)
        else:
            if in_excluded_entry:
                continue
            component = ""
            clean_msg = line
            clean_line = line
            if (
                ASAN_BANNER_RE.match(line)
                or ASAN_SUMMARY_RE.match(line)
                or UBSAN_RUNTIME_RE.match(line)
                or C_ASSERTION_RE.match(line)
                or BSD_ASSERTION_RE.match(line)
                or ASAN_FRAME_FILE_RE.match(line)
                or ASAN_FRAME_MOD_RE.match(line)
                or ASAN_FRAME_ADDR_RE.match(line)
                or (in_fatal_entry and CH_FRAME_RE.match(line))
            ):
                candidate_lines.append(line)
            else:
                continue

        m_sig = CH_SIGNAL_RE.search(clean_msg)
        if m_sig:
            sig = int(m_sig.group("paren_num") or m_sig.group("num"))
            faults.append(
                {
                    "kind": "clickhouse_server_signal",
                    "signal": sig,
                    "signal_desc": (m_sig.group("desc") or "").strip() or None,
                    "component": component or None,
                    "detail": clean_line,
                }
            )
            continue

        m_logic = CH_LOGICAL_ERROR_RE.search(clean_msg)
        if m_logic:
            faults.append(
                {
                    "kind": "clickhouse_logical_error",
                    "message": m_logic.group("msg").strip(),
                    "component": component or None,
                    "detail": clean_line,
                }
            )
            continue

        if CH_ASSERTION_RE.search(clean_msg) or C_ASSERTION_RE.match(clean_msg) or BSD_ASSERTION_RE.match(clean_msg):
            faults.append(
                {
                    "kind": "clickhouse_assertion_failure",
                    "component": component or None,
                    "detail": clean_line,
                }
            )
            continue

        if ASAN_RE.search(clean_msg) or UBSAN_RUNTIME_RE.match(clean_msg):
            faults.append(
                {
                    "kind": "clickhouse_asan_fault",
                    "component": component or None,
                    "detail": clean_line,
                }
            )
            continue

    corroboration = {
        "connection_lost": any(p in stderr_text for p in CLICKHOUSE_CONNECTION_LOST_PHRASES),
        "supervisor_exit_codes": supervisor_exits,
    }
    return faults, candidate_lines, corroboration


def _looks_like_backtrace_line(line):
    return bool(
        ASAN_FRAME_FILE_RE.match(line)
        or ASAN_FRAME_MOD_RE.match(line)
        or ASAN_FRAME_ADDR_RE.match(line)
        or GLIBC_BT_RE.match(line)
        or MYSQL_BT_FILE_RE.match(line)
    )


def _strip_sqlite_error_blocks(raw_lines):
    """
    Remove bounded SQLite error blocks (caret-terminated syntax error echoes,
    unclosed-quote multi-line error echoes, and non-backtrace 2-space SQL
    echo lines) before scanning for ASan/UBSan/assertion evidence.
    Non-caret SQLite error headers are retained so `in_sqlite_error` still
    guards immediate follow-up lines when no crash signal occurred.
    """
    cleaned = []
    idx = 0
    n = len(raw_lines)
    while idx < n:
        raw_line = raw_lines[idx]
        stripped = raw_line.strip()
        if not SQLITE_ERROR_START_RE.match(stripped):
            cleaned.append(raw_line)
            idx += 1
            continue

        # Look ahead for a matching SQLite caret terminator (`^--- error here`
        # or `error here ---^`) belonging to this error block.
        in_quotes = bool(SQLITE_NEAR_QUOTE_START_RE.search(raw_line)) and (
            raw_line.count('"') % 2
        ) == 1
        caret_end = None
        j = idx + 1
        while j < n:
            nxt = raw_lines[j]
            nxt_stripped = nxt.strip()
            if SQLITE_CARET_END_RE.match(nxt):
                caret_end = j
                break
            if not in_quotes and not nxt.startswith("  ") and (
                SQLITE_ERROR_START_RE.match(nxt_stripped)
                or ASAN_BANNER_RE.match(nxt_stripped)
                or ASAN_SUMMARY_RE.match(nxt_stripped)
                or _looks_like_backtrace_line(nxt_stripped)
            ):
                break
            if (in_quotes or nxt.startswith("  ")) and (nxt.count('"') % 2) == 1:
                in_quotes = not in_quotes
            j += 1

        if caret_end is not None:
            idx = caret_end + 1
            continue

        # No caret terminator: if the header is a `near "..."` syntax error
        # with an unclosed double-quoted token that closes later (e.g. SQLite
        # 3.31 `near "a\n...": syntax error`), strip the whole quoted block.
        if SQLITE_NEAR_QUOTE_START_RE.search(raw_line) and (raw_line.count('"') % 2) == 1:
            k = idx + 1
            closed = False
            while k < n:
                if (raw_lines[k].count('"') % 2) == 1:
                    closed = True
                    k += 1
                    break
                k += 1
            if closed:
                while (
                    k < n
                    and raw_lines[k].startswith("  ")
                    and not _looks_like_backtrace_line(raw_lines[k].strip())
                ):
                    k += 1
                idx = k
                continue

        # Retain the header line as an in_sqlite_error boundary marker, and
        # drop any immediate non-backtrace 2-space SQL echo lines.
        cleaned.append(raw_line)
        idx += 1
        while (
            idx < n
            and raw_lines[idx].startswith("  ")
            and not _looks_like_backtrace_line(raw_lines[idx].strip())
        ):
            idx += 1

    return cleaned


def extract_sqlite_evidence(stderr_text, retcode=0):
    """
    Extract embedded SQLite fault evidence from process exit status and stderr.
    Filters out SQLite Parse/Runtime/Error SQL echoes and distinguishes
    SQLITE_ABORT (exit 4) and external/OOM kills (SIGKILL/SIGTERM) from crashes.
    """
    faults = []
    candidate_lines = []
    sig_info = decode_termination_signal(retcode)
    has_crash_signal = sig_info is not None and sig_info[0] in CRASH_SIGNALS

    raw_lines = _strip_sqlite_error_blocks(stderr_text.splitlines())
    has_asan_banner = any(ASAN_BANNER_RE.match(ln.strip()) for ln in raw_lines)
    has_asan_frame = any(_looks_like_backtrace_line(ln.strip()) for ln in raw_lines)
    has_sanitizer_corroboration = has_asan_banner and has_asan_frame
    has_asan_abrt = has_sanitizer_corroboration and any(
        "AddressSanitizer: ABRT" in ln for ln in raw_lines
    )
    has_abrt_signal = (sig_info is not None and sig_info[0] == 6) or has_asan_abrt

    in_sqlite_error = False
    for raw_line in raw_lines:
        stripped = raw_line.strip()
        if not stripped:
            continue

        if SQLITE_ERROR_START_RE.match(stripped):
            in_sqlite_error = True
            continue
        if SQLITE_CARET_END_RE.match(raw_line):
            in_sqlite_error = False
            continue
        if _looks_like_backtrace_line(stripped) and (
            has_crash_signal or has_sanitizer_corroboration or not in_sqlite_error
        ):
            in_sqlite_error = False
            candidate_lines.append(stripped)
            continue
        if raw_line.startswith("  ") and in_sqlite_error:
            continue

        m_c = C_ASSERTION_RE.match(stripped)
        m_bsd = BSD_ASSERTION_RE.match(stripped)
        if m_c or m_bsd:
            if not has_abrt_signal:
                continue
            in_sqlite_error = False
            candidate_lines.append(stripped)
            m = m_c or m_bsd
            faults.append(
                {
                    "kind": "sqlite_assertion_failure",
                    "file": m.group("file"),
                    "line": int(m.group("line")),
                    "function": m.group("func").strip(),
                    "assertion": m.group("expr").strip(),
                    "detail": stripped,
                }
            )
            continue

        if ASAN_BANNER_RE.match(stripped):
            if has_crash_signal or has_sanitizer_corroboration:
                in_sqlite_error = False
                candidate_lines.append(stripped)
                faults.append(
                    {
                        "kind": "sqlite_asan_fault",
                        "detail": stripped,
                    }
                )
            continue

        if ASAN_SUMMARY_RE.match(stripped):
            if has_crash_signal or has_sanitizer_corroboration:
                in_sqlite_error = False
                candidate_lines.append(stripped)
            continue

        if UBSAN_RUNTIME_RE.match(stripped):
            if has_crash_signal or not in_sqlite_error:
                in_sqlite_error = False
                candidate_lines.append(stripped)
                faults.append(
                    {
                        "kind": "sqlite_asan_fault",
                        "detail": stripped,
                    }
                )
            continue

    oom = False
    external_kill = False
    sqlite_abort = retcode == 4 and not faults and not has_crash_signal
    if sig_info is not None:
        sig, sig_name = sig_info
        if sig in CRASH_SIGNALS:
            detail = f"sqlite3 terminated by signal {sig} ({sig_name})"
            candidate_lines.append(detail)
            faults.append(
                {
                    "kind": "sqlite_process_signal",
                    "signal": sig,
                    "signal_name": sig_name,
                    "detail": detail,
                }
            )
        elif sig == 9:
            oom = True
            external_kill = True
        elif sig in EXTERNAL_OR_OOM_SIGNALS:
            external_kill = True

    corroboration = {
        "retcode": retcode,
        "signal": sig_info[0] if sig_info else None,
        "sqlite_abort": sqlite_abort,
        "oom": oom,
        "oom_cause": "signal_9_kill" if oom else None,
        "external_kill": external_kill,
    }
    return faults, candidate_lines, corroboration


def _scan_duckdb_error_delimiters(line, in_dq, in_sq, paren_depth):
    """Update double/single quote state and unquoted paren depth across a line."""
    n = len(line)
    i = 0
    while i < n:
        ch = line[i]
        if in_dq:
            if ch == '"':
                in_dq = False
        elif in_sq:
            if ch == "'":
                prev_alpha = i > 0 and line[i - 1].isalpha()
                next_alpha = i + 1 < n and line[i + 1].isalpha()
                if not (prev_alpha and next_alpha):
                    in_sq = False
        else:
            if ch == '"':
                in_dq = True
            elif ch == "'":
                prev_alpha = i > 0 and line[i - 1].isalpha()
                next_alpha = i + 1 < n and line[i + 1].isalpha()
                if not (prev_alpha and next_alpha):
                    in_sq = True
            elif ch == "(":
                paren_depth += 1
            elif ch == ")":
                paren_depth = max(0, paren_depth - 1)
        i += 1
    return in_dq, in_sq, paren_depth


def _consume_duckdb_line_echo(raw_lines, start_idx):
    """
    Advance past a DuckDB `LINE N:` echo block starting at raw_lines[start_idx - 1].
    Consumes through the matching `^` caret line when present; if no caret line
    belongs to this echo (e.g. truncated stderr), consumes only the `LINE N:`
    line so subsequent faults are never swallowed.
    """
    header = raw_lines[start_idx - 1]
    in_dq, in_sq, _ = _scan_duckdb_error_delimiters(header, False, False, 0)
    k = start_idx
    while k < len(raw_lines):
        nxt = raw_lines[k]
        nxt_stripped = nxt.strip()
        if DUCKDB_CARET_END_RE.match(nxt):
            return k + 1
        if not in_dq and not in_sq and (
            not nxt_stripped
            or DUCKDB_LINE_ECHO_RE.match(nxt_stripped)
            or DUCKDB_NONFATAL_ERROR_RE.match(nxt_stripped)
            or DUCKDB_OOM_ERROR_RE.match(nxt_stripped)
            or DUCKDB_INTERNAL_ERROR_RE.match(nxt_stripped)
            or DUCKDB_FATAL_INVALIDATED_RE.match(nxt_stripped)
            or ASAN_BANNER_RE.match(nxt_stripped)
            or ASAN_SUMMARY_RE.match(nxt_stripped)
            or UBSAN_RUNTIME_RE.match(nxt_stripped)
            or C_ASSERTION_RE.match(nxt_stripped)
            or BSD_ASSERTION_RE.match(nxt_stripped)
            or _looks_like_backtrace_line(nxt_stripped)
            or nxt_stripped.startswith(
                (
                    "This error signals an assertion failure",
                    "Possible solutions:",
                    "Stack Trace:",
                )
            )
        ):
            break
        in_dq, in_sq, _ = _scan_duckdb_error_delimiters(nxt, in_dq, in_sq, 0)
        k += 1

    return start_idx


def _consume_duckdb_nonfatal_error(raw_lines, start_idx):
    """
    Advance past a single DuckDB non-fatal error block starting at raw_lines[start_idx - 1].
    Handles both `LINE N:` + caret blocks and standalone errors without `LINE N:`
    (Constraint / TransactionContext / Unknown Command / PRAGMA / CALL errors),
    ensuring multi-line SQL echoes cannot spoof faults while subsequent statements'
    OOM / INTERNAL / FATAL errors are never swallowed.
    """
    header = raw_lines[start_idx - 1].strip()
    is_unquoted_catalog = bool(DUCKDB_CATALOG_WITH_NAME_RE.match(header))
    if is_unquoted_catalog:
        in_dq = False
        in_sq = False
        paren_depth = 0
        saw_catalog_end = bool(DUCKDB_CATALOG_END_RE.search(header))
    else:
        in_dq, in_sq, paren_depth = _scan_duckdb_error_delimiters(
            header, False, False, 0
        )
        saw_catalog_end = True

    # Pass 1: check if a `LINE N:` + caret terminator belongs to this error.
    j = start_idx
    lk_dq, lk_sq, lk_parens = in_dq, in_sq, paren_depth
    lk_cat_end = saw_catalog_end
    while j < len(raw_lines):
        s = raw_lines[j].strip()
        if DUCKDB_LINE_ECHO_RE.match(s):
            return _consume_duckdb_line_echo(raw_lines, j + 1)
        if not lk_dq and not lk_sq and lk_parens == 0 and lk_cat_end and (
            s.startswith(
                (
                    "This error signals an assertion failure",
                    "Possible solutions:",
                    "Stack Trace:",
                    "For more information, see",
                )
            )
            or DUCKDB_NONFATAL_ERROR_RE.match(s)
        ):
            break
        if is_unquoted_catalog and not lk_cat_end:
            if DUCKDB_CATALOG_END_RE.search(s):
                lk_cat_end = True
        else:
            lk_dq, lk_sq, lk_parens = _scan_duckdb_error_delimiters(
                s, lk_dq, lk_sq, lk_parens
            )
        j += 1

    # Pass 2: no `LINE N:` block belongs to this error; consume only its own
    # unclosed quotes/parens/unquoted-Catalog message plus immediate hint lines.
    idx = start_idx
    while idx < len(raw_lines) and (
        in_dq or in_sq or paren_depth > 0 or (is_unquoted_catalog and not saw_catalog_end)
    ):
        s = raw_lines[idx].strip()
        if not in_dq and not in_sq and paren_depth == 0 and saw_catalog_end and (
            s.startswith(
                (
                    "This error signals an assertion failure",
                    "Possible solutions:",
                    "Stack Trace:",
                    "For more information, see",
                )
            )
            or ASAN_BANNER_RE.match(s)
            or DUCKDB_FATAL_INVALIDATED_RE.match(s)
            or DUCKDB_INTERNAL_ERROR_RE.match(s)
            or DUCKDB_OOM_ERROR_RE.match(s)
            or DUCKDB_NONFATAL_ERROR_RE.match(s)
        ):
            break
        idx += 1
        if is_unquoted_catalog and not saw_catalog_end:
            if DUCKDB_CATALOG_END_RE.search(s):
                saw_catalog_end = True
        else:
            in_dq, in_sq, paren_depth = _scan_duckdb_error_delimiters(
                s, in_dq, in_sq, paren_depth
            )

    while idx < len(raw_lines):
        peek = idx
        while peek < len(raw_lines) and not raw_lines[peek].strip():
            peek += 1
        if peek < len(raw_lines) and raw_lines[peek].strip().startswith(
            ("Did you mean", "Candidate bindings:", "Run '.help'", "See also ")
        ):
            idx = peek + 1
        else:
            break

    return idx


def extract_duckdb_evidence(stderr_text, retcode=0):
    """
    Extract embedded DuckDB fault evidence from process exit status and stderr.
    Distinguishes isolated process termination (signals/ASan/assertions) from
    INTERNAL Error / database invalidation, filters SQL error echoes, and retains
    Out of Memory Error / SIGKILL as an explicit OOM cause.
    """
    faults = []
    candidate_lines = []
    sig_info = decode_termination_signal(retcode)
    has_crash_signal = sig_info is not None and sig_info[0] in CRASH_SIGNALS

    raw_lines = stderr_text.splitlines()
    has_asan_banner = any(ASAN_BANNER_RE.match(ln.strip()) for ln in raw_lines)
    has_asan_frame = any(_looks_like_backtrace_line(ln.strip()) for ln in raw_lines)
    has_sanitizer_corroboration = has_asan_banner and has_asan_frame
    has_asan_abrt = has_sanitizer_corroboration and any(
        "AddressSanitizer: ABRT" in ln for ln in raw_lines
    )
    has_abrt_signal = (sig_info is not None and sig_info[0] == 6) or has_asan_abrt

    oom = False
    oom_cause = None
    database_invalidated = False

    idx = 0
    while idx < len(raw_lines):
        raw_line = raw_lines[idx]
        idx += 1
        stripped = raw_line.strip()

        if DUCKDB_LINE_ECHO_RE.match(stripped):
            idx = _consume_duckdb_line_echo(raw_lines, idx)
            continue

        if not stripped:
            continue

        if DUCKDB_OOM_ERROR_RE.match(stripped):
            oom = True
            oom_cause = "duckdb_oom_error"
            continue

        if DUCKDB_NONFATAL_ERROR_RE.match(stripped):
            idx = _consume_duckdb_nonfatal_error(raw_lines, idx)
            continue

        if _looks_like_backtrace_line(stripped):
            candidate_lines.append(stripped)
            continue

        m_int = DUCKDB_INTERNAL_ERROR_RE.match(stripped)
        if m_int:
            parts = []
            inline_msg = (m_int.group("msg") or "").strip()
            if inline_msg:
                parts.append(inline_msg)
            while idx < len(raw_lines):
                nxt = raw_lines[idx].strip()
                if (
                    not nxt
                    or DUCKDB_LINE_ECHO_RE.match(nxt)
                    or DUCKDB_FATAL_INVALIDATED_RE.match(nxt)
                    or DUCKDB_NONFATAL_ERROR_RE.match(nxt)
                    or DUCKDB_OOM_ERROR_RE.match(nxt)
                ):
                    break
                if nxt == "Stack Trace:" or _looks_like_backtrace_line(nxt):
                    break
                if not nxt.startswith(("This error signals an assertion failure", "For more information, see")):
                    parts.append(nxt)
                idx += 1
            body = " ".join(parts).strip()
            detail = f"INTERNAL Error: {body}" if body else "INTERNAL Error"
            candidate_lines.append(detail)
            faults.append(
                {
                    "kind": "duckdb_internal_error",
                    "message": body,
                    "detail": detail,
                }
            )
            continue

        m_fat = DUCKDB_FATAL_INVALIDATED_RE.match(stripped)
        if m_fat:
            parts = []
            inline_msg = (m_fat.group("msg") or "").strip()
            if inline_msg:
                parts.append(inline_msg)
            while idx < len(raw_lines):
                nxt = raw_lines[idx].strip()
                if (
                    not nxt
                    or DUCKDB_LINE_ECHO_RE.match(nxt)
                    or DUCKDB_INTERNAL_ERROR_RE.match(nxt)
                    or DUCKDB_NONFATAL_ERROR_RE.match(nxt)
                    or DUCKDB_OOM_ERROR_RE.match(nxt)
                ):
                    break
                parts.append(nxt)
                idx += 1
            body = " ".join(parts).strip()
            if "invalidated" in body.lower() or "fatal error" in body.lower() or body:
                database_invalidated = True
                detail = f"FATAL Error: {body}" if body else "FATAL Error"
                candidate_lines.append(detail)
                faults.append(
                    {
                        "kind": "duckdb_database_invalidated",
                        "message": body,
                        "detail": detail,
                    }
                )
            continue

        m_c = C_ASSERTION_RE.match(stripped)
        m_bsd = BSD_ASSERTION_RE.match(stripped)
        if m_c or m_bsd:
            if not has_abrt_signal:
                continue
            candidate_lines.append(stripped)
            m = m_c or m_bsd
            faults.append(
                {
                    "kind": "duckdb_assertion_failure",
                    "file": m.group("file"),
                    "line": int(m.group("line")),
                    "function": m.group("func").strip(),
                    "assertion": m.group("expr").strip(),
                    "detail": stripped,
                }
            )
            continue

        if ASAN_BANNER_RE.match(stripped):
            if has_crash_signal or has_sanitizer_corroboration:
                candidate_lines.append(stripped)
                faults.append(
                    {
                        "kind": "duckdb_asan_fault",
                        "detail": stripped,
                    }
                )
            continue

        if ASAN_SUMMARY_RE.match(stripped):
            if has_crash_signal or has_sanitizer_corroboration:
                candidate_lines.append(stripped)
            continue

        if UBSAN_RUNTIME_RE.match(stripped):
            candidate_lines.append(stripped)
            faults.append(
                {
                    "kind": "duckdb_asan_fault",
                    "detail": stripped,
                }
            )
            continue

    external_kill = False
    if sig_info is not None:
        sig, sig_name = sig_info
        if sig in CRASH_SIGNALS:
            detail = f"duckdb terminated by signal {sig} ({sig_name})"
            candidate_lines.append(detail)
            faults.append(
                {
                    "kind": "duckdb_process_signal",
                    "signal": sig,
                    "signal_name": sig_name,
                    "detail": detail,
                }
            )
        elif sig == 9:
            oom = True
            oom_cause = oom_cause or "oom_kill_signal_9"
            external_kill = True
        elif sig in EXTERNAL_OR_OOM_SIGNALS:
            external_kill = True

    corroboration = {
        "retcode": retcode,
        "signal": sig_info[0] if sig_info else None,
        "database_invalidated": database_invalidated,
        "oom": oom,
        "oom_cause": oom_cause,
        "external_kill": external_kill,
    }
    return faults, candidate_lines, corroboration


def extract_dbms_evidence(dbms, server_log_text, stderr_text, retcode=0):
    if dbms == "mysql":
        return extract_mysql_evidence(server_log_text, stderr_text)
    if dbms == "mariadb":
        return extract_mariadb_evidence(server_log_text, stderr_text)
    if dbms == "clickhouse":
        return extract_clickhouse_evidence(server_log_text, stderr_text)
    if dbms == "sqlite":
        return extract_sqlite_evidence(stderr_text, retcode=retcode)
    if dbms == "duckdb":
        return extract_duckdb_evidence(stderr_text, retcode=retcode)
    return extract_postgres_evidence(server_log_text, stderr_text)


# ---------------------------------------------------------------------------
# RQ3: Structured Crash Signature Normalization & Bug Slippage Tracking
# ---------------------------------------------------------------------------

_WRAPPER_FRAME_PREFIXES = (
    "__asan",
    "__ubsan",
    "__lsan",
    "__msan",
    "__tsan",
    "__sanitizer",
    "__interceptor_",
    "__gi_raise",
    "__gi_abort",
    "__gi___assert_fail",
    "__assert_fail",
    "__pthread_kill",
    "pthread_kill",
    "__restore_rt",
    "handle_fatal_signal",
    "my_print_stacktrace",
    "print_stacktrace",
    "ut_dbg_assertion_failed",
    "basedaemon",
    "poco::signalhandler",
    "signalhandler",
    "stacktrace::",
    "db::abortonfailedassertion",
    "__libc_start_",
    "libpthread.so",
    "libc.so",
    "libasan.so",
    "libubsan.so",
    "linux-vdso.so",
)
_WRAPPER_EXACT_FUNCS = {"abort", "raise", "gsignal", "killpg", "_start"}


def _env_int(name, default):
    raw = os.environ.get(name, "").strip()
    return int(raw) if raw else default


def _env_float(name, default):
    raw = os.environ.get(name, "").strip()
    return float(raw) if raw else default


def normalize_source_path(path, keep_src_rel=False):
    if not path:
        return None
    norm = path.replace("\\", "/").strip()
    norm = re.sub(r"^\./", "", norm)
    if keep_src_rel:
        for marker in ("/src/", "/storage/", "/sql/"):
            idx = norm.find(marker)
            if idx != -1:
                rel = norm[idx + 1 :]
                tail = rel.split("/", 1)[1] if "/" in rel else ""
                if "/" in tail:
                    return rel
        if norm.startswith(("src/", "storage/", "sql/")):
            tail = norm.split("/", 1)[1] if "/" in norm else ""
            if "/" in tail:
                return norm
    return os.path.basename(norm)


def _normalize_volatile_tokens(text):
    out = re.sub(r"0x[0-9a-fA-F]+", "0xADDR", text)
    out = re.sub(
        r"\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\b",
        "<UUID>",
        out,
    )
    out = re.sub(r"\bPID\s+\d+\b", "PID <N>", out)
    out = re.sub(r"\s+", " ", out).strip()
    return out


def _is_wrapper_frame(frame):
    parts = frame.split("@", 1)
    func_part = parts[0].strip().lower()
    mod_part = parts[1].strip().lower() if len(parts) > 1 else ""
    if mod_part.startswith(("libasan.so", "libubsan.so")):
        return True
    if func_part in _WRAPPER_EXACT_FUNCS:
        return True
    for prefix in _WRAPPER_FRAME_PREFIXES:
        if func_part.startswith(prefix):
            return True
    return False


def _parse_stack_frame(raw_line):
    line = raw_line.strip()
    if not line:
        return None

    m = ASAN_FRAME_FILE_RE.match(line)
    if m:
        func = re.sub(r"\s+", " ", m.group("func").strip())
        src_path, line_no = m.group("file").rsplit(":", 1)
        norm_file = normalize_source_path(src_path, keep_src_rel=True)
        return f"{func}@{norm_file}:{line_no}"

    m = ASAN_FRAME_MOD_RE.match(line)
    if m:
        func = re.sub(r"\s+", " ", m.group("func").strip())
        mod = m.group("mod").strip()
        mod_base = os.path.basename(mod.split("+", 1)[0])
        if "+" in mod:
            off = mod.split("+", 1)[1]
            return f"{func}@{mod_base}+{off}"
        return f"{func}@{mod_base}"

    m = ASAN_FRAME_ADDR_RE.match(line)
    if m:
        mod = m.group("mod").strip()
        if "+" in mod:
            path_part, off = mod.split("+", 1)
            return f"{os.path.basename(path_part)}+{off}"
        return os.path.basename(mod)

    m_ch = CH_LOG_LINE_RE.match(line)
    if m_ch:
        ch_msg = m_ch.group("message").strip()
    elif line.startswith("<Fatal> "):
        ch_msg = line.split(":", 1)[1].strip() if ":" in line else line
    else:
        ch_msg = line

    m = CH_FRAME_RE.match(ch_msg)
    if m:
        func = re.sub(r"\s+", " ", m.group("func").strip())
        if m.group("file"):
            norm_file = normalize_source_path(m.group("file"), keep_src_rel=True)
            return f"{func}@{norm_file}:{m.group('line')}"
        if func.startswith("/") and "(+" in func:
            mod_part, off_part = func.rstrip(")").split("(+", 1)
            return f"{os.path.basename(mod_part)}+{off_part}"
        return func

    m = GLIBC_BT_RE.match(line)
    if m:
        sym = m.group("sym").strip()
        bin_base = os.path.basename(m.group("bin"))
        if not sym:
            return bin_base
        if sym.startswith("+0x"):
            return f"{bin_base}{sym}"
        func = re.sub(r"\+0x[0-9a-fA-F]+$", "", sym).strip()
        return func

    m = MYSQL_BT_FILE_RE.match(line)
    if m:
        norm_file = normalize_source_path(m.group("file"), keep_src_rel=True)
        return f"{m.group('func').strip()}@{norm_file}:{m.group('line')}"

    return None


def extract_stack_frames(lines, top_n=5):
    raw_frames = []
    seen = set()
    for line in lines:
        frame = _parse_stack_frame(line)
        if frame and frame not in seen:
            seen.add(frame)
            raw_frames.append(frame)

    filtered = [f for f in raw_frames if not _is_wrapper_frame(f)]
    chosen = filtered if filtered else raw_frames
    return chosen[: max(1, top_n)]


def _parse_assertion_details(faults):
    file_name = None
    line_num = None
    func_name = None
    assertion_expr = None

    for fault in faults:
        kind = fault.get("kind", "")
        if kind not in {
            "pg_assertion_trap",
            "mysql_assertion_failure",
            "mariadb_assertion_failure",
            "clickhouse_assertion_failure",
            "sqlite_assertion_failure",
            "duckdb_assertion_failure",
        }:
            continue
        detail = fault.get("detail", "")

        if fault.get("file") and file_name is None:
            file_name = normalize_source_path(fault["file"])
        if fault.get("line") and line_num is None:
            line_num = int(fault["line"])
        if fault.get("function") and func_name is None:
            func_name = fault["function"]
        if fault.get("assertion") and assertion_expr is None:
            assertion_expr = re.sub(r"\s+", " ", fault["assertion"]).strip()

        m_pg = PG_TRAP_STRUCT_RE.search(detail)
        if m_pg:
            file_name = file_name or normalize_source_path(m_pg.group("file"))
            line_num = line_num or int(m_pg.group("line"))
            assertion_expr = assertion_expr or re.sub(
                r"\s+", " ", m_pg.group("expr")
            ).strip()
            continue

        m_my = re.search(
            r"Assertion failure:\s*(?P<file>[^:\s]+):(?P<line>\d+):(?P<expr>.+)$",
            detail,
        )
        if m_my:
            file_name = file_name or normalize_source_path(m_my.group("file"))
            line_num = line_num or int(m_my.group("line"))
            assertion_expr = assertion_expr or re.sub(
                r"\s+", " ", m_my.group("expr")
            ).strip()
            continue

        m_inno_file = re.search(
            r"Assertion failure\s+in\s+(?:thread\s+\S+\s+in\s+)?file\s+(?P<file>\S+)\s+line\s+(?P<line>\d+)",
            detail,
        )
        if m_inno_file:
            file_name = file_name or normalize_source_path(m_inno_file.group("file"))
            line_num = line_num or int(m_inno_file.group("line"))
            continue

        m_fail_assert = re.search(r"Failing assertion:\s*(?P<expr>.+)$", detail)
        if m_fail_assert:
            assertion_expr = assertion_expr or re.sub(
                r"\s+", " ", m_fail_assert.group("expr")
            ).strip()
            continue

        m_c = C_ASSERTION_SEARCH_RE.search(detail)
        if m_c:
            if m_c.group("file"):
                file_name = file_name or normalize_source_path(m_c.group("file"))
            if m_c.group("line"):
                line_num = line_num or int(m_c.group("line"))
            if m_c.group("func"):
                func_name = func_name or m_c.group("func").strip()
            assertion_expr = assertion_expr or re.sub(
                r"\s+", " ", m_c.group("expr")
            ).strip()
            continue

        m_bsd = BSD_ASSERTION_RE.search(detail)
        if m_bsd:
            file_name = file_name or normalize_source_path(m_bsd.group("file"))
            line_num = line_num or int(m_bsd.group("line"))
            func_name = func_name or m_bsd.group("func").strip()
            assertion_expr = assertion_expr or re.sub(
                r"\s+", " ", m_bsd.group("expr")
            ).strip()
            continue

        if assertion_expr is None:
            m_generic = re.search(r"Assertion\s+`([^']+)'\s+failed", detail)
            if m_generic:
                assertion_expr = re.sub(r"\s+", " ", m_generic.group(1)).strip()
            else:
                assertion_expr = _normalize_volatile_tokens(detail)

    return file_name, line_num, func_name, assertion_expr


def extract_crash_signature(
    dbms, faults, candidate_lines, stderr_text="", retcode=0, top_frames=5
):
    """
    Build a normalized structured crash signature (RQ3) from fault events and
    captured backtrace/log lines for deduplication and slippage tracking.
    """
    if not faults:
        return None

    lines = [f.get("detail", "") for f in faults] + list(candidate_lines or [])
    frames = extract_stack_frames(lines, top_n=top_frames)

    sig_num = None
    sig_name = None
    exit_code = None
    for fault in faults:
        if fault.get("signal") is not None and sig_num is None:
            sig_num = int(fault["signal"])
            sig_name = SIGNAL_NAMES.get(sig_num, f"SIG{sig_num}")
        if fault.get("exit_status") is not None and exit_code is None:
            exit_code = int(fault["exit_status"])
        if fault.get("exit_code") is not None and exit_code is None:
            exit_code = int(fault["exit_code"])
    if sig_num is None:
        decoded = decode_termination_signal(retcode)
        if decoded is not None and decoded[0] in CRASH_SIGNALS:
            sig_num, sig_name = decoded

    # Check sanitizer evidence
    asan_error = None
    ubsan_error = None
    asan_loc = None
    ubsan_file = None
    ubsan_line = None
    ubsan_func = None

    for line in lines:
        m_asan = ASAN_RE.search(line)
        if m_asan and asan_error is None:
            asan_error = m_asan.group("kind").strip()
        m_sum = ASAN_SUMMARY_RE.match(line.strip())
        if m_sum:
            if m_sum.group("tool") == "AddressSanitizer":
                asan_error = asan_error or m_sum.group("kind").strip()
            else:
                ubsan_error = ubsan_error or m_sum.group("kind").strip()
                if m_sum.group("func"):
                    ubsan_func = ubsan_func or m_sum.group("func").strip()
            if m_sum.group("loc"):
                loc_str = m_sum.group("loc").strip()
                parts = loc_str.split(":")
                if len(parts) >= 2 and parts[1].isdigit():
                    norm_f = normalize_source_path(parts[0], keep_src_rel=True)
                    loc_norm = f"{norm_f}:{parts[1]}"
                    if m_sum.group("func"):
                        loc_norm = f"{m_sum.group('func').strip()}@{loc_norm}"
                    asan_loc = asan_loc or loc_norm
        m_ub = UBSAN_RUNTIME_RE.match(line.strip())
        if m_ub and ubsan_error is None:
            ubsan_file = normalize_source_path(m_ub.group("file"))
            ubsan_line = int(m_ub.group("line"))
            msg = m_ub.group("msg").strip()
            ubsan_error = _normalize_volatile_tokens(msg.split(":", 1)[0].strip())

    has_assertion = any(
        f.get("kind", "").endswith(("assertion_trap", "assertion_failure"))
        for f in faults
    )
    has_internal_err = any(
        f.get("kind") in {"duckdb_internal_error", "clickhouse_logical_error"}
        for f in faults
    )
    has_panic = any(f.get("kind") == "pg_backend_panic" for f in faults)

    file_name = None
    line_num = None
    func_name = None
    assertion_expr = None
    category = None

    # Real sanitizer findings (not secondary ABRT from an already-logged assertion)
    if asan_error and not (has_assertion and asan_error in {"ABRT", "DEADLYSIGNAL"}):
        category = "asan"
        location = frames[0] if frames else asan_loc
        id_parts = ["asan", asan_error]
        if location:
            id_parts.append(location)
        sig_id = ":".join(id_parts)
    elif ubsan_error and not has_assertion:
        category = "ubsan"
        file_name = ubsan_file
        line_num = ubsan_line
        location = frames[0] if frames else (
            f"{file_name}:{line_num}" if file_name and line_num else asan_loc
        )
        id_parts = ["ubsan", ubsan_error]
        if location:
            id_parts.append(location)
        sig_id = ":".join(id_parts)
    elif has_assertion:
        category = "assertion"
        file_name, line_num, func_name, assertion_expr = _parse_assertion_details(faults)
        if sig_num is None and dbms in {"postgres", "mysql", "mariadb", "sqlite", "duckdb"}:
            sig_num = 6
            sig_name = "SIGABRT"
        id_parts = ["assertion"]
        if file_name:
            id_parts.append(file_name)
        if line_num is not None:
            id_parts.append(str(line_num))
        if func_name:
            id_parts.append(func_name)
        if assertion_expr:
            id_parts.append(assertion_expr)
        sig_id = ":".join(id_parts)
    elif has_internal_err:
        target_fault = next(
            f
            for f in faults
            if f.get("kind") in {"duckdb_internal_error", "clickhouse_logical_error"}
        )
        category = (
            "internal_error"
            if target_fault["kind"] == "duckdb_internal_error"
            else "logical_error"
        )
        raw_msg = target_fault.get("message") or target_fault.get("detail", "")
        assertion_expr = _normalize_volatile_tokens(raw_msg)
        sig_id = f"{category}:{assertion_expr}" if assertion_expr else category
    elif has_panic:
        category = "panic"
        panic_fault = next(f for f in faults if f.get("kind") == "pg_backend_panic")
        raw_msg = panic_fault.get("detail", "")
        raw_msg = re.sub(r"^PANIC:\s*", "", raw_msg)
        assertion_expr = _normalize_volatile_tokens(raw_msg)
        sig_id = f"panic:{assertion_expr}"
    elif sig_num is not None:
        category = "signal"
        if frames:
            sig_id = f"signal:{sig_name}:{'|'.join(frames)}"
        else:
            sig_id = f"signal:{sig_name}"
    else:
        exc_fault = next(
            (f for f in faults if f.get("kind") == "pg_postmaster_child_exception"),
            None,
        )
        if exc_fault is not None:
            category = "signal"
            sig_name = f"EXC_{exc_fault['exception'].upper()}"
            sig_id = f"signal:{sig_name}"
        elif any(f.get("kind") == "duckdb_database_invalidated" for f in faults):
            category = "internal_error"
            assertion_expr = "database invalidated"
            sig_id = "internal_error:database invalidated"
        elif any(f.get("kind") == "pg_postmaster_crash_recovery" for f in faults):
            category = "crash_recovery"
            sig_id = "crash_recovery:postmaster"
        else:
            category = "abnormal_exit"
            code_val = exit_code if exit_code is not None else retcode
            sig_id = f"abnormal_exit:{code_val}"

    sig_hash = hashlib.sha256(sig_id.encode("utf-8")).hexdigest()[:16]
    return {
        "id": sig_id,
        "hash": sig_hash,
        "category": category,
        "signal": sig_name,
        "signal_num": sig_num,
        "assertion": assertion_expr,
        "asan_error": asan_error or ubsan_error,
        "file": file_name,
        "line": line_num,
        "function": func_name or ubsan_func,
        "exit_code": exit_code,
        "frames": frames,
        "top_frames": top_frames,
    }


def _extract_sig_dict(data, raw):
    if isinstance(data, dict) and isinstance(data.get("crash_signature"), dict):
        data = data["crash_signature"]
    if isinstance(data, dict) and (data.get("id") or data.get("hash")):
        return {
            "raw": raw,
            "id": data.get("id"),
            "hash": data.get("hash"),
            "category": data.get("category"),
        }
    return None


def _load_baseline_signature(baseline):
    text = (baseline or "").strip()
    if not text:
        return None
    if os.path.isfile(text):
        try:
            with open(text, encoding="utf-8") as handle:
                content = handle.read()
            try:
                parsed = _extract_sig_dict(json.loads(content), baseline)
                if parsed is not None:
                    return parsed
            except json.JSONDecodeError:
                pass
            for line in content.splitlines():
                line = line.strip()
                if not line:
                    continue
                try:
                    parsed = _extract_sig_dict(json.loads(line), baseline)
                    if parsed is not None:
                        return parsed
                except json.JSONDecodeError:
                    continue
        except OSError:
            pass
    if text.startswith("{"):
        try:
            parsed = _extract_sig_dict(json.loads(text), baseline)
            if parsed is not None:
                return parsed
        except json.JSONDecodeError:
            pass
    category = text.split(":", 1)[0] if ":" in text else None
    return {"raw": text, "id": text, "hash": text, "category": category}


def evaluate_slippage(baseline, crash_signature):
    parsed = _load_baseline_signature(baseline)
    if not parsed or not crash_signature:
        return None

    obs_id = crash_signature.get("id")
    obs_hash = crash_signature.get("hash")
    obs_cat = crash_signature.get("category")

    if parsed.get("id") and obs_id == parsed["id"]:
        match_type = "exact_id"
        slipped = False
    elif parsed.get("hash") and obs_hash == parsed["hash"]:
        match_type = "exact_hash"
        slipped = False
    else:
        match_type = "mismatch"
        slipped = True

    base_cat = parsed.get("category")
    category_changed = (base_cat != obs_cat) if base_cat else None

    return {
        "baseline": parsed["raw"],
        "observed_id": obs_id,
        "observed_hash": obs_hash,
        "slipped": slipped,
        "match_type": match_type,
        "category_changed": category_changed,
    }


def signature_matches(text, signature):
    """Return (matched, regex_valid). Invalid regexes fall back to literal."""
    try:
        return bool(re.search(signature, text, re.IGNORECASE | re.DOTALL)), True
    except re.error:
        return signature.lower() in text.lower(), False


def has_psql_meta_command(sql_bytes):
    text = sql_bytes.decode("utf-8", errors="replace")
    return any(line.lstrip().startswith("\\") for line in text.splitlines())



def _default_connection_settings(dbms):
    if dbms == "mysql":
        return {
            "host": os.environ.get("MYSQL_HOST") or "127.0.0.1",
            "port": _env_int("MYSQL_TCP_PORT", 3306),
            "socket": os.environ.get("MYSQL_SOCKET", ""),
            "user": os.environ.get("MYSQL_USER") or "root",
            "password": os.environ.get("MYSQL_PWD", ""),
            "dbname": os.environ.get("MYSQL_DATABASE") or "occam_test",
            "container_log": "/var/log/mysql/error.log",
        }
    if dbms == "mariadb":
        return {
            "host": os.environ.get("MARIADB_HOST")
            or os.environ.get("MYSQL_HOST")
            or "127.0.0.1",
            "port": _env_int("MARIADB_TCP_PORT", _env_int("MYSQL_TCP_PORT", 3306)),
            "socket": os.environ.get("MARIADB_UNIX_PORT")
            or os.environ.get("MYSQL_SOCKET", ""),
            "user": os.environ.get("MARIADB_USER")
            or os.environ.get("MYSQL_USER")
            or "root",
            "password": os.environ.get("MARIADB_PWD")
            or os.environ.get("MYSQL_PWD", ""),
            "dbname": os.environ.get("MARIADB_DATABASE")
            or os.environ.get("MYSQL_DATABASE")
            or "occam_test",
            "container_log": "/var/log/mysql/error.log",
        }
    if dbms == "clickhouse":
        return {
            "host": os.environ.get("CLICKHOUSE_HOST") or "127.0.0.1",
            "port": _env_int("CLICKHOUSE_PORT", 9000),
            "socket": "",
            "user": os.environ.get("CLICKHOUSE_USER") or "default",
            "password": os.environ.get("CLICKHOUSE_PASSWORD", ""),
            "dbname": os.environ.get("CLICKHOUSE_DATABASE")
            or os.environ.get("CLICKHOUSE_DB")
            or "occam_test",
            "container_log": "/var/log/clickhouse-server/clickhouse-server.err.log",
        }
    if dbms in EMBEDDED_DBMS:
        return {
            "host": "",
            "port": 0,
            "socket": "",
            "user": "",
            "password": "",
            "dbname": ":memory:",
            "container_log": "",
        }
    return {
        "host": os.environ.get("PGHOST") or "127.0.0.1",
        "port": _env_int("PGPORT", 5432),
        "socket": "",
        "user": os.environ.get("PGUSER") or "postgres",
        "password": os.environ.get("PGPASSWORD", ""),
        "dbname": os.environ.get("PGDATABASE") or "occam_test",
        "container_log": "/var/log/postgresql/postgresql.log",
    }


def main():
    parser = argparse.ArgumentParser(
        description="Occam crash oracle: executes SQL from stdin and validates a DBMS crash."
    )
    parser.add_argument(
        "--dbms",
        choices=list(SUPPORTED_DBMS),
        default=os.environ.get("DBMS") or "postgres",
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
        help="Database port (default: 5432 postgres, 3306 mysql/mariadb, 9000 clickhouse)",
    )
    parser.add_argument(
        "--socket",
        default=None,
        help="MySQL/MariaDB unix socket path (overrides --host/--port)",
    )
    parser.add_argument(
        "--user",
        default=None,
        help="Database user (default: postgres / root / default)",
    )
    parser.add_argument(
        "--password",
        default=None,
        help="Database password (prefer PGPASSWORD / MYSQL_PWD / MARIADB_PWD / CLICKHOUSE_PASSWORD env vars)",
    )
    parser.add_argument(
        "--dbname",
        default=None,
        help="Target database name (default: occam_test; :memory: for embedded)",
    )
    parser.add_argument(
        "--engine-bin",
        default=None,
        help="Override embedded engine executable (sqlite3 / duckdb / occam-exec)",
    )
    parser.add_argument(
        "--expected-signature",
        default=os.environ.get("EXPECTED_SIGNATURE", ""),
        help="Regex or literal substring of the expected crash signature",
    )
    parser.add_argument(
        "--baseline-signature",
        default=os.environ.get("BASELINE_SIGNATURE", ""),
        help="Baseline normalized crash signature (id, hash, or JSON) for RQ3 slippage tracking",
    )
    parser.add_argument(
        "--reject-on-slippage",
        action="store_true",
        default=env_flag("ORACLE_REJECT_ON_SLIPPAGE") or bool(ORACLE_DEFAULTS.get("reject_on_slippage", False)),
        help="Reject faults whose normalized crash_signature differs from --baseline-signature",
    )
    parser.add_argument(
        "--stack-frames",
        type=int,
        default=_env_int("ORACLE_STACK_FRAMES", int(ORACLE_DEFAULTS.get("stack_frames", 5))),
        help="Maximum non-wrapper stack frames retained in crash_signature (default: 5)",
    )
    parser.add_argument(
        "--print-signature",
        action="store_true",
        default=False,
        help="Print extracted crash_signature JSON to stdout when a fault is observed",
    )
    parser.add_argument(
        "--log-file",
        default=None,
        help="Host path to the DBMS server log (for server DBMSes)",
    )
    parser.add_argument(
        "--container-name",
        default=os.environ.get("CONTAINER_NAME", ""),
        help="Docker container name (reads logs for server DBMSes; runs occam-exec for embedded)",
    )
    parser.add_argument(
        "--container-log",
        default="",
        help="Server log path inside the container (defaults per server DBMS)",
    )
    parser.add_argument(
        "--decision-log",
        default=os.environ.get("ORACLE_DECISION_LOG", ""),
        help="Append one JSON verdict record per invocation to this file",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=_env_float("ORACLE_TIMEOUT", float(ORACLE_DEFAULTS.get("timeout_secs", 0.0))),
        help=(
            "Optional client-side timeout in seconds (default: 0 = disabled). "
            "Under Occam leave this disabled and use Occam's --timeout instead."
        ),
    )
    parser.add_argument(
        "--allow-stderr-only",
        action="store_true",
        default=env_flag("ORACLE_ALLOW_STDERR_ONLY") or bool(ORACLE_DEFAULTS.get("allow_stderr_only", False)),
        help="UNSAFE: accept connection loss from stderr when no server log is configured",
    )
    parser.add_argument(
        "--allow-psql-meta",
        action="store_true",
        default=env_flag("ORACLE_ALLOW_PSQL_META") or bool(ORACLE_DEFAULTS.get("allow_psql_meta", False)),
        help="UNSAFE: allow psql backslash meta-commands in candidate SQL",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        default=env_flag("ORACLE_VERBOSE") or bool(ORACLE_DEFAULTS.get("verbose", False)),
        help="Print verbose diagnostics to stderr",
    )

    args = parser.parse_args()
    if args.timeout < 0:
        parser.error("--timeout must be non-negative")
    if args.stack_frames <= 0:
        parser.error("--stack-frames must be a positive integer")

    # Resolve DBMS-aware connection defaults.
    defaults = _default_connection_settings(args.dbms)
    if args.host is None:
        args.host = defaults["host"]
    if args.port is None:
        args.port = defaults["port"]
    if args.socket is None:
        args.socket = defaults["socket"]
    if args.user is None:
        args.user = defaults["user"]
    if args.password is None:
        args.password = defaults["password"]
    if args.dbname is None:
        args.dbname = defaults["dbname"]

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
    if args.baseline_signature:
        base_payload["baseline_signature"] = args.baseline_signature

    def finish(code, verdict, classification, reason, **extra):
        payload = dict(base_payload)
        payload.update(
            {
                "verdict": verdict,
                "classification": classification,
                "reason": reason,
            }
        )
        for k, v in extra.items():
            if v is not None or k not in {"crash_signature", "slippage"}:
                payload[k] = v
        emit_decision(decision_log, payload)
        if args.print_signature and extra.get("crash_signature") is not None:
            sys.stdout.write(
                json.dumps(extra["crash_signature"], sort_keys=True) + "\n"
            )
            sys.stdout.flush()
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

    # Build client/engine command.
    env = os.environ.copy()
    is_embedded = args.dbms in EMBEDDED_DBMS
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
    elif args.dbms in ("mysql", "mariadb"):
        if args.dbms == "mariadb":
            mysql_candidates = [
                "/usr/local/mysql/bin/mariadb",
                "/usr/local/mysql/bin/mysql",
                "/usr/local/mariadb/bin/mariadb",
                "/usr/bin/mariadb",
                "/usr/bin/mysql",
            ]
            binary = resolve_binary_any(["mariadb", "mysql"], mysql_candidates)
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
    elif args.dbms == "clickhouse":
        ch_candidates = [
            "/usr/bin/clickhouse-client",
            "/usr/local/bin/clickhouse-client",
        ]
        found_binary = shutil.which("clickhouse-client")
        if not found_binary:
            for candidate in ch_candidates:
                if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
                    found_binary = candidate
                    break
        if found_binary or not args.container_name:
            binary = found_binary or "clickhouse-client"
            cmd = [
                binary,
                "--host",
                args.host,
                "--port",
                str(args.port),
                "--user",
                args.user,
                "--database",
                args.dbname,
                "--multiquery",
                "--ignore-error",
            ]
            if args.password:
                env["CLICKHOUSE_PASSWORD"] = args.password
        else:
            cmd = ["docker", "exec", "-i"]
            if args.password:
                env["CLICKHOUSE_PASSWORD"] = args.password
                cmd.extend(["-e", "CLICKHOUSE_PASSWORD"])
            cmd.extend([
                args.container_name,
                "clickhouse-client",
                "--user",
                args.user,
                "--database",
                args.dbname,
                "--multiquery",
                "--ignore-error",
            ])
    elif args.dbms == "sqlite":
        if args.container_name:
            exec_bin = args.engine_bin or "occam-exec"
            cmd = ["docker", "exec", "-i", args.container_name, exec_bin]
        else:
            binary = (
                args.engine_bin
                or os.environ.get("SQLITE_BIN")
                or resolve_binary(
                    "sqlite3", ["/usr/local/bin/sqlite3", "/usr/bin/sqlite3"]
                )
            )
            cmd = [binary, "-batch", ":memory:"]
    else:  # duckdb
        if args.container_name:
            exec_bin = args.engine_bin or "occam-exec"
            cmd = ["docker", "exec", "-i", args.container_name, exec_bin]
        else:
            binary = (
                args.engine_bin
                or os.environ.get("DUCKDB_BIN")
                or resolve_binary(
                    "duckdb", ["/usr/local/bin/duckdb", "/usr/bin/duckdb"]
                )
            )
            cmd = [binary, "-batch", ":memory:"]

    # Resolve the log / execution mode before executing anything.
    log_mode = ""
    log_offset = 0
    log_inode = None
    internal_log = args.container_log or defaults["container_log"]

    if is_embedded:
        log_mode = "embedded_container" if args.container_name else "embedded_local"
        if log_mode == "embedded_local":
            env.setdefault(
                "ASAN_OPTIONS",
                "detect_leaks=0:abort_on_error=1:disable_coredump=1:symbolize=1",
            )
            env.setdefault(
                "UBSAN_OPTIONS",
                "halt_on_error=1:abort_on_error=1:print_stacktrace=1",
            )
    elif log_file:
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

    # Execute the candidate. No start_new_session: child processes must stay in
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

    if log_mode == "embedded_container" and retcode in (125, 126, 127):
        finish(
            1,
            "rejected",
            "missing_evidence",
            "container-exec-failed",
            container=args.container_name,
            retcode=retcode,
            stderr=stderr_text[:512],
        )

    if args.dbms == "mysql":
        conn_phrases = MYSQL_CONNECTION_LOST_PHRASES
    elif args.dbms == "mariadb":
        conn_phrases = MARIADB_CONNECTION_LOST_PHRASES
    elif args.dbms == "clickhouse":
        conn_phrases = CLICKHOUSE_CONNECTION_LOST_PHRASES
    elif args.dbms == "postgres":
        conn_phrases = PG_CONNECTION_LOST_PHRASES
    else:
        conn_phrases = []

    connection_lost = any(phrase in stderr_text for phrase in conn_phrases)
    possible_crash = connection_lost or retcode != 0

    # Collect only new server-log bytes for server DBMSes. Poll briefly when a
    # crash is plausible because logging collectors flush asynchronously.
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
                args.dbms, server_log_text, stderr_text, retcode=retcode
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
        stderr_faults, stderr_lines, _ = extract_dbms_evidence(
            args.dbms, stderr_text, stderr_text, retcode=retcode
        )
        stderr_sig = (
            extract_crash_signature(
                args.dbms,
                stderr_faults,
                stderr_lines,
                stderr_text=stderr_text,
                retcode=retcode,
                top_frames=args.stack_frames,
            )
            if stderr_faults
            else None
        )
        if args.expected_signature:
            matched, regex_valid = signature_matches(stderr_text, args.expected_signature)
            if matched:
                finish(
                    0,
                    "accepted",
                    "missing_evidence",
                    "stderr-signature-unsafe",
                    regex_valid=regex_valid,
                    crash_signature=stderr_sig,
                )
            finish(
                1,
                "rejected",
                "missing_evidence",
                "stderr-signature-mismatch",
                regex_valid=regex_valid,
                crash_signature=stderr_sig,
            )
        if connection_lost or stderr_faults:
            finish(
                0,
                "accepted",
                "missing_evidence",
                "stderr-crash-unsafe",
                fault_events=stderr_faults,
                connection_lost=connection_lost,
                crash_signature=stderr_sig,
            )
        finish(
            1,
            "rejected",
            "missing_evidence",
            "no-crash",
            connection_lost=connection_lost,
        )

    faults, candidate_lines, corroboration = extract_dbms_evidence(
        args.dbms, server_log_text, stderr_text, retcode=retcode
    )
    indicators = [f["detail"] for f in faults]
    crash_sig = (
        extract_crash_signature(
            args.dbms,
            faults,
            candidate_lines,
            stderr_text=stderr_text,
            retcode=retcode,
            top_frames=args.stack_frames,
        )
        if faults
        else None
    )
    slippage = (
        evaluate_slippage(args.baseline_signature, crash_sig)
        if (args.baseline_signature and crash_sig is not None)
        else None
    )

    if corroboration.get("oom"):
        no_crash_reason = (
            "oom-kill"
            if corroboration.get("oom_cause") == "oom_kill_signal_9"
            else "oom"
        )
    elif corroboration.get("sqlite_abort"):
        no_crash_reason = "sqlite-abort"
    elif corroboration.get("external_kill"):
        no_crash_reason = "external-kill"
    else:
        no_crash_reason = "no-crash"

    signature_found = False
    regex_valid = True
    if args.expected_signature:
        # Require at least one explicit fault event before classifying as
        # confirmed_fault; when a fault exists, allow matching its detail or
        # accompanying non-echo log/stderr lines (e.g. crash backtrace frames).
        if faults:
            scope_text = "\n".join(indicators + candidate_lines)
            signature_found, regex_valid = signature_matches(scope_text, args.expected_signature)
        else:
            _, regex_valid = signature_matches("", args.expected_signature)

    if args.expected_signature:
        if not (faults and signature_found):
            finish(
                1,
                "rejected",
                "healthy_no_fault",
                "signature-mismatch" if faults else no_crash_reason,
                indicators=indicators,
                fault_events=faults,
                corroboration=corroboration,
                connection_lost=connection_lost,
                retcode=retcode,
                regex_valid=regex_valid,
                crash_signature=crash_sig,
                slippage=slippage,
            )
        if args.reject_on_slippage and slippage and slippage["slipped"]:
            finish(
                1,
                "rejected",
                "confirmed_fault",
                "signature-slippage",
                indicators=indicators,
                fault_events=faults,
                corroboration=corroboration,
                connection_lost=connection_lost,
                retcode=retcode,
                regex_valid=regex_valid,
                crash_signature=crash_sig,
                slippage=slippage,
            )
        finish(
            0,
            "accepted",
            "confirmed_fault",
            "crash-signature",
            indicators=indicators,
            fault_events=faults,
            corroboration=corroboration,
            connection_lost=connection_lost,
            retcode=retcode,
            regex_valid=regex_valid,
            crash_signature=crash_sig,
            slippage=slippage,
        )

    if faults:
        if args.reject_on_slippage and slippage and slippage["slipped"]:
            finish(
                1,
                "rejected",
                "confirmed_fault",
                "signature-slippage",
                indicators=indicators,
                fault_events=faults,
                corroboration=corroboration,
                connection_lost=connection_lost,
                retcode=retcode,
                crash_signature=crash_sig,
                slippage=slippage,
            )
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
            crash_signature=crash_sig,
            slippage=slippage,
        )

    if connection_lost and args.allow_stderr_only and not is_embedded:
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
        no_crash_reason,
        corroboration=corroboration,
        connection_lost=connection_lost,
        retcode=retcode,
        log_mode=log_mode,
    )


if __name__ == "__main__":
    main()

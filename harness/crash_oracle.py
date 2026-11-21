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
    loss; it is never scanned for crash signatures because psql/mysql echo the
    failing statement in error messages.
  - --allow-stderr-only is available for trusted, log-less setups and is
    explicitly unsafe.

DBMS supported:
  - postgres (default)
  - mysql
"""

import argparse
import datetime
import hashlib
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

# These phrases are emitted by the client when the server connection drops.
# They are only used as corroboration, never as proof of a crash.
CONNECTION_LOST_PHRASES = [
    "server closed the connection unexpectedly",
    "connection to server was lost",
    "could not receive data from server",
    "Lost connection to MySQL server",
    "MySQL server has gone away",
]

# Server-log crash evidence. Deliberately excludes generic client phrases such
# as "Aborted" (MySQL logs "Aborted connection" for ordinary client drops).
CRASH_INDICATORS = [
    r"TRAP:\s*FailedAssertion\([^\n]+\)",
    r"server process \(PID \d+\) was terminated by signal \d+",
    r"terminating any other active server processes",
    r"PANIC:\s+[^\n]+",
    r"Assertion failure:\s+[^\n]+",
    r"Assertion `[^`]+' failed",
    r"\[ERROR\].*Fatal signal \d+",
    r"\[ERROR\].*Got signal \d+",
    r"mysqld got signal \d+",
    r"Segmentation fault",
    r"Aborted\s*\(core dumped\)",
    r"AddressSanitizer:\s+[^\n]+",
]


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


def find_indicators(text):
    return [
        match.group(0)
        for pattern in CRASH_INDICATORS
        for match in [re.search(pattern, text, re.IGNORECASE)]
        if match
    ]


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

    def finish(code, verdict, reason, **extra):
        payload = dict(base_payload)
        payload.update({"verdict": verdict, "reason": reason})
        payload.update(extra)
        emit_decision(decision_log, payload)
        if args.verbose:
            sys.stderr.write(
                f"[crash_oracle] {verdict.upper()}: {reason}"
                + (f" ({extra})" if extra else "")
                + "\n"
            )
        sys.exit(code)

    if not candidate_sql.strip():
        finish(1, "rejected", "empty-sql")

    if args.dbms == "postgres" and not args.allow_psql_meta:
        if has_psql_meta_command(candidate_sql):
            finish(1, "rejected", "psql-meta-command")

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
                finish(1, "rejected", "log-stat-failed", log_file=log_file)
        elif args.container_name:
            log_mode = "container"
        else:
            finish(
                1,
                "rejected",
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
        finish(124, "timeout", "client-timeout", timeout=args.timeout)
    except FileNotFoundError:
        finish(1, "rejected", "client-not-found", binary=cmd[0])
    except OSError as exc:
        finish(1, "rejected", "execution-error", error=str(exc))

    connection_lost = any(phrase in stderr_text for phrase in CONNECTION_LOST_PHRASES)
    possible_crash = connection_lost or retcode != 0

    # Collect only new server-log bytes. Poll briefly when a crash is plausible
    # because logging collectors flush asynchronously.
    server_log_text = ""
    log_read_ok = True
    container_stdout_read = False
    deadline = time.monotonic() + (LOG_WAIT_SECONDS if possible_crash else 0.0)

    def collect_logs():
        nonlocal log_offset, log_inode, container_stdout_read
        if log_mode == "file":
            text, log_offset, log_inode, ok = read_new_log_file(
                log_file, log_offset, log_inode
            )
            return text, ok
        if log_mode == "container":
            text, log_offset, log_inode, ok = read_container_file_log(
                args.container_name, internal_log, log_offset, log_inode
            )
            if not container_stdout_read:
                text += read_container_stdout_logs(args.container_name, since_iso)
                container_stdout_read = True
            return text, ok
        return "", True

    if log_mode in ("file", "container"):
        while True:
            chunk, ok = collect_logs()
            if chunk:
                server_log_text = (server_log_text + chunk)[-MAX_TOTAL_LOG_BYTES:]
            if not ok:
                log_read_ok = False
                break
            if args.expected_signature:
                matched, _ = signature_matches(server_log_text, args.expected_signature)
                if matched:
                    break
            elif find_indicators(server_log_text):
                break
            if time.monotonic() >= deadline:
                break
            time.sleep(LOG_POLL_INTERVAL)

        if not log_read_ok and not server_log_text:
            finish(
                1,
                "rejected",
                "log-read-failed",
                log_mode=log_mode,
                log_file=log_file or internal_log,
            )

    indicators = find_indicators(server_log_text)
    signature_found = False
    regex_valid = True
    if args.expected_signature:
        signature_found, regex_valid = signature_matches(
            server_log_text if log_mode != "stderr" else stderr_text,
            args.expected_signature,
        )

    if log_mode == "stderr":
        stderr_indicators = find_indicators(stderr_text)
        if args.expected_signature:
            if signature_found:
                finish(
                    0,
                    "accepted",
                    "stderr-signature-unsafe",
                    regex_valid=regex_valid,
                )
            finish(
                1,
                "rejected",
                "stderr-signature-mismatch",
                regex_valid=regex_valid,
            )
        if connection_lost or stderr_indicators:
            finish(
                0,
                "accepted",
                "stderr-crash-unsafe",
                indicators=stderr_indicators,
                connection_lost=connection_lost,
            )
        finish(1, "rejected", "no-crash", connection_lost=connection_lost)

    if args.expected_signature:
        if signature_found:
            finish(
                0,
                "accepted",
                "crash-signature",
                indicators=indicators,
                connection_lost=connection_lost,
                regex_valid=regex_valid,
            )
        finish(
            1,
            "rejected",
            "signature-mismatch" if indicators else "no-crash",
            indicators=indicators,
            connection_lost=connection_lost,
            retcode=retcode,
            regex_valid=regex_valid,
        )

    if indicators:
        finish(
            0,
            "accepted",
            "crash-indicator",
            indicators=indicators,
            connection_lost=connection_lost,
            retcode=retcode,
        )

    if connection_lost and args.allow_stderr_only:
        finish(
            0,
            "accepted",
            "connection-loss-stderr-unsafe",
            connection_lost=True,
            retcode=retcode,
        )

    finish(
        1,
        "rejected",
        "no-crash",
        connection_lost=connection_lost,
        retcode=retcode,
        log_mode=log_mode,
    )


if __name__ == "__main__":
    main()

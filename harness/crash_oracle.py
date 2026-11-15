#!/usr/bin/env python3
"""
crash_oracle.py - Crash interestingness oracle for Occam multi-statement reduction.

Occam Contract:
  - Candidate SQL is piped to sys.stdin.
  - Exit 0: Interesting (target DBMS crashed with matching signature).
  - Exit 1: Rejected (normal execution, standard SQL error, or mismatched signature).
  - Exit 124: Timeout (query hung / exceeded timeout limit).

DBMS Supported:
  - postgres (default)
  - mysql
"""

import argparse
import datetime
import os
import re
import shutil
import signal
import subprocess
import sys
import time

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
    r"Aborted\s*(?:\(core dumped\))?",
    r"AddressSanitizer:\s+[^\n]+",
    r"server closed the connection unexpectedly",
    r"connection to server was lost",
    r"could not receive data from server",
    r"Lost connection to MySQL server",
    r"MySQL server has gone away",
]


def resolve_binary(name, candidate_paths=None):
    found = shutil.which(name)
    if found:
        return found
    if candidate_paths:
        for p in candidate_paths:
            if os.path.isfile(p) and os.access(p, os.X_OK):
                return p
    return name


def get_log_offset(log_file):
    if log_file and os.path.isfile(log_file):
        try:
            return os.path.getsize(log_file)
        except OSError:
            return 0
    return 0


def read_new_log(log_file, offset):
    if log_file and os.path.isfile(log_file):
        try:
            with open(log_file, "r", errors="replace") as f:
                f.seek(offset)
                return f.read()
        except OSError as e:
            sys.stderr.write(f"[crash_oracle] Warning: failed to read {log_file}: {e}\n")
    return ""


def get_container_log_offset(container_name, internal_log="/var/log/postgresql/postgresql.log"):
    try:
        res = subprocess.run(
            ["docker", "exec", container_name, "stat", "-c", "%s", internal_log],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=3,
        )
        if res.returncode == 0 and res.stdout.strip().isdigit():
            return int(res.stdout.strip())
    except Exception:
        pass
    return 0


def read_container_logs(container_name, since_iso, log_offset=0, internal_log="/var/log/postgresql/postgresql.log"):
    file_log = ""
    if log_offset >= 0:
        try:
            cmd = ["docker", "exec", container_name, "tail", "-c", f"+{log_offset + 1}", internal_log]
            res = subprocess.run(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                timeout=5,
            )
            if res.returncode == 0:
                file_log = res.stdout or ""
        except Exception:
            pass

    docker_log = ""
    try:
        res = subprocess.run(
            ["docker", "logs", "--since", since_iso, container_name],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=5,
        )
        docker_log = (res.stdout or "") + "\n" + (res.stderr or "")
    except Exception as e:
        sys.stderr.write(f"[crash_oracle] Warning: docker logs failed: {e}\n")

    return f"{file_log}\n{docker_log}"


def main():
    parser = argparse.ArgumentParser(
        description="Occam crash oracle: executes SQL from stdin and validates crash signature."
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
        "--user",
        default=None,
        help="Database user (default: postgres / root)",
    )
    parser.add_argument(
        "--password",
        default=None,
        help="Database password",
    )
    parser.add_argument(
        "--dbname",
        default=None,
        help="Target database name (default: occam_test)",
    )
    parser.add_argument(
        "--expected-signature",
        default=os.environ.get("EXPECTED_SIGNATURE", ""),
        help="Regex or substring of expected assertion failure / crash signature",
    )
    parser.add_argument(
        "--log-file",
        default=None,
        help="Path to database server log file",
    )
    parser.add_argument(
        "--container-name",
        default=os.environ.get("CONTAINER_NAME", ""),
        help="Docker container name to inspect logs if running externally",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=float(os.environ.get("ORACLE_TIMEOUT", "10.0")),
        help="Query execution timeout in seconds (default: 10.0, exit 124 on timeout)",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        default=bool(os.environ.get("ORACLE_VERBOSE", "")),
        help="Print verbose diagnostics to stderr",
    )

    args = parser.parse_args()

    # Resolve dbms-aware connection defaults
    if args.host is None:
        args.host = os.environ.get("MYSQL_HOST", "127.0.0.1") if args.dbms == "mysql" else os.environ.get("PGHOST", "127.0.0.1")
    if args.port is None:
        args.port = int(os.environ.get("MYSQL_TCP_PORT", 3306)) if args.dbms == "mysql" else int(os.environ.get("PGPORT", 5432))
    if args.user is None:
        args.user = os.environ.get("MYSQL_USER", "root") if args.dbms == "mysql" else os.environ.get("PGUSER", "postgres")
    if args.password is None:
        args.password = os.environ.get("MYSQL_PWD", "") if args.dbms == "mysql" else os.environ.get("PGPASSWORD", "")
    if args.dbname is None:
        args.dbname = os.environ.get("MYSQL_DATABASE", "occam_test") if args.dbms == "mysql" else os.environ.get("PGDATABASE", "occam_test")
    if args.log_file is None:
        args.log_file = os.environ.get("DBMS_LOG_FILE", "")

    # Read candidate SQL from stdin
    candidate_sql = sys.stdin.read()
    if not candidate_sql.strip():
        if args.verbose:
            sys.stderr.write("[crash_oracle] Empty SQL input, rejecting\n")
        sys.exit(1)

    # Prepare log offset or container timestamp
    log_offset = 0
    container_offset = 0
    since_iso = ""
    local_log_usable = False
    internal_log = args.log_file if args.log_file else (
        "/var/log/postgresql/postgresql.log" if args.dbms == "postgres" else "/var/log/mysql/error.log"
    )

    if args.log_file and os.path.isfile(args.log_file):
        try:
            with open(args.log_file, "r", errors="replace") as f:
                pass
            log_offset = get_log_offset(args.log_file)
            local_log_usable = True
        except (OSError, PermissionError):
            local_log_usable = False

    if args.container_name:
        since_iso = datetime.datetime.now(datetime.timezone.utc).isoformat()
        if not local_log_usable:
            container_offset = get_container_log_offset(args.container_name, internal_log)
    # Build client command
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
            "-h", args.host,
            "-p", str(args.port),
            "-U", args.user,
            "-d", args.dbname,
            "-v", "ON_ERROR_STOP=0",
            "-X",
            "-q",
        ]
        if args.password:
            env["PGPASSWORD"] = args.password
    else:  # mysql
        mysql_candidates = [
            "/usr/local/mysql/bin/mysql",
            "/usr/bin/mysql",
        ]
        binary = resolve_binary("mysql", mysql_candidates)
        cmd = [
            binary,
            "-h", args.host,
            "-P", str(args.port),
            "-u", args.user,
            "-D", args.dbname,
            "--force",
            "-s",
        ]
        if args.password:
            cmd.append(f"-p{args.password}")

    # Execute candidate SQL
    client_stdout = ""
    client_stderr = ""
    retcode = 0
    try:
        proc = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            env=env,
            start_new_session=True,
        )
        client_stdout, client_stderr = proc.communicate(input=candidate_sql, timeout=args.timeout)
        retcode = proc.returncode
    except subprocess.TimeoutExpired:
        try:
            os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
        except OSError:
            pass
        try:
            proc.communicate(timeout=1.0)
        except Exception:
            pass
        if args.verbose:
            sys.stderr.write(f"[crash_oracle] TIMEOUT: Query exceeded {args.timeout}s\n")
        sys.exit(124)
    except FileNotFoundError:
        sys.stderr.write(f"[crash_oracle] Error: DBMS client binary '{cmd[0]}' not found\n")
        sys.exit(1)
    except Exception as e:
        sys.stderr.write(f"[crash_oracle] Unexpected execution error: {e}\n")
        sys.exit(1)

    # Allow brief window for server logging collector / postmaster signal processing
    connection_lost = any(
        phrase in client_stderr
        for phrase in [
            "server closed the connection unexpectedly",
            "connection to server was lost",
            "could not receive data from server",
            "Lost connection to MySQL server",
            "MySQL server has gone away",
        ]
    )
    if connection_lost or retcode != 0:
        time.sleep(0.1)

    # Collect newly appended server log entries
    new_log = ""
    if local_log_usable:
        new_log = read_new_log(args.log_file, log_offset)
    elif args.container_name:
        new_log = read_container_logs(args.container_name, since_iso, container_offset, internal_log)

    combined_text = f"{new_log}\n{client_stderr}"

    # Determine if crash / assertion failure occurred
    found_crash_indicators = []
    for pat in CRASH_INDICATORS:
        m = re.search(pat, combined_text, re.IGNORECASE)
        if m:
            found_crash_indicators.append(m.group(0))

    has_crashed = bool(found_crash_indicators) or connection_lost

    if args.verbose:
        sys.stderr.write(
            f"[crash_oracle] Client retcode={retcode}, connection_lost={connection_lost}, "
            f"crashed={has_crashed}, indicators={found_crash_indicators}\n"
        )

    # If normal execution without crash
    if not has_crashed:
        if args.verbose:
            sys.stderr.write("[crash_oracle] REJECT: Server did not crash\n")
        sys.exit(1)

    # Signature matching
    if args.expected_signature:
        sig = args.expected_signature
        matched = False
        try:
            if re.search(sig, combined_text, re.IGNORECASE | re.DOTALL):
                matched = True
        except re.error:
            pass

        if not matched and sig.lower() in combined_text.lower():
            matched = True

        if matched:
            if args.verbose:
                sys.stderr.write(f"[crash_oracle] ACCEPT: Crash signature matched '{sig}'\n")
            sys.exit(0)
        else:
            if args.verbose:
                sys.stderr.write(
                    f"[crash_oracle] REJECT: Bug slippage (crashed but signature '{sig}' not matched in log)\n"
                )
            sys.exit(1)
    else:
        # No specific signature required; any crash accepted
        if args.verbose:
            sys.stderr.write("[crash_oracle] ACCEPT: Crash detected\n")
        sys.exit(0)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Adapter to pinned SQLreduce for fixed-setup, single PostgreSQL SELECT cases."""

from __future__ import annotations

import argparse
import json
import os
import shlex
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

PIN = "568c8a9a31601ead7128d0b067658b4d5a26821d"
UNSUPPORTED = 78
INVALID_RESULT = 77


def unsupported(message: str) -> int:
    print(message, file=sys.stderr)
    return UNSUPPORTED


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", required=True, type=Path)
    parser.add_argument("--setup", required=True, type=Path)
    parser.add_argument("--query", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--metrics", required=True, type=Path)
    parser.add_argument("--expected-sqlstate", required=True)
    parser.add_argument("--database", default=os.environ.get("SQLREDUCE_DATABASE", ""))
    parser.add_argument("--timeout", required=True, type=float)
    parser.add_argument("--reset-command", default=os.environ.get("SQLREDUCE_RESET_COMMAND", ""))
    args = parser.parse_args()

    if not args.database:
        return unsupported("SQLreduce requires SQLREDUCE_DATABASE for PostgreSQL")
    if not args.setup.is_file() or not args.query.is_file():
        return unsupported("SQLreduce fixed-setup track requires readable setup and query files")
    if not args.reset_command:
        return unsupported("SQLreduce fixed-setup track requires SQLREDUCE_RESET_COMMAND")
    try:
        revision = subprocess.run(
            ["git", "-C", str(args.source), "rev-parse", "HEAD"],
            check=True,
            capture_output=True,
            text=True,
        ).stdout.strip()
    except (OSError, subprocess.CalledProcessError) as exc:
        return unsupported(f"Cannot verify pinned SQLreduce source: {exc}")
    if revision != PIN:
        return unsupported(f"SQLreduce source must be pinned at {PIN}, found {revision}")

    sys.path.insert(0, str(args.source))
    try:
        import pglast
        import sqlreduce
        from pglast.parser import parse_sql
    except ImportError as exc:
        return unsupported(f"SQLreduce requires its pinned Python dependencies: {exc}")
    if Path(sqlreduce.__file__).resolve().parent != (args.source / "sqlreduce").resolve():
        return unsupported("Imported SQLreduce module is not from the pinned source checkout")

    query = args.query.read_text(encoding="utf-8")
    try:
        parsed = parse_sql(query)
    except Exception as exc:
        return unsupported(f"SQLreduce could not parse the PostgreSQL query: {exc}")
    if len(parsed) != 1 or parsed[0].stmt.__class__.__name__ != "SelectStmt":
        return unsupported("SQLreduce track supports exactly one PostgreSQL SELECT/CTE query, not multi-statement or stateful input")
    if not args.expected_sqlstate or len(args.expected_sqlstate) != 5:
        return unsupported("SQLreduce track requires the expected five-character PostgreSQL SQLSTATE")

    try:
        reset_command = shlex.split(args.reset_command)
        if not reset_command:
            return unsupported("SQLREDUCE_RESET_COMMAND is empty")
        reset_start = time.monotonic()
        subprocess.run(reset_command, check=True, timeout=args.timeout)
        setup_start = time.monotonic()
        subprocess.run(
            ["psql", "-X", "-v", "ON_ERROR_STOP=1", "-d", args.database, "-f", str(args.setup)],
            check=True,
            timeout=args.timeout,
            capture_output=True,
            text=True,
        )
        setup_seconds = time.monotonic() - setup_start

        timeout_text = f"{args.timeout:g}s"
        connection_args = args.database
        if "connect_timeout=" not in connection_args:
            connection_args = f"{connection_args} connect_timeout={max(1, int(args.timeout))}"
        state: dict[str, Any] = {
            "database": connection_args,
            "timeout": timeout_text,
            "use_sqlstate": True,
        }
        observed = sqlreduce.run_query(state, query)
        if observed == "CRASH" or observed == "no error":
            return unsupported("SQLreduce cannot preserve crash/reconnect behavior; input must produce a PostgreSQL SQLSTATE")
        if observed != args.expected_sqlstate:
            args.metrics.write_text(
                json.dumps({"status": "invalid-result", "expected_sqlstate": args.expected_sqlstate,
                            "observed_sqlstate": observed, "reset_time_sec": time.monotonic() - reset_start,
                            "setup_time_sec": setup_seconds}) + "\n",
                encoding="utf-8",
            )
            return INVALID_RESULT

        reduction_start = time.monotonic()
        reduced_query, result_state = sqlreduce.run_reduce(
            query,
            database=connection_args,
            verbose=False,
            use_sqlstate=True,
            timeout=timeout_text,
        )
        reduction_seconds = time.monotonic() - reduction_start
        result_state = result_state or {}
        if result_state.get("expected_error") != args.expected_sqlstate:
            return INVALID_RESULT
        args.output.write_text(reduced_query.rstrip() + "\n", encoding="utf-8")
        args.metrics.write_text(
            json.dumps({"status": "completed", "oracle_calls": result_state.get("called"),
                        "expected_sqlstate": args.expected_sqlstate,
                        "reset_time_sec": time.monotonic() - reset_start,
                        "setup_time_sec": setup_seconds, "reduction_time_sec": reduction_seconds,
                        "reduction_track": "fixed_setup_query"}) + "\n",
            encoding="utf-8",
        )
        return 0
    except subprocess.TimeoutExpired:
        return 124
    except (OSError, subprocess.CalledProcessError) as exc:
        print(f"SQLreduce setup failed: {exc}", file=sys.stderr)
        return 1
    except Exception as exc:
        print(f"SQLreduce adapter failed: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())

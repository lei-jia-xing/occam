#!/usr/bin/env python3
"""C-Reduce and upstream callback with shared uncached verification and checkpoints."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import signal
import subprocess
import sys
import time
from pathlib import Path
from typing import Any


def append_record(path: Path, record: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as output:
        output.write(json.dumps(record, sort_keys=True) + "\n")



def read_jsonl_since(path: Path, offset: int) -> list[dict[str, Any]]:
    records: list[dict[str, Any]] = []
    try:
        with path.open("rb") as source:
            source.seek(offset)
            for line in source:
                try:
                    value = json.loads(line)
                    if isinstance(value, dict):
                        records.append(value)
                except json.JSONDecodeError:
                    continue
    except OSError:
        pass
    return records

def kill_group(process: subprocess.Popen[str]) -> None:
    try:
        os.killpg(process.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    process.communicate()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--candidate", type=Path)
    parser.add_argument("candidate_arg", nargs="?", type=Path)
    parser.add_argument("--checkpoint", required=True, type=Path)
    parser.add_argument("--calls", required=True, type=Path)
    parser.add_argument("--cache", required=True, type=Path)
    parser.add_argument("--timeout", required=True, type=float)
    parser.add_argument("--deadline", required=True, type=float)
    parser.add_argument("--oracle", required=True)
    parser.add_argument("--oracle-audit", type=Path)
    parser.add_argument("--max-executions", type=int)
    parser.add_argument("--no-cache", action="store_true")
    args = parser.parse_args()
    args.candidate = args.candidate or args.candidate_arg
    if args.candidate is None:
        parser.error("candidate SQL file is required")

    digest = hashlib.sha256(args.candidate.read_bytes()).hexdigest()
    try:
        cache = json.loads(args.cache.read_text(encoding="utf-8"))
        if not isinstance(cache, dict):
            cache = {}
    except (OSError, json.JSONDecodeError):
        cache = {}
    if not args.no_cache and digest in cache:
        status = cache[digest]
        append_record(args.calls, {
            "digest": digest, "cache_hit": True, "status": status, "duration_sec": 0,
        })
        return 0 if status == "accepted" else 1

    call_records: list[dict[str, Any]] = []
    if args.calls.is_file():
        for line in args.calls.read_text(encoding="utf-8", errors="replace").splitlines():
            try:
                value = json.loads(line)
                if isinstance(value, dict):
                    call_records.append(value)
            except json.JSONDecodeError:
                pass
    actual_calls = sum(
        not row.get("cache_hit") and row.get("status") in {"accepted", "rejected", "timeout", "tool-error"}
        for row in call_records
    )
    if args.max_executions is not None and actual_calls >= args.max_executions:
        append_record(args.calls, {
            "digest": digest, "cache_hit": False, "status": "execution-limit", "duration_sec": 0,
        })
        return 126

    audit_offset = 0
    if args.oracle_audit is not None:
        try:
            audit_offset = args.oracle_audit.stat().st_size
        except OSError:
            pass

    remaining = args.deadline - time.monotonic()
    if remaining <= 0:
        status = "timeout"
        elapsed = 0.0
        stdout = ""
        stderr = "total tool budget exhausted"
    else:
        command = ["bash", "-c", args.oracle + ' "$1"', "oracle",
                   str(args.candidate.resolve())]
        def interrupt(_signal, _frame):
            raise InterruptedError("predicate checker interrupted")

        signal.signal(signal.SIGTERM, interrupt)
        started = time.monotonic()
        try:
            process = subprocess.Popen(
                command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, start_new_session=True,
            )
            try:
                stdout, stderr = process.communicate(timeout=min(args.timeout, remaining))
                status = "accepted" if process.returncode == 0 else "rejected"
            except (subprocess.TimeoutExpired, InterruptedError) as exc:
                signal.signal(signal.SIGTERM, signal.SIG_IGN)
                stdout = (getattr(exc, "stdout", None) or b"")
                stderr = (getattr(exc, "stderr", None) or b"")
                stdout = stdout.decode(errors="replace") if isinstance(stdout, bytes) else stdout
                stderr = stderr.decode(errors="replace") if isinstance(stderr, bytes) else stderr
                kill_group(process)
                status = "timeout"
        except OSError as exc:
            status = "tool-error"
            stdout = ""
            stderr = str(exc)
        elapsed = time.monotonic() - started
        if args.oracle_audit is not None:
            wrapper_record = next(
                (
                    row
                    for row in reversed(read_jsonl_since(args.oracle_audit, audit_offset))
                    if row.get("phase") == "reducer" and row.get("digest") == digest
                ),
                None,
            )
            if status not in {"timeout", "tool-error"}:
                if wrapper_record is None or wrapper_record.get("status") not in {
                    "accepted", "rejected", "timeout", "interrupted", "tool-error"
                }:
                    status = "tool-error"
                elif wrapper_record["status"] in {"timeout", "interrupted"}:
                    status = "timeout"
                elif wrapper_record["status"] == "tool-error":
                    status = "tool-error"

    if status == "accepted" and time.monotonic() >= args.deadline:
        status = "timeout"

    if status in {"accepted", "rejected"} and not args.no_cache:
        cache[digest] = status
        args.cache.write_text(json.dumps(cache, sort_keys=True), encoding="utf-8")
    if status == "accepted":
        temporary = args.checkpoint.with_suffix(args.checkpoint.suffix + ".tmp")
        temporary.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(args.candidate, temporary)
        os.replace(temporary, args.checkpoint)
    append_record(args.calls, {
        "digest": digest,
        "cache_hit": False,
        "status": status,
        "duration_sec": round(elapsed, 6),
        "stdout": stdout[-8192:],
        "stderr": stderr[-8192:],
    })
    return {"accepted": 0, "rejected": 1, "timeout": 124, "tool-error": 125}[status]


if __name__ == "__main__":
    raise SystemExit(main())

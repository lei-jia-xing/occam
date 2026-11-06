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
        return 1

    remaining = args.deadline - time.monotonic()
    if remaining <= 0:
        status = "timeout"
        elapsed = 0.0
        stdout = ""
        stderr = "total tool budget exhausted"
    else:
        command = ["bash", "-c", args.oracle + ' "$1"', "oracle",
                   str(args.candidate.resolve())]
        started = time.monotonic()
        try:
            process = subprocess.Popen(
                command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, start_new_session=True,
            )
            try:
                stdout, stderr = process.communicate(timeout=min(args.timeout, remaining))
                if "OCCAM_INFRASTRUCTURE:" in stdout or "OCCAM_INFRASTRUCTURE:" in stderr:
                    status = "tool-error"
                else:
                    status = "accepted" if process.returncode == 0 else "rejected"
            except subprocess.TimeoutExpired as exc:
                stdout = (exc.stdout or b"").decode(errors="replace") if isinstance(exc.stdout, bytes) else (exc.stdout or "")
                stderr = (exc.stderr or b"").decode(errors="replace") if isinstance(exc.stderr, bytes) else (exc.stderr or "")
                kill_group(process)
                status = "timeout"
        except OSError as exc:
            status = "tool-error"
            stdout = ""
            stderr = str(exc)
        elapsed = time.monotonic() - started

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
    return 0 if status == "accepted" else 1


if __name__ == "__main__":
    raise SystemExit(main())

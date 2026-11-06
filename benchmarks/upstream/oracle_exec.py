#!/usr/bin/env python3
"""Run the configured SQL oracle and append its raw observation to a JSONL audit."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shlex
import signal
import subprocess
import sys
import time
import tempfile
from pathlib import Path
from typing import Any

def interrupted(signum: int, _frame: Any) -> None:
    raise SystemExit(128 + signum)



def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--command", required=True, help="Oracle shell command; SQL is supplied on stdin")
    parser.add_argument("--audit", required=True, type=Path)
    parser.add_argument("--timeout", required=True, type=float)
    parser.add_argument("--cwd", required=True, type=Path)
    parser.add_argument("--reset", help="Reset command run before each candidate in the same workdir")
    parser.add_argument("--scenario")
    parser.add_argument("candidate", nargs="?", type=Path)
    args = parser.parse_args()
    signal.signal(signal.SIGTERM, interrupted)
    sql = args.candidate.read_bytes() if args.candidate else sys.stdin.buffer.read()
    started = time.monotonic()
    status = "tool-error"
    returncode: int | None = None
    stdout = ""
    stderr = ""
    with tempfile.TemporaryDirectory(prefix="occam-predicate-") as work_dir:
        candidate = Path(work_dir) / "candidate.sql"
        candidate.write_bytes(sql)
        command = args.command
        if args.scenario:
            command += " " + shlex.quote(args.scenario)
        command = command.replace("{input}", shlex.quote(str(candidate)))
        if args.reset:
            command = (
                f"({args.reset}) </dev/null; reset_status=$?; "
                "if [ \"$reset_status\" -ne 0 ]; then "
                "echo 'OCCAM_INFRASTRUCTURE: reset failed' >&2; exit 125; fi; "
                + command
            )
        environment = os.environ | {
            "OCCAM_WORKDIR": work_dir,
            "OCCAM_ORIGINAL_CWD": str(args.cwd.resolve()),
            "TMPDIR": work_dir,
            "TMP": work_dir,
            "TEMP": work_dir,
        }
        try:
            process = subprocess.Popen(
                ["bash", "-c", command], cwd=args.cwd.resolve(), env=environment,
                stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                start_new_session=True,
            )
            try:
                out, err = process.communicate(input=sql, timeout=args.timeout)
                returncode = process.returncode
                status = "accepted" if returncode == 0 else "rejected"
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                out, err = process.communicate()
                returncode = 124
                status = "timeout"
            except BaseException:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                process.communicate()
                raise
            finally:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            stdout = out.decode(errors="replace")
            stderr = err.decode(errors="replace")
            if "OCCAM_INFRASTRUCTURE:" in stdout or "OCCAM_INFRASTRUCTURE:" in stderr:
                status = "tool-error"
                returncode = 125
        except OSError as exc:
            stderr = str(exc)
            stderr = "OCCAM_INFRASTRUCTURE: " + stderr
            returncode = 125

    record: dict[str, Any] = {
        "digest": hashlib.sha256(sql).hexdigest(),
        "status": status,
        "returncode": returncode,
        "duration_sec": round(time.monotonic() - started, 6),
        "stdout": stdout[-8192:],
        "stderr": stderr[-8192:],
    }
    args.audit.parent.mkdir(parents=True, exist_ok=True)
    with args.audit.open("a", encoding="utf-8") as audit:
        audit.write(json.dumps(record, sort_keys=True) + "\n")
    sys.stdout.write(stdout)
    sys.stderr.write(stderr)
    return returncode if returncode is not None and 0 <= returncode <= 255 else 1


if __name__ == "__main__":
    raise SystemExit(main())

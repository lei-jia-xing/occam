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
import threading
import time
import tempfile
from pathlib import Path
from typing import Any

OUTPUT_CHUNK_BYTES = 32768
AUDIT_TAIL_BYTES = 8192
DIAGNOSTIC_BYTES = 8192

INFRASTRUCTURE_MARKER = b"OCCAM_INFRASTRUCTURE:"


class InterruptedCall(Exception):
    def __init__(self, signum: int):
        self.signum = signum


def interrupted(signum: int, _frame: Any) -> None:
    raise InterruptedCall(signum)


def kill_group(process: subprocess.Popen[bytes]) -> None:
    try:
        if os.name == "posix":
            os.killpg(process.pid, signal.SIGKILL)
        else:
            process.kill()
    except OSError:
        pass


def append_tail(tail: bytearray, chunk: bytes) -> None:
    tail.extend(chunk)
    del tail[:-AUDIT_TAIL_BYTES]


def append_diagnostic(diagnostic: bytearray, chunk: bytes) -> bool:
    newline = chunk.find(b"\n")
    complete = newline >= 0
    if complete:
        chunk = chunk[: newline + 1]
    remaining = max(0, DIAGNOSTIC_BYTES - len(diagnostic))
    diagnostic.extend(chunk[:remaining])
    return complete or len(diagnostic) >= DIAGNOSTIC_BYTES


def run_command(
    command: str,
    cwd: Path,
    environment: dict[str, str],
    deadline: float,
    sql: bytes | None,
) -> dict[str, Any]:
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        return {
            "status": "timeout",
            "returncode": 124,
            "stdout": b"",
            "stderr": b"",
            "stdout_diagnostic": None,
            "stderr_diagnostic": None,
            "infrastructure_marker": False,
        }
    try:
        process = subprocess.Popen(
            ["bash", "-c", command],
            cwd=cwd,
            env=environment,
            stdin=subprocess.PIPE if sql is not None else subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            start_new_session=(os.name == "posix"),
        )
    except OSError as exc:
        return {
            "status": "tool-error",
            "returncode": None,
            "stdout": b"",
            "stderr": str(exc).encode(),
            "stdout_diagnostic": None,
            "stderr_diagnostic": None,
            "infrastructure_marker": False,
        }

    def new_capture() -> dict[str, Any]:
        return {
            "tail": bytearray(),
            "marker": False,
            "diagnostic": bytearray(),
            "diagnostic_complete": False,
            "error": None,
            "lock": threading.Lock(),
        }

    stdout_capture = new_capture()
    stderr_capture = new_capture()

    def drain_output(source: Any, capture: dict[str, Any]) -> None:
        pending = b""
        try:
            while True:
                chunk = source.read1(OUTPUT_CHUNK_BYTES)
                if not chunk:
                    break
                with capture["lock"]:
                    append_tail(capture["tail"], chunk)
                    if not capture["marker"]:
                        scanned = pending + chunk
                        marker_index = scanned.find(INFRASTRUCTURE_MARKER)
                        if marker_index >= 0:
                            capture["marker"] = True
                            capture["diagnostic_complete"] = append_diagnostic(
                                capture["diagnostic"],
                                scanned[marker_index:],
                            )
                        else:
                            pending = scanned[-(len(INFRASTRUCTURE_MARKER) - 1) :]
                    elif not capture["diagnostic_complete"]:
                        capture["diagnostic_complete"] = append_diagnostic(capture["diagnostic"], chunk)
        except Exception as exc:
            with capture["lock"]:
                capture["error"] = exc
            kill_group(process)
        finally:
            try:
                source.close()
            except OSError:
                pass

    workers = [
        threading.Thread(target=drain_output, args=(process.stdout, stdout_capture), daemon=True),
        threading.Thread(target=drain_output, args=(process.stderr, stderr_capture), daemon=True),
    ]
    writer_error: list[Exception] = []
    writer_thread: threading.Thread | None = None
    if sql is not None:
        assert process.stdin is not None
        input_stream = process.stdin

        def write_input() -> None:
            try:
                input_stream.write(sql)
                input_stream.flush()
            except BrokenPipeError:
                pass
            except Exception as exc:
                writer_error.append(exc)
                kill_group(process)
            finally:
                try:
                    input_stream.close()
                except OSError:
                    pass

        writer_thread = threading.Thread(target=write_input, daemon=True)
        workers.append(writer_thread)

    timed_out = False
    cancelled = False
    try:
        for worker in workers:
            worker.start()
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            timed_out = True
        else:
            try:
                process.wait(timeout=remaining)
            except subprocess.TimeoutExpired:
                timed_out = True
    except BaseException:
        cancelled = True
        raise
    finally:
        kill_group(process)
        if process.poll() is None:
            try:
                wait_timeout = 0.0 if cancelled else max(0.0, deadline - time.monotonic())
                process.wait(timeout=wait_timeout)
            except subprocess.TimeoutExpired:
                timed_out = True
        join_deadline = time.monotonic() if cancelled else deadline
        for worker in workers:
            if worker.ident is not None:
                worker.join(timeout=max(0.0, join_deadline - time.monotonic()))
        if not cancelled and any(worker.is_alive() for worker in workers):
            timed_out = True

    with stdout_capture["lock"]:
        stdout_tail = bytes(stdout_capture["tail"])
        stdout_marker = stdout_capture["marker"]
        stdout_message = bytes(stdout_capture["diagnostic"]) if stdout_marker else None
        stdout_error = stdout_capture["error"]
    with stderr_capture["lock"]:
        stderr_tail = bytes(stderr_capture["tail"])
        stderr_marker = stderr_capture["marker"]
        stderr_message = bytes(stderr_capture["diagnostic"]) if stderr_marker else None
        stderr_error = stderr_capture["error"]
    if stdout_error is not None:
        raise stdout_error
    if stderr_error is not None:
        raise stderr_error
    if writer_thread is not None and not writer_thread.is_alive() and writer_error:
        raise writer_error[0]
    if writer_thread is not None and writer_thread.is_alive():
        timed_out = True
    if not cancelled and time.monotonic() > deadline:
        timed_out = True
    return {
        "status": "timeout" if timed_out else "completed",
        "returncode": 124 if timed_out else process.returncode,
        "stdout": stdout_tail,
        "stderr": stderr_tail,
        "stdout_diagnostic": stdout_message,
        "stderr_diagnostic": stderr_message,
        "infrastructure_marker": stdout_marker or stderr_marker,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--command", required=True, help="Oracle shell command; SQL is supplied on stdin")
    parser.add_argument("--audit", required=True, type=Path)
    parser.add_argument("--timeout", required=True, type=float)
    parser.add_argument("--cwd", required=True, type=Path)
    parser.add_argument("--reset", help="Reset command run before each candidate in the same workdir")
    parser.add_argument("--scenario")
    parser.add_argument("--phase", choices=("validation", "reducer"), default="reducer")
    parser.add_argument("candidate", nargs="?", type=Path)
    args = parser.parse_args()
    signal.signal(signal.SIGTERM, interrupted)
    sql = args.candidate.read_bytes() if args.candidate else sys.stdin.buffer.read()
    started = time.monotonic()
    deadline = started + args.timeout
    status = "tool-error"
    returncode: int | None = None
    stdout = bytearray()
    stderr = bytearray()
    infrastructure_marker = False
    stdout_diagnostic: bytes | None = None
    stderr_diagnostic: bytes | None = None

    reset_attempted = False
    reset_elapsed_sec: float | None = None
    reset_status: str | None = None
    reset_started: float | None = None
    try:
        with tempfile.TemporaryDirectory(prefix="occam-predicate-") as work_dir:
            candidate = Path(work_dir) / "candidate.sql"
            candidate.write_bytes(sql)
            command = args.command
            if args.scenario:
                command += " " + shlex.quote(args.scenario)
            command = command.replace("{input}", shlex.quote(str(candidate)))
            environment = os.environ | {
                "OCCAM_WORKDIR": work_dir,
                "OCCAM_ORIGINAL_CWD": str(args.cwd.resolve()),
                "TMPDIR": work_dir,
                "TMP": work_dir,
                "TEMP": work_dir,
            }

            if args.reset:
                reset_attempted = True
                reset_started = time.monotonic()
                try:
                    reset = run_command(args.reset, args.cwd.resolve(), environment, deadline, None)
                finally:
                    reset_elapsed_sec = round(time.monotonic() - reset_started, 6)
                reset_status = reset["status"]
                append_tail(stdout, reset["stdout"])
                append_tail(stderr, reset["stderr"])
                stdout_diagnostic = reset["stdout_diagnostic"]
                stderr_diagnostic = reset["stderr_diagnostic"]
                infrastructure_marker |= reset["infrastructure_marker"]
                if reset["status"] == "completed" and reset["returncode"] != 0:
                    reset_status = "failed"
                    reset_error = b"OCCAM_INFRASTRUCTURE: reset failed\n"
                    append_tail(stderr, reset_error)


                    infrastructure_marker = True
                    status = "tool-error"
                    returncode = 125
                elif reset["status"] == "timeout":
                    status = "timeout"
                    returncode = 124
                elif reset["status"] != "completed":
                    status = "tool-error"
                    returncode = 125
                    reset_error = b"OCCAM_INFRASTRUCTURE: reset failed\n"
                    append_tail(stderr, reset_error)

                    infrastructure_marker = True
                if reset["status"] == "completed" and reset["returncode"] == 0:
                    result = run_command(command, args.cwd.resolve(), environment, deadline, sql)
                else:
                    result = None
            else:
                result = run_command(command, args.cwd.resolve(), environment, deadline, sql)

            if result is not None:
                append_tail(stdout, result["stdout"])
                append_tail(stderr, result["stderr"])
                stdout_diagnostic = stdout_diagnostic or result["stdout_diagnostic"]
                stderr_diagnostic = stderr_diagnostic or result["stderr_diagnostic"]
                infrastructure_marker |= result["infrastructure_marker"]
                if result["status"] == "timeout":
                    status = "timeout"
                    returncode = 124
                elif result["status"] == "tool-error":
                    status = "tool-error"
                    returncode = 125
                    oracle_error = b"OCCAM_INFRASTRUCTURE: oracle invocation failed\n"
                    append_tail(stderr, oracle_error)

                    infrastructure_marker = True
                else:
                    returncode = result["returncode"]
                    status = "accepted" if returncode == 0 else "rejected"
    except InterruptedCall as exc:
        status = "interrupted"
        returncode = 128 + exc.signum
        if reset_attempted and reset_status is None:
            reset_status = "interrupted"
        if reset_attempted and reset_elapsed_sec is None and reset_started is not None:
            reset_elapsed_sec = round(time.monotonic() - reset_started, 6)
    except Exception as exc:
        status = "tool-error"
        returncode = 125
        error_output = f"OCCAM_INFRASTRUCTURE: {exc}\n".encode()
        append_tail(stderr, error_output)

        infrastructure_marker = True
    stdout_output = bytes(stdout)
    stderr_output = bytes(stderr)
    if stdout_diagnostic and stdout_diagnostic not in stdout_output:
        stdout_output = stdout_diagnostic + (b"" if stdout_diagnostic.endswith(b"\n") else b"\n") + stdout_output
    if stderr_diagnostic and stderr_diagnostic not in stderr_output:
        stderr_output = stderr_diagnostic + (b"" if stderr_diagnostic.endswith(b"\n") else b"\n") + stderr_output

    stdout_text = bytes(stdout).decode(errors="replace")
    stderr_text = bytes(stderr).decode(errors="replace")
    if infrastructure_marker:
        status = "tool-error"
        returncode = 125

    record: dict[str, Any] = {
        "digest": hashlib.sha256(sql).hexdigest(),
        "phase": args.phase,
        "status": status,
        "returncode": returncode,
        "duration_sec": round(time.monotonic() - started, 6),
        "reset_attempted": reset_attempted,
        "reset_elapsed_sec": reset_elapsed_sec,
        "reset_status": reset_status,
        "stdout": stdout_text[-AUDIT_TAIL_BYTES:],
        "stdout_diagnostic": stdout_diagnostic.decode(errors="replace")
        if stdout_diagnostic is not None
        else None,
        "stderr_diagnostic": stderr_diagnostic.decode(errors="replace")
        if stderr_diagnostic is not None
        else None,
        "stderr": stderr_text[-AUDIT_TAIL_BYTES:],
    }
    args.audit.parent.mkdir(parents=True, exist_ok=True)
    with args.audit.open("a", encoding="utf-8") as audit:
        audit.write(json.dumps(record, sort_keys=True) + "\n")
    sys.stdout.buffer.write(stdout_output.decode(errors="replace").encode("utf-8"))
    sys.stderr.buffer.write(stderr_output.decode(errors="replace").encode("utf-8"))
    return returncode if returncode is not None and 0 <= returncode <= 255 else 1


if __name__ == "__main__":
    raise SystemExit(main())

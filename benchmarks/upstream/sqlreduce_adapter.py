#!/usr/bin/env python3
"""Run pinned SQLreduce AST transformations with the shared external predicate."""
from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time

PIN = "568c8a9a31601ead7128d0b067658b4d5a26821d"


class ReductionStopped(Exception):
    def __init__(self, code, reason):
        super().__init__(reason)
        self.code = code


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True, type=Path)
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--metrics", required=True, type=Path)
    parser.add_argument("--timeout", required=True, type=float)
    parser.add_argument("--deadline", required=True, type=float)
    parser.add_argument("checker", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    checker = args.checker[1:] if args.checker[:1] == ["--"] else args.checker
    if not checker or not math.isfinite(args.timeout) or not math.isfinite(args.deadline) or args.timeout <= 0:
        parser.error("a checker command and a positive timeout are required")
    try:
        revision = subprocess.run(
            ["git", "-C", str(args.source), "rev-parse", "HEAD"], check=True,
            capture_output=True, text=True, timeout=max(0.001, args.deadline - time.monotonic()),
        ).stdout.strip()
        if revision != PIN:
            raise ReductionStopped(78, f"SQLreduce source must be pinned at {PIN}, found {revision}")
        sys.path.insert(0, str(args.source))
        import pglast
        import sqlreduce
        if Path(sqlreduce.__file__).resolve().parent != (args.source / "sqlreduce").resolve():
            raise ReductionStopped(78, "SQLreduce was not imported from the pinned checkout")
    except (ImportError, OSError, subprocess.CalledProcessError) as exc:
        print(f"unsupported: SQLreduce prerequisite: {exc}", file=sys.stderr)
        return 78
    except subprocess.TimeoutExpired:
        return 124
    except ReductionStopped as exc:
        print(str(exc), file=sys.stderr)
        return exc.code

    checks = 0
    started = time.monotonic()
    status = "tool-error"
    reason = None
    code = 125
    args.output.parent.mkdir(parents=True, exist_ok=True)

    def predicate(_state, query):
        nonlocal checks
        remaining = args.deadline - time.monotonic()
        if remaining <= 0:
            raise ReductionStopped(124, "total tool budget exhausted")
        with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", suffix=".sql", dir=args.output.parent) as candidate:
            def terminate_checker():
                if process.poll() is None:
                    process.terminate()
                    try:
                        process.wait(timeout=0.5)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        process.wait()

            candidate.write(query)
            candidate.flush()
            process = subprocess.Popen([*checker, candidate.name], start_new_session=True)
            try:
                result = process.wait(timeout=min(args.timeout, remaining))
            except subprocess.TimeoutExpired:
                terminate_checker()
                result = 124
            finally:
                terminate_checker()
        checks += 1
        if result == 124:
            raise ReductionStopped(124, "predicate call timed out")
        if result in {125, 126}:
            raise ReductionStopped(result, "predicate infrastructure failure" if result == 125 else "max oracle executions reached")
        if result not in {0, 1}:
            raise ReductionStopped(125, f"checker exited {result}")
        if checks <= 2 and result != 0:
            raise ReductionStopped(77, "original or pglast-regenerated SQL did not preserve the predicate")
        return "accepted" if result == 0 else "rejected"

    def interrupt(_signal, _frame):
        raise ReductionStopped(124, "adapter interrupted")

    signal.signal(signal.SIGTERM, interrupt)

    try:
        original = args.input.read_text(encoding="utf-8")
        try:
            pglast.parse_sql(original)
        except Exception as exc:
            raise ReductionStopped(78, f"unsupported PostgreSQL syntax: {exc}") from exc
        # Keep upstream's search/rules unchanged; replace only candidate execution, not its AST engine.
        sqlreduce.run_query = predicate
        reduced, state = sqlreduce.run_reduce(original, debug=True)
        if predicate(state, reduced) != "accepted":
            raise ReductionStopped(77, "SQLreduce's final SQL did not preserve the predicate")
        temporary = args.output.with_suffix(args.output.suffix + ".tmp")
        temporary.write_text(reduced.rstrip() + "\n", encoding="utf-8")
        temporary.replace(args.output)
        status, code = "completed", 0
    except ReductionStopped as exc:
        code, reason = exc.code, str(exc)
        status = {77: "invalid-result", 78: "unsupported", 124: "timeout", 125: "tool-error", 126: "execution-budget"}[code]
    except Exception as exc:
        reason = str(exc)
    args.metrics.write_text(json.dumps({
        "status": status, "reason": reason, "upstream_pin": PIN,
        "upstream_source_sha256": hashlib.sha256(Path(sqlreduce.__file__).read_bytes()).hexdigest(),
        "pglast_version": pglast.__version__, "candidate_checks": checks,
        "elapsed_sec": time.monotonic() - started,
        "execution_adapter": "shared external predicate; no upstream database/reconnect loop",
    }, sort_keys=True) + "\n", encoding="utf-8")
    if reason:
        print(reason, file=sys.stderr)
    return code


if __name__ == "__main__":
    raise SystemExit(main())

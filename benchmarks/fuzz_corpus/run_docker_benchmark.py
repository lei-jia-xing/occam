#!/usr/bin/env python3
"""Run equal-budget reducer comparisons on fuzz SQL with external predicates."""

from __future__ import annotations

import argparse
import concurrent.futures
import hashlib
import json
import math
import os
import re
import shlex
import signal
import shutil
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

SQLANCER_PIN = "a7d60ee450e6a22ca38599c14e984a49773f4745"
SQLREDUCE_PIN = "568c8a9a31601ead7128d0b067658b4d5a26821d"
PERSES_PIN = "52d6b501f257385d5b50dfe68780986c19136abf"
PICIRENY_PIN = "dcf4fb0426e8ca157db6bf346782db0d87c33df7"
ANTLR_GRAMMARS_PIN = "9fa9f94b20cd3225bd84f27554515da207ec39ff"



def find_binary(root: Path, name: str) -> Path | None:
    if shutil.which("cabal"):
        try:
            build_env = os.environ.copy()
            build_env.pop("GHCRTS", None)
            result = subprocess.run(["cabal", "list-bin", name], check=False, cwd=root,
                                   capture_output=True, text=True, timeout=10, env=build_env)
            if result.returncode == 0 and result.stdout.strip():
                path = Path(result.stdout.strip())
                if path.is_file() and os.access(path, os.X_OK):
                    return path
        except (OSError, subprocess.TimeoutExpired):
            pass
    found = shutil.which(name)
    if found:
        return Path(found)
    for path in root.glob(f"dist-newstyle/**/{name}"):
        if path.is_file() and os.access(path, os.X_OK) and path.name == name:
            return path
    return None


def split_sql_statements(sql: str, dialect: str = "") -> list[str]:
    """Split on SQL terminators, not semicolons inside strings/comments/dollar quotes."""
    dialect = dialect.lower()
    statements: list[str] = []
    start = 0
    has_code = False
    quote = ""
    dollar_tag = ""
    block_depth = 0
    i = 0
    while i < len(sql):
        char = sql[i]
        following = sql[i + 1] if i + 1 < len(sql) else ""
        if block_depth:
            if char == "/" and following == "*":
                block_depth += 1
                i += 2
            elif char == "*" and following == "/":
                block_depth -= 1
                i += 2
            else:
                i += 1
            continue
        if dollar_tag:
            if sql.startswith(dollar_tag, i):
                i += len(dollar_tag)
                dollar_tag = ""
            else:
                i += 1
            continue
        if quote:
            if char == "\\" and i + 1 < len(sql):
                i += 2
            elif char == quote:
                if following == quote:
                    i += 2
                else:
                    quote = ""
                    i += 1
            else:
                i += 1
            continue
        if char == "/" and following == "*":
            block_depth = 1
            i += 2
        elif char == "-" and following == "-" and (dialect not in {"mysql", "mariadb"} or
                i + 2 == len(sql) or sql[i + 2].isspace() or ord(sql[i + 2]) < 32):
            i += 2
            while i < len(sql) and sql[i] not in "\r\n":
                i += 1
        elif char == "#" and dialect in {"mysql", "mariadb"}:
            while i < len(sql) and sql[i] not in "\r\n":
                i += 1
        elif char in "'\"`":
            quote = char
            has_code = True
            i += 1
        elif char == "$" and dialect not in {"mysql", "mariadb"}:
            match = re.match(r"\$([A-Za-z_][A-Za-z_0-9]*)?\$", sql[i:])
            if match:
                dollar_tag = match.group(0)
                has_code = True
                i += len(dollar_tag)
            else:
                has_code = True
                i += 1
        elif char == ";":
            if has_code:
                statement = sql[start:i].strip()
                if statement:
                    statements.append(statement + ";")
            start = i + 1
            has_code = False
            i += 1
        else:
            if not char.isspace():
                has_code = True
            i += 1
    if has_code:
        statement = sql[start:].strip()
        if statement:
            statements.append(statement + ("" if statement.endswith(";") else ";"))
    return statements


def count_statements(sql_bytes: bytes, dialect: str = "") -> int:
    return len(split_sql_statements(sql_bytes.decode("utf-8", errors="replace"), dialect))


def case_file(root: Path, entry: dict[str, Any], manifest_dir: Path) -> Path:
    candidate = Path(entry["file"])
    if candidate.is_absolute():
        return candidate
    for base in (manifest_dir, root, root / "benchmarks" / "fuzz_corpus"):
        path = base / candidate
        if path.is_file():
            return path
    return manifest_dir / candidate


def oracle_wrapper(root: Path, command: str, audit_file: Path, timeout: float,
                   scenario: str | None = None, reset: str | None = None,
                   phase: str = "reducer") -> str:
    args = [
        sys.executable, str(root / "benchmarks" / "upstream" / "oracle_exec.py"),
        "--command", command, "--audit", str(audit_file), "--timeout", str(timeout), "--cwd", str(root),
        "--phase", phase,
    ]
    if scenario:
        args += ["--scenario", scenario]
    if reset:
        args += ["--reset", reset]
    return shlex.join(args)


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


def run_exit_oracle(oracle: str, candidate: Path, cwd: Path, deadline: float,
                    call_timeout: float, audit_file: Path) -> tuple[bool | None, float, str | None]:
    try:
        audit_offset = audit_file.stat().st_size
    except OSError:
        audit_offset = 0
    command = ["bash", "-c", "exec " + oracle + ' "$1"', "oracle", str(candidate.resolve())]
    outcome = command_result(command, cwd, deadline, call_timeout, allow_nonzero=True)
    if outcome["status"] == "timeout":
        return None, outcome["elapsed"], "oracle call timed out"
    if outcome["status"] == "tool-error":
        return None, outcome["elapsed"], outcome["stderr"] or "oracle invocation failed"
    wrapper_record = next(
        (row for row in reversed(read_jsonl_since(audit_file, audit_offset)) if row.get("phase") == "validation"),
        None,
    )
    if wrapper_record is not None:
        if wrapper_record.get("status") == "timeout":
            return None, outcome["elapsed"], "oracle call timed out"
        if wrapper_record.get("status") == "interrupted":
            return None, outcome["elapsed"], "oracle call interrupted"
        if wrapper_record.get("status") == "tool-error":
            return None, outcome["elapsed"], outcome["stderr"] or "oracle infrastructure failure"
    if "OCCAM_INFRASTRUCTURE:" in outcome["stdout"] or "OCCAM_INFRASTRUCTURE:" in outcome["stderr"]:
        return None, outcome["elapsed"], outcome["stderr"] or outcome["stdout"] or "oracle infrastructure failure"
    return outcome["returncode"] == 0, outcome["elapsed"], (
        None if outcome["returncode"] == 0 else "configured exit-code oracle rejected the candidate"
    )


def base_record(entry: dict[str, Any], tool: str, oracle_timeout: float, budget: float,
                max_oracle_executions: int | None) -> dict[str, Any]:
    sid = entry["id"]
    evidence = entry.get("evidence_class", "synthetic")
    oracle_kind = entry.get("oracle_kind", "text_pattern" if evidence == "synthetic" else "unknown")
    return {
        "scenario": sid,
        "evidence_class": evidence,
        "oracle_kind": oracle_kind,
        "dialect": entry.get("dialect", "unknown"),
        "tool": tool,
        "status": "tool-error",
        "status_reason": None,
        "scale": None,
        "initial_stmts": None,
        "reduced_stmts": None,
        "stmt_red_pct": None,
        "initial_bytes": None,
        "reduced_bytes": None,
        "byte_red_pct": None,
        "oracle_timeout_sec": oracle_timeout,
        "oracle_timeouts": None,
        "budget_sec": budget,
        "max_oracle_executions": max_oracle_executions,
        "oracle_calls": None,
        "oracle_calls_scope": "uncached reducer-side callbacks; runner initial/final checks excluded",
        "candidate_checks": None,
        "oracle_cache_hits": None,
        "validation_calls": 0,
        "validation_time_sec": 0.0,
        "reset_calls": None,
        "reset_calls_scope": "uncached reducer-side callbacks; runner initial/final checks excluded",
        "reset_time_sec": None,
        "reset_accounting": "not measured; predicate-internal reset unknown",
        "reduction_time_sec": None,
        "wall_time_sec": None,
        "predicate_passed": None,
        "tool_candidate_valid": None,
        "checkpoint_used": None,
        "reduction_track": entry.get("reduction_track", "whole_script"),
    }


def set_size_metrics(record: dict[str, Any], original: bytes, reduced: bytes, dialect: str) -> None:
    initial_statements = count_statements(original, dialect)
    reduced_statements = count_statements(reduced, dialect)
    initial_bytes = len(original)
    reduced_bytes = len(reduced)
    record.update({
        "scale": f"{initial_statements} stmts",
        "initial_stmts": initial_statements,
        "reduced_stmts": reduced_statements,
        "stmt_red_pct": round((1 - reduced_statements / initial_statements) * 100, 2)
        if initial_statements else None,
        "initial_bytes": initial_bytes,
        "reduced_bytes": reduced_bytes,
        "byte_red_pct": round((1 - reduced_bytes / initial_bytes) * 100, 2) if initial_bytes else None,
    })

def kill_process_tree(pid: int) -> None:
    try:
        children = Path(f"/proc/{pid}/task/{pid}/children").read_text(encoding="ascii").split()
    except OSError:
        children = []
    for child in children:
        kill_process_tree(int(child))
    if os.name == "posix":
        try:
            os.killpg(pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
    else:
        try:
            os.kill(pid, signal.SIGKILL)
        except ProcessLookupError:
            pass

def command_result(command: list[str], cwd: Path, deadline: float, timeout_limit: float | None = None,
                   allow_nonzero: bool = False) -> dict[str, Any]:
    started = time.monotonic()
    timeout = min(deadline - started, timeout_limit) if timeout_limit is not None else deadline - started
    if timeout <= 0:
        return {"status": "timeout", "elapsed": 0.0, "returncode": None, "stdout": "", "stderr": "budget exhausted"}
    try:
        process = subprocess.Popen(
            command, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
            start_new_session=(os.name == "posix"),
        )
        try:
            stdout, stderr = process.communicate(timeout=timeout)
            status = "completed" if process.returncode == 0 or allow_nonzero else "tool-error"
            return {"status": status, "elapsed": time.monotonic() - started, "returncode": process.returncode,
                    "stdout": stdout, "stderr": stderr}
        except subprocess.TimeoutExpired as exc:
            kill_process_tree(process.pid)
            stdout, stderr = process.communicate()
            captured_stdout = exc.stdout.decode(errors="replace") if isinstance(exc.stdout, bytes) else (exc.stdout or "")
            captured_stderr = exc.stderr.decode(errors="replace") if isinstance(exc.stderr, bytes) else (exc.stderr or "")
            return {"status": "timeout", "elapsed": time.monotonic() - started, "returncode": None,
                    "stdout": stdout or captured_stdout, "stderr": stderr or captured_stderr or "process timed out"}
    except OSError as exc:
        return {"status": "tool-error", "elapsed": time.monotonic() - started, "returncode": None,
                "stdout": "", "stderr": str(exc)}


def read_json(path: Path) -> dict[str, Any] | list[Any] | None:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
        return value if isinstance(value, (dict, list)) else None
    except (OSError, json.JSONDecodeError):
        return None


def read_jsonl(path: Path) -> list[dict[str, Any]]:
    records: list[dict[str, Any]] = []
    if not path.is_file():
        return records
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        try:
            value = json.loads(line)
            if isinstance(value, dict):
                records.append(value)
        except json.JSONDecodeError:
            continue
    return records


def metric_count(value: Any) -> int | None:
    return value if type(value) is int and value >= 0 else None


def eval_internal_scope(profile: Any) -> tuple[int | None, int | None, int | None]:
    if not isinstance(profile, dict):
        return None, None, None
    budget = profile.get("evaluationBudget")
    if not isinstance(budget, dict):
        return None, None, None
    preflight = metric_count(budget.get("evaluationPreflightAttempts"))
    reduction = metric_count(profile.get("evaluationDatabaseObservations"))
    validation = metric_count(profile.get("evaluationValidationDatabaseObservations"))
    if preflight is None or reduction is None or validation is None:
        return None, None, None
    return preflight + reduction + validation, preflight, reduction
def set_reducer_reset_metrics(
    record: dict[str, Any],
    audit_file: Path,
    configured: bool,
    expected_internal_rows: int | None,
    reducer_start: int | None,
    reducer_count: int | None,
) -> None:
    if not configured:
        record["reset_accounting"] = "wrapper reset not configured; predicate-internal reset unknown"
        return
    if (
        expected_internal_rows is None
        or reducer_start is None
        or reducer_count is None
        or expected_internal_rows < 0
        or reducer_start < 0
        or reducer_count < 0
    ):
        record["reset_accounting"] = "wrapper reset audit incomplete; reducer callback scope only"
        return
    internal_rows = [
        row for row in read_jsonl(audit_file) if row.get("phase") == "reducer"
    ]
    if len(internal_rows) != expected_internal_rows:
        record["reset_accounting"] = "wrapper reset audit incomplete; reducer callback scope only"
        return
    reducer_rows = internal_rows[reducer_start : reducer_start + reducer_count]
    if len(reducer_rows) != reducer_count:
        record["reset_accounting"] = "wrapper reset audit incomplete; reducer callback scope only"
        return
    durations = [row.get("reset_elapsed_sec") for row in reducer_rows]
    if any(
        row.get("reset_attempted") is not True
        or not isinstance(duration, (int, float))
        or isinstance(duration, bool)
        or not math.isfinite(duration)
        for row, duration in zip(reducer_rows, durations)
    ):
        record["reset_accounting"] = "wrapper reset audit incomplete; reducer callback scope only"
        return
    record["reset_calls"] = sum(row["reset_attempted"] for row in reducer_rows)
    record["reset_time_sec"] = round(sum(durations), 6)
    record["reset_calls_scope"] = record["oracle_calls_scope"]
    record["reset_accounting"] = "oracle_exec measured; reducer callbacks only"


def set_no_reducer_reset_metrics(record: dict[str, Any], configured: bool) -> None:
    record["oracle_calls"] = 0
    if configured:
        record["reset_calls"] = 0
        record["reset_time_sec"] = 0.0
        record["reset_calls_scope"] = record["oracle_calls_scope"]
        record["reset_accounting"] = "no reducer callbacks; wrapper reset scope only"




def unsupported(record: dict[str, Any], reason: str) -> dict[str, Any]:
    record["status"] = "unsupported"
    record["status_reason"] = reason
    return record




def run_one(entry: dict[str, Any], root: Path, manifest_dir: Path, tool: str,
            binaries: dict[str, Path | None], oracle_override: str | None, work_dir: Path,
            oracle_timeout: float, budget: float, max_oracle_executions: int | None) -> dict[str, Any]:
    record = base_record(entry, tool, oracle_timeout, budget, max_oracle_executions)
    source_file = case_file(root, entry, manifest_dir)
    if not source_file.is_file():
        return unsupported(record, f"input SQL does not exist: {source_file}")
    original = source_file.read_bytes()
    dialect = str(entry.get("dialect", ""))
    synthetic = record["evidence_class"] == "synthetic"
    set_size_metrics(record, original, original, dialect)

    if tool in {"occam", "ddmin", "creduce"} and binaries.get(tool) is None:
        return unsupported(record, f"{tool} binary is not installed or discoverable")
    if tool == "sqlreduce":
        if dialect.lower() not in {"postgres", "postgresql", "pg"}:
            return unsupported(record, "SQLreduce supports PostgreSQL only")

    dialect_key = {"postgresql": "postgres", "pg": "postgres", "sqlite3": "sqlite"}.get(
        dialect.lower(), dialect.lower())
    if tool == "perses":
        record["upstream_pin"] = f"uw-pluverse/perses@{PERSES_PIN}"
        if dialect_key not in {"mysql", "sqlite"}:
            record["grammar_status"] = "unavailable"
            return unsupported(
                record,
                f"Perses {PERSES_PIN} has MySQL and SQLite grammars, but no {dialect} SQL grammar",
            )
        record["grammar_status"] = "available_not_integrated"
        record["status"] = "not_integrated"
        record["status_reason"] = (
            f"Perses {PERSES_PIN} has a {dialect_key} grammar, but no benchmark adapter "
            "wires the shared interestingness predicate"
        )
        return record
    if tool == "hdd":
        record["upstream_pin"] = f"renatahodovan/picireny@{PICIRENY_PIN}"
        record["grammar_pin"] = f"antlr/grammars-v4@{ANTLR_GRAMMARS_PIN}"
        if dialect_key not in {"postgres", "mysql", "sqlite"}:
            record["grammar_status"] = "unavailable"
            return unsupported(
                record,
                f"ANTLR grammars-v4 {ANTLR_GRAMMARS_PIN} has PostgreSQL, MySQL, and SQLite "
                f"SQL grammars, but no {dialect} grammar",
            )
        record["grammar_status"] = "available_not_integrated"
        record["status"] = "not_integrated"
        record["status_reason"] = (
            f"Picireny {PICIRENY_PIN} requires an external ANTLR grammar and start rule; "
            f"grammars-v4 {ANTLR_GRAMMARS_PIN} has the {dialect_key} grammar, but no "
            "benchmark adapter configures it with the shared interestingness predicate"
        )
        return record

    if tool not in {"occam", "ddmin", "creduce", "sqlancer", "sqlreduce"}:
        return unsupported(record, f"unknown or unintegrated reducer: {tool}")

    tool_dir = work_dir / f"{entry['id']}_{tool}"
    tool_dir.mkdir(parents=True, exist_ok=True)
    candidate = tool_dir / "candidate.sql"
    checkpoint = tool_dir / "checkpoint.sql"
    calls_file = tool_dir / "calls.jsonl"
    audit_file = tool_dir / "oracle_audit.jsonl"
    cache_file = tool_dir / "oracle-cache.json"
    metrics_file = tool_dir / "metrics.json"
    candidate.write_bytes(original)
    checkpoint.write_bytes(original)
    deadline = time.monotonic() + budget
    started = deadline - budget
    validation_time = 0.0
    validation_calls = 0
    reducer_calls: int | None = None
    candidate_checks: int | None = None
    cache_hits: int | None = None
    internal_expected_rows: int | None = None
    internal_reducer_start: int | None = None
    internal_reducer_count: int | None = None
    native_profile: dict[str, Any] | None = None
    native_stats: dict[str, Any] | None = None
    native_metrics_path: Path | None = None
    native_metrics_mtime: int | None = None
    native_profile_fresh = False
    reset_command = entry.get("reset_command")
    if isinstance(reset_command, list):
        reset_command = shlex.join([str(part) for part in reset_command])
    elif reset_command is not None:
        reset_command = str(reset_command)
    reset_configured = bool(reset_command)

    raw_oracle = oracle_override or entry.get("oracle_command")
    scenario = None
    if synthetic and not raw_oracle:
        raw_oracle = str(root / "benchmarks" / "fuzz_corpus" / "fuzz_oracle.sh")
        scenario = entry["id"]
    if not raw_oracle:
        return unsupported(record, "case has no oracle command")
    reducer_oracle = oracle_wrapper(
        root, str(raw_oracle), audit_file, oracle_timeout, scenario, reset_command, "reducer"
    )
    validation_oracle = oracle_wrapper(
        root, str(raw_oracle), audit_file, oracle_timeout, scenario, reset_command, "validation"
    )

    valid, validation_elapsed, validation_error = run_exit_oracle(
        validation_oracle, source_file, root, deadline, oracle_timeout, audit_file)

    validation_calls += 1
    validation_time += validation_elapsed
    if valid is None:
        record.update({
            "status": "timeout" if "timed out" in (validation_error or "") or "budget" in (validation_error or "") else "tool-error",
            "status_reason": validation_error,
            "validation_calls": validation_calls,
            "validation_time_sec": round(validation_time, 4),
            "predicate_passed": None,
            "wall_time_sec": round(time.monotonic() - started, 4),
        })
        set_no_reducer_reset_metrics(record, reset_configured)
        return record
    if not valid:
        record.update({
            "status": "invalid-result",
            "status_reason": "input did not satisfy its configured oracle",
            "validation_calls": validation_calls,
            "validation_time_sec": round(validation_time, 4),
            "predicate_passed": False,
            "tool_candidate_valid": False,
            "wall_time_sec": round(time.monotonic() - started, 4),
        })
        set_no_reducer_reset_metrics(record, reset_configured)
        return record

    remaining = max(0.0, deadline - time.monotonic())
    if remaining <= 0:
        record.update({
            "status": "timeout",
            "status_reason": "total budget exhausted during input validation",
            "validation_calls": validation_calls,
            "validation_time_sec": round(validation_time, 4),
            "predicate_passed": True,
            "checkpoint_used": "initial",
            "wall_time_sec": round(time.monotonic() - started, 4),
        })
        set_size_metrics(record, original, checkpoint.read_bytes(), dialect)
        set_no_reducer_reset_metrics(record, reset_configured)
        return record

    if tool in {"occam", "ddmin"}:
        native_metrics_path = tool_dir / ("stats.json" if tool == "occam" else "benchmark.json")
        # A stale profile must not reinterpret this invocation's exit.
        try:
            native_metrics_mtime = native_metrics_path.stat().st_mtime_ns
        except OSError:
            pass
        command = [
            str(binaries[tool]), str(source_file), "--oracle", reducer_oracle, "--out", str(tool_dir),
            "--timeout", str(oracle_timeout), "--budget", str(remaining),
        ]
        command += ["--dialect", dialect_key if dialect_key in {"postgres", "mysql", "sqlite"} else "generic"]
        if max_oracle_executions is not None:
            command += ["--max-oracle-executions", str(max_oracle_executions)]
        if tool == "ddmin":
            command += ["--baseline", "2"]
        outcome = command_result(command, root, deadline)
        reduction_elapsed = outcome["elapsed"]
        if tool == "occam":
            stats = read_json(tool_dir / "stats.json")
            if isinstance(stats, dict):
                native_stats = stats
                profile = stats.get("evaluation_metrics")
                if isinstance(profile, dict):
                    native_profile = profile
                    reducer_calls = profile.get("evaluationDatabaseObservations")
                    cache_hits = profile.get("evaluationCacheHits")
                    hits, misses = profile.get("evaluationCacheHits"), profile.get("evaluationCacheMisses")
                    candidate_checks = hits + misses if isinstance(hits, int) and isinstance(misses, int) else None
                    internal_expected_rows, internal_reducer_start, internal_reducer_count = eval_internal_scope(profile)
                    if outcome["status"] == "timeout":
                        internal_expected_rows = internal_reducer_start = internal_reducer_count = None
            candidate_path = tool_dir / "minimal.sql"
            metrics = stats
        else:
            values = read_json(tool_dir / "benchmark.json")
            metrics = values[0] if isinstance(values, list) and values and isinstance(values[0], dict) else None
            if metrics:
                native_stats = metrics
                profile = metrics.get("metricEvaluationMetrics")
                if isinstance(profile, dict):
                    native_profile = profile
                    reducer_calls = profile.get("evaluationDatabaseObservations")
                    cache_hits = profile.get("evaluationCacheHits")
                    hits, misses = profile.get("evaluationCacheHits"), profile.get("evaluationCacheMisses")
                    candidate_checks = hits + misses if isinstance(hits, int) and isinstance(misses, int) else None
                    internal_expected_rows, internal_reducer_start, internal_reducer_count = eval_internal_scope(profile)
                    if outcome["status"] == "timeout":
                        internal_expected_rows = internal_reducer_start = internal_reducer_count = None
            candidate_path = tool_dir / "minimal.sql"
    elif tool == "creduce":
        helper = root / "benchmarks" / "upstream" / "oracle_test.py"
        test_script = tool_dir / "test.sh"
        cache_file.write_text(json.dumps({hashlib.sha256(original).hexdigest(): "accepted"}), encoding="utf-8")
        helper_args = [
            "--checkpoint", str(checkpoint), "--calls", str(calls_file), "--cache", str(cache_file),
            "--timeout", str(oracle_timeout), "--deadline", str(deadline),
            "--oracle", reducer_oracle, "--oracle-audit", str(audit_file),
        ]
        if max_oracle_executions is not None:
            helper_args += ["--max-executions", str(max_oracle_executions)]
        test_script.write_text(
            "#!/usr/bin/env bash\nset -euo pipefail\nexec " + shlex.join([sys.executable, str(helper), *helper_args])
            + " --candidate " + shlex.quote(str(candidate)) + "\n", encoding="utf-8")
        test_script.chmod(0o755)
        command = [
            str(binaries[tool]), "--not-c", str(test_script), str(candidate),
            "--n", "1", "--timeout", str(max(1, math.ceil(oracle_timeout + 5))),
        ]
        outcome = command_result(command, tool_dir, deadline)
        reduction_elapsed = outcome["elapsed"]
        call_records = read_jsonl(calls_file)
        reducer_calls = sum(1 for row in call_records if not row.get("cache_hit") and row.get("status") in
                            {"accepted", "rejected", "timeout", "tool-error"})
        candidate_checks = len(call_records)
        cache_hits = sum(1 for row in call_records if row.get("cache_hit"))
        if outcome["status"] != "timeout" and reducer_calls is not None:
            internal_expected_rows = reducer_calls
            internal_reducer_start = 0
            internal_reducer_count = reducer_calls
        candidate_path = checkpoint
        metrics = None
    elif tool in {"sqlancer", "sqlreduce"}:
        candidate.unlink()
        helper = root / "benchmarks" / "upstream" / "oracle_test.py"
        helper_args = [
            sys.executable, str(helper), "--checkpoint", str(checkpoint), "--calls", str(calls_file),
            "--cache", str(cache_file), "--timeout", str(oracle_timeout), "--deadline", str(deadline),
            "--oracle", reducer_oracle, "--oracle-audit", str(audit_file),
        ]
        if max_oracle_executions is not None:
            helper_args += ["--max-executions", str(max_oracle_executions)]
        if tool == "sqlancer":
            adapter = root / "benchmarks" / "upstream" / "sqlancer_adapter.sh"
            command = [
                str(adapter), "--input", str(source_file.resolve()), "--output", str(candidate.resolve()),
                "--calls", str(tool_dir / "reducer-calls.jsonl"), "--metrics", str(metrics_file),
                "--timeout-ms", str(max(1, int(oracle_timeout * 1000))),
                "--budget-ms", str(max(1, int(remaining * 1000))),
            ]
        else:
            source = Path(os.environ.get("SQLREDUCE_SOURCE", f"{Path.home()}/.cache/occam/sqlreduce-{SQLREDUCE_PIN}"))
            command = [
                sys.executable, str(root / "benchmarks" / "upstream" / "sqlreduce_adapter.py"),
                "--source", str(source.resolve()), "--input", str(source_file.resolve()),
                "--output", str(candidate.resolve()), "--metrics", str(metrics_file),
                "--timeout", str(oracle_timeout), "--deadline", str(deadline),
            ]
        outcome = command_result([*command, "--", *helper_args], tool_dir, deadline)
        if outcome["status"] != "timeout":
            outcome["status"] = {
                0: "completed", 77: "invalid-result", 78: "unsupported", 124: "timeout",
                125: "tool-error", 126: "execution-budget",
            }.get(outcome["returncode"], "tool-error")
        reduction_elapsed = outcome["elapsed"]
        call_records = read_jsonl(calls_file)
        reducer_calls = sum(1 for row in call_records if not row.get("cache_hit") and row.get("status") in
                            {"accepted", "rejected", "timeout", "tool-error"})
        candidate_checks = len(call_records)
        cache_hits = sum(1 for row in call_records if row.get("cache_hit"))
        if outcome["status"] != "timeout":
            internal_expected_rows = reducer_calls
            internal_reducer_start = 0
            internal_reducer_count = reducer_calls
        metrics = read_json(metrics_file)
        launched = metrics.get("reducer_callbacks") if isinstance(metrics, dict) and tool == "sqlancer" else None
        if outcome["returncode"] is None or isinstance(launched, int) and launched > len(call_records):
            record["observed_uncached_callbacks"] = reducer_calls
            record["observed_callback_records"] = len(call_records)
            reducer_calls = candidate_checks = cache_hits = None
            internal_expected_rows = internal_reducer_start = internal_reducer_count = None
        if isinstance(metrics, dict) and tool == "sqlancer":
            record["sqlancer_ast_unsupported_statements"] = metrics.get("ast_unsupported_statements")
            record["sqlancer_ast_statement_count"] = metrics.get("statement_count")
        candidate_path = candidate

    if native_metrics_path is not None:
        try:
            native_profile_fresh = native_metrics_path.stat().st_mtime_ns != native_metrics_mtime
        except OSError:
            native_profile_fresh = False
    if (
        native_profile is not None
        and native_metrics_path is not None
        and outcome["status"] in {"completed", "tool-error"}
        and outcome["returncode"] is not None
    ):
        if native_profile_fresh:
            native_status = native_profile.get("evaluationStatus")
            failure = native_profile.get("evaluationFailure")
            failure_reason = failure.strip() if isinstance(failure, str) and failure.strip() else None
            if failure_reason:
                outcome["status_reason"] = failure_reason
            if isinstance(native_status, str) and native_status in {
                "completed", "timeout", "execution-budget", "error",
                "validation-failed", "validation-incomplete",
            }:
                if native_status == "completed":
                    if outcome["returncode"] != 0:
                        outcome["status"] = "tool-error"
                        outcome["status_reason"] = (
                            failure_reason or "native reducer exited nonzero with completed status"
                        )
                else:
                    outcome["status"] = native_status
                    outcome["status_reason"] = failure_reason or (
                        "max oracle executions reached"
                        if native_status == "execution-budget"
                        else native_status
                    )

    if (
        tool in {"occam", "ddmin", "sqlancer", "sqlreduce"}
        and outcome["status"] == "completed"
        and outcome["returncode"] == 0
        and not candidate_path.is_file()
    ):
        outcome["status"] = "tool-error"
        outcome["status_reason"] = "reducer exited successfully without its required SQL output"

    if tool in {"occam", "ddmin"} and candidate_path and candidate_path.is_file():
        shutil.copyfile(candidate_path, checkpoint)

    call_records = read_jsonl(calls_file)
    if outcome["status"] == "completed":
        if any(row.get("status") == "execution-limit" for row in call_records):
            outcome.update({"status": "execution-budget", "status_reason": "max oracle executions reached"})
        elif any(row.get("status") == "tool-error" for row in call_records):
            outcome.update({"status": "tool-error", "status_reason": "predicate infrastructure failure"})

    tool_status = outcome["status"]
    if tool_status == "completed" and outcome["returncode"] == 0:
        candidate_bytes = candidate_path.read_bytes() if candidate_path and candidate_path.is_file() else original
        candidate.write_bytes(candidate_bytes)
        valid, final_validation_elapsed, validation_error = run_exit_oracle(
            validation_oracle, candidate, root, deadline, oracle_timeout, audit_file)
        validation_calls += 1
        validation_time += final_validation_elapsed
        if valid is True:
            record.update({
                "status": "completed",
                "tool_candidate_valid": True,
                "predicate_passed": True,
                "checkpoint_used": "tool-result",
            })
        elif valid is None:
            record.update({
                "status": "timeout" if "timed out" in (validation_error or "") or "budget" in (validation_error or "") else "tool-error",
                "status_reason": validation_error,
                "tool_candidate_valid": None,
                "predicate_passed": None,
                "checkpoint_used": "last-valid",
            })
            candidate_bytes = checkpoint.read_bytes() if checkpoint.is_file() else original
        else:
            record.update({
                "status": "invalid-result",
                "status_reason": validation_error or "reduced output did not satisfy the configured predicate",
                "tool_candidate_valid": False,
                "predicate_passed": False,
                "checkpoint_used": "initial",
            })
            candidate_bytes = original
    else:
        record["status"] = tool_status
        detail = (
            outcome.get("status_reason")
            or outcome.get("stderr")
            or outcome.get("stdout")
            or ""
        ).strip()
        record["status_reason"] = detail[:500] if detail else tool_status
        candidate_bytes = checkpoint.read_bytes() if checkpoint.is_file() else original
        record["checkpoint_used"] = "last-valid" if candidate_bytes != original else "initial"
        record["predicate_passed"] = False if tool_status == "validation-failed" else None
        record["tool_candidate_valid"] = None

    record["oracle_calls"] = reducer_calls
    record["candidate_checks"] = candidate_checks
    record["oracle_cache_hits"] = cache_hits
    record["validation_calls"] = validation_calls
    record["validation_time_sec"] = round(validation_time, 4)
    record["reduction_time_sec"] = round(reduction_elapsed, 4)
    record["wall_time_sec"] = round(time.monotonic() - started, 4)
    record["oracle_calls_scope"] = "uncached reducer-side callbacks; runner initial/final checks excluded"
    set_reducer_reset_metrics(
        record,
        audit_file,
        reset_configured,
        internal_expected_rows,
        internal_reducer_start,
        internal_reducer_count,
    )
    call_records = read_jsonl(calls_file)
    audit_records = read_jsonl(audit_file)
    if tool in {"occam", "ddmin"}:
        timed_out_observations = (
            native_profile.get("evaluationTimedOutObservations")
            if native_profile is not None and native_profile_fresh
            else None
        )
        record["oracle_timeouts"] = metric_count(timed_out_observations)
    else:
        reducer_timeout_count = sum(1 for row in call_records if row.get("status") == "timeout")
        audit_timeout_count = sum(
            1 for row in audit_records
            if row.get("phase") == "reducer" and row.get("status") == "timeout"
        )
        record["oracle_timeouts"] = max(reducer_timeout_count, audit_timeout_count)
    if any(row.get("status") == "execution-limit" for row in call_records):
        record["status_reason"] = record.get("status_reason") or "max oracle executions reached"
    result_file = tool_dir / "result.sql"
    result_file.write_bytes(candidate_bytes)
    record["result_file"] = str(result_file)
    record["oracle_audit_file"] = str(audit_file)
    set_size_metrics(record, original, candidate_bytes, dialect)
    if tool in {"occam", "ddmin"} and native_profile_fresh and native_stats is not None:
        initial_statements = (
            native_stats.get("initial_statements")
            if tool == "occam"
            else native_stats.get("metricInitialStmts")
        )
        native_reduced_statements = (
            native_stats.get("reduced_statements")
            if tool == "occam"
            else native_stats.get("metricReducedStmts")
        )
        if isinstance(initial_statements, int):
            reduced_statements = record["reduced_stmts"]
            if record["checkpoint_used"] == "tool-result" and isinstance(native_reduced_statements, int):
                reduced_statements = native_reduced_statements
            record.update(
                {
                    "scale": f"{initial_statements} stmts",
                    "initial_stmts": initial_statements,
                    "reduced_stmts": reduced_statements,
                    "stmt_red_pct": round((1 - reduced_statements / initial_statements) * 100, 2)
                    if initial_statements
                    else None,
                }
            )
    return record


def run_scenario_tools(entry: dict[str, Any], root: Path, manifest_dir: Path, tools: list[str],
                       binaries: dict[str, Path | None], oracle_override: str | None,
                       work_dir: Path, oracle_timeout: float, budget: float,
                       max_oracle_executions: int | None) -> list[dict[str, Any]]:
    results: list[dict[str, Any]] = []
    for tool in tools:
        result = run_one(entry, root, manifest_dir, tool, binaries, oracle_override, work_dir,
                         oracle_timeout, budget, max_oracle_executions)
        results.append(result)
        reduced = result.get("reduced_stmts")
        calls = result.get("oracle_calls")
        wall_time = result.get("wall_time_sec")
        print(f"[{result['tool']:10s}] {entry['id']:22s} status={result['status'].upper():14s} "
              f"stmts={str(reduced) if reduced is not None else 'N/A':>4s} "
              f"calls={str(calls) if calls is not None else 'N/A':>5s} "
              f"time={str(wall_time) if wall_time is not None else 'N/A':>7s}s")
    return results


def main() -> None:
    parser = argparse.ArgumentParser(description="Equal-budget multi-tool SQL reducer benchmark")
    parser.add_argument("--jobs", "-j", type=int, default=2, help="Parallel case workers (default: 2)")
    parser.add_argument("--oracle-timeout", "--timeout", dest="oracle_timeout", type=float, default=15.0,
                        help="Per-oracle-call timeout in seconds (default: 15)")
    parser.add_argument("--budget", type=float, default=300.0,
                        help="Per-tool total wall-clock budget including validation (default: 300)")
    parser.add_argument("--max-oracle-executions", type=int, default=None,
                        help="Optional maximum uncached reducer oracle executions")
    parser.add_argument("--oracle", type=str, default=None, help="Override each case's oracle command")
    parser.add_argument("--out", type=str, default=None, help="Output JSONL path")
    parser.add_argument("--artifacts-dir", type=str, default="benchmarks/benchmark_artifacts",
                        help="Persistent root directory for reducer artifacts")
    parser.add_argument("--manifest", type=str, default=None, help="Case manifest (default: synthetic fuzz metadata)")
    parser.add_argument("--tools", type=str, default="occam,ddmin,creduce,sqlancer,sqlreduce",
                        help="Comma-separated reducers")
    args = parser.parse_args()

    if args.oracle_timeout <= 0 or args.budget <= 0 or args.jobs < 1:
        parser.error("--oracle-timeout and --budget must be positive, and --jobs must be at least 1")
    if args.max_oracle_executions is not None and args.max_oracle_executions < 1:
        parser.error("--max-oracle-executions must be positive")

    root = Path(__file__).resolve().parents[2]
    default_dir = root / "benchmarks" / "fuzz_corpus"
    manifest_path = Path(args.manifest) if args.manifest else default_dir / "metadata.json"
    if not manifest_path.is_absolute():
        manifest_path = root / manifest_path
    manifest_path = manifest_path.resolve()
    if not manifest_path.is_file():
        parser.error(f"manifest does not exist: {manifest_path}")
    try:
        raw_manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        parser.error(f"invalid manifest JSON: {exc}")
    if isinstance(raw_manifest, dict):
        entries = raw_manifest.get("cases")
        manifest_evidence = raw_manifest.get("evidence_class", "fuzz")
        manifest_oracle_kind = raw_manifest.get("oracle_kind", "unknown")
        manifest_oracle = raw_manifest.get("oracle_command")
    elif isinstance(raw_manifest, list):
        entries = raw_manifest
        is_default_manifest = manifest_path == (default_dir / "metadata.json").resolve()
        manifest_evidence = "synthetic" if is_default_manifest else "fuzz"
        manifest_oracle_kind = "text_pattern" if manifest_evidence == "synthetic" else "unknown"
        manifest_oracle = None
    else:
        parser.error("manifest must be a JSON list or object with a cases list")
    if not isinstance(entries, list) or any(not isinstance(entry, dict) for entry in entries):
        parser.error("manifest cases must be a list of case objects")
    seen_ids: set[str] = set()
    for entry in entries:
        if not entry.get("id") or not entry.get("file"):
            parser.error("every case must have id and file")
        if entry["id"] in seen_ids:
            parser.error(f"duplicate case id: {entry['id']}")
        seen_ids.add(entry["id"])
        entry.setdefault("evidence_class", manifest_evidence)
        entry.setdefault("oracle_kind", manifest_oracle_kind)
        entry.setdefault("oracle_command", manifest_oracle)
    selected_tools = [tool.strip().lower() for tool in args.tools.split(",") if tool.strip()]
    if not selected_tools:
        parser.error("--tools must select at least one reducer")
    jsonl_path = Path(args.out) if args.out else root / "benchmarks" / "fuzz_benchmark_results.jsonl"
    if not jsonl_path.is_absolute():
        jsonl_path = root / jsonl_path
    artifacts_root = Path(args.artifacts_dir)
    if not artifacts_root.is_absolute():
        artifacts_root = root / artifacts_root
    work_dir = artifacts_root / f"run_{time.strftime('%Y%m%dT%H%M%S')}_{os.getpid()}"

    binaries = {
        "occam": find_binary(root, "occam") if "occam" in selected_tools else None,
        "ddmin": find_binary(root, "occam-eval") if "ddmin" in selected_tools else None,
        "creduce": Path(shutil.which("creduce")) if "creduce" in selected_tools and shutil.which("creduce") else None,
    }
    evidence = entries[0].get("evidence_class", manifest_evidence) if entries else manifest_evidence
    print("=" * 72)
    print("SQL reducer comparison (results preserve only the configured predicate)")
    print(f"Cases: {len(entries)} | evidence_class: {evidence} | tools: {', '.join(selected_tools)}")
    print(f"Oracle timeout: {args.oracle_timeout}s | total tool budget: {args.budget}s")
    print(f"SQLancer reducer pin: {SQLANCER_PIN} | SQLreduce pin: {SQLREDUCE_PIN}")
    print(f"Artifacts: {work_dir}")
    print(f"Output: {jsonl_path}")
    print("=" * 72)

    started = time.monotonic()
    work_dir.mkdir(parents=True, exist_ok=True)
    jsonl_path.parent.mkdir(parents=True, exist_ok=True)
    summary_path = jsonl_path.with_suffix(".summary.json")
    try:
        summary_path.unlink()
    except FileNotFoundError:
        pass

    input_count = 0
    record_count = 0
    validated_reduction_count = 0
    status_counts: dict[str, int] = {}
    expected_record_count = len(entries) * len(selected_tools)
    with jsonl_path.open("w", encoding="utf-8") as output:
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
            futures = {
                executor.submit(
                    run_scenario_tools, entry, root, manifest_path.parent, selected_tools, binaries,
                    args.oracle, work_dir, args.oracle_timeout, args.budget, args.max_oracle_executions,
                ): entry
                for entry in entries
            }
            for future in concurrent.futures.as_completed(futures):
                entry = futures[future]
                try:
                    results = future.result()
                except Exception as exc:
                    print(f"[ERROR] {entry.get('id', 'unknown')} failed: {exc}", file=sys.stderr)
                    results = [base_record(entry, tool, args.oracle_timeout, args.budget,
                                           args.max_oracle_executions) for tool in selected_tools]
                    for result in results:
                        result["status"] = "tool-error"
                        result["status_reason"] = str(exc)
                input_count += 1
                for result in results:
                    output.write(json.dumps(result, sort_keys=True) + "\n")
                    output.flush()
                    record_count += 1
                    status = str(result.get("status") or "unknown")
                    status_counts[status] = status_counts.get(status, 0) + 1
                    if status == "completed" and result.get("predicate_passed") is True:
                        validated_reduction_count += 1

    elapsed = time.monotonic() - started
    summary = {
        "schema_version": 1,
        "results_file": str(jsonl_path),
        "expected_input_count": len(entries),
        "input_count": input_count,
        "input_count_scope": "manifest inputs with records emitted; errors count as processed",
        "inputs_per_second": input_count / elapsed if elapsed > 0 else None,
        "expected_record_count": expected_record_count,
        "record_count": record_count,
        "record_count_scope": "emitted case/tool records, including failures and unsupported cases",
        "records_per_second": record_count / elapsed if elapsed > 0 else None,
        "validated_reduction_count": validated_reduction_count,
        "nonvalidated_record_count": record_count - validated_reduction_count,
        "status_counts": status_counts,
        "wall_time_sec": elapsed,
    }
    summary_tmp = summary_path.with_suffix(summary_path.suffix + ".tmp")
    summary_tmp.write_text(json.dumps(summary, sort_keys=True) + "\n", encoding="utf-8")
    os.replace(summary_tmp, summary_path)
    print(
        f"Processed {input_count}/{len(entries)} inputs and {record_count}/{expected_record_count} "
        f"case/tool records in {elapsed:.2f}s; validated reductions: {validated_reduction_count}"
    )
    input_rate = f"{summary['inputs_per_second']:.2f}" if summary["inputs_per_second"] is not None else "N/A"
    record_rate = f"{summary['records_per_second']:.2f}" if summary["records_per_second"] is not None else "N/A"
    print(f"Throughput: {input_rate} inputs/s, {record_rate} records/s")
    print(f"Results written to: {jsonl_path}")
    print(f"Run summary written to: {summary_path}")


if __name__ == "__main__":
    main()

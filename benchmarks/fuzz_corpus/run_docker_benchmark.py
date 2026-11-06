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
    try:
        result = subprocess.run(["cabal", "list-bin", name], check=False, cwd=root,
                               capture_output=True, text=True, timeout=10)
        if result.returncode == 0 and result.stdout.strip():
            path = Path(result.stdout.strip())
            if path.is_file() and os.access(path, os.X_OK):
                return path
    except (OSError, subprocess.TimeoutExpired):
        pass
    for path in root.glob(f"dist-newstyle/**/{name}"):
        if path.is_file() and os.access(path, os.X_OK) and path.name == name:
            return path
    found = shutil.which(name)
    return Path(found) if found else None


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
                   scenario: str | None = None, reset: str | None = None) -> str:
    args = [
        sys.executable, str(root / "benchmarks" / "upstream" / "oracle_exec.py"),
        "--command", command, "--audit", str(audit_file), "--timeout", str(timeout), "--cwd", str(root),
    ]
    if scenario:
        args += ["--scenario", scenario]
    if reset:
        args += ["--reset", reset]
    return shlex.join(args)


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
        "oracle_calls_scope": "reduction only; validation is separate",
        "candidate_checks": None,
        "oracle_cache_hits": None,
        "validation_calls": 0,
        "validation_time_sec": 0.0,
        "reset_calls": None,
        "reset_time_sec": None,
        "reset_accounting": "not exposed by the configured oracle",
        "reduction_time_sec": None,
        "wall_time_sec": None,
        "predicate_passed": None,
        "tool_candidate_valid": None,
        "checkpoint_used": None,
        "reduction_track": "whole_script",
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


def unsupported(record: dict[str, Any], reason: str) -> dict[str, Any]:
    record["status"] = "unsupported"
    record["status_reason"] = reason
    return record


def run_exit_oracle(oracle: str, candidate: Path, cwd: Path, deadline: float,
                    call_timeout: float) -> tuple[bool | None, float, str | None]:
    command = ["bash", "-c", "exec " + oracle + ' "$1"', "oracle", str(candidate.resolve())]
    outcome = command_result(command, cwd, deadline, call_timeout, allow_nonzero=True)
    if outcome["status"] == "timeout":
        return None, outcome["elapsed"], "oracle call timed out"
    if outcome["status"] == "tool-error":
        return None, outcome["elapsed"], outcome["stderr"] or "oracle invocation failed"
    if "OCCAM_INFRASTRUCTURE:" in outcome["stdout"] or "OCCAM_INFRASTRUCTURE:" in outcome["stderr"]:
        return None, outcome["elapsed"], outcome["stderr"] or outcome["stdout"] or "oracle infrastructure failure"
    return outcome["returncode"] == 0, outcome["elapsed"], (
        None if outcome["returncode"] == 0 else "configured exit-code oracle rejected the candidate"
    )


def run_one(entry: dict[str, Any], root: Path, manifest_dir: Path, tool: str,
            binaries: dict[str, Path | None], oracle_override: str | None, work_dir: Path,
            oracle_timeout: float, budget: float, max_oracle_executions: int | None) -> dict[str, Any]:
    record = base_record(entry, tool, oracle_timeout, budget, max_oracle_executions)
    source_file = case_file(root, entry, manifest_dir)
    if not source_file.is_file():
        return unsupported(record, f"input SQL does not exist: {source_file}")
    original = source_file.read_bytes()
    input_file = source_file
    dialect = str(entry.get("dialect", ""))
    synthetic = record["evidence_class"] == "synthetic"
    sqlreduce_track = entry.get("sqlreduce_track") == "fixed_setup_query"
    if sqlreduce_track:
        query_file = case_file(root, {"file": entry.get("query_file", "")}, manifest_dir)
        if not query_file.is_file():
            return unsupported(record, "SQLreduce fixed-setup track requires a readable query_file")
        input_file = query_file
        original = query_file.read_bytes()
        record["reduction_track"] = "fixed_setup_query"
    set_size_metrics(record, original, original, dialect)

    if tool in {"occam", "ddmin", "creduce"} and binaries.get(tool) is None:
        return unsupported(record, f"{tool} binary is not installed or discoverable")
    if tool == "sqlreduce":
        if dialect.lower() not in {"postgres", "postgresql", "pg"}:
            return unsupported(record, "SQLreduce supports PostgreSQL only")
        if not sqlreduce_track:
            return unsupported(record, "pinned SQLreduce supports only a fixed_setup_query with one parsed PostgreSQL query")
    elif sqlreduce_track:
        input_file = source_file
        original = source_file.read_bytes()
        record["reduction_track"] = "whole_script"
        set_size_metrics(record, original, original, dialect)

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
    reset_command = entry.get("reset_command")
    if isinstance(reset_command, list):
        reset_command = shlex.join([str(part) for part in reset_command])
    elif reset_command is not None:
        reset_command = str(reset_command)

    raw_oracle = oracle_override or entry.get("oracle_command")
    scenario = None
    if synthetic and not raw_oracle:
        raw_oracle = str(root / "benchmarks" / "fuzz_corpus" / "fuzz_oracle.sh")
        scenario = entry["id"]
    if not raw_oracle:
        return unsupported(record, "case has no oracle command")
    oracle = oracle_wrapper(root, str(raw_oracle), audit_file, oracle_timeout, scenario, reset_command)

    valid, validation_elapsed, validation_error = run_exit_oracle(
        oracle, source_file, root, deadline, oracle_timeout)
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
        return record

    if tool in {"occam", "ddmin"}:
        command = [
            str(binaries[tool]), str(input_file), "--oracle", oracle, "--out", str(tool_dir),
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
                profile = stats.get("evaluation_metrics", {})
                reducer_calls = profile.get("evaluationDatabaseObservations")
                cache_hits = profile.get("evaluationCacheHits")
                hits, misses = profile.get("evaluationCacheHits"), profile.get("evaluationCacheMisses")
                candidate_checks = hits + misses if isinstance(hits, int) and isinstance(misses, int) else None
            candidate_path = tool_dir / "minimal.sql"
            metrics = stats
        else:
            values = read_json(tool_dir / "benchmark.json")
            metrics = values[0] if isinstance(values, list) and values and isinstance(values[0], dict) else None
            if metrics:
                profile = metrics.get("metricEvaluationMetrics", {})
                reducer_calls = profile.get("evaluationDatabaseObservations")
                cache_hits = profile.get("evaluationCacheHits")
                hits, misses = profile.get("evaluationCacheHits"), profile.get("evaluationCacheMisses")
                candidate_checks = hits + misses if isinstance(hits, int) and isinstance(misses, int) else None
            candidate_path = tool_dir / "minimal.sql"
    elif tool == "creduce":
        helper = root / "benchmarks" / "upstream" / "oracle_test.py"
        test_script = tool_dir / "test.sh"
        cache_file.write_text(json.dumps({hashlib.sha256(original).hexdigest(): "accepted"}), encoding="utf-8")
        helper_args = [
            "--checkpoint", str(checkpoint), "--calls", str(calls_file), "--cache", str(cache_file),
            "--timeout", str(oracle_timeout), "--deadline", str(deadline),
            "--oracle", oracle,
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
        candidate_path = checkpoint
        metrics = None
    elif tool == "sqlancer":
        adapter = root / "benchmarks" / "upstream" / "sqlancer_adapter.sh"
        helper = root / "benchmarks" / "upstream" / "oracle_test.py"
        reducer_calls_file = tool_dir / "reducer-calls.jsonl"
        helper_args = [
            sys.executable, str(helper), "--checkpoint", str(checkpoint), "--calls", str(calls_file),
            "--cache", str(cache_file), "--timeout", str(oracle_timeout), "--deadline", str(deadline),
            "--oracle", oracle,
        ]
        if max_oracle_executions is not None:
            helper_args += ["--max-executions", str(max_oracle_executions)]
        command = [
            str(adapter), "--input", str(source_file.resolve()), "--output", str(candidate.resolve()),
            "--calls", str(reducer_calls_file), "--metrics", str(metrics_file),
            "--timeout-ms", str(max(1, int(oracle_timeout * 1000))),
            "--budget-ms", str(max(1, int(remaining * 1000))),
        ]
        command += ["--", *helper_args]
        outcome = command_result(command, tool_dir, deadline)
        reduction_elapsed = outcome["elapsed"]
        call_records = read_jsonl(calls_file)
        reducer_calls = sum(1 for row in call_records if not row.get("cache_hit") and row.get("status") in
                            {"accepted", "rejected", "timeout", "tool-error"})
        candidate_checks = len(call_records)
        cache_hits = sum(1 for row in call_records if row.get("cache_hit"))
        metrics = read_json(metrics_file)
        if isinstance(metrics, dict):
            record["sqlancer_ast_unsupported_statements"] = metrics.get("ast_unsupported_statements")
            record["sqlancer_ast_statement_count"] = metrics.get("statement_count")
        candidate_path = checkpoint
    else:
        source = Path(os.environ.get("SQLREDUCE_SOURCE", f"{Path.home()}/.cache/occam/sqlreduce-{SQLREDUCE_PIN}"))
        setup_file = case_file(root, {"file": entry["setup_file"]}, manifest_dir)
        query_file = case_file(root, {"file": entry["query_file"]}, manifest_dir)
        reset = entry.get("reset_command", os.environ.get("SQLREDUCE_RESET_COMMAND", ""))
        if isinstance(reset, list):
            reset = shlex.join([str(part) for part in reset])
        command = [
            sys.executable, str(root / "benchmarks" / "upstream" / "sqlreduce_adapter.py"),
            "--source", str(source), "--setup", str(setup_file), "--query", str(query_file),
            "--output", str(candidate), "--metrics", str(metrics_file),
            "--timeout", str(oracle_timeout), "--reset-command", str(reset),
            "--expected-sqlstate", str(entry.get("expected_sqlstate", "")),
        ]
        outcome = command_result(command, root, deadline)
        if outcome["returncode"] == 78:
            outcome["status"] = "unsupported"
        elif outcome["returncode"] == 124:
            outcome["status"] = "timeout"
        reduction_elapsed = outcome["elapsed"]
        metrics = read_json(metrics_file)
        if isinstance(metrics, dict):
            reducer_calls = metrics.get("oracle_calls")
            candidate_checks = metrics.get("candidate_checks")
            cache_hits = metrics.get("cache_hits")
        candidate_path = checkpoint

    if tool == "sqlreduce" and isinstance(metrics, dict):
        record["reset_time_sec"] = metrics.get("reset_time_sec")
        record["reset_calls"] = metrics.get("reset_calls")
        record["reset_accounting"] = "reset command measured by SQLreduce adapter"
        record["reduction_track"] = metrics.get("reduction_track", "fixed_setup_query")

    if tool in {"occam", "ddmin"} and candidate_path and candidate_path.is_file():
        shutil.copyfile(candidate_path, checkpoint)
    if tool == "sqlreduce" and candidate.is_file() and not checkpoint.is_file():
        shutil.copyfile(candidate, checkpoint)

    tool_status = outcome["status"]
    if tool_status == "completed" and outcome["returncode"] == 0:
        candidate_bytes = candidate_path.read_bytes() if candidate_path and candidate_path.is_file() else original
        candidate.write_bytes(candidate_bytes)
        valid, final_validation_elapsed, validation_error = run_exit_oracle(
            oracle, candidate, root, deadline, oracle_timeout)
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
        detail = (outcome.get("stderr") or outcome.get("stdout") or "").strip()
        record["status_reason"] = detail[:500] if detail else tool_status
        candidate_bytes = checkpoint.read_bytes() if checkpoint.is_file() else original
        record["checkpoint_used"] = "last-valid" if candidate_bytes != original else "initial"
        record["predicate_passed"] = None if tool_status in {"timeout", "tool-error"} else True
        record["tool_candidate_valid"] = None

    record["oracle_calls"] = reducer_calls
    record["candidate_checks"] = candidate_checks
    record["oracle_cache_hits"] = cache_hits
    record["validation_calls"] = validation_calls
    record["validation_time_sec"] = round(validation_time, 4)
    record["reduction_time_sec"] = round(reduction_elapsed, 4)
    record["wall_time_sec"] = round(time.monotonic() - started, 4)
    record["oracle_calls_scope"] = "uncached reducer callbacks only; initial checks and validation calls excluded"
    reducer_timeout_count = sum(1 for row in read_jsonl(calls_file) if row.get("status") == "timeout")
    audit_timeout_count = sum(1 for row in read_jsonl(audit_file) if row.get("status") == "timeout")
    record["oracle_timeouts"] = max(reducer_timeout_count, audit_timeout_count)
    if any(row.get("status") == "execution-limit" for row in read_jsonl(calls_file)):
        record["status_reason"] = record.get("status_reason") or "max oracle executions reached"
    result_file = tool_dir / "result.sql"
    result_file.write_bytes(candidate_bytes)
    record["result_file"] = str(result_file)
    record["oracle_audit_file"] = str(audit_file)
    set_size_metrics(record, original, candidate_bytes, dialect)
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
                for result in results:
                    output.write(json.dumps(result, sort_keys=True) + "\n")
                    output.flush()
    print(f"Completed {len(entries) * len(selected_tools)} case/tool records in {time.monotonic() - started:.2f}s")
    print(f"Results written to: {jsonl_path}")


if __name__ == "__main__":
    main()

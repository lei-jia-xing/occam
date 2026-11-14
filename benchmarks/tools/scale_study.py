#!/usr/bin/env python3
"""Reproduce SQLite scale/ablation workloads using the existing reducer runners."""
from __future__ import annotations

from contextlib import closing
import hashlib
import json
import os
from pathlib import Path
import platform
import sqlite3
import sys
import uuid

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = Path(__file__).resolve()


def execute_sql(connection, sql):
    pending = ""
    rows = None
    for character in sql:
        pending += character
        if character == ";" and sqlite3.complete_statement(pending):
            cursor = connection.execute(pending)
            if cursor.description is not None:
                rows = [list(row) for row in cursor.fetchall()]
            pending = ""
    if pending.strip():
        cursor = connection.execute(pending)
        if cursor.description is not None:
            rows = [list(row) for row in cursor.fetchall()]
    return rows


def state_signature(connection):
    import hashlib
    return {
        "dump_sha256": hashlib.sha256("\n".join(connection.iterdump()).encode()).hexdigest(),
        "user_version": connection.execute("PRAGMA user_version").fetchone()[0],
        "foreign_keys": connection.execute("PRAGMA foreign_keys").fetchone()[0],
    }


def seed_database(path, case):
    with closing(sqlite3.connect(path, isolation_level=None)) as connection:
        connection.execute("PRAGMA foreign_keys=ON")
        execute_sql(connection, case["setup_sql"])
        connection.execute("CREATE TABLE study_marker (case_id TEXT)")
        connection.execute("INSERT INTO study_marker VALUES (?)", (case["marker"],))
        return state_signature(connection)


def load_case(path):
    case = json.loads(Path(path).read_text(encoding="utf-8"))
    if not isinstance(case, dict) or not isinstance(case.get("expected_rows"), list):
        raise ValueError("case metadata requires an expected_rows array")
    if case.get("reset_mode") not in {None, "fresh", "template"}:
        raise ValueError("unknown reset strategy")
    return case


def work_database():
    workdir = os.environ.get("OCCAM_WORKDIR")
    if not workdir or not Path(workdir).is_dir():
        raise ValueError("OCCAM_WORKDIR is required for file-state observations")
    return Path(workdir) / "study.sqlite"


def reset(path):
    import shutil
    case = load_case(path)
    if case.get("reset_failure"):
        raise RuntimeError("intentional reset-failure isolation case")
    database = work_database()
    if database.exists():
        database.unlink()
    if case.get("reset_mode") == "template":
        shutil.copyfile(case["template"], database)
    else:
        seed_database(database, case)
    return 0


def oracle(path):
    case = load_case(path)
    if case.get("reset_mode"):
        database = work_database()
        if not database.is_file():
            raise RuntimeError("reset did not create the observation database")
        connection = sqlite3.connect(database, isolation_level=None)
    else:
        connection = sqlite3.connect(":memory:", isolation_level=None)
    try:
        connection.execute("PRAGMA foreign_keys=ON")
        if case.get("reset_mode") and state_signature(connection) != case["state_signature"]:
            raise RuntimeError("schema/data/sequence/PRAGMA state was not restored")
        try:
            rows = execute_sql(connection, sys.stdin.read())
        except (sqlite3.Error, UnicodeError):
            return 1
        return 0 if rows == case["expected_rows"] else 1
    finally:
        connection.close()


def runner_module():
    from importlib.util import module_from_spec, spec_from_file_location
    spec = spec_from_file_location("occam_study_runner", ROOT / "benchmarks/fuzz_corpus/run_docker_benchmark.py")
    module = module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def prepare(directory, corpus_size):
    import hashlib
    import shlex
    runner = runner_module()
    directory.mkdir(parents=True, exist_ok=True)
    if any(directory.iterdir()):
        raise ValueError("choose an empty study directory; existing evidence is preserved")
    source = ROOT / "test/golden/fuzz_golden_03_deep_expressions.golden.sql"
    statements = runner.split_sql_statements(source.read_text(), "sqlite")
    setup = "\n".join(statements[:-1]) + "\n"
    groups = {"scale": [], "corpus": [], "profiles": [], "resets": []}

    def add(group, name, sql, parameters, expected=None, state=None):
        sql_path = directory / f"{name}.sql"
        metadata_path = directory / f"{name}.case.json"
        sql_path.write_text(sql, encoding="utf-8")
        case = {"id": name, "parameters": parameters, "file": str(sql_path),
                "input_sha256": hashlib.sha256(sql.encode()).hexdigest()}
        if state:
            case.update(state)
            template = directory / f"{name}.template.sqlite"
            case["state_signature"] = seed_database(template, case)
            case["template"] = str(template)
            with closing(sqlite3.connect(template, isolation_level=None)) as connection:
                expected = execute_sql(connection, sql)
        elif expected is None:
            with closing(sqlite3.connect(":memory:", isolation_level=None)) as connection:
                expected = execute_sql(connection, sql)
        case["expected_rows"] = expected
        metadata_path.write_text(json.dumps(case, sort_keys=True) + "\n")
        entry = {"id": name, "file": str(sql_path), "dialect": "sqlite",
                 "evidence_class": "generated_db_execution", "oracle_kind": "sqlite_rowset",
                 "oracle_command": shlex.join([sys.executable, str(SCRIPT), "oracle", str(metadata_path)]),
                 "case_metadata": str(metadata_path), "parameters": parameters,
                 "reduction_track": "fixed_setup_query" if state else "whole_script"}
        if state:
            entry["reset_command"] = shlex.join([sys.executable, str(SCRIPT), "reset", str(metadata_path)])
        groups[group].append(entry)
        return entry

    def noisy(noise_statements, objects=1, depth=0, offset=0):
        definitions = [f"CREATE TABLE study_noise_{i} (v INT);" for i in range(objects)]
        writes = [f"INSERT INTO study_noise_{i % objects} VALUES ({i});"
                  for i in range(max(0, noise_statements - objects))]
        expression = "base_val"
        for _ in range(depth):
            expression = f"({expression} + 0)"
        query = f"SELECT id + {offset}, {expression} FROM expr_calc_inputs;\n"
        return setup + "\n".join(definitions + writes) + "\n" + query

    for count in (64, 512, 4096):
        add("scale", f"statements-{count}", noisy(count), {"dimension": "statements", "noise_statements": count, "noise_objects": 1, "depth": 0})
    for objects in (1, 8, 32):
        add("scale", f"objects-{objects}", noisy(128, objects), {"dimension": "objects", "noise_statements": 128, "noise_objects": objects, "depth": 0})
    for depth in (1, 8, 24):
        add("scale", f"depth-{depth}", noisy(16, 1, depth), {"dimension": "depth", "noise_statements": 16, "noise_objects": 1, "depth": depth})
    for fan_in in (1, 2, 4):
        views = ["CREATE VIEW study_v0 AS SELECT id, base_val FROM expr_calc_inputs;"]
        for index in range(1, 5):
            parents = list(range(index - 1, max(-1, index - fan_in - 1), -1))
            query = f"SELECT p0.id, p0.base_val FROM study_v{parents[0]} p0"
            for alias, parent in enumerate(parents[1:], 1):
                query += f" JOIN study_v{parent} p{alias} ON p{alias}.id = p0.id"
            views.append(f"CREATE VIEW study_v{index} AS {query};")
        add("scale", f"density-{fan_in}", setup + "\n".join(views) + "\nSELECT id, base_val FROM study_v4;\n",
            {"dimension": "dependency_density", "views": 5, "maximum_fan_in": fan_in})
    for index in range(corpus_size):
        add("corpus", f"corpus-{index:04}", noisy(4 + index % 4, 1, index % 3, index + 1),
            {"source": source.name, "case_index": index, "result_offset": index + 1})
    add("corpus", "reset-failure", "SELECT 42;\n", {"injected_outcome": "reset-failure"},
        state={"reset_mode": "fresh", "reset_failure": True, "marker": "reset-failure", "setup_sql": "SELECT 1;"})
    count = 1_000_000_000
    add("corpus", "oracle-timeout", f"WITH RECURSIVE t(n) AS (SELECT 1 UNION ALL SELECT n+1 FROM t WHERE n<{count}) SELECT SUM(n) FROM t;\n",
        {"injected_outcome": "initial SQLite execution timeout", "recursion_rows": count}, expected=[[count * (count + 1) // 2]])
    add("profiles", "false-barrier", "SELECT 111;\nBEGIN;\nSELECT 42;\nCOMMIT;\n", {"shape": "false_conservative_dependency"})
    for name in ("statements-64", "density-1", "density-4", "depth-8"):
        groups["profiles"].append(next(entry for entry in groups["scale"] if entry["id"] == name))
    add("profiles", "golden-expression", source.read_text(), {"source": source.name})
    reset_setup = setup + """
CREATE INDEX study_idx ON expr_calc_inputs(id);
CREATE TABLE study_log (v INT);
CREATE TRIGGER study_trigger AFTER INSERT ON expr_calc_inputs BEGIN INSERT INTO study_log VALUES(NEW.id); END;
CREATE TABLE study_sequence (id INTEGER PRIMARY KEY AUTOINCREMENT);
INSERT INTO study_sequence DEFAULT VALUES;
PRAGMA user_version=42;
"""
    for mode in ("fresh", "template"):
        add("resets", f"reset-{mode}", "SELECT id, base_val + 0 FROM expr_calc_inputs;\n",
            {"reset_strategy": mode, "track": "fixed_setup_query"},
            state={"reset_mode": mode, "marker": "reset-comparison", "setup_sql": reset_setup})
    for group, entries in groups.items():
        (directory / f"{group}.json").write_text(json.dumps({"cases": entries}, indent=2) + "\n")
    (directory / "provenance.json").write_text(json.dumps({
        "source_fixture": str(source.relative_to(ROOT)), "source_sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
        "python": sys.version, "sqlite": sqlite3.sqlite_version, "corpus_size": corpus_size,
        "script_sha256": hashlib.sha256(SCRIPT.read_bytes()).hexdigest(), "scope": "generated SQL execution; not DBMS bug evidence",
    }, indent=2) + "\n")
    print(json.dumps({group: len(entries) for group, entries in groups.items()}, sort_keys=True))


def profiles(manifest, binary, output, timeout, budget, executions, profile_names, repetitions=1, outer_timeout=None):
    import math
    import time

    runner = runner_module()
    if not all(isinstance(value, (int, float)) and not isinstance(value, bool)
               and math.isfinite(value) and value > 0 for value in (timeout, budget)):
        raise ValueError("timeout and budget must be finite and positive")
    if type(executions) is not int or executions < 1:
        raise ValueError("max oracle executions must be a positive integer")
    if type(repetitions) is not int or repetitions < 1:
        raise ValueError("repetitions must be a positive integer")
    if outer_timeout is None:
        outer_timeout = budget + max(timeout, 1.0)
    if not isinstance(outer_timeout, (int, float)) or isinstance(outer_timeout, bool) \
            or not math.isfinite(outer_timeout) or outer_timeout <= 0:
        raise ValueError("outer timeout must be finite and positive")

    def safe_component(value):
        return (isinstance(value, str) and value not in {"", ".", ".."}
                and Path(value).name == value and "/" not in value
                and "\\" not in value and "\0" not in value)

    if not isinstance(profile_names, list) or not profile_names \
            or any(not safe_component(name) for name in profile_names):
        raise ValueError("select one or more path-safe internal profile names")
    if len(set(profile_names)) != len(profile_names):
        raise ValueError("duplicate profiles would overwrite child evidence")

    manifest = manifest.resolve()
    binary = binary.resolve()
    output = output.resolve()
    manifest_bytes = manifest.read_bytes()
    document = json.loads(manifest_bytes)
    if not isinstance(document, dict) or not isinstance(document.get("cases"), list) or not document["cases"]:
        raise ValueError("profile manifest requires a nonempty cases array")

    cases = []
    seen_ids = set()
    manifest_oracle = document.get("oracle_command")
    for source in document["cases"]:
        if not isinstance(source, dict):
            raise ValueError("each profile case must be an object")
        case_id = source.get("id")
        if not safe_component(case_id):
            raise ValueError(f"unsafe profile case id: {case_id!r}")
        if case_id in seen_ids:
            raise ValueError(f"duplicate profile case id: {case_id}")
        seen_ids.add(case_id)
        oracle_command = source.get("oracle_command", manifest_oracle)
        if not isinstance(oracle_command, str) or not oracle_command:
            raise ValueError(f"case {case_id} requires an oracle command")
        dialect = source.get("dialect")
        if not isinstance(dialect, str) or not dialect:
            raise ValueError(f"case {case_id} requires a dialect")
        track = source.get("reduction_track", "whole_script")
        if not isinstance(track, str) or not track:
            raise ValueError(f"case {case_id} has an invalid reduction_track")
        if source.get("reset_command") is not None and not isinstance(source["reset_command"], str):
            raise ValueError(f"case {case_id} has an invalid reset_command")
        case_path = runner.case_file(ROOT, source, manifest.parent).resolve()
        if not case_path.is_file():
            raise ValueError(f"case {case_id} input file does not exist: {case_path}")
        original = case_path.read_bytes()
        entry = dict(source)
        entry.update({"file": str(case_path), "oracle_command": oracle_command,
                      "reduction_track": track})
        reset_command = entry.get("reset_command")
        cases.append({
            "entry": entry,
            "id": case_id,
            "input_path": case_path,
            "original": original,
            "input_sha256": hashlib.sha256(original).hexdigest(),
            "predicate_command_sha256": hashlib.sha256(oracle_command.encode("utf-8")).hexdigest(),
            "reset_command_sha256": (
                hashlib.sha256(reset_command.encode("utf-8")).hexdigest() if reset_command else None
            ),
        })

    binary_sha256 = None
    try:
        with binary.open("rb") as native:
            digest = hashlib.sha256()
            for chunk in iter(lambda: native.read(1024 * 1024), b""):
                digest.update(chunk)
            binary_sha256 = digest.hexdigest()
    except OSError:
        pass
    run_id = uuid.uuid4().hex
    output.mkdir(parents=True, exist_ok=True)
    if any(output.iterdir()):
        raise ValueError("choose an empty profile directory; existing evidence is preserved")
    run_metadata = {
        "schema_version": 1,
        "run_id": run_id,
        "experiment_line": "internal",
        "execution_mode": "native",
        "runtime": {
            "python_executable": sys.executable,
            "python_version": sys.version,
            "platform": platform.platform(),
        },
        "native_binary": {"path": str(binary), "sha256": binary_sha256},
        "configuration": {
            "manifest": str(manifest),
            "manifest_sha256": hashlib.sha256(manifest_bytes).hexdigest(),
            "predicate_timeout_sec": timeout,
            "budget_sec": budget,
            "max_oracle_executions": executions,
            "outer_timeout_sec": outer_timeout,
            "profiles": profile_names,
            "repetitions": repetitions,
            "case_order": [case["id"] for case in cases],
            "cases": [{
                "case_id": case["id"],
                "input_path": str(case["input_path"]),
                "input_sha256": case["input_sha256"],
                "reduction_track": case["entry"]["reduction_track"],
                "predicate_command_sha256": case["predicate_command_sha256"],
                "reset_command_sha256": case["reset_command_sha256"],
            } for case in cases],
            "command_fingerprint_scope": "configured command strings only; transitive scripts/dependencies are not fingerprinted",
            "child_order": "manifest case order, selected profile order, repetition ascending",
            "resources": {"serial": True, "jobs": 1, "GHCRTS": "-N1", "cpu_affinity": "inherited"},
            "warmup": "none",
            "native_cache_policy": "fresh process-local cache per child; effective setting is in the metric",
            "os_page_cache_policy": "not reset; repeated children may warm filesystem cache",
            "fresh_process_per_child": True,
            "validation_phases": {
                "preflight": "one physical initial predicate check per child; debited to its limits",
                "reduction": "uncached reducer observations; measured separately from preflight and final checks",
                "final": "up to two fresh checks when status and budget permit; debited to its limits",
            },
            "count_scope": {
                "oracle_calls": "reducer-scope predicate observations only",
                "preflight_calls_debited": "physical preflight attempts",
                "validation_calls": "final-validation predicate observations",
                "max_oracle_executions": "shared per-child cap includes preflight, reducer attempts, and final validation",
                "statement_counts": "native typed countStatements(dialect) metrics only",
                "byte_counts": "exact input and minimal.sql bytes",
                "token_counts": "unavailable; null",
            },
            "outer_timeout_cleanup_scope": "Linux attempts /proc enumeration plus owned POSIX group; other POSIX systems signal the group; Windows signals only the direct process; escaped/reparented descendants are not guaranteed",
        },
    }
    (output / "run.json").write_text(json.dumps(run_metadata, sort_keys=True) + "\n")

    def finite_seconds(value):
        return value if type(value) in (int, float) and math.isfinite(value) and value >= 0 else None

    def populate_native_metrics(row, metric, case_dir, case, returncode, candidate):
        evaluation = metric.get("metricEvaluationMetrics")
        settings = metric.get("metricReductionSettings")
        if not isinstance(evaluation, dict):
            return None
        native_status = evaluation.get("evaluationStatus")
        valid_statuses = {"completed", "timeout", "execution-budget", "error",
                          "validation-failed", "validation-incomplete"}
        if not isinstance(native_status, str) or native_status not in valid_statuses:
            return None
        artifact = metric.get("metricSQLArtifact")
        expected_path = (case_dir / "minimal.sql").resolve()
        path = Path(artifact) if isinstance(artifact, str) else None
        if path is not None and not path.is_absolute():
            path = ROOT / path
        if path is None or path.resolve() != expected_path:
            return None
        reason = evaluation.get("evaluationFailure")
        if not isinstance(reason, str):
            reason = None
        observation = {
            "source": "native-profile",
            "status": native_status,
            "reason": reason,
            "raw_metric_file": str(case_dir / "benchmark.json"),
        }
        row["native_observation"] = observation
        row["terminal_observation"] = observation
        row["settings"] = settings
        row["profile_family"] = None
        row["intra_statement_edits"] = None
        if isinstance(settings, dict):
            kind = settings.get("kind")
            if kind in {"sequential-deletion", "statement-ddmin"}:
                row["profile_family"] = "statement-only-control"
                row["intra_statement_edits"] = False
            elif kind == "shared-reducer":
                row["profile_family"] = "shared-reducer-profile"
                if type(settings.get("intra_statement")) is bool:
                    row["intra_statement_edits"] = settings["intra_statement"]
        budget_metrics = evaluation.get("evaluationBudget")
        if not isinstance(budget_metrics, dict):
            budget_metrics = {}
        metric_count = runner.metric_count
        row["oracle_calls"] = metric_count(evaluation.get("evaluationDatabaseObservations"))
        row["candidate_checks"] = metric_count(evaluation.get("evaluationOracleAttempts"))
        row["preflight_calls_debited"] = metric_count(budget_metrics.get("evaluationPreflightAttempts"))
        row["validation_calls"] = metric_count(evaluation.get("evaluationValidationDatabaseObservations"))
        row["validation_observations"] = metric_count(evaluation.get("evaluationValidationObservations"))
        counts = (row["preflight_calls_debited"], row["oracle_calls"], row["validation_calls"])
        row["total_predicate_observations"] = sum(counts) if all(count is not None for count in counts) else None
        row["oracle_timeouts"] = metric_count(evaluation.get("evaluationTimedOutObservations"))
        row["oracle_cache_hits"] = metric_count(evaluation.get("evaluationCacheHits"))
        row["native_validation_passed"] = (
            evaluation.get("evaluationValidationPassed")
            if type(evaluation.get("evaluationValidationPassed")) is bool else None
        )
        preflight_micros = metric_count(budget_metrics.get("evaluationPreflightMicros"))
        row["preflight_time_sec"] = preflight_micros / 1000000 if preflight_micros is not None else None
        row["validation_time_sec"] = finite_seconds(evaluation.get("evaluationValidationSeconds"))
        row["reset_preflight_sec"] = finite_seconds(evaluation.get("evaluationPreflightResetSeconds"))
        row["reset_reduction_sec"] = finite_seconds(evaluation.get("evaluationResetSeconds"))
        row["reset_validation_sec"] = finite_seconds(evaluation.get("evaluationValidationResetSeconds"))
        reset_parts = (row["reset_preflight_sec"], row["reset_reduction_sec"], row["reset_validation_sec"])
        row["reset_time_sec"] = sum(reset_parts) if all(part is not None for part in reset_parts) else None
        row["reduction_time_sec"] = finite_seconds(evaluation.get("evaluationReductionSeconds"))
        row["wall_time_sec"] = finite_seconds(evaluation.get("evaluationTotalSeconds"))
        row["child_reported_wall_time_sec"] = row["wall_time_sec"]
        profile_artifact = metric.get("metricMetricsArtifact")
        row["profile_metrics_file"] = profile_artifact if isinstance(profile_artifact, str) else None
        row["reset_calls"] = None if case["entry"].get("reset_command") else 0
        row["reset_accounting"] = (
            "reset duration measured by phase; reset call count uninstrumented"
            if case["entry"].get("reset_command")
            else "no reset command; predicate-internal reset is unknown"
        )
        initial_stmts = metric_count(metric.get("metricInitialStmts"))
        reduced_stmts = metric_count(metric.get("metricReducedStmts"))
        row["initial_stmts"] = initial_stmts
        row["reduced_stmts"] = reduced_stmts
        row["scale"] = f"{initial_stmts} stmts" if initial_stmts is not None else None
        row["stmt_red_pct"] = (
            round((1 - reduced_stmts / initial_stmts) * 100, 2)
            if initial_stmts is not None and reduced_stmts is not None and initial_stmts else None
        )
        row["statement_count_provenance"] = (
            "native BenchmarkMetric countStatements(dialect); nonnegative integer fields only"
            if initial_stmts is not None and reduced_stmts is not None else None
        )
        row["initial_bytes"] = len(case["original"])
        row["reduced_bytes"] = len(candidate) if candidate is not None else None
        row["byte_red_pct"] = (
            round((1 - len(candidate) / len(case["original"])) * 100, 2)
            if candidate is not None and case["original"] else None
        )
        row["byte_count_provenance"] = "exact source and minimal.sql artifact bytes"
        row["initial_tokens"] = None
        row["reduced_tokens"] = None
        row["token_count_provenance"] = None
        validation_complete = (
            row["native_validation_passed"] is True
            and row["preflight_calls_debited"] == 1
            and row["validation_observations"] is not None
            and row["validation_observations"] >= 2
            and row["validation_calls"] is not None
            and row["validation_calls"] >= 2
        )
        if native_status == "completed":
            if returncode != 0:
                row["status"] = "tool-error"
                row["status_reason"] = "native process did not exit successfully"
            elif not validation_complete:
                row["status"] = "tool-error"
                row["status_reason"] = "completed profile lacks fresh preflight/final-validation evidence"
            elif candidate is None:
                row["status"] = "tool-error"
                row["status_reason"] = "completed native result is missing minimal.sql"
            else:
                row["status"] = "completed"
                row["predicate_passed"] = True
                row["status_reason"] = None
        else:
            row["status"] = native_status
            row["status_reason"] = reason
        return observation
    started = time.monotonic()
    statuses = {}
    records = 0
    validated = 0
    child_order = 0
    with (output / "results.jsonl").open("w", encoding="utf-8") as stream:
        for case in cases:
            entry = case["entry"]
            for profile_name in profile_names:
                for repetition in range(1, repetitions + 1):
                    child_order += 1
                    child_id = uuid.uuid4().hex
                    case_dir = (output / "children" / case["id"] / f"profile-{profile_name}"
                                / f"repetition-{repetition:04}")
                    case_dir.mkdir(parents=True)
                    command = [str(binary), str(case["input_path"]), "--profile", profile_name,
                               "--oracle", entry["oracle_command"], "--dialect", entry["dialect"],
                               "--timeout", str(timeout), "--budget", str(budget),
                               "--max-oracle-executions", str(executions),
                               "--log-format", "json", "--out", str(case_dir)]
                    if entry.get("reset_command"):
                        command += ["--reset", entry["reset_command"]]
                    (case_dir / "command.json").write_text(json.dumps({
                        "run_id": run_id,
                        "child_id": child_id,
                        "order": child_order,
                        "case_id": case["id"],
                        "profile": profile_name,
                        "repetition": repetition,
                        "experiment_line": "internal",
                        "execution_mode": "native",
                        "environment": {"GHCRTS": "-N1"},
                        "predicate_command_sha256": case["predicate_command_sha256"],
                        "reset_command_sha256": case["reset_command_sha256"],
                        "command": command,
                    }, sort_keys=True) + "\n")
                    child_started = time.monotonic()
                    child_env = dict(os.environ, GHCRTS="-N1")
                    try:
                        process = runner.command_result(
                            command, ROOT, child_started + outer_timeout,
                            allow_nonzero=True, env=child_env,
                        )
                    except Exception as exc:
                        process = {
                            "status": "tool-error", "elapsed": time.monotonic() - child_started,
                            "returncode": None, "stdout": "", "stderr": str(exc),
                            "cleanup_status": "unknown", "cleanup_error": str(exc),
                            "cleanup_scope": "process helper failed; cleanup not observed",
                        }
                    stdout = process.get("stdout") if isinstance(process.get("stdout"), str) else ""
                    stderr = process.get("stderr") if isinstance(process.get("stderr"), str) else ""
                    stdout_file = case_dir / "stdout.log"
                    stderr_file = case_dir / "stderr.log"
                    stdout_file.write_text(stdout)
                    stderr_file.write_text(stderr)
                    returncode = process.get("returncode") if type(process.get("returncode")) is int else None
                    outer_elapsed = finite_seconds(process.get("elapsed"))
                    if outer_elapsed is None:
                        outer_elapsed = time.monotonic() - child_started
                    row = runner.base_record(entry, profile_name, timeout, budget, executions)
                    artifact_dir = str(case_dir.resolve())
                    raw_metric_file = case_dir / "benchmark.json"
                    result_file = case_dir / "minimal.sql"
                    candidate = None
                    try:
                        resolved_result = result_file.resolve()
                        if resolved_result.is_relative_to(case_dir.resolve()) and resolved_result.is_file():
                            candidate = resolved_result.read_bytes()
                    except OSError:
                        pass
                    row.update({
                        "run_id": run_id,
                        "child_id": child_id,
                        "order": child_order,
                        "case_id": case["id"],
                        "repetition": repetition,
                        "experiment_line": "internal",
                        "execution_mode": "native",
                        "profile": profile_name,
                        "profile_family": None,
                        "intra_statement_edits": None,
                        "reduction_track": entry["reduction_track"],
                        "input_path": str(case["input_path"]),
                        "input_sha256": case["input_sha256"],
                        "predicate_command_sha256": case["predicate_command_sha256"],
                        "reset_command_sha256": case["reset_command_sha256"],
                        "output_sha256": hashlib.sha256(candidate).hexdigest() if candidate is not None else None,
                        "artifact_directory": artifact_dir,
                        "command_file": str(case_dir / "command.json"),
                        "stdout_file": str(stdout_file),
                        "stderr_file": str(stderr_file),
                        "raw_metric_file": str(raw_metric_file),
                        "profile_metrics_file": None,
                        "result_file": str(result_file),
                        "result_artifact_present": candidate is not None,
                        "outer_timeout_sec": outer_timeout,
                        "outer_wall_time_sec": outer_elapsed,
                        "child_reported_wall_time_sec": None,
                        "native_process_returncode": returncode,
                        "cleanup_status": process.get("cleanup_status"),
                        "cleanup_reason": process.get("cleanup_error"),
                        "cleanup_scope": process.get("cleanup_scope"),
                        "native_observation": None,
                        "terminal_observation": None,
                        "preflight_calls_debited": None,
                        "validation_calls": None,
                        "validation_observations": None,
                        "total_predicate_observations": None,
                        "candidate_checks": None,
                        "oracle_calls": None,
                        "oracle_timeouts": None,
                        "oracle_cache_hits": None,
                        "native_validation_passed": None,
                        "preflight_time_sec": None,
                        "validation_time_sec": None,
                        "reset_preflight_sec": None,
                        "reset_reduction_sec": None,
                        "reset_validation_sec": None,
                        "reset_time_sec": None,
                        "reduction_time_sec": None,
                        "wall_time_sec": None,
                        "initial_stmts": None,
                        "reduced_stmts": None,
                        "stmt_red_pct": None,
                        "statement_count_provenance": None,
                        "initial_bytes": len(case["original"]),
                        "reduced_bytes": len(candidate) if candidate is not None else None,
                        "byte_red_pct": (
                            round((1 - len(candidate) / len(case["original"])) * 100, 2)
                            if candidate is not None and case["original"] else None
                        ),
                        "byte_count_provenance": "exact source and minimal.sql artifact bytes",
                        "initial_tokens": None,
                        "reduced_tokens": None,
                        "token_count_provenance": None,
                        "preflight_scope": "one physical initial predicate check per child",
                        "oracle_calls_scope": "uncached reducer-scope predicate observations only",
                        "validation_calls_scope": "fresh final-validation predicate observations only",
                        "execution_cap_scope": "one child cap includes preflight, reducer, and final validation",
                        "reset_calls": None,
                        "reset_calls_scope": "native reset call count uninstrumented",
                        "parameters": entry.get("parameters"),
                    })
                    native = runner.read_json(raw_metric_file)
                    metric = native[0] if isinstance(native, list) and len(native) == 1 \
                        and isinstance(native[0], dict) else None
                    malformed_reason = None
                    if metric is None:
                        malformed_reason = "selected internal profile metric is missing or malformed"
                    elif metric.get("metricAlgorithm") != profile_name \
                            or metric.get("metricExperimentLine") != "internal":
                        malformed_reason = "fresh metric does not match the selected internal profile"
                    else:
                        try:
                            native_observation = populate_native_metrics(
                                row, metric, case_dir, case, returncode, candidate
                            )
                            if native_observation is None:
                                malformed_reason = "fresh metric has an unknown status or artifact path"
                        except Exception as exc:
                            malformed_reason = f"could not read fresh profile evidence: {exc}"
                    outer_status = process.get("status")
                    if outer_status == "timeout":
                        row["status"] = "timeout"
                        row["status_reason"] = "native child exceeded the outer wall-time limit"
                        row["terminal_observation"] = {
                            "source": "outer-runner", "status": "timeout",
                            "reason": row["status_reason"], "raw_metric_file": str(raw_metric_file),
                        }
                    elif outer_status != "completed":
                        row["status"] = "tool-error"
                        row["status_reason"] = "native child failed to start or complete"
                        row["terminal_observation"] = {
                            "source": "outer-runner", "status": "tool-error",
                            "reason": row["status_reason"], "raw_metric_file": str(raw_metric_file),
                        }
                    elif malformed_reason is not None:
                        row["status"] = "tool-error"
                        row["status_reason"] = malformed_reason
                        row["terminal_observation"] = {
                            "source": "outer-runner", "status": "tool-error",
                            "reason": malformed_reason, "raw_metric_file": str(raw_metric_file),
                        }
                    else:
                        row["terminal_observation"] = row["native_observation"]
                    stream.write(json.dumps(row, sort_keys=True) + "\n")
                    stream.flush()
                    records += 1
                    validated += row["status"] == "completed" and row["predicate_passed"] is True
                    statuses[row["status"]] = statuses.get(row["status"], 0) + 1
    elapsed = time.monotonic() - started
    summary = {
        "schema_version": 1,
        "run_id": run_id,
        "experiment_line": "internal",
        "execution_mode": "native",
        "input_count": len(cases),
        "profile_count": len(profile_names),
        "repetitions": repetitions,
        "expected_record_count": len(cases) * len(profile_names) * repetitions,
        "record_count": records,
        "wall_time_sec": elapsed,
        "wall_time_scope": "observed serial batch makespan, distinct from per-child native wall_time_sec",
        "inputs_per_second": len(cases) / elapsed if elapsed else None,
        "records_per_second": records / elapsed if elapsed else None,
        "validated_reduction_count": validated,
        "nonvalidated_record_count": records - validated,
        "status_counts": statuses,
    }
    (output / "results.summary.json").write_text(json.dumps(summary, sort_keys=True) + "\n")
    print(json.dumps(summary, sort_keys=True))


def main():
    if len(sys.argv) == 3 and sys.argv[1] in {"oracle", "reset"}:
        try:
            return (oracle if sys.argv[1] == "oracle" else reset)(sys.argv[2])
        except Exception as exc:
            print(f"study {sys.argv[1]}: {exc}", file=sys.stderr)
            return 125
    import argparse
    import math
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    prepare_parser = commands.add_parser("prepare")
    prepare_parser.add_argument("directory", type=Path)
    prepare_parser.add_argument("--corpus-size", type=int, default=128)
    profile_parser = commands.add_parser("profiles")
    profile_parser.add_argument("manifest", type=Path)
    profile_parser.add_argument("--binary", type=Path, required=True)
    profile_parser.add_argument("--out", type=Path, required=True)
    profile_parser.add_argument("--profile", action="append", required=True, help="One internal profile per child; repeat to select more")
    profile_parser.add_argument("--repetitions", type=int, default=1, help="Fresh child runs per case/profile; default 1")
    profile_parser.add_argument("--timeout", type=float, default=2)
    profile_parser.add_argument("--budget", type=float, default=30)
    profile_parser.add_argument("--max-oracle-executions", type=int, default=500)
    profile_parser.add_argument("--outer-timeout", type=float, default=None,
                                help="Outer child deadline in seconds (default: budget + max(timeout, 1))")
    args = parser.parse_args()
    if args.command == "prepare":
        if args.corpus_size < 1:
            parser.error("--corpus-size must be positive")
        prepare(args.directory.resolve(), args.corpus_size)
    else:
        if not math.isfinite(args.timeout) or not math.isfinite(args.budget) or args.timeout <= 0 or args.budget <= 0 \
                or args.max_oracle_executions < 1 or args.repetitions < 1 \
                or (args.outer_timeout is not None and (
                    not math.isfinite(args.outer_timeout) or args.outer_timeout <= 0
                )):
            parser.error("limits and repetitions must be finite and positive")
        profiles(args.manifest.resolve(), args.binary.resolve(), args.out.resolve(), args.timeout,
                 args.budget, args.max_oracle_executions, args.profile, args.repetitions, args.outer_timeout)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

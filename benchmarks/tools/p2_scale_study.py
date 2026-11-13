#!/usr/bin/env python3
"""Reproduce SQLite scale/ablation workloads using the existing reducer runners."""
from __future__ import annotations

from contextlib import closing
import json
import os
from pathlib import Path
import sqlite3
import sys

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


def profiles(manifest, binary, output, timeout, budget, executions):
    import subprocess
    import time
    runner = runner_module()
    entries = json.loads(manifest.read_text())["cases"]
    output.mkdir(parents=True, exist_ok=True)
    if any(output.iterdir()):
        raise ValueError("choose an empty profile directory; existing evidence is preserved")
    started = time.monotonic()
    statuses = {}
    records = 0
    validated = 0
    with (output / "results.jsonl").open("w") as stream:
        for entry in entries:
            case_dir = output / entry["id"]
            case_dir.mkdir(exist_ok=True)
            command = [str(binary), entry["file"], "--oracle", entry["oracle_command"], "--dialect", "sqlite",
                       "--timeout", str(timeout), "--budget", str(budget), "--max-oracle-executions", str(executions),
                       "--log-format", "json", "--out", str(case_dir)]
            if entry.get("reset_command"):
                command += ["--reset", entry["reset_command"]]
            (case_dir / "command.json").write_text(json.dumps(command) + "\n")
            result = subprocess.run(command, cwd=ROOT, env=dict(os.environ, GHCRTS="-N1"), capture_output=True, text=True)
            (case_dir / "stdout.log").write_text(result.stdout)
            (case_dir / "stderr.log").write_text(result.stderr)
            native = runner.read_json(case_dir / "benchmark.json")
            if not isinstance(native, list) or not native:
                row = runner.base_record(entry, "occam-eval/profiles", timeout, budget, executions)
                row.update({"status": "tool-error", "status_reason": result.stderr.strip() or "native profiles missing",
                            "validation_calls": None, "wall_time_sec": None})
                stream.write(json.dumps(row, sort_keys=True) + "\n")
                stream.flush()
                records += 1
                statuses["tool-error"] = statuses.get("tool-error", 0) + 1
                continue
            original = Path(entry["file"]).read_bytes()
            for metric in native:
                evaluation = metric["metricEvaluationMetrics"]
                row = runner.base_record(entry, metric["metricAlgorithm"], timeout, budget, executions)
                runner.set_size_metrics(row, original, Path(metric["metricSQLArtifact"]).read_bytes(), "sqlite")
                row.update({"status": evaluation["evaluationStatus"], "status_reason": evaluation["evaluationFailure"],
                            "predicate_passed": evaluation["evaluationValidationPassed"] if evaluation["evaluationStatus"] == "completed" else None,
                            "native_validation_passed": evaluation["evaluationValidationPassed"],
                            "oracle_calls": evaluation["evaluationDatabaseObservations"], "oracle_timeouts": evaluation["evaluationTimedOutObservations"],
                            "oracle_cache_hits": evaluation["evaluationCacheHits"], "validation_calls": evaluation["evaluationValidationDatabaseObservations"],
                            "preflight_calls_debited": evaluation["evaluationBudget"]["evaluationPreflightAttempts"],
                            "preflight_scope": "one physical shared preflight per case; same debit per profile",
                            "validation_time_sec": evaluation["evaluationValidationSeconds"], "reset_time_sec": evaluation["evaluationResetSeconds"],
                            "reset_calls": None if entry.get("reset_command") else 0,
                            "reset_accounting": "native reduction resets measured; call count uninstrumented" if entry.get("reset_command") else "no reset command; database setup internal to predicate",
                            "native_process_returncode": result.returncode,
                            "reduction_time_sec": evaluation["evaluationReductionSeconds"], "wall_time_sec": evaluation["evaluationTotalSeconds"],
                            "result_file": metric["metricSQLArtifact"], "native_metrics_file": metric["metricMetricsArtifact"],
                            "settings": metric["metricReductionSettings"], "parameters": entry["parameters"],
                            "reduction_track": entry.get("reduction_track", "whole_script")})
                stream.write(json.dumps(row, sort_keys=True) + "\n")
                stream.flush()
                records += 1
                validated += row["status"] == "completed" and row["predicate_passed"] is True
                statuses[row["status"]] = statuses.get(row["status"], 0) + 1
    elapsed = time.monotonic() - started
    summary = {"schema_version": 1, "input_count": len(entries), "record_count": records, "wall_time_sec": elapsed,
               "inputs_per_second": len(entries) / elapsed, "records_per_second": records / elapsed,
               "validated_reduction_count": validated, "nonvalidated_record_count": records - validated,
               "status_counts": statuses}
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
    profile_parser.add_argument("--timeout", type=float, default=2)
    profile_parser.add_argument("--budget", type=float, default=30)
    profile_parser.add_argument("--max-oracle-executions", type=int, default=500)
    args = parser.parse_args()
    if args.command == "prepare":
        if args.corpus_size < 1:
            parser.error("--corpus-size must be positive")
        prepare(args.directory.resolve(), args.corpus_size)
    else:
        if not math.isfinite(args.timeout) or not math.isfinite(args.budget) or args.timeout <= 0 or args.budget <= 0 or args.max_oracle_executions < 1:
            parser.error("limits must be positive")
        profiles(args.manifest.resolve(), args.binary.resolve(), args.out.resolve(), args.timeout, args.budget, args.max_oracle_executions)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

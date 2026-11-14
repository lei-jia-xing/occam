#!/usr/bin/env python3
"""Probe repeated native profiles against the bounded SQLite reset case."""
from __future__ import annotations

import argparse
from contextlib import closing, redirect_stdout
import hashlib
import json
import os
from pathlib import Path
import shlex
import shutil
import sqlite3
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from benchmarks.tools import scale_study as study

SENTINEL = "probe-run-1-artifact.sentinel"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def append_jsonl(path: Path, value: dict) -> None:
    with path.open("a", encoding="utf-8") as stream:
        stream.write(json.dumps(value, sort_keys=True) + "\n")


def read_jsonl(path: Path) -> list[dict]:
    if not path.is_file():
        return []
    return [json.loads(line) for line in path.read_text(encoding="utf-8").splitlines()]


def database_observation(database: Path) -> dict:
    with closing(sqlite3.connect(database, isolation_level=None)) as connection:
        connection.execute("PRAGMA foreign_keys=ON")
        marker = connection.execute("SELECT case_id FROM study_marker").fetchone()
        return {
            "state_signature": study.state_signature(connection),
            "marker": marker[0] if marker else None,
            "user_version": connection.execute("PRAGMA user_version").fetchone()[0],
        }


def dirty_database(database: Path) -> dict:
    with closing(sqlite3.connect(database, isolation_level=None)) as connection:
        connection.execute("PRAGMA foreign_keys=ON")
        connection.executescript("""
DELETE FROM study_marker;
DELETE FROM expr_calc_inputs;
DROP INDEX IF EXISTS study_idx;
DROP TRIGGER IF EXISTS study_trigger;
DELETE FROM study_sequence;
UPDATE sqlite_sequence SET seq=999 WHERE name='study_sequence';
PRAGMA user_version=7;
""")
        return {
            "state_signature": study.state_signature(connection),
            "marker": None,
            "user_version": connection.execute("PRAGMA user_version").fetchone()[0],
        }


def _oracle(args: list[str]) -> int:
    if len(args) != 1:
        return 125
    metadata = Path(args[0])
    case = study.load_case(metadata)
    run_index = int(os.environ["OCCAM_PROFILE_PROBE_RUN"])
    root = Path(os.environ["OCCAM_PROFILE_PROBE_DIR"])
    artifact_dir = Path(os.environ["OCCAM_PROFILE_PROBE_ARTIFACT_DIR"])
    log = root / "observations.jsonl"
    counter = root / f"oracle-calls-{run_index}.txt"
    call_index = int(counter.read_text(encoding="utf-8")) + 1 if counter.exists() else 1
    counter.write_text(str(call_index), encoding="utf-8")
    workdir = Path(os.environ["OCCAM_WORKDIR"]).resolve()
    database = workdir / "study.sqlite"
    sql = sys.stdin.buffer.read()
    expected = case["state_signature"]
    sentinel_seen = (artifact_dir / SENTINEL).is_file()
    record = {
        "repetition": run_index,
        "call_index": call_index,
        "physical_preflight": call_index == 1,
        "sql_sha256": hashlib.sha256(sql).hexdigest(),
        "candidate_workdir": str(workdir),
        "database_exists": database.is_file(),
        "artifact_sentinel_seen": sentinel_seen,
        "expected_state_signature": expected,
        "marker_expected": case["marker"],
    }
    try:
        before = database_observation(database)
        record.update({
            "before_state_signature": before["state_signature"],
            "before_marker": before["marker"],
            "before_user_version": before["user_version"],
        })
        if sentinel_seen:
            returncode = 1
        else:
            result = subprocess.run(
                [sys.executable, str(study.SCRIPT), "oracle", str(metadata)],
                input=sql, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=ROOT,
            )
            sys.stdout.buffer.write(result.stdout)
            sys.stdout.buffer.flush()
            sys.stderr.buffer.write(result.stderr)
            sys.stderr.buffer.flush()
            returncode = result.returncode
        record["predicate_returncode"] = returncode
        record["accepted"] = returncode == 0
        if run_index == 1:
            dirty = dirty_database(database)
            record.update({
                "after_state_signature": dirty["state_signature"],
                "after_marker": dirty["marker"],
                "after_user_version": dirty["user_version"],
                "deliberately_contaminated": dirty["state_signature"] != expected,
            })
        else:
            record["deliberately_contaminated"] = False
    except Exception as exc:
        record["probe_error"] = f"{type(exc).__name__}: {exc}"
        record["predicate_returncode"] = 125
        record["accepted"] = False
        returncode = 125
        print(record["probe_error"], file=sys.stderr)
    finally:
        append_jsonl(log, record)
    return returncode


def _binary_shim(args: list[str]) -> int:
    out_value = None
    for index, arg in enumerate(args[:-1]):
        if arg == "--out":
            out_value = args[index + 1]
            break
    if out_value is None:
        print("profile isolation probe: missing --out", file=sys.stderr)
        return 125
    out_dir = Path(out_value).resolve()
    out_dir.mkdir(parents=True, exist_ok=True)
    root = Path(os.environ["OCCAM_PROFILE_PROBE_DIR"])
    counter = root / "child-count.txt"
    run_index = int(counter.read_text(encoding="utf-8")) + 1 if counter.exists() else 1
    counter.write_text(str(run_index), encoding="utf-8")
    child_env = os.environ.copy()
    child_env["OCCAM_PROFILE_PROBE_RUN"] = str(run_index)
    child_env["OCCAM_PROFILE_PROBE_ARTIFACT_DIR"] = str(out_dir)
    # Oracle.hs owns OCCAM_WORKDIR/TMPDIR for each predicate observation.
    result = subprocess.run(
        [os.environ["OCCAM_PROFILE_PROBE_REAL_BINARY"], *args], env=child_env,
    )
    sentinel_written = False
    if run_index == 1 and result.returncode == 0:
        (out_dir / SENTINEL).write_text("run-one artifact contamination sentinel\n", encoding="utf-8")
        sentinel_written = True
    append_jsonl(root / "children.jsonl", {
        "repetition": run_index,
        "out_dir": str(out_dir),
        "native_returncode": result.returncode,
        "sentinel_written": sentinel_written,
    })
    return result.returncode


def _copy_case(output: Path) -> tuple[Path, Path, dict, dict]:
    fixture = output / "fixture"
    fixture.mkdir()
    with tempfile.TemporaryDirectory(prefix="occam-isolation-fixture-") as staging_name:
        staging = Path(staging_name)
        with open(os.devnull, "w") as null, redirect_stdout(null):
            study.prepare(staging, 1)
        source_manifest = json.loads((staging / "resets.json").read_text(encoding="utf-8"))
        entry = next(case for case in source_manifest["cases"] if case["id"] == "reset-template")
        metadata = json.loads(Path(entry["case_metadata"]).read_text(encoding="utf-8"))
        sql_path = fixture / "reset-template.sql"
        metadata_path = fixture / "reset-template.case.json"
        template_path = fixture / "reset-template.template.sqlite"
        shutil.copyfile(entry["file"], sql_path)
        shutil.copyfile(metadata["template"], template_path)
        metadata["template"] = str(template_path.resolve())
        metadata["file"] = str(sql_path.resolve())
        metadata_path.write_text(json.dumps(metadata, sort_keys=True) + "\n", encoding="utf-8")
        shutil.copyfile(staging / "provenance.json", fixture / "provenance.json")
    entry.update({
        "file": str(sql_path.resolve()),
        "case_metadata": str(metadata_path.resolve()),
        "reset_command": shlex.join([sys.executable, str(study.SCRIPT), "reset", str(metadata_path.resolve())]),
        "oracle_command": shlex.join([sys.executable, str(Path(__file__).resolve()), "--oracle", str(metadata_path.resolve())]),
    })
    manifest = fixture / "reset-template.json"
    manifest.write_text(json.dumps({"cases": [entry]}, indent=2) + "\n", encoding="utf-8")
    return manifest, sql_path, metadata, entry


def run_probe(binary: Path, output: Path, profile: str = "full", timeout: float = 2,
              budget: float = 10, executions: int = 100, outer_timeout: float = 30) -> dict:
    binary = binary.resolve()
    output = output.resolve()
    if not binary.is_file() or not os.access(binary, os.X_OK):
        raise ValueError(f"--binary is not an executable file: {binary}")
    output.mkdir(parents=True, exist_ok=True)
    if any(output.iterdir()):
        raise ValueError("choose an empty probe output directory; existing evidence is preserved")
    manifest, sql_path, case, entry = _copy_case(output)
    runner_output = output / "runs"
    runner_command = [
        sys.executable, str(study.SCRIPT), "profiles", str(manifest),
        "--binary", str(output / "occam-eval-probe"), "--out", str(runner_output),
        "--profile", profile, "--repetitions", "2", "--timeout", str(timeout),
        "--budget", str(budget), "--max-oracle-executions", str(executions),
        "--outer-timeout", str(outer_timeout),
    ]
    shim = output / "occam-eval-probe"
    shim.write_text(
        "#!/usr/bin/env python3\n"
        "import os, sys\n"
        f"os.execv(sys.executable, [sys.executable, {str(Path(__file__).resolve())!r}, '--shim', *sys.argv[1:]])\n",
        encoding="utf-8",
    )
    shim.chmod(0o755)
    version = subprocess.run([str(binary), "--version"], cwd=ROOT, capture_output=True, text=True, timeout=5)
    probe = {
        "schema_version": 1,
        "scope": "one existing reset-template SQLite rowset case; two native profile children; per-observation SQLite candidate-workdir plus external reset only; not shared-server restoration or representative workload evidence",
        "predicate_contract": {"accept": "exit 0", "reject": "any nonzero or signal", "stdout_stderr": "opaque"},
        "case_id": entry["id"],
        "profile": profile,
        "repetitions": 2,
        "binary": str(binary),
        "binary_shim": str(shim),
        "binary_shim_sha256": sha256(shim),
        "binary_sha256": sha256(binary),
        "binary_version": version.stdout.strip(),
        "sql_sha256": sha256(sql_path),
        "case_metadata_sha256": sha256(manifest.parent / "reset-template.case.json"),
        "reset_template_sha256": sha256(manifest.parent / "reset-template.template.sqlite"),
        "probe_helper": str(Path(__file__).resolve()),
        "probe_helper_sha256": sha256(Path(__file__).resolve()),
        "scale_study_sha256": sha256(study.SCRIPT),
        "python_version": sys.version,
        "sqlite_version": sqlite3.sqlite_version,
        "command": runner_command,
        "runner_returncode": None,
    }
    (output / "probe.json").write_text(json.dumps(probe, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    (output / "probe-command.json").write_text(json.dumps({
        "command": runner_command,
        "real_binary": str(binary),
        "real_binary_sha256": probe["binary_sha256"],
        "binary_shim": str(shim),
        "binary_shim_sha256": probe["binary_shim_sha256"],
    }, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    env = os.environ.copy()
    env.update({
        "OCCAM_PROFILE_PROBE_DIR": str(output),
        "OCCAM_PROFILE_PROBE_REAL_BINARY": str(binary),
    })
    control_timeout = max(60, int(outer_timeout * 2 + 30))
    result = study.command_result(
        runner_command,
        cwd=ROOT,
        deadline=time.monotonic() + control_timeout,
        allow_nonzero=True,
        env=env,
    )
    (output / "runner.stdout.log").write_text(result["stdout"], encoding="utf-8")
    (output / "runner.stderr.log").write_text(result["stderr"], encoding="utf-8")
    probe["runner_process_result"] = result
    probe["runner_returncode"] = result["returncode"]
    if result["status"] == "timeout":
        probe["runner_failure"] = "profile control plane timed out"
    result_path = runner_output / "results.jsonl"
    rows = read_jsonl(result_path)
    observations = read_jsonl(output / "observations.jsonl")
    children = read_jsonl(output / "children.jsonl")
    row_by_rep = {int(row.get("repetition", 0)): row for row in rows}
    child_by_rep = {int(child["repetition"]): child for child in children}
    failures = []
    if not (runner_output / "run.json").is_file():
        failures.append("missing runner run.json")
    if len(rows) != 2 or set(row_by_rep) != {1, 2}:
        failures.append("runner did not retain two per-repetition result records")
    if len(children) != 2 or set(child_by_rep) != {1, 2}:
        failures.append("real reducer shim did not observe two serial children")
    if probe["runner_returncode"] != 0:
        failures.append("profile runner process did not exit successfully")
    records_by_rep = {rep: [record for record in observations if record.get("repetition") == rep] for rep in (1, 2)}
    for repetition in (1, 2):
        row = row_by_rep.get(repetition)
        child = child_by_rep.get(repetition)
        records = records_by_rep[repetition]
        if row is None or child is None:
            continue
        if row.get("case_id") != entry["id"] or row.get("repetition") != repetition:
            failures.append(f"repetition {repetition} result identity does not match its case")
        if row.get("execution_mode") != "native":
            failures.append(f"repetition {repetition} is not recorded as native execution")
        terminal = row.get("terminal_observation")
        if row.get("status") != "completed" or not isinstance(terminal, dict) or terminal.get("status") != "completed":
            failures.append(f"repetition {repetition} did not retain completed terminal observation")
        if child.get("native_returncode") != 0:
            failures.append(f"repetition {repetition} native CLI did not exit successfully")
        if not records or records[0].get("call_index") != 1 or not records[0].get("physical_preflight"):
            failures.append(f"repetition {repetition} has no independently observed physical preflight")
            continue
        native_path = Path(child["out_dir"]) / "benchmark.json"
        try:
            native = json.loads(native_path.read_text(encoding="utf-8"))
            metric = native[0]
            metrics = metric["metricEvaluationMetrics"]
            budget_metrics = metrics["evaluationBudget"]
        except (OSError, ValueError, KeyError, IndexError, TypeError):
            failures.append(f"repetition {repetition} lacks a readable native profile artifact")
            continue
        if metric.get("metricAlgorithm") != profile or metric.get("metricExperimentLine") != "internal":
            failures.append(f"repetition {repetition} native artifact is not the selected internal profile")
        if budget_metrics.get("evaluationPreflightAttempts") != 1:
            failures.append(f"repetition {repetition} did not debit its own physical preflight")
        if metrics.get("evaluationValidationPassed") is not True or metrics.get("evaluationValidationDatabaseObservations", 0) < 1:
            failures.append(f"repetition {repetition} did not complete fresh final validation")
        if metrics.get("evaluationCacheMisses", 0) < 1:
            failures.append(f"repetition {repetition} did not populate its own candidate cache counters")
        audited_count = len(records)
        expected_count = 1 + metrics.get("evaluationDatabaseObservations", 0) + metrics.get("evaluationValidationDatabaseObservations", 0)
        if audited_count != expected_count:
            failures.append(f"repetition {repetition} observation audit does not reconcile to native per-run counters")
        if records[0].get("predicate_returncode") != 0:
            failures.append(f"repetition {repetition} physical preflight did not accept")
        validation_count = metrics.get("evaluationValidationDatabaseObservations", 0)
        if type(validation_count) is not int or validation_count < 1 or len(records) < validation_count + 1:
            failures.append(f"repetition {repetition} lacks its independently audited final checks")
        elif any(record.get("predicate_returncode") != 0 for record in records[-validation_count:]):
            failures.append(f"repetition {repetition} final validation did not accept")
        for record in records:
            before_ok = (
                record.get("before_state_signature") == case["state_signature"]
                and record.get("before_marker") == case["marker"]
                and record.get("before_user_version") == 42
            )
            if not before_ok:
                failures.append(f"repetition {repetition} observation did not see restored SQLite state")
            if record.get("artifact_sentinel_seen"):
                failures.append(f"repetition {repetition} observed run-one artifact sentinel")
            workdir = Path(record["candidate_workdir"])
            record["cleanup_observed"] = "removed" if not workdir.exists() else "still_exists"
            if workdir.exists():
                failures.append(f"repetition {repetition} candidate workdir was not cleaned")
            if repetition == 1 and not record.get("deliberately_contaminated"):
                failures.append("run-one predicate did not leave deliberate dirty fixture state")
        expected_input_hash = sha256(sql_path)
        if records[0].get("sql_sha256") != expected_input_hash:
            failures.append(f"repetition {repetition} preflight did not check the exact initial SQL")
        if repetition == 2 and (records[0].get("predicate_returncode") != 0 or records[0].get("before_state_signature") != case["state_signature"]):
            failures.append("run-two physical preflight did not accept the restored fixture")
    if all(len(records_by_rep[repetition]) > 1 for repetition in (1, 2)):
        first_candidates = [records_by_rep[repetition][1]["sql_sha256"] for repetition in (1, 2)]
        probe["first_reducer_candidate_sha256"] = first_candidates
        if first_candidates[0] != first_candidates[1]:
            failures.append("run two did not independently observe run one's first cache-miss candidate")
    else:
        failures.append("missing reducer callback needed to prove per-child cache state")
    all_workdirs = [record.get("candidate_workdir") for record in observations]
    if len(all_workdirs) != len(set(all_workdirs)):
        failures.append("candidate workdir was reused across observations")
    if 1 in child_by_rep and 2 in child_by_rep:
        first_out = Path(child_by_rep[1]["out_dir"])
        second_out = Path(child_by_rep[2]["out_dir"])
        if not (first_out / SENTINEL).is_file():
            failures.append("run-one artifact contamination sentinel was not retained")
        if first_out == second_out:
            failures.append("repetitions reused one native artifact directory")
        if (second_out / SENTINEL).exists():
            failures.append("run-one artifact sentinel leaked into run two")
    for record in observations:
        row = row_by_rep.get(int(record["repetition"]))
        if row is not None:
            record["runner_run_id"] = row.get("run_id")
            record["runner_child_id"] = row.get("child_id")
            record["runner_order"] = row.get("order")
    for child in children:
        row = row_by_rep.get(int(child["repetition"]))
        if row is not None:
            child["runner_run_id"] = row.get("run_id")
            child["runner_child_id"] = row.get("child_id")
            child["runner_order"] = row.get("order")
    probe["rows"] = rows
    probe["observations"] = observations
    probe["children"] = children
    probe["failures"] = failures
    probe["qualified"] = not failures
    (output / "probe.json").write_text(json.dumps(probe, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    (output / "observations.jsonl").write_text("".join(json.dumps(record, sort_keys=True) + "\n" for record in observations), encoding="utf-8")
    (output / "cleanup.jsonl").write_text("".join(json.dumps({key: record.get(key) for key in ("repetition", "runner_run_id", "runner_child_id", "candidate_workdir", "cleanup_observed")}, sort_keys=True) + "\n" for record in observations), encoding="utf-8")
    if failures:
        raise RuntimeError("SQLite profile isolation probe failed: " + "; ".join(failures))
    return probe


def main() -> int:
    args = sys.argv[1:]
    if args[:1] == ["--shim"]:
        return _binary_shim(args[1:])
    if args[:1] == ["--oracle"]:
        return _oracle(args[1:])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, required=True, help="built executable occam-eval")
    parser.add_argument("--out", type=Path, required=True, help="new empty evidence directory")
    parser.add_argument("--profile", default="full")
    parser.add_argument("--timeout", type=float, default=2)
    parser.add_argument("--budget", type=float, default=10)
    parser.add_argument("--max-oracle-executions", type=int, default=100)
    parser.add_argument("--outer-timeout", type=float, default=30)
    options = parser.parse_args(args)
    try:
        report = run_probe(options.binary, options.out, options.profile, options.timeout,
                           options.budget, options.max_oracle_executions, options.outer_timeout)
    except Exception as exc:
        print(f"profile isolation probe: {exc}", file=sys.stderr)
        return 1
    print(json.dumps({"out": str(options.out.resolve()), "qualified": report["qualified"], "repetitions": 2}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

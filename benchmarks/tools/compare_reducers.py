#!/usr/bin/env python3
"""Bounded SQLite pilot against independent upstream reducers; not a bug corpus."""
from __future__ import annotations

import argparse
import hashlib
import importlib.metadata
import json
import os
from pathlib import Path
import platform
import shlex
import shutil
import sqlite3
import subprocess
import sys
import time
from statistics import median
from contextlib import closing

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from benchmarks.fuzz_corpus import run_docker_benchmark as runner
from benchmarks.tools import scale_study as study

TOOLS = ("occam", "creduce", "sqlancer", "sqlreduce")
DEFAULT_PRODUCTION_SOURCES = ("src/Occam/Reducer.hs",)


def normalize_tools(requested=None):
    if requested is None:
        requested = TOOLS
    if not isinstance(requested, (list, tuple)) or not requested:
        raise ValueError("selected_tools must be a non-empty list")
    if any(not isinstance(tool, str) or tool not in TOOLS for tool in requested):
        raise ValueError(f"selected_tools must be drawn from {TOOLS}")
    if len(set(requested)) != len(requested):
        raise ValueError("selected_tools must not contain duplicates")
    if "occam" not in requested:
        raise ValueError("selected_tools must include occam")
    return tuple(tool for tool in TOOLS if tool in requested)


def production_sources(extra_sources):
    sources = []
    for source in [*(ROOT / name for name in DEFAULT_PRODUCTION_SOURCES), *extra_sources]:
        source = source.resolve()
        try:
            source.relative_to(ROOT)
        except ValueError as exc:
            raise ValueError(f"production source must be inside the repository: {source}") from exc
        if not source.is_file():
            raise ValueError(f"production source does not exist: {source}")
        if source not in sources:
            sources.append(source)
    return sources


SEEDS = (
    "01_cte_recursive_analytics.sql",
    "02_transaction_savepoint_churn.sql",
    "03_deep_expressions_polymorphic.sql",
    "04_multitable_diamond_dag.sql",
    "05_upsert_conflict.sql",
    "06_views_and_aggregates.sql",
)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def save(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def execute(command, directory, deadline, label, environment):
    save(directory / f"{label}.command.json", command)
    result = runner.command_result(command, directory, deadline, env=environment)
    (directory / f"{label}.stdout.log").write_text(result["stdout"], encoding="utf-8")
    (directory / f"{label}.stderr.log").write_text(result["stderr"], encoding="utf-8")
    save(directory / f"{label}.observation.json", {k: v for k, v in result.items() if k not in {"stdout", "stderr"}})
    return result


def qualify_cases(out, cases, timeout):
    qualification = []
    helper = ROOT / "benchmarks/tools/scale_study.py"
    environment = {
        "python_executable": sys.executable,
        "python_version": platform.python_version(),
        "sqlite_version": sqlite3.sqlite_version,
    }
    helper_hash = digest(helper)
    for case in cases:
        path = Path(case["file"])
        metadata = Path(case["case_metadata"])
        input_hash = digest(path)
        if case.get("input_sha256", input_hash) != input_hash:
            raise ValueError(f"input checksum mismatch: {case['id']}")
        case["input_sha256"] = input_hash
        case["oracle_command"] = shlex.join(
            [sys.executable, str(helper), "oracle", str(metadata)]
        )
        if "case_contract" not in case:
            case["dialect"] = "sqlite"
            case["reduction_track"] = "whole_script"
            case["reset_command_sha256"] = None
            case["case_contract"] = {
                "effect_scope": "fresh in-memory SQLite connection per predicate invocation",
                "comparison_policy": "exact last-rowset ordered equality; any SQL error rejects",
                "declared_scope": "eight selected seed/scale inputs; not independent bugs or dialect coverage",
                "environment": environment,
                "hashes": {
                    "sql_sha256": input_hash,
                    "metadata_sha256": digest(metadata),
                    "helper_sha256": helper_hash,
                    "template_sha256": None,
                },
            }
            case["case_contract_sha256"] = hashlib.sha256(
                json.dumps(case["case_contract"], sort_keys=True).encode()
            ).hexdigest()
        else:
            contract = case["case_contract"]
            hashes = contract.get("hashes", {})
            if contract.get("environment") != environment:
                raise ValueError(f"input environment mismatch: {case['id']}")
            if (
                hashes.get("sql_sha256") != input_hash
                or hashes.get("metadata_sha256") != digest(metadata)
                or hashes.get("helper_sha256") != helper_hash
            ):
                raise ValueError(f"input oracle contract mismatch: {case['id']}")
            expected_contract_hash = hashlib.sha256(
                json.dumps(contract, sort_keys=True).encode()
            ).hexdigest()
            if case.get("case_contract_sha256") != expected_contract_hash:
                raise ValueError(f"case contract checksum mismatch: {case['id']}")
        command = [sys.executable, str(helper), "oracle", str(metadata)]
        for name, sql, expected in (
            ("positive-1", path.read_bytes(), 0),
            ("positive-2", path.read_bytes(), 0),
            ("empty", b"", 1),
            ("syntax-error", b"SELECT FROM;", 1),
            ("wrong-rows", b"SELECT NULL;", 1),
        ):
            result = subprocess.run(
                command, input=sql, capture_output=True, timeout=timeout, check=False
            )
            qualification.append(
                {
                    "case": case["id"],
                    "control": name,
                    "returncode": result.returncode,
                    "expected_returncode": expected,
                    "stderr": result.stderr.decode(errors="replace"),
                }
            )
            if result.returncode != expected:
                save(out / "qualification.json", qualification)
                raise RuntimeError(f"predicate control failed: {case['id']} / {name}")
    save(out / "qualification.json", qualification)
    save(out / "manifest.json", {"cases": cases})
    return cases


def prepare(out, timeout, reuse_cases_from=None):
    if reuse_cases_from is not None:
        bundle = Path(reuse_cases_from).resolve()
        manifest = json.loads((bundle / "manifest.json").read_text())
        cases = []
        for original in manifest["cases"]:
            case = dict(original)
            source = Path(original["file"])
            metadata_source = Path(original["case_metadata"])
            if not source.is_absolute():
                source = bundle / source
            if not metadata_source.is_absolute():
                metadata_source = bundle / metadata_source
            source = source.resolve()
            metadata_source = metadata_source.resolve()
            try:
                source.relative_to(bundle)
                metadata_source.relative_to(bundle)
            except ValueError as exc:
                raise ValueError("reused case files must be inside the source bundle") from exc
            path = out / f"{case['id']}.sql"
            metadata = path.with_suffix(".case.json")
            shutil.copyfile(source, path)
            shutil.copyfile(metadata_source, metadata)
            case["file"] = str(path)
            case["case_metadata"] = str(metadata)
            cases.append(case)
        return qualify_cases(out, cases, timeout)

    study.prepare(out / "study", 1)
    cases = []
    for name in SEEDS:
        source = ROOT / "benchmarks/seeds" / name
        sql = source.read_bytes()
        path = out / name
        path.write_bytes(sql)
        with closing(sqlite3.connect(":memory:", isolation_level=None)) as connection:
            connection.execute("PRAGMA foreign_keys=ON")
            expected = study.execute_sql(connection, sql.decode("utf-8"))
        metadata = path.with_suffix(".case.json")
        save(metadata, {"id": path.stem, "expected_rows": expected})
        cases.append({"id": path.stem, "file": str(path), "case_metadata": str(metadata)})
    scales = json.loads((out / "study/scale.json").read_text())["cases"]
    cases.extend(case for case in scales if case["id"] in {"statements-64", "statements-512"})
    return qualify_cases(out, cases, timeout)


def run_one(args, case, tool, repetition, classes, environment):
    directory = args.out / "runs" / case["id"] / f"{repetition}-{tool}"
    directory.mkdir(parents=True)
    source = Path(case["file"])
    checkpoint = directory / "checkpoint.sql"
    candidate = directory / "candidate.sql"
    checkpoint.write_bytes(source.read_bytes())
    candidate.write_bytes(source.read_bytes())
    audit = directory / "oracle_audit.jsonl"
    callbacks = directory / "callbacks.jsonl"
    metrics = directory / "metrics.json"
    started = time.monotonic()
    deadline = started + args.budget
    # Every tool has the same outer cap, including a common two-call final check.
    reduction_deadline = deadline - 2 * (args.timeout + 0.25)
    wrapper = runner.oracle_wrapper(ROOT, case["oracle_command"], audit, args.timeout)
    validation = runner.oracle_wrapper(ROOT, case["oracle_command"], audit, args.timeout, phase="validation")
    initial = runner.run_exit_oracle(validation, source, ROOT, reduction_deadline, args.timeout + 0.25, audit)
    if initial[0] != "accepted":
        raise RuntimeError(f"qualified input no longer accepted: {case['id']}: {initial}")
    helper = [sys.executable, str(ROOT / "benchmarks/upstream/oracle_test.py"), "--checkpoint", str(checkpoint), "--calls", str(callbacks), "--cache", str(directory / "cache.json"), "--timeout", str(args.timeout), "--deadline", str(reduction_deadline), "--oracle", wrapper, "--oracle-audit", str(audit)]
    remaining = reduction_deadline - time.monotonic()
    if tool == "occam":
        command = [str(args.occam), str(source), "--oracle", wrapper, "--dialect", "sqlite", "--intra-statement", "--timeout", str(args.timeout), "--budget", str(remaining), "--out", str(directory / "native"), "--log-format", "json"]
        output = directory / "native/minimal.sql"
    elif tool == "creduce":
        # C-Reduce tests a COPY in its own temporary cwd: never use an absolute candidate path.
        test = directory / "test.sh"
        test.write_text("#!/usr/bin/env bash\nexec " + shlex.join([*helper, "--candidate", "candidate.sql"]) + "\n", encoding="utf-8")
        test.chmod(0o755)
        command = [str(args.creduce), "--not-c", "--n", "1", "--timeout", str(int(args.timeout + 2)), str(test), "candidate.sql"]
        output = candidate
    elif tool == "sqlancer":
        classpath = f"{classes}:{args.sqlancer_source}/target/sqlancer-2.0.0.jar:{args.sqlancer_source}/target/lib/*"
        command = ["java", "-XX:ActiveProcessorCount=1", "-cp", classpath, "sqlancer.PredicateReducer", "--input", str(source), "--output", str(candidate), "--calls", str(directory / "reducer-calls.jsonl"), "--metrics", str(metrics), "--timeout-ms", str(int((args.timeout + 0.25) * 1000)), "--budget-ms", str(int(remaining * 1000)), "--", *helper]
        output = candidate
    else:
        command = [sys.executable, str(ROOT / "benchmarks/upstream/sqlreduce_adapter.py"), "--source", str(args.sqlreduce_source), "--input", str(source), "--output", str(candidate), "--metrics", str(metrics), "--timeout", str(args.timeout + 0.25), "--deadline", str(reduction_deadline), "--", *helper]
        output = candidate
    outcome = execute(command, directory, reduction_deadline, "reducer", environment)
    native = runner.read_json(directory / "native/stats.json") if tool == "occam" else None
    code = outcome["returncode"]
    status = {0: "completed", 77: "invalid-result", 78: "unsupported", 124: "timeout", 125: "tool-error", 126: "execution-budget"}.get(code, "tool-error")
    if outcome["status"] == "timeout":
        status = "timeout"
    if isinstance(native, dict):
        profile = native.get("evaluation_metrics", {})
        native_status = profile.get("evaluationStatus")
        if native_status != "completed" and code != 0:
            status = native_status or status
    if status != "completed" and tool != "occam":
        output = checkpoint
    if not output.is_file():
        output = checkpoint
        if status == "completed":
            status = "tool-error"
    result = directory / "result.sql"
    result.write_bytes(output.read_bytes())
    final = [runner.run_exit_oracle(validation, result, ROOT, deadline, args.timeout + 0.25, audit)[0] for _ in range(2)]
    verified = final == ["accepted", "accepted"]
    if status == "completed" and not verified:
        status = "validation-failed"
    observations = runner.read_jsonl(audit)
    reduction_observations = [row for row in observations if row.get("phase") == "reducer"]
    if any(row.get("status") in {"timeout", "interrupted", "tool-error"} for row in reduction_observations) and status == "completed":
        status = "inconclusive-oracle"
    row = {
        "case_id": case["id"], "tool": tool, "repetition": repetition,
        "experiment_line": "external", "run_type": "score", "execution_mode": "native-process",
        "reduction_track": "whole_script", "input_sha256": case["input_sha256"],
        "case_contract_sha256": case["case_contract_sha256"], "reset_command_sha256": None,
        "budget_sec": args.budget, "oracle_timeout_sec": args.timeout, "max_oracle_executions": None,
        "status": status, "returncode": code, "validated_twice": verified,
        "result_kind": "completed" if status == "completed" else "checkpoint",
        "initial_bytes": source.stat().st_size, "result_bytes": result.stat().st_size,
        "output_sha256": digest(result), "outer_wall_time_sec": time.monotonic() - started,
        "reducer_wall_time_sec": outcome["elapsed"], "cleanup_status": outcome["cleanup_status"],
        "audited_oracle_observations": len(observations),
        "oracle_count_scope": "all wrapper observations including input and common/native final checks; interrupted callbacks may be missing on noncompleted runs",
        "oracle_count_complete": status == "completed", "oracle_status_counts": {status: sum(obs.get("status") == status for obs in observations) for status in {obs.get("status") for obs in observations}},
        "callback_cache_hits": sum(bool(item.get("cache_hit")) for item in runner.read_jsonl(callbacks)) if tool != "occam" else native.get("evaluation_metrics", {}).get("evaluationCacheHits") if isinstance(native, dict) else None,
        "final_validation": final, "artifacts": str(directory),
    }
    save(directory / "result.json", row)
    return row


def report(directory):
    run = json.loads((directory / "run.json").read_text())
    legacy = "selected_tools" not in run
    if not legacy and not isinstance(run["selected_tools"], list):
        raise ValueError("selected_tools must be a list")
    if legacy and run.get("run_schema_version", 1) >= 2:
        raise ValueError("run schema requires a selected_tools declaration")
    tools = TOOLS if legacy else normalize_tools(run["selected_tools"])
    if "tools" in run and (set(run["tools"]) & set(TOOLS)) != set(tools):
        raise ValueError("selected-tools declaration does not match tool identities")
    cases = {case["id"]: case for case in json.loads((directory / "manifest.json").read_text())["cases"]}
    rows = runner.read_jsonl(directory / "results.jsonl")
    expected = {
        (case, tool, repetition)
        for case in run["case_order"]
        for tool in tools
        for repetition in range(1, run["repetitions"] + 1)
    }
    indexed = {}
    for row in rows:
        key = (row["case_id"], row["tool"], row["repetition"])
        if key not in expected or key in indexed:
            raise ValueError(f"unexpected or duplicate comparison slot: {key}")
        case = cases[row["case_id"]]
        if any(row[field] != case[field] for field in ("input_sha256", "case_contract_sha256", "reset_command_sha256", "reduction_track")):
            raise ValueError(f"mismatched input/predicate contract: {key}")
        if any(row[field] != run[field] for field in ("budget_sec", "oracle_timeout_sec", "max_oracle_executions")):
            raise ValueError(f"mismatched budget protocol: {key}")
        if row["experiment_line"] != "external" or row["run_type"] != "score":
            raise ValueError(f"mismatched experiment line/type: {key}")
        indexed[key] = row
    if set(indexed) != expected:
        raise ValueError(f"incomplete cohort: {len(indexed)}/{len(expected)} slots")
    summary = {
        "records": len(rows),
        "selected_tools": list(tools),
        "tools": {},
        "cases": {},
        "completed_pairs": {},
    }
    text = [
        "# External reducer pilot", "",
        f"Selected tools: {', '.join(tools)}. Completion is tool-local; it does not establish equal minimality or complete AST coverage. "
        "Selected SQLite seed/scale inputs, not independent bugs. Occam uses --intra-statement; external tools use their declared adapters. "
        "Noncompleted outputs are checkpoints, never completed reductions. "
        "Byte counts are raw output bytes, including whitespace and comments.", "",
        "| Tool | Completed | Twice-validated checkpoints | Unvalidated outputs |",
        "| --- | ---: | ---: | ---: |",
    ]
    for tool in tools:
        selected = [row for row in rows if row["tool"] == tool]
        counts = {
            "total": len(selected),
            "completed": sum(row["status"] == "completed" for row in selected),
            "validated_checkpoints": sum(row["status"] != "completed" and row["validated_twice"] for row in selected),
            "unvalidated": sum(not row["validated_twice"] for row in selected),
            "statuses": {status: sum(row["status"] == status for row in selected) for status in sorted({row["status"] for row in selected})},
        }
        summary["tools"][tool] = counts
        text.append(f"| {tool} | {counts['completed']}/{counts['total']} | {counts['validated_checkpoints']} | {counts['unvalidated']} |")
    text += [
        "", "## Budget-limited output size", "",
        "Bytes: median across repetitions, including noncompleted checkpoints. "
        "Unvalidated outputs are excluded from size claims (shown as unavailable). "
        "Completion denominators above remain separate.", "",
        "| Case | Input B | " + " | ".join(f"{tool} B" for tool in tools) + " |",
        "| --- | ---: | " + " | ".join("---:" for _ in tools) + " |",
    ]
    for case in run["case_order"]:
        selected = [row for row in rows if row["case_id"] == case]
        values = {"initial_bytes": selected[0]["initial_bytes"]}
        for tool in tools:
            outputs = [row for row in selected if row["tool"] == tool]
            values[tool] = median(row["result_bytes"] for row in outputs) if all(row["validated_twice"] for row in outputs) else None
        summary["cases"][case] = values
        cells = [str(values[tool]) if values[tool] is not None else "unavailable" for tool in tools]
        text.append(f"| {case} | {values['initial_bytes']} | " + " | ".join(cells) + " |")
    text += [
        "", "## Jointly completed pairs only", "",
        "Ratios are other/Occam: values above 1 favor Occam on that cost metric. "
        "Outputs need not have the same size; report quality alongside cost. "
        "Timeout/error pairs are excluded, not treated as speedups. "
        "Observation counts include input checks and common/native final checks.", "",
        "| Other tool | Completed pairs / possible | Median wall ratio | Median observation ratio | Occam bytes smaller / tied / larger |",
        "| --- | ---: | ---: | ---: | --- |",
    ]
    for tool in (name for name in tools if name != "occam"):
        pairs = [
            (row, indexed[(row["case_id"], tool, row["repetition"])])
            for row in rows
            if row["tool"] == "occam"
            and row["status"] == "completed"
            and row["validated_twice"]
            and row["oracle_count_complete"]
            and indexed[(row["case_id"], tool, row["repetition"])]["status"] == "completed"
            and indexed[(row["case_id"], tool, row["repetition"])]["validated_twice"]
            and indexed[(row["case_id"], tool, row["repetition"])]["oracle_count_complete"]
        ]
        pair_summary = {
            "completed": len(pairs),
            "possible": len(run["case_order"]) * run["repetitions"],
            "median_wall_ratio": median(other["outer_wall_time_sec"] / own["outer_wall_time_sec"] for own, other in pairs) if pairs else None,
            "median_observation_ratio": median(other["audited_oracle_observations"] / own["audited_oracle_observations"] for own, other in pairs) if pairs else None,
            "occam_bytes_smaller": sum(own["result_bytes"] < other["result_bytes"] for own, other in pairs),
            "occam_bytes_tied": sum(own["result_bytes"] == other["result_bytes"] for own, other in pairs),
            "occam_bytes_larger": sum(own["result_bytes"] > other["result_bytes"] for own, other in pairs),
        }
        summary["completed_pairs"][tool] = pair_summary
        ratios = [f"{pair_summary[field]:.3f}" if pair_summary[field] is not None else "unavailable" for field in ("median_wall_ratio", "median_observation_ratio")]
        text.append(f"| {tool} | {len(pairs)}/{pair_summary['possible']} | {ratios[0]} | {ratios[1]} | {pair_summary['occam_bytes_smaller']} / {pair_summary['occam_bytes_tied']} / {pair_summary['occam_bytes_larger']} |")
    rendered = "\n".join(text) + "\n"
    if not legacy:
        save(directory / "summary.json", summary)
        (directory / "report.md").write_text(rendered, encoding="utf-8")
    print(rendered)
    return summary


def main():
    if sys.argv[1:2] == ["report"]:
        parser = argparse.ArgumentParser(description="Report a complete, protocol-matched external pilot")
        parser.add_argument("directory", type=Path)
        report(parser.parse_args(sys.argv[2:]).directory.resolve())
        return
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--occam", type=Path)
    parser.add_argument("--creduce", type=Path)
    parser.add_argument("--sqlancer-source", type=Path)
    parser.add_argument("--sqlreduce-source", type=Path)
    parser.add_argument("--tools", nargs="+", choices=TOOLS, help="selected reducers; defaults to all tools")
    parser.add_argument("--reuse-cases-from", type=Path, help="copy exact SQL and predicate metadata from a prior bundle")
    parser.add_argument("--production-source", type=Path, action="append", default=[], help="additional production source to snapshot and hash")
    parser.add_argument("--budget", type=float, default=20)
    parser.add_argument("--timeout", type=float, default=1)
    parser.add_argument("--repetitions", type=int, default=2)
    parser.add_argument("--case", action="append", dest="cases")
    args = parser.parse_args()
    try:
        selected_tools = normalize_tools(args.tools)
        prod_sources = production_sources(args.production_source)
    except ValueError as exc:
        parser.error(str(exc))
    if args.timeout <= 0 or args.budget <= 3 * (args.timeout + 0.25) or args.repetitions < 1:
        parser.error("positive timeout/repetitions and a budget exceeding three checks are required")
    tool_paths = {
        "occam": args.occam,
        "creduce": args.creduce,
        "sqlancer": args.sqlancer_source,
        "sqlreduce": args.sqlreduce_source,
    }
    missing = [tool for tool in selected_tools if tool_paths[tool] is None]
    if missing:
        parser.error("missing required path argument(s) for selected tools: " + ", ".join(missing))
    args.out = args.out.resolve()
    for tool in selected_tools:
        argument = {
            "occam": "occam",
            "creduce": "creduce",
            "sqlancer": "sqlancer_source",
            "sqlreduce": "sqlreduce_source",
        }[tool]
        setattr(args, argument, getattr(args, argument).resolve())
    if args.reuse_cases_from is not None:
        args.reuse_cases_from = args.reuse_cases_from.resolve()
        if not (args.reuse_cases_from / "manifest.json").is_file():
            parser.error("--reuse-cases-from must name an artifact bundle with manifest.json")
    if args.out.exists() and any(args.out.iterdir()):
        parser.error("choose a new empty output directory; evidence is never overwritten")
    args.out.mkdir(parents=True, exist_ok=True)
    identities = {}
    for name, pin in (("sqlancer", runner.SQLANCER_PIN), ("sqlreduce", runner.SQLREDUCE_PIN)):
        if name not in selected_tools:
            continue
        source = tool_paths[name].resolve()
        revision = subprocess.run(
            ["git", "-C", str(source), "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            check=True,
        ).stdout.strip()
        if revision != pin:
            parser.error(f"{name} must be pinned at {pin}, found {revision}")
        identities[name] = {"pin": revision, "source": str(source)}
    identities["occam"] = {
        "binary": str(args.occam),
        "sha256": digest(args.occam),
        "mode": "--intra-statement (default graph/cache mechanisms)",
    }
    if "creduce" in selected_tools:
        identities["creduce"] = {
            "binary": str(args.creduce),
            "sha256": digest(args.creduce),
            "version": subprocess.run(
                [str(args.creduce), "--version"],
                capture_output=True,
                text=True,
                check=True,
            ).stdout.strip(),
        }
    if "sqlreduce" in selected_tools:
        identities["python_packages"] = {
            name: importlib.metadata.version(name)
            for name in ("pglast", "psycopg2-binary", "PyYAML")
        }
    environment = os.environ | {"GHCRTS": "-N1"}
    original_affinity = sorted(os.sched_getaffinity(0)) if hasattr(os, "sched_getaffinity") else None
    if original_affinity:
        os.sched_setaffinity(0, {original_affinity[0]})
    cases = prepare(args.out, args.timeout, args.reuse_cases_from)
    if args.cases:
        unknown = set(args.cases) - {case["id"] for case in cases}
        if unknown:
            parser.error(f"unknown cases: {sorted(unknown)}")
        cases = [case for case in cases if case["id"] in args.cases]
    classes = None
    if "sqlancer" in selected_tools:
        classes = args.out / "sqlancer-classes"
        classes.mkdir()
        classpath = f"{args.sqlancer_source}/target/sqlancer-2.0.0.jar:{args.sqlancer_source}/target/lib/*"
        subprocess.run(
            ["javac", "-cp", classpath, "-d", str(classes), str(ROOT / "benchmarks/upstream/sqlancer/PredicateReducer.java")],
            check=True,
            timeout=60,
        )
    files = [
        Path(__file__).resolve(),
        ROOT / "benchmarks/tools/scale_study.py",
        ROOT / "benchmarks/fuzz_corpus/run_docker_benchmark.py",
        ROOT / "benchmarks/upstream/oracle_exec.py",
        ROOT / "benchmarks/upstream/oracle_test.py",
    ]
    if "sqlancer" in selected_tools:
        files.append(ROOT / "benchmarks/upstream/sqlancer/PredicateReducer.java")
    if "sqlreduce" in selected_tools:
        files.append(ROOT / "benchmarks/upstream/sqlreduce_adapter.py")
    snapshots = args.out / "sources"
    snapshots.mkdir()
    for source in files:
        shutil.copyfile(source, snapshots / source.name)
    for source in prod_sources:
        target = snapshots / "production" / source.relative_to(ROOT)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source, target)
    source_hashes = {str(source.relative_to(ROOT)): digest(source) for source in files}
    production_hashes = {str(source.relative_to(ROOT)): digest(source) for source in prod_sources}
    resource_controls = ["one inherited CPU", "GHCRTS=-N1"]
    if "creduce" in selected_tools:
        resource_controls.append("C-Reduce --n 1")
    if "sqlancer" in selected_tools:
        resource_controls.append("JVM ActiveProcessorCount=1")
    resource_controls.extend(["no page-cache flush", "no separate warmup"])
    save(
        args.out / "run.json",
        {
            "run_schema_version": 2,
            "command": sys.argv,
            "selected_tools": list(selected_tools),
            "tools": identities,
            "python": sys.version,
            "sqlite": sqlite3.sqlite_version,
            "platform": platform.platform(),
            "budget_sec": args.budget,
            "oracle_timeout_sec": args.timeout,
            "max_oracle_executions": None,
            "repetitions": args.repetitions,
            "case_order": [case["id"] for case in cases],
            "input_source_bundle": str(args.reuse_cases_from) if args.reuse_cases_from else None,
            "tool_order": "rotate selected tool order by case index + repetition index; serial children",
            "cpu_affinity": sorted(os.sched_getaffinity(0)) if original_affinity else None,
            "original_cpu_affinity": original_affinity,
            "resource_protocol": "; ".join(resource_controls),
            "cache_protocol": "native/upstream caches preserved; external callbacks additionally share a per-child exact-byte verdict cache",
            "timing_scope": "input check, tool process and two common final checks; installations and adapter compilation excluded",
            "sources": source_hashes,
            "production_sources": production_hashes,
            "scope": "selected SQLite seed/scale pilot; no DBMS crash evidence; external tools use upstream search with external predicate adapters",
        },
    )
    with (args.out / "results.jsonl").open("w", encoding="utf-8") as output:
        for repetition in range(1, args.repetitions + 1):
            for index, case in enumerate(cases):
                rotation = (index + repetition - 1) % len(selected_tools)
                order = selected_tools[rotation:] + selected_tools[:rotation]
                for tool in order:
                    row = run_one(args, case, tool, repetition, classes, environment)
                    output.write(json.dumps(row, sort_keys=True) + "\n")
                    output.flush()
                    print(f"{case['id']} rep={repetition} {tool}: {row['status']} {row['initial_bytes']}->{row['result_bytes']} B, {row['audited_oracle_observations']} observations, {row['outer_wall_time_sec']:.3f}s, validated={row['validated_twice']}", flush=True)
    report(args.out)


if __name__ == "__main__":
    main()

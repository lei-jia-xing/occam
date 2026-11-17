#!/usr/bin/env python3
"""Three-arm column ablation: occam-eval column-off/single/atomic, internal line.

Strict cohort (default 8 cases) x N reps x 3 arms on byte-pinned SQLite inputs
with fresh predicate contracts under the current helper, or fresh mechanism
cohorts minted from plain SQL+metadata dirs. Same resource/budget/count/final
protocol as compare_reducers.py; records the native cost section and optional
diagnostic-sidecar completeness per child. Completed and censored stay
separate; no pooled speedups, no winner claims.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import shutil
import sqlite3
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from benchmarks.fuzz_corpus import run_docker_benchmark as runner
from benchmarks.tools import compare_reducers as compare

ARMS = ("column-off", "column-single", "column-atomic")


def prepare_fresh(out, timeout, bundle, expect_cases):
    """Mint fresh contracts for a plain SQL+metadata dir (mechanism cohorts)."""
    sql_files = sorted(bundle.glob("*.sql"))
    if len(sql_files) != expect_cases:
        raise ValueError(f"cohort requires exactly {expect_cases} cases, got {len(sql_files)}")
    pinned = {}
    cases = []
    for source in sql_files:
        case_id = source.stem
        metadata_source = source.with_suffix(".case.json")
        if not metadata_source.is_file():
            raise ValueError(f"case {case_id}: missing sibling .case.json contract input")
        sql = source.read_bytes()
        metadata_bytes = metadata_source.read_bytes()
        pinned[case_id] = hashlib.sha256(sql).hexdigest()
        path = out / f"{case_id}.sql"
        metadata = path.with_suffix(".case.json")
        path.write_bytes(sql)
        metadata.write_bytes(metadata_bytes)
        cases.append({"id": case_id, "file": str(path), "case_metadata": str(metadata)})
    qualified = compare.qualify_cases(out, cases, timeout)
    for case in qualified:
        contract = case["case_contract"]
        contract["declared_scope"] = "mechanism fixtures; not independent bugs or primary evidence"
        case["case_contract_sha256"] = hashlib.sha256(
            json.dumps(contract, sort_keys=True).encode("utf-8")
        ).hexdigest()
    compare.save(out / "manifest.json", {"cases": qualified})
    return qualified, pinned


def prepare(out, timeout, cases_from, expect_cases):
    """Pin SQL + metadata bytes; validate old contract/env; mint helper-only fresh contracts."""
    bundle = Path(cases_from).resolve()
    if (bundle / "manifest.json").is_file():
        return prepare_pinned(out, timeout, bundle, expect_cases)
    return prepare_fresh(out, timeout, bundle, expect_cases)


def prepare_pinned(out, timeout, bundle, expect_cases):
    manifest = json.loads((bundle / "manifest.json").read_text())
    if len(manifest["cases"]) != expect_cases:
        raise ValueError(f"cohort requires exactly {expect_cases} cases, got {len(manifest['cases'])}")
    helper_hash = compare.digest(ROOT / "benchmarks/tools/scale_study.py")
    current_env = {
        "python_executable": sys.executable,
        "python_version": platform.python_version(),
        "sqlite_version": sqlite3.sqlite_version,
    }
    pinned = {}
    cases = []
    for original in manifest["cases"]:
        case_id = original.get("id")
        for key in ("file", "case_metadata", "input_sha256", "case_contract", "case_contract_sha256"):
            if key not in original:
                raise ValueError(f"case {case_id}: source manifest lacks required {key}")
        old_contract = original["case_contract"]
        old_hashes = old_contract.get("hashes", {})
        if (
            hashlib.sha256(json.dumps(old_contract, sort_keys=True).encode("utf-8")).hexdigest()
            != original["case_contract_sha256"]
        ):
            raise ValueError(f"case {case_id}: source contract checksum mismatch; refusing unpinned reuse")
        if old_contract.get("environment") != current_env:
            raise ValueError(
                f"case {case_id}: interpreter environment differs from source contract; "
                "rerun under the pinned interpreter"
            )
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
            raise ValueError("case files must be inside the source bundle") from exc
        sql = source.read_bytes()
        metadata_bytes = metadata_source.read_bytes()
        if hashlib.sha256(sql).hexdigest() != original["input_sha256"]:
            raise ValueError(f"case {case_id}: pinned input bytes changed")
        if hashlib.sha256(sql).hexdigest() != old_hashes.get("sql_sha256"):
            raise ValueError(f"case {case_id}: input bytes disagree with source contract")
        if hashlib.sha256(metadata_bytes).hexdigest() != old_hashes.get("metadata_sha256"):
            raise ValueError(f"case {case_id}: pinned metadata bytes changed")
        pinned[case_id] = original["input_sha256"]
        path = out / f"{case_id}.sql"
        metadata = path.with_suffix(".case.json")
        path.write_bytes(sql)
        metadata.write_bytes(metadata_bytes)
        fresh_contract = json.loads(json.dumps(old_contract, sort_keys=True))
        fresh_contract["hashes"]["helper_sha256"] = helper_hash
        case = dict(original)
        case["file"] = str(path)
        case["case_metadata"] = str(metadata)
        case["case_contract"] = fresh_contract
        case["case_contract_sha256"] = hashlib.sha256(
            json.dumps(fresh_contract, sort_keys=True).encode("utf-8")
        ).hexdigest()
        cases.append(case)
    return compare.qualify_cases(out, cases, timeout), pinned


def run_one(args, case, arm, repetition, environment):
    directory = args.out / "runs" / case["id"] / f"{repetition}-{arm}"
    directory.mkdir(parents=True)
    source = Path(case["file"])
    checkpoint = directory / "checkpoint.sql"
    checkpoint.write_bytes(source.read_bytes())
    audit = directory / "oracle_audit.jsonl"
    started = time.monotonic()
    deadline = started + args.budget
    # Every arm has the same outer cap, including a common two-call final check.
    reduction_deadline = deadline - 2 * (args.timeout + 0.25)
    wrapper = runner.oracle_wrapper(ROOT, case["oracle_command"], audit, args.timeout)
    validation = runner.oracle_wrapper(ROOT, case["oracle_command"], audit, args.timeout, phase="validation")
    initial = runner.run_exit_oracle(validation, source, ROOT, reduction_deadline, args.timeout + 0.25, audit)
    if initial[0] != "accepted":
        raise RuntimeError(f"qualified input no longer accepted: {case['id']}: {initial}")
    remaining = reduction_deadline - time.monotonic()
    command = [
        str(args.occam_eval), str(source), "--oracle", wrapper, "--dialect", "sqlite",
        "--profile", arm, "--timeout", str(args.timeout), "--budget", str(remaining),
        "--out", str(directory / "native"), "--log-format", "json",
    ]
    output = directory / "native/minimal.sql"
    child_env = dict(environment)
    diag_path = None
    if args.diag:
        diag_path = directory / "diag.jsonl"
        child_env["OCCAM_ORACLE_DIAG"] = str(diag_path)
    outcome = compare.execute(command, directory, reduction_deadline, "reducer", child_env)
    profile = runner.read_json(directory / "native/profile.json")
    metrics = profile.get("metricEvaluationMetrics") if isinstance(profile, dict) else None
    if not isinstance(metrics, dict):
        metrics = None
    settings = profile.get("metricReductionSettings") if isinstance(profile, dict) else None
    if not isinstance(settings, dict):
        settings = None
    budget = metrics.get("evaluationBudget") if isinstance(metrics, dict) else None
    if not isinstance(budget, dict):
        budget = None
    policy_section = (metrics or {}).get("evaluationCostAttribution")
    policy = policy_section.get("snapshotPolicy") if isinstance(policy_section, dict) else None
    native_identity = None
    if isinstance(profile, dict):
        native_identity = {
            "metricAlgorithm": profile.get("metricAlgorithm"),
            "intra_statement": (settings or {}).get("intra_statement"),
            "cache": (settings or {}).get("cache"),
            "column_mode": (settings or {}).get("column_mode"),
            "policy_mode": (policy or {}).get("metricColumnMode"),
            "policy_jobs": (policy or {}).get("metricJobs"),
            "policy_search": (policy or {}).get("metricSearchPolicy"),
            "budget_jobs": (budget or {}).get("evaluationJobs"),
        }
    code = outcome["returncode"]
    outer_timeout = outcome["status"] == "timeout"
    native_status = (metrics or {}).get("evaluationStatus")
    native_ok = (
        isinstance(metrics, dict)
        and native_status == "completed"
        and (metrics.get("evaluationValidationPassed") is True)
        and metrics.get("evaluationValidationRepeats") == 2
    )
    native_censored = {"timeout", "execution-budget", "error", "validation-failed", "validation-incomplete"}
    output_exists = output.is_file()
    if not output_exists:
        output = checkpoint
    result = directory / "result.sql"
    result.write_bytes(output.read_bytes())
    final = [runner.run_exit_oracle(validation, result, ROOT, deadline, args.timeout + 0.25, audit)[0] for _ in range(2)]
    verified = final == ["accepted", "accepted"]
    if code == 0 and not outer_timeout and native_ok and output_exists and verified:
        status = "completed"
    elif outer_timeout:
        status = "timeout"
    elif native_status in native_censored:
        status = native_status
    elif not verified:
        status = "validation-failed"
    elif not output_exists:
        status = "tool-error"
    elif code != 0:
        status = {77: "invalid-result", 78: "unsupported", 124: "timeout", 125: "tool-error", 126: "execution-budget"}.get(code, "tool-error")
    else:
        status = "tool-error"
    observations = runner.read_jsonl(audit)
    reduction_observations = [row for row in observations if row.get("phase") == "reducer"]
    # Wrapper audit rows stay cost data only: under a verified native
    # completion, timeout/interrupted/tool-error rows are unselected-loser
    # cancel cost (pool contract); selected unknowns abort natively instead.
    # No audit row ever downgrades a strict completion above.
    cost = (metrics or {}).get("evaluationCostAttribution")
    diag_rows = diag_expected = diag_complete = None
    if args.diag and diag_path is not None:
        diag_rows = runner.read_jsonl(diag_path)
        diag_digests = {row.get("stdin_sha256") for row in diag_rows if isinstance(row, dict)}
        expected_digests = {row.get("digest") for row in reduction_observations if isinstance(row, dict)}
        diag_expected = len(expected_digests)
        diag_complete = bool(expected_digests) and expected_digests <= diag_digests
    row = {
        "case_id": case["id"], "tool": arm, "arm": arm, "repetition": repetition,
        "experiment_line": "internal", "run_type": "score", "execution_mode": "native-process",
        "reduction_track": "whole_script", "input_sha256": case["input_sha256"],
        "case_contract_sha256": case["case_contract_sha256"], "reset_command_sha256": None,
        "budget_sec": args.budget, "oracle_timeout_sec": args.timeout, "max_oracle_executions": None,
        "status": status, "returncode": code, "validated_twice": verified,
        "result_kind": "completed" if status == "completed" else "checkpoint",
        "initial_bytes": source.stat().st_size, "result_bytes": result.stat().st_size,
        "output_sha256": compare.digest(result), "outer_wall_time_sec": time.monotonic() - started,
        "reducer_wall_time_sec": outcome["elapsed"], "cleanup_status": outcome["cleanup_status"],
        "audited_oracle_observations": len(observations),
        "oracle_count_scope": "all wrapper observations including input and common/native final checks; interrupted callbacks may be missing on noncompleted runs",
        "oracle_count_complete": status == "completed", "oracle_status_counts": {status: sum(obs.get("status") == status for obs in observations) for status in {obs.get("status") for obs in observations}},
        "callback_cache_hits": (metrics or {}).get("evaluationCacheHits"),
        "native_identity": native_identity,
        "cost_present": cost is not None, "cost_attribution": cost,
        "diag_rows": diag_rows, "diag_expected_digests": diag_expected, "diag_complete": diag_complete,
        "final_validation": final, "artifacts": str(directory),
    }
    compare.save(directory / "result.json", row)
    return row


EXPECTED_SEARCH_POLICIES = {
    "column-off": "phase-forward-sweep-list-chunks",
    "column-single": "phase-forward-sweep-list-chunks+column-single",
    "column-atomic": "phase-forward-sweep-list-chunks+column-atomic",
}
EXPECTED_COLUMN_MODES = {"column-off": "off", "column-single": "single", "column-atomic": "atomic"}


def validate_cohort(directory, run, rows):
    """Strict cohort/identity validation; raises on any mismatch.

    Reuses partitioned_report's scalar validators and strict per-field message
    convention. Full qualify_run/validate_row do not apply: this runner keeps
    compare_reducers-style rows (case_contract/input pinning, no child_id/scope
    keys) and a 3-arm native-only ablation (not a pairwise contrast), so the
    cohort/identity rules below mirror that validator's strictness instead.
    Marks each row profile_complete (native profile + cost present and
    identity-matched); missing evidence is INCOMPLETE_PROFILE at tally time.
    """
    from benchmarks.tools import partitioned_report as partitioned

    arms = run.get("selected_arms")
    if not isinstance(arms, list) or not arms:
        raise ValueError("run lacks selected_arms")
    if not isinstance(run.get("case_order"), list) or not run["case_order"]:
        raise ValueError("run lacks case_order")
    run_reps = partitioned._exact_int(run.get("repetitions"), min_val=1)
    if run_reps is None:
        raise ValueError("run repetitions is not an exact integer >= 1")
    manifest = json.loads((directory / "manifest.json").read_text())
    declared = {case["id"]: case for case in manifest["cases"]}
    if set(run["case_order"]) != set(declared):
        raise ValueError("run case_order disagrees with manifest cases")
    expected_slots = {
        (case, repetition, arm)
        for case in run["case_order"]
        for repetition in range(1, run_reps + 1)
        for arm in arms
    }
    seen = {}
    for row in rows:
        repetition = partitioned._exact_int(row.get("repetition"), min_val=1)
        if repetition is None or repetition > run_reps:
            raise ValueError(f"invalid repetition: {row.get('repetition')!r}")
        key = (row.get("case_id"), repetition, row.get("arm"))
        if key in seen:
            raise ValueError(f"duplicate cohort slot: {key}")
        seen[key] = row
    if set(seen) != expected_slots:
        missing = sorted(expected_slots - set(seen), key=str)
        extra = sorted(set(seen) - expected_slots, key=str)
        raise ValueError(f"cohort slots incomplete: missing={missing} extra={extra}")
    for key in sorted(seen, key=str):
        row = seen[key]
        case_id = key[0]
        arm = key[2]
        decl = declared[case_id]
        for field in ("input_sha256", "case_contract_sha256"):
            if not partitioned._is_sha256(row.get(field)):
                raise ValueError(f"slot {key}: {field} is not a sha256: {row.get(field)!r}")
            if row.get(field) != decl.get(field):
                raise ValueError(f"slot {key}: {field} {row.get(field)!r} != manifest {decl.get(field)!r}")
        if row.get("input_sha256") != (run.get("pinned_input_sha256") or {}).get(case_id):
            raise ValueError(f"slot {key}: input_sha256 disagrees with run pinning")
        for field, want in (
            ("reduction_track", "whole_script"),
            ("experiment_line", "internal"),
            ("execution_mode", "native-process"),
            ("run_type", "score"),
        ):
            if row.get(field) != want:
                raise ValueError(f"slot {key}: {field} {row.get(field)!r} != {want!r}")
        if partitioned._finite_pos_float(row.get("budget_sec")) != partitioned._finite_pos_float(run.get("budget_sec")):
            raise ValueError(f"slot {key}: budget_sec does not match run configuration")
        if partitioned._finite_pos_float(row.get("oracle_timeout_sec")) != partitioned._finite_pos_float(run.get("oracle_timeout_sec")):
            raise ValueError(f"slot {key}: oracle_timeout_sec does not match run configuration")
        native = row.get("native_identity") or {}
        fatal = []
        missing = []
        def check_native(name, got, want):
            if got is None:
                missing.append(name)
            elif got != want:
                fatal.append(f"{name} {got!r} != {want!r}")
        check_native("metricAlgorithm", native.get("metricAlgorithm"), arm)
        check_native("intra_statement", native.get("intra_statement"), True)
        check_native("cache", native.get("cache"), True)
        check_native("column_mode", native.get("column_mode"), EXPECTED_COLUMN_MODES[arm])
        if row.get("cost_attribution") is None:
            missing.append("cost_attribution")
        else:
            check_native("policy_mode", native.get("policy_mode"), EXPECTED_COLUMN_MODES[arm])
            check_native("policy_jobs", native.get("policy_jobs"), 1)
            check_native("policy_search", native.get("policy_search"), EXPECTED_SEARCH_POLICIES[arm])
        check_native("budget_jobs", native.get("budget_jobs"), 1)
        if fatal:
            raise ValueError(f"slot {key}: native identity mismatch: {'; '.join(fatal)}")
        row["profile_complete"] = not missing
        row["profile_missing"] = sorted(missing)
    affinity = run.get("cpu_affinity") or []
    if len(affinity) != 1:
        raise ValueError(f"resource protocol requires one pinned CPU, got {affinity!r}")
    if "GHCRTS=-N1" not in run.get("resource_protocol", ""):
        raise ValueError("resource protocol text lacks GHCRTS=-N1")
    if run.get("tool_order") != "rotate arm order by case index + repetition index; serial children":
        raise ValueError("unexpected tool_order for a strict triad run")
    return rows


def report(directory):
    run = json.loads((directory / "run.json").read_text())
    rows = [json.loads(line) for line in (directory / "results.jsonl").read_text().splitlines()]
    arms = run["selected_arms"]
    by_arm = {arm: [row for row in rows if row["arm"] == arm] for arm in arms}
    validate_cohort(directory, run, rows)
    tools = {}
    for arm, children in by_arm.items():
        statuses = {}
        for row in children:
            statuses[row["status"]] = statuses.get(row["status"], 0) + 1
        tools[arm] = {
            "total": len(children),
            "completed": sum(1 for row in children if row["status"] == "completed" and row["profile_complete"]),
            "statuses": statuses,
            "incomplete_profile": sum(1 for row in children if not row["profile_complete"]),
            "incomplete_underlying": {
                s: sum(1 for row in children if not row["profile_complete"] and row["status"] == s)
                for s in sorted({row["status"] for row in children if not row["profile_complete"]})
            },
            "validated_checkpoints": sum(1 for row in children if row["status"] != "completed" and row["validated_twice"]),
            "unvalidated": sum(1 for row in children if not row["validated_twice"]),
            "cost_present": sum(1 for row in children if row["cost_present"]),
            "diag_complete_children": sum(1 for row in children if row["diag_complete"]) if run["diag_enabled"] else None,
        }
    cases = {}
    for case in run["case_order"]:
        entry = {"initial_bytes": next(row["initial_bytes"] for row in rows if row["case_id"] == case)}
        for arm in arms:
            sizes = sorted(row["result_bytes"] for row in by_arm[arm] if row["case_id"] == case and row["validated_twice"])
            entry[arm] = (sizes[len(sizes) // 2] if len(sizes) % 2 else (sizes[len(sizes) // 2 - 1] + sizes[len(sizes) // 2]) / 2) if sizes else None
        cases[case] = entry
    pairs = {}
    pairs_order = [pair for pair in (("column-off", "column-single"), ("column-off", "column-atomic"), ("column-single", "column-atomic")) if pair[0] in arms and pair[1] in arms]
    for left, right in pairs_order:
        completed = {}
        for row in rows:
            if row["status"] == "completed" and row["profile_complete"]:
                completed.setdefault((row["case_id"], row["repetition"]), {})[row["arm"]] = row
        joint = []
        for key, slot in completed.items():
            if left in slot and right in slot:
                joint.append((slot[left], slot[right]))
        wall = sorted(b["reducer_wall_time_sec"] / a["reducer_wall_time_sec"] for a, b in joint if a["reducer_wall_time_sec"] > 0)
        obs = sorted(b["audited_oracle_observations"] / a["audited_oracle_observations"] for a, b in joint if a["audited_oracle_observations"] > 0)
        pairs[f"{left}_vs_{right}"] = {
            "completed": len(joint),
            "possible": len({(row["case_id"], row["repetition"]) for row in rows}),
            "median_wall_ratio_right_over_left": (wall[len(wall) // 2] if len(wall) % 2 else (wall[len(wall) // 2 - 1] + wall[len(wall) // 2]) / 2) if wall else None,
            "median_observation_ratio_right_over_left": (obs[len(obs) // 2] if len(obs) % 2 else (obs[len(obs) // 2 - 1] + obs[len(obs) // 2]) / 2) if obs else None,
            "right_bytes_smaller": sum(1 for a, b in joint if b["result_bytes"] < a["result_bytes"]),
            "bytes_tied": sum(1 for a, b in joint if b["result_bytes"] == a["result_bytes"]),
            "right_bytes_larger": sum(1 for a, b in joint if b["result_bytes"] > a["result_bytes"]),
        }
    cost = {}
    for arm in arms:
        measured_oracle = []
        measured_reset = []
        for row in by_arm[arm]:
            snap = row.get("cost_attribution") or {}
            fams = snap.get("snapshotFamilies") or {}
            for counters in fams.values():
                if isinstance(counters, dict):
                    if counters.get("oracleMicros") is not None:
                        measured_oracle.append(counters["oracleMicros"])
                    if counters.get("resetMicros") is not None:
                        measured_reset.append(counters["resetMicros"])
        cost[arm] = {
            "children_with_cost": sum(1 for row in by_arm[arm] if row["cost_present"]),
            "children_total": len(by_arm[arm]),
            "families_measured_oracle": len(measured_oracle),
            "sum_oracle_micros_measured_only": sum(measured_oracle) if measured_oracle else None,
            "families_measured_reset": len(measured_reset),
            "sum_reset_micros_measured_only": sum(measured_reset) if measured_reset else None,
        }
    summary = {
        "records": len(rows), "selected_arms": arms, "tools": tools,
        "cases": cases, "completed_pairs": pairs, "cost": cost,
    }
    compare.save(directory / "summary.json", summary)
    lines = [
        "Selected arms: " + ", ".join(arms) + ". Internal line; completion is arm-local.",
        "Selected SQLite seed/scale inputs, not independent bugs. All arms share the same",
        "frozen occam-eval binary, inputs, predicate, budgets, and protocol; only the",
        "column mode differs. Noncompleted outputs are checkpoints, never completions.",
        "",
        "| Arm | Completed | Twice-validated checkpoints | Unvalidated | Cost present |",
        "| --- | ---: | ---: | ---: | ---: |",
    ]
    for arm in arms:
        entry = tools[arm]
        lines.append(f"| {arm} | {entry['completed']}/{entry['total']} | {entry['validated_checkpoints']} | {entry['unvalidated']} | {entry['cost_present']}/{entry['total']} |")
    incomplete_total = sum(entry["incomplete_profile"] for entry in tools.values())
    lines.append("Completed counts verified completions only (native profile + cost evidence present).")
    lines.append(f"INCOMPLETE_PROFILE rows: {incomplete_total} (excluded from completed counts; underlying censor statuses kept in summary).")
    lines += ["", "## Budget-limited output size", ""]
    lines.append("| Case | Input B | " + " | ".join(f"{arm} B" for arm in arms) + " |")
    lines.append("| --- | ---: | " + " | ".join("---:" for _ in arms) + " |")
    for case in run["case_order"]:
        entry = cases[case]
        lines.append(f"| {case} | {entry['initial_bytes']} | " + " | ".join(str(entry[arm]) for arm in arms) + " |")
    lines += ["", "## Jointly completed pairs only", ""]
    lines.append("Ratios are right-over-left on reducer wall / audited observations. Censored pairs excluded.")
    lines.append("Not causally comparable to external-baseline cohorts: different tools and protocol.")
    lines.append("")
    lines.append("| Pair | Completed pairs / possible | Median wall ratio | Median observation ratio |")
    lines.append("| --- | ---: | ---: | ---: |")
    for key, entry in pairs.items():
        wall_ratio = entry["median_wall_ratio_right_over_left"]
        obs_ratio = entry["median_observation_ratio_right_over_left"]
        lines.append(f"| {key} | {entry['completed']}/{entry['possible']} | {wall_ratio if wall_ratio is not None else 'unavailable'} | {obs_ratio if obs_ratio is not None else 'unavailable'} |")
    lines += ["", "## Measured oracle/reset micros (available per-observation profiles)", ""]
    lines.append("Sums span measured family profiles across all cohort statuses (missing/unmeasured excluded, never zero); canceled work bills separate wall. Not total CPU.")
    lines.append("")
    lines.append("| Arm | Oracle sum us (measured fams) | Reset sum us (measured fams) |")
    lines.append("| --- | ---: | ---: |")
    for arm in arms:
        entry = cost[arm]
        oracle = entry["sum_oracle_micros_measured_only"]
        reset = entry["sum_reset_micros_measured_only"]
        lines.append(f"| {arm} | {oracle if oracle is not None else 'unknown'} ({entry['families_measured_oracle']}) | {reset if reset is not None else 'unknown'} ({entry['families_measured_reset']}) |")
    if run["diag_enabled"]:
        covered = sum(1 for row in rows if row["diag_complete"])
        lines += ["", f"Diagnostic sidecar: {covered}/{len(rows)} children fully covered (every reducer digest has a row).", ""]
    text = "\n".join(lines) + "\n"
    (directory / "report.md").write_text(text, encoding="utf-8")
    print(text)
    return summary


def main():
    if sys.argv[1:2] == ["report"]:
        parser = argparse.ArgumentParser(description="Report a complete three-arm column ablation")
        parser.add_argument("directory", type=Path)
        report(parser.parse_args(sys.argv[2:]).directory.resolve())
        return
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--occam-eval", type=Path, required=True)
    parser.add_argument("--cases-from", type=Path, required=True, help="prior bundle (byte-pinned reuse) or plain SQL+metadata dir (fresh mechanism mint)")
    parser.add_argument("--arms", nargs="+", choices=ARMS, default=list(ARMS))
    parser.add_argument("--expect-cases", type=int, default=8, help="exact cohort size required (8 primary, 3 mechanism)")
    parser.add_argument("--production-source", type=Path, action="append", default=[])
    parser.add_argument("--budget", type=float, default=20)
    parser.add_argument("--timeout", type=float, default=1)
    parser.add_argument("--repetitions", type=int, default=2)
    parser.add_argument("--diag", action="store_true", help="enable the optional diagnostic sidecar per child (metrics only)")
    args = parser.parse_args()
    if len(set(args.arms)) != len(args.arms) or not args.arms:
        parser.error("selected arms must be a non-empty list without duplicates")
    if args.expect_cases < 1:
        parser.error("expected cases must be at least 1")
    selected_arms = tuple(arm for arm in ARMS if arm in args.arms)
    try:
        prod_sources = compare.production_sources(args.production_source)
    except ValueError as exc:
        parser.error(str(exc))
    if args.timeout <= 0 or args.budget <= 3 * (args.timeout + 0.25) or args.repetitions < 1:
        parser.error("positive timeout/repetitions and a budget exceeding three checks are required")
    args.out = args.out.resolve()
    args.occam_eval = args.occam_eval.resolve()
    args.cases_from = args.cases_from.resolve()
    if not args.cases_from.is_dir():
        parser.error("--cases-from must name a bundle or plain SQL+metadata directory")
    if args.out.exists() and any(args.out.iterdir()):
        parser.error("choose a new empty output directory; evidence is never overwritten")
    args.out.mkdir(parents=True, exist_ok=True)
    identities = {
        "occam_eval": {
            "binary": str(args.occam_eval),
            "sha256": compare.digest(args.occam_eval),
            "mode": f"native occam-eval --profile {{{','.join(selected_arms)}}} (intra=true, cache=true)",
        }
    }
    environment = os.environ | {"GHCRTS": "-N1"}
    original_affinity = sorted(os.sched_getaffinity(0)) if hasattr(os, "sched_getaffinity") else None
    if original_affinity:
        os.sched_setaffinity(0, {original_affinity[0]})
    cases, pinned = prepare(args.out, args.timeout, args.cases_from, args.expect_cases)
    files = [
        Path(__file__).resolve(),
        ROOT / "benchmarks/tools/compare_reducers.py",
        ROOT / "benchmarks/tools/scale_study.py",
        ROOT / "benchmarks/fuzz_corpus/run_docker_benchmark.py",
        ROOT / "benchmarks/upstream/oracle_exec.py",
    ]
    snapshots = args.out / "sources"
    snapshots.mkdir()
    for source in files:
        shutil.copyfile(source, snapshots / source.name)
    for source in prod_sources:
        target = snapshots / "production" / source.relative_to(ROOT)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source, target)
    source_hashes = {str(source.relative_to(ROOT)): compare.digest(source) for source in files}
    production_hashes = {str(source.relative_to(ROOT)): compare.digest(source) for source in prod_sources}
    compare.save(
        args.out / "run.json",
        {
            "run_schema_version": 2,
            "command": sys.argv,
            "selected_arms": list(selected_arms),
            "tools": identities,
            "python": sys.version,
            "sqlite": sqlite3.sqlite_version,
            "platform": platform.platform(),
            "budget_sec": args.budget,
            "oracle_timeout_sec": args.timeout,
            "max_oracle_executions": None,
            "repetitions": args.repetitions,
            "case_order": [case["id"] for case in cases],
            "bytes_source_bundle": str(args.cases_from),
            "pinned_input_sha256": pinned,
            "diag_enabled": args.diag,
            "tool_order": "rotate arm order by case index + repetition index; serial children",
            "cpu_affinity": sorted(os.sched_getaffinity(0)) if original_affinity else None,
            "original_cpu_affinity": original_affinity,
            "resource_protocol": "one inherited CPU; GHCRTS=-N1; no page-cache flush; no separate warmup",
            "cache_protocol": "native occam-eval per-child cache only; no external-callback adapter verdict cache",
            "timing_scope": "input check, tool process and two common final checks; installations excluded",
            "sources": source_hashes,
            "production_sources": production_hashes,
            "scope": "three-arm column ablation on selected SQLite seed/scale inputs; internal line; only the column mode differs; not causally comparable to external-baseline cohorts",
        },
    )
    with (args.out / "results.jsonl").open("w", encoding="utf-8") as output:
        for repetition in range(1, args.repetitions + 1):
            for index, case in enumerate(cases):
                rotation = (index + repetition - 1) % len(selected_arms)
                order = selected_arms[rotation:] + selected_arms[:rotation]
                for arm in order:
                    row = run_one(args, case, arm, repetition, environment)
                    output.write(json.dumps(row, sort_keys=True) + "\n")
                    output.flush()
                    print(f"{case['id']} rep={repetition} {arm}: {row['status']} {row['initial_bytes']}->{row['result_bytes']} B, {row['audited_oracle_observations']} observations, {row['outer_wall_time_sec']:.3f}s, validated={row['validated_twice']}, cost={row['cost_present']}, diag={row['diag_complete']}", flush=True)
    report(args.out)


if __name__ == "__main__":
    main()

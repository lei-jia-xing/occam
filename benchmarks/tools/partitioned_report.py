#!/usr/bin/env python3
"""Strictly partitioned benchmark reporter for internal controlled ablations.

Derives paired deltas within ONE qualified current run directory where:
- Comparison sides (profile, child_id) are comparison targets, NOT equality keys.
- Run metadata binds declared cases (with structured contract hashes), limits,
  resources, and validation scopes.
- Planned comparison slots (cases × repetitions) track all outcomes: paired, missing,
  ambiguous, or rejected with full structured exclusions.
- Native settings must match identically across all keys except cache (True base, False target).
- Terminal observation, physical validation, and native exit code 0 dominate stale flags.
- Residual wall time is explicitly noted as non-CPU attribution.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import sys
from pathlib import Path
from typing import Any, Iterable, Sequence

RESIDUAL_WALL_NOTE = (
    "Residual wall time is wall-clock duration minus (predicate + reset duration); "
    "it reflects runtime overhead, process scheduling, and parsing latency, NOT CPU attribution."
)

MANDATORY_SETTINGS_KEYS = (
    "kind",
    "slicing",
    "closure",
    "intra_statement",
    "unrestricted",
    "dialect",
    "cache",
)

REQUIRED_SCOPE_KEYS = (
    "preflight_scope",
    "oracle_calls_scope",
    "validation_calls_scope",
    "execution_cap_scope",
)


def _exact_int(val: Any, min_val: int = 0) -> int | None:
    if type(val) is not int:
        return None
    return val if val >= min_val else None


def _finite_pos_float(val: Any) -> float | None:
    if type(val) not in (float, int) or isinstance(val, bool):
        return None
    f = float(val)
    return f if math.isfinite(f) and f > 0.0 else None


def _metric_int(val: Any) -> int | None:
    if type(val) is not int:
        return None
    return val if val >= 0 else None


def _metric_float(val: Any) -> float | None:
    if type(val) not in (float, int) or isinstance(val, bool):
        return None
    f = float(val)
    return round(f, 6) if math.isfinite(f) and f >= 0.0 else None


def _is_sha256(val: Any) -> bool:
    return isinstance(val, str) and len(val) == 64 and all(c in "0123456789abcdefABCDEF" for c in val)
def _canonical_contract_sha256(contract: Any) -> str | None:
    if not isinstance(contract, (dict, str)):
        return None
    if isinstance(contract, dict):
        raw = json.dumps(contract, sort_keys=True).encode("utf-8")
    else:
        raw = contract.encode("utf-8")
    return hashlib.sha256(raw).hexdigest()


def qualify_run(
    meta: dict[str, Any],
    base_profile: str,
    target_profile: str,
) -> tuple[bool, str | None, dict[str, Any]]:
    """Validate that run_metadata satisfies the qualified internal score contract."""
    if not isinstance(meta, dict):
        return False, "run_metadata is not a dictionary", {}

    run_id = meta.get("run_id")
    if not isinstance(run_id, str) or not run_id:
        return False, "missing or empty run_id", {}

    if meta.get("experiment_line") != "internal":
        return False, f"experiment_line {meta.get('experiment_line')!r} != 'internal'", {}

    if meta.get("execution_mode") != "native":
        return False, f"execution_mode {meta.get('execution_mode')!r} != 'native'", {}

    if meta.get("run_type") != "score":
        return False, f"run_type {meta.get('run_type')!r} != 'score'", {}

    binary = meta.get("native_binary")
    if not isinstance(binary, dict) or not binary.get("path") or not binary.get("sha256"):
        return False, "missing native_binary path or sha256", {}

    cfg = meta.get("configuration")
    if not isinstance(cfg, dict):
        return False, "missing configuration dictionary", {}

    profiles = cfg.get("profiles")
    if not isinstance(profiles, list) or base_profile not in profiles or target_profile not in profiles:
        return False, f"configuration profiles do not include {base_profile!r} and {target_profile!r}", {}

    repetitions = _exact_int(cfg.get("repetitions"), min_val=1)
    if repetitions is None:
        return False, "invalid repetitions in configuration (must be exact integer >= 1)", {}

    p_timeout = _finite_pos_float(cfg.get("predicate_timeout_sec"))
    budget = _finite_pos_float(cfg.get("budget_sec"))
    max_execs = _exact_int(cfg.get("max_oracle_executions"), min_val=1)
    outer_timeout = _finite_pos_float(cfg.get("outer_timeout_sec"))
    if p_timeout is None or budget is None or max_execs is None or outer_timeout is None:
        return False, "invalid limits in configuration (must be finite positive numbers)", {}

    resources = cfg.get("resources")
    if not isinstance(resources, dict) or not resources:
        return False, "missing or empty resources in configuration", {}
    if resources.get("serial") is not True or resources.get("jobs") != 1:
        return False, "resources must declare serial execution with jobs=1", {}
    if not isinstance(resources.get("cpu_affinity"), str) or not resources["cpu_affinity"]:
        return False, "resources must specify cpu_affinity", {}

    v_phases = cfg.get("validation_phases")
    if not isinstance(v_phases, dict) or not v_phases.get("preflight") or not v_phases.get("final"):
        return False, "missing validation_phases specification in configuration", {}

    c_scope = cfg.get("count_scope")
    if not isinstance(c_scope, dict) or not c_scope.get("oracle_calls") or not c_scope.get("validation_calls"):
        return False, "missing count_scope specification in configuration", {}

    raw_cases = cfg.get("cases")
    if not isinstance(raw_cases, list) or not raw_cases:
        return False, "configuration.cases must be a non-empty list", {}

    declared_cases: dict[str, dict[str, Any]] = {}
    for entry in raw_cases:
        if not isinstance(entry, dict):
            return False, "each configuration case must be an object", {}
        cid = entry.get("case_id")
        if not isinstance(cid, str) or not cid:
            return False, "declared case missing case_id", {}
        if cid in declared_cases:
            return False, f"duplicate case_id in configuration: {cid}", {}

        input_sha = entry.get("input_sha256")
        track = entry.get("reduction_track")
        pred_sha = entry.get("predicate_command_sha256")
        reset_sha = entry.get("reset_command_sha256")
        contract = entry.get("case_contract")
        contract_sha = entry.get("case_contract_sha256")

        if not all(_is_sha256(x) for x in (input_sha, pred_sha, reset_sha, contract_sha)):
            return False, f"case {cid} missing required 64-char hex identity hashes", {}
        if not isinstance(track, str) or not track:
            return False, f"case {cid} missing reduction_track", {}

        # Validate explicit structured case_contract
        if not isinstance(contract, dict):
            return False, f"case {cid} case_contract must be an explicit dictionary", {}

        effect_scope = contract.get("effect_scope")
        comparison_policy = contract.get("comparison_policy")
        environment = contract.get("environment")
        hashes = contract.get("hashes")
        declared_scope = contract.get("declared_scope")

        if not isinstance(effect_scope, str) or not effect_scope:
            return False, f"case {cid} case_contract missing effect_scope", {}
        if not isinstance(comparison_policy, str) or not comparison_policy:
            return False, f"case {cid} case_contract missing comparison_policy", {}
        if not isinstance(declared_scope, str) or not declared_scope:
            return False, f"case {cid} case_contract missing declared_scope", {}

        if not isinstance(environment, dict) or not environment.get("python_executable") \
                or not environment.get("python_version") or not environment.get("sqlite_version"):
            return False, f"case {cid} case_contract missing valid environment specification", {}

        if not isinstance(hashes, dict):
            return False, f"case {cid} case_contract missing hashes dictionary", {}

        sql_sha = hashes.get("sql_sha256")
        template_sha = hashes.get("template_sha256")
        metadata_sha = hashes.get("metadata_sha256")
        helper_sha = hashes.get("helper_sha256")

        if not all(_is_sha256(h) for h in (sql_sha, template_sha, metadata_sha, helper_sha)):
            return False, f"case {cid} case_contract hashes must contain 64-char hex strings", {}

        if sql_sha != input_sha:
            return False, f"case {cid} case_contract sql_sha256 does not match case input_sha256", {}

        computed_sha = _canonical_contract_sha256(contract)
        if computed_sha != contract_sha:
            return False, f"case {cid} case_contract_sha256 does not match canonical hash of case_contract", {}

        declared_cases[cid] = {
            "case_id": cid,
            "input_sha256": input_sha,
            "reduction_track": track,
            "predicate_command_sha256": pred_sha,
            "reset_command_sha256": reset_sha,
            "case_contract_sha256": contract_sha,
        }

    plan = {
        "run_id": run_id,
        "run_type": "score",
        "experiment_line": "internal",
        "execution_mode": "native",
        "binary_sha256": str(binary["sha256"]),
        "predicate_timeout_sec": p_timeout,
        "budget_sec": budget,
        "max_oracle_executions": max_execs,
        "outer_timeout_sec": outer_timeout,
        "repetitions": repetitions,
        "declared_cases": declared_cases,
    }
    return True, None, plan


def validate_row(row: dict[str, Any], plan: dict[str, Any]) -> tuple[bool, str | None]:
    """Validate that a raw evidence row strictly conforms to the root run plan."""
    if row.get("run_id") != plan["run_id"]:
        return False, f"foreign run_id: {row.get('run_id')!r}"
    if row.get("experiment_line") != plan["experiment_line"]:
        return False, f"cross-line record: {row.get('experiment_line')!r}"
    if row.get("execution_mode") != plan["execution_mode"]:
        return False, f"mismatched mode: {row.get('execution_mode')!r}"
    if row.get("run_type") != plan["run_type"]:
        return False, f"mismatched run_type: {row.get('run_type')!r}"

    if _finite_pos_float(row.get("oracle_timeout_sec")) != plan["predicate_timeout_sec"]:
        return False, "oracle_timeout_sec does not match root configuration"
    if _finite_pos_float(row.get("budget_sec")) != plan["budget_sec"]:
        return False, "budget_sec does not match root configuration"
    if _exact_int(row.get("max_oracle_executions"), min_val=1) != plan["max_oracle_executions"]:
        return False, "max_oracle_executions does not match root configuration"
    if _finite_pos_float(row.get("outer_timeout_sec")) != plan["outer_timeout_sec"]:
        return False, "outer_timeout_sec does not match root configuration"

    rep = _exact_int(row.get("repetition"), min_val=1)
    if rep is None or rep > plan["repetitions"]:
        return False, f"invalid repetition: {row.get('repetition')!r}"

    child_id = row.get("child_id")
    if not isinstance(child_id, str) or not child_id:
        return False, "missing or empty child_id"

    cid = row.get("case_id")
    if cid not in plan["declared_cases"]:
        return False, f"case_id {cid!r} not declared in root configuration"

    decl = plan["declared_cases"][cid]
    for key in ("input_sha256", "reduction_track", "predicate_command_sha256",
                "reset_command_sha256", "case_contract_sha256"):
        if row.get(key) != decl[key]:
            return False, f"case {cid} mismatched {key}: {row.get(key)!r} != {decl[key]!r}"

    for scope_key in REQUIRED_SCOPE_KEYS:
        if not isinstance(row.get(scope_key), str) or not row[scope_key]:
            return False, f"missing scope field: {scope_key}"

    return True, None


def validate_settings(base: dict[str, Any], target: dict[str, Any]) -> tuple[bool, str | None]:
    """Ensure base and target native settings differ ONLY in candidate memoization."""
    b_set = base.get("settings")
    t_set = target.get("settings")
    if not isinstance(b_set, dict) or not isinstance(t_set, dict):
        return False, "missing settings dictionary"

    if set(b_set.keys()) != set(t_set.keys()):
        diff = set(b_set.keys()) ^ set(t_set.keys())
        return False, f"settings keys mismatch: {sorted(diff)}"

    for req in MANDATORY_SETTINGS_KEYS:
        if req not in b_set:
            return False, f"missing mandatory settings key: {req}"

    # Mandatory pass flags must be real bools
    for flag in ("slicing", "closure", "intra_statement", "unrestricted", "cache"):
        if type(b_set.get(flag)) is not bool:
            return False, f"base settings {flag} is {b_set.get(flag)!r}, expected bool"
        if type(t_set.get(flag)) is not bool:
            return False, f"target settings {flag} is {t_set.get(flag)!r}, expected bool"

    # Mandatory strings must be non-empty strings
    for s_key in ("kind", "dialect"):
        if not isinstance(b_set.get(s_key), str) or not b_set[s_key]:
            return False, f"base settings {s_key} must be non-empty string"
        if not isinstance(t_set.get(s_key), str) or not t_set[s_key]:
            return False, f"target settings {s_key} must be non-empty string"

    if b_set.get("cache") is not True:
        return False, f"base profile settings cache is {b_set.get('cache')!r}, expected True"
    if t_set.get("cache") is not False:
        return False, f"target profile settings cache is {t_set.get('cache')!r}, expected False"

    for k in b_set:
        if k == "cache":
            continue
        b_val = b_set[k]
        t_val = t_set[k]
        if type(b_val) is not type(t_val) or b_val != t_val or b_val is None:
            return False, f"reduction pass mismatch on {k}: base={b_val!r}({type(b_val).__name__}) vs target={t_val!r}({type(t_val).__name__})"
    return True, None


def is_validated_complete(row: dict[str, Any]) -> tuple[bool, str]:
    """Check whether a single child execution completed with rigorous validation evidence."""
    if row.get("status") != "completed":
        return False, str(row.get("status_reason") or row.get("status"))
    if row.get("predicate_passed") is not True:
        return False, "predicate_passed is not True"
    if row.get("native_validation_passed") is not True:
        return False, "native_validation_passed is not True"

    # Strict score native returncode check
    native_rc = row.get("native_process_returncode")
    if type(native_rc) is not int or native_rc != 0:
        return False, f"native_process_returncode is {native_rc!r}, expected exact int 0"
    wrapper_rc = row.get("wrapper_process_returncode")
    if wrapper_rc is not None and (type(wrapper_rc) is not int or wrapper_rc != 0):
        return False, f"wrapper_process_returncode is {wrapper_rc!r}, expected exact int 0 or None"
    if row.get("result_artifact_present") is not True:
        return False, f"result_artifact_present is {row.get('result_artifact_present')!r}, expected True"
    term = row.get("terminal_observation")
    if not isinstance(term, dict) or term.get("status") != "completed":
        return False, f"terminal observation not completed: {term}"

    preflight = _exact_int(row.get("preflight_calls_debited"))
    if preflight != 1:
        return False, f"preflight calls debited is {preflight!r}, expected 1"

    v_calls = _exact_int(row.get("validation_calls"))
    v_obs = _exact_int(row.get("validation_observations"))
    if v_calls is None or v_calls < 2 or v_obs is None or v_obs < 2:
        return False, f"insufficient final validation checks: calls={v_calls}, obs={v_obs}"

    if not row.get("result_artifact_present") or _metric_int(row.get("reduced_bytes")) is None:
        return False, "minimal.sql artifact missing or unmeasured"

    return True, "completed"


def build_pair_record(
    base: dict[str, Any],
    target: dict[str, Any],
    base_name: str,
    target_name: str,
) -> dict[str, Any]:
    """Derive paired deltas for one matched slot."""
    b_ok, b_msg = is_validated_complete(base)
    t_ok, t_msg = is_validated_complete(target)

    if b_ok and t_ok:
        status = "completed"
        reason = None
    elif base.get("status") == "timeout" or target.get("status") == "timeout":
        status = "timeout"
        reason = target.get("status_reason") if target.get("status") == "timeout" else base.get("status_reason")
    elif base.get("status") == "tool-error" or target.get("status") == "tool-error":
        status = "tool-error"
        reason = target.get("status_reason") if target.get("status") == "tool-error" else base.get("status_reason")
    else:
        status = "validation_incomplete"
        reason = f"base: {b_msg}; target: {t_msg}"

    b_out = base.get("output_sha256")
    t_out = target.get("output_sha256")
    if _is_sha256(b_out) and _is_sha256(t_out):
        identical = (b_out.lower() == t_out.lower())
    else:
        identical = None
    pair: dict[str, Any] = {
        "case_id": base["case_id"],
        "repetition": base["repetition"],
        "base_profile": base_name,
        "target_profile": target_name,
        "base_child_id": base["child_id"],
        "target_child_id": target["child_id"],
        "status": status,
        "status_reason": reason,
        "base_status": base.get("status"),
        "base_status_reason": base.get("status_reason"),
        "target_status": target.get("status"),
        "target_status_reason": target.get("status_reason"),
        "output_identical": identical,
        "base": {
            "reduced_bytes": _metric_int(base.get("reduced_bytes")),
            "reduced_stmts": _metric_int(base.get("reduced_stmts")),
            "oracle_calls": _metric_int(base.get("oracle_calls")),
            "candidate_checks": _metric_int(base.get("candidate_checks")),
            "oracle_cache_hits": _metric_int(base.get("oracle_cache_hits")),
            "wall_time_sec": _metric_float(base.get("wall_time_sec")),
            "unattributed_wall_time_sec": _metric_float(base.get("unattributed_wall_time_sec")),
            "output_sha256": b_out,
        },
        "target": {
            "reduced_bytes": _metric_int(target.get("reduced_bytes")),
            "reduced_stmts": _metric_int(target.get("reduced_stmts")),
            "oracle_calls": _metric_int(target.get("oracle_calls")),
            "candidate_checks": _metric_int(target.get("candidate_checks")),
            "oracle_cache_hits": _metric_int(target.get("oracle_cache_hits")),
            "wall_time_sec": _metric_float(target.get("wall_time_sec")),
            "unattributed_wall_time_sec": _metric_float(target.get("unattributed_wall_time_sec")),
            "output_sha256": t_out,
        },
        "delta": None,
    }

    if status == "completed":
        def _diff(key: str) -> Any:
            bv = pair["base"].get(key)
            tv = pair["target"].get(key)
            if bv is not None and tv is not None:
                d = tv - bv
                return round(d, 6) if isinstance(d, float) else d
            return None

        pair["delta"] = {
            "delta_reduced_bytes": _diff("reduced_bytes"),
            "delta_reduced_stmts": _diff("reduced_stmts"),
            "delta_oracle_calls": _diff("oracle_calls"),
            "delta_candidate_checks": _diff("candidate_checks"),
            "delta_oracle_cache_hits": _diff("oracle_cache_hits"),
            "delta_wall_time_sec": _diff("wall_time_sec"),
            "delta_unattributed_wall_time_sec": _diff("unattributed_wall_time_sec"),
        }

    return pair


def pair_internal_ablation(
    run_dir: Path | str | None = None,
    *,
    run_metadata: dict[str, Any] | None = None,
    records: Iterable[dict[str, Any]] | None = None,
    base_profile: str = "full",
    target_profile: str = "no-cache",
) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    """Strictly pair raw evidence records for an internal ablation within one qualified run."""
    if run_dir is not None:
        p = Path(run_dir)
        run_file = p / "run.json"
        results_file = p / "results.jsonl"
        if not run_file.is_file():
            raise FileNotFoundError(f"run.json not found in {p}")
        if not results_file.is_file():
            raise FileNotFoundError(f"results.jsonl not found in {p}")
        run_metadata = json.loads(run_file.read_text(encoding="utf-8"))
        records = [
            json.loads(line) for line in results_file.read_text(encoding="utf-8").splitlines()
            if line.strip()
        ]

    if run_metadata is None or records is None:
        raise ValueError("pair_internal_ablation requires either run_dir or both run_metadata and records")

    records_list = list(records)

    qualified, disqualification_reason, plan = qualify_run(run_metadata, base_profile, target_profile)
    if not qualified:
        summary = {
            "qualified": False,
            "disqualification_reason": disqualification_reason,
            "base_profile": base_profile,
            "target_profile": target_profile,
            "total_records_read": len(records_list),
            "expected_slots_count": 0,
            "paired_instances_count": 0,
            "completed_pairs_count": 0,
            "noncompleted_pairs_count": 0,
            "missing_both_slots_count": 0,
            "unpaired_base_count": 0,
            "unpaired_target_count": 0,
            "ambiguous_slots_count": 0,
            "rejected_settings_slots_count": 0,
            "rejected_child_id_slots_count": 0,
            "rejected_scopes_slots_count": 0,
            "incompatible_protocol_records": len(records_list),
            "exclusions": {
                "missing_both": [],
                "unpaired_base": [],
                "unpaired_target": [],
                "ambiguous": [],
                "rejected_settings": [],
                "rejected_child_id": [],
                "rejected_scopes": [],
                "incompatible_protocol": [{"record_index": i, "reason": "unqualified run"} for i in range(len(records_list))],
            },
            "residual_wall_scope": RESIDUAL_WALL_NOTE,
        }
        return [], summary

    total_records = 0
    incompatible_records: list[dict[str, Any]] = []
    other_profile_records = 0

    base_by_slot: dict[tuple[str, int], list[dict[str, Any]]] = {}
    target_by_slot: dict[tuple[str, int], list[dict[str, Any]]] = {}

    for row in records_list:
        total_records += 1
        if not isinstance(row, dict):
            incompatible_records.append({"row": row, "reason": "not a dictionary"})
            continue

        prof = row.get("profile")
        if prof not in (base_profile, target_profile):
            other_profile_records += 1
            continue

        valid, err = validate_row(row, plan)
        if not valid:
            incompatible_records.append({"child_id": row.get("child_id"), "reason": err})
            continue

        cid = row["case_id"]
        rep = row["repetition"]
        slot_key = (cid, rep)

        if prof == base_profile:
            base_by_slot.setdefault(slot_key, []).append(row)
        else:
            target_by_slot.setdefault(slot_key, []).append(row)

    expected_slots = [
        (cid, rep)
        for cid in sorted(plan["declared_cases"].keys())
        for rep in range(1, plan["repetitions"] + 1)
    ]

    paired_records: list[dict[str, Any]] = []
    missing_both: list[tuple[str, int]] = []
    unpaired_base: list[dict[str, Any]] = []
    unpaired_target: list[dict[str, Any]] = []
    ambiguous_slots: list[dict[str, Any]] = []
    rejected_child_slots: list[dict[str, Any]] = []
    rejected_settings_slots: list[dict[str, Any]] = []
    rejected_scopes_slots: list[dict[str, Any]] = []

    for slot_key in expected_slots:
        bases = base_by_slot.get(slot_key, [])
        targets = target_by_slot.get(slot_key, [])

        if len(bases) == 0 and len(targets) == 0:
            missing_both.append(slot_key)
        elif len(bases) == 1 and len(targets) == 0:
            unpaired_base.append(bases[0])
        elif len(bases) == 0 and len(targets) == 1:
            unpaired_target.append(targets[0])
        elif len(bases) > 1 or len(targets) > 1:
            ambiguous_slots.append({"slot": slot_key, "base_count": len(bases), "target_count": len(targets)})
        else:
            b = bases[0]
            t = targets[0]
            if b.get("child_id") == t.get("child_id"):
                rejected_child_slots.append({"slot": slot_key, "child_id": b.get("child_id")})
                continue

            # Scope equality check between sides
            scope_diff = None
            for sk in REQUIRED_SCOPE_KEYS + ("reset_calls_scope",):
                if b.get(sk) != t.get(sk):
                    scope_diff = f"scope mismatch on {sk}: {b.get(sk)!r} != {t.get(sk)!r}"
                    break
            if scope_diff is not None:
                rejected_scopes_slots.append({"slot": slot_key, "reason": scope_diff})
                continue

            settings_ok, s_err = validate_settings(b, t)
            if not settings_ok:
                rejected_settings_slots.append({"slot": slot_key, "reason": s_err})
                continue

            pair = build_pair_record(b, t, base_profile, target_profile)
            paired_records.append(pair)

    completed = [p for p in paired_records if p["status"] == "completed"]
    noncompleted = [p for p in paired_records if p["status"] != "completed"]
    identical_outputs = sum(1 for p in completed if p["output_identical"] is True)

    summary = {
        "qualified": True,
        "run_id": plan["run_id"],
        "binary_sha256": plan["binary_sha256"],
        "base_profile": base_profile,
        "target_profile": target_profile,
        "total_records_read": total_records,
        "expected_slots_count": len(expected_slots),
        "paired_instances_count": len(paired_records),
        "completed_pairs_count": len(completed),
        "noncompleted_pairs_count": len(noncompleted),
        "missing_both_slots_count": len(missing_both),
        "unpaired_base_count": len(unpaired_base),
        "unpaired_target_count": len(unpaired_target),
        "ambiguous_slots_count": len(ambiguous_slots),
        "rejected_settings_slots_count": len(rejected_settings_slots),
        "rejected_child_id_slots_count": len(rejected_child_slots),
        "rejected_scopes_slots_count": len(rejected_scopes_slots),
        "incompatible_protocol_records": len(incompatible_records),
        "other_profile_records": other_profile_records,
        "output_identical_count": identical_outputs,
        "exclusions": {
            "missing_both": [{"case_id": cid, "repetition": rep} for (cid, rep) in missing_both],
            "unpaired_base": [{"case_id": b["case_id"], "repetition": b["repetition"], "child_id": b.get("child_id")} for b in unpaired_base],
            "unpaired_target": [{"case_id": t["case_id"], "repetition": t["repetition"], "child_id": t.get("child_id")} for t in unpaired_target],
            "ambiguous": ambiguous_slots,
            "rejected_settings": rejected_settings_slots,
            "rejected_child_id": rejected_child_slots,
            "rejected_scopes": rejected_scopes_slots,
            "incompatible_protocol": incompatible_records,
        },
        "residual_wall_scope": RESIDUAL_WALL_NOTE,
    }

    return paired_records, summary


def render_markdown(paired: list[dict[str, Any]], summary: dict[str, Any]) -> str:
    """Format report as concise readable Markdown."""
    if not summary.get("qualified", True):
        return (
            f"## Internal Ablation Report (DISQUALIFIED)\n\n"
            f"**Disqualification Reason**: `{summary.get('disqualification_reason')}`\n"
            f"- Total records read: {summary.get('total_records_read')}\n"
            f"- Incompatible records: {summary.get('incompatible_protocol_records')}\n"
        )

    lines = [
        f"## Internal Ablation Report: `{summary['base_profile']}` vs `{summary['target_profile']}`",
        f"**Run ID**: `{summary.get('run_id')}` | **Binary SHA**: `{summary.get('binary_sha256', '')[:12]}...`",
        "",
        "| Case | Rep | Status | Base Bytes | Target Bytes | Δ Bytes | Base Calls | Target Calls | Δ Calls | Base Hits | Target Hits | Base Wall(s) | Target Wall(s) | Δ Wall(s) | Output Identical |",
        "| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |",
    ]

    def _val(v: Any, fmt: str | None = None) -> str:
        if v is None:
            return "N/A"
        return f"{v:{fmt}}" if fmt else str(v)

    for p in paired:
        b = p["base"]
        t = p["target"]
        d = p["delta"] or {}
        d_b = f"{d.get('delta_reduced_bytes'):+d}" if d.get("delta_reduced_bytes") is not None else "N/A"
        d_c = f"{d.get('delta_oracle_calls'):+d}" if d.get("delta_oracle_calls") is not None else "N/A"
        d_w = f"{d.get('delta_wall_time_sec'):+.3f}" if d.get("delta_wall_time_sec") is not None else "N/A"
        ident = "YES" if p["output_identical"] is True else ("NO" if p["output_identical"] is False else "N/A")

        lines.append(
            f"| {p['case_id']} | {p['repetition']} | {p['status']} | "
            f"{_val(b.get('reduced_bytes'))} | {_val(t.get('reduced_bytes'))} | {d_b} | "
            f"{_val(b.get('oracle_calls'))} | {_val(t.get('oracle_calls'))} | {d_c} | "
            f"{_val(b.get('oracle_cache_hits'))} | {_val(t.get('oracle_cache_hits'))} | "
            f"{_val(b.get('wall_time_sec'), '.3f')} | {_val(t.get('wall_time_sec'), '.3f')} | {d_w} | {ident} |"
        )

    lines.extend([
        "",
        "### Planned Slot Denominators",
        f"- Expected Comparison Slots: {summary['expected_slots_count']}",
        f"- Paired Slots: {summary['paired_instances_count']} (Completed: {summary['completed_pairs_count']}, Noncompleted: {summary['noncompleted_pairs_count']})",
        f"- Missing Slots: Both={summary['missing_both_slots_count']}, BaseOnly={summary['unpaired_base_count']}, TargetOnly={summary['unpaired_target_count']}",
        f"- Rejected Slots: Settings={summary['rejected_settings_slots_count']}, IdenticalChild={summary['rejected_child_id_slots_count']}, Scopes={summary['rejected_scopes_slots_count']}, Ambiguous={summary['ambiguous_slots_count']}",
        f"- Incompatible Protocol Records: {summary['incompatible_protocol_records']}",
        f"- Output Equivalence: {summary['output_identical_count']}/{summary['completed_pairs_count']}",
        "",
        f"**Scope Note**: {summary['residual_wall_scope']}",
    ])
    return "\n".join(lines)


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Derive strictly partitioned paired deltas for internal controlled ablations."
    )
    parser.add_argument(
        "run_dir",
        type=Path,
        help="Path to directory containing run.json and results.jsonl",
    )
    parser.add_argument(
        "--base",
        default="full",
        help="Base profile name (default: full)",
    )
    parser.add_argument(
        "--target",
        default="no-cache",
        help="Target ablation profile name (default: no-cache)",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="Output JSON summary and paired instances",
    )
    args = parser.parse_args(argv)

    try:
        paired, summary = pair_internal_ablation(
            args.run_dir.resolve(),
            base_profile=args.base,
            target_profile=args.target,
        )
    except Exception as exc:
        print(f"Error reading run directory: {exc}", file=sys.stderr)
        return 1

    if args.json:
        print(json.dumps({"summary": summary, "paired": paired}, indent=2))
    else:
        print(render_markdown(paired, summary))

    return 0


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""
batch_eval.py - Resumable container-per-worker horizontal evaluation orchestrator
for Occam multi-DBMS crash reduction experiments (Stage 1 / Stage 2).

Key features:
  - Container-per-worker parallelism aligned with `--jobs` (horizontal workers,
    each owning an isolated target DBMS container on dynamically bound 127.0.0.1
    ports, or an external container pool, or local embedded execution).
  - Phase 1: 5-replay input stability verification and RQ3 baseline crash
    signature extraction (`crash_oracle.py --print-signature`).
  - Phase 2: Multi-profile, multi-repeat Occam reduction (`audit-only`,
    `ddmin-only`, `Ours-S`, `Ours-SI`, `Ours-Full`, `Ours-NoDep`, or custom)
    plus post-reduction 5-replay stability & RQ3 slippage verification.
  - Crash-safe resumability (`--resume`, `--rerun-failed`, atomic `job_result.json`
    writes, stale temp/partial run cleanup, and SIGINT/SIGTERM container teardown).
  - Rigorous `stats.json` aggregation (`aggregated_stats.json` + `summary.md`)
    with Attempt-AUC, completed vs censored separation, and stratification by
    unique crash signature.
"""

import argparse
import datetime
import hashlib
import json
import math
import os
import re
import shlex
import shutil
import signal
import subprocess
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

HARNESS_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(HARNESS_DIR)
CRASH_ORACLE_PATH = os.path.join(HARNESS_DIR, "crash_oracle.py")
RESET_DB_PATH = os.path.join(HARNESS_DIR, "reset_db.sh")

if HARNESS_DIR not in sys.path:
    sys.path.insert(0, HARNESS_DIR)

from collect_fuzzer_crashes import (
    AMBIENT_ORACLE_ENV_KEYS,
    DBMS_TO_OCCAM_DIALECT,
    SERVER_DBMS,
    SUPPORTED_DBMS,
    atomic_write_json,
    normalize_dbms,
    resolve_engine_bin,
    run_replay_verification,
)
from dhall_config import (
    BUILTIN_PROFILES,
    DEFAULT_CONTAINER_PORTS,
    DEFAULT_IMAGES,
    load_experiment_plan,
    resolve_repo_binary,
)


def resolve_occam_binary(explicit_path=""):
    return resolve_repo_binary("occam", "OCCAM_BIN", explicit_path)


def resolve_harness_binary(explicit_path=""):
    return resolve_repo_binary("occam-harness", "OCCAM_HARNESS_BIN", explicit_path)


def parse_profiles(profile_names_csv, custom_specs=None):
    custom_map = {}
    for spec in custom_specs or []:
        if "=" not in spec:
            raise ValueError(f"Invalid --profile-spec '{spec}'; expected NAME=FLAGS")
        raw_name, raw_flags = spec.split("=", 1)
        name = raw_name.strip()
        if not name:
            raise ValueError(f"Empty profile name in --profile-spec '{spec}'")
        flags = shlex.split(raw_flags)
        custom_map[name.lower()] = (name, flags)

    profiles = []
    seen = set()
    for raw in profile_names_csv.split(","):
        token = raw.strip()
        if not token:
            continue
        low = token.lower()
        if low in custom_map:
            canon, flags = custom_map[low]
        elif low in BUILTIN_PROFILES:
            canon, flags = BUILTIN_PROFILES[low]
        else:
            valid = ", ".join(
                [v[0] for v in BUILTIN_PROFILES.values()] + sorted(custom_map)
            )
            raise ValueError(f"Unknown profile '{token}'. Valid profiles: {valid}")
        if canon not in seen:
            seen.add(canon)
            profiles.append({"name": canon, "flags": list(flags)})

    if not profiles:
        raise ValueError("At least one reduction profile must be specified")
    return profiles


def parse_image_maps(map_items):
    mapping = {}
    for item in map_items or []:
        if "=" not in item:
            raise ValueError(f"Invalid --image-map '{item}'; expected KEY=IMAGE")
        key, image = item.split("=", 1)
        key = key.strip()
        image = image.strip()
        if not key or not image:
            raise ValueError(f"Invalid --image-map '{item}'; expected non-empty KEY=IMAGE")
        mapping[key] = image
    return mapping


def parse_container_specs(spec_csv):
    specs = []
    for raw in spec_csv.split(","):
        item = raw.strip()
        if not item:
            continue
        host = "127.0.0.1"
        port = None
        if "@" in item:
            name, addr = item.split("@", 1)
            if ":" in addr:
                host, port_s = addr.rsplit(":", 1)
                port = int(port_s)
            else:
                host = addr
        elif ":" in item:
            name, port_s = item.rsplit(":", 1)
            port = int(port_s)
        else:
            name = item
        cname = name.strip()
        if not cname:
            raise ValueError(f"Empty container name in '{item}'")
        if port is not None and port <= 0:
            raise ValueError(f"Port must be positive in '{item}'")
        specs.append(
            {
                "container_name": cname,
                "host": host.strip() or "127.0.0.1",
                "port": port,
            }
        )
    if not specs:
        raise ValueError("Empty container specification")
    return specs


def _resolve_case_image(dbms, version, case_id, explicit_image, image_map, meta_image):
    if explicit_image:
        return explicit_image
    for key in (
        f"case:{case_id}",
        f"{dbms}@{version}" if version else "",
        dbms,
    ):
        if key and key in image_map:
            return image_map[key]
    if meta_image:
        return meta_image
    return DEFAULT_IMAGES[dbms]


def _resolve_case_id(mapping, fallback_stem, source_desc):
    if "id" in mapping and mapping["id"] is not None:
        raw_id = mapping["id"]
        if not isinstance(raw_id, str):
            raise ValueError(f"Invalid case id {raw_id!r} from {source_desc}")
        if not raw_id or not raw_id.strip() or raw_id != raw_id.strip():
            raise ValueError(f"Unsafe or empty case id {raw_id!r} from {source_desc}")
        return raw_id
    return fallback_stem


def _validate_case_id(case_id, source_desc, seen_ids):
    if (
        not case_id
        or not case_id.strip()
        or case_id != case_id.strip()
        or case_id in (".", "..")
        or "/" in case_id
        or "\\" in case_id
        or any(ord(ch) < 32 for ch in case_id)
    ):
        raise ValueError(f"Unsafe case id '{case_id}' from {source_desc}")
    if case_id in seen_ids:
        raise ValueError(f"Duplicate case id '{case_id}' encountered at {source_desc}")
    seen_ids.add(case_id)


def load_cases_from_corpus(corpus_dir, dbms_filter=None, case_filter=None, default_expected_sig=""):
    root = os.path.abspath(corpus_dir)
    if not os.path.isdir(root):
        raise FileNotFoundError(f"Corpus directory not found: {root}")

    cases = []
    seen_ids = set()
    for fname in sorted(os.listdir(root)):
        if not fname.endswith(".sql"):
            continue
        sql_path = os.path.join(root, fname)
        if not os.path.isfile(sql_path):
            continue
        stem = os.path.splitext(fname)[0]
        meta_path = os.path.join(root, f"{stem}.meta.json")
        meta = {}
        if os.path.isfile(meta_path):
            with open(meta_path, encoding="utf-8") as handle:
                meta = json.load(handle)
            if not isinstance(meta, dict):
                raise ValueError(f"Expected JSON object in '{meta_path}'")

        case_id = _resolve_case_id(meta, stem, meta_path if meta else sql_path)
        _validate_case_id(case_id, sql_path, seen_ids)
        if case_filter and case_id not in case_filter:
            continue

        target = meta.get("target") or {}
        if not isinstance(target, dict):
            raise TypeError(f"Invalid target section in '{meta_path}'")
        raw_dbms = target.get("dbms") or meta.get("dialect") or dbms_filter or ""
        dbms = normalize_dbms(raw_dbms)
        if not dbms:
            raise ValueError(
                f"Cannot infer DBMS for '{sql_path}'; pass --dbms or add target.dbms to .meta.json"
            )
        if dbms not in SUPPORTED_DBMS:
            raise ValueError(f"Unsupported DBMS '{dbms}' in '{sql_path}'")
        if dbms_filter and dbms != dbms_filter:
            continue

        dialect = DBMS_TO_OCCAM_DIALECT[dbms]
        sig_block = meta.get("crash_signature") or {}
        if not isinstance(sig_block, dict):
            sig_block = {}
        expected_sig = str(sig_block.get("expected_signature") or default_expected_sig or "")
        version = str(target.get("version") or "")
        meta_image = str(target.get("image") or "")

        with open(sql_path, "rb") as handle:
            sql_bytes = handle.read()

        cases.append(
            {
                "id": case_id,
                "dbms": dbms,
                "dialect": dialect,
                "version": version,
                "meta_image": meta_image,
                "file": sql_path,
                "byte_count": len(sql_bytes),
                "sql_sha256": hashlib.sha256(sql_bytes).hexdigest(),
                "expected_signature": expected_sig,
                "baseline_signature": None,
                "meta": meta,
            }
        )
    return cases


def load_cases_from_manifest(manifest_path, dbms_filter=None, case_filter=None, default_expected_sig=""):
    mpath = os.path.abspath(manifest_path)
    with open(mpath, encoding="utf-8") as handle:
        payload = json.load(handle)

    if isinstance(payload, dict):
        raw_cases = payload.get("cases", [])
    elif isinstance(payload, list):
        raw_cases = payload
    else:
        raise TypeError(f"Invalid manifest JSON root in '{mpath}'")
    if not isinstance(raw_cases, list):
        raise TypeError(f"Manifest 'cases' must be a list in '{mpath}'")

    manifest_dir = os.path.dirname(mpath)
    cases = []
    seen_ids = set()

    for idx, item in enumerate(raw_cases):
        if not isinstance(item, dict):
            raise TypeError(f"Manifest entry {idx} is not a JSON object in '{mpath}'")
        raw_file = item.get("file")
        if not isinstance(raw_file, str) or not raw_file.strip():
            raise ValueError(f"Manifest entry {idx} missing valid 'file' field in '{mpath}'")
        stem = os.path.splitext(os.path.basename(raw_file))[0]
        case_id = _resolve_case_id(item, stem, mpath)
        _validate_case_id(case_id, mpath, seen_ids)
        if case_filter and case_id not in case_filter:
            continue
        dbms = normalize_dbms(item.get("dbms") or dbms_filter or "")
        if dbms not in SUPPORTED_DBMS:
            raise ValueError(f"Unsupported or missing dbms for case '{case_id}': '{dbms}'")
        if dbms_filter and dbms != dbms_filter:
            continue

        file_path = raw_file
        if not os.path.isabs(file_path):
            file_path = os.path.abspath(os.path.join(manifest_dir, file_path))
        if not os.path.isfile(file_path):
            raise FileNotFoundError(f"Case SQL file missing: {file_path}")

        with open(file_path, "rb") as handle:
            sql_bytes = handle.read()

        baseline_sig = item.get("baseline_signature")
        if not baseline_sig and item.get("baseline_signature_id"):
            baseline_sig = {"id": item["baseline_signature_id"]}

        cases.append(
            {
                "id": case_id,
                "dbms": dbms,
                "dialect": item.get("dialect") or DBMS_TO_OCCAM_DIALECT[dbms],
                "version": str(item.get("version") or ""),
                "meta_image": str(item.get("image") or ""),
                "file": file_path,
                "byte_count": len(sql_bytes),
                "sql_sha256": hashlib.sha256(sql_bytes).hexdigest(),
                "expected_signature": str(
                    item.get("expected_signature") or default_expected_sig or ""
                ),
                "baseline_signature": baseline_sig,
                "meta": item,
            }
        )
    return cases


def load_cases_from_inputs(input_files, dbms, default_expected_sig=""):
    if not dbms:
        raise ValueError("--dbms is required when specifying raw --inputs SQL files")
    cases = []
    used = set()
    for raw_path in input_files:
        sql_path = os.path.abspath(raw_path)
        if not os.path.isfile(sql_path):
            raise FileNotFoundError(f"Input SQL file not found: {sql_path}")
        base_id = re.sub(
            r"[^A-Za-z0-9_.-]+",
            "_",
            os.path.splitext(os.path.basename(sql_path))[0],
        ).strip("._-")
        base_id = base_id or "case"
        case_id = base_id
        n = 2
        while case_id in used:
            case_id = f"{base_id}_{n}"
            n += 1
        used.add(case_id)

        with open(sql_path, "rb") as handle:
            sql_bytes = handle.read()

        cases.append(
            {
                "id": case_id,
                "dbms": dbms,
                "dialect": DBMS_TO_OCCAM_DIALECT[dbms],
                "version": "",
                "meta_image": "",
                "file": sql_path,
                "byte_count": len(sql_bytes),
                "sql_sha256": hashlib.sha256(sql_bytes).hexdigest(),
                "expected_signature": default_expected_sig,
                "baseline_signature": None,
                "meta": {},
            }
        )
    return cases


def summarize_decision_log(path):
    """Summarize oracle_decisions.jsonl using the typed Haskell occam-harness CLI."""
    if not path or not os.path.isfile(path):
        return {
            "total_calls": 0,
            "accepted_calls": 0,
            "confirmed_faults": 0,
            "healthy_no_fault": 0,
            "missing_evidence": 0,
            "slippage_events": 0,
            "unique_fault_signatures": [],
        }
    harness_bin = resolve_harness_binary()
    proc = subprocess.run(
        [harness_bin, "summarize-decisions", path],
        capture_output=True,
        text=True,
        check=False,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"occam-harness summarize-decisions failed ({proc.returncode}): {proc.stderr.strip()}"
        )
    return json.loads(proc.stdout)


class WorkerTarget:
    """Isolated DBMS execution target owned exclusively by one batch worker."""

    def __init__(
        self,
        worker_id,
        run_tag,
        container_prefix="occam-eval",
        external_container=None,
        local_embedded=False,
        engine_bin="",
        docker_network="",
        container_wait=30,
        keep_containers=False,
    ):
        self.worker_id = worker_id
        self.run_tag = run_tag
        self.container_prefix = container_prefix
        self.external_container = external_container
        self.local_embedded = local_embedded
        self.engine_bin = engine_bin
        self.docker_network = docker_network
        self.container_wait = container_wait
        self.keep_containers = keep_containers

        self.managed_container = None
        self.current_dbms = None
        self.current_image = None
        self.host = ""
        self.port = None
        self.dbname = ""
        self.container_name = ""

    def _run_docker(self, args, check=True, timeout=60):
        return subprocess.run(
            ["docker"] + args,
            capture_output=True,
            text=True,
            timeout=timeout,
            check=check,
        )

    def _resolve_container_port(self, cname, cport):
        proc = self._run_docker(["port", cname, f"{cport}/tcp"], check=False, timeout=15)
        if proc.returncode != 0:
            return None
        for line in (proc.stdout or "").splitlines():
            line = line.strip()
            if not line:
                continue
            # Formats: "127.0.0.1:49153" or "[::]:49153"
            m = re.search(r":(\d+)$", line)
            if m:
                return int(m.group(1))
        return None

    def _wait_ready(self, dbms, host, port, cname, dbname=""):
        cmd = [
            RESET_DB_PATH,
            "--dbms",
            dbms,
            "--max-wait",
            str(self.container_wait),
        ]
        if cname:
            cmd.extend(["--container-name", cname])
        if dbms in SERVER_DBMS:
            if host:
                cmd.extend(["--host", host])
            if port is not None:
                cmd.extend(["--port", str(port)])
            if dbname:
                cmd.extend(["--dbname", dbname])
        proc = subprocess.run(cmd, check=False, capture_output=True, text=True, timeout=self.container_wait + 15)
        if proc.returncode != 0:
            raise RuntimeError(
                f"Worker {self.worker_id} target readiness failed ({dbms}, container={cname}): "
                f"{(proc.stderr or proc.stdout or '').strip()}"
            )

    def ensure_target(self, dbms, image, force_recreate=False):
        if self.local_embedded:
            if dbms in SERVER_DBMS:
                raise ValueError(f"--local-embedded only supports sqlite/duckdb, got {dbms}")
            self.current_dbms = dbms
            self.current_image = "local-embedded"
            self.container_name = ""
            self.host = ""
            self.port = None
            self.dbname = ""
            return

        if self.external_container is not None:
            cname = self.external_container["container_name"]
            host = self.external_container["host"]
            port = self.external_container["port"]
            if dbms in SERVER_DBMS and port is None:
                port = self._resolve_container_port(cname, DEFAULT_CONTAINER_PORTS[dbms])
                if port is None:
                    port = DEFAULT_CONTAINER_PORTS[dbms]
            eff_host = host if dbms in SERVER_DBMS else ""
            eff_port = port if dbms in SERVER_DBMS else None
            need_wait = force_recreate or self.current_dbms != dbms
            if need_wait:
                self._wait_ready(dbms, eff_host, eff_port, cname)
            self.current_dbms = dbms
            self.current_image = "external"
            self.container_name = cname
            self.host = eff_host
            self.port = eff_port
            self.dbname = ""
            return

        if (
            not force_recreate
            and self.managed_container
            and self.current_dbms == dbms
            and self.current_image == image
        ):
            return

        self.cleanup(force_remove=True)

        cname = f"{self.container_prefix}-{self.run_tag}-w{self.worker_id}"
        # Remove any stale container left behind by an earlier killed run.
        self._run_docker(["rm", "-f", cname], check=False, timeout=20)

        run_args = ["run", "-d", "--name", cname]
        if self.docker_network:
            run_args.extend(["--network", self.docker_network])
        if dbms in SERVER_DBMS:
            cport = DEFAULT_CONTAINER_PORTS[dbms]
            run_args.extend(["-p", f"127.0.0.1::{cport}"])
        run_args.append(image)

        proc = self._run_docker(run_args, check=False, timeout=60)
        if proc.returncode != 0:
            raise RuntimeError(
                f"Worker {self.worker_id} failed to start container {cname} ({image}): "
                f"{(proc.stderr or proc.stdout or '').strip()}"
            )

        # Register managed_container early so cleanup() removes it if port/wait fails.
        self.managed_container = cname
        try:
            eff_host = ""
            eff_port = None
            eff_dbname = ""
            if dbms in SERVER_DBMS:
                cport = DEFAULT_CONTAINER_PORTS[dbms]
                resolved = None
                for _ in range(25):
                    resolved = self._resolve_container_port(cname, cport)
                    if resolved is not None:
                        break
                    time.sleep(0.2)
                if resolved is None:
                    raise RuntimeError(
                        f"Worker {self.worker_id} could not resolve mapped port for {cname}:{cport}"
                    )
                eff_host = "127.0.0.1"
                eff_port = resolved
                eff_dbname = "occam_test"

            self._wait_ready(dbms, eff_host, eff_port, cname, dbname=eff_dbname)
            self.container_name = cname
            self.current_dbms = dbms
            self.current_image = image
            self.host = eff_host
            self.port = eff_port
            self.dbname = eff_dbname
        except Exception:
            self.cleanup(force_remove=True)
            raise

    def _build_reset_argv(self, include_max_wait=False):
        cmd = [RESET_DB_PATH, "--dbms", self.current_dbms]
        if include_max_wait:
            cmd.extend(["--max-wait", str(self.container_wait)])
        if self.container_name:
            cmd.extend(["--container-name", self.container_name])
        if self.current_dbms in SERVER_DBMS:
            if self.host:
                cmd.extend(["--host", self.host])
            if self.port is not None:
                cmd.extend(["--port", str(self.port)])
            if self.dbname:
                cmd.extend(["--dbname", self.dbname])
        return cmd

    def reset(self):
        cmd = self._build_reset_argv(include_max_wait=True)
        proc = subprocess.run(cmd, check=False, capture_output=True, text=True, timeout=self.container_wait + 15)
        if proc.returncode != 0 and self.managed_container:
            # Recover by recreating the managed worker container once, then rebuild cmd with new port.
            self.ensure_target(self.current_dbms, self.current_image, force_recreate=True)
            cmd = self._build_reset_argv(include_max_wait=True)
            proc = subprocess.run(cmd, check=False, capture_output=True, text=True, timeout=self.container_wait + 15)
        return proc.returncode == 0, (proc.stderr or proc.stdout or "").strip()

    def build_oracle_cmd(
        self,
        expected_signature="",
        baseline_signature="",
        reject_on_slippage=False,
        stack_frames=5,
        decision_log="",
    ):
        parts = [
            sys.executable,
            CRASH_ORACLE_PATH,
            "--dbms",
            self.current_dbms,
            "--stack-frames",
            str(stack_frames),
        ]
        if self.container_name:
            parts.extend(["--container-name", self.container_name])
        if self.current_dbms in SERVER_DBMS:
            if self.host:
                parts.extend(["--host", self.host])
            if self.port is not None:
                parts.extend(["--port", str(self.port)])
            if self.dbname:
                parts.extend(["--dbname", self.dbname])
        elif self.local_embedded and self.engine_bin:
            parts.extend(["--engine-bin", resolve_engine_bin(self.engine_bin)])
        if expected_signature:
            parts.extend(["--expected-signature", expected_signature])
        if baseline_signature:
            parts.extend(["--baseline-signature", baseline_signature])
        if reject_on_slippage and baseline_signature:
            parts.append("--reject-on-slippage")
        if decision_log:
            parts.extend(["--decision-log", os.path.abspath(decision_log)])
        return " ".join(shlex.quote(p) for p in parts)

    def build_reset_cmd(self):
        return " ".join(shlex.quote(p) for p in self._build_reset_argv(include_max_wait=False))

    def cleanup(self, force_remove=False):
        if self.managed_container and (force_remove or not self.keep_containers):
            try:
                self._run_docker(["rm", "-f", self.managed_container], check=False, timeout=20)
            except Exception:
                pass
            self.managed_container = None
            self.current_dbms = None
            self.current_image = None
            self.host = ""
            self.port = None
            self.dbname = ""
            self.container_name = ""


def _profile_signature_hash(profile_flags):
    return hashlib.sha256(" ".join(profile_flags).encode("utf-8")).hexdigest()[:12]


def _run_config_hash(
    occam_jobs,
    timeout_secs,
    budget_secs,
    max_oracle_execs,
    no_cache,
    replays,
    reject_on_slippage,
    stack_frames,
    expected_signature,
):
    payload = json.dumps(
        {
            "occam_jobs": occam_jobs,
            "timeout_secs": float(timeout_secs),
            "budget_secs": float(budget_secs) if budget_secs is not None else None,
            "max_oracle_execs": max_oracle_execs,
            "no_cache": bool(no_cache),
            "replays": replays,
            "reject_on_slippage": bool(reject_on_slippage),
            "stack_frames": stack_frames,
            "expected_signature": expected_signature or "",
        },
        sort_keys=True,
    )
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()[:12]


def _is_valid_cached_qualification(qual_path, case, replays, stack_frames=None):
    if not os.path.isfile(qual_path):
        return None
    try:
        with open(qual_path, encoding="utf-8") as handle:
            data = json.load(handle)
        if not isinstance(data, dict):
            return None
        if data.get("case_id") != case["id"]:
            return None
        if data.get("sql_sha256") != case["sql_sha256"]:
            return None
        if data.get("replays_required") != replays:
            return None
        if (
            "expected_signature" in data
            and data.get("expected_signature") != case["expected_signature"]
        ):
            return None
        if (
            stack_frames is not None
            and "stack_frames" in data
            and data.get("stack_frames") != stack_frames
        ):
            return None
        return data
    except (OSError, json.JSONDecodeError):
        return None


def _is_valid_completed_job(job_result_path, job, rerun_failed=False, qual_record=None):
    if not os.path.isfile(job_result_path):
        return None
    try:
        with open(job_result_path, encoding="utf-8") as handle:
            data = json.load(handle)
        if not isinstance(data, dict):
            return None
        if data.get("job_id") != job["job_id"]:
            return None
        if data.get("sql_sha256") != job["case"]["sql_sha256"]:
            return None
        if data.get("profile_hash") != job["profile_hash"]:
            return None
        if (
            "run_config_hash" in data
            and "run_config_hash" in job
            and data.get("run_config_hash") != job.get("run_config_hash")
        ):
            return None
        state = data.get("state")
        if state == "skipped_unstable_input" and (
            job.get("allow_unstable_input")
            or job.get("skip_input_replay")
            or rerun_failed
            or (qual_record and qual_record.get("qualified"))
        ):
            return None
        if rerun_failed and state in ("error", "infra_error", "validation_failed"):
            return None
        if state in ("completed", "censored"):
            stats_path = data.get("stats_path")
            if stats_path and os.path.isfile(stats_path):
                with open(stats_path, encoding="utf-8") as sf:
                    json.load(sf)
            elif data.get("ran_occam"):
                return None
        return data
    except (OSError, json.JSONDecodeError):
        return None


def _clean_partial_dir(job_dir):
    os.makedirs(job_dir, exist_ok=True)
    for fname in os.listdir(job_dir):
        fpath = os.path.join(job_dir, fname)
        try:
            if os.path.isdir(fpath):
                shutil.rmtree(fpath)
            else:
                os.unlink(fpath)
        except OSError:
            pass


def _extract_sig_fields(baseline_sig, fallback_record=None):
    if isinstance(baseline_sig, dict):
        return (
            baseline_sig.get("id"),
            baseline_sig.get("hash"),
            baseline_sig.get("category"),
        )
    if isinstance(baseline_sig, str) and baseline_sig.strip():
        sig_id = baseline_sig.strip()
        return sig_id, None, (sig_id.split(":", 1)[0] if ":" in sig_id else None)
    fb = fallback_record or {}
    return (
        fb.get("baseline_signature_id"),
        fb.get("baseline_signature_hash"),
        fb.get("baseline_category"),
    )


def qualify_case_on_worker(
    worker,
    case,
    out_dir,
    replays,
    stack_frames,
    probe_timeout,
    resume,
    stop_event,
):
    if stop_event.is_set():
        return None

    qual_dir = os.path.join(out_dir, "qualified", case["id"])
    qual_path = os.path.join(qual_dir, "input_replay.json")

    if resume:
        cached = _is_valid_cached_qualification(
            qual_path, case, replays, stack_frames=stack_frames
        )
        if cached is not None:
            return cached

    if stop_event.is_set():
        return None

    os.makedirs(qual_dir, exist_ok=True)
    with open(case["file"], "rb") as handle:
        sql_bytes = handle.read()

    worker.ensure_target(case["dbms"], case["image"])
    decision_log = os.path.join(qual_dir, "input_replay_decisions.jsonl")
    try:
        if os.path.exists(decision_log):
            os.unlink(decision_log)
    except OSError:
        pass

    replay_res = run_replay_verification(
        sql_bytes=sql_bytes,
        dbms=case["dbms"],
        replays=replays,
        expected_signature=case["expected_signature"],
        container_name=worker.container_name,
        host=worker.host,
        port=worker.port,
        dbname=worker.dbname,
        engine_bin=worker.engine_bin if worker.local_embedded else "",
        decision_log=decision_log,
        stack_frames=stack_frames,
        probe_timeout=probe_timeout,
        stop_event=stop_event,
    )
    if replay_res.get("interrupted") or stop_event.is_set():
        return None

    qualified = replay_res["all_passed"] and replay_res["consistent_signature"]
    baseline_sig = replay_res.get("baseline_signature") or case.get("baseline_signature")
    sig_id, sig_hash, sig_cat = _extract_sig_fields(baseline_sig)
    record = {
        "case_id": case["id"],
        "dbms": case["dbms"],
        "sql_sha256": case["sql_sha256"],
        "replays_required": replays,
        "stack_frames": stack_frames,
        "expected_signature": case["expected_signature"],
        "qualified": qualified,
        "baseline_signature": baseline_sig,
        "baseline_signature_id": sig_id,
        "baseline_signature_hash": sig_hash,
        "baseline_category": sig_cat,
        "replay_verification": replay_res,
        "qualified_at": datetime.datetime.now(datetime.UTC).isoformat(),
    }
    atomic_write_json(qual_path, record)
    return record


def execute_job_on_worker(
    worker,
    job,
    qual_record,
    out_dir,
    occam_bin,
    occam_jobs,
    timeout_secs,
    budget_secs,
    max_oracle_execs,
    no_cache,
    replays,
    reject_on_slippage,
    stack_frames,
    probe_timeout,
    resume,
    rerun_failed,
    stop_event,
):
    if stop_event.is_set():
        return None

    case = job["case"]
    job_id = job["job_id"]
    job_dir = os.path.join(out_dir, "runs", job_id)
    job_result_path = os.path.join(job_dir, "job_result.json")

    if resume:
        cached = _is_valid_completed_job(
            job_result_path,
            job,
            rerun_failed=rerun_failed,
            qual_record=qual_record,
        )
        if cached is not None:
            return cached

    if stop_event.is_set():
        return None

    _clean_partial_dir(job_dir)

    baseline_sig = (qual_record or {}).get("baseline_signature") or case.get("baseline_signature")
    baseline_sig_id, baseline_sig_hash, baseline_cat = _extract_sig_fields(
        baseline_sig, fallback_record=qual_record
    )
    baseline_ref = baseline_sig_id or baseline_sig_hash or ""

    if qual_record and not qual_record.get("qualified") and not job["allow_unstable_input"]:
        result = {
            "job_id": job_id,
            "case_id": case["id"],
            "dbms": case["dbms"],
            "dialect": case["dialect"],
            "profile": job["profile_name"],
            "profile_hash": job["profile_hash"],
            "run_config_hash": job.get("run_config_hash"),
            "repeat": job["repeat"],
            "sql_sha256": case["sql_sha256"],
            "input_file": case["file"],
            "initial_bytes": case["byte_count"],
            "state": "skipped_unstable_input",
            "ran_occam": False,
            "input_qualified": False,
            "baseline_signature": baseline_sig,
            "baseline_signature_id": baseline_sig_id,
            "baseline_signature_hash": baseline_sig_hash,
            "baseline_category": baseline_cat,
            "stats_path": None,
            "minimal_path": None,
            "occam_returncode": None,
            "reduced_replay_verification": None,
            "oracle_decisions_summary": None,
            "finished_at": datetime.datetime.now(datetime.UTC).isoformat(),
        }
        atomic_write_json(job_result_path, result)
        return result

    worker.ensure_target(case["dbms"], case["image"])
    if stop_event.is_set():
        return None
    reset_ok, reset_msg = worker.reset()
    if stop_event.is_set():
        return None
    if not reset_ok:
        result = {
            "job_id": job_id,
            "case_id": case["id"],
            "dbms": case["dbms"],
            "dialect": case["dialect"],
            "profile": job["profile_name"],
            "profile_hash": job["profile_hash"],
            "run_config_hash": job.get("run_config_hash"),
            "repeat": job["repeat"],
            "sql_sha256": case["sql_sha256"],
            "input_file": case["file"],
            "initial_bytes": case["byte_count"],
            "state": "infra_error",
            "error": f"Pre-run reset failed: {reset_msg}",
            "ran_occam": False,
            "input_qualified": bool(qual_record.get("qualified")) if qual_record else True,
            "baseline_signature": baseline_sig,
            "baseline_signature_id": baseline_sig_id,
            "baseline_signature_hash": baseline_sig_hash,
            "baseline_category": baseline_cat,
            "stats_path": None,
            "minimal_path": None,
            "occam_returncode": None,
            "reduced_replay_verification": None,
            "oracle_decisions_summary": None,
            "finished_at": datetime.datetime.now(datetime.UTC).isoformat(),
        }
        atomic_write_json(job_result_path, result)
        return result

    minimal_path = os.path.join(job_dir, "minimal.sql")
    stats_path = os.path.join(job_dir, "stats.json")
    oracle_decisions_path = os.path.join(job_dir, "oracle_decisions.jsonl")
    replay_decisions_path = os.path.join(job_dir, "replay_decisions.jsonl")
    stderr_log_path = os.path.join(job_dir, "occam.stderr.log")

    oracle_cmd = worker.build_oracle_cmd(
        expected_signature=case["expected_signature"],
        baseline_signature=baseline_ref,
        reject_on_slippage=reject_on_slippage,
        stack_frames=stack_frames,
        decision_log=oracle_decisions_path,
    )
    reset_cmd = worker.build_reset_cmd()

    occam_cmd = [
        occam_bin,
        case["file"],
        "--oracle",
        oracle_cmd,
        "--reset",
        reset_cmd,
        "--dialect",
        case["dialect"],
        "--timeout",
        str(timeout_secs),
        "--jobs",
        str(occam_jobs),
        "--out",
        minimal_path,
        "--stats",
        stats_path,
    ]
    if budget_secs is not None:
        occam_cmd.extend(["--budget", str(budget_secs)])
    if max_oracle_execs is not None:
        occam_cmd.extend(["--max-oracle-executions", str(max_oracle_execs)])
    if no_cache:
        occam_cmd.append("--no-cache")
    occam_cmd.extend(job["profile_flags"])

    occam_env = os.environ.copy()
    for key in AMBIENT_ORACLE_ENV_KEYS:
        occam_env.pop(key, None)

    wall_start = time.monotonic()
    hard_timeout = None
    if budget_secs is not None:
        hard_timeout = budget_secs + max(60.0, timeout_secs * 4.0)

    proc = subprocess.Popen(
        occam_cmd,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        env=occam_env,
        start_new_session=True,
    )
    deadline = (wall_start + hard_timeout) if hard_timeout is not None else None
    interrupted = False
    while True:
        if stop_event.is_set():
            interrupted = True
            try:
                os.killpg(proc.pid, signal.SIGTERM)
            except OSError:
                pass
            try:
                _, stderr_bytes = proc.communicate(timeout=2.0)
            except subprocess.TimeoutExpired:
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except OSError:
                    pass
                _, stderr_bytes = proc.communicate()
            return None
        wait_slice = 0.5
        if deadline is not None:
            rem = deadline - time.monotonic()
            if rem <= 0:
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except OSError:
                    pass
                _, stderr_bytes = proc.communicate()
                proc_rc = 124
                break
            wait_slice = min(wait_slice, rem)
        try:
            _, stderr_bytes = proc.communicate(timeout=wait_slice)
            proc_rc = proc.returncode
            break
        except subprocess.TimeoutExpired:
            continue

    if interrupted:
        return None
    wall_seconds = time.monotonic() - wall_start

    stderr_text = (stderr_bytes or b"").decode("utf-8", errors="replace")
    with open(stderr_log_path, "w", encoding="utf-8") as handle:
        handle.write(stderr_text)

    stats_ok = False
    stats_data = None
    if os.path.isfile(stats_path):
        try:
            with open(stats_path, encoding="utf-8") as handle:
                stats_data = json.load(handle)
            stats_ok = isinstance(stats_data, dict)
        except (OSError, json.JSONDecodeError):
            stats_ok = False

    replay_res = None
    if replays > 0 and os.path.isfile(minimal_path) and os.path.getsize(minimal_path) > 0:
        with open(minimal_path, "rb") as handle:
            reduced_bytes = handle.read()
        # During post-reduction 5-replay verification we do NOT pass --reject-on-slippage,
        # so we observe the actual crash signature and record RQ3 slippage accurately.
        replay_res = run_replay_verification(
            sql_bytes=reduced_bytes,
            dbms=case["dbms"],
            replays=replays,
            expected_signature=case["expected_signature"],
            container_name=worker.container_name,
            host=worker.host,
            port=worker.port,
            dbname=worker.dbname,
            engine_bin=worker.engine_bin if worker.local_embedded else "",
            decision_log=replay_decisions_path,
            stack_frames=stack_frames,
            probe_timeout=probe_timeout,
            baseline_signature=baseline_ref,
            stop_event=stop_event,
        )
        if replay_res.get("interrupted") or stop_event.is_set():
            return None
    if stop_event.is_set():
        return None

    eval_status = stats_data.get("status") if stats_data else None
    val_passed = (
        (stats_data.get("evaluation_metrics") or {}).get("evaluation_validation_passed", True)
        if stats_ok
        else False
    )
    if stats_ok and eval_status == "completed" and proc_rc == 0 and bool(val_passed):
        state = "completed"
    elif stats_ok and eval_status in ("timeout", "execution-budget"):
        state = "censored"
    elif stats_ok and (
        eval_status in ("validation-failed", "validation-incomplete")
        or (eval_status == "completed" and not val_passed)
    ):
        state = "validation_failed"
    else:
        state = "error"

    decision_summary = summarize_decision_log(oracle_decisions_path)

    result = {
        "job_id": job_id,
        "case_id": case["id"],
        "dbms": case["dbms"],
        "dialect": case["dialect"],
        "profile": job["profile_name"],
        "profile_hash": job["profile_hash"],
        "run_config_hash": job.get("run_config_hash"),
        "repeat": job["repeat"],
        "sql_sha256": case["sql_sha256"],
        "input_file": case["file"],
        "initial_bytes": case["byte_count"],
        "state": state,
        "ran_occam": True,
        "occam_returncode": proc_rc,
        "eval_status": eval_status,
        "wall_seconds": round(wall_seconds, 4),
        "worker_id": worker.worker_id,
        "worker_container": worker.container_name or "local-embedded",
        "input_qualified": bool(qual_record.get("qualified")) if qual_record else True,
        "baseline_signature": baseline_sig,
        "baseline_signature_id": baseline_sig_id,
        "baseline_signature_hash": baseline_sig_hash,
        "baseline_category": baseline_cat,
        "stats_path": stats_path if stats_ok else None,
        "minimal_path": minimal_path if os.path.isfile(minimal_path) else None,
        "stderr_log_path": stderr_log_path,
        "oracle_decisions_summary": decision_summary,
        "reduced_replay_verification": replay_res,
        "finished_at": datetime.datetime.now(datetime.UTC).isoformat(),
    }
    atomic_write_json(job_result_path, result)
    return result


def aggregate_out_dir(out_dir):
    """Aggregate batch results and render summary.md using typed Haskell occam-harness."""
    out_abs = os.path.abspath(out_dir)
    harness_bin = resolve_harness_binary()
    proc = subprocess.run(
        [harness_bin, "aggregate", "--print-json", out_abs],
        capture_output=True,
        text=True,
        check=False,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"occam-harness aggregate failed ({proc.returncode}): {proc.stderr.strip()}"
        )
    aggregated = json.loads(proc.stdout)
    return (
        aggregated,
        os.path.join(out_abs, "aggregated_stats.json"),
        os.path.join(out_abs, "summary.md"),
    )


def main():
    parser = argparse.ArgumentParser(
        description="Resumable container-per-worker evaluation orchestrator for Occam."
    )
    src_group = parser.add_mutually_exclusive_group(required=False)
    src_group.add_argument(
        "--corpus-dir",
        help="Directory containing .sql and optional .meta.json files (e.g. benchmarks/real_fuzz_corpus)",
    )
    src_group.add_argument(
        "--manifest",
        help="Path to manifest.json (from collect_fuzzer_crashes.py)",
    )
    src_group.add_argument(
        "--inputs",
        nargs="+",
        help="Explicit list of input .sql files (requires --dbms)",
    )

    parser.add_argument(
        "--cases",
        default="",
        help="Comma-separated subset of case IDs to execute",
    )
    parser.add_argument(
        "--dbms",
        default="",
        help="Filter corpus/manifest by DBMS, or required default DBMS for --inputs",
    )
    parser.add_argument(
        "--expected-signature",
        default="",
        help="Default expected crash signature regex for cases without metadata",
    )
    parser.add_argument(
        "--plan",
        default="",
        help=(
            "Experiment plan name ('smoke', 'stage2-thesis') or path to a .json/.dhall "
            "ExperimentPlan file (CLI flags override plan defaults)"
        ),
    )
    parser.add_argument(
        "--profiles",
        default=None,
        help=(
            "Comma-separated reduction profiles: audit-only,ddmin-only,Ours-S,Ours-SI,Ours-Full,Ours-NoDep "
            "(default: Ours-S)"
        ),
    )
    parser.add_argument(
        "--profile-spec",
        action="append",
        default=[],
        help="Define custom profile flags as NAME=\"--flag1 --flag2\"",
    )
    parser.add_argument(
        "--repeats",
        type=int,
        default=None,
        help="Repetitions per (case, profile) pair (default: 1)",
    )
    parser.add_argument(
        "-j",
        "--jobs",
        type=int,
        default=None,
        help="Horizontal worker count (1 isolated container per worker, default: 1)",
    )
    parser.add_argument(
        "--occam-jobs",
        type=int,
        default=None,
        help="--jobs passed to each occam invocation (default: 1; server DBMSes require 1 unless --allow-unsafe-occam-jobs)",
    )
    parser.add_argument(
        "--allow-unsafe-occam-jobs",
        action="store_true",
        help="Allow --occam-jobs > 1 on server DBMSes sharing one database per worker",
    )
    parser.add_argument(
        "--occam-bin",
        default="",
        help="Path to occam executable (auto-detected if omitted)",
    )

    target_group = parser.add_mutually_exclusive_group(required=False)
    target_group.add_argument(
        "--containers",
        default=None,
        help="Comma-separated pre-running target container names (or name@host:port)",
    )
    target_group.add_argument(
        "--local-embedded",
        action="store_true",
        help="Run sqlite/duckdb directly on local binaries without Docker containers",
    )

    parser.add_argument(
        "--image",
        default="",
        help="Override Docker image for managed worker containers",
    )
    parser.add_argument(
        "--image-map",
        action="append",
        default=[],
        help="Map dbms|dbms@version|case:id=IMAGE for managed worker containers",
    )
    parser.add_argument(
        "--engine-bin",
        default="",
        help="Local sqlite3/duckdb binary path when using --local-embedded",
    )
    parser.add_argument(
        "--container-prefix",
        default="occam-eval",
        help="Prefix for managed worker containers (default: occam-eval)",
    )
    parser.add_argument(
        "--docker-network",
        default="",
        help="Optional Docker network passed to managed worker containers",
    )
    parser.add_argument(
        "--keep-containers",
        action="store_true",
        help="Keep managed worker containers after batch completion",
    )
    parser.add_argument(
        "--container-wait",
        type=int,
        default=None,
        help="Max seconds waiting for container readiness/reset (default: 30)",
    )

    parser.add_argument(
        "--timeout",
        type=float,
        default=None,
        help="Per-probe --timeout in seconds passed to occam (default: 10.0)",
    )
    parser.add_argument(
        "--budget",
        type=float,
        default=None,
        help="Optional per-run --budget in seconds passed to occam",
    )
    parser.add_argument(
        "--max-oracle-executions",
        type=int,
        default=None,
        help="Optional --max-oracle-executions cap passed to occam",
    )
    parser.add_argument(
        "--no-cache",
        action="store_true",
        default=None,
        help="Pass --no-cache to occam",
    )
    parser.add_argument(
        "--replays",
        type=int,
        default=5,
        help="Clean-state stability verification replays (default: 5; 0 disables)",
    )
    parser.add_argument(
        "--skip-input-replay",
        action="store_true",
        help="Skip Phase 1 input 5-replay qualification",
    )
    parser.add_argument(
        "--allow-unstable-input",
        action="store_true",
        help="Proceed to reduction even if input fails Phase 1 replay stability",
    )
    parser.add_argument(
        "--reject-on-slippage",
        action="store_true",
        default=None,
        help="Pass --reject-on-slippage to crash_oracle.py during reduction",
    )
    parser.add_argument(
        "--stack-frames",
        type=int,
        default=None,
        help="Stack frames retained in normalized RQ3 crash signatures (default: 5)",
    )
    parser.add_argument(
        "--probe-timeout",
        type=float,
        default=30.0,
        help="Standalone replay probe timeout in seconds (default: 30)",
    )

    parser.add_argument(
        "-o",
        "--out-dir",
        default="occam-out/batch_eval",
        help="Output workspace directory for checkpoints, stats.json, and aggregated reports",
    )
    parser.add_argument(
        "--no-resume",
        action="store_true",
        help="Re-run all jobs instead of skipping valid completed checkpoints",
    )
    parser.add_argument(
        "--rerun-failed",
        action="store_true",
        help="With resume enabled, retry jobs that previously failed/errored",
    )
    parser.add_argument(
        "--aggregate-only",
        action="store_true",
        help="Only regenerate aggregated_stats.json and summary.md from existing out-dir",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Build execution plan and write plan.json without running Docker or occam",
    )

    args = parser.parse_args()

    plan_cfg = {}
    if args.plan:
        try:
            plan_cfg = load_experiment_plan(args.plan)
        except (ValueError, OSError, RuntimeError, json.JSONDecodeError) as exc:
            parser.error(f"failed to load --plan '{args.plan}': {exc}")
    plan_oracle = plan_cfg.get("oracle") if isinstance(plan_cfg.get("oracle"), dict) else {}

    if args.profiles is None:
        raw_plan_profiles = plan_cfg.get("profiles")
        args.profiles = (
            ",".join(raw_plan_profiles)
            if isinstance(raw_plan_profiles, list) and raw_plan_profiles
            else "Ours-S"
        )
    if args.repeats is None:
        args.repeats = int(plan_cfg.get("repeats", 1))
    if args.jobs is None:
        args.jobs = int(plan_cfg.get("worker_jobs", 1))
    if args.occam_jobs is None:
        args.occam_jobs = int(plan_cfg.get("occam_jobs", 1))
    if args.container_wait is None:
        args.container_wait = int(plan_cfg.get("container_wait_secs", 30))
    if args.timeout is None:
        args.timeout = float(plan_cfg.get("timeout_secs", 10.0))
    if args.budget is None and plan_cfg.get("budget_secs") is not None:
        args.budget = float(plan_cfg["budget_secs"])
    if (
        args.max_oracle_executions is None
        and plan_cfg.get("max_oracle_executions") is not None
    ):
        args.max_oracle_executions = int(plan_cfg["max_oracle_executions"])
    if args.no_cache is None:
        args.no_cache = bool(plan_cfg.get("no_cache", False))
    if args.reject_on_slippage is None:
        args.reject_on_slippage = bool(plan_oracle.get("reject_on_slippage", False))
    if args.stack_frames is None:
        args.stack_frames = int(plan_oracle.get("stack_frames", 5))

    out_dir = os.path.abspath(args.out_dir)
    os.makedirs(out_dir, exist_ok=True)

    if args.aggregate_only:
        agg, agg_path, md_path = aggregate_out_dir(out_dir)
        print(
            json.dumps(
                {
                    "aggregated_stats": agg_path,
                    "summary_md": md_path,
                    "overview": agg["overview"],
                },
                indent=2,
            )
        )
        return 0

    if not (args.corpus_dir or args.manifest or args.inputs):
        parser.error("specify one of --corpus-dir, --manifest, --inputs, or --aggregate-only")

    if args.jobs <= 0:
        parser.error("--jobs must be a positive integer")
    if args.occam_jobs <= 0:
        parser.error("--occam-jobs must be a positive integer")
    if args.repeats <= 0:
        parser.error("--repeats must be a positive integer")
    if args.replays < 0:
        parser.error("--replays must be non-negative")
    if args.stack_frames <= 0:
        parser.error("--stack-frames must be a positive integer")
    if not math.isfinite(args.timeout) or args.timeout <= 0:
        parser.error("--timeout must be finite and positive")
    if args.budget is not None and (not math.isfinite(args.budget) or args.budget <= 0):
        parser.error("--budget must be finite and positive")
    if not math.isfinite(args.probe_timeout) or args.probe_timeout <= 0:
        parser.error("--probe-timeout must be finite and positive")
    if args.container_wait <= 0:
        parser.error("--container-wait must be a positive integer")
    if args.max_oracle_executions is not None and args.max_oracle_executions <= 0:
        parser.error("--max-oracle-executions must be positive")

    dbms_filter = normalize_dbms(args.dbms) if args.dbms else ""
    if dbms_filter and dbms_filter not in SUPPORTED_DBMS:
        parser.error(f"unsupported --dbms: {args.dbms}")

    case_filter = {c.strip() for c in args.cases.split(",") if c.strip()} if args.cases else None

    try:
        profiles = parse_profiles(args.profiles, args.profile_spec)
        image_map = parse_image_maps(args.image_map)
    except ValueError as exc:
        parser.error(str(exc))

    try:
        if args.corpus_dir:
            cases = load_cases_from_corpus(
                args.corpus_dir,
                dbms_filter=dbms_filter or None,
                case_filter=case_filter,
                default_expected_sig=args.expected_signature,
            )
        elif args.manifest:
            cases = load_cases_from_manifest(
                args.manifest,
                dbms_filter=dbms_filter or None,
                case_filter=case_filter,
                default_expected_sig=args.expected_signature,
            )
        else:
            cases = load_cases_from_inputs(
                args.inputs,
                dbms=dbms_filter,
                default_expected_sig=args.expected_signature,
            )
    except (ValueError, OSError, KeyError, TypeError) as exc:
        parser.error(str(exc))

    if not cases:
        print("Error: no matching cases found.", file=sys.stderr)
        return 1

    for case in cases:
        case["image"] = _resolve_case_image(
            case["dbms"],
            case.get("version", ""),
            case["id"],
            args.image,
            image_map,
            case.get("meta_image", ""),
        )

    has_server_dbms = any(c["dbms"] in SERVER_DBMS for c in cases)
    if args.local_embedded and has_server_dbms:
        server_ids = [c["id"] for c in cases if c["dbms"] in SERVER_DBMS]
        parser.error(
            f"--local-embedded supports sqlite/duckdb only, incompatible with server cases: {server_ids}"
        )

    if args.occam_jobs > 1 and has_server_dbms and not args.allow_unsafe_occam_jobs:
        parser.error(
            "--occam-jobs > 1 is unsafe for server DBMSes sharing occam_test per container; "
            "scale horizontal workers with --jobs instead (or pass --allow-unsafe-occam-jobs)."
        )

    try:
        external_containers = parse_container_specs(args.containers) if args.containers is not None else []
    except ValueError as exc:
        parser.error(f"invalid --containers specification: {exc}")
    if external_containers and has_server_dbms and args.jobs > len(external_containers):
        parser.error(
            f"--jobs {args.jobs} requires {args.jobs} isolated containers for server DBMSes, "
            f"but only {len(external_containers)} --containers were provided."
        )

    # Sort cases and jobs by (dbms, image, id) so managed worker containers minimize swaps.
    cases.sort(key=lambda c: (c["dbms"], c["image"], c["id"]))
    jobs = []
    for case in cases:
        case_run_cfg_hash = _run_config_hash(
            occam_jobs=args.occam_jobs,
            timeout_secs=args.timeout,
            budget_secs=args.budget,
            max_oracle_execs=args.max_oracle_executions,
            no_cache=args.no_cache,
            replays=args.replays,
            reject_on_slippage=args.reject_on_slippage,
            stack_frames=args.stack_frames,
            expected_signature=case["expected_signature"],
        )
        for prof in profiles:
            prof_hash = _profile_signature_hash(prof["flags"])
            for rep in range(1, args.repeats + 1):
                job_id = f"{case['id']}__{prof['name']}__r{rep}"
                jobs.append(
                    {
                        "job_id": job_id,
                        "case": case,
                        "profile_name": prof["name"],
                        "profile_flags": prof["flags"],
                        "profile_hash": prof_hash,
                        "run_config_hash": case_run_cfg_hash,
                        "repeat": rep,
                        "allow_unstable_input": args.allow_unstable_input,
                        "skip_input_replay": args.skip_input_replay,
                    }
                )

    plan = {
        "schema_version": 1,
        "created_at": datetime.datetime.now(datetime.UTC).isoformat(),
        "out_dir": out_dir,
        "jobs_workers": args.jobs,
        "occam_jobs": args.occam_jobs,
        "replays": args.replays,
        "skip_input_replay": args.skip_input_replay,
        "reject_on_slippage": args.reject_on_slippage,
        "resume": not args.no_resume,
        "rerun_failed": args.rerun_failed,
        "profiles": profiles,
        "case_count": len(cases),
        "total_jobs": len(jobs),
        "cases": [
            {
                "id": c["id"],
                "dbms": c["dbms"],
                "dialect": c["dialect"],
                "image": c["image"],
                "file": c["file"],
                "byte_count": c["byte_count"],
                "expected_signature": c["expected_signature"],
            }
            for c in cases
        ],
        "job_ids": [j["job_id"] for j in jobs],
    }
    plan_path = os.path.join(out_dir, "plan.json")
    atomic_write_json(plan_path, plan)

    if args.dry_run:
        print(json.dumps({"dry_run": True, "plan_path": plan_path, **{k: plan[k] for k in ("case_count", "total_jobs", "jobs_workers", "occam_jobs")}}, indent=2))
        return 0

    try:
        occam_bin = resolve_occam_binary(args.occam_bin)
    except FileNotFoundError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 2

    run_tag = hashlib.sha256(out_dir.encode("utf-8")).hexdigest()[:6]
    workers = []
    for wid in range(args.jobs):
        ext = None
        if external_containers:
            ext = external_containers[wid % len(external_containers)]
        workers.append(
            WorkerTarget(
                worker_id=wid,
                run_tag=run_tag,
                container_prefix=args.container_prefix,
                external_container=ext,
                local_embedded=args.local_embedded,
                engine_bin=args.engine_bin,
                docker_network=args.docker_network,
                container_wait=args.container_wait,
                keep_containers=args.keep_containers,
            )
        )

    stop_event = threading.Event()
    worker_lock = threading.Lock()
    free_workers = list(workers)
    worker_avail = threading.Condition(worker_lock)

    def acquire_worker():
        with worker_avail:
            while not free_workers and not stop_event.is_set():
                worker_avail.wait(timeout=0.25)
            if stop_event.is_set():
                return None
            return free_workers.pop(0)

    def release_worker(w):
        with worker_avail:
            free_workers.append(w)
            worker_avail.notify()

    is_main_thread = threading.current_thread() is threading.main_thread()
    prev_sigint = signal.getsignal(signal.SIGINT) if is_main_thread else None
    prev_sigterm = signal.getsignal(signal.SIGTERM) if is_main_thread else None

    def handle_signal(signum, _frame):
        stop_event.set()
        try:
            sys.stderr.write(
                f"\n[batch_eval] Signal {signum} received; draining active workers and cleaning containers...\n"
            )
            sys.stderr.flush()
        except Exception:
            pass

    if is_main_thread:
        signal.signal(signal.SIGINT, handle_signal)
        signal.signal(signal.SIGTERM, handle_signal)

    qualifications = {}
    resume = not args.no_resume
    errors = []

    qual_errors = set()
    try:
        # Phase 1: Input 5-replay stability qualification (per unique case)
        if not args.skip_input_replay and args.replays > 0:
            def qual_task(case):
                w = acquire_worker()
                if w is None:
                    return None
                try:
                    return qualify_case_on_worker(
                        worker=w,
                        case=case,
                        out_dir=out_dir,
                        replays=args.replays,
                        stack_frames=args.stack_frames,
                        probe_timeout=args.probe_timeout,
                        resume=resume,
                        stop_event=stop_event,
                    )
                finally:
                    release_worker(w)

            with ThreadPoolExecutor(max_workers=args.jobs) as pool:
                futs = {pool.submit(qual_task, c): c for c in cases}
                for fut in as_completed(futs):
                    case = futs[fut]
                    try:
                        res = fut.result()
                        if res is not None:
                            qualifications[case["id"]] = res
                    except Exception as exc:
                        qual_errors.add(case["id"])
                        if not stop_event.is_set():
                            errors.append(f"qualify {case['id']}: {exc}")
                            print(f"[batch_eval] Qualification error on {case['id']}: {exc}", file=sys.stderr)

        if not stop_event.is_set():
            # Phase 2: Resumable reduction + post-reduction 5-replay verification
            phase2_jobs = [j for j in jobs if j["case"]["id"] not in qual_errors]
            def run_task(job):
                w = acquire_worker()
                if w is None:
                    return None
                try:
                    qrec = qualifications.get(job["case"]["id"])
                    return execute_job_on_worker(
                        worker=w,
                        job=job,
                        qual_record=qrec,
                        out_dir=out_dir,
                        occam_bin=occam_bin,
                        occam_jobs=args.occam_jobs,
                        timeout_secs=args.timeout,
                        budget_secs=args.budget,
                        max_oracle_execs=args.max_oracle_executions,
                        no_cache=args.no_cache,
                        replays=args.replays,
                        reject_on_slippage=args.reject_on_slippage,
                        stack_frames=args.stack_frames,
                        probe_timeout=args.probe_timeout,
                        resume=resume,
                        rerun_failed=args.rerun_failed,
                        stop_event=stop_event,
                    )
                finally:
                    release_worker(w)

            with ThreadPoolExecutor(max_workers=args.jobs) as pool:
                futs = {pool.submit(run_task, j): j for j in phase2_jobs}
                for fut in as_completed(futs):
                    job = futs[fut]
                    try:
                        fut.result()
                    except Exception as exc:
                        if not stop_event.is_set():
                            errors.append(f"job {job['job_id']}: {exc}")
                            print(f"[batch_eval] Job error on {job['job_id']}: {exc}", file=sys.stderr)

    finally:
        if is_main_thread:
            signal.signal(signal.SIGINT, prev_sigint)
            signal.signal(signal.SIGTERM, prev_sigterm)
        for w in workers:
            w.cleanup()

    agg, agg_path, md_path = aggregate_out_dir(out_dir)
    print(
        json.dumps(
            {
                "aggregated_stats": agg_path,
                "summary_md": md_path,
                "interrupted": stop_event.is_set(),
                "worker_exceptions": len(errors),
                "overview": agg["overview"],
            },
            indent=2,
        )
    )
    if stop_event.is_set():
        return 130
    if errors:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())

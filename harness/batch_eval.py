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
import statistics
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
    _atomic_write_bytes,
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
)


def resolve_occam_binary(explicit_path=""):
    if explicit_path:
        path = os.path.abspath(os.path.expanduser(explicit_path))
        if not os.path.isfile(path) or not os.access(path, os.X_OK):
            raise FileNotFoundError(f"occam binary is not executable: {path}")
        return path

    env_bin = os.environ.get("OCCAM_BIN", "").strip()
    if env_bin:
        path = os.path.abspath(os.path.expanduser(env_bin))
        if os.path.isfile(path) and os.access(path, os.X_OK):
            return path

    which_bin = shutil.which("occam")
    if which_bin:
        return which_bin

    dist_dir = os.path.join(REPO_ROOT, "dist-newstyle")
    if os.path.isdir(dist_dir):
        matches = []
        for dirpath, _, filenames in os.walk(dist_dir):
            if "occam" in filenames:
                cand = os.path.join(dirpath, "occam")
                if os.path.isfile(cand) and os.access(cand, os.X_OK):  # noqa: SIM102
                    # Prefer build/occam/occam over tmp artifacts
                    if os.path.basename(dirpath) == "occam":
                        matches.append((os.path.getmtime(cand), cand))
        if matches:
            matches.sort(reverse=True)
            return matches[0][1]

    raise FileNotFoundError(
        "Could not locate 'occam' binary. Pass --occam-bin PATH, set OCCAM_BIN, or build with 'cabal build'."
    )


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


def compute_attempt_auc(initial_bytes, curve, total_attempts):
    """Exact port of Occam.Benchmark.Metrics.computeAttemptAuc."""
    if initial_bytes <= 0 or total_attempts <= 0:
        return 1.0
    best_at = {}
    for pt in curve or []:
        if not pt.get("point_accepted"):
            continue
        idx = int(pt.get("point_call_index", 0))
        c_bytes = int(pt.get("point_candidate_bytes", initial_bytes))
        if idx not in best_at or c_bytes < best_at[idx]:
            best_at[idx] = c_bytes

    cur_best = initial_bytes
    area = 0.0
    for i in range(1, total_attempts + 1):
        if i in best_at and best_at[i] < cur_best:
            cur_best = best_at[i]
        area += cur_best / float(initial_bytes)
    return area / float(total_attempts)


def compute_engine_overhead_us(cost_attribution):
    """Exact port of Occam.Benchmark.Metrics.engineOverheadMicros."""
    if not isinstance(cost_attribution, dict):
        return None
    ffi = sum(int(v) for v in (cost_attribution.get("snapshot_parse_ffi_micros") or {}).values())
    decode = sum(
        int(v) for v in (cost_attribution.get("snapshot_parse_decode_micros") or {}).values()
    )
    cand_build = sum(
        int((entry.get("counters") or {}).get("candidate_build_micros", 0))
        for entry in (cost_attribution.get("snapshot_families") or [])
        if isinstance(entry, dict)
    )
    return ffi + decode + cand_build


def summarize_decision_log(path):
    summary = {
        "total_calls": 0,
        "accepted_calls": 0,
        "confirmed_faults": 0,
        "healthy_no_fault": 0,
        "missing_evidence": 0,
        "slippage_events": 0,
        "unique_fault_signatures": [],
    }
    if not path or not os.path.isfile(path):
        return summary
    seen_sigs = []
    seen_set = set()
    try:
        with open(path, encoding="utf-8") as handle:
            for line in handle:
                line = line.strip()
                if not line:
                    continue
                try:
                    rec = json.loads(line)
                except json.JSONDecodeError:
                    continue
                summary["total_calls"] += 1
                if rec.get("verdict") == "accepted":
                    summary["accepted_calls"] += 1
                cls = rec.get("classification")
                if cls == "confirmed_fault":
                    summary["confirmed_faults"] += 1
                elif cls == "healthy_no_fault":
                    summary["healthy_no_fault"] += 1
                elif cls == "missing_evidence":
                    summary["missing_evidence"] += 1
                slip = rec.get("slippage")
                if isinstance(slip, dict) and slip.get("slipped") is True:
                    summary["slippage_events"] += 1
                csig = rec.get("crash_signature")
                if isinstance(csig, dict) and csig.get("id"):
                    sid = csig["id"]
                    if sid not in seen_set:
                        seen_set.add(sid)
                        seen_sigs.append(sid)
    except OSError:
        pass
    summary["unique_fault_signatures"] = seen_sigs
    return summary


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


def _metric_summary(values):
    clean = [float(v) for v in values if v is not None]
    if not clean:
        return None
    clean.sort()
    return {
        "n": len(clean),
        "median": statistics.median(clean),
        "mean": sum(clean) / len(clean),
        "min": clean[0],
        "max": clean[-1],
    }


def _collect_run_record(job_res):
    stats_path = job_res.get("stats_path")
    stats = None
    if stats_path and os.path.isfile(stats_path):
        try:
            with open(stats_path, encoding="utf-8") as handle:
                stats = json.load(handle)
        except (OSError, json.JSONDecodeError):
            stats = None

    em = (stats or {}).get("evaluation_metrics") or {}
    eval_status = (stats or {}).get("status") or job_res.get("eval_status") or job_res.get("state")

    init_bytes = (stats or {}).get("initial_bytes", job_res.get("initial_bytes"))
    red_bytes = (stats or {}).get("reduced_bytes")
    red_ratio = (stats or {}).get("reduction_ratio")
    init_stmts = (stats or {}).get("initial_statements")
    red_stmts = (stats or {}).get("reduced_statements")
    stmt_ratio = None
    if init_stmts is not None and red_stmts is not None and init_stmts > 0:
        stmt_ratio = ((init_stmts - red_stmts) / float(init_stmts)) * 100.0

    attempts = (stats or {}).get("oracle_attempts")
    curve = em.get("evaluation_curve") or []
    peak_idx = max([int(p.get("point_call_index", 0)) for p in curve], default=0)
    total_auc_calls = max(int(attempts or 0), peak_idx)
    attempt_auc = (
        compute_attempt_auc(int(init_bytes), curve, total_auc_calls)
        if stats is not None and init_bytes
        else None
    )
    overhead_us = compute_engine_overhead_us(em.get("evaluation_cost_attribution"))

    replay = job_res.get("reduced_replay_verification") or {}
    has_replay = bool(replay and replay.get("runs_required", 0) > 0)
    replay_all_passed = bool(replay.get("all_passed")) if has_replay else None
    replay_consistent = bool(replay.get("consistent_signature")) if has_replay else None
    replay_matches_base = bool(replay.get("matches_baseline")) if has_replay else None
    replay_slip_count = replay.get("slippage_count") if has_replay else None

    is_completed = (
        eval_status == "completed" and bool(em.get("evaluation_validation_passed"))
    )
    is_censored = eval_status in ("timeout", "execution-budget")

    dbms = job_res.get("dbms") or "unknown"
    sig_id = job_res.get("baseline_signature_id")
    strat_sig = (
        f"{dbms}::{sig_id}"
        if sig_id
        else f"{dbms}::unqualified:{job_res.get('case_id')}"
    )

    return {
        "job_id": job_res.get("job_id"),
        "case_id": job_res.get("case_id"),
        "dbms": job_res.get("dbms"),
        "dialect": job_res.get("dialect"),
        "profile": job_res.get("profile"),
        "repeat": job_res.get("repeat"),
        "job_state": job_res.get("state"),
        "status": eval_status,
        "is_completed": is_completed,
        "is_censored": is_censored,
        "input_qualified": job_res.get("input_qualified"),
        "baseline_signature_id": sig_id,
        "baseline_signature_hash": job_res.get("baseline_signature_hash"),
        "baseline_category": job_res.get("baseline_category"),
        "stratification_signature": strat_sig,
        "initial_bytes": init_bytes,
        "reduced_bytes": red_bytes,
        "reduction_ratio": red_ratio,
        "initial_statements": init_stmts,
        "reduced_statements": red_stmts,
        "statement_reduction_ratio": stmt_ratio,
        "oracle_executions": (stats or {}).get("oracle_executions"),
        "oracle_attempts": attempts,
        "cache_hits": (stats or {}).get("cache_hits"),
        "cache_misses": (stats or {}).get("cache_misses"),
        "cache_joins": (stats or {}).get("cache_joins"),
        "cache_hit_ratio": (stats or {}).get("cache_hit_ratio"),
        "cancelled_observations": (stats or {}).get("cancelled_observations"),
        "elapsed_seconds": (stats or {}).get("elapsed_seconds"),
        "oracle_seconds": em.get("evaluation_oracle_seconds"),
        "reset_seconds": em.get("evaluation_reset_seconds"),
        "audit_seconds": em.get("evaluation_audit_seconds"),
        "validation_seconds": em.get("evaluation_validation_seconds"),
        "validation_passed": em.get("evaluation_validation_passed"),
        "minimality_claim": em.get("evaluation_minimality_claim"),
        "attempt_auc": attempt_auc,
        "engine_overhead_us": overhead_us,
        "evaluation_jobs": ((stats or {}).get("evaluation_budget") or {}).get(
            "evaluation_jobs"
        ),
        "reduced_replay_runs_required": replay.get("runs_required") if has_replay else 0,
        "reduced_replay_runs_passed": replay.get("runs_passed") if has_replay else None,
        "reduced_replay_all_passed": replay_all_passed,
        "reduced_replay_consistent_signature": replay_consistent,
        "reduced_replay_matches_baseline": replay_matches_base,
        "reduced_replay_slippage_count": replay_slip_count,
        "reduced_replay_observed_signatures": (
            replay.get("observed_signature_ids") if has_replay else []
        ),
        "oracle_decisions_summary": job_res.get("oracle_decisions_summary"),
    }


def _aggregate_group(records):
    completed = [r for r in records if r["is_completed"]]
    censored = [r for r in records if r["is_censored"]]
    with_checkpoints = [
        r for r in records if r["reduced_bytes"] is not None and (r["is_completed"] or r["is_censored"])
    ]

    replay_checked = [r for r in records if r["reduced_replay_runs_required"] > 0]
    replay_passed_5 = [r for r in replay_checked if r["reduced_replay_all_passed"] is True]
    replay_zero_slip = [
        r
        for r in replay_checked
        if r["reduced_replay_all_passed"] is True
        and r["reduced_replay_matches_baseline"] is True
    ]
    replay_slipped = [
        r for r in replay_checked if (r["reduced_replay_slippage_count"] or 0) > 0
    ]

    def extract_metrics(subset):
        return {
            "count": len(subset),
            "reduced_bytes": _metric_summary([r["reduced_bytes"] for r in subset]),
            "reduction_ratio": _metric_summary([r["reduction_ratio"] for r in subset]),
            "reduced_statements": _metric_summary([r["reduced_statements"] for r in subset]),
            "statement_reduction_ratio": _metric_summary(
                [r["statement_reduction_ratio"] for r in subset]
            ),
            "oracle_executions": _metric_summary([r["oracle_executions"] for r in subset]),
            "oracle_attempts": _metric_summary([r["oracle_attempts"] for r in subset]),
            "cache_hit_ratio": _metric_summary([r["cache_hit_ratio"] for r in subset]),
            "elapsed_seconds": _metric_summary([r["elapsed_seconds"] for r in subset]),
            "oracle_seconds": _metric_summary([r["oracle_seconds"] for r in subset]),
            "reset_seconds": _metric_summary([r["reset_seconds"] for r in subset]),
            "audit_seconds": _metric_summary([r["audit_seconds"] for r in subset]),
            "validation_seconds": _metric_summary([r["validation_seconds"] for r in subset]),
            "attempt_auc": _metric_summary([r["attempt_auc"] for r in subset]),
            "engine_overhead_us": _metric_summary([r["engine_overhead_us"] for r in subset]),
        }

    return {
        "total_runs": len(records),
        "completed_runs": len(completed),
        "censored_runs": len(censored),
        "error_or_failed_runs": sum(
            1
            for r in records
            if not r["is_completed"]
            and not r["is_censored"]
            and r["job_state"] != "skipped_unstable_input"
        ),
        "skipped_unstable_runs": sum(
            1 for r in records if r["job_state"] == "skipped_unstable_input"
        ),
        "validation_passed_runs": sum(1 for r in records if r["validation_passed"] is True),
        "minimality_1_minimal_runs": sum(
            1 for r in records if r["minimality_claim"] == "statement-deletion-1-minimal"
        ),
        "reduced_replay_checked_runs": len(replay_checked),
        "reduced_replay_5of5_runs": len(replay_passed_5),
        "reduced_replay_zero_slippage_runs": len(replay_zero_slip),
        "reduced_replay_slipped_runs": len(replay_slipped),
        "completed_only": extract_metrics(completed),
        "all_with_checkpoints": extract_metrics(with_checkpoints),
    }


def aggregate_out_dir(out_dir):
    out_abs = os.path.abspath(out_dir)
    qual_root = os.path.join(out_abs, "qualified")
    runs_root = os.path.join(out_abs, "runs")

    qualifications = {}
    if os.path.isdir(qual_root):
        for case_dir in sorted(os.listdir(qual_root)):
            qpath = os.path.join(qual_root, case_dir, "input_replay.json")
            if os.path.isfile(qpath):
                try:
                    with open(qpath, encoding="utf-8") as handle:
                        qdata = json.load(handle)
                    qualifications[qdata.get("case_id", case_dir)] = qdata
                except (OSError, json.JSONDecodeError):
                    pass

    run_records = []
    if os.path.isdir(runs_root):
        for job_dir in sorted(os.listdir(runs_root)):
            jpath = os.path.join(runs_root, job_dir, "job_result.json")
            if not os.path.isfile(jpath):
                continue
            try:
                with open(jpath, encoding="utf-8") as handle:
                    jdata = json.load(handle)
            except (OSError, json.JSONDecodeError):
                continue
            run_records.append(_collect_run_record(jdata))

    by_profile = {}
    profile_groups = {}
    for rec in run_records:
        profile_groups.setdefault(rec["profile"], []).append(rec)
    for prof, recs in sorted(profile_groups.items()):
        by_profile[prof] = _aggregate_group(recs)

    by_dbms = {}
    dbms_groups = {}
    for rec in run_records:
        dbms_groups.setdefault(rec["dbms"], []).append(rec)
    for dbms, recs in sorted(dbms_groups.items()):
        by_dbms[dbms] = _aggregate_group(recs)

    by_signature = {}
    sig_groups = {}
    for rec in run_records:
        sig_groups.setdefault(rec["stratification_signature"], []).append(rec)
    for sig_key, recs in sorted(sig_groups.items()):
        sig_prof_groups = {}
        for rec in recs:
            sig_prof_groups.setdefault(rec["profile"], []).append(rec)
        sig_id = next(
            (r["baseline_signature_id"] for r in recs if r.get("baseline_signature_id")),
            None,
        )
        sig_hash = next(
            (r["baseline_signature_hash"] for r in recs if r.get("baseline_signature_hash")),
            None,
        )
        sig_cat = next(
            (r["baseline_category"] for r in recs if r.get("baseline_category")),
            None,
        )
        by_signature[sig_key] = {
            "signature_id": sig_id,
            "signature_hash": sig_hash,
            "category": sig_cat,
            "dbms": recs[0]["dbms"],
            "case_ids": sorted({r["case_id"] for r in recs}),
            "case_count": len({r["case_id"] for r in recs}),
            "overall": _aggregate_group(recs),
            "by_profile": {
                prof: _aggregate_group(precs)
                for prof, precs in sorted(sig_prof_groups.items())
            },
        }

    # Signature-stratified macro averages across unique signatures per profile
    # (prevents duplicate crash triggers of the same bug from skewing averages).
    sig_stratified_by_profile = {}
    for prof in sorted(profile_groups):
        sig_medians_ratio = []
        sig_medians_bytes = []
        sig_medians_attempts = []
        sig_medians_auc = []
        sig_medians_elapsed = []
        sig_zero_slip = 0
        sig_with_completed = 0
        for sig_entry in by_signature.values():
            p_agg = sig_entry["by_profile"].get(prof)
            if not p_agg:
                continue
            comp = p_agg["completed_only"]
            if comp["count"] > 0:
                sig_with_completed += 1
                if comp["reduction_ratio"]:
                    sig_medians_ratio.append(comp["reduction_ratio"]["median"])
                if comp["reduced_bytes"]:
                    sig_medians_bytes.append(comp["reduced_bytes"]["median"])
                if comp["oracle_attempts"]:
                    sig_medians_attempts.append(comp["oracle_attempts"]["median"])
                if comp["attempt_auc"]:
                    sig_medians_auc.append(comp["attempt_auc"]["median"])
                if comp["elapsed_seconds"]:
                    sig_medians_elapsed.append(comp["elapsed_seconds"]["median"])
            if (
                p_agg["reduced_replay_checked_runs"] > 0
                and p_agg["reduced_replay_zero_slippage_runs"] == p_agg["reduced_replay_checked_runs"]
            ):
                sig_zero_slip += 1

        sig_stratified_by_profile[prof] = {
            "signatures_observed": sum(1 for s in by_signature.values() if prof in s["by_profile"]),
            "signatures_with_completed_runs": sig_with_completed,
            "signatures_zero_slippage_all_runs": sig_zero_slip,
            "completed_signature_median_reduction_ratio": _metric_summary(sig_medians_ratio),
            "completed_signature_median_reduced_bytes": _metric_summary(sig_medians_bytes),
            "completed_signature_median_oracle_attempts": _metric_summary(sig_medians_attempts),
            "completed_signature_median_attempt_auc": _metric_summary(sig_medians_auc),
            "completed_signature_median_elapsed_seconds": _metric_summary(sig_medians_elapsed),
        }

    unique_qualified_sigs = {
        (q.get("dbms"), q["baseline_signature_id"])
        for q in qualifications.values()
        if q.get("qualified") and q.get("baseline_signature_id")
    }

    aggregated = {
        "schema_version": 1,
        "generated_at": datetime.datetime.now(datetime.UTC).isoformat(),
        "out_dir": out_abs,
        "overview": {
            "cases_in_runs": len({r["case_id"] for r in run_records}),
            "qualified_cases_evaluated": len(qualifications),
            "input_stable_cases": sum(1 for q in qualifications.values() if q.get("qualified")),
            "input_unstable_cases": sum(
                1 for q in qualifications.values() if not q.get("qualified")
            ),
            "unique_qualified_crash_signatures": len(unique_qualified_sigs),
            "total_job_runs": len(run_records),
            "unique_stratification_groups": len(by_signature),
        },
        "qualifications": qualifications,
        "by_profile": by_profile,
        "by_dbms": by_dbms,
        "by_signature": by_signature,
        "signature_stratified_by_profile": sig_stratified_by_profile,
        "runs": run_records,
    }

    agg_path = os.path.join(out_abs, "aggregated_stats.json")
    atomic_write_json(agg_path, aggregated)

    md_path = os.path.join(out_abs, "summary.md")
    md_text = render_summary_markdown(aggregated)
    _atomic_write_bytes(md_path, md_text.encode("utf-8"))

    return aggregated, agg_path, md_path


def _fmt_num(metric, fmt="{:.2f}"):
    if not isinstance(metric, dict) or metric.get("median") is None:
        return "-"
    return fmt.format(metric["median"])


def render_summary_markdown(agg):
    ov = agg["overview"]
    lines = [
        "# Occam Horizontal Batch Evaluation Summary",
        "",
        f"- **Generated:** `{agg['generated_at']}`",
        f"- **Input cases qualified:** {ov['input_stable_cases']}/{ov['qualified_cases_evaluated']} stable (5/5 consistent signature)",
        f"- **Unique crash signatures:** {ov['unique_qualified_crash_signatures']}",
        f"- **Total reduction runs:** {ov['total_job_runs']}",
        "",
        "## 1. Profile Comparison (Completed-Only Medians + Stability)",
        "",
        "| Profile | Completed / Total | Censored | Reduced B (med) | Byte Red % (med) | Attempts (med) | Attempt-AUC (med) | Elapsed s (med) | 5/5 Replay | Zero-Slip |",
        "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|",
    ]
    for prof, pdata in sorted(agg["by_profile"].items()):
        comp = pdata["completed_only"]
        lines.append(
            f"| `{prof}` | {pdata['completed_runs']} / {pdata['total_runs']} | "
            f"{pdata['censored_runs']} | "
            f"{_fmt_num(comp['reduced_bytes'], '{:.0f}')} | "
            f"{_fmt_num(comp['reduction_ratio'], '{:.2f}%')} | "
            f"{_fmt_num(comp['oracle_attempts'], '{:.0f}')} | "
            f"{_fmt_num(comp['attempt_auc'], '{:.4f}')} | "
            f"{_fmt_num(comp['elapsed_seconds'], '{:.2f}')} | "
            f"{pdata['reduced_replay_5of5_runs']}/{pdata['reduced_replay_checked_runs']} | "
            f"{pdata['reduced_replay_zero_slippage_runs']}/{pdata['reduced_replay_checked_runs']} |"
        )

    lines.extend(
        [
            "",
            "## 2. Stratified Summary by Unique Crash Signature (Macro-Median Across Signatures)",
            "",
            "| Profile | Signatures Completed | Zero-Slip Signatures | Sig-Median Byte Red % | Sig-Median Attempts | Sig-Median AUC |",
            "|---|---:|---:|---:|---:|---:|",
        ]
    )
    for prof, sdata in sorted(agg["signature_stratified_by_profile"].items()):
        lines.append(
            f"| `{prof}` | {sdata['signatures_with_completed_runs']}/{sdata['signatures_observed']} | "
            f"{sdata['signatures_zero_slippage_all_runs']}/{sdata['signatures_observed']} | "
            f"{_fmt_num(sdata['completed_signature_median_reduction_ratio'], '{:.2f}%')} | "
            f"{_fmt_num(sdata['completed_signature_median_oracle_attempts'], '{:.0f}')} | "
            f"{_fmt_num(sdata['completed_signature_median_attempt_auc'], '{:.4f}')} |"
        )

    lines.extend(
        [
            "",
            "## 3. Breakdown by DBMS",
            "",
            "| DBMS | Completed / Total | Censored | 5/5 Replay | Zero-Slip | Byte Red % (med) |",
            "|---|---:|---:|---:|---:|---:|",
        ]
    )
    for dbms, ddata in sorted(agg["by_dbms"].items()):
        comp = ddata["completed_only"]
        lines.append(
            f"| `{dbms}` | {ddata['completed_runs']} / {ddata['total_runs']} | "
            f"{ddata['censored_runs']} | "
            f"{ddata['reduced_replay_5of5_runs']}/{ddata['reduced_replay_checked_runs']} | "
            f"{ddata['reduced_replay_zero_slippage_runs']}/{ddata['reduced_replay_checked_runs']} | "
            f"{_fmt_num(comp['reduction_ratio'], '{:.2f}%')} |"
        )

    lines.append("")
    return "\n".join(lines)


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

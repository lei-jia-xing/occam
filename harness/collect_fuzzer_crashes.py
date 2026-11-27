#!/usr/bin/env python3
"""
collect_fuzzer_crashes.py - Harvest, normalize, deduplicate, and optionally
5-replay-verify crash testcases from SQUIRREL, SQLancer, SQLsmith, or raw corpora.

Outputs extracted `.sql` files and a `manifest.json` consumed directly by
`harness/batch_eval.py --manifest ...`.
"""

import argparse
import hashlib
import json
import math
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import threading

HARNESS_DIR = os.path.dirname(os.path.abspath(__file__))
if HARNESS_DIR not in sys.path:
    sys.path.insert(0, HARNESS_DIR)

from dhall_config import (  # noqa: E402
    DBMS_TO_OCCAM_DIALECT,
    SERVER_DBMS,
    SUPPORTED_DBMS,
    resolve_repo_binary,
)

CRASH_ORACLE_PATH = os.path.join(HARNESS_DIR, "crash_oracle.py")
RESET_DB_PATH = os.path.join(HARNESS_DIR, "reset_db.sh")

SUPPORTED_SOURCES = ("squirrel", "sqlancer", "sqlsmith", "raw")


def normalize_dbms(value):
    low = (value or "").strip().lower()
    aliases = {
        "postgresql": "postgres",
        "pgsql": "postgres",
        "sqlite3": "sqlite",
    }
    return aliases.get(low, low)


def _atomic_write_bytes(path, data):
    parent = os.path.dirname(os.path.abspath(path))
    os.makedirs(parent, exist_ok=True)
    fd, tmp_path = tempfile.mkstemp(dir=parent, prefix=".tmp_", suffix=".part")
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(data)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(tmp_path, path)
    except Exception:
        try:
            os.unlink(tmp_path)
        except OSError:
            pass
        raise


def atomic_write_json(path, payload):
    encoded = (json.dumps(payload, indent=2, sort_keys=False) + "\n").encode("utf-8")
    _atomic_write_bytes(path, encoded)


def sanitize_sqlancer_log(raw_bytes):
    """
    Convert a SQLancer `.log` or `-cur.log` file into replayable SQL targeting
    the harness's pre-created test database (`occam_test` / `:memory:`),
    delegating parsing/sanitization to the typed Haskell `occam-harness` CLI.
    """
    harness_bin = resolve_repo_binary("occam-harness", "OCCAM_HARNESS_BIN")
    proc = subprocess.run(
        [harness_bin, "sanitize-sqlancer", "--json"],
        input=raw_bytes,
        capture_output=True,
        check=False,
    )
    if proc.returncode != 0:
        stderr = proc.stderr.decode("utf-8", errors="replace").strip()
        raise RuntimeError(
            f"occam-harness sanitize-sqlancer failed ({proc.returncode}): {stderr}"
        )
    payload = json.loads(proc.stdout.decode("utf-8"))
    return payload["sql"].encode("utf-8"), payload["metadata"]


def _safe_case_id(raw_name):
    slug = re.sub(r"[^A-Za-z0-9_.-]+", "_", raw_name).strip("._-")
    return slug or "case"


def discover_fuzzer_candidates(
    input_dir,
    source,
    dbms,
    include_cur_logs=False,
    fallback_cur_logs=True,
):
    """
    Discover candidate SQL payloads from a fuzzer output directory.
    Returns a list of dicts: {raw_name, source_path, sql_bytes, metadata}.
    """
    root = os.path.abspath(input_dir)
    if not os.path.isdir(root):
        raise FileNotFoundError(f"Input directory does not exist: {root}")

    candidates = []

    if source == "squirrel":
        for dirpath, dirnames, filenames in os.walk(root):
            dirnames.sort()
            if os.path.basename(dirpath) != "crashes":
                continue
            worker_id = os.path.basename(os.path.dirname(dirpath))
            for fname in sorted(filenames):
                if fname == "README.txt" or fname.startswith("."):
                    continue
                fpath = os.path.join(dirpath, fname)
                if not os.path.isfile(fpath):
                    continue
                with open(fpath, "rb") as handle:
                    data = handle.read()
                if not data.strip():
                    continue
                if not data.endswith(b"\n"):
                    data += b"\n"
                candidates.append(
                    {
                        "raw_name": f"squirrel_{worker_id}_{fname}",
                        "source_path": fpath,
                        "sql_bytes": data,
                        "metadata": {"fuzzer": "squirrel", "worker_id": worker_id},
                    }
                )

    elif source == "sqlancer":
        primary_logs = []
        cur_logs = []
        for dirpath, dirnames, filenames in os.walk(root):
            dirnames.sort()
            for fname in sorted(filenames):
                if not fname.endswith(".log"):
                    continue
                if fname.startswith(f"sqlancer_{dbms}_"):
                    continue
                if fname.endswith(("-plan.log", "-reduce.log")):
                    continue
                fpath = os.path.join(dirpath, fname)
                if not os.path.isfile(fpath):
                    continue
                if fname.endswith("-cur.log"):
                    cur_logs.append(fpath)
                else:
                    primary_logs.append(fpath)

        chosen = list(primary_logs)
        if include_cur_logs or (fallback_cur_logs and not primary_logs):
            chosen.extend(cur_logs)

        for fpath in chosen:
            with open(fpath, "rb") as handle:
                raw_data = handle.read()
            sql_bytes, parsed_meta = sanitize_sqlancer_log(raw_data)
            # Strip metadata comments when checking if any actual SQL statements remain.
            non_comment = [
                ln
                for ln in sql_bytes.decode("utf-8", errors="replace").splitlines()
                if ln.strip() and not ln.strip().startswith("--")
            ]
            if not non_comment:
                continue
            rel = os.path.relpath(fpath, root)
            stem = os.path.splitext(rel)[0]
            candidates.append(
                {
                    "raw_name": f"sqlancer_{stem}",
                    "source_path": fpath,
                    "sql_bytes": sql_bytes,
                    "metadata": {"fuzzer": "sqlancer", **parsed_meta},
                }
            )

    elif source in ("sqlsmith", "raw"):
        for dirpath, dirnames, filenames in os.walk(root):
            dirnames.sort()
            for fname in sorted(filenames):
                if not fname.endswith(".sql"):
                    continue
                fpath = os.path.join(dirpath, fname)
                if not os.path.isfile(fpath):
                    continue
                with open(fpath, "rb") as handle:
                    data = handle.read()
                if not data.strip():
                    continue
                meta_path = os.path.splitext(fpath)[0] + ".meta.json"
                meta = {}
                if os.path.isfile(meta_path):
                    try:
                        with open(meta_path, encoding="utf-8") as mf:
                            meta = json.load(mf)
                    except (OSError, json.JSONDecodeError):
                        meta = {}
                stem = os.path.splitext(os.path.relpath(fpath, root))[0]
                candidates.append(
                    {
                        "raw_name": stem,
                        "source_path": fpath,
                        "sql_bytes": data,
                        "metadata": {"fuzzer": source, **meta},
                    }
                )

    return candidates


def _normalize_baseline_ref(baseline_signature):
    if not baseline_signature:
        return "", ""
    if isinstance(baseline_signature, dict):
        inner = baseline_signature.get("crash_signature")
        if isinstance(inner, dict):
            baseline_signature = inner
        return str(baseline_signature.get("id") or ""), str(
            baseline_signature.get("hash") or ""
        )
    text = str(baseline_signature).strip()
    if text.startswith("{"):
        try:
            obj = json.loads(text)
            if isinstance(obj, dict):
                return _normalize_baseline_ref(obj)
        except json.JSONDecodeError:
            pass
    return text, text


AMBIENT_ORACLE_ENV_KEYS = (
    "ORACLE_REJECT_ON_SLIPPAGE",
    "ORACLE_STACK_FRAMES",
    "ORACLE_TIMEOUT",
    "ORACLE_DECISION_LOG",
    "ORACLE_ALLOW_STDERR_ONLY",
    "ORACLE_ALLOW_PSQL_META",
    "BASELINE_SIGNATURE",
    "EXPECTED_SIGNATURE",
    "CONTAINER_NAME",
    "DBMS",
)


def resolve_engine_bin(engine_bin):
    if not engine_bin:
        return ""
    if (
        os.path.sep in engine_bin
        or (os.path.altsep and os.path.altsep in engine_bin)
        or os.path.exists(engine_bin)
    ):
        return os.path.abspath(engine_bin)
    return shutil.which(engine_bin) or engine_bin


def _run_cmd_interruptible(cmd, input_bytes, timeout_secs, env, stop_event):
    if stop_event is None:
        return subprocess.run(
            cmd,
            input=input_bytes,
            capture_output=True,
            timeout=timeout_secs,
            env=env,
            check=False,
        )

    proc = subprocess.Popen(
        cmd,
        stdin=subprocess.PIPE if input_bytes is not None else subprocess.DEVNULL,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
        start_new_session=True,
    )
    done_event = threading.Event()
    reaped_lock = threading.Lock()
    reaped = {"done": False}

    def _watch():
        while not done_event.is_set():
            if stop_event.wait(timeout=0.15):
                with reaped_lock:
                    if not reaped["done"]:
                        try:
                            os.killpg(proc.pid, signal.SIGTERM)
                        except OSError:
                            pass
                if not done_event.wait(timeout=1.0):
                    with reaped_lock:
                        if not reaped["done"]:
                            try:
                                os.killpg(proc.pid, signal.SIGKILL)
                            except OSError:
                                pass
                return

    watcher = threading.Thread(target=_watch, daemon=True)
    watcher.start()
    try:
        stdout_bytes, stderr_bytes = proc.communicate(input=input_bytes, timeout=timeout_secs)
    except subprocess.TimeoutExpired:
        with reaped_lock:
            if not reaped["done"]:
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except OSError:
                    pass
        proc.communicate()
        raise
    finally:
        with reaped_lock:
            reaped["done"] = True
        done_event.set()
        watcher.join(timeout=1.0)

    return subprocess.CompletedProcess(
        args=cmd,
        returncode=proc.returncode,
        stdout=stdout_bytes,
        stderr=stderr_bytes,
    )


def run_replay_verification(
    sql_bytes,
    dbms,
    replays=5,
    expected_signature="",
    container_name="",
    host="",
    port=None,
    user="",
    password="",
    dbname="",
    engine_bin="",
    decision_log="",
    stack_frames=5,
    probe_timeout=30.0,
    baseline_signature="",
    require_consistent_signature=True,
    stop_event=None,
):
    """
    Execute `replays` clean-state replays using reset_db.sh + crash_oracle.py.

    Returns a structured verification report including extracted baseline
    crash_signature, slippage counts, and per-replay outcomes.
    """
    replay_records = []
    if replays <= 0:
        return {
            "runs_required": 0,
            "runs_passed": 0,
            "all_passed": True,
            "consistent_signature": True,
            "matches_baseline": None,
            "slippage_count": 0,
            "baseline_signature": None,
            "observed_signature_ids": [],
            "replays": [],
            "interrupted": False,
        }

    base_id_ref, base_hash_ref = _normalize_baseline_ref(baseline_signature)
    base_cli_arg = base_id_ref or (
        json.dumps(baseline_signature, sort_keys=True)
        if isinstance(baseline_signature, dict)
        else str(baseline_signature or "")
    )

    reset_cmd = [RESET_DB_PATH, "--dbms", dbms]
    oracle_cmd = [
        sys.executable,
        CRASH_ORACLE_PATH,
        "--dbms",
        dbms,
        "--print-signature",
        "--stack-frames",
        str(stack_frames),
    ]

    if container_name:
        reset_cmd.extend(["--container-name", container_name])
        oracle_cmd.extend(["--container-name", container_name])
    if host:
        reset_cmd.extend(["--host", host])
        oracle_cmd.extend(["--host", host])
    if port is not None:
        reset_cmd.extend(["--port", str(port)])
        oracle_cmd.extend(["--port", str(port)])
    if user:
        reset_cmd.extend(["--user", user])
        oracle_cmd.extend(["--user", user])
    if dbname:
        reset_cmd.extend(["--dbname", dbname])
        oracle_cmd.extend(["--dbname", dbname])
    resolved_engine_bin = resolve_engine_bin(engine_bin)
    if resolved_engine_bin and dbms in {"sqlite", "duckdb"} and not container_name:
        oracle_cmd.extend(["--engine-bin", resolved_engine_bin])
    if expected_signature:
        oracle_cmd.extend(["--expected-signature", expected_signature])
    if base_cli_arg:
        oracle_cmd.extend(["--baseline-signature", base_cli_arg])
    if decision_log:
        os.makedirs(os.path.dirname(os.path.abspath(decision_log)), exist_ok=True)
        oracle_cmd.extend(["--decision-log", os.path.abspath(decision_log)])

    env = os.environ.copy()
    for key in AMBIENT_ORACLE_ENV_KEYS:
        env.pop(key, None)
    if password:
        if dbms == "postgres":
            env["PGPASSWORD"] = password
        elif dbms == "mysql":
            env["MYSQL_PWD"] = password
        elif dbms == "mariadb":
            env["MARIADB_PWD"] = password
            env["MYSQL_PWD"] = password
        elif dbms == "clickhouse":
            env["CLICKHOUSE_PASSWORD"] = password

    observed_sigs = []
    sig_objects = []
    slippage_count = 0
    interrupted = False

    for idx in range(1, replays + 1):
        if stop_event is not None and stop_event.is_set():
            interrupted = True
            break

        try:
            reset_proc = _run_cmd_interruptible(
                reset_cmd,
                input_bytes=None,
                timeout_secs=max(30.0, probe_timeout),
                env=env,
                stop_event=stop_event,
            )
        except subprocess.TimeoutExpired:
            replay_records.append(
                {
                    "replay": idx,
                    "reset_ok": False,
                    "exit_code": None,
                    "passed": False,
                    "reason": "reset-timeout",
                    "crash_signature_id": None,
                    "crash_signature_hash": None,
                    "slipped": None,
                }
            )
            continue

        if stop_event is not None and stop_event.is_set():
            interrupted = True
            break

        if reset_proc.returncode != 0:
            reset_stderr = reset_proc.stderr
            if isinstance(reset_stderr, bytes):
                reset_stderr = reset_stderr.decode("utf-8", errors="replace")
            replay_records.append(
                {
                    "replay": idx,
                    "reset_ok": False,
                    "exit_code": reset_proc.returncode,
                    "passed": False,
                    "reason": "reset-failed",
                    "stderr": (reset_stderr or "")[-512:],
                    "crash_signature_id": None,
                    "crash_signature_hash": None,
                    "slipped": None,
                }
            )
            continue

        try:
            proc = _run_cmd_interruptible(
                oracle_cmd,
                input_bytes=sql_bytes,
                timeout_secs=probe_timeout,
                env=env,
                stop_event=stop_event,
            )
        except subprocess.TimeoutExpired:
            replay_records.append(
                {
                    "replay": idx,
                    "reset_ok": True,
                    "exit_code": 124,
                    "passed": False,
                    "reason": "oracle-timeout",
                    "crash_signature_id": None,
                    "crash_signature_hash": None,
                    "slipped": None,
                }
            )
            continue

        if stop_event is not None and stop_event.is_set():
            interrupted = True
            break

        stdout_text = (proc.stdout or b"").decode("utf-8", errors="replace").strip()
        sig_obj = None
        if stdout_text:
            for line in reversed(stdout_text.splitlines()):
                line = line.strip()
                if not line.startswith("{"):
                    continue
                try:
                    sig_obj = json.loads(line)
                    break
                except json.JSONDecodeError:
                    continue

        sig_id = sig_obj.get("id") if isinstance(sig_obj, dict) else None
        sig_hash = sig_obj.get("hash") if isinstance(sig_obj, dict) else None
        slipped = None
        if (base_id_ref or base_hash_ref) and sig_id:
            slipped = sig_id != base_id_ref and sig_hash != base_hash_ref
            if slipped:
                slippage_count += 1

        passed = proc.returncode == 0
        if passed and sig_id:
            observed_sigs.append(sig_id)
            sig_objects.append(sig_obj)

        replay_records.append(
            {
                "replay": idx,
                "reset_ok": True,
                "exit_code": proc.returncode,
                "passed": passed,
                "crash_signature_id": sig_id,
                "crash_signature_hash": sig_hash,
                "slipped": slipped,
            }
        )

    runs_passed = sum(1 for r in replay_records if r["passed"])
    all_passed = (not interrupted) and runs_passed == replays
    unique_sig_ids = list(dict.fromkeys(observed_sigs))
    consistent_signature = (
        all_passed
        and len(observed_sigs) == replays
        and len(unique_sig_ids) == 1
    )
    if not require_consistent_signature and all_passed and len(unique_sig_ids) <= 1:
        consistent_signature = True

    canonical_sig = (
        sig_objects[0] if (sig_objects and len(unique_sig_ids) == 1) else None
    )
    matches_baseline = None
    if base_id_ref or base_hash_ref:
        matches_baseline = (
            all_passed and slippage_count == 0 and len(unique_sig_ids) == 1
        )

    return {
        "runs_required": replays,
        "runs_passed": runs_passed,
        "all_passed": all_passed,
        "consistent_signature": consistent_signature,
        "matches_baseline": matches_baseline,
        "slippage_count": slippage_count,
        "baseline_signature": canonical_sig,
        "observed_signature_ids": unique_sig_ids,
        "replays": replay_records,
        "interrupted": interrupted,
    }


def collect_crashes(
    input_dir,
    out_dir,
    source,
    dbms,
    include_cur_logs=False,
    fallback_cur_logs=True,
    verify=False,
    replays=5,
    dedup_by_signature=True,
    expected_signature="",
    container_name="",
    host="",
    port=None,
    user="",
    password="",
    dbname="",
    engine_bin="",
    stack_frames=5,
    probe_timeout=30.0,
):
    raw_candidates = discover_fuzzer_candidates(
        input_dir=input_dir,
        source=source,
        dbms=dbms,
        include_cur_logs=include_cur_logs,
        fallback_cur_logs=fallback_cur_logs,
    )

    out_abs = os.path.abspath(out_dir)
    cases_dir = os.path.join(out_abs, "cases")
    os.makedirs(cases_dir, exist_ok=True)

    # Phase 1: Exact-byte deduplication.
    seen_sha = {}
    unique_candidates = []
    used_ids = set()

    for cand in raw_candidates:
        sha = hashlib.sha256(cand["sql_bytes"]).hexdigest()
        if sha in seen_sha:
            seen_sha[sha]["byte_duplicates"].append(cand["source_path"])
            continue

        base_id = _safe_case_id(cand["raw_name"])
        case_id = base_id
        suffix = 2
        while case_id in used_ids:
            case_id = f"{base_id}_{suffix}"
            suffix += 1
        used_ids.add(case_id)

        meta = cand.get("metadata") or {}
        crash_sig_meta = meta.get("crash_signature") or {}
        if not isinstance(crash_sig_meta, dict):
            crash_sig_meta = {}
        entry = {
            "id": case_id,
            "dbms": dbms,
            "dialect": DBMS_TO_OCCAM_DIALECT[dbms],
            "source_type": source,
            "source_path": cand["source_path"],
            "sql_sha256": sha,
            "byte_count": len(cand["sql_bytes"]),
            "expected_signature": expected_signature
            or crash_sig_meta.get("expected_signature", ""),
            "provenance": meta,
            "sql_bytes": cand["sql_bytes"],
            "byte_duplicates": [],
        }
        seen_sha[sha] = entry
        unique_candidates.append(entry)

    # Phase 2: Optional 5-replay verification + crash-signature deduplication.
    manifest_cases = []
    rejected_unstable = []
    seen_sig_ids = {}
    signature_duplicates = 0

    for entry in unique_candidates:
        sql_rel = os.path.join("cases", f"{entry['id']}.sql")
        sql_abs = os.path.join(out_abs, sql_rel)

        verification = None
        qualified = True if verify else None
        sig_id = None
        sig_obj = None

        if verify:
            decision_log = os.path.join(
                out_abs, "verify_logs", f"{entry['id']}_decisions.jsonl"
            )
            try:
                if os.path.exists(decision_log):
                    os.unlink(decision_log)
            except OSError:
                pass
            verification = run_replay_verification(
                sql_bytes=entry["sql_bytes"],
                dbms=dbms,
                replays=replays,
                expected_signature=entry["expected_signature"],
                container_name=container_name,
                host=host,
                port=port,
                user=user,
                password=password,
                dbname=dbname,
                engine_bin=engine_bin,
                decision_log=decision_log,
                stack_frames=stack_frames,
                probe_timeout=probe_timeout,
            )
            qualified = (
                verification["all_passed"] and verification["consistent_signature"]
            )
            sig_obj = verification.get("baseline_signature")
            sig_id = sig_obj.get("id") if isinstance(sig_obj, dict) else None

            if not qualified:
                rejected_unstable.append(
                    {
                        "id": entry["id"],
                        "source_path": entry["source_path"],
                        "sql_sha256": entry["sql_sha256"],
                        "replay_verification": verification,
                    }
                )
                continue

            if dedup_by_signature and sig_id and sig_id in seen_sig_ids:
                signature_duplicates += 1
                seen_sig_ids[sig_id]["signature_duplicates"].append(
                    entry["source_path"]
                )
                continue

        _atomic_write_bytes(sql_abs, entry["sql_bytes"])
        record = {
            "id": entry["id"],
            "dbms": entry["dbms"],
            "dialect": entry["dialect"],
            "file": sql_abs,
            "relative_file": sql_rel,
            "source_type": entry["source_type"],
            "source_path": entry["source_path"],
            "sql_sha256": entry["sql_sha256"],
            "byte_count": entry["byte_count"],
            "expected_signature": entry["expected_signature"],
            "baseline_signature": sig_obj,
            "baseline_signature_id": sig_id,
            "qualified": qualified,
            "replay_verification": verification,
            "provenance": entry["provenance"],
            "byte_duplicate_count": len(entry["byte_duplicates"]),
            "signature_duplicates": [],
        }
        manifest_cases.append(record)
        if sig_id and sig_id not in seen_sig_ids:
            seen_sig_ids[sig_id] = record

    summary = {
        "source": source,
        "dbms": dbms,
        "raw_candidates_discovered": len(raw_candidates),
        "unique_byte_candidates": len(unique_candidates),
        "verified": verify,
        "replays_required": replays if verify else 0,
        "rejected_unstable_or_noncrashing": len(rejected_unstable),
        "signature_duplicates_collapsed": signature_duplicates,
        "unique_signatures": len(seen_sig_ids),
        "manifest_case_count": len(manifest_cases),
    }

    manifest = {
        "schema_version": 1,
        "dbms": dbms,
        "source": source,
        "cases": manifest_cases,
        "rejected_cases": rejected_unstable,
        "summary": summary,
    }
    manifest_path = os.path.join(out_abs, "manifest.json")
    atomic_write_json(manifest_path, manifest)
    return manifest, manifest_path


def main():
    parser = argparse.ArgumentParser(
        description="Harvest and optionally 5-replay-verify fuzzer crashes into an Occam batch manifest."
    )
    parser.add_argument(
        "--source",
        required=True,
        choices=list(SUPPORTED_SOURCES),
        help="Fuzzer crash source layout",
    )
    parser.add_argument(
        "--dbms",
        required=True,
        help="Target DBMS: postgres, mysql, mariadb, clickhouse, sqlite, duckdb",
    )
    parser.add_argument(
        "--input-dir",
        required=True,
        help="Directory containing fuzzer outputs / crash logs",
    )
    parser.add_argument(
        "--out-dir",
        required=True,
        help="Output directory for normalized .sql files and manifest.json",
    )
    parser.add_argument(
        "--include-cur-logs",
        action="store_true",
        help="For SQLancer, also harvest *-cur.log files even when exception .log files exist",
    )
    parser.add_argument(
        "--no-fallback-cur-logs",
        action="store_true",
        help="For SQLancer, do not fall back to *-cur.log when no exception .log exists",
    )
    parser.add_argument(
        "--verify",
        action="store_true",
        help="Run N-replay verification via reset_db.sh + crash_oracle.py and extract signatures",
    )
    parser.add_argument(
        "--replays",
        type=int,
        default=5,
        help="Number of clean-state replays when --verify is enabled (default: 5)",
    )
    parser.add_argument(
        "--keep-signature-duplicates",
        action="store_true",
        help="Keep cases with duplicate crash_signature IDs instead of deduplicating",
    )
    parser.add_argument(
        "--expected-signature",
        default="",
        help="Optional expected crash signature regex for verification",
    )
    parser.add_argument(
        "--container-name",
        default="",
        help="Target container name passed to reset_db.sh and crash_oracle.py",
    )
    parser.add_argument("--host", default="", help="Target DBMS host")
    parser.add_argument("--port", type=int, default=None, help="Target DBMS port")
    parser.add_argument("--user", default="", help="Target DBMS user")
    parser.add_argument("--password", default="", help="Target DBMS password")
    parser.add_argument("--dbname", default="", help="Target database name")
    parser.add_argument(
        "--engine-bin",
        default="",
        help="Local embedded engine binary override for sqlite/duckdb",
    )
    parser.add_argument(
        "--stack-frames",
        type=int,
        default=5,
        help="Stack frames retained in normalized crash signatures (default: 5)",
    )
    parser.add_argument(
        "--probe-timeout",
        type=float,
        default=30.0,
        help="Per-replay timeout in seconds (default: 30)",
    )

    args = parser.parse_args()
    dbms = normalize_dbms(args.dbms)
    if dbms not in SUPPORTED_DBMS:
        parser.error(f"unsupported --dbms: {args.dbms}")
    if args.replays <= 0:
        parser.error("--replays must be positive")
    if args.stack_frames <= 0:
        parser.error("--stack-frames must be positive")
    if not math.isfinite(args.probe_timeout) or args.probe_timeout <= 0:
        parser.error("--probe-timeout must be finite and positive")

    manifest, manifest_path = collect_crashes(
        input_dir=args.input_dir,
        out_dir=args.out_dir,
        source=args.source,
        dbms=dbms,
        include_cur_logs=args.include_cur_logs,
        fallback_cur_logs=not args.no_fallback_cur_logs,
        verify=args.verify,
        replays=args.replays,
        dedup_by_signature=not args.keep_signature_duplicates,
        expected_signature=args.expected_signature,
        container_name=args.container_name,
        host=args.host,
        port=args.port,
        user=args.user,
        password=args.password,
        dbname=args.dbname,
        engine_bin=args.engine_bin,
        stack_frames=args.stack_frames,
        probe_timeout=args.probe_timeout,
    )
    print(json.dumps({"manifest_path": manifest_path, **manifest["summary"]}, indent=2))


if __name__ == "__main__":
    main()

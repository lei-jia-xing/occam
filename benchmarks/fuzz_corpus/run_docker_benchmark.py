#!/usr/bin/env python3
"""Multi-Tool Comparative Driver for Modern SQL Fuzz Corpus.

Executes comparative crash reduction across 18 modern SQL fuzz scenarios comparing:
  1. Occam: Dependency-aware hierarchical SQL reducer
  2. C-Reduce: Upstream C/C++ & text reducer (if installed)
  3. SQLancer: Java SQLancer reducer (if /opt/sqlancer/sqlancer.jar exists)
  4. ddmin: Standard delta debugging baseline via `occam-eval --baseline 2`

Emits machine-readable streaming JSONL records to benchmarks/fuzz_benchmark_results.jsonl.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Any


def find_binary(root: Path, name: str) -> Path | None:
    # 1. Check cabal list-bin
    try:
        res = subprocess.run(
            ["cabal", "list-bin", name],
            check=False,
            cwd=root,
            capture_output=True,
            text=True,
        )
        if res.returncode == 0 and res.stdout.strip():
            p = Path(res.stdout.strip())
            if p.is_file() and os.access(p, os.X_OK):
                return p
    except Exception:
        pass

    # 2. Check dist-newstyle
    pattern = f"dist-newstyle/**/{name}"
    for p in root.glob(pattern):
        if p.is_file() and os.access(p, os.X_OK) and p.name == name:
            return p

    # 3. Check system PATH
    which_path = shutil.which(name)
    if which_path:
        return Path(which_path)

    return None


def count_statements(sql_bytes: bytes) -> int:
    text = sql_bytes.decode("utf-8", errors="replace")
    text = re.sub(r"--.*", "", text)
    text = re.sub(r"/\*.*?\*/", "", text, flags=re.DOTALL)
    stmts = [s.strip() for s in text.split(";") if s.strip()]
    return max(1, len(stmts))


def verify_reproducer(oracle_sh: Path, scenario_id: str, sql_file: Path) -> bool:
    if not sql_file.is_file():
        return False
    res = subprocess.run(
        [str(oracle_sh), scenario_id, str(sql_file)],
        check=False,
        capture_output=True,
    )
    return res.returncode == 0


def run_occam(
    entry: dict[str, Any],
    root: Path,
    occam_bin: Path,
    oracle_sh: Path,
    work_dir: Path,
    timeout_sec: float,
) -> dict[str, Any] | None:
    sid = entry["id"]
    sql_file = root / "benchmarks" / "fuzz_corpus" / entry["file"]
    out_dir = work_dir / f"{sid}_occam"
    out_dir.mkdir(parents=True, exist_ok=True)

    oracle_cmd = f"{oracle_sh} {sid}"
    cmd = [
        str(occam_bin),
        str(sql_file),
        "--oracle", oracle_cmd,
        "--out", str(out_dir),
        "--timeout", str(timeout_sec),
    ]

    t0 = time.time()
    res = subprocess.run(cmd, check=False, cwd=root, capture_output=True, text=True)
    wall_time = time.time() - t0

    stats_file = out_dir / "stats.json"
    minimal_file = out_dir / "minimal.sql"

    if not stats_file.exists():
        return None

    try:
        stats = json.loads(stats_file.read_text(encoding="utf-8"))
    except Exception:
        return None

    init_stmts = stats.get("initial_statements", entry["statements"])
    red_stmts = stats.get("reduced_statements", 0)
    init_bytes = stats.get("initial_bytes", entry["bytes"])
    red_bytes = stats.get("reduced_bytes", 0)
    calls = stats.get("oracle_executions", 0)
    elapsed = stats.get("elapsed_seconds", wall_time)

    stmt_red_pct = round((1.0 - (red_stmts / init_stmts)) * 100, 2) if init_stmts > 0 else 0.0
    byte_red_pct = round((1.0 - (red_bytes / init_bytes)) * 100, 2) if init_bytes > 0 else 0.0
    valid = verify_reproducer(oracle_sh, sid, minimal_file)

    return {
        "scenario": sid,
        "tool": "Occam",
        "scale": f"{entry['statements']} stmts",
        "initial_stmts": init_stmts,
        "reduced_stmts": red_stmts,
        "stmt_red_pct": stmt_red_pct,
        "initial_bytes": init_bytes,
        "reduced_bytes": red_bytes,
        "byte_red_pct": byte_red_pct,
        "oracle_calls": calls,
        "wall_time_sec": round(elapsed, 4),
        "valid_reproducer": valid,
    }


def run_ddmin(
    entry: dict[str, Any],
    root: Path,
    eval_bin: Path,
    oracle_sh: Path,
    work_dir: Path,
    timeout_sec: float,
) -> dict[str, Any] | None:
    sid = entry["id"]
    sql_file = root / "benchmarks" / "fuzz_corpus" / entry["file"]
    out_dir = work_dir / f"{sid}_ddmin"
    out_dir.mkdir(parents=True, exist_ok=True)

    oracle_cmd = f"{oracle_sh} {sid}"
    cmd = [
        str(eval_bin),
        str(sql_file),
        "--oracle", oracle_cmd,
        "--out", str(out_dir),
        "--timeout", str(timeout_sec),
        "--baseline", "2",
    ]

    t0 = time.time()
    res = subprocess.run(cmd, check=False, cwd=root, capture_output=True, text=True)
    wall_time = time.time() - t0

    bench_json = out_dir / "benchmark.json"
    minimal_file = out_dir / "minimal.sql"

    if not bench_json.exists():
        return None

    try:
        metrics = json.loads(bench_json.read_text(encoding="utf-8"))
        if not metrics or not isinstance(metrics, list):
            return None
        m = metrics[0]
    except Exception:
        return None

    init_stmts = m.get("metricInitialStmts", entry["statements"])
    red_stmts = m.get("metricReducedStmts", 0)
    init_bytes = m.get("metricInitialBytes", entry["bytes"])
    red_bytes = m.get("metricReducedBytes", 0)
    calls = m.get("metricOracleCalls", 0)
    sec = m.get("metricTimeSeconds", wall_time)

    stmt_red = round(m.get("metricStmtReduction", 0.0), 2)
    byte_red = round(m.get("metricByteReduction", 0.0), 2)
    valid = verify_reproducer(oracle_sh, sid, minimal_file)

    return {
        "scenario": sid,
        "tool": "ddmin",
        "scale": f"{entry['statements']} stmts",
        "initial_stmts": init_stmts,
        "reduced_stmts": red_stmts,
        "stmt_red_pct": stmt_red,
        "initial_bytes": init_bytes,
        "reduced_bytes": red_bytes,
        "byte_red_pct": byte_red,
        "oracle_calls": calls,
        "wall_time_sec": round(sec, 4),
        "valid_reproducer": valid,
    }


def run_creduce(
    entry: dict[str, Any],
    root: Path,
    creduce_bin: Path,
    oracle_sh: Path,
    work_dir: Path,
    timeout_sec: float,
) -> dict[str, Any] | None:
    sid = entry["id"]
    sql_file = root / "benchmarks" / "fuzz_corpus" / entry["file"]
    out_dir = work_dir / f"{sid}_creduce"
    out_dir.mkdir(parents=True, exist_ok=True)

    candidate_sql = out_dir / "candidate.sql"
    shutil.copy2(sql_file, candidate_sql)
    calls_file = out_dir / "calls.txt"
    if calls_file.exists():
        calls_file.unlink()

    # Create interestingness test script
    test_sh = out_dir / "test.sh"
    test_content = f"""#!/usr/bin/env bash
echo 1 >> "{calls_file.resolve()}"
exec "{oracle_sh.resolve()}" "{sid}" "{candidate_sql.resolve()}"
"""
    test_sh.write_text(test_content, encoding="utf-8")
    test_sh.chmod(0o755)

    cmd = [
        str(creduce_bin),
        str(test_sh),
        str(candidate_sql),
        "--n", "1",
        "--timeout", str(int(timeout_sec)),
    ]

    t0 = time.time()
    try:
        subprocess.run(cmd, check=False, cwd=out_dir, capture_output=True, timeout=timeout_sec * 5)
    except subprocess.TimeoutExpired:
        pass
    wall_time = time.time() - t0

    calls = 0
    if calls_file.exists():
        calls = len(calls_file.read_text(encoding="utf-8").splitlines())

    reduced_bytes_val = candidate_sql.stat().st_size if candidate_sql.exists() else entry["bytes"]
    reduced_bytes_content = candidate_sql.read_bytes() if candidate_sql.exists() else b""
    red_stmts = count_statements(reduced_bytes_content)

    init_stmts = entry["statements"]
    init_bytes = entry["bytes"]
    stmt_red_pct = round((1.0 - (red_stmts / init_stmts)) * 100, 2) if init_stmts > 0 else 0.0
    byte_red_pct = round((1.0 - (reduced_bytes_val / init_bytes)) * 100, 2) if init_bytes > 0 else 0.0
    valid = verify_reproducer(oracle_sh, sid, candidate_sql)

    return {
        "scenario": sid,
        "tool": "C-Reduce",
        "scale": f"{entry['statements']} stmts",
        "initial_stmts": init_stmts,
        "reduced_stmts": red_stmts,
        "stmt_red_pct": stmt_red_pct,
        "initial_bytes": init_bytes,
        "reduced_bytes": reduced_bytes_val,
        "byte_red_pct": byte_red_pct,
        "oracle_calls": calls,
        "wall_time_sec": round(wall_time, 4),
        "valid_reproducer": valid,
    }


def run_sqlancer(
    entry: dict[str, Any],
    root: Path,
    sqlancer_jar: Path,
    oracle_sh: Path,
    work_dir: Path,
    timeout_sec: float,
) -> dict[str, Any] | None:
    sid = entry["id"]
    sql_file = root / "benchmarks" / "fuzz_corpus" / entry["file"]
    out_dir = work_dir / f"{sid}_sqlancer"
    out_dir.mkdir(parents=True, exist_ok=True)

    candidate_sql = out_dir / "candidate.sql"
    shutil.copy2(sql_file, candidate_sql)

    # Attempt to invoke SQLancer reducer if class available
    cmd = [
        "java",
        "-cp", str(sqlancer_jar),
        "sqlancer.reducer.Reducer",
        str(candidate_sql),
    ]

    t0 = time.time()
    try:
        res = subprocess.run(cmd, check=False, cwd=out_dir, capture_output=True, timeout=timeout_sec)
        wall_time = time.time() - t0
        if res.returncode != 0:
            return None
    except Exception:
        return None

    reduced_bytes_val = candidate_sql.stat().st_size
    red_stmts = count_statements(candidate_sql.read_bytes())
    init_stmts = entry["statements"]
    init_bytes = entry["bytes"]
    stmt_red_pct = round((1.0 - (red_stmts / init_stmts)) * 100, 2) if init_stmts > 0 else 0.0
    byte_red_pct = round((1.0 - (reduced_bytes_val / init_bytes)) * 100, 2) if init_bytes > 0 else 0.0
    valid = verify_reproducer(oracle_sh, sid, candidate_sql)

    return {
        "scenario": sid,
        "tool": "SQLancer",
        "scale": f"{entry['statements']} stmts",
        "initial_stmts": init_stmts,
        "reduced_stmts": red_stmts,
        "stmt_red_pct": stmt_red_pct,
        "initial_bytes": init_bytes,
        "reduced_bytes": reduced_bytes_val,
        "byte_red_pct": byte_red_pct,
        "oracle_calls": 0,
        "wall_time_sec": round(wall_time, 4),
        "valid_reproducer": valid,
    }


def run_scenario_tools(
    entry: dict[str, Any],
    root: Path,
    tools: list[str],
    occam_bin: Path | None,
    eval_bin: Path | None,
    creduce_bin: Path | None,
    sqlancer_jar: Path | None,
    oracle_sh: Path,
    work_dir: Path,
    timeout_sec: float,
) -> list[dict[str, Any]]:
    results: list[dict[str, Any]] = []
    sid = entry["id"]

    for tool in tools:
        res = None
        if tool == "occam" and occam_bin:
            res = run_occam(entry, root, occam_bin, oracle_sh, work_dir, timeout_sec)
        elif tool == "ddmin" and eval_bin:
            res = run_ddmin(entry, root, eval_bin, oracle_sh, work_dir, timeout_sec)
        elif tool == "creduce" and creduce_bin:
            res = run_creduce(entry, root, creduce_bin, oracle_sh, work_dir, timeout_sec)
        elif tool == "sqlancer" and sqlancer_jar:
            res = run_sqlancer(entry, root, sqlancer_jar, oracle_sh, work_dir, timeout_sec)

        if res:
            results.append(res)
            v_str = "PASS" if res.get("valid_reproducer") else "FAIL"
            print(
                f"[{res['tool']:8s}] {sid:22s} ({entry['statements']:3d} stmts) "
                f"-> red {res['reduced_stmts']:2d} stmts ({res['stmt_red_pct']:5.1f}%), "
                f"calls: {res['oracle_calls']:4d}, time: {res['wall_time_sec']:5.2f}s [{v_str}]"
            )

    return results


def main() -> None:
    parser = argparse.ArgumentParser(description="Multi-Tool Fuzz Benchmark Runner")
    parser.add_argument("--jobs", "-j", type=int, default=2, help="Parallel worker threads (default: 2)")
    parser.add_argument("--timeout", type=float, default=15.0, help="Per-oracle execution timeout (s)")
    parser.add_argument("--out", type=str, default=None, help="Output JSONL path")
    parser.add_argument("--tools", type=str, default="occam,ddmin,creduce,sqlancer", help="Comma-separated tools")
    args = parser.parse_args()

    root = Path(__file__).resolve().parents[2]
    fuzz_dir = root / "benchmarks" / "fuzz_corpus"
    meta_path = fuzz_dir / "metadata.json"
    oracle_sh = fuzz_dir / "fuzz_oracle.sh"
    jsonl_path = Path(args.out) if args.out else root / "benchmarks" / "fuzz_benchmark_results.jsonl"

    if not meta_path.exists():
        print(f"Error: {meta_path} does not exist.", file=sys.stderr)
        sys.exit(1)

    metadata = json.loads(meta_path.read_text(encoding="utf-8"))
    selected_tools = [t.strip().lower() for t in args.tools.split(",") if t.strip()]

    # Locate reducer binaries / tools
    occam_bin = find_binary(root, "occam") if "occam" in selected_tools else None
    eval_bin = find_binary(root, "occam-eval") if "ddmin" in selected_tools else None
    creduce_bin = Path(shutil.which("creduce")) if ("creduce" in selected_tools and shutil.which("creduce")) else None

    sqlancer_path = Path("/opt/sqlancer/sqlancer.jar")
    sqlancer_jar = sqlancer_path if ("sqlancer" in selected_tools and sqlancer_path.is_file() and sqlancer_path.stat().st_size > 1000) else None

    print("======================================================================")
    print("Multi-Tool Comparative Fuzz Benchmark Suite")
    print(f"Scenarios:    {len(metadata)} (50 ~ 200 statements)")
    print(f"Occam:        {occam_bin if occam_bin else 'NOT FOUND'}")
    print(f"ddmin:        {eval_bin if eval_bin else 'NOT FOUND'}")
    print(f"C-Reduce:     {creduce_bin if creduce_bin else 'SKIPPED (not installed)'}")
    print(f"SQLancer:     {sqlancer_jar if sqlancer_jar else 'SKIPPED (jar not present)'}")
    print(f"Output JSONL: {jsonl_path}")
    print("======================================================================\n")

    work_dir = Path(tempfile.mkdtemp(prefix="occam_benchmark_"))

    active_tools: list[str] = []
    if "occam" in selected_tools and occam_bin:
        active_tools.append("occam")
    if "ddmin" in selected_tools and eval_bin:
        active_tools.append("ddmin")
    if "creduce" in selected_tools and creduce_bin:
        active_tools.append("creduce")
    if "sqlancer" in selected_tools and sqlancer_jar:
        active_tools.append("sqlancer")

    if not active_tools:
        print("Error: No configured reduction tools are available to run.", file=sys.stderr)
        sys.exit(1)

    # Sort scenarios by statement count
    metadata.sort(key=lambda r: (r["statements"], r["dialect"], r["id"]))
    t_start = time.time()
    total_records = 0

    jsonl_path.parent.mkdir(parents=True, exist_ok=True)
    with jsonl_path.open("w", encoding="utf-8") as f_out:
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
            future_to_entry = {
                executor.submit(
                    run_scenario_tools,
                    entry,
                    root,
                    active_tools,
                    occam_bin,
                    eval_bin,
                    creduce_bin,
                    sqlancer_jar,
                    oracle_sh,
                    work_dir,
                    args.timeout,
                ): entry
                for entry in metadata
            }

            for future in concurrent.futures.as_completed(future_to_entry):
                entry = future_to_entry[future]
                try:
                    tool_results = future.result()
                    for rec in tool_results:
                        f_out.write(json.dumps(rec) + "\n")
                        f_out.flush()
                        total_records += 1
                except Exception as e:
                    print(f"[ERROR] Scenario {entry['id']} failed: {e}", file=sys.stderr)

    shutil.rmtree(work_dir, ignore_errors=True)
    total_elapsed = time.time() - t_start
    print(f"\nCompleted {total_records} benchmark evaluations in {total_elapsed:.2f}s.")
    print(f"Results streamed to: {jsonl_path}")


if __name__ == "__main__":
    main()

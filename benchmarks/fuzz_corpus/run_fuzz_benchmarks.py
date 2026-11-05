#!/usr/bin/env python3
"""Automated Benchmark Runner for Modern Fuzz Corpus.

Executes Occam vs B1 (Sequential Drop) vs B2 (Naive ddmin) across 18 modern
fuzz scenarios (50 ~ 200 statements) using occam-eval and fuzz_oracle.sh.
Generates comprehensive comparative markdown and CSV summary reports.
"""

from __future__ import annotations

import concurrent.futures
import json
import os
import subprocess
import sys
import time
from pathlib import Path
from typing import Any


def find_occam_eval(root: Path) -> Path:
    candidates = [
        root / "dist-newstyle" / "build" / "x86_64-linux" / "ghc-9.10.3" / "occam-0.1.0.0" / "x" / "occam-eval" / "build" / "occam-eval" / "occam-eval",
    ]
    for c in candidates:
        if c.exists() and os.access(c, os.X_OK):
            return c
    res = subprocess.run(["cabal", "list-bin", "occam-eval"], check=False, cwd=root, capture_output=True, text=True)
    if res.returncode == 0 and res.stdout.strip():
        p = Path(res.stdout.strip())
        if p.exists():
            return p
    raise FileNotFoundError("Could not find occam-eval executable. Run 'cabal build occam-eval' first.")


def run_single_benchmark(
    entry: dict[str, Any],
    root: Path,
    eval_bin: Path,
    oracle_sh: Path,
    results_dir: Path,
) -> dict[str, Any]:
    sid = entry["id"]
    sql_file = root / "benchmarks" / "fuzz_corpus" / entry["file"]
    out_dir = results_dir / sid
    out_dir.mkdir(parents=True, exist_ok=True)

    oracle_cmd = f"{oracle_sh} {sid}"
    cmd = [
        str(eval_bin),
        str(sql_file),
        "--oracle", oracle_cmd,
        "--out", str(out_dir),
        "--timeout", "15.0",
    ]

    t0 = time.time()
    res = subprocess.run(cmd, check=False, cwd=root, capture_output=True, text=True)
    elapsed = time.time() - t0

    benchmark_json = out_dir / "benchmark.json"
    metrics = []
    if benchmark_json.exists():
        try:
            metrics = json.loads(benchmark_json.read_text(encoding="utf-8"))
        except Exception:
            pass

    return {
        "scenario": sid,
        "entry": entry,
        "exit_code": res.returncode,
        "stdout": res.stdout,
        "stderr": res.stderr,
        "elapsed_total": elapsed,
        "metrics": metrics,
    }


def main() -> None:
    root = Path(__file__).resolve().parents[2]
    fuzz_dir = root / "benchmarks" / "fuzz_corpus"
    meta_path = fuzz_dir / "metadata.json"
    oracle_sh = fuzz_dir / "fuzz_oracle.sh"
    results_dir = fuzz_dir / "results"
    results_dir.mkdir(parents=True, exist_ok=True)

    if not meta_path.exists():
        print(f"Error: {meta_path} does not exist.")
        sys.exit(1)

    metadata = json.loads(meta_path.read_text(encoding="utf-8"))
    eval_bin = find_occam_eval(root)

    print("======================================================================")
    print("Occam vs Baselines: Modern Fuzz Benchmark Suite")
    print(f"Scenarios:    {len(metadata)} (50 ~ 200 statements)")
    print(f"Evaluator:    {eval_bin}")
    print(f"Oracle:       {oracle_sh}")
    print(f"Output Dir:   {results_dir}")
    print("======================================================================\n")

    t_start = time.time()
    results: list[dict[str, Any]] = []

    # Run in parallel using 4 worker threads
    max_workers = 4
    with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
        future_map = {
            executor.submit(run_single_benchmark, entry, root, eval_bin, oracle_sh, results_dir): entry
            for entry in metadata
        }
        for future in concurrent.futures.as_completed(future_map):
            entry = future_map[future]
            try:
                res = future.result()
                results.append(res)
                sid = entry["id"]
                status = "PASS" if res["exit_code"] == 0 else "FAIL"
                print(f"[{status}] {sid:22s} ({entry['statements']:3d} stmts, {entry['dialect']:8s}) - {res['elapsed_total']:.2f}s")
            except Exception as e:
                print(f"[ERROR] {entry['id']}: {e}")

    results.sort(key=lambda r: (r["entry"]["statements"], r["entry"]["dialect"], r["scenario"]))
    total_time = time.time() - t_start
    print(f"\nAll {len(results)} benchmarks finished in {total_time:.2f}s.\n")

    jsonl_path = root / "benchmarks" / "fuzz_benchmark_results.jsonl"
    with jsonl_path.open("w", encoding="utf-8") as f:
        for r in results:
            entry = r["entry"]
            sid = r["scenario"]
            dialect = entry["dialect"].capitalize()
            scale = f"{entry['statements']} stmts"
            init_s = entry["statements"]
            init_b = entry["bytes"]

            for m in r["metrics"]:
                alg = m.get("metricAlgorithm", "")
                red_s = m.get("metricReducedStmts", 0)
                red_b = m.get("metricReducedBytes", 0)
                s_red = m.get("metricStmtReduction", 0.0)
                b_red = m.get("metricByteReduction", 0.0)
                calls = m.get("metricOracleCalls", 0)
                sec = m.get("metricTimeSeconds", 0.0)

                record = {
                    "scenario": sid,
                    "dialect": dialect,
                    "scale": scale,
                    "initial_stmts": init_s,
                    "algorithm": alg,
                    "reduced_stmts": red_s,
                    "stmt_reduction_pct": round(s_red, 2),
                    "initial_bytes": init_b,
                    "reduced_bytes": red_b,
                    "byte_reduction_pct": round(b_red, 2),
                    "oracle_calls": calls,
                    "time_seconds": round(sec, 4),
                }
                f.write(json.dumps(record) + "\n")

    print(f"Wrote benchmark results JSONL:    {jsonl_path}")


if __name__ == "__main__":
    main()

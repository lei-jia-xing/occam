#!/usr/bin/env python3
"""
scripts/run_real_benchmarks.py - Automated comparative benchmark evaluator for Occam.

Evaluates Occam vs baselines on authentic DBMS fuzzer crash cases:
  - Ours (no-intra / full): Dependency slicing + closure ddmin
  - B1 (sequential): 1-by-1 linear statement deletion audit
  - B2 (statement-ddmin): Classic statement-level delta debugging
  - Ours-NoDep (graph-off): Delta debugging without semantic dependency DAG

Performs 5/5 clean-state replay verification on the final minimal SQL.
Outputs standardized JSON and Markdown reports into benchmarks/real_fuzz_corpus/.
"""

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path


def find_eval_binary(workspace_root: Path) -> Path:
    # 1. Try cabal list-bin
    try:
        res = subprocess.run(
            ["cabal", "list-bin", "exe:occam-eval"],
            cwd=workspace_root,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=10,
        )
        if res.returncode == 0:
            p = Path(res.stdout.strip())
            if p.is_file():
                return p
    except Exception:
        pass

    # 2. Check standard dist-newstyle path
    for p in workspace_root.glob("dist-newstyle/**/occam-eval"):
        if p.is_file() and os.access(p, os.X_OK):
            return p

    raise FileNotFoundError("occam-eval binary not found. Run 'cabal build exe:occam-eval'.")


def count_sql_statements(sql_file: Path) -> int:
    text = sql_file.read_text(errors="replace")
    # Clean single line and block comments
    cleaned = re.sub(r"--[^\n]*", "", text)
    cleaned = re.sub(r"/\*.*?\*/", "", cleaned, flags=re.DOTALL)
    stmts = [s.strip() for s in cleaned.split(";") if s.strip()]
    return len(stmts)


def run_benchmark_case(
    case_meta_path: Path,
    eval_binary: Path,
    workspace_root: Path,
    profiles: list[str],
    output_dir: Path,
    skip_existing: bool = False,
) -> dict:
    meta = json.loads(case_meta_path.read_text())
    case_id = meta["id"]
    sql_file = case_meta_path.parent / meta["file"]
    dialect = meta.get("dialect", "generic")
    target_info = meta.get("target", {})
    dbms = target_info.get("dbms", "postgres")
    expected_sig = meta.get("crash_signature", {}).get("expected_signature", "")

    if dbms == "mysql":
        container_name = "occam-target-mysql80"
    elif dbms == "postgres":
        container_name = "occam-target-pg12"
    else:
        container_name = ""

    print(f"\n=======================================================")
    print(f"Benchmark Case: {case_id} (DBMS: {dbms}, Dialect: {dialect})")
    print(f"Input: {sql_file.name} ({sql_file.stat().st_size} bytes, {count_sql_statements(sql_file)} stmts)")
    print(f"Container: {container_name}")
    print(f"Expected Signature: {expected_sig}")
    print(f"=======================================================\n")

    oracle_cmd = (
        f"python3 harness/crash_oracle.py --dbms {dbms} --dbname occam_test "
        f"--container-name {container_name} --expected-signature \"{expected_sig}\""
    )
    reset_cmd = f"bash harness/reset_db.sh --dbms {dbms} --dbname occam_test --container-name {container_name}"

    # Initial sanity check on input SQL
    print("[benchmark] Checking initial crash interestingness...")
    subprocess.run(reset_cmd, shell=True, check=True, cwd=workspace_root)
    chk = subprocess.run(
        oracle_cmd,
        shell=True,
        stdin=open(sql_file, "rb"),
        cwd=workspace_root,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    if chk.returncode != 0:
        raise RuntimeError(f"Initial input SQL failed oracle check (exit {chk.returncode}):\n{chk.stderr}")
    print("  -> Initial input crashed as expected (PASS)\n")

    initial_bytes = sql_file.stat().st_size
    initial_stmts = count_sql_statements(sql_file)

    results = {
        "case_id": case_id,
        "input": {
            "path": str(sql_file.relative_to(workspace_root)),
            "bytes": initial_bytes,
            "statements": initial_stmts,
        },
        "target": {
            "dbms": target_info.get("dbms", dbms),
            "version": target_info.get("version", ""),
            "container": container_name,
            "expected_signature": expected_sig,
        },
        "limits": {
            "oracle_timeout_seconds": 15,
            "budget_seconds": 300,
            "max_oracle_executions": 200,
        },
        "profiles": {},
        "replay_verification": {},
    }

    case_out_dir = output_dir / case_id
    case_out_dir.mkdir(parents=True, exist_ok=True)

    minimal_outputs = {}

    for prof in profiles:
        print(f"--- Running Profile: {prof} ---")
        prof_out = case_out_dir / prof
        prof_out.mkdir(parents=True, exist_ok=True)
        stats_file = prof_out / "benchmark.json"
        minimal_file = prof_out / "minimal.sql"
        wall_time = 0.0

        if skip_existing and stats_file.is_file() and minimal_file.is_file():
            print(f"  [INFO] Reusing existing results for {prof}")
        else:
            cmd = [
                str(eval_binary),
                str(sql_file),
                "--oracle", oracle_cmd,
                "--reset", reset_cmd,
                "--profile", prof,
                "--dialect", dialect,
                "--out", str(prof_out),
                "--timeout", "15.0",
                "--budget", "300.0",
                "--max-oracle-executions", "200",
            ]

            t0 = time.time()
            res = subprocess.run(cmd, cwd=workspace_root, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            wall_time = time.time() - t0

            if res.returncode != 0:
                print(f"  [ERROR] Profile {prof} failed with exit {res.returncode}:\n{res.stderr}")
                continue

        if not stats_file.is_file():
            stats_file = prof_out / "profile.json"
        if not stats_file.is_file():
            stats_file = prof_out / "stats.json"

        if not stats_file.is_file():
            print(f"  [WARN] stats/benchmark.json missing for {prof}")
            continue

        raw_content = json.loads(stats_file.read_text())
        if isinstance(raw_content, list):
            raw_stats = raw_content[0]
        else:
            raw_stats = raw_content

        # Handle both BenchmarkMetric format and standard stats.json format
        if "metricReducedBytes" in raw_stats:
            reduced_bytes = raw_stats["metricReducedBytes"]
            reduced_stmts = raw_stats["metricReducedStmts"]
            byte_pct = raw_stats["metricByteReduction"]
            stmt_pct = raw_stats["metricStmtReduction"]
            internal_secs = raw_stats["metricTimeSeconds"]
            ev_metrics = raw_stats.get("metricEvaluationMetrics", {})
            cache_hits = ev_metrics.get("evaluationCacheHits", 0)
            cache_misses = ev_metrics.get("evaluationCacheMisses", 0)
        else:
            reduced_bytes = raw_stats.get("reduced_bytes", initial_bytes)
            reduced_stmts = raw_stats.get("reduced_statements", initial_stmts)
            byte_pct = ((initial_bytes - reduced_bytes) / initial_bytes * 100.0) if initial_bytes else 0.0
            stmt_pct = ((initial_stmts - reduced_stmts) / initial_stmts * 100.0) if initial_stmts else 0.0
            internal_secs = raw_stats.get("elapsed_seconds", 0.0)
            cache_hits = raw_stats.get("cache_hits", 0)
            cache_misses = raw_stats.get("cache_misses", 0)

        total_evals = cache_hits + cache_misses
        hit_ratio = (cache_hits / total_evals * 100.0) if total_evals > 0 else 0.0
        if wall_time == 0.0:
            wall_time = internal_secs

        results["profiles"][prof] = {
            "initial_bytes": initial_bytes,
            "reduced_bytes": reduced_bytes,
            "byte_reduction_pct": byte_pct,
            "initial_statements": initial_stmts,
            "reduced_statements": reduced_stmts,
            "statement_reduction_pct": stmt_pct,
            "oracle_calls_misses": cache_misses,
            "cache_hits": cache_hits,
            "cache_hit_ratio_pct": hit_ratio,
            "wall_time_seconds": round(wall_time, 2),
            "internal_reduction_seconds": raw_stats.get("elapsed_seconds", 0.0),
            "output_sql": str(minimal_file),
        }

        minimal_outputs[prof] = minimal_file
        print(f"  -> Reduced: {initial_stmts} -> {reduced_stmts} stmts, {initial_bytes} -> {reduced_bytes} bytes (-{byte_pct:.1f}%) in {wall_time:.1f}s")

    # 5/5 Replay Verification on the primary Occam output
    primary_prof = "no-intra" if "no-intra" in minimal_outputs else ("full" if "full" in minimal_outputs else list(minimal_outputs.keys())[0])
    best_minimal = minimal_outputs.get(primary_prof)

    if best_minimal and best_minimal.is_file():
        print(f"\n[benchmark] Performing 5/5 Replay Verification on primary output ({primary_prof}): {best_minimal}")
        replays = []
        for i in range(1, 6):
            subprocess.run(reset_cmd, shell=True, check=True, cwd=workspace_root, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            t_rep = subprocess.run(
                oracle_cmd,
                shell=True,
                stdin=open(best_minimal, "rb"),
                cwd=workspace_root,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
            passed = (t_rep.returncode == 0)
            replays.append({"replay": i, "exit_code": t_rep.returncode, "status": "PASSED" if passed else "FAILED"})
            print(f"  Replay {i}/5: {'PASS' if passed else 'FAIL'} (exit {t_rep.returncode})")

        all_passed = all(r["status"] == "PASSED" for r in replays)
        results["replay_verification"] = {
            "target_profile": primary_prof,
            "status": "PASSED_5_OF_5" if all_passed else "REPLAY_FAILED",
            "replays": replays,
        }

    # Write results JSON
    json_path = case_meta_path.parent / f"{case_id}_results.json"
    json_path.write_text(json.dumps(results, indent=2))
    print(f"\n[benchmark] Saved results JSON to {json_path}")

    # Generate Markdown summary
    md_path = case_meta_path.parent / f"{case_id}_results.md"
    generate_markdown_report(results, md_path)
    print(f"[benchmark] Saved results Markdown to {md_path}\n")

    return results


def generate_markdown_report(results: dict, md_path: Path):
    c_id = results["case_id"]
    inp = results["input"]
    target = results["target"]
    profs = results["profiles"]
    replay = results.get("replay_verification", {})

    lines = [
        f"# {c_id} Comparative Reduction Results",
        "",
        f"**Input:** `{inp['path']}` — {inp['bytes']} bytes, {inp['statements']} statements.",
        f"**Target DBMS:** {target.get('dbms', '')} ({target.get('container', '')}).",
        f"**Observed Crash Signature:** `{target.get('expected_signature', '')}`.",
        "",
        "## Comparative Evaluation Across Reduction Profiles",
        "",
        "| Profile | Initial (B / stmts) | Reduced (B / stmts) | Byte Reduction | Stmt Reduction | Oracle Calls (Misses) | Cache Hits | Cache Hit Ratio | Wall Time (s) |",
        "|---|---:|---:|---:|---:|---:|---:|---:|---:|",
    ]

    profile_labels = {
        "full": "Ours-Full (`full`)",
        "no-intra": "Ours-Statement (`no-intra`)",
        "sequential": "B1 (`sequential`)",
        "statement-ddmin": "B2 (`statement-ddmin`)",
        "graph-off": "Ours-NoDep (`graph-off`)",
        "no-closure": "Ablation (`no-closure`)",
        "no-slicing": "Ablation (`no-slicing`)",
    }

    for p_name, p_data in profs.items():
        label = profile_labels.get(p_name, p_name)
        lines.append(
            f"| {label} | {p_data['initial_bytes']} / {p_data['initial_statements']} | "
            f"{p_data['reduced_bytes']} / {p_data['reduced_statements']} | "
            f"{p_data['byte_reduction_pct']:.2f}% | {p_data['statement_reduction_pct']:.2f}% | "
            f"{p_data['oracle_calls_misses']} | {p_data['cache_hits']} | "
            f"{p_data['cache_hit_ratio_pct']:.2f}% | {p_data['wall_time_seconds']:.2f} |"
        )

    lines.extend([
        "",
        "## Clean-State 5/5 Replay Verification",
        "",
        f"Target Profile: `{replay.get('target_profile', '')}` — Status: **{replay.get('status', '')}**",
        "",
        "| Replay | Exit Code | Result |",
        "|---:|---:|---|",
    ])

    for r in replay.get("replays", []):
        lines.append(f"| {r['replay']} | {r['exit_code']} | {r['status']} |")

    lines.append("")
    md_path.write_text("\n".join(lines))


def main():
    parser = argparse.ArgumentParser(description="Occam Real Fuzz Benchmark Evaluator")
    parser.add_argument("--case", default="mysql_squirrel_bug99421_enhanced", help="Benchmark case name")
    parser.add_argument("--profiles", default="no-intra,sequential,statement-ddmin,graph-off", help="Comma-separated profile list")
    parser.add_argument("--out-dir", default="occam-out/benchmarks", help="Output directory")
    parser.add_argument("--skip-existing", action="store_true", help="Reuse existing profile artifacts if present")

    args = parser.parse_args()
    workspace_root = Path(__file__).resolve().parent.parent
    eval_binary = find_eval_binary(workspace_root)
    corpus_dir = workspace_root / "benchmarks" / "real_fuzz_corpus"

    meta_file = corpus_dir / f"{args.case}.meta.json"
    if not meta_file.is_file():
        sys.exit(f"Case metadata not found: {meta_file}")

    profiles = [p.strip() for p in args.profiles.split(",") if p.strip()]
    output_dir = workspace_root / args.out_dir

    run_benchmark_case(
        case_meta_path=meta_file,
        eval_binary=eval_binary,
        workspace_root=workspace_root,
        profiles=profiles,
        output_dir=output_dir,
        skip_existing=args.skip_existing,
    )


if __name__ == "__main__":
    main()

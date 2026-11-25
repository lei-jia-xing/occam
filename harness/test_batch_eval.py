#!/usr/bin/env python3
"""Unit and integration tests for harness/batch_eval.py."""  # noqa: EXE001

import json
import os
import subprocess
import sys
import tempfile
import unittest

HARNESS_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(HARNESS_DIR)
REAL_CORPUS_DIR = os.path.join(REPO_ROOT, "benchmarks", "real_fuzz_corpus")
BATCH_EVAL = os.path.join(HARNESS_DIR, "batch_eval.py")

if HARNESS_DIR not in sys.path:
    sys.path.insert(0, HARNESS_DIR)

import batch_eval


def _make_sample_stats(
    status="completed",
    initial_bytes=200,
    reduced_bytes=50,
    initial_stmts=6,
    reduced_stmts=2,
    attempts=10,
    executions=8,
    elapsed=1.5,
    validation_passed=True,
    minimality="statement-deletion-1-minimal",
    curve=None,
    cost_attr=None,
):
    if curve is None:
        curve = [
            {
                "point_elapsed_seconds": 0.1,
                "point_call_index": 1,
                "point_candidate_bytes": 150,
                "point_candidate_statements": 4,
                "point_cache_hit": False,
                "point_accepted": True,
            },
            {
                "point_elapsed_seconds": 0.3,
                "point_call_index": 5,
                "point_candidate_bytes": reduced_bytes,
                "point_candidate_statements": reduced_stmts,
                "point_cache_hit": False,
                "point_accepted": True,
            },
        ]
    if cost_attr is None:
        cost_attr = {
            "snapshot_policy": {
                "metric_search_policy": "full",
                "metric_column_mode": "off",
                "metric_jobs": 1,
            },
            "snapshot_families": [
                {
                    "tag": {"kind": "statement"},
                    "counters": {
                        "physical_starts": 5,
                        "retired_accept": 2,
                        "retired_reject": 3,
                        "retired_inconclusive": 0,
                        "physical_completions": 5,
                        "completed_unselected": 0,
                        "completed_unselected_interaction_wall_micros": 0,
                        "canceled": 0,
                        "canceled_interaction_wall_micros": 0,
                        "failed": 0,
                        "failed_interaction_wall_micros": 0,
                        "candidate_build_micros": 120,
                    },
                }
            ],
            "snapshot_generated_skipped": {},
            "snapshot_parse_ffi_micros": {"statement-only": 300},
            "snapshot_parse_decode_micros": {"statement-only": 80},
        }
    return {
        "initial_bytes": initial_bytes,
        "reduced_bytes": reduced_bytes,
        "reduction_ratio": ((initial_bytes - reduced_bytes) / float(initial_bytes)) * 100.0,
        "initial_statements": initial_stmts,
        "reduced_statements": reduced_stmts,
        "oracle_executions": executions,
        "oracle_attempts": attempts,
        "initial_oracle_observations": 1,
        "cache_hits": max(0, attempts - executions),
        "cache_misses": executions,
        "cache_joins": 0,
        "cache_hit_ratio": (
            max(0, attempts - executions) / float(attempts) if attempts > 0 else 0.0
        ),
        "cancelled_observations": 0,
        "status": status,
        "reduction_settings": {},
        "evaluation_budget": {
            "evaluation_cache_enabled": True,
            "evaluation_jobs": 1,
            "evaluation_total_timeout_micros": None,
            "evaluation_max_oracle_executions": None,
            "evaluation_preflight_micros": 1000,
            "evaluation_preflight_attempts": 1,
        },
        "evaluation_metrics": {
            "evaluation_status": status,
            "evaluation_failure": None,
            "evaluation_cache_used": True,
            "evaluation_total_seconds": elapsed,
            "evaluation_reduction_seconds": elapsed * 0.8,
            "evaluation_oracle_seconds": elapsed * 0.5,
            "evaluation_reset_seconds": elapsed * 0.2,
            "evaluation_oracle_attempts": max(0, attempts - 3),
            "evaluation_database_observations": max(0, executions - 3),
            "evaluation_completed_oracle_processes": max(0, executions - 3),
            "evaluation_accepted_observations": 2,
            "evaluation_rejected_observations": 3,
            "evaluation_timed_out_observations": 0,
            "evaluation_reset_failures": 0,
            "evaluation_oracle_errors": 0,
            "evaluation_cancelled_observations": 0,
            "evaluation_cache_hits": max(0, attempts - executions),
            "evaluation_cache_misses": executions,
            "evaluation_cache_joins": 0,
            "evaluation_validation_seconds": 0.1,
            "evaluation_validation_oracle_seconds": 0.05,
            "evaluation_validation_reset_seconds": 0.04,
            "evaluation_validation_observations": 2 if validation_passed else 0,
            "evaluation_validation_database_observations": 2 if validation_passed else 0,
            "evaluation_validation_errors": 0,
            "evaluation_validation_passed": validation_passed,
            "evaluation_audit_seconds": 0.05,
            "evaluation_audit_checks": 2,
            "evaluation_audit_completed": minimality == "statement-deletion-1-minimal",
            "evaluation_audit_skipped": False,
            "evaluation_minimality_claim": minimality,
            "evaluation_curve": curve,
            "evaluation_stages": [],
            "evaluation_cost_attribution": cost_attr,
        },
        "initial_oracle_seconds": 0.05,
        "elapsed_seconds": elapsed,
    }


class TestBatchEval(unittest.TestCase):
    def _write_exec(self, directory, name, body):
        path = os.path.join(directory, name)
        with open(path, "w", encoding="utf-8") as handle:
            handle.write("#!/bin/sh\n" + body)
        os.chmod(path, 0o755)
        return path

    def test_parse_profiles_and_auc_helpers(self):
        profs = batch_eval.parse_profiles(
            "audit-only,ddmin-only,ours-s,Ours-SI,ours-full,Ours-NoDep,custom",
            custom_specs=["custom=--strategy full --intra-statement --ast-batching"],
        )
        names = [p["name"] for p in profs]
        self.assertEqual(
            names,
            [
                "audit-only",
                "ddmin-only",
                "Ours-S",
                "Ours-SI",
                "Ours-Full",
                "Ours-NoDep",
                "custom",
            ],
        )
        self.assertIn("--column-mode", profs[4]["flags"])
        self.assertIn("atomic", profs[4]["flags"])

        # Stepwise AUC check: initial=100, at call 1->50, total=4 -> (50+50+50+50)/(100*4) = 0.5
        auc = batch_eval.compute_attempt_auc(
            100,
            [{"point_call_index": 1, "point_candidate_bytes": 50, "point_accepted": True}],
            4,
        )
        self.assertAlmostEqual(auc, 0.5, places=6)

        stats = _make_sample_stats()
        self.assertEqual(
            batch_eval.compute_engine_overhead_us(
                stats["evaluation_metrics"]["evaluation_cost_attribution"]
            ),
            300 + 80 + 120,
        )

    def test_cli_validation_contracts_and_dry_run_real_corpus(self):
        with tempfile.TemporaryDirectory() as td:
            # 1. --occam-jobs > 1 on server DBMS must fail fast (exit 2)
            r_srv = subprocess.run(
                [
                    sys.executable,
                    BATCH_EVAL,
                    "--corpus-dir",
                    REAL_CORPUS_DIR,
                    "--dbms",
                    "postgres",
                    "--occam-jobs",
                    "2",
                    "--out-dir",
                    os.path.join(td, "r1"),
                    "--dry-run",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(r_srv.returncode, 2)
            self.assertIn("--occam-jobs > 1", r_srv.stderr)

            # 2. --occam-jobs > 1 IS allowed on embedded SQLite
            r_emb = subprocess.run(
                [
                    sys.executable,
                    BATCH_EVAL,
                    "--corpus-dir",
                    REAL_CORPUS_DIR,
                    "--dbms",
                    "sqlite",
                    "--occam-jobs",
                    "4",
                    "--out-dir",
                    os.path.join(td, "r2"),
                    "--dry-run",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(r_emb.returncode, 0, r_emb.stderr)

            # 3. --jobs 2 with only 1 --containers on server DBMS fails fast (exit 2)
            r_cnt = subprocess.run(
                [
                    sys.executable,
                    BATCH_EVAL,
                    "--corpus-dir",
                    REAL_CORPUS_DIR,
                    "--dbms",
                    "postgres",
                    "--containers",
                    "pg_only_one",
                    "--jobs",
                    "2",
                    "--out-dir",
                    os.path.join(td, "r3"),
                    "--dry-run",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(r_cnt.returncode, 2)
            self.assertIn("isolated containers", r_cnt.stderr)

            # 4. Full dry-run across all 71 real_fuzz_corpus cases x 6 profiles x 3 repeats
            plan_dir = os.path.join(td, "full_plan")
            r_dry = subprocess.run(
                [
                    sys.executable,
                    BATCH_EVAL,
                    "--corpus-dir",
                    REAL_CORPUS_DIR,
                    "--profiles",
                    "audit-only,ddmin-only,Ours-S,Ours-SI,Ours-Full,Ours-NoDep",
                    "--repeats",
                    "3",
                    "--jobs",
                    "4",
                    "--out-dir",
                    plan_dir,
                    "--dry-run",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(r_dry.returncode, 0, r_dry.stderr)
            with open(os.path.join(plan_dir, "plan.json"), encoding="utf-8") as handle:
                plan = json.load(handle)
            self.assertEqual(plan["case_count"], 71)
            self.assertEqual(plan["total_jobs"], 1278)
            self.assertEqual(plan["jobs_workers"], 4)
            self.assertEqual(
                {c["dbms"] for c in plan["cases"]},
                {"postgres", "mysql", "mariadb", "clickhouse", "sqlite", "duckdb"},
            )

    def test_container_per_worker_parallel_batch_and_5replay_slippage_and_resume(self):
        """
        Tests full orchestrator flow with --jobs 2 using mocked docker + psql + occam:
          - Spawns 2 distinct managed containers (w0, w1) with distinct mapped ports
          - Qualifies 3 cases via 5-replay check:
              * case_good: 5/5 consistent assertion signature -> passes & reduces cleanly
              * case_slip: 5/5 input passes, but reduced SQL slips to a different assertion
              * case_flaky: 1/5 input crashes then stops -> marked skipped_unstable_input
          - Verifies container cleanup, resumability, and aggregated_stats.json stratification
        """
        with tempfile.TemporaryDirectory() as td:
            bin_dir = os.path.join(td, "bin")
            corpus_dir = os.path.join(td, "corpus")
            state_dir = os.path.join(td, "mock_state")
            out_dir = os.path.join(td, "out")
            os.makedirs(bin_dir)
            os.makedirs(corpus_dir)
            os.makedirs(state_dir)

            # Create 3 synthetic SQLite/Postgres cases (use postgres to test managed ports + container logs)
            for cid, body in (
                ("case_good", "CREATE TABLE t0(a INT);\nSELECT 'CRASH_GOOD';\n"),
                ("case_slip", "CREATE TABLE t1(a INT);\nSELECT 'CRASH_SLIP_IN';\n"),
                ("case_flaky", "CREATE TABLE t2(a INT);\nSELECT 'CRASH_FLAKY';\n"),
            ):
                with open(os.path.join(corpus_dir, f"{cid}.sql"), "w", encoding="utf-8") as h:
                    h.write(body)
                with open(os.path.join(corpus_dir, f"{cid}.meta.json"), "w", encoding="utf-8") as h:
                    json.dump(
                        {
                            "id": cid,
                            "target": {"dbms": "postgres"},
                            "crash_signature": {"expected_signature": "FailedAssertion"},
                        },
                        h,
                    )

            docker_log = os.path.join(state_dir, "docker_ops.log")
            flaky_flag = os.path.join(state_dir, "flaky_seen")

            # Mock docker: tracks run/port/exec/logs/rm per container name
            self._write_exec(
                bin_dir,
                "docker",
                f"""set -e
cmd="$1"; shift
case "$cmd" in
  run)
    cname=""
    while [ $# -gt 0 ]; do
      if [ "$1" = "--name" ]; then cname="$2"; shift 2; else shift; fi
    done
    printf 'run %s\\n' "$cname" >> {docker_log}
    touch "{state_dir}/${{cname}}.running"
    : > "{state_dir}/${{cname}}.log"
    ;;
  port)
    cname="$1"
    case "$cname" in
      *-w0) echo "127.0.0.1:15432" ;;
      *-w1) echo "127.0.0.1:15433" ;;
      *) echo "127.0.0.1:15439" ;;
    esac
    ;;
  ps)
    for f in {state_dir}/*.running; do
      [ -e "$f" ] || continue
      basename "$f" .running
    done
    ;;
  exec)
    if [ "$1" = "-i" ]; then shift; fi
    cname="$1"; shift
    op="$1"; shift
    if [ "$op" = "stat" ]; then
      sz=$(wc -c < "{state_dir}/${{cname}}.log" | tr -d ' ')
      echo "$sz 1001"
    elif [ "$op" = "tail" ]; then
      # tail -c +N <file>
      shift
      byte_arg="$1"
      tail -c "$byte_arg" "{state_dir}/${{cname}}.log"
    fi
    ;;
  logs)
    exit 0
    ;;
  rm)
    for a in "$@"; do
      if [ "$a" != "-f" ]; then
        printf 'rm %s\\n' "$a" >> {docker_log}
        rm -f "{state_dir}/${{a}}.running"
      fi
    done
    ;;
esac
""",
            )
            self._write_exec(bin_dir, "pg_isready", "exit 0\n")

            # Mock psql: routes log output to the container corresponding to -p port
            self._write_exec(
                bin_dir,
                "psql",
                f"""port="5432"
prev=""
for a in "$@"; do
  if [ "$prev" = "-p" ]; then port="$a"; fi
  prev="$a"
done
cname=""
for f in {state_dir}/*.running; do
  [ -e "$f" ] || continue
  bn=$(basename "$f" .running)
  case "$port:$bn" in
    15432:*-w0) cname="$bn" ;;
    15433:*-w1) cname="$bn" ;;
  esac
done
sql="$(cat)"
[ -n "$cname" ] || exit 0
case "$sql" in
  *CRASH_GOOD*)
    echo '2026-11-23 00:00:00 UTC [10] TRAP: FailedAssertion("!(good_expr)", File: "good.c", Line: 10)' >> "{state_dir}/${{cname}}.log"
    exit 2
    ;;
  *CRASH_SLIP_IN*)
    echo '2026-11-23 00:00:00 UTC [10] TRAP: FailedAssertion("!(orig_expr)", File: "orig.c", Line: 20)' >> "{state_dir}/${{cname}}.log"
    exit 2
    ;;
  *CRASH_SLIP_OUT*)
    echo '2026-11-23 00:00:00 UTC [10] TRAP: FailedAssertion("!(slipped_expr)", File: "slip.c", Line: 99)' >> "{state_dir}/${{cname}}.log"
    exit 2
    ;;
  *CRASH_FLAKY*)
    if [ ! -f {flaky_flag} ]; then
      touch {flaky_flag}
      echo '2026-11-23 00:00:00 UTC [10] TRAP: FailedAssertion("!(flaky_expr)", File: "flaky.c", Line: 30)' >> "{state_dir}/${{cname}}.log"
      exit 2
    fi
    exit 0
    ;;
esac
exit 0
""",
            )

            # Mock occam: runs oracle once (to populate oracle_decisions.jsonl), writes minimal.sql and stats.json
            mock_occam = self._write_exec(
                bin_dir,
                "occam",
                f"""#!/bin/sh
set -e
input="$1"; shift
oracle_cmd=""
reset_cmd=""
out_path=""
stats_path=""
while [ $# -gt 0 ]; do
  case "$1" in
    --oracle) oracle_cmd="$2"; shift 2 ;;
    --reset) reset_cmd="$2"; shift 2 ;;
    --out) out_path="$2"; shift 2 ;;
    --stats) stats_path="$2"; shift 2 ;;
    *) shift ;;
  esac
done
sh -c "$reset_cmd"
sh -c "$oracle_cmd" < "$input" >/dev/null 2>&1 || true
if grep -q "CRASH_SLIP_IN" "$input"; then
  printf "SELECT 'CRASH_SLIP_OUT';\\n" > "$out_path"
else
  printf "SELECT 'CRASH_GOOD';\\n" > "$out_path"
fi
python3 - "$input" "$out_path" "$stats_path" <<'PY'
import json, os, sys
sys.path.insert(0, {HARNESS_DIR!r})
sys.path.insert(0, {os.path.join(HARNESS_DIR)!r})
inp_sz = os.path.getsize(sys.argv[1])
out_sz = os.path.getsize(sys.argv[2])
stats = {{
    "initial_bytes": inp_sz,
    "reduced_bytes": out_sz,
    "reduction_ratio": ((inp_sz - out_sz) / float(inp_sz)) * 100.0,
    "initial_statements": 2,
    "reduced_statements": 1,
    "oracle_executions": 4,
    "oracle_attempts": 5,
    "initial_oracle_observations": 1,
    "cache_hits": 1,
    "cache_misses": 4,
    "cache_joins": 0,
    "cache_hit_ratio": 0.2,
    "cancelled_observations": 0,
    "status": "completed",
    "reduction_settings": {{}},
    "evaluation_budget": {{"evaluation_jobs": 1}},
    "evaluation_metrics": {{
        "evaluation_status": "completed",
        "evaluation_oracle_seconds": 0.2,
        "evaluation_reset_seconds": 0.1,
        "evaluation_audit_seconds": 0.05,
        "evaluation_validation_seconds": 0.05,
        "evaluation_validation_passed": True,
        "evaluation_minimality_claim": "statement-deletion-1-minimal",
        "evaluation_curve": [
            {{"point_call_index": 1, "point_candidate_bytes": out_sz, "point_accepted": True}}
        ],
    }},
    "initial_oracle_seconds": 0.02,
    "elapsed_seconds": 0.4,
}}
with open(sys.argv[3], "w") as h:
    json.dump(stats, h)
PY
""",
            )

            env = os.environ.copy()
            env["PATH"] = f"{bin_dir}:{env.get('PATH', '')}"

            res = subprocess.run(
                [
                    sys.executable,
                    BATCH_EVAL,
                    "--corpus-dir",
                    corpus_dir,
                    "--profiles",
                    "Ours-S",
                    "--repeats",
                    "1",
                    "--jobs",
                    "2",
                    "--occam-bin",
                    mock_occam,
                    "--out-dir",
                    out_dir,
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res.returncode, 0, res.stderr + "\n" + res.stdout)

            # Verify 2 distinct managed containers were created and cleaned up
            with open(docker_log, encoding="utf-8") as h:
                dops = h.read()
            self.assertIn("-w0", dops)
            self.assertIn("-w1", dops)
            self.assertIn("rm ", dops)
            self.assertEqual([f for f in os.listdir(state_dir) if f.endswith(".running")], [])

            agg_path = os.path.join(out_dir, "aggregated_stats.json")
            self.assertTrue(os.path.isfile(agg_path))
            self.assertTrue(os.path.isfile(os.path.join(out_dir, "summary.md")))
            with open(agg_path, encoding="utf-8") as h:
                agg = json.load(h)

            self.assertEqual(agg["overview"]["qualified_cases_evaluated"], 3)
            self.assertEqual(agg["overview"]["input_stable_cases"], 2)
            self.assertEqual(agg["overview"]["input_unstable_cases"], 1)
            self.assertEqual(agg["overview"]["unique_qualified_crash_signatures"], 2)

            prof_agg = agg["by_profile"]["Ours-S"]
            self.assertEqual(prof_agg["total_runs"], 3)
            self.assertEqual(prof_agg["completed_runs"], 2)
            self.assertEqual(prof_agg["skipped_unstable_runs"], 1)
            self.assertEqual(prof_agg["reduced_replay_5of5_runs"], 2)
            self.assertEqual(prof_agg["reduced_replay_zero_slippage_runs"], 1)
            self.assertEqual(prof_agg["reduced_replay_slipped_runs"], 1)

            # Verify resuming immediately skips everything without touching docker again!
            os.unlink(docker_log)
            res_resume = subprocess.run(
                [
                    sys.executable,
                    BATCH_EVAL,
                    "--corpus-dir",
                    corpus_dir,
                    "--profiles",
                    "Ours-S",
                    "--repeats",
                    "1",
                    "--jobs",
                    "2",
                    "--occam-bin",
                    mock_occam,
                    "--out-dir",
                    out_dir,
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res_resume.returncode, 0, res_resume.stderr)
            self.assertFalse(os.path.exists(docker_log))

    def test_resumability_cleans_partial_and_respects_rerun_failed(self):
        with tempfile.TemporaryDirectory() as td:
            out_dir = os.path.join(td, "out")
            corpus_dir = os.path.join(td, "corpus")
            os.makedirs(corpus_dir)
            sql_path = os.path.join(corpus_dir, "c1.sql")
            with open(sql_path, "wb") as h:
                h.write(b"SELECT 1;\nSELECT 2;\n")
            with open(sql_path, "rb") as h:
                sha = batch_eval.hashlib.sha256(h.read()).hexdigest()

            job_completed = {
                "job_id": "c1__Ours-S__r1",
                "case": {"sql_sha256": sha},
                "profile_hash": batch_eval._profile_signature_hash(
                    batch_eval.BUILTIN_PROFILES["ours-s"][1]
                ),
            }
            dir_completed = os.path.join(out_dir, "runs", "c1__Ours-S__r1")
            os.makedirs(dir_completed)
            stats_path = os.path.join(dir_completed, "stats.json")
            with open(stats_path, "w", encoding="utf-8") as h:
                json.dump(_make_sample_stats(), h)
            with open(os.path.join(dir_completed, "job_result.json"), "w", encoding="utf-8") as h:
                json.dump(
                    {
                        "job_id": "c1__Ours-S__r1",
                        "sql_sha256": sha,
                        "profile_hash": job_completed["profile_hash"],
                        "state": "completed",
                        "ran_occam": True,
                        "stats_path": stats_path,
                    },
                    h,
                )

            # Completed job is skipped on resume
            self.assertIsNotNone(
                batch_eval._is_valid_completed_job(
                    os.path.join(dir_completed, "job_result.json"),
                    job_completed,
                    rerun_failed=False,
                )
            )

            # Error job is skipped unless rerun_failed=True
            with open(os.path.join(dir_completed, "job_result.json"), "w", encoding="utf-8") as h:
                json.dump(
                    {
                        "job_id": "c1__Ours-S__r1",
                        "sql_sha256": sha,
                        "profile_hash": job_completed["profile_hash"],
                        "state": "error",
                        "ran_occam": True,
                    },
                    h,
                )
            self.assertIsNotNone(
                batch_eval._is_valid_completed_job(
                    os.path.join(dir_completed, "job_result.json"),
                    job_completed,
                    rerun_failed=False,
                )
            )
            self.assertIsNone(
                batch_eval._is_valid_completed_job(
                    os.path.join(dir_completed, "job_result.json"),
                    job_completed,
                    rerun_failed=True,
                )
            )

            # Corrupt/modified SQL invalidates cached job
            job_completed["case"]["sql_sha256"] = "different"
            self.assertIsNone(
                batch_eval._is_valid_completed_job(
                    os.path.join(dir_completed, "job_result.json"),
                    job_completed,
                    rerun_failed=False,
                )
            )

    def test_aggregation_separates_completed_vs_censored_and_stratifies_signatures(self):
        with tempfile.TemporaryDirectory() as td:
            out_dir = os.path.join(td, "out")
            runs_dir = os.path.join(out_dir, "runs")
            qual_dir = os.path.join(out_dir, "qualified")
            os.makedirs(runs_dir)
            os.makedirs(qual_dir)

            # 2 duplicate cases sharing sigA, 1 case with sigB; plus 1 censored timeout run
            records = [
                ("cA1__Ours-S__r1", "cA1", "sigA", "completed", 200, 40, True),
                ("cA2__Ours-S__r1", "cA2", "sigA", "completed", 200, 60, True),
                ("cB1__Ours-S__r1", "cB1", "sigB", "completed", 200, 100, True),
                ("cB2__Ours-S__r1", "cB2", "sigB", "timeout", 200, 180, False),
            ]
            for jid, cid, sig, st, ib, rb, vp in records:
                jdir = os.path.join(runs_dir, jid)
                os.makedirs(jdir)
                sp = os.path.join(jdir, "stats.json")
                with open(sp, "w", encoding="utf-8") as h:
                    json.dump(
                        _make_sample_stats(
                            status=st,
                            initial_bytes=ib,
                            reduced_bytes=rb,
                            validation_passed=vp,
                            minimality="statement-deletion-1-minimal" if vp else "not-established",
                        ),
                        h,
                    )
                with open(os.path.join(jdir, "job_result.json"), "w", encoding="utf-8") as h:
                    json.dump(
                        {
                            "job_id": jid,
                            "case_id": cid,
                            "dbms": "sqlite",
                            "dialect": "sqlite",
                            "profile": "Ours-S",
                            "repeat": 1,
                            "state": "censored" if st == "timeout" else "completed",
                            "eval_status": st,
                            "input_qualified": True,
                            "baseline_signature_id": sig,
                            "baseline_signature_hash": sig + "_hash",
                            "baseline_category": "assertion",
                            "stats_path": sp,
                            "reduced_replay_verification": {
                                "runs_required": 5,
                                "runs_passed": 5,
                                "all_passed": True,
                                "consistent_signature": True,
                                "matches_baseline": True,
                                "slippage_count": 0,
                                "observed_signature_ids": [sig],
                            },
                        },
                        h,
                    )

            agg, _, md_path = batch_eval.aggregate_out_dir(out_dir)
            prof = agg["by_profile"]["Ours-S"]
            # Completed-only excludes the 180B timeout checkpoint
            self.assertEqual(prof["completed_runs"], 3)
            self.assertEqual(prof["censored_runs"], 1)
            self.assertEqual(prof["completed_only"]["count"], 3)
            self.assertEqual(prof["completed_only"]["reduced_bytes"]["median"], 60.0)
            # all_with_checkpoints includes the 180B checkpoint separately
            self.assertEqual(prof["all_with_checkpoints"]["count"], 4)
            self.assertEqual(prof["all_with_checkpoints"]["reduced_bytes"]["max"], 180.0)

            # Signature-stratified: sigA median bytes = 50, sigB completed median bytes = 100
            # Macro-median across unique signatures = median([50, 100]) = 75 (not biased to 60!)
            strat = agg["signature_stratified_by_profile"]["Ours-S"]
            self.assertEqual(strat["signatures_observed"], 2)
            self.assertEqual(strat["signatures_with_completed_runs"], 2)
            self.assertEqual(strat["completed_signature_median_reduced_bytes"]["median"], 75.0)
            self.assertTrue(os.path.isfile(md_path))

    def test_real_occam_end_to_end_with_local_embedded_and_jobs_2(self):
        try:
            occam_bin = batch_eval.resolve_occam_binary()
        except FileNotFoundError:
            self.skipTest("occam executable not built in this checkout")

        with tempfile.TemporaryDirectory() as td:
            corpus_dir = os.path.join(td, "corpus")
            out_dir = os.path.join(td, "out")
            os.makedirs(corpus_dir)

            # Two authentic multi-statement SQLite inputs with noise + required setup + trigger
            sql1 = (
                "CREATE TABLE noise_a(x INT);\n"
                "INSERT INTO noise_a VALUES (1);\n"
                "CREATE TABLE t0(c0 INT);\n"
                "INSERT INTO t0 VALUES (42);\n"
                "CREATE TABLE noise_b(y INT);\n"
                "SELECT c0 FROM t0 WHERE c0 = 42;\n"
            )
            sql2 = (
                "CREATE TABLE noise_c(z INT);\n"
                "CREATE TABLE u0(k INT);\n"
                "INSERT INTO u0 VALUES (7);\n"
                "SELECT k FROM u0 WHERE k = 7;\n"
            )
            for name, content, pat in (
                ("e2e_case1", sql1, "Assertion.*t0_bug"),
                ("e2e_case2", sql2, "Assertion.*u0_bug"),
            ):
                with open(os.path.join(corpus_dir, f"{name}.sql"), "w", encoding="utf-8") as h:
                    h.write(content)
                with open(os.path.join(corpus_dir, f"{name}.meta.json"), "w", encoding="utf-8") as h:
                    json.dump(
                        {
                            "id": name,
                            "target": {"dbms": "sqlite"},
                            "crash_signature": {"expected_signature": pat},
                        },
                        h,
                    )

            # Mock sqlite3 engine: crashes with deterministic assertion iff table + insert + select present
            mock_sqlite = self._write_exec(
                td,
                "mock_sqlite3",
                r"""sql="$(cat)"
if echo "$sql" | grep -q "CREATE TABLE t0" && echo "$sql" | grep -q "INSERT INTO t0" && echo "$sql" | grep -q "SELECT c0 FROM t0"; then
  echo "sqlite3: sqlite3.c:555: vdbeExec: Assertion \`t0_bug != 0' failed." >&2
  exit 134
fi
if echo "$sql" | grep -q "CREATE TABLE u0" && echo "$sql" | grep -q "INSERT INTO u0" && echo "$sql" | grep -q "SELECT k FROM u0"; then
  echo "sqlite3: sqlite3.c:777: vdbeExec: Assertion \`u0_bug != 0' failed." >&2
  exit 134
fi
exit 0
""",
            )

            res = subprocess.run(
                [
                    sys.executable,
                    BATCH_EVAL,
                    "--corpus-dir",
                    corpus_dir,
                    "--local-embedded",
                    "--engine-bin",
                    mock_sqlite,
                    "--occam-bin",
                    occam_bin,
                    "--profiles",
                    "Ours-S,ddmin-only",
                    "--repeats",
                    "1",
                    "--jobs",
                    "2",
                    "--replays",
                    "5",
                    "--reject-on-slippage",
                    "--out-dir",
                    out_dir,
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res.returncode, 0, res.stderr + "\n" + res.stdout)

            with open(os.path.join(out_dir, "aggregated_stats.json"), encoding="utf-8") as h:
                agg = json.load(h)

            self.assertEqual(agg["overview"]["input_stable_cases"], 2)
            self.assertEqual(agg["overview"]["unique_qualified_crash_signatures"], 2)
            self.assertEqual(agg["overview"]["total_job_runs"], 4)

            for prof in ("Ours-S", "ddmin-only"):
                p = agg["by_profile"][prof]
                self.assertEqual(p["completed_runs"], 2)
                self.assertEqual(p["reduced_replay_5of5_runs"], 2)
                self.assertEqual(p["reduced_replay_zero_slippage_runs"], 2)
                self.assertEqual(p["minimality_1_minimal_runs"], 2)

            # Check minimal SQL actually dropped noise statements while preserving core
            min_path = os.path.join(out_dir, "runs", "e2e_case1__Ours-S__r1", "minimal.sql")
            with open(min_path, encoding="utf-8") as h:
                minimal_sql = h.read()
            self.assertNotIn("noise_a", minimal_sql)
            self.assertNotIn("noise_b", minimal_sql)
            self.assertIn("CREATE TABLE t0", minimal_sql)

            # Verify --aggregate-only regenerates aggregated_stats.json and summary.md cleanly
            os.unlink(os.path.join(out_dir, "aggregated_stats.json"))
            os.unlink(os.path.join(out_dir, "summary.md"))
            r_agg = subprocess.run(
                [sys.executable, BATCH_EVAL, "--aggregate-only", "--out-dir", out_dir],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(r_agg.returncode, 0, r_agg.stderr)
            self.assertTrue(os.path.isfile(os.path.join(out_dir, "aggregated_stats.json")))
            self.assertTrue(os.path.isfile(os.path.join(out_dir, "summary.md")))

    def test_external_containers_pool_does_not_spawn_or_remove_containers(self):
        with tempfile.TemporaryDirectory() as td:
            bin_dir = os.path.join(td, "bin")
            out_dir = os.path.join(td, "out")
            os.makedirs(bin_dir)
            sql_a = os.path.join(td, "a.sql")
            sql_b = os.path.join(td, "b.sql")
            for p in (sql_a, sql_b):
                with open(p, "w", encoding="utf-8") as h:
                    h.write("SELECT 1;\n")

            docker_ops = os.path.join(td, "docker_ops.log")
            self._write_exec(
                bin_dir,
                "docker",
                f"""printf '%s\\n' "$*" >> {docker_ops}
case "$1" in
  ps)
    echo "ext_pg0"
    echo "ext_pg1"
    ;;
  exec)
    if [ "$1" = "exec" ]; then shift; fi
    if [ "$1" = "-i" ]; then shift; fi
    cname="$1"; shift
    op="$1"; shift
    if [ "$op" = "stat" ]; then
      [ -f "{td}/${{cname}}.log" ] || : > "{td}/${{cname}}.log"
      echo "$(wc -c < "{td}/${{cname}}.log" | tr -d ' ') 2001"
    elif [ "$op" = "tail" ]; then
      shift
      tail -c "$1" "{td}/${{cname}}.log"
    fi
    ;;
esac
""",
            )
            self._write_exec(bin_dir, "pg_isready", "exit 0\n")
            self._write_exec(
                bin_dir,
                "psql",
                f"""port="5432"; db=""; prev=""
for a in "$@"; do
  if [ "$prev" = "-p" ]; then port="$a"; fi
  if [ "$prev" = "-d" ]; then db="$a"; fi
  prev="$a"
done
if [ "$db" = "template1" ]; then
  exit 0
fi
cname="ext_pg0"
[ "$port" = "15402" ] && cname="ext_pg1"
echo '2026-11-23 00:00:00 UTC [10] TRAP: FailedAssertion("!(ok)", File: "x.c", Line: 1)' >> "{td}/${{cname}}.log"
exit 2
""",
            )
            mock_occam = self._write_exec(
                bin_dir,
                "occam",
                """#!/bin/sh
set -e
input="$1"; shift
out_path=""; stats_path=""
while [ $# -gt 0 ]; do
  case "$1" in
    --out) out_path="$2"; shift 2 ;;
    --stats) stats_path="$2"; shift 2 ;;
    *) shift ;;
  esac
done
cp "$input" "$out_path"
cat > "$stats_path" <<'JSON'
{"initial_bytes":10,"reduced_bytes":10,"reduction_ratio":0.0,"initial_statements":1,"reduced_statements":1,"oracle_executions":1,"oracle_attempts":1,"initial_oracle_observations":1,"cache_hits":0,"cache_misses":1,"cache_joins":0,"cache_hit_ratio":0.0,"cancelled_observations":0,"status":"completed","reduction_settings":{},"evaluation_budget":{"evaluation_jobs":1},"evaluation_metrics":{"evaluation_validation_passed":true,"evaluation_minimality_claim":"statement-deletion-1-minimal","evaluation_curve":[]},"initial_oracle_seconds":0.01,"elapsed_seconds":0.02}
JSON
""",
            )

            env = os.environ.copy()
            env["PATH"] = f"{bin_dir}:{env.get('PATH', '')}"
            res = subprocess.run(
                [
                    sys.executable,
                    BATCH_EVAL,
                    "--inputs",
                    sql_a,
                    sql_b,
                    "--dbms",
                    "postgres",
                    "--containers",
                    "ext_pg0@127.0.0.1:15401,ext_pg1@127.0.0.1:15402",
                    "--jobs",
                    "2",
                    "--replays",
                    "5",
                    "--occam-bin",
                    mock_occam,
                    "--out-dir",
                    out_dir,
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res.returncode, 0, res.stderr + "\n" + res.stdout)
            with open(docker_ops, encoding="utf-8") as h:
                ops = h.read()
            self.assertNotIn("run ", ops)
            self.assertNotIn("rm ", ops)
            with open(os.path.join(out_dir, "aggregated_stats.json"), encoding="utf-8") as h:
                agg = json.load(h)
            self.assertEqual(agg["by_profile"]["Ours-S"]["completed_runs"], 2)


class TestBatchEvalRegressions(unittest.TestCase):
    def _import_batch_eval(self):
        try:
            from harness import batch_eval as be
        except ImportError:
            import batch_eval as be
        return be

    def test_cross_dbms_identical_signature_stays_distinct(self):
        be = self._import_batch_eval()
        with tempfile.TemporaryDirectory() as td:
            for cid, dbms in (("pg1", "postgres"), ("sq1", "sqlite")):
                qdir = os.path.join(td, "qualified", cid)
                os.makedirs(qdir, exist_ok=True)
                with open(os.path.join(qdir, "input_replay.json"), "w", encoding="utf-8") as h:
                    json.dump(
                        {
                            "case_id": cid,
                            "dbms": dbms,
                            "qualified": True,
                            "baseline_signature_id": "signal:SIGSEGV",
                            "baseline_signature_hash": f"hash_{dbms}",
                        },
                        h,
                    )

                rdir = os.path.join(td, "runs", f"{cid}__ours-s__r01")
                os.makedirs(rdir, exist_ok=True)
                spath = os.path.join(rdir, "stats.json")
                with open(spath, "w", encoding="utf-8") as h:
                    json.dump(
                        {
                            "status": "completed",
                            "initial_bytes": 100,
                            "reduced_bytes": 20,
                            "reduction_ratio": 80.0,
                            "oracle_attempts": 5,
                            "elapsed_seconds": 0.2,
                            "evaluation_metrics": {
                                "evaluation_status": "completed",
                                "evaluation_validation_passed": True,
                            },
                        },
                        h,
                    )
                with open(os.path.join(rdir, "job_result.json"), "w", encoding="utf-8") as h:
                    json.dump(
                        {
                            "job_id": f"{cid}__ours-s__r01",
                            "case_id": cid,
                            "dbms": dbms,
                            "dialect": dbms,
                            "profile": "Ours-S",
                            "repeat": 1,
                            "state": "completed",
                            "input_qualified": True,
                            "baseline_signature_id": "signal:SIGSEGV",
                            "baseline_signature_hash": f"hash_{dbms}",
                            "baseline_category": "signal",
                            "stats_path": spath,
                        },
                        h,
                    )

            agg, _, _ = be.aggregate_out_dir(td)
            self.assertEqual(agg["overview"]["unique_qualified_crash_signatures"], 2)
            self.assertEqual(agg["overview"]["unique_stratification_groups"], 2)
            self.assertEqual(
                set(agg["by_signature"].keys()),
                {"postgres::signal:SIGSEGV", "sqlite::signal:SIGSEGV"},
            )

    def test_rejects_duplicate_and_unsafe_case_ids(self):
        import sys

        be = self._import_batch_eval()
        with tempfile.TemporaryDirectory() as td:
            cdir = os.path.join(td, "corpus")
            os.makedirs(cdir, exist_ok=True)
            for fname in ("case_a", "case_b"):
                with open(os.path.join(cdir, f"{fname}.sql"), "w", encoding="utf-8") as h:
                    h.write("SELECT 1;\n")
                with open(os.path.join(cdir, f"{fname}.meta.json"), "w", encoding="utf-8") as h:
                    json.dump({"id": "dup_case", "target": {"dbms": "sqlite"}}, h)
            with self.assertRaises(ValueError):
                be.load_cases_from_corpus(cdir)

            cdir_bad = os.path.join(td, "corpus_bad")
            os.makedirs(cdir_bad, exist_ok=True)
            with open(os.path.join(cdir_bad, "x.sql"), "w", encoding="utf-8") as h:
                h.write("SELECT 1;\n")
            with open(os.path.join(cdir_bad, "x.meta.json"), "w", encoding="utf-8") as h:
                json.dump({"id": "../evil", "target": {"dbms": "sqlite"}}, h)
            with self.assertRaises(ValueError):
                be.load_cases_from_corpus(cdir_bad)

            sql_file = os.path.join(td, "q.sql")
            with open(sql_file, "w", encoding="utf-8") as h:
                h.write("SELECT 1;\n")

            for bad_id in ("../evil", "a/b", "a\\b", ".", "..", "", "   ", " leading", "bad\nid"):
                mpath = os.path.join(td, "bad_manifest.json")
                with open(mpath, "w", encoding="utf-8") as h:
                    json.dump({"cases": [{"id": bad_id, "dbms": "sqlite", "file": sql_file}]}, h)
                with self.assertRaises(ValueError, msg=f"bad_id={bad_id!r}"):
                    be.load_cases_from_manifest(mpath)

            cdir_empty_id = os.path.join(td, "corpus_empty_id")
            os.makedirs(cdir_empty_id, exist_ok=True)
            with open(os.path.join(cdir_empty_id, "x.sql"), "w", encoding="utf-8") as h:
                h.write("SELECT 1;\n")
            with open(os.path.join(cdir_empty_id, "x.meta.json"), "w", encoding="utf-8") as h:
                json.dump({"id": "", "target": {"dbms": "sqlite"}}, h)
            with self.assertRaises(ValueError):
                be.load_cases_from_corpus(cdir_empty_id)

            dup_manifest = os.path.join(td, "dup_manifest.json")
            with open(dup_manifest, "w", encoding="utf-8") as h:
                json.dump(
                    {
                        "cases": [
                            {"id": "dup", "dbms": "sqlite", "file": sql_file},
                            {"id": "dup", "dbms": "sqlite", "file": sql_file},
                        ]
                    },
                    h,
                )
            with self.assertRaises(ValueError):
                be.load_cases_from_manifest(dup_manifest)

            evil_manifest = os.path.join(td, "evil_manifest.json")
            with open(evil_manifest, "w", encoding="utf-8") as h:
                json.dump({"cases": [{"id": "../evil", "dbms": "sqlite", "file": sql_file}]}, h)
            bad_json_manifest = os.path.join(td, "malformed_manifest.json")
            with open(bad_json_manifest, "w", encoding="utf-8") as h:
                h.write("{not valid json")
            ok_manifest = os.path.join(td, "ok_manifest.json")
            with open(ok_manifest, "w", encoding="utf-8") as h:
                json.dump({"cases": [{"id": "c1", "dbms": "sqlite", "file": sql_file}]}, h)

            batch_script = os.path.join(os.path.dirname(os.path.abspath(__file__)), "batch_eval.py")
            for extra_args in (
                ["--manifest", evil_manifest, "--local-embedded"],
                ["--manifest", bad_json_manifest, "--local-embedded"],
                ["--manifest", ok_manifest, "--local-embedded", "--timeout", "nan"],
                ["--manifest", ok_manifest, "--local-embedded", "--probe-timeout", "inf"],
                ["--manifest", ok_manifest, "--local-embedded", "--container-wait", "0"],
                ["--manifest", ok_manifest, "--containers", ""],
                ["--manifest", ok_manifest, "--containers", " , "],
                ["--manifest", ok_manifest, "--containers", "c1:0"],
            ):
                res = subprocess.run(
                    [
                        sys.executable,
                        batch_script,
                        *extra_args,
                        "--out-dir",
                        os.path.join(td, "out"),
                        "--dry-run",
                    ],
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertEqual(res.returncode, 2, f"args={extra_args}: {res.stderr}\n{res.stdout}")
                self.assertNotIn("Traceback", res.stderr, f"args={extra_args}: {res.stderr}")

    def test_invalidates_cached_skipped_unstable_input_when_rerun_or_allowed(self):
        be = self._import_batch_eval()
        with tempfile.TemporaryDirectory() as td:
            jpath = os.path.join(td, "job_result.json")
            job = {
                "job_id": "c1__ours-s__r01",
                "profile_hash": "phash1",
                "allow_unstable_input": False,
                "case": {"sql_sha256": "sha1"},
            }
            with open(jpath, "w", encoding="utf-8") as h:
                json.dump(
                    {
                        "job_id": "c1__ours-s__r01",
                        "sql_sha256": "sha1",
                        "profile_hash": "phash1",
                        "state": "skipped_unstable_input",
                    },
                    h,
                )
            self.assertIsNotNone(
                be._is_valid_completed_job(
                    jpath,
                    job,
                    rerun_failed=False,
                    qual_record={"qualified": False},
                )
            )
            self.assertIsNone(
                be._is_valid_completed_job(
                    jpath,
                    {**job, "allow_unstable_input": True},
                    rerun_failed=False,
                    qual_record={"qualified": False},
                )
            )
            self.assertIsNone(
                be._is_valid_completed_job(
                    jpath,
                    job,
                    rerun_failed=True,
                    qual_record={"qualified": False},
                )
            )
            self.assertIsNone(
                be._is_valid_completed_job(
                    jpath,
                    job,
                    rerun_failed=False,
                    qual_record={"qualified": True},
                )
            )

    def test_worker_target_reset_rebuilds_cmd_with_new_port_after_recreate(self):
        from unittest import mock

        be = self._import_batch_eval()
        worker = be.WorkerTarget(worker_id=1, run_tag="t1")
        worker.managed_container = "occam-eval-t1-w1"
        worker.container_name = "occam-eval-t1-w1"
        worker.current_dbms = "postgres"
        worker.current_image = "img:1"
        worker.host = "127.0.0.1"
        worker.port = 15432
        worker.dbname = "occam_test"

        seen_cmds = []

        def fake_ensure(dbms, image, force_recreate=False):
            self.assertTrue(force_recreate)
            worker.port = 15433

        def fake_run(cmd, **_kwargs):
            seen_cmds.append(list(cmd))
            if len(seen_cmds) == 1:
                return subprocess.CompletedProcess(cmd, 1, "", "reset failed")
            return subprocess.CompletedProcess(cmd, 0, "ok", "")

        with mock.patch.object(worker, "ensure_target", side_effect=fake_ensure), mock.patch.object(
            be.subprocess, "run", side_effect=fake_run
        ):
            ok, _msg = worker.reset()

        self.assertTrue(ok)
        self.assertEqual(len(seen_cmds), 2)
        self.assertIn("15432", seen_cmds[0])
        self.assertIn("15433", seen_cmds[1])

    def test_real_occam_remaining_profiles_and_censored_max_oracle_executions(self):
        import sys

        be = self._import_batch_eval()
        try:
            occam_bin = be.resolve_occam_binary("")
        except FileNotFoundError:
            self.skipTest("occam binary not built locally")

        batch_script = os.path.join(os.path.dirname(os.path.abspath(__file__)), "batch_eval.py")
        with tempfile.TemporaryDirectory() as td:
            cdir = os.path.join(td, "corpus")
            os.makedirs(cdir, exist_ok=True)
            sql_file = os.path.join(cdir, "c1.sql")
            with open(sql_file, "w", encoding="utf-8") as h:
                h.write(
                    "SELECT 1;\nSELECT 2;\nSELECT 3;\nSELECT 4;\n"
                    "SELECT 5;\nSELECT 6;\nSELECT (1 + 999);\n"
                )
            with open(os.path.join(cdir, "c1.meta.json"), "w", encoding="utf-8") as h:
                json.dump(
                    {
                        "target": {"dbms": "sqlite"},
                        "crash_signature": {"expected_signature": r"Assertion.*t0_bug"},
                    },
                    h,
                )

            fake_sqlite = os.path.join(td, "fake_sqlite3")
            with open(fake_sqlite, "w", encoding="utf-8") as h:
                h.write(
                    "#!/bin/sh\n"
                    "inp=$(cat)\n"
                    'if printf "%s" "$inp" | grep -q "999"; then\n'
                    '  echo "sqlite3: sqlite3.c:555: vdbeExec: Assertion \\`t0_bug != 0\' failed." >&2\n'
                    "  exit 134\n"
                    "fi\n"
                    "exit 0\n"
                )
            os.chmod(fake_sqlite, 0o755)

            out_all = os.path.join(td, "out_profiles")
            res1 = subprocess.run(
                [
                    sys.executable,
                    batch_script,
                    "--corpus-dir",
                    cdir,
                    "--profiles",
                    "audit-only,Ours-SI,Ours-Full,Ours-NoDep",
                    "--repeats",
                    "1",
                    "--jobs",
                    "2",
                    "--replays",
                    "5",
                    "--reject-on-slippage",
                    "--local-embedded",
                    "--engine-bin",
                    fake_sqlite,
                    "--occam-bin",
                    occam_bin,
                    "--out-dir",
                    out_all,
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res1.returncode, 0, res1.stderr + "\n" + res1.stdout)
            with open(os.path.join(out_all, "aggregated_stats.json"), encoding="utf-8") as h:
                agg1 = json.load(h)
            self.assertEqual(agg1["overview"]["input_stable_cases"], 1)
            for pname in ("audit-only", "Ours-SI", "Ours-Full", "Ours-NoDep"):
                self.assertIn(pname, agg1["by_profile"])
                pstats = agg1["by_profile"][pname]
                self.assertEqual(pstats["completed_runs"], 1, pname)
                self.assertEqual(pstats["censored_runs"], 0, pname)
                self.assertEqual(pstats["reduced_replay_5of5_runs"], 1, pname)
                self.assertEqual(pstats["reduced_replay_zero_slippage_runs"], 1, pname)

            out_censored = os.path.join(td, "out_censored")
            res2 = subprocess.run(
                [
                    sys.executable,
                    batch_script,
                    "--corpus-dir",
                    cdir,
                    "--profiles",
                    "Ours-Full",
                    "--repeats",
                    "1",
                    "--jobs",
                    "1",
                    "--replays",
                    "5",
                    "--max-oracle-executions",
                    "2",
                    "--reject-on-slippage",
                    "--local-embedded",
                    "--engine-bin",
                    fake_sqlite,
                    "--occam-bin",
                    occam_bin,
                    "--out-dir",
                    out_censored,
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res2.returncode, 0, res2.stderr + "\n" + res2.stdout)
            with open(os.path.join(out_censored, "aggregated_stats.json"), encoding="utf-8") as h:
                agg2 = json.load(h)
            p2 = agg2["by_profile"]["Ours-Full"]
            self.assertEqual(p2["completed_runs"], 0)
            self.assertEqual(p2["censored_runs"], 1)
            self.assertEqual(p2["reduced_replay_5of5_runs"], 1)
            self.assertEqual(p2["reduced_replay_zero_slippage_runs"], 1)

    def test_real_occam_sigint_exits_130_and_resumes_cleanly(self):
        import signal
        import time

        be = self._import_batch_eval()
        try:
            occam_bin = be.resolve_occam_binary("")
        except FileNotFoundError:
            self.skipTest("occam binary not built locally")

        with tempfile.TemporaryDirectory() as td:
            cdir = os.path.join(td, "corpus")
            os.makedirs(cdir, exist_ok=True)
            with open(os.path.join(cdir, "c1.sql"), "w", encoding="utf-8") as h:
                h.write(
                    "SELECT 1;\nSELECT 2;\nSELECT 3;\nSELECT 4;\n"
                    "SELECT 5;\nSELECT (1 + 999);\n"
                )
            with open(os.path.join(cdir, "c1.meta.json"), "w", encoding="utf-8") as h:
                json.dump(
                    {
                        "target": {"dbms": "sqlite"},
                        "crash_signature": {"expected_signature": r"Assertion.*sig_bug"},
                    },
                    h,
                )

            gate_file = os.path.join(td, "block.gate")
            with open(gate_file, "w", encoding="utf-8") as h:
                h.write("1\n")
            started_flag = os.path.join(td, "oracle.started")
            cnt_file = os.path.join(td, "cnt.txt")
            fake_sqlite = os.path.join(td, "fake_sqlite3")
            with open(fake_sqlite, "w", encoding="utf-8") as h:
                h.write(
                    "#!/bin/sh\n"
                    "inp=$(cat)\n"
                    f"n=0\n[ -f '{cnt_file}' ] && n=$(cat '{cnt_file}')\n"
                    f"n=$((n + 1))\nprintf '%s' \"$n\" > '{cnt_file}'\n"
                    f"if [ -f '{gate_file}' ] && [ \"$n\" -ge 4 ]; then\n"
                    f"  touch '{started_flag}'\n"
                    "  exec sleep 15\n"
                    "fi\n"
                    'if printf "%s" "$inp" | grep -q "999"; then\n'
                    '  echo "sqlite3: sqlite3.c:555: vdbeExec: Assertion \\`sig_bug != 0\' failed." >&2\n'
                    "  exit 134\n"
                    "fi\n"
                    "exit 0\n"
                )
            os.chmod(fake_sqlite, 0o755)

            out_dir = os.path.join(td, "out")
            cmd = [
                sys.executable,
                BATCH_EVAL,
                "--corpus-dir",
                cdir,
                "--profiles",
                "ddmin-only,Ours-Full",
                "--repeats",
                "1",
                "--jobs",
                "1",
                "--replays",
                "2",
                "--timeout",
                "15.0",
                "--local-embedded",
                "--engine-bin",
                fake_sqlite,
                "--occam-bin",
                occam_bin,
                "--out-dir",
                out_dir,
            ]
            proc = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
            try:
                deadline = time.monotonic() + 20.0
                while time.monotonic() < deadline:
                    if os.path.exists(started_flag):
                        break
                    if proc.poll() is not None:
                        break
                    time.sleep(0.05)
                self.assertTrue(os.path.exists(started_flag), "occam oracle probe never blocked")
                self.assertIsNone(proc.poll())
                proc.send_signal(signal.SIGINT)
                stdout, stderr = proc.communicate(timeout=20)
            except Exception:
                proc.kill()
                proc.communicate()
                raise

            self.assertEqual(proc.returncode, 130, stderr + "\n" + stdout)
            summary = json.loads(stdout)
            self.assertTrue(summary["interrupted"])
            self.assertEqual(summary["worker_exceptions"], 0)
            self.assertTrue(os.path.isfile(os.path.join(out_dir, "qualified", "c1", "input_replay.json")))
            runs_dir = os.path.join(out_dir, "runs")
            if os.path.isdir(runs_dir):
                for jdir in os.listdir(runs_dir):
                    jres_path = os.path.join(runs_dir, jdir, "job_result.json")
                    if os.path.isfile(jres_path):
                        with open(jres_path, encoding="utf-8") as h:
                            jres = json.load(h)
                        self.assertNotIn(jres.get("state"), ("error", "infra_error"))

            os.unlink(gate_file)
            res2 = subprocess.run(cmd, capture_output=True, text=True, check=False)
            self.assertEqual(res2.returncode, 0, res2.stderr + "\n" + res2.stdout)
            with open(os.path.join(out_dir, "aggregated_stats.json"), encoding="utf-8") as h:
                agg = json.load(h)
            db_stats = agg["by_dbms"]["sqlite"]
            self.assertEqual(db_stats["total_runs"], 2)
            self.assertEqual(db_stats["completed_runs"], 2)
            self.assertEqual(db_stats["censored_runs"], 0)
            self.assertEqual(db_stats["error_or_failed_runs"], 0)

    def test_real_occam_budget_timeout_produces_censored_run(self):
        be = self._import_batch_eval()
        try:
            occam_bin = be.resolve_occam_binary("")
        except FileNotFoundError:
            self.skipTest("occam binary not built locally")

        with tempfile.TemporaryDirectory() as td:
            cdir = os.path.join(td, "corpus")
            os.makedirs(cdir, exist_ok=True)
            with open(os.path.join(cdir, "c1.sql"), "w", encoding="utf-8") as h:
                h.write(
                    "\n".join(f"SELECT {i};" for i in range(1, 10)) + "\nSELECT (1 + 999);\n"
                )
            with open(os.path.join(cdir, "c1.meta.json"), "w", encoding="utf-8") as h:
                json.dump(
                    {
                        "target": {"dbms": "sqlite"},
                        "crash_signature": {"expected_signature": r"Assertion.*t0_budget"},
                    },
                    h,
                )

            fake_sqlite = os.path.join(td, "slow_probe_sqlite3")
            with open(fake_sqlite, "w", encoding="utf-8") as h:
                h.write(
                    "#!/bin/sh\n"
                    "inp=$(cat)\n"
                    # Keep preflight fast and exhaust the budget on a deletion.
                    'if [ "$(printf "%s\\n" "$inp" | grep -c "^SELECT ")" -lt 10 ]; then sleep 3; fi\n'
                    'if printf "%s" "$inp" | grep -q "999"; then\n'
                    '  echo "sqlite3: sqlite3.c:666: vdbeExec: Assertion \\`t0_budget != 0\' failed." >&2\n'
                    "  exit 134\n"
                    "fi\n"
                    "exit 0\n"
                )
            os.chmod(fake_sqlite, 0o755)

            out_dir = os.path.join(td, "out_budget")
            res = subprocess.run(
                [
                    sys.executable,
                    BATCH_EVAL,
                    "--corpus-dir",
                    cdir,
                    "--profiles",
                    "Ours-Full",
                    "--repeats",
                    "1",
                    "--jobs",
                    "1",
                    "--replays",
                    "0",
                    "--budget",
                    "1.5",
                    "--timeout",
                    "5.0",
                    "--local-embedded",
                    "--engine-bin",
                    fake_sqlite,
                    "--occam-bin",
                    occam_bin,
                    "--out-dir",
                    out_dir,
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res.returncode, 0, res.stderr + "\n" + res.stdout)
            with open(os.path.join(out_dir, "aggregated_stats.json"), encoding="utf-8") as h:
                agg = json.load(h)
            p = agg["by_profile"]["Ours-Full"]
            self.assertEqual(p["completed_runs"], 0)
            self.assertEqual(p["censored_runs"], 1)
            self.assertEqual(p["error_or_failed_runs"], 0)

    def test_run_config_hash_invalidates_stale_checkpoint_on_resume(self):
        be = self._import_batch_eval()
        try:
            occam_bin = be.resolve_occam_binary("")
        except FileNotFoundError:
            self.skipTest("occam binary not built locally")

        with tempfile.TemporaryDirectory() as td:
            cdir = os.path.join(td, "corpus")
            os.makedirs(cdir, exist_ok=True)
            with open(os.path.join(cdir, "c1.sql"), "w", encoding="utf-8") as h:
                h.write(
                    "SELECT 1;\nSELECT 2;\nSELECT 3;\nSELECT 4;\n"
                    "SELECT 5;\nSELECT (1 + 999);\n"
                )
            with open(os.path.join(cdir, "c1.meta.json"), "w", encoding="utf-8") as h:
                json.dump(
                    {
                        "target": {"dbms": "sqlite"},
                        "crash_signature": {"expected_signature": r"Assertion.*cfg_bug"},
                    },
                    h,
                )

            fake_sqlite = os.path.join(td, "fake_sqlite3")
            with open(fake_sqlite, "w", encoding="utf-8") as h:
                h.write(
                    "#!/bin/sh\n"
                    "inp=$(cat)\n"
                    'if printf "%s" "$inp" | grep -q "999"; then\n'
                    '  echo "sqlite3: sqlite3.c:777: vdbeExec: Assertion \\`cfg_bug != 0\' failed." >&2\n'
                    "  exit 134\n"
                    "fi\n"
                    "exit 0\n"
                )
            os.chmod(fake_sqlite, 0o755)

            out_dir = os.path.join(td, "out_cfg")
            base_cmd = [
                sys.executable,
                BATCH_EVAL,
                "--corpus-dir",
                cdir,
                "--profiles",
                "Ours-Full",
                "--repeats",
                "1",
                "--jobs",
                "1",
                "--local-embedded",
                "--engine-bin",
                fake_sqlite,
                "--occam-bin",
                occam_bin,
                "--out-dir",
                out_dir,
            ]
            res1 = subprocess.run(
                [*base_cmd, "--replays", "2", "--max-oracle-executions", "2"],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res1.returncode, 0, res1.stderr + "\n" + res1.stdout)
            jres_path = os.path.join(out_dir, "runs", "c1__Ours-Full__r1", "job_result.json")
            with open(jres_path, encoding="utf-8") as h:
                j1 = json.load(h)
            self.assertEqual(j1["state"], "censored")

            res2 = subprocess.run(
                [*base_cmd, "--replays", "2", "--max-oracle-executions", "50"],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res2.returncode, 0, res2.stderr + "\n" + res2.stdout)
            with open(jres_path, encoding="utf-8") as h:
                j2 = json.load(h)
            self.assertEqual(j2["state"], "completed")
            self.assertNotEqual(j1["run_config_hash"], j2["run_config_hash"])

            res3 = subprocess.run(
                [*base_cmd, "--replays", "3", "--max-oracle-executions", "50"],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res3.returncode, 0, res3.stderr + "\n" + res3.stdout)
            with open(jres_path, encoding="utf-8") as h:
                j3 = json.load(h)
            self.assertEqual(j3["reduced_replay_verification"]["runs_required"], 3)
            self.assertNotEqual(j2["run_config_hash"], j3["run_config_hash"])

    def test_skip_input_replay_invalidates_skipped_and_leaves_slippage_zero_without_baseline(self):
        be = self._import_batch_eval()
        try:
            occam_bin = be.resolve_occam_binary("")
        except FileNotFoundError:
            self.skipTest("occam binary not built locally")

        with tempfile.TemporaryDirectory() as td:
            cdir = os.path.join(td, "corpus")
            os.makedirs(cdir, exist_ok=True)
            with open(os.path.join(cdir, "c1.sql"), "w", encoding="utf-8") as h:
                h.write("SELECT 1;\nSELECT 2;\nSELECT (1 + 999);\n")
            with open(os.path.join(cdir, "c1.meta.json"), "w", encoding="utf-8") as h:
                json.dump({"target": {"dbms": "sqlite"}}, h)

            flaky_flag = os.path.join(td, "flaky.flag")
            with open(flaky_flag, "w", encoding="utf-8") as h:
                h.write("1\n")
            cnt_file = os.path.join(td, "cnt.txt")
            fake_sqlite = os.path.join(td, "fake_sqlite3")
            with open(fake_sqlite, "w", encoding="utf-8") as h:
                h.write(
                    "#!/bin/sh\n"
                    "inp=$(cat)\n"
                    f"if [ -f '{flaky_flag}' ]; then\n"
                    f"  n=0\n  [ -f '{cnt_file}' ] && n=$(cat '{cnt_file}')\n"
                    f"  n=$((n + 1))\n  printf '%s' \"$n\" > '{cnt_file}'\n"
                    '  if [ "$n" -ne 1 ]; then\n'
                    "    exit 0\n"
                    "  fi\n"
                    "fi\n"
                    'if printf "%s" "$inp" | grep -q "999"; then\n'
                    '  echo "sqlite3: sqlite3.c:888: vdbeExec: Assertion \\`no_base_bug != 0\' failed." >&2\n'
                    "  exit 134\n"
                    "fi\n"
                    "exit 0\n"
                )
            os.chmod(fake_sqlite, 0o755)

            out_dir = os.path.join(td, "out_skip")
            base_cmd = [
                sys.executable,
                BATCH_EVAL,
                "--corpus-dir",
                cdir,
                "--profiles",
                "Ours-Full",
                "--repeats",
                "1",
                "--jobs",
                "1",
                "--local-embedded",
                "--engine-bin",
                fake_sqlite,
                "--occam-bin",
                occam_bin,
                "--out-dir",
                out_dir,
            ]
            res1 = subprocess.run(
                [*base_cmd, "--replays", "2"],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res1.returncode, 0, res1.stderr + "\n" + res1.stdout)
            jres_path = os.path.join(out_dir, "runs", "c1__Ours-Full__r1", "job_result.json")
            with open(jres_path, encoding="utf-8") as h:
                j1 = json.load(h)
            self.assertEqual(j1["state"], "skipped_unstable_input")

            os.unlink(flaky_flag)
            res2 = subprocess.run(
                [*base_cmd, "--replays", "5", "--skip-input-replay"],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res2.returncode, 0, res2.stderr + "\n" + res2.stdout)
            with open(jres_path, encoding="utf-8") as h:
                j2 = json.load(h)
            self.assertEqual(j2["state"], "completed")
            self.assertIsNone(j2["reduced_replay_verification"]["matches_baseline"])
            with open(os.path.join(out_dir, "aggregated_stats.json"), encoding="utf-8") as h:
                agg2 = json.load(h)
            p2 = agg2["by_profile"]["Ours-Full"]
            self.assertEqual(p2["completed_runs"], 1)
            self.assertEqual(p2["reduced_replay_5of5_runs"], 1)
            self.assertEqual(p2["reduced_replay_zero_slippage_runs"], 0)


if __name__ == "__main__":
    unittest.main()

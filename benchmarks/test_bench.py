#!/usr/bin/env python3
"""Unit tests for benchmarks/bench.py (Tier 2/3/4 unified runner)."""
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import bench

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CORPUS = os.path.join(ROOT, "benchmarks", "corpus")
TIER4 = os.path.join(ROOT, "benchmarks", "real_fuzz_corpus")

CASE_SQL = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t VALUES (1, 10), (2, 20);\nSELECT a FROM t ORDER BY a;\n"


def contract_for(sql, **over):
    rows, digest = bench.execute_sql(sql)
    c = {"expected_rows": rows, "state_sha256": digest, "check_state": False}
    c.update(over)
    return c


class TestOracle(unittest.TestCase):
    def test_execute_sql_deterministic(self):
        self.assertEqual(bench.execute_sql(CASE_SQL), bench.execute_sql(CASE_SQL))
        rows, digest = bench.execute_sql(CASE_SQL)
        self.assertEqual(rows, [[1], [2]])
        self.assertEqual(len(digest), 64)

    def test_accepts_original_corpus_case(self):
        sql = Path(CORPUS, "07_column_explicit.sql").read_text()
        self.assertTrue(bench.oracle_accepts(sql, contract_for(sql)))

    def test_rejects_wrong_rowset(self):
        c = contract_for(CASE_SQL)
        c["expected_rows"] = [[1], [2], [3]]
        self.assertFalse(bench.oracle_accepts(CASE_SQL, c))

    def test_rejects_empty(self):
        self.assertFalse(bench.oracle_accepts("", contract_for(CASE_SQL)))
        self.assertFalse(bench.oracle_accepts("  \n ", contract_for(CASE_SQL)))

    def test_rejects_sql_error(self):
        self.assertFalse(bench.oracle_accepts("SYNTAX ERROR;", contract_for(CASE_SQL)))

    def test_check_state(self):
        rows, digest = bench.execute_sql(CASE_SQL)
        ok = {"expected_rows": rows, "state_sha256": digest, "check_state": True}
        self.assertTrue(bench.oracle_accepts(CASE_SQL, ok))
        bad = dict(ok, state_sha256="0" * 64)
        self.assertFalse(bench.oracle_accepts(CASE_SQL, bad))

    def test_rowset_only_ignores_digest(self):
        c = contract_for(CASE_SQL, state_sha256="0" * 64)
        self.assertFalse(c["check_state"])
        self.assertTrue(bench.oracle_accepts(CASE_SQL, c))
        rows, digest = bench.execute_sql(CASE_SQL, want_digest=False)
        self.assertEqual(rows, [[1], [2]])
        self.assertIsNone(digest)

    def test_cli_exit_codes(self):
        with tempfile.TemporaryDirectory() as td:
            cp = os.path.join(td, "c.json")
            Path(cp).write_text(json.dumps(contract_for(CASE_SQL)))
            for sql, want in [(CASE_SQL, 0), ("", 1), ("SYNTAX ERROR;", 1)]:
                p = subprocess.run([sys.executable, bench.BENCH_PY, "oracle", cp],
                                   check=False, input=sql, capture_output=True, text=True)
                self.assertEqual(p.returncode, want, sql)


class TestPrepare(unittest.TestCase):
    def test_gen_scale_deterministic(self):
        self.assertEqual(bench.gen_scale(8), bench.gen_scale(8))
        rows, _ = bench.execute_sql(bench.gen_scale(8))
        self.assertEqual(rows, [[8, 36]])

    def test_prepare_real_corpus(self):
        with tempfile.TemporaryDirectory() as td:
            rc = bench.cmd_prepare(_ns(out=td, corpus=CORPUS, scales="8"))
            self.assertEqual(rc, 0)
            manifest = json.loads(Path(td, "manifest.json").read_text())
            names = [c["name"] for c in manifest["cases"]]
            self.assertEqual(len(names), 10)  # 9 corpus + scale_008
            self.assertIn("scale_008", names)
            for case in manifest["cases"]:
                sql = os.path.join(td, case["sql"])
                con = os.path.join(td, case["contract"])
                self.assertTrue(os.path.exists(sql))
                c = json.loads(Path(con).read_text())
                self.assertTrue(c["expected_rows"])  # empty control needs rows
                self.assertTrue(bench.oracle_accepts(Path(sql).read_text(), c))


class TestAUC(unittest.TestCase):
    def curve(self, *pts):
        return [{"pointCallIndex": i, "pointCandidateBytes": b, "pointAccepted": a}
                for i, b, a in pts]

    def test_step_curve_exact(self):
        c = self.curve((1, 100, True), (2, 50, True))
        self.assertAlmostEqual(bench.compute_attempt_auc(100, c, 4), 0.625)

    def test_immediate_convergence(self):
        c = self.curve((1, 10, True))
        self.assertAlmostEqual(bench.compute_attempt_auc(100, c, 3), 0.1)

    def test_no_acceptance_is_worst(self):
        self.assertAlmostEqual(bench.compute_attempt_auc(100, [], 2), 1.0)

    def test_rejected_points_ignored(self):
        c = self.curve((1, 100, True), (2, 5, False), (3, 60, True))
        self.assertAlmostEqual(bench.compute_attempt_auc(100, c, 3), (1 + 1 + 0.6) / 3)

    def test_degenerate_inputs(self):
        self.assertEqual(bench.compute_attempt_auc(0, [], 5), 1.0)
        self.assertEqual(bench.compute_attempt_auc(100, [], 0), 1.0)


class TestSchedule(unittest.TestCase):
    def test_ab_ba_interleave(self):
        self.assertEqual(bench.build_schedule(["a", "b"], 3),
                         [(1, "a"), (1, "b"), (2, "b"), (2, "a"), (3, "a"), (3, "b")])

    def test_single_arm_single_rep(self):
        self.assertEqual(bench.build_schedule(["solo"], 1), [(1, "solo")])


def stats_fixture():
    return {
        "initial_bytes": 1000, "reduced_bytes": 200, "reduction_ratio": 0.2,
        "reduced_statements": 2, "oracle_attempts": 4, "cache_hits": 3,
        "elapsed_seconds": 9.5, "status": "completed",
        "evaluation_metrics": {
            "evaluationAcceptedObservations": 2,
            "evaluationReductionSeconds": 7.25,
            "evaluationCurve": [
                {"pointCallIndex": 1, "pointCandidateBytes": 1000, "pointAccepted": True},
                {"pointCallIndex": 2, "pointCandidateBytes": 500, "pointAccepted": True},
                {"pointCallIndex": 3, "pointCandidateBytes": 100, "pointAccepted": False},
                {"pointCallIndex": 4, "pointCandidateBytes": 200, "pointAccepted": True},
            ],
            "evaluationCostAttribution": {
                "snapshotParseFfiMicros": {"statement-only": 100, "syntax-chunks": 50},
                "snapshotParseDecodeMicros": {"statement-only": 25},
                "snapshotFamilies": [
                    {
                        "tag": {"kind": "statement"},
                        "counters": {"candidateBuildMicros": 75},
                    },
                    {
                        "tag": {"kind": "unknown"},
                        "counters": {"candidateBuildMicros": 0},
                    },
                ],
            },
        },
    }


class TestReport(unittest.TestCase):
    def test_report_four_dimensions(self):
        with tempfile.TemporaryDirectory() as td:
            rd = os.path.join(td, "caseA__default__r1")
            os.makedirs(rd)
            Path(rd, "stats.json").write_text(json.dumps(stats_fixture()))
            os.makedirs(os.path.join(td, "caseA__intra__r1"))  # missing stats
            self.assertEqual(bench.cmd_report(_ns(run_dir=td)), 0)
            summary = json.loads(Path(td, "summary.json").read_text())
            self.assertEqual(len(summary["runs"]), 2)
            good = next(r for r in summary["runs"] if r["run"].startswith("caseA__default"))
            self.assertEqual((good["case"], good["arm"], good["rep"]), ("caseA", "default", "r1"))
            # Quality
            self.assertEqual((good["reduced_bytes"], good["reduced_statements"],
                              good["reduction_ratio"]), (200, 2, 0.2))
            # Search
            self.assertEqual((good["oracle_attempts"], good["accepted"],
                              good["cache_hits"]), (4, 2, 3))
            self.assertAlmostEqual(good["attempt_auc"], (1 + 0.5 + 0.5 + 0.2) / 4)
            # Overhead: 100 + 50 + 25 + 75
            self.assertEqual(good["engine_overhead_us"], 250)
            # Throughput prefers evaluationReductionSeconds
            self.assertEqual((good["elapsed_seconds"], good["status"]), (7.25, "completed"))
            bad = next(r for r in summary["runs"] if r["run"].startswith("caseA__intra"))
            self.assertEqual(bad["status"], "missing-stats")
            md = Path(td, "report.md").read_text()
            self.assertIn("| run | reduced_bytes |", md)
            self.assertIn("caseA__default__r1", md)

    def test_overhead_absent_attribution(self):
        self.assertEqual(bench.engine_overhead_us({"evaluation_metrics": {}}), 0)


class TestCrashCmd(unittest.TestCase):
    def test_meta_parsing(self):
        meta = os.path.join(TIER4, "pg_fuzzer_mcv_clauses.meta.json")
        cmd = bench.build_crash_cmd(meta, occam="occam", out="/tmp/r")
        self.assertEqual(cmd[0], "occam")
        self.assertTrue(cmd[1].endswith("pg_fuzzer_mcv_clauses.sql"))
        oi = cmd.index("--oracle") + 1
        self.assertIn("harness/crash_oracle.py", cmd[oi])
        self.assertIn("--dbms postgres", cmd[oi])
        self.assertIn("mcv", cmd[oi])
        ri = cmd.index("--reset") + 1
        self.assertIn("harness/reset_db.sh --dbms postgres", cmd[ri])
        self.assertEqual(cmd[cmd.index("--dialect") + 1], "postgres")
        self.assertEqual(cmd[cmd.index("--out") + 1], "/tmp/r")

    def test_cli_prints(self):
        meta = os.path.join(TIER4, "pg_fuzzer_mcv_clauses.meta.json")
        p = subprocess.run([sys.executable, bench.BENCH_PY, "crash-cmd",
                            "--meta", meta, "--out", "/tmp/r"],
                           check=False, capture_output=True, text=True)
        self.assertEqual(p.returncode, 0)
        self.assertIn("crash_oracle.py", p.stdout)


class _ns:
    def __init__(self, **kw):
        self.__dict__.update(kw)


if __name__ == "__main__":
    unittest.main()

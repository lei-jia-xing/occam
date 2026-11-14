import json
import os
from pathlib import Path
import sqlite3
import signal
import subprocess
import sys
import tempfile
import textwrap
import time
import unittest
from contextlib import closing
from unittest.mock import patch

from benchmarks.tools import profile_isolation_probe as isolation_probe
from benchmarks.tools import scale_study as study


class SQLiteStudyResetTests(unittest.TestCase):
    def test_template_restores_schema_data_sequence_and_pragma(self):
        with tempfile.TemporaryDirectory(prefix="occam-study-reset-") as directory:
            root = Path(directory)
            work = root / "observation"
            work.mkdir()
            template = root / "template.sqlite"
            metadata = root / "case.json"
            case = {
                "reset_mode": "template",
                "marker": "reset-case",
                "template": str(template),
                "expected_rows": [[42]],
                "setup_sql": """
CREATE TABLE t(x INT);
INSERT INTO t VALUES(42);
CREATE INDEX idx ON t(x);
CREATE TABLE audit(x INT);
CREATE TRIGGER tr AFTER INSERT ON t BEGIN INSERT INTO audit VALUES(NEW.x); END;
CREATE TABLE ids(id INTEGER PRIMARY KEY AUTOINCREMENT);
INSERT INTO ids DEFAULT VALUES;
PRAGMA user_version=42;
""",
            }
            case["state_signature"] = study.seed_database(template, case)
            metadata.write_text(json.dumps(case), encoding="utf-8")
            environment = dict(os.environ, OCCAM_WORKDIR=str(work))

            def invoke(mode, sql=""):
                return subprocess.run(
                    [sys.executable, str(study.SCRIPT), mode, str(metadata)],
                    input=sql, text=True, capture_output=True, env=environment, timeout=5,
                ).returncode

            self.assertEqual(0, invoke("reset"))
            self.assertEqual(0, invoke("oracle", "SELECT x FROM t;"))
            self.assertEqual(1, invoke("oracle", "SELECT 0 FROM t;"))
            with closing(sqlite3.connect(work / "study.sqlite", isolation_level=None)) as connection:
                connection.executescript("""
DELETE FROM t;
DROP INDEX idx;
DROP TRIGGER tr;
UPDATE sqlite_sequence SET seq=999 WHERE name='ids';
PRAGMA user_version=7;
""")
            self.assertEqual(125, invoke("oracle", "SELECT x FROM t;"))
            self.assertEqual(0, invoke("reset"))
            self.assertEqual(0, invoke("oracle", "SELECT x FROM t;"))


    def test_repeated_profiles_reset_fixture_and_isolate_artifacts(self):
        configured = os.environ.get("OCCAM_EVAL")
        candidates = (
            [Path(configured)] if configured
            else list((study.ROOT / "dist-newstyle").glob(
                "build/*/*/occam-*/build/occam-eval/occam-eval"
            ))
        )
        binaries = [path for path in candidates if path.is_file() and os.access(path, os.X_OK)]
        if not binaries:
            self.skipTest("built occam-eval is unavailable")
        binary = max(binaries, key=lambda path: path.stat().st_mtime)
        with tempfile.TemporaryDirectory(prefix="occam-profile-isolation-") as directory:
            report = isolation_probe.run_probe(
                binary, Path(directory) / "evidence", timeout=2, budget=10,
                executions=100, outer_timeout=30,
            )
            self.assertTrue(report["qualified"], report["failures"])

    def test_outer_timeout_cleans_descendant_and_retains_next_child(self):
        if not sys.platform.startswith("linux") or not Path("/proc").is_dir():
            self.skipTest("process-tree cleanup probe requires Linux /proc")
        with tempfile.TemporaryDirectory(prefix="occam-scale-timeout-") as directory:
            root = Path(directory)
            sql = root / "input.sql"
            sql.write_text("SELECT 1;\n", encoding="utf-8")
            manifest = root / "manifest.json"
            manifest.write_text(json.dumps({"cases": [{
                "id": "timeout-then-complete",
                "file": str(sql),
                "dialect": "sqlite",
                "evidence_class": "generated_db_execution",
                "oracle_kind": "sqlite_rowset",
                "oracle_command": "exit 0",
                "parameters": {},
            }]}), encoding="utf-8")
            count_file = root / "worker-count.txt"
            pid_file = root / "descendant.pid"
            children_file = root / "children.jsonl"
            worker = root / "worker"
            worker.write_text(textwrap.dedent(r'''
                #!/usr/bin/env python3
                import json
                import os
                from pathlib import Path
                import subprocess
                import sys
                import time

                args = sys.argv[1:]
                out = Path(args[args.index("--out") + 1])
                out.mkdir(parents=True, exist_ok=True)
                count_path = Path(os.environ["SCALE_STUDY_WORKER_COUNT"])
                count = int(count_path.read_text()) + 1 if count_path.exists() else 1
                count_path.write_text(str(count))
                with Path(os.environ["SCALE_STUDY_WORKER_CHILDREN"]).open("a") as stream:
                    stream.write(json.dumps({"repetition": count, "out": str(out)}) + "\n")
                sql = Path(args[0]).read_bytes()
                profile = args[args.index("--profile") + 1]
                minimal = out / "minimal.sql"
                minimal.write_bytes(sql)
                metrics_path = out / "profile.json"
                metric = {
                    "metricAlgorithm": profile,
                    "metricExperimentLine": "internal",
                    "metricInitialBytes": len(sql),
                    "metricReducedBytes": len(sql),
                    "metricByteReduction": 0.0,
                    "metricInitialStmts": 1,
                    "metricReducedStmts": 1,
                    "metricStmtReduction": 0.0,
                    "metricOracleCalls": 1,
                    "metricTimeSeconds": 0.001,
                    "metricEvaluationMetrics": {
                        "evaluationStatus": "completed",
                        "evaluationFailure": None,
                        "evaluationCacheUsed": True,
                        "evaluationCacheHits": 1,
                        "evaluationCacheMisses": 1,
                        "evaluationDatabaseObservations": 1,
                        "evaluationTimedOutObservations": 0,
                        "evaluationValidationObservations": 2,
                        "evaluationValidationDatabaseObservations": 2,
                        "evaluationValidationPassed": True,
                        "evaluationValidationSeconds": 0.001,
                        "evaluationResetSeconds": 0.0,
                        "evaluationReductionSeconds": 0.001,
                        "evaluationTotalSeconds": 0.002,
                        "evaluationBudget": {"evaluationPreflightAttempts": 1},
                    },
                    "metricReductionSettings": {},
                    "metricSQLArtifact": str(minimal),
                    "metricMetricsArtifact": str(metrics_path),
                }
                metrics_path.write_text(json.dumps(metric))
                (out / "benchmark.json").write_text(json.dumps([metric]))
                if count == 1:
                    descendant = subprocess.Popen(
                        [sys.executable, "-c", "import time; time.sleep(60)"],
                        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                    )
                    Path(os.environ["SCALE_STUDY_WORKER_PID"]).write_text(str(descendant.pid))
                    time.sleep(60)
            ''').lstrip(), encoding="utf-8")
            worker.chmod(0o755)
            output = root / "results"
            output.mkdir()
            pid = None

            def process_state(process_id):
                try:
                    stat = Path(f"/proc/{process_id}/stat").read_text(encoding="utf-8")
                except FileNotFoundError:
                    return None
                return stat[stat.rfind(")") + 2]
            try:
                with patch.dict(os.environ, {
                    "SCALE_STUDY_WORKER_COUNT": str(count_file),
                    "SCALE_STUDY_WORKER_PID": str(pid_file),
                    "SCALE_STUDY_WORKER_CHILDREN": str(children_file),
                }):
                    study.profiles(
                        manifest, worker, output, 1, 10, 10, ["full"],
                        repetitions=2, outer_timeout=2,
                    )
                rows = [json.loads(line) for line in (output / "results.jsonl").read_text().splitlines()]
                self.assertEqual(2, len(rows))
                first, second = sorted(rows, key=lambda row: row["repetition"])
                self.assertEqual("timeout", first["status"])
                self.assertEqual("timeout", first["terminal_observation"]["status"])
                self.assertTrue(first["terminal_observation"].get("source"))
                self.assertTrue(first["terminal_observation"].get("reason"))
                self.assertIsNot(first.get("predicate_passed"), True)
                self.assertEqual("completed", second["status"])
                self.assertEqual("completed", second["terminal_observation"]["status"])
                self.assertNotEqual(first["child_id"], second["child_id"])
                self.assertLess(first["order"], second["order"])
                self.assertIn("cleanup_status", first)
                self.assertTrue(first["cleanup_status"])
                self.assertIn("cleanup_reason", first)
                self.assertFalse(first["cleanup_reason"])
                self.assertIn("reason", second["terminal_observation"])
                self.assertTrue((output / "run.json").is_file())
                self.assertTrue(pid_file.is_file())
                pid = int(pid_file.read_text(encoding="utf-8"))
                child_records = [json.loads(line) for line in children_file.read_text().splitlines()]
                first_out = Path(next(row["out"] for row in child_records if row["repetition"] == 1))
                second_out = Path(next(row["out"] for row in child_records if row["repetition"] == 2))
                self.assertNotEqual(first_out, second_out)
                stale_metric = json.loads((first_out / "benchmark.json").read_text())[0]
                self.assertEqual("completed", stale_metric["metricEvaluationMetrics"]["evaluationStatus"])
                self.assertTrue((first_out / "minimal.sql").is_file())


                deadline = time.monotonic() + 3
                state = process_state(pid)
                while state not in (None, "Z") and time.monotonic() < deadline:
                    time.sleep(0.05)
                    state = process_state(pid)
                self.assertIn(state, (None, "Z"), "ordinary descendant remained alive after outer timeout")
            finally:
                if pid is None and pid_file.is_file():
                    pid = int(pid_file.read_text(encoding="utf-8"))
                if pid is not None:
                    try:
                        if process_state(pid) not in (None, "Z"):
                            os.kill(pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass


    def test_malformed_metrics_and_nonzero_child_do_not_hide_later_cases(self):
        with tempfile.TemporaryDirectory(prefix="occam-scale-artifacts-") as directory:
            root = Path(directory)
            cases = []
            for case_id in ("malformed", "missing", "nonzero", "good"):
                sql = root / f"{case_id}.sql"
                sql.write_text("SELECT 1;\n", encoding="utf-8")
                cases.append({
                    "id": case_id, "file": str(sql), "dialect": "sqlite",
                    "evidence_class": "generated_db_execution", "oracle_kind": "sqlite_rowset",
                    "oracle_command": "exit 0", "parameters": {},
                })
            manifest = root / "manifest.json"
            manifest.write_text(json.dumps({"cases": cases}), encoding="utf-8")
            worker = root / "worker"
            worker.write_text(textwrap.dedent(r'''
                #!/usr/bin/env python3
                import json
                from pathlib import Path
                import sys

                args = sys.argv[1:]
                source = Path(args[0])
                out = Path(args[args.index("--out") + 1])
                out.mkdir(parents=True, exist_ok=True)
                profile = args[args.index("--profile") + 1]
                sql = source.read_bytes()
                minimal = out / "minimal.sql"
                minimal.write_bytes(sql)
                scenario = source.stem
                if scenario == "malformed":
                    (out / "benchmark.json").write_bytes(bytes([255]))
                    raise SystemExit(0)
                if scenario == "missing":
                    raise SystemExit(0)
                metrics_path = out / "profile.json"
                metric = {
                    "metricAlgorithm": profile,
                    "metricExperimentLine": "internal",
                    "metricInitialBytes": len(sql),
                    "metricReducedBytes": len(sql),
                    "metricByteReduction": 0.0,
                    "metricInitialStmts": 1,
                    "metricReducedStmts": 1,
                    "metricStmtReduction": 0.0,
                    "metricOracleCalls": 1,
                    "metricTimeSeconds": 0.001,
                    "metricEvaluationMetrics": {
                        "evaluationStatus": "completed",
                        "evaluationFailure": None,
                        "evaluationCacheUsed": True,
                        "evaluationCacheHits": 1,
                        "evaluationCacheMisses": 1,
                        "evaluationOracleAttempts": 1,
                        "evaluationDatabaseObservations": 1,
                        "evaluationTimedOutObservations": 0,
                        "evaluationValidationObservations": 2,
                        "evaluationValidationDatabaseObservations": 2,
                        "evaluationValidationPassed": True,
                        "evaluationValidationSeconds": 0.001,
                        "evaluationResetSeconds": 0.0,
                        "evaluationReductionSeconds": 0.001,
                        "evaluationTotalSeconds": 0.002,
                        "evaluationBudget": {
                            "evaluationPreflightAttempts": 1,
                            "evaluationPreflightMicros": 1000,
                        },
                    },
                    "metricReductionSettings": {},
                    "metricSQLArtifact": str(minimal),
                    "metricMetricsArtifact": str(metrics_path),
                }
                metrics_path.write_text(json.dumps(metric))
                (out / "benchmark.json").write_text(json.dumps([metric]))
                if scenario == "nonzero":
                    raise SystemExit(7)
            ''').lstrip(), encoding="utf-8")
            worker.chmod(0o755)
            output = root / "results"
            output.mkdir()
            study.profiles(manifest, worker, output, 1, 10, 10, ["full"], outer_timeout=10)
            rows = [json.loads(line) for line in (output / "results.jsonl").read_text().splitlines()]
            self.assertEqual(["malformed", "missing", "nonzero", "good"], [row["case_id"] for row in rows])
            self.assertEqual(["tool-error", "tool-error", "tool-error", "completed"],
                             [row["status"] for row in rows])
            for row in rows[:2]:
                self.assertEqual("outer-runner", row["terminal_observation"]["source"])
                self.assertEqual("tool-error", row["terminal_observation"]["status"])
                self.assertTrue(row["result_artifact_present"])
            nonzero = rows[2]
            self.assertEqual(7, nonzero["native_process_returncode"])
            self.assertIsNot(nonzero.get("predicate_passed"), True)
            stale = json.loads(Path(nonzero["raw_metric_file"]).read_text())[0]
            self.assertEqual("completed", stale["metricEvaluationMetrics"]["evaluationStatus"])
            self.assertTrue((Path(nonzero["artifact_directory"]) / "minimal.sql").is_file())
            self.assertEqual("completed", rows[3]["terminal_observation"]["status"])
            self.assertTrue((output / "run.json").is_file())

    def test_spawn_failures_are_recorded_for_later_children(self):
        with tempfile.TemporaryDirectory(prefix="occam-scale-spawn-") as directory:
            root = Path(directory)
            cases = []
            for case_id in ("spawn-first", "spawn-next"):
                sql = root / f"{case_id}.sql"
                sql.write_text("SELECT 1;\n", encoding="utf-8")
                cases.append({
                    "id": case_id, "file": str(sql), "dialect": "sqlite",
                    "oracle_command": "exit 0",
                })
            manifest = root / "manifest.json"
            manifest.write_text(json.dumps({"cases": cases}), encoding="utf-8")
            output = root / "results"
            output.mkdir()
            study.profiles(
                manifest, root / "missing-binary", output, 1, 10, 10, ["full"],
                repetitions=1, outer_timeout=10,
            )
            rows = [json.loads(line) for line in (output / "results.jsonl").read_text().splitlines()]
            self.assertEqual(["spawn-first", "spawn-next"], [row["case_id"] for row in rows])
            self.assertEqual(["tool-error", "tool-error"], [row["status"] for row in rows])
            self.assertEqual(["tool-error", "tool-error"],
                             [row["terminal_observation"]["status"] for row in rows])
            self.assertEqual(2, len({row["child_id"] for row in rows}))
            self.assertTrue((output / "run.json").is_file())


if __name__ == "__main__":
    unittest.main()

import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile
import time
import unittest

from benchmarks.fuzz_corpus import run_docker_benchmark as runner


ROOT = Path(__file__).resolve().parents[1]


class NativeDialectTests(unittest.TestCase):
    def test_unknown_dialect_is_rejected_instead_of_reinterpreted(self):
        binary = runner.find_binary(ROOT, "exe:occam")
        if binary is None:
            self.skipTest("build exe:occam before running native integration checks")
        with tempfile.TemporaryDirectory(prefix="occam-dialect-") as directory:
            work = Path(directory)
            source = work / "input.sql"
            source.write_text("SELECT 42;\n", encoding="utf-8")
            predicate = shlex.join([
                sys.executable, "-c",
                "import sqlite3,sys; c=sqlite3.connect(':memory:'); "
                "sys.exit(0 if c.execute(sys.stdin.read()).fetchall()==[(42,)] else 1)",
            ])
            entry = {
                "id": "unknown", "file": str(source), "dialect": "unsupported-dialect",
                "evidence_class": "synthetic", "oracle_kind": "sqlite-rowset",
                "oracle_command": predicate,
            }
            record = runner.run_one(
                entry, ROOT, work, "occam", {"occam": binary}, None, work, 1, 15, 30,
            )
            self.assertEqual("tool-error", record["status"])
            self.assertIsNone(record["predicate_passed"])
            self.assertFalse((work / "unknown_occam" / "stats.json").exists())

    def test_native_wrapper_reset_failure_does_not_promote_candidate(self):
        binary = runner.find_binary(ROOT, "exe:occam")
        if binary is None:
            self.skipTest("build exe:occam before running native integration checks")
        with tempfile.TemporaryDirectory(prefix="occam-native-audit-") as directory:
            work = Path(directory)
            source = work / "input.sql"
            original = b"SELECT 0;\nSELECT 42;\n"
            source.write_bytes(original)
            reset = work / "reset.py"
            counter = work / "resets.txt"
            reset.write_text(
                "from pathlib import Path\nimport sys\n"
                "p=Path(sys.argv[1]); n=int(p.read_text())+1 if p.exists() else 1\n"
                "p.write_text(str(n)); sys.exit(1 if n == 4 else 0)\n", encoding="utf-8",
            )
            predicate = shlex.join([
                sys.executable, "-c", "import sys; sys.exit(0 if '42' in sys.stdin.read() else 1)",
            ])
            entry = {
                "id": "reset-failure", "file": str(source), "dialect": "sqlite",
                "evidence_class": "synthetic", "oracle_kind": "text-pattern",
                "oracle_command": predicate,
                "reset_command": [sys.executable, str(reset), str(counter)],
            }
            record = runner.run_one(
                entry, ROOT, work, "occam", {"occam": binary}, None, work, 1, 15, 30,
            )
            audit = runner.read_jsonl(Path(record["oracle_audit_file"]))
            self.assertEqual("accepted", audit[0]["status"], "outer initial preflight did not pass")
            self.assertTrue(any(row["phase"] == "reducer" and row["status"] == "tool-error" for row in audit))
            self.assertEqual("tool-error", record["status"])
            self.assertIsNone(record["predicate_passed"])
            self.assertEqual("initial", record["checkpoint_used"])
            self.assertEqual(original, Path(record["result_file"]).read_bytes())

    def test_reset_infrastructure_status_is_independent_of_error_text(self):
        binary = runner.find_binary(ROOT, "exe:occam")
        if binary is None:
            self.skipTest("build exe:occam before running native integration checks")
        with tempfile.TemporaryDirectory(prefix="occam-validation-status-") as directory:
            work = Path(directory)
            source = work / "input.sql"
            source.write_bytes(b"SELECT 42;\n")
            entry = {
                "id": "reset-error", "file": str(source), "dialect": "sqlite",
                "evidence_class": "synthetic", "oracle_kind": "exit-code",
                "oracle_command": "exit 0",
                "reset_command": "printf 'timed out; budget exhausted\\n' >&2; exit 1",
            }
            record = runner.run_one(
                entry, ROOT, work, "occam", {"occam": binary}, None, work, 1, 15, 30,
            )
            self.assertEqual("tool-error", record["status"])
            self.assertIsNone(record["predicate_passed"])
            self.assertEqual(1, record["validation_calls"])
            self.assertFalse((work / "reset-error_occam" / "stats.json").exists())


class UpstreamTimeoutTests(unittest.TestCase):
    def check_timeout(self, tool, source):
        with tempfile.TemporaryDirectory(prefix=f"occam-{tool}-timeout-") as directory:
            work = Path(directory)
            candidate = work / "input.sql"
            candidate.write_text("SELECT 40 + 2;\nSELECT 43;\nSELECT 44;\n", encoding="utf-8")
            counter = work / "checks.txt"
            checker = work / "checker.py"
            checker.write_text(
                "from pathlib import Path\nimport sys\n"
                "p=Path(sys.argv[1]); n=int(p.read_text())+1 if p.exists() else 1\n"
                "p.write_text(str(n)); sys.exit(0 if n <= 2 else 124)\n", encoding="utf-8",
            )
            output = work / "result.sql"
            metrics = work / "metrics.json"
            if tool == "sqlreduce":
                command = [
                    sys.executable, str(ROOT / "benchmarks/upstream/sqlreduce_adapter.py"),
                    "--source", str(source), "--input", str(candidate), "--output", str(output),
                    "--metrics", str(metrics), "--timeout", "2", "--deadline", str(time.monotonic() + 20),
                ]
            else:
                command = [
                    str(ROOT / "benchmarks/upstream/sqlancer_adapter.sh"),
                    "--input", str(candidate), "--output", str(output), "--metrics", str(metrics),
                    "--calls", str(work / "calls.jsonl"), "--timeout-ms", "2000", "--budget-ms", "20000",
                ]
            result = subprocess.run(
                [*command, "--", sys.executable, str(checker), str(counter)],
                cwd=work, capture_output=True, text=True, timeout=30,
            )
            self.assertEqual(124, result.returncode, result.stderr)
            self.assertEqual(3, int(counter.read_text()), "search continued after an inconclusive callback")
            detail = json.loads(metrics.read_text())
            if tool == "sqlreduce":
                self.assertEqual("timeout", detail["status"])
            else:
                self.assertEqual(124, detail["exit_code"])

    def test_sqlreduce_stops_on_post_initial_callback_timeout(self):
        source = Path(os.environ.get("SQLREDUCE_SOURCE", Path.home() / ".cache/occam" / f"sqlreduce-{runner.SQLREDUCE_PIN}"))
        if not source.is_dir():
            self.skipTest("prepare pinned SQLREDUCE_SOURCE and pglast dependencies")
        self.check_timeout("sqlreduce", source)

    def test_sqlancer_stops_on_post_initial_callback_timeout(self):
        source = Path(os.environ.get("SQLANCER_SOURCE", Path.home() / ".cache/occam" / f"sqlancer-{runner.SQLANCER_PIN}"))
        if not (source / "target/sqlancer-2.0.0.jar").is_file() or not (source / "target/lib").is_dir():
            self.skipTest("build pinned SQLANCER_SOURCE before running upstream integration checks")
        self.check_timeout("sqlancer", source)


if __name__ == "__main__":
    unittest.main()

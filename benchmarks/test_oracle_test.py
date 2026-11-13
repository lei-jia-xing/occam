import json
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile
import time
import unittest


class SharedCheckerTests(unittest.TestCase):
    def test_execution_limit_preserves_accepted_sql_without_another_database_call(self):
        upstream = Path(__file__).with_name("upstream")
        with tempfile.TemporaryDirectory(prefix="occam-checker-budget-") as directory:
            root = Path(directory)
            source = root / "candidate.sql"
            checkpoint = root / "checkpoint.sql"
            counter = root / "executions.txt"
            predicate = root / "predicate.py"
            predicate.write_text(
                "import sqlite3,sys\n"
                "with open(sys.argv[1], 'a') as output: output.write('executed\\n')\n"
                "connection=sqlite3.connect(':memory:')\n"
                "rows=connection.execute(sys.stdin.read()).fetchall()\n"
                "connection.close()\n"
                "sys.exit(0 if rows == [(42,)] else 1)\n", encoding="utf-8",
            )
            oracle = shlex.join([
                sys.executable, str(upstream / "oracle_exec.py"),
                "--command", shlex.join([sys.executable, str(predicate), str(counter)]),
                "--audit", str(root / "audit.jsonl"), "--timeout", "2", "--cwd", str(root),
            ])
            command = [
                sys.executable, str(upstream / "oracle_test.py"), "--candidate", str(source),
                "--checkpoint", str(checkpoint), "--calls", str(root / "calls.jsonl"),
                "--cache", str(root / "cache.json"), "--timeout", "3", "--deadline", str(time.monotonic() + 10),
                "--oracle", oracle, "--max-executions", "1", "--no-cache",
            ]
            source.write_bytes(b"SELECT 42;\n")
            first = subprocess.run(command, capture_output=True, timeout=5)
            self.assertEqual(0, first.returncode)
            source.write_bytes(b"SELECT 40+2;\n")
            second = subprocess.run(command, capture_output=True, timeout=5)
            self.assertEqual(126, second.returncode)
            self.assertEqual(b"SELECT 42;\n", checkpoint.read_bytes())
            self.assertEqual(["executed"], counter.read_text().splitlines())
            records = [json.loads(line) for line in (root / "calls.jsonl").read_text().splitlines()]
            self.assertEqual(["accepted", "execution-limit"], [record["status"] for record in records])

    def test_raw_125_rejection_and_wrapper_failure_have_distinct_checker_statuses(self):
        upstream = Path(__file__).with_name("upstream")
        with tempfile.TemporaryDirectory(prefix="occam-checker-status-") as directory:
            root = Path(directory)
            source = root / "candidate.sql"
            source.write_bytes(b"SELECT 42;\n")
            for name, command, reset, expected in [
                ("rejected", "exit 125", None, 1),
                ("infrastructure", "exit 0", "exit 1", 125),
            ]:
                with self.subTest(name=name):
                    audit = root / f"{name}.audit.jsonl"
                    wrapper = [
                        sys.executable, str(upstream / "oracle_exec.py"),
                        "--command", command, "--audit", str(audit), "--timeout", "2", "--cwd", str(root),
                    ]
                    if reset:
                        wrapper += ["--reset", reset]
                    result = subprocess.run(
                        [
                            sys.executable, str(upstream / "oracle_test.py"), "--candidate", str(source),
                            "--checkpoint", str(root / f"{name}.sql"), "--calls", str(root / f"{name}.calls.jsonl"),
                            "--cache", str(root / f"{name}.cache.json"), "--timeout", "3",
                            "--deadline", str(time.monotonic() + 10), "--oracle", shlex.join(wrapper),
                            "--oracle-audit", str(audit),
                        ],
                        capture_output=True, timeout=5,
                    )
                    self.assertEqual(expected, result.returncode, result.stderr)

    def test_explicit_audit_requires_a_fresh_observation(self):
        upstream = Path(__file__).with_name("upstream")
        with tempfile.TemporaryDirectory(prefix="occam-checker-missing-audit-") as directory:
            root = Path(directory)
            source = root / "candidate.sql"
            source.write_bytes(b"SELECT 42;\n")
            result = subprocess.run(
                [
                    sys.executable, str(upstream / "oracle_test.py"), "--candidate", str(source),
                    "--checkpoint", str(root / "checkpoint.sql"), "--calls", str(root / "calls.jsonl"),
                    "--cache", str(root / "cache.json"), "--timeout", "2",
                    "--deadline", str(time.monotonic() + 10), "--oracle", "true",
                    "--oracle-audit", str(root / "missing.jsonl"),
                ],
                capture_output=True, timeout=5,
            )
            self.assertEqual(125, result.returncode, result.stderr)
            self.assertFalse((root / "checkpoint.sql").exists())


if __name__ == "__main__":
    unittest.main()

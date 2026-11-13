import json
import shlex
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


class OracleExecTests(unittest.TestCase):
    def test_raw_predicate_exit_125_is_rejection_not_infrastructure(self):
        wrapper = Path(__file__).with_name("upstream") / "oracle_exec.py"
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            audit = root / "oracle.jsonl"
            result = subprocess.run(
                [
                    sys.executable, str(wrapper), "--command",
                    "printf 'arbitrary noisy output\\n'; printf 'opaque stderr\\n' >&2; exit 125",
                    "--audit", str(audit), "--timeout", "2", "--cwd", str(root),
                ],
                input=b"SELECT 1;\n", capture_output=True, timeout=5,
            )
            record = json.loads(audit.read_text(encoding="utf-8"))
            self.assertEqual(125, result.returncode)
            self.assertEqual("rejected", record["status"])

    def test_wrapper_reset_failure_is_typed_and_does_not_launch_predicate(self):
        wrapper = Path(__file__).with_name("upstream") / "oracle_exec.py"
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            audit = root / "oracle.jsonl"
            sentinel = root / "launched"
            result = subprocess.run(
                [
                    sys.executable, str(wrapper), "--command", f"touch {shlex.quote(str(sentinel))}",
                    "--reset", "printf 'opaque reset output\\n'; exit 1",
                    "--audit", str(audit), "--timeout", "2", "--cwd", str(root),
                ],
                input=b"SELECT 1;\n", capture_output=True, timeout=5,
            )
            self.assertEqual(125, result.returncode)
            record = json.loads(audit.read_text(encoding="utf-8"))
            self.assertEqual("tool-error", record["status"])
            self.assertEqual("failed", record["reset_status"])
            self.assertFalse(sentinel.exists())

    def test_non_utf8_output_is_bounded_and_readable_to_text_consumers(self):
        wrapper = Path(__file__).with_name("upstream") / "oracle_exec.py"
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            candidate = root / "candidate.sql"
            candidate.write_bytes(b"SELECT 1;\n")
            audit = root / "oracle.jsonl"
            child = (
                "import sys; "
                "sys.stdout.buffer.write(b'\\xff' * 1048576); "
                "sys.stderr.buffer.write(b'\\xfe' * 1048576)"
            )
            command = f"{shlex.quote(sys.executable)} -c {shlex.quote(child)}"
            result = subprocess.run(
                [
                    sys.executable,
                    str(wrapper),
                    "--command",
                    command,
                    "--audit",
                    str(audit),
                    "--timeout",
                    "5",
                    "--cwd",
                    str(root),
                    str(candidate),
                ],
                capture_output=True,
                text=True,
                encoding="utf-8",
                timeout=10,
            )

            record = json.loads(audit.read_text(encoding="utf-8"))
            self.assertEqual(result.returncode, 0)
            self.assertEqual(record["status"], "accepted")
            self.assertEqual(record["returncode"], 0)
            self.assertLessEqual(len(result.stdout), 8192)
            self.assertLessEqual(len(result.stderr), 8192)
            self.assertLessEqual(len(record["stdout"]), 8192)
            self.assertLessEqual(len(record["stderr"]), 8192)


if __name__ == "__main__":
    unittest.main()

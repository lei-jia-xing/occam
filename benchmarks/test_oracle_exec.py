import json
import shlex
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


class OracleExecTests(unittest.TestCase):
    def test_early_marker_survives_bounded_tail(self):
        wrapper = Path(__file__).with_name("upstream") / "oracle_exec.py"
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            candidate = root / "candidate.sql"
            candidate.write_bytes(b"SELECT 1;\n")
            audit = root / "oracle.jsonl"
            child = (
                "import sys; "
                "sys.stdout.buffer.write(b'OCCAM_INFRASTRUCTURE: early marker' + bytes([10])); "
                "sys.stdout.buffer.write(b'x' * 2097152)"
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
                timeout=10,
            )

            record = json.loads(audit.read_text(encoding="utf-8"))
            self.assertEqual(result.returncode, 125)
            self.assertEqual(record["status"], "tool-error")
            self.assertIn("OCCAM_INFRASTRUCTURE:", record["stdout_diagnostic"])
            self.assertNotIn("OCCAM_INFRASTRUCTURE:", record["stdout"])
            self.assertLessEqual(len(record["stdout"].encode()), 8192)
            self.assertLessEqual(len(record["stdout_diagnostic"].encode()), 8192)
            self.assertIn(b"OCCAM_INFRASTRUCTURE:", result.stdout)
            self.assertLess(len(result.stdout), 16384)

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

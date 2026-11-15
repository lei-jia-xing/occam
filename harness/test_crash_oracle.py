#!/usr/bin/env python3
"""
Unit tests for harness/crash_oracle.py.
Verifies return code semantics according to Occam's oracle contract:
  - Exit 0: Matching crash signature reproduced.
  - Exit 1: Normal query completion, SQL syntax error, or mismatched signature.
  - Exit 124: Timeout.
"""

import os
import subprocess
import tempfile
import unittest


class TestCrashOracle(unittest.TestCase):
    def setUp(self):
        self.oracle_script = os.path.abspath(os.path.join(os.path.dirname(__file__), "crash_oracle.py"))

    def run_oracle(self, sql, args=None, env=None):
        cmd = [self.oracle_script] + (args or [])
        run_env = os.environ.copy()
        if env:
            run_env.update(env)
        proc = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            env=run_env,
        )
        stdout, stderr = proc.communicate(input=sql)
        return proc.returncode, stdout, stderr

    def test_empty_sql_rejected(self):
        code, stdout, stderr = self.run_oracle("")
        self.assertEqual(code, 1)

    def test_whitespace_sql_rejected(self):
        code, stdout, stderr = self.run_oracle("   \n\t  ")
        self.assertEqual(code, 1)

    def test_matching_signature_in_log(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = os.path.join(td, "postgresql.log")
            # Create initially empty log file
            with open(log_path, "w") as f:
                f.write("Log started\n")

            # Create mock psql that appends crash to log and exits with connection loss
            psql_path = os.path.join(td, "psql")
            with open(psql_path, "w") as f:
                f.write(f"""#!/bin/sh
echo '2026-11-15 12:00:00 UTC [1234] TRAP: FailedAssertion("!(IsA(node, TargetEntry))", File: "subselect.c", Line: 2883)' >> {log_path}
echo 'server closed the connection unexpectedly' >&2
exit 2
""")
            os.chmod(psql_path, 0o755)

            env = {"PATH": f"{td}:{os.environ.get('PATH', '')}"}
            code, out, err = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file", log_path,
                    "--expected-signature", "FailedAssertion.*subselect.c",
                    "--verbose",
                ],
                env=env,
            )
            self.assertEqual(code, 0)

    def test_mismatched_signature_in_log_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = os.path.join(td, "postgresql.log")
            with open(log_path, "w") as f:
                f.write("Log started\n")

            psql_path = os.path.join(td, "psql")
            with open(psql_path, "w") as f:
                f.write(f"""#!/bin/sh
echo '2026-11-15 12:00:00 UTC [1234] TRAP: FailedAssertion("!(bms_is_member(i, rels))", File: "initsplan.c", Line: 512)' >> {log_path}
echo 'server closed the connection unexpectedly' >&2
exit 2
""")
            os.chmod(psql_path, 0o755)

            env = {"PATH": f"{td}:{os.environ.get('PATH', '')}"}
            # Expected signature does NOT match initsplan.c
            code, out, err = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file", log_path,
                    "--expected-signature", "subselect.c",
                    "--verbose",
                ],
                env=env,
            )
            self.assertEqual(code, 1)

    def test_normal_query_completion_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            psql_path = os.path.join(td, "psql")
            with open(psql_path, "w") as f:
                f.write("#!/bin/sh\necho '1 row selected'\nexit 0\n")
            os.chmod(psql_path, 0o755)

            env = {"PATH": f"{td}:{os.environ.get('PATH', '')}"}
            code, out, err = self.run_oracle("SELECT 1;", env=env)
            self.assertEqual(code, 1)

    def test_sql_syntax_error_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            psql_path = os.path.join(td, "psql")
            with open(psql_path, "w") as f:
                f.write("#!/bin/sh\necho 'ERROR: syntax error at or near \"BAD\"' >&2\nexit 0\n")
            os.chmod(psql_path, 0o755)

            env = {"PATH": f"{td}:{os.environ.get('PATH', '')}"}
            code, out, err = self.run_oracle("BAD SQL;", env=env)
            self.assertEqual(code, 1)

    def test_timeout_returns_124(self):
        with tempfile.TemporaryDirectory() as td:
            psql_path = os.path.join(td, "psql")
            with open(psql_path, "w") as f:
                f.write("#!/bin/sh\nsleep 5\nexit 0\n")
            os.chmod(psql_path, 0o755)

            env = {"PATH": f"{td}:{os.environ.get('PATH', '')}"}
            code, out, err = self.run_oracle(
                "SELECT pg_sleep(10);",
                args=["--timeout", "0.5"],
                env=env,
            )
            self.assertEqual(code, 124)


if __name__ == "__main__":
    unittest.main()

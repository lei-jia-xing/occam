#!/usr/bin/env python3
"""
Unit tests for harness/crash_oracle.py.

Contract under Occam:
  - Exit 0: matching crash signature / crash indicator in the server log.
  - Any nonzero exit: rejected.
  - Exit 124 is only a standalone client-timeout convention; Occam treats it
    as a rejection, so the oracle disables its own timeout by default.

These tests use mock psql/mysql/docker binaries and never need a real DBMS.
"""

import json
import os
import shlex
import subprocess
import tempfile
import unittest


class TestCrashOracle(unittest.TestCase):
    def setUp(self):
        self.oracle_script = os.path.abspath(
            os.path.join(os.path.dirname(__file__), "crash_oracle.py")
        )

    def run_oracle(self, sql, args=None, env=None):
        cmd = [self.oracle_script] + (args or [])
        run_env = os.environ.copy()
        for key in (
            "DBMS",
            "CONTAINER_NAME",
            "EXPECTED_SIGNATURE",
            "ORACLE_TIMEOUT",
            "ORACLE_VERBOSE",
            "ORACLE_ALLOW_STDERR_ONLY",
            "ORACLE_ALLOW_PSQL_META",
            "ORACLE_DECISION_LOG",
            "MYSQL_SOCKET",
            "OCCAM_WORKDIR",
            "PGPASSWORD",
            "MYSQL_PWD",
        ):
            run_env.pop(key, None)
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

    def write_mock(self, directory, name, body):
        path = os.path.join(directory, name)
        with open(path, "w", encoding="utf-8") as handle:
            handle.write("#!/bin/sh\nset -e\n" + body)
        os.chmod(path, 0o755)
        return path

    def make_log(self, directory, content="Log started\n"):
        path = os.path.join(directory, "server.log")
        with open(path, "w", encoding="utf-8") as handle:
            handle.write(content)
        return path

    def path_env(self, directory):
        return {"PATH": f"{directory}:{os.environ.get('PATH', '')}"}

    def read_jsonl(self, path):
        with open(path, encoding="utf-8") as handle:
            return [json.loads(line) for line in handle if line.strip()]

    def test_empty_sql_rejected(self):
        code, _, _ = self.run_oracle("")
        self.assertEqual(code, 1)

    def test_whitespace_sql_rejected(self):
        code, _, _ = self.run_oracle("   \n\t  ")
        self.assertEqual(code, 1)

    def test_matching_signature_in_log(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
echo '2026-11-15 12:00:00 UTC [1234] TRAP: FailedAssertion("!(IsA(node, TargetEntry))", File: "subselect.c", Line: 2883)' >> {shlex.quote(log_path)}
echo 'server closed the connection unexpectedly' >&2
exit 2
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "FailedAssertion.*subselect.c",
                    "--verbose",
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)

    def test_mismatched_signature_in_log_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
echo '2026-11-15 12:00:00 UTC [1234] TRAP: FailedAssertion("!(bms_is_member(i, rels))", File: "initsplan.c", Line: 512)' >> {shlex.quote(log_path)}
echo 'server closed the connection unexpectedly' >&2
exit 2
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "subselect.c",
                    "--verbose",
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)

    def test_normal_query_completion_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(td, "psql", "cat >/dev/null || true\nexit 0\n")
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--log-file", log_path],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)

    def test_sql_syntax_error_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(
                td,
                "psql",
                """cat >/dev/null || true
echo 'ERROR: syntax error at or near "BAD"' >&2
exit 0
""",
            )
            code, _, _ = self.run_oracle(
                "BAD SQL;",
                args=["--log-file", log_path],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)

    def test_timeout_returns_124_when_explicitly_enabled(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(td, "psql", "sleep 5\nexit 0\n")
            code, _, _ = self.run_oracle(
                "SELECT pg_sleep(10);",
                args=["--log-file", log_path, "--timeout", "0.5"],
                env=self.path_env(td),
            )
            self.assertEqual(code, 124)

    def test_stderr_crash_text_is_not_accepted(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(
                td,
                "psql",
                """cat >/dev/null || true
echo 'Segmentation fault' >&2
exit 0
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1; -- Segmentation fault\n",
                args=["--log-file", log_path],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)

    def test_aborted_connection_is_not_a_crash(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
echo 'Aborted connection 42 to db: occam_test user: root (Got an error reading communication packets)' >> {shlex.quote(log_path)}
exit 0
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--log-file", log_path],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)

    def test_signature_only_in_stderr_is_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(
                td,
                "psql",
                """cat >/dev/null || true
echo 'TRAP: FailedAssertion("!(x)", File: "subselect.c", Line: 1)' >&2
echo 'server closed the connection unexpectedly' >&2
exit 2
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "subselect.c",
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)

    def test_log_rotation_is_detected(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td, "old unrelated crash\n" * 10)
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
: > {shlex.quote(log_path)}
echo 'TRAP: FailedAssertion("!(x)", File: "mcv.c", Line: 1553)' >> {shlex.quote(log_path)}
echo 'server closed the connection unexpectedly' >&2
exit 2
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "mcv.c",
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)

    def test_log_flush_is_retried(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
sleep 0.3
echo 'TRAP: FailedAssertion("!(x)", File: "mcv.c", Line: 1553)' >> {shlex.quote(log_path)}
echo 'server closed the connection unexpectedly' >&2
exit 2
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "mcv.c",
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)

    def test_occam_workdir_is_used_as_cwd(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            cwd_file = os.path.join(td, "cwd.txt")
            workdir = os.path.join(td, "work")
            os.mkdir(workdir)
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
pwd > {shlex.quote(cwd_file)}
exit 0
""",
            )
            env = self.path_env(td)
            env["OCCAM_WORKDIR"] = workdir
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--log-file", log_path],
                env=env,
            )
            self.assertEqual(code, 1)
            with open(cwd_file, encoding="utf-8") as handle:
                self.assertEqual(handle.read().strip(), workdir)

    def test_mysql_client_hardening_flags(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            args_file = os.path.join(td, "args.txt")
            password_file = os.path.join(td, "password.txt")
            self.write_mock(
                td,
                "mysql",
                f"""cat >/dev/null || true
printf '%s\\n' "$@" > {shlex.quote(args_file)}
printf '%s' "$MYSQL_PWD" > {shlex.quote(password_file)}
exit 0
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "mysql",
                    "--log-file",
                    log_path,
                    "--password",
                    "secret",
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)
            with open(args_file, encoding="utf-8") as handle:
                argv = handle.read().splitlines()
            self.assertIn("--no-defaults", argv)
            self.assertIn("--binary-mode", argv)
            self.assertIn("--force", argv)
            self.assertNotIn("-psecret", argv)
            with open(password_file, encoding="utf-8") as handle:
                self.assertEqual(handle.read(), "secret")

    def test_psql_meta_command_is_rejected_by_default(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            self.write_mock(td, "psql", "cat >/dev/null || true\nexit 0\n")
            code, _, _ = self.run_oracle(
                "\\! echo unsafe\n",
                args=["--log-file", log_path],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)

    def test_decision_log_records_verdict(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
echo 'TRAP: FailedAssertion("!(x)", File: "mcv.c", Line: 1553)' >> {shlex.quote(log_path)}
echo 'server closed the connection unexpectedly' >&2
exit 2
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "mcv.c",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            records = self.read_jsonl(decision_log)
            self.assertEqual(len(records), 1)
            self.assertEqual(records[0]["verdict"], "accepted")
            self.assertEqual(records[0]["reason"], "crash-signature")

    def test_invalid_regex_falls_back_to_literal(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
echo 'Assertion failure: [x] failed' >> {shlex.quote(log_path)}
echo 'server closed the connection unexpectedly' >&2
exit 2
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "[",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            records = self.read_jsonl(decision_log)
            self.assertFalse(records[0]["regex_valid"])

    def test_container_log_mode(self):
        with tempfile.TemporaryDirectory() as td:
            state_file = os.path.join(td, "stat-called")
            self.write_mock(
                td,
                "docker",
                f"""cat >/dev/null || true
if [ "$1" = "exec" ] && [ "$3" = "stat" ]; then
    if [ -f {shlex.quote(state_file)} ]; then
        echo "61 1"
    else
        : > {shlex.quote(state_file)}
        echo "0 1"
    fi
    exit 0
fi
if [ "$1" = "exec" ] && [ "$3" = "tail" ]; then
    echo 'TRAP: FailedAssertion("!(x)", File: "mcv.c", Line: 1553)'
    exit 0
fi
if [ "$1" = "logs" ]; then
    exit 0
fi
exit 1
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--container-name",
                    "occam-target-pg",
                    "--expected-signature",
                    "mcv.c",
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)


if __name__ == "__main__":
    unittest.main()

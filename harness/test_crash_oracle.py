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
                "mysql",
                f"""cat >/dev/null || true
echo 'Assertion failure: [x] failed' >> {shlex.quote(log_path)}
echo 'Lost connection to MySQL server during query' >&2
exit 1
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "mysql",
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

    def test_pg13_postmaster_child_exit_and_csv_sqlstate_57p02(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            csv_row = (
                '2026-11-22 12:00:00.123 UTC,"postgres","occam_test",1234,'
                '"127.0.0.1:50000",673f,1,"SELECT",2026-11-22 12:00:00 UTC,3/1,0,'
                'FATAL,57P02,"terminating connection because of crash of another server process",,,,,,,,,"psql"'
            )
            postmaster_line = (
                "2026-11-22 12:00:00.124 UTC [1] LOG:  "
                "server process (PID 1230) was terminated by signal 11: Segmentation fault"
            )
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(csv_row)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(postmaster_line)} >> {shlex.quote(log_path)}
echo 'server closed the connection unexpectedly' >&2
exit 2
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "postgres",
                    "--log-file",
                    log_path,
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            records = self.read_jsonl(decision_log)
            self.assertEqual(records[0]["classification"], "confirmed_fault")
            self.assertTrue(records[0]["corroboration"]["sqlstate_57p02"])
            self.assertEqual(records[0]["corroboration"]["postmaster_child_pids"], [1230])

    def test_pg13_sql_echo_and_normal_exit_are_healthy_no_fault(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            echo_line = (
                "2026-11-22 12:00:00.100 UTC [1234] postgres@occam_test "
                "LOG:  statement: SELECT 'TRAP: FailedAssertion(\"!(x)\", File: \"mcv.c\", Line: 1)';"
            )
            normal_exit = (
                "2026-11-22 12:00:00.101 UTC [1] LOG:  "
                "server process (PID 1234) exited with exit code 0"
            )
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(echo_line)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(normal_exit)} >> {shlex.quote(log_path)}
exit 0
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "postgres",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "mcv.c",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)
            records = self.read_jsonl(decision_log)
            self.assertEqual(records[0]["verdict"], "rejected")
            self.assertEqual(records[0]["classification"], "healthy_no_fault")

    def test_mysql80_error_event_and_supervisor_exit(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            mysql_err = (
                "2026-11-22T12:00:00.123456Z 0 [ERROR] [MY-013183] [InnoDB] "
                "Assertion failure: btr0cur.cc:1234:cursor->index"
            )
            sup_line = "[entrypoint_target_mysql] mysqld-debug terminated (exit status: 134). Restarting..."
            self.write_mock(
                td,
                "mysql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(mysql_err)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(sup_line)} >> {shlex.quote(log_path)}
echo 'ERROR 2013 (HY000): Lost connection to MySQL server during query' >&2
exit 1
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "mysql",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "MY-013183",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            records = self.read_jsonl(decision_log)
            self.assertEqual(records[0]["classification"], "confirmed_fault")
            self.assertEqual(records[0]["corroboration"]["supervisor_exit_codes"], [134])

    def test_mysql80_transport_disconnect_without_server_fault_is_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            note_line = (
                "2026-11-22T12:00:00.123456Z 9 [Note] [MY-010914] [Server] "
                "Aborted connection 9 to db: 'occam_test' user: 'root' (Got timeout reading communication packets)"
            )
            normal_exit = "[entrypoint_target_mysql] mysqld-debug terminated (exit status: 0)."
            self.write_mock(
                td,
                "mysql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(note_line)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(normal_exit)} >> {shlex.quote(log_path)}
echo 'ERROR 2006 (HY000): MySQL server has gone away' >&2
exit 1
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "mysql",
                    "--log-file",
                    log_path,
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)
            records = self.read_jsonl(decision_log)
            self.assertEqual(records[0]["classification"], "healthy_no_fault")
            self.assertTrue(records[0]["corroboration"]["connection_lost"])

    def test_missing_evidence_classification_when_log_missing(self):
        with tempfile.TemporaryDirectory() as td:
            decision_log = os.path.join(td, "decisions.jsonl")
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--decision-log", decision_log],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)
            records = self.read_jsonl(decision_log)
            self.assertEqual(records[0]["classification"], "missing_evidence")
            self.assertEqual(records[0]["reason"], "log-source-required")

    def test_pg13_multiline_statement_echo_and_csv_are_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            stderr_multiline = (
                "2026-11-22 12:00:00.100 UTC [1234] postgres@occam_test "
                "LOG:  statement: SELECT 1,\n"
                "\t'TRAP: FailedAssertion(\"!(x)\", File: \"mcv.c\", Line: 1)';\n"
                "\tPANIC: fake continuation line"
            )
            csv_multiline = (
                '2026-11-22 12:00:00.101 UTC,"postgres","occam_test",1234,'
                '"127.0.0.1:50000",673f,1,"SELECT",2026-11-22 12:00:00 UTC,3/1,0,'
                'LOG,00000,"statement: SELECT 1,\n'
                'TRAP: FailedAssertion(""!(x)"", File: ""mcv.c"", Line: 1)",,,,,,,,,"psql"'
            )
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(stderr_multiline)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(csv_multiline)} >> {shlex.quote(log_path)}
exit 0
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "postgres",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "mcv.c",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)
            records = self.read_jsonl(decision_log)
            self.assertEqual(records[0]["classification"], "healthy_no_fault")
            self.assertEqual(records[0]["fault_events"], [])

    def test_expected_signature_without_fault_is_healthy_no_fault(self):
        with tempfile.TemporaryDirectory() as td:
            pg_log = self.make_log(td)
            pg_decisions = os.path.join(td, "pg_decisions.jsonl")
            pg_fatal = (
                '2026-11-22 12:00:00.100 UTC [1234] postgres@occam_test '
                'FATAL:  database "mcv.c" does not exist'
            )
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(pg_fatal)} >> {shlex.quote(pg_log)}
exit 2
""",
            )
            pg_code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "postgres",
                    "--log-file",
                    pg_log,
                    "--expected-signature",
                    "mcv.c",
                    "--decision-log",
                    pg_decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(pg_code, 1)
            pg_rec = self.read_jsonl(pg_decisions)[0]
            self.assertEqual(pg_rec["classification"], "healthy_no_fault")
            self.assertEqual(pg_rec["reason"], "no-crash")

            mysql_log = os.path.join(td, "mysql.log")
            with open(mysql_log, "w", encoding="utf-8") as handle:
                handle.write("Log started\n")
            mysql_decisions = os.path.join(td, "mysql_decisions.jsonl")
            mysql_err = (
                "2026-11-22T12:00:00.123456Z 9 [ERROR] [MY-010123] [Server] "
                "Can't find file: 'btr0cur.cc'"
            )
            self.write_mock(
                td,
                "mysql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(mysql_err)} >> {shlex.quote(mysql_log)}
exit 1
""",
            )
            mysql_code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "mysql",
                    "--log-file",
                    mysql_log,
                    "--expected-signature",
                    "btr0cur.cc",
                    "--decision-log",
                    mysql_decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(mysql_code, 1)
            mysql_rec = self.read_jsonl(mysql_decisions)[0]
            self.assertEqual(mysql_rec["classification"], "healthy_no_fault")
            self.assertEqual(mysql_rec["reason"], "no-crash")

    def test_pg13_asan_stack_overflow_is_confirmed_fault(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            asan_line = "==1234==ERROR: AddressSanitizer: stack-buffer-overflow on address 0x7fff"
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(asan_line)} >> {shlex.quote(log_path)}
echo 'server closed the connection unexpectedly' >&2
exit 2
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "postgres",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "stack-buffer-overflow",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            rec = self.read_jsonl(decision_log)[0]
            self.assertEqual(rec["classification"], "confirmed_fault")


if __name__ == "__main__":
    unittest.main()

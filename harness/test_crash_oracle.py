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
            "BASELINE_SIGNATURE",
            "ORACLE_REJECT_ON_SLIPPAGE",
            "ORACLE_STACK_FRAMES",
            "ORACLE_TIMEOUT",
            "ORACLE_VERBOSE",
            "ORACLE_ALLOW_STDERR_ONLY",
            "ORACLE_ALLOW_PSQL_META",
            "ORACLE_DECISION_LOG",
            "MYSQL_SOCKET",
            "MARIADB_HOST",
            "MARIADB_TCP_PORT",
            "MARIADB_USER",
            "MARIADB_PWD",
            "MARIADB_DATABASE",
            "MARIADB_UNIX_PORT",
            "CLICKHOUSE_HOST",
            "CLICKHOUSE_PORT",
            "CLICKHOUSE_USER",
            "CLICKHOUSE_PASSWORD",
            "CLICKHOUSE_DATABASE",
            "CLICKHOUSE_DB",
            "SQLITE_BIN",
            "DUCKDB_BIN",
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

    def test_mariadb_innodb_multiline_assertion_and_supervisor_exit(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            line1 = (
                "2026-11-22 12:00:00 0 [ERROR] [FATAL] InnoDB: Assertion failure "
                "in file /tmp/mariadb/storage/innobase/btr/btr0cur.cc line 1234"
            )
            line2 = "InnoDB: Failing assertion: cursor->index"
            sup = "[entrypoint_target_mariadb] mysqld terminated (exit status: 134). Restarting..."
            self.write_mock(
                td,
                "mariadb",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(line1)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(line2)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(sup)} >> {shlex.quote(log_path)}
echo 'ERROR 2013 (HY000): Lost connection to server during query' >&2
exit 1
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "mariadb",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "cursor->index",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            rec = self.read_jsonl(decision_log)[0]
            self.assertEqual(rec["classification"], "confirmed_fault")
            self.assertTrue(rec["corroboration"]["connection_lost"])
            self.assertEqual(rec["corroboration"]["supervisor_exit_codes"], [134])
            sig = rec["crash_signature"]
            self.assertEqual(sig["category"], "assertion")
            self.assertEqual(sig["file"], "btr0cur.cc")
            self.assertEqual(sig["line"], 1234)
            self.assertEqual(sig["assertion"], "cursor->index")
            self.assertEqual(sig["signal"], "SIGABRT")
            self.assertEqual(sig["signal_num"], 6)

    def test_mariadb_note_warning_and_query_dump_echo_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            note_line = (
                "2026-11-22 12:00:00 9 [Note] Aborted connection 9 to db: 'occam_test'"
            )
            warn_line = "2026-11-22 12:00:00 9 [Warning] InnoDB: Assertion failure: spoof"
            normal_exit = "[entrypoint_target_mariadb] mysqld terminated (exit status: 0)."
            self.write_mock(
                td,
                "mysql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(note_line)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(warn_line)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(normal_exit)} >> {shlex.quote(log_path)}
echo 'Server has gone away' >&2
exit 1
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "mariadb",
                    "--log-file",
                    log_path,
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)
            rec = self.read_jsonl(decision_log)[0]
            self.assertEqual(rec["classification"], "healthy_no_fault")
            self.assertTrue(rec["corroboration"]["connection_lost"])

            # Crash dump Query (...) SQL echo must not satisfy --expected-signature.
            sig_line = "261122 12:00:01 [ERROR] mysqld got signal 11 ;"
            dump_line = "Query (7f123456): SELECT 'fake_assertion_spoof'"
            self.write_mock(
                td,
                "mysql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(sig_line)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(dump_line)} >> {shlex.quote(log_path)}
exit 1
""",
            )
            code2, _, _ = self.run_oracle(
                "SELECT 'fake_assertion_spoof';",
                args=[
                    "--dbms",
                    "mariadb",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "fake_assertion_spoof",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code2, 1)
            rec2 = self.read_jsonl(decision_log)[1]
            self.assertEqual(rec2["reason"], "signature-mismatch")

    def test_mariadb_client_defaults_and_flags(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            args_file = os.path.join(td, "args.txt")
            pwd_file = os.path.join(td, "pwd.txt")
            self.write_mock(
                td,
                "mariadb",
                f"""cat >/dev/null || true
printf '%s\\n' "$@" > {shlex.quote(args_file)}
printf '%s' "$MYSQL_PWD" > {shlex.quote(pwd_file)}
exit 0
""",
            )
            env = self.path_env(td)
            env.update(
                {
                    "MARIADB_HOST": "maria-host",
                    "MARIADB_TCP_PORT": "3307",
                    "MARIADB_USER": "muser",
                    "MARIADB_PWD": "mpass",
                    "MARIADB_DATABASE": "mdb",
                }
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "mariadb", "--log-file", log_path],
                env=env,
            )
            self.assertEqual(code, 1)
            with open(args_file, encoding="utf-8") as handle:
                argv = handle.read().splitlines()
            self.assertIn("--no-defaults", argv)
            self.assertIn("--binary-mode", argv)
            self.assertIn("--force", argv)
            self.assertIn("maria-host", argv)
            self.assertIn("3307", argv)
            self.assertIn("muser", argv)
            self.assertIn("mdb", argv)
            with open(pwd_file, encoding="utf-8") as handle:
                self.assertEqual(handle.read(), "mpass")

    def test_clickhouse_fatal_signal_stack_and_query_echo_stripping(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            l1 = (
                "2026-11-22 12:00:00.123456 [ 25819 ] {} <Fatal> BaseDaemon: "
                "(version 26.8.44.3) (from thread 26408) Received signal 11"
            )
            l2 = (
                "2026-11-22 12:00:00.123490 [ 25819 ] {} <Fatal> BaseDaemon: "
                "(version 26.8.44.3) (from thread 26408) (query_id: 404ee645-9173-4286-9afb-93d4dec49793) "
                "(query: SELECT 'spoof_token_xyz') Received signal Segmentation fault (11)"
            )
            f0 = (
                "2026-11-22 12:00:00.123500 [ 25819 ] {} <Fatal> BaseDaemon: "
                "0. ./build_docker/./src/Parsers/ParserQuery.cpp:42: DB::ParserQuery::parseImpl() @ 0x000000001234abcd"
            )
            f1 = (
                "2026-11-22 12:00:00.123510 [ 25819 ] {} <Fatal> BaseDaemon: "
                "1. ./build_docker/./src/Interpreters/executeQuery.cpp:99: DB::executeQuery() @ 0x00000000deadbeef"
            )
            sup = "[entrypoint_target_clickhouse] clickhouse-server terminated (exit status: 139). Restarting server for next test..."
            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(l1)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(l2)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(f0)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(f1)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(sup)} >> {shlex.quote(log_path)}
echo 'Code: 210. DB::NetException: Connection reset by peer' >&2
exit 210
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 'spoof_token_xyz';",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "ParserQuery",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            rec = self.read_jsonl(decision_log)[0]
            self.assertEqual(rec["classification"], "confirmed_fault")
            self.assertTrue(rec["corroboration"]["connection_lost"])
            self.assertEqual(rec["corroboration"]["supervisor_exit_codes"], [139])
            sig = rec["crash_signature"]
            self.assertEqual(sig["category"], "signal")
            self.assertEqual(sig["signal"], "SIGSEGV")
            self.assertEqual(sig["signal_num"], 11)
            self.assertEqual(
                sig["frames"],
                [
                    "DB::ParserQuery::parseImpl()@src/Parsers/ParserQuery.cpp:42",
                    "DB::executeQuery()@src/Interpreters/executeQuery.cpp:99",
                ],
            )

            # Spoofed string inside (query: ...) must NOT match --expected-signature
            code_spoof, _, _ = self.run_oracle(
                "SELECT 'spoof_token_xyz';",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "spoof_token_xyz",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code_spoof, 1)
            rec_spoof = self.read_jsonl(decision_log)[1]
            self.assertEqual(rec_spoof["reason"], "signature-mismatch")

    def test_clickhouse_logical_error_vs_normal_query_error_and_client_flags(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decision_log = os.path.join(td, "decisions.jsonl")
            args_file = os.path.join(td, "ch_args.txt")
            normal_err = (
                "2026-11-22 12:00:00.100000 [ 42 ] {qid} <Error> executeQuery: "
                "Code: 60. DB::Exception: Table occam_test.nope doesn't exist"
            )
            normal_sup = "[entrypoint_target_clickhouse] clickhouse-server terminated (exit status: 0)."
            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' "$@" > {shlex.quote(args_file)}
printf '%s\\n' {shlex.quote(normal_err)} >> {shlex.quote(log_path)}
printf '%s\\n' {shlex.quote(normal_sup)} >> {shlex.quote(log_path)}
exit 0
""",
            )
            code_ok, _, _ = self.run_oracle(
                "SELECT * FROM nope;",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log_path,
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code_ok, 1)
            self.assertEqual(self.read_jsonl(decision_log)[0]["classification"], "healthy_no_fault")
            with open(args_file, encoding="utf-8") as handle:
                argv = handle.read().splitlines()
            self.assertIn("--multiquery", argv)
            self.assertIn("--ignore-error", argv)
            self.assertIn("9000", argv)

            logical_err = (
                "2026-11-22 12:00:01.100000 [ 42 ] {} <Fatal> : "
                "Logical error: 'Deleting data part all_0_0_0 doesn't exist'"
            )
            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(logical_err)} >> {shlex.quote(log_path)}
exit 1
""",
            )
            code_fatal, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log_path,
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code_fatal, 0)
            rec_fatal = self.read_jsonl(decision_log)[1]
            self.assertEqual(rec_fatal["classification"], "confirmed_fault")
            self.assertEqual(rec_fatal["crash_signature"]["category"], "logical_error")

    def test_sqlite_local_assertion_and_sigabrt_exit_134(self):
        with tempfile.TemporaryDirectory() as td:
            args_file = os.path.join(td, "args.txt")
            decision_log = os.path.join(td, "decisions.jsonl")
            self.write_mock(
                td,
                "sqlite3",
                f"""cat >/dev/null || true
printf '%s\\n' "$@" > {shlex.quote(args_file)}
echo "sqlite3: /tmp/sqlite/sqlite3.c:86402: sqlite3VdbeExec: Assertion \\`pIn1!=pIn3' failed." >&2
exit 134
""",
            )
            code, _, _ = self.run_oracle(
                "UPDATE t0 SET c0 = 0;",
                args=[
                    "--dbms",
                    "sqlite",
                    "--expected-signature",
                    "sqlite3VdbeExec",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            with open(args_file, encoding="utf-8") as handle:
                self.assertEqual(handle.read().splitlines(), ["-batch", ":memory:"])
            rec = self.read_jsonl(decision_log)[0]
            self.assertEqual(rec["classification"], "confirmed_fault")
            sig = rec["crash_signature"]
            self.assertEqual(
                sig["id"],
                "assertion:sqlite3.c:86402:sqlite3VdbeExec:pIn1!=pIn3",
            )
            self.assertEqual(sig["signal"], "SIGABRT")
            self.assertEqual(sig["signal_num"], 6)

    def test_sqlite_container_occam_exec_and_infra_failure(self):
        with tempfile.TemporaryDirectory() as td:
            args_file = os.path.join(td, "docker_args.txt")
            decision_log = os.path.join(td, "decisions.jsonl")
            self.write_mock(
                td,
                "docker",
                f"""cat >/dev/null || true
printf '%s\\n' "$@" > {shlex.quote(args_file)}
exit 139
""",
            )
            code, _, _ = self.run_oracle(
                "REPLACE INTO t0 VALUES(1);",
                args=[
                    "--dbms",
                    "sqlite",
                    "--container-name",
                    "occam-target-sqlite",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            with open(args_file, encoding="utf-8") as handle:
                self.assertEqual(
                    handle.read().splitlines(),
                    ["exec", "-i", "occam-target-sqlite", "occam-exec"],
                )
            rec = self.read_jsonl(decision_log)[0]
            self.assertEqual(rec["crash_signature"]["signal"], "SIGSEGV")
            self.assertEqual(rec["crash_signature"]["signal_num"], 11)

            # docker exec exit 125/127 is an infrastructure failure (missing_evidence)
            for infra_rc in (125, 127):
                self.write_mock(
                    td,
                    "docker",
                    f"cat >/dev/null || true\necho 'container not running' >&2\nexit {infra_rc}\n",
                )
                code_err, _, _ = self.run_oracle(
                    "SELECT 1;",
                    args=[
                        "--dbms",
                        "sqlite",
                        "--container-name",
                        "occam-target-sqlite",
                        "--decision-log",
                        decision_log,
                    ],
                    env=self.path_env(td),
                )
                self.assertEqual(code_err, 1)
                rec_err = self.read_jsonl(decision_log)[-1]
                self.assertEqual(rec_err["classification"], "missing_evidence")
                self.assertEqual(rec_err["reason"], "container-exec-failed")

    def test_sqlite_abort_exit_4_and_parse_error_spoof_rejected_and_asan_accepted(self):
        with tempfile.TemporaryDirectory() as td:
            decision_log = os.path.join(td, "decisions.jsonl")
            # SQLITE_ABORT (4) is an interrupted operation, not a crash.
            self.write_mock(td, "sqlite3", "cat >/dev/null || true\nexit 4\n")
            code_abort, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "sqlite", "--decision-log", decision_log],
                env=self.path_env(td),
            )
            self.assertEqual(code_abort, 1)
            rec_abort = self.read_jsonl(decision_log)[-1]
            self.assertEqual(rec_abort["classification"], "healthy_no_fault")
            self.assertEqual(rec_abort["reason"], "sqlite-abort")
            self.assertTrue(rec_abort["corroboration"]["sqlite_abort"])

            # Multiline Parse error spoofing AddressSanitizer without signal/frames/SUMMARY
            self.write_mock(
                td,
                "sqlite3",
                """cat >/dev/null || true
printf 'Parse error near line 1: no such table: a\\n==1234==ERROR: AddressSanitizer: fake\\n' >&2
exit 1
""",
            )
            code_spoof, _, _ = self.run_oracle(
                'SELECT 1 FROM "a\n==1234==ERROR: AddressSanitizer: fake";',
                args=["--dbms", "sqlite", "--decision-log", decision_log],
                env=self.path_env(td),
            )
            self.assertEqual(code_spoof, 1)
            self.assertEqual(
                self.read_jsonl(decision_log)[-1]["classification"], "healthy_no_fault"
            )

            # Real ASan report with abort_on_error=0 (exit 1 + frames + SUMMARY)
            self.write_mock(
                td,
                "sqlite3",
                """cat >/dev/null || true
printf 'Parse error near line 1: no such table: x\\n' >&2
printf '==999==ERROR: AddressSanitizer: heap-buffer-overflow on address 0x60200001a2f4\\n' >&2
printf '    #0 0x555555 in __asan_report_load4 /asan.c:1\\n' >&2
printf '    #1 0x555556 in sqlite3VdbeExec /tmp/sqlite/sqlite3.c:86402:13\\n' >&2
printf 'SUMMARY: AddressSanitizer: heap-buffer-overflow /tmp/sqlite/sqlite3.c:86402:13 in sqlite3VdbeExec\\n' >&2
exit 1
""",
            )
            code_asan, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "sqlite", "--decision-log", decision_log],
                env=self.path_env(td),
            )
            self.assertEqual(code_asan, 0)
            rec_asan = self.read_jsonl(decision_log)[-1]
            self.assertEqual(rec_asan["classification"], "confirmed_fault")
            self.assertEqual(rec_asan["crash_signature"]["category"], "asan")
            self.assertEqual(
                rec_asan["crash_signature"]["id"],
                "asan:heap-buffer-overflow:sqlite3VdbeExec@sqlite3.c:86402",
            )

    def test_duckdb_distinguishes_signal_internal_error_and_oom(self):
        with tempfile.TemporaryDirectory() as td:
            decision_log = os.path.join(td, "decisions.jsonl")

            # 1. Process signal termination
            self.write_mock(td, "duckdb", "cat >/dev/null || true\nexit 139\n")
            c1, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decision_log],
                env=self.path_env(td),
            )
            self.assertEqual(c1, 0)
            r1 = self.read_jsonl(decision_log)[-1]
            self.assertEqual(r1["fault_events"][0]["kind"], "duckdb_process_signal")
            self.assertEqual(r1["crash_signature"]["category"], "signal")
            self.assertEqual(r1["crash_signature"]["signal"], "SIGSEGV")

            # 2. INTERNAL Error + database invalidated (not signal termination)
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'INTERNAL Error:\\nAttempted to access index 3 within vector of size 3\\n' >&2
printf 'FATAL Error:\\nFailed: database has been invalidated because of a previous fatal error.\\n' >&2
exit 1
""",
            )
            c2, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decision_log],
                env=self.path_env(td),
            )
            self.assertEqual(c2, 0)
            r2 = self.read_jsonl(decision_log)[-1]
            self.assertEqual(r2["fault_events"][0]["kind"], "duckdb_internal_error")
            self.assertTrue(r2["corroboration"]["database_invalidated"])
            self.assertEqual(r2["crash_signature"]["category"], "internal_error")
            self.assertEqual(
                r2["crash_signature"]["id"],
                "internal_error:Attempted to access index 3 within vector of size 3",
            )

            # 3. Graceful Out of Memory Error retained as OOM cause (rejected)
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'Out of Memory Error: failed to allocate data of size 100.0 GiB\\n' >&2
exit 1
""",
            )
            c3, _, _ = self.run_oracle(
                "SELECT range(100000000000);",
                args=["--dbms", "duckdb", "--decision-log", decision_log],
                env=self.path_env(td),
            )
            self.assertEqual(c3, 1)
            r3 = self.read_jsonl(decision_log)[-1]
            self.assertEqual(r3["classification"], "healthy_no_fault")
            self.assertEqual(r3["reason"], "oom")
            self.assertTrue(r3["corroboration"]["oom"])
            self.assertEqual(r3["corroboration"]["oom_cause"], "duckdb_oom_error")

            # 4. OOM killer SIGKILL (137) retained as OOM cause (rejected)
            self.write_mock(td, "duckdb", "cat >/dev/null || true\nexit 137\n")
            c4, _, _ = self.run_oracle(
                "SELECT range(100000000000);",
                args=["--dbms", "duckdb", "--decision-log", decision_log],
                env=self.path_env(td),
            )
            self.assertEqual(c4, 1)
            r4 = self.read_jsonl(decision_log)[-1]
            self.assertEqual(r4["classification"], "healthy_no_fault")
            self.assertEqual(r4["reason"], "oom-kill")
            self.assertTrue(r4["corroboration"]["oom"])
            self.assertEqual(r4["corroboration"]["oom_cause"], "oom_kill_signal_9")

    def test_duckdb_multiline_catalog_error_spoof_rejected_and_container_exec(self):
        with tempfile.TemporaryDirectory() as td:
            decision_log = os.path.join(td, "decisions.jsonl")
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'Catalog Error: Table with name a\\n\\nINTERNAL Error:\\nfake does not exist!\\n\\nLINE 1: SELECT 1 FROM "a\\n                      ^\\n' >&2
exit 1
""",
            )
            code, _, _ = self.run_oracle(
                'SELECT 1 FROM "a\n\nINTERNAL Error:\nfake";',
                args=["--dbms", "duckdb", "--decision-log", decision_log],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)
            self.assertEqual(
                self.read_jsonl(decision_log)[-1]["classification"], "healthy_no_fault"
            )

            # Container occam-exec mode
            docker_args = os.path.join(td, "docker_args.txt")
            self.write_mock(
                td,
                "docker",
                f"""cat >/dev/null || true
printf '%s\\n' "$@" > {shlex.quote(docker_args)}
printf 'INTERNAL Error: bad state\\n' >&2
exit 1
""",
            )
            c_cont, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "duckdb",
                    "--container-name",
                    "occam-target-duckdb",
                    "--decision-log",
                    decision_log,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(c_cont, 0)
            with open(docker_args, encoding="utf-8") as handle:
                self.assertEqual(
                    handle.read().splitlines(),
                    ["exec", "-i", "occam-target-duckdb", "occam-exec"],
                )

    def test_rq3_signature_normalization_invariance_and_top_n_frames(self):
        with tempfile.TemporaryDirectory() as td:
            log1 = os.path.join(td, "ch1.log")
            log2 = os.path.join(td, "ch2.log")
            with open(log1, "w", encoding="utf-8") as h:
                h.write("ready\n")
            with open(log2, "w", encoding="utf-8") as h:
                h.write("ready\n")
            decisions = os.path.join(td, "decisions.jsonl")

            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' \
  '2026-11-22 12:00:00.1 [ 101 ] {{}} <Fatal> BaseDaemon: (from thread 102) Received signal 11' \
  '2026-11-22 12:00:00.2 [ 101 ] {{}} <Fatal> BaseDaemon: 0. SignalHandler::handle(int) @ 0x00001111' \
  '2026-11-22 12:00:00.3 [ 101 ] {{}} <Fatal> BaseDaemon: 1. ./build_a/./src/Core/Block.cpp:10: DB::Block::check() @ 0x00001112' \
  '2026-11-22 12:00:00.4 [ 101 ] {{}} <Fatal> BaseDaemon: 2. ./build_a/./src/Planner/Plan.cpp:20: DB::Plan::run() @ 0x00001113' \
  '2026-11-22 12:00:00.5 [ 101 ] {{}} <Fatal> BaseDaemon: 3. ./build_a/./src/Server/Main.cpp:30: DB::mainA() @ 0x00001114' \
  >> {shlex.quote(log1)}
exit 1
""",
            )
            c1, out1, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log1,
                    "--stack-frames",
                    "2",
                    "--print-signature",
                    "--decision-log",
                    decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(c1, 0)
            sig1 = json.loads(out1.strip())
            self.assertEqual(len(sig1["frames"]), 2)

            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' \
  '2026-11-23 01:02:03.9 [ 8888 ] {{}} <Fatal> BaseDaemon: (from thread 9999) Received signal 11' \
  '2026-11-23 01:02:04.0 [ 8888 ] {{}} <Fatal> BaseDaemon: 0. SignalHandler::handle(int) @ 0x0000aaaa' \
  '2026-11-23 01:02:04.1 [ 8888 ] {{}} <Fatal> BaseDaemon: 1. /workspace/repo/src/Core/Block.cpp:10: DB::Block::check() @ 0x0000bbbb' \
  '2026-11-23 01:02:04.2 [ 8888 ] {{}} <Fatal> BaseDaemon: 2. /workspace/repo/src/Planner/Plan.cpp:20: DB::Plan::run() @ 0x0000cccc' \
  '2026-11-23 01:02:04.3 [ 8888 ] {{}} <Fatal> BaseDaemon: 3. /workspace/repo/src/Server/Other.cpp:99: DB::mainB() @ 0x0000dddd' \
  >> {shlex.quote(log2)}
exit 1
""",
            )
            c2, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log2,
                    "--stack-frames",
                    "2",
                    "--baseline-signature",
                    sig1["hash"],
                    "--reject-on-slippage",
                    "--decision-log",
                    decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(c2, 0)
            recs = self.read_jsonl(decisions)
            self.assertEqual(recs[0]["crash_signature"]["id"], recs[1]["crash_signature"]["id"])
            self.assertEqual(recs[0]["crash_signature"]["hash"], recs[1]["crash_signature"]["hash"])
            self.assertFalse(recs[1]["slippage"]["slipped"])

    def test_rq3_slippage_tracking_detects_and_optionally_rejects(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decisions = os.path.join(td, "decisions.jsonl")
            baseline_file = os.path.join(td, "baseline.json")

            # Seed crash in mcv.c:1553
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
echo '2026-11-22 12:00:00 UTC [100] TRAP: FailedAssertion("!(clauses != NIL)", File: "mcv.c", Line: 1553, PID: 100)' >> {shlex.quote(log_path)}
exit 2
""",
            )
            c_seed, out_seed, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--print-signature",
                    "--decision-log",
                    decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(c_seed, 0)
            with open(baseline_file, "w", encoding="utf-8") as h:
                h.write(out_seed)

            # Slipped crash in subselect.c:2883
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
echo '2026-11-22 12:00:01 UTC [200] TRAP: FailedAssertion("!(IsA(node, TargetEntry))", File: "subselect.c", Line: 2883, PID: 200)' >> {shlex.quote(log_path)}
exit 2
""",
            )
            # Measurement-only mode: accepted, slippage recorded as True
            c_measure, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--baseline-signature",
                    baseline_file,
                    "--decision-log",
                    decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(c_measure, 0)
            r_measure = self.read_jsonl(decisions)[-1]
            self.assertTrue(r_measure["slippage"]["slipped"])
            self.assertFalse(r_measure["slippage"]["category_changed"])

            # Strict mode (--reject-on-slippage): rejected with reason signature-slippage
            c_reject, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--baseline-signature",
                    baseline_file,
                    "--reject-on-slippage",
                    "--decision-log",
                    decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(c_reject, 1)
            r_reject = self.read_jsonl(decisions)[-1]
            self.assertEqual(r_reject["verdict"], "rejected")
            self.assertEqual(r_reject["classification"], "confirmed_fault")
            self.assertEqual(r_reject["reason"], "signature-slippage")
            self.assertTrue(r_reject["slippage"]["slipped"])

    def test_rq3_asan_abrt_after_assertion_preserves_assertion_identity(self):
        with tempfile.TemporaryDirectory() as td:
            decisions = os.path.join(td, "decisions.jsonl")
            self.write_mock(
                td,
                "sqlite3",
                """cat >/dev/null || true
printf "sqlite3: /src/sqlite3.c:86402: sqlite3VdbeExec: Assertion \\`pIn1!=pIn3' failed.\\n" >&2
printf '==123==ERROR: AddressSanitizer: ABRT on unknown address 0x000000000000\\n' >&2
printf '    #0 0x7f1111 in __GI_raise /libc.c:1\\n' >&2
printf '    #1 0x7f1112 in __GI_abort /libc.c:2\\n' >&2
printf '    #2 0x551113 in sqlite3VdbeExec /src/sqlite3.c:86402:5\\n' >&2
exit 134
""",
            )
            code, _, _ = self.run_oracle(
                "UPDATE t0 SET c0 = 0;",
                args=["--dbms", "sqlite", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            sig = self.read_jsonl(decisions)[0]["crash_signature"]
            self.assertEqual(sig["category"], "assertion")
            self.assertEqual(sig["id"], "assertion:sqlite3.c:86402:sqlite3VdbeExec:pIn1!=pIn3")
            self.assertEqual(sig["frames"], ["sqlite3VdbeExec@sqlite3.c:86402"])

    def test_sqlite_multiline_error_spoofs_rejected_including_error_near_line_and_summary(self):
        with tempfile.TemporaryDirectory() as td:
            decisions = os.path.join(td, "decisions.jsonl")
            spoof_outputs = [
                'printf \'Parse error near line 1: no such table: foo\\nsqlite3: a.c:1: f: Assertion `x\\x27 failed.\\n\' >&2\nexit 1\n',
                'printf \'Parse error near line 1: no such table: foo\\nSUMMARY: AddressSanitizer: heap-buffer-overflow /a.c:1 in f\\n\' >&2\nexit 1\n',
                'printf \'Error near line 1: CHECK constraint failed: a\\nsqlite3: f.c:1: fn: Assertion `x\\x27 failed.\\n\' >&2\nexit 1\n',
            ]
            for idx, body in enumerate(spoof_outputs):
                self.write_mock(td, "sqlite3", "cat >/dev/null || true\n" + body)
                code, _, _ = self.run_oracle(
                    "SELECT 1;",
                    args=["--dbms", "sqlite", "--decision-log", decisions],
                    env=self.path_env(td),
                )
                self.assertEqual(code, 1, f"spoof #{idx} unexpectedly accepted")
                rec = self.read_jsonl(decisions)[-1]
                self.assertEqual(rec["classification"], "healthy_no_fault")
                self.assertEqual(rec["reason"], "no-crash")

    def test_duckdb_nonline_errors_do_not_swallow_subsequent_oom_or_internal_error(self):
        with tempfile.TemporaryDirectory() as td:
            decisions = os.path.join(td, "decisions.jsonl")

            # 1. Constraint Error (no LINE N:) followed by Out of Memory Error
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'Constraint Error: CHECK constraint failed on table t with expression CHECK((a > 0))\\n' >&2
printf 'Out of Memory Error: Failed to change memory limit to 1000\\n\\nPossible solutions:\\n* Increasing memory\\n' >&2
exit 1
""",
            )
            c1, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c1, 1)
            r1 = self.read_jsonl(decisions)[-1]
            self.assertEqual(r1["classification"], "healthy_no_fault")
            self.assertEqual(r1["reason"], "oom")
            self.assertTrue(r1["corroboration"]["oom"])
            self.assertEqual(r1["corroboration"]["oom_cause"], "duckdb_oom_error")

            # 2. Constraint Error followed by real INTERNAL Error
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'Constraint Error: CHECK constraint failed on table t with expression CHECK((a > 0))\\n' >&2
printf 'INTERNAL Error:\\nAttempted to access index 3 within vector of size 3\\nThis error signals an assertion failure within DuckDB.\\n' >&2
exit 1
""",
            )
            c2, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c2, 0)
            r2 = self.read_jsonl(decisions)[-1]
            self.assertEqual(r2["classification"], "confirmed_fault")
            self.assertEqual(r2["crash_signature"]["category"], "internal_error")

            # 3. Unknown Command Error followed by INTERNAL Error
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'Unknown Command Error: Unrecognized command "badcmd"\\n\\nDid you mean: ".cd"\\nRun ".help" for more information.\\n' >&2
printf 'INTERNAL Error: bad state\\n' >&2
exit 1
""",
            )
            c3, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c3, 0)
            self.assertEqual(
                self.read_jsonl(decisions)[-1]["classification"], "confirmed_fault"
            )

            # 4. Spoof inside Constraint Error quoted column + PRAGMA Catalog Error rejected
            for body in (
                'printf \'Constraint Error: CHECK constraint failed on table t with expression CHECK(("col\\nINTERNAL Error: spoof" > 0))\\n\' >&2\nexit 1\n',
                'printf \'Catalog Error: Pragma Function with name a\\nINTERNAL Error: spoof does not exist!\\nDid you mean "collations"?\\n\' >&2\nexit 1\n',
            ):
                self.write_mock(td, "duckdb", "cat >/dev/null || true\n" + body)
                c_sp, _, _ = self.run_oracle(
                    "SELECT 1;",
                    args=["--dbms", "duckdb", "--decision-log", decisions],
                    env=self.path_env(td),
                )
                self.assertEqual(c_sp, 1)
                self.assertEqual(
                    self.read_jsonl(decisions)[-1]["classification"], "healthy_no_fault"
                )

    def test_duckdb_cpp_assertion_with_namespace_colons_parsed(self):
        with tempfile.TemporaryDirectory() as td:
            decisions = os.path.join(td, "decisions.jsonl")
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf "duckdb: /src/duckdb/src/planner/binder.cpp:42: duckdb::BoundStatement duckdb::Binder::Bind(): Assertion \\`x != nullptr' failed.\\n" >&2
exit 134
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(code, 0)
            rec = self.read_jsonl(decisions)[0]
            self.assertEqual(rec["fault_events"][0]["kind"], "duckdb_assertion_failure")
            sig = rec["crash_signature"]
            self.assertEqual(sig["category"], "assertion")
            self.assertEqual(sig["file"], "binder.cpp")
            self.assertEqual(sig["line"], 42)
            self.assertEqual(sig["function"], "duckdb::BoundStatement duckdb::Binder::Bind()")
            self.assertEqual(sig["assertion"], "x != nullptr")

    def test_clickhouse_multiline_error_echo_rejected_and_multiline_basedaemon_query_stripped(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decisions = os.path.join(td, "decisions.jsonl")

            # Multi-line <Error> executeQuery with SQL spoof and exception stack trace
            err_lines = "\n".join(
                [
                    "2026.11.22 12:00:00.100000 [ 42 ] {qid} <Error> executeQuery: Code: 60. DB::Exception: Table occam_test.foo",
                    "Assertion `x' failed.",
                    "SUMMARY: AddressSanitizer: fake /a.c:1 in f",
                    "doesn't exist. (UNKNOWN_TABLE) (version 26.8.44.3), Stack trace:",
                    "",
                    "0. DB::Exception::Exception() @ 0x00001234",
                ]
            )
            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(err_lines)} >> {shlex.quote(log_path)}
exit 0
""",
            )
            c_err, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "clickhouse", "--log-file", log_path, "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c_err, 1)
            self.assertEqual(self.read_jsonl(decisions)[-1]["classification"], "healthy_no_fault")

            # Multi-line (query: ...) in <Fatal> BaseDaemon + unprefixed stack frame
            fatal_lines = "\n".join(
                [
                    "2026-11-22 12:00:01.000000 [ 99 ] {} <Fatal> BaseDaemon: (version 26.8.44.3) (query_id: q1) (query: SELECT 'spoof_tok'",
                    "Assertion `fake' failed.",
                    ") Received signal Segmentation fault (11)",
                    "0. ./build_docker/./src/Core/Block.cpp:10: DB::Block::check() @ 0x0000aaaa",
                ]
            )
            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(fatal_lines)} >> {shlex.quote(log_path)}
exit 210
""",
            )
            c_spoof, _, _ = self.run_oracle(
                "SELECT 'spoof_tok';",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "spoof_tok",
                    "--decision-log",
                    decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(c_spoof, 1)
            r_sp = self.read_jsonl(decisions)[-1]
            self.assertEqual(r_sp["reason"], "signature-mismatch")
            sig = r_sp["crash_signature"]
            self.assertEqual(sig["category"], "signal")
            self.assertEqual(sig["signal"], "SIGSEGV")
            self.assertEqual(sig["frames"], ["DB::Block::check()@src/Core/Block.cpp:10"])

    def test_mysql_cpp_backtrace_buildid_and_multiline_warning_exclusion(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decisions = os.path.join(td, "decisions.jsonl")

            # Multi-line [Warning] with SQL spoof must not trigger crash
            warn_lines = "\n".join(
                [
                    "2026-11-22T12:00:00.123456Z 9 [Warning] [MY-010000] [Server] Unsafe statement:",
                    "Assertion failure: spoof.cc:1:fake",
                ]
            )
            self.write_mock(
                td,
                "mysql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(warn_lines)} >> {shlex.quote(log_path)}
exit 0
""",
            )
            c_warn, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "mysql", "--log-file", log_path, "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c_warn, 1)
            self.assertEqual(self.read_jsonl(decisions)[-1]["classification"], "healthy_no_fault")

            # Real MySQL C++ backtrace with space before [0x...], params (), libpthread wrapper, and BuildId
            bt_lines = "\n".join(
                [
                    "12:00:00 UTC - mysqld got signal 11 ;",
                    "/usr/sbin/mysqld-debug(my_print_stacktrace(unsigned char const*, unsigned long)+0x43) [0x55a1b2c3d4e5]",
                    "/usr/sbin/mysqld-debug(handle_fatal_signal+0x31a) [0x55a1b2345678]",
                    "/lib/x86_64-linux-gnu/libpthread.so.0(+0x12980) [0x7f8b71234980]",
                    "/usr/sbin/mysqld-debug(Item_func_rollup_const::val_str(String*)+0x52) [0x55a1b1234567]",
                    "sql/sql_select.cc:4512(JOIN::optimize_inner())",
                ]
            )
            self.write_mock(
                td,
                "mysql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(bt_lines)} >> {shlex.quote(log_path)}
exit 1
""",
            )
            c_bt, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "mysql", "--log-file", log_path, "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c_bt, 0)
            sig = self.read_jsonl(decisions)[-1]["crash_signature"]
            self.assertEqual(sig["category"], "signal")
            self.assertEqual(sig["signal"], "SIGSEGV")
            self.assertEqual(
                sig["frames"],
                [
                    "Item_func_rollup_const::val_str(String*)",
                    "JOIN::optimize_inner()@sql_select.cc:4512",
                ],
            )

            # Ubuntu 24.04 ASan BuildId + c++ header path normalization
            self.write_mock(
                td,
                "sqlite3",
                """cat >/dev/null || true
printf '==55==ERROR: AddressSanitizer: heap-use-after-free on address 0x6020\\n' >&2
printf '    #0 0x7f11 in __asan_report_load4 (/usr/lib/libasan.so.8+0x1234) (BuildId: deadbeef)\\n' >&2
printf '    #1 0x5511 in std::vector::at /usr/include/c++/13/bits/vector.tcc:123:5\\n' >&2
printf '    #2 0x5522 in sqlite3Step (/usr/local/bin/sqlite3+0x4444) (BuildId: cafebabe)\\n' >&2
exit 134
""",
            )
            c_asan, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "sqlite", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c_asan, 0)
            sig_asan = self.read_jsonl(decisions)[-1]["crash_signature"]
            self.assertEqual(
                sig_asan["frames"],
                ["std::vector::at@vector.tcc:123", "sqlite3Step@sqlite3+0x4444"],
            )

    def test_empty_env_vars_and_jsonl_baseline_signature(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decisions = os.path.join(td, "decisions.jsonl")
            pg_line = (
                '2026-11-22 12:00:00 UTC [100] TRAP: FailedAssertion('
                '"!(strcmp(a, "b") == 0)", File: "foo.c", Line: 10, PID: 100)'
            )
            self.write_mock(
                td,
                "psql",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(pg_line)} >> {shlex.quote(log_path)}
exit 2
""",
            )
            env = self.path_env(td)
            env.update(
                {
                    "PGPORT": "",
                    "ORACLE_STACK_FRAMES": "",
                    "ORACLE_TIMEOUT": "",
                }
            )
            c1, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--log-file", log_path, "--decision-log", decisions],
                env=env,
            )
            self.assertEqual(c1, 0)
            r1 = self.read_jsonl(decisions)[0]
            self.assertEqual(r1["crash_signature"]["file"], "foo.c")
            self.assertEqual(r1["crash_signature"]["line"], 10)
            self.assertEqual(r1["crash_signature"]["assertion"], '!(strcmp(a, "b") == 0)')

            # Passing .jsonl decision log directly as --baseline-signature works
            c2, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--log-file",
                    log_path,
                    "--baseline-signature",
                    decisions,
                    "--reject-on-slippage",
                    "--decision-log",
                    decisions,
                ],
                env=env,
            )
            self.assertEqual(c2, 0)
            r2 = self.read_jsonl(decisions)[-1]
            self.assertFalse(r2["slippage"]["slipped"])
            self.assertEqual(r2["slippage"]["match_type"], "exact_id")

    def test_clickhouse_password_passed_via_env_not_cli(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            args_file = os.path.join(td, "ch_args.txt")
            pwd_file = os.path.join(td, "ch_pwd.txt")
            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' "$@" > {shlex.quote(args_file)}
printf '%s' "$CLICKHOUSE_PASSWORD" > {shlex.quote(pwd_file)}
exit 0
""",
            )
            code, _, _ = self.run_oracle(
                "SELECT 1;",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log_path,
                    "--password",
                    "chsecret",
                ],
                env=self.path_env(td),
            )
            self.assertEqual(code, 1)
            with open(args_file, encoding="utf-8") as handle:
                argv = handle.read().splitlines()
            self.assertNotIn("--password", argv)
            self.assertNotIn("chsecret", argv)
            with open(pwd_file, encoding="utf-8") as handle:
                self.assertEqual(handle.read(), "chsecret")

    def test_sqlite_strip_error_blocks_caret_and_quoted_spoofs(self):
        with tempfile.TemporaryDirectory() as td:
            decisions = os.path.join(td, "decisions.jsonl")

            # 1. Caret-terminated (^--- error here) Parse error spoofing full ASan + assertion + blank line
            self.write_mock(
                td,
                "sqlite3",
                """cat >/dev/null || true
printf 'Parse error near line 1: near ";": syntax error\\n' >&2
printf '  SELECT "\\n' >&2
printf '==999==ERROR: AddressSanitizer: heap-buffer-overflow on address 0x6020\\n' >&2
printf '    #0 0x555555 in __asan_report_load4 /asan.c:1\\n' >&2
printf '    #1 0x555556 in sqlite3VdbeExec /tmp/sqlite/sqlite3.c:86402:13\\n' >&2
printf 'SUMMARY: AddressSanitizer: heap-buffer-overflow /tmp/sqlite/sqlite3.c:86402:13 in sqlite3VdbeExec\\n' >&2
printf '\\n' >&2
printf "sqlite3: /tmp/sqlite/sqlite3.c:1: fake: Assertion \\`spoof' failed.\\n" >&2
printf '  " FROM ;\\n' >&2
printf '                  ^--- error here\\n' >&2
exit 1
""",
            )
            c1, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "sqlite", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c1, 1)
            self.assertEqual(
                self.read_jsonl(decisions)[-1]["classification"], "healthy_no_fault"
            )

            # 2. Right-aligned (error here ---^) caret spoof cannot hijack a later real SIGSEGV signature
            self.write_mock(
                td,
                "sqlite3",
                """cat >/dev/null || true
printf 'Parse error near line 1: near ";": syntax error\\n' >&2
printf '  SELECT "sqlite3: /tmp/sqlite.c:1: fake: Assertion `spoof\\x27 failed." FROM ;\\n' >&2
printf "sqlite3: /tmp/sqlite.c:1: fake: Assertion \\`spoof' failed.\\n" >&2
printf '                                                  error here ---^\\n' >&2
exit 139
""",
            )
            c2, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "sqlite", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c2, 0)
            sig2 = self.read_jsonl(decisions)[-1]["crash_signature"]
            self.assertEqual(sig2["category"], "signal")
            self.assertEqual(sig2["signal"], "SIGSEGV")

            # 3. SQLite 3.31-style unclosed-quote Error: near line 1: near "...": syntax error (no caret)
            self.write_mock(
                td,
                "sqlite3",
                """cat >/dev/null || true
printf 'Error: near line 1: near "a\\n' >&2
printf '==999==ERROR: AddressSanitizer: heap-buffer-overflow on address 0x6020\\n' >&2
printf '    #0 0x555555 in __asan_report_load4 /asan.c:1\\n' >&2
printf '    #1 0x555556 in sqlite3VdbeExec /tmp/sqlite/sqlite3.c:86402:13\\n' >&2
printf 'SUMMARY: AddressSanitizer: heap-buffer-overflow /tmp/sqlite/sqlite3.c:86402:13 in sqlite3VdbeExec\\n' >&2
printf '": syntax error\\n' >&2
exit 1
""",
            )
            c3, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "sqlite", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c3, 1)
            self.assertEqual(
                self.read_jsonl(decisions)[-1]["classification"], "healthy_no_fault"
            )

    def test_duckdb_line_echo_without_caret_does_not_swallow_subsequent_faults(self):
        with tempfile.TemporaryDirectory() as td:
            decisions = os.path.join(td, "decisions.jsonl")

            # 1. Standalone LINE N: without caret followed by INTERNAL Error
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'LINE 1: SELECT 1;\\n' >&2
printf 'INTERNAL Error: real crash after caretless echo\\n' >&2
exit 1
""",
            )
            c1, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c1, 0)
            r1 = self.read_jsonl(decisions)[-1]
            self.assertEqual(r1["classification"], "confirmed_fault")
            self.assertEqual(
                r1["crash_signature"]["id"],
                "internal_error:real crash after caretless echo",
            )

            # 2. Catalog Error + LINE N: without caret followed by Out of Memory Error
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'Catalog Error: Table with name nope does not exist!\\n\\nLINE 1: SELECT * FROM nope;\\n' >&2
printf 'Out of Memory Error: failed to allocate\\n' >&2
exit 1
""",
            )
            c2, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c2, 1)
            r2 = self.read_jsonl(decisions)[-1]
            self.assertEqual(r2["classification"], "healthy_no_fault")
            self.assertEqual(r2["reason"], "oom")

            # 3. Multiline LINE N: spoof WITH caret is still rejected
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'LINE 1: SELECT "a\\nINTERNAL Error: spoof" FROM t;\\n                          ^\\n' >&2
exit 1
""",
            )
            c3, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c3, 1)
            self.assertEqual(
                self.read_jsonl(decisions)[-1]["classification"], "healthy_no_fault"
            )

    def test_clickhouse_multiline_basedaemon_query_with_inner_paren_stripped(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decisions = os.path.join(td, "decisions.jsonl")
            fatal_lines = "\n".join(
                [
                    "2026-11-22 12:00:01.000000 [ 99 ] {} <Fatal> BaseDaemon: (version 26.8.44.3) (query_id: q1) (query: SELECT (1)",
                    "Assertion `fake_paren' failed.",
                    ") Received signal Segmentation fault (11)",
                    "0. ./build_docker/./src/Core/Block.cpp:10: DB::Block::check() @ 0x0000aaaa",
                ]
            )
            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(fatal_lines)} >> {shlex.quote(log_path)}
exit 210
""",
            )
            c_spoof, _, _ = self.run_oracle(
                "SELECT (1)\nAssertion `fake_paren' failed.",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "fake_paren",
                    "--decision-log",
                    decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(c_spoof, 1)
            rec = self.read_jsonl(decisions)[-1]
            self.assertEqual(rec["reason"], "signature-mismatch")
            self.assertEqual(rec["crash_signature"]["category"], "signal")
            self.assertEqual(rec["crash_signature"]["signal"], "SIGSEGV")

    def test_duckdb_quoted_catalog_conversion_and_invalid_input_spoofs_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            decisions = os.path.join(td, "decisions.jsonl")
            spoofs = [
                # Conversion Error single-quoted multiline INTERNAL / FATAL spoof
                "printf \"Conversion Error: Could not convert string 'a\\nINTERNAL Error: spoof' to INT32\\n\\nLINE 1: SELECT CAST('a\\n               ^\\n\" >&2\nexit 1\n",
                "printf \"Conversion Error: Could not convert string 'a\\nFATAL Error: spoof' to INT32\\n\\nLINE 1: SELECT CAST('a\\n               ^\\n\" >&2\nexit 1\n",
                # Catalog Error with FATAL Error, full ASan text, ! punctuation, and 'does not exist!' inside identifier
                "printf 'Catalog Error: Table with name a\\nFATAL Error: spoof does not exist!\\nDid you mean \"pg_namespace\"?\\n\\nLINE 1: SELECT 1 FROM \"a\\n                      ^\\n' >&2\nexit 1\n",
                "printf 'Catalog Error: Table with name a\\n==123==ERROR: AddressSanitizer: fake\\n    #0 0x1 in f /a.c:1\\nSUMMARY: AddressSanitizer: fake /a.c:1 does not exist!\\nDid you mean \"pg_attrdef\"?\\n\\nLINE 1: SELECT 1 FROM \"a\\n                      ^\\n' >&2\nexit 1\n",
                "printf 'Catalog Error: Table with name a!\\nINTERNAL Error: spoof does not exist!\\n\\nLINE 1: SELECT 1 FROM \"a!\\n                      ^\\n' >&2\nexit 1\n",
                "printf 'Catalog Error: Table with name a does not exist!\\nINTERNAL Error: spoof does not exist!\\n\\nLINE 1: SELECT 1 FROM \"a does not exist!\\n                      ^\\n' >&2\nexit 1\n",
                # Invalid Input Error (no LINE N:) with FATAL Error inside quoted JSON input
                "printf 'Invalid Input Error: Malformed JSON at byte 2 of input: unexpected character.  Input: \"{\\nFATAL Error: spoof\"\\n' >&2\nexit 1\n",
                # Uncorroborated C assertion inside PRAGMA error at exit 1
                "printf \"Catalog Error: Pragma Function with name a!\\nduckdb: b.cpp:1: f: Assertion \\`x' failed. does not exist!\\nDid you mean \\\"tpcds\\\"?\\n\" >&2\nexit 1\n",
            ]
            for idx, body in enumerate(spoofs):
                self.write_mock(td, "duckdb", "cat >/dev/null || true\n" + body)
                code, _, _ = self.run_oracle(
                    "SELECT 1;",
                    args=["--dbms", "duckdb", "--decision-log", decisions],
                    env=self.path_env(td),
                )
                self.assertEqual(code, 1, f"duckdb spoof #{idx} unexpectedly accepted")
                rec = self.read_jsonl(decisions)[-1]
                self.assertEqual(rec["classification"], "healthy_no_fault")
                self.assertEqual(rec["reason"], "no-crash")

    def test_duckdb_constraint_quoted_parens_and_fk_catalog_do_not_swallow_faults(self):
        with tempfile.TemporaryDirectory() as td:
            decisions = os.path.join(td, "decisions.jsonl")

            # 1. Constraint Error with '(' inside single quotes followed by INTERNAL Error
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf "Constraint Error: CHECK constraint failed on table t with expression CHECK((a != '('))\\n" >&2
printf "INTERNAL Error: real crash after constraint\\nThis error signals an assertion failure within DuckDB.\\n" >&2
exit 1
""",
            )
            c1, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c1, 0)
            r1 = self.read_jsonl(decisions)[-1]
            self.assertEqual(r1["classification"], "confirmed_fault")
            self.assertEqual(r1["crash_signature"]["category"], "internal_error")

            # 2. Constraint Error with '(' inside single quotes followed by OOM
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf "Constraint Error: CHECK constraint failed on table t with expression CHECK((a != '('))\\n" >&2
printf "Out of Memory Error: failed to allocate\\n" >&2
exit 1
""",
            )
            c2, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c2, 1)
            r2 = self.read_jsonl(decisions)[-1]
            self.assertEqual(r2["classification"], "healthy_no_fault")
            self.assertEqual(r2["reason"], "oom")

            # 3. Catalog Error without trailing punctuation (FK drop error) followed by INTERNAL Error
            self.write_mock(
                td,
                "duckdb",
                """cat >/dev/null || true
printf 'Catalog Error: Could not drop the table because this table is main key table of the table "c"\\n' >&2
printf 'INTERNAL Error: real crash after fk drop\\n' >&2
exit 1
""",
            )
            c3, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "duckdb", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c3, 0)
            r3 = self.read_jsonl(decisions)[-1]
            self.assertEqual(r3["classification"], "confirmed_fault")
            self.assertEqual(r3["crash_signature"]["category"], "internal_error")

    def test_sqlite_blank_line_spoofs_and_quote_in_identifier_with_real_asan(self):
        with tempfile.TemporaryDirectory() as td:
            decisions = os.path.join(td, "decisions.jsonl")

            # 1-3. Blank-line separated assertion and UBSan spoofs at exit 1 are rejected
            spoofs = [
                'printf "Parse error near line 1: no such table: foo\\n\\nsqlite3: a.c:1: f: Assertion \\`x\' failed.\\n" >&2\nexit 1\n',
                'printf "Error near line 1: ok\\n\\nsqlite3: a.c:1: f: Assertion \\`x\' failed.\\n" >&2\nexit 1\n',
                "printf 'Parse error near line 1: no such table: foo\\n\\na.c:1: runtime error: signed integer overflow\\n' >&2\nexit 1\n",
            ]
            for idx, body in enumerate(spoofs):
                self.write_mock(td, "sqlite3", "cat >/dev/null || true\n" + body)
                code, _, _ = self.run_oracle(
                    "SELECT 1;",
                    args=["--dbms", "sqlite", "--decision-log", decisions],
                    env=self.path_env(td),
                )
                self.assertEqual(code, 1, f"sqlite blank-line spoof #{idx} accepted")
                self.assertEqual(
                    self.read_jsonl(decisions)[-1]["classification"], "healthy_no_fault"
                )

            # 4. Spoofed assertion cannot hijack a SIGSEGV (signal 11 != SIGABRT)
            self.write_mock(
                td,
                "sqlite3",
                """cat >/dev/null || true
printf "Parse error near line 1: no such table: foo\\nsqlite3: a.c:1: f: Assertion \\`spoof' failed.\\n" >&2
exit 139
""",
            )
            c_segv, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "sqlite", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c_segv, 0)
            sig_segv = self.read_jsonl(decisions)[-1]["crash_signature"]
            self.assertEqual(sig_segv["category"], "signal")
            self.assertEqual(sig_segv["signal"], "SIGSEGV")

            # 5. Embedded quote in `no such table: foo"bar` does not strip a real ASan report before a later caret error
            self.write_mock(
                td,
                "sqlite3",
                """cat >/dev/null || true
printf 'Parse error near line 1: no such table: foo"bar\\n' >&2
printf '==999==ERROR: AddressSanitizer: heap-buffer-overflow on address 0x6020\\n' >&2
printf '    #0 0x555555 in sqlite3VdbeExec /tmp/sqlite/sqlite3.c:86402:13\\n' >&2
printf 'SUMMARY: AddressSanitizer: heap-buffer-overflow /tmp/sqlite/sqlite3.c:86402:13 in sqlite3VdbeExec\\n' >&2
printf 'Parse error near line 2: near ";": syntax error\\n  SELECT 1 FROM ;\\n                ^--- error here\\n' >&2
exit 134
""",
            )
            c_asan, _, _ = self.run_oracle(
                "SELECT 1;",
                args=["--dbms", "sqlite", "--decision-log", decisions],
                env=self.path_env(td),
            )
            self.assertEqual(c_asan, 0)
            sig_asan = self.read_jsonl(decisions)[-1]["crash_signature"]
            self.assertEqual(sig_asan["category"], "asan")
            self.assertEqual(
                sig_asan["id"],
                "asan:heap-buffer-overflow:sqlite3VdbeExec@sqlite3.c:86402",
            )

    def test_clickhouse_basedaemon_asan_spoof_in_query_and_ubsan_aslr_normalization(self):
        with tempfile.TemporaryDirectory() as td:
            log_path = self.make_log(td)
            decisions = os.path.join(td, "decisions.jsonl")
            fatal_lines = "\n".join(
                [
                    "2026-11-22 12:00:01.000000 [ 99 ] {} <Fatal> BaseDaemon: (version 26.8.44.3) (query_id: q1) (query: SELECT (1)",
                    "==123==ERROR: AddressSanitizer: fake_in_query",
                    "1. fake_frame_in_query",
                    ") Received signal Segmentation fault (11)",
                    "0. ./build_docker/./src/Core/Block.cpp:10: DB::Block::check() @ 0x0000aaaa in /usr/bin/clickhouse",
                ]
            )
            self.write_mock(
                td,
                "clickhouse-client",
                f"""cat >/dev/null || true
printf '%s\\n' {shlex.quote(fatal_lines)} >> {shlex.quote(log_path)}
exit 210
""",
            )
            c_ch, _, _ = self.run_oracle(
                "SELECT (1);",
                args=[
                    "--dbms",
                    "clickhouse",
                    "--log-file",
                    log_path,
                    "--expected-signature",
                    "fake_in_query",
                    "--decision-log",
                    decisions,
                ],
                env=self.path_env(td),
            )
            self.assertEqual(c_ch, 1)
            rec_ch = self.read_jsonl(decisions)[-1]
            self.assertEqual(rec_ch["reason"], "signature-mismatch")
            self.assertEqual(
                rec_ch["crash_signature"]["id"],
                "signal:SIGSEGV:DB::Block::check()@src/Core/Block.cpp:10",
            )

            # UBSan error containing volatile 0x... pointer addresses normalizes identically across ASLR runs
            for addr in ("0x603000001230", "0x7fffdeadbeef"):
                self.write_mock(
                    td,
                    "sqlite3",
                    f"""cat >/dev/null || true
printf "/src/node.c:10:5: runtime error: member call on address {addr} which does not point to an object of type 'Node'\\n" >&2
exit 134
""",
                )
                c_ub, _, _ = self.run_oracle(
                    "SELECT 1;",
                    args=["--dbms", "sqlite", "--decision-log", decisions],
                    env=self.path_env(td),
                )
                self.assertEqual(c_ub, 0)
            recs = self.read_jsonl(decisions)
            self.assertEqual(recs[-2]["crash_signature"]["id"], recs[-1]["crash_signature"]["id"])
            self.assertEqual(recs[-2]["crash_signature"]["hash"], recs[-1]["crash_signature"]["hash"])
            self.assertIn("0xADDR", recs[-1]["crash_signature"]["id"])


if __name__ == "__main__":
    unittest.main()

#!/usr/bin/env python3
"""Contract tests for harness/reset_db.sh using mock DBMS clients."""

import os
import subprocess
import tempfile
import unittest


class TestResetDb(unittest.TestCase):
    def setUp(self):
        self.script = os.path.abspath(
            os.path.join(os.path.dirname(__file__), "reset_db.sh")
        )

    def write_mock(self, directory, name, body):
        path = os.path.join(directory, name)
        with open(path, "w", encoding="utf-8") as handle:
            handle.write("#!/bin/sh\nset -e\n" + body)
        os.chmod(path, 0o755)

    def run_reset(self, args, path_dir, env=None):
        run_env = os.environ.copy()
        # reset_db.sh resolves clients with `command -v`; put the mock directory
        # first so the real psql/mysql binaries are never contacted.
        run_env["PATH"] = f"{path_dir}:{run_env.get('PATH', '')}"
        for key in (
            "DBMS",
            "PGHOST",
            "PGPORT",
            "PGUSER",
            "PGPASSWORD",
            "PGDATABASE",
            "MYSQL_HOST",
            "MYSQL_TCP_PORT",
            "MYSQL_USER",
            "MYSQL_PWD",
            "MYSQL_DATABASE",
            "MYSQL_SOCKET",
            "CONTAINER_NAME",
            "TEMPLATE_DB",
        ):
            run_env.pop(key, None)
        if env:
            run_env.update(env)
        return subprocess.run(
            [self.script] + args,
            check=False,
            capture_output=True,
            text=True,
            env=run_env,
        )

    def test_postgres_uses_dbms_aware_defaults_and_exports_password(self):
        with tempfile.TemporaryDirectory() as td:
            args_file = os.path.join(td, "args.txt")
            sql_file = os.path.join(td, "sql.txt")
            password_file = os.path.join(td, "password.txt")
            self.write_mock(td, "pg_isready", "exit 0\n")
            self.write_mock(
                td,
                "psql",
                f"""printf '%s\\n' "$@" > {args_file}
cat > {sql_file}
printf '%s' "$PGPASSWORD" > {password_file}
exit 0
""",
            )
            result = self.run_reset(
                ["--dbms", "postgres", "--password", "secret"],
                td,
                env={"PGHOST": "pg-host", "MYSQL_HOST": "mysql-host"},
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            with open(args_file, encoding="utf-8") as handle:
                argv = handle.read().splitlines()
            self.assertIn("pg-host", argv)
            self.assertNotIn("mysql-host", argv)
            with open(password_file, encoding="utf-8") as handle:
                self.assertEqual(handle.read(), "secret")
            with open(sql_file, encoding="utf-8") as handle:
                sql = handle.read()
            self.assertIn('DROP DATABASE IF EXISTS :"dbname";', sql)
            self.assertIn('CREATE DATABASE :"dbname" TEMPLATE :"template";', sql)

    def test_postgres_does_not_interpolate_database_name_into_sql(self):
        with tempfile.TemporaryDirectory() as td:
            sql_file = os.path.join(td, "sql.txt")
            args_file = os.path.join(td, "args.txt")
            self.write_mock(td, "pg_isready", "exit 0\n")
            self.write_mock(
                td,
                "psql",
                f"""printf '%s\\n' "$@" > {args_file}
cat > {sql_file}
exit 0
""",
            )
            dangerous = 'occam_test"; DROP DATABASE postgres; --'
            result = self.run_reset(
                ["--dbms", "postgres", "--dbname", dangerous],
                td,
                env={"PGHOST": "pg-host"},
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            with open(args_file, encoding="utf-8") as handle:
                argv = handle.read().splitlines()
            self.assertTrue(any(dangerous in arg for arg in argv))
            with open(sql_file, encoding="utf-8") as handle:
                sql = handle.read()
            self.assertNotIn(dangerous, sql)
            self.assertIn('DROP DATABASE IF EXISTS :"dbname";', sql)

    def test_mysql_uses_mysql_defaults_and_clears_read_only_globals(self):
        with tempfile.TemporaryDirectory() as td:
            args_file = os.path.join(td, "args.txt")
            sql_file = os.path.join(td, "sql.txt")
            password_file = os.path.join(td, "password.txt")
            self.write_mock(td, "mysqladmin", "exit 0\n")
            self.write_mock(
                td,
                "mysql",
                f"""printf '%s\\n' "$@" > {args_file}
cat > {sql_file}
printf '%s' "$MYSQL_PWD" > {password_file}
exit 0
""",
            )
            result = self.run_reset(
                ["--dbms", "mysql", "--password", "secret"],
                td,
                env={"PGHOST": "pg-host", "MYSQL_HOST": "mysql-host"},
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            with open(args_file, encoding="utf-8") as handle:
                argv = handle.read().splitlines()
            self.assertIn("mysql-host", argv)
            self.assertNotIn("pg-host", argv)
            self.assertIn("--no-defaults", argv)
            with open(password_file, encoding="utf-8") as handle:
                self.assertEqual(handle.read(), "secret")
            with open(sql_file, encoding="utf-8") as handle:
                sql = handle.read()
            self.assertIn("SET GLOBAL super_read_only = OFF;", sql)
            self.assertIn("SET GLOBAL read_only = OFF;", sql)
            self.assertIn("DROP DATABASE IF EXISTS `occam_test`;", sql)

    def test_mysql_rejects_unsafe_database_name(self):
        with tempfile.TemporaryDirectory() as td:
            called = os.path.join(td, "called")
            self.write_mock(td, "mysqladmin", f"touch {called}\nexit 0\n")
            self.write_mock(td, "mysql", f"touch {called}\nexit 0\n")
            result = self.run_reset(
                ["--dbms", "mysql", "--dbname", "bad;name"],
                td,
                env={"MYSQL_HOST": "mysql-host"},
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("must match", result.stderr)
            self.assertFalse(os.path.exists(called))

    def test_mysql_socket_overrides_host_and_port(self):
        with tempfile.TemporaryDirectory() as td:
            args_file = os.path.join(td, "args.txt")
            self.write_mock(td, "mysqladmin", "exit 0\n")
            self.write_mock(
                td,
                "mysql",
                f"""cat >/dev/null
printf '%s\\n' "$@" > {args_file}
exit 0
""",
            )
            result = self.run_reset(
                [
                    "--dbms",
                    "mysql",
                    "--host",
                    "ignored-host",
                    "--port",
                    "9999",
                    "--socket",
                    "/tmp/mysql.sock",
                ],
                td,
                env={"MYSQL_HOST": "mysql-host"},
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            with open(args_file, encoding="utf-8") as handle:
                argv = handle.read().splitlines()
            self.assertIn("--socket", argv)
            self.assertIn("/tmp/mysql.sock", argv)
            self.assertNotIn("-h", argv)
            self.assertNotIn("9999", argv)


if __name__ == "__main__":
    unittest.main()

#!/usr/bin/env python3
"""Contract tests for harness/sqlancer_driver.sh and harness/squirrel_driver.sh."""

import os
import subprocess
import tempfile
import unittest

HARNESS_DIR = os.path.dirname(os.path.abspath(__file__))
SQLANCER_DRIVER = os.path.join(HARNESS_DIR, "sqlancer_driver.sh")
SQUIRREL_DRIVER = os.path.join(HARNESS_DIR, "squirrel_driver.sh")


class TestFuzzerDrivers(unittest.TestCase):
    def _clean_env(self):
        env = os.environ.copy()
        for key in (
            "DBMS",
            "PGHOST",
            "PGPORT",
            "PGUSER",
            "PGPASSWORD",
            "MYSQL_HOST",
            "MYSQL_TCP_PORT",
            "MYSQL_USER",
            "MYSQL_PWD",
            "MARIADB_HOST",
            "MARIADB_TCP_PORT",
            "MARIADB_USER",
            "MARIADB_PWD",
            "CLICKHOUSE_HOST",
            "CLICKHOUSE_HTTP_PORT",
            "CLICKHOUSE_USER",
            "CLICKHOUSE_PASSWORD",
            "SQLANCER_JAR",
            "SQUIRREL_HOME",
            "AFL_MAP_SIZE",
            "NUM_THREADS",
        ):
            env.pop(key, None)
        return env

    def _write_exec(self, directory, name, body):
        path = os.path.join(directory, name)
        with open(path, "w", encoding="utf-8") as handle:
            handle.write("#!/bin/sh\nset -e\n" + body)
        os.chmod(path, 0o755)
        return path

    def test_sqlancer_help_and_validation(self):
        res_help = subprocess.run(
            [SQLANCER_DRIVER, "--help"],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(res_help.returncode, 0)
        self.assertIn("--dbms", res_help.stdout)

        res_missing = subprocess.run(
            [SQLANCER_DRIVER],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(res_missing.returncode, 2)

        res_bad = subprocess.run(
            [SQLANCER_DRIVER, "--dbms", "mssql"],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(res_bad.returncode, 2)

        res_threads = subprocess.run(
            [SQLANCER_DRIVER, "--dbms", "sqlite", "--num-threads", "0"],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(res_threads.returncode, 2)

    def test_sqlancer_postgres_defaults_and_option_ordering(self):
        with tempfile.TemporaryDirectory() as td:
            jar = os.path.join(td, "sqlancer.jar")
            with open(jar, "wb") as handle:
                handle.write(b"jar")
            out_dir = os.path.join(td, "out")
            env = self._clean_env()
            env["PGHOST"] = "/var/run/postgresql"
            env["MYSQL_HOST"] = "wrong-mysql-host"

            res = subprocess.run(
                [
                    SQLANCER_DRIVER,
                    "--dbms",
                    "postgres",
                    "--sqlancer-jar",
                    jar,
                    "--out-dir",
                    out_dir,
                    "--num-threads",
                    "2",
                    "--oracle",
                    "QUERY_PARTITIONING",
                    "--dry-run",
                    "--",
                    "--test-collations=false",
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res.returncode, 0, res.stderr)
            self.assertIn("DBMS=postgres", res.stdout)
            # Unix socket path must normalize to TCP 127.0.0.1 and ignore MYSQL_HOST
            self.assertIn("--host 127.0.0.1", res.stdout)
            self.assertNotIn("wrong-mysql-host", res.stdout)
            self.assertIn("--port 5432", res.stdout)
            self.assertIn("--username postgres", res.stdout)
            # MainOptions must appear BEFORE subcommand `postgres`, oracle/extra AFTER
            cmd_line = next(
                ln for ln in res.stdout.splitlines() if ln.startswith("CMD=")
            )
            idx_host = cmd_line.index("--host")
            idx_threads = cmd_line.index("--num-threads")
            idx_sub = cmd_line.index(" postgres ")
            idx_oracle = cmd_line.index("--oracle")
            idx_extra = cmd_line.index("--test-collations")
            self.assertLess(idx_host, idx_sub)
            self.assertLess(idx_threads, idx_sub)
            self.assertLess(idx_sub, idx_oracle)
            self.assertLess(idx_oracle, idx_extra)

    def test_sqlancer_clickhouse_http_port_and_sqlite_embedded(self):
        with tempfile.TemporaryDirectory() as td:
            jar = os.path.join(td, "sqlancer.jar")
            with open(jar, "wb") as handle:
                handle.write(b"jar")
            env = self._clean_env()

            ch = subprocess.run(
                [
                    SQLANCER_DRIVER,
                    "--dbms",
                    "clickhouse",
                    "--sqlancer-jar",
                    jar,
                    "--out-dir",
                    os.path.join(td, "ch"),
                    "--dry-run",
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(ch.returncode, 0, ch.stderr)
            self.assertIn("--port 8123", ch.stdout)
            self.assertIn("--username default", ch.stdout)

            sq = subprocess.run(
                [
                    SQLANCER_DRIVER,
                    "--dbms",
                    "sqlite",
                    "--sqlancer-jar",
                    jar,
                    "--oracle",
                    "NoREC",
                    "--out-dir",
                    os.path.join(td, "sq"),
                    "--dry-run",
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(sq.returncode, 0, sq.stderr)
            self.assertIn(" sqlite3 --oracle NoREC", sq.stdout)
            self.assertNotIn("--host", sq.stdout)

    def test_sqlancer_mock_execution_creates_test_db_and_runs_in_outdir(self):
        with tempfile.TemporaryDirectory() as td:
            jar = os.path.join(td, "sqlancer.jar")
            with open(jar, "wb") as handle:
                handle.write(b"jar")
            out_dir = os.path.join(td, "run_out")
            psql_log = os.path.join(td, "psql_calls.txt")
            java_cwd = os.path.join(td, "java_cwd.txt")
            java_args = os.path.join(td, "java_args.txt")

            mock_psql = self._write_exec(
                td,
                "psql",
                f"""printf '%s\\n' "$*" >> {psql_log}
exit 0
""",
            )
            del mock_psql
            mock_java = self._write_exec(
                td,
                "java",
                f"""pwd > {java_cwd}
printf '%s\\n' "$@" > {java_args}
exit 0
""",
            )

            env = self._clean_env()
            env["PATH"] = f"{td}:{env.get('PATH', '')}"
            res = subprocess.run(
                [
                    SQLANCER_DRIVER,
                    "--dbms",
                    "postgres",
                    "--sqlancer-jar",
                    jar,
                    "--java-bin",
                    mock_java,
                    "--out-dir",
                    out_dir,
                    "--num-queries",
                    "50",
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res.returncode, 0, res.stderr)
            with open(java_cwd, encoding="utf-8") as handle:
                self.assertEqual(os.path.realpath(handle.read().strip()), os.path.realpath(out_dir))
            with open(psql_log, encoding="utf-8") as handle:
                psql_text = handle.read()
            self.assertIn("CREATE DATABASE test;", psql_text)

    def test_squirrel_validation_and_multi_dbms_dry_run(self):
        with tempfile.TemporaryDirectory() as td:
            sq_home = os.path.join(td, "squirrel")
            os.makedirs(os.path.join(sq_home, "data", "fuzz_root"), exist_ok=True)
            with open(os.path.join(sq_home, "mapsize.postgresql"), "w", encoding="utf-8") as handle:
                handle.write("65536\n")
            env = self._clean_env()

            res_bad = subprocess.run(
                [SQUIRREL_DRIVER, "--dbms", "clickhouse"],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res_bad.returncode, 2)

            out_sq = os.path.join(td, "out_sq")
            res_sq = subprocess.run(
                [
                    SQUIRREL_DRIVER,
                    "--dbms",
                    "sqlite",
                    "--squirrel-home",
                    sq_home,
                    "--output-dir",
                    out_sq,
                    "--timeout-secs",
                    "120",
                    "--dry-run",
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res_sq.returncode, 0, res_sq.stderr)
            self.assertIn("libsqlite_mutator.so", res_sq.stdout)
            self.assertIn("-V 120", res_sq.stdout)
            self.assertIn("sqlite_ossfuzz @@", res_sq.stdout)
            cfg_sq = os.path.join(out_sq, "squirrel_config_sqlite.yml")
            self.assertTrue(os.path.isfile(cfg_sq))
            with open(cfg_sq, encoding="utf-8") as handle:
                self.assertIn("db: sqlite", handle.read())

            # MariaDB shares libmysql_mutator.so and sets db: mysql in generated config
            out_md = os.path.join(td, "out_md")
            res_md = subprocess.run(
                [
                    SQUIRREL_DRIVER,
                    "--dbms",
                    "mariadb",
                    "--squirrel-home",
                    sq_home,
                    "--output-dir",
                    out_md,
                    "--password",
                    "p'w",
                    "--dry-run",
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res_md.returncode, 0, res_md.stderr)
            self.assertIn("libmysql_mutator.so", res_md.stdout)
            self.assertIn("-t 60000 -- ", res_md.stdout)
            self.assertIn("db_driver", res_md.stdout)
            with open(os.path.join(out_md, "squirrel_config_mariadb.yml"), encoding="utf-8") as handle:
                yml = handle.read()
            self.assertIn("db: mysql", yml)
            self.assertIn("passwd: 'p''w'", yml)

            # PostgreSQL picks up mapsize.postgresql
            out_pg = os.path.join(td, "out_pg")
            res_pg = subprocess.run(
                [
                    SQUIRREL_DRIVER,
                    "--dbms",
                    "postgres",
                    "--squirrel-home",
                    sq_home,
                    "--output-dir",
                    out_pg,
                    "--dry-run",
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res_pg.returncode, 0, res_pg.stderr)
            self.assertIn("DBMS=postgresql", res_pg.stdout)
            self.assertIn("AFL_MAP_SIZE=65536", res_pg.stdout)
            self.assertIn("libpostgresql_mutator.so", res_pg.stdout)

    def test_squirrel_mock_execution_exports_afl_env(self):
        with tempfile.TemporaryDirectory() as td:
            sq_home = os.path.join(td, "squirrel")
            in_dir = os.path.join(sq_home, "data", "fuzz_root", "input")
            os.makedirs(in_dir, exist_ok=True)
            os.makedirs(os.path.join(sq_home, "build"), exist_ok=True)
            os.makedirs(os.path.join(sq_home, "bin"), exist_ok=True)
            os.makedirs(os.path.join(sq_home, "AFLplusplus"), exist_ok=True)
            mut_path = os.path.join(sq_home, "build", "libsqlite_mutator.so")
            with open(mut_path, "wb") as handle:
                handle.write(b"so")
            target = self._write_exec(os.path.join(sq_home, "bin"), "sqlite_ossfuzz", "exit 0\n")
            del target
            env_out = os.path.join(td, "afl_env.txt")
            self._write_exec(
                os.path.join(sq_home, "AFLplusplus"),
                "afl-fuzz",
                f"""printf '%s\\n' "ONLY=$AFL_CUSTOM_MUTATOR_ONLY" "NOTRIM=$AFL_DISABLE_TRIM" "NOUI=$AFL_NO_UI" "LIB=$AFL_CUSTOM_MUTATOR_LIBRARY" "CFG=$SQUIRREL_CONFIG" > {env_out}
exit 0
""",
            )
            out_dir = os.path.join(td, "out")
            res = subprocess.run(
                [
                    SQUIRREL_DRIVER,
                    "--dbms",
                    "sqlite",
                    "--squirrel-home",
                    sq_home,
                    "--output-dir",
                    out_dir,
                ],
                env=self._clean_env(),
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res.returncode, 0, res.stderr)
            with open(env_out, encoding="utf-8") as handle:
                txt = handle.read()
            self.assertIn("ONLY=1", txt)
            self.assertIn("NOTRIM=1", txt)
            self.assertIn("NOUI=1", txt)
            self.assertIn(f"LIB={mut_path}", txt)
            self.assertIn(f"CFG={os.path.join(out_dir, 'squirrel_config_sqlite.yml')}", txt)


class TestFuzzerDriverRegressions(unittest.TestCase):
    def _write_exec(self, path, content="#!/bin/sh\nexit 0\n"):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8") as handle:
            handle.write(content)
        os.chmod(path, 0o755)

    def _base_env(self, extra_bin):
        env = {}
        for key in ("PATH", "HOME", "LANG", "LC_ALL", "TERM", "TMPDIR"):
            if key in os.environ:
                env[key] = os.environ[key]
        env["PATH"] = extra_bin + os.pathsep + env.get("PATH", "/usr/bin:/bin")
        return env

    def test_sqlancer_skips_sources_and_javadoc_jars_in_target_dir(self):
        sqlancer_driver = os.path.join(os.path.dirname(SQUIRREL_DRIVER), "sqlancer_driver.sh")
        with tempfile.TemporaryDirectory() as td:
            bin_dir = os.path.join(td, "bin")
            argv_log = os.path.join(td, "java_argv.txt")
            cwd_log = os.path.join(td, "java_cwd.txt")
            self._write_exec(
                os.path.join(bin_dir, "java"),
                "#!/bin/sh\n"
                f"pwd -P > '{cwd_log}'\n"
                f"printf '%s\\n' \"$@\" > '{argv_log}'\n"
                "exit 0\n",
            )
            target_dir = os.path.join(td, "target")
            os.makedirs(target_dir, exist_ok=True)
            for name in (
                "sqlancer-2.0.0-javadoc.jar",
                "sqlancer-2.0.0-sources.jar",
                "sqlancer-2.0.0-tests.jar",
                "sqlancer-2.0.0.jar",
            ):
                with open(os.path.join(target_dir, name), "w", encoding="utf-8") as handle:
                    handle.write("stub\n")
            out_dir = os.path.join(td, "out")
            env = self._base_env(bin_dir)
            env["SQLANCER_HOME"] = os.path.join(td, "no_sqlancer_home")
            res = subprocess.run(
                [
                    sqlancer_driver,
                    "--dbms",
                    "sqlite",
                    "--output-dir",
                    out_dir,
                    "--timeout-seconds",
                    "5",
                ],
                cwd=td,
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res.returncode, 0, res.stderr + "\n" + res.stdout)
            with open(cwd_log, encoding="utf-8") as handle:
                self.assertEqual(handle.read().strip(), os.path.realpath(out_dir))
            with open(argv_log, encoding="utf-8") as handle:
                lines = [line.strip() for line in handle.read().splitlines() if line.strip()]
            self.assertNotIn("--output-dir", lines)
            self.assertIn("--timeout-seconds", lines)
            self.assertIn("-jar", lines)
            jar_idx = lines.index("-jar") + 1
            self.assertEqual(os.path.basename(lines[jar_idx]), "sqlancer-2.0.0.jar")
            self.assertTrue(
                any(name.startswith("sqlancer_sqlite_") and name.endswith(".log") for name in os.listdir(out_dir)),
                os.listdir(out_dir),
            )

    def test_squirrel_mysql_mariadb_server_cmd_and_empty_mapsize(self):
        for dbms, bin_rel, server_name in (
            ("mysql", os.path.join("targets", "mysql", "bin", "mysqld"), "mysqld"),
            ("mariadb", os.path.join("targets", "mariadb", "bin", "mariadbd"), "mariadbd"),
        ):
            with self.subTest(dbms=dbms), tempfile.TemporaryDirectory() as td:
                bin_dir = os.path.join(td, "bin")
                sq_home = os.path.join(td, "squirrel")
                out_dir = os.path.join(td, "out")
                env_log = os.path.join(td, "afl_env.txt")
                self._write_exec(
                    os.path.join(sq_home, "AFLplusplus", "afl-fuzz"),
                    "#!/bin/sh\n"
                    f"printf 'MAP=%s\\nARGS=%s\\n' \"${{AFL_MAP_SIZE-UNSET}}\" \"$*\" > '{env_log}'\n"
                    "exit 0\n",
                )
                server_path = os.path.join(sq_home, bin_rel)
                self._write_exec(server_path)
                db_driver = os.path.join(sq_home, "build", "db_driver")
                self._write_exec(db_driver)
                mut_path = os.path.join(sq_home, "build", "libmysql_mutator.so")
                self._write_exec(mut_path, "stub")
                data_lib = os.path.join(sq_home, "data", "fuzz_root", "mysql_init_lib")
                os.makedirs(data_lib, exist_ok=True)
                with open(os.path.join(data_lib, "seed.sql"), "w", encoding="utf-8") as h:
                    h.write("SELECT 1;\n")
                input_dir = os.path.join(sq_home, "data", "fuzz_root", "mysql_input")
                os.makedirs(input_dir, exist_ok=True)
                with open(os.path.join(input_dir, "seed.sql"), "w", encoding="utf-8") as h:
                    h.write("SELECT 1;\n")

                mapsize_file = os.path.join(sq_home, f"mapsize.{dbms}")
                with open(mapsize_file, "w", encoding="utf-8") as h:
                    h.write("")

                env = self._base_env(bin_dir)
                env["SQUIRREL_MUTATOR_LIB"] = mut_path
                env["SQUIRREL_INIT_LIB"] = data_lib
                res = subprocess.run(
                    [
                        SQUIRREL_DRIVER,
                        "--dbms",
                        dbms,
                        "--squirrel-home",
                        sq_home,
                        "--output-dir",
                        out_dir,
                        "--port",
                        "13306",
                    ],
                    env=env,
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertEqual(res.returncode, 0, res.stderr + "\n" + res.stdout)
                with open(env_log, encoding="utf-8") as h:
                    afl_txt = h.read()
                self.assertIn("MAP=UNSET\n", afl_txt)

                cfg_path = os.path.join(out_dir, f"squirrel_config_{dbms}.yml")
                with open(cfg_path, encoding="utf-8") as h:
                    cfg_txt = h.read()
                self.assertIn(server_path, cfg_txt)
                self.assertIn(server_name, cfg_txt)
                self.assertIn(os.path.join(os.path.realpath(out_dir), "server", "err_log.err"), cfg_txt)
                self.assertIn("--port=13306", cfg_txt)
                self.assertIn("port: 13306", cfg_txt)
                self.assertIn("while true; do", cfg_txt)

                with open(mapsize_file, "w", encoding="utf-8") as h:
                    h.write("262144\n")
                res2 = subprocess.run(
                    [
                        SQUIRREL_DRIVER,
                        "--dbms",
                        dbms,
                        "--squirrel-home",
                        sq_home,
                        "--output-dir",
                        out_dir,
                    ],
                    env=env,
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertEqual(res2.returncode, 0, res2.stderr + "\n" + res2.stdout)
                with open(env_log, encoding="utf-8") as h:
                    afl_txt2 = h.read()
                self.assertIn("MAP=262144\n", afl_txt2)


if __name__ == "__main__":
    unittest.main()

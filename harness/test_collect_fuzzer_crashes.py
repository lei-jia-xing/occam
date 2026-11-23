#!/usr/bin/env python3
"""Unit tests for harness/collect_fuzzer_crashes.py."""

import json
import os
import sys
import tempfile
import unittest

HARNESS_DIR = os.path.dirname(os.path.abspath(__file__))
if HARNESS_DIR not in sys.path:
    sys.path.insert(0, HARNESS_DIR)

from collect_fuzzer_crashes import (
    collect_crashes,
    sanitize_sqlancer_log,
)


class TestCollectFuzzerCrashes(unittest.TestCase):
    def _write_exec(self, directory, name, body):
        path = os.path.join(directory, name)
        with open(path, "w", encoding="utf-8") as handle:
            handle.write("#!/bin/sh\n" + body)
        os.chmod(path, 0o755)
        return path

    def test_sanitize_sqlancer_log_strips_stacktrace_and_db_lifecycle(self):
        raw = b"""--java.lang.AssertionError: bug
--\tat sqlancer.Main.run(Main.java:123)
-- Time: 2026-11-23
-- Database: fuzzdb7
-- Database version: 13.0
-- seed value: 42
\\c test;
DROP DATABASE FORCE IF EXISTS fuzzdb7;
CREATE DATABASE fuzzdb7 WITH ENCODING 'UTF8' TEMPLATE template0;
\\c fuzzdb7;
CREATE TABLE t0(c0 INT);
INSERT INTO t0 VALUES(1);
SELECT * FROM t0;
"""
        cleaned, meta = sanitize_sqlancer_log(raw)
        txt = cleaned.decode("utf-8")
        self.assertEqual(meta["database"], "fuzzdb7")
        self.assertEqual(meta["seed_value"], "42")
        self.assertNotIn("AssertionError", txt)
        self.assertNotIn("\\c ", txt)
        self.assertNotIn("DROP DATABASE", txt)
        self.assertNotIn("CREATE DATABASE", txt)
        self.assertIn("CREATE TABLE t0(c0 INT);", txt)
        self.assertIn("SELECT * FROM t0;", txt)

    def test_squirrel_collection_deduplicates_bytes_and_skips_readme(self):
        with tempfile.TemporaryDirectory() as td:
            in_dir = os.path.join(td, "squirrel_out")
            crash_a = os.path.join(in_dir, "squirrel0", "crashes")
            crash_b = os.path.join(in_dir, "squirrel1", "crashes")
            os.makedirs(crash_a)
            os.makedirs(crash_b)
            with open(os.path.join(crash_a, "README.txt"), "w", encoding="utf-8") as h:
                h.write("afl readme")
            with open(os.path.join(crash_a, "id:000000,sig:06"), "w", encoding="utf-8") as h:
                h.write("CREATE TABLE t(a INT);\nSELECT * FROM t;\n")
            with open(os.path.join(crash_b, "id:000000,sig:06"), "w", encoding="utf-8") as h:
                h.write("CREATE TABLE t(a INT);\nSELECT * FROM t;\n")
            with open(os.path.join(crash_b, "id:000001,sig:11"), "w", encoding="utf-8") as h:
                h.write("CREATE VIRTUAL TABLE vt USING fts5(x);\n")

            out_dir = os.path.join(td, "collected")
            manifest, manifest_path = collect_crashes(
                input_dir=in_dir,
                out_dir=out_dir,
                source="squirrel",
                dbms="sqlite",
            )
            self.assertTrue(os.path.isfile(manifest_path))
            self.assertEqual(manifest["summary"]["raw_candidates_discovered"], 3)
            self.assertEqual(manifest["summary"]["unique_byte_candidates"], 2)
            self.assertEqual(manifest["summary"]["manifest_case_count"], 2)
            for case in manifest["cases"]:
                self.assertTrue(os.path.isfile(case["file"]))

    def test_sqlancer_collection_prefers_primary_logs_and_falls_back_to_cur(self):
        with tempfile.TemporaryDirectory() as td:
            in_dir = os.path.join(td, "logs", "sqlite3")
            os.makedirs(in_dir)
            with open(os.path.join(in_dir, "database0-cur.log"), "w", encoding="utf-8") as h:
                h.write("-- Database: database0\nCREATE TABLE cur_tbl(a INT);\n")
            with open(os.path.join(in_dir, "database1-plan.log"), "w", encoding="utf-8") as h:
                h.write("SeqScan\n")
            with open(os.path.join(in_dir, "database0.log"), "w", encoding="utf-8") as h:
                h.write("--java.lang.AssertionError\nDROP DATABASE IF EXISTS database0;\nCREATE DATABASE database0;\nUSE database0;\nCREATE TABLE bug_tbl(a INT);\n")

            out1 = os.path.join(td, "out1")
            m1, _ = collect_crashes(
                input_dir=in_dir,
                out_dir=out1,
                source="sqlancer",
                dbms="sqlite",
                include_cur_logs=False,
            )
            self.assertEqual(m1["summary"]["manifest_case_count"], 1)
            with open(m1["cases"][0]["file"], encoding="utf-8") as h:
                self.assertIn("bug_tbl", h.read())

            # When only -cur.log exists (e.g. JVM crashed inside JNI), fallback picks it up.
            os.unlink(os.path.join(in_dir, "database0.log"))
            out2 = os.path.join(td, "out2")
            m2, _ = collect_crashes(
                input_dir=in_dir,
                out_dir=out2,
                source="sqlancer",
                dbms="sqlite",
            )
            self.assertEqual(m2["summary"]["manifest_case_count"], 1)
            with open(m2["cases"][0]["file"], encoding="utf-8") as h:
                self.assertIn("cur_tbl", h.read())

    def test_verify_filters_flaky_and_deduplicates_by_crash_signature(self):
        with tempfile.TemporaryDirectory() as td:
            in_dir = os.path.join(td, "raw")
            os.makedirs(in_dir)
            # Case 1 & Case 2 trigger the exact same assertion signature (deduped to 1)
            with open(os.path.join(in_dir, "c1.sql"), "w", encoding="utf-8") as h:
                h.write("SELECT 'TRIGGER_A1';\n")
            with open(os.path.join(in_dir, "c2.sql"), "w", encoding="utf-8") as h:
                h.write("SELECT 'TRIGGER_A2';\n")
            # Case 3 triggers a distinct assertion signature
            with open(os.path.join(in_dir, "c3.sql"), "w", encoding="utf-8") as h:
                h.write("SELECT 'TRIGGER_B';\n")
            # Case 4 fails intermittently (flaky: passes 1st time, healthy 2nd time)
            with open(os.path.join(in_dir, "c4_flaky.sql"), "w", encoding="utf-8") as h:
                h.write("SELECT 'TRIGGER_FLAKY';\n")

            state_file = os.path.join(td, "flaky_count")
            mock_sqlite = self._write_exec(
                td,
                "mock_sqlite3",
                f"""sql="$(cat)"
case "$sql" in
  *TRIGGER_A1*|*TRIGGER_A2*)
    echo "sqlite3: sqlite3.c:100: fnA: Assertion \\`x == 1' failed." >&2
    exit 134
    ;;
  *TRIGGER_B*)
    echo "sqlite3: sqlite3.c:200: fnB: Assertion \\`y == 2' failed." >&2
    exit 134
    ;;
  *TRIGGER_FLAKY*)
    if [ ! -f {state_file} ]; then
      touch {state_file}
      echo "sqlite3: sqlite3.c:300: fnF: Assertion \\`z == 3' failed." >&2
      exit 134
    fi
    exit 0
    ;;
  *)
    exit 0
    ;;
esac
""",
            )
            out_dir = os.path.join(td, "verified")
            manifest, _ = collect_crashes(
                input_dir=in_dir,
                out_dir=out_dir,
                source="raw",
                dbms="sqlite",
                verify=True,
                replays=5,
                dedup_by_signature=True,
                engine_bin=mock_sqlite,
            )
            self.assertEqual(manifest["summary"]["raw_candidates_discovered"], 4)
            self.assertEqual(manifest["summary"]["rejected_unstable_or_noncrashing"], 1)
            self.assertEqual(manifest["summary"]["signature_duplicates_collapsed"], 1)
            self.assertEqual(manifest["summary"]["unique_signatures"], 2)
            self.assertEqual(manifest["summary"]["manifest_case_count"], 2)
            ids = {c["id"] for c in manifest["cases"]}
            self.assertEqual(ids, {"c1", "c3"})
            sigs = {c["baseline_signature_id"] for c in manifest["cases"]}
            self.assertEqual(len(sigs), 2)
            with open(os.path.join(out_dir, "manifest.json"), encoding="utf-8") as h:
                saved = json.load(h)
            self.assertEqual(len(saved["cases"]), 2)


class TestCollectFuzzerCrashesRegressions(unittest.TestCase):
    def _write_exec(self, directory, name, body):
        path = os.path.join(directory, name)
        with open(path, "w", encoding="utf-8") as handle:
            handle.write("#!/bin/sh\n" + body)
        os.chmod(path, 0o755)
        return path

    def _import_cfc(self):
        try:
            from harness import collect_fuzzer_crashes as cfc
        except ImportError:
            import collect_fuzzer_crashes as cfc
        return cfc

    def test_sanitize_sqlancer_log_java17_frames_and_clickhouse_no_semicolon(self):
        cfc = self._import_cfc()
        raw = (
            b"-- Database: database42\n"
            b"-- seed value: 12345\n"
            b"DROP DATABASE IF EXISTS database42\n"
            b"CREATE DATABASE IF NOT EXISTS database42\n"
            b"USE database42\n"
            b"\\c otherdb\n"
            b"SELECT 1;\n"
            b"--\tat java.base/java.lang.Thread.run(Thread.java:833)\n"
            b"--\tat app//sqlancer.Main$Executor.run(Main.java:42)\n"
            b"--\tSuppressed: java.lang.IllegalStateException: nested\n"
            b'Exception in thread "main" java.lang.AssertionError: boom\n'
            b"...\t3 more\n".replace(b"...\t", b"... ")
            + b"SELECT 2;\n"
        )
        cleaned, meta = cfc.sanitize_sqlancer_log(raw)
        txt = cleaned.decode("utf-8")
        self.assertEqual(meta.get("database"), "database42")
        self.assertEqual(meta.get("seed_value"), "12345")
        self.assertIn("SELECT 1;", txt)
        self.assertIn("SELECT 2;", txt)
        self.assertNotIn("DROP DATABASE", txt)
        self.assertNotIn("CREATE DATABASE", txt)
        self.assertNotIn("USE database42", txt)
        self.assertNotIn("\\c otherdb", txt)
        self.assertNotIn("java.base/", txt)
        self.assertNotIn("app//", txt)
        self.assertNotIn("Suppressed:", txt)
        self.assertNotIn("Exception in thread", txt)
        self.assertNotIn("3 more", txt)

    def test_collect_crashes_unverified_sets_qualified_none(self):
        cfc = self._import_cfc()
        with tempfile.TemporaryDirectory() as td:
            in_dir = os.path.join(td, "crashes")
            out_dir = os.path.join(td, "out")
            os.makedirs(in_dir, exist_ok=True)
            with open(os.path.join(in_dir, "database0.log"), "w", encoding="utf-8") as h:
                h.write("SELECT 1;\n")
            manifest, manifest_path = cfc.collect_crashes(
                in_dir,
                out_dir,
                source="sqlancer",
                dbms="sqlite",
                verify=False,
            )
            self.assertEqual(manifest_path, os.path.join(out_dir, "manifest.json"))
            self.assertTrue(os.path.exists(manifest_path))
            self.assertEqual(len(manifest["cases"]), 1)
            self.assertIsNone(manifest["cases"][0]["qualified"])

    def test_run_replay_verification_inconsistent_sigs_and_stop_event(self):
        import threading

        cfc = self._import_cfc()
        with tempfile.TemporaryDirectory() as td:
            cnt_file = os.path.join(td, "cnt.txt")
            fake_sqlite = os.path.join(td, "fake_sqlite3")
            with open(fake_sqlite, "w", encoding="utf-8") as h:
                h.write(
                    "#!/bin/sh\n"
                    f"n=0\n[ -f '{cnt_file}' ] && n=$(cat '{cnt_file}')\n"
                    f"n=$((n + 1))\nprintf '%s' \"$n\" > '{cnt_file}'\n"
                    "if [ \"$n\" -eq 1 ]; then\n"
                    "  echo \"sqlite3: sqlite3.c:100: bugOne: Assertion \\`x != 0' failed.\" >&2\n"
                    "else\n"
                    "  echo \"sqlite3: sqlite3.c:200: bugTwo: Assertion \\`y != 0' failed.\" >&2\n"
                    "fi\n"
                    "exit 134\n"
                )
            os.chmod(fake_sqlite, 0o755)

            res = cfc.run_replay_verification(
                b"SELECT 1;\n",
                "sqlite",
                replays=3,
                engine_bin=fake_sqlite,
                require_consistent_signature=True,
            )
            self.assertFalse(res["interrupted"])
            self.assertFalse(res["consistent_signature"])
            self.assertIsNone(res["baseline_signature"])
            self.assertEqual(len(res["observed_signature_ids"]), 2)

            cnt_file_stop = os.path.join(td, "cnt_stop.txt")
            fake_sqlite_stop = os.path.join(td, "fake_sqlite3_stop")
            with open(fake_sqlite_stop, "w", encoding="utf-8") as h:
                h.write(
                    "#!/bin/sh\n"
                    f"touch '{cnt_file_stop}'\n"
                    "exit 134\n"
                )
            os.chmod(fake_sqlite_stop, 0o755)

            stop_event = threading.Event()
            stop_event.set()
            res_stopped = cfc.run_replay_verification(
                b"SELECT 1;\n",
                "sqlite",
                replays=3,
                engine_bin=fake_sqlite_stop,
                stop_event=stop_event,
            )
            self.assertTrue(res_stopped["interrupted"])
            self.assertFalse(res_stopped["all_passed"])
            self.assertEqual(res_stopped["runs_passed"], 0)
            self.assertFalse(os.path.exists(cnt_file_stop))

    def test_sanitize_sqlancer_extended_headers_and_stack_frames(self):
        raw = b"""---- Time: 2026-11-23T01:02:03
-- Database: db_custom
-- seed value: 12345
--java.lang.RuntimeException: boom
--\tat io.netty.channel.ChannelHandler.run(ChannelHandler.java:10)
--\tat jdk.internal.reflect.MethodAccessor.invoke(Native Method)
--\tat ru.yandex.clickhouse.ClickHouseConnection.exec(ClickHouseConnection.java:42)
DROP DATABASE IF EXISTS db_custom;
CREATE DATABASE db_custom;
USE db_custom;
CREATE TABLE t0(x INT);
INSERT INTO t0 VALUES (1);
"""
        cleaned, meta = sanitize_sqlancer_log(raw)
        txt = cleaned.decode("utf-8")
        self.assertEqual(meta["time"], "2026-11-23T01:02:03")
        self.assertEqual(meta["database"], "db_custom")
        self.assertEqual(meta["seed_value"], "12345")
        self.assertNotIn("io.netty", txt)
        self.assertNotIn("jdk.internal", txt)
        self.assertNotIn("ru.yandex", txt)
        self.assertNotIn("DROP DATABASE", txt)
        self.assertNotIn("CREATE DATABASE", txt)
        self.assertNotIn("USE db_custom", txt)
        self.assertIn("CREATE TABLE t0(x INT);", txt)
        self.assertIn("INSERT INTO t0 VALUES (1);", txt)

    def test_collect_crashes_null_crash_signature_and_verify_logs_overwrite(self):
        with tempfile.TemporaryDirectory() as td:
            in_dir = os.path.join(td, "corpus")
            out_dir = os.path.join(td, "out")
            os.makedirs(in_dir, exist_ok=True)
            with open(os.path.join(in_dir, "c1.sql"), "w", encoding="utf-8") as h:
                h.write("SELECT 1;\n")
            with open(os.path.join(in_dir, "c1.meta.json"), "w", encoding="utf-8") as h:
                json.dump({"target": {"dbms": "sqlite"}, "crash_signature": None}, h)

            fake_sqlite = self._write_exec(
                td,
                "fake_sqlite3",
                'echo "sqlite3: sqlite3.c:77: nullSigTest: Assertion \\`x != 0\' failed." >&2\n'
                "exit 134\n",
            )
            manifest1, _ = collect_crashes(
                in_dir,
                out_dir,
                source="raw",
                dbms="sqlite",
                verify=True,
                replays=2,
                engine_bin=fake_sqlite,
            )
            self.assertEqual(len(manifest1["cases"]), 1)
            self.assertTrue(manifest1["cases"][0]["qualified"])
            log_path = os.path.join(out_dir, "verify_logs", "c1_decisions.jsonl")
            with open(log_path, encoding="utf-8") as h:
                self.assertEqual(len(h.read().splitlines()), 2)

            manifest2, _ = collect_crashes(
                in_dir,
                out_dir,
                source="raw",
                dbms="sqlite",
                verify=True,
                replays=2,
                engine_bin=fake_sqlite,
            )
            self.assertEqual(len(manifest2["cases"]), 1)
            with open(log_path, encoding="utf-8") as h:
                self.assertEqual(len(h.read().splitlines()), 2)

    def test_no_baseline_matches_none_and_mid_subprocess_stop_and_probe_timeout_cli(self):
        import subprocess
        import threading
        import time

        cfc = self._import_cfc()
        with tempfile.TemporaryDirectory() as td:
            fast_sqlite = self._write_exec(
                td,
                "fast_sqlite3",
                'echo "sqlite3: sqlite3.c:88: baseNone: Assertion \\`z != 0\' failed." >&2\n'
                "exit 134\n",
            )
            res = cfc.run_replay_verification(
                b"SELECT 1;\n",
                "sqlite",
                replays=2,
                engine_bin=fast_sqlite,
                baseline_signature="",
            )
            self.assertTrue(res["all_passed"])
            self.assertTrue(res["consistent_signature"])
            self.assertIsNone(res["matches_baseline"])

            slow_sqlite = self._write_exec(
                td,
                "slow_sqlite3",
                "sleep 15\nexit 134\n",
            )
            stop_event = threading.Event()
            timer = threading.Timer(0.2, stop_event.set)
            timer.start()
            t0 = time.monotonic()
            try:
                res_mid = cfc.run_replay_verification(
                    b"SELECT 1;\n",
                    "sqlite",
                    replays=2,
                    engine_bin=slow_sqlite,
                    probe_timeout=15.0,
                    stop_event=stop_event,
                )
            finally:
                timer.cancel()
            elapsed = time.monotonic() - t0
            self.assertLess(elapsed, 5.0)
            self.assertTrue(res_mid["interrupted"])
            self.assertFalse(res_mid["all_passed"])
            self.assertEqual(res_mid["runs_passed"], 0)

            script = os.path.join(HARNESS_DIR, "collect_fuzzer_crashes.py")
            for bad_val in ("nan", "inf", "-1", "0"):
                cli = subprocess.run(
                    [
                        sys.executable,
                        script,
                        "--input-dir",
                        td,
                        "--out-dir",
                        os.path.join(td, "out"),
                        "--source",
                        "raw",
                        "--dbms",
                        "sqlite",
                        "--probe-timeout",
                        bad_val,
                    ],
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertEqual(cli.returncode, 2, bad_val + ": " + cli.stderr)


if __name__ == "__main__":
    unittest.main()

#!/usr/bin/env python3
"""Contract tests for Dhall-compiled harness configuration (`config/generated/harness.json`)."""

import json
import os
import subprocess
import sys
import tempfile
import unittest

HARNESS_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(HARNESS_DIR)
if HARNESS_DIR not in sys.path:
    sys.path.insert(0, HARNESS_DIR)

import batch_eval  # noqa: E402
import collect_fuzzer_crashes  # noqa: E402
import crash_oracle  # noqa: E402
import dhall_config  # noqa: E402


class TestDhallConfig(unittest.TestCase):
    def test_six_dbms_specs_and_mariadb_dialect(self):
        self.assertEqual(
            dhall_config.SUPPORTED_DBMS,
            ("postgres", "mysql", "mariadb", "clickhouse", "sqlite", "duckdb"),
        )
        self.assertEqual(collect_fuzzer_crashes.DBMS_TO_OCCAM_DIALECT["mariadb"], "mariadb")
        self.assertEqual(collect_fuzzer_crashes.DBMS_TO_OCCAM_DIALECT["postgres"], "postgresql")
        self.assertEqual(crash_oracle.SUPPORTED_DBMS, dhall_config.SUPPORTED_DBMS)
        self.assertEqual(crash_oracle.SERVER_DBMS, dhall_config.SERVER_DBMS)

    def test_builtin_profiles_match_dhall_json_presets(self):
        self.assertIn("ours-s", batch_eval.BUILTIN_PROFILES)
        self.assertIn("ours-full", batch_eval.BUILTIN_PROFILES)
        for key, (_name, flags) in batch_eval.BUILTIN_PROFILES.items():
            cfg_path = os.path.join(REPO_ROOT, "config", "generated", "profiles", f"{key}.json")
            self.assertTrue(os.path.isfile(cfg_path), f"missing profile preset {cfg_path}")
            with open(cfg_path, encoding="utf-8") as handle:
                cfg = json.load(handle)
            self.assertIn("strategy", cfg)
            self.assertIn("--strategy", flags)

    def test_squirrel_yaml_rendering_for_all_supported_dbms(self):
        sq_out = dhall_config.render_squirrel_yaml(
            dbms="sqlite",
            squirrel_home="/opt/squirrel",
        )
        self.assertIn("db: sqlite", sq_out)
        self.assertIn("/opt/squirrel/data/fuzz_root/init_lib", sq_out)

        mdb_out = dhall_config.render_squirrel_yaml(
            dbms="mariadb",
            squirrel_home="/opt/squirrel",
            host="127.0.0.1",
            port=3306,
            user="root",
            password="",
            socket="/tmp/mysql.sock",
            db_name="test",
            server_cmd="mariadbd",
        )
        self.assertIn("db: mysql", mdb_out)
        self.assertIn("sock_path: '/tmp/mysql.sock'", mdb_out)

    def test_batch_eval_plan_smoke_and_stage2_thesis_dry_run(self):
        stage2 = dhall_config.load_experiment_plan("stage2-thesis")
        self.assertEqual(stage2["repeats"], 3)
        self.assertTrue(stage2["oracle"]["reject_on_slippage"])

        with tempfile.TemporaryDirectory() as td:
            sql_file = os.path.join(td, "case1.sql")
            with open(sql_file, "w", encoding="utf-8") as handle:
                handle.write("SELECT 1;\n")
            out_dir = os.path.join(td, "out")
            res = subprocess.run(
                [
                    sys.executable,
                    os.path.join(HARNESS_DIR, "batch_eval.py"),
                    "--plan",
                    "stage2-thesis",
                    "--inputs",
                    sql_file,
                    "--dbms",
                    "sqlite",
                    "--local-embedded",
                    "--skip-input-replay",
                    "--out-dir",
                    out_dir,
                    "--dry-run",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res.returncode, 0, res.stderr)
            with open(os.path.join(out_dir, "plan.json"), encoding="utf-8") as handle:
                plan = json.load(handle)
            self.assertEqual(plan["jobs_workers"], 4)
            self.assertEqual(len(plan["profiles"]), 6)
            self.assertEqual(plan["total_jobs"], 18)
            self.assertTrue(plan["reject_on_slippage"])

            out_override = os.path.join(td, "out_override")
            res2 = subprocess.run(
                [
                    sys.executable,
                    os.path.join(HARNESS_DIR, "batch_eval.py"),
                    "--plan",
                    "stage2-thesis",
                    "--profiles",
                    "Ours-S",
                    "--repeats",
                    "1",
                    "--inputs",
                    sql_file,
                    "--dbms",
                    "sqlite",
                    "--local-embedded",
                    "--skip-input-replay",
                    "--out-dir",
                    out_override,
                    "--dry-run",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(res2.returncode, 0, res2.stderr)
            with open(os.path.join(out_override, "plan.json"), encoding="utf-8") as handle:
                plan2 = json.load(handle)
            self.assertEqual(plan2["total_jobs"], 1)
            self.assertEqual(len(plan2["profiles"]), 1)


if __name__ == "__main__":
    unittest.main()

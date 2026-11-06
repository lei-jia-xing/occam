import json
import os
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile
import unittest
from contextlib import closing

from benchmarks.tools import p2_scale_study as study


class SQLiteStudyResetTests(unittest.TestCase):
    def test_template_restores_schema_data_sequence_and_pragma(self):
        with tempfile.TemporaryDirectory(prefix="occam-study-reset-") as directory:
            root = Path(directory)
            work = root / "observation"
            work.mkdir()
            template = root / "template.sqlite"
            metadata = root / "case.json"
            case = {
                "reset_mode": "template",
                "marker": "reset-case",
                "template": str(template),
                "expected_rows": [[42]],
                "setup_sql": """
CREATE TABLE t(x INT);
INSERT INTO t VALUES(42);
CREATE INDEX idx ON t(x);
CREATE TABLE audit(x INT);
CREATE TRIGGER tr AFTER INSERT ON t BEGIN INSERT INTO audit VALUES(NEW.x); END;
CREATE TABLE ids(id INTEGER PRIMARY KEY AUTOINCREMENT);
INSERT INTO ids DEFAULT VALUES;
PRAGMA user_version=42;
""",
            }
            case["state_signature"] = study.seed_database(template, case)
            metadata.write_text(json.dumps(case), encoding="utf-8")
            environment = dict(os.environ, OCCAM_WORKDIR=str(work))

            def invoke(mode, sql=""):
                return subprocess.run(
                    [sys.executable, str(study.SCRIPT), mode, str(metadata)],
                    input=sql, text=True, capture_output=True, env=environment, timeout=5,
                ).returncode

            self.assertEqual(0, invoke("reset"))
            self.assertEqual(0, invoke("oracle", "SELECT x FROM t;"))
            self.assertEqual(1, invoke("oracle", "SELECT 0 FROM t;"))
            with closing(sqlite3.connect(work / "study.sqlite", isolation_level=None)) as connection:
                connection.executescript("""
DELETE FROM t;
DROP INDEX idx;
DROP TRIGGER tr;
UPDATE sqlite_sequence SET seq=999 WHERE name='ids';
PRAGMA user_version=7;
""")
            self.assertEqual(125, invoke("oracle", "SELECT x FROM t;"))
            self.assertEqual(0, invoke("reset"))
            self.assertEqual(0, invoke("oracle", "SELECT x FROM t;"))


if __name__ == "__main__":
    unittest.main()

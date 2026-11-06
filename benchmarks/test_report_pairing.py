import json
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


@unittest.skipUnless(shutil.which("jq") and shutil.which("column"), "report dependencies unavailable")
class ReportPairingTests(unittest.TestCase):
    def test_pairing_respects_evidence_oracle_dialect_and_reduction_track(self):
        base = {"scenario": "same-input", "evidence_class": "synthetic", "oracle_kind": "rowset",
                "dialect": "sqlite", "reduction_track": "whole_script"}
        records = [
            dict(base, tool="occam"),
            dict(base, evidence_class="real", tool="ddmin"),
            dict(base, oracle_kind="different-predicate", tool="creduce"),
            dict(base, dialect="postgres", tool="postgres-only"),
            dict(base, reduction_track="fixed_setup_query", tool="query-only"),
            dict(base, scenario="paired-input", evidence_class="control", tool="control-a"),
            dict(base, scenario="paired-input", evidence_class="control", tool="control-b"),
        ]
        report = Path(__file__).with_name("report.sh")
        with tempfile.TemporaryDirectory() as tmp:
            input_path = Path(tmp) / "mixed.jsonl"
            input_path.write_text("".join(json.dumps(record) + "\n" for record in records))
            result = subprocess.run(
                ["bash", str(report), "--summary", str(input_path)],
                check=True, capture_output=True, text=True,
            )
        header = None
        paired_counts = {}
        coverage = []
        for line in result.stdout.splitlines():
            fields = line.split()
            if fields and fields[0] in {"EVIDENCE", "SCENARIO"}:
                header = fields
            elif header and len(fields) == len(header):
                row = dict(zip(header, fields))
                if "PAIRED_INPUTS" in row:
                    paired_counts[row["TOOL"]] = int(row["PAIRED_INPUTS"])
                elif row.get("SCENARIO") == "same-input":
                    coverage.append(row["TOOLS"])
        self.assertEqual(
            {"occam": 0, "ddmin": 0, "creduce": 0, "postgres-only": 0, "query-only": 0,
             "control-a": 1, "control-b": 1}, paired_counts,
        )
        self.assertEqual({"occam", "ddmin", "creduce", "postgres-only", "query-only"}, set(coverage))



if __name__ == "__main__":
    unittest.main()

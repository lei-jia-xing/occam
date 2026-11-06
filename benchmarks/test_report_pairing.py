import json
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


@unittest.skipUnless(shutil.which("jq") and shutil.which("column"), "report dependencies unavailable")
class ReportPairingTests(unittest.TestCase):
    def test_pairing_does_not_cross_evidence_or_oracle_kind(self):
        records = [
            {"scenario": "same-input", "evidence_class": "synthetic", "oracle_kind": "sqlite", "tool": "occam"},
            {"scenario": "same-input", "evidence_class": "real", "oracle_kind": "sqlite", "tool": "ddmin"},
            {"scenario": "same-input", "evidence_class": "synthetic", "oracle_kind": "mysql", "tool": "creduce"},
        ]
        report = Path(__file__).with_name("report.sh")
        with tempfile.TemporaryDirectory() as tmp:
            input_path = Path(tmp) / "mixed.jsonl"
            input_path.write_text("".join(json.dumps(record) + "\n" for record in records))
            result = subprocess.run(
                ["bash", str(report), "--summary", str(input_path)],
                check=True,
                capture_output=True,
                text=True,
            )

        paired_counts = {}
        coverage_groups = {}
        for line in result.stdout.splitlines():
            fields = line.split()
            if len(fields) >= 6 and fields[0] in {"synthetic", "real"} and fields[1] in {"sqlite", "mysql"}:
                paired_counts[tuple(fields[:3])] = int(fields[5])
            if len(fields) >= 4 and fields[0] == "same-input":
                coverage_groups[tuple(fields[:3])] = fields[3]

        self.assertEqual(
            paired_counts,
            {
                ("real", "sqlite", "ddmin"): 0,
                ("synthetic", "mysql", "creduce"): 0,
                ("synthetic", "sqlite", "occam"): 0,
            },
        )
        self.assertEqual(
            coverage_groups,
            {
                ("same-input", "real", "sqlite"): "ddmin",
                ("same-input", "synthetic", "mysql"): "creduce",
                ("same-input", "synthetic", "sqlite"): "occam",
            },
        )



if __name__ == "__main__":
    unittest.main()

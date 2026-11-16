import contextlib
import io
import json
from pathlib import Path
import tempfile
import unittest

from benchmarks.tools import compare_reducers


class ExternalComparisonReportTests(unittest.TestCase):
    def cohort(self, directory, selected=None):
        case = {
            "id": "stateful-input",
            "input_sha256": "a" * 64,
            "case_contract_sha256": "b" * 64,
            "reset_command_sha256": None,
            "reduction_track": "whole_script",
        }
        run = {
            "case_order": [case["id"]],
            "repetitions": 1,
            "budget_sec": 20,
            "oracle_timeout_sec": 1,
            "max_oracle_executions": None,
        }
        if selected is not None:
            run["run_schema_version"] = 2
            run["selected_tools"] = selected
        compare_reducers.save(directory / "run.json", run)
        compare_reducers.save(directory / "manifest.json", {"cases": [case]})
        tools = compare_reducers.normalize_tools(selected)
        return [
            {
                **case,
                "case_id": case["id"],
                "tool": tool,
                "repetition": 1,
                "budget_sec": 20,
                "oracle_timeout_sec": 1,
                "max_oracle_executions": None,
                "experiment_line": "external",
                "run_type": "score",
                "status": "completed",
                "validated_twice": True,
                "oracle_count_complete": True,
                "initial_bytes": 1000,
                "result_bytes": 50,
                "outer_wall_time_sec": 2,
                "audited_oracle_observations": 10,
            }
            for tool in tools
        ]

    def test_selected_tools_define_the_report_cohort(self):
        selected = ["occam", "sqlancer"]
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            rows = self.cohort(directory, selected)
            summary = self.render(directory, rows)
            self.assertEqual(summary["selected_tools"], selected)
            self.assertEqual(summary["records"], 2)
            self.assertEqual(set(summary["tools"]), set(selected))
            self.assertEqual(set(summary["completed_pairs"]), {"sqlancer"})
            self.assertEqual(summary["completed_pairs"]["sqlancer"]["possible"], 1)

        for mutation in ("missing", "duplicate", "unselected", "budget"):
            with self.subTest(mutation=mutation), tempfile.TemporaryDirectory() as temporary:
                directory = Path(temporary)
                rows = self.cohort(directory, selected)
                if mutation == "missing":
                    rows.pop()
                elif mutation == "duplicate":
                    rows.append(rows[-1].copy())
                elif mutation == "unselected":
                    foreign = rows[0].copy()
                    foreign["tool"] = "creduce"
                    rows.append(foreign)
                else:
                    rows[-1]["budget_sec"] = 30
                with self.assertRaises(ValueError):
                    self.render(directory, rows)

    def test_invalid_selected_tool_declarations_are_rejected(self):
        for selected in (["occam", "occam"], ["sqlancer"], ["occam", "unknown"]):
            with self.subTest(selected=selected), self.assertRaises(ValueError):
                compare_reducers.normalize_tools(selected)

    def test_legacy_v1_report_keeps_full_cohort_without_rewriting(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            rows = self.cohort(directory)
            report_path = directory / "report.md"
            summary_path = directory / "summary.json"
            report_path.write_text("immutable report")
            summary_path.write_text("immutable summary")
            summary = self.render(directory, rows)
            self.assertEqual(summary["selected_tools"], list(compare_reducers.TOOLS))
            self.assertEqual(report_path.read_text(), "immutable report")
            self.assertEqual(summary_path.read_text(), "immutable summary")


    def render(self, directory, rows):
        (directory / "results.jsonl").write_text(
            "".join(json.dumps(row) + "\n" for row in rows)
        )
        with contextlib.redirect_stdout(io.StringIO()):
            return compare_reducers.report(directory)

    def test_timeout_checkpoint_is_not_a_completed_speedup(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            rows = self.cohort(directory)
            target = next(row for row in rows if row["tool"] == "creduce")
            target.update(status="timeout", oracle_count_complete=False, result_bytes=7)
            summary = self.render(directory, rows)
            self.assertEqual(summary["completed_pairs"]["creduce"]["completed"], 0)
            self.assertIsNone(summary["completed_pairs"]["creduce"]["median_wall_ratio"])
            self.assertEqual(summary["tools"]["creduce"]["validated_checkpoints"], 1)
            self.assertEqual(summary["cases"]["stateful-input"]["creduce"], 7)

    def test_unvalidated_checkpoint_has_no_quality_claim(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            rows = self.cohort(directory)
            target = next(row for row in rows if row["tool"] == "sqlancer")
            target.update(status="validation-failed", validated_twice=False, result_bytes=0)
            summary = self.render(directory, rows)
            self.assertIsNone(summary["cases"]["stateful-input"]["sqlancer"])
            self.assertEqual(summary["completed_pairs"]["sqlancer"]["completed"], 0)

    def test_different_predicate_or_budget_cannot_be_paired(self):
        for field, value in (("case_contract_sha256", "c" * 64), ("budget_sec", 30)):
            with self.subTest(field=field), tempfile.TemporaryDirectory() as temporary:
                directory = Path(temporary)
                rows = self.cohort(directory)
                rows[-1][field] = value
                with self.assertRaisesRegex(ValueError, "mismatched"):
                    self.render(directory, rows)

    def test_missing_and_duplicate_slots_are_not_silently_dropped(self):
        for duplicate in (False, True):
            with self.subTest(duplicate=duplicate), tempfile.TemporaryDirectory() as temporary:
                directory = Path(temporary)
                rows = self.cohort(directory)
                rows = rows + [rows[0]] if duplicate else rows[:-1]
                with self.assertRaises(ValueError):
                    self.render(directory, rows)


if __name__ == "__main__":
    unittest.main()

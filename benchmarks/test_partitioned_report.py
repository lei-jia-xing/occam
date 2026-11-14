import copy
import hashlib
import io
import json
import math
import sys
import tempfile
import unittest
from pathlib import Path

from benchmarks.tools import partitioned_report as report


def canonical_contract_sha(contract):
    return hashlib.sha256(json.dumps(contract, sort_keys=True).encode("utf-8")).hexdigest()


CONTRACT_DICT = {
    "effect_scope": "candidate-local SQLite file-state control via OCCAM_WORKDIR",
    "comparison_policy": "exact ordered-row comparison against expected_rows",
    "environment": {
        "python_executable": "/usr/bin/python3",
        "python_version": "3.14.10",
        "sqlite_version": "3.54.1",
    },
    "hashes": {
        "sql_sha256": "12f182722a25864cfc7a0deeca2c8d3ff692a90c35b89d0d200a597fdbe4b3cc",
        "template_sha256": "7fcc5da99d11eadf54a507a26aee65ed37c4389ffb9995a46318a6ecf08f0e26",
        "metadata_sha256": "109ae3a5782de2b01978f82cc459ee92029f555f4c46b88fd2e6b1a9a0c44a13",
        "helper_sha256": "27bfbc67bfb3c8ca4bece4a822f7e9d77647065d627e783ef55bb8e7074880f6",
    },
    "declared_scope": "one SQLite file-state control only; not primary DBMS/external tool selection",
}
CONTRACT_SHA = canonical_contract_sha(CONTRACT_DICT)
CASE_ID = "reset-template"
INPUT_SHA = "12f182722a25864cfc7a0deeca2c8d3ff692a90c35b89d0d200a597fdbe4b3cc"
PRED_SHA = "1111111111111111111111111111111111111111111111111111111111111111"
RESET_SHA = "2222222222222222222222222222222222222222222222222222222222222222"
BINARY_SHA = "3333333333333333333333333333333333333333333333333333333333333333"
VALID_OUTPUT_SHA = "97a763885d27f6300f5f1e600dbc6051126d9a54e3a98d076d6c4f6f83a8d544"


def make_valid_metadata():
    return {
        "run_id": "test_run_12345",
        "experiment_line": "internal",
        "run_type": "score",
        "execution_mode": "native",
        "native_binary": {
            "path": "/path/to/occam-eval",
            "sha256": BINARY_SHA,
        },
        "configuration": {
            "predicate_timeout_sec": 2.0,
            "budget_sec": 30.0,
            "max_oracle_executions": 100,
            "outer_timeout_sec": 32.0,
            "profiles": ["full", "no-cache"],
            "repetitions": 2,
            "resources": {
                "serial": True,
                "jobs": 1,
                "cpu_affinity": "inherited",
                "GHCRTS": "-N1",
            },
            "validation_phases": {
                "preflight": "one physical check",
                "reduction": "uncached checks",
                "final": "two fresh checks",
            },
            "count_scope": {
                "oracle_calls": "uncached reducer observations",
                "validation_calls": "final validation observations",
            },
            "cases": [
                {
                    "case_id": CASE_ID,
                    "input_sha256": INPUT_SHA,
                    "reduction_track": "fixed_setup_query",
                    "predicate_command_sha256": PRED_SHA,
                    "reset_command_sha256": RESET_SHA,
                    "case_contract": CONTRACT_DICT,
                    "case_contract_sha256": CONTRACT_SHA,
                }
            ],
        },
    }


def make_valid_row(profile="full", repetition=1, child_id="child_01", cache=True):
    return {
        "run_id": "test_run_12345",
        "child_id": child_id,
        "profile": profile,
        "repetition": repetition,
        "case_id": CASE_ID,
        "input_sha256": INPUT_SHA,
        "experiment_line": "internal",
        "run_type": "score",
        "execution_mode": "native",
        "reduction_track": "fixed_setup_query",
        "predicate_command_sha256": PRED_SHA,
        "reset_command_sha256": RESET_SHA,
        "case_contract": CONTRACT_DICT,
        "case_contract_sha256": CONTRACT_SHA,
        "oracle_timeout_sec": 2.0,
        "budget_sec": 30.0,
        "max_oracle_executions": 100,
        "outer_timeout_sec": 32.0,
        "preflight_scope": "one physical check",
        "oracle_calls_scope": "uncached reducer observations",
        "validation_calls_scope": "final validation",
        "execution_cap_scope": "shared cap",
        "reset_calls_scope": "native reset call count uninstrumented",
        "status": "completed",
        "predicate_passed": True,
        "native_validation_passed": True,
        "native_process_returncode": 0,
        "wrapper_name": None,
        "wrapper_process_returncode": None,
        "status_reason": None,
        "output_sha256": VALID_OUTPUT_SHA,
        "result_artifact_present": True,
        "reduced_bytes": 43,
        "reduced_stmts": 1,
        "preflight_calls_debited": 1,
        "validation_calls": 2,
        "validation_observations": 2,
        "oracle_calls": 6 if cache else 26,
        "candidate_checks": 6 if cache else 26,
        "oracle_cache_hits": 20 if cache else 0,
        "total_predicate_observations": 9 if cache else 29,
        "wall_time_sec": 2.40 if cache else 5.50,
        "reduction_time_sec": 0.95,
        "predicate_time_sec": 1.20 if cache else 2.70,
        "reset_time_sec": 1.10 if cache else 2.65,
        "unattributed_wall_time_sec": 0.05,
        "terminal_observation": {
            "source": "native-profile",
            "status": "completed",
            "reason": None,
        },
        "settings": {
            "kind": "shared-reducer",
            "slicing": True,
            "closure": True,
            "intra_statement": True,
            "unrestricted": True,
            "dialect": "sqlite",
            "cache": cache,
        },
    }


class PartitionedReportConsumerTests(unittest.TestCase):
    def test_matched_full_no_cache_pairing_success(self):
        metadata = make_valid_metadata()
        row_full = make_valid_row(profile="full", repetition=1, child_id="c_full_1", cache=True)
        row_nocache = make_valid_row(profile="no-cache", repetition=1, child_id="c_nocache_1", cache=False)

        paired, summary = report.pair_internal_ablation(
            run_metadata=metadata,
            records=[row_full, row_nocache],
            base_profile="full",
            target_profile="no-cache",
        )

        self.assertTrue(summary["qualified"])
        self.assertEqual(len(paired), 1)
        self.assertEqual(summary["paired_instances_count"], 1)
        self.assertEqual(summary["completed_pairs_count"], 1)
        self.assertEqual(summary["unpaired_base_count"], 0)
        self.assertEqual(summary["unpaired_target_count"], 0)

        p = paired[0]
        self.assertEqual(p["case_id"], CASE_ID)
        self.assertEqual(p["base_child_id"], "c_full_1")
        self.assertEqual(p["target_child_id"], "c_nocache_1")
        self.assertEqual(p["status"], "completed")
        self.assertTrue(p["output_identical"])
        self.assertEqual(p["delta"]["delta_oracle_calls"], 20)
        self.assertEqual(p["delta"]["delta_oracle_cache_hits"], -20)
        self.assertEqual(p["delta"]["delta_reduced_bytes"], 0)

    def test_reject_cross_line_and_score_vs_diagnostic(self):
        # 1. Non-internal experiment line rejected by qualification
        meta_cross = make_valid_metadata()
        meta_cross["experiment_line"] = "external"
        paired, summary = report.pair_internal_ablation(
            run_metadata=meta_cross,
            records=[make_valid_row(profile="full"), make_valid_row(profile="no-cache", cache=False)],
        )
        self.assertFalse(summary["qualified"])
        self.assertEqual(len(paired), 0)

        # 2. Diagnostic run_type rejected by qualification
        meta_diag = make_valid_metadata()
        meta_diag["run_type"] = "diagnostic"
        paired, summary = report.pair_internal_ablation(
            run_metadata=meta_diag,
            records=[make_valid_row(profile="full"), make_valid_row(profile="no-cache", cache=False)],
        )
        self.assertFalse(summary["qualified"])
        self.assertEqual(len(paired), 0)

        # 3. Diagnostic record inside score run rejected as incompatible protocol
        meta_score = make_valid_metadata()
        row_diag = make_valid_row(profile="full")
        row_diag["run_type"] = "diagnostic"
        row_nocache = make_valid_row(profile="no-cache", cache=False)
        paired, summary = report.pair_internal_ablation(
            run_metadata=meta_score,
            records=[row_diag, row_nocache],
        )
        self.assertEqual(len(paired), 0)
        self.assertEqual(summary["incompatible_protocol_records"], 1)
        self.assertEqual(summary["unpaired_target_count"], 1)

    def test_reject_empty_resources_or_missing_validation_count_plan(self):
        with self.subTest("empty resources"):
            meta = make_valid_metadata()
            meta["configuration"]["resources"] = {}
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[])
            self.assertFalse(summary["qualified"])

        with self.subTest("missing validation phases"):
            meta = make_valid_metadata()
            del meta["configuration"]["validation_phases"]
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[])
            self.assertFalse(summary["qualified"])

        with self.subTest("missing count scope"):
            meta = make_valid_metadata()
            del meta["configuration"]["count_scope"]
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[])
            self.assertFalse(summary["qualified"])

    def test_reject_mismatched_sql_track_or_contract(self):
        meta = make_valid_metadata()

        with self.subTest("mismatched input_sha256"):
            row_full = make_valid_row(profile="full", child_id="c1", cache=True)
            row_nocache = make_valid_row(profile="no-cache", child_id="c2", cache=False)
            row_nocache["input_sha256"] = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            paired, summary = report.pair_internal_ablation(
                run_metadata=meta,
                records=[row_full, row_nocache],
            )
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["incompatible_protocol_records"], 1)
            self.assertEqual(summary["unpaired_base_count"], 1)

        with self.subTest("undeclared case_id"):
            row_bad_cid = make_valid_row(profile="full", child_id="c3", cache=True)
            row_bad_cid["case_id"] = "unknown-case"
            row_nocache = make_valid_row(profile="no-cache", child_id="c4", cache=False)
            paired, summary = report.pair_internal_ablation(
                run_metadata=meta,
                records=[row_bad_cid, row_nocache],
            )
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["incompatible_protocol_records"], 1)

        with self.subTest("mismatched reduction_track"):
            row_full = make_valid_row(profile="full", child_id="c5", cache=True)
            row_nocache = make_valid_row(profile="no-cache", child_id="c6", cache=False)
            row_nocache["reduction_track"] = "whole_script"
            paired, summary = report.pair_internal_ablation(
                run_metadata=meta,
                records=[row_full, row_nocache],
            )
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["incompatible_protocol_records"], 1)

        with self.subTest("mismatched predicate_command_sha256"):
            row_full = make_valid_row(profile="full", child_id="c7", cache=True)
            row_nocache = make_valid_row(profile="no-cache", child_id="c8", cache=False)
            row_nocache["predicate_command_sha256"] = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            paired, summary = report.pair_internal_ablation(
                run_metadata=meta,
                records=[row_full, row_nocache],
            )
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["incompatible_protocol_records"], 1)

        with self.subTest("mismatched reset_command_sha256"):
            row_full = make_valid_row(profile="full", child_id="c9", cache=True)
            row_nocache = make_valid_row(profile="no-cache", child_id="c10", cache=False)
            row_nocache["reset_command_sha256"] = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
            paired, summary = report.pair_internal_ablation(
                run_metadata=meta,
                records=[row_full, row_nocache],
            )
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["incompatible_protocol_records"], 1)

    def test_case_contract_validation_edges(self):
        with self.subTest("opaque string contract fails"):
            meta = make_valid_metadata()
            meta["configuration"]["cases"][0]["case_contract"] = "hello"
            meta["configuration"]["cases"][0]["case_contract_sha256"] = hashlib.sha256(b"hello").hexdigest()
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[])
            self.assertFalse(summary["qualified"])

        with self.subTest("empty dict contract fails"):
            meta = make_valid_metadata()
            meta["configuration"]["cases"][0]["case_contract"] = {}
            meta["configuration"]["cases"][0]["case_contract_sha256"] = canonical_contract_sha({})
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[])
            self.assertFalse(summary["qualified"])

        with self.subTest("missing contract in row fails"):
            meta = make_valid_metadata()
            row_full = make_valid_row(profile="full", child_id="c1", cache=True)
            row_nocache = make_valid_row(profile="no-cache", child_id="c2", cache=False)
            row_full["case_contract"] = None
            row_full["case_contract_sha256"] = None
            row_nocache["case_contract"] = None
            row_nocache["case_contract_sha256"] = None
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full, row_nocache])
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["incompatible_protocol_records"], 2)

    def test_reject_settings_with_extra_keys_or_pass_difference(self):
        meta = make_valid_metadata()

        with self.subTest("differing pass (slicing)"):
            row_full = make_valid_row(profile="full", child_id="c1", cache=True)
            row_nocache = make_valid_row(profile="no-cache", child_id="c2", cache=False)
            row_nocache["settings"]["slicing"] = False
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full, row_nocache])
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["rejected_settings_slots_count"], 1)
            self.assertEqual(len(summary["exclusions"]["rejected_settings"]), 1)

        with self.subTest("extra unexpected settings key"):
            row_full2 = make_valid_row(profile="full", child_id="c3", cache=True)
            row_nocache2 = make_valid_row(profile="no-cache", child_id="c4", cache=False)
            row_nocache2["settings"]["unrequested_extra_knob"] = True
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full2, row_nocache2])
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["rejected_settings_slots_count"], 1)

        with self.subTest("mandatory setting value is None"):
            row_full3 = make_valid_row(profile="full", child_id="c5", cache=True)
            row_nocache3 = make_valid_row(profile="no-cache", child_id="c6", cache=False)
            row_nocache3["settings"]["kind"] = None
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full3, row_nocache3])
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["rejected_settings_slots_count"], 1)

        with self.subTest("setting type mismatch (1 vs True)"):
            row_full4 = make_valid_row(profile="full", child_id="c7", cache=True)
            row_nocache4 = make_valid_row(profile="no-cache", child_id="c8", cache=False)
            row_nocache4["settings"]["slicing"] = 1
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full4, row_nocache4])
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["rejected_settings_slots_count"], 1)


        with self.subTest("mismatched dialect in settings"):
            row_full5 = make_valid_row(profile="full", child_id="c9_dial", cache=True)
            row_nocache5 = make_valid_row(profile="no-cache", child_id="c10_dial", cache=False)
            row_nocache5["settings"]["dialect"] = "postgres"
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full5, row_nocache5])
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["rejected_settings_slots_count"], 1)
            self.assertEqual(len(summary["exclusions"]["rejected_settings"]), 1)
    def test_reject_differing_side_scopes(self):
        meta = make_valid_metadata()
        row_full = make_valid_row(profile="full", child_id="c1", cache=True)
        row_nocache = make_valid_row(profile="no-cache", child_id="c2", cache=False)
        row_nocache["oracle_calls_scope"] = "different oracle calls scope"

        paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full, row_nocache])
        self.assertEqual(len(paired), 0)
        self.assertEqual(summary["rejected_scopes_slots_count"], 1)
        self.assertEqual(len(summary["exclusions"]["rejected_scopes"]), 1)

    def test_reject_nonzero_native_exit_even_if_status_completed(self):
        meta = make_valid_metadata()

        with self.subTest("nonzero returncode"):
            row_full = make_valid_row(profile="full", child_id="c1", cache=True)
            row_nocache = make_valid_row(profile="no-cache", child_id="c2", cache=False)
            row_nocache["native_process_returncode"] = 7
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full, row_nocache])
            self.assertEqual(len(paired), 1)
            self.assertEqual(summary["completed_pairs_count"], 0)
            self.assertNotEqual(paired[0]["status"], "completed")
            self.assertIsNone(paired[0]["delta"])

        with self.subTest("boolean False returncode rejected"):
            row_full2 = make_valid_row(profile="full", child_id="c3", cache=True)
            row_nocache2 = make_valid_row(profile="no-cache", child_id="c4", cache=False)
            row_nocache2["native_process_returncode"] = False
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full2, row_nocache2])
            self.assertEqual(len(paired), 1)
            self.assertEqual(summary["completed_pairs_count"], 0)
            self.assertNotEqual(paired[0]["status"], "completed")

        with self.subTest("wrapper boolean False rejected"):
            row_full3 = make_valid_row(profile="full", child_id="c5", cache=True)
            row_nocache3 = make_valid_row(profile="no-cache", child_id="c6", cache=False)
            row_nocache3["wrapper_process_returncode"] = False
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full3, row_nocache3])
            self.assertEqual(len(paired), 1)
            self.assertEqual(summary["completed_pairs_count"], 0)
            self.assertNotEqual(paired[0]["status"], "completed")

        with self.subTest("truthy string artifact_present rejected"):
            row_full4 = make_valid_row(profile="full", child_id="c7", cache=True)
            row_nocache4 = make_valid_row(profile="no-cache", child_id="c8", cache=False)
            row_nocache4["result_artifact_present"] = "true"
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full4, row_nocache4])
            self.assertEqual(len(paired), 1)
            self.assertEqual(summary["completed_pairs_count"], 0)
            self.assertNotEqual(paired[0]["status"], "completed")

    def test_exact_types_and_normalization_of_invalid_side_metrics(self):
        meta = make_valid_metadata()

        with self.subTest("string repetition rejected"):
            row_full = make_valid_row(profile="full", child_id="c1", cache=True)
            row_nocache = make_valid_row(profile="no-cache", child_id="c2", cache=False)
            row_full["repetition"] = "1"
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full, row_nocache])
            self.assertEqual(len(paired), 0)
            self.assertEqual(summary["incompatible_protocol_records"], 1)

        with self.subTest("invalid side metrics normalized to null without crash"):
            row_full2 = make_valid_row(profile="full", child_id="c3", cache=True)
            row_nocache2 = make_valid_row(profile="no-cache", child_id="c4", cache=False)
            row_nocache2["oracle_calls"] = "invalid_string"
            row_nocache2["wall_time_sec"] = float("nan")
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full2, row_nocache2])
            self.assertEqual(len(paired), 1)
            p = paired[0]
            self.assertIsNone(p["target"]["oracle_calls"])
            self.assertIsNone(p["target"]["wall_time_sec"])
            self.assertIsNone(p["delta"]["delta_oracle_calls"])
            self.assertIsNone(p["delta"]["delta_wall_time_sec"])
            # True 0 is preserved
            self.assertEqual(p["delta"]["delta_reduced_bytes"], 0)

        with self.subTest("short non-sha output hash yields output_identical None"):
            row_full3 = make_valid_row(profile="full", child_id="c5", cache=True)
            row_nocache3 = make_valid_row(profile="no-cache", child_id="c6", cache=False)
            row_full3["output_sha256"] = "short_hash"
            row_nocache3["output_sha256"] = "short_hash"
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full3, row_nocache3])
            self.assertEqual(len(paired), 1)
            self.assertIsNone(paired[0]["output_identical"])

        with self.subTest("empty output string yields output_identical None"):
            row_full4 = make_valid_row(profile="full", child_id="c7", cache=True)
            row_nocache4 = make_valid_row(profile="no-cache", child_id="c8", cache=False)
            row_full4["output_sha256"] = ""
            row_nocache4["output_sha256"] = ""
            paired, summary = report.pair_internal_ablation(run_metadata=meta, records=[row_full4, row_nocache4])
            self.assertEqual(len(paired), 1)
            self.assertIsNone(paired[0]["output_identical"])

    def test_identical_child_id_rejected(self):
        meta = make_valid_metadata()
        row_full = make_valid_row(profile="full", child_id="same_child", cache=True)
        row_nocache = make_valid_row(profile="no-cache", child_id="same_child", cache=False)

        paired, summary = report.pair_internal_ablation(
            run_metadata=meta,
            records=[row_full, row_nocache],
        )
        self.assertEqual(len(paired), 0)
        self.assertEqual(summary["rejected_child_id_slots_count"], 1)

    def test_retains_failure_and_denominators(self):
        meta = make_valid_metadata()
        row_full = make_valid_row(profile="full", child_id="c1", cache=True)
        row_nocache = make_valid_row(profile="no-cache", child_id="c2", cache=False)

        # Target failed with timeout
        row_nocache["status"] = "timeout"
        row_nocache["status_reason"] = "wall budget expired"

        paired, summary = report.pair_internal_ablation(
            run_metadata=meta,
            records=[row_full, row_nocache],
        )
        self.assertEqual(len(paired), 1)
        self.assertEqual(summary["paired_instances_count"], 1)
        self.assertEqual(summary["completed_pairs_count"], 0)
        self.assertEqual(summary["noncompleted_pairs_count"], 1)

        p = paired[0]
        self.assertEqual(p["status"], "timeout")
        self.assertEqual(p["status_reason"], "wall budget expired")
        self.assertIsNone(p["delta"])

    def test_planned_slot_denominator_enumerates_missing_slots(self):
        meta = make_valid_metadata()
        # 1 case * 2 repetitions = 2 expected slots
        paired, summary = report.pair_internal_ablation(
            run_metadata=meta,
            records=[],
        )
        self.assertEqual(len(paired), 0)
        self.assertEqual(summary["expected_slots_count"], 2)
        self.assertEqual(summary["missing_both_slots_count"], 2)
        self.assertEqual(len(summary["exclusions"]["missing_both"]), 2)

    def test_cli_invocation_on_isolated_run_directory(self):
        meta = make_valid_metadata()
        rows = [
            make_valid_row(profile="full", repetition=1, child_id="c_full_1", cache=True),
            make_valid_row(profile="full", repetition=2, child_id="c_full_2", cache=True),
            make_valid_row(profile="no-cache", repetition=1, child_id="c_nocache_1", cache=False),
            make_valid_row(profile="no-cache", repetition=2, child_id="c_nocache_2", cache=False),
        ]

        with tempfile.TemporaryDirectory(prefix="test-isolated-run-") as tmpdir:
            p = Path(tmpdir)
            (p / "run.json").write_text(json.dumps(meta) + "\n")
            (p / "results.jsonl").write_text("\n".join(json.dumps(r) for r in rows) + "\n")

            # 1. Test --json CLI output
            buf_json = io.StringIO()
            old_stdout = sys.stdout
            try:
                sys.stdout = buf_json
                rc_json = report.main([str(p), "--json"])
                self.assertEqual(rc_json, 0)
            finally:
                sys.stdout = old_stdout

            payload = json.loads(buf_json.getvalue())
            summary = payload["summary"]
            self.assertTrue(summary["qualified"])
            self.assertEqual(summary["expected_slots_count"], 2)
            self.assertEqual(summary["paired_instances_count"], 2)
            self.assertEqual(summary["completed_pairs_count"], 2)
            self.assertEqual(summary["output_identical_count"], 2)

            paired = payload["paired"]
            self.assertEqual(len(paired), 2)
            for p_rec in paired:
                self.assertEqual(p_rec["status"], "completed")
                self.assertTrue(p_rec["output_identical"])
                self.assertEqual(p_rec["delta"]["delta_oracle_calls"], 20)
                self.assertEqual(p_rec["delta"]["delta_oracle_cache_hits"], -20)
                self.assertEqual(p_rec["delta"]["delta_reduced_bytes"], 0)

            # 2. Test default Markdown CLI output
            buf_md = io.StringIO()
            try:
                sys.stdout = buf_md
                rc_md = report.main([str(p)])
                self.assertEqual(rc_md, 0)
            finally:
                sys.stdout = old_stdout

            md_text = buf_md.getvalue()
            self.assertIn("Internal Ablation Report", md_text)
            self.assertIn("reset-template", md_text)
            # Assert known 0 cache hits renders as 0, not N/A
            self.assertNotIn("N/A", md_text)

if __name__ == "__main__":
    unittest.main()

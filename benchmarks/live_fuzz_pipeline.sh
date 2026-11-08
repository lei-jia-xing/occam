#!/usr/bin/env bash
# Reduce an existing fuzz SQL input, or an explicitly synthetic stress input.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
INPUT=""
ORACLE_CMD=""
RESET_CMD=""
DIALECT="postgres"
OUT_DIR="$SCRIPT_DIR/live_fuzz_out"
TIMEOUT=30
BUDGET=""
MAX_EXECUTIONS=""
STMTS=100
CASE_ID=""

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]
  --input FILE       Existing fuzz SQL, including schema/setup statements
  --oracle CMD       Predicate command: SQL on stdin, exit 0 accepts (required with --input)
  --reset CMD        Optional per-candidate reset command
  --dialect DIALECT  postgres, mysql, sqlite, or duckdb (default: postgres)
  --timeout SECONDS  Per-candidate timeout including reset (default: 30)
  --budget SECONDS   Total reduction budget including initial/final checks
  --max-oracle-executions N  Uncached execution budget including validation
  --out-dir DIR      Parent directory for independent run artifacts
  --stmts N          Synthetic input size when --input is omitted (default: 100)
  --case-id ID       Synthetic predicate marker (default: dialect-specific)
  -h, --help         Show help
EOF
}

while [[ $# -gt 0 ]]; do
    if [[ "$1" == --help || "$1" == -h ]]; then usage; exit 0; fi
    if [[ $# -lt 2 ]]; then echo "Missing value for $1" >&2; exit 2; fi
    case "$1" in
        --input) INPUT="$2" ;;
        --oracle) ORACLE_CMD="$2" ;;
        --reset) RESET_CMD="$2" ;;
        --dialect) DIALECT="$2" ;;
        --timeout) TIMEOUT="$2" ;;
        --budget) BUDGET="$2" ;;
        --max-oracle-executions) MAX_EXECUTIONS="$2" ;;
        --out-dir) OUT_DIR="$2" ;;
        --stmts) STMTS="$2" ;;
        --case-id) CASE_ID="$2" ;;
        *) echo "Unknown option: $1" >&2; exit 2 ;;
    esac
    shift 2
done

case "$DIALECT" in
    postgres|postgresql|pg) DIALECT=postgres; DEPENDENCY_DIALECT=postgres; DEFAULT_CASE=pg_01 ;;
    mysql|mariadb) DIALECT=mysql; DEPENDENCY_DIALECT=mysql; DEFAULT_CASE=mysql_01 ;;
    sqlite|sqlite3) DIALECT=sqlite; DEPENDENCY_DIALECT=sqlite; DEFAULT_CASE=sqlite_01 ;;
    duckdb) DEPENDENCY_DIALECT=generic; DEFAULT_CASE=duckdb_01 ;;
    *) echo "Unsupported dialect: $DIALECT" >&2; exit 2 ;;
esac
if [[ -n "$INPUT" && ( ! -r "$INPUT" || -z "$ORACLE_CMD" ) ]]; then
    echo "--input requires a readable SQL file and an explicit --oracle command" >&2
    exit 2
fi

mkdir -p "$OUT_DIR"
RUN_DIR="$(mktemp -d "$OUT_DIR/run_XXXXXX")"
SQL_FILE="$RUN_DIR/input.sql"
EVIDENCE_CLASS=fuzz
ORACLE_KIND=external_predicate
if [[ -n "$INPUT" ]]; then
    cp -- "$INPUT" "$SQL_FILE"
else
    EVIDENCE_CLASS=synthetic
    CASE_ID="${CASE_ID:-$DEFAULT_CASE}"
    python3 "$SCRIPT_DIR/tools/fuzz_generator.py" --dialect "$DIALECT" \
        --stmts "$STMTS" --inject "$CASE_ID" --out "$SQL_FILE"
    if [[ -z "$ORACLE_CMD" ]]; then
        printf -v ORACLE_CMD '%q %q' "$SCRIPT_DIR/fuzz_corpus/fuzz_oracle.sh" "$CASE_ID"
        ORACLE_KIND=text_pattern
    fi
fi

if [[ -z "${OCCAM_BIN:-}" ]]; then
    if command -v cabal >/dev/null 2>&1; then
        OCCAM_BIN="$(cd "$ROOT_DIR" && cabal list-bin exe:occam)"
    elif command -v occam >/dev/null 2>&1; then
        OCCAM_BIN="$(command -v occam)"
    else
        echo "Error: Occam binary not found (install Cabal or add it to PATH)." >&2
        exit 1
    fi
fi
ARGS=("$SQL_FILE" --oracle "$ORACLE_CMD" --out "$RUN_DIR" --timeout "$TIMEOUT" --dialect "$DEPENDENCY_DIALECT")
if [[ -n "$RESET_CMD" ]]; then ARGS+=(--reset "$RESET_CMD"); fi
if [[ -n "$BUDGET" ]]; then ARGS+=(--budget "$BUDGET"); fi
if [[ -n "$MAX_EXECUTIONS" ]]; then ARGS+=(--max-oracle-executions "$MAX_EXECUTIONS"); fi
status=0
"$OCCAM_BIN" "${ARGS[@]}" || status=$?

# The CLI's final checks are uncached, use fresh candidate state, and share its budget.
python3 - "$RUN_DIR" "$EVIDENCE_CLASS" "$ORACLE_KIND" "$DIALECT" "$status" <<'PY'
import json
import math
import sys
from pathlib import Path

run_dir = Path(sys.argv[1]).resolve()
stats_path = run_dir / "stats.json"
stats = json.loads(stats_path.read_text()) if stats_path.is_file() else {}
metrics = stats.get("evaluation_metrics", {})
initial = stats.get("initial_statements")
reduced = stats.get("reduced_statements")

def seconds(name):
    value = metrics.get(name)
    return value if type(value) in (int, float) and math.isfinite(value) else None

reset_preflight = seconds("evaluationPreflightResetSeconds")
reset_reduction = seconds("evaluationResetSeconds")
reset_validation = seconds("evaluationValidationResetSeconds")
reset_total = (
    reset_preflight + reset_reduction + reset_validation
    if all(value is not None for value in (reset_preflight, reset_reduction, reset_validation))
    else None
)
record = {
    "scenario": run_dir.name,
    "tool": "occam",
    "evidence_class": sys.argv[2],
    "oracle_kind": sys.argv[3],
    "dialect": sys.argv[4],
    "status": stats.get("status", "tool-error"),
    "exit_code": int(sys.argv[5]),
    "initial_stmts": initial,
    "reduced_stmts": reduced,
    "stmt_red_pct": (1 - reduced / initial) * 100 if initial and reduced is not None else None,
    "initial_bytes": stats.get("initial_bytes"),
    "reduced_bytes": stats.get("reduced_bytes"),
    "byte_red_pct": stats.get("reduction_ratio"),
    "oracle_calls": stats.get("oracle_executions"),
    "oracle_calls_scope": "initial check, reduction, and final validation",
    "reset_preflight_sec": reset_preflight,
    "reset_reduction_sec": reset_reduction,
    "reset_validation_sec": reset_validation,
    "reset_time_sec": reset_total,
    "reset_time_scope": "preflight + reduction/audit + repeated final validation; each stage counted once",
    "oracle_timeouts": metrics.get("evaluationTimedOutObservations"),
    "wall_time_sec": stats.get("elapsed_seconds"),
    "predicate_passed": metrics.get("evaluationValidationPassed"),
    "result_file": str(run_dir / "minimal.sql"),
}
(run_dir / "result.jsonl").write_text(json.dumps(record) + "\n")
print(json.dumps(record, indent=2))
PY
exit "$status"

#!/usr/bin/env bash
# ==============================================================================
# Occam Real-World SQL Crash Corpus Benchmark Suite Runner
# ==============================================================================
# Iterates through real-world crash reproduction cases in benchmarks/corpus/real_crashes,
# executes Occam (or Occam-Eval), collects reduction metrics, and generates a
# consolidated Markdown and JSON report.
# ==============================================================================

set -eo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
CRASHES_DIR="$SCRIPT_DIR/real_crashes"
DEFAULT_RESULTS_DIR="$SCRIPT_DIR/results"

TOOL_NAME="occam"
FILTER_PATTERN="*.sql"
CUSTOM_ORACLE=""
FORCE_MOCK=0
TIMEOUT_SECS="10.0"
RESULTS_DIR="$DEFAULT_RESULTS_DIR"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Options:
  --tool <occam|occam-eval>  Reducer engine to execute (default: occam)
  --eval                     Shorthand for --tool occam-eval
  --oracle <command>         Custom oracle command
  --mock                     Force simulated crash oracle (default if DBMS offline)
  --filter <pattern>         Glob filter for cases (e.g., 'pg_*', 'mysql_*', '*json*')
  --timeout <seconds>        Per-case execution timeout (default: 10.0)
  --out <dir>                Directory for output artifacts (default: benchmarks/corpus/results)
  -h, --help                 Show this help message
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --tool)
            TOOL_NAME="$2"
            shift 2
            ;;
        --eval)
            TOOL_NAME="occam-eval"
            shift 1
            ;;
        --oracle)
            CUSTOM_ORACLE="$2"
            shift 2
            ;;
        --mock)
            FORCE_MOCK=1
            shift 1
            ;;
        --filter)
            FILTER_PATTERN="$2"
            shift 2
            ;;
        --timeout)
            TIMEOUT_SECS="$2"
            shift 2
            ;;
        --out)
            RESULTS_DIR="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            ;;
    esac
done

cd "$ROOT_DIR"
mkdir -p "$RESULTS_DIR"

# 1. Locate tool executable
TOOL_BIN=""
if [[ -x "$ROOT_DIR/dist-newstyle/build/x86_64-linux/ghc-9.10.3/occam-0.1.0.0/x/$TOOL_NAME/build/$TOOL_NAME/$TOOL_NAME" ]]; then
    TOOL_BIN="$ROOT_DIR/dist-newstyle/build/x86_64-linux/ghc-9.10.3/occam-0.1.0.0/x/$TOOL_NAME/build/$TOOL_NAME/$TOOL_NAME"
else
    # Try cabal list-bin
    if command -v cabal >/dev/null 2>&1; then
        TOOL_BIN=$(cabal list-bin "$TOOL_NAME" 2>/dev/null || true)
    fi
fi

if [[ -z "$TOOL_BIN" || ! -x "$TOOL_BIN" ]]; then
    TOOL_RUN_CMD=(cabal run "$TOOL_NAME" --)
else
    TOOL_RUN_CMD=("$TOOL_BIN")
fi

# 2. Check DBMS daemon status
PG_ACTIVE=0
MYSQL_ACTIVE=0
if command -v pg_isready >/dev/null 2>&1 && pg_isready -q 2>/dev/null; then
    PG_ACTIVE=1
fi
if command -v mysqladmin >/dev/null 2>&1 && mysqladmin ping >/dev/null 2>&1; then
    MYSQL_ACTIVE=1
fi

# 3. Discover benchmark test cases
shopt -s nullglob
raw_files=("$CRASHES_DIR"/$FILTER_PATTERN)
shopt -u nullglob

if [[ ${#raw_files[@]} -eq 0 ]]; then
    echo "Error: No benchmark SQL files found matching '$FILTER_PATTERN' in $CRASHES_DIR" >&2
    exit 1
fi

# Natural sorting
IFS=$'\n' sorted_files=($(sort <<<"${raw_files[*]}"))
unset IFS

total_cases=${#sorted_files[@]}

echo "======================================================================"
echo "Occam Real-World Crash Corpus Suite"
echo "======================================================================"
echo "  Target Tool:    $TOOL_NAME (${TOOL_RUN_CMD[*]})"
echo "  Corpus Dir:     $CRASHES_DIR"
echo "  Results Dir:    $RESULTS_DIR"
echo "  Total Cases:    $total_cases"
echo "  Timeout:        ${TIMEOUT_SECS}s"
echo "  PostgreSQL:     $([ $PG_ACTIVE -eq 1 ] && echo "Active (Live)" || echo "Inactive (Mock)")"
echo "  MySQL:          $([ $MYSQL_ACTIVE -eq 1 ] && echo "Active (Live)" || echo "Inactive (Mock)")"
echo "======================================================================"
echo ""

# Summary records storage
REPORT_JSON="$RESULTS_DIR/summary.json"
REPORT_MD="$RESULTS_DIR/summary.md"

declare -a CASE_IDS
declare -a CASE_DBMS
declare -a CASE_CLASSES
declare -a CASE_INIT_BYTES
declare -a CASE_RED_BYTES
declare -a CASE_INIT_STMTS
declare -a CASE_RED_STMTS
declare -a CASE_TIMES
declare -a CASE_STATUS

passed_count=0
failed_count=0
idx=0

for sql_file in "${sorted_files[@]}"; do
    idx=$((idx + 1))
    fname=$(basename "$sql_file")
    case_id="${fname%%.*}"
    case_out_dir="$RESULTS_DIR/$case_id"
    mkdir -p "$case_out_dir"

    # Extract metadata from header
    dbms="unknown"
    bug_class="unknown"
    if grep -q "Target DBMS: PostgreSQL" "$sql_file"; then
        dbms="postgresql"
    elif grep -q "Target DBMS: MySQL" "$sql_file"; then
        dbms="mysql"
    fi

    bug_class=$(grep -m1 "^-- Bug Class:" "$sql_file" | sed 's/^-- Bug Class:[[:space:]]*//' || echo "Unknown")

    # Select oracle
    effective_oracle=""
    if [[ -n "$CUSTOM_ORACLE" ]]; then
        effective_oracle="$CUSTOM_ORACLE"
    elif [[ $FORCE_MOCK -eq 1 ]]; then
        effective_oracle="./benchmarks/corpus/semantic_oracle.sh $case_id"
    elif [[ "$dbms" == "postgresql" && $PG_ACTIVE -eq 1 ]]; then
        effective_oracle="./harness/pg_harness.sh"
    elif [[ "$dbms" == "mysql" && $MYSQL_ACTIVE -eq 1 ]]; then
        effective_oracle="./harness/mysql_harness.sh"
    else
        effective_oracle="./benchmarks/corpus/semantic_oracle.sh $case_id"
    fi

    printf "[%2d/%2d] Running %-35s (%s)... " "$idx" "$total_cases" "$fname" "$dbms"
    start_time=$(date +%s%N 2>/dev/null || date +%s)

    status="FAILED"
    init_b=0
    red_b=0
    init_s=0
    red_s=0
    elapsed_sec="0.00"

    log_file="$case_out_dir/execution.log"

    if "${TOOL_RUN_CMD[@]}" "$sql_file" \
        --oracle "$effective_oracle" \
        --out "$case_out_dir" \
        --timeout "$TIMEOUT_SECS" > "$log_file" 2>&1; then
        status="PASSED"
        passed_count=$((passed_count + 1))
    else
        status="FAILED"
        failed_count=$((failed_count + 1))
    fi

    end_time=$(date +%s%N 2>/dev/null || date +%s)
    if [[ ${#start_time} -gt 10 && ${#end_time} -gt 10 ]]; then
        elapsed_nanos=$((end_time - start_time))
        elapsed_sec=$(awk "BEGIN {printf \"%.3f\", $elapsed_nanos / 1000000000}")
    else
        elapsed_sec=$((end_time - start_time))
    fi

    # Read stats from generated artifacts
    if [[ "$status" == "PASSED" ]]; then
        if [[ -f "$case_out_dir/stats.json" ]]; then
            # Occam stats
            init_b=$(python3 -c "import json; d=json.load(open('$case_out_dir/stats.json')); print(d.get('initial_bytes', 0))" 2>/dev/null || echo 0)
            red_b=$(python3 -c "import json; d=json.load(open('$case_out_dir/stats.json')); print(d.get('reduced_bytes', 0))" 2>/dev/null || echo 0)
            init_s=$(python3 -c "import json; d=json.load(open('$case_out_dir/stats.json')); print(d.get('initial_statements', d.get('initial_stmts', 0)))" 2>/dev/null || echo 0)
            red_s=$(python3 -c "import json; d=json.load(open('$case_out_dir/stats.json')); print(d.get('reduced_statements', d.get('reduced_stmts', 0)))" 2>/dev/null || echo 0)
        elif [[ -f "$case_out_dir/benchmark.json" ]]; then
            # Occam-eval stats (first entry is Occam)
            init_b=$(python3 -c "import json; d=json.load(open('$case_out_dir/benchmark.json')); row=d[0]; print(row.get('metricInitialBytes', row.get('initial_bytes', 0)))" 2>/dev/null || echo 0)
            red_b=$(python3 -c "import json; d=json.load(open('$case_out_dir/benchmark.json')); row=d[0]; print(row.get('metricReducedBytes', row.get('reduced_bytes', 0)))" 2>/dev/null || echo 0)
            init_s=$(python3 -c "import json; d=json.load(open('$case_out_dir/benchmark.json')); row=d[0]; print(row.get('metricInitialStmts', row.get('initial_stmts', 0)))" 2>/dev/null || echo 0)
            red_s=$(python3 -c "import json; d=json.load(open('$case_out_dir/benchmark.json')); row=d[0]; print(row.get('metricReducedStmts', row.get('reduced_stmts', 0)))" 2>/dev/null || echo 0)
        fi
        printf "%s (stmts: %d->%d, bytes: %d->%d, time: %ss)\n" "$status" "$init_s" "$red_s" "$init_b" "$red_b" "$elapsed_sec"
    else
        printf "%s (failed, check %s)\n" "$status" "$log_file"
    fi

    CASE_IDS+=("$case_id")
    CASE_DBMS+=("$dbms")
    CASE_CLASSES+=("$bug_class")
    CASE_INIT_BYTES+=("$init_b")
    CASE_RED_BYTES+=("$red_b")
    CASE_INIT_STMTS+=("$init_s")
    CASE_RED_STMTS+=("$red_s")
    CASE_TIMES+=("$elapsed_sec")
    CASE_STATUS+=("$status")
done

echo ""
echo "======================================================================"
echo "Execution Summary (${passed_count}/${total_cases} Passed)"
echo "======================================================================"

# Generate Markdown and JSON consolidated reports
python3 -c "
import json

case_ids = '''${CASE_IDS[*]}'''.split()
case_dbms = '''${CASE_DBMS[*]}'''.split()
init_bytes = [int(x) for x in '''${CASE_INIT_BYTES[*]}'''.split()]
red_bytes = [int(x) for x in '''${CASE_RED_BYTES[*]}'''.split()]
init_stmts = [int(x) for x in '''${CASE_INIT_STMTS[*]}'''.split()]
red_stmts = [int(x) for x in '''${CASE_RED_STMTS[*]}'''.split()]
times = [float(x) for x in '''${CASE_TIMES[*]}'''.split()]
statuses = '''${CASE_STATUS[*]}'''.split()

classes = []
" || true

python3 - <<PYEOF
import json, re

report_data = []
passed = 0
failed = 0
total_init_b = 0
total_red_b = 0
total_init_s = 0
total_red_s = 0

case_ids = """${CASE_IDS[*]}""".split()
case_dbms = """${CASE_DBMS[*]}""".split()
init_bytes = [int(x) for x in """${CASE_INIT_BYTES[*]}""".split()]
red_bytes = [int(x) for x in """${CASE_RED_BYTES[*]}""".split()]
init_stmts = [int(x) for x in """${CASE_INIT_STMTS[*]}""".split()]
red_stmts = [int(x) for x in """${CASE_RED_STMTS[*]}""".split()]
times = [float(x) for x in """${CASE_TIMES[*]}""".split()]
statuses = """${CASE_STATUS[*]}""".split()

for i, cid in enumerate(case_ids):
    ib = init_bytes[i]
    rb = red_bytes[i]
    ins = init_stmts[i]
    rs = red_stmts[i]
    st = statuses[i]
    tm = times[i]
    db = case_dbms[i]

    b_pct = round((1.0 - rb / ib) * 100.0, 1) if ib > 0 else 0.0
    s_pct = round((1.0 - rs / ins) * 100.0, 1) if ins > 0 else 0.0

    if st == "PASSED":
        passed += 1
        total_init_b += ib
        total_red_b += rb
        total_init_s += ins
        total_red_s += rs
    else:
        failed += 1

    report_data.append({
        "id": cid,
        "dbms": db,
        "status": st,
        "initial_bytes": ib,
        "reduced_bytes": rb,
        "byte_reduction_pct": b_pct,
        "initial_stmts": ins,
        "reduced_stmts": rs,
        "stmt_reduction_pct": s_pct,
        "time_sec": tm
    })

avg_byte_red = round((1.0 - total_red_b / total_init_b) * 100.0, 1) if total_init_b > 0 else 0.0
avg_stmt_red = round((1.0 - total_red_s / total_init_s) * 100.0, 1) if total_init_s > 0 else 0.0

summary_json = {
    "total_cases": len(case_ids),
    "passed": passed,
    "failed": failed,
    "overall_byte_reduction_pct": avg_byte_red,
    "overall_stmt_reduction_pct": avg_stmt_red,
    "results": report_data
}

with open("$REPORT_JSON", "w", encoding="utf-8") as fp:
    json.dump(summary_json, fp, indent=2)

md_lines = [
    "# Occam Real-World Crash Corpus Execution Summary",
    "",
    f"- **Tool Executed:** \`$TOOL_NAME\`",
    f"- **Total Scenarios:** {len(case_ids)}",
    f"- **Passed:** {passed}",
    f"- **Failed:** {failed}",
    f"- **Overall Byte Reduction:** {avg_byte_red}%",
    f"- **Overall Statement Reduction:** {avg_stmt_red}%",
    "",
    "## Per-Scenario Reduction Results",
    "",
    "| Scenario ID | DBMS | Status | Stmts (Init -> Red) | Stmt Red % | Bytes (Init -> Red) | Byte Red % | Time (s) |",
    "| :--- | :---: | :---: | :---: | ---: | :---: | ---: | ---: |"
]

for r in report_data:
    md_lines.append(
        f"| \`{r['id']}\` | {r['dbms']} | **{r['status']}** | {r['initial_stmts']} -> {r['reduced_stmts']} | {r['stmt_reduction_pct']}% | {r['initial_bytes']} -> {r['reduced_bytes']} | {r['byte_reduction_pct']}% | {r['time_sec']}s |"
    )

md_lines.append("")
with open("$REPORT_MD", "w", encoding="utf-8") as fp:
    fp.write("\n".join(md_lines))

print(f"Consolidated reports written to:")
print(f"  Markdown: $REPORT_MD")
print(f"  JSON:     $REPORT_JSON")
PYEOF

echo "======================================================================"
if [[ $failed_count -gt 0 ]]; then
    echo "FAILED: $failed_count / $total_cases cases failed." >&2
    exit 1
else
    echo "SUCCESS: All $total_cases cases successfully executed and reduced!"
    exit 0
fi

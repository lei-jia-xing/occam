#!/usr/bin/env bash
# ==============================================================================
# Occam Fuzz Benchmark Presentation Tool (Unix native)
# ==============================================================================
# Formats and aggregates benchmark results from fuzz_benchmark_results.jsonl
# or stdin using standard Unix tools (jq, awk, column).
# ==============================================================================

set -eo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEFAULT_JSONL="$SCRIPT_DIR/fuzz_benchmark_results.jsonl"

MODE="table"
INPUT_FILE=""

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS] [FILE.jsonl]

Modes:
  --table, -t     Format comparative runs into aligned column table via column -t (default)
  --summary, -s   Aggregate metrics per algorithm (initial/reduced stmts, %, calls, time)
  --md, -m        Output GitHub-flavored Markdown tables on demand
  --help, -h      Show this help message

Input:
  Accepts a JSONL file as argument or reads from standard input (pipeline).
  If no argument is given and stdin is empty, defaults to:
  $DEFAULT_JSONL

Examples:
  ./benchmarks/report.sh
  ./benchmarks/report.sh --summary
  ./benchmarks/report.sh --md > benchmarks/summary.md
  cat benchmarks/fuzz_benchmark_results.jsonl | ./benchmarks/report.sh --summary
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --table|-t)
            MODE="table"
            shift 1
            ;;
        --summary|-s)
            MODE="summary"
            shift 1
            ;;
        --md|-m|--markdown)
            MODE="md"
            shift 1
            ;;
        --help|-h)
            usage
            ;;
        -*)
            echo "Error: Unknown option $1" >&2
            usage
            ;;
        *)
            if [[ -z "$INPUT_FILE" ]]; then
                INPUT_FILE="$1"
                shift 1
            else
                echo "Error: Unexpected argument $1" >&2
                usage
            fi
            ;;
    esac
done

# Resolve input stream
RAW_DATA=""
if [[ -n "$INPUT_FILE" ]]; then
    if [[ ! -r "$INPUT_FILE" ]]; then
        echo "Error: Cannot read input file '$INPUT_FILE'" >&2
        exit 1
    fi
    RAW_DATA=$(cat "$INPUT_FILE")
elif [[ -t 0 ]]; then
    if [[ -f "$DEFAULT_JSONL" ]]; then
        RAW_DATA=$(cat "$DEFAULT_JSONL")
    else
        echo "Error: Default benchmark results file '$DEFAULT_JSONL' does not exist." >&2
        echo "Run benchmarks first via ./benchmarks/run_all.sh or provide a JSONL file." >&2
        exit 1
    fi
else
    # Piped or redirected input; fallback to default file if stdin is empty
    if RAW_DATA=$(cat) && [[ -n "$RAW_DATA" ]]; then
        :
    elif [[ -f "$DEFAULT_JSONL" ]]; then
        RAW_DATA=$(cat "$DEFAULT_JSONL")
    fi
fi

if [[ -z "$RAW_DATA" ]]; then
    echo "Error: Input JSONL stream is empty and default '$DEFAULT_JSONL' does not exist." >&2
    exit 1
fi

render_table() {
    {
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "SCENARIO" "TOOL" "SCALE" "INIT_S" "RED_S" "STMT_RED%" "INIT_B" "RED_B" "BYTE_RED%" "CALLS" "TIME(s)" "VALID"
        printf "%s" "$RAW_DATA" | jq -r '
            [
                .scenario,
                (.tool // .algorithm // "Unknown"),
                (.scale // "N/A"),
                ((.initial_stmts // 0) | tostring),
                ((.reduced_stmts // 0) | tostring),
                ((((.stmt_red_pct // .stmt_reduction_pct // 0) * 10 | round / 10 | tostring) + "%")),
                ((.initial_bytes // 0) | tostring),
                ((.reduced_bytes // 0) | tostring),
                ((((.byte_red_pct // .byte_reduction_pct // 0) * 10 | round / 10 | tostring) + "%")),
                ((.oracle_calls // 0) | tostring),
                ((((.wall_time_sec // .time_seconds // 0) * 100 | round / 100 | tostring) + "s")),
                (if .valid_reproducer == true then "PASS" elif .valid_reproducer == false then "FAIL" else "N/A" end)
            ] | @tsv
        '
    } | column -t -s $'\t'
}

render_summary() {
    {
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "TOOL" "CASES" "INIT_STMTS" "RED_STMTS" "STMT_RED%" "TOTAL_CALLS" "WALL_TIME(s)" "AVG_CALLS/CASE" "VALID_REPROD"
        printf "%s" "$RAW_DATA" | jq -s -r '
            group_by(.tool // .algorithm)
            | map({
                tool: (.[0].tool // .[0].algorithm),
                cases: length,
                initial_stmts: (map(.initial_stmts // 0) | add),
                reduced_stmts: (map(.reduced_stmts // 0) | add),
                reduction_pct: (
                    (map(.initial_stmts // 0) | add) as $init
                    | if $init > 0 then ((1 - ((map(.reduced_stmts // 0) | add) / $init)) * 100) else 0 end
                ),
                oracle_calls: (map(.oracle_calls // 0) | add),
                wall_time: (map(.wall_time_sec // .time_seconds // 0) | add),
                avg_calls: (if length > 0 then ((map(.oracle_calls // 0) | add) / length) else 0 end),
                valid_cases: (map(if .valid_reproducer == true then 1 else 0 end) | add)
              })
            | sort_by(
                if ((.tool) | ascii_downcase) == "occam" then 0
                elif ((.tool) | ascii_downcase) | contains("creduce") then 1
                elif ((.tool) | ascii_downcase) | contains("sqlancer") then 2
                elif ((.tool) | ascii_downcase) | contains("ddmin") then 3
                elif ((.tool) | ascii_downcase) | startswith("b1") then 4
                else 5 end
              )
            | .[]
            | [
                .tool,
                (.cases | tostring),
                (.initial_stmts | tostring),
                (.reduced_stmts | tostring),
                ((.reduction_pct * 10 | round / 10 | tostring) + "%"),
                (.oracle_calls | tostring),
                ((.wall_time * 100 | round / 100 | tostring) + "s"),
                (.avg_calls * 10 | round / 10 | tostring),
                ((.valid_cases | tostring) + "/" + (.cases | tostring))
              ] | @tsv
        '
    } | column -t -s $'\t'
}

render_md() {
    cat <<'EOF'
# Multi-Tool Fuzz Crash Reduction Benchmark Evaluation

## Detailed Comparative Results Table

| Scenario | Tool | Scale | Initial Stmts | Reduced Stmts | Stmt Red % | Initial Bytes | Reduced Bytes | Byte Red % | Oracle Calls | Time (s) | Reproducer Valid |
| :--- | :--- | :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | :---: |
EOF
    printf "%s" "$RAW_DATA" | jq -r '
        [
            ("`" + .scenario + "`"),
            ("**" + (.tool // .algorithm // "Unknown") + "**"),
            (.scale // "N/A"),
            ((.initial_stmts // 0) | tostring),
            ((.reduced_stmts // 0) | tostring),
            ((((.stmt_red_pct // .stmt_reduction_pct // 0) * 10 | round / 10 | tostring) + "%")),
            ((.initial_bytes // 0) | tostring),
            ((.reduced_bytes // 0) | tostring),
            ((((.byte_red_pct // .byte_reduction_pct // 0) * 10 | round / 10 | tostring) + "%")),
            ((.oracle_calls // 0) | tostring),
            ((((.wall_time_sec // .time_seconds // 0) * 100 | round / 100 | tostring) + "s")),
            (if .valid_reproducer == true then "PASS" elif .valid_reproducer == false then "FAIL" else "N/A" end)
        ] | "| " + join(" | ") + " |"
    '

    cat <<'EOF'

---

## Overall Aggregate Performance Comparison

| Tool | Scenarios | Total Initial Stmts | Total Reduced Stmts | Overall Stmt Reduction % | Total Oracle Calls | Total Wall Time (s) | Avg Calls / Case | Valid / Total |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | :---: |
EOF

    printf "%s" "$RAW_DATA" | jq -s -r '
        group_by(.tool // .algorithm)
        | map({
            tool: (.[0].tool // .[0].algorithm),
            cases: length,
            initial_stmts: (map(.initial_stmts // 0) | add),
            reduced_stmts: (map(.reduced_stmts // 0) | add),
            reduction_pct: (
                (map(.initial_stmts // 0) | add) as $init
                | if $init > 0 then ((1 - ((map(.reduced_stmts // 0) | add) / $init)) * 100) else 0 end
            ),
            oracle_calls: (map(.oracle_calls // 0) | add),
            wall_time: (map(.wall_time_sec // .time_seconds // 0) | add),
            avg_calls: (if length > 0 then ((map(.oracle_calls // 0) | add) / length) else 0 end),
            valid_cases: (map(if .valid_reproducer == true then 1 else 0 end) | add)
          })
        | sort_by(
            if ((.tool) | ascii_downcase) == "occam" then 0
            elif ((.tool) | ascii_downcase) | contains("creduce") then 1
            elif ((.tool) | ascii_downcase) | contains("sqlancer") then 2
            elif ((.tool) | ascii_downcase) | contains("ddmin") then 3
            elif ((.tool) | ascii_downcase) | startswith("b1") then 4
            else 5 end
          )
        | .[]
        | [
            ("**" + .tool + "**"),
            (.cases | tostring),
            (.initial_stmts | tostring),
            (.reduced_stmts | tostring),
            ("**" + (.reduction_pct * 10 | round / 10 | tostring) + "%**"),
            ("**" + (.oracle_calls | tostring) + "**"),
            ((.wall_time * 100 | round / 100 | tostring) + "s"),
            (.avg_calls * 10 | round / 10 | tostring),
            ((.valid_cases | tostring) + "/" + (.cases | tostring))
          ] | "| " + join(" | ") + " |"
    '
}

case "$MODE" in
    table)
        render_table
        ;;
    summary)
        render_summary
        ;;
    md)
        render_md
        ;;
    *)
        echo "Error: Unknown mode '$MODE'" >&2
        exit 1
        ;;
esac

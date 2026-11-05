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
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "SCENARIO" "DIALECT" "SCALE" "INIT_S" "ALGORITHM" "RED_S" "STMT_RED%" "INIT_B" "RED_B" "BYTE_RED%" "CALLS" "TIME(s)"
        printf "%s" "$RAW_DATA" | jq -r '
            [
                .scenario,
                .dialect,
                .scale,
                (.initial_stmts | tostring),
                .algorithm,
                (.reduced_stmts | tostring),
                ((.stmt_reduction_pct | tostring) + "%"),
                (.initial_bytes | tostring),
                (.reduced_bytes | tostring),
                ((.byte_reduction_pct | tostring) + "%"),
                (.oracle_calls | tostring),
                (if .time_seconds then (.time_seconds | tostring) + "s" else "0s" end)
            ] | @tsv
        '
    } | column -t -s $'\t'
}

render_summary() {
    {
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "ALGORITHM" "CASES" "INIT_STMTS" "RED_STMTS" "STMT_RED%" "TOTAL_CALLS" "WALL_TIME(s)" "AVG_CALLS/CASE"
        printf "%s" "$RAW_DATA" | jq -s -r '
            (map({scenario, initial_stmts}) | unique_by(.scenario) | {count: length, init_stmts: (map(.initial_stmts) | add)}) as $meta
            | group_by(.algorithm)
            | map({
                algorithm: .[0].algorithm,
                cases: $meta.count,
                initial_stmts: $meta.init_stmts,
                reduced_stmts: (map(.reduced_stmts) | add),
                reduction_pct: (if $meta.init_stmts > 0 then ((1 - ((map(.reduced_stmts) | add) / $meta.init_stmts)) * 100) else 0 end),
                oracle_calls: (map(.oracle_calls) | add),
                wall_time: (map(.time_seconds) | add),
                avg_calls: (if $meta.count > 0 then ((map(.oracle_calls) | add) / $meta.count) else 0 end)
              })
            | sort_by(if .algorithm == "Occam" then 0 elif .algorithm | startswith("B1") then 1 else 2 end)
            | .[]
            | [
                .algorithm,
                (.cases | tostring),
                (.initial_stmts | tostring),
                (.reduced_stmts | tostring),
                ((.reduction_pct * 10 | round / 10 | tostring) + "%"),
                (.oracle_calls | tostring),
                ((.wall_time * 100 | round / 100 | tostring) + "s"),
                (.avg_calls * 10 | round / 10 | tostring)
              ] | @tsv
        '
    } | column -t -s $'\t'
}

render_md() {
    cat <<'EOF'
# Occam vs Baselines: Fuzz Benchmark Evaluation

## Detailed Comparative Results Table

| Scenario | Dialect | Scale | Initial Stmts | Algorithm | Reduced Stmts | Stmt Red % | Initial Bytes | Reduced Bytes | Byte Red % | Oracle Calls | Time (s) |
| :--- | :--- | :--- | ---: | :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
EOF
    printf "%s" "$RAW_DATA" | jq -r '
        [
            ("`" + .scenario + "`"),
            .dialect,
            .scale,
            (.initial_stmts | tostring),
            ("**" + .algorithm + "**"),
            (.reduced_stmts | tostring),
            ((.stmt_reduction_pct | tostring) + "%"),
            (.initial_bytes | tostring),
            (.reduced_bytes | tostring),
            ((.byte_reduction_pct | tostring) + "%"),
            (.oracle_calls | tostring),
            (.time_seconds | tostring)
        ] | "| " + join(" | ") + " |"
    '

    cat <<'EOF'

---

## Overall Aggregate Performance Comparison

| Algorithm | Scenarios | Total Initial Stmts | Total Reduced Stmts | Overall Stmt Reduction % | Total Oracle Calls | Total Wall Time (s) | Avg Calls / Case |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
EOF

    printf "%s" "$RAW_DATA" | jq -s -r '
        (map({scenario, initial_stmts}) | unique_by(.scenario) | {count: length, init_stmts: (map(.initial_stmts) | add)}) as $meta
        | group_by(.algorithm)
        | map({
            algorithm: .[0].algorithm,
            cases: $meta.count,
            initial_stmts: $meta.init_stmts,
            reduced_stmts: (map(.reduced_stmts) | add),
            reduction_pct: (if $meta.init_stmts > 0 then ((1 - ((map(.reduced_stmts) | add) / $meta.init_stmts)) * 100) else 0 end),
            oracle_calls: (map(.oracle_calls) | add),
            wall_time: (map(.time_seconds) | add),
            avg_calls: (if $meta.count > 0 then ((map(.oracle_calls) | add) / $meta.count) else 0 end)
          })
        | sort_by(if .algorithm == "Occam" then 0 elif .algorithm | startswith("B1") then 1 else 2 end)
        | .[]
        | [
            ("**" + .algorithm + "**"),
            (.cases | tostring),
            (.initial_stmts | tostring),
            (.reduced_stmts | tostring),
            ("**" + (.reduction_pct * 10 | round / 10 | tostring) + "%**"),
            ("**" + (.oracle_calls | tostring) + "**"),
            ((.wall_time * 100 | round / 100 | tostring) + "s"),
            (.avg_calls * 10 | round / 10 | tostring)
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

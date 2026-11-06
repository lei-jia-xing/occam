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
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "SCENARIO" "EVIDENCE" "TOOL" "STATUS" "SCALE" "INIT_S" "RED_S" \
            "STMT_RED%" "INIT_B" "RED_B" "BYTE_RED%" "CALLS" "ORACLE_TO" "TIME(s)" "PREDICATE"
        printf "%s" "$RAW_DATA" | jq -r '
            [
                (.scenario // "N/A"),
                (.evidence_class // "unknown"),
                (.tool // .algorithm // "Unknown"),
                (.status // "unknown"),
                (.scale // "N/A"),
                (if .initial_stmts == null then "N/A" else (.initial_stmts | tostring) end),
                (if .reduced_stmts == null then "N/A" else (.reduced_stmts | tostring) end),
                (if .stmt_red_pct == null and .stmt_reduction_pct == null then "N/A"
                 else (((.stmt_red_pct // .stmt_reduction_pct) * 10 | round / 10 | tostring) + "%") end),
                (if .initial_bytes == null then "N/A" else (.initial_bytes | tostring) end),
                (if .reduced_bytes == null then "N/A" else (.reduced_bytes | tostring) end),
                (if .byte_red_pct == null and .byte_reduction_pct == null then "N/A"
                 else (((.byte_red_pct // .byte_reduction_pct) * 10 | round / 10 | tostring) + "%") end),
                (if .oracle_calls == null then "N/A" else (.oracle_calls | tostring) end),
                (if .oracle_timeouts == null then "N/A" else (.oracle_timeouts | tostring) end),
                (if .wall_time_sec == null and .time_seconds == null then "N/A"
                 else (((.wall_time_sec // .time_seconds) * 100 | round / 100 | tostring) + "s") end),
                (if .predicate_passed == true then "PASS" elif .predicate_passed == false then "FAIL" else "N/A" end)
            ] | @tsv
        '
    } | column -t -s $'\t'
}

render_summary() {
    {
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "TOOL" "CASES" "INPUTS" "PAIRED_INPUTS" "UNKNOWN_SIZES" "INIT_STMTS" "RED_STMTS" \
            "MEDIAN_STMT_RED%" "CALLS(KNOWN/UNKNOWN)" "ORACLE_TO(KNOWN/UNKNOWN)" \
            "TIMEOUT_CASES" "STATUS_DISTRIBUTION" "WALL(s)/UNKNOWN" "PREDICATE(T/F/U)"
        printf "%s" "$RAW_DATA" | jq -s -r '
            . as $all
            | group_by(.tool // .algorithm)
            | map(
                . as $rows
                | ($rows[0].tool // $rows[0].algorithm) as $tool
                | ($rows | map(select(.initial_stmts != null) | .initial_stmts) | add // 0) as $init
                | ($rows | map(select(.reduced_stmts != null) | .reduced_stmts) | add // 0) as $reduced
                | ($rows | map(.stmt_red_pct // .stmt_reduction_pct)
                          | map(select(. != null)) | sort) as $pct
                | ($all | group_by(.scenario)
                    | map(select((map(.tool // .algorithm) | unique | length) > 1)
                          | select(any(.[]; (.tool // .algorithm) == $tool)))
                    | length) as $paired
                | {
                    tool: $tool,
                    cases: ($rows | length),
                    inputs: ($rows | map(.scenario) | unique | length),
                    paired_inputs: $paired,
                    missing_sizes: ($rows | map(select(.initial_stmts == null or .reduced_stmts == null)) | length),
                    initial_stmts: $init,
                    reduced_stmts: $reduced,
                    median_pct: (if ($pct | length) == 0 then null
                                 elif (($pct | length) % 2) == 1
                                 then $pct[((($pct | length) - 1) / 2 | floor)]
                                 else (($pct[(($pct | length) / 2 | floor) - 1]
                                        + $pct[(($pct | length) / 2 | floor)]) / 2) end),
                    calls: ($rows | map(select(.oracle_calls != null) | .oracle_calls) | add // 0),
                    unknown_calls: ($rows | map(select(.oracle_calls == null)) | length),
                    oracle_timeouts: ($rows | map(select(.oracle_timeouts != null) | .oracle_timeouts) | add // 0),
                    unknown_oracle_timeouts: ($rows | map(select(.oracle_timeouts == null)) | length),
                    timeouts: ($rows | map(select(.status == "timeout")) | length),
                    statuses: ($rows | group_by(.status // "unknown")
                               | map((.[0].status // "unknown") + ":" + (length | tostring))
                               | join(",")),
                    wall: ($rows | map(select(.wall_time_sec != null or .time_seconds != null)
                                       | (.wall_time_sec // .time_seconds)) | add // 0),
                    unknown_wall: ($rows | map(select(.wall_time_sec == null and .time_seconds == null)) | length),
                    valid_true: ($rows | map(select(.predicate_passed == true)) | length),
                    valid_false: ($rows | map(select(.predicate_passed == false)) | length),
                    valid_unknown: ($rows | map(select(.predicate_passed == null)) | length)
                  })
            | sort_by(
                if ((.tool) | ascii_downcase) == "occam" then 0
                elif ((.tool) | ascii_downcase) | contains("creduce") then 1
                elif ((.tool) | ascii_downcase) | contains("sqlancer") then 2
                elif ((.tool) | ascii_downcase) | contains("ddmin") then 3
                else 4 end
              )
            | .[]
            | [
                .tool, (.cases | tostring), (.inputs | tostring), (.paired_inputs | tostring),
                (.missing_sizes | tostring),
                (if .missing_sizes > 0 then "N/A" else (.initial_stmts | tostring) end),
                (if .missing_sizes > 0 then "N/A" else (.reduced_stmts | tostring) end),
                (if .median_pct == null then "N/A" else ((.median_pct * 10 | round / 10 | tostring) + "%") end),
                ((.calls | tostring) + "/" + (.unknown_calls | tostring)),
                ((.oracle_timeouts | tostring) + "/" + (.unknown_oracle_timeouts | tostring)),
                (.timeouts | tostring), .statuses,
                (((.wall * 100 | round / 100 | tostring) + "s/") + (.unknown_wall | tostring)),
                ((.valid_true | tostring) + "/" + (.valid_false | tostring) + "/" + (.valid_unknown | tostring))
              ] | @tsv
        '
    } | column -t -s $'\t'

    printf "\nPaired input coverage:\n"
    {
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "SCENARIO" "EVIDENCE" "TOOLS" "CASES" "VALIDATED_TOOLS" "ORACLE_TIMEOUT_TOOLS" "TIMEOUT_STATUS_TOOLS"
        printf "%s" "$RAW_DATA" | jq -s -r '
            group_by(.scenario)
            | map({
                input: .[0].scenario,
                evidence: (map(.evidence_class // "unknown") | unique | join(",")),
                tools: (map(.tool // .algorithm) | unique | join(",")),
                cases: length,
                validated: (map(select(.predicate_passed == true) | (.tool // .algorithm)) | unique | join(",")),
                oracle_timeouts: (map(select((.oracle_timeouts // 0) > 0) | (.tool // .algorithm)) | unique | join(",")),
                timeouts: (map(select(.status == "timeout") | (.tool // .algorithm)) | unique | join(","))
              })
            | .[]
            | [.input, .evidence, .tools, (.cases | tostring),
               (if .validated == "" then "none" else .validated end),
               (if .oracle_timeouts == "" then "none" else .oracle_timeouts end),
               (if .timeouts == "" then "none" else .timeouts end)] | @tsv
        '
    } | column -t -s $'\t'
}

render_md() {
    cat <<'EOF'
# Multi-Tool Fuzz SQL Reduction Benchmark Evaluation

## Detailed Comparative Results

| Scenario | Evidence | Tool | Status | Scale | Initial Stmts | Reduced Stmts | Stmt Red % | Initial Bytes | Reduced Bytes | Byte Red % | Oracle Calls | Oracle Timeouts | Time (s) | Predicate |
| :--- | :--- | :--- | :--- | :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | :---: |
EOF
    printf "%s" "$RAW_DATA" | jq -r '
        [
            (.scenario // "N/A"),
            (.evidence_class // "unknown"),
            (.tool // .algorithm // "Unknown"),
            (.status // "unknown"),
            (.scale // "N/A"),
            (if .initial_stmts == null then "N/A" else (.initial_stmts | tostring) end),
            (if .reduced_stmts == null then "N/A" else (.reduced_stmts | tostring) end),
            (if .stmt_red_pct == null and .stmt_reduction_pct == null then "N/A"
             else (((.stmt_red_pct // .stmt_reduction_pct) * 10 | round / 10 | tostring) + "%") end),
            (if .initial_bytes == null then "N/A" else (.initial_bytes | tostring) end),
            (if .reduced_bytes == null then "N/A" else (.reduced_bytes | tostring) end),
            (if .byte_red_pct == null and .byte_reduction_pct == null then "N/A"
             else (((.byte_red_pct // .byte_reduction_pct) * 10 | round / 10 | tostring) + "%") end),
            (if .oracle_calls == null then "N/A" else (.oracle_calls | tostring) end),
            (if .oracle_timeouts == null then "N/A" else (.oracle_timeouts | tostring) end),
            (if .wall_time_sec == null and .time_seconds == null then "N/A"
             else (((.wall_time_sec // .time_seconds) * 100 | round / 100 | tostring) + "s") end),
            (if .predicate_passed == true then "PASS" elif .predicate_passed == false then "FAIL" else "N/A" end)
        ] | "| " + join(" | ") + " |"
    '

    cat <<'EOF'

## Per-Tool Summary

| Tool | Cases | Inputs | Paired Inputs | Unknown Sizes | Initial Stmts | Reduced Stmts | Median Stmt Red % | Calls Known/Unknown | Oracle Timeouts Known/Unknown | Timeout Cases | Status Distribution | Wall (s)/Unknown | Predicate (T/F/U) |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | :---: | :---: | ---: | :--- | :---: | :---: |
EOF
    printf "%s" "$RAW_DATA" | jq -s -r '
        . as $all
        | group_by(.tool // .algorithm)
        | map(
            . as $rows
            | ($rows[0].tool // $rows[0].algorithm) as $tool
            | ($rows | map(select(.initial_stmts != null) | .initial_stmts) | add // 0) as $init
            | ($rows | map(select(.reduced_stmts != null) | .reduced_stmts) | add // 0) as $reduced
            | ($rows | map(.stmt_red_pct // .stmt_reduction_pct)
                      | map(select(. != null)) | sort) as $pct
            | ($all | group_by(.scenario)
                | map(select((map(.tool // .algorithm) | unique | length) > 1)
                      | select(any(.[]; (.tool // .algorithm) == $tool)))
                | length) as $paired
            | {
                tool: $tool,
                cases: ($rows | length),
                inputs: ($rows | map(.scenario) | unique | length),
                paired_inputs: $paired,
                missing_sizes: ($rows | map(select(.initial_stmts == null or .reduced_stmts == null)) | length),
                initial_stmts: $init,
                reduced_stmts: $reduced,
                median_pct: (if ($pct | length) == 0 then null
                             elif (($pct | length) % 2) == 1
                             then $pct[((($pct | length) - 1) / 2 | floor)]
                             else (($pct[(($pct | length) / 2 | floor) - 1]
                                    + $pct[(($pct | length) / 2 | floor)]) / 2) end),
                calls: ($rows | map(select(.oracle_calls != null) | .oracle_calls) | add // 0),
                unknown_calls: ($rows | map(select(.oracle_calls == null)) | length),
                oracle_timeouts: ($rows | map(select(.oracle_timeouts != null) | .oracle_timeouts) | add // 0),
                unknown_oracle_timeouts: ($rows | map(select(.oracle_timeouts == null)) | length),
                timeouts: ($rows | map(select(.status == "timeout")) | length),
                statuses: ($rows | group_by(.status // "unknown")
                           | map((.[0].status // "unknown") + ":" + (length | tostring))
                           | join(",")),
                wall: ($rows | map(select(.wall_time_sec != null or .time_seconds != null)
                                   | (.wall_time_sec // .time_seconds)) | add // 0),
                unknown_wall: ($rows | map(select(.wall_time_sec == null and .time_seconds == null)) | length),
                valid_true: ($rows | map(select(.predicate_passed == true)) | length),
                valid_false: ($rows | map(select(.predicate_passed == false)) | length),
                valid_unknown: ($rows | map(select(.predicate_passed == null)) | length)
              })
        | sort_by(
            if ((.tool) | ascii_downcase) == "occam" then 0
            elif ((.tool) | ascii_downcase) | contains("creduce") then 1
            elif ((.tool) | ascii_downcase) | contains("sqlancer") then 2
            elif ((.tool) | ascii_downcase) | contains("ddmin") then 3
            else 4 end
          )
        | .[]
        | [
            .tool, (.cases | tostring), (.inputs | tostring), (.paired_inputs | tostring),
            (.missing_sizes | tostring),
            (if .missing_sizes > 0 then "N/A" else (.initial_stmts | tostring) end),
            (if .missing_sizes > 0 then "N/A" else (.reduced_stmts | tostring) end),
            (if .median_pct == null then "N/A" else ((.median_pct * 10 | round / 10 | tostring) + "%") end),
            ((.calls | tostring) + "/" + (.unknown_calls | tostring)),
            ((.oracle_timeouts | tostring) + "/" + (.unknown_oracle_timeouts | tostring)),
            (.timeouts | tostring), .statuses,
            (((.wall * 100 | round / 100 | tostring) + "s/") + (.unknown_wall | tostring)),
            ((.valid_true | tostring) + "/" + (.valid_false | tostring) + "/" + (.valid_unknown | tostring))
          ] | "| " + join(" | ") + " |"
    '

    cat <<'EOF'

## Paired Input Coverage and Timeouts

| Scenario | Evidence | Tools | Cases | Validated Tools | Oracle Timeout Tools | Timeout-Status Tools |
| :--- | :--- | :--- | ---: | :--- | :--- | :--- |
EOF
    printf "%s" "$RAW_DATA" | jq -s -r '
        group_by(.scenario)
        | map({
            input: .[0].scenario,
            evidence: (map(.evidence_class // "unknown") | unique | join(",")),
            tools: (map(.tool // .algorithm) | unique | join(",")),
            cases: length,
            validated: (map(select(.predicate_passed == true) | (.tool // .algorithm)) | unique | join(",")),
            oracle_timeouts: (map(select((.oracle_timeouts // 0) > 0) | (.tool // .algorithm)) | unique | join(",")),
            timeouts: (map(select(.status == "timeout") | (.tool // .algorithm)) | unique | join(","))
          })
        | .[]
        | [.input, .evidence, .tools, (.cases | tostring),
           (if .validated == "" then "none" else .validated end),
           (if .oracle_timeouts == "" then "none" else .oracle_timeouts end),
           (if .timeouts == "" then "none" else .timeouts end)]
        | "| " + join(" | ") + " |"
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

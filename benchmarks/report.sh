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

  --table, -t     Format comparative runs into aligned column table via column -t (default)
  --summary, -s   Aggregate case metrics by evidence/oracle/tool; show batch summary
  --md, -m        Output GitHub-flavored Markdown tables on demand
  --json, -j      Output the input records as a JSON array
  --csv           Output the input records as CSV
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
  ./benchmarks/report.sh --json results.jsonl > results.json
  ./benchmarks/report.sh --csv results.jsonl > results.csv
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
        --json|-j)
            MODE="json"
            shift 1
            ;;
        --csv)
            MODE="csv"
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

summary_path_for() {
    local path="$1" directory="${1%/*}" filename="${1##*/}"
    if [[ "$path" != */* ]]; then directory="."; fi
    if [[ "$filename" == *.* ]]; then
        local stem="${filename%.*}"
        if [[ -n "$stem" && "$stem" != "." ]]; then filename="$stem"; fi
    fi
    printf "%s/%s.summary.json" "${directory%/}" "$filename"
}

# Resolve input stream
RAW_DATA=""
SUMMARY_FILE=""
if [[ -n "$INPUT_FILE" ]]; then
    if [[ ! -r "$INPUT_FILE" ]]; then
        echo "Error: Cannot read input file '$INPUT_FILE'" >&2
        exit 1
    fi
    RAW_DATA=$(cat "$INPUT_FILE")
    SUMMARY_FILE="$(summary_path_for "$INPUT_FILE")"
elif [[ -t 0 ]]; then
    if [[ -f "$DEFAULT_JSONL" ]]; then
        RAW_DATA=$(cat "$DEFAULT_JSONL")
        SUMMARY_FILE="$(summary_path_for "$DEFAULT_JSONL")"
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
        SUMMARY_FILE="$(summary_path_for "$DEFAULT_JSONL")"
    fi
fi

if [[ -z "$RAW_DATA" ]]; then
    echo "Error: Input JSONL stream is empty and default '$DEFAULT_JSONL' does not exist." >&2
    exit 1
fi

batch_summary_rows() {
    jq -r '
        def show: if . == null then "N/A" else tostring end;
        [
            ["Processed inputs / expected", ((.input_count | show) + "/" + (.expected_input_count | show))],
            ["Inputs per second", (if .inputs_per_second == null then "N/A" else ((.inputs_per_second * 100 | round / 100 | tostring) + "/s") end)],
            ["Case/tool records / expected", ((.record_count | show) + "/" + (.expected_record_count | show))],
            ["Records per second", (if .records_per_second == null then "N/A" else ((.records_per_second * 100 | round / 100 | tostring) + "/s") end)],
            ["Validated reductions", (.validated_reduction_count | show)],
            ["Nonvalidated records", (.nonvalidated_record_count | show)],
            ["Status counts", (if .status_counts == null then "unknown"
                              else (.status_counts | to_entries | sort_by(.key) | map(.key + ":" + (.value | tostring)) | join(",")) end)],
            ["Batch makespan", (if .wall_time_sec == null then "N/A" else ((.wall_time_sec * 100 | round / 100 | tostring) + "s") end)]
        ][] | @tsv
    ' "$SUMMARY_FILE"
}

render_batch_summary() {
    [[ -n "$SUMMARY_FILE" && -r "$SUMMARY_FILE" ]] || return 0
    jq -e 'type == "object"' "$SUMMARY_FILE" >/dev/null || return 0
    if [[ "$MODE" == "md" ]]; then
        printf "\n## Batch run summary\n\n| Metric | Value |\n| :--- | :--- |\n"
        batch_summary_rows | while IFS=$'\t' read -r metric value; do
            printf "| %s | %s |\n" "$metric" "$value"
        done
    else
        printf "\nBatch run summary: %s\n" "$SUMMARY_FILE"
        batch_summary_rows | column -t -s $'\t'
    fi
}

render_table() {
    {
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "SCENARIO" "EVIDENCE" "ORACLE_KIND" "TOOL" "STATUS" "SCALE" "INIT_S" "RED_S" \
            "STMT_RED%" "INIT_B" "RED_B" "BYTE_RED%" "CALLS" "RESET_CALLS" "RESET(s)" \
            "RESET_SCOPE" "ORACLE_TO" "TIME(s)" "PREDICATE"
        printf "%s" "$RAW_DATA" | jq -r '
            [
                (.scenario // "N/A"),
                (.evidence_class // "unknown"),
                (.oracle_kind // "unknown"),
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
                (if .reset_calls == null then "N/A" else (.reset_calls | tostring) end),
                (if .reset_time_sec == null then "N/A" else ((.reset_time_sec * 1000000 | round / 1000000 | tostring) + "s") end),
                (.reset_calls_scope // "unknown"),
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
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "EVIDENCE" "ORACLE_KIND" "TOOL" "CASES" "INPUTS" "PAIRED_INPUTS" \
            "UNKNOWN_SIZES" "INIT_STMTS" "RED_STMTS" "MEDIAN_STMT_RED%" \
            "CALLS(KNOWN/UNKNOWN)" "ORACLE_TO(KNOWN/UNKNOWN)" "RESET_CALLS(KNOWN/UNKNOWN)" \
            "RESET(s)SUM/UNKNOWN_RECORDS" "RESET_SCOPE" "TIMEOUT_CASES" "STATUS_DISTRIBUTION" \
            "SUM_CASE_WALL(s)/UNKNOWN" "PREDICATE(T/F/U)"
        printf "%s" "$RAW_DATA" | jq -s -r '
            . as $all
            | group_by([(.evidence_class // "unknown"), (.oracle_kind // "unknown"),
                        (.tool // .algorithm // "unknown")])
            | map(
                . as $rows
                | ($rows[0].evidence_class // "unknown") as $evidence
                | ($rows[0].oracle_kind // "unknown") as $kind
                | ($rows[0].tool // $rows[0].algorithm // "unknown") as $tool
                | ($rows | map(select(.initial_stmts != null) | .initial_stmts) | add // 0) as $init
                | ($rows | map(select(.reduced_stmts != null) | .reduced_stmts) | add // 0) as $reduced
                | ($rows | map(.stmt_red_pct // .stmt_reduction_pct)
                          | map(select(. != null)) | sort) as $pct
                | ($all
                   | group_by([(.scenario // "unknown"), (.evidence_class // "unknown"),
                               (.oracle_kind // "unknown")])
                   | map(select((map(.tool // .algorithm // "unknown") | unique | length) > 1)
                         | select(any(.[]; (.evidence_class // "unknown") == $evidence
                                            and (.oracle_kind // "unknown") == $kind
                                            and (.tool // .algorithm // "unknown") == $tool)))
                   | length) as $paired
                | {
                    evidence: $evidence,
                    oracle_kind: $kind,
                    tool: $tool,
                    cases: ($rows | length),
                    inputs: ($rows | map(.scenario // "unknown") | unique | length),
                    paired_inputs: $paired,
                    missing_sizes: ($rows | map(select(.initial_stmts == null or .reduced_stmts == null)) | length),
                    initial_stmts: $init,
                    reduced_stmts: $reduced,
                    median_pct: (if ($pct | length) == 0 then null
                                 elif (($pct | length) % 2) == 1
                                 then $pct[((($pct | length) - 1) / 2 | floor)]
                                 else (($pct[(($pct | length) / 2 | floor) - 1]
                                        + $pct[(($pct | length) / 2 | floor)]) / 2) end),
                    calls: ($rows | map(select(.oracle_calls != null) | .oracle_calls)
                            | if length == 0 then null else add end),
                    unknown_calls: ($rows | map(select(.oracle_calls == null)) | length),
                    oracle_timeouts: ($rows | map(select(.oracle_timeouts != null) | .oracle_timeouts)
                                      | if length == 0 then null else add end),
                    unknown_oracle_timeouts: ($rows | map(select(.oracle_timeouts == null)) | length),
                    reset_calls: ($rows | map(select(.reset_calls != null) | .reset_calls)
                                  | if length == 0 then null else add end),
                    unknown_reset_calls: ($rows | map(select(.reset_calls == null)) | length),
                    reset_time: ($rows | map(select(.reset_time_sec != null) | .reset_time_sec)
                                 | if length == 0 then null else add end),
                    unknown_reset_time: ($rows | map(select(.reset_time_sec == null)) | length),
                    reset_scopes: ($rows | map(.reset_calls_scope // "unknown") | unique | join(",")),
                    timeouts: ($rows | map(select(.status == "timeout")) | length),
                    statuses: ($rows | group_by(.status // "unknown")
                               | map((.[0].status // "unknown") + ":" + (length | tostring))
                               | join(",")),
                    wall: ($rows | map(select(.wall_time_sec != null or .time_seconds != null)
                                       | (.wall_time_sec // .time_seconds))
                           | if length == 0 then null else add end),
                    unknown_wall: ($rows | map(select(.wall_time_sec == null and .time_seconds == null)) | length),
                    valid_true: ($rows | map(select(.predicate_passed == true)) | length),
                    valid_false: ($rows | map(select(.predicate_passed == false)) | length),
                    valid_unknown: ($rows | map(select(.predicate_passed == null)) | length)
                  })
            | sort_by([.evidence, .oracle_kind, .tool])
            | .[]
            | [
                .evidence, .oracle_kind, .tool, (.cases | tostring), (.inputs | tostring),
                (.paired_inputs | tostring), (.missing_sizes | tostring),
                (if .missing_sizes > 0 then "N/A" else (.initial_stmts | tostring) end),
                (if .missing_sizes > 0 then "N/A" else (.reduced_stmts | tostring) end),
                (if .median_pct == null then "N/A" else ((.median_pct * 10 | round / 10 | tostring) + "%") end),
                ((if .calls == null then "N/A" else (.calls | tostring) end) + "/" + (.unknown_calls | tostring)),
                ((if .oracle_timeouts == null then "N/A" else (.oracle_timeouts | tostring) end) + "/" + (.unknown_oracle_timeouts | tostring)),
                ((if .reset_calls == null then "N/A" else (.reset_calls | tostring) end) + "/" + (.unknown_reset_calls | tostring)),
                ((if .reset_time == null then "N/A" else ((.reset_time * 1000000 | round / 1000000 | tostring) + "s") end)
                 + "/" + (.unknown_reset_time | tostring)),
                .reset_scopes, (.timeouts | tostring), .statuses,
                ((if .wall == null then "N/A" else ((.wall * 100 | round / 100 | tostring) + "s") end)
                 + "/" + (.unknown_wall | tostring)),
                ((.valid_true | tostring) + "/" + (.valid_false | tostring) + "/" + (.valid_unknown | tostring))
              ] | @tsv
        '
    } | column -t -s $'\t'

    printf "\nPaired input coverage (within evidence class and oracle kind):\n"
    {
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
            "SCENARIO" "EVIDENCE" "ORACLE_KIND" "TOOLS" "CASES" "VALIDATED_TOOLS" \
            "ORACLE_TIMEOUT_TOOLS" "TIMEOUT_STATUS_TOOLS"
        printf "%s" "$RAW_DATA" | jq -s -r '
            group_by([(.scenario // "unknown"), (.evidence_class // "unknown"), (.oracle_kind // "unknown")])
            | map({
                input: (.[0].scenario // "unknown"),
                evidence: (.[0].evidence_class // "unknown"),
                oracle_kind: (.[0].oracle_kind // "unknown"),
                tools: (map(.tool // .algorithm // "unknown") | unique | join(",")),
                cases: length,
                validated: (map(select(.predicate_passed == true) | (.tool // .algorithm // "unknown")) | unique | join(",")),
                oracle_timeouts: (map(select((.oracle_timeouts // 0) > 0) | (.tool // .algorithm // "unknown")) | unique | join(",")),
                timeouts: (map(select(.status == "timeout") | (.tool // .algorithm // "unknown")) | unique | join(","))
              })
            | .[]
            | [.input, .evidence, .oracle_kind, .tools, (.cases | tostring),
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

| Scenario | Evidence | Oracle Kind | Tool | Status | Scale | Initial Stmts | Reduced Stmts | Stmt Red % | Initial Bytes | Reduced Bytes | Byte Red % | Oracle Calls | Reset Calls | Reset (s) | Reset Scope | Oracle Timeouts | Time (s) | Predicate |
| :--- | :--- | :--- | :--- | :--- | :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | :--- | ---: | ---: | :---: |
EOF
    printf "%s" "$RAW_DATA" | jq -r '
        [
            (.scenario // "N/A"),
            (.evidence_class // "unknown"),
            (.oracle_kind // "unknown"),
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
            (if .reset_calls == null then "N/A" else (.reset_calls | tostring) end),
            (if .reset_time_sec == null then "N/A" else ((.reset_time_sec * 1000000 | round / 1000000 | tostring) + "s") end),
            (.reset_calls_scope // "unknown"),
            (if .oracle_timeouts == null then "N/A" else (.oracle_timeouts | tostring) end),
            (if .wall_time_sec == null and .time_seconds == null then "N/A"
             else (((.wall_time_sec // .time_seconds) * 100 | round / 100 | tostring) + "s") end),
            (if .predicate_passed == true then "PASS" elif .predicate_passed == false then "FAIL" else "N/A" end)
        ] | "| " + join(" | ") + " |"
    '

    cat <<'EOF'

## Per-Tool Summary

| Evidence | Oracle Kind | Tool | Cases | Inputs | Paired Inputs | Unknown Sizes | Initial Stmts | Reduced Stmts | Median Stmt Red % | Calls Known/Unknown | Oracle Timeouts Known/Unknown | Reset Calls Known/Unknown | Reset (s) Sum/Unknown Records | Reset Scope | Timeout Cases | Status Distribution | Sum Case Wall (s)/Unknown | Predicate (T/F/U) |
| :--- | :--- | :--- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | :---: | :---: | :---: | :---: | :--- | ---: | :--- | :---: | :---: |
EOF
    printf "%s" "$RAW_DATA" | jq -s -r '
        . as $all
        | group_by([(.evidence_class // "unknown"), (.oracle_kind // "unknown"),
                    (.tool // .algorithm // "unknown")])
        | map(
            . as $rows
            | ($rows[0].evidence_class // "unknown") as $evidence
            | ($rows[0].oracle_kind // "unknown") as $kind
            | ($rows[0].tool // $rows[0].algorithm // "unknown") as $tool
            | ($rows | map(select(.initial_stmts != null) | .initial_stmts) | add // 0) as $init
            | ($rows | map(select(.reduced_stmts != null) | .reduced_stmts) | add // 0) as $reduced
            | ($rows | map(.stmt_red_pct // .stmt_reduction_pct)
                      | map(select(. != null)) | sort) as $pct
            | ($all
               | group_by([(.scenario // "unknown"), (.evidence_class // "unknown"),
                           (.oracle_kind // "unknown")])
               | map(select((map(.tool // .algorithm // "unknown") | unique | length) > 1)
                     | select(any(.[]; (.evidence_class // "unknown") == $evidence
                                        and (.oracle_kind // "unknown") == $kind
                                        and (.tool // .algorithm // "unknown") == $tool)))
               | length) as $paired
            | {
                evidence: $evidence,
                oracle_kind: $kind,
                tool: $tool,
                cases: ($rows | length),
                inputs: ($rows | map(.scenario // "unknown") | unique | length),
                paired_inputs: $paired,
                missing_sizes: ($rows | map(select(.initial_stmts == null or .reduced_stmts == null)) | length),
                initial_stmts: $init,
                reduced_stmts: $reduced,
                median_pct: (if ($pct | length) == 0 then null
                             elif (($pct | length) % 2) == 1
                             then $pct[((($pct | length) - 1) / 2 | floor)]
                             else (($pct[(($pct | length) / 2 | floor) - 1]
                                    + $pct[(($pct | length) / 2 | floor)]) / 2) end),
                calls: ($rows | map(select(.oracle_calls != null) | .oracle_calls)
                        | if length == 0 then null else add end),
                unknown_calls: ($rows | map(select(.oracle_calls == null)) | length),
                oracle_timeouts: ($rows | map(select(.oracle_timeouts != null) | .oracle_timeouts)
                                  | if length == 0 then null else add end),
                unknown_oracle_timeouts: ($rows | map(select(.oracle_timeouts == null)) | length),
                reset_calls: ($rows | map(select(.reset_calls != null) | .reset_calls)
                              | if length == 0 then null else add end),
                unknown_reset_calls: ($rows | map(select(.reset_calls == null)) | length),
                reset_time: ($rows | map(select(.reset_time_sec != null) | .reset_time_sec)
                             | if length == 0 then null else add end),
                unknown_reset_time: ($rows | map(select(.reset_time_sec == null)) | length),
                reset_scopes: ($rows | map(.reset_calls_scope // "unknown") | unique | join(",")),
                timeouts: ($rows | map(select(.status == "timeout")) | length),
                statuses: ($rows | group_by(.status // "unknown")
                           | map((.[0].status // "unknown") + ":" + (length | tostring))
                           | join(",")),
                wall: ($rows | map(select(.wall_time_sec != null or .time_seconds != null)
                                   | (.wall_time_sec // .time_seconds))
                       | if length == 0 then null else add end),
                unknown_wall: ($rows | map(select(.wall_time_sec == null and .time_seconds == null)) | length),
                valid_true: ($rows | map(select(.predicate_passed == true)) | length),
                valid_false: ($rows | map(select(.predicate_passed == false)) | length),
                valid_unknown: ($rows | map(select(.predicate_passed == null)) | length)
              })
        | sort_by([.evidence, .oracle_kind, .tool])
        | .[]
        | [
            .evidence, .oracle_kind, .tool, (.cases | tostring), (.inputs | tostring), (.paired_inputs | tostring),
            (.missing_sizes | tostring),
            (if .missing_sizes > 0 then "N/A" else (.initial_stmts | tostring) end),
            (if .missing_sizes > 0 then "N/A" else (.reduced_stmts | tostring) end),
            (if .median_pct == null then "N/A" else ((.median_pct * 10 | round / 10 | tostring) + "%") end),
            ((if .calls == null then "N/A" else (.calls | tostring) end) + "/" + (.unknown_calls | tostring)),
            ((if .oracle_timeouts == null then "N/A" else (.oracle_timeouts | tostring) end) + "/" + (.unknown_oracle_timeouts | tostring)),
            ((if .reset_calls == null then "N/A" else (.reset_calls | tostring) end) + "/" + (.unknown_reset_calls | tostring)),
            ((if .reset_time == null then "N/A" else ((.reset_time * 1000000 | round / 1000000 | tostring) + "s") end)
             + "/" + (.unknown_reset_time | tostring)),
            .reset_scopes, (.timeouts | tostring), .statuses,
            ((if .wall == null then "N/A" else ((.wall * 100 | round / 100 | tostring) + "s") end)
             + "/" + (.unknown_wall | tostring)),
            ((.valid_true | tostring) + "/" + (.valid_false | tostring) + "/" + (.valid_unknown | tostring))
          ] | "| " + join(" | ") + " |"
    '

    cat <<'EOF'

## Paired Input Coverage and Timeouts

| Scenario | Evidence | Oracle Kind | Tools | Cases | Validated Tools | Oracle Timeout Tools | Timeout-Status Tools |
| :--- | :--- | :--- | :--- | ---: | :--- | :--- | :--- |
EOF
    printf "%s" "$RAW_DATA" | jq -s -r '
        group_by([(.scenario // "unknown"), (.evidence_class // "unknown"), (.oracle_kind // "unknown")])
        | map({
            input: (.[0].scenario // "unknown"),
            evidence: (.[0].evidence_class // "unknown"),
            oracle_kind: (.[0].oracle_kind // "unknown"),
            tools: (map(.tool // .algorithm // "unknown") | unique | join(",")),
            cases: length,
            validated: (map(select(.predicate_passed == true) | (.tool // .algorithm // "unknown")) | unique | join(",")),
            oracle_timeouts: (map(select((.oracle_timeouts // 0) > 0) | (.tool // .algorithm // "unknown")) | unique | join(",")),
            timeouts: (map(select(.status == "timeout") | (.tool // .algorithm // "unknown")) | unique | join(","))
          })
        | .[]
        | [.input, .evidence, .oracle_kind, .tools, (.cases | tostring),
           (if .validated == "" then "none" else .validated end),
           (if .oracle_timeouts == "" then "none" else .oracle_timeouts end),
           (if .timeouts == "" then "none" else .timeouts end)]
        | "| " + join(" | ") + " |"
    '
}

case "$MODE" in
    table)
        render_table
        render_batch_summary
        ;;
    summary)
        render_summary
        render_batch_summary
        ;;
    md)
        render_md
        render_batch_summary
        ;;
    json)
        printf "%s" "$RAW_DATA" | jq -s '.'
        ;;
    csv)
        {
            printf "%s\n" \
                "scenario,evidence_class,oracle_kind,tool,status,scale,initial_stmts,reduced_stmts,stmt_red_pct,initial_bytes,reduced_bytes,byte_red_pct,oracle_calls,reset_calls,reset_time_sec,reset_calls_scope,oracle_timeouts,wall_time_sec,predicate_passed"
            printf "%s" "$RAW_DATA" | jq -r '
                [
                    (.scenario // ""),
                    (.evidence_class // ""),
                    (.oracle_kind // ""),
                    (.tool // .algorithm // ""),
                    (.status // ""),
                    (.scale // ""),
                    (.initial_stmts // ""),
                    (.reduced_stmts // ""),
                    (.stmt_red_pct // .stmt_reduction_pct // ""),
                    (.initial_bytes // ""),
                    (.reduced_bytes // ""),
                    (.byte_red_pct // .byte_reduction_pct // ""),
                    (.oracle_calls // ""),
                    (.reset_calls // ""),
                    (.reset_time_sec // ""),
                    (.reset_calls_scope // ""),
                    (.oracle_timeouts // ""),
                    (.wall_time_sec // .time_seconds // ""),
                    (.predicate_passed // "")
                ] | @csv
            '
        }
        ;;
    *)
        echo "Error: Unknown mode '$MODE'" >&2
        exit 1
        ;;
esac

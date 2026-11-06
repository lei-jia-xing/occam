#!/usr/bin/env bash
set -eo pipefail

if [[ $# -ne 1 || ! -r $1 ]]; then
    printf 'Usage: %s <candidate_sql_file>\n' "$0" >&2
    exit 2
fi

DUCKDB_DB=${DUCKDB_DB:-/tmp/occam_duckdb_test.db}
DUCKDB_BIN=${DUCKDB_BIN:-duckdb}
export DUCKDB_DB DUCKDB_BIN

rm -f -- "$DUCKDB_DB"

if output=$("$DUCKDB_BIN" "$DUCKDB_DB" < "$1" 2>&1); then
    status=0
else
    status=$?
fi
re_asan='(heap-use-after-free|heap-buffer-overflow|stack-buffer-overflow|global-buffer-overflow|use-after-poison|SEGV|null-dereference)'
if [[ "$output" == *"AddressSanitizer"* || "$output" == *"ASAN:"* ]]; then
    err_type="heap-use-after-free"
    if [[ "$output" =~ $re_asan ]]; then
        err_type="${BASH_REMATCH[1]}"
    fi
    printf '{"kind":"asan","error_type":"%s","frames":[]}\n' "$err_type"
    exit 0
fi

re_assert='(assertion|Assertion)[[:space:]]+.*(failed|FAILED)'
if [[ "$output" =~ $re_assert ]]; then
    printf '{"kind":"assert","assertion":"duckdb_assertion_failed","frames":[]}\n'
    exit 0
fi

# 3. Process exit status / signal detection
case "$status" in
    139|-11) signal=11 ;;
    134|-6)  signal=6  ;;
    138|-10) signal=10 ;;
    136|-8)  signal=8  ;;
    -*)      signal=$((-status)) ;;
    *)
        if (( status >= 128 )); then
            signal=$((status - 128))
        else
            if [[ -n "$output" ]]; then
                printf '%s\n' "$output" >&2
            fi
            exit 1
        fi
        ;;
esac

printf '{"kind":"signal","signal":%s,"frames":[]}\n' "$signal"

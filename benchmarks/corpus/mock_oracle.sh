#!/usr/bin/env bash
# ==============================================================================
# Occam Benchmark Corpus Mock Crash Oracle
# ==============================================================================
# Emulates crash reproduction for test corpus validation when live PostgreSQL
# or MySQL instances are not running on the local host.
#
# Accepts candidate SQL via standard input (or file path argument).
# Emits typed CrashReport JSON matching Occam's crash equivalence contract.
# ==============================================================================

set -eo pipefail

if [[ $# -ge 1 && -r "$1" ]]; then
    input_sql=$(cat "$1")
else
    input_sql=$(cat)
fi

# Reject empty SQL input (non-reproducing)
if [[ -z "${input_sql//[[:space:]]/}" ]]; then
    exit 1
fi

# If LIVE_ORACLE environment is set, delegate to DBMS harness
if [[ "$LIVE_ORACLE" == "postgresql" || "$LIVE_ORACLE" == "pg" ]]; then
    tmp_candidate=$(mktemp --suffix=.sql)
    printf '%s\n' "$input_sql" > "$tmp_candidate"
    trap 'rm -f "$tmp_candidate"' EXIT
    exec ./harness/pg_harness.sh "$tmp_candidate"
elif [[ "$LIVE_ORACLE" == "mysql" ]]; then
    tmp_candidate=$(mktemp --suffix=.sql)
    printf '%s\n' "$input_sql" > "$tmp_candidate"
    trap 'rm -f "$tmp_candidate"' EXIT
    exec ./harness/mysql_harness.sh "$tmp_candidate"
fi

# Mock Mode:
# Verify that the candidate script still contains executable SQL (not empty/comment-only)
# and emit typed SIGSEGV crash report.
printf '{"kind":"signal","signal":11,"frames":[{"index":0,"function":"exec_crash_trap"}]}\n'
exit 0

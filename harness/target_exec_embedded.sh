#!/usr/bin/env bash
# occam-exec: run the SQL script on stdin against a fresh in-memory database
# of the image's embedded engine (selected by OCCAM_TARGET_DBMS).
#
# Statement errors do not stop the script. The exit status is the engine's
# own: 0 when every statement succeeded, 1 when a statement failed, and
# 128+N when the engine died from signal N (134 = SIGABRT from a failed
# assert() or an AddressSanitizer report, 139 = SIGSEGV). Sanitizer and
# assertion reports go to stderr.
set -u

case "${OCCAM_TARGET_DBMS:-}" in
    sqlite) exec /usr/local/bin/sqlite3 -batch :memory: ;;
    duckdb) exec /usr/local/bin/duckdb -batch :memory: ;;
    *)
        echo "occam-exec: unsupported OCCAM_TARGET_DBMS='${OCCAM_TARGET_DBMS:-}'" >&2
        exit 125
        ;;
esac

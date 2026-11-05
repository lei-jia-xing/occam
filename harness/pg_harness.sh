#!/usr/bin/env bash
set -eo pipefail

if [[ $# -ne 1 || ! -r $1 ]]; then
    printf 'Usage: %s <candidate_sql_file>\n' "$0" >&2
    exit 2
fi

PGDATABASE=${PGDATABASE:-occam_test}
PGHOST=${PGHOST:-/tmp}
PGPORT=${PGPORT:-5432}
PGUSER=${PGUSER:-$USER}
export PGDATABASE PGHOST PGPORT PGUSER

dropdb --if-exists "$PGDATABASE"
createdb "$PGDATABASE"

if psql -v ON_ERROR_STOP=1 -d "$PGDATABASE" -f "$1" >/dev/null; then
    exit 1
else
    status=$?
fi

case "$status" in
    139|-11) signal=11 ;;
    134|-6) signal=6 ;;
    -*) signal=$((-status)) ;;
    *) exit 1 ;;
esac

printf '{"kind":"signal","signal":%s,"frames":[]}\n' "$signal"

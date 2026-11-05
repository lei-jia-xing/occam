#!/usr/bin/env bash
set -eo pipefail

if [[ $# -ne 1 || ! -r $1 ]]; then
    printf 'Usage: %s <candidate_sql_file>\n' "$0" >&2
    exit 2
fi

MYSQL_DATABASE=${MYSQL_DATABASE:-occam_test}
MYSQL_HOST=${MYSQL_HOST:-127.0.0.1}
MYSQL_USER=${MYSQL_USER:-root}
export MYSQL_DATABASE MYSQL_HOST MYSQL_USER

mysql -h "$MYSQL_HOST" -u "$MYSQL_USER" -e "DROP DATABASE IF EXISTS $MYSQL_DATABASE; CREATE DATABASE $MYSQL_DATABASE;"

if output=$(mysql -h "$MYSQL_HOST" -u "$MYSQL_USER" "$MYSQL_DATABASE" < "$1" 2>&1 >/dev/null); then
    status=0
else
    status=$?
fi

if [[ $status -eq 2013 || $output == *'ERROR 2013'* || $output == *CR_SERVER_LOST* ]]; then
    printf '{"kind":"signal","signal":11,"frames":[]}\n'
    exit 0
fi

if (( status < 0 )); then
    signal=$((-status))
elif (( status >= 128 )); then
    signal=$((status - 128))
else
    if [[ -n $output ]]; then
        printf '%s\n' "$output" >&2
    fi
    exit 1
fi

printf '{"kind":"signal","signal":%s,"frames":[]}\n' "$signal"

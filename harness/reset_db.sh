#!/usr/bin/env bash
# reset_db.sh - State reset and recovery harness for Occam candidate evaluation.
#
# Conforms to Occam's --reset contract:
#   - Resets the target database (occam_test) to a clean state from template_occam.
#   - If the DBMS server crashed and died, restarts/recovers the server and waits
#     until pg_isready (or mysqladmin ping) succeeds.
#   - Exits 0 on successful reset; nonzero on failure.

set -eo pipefail

DBMS="${DBMS:-postgres}"
HOST="${PGHOST:-127.0.0.1}"
PORT="${PGPORT:-5432}"
USER="${PGUSER:-postgres}"
DBNAME="${PGDATABASE:-occam_test}"
TEMPLATE="${TEMPLATE_DB:-template_occam}"
CONTAINER_NAME="${CONTAINER_NAME:-}"
MAX_WAIT_SECS=20

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dbms)
            DBMS="$2"; shift 2 ;;
        --host)
            HOST="$2"; shift 2 ;;
        --port)
            PORT="$2"; shift 2 ;;
        --user)
            USER="$2"; shift 2 ;;
        --dbname)
            DBNAME="$2"; shift 2 ;;
        --template)
            TEMPLATE="$2"; shift 2 ;;
        --container-name)
            CONTAINER_NAME="$2"; shift 2 ;;
        --help|-h)
            echo "Usage: $0 [--dbms postgres|mysql] [--host HOST] [--port PORT] [--user USER] [--dbname DBNAME] [--template TEMPLATE] [--container-name NAME]"
            exit 0 ;;
        *)
            echo "Unknown argument: $1" >&2
            exit 1 ;;
    esac
done

resolve_tool() {
    local tool="$1"
    shift
    if command -v "$tool" >/dev/null 2>&1; then
        echo "$tool"
        return 0
    fi
    for p in "$@"; do
        if [ -x "$p" ]; then
            echo "$p"
            return 0
        fi
    done
    echo "$tool"
}

PSQL_BIN=$(resolve_tool psql "$HOME/.cache/occam/postgresql-17.0-debug/install/bin/psql" /usr/local/pgsql/bin/psql /usr/lib/postgresql/13/bin/psql /usr/lib/postgresql/12/bin/psql /usr/lib/postgresql/14/bin/psql /usr/lib/postgresql/17/bin/psql /usr/bin/psql)
PG_ISREADY_BIN=$(resolve_tool pg_isready "$HOME/.cache/occam/postgresql-17.0-debug/install/bin/pg_isready" /usr/local/pgsql/bin/pg_isready /usr/lib/postgresql/13/bin/pg_isready /usr/lib/postgresql/12/bin/pg_isready /usr/lib/postgresql/14/bin/pg_isready /usr/lib/postgresql/17/bin/pg_isready /usr/bin/pg_isready)

reset_postgres() {
    local is_ready=0
    for ((i=1; i<=MAX_WAIT_SECS*5; i++)); do
        if "$PG_ISREADY_BIN" -h "$HOST" -p "$PORT" -U "$USER" >/dev/null 2>&1; then
            is_ready=1
            break
        fi

        # If server process is dead and container is specified, attempt container restart
        if [[ -n "$CONTAINER_NAME" ]] && command -v docker >/dev/null 2>&1; then
            if ! docker ps --filter "name=${CONTAINER_NAME}" --filter "status=running" --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
                echo "[reset_db] Target container ${CONTAINER_NAME} stopped; restarting..." >&2
                docker start "${CONTAINER_NAME}" >/dev/null 2>&1 || true
            fi
        elif [[ -n "${PGDATA:-}" && -d "${PGDATA}" ]] && command -v pg_ctl >/dev/null 2>&1; then
            if ! pg_ctl -D "${PGDATA}" status >/dev/null 2>&1; then
                echo "[reset_db] Postgres stopped; restarting via pg_ctl..." >&2
                pg_ctl -D "${PGDATA}" -l /var/log/postgresql/postgresql.log -w start >/dev/null 2>&1 || true
            fi
        fi

        sleep 0.2
    done

    if [[ "$is_ready" -ne 1 ]]; then
        echo "[reset_db] Error: PostgreSQL not ready at $HOST:$PORT after ${MAX_WAIT_SECS}s" >&2
        return 1
    fi

    # Terminate active backend connections to target database and reset from template
    "$PSQL_BIN" -h "$HOST" -p "$PORT" -U "$USER" -d template1 -v ON_ERROR_STOP=1 -X -q <<EOF
SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE datname = '${DBNAME}' AND pid <> pg_backend_pid();

DROP DATABASE IF EXISTS ${DBNAME};
CREATE DATABASE ${DBNAME} TEMPLATE ${TEMPLATE};
GRANT ALL PRIVILEGES ON DATABASE ${DBNAME} TO PUBLIC;
EOF
}

reset_mysql() {
    local is_ready=0
    for ((i=1; i<=MAX_WAIT_SECS*5; i++)); do
        if mysqladmin ping -h "$HOST" -P "$PORT" -u "$USER" --silent 2>/dev/null; then
            is_ready=1
            break
        fi

        if [[ -n "$CONTAINER_NAME" ]] && command -v docker >/dev/null 2>&1; then
            if ! docker ps --filter "name=${CONTAINER_NAME}" --filter "status=running" --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
                echo "[reset_db] MySQL container ${CONTAINER_NAME} stopped; restarting..." >&2
                docker start "${CONTAINER_NAME}" >/dev/null 2>&1 || true
            fi
        fi

        sleep 0.2
    done

    if [[ "$is_ready" -ne 1 ]]; then
        echo "[reset_db] Error: MySQL not ready at $HOST:$PORT after ${MAX_WAIT_SECS}s" >&2
        return 1
    fi

    mysql -h "$HOST" -P "$PORT" -u "$USER" <<EOF
DROP DATABASE IF EXISTS ${DBNAME};
CREATE DATABASE ${DBNAME};
EOF
}

if [[ "$DBMS" == "postgres" ]]; then
    reset_postgres
elif [[ "$DBMS" == "mysql" ]]; then
    reset_mysql
else
    echo "[reset_db] Unsupported DBMS: $DBMS" >&2
    exit 1
fi

#!/usr/bin/env bash
# reset_db.sh - State reset and recovery harness for Occam candidate evaluation.
#
# Conforms to Occam's --reset contract:
#   - PostgreSQL: drops and recreates the target database from a template.
#   - MySQL: drops and recreates the target database, and clears read-only
#     globals that multi-statement fuzz cases can leave behind. MySQL has no
#     CREATE DATABASE ... TEMPLATE; use a fresh container/datadir for full
#     global-state isolation.
#   - If the server crashed during a prior candidate, waits for recovery or
#     restarts the container/server and waits for connectivity.
#   - Exits 0 on successful reset; nonzero on failure (Occam treats this as an
#     infrastructure error, which is the only clean infrastructure channel).
#
# Environment defaults are DBMS-aware and are applied after --dbms is parsed.

set -eo pipefail

DBMS="${DBMS:-postgres}"
HOST=""
PORT=""
DB_USER=""
DBNAME=""
TEMPLATE="${TEMPLATE_DB:-template_occam}"
PASSWORD=""
CONTAINER_NAME="${CONTAINER_NAME:-}"
SOCKET=""
MAX_WAIT_SECS="${MAX_WAIT_SECS:-20}"

usage() {
    cat <<'EOF'
Usage: reset_db.sh [options]

Options:
  --dbms postgres|mysql   Target DBMS (default: postgres or DBMS env)
  --host HOST             Database host or PostgreSQL socket directory
  --port PORT             Database port
  --socket PATH           MySQL unix socket path (overrides --host/--port)
  --user USER             Database user
  --password PASSWORD     Database password (prefer PGPASSWORD / MYSQL_PWD)
  --dbname NAME           Target database (default: occam_test)
  --template NAME         PostgreSQL template database (default: template_occam)
  --container-name NAME   Docker container to restart if the server died
  --max-wait SECONDS      Readiness timeout (default: 20)
  -h, --help              Show this help
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dbms)
            DBMS="${2:?missing value for --dbms}"
            shift 2
            ;;
        --host)
            HOST="${2:?missing value for --host}"
            shift 2
            ;;
        --port)
            PORT="${2:?missing value for --port}"
            shift 2
            ;;
        --socket)
            SOCKET="${2:?missing value for --socket}"
            shift 2
            ;;
        --user)
            DB_USER="${2:?missing value for --user}"
            shift 2
            ;;
        --password)
            PASSWORD="${2:?missing value for --password}"
            shift 2
            ;;
        --dbname)
            DBNAME="${2:?missing value for --dbname}"
            shift 2
            ;;
        --template)
            TEMPLATE="${2:?missing value for --template}"
            shift 2
            ;;
        --container-name)
            CONTAINER_NAME="${2:?missing value for --container-name}"
            shift 2
            ;;
        --max-wait)
            MAX_WAIT_SECS="${2:?missing value for --max-wait}"
            shift 2
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        *)
            echo "Unknown argument: $1" >&2
            usage >&2
            exit 1
            ;;
    esac
done

if [[ "$DBMS" != "postgres" && "$DBMS" != "mysql" ]]; then
    echo "[reset_db] Unsupported DBMS: $DBMS" >&2
    exit 1
fi

if [[ ! "$MAX_WAIT_SECS" =~ ^[0-9]+$ ]] || (( MAX_WAIT_SECS <= 0 )); then
    echo "[reset_db] --max-wait must be a positive integer" >&2
    exit 1
fi

# Resolve defaults after parsing so PGHOST/MYSQL_HOST do not cross-contaminate.
if [[ "$DBMS" == "mysql" ]]; then
    HOST="${HOST:-${MYSQL_HOST:-127.0.0.1}}"
    PORT="${PORT:-${MYSQL_TCP_PORT:-3306}}"
    DB_USER="${DB_USER:-${MYSQL_USER:-root}}"
    DBNAME="${DBNAME:-${MYSQL_DATABASE:-occam_test}}"
    PASSWORD="${PASSWORD:-${MYSQL_PWD:-}}"
    SOCKET="${SOCKET:-${MYSQL_SOCKET:-}}"
    export MYSQL_PWD="$PASSWORD"
    if [[ ! "$DBNAME" =~ ^[A-Za-z0-9_]+$ ]]; then
        echo "[reset_db] MySQL database name must match ^[A-Za-z0-9_]+$: $DBNAME" >&2
        exit 1
    fi
else
    HOST="${HOST:-${PGHOST:-127.0.0.1}}"
    PORT="${PORT:-${PGPORT:-5432}}"
    DB_USER="${DB_USER:-${PGUSER:-postgres}}"
    DBNAME="${DBNAME:-${PGDATABASE:-occam_test}}"
    PASSWORD="${PASSWORD:-${PGPASSWORD:-}}"
    export PGPASSWORD="$PASSWORD"
fi

resolve_tool() {
    local tool="$1"
    shift
    if command -v "$tool" >/dev/null 2>&1; then
        echo "$tool"
        return 0
    fi
    for path in "$@"; do
        if [ -x "$path" ]; then
            echo "$path"
            return 0
        fi
    done
    echo "$tool"
}

PSQL_BIN=$(resolve_tool psql "$HOME/.cache/occam/postgresql-17.0-debug/install/bin/psql" /usr/local/pgsql/bin/psql /usr/lib/postgresql/13/bin/psql /usr/lib/postgresql/12/bin/psql /usr/lib/postgresql/14/bin/psql /usr/lib/postgresql/15/bin/psql /usr/lib/postgresql/16/bin/psql /usr/lib/postgresql/17/bin/psql /usr/bin/psql)
PG_ISREADY_BIN=$(resolve_tool pg_isready "$HOME/.cache/occam/postgresql-17.0-debug/install/bin/pg_isready" /usr/local/pgsql/bin/pg_isready /usr/lib/postgresql/13/bin/pg_isready /usr/lib/postgresql/12/bin/pg_isready /usr/lib/postgresql/14/bin/pg_isready /usr/lib/postgresql/15/bin/pg_isready /usr/lib/postgresql/16/bin/pg_isready /usr/lib/postgresql/17/bin/pg_isready /usr/bin/pg_isready)
MYSQL_BIN=$(resolve_tool mysql /usr/local/mysql/bin/mysql /usr/bin/mysql)
MYSQLADMIN_BIN=$(resolve_tool mysqladmin /usr/local/mysql/bin/mysqladmin /usr/bin/mysqladmin)

container_running() {
    [[ -n "$CONTAINER_NAME" ]] \
        && command -v docker >/dev/null 2>&1 \
        && docker ps --filter "name=${CONTAINER_NAME}" --format '{{.Names}}' \
            | grep -Fxq "$CONTAINER_NAME"
}

reset_postgres() {
    local is_ready=0
    local attempt
    for ((attempt = 1; attempt <= MAX_WAIT_SECS * 5; attempt++)); do
        if "$PG_ISREADY_BIN" -h "$HOST" -p "$PORT" -U "$DB_USER" >/dev/null 2>&1; then
            is_ready=1
            break
        fi

        if ! container_running; then
            if [[ -n "$CONTAINER_NAME" ]] && command -v docker >/dev/null 2>&1; then
                echo "[reset_db] Target container ${CONTAINER_NAME} stopped; restarting..." >&2
                docker start "$CONTAINER_NAME" >/dev/null 2>&1 || true
            elif [[ -n "${PGDATA:-}" && -d "${PGDATA}" ]] && command -v pg_ctl >/dev/null 2>&1; then
                if ! pg_ctl -D "${PGDATA}" status >/dev/null 2>&1; then
                    echo "[reset_db] Postgres stopped; restarting via pg_ctl..." >&2
                    pg_ctl -D "${PGDATA}" -l /var/log/postgresql/postgresql.log -w start >/dev/null 2>&1 || true
                fi
            fi
        fi

        sleep 0.2
    done

    if [[ "$is_ready" -ne 1 ]]; then
        echo "[reset_db] Error: PostgreSQL not ready at $HOST:$PORT after ${MAX_WAIT_SECS}s" >&2
        return 1
    fi

    # Terminate active backend connections and reset from the template.
    # psql variables quote the database names as identifiers/literals.
    "$PSQL_BIN" -h "$HOST" -p "$PORT" -U "$DB_USER" -d template1 \
        -v ON_ERROR_STOP=1 -v dbname="$DBNAME" -v template="$TEMPLATE" -X -q <<'EOF'
SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE datname = :'dbname' AND pid <> pg_backend_pid();

DROP DATABASE IF EXISTS :"dbname";
CREATE DATABASE :"dbname" TEMPLATE :"template";
GRANT ALL PRIVILEGES ON DATABASE :"dbname" TO PUBLIC;
EOF
}

reset_mysql() {
    local is_ready=0
    local attempt
    local conn_args=()
    if [[ -n "$SOCKET" ]]; then
        conn_args=(--socket "$SOCKET")
    else
        conn_args=(-h "$HOST" -P "$PORT")
    fi

    for ((attempt = 1; attempt <= MAX_WAIT_SECS * 5; attempt++)); do
        if "$MYSQLADMIN_BIN" --no-defaults "${conn_args[@]}" -u "$DB_USER" \
            --silent ping >/dev/null 2>&1; then
            is_ready=1
            break
        fi

        if ! container_running; then
            if [[ -n "$CONTAINER_NAME" ]] && command -v docker >/dev/null 2>&1; then
                echo "[reset_db] MySQL container ${CONTAINER_NAME} stopped; restarting..." >&2
                docker start "$CONTAINER_NAME" >/dev/null 2>&1 || true
            fi
        fi

        sleep 0.2
    done

    if [[ "$is_ready" -ne 1 ]]; then
        echo "[reset_db] Error: MySQL not ready at $HOST:$PORT after ${MAX_WAIT_SECS}s" >&2
        return 1
    fi

    # Clear globals that fuzz cases can leave behind (for example
    # mysql_bug_91975 sets @@global.super_read_only = TRUE). Without this,
    # later candidates and even DROP DATABASE can fail.
    "$MYSQL_BIN" --no-defaults "${conn_args[@]}" -u "$DB_USER" <<EOF
SET GLOBAL super_read_only = OFF;
SET GLOBAL read_only = OFF;
DROP DATABASE IF EXISTS \`${DBNAME}\`;
CREATE DATABASE \`${DBNAME}\`;
EOF
}

case "$DBMS" in
    postgres)
        reset_postgres
        ;;
    mysql)
        reset_mysql
        ;;
esac

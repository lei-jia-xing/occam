#!/usr/bin/env bash
# ==============================================================================
# Occam Upstream Fuzz Driver: SQUIRREL (AFL++ coverage-guided mutator)
# Targets SQLite, PostgreSQL, MySQL, and MariaDB.
# ==============================================================================
set -eo pipefail

DBMS=""
SQUIRREL_HOME="${SQUIRREL_HOME:-/opt/squirrel}"
INPUT_DIR=""
OUTPUT_DIR=""
CONFIG_FILE=""
MUTATOR_LIB=""
FUZZER_BIN=""
TARGET_BIN=""
FUZZER_ID="${FUZZER_ID:-squirrel0}"
TIMEOUT_SECS=""
AFL_MAP_SIZE_ARG="${AFL_MAP_SIZE:-}"
HOST=""
PORT=""
DB_USER=""
PASSWORD=""
PASSWORD_SET=0
SOCKET=""
DB_NAME=""
SERVER_CMD=""
DRY_RUN=0
EXTRA_ARGS=()

usage() {
    cat <<'EOF'
Usage: squirrel_driver.sh [OPTIONS] [DBMS] [-- EXTRA_AFL_ARGS...]

Options:
  --dbms DBMS              Target DBMS: sqlite|postgres|postgresql|mysql|mariadb
  --squirrel-home DIR      Path to built SQUIRREL checkout (default: /opt/squirrel)
  --input-dir DIR          AFL++ seed corpus directory
  --output-dir DIR         AFL++ output directory (crashes land in <dir>/<fuzzer_id>/crashes/)
  --config FILE            Explicit SQUIRREL YAML config (auto-generated in output-dir if omitted)
  --mutator-lib PATH       Path to lib<dbms>_mutator.so
  --fuzzer-bin PATH        Path to afl-fuzz binary
  --target-bin PATH        Target fuzz harness binary (sqlite_ossfuzz or db_driver)
  --fuzzer-id NAME         AFL++ fuzzer instance name (-M NAME, default: squirrel0)
  --timeout-secs N         Stop AFL++ after N seconds (-V N)
  --afl-map-size N         Override AFL_MAP_SIZE for client/server instrumentation
  --host HOST              Server host for generated client/server config (default: 127.0.0.1)
  --port PORT              Server port for generated client/server config
  --user USER              Database user for generated config
  --password PASSWORD      Database password for generated config
  --socket PATH            MySQL/MariaDB unix socket path for generated config
  --db-name NAME           Database name / prefix for generated config
  --server-cmd CMD         Server startup command for generated config
  --dry-run                Print resolved environment and command without executing afl-fuzz
  -h, --help               Show this help
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dbms)
            DBMS="${2:?missing value for --dbms}"
            shift 2
            ;;
        --dbms=*)
            DBMS="${1#*=}"
            shift
            ;;
        --squirrel-home)
            SQUIRREL_HOME="${2:?missing value for --squirrel-home}"
            shift 2
            ;;
        --squirrel-home=*)
            SQUIRREL_HOME="${1#*=}"
            shift
            ;;
        --input-dir)
            INPUT_DIR="${2:?missing value for --input-dir}"
            shift 2
            ;;
        --input-dir=*)
            INPUT_DIR="${1#*=}"
            shift
            ;;
        --output-dir)
            OUTPUT_DIR="${2:?missing value for --output-dir}"
            shift 2
            ;;
        --output-dir=*)
            OUTPUT_DIR="${1#*=}"
            shift
            ;;
        --config)
            CONFIG_FILE="${2:?missing value for --config}"
            shift 2
            ;;
        --config=*)
            CONFIG_FILE="${1#*=}"
            shift
            ;;
        --mutator-lib)
            MUTATOR_LIB="${2:?missing value for --mutator-lib}"
            shift 2
            ;;
        --mutator-lib=*)
            MUTATOR_LIB="${1#*=}"
            shift
            ;;
        --fuzzer-bin)
            FUZZER_BIN="${2:?missing value for --fuzzer-bin}"
            shift 2
            ;;
        --fuzzer-bin=*)
            FUZZER_BIN="${1#*=}"
            shift
            ;;
        --target-bin)
            TARGET_BIN="${2:?missing value for --target-bin}"
            shift 2
            ;;
        --target-bin=*)
            TARGET_BIN="${1#*=}"
            shift
            ;;
        --fuzzer-id)
            FUZZER_ID="${2:?missing value for --fuzzer-id}"
            shift 2
            ;;
        --fuzzer-id=*)
            FUZZER_ID="${1#*=}"
            shift
            ;;
        --timeout-secs|--timeout)
            TIMEOUT_SECS="${2:?missing value for --timeout-secs}"
            shift 2
            ;;
        --timeout-secs=*|--timeout=*)
            TIMEOUT_SECS="${1#*=}"
            shift
            ;;
        --afl-map-size)
            AFL_MAP_SIZE_ARG="${2:?missing value for --afl-map-size}"
            shift 2
            ;;
        --afl-map-size=*)
            AFL_MAP_SIZE_ARG="${1#*=}"
            shift
            ;;
        --host)
            HOST="${2:?missing value for --host}"
            shift 2
            ;;
        --host=*)
            HOST="${1#*=}"
            shift
            ;;
        --port)
            PORT="${2:?missing value for --port}"
            shift 2
            ;;
        --port=*)
            PORT="${1#*=}"
            shift
            ;;
        --user)
            DB_USER="${2:?missing value for --user}"
            shift 2
            ;;
        --user=*)
            DB_USER="${1#*=}"
            shift
            ;;
        --password)
            PASSWORD="${2?missing value for --password}"
            PASSWORD_SET=1
            shift 2
            ;;
        --password=*)
            PASSWORD="${1#*=}"
            PASSWORD_SET=1
            shift
            ;;
        --socket)
            SOCKET="${2:?missing value for --socket}"
            shift 2
            ;;
        --socket=*)
            SOCKET="${1#*=}"
            shift
            ;;
        --db-name)
            DB_NAME="${2:?missing value for --db-name}"
            shift 2
            ;;
        --db-name=*)
            DB_NAME="${1#*=}"
            shift
            ;;
        --server-cmd)
            SERVER_CMD="${2:?missing value for --server-cmd}"
            shift 2
            ;;
        --server-cmd=*)
            SERVER_CMD="${1#*=}"
            shift
            ;;
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        --)
            shift
            EXTRA_ARGS+=("$@")
            break
            ;;
        sqlite|postgres|postgresql|mysql|mariadb)
            if [[ -z "$DBMS" ]]; then
                DBMS="$1"
            else
                EXTRA_ARGS+=("$1")
            fi
            shift
            ;;
        *)
            EXTRA_ARGS+=("$1")
            shift
            ;;
    esac
done

case "$DBMS" in
    sqlite)
        SQUIRREL_DBMS="sqlite"
        MUTATOR_DBMS="sqlite"
        DEFAULT_INPUT_SUBDIR="input"
        ;;
    postgres|postgresql)
        SQUIRREL_DBMS="postgresql"
        MUTATOR_DBMS="postgresql"
        DEFAULT_INPUT_SUBDIR="pqsql_input"
        ;;
    mysql)
        SQUIRREL_DBMS="mysql"
        MUTATOR_DBMS="mysql"
        DEFAULT_INPUT_SUBDIR="mysql_input"
        ;;
    mariadb)
        SQUIRREL_DBMS="mariadb"
        # Upstream SQUIRREL shares libmysql_mutator.so between MySQL and MariaDB.
        MUTATOR_DBMS="mysql"
        DEFAULT_INPUT_SUBDIR="mysql_input"
        ;;
    "")
        echo "Error: --dbms (or positional DBMS) is required." >&2
        usage >&2
        exit 2
        ;;
    *)
        echo "Error: unsupported DBMS '$DBMS' for SQUIRREL (expected sqlite, postgres, mysql, or mariadb)." >&2
        exit 2
        ;;
esac

if [[ -n "$TIMEOUT_SECS" ]] && [[ ! "$TIMEOUT_SECS" =~ ^[1-9][0-9]*$ ]]; then
    echo "Error: --timeout-secs must be a positive integer: $TIMEOUT_SECS" >&2
    exit 2
fi

if [[ -n "$AFL_MAP_SIZE_ARG" ]] && [[ ! "$AFL_MAP_SIZE_ARG" =~ ^[1-9][0-9]*$ ]]; then
    echo "Error: --afl-map-size must be a positive integer: $AFL_MAP_SIZE_ARG" >&2
    exit 2
fi

INPUT_DIR="${INPUT_DIR:-${SQUIRREL_HOME}/data/fuzz_root/${DEFAULT_INPUT_SUBDIR}}"
OUTPUT_DIR="${OUTPUT_DIR:-/tmp/squirrel_${SQUIRREL_DBMS}_out}"
MUTATOR_LIB="${MUTATOR_LIB:-${SQUIRREL_HOME}/build/lib${MUTATOR_DBMS}_mutator.so}"
FUZZER_BIN="${FUZZER_BIN:-${SQUIRREL_HOME}/AFLplusplus/afl-fuzz}"

if [[ -z "$TARGET_BIN" ]]; then
    if [[ "$SQUIRREL_DBMS" == "sqlite" ]]; then
        TARGET_BIN="${SQUIRREL_HOME}/bin/sqlite_ossfuzz"
    else
        TARGET_BIN="${SQUIRREL_HOME}/build/db_driver"
    fi
fi

if [[ -z "$AFL_MAP_SIZE_ARG" && -f "${SQUIRREL_HOME}/mapsize.${SQUIRREL_DBMS}" ]]; then
    AFL_MAP_SIZE_ARG="$(tr -d '[:space:]' < "${SQUIRREL_HOME}/mapsize.${SQUIRREL_DBMS}")"
fi

mkdir -p "$OUTPUT_DIR"
OUTPUT_DIR="$(cd -P -- "$OUTPUT_DIR" && pwd)"

# Resolve client/server defaults after DBMS selection.
if [[ "$SQUIRREL_DBMS" == "postgresql" ]]; then
    HOST="${HOST:-${PGHOST:-127.0.0.1}}"
    PORT="${PORT:-${PGPORT:-5432}}"
    DB_USER="${DB_USER:-${PGUSER:-postgres}}"
    if [[ "$PASSWORD_SET" -eq 0 ]]; then
        PASSWORD="${PGPASSWORD:-}"
    fi
    DB_NAME="${DB_NAME:-${PGDATABASE:-postgres}}"
    if [[ -z "$SERVER_CMD" ]]; then
        PG_BASE="${SQUIRREL_HOME}/targets/pgsql"
        PG_CMD="\"${PG_BASE}/bin/postgres\" -D \"${PG_BASE}/data\" -p ${PORT}"
        if [[ "$(id -u)" -eq 0 ]] && command -v gosu >/dev/null 2>&1 && id squirrel >/dev/null 2>&1; then
            RAW_SRV="gosu squirrel ${PG_CMD}"
        else
            RAW_SRV="${PG_CMD}"
        fi
        SERVER_CMD="(while true; do ${RAW_SRV}; sleep 0.2; done) >/dev/null 2>&1 &"
    fi
elif [[ "$SQUIRREL_DBMS" == "mysql" || "$SQUIRREL_DBMS" == "mariadb" ]]; then
    if [[ "$SQUIRREL_DBMS" == "mariadb" ]]; then
        HOST="${HOST:-${MARIADB_HOST:-${MYSQL_HOST:-127.0.0.1}}}"
        PORT="${PORT:-${MARIADB_TCP_PORT:-${MYSQL_TCP_PORT:-3306}}}"
        DB_USER="${DB_USER:-${MARIADB_USER:-${MYSQL_USER:-root}}}"
        SOCKET="${SOCKET:-${MARIADB_UNIX_PORT:-${MYSQL_SOCKET:-/tmp/mysql.sock}}}"
        if [[ "$PASSWORD_SET" -eq 0 ]]; then
            PASSWORD="${MARIADB_PWD:-${MYSQL_PWD:-}}"
        fi
        if [[ -z "$SERVER_CMD" ]]; then
            MDB_BASE="${SQUIRREL_HOME}/targets/mariadb"
            if [[ -x "${MDB_BASE}/bin/mariadbd" ]]; then
                MDB_BIN="${MDB_BASE}/bin/mariadbd"
            elif [[ -x "${MDB_BASE}/bin/mysqld" ]]; then
                MDB_BIN="${MDB_BASE}/bin/mysqld"
            elif [[ -x "/usr/local/mysql/bin/mariadbd" ]]; then
                MDB_BASE="/usr/local/mysql"
                MDB_BIN="/usr/local/mysql/bin/mariadbd"
            else
                MDB_BIN="${MDB_BASE}/bin/mariadbd"
            fi
            USER_FLAG=""
            SERVER_RUN_DIR="${OUTPUT_DIR}/server"
            mkdir -p "$SERVER_RUN_DIR"
            if [[ "$(id -u)" -eq 0 ]] && id squirrel >/dev/null 2>&1; then
                USER_FLAG="--user=squirrel "
                chown squirrel:squirrel "$SERVER_RUN_DIR"
            fi
            RAW_SRV="\"${MDB_BIN}\" --basedir=\"${MDB_BASE}\" --datadir=\"${MDB_BASE}/data\" ${USER_FLAG}--port=${PORT} --socket=\"${SOCKET}\" --log-error=\"${SERVER_RUN_DIR}/err_log.err\" --pid-file=\"${SERVER_RUN_DIR}/server_pid.pid\""
            SERVER_CMD="(while true; do ${RAW_SRV}; sleep 0.2; done) >/dev/null 2>&1 &"
        fi
    else
        HOST="${HOST:-${MYSQL_HOST:-127.0.0.1}}"
        PORT="${PORT:-${MYSQL_TCP_PORT:-3306}}"
        DB_USER="${DB_USER:-${MYSQL_USER:-root}}"
        SOCKET="${SOCKET:-${MYSQL_SOCKET:-/tmp/mysql.sock}}"
        if [[ "$PASSWORD_SET" -eq 0 ]]; then
            PASSWORD="${MYSQL_PWD:-}"
        fi
        if [[ -z "$SERVER_CMD" ]]; then
            MY_BASE="${SQUIRREL_HOME}/targets/mysql"
            if [[ ! -x "${MY_BASE}/bin/mysqld" && -x "/usr/local/mysql/bin/mysqld" ]]; then
                MY_BASE="/usr/local/mysql"
            fi
            USER_FLAG=""
            SERVER_RUN_DIR="${OUTPUT_DIR}/server"
            mkdir -p "$SERVER_RUN_DIR"
            if [[ "$(id -u)" -eq 0 ]] && id squirrel >/dev/null 2>&1; then
                USER_FLAG="--user=squirrel "
                chown squirrel:squirrel "$SERVER_RUN_DIR"
            fi
            RAW_SRV="\"${MY_BASE}/bin/mysqld\" --basedir=\"${MY_BASE}\" --datadir=\"${MY_BASE}/data\" ${USER_FLAG}--port=${PORT} --socket=\"${SOCKET}\" --log-error=\"${SERVER_RUN_DIR}/err_log.err\" --pid-file=\"${SERVER_RUN_DIR}/server_pid.pid\""
            SERVER_CMD="(while true; do ${RAW_SRV}; sleep 0.2; done) >/dev/null 2>&1 &"
        fi
    fi
    DB_NAME="${DB_NAME:-test}"
fi

if [[ -z "$CONFIG_FILE" ]]; then
    CONFIG_FILE="${OUTPUT_DIR}/squirrel_config_${SQUIRREL_DBMS}.yml"
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    REPO_CONFIG="${SCRIPT_DIR}/../config/generated/harness.json"
    if [[ -z "${OCCAM_HARNESS_JSON:-}" && -f "$REPO_CONFIG" ]]; then
        export OCCAM_HARNESS_JSON="$REPO_CONFIG"
    fi

    HARNESS_BIN="${OCCAM_HARNESS_BIN:-}"
    if [[ -z "$HARNESS_BIN" ]]; then
        HARNESS_BIN="$(command -v occam-harness || true)"
        if [[ -z "$HARNESS_BIN" && -f "${SCRIPT_DIR}/../occam.cabal" ]] && command -v cabal >/dev/null 2>&1; then
            HARNESS_BIN="$(cd "${SCRIPT_DIR}/.." && cabal list-bin exe:occam-harness 2>/dev/null)" || HARNESS_BIN=""
        fi
    fi
    if [[ ! -x "$HARNESS_BIN" ]]; then
        echo "Error: build occam-harness with 'cabal build exe:occam-harness' or set OCCAM_HARNESS_BIN." >&2
        exit 1
    fi
    "$HARNESS_BIN" render-squirrel-yaml \
        --dbms "$SQUIRREL_DBMS" \
        --squirrel-home "$SQUIRREL_HOME" \
        --host "$HOST" \
        --port "${PORT:-0}" \
        --user "$DB_USER" \
        --password "$PASSWORD" \
        --socket "$SOCKET" \
        --db-name "$DB_NAME" \
        --server-cmd "$SERVER_CMD" > "$CONFIG_FILE"
fi

if [[ "$DRY_RUN" -eq 0 ]]; then
    if [[ ! -d "$INPUT_DIR" ]]; then
        echo "Error: SQUIRREL input seed directory not found: $INPUT_DIR" >&2
        exit 1
    fi
    if [[ ! -f "$CONFIG_FILE" ]]; then
        echo "Error: SQUIRREL config file not found: $CONFIG_FILE" >&2
        exit 1
    fi
    if [[ ! -f "$MUTATOR_LIB" ]]; then
        echo "Error: SQUIRREL mutator library not found: $MUTATOR_LIB" >&2
        exit 1
    fi
    if [[ ! -x "$FUZZER_BIN" ]]; then
        echo "Error: AFL++ afl-fuzz binary not executable: $FUZZER_BIN" >&2
        exit 1
    fi
    if [[ ! -x "$TARGET_BIN" ]]; then
        echo "Error: target fuzz binary not executable: $TARGET_BIN" >&2
        exit 1
    fi
fi

export AFL_CUSTOM_MUTATOR_ONLY=1
export AFL_DISABLE_TRIM=1
export AFL_FAST_CAL=1
export AFL_AUTORESUME=1
export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1
export AFL_SKIP_CPUFREQ=1
export AFL_NO_UI=1
export AFL_FORKSRV_INIT_TMOUT=1000000
export AFL_CUSTOM_MUTATOR_LIBRARY="$MUTATOR_LIB"
export SQUIRREL_CONFIG="$CONFIG_FILE"
if [[ -n "$AFL_MAP_SIZE_ARG" ]]; then
    export AFL_MAP_SIZE="$AFL_MAP_SIZE_ARG"
fi

CMD=("$FUZZER_BIN" "-i" "$INPUT_DIR" "-o" "$OUTPUT_DIR" "-M" "$FUZZER_ID")
if [[ "$SQUIRREL_DBMS" != "sqlite" ]]; then
    CMD+=("-t" "60000")
fi
if [[ -n "$TIMEOUT_SECS" ]]; then
    CMD+=("-V" "$TIMEOUT_SECS")
fi
if [[ ${#EXTRA_ARGS[@]} -gt 0 ]]; then
    CMD+=("${EXTRA_ARGS[@]}")
fi
if [[ "$SQUIRREL_DBMS" == "sqlite" ]]; then
    CMD+=("--" "$TARGET_BIN" "@@")
else
    CMD+=("--" "$TARGET_BIN")
fi

if [[ "$DRY_RUN" -eq 1 ]]; then
    printf 'DBMS=%s\nSQUIRREL_CONFIG=%s\nAFL_CUSTOM_MUTATOR_LIBRARY=%s\nOUTPUT_DIR=%s\n' \
        "$SQUIRREL_DBMS" "$SQUIRREL_CONFIG" "$AFL_CUSTOM_MUTATOR_LIBRARY" "$OUTPUT_DIR"
    if [[ -n "$AFL_MAP_SIZE_ARG" ]]; then
        printf 'AFL_MAP_SIZE=%s\n' "$AFL_MAP_SIZE_ARG"
    fi
    printf 'CMD='
    printf '%q ' "${CMD[@]}"
    printf '\n'
    exit 0
fi

echo "==> Running SQUIRREL ($SQUIRREL_DBMS) with mutator $MUTATOR_LIB..."
echo "==> Config: $SQUIRREL_CONFIG"
echo "==> Crash output dir: $OUTPUT_DIR/$FUZZER_ID/crashes"

exec "${CMD[@]}"

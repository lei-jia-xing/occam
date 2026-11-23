#!/usr/bin/env bash
# ==============================================================================
# Occam Upstream Fuzz Driver: SQLancer
# Targets PostgreSQL, MySQL, MariaDB, ClickHouse, SQLite3, and DuckDB.
# ==============================================================================
set -eo pipefail

DBMS=""
HOST=""
PORT=""
DB_USER=""
PASSWORD=""
PASSWORD_SET=0
ORACLE=""
NUM_THREADS="${NUM_THREADS:-1}"
NUM_TRIES=""
NUM_QUERIES=""
MAX_GENERATED_DBS=""
TIMEOUT_SECONDS=""
RANDOM_SEED=""
DATABASE_PREFIX=""
EXIT_CODE_ERROR=""
OUT_DIR=""
SQLANCER_JAR_ARG=""
JAVA_BIN="${JAVA_BIN:-java}"
DRY_RUN=0
EXTRA_ARGS=()

usage() {
    cat <<'EOF'
Usage: sqlancer_driver.sh [OPTIONS] [DBMS] [-- EXTRA_SQLANCER_ARGS...]

Options:
  --dbms DBMS                    Target DBMS: postgres|mysql|mariadb|clickhouse|sqlite|sqlite3|duckdb
  --host HOST                    Server host (default: 127.0.0.1 or DBMS env)
  --port PORT                    Server port (default: 5432 postgres, 3306 mysql/mariadb, 8123 clickhouse HTTP)
  --user USER                    Server username (default: postgres / root / default)
  --password PASSWORD            Server password (prefer PGPASSWORD / MYSQL_PWD / MARIADB_PWD / CLICKHOUSE_PASSWORD)
  --oracle NAME                  SQLancer oracle after DBMS command (e.g., NoREC, TLP, TLPWhere, QUERY_PARTITIONING)
  --num-threads N                Concurrent SQLancer worker threads (default: 1)
  --num-tries N                  Stop after N found errors (SQLancer --num-tries)
  --num-queries N                Queries per database before rotation (SQLancer --num-queries)
  --max-generated-databases N    Maximum generated databases per thread
  --timeout-seconds N            Maximum runtime duration in seconds
  --random-seed N                Deterministic RNG seed
  --database-prefix PREFIX       Prefix for generated databases
  --exit-code-error CODE         Exit code when SQLancer detects a bug/error
  --out-dir DIR                  Output directory for SQLancer logs (default: /tmp/sqlancer_<dbms>_logs)
  --sqlancer-jar PATH            Path to shaded/packaged sqlancer.jar (and sibling lib/ directory)
  --java-bin PATH                Java executable (default: java)
  --dry-run                      Print resolved command and environment without launching Java
  -h, --help                     Show this help
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
        --user|--username)
            DB_USER="${2:?missing value for --user}"
            shift 2
            ;;
        --user=*|--username=*)
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
        --oracle)
            ORACLE="${2:?missing value for --oracle}"
            shift 2
            ;;
        --oracle=*)
            ORACLE="${1#*=}"
            shift
            ;;
        --num-threads)
            NUM_THREADS="${2:?missing value for --num-threads}"
            shift 2
            ;;
        --num-threads=*)
            NUM_THREADS="${1#*=}"
            shift
            ;;
        --num-tries)
            NUM_TRIES="${2:?missing value for --num-tries}"
            shift 2
            ;;
        --num-tries=*)
            NUM_TRIES="${1#*=}"
            shift
            ;;
        --num-queries)
            NUM_QUERIES="${2:?missing value for --num-queries}"
            shift 2
            ;;
        --num-queries=*)
            NUM_QUERIES="${1#*=}"
            shift
            ;;
        --max-generated-databases)
            MAX_GENERATED_DBS="${2:?missing value for --max-generated-databases}"
            shift 2
            ;;
        --max-generated-databases=*)
            MAX_GENERATED_DBS="${1#*=}"
            shift
            ;;
        --timeout-seconds|--timeout)
            TIMEOUT_SECONDS="${2:?missing value for --timeout-seconds}"
            shift 2
            ;;
        --timeout-seconds=*|--timeout=*)
            TIMEOUT_SECONDS="${1#*=}"
            shift
            ;;
        --random-seed)
            RANDOM_SEED="${2:?missing value for --random-seed}"
            shift 2
            ;;
        --random-seed=*)
            RANDOM_SEED="${1#*=}"
            shift
            ;;
        --database-prefix)
            DATABASE_PREFIX="${2:?missing value for --database-prefix}"
            shift 2
            ;;
        --database-prefix=*)
            DATABASE_PREFIX="${1#*=}"
            shift
            ;;
        --exit-code-error)
            EXIT_CODE_ERROR="${2:?missing value for --exit-code-error}"
            shift 2
            ;;
        --exit-code-error=*)
            EXIT_CODE_ERROR="${1#*=}"
            shift
            ;;
        --out-dir|--output-dir)
            OUT_DIR="${2:?missing value for --out-dir}"
            shift 2
            ;;
        --out-dir=*|--output-dir=*)
            OUT_DIR="${1#*=}"
            shift
            ;;
        --sqlancer-jar)
            SQLANCER_JAR_ARG="${2:?missing value for --sqlancer-jar}"
            shift 2
            ;;
        --sqlancer-jar=*)
            SQLANCER_JAR_ARG="${1#*=}"
            shift
            ;;
        --java-bin)
            JAVA_BIN="${2:?missing value for --java-bin}"
            shift 2
            ;;
        --java-bin=*)
            JAVA_BIN="${1#*=}"
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
        postgres|postgresql|mysql|mariadb|clickhouse|sqlite|sqlite3|duckdb)
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
    postgres|postgresql)
        DBMS="postgres"
        SQLANCER_SUBCMD="postgres"
        IS_EMBEDDED=0
        ;;
    mysql)
        SQLANCER_SUBCMD="mysql"
        IS_EMBEDDED=0
        ;;
    mariadb)
        SQLANCER_SUBCMD="mariadb"
        IS_EMBEDDED=0
        ;;
    clickhouse)
        SQLANCER_SUBCMD="clickhouse"
        IS_EMBEDDED=0
        ;;
    sqlite|sqlite3)
        DBMS="sqlite"
        SQLANCER_SUBCMD="sqlite3"
        IS_EMBEDDED=1
        ;;
    duckdb)
        SQLANCER_SUBCMD="duckdb"
        IS_EMBEDDED=1
        ;;
    "")
        echo "Error: --dbms (or positional DBMS) is required." >&2
        usage >&2
        exit 2
        ;;
    *)
        echo "Error: unsupported DBMS '$DBMS' (expected postgres, mysql, mariadb, clickhouse, sqlite, or duckdb)." >&2
        exit 2
        ;;
esac

if [[ ! "$NUM_THREADS" =~ ^[1-9][0-9]*$ ]]; then
    echo "Error: --num-threads must be a positive integer: $NUM_THREADS" >&2
    exit 2
fi

if [[ -n "$TIMEOUT_SECONDS" ]] && [[ ! "$TIMEOUT_SECONDS" =~ ^[1-9][0-9]*$ ]]; then
    echo "Error: --timeout-seconds must be a positive integer: $TIMEOUT_SECONDS" >&2
    exit 2
fi

# Resolve DBMS-aware defaults after parsing so unrelated env vars never leak across DBMSes.
if [[ "$IS_EMBEDDED" -eq 0 ]]; then
    case "$DBMS" in
        postgres)
            HOST="${HOST:-${PGHOST:-127.0.0.1}}"
            if [[ "$HOST" == /* ]]; then
                HOST="127.0.0.1"
            fi
            PORT="${PORT:-${PGPORT:-5432}}"
            DB_USER="${DB_USER:-${PGUSER:-postgres}}"
            if [[ "$PASSWORD_SET" -eq 0 ]]; then
                PASSWORD="${PGPASSWORD:-}"
            fi
            ;;
        mysql)
            HOST="${HOST:-${MYSQL_HOST:-127.0.0.1}}"
            PORT="${PORT:-${MYSQL_TCP_PORT:-3306}}"
            DB_USER="${DB_USER:-${MYSQL_USER:-root}}"
            if [[ "$PASSWORD_SET" -eq 0 ]]; then
                PASSWORD="${MYSQL_PWD:-}"
            fi
            ;;
        mariadb)
            HOST="${HOST:-${MARIADB_HOST:-${MYSQL_HOST:-127.0.0.1}}}"
            PORT="${PORT:-${MARIADB_TCP_PORT:-${MYSQL_TCP_PORT:-3306}}}"
            DB_USER="${DB_USER:-${MARIADB_USER:-${MYSQL_USER:-root}}}"
            if [[ "$PASSWORD_SET" -eq 0 ]]; then
                PASSWORD="${MARIADB_PWD:-${MYSQL_PWD:-}}"
            fi
            ;;
        clickhouse)
            HOST="${HOST:-${CLICKHOUSE_HOST:-127.0.0.1}}"
            # SQLancer ClickHouse JDBC connects over HTTP (default 8123), unlike clickhouse-client TCP (9000).
            PORT="${PORT:-${CLICKHOUSE_HTTP_PORT:-8123}}"
            DB_USER="${DB_USER:-${CLICKHOUSE_USER:-default}}"
            if [[ "$PASSWORD_SET" -eq 0 ]]; then
                PASSWORD="${CLICKHOUSE_PASSWORD:-}"
            fi
            ;;
    esac
fi

resolve_sqlancer_jar() {
    local candidate
    if [[ -n "$SQLANCER_JAR_ARG" ]]; then
        echo "$SQLANCER_JAR_ARG"
        return 0
    fi
    if [[ -n "${SQLANCER_JAR:-}" ]]; then
        echo "$SQLANCER_JAR"
        return 0
    fi
    for candidate in \
        "${SQLANCER_HOME:-/opt/sqlancer}/sqlancer.jar" \
        "/usr/local/share/sqlancer/sqlancer.jar" \
        "./target"/sqlancer-*.jar; do
        case "$candidate" in
            *-sources.jar|*-javadoc.jar|*-tests.jar)
                continue
                ;;
        esac
        if [[ -f "$candidate" ]]; then
            echo "$candidate"
            return 0
        fi
    done
    return 1
}

RESOLVED_JAR=""
if ! RESOLVED_JAR="$(resolve_sqlancer_jar)"; then
    cat >&2 <<'EOF'
Error: SQLancer JAR not found. Pass --sqlancer-jar PATH or set SQLANCER_JAR.
Build SQLancer via:
  docker build -t occam-fuzzer-sqlancer -f harness/Dockerfile.fuzzer-sqlancer .
or locally:
  git clone https://github.com/sqlancer/sqlancer && cd sqlancer && mvn package -DskipTests
EOF
    exit 1
fi

if [[ ! -f "$RESOLVED_JAR" ]]; then
    echo "Error: SQLancer JAR does not exist: $RESOLVED_JAR" >&2
    exit 1
fi

if [[ "$DRY_RUN" -eq 0 ]] && ! command -v "$JAVA_BIN" >/dev/null 2>&1; then
    echo "Error: Java runtime '$JAVA_BIN' not found in PATH." >&2
    exit 1
fi

if [[ -z "$OUT_DIR" ]]; then
    OUT_DIR="/tmp/sqlancer_${DBMS}_logs"
fi
mkdir -p "$OUT_DIR"

# SQLancer MainOptions must precede the DBMS subcommand.
GLOBAL_ARGS=("--num-threads" "$NUM_THREADS" "--log-each-select=true")
if [[ "$IS_EMBEDDED" -eq 0 ]]; then
    # Always pass --password explicitly so MainOptions does not fall back to default "sqlancer".
    GLOBAL_ARGS+=("--host" "$HOST" "--port" "$PORT" "--username" "$DB_USER" "--password" "$PASSWORD")
fi
if [[ -n "$NUM_TRIES" ]]; then
    GLOBAL_ARGS+=("--num-tries" "$NUM_TRIES")
fi
if [[ -n "$NUM_QUERIES" ]]; then
    GLOBAL_ARGS+=("--num-queries" "$NUM_QUERIES")
fi
if [[ -n "$MAX_GENERATED_DBS" ]]; then
    GLOBAL_ARGS+=("--max-generated-databases" "$MAX_GENERATED_DBS")
fi
if [[ -n "$TIMEOUT_SECONDS" ]]; then
    GLOBAL_ARGS+=("--timeout-seconds" "$TIMEOUT_SECONDS")
fi
if [[ -n "$RANDOM_SEED" ]]; then
    GLOBAL_ARGS+=("--random-seed" "$RANDOM_SEED")
fi
if [[ -n "$DATABASE_PREFIX" ]]; then
    GLOBAL_ARGS+=("--database-prefix" "$DATABASE_PREFIX")
fi
if [[ -n "$EXIT_CODE_ERROR" ]]; then
    GLOBAL_ARGS+=("--exit-code-error" "$EXIT_CODE_ERROR")
fi

SUBCMD_ARGS=()
if [[ -n "$ORACLE" ]]; then
    SUBCMD_ARGS+=("--oracle" "$ORACLE")
fi
if [[ ${#EXTRA_ARGS[@]} -gt 0 ]]; then
    SUBCMD_ARGS+=("${EXTRA_ARGS[@]}")
fi

CMD=("$JAVA_BIN" "-jar" "$RESOLVED_JAR" "${GLOBAL_ARGS[@]}" "$SQLANCER_SUBCMD" "${SUBCMD_ARGS[@]}")

if [[ "$DRY_RUN" -eq 1 ]]; then
    printf 'DBMS=%s\nOUT_DIR=%s\n' "$DBMS" "$OUT_DIR"
    printf 'CMD='
    printf '%q ' "${CMD[@]}"
    printf '\n'
    exit 0
fi

# Upstream PostgresProvider connects initially to database "test" regardless of --connection-url.
if [[ "$DBMS" == "postgres" ]] && command -v psql >/dev/null 2>&1; then
    PGPASSWORD="$PASSWORD" psql -h "$HOST" -p "$PORT" -U "$DB_USER" -d postgres -tAc \
        "SELECT 1 FROM pg_database WHERE datname='test'" 2>/dev/null | grep -qx '1' || \
        PGPASSWORD="$PASSWORD" psql -h "$HOST" -p "$PORT" -U "$DB_USER" -d postgres -v ON_ERROR_STOP=1 -c \
            "CREATE DATABASE test;" >/dev/null 2>&1 || true
fi

TIMESTAMP="$(date +%Y%m%d_%H%M%S)"
RUN_LOG="${OUT_DIR}/sqlancer_${DBMS}_${TIMESTAMP}.log"

echo "==> Running SQLancer ($SQLANCER_SUBCMD) with $NUM_THREADS thread(s)..."
if [[ "$IS_EMBEDDED" -eq 0 ]]; then
    echo "==> Target: $HOST:$PORT (user=$DB_USER)"
fi
echo "==> Streaming logs and SQLancer logs/ into: $OUT_DIR"

set +e
(
    cd "$OUT_DIR" || exit 1
    "${CMD[@]}" 2>&1 | tee "$RUN_LOG"
    exit "${PIPESTATUS[0]}"
)
status=$?
set -e

if [[ "$status" -ne 0 ]]; then
    echo "==> SQLancer exited with status $status (logs saved in $OUT_DIR)" >&2
    exit "$status"
fi

echo "==> SQLancer completed cleanly. Logs saved in $OUT_DIR"

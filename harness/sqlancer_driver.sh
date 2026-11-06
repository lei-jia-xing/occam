#!/usr/bin/env bash
# ==============================================================================
# Occam Upstream Fuzz Driver: SQLancer
# Supports PostgreSQL and MySQL targets and records the fuzzer's execution log.
# ==============================================================================
set -euo pipefail

SQLANCER_PIN="a7d60ee450e6a22ca38599c14e984a49773f4745"
SQLANCER_SOURCE="${SQLANCER_SOURCE:-${XDG_CACHE_HOME:-$HOME/.cache}/occam/sqlancer-$SQLANCER_PIN}"

for tool in java git mvn; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        echo "Error: SQLancer requires $tool (Java 11+, Git, and Maven)." >&2
        exit 1
    fi
done

JAVA_VER=$(java -version 2>&1 | awk -F '"' '/version/ {print $2}')
JAVA_MAJOR=$(echo "$JAVA_VER" | awk -F '.' '{if ($1 == "1") print $2; else print $1}')
if [[ -z "$JAVA_MAJOR" || "$JAVA_MAJOR" -lt 11 ]]; then
    echo "Error: Java 11+ is required, but detected Java version ${JAVA_VER:-unknown}." >&2
    exit 1
fi

if [[ ! -d "$SQLANCER_SOURCE/.git" ]]; then
    mkdir -p "$(dirname "$SQLANCER_SOURCE")"
    git init -q "$SQLANCER_SOURCE"
    git -C "$SQLANCER_SOURCE" remote add origin https://github.com/sqlancer/sqlancer.git
    git -C "$SQLANCER_SOURCE" fetch -q --depth 1 origin "$SQLANCER_PIN"
    git -C "$SQLANCER_SOURCE" checkout -q --detach FETCH_HEAD
fi
actual_pin=$(git -C "$SQLANCER_SOURCE" rev-parse HEAD 2>/dev/null || true)
if [[ "$actual_pin" != "$SQLANCER_PIN" ]]; then
    echo "Error: SQLANCER_SOURCE must be pinned at $SQLANCER_PIN (found ${actual_pin:-not a git checkout})." >&2
    exit 1
fi

mvn -q -DskipTests package -f "$SQLANCER_SOURCE/pom.xml"
SQLANCER_JAR="$SQLANCER_SOURCE/target/sqlancer-2.0.0.jar"
if [[ ! -f "$SQLANCER_JAR" ]]; then
    echo "Error: Maven did not produce $SQLANCER_JAR." >&2
    exit 1
fi

# 3. Parse arguments: <dialect> [--queries N] [--out-dir DIR]
DIALECT=""
QUERIES="${QUERIES:-1000}"
OUT_DIR="${OUT_DIR:-}"
EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        postgres|mysql)
            DIALECT="$1"
            shift
            ;;
        --queries)
            QUERIES="$2"
            shift 2
            ;;
        --queries=*)
            QUERIES="${1#*=}"
            shift
            ;;
        --out-dir)
            OUT_DIR="$2"
            shift 2
            ;;
        --out-dir=*)
            OUT_DIR="${1#*=}"
            shift
            ;;
        -h|--help)
            echo "Usage: $0 <postgres|mysql> [--queries N] [--out-dir DIR] [extra sqlancer args...]"
            exit 0
            ;;
        *)
            EXTRA_ARGS+=("$1")
            shift
            ;;
    esac
done

if [[ -z "$DIALECT" ]]; then
    echo "Error: dialect is required ('postgres' or 'mysql')." >&2
    echo "Usage: $0 <postgres|mysql> [--queries N] [--out-dir DIR]" >&2
    exit 2
fi

if [[ -z "$OUT_DIR" ]]; then
    OUT_DIR="/tmp/sqlancer_${DIALECT}_logs"
fi
mkdir -p "$OUT_DIR"

# 4. Configure connection parameters
if [[ "$DIALECT" == "postgres" ]]; then
    HOST="${PGHOST:-127.0.0.1}"
    if [[ "$HOST" == /* ]]; then
        HOST="127.0.0.1"
    fi
    PORT="${PGPORT:-5432}"
    DB="${PGDATABASE:-occam_test}"
    USER="${PGUSER:-${USER:-postgres}}"
    PASS="${PGPASSWORD:-}"
elif [[ "$DIALECT" == "mysql" ]]; then
    HOST="${MYSQL_HOST:-127.0.0.1}"
    PORT="${MYSQL_PORT:-3306}"
    DB="${MYSQL_DATABASE:-occam_test}"
    USER="${MYSQL_USER:-root}"
    PASS="${MYSQL_PWD:-${MYSQL_PASSWORD:-}}"
fi

DIALECT_ARGS=("--host" "$HOST" "--port" "$PORT" "--database" "$DB" "--username" "$USER")
if [[ -n "$PASS" ]]; then
    DIALECT_ARGS+=("--password" "$PASS")
fi

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_FILE="$OUT_DIR/sqlancer_${DIALECT}_${TIMESTAMP}.log"

echo "==> Running SQLancer ($DIALECT) for $QUERIES queries..."
echo "==> Host: $HOST:$PORT | Database: $DB | User: $USER"
echo "==> Capturing logs to: $LOG_FILE"

# 5. Run SQLancer and record its output.
JAVA_CMD=("java")
if [[ -n "${SQLANCER_CLASSPATH:-}" ]]; then
    JAVA_CMD+=("-cp" "$SQLANCER_CLASSPATH:$SQLANCER_JAR" "sqlancer.Main")
else
    JAVA_CMD+=("-jar" "$SQLANCER_JAR")
fi

set +e
"${JAVA_CMD[@]}" --num-queries "$QUERIES" "$DIALECT" "${DIALECT_ARGS[@]}" "${EXTRA_ARGS[@]}" 2>&1 | tee "$LOG_FILE"
status=${PIPESTATUS[0]}
set -e

if [[ $status -ne 0 ]]; then
    echo "==> SQLancer exited with status $status (logs saved to $OUT_DIR)" >&2
    exit "$status"
fi

echo "==> SQLancer completed successfully. Logs saved to $LOG_FILE"

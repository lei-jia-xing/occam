#!/usr/bin/env bash
# ==============================================================================
# Occam Upstream Fuzz Driver: SQLancer
# Supports PostgreSQL and MySQL targets, capturing crashes and execution logs.
# ==============================================================================
set -eo pipefail

# 1. Verify Java 17+
if ! command -v java >/dev/null 2>&1; then
    echo "Error: java command not found in PATH. Java 17+ is required." >&2
    exit 1
fi

JAVA_VER=$(java -version 2>&1 | awk -F '"' '/version/ {print $2}')
JAVA_MAJOR=$(echo "$JAVA_VER" | awk -F '.' '{if ($1 == "1") print $2; else print $1}')
if [[ -z "$JAVA_MAJOR" || "$JAVA_MAJOR" -lt 17 ]]; then
    echo "Error: Java 17+ is required, but detected Java version $JAVA_VER." >&2
    exit 1
fi

# 2. Locate sqlancer.jar
SQLANCER_JAR="${SQLANCER_JAR:-}"
if [[ -n "$SQLANCER_JAR" && -f "$SQLANCER_JAR" ]]; then
    :
elif [[ -f "/opt/sqlancer/sqlancer.jar" ]]; then
    SQLANCER_JAR="/opt/sqlancer/sqlancer.jar"
elif [[ -f "$HOME/.sqlancer/sqlancer.jar" ]]; then
    SQLANCER_JAR="$HOME/.sqlancer/sqlancer.jar"
elif [[ -f "./sqlancer.jar" ]]; then
    SQLANCER_JAR="./sqlancer.jar"
elif [[ -f "./target/sqlancer.jar" ]]; then
    SQLANCER_JAR="./target/sqlancer.jar"
else
    found=$(find /opt "$HOME/.sqlancer" . -maxdepth 3 -name "sqlancer*.jar" 2>/dev/null | head -n 1 || true)
    if [[ -n "$found" ]]; then
        SQLANCER_JAR="$found"
    fi
fi

if [[ -z "$SQLANCER_JAR" || ! -f "$SQLANCER_JAR" ]]; then
    echo "sqlancer.jar not found. Attempting to download..." >&2
    mkdir -p "$HOME/.sqlancer"
    if curl -fsSL -o "$HOME/.sqlancer/sqlancer.jar" "https://github.com/sqlancer/sqlancer/releases/download/v2.0.0/sqlancer-2.0.0.jar" 2>/dev/null; then
        SQLANCER_JAR="$HOME/.sqlancer/sqlancer.jar"
    fi
fi

if [[ -z "$SQLANCER_JAR" || ! -f "$SQLANCER_JAR" ]]; then
    cat >&2 <<'EOF'
Error: sqlancer.jar not found.
Checked locations:
  - $SQLANCER_JAR
  - /opt/sqlancer/sqlancer.jar
  - ~/.sqlancer/sqlancer.jar
  - ./sqlancer.jar

To install SQLancer:
  mkdir -p ~/.sqlancer
  curl -L -o ~/.sqlancer/sqlancer.jar https://github.com/sqlancer/sqlancer/releases/download/v2.0.0/sqlancer-2.0.0.jar
Or build from source:
  git clone https://github.com/sqlancer/sqlancer.git
  cd sqlancer && mvn package -DskipTests
  cp target/sqlancer-*.jar ~/.sqlancer/sqlancer.jar
EOF
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

# 5. Run SQLancer and capture crashes / logs
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

# Capture any crash reports or logs generated in workdir
for crash_src in logs/ target/logs/; do
    if [[ -d "$crash_src" ]]; then
        cp -r "$crash_src"* "$OUT_DIR/" 2>/dev/null || true
    fi
done

if [[ $status -ne 0 ]]; then
    echo "==> SQLancer exited with status $status (logs saved to $OUT_DIR)" >&2
    exit "$status"
fi

echo "==> SQLancer completed successfully. Logs saved to $LOG_FILE"

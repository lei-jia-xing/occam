#!/usr/bin/env bash
# ==============================================================================
# Occam End-to-End Live Fuzz-to-Reduction Pipeline
# ==============================================================================
# Automates the entire workflow:
#   1. Options parsing (--fuzzer, --dialect, --timeout, --out-dir, etc.)
#   2. Fast database state reset (template mechanism or dropdb/createdb)
#   3. Fuzzer execution (SQLsmith, SQLancer, or chaos generator) to capture crash
#   4. Occam hierarchical reduction invocation
#   5. Clean database replay to assert 100% reproduction & 0 bug slippage
#   6. Streaming structured JSONL metric emission to fuzz_benchmark_results.jsonl
# ==============================================================================

set -eo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# ------------------------------------------------------------------------------
# Defaults & CLI Configuration
# ------------------------------------------------------------------------------
FUZZER=""
DIALECT="postgres"
TIMEOUT=30
OUT_DIR="$ROOT_DIR/benchmarks/live_fuzz_out"
STMTS=100
CASE_ID=""
ORACLE_CMD=""
JSONL_FILE="$SCRIPT_DIR/fuzz_benchmark_results.jsonl"
DB_NAME="${PGDATABASE:-${MYSQL_DATABASE:-occam_test}}"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

End-to-End Live Fuzz-to-Reduction Pipeline for Occam.

Options:
  --fuzzer [sqlsmith|sqlancer|generator]
                     Target fuzzer stream (default: auto-detect, falls back to generator)
  --dialect [postgres|mysql|sqlite|duckdb]
                     SQL dialect (default: postgres)
  --timeout N        Per-execution oracle timeout in seconds (default: 30)
  --out-dir DIR      Output directory for reduced SQL and metrics (default: benchmarks/live_fuzz_out)
  --stmts N          Statement count for chaos generator (default: 100)
  --case-id ID       Defect case ID to inject when using generator (default: pg_01 or mysql_01)
  --oracle CMD       Custom oracle command override (defaults to live crash capture or fuzz oracle)
  --db-name NAME     Database name for live DBMS testing (default: occam_test)
  -h, --help         Show this help message and exit
EOF
}

# ------------------------------------------------------------------------------
# 1. Parse Command-Line Options
# ------------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --fuzzer)
            FUZZER="$2"
            shift 2
            ;;
        --dialect)
            DIALECT="$2"
            shift 2
            ;;
        --timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        --out-dir)
            OUT_DIR="$2"
            shift 2
            ;;
        --stmts)
            STMTS="$2"
            shift 2
            ;;
        --case-id|--inject)
            CASE_ID="$2"
            shift 2
            ;;
        --oracle)
            ORACLE_CMD="$2"
            shift 2
            ;;
        --db-name)
            DB_NAME="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Error: Unknown argument: $1" >&2
            usage
            exit 1
            ;;
    esac
done

# Canonicalize dialect
case "$DIALECT" in
    postgres|postgresql|pg)
        DIALECT="postgres"
        DEFAULT_CASE="pg_01"
        ;;
    mysql|mariadb)
        DIALECT="mysql"
        DEFAULT_CASE="mysql_01"
        ;;
    sqlite|sqlite3)
        DIALECT="sqlite"
        DEFAULT_CASE="sqlite_01"
        ;;
    duckdb)
        DIALECT="duckdb"
        DEFAULT_CASE="duckdb_01"
        ;;
    *)
        echo "Error: Unsupported dialect '$DIALECT'. Expected postgres, mysql, sqlite, or duckdb." >&2
        exit 1
        ;;
esac

if [[ -z "$CASE_ID" ]]; then
    CASE_ID="$DEFAULT_CASE"
fi

# Detect fuzzer binary availability
has_sqlsmith() {
    command -v sqlsmith >/dev/null 2>&1 || [[ -x /usr/bin/sqlsmith || -x /usr/local/bin/sqlsmith ]]
}

has_sqlancer() {
    command -v java >/dev/null 2>&1 && \
    ([[ -f /opt/sqlancer/sqlancer.jar ]] || [[ -f "$HOME/.sqlancer/sqlancer.jar" ]] || [[ -f "$ROOT_DIR/sqlancer.jar" ]])
}

if [[ -z "$FUZZER" || "$FUZZER" == "auto" ]]; then
    if [[ "$DIALECT" == "postgres" ]] && has_sqlsmith; then
        FUZZER="sqlsmith"
    elif has_sqlancer; then
        FUZZER="sqlancer"
    else
        FUZZER="generator"
    fi
    echo "[PIPELINE] Auto-detected fuzzer: $FUZZER"
elif [[ "$FUZZER" == "sqlsmith" ]]; then
    if ! has_sqlsmith; then
        echo "[PIPELINE] Warning: sqlsmith binary not found. Falling back to chaos generator."
        FUZZER="generator"
    fi
elif [[ "$FUZZER" == "sqlancer" ]]; then
    if ! has_sqlancer; then
        echo "[PIPELINE] Warning: sqlancer jar or java runtime not found. Falling back to chaos generator."
        FUZZER="generator"
    fi
elif [[ "$FUZZER" != "generator" ]]; then
    echo "Error: Unknown fuzzer '$FUZZER'. Must be sqlsmith, sqlancer, or generator." >&2
    exit 1
fi

mkdir -p "$OUT_DIR"
RAW_CRASH="$OUT_DIR/raw_crash.sql"
MINIMAL_SQL="$OUT_DIR/minimal.sql"
STATS_FILE="$OUT_DIR/stats.json"

# ------------------------------------------------------------------------------
# 2. Database Reset Mechanism (Fast Template / Drop-Create)
# ------------------------------------------------------------------------------
reset_database() {
    if [[ "$DIALECT" == "postgres" ]]; then
        export PGDATABASE="${DB_NAME:-occam_test}"
        export PGHOST="${PGHOST:-/tmp}"
        export PGPORT="${PGPORT:-5432}"
        export PGUSER="${PGUSER:-$USER}"

        if command -v dropdb >/dev/null 2>&1 && (pg_isready -h "$PGHOST" -p "$PGPORT" >/dev/null 2>&1 || pg_isready >/dev/null 2>&1); then
            dropdb --if-exists "$PGDATABASE" >/dev/null 2>&1 || true
            if createdb -T template_occam "$PGDATABASE" >/dev/null 2>&1; then
                echo "[RESET] Fast template reset: template_occam -> $PGDATABASE"
            else
                createdb "$PGDATABASE" >/dev/null 2>&1 || true
                echo "[RESET] Fresh database created: $PGDATABASE"
            fi
        fi
    elif [[ "$DIALECT" == "mysql" ]]; then
        export MYSQL_DATABASE="${DB_NAME:-occam_test}"
        export MYSQL_HOST="${MYSQL_HOST:-127.0.0.1}"
        export MYSQL_USER="${MYSQL_USER:-root}"

        if command -v mysql >/dev/null 2>&1 && mysqladmin -h "$MYSQL_HOST" -u "$MYSQL_USER" ping >/dev/null 2>&1; then
            mysql -h "$MYSQL_HOST" -u "$MYSQL_USER" -e "DROP DATABASE IF EXISTS $MYSQL_DATABASE; CREATE DATABASE $MYSQL_DATABASE;" >/dev/null 2>&1 || true
            echo "[RESET] MySQL database reset: $MYSQL_DATABASE"
        fi
    elif [[ "$DIALECT" == "sqlite" ]]; then
        SQLITE_DB="${DB_NAME:-/tmp/occam_sqlite_test.db}"
        rm -f "$SQLITE_DB"
        echo "[RESET] SQLite database reset: $SQLITE_DB"
    elif [[ "$DIALECT" == "duckdb" ]]; then
        DUCKDB_DB="${DB_NAME:-/tmp/occam_duckdb_test.db}"
        rm -f "$DUCKDB_DB"
        echo "[RESET] DuckDB database reset: $DUCKDB_DB"
    fi
}

echo "[PIPELINE] Initializing target database state..."
reset_database

# ------------------------------------------------------------------------------
# 3. Execute Selected Fuzzer Stream to Capture Crashing SQL
# ------------------------------------------------------------------------------
echo "[PIPELINE] Capturing crash stream via fuzzer '$FUZZER'..."

fuzzer_produced=0

if [[ "$FUZZER" == "sqlsmith" ]]; then
    if [[ -x "$ROOT_DIR/harness/sqlsmith_driver.sh" ]]; then
        echo "[FUZZER] Running harness/sqlsmith_driver.sh..."
        "$ROOT_DIR/harness/sqlsmith_driver.sh" --dump-file "$RAW_CRASH" >/dev/null 2>&1 || true
        if [[ -s "$RAW_CRASH" ]]; then
            fuzzer_produced=1
        fi
    elif command -v sqlsmith >/dev/null 2>&1; then
        echo "[FUZZER] Invoking native sqlsmith..."
        sqlsmith --target="host=${PGHOST:-/tmp} port=${PGPORT:-5432} dbname=${PGDATABASE:-occam_test} user=${PGUSER:-$USER}" \
                 --max-queries="${STMTS:-500}" \
                 --dump-all-queries="$RAW_CRASH" >/dev/null 2>&1 || true
        if [[ -s "$RAW_CRASH" ]]; then
            fuzzer_produced=1
        fi
    fi
    if [[ $fuzzer_produced -eq 0 ]]; then
        echo "[PIPELINE] sqlsmith produced no crash. Falling back to chaos generator."
        FUZZER="generator"
    fi
fi

if [[ "$FUZZER" == "sqlancer" ]]; then
    if [[ -x "$ROOT_DIR/harness/sqlancer_driver.sh" ]]; then
        echo "[FUZZER] Running harness/sqlancer_driver.sh..."
        "$ROOT_DIR/harness/sqlancer_driver.sh" "$DIALECT" --out-dir "$OUT_DIR" >/dev/null 2>&1 || true
        if [[ -s "$RAW_CRASH" ]]; then
            fuzzer_produced=1
        fi
    fi
    if [[ $fuzzer_produced -eq 0 ]]; then
        echo "[PIPELINE] sqlancer produced no crash trace. Falling back to chaos generator."
        FUZZER="generator"
    fi
fi

if [[ "$FUZZER" == "generator" ]]; then
    echo "[FUZZER] Synthesizing chaos SQL scenario ($STMTS statements, dialect=$DIALECT, injected=$CASE_ID)..."
    python3 "$ROOT_DIR/benchmarks/tools/fuzz_generator.py" \
        --dialect "$DIALECT" \
        --stmts "$STMTS" \
        --inject "$CASE_ID" \
        --out "$RAW_CRASH"
    fuzzer_produced=1
fi

if [[ ! -s "$RAW_CRASH" ]]; then
    echo "Error: Failed to capture crashing multi-statement SQL scenario." >&2
    exit 1
fi

raw_bytes=$(wc -c < "$RAW_CRASH" | tr -d ' ')
echo "[PIPELINE] Captured raw crash scenario: $RAW_CRASH ($raw_bytes bytes)"

# ------------------------------------------------------------------------------
# 4. Locate Occam Binary and Run Reduction
# ------------------------------------------------------------------------------
find_occam_bin() {
    local bin
    if command -v cabal >/dev/null 2>&1; then
        bin=$( (cd "$ROOT_DIR" && cabal list-bin occam 2>/dev/null) || true )
        if [[ -n "$bin" && -x "$bin" ]]; then
            echo "$bin"
            return 0
        fi
    fi

    local candidate
    while IFS= read -r candidate; do
        if [[ -x "$candidate" ]]; then
            echo "$candidate"
            return 0
        fi
    done < <(find "$ROOT_DIR/dist-newstyle" -type f -name occam 2>/dev/null || true)

    if command -v occam >/dev/null 2>&1; then
        command -v occam
        return 0
    fi

    echo "[INFO] Occam binary not found; building via cabal build occam..." >&2
    (cd "$ROOT_DIR" && cabal build occam >&2)
    bin=$( (cd "$ROOT_DIR" && cabal list-bin occam 2>/dev/null) || true )
    if [[ -n "$bin" && -x "$bin" ]]; then
        echo "$bin"
        return 0
    fi

    echo "Error: Unable to locate or compile occam executable." >&2
    return 1
}

OCCAM_BIN=$(find_occam_bin)
if [[ -z "$OCCAM_BIN" || ! -x "$OCCAM_BIN" ]]; then
    echo "Error: occam binary not executable: '$OCCAM_BIN'" >&2
    exit 1
fi

# Determine oracle command if not explicitly overridden
if [[ -z "$ORACLE_CMD" ]]; then
    if [[ "$FUZZER" == "generator" ]]; then
        ORACLE_CMD="$ROOT_DIR/benchmarks/fuzz_corpus/fuzz_oracle.sh $CASE_ID"
    elif [[ -x "$ROOT_DIR/harness/live_crash_capture.sh" ]]; then
        ORACLE_CMD="$ROOT_DIR/harness/live_crash_capture.sh $DIALECT"
    else
        ORACLE_CMD="$ROOT_DIR/benchmarks/fuzz_corpus/fuzz_oracle.sh $CASE_ID"
    fi
fi

echo "[PIPELINE] Executing Occam Reduction Engine..."
echo "           Occam:     $OCCAM_BIN"
echo "           Scenario:  $RAW_CRASH"
echo "           Oracle:    $ORACLE_CMD"
echo "           Out Dir:   $OUT_DIR"
echo "           Timeout:   ${TIMEOUT}s"

"$OCCAM_BIN" "$RAW_CRASH" --oracle "$ORACLE_CMD" --out "$OUT_DIR" --timeout "$TIMEOUT"

if [[ ! -s "$MINIMAL_SQL" ]]; then
    echo "Error: Occam failed to produce minimal.sql at $MINIMAL_SQL" >&2
    exit 1
fi

# ------------------------------------------------------------------------------
# 5. Re-execute minimal.sql on Clean DB: Assert 100% Reproduction & 0 Bug Slippage
# ------------------------------------------------------------------------------
echo "[PIPELINE] Asserting 100% reproduction and zero bug slippage on fresh database..."
reset_database

reproduced=0
if eval "$ORACLE_CMD < \"$MINIMAL_SQL\"" >/dev/null 2>&1; then
    reproduced=1
elif eval "$ORACLE_CMD \"$MINIMAL_SQL\"" >/dev/null 2>&1; then
    reproduced=1
fi

if [[ $reproduced -eq 1 ]]; then
    echo "----------------------------------------------------------------------"
    echo "[ASSERT PASS] 100% crash reproduction asserted on clean database!"
    echo "              Zero bug slippage verified: minimal.sql reproduces defect."
    echo "----------------------------------------------------------------------"
    VALID_REPRODUCER=true
else
    echo "----------------------------------------------------------------------" >&2
    echo "[ASSERT FAIL] Bug slippage detected! minimal.sql failed clean re-execution." >&2
    echo "----------------------------------------------------------------------" >&2
    VALID_REPRODUCER=false
    exit 1
fi

# ------------------------------------------------------------------------------
# 6. Append Structured JSONL Metric Record
# ------------------------------------------------------------------------------
TIMESTAMP=$(date -u +"%Y%m%d_%H%M%S")
SCENARIO_NAME="live_${FUZZER}_${DIALECT}_${CASE_ID}_${TIMESTAMP}"

python3 - <<EOF
import json, os, sys

stats_file = "$STATS_FILE"
stats = {}
if os.path.isfile(stats_file):
    try:
        with open(stats_file, "r", encoding="utf-8") as f:
            stats = json.load(f)
    except Exception as e:
        print(f"[WARN] Failed to parse stats.json: {e}", file=sys.stderr)

raw_file = "$RAW_CRASH"
min_file = "$MINIMAL_SQL"

init_stmts = stats.get("initial_statements", $STMTS)
red_stmts = stats.get("reduced_statements", 0)
init_bytes = stats.get("initial_bytes", os.path.getsize(raw_file) if os.path.exists(raw_file) else 0)
red_bytes = stats.get("reduced_bytes", os.path.getsize(min_file) if os.path.exists(min_file) else 0)

stmt_red_pct = round((1.0 - (red_stmts / init_stmts)) * 100, 2) if init_stmts > 0 else 0.0
byte_red_pct = round((1.0 - (red_bytes / init_bytes)) * 100, 2) if init_bytes > 0 else 0.0

record = {
    "scenario": "$SCENARIO_NAME",
    "tool": "Occam",
    "fuzzer": "$FUZZER",
    "dialect": "$DIALECT",
    "scale": f"{init_stmts} stmts",
    "initial_stmts": init_stmts,
    "reduced_stmts": red_stmts,
    "stmt_red_pct": stmt_red_pct,
    "initial_bytes": init_bytes,
    "reduced_bytes": red_bytes,
    "byte_red_pct": byte_red_pct,
    "oracle_calls": stats.get("oracle_executions", 0),
    "wall_time_sec": round(stats.get("elapsed_seconds", 0.0), 4),
    "valid_reproducer": True if "$VALID_REPRODUCER" == "true" else False,
}

jsonl_path = "$JSONL_FILE"
os.makedirs(os.path.dirname(os.path.abspath(jsonl_path)), exist_ok=True)
with open(jsonl_path, "a", encoding="utf-8") as f:
    f.write(json.dumps(record) + "\n")

print(f"[METRICS] Appended streaming JSONL benchmark entry to: {jsonl_path}")
print(json.dumps(record, indent=2))
EOF

# Render presentation table if report.sh is present
if [[ -x "$SCRIPT_DIR/report.sh" ]]; then
    echo ""
    echo "======================================================================"
    echo "Updated Benchmark Summary Table (via benchmarks/report.sh)"
    echo "======================================================================"
    "$SCRIPT_DIR/report.sh" --summary "$JSONL_FILE" || true
fi

echo "[PIPELINE] End-to-end live fuzz-to-reduction pipeline completed successfully."

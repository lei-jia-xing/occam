#!/usr/bin/env bash
# ==============================================================================
# Occam Unified Live Crash Detector & Report Extractor
# Executes candidate SQL against PostgreSQL or MySQL, traps server crashes / ASan,
# and emits standard Occam CrashReport JSON.
# ==============================================================================
set -eo pipefail

DIALECT=""
CANDIDATE_FILE=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        postgres|mysql|sqlite|sqlite3|duckdb)
            DIALECT="$1"
            shift
            ;;
        --dialect)
            DIALECT="$2"
            shift 2
            ;;
        --dialect=*)
            DIALECT="${1#*=}"
            shift
            ;;
        -h|--help)
            echo "Usage: $0 <candidate_sql_file> <postgres|mysql|sqlite|duckdb>"
            echo "   or: $0 <postgres|mysql|sqlite|duckdb> [candidate_sql_file]"
            exit 0
            ;;
        *)
            if [[ -z "$CANDIDATE_FILE" ]]; then
                CANDIDATE_FILE="$1"
                shift
            else
                echo "Unexpected argument: $1" >&2
                exit 2
            fi
            ;;
    esac
done

if [[ -z "$DIALECT" ]]; then
    echo "Error: dialect is required ('postgres', 'mysql', 'sqlite', or 'duckdb')." >&2
    echo "Usage: $0 <candidate_sql_file> <postgres|mysql|sqlite|duckdb>" >&2
    exit 2
fi

TEMP_FILE=""
if [[ -z "$CANDIDATE_FILE" ]]; then
    if [[ ! -t 0 ]]; then
        TEMP_FILE=$(mktemp /tmp/occam_candidate_XXXXXX.sql)
        cat > "$TEMP_FILE"
        if [[ ! -s "$TEMP_FILE" ]]; then
            rm -f "$TEMP_FILE"
            echo "Error: candidate SQL input is empty." >&2
            echo "Usage: $0 <candidate_sql_file> <postgres|mysql>" >&2
            exit 2
        fi
        CANDIDATE_FILE="$TEMP_FILE"
    else
        echo "Error: candidate SQL file is required (pass as argument or via stdin)." >&2
        echo "Usage: $0 <candidate_sql_file> <postgres|mysql>" >&2
        exit 2
    fi
elif [[ ! -r "$CANDIDATE_FILE" ]]; then
    echo "Error: candidate SQL file '$CANDIDATE_FILE' not found or unreadable." >&2
    exit 2
fi

cleanup() {
    if [[ -n "$TEMP_FILE" && -f "$TEMP_FILE" ]]; then
        rm -f "$TEMP_FILE"
    fi
}
trap cleanup EXIT

# Parse crash details and emit Occam JSON
parse_crash() {
    local output="$1"
    local status="$2"

    local is_asan=0
    local is_sigsegv=0
    local is_sigabrt=0

    local re_asan="(AddressSanitizer|DEADLYSIGNAL|heap-use-after-free|heap-buffer-overflow|stack-buffer-overflow|global-buffer-overflow)"
    if [[ "$output" =~ $re_asan ]]; then
        is_asan=1
    fi

    local re_sigsegv="(signal[[:space:]]+11|Segmentation[[:space:]]+fault|SEGV|ERROR[[:space:]]+2013|CR_SERVER_LOST|server[[:space:]]+closed[[:space:]]+the[[:space:]]+connection[[:space:]]+unexpectedly)"
    if [[ "$status" -eq 139 || "$status" -eq -11 ]] || [[ "$output" =~ $re_sigsegv ]]; then
        is_sigsegv=1
    fi

    local re_sigabrt="(signal[[:space:]]+6|Aborted|FailedAssertion|ExceptionalCondition)"
    if [[ "$status" -eq 134 || "$status" -eq -6 ]] || [[ "$output" =~ $re_sigabrt ]]; then
        is_sigabrt=1
    fi

    local exit_sig=0
    if (( status == 139 || status == -11 )); then
        exit_sig=11
    elif (( status == 134 || status == -6 )); then
        exit_sig=6
    elif (( status < 0 )); then
        exit_sig=$((-status))
    elif (( status > 128 && status < 165 )); then
        exit_sig=$((status - 128))
    fi

    if [[ $exit_sig -eq 11 ]]; then
        is_sigsegv=1
    elif [[ $exit_sig -eq 6 ]]; then
        is_sigabrt=1
    fi

    if [[ $is_asan -eq 0 && $is_sigsegv -eq 0 && $is_sigabrt -eq 0 && $exit_sig -eq 0 ]]; then
        return 1
    fi

    local signal=11
    if [[ $is_sigabrt -eq 1 && $is_sigsegv -eq 0 ]]; then
        signal=6
    elif [[ $exit_sig -ne 0 && $exit_sig -ne 11 ]]; then
        signal="$exit_sig"
    fi

    local func=""
    local re_frame='#[0-9]+[[:space:]]+0x[0-9a-fA-F]+[[:space:]]+in[[:space:]]+([A-Za-z0-9_:]+)'
    local re_asan_type='AddressSanitizer:[[:space:]]*([A-Za-z0-9_-]+)'
    local re_assert='FailedAssertion\("([^"]+)"'

    if [[ "$output" =~ $re_frame ]]; then
        func="${BASH_REMATCH[1]}"
    elif [[ "$output" =~ $re_asan_type ]]; then
        func="${BASH_REMATCH[1]}"
    elif [[ "$output" =~ $re_assert ]]; then
        func="${BASH_REMATCH[1]}"
    elif [[ $signal -eq 6 ]]; then
        func="SIGABRT"
    elif [[ $signal -eq 11 ]]; then
        func="SIGSEGV"
    else
        func="signal_$signal"
    fi

    func="${func//\\/\\\\}"
    func="${func//\"/\\\"}"

    printf '{"kind":"signal","signal":%d,"frames":[{"index":0,"function":"%s"}]}\n' "$signal" "$func"
    return 0
}

output=""
status=0

if [[ "$DIALECT" == "postgres" ]]; then
    PGDATABASE=${PGDATABASE:-occam_test}
    PGHOST=${PGHOST:-/tmp}
    PGPORT=${PGPORT:-5432}
    PGUSER=${PGUSER:-$USER}
    export PGDATABASE PGHOST PGPORT PGUSER

    dropdb --if-exists "$PGDATABASE" 2>/dev/null || true
    createdb "$PGDATABASE" 2>/dev/null || true

    if output=$(psql -v ON_ERROR_STOP=1 -d "$PGDATABASE" -f "$CANDIDATE_FILE" 2>&1); then
        status=0
    else
        status=$?
    fi

    server_logs=$(tail -n 100 /var/log/postgresql/*.log 2>/dev/null || true)
    full_output="${output}"$'\n'"${server_logs}"

elif [[ "$DIALECT" == "mysql" ]]; then
    MYSQL_DATABASE=${MYSQL_DATABASE:-occam_test}
    MYSQL_HOST=${MYSQL_HOST:-127.0.0.1}
    MYSQL_PORT=${MYSQL_PORT:-3306}
    MYSQL_USER=${MYSQL_USER:-root}
    if [[ -n "${MYSQL_PASSWORD:-}" && -z "${MYSQL_PWD:-}" ]]; then
        export MYSQL_PWD="$MYSQL_PASSWORD"
    fi
    export MYSQL_DATABASE MYSQL_HOST MYSQL_PORT MYSQL_USER

    mysql -h "$MYSQL_HOST" -P "$MYSQL_PORT" -u "$MYSQL_USER" -e "DROP DATABASE IF EXISTS $MYSQL_DATABASE; CREATE DATABASE $MYSQL_DATABASE;" 2>/dev/null || true

    if output=$(mysql -h "$MYSQL_HOST" -P "$MYSQL_PORT" -u "$MYSQL_USER" "$MYSQL_DATABASE" < "$CANDIDATE_FILE" 2>&1); then
        status=0
    else
        status=$?
    fi

    server_logs=$(tail -n 100 /var/log/mysql/*.log /var/lib/mysql/*.err 2>/dev/null || true)
    full_output="${output}"$'\n'"${server_logs}"
elif [[ "$DIALECT" == "sqlite" || "$DIALECT" == "sqlite3" ]]; then
    SQLITE_DATABASE=${SQLITE_DATABASE:-/tmp/occam_sqlite_test.db}
    SQLITE_BIN=${SQLITE_BIN:-sqlite3}
    rm -f "$SQLITE_DATABASE"

    if output=$("$SQLITE_BIN" -bail "$SQLITE_DATABASE" < "$CANDIDATE_FILE" 2>&1); then
        status=0
    else
        status=$?
    fi
    full_output="$output"

elif [[ "$DIALECT" == "duckdb" ]]; then
    DUCKDB_DATABASE=${DUCKDB_DATABASE:-/tmp/occam_duckdb_test.db}
    DUCKDB_BIN=${DUCKDB_BIN:-duckdb}
    rm -f "$DUCKDB_DATABASE"

    if output=$("$DUCKDB_BIN" "$DUCKDB_DATABASE" < "$CANDIDATE_FILE" 2>&1); then
        status=0
    else
        status=$?
    fi
    full_output="$output"
fi

if parse_crash "$full_output" "$status"; then
    exit 0
else
    if [[ -n "$output" ]]; then
        echo "$output" >&2
    fi
    exit 1
fi

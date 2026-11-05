#!/usr/bin/env bash
# ==============================================================================
# Occam Upstream Fuzz Driver: SQLsmith
# Targets PostgreSQL using standard connection variables.
# ==============================================================================
set -eo pipefail
for arg in "$@"; do
    if [[ "$arg" == "-h" || "$arg" == "--help" ]]; then
        echo "Usage: $0 [--max-queries N] [--dump-file FILE] [--target CONNINFO] [extra args...]"
        exit 0
    fi
done


# 1. Locate sqlsmith binary
SQLSMITH_BIN=""
if command -v sqlsmith >/dev/null 2>&1; then
    SQLSMITH_BIN="$(command -v sqlsmith)"
elif [[ -x "/usr/bin/sqlsmith" ]]; then
    SQLSMITH_BIN="/usr/bin/sqlsmith"
elif [[ -x "/usr/local/bin/sqlsmith" ]]; then
    SQLSMITH_BIN="/usr/local/bin/sqlsmith"
fi

if [[ -z "$SQLSMITH_BIN" ]]; then
    cat >&2 <<'EOF'
Error: sqlsmith binary not found in PATH, /usr/bin/sqlsmith, or /usr/local/bin/sqlsmith.
To install sqlsmith:
  - Arch Linux: sudo pacman -S sqlsmith
  - Debian/Ubuntu: sudo apt-get install -y sqlsmith
  - From source: git clone https://github.com/anse1/sqlsmith && cd sqlsmith && autoreconf -i && ./configure && make
EOF
    exit 1
fi

# 2. Configure PostgreSQL target connection
if [[ -z "${PGHOST:-}" ]]; then
    if [[ -d "/var/run/postgresql" ]]; then
        PGHOST="/var/run/postgresql"
    else
        PGHOST="/tmp"
    fi
fi
PGPORT="${PGPORT:-5432}"
PGDATABASE="${PGDATABASE:-occam_test}"
PGUSER="${PGUSER:-${USER:-postgres}}"
export PGHOST PGPORT PGDATABASE PGUSER

MAX_QUERIES="${MAX_QUERIES:-1000}"
DUMP_FILE="${DUMP_FILE:-}"
TARGET="host=$PGHOST port=$PGPORT dbname=$PGDATABASE user=$PGUSER"

# 3. Parse CLI overrides
EXTRA_ARGS=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --max-queries=*)
            MAX_QUERIES="${1#*=}"
            shift
            ;;
        --max-queries)
            MAX_QUERIES="$2"
            shift 2
            ;;
        --dump-file=*|--dump-all-queries=*)
            DUMP_FILE="${1#*=}"
            shift
            ;;
        --dump-file|--dump-all-queries)
            DUMP_FILE="$2"
            shift 2
            ;;
        --target=*)
            TARGET="${1#*=}"
            shift
            ;;
        --target)
            TARGET="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [--max-queries N] [--dump-file FILE] [--target CONNINFO] [extra args...]"
            exit 0
            ;;
        *)
            EXTRA_ARGS+=("$1")
            shift
            ;;
    esac
done

CMD=("$SQLSMITH_BIN" "--target=$TARGET" "--max-queries=$MAX_QUERIES")
if [[ -n "$DUMP_FILE" ]]; then
    CMD+=("--dump-all-queries=$DUMP_FILE")
fi
if [[ ${#EXTRA_ARGS[@]} -gt 0 ]]; then
    CMD+=("${EXTRA_ARGS[@]}")
fi

exec "${CMD[@]}"

#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "==> Replaying minimal.sql against oracle..."
./benchmarks/corpus/semantic_oracle.sh mysql_05_multi_row_upsert_deadlock < "$DIR/minimal.sql"

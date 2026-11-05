#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "==> Replaying minimal.sql against oracle..."
./benchmarks/corpus/semantic_oracle.sh pg_09_foreign_table_join_crash < "$DIR/minimal.sql"

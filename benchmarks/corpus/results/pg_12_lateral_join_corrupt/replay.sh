#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "==> Replaying minimal.sql against oracle..."
./benchmarks/corpus/semantic_oracle.sh pg_12_lateral_join_corrupt < "$DIR/minimal.sql"

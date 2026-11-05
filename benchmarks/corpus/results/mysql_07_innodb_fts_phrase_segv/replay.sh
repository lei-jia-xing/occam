#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "==> Replaying minimal.sql against oracle..."
/home/hunter/Workspace/occam/benchmarks/corpus/mock_oracle.sh < "$DIR/minimal.sql"

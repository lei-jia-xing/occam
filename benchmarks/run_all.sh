#!/usr/bin/env bash
set -eo pipefail

# Occam Multi-Tool Benchmark Suite Runner
# Executes multi-tool comparative driver (benchmarks/fuzz_corpus/run_docker_benchmark.py)
# across 18 modern 50~200 statement fuzz scenarios and emits streaming JSONL to
# benchmarks/fuzz_benchmark_results.jsonl.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$ROOT_DIR"

python3 "$SCRIPT_DIR/fuzz_corpus/run_docker_benchmark.py" "$@"
"$SCRIPT_DIR/report.sh" --summary

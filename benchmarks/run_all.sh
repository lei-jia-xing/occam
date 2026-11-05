#!/usr/bin/env bash
set -eo pipefail

# Occam Phase 9 Benchmark Suite Runner
# Iterates through all .sql benchmark testcases and evaluates reducer performance.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS_DIR="$SCRIPT_DIR/results"

mkdir -p "$RESULTS_DIR"

cd "$ROOT_DIR"

echo "======================================================================"
echo "Occam Phase 9: Real-World Crash Corpus Benchmark Suite"
echo "Target Directory: $SCRIPT_DIR"
echo "Results Output:   $RESULTS_DIR"
echo "======================================================================"

shopt -s nullglob
sql_files=("$SCRIPT_DIR"/*.sql)

if [[ ${#sql_files[@]} -eq 0 ]]; then
    echo "Error: No benchmark SQL files found in $SCRIPT_DIR" >&2
    exit 1
fi

total=${#sql_files[@]}
passed=0
failed=0
idx=0

echo "Discovered $total benchmark scenarios."
echo ""

for f in "${sql_files[@]}"; do
    idx=$((idx + 1))
    bench_name="$(basename "$f" .sql)"
    out_dir="./benchmarks/results/$bench_name"

    echo "======================================================================"
    echo "[$idx/$total] Starting Benchmark: $bench_name"
    echo "  Script: $f"
    echo "  Output: $out_dir"
    echo "----------------------------------------------------------------------"

    if cabal run occam-eval -- "$f" --oracle "./harness/pg_harness.sh" --out "./benchmarks/results/$(basename "$f" .sql)"; then
        echo "[$idx/$total] SUCCESS: $bench_name"
        passed=$((passed + 1))
    else
        echo "[$idx/$total] FAILURE: $bench_name" >&2
        failed=$((failed + 1))
    fi
    echo ""
done

echo "======================================================================"
echo "Benchmark Suite Execution Complete"
echo "  Total Benchmarks: $total"
echo "  Passed:           $passed"
echo "  Failed:           $failed"
echo "======================================================================"

if [[ $failed -gt 0 ]]; then
    exit 1
fi

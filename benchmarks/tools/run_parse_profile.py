#!/usr/bin/env python3
"""Quick parse/allocator profiling driver (engine mechanics, not reduction).

Runs each input under a frozen occam binary (default statement-only flags,
exit-0 predicate) and harvests the native cost parse split plus RTS
allocation/peak from stderr. One row per input per repetition; no pairing or
claims here -- A/B pairing happens separately over frozen builds.
Never run timing on CPU0 while a score cohort runs; helpers immutable during runs.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
import sys
import time
from pathlib import Path


def parse_rts(stderr):
    """Extract allocated bytes + max residency from +RTS -s stderr (None if absent)."""
    alloc = residency = None
    for line in stderr.splitlines():
        fields = line.split()
        if "bytes allocated" in line and fields:
            try:
                alloc = int(fields[0].replace(",", ""))
            except ValueError:
                pass
        match = re.search(r"([0-9,]+) bytes maximum residency", line)
        if match:
            residency = int(match.group(1).replace(",", ""))
    return alloc, residency


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--occam", type=Path, required=True)
    parser.add_argument("--inputs", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--timeout", type=float, default=10)
    parser.add_argument("--budget", type=float, default=600)
    parser.add_argument("--repetitions", type=int, default=1)
    args = parser.parse_args()
    if args.repetitions < 1 or args.timeout <= 0 or args.budget <= 0:
        parser.error("repetitions/timeout/budget must be positive")
    occam = args.occam.resolve()
    inputs = sorted(args.inputs.resolve().glob("*.sql"))
    if not inputs:
        parser.error("no .sql inputs found")
    out = args.out.resolve()
    if out.exists() and any(out.iterdir()):
        parser.error("choose a new empty output directory; evidence is never overwritten")
    out.mkdir(parents=True, exist_ok=True)
    rows = []
    for repetition in range(1, args.repetitions + 1):
        for sql_path in inputs:
            child = out / sql_path.stem / str(repetition)
            child.mkdir(parents=True)
            input_bytes = sql_path.read_bytes()
            argv = [str(occam), str(sql_path), "--oracle", "exit 0", "--dialect", "sqlite",
                   "--timeout", str(args.timeout), "--budget", str(args.budget),
                   "--out", str(child / "native"), "--log-format", "json",
                   "+RTS", "-s", "-RTS"]
            (child / "argv.json").write_text(json.dumps(argv) + "\n", encoding="utf-8")
            started = time.monotonic()
            try:
                proc = subprocess.run(argv, capture_output=True, timeout=args.budget + 60)
            except subprocess.TimeoutExpired:
                raise SystemExit(f"profile run overran outer guard: {sql_path.name} rep={repetition}")
            wall = time.monotonic() - started
            (child / "stdout.log").write_bytes(proc.stdout)
            (child / "stderr.log").write_bytes(proc.stderr)
            stats_path = child / "native" / "stats.json"
            stats = json.loads(stats_path.read_text()) if stats_path.is_file() else None
            metrics = (stats or {}).get("evaluation_metrics") or {}
            cost = metrics.get("evaluationCostAttribution") or {}
            alloc, residency = parse_rts(proc.stderr.decode("utf-8", errors="replace"))
            rows.append({
                "input": sql_path.name,
                "input_sha256": hashlib.sha256(input_bytes).hexdigest(),
                "repetition": repetition,
                "returncode": proc.returncode,
                "status": metrics.get("evaluationStatus"),
                "wall_sec": wall,
                "rts_alloc_bytes": alloc,
                "rts_max_residency_bytes": residency,
                "parse_ffi_micros": (cost.get("snapshotParseFfiMicros") or {}),
                "parse_decode_micros": (cost.get("snapshotParseDecodeMicros") or {}),
                "artifacts": str(child),
            })
            print(f"{sql_path.name} rep={repetition}: rc={proc.returncode} {metrics.get('evaluationStatus')} "
                  f"wall={wall:.2f}s alloc={alloc} peak={residency} ffi={rows[-1]['parse_ffi_micros']} "
                  f"decode={rows[-1]['parse_decode_micros']}", flush=True)
    (out / "profile.jsonl").write_text("\n".join(json.dumps(r, sort_keys=True) for r in rows) + "\n")
    print(f"wrote {len(rows)} profile rows to {out / 'profile.jsonl'}")


if __name__ == "__main__":
    main()

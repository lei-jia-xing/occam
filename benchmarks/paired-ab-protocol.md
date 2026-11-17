# Paired A2/B notes: syntax-edit IR emission gate (2026-11-17)

Question: did gating per-call emission of unused syntax-edit IR reduce
decode/alloc cost without changing reduction behavior? Measured: identical
behavior on all matched slots; decode/alloc down. Details in
`benchmarks/benchmark_artifacts/column-ab-paired-20261116-a/report.md`.

## Builds

- A2 = metering-complete CONTROL: statement candidate-build timers + gate
  plumbing at legacy default TRUE (zero behavior change vs A).
- B = gate ACTIVE: default flipped to False with all consumers explicit-True.
- Same instrumentation in A2 and B; the default flip is the one isolated
  optimization. Before running anything against a build, record its commit
  SHA + binary SHA256 + fixed input/argv; no freeze helper is required.

## What was run

- Micro (engine mechanics, A2-then-B blocks, not interleaved): parse-profile pairs on both builds; alloc is the strong control, wall observed with host load unmeasured.
- Matched off-only: 8 cases × 2 reps × 2 builds via `tools/run_three_arm.py --arms column-off` (20s/1s budgets, one CPU, `--diag`).
- Revision (2026-11-17): rescoped from 3-arms-per-build to off-only per build (triad policy already covered; gate question needs the production profile). A full-triad A2 attempt was superseded; see `benchmarks/benchmark_artifacts/column-triad-A2-20261116-a/VOID.md` for its disposition.

## Analysis rules

- Joint-completed pairs only for speed/cost ratios; completed and censored
  reported separately; never pool censored endpoints into speedups.
- Candidate-build scope is COMPLETE in A2/B (statement timers present);
  never compare A2/B cost tables against A scope-incomplete data.
- Missing diagnostics are unavailable/incomplete, never 0; digest-presence
  coverage is not per-execution classification.
- No comparison against the uninstrumented 632 baseline or any external tool.
- Preserved wording: no global-winner claims; checkpoint size is not
  completion; column mode stays OFF by default.

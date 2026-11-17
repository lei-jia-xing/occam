# Paired A2/B protocol: syntax-edit IR emission gate

Question (investigation owned by measurements; fix owned by Good): does gating
per-call emission of unused syntax-edit IR reduce decode/alloc cost without
changing reduction behavior?

## Builds and freezes

- A2 = metering-complete CONTROL: statement candidate-build timers + gate
  plumbing at legacy default TRUE (zero behavior change vs A).
- B = gate ACTIVE: default flipped to False with all consumers explicit-True.
- Same instrumentation in A2 and B; the default flip is the one isolated
  optimization. Freeze each with `benchmarks/tools/freeze_build.py` (binaries +
  all compile inputs + helper identity) before running anything against it.

## Cohorts (matched exactly, helper bytes identical within/across the pair)

1. Micro (engine mechanics): `benchmarks/tools/run_parse_profile.py` over
   `benchmarks/benchmark_artifacts/profile-inputs-20261116-a/` (pairs
   004/016/064/256, exit-0 predicate), same reps/timeout/budget on A2 and B.
   Harvest: cost parse FFI vs decode micros by kind, RTS bytes allocated +
   max residency, wall, input/source SHAs, exact argv.
   Order is sequential blocks (all-A2 then all-B), NOT interleaved ABBA;
   wall comparisons carry host-load caveats; alloc bytes are the strong control.
2. Matched 8-case: `benchmarks/tools/run_three_arm.py --arms column-off`, same
   8 cases × 2 reps × 2 builds (32 children), same budgets (20s/1s), pinned
   venv, one CPU, `--diag`. Column-off runs the full intra syntax phases with
   only column edits disabled, so the gate (statement/dependency parses) is
   exercised in every mode; off is the production profile under test, not a
   no-op control. The f56 48-triad already covers the three-mode policy
   question separately and is never re-run or pooled here.

Optional same-B-binary parser-API on/off micro can further isolate the gate
but is unnecessary if A2 carries all new plumbing and only the default
toggles between builds.

Revision (2026-11-17): rescoped from 3-arms-per-build to off-only per build
per parent (triad policy already covered; gate question needs the production
profile). A full-triad A2 attempt was superseded; see
column-triad-A2-20261116-a/VOID.md for its disposition.

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

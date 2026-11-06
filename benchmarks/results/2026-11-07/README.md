# Fuzz reduction evaluation — 2026-11-07

## Scope and reproduction

These are generated SQL execution workloads, **not collected DBMS bugs**. Core parsing remains Tree-sitter; sqlparser-rs remains an open question. Source milestones: `0a36e7d` (SQL identity/CST quality), `8020183` (validated last-statement slice), `d9b79e3` (study/adapter/report tooling). No core parser or dependency was added.

- SQLite inputs derive from `test/golden/fuzz_golden_03_deep_expressions.golden.sql`. Statement count, object count, nesting and view fan-in vary separately. The corpus is 128 related variants plus intentional reset failure and a real SQLite recursive-query timeout: separate executions, not statistically independent bugs or diverse fuzz families.
- Commands and pinned optional prerequisites are in [the benchmark guide](../../README.md). Use `p2_scale_study.py prepare` and the existing batch/profile runners; preserve the same oracle, reset and budget. The evidence archive contains original manifests, inputs, metadata, outputs, metrics, audits, commands, experiment source and source/binary fingerprints.
- Scale: per-call 2 s, per-tool 30 s, cap 500, four input workers. Corpus: 1 s/12 s/cap 100, four workers. Native profiles: 2 s/30 s/cap 500, serial. Native executables and the supplemental evaluator use `GHCRTS=-N1`; SQLancer used `-XX:ActiveProcessorCount=1`. Host exposes eight logical CPUs; no CPU pinning, randomized order, confidence intervals or equal-resource serial/parallel speedup experiment.
- One physical preflight is shared per native-profile input and debited identically to each profile. Native aggregate exit may be nonzero because some profiles timed out; individual metric statuses are authoritative, not an assertion that the whole invocation succeeded. Outer initial/final validation costs and adapter-internal probes have distinct scopes.
- Runs were sampled once; some optional checks and profiling overlapped on the host. Wall times are descriptive, not isolated algorithm speedups. Smaller timeout checkpoints are not completed minima.

Machine-readable [profile matrix](profile-matrix.csv), [summary/distributions](summary.json), and [complete evidence](evidence.tar.gz). Archive paths retain original absolute command/artifact provenance; regenerate manifests into a new directory to reproduce on another checkout. The archived PostgreSQL predicate points to the isolated study socket; change its connection string only to a fresh, disposable cluster when repeating it.

## Scale, throughput and failure isolation

| Run | Inputs | Records | Completed | Timeout | Tool error | Makespan (s) | Inputs/s |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Scale | 12 | 24 | 11 | 13 | 0 | 135.396 | 0.089 |
| Corpus | 130 | 260 | 128 | 130 | 2 | 576.891 | 0.225 |
| Native profiles | 6 | 84 | 58 | 26 | 0 | 1115.665 | 0.005 |
| Reset comparison | 2 | 4 | 4 | 0 | 0 | 21.221 | 0.094 |

Makespan is measured monotonic batch processing time, **not summed worker time**. Status counts include intentional failures. Rates count processed inputs, not completed reductions. The 14-profile matrix includes all six inputs; a further six shared-evaluator observations compare closure strategies on three inputs, producing 90 unique input/profile records in the CSV.

| Tool | Completed/130 | Timeout | Tool error | Worker median/p90 (s) | Calls median/p90 | Checkpoint byte reduction median |
| --- | --- | --- | --- | --- | --- | --- |
| ddmin | 128 | 1 | 1 | 7.282/9.029 | 23.0/30 | 26.81% |
| occam | 0 | 129 | 1 | 10.560/10.614 | 37.0/39 | 28.24% |

The two injected cases fail before an accepted input: one reset infrastructure failure and one initial SQLite timeout per tool. They have no accepted-result record. Every other input continued independently. All 128 ordinary corpus inputs exhausted Occam's short AST budget; B2 completed them. This is a measured quality/cost trade-off, not evidence that the default full pipeline is faster for bulk input processing. Call/time distributions above cover all 130 records per tool; byte-reduction distributions cover the 128 retained checkpoints. P90 is nearest rank.

| Tool | Status | Statements in/out | Bytes in/out | Reducer callbacks |
| --- | --- | --- | --- | --- |
| occam | timeout | 4099/3 | 167377/542 | 67 |
| ddmin | timeout | 4099/65 | 167377/3023 | 46 |

Both long-input records remain timeout. The dependency-guided pipeline retained a smaller accepted checkpoint but used more callbacks; this is **not** a completed-minimum or fewer-DB-executions claim. Inputs reached 4099 statements/167377 bytes; broader dialect/fuzzer-family scaling is unmeasured.

All **374 SQLite retained results** (24 scale, 256 corpus, 84 original profiles, six supplemental strategies, four reset records) passed **two separately initialized, uncached observations each**, 748 total. These checks run outside original budgets and are stored separately in `p2/checkpoint-validation.jsonl`; they do not change timeout statuses or original `predicate_passed=null` fields.

## Prefix, extraction and repeated work

| 67-statement run | Backward-slice stage (s) | Checkpoint bytes | Overall status |
| --- | --- | --- | --- |
| Before last-root trial | 17.481291 | 545 | timeout |
| Validated last-root trial | 0.208020 | 541 | timeout |

The original scan tested 67 prefixes; its graph analysis took 0.061 s. The new trial checks the last statement's backward slice, and falls back to the unchanged linear scan if rejected. It assumes neither a causal last statement nor prefix monotonicity. A failed trial can add one rejected observation. The same 30 s budget was retained; saved prefix time allowed more AST work, not a fabricated completed run.

Forced single-pass profiling at 4099 statements measured parsing 0.061258 s, statement units 0.399811 s, action extraction 4.495262 s, graph construction 0.060933 s and closure 0.000270 s. `p2/phase-costs.csv` contains all 12 samples. This does not justify an incremental graph framework; extraction is more material than graph construction on this case.

The earlier compact-01 180 s timeout was **not rerun or relabeled**. Its archived profile reports 493 reducer observations, 174.012318 s in the wrapped oracle out of 175.571544 s reduction, six prefix tests/1.509469 s slicing, and roughly 0.006 s graph analysis. Audited wrapper resets consumed 40.303778 s **inside** that oracle time; native reset time zero does not mean no reset. Do not add overlapping scopes or claim the prefix change fixes this separate AST/oracle-dominated case. Both native final checks of its checkpoint had accepted.

## B1/B2, weighting and controlled dependency ablations

Statement-only tracks share statement units, predicate, cache, limits and evaluator; all completed on the six inputs. They do **not** share AST postprocessing with the full pipeline.

| Input | B1 calls | B2 calls | Byte-WeightedDD calls | CDD calls |
| --- | --- | --- | --- | --- |
| density-1 | 9 | 27 | 23 | 15 |
| density-4 | 9 | 27 | 25 | 15 |
| depth-8 | 26 | 30 | 48 | 15 |
| false-barrier | 7 | 7 | 7 | 5 |
| golden-expression | 4 | 7 | 7 | 6 |
| statements-64 | 74 | 44 | 45 | 31 |

Byte weighting is local WeightedDD, not token weighting or upstream WProbDD; CDD is the existing local counter-based strategy. Candidate memoization, closed-set deduplication and closure reuse remain active; this is not a separate closure-cache ablation. These small related workloads do not justify changing the default search strategy.

The following pipelines share AST passes, cache and budget; graph-off disables both slicing and closure. Hard closure disables unrestricted recovery. Full flags, stage costs, fresh validation and cache-toggle/no-intra/individual slicing/closure ablations are in the CSV and raw metrics.

| Input | Default bytes | Graph-off bytes | Hard-closure bytes | Statuses (same order) |
| --- | --- | --- | --- | --- |
| density-1 | 864 | 864 | 864 | timeout/completed/completed |
| density-4 | 969 | 1035 | 1002 | timeout/timeout/timeout |
| depth-8 | 579 | 569 | 579 | timeout/timeout/timeout |
| false-barrier | 11 | 11 | 28 | completed/completed/completed |
| golden-expression | 828 | 828 | 828 | completed/completed/completed |
| statements-64 | 533 | 533 | 533 | completed/timeout/completed |

The false-barrier case demonstrates why conservative edges cannot be hard constraints: unrestricted deletion recovers the smaller result. Other inputs show different search-order/timeout trade-offs, not a universal graph advantage. Completed status means the configured search finished and its predicate rechecked, not a global minimum; partial runs have no fixed-point/minimality guarantee.

Supplemental WeightedDD/CDD closure runs disable slicing, matching the existing count-DD `Shared (no slicing)` control, with the same AST/cache/limits and native `runEvaluatedReduction` API. They are a separate invocation, not duplicated independent inputs. Callback totals include all reduction stages; they are not closure-only costs.

| Input | Closure strategy | Status | Callbacks | Bytes |
| --- | --- | --- | --- | --- |
| false-barrier | count-dd | completed | 8 | 11 |
| false-barrier | cdd | completed | 7 | 11 |
| false-barrier | weighted-dd | completed | 10 | 11 |
| statements-64 | count-dd | timeout | 180 | 537 |
| statements-64 | cdd | timeout | 158 | 537 |
| statements-64 | weighted-dd | timeout | 173 | 541 |
| density-4 | count-dd | timeout | 247 | 969 |
| density-4 | cdd | timeout | 242 | 969 |
| density-4 | weighted-dd | timeout | 166 | 1035 |

## Reset and parallelism decisions

| Reset | Tool | Reset callbacks | Reducer reset (s) | Outer validation (s) | Worker (s) | Bytes |
| --- | --- | --- | --- | --- | --- | --- |
| reset-fresh | occam | 6 | 0.899783 | 1.352 | 6.491 | 43 |
| reset-fresh | ddmin | 2 | 0.383196 | 1.257 | 4.758 | 47 |
| reset-template | occam | 6 | 0.596756 | 1.166 | 5.230 | 43 |
| reset-template | ddmin | 2 | 0.282015 | 1.432 | 4.731 | 47 |

Reducer reset sums exclude outer initial/final validation. Same-tool fresh/template pairs use equal reset counts. Copying a closed SQLite template reduced measured reset cost in this sample, but there is no general server/container snapshot result or significance claim. A permanent regression corrupts schema, data, index, trigger, sequence and user_version, detects contamination and proves restoration; foreign_keys is set and checked per connection. Query-only scope keeps setup fixed for both tools.

Retain independent-input parallelism with private observation directories and fresh DB state. No speculative candidate workers were started, so speculative waste is zero by construction; no parallel candidate speedup or deterministic deadline checkpoint is claimed. There is no measured need for a second scheduler, generic snapshot layer or incremental parser/graph state.

## Pinned upstream compatibility comparison

All completed results below passed independent fresh-state predicates: SQLite rowset 42 or PostgreSQL ordinary SQLSTATE 22012. SQLancer uses actual upstream statement/AST reducers with a virtual JDBC provider and external predicate; SQLreduce uses actual upstream AST rules with external candidate execution, not its native reconnect/error oracle. SQLreduce preserves full multi-statement inputs, with no tool-specific setup/query substitution. B2 remains statement-only, so this is compatibility/quality evidence rather than an isolated AST algorithm timing comparison.

| Input | Occam bytes | B2 statement-only bytes | SQLancer bytes | SQLreduce bytes |
| --- | --- | --- | --- | --- |
| pg-temp-chain | 79 | 86 | 84 | 85 |
| pg-quoted-session | 106 | 114 | 111 | 112 |
| sqlite-cte | 51 | 74 | 50 | unsupported |
| sqlite-false-barrier | 11 | 11 | 11 | unsupported |

SQLancer pin `a7d60ee450e6a22ca38599c14e984a49773f4745`; SQLreduce pin `568c8a9a31601ead7128d0b067658b4d5a26821d`. Optional Python environment: pglast 8.7, psycopg2-binary 2.9.13, PyYAML 6.0.3. The original build-in-budget SQLancer run had three timeouts; after removing repeated fetch/build, all four prepared-source cases completed under the same per-input limits. Raw runs and their separate makespans remain preserved; paired quality data selects the prepared SQLancer records and does **not** invent a combined makespan. Earlier callback scope labels are corrected only in derived paired records, with explicit provenance, not rewritten raw data.

Both upstream adapters at cap one report `execution-budget`, one shared-checker DB observation and unknown runner validation; retained PostgreSQL checkpoints passed a separate fresh observation. The old Java adapter returned success while an owned SQLite recursive-query child remained active; the fixed adapter reports timeout and the observed child is gone. Arbitrary detached/reparented processes and untested dialects are not claimed covered. The private Unix-socket PostgreSQL service was stopped successfully after verification.

Existing CountDD/WeightedDD/CDD tracks satisfy the requested modern comparison without adding another baseline framework. Pinned Perses has SQLite/MySQL facades but no verified built-in PostgreSQL facade; Picireny requires an external ANTLR grammar. Neither is reported as a runtime SQL baseline: the runner retains explicit `not_integrated`/unsupported outcomes. See [source-backed DRReduce positioning](../../README.md#dependency-reconstruction-versus-sql-state-slicing); no compiler timings are reused as SQL measurements.

## Verification and limits

- Native build and 87 main-suite plus 10 fuzz-suite tests passed; five Python contract regressions passed. Commit hooks passed Fourmolu/HLint; existing compiler warnings are not claimed fixed.
- Actual CLI reductions, 90 native profile/strategy observations, 130-input failure isolation, full reset restoration, pinned upstream DB execution, cap-one checkpoints and five report output formats were exercised. No tests of source wording, fake DB outcomes, real-crash harvesting or Rust parser integration were added.
- These are bounded, oracle-scoped reductions. The grammar is not all-dialect complete; recognized catalog semantics are conservative, no typed identifier rebinding/semantic stubbing was implemented, and neither global minima nor universal performance gains are claimed.

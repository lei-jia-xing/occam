# Fuzz SQL reduction and benchmarks

Occam reduces existing multi-statement fuzz SQL while preserving an external predicate. Include required schema/setup statements in the input. The predicate is the target: exit 0 accepts; every nonzero or signal exit rejects. Output is not parsed for crash signatures. Database reset and isolation are the predicate's responsibility.

## Reduce an existing fuzz input

```bash
./benchmarks/live_fuzz_pipeline.sh \
  --input /path/to/fuzz.sql \
  --oracle './interesting.sh' \
  --reset './reset-db' \
  --dialect postgres \
  --timeout 30 --budget 300 --max-oracle-executions 1000 \
  --out-dir /tmp/occam-runs
```

With `--input`, an explicit `--oracle` command is required. Each candidate, including the initial input, is sent to the predicate on stdin. Exit 0 means interesting; every nonzero exit rejects. `--reset` runs before candidates but does not itself guarantee database isolation. Time, per-call and uncached-execution budgets include initial/final checks; on exhaustion the last accepted SQL is retained and the run reports failure. Each run gets an independent `run_*` directory with `input.sql` and `result.jsonl`; after the initial input is accepted, the reducer also writes `minimal.sql` and `stats.json`. If initial validation rejects or times out before CLI output, those files are absent.

The pipeline can also generate a synthetic stress input when `--input` is omitted:

```bash
./benchmarks/live_fuzz_pipeline.sh --dialect postgres --stmts 100 --case-id pg_01
```

That built-in marker predicate checks SQL text only. It is a reducer-mechanics example, not SQL execution or evidence of a DBMS bug. `--stmts` controls generated size and `--case-id` the marker.

## Experiment-line contract

There are exactly two experiment lines:

- **Internal**: the Occam full reducer and explicitly named internal controls/ablations. Each measured child runs one selected `occam-eval --profile NAME`.
- **External**: independently implemented reducer search against the same exact input, setup, predicate and declared budget. Adapters must execute against qualified DBMS targets and workloads under matching protocols.

### Primary DBMS scope and evaluation strategy (Strategy 3A)

Owner review adopted **Strategy 3A**: primary benchmark evaluation is centered on self-contained, reproducible, deterministic **SQLite** rowset predicates (scale study, internal ablations, and golden fixtures). External heavy DBMS cluster deployments (PostgreSQL, MySQL) are explicitly scoped out of the primary evaluation line to eliminate daemon fragility, socket lifecycle issues, and non-deterministic environment dependencies. Real DBMS crash harvesting is explicitly out of scope; workloads focus strictly on multi-statement DDL/DML reductions with stateful setup and deterministic rowset contracts.

External multi-tool benchmarks (SQLreduce, SQLancer) are optional reference adapters only and are explicitly outside the primary evaluation scope in favor of strictly partitioned internal controls and ablations.

#### SQLite reset and isolation model

- **SQLite**: Isolated per-child template database copy or in-memory execution within `OCCAM_WORKDIR` (`study.sqlite`).
- **Isolation boundaries and lifecycles**: Predicate/reset scripts provide deterministic rowset validation and state resets. Occam provides the per-call `OCCAM_WORKDIR` (with scoped `TMPDIR`/`TMP`/`TEMP`), enforces execution timeouts, guarantees clean POSIX process group teardown on exit or timeout, and immutably retains failures (never promoting partial checkpoints to completed reductions).
- **External DBMS adapters (reference only)**: PostgreSQL and MySQL reset models remain available as reference integration templates but are not part of the active evaluation line.

Execution mode (native CLI or API) is orthogonal to experiment line; this scale-study runner records native mode only. `reduction_track` labels the per-input workload (`whole_script` or `fixed_setup_query`), not a line or profile family. `profile_family` distinguishes statement-only controls from shared-reducer profiles; `intra_statement_edits` records whether that syntax pass is enabled, not universal AST coverage. Pairing requires matching line, input identity, track, validation, resource and count protocol. Profile names/effective settings are comparison sides: controlled ablations intentionally differ in a named mechanism. The runner preserves each selected profile as a separate record but does not form or validate homogeneous cohorts; do not pool cross-line or statement-only/syntax-enabled coverage.

The legacy multi-tool runner and reporter below do not enforce these partitions. Their mixed records are not a line-compliant leaderboard or evidence bundle.

## Optional external reducer pilot

`tools/compare_reducers.py` is a separate, bounded external comparison, not an
internal profile cohort or the legacy mixed-tool leaderboard. The primary
SQLite/internal evaluation scope above is unchanged.

It compares the native Occam CLI with `--intra-statement`, C-Reduce's `--not-c`
mode, SQLancer's upstream statement + AST reducers, and SQLreduce's upstream
search/rules. SQLancer and SQLreduce use the existing external-predicate
adapters, not their native DBMS orchestration. In particular, SQLreduce uses a
PostgreSQL parser against a shared SQLite predicate on these inputs; this is
not a PostgreSQL engine result or a general SQLite-support claim.

The default cohort is the six numbered SQL seeds in `seeds/` plus the existing
`statements-64` and `statements-512` scale fixtures. These are selected workloads,
not eight independent bugs or a representative SQL corpus. Each predicate
executes the complete candidate in a fresh in-memory SQLite connection, rejects
SQL errors, and compares the last rowset in exact order. Preparation checks
each original twice and rejects empty, malformed, and wrong-row controls.

Prepare the pinned upstream checkouts outside the repository:

```bash
export SQLANCER_SOURCE="$HOME/.cache/occam/sqlancer-a7d60ee450e6a22ca38599c14e984a49773f4745"
export SQLREDUCE_SOURCE="$HOME/.cache/occam/sqlreduce-568c8a9a31601ead7128d0b067658b4d5a26821d"
# For new checkouts; retain existing prepared checkouts instead of overwriting them.
git clone https://github.com/sqlancer/sqlancer.git "$SQLANCER_SOURCE"
git -C "$SQLANCER_SOURCE" checkout --detach a7d60ee450e6a22ca38599c14e984a49773f4745
mvn -q -Dmaven.test.skip=true package -f "$SQLANCER_SOURCE/pom.xml"
git clone https://github.com/df7cb/sqlreduce.git "$SQLREDUCE_SOURCE"
git -C "$SQLREDUCE_SOURCE" checkout --detach 568c8a9a31601ead7128d0b067658b4d5a26821d
uv venv --python python3 /tmp/occam-external-compare-venv
uv pip install --python /tmp/occam-external-compare-venv/bin/python \
  pglast==8.8 psycopg2-binary==2.9.13 PyYAML==6.0.3
CREDUCE="$(nix build --no-link --print-out-paths \
  github:nixos/nixpkgs/1d060086d60cb76a40cf37109f979d88a5bd6396#creduce)/bin/creduce"
env -u GHCRTS cabal build exe:occam
OUT="$(mktemp -d /tmp/occam-external-comparison.XXXXXX)"
/tmp/occam-external-compare-venv/bin/python benchmarks/tools/compare_reducers.py \
  --out "$OUT" --occam "$(env -u GHCRTS cabal list-bin exe:occam)" \
  --creduce "$CREDUCE" --sqlancer-source "$SQLANCER_SOURCE" \
  --sqlreduce-source "$SQLREDUCE_SOURCE" \
  --budget 20 --timeout 1 --repetitions 2
python3 benchmarks/tools/compare_reducers.py report "$OUT"
```

`--tools` selects a declared cohort (for example, `--tools occam sqlancer`);
the default remains all four tools. Only selected tools require binaries or
source checkouts. The selected list is recorded in `run.json`, and the reporter
requires exactly that tool/case/repetition cohort. Legacy v1 bundles without a
selection declaration retain their original four-tool cohort and are not
rewritten by the reporter.

For a matched rerun, use the original Python/SQLite environment and
`--reuse-cases-from benchmarks/benchmark_artifacts/external-comparison-20261115-a`
to copy the original SQL bytes and predicate metadata. The reuse path checks
the input, metadata, helper, interpreter, and SQLite contract before controls.
The source snapshot policy always records and hashes `src/Occam/Reducer.hs`
under `sources/production/`; pass additional changed production dependencies
with repeatable `--production-source PATH` arguments. Raw output bytes remain
the primary size metric; any structural count is diagnostic and must name its
parser and version.

Example Occam/SQLancer selection (no C-Reduce or SQLreduce arguments or
dependencies):

```bash
OCCAM="$(env -u GHCRTS cabal list-bin exe:occam)"
OUT="$(mktemp -d /tmp/occam-external-comparison.XXXXXX)"
/tmp/occam-external-compare-venv/bin/python benchmarks/tools/compare_reducers.py \
  --out "$OUT" --tools occam sqlancer \
  --occam "$OCCAM" --sqlancer-source "$SQLANCER_SOURCE" \
  --reuse-cases-from benchmarks/benchmark_artifacts/external-comparison-20261115-a \
  --budget 20 --timeout 1 --repetitions 2
```


The driver compiles the SQLancer bridge once before timing. Children run
serially with rotating tool order, inherit one CPU affinity where supported,
and use `GHCRTS=-N1`, C-Reduce `--n 1`, and JVM `ActiveProcessorCount=1`.
The common wall cap includes an input check, the tool process, and two final
checks; all tools reserve the same final-check window. There is no execution
count cap, page-cache flush, or separate warmup. External callbacks receive a
per-child exact-byte verdict cache in addition to upstream caches; Occam keeps
its native cache and its additional native preflight/final checks. Consequently
the comparison measures these declared execution adapters, not search-only CPU.

`run.json`, `manifest.json`, `qualification.json`, input/metadata files, source
snapshots, per-child commands/logs/audits, and reduced SQL retain the evidence.
`report.md` and `summary.json` show completion denominators and budget-limited
output sizes separately. Cost ratios use jointly completed pairs only;
noncompleted oracle audit counts can be incomplete and are not treated as
exact costs. Bytes include whitespace/comments, which AST printers can remove:
byte improvement is not a pure structural-complexity metric.

The reporter rejects missing/duplicate slots and mismatched input, predicate,
budget, experiment-line, or run-type identities. This pilot does not replace
the stricter internal partitioned reporting protocol or establish crash
identity. Any future crash/stack predicate must be shared unchanged by every
tool in its cohort; stack collection is optional and database-specific.

### Retained pilot observations

The local gitignored evidence bundle is
[`benchmark_artifacts/external-comparison-20261115-a/report.md`](benchmark_artifacts/external-comparison-20261115-a/report.md);
`run.json` records the actual Python/SQLite versions, native binary identity,
upstream pins, affinity, commands, and timing/cache scopes.
`evidence_validation.json` records the retained-output/hash, validation-audit,
and completed-call-count checks. The maintained driver above regenerates the
cohort in a new directory; the local bundle is not fresh-checkout packaging.

Across eight inputs and two repetitions, the bounded protocol completed
12/16 Occam runs, 5/16 SQLancer runs, and 0/16 C-Reduce or SQLreduce runs.
All 64 completed outputs/checkpoints passed the two common final checks;
40 predicate qualification controls passed. Completion means a tool finished
its own declared pass sequence, not equal search coverage or global minimality.
The report-contract regression command
`python3 -m unittest -v benchmarks.test_compare_reducers` passed four tests.

The clearest positive result is statement-noise scaling: Occam reduced both
the 67-statement and 515-statement inputs to the same 3-statement, 283-byte
output, with respectively 55 and 58 total audited predicate observations per
repetition. The larger input started at 20,679 bytes; its median outer time
was 15.88 seconds. This supports a bounded large-script reduction use case,
not a causal claim that dependency graphs alone produced the advantage.

The counterevidence matters: SQLancer produced a smaller completed output on
the deep-expression case, and C-Reduce/SQLreduce often retained smaller
validated checkpoints on short inputs despite not finishing. On the five
jointly completed Occam/SQLancer pairs, Occam's median wall advantage includes
adapter/runtime overhead; SQLancer used fewer predicate observations in four
of those pairs. This run does not establish uniformly better reduction quality,
lower oracle cost, crash preservation, or superiority across DBMSs.

#### Post-cap-fix Occam/SQLancer score (2026-11-16)

The fresh 8-input × 2-repetition score bundle is
[`benchmark_artifacts/external-comparison-post-capfix-20261116-a/`](benchmark_artifacts/external-comparison-post-capfix-20261116-a/);
per-case raw-byte, validation, scoped-cost, and parser-qualified diagnostics
are in
[`external-comparison-post-capfix-analysis-20261116-a/report.md`](benchmark_artifacts/external-comparison-post-capfix-analysis-20261116-a/report.md)
and `diagnostics.json` in the same directory. It uses the exact reused SQL
bytes and predicate, 20 s cap, 1 s oracle timeout, CPU 0, Python 3.14.8 /
SQLite 3.53.3, and declared cache/final-check protocol. The old
`external-comparison-20261115-a` bundle was not rerun or changed.

All 32 new outputs/checkpoints passed the two common final validations, but
new Occam completed 0/16 runs while fresh SQLancer completed 4/16 (deep
expressions and upsert, both repetitions). Thus there are no jointly completed
new Occam/SQLancer pairs and no completed-pair speed or cost ratio. Relative
to old Occam's raw output medians, the new validated timeout checkpoints are
smaller on five cases and larger on three; these are not completed reduction
quality wins. SQLancer's completed deep output is 955 B versus Occam's 1106 B
timeout checkpoint. The pglast 8.8 PostgreSQL-grammar diagnostic shows why
raw bytes need qualification: SQLancer has 30 more non-comment token bytes
but 181 fewer whitespace/comment bytes there. Parser counts are diagnostic,
not the SQLite oracle.

The old 12/16 Occam completion result was not uncapped syntax saturation:
the retained native stats show cap-boundary signatures in its completed
children, with no explicit termination reason. In the new run all 16 Occam
children log coarse-to-fine order but time out before terminal syntax summaries.
Each native timeout records one preflight and completed final-validation fields
(`evaluationValidationDatabaseObservations=2`, `evaluationValidationObservations=2`,
`evaluationValidationPassed=true`); these are separate from the three common
checks. Reduction observations and cache requests are exact through cutoff,
not total cost to saturation, and the native audit remains incomplete.
SQLancer's four completed rows have exact reduction-only scopes in the
diagnostic report, separated from the three common checks and its adapter
preflight. The report also records 76 accepted syntax edits and repeated
queue regeneration; rejected edit kinds are not logged, so repeated expensive
coarse-probe counts remain unknown. Neither result establishes general
superiority or a graph contribution.

The score snapshots the production `src/Occam/Reducer.hs` used for the run
(SHA-256 recorded in `run.json`); that is the only production source file
included in this bundle. Pass every other changed production dependency
explicitly with repeatable `--production-source PATH` when making a new
comparison.

#### Final phased-scheduler Occam/SQLancer score (2026-11-16)

The final immutable 8-input × 2-repetition score is
[`benchmark_artifacts/external-comparison-phased-20261116-a/`](benchmark_artifacts/external-comparison-phased-20261116-a/).
The regenerated machine diagnostics and case-by-case report are
[`diagnostics.json`](benchmark_artifacts/external-comparison-phased-analysis-20261116-a/diagnostics.json)
and
[`report.md`](benchmark_artifacts/external-comparison-phased-analysis-20261116-a/report.md).
The comparison reuses the exact inputs and full-script SQLite ordered-last-rowset
predicate, with a 20 s child cap, 1 s oracle-call timeout, CPU 0, and
`GHCRTS=-N1`; SQLancer is pinned to
`a7d60ee450e6a22ca38599c14e984a49773f4745`. The Occam binary SHA-256 is
`7ef5ce46f44faa1ab663557bf3e381c53d97db498d524e6fb71f20509954b9a6`,
and the recorded `src/Occam/Reducer.hs` SHA-256 is
`4665c455c3efa2df703208b57fa3a0911b57a767cfbca0897216ad43b6d8b4b1`.
The old baseline and intervening score bundles were not changed.

Phased Occam completed 0/16 children; fresh SQLancer completed 4/16. All
16 Occam timeout checkpoints and all 16 SQLancer outputs passed both common
final validations; all 32 selected outputs were validated twice. There are no
jointly completed pairs. These matched-budget endpoints permit a quality
comparison despite timeouts, but establish no completion-time or saturated-
quality result.

Median raw output bytes, phased Occam / SQLancer: CTE 1495/1353; transaction
942.5/970.5; deep expressions 685/955; multitable diamond 1581.5/1525;
upsert 632/674; views 735/837; `statements-64` fixture (67 total statements)
278/313; `statements-512` fixture (515 total statements) 278/507.
Occam is smaller in six cases and larger in CTE and diamond. The report
separately lists pglast 8.8 PostgreSQL-grammar AST/token/whitespace diagnostics;
these are not the SQLite oracle or a minimality score. In the CTE case, for
example, the 142-byte raw gap includes 21 more Occam token bytes and 121 more
whitespace/comment bytes.

Phased logs contain 111 accepted syntax rewrites versus 76 in the
post-cap-fix global-restart run, including accepted CASE, function, and type
simplifications in the deep case. However, none of the 16 phased children
emitted a terminal syntax summary before timeout, so syntax-pass saturation
is unproven. CTE and multitable diamond remain larger than SQLancer's
checkpoints; equal-length scalar canonicalization remains unsupported by the
strict byte-shrink gate, and this iteration added no AST rewrite families.
This evidence does not establish full AST/global minimality or general
superiority over SQLancer. Scoped counters and remaining limitations are in
the linked diagnostics/report; timeout counters are observed through cutoff,
not total cost to saturation.

#### Offline syntax/List prefix diagnostic (2026-11-16)

The bounded offline audit reuses the immutable phased-score artifacts; it ran
no reducer, predicate, Oracle, or score. Its exact-digest joins show two
rejected CTE List candidates replayed as cache hits after an acceptance, while
the MAX-removal candidate from the actual retained checkpoint is distinct
from the original-input candidate and absent from all saved requests. The
diamond case has three accepted List row removals but only one request after
the final acceptance before timeout. Findings, counter-scope limits,
provenance, and the offline-only reproduction command are in
[`benchmark_artifacts/syntax-trace-20261116-a/report.md`](benchmark_artifacts/syntax-trace-20261116-a/report.md);
machine-generated joins and source are in the same directory. Diagnostic
observer work is not pooled with score timing.

#### Forward-sweep Occam/SQLancer score (2026-11-16)

The immutable 8-input × 2-repetition comparison is
[`benchmark_artifacts/external-comparison-forward-20261116-a/`](benchmark_artifacts/external-comparison-forward-20261116-a/).
Its generated diagnostics and full report are
[`diagnostics.json`](benchmark_artifacts/external-comparison-forward-analysis-20261116-a/diagnostics.json)
and
[`report.md`](benchmark_artifacts/external-comparison-forward-analysis-20261116-a/report.md).
The run records the exact reused SQLite inputs, 20 s child cap, 1 s oracle
timeout, CPU 0, `GHCRTS=-N1`, Python 3.14.8 / SQLite 3.53.3, Occam binary and
`Reducer.hs` SHA-256 identities, and SQLancer's upstream pin.

Forward-sweep Occam completed 4/16 children, timed out on 10, and returned
error on both CTE children; fresh SQLancer completed 6/16 and timed out on 10.
All 32 new outputs/checkpoints passed both common final checks, but there are
zero jointly completed pairs. Forward-sweep median raw bytes were 1379/879/
670/1399/616/729/274/274 for CTE/transaction/deep/diamond/upsert/views/
67-statement/515-statement inputs; they are smaller than the historical
phase-fixed-point medians on all eight inputs. Against SQLancer, 14/16
per-repetition slots are smaller and both larger slots are CTE; this is a
matched-budget checkpoint comparison, not a speedup, completed-quality,
saturation, or global-minimality result. The two CTE errors are retained
`OracleInconclusiveError` failures after per-call timeouts; their 1379-byte
checkpoints passed native and common final validation. See the report for
native/common cost scopes, pglast 8.8 syntax diagnostics, and timeout
limitations.

#### Bounded --jobs paired measurement (2026-11-16)

The immutable same-binary jobs1-vs-jobs4 paired run is
[`benchmark_artifacts/concurrency-jobs1-vs-jobs4-20261116-a/`](benchmark_artifacts/concurrency-jobs1-vs-jobs4-20261116-a/);
per-pair verdicts and the aggregate are
[`pairs.json`](benchmark_artifacts/concurrency-jobs1-vs-jobs4-20261116-a/pairs.json)
and
[`summary.json`](benchmark_artifacts/concurrency-jobs1-vs-jobs4-20261116-a/summary.json).
The prepared runner and fixed protocol are
[`run_paired.py`](benchmark_artifacts/concurrency-paired-jobs-20261116-a/run_paired.py)
and
[`PROTOCOL.json`](benchmark_artifacts/concurrency-paired-jobs-20261116-a/PROTOCOL.json).

Both arms ran the same snapshotted new binary (`98d276d9...`) under the same
inherited CPUs {0,1,2,3} with `GHCRTS=-N1`, on the 64-row SUM workload only:
4 AB/BA pairs (8 arms), 90 s outer / 1 s per call / no execution cap, helpers
`5b36f6` + `713cd458` verified against the frozen ledger (drift refuses the run).
Strict completion (rc 0, native completed, native final 2/pass, output present,
common double validation) held for all 8 arms with zero blockers; missing
metrics would classify `INCOMPLETE-PROFILE` and excluded pairs never enter
ratios. All 4 pairs jointly completed with identical `67B` endpoints matching
the recorded frozen endpoint, each validated twice.

Reducer-wall jobs4/jobs1 ratios were 0.531/0.600/0.550/0.573 (AB-1/AB-2/BA-1/BA-2),
median 0.561, on jointly-completed identical-bytes pairs only. Physical oracle
attempts were 46 vs 31 every rep; jobs4 recorded cancels 9/11/14/11, joins 11,
and cache hits 34 per arm, with reducer-phase unknowns kept as cost-only (never
downgrades). The pre-scenario CLI phase captured `--help` and rejected invalid
`--jobs` values (0/-1/abc/huge) with zero oracle side effects.

This is narrow same-resource paired evidence for one workload only: not a
primary-protocol score, not an eight-input run, not an SQLancer comparison, and
not a general speedup claim.

## Internal profile worker

`occam-eval` runs exactly one named internal control or ablation per process.
It does not provide independent external baselines. `--profile` is required;
the numeric `--baseline` selector and implicit all-profile run are removed.

```bash
cabal run occam-eval -- input.sql --profile full \
  --oracle './interesting.sh' --dialect postgres \
  --budget 30 --max-oracle-executions 100 --out /tmp/profile-full
```

Profiles: `full`, `no-slicing`, `no-closure`, `graph-off`, `no-intra`,
`hard-closure`, `no-cache`, `sequential`, `statement-ddmin`,
`statement-ddmin-fixed-point`, and `statement-ddmin-with-syntax`.
`sequential` and `statement-ddmin` are direct statement-only controls.
The other profiles use the shared reducer; the two graph-free fixed-point
profiles disable/enable its intra-statement syntax pass. `profile_family` and
`intra_statement_edits` in scale-study records preserve these distinctions;
inspect the effective settings rather than treating names as coverage claims.

Multiple `--profile` options produce independent child records, not an
automatically homogeneous or paired cohort. Compare full-reducer ablations as
declared mechanism contrasts; keep statement-only controls and syntax-enabled
profiles visibly distinct rather than aggregating their coverage.
Each direct invocation selects exactly one internal profile and writes
`minimal.sql`, `profile.json` and a one-metric `benchmark.json`; use a distinct
output directory per invocation. Scale-study runs give each case × profile ×
repetition a fresh process, cache, counters, budget, physical preflight and
output directory.
`--timeout` is per predicate
execution; `--budget` caps preflight, reduction/audit and final validation;
`--max-oracle-executions` includes preflight and final validation. Reducer
`oracle_calls`, preflight debit and final-validation calls are reported
separately. Budget exhaustion remains a nonzero exit with a retained accepted
checkpoint, never a completed result. Database-state isolation remains the
predicate/reset's responsibility, not a property of process isolation.

## Scale-study fixture and internal profile driver

`tools/scale_study.py` derives SQL from the existing deep-expression golden fuzz fixture, executes it with SQLite to establish expected rowsets, and keeps each input beside its predicate metadata. It varies statement count (up to 4099 total), object count, expression depth and view fan-in separately. The corpus contains 128 related variants plus explicit reset-failure and real SQLite timeout cases; it is not 128 independent bugs or broad dialect coverage.

```bash
python3 benchmarks/tools/scale_study.py prepare /tmp/occam-study --corpus-size 128
python3 benchmarks/tools/scale_study.py profiles /tmp/occam-study/profiles.json \
  --binary "$(env -u GHCRTS cabal list-bin exe:occam-eval)" \
  --profile full --profile graph-off --repetitions 3 \
  --out /tmp/occam-study/profiles-run --timeout 2 --budget 30 \
  --max-oracle-executions 500 --outer-timeout 35
python3 benchmarks/tools/scale_study.py report /tmp/occam-study/profiles-run --base full --target graph-off
```

The internal profile driver (`scale_study.py profiles`)
and partitioned reporter (`partitioned_report.py`) provide the unified control plane for Strategy 3A;
external reducer comparisons are explicitly scoped out of the primary evaluation line.

`--repetitions` is a positive integer (default 1); children run serially in
manifest-case, profile-option, repetition order. The optional outer child
deadline defaults to `budget + max(timeout, 1)` and uses the existing
process-group helper. Timeout cleanup records requested/failed/incomplete or
unknown; normal-exit cleanup is unknown, and a requested signal is not proof
that every descendant exited. Escaped/reparented children are not guaranteed.
Each child receives its own `GHCRTS=-N1` environment without mutating the
runner's environment.

There is no separate warm-up or CPU-affinity pinning; the native cache setting
is recorded from each profile metric, while the OS page cache is not reset.
Each child directory preserves exact `command.json`, opaque `stdout.log` and
`stderr.log`, and any produced `benchmark.json`, `profile.json` or
`minimal.sql` checkpoint; rows retain their raw/expected artifact paths.
`run.json` records run/config/order, Python and native binary identity, actual input
checksums, and configured predicate/reset command fingerprints (not their
transitive scripts or dependencies). JSONL rows carry unique run/child IDs,
command/artifact paths, input/output hashes, structured terminal observations,
and native return code independently. `wall_time_sec` is child-reported;
`outer_wall_time_sec` includes process startup and bounded cleanup, while the
summary's `wall_time_sec` is the observed serial batch makespan. Byte lengths
and hashes use exact bytes; statement counts require nonnegative native typed
`countStatements(dialect)` metrics, and token counts remain null. Missing or
malformed metrics, spawn failures and outer timeouts are retained per child
and do not suppress later children.

`cleanup_reason` records observed helper failures. Linux attempts `/proc`
descendant enumeration and signals the owned POSIX group; other POSIX systems
signal the group without enumeration, and Windows targets only the direct
process. Escaped/reparented descendants are not guaranteed to be killed.

Reset durations are split into preflight, reduction and final-validation
phases; their sum remains null if any phase is unknown. Configured reset-command
call counts are uninstrumented and stay null. With no configured reset command,
zero runner-issued reset calls does not establish that a predicate has no
private reset behavior.

Each predicate observation receives a temporary candidate workdir with scoped
`OCCAM_WORKDIR`/`TMPDIR`; configured reset and predicate run there, and the
directory is removed afterward. For the SQLite study, the file-backed reset
and state signature check qualify that fixture path only. Process/output
separation does not establish isolation for arbitrary server databases,
external state or escaped descendants.

The old `results/2026-11-07/README.md` path is unavailable and does not support
replaying the claimed 90-record evaluation. A bounded local isolation probe is
retained in the gitignored directory
`benchmarks/benchmark_artifacts/profile-isolation-20261114-b/`:

```bash
python3 benchmarks/tools/profile_isolation_probe.py \
  --binary dist-newstyle/build/x86_64-linux/ghc-9.10.3/occam-0.1.0.0/build/occam-eval/occam-eval \
  --out benchmarks/benchmark_artifacts/profile-isolation-20261114-b
```

The retained probe reports `qualified=true`; both repetitions produced
completed, validated native `full` profile rows. It covers one existing
`reset-template` SQLite rowset using per-observation candidate workdirs and
external reset only. This does not establish shared-server restoration,
representative workload evidence, or the full two-line control plane. The
probe helper SHA256 is
`8b8a37fe066f1932f8a3be2bd2fd9196739382af027a881c78ae67f6d79ab4f3`; evidence
is in `probe.json`, `observations.jsonl`, `cleanup.jsonl`, and `runs/` there.
The controller's normal-exit process cleanup status is unknown, not assumed
clean. To rerun, choose a new empty `--out` directory.

Focused orchestration verification reported:
`python3 -m unittest -v benchmarks.test_scale_study` — 5 tests, 11.971s, OK.

## GHC flamegraph

This checks the GHC 9.10 `.prof` → SVG toolchain on one generated SQLite-rowset input; it is not a performance baseline. Install `ghc-prof-flamegraph` separately from project dependencies:

```bash
set -e
TOOL_DIR=$(mktemp -d /tmp/ghc-prof-flamegraph.XXXXXX)
BUILD_DIR=$(mktemp -d /tmp/occam-ghc-profile-build.XXXXXX)
OUT_DIR=$(mktemp -d /tmp/occam-ghc-profile.XXXXXX)
cabal install ghc-prof-flamegraph-0.2.0.1 --installdir="$TOOL_DIR" --overwrite-policy=always
cabal build exe:occam --builddir="$BUILD_DIR" --enable-profiling --enable-executable-profiling \
  --ghc-options='-O2 -fprof-late' --disable-executable-stripping
BIN=$(cabal list-bin exe:occam --builddir="$BUILD_DIR")
python3 benchmarks/tools/scale_study.py prepare "$OUT_DIR/study" --corpus-size 1
ORACLE=$(jq -r '.cases[] | select(.id=="statements-4096") | .oracle_command' "$OUT_DIR/study/scale.json")
set +e
(cd "$OUT_DIR" && env -u GHCRTS "$BIN" +RTS -N1 -P -RTS "$OUT_DIR/study/statements-4096.sql" \
  --oracle "$ORACLE" --dialect sqlite --timeout 1 --budget 10 --max-oracle-executions 100 \
  --log-format json --out "$OUT_DIR/run")
status=$?
set -e
printf 'bounded smoke exit=%s (inspect run/stats.json; timeout is not completion evidence)\n' "$status"
"$TOOL_DIR/ghc-prof-flamegraph" --time "$OUT_DIR/occam.prof" \
  --flamegraph-option=--fonttype=sans-serif -o "$OUT_DIR/cpu-time.svg"
"$TOOL_DIR/ghc-prof-flamegraph" --alloc "$OUT_DIR/occam.prof" \
  --flamegraph-option=--fonttype=sans-serif -o "$OUT_DIR/haskell-allocation.svg"
```

The setup links the same, uninstrumented Rust release archive under Cabal's `_p` library name; this does not profile Rust. GHC time profiles exclude safe foreign-call CPU (`c_parseSQL`), child-oracle time and Rust allocations; allocation is cumulative Haskell heap, not live heap. Treat these graphs as Haskell diagnostics, not whole-process timing.

## Evidence-led diagnostic profiling

To capture symbolized runtime profiles and separately attribute child phase costs without contaminating score runs:

```bash
python3 benchmarks/tools/scale_study.py profiles manifest.json \
  --binary /path/to/profiled/occam-eval \
  --out /path/to/diagnostic-output \
  --profile full --repetitions 2 \
  --diagnostic
```

When `--diagnostic` is passed:
- `run.json` and every record in `results.jsonl` record `run_type: "diagnostic"` (normal runs record `run_type: "score"`) to support line-compliant partitioning; do not mix score and diagnostic runs or rely on the legacy reporter to enforce this boundary.
- The instrumented binary is invoked with `+RTS -N1 -P -po{case_dir}/occam -RTS`, writing symbolized cost-centre profile `{case_dir}/occam.prof`.
- If `ghc-prof-flamegraph` is available, SVG flamegraphs `{case_dir}/cpu-time.svg` and `{case_dir}/haskell-allocation.svg` are generated.
- If Linux `perf` is supported, the run is wrapped with resolved absolute `perf record -o {case_dir}/perf.data -g -- ...` and `{case_dir}/perf-report.txt` is generated with symbol tables.
- Postprocessing renderers and reporters reuse bounded runner process management, capturing structured execution records and raw stderrs into `diagnostic_manifest.json` under `postprocessing`.
- A top-level `diagnostic_status` (`"completed"`, `"partial"`, or `"failed"`) is recorded separately from native SQL reduction completion.
- Child predicate and reset costs are typed and attributed by phase in each record:
  `predicate_preflight_sec`, `predicate_reduction_sec`, `predicate_validation_sec`, `predicate_time_sec`,
  `reset_preflight_sec`, `reset_reduction_sec`, `reset_validation_sec`, `reset_time_sec`, and
  `unattributed_wall_time_sec` (residual wall-clock duration: `wall_time_sec - (predicate_time_sec + reset_time_sec)`; reflects scheduling, RTS runtime overhead, parser execution, and measurement latency, not CPU attribution; set to null with reason on inconsistent timing).
- A per-child `diagnostic_manifest.json` records toolchain versions, binary fingerprints, exact CLI and RTS flags, artifact paths, postprocessor command outcomes, and explicit blind spots disclosure (input checksums and reduction track provenance remain in `run.json` and per-child `results.jsonl` rows).

### Known blind spots and constraints
- **Internal Rust CPU**: GHC cost centres do not resolve internal Rust CPU; safe foreign calls can run outside sampled Haskell capabilities; use native perf with declared coverage.
- **Rust Allocations**: Rust allocations occur via the system/Rust allocator and are completely invisible to the GHC heap profiler.
- **Child Process Isolation**: External predicate (`--oracle`) and reset (`--reset`) commands execute in independent child processes. GHC profilers cover Haskell-side execution, not child process CPU or memory; predicate and reset phase durations are wall-clock times.
- **Linux perf Scope**: Linux `perf` default `-g` inherits the entire native process tree (including child oracle/reset processes and kernel fork/clone routines; symbolized aggregate in `perf-report.txt` is not target-only or Rust-only CPU, and presence of `perf.data` does not prove isolated Rust profiling). Linux `perf` does not observe Haskell heap allocations or green-thread scheduling.
- **Phase Duration Nature**: Typed predicate and reset phase costs (`predicate_*_sec`, `reset_*_sec`) are wall-clock durations measured from the runner/engine perspective, not isolated CPU time.
- **Bounded Case Sample Count**: Low CPU sample counts on small bounded inputs (such as the 47-byte SQLite `reset-template` case) reflect fast execution where startup, reset, and predicate phases dominate, rather than an absence of hotspots.
- **Toolchain Smoke Proof**: Prior flamegraph artifacts (e.g. `ghc-profile-20261113-a` on `statements-4096.sql`) remain toolchain proofs, not a performance baseline.

### Retained diagnostic evidence
Verified diagnostic evidence on the bounded SQLite `reset-template` case is retained at gitignored `benchmarks/benchmark_artifacts/diagnostic-profile-20261114-final2/` (exact commands and verified artifacts in per-child `command.json` and `diagnostic_manifest.json`). Target binary preparation metadata and build logs are recorded in `benchmarks/benchmark_artifacts/profile-prep-20261114-eval/preparation.json` and `cabal-build-v2.log`.

### Statement boundary traversal optimization
- **Bottleneck**: Old GHC profiling (`ghc-profile-20261113-a`) revealed `$wunsafeDrop` in `Occam.Reducer` consuming 79.2% CPU ticks during multi-statement reduction. In `cutRemovedStatements`, boundary lookup used `drop (index + 1) allStatements` for each removed statement, leading to repeated $O(N^2)$ list suffix traversals.
- **Optimization**: Replaced quadratic drop rescans with a single linear adjacent-statement walk (`collectSpans`) computing `nextStart` from the list tail in $O(1)$ per statement.
- **Scope and Ceiling**: Boundary lookup alone improves from $O(N^2)$ suffix rescans to an $O(N)$ adjacent walk. Kept-ID `Set` operations ($O(N \log K)$), removed-span sorting ($O(M \log M)$), and repeated ByteString copying in `cutSpans` remain separate costs. No whole-function $O(N)$ or external DBMS speedup is inferred. Candidate order, reduction semantics, and byte preservation remain exact.
- **Evidence**: Matched V2 before/after measurements across 1024, 4096, and 8192 statements (`benchmarks/benchmark_artifacts/statement-surgery-perf-20261114/final_v2_comparison.json`), using GHC 9.10.3 `-O1`, RTS `-N1 -T`, and 5 repeats per case. Reported values are 5-run arithmetic means of the helper microbenchmark timed region (timed statement surgery plus deep output forcing and FNV-1a64 checksumming, not isolated pure-function CPU). Across all 75 benchmark runs (15 size/pattern cases × 5 repeats), output lengths and FNV-1a64 checksums matched identically; this does not claim byte-for-byte equality across all 75 outputs. Separately, 14 exact ByteString semantics checks and all 184 `occam-test` suite tests passed.

| Statements | Pattern | Process CPU Before → After (ms) | CPU Speedup | Wall Before → After (ms) |
|---|---|---|---|---|
| 8192 | `keep_one` | 274.30 → 92.28 | 2.97x | 275.96 → 93.18 |
| 8192 | `keep_half` | 226.05 → 95.99 | 2.35x | 226.98 → 96.78 |
| 4096 | `keep_one` | 58.06 → 25.27 | 2.30x | 58.21 → 25.53 |
| 4096 | `keep_half` | 49.97 → 19.84 | 2.52x | 50.18 → 19.93 |
| 8192 | `keep_all` | 1.28 → 1.49 | 0.86x | 1.28 → 1.49 |
| 4096 | `keep_all` | 0.63 → 0.72 | 0.87x | 0.63 → 0.72 |
| 8192 | `keep_none` | 153.23 → 0.42 | 364.55x | 153.56 → 0.43 |
| 4096 | `keep_none` | 35.77 → 0.19 | 183.72x | 35.80 → 0.20 |

Small absolute regressions were observed on `keep_all` controls (+14% to +16%), which perform no cuts; the optimization does not generalize to speeding up every pattern. The mechanics-only `keep_none` case performs zero retained output copying, isolating the boundary traversal elimination ceiling.

### End-to-end large-SQL performance closure
- **Workload & Protocol**: Full end-to-end reduction on `statements-512.sql` (515 statements, 20,922 bytes; dialect SQLite) reduced to 3 statements (517 bytes), evaluated against the deterministic SQLite rowset oracle via 3 matched interleaved runs (`B1, C1, B2, C2, B3, C3`). Binaries built in isolated trees under GHC 9.10.3 (`-O1 -threaded -rtsopts`) differing strictly in `cutRemovedStatements` (baseline drop-rescan vs current `collectSpans`), run with `+RTS -N1 -s -RTS`. Raw records in gitignored `benchmarks/benchmark_artifacts/large-sql-e2e-20261114/e2e_comparison.json`.
- **Observed Metrics**:
  - **Occam Process CPU**: Total user CPU improved from 3.182s to 2.614s (1.22x speedup; Mutator CPU 2.892s → 2.360s, 1.23x speedup).
  - **Occam Internal Reduction Wall**: Reduction wall excluding child oracle duration improved from 1.432s to 1.096s (1.31x speedup).
  - **External Process Dominance**: External child oracle subprocess wall clock accounted for 98.19% (baseline, 77.67s / 79.11s) and 98.52% (current, 72.78s / 73.88s) of reduction loop wall time.
  - **Reduction Quality**: All 6 runs completed cleanly with status `completed` (exit code 0), achieving 100% identical outputs (3 statements, 517 bytes).
- **Historical Stopping Decision (Superseded)**: Earlier iterations closed large-SQL performance tuning on the basis that external child process execution dominated end-to-end makespan (~98%). On 2026-11-14, owner instructions reopened internal performance and source quality engineering, requiring general algorithmic and allocation improvements without heuristics, preserving exact candidate order, reduction semantics, and byte preservation contracts.

### Single-pass retained-span assembly and candidate hoisting (2026-11-14)
- **Mechanics & Invariants**:
  - `cutRemovedStatements`: Replaced descending span sort and repeated `foldl'` concatenation (worst-case $O(\text{removed} \times \text{input bytes})$ copying across up to removed-count intermediate ByteString allocations) with linear cursor assembly (`chunks !cursor (stmt : rest)`). Kept statements advance down the list without moving the cursor, automatically coalescing adjacent kept spans. A removed statement emits preceding non-empty retained slice `[cursor, start)` (covering kept statements and leading prefix bytes $[0, start_0)$) and advances cursor to `nextStart`. Returns `baseSQL` directly on zero removals (`[single] -> single`), emitting at most one output-buffer allocation via `BS.concat` when multiple slices exist.
  - Complexity: $O(K \log K + S \log K)$ Set construction and lookup ($K = \text{kept}$, $S = \text{total}$ statements), plus $O(\text{output bytes})$ copying; at most one output-buffer allocation for $>1$ slice, 0 output-buffer allocations for $\le 1$ slice (Set and list cell allocations remain separate).
  - `searchDDMin`: Hoisted `currentIds`, `currentSQL`, and `currentLen` to the outer `go granularity current` step loop, computing them once per granularity step rather than per candidate trial (recomputed if delta debugging increases granularity on an unchanged `current`).
  - Candidate trials, oracle sequence, feedback caching, and reduction semantics remain identical.
- **Evaluation & Commands**:
  - Build command: `cabal exec -- ghc -O1 -threaded -rtsopts -package occam /tmp/helper.hs -o helper` using GHC 9.10.3 and identical pinned Rust static library `libCoccam_sqlparser.a` (SHA-256 `abf74a41f32f9d566536764a8c9e4e9b0a34a666b51577c7478d3aa54f5be348`).
  - Workload: `benchmarks/benchmark_artifacts/haskell-perf-20261114/statements-512.sql` (515 statements, 20,922 B), deterministically generated via `scale_study.py`.
  - Microbenchmark: `./helper micro +RTS -N1 -s -RTS` across 4 sizes × 6 patterns (100 iterations/case). Note: initial micro run (`baseline_micro.txt`) was invalidated and excluded because GHC let-floating floated the pure surgery outside `replicateM_`; the valid protocol (`baseline_micro_corrected.txt` vs `after_micro_corrected.txt`) enforced runtime IO input dependencies via `readIORef` inside `{-# NOINLINE runOneSurgery #-}`.
  - Trace benchmark: `perf stat -e instructions,cycles,branch-misses,page-faults ./helper trace statements-512.sql trace.bin min.sql +RTS -N1 -s -RTS` (deterministic in-process oracle, 19 trials). Valid trace is independent of the flawed micro loop.
  - Linux `perf record`: `perf record -F 999 -g -o perf.data -- sh -c 'for i in $(seq 1 30); do ./helper trace statements-512.sql /tmp/trace_trash.bin /tmp/min_trash.sql > /dev/null; done'`, analyzed with `perf report -i perf.data --stdio --no-children -n --percent-limit 1.0`.

| Benchmark Workload / Metric | Baseline (`collectSpans`) | After Optimization | Delta / Speedup | Evidence File |
|---|---|---|---|---|
| Micro Total RTS Heap Allocation (24 cases × 100 iters) | 132,823,984,792 B (132.8 GB) | 1,107,905,104 B (1.11 GB) | 120x reduction (-99.17%) | `baseline_micro_corrected.txt` vs `after_micro_corrected.txt` |
| Micro 4096 Dense90 CPU (90% removed) | 27.787 ms/call | 0.154 ms/call | 180.9x speedup | `after_micro_corrected.txt` |
| Micro 4096 Interleaved50 CPU (50% removed) | 18.069 ms/call | 0.368 ms/call | 49.1x speedup | `after_micro_corrected.txt` |
| Micro 4096 Sparse10 CPU (10% removed) | 6.159 ms/call | 0.484 ms/call | 12.7x speedup | `after_micro_corrected.txt` |
| Micro 2048 Dense90 CPU | 7.511 ms/call | 0.072 ms/call | 105.0x speedup | `after_micro_corrected.txt` |
| Micro 2048 Interleaved50 CPU | 5.052 ms/call | 0.162 ms/call | 31.3x speedup | `after_micro_corrected.txt` |
| Micro 512 KeepAll CPU (0 removed) | 0.100 ms/call | 0.156 ms/call | +0.056 ms/call | `after_micro_corrected.txt` |
| Micro 512 SingleRemoval CPU (1 removed) | 0.121 ms/call | 0.156 ms/call | +0.035 ms/call | `after_micro_corrected.txt` |
| Reducer Trace Heap Allocation (19 trials on 512 stmts) | 202,126,512 B (202.1 MB) | 21,837,848 B (21.8 MB) | 9.26x reduction (-89.2%) | `baseline_trace.txt` vs `after_trace.txt` |
| Reducer Trace Wall Time | 121.912 ms | 76.676 ms | 1.59x speedup (Mutator 1.69x) | `baseline_trace.txt` vs `after_trace.txt` |
| Reducer Trace CPU Cycles | 145,491,031 | 93,926,452 | -35.4% cycles | `baseline_trace.txt` vs `after_trace.txt` |
| Reducer Trace Instructions | 150,088,873 | 119,755,264 | -20.2% instructions | `baseline_trace.txt` vs `after_trace.txt` |
| Reducer Trace Branch Misses | 521,767 | 312,181 | -40.2% branch misses | `baseline_trace.txt` vs `after_trace.txt` |
| Reducer Trace Page Faults | 3,937 | 3,312 | -15.9% page faults | `baseline_trace.txt` vs `after_trace.txt` |
| Linux `perf` `__memmove_avx_unaligned_erms` share | 17.33% sampled share | 2.31% sampled share | -15.02 pp share | `perf_report_baseline.txt` vs `perf_report_after.txt` |

- **Verification & Limitations**:
  - Candidate equivalence: Length-framed traces (`trace_baseline.bin` and `trace_after.bin`, 66,768 B; SHA-256 `59f00b64b5970a201f8d4c741754c7faf945f5bf215da704ca41f8a60f0215ed`) and minimal outputs (`minimal_baseline.sql` and `minimal_after.sql`, 290 B; SHA-256 `598f8f2b3f77bdc84b8c89157feb36246a4003d254513919313391168a232f43`) are 100% byte-for-byte identical via `cmp`.
  - Microbenchmark numbers reflect one aggregate run of 100 iterations per case; no statistical significance estimate is claimed for small differences on 512 KeepAll/SingleRemoval.
  - Linux `perf` samples native threads across the process tree (including startup, parser, libc, and RTS), but does not observe GHC heap allocations or green-thread scheduling; Rust allocations use the system allocator outside GHC RTS.
  - The in-process deterministic oracle isolates Occam reduction from child process startup and external SQLite costs; no external DBMS makespan speedup is claimed or inferred.
- **CLI Final Integration Smoke**:
  - Rebuilt `exe:occam` linking final Haskell and static Rust library `libCoccam_sqlparser.a`.
  - Workload & Artifacts: `benchmarks/benchmark_artifacts/general-perf-final-smoke-20261114/` containing input SQL (485 B, 9 statements; multi-statement SQLite DDL/DML with UTF-8 comments and Chinese string literal), exact executed CLI command and Python sqlite3 predicate source snapshotted in `README.md` (fresh `:memory:` database per call requiring `SELECT a FROM t WHERE a = 7` to return row `(7,)`), minimal SQL (`out/minimal.sql`, 125 B, 2 statements, preserving UTF-8 comment and required table), stats (`out/stats.json`), and complete log (`smoke.log`).
  - Observed CLI Outcome: Status `completed` (exit code 0), 485 B → 125 B (-74.2%), 17 database observations, 19 cache hits, 14 cache misses, exactly two uncached final validation observations (`validation_observations: 2`, `validation_passed: true`), total elapsed time 0.890s under 2s timeout / 30s budget / 200 cap protocol with `+RTS -N1 -RTS`.
- **Native Rust SourceMap Flattening Experiment (Declined)**:
  - An exploratory Rust worker evaluated flattening line index lookups in `rust/sqlparser-ffi/src/source.rs` (detailed in `benchmarks/benchmark_artifacts/rust-sourcemap-perf-20261114/SUMMARY.md`).
  - Microbenchmark and interleaved FFI timings showed mixed results (+2.9% CPU latency on ASCII inputs, other signals mixed) without reliable latency wins.
  - The production Rust code remains the original `Vec<Vec<usize>>` implementation with one Unicode/bounds regression test added. No Rust performance win or allocation reduction is claimed; only Haskell general performance improvements are retained in shipped code.
## External experiment line (reference adapters only)

Under Strategy 3A, external reducer comparisons (SQLreduce, SQLancer) are
explicitly scoped out of the primary evaluation line in favor of strictly
partitioned internal controls and ablations. Existing adapter notes below
describe optional reference integration behavior only; they do not form part of
the primary evaluation scope.

If a future scope selects these adapters, SQLancer requires a Git checkout at [`a7d60ee`](https://github.com/sqlancer/sqlancer/tree/a7d60ee450e6a22ca38599c14e984a49773f4745), a JDK, and its built JAR/dependency directory. Set `SQLANCER_SOURCE` and run `mvn -q -Dmaven.test.skip=true package -f "$SQLANCER_SOURCE/pom.xml"` once before measurement; the adapter no longer fetches or rebuilds on each input. This prerequisite inventory is not a selection.

SQLreduce's optional adapter requires a checkout at [`568c8a9`](https://github.com/credativ/sqlreduce/tree/568c8a9a31601ead7128d0b067658b4d5a26821d), exposed through `SQLREDUCE_SOURCE`. An exercised optional environment used pglast 8.7, psycopg2-binary 2.9.13 and PyYAML 6.0.3. These are adapter-specific prerequisites, not an Occam core dependency or external-line decision.

The existing adapter code is wired to invoke upstream search through the common external predicate: SQLancer's StatementReducer/ASTBasedReducer use a virtual JDBC provider, while SQLreduce's AST/rules use a replaced candidate-execution callback. This implementation note is not evidence of a selected or qualified external experiment line, and neither adapter uses its project's native DBMS oracle. SQLreduce accepts complete PostgreSQL scripts; its old single-SELECT/setup/DSN/SQLSTATE adapter flags and reconnect loop are gone. Keep the same input, reset and predicate across any future comparison; `reduction_track` is a per-input label, not a tool-specific input substitution.

Scoped adapter checks have exercised SQLite and PostgreSQL multi-statement predicates; that compatibility is not representative workload or DBMS qualification. SQLreduce rejects non-PostgreSQL dialects as unsupported. SQLancer/JSQLParser coverage is partial and unsupported AST statements are counted. Other dialects, escaped-string modes and procedural bodies need their own evidence. The shared checker distinguishes rejection, timeout, infrastructure failure and execution-budget exhaustion, keeps accepted checkpoints, and does not cache control failures.

Predicate stdout/stderr are opaque, and every raw nonzero predicate exit (including 124/125/126) rejects SQL. Wrapper-owned failures are carried by fresh typed JSONL audit records, not log text. The internal checker translates those records into its explicit 0/1/124/125/126 protocol; both upstream adapters stop on an inconclusive timeout instead of treating it as rejection.

`oracle_exec.py` waits on each leader with a dedicated blocking-wait reaper thread under the same absolute deadline, verdict taxonomy, and JSONL audit contract; waiter errors propagate through the existing cancel path and verdicts follow leader exit rather than stream EOF. This is a generic external-oracle adapter latency change only (native search untouched): the paired same-protocol probe (`benchmark_artifacts/oracle-latency-paired-20261116-a/paired.json`, frozen 9ba1c269 consumer, AB/BA rotation) measured median -28.2ms per oracle observation over 10 kept pairs (10/10 negative, all accepted; sleep-0.07 control -48.8ms, 3/3), under recorded host startup-timing drift. The fresh 32-child score (`benchmark_artifacts/external-comparison-eventwait-20261116-a/`, analysis in `external-comparison-eventwait-analysis-20261116-a/report.md`) is an observed fresh cohort, not a same-binary RCT: before ran f9b9e0ce/Reducer-177cfd87, after runs 57de9a75/Reducer-32a483df (drift is the committed fmt/fromMaybe mechanical fix; same phase-forward-sweep policy), and all other scripts/predicate/interpreter are identical except oracle_exec 9068a812->5b36f6b2. After-occam completed 6/16 (8 timeouts, 2 CTE-error validated checkpoints) and after-sqlancer 10/16 (6 timeouts), every output twice-validated; cross-tool joint completion is 4 slots (upsert x2, statements-64 x2) at wall 1.07/obs 0.66. SQLancer before/after has no native drift (same pin and predicate), but its old/new full scores are not interleaved: 6 jointly completed pairs at median before/after wall 1.25 with identical observation counts is an observed joint-completion ratio, not exclusive causal proof or a guaranteed gain. Selected SQLite seed/scale inputs from local checkouts only (not fresh-checkout packaging); scale_study lazy-import scope intentionally deferred (guard-bound SHA); no censored speedups or global claims.

Outer initial/final checks are reported separately. Native execution caps include native preflight/final checks; callback caps cover shared-checker observations, including adapter format probes. Adapter overhead counts toward the wall deadline. Do not present differing internal validation protocols as identical callback counts; compare their scope and final fresh-state checks explicitly.

## Dependency reconstruction versus SQL state slicing

[DRReduce's full paper](https://arxiv.org/html/2605.19412v1) (§§3–3.3) classifies provider/user/conditioner nodes, applies DDMin with typed default-value reconstruction or associated-node deletion, then delegates to Perses. Its evaluated frontends are C/Java through IntelliJ/CLion PSI, not SQL sessions or catalogs. The [public artifact](https://github.com/XYZboom/DRReduceData) contains compiler workloads/predicates/results; its reconstruction-named Python script plots results rather than implementing a SQL reducer.

Occam instead tracks ordered SQL definitions, uses, writes, drops, renames and generations, plus recognized transaction/savepoint catalog snapshots. Conservative barriers retain uncertain state; unrestricted deletion lets the supplied predicate remove false dependencies. Unchanged source bytes are retained, without DRReduce-style typed identifier repair. These are scoped engineering differences, not proof of a novel or universally superior method. DRReduce excludes some C expression reductions and leaves Java inheritance propagation incomplete; neither its compiler timings nor its other-language limitations establish SQL performance. Use the controlled SQL profiles above for SQL-specific evidence.

## Internal ablation reporting

`benchmarks/tools/partitioned_report.py` (and `scale_study.py report`) derives paired deltas only within ONE qualified current run directory (`run.json` + `results.jsonl`). It enforces strict protocol compatibility:
- Pairing requires exact match on `case_id`, `input_sha256`, `reduction_track`, `predicate_command_sha256`, `reset_command_sha256`, `case_contract_sha256`, `repetition`, limits (`oracle_timeout_sec`, `budget_sec`, `max_oracle_executions`, `outer_timeout_sec`), and resource/count scopes. Raw row records must explicitly contain each declared identity key (including explicit `null` for unconfigured reset commands; missing keys fail row validation).
- Native reduction settings must match the declared internal contrast contract:
  * `full` vs `no-cache` (memoization ablation): differs ONLY in candidate cache memoization (`True` for base `full`, `False` for target `no-cache`), with identical active reduction passes (`kind: "shared-reducer"`, `slicing: true`, `closure: true`, `intra_statement: true`, `unrestricted: true`, matching `dialect`).
  * `full` vs `graph-off` (dependency graph ablation): differs ONLY in dependency graph passes (`slicing` and `closure` both `True` for base `full`, both `False` for target `graph-off`), with identical candidate cache (`cache: true`), active intra-statement pass (`intra_statement: true`), unrestricted 1-minimality audit (`unrestricted: true`), shared-reducer kind (`kind: "shared-reducer"`), and matching dialect. Weaker statement-only baselines (`statement-ddmin`, `sequential-deletion`) are strictly rejected.
  * Unknown contrasts, missing/null/malformed settings, asymmetric keys, or non-target setting differences refuse to pair and fail qualification.
- Profiles and child IDs are comparison sides, NOT equality keys.
- Metrics: `oracle_calls` counts spawned predicate observations; `candidate_checks` counts attempted observations (debited before reset). Quality and cost metrics remain in their own units.
- `unattributed_wall_time_sec` is residual wall-clock time minus predicate and reset duration; it reflects runtime overhead, scheduling, and parser latency, NOT CPU attribution.

### Required `case_contract` schema

For qualified case admission, `manifest.json` cases carry an explicit `case_contract` dictionary (with canonical `case_contract_sha256 = hashlib.sha256(json.dumps(case_contract, sort_keys=True).encode("utf-8")).hexdigest()`):
- `effect_scope`: string (e.g. `"candidate-local SQLite file-state control via OCCAM_WORKDIR"`)
- `comparison_policy`: string (e.g. `"exact ordered-row comparison against expected_rows"`)
- `declared_scope`: string (e.g. `"one SQLite file-state control only; not primary DBMS/external tool selection"`)
- `environment`: object with `python_executable`, `python_version`, and `sqlite_version`
- `hashes`: object containing 64-char hex strings for `sql_sha256` (must agree with `input_sha256`), `metadata_sha256`, and `helper_sha256`, plus explicit `template_sha256` (64-char hex string for file-state fixtures requiring template DB restoration, or explicit `null` when no template DB is used, e.g. in-memory or fresh-seed cases)
- Case configuration and raw row records must declare explicit `reset_command_sha256`: 64-char hex string when an external reset command is configured, or explicit `null` when no reset command is configured.

Declaration and fingerprints record declared protocol and effect scope, not automatic oracle certification or DBMS bug reproduction. Running `prepare` alone does not qualify engine evidence.

### Verified full vs no-cache control slice

- **Pass and reduction contract**: Traced and verified from `bench/Benchmark.hs`, `src/Occam/Reducer.hs`, and `src/Occam/Evaluation.hs`. Both profiles execute identical ordered mechanisms: after preflight, an outer shrinking loop runs backward dependency slicing (`reductionSlicing: true`), DAG closure delta debugging (`reductionClosure: true`), unrestricted statement deletion fallback (`reductionUnrestricted: true`), and an inner fixed-point loop (`interleaveFixedPoint`) alternating single-statement audit (`statementAuditWithLogger`) THEN intra-statement syntax rewrites (`reduceSyntaxWithDialect`). `src/Occam/Evaluation.hs` then applies a separate final shared statement audit (`statementAuditWithLogger`), achieving statement deletion 1-minimality only when audit is confirmed (not a global SQL minimum), followed by 2 uncached physical validation checks (`validateTwice`). The named difference is strictly candidate memoization during reduction (`evaluationCacheEnabled: true` in `full` vs `false` in `no-cache`).
- **Case provenance & helper qualification**: Input derived from `test/golden/fuzz_golden_03_deep_expressions.golden.sql` (47 bytes, input SHA256 `12f182722a25864cfc7a0deeca2c8d3ff692a90c35b89d0d200a597fdbe4b3cc`). Comparison semantics enforce exact ordered-row equality against expected rows `[[1, 100.5], [2, 250.75]]`. Standalone helper `sqlite_case_runner.py` is qualified with clean reset, positive control, wrong-row negative control (`rc=1`), syntax-error negative control (`rc=1`), contamination detection (`rc=125`), and clean restoration (`helper_qualification.json`).
- **Execution and reporting commands**:
  The command below records local historical evidence; for new runs, choose a NEW, empty `--out` directory (existing directories are preserved). Local gitignored evidence is not fresh-checkout-ready packaging.
  ```bash
  python3 benchmarks/tools/scale_study.py profiles \
    benchmarks/benchmark_artifacts/internal-ablation-20261114-a/fixture/manifest.json \
    --binary dist-newstyle/build/x86_64-linux/ghc-9.10.3/occam-0.1.0.0/build/occam-eval/occam-eval \
    --out benchmarks/benchmark_artifacts/internal-ablation-20261114-a/runs \
    --profile full --profile no-cache --repetitions 2 \
    --timeout 2 --budget 30 --max-oracle-executions 100

  python3 benchmarks/tools/scale_study.py report \
    benchmarks/benchmark_artifacts/internal-ablation-20261114-a/runs
  python3 benchmarks/tools/scale_study.py report \
    benchmarks/benchmark_artifacts/internal-ablation-20261114-a/runs --json
  ```
- **Evidence and observations**: Retained evidence lives in `benchmarks/benchmark_artifacts/internal-ablation-20261114-a` (local gitignored).
  * Reducer-scope physical predicate observations: 6 in `full` vs 26 in `no-cache` **per repetition**, eliminating exactly 20 uncached physical predicate observations in each repetition (`Δ = +20`).
  * Reducer cache hits: 20 in `full` vs 0 in `no-cache` per repetition (`Δ = -20`).
  * Reduction quality: 100% identical across all 4 children (43 bytes, 1 statement, output SHA256 `97a763885d27f6300f5f1e600dbc6051126d9a54e3a98d076d6c4f6f83a8d544`).
  * Denominators: 2/2 comparison slots paired and completed; 0 exclusions.
- **Scope and non-causal caveats**: Runs executed serially in fixed child order without OS filesystem page cache flushes. Residual wall time (0.04s in `full` vs 0.08–0.10s in `no-cache`) is not CPU attribution or a causal speedup claim. Bounded strictly to self-contained SQLite file-state / memory controls under Strategy 3A; external heavy DBMS cluster deployments and external reducer comparisons are explicitly scoped out.


### Stratified 12-case SQLite pilot methodology and provenance

To evaluate internal mechanism deltas beyond the 1-case micro-control, a stratified 12-case SQLite pilot is configured under Strategy 3A (`benchmarks/benchmark_artifacts/sqlite-pilot-20261114-b/`):
- **Workload provenance and generation**: 12 related SQL cases derived from deep-expression fuzzing fixture `test/golden/fuzz_golden_03_deep_expressions.golden.sql` (source SHA256 `5667a182dfcb360bb8a824c0748b5858f36a149c93870517ea9281f97a87612a`) using Python 3.14 / SQLite 3.54.1; these are selected scale/reset suite variants with explicit controls, not independent bugs or representative of SQLite workloads generally:
  * 10 `whole_script` cases using isolated in-memory execution (`:memory:`) without external reset command (`reset_command_sha256 = null`) or template database (`template_sha256 = null`):
    - Statement count scaling: `statements-64` (64 noise inserts), `statements-512` (515 initial statements; declared budget-stress case).
    - Object breadth: `objects-1` (1 table, 128 inserts), `objects-32` (32 tables, 128 inserts).
    - AST expression depth: `depth-1` (shallow), `depth-8` (moderate), `depth-24` (deep).
    - View dependency density: `density-1` (linear chain fan-in 1), `density-4` (diamond chain fan-in 4).
    - Structural barrier control: `false-barrier` (`SELECT; BEGIN; SELECT; COMMIT;`).
  * 2 `fixed_setup_query` file-state cases executing in candidate-local workdirs (`OCCAM_WORKDIR`):
    - `reset-template`: file-state restored from SQLite template database (`reset_backend: "template"`).
    - `reset-fresh`: file-state re-initialized from seed setup SQL (`reset_backend: "fresh"` with `template_sha256 = null`).
- **Control plane and profile plan**: Evaluates three internal profiles—`full`, `no-cache`, and `graph-off`—across 3 serial repetitions (12 cases × 3 profiles × 3 reps = 108 total child runs).
- **Uniform execution limits & resource protocol**:
  * `--budget 35`: cumulative wall budget per child.
  * `--timeout 2`: per-predicate execution deadline.
  * `--max-oracle-executions 500`: shared cap on preflight, reduction, and validation oracle observations.
  * `--outer-timeout 40`: outer child deadline followed by bounded process-group cleanup (not proof that all descendants or escaped processes exited).
  * `GHCRTS="-N1"`, serial single-job execution (`jobs=1`), inherited CPU affinity.
- **Oracle and helper qualification**: Standalone helper `sqlite_case_runner.py` (SHA256 `27bfbc67bfb3c8ca4bece4a822f7e9d77647065d627e783ef55bb8e7074880f6`) is pre-qualified across all 12 cases (`helper_qualification.json`):
  * Positive identity control (`rc=0`),
  * Negative controls: syntax error (`rc=1`), empty SQL (`rc=1`), wrong rowset values (`rc=1`),
  * Contamination rejection: prior-session tables absent in fresh in-memory connections and corrupted user_version state rejected,
  * Reset recovery: clean state restoration on template and fresh seed backends.
- **Execution order and caveats**: Child processes execute serially in fixed `(case, profile, repetition)` order without flushing the OS page cache between runs. Raw artifacts and runs reside in gitignored local storage (`benchmarks/benchmark_artifacts/sqlite-pilot-20261114-b/`); these generated cases measure reduction and caching mechanics under Strategy 3A, not third-party DBMS bug reproduction, external cluster concurrency, or general SQLite behavior.
- **Observed pilot outcomes and denominators** (`runs/results.summary.json`):
  * Total child runs: 108 (71 completed, 36 timeout, 1 error; serial makespan 2091.58s).
  * `whole_script` track (10 cases × 3 profiles × 3 reps = 90 runs): `full` 18 completed / 11 timeout / 1 error; `no-cache` 11 completed / 19 timeout; `graph-off` 24 completed / 6 timeout.
  * `fixed_setup_query` track (2 cases × 3 profiles × 3 reps = 18 runs): `full` 6/6 completed; `no-cache` 6/6 completed; `graph-off` 6/6 completed.
- **Candidate memoization evidence (`full` vs `no-cache`)**:
  * 17 of 36 comparison slots completed jointly (11 `whole_script`, 6 `fixed_setup_query`; 19 censored due to target timeout).
  * Output identity on completed pairs: 17/17 (100.0%) byte-for-byte identical outputs.
  * Across all 6 completed cases, memoization observed fewer physical database oracle executions per case (median delta: `density-1` +127 calls, `depth-1` +68 calls, `statements-64` +66 calls, `reset-fresh` +20 calls, `reset-template` +20 calls, `false-barrier` +14 calls).
  * In this bounded cohort under the 35s protocol, `full` had fewer noncompleted `whole_script` children than `no-cache` (refer to per-track denominator table above). Disclosing selection bias: evaluating completed-only pairs filters out harder uncached timeout workloads.
- **Dependency graph ablation evidence (`full` vs `graph-off`)**:
  * 24 of 36 comparison slots completed jointly (18 `whole_script`, 6 `fixed_setup_query`; 12 censored: at least one side reported timeout; density-4 rep2 additionally contains the raw infrastructure error).
  * Output identity on completed pairs: 24/24 (100.0%) byte-for-byte identical outputs. Disabling dependency graph passes did not cause quality loss on any completed case.
  * Case-dependent tradeoffs (reporting wins and regressions honestly without pre-framing):
    - On `objects-1` (median delta -84 calls: 137 vs 221), `objects-32` (median delta -47 calls: 174 vs 221), `statements-64` (median delta -27 calls: 130 vs 157; rep 1: full 157 vs graph-off 130, same 517 B), and `density-1` (median delta -14 calls: 115 vs 129), `graph-off` completed with fewer oracle calls. On `statements-512` (declared budget stress) and `depth-8`, `graph-off` completed within budget (151 and 372 calls) while `full` reported timeout/budget exhaustion on all 3 repetitions.
    - On shallow AST expression scaling (`depth-1`), `graph-off` incurred +7 more oracle calls (+5.0%, 148 vs 141 calls) and +2.120s median wall time.
    - On single-statement fixed-setup inputs (`reset-fresh`, `reset-template`), graph passes had zero delta (identical 6 and 6 calls). On the 4-statement whole-script transaction control fixture `false-barrier` (`SELECT; BEGIN; SELECT; COMMIT;`), graph passes also showed zero delta (identical 9 calls).
- **Censored cases and selection bias**:
  * `statements-512` (whole_script, 515 initial stmts): Declared budget-stress case. `full` (3/3) and `no-cache` (3/3) reported timeout/budget exhaustion under the 35s protocol; `graph-off` completed (3/3, 151 calls, wall 18.04s–21.06s, median 18.88s). Both pairs censored due to base timeout.
  * `depth-8` (whole_script, 19 initial stmts): Moderate AST expression depth. `full` (3/3) and `no-cache` (3/3) reported timeout/budget exhaustion under the 35s protocol; `graph-off` completed (3/3, 372 calls, wall 26.89s–27.46s, median 27.20s). Both pairs censored due to base timeout.
  * `depth-24` (whole_script, 19 initial stmts): Deep AST expression depth. All three profiles reported timeout/budget exhaustion (3/3) under the 35s protocol.
  * `density-4` (whole_script, 8 initial stmts): `no-cache` and `graph-off` reported timeout/budget exhaustion (3/3). `full` reported timeout/budget exhaustion on repetitions 1 and 3; repetition 2 exited with `status: "error"` due to a retained process-management/infrastructure error (`stderr.log:1`: `occam-eval: waitForProcess: does not exist (No child processes)`, classified as `OracleIOFailed`/`InfrastructureError` in `Occam.Oracle`, exit code 1, native validation passed true, predicate passed null; exact underlying process cause unestablished without claiming a proven race or reaping cause from a single log). In pair evaluation, the `density-4`/`full`/rep2 slot is labeled timeout in each contrast because its target timed out, while the raw full child is separately an infrastructure error.
  * `objects-1` & `objects-32` (whole_script, 131 initial stmts): Object breadth scaling. `full` and `graph-off` completed 3/3; `no-cache` reported timeout/budget exhaustion (3/3; calls 244–267), censoring `full-vs-no-cache`.
  * `density-1` (whole_script, 8 initial stmts): Linear view chain. `full` and `graph-off` completed 3/3; `no-cache` completed 2/3 and reported timeout/budget exhaustion on repetition 2 at 31.31s (243 calls).
- Full track-separated denominators, case tables, and medians are retained in `benchmarks/benchmark_artifacts/sqlite-pilot-20261114-b/track_summary.{json,md}`.
## Supported partitioned reporting control plane

The supported partitioned report tool is `benchmarks/tools/partitioned_report.py` (also accessible via `python3 benchmarks/tools/scale_study.py report <run_dir>`). It enforces strict partition validation: rejecting cross-line, cross-protocol, cross-track, or mismatched-pass pairs.

```bash
# Memoization ablation: candidate cache enabled vs disabled
python3 benchmarks/tools/partitioned_report.py <run_dir> --base full --target no-cache
python3 benchmarks/tools/partitioned_report.py <run_dir> --base full --target no-cache --json

# Dependency graph ablation: dependency graph passes enabled vs disabled
python3 benchmarks/tools/partitioned_report.py <run_dir> --base full --target graph-off
python3 benchmarks/tools/partitioned_report.py <run_dir> --base full --target graph-off --json
```

Legacy unpartitioned presentation scripts (`benchmarks/report.sh`, `benchmarks/run_all.sh`) have been retired.
Generate a synthetic SQL stress input directly when needed:

```bash
python3 benchmarks/tools/fuzz_generator.py \
  --dialect postgres --stmts 100 --depth 12 --inject pg_01 --out /tmp/fuzz.sql
```

## Appendix: Legacy multi-tool manifest runner (reference only)

### Manifest and output format

The comparison runner accepts a JSON manifest with one shared predicate and per-case SQL, dialect, and optional reset. No bug provenance or crash-signature fields are required; case files may be relative to the manifest or repository.

This legacy runner can dispatch internal and optional external tools in one batch. Do not use mixed selections or its default tool list as a comparison across the two lines; line-specific orchestration/reporting remains a separate cutover. Existing output remains mechanics/history only.

```json
{
  "oracle_command": "./interesting.sh",
  "cases": [
    {
      "id": "fuzz-001",
      "file": "inputs/fuzz-001.sql",
      "dialect": "postgres",
      "reset_command": "./reset-db"
    }
  ]
}
```

Run the built-in reducers without making optional external downloads prerequisites:

```bash
python3 benchmarks/fuzz_corpus/run_docker_benchmark.py \
  --manifest cases.json --tools occam,ddmin \
  --oracle-timeout 30 --budget 300
```

The runner defaults to optional external reducers as well; because it does not enforce line separation, avoid the default or any cross-line tool list for experiment claims. `--out` selects the JSONL result file and `--artifacts-dir` the per-run artifacts. The default manifest uses synthetic text-marker fixtures; historical JSONL records from that corpus are mechanics data, not validated DBMS results.

Each JSONL line stays a per-case/tool record. The runner writes a sibling `<stem>.summary.json` sidecar (`results.summary.json` for `results.jsonl`) with processed/expected counts, validated reductions, status counts, throughput, and actual monotonic batch makespan. Rates use that makespan; `wall_time_sec` on each record is worker time, so summed case time is not batch elapsed time (especially with `--jobs`).

For `occam`/`ddmin`, a normally terminated native process may report a recognized evaluation status from a fresh stats/profile file; outer command timeouts, spawn failures, and missing, stale, or unknown profiles are not overridden. A nonzero native exit is never promoted to `completed`. Successful native exit requires `minimal.sql`; missing output is a `tool-error`, not a successful unchanged result. Partial budget/timeout/error outcomes keep `predicate_passed` unknown; `evaluationFailure` is preferred as the reason when available.

Native dialects are passed explicitly to the CLI; unsupported or absent dialects are not reinterpreted as Generic SQL. Native results also require a complete, recognized typed predicate audit matching the fresh profile's process count. Wrapper timeout, interruption, infrastructure failure, or incomplete audit evidence prevents result promotion and preserves the initially accepted checkpoint.

`evidence_class` and `oracle_kind` come from the manifest (or documented runner defaults); reports group and pair by input/evidence/predicate/dialect/track but do not partition by experiment line, execution mode or internal profile family. These labels do not prove SQL execution or identical predicates. Keep different oracle configurations in distinct input IDs; do not treat legacy multi-tool output as a cross-line leaderboard.

Comparison-runner `reset_calls` and `reset_time_sec` cover audited reducer callbacks only; initial/final validation calls are excluded and reported separately. Incomplete reducer audit data stays `null`. A zero reducer-scope count does not mean no reset was attempted: inspect `oracle_audit_file` for validation-phase attempts and failures. The single-input pipeline records `reset_preflight_sec`, `reset_reduction_sec`, and `reset_validation_sec`; `reset_time_sec` sums these stages once only when all are known, otherwise it is `null`.

`oracle_timeouts` counts reducer-scope observations only: native tools use a fresh `evaluationTimedOutObservations` profile count, callback tools use observed callback/reducer-audit outcomes, and validation-phase audit rows are excluded. If a native count is missing or invalid, it remains `null`.

## Parser & JSON Cost Share Investigation (2026-11-14)

An immutable measurement suite in `benchmarks/benchmark_artifacts/json-cost-20261114/` evaluates the exact matching-call cost share of the current JSON transport pipeline between Haskell and the Rust native sqlparser FFI across four canonical workloads (`smoke_9`, `scale_512`, `ascii_1000`, `cjk_500`):

1. **Haskell JSON Deserialization Dominates (Aeson Value AST: ~50% – 62% at Scale)**: Supplementary two-step diagnostic split runs demonstrate that Aeson JSON text parsing, `Text`/`KeyMap` allocations, and `Value` syntax tree construction account for **49.8% – 62.5%** of split time at scale (**28.9%** on tiny `smoke_9`). Combined with typed domain construction (`FromJSON` conversions, `makeUnits` UTF-8 slicing, and ADT materialization: **16.1% – 22.1%**), the Haskell decode and materialization upper envelope reaches **71.0% – 82.3%** of full parse time at scale.
2. **Partial Codec Cost Excluding Haskell Decoder Is Minor (2.4% – 2.8% at Scale)**: The isolated partial codec cost—Haskell config encode + Rust config decode (`serde_json::from_slice`) + Rust response JSON encode (`serde_json::to_vec`)—accounts for only **2.38% – 2.84%** of full parse time at scale (**7.86%** on `smoke_9`). Rust response encoding alone is **2.35% – 2.70%** at scale.
3. **Buffer Transport Overhead Is Minimal (<0.4%)**: Pointer export, copying into a managed ByteString (`BS.packCStringLen`), and buffer freeing (`c_freeSQL`) together account for **0.17% – 0.38%** of full parse time.
4. **Rust Core Parsing**: AST parsing, tokenization, semantic facts, and edit calculations consume **80% to 87%** of native FFI time (**14.3% to 45.3%** of full parse time).
5. **Reducer Invariants & Limits**: A single-run trace of `statementDdmin` on `workload_scale_512.sql` confirms that initial input ingestion accounts for 95.15% of that run's 66.8 ms runtime (partial codec cost 2.82%), with all subsequent 19 candidates evaluated via pure statement surgery (`cutRemovedStatements`). Candidate traces and minimal outputs match **100% byte-for-byte via `cmp`** across baseline, same-forcing, and profiled modes, and match golden reference `benchmarks/benchmark_artifacts/haskell-perf-20261114/trace_after.bin`.

Full immutable tables, weighted-sum aggregations, paired observer overhead accounting, raw logs, and source snapshots are documented in [`benchmarks/benchmark_artifacts/json-cost-20261114/README.md`](benchmark_artifacts/json-cost-20261114/README.md).

## List batching: measured gain and limitation (2026-11-16)

The intra-statement List phase under `phase-forward-sweep-list-chunks` offers coarse-to-fine contiguous chunk deletions before the original single-item deletions in each comma-separated list. Arbitrary oracles (no monotonicity assumption), exact source spans, and strict byte shrink are unchanged; only the syntax pass requests chunks per parse.

| | before (`57de9a75`) | new (`0c0febfe`) |
|---|---|---|
| 64-row proof reduction db observations | 78 | 31 |
| 64-row proof reducer wall (s) | 12.978 | 5.428 |
| 64-row proof final bytes | 67 | 67 |
| primary completed/timeout/error | 6/8/2 | 6/8/2 |
| primary recorded attempts at cutoff | 1214 | 1273 |

The 64-row SUM feature proof is a single paired trial (same adapter/predicate, both completed, identical final SQL, twice-validated) and is not pooled with the primary. The primary is 8 SQLite cases × 2 reps × 2 tools = 32 children (16 per tool) under a 20s budget, 1s per-call timeout, and one CPU. Primary raw-result-byte medians and per-slot statuses/bytes are identical before/after for Occam (6/8/2 both); the +59 attempts are observed at cutoff (mixed complete+censored), and jointly completed old/new pairs show median wall 1.05 old-over-new with observation ratio 1.00 (cohorts noninterleaved; wall ratio not isolated causal proof) — no primary completion/quality gain, no pooled or censored speedup claim, no global-superiority claim.

Canonical evidence: [`list-chunk-proof-20261116-a/proof_result.json`](benchmark_artifacts/list-chunk-proof-20261116-a/proof_result.json), [`external-comparison-list-chunks-20261116-a/`](benchmark_artifacts/external-comparison-list-chunks-20261116-a/), and [`external-comparison-list-chunks-analysis-20261116-a/report.md`](benchmark_artifacts/external-comparison-list-chunks-analysis-20261116-a/report.md).

## Column ablation: predicate sidecar, three-arm runner, mechanism fixtures (2026-11-17)

Predicate diagnostic sidecar (metrics only): setting `OCCAM_ORACLE_DIAG=<jsonl>`
makes `tools/scale_study.py oracle` append one bounded row per verdict —
`{"reason": accepted|sql_error|result_mismatch, "stdin_sha256": <exact STDIN
bytes>}` — keyed by the raw input bytes (CRLF/UTF-8 significant, matching the
wrapper audit digest), never SQL text or DB error strings. Exit codes and
stdout/stderr bytes are unchanged; when unset, the sidecar path performs no
hash/serialization/file IO. A failed ENABLED write leaves no row, preserves the
verdict, and emits one bounded stderr warning (the documented exception to byte
identity, which still holds on all normal paths); analysis additionally reports
digest join coverage as incomplete. Appends are single small atomic writes
(line-buffered O_APPEND), isolated per call.

Three-arm runner `tools/run_three_arm.py` (internal line): strict 8 cases × 2
reps × arms `column-off`/`column-single`/`column-atomic` via native `occam-eval
--profile`, reusing the baseline resource/budget/count/final-check protocol
(20s wall, 1s per call, one CPU, `GHCRTS=-N1`, twice-validated checkpoints).
Each child records its native `evaluationCostAttribution` section and, with
`--diag`, sidecar completeness against audited digests. Reports per-arm
completed/censored tables, joint-completion ratios only, cost presence, and
diag coverage — no pooled speedups, no winner claims.

Mechanism proof fixtures in `benchmarks/column_proof/` (`explicit`,
`implicit`, `star`): tiny SQLite cases with verified 2-row rowsets that
disentangle listing coverage from joint search. Throwaway proof only, separate
from the primary cohort and never pooled with it.

## Column co-reduction: conclusions (2026-11-17)

- Three-arm triad (off/single/atomic): joint-completion ratios only; atomic is not generally faster; checkpoint size is not completion.
- A2/B emission gate: identical reduction behavior on all matched slots; statement timers complete the candidate-build scope in A2/B.
- Missing diagnostics are unavailable/incomplete, never 0; never compare A2/B cost tables against A scope-incomplete data. Full tables live in the frozen bundle reports under `benchmarks/benchmark_artifacts/`.

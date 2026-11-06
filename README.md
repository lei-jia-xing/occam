# Occam

> *Entia non sunt multiplicanda praeter necessitatem.* (Entities should not be multiplied beyond necessity.)

**Occam** minimizes multi-statement SQL while preserving a supplied interestingness predicate. Existing fuzz SQL, including its schema and setup statements, is the input; collecting real DBMS bugs is not a prerequisite.

## Predicate contract

Pass the complete candidate SQL to an external command on stdin. Exit status 0 accepts it; every nonzero status, including signal exits, rejects it. Stdout and stderr are opaque except `OCCAM_INFRASTRUCTURE:` diagnostics. Occam does not interpret crashes or automatically treat failures as interesting.

```bash
cabal run occam -- input.sql \
  --oracle './interesting.sh' \
  --reset './reset-db' \
  --timeout 30 --budget 300 --max-oracle-executions 1000 \
  --out /tmp/occam-result
```

`--reset` is optional. The initial input must pass before reduction. Per-call, total-time, and uncached-execution budgets include the initial and final checks; when budgets permit, Occam performs two uncached final validations. On exhaustion, it preserves the last accepted SQL and reports a nonzero status. `stats.json` reports available measurements; tokens can be unknown.

## Candidate state

Each predicate call gets a fresh `OCCAM_WORKDIR`, also used for `TMP`, `TMPDIR`, and `TEMP`. The reset command and predicate share it; the original working directory is preserved and exported as `OCCAM_ORIGINAL_CWD`. Occam does not isolate or reset a real database for you: the external predicate/reset must provide the needed database state and isolation. Owned POSIX process groups are cleaned up; descendants that leave the group and full Windows process-tree behavior are outside the verified contract.

## Architecture

- Tree-sitter C grammar bindings via FFI for exact byte-span SQL edits.
- Haskell dependency analysis, backward slicing, and dependency-constrained delta debugging.
- External predicates define the behavior being preserved; Occam does not launch or configure a DBMS.

## Building

```bash
cabal build
cabal test
```

See `benchmarks/README.md` for processing existing fuzz SQL and comparing reducers.

## License

MIT License.

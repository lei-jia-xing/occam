# Occam

> *Entia non sunt multiplicanda praeter necessitatem.*

**Occam** is a dependency-aware test-case reducer for multi-statement SQL. Given a SQL script and an interestingness predicate, it minimizes statements (and optionally intra-statement syntax and columns) while preserving the target behavior.

---

## Quick Start

### External predicate (`--oracle`)

```bash
occam input.sql \
  --oracle './interesting.sh' \
  --reset './reset-db.sh' \
  --dialect postgresql \
  --timeout 30 --budget 300 --max-oracle-executions 1000 \
  --out reduced.sql
```

- **Input**: pass a file path, or `-` to read SQL from `stdin` (never implicit).
- Reduced SQL goes to `stdout` by default. Use `--out PATH` to write a file or `--in-place` (`-i`) to replace the input with a `.bak` backup after final validation. The replacement and backup inherit the input's permission bits.
- **`--reset`** is optional and runs before each oracle invocation.

```bash
# Pipe from stdin to stdout
cat crash.sql | occam - -d sqlite -c './interesting.sh' > reduced.sql
```

### In-process result oracle (`--result-oracle`)

Preserve the last result set (ordered rows, column names, and value types) using an embedded `:memory:` SQLite or DuckDB engine, replacing both `--oracle` and `--reset`:

```bash
occam input.sql --dialect sqlite --result-oracle --out reduced.sql
occam input.sql --dialect duckdb --result-oracle --out reduced.sql
```

Trailing statements after the last result set must still execute cleanly. Use deterministic queries (`ORDER BY`) to fix row order. SQLite starts with foreign keys disabled unless enabled in the script (`PRAGMA foreign_keys = ON`).

---

## CLI Summary

Run `occam --help` for the full flag list, or use `--list-dialects`, `--list-strategies`, `--list-column-modes`, and `--print-defaults`.

| Flag | Purpose |
| :--- | :--- |
| `-c, --oracle COMMAND` | External predicate reading candidate SQL on `stdin` (exit `0` accepts) |
| `--reset COMMAND` | State reset command executed before each oracle check |
| `--result-oracle` | In-process result-preserving oracle (`--dialect sqlite` or `duckdb`) |
| `-d, --dialect DIALECT` | SQL dialect (**required**; 6 active dialects via `--list-dialects`) |
| `-o, --out PATH` / `-i, --in-place` | Output destination (`-` / `stdout` default) or safe in-place replacement |
| `--stats FILE` | Write structured JSON reduction and evaluation metrics |
| `--timeout` / `--budget` / `--max-oracle-executions` | Per-probe timeout (`10s` default), total wall-time budget, uncached attempt cap |
| `-j, --jobs N` | Max concurrent oracle/reset executions (`1` default; caller must isolate DB state) |
| `--strategy STRATEGY` | `full` (default), `ddmin-only`, or `audit-only` |
| `--intra-statement` / `--column-mode MODE` | Opt-in exact-span intra-statement reduction and column co-reduction (`off`\|`single`\|`atomic`) |
| `--completion bash\|zsh\|fish` | Emit shell completion script |

**Exit codes**: `0` completed & validated · `1` initial predicate failed or runtime error · `2` CLI usage/validation error · `124` time or execution budget exhausted (last accepted checkpoint preserved).

---

## Predicate & Isolation Contract

- **Verdict**: Occam pipes candidate SQL to `--oracle` on `stdin`. Exit `0` accepts; **every nonzero exit** (including `125` and signal exits) rejects. `stdout` and `stderr` are opaque and never determine the verdict.
- **Control outcomes**: timeouts, spawn/I/O failures, and `--reset` failures are handled separately from rejections. Occam does not interpret crashes automatically.
- **Workdir & concurrency**: each run gets a fresh `OCCAM_WORKDIR` (also `TMP`/`TMPDIR`/`TEMP`) and `OCCAM_ORIGINAL_CWD`. This is filesystem hygiene only—Occam never isolates or resets a DBMS for you. With `--jobs > 1`, `--oracle` and `--reset` must safely support concurrent independent database state.

See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for process-group reaping, budget reservation, scheduler semantics, and `stats.json` schema details.

---

## Building & Testing

`nix develop` provides GHC, Cabal, Rust/Cargo, Fourmolu, HLint, HLS, Clippy, and Python 3. Run `make hooks` once per checkout to install the pre-commit hook.

```bash
make build        # Build Rust FFI + Haskell binary
make test         # Run Haskell + Rust test suites (-Werror)
make check        # Full gate: fmt, hlint, build, tests, clippy, harness tests, Lean 4 proofs
```

- **Dev builds** (`cabal build` / `make build`) dynamically link the Rust `sqlparser-ffi` cdylib via an rpath configured by `Setup.hs`. After editing Rust, run `make build` (bare `cabal build` skips Cargo when Haskell sources are unchanged).
- **Release builds** use `-f static-rust` for a single self-contained binary (Windows is static-only).
- Source distributions bundle the Rust crate and `Cargo.lock` for standalone builds.

---

## Documentation

| Document | Contents |
| :--- | :--- |
| [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) | Pipeline, dependency fences, ordered scheduler, oracle/process contract, `stats.json`, verification boundaries |
| [docs/DIALECTS.md](docs/DIALECTS.md) | 6 supported SQL dialects, modeled dependency effects, and explicit opaque boundaries |
| [docs/UPSTREAM_TRACKER.md](docs/UPSTREAM_TRACKER.md) | Upstream `sqlparser-rs` gaps, workarounds, and local patching workflow |
| [benchmarks/README.md](benchmarks/README.md) | 4-tier benchmark suite, flame graphs, and search/result baseline scripts |
| [benchmarks/real_fuzz_corpus/README.md](benchmarks/real_fuzz_corpus/README.md) | 71 multi-statement real DBMS crash cases across 6 DBMS targets |
| [harness/README.md](harness/README.md) | Crash oracle, target DBMS containers, fuzzer drivers, and batch evaluation |
| [proofs/README.md](proofs/README.md) | Lean 4 formal proofs for DAG, reduction convergence, and 1-minimality |

---

## License

MIT License. See [LICENSE](LICENSE).

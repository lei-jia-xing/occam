<p align="center">
  <img src="docs/assets/occam-logo.png" width="180" alt="Occam logo" />
</p>

<h1 align="center">Occam</h1>

<p align="center">
  <em>Entia non sunt multiplicanda praeter necessitatem.</em><br/>
  Pure-Haskell, dependency-aware test-case reducer for multi-statement SQL.
</p>

---

**Occam** minimizes multi-statement SQL scripts (plus optional exact-span intra-statement syntax and co-reduced columns) while preserving an interestingness predicate or embedded result set across **PostgreSQL**, **MySQL**, **MariaDB**, **SQLite**, **DuckDB**, and **ClickHouse**.

---

## Installation

### Nix (Flakes)

Run directly without installing, or install into your profile (includes shell completions and `libduckdb` support for `--result-oracle --dialect duckdb`):

```bash
# Run once
nix run github:lei-jia-xing/occam -- --help

# Install into Nix profile
nix profile install github:lei-jia-xing/occam
```

For NixOS / Home Manager flake configurations, `overlays.default` provides `pkgs.occam`, and `packages.<system>.default` exposes the standalone wrapped binary.

### Arch Linux (AUR `PKGBUILD`)

A ready-to-publish AUR `occam-git` package definition lives in [packaging/aur/PKGBUILD](packaging/aur/PKGBUILD) (with [packaging/aur/.SRCINFO](packaging/aur/.SRCINFO)), building against Arch `extra/ghc` (`9.6.7`) and `extra/haskell-*` packages without requiring GitHub releases:

```bash
cd packaging/aur
makepkg -si
```

Install optional `duckdb` if using `--result-oracle --dialect duckdb`.

### From Source (Cabal / Stack)

Occam compiles with GHC `>= 9.6` (`base >= 4.18`).

#### Cabal

```bash
git clone https://github.com/lei-jia-xing/occam.git
cd occam
cabal update
cabal install exe:occam
```

#### Stack

Using [stack.yaml](stack.yaml) (`lts-24.68`, GHC `9.10.3`):

```bash
git clone https://github.com/lei-jia-xing/occam.git
cd occam
stack install
```

Optionally install shell completions via `make install-completions` (or `occam --completion bash|zsh|fish`), and ensure `libduckdb` is available on the system library path (or via `OCCAM_DUCKDB_LIB`) if using `--result-oracle --dialect duckdb`.

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

With `--intra-statement`, the result oracle defers fine-grained `VALUES` chunk deletions until the main rewrite sweep stalls. Use `--no-adaptive-values` to keep all chunk sizes in the main sweep.

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
| `--config FILE` | Load JSON option presets (e.g. from `config/generated/profiles/*.json`); explicit CLI flags override |
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

`nix develop` provides GHC, Cabal, Fourmolu, HLint, HLS, Lean (`elan`), DuckDB, `libpg_query`, and Python 3. `nix develop .#syntax` additionally provides prebuilt ClickHouse `26.8`, MySQL `8.4`, and MariaDB `11.4` binaries for full six-engine syntax conformance without Dockerfiles. Occam has a 100% pure-Haskell SQL frontend with zero FFI/Cargo/C-toolchain dependencies in production binaries. Run `make hooks` once per checkout to install the pre-commit hook.

```bash
make build        # Build Haskell library and occam binary
make test         # Run Haskell test & fuzz suites (-Werror)
make check        # Full gate: fmt, hlint, build, tests, harness tests, Lean 4 proofs
```

`cabal.project` applies `-Werror` consistently and sets package optimization to
`2` for normal and CI builds. For faster local incremental builds, create an
ignored `cabal.project.local` with:

```cabal
package occam
  optimization: 0
```

---

## Documentation

| Document | Contents |
| :--- | :--- |
| [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) | Pipeline, dependency fences, ordered scheduler, oracle/process contract, `stats.json`, verification boundaries |
| [docs/DIALECTS.md](docs/DIALECTS.md) | 6 supported SQL dialects, modeled dependency effects, and explicit opaque boundaries |
| [docs/FRONTEND_GRAMMAR.md](docs/FRONTEND_GRAMMAR.md) | Pure-Haskell strict SQL frontend module map, dialect dispatch, grammar coverage, and seed corpus |
| [benchmarks/README.md](benchmarks/README.md) | 4-tier benchmark suite, flame graphs, and search/result baseline scripts |
| [benchmarks/real_fuzz_corpus/README.md](benchmarks/real_fuzz_corpus/README.md) | 71 multi-statement real DBMS crash cases across 6 DBMS targets |
| [harness/README.md](harness/README.md) | Crash oracle, target DBMS containers, fuzzer drivers, and batch evaluation |
| [proofs/README.md](proofs/README.md) | Lean 4 formal proofs for DAG, reduction convergence, and 1-minimality |

---

## License

MIT License. See [LICENSE](LICENSE).

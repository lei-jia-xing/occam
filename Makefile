.PHONY: all build build-check test bench microbench bench-product fmt fmt-check lint rust-check harness-check proof-check check hooks dev clean completions install-completions

all: build

build:
	cargo build --locked --release --manifest-path rust/sqlparser-ffi/Cargo.toml --target-dir rust/sqlparser-ffi/target
	cabal build

build-check:
	cargo build --locked --release --manifest-path rust/sqlparser-ffi/Cargo.toml --target-dir rust/sqlparser-ffi/target
	cabal build all --enable-tests --ghc-options=-Werror

test:
	cargo build --locked --release --manifest-path rust/sqlparser-ffi/Cargo.toml --target-dir rust/sqlparser-ffi/target
	cabal test all --ghc-options=-Werror
	cargo test --locked --manifest-path rust/sqlparser-ffi/Cargo.toml

# Cargo runs first (its fingerprinting is the freshness mechanism); stale
# -DOCCAM_RUST_STAMP lines in gitignored cabal.project.local are a harmless unused -D.

bench:
	cabal bench --benchmark-options="-p corpus --stdev 20 --timeout 5s"

microbench:
	cabal bench --benchmark-options="-p 'parse || dag || surgery || cache' --stdev 20 --timeout 5s"

bench-product:
	cabal bench --benchmark-options="-p reduction-audit-validation --stdev 20 --timeout 5s"

fmt:
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu -m inplace $$(find src app test bench -name "*.hs") Setup.hs; \
	else \
		nix develop --command fourmolu -m inplace $$(find src app test bench -name "*.hs") Setup.hs; \
	fi
	@if command -v cargo >/dev/null 2>&1; then \
		cargo fmt --manifest-path rust/sqlparser-ffi/Cargo.toml --all; \
	else \
		nix develop --command cargo fmt --manifest-path rust/sqlparser-ffi/Cargo.toml --all; \
	fi

fmt-check:
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu --mode check $$(find src app test bench -name "*.hs") Setup.hs; \
	else \
		nix develop --command fourmolu --mode check $$(find src app test bench -name "*.hs") Setup.hs; \
	fi

lint:
	@if command -v hlint >/dev/null 2>&1; then \
		hlint src app test bench Setup.hs; \
	else \
		nix develop --command hlint src app test bench Setup.hs; \
	fi

rust-check:
	cargo fmt --manifest-path rust/sqlparser-ffi/Cargo.toml --all -- --check
	cargo check --locked --manifest-path rust/sqlparser-ffi/Cargo.toml --all-targets
	cargo clippy --locked --manifest-path rust/sqlparser-ffi/Cargo.toml --all-targets -- -D warnings

harness-check:
	python3 -m unittest discover -s harness -p "test_*.py" -v
	@if command -v shellcheck >/dev/null 2>&1; then \
		shellcheck -S style harness/*.sh; \
	else \
		echo "shellcheck not installed; skipping harness shell checks"; \
	fi

proof-check:
	@if command -v lake >/dev/null 2>&1; then \
		(cd proofs && lake build); \
	else \
		nix develop --command bash -c "cd proofs && lake build"; \
	fi

check: fmt-check lint build-check test rust-check harness-check proof-check


hooks:
	git config core.hooksPath .githooks

dev:
	nix develop

clean:
	cabal clean

completions:
	@mkdir -p completions
	cabal build exe:occam
	BIN=$$(cabal list-bin exe:occam); \
	$$BIN --completion=bash > completions/occam.bash && \
	$$BIN --completion=zsh > completions/_occam && \
	$$BIN --completion=fish > completions/occam.fish

install-completions: completions
	install -D -m 644 completions/occam.bash ~/.local/share/bash-completion/completions/occam
	install -D -m 644 completions/_occam ~/.zsh/completions/_occam
	install -D -m 644 completions/occam.fish ~/.config/fish/completions/occam.fish

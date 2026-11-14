.PHONY: all build build-check test fmt fmt-check lint rust-check benchmark-test check hooks dev clean

all: build

build:
	cabal build

build-check:
	cabal build all --enable-tests --ghc-options=-Werror

test:
	cabal test all --ghc-options=-Werror
	cargo test --locked --manifest-path rust/sqlparser-ffi/Cargo.toml

benchmark-test:
	python3 -m unittest discover -s benchmarks

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

check: fmt-check lint build-check test rust-check benchmark-test


hooks:
	git config core.hooksPath .githooks

dev:
	nix develop

clean:
	cabal clean

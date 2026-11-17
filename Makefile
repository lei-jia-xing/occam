.PHONY: all build build-check test rust-fresh fmt fmt-check lint rust-check benchmark-test proof-check check hooks dev clean completions install-completions

all: build

build: rust-fresh
	cabal build

build-check: rust-fresh
	cabal build all --enable-tests --ghc-options=-Werror

test: rust-fresh
	cabal test all --ghc-options=-Werror
	cargo test --locked --manifest-path rust/sqlparser-ffi/Cargo.toml

# Rust sources are not cabal build inputs: Setup.hs only runs cargo when cabal
# decides to build, so bare `cabal build` after Rust-only edits may link a
# stale staticlib. Always use these make targets (guard detects by content, not mtime).
rust-fresh:
	@if [ -d dist-newstyle ]; then \
		[ -f rust/sqlparser-ffi/Cargo.toml ] && [ -f rust/sqlparser-ffi/Cargo.lock ] || exit 1; \
		fp=$$(sha256sum rust/sqlparser-ffi/Cargo.toml rust/sqlparser-ffi/Cargo.lock rust/sqlparser-ffi/src/*.rs | sha256sum | cut -c1-16); [ -n "$$fp" ] || exit 1; \
		cur=$$(grep '^  ghc-options: -DOCCAM_RUST_STAMP=' cabal.project.local 2>/dev/null | tail -1 | sed 's/.*=//'); \
		if [ "$$fp" != "$$cur" ]; then \
			echo "rust-fresh: Rust fingerprint $$fp differs from stamp ($${cur:-none}); restamping"; \
			touch cabal.project.local; \
			if [ -n "$$cur" ]; then \
				tmp=$$(mktemp) && sed "s/^  ghc-options: -DOCCAM_RUST_STAMP=.*/  ghc-options: -DOCCAM_RUST_STAMP=$$fp/" cabal.project.local > "$$tmp" && mv "$$tmp" cabal.project.local || exit 1; \
			else \
				{ echo; echo 'package occam'; echo "  ghc-options: -DOCCAM_RUST_STAMP=$$fp"; } >> cabal.project.local || exit 1; \
			fi; \
		fi; \
	fi

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

proof-check:
	@if command -v lake >/dev/null 2>&1; then \
		(cd proofs && lake build); \
	else \
		nix develop --command bash -c "cd proofs && lake build"; \
	fi

check: fmt-check lint build-check test rust-check benchmark-test proof-check


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
	$$BIN --bash-completion-script occam > completions/occam.bash && \
	$$BIN --zsh-completion-script occam > completions/_occam && \
	$$BIN --fish-completion-script occam > completions/occam.fish

install-completions: completions
	install -D -m 644 completions/occam.bash ~/.local/share/bash-completion/completions/occam
	install -D -m 644 completions/_occam ~/.zsh/completions/_occam
	install -D -m 644 completions/occam.fish ~/.config/fish/completions/occam.fish

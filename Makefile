.PHONY: all build build-check test fmt fmt-check lint check hooks dev clean

all: build

build:
	cabal build

build-check:
	cabal build all --enable-tests --ghc-options=-Werror

test:
	cabal test all --ghc-options=-Werror

fmt:
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu -m inplace $$(find src app test bench -name "*.hs"); \
	else \
		nix develop --command fourmolu -m inplace $$(find src app test bench -name "*.hs"); \
	fi

fmt-check:
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu --mode check $$(find src app test bench -name "*.hs"); \
	else \
		nix develop --command fourmolu --mode check $$(find src app test bench -name "*.hs"); \
	fi

lint:
	@if command -v hlint >/dev/null 2>&1; then \
		hlint src app test bench; \
	else \
		nix develop --command hlint src app test bench; \
	fi

check: fmt-check lint build-check test

hooks:
	git config core.hooksPath .githooks

dev:
	nix develop

clean:
	cabal clean

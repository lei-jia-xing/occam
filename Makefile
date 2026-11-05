.PHONY: all build test fmt fmt-check lint check hooks dev clean

all: build

build:
	cabal build

test:
	cabal test

fmt:
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu -m inplace $$(find src app test -name "*.hs"); \
	else \
		nix develop --command fourmolu -m inplace $$(find src app test -name "*.hs"); \
	fi

fmt-check:
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu --mode check $$(find src app test -name "*.hs"); \
	else \
		nix develop --command fourmolu --mode check $$(find src app test -name "*.hs"); \
	fi

lint:
	@if command -v hlint >/dev/null 2>&1; then \
		hlint src app test; \
	else \
		nix develop --command hlint src app test; \
	fi

check: fmt-check lint test

hooks:
	git config core.hooksPath .githooks

dev:
	nix develop

clean:
	cabal clean

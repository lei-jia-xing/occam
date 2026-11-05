.PHONY: all build test fmt fmt-check lint check hooks dev clean

all: build

build:
	cabal build

test:
	cabal test

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

check: fmt-check lint test

hooks:
	git config core.hooksPath .githooks

dev:
	nix develop

clean:
	cabal clean

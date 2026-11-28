.PHONY: all build build-check test syntax-check bench microbench bench-product flamegraph fmt fmt-check lint dhall-gen dhall-check harness-check proof-check check hooks dev clean completions install-completions

all: build

build:
	cabal build

build-check:
	cabal build all --enable-tests

test:
	cabal test all

syntax-check:
	@if [ -n "$$OCCAM_CLICKHOUSE_BIN" ] && [ -n "$$OCCAM_MYSQLD_BIN" ] && [ -n "$$OCCAM_MARIADBD_BIN" ]; then \
		cabal test occam-test; \
	else \
		nix develop .#syntax --command cabal test occam-test; \
	fi

bench:
	cabal bench --benchmark-options="-p corpus --stdev 20 --timeout 5s"

microbench:
	cabal bench --benchmark-option=-p --benchmark-option='$$2 == "parse" || $$2 == "dag" || $$2 == "surgery" || $$2 == "cache"' --benchmark-options="--stdev 20 --timeout 5s"

bench-product:
	cabal bench --benchmark-option=-p --benchmark-option='$$2 == "reduction-audit-validation" || $$2 == "audit-product"' --benchmark-options="--stdev 20 --timeout 5s"

FLAMEGRAPH_DIR ?= dist-newstyle/flamegraph
FLAMEGRAPH_SQL ?= benchmarks/corpus/06_views_and_aggregates.sql

flamegraph:
	cabal build exe:occam --builddir="$(FLAMEGRAPH_DIR)" --enable-profiling --profiling-detail=none --library-profiling-detail=toplevel-functions
	@bin=$$(cabal list-bin exe:occam --builddir="$(FLAMEGRAPH_DIR)" --enable-profiling) && \
		"$$bin" "$(FLAMEGRAPH_SQL)" --dialect sqlite --result-oracle --jobs 1 --intra-statement --quiet \
			--out /dev/null --stats "$(FLAMEGRAPH_DIR)/stats.json" \
			+RTS -N1 -pj "-po$(abspath $(FLAMEGRAPH_DIR))/occam" -RTS
	@printf 'Open %s in https://www.speedscope.app/ (Left Heavy view).\n' "$(FLAMEGRAPH_DIR)/occam.prof"

fmt:
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu -m inplace $$(find src app test bench eval-lib -name "*.hs"); \
	else \
		nix develop --command fourmolu -m inplace $$(find src app test bench eval-lib -name "*.hs"); \
	fi

fmt-check:
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu --mode check $$(find src app test bench eval-lib -name "*.hs"); \
	else \
		nix develop --command fourmolu --mode check $$(find src app test bench eval-lib -name "*.hs"); \
	fi

lint:
	@if command -v hlint >/dev/null 2>&1; then \
		hlint src app test bench eval-lib; \
	else \
		nix develop --command hlint src app test bench eval-lib; \
	fi

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

dhall-gen:
	@gen_cmd=' \
		set -e; \
		mkdir -p config/generated/profiles config/generated/plans; \
		dhall-to-json --file config/dhall/Harness.dhall > config/generated/harness.json; \
		dhall-to-json --file config/dhall/OccamDefaultsJson.dhall > config/generated/occam_defaults.json; \
		for f in config/dhall/profiles/*.dhall; do \
			name=$$(basename "$$f" .dhall); \
			dhall-to-json --file "$$f" > "config/generated/profiles/$${name}.json"; \
		done; \
		for f in config/dhall/plans/*.dhall; do \
			name=$$(basename "$$f" .dhall); \
			dhall-to-json --file "$$f" > "config/generated/plans/$${name}.json"; \
		done; \
	'; \
	if command -v dhall-to-json >/dev/null 2>&1; then \
		bash -c "$$gen_cmd"; \
	else \
		nix develop .#dhall --command bash -c "$$gen_cmd"; \
	fi

dhall-check:
	@chk_cmd=' \
		set -eo pipefail; \
		for f in config/dhall/*.dhall config/dhall/profiles/*.dhall config/dhall/plans/*.dhall; do \
			dhall type --quiet --file "$$f" > /dev/null; \
		done; \
		tmp=$$(mktemp -d); \
		trap "rm -rf \"$$tmp\"" EXIT; \
		mkdir -p "$$tmp/profiles" "$$tmp/plans"; \
		dhall-to-json --file config/dhall/Harness.dhall > "$$tmp/harness.json"; \
		dhall-to-json --file config/dhall/OccamDefaultsJson.dhall > "$$tmp/occam_defaults.json"; \
		for f in config/dhall/profiles/*.dhall; do \
			name=$$(basename "$$f" .dhall); \
			dhall-to-json --file "$$f" > "$$tmp/profiles/$${name}.json"; \
		done; \
		for f in config/dhall/plans/*.dhall; do \
			name=$$(basename "$$f" .dhall); \
			dhall-to-json --file "$$f" > "$$tmp/plans/$${name}.json"; \
		done; \
		diff -ru config/generated "$$tmp"; \
	'; \
	if command -v dhall >/dev/null 2>&1 && command -v dhall-to-json >/dev/null 2>&1; then \
		bash -c "$$chk_cmd"; \
	else \
		nix develop .#dhall --command bash -c "$$chk_cmd"; \
	fi

check: fmt-check lint build-check dhall-check test harness-check proof-check


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

# Occam

> *Entia non sunt multiplicanda praeter necessitatem.* (Entities should not be multiplied beyond necessity.)

**Occam** is a dependency-aware, state-preserving test case minimizer for multi-statement SQL crashes.

## Overview

Fuzzers like SQUIRREL, SQLancer, and Griffin produce lengthy, stateful multi-statement SQL scripts (often 50–200+ statements) when triggering DBMS crashes. Most existing reducers either:
1. Treat SQL as flat text/chunks (ddmin), wasting hundreds of database resets on syntactically/semantically invalid candidates.
2. Rely on language unparsers that mangle precise byte sequences, comments, or float encodings, inadvertently causing **bug slippage**.

Occam combines **Tree-sitter CST byte slicing** with **cross-statement object dependency analysis (Def/Use/Mod)** to prune crashing SQL scripts to 1-minimal reproducing examples without modifying the original byte syntax of preserved statements.

## Architecture

- **Frontend**: Tree-sitter C grammar bindings via FFI (zero unparser, exact byte span slicing).
- **Core (Haskell)**:
  - Multi-statement Def/Use/Mod object dependency graph.
  - Backward dependency slicing.
  - Dependency-constrained hierarchical delta debugging (closure-level ddmin).
- **Oracle (Haskell IO)**:
  - Clean container / DBMS state management.
  - ASan stack trace & signal consistency verification.

## Building

```bash
cabal build
cabal test
```

## License

MIT License.

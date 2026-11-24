# Occam Formal Verification (Lean 4)

Machine-checked Lean 4 proofs for the abstract reduction model (zero `sorry`):

- [`Proofs/DAG.lean`](Proofs/DAG.lean): temporal dependency DAG acyclicity, backward-slice soundness, and closure minimality.
- [`Proofs/Reduction.lean`](Proofs/Reduction.lean): reduction measure monotonicity, well-founded termination, and fixed-point convergence.
- [`Proofs/Minimality.lean`](Proofs/Minimality.lean): single-statement audit 1-minimality and closure 1-minimality.

## Running

```bash
make proof-check
# or directly:
cd proofs && lake build
```

## Scope

These proofs verify the abstract DAG and reduction algorithms. They do not formalize SQL dialect semantics, the Rust parser frontend, or external DBMS execution (see [Architecture](../docs/ARCHITECTURE.md#5-verification-boundaries)).

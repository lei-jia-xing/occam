# Occam Formal Verification (Lean 4)

Machine-checked Lean 4 proofs for the abstract reduction model (zero `sorry`):

- [`Proofs/DAG.lean`](Proofs/DAG.lean): temporal dependency DAG acyclicity, backward-slice soundness, and closure minimality.
- [`Proofs/EditIntervals.lean`](Proofs/EditIntervals.lean) proves maximum-weight edit selection and the endpoint rule for adjacent replacements.
- [`Proofs/OrderedWindow.lean`](Proofs/OrderedWindow.lean) proves ordered retirement equivalence for one invocation with deterministic verdicts and a shared starting cache and budget. It also shows how speculative starts can change the next invocation's remaining budget.
- [`Proofs/Reduction.lean`](Proofs/Reduction.lean): reduction measure monotonicity, well-founded termination, and fixed-point convergence.
- [`Proofs/Minimality.lean`](Proofs/Minimality.lean): single-statement audit 1-minimality and closure 1-minimality.

## Running

```bash
make proof-check
# or directly:
cd proofs && lake build
```

## Scope

These proofs verify the abstract DAG and reduction algorithms. They do not formalize SQL dialect semantics, the pure-Haskell SQL parser frontend, or external DBMS execution (see [Architecture](../docs/ARCHITECTURE.md#5-verification-boundaries)).

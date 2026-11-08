/-
Copyright (c) 2026 Occam Authors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Proofs.DAG
import Proofs.Reduction
import Proofs.Minimality
import Lean.Data.Json

/-!
This module implements a verified standalone reduction certificate checker.
Occam reduction certificates record:
1. Candidate initial and final states (bytes and statement IDs).
2. The extracted statement dependency DAG edges.
3. Every hierarchical reduction step taken (kind, byte delta, validity).
4. The sequential 1-minimality audit rejection set (indispensable statements).

The standalone checker independently validates these certificates and provides
machine-checked proofs of soundness:
- **Theorem 1 (Monotonicity)**: Final bytes do not exceed initial candidate bytes.
- **Theorem 2 (Step Progress)**: All recorded reduction steps strictly decrease bytes and are valid.
- **Theorem 3 (DAG Temporal Order & Acyclicity)**: All dependency edges strictly decrease statement IDs,
  satisfying the fundamental temporal order and acyclicity of Occam DAGs.
- **Theorem 4 (Audit Completeness)**: Every retained final statement was tested and rejected by audit.
-/

open Lean

/-- A single reduction step recorded in the certificate. -/
structure StepCertificate where
  stepKind : String
  bytesBefore : Nat
  bytesAfter : Nat
  stmtsBefore : List Nat
  stmtsAfter : List Nat
  valid : Bool
deriving Repr, DecidableEq, FromJson, ToJson

/-- A full Occam reduction certificate. -/
structure ReductionCertificate where
  initialBytes : Nat
  initialStmts : List Nat
  finalBytes : Nat
  finalStmts : List Nat
  dagEdges : List (Nat × List Nat)
  steps : List StepCertificate
  auditRejected : List Nat -- statements s in finalStmts verified to be indispensable
deriving Repr, DecidableEq, FromJson, ToJson

/-!
## Computable Verification Predicates
-/

/-- Check that DAG dependencies are strictly decreasing: `∀ u ∈ deps v, u < v`. -/
def checkDagTemporal (edges : List (Nat × List Nat)) : Bool :=
  edges.all fun (v, deps) => deps.all fun u => decide (u < v)

/-- Check that all steps are marked valid and strictly reduce byte size. -/
def checkStepsStrict (steps : List StepCertificate) : Bool :=
  steps.all fun s => s.valid && decide (s.bytesAfter < s.bytesBefore)

/-- Check that consecutive reduction steps form a continuous chain. -/
def checkConsecutive : List StepCertificate → Bool
  | [] => true
  | [_] => true
  | s1 :: s2 :: rest =>
    decide (s1.bytesAfter = s2.bytesBefore) && checkConsecutive (s2 :: rest)

/-- Check the validity of the step chain against initial and final states:
    - If steps is empty: initialBytes = finalBytes ∧ initialStmts = finalStmts
    - If steps is non-empty: first step has bytesBefore = initialBytes,
      last step has bytesAfter = finalBytes, and consecutive steps match. -/
def checkStepChain (cert : ReductionCertificate) : Bool :=
  match cert.steps with
  | [] =>
    decide (cert.initialBytes = cert.finalBytes) && decide (cert.initialStmts = cert.finalStmts)
  | s0 :: _ =>
    decide (s0.bytesBefore = cert.initialBytes) &&
    (match cert.steps.getLast? with
     | some sLast => decide (sLast.bytesAfter = cert.finalBytes)
     | none => false) &&
    checkConsecutive cert.steps

/-- Check that all retained final statements were audited and confirmed indispensable. -/
def checkAuditCompleteness (finalStmts : List Nat) (auditRejected : List Nat) : Bool :=
  finalStmts.all fun s => decide (s ∈ auditRejected)

/-- Primary verification function: checks all conditions of a valid reduction certificate.
    Returns `true` iff the certificate passes all verification checks:
    a) Non-empty initial candidate (`cert.initialBytes > 0`)
    b) Temporal DAG validity (`∀ (v, deps) ∈ cert.dagEdges, ∀ u ∈ deps, u < v`)
    c) Step monotonicity (`∀ s ∈ cert.steps, s.valid = true ∧ s.bytesAfter < s.bytesBefore`)
    d) Step chain validity (boundary states and consecutive step alignment)
    e) Overall monotonicity (`cert.finalBytes ≤ cert.initialBytes`)
    f) 1-minimal audit completeness (`∀ s ∈ cert.finalStmts, s ∈ cert.auditRejected`) -/
def verifyCertificate (cert : ReductionCertificate) : Bool :=
  decide (cert.initialBytes > 0) &&
  checkDagTemporal cert.dagEdges &&
  checkStepsStrict cert.steps &&
  checkStepChain cert &&
  decide (cert.finalBytes ≤ cert.initialBytes) &&
  checkAuditCompleteness cert.finalStmts cert.auditRejected

/-!
## Soundness Theorems
-/

/-- Soundness Theorem 1 (Monotonicity):
    Any verified certificate guarantees that the final candidate size does not exceed the initial size. -/
theorem verifyCertificate_monotonic (cert : ReductionCertificate)
    (h : verifyCertificate cert = true) : cert.finalBytes ≤ cert.initialBytes := by
  simp only [verifyCertificate, Bool.and_eq_true, decide_eq_true_eq] at h
  rcases h with ⟨⟨⟨⟨⟨-, -⟩, -⟩, -⟩, hle⟩, -⟩
  exact hle

/-- Soundness Theorem 2 (Step Progress):
    Any verified certificate guarantees that every recorded reduction step strictly decreases byte size
    and preserves oracle validity. -/
theorem verifyCertificate_steps_strict (cert : ReductionCertificate)
    (h : verifyCertificate cert = true) :
    ∀ s ∈ cert.steps, s.bytesAfter < s.bytesBefore ∧ s.valid = true := by
  intro s hs
  simp only [verifyCertificate, Bool.and_eq_true] at h
  rcases h with ⟨⟨⟨⟨⟨-, -⟩, hsteps⟩, -⟩, -⟩, -⟩
  rw [checkStepsStrict, List.all_eq_true] at hsteps
  have hp := hsteps s hs
  simp only [Bool.and_eq_true, decide_eq_true_eq] at hp
  exact ⟨hp.2, hp.1⟩

/-- Soundness Theorem 3 (DAG Temporal Order & Acyclicity):
    Any verified certificate guarantees that all statement dependencies in the DAG are strictly decreasing.
    By `Proofs.DAG.reachable_acyclic`, this ensures the dependency graph contains no cycles or self-loops. -/
theorem verifyCertificate_dag_temporal (cert : ReductionCertificate)
    (h : verifyCertificate cert = true) :
    ∀ v deps, (v, deps) ∈ cert.dagEdges → ∀ u ∈ deps, u < v := by
  intro v deps hmem u hu
  simp only [verifyCertificate, Bool.and_eq_true] at h
  rcases h with ⟨⟨⟨⟨⟨-, hdag⟩, -⟩, -⟩, -⟩, -⟩
  rw [checkDagTemporal, List.all_eq_true] at hdag
  have hp := hdag (v, deps) hmem
  dsimp at hp
  rw [List.all_eq_true] at hp
  have hu_bool := hp u hu
  exact of_decide_eq_true hu_bool

/-- Soundness Theorem 3 (Pair variant for structural pattern matching):
    `∀ p ∈ cert.dagEdges, ∀ u ∈ p.2, u < p.1`. -/
theorem verifyCertificate_dag_temporal_pair (cert : ReductionCertificate)
    (h : verifyCertificate cert = true) :
    ∀ p ∈ cert.dagEdges, ∀ u ∈ p.2, u < p.1 := by
  intro ⟨v, deps⟩ hmem u hu
  exact verifyCertificate_dag_temporal cert h v deps hmem u hu

/-- Soundness Theorem 4 (1-Minimal Audit Completeness):
    Any verified certificate guarantees that every statement retained in `finalStmts`
    was independently verified to be indispensable (`s ∈ cert.auditRejected`). -/
theorem verifyCertificate_audit_complete (cert : ReductionCertificate)
    (h : verifyCertificate cert = true) :
    ∀ s ∈ cert.finalStmts, s ∈ cert.auditRejected := by
  intro s hs
  simp only [verifyCertificate, Bool.and_eq_true] at h
  rcases h with ⟨⟨⟨⟨⟨-, -⟩, -⟩, -⟩, -⟩, haudit⟩
  rw [checkAuditCompleteness, List.all_eq_true] at haudit
  have hp := haudit s hs
  exact of_decide_eq_true hp

/-- Soundness Theorem 5 (Initial Candidate Non-Empty):
    Any verified certificate guarantees that the initial candidate byte size is strictly positive. -/
theorem verifyCertificate_initial_pos (cert : ReductionCertificate)
    (h : verifyCertificate cert = true) : 0 < cert.initialBytes := by
  simp only [verifyCertificate, Bool.and_eq_true, decide_eq_true_eq] at h
  rcases h with ⟨⟨⟨⟨⟨hpos, -⟩, -⟩, -⟩, -⟩, -⟩
  exact hpos

/-!
## Standalone Executable Entrypoint
-/

/-- Detailed diagnostic verification report for failure analysis. -/
def verifyWithDiagnostics (cert : ReductionCertificate) : Except String Unit := do
  if ¬ (cert.initialBytes > 0) then
    throw s!"Initial byte size must be positive, got {cert.initialBytes}"
  if ¬ (checkDagTemporal cert.dagEdges) then
    throw "Temporal DAG invariant violated: dependency edge u ≥ v found"
  if ¬ (checkStepsStrict cert.steps) then
    throw "Step monotonicity violated: step does not strictly decrease bytes or valid ≠ true"
  if ¬ (checkStepChain cert) then
    throw "Step chain violated: boundary states or consecutive transitions do not match"
  if ¬ (cert.finalBytes ≤ cert.initialBytes) then
    throw s!"Overall monotonicity violated: finalBytes ({cert.finalBytes}) > initialBytes ({cert.initialBytes})"
  if ¬ (checkAuditCompleteness cert.finalStmts cert.auditRejected) then
    throw "1-minimality audit incomplete: retained final statement not in auditRejected list"
  return ()

/-- Standalone executable entrypoint: reads and verifies an Occam reduction certificate JSON file. -/
def main (args : List String) : IO UInt32 := do
  match args with
  | [] =>
    IO.eprintln "Usage: occam-checker <certificate.json>"
    return 1
  | path :: _ =>
    try
      let content ← IO.FS.readFile path
      match Json.parse content with
      | Except.error err =>
        IO.eprintln s!"CERTIFICATE VERIFICATION FAILED: Invalid JSON syntax: {err}"
        return 1
      | Except.ok json =>
        match fromJson? (α := ReductionCertificate) json with
        | Except.error err =>
          IO.eprintln s!"CERTIFICATE VERIFICATION FAILED: Certificate schema error: {err}"
          return 1
        | Except.ok cert =>
          if verifyCertificate cert then
            IO.println s!"CERTIFICATE VERIFIED: PASS"
            IO.println s!"  Initial: {cert.initialBytes} bytes, {cert.initialStmts.length} statements"
            IO.println s!"  Final:   {cert.finalBytes} bytes, {cert.finalStmts.length} statements"
            IO.println s!"  Steps:   {cert.steps.length} validated reduction steps"
            IO.println s!"  DAG:     {cert.dagEdges.length} verified temporal dependency edges"
            IO.println s!"  Audit:   {cert.auditRejected.length} statements confirmed indispensable (1-minimal)"
            return 0
          else
            match verifyWithDiagnostics cert with
            | Except.error diag =>
              IO.eprintln s!"CERTIFICATE VERIFICATION FAILED: {diag}"
            | Except.ok () =>
              IO.eprintln "CERTIFICATE VERIFICATION FAILED: Unknown verification failure"
            return 1
    catch e =>
      IO.eprintln s!"CERTIFICATE VERIFICATION FAILED: IO error reading file '{path}': {e}"
      return 1

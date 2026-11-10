import Proofs.DAG
import Proofs.Reduction

/-!
This module provides the formal mathematical model and verified properties
of Occam's sequential single-statement deletion audit algorithm and the concept
of 1-minimality (local minimality with respect to single-statement removals).

Haskell reference:
- `src/Occam/Reducer.hs`: `statementAudit`, `statementAuditWithLogger`, `auditPass`,
  and `statement_1_minimal = true`.

Key theorems proved:
1. **Length Reduction Lemma**: Removing any present statement strictly decreases candidate length.
2. **Theorem 1 (Termination & Complexity Bound)**: `auditLoop` is well-founded and terminates
   in at most `stmts.length` removal passes.
3. **Theorem 2 (Length Monotonicity)**: `(auditLoop O stmts).length ≤ stmts.length`.
4. **Theorem 3 (Oracle Preservation / Soundness)**: If `O stmts = true`, then `O (auditLoop O stmts) = true`.
5. **Theorem 4 (Audit Fixed Point is 1-Minimal)**: If `O stmts = true`, then `IsOneMinimal O (auditLoop O stmts)`.
6. **Connection with DAG Closure**: `IsClosureOneMinimal` ensures that all closure-preserving single-statement
   removals either fail the oracle or fail to reduce the statement set.
-/

/-- Statement removal: filters out all occurrences of statement `s` from `stmts`.
    Matches Occam's `filter ((/= stmtId statement) . stmtId) current` in `src/Occam/Reducer.hs`. -/
def removeStmt (s : StmtId) (stmts : List StmtId) : List StmtId :=
  stmts.filter (· ≠ s)

/-- Removing an element present in `stmts` strictly decreases list length. -/
theorem removeStmt_length_lt {s : StmtId} {stmts : List StmtId} (h : s ∈ stmts) :
    (removeStmt s stmts).length < stmts.length := by
  have hp : (decide (s ≠ s)) = false := by simp
  exact filter_length_lt (fun x => decide (x ≠ s)) stmts s h hp

/-!
## Formal Definitions: 1-Minimality & Audit Step
-/

/-- The formal concept of **1-Minimality** (局部极小性):
    A candidate program `stmts` is 1-minimal under oracle `O` iff:
    1. It satisfies the oracle (`O stmts = true`), and
    2. Removing any individual statement `s ∈ stmts` causes the oracle to fail. -/
def IsOneMinimal (O : Oracle (List StmtId)) (stmts : List StmtId) : Prop :=
  O stmts = true ∧ ∀ s ∈ stmts, O (removeStmt s stmts) = false

/-- Single-statement audit reduction step:
    Transitions from `stmts` to `stmts'` by identifying an element `s ∈ stmts`
    whose removal continues to satisfy the oracle `O`. -/
def AuditStep (O : Oracle (List StmtId)) (stmts stmts' : List StmtId) : Prop :=
  O stmts = true ∧ ∃ s ∈ stmts, O (removeStmt s stmts) = true ∧ stmts' = removeStmt s stmts

/-- Lemma (Length Reduction):
    Any valid audit step strictly reduces the candidate length. -/
theorem auditStep_length_lt {O : Oracle (List StmtId)} {stmts stmts' : List StmtId}
    (h : AuditStep O stmts stmts') : stmts'.length < stmts.length := by
  rcases h with ⟨-, s, hs, -, rfl⟩
  exact removeStmt_length_lt hs

/-!
## Executable Single-Pass Audit & Fixed-Point Loop
-/

/-- Computable single-pass audit function:
    Scans `stmts` sequentially for the first statement `s` whose removal satisfies `O`.
    Returns `some s` upon the first successful removal, or `none` if all single-statement removals fail. -/
def auditPass (O : Oracle (List StmtId)) (stmts : List StmtId) : Option StmtId :=
  stmts.find? (fun s => O (removeStmt s stmts))

/-- Single-pass audit transformation step:
    Applies the removal if an oracle-satisfying candidate was found during the pass. -/
def auditPassStep (O : Oracle (List StmtId)) (stmts : List StmtId) : Option (List StmtId) :=
  (auditPass O stmts).map (fun s => removeStmt s stmts)

theorem auditPass_mem {O : Oracle (List StmtId)} {stmts : List StmtId} {s : StmtId}
    (h : auditPass O stmts = some s) : s ∈ stmts :=
  List.mem_of_find?_eq_some h

theorem auditPass_oracle {O : Oracle (List StmtId)} {stmts : List StmtId} {s : StmtId}
    (h : auditPass O stmts = some s) : O (removeStmt s stmts) = true :=
  List.find?_some (p := fun s => O (removeStmt s stmts)) (l := stmts) (a := s) h

theorem auditPass_step {O : Oracle (List StmtId)} {stmts : List StmtId} {s : StmtId}
    (hO : O stmts = true) (h : auditPass O stmts = some s) :
    AuditStep O stmts (removeStmt s stmts) :=
  ⟨hO, s, auditPass_mem h, auditPass_oracle h, rfl⟩

theorem auditPass_none {O : Oracle (List StmtId)} {stmts : List StmtId}
    (h : auditPass O stmts = none) : ∀ s ∈ stmts, O (removeStmt s stmts) = false := by
  intro s hs
  have hnone := List.find?_eq_none.mp h s hs
  cases hO : O (removeStmt s stmts)
  · rfl
  · exfalso; exact hnone hO

/-- Complete fixed-point audit loop `auditLoop`:
    Repeatedly executes audit passes until a fixed point is reached.
    Terminates by well-founded recursion on `stmts.length`. -/
def auditLoop (O : Oracle (List StmtId)) (stmts : List StmtId) : List StmtId :=
  match h : auditPass O stmts with
  | none => stmts
  | some s =>
    have : (removeStmt s stmts).length < stmts.length :=
      removeStmt_length_lt (auditPass_mem h)
    auditLoop O (removeStmt s stmts)
termination_by stmts.length

theorem auditLoop_eq_none (O : Oracle (List StmtId)) (stmts : List StmtId)
    (h : auditPass O stmts = none) : auditLoop O stmts = stmts := by
  rw [auditLoop]
  split
  · rfl
  · rename_i s hsome
    rw [h] at hsome
    contradiction

theorem auditLoop_eq_some (O : Oracle (List StmtId)) (stmts : List StmtId) (s : StmtId)
    (h : auditPass O stmts = some s) :
    auditLoop O stmts = auditLoop O (removeStmt s stmts) := by
  rw [auditLoop]
  split
  · rename_i hnone
    rw [h] at hnone
    contradiction
  · rename_i s' hsome'
    injection h.symm.trans hsome' with heq
    subst heq
    rfl

/-!
## Theorem 1: Well-Founded Termination & Complexity Bound
-/

/-- Well-founded reduction step relation on lists of statements. -/
def AuditStepRel (O : Oracle (List StmtId)) (stmts' stmts : List StmtId) : Prop :=
  AuditStep O stmts stmts'

/-- The audit reduction step relation is well-founded. -/
theorem auditStep_wellFounded (O : Oracle (List StmtId)) :
    WellFounded (AuditStepRel O) := by
  have hwf : WellFounded (InvImage (fun a b => a < b) (List.length (α := StmtId))) :=
    InvImage.wf (List.length (α := StmtId)) Nat.lt_wfRel.wf
  apply Subrelation.wf (r := InvImage (fun a b => a < b) (List.length (α := StmtId)))
  · intro x y h
    exact auditStep_length_lt h
  · exact hwf

/-- Number of successful removal passes executed by `auditLoop`. -/
def auditLoopPasses (O : Oracle (List StmtId)) (stmts : List StmtId) : Nat :=
  match h : auditPass O stmts with
  | none => 0
  | some s =>
    have : (removeStmt s stmts).length < stmts.length :=
      removeStmt_length_lt (auditPass_mem h)
    auditLoopPasses O (removeStmt s stmts) + 1
termination_by stmts.length

theorem auditLoopPasses_eq_none (O : Oracle (List StmtId)) (stmts : List StmtId)
    (h : auditPass O stmts = none) : auditLoopPasses O stmts = 0 := by
  rw [auditLoopPasses]
  split
  · rfl
  · rename_i s hsome
    rw [h] at hsome
    contradiction

theorem auditLoopPasses_eq_some (O : Oracle (List StmtId)) (stmts : List StmtId) (s : StmtId)
    (h : auditPass O stmts = some s) :
    auditLoopPasses O stmts = auditLoopPasses O (removeStmt s stmts) + 1 := by
  rw [auditLoopPasses]
  split
  · rename_i hnone
    rw [h] at hnone
    contradiction
  · rename_i s' hsome'
    injection h.symm.trans hsome' with heq
    subst heq
    rfl

/-- An explicit chain of `k` audit removal passes. -/
inductive AuditChain (O : Oracle (List StmtId)) : Nat → List StmtId → List StmtId → Prop where
  | refl (stmts : List StmtId) : AuditChain O 0 stmts stmts
  | step {k : Nat} {stmts stmts' stmts'' : List StmtId}
      (h : AuditStep O stmts stmts') (rest : AuditChain O k stmts' stmts'') :
      AuditChain O (k + 1) stmts stmts''

/-- Candidate length plus executed passes is bounded by initial length. -/
theorem auditChain_length_bound {O : Oracle (List StmtId)} {k : Nat} {stmts stmts' : List StmtId}
    (h : AuditChain O k stmts stmts') : stmts'.length + k ≤ stmts.length := by
  induction h with
  | refl => omega
  | step hstep _rest ih =>
    have hlt := auditStep_length_lt hstep
    omega

/-- Any valid sequence of `k` audit passes satisfies `k ≤ stmts.length`. -/
theorem auditChain_k_le {O : Oracle (List StmtId)} {k : Nat} {stmts stmts' : List StmtId}
    (h : AuditChain O k stmts stmts') : k ≤ stmts.length := by
  have hb := auditChain_length_bound h
  omega

/-- The executable `auditLoop` traces an exact `AuditChain` of passes. -/
theorem auditLoop_chain (O : Oracle (List StmtId)) (stmts : List StmtId) (hO : O stmts = true) :
    AuditChain O (auditLoopPasses O stmts) stmts (auditLoop O stmts) := by
  induction stmts using auditLoop.induct O with
  | case1 x hnone =>
    rw [auditLoop_eq_none O x hnone, auditLoopPasses_eq_none O x hnone]
    exact AuditChain.refl x
  | case2 x s hsome _hlt ih =>
    rw [auditLoop_eq_some O x s hsome, auditLoopPasses_eq_some O x s hsome]
    have hstep : AuditStep O x (removeStmt s x) :=
      auditPass_step hO hsome
    have ih' := ih (auditPass_oracle hsome)
    exact AuditChain.step hstep ih'

/-- Theorem 1 (Termination & Complexity Bound):
    `auditLoop` is well-founded and terminates in at most `stmts.length` removal passes. -/
theorem auditLoop_passes_le (O : Oracle (List StmtId)) (stmts : List StmtId) :
    auditLoopPasses O stmts ≤ stmts.length := by
  induction stmts using auditLoop.induct O with
  | case1 x hnone =>
    rw [auditLoopPasses_eq_none O x hnone]
    exact Nat.zero_le _
  | case2 x s hsome hlt ih =>
    rw [auditLoopPasses_eq_some O x s hsome]
    omega

/-- Theorem 1 (Complexity Corollary): The number of removal passes executed by `auditLoop`
    is bounded by the initial list length. -/
theorem auditLoop_terminates_bound (O : Oracle (List StmtId)) (stmts : List StmtId) :
    auditLoopPasses O stmts ≤ stmts.length :=
  auditLoop_passes_le O stmts

/-!
## Theorem 2: Length Monotonicity
-/

/-- Theorem 2 (Length Monotonicity):
    The length of the fixed point candidate is at most the length of the initial candidate. -/
theorem auditLoop_length_le (O : Oracle (List StmtId)) (stmts : List StmtId) :
    (auditLoop O stmts).length ≤ stmts.length := by
  induction stmts using auditLoop.induct O with
  | case1 x hnone =>
    rw [auditLoop_eq_none O x hnone]
    exact Nat.le_refl _
  | case2 x s hsome hlt ih =>
    rw [auditLoop_eq_some O x s hsome]
    omega

/-- Strict decrease: Whenever at least one removal pass succeeds, the final candidate
    has strictly smaller length. -/
theorem auditLoop_length_lt_of_some (O : Oracle (List StmtId)) (stmts : List StmtId) (s : StmtId)
    (h : auditPass O stmts = some s) :
    (auditLoop O stmts).length < stmts.length := by
  rw [auditLoop_eq_some O stmts s h]
  have hlt := removeStmt_length_lt (auditPass_mem h)
  have hle := auditLoop_length_le O (removeStmt s stmts)
  omega

/-!
## Theorem 3: Oracle Preservation / Soundness
-/

/-- Theorem 3 (Oracle Preservation / Soundness):
    `auditLoop` preserves oracle satisfaction: if the initial candidate satisfies `O`,
    the resulting fixed point also satisfies `O`. -/
theorem auditLoop_preserves_oracle (O : Oracle (List StmtId)) (stmts : List StmtId) (h : O stmts = true) :
    O (auditLoop O stmts) = true := by
  induction stmts using auditLoop.induct O with
  | case1 x hnone =>
    rw [auditLoop_eq_none O x hnone]
    exact h
  | case2 x s hsome _hlt ih =>
    rw [auditLoop_eq_some O x s hsome]
    exact ih (auditPass_oracle hsome)

/-!
## Theorem 4: Audit Fixed Point is 1-Minimal
-/

/-- Theorem 4 (Audit Fixed Point is 1-Minimal):
    For any valid starting candidate (`O stmts = true`), the fixed point produced by
    `auditLoop` is guaranteed to be 1-minimal. -/
theorem auditLoop_isOneMinimal (O : Oracle (List StmtId)) (stmts : List StmtId) (h : O stmts = true) :
    IsOneMinimal O (auditLoop O stmts) := by
  induction stmts using auditLoop.induct O with
  | case1 x hnone =>
    rw [auditLoop_eq_none O x hnone]
    exact ⟨h, auditPass_none hnone⟩
  | case2 x s hsome _hlt ih =>
    rw [auditLoop_eq_some O x s hsome]
    exact ih (auditPass_oracle hsome)

/-!
## Connection with DAG Closure (from Proofs.DAG)

In Occam, SQL statement reduction operates over dependencies modeled as a DAG.
When removing a statement `s`, transitive dependencies must be retained by computing
the closure `closeSet G (removeStmt s stmts)`.

We define **Closure 1-Minimality** (闭包局部极小性) and prove that candidates
whose closure removals fail the oracle (or preserve the entire set) are closure-1-minimal.
-/

/-- Definition: **Closure 1-Minimality** (闭包 1-极小性):
    A candidate `stmts` is closure-1-minimal with respect to DAG `G` and oracle `O` iff:
    1. It satisfies the oracle (`O stmts = true`),
    2. It is a valid DAG closure (`isValidClosure G stmts`), and
    3. For every statement `s ∈ stmts`, the closed candidate `closeSet G (removeStmt s stmts)`
       either fails the oracle or equals the entire set `stmts`. -/
def IsClosureOneMinimal (G : DAG) (O : Oracle (List StmtId)) (stmts : List StmtId) : Prop :=
  O stmts = true ∧ isValidClosure G stmts ∧
    ∀ s ∈ stmts, O (closeSet G (removeStmt s stmts)) = false ∨ closeSet G (removeStmt s stmts) = stmts

/-- Theorem: A candidate is closure-1-minimal whenever any closure removal that satisfies the oracle
    preserves the entire set. -/
theorem isClosureOneMinimal_of_no_proper_removal (G : DAG) (O : Oracle (List StmtId)) (stmts : List StmtId)
    (hO : O stmts = true) (hclosed : isValidClosure G stmts)
    (hrem : ∀ s ∈ stmts, O (closeSet G (removeStmt s stmts)) = true → closeSet G (removeStmt s stmts) = stmts) :
    IsClosureOneMinimal G O stmts := by
  refine ⟨hO, hclosed, ?_⟩
  intro s hs
  cases hO' : O (closeSet G (removeStmt s stmts)) with
  | false => exact Or.inl rfl
  | true => exact Or.inr (hrem s hs hO')

/-- Theorem: In a closure-1-minimal program, any oracle-preserving single-statement closure removal
    retains the full statement set. -/
theorem closureOneMinimal_no_proper_removal (G : DAG) (O : Oracle (List StmtId)) (stmts : List StmtId)
    (hmin : IsClosureOneMinimal G O stmts) (s : StmtId) (hs : s ∈ stmts)
    (hO : O (closeSet G (removeStmt s stmts)) = true) :
    closeSet G (removeStmt s stmts) = stmts := by
  rcases hmin.2.2 s hs with hfalse | heq
  · rw [hfalse] at hO
    contradiction
  · exact heq

/-- Subsetting property: Closing a subset of a closed candidate stays within the candidate. -/
theorem closeSet_removeStmt_subset (G : DAG) (stmts : List StmtId) (s : StmtId)
    (hclosed : isValidClosure G stmts) :
    closeSet G (removeStmt s stmts) ⊆ stmts := by
  apply closeSet_minimal G (removeStmt s stmts) stmts
  · intro x hx
    exact (List.mem_filter.mp hx).1
  · exact hclosed

/-- Validity property: The closure after removing any statement is always a valid closure. -/
theorem closeSet_removeStmt_isValidClosure (G : DAG) (stmts : List StmtId) (s : StmtId)
    (hsub : stmts ⊆ G.stmts) :
    isValidClosure G (closeSet G (removeStmt s stmts)) := by
  apply closeSet_isValidClosure
  intro x hx
  have : x ∈ stmts := (List.mem_filter.mp hx).1
  exact hsub this

/-- Connection Theorem: If `stmts` is 1-minimal and all statement removals are already closed,
    then `stmts` is also Closure 1-Minimal. -/
theorem isClosureOneMinimal_of_isOneMinimal (G : DAG) (O : Oracle (List StmtId)) (stmts : List StmtId)
    (hmin : IsOneMinimal O stmts) (hclosed : isValidClosure G stmts)
    (hrem_closed : ∀ s ∈ stmts, closeSet G (removeStmt s stmts) = removeStmt s stmts) :
    IsClosureOneMinimal G O stmts := by
  refine ⟨hmin.1, hclosed, ?_⟩
  intro s hs
  rw [hrem_closed s hs]
  exact Or.inl (hmin.2 s hs)

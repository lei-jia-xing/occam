/-
Copyright (c) 2026 Occam Authors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Proofs.DAG

/-!
This module provides the formal mathematical model and verified properties
of Occam's hierarchical reduction engine, reduction measure, termination,
oracle preservation, fixed-point convergence, and connection to DAG closure validity.

Haskell reference:
- `src/Occam/Reducer.hs`: `reduceSQLWithConfig`, `loop iteration currentSQL`,
  `reduceIterationWithConfig`, measure by statements & bytes (`cutRemovedStatements`, `sliceSQL`).
- `src/Occam/Core.hs`: `StatementId`, `Span`, `Statement`.
-/

/-- A candidate program representation consisting of an ordered list of statement IDs
    and its total byte length (or measure weight). -/
structure Program where
  stmts : List StmtId
  bytes : Nat
deriving Repr, DecidableEq

/-- The primary strict reduction measure of a candidate program.
    In Occam (`Occam.Reducer.reduceSQLWithConfig`), the primary progress measure
    is the byte length of the SQL candidate (`BS.length currentSQL`). -/
def Program.measure (P : Program) : Nat := P.bytes

open Program (measure)

/-- Statement count of a program candidate. -/
def Program.stmtCount (P : Program) : Nat := P.stmts.length

/-- An alternative composite measure combining statement count and byte size. -/
def Program.compositeMeasure (P : Program) : Nat := P.stmts.length + P.bytes

/-- An oracle is a decision procedure verifying whether a candidate program satisfies
    the target failure / property predicate. Matches Haskell `type Oracle = ByteString -> IO Bool`. -/
def Oracle (α : Type) := α → Bool

/-- A single reduction step from program `P` to `P'`.
    A step is valid if both candidates satisfy the oracle and `P'` has strictly smaller measure. -/
def Step (O : Oracle Program) (P P' : Program) : Prop :=
  O P = true ∧ O P' = true ∧ measure P' < measure P

/-- The reflexive transitive reduction relation.
    `Reduces O P P'` indicates that program `P` can be reduced to `P'` through zero or more
    valid reduction steps. -/
inductive Reduces (O : Oracle Program) : Program → Program → Prop where
  | refl (P : Program) : Reduces O P P
  | step {P P' P'' : Program} (h : Step O P P') (rest : Reduces O P' P'') : Reduces O P P''

/-- A single step is a reduction sequence. -/
theorem reduces_of_step {O : Oracle Program} {P P' : Program} (h : Step O P P') :
    Reduces O P P' :=
  Reduces.step h (Reduces.refl P')

/-- Transitivity of reduction sequences. -/
theorem reduces_trans {O : Oracle Program} {P P' P'' : Program}
    (h1 : Reduces O P P') (h2 : Reduces O P' P'') : Reduces O P P'' := by
  induction h1 with
  | refl _ => exact h2
  | step h _rest ih => exact Reduces.step h (ih h2)

/-- Appending a step to the end of a reduction sequence. -/
theorem reduces_tail {O : Oracle Program} {P P' P'' : Program}
    (h1 : Reduces O P P') (h2 : Step O P' P'') : Reduces O P P'' :=
  reduces_trans h1 (reduces_of_step h2)

/-- A program `P` is a reduction fixed point if no further reduction step is possible. -/
def IsFixedPoint (O : Oracle Program) (P : Program) : Prop :=
  ∀ P', ¬ Step O P P'

/-!
## Computable Reduction Execution Loop

Matching Occam's `loop iteration currentSQL` in `src/Occam/Reducer.hs`.
Given a reduction step search function `next`, `reduceLoop` repeatedly applies steps
until no further step is found, guaranteed to terminate by well-founded recursion on `measure P`.
-/

/-- Computable reduction execution loop matching Occam's iterative reducer. -/
def reduceLoop (O : Oracle Program)
    (next : (P : Program) → Option { P' : Program // Step O P P' })
    (P : Program) : Program :=
  match next P with
  | none => P
  | some ⟨P', hstep⟩ =>
    have : measure P' < measure P := hstep.2.2
    reduceLoop O next P'
termination_by measure P

/-- The executable reduction loop always produces a valid reduction sequence. -/
theorem reduceLoop_reduces (O : Oracle Program)
    (next : (P : Program) → Option { P' : Program // Step O P P' })
    (P : Program) : Reduces O P (reduceLoop O next P) := by
  induction h : measure P using Nat.strongRecOn generalizing P with
  | ind n ih =>
    rw [reduceLoop]
    cases hnext : next P with
    | none =>
      exact Reduces.refl P
    | some pair =>
      rcases pair with ⟨P', hstep⟩
      have hlt : measure P' < n := by
        rw [← h]
        exact hstep.2.2
      have ih' := ih (measure P') hlt P' rfl
      exact Reduces.step hstep ih'

/-!
## Theorem 1: Strict Monotonicity

Every reduction step strictly decreases the candidate measure.
Any sequence of reduction steps weakly decreases the candidate measure,
and strictly decreases whenever at least one step is taken.
-/

/-- Theorem 1 (Strict Step Decrease): A single reduction step strictly decreases measure. -/
theorem reduces_strict_lt {O : Oracle Program} {P P' : Program} (h : Step O P P') :
    measure P' < measure P :=
  h.2.2

/-- Theorem 1 (Monotonicity): A reduction sequence weakly decreases candidate measure. -/
theorem reduces_measure_le {O : Oracle Program} {P P' : Program} (h : Reduces O P P') :
    measure P' ≤ measure P := by
  induction h with
  | refl P => exact Nat.le_refl _
  | @step P P_mid P' hstep _rest ih =>
    have hlt : measure P_mid < measure P := hstep.2.2
    exact Nat.le_trans ih (Nat.le_of_lt hlt)

/-- Strict decrease for non-trivial reduction sequences. -/
theorem reduces_strict_of_step {O : Oracle Program} {P P' P'' : Program}
    (h : Step O P P') (rest : Reduces O P' P'') : measure P'' < measure P := by
  have hle := reduces_measure_le rest
  have hlt : measure P' < measure P := h.2.2
  omega

/-- Measure weakly decreases through computable reduction execution. -/
theorem reduceLoop_measure_le (O : Oracle Program)
    (next : (P : Program) → Option { P' : Program // Step O P P' })
    (P : Program) : measure (reduceLoop O next P) ≤ measure P :=
  reduces_measure_le (reduceLoop_reduces O next P)

/-!
## Theorem 2: Oracle Preservation / Soundness

Reduction steps only transition between oracle-satisfying candidates.
Therefore, any reduction sequence originating from a valid candidate preserves the oracle.
-/

/-- Theorem 2 (Oracle Preservation): Reduction sequences preserve oracle satisfaction. -/
theorem reduces_preserves_oracle {O : Oracle Program} {P P' : Program}
    (h : Reduces O P P') (hP : O P = true) : O P' = true := by
  induction h with
  | refl _ => exact hP
  | @step _ P_mid _ hstep _rest ih =>
    exact ih hstep.2.1

/-- Oracle satisfaction is preserved across computable reduction execution. -/
theorem reduceLoop_preserves_oracle (O : Oracle Program)
    (next : (P : Program) → Option { P' : Program // Step O P P' })
    (P : Program) (hP : O P = true) : O (reduceLoop O next P) = true :=
  reduces_preserves_oracle (reduceLoop_reduces O next P) hP

/-!
## Theorem 3: Well-Founded Termination & Sequence Bounds

Because `measure P : Nat` is strictly decreasing, the reduction relation `Step O`
admits no infinite descending paths and is well-founded.
Any reduction chain of length `k` satisfies `k ≤ measure P_init`.
-/

/-- Reduction predecessor relation for well-founded analysis. -/
def StepRel (O : Oracle Program) (P' P : Program) : Prop :=
  Step O P P'

/-- Theorem 3 (Well-Founded Relation): The reduction relation is well-founded. -/
theorem step_wellFounded (O : Oracle Program) :
    WellFounded (fun P' P => Step O P P') := by
  have hwf : WellFounded (InvImage (fun a b => a < b) Program.measure) :=
    InvImage.wf Program.measure Nat.lt_wfRel.wf
  apply Subrelation.wf (r := InvImage (fun a b => a < b) Program.measure)
  · intro x y h
    exact h.2.2
  · exact hwf

theorem stepRel_wellFounded (O : Oracle Program) :
    WellFounded (StepRel O) :=
  step_wellFounded O

/-- An explicit finite reduction chain of `k` steps from `P` to `P'`. -/
inductive StepChain (O : Oracle Program) : Nat → Program → Program → Prop where
  | refl (P : Program) : StepChain O 0 P P
  | step {k : Nat} {P P' P'' : Program} (h : Step O P P') (rest : StepChain O k P' P'') :
      StepChain O (k + 1) P P''

/-- Any step chain is a valid reduction sequence. -/
theorem stepChain_reduces {O : Oracle Program} {k : Nat} {P P' : Program}
    (h : StepChain O k P P') : Reduces O P P' := by
  induction h with
  | refl P => exact Reduces.refl P
  | step h _rest ih => exact Reduces.step h ih

/-- Theorem 3 (Measure Bounded Steps): The measure after `k` steps plus `k` is bounded by the initial measure. -/
theorem stepChain_measure_bound {O : Oracle Program} {k : Nat} {P P' : Program}
    (h : StepChain O k P P') : measure P' + k ≤ measure P := by
  induction h with
  | refl P =>
    dsimp
    omega
  | @step m P P_mid P' hstep _rest ih =>
    have hlt : measure P_mid < measure P := hstep.2.2
    omega

/-- Theorem 3 (Finite Length Bound): Any valid reduction sequence of `k` steps has length bounded by `measure P`. -/
theorem stepChain_length_le {O : Oracle Program} {k : Nat} {P P' : Program}
    (h : StepChain O k P P') : k ≤ measure P := by
  have hb := stepChain_measure_bound h
  omega

/-!
## Theorem 4: Fixed Point Convergence

Every program candidate `P` has an accessible fixed point `P_fix` reachable
via valid reduction steps (`Reduces O P P_fix`) such that `IsFixedPoint O P_fix`.
-/

open Classical

/-- Helper: Classical existential witness from negation of universal. -/
theorem exists_of_not_forall_not {α : Type} {p : α → Prop} (h : ¬ ∀ x, ¬ p x) : ∃ x, p x := by
  apply byContradiction
  intro hn
  apply h
  intro x hpx
  apply hn
  exact ⟨x, hpx⟩

/-- Theorem 4 (Fixed Point Convergence): Every program has an accessible fixed point. -/
theorem fixed_point_exists (O : Oracle Program) (P : Program) :
    ∃ P_fix : Program, Reduces O P P_fix ∧ IsFixedPoint O P_fix := by
  induction h : measure P using Nat.strongRecOn generalizing P with
  | ind n ih =>
    by_cases hfp : IsFixedPoint O P
    · exact ⟨P, Reduces.refl P, hfp⟩
    · rw [IsFixedPoint] at hfp
      have ⟨P', hstep⟩ := exists_of_not_forall_not hfp
      have hlt : measure P' < n := by
        rw [← h]
        exact hstep.2.2
      have ⟨P_fix, hred, hfp'⟩ := ih (measure P') hlt P' rfl
      exact ⟨P_fix, Reduces.step hstep hred, hfp'⟩

/-- Fixed points preserve the oracle predicate when starting from a valid candidate. -/
theorem fixed_point_preserves_oracle {O : Oracle Program} {P P_fix : Program}
    (hred : Reduces O P P_fix) (hP : O P = true) : O P_fix = true :=
  reduces_preserves_oracle hred hP

/-- If the step strategy `next` is complete, the computable `reduceLoop` reaches a true fixed point. -/
theorem reduceLoop_isFixedPoint (O : Oracle Program)
    (next : (P : Program) → Option { P' : Program // Step O P P' })
    (h_sound : ∀ Q, next Q = none → IsFixedPoint O Q)
    (P : Program) : IsFixedPoint O (reduceLoop O next P) := by
  induction h : measure P using Nat.strongRecOn generalizing P with
  | ind n ih =>
    rw [reduceLoop]
    cases hnext : next P with
    | none =>
      exact h_sound P hnext
    | some pair =>
      rcases pair with ⟨P', hstep⟩
      have hlt : measure P' < n := by
        rw [← h]
        exact hstep.2.2
      exact ih (measure P') hlt P' rfl

/-!
## Connection with DAG Closure (from Proofs.DAG)

Connecting reduction candidate statements with the temporal dependency DAG.
When applying a closed subset filter `K ⊆ P.stmts` with `K ≠ P.stmts`:
1. The statement count strictly decreases (`K.length < P.stmts.length`).
2. The retained statement subset preserves closure validity (`isValidClosure G K`).
-/

/-- Strict sublists have strictly smaller length. -/
theorem sublist_length_lt {α : Type} {l1 l2 : List α} (hsub : l1.Sublist l2) (hne : l1 ≠ l2) :
    l1.length < l2.length := by
  induction hsub with
  | slnil => contradiction
  | @cons l₁ l₂ a h _ih =>
    have hle := List.Sublist.length_le h
    dsimp
    omega
  | @cons_cons l₁ l₂ a _ ih =>
    have hne' : l₁ ≠ l₂ := by
      intro heq
      subst heq
      exact hne rfl
    have hlt := ih hne'
    dsimp
    omega

/-- Filtering a list strictly decreases its length whenever at least one element is rejected. -/
theorem filter_length_lt {α : Type} (p : α → Bool) (l : List α) (x : α) (hx : x ∈ l) (hpx : p x = false) :
    (l.filter p).length < l.length := by
  induction l with
  | nil => contradiction
  | cons y ys ih =>
    cases hx with
    | head =>
      rw [List.filter_cons, hpx]
      dsimp
      have := List.length_filter_le p ys
      omega
    | tail _ htail =>
      rw [List.filter_cons]
      cases hp : p y
      · dsimp
        have := List.length_filter_le p ys
        omega
      · dsimp
        have := ih htail
        omega

/-- Filtering a list strictly decreases length whenever the filtered list differs from the original. -/
theorem filter_length_lt_of_ne {α : Type} (p : α → Bool) (l : List α)
    (hne : l.filter p ≠ l) : (l.filter p).length < l.length :=
  sublist_length_lt (List.filter_sublist (p := p) (l := l)) hne

/-- Applying a closed sublist filter strictly decreases statement count while preserving closure validity. -/
theorem closed_sublist_reduction (G : DAG) (P : Program) (K : List StmtId)
    (hsub : K.Sublist P.stmts) (hne : K ≠ P.stmts) (hclosed : isValidClosure G K) :
    K.length < P.stmts.length ∧ isValidClosure G K :=
  ⟨sublist_length_lt hsub hne, hclosed⟩

/-- Applying a closed predicate filter strictly decreases statement count while preserving closure validity. -/
theorem closed_filter_reduction (G : DAG) (P : Program) (p : StmtId → Bool)
    (hne : P.stmts.filter p ≠ P.stmts) (hclosed : isValidClosure G (P.stmts.filter p)) :
    (P.stmts.filter p).length < P.stmts.length ∧ isValidClosure G (P.stmts.filter p) :=
  ⟨filter_length_lt_of_ne p P.stmts hne, hclosed⟩

/-- Closed backward slice filtering (matching Occam's backward slicing reduction):
    Filtering statements to the closure of valid roots strictly decreases statement count
    and preserves closure validity. -/
theorem closeSet_filter_reduction (G : DAG) (P : Program) (roots : List StmtId)
    (hroots : roots ⊆ G.stmts)
    (hsub : (closeSet G roots).Sublist P.stmts)
    (hne : closeSet G roots ≠ P.stmts) :
    (closeSet G roots).length < P.stmts.length ∧ isValidClosure G (closeSet G roots) :=
  ⟨sublist_length_lt hsub hne, closeSet_isValidClosure G roots hroots⟩

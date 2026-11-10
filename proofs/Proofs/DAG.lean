/-!
This module provides the formal mathematical model and verified properties
of Occam's temporal dependency DAG, backward slicing, and set closure algorithms.

Haskell reference:
- `src/Occam/Core.hs`: StatementId (Nat), DependencyGraph
- `src/Occam/Slicer.hs`: `backwardSlice target graph`
- `src/Occam/Reducer.hs`: `isValidClosure graph kept`, `closeSet graph roots`
- `test/Spec.hs`: Prop 1 (Acyclicity), Prop 2 (Backward Slice Soundness), Prop 2b (Closure Reachability)
-/

abbrev StmtId := Nat

/-- A temporal dependency DAG of SQL statements.
    Statements are identified by `StmtId` (natural numbers).
    The critical invariant `deps_temporal` asserts that any statement `v ∈ stmts`
    depends only on strictly preceding statements `u < v` that also belong to `stmts`. -/
structure DAG where
  stmts : List StmtId
  deps : StmtId → List StmtId
  deps_temporal : ∀ v ∈ stmts, ∀ u ∈ deps v, u < v ∧ u ∈ stmts

/-- Inductive backward reachability relation in a dependency DAG.
    - Base: reflexive (`Reachable G u u`)
    - Step: if `w ∈ G.deps v` and `Reachable G u w`, then `Reachable G u v`. -/
inductive Reachable (G : DAG) (u : StmtId) : StmtId → Prop where
  | refl : Reachable G u u
  | step {v w : StmtId} (hw : w ∈ G.deps v) (hr : Reachable G u w) : Reachable G u v

theorem reachable_refl (G : DAG) (v : StmtId) : Reachable G v v :=
  Reachable.refl

theorem reachable_step (G : DAG) {u v w : StmtId} (hw : w ∈ G.deps v) (hr : Reachable G u w) :
    Reachable G u v :=
  Reachable.step hw hr

theorem reachable_trans (G : DAG) {u v w : StmtId} (h1 : Reachable G u v) (h2 : Reachable G v w) :
    Reachable G u w := by
  induction h2 with
  | refl => exact h1
  | step hw _ ih => exact Reachable.step hw ih

theorem reachable_mem_or_lt (G : DAG) {u v : StmtId} (h : Reachable G u v) :
    v ∈ G.stmts → (u = v ∨ (u ∈ G.stmts ∧ u < v)) := by
  induction h with
  | refl =>
    intro _
    exact Or.inl rfl
  | @step v w hw _ ih =>
    intro hv
    have htemp := G.deps_temporal v hv w hw
    have hw_stmts : w ∈ G.stmts := htemp.2
    have hw_lt : w < v := htemp.1
    have ih' := ih hw_stmts
    cases ih' with
    | inl huw =>
      subst huw
      exact Or.inr ⟨hw_stmts, hw_lt⟩
    | inr hu =>
      exact Or.inr ⟨hu.1, Nat.lt_trans hu.2 hw_lt⟩

/-!
## Theorem 1: Acyclicity

Because dependencies are strictly decreasing on statements (`u < v`),
backward reachability between distinct nodes implies strict natural number inequality `u < v`.
Consequently, mutual reachability forces identity `u = v`, and no non-trivial cycles or self-loops exist.
-/

/-- Theorem 1 (Strict Temporal Order): If `u` is reachable from `v ∈ G.stmts` and `u ≠ v`, then `u < v`. -/
theorem reachable_lt (G : DAG) {u v : StmtId} (hv : v ∈ G.stmts) (h : Reachable G u v) (hne : u ≠ v) :
    u < v := by
  have hcases := reachable_mem_or_lt G h hv
  cases hcases with
  | inl heq => contradiction
  | inr hlt => exact hlt.2

/-- Theorem 1 (Acyclicity): Mutual reachability implies identity. -/
theorem reachable_acyclic (G : DAG) {u v : StmtId} (hv : v ∈ G.stmts)
    (h1 : Reachable G u v) (h2 : Reachable G v u) : u = v := by
  by_cases h : u = v
  · exact h
  · have hlt := reachable_lt G hv h1 h
    have hu_stmts : u ∈ G.stmts := (reachable_mem_or_lt G h1 hv).resolve_left h |>.1
    have hgt := reachable_lt G hu_stmts h2 (Ne.symm h)
    exact (Nat.lt_asymm hlt hgt).elim

/-- Corollary: No distinct statement cycle can exist. -/
theorem reachable_no_cycle (G : DAG) {u v : StmtId} (hv : v ∈ G.stmts)
    (h1 : Reachable G u v) (hne : u ≠ v) : ¬ Reachable G v u := by
  intro h2
  have heq := reachable_acyclic G hv h1 h2
  exact hne heq

/-- Corollary: Direct dependencies cannot be reachable back to the parent. -/
theorem reachable_no_self_loop (G : DAG) {v : StmtId} (hv : v ∈ G.stmts) (w : StmtId) (hw : w ∈ G.deps v) :
    ¬ Reachable G v w := by
  intro h
  have htemp := G.deps_temporal v hv w hw
  have hw_stmts : w ∈ G.stmts := htemp.2
  have hw_lt : w < v := htemp.1
  by_cases hvw : v = w
  · subst hvw
    exact Nat.lt_irrefl v hw_lt
  · have hlt := reachable_lt G hw_stmts h hvw
    exact Nat.lt_asymm hw_lt hlt

/-!
## Executable Backward Slice & Closure Algorithms

We define computable, terminating backward slice and closure functions matching
`Occam.Slicer.backwardSlice` and `Occam.Reducer.closeSet`.
-/

/-- Computable backward slice for a single target statement `v`.
    Recursively accumulates predecessors with well-founded termination on `v : Nat`. -/
def backwardSlice (G : DAG) (v : StmtId) : List StmtId :=
  (v :: ((G.deps v).flatMap fun w =>
    if w < v then
      backwardSlice G w
    else
      [w])).eraseDups
termination_by v

/-- Computable transitive backward closure for a set of root statement IDs.
    Folds backward slices across roots and deduplicates. -/
def closeSet (G : DAG) (roots : List StmtId) : List StmtId :=
  (roots.flatMap fun r => backwardSlice G r).eraseDups

/-- A candidate statement set `kept` is a valid closure iff every kept statement's
    predecessors are also retained in `kept`. -/
def isValidClosure (G : DAG) (kept : List StmtId) : Prop :=
  ∀ v ∈ kept, ∀ u ∈ G.deps v, u ∈ kept

theorem mem_backwardSlice_self (G : DAG) (v : StmtId) :
    v ∈ backwardSlice G v := by
  rw [backwardSlice.eq_def, List.mem_eraseDups]
  exact List.Mem.head _

theorem backwardSlice_step (G : DAG) {r w : StmtId} (hw : w ∈ G.deps r) (hlt : w < r) :
    backwardSlice G w ⊆ backwardSlice G r := by
  intro x hx
  rw [backwardSlice.eq_def, List.mem_eraseDups]
  apply List.Mem.tail
  apply List.mem_flatMap.2
  refine ⟨w, hw, ?_⟩
  simp [hlt, hx]

theorem backwardSlice_dep_self (G : DAG) {r w : StmtId} (hw : w ∈ G.deps r) (hlt : w < r) :
    w ∈ backwardSlice G r := by
  have hw_self := mem_backwardSlice_self G w
  exact backwardSlice_step G hw hlt hw_self

theorem backwardSlice_closed (G : DAG) (r : StmtId) (hr : r ∈ G.stmts) :
    ∀ v ∈ backwardSlice G r, ∀ u ∈ G.deps v, u ∈ backwardSlice G r := by
  induction r using Nat.strongRecOn with
  | ind r ih =>
    intro v hv u hu
    rw [backwardSlice.eq_def, List.mem_eraseDups] at hv
    cases hv with
    | head =>
      have htemp := G.deps_temporal r hr u hu
      exact backwardSlice_dep_self G hu htemp.1
    | tail _ htail =>
      have ⟨w, hw_deps, hw_slice⟩ := List.mem_flatMap.1 htail
      have htemp := G.deps_temporal r hr w hw_deps
      have hw_lt : w < r := htemp.1
      have hw_stmts : w ∈ G.stmts := htemp.2
      have hw_slice' : v ∈ backwardSlice G w := by
        revert hw_slice
        simp [hw_lt]
      have hu_in_w := ih w hw_lt hw_stmts v hw_slice' u hu
      exact backwardSlice_step G hw_deps hw_lt hu_in_w

/-!
## Theorem 2: Closure Soundness

The computed closure `closeSet G roots` is always a valid closure in the sense of `isValidClosure`.
-/

/-- Theorem 2 (Closure Soundness): `closeSet G roots` satisfies `isValidClosure`. -/
theorem closeSet_isValidClosure (G : DAG) (roots : List StmtId) (hroots : roots ⊆ G.stmts) :
    isValidClosure G (closeSet G roots) := by
  intro v hv u hu
  have ⟨r, hr_roots, hr_slice⟩ := List.mem_flatMap.1 (List.mem_eraseDups.1 hv)
  have hr_stmts : r ∈ G.stmts := hroots hr_roots
  have hu_slice := backwardSlice_closed G r hr_stmts v hr_slice u hu
  rw [closeSet, List.mem_eraseDups]
  apply List.mem_flatMap.2
  exact ⟨r, hr_roots, hu_slice⟩

/-!
## Theorem 3: Closure Extensivity & Inclusion

Roots are always included in their closure, and the closure remains within the graph statements.
-/

/-- Theorem 3 (Extensivity): Roots are contained in their transitive closure. -/
theorem roots_subset_closeSet (G : DAG) (roots : List StmtId) :
    roots ⊆ closeSet G roots := by
  intro r hr
  rw [closeSet, List.mem_eraseDups]
  apply List.mem_flatMap.2
  exact ⟨r, hr, mem_backwardSlice_self G r⟩

theorem backwardSlice_subset_of_closed (G : DAG) (C : List StmtId) (hclosed : isValidClosure G C)
    (v : StmtId) (hv : v ∈ C) : backwardSlice G v ⊆ C := by
  induction v using Nat.strongRecOn with
  | ind v ih =>
    intro x hx
    rw [backwardSlice.eq_def, List.mem_eraseDups] at hx
    cases hx with
    | head =>
      exact hv
    | tail _ htail =>
      have ⟨w, hw_deps, hw_slice⟩ := List.mem_flatMap.1 htail
      have hw_in_C : w ∈ C := hclosed v hv w hw_deps
      split at hw_slice
      · rename_i hlt
        exact ih w hlt hw_in_C hw_slice
      · cases hw_slice with
        | head => exact hw_in_C
        | tail _ hnil => contradiction

/-!
## Theorem 4: Closure Minimality

Any valid closure `C` that contains `roots` must also contain `closeSet G roots`.
Thus `closeSet G roots` is the least valid closure containing `roots`.
-/

/-- Theorem 4 (Closure Minimality): `closeSet G roots` is minimal among valid closures containing `roots`. -/
theorem closeSet_minimal (G : DAG) (roots : List StmtId) (C : List StmtId)
    (hroots : roots ⊆ C) (hclosed : isValidClosure G C) : closeSet G roots ⊆ C := by
  intro x hx
  have ⟨r, hr_roots, hr_slice⟩ := List.mem_flatMap.1 (List.mem_eraseDups.1 hx)
  have hr_C : r ∈ C := hroots hr_roots
  exact backwardSlice_subset_of_closed G C hclosed r hr_C hr_slice

/-- The entire statement set `G.stmts` is always a valid closure. -/
theorem stmts_isValidClosure (G : DAG) : isValidClosure G G.stmts := by
  intro v hv u hu
  exact (G.deps_temporal v hv u hu).2

/-- The closure of valid roots is entirely contained within `G.stmts`. -/
theorem closeSet_subset_stmts (G : DAG) (roots : List StmtId) (hroots : roots ⊆ G.stmts) :
    closeSet G roots ⊆ G.stmts :=
  closeSet_minimal G roots G.stmts hroots (stmts_isValidClosure G)

/-!
## Theorem 5: Closure Idempotence

Closing an already-closed set yields the same set.
Both inclusion directions hold: `closeSet G (closeSet G roots) ⊆ closeSet G roots`
and `closeSet G roots ⊆ closeSet G (closeSet G roots)`.
-/

/-- Theorem 5 (Idempotence - Upper Bound): Closing twice adds no new statements. -/
theorem closeSet_idempotent_subset (G : DAG) (roots : List StmtId) (hroots : roots ⊆ G.stmts) :
    closeSet G (closeSet G roots) ⊆ closeSet G roots :=
  closeSet_minimal G (closeSet G roots) (closeSet G roots)
    (fun _ hx => hx)
    (closeSet_isValidClosure G roots hroots)

/-- Theorem 5 (Idempotence - Lower Bound): Extensivity preserves all statements. -/
theorem closeSet_idempotent_superset (G : DAG) (roots : List StmtId) :
    closeSet G roots ⊆ closeSet G (closeSet G roots) :=
  roots_subset_closeSet G (closeSet G roots)

/-- Theorem 5 (Closure Idempotence): Element-wise equivalence of closing twice. -/
theorem closeSet_idempotent (G : DAG) (roots : List StmtId) (hroots : roots ⊆ G.stmts) :
    ∀ x, x ∈ closeSet G (closeSet G roots) ↔ x ∈ closeSet G roots := by
  intro x
  constructor
  · exact fun hx => closeSet_idempotent_subset G roots hroots hx
  · exact fun hx => closeSet_idempotent_superset G roots hx

/-!
## Reachability Equivalence (Occam Spec Props 2 & 2b)

We prove that `backwardSlice` and `closeSet` exactly match graph reachability.
-/

/-- Backward slice soundness: every node in `backwardSlice G v` is reachable from `v`. -/
theorem backwardSlice_sound (G : DAG) (v : StmtId) :
    ∀ u ∈ backwardSlice G v, Reachable G u v := by
  induction v using Nat.strongRecOn with
  | ind v ih =>
    intro u hu
    rw [backwardSlice.eq_def, List.mem_eraseDups] at hu
    cases hu with
    | head =>
      exact Reachable.refl
    | tail _ htail =>
      have ⟨w, hw_deps, hw_slice⟩ := List.mem_flatMap.1 htail
      split at hw_slice
      · rename_i hlt
        have hr_w := ih w hlt u hw_slice
        exact Reachable.step hw_deps hr_w
      · cases hw_slice with
        | head =>
          exact Reachable.step hw_deps Reachable.refl
        | tail _ hnil => contradiction

/-- Backward slice completeness: every reachable node is retained in `backwardSlice G v`. -/
theorem backwardSlice_complete (G : DAG) {u v : StmtId} (hv : v ∈ G.stmts) (h : Reachable G u v) :
    u ∈ backwardSlice G v := by
  induction h with
  | refl =>
    exact mem_backwardSlice_self G u
  | @step v' w' hw _ ih =>
    have htemp := G.deps_temporal v' hv w' hw
    have hw_lt : w' < v' := htemp.1
    have hw_stmts : w' ∈ G.stmts := htemp.2
    have hu_w := ih hw_stmts
    exact backwardSlice_step G hw hw_lt hu_w

/-- Occam Prop 2 & Prop 2b: A node `u` belongs to `closeSet G roots` if and only if
    it is backward-reachable from some root in `roots`. -/
theorem closeSet_reachability (G : DAG) {u : StmtId} {roots : List StmtId} (hroots : roots ⊆ G.stmts) :
    u ∈ closeSet G roots ↔ ∃ r ∈ roots, Reachable G u r := by
  constructor
  · intro hu
    have ⟨r, hr_roots, hr_slice⟩ := List.mem_flatMap.1 (List.mem_eraseDups.1 hu)
    exact ⟨r, hr_roots, backwardSlice_sound G r u hr_slice⟩
  · rintro ⟨r, hr_roots, hr_reach⟩
    have hr_stmts : r ∈ G.stmts := hroots hr_roots
    have hu_slice := backwardSlice_complete G hr_stmts hr_reach
    rw [closeSet, List.mem_eraseDups]
    apply List.mem_flatMap.2
    exact ⟨r, hr_roots, hu_slice⟩

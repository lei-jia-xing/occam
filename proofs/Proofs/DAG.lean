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

/-- To close a retained set, it suffices to close its missing direct predecessors.
    A reverse-edge index can find this boundary by scanning the deleted vertices
    instead of visiting every retained vertex. -/
theorem closeSet_via_boundary (G : DAG) (kept boundary : List StmtId)
    (hkept : kept ⊆ G.stmts)
    (hboundary : ∀ u, u ∈ boundary ↔ u ∉ kept ∧ ∃ v ∈ kept, u ∈ G.deps v)
    (x : StmtId) :
    x ∈ closeSet G kept ↔ x ∈ kept ∨ x ∈ closeSet G boundary := by
  have hboundaryStmts : boundary ⊆ G.stmts := by
    intro u hu
    obtain ⟨_, v, hv, hdep⟩ := (hboundary u).1 hu
    exact (G.deps_temporal v (hkept hv) u hdep).2
  have hclosed := closeSet_isValidClosure G kept hkept
  have hboundaryInClosure : boundary ⊆ closeSet G kept := by
    intro u hu
    obtain ⟨_, v, hv, hdep⟩ := (hboundary u).1 hu
    exact hclosed v (roots_subset_closeSet G kept hv) u hdep
  constructor
  · intro hx
    have hsubset : kept ⊆ kept ++ closeSet G boundary := by
      intro v hv
      exact List.mem_append.2 (Or.inl hv)
    have hclosedUnion : isValidClosure G (kept ++ closeSet G boundary) := by
      intro v hv u hdep
      rcases List.mem_append.1 hv with hv | hv
      · by_cases hu : u ∈ kept
        · exact hsubset hu
        · exact List.mem_append.2 (Or.inr
            (roots_subset_closeSet G boundary ((hboundary u).2 ⟨hu, v, hv, hdep⟩)))
      · exact List.mem_append.2 (Or.inr
          (closeSet_isValidClosure G boundary hboundaryStmts v hv u hdep))
    exact List.mem_append.1 (closeSet_minimal G kept _ hsubset hclosedUnion hx)
  · rintro (hx | hx)
    · exact roots_subset_closeSet G kept hx
    · exact closeSet_minimal G boundary _ hboundaryInClosure hclosed hx

/-- Suppress predecessor edges whose destination is already kept. -/
def missingPredecessorDAG (G : DAG) (kept : List StmtId) : DAG where
  stmts := G.stmts
  deps v := (G.deps v).filter (fun u => u ∉ kept)
  deps_temporal v hv u hu := G.deps_temporal v hv u (List.mem_filter.1 hu).1

/-- The boundary walk may stop at retained vertices. Any missing predecessors
    behind those vertices are already included among the boundary roots. -/
theorem closeSet_via_missing_predecessors (G : DAG) (kept boundary : List StmtId)
    (hkept : kept ⊆ G.stmts)
    (hboundary : ∀ u, u ∈ boundary ↔ u ∉ kept ∧ ∃ v ∈ kept, u ∈ G.deps v)
    (x : StmtId) :
    x ∈ closeSet G kept ↔
      x ∈ kept ∨ x ∈ closeSet (missingPredecessorDAG G kept) boundary := by
  let missing := missingPredecessorDAG G kept
  have hboundaryStmts : boundary ⊆ missing.stmts := by
    intro u hu
    obtain ⟨_, v, hv, hdep⟩ := (hboundary u).1 hu
    exact (G.deps_temporal v (hkept hv) u hdep).2
  have hboundaryInClosure : boundary ⊆ closeSet G kept := by
    intro u hu
    obtain ⟨_, v, hv, hdep⟩ := (hboundary u).1 hu
    exact closeSet_isValidClosure G kept hkept v (roots_subset_closeSet G kept hv) u hdep
  have hkeepUnion : kept ⊆ kept ++ closeSet missing boundary :=
    fun _ hv => List.mem_append.2 (Or.inl hv)
  constructor
  · intro hx
    have hclosedUnion : isValidClosure G (kept ++ closeSet missing boundary) := by
      intro v hv u hdep
      by_cases hu : u ∈ kept
      · exact hkeepUnion hu
      · apply List.mem_append.2 ∘ Or.inr
        rcases List.mem_append.1 hv with hv | hv
        · exact roots_subset_closeSet missing boundary ((hboundary u).2 ⟨hu, v, hv, hdep⟩)
        · apply closeSet_isValidClosure missing boundary hboundaryStmts v hv u
          exact List.mem_filter.2 ⟨hdep, by simpa using hu⟩
    exact List.mem_append.1 (closeSet_minimal G kept _ hkeepUnion hclosedUnion hx)
  · rintro (hx | hx)
    · exact roots_subset_closeSet G kept hx
    · apply closeSet_minimal missing boundary _ hboundaryInClosure _ hx
      intro v hv u hu
      exact closeSet_isValidClosure G kept hkept v hv u (List.mem_filter.1 hu).1

/-- A graph can replace a direct edge by a path without losing reachability. -/
theorem reachable_of_edge_paths (G H : DAG)
    (hpaths : ∀ v u, u ∈ G.deps v → Reachable H u v)
    {u v : StmtId} (h : Reachable G u v) : Reachable H u v := by
  induction h with
  | refl => exact Reachable.refl
  | @step v w hw _ ih => exact reachable_trans H ih (hpaths v w hw)

/-- An edge-pruned graph with a replacement path for every original edge
    yields the same candidate closures for every valid root set. -/
theorem closeSet_eq_of_edge_pruning (G H : DAG) (roots : List StmtId)
    (hG : roots ⊆ G.stmts) (hH : roots ⊆ H.stmts)
    (hsub : ∀ v u, u ∈ H.deps v → u ∈ G.deps v)
    (hpaths : ∀ v u, u ∈ G.deps v → Reachable H u v)
    (x : StmtId) : x ∈ closeSet G roots ↔ x ∈ closeSet H roots := by
  rw [closeSet_reachability G hG, closeSet_reachability H hH]
  constructor
  · rintro ⟨r, hr, hx⟩
    exact ⟨r, hr, reachable_of_edge_paths G H hpaths hx⟩
  · rintro ⟨r, hr, hx⟩
    exact ⟨r, hr, reachable_of_edge_paths H G
      (fun v u h => Reachable.step (hsub v u h) Reachable.refl) hx⟩

/-- All two-hop shortcuts may be removed at once. Even if a witness edge
    is also removed, it spans fewer statement IDs than the shortcut. -/
theorem closeSet_eq_of_two_hop_pruning (G H : DAG) (roots : List StmtId)
    (hG : roots ⊆ G.stmts) (hH : roots ⊆ H.stmts)
    (hsub : ∀ v u, u ∈ H.deps v → u ∈ G.deps v)
    (hpruned : ∀ v ∈ G.stmts, ∀ u ∈ G.deps v,
      u ∈ H.deps v ∨ ∃ w ∈ G.deps v, u ∈ G.deps w)
    (x : StmtId) : x ∈ closeSet G roots ↔ x ∈ closeSet H roots := by
  have hpaths : ∀ n v u, v - u = n → v ∈ G.stmts → u ∈ G.deps v → Reachable H u v := by
    intro n
    induction n using Nat.strongRecOn with
    | ind n ih =>
      intro v u hspan hv hu
      rcases hpruned v hv u hu with hkept | ⟨w, hw, huw⟩
      · exact Reachable.step hkept Reachable.refl
      · obtain ⟨hwv, hwm⟩ := G.deps_temporal v hv w hw
        have huwlt : u < w := (G.deps_temporal w hwm u huw).1
        subst n
        have leftSpan : w - u < v - u := by
          simp only [StmtId] at *
          omega
        have rightSpan : v - w < v - u := by
          simp only [StmtId] at *
          omega
        exact reachable_trans H
          (ih (w - u) leftSpan w u rfl hwm huw)
          (ih (v - w) rightSpan v w rfl hv hw)
  constructor
  · intro hx
    rw [closeSet_reachability G hG] at hx
    obtain ⟨r, hr, hreach⟩ := hx
    rw [closeSet_reachability H hH]
    refine ⟨r, hr, ?_⟩
    have hlocal : ∀ v ∈ G.stmts, Reachable G x v → Reachable H x v := by
      intro v hv h
      induction h with
      | refl => exact Reachable.refl
      | @step v w hw _ ih =>
        have hwm := (G.deps_temporal v hv w hw).2
        exact reachable_trans H (ih hwm) (hpaths (v - w) v w rfl hv hw)
    exact hlocal r (hG hr) hreach
  · intro hx
    rw [closeSet_reachability H hH] at hx
    obtain ⟨r, hr, hreach⟩ := hx
    rw [closeSet_reachability G hG]
    exact ⟨r, hr, reachable_of_edge_paths H G
      (fun v u h => Reachable.step (hsub v u h) Reachable.refl) hreach⟩

/-- Remove a direct edge if the original graph has a two-edge witness. -/
def pruneTwoHopEdges (G : DAG) : DAG where
  stmts := G.stmts
  deps v := (G.deps v).filter fun u => !(G.deps v).any fun w => (G.deps w).contains u
  deps_temporal v hv u hu := G.deps_temporal v hv u (List.mem_filter.1 hu).1

theorem closeSet_pruneTwoHopEdges (G : DAG) (roots : List StmtId)
    (hroots : roots ⊆ G.stmts) (x : StmtId) :
    x ∈ closeSet G roots ↔ x ∈ closeSet (pruneTwoHopEdges G) roots := by
  apply closeSet_eq_of_two_hop_pruning G (pruneTwoHopEdges G) roots hroots hroots
  · intro v u hu
    exact (List.mem_filter.1 hu).1
  · intro v _ u hu
    change (u ∈ (G.deps v).filter
      (fun u => !(G.deps v).any (fun w => (G.deps w).contains u))) ∨ _
    by_cases witness : (G.deps v).any (fun w => (G.deps w).contains u) = true
    · exact Or.inr (by simpa using witness)
    · exact Or.inl (List.mem_filter.2 ⟨hu, by simpa using witness⟩)

/-- Use at most one witness per vertex. The chosen witness is retained by
    temporal ordering, and only predecessors shared with it are removed. -/
def pruneViaWitness (G : DAG) (witness : StmtId → StmtId) : DAG where
  stmts := G.stmts
  deps v :=
    if witness v ∈ G.deps v then
      (G.deps v).filter (fun u => u ∉ G.deps (witness v))
    else
      G.deps v
  deps_temporal v hv u hu := by
    apply G.deps_temporal v hv u
    split at hu
    · exact (List.mem_filter.1 hu).1
    · exact hu

theorem closeSet_pruneViaWitness (G : DAG) (witness : StmtId → StmtId)
    (roots : List StmtId) (hroots : roots ⊆ G.stmts) (x : StmtId) :
    x ∈ closeSet G roots ↔ x ∈ closeSet (pruneViaWitness G witness) roots := by
  apply closeSet_eq_of_two_hop_pruning G (pruneViaWitness G witness) roots hroots hroots
  · intro v u hu
    change u ∈ (if witness v ∈ G.deps v then
      (G.deps v).filter (fun u => u ∉ G.deps (witness v)) else G.deps v) at hu
    split at hu
    · exact (List.mem_filter.1 hu).1
    · exact hu
  · intro v _ u hu
    change u ∈ (if witness v ∈ G.deps v then
      (G.deps v).filter (fun u => u ∉ G.deps (witness v)) else G.deps v) ∨ _
    split
    · rename_i hw
      by_cases hdep : u ∈ G.deps (witness v)
      · exact Or.inr ⟨witness v, hw, hdep⟩
      · exact Or.inl (List.mem_filter.2 ⟨hu, by simpa using hdep⟩)
    · exact Or.inl hu

/-- Validating a projected closure needs only a containment check. If the
    projection is closed and still contains the roots, minimality forces it
    to contain the entire original closure. -/
theorem projected_closeSet_closed_iff_subset (G : DAG) (roots current : List StmtId)
    (hroots : roots ⊆ G.stmts) (hcurrent : roots ⊆ current) :
    isValidClosure G ((closeSet G roots).filter (fun u => u ∈ current)) ↔
      closeSet G roots ⊆ current := by
  constructor
  · intro hclosed u hu
    have hprojected : roots ⊆ (closeSet G roots).filter (fun v => v ∈ current) := by
      intro v hv
      exact List.mem_filter.2 ⟨roots_subset_closeSet G roots hv, by simpa using hcurrent hv⟩
    have hu' := closeSet_minimal G roots _ hprojected hclosed hu
    simpa using (List.mem_filter.1 hu').2
  · intro hsubset v hv u hdep
    have hu := closeSet_isValidClosure G roots hroots v (List.mem_filter.1 hv).1 u hdep
    exact List.mem_filter.2 ⟨hu, by simpa using hsubset hu⟩

/-- Adding opaque statements to the proposal roots can only add statements
    to the proposal closure. -/
theorem closeSet_mono_roots (G : DAG) (roots larger : List StmtId)
    (hlarger : larger ⊆ G.stmts) (hsubset : roots ⊆ larger) :
    closeSet G roots ⊆ closeSet G larger :=
  closeSet_minimal G roots (closeSet G larger)
    (fun _ hr => roots_subset_closeSet G larger (hsubset hr))
    (closeSet_isValidClosure G larger hlarger)

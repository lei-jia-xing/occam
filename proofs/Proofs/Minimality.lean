import Proofs.DAG
import Proofs.Reduction

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

We define **Closure 1-Minimality** and prove that candidates
whose closure removals fail the oracle (or preserve the entire set) are closure-1-minimal.
-/

/-- Definition: **Closure 1-Minimality**:
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

/-- Filtering conclusive rejections by candidate bytes preserves the first
    accepted candidate, including its position relative to other accepts. -/
theorem find?_skip_known_rejections {Candidate Key : Type}
    (candidates : List Candidate) (key : Candidate → Key)
    (oracle rejected : Key → Bool)
    (sound : ∀ k, rejected k = true → oracle k = false) :
    (candidates.filter (fun c => !rejected (key c))).find? (oracle ∘ key) =
      candidates.find? (oracle ∘ key) := by
  rw [List.find?_filter]
  congr 1
  funext c
  cases h : rejected (key c)
  · rfl
  · simpa using (sound _ h).symm

/-- A deletion rejected for the old program can succeed after another
    deletion. This predicate accepts only the original, the intermediate
    candidate, and the candidate following both deletions. -/
theorem rejected_deletion_can_succeed_after_shrink :
    ∃ (oracle : List StmtId → Bool) (before after : List StmtId) (first next : StmtId),
      oracle before = true ∧ first ∈ before ∧ next ∈ before ∧
      after = removeStmt next before ∧ oracle after = true ∧ first ∈ after ∧
      oracle (removeStmt first before) = false ∧ oracle (removeStmt first after) = true := by
  refine ⟨(fun ids => ids == [1, 2, 3] || ids == [1, 3] || ids == [3]),
    [1, 2, 3], [1, 3], 1, 2, ?_⟩
  decide

/-- An all-rejected candidate prefix returns the newly accepted key when
    exactly one of its keys changes verdict. -/
theorem find?_flip_rejected {Key : Type} [DecidableEq Key]
    (oracle : Key → Bool) (rejected : List Key) (missing : Key)
    (hall : ∀ k ∈ rejected, oracle k = false) (hmem : missing ∈ rejected) :
    rejected.find? (fun k => if k = missing then true else oracle k) = some missing := by
  induction rejected with
  | nil => simp at hmem
  | cons k rest ih =>
    have hk := hall k (by simp)
    by_cases heq : k = missing
    · simp [heq]
    · have hrest : missing ∈ rest := (List.mem_cons.1 hmem).resolve_left (Ne.symm heq)
      rw [List.find?_cons, ite_eq_right heq, hk]
      exact ih (fun j hj => hall j (by simp [hj])) hrest

/-- An unobserved key earlier than the first acceptance cannot be skipped by
    an equivalent black-box search. Two oracles agree on every observed key
    but choose different candidates. -/
theorem find?_unobserved_rejection_is_necessary {Key : Type} [DecidableEq Key]
    (oracle : Key → Bool) (earlier later : List Key) (observed : List Key)
    (chosen missing : Key)
    (hreject : ∀ k ∈ earlier, oracle k = false) (haccept : oracle chosen = true)
    (hmem : missing ∈ earlier) (hunobserved : missing ∉ observed) :
    ∃ other : Key → Bool,
      (∀ k ∈ observed, other k = oracle k) ∧
      (earlier ++ chosen :: later).find? oracle = some chosen ∧
      (earlier ++ chosen :: later).find? other = some missing ∧ missing ≠ chosen := by
  let other := fun k => if k = missing then true else oracle k
  have hnone : earlier.find? oracle = none :=
    List.find?_eq_none.2 (fun k hk => by simp [hreject k hk])
  refine ⟨other, ?_, ?_, ?_, ?_⟩
  · intro k hk
    have hne : k ≠ missing := fun heq => hunobserved (heq ▸ hk)
    simp [other, hne]
  · simp [List.find?_append, hnone, haccept]
  · change (earlier ++ chosen :: later).find? (fun k => if k = missing then true else oracle k) = _
    rw [List.find?_append, find?_flip_rejected oracle earlier missing hreject hmem]
    rfl
  · intro heq
    have hfalse := hreject missing hmem
    rw [heq, haccept] at hfalse
    exact Bool.noConfusion hfalse

/-- The same indistinguishability remains when an entire search pass rejects.
    A missing observation can hide an accepted candidate. -/
theorem find?_unobserved_failure_is_necessary {Key : Type} [DecidableEq Key]
    (candidates observed : List Key) (missing : Key)
    (hmem : missing ∈ candidates) (hunobserved : missing ∉ observed) :
    ∃ oracle other : Key → Bool,
      (∀ k ∈ observed, other k = oracle k) ∧
      candidates.find? oracle = none ∧ candidates.find? other = some missing := by
  let oracle : Key → Bool := fun _ => false
  let other := fun k => if k = missing then true else oracle k
  refine ⟨oracle, other, ?_, ?_, find?_flip_rejected oracle candidates missing (by simp [oracle]) hmem⟩
  · intro k hk
    have hne : k ≠ missing := fun heq => hunobserved (heq ▸ hk)
    simp [other, hne]
  · exact List.find?_eq_none.2 (by simp [oracle])

/-- At partition sizes 4, 2, and 1, all contiguous chunk deletions can
    reject even when retaining the last size-2 chunk succeeds. A final
    single-statement audit cannot recover this missed candidate. -/
theorem complements_can_miss_accepted_suffix :
    ∃ oracle : List StmtId → Bool,
      let input : List StmtId := [1, 2, 3, 4, 5, 6, 7, 8]
      let suffix : List StmtId := [7, 8]
      oracle input = true ∧ oracle suffix = true ∧
      ([4, 2, 1] : List Nat).all (fun width =>
        (List.range (8 / width)).all (fun i =>
          !oracle ((input.take (i * width)) ++ input.drop ((i + 1) * width)))) = true ∧
      auditPass oracle input = none := by
  refine ⟨(fun ids => ids == [1, 2, 3, 4, 5, 6, 7, 8] || ids == [7, 8]), ?_⟩
  decide

/-- Keep the last result set from a list of statement observations. -/
def lastStatementResult {Result : Type} (observations : List (Option Result)) : Option Result :=
  observations.foldl (fun last next => next.or last) none

/-- A native certificate that every statement in an exact prefix produced
    no result is sufficient to reject that prefix for a result contract. -/
theorem result_prefix_rejects_of_no_observations {Result : Type}
    (observations : List (Option Result)) (expected : Result)
    (noResults : ∀ observation ∈ observations, observation = none) :
    lastStatementResult observations ≠ some expected := by
  have allNone : lastStatementResult observations = none := by
    induction observations with
    | nil => rfl
    | cons first rest ih =>
      have hfirst := noResults first (by simp)
      have hrest := ih (fun observation hmem => noResults observation (by simp [hmem]))
      simpa [lastStatementResult, hfirst] using hrest
  simp [allNone]

/-- Statement count and last-result position do not identify the first
    result. Two traces can agree on both yet differ on a prefix verdict. -/
theorem last_result_location_does_not_certify_no_result_prefix :
    ∃ a b : List (Option Nat),
      a.length = b.length ∧ a.getLast? = some (some 7) ∧ b.getLast? = some (some 7) ∧
      lastStatementResult a = lastStatementResult b ∧
      lastStatementResult (a.take 1) ≠ lastStatementResult (b.take 1) := by
  refine ⟨[some 7, some 7], [none, some 7], ?_⟩
  decide

/-- Deleting a preceding statement can change the observation of a
    retained statement. A no-result flag from the full run needs a
    context-invariance proof before it can reject arbitrary subsequences. -/
theorem observed_no_result_need_not_survive_deletion :
    ∃ step : Bool → Bool → Bool × Option Nat,
      let first := step false false
      let second := step first.1 true
      let originalResult := step second.1 false
      second.2 = none ∧ originalResult.2 = some 7 ∧ (step false true).2 = some 7 := by
  refine ⟨(fun state stmt =>
    if stmt then (state, if state then none else some 7)
    else (true, if state then some 7 else none)), ?_⟩
  decide

/-!
## Soundness of Additional Native Early-Rejection Certificates

We formalize the abstract soundness properties of:
1. `EarlyRejectFailingPrefix`: deterministic fail-stop prefix errors propagate
   to every extension `prefix ++ suffix`.
2. `EarlyRejectColumnArityMismatch`: reducing the column arity of the final
   result-producing statement (when all trailing statements produce `none`)
   strictly violates the contract result equality.
3. `find?_skip_known_rejections` composes any sound certificate with ordered
   search and 1-minimality audit without altering the first accepted candidate.
-/

/-- Outcome of executing a SQL script against a fresh native result oracle:
    either a fail-stop SQL error, or clean completion with per-statement
    optional result sets. -/
inductive NativeExecOutcome (Result : Type) where
  | error
  | ok (observations : List (Option Result))
  deriving DecidableEq

/-- A native result contract accepts iff execution succeeds without error and
    the last produced result set equals `expected`. -/
def nativeContractAccepts {Result : Type} [DecidableEq Result]
    (expected : Result) : NativeExecOutcome Result → Bool
  | .error => false
  | .ok obs => decide (lastStatementResult obs = some expected)

/-- Soundness of `EarlyRejectFailingPrefix`: under a deterministic fail-stop
    engine where any error on a complete statement prefix `p` aborts before
    executing any trailing statements `s`, every extension `p ++ s` is rejected
    by any result contract. -/
theorem failing_prefix_rejects_extension {Stmt Result : Type} [DecidableEq Result]
    (exec : List Stmt → NativeExecOutcome Result) (expected : Result)
    (failStop : ∀ p s, exec p = .error → exec (p ++ s) = .error)
    (p s : List Stmt) (hErr : exec p = .error) :
    nativeContractAccepts expected (exec (p ++ s)) = false := by
  rw [failStop p s hErr]
  rfl

/-- Appending trailing statements that produce no result (`none`) leaves
    `lastStatementResult` unchanged. -/
theorem lastStatementResult_append_all_none {Result : Type}
    (init trailing : List (Option Result))
    (hNone : ∀ obs ∈ trailing, obs = none) :
    lastStatementResult (init ++ trailing) = lastStatementResult init := by
  unfold lastStatementResult
  rw [List.foldl_append]
  induction trailing generalizing init with
  | nil => rfl
  | cons x xs ih =>
    have hx : x = none := hNone x (by simp)
    have hxs : ∀ obs ∈ xs, obs = none := fun obs hmem => hNone obs (by simp [hmem])
    simp only [List.foldl_cons, hx]
    have hor : Option.or none (List.foldl (fun last next => next.or last) none init) =
        List.foldl (fun last next => next.or last) none init := rfl
    rw [hor]
    exact ih init hxs

/-- Soundness of `EarlyRejectColumnArityMismatch`: if the final result-producing
    statement produces a result `res` whose column arity differs from
    `arity expected` and all trailing statements produce `none`, the script is
    guaranteed rejected by the native result contract. -/
theorem column_arity_mismatch_rejects {Result : Type} [DecidableEq Result]
    (arity : Result → Nat) (expected actual : Result)
    (prefixObs trailingObs : List (Option Result))
    (hTrailing : ∀ obs ∈ trailingObs, obs = none)
    (hArity : arity actual ≠ arity expected) :
    nativeContractAccepts expected (.ok (prefixObs ++ [some actual] ++ trailingObs)) = false := by
  simp only [nativeContractAccepts]
  rw [lastStatementResult_append_all_none (prefixObs ++ [some actual]) trailingObs hTrailing]
  have hLast : lastStatementResult (prefixObs ++ [some actual]) = some actual := by
    unfold lastStatementResult
    rw [List.foldl_append]
    rfl
  rw [hLast]
  have hNe : actual ≠ expected := fun hEq => hArity (congrArg arity hEq)
  simp [hNe]

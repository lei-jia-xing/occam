namespace EditIntervals

/-- An edit uses half-open byte offsets. Replacements may additionally
    conflict at a shared endpoint. -/
structure Edit where
  startByte : Nat
  endByte : Nat
  replaces : Bool
  savings : Nat

def Edit.startKey (edit : Edit) : Nat :=
  2 * edit.startByte + if edit.replaces then 0 else 1

def Edit.endKey (edit : Edit) : Nat :=
  2 * edit.endByte + if edit.replaces then 1 else 0

theorem compatible_boundaries (earlier later : Edit) :
    earlier.endKey ≤ later.startKey ↔
      earlier.endByte < later.startByte ∨
        (earlier.endByte = later.startByte ∧ (earlier.replaces = false ∨ later.replaces = false)) := by
  cases hEarlier : earlier.replaces <;> cases hLater : later.replaces <;>
    simp [Edit.endKey, Edit.startKey, hEarlier, hLater] <;> omega

def Compatible (later earlier : Edit) : Prop :=
  earlier.endKey ≤ later.startKey

def eligible (last : Edit) (offered : List Edit) : List Edit :=
  offered.filter (fun edit => decide (edit.endKey ≤ last.startKey))

def weight (schedule : List Edit) : Nat :=
  (schedule.map Edit.savings).sum

/-- The weighted interval recurrence, with the latest-finishing edit first.
    The implementation stores optima in the opposite order by end key. -/
def optimum : List Edit → Nat
  | [] => 0
  | last :: previous =>
    max (optimum previous) (last.savings + optimum (eligible last previous))
termination_by offered => offered.length
decreasing_by
  · simp_wf
  · have h := List.length_filter_le (fun edit => decide (edit.endKey ≤ last.startKey)) previous
    simp_wf
    unfold eligible
    omega

/-- Every compatible subsequence is bounded by the recurrence. -/
theorem weight_le_optimum (offered selected : List Edit)
    (hSublist : selected.Sublist offered) (hSchedule : selected.Pairwise Compatible) :
    weight selected ≤ optimum offered := by
  induction offered using optimum.induct generalizing selected with
  | case1 =>
    obtain rfl := List.sublist_nil.mp hSublist
    simp [optimum, weight]
  | case2 last previous ihSkip ihTake =>
    by_cases hPick : selected.Sublist previous
    · rw [optimum]
      exact Nat.le_trans (ihSkip selected hPick hSchedule) (Nat.le_max_left _ _)
    · have hCons := List.sublist_cons_iff.mp hSublist
      rcases hCons with hSkip | ⟨tail, rfl, hTail⟩
      · exact (hPick hSkip).elim
      · obtain ⟨hFits, hCompatible⟩ := List.pairwise_cons.mp hSchedule
        have hFiltered : tail.Sublist (eligible last previous) := by
          unfold eligible
          have hKeep : tail.filter (fun edit => decide (edit.endKey ≤ last.startKey)) = tail := by
            exact List.filter_eq_self.mpr fun edit hMem => decide_eq_true (hFits edit hMem)
          rw [← hKeep]
          exact hTail.filter _
        rw [optimum]
        have hBound := ihTake tail hFiltered hCompatible
        simp only [weight, List.map_cons, List.sum_cons] at hBound ⊢
        exact Nat.le_trans (Nat.add_le_add_left hBound _) (Nat.le_max_right _ _)

/-- Both branches of the recurrence correspond to actual schedules. -/
theorem optimum_attained (offered : List Edit) :
    ∃ selected : List Edit, selected.Sublist offered ∧
      selected.Pairwise Compatible ∧ weight selected = optimum offered := by
  induction offered using optimum.induct with
  | case1 => exact ⟨[], .slnil, .nil, by simp [optimum, weight]⟩
  | case2 last previous ihSkip ihTake =>
    by_cases hTake : optimum previous ≤ last.savings + optimum (eligible last previous)
    · obtain ⟨chosen, hSublist, hCompatible, hWeight⟩ := ihTake
      refine ⟨last :: chosen, .cons_cons _ (hSublist.trans List.filter_sublist), .cons ?_ hCompatible, ?_⟩
      · intro earlier hMem
        change earlier.endKey ≤ last.startKey
        exact of_decide_eq_true (List.mem_filter.mp (hSublist.subset hMem)).2
      · rw [optimum, Nat.max_eq_right hTake]
        simpa only [weight, List.map_cons, List.sum_cons] using congrArg (last.savings + ·) hWeight
    · obtain ⟨chosen, hSublist, hCompatible, hWeight⟩ := ihSkip
      refine ⟨chosen, .cons _ hSublist, hCompatible, ?_⟩
      rw [optimum, Nat.max_eq_left (Nat.le_of_not_ge hTake)]
      exact hWeight


/-- In descending finish order, eligible predecessors form one suffix.
    This is a prefix in the implementation's ascending finish order. -/
theorem eligible_is_suffix (last : Edit) (previous : List Edit)
    (hOrder : previous.Pairwise fun later earlier => earlier.endKey ≤ later.endKey) :
    eligible last previous = previous.dropWhile (fun edit => !(decide (edit.endKey ≤ last.startKey))) := by
  induction previous with
  | nil => rfl
  | cons front rest ih =>
    obtain ⟨hEnds, hRest⟩ := List.pairwise_cons.mp hOrder
    by_cases hFront : front.endKey ≤ last.startKey
    · have hFilter : eligible last rest = rest := by
        apply List.filter_eq_self.mpr
        intro edit hMem
        exact decide_eq_true (Nat.le_trans (hEnds edit hMem) hFront)
      simp only [eligible] at hFilter
      simp [eligible, hFront, hFilter]
    · simpa [eligible, List.dropWhile_cons, hFront] using ih hRest

/-- Extending the available prefix can only increase the optimum. -/
theorem optimum_le_of_sublist {earlier later : List Edit} (hSublist : earlier.Sublist later) :
    optimum earlier ≤ optimum later := by
  obtain ⟨selected, hSelected, hCompatible, hWeight⟩ := optimum_attained earlier
  rw [← hWeight]
  exact weight_le_optimum later selected (hSelected.trans hSublist) hCompatible

end EditIntervals

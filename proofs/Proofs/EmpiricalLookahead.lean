/-! An exact cost table for choosing a constant width for the next window.
    The model uses equally long waves and charges unused slots in the final
    accepting wave. Actual worker durations and cancellation costs can vary. -/

namespace EmpiricalLookahead

structure Run where
  retired : Nat
  accepted : Bool

structure Costs where
  wave : Int
  waste : Int

def waveCount (run : Run) (width : Nat) : Nat :=
  (run.retired - 1) / width + 1

def runCost (costs : Costs) (run : Run) (width : Nat) : Int :=
  let waves : Int := waveCount run width
  costs.wave * waves +
    if run.accepted then costs.waste * (waves * (width : Int) - (run.retired : Int)) else 0

def historyCost (costs : Costs) (runs : List Run) (width : Nat) : Int :=
  (runs.map (fun run => runCost costs run width)).sum

def acceptedCount (runs : List Run) : Nat :=
  (runs.filter Run.accepted).length

def acceptedRetirements (runs : List Run) : Nat :=
  ((runs.filter Run.accepted).map Run.retired).sum

theorem one_wave_when_width_covers_run (run : Run) (width : Nat)
    (positive : 0 < run.retired) (covers : run.retired ≤ width) :
    waveCount run width = 1 := by
  have hDiv : (run.retired - 1) / width = 0 := Nat.div_eq_of_lt (by omega)
  simp [waveCount, hDiv]

theorem cost_of_untracked_width (costs : Costs) (runs : List Run) (width : Nat)
    (covers : ∀ run ∈ runs, 0 < run.retired ∧ run.retired ≤ width) :
    historyCost costs runs width =
      costs.wave * (runs.length : Int) +
        costs.waste * ((acceptedCount runs : Int) * (width : Int) -
          (acceptedRetirements runs : Int)) := by
  induction runs with
  | nil => simp [historyCost, acceptedCount, acceptedRetirements]
  | cons run rest ih =>
    have hRun := covers run (by simp)
    have hWaves := one_wave_when_width_covers_run run width hRun.1 hRun.2
    have hRest := ih (fun run hMem => covers run (by simp [hMem]))
    have hCons : historyCost costs (run :: rest) width =
        runCost costs run width + historyCost costs rest width := rfl
    rw [hCons, hRest]
    cases hAccept : run.accepted
    · simp [runCost, hWaves, hAccept, acceptedCount, acceptedRetirements, Int.mul_add]
      omega
    · simp [runCost, hWaves, hAccept, acceptedCount, acceptedRetirements,
        Int.mul_add, Int.add_mul, Int.mul_sub]
      omega

theorem add_run_updates_cost (costs : Costs) (run : Run) (history : List Run) (width : Nat) :
    historyCost costs (run :: history) width =
      runCost costs run width + historyCost costs history width := rfl

theorem wider_than_every_run_cannot_help (costs : Costs) (runs : List Run)
    (width wider : Nat) (widthOrder : width ≤ wider) (wasteNonnegative : 0 ≤ costs.waste)
    (covers : ∀ run ∈ runs, 0 < run.retired ∧ run.retired ≤ width) :
    historyCost costs runs width ≤ historyCost costs runs wider := by
  have hWider : ∀ run ∈ runs, 0 < run.retired ∧ run.retired ≤ wider :=
    fun run present => ⟨(covers run present).1, Nat.le_trans (covers run present).2 widthOrder⟩
  rw [cost_of_untracked_width costs runs width covers, cost_of_untracked_width costs runs wider hWider]
  have hWidths : (width : Int) ≤ (wider : Int) := Int.ofNat_le.mpr widthOrder
  have hProduct := Int.mul_le_mul_of_nonneg_left hWidths
      (Int.natCast_nonneg (acceptedCount runs))
  have hDifference : (acceptedCount runs : Int) * (width : Int) - (acceptedRetirements runs : Int) ≤
      (acceptedCount runs : Int) * (wider : Int) - (acceptedRetirements runs : Int) := by omega
  have hWaste := Int.mul_le_mul_of_nonneg_left hDifference wasteNonnegative
  omega

def select (score : Nat → Int) (best : Nat) : List Nat → Nat
  | [] => best
  | candidate :: rest =>
    select score (if score candidate < score best then candidate else best) rest

theorem select_minimum (score : Nat → Int) (best : Nat) (remaining : List Nat) :
    select score best remaining ∈ best :: remaining ∧
      ∀ width ∈ best :: remaining, score (select score best remaining) ≤ score width := by
  induction remaining generalizing best with
  | nil => simp [select]
  | cons candidate rest ih =>
    rw [select]
    by_cases hBetter : score candidate < score best
    · rw [ite_eq_left hBetter]
      obtain ⟨hMem, hLeast⟩ := ih candidate
      refine ⟨by simp [hMem], ?_⟩
      intro width present
      rcases List.mem_cons.mp present with rfl | inTail
      · exact Int.le_trans (hLeast candidate (by simp)) (Int.le_of_lt hBetter)
      · exact hLeast width inTail
    · rw [ite_eq_right hBetter]
      obtain ⟨hMem, hLeast⟩ := ih best
      refine ⟨?_, ?_⟩
      · rcases List.mem_cons.mp hMem with same | inTail
        · simp [same]
        · simp [inTail]
      · intro width present
        simp only [List.mem_cons] at present
        rcases present with same | same | inTail
        · subst width
          exact hLeast best (by simp)
        · subst width
          exact Int.le_trans (hLeast best (by simp)) (Int.not_lt.mp hBetter)
        · exact hLeast width (by simp [inTail])

/-- The archived lengths bound the widths that need to be represented.
    If the configured limit is reached first, the fold checks every width. -/
theorem select_minimum_with_cap (costs : Costs) (runs : List Run) (offered : List Nat)
    (cap jobs : Nat) (capPositive : 0 < cap) (capWithinJobs : cap ≤ jobs)
    (covers : cap = jobs ∨ (∀ run ∈ runs, 0 < run.retired ∧ run.retired ≤ cap))
    (inTable : ∀ width, 0 < width → width ≤ cap → width ∈ 1 :: offered)
    (tableWithinCap : ∀ width ∈ 1 :: offered, 0 < width ∧ width ≤ cap)
    (width : Nat) (widthPositive : 0 < width) (widthWithinJobs : width ≤ jobs)
    (wasteNonnegative : 0 ≤ costs.waste) :
    let score := historyCost costs runs
    let chosen := select score 1 offered
    (0 < chosen ∧ chosen ≤ jobs) ∧ score chosen ≤ score width := by
  dsimp
  obtain ⟨hChosen, best⟩ := select_minimum (historyCost costs runs) 1 offered
  obtain ⟨positive, belowCap⟩ := tableWithinCap _ hChosen
  refine ⟨⟨positive, Nat.le_trans belowCap capWithinJobs⟩, ?_⟩
  by_cases within : width ≤ cap
  · exact best width (inTable width widthPositive within)
  · rcases covers with same | lengths
    · exact (within (same.symm ▸ widthWithinJobs)).elim
    · exact Int.le_trans (best cap (inTable cap capPositive (Nat.le_refl _)))
        (wider_than_every_run_cannot_help costs runs cap width (by omega) wasteNonnegative lengths)

end EmpiricalLookahead

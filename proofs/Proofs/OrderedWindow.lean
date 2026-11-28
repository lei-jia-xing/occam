/-!
# Ordered oracle window

The model records candidate verdicts, reuse flags, and the stopping reason
for one invocation of `runSlidingWindow`. Oracle verdicts are deterministic
in the candidate cache key. The input represents an ordered stream whose
producer is independent of speculative execution. Warm cache entries and
the starting permit balance are shared with the serial model.

The launch plan extends the bounded active window with potential later
launches. A visibility predicate distinguishes a cache hit from an in-flight
join without prescribing worker completion order. Owners remain in the
logical plan even if an inconclusive worker removes its entry from the real
cache, since retirement stops at that owner's ordinal. Permit consumption
after the stopping decision is shown separately from the retirement trace.

Process cancellation, reaping failures, metrics, and external deadlines
are outside the model.
-/

namespace OrderedWindow

inductive Verdict where
  | reject | accept | inconclusive
  deriving DecidableEq, Repr

inductive Stop where
  | exhausted | accepted | inconclusive | denied | missingOwner
  deriving DecidableEq, Repr

structure Event where
  key : Nat
  verdict : Verdict
  cacheHit : Bool
  deriving DecidableEq, Repr

structure Trace where
  events : List Event
  stop : Stop
  deriving DecidableEq, Repr

structure Config where
  oracle : Nat → Verdict
  precomputed : Nat → Option Verdict
  cacheEnabled : Bool

structure LaunchState where
  next : Nat
  permits : Option Nat
  owner : Nat → Option Nat

inductive Task where
  | ready (verdict : Verdict) (hit : Bool)
  | join (owner : Nat)
  | denied

abbrev Published := Nat → Option Verdict

def assign {α : Type} (entries : Nat → Option α) (key : Nat) (value : α) :
    Nat → Option α :=
  fun query => if query = key then some value else entries query

/-- Look up cached work before reserving a permit for a fresh execution. -/
def launch (config : Config) (visible : Nat → Nat → Bool) (state : LaunchState)
    (key : Nat) : Task × LaunchState :=
  let step := {state with next := state.next + 1}
  match config.precomputed key with
  | some verdict => (.ready verdict true, step)
  | none =>
    match state.owner key with
    | some owner =>
      (if visible owner state.next then .ready (config.oracle key) true else .join owner, step)
    | none =>
      match state.permits with
      | some 0 => (.denied, step)
      | remaining =>
        let newOwners := if config.cacheEnabled then assign state.owner key state.next else state.owner
        (.ready (config.oracle key) false,
          {step with owner := newOwners, permits := remaining.map (· - 1)})

def resolve (published : Published) : Task → Except Stop (Verdict × Bool)
  | .ready verdict hit => .ok (verdict, hit)
  | .join owner =>
    match published owner with
    | some verdict => .ok (verdict, true)
    | none => .error .missingOwner
  | .denied => .error .denied

def record (key : Nat) (result : Except Stop (Verdict × Bool))
    (rest : Unit → Trace) : Trace :=
  match result with
  | .error reason => ⟨[], reason⟩
  | .ok (.accept, hit) => ⟨[⟨key, .accept, hit⟩], .accepted⟩
  | .ok (.inconclusive, hit) => ⟨[⟨key, .inconclusive, hit⟩], .inconclusive⟩
  | .ok (.reject, hit) =>
    let tail := rest ()
    ⟨⟨key, .reject, hit⟩ :: tail.events, tail.stop⟩

def serial (config : Config) (state : LaunchState) : List Nat → Trace
  | [] => ⟨[], .exhausted⟩
  | key :: rest =>
    let (task, more) := launch config (fun _ _ => true) state key
    record key (resolve (fun _ => none) task) (fun _ => serial config more rest)

def plan (config : Config) (visible : Nat → Nat → Bool) (state : LaunchState) :
    List Nat → List (Nat × Nat × Task)
  | [] => []
  | key :: rest =>
    let (task, more) := launch config visible state key
    (key, state.next, task) :: plan config visible more rest

/-- Publication at the earlier owner supplies the observation to a later
    join. The table models the readable owner result cells. -/
def retire (published : Published) : List (Nat × Nat × Task) → Trace
  | [] => ⟨[], .exhausted⟩
  | (key, ordinal, task) :: rest =>
    record key (resolve published task)
      (fun _ => retire (assign published ordinal .reject) rest)

def SoundOwners (config : Config) (state : LaunchState) (published : Published) : Prop :=
  ∀ key ordinal, state.owner key = some ordinal →
    ordinal < state.next ∧ published ordinal = some (config.oracle key)

/-- A hit and a join to the same owner consume the same number of permits
    and yield the same logical observation at retirement. -/
theorem same_dispatch (config : Config) (visible : Nat → Nat → Bool)
    (state : LaunchState) (published : Published) (key : Nat)
    (hOwners : SoundOwners config state published) :
    let speculative := launch config visible state key
    let ordered := launch config (fun _ _ => true) state key
    speculative.2 = ordered.2 ∧
      resolve published speculative.1 = resolve (fun _ => none) ordered.1 := by
  dsimp
  cases hCached : config.precomputed key with
  | some verdict => simp [launch, resolve, hCached]
  | none =>
    cases hOwner : state.owner key with
    | some ordinal =>
      have hPublished := (hOwners key ordinal hOwner).2
      cases hVisible : visible ordinal state.next <;>
        simp [launch, resolve, hCached, hOwner, hPublished, hVisible]
    | none =>
      cases hPermits : state.permits with
      | none => simp [launch, resolve, hCached, hOwner, hPermits]
      | some count => cases count <;> simp [launch, resolve, hCached, hOwner, hPermits]

theorem extend_owners (config : Config) (visible : Nat → Nat → Bool)
    (state : LaunchState) (published : Published) (key : Nat) (hit : Bool)
    (hOwners : SoundOwners config state published)
    (hRejected : resolve published (launch config visible state key).1 = .ok (.reject, hit)) :
    SoundOwners config (launch config visible state key).2
      (assign published state.next .reject) := by
  have unchanged (remaining : Option Nat) :
      SoundOwners config {next := state.next + 1, permits := remaining, owner := state.owner}
        (assign published state.next .reject) := by
    intro query ordinal present
    obtain ⟨before, value⟩ := hOwners query ordinal present
    exact ⟨Nat.lt_succ_of_lt before, by simpa [assign, Nat.ne_of_lt before] using value⟩
  have fresh (remaining : Option Nat) (hVerdict : config.oracle key = .reject) :
      SoundOwners config
        { next := state.next + 1, permits := remaining,
          owner := if config.cacheEnabled then assign state.owner key state.next else state.owner }
        (assign published state.next .reject) := by
    by_cases enabled : config.cacheEnabled = true
    · intro query ordinal present
      by_cases same : query = key
      · subst query
        have hOrdinal : state.next = ordinal := by simpa [enabled, assign] using present
        subst ordinal
        exact ⟨Nat.lt_succ_self _, by simp [assign, hVerdict]⟩
      · have prior : state.owner query = some ordinal := by simpa [enabled, assign, same] using present
        exact unchanged remaining query ordinal prior
    · simpa [enabled] using unchanged remaining
  cases hCached : config.precomputed key with
  | some verdict => simpa [launch, hCached] using unchanged state.permits
  | none =>
    cases hOwner : state.owner key with
    | some ordinal => simpa [launch, hCached, hOwner] using unchanged state.permits
    | none =>
      cases hPermits : state.permits with
      | none =>
        simp [launch, hCached, hOwner, hPermits, resolve] at hRejected
        simpa [launch, hCached, hOwner, hPermits] using fresh none hRejected.1
      | some count =>
        cases count with
        | zero => simp [launch, hCached, hOwner, hPermits, resolve] at hRejected
        | succ count =>
          simp [launch, hCached, hOwner, hPermits, resolve] at hRejected
          simpa [launch, hCached, hOwner, hPermits] using fresh (some count) hRejected.1

theorem same_trace (config : Config) (visible : Nat → Nat → Bool)
    (keys : List Nat) (state : LaunchState) (published : Published)
    (hOwners : SoundOwners config state published) :
    retire published (plan config visible state keys) = serial config state keys := by
  induction keys generalizing state published with
  | nil => rfl
  | cons key keys ih =>
    obtain ⟨hState, hResult⟩ := same_dispatch config visible state published key hOwners
    cases hWindow : launch config visible state key with
    | mk task next =>
      cases hSerial : launch config (fun _ _ => true) state key with
      | mk ordered after =>
        simp only [hWindow, hSerial] at hState hResult
        change next = after at hState
        subst after
        simp only [plan, hWindow, retire, serial, hSerial, ← hResult]
        cases hOutcome : resolve published task with
        | error reason => rfl
        | ok details =>
          obtain ⟨verdict, hit⟩ := details
          cases verdict
          · have nextOwners : SoundOwners config next (assign published state.next .reject) := by
              have hRejected : resolve published (launch config visible state key).1 = .ok (.reject, hit) := by
                simpa [hWindow] using hOutcome
              simpa [hWindow] using extend_owners config visible state published key hit hOwners hRejected
            simp [record, ih _ _ nextOwners]
          · rfl
          · rfl


def initial (permits : Option Nat) : LaunchState :=
  {next := 0, permits, owner := fun _ => none}

/-- Ordered verdicts and hit flags are independent of publication times
    when the invocation starts with the same cache and permit balance. -/
theorem same_trace_from_initial (config : Config) (visible : Nat → Nat → Bool)
    (keys : List Nat) (permits : Option Nat) :
    retire (fun _ => none) (plan config visible (initial permits) keys) =
      serial config (initial permits) keys := by
  apply same_trace
  simp [SoundOwners, initial]

/-- Once retirement has an ordered stopping decision, an unneeded suffix
    of potential launches contributes no retirement events. -/
theorem retire_append_stops (published : Published) (before suffix : List (Nat × Nat × Task))
    (hStop : (retire published before).stop ≠ .exhausted) :
    retire published (before ++ suffix) = retire published before := by
  induction before generalizing published with
  | nil => exact (hStop rfl).elim
  | cons entry rest ih =>
    obtain ⟨key, ordinal, task⟩ := entry
    cases hOutcome : resolve published task with
    | error reason => simp [retire, record, hOutcome]
    | ok details =>
      obtain ⟨verdict, hit⟩ := details
      cases verdict
      · have hTail : (retire (assign published ordinal .reject) rest).stop ≠ .exhausted := by
          simpa [retire, record, hOutcome] using hStop
        simp [retire, record, hOutcome, ih _ hTail]
      · simp [retire, record, hOutcome]
      · simp [retire, record, hOutcome]

/-- Index an in-flight owner in the contiguous active queue. -/
theorem active_owner_index (head next owner : Nat)
    (hActive : head ≤ owner) (hEarlier : owner < next) :
    owner - (next - (next - head)) = owner - head ∧
      owner - (next - (next - head)) < next - head := by
  omega

/-- A finite budget holds free permits, outstanding reservations, and
    physically started work. Only an unstarted reservation can be refunded. -/
structure PermitBalance where
  free : Nat
  reserved : Nat
  started : Nat

def PermitBalance.total (balance : PermitBalance) : Nat :=
  balance.free + balance.reserved + balance.started

inductive PermitStep : PermitBalance → PermitBalance → Prop where
  | reserve (balance : PermitBalance) (available : 0 < balance.free) :
    PermitStep balance {balance with free := balance.free - 1, reserved := balance.reserved + 1}
  | start (balance : PermitBalance) (reserved : 0 < balance.reserved) :
    PermitStep balance {balance with reserved := balance.reserved - 1, started := balance.started + 1}
  | refundUnstarted (balance : PermitBalance) (reserved : 0 < balance.reserved) :
    PermitStep balance {balance with reserved := balance.reserved - 1, free := balance.free + 1}

theorem permit_step_preserves_total {before after : PermitBalance} (step : PermitStep before after) :
    after.total = before.total := by
  cases step <;> simp only [PermitBalance.total] <;> omega

inductive PermitHistory (limit : Nat) : PermitBalance → Prop where
  | initial : PermitHistory limit ⟨limit, 0, 0⟩
  | step {before after : PermitBalance} :
    PermitHistory limit before → PermitStep before after → PermitHistory limit after

theorem permit_history_preserves_total {limit : Nat} {balance : PermitBalance}
    (history : PermitHistory limit balance) : balance.total = limit := by
  induction history with
  | initial => simp [PermitBalance.total]
  | step _ step ih => exact (permit_step_preserves_total step).trans ih

theorem physical_starts_stay_within_budget {limit : Nat} {balance : PermitBalance}
    (history : PermitHistory limit balance) : balance.started ≤ limit := by
  have hTotal := permit_history_preserves_total history
  unfold PermitBalance.total at hTotal
  omega

/-- A later speculative worker that starts before the head commits can
    consume the final permit without changing this invocation's retirement.
    The next invocation can therefore see a different budget stop. -/
theorem speculation_can_spend_a_later_permit :
    let config : Config :=
      {oracle := fun key => if key = 0 then .accept else .reject,
        precomputed := fun _ => none, cacheEnabled := true}
    let start := initial (some 2)
    let afterHead := (launch config (fun _ _ => true) start 0).2
    let afterSpeculation := (launch config (fun _ _ => true) afterHead 1).2
    retire (fun _ => none) (plan config (fun _ _ => false) start [0, 1]) =
      serial config start [0, 1] ∧
    (serial config afterHead [2]).stop = .exhausted ∧
    (serial config afterSpeculation [2]).stop = .denied := by
  decide

end OrderedWindow

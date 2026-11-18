#!/usr/bin/env python3
"""Cross-validate occam's per-statement modeled dependencies against SQLite.

For each corpus .sql file this harness:
  1. runs `occam --dump-facts --dialect sqlite` (built via cabal first),
  2. replays the file statement-by-statement (sliced on occam's own spans,
     so segmentation is shared, not re-implemented) in a fresh in-memory
     SQLite DB with a `set_authorizer` callback recording ground truth,
  3. aligns and compares the two per statement.

Buckets per statement:
  agree        occam's facts match engine ground truth under the rules below
  abstain      occam emitted ANY barrier (fenced; counted as fenced-rate,
               never a mismatch)
  engine-error the statement failed in the engine (truth is "fails")
  mismatch     comparable statements that disagree (every one is triaged
               by hand into OCCAM BUG / GRANULARITY / ENGINE QUIRK)

ALIGNMENT RULES (engine = authorizer callback stream per statement):

  Identifiers are compared case-insensitively on the last dotted component
  (occam may qualify names; the engine reports bare table + db separately).

  F1 sqlite-internals: drop events on `sqlite_%` tables (sqlite_master,
      sqlite_temp_master, sqlite_sequence, sqlite_stat1) and on
      `sqlite_autoindex_%` indexes. Rationale: catalog bookkeeping from DDL
      (CREATE/ALTER/DROP rewrite sqlite_master); occam models user schema.
  F2 vtable-shadows: file-level set VT of tables from engine CREATE_VTABLE;
      drop all events on `{vt}_*` shadow tables (FTS content/segments/idx/
      docsize/config/stat). Rationale: engine internals; occam models the
      vtable as one table (in fact occam currently fences CREATE VIRTUAL
      TABLE, so those statements abstain anyway).
  F3 non-dep actions: drop SELECT, RECURSIVE, COPY, FUNCTION. They carry no
      table/column dependency; occam models routine effects via barriers,
      not per-call (engine-dialect routines fence the statement into
      abstain; only Generic keyword forms lift, and xval runs sqlite).
  F4 sourced expansion: events with source S (trigger/view/CTE name). If S
      is a schema trigger/view (collected from occam ddl + engine CREATEs)
      the access is trigger/view expansion firing at USE time while occam
      attributes it at DEFINITION time (documented attribution gap G-attr):
      dropped from strict compare, counted as engine_expansion. Otherwise
      S is a query-local CTE name (WITH ...): source is stripped and the
      access treated as direct (derived-table subqueries already arrive
      with NULL source).
  F5 pragma scoping: PRAGMA events are dropped unless the statement itself
      is PRAGMA-shaped (engine fires internal pragmas, e.g. data_version /
      page_size, during FTS maintenance).
  F6 ddl-self bookkeeping, same statement only:
      (a) CREATE_TABLE/CREATE_VTABLE(t): drop engine READ/UPDATE on t
          (CHECK/UNIQUE/PK validation scans of the created table).
      (b) CREATE_INDEX(i,t): drop engine READs on t (index-build scans,
          including partial-index predicate columns; occam records the
          index definition, not build reads).
      (c) any DROP_* event: drop engine DELETE on the dropped object(s)
          (DROP TABLE/VIEW also fires DELETE on the victim: drop
          implementation, not a data dependency).
      (d) REINDEX(i) where i was created by the same statement: dropped
          (CREATE INDEX re-indexes itself).
  F7 transactions: the connection runs with autocommit=True so only
      explicit transaction statements produce TRANSACTION/SAVEPOINT
      events (no driver-implicit BEGIN noise).
  R-schema-ddl: occam ReadSchema-mode reads on statements that ALSO carry
      DDL effects (FK REFERENCES parents, view definition sources) are
      definition-time catalog deps with no authorizer counterpart (the
      engine reports no parent access at CREATE time; view bodies are
      authorized at USE time with source=view, dropped by F4). Excluded
      from strict compare, counted as schema_excluded. Schema-mode reads
      on non-DDL statements stay strict (none expected).
  F8 phantom-fts-insert: drop an engine INSERT(t) when t is a vtable AND
      the same statement also reports an unsourced UPDATE/DELETE/DROP on
      a non-internal table. Characterized engine quirk: after an
      in-transaction write to an FTS table (fts4 AND fts5), a later
      UPDATE/DELETE/DROP statement fires a row-neutral phantom
      INSERT(vtable) during stepping, targeting a previously-written
      FTS table (verified: row counts unchanged; plain tables and
      SELECTs never do this). A single statement performs one DML
      operation, so an unsourced INSERT on a vtable co-occurring with a
      non-INSERT write op can only be this artifact (trigger-fired
      writes arrive with source set and are dropped by F4).
  R-target-self-read: an occam read on table t is tolerated (not flagged
      occam-only) when the same statement also WRITES t on both sides.
      The engine does not authorize full-scan reads of write targets:
      unqualified UPDATE/DELETE report only UPDATE/DELETE, while occam
      conservatively models the target scan. Qualified accesses still
      compare strictly via the WHERE-clause READs the engine does report.
R-fk-off (default): foreign_keys stays at the SQLite/Python default
    OFF, so FK enforcement reads and cascade writes (which the
    authorizer DOES report when enforcement is on) are out of scope:
    config-dependent runtime behavior, not statement dependencies.
    FK-ON VARIANT (--fk-on): each fresh DB runs PRAGMA foreign_keys=ON
    before replay, lifting R-fk-off: enforcement reads and cascade
    writes arrive UNSOURCED (no trigger-style source tag) and are
    fully comparable, so FK-bearing statements that agree by default
    may mismatch here. Expected buckets are adjusted explicitly per
    statement in triage (each divergence classified OCCAM BUG /
    GRANULARITY / ENGINE QUIRK); there is deliberately no blanket
    rule dropping FK traffic, so this mode quantifies the gap.
    The occam invocation gains --dump-facts-fk-on so its modeled initial
    enforcement agrees with the engine's; default mode is unaffected.
  R-wildcard: empty occam column lists (SELECT *, unqualified DML target
      scans) and NULL/empty engine columns match any column set.

  C-read: engine READ(t,c) needs an occam read on t covering c (miss =
      occam under-models: the dangerous direction); occam reads need an
      engine READ on t covering them (miss = occam over-models), modulo
      R-target-self-read / R-schema-ddl / R-wildcard.
  C-write: engine INSERT/DELETE need occam insert*/delete writes;
      engine UPDATE(t,c) needs an occam update (exact column-set match)
      or insert-or-update (engine cols subset of write cols: the DO
      UPDATE arm) write. Reverse direction likewise. INSERT/DELETE carry
      no columns on the engine side, so write-column lists are only
      checked for UPDATE.
  C-ddl: CREATE/DROP/ALTER actions match occam ddl ops on table (+index/
      trigger name where both sides have one). CREATE_VTABLE/DROP_VTABLE
      map to create-table/drop-table; engine ALTER_TABLE(db,t) (note: db
      first) maps to occam alter-table/rename-column/rename-relation.
  C-pragma: PRAGMA-shaped statements match loosely: value-carrying engine
      pragmas need >=1 occam session effect (else pragma-effect-ignored);
      bare query-form pragmas (NULL value) match empty occam facts (pure
      queries, no deps). Non-PRAGMA statements with occam session effects
      are flagged.
  C-tx: engine TRANSACTION/SAVEPOINT ops map to TxBegin/TxCommit/
      TxRollbackAll/TxSavepoint/TxReleaseSavepoint/TxRollbackTo and the
      mapped multiset must equal occam pre_tx+post_tx exactly.
  C-maintenance: leftover engine ANALYZE/REINDEX/ATTACH/DETACH actions
      (no occam counterpart by design) are flagged for triage.
  R-occam-parse: occam per-statement parse_error + engine success =
      mismatch (frontend gap); + engine error = engine-error bucket.
Known engine blind spot (no tolerance rule; triage only): INSERT..SELECT
    eligible for the transfer (xfer) optimization reports INSERT but no
    source READs. Pinned in-corpus by
    scripts/xval_corpus/02_insert_select_xfer.sql (xfer shape = two
    tables with identical schemas + plain SELECT *; same-table
    INSERT..SELECT does NOT take xfer and reports READs normally).

Known gaps (KNOWN_GAPS below, keyed by (file, statement index)): every
currently-triaged NON-agree verdict that is NOT an occam bug lives here
with its exact expected reasons, scoped per mode ("default", "--fk-on",
or "both"). The harness exits 0 only when every non-agree verdict is an
unpinned abstain/engine-error or a mismatch covered by a pin with
exactly-matching reasons; any NEW mismatch, any pin whose reasons
drifted, and any pin that no longer reproduces (stale: statement now
agrees, or vanished) exits NONZERO. Pins for files outside the
current --corpus run are skipped (listed, not failed), so subset runs
stay usable; the nightly gate always runs the full default corpus.
Re-triage workflow when the gate fails: (1) re-run to confirm (output
is deterministic); (2) for each NEW/changed item decide OCCAM BUG
(fix modeling, no pin) vs ENGINE QUIRK / UPSTREAM PARSE GAP /
DELIBERATELY-UNMODELED (add or update a pin with a note naming the
root cause); (3) delete pins that went stale because the underlying
gap was fixed, never to silence the gate; (4) keep both modes green.

Output: human summary on stdout + machine JSON (--out). Mismatch records
carry statement sha256 + a <=200-char preview, never full SQL text.
Deterministic: statement order, sorted sets, sort_keys JSON.
"""

import argparse
import hashlib
import json
import os
import sqlite3
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DEFAULT_GLOBS = [
    "benchmarks/corpus/*.sql",
    "benchmarks/real_fuzz_corpus/sqlite_*.sql",
    "scripts/xval_corpus/*.sql",
]

ACTION = {
    0: "COPY",
    1: "CREATE_INDEX",
    2: "CREATE_TABLE",
    3: "CREATE_TEMP_INDEX",
    4: "CREATE_TEMP_TABLE",
    5: "CREATE_TEMP_TRIGGER",
    6: "CREATE_TEMP_VIEW",
    7: "CREATE_TRIGGER",
    8: "CREATE_VIEW",
    9: "DELETE",
    10: "DROP_INDEX",
    11: "DROP_TABLE",
    12: "DROP_TEMP_INDEX",
    13: "DROP_TEMP_TABLE",
    14: "DROP_TEMP_TRIGGER",
    15: "DROP_TEMP_VIEW",
    16: "DROP_TRIGGER",
    17: "DROP_VIEW",
    18: "INSERT",
    19: "PRAGMA",
    20: "READ",
    21: "SELECT",
    22: "TRANSACTION",
    23: "UPDATE",
    24: "ATTACH",
    25: "DETACH",
    26: "ALTER_TABLE",
    27: "REINDEX",
    28: "ANALYZE",
    29: "CREATE_VTABLE",
    30: "DROP_VTABLE",
    31: "FUNCTION",
    32: "SAVEPOINT",
    33: "RECURSIVE",
}

INSERT_KINDS = {"insert", "insert-or-ignore", "insert-or-replace", "insert-or-update"}
UPDATE_KINDS = {"update", "insert-or-update"}
TXMAP = {
    ("TRANSACTION", "BEGIN"): "TxBegin",
    ("TRANSACTION", "COMMIT"): "TxCommit",
    ("TRANSACTION", "ROLLBACK"): "TxRollbackAll",
    ("SAVEPOINT", "BEGIN"): "TxSavepoint",
    ("SAVEPOINT", "RELEASE"): "TxReleaseSavepoint",
    ("SAVEPOINT", "ROLLBACK"): "TxRollbackTo",
}
# Triaged non-agree verdicts that are NOT occam bugs, keyed by
# (repo-relative file, statement index) -> {mode: (expected reasons, note)}.
# Mode is "default", "fk-on", or "both" (same expectation in both modes).
# Reasons must match EXACTLY (sorted); any drift fails the gate. Verified
# against default + --fk-on runs on 2026-11-17 (sqlite 3.53.3); re-verify
# (never blindly extend) per the header workflow. Abstains (incl. VACUUM
# 04#5) and engine-errors pass unpinned by design.
KNOWN_GAPS = {
    # -- upstream parse gaps (engine succeeds, occam parse_error) --
    ("benchmarks/real_fuzz_corpus/sqlite_fuzz_recursive_trigger.sql", 0): {
        "both": (
            ["occam-parse-error"],
            ("upstream grammar rejects boolean PRAGMA value `= true` "
            "(TODO.org Upstream Gaps); engine runs it (recursive_triggers=true)"),
        ),
    },
    ("scripts/xval_corpus/04_maintenance.sql", 4): {
        "both": (
            ["occam-parse-error"],
            "REINDEX unsupported by occam frontend; engine succeeds (REINDEX(mi))",
        ),
    },
    ("scripts/xval_corpus/06_midfile_toggle.sql", 4): {
        "both": (
            ["occam-parse-error"],
            ("upstream grammar rejects bare `PRAGMA foreign_keys=ON` "
            "(Expected: a concrete value, found: ON); same boolean-PRAGMA "
            "gap class as recursive_trigger#0"),
        ),
    },
    # -- engine quirks / by-design gaps --
    ("scripts/xval_corpus/02_insert_select_xfer.sql", 3): {
        "both": (
            ["occam-only-read:isel_src"],
            ("engine transfer (xfer) optimization reports INSERT with zero "
            "source READs; occam's conservative source read is correct"),
        ),
    },
    ("scripts/xval_corpus/04_maintenance.sql", 3): {
        "both": (
            ["engine-only-maintenance:ANALYZE(m)"],
            "ANALYZE has no occam counterpart by design (C-maintenance)",
        ),
    },
    # -- mid-file toggle knock-on (default) / enforcement reads (fk-on) --
    ("scripts/xval_corpus/06_midfile_toggle.sql", 5): {
        "default": (
            ["engine-only-delete:tgl_c", "engine-only-read:tgl_c"],
            ("knock-on of 06#4: engine executes the toggle and cascades, "
            "occam never threads enforcement ON (toggle unparsed) so the "
            "DELETE does not expand; if upstream ever parses bare ON, #4 "
            "and this pin go stale together and the DELETE must agree"),
        ),
        "fk-on": (
            ["engine-only-read:tgl_c"],
            ("writes agree via initial-ON expansion (parent+child DELETE); "
            "only deliberately-unmodeled FK check reads diverge"),
        ),
    },
    # -- fk-on only: enforcement-check reads surface unsourced and are
    # -- deliberately unmodeled (Enforcement.hs); writes agree everywhere,
    # -- including expanded cascades (03#6-7, 05#4-5). Default mode agrees.
    ("benchmarks/corpus/01_cte_recursive_analytics.sql", 1): {
        "fk-on": (
            ["engine-only-read:employees"],
            "self-referential FK check reads (emp_id, manager_id) on INSERT",
        ),
    },
    ("benchmarks/corpus/04_multitable_diamond_dag.sql", 4): {
        "fk-on": (
            [
                "engine-only-read:cluster_node_left",
                "engine-only-read:cluster_node_right",
            ],
            "FK enforcement scans on parent INSERT",
        ),
    },
    ("benchmarks/corpus/04_multitable_diamond_dag.sql", 5): {
        "fk-on": (
            ["engine-only-read:cluster_root", "engine-only-read:cluster_sink"],
            "FK enforcement scans on child INSERT",
        ),
    },
    ("benchmarks/corpus/04_multitable_diamond_dag.sql", 6): {
        "fk-on": (
            ["engine-only-read:cluster_root", "engine-only-read:cluster_sink"],
            "FK enforcement scans on child INSERT",
        ),
    },
    ("benchmarks/corpus/04_multitable_diamond_dag.sql", 7): {
        "fk-on": (
            [
                "engine-only-read:cluster_node_left",
                "engine-only-read:cluster_node_right",
            ],
            "FK enforcement scans on sink INSERT",
        ),
    },
    ("scripts/xval_corpus/03_fk_cascade.sql", 3): {
        "fk-on": (["engine-only-read:emp"], "FK enforcement scans on parent INSERT"),
    },
    ("scripts/xval_corpus/03_fk_cascade.sql", 4): {
        "fk-on": (
            ["engine-only-read:dept", "engine-only-read:task"],
            "FK enforcement scans on child INSERT",
        ),
    },
    ("scripts/xval_corpus/03_fk_cascade.sql", 5): {
        "fk-on": (["engine-only-read:emp"], "FK enforcement scans on child INSERT"),
    },
    ("scripts/xval_corpus/03_fk_cascade.sql", 6): {
        "fk-on": (
            ["engine-only-read:emp"],
            "writes agree via expansion (parent+child UPDATE); check reads diverge",
        ),
    },
    ("scripts/xval_corpus/03_fk_cascade.sql", 7): {
        "fk-on": (
            ["engine-only-read:emp", "engine-only-read:task"],
            "writes agree via expansion (3-level cascade DELETE); check reads diverge",
        ),
    },
    ("scripts/xval_corpus/05_set_null_default.sql", 2): {
        "fk-on": (["engine-only-read:snd_c"], "FK enforcement scans on parent INSERT"),
    },
    ("scripts/xval_corpus/05_set_null_default.sql", 3): {
        "fk-on": (["engine-only-read:snd_p"], "FK enforcement scans on child INSERT"),
    },
    ("scripts/xval_corpus/05_set_null_default.sql", 4): {
        "fk-on": (
            ["engine-only-read:snd_c"],
            ("writes agree via expansion (parent DELETE + SET NULL child "
            "UPDATE); check reads diverge"),
        ),
    },
    ("scripts/xval_corpus/05_set_null_default.sql", 5): {
        "fk-on": (
            ["engine-only-read:snd_c"],
            ("writes agree via expansion (parent UPDATE + SET DEFAULT child "
            "UPDATE); check reads diverge"),
        ),
    },
    ("scripts/xval_corpus/06_midfile_toggle.sql", 2): {
        "fk-on": (["engine-only-read:tgl_c"], "FK enforcement scans on parent INSERT"),
    },
    ("scripts/xval_corpus/06_midfile_toggle.sql", 3): {
        "fk-on": (["engine-only-read:tgl_p"], "FK enforcement scans on child INSERT"),
    },
}


def pin_for(key, mode):
    """Active pin (expected reasons, note) for key in mode, or None."""
    entry = KNOWN_GAPS.get(key)
    if not entry:
        return None
    if mode in entry:
        return entry[mode]
    return entry.get("both")


def check_gate(results, mode):
    """Return (failures, pinned, skipped). Failures is a list of lines."""
    by_file = {f["file"]: f for f in results if not f.get("dump_error")}
    seen = {
        (f["file"], s["index"]): s for f in results for s in f.get("statements", [])
    }
    failures, changed = [], set()
    pinned = 0
    for f in results:
        if f.get("dump_error"):
            failures.append("DUMP-ERROR {}: {}".format(f["file"], f["dump_error"][:120]))
    for key in sorted(seen):
        s = seen[key]
        if s["verdict"] in ("agree", "abstain", "engine-error"):
            continue
        pin = pin_for(key, mode)
        if (
            pin is not None
            and s["verdict"] == "mismatch"
            and sorted(s["reasons"]) == sorted(pin[0])
        ):
            pinned += 1
        elif pin is not None and s["verdict"] == "mismatch":
            changed.add(key)
            failures.append(
                "PIN-DRIFT %s#%d: pinned [%s] but got [%s] (%s)"
                % (
                    key[0],
                    key[1],
                    " ".join(sorted(pin[0])),
                    " ".join(sorted(s["reasons"])),
                    pin[1],
                )
            )
        else:
            failures.append(
                "NEW %s %s#%d: [%s] %s"
                % (
                    s["verdict"],
                    key[0],
                    key[1],
                    " ".join(sorted(s.get("reasons", []))),
                    s["preview"][:120],
                )
            )
    skipped = []
    for key in sorted(KNOWN_GAPS):
        pin = pin_for(key, mode)
        if pin is None or key in changed:
            continue
        if key[0] not in by_file:
            skipped.append("%s#%d (file not in run)" % key)
            continue
        s = seen.get(key)
        if s is None:
            failures.append(
                "STALE %s#%d: statement vanished (%s)" % (key[0], key[1], pin[1])
            )
        elif s["verdict"] in ("agree", "abstain", "engine-error"):
            failures.append(
                "STALE %s#%d: now %s, pin no longer reproduces (%s)"
                % (key[0], key[1], s["verdict"], pin[1])
            )
    return failures, pinned, skipped


def last(name):
    """Last dotted identifier component, lowercased (None-safe)."""
    if name is None:
        return None
    return str(name).split(".")[-1].lower()


def run(cmd, **kw):
    return subprocess.run(cmd, check=False, cwd=ROOT, capture_output=True, text=True, **kw)


def resolve_occam(override):
    if override:
        return override
    build = run(["cabal", "build", "exe:occam", "-v0"])
    if build.returncode != 0:
        raise RuntimeError("cabal build exe:occam failed:\n" + build.stderr[-3000:])
    listed = run(["cabal", "list-bin", "occam"])
    if listed.returncode != 0:
        raise RuntimeError("cabal list-bin occam failed:\n" + listed.stderr[-1000:])
    return listed.stdout.strip()


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()


def corpus_files(patterns):
    import glob as globmod

    found = []
    for pat in patterns:
        found.extend(globmod.glob(os.path.join(ROOT, pat)))
    return sorted(set(found))


def strip_leading_comments(text):
    s = text.lstrip()
    while True:
        if s.startswith("--"):
            nl = s.find("\n")
            s = s[nl + 1 :] if nl >= 0 else ""
            s = s.lstrip()
        elif s.startswith("/*"):
            end = s.find("*/")
            s = s[end + 2 :] if end >= 0 else ""
            s = s.lstrip()
        else:
            return s


def is_pragma_stmt(text):
    return strip_leading_comments(text).upper().startswith("PRAGMA")


def preview_of(text):
    return " ".join(text.split())[:200]


class EngineFacts:
    def __init__(self):
        self.reads = {}  # table -> set(cols) | None (None = wildcard)
        self.inserts = set()
        self.deletes = set()
        self.updates = {}  # table -> set(cols) | None
        self.ddl = []  # (op, target, name)
        self.tx = []
        self.pragmas = []  # (name, value)
        self.maintenance = []
        self.expansion = 0
        self.filtered = 0
        self.phantom = 0

    def add_read(self, table, col):
        cur = self.reads.get(table)
        if cur is None and table in self.reads:
            return
        if not col:
            self.reads[table] = None
        elif table in self.reads:
            self.reads[table].add(col)
        else:
            self.reads[table] = {col}

    def add_update(self, table, col):
        cur = self.updates.get(table)
        if cur is None and table in self.updates:
            return
        if not col:
            self.updates[table] = None
        elif table in self.updates:
            self.updates[table].add(col)
        else:
            self.updates[table] = {col}

    def write_tables(self):
        return set(self.inserts) | set(self.deletes) | set(self.updates)


def table_of(action, p1, p2):
    """Table argument position differs per action (ALTER_TABLE is db-first)."""
    if action in (
        "CREATE_INDEX",
        "CREATE_TEMP_INDEX",
        "DROP_INDEX",
        "DROP_TEMP_INDEX",
        "CREATE_TRIGGER",
        "CREATE_TEMP_TRIGGER",
        "DROP_TRIGGER",
        "DROP_TEMP_TRIGGER",
    ):
        return p2
    if action == "ALTER_TABLE":
        return p2
    return p1


def filter_events(raw, vtables, schema_objs, pragma_shaped):
    """Apply F1-F8. Mutates vtables/schema_objs incrementally. Returns EngineFacts."""
    f = EngineFacts()
    # F2 needs vtables known before filtering; CREATE_VTABLE events in this
    # statement extend the set as they appear, so pre-scan for them first.
    for action, p1, p2, _db, _src in raw:
        if action in ("CREATE_VTABLE",) and p1:
            vtables.add(p1.lower())
    created_tables, created_indexes, index_build_tables, drop_args = (
        set(),
        set(),
        set(),
        set(),
    )
    # F8 arm: unsourced non-INSERT write op on a non-internal table.
    # (Shadow/sqlite-master traffic must not arm: legit FTS INSERTs
    # always carry shadow DELETEs.)
    f8_armed = False
    for action, p1, p2, _db, src in raw:
        if action in ("CREATE_TABLE", "CREATE_TEMP_TABLE", "CREATE_VTABLE") and p1:
            created_tables.add(p1.lower())
        elif action in ("CREATE_INDEX", "CREATE_TEMP_INDEX") and p1:
            created_indexes.add(p1.lower())
            if p2:
                index_build_tables.add(p2.lower())
        elif action in (
            "DROP_INDEX",
            "DROP_TEMP_INDEX",
            "DROP_TRIGGER",
            "DROP_TEMP_TRIGGER",
            "DROP_TABLE",
            "DROP_TEMP_TABLE",
            "DROP_VIEW",
            "DROP_TEMP_VIEW",
            "DROP_VTABLE",
        ):
            for a in (p1, p2):
                if a:
                    drop_args.add(a.lower())
        if action in ("UPDATE", "DELETE") or action.startswith("DROP_"):
            if src is not None and src.lower() in schema_objs:
                continue
            t0 = table_of(action, p1, p2)
            t0 = t0.lower() if t0 else None
            if t0 is None or t0.startswith("sqlite_"):
                continue
            if any(t0.startswith(v + "_") for v in vtables):
                continue
            if (
                action.startswith("DROP_")
                and p1
                and p1.lower().startswith("sqlite_autoindex_")
            ):
                continue
            f8_armed = True
    for action, p1, p2, _db, src in raw:
        table = table_of(action, p1, p2)
        t = table.lower() if table else None
        # F1 sqlite-internals
        if t is not None and t.startswith("sqlite_"):
            f.filtered += 1
            continue
        if (
            action
            in (
                "CREATE_INDEX",
                "CREATE_TEMP_INDEX",
                "DROP_INDEX",
                "DROP_TEMP_INDEX",
                "REINDEX",
            )
            and p1
            and p1.lower().startswith("sqlite_autoindex_")
        ):
            f.filtered += 1
            continue
        # F2 vtable shadows
        if t is not None and any(t.startswith(v + "_") for v in vtables):
            f.filtered += 1
            continue
        # F3 non-dep actions
        if action in ("SELECT", "RECURSIVE", "COPY", "FUNCTION"):
            f.filtered += 1
            continue
        # F4 sourced expansion
        if src is not None and src.lower() in schema_objs:
            f.expansion += 1
            continue
            # else: CTE name -> fall through as direct (source stripped)
        # F5 pragma scoping
        if action == "PRAGMA" and not pragma_shaped:
            f.filtered += 1
            continue
        # F6 ddl-self bookkeeping
        if action in ("READ", "UPDATE") and t in created_tables:
            f.filtered += 1
            continue
        if action == "READ" and t in index_build_tables:
            f.filtered += 1
            continue
        if action == "DELETE" and t in drop_args:
            f.filtered += 1
            continue
        if action == "REINDEX" and p1 and p1.lower() in created_indexes:
            f.filtered += 1
            continue
        # F8 phantom-fts-insert
        if action == "INSERT" and t in vtables and f8_armed:
            f.phantom += 1
            continue
        # structure the survivors
        if action == "READ":
            f.add_read(t, p2.lower() if p2 else None)
        elif action == "INSERT":
            f.inserts.add(t)
        elif action == "DELETE":
            f.deletes.add(t)
        elif action == "UPDATE":
            f.add_update(t, p2.lower() if p2 else None)
        elif action in ("CREATE_TABLE", "CREATE_TEMP_TABLE", "CREATE_VTABLE"):
            f.ddl.append(("create-table", t, None))
        elif action in ("CREATE_INDEX", "CREATE_TEMP_INDEX"):
            f.ddl.append(("create-index", t, p1.lower() if p1 else None))
        elif action in ("CREATE_TRIGGER", "CREATE_TEMP_TRIGGER"):
            f.ddl.append(("create-trigger", t, p1.lower() if p1 else None))
            if p1:
                schema_objs.add(p1.lower())
        elif action in ("CREATE_VIEW", "CREATE_TEMP_VIEW"):
            f.ddl.append(("create-view", t, None))
            if p1:
                schema_objs.add(p1.lower())
        elif action in ("DROP_TABLE", "DROP_TEMP_TABLE", "DROP_VTABLE"):
            f.ddl.append(("drop-table", t, None))
        elif action in ("DROP_INDEX", "DROP_TEMP_INDEX"):
            f.ddl.append(("drop-index", t, p1.lower() if p1 else None))
        elif action in ("DROP_TRIGGER", "DROP_TEMP_TRIGGER"):
            f.ddl.append(("drop-trigger", t, p1.lower() if p1 else None))
        elif action in ("DROP_VIEW", "DROP_TEMP_VIEW"):
            f.ddl.append(("drop-view", t, None))
        elif action == "ALTER_TABLE":
            f.ddl.append(("alter-table", t, None))
        elif action == "TRANSACTION" or action == "SAVEPOINT":
            key = (action, (p1 or "").upper())
            f.tx.append(TXMAP.get(key, f"TX:{action}:{p1}"))
        elif action == "PRAGMA":
            f.pragmas.append((p1.lower() if p1 else None, p2))
        elif action in ("ANALYZE", "REINDEX", "ATTACH", "DETACH"):
            f.maintenance.append(
                "{}({})".format(action, ",".join(a for a in (p1, p2) if a))
            )
        else:
            f.maintenance.append(
                "{}({})".format(action, ",".join(a for a in (p1, p2) if a))
            )
    return f


def ddl_matches(occ_op, occ_target, occ_name, eng_op, eng_target, eng_name):
    if eng_op == "create-table":
        return occ_op == "create-table" and occ_target == eng_target
    if eng_op == "create-index":
        return (
            occ_op == "create-index"
            and occ_target == eng_target
            and (occ_name is None or eng_name is None or occ_name == eng_name)
        )
    if eng_op == "create-trigger":
        return (
            occ_op == "create-trigger"
            and occ_target == eng_target
            and (occ_name is None or eng_name is None or occ_name == eng_name)
        )
    if eng_op == "create-view":
        return occ_op == "create-view" and occ_target == eng_target
    if eng_op == "drop-table":
        return occ_op == "drop-table" and occ_target == eng_target
    if eng_op == "drop-index":
        return occ_op == "drop-index" and (
            occ_target == eng_name or occ_target == eng_target
        )
    if eng_op == "drop-trigger":
        return occ_op == "drop-trigger" and (
            occ_target == eng_name or occ_target == eng_target
        )
    if eng_op == "drop-view":
        return occ_op == "drop-view" and occ_target == eng_target
    if eng_op == "alter-table":
        return (
            occ_op in ("alter-table", "rename-column", "rename-relation")
            and occ_target == eng_target
        )
    return False


def compare(unit, eng, pragma_shaped):
    """Return (verdict, reasons, detail). Assumes buckets checked by caller."""
    reasons = []
    detail = {"schema_excluded": 0, "self_read_tolerated": 0}

    occ_reads, occ_writes = {}, {}
    has_ddl = bool(unit.get("ddl"))
    for r in unit.get("reads", []):
        t = last(r.get("table"))
        if r.get("mode") == "schema" and has_ddl:
            detail["schema_excluded"] += 1  # R-schema-ddl
            continue
        cols = {c.lower() for c in r.get("columns", [])}
        prev = occ_reads.get(t)
        if prev is None and t in occ_reads:
            continue
        occ_reads[t] = None if not cols else (cols if prev is None else prev | cols)
        if t in occ_reads and occ_reads[t] is not None and not cols:
            occ_reads[t] = None
    for w in unit.get("writes", []):
        t = last(w.get("target"))
        cols = {c.lower() for c in w.get("columns", [])} | {
            c.lower() for c in w.get("unqualified_columns", [])
        }
        entry = occ_writes.setdefault(
            t, {"kinds": set(), "cols": set(), "wildcard": False}
        )
        entry["kinds"].add(w.get("kind"))
        if cols:
            entry["cols"] |= cols
        else:
            entry["wildcard"] = True

    # C-read: engine -> occam (missed reads: dangerous)
    for t in sorted(eng.reads):
        ecols = eng.reads[t]
        if t not in occ_reads:
            reasons.append(f"engine-only-read:{t}")
            continue
        ocols = occ_reads[t]
        if ocols is not None and ecols is not None and not ecols <= ocols:
            reasons.append(
                "engine-only-column:{}:{}".format(t, ",".join(sorted(ecols - ocols)))
            )
    # C-read: occam -> engine (over-modelled reads)
    for t in sorted(occ_reads):
        ocols = occ_reads[t]
        if t not in eng.reads:
            if t in occ_writes and t in eng.write_tables():
                detail["self_read_tolerated"] += 1  # R-target-self-read
            else:
                reasons.append(f"occam-only-read:{t}")
            continue
        ecols = eng.reads[t]
        if ecols is None or ocols is None:
            continue
        if not ocols <= ecols:
            reasons.append(
                "occam-only-column:{}:{}".format(t, ",".join(sorted(ocols - ecols)))
            )

    # C-write: engine -> occam
    for t in sorted(eng.inserts):
        if not (t in occ_writes and occ_writes[t]["kinds"] & INSERT_KINDS):
            reasons.append(f"engine-only-insert:{t}")
    for t in sorted(eng.deletes):
        if not (t in occ_writes and "delete" in occ_writes[t]["kinds"]):
            reasons.append(f"engine-only-delete:{t}")
    for t in sorted(eng.updates):
        ecols = eng.updates[t]
        if not (t in occ_writes and occ_writes[t]["kinds"] & UPDATE_KINDS):
            reasons.append(f"engine-only-update:{t}")
            continue
        if (
            ecols is not None
            and not occ_writes[t]["wildcard"]
            and not ecols <= occ_writes[t]["cols"]
        ):
            reasons.append(
                "engine-only-update-column:{}:{}".format(t, ",".join(sorted(ecols - occ_writes[t]["cols"])))
            )
    # C-write: occam -> engine
    for t in sorted(occ_writes):
        kinds, ocols = occ_writes[t]["kinds"], occ_writes[t]["cols"]
        if kinds & INSERT_KINDS and t not in eng.inserts:
            reasons.append("occam-only-write:{}:{}".format(t, ",".join(sorted(kinds))))
        if "delete" in kinds and t not in eng.deletes:
            reasons.append(f"occam-only-write:{t}:delete")
        if "update" in kinds:
            if t not in eng.updates:
                reasons.append(f"occam-only-write:{t}:update")
            else:
                ecols = eng.updates[t]
                if ecols is not None and ocols and not ocols <= ecols:
                    reasons.append(
                        "occam-only-update-column:{}:{}".format(t, ",".join(sorted(ocols - ecols)))
                    )
        for k in sorted(kinds - INSERT_KINDS - {"delete", "update"}):
            reasons.append(f"occam-only-write:{t}:{k}")

    # C-ddl: greedy bipartite match
    occ_ddl = [
        (
            d.get("op"),
            last(d.get("target")),
            last(d.get("name")) if d.get("name") else None,
        )
        for d in unit.get("ddl", [])
    ]
    unmatched = list(occ_ddl)
    for eop, etarget, ename in eng.ddl:
        hit = None
        for i, (oop, otarget, oname) in enumerate(unmatched):
            if ddl_matches(oop, otarget, oname, eop, etarget, ename):
                hit = i
                break
        if hit is None:
            reasons.append(f"engine-only-ddl:{eop}:{etarget}")
        else:
            unmatched.pop(hit)
    for oop, otarget, _oname in unmatched:
        reasons.append(f"occam-only-ddl:{oop}:{otarget}")

    # C-pragma
    occ_session = unit.get("session", [])
    if pragma_shaped:
        if eng.pragmas and not occ_session:
            if any(v is not None for _, v in eng.pragmas):
                names = ",".join(sorted({n for n, _ in eng.pragmas if n}))
                reasons.append(f"pragma-effect-ignored:{names}")
    elif occ_session:
        reasons.append("occam-only-session:{}".format(",".join(sorted(set(occ_session)))))

    # C-tx
    if sorted(eng.tx) != sorted(unit.get("pre_tx", []) + unit.get("post_tx", [])):
        reasons.append(
            "tx-divergence:engine={},occam={}".format(
                ",".join(sorted(eng.tx)) or "-",
                ",".join(sorted(unit.get("pre_tx", []) + unit.get("post_tx", [])))
                or "-",
            )
        )

    # C-maintenance
    for m in sorted(set(eng.maintenance)):
        reasons.append(f"engine-only-maintenance:{m}")

    return ("agree" if not reasons else "mismatch", sorted(set(reasons)), detail)


def execute_file(con, sql_bytes, units):
    """Replay slices; return per-index (events, error)."""
    out = {}
    for unit in units:
        idx, start, end = unit["index"], unit["start"], unit["end"]
        raw = sql_bytes[start:end]
        try:
            text = raw.decode("utf-8")
        except UnicodeDecodeError:
            out[idx] = ([], "decode-error")
            continue
        if not text.strip():
            out[idx] = ([], None, True)  # empty-slice anomaly marker
            continue
        events = []
        holder = {"events": events}
        con.set_authorizer(
            lambda a, p1, p2, db, src, h=holder: (
                h["events"].append((ACTION.get(a, "CODE%d" % a), p1, p2, db, src)),
                sqlite3.SQLITE_OK,
            )[1]
        )
        try:
            cur = con.execute(text)
            cur.fetchall()
            out[idx] = (events, None)
        except Exception as ex:  # noqa: BLE001 - engine truth is "fails"
            out[idx] = (events, str(ex).strip().split("\n")[0][:300])
    return out


def process_file(occam_bin, path, fk_on=False):
    with open(path, "rb") as f:
        sql_bytes = f.read()
    rel = os.path.relpath(path, ROOT)
    occam_args = [occam_bin, path, "--dump-facts", "--dialect", "sqlite"]
    if fk_on:
        # Mirror the out-of-band PRAGMA below in occam's modeled initial
        # session so cascade-write expansion agrees with the engine.
        occam_args.append("--dump-facts-fk-on")
    proc = run(occam_args)
    if proc.returncode != 0:
        return {
            "file": rel,
            "dump_error": (proc.stderr or "").strip()[-500:],
            "statements": [],
        }
    try:
        dump = json.loads(proc.stdout)
    except json.JSONDecodeError as ex:
        return {"file": rel, "dump_error": f"invalid JSON: {ex}", "statements": []}
    units = dump.get("statements", [])

    schema_objs = set()  # triggers/views for F4 (occam pre-pass, whole file)
    for u in units:
        for d in u.get("ddl", []):
            if d.get("op") == "create-trigger":
                if d.get("name"):
                    schema_objs.add(last(d["name"]))
            elif d.get("op") == "create-view" and d.get("target"):
                schema_objs.add(last(d["target"]))

    con = sqlite3.connect(":memory:", autocommit=True)
    if fk_on:
        con.execute("PRAGMA foreign_keys=ON")
    try:
        ran = execute_file(con, sql_bytes, units)
    finally:
        con.close()

    vtables = set()
    stmts = []
    for u in units:
        idx = u["index"]
        raw = sql_bytes[u["start"] : u["end"]]
        digest = hashlib.sha256(raw).hexdigest()
        try:
            text = raw.decode("utf-8")
        except UnicodeDecodeError:
            text = ""
        preview = preview_of(text)
        pragma_shaped = is_pragma_stmt(text)
        entry = {
            "index": idx,
            "sha256": digest,
            "preview": preview,
            "start": u["start"],
            "end": u["end"],
        }
        events, err, *rest = ran.get(idx, ([], "missing-execution"))
        if rest:  # empty-slice anomaly
            entry.update(
                {
                    "verdict": "anomaly",
                    "reasons": ["empty-slice"],
                    "engine_error": None,
                    "engine": {},
                    "occam": {},
                }
            )
            stmts.append(entry)
            continue
        eng = filter_events(events, vtables, schema_objs, pragma_shaped)
        entry["engine_error"] = err
        barriers = u.get("barriers", [])
        if barriers:
            entry.update(
                {
                    "verdict": "abstain",
                    "reasons": ["fenced:{}".format(b.get("reason")) for b in barriers],
                    "occam_barriers": [
                        (b.get("scope"), b.get("reason")) for b in barriers
                    ],
                }
            )
        elif u.get("parse_error"):
            if err:
                entry.update(
                    {
                        "verdict": "engine-error",
                        "reasons": ["occam-parse-error+engine-error"],
                    }
                )
            else:
                entry.update(
                    {
                        "verdict": "mismatch",
                        "reasons": ["occam-parse-error"],
                        "occam_parse_error": (u.get("parse_error") or "")[:300],
                    }
                )
        elif err:
            reasons = []
            low = err.lower()
            if (
                "incomplete" in low
                or "unrecognized token" in low
                or "only execute one statement" in low
            ):
                reasons.append("possible-segmentation-bug")
            entry.update({"verdict": "engine-error", "reasons": reasons})
        else:
            verdict, reasons, detail = compare(u, eng, pragma_shaped)
            entry.update({"verdict": verdict, "reasons": reasons, "detail": detail})
        if entry["verdict"] == "mismatch":
            entry["occam"] = {
                "reads": [
                    (
                        last(r.get("table")),
                        sorted(c.lower() for c in r.get("columns", [])),
                        r.get("mode"),
                    )
                    for r in u.get("reads", [])
                ],
                "writes": [
                    (
                        last(w.get("target")),
                        w.get("kind"),
                        sorted(
                            
                                c.lower()
                                for c in w.get("columns", [])
                                + w.get("unqualified_columns", [])
                            
                        ),
                    )
                    for w in u.get("writes", [])
                ],
                "ddl": [(d.get("op"), last(d.get("target"))) for d in u.get("ddl", [])],
                "session": u.get("session", []),
                "tx": sorted(u.get("pre_tx", []) + u.get("post_tx", [])),
            }
            entry["engine"] = {
                "reads": {
                    t: (sorted(c) if c is not None else None)
                    for t, c in sorted(eng.reads.items())
                },
                "inserts": sorted(eng.inserts),
                "deletes": sorted(eng.deletes),
                "updates": {
                    t: (sorted(c) if c is not None else None)
                    for t, c in sorted(eng.updates.items())
                },
                "ddl": sorted(f"{op}:{tgt}" for op, tgt, _n in eng.ddl),
                "tx": sorted(eng.tx),
                "pragmas": sorted(f"{n}={v}" for n, v in eng.pragmas),
                "maintenance": sorted(set(eng.maintenance)),
                "expansion": eng.expansion,
                "phantom": eng.phantom,
            }
        else:
            entry["engine_read_tables"] = sorted(eng.reads)
            entry["engine_write_tables"] = sorted(eng.write_tables())
            entry["engine_expansion"] = eng.expansion
            entry["engine_phantom"] = eng.phantom
        stmts.append(entry)
    return {"file": rel, "statements": stmts}


def summarize(files):
    counts = {
        "statements": 0,
        "agree": 0,
        "abstain": 0,
        "engine-error": 0,
        "mismatch": 0,
        "anomaly": 0,
        "files": len(files),
        "dump-errors": 0,
    }
    hist = {}
    for f in files:
        if f.get("dump_error"):
            counts["dump-errors"] += 1
            continue
        for s in f["statements"]:
            counts["statements"] += 1
            counts[s["verdict"]] = counts.get(s["verdict"], 0) + 1
            if s["verdict"] == "mismatch":
                for r in s["reasons"]:
                    key = r.split(":", 1)[0]
                    hist[key] = hist.get(key, 0) + 1
    return counts, hist


def main(argv=None):
    ap = argparse.ArgumentParser(
        description="Cross-validate occam deps vs SQLite authorizer."
    )
    ap.add_argument(
        "--occam", default=None, help="occam binary (default: cabal build + list-bin)"
    )
    ap.add_argument(
        "--corpus",
        action="append",
        default=[],
        help="glob rel. to repo root (repeatable)",
    )
    ap.add_argument(
        "--out",
        default="/tmp/occam_cross_validate_report.json",
        help="machine JSON output",
    )
    ap.add_argument(
        "--fk-on",
        action="store_true",
        help="PRAGMA foreign_keys=ON per fresh DB before replay (lifts R-fk-off)",
    )
    args = ap.parse_args(argv)

    occam_bin = resolve_occam(args.occam)
    patterns = args.corpus or DEFAULT_GLOBS
    files = corpus_files(patterns)
    if not files:
        print("no corpus files matched", file=sys.stderr)
        return 2

    results = [process_file(occam_bin, p, fk_on=args.fk_on) for p in files]
    counts, hist = summarize(results)
    compared = counts.get("agree", 0) + counts.get("mismatch", 0)
    total = counts.get("statements", 0)
    summary = {
        "counts": counts,
        "agreement_pct": round(100.0 * counts.get("agree", 0) / compared, 2)
        if compared
        else None,
        "abstain_pct": round(100.0 * counts.get("abstain", 0) / total, 2)
        if total
        else None,
        "engine_error_pct": round(100.0 * counts.get("engine-error", 0) / total, 2)
        if total
        else None,
        "reason_histogram": hist,
    }
    report = {
        "tool": "cross_validate_deps",
        "version": 1,
        "dialect": "sqlite",
        "sqlite_version": sqlite3.sqlite_version,
        "python": sys.version.split()[0],
        "occam": {
            "binary": os.path.relpath(occam_bin, ROOT)
            if occam_bin.startswith(ROOT)
            else occam_bin,
            "sha256": sha256_file(occam_bin),
        },
        "corpus": patterns,
        "summary": summary,
        "files": results,
    }
    if args.fk_on:
        # Gated so default-mode output stays byte-identical to before.
        report["fk_enforcement"] = True
    out_path = os.path.join(ROOT, args.out)
    with open(out_path, "w") as f:
        json.dump(report, f, indent=2, sort_keys=True)
        f.write("\n")

    # human summary
    print("corpus: {}".format(" ".join(patterns)))
    print("occam:  {}  sha256={:.12}".format(occam_bin, report["occam"]["sha256"]))
    print(f"sqlite: {sqlite3.sqlite_version}  python: {sys.version.split()[0]}")
    if args.fk_on:
        print("fk_enforcement: ON (R-fk-off lifted)")
    print(
        "files=%d statements=%d agree=%d abstain=%d engine-error=%d mismatch=%d anomaly=%d"
        % (
            counts["files"],
            total,
            counts.get("agree", 0),
            counts.get("abstain", 0),
            counts.get("engine-error", 0),
            counts.get("mismatch", 0),
            counts.get("anomaly", 0),
        )
    )
    if compared:
        print(
            "agreement=%.2f%% of %d compared | abstain=%.2f%% | engine-error=%.2f%%"
            % (
                summary["agreement_pct"],
                compared,
                summary["abstain_pct"],
                summary["engine_error_pct"],
            )
        )
    if hist:
        print(
            "mismatch reasons: {}".format(" ".join("%s=%d" % kv for kv in sorted(hist.items())))
        )
    print("--- per-file ---")
    for f in results:
        if f.get("dump_error"):
            print("{} DUMP-ERROR {}".format(f["file"], f["dump_error"][:120]))
            continue
        c = {}
        for s in f["statements"]:
            c[s["verdict"]] = c.get(s["verdict"], 0) + 1
        print(
            "%s stmts=%d %s"
            % (
                f["file"],
                len(f["statements"]),
                " ".join("%s=%d" % kv for kv in sorted(c.items())),
            )
        )
    print("--- mismatches ---")
    for f in results:
        for s in f.get("statements", []):
            if s["verdict"] != "mismatch":
                continue
            print(
                "%s#%d sha256=%.12s %s"
                % (f["file"], s["index"], s["sha256"], s["preview"][:160])
            )
            print("    reasons: {}".format(" ".join(s["reasons"])))
            print("    occam:  {}".format(json.dumps(s["occam"], sort_keys=True)))
            print("    engine: {}".format(json.dumps(s["engine"], sort_keys=True)))
    mode = "fk-on" if args.fk_on else "default"
    failures, pinned, skipped = check_gate(results, mode)
    print(f"--- known gaps ({mode}) ---")
    print("pinned=%d skipped=%d failures=%d" % (pinned, len(skipped), len(failures)))  # noqa: UP031
    for line in failures:
        print(f"GATE-FAIL {line}")
    for line in skipped:
        print(f"GATE-SKIP {line}")
    print(f"wrote {args.out}")
    print("gate: %s" % ("PASS" if not failures else "FAIL"))
    return 0 if not failures else 1


if __name__ == "__main__":
    sys.exit(main())

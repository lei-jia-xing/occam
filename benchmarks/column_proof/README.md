# Column mechanism proof fixtures (separate from primary, never pooled)

Tiny SQLite cases that disentangle column-listing coverage (single) from joint
search (atomic). Each preserves a 2-row rowset under exact ordered equality.

- `explicit`: qualified `t.a` selected with explicit `INSERT` column list;
  column `b` unused (droppable).
- `implicit`: unqualified `a` selected, `INSERT` without column list;
  column `b` unused (droppable).
- `star`: `SELECT *`; no column droppable, star/reference fence (skip reason).

Run throwaway with the frozen A-instrumented `occam-eval` profiles once the
shared build lands (see `benchmarks/tools/run_three_arm.py`); do not pool
with the primary 8-case cohort. Fixture SQL and rowsets are validated by direct
SQLite execution; reduction behavior is observed at run time, never asserted here.

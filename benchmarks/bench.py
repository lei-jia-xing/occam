#!/usr/bin/env python3
"""Tier 2/3/4 unified benchmark runner: fast SQLite oracle, deterministic
corpus staging, AB/BA interleaved reduction runs, and 4-dimension reports."""
import argparse
import hashlib
import json
import shlex
import sqlite3
import subprocess
import sys
from pathlib import Path

BENCH_PY = Path(__file__).resolve()
ROOT = BENCH_PY.parent.parent


def split_statements(sql):
    """Split on complete statements; a trailing fragment stays (fail-closed)."""
    stmts, buf = [], ""
    for line in sql.splitlines(keepends=True):
        buf += line
        if sqlite3.complete_statement(buf):
            if buf.strip():
                stmts.append(buf.strip())
            buf = ""
    if buf.strip():
        stmts.append(buf.strip())
    return stmts


def execute_sql(sql, want_digest=True):
    """Run SQL in a fresh :memory: DB; return (final_rows, digest or None)."""
    con = sqlite3.connect(":memory:", isolation_level=None)
    try:
        con.execute("PRAGMA foreign_keys=ON")
        rows = []
        for stmt in split_statements(sql):
            cur = con.execute(stmt)
            if cur.description is not None:
                rows = [[v.hex() if isinstance(v, bytes) else v for v in r]
                        for r in cur.fetchall()]
        digest = (hashlib.sha256("\n".join(con.iterdump()).encode()).hexdigest()
                  if want_digest else None)
    finally:
        con.close()
    return rows, digest


def oracle_accepts(sql, contract):
    """True iff the rowset matches (and the state digest, when requested)."""
    check_state = contract.get("check_state")
    try:
        rows, digest = execute_sql(sql, want_digest=bool(check_state))
    except sqlite3.Error:
        return False
    if rows != contract.get("expected_rows"):
        return False
    if check_state:
        return digest == contract.get("state_sha256")
    return True


def cmd_oracle(args):
    contract = json.loads(Path(args.contract).read_text())
    return 0 if oracle_accepts(sys.stdin.read(), contract) else 1


def gen_scale(n):
    """Deterministic multi-table case: noise_b + index are pure noise."""
    rows_a = ", ".join(f"({i}, {i % 8}, 'v{i % 13}')" for i in range(1, n + 1))
    rows_b = ", ".join(f"({i}, {(i * 7) % n + 1}, 't{i % 5}')"
                       for i in range(1, n + 1))
    return (
        "CREATE TABLE noise_a(id INTEGER PRIMARY KEY, grp INT, val TEXT);\n"
        f"INSERT INTO noise_a(id, grp, val) VALUES {rows_a};\n"
        "CREATE TABLE noise_b(id INTEGER PRIMARY KEY, a_id INT "
        "REFERENCES noise_a(id), tag TEXT);\n"
        f"INSERT INTO noise_b(id, a_id, tag) VALUES {rows_b};\n"
        "CREATE INDEX idx_noise_b_tag ON noise_b(tag);\n"
        "SELECT count(*), sum(id) FROM noise_a;\n"
    )


def write_contract(sql_path, contract_path):
    rows, digest = execute_sql(sql_path.read_text())
    contract = {"expected_rows": rows, "state_sha256": digest,
                "check_state": False}
    contract_path.write_text(json.dumps(contract, indent=2) + "\n")
    return contract


def qualify(name, sql, contract):
    checks = [(sql, True, "original"), ("", False, "empty"),
              ("SYNTAX ERROR;", False, "invalid")]
    for candidate, want, label in checks:
        if oracle_accepts(candidate, contract) != want:
            raise SystemExit(f"prepare: {name} failed {label} control")


def cmd_prepare(args):
    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    jobs = [(p.stem, p.read_text())
            for p in sorted(Path(args.corpus).glob("*.sql"))]
    jobs += [(f"scale_{n:03d}", gen_scale(n))
             for n in (int(x) for x in args.scales.split(",") if x.strip())]
    cases = []
    for name, sql in jobs:
        (out / f"{name}.sql").write_text(sql)
        contract = write_contract(out / f"{name}.sql",
                                  out / f"{name}.contract.json")
        qualify(name, sql, contract)
        cases.append({"name": name, "sql": f"{name}.sql",
                      "contract": f"{name}.contract.json"})
    (out / "manifest.json").write_text(json.dumps({"cases": cases},
                                                  indent=2) + "\n")
    print(f"prepared {len(cases)} cases in {out}")
    return 0


def parse_arm(spec):
    name, eq, flags = spec.partition("=")
    if not eq or not name:
        raise SystemExit(f"bad --arm {spec!r}, want NAME=FLAGS")
    return name, shlex.split(flags)


def build_schedule(arms, repetitions):
    """AB/BA interleave: odd reps run arms forward, even reps reversed."""
    sched = []
    for rep in range(1, repetitions + 1):
        order = arms if rep % 2 else list(reversed(arms))
        sched.extend((rep, arm) for arm in order)
    return sched


def cmd_run(args):
    manifest = json.loads(Path(args.manifest).read_text())
    mdir = Path(args.manifest).parent
    arms = ([parse_arm(s) for s in args.arm] if args.arm
            else [("default", []), ("intra", ["--intra-statement"])])
    flags = dict(arms)
    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    for rep, arm in build_schedule([n for n, _ in arms], args.repetitions):
        for case in manifest["cases"]:
            sql = str((mdir / case["sql"]).resolve())
            contract = str((mdir / case["contract"]).resolve())
            rundir = str(out / f"{case['name']}__{arm}__r{rep}")
            oracle = f"python3 {BENCH_PY} oracle {contract}"
            cmd = [args.occam, sql, "--oracle", oracle, "--dialect", "sqlite",
                   "--timeout", str(args.timeout), "--budget", str(args.budget),
                   "--out", rundir, "--quiet", *flags[arm]]
            print(f"[{arm} r{rep}] {case['name']}", flush=True)
            try:
                subprocess.run(cmd, check=False, timeout=args.budget + 60)
            except subprocess.TimeoutExpired:
                print(f"[{arm} r{rep}] {case['name']}: TIMEOUT", flush=True)
    return 0


def build_crash_cmd(meta_path, occam="occam", out=None):
    """Exact occam command for a Tier 4 .meta.json case."""
    meta = json.loads(Path(meta_path).read_text())
    sql = str((Path(meta_path).parent / meta["file"]).resolve())
    dbms = meta["target"]["dbms"]
    sig = meta["crash_signature"]["expected_signature"]
    oracle = (f"python3 {ROOT}/harness/crash_oracle.py --dbms {dbms} "
              f"--expected-signature {shlex.quote(sig)}")
    cmd = [occam, sql, "--oracle", oracle, "--reset",
           f"{ROOT}/harness/reset_db.sh --dbms {dbms}",
           "--dialect", meta.get("dialect", dbms)]
    if out:
        cmd += ["--out", out]
    return cmd


def cmd_crash_cmd(args):
    cmd = build_crash_cmd(args.meta, args.occam, args.out)
    print(shlex.join(cmd))
    if args.run:
        subprocess.run(cmd, check=False)
    return 0


def compute_attempt_auc(initial_bytes, curve, total_attempts):
    """Normalized area of best_bytes/initial over 1..total; lower is faster."""
    if initial_bytes <= 0 or total_attempts <= 0:
        return 1.0
    best_at = {}
    for p in curve or []:
        i, b = p.get("pointCallIndex"), p.get("pointCandidateBytes")
        if p.get("pointAccepted") and isinstance(i, int) and isinstance(b, int):
            best_at[i] = min(b, best_at.get(i, b))
    best, area = initial_bytes, 0.0
    for i in range(1, total_attempts + 1):
        best = min(best, best_at.get(i, best))
        area += best / initial_bytes
    return area / total_attempts


def engine_overhead_us(stats):
    """Parse FFI + decode + candidate-build micros from cost attribution."""
    cost = (stats.get("evaluation_metrics", {})
            .get("evaluationCostAttribution") or {})
    total = sum((cost.get("snapshotParseFfiMicros") or {}).values())
    total += sum((cost.get("snapshotParseDecodeMicros") or {}).values())
    for fam in cost.get("snapshotFamilies") or []:
        counters = (fam or {}).get("counters") or {}
        total += counters.get("candidateBuildMicros", 0) or 0
    return total


def summarize_run(stats):
    m = stats.get("evaluation_metrics", {})
    curve = m.get("evaluationCurve") or []
    peak = max([p.get("pointCallIndex", 0) or 0 for p in curve] + [0])
    total = max(stats.get("oracle_attempts") or 0, peak)
    return {
        "reduced_bytes": stats.get("reduced_bytes"),
        "reduced_statements": stats.get("reduced_statements"),
        "reduction_ratio": stats.get("reduction_ratio"),
        "oracle_attempts": stats.get("oracle_attempts"),
        "accepted": m.get("evaluationAcceptedObservations"),
        "cache_hits": stats.get("cache_hits"),
        "attempt_auc": compute_attempt_auc(stats.get("initial_bytes") or 0,
                                           curve, total),
        "engine_overhead_us": engine_overhead_us(stats),
        "elapsed_seconds": m.get("evaluationReductionSeconds",
                                 stats.get("elapsed_seconds")),
        "status": stats.get("status"),
    }


COLUMNS = ["run", "reduced_bytes", "reduced_statements", "reduction_ratio",
           "oracle_attempts", "accepted", "cache_hits", "attempt_auc",
           "engine_overhead_us", "elapsed_seconds", "status"]


def cmd_report(args):
    rundir = Path(args.run_dir)
    runs = []
    for child in sorted(rundir.iterdir()):
        if not child.is_dir():
            continue
        rec = {"run": child.name}
        parts = child.name.split("__")
        if len(parts) == 3:
            rec["case"], rec["arm"], rec["rep"] = parts
        stats = child / "stats.json"
        rec.update(summarize_run(json.loads(stats.read_text()))
                   if stats.exists() else {"status": "missing-stats"})
        runs.append(rec)
    (rundir / "summary.json").write_text(json.dumps({"runs": runs},
                                                    indent=2) + "\n")
    lines = ["# Benchmark report", "",
             "| " + " | ".join(COLUMNS) + " |",
             "|" + "|".join(["---"] * len(COLUMNS)) + "|"]
    for r in runs:
        cells = [f"{r.get(c):.4f}" if isinstance(r.get(c), float)
                 else str(r.get(c)) for c in COLUMNS]
        lines.append("| " + " | ".join(cells) + " |")
    (rundir / "report.md").write_text("\n".join(lines) + "\n")
    print(f"reported {len(runs)} runs in {rundir}")
    return 0


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__)
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("oracle", help="Tier 2 SQLite oracle (SQL on stdin)")
    p.add_argument("contract")
    p = sub.add_parser("prepare", help="stage corpus + scale cases")
    p.add_argument("--out", required=True)
    p.add_argument("--corpus", default="benchmarks/corpus")
    p.add_argument("--scales", default="64,256")
    p = sub.add_parser("run", help="AB/BA interleaved reduction runs")
    p.add_argument("--manifest", required=True)
    p.add_argument("--out", required=True)
    p.add_argument("--occam", required=True)
    p.add_argument("--arm", action="append", default=[])
    p.add_argument("--repetitions", type=int, default=2)
    p.add_argument("--budget", type=float, default=30)
    p.add_argument("--timeout", type=float, default=2)
    p = sub.add_parser("crash-cmd", help="occam command for a Tier 4 case")
    p.add_argument("--meta", required=True)
    p.add_argument("--occam", default="occam")
    p.add_argument("--out", default=None)
    p.add_argument("--run", action="store_true")
    p = sub.add_parser("report", help="4-dimension report over a run dir")
    p.add_argument("run_dir")
    args = ap.parse_args(argv)
    return {"oracle": cmd_oracle, "prepare": cmd_prepare, "run": cmd_run,
            "crash-cmd": cmd_crash_cmd, "report": cmd_report}[args.cmd](args)


if __name__ == "__main__":
    sys.exit(main())

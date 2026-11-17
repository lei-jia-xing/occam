#!/usr/bin/env python3
"""Freeze one instrumented build: binaries + all compile inputs + identity.

Run once per build under test, AFTER build proof and BEFORE any further
source edits. Refuses to overwrite; chmod a-w at the end. Emits LAUNCH.json
with the exact triad/mechanism commands for the frozen paths. After freezing,
do not edit helpers until cohorts complete; if unavoidable, record a
supplement bundle, never mutate the snapshot.
"""
from __future__ import annotations

import argparse
import hashlib
import importlib.metadata
import json
import os
import platform
import shutil
import sqlite3
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

HS_DIRS = ("src", "app", "bench", "test")
TOP_FILES = ("occam.cabal", "cabal.project", "Setup.hs")
RUST_TOP = ("rust/sqlparser-ffi/Cargo.toml", "rust/sqlparser-ffi/Cargo.lock")
OPTIONAL_TOP = ("cabal.project.local",)
PROTOCOL_HELPERS = (
    "benchmarks/tools/run_three_arm.py",
    "benchmarks/tools/compare_reducers.py",
    "benchmarks/tools/scale_study.py",
    "benchmarks/fuzz_corpus/run_docker_benchmark.py",
    "benchmarks/upstream/oracle_exec.py",
    "benchmarks/upstream/oracle_test.py",
    "benchmarks/upstream/sqlancer/PredicateReducer.java",
    "benchmarks/upstream/sqlreduce_adapter.py",
)
PRODUCTION_SOURCES = (
    "src/Occam/Reducer.hs",
    "src/Occam/Parser.hs",
    "src/Occam/Pool.hs",
    "src/Occam/Evaluation.hs",
    "src/Occam/Metrics.hs",
    "src/Occam/Facts.hs",
    "rust/sqlparser-ffi/src/edits.rs",
    "rust/sqlparser-ffi/src/ir.rs",
    "rust/sqlparser-ffi/src/parser.rs",
    "rust/sqlparser-ffi/src/lib.rs",
    "rust/sqlparser-ffi/src/ffi.rs",
    "rust/sqlparser-ffi/src/actions.rs",
    "rust/sqlparser-ffi/src/source.rs",
    "rust/sqlparser-ffi/Cargo.toml",
    "rust/sqlparser-ffi/Cargo.lock",
)


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def mtime(path):
    return time.strftime("%Y-%m-%d %H:%M:%S %z", time.localtime(Path(path).stat().st_mtime))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--occam", type=Path, required=True)
    parser.add_argument("--occam-eval", type=Path, required=True)
    parser.add_argument("--label", required=True)
    parser.add_argument("--cases-from", type=Path, default=ROOT / "benchmarks/benchmark_artifacts/external-comparison-baseline-20261116-a")
    parser.add_argument("--venv-python", default="/tmp/occam-external-compare-venv/bin/python")
    args = parser.parse_args()
    out = args.out.resolve()
    if out.exists() and any(out.iterdir()):
        parser.error("choose a new empty output directory; freezes are never overwritten")
    for key in ("occam", "occam_eval"):
        binary = getattr(args, key).resolve()
        if not binary.is_file() or not os.access(binary, os.X_OK):
            parser.error(f"--{key.replace('_', '-')} must be an executable file: {binary}")
        setattr(args, key, binary)
    out.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(args.occam, out / "occam")
    shutil.copyfile(args.occam_eval, out / "occam-eval")
    os.chmod(out / "occam", 0o755)
    os.chmod(out / "occam-eval", 0o755)
    sources = out / "sources"
    count = 0
    for subdir in HS_DIRS:
        for path in sorted((ROOT / subdir).rglob("*.hs")):
            target = sources / path.relative_to(ROOT)
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(path, target)
            count += 1
    rust = ROOT / "rust/sqlparser-ffi"
    for path in sorted((rust / "src").rglob("*.rs")):
        target = sources / path.relative_to(ROOT)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(path, target)
        count += 1
    for name in (*TOP_FILES, *RUST_TOP, *PROTOCOL_HELPERS):
        path = ROOT / name
        if not path.is_file():
            raise SystemExit(f"missing compile/helper input: {name}")
        target = sources / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(path, target)
        count += 1
    for name in OPTIONAL_TOP:
        path = ROOT / name
        if path.is_file():
            target = sources / name
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(path, target)
            count += 1
    hashes = {}
    for path in sorted((out / "sources").rglob("*")):
        if path.is_file():
            hashes[str(path.relative_to(out / "sources"))] = sha(path)
    with (out / "SOURCES.sha256").open("w", encoding="utf-8") as output:
        for rel, digest in hashes.items():
            output.write(f"{digest}  {rel}\n")
    staticlibs = {}
    for path in sorted(ROOT.glob("dist-newstyle/**/libCoccam_sqlparser.a")):
        staticlibs[str(path.relative_to(ROOT))] = {"sha256": sha(path), "bytes": path.stat().st_size}
    sqlancer_src = "/home/hunter/.cache/occam/sqlancer-a7d60ee450e6a22ca38599c14e984a49773f4745"
    sqlreduce_src = "/home/hunter/.cache/occam/sqlreduce-568c8a9a31601ead7128d0b067658b4d5a26821d"
    rev = lambda src: subprocess.run(["git", "-C", src, "rev-parse", "HEAD"], capture_output=True, text=True, check=True).stdout.strip()
    creduce = "/nix/store/vadhs8h6l47c73zv8gkwklkgs8299zw2-creduce-2.10.0-unstable-2024-06-01/bin/creduce"
    ldd = subprocess.run(["ldd", str(out / "occam")], capture_output=True, text=True).stdout
    non_vdso = [line for line in ldd.splitlines() if "vdso" not in line]
    venv = Path(args.venv_python)
    identity = {
        "bundle": str(out),
        "label": args.label,
        "created_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "vcs": "not audited by this workstream",
        "occam": {"sha256": sha(out / "occam"), "bytes": (out / "occam").stat().st_size, "dist_mtime": mtime(args.occam), "dist_path": str(args.occam)},
        "occam_eval": {"sha256": sha(out / "occam-eval"), "bytes": (out / "occam-eval").stat().st_size, "dist_mtime": mtime(args.occam_eval), "dist_path": str(args.occam_eval)},
        "staticlibs": staticlibs,
        "source_count": count,
        "sources_sha256": sha(out / "SOURCES.sha256"),
        "protocol_helpers": {name: hashes[name] for name in PROTOCOL_HELPERS},
        "upstream": {
            "sqlancer": {"pin": "a7d60ee450e6a22ca38599c14e984a49773f4745", "rev": rev(sqlancer_src)},
            "sqlreduce": {"pin": "568c8a9a31601ead7128d0b067658b4d5a26821d", "rev": rev(sqlreduce_src)},
        },
        "creduce": {"binary": creduce, "sha256": sha(creduce)} if Path(creduce).is_file() else None,
        "venv_python": str(venv),
        "python": sys.version,
        "sqlite": sqlite3.sqlite_version,
        "platform": platform.platform(),
        "python_packages": {name: importlib.metadata.version(name) for name in ("pglast", "psycopg2-binary", "PyYAML")},
        "linkage": {"ldd_lines": len(non_vdso) + 1, "nix_only": all("/nix/store" in line or "=>" not in line for line in non_vdso), "repo_refs": sum("/home/hunter/Workspace/occam" in line for line in non_vdso)},
    }
    (out / "IDENTITY.json").write_text(json.dumps(identity, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    triad_argv = [
        str(venv), "benchmarks/tools/run_three_arm.py", "--out", "<TRIAD_OUT>",
        "--occam-eval", str(out / "occam-eval"), "--cases-from", str(args.cases_from.resolve()),
        "--budget", "20", "--timeout", "1", "--repetitions", "2", "--diag",
        *[flag for src in PRODUCTION_SOURCES for flag in ("--production-source", src)],
    ]
    mechanism_argv = [
        str(venv), "benchmarks/tools/run_three_arm.py", "--out", "<MECH_OUT>",
        "--occam-eval", str(out / "occam-eval"), "--cases-from", str(ROOT / "benchmarks/column_proof"),
        "--expect-cases", "3", "--budget", "120", "--timeout", "5", "--repetitions", "1", "--diag",
        *[flag for src in PRODUCTION_SOURCES for flag in ("--production-source", src)],
    ]
    (out / "LAUNCH.json").write_text(json.dumps({
        "triad_argv": triad_argv,
        "mechanism_argv": mechanism_argv,
        "cwd": str(ROOT),
        "notes": "substitute dated out dirs at launch; triad expects 48 children, mechanism 9; score runs require live helper bytes identical to protocol_helpers",
    }, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    subprocess.run(["chmod", "-R", "a-w", str(out)], check=True)
    print(json.dumps({"bundle": str(out), "occam": identity["occam"]["sha256"][:16], "occam_eval": identity["occam_eval"]["sha256"][:16], "sources": count}, indent=2))


if __name__ == "__main__":
    main()

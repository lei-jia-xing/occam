#!/usr/bin/env python3
"""Deterministic CREATE/INSERT-pair series for engine-mechanics profiling.

Emits independent-table pairs (no FKs, no shared state) at fixed sizes under a
fixed seed. Validity (executes clean in SQLite) and byte-determinism (two
generations identical) are asserted during the run; MANIFEST.json records
argv, seed, sizes, and input SHAs.
"""

import argparse
import hashlib
import json
import random
import sqlite3
import string
import sys
from contextlib import closing
from pathlib import Path


def generate(seed, pairs):
    rng = random.Random(seed)
    files = {}
    for count in pairs:
        chunks = []
        for index in range(1, count + 1):
            table = f"t_{index:04d}"
            values = []
            for column in range(4):
                if column % 2 == 0:
                    values.append(str(rng.randint(1, 999999)))
                else:
                    word = "".join(rng.choice(string.ascii_letters) for _ in range(rng.randint(1, 12)))
                    values.append(f"'{word}'")
            chunks.append(f"CREATE TABLE {table} (a INT, b TEXT, c INT, d TEXT);")
            chunks.append(f"INSERT INTO {table} (a, b, c, d) VALUES ({', '.join(values)});")
        files[f"pairs-{count:03d}.sql"] = "\n".join(chunks) + "\n"
    return files


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--seed", type=int, default=20261117)
    parser.add_argument("--pairs", default="4,16,64,256")
    args = parser.parse_args()
    pairs = [int(piece) for piece in args.pairs.split(",")]
    if not pairs or any(count < 1 for count in pairs):
        parser.error("--pairs must be a non-empty list of positive integers")
    out = args.out.resolve()
    if out.exists() and any(out.iterdir()):
        parser.error("choose a new empty output directory; inputs are never overwritten")
    first = generate(args.seed, pairs)
    second = generate(args.seed, pairs)
    if first != second:
        raise SystemExit("generator is not deterministic for this seed")
    out.mkdir(parents=True, exist_ok=True)
    manifest = {"generator_argv": sys.argv, "seed": args.seed, "python": sys.version, "files": {}}
    for name, sql in sorted(first.items()):
        with closing(sqlite3.connect(":memory:", isolation_level=None)) as connection:
            connection.execute("PRAGMA foreign_keys=ON")
            try:
                connection.executescript(sql)
            except sqlite3.Error as exc:
                raise SystemExit(f"generated invalid SQL for {name}: {exc}")
        (out / name).write_text(sql, encoding="utf-8")
        manifest["files"][name] = {
            "sha256": hashlib.sha256(sql.encode("utf-8")).hexdigest(),
            "bytes": len(sql.encode("utf-8")),
            "statements": sql.count(";"),
        }
        print(f"{name}: {manifest['files'][name]['statements']} statements, {manifest['files'][name]['bytes']} bytes", flush=True)
    (out / "MANIFEST.json").write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"wrote {len(first)} inputs + MANIFEST.json to {out}")


if __name__ == "__main__":
    main()

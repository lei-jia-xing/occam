#!/usr/bin/env python3
"""Audit reduced corpus SQL with the repository's Tree-sitter SQL grammar."""

import argparse
import ctypes
import ctypes.util
import json
import re
import subprocess
import tempfile
from pathlib import Path


class TSNode(ctypes.Structure):
    _fields_ = [("context", ctypes.c_uint32 * 4), ("id", ctypes.c_void_p), ("tree", ctypes.c_void_p)]


def make_parser(root, temp_dir):
    runtime_path = ctypes.util.find_library("tree-sitter")
    if not runtime_path:
        raise RuntimeError("libtree-sitter is required (the same dependency as Occam)")
    runtime = ctypes.CDLL(runtime_path, mode=ctypes.RTLD_GLOBAL)
    grammar_dir = root / "cbits" / "tree-sitter-sql"
    grammar_path = Path(temp_dir) / "liboccam_sql.so"
    subprocess.run(
        [
            "cc", "-shared", "-fPIC", f"-I{grammar_dir}",
            str(grammar_dir / "parser.c"), str(grammar_dir / "scanner.c"),
            "-ltree-sitter", "-o", str(grammar_path),
        ],
        check=True,
        capture_output=True,
        text=True,
    )
    grammar = ctypes.CDLL(str(grammar_path))
    grammar.tree_sitter_sql.restype = ctypes.c_void_p
    runtime.ts_parser_new.restype = ctypes.c_void_p
    runtime.ts_parser_delete.argtypes = [ctypes.c_void_p]
    runtime.ts_parser_set_language.argtypes = [ctypes.c_void_p, ctypes.c_void_p]
    runtime.ts_parser_set_language.restype = ctypes.c_bool
    runtime.ts_parser_parse_string.argtypes = [ctypes.c_void_p, ctypes.c_void_p, ctypes.c_char_p, ctypes.c_uint32]
    runtime.ts_parser_parse_string.restype = ctypes.c_void_p
    runtime.ts_tree_delete.argtypes = [ctypes.c_void_p]
    runtime.ts_tree_root_node.argtypes = [ctypes.c_void_p]
    runtime.ts_tree_root_node.restype = TSNode
    runtime.ts_node_type.argtypes = [TSNode]
    runtime.ts_node_type.restype = ctypes.c_char_p
    runtime.ts_node_named_child_count.argtypes = [TSNode]
    runtime.ts_node_named_child_count.restype = ctypes.c_uint32
    runtime.ts_node_named_child.argtypes = [TSNode, ctypes.c_uint32]
    runtime.ts_node_named_child.restype = TSNode
    runtime.ts_node_start_byte.argtypes = [TSNode]
    runtime.ts_node_start_byte.restype = ctypes.c_uint32
    runtime.ts_node_end_byte.argtypes = [TSNode]
    runtime.ts_node_end_byte.restype = ctypes.c_uint32
    runtime.ts_node_has_error.argtypes = [TSNode]
    runtime.ts_node_has_error.restype = ctypes.c_bool
    parser = runtime.ts_parser_new()
    if not runtime.ts_parser_set_language(parser, grammar.tree_sitter_sql()):
        raise RuntimeError("Tree-sitter SQL grammar is incompatible with libtree-sitter")
    return runtime, grammar, parser


def parse(runtime, parser, sql):
    tree = runtime.ts_parser_parse_string(parser, None, sql, len(sql))
    if not tree:
        raise RuntimeError("Tree-sitter failed to parse SQL")
    return tree, runtime.ts_tree_root_node(tree)


def node_type(runtime, node):
    return runtime.ts_node_type(node).decode("utf-8")


def children(runtime, node):
    return [
        runtime.ts_node_named_child(node, i)
        for i in range(runtime.ts_node_named_child_count(node))
    ]


def descendants(runtime, node):
    yield node
    for child in children(runtime, node):
        yield from descendants(runtime, child)


def node_text(runtime, node, source):
    start = runtime.ts_node_start_byte(node)
    end = runtime.ts_node_end_byte(node)
    return source[start:end].decode("utf-8", errors="replace")


def normalize_name(value):
    value = value.strip().split(".")[-1].strip()
    return value.strip('"`[]\'').lower()


def cte_names(runtime, root, source):
    names = set()
    for node in descendants(runtime, root):
        if node_type(runtime, node) == "cte":
            named = children(runtime, node)
            if named:
                names.add(normalize_name(node_text(runtime, named[0], source)))
    return names


def relation_aliases(runtime, root, source):
    aliases = set()
    for node in descendants(runtime, root):
        if node_type(runtime, node) != "relation":
            continue
        for child in children(runtime, node):
            if node_type(runtime, child) == "identifier":
                aliases.add(normalize_name(node_text(runtime, child, source)))
    return aliases


SQL_KEYWORDS = {"lateral", "generate_series", "unnest", "dual", "generate_subscripts"}


def table_references(runtime, root, source):
    refs = set()
    aliases = relation_aliases(runtime, root, source)
    ctes = cte_names(runtime, root, source)
    for node in descendants(runtime, root):
        kind = node_type(runtime, node)
        direct = children(runtime, node)
        if kind == "relation":
            candidates = [child for child in direct if node_type(runtime, child) == "object_reference"]
        elif kind in {"from", "insert", "replace"}:
            candidates = [child for child in direct if node_type(runtime, child) == "object_reference"]
        else:
            continue
        refs.update(
            name for child in candidates
            if (name := normalize_name(node_text(runtime, child, source)))
            and name not in aliases
            and name not in ctes
            and name not in SQL_KEYWORDS
        )
    return refs

def defined_tables(runtime, root, source):
    names = set()
    for node in descendants(runtime, root):
        if node_type(runtime, node) == "create_table":
            for child in children(runtime, node):
                if node_type(runtime, child) == "object_reference":
                    name = normalize_name(node_text(runtime, child, source))
                    if name:
                        names.add(name)
                    break
    return names


def insert_targets(runtime, root, source):
    names = set()
    for node in descendants(runtime, root):
        if node_type(runtime, node) in {"insert", "replace"}:
            for child in children(runtime, node):
                if node_type(runtime, child) == "object_reference":
                    name = normalize_name(node_text(runtime, child, source))
                    if name:
                        names.add(name)
                    break
    return names


def auxiliary_tables(original):
    section = original.split("-- Auxiliary Setup Tables", 1)
    if len(section) != 2:
        return set()
    section = section[1].split("-- Target Reproduction Scenario", 1)[0]
    pattern = r'CREATE\s+(?:TEMP(?:ORARY)?\s+)?TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?([\w.`\[\]"]+)'
    return {normalize_name(name) for name in re.findall(pattern, section, re.IGNORECASE)}


def uncommented(sql):
    return "\n".join(line for line in sql.splitlines() if not line.lstrip().startswith("--"))


def audit_case(runtime, parser, case, original_path, minimal_path, metadata):
    original = original_path.read_bytes()
    minimal = minimal_path.read_bytes()
    original_tree, original_root = parse(runtime, parser, original)
    minimal_tree, minimal_root = parse(runtime, parser, minimal)
    try:
        refs = table_references(runtime, minimal_root, minimal)
        defined = defined_tables(runtime, minimal_root, minimal)
        target_present = metadata["target_table"].lower() in defined
        trigger_present = metadata["trigger_pattern"].lower() in uncommented(minimal.decode("utf-8", errors="replace")).lower()
        required_aux = auxiliary_tables(original.decode("utf-8", errors="replace")) - {metadata["target_table"].lower()}
        unpruned_aux = required_aux & defined
        dangling = refs - defined
        missing_data = set()
        parse_errors = 0
        passed = not (dangling or missing_data or not target_present or not trigger_present or unpruned_aux or parse_errors)
        return {
            "case": case,
            "status": "PASS" if passed else "FAIL",
            "referenced_tables": sorted(refs),
            "defined_tables": sorted(defined),
            "dangling_references": sorted(dangling),
            "missing_setup_data": sorted(missing_data),
            "target_table_present": target_present,
            "crashing_query_present": trigger_present,
            "unpruned_auxiliary_tables": sorted(unpruned_aux),
            "parse_errors": parse_errors,
        }
    finally:
        runtime.ts_tree_delete(original_tree)
        runtime.ts_tree_delete(minimal_tree)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--format", choices=("table", "json"), default="table")
    ap.add_argument("--json", action="store_true", help="emit JSON instead of the case table")
    args = ap.parse_args()
    root = Path(__file__).resolve().parents[2]
    corpus = root / "benchmarks" / "corpus"
    results = corpus / "results"
    cases = json.loads((corpus / "index.json").read_text(encoding="utf-8"))
    metadata = {entry["id"]: entry for entry in cases}
    with tempfile.TemporaryDirectory(prefix="occam-corpus-audit-") as temp_dir:
        runtime, grammar, parser = make_parser(root, temp_dir)
        try:
            report = []
            for entry in cases:
                case = entry["id"]
                filename = Path(entry["file"]).name
                original_path = corpus / entry["file"]
                minimal_path = results / Path(filename).stem / "minimal.sql"
                report.append(audit_case(runtime, parser, case, original_path, minimal_path, metadata[case]))
            if args.json or args.format == "json":
                print(json.dumps(report, indent=2))
            else:
                print("| Case | Result | Referenced tables | Dangling | Missing setup data | Target | Query | Aux pruned | Parse errors |")
                print("|---|---:|---|---|---|---:|---:|---:|---:|")
                for row in report:
                    print(
                        f"| {row['case']} | {row['status']} | {', '.join(row['referenced_tables'])} | "
                        f"{', '.join(row['dangling_references']) or '—'} | "
                        f"{', '.join(row['missing_setup_data']) or '—'} | "
                        f"{'yes' if row['target_table_present'] else 'no'} | "
                        f"{'yes' if row['crashing_query_present'] else 'no'} | "
                        f"{'yes' if not row['unpruned_auxiliary_tables'] else ', '.join(row['unpruned_auxiliary_tables'])} | "
                        f"{row['parse_errors']} |"
                    )
                print(f"\n{sum(row['status'] == 'PASS' for row in report)}/{len(report)} cases passed.")
        finally:
            runtime.ts_parser_delete(parser)


if __name__ == "__main__":
    main()

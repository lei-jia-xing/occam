#!/usr/bin/env python3
"""
dhall_config.py - Load Occam's Dhall-compiled harness and experiment configs.

Reads deterministic JSON generated from `config/dhall/` (`config/generated/harness.json`,
or compiles `.dhall` on demand when `dhall-to-json` is available) without requiring
non-stdlib Python dependencies at runtime.
"""

import json
import os
import shutil
import subprocess
import sys

HARNESS_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(HARNESS_DIR)
DEFAULT_HARNESS_JSON = os.path.join(REPO_ROOT, "config", "generated", "harness.json")


def _locate_harness_json():
    override = os.environ.get("OCCAM_HARNESS_JSON") or os.environ.get("OCCAM_HARNESS_CONFIG")
    if override:
        return override
    candidates = [
        DEFAULT_HARNESS_JSON,
        os.path.join(HARNESS_DIR, "harness.json"),
        "/usr/local/share/occam/config/generated/harness.json",
    ]
    for cand in candidates:
        if cand and os.path.isfile(cand):
            return cand
    raise FileNotFoundError(
        "Could not locate Dhall-compiled harness.json (run 'make dhall-gen')"
    )


def load_json_or_dhall(path_or_name, kind="plans"):
    """Load a JSON config file, `.dhall` file (via dhall-to-json), or built-in named plan."""
    cand = str(path_or_name).strip()
    if not cand:
        raise ValueError("Empty config path or plan name")

    resolved = None
    if os.path.isfile(cand):
        resolved = cand
    else:
        plan_json = os.path.join(REPO_ROOT, "config", "generated", kind, f"{cand}.json")
        plan_dhall = os.path.join(REPO_ROOT, "config", "dhall", kind, f"{cand}.dhall")
        if os.path.isfile(plan_json):
            resolved = plan_json
        elif os.path.isfile(plan_dhall):
            resolved = plan_dhall

    if resolved is None:
        raise FileNotFoundError(f"Config '{path_or_name}' not found")

    if resolved.endswith(".dhall"):
        exe = shutil.which("dhall-to-json")
        if not exe:
            raise RuntimeError(
                f"Cannot load '{resolved}' directly without 'dhall-to-json' on PATH "
                "(compile with 'make dhall-gen' or pass the .json file)"
            )
        proc = subprocess.run(
            [exe, "--file", resolved],
            capture_output=True,
            text=True,
            check=False,
        )
        if proc.returncode != 0:
            raise RuntimeError(
                f"dhall-to-json failed on '{resolved}': {proc.stderr.strip()}"
            )
        return json.loads(proc.stdout)

    with open(resolved, encoding="utf-8") as handle:
        return json.load(handle)


def load_harness_config():
    path = _locate_harness_json()
    with open(path, encoding="utf-8") as handle:
        return json.load(handle)


HARNESS_CONFIG = load_harness_config()
DBMS_SPECS = {entry["id"]: entry for entry in HARNESS_CONFIG["dbms"]}
SUPPORTED_DBMS = tuple(entry["id"] for entry in HARNESS_CONFIG["dbms"])
SERVER_DBMS = {
    entry["id"] for entry in HARNESS_CONFIG["dbms"] if entry.get("server_based")
}
EMBEDDED_DBMS = {
    entry["id"] for entry in HARNESS_CONFIG["dbms"] if not entry.get("server_based")
}
DBMS_TO_OCCAM_DIALECT = {
    entry["id"]: entry["occam_dialect"] for entry in HARNESS_CONFIG["dbms"]
}
DEFAULT_IMAGES = {
    entry["id"]: entry["container_image"] for entry in HARNESS_CONFIG["dbms"]
}
DEFAULT_CONTAINER_PORTS = {
    entry["id"]: int(entry["default_port"])
    for entry in HARNESS_CONFIG["dbms"]
    if entry.get("default_port") is not None
}
CONTAINER_LOGS = {
    entry["id"]: entry["container_log"]
    for entry in HARNESS_CONFIG["dbms"]
    if entry.get("container_log")
}
BUILTIN_PROFILES = {
    entry["key"]: (entry["name"], list(entry["cli_flags"]))
    for entry in HARNESS_CONFIG["profiles"]
}
ORACLE_DEFAULTS = dict(HARNESS_CONFIG["oracle"])
EXPERIMENT_PLANS = dict(HARNESS_CONFIG["plans"])
SQUIRREL_PROFILES = {
    entry["dbms"]: entry for entry in HARNESS_CONFIG["squirrel"]
}


def load_experiment_plan(plan_ref):
    key = str(plan_ref).strip()
    if key in EXPERIMENT_PLANS:
        return dict(EXPERIMENT_PLANS[key])
    normalized = key.replace("-", "_")
    if normalized in EXPERIMENT_PLANS:
        return dict(EXPERIMENT_PLANS[normalized])
    return load_json_or_dhall(key, kind="plans")


def resolve_repo_binary(binary_name, env_var, explicit_path=""):
    if explicit_path:
        path = os.path.abspath(os.path.expanduser(explicit_path))
        if not os.path.isfile(path) or not os.access(path, os.X_OK):
            raise FileNotFoundError(f"{binary_name} binary is not executable: {path}")
        return path

    env_bin = os.environ.get(env_var, "").strip()
    if env_bin:
        path = os.path.abspath(os.path.expanduser(env_bin))
        if os.path.isfile(path) and os.access(path, os.X_OK):
            return path

    which_bin = shutil.which(binary_name)
    if which_bin:
        return which_bin

    dist_dir = os.path.join(REPO_ROOT, "dist-newstyle")
    if os.path.isdir(dist_dir):
        matches = []
        for dirpath, _, filenames in os.walk(dist_dir):
            if binary_name in filenames and os.path.basename(dirpath) == binary_name:
                cand = os.path.join(dirpath, binary_name)
                if os.path.isfile(cand) and os.access(cand, os.X_OK):
                    matches.append((os.path.getmtime(cand), cand))
        if matches:
            matches.sort(reverse=True)
            return matches[0][1]

    raise FileNotFoundError(
        f"Could not locate '{binary_name}' binary. Set {env_var} or build with 'cabal build all'."
    )


def _yaml_quote(value):
    escaped = str(value).replace("'", "''")
    return f"'{escaped}'"


def _yaml_list(items):
    return "[" + ", ".join(json.dumps(item, ensure_ascii=False) for item in items) + "]"


def render_squirrel_yaml(
    dbms,
    squirrel_home,
    host="",
    port=0,
    user="",
    password="",
    socket="",
    db_name="",
    server_cmd="",
):
    if dbms not in SQUIRREL_PROFILES:
        raise ValueError(f"Unsupported SQUIRREL DBMS '{dbms}'")
    spec = SQUIRREL_PROFILES[dbms]
    lines = [
        "---",
        f"required: {_yaml_list(spec['required'])}",
        f"should_exist: {_yaml_list(spec['should_exist'])}",
        f"init_lib: {_yaml_quote(os.path.join(squirrel_home, spec['init_lib_rel']))}",
    ]
    if spec.get("data_lib_rel"):
        lines.append(
            f"data_lib: {_yaml_quote(os.path.join(squirrel_home, spec['data_lib_rel']))}"
        )
    if spec.get("pragma_rel"):
        lines.append(
            f"pragma: {_yaml_quote(os.path.join(squirrel_home, spec['pragma_rel']))}"
        )
    lines.append(f"db: {spec['db']}")

    if spec["db"] == "postgresql":
        lines.extend(
            [
                f"passwd: {_yaml_quote(password)}",
                f"user_name: {_yaml_quote(user)}",
                f"db_name: {_yaml_quote(db_name)}",
                f"host: {_yaml_quote(host)}",
                f"port: {int(port)}",
                f"startup_cmd: {_yaml_quote(server_cmd)}",
            ]
        )
    elif spec["db"] == "mysql":
        lines.extend(
            [
                f"passwd: {_yaml_quote(password)}",
                f"user_name: {_yaml_quote(user)}",
                f"host: {_yaml_quote(host)}",
                f"port: {int(port)}",
                f"sock_path: {_yaml_quote(socket)}",
                f"db_prefix: {_yaml_quote(db_name)}",
                f"startup_cmd: {_yaml_quote(server_cmd)}",
            ]
        )
    return "\n".join(lines) + "\n"


def _cli_main(argv):
    if len(argv) >= 2 and argv[1] == "render-squirrel-yaml":
        import argparse

        parser = argparse.ArgumentParser(prog="dhall_config.py render-squirrel-yaml")
        parser.add_argument("--dbms", required=True)
        parser.add_argument("--squirrel-home", required=True)
        parser.add_argument("--host", default="")
        parser.add_argument("--port", type=int, default=0)
        parser.add_argument("--user", default="")
        parser.add_argument("--password", default="")
        parser.add_argument("--socket", default="")
        parser.add_argument("--db-name", default="")
        parser.add_argument("--server-cmd", default="")
        args = parser.parse_args(argv[2:])
        sys.stdout.write(
            render_squirrel_yaml(
                dbms=args.dbms,
                squirrel_home=args.squirrel_home,
                host=args.host,
                port=args.port,
                user=args.user,
                password=args.password,
                socket=args.socket,
                db_name=args.db_name,
                server_cmd=args.server_cmd,
            )
        )
        return 0
    if len(argv) >= 2 and argv[1] == "squirrel-field":
        dbms, field = argv[2], argv[3]
        val = SQUIRREL_PROFILES[dbms].get(field)
        if val is not None:
            sys.stdout.write(str(val))
        return 0
    raise SystemExit("Usage: dhall_config.py <render-squirrel-yaml|squirrel-field> ...")


if __name__ == "__main__":
    raise SystemExit(_cli_main(sys.argv))

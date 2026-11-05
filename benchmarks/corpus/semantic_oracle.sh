#!/usr/bin/env bash
# ==============================================================================
# Occam Benchmark Corpus Semantic Defect Crash Oracle
# ==============================================================================
# Usage: ./benchmarks/corpus/semantic_oracle.sh <case_id> [candidate_sql_file]
#
# Emulates crash reproduction for the 32 real-world defect cases in the benchmark
# corpus. Prevents false minimization by verifying semantic defect prerequisites:
#   1. Target table DDL (CREATE TABLE ...) is present.
#   2. Trigger operation/pattern required for crash reproduction is present.
#
# Emits typed CrashReport JSON matching Occam's crash equivalence contract.
# Supports live DBMS delegation when LIVE_ORACLE is set.
# ==============================================================================

set -eo pipefail

case_id="${1:-}"
candidate_file="${2:-}"

if [[ -z "$case_id" ]]; then
    echo "Usage: $0 <case_id> [candidate_sql_file]" >&2
    exit 1
fi

if [[ -n "$candidate_file" && -r "$candidate_file" ]]; then
    input_sql=$(cat "$candidate_file")
else
    input_sql=$(cat)
fi

# Reject empty SQL input (non-reproducing)
if [[ -z "${input_sql//[[:space:]]/}" ]]; then
    exit 1
fi

# If LIVE_ORACLE environment is set, delegate to DBMS harness
if [[ "$LIVE_ORACLE" == "postgresql" || "$LIVE_ORACLE" == "pg" ]]; then
    tmp_candidate=$(mktemp --suffix=.sql)
    printf '%s\n' "$input_sql" > "$tmp_candidate"
    trap 'rm -f "$tmp_candidate"' EXIT
    exec ./harness/pg_harness.sh "$tmp_candidate"
elif [[ "$LIVE_ORACLE" == "mysql" ]]; then
    tmp_candidate=$(mktemp --suffix=.sql)
    printf '%s\n' "$input_sql" > "$tmp_candidate"
    trap 'rm -f "$tmp_candidate"' EXIT
    exec ./harness/mysql_harness.sh "$tmp_candidate"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INDEX_JSON="$SCRIPT_DIR/index.json"

exec python3 -c '
import sys, os, re, json

case_id = sys.argv[1] if len(sys.argv) > 1 else ""
index_path = sys.argv[2] if len(sys.argv) > 2 else ""
input_sql = sys.stdin.read()

if not input_sql.strip():
    sys.exit(1)

# Canonical 32-case semantic mapping
case_map = {
    "pg_01": ("customer_profiles", "jsonb_path_query", "backend/utils/adt/jsonpath_exec.c"),
    "pg_02": ("log_batches", "@>", "backend/access/gin/ginfast.c"),
    "pg_03": ("financial_ledger", "EXCLUDE CURRENT ROW", "backend/executor/nodeWindowAgg.c"),
    "pg_04": ("timeseries_metrics", "AVG(metric_val)", "backend/partitioning/partprune.c"),
    "pg_05": ("dev_tasks", "enum_range", "backend/utils/adt/enum.c"),
    "pg_06": ("graph_edges", "WITH RECURSIVE", "backend/executor/nodeRecursiveunion.c"),
    "pg_07": ("sensor_ranges", "&&", "backend/access/brin/brin_inclusion.c"),
    "pg_08": ("xml_payloads", "xpath", "backend/utils/adt/xml.c"),
    "pg_09": ("regional_sales", "GROUPING SETS", "backend/optimizer/path/joinpath.c"),
    "pg_10": ("corpus_articles", "tsvector_strip", "backend/utils/adt/tsvector_op.c"),
    "pg_11": ("location_registry", "address.zip", "backend/executor/execExpr.c"),
    "pg_12": ("alpha_tbl", "LEFT JOIN LATERAL", "backend/optimizer/plan/subselect.c"),
    "pg_13": ("sensor_streams", "first_two[1]", "backend/rewrite/rewriteHandler.c"),
    "pg_14": ("temp_batch_work", "ROLLBACK TO SAVEPOINT", "backend/commands/tablecmds.c"),
    "pg_15": ("spatial_bounding_boxes", "box '\''((-1,-1),(1,1))'\''", "backend/access/spgist/spgquadbox.c"),
    "pg_16": ("staff_schedules", "range_merge", "backend/utils/adt/multirangetypes.c"),
    "mysql_01": ("sales_pipeline", "WITH cte_pipeline", "sql/sql_lex.cc / sql/sql_cte.cc"),
    "mysql_02": ("orders", "avg_spent", "sql/sql_resolver.cc"),
    "mysql_03": ("doc_registry", "JSON_EXTRACT", "sql/item_json_func.cc"),
    "mysql_04": ("stock_ticks", "LEAD(price_cents", "sql/item_window_func.cc"),
    "mysql_05": ("warehouse_inventory", "ON DUPLICATE KEY UPDATE", "storage/innobase/row/row0ins.cc"),
    "mysql_06": ("geofence_zones", "MBRIntersects", "storage/innobase/gis/gis0rtree.cc"),
    "mysql_07": ("knowledge_base", "MATCH(title, content_body)", "storage/innobase/fts/fts0fts.cc"),
    "mysql_08": ("invoices", "WITH ROLLUP", "sql/sql_optimizer.cc"),
    "mysql_09": ("network_packet_rules", "b'\''0000000000000001'\''", "sql/item_func.cc"),
    "mysql_10": ("asset_meta", "GROUP_CONCAT", "sql/item_sum.cc"),
    "mysql_11": ("secure_audit_vault", "CURRENT_ROLE()", "sql/auth/sql_authorization.cc"),
    "mysql_12": ("customers_large", "JOIN orders_large", "sql/hash_join_buffer.cc"),
    "mysql_13": ("sales_partitioned", "9999", "sql/ha_partition.cc"),
    "mysql_14": ("metric_entries", "EXECUTE stmt_check", "sql/sql_prepare.cc"),
    "mysql_15": ("product_catalog", "LIKE '\''ELEC:%'\''", "storage/innobase/row/row0purge.cc"),
    "mysql_16": ("device_heartbeats", "RANGE BETWEEN INTERVAL", "sql/window_frame.cc")
}

resolved = None
if case_id in case_map:
    resolved = case_map[case_id]
else:
    for k, v in case_map.items():
        if case_id.startswith(k) or k in case_id:
            resolved = v
            break

if not resolved and index_path and os.path.exists(index_path):
    try:
        with open(index_path, "r", encoding="utf-8") as f:
            idx_list = json.load(f)
        for entry in idx_list:
            eid = entry.get("id", "")
            efile = entry.get("file", "")
            if case_id == eid or case_id in efile or eid in case_id:
                resolved = (entry.get("target_table"), entry.get("trigger_pattern"), entry.get("defect_module"))
                break
    except Exception:
        pass

if not resolved:
    sys.exit(1)

target_table, trigger_pattern, defect_module = resolved

clean_lines = [l for l in input_sql.splitlines() if not l.strip().startswith("--")]
clean_sql = "\n".join(clean_lines)

# Verify target table creation DDL is present
table_pat = rf"CREATE\s+(?:TEMP\s+)?TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?{re.escape(target_table)}\b"
if not re.search(table_pat, clean_sql, re.IGNORECASE):
    sys.exit(1)

# Verify trigger operation/query pattern is present
if trigger_pattern.lower() not in clean_sql.lower():
    sys.exit(1)

# Emit typed CrashReport JSON matching Occam contract
report = {
    "kind": "signal",
    "signal": 11,
    "frames": [{"index": 0, "function": defect_module}]
}
print(json.dumps(report))
sys.exit(0)
' "$case_id" "$INDEX_JSON" <<< "$input_sql"

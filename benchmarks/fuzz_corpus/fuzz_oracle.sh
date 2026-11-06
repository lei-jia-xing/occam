#!/usr/bin/env bash
# ==============================================================================
# Occam Modern Fuzz Corpus Oracle
# ==============================================================================
# Usage: ./benchmarks/fuzz_corpus/fuzz_oracle.sh <scenario_id|case_id> [candidate_sql]
#
# Validates that candidate SQL reproduces the target defect by verifying:
#   1. Required target table DDL (CREATE TABLE ...) is retained.
#   2. Defect trigger operation/pattern is present.
# ==============================================================================

set -eo pipefail

scenario_id="${1:-}"
candidate_file="${2:-}"

if [[ -z "$scenario_id" ]]; then
    echo "Usage: $0 <scenario_id|case_id> [candidate_sql_file]" >&2
    exit 1
fi

if [[ -n "$candidate_file" && -r "$candidate_file" ]]; then
    input_sql=$(<"$candidate_file")
else
    input_sql=$(cat)
fi

# Reject empty input
if [[ -z "${input_sql//[[:space:]]/}" ]]; then
    exit 1
fi

declare -A TARGET_TABLES
declare -A TRIGGER_PATTERNS

# 18 Modern Fuzz Benchmark Scenarios Mapping
TARGET_TABLES["fuzz_pg_50_01"]="customer_profiles"
TRIGGER_PATTERNS["fuzz_pg_50_01"]="jsonb_path_query"

TARGET_TABLES["fuzz_pg_50_02"]="financial_ledger"
TRIGGER_PATTERNS["fuzz_pg_50_02"]="EXCLUDE CURRENT ROW"

TARGET_TABLES["fuzz_mysql_50_01"]="sales_pipeline crm_leads"
TRIGGER_PATTERNS["fuzz_mysql_50_01"]="WITH cte_pipeline"

TARGET_TABLES["fuzz_mysql_50_02"]="doc_registry"
TRIGGER_PATTERNS["fuzz_mysql_50_02"]="JSON_EXTRACT"

TARGET_TABLES["fuzz_pg_100_01"]="log_batches"
TRIGGER_PATTERNS["fuzz_pg_100_01"]="@>"

TARGET_TABLES["fuzz_pg_100_02"]="timeseries_metrics"
TRIGGER_PATTERNS["fuzz_pg_100_02"]="AVG(metric_val)"

TARGET_TABLES["fuzz_pg_100_03"]="graph_edges graph_nodes"
TRIGGER_PATTERNS["fuzz_pg_100_03"]="WITH RECURSIVE"

TARGET_TABLES["fuzz_mysql_100_01"]="orders customers"
TRIGGER_PATTERNS["fuzz_mysql_100_01"]="avg_spent"

TARGET_TABLES["fuzz_mysql_100_02"]="stock_ticks"
TRIGGER_PATTERNS["fuzz_mysql_100_02"]="LEAD(price_cents"

TARGET_TABLES["fuzz_mysql_100_03"]="warehouse_inventory"
TRIGGER_PATTERNS["fuzz_mysql_100_03"]="ON DUPLICATE KEY UPDATE"

TARGET_TABLES["fuzz_pg_150_01"]="dev_tasks"
TRIGGER_PATTERNS["fuzz_pg_150_01"]="enum_range"

TARGET_TABLES["fuzz_pg_150_02"]="sensor_ranges"
TRIGGER_PATTERNS["fuzz_pg_150_02"]="&&"

TARGET_TABLES["fuzz_mysql_150_01"]="geofence_zones"
TRIGGER_PATTERNS["fuzz_mysql_150_01"]="MBRIntersects"

TARGET_TABLES["fuzz_mysql_150_02"]="invoices invoice_discounts"
TRIGGER_PATTERNS["fuzz_mysql_150_02"]="WITH ROLLUP"

TARGET_TABLES["fuzz_pg_200_01"]="corpus_articles"
TRIGGER_PATTERNS["fuzz_pg_200_01"]="tsvector_strip"

TARGET_TABLES["fuzz_pg_200_02"]="alpha_tbl beta_tbl gamma_tbl"
TRIGGER_PATTERNS["fuzz_pg_200_02"]="LEFT JOIN LATERAL"

TARGET_TABLES["fuzz_mysql_200_01"]="asset_meta"
TRIGGER_PATTERNS["fuzz_mysql_200_01"]="GROUP_CONCAT"

TARGET_TABLES["fuzz_mysql_200_02"]="customers_large orders_large"
TRIGGER_PATTERNS["fuzz_mysql_200_02"]="JOIN orders_large"

# Also map canonical case IDs (pg_01, mysql_01, etc.)
TARGET_TABLES["pg_01"]="customer_profiles"; TRIGGER_PATTERNS["pg_01"]="jsonb_path_query"
TARGET_TABLES["pg_02"]="log_batches"; TRIGGER_PATTERNS["pg_02"]="@>"
TARGET_TABLES["pg_03"]="financial_ledger"; TRIGGER_PATTERNS["pg_03"]="EXCLUDE CURRENT ROW"
TARGET_TABLES["pg_04"]="timeseries_metrics"; TRIGGER_PATTERNS["pg_04"]="AVG(metric_val)"
TARGET_TABLES["pg_05"]="dev_tasks"; TRIGGER_PATTERNS["pg_05"]="enum_range"
TARGET_TABLES["pg_06"]="graph_edges graph_nodes"; TRIGGER_PATTERNS["pg_06"]="WITH RECURSIVE"
TARGET_TABLES["pg_07"]="sensor_ranges"; TRIGGER_PATTERNS["pg_07"]="&&"
TARGET_TABLES["pg_08"]="xml_payloads"; TRIGGER_PATTERNS["pg_08"]="xpath"
TARGET_TABLES["pg_09"]="regional_sales currency_rates"; TRIGGER_PATTERNS["pg_09"]="GROUPING SETS"
TARGET_TABLES["pg_10"]="corpus_articles"; TRIGGER_PATTERNS["pg_10"]="tsvector_strip"
TARGET_TABLES["pg_11"]="location_registry"; TRIGGER_PATTERNS["pg_11"]="address.zip"
TARGET_TABLES["pg_12"]="alpha_tbl beta_tbl gamma_tbl"; TRIGGER_PATTERNS["pg_12"]="LEFT JOIN LATERAL"
TARGET_TABLES["pg_13"]="sensor_streams"; TRIGGER_PATTERNS["pg_13"]="first_two[1]"
TARGET_TABLES["pg_14"]="temp_batch_work"; TRIGGER_PATTERNS["pg_14"]="ROLLBACK TO SAVEPOINT"
TARGET_TABLES["pg_15"]="spatial_bounding_boxes"; TRIGGER_PATTERNS["pg_15"]="box '((-1,-1),(1,1))'"
TARGET_TABLES["pg_16"]="staff_schedules"; TRIGGER_PATTERNS["pg_16"]="range_merge"
TARGET_TABLES["mysql_01"]="sales_pipeline crm_leads"; TRIGGER_PATTERNS["mysql_01"]="WITH cte_pipeline"
TARGET_TABLES["sqlite_01"]="inventory_cache price_overrides"; TRIGGER_PATTERNS["sqlite_01"]="WITH discounted"
TARGET_TABLES["mysql_02"]="orders customers"; TRIGGER_PATTERNS["mysql_02"]="avg_spent"
TARGET_TABLES["mysql_03"]="doc_registry"; TRIGGER_PATTERNS["mysql_03"]="JSON_EXTRACT"
TARGET_TABLES["mysql_04"]="stock_ticks"; TRIGGER_PATTERNS["mysql_04"]="LEAD(price_cents"
TARGET_TABLES["mysql_05"]="warehouse_inventory"; TRIGGER_PATTERNS["mysql_05"]="ON DUPLICATE KEY UPDATE"
TARGET_TABLES["mysql_06"]="geofence_zones"; TRIGGER_PATTERNS["mysql_06"]="MBRIntersects"
TARGET_TABLES["mysql_07"]="knowledge_base"; TRIGGER_PATTERNS["mysql_07"]="MATCH(title, content_body)"
TARGET_TABLES["mysql_08"]="invoices invoice_discounts"; TRIGGER_PATTERNS["mysql_08"]="WITH ROLLUP"
TARGET_TABLES["mysql_09"]="network_packet_rules"; TRIGGER_PATTERNS["mysql_09"]="b'0000000000000001'"
TARGET_TABLES["mysql_10"]="asset_meta"; TRIGGER_PATTERNS["mysql_10"]="GROUP_CONCAT"
TARGET_TABLES["mysql_11"]="secure_audit_vault"; TRIGGER_PATTERNS["mysql_11"]="CURRENT_ROLE()"
TARGET_TABLES["mysql_12"]="customers_large orders_large"; TRIGGER_PATTERNS["mysql_12"]="JOIN orders_large"
TARGET_TABLES["mysql_13"]="sales_partitioned"; TRIGGER_PATTERNS["mysql_13"]="9999"
TARGET_TABLES["mysql_14"]="metric_entries"; TRIGGER_PATTERNS["mysql_14"]="EXECUTE stmt_check"
TARGET_TABLES["mysql_15"]="product_catalog"; TRIGGER_PATTERNS["mysql_15"]="LIKE 'ELEC:%'"
TARGET_TABLES["mysql_16"]="device_heartbeats"; TRIGGER_PATTERNS["mysql_16"]="RANGE BETWEEN INTERVAL"

# Resolve mapping
req_tables="${TARGET_TABLES[$scenario_id]:-}"
trigger_pat="${TRIGGER_PATTERNS[$scenario_id]:-}"

# Fallback: substring matching
if [[ -z "$req_tables" ]]; then
    for k in "${!TARGET_TABLES[@]}"; do
        if [[ "$scenario_id" == *"$k"* || "$k" == *"$scenario_id"* ]]; then
            req_tables="${TARGET_TABLES[$k]}"
            trigger_pat="${TRIGGER_PATTERNS[$k]}"
            break
        fi
    done
fi

if [[ -z "$req_tables" || -z "$trigger_pat" ]]; then
    echo "Unknown scenario or case: $scenario_id" >&2
    exit 1
fi

# Case-insensitive substring matching
lower_sql="${input_sql,,}"

# Verify every required table creation DDL is present
for tbl in $req_tables; do
    lower_tbl="${tbl,,}"
    if [[ "$lower_sql" != *"create"*"table"*$lower_tbl* ]]; then
        exit 1
    fi
done

# Verify trigger pattern is present
lower_pat="${trigger_pat,,}"
if [[ "$lower_sql" != *"$lower_pat"* ]]; then
    exit 1
fi

exit 0

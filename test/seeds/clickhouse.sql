-- DDL
-- @case: clickhouse_ddl_mergetree_projection_ttl_settings
-- @dialects: clickhouse
-- @units: sql=;
CREATE TABLE seed_ch_events (
    id UInt64,
    ts DateTime,
    user_id UInt64,
    payload String,
    PROJECTION seed_ch_by_user (SELECT user_id ORDER BY ts)
) ENGINE = MergeTree
ORDER BY (id, ts)
TTL ts + INTERVAL 1 DAY
SETTINGS index_granularity = 8192;

-- @case: clickhouse_ddl_create_function
-- @dialects: clickhouse
-- @units: sql=;
CREATE FUNCTION seed_ch_double AS (x) -> x * 2;

-- @case: clickhouse_ddl_materialized_view_to
-- @dialects: clickhouse
-- @units: sql=;,sql=;,sql=;
CREATE TABLE seed_ch_src (k UInt32) ENGINE = MergeTree ORDER BY k;
-- @unit
CREATE TABLE seed_ch_dst (k UInt32, total UInt64) ENGINE = MergeTree ORDER BY k;
-- @unit
CREATE MATERIALIZED VIEW seed_ch_mv TO seed_ch_dst
AS SELECT k, count() AS total FROM seed_ch_src GROUP BY k;

-- DML
-- @case: clickhouse_dml_insert_select
-- @dialects: clickhouse
-- @units: sql=;,sql=;
CREATE TABLE seed_ch_numbers (n UInt64, label String) ENGINE = MergeTree ORDER BY n;
-- @unit
INSERT INTO seed_ch_numbers
SELECT number + 10, 'n' FROM numbers(3);

-- @case: clickhouse_dml_alter_update
-- @dialects: clickhouse
-- @units: sql=;,sql=;
CREATE TABLE seed_ch_mut (id UInt64, v UInt32) ENGINE = MergeTree ORDER BY id;
-- @unit
ALTER TABLE seed_ch_mut UPDATE v = v + 1 WHERE id = 1;

-- Query
-- @case: clickhouse_query_prewhere_array_join
-- @dialects: clickhouse
-- @units: sql=;,sql=;
CREATE TABLE seed_ch_events2 (
    id UInt64,
    user_id UInt64,
    tags Array(String),
    score Int32
) ENGINE = MergeTree ORDER BY id;
-- @unit
SELECT id, tag, score
FROM seed_ch_events2
ARRAY JOIN tags AS tag
PREWHERE score > 0
WHERE id > 0
ORDER BY id
SETTINGS max_threads = 1
FORMAT Null;

-- Trailing comma without alias isolates ClickHouse trailing-comma syntax.
-- @case: clickhouse_query_grouping_sets_trailing_comma
-- @dialects: clickhouse
-- @units: sql=;
SELECT
    number % 2 AS bucket,
    count(),
FROM numbers(6)
GROUP BY GROUPING SETS ((number % 2), ())
ORDER BY bucket NULLS FIRST
FORMAT Null;

-- Utility
-- @case: clickhouse_utility_attach_detach
-- @dialects: clickhouse
-- @units: sql=;,sql=;,sql=;
CREATE TABLE seed_ch_attach (id UInt64) ENGINE = MergeTree ORDER BY id;
-- @unit
DETACH TABLE seed_ch_attach SYNC;
-- @unit
ATTACH TABLE seed_ch_attach;

-- Lexical
-- ClickHouse two-argument CAST(expr, 'Type') isolates ClickHouse cast syntax
-- from PostgreSQL/SQLite/DuckDB (which accept standard CAST(x AS UInt32) syntactically).
-- @case: clickhouse_utf8_cast_ordered
-- @dialects: clickhouse
-- @units: sql=;
SELECT 'héllo' AS label,
       concat('wörld', '!') AS other,
       CAST('42', 'UInt32') AS n
FROM numbers(1)
ORDER BY n;

-- Extended ClickHouse coverage
-- @case: clickhouse_typed_path_cast
-- @dialects: clickhouse
-- @units: sql=;,sql=;
CREATE TABLE seed_ch_json (data JSON) ENGINE = MergeTree ORDER BY tuple();
-- @unit
SELECT data.:Int64 AS v FROM seed_ch_json;

-- @case: clickhouse_dml_alter_delete_optimize
-- @dialects: clickhouse
-- @units: sql=;,sql=;,sql=;
CREATE TABLE seed_ch_del (id UInt64, v Int32) ENGINE = MergeTree ORDER BY id;
-- @unit
ALTER TABLE seed_ch_del DELETE WHERE id = 1;
-- @unit
OPTIMIZE TABLE seed_ch_del FINAL;

-- @case: clickhouse_query_lambda_array_map
-- @dialects: clickhouse
-- @units: sql=;
SELECT arrayMap(x -> x * 2, [1, 2, 3]) AS doubled FORMAT Null;

-- @case: clickhouse_query_global_strictness_join
-- @dialects: clickhouse
-- @units: sql=;,sql=;,sql=;
CREATE TABLE seed_ch_j1 (k UInt64) ENGINE = MergeTree ORDER BY k;
-- @unit
CREATE TABLE seed_ch_j2 (k UInt64) ENGINE = MergeTree ORDER BY k;
-- @unit
SELECT j1.k
FROM seed_ch_j1 AS j1
GLOBAL ANY LEFT JOIN seed_ch_j2 AS j2 ON j1.k = j2.k;

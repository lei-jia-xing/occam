-- DDL
-- @case: duckdb_ddl_macro
-- @dialects: duckdb
-- @units: sql=;
CREATE MACRO seed_duck_double(x) AS x * 2;

-- @case: shared_ddl_create_view
-- @dialects: postgresql,mysql,mariadb,sqlite,duckdb,clickhouse
-- @units: sql=;
CREATE VIEW seed_shared_view AS SELECT 1 AS n;

-- DML
-- @case: duckdb_dml_insert_returning
-- @dialects: postgresql,mariadb,sqlite,duckdb
-- @units: sql=;,sql=;
CREATE TABLE seed_duck_returning (id INT PRIMARY KEY, v VARCHAR(16) NOT NULL);
-- @unit
INSERT INTO seed_duck_returning (id, v) VALUES (1, 'alpha'), (2, 'beta')
RETURNING id, v;

-- @case: duckdb_dml_delete_using_returning
-- @dialects: postgresql,duckdb
-- @units: sql=;,sql=;,sql=;
CREATE TABLE seed_duck_del (id INTEGER PRIMARY KEY, v VARCHAR NOT NULL);
-- @unit
CREATE TABLE seed_duck_del_src (id INTEGER PRIMARY KEY);
-- @unit
DELETE FROM seed_duck_del
USING seed_duck_del_src AS s
WHERE seed_duck_del.id = s.id
RETURNING id;

-- Query
-- @case: duckdb_query_from_first
-- @dialects: duckdb,clickhouse
-- @units: sql=;
FROM (VALUES (1, 'a'), (2, 'b')) AS t(id, label)
SELECT id, label
ORDER BY id;

-- Static IN (...) keeps PIVOT a single statement in DuckDB's
-- duckdb_extract_statements (dynamic PIVOT without IN expands into 2 statements).
-- @case: duckdb_query_pivot
-- @dialects: duckdb
-- @units: sql=;,sql=;,sql=;
CREATE TABLE seed_duck_sales (region VARCHAR, quarter VARCHAR, amount INTEGER);
-- @unit
INSERT INTO seed_duck_sales (region, quarter, amount) VALUES
    ('east', 'q1', 10),
    ('east', 'q2', 20),
    ('west', 'q1', 5);
-- @unit
PIVOT seed_duck_sales ON quarter IN ('q1', 'q2') USING sum(amount) GROUP BY region;

-- @case: duckdb_query_qualify
-- @dialects: duckdb,clickhouse
-- @units: sql=;
SELECT id, team, score,
       row_number() OVER (PARTITION BY team ORDER BY score DESC) AS rn
FROM (
    SELECT 1 AS id, 'a' AS team, 10 AS score
    UNION ALL SELECT 2, 'a', 20
    UNION ALL SELECT 3, 'b', 30
) AS t
QUALIFY rn = 1
ORDER BY id;

-- ASOF RIGHT JOIN is DuckDB-only (ClickHouse supports ASOF LEFT JOIN but not RIGHT).
-- @case: duckdb_query_asof_join
-- @dialects: duckdb
-- @units: sql=;,sql=;,sql=;
CREATE TABLE seed_duck_quotes (ts TIMESTAMP, price DOUBLE);
-- @unit
CREATE TABLE seed_duck_trades (ts TIMESTAMP, qty INTEGER);
-- @unit
SELECT q.ts, q.price, t.qty
FROM seed_duck_quotes AS q
ASOF RIGHT JOIN seed_duck_trades AS t ON q.ts >= t.ts
ORDER BY q.ts;

-- Extended DDL, DML, utility, and grouping coverage (Phase 2b ledger)
-- @case: duckdb_ddl_create_or_replace
-- @dialects: duckdb
-- @units: sql=;,sql=;
CREATE OR REPLACE TABLE seed_duck_rep (id INT, name VARCHAR);
-- @unit
CREATE OR REPLACE MACRO seed_duck_add(x, y) AS x + y;

-- @case: duckdb_utility_pragma_attach_prepare
-- @dialects: duckdb
-- @units: sql=;,sql=;,sql=;,sql=;,sql=;,sql=;
PRAGMA version;
-- @unit
ATTACH IF NOT EXISTS ':memory:' AS seed_duck_aux;
-- @unit
DETACH seed_duck_aux;
-- @unit
PREPARE seed_duck_ps AS SELECT $1 + $2;
-- @unit
EXECUTE seed_duck_ps(1, 2);
-- @unit
DEALLOCATE PREPARE seed_duck_ps;

-- @case: duckdb_dml_update_from_returning
-- @dialects: postgresql,sqlite,duckdb
-- @units: sql=;,sql=;,sql=;,sql=;,sql=;
CREATE TABLE seed_duck_u1 (id INT PRIMARY KEY, v INT NOT NULL);
-- @unit
CREATE TABLE seed_duck_u2 (id INT PRIMARY KEY, v INT NOT NULL);
-- @unit
INSERT INTO seed_duck_u1 VALUES (1, 10);
-- @unit
INSERT INTO seed_duck_u2 VALUES (1, 20);
-- @unit
UPDATE seed_duck_u1
SET v = seed_duck_u2.v
FROM seed_duck_u2
WHERE seed_duck_u1.id = seed_duck_u2.id
RETURNING seed_duck_u1.id, seed_duck_u1.v;

-- @case: duckdb_query_grouping_sets_shared
-- @dialects: postgresql,duckdb,clickhouse
-- @units: sql=;
SELECT a, b, count(*) AS cnt
FROM (VALUES (1, 10), (1, 20), (2, 30)) AS t(a, b)
GROUP BY GROUPING SETS ((a), (b), ())
ORDER BY a NULLS LAST, b NULLS LAST;

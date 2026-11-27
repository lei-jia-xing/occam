-- DDL
-- @case: sqlite_ddl_strict_table
-- @dialects: sqlite
-- @units: sql=;
CREATE TABLE seed_sqlite_strict (
    id INTEGER PRIMARY KEY,
    code TEXT NOT NULL UNIQUE,
    qty INTEGER NOT NULL DEFAULT 0 CHECK (qty >= 0),
    total INTEGER GENERATED ALWAYS AS (qty * 2) STORED
) STRICT;

-- @case: sqlite_ddl_partial_index
-- @dialects: postgresql,sqlite
-- @units: sql=;,sql=;
CREATE TABLE seed_shared_partial (id INTEGER PRIMARY KEY, qty INTEGER NOT NULL);
-- @unit
CREATE INDEX seed_shared_partial_qty_idx ON seed_shared_partial (qty) WHERE qty > 0;

-- DML
-- @case: sqlite_dml_upsert_returning
-- @dialects: postgresql,sqlite,duckdb
-- @units: sql=;,sql=;,sql=;
CREATE TABLE seed_shared_upsert (
    id INT PRIMARY KEY,
    label TEXT NOT NULL,
    hits INT NOT NULL DEFAULT 0
);
-- @unit
INSERT INTO seed_shared_upsert (id, label) VALUES (1, 'alpha'), (2, 'beta');
-- @unit
INSERT INTO seed_shared_upsert (id, label)
VALUES (1, 'gamma')
ON CONFLICT (id) DO UPDATE
SET label = excluded.label, hits = hits + 1
RETURNING id, label, hits;

-- WHEN (...) on a BEGIN ... END trigger isolates SQLite grammar from
-- MySQL/MariaDB server grammar (which accept FOR EACH ROW BEGIN ... END).
-- @case: sqlite_trigger_opaque_body
-- @dialects: sqlite
-- @units: sql=;,sql=;
CREATE TABLE seed_sqlite_trg (id INTEGER PRIMARY KEY, n INTEGER NOT NULL DEFAULT 0);
-- @unit
CREATE TRIGGER seed_sqlite_trg_ai
AFTER INSERT ON seed_sqlite_trg
FOR EACH ROW
WHEN (NEW.n >= 0)
BEGIN
    UPDATE seed_sqlite_trg SET n = n + 10 WHERE id = NEW.id;
END;

-- @case: sqlite_ddl_rename_column
-- @dialects: postgresql,mysql,mariadb,sqlite,duckdb
-- @units: sql=;,sql=;
CREATE TABLE seed_shared_rename (id INTEGER PRIMARY KEY, old_name TEXT NOT NULL);
-- @unit
ALTER TABLE seed_shared_rename RENAME COLUMN old_name TO new_name;

-- Query
-- @case: sqlite_query_recursive_cte
-- @dialects: postgresql,mysql,mariadb,sqlite,duckdb
-- @units: sql=;
WITH RECURSIVE seed_shared_n(n) AS (
    SELECT 1
    UNION ALL
    SELECT n + 1 FROM seed_shared_n WHERE n < 4
)
SELECT n FROM seed_shared_n ORDER BY n;

-- ClickHouse also supports MySQL-style LIMIT offset, count per
-- https://clickhouse.com/docs/sql-reference/statements/select/limit.
-- @case: sqlite_query_limit_comma
-- @dialects: mysql,mariadb,sqlite,clickhouse
-- @units: sql=;
SELECT value
FROM (SELECT 1 AS value UNION ALL SELECT 2 UNION ALL SELECT 3) AS s
ORDER BY value
LIMIT 1, 2;

-- ClickHouse also supports LIMIT count OFFSET offset per
-- https://clickhouse.com/docs/sql-reference/statements/select/limit.
-- @case: sqlite_query_limit_offset
-- @dialects: postgresql,mysql,mariadb,sqlite,duckdb,clickhouse
-- @units: sql=;
SELECT value
FROM (SELECT 1 AS value UNION ALL SELECT 2 UNION ALL SELECT 3) AS s
ORDER BY value
LIMIT 2 OFFSET 1;

-- Lexical
-- @case: sqlite_lexical_bracket_double_quote_pair
-- @dialects: sqlite
-- @units: sql=;,sql=;
SELECT [id] AS [naïve], 'héllo' AS label, CAST('12' AS INTEGER) AS casted
FROM (SELECT 1 AS [id]);
-- @unit
SELECT "id" AS "naïve", 'It''s fine' AS quoted, x'00ff' AS blob_value
FROM (SELECT 1 AS "id");

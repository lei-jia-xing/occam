-- Client directive negatives
-- @case: invalid_mysql_delimiter_no_space
-- @dialects: mysql,mariadb
-- @units: sql=;
DELIMITER;

-- DDL negatives
-- @case: invalid_mysql_alter_drop_key_primary_key
-- @dialects: mysql,mariadb
-- @units: sql=;
ALTER TABLE seed_invalid_t DROP KEY PRIMARY KEY;

-- @case: invalid_mysql_trigger_event_select
-- @dialects: mysql,mariadb
-- @units: sql=;
CREATE TRIGGER seed_invalid_tr
BEFORE SELECT ON seed_invalid_t
FOR EACH ROW SET @x = 1;

-- @case: invalid_postgresql_routine_missing_body
-- @dialects: postgresql
-- @units: sql=;
CREATE FUNCTION seed_invalid_fn() RETURNS int AS;

-- @case: invalid_clickhouse_drop_materialized_view
-- @dialects: clickhouse
-- @units: sql=;
DROP MATERIALIZED VIEW seed_invalid_mv;

-- DML negatives
-- @case: invalid_sqlite_delete_using
-- @dialects: sqlite
-- @units: sql=;
DELETE FROM seed_invalid_a
USING seed_invalid_b
WHERE seed_invalid_a.id = seed_invalid_b.id;

-- @case: invalid_mysql_insert_returning
-- @dialects: mysql
-- @units: sql=;
INSERT INTO seed_invalid_ret (id) VALUES (1) RETURNING id;

-- Transaction negatives
-- @case: invalid_postgresql_begin_foobar
-- @dialects: postgresql
-- @units: sql=;
BEGIN FOOBAR;

-- @case: invalid_duckdb_savepoint
-- @dialects: duckdb
-- @units: sql=;
SAVEPOINT seed_invalid_sp;

-- Join & locking & DDL tightening negatives (Phase 2b)
-- @case: invalid_natural_join_on
-- @dialects: postgresql,mysql,mariadb,sqlite,duckdb,clickhouse
-- @units: sql=;
SELECT t.a FROM (SELECT 1 AS a) AS t NATURAL JOIN (SELECT 1 AS a) AS u ON t.a = u.a;

-- @case: invalid_natural_join_using
-- @dialects: postgresql,mysql,mariadb,sqlite,duckdb,clickhouse
-- @units: sql=;
SELECT t.a FROM (SELECT 1 AS a) AS t NATURAL LEFT JOIN (SELECT 1 AS a) AS u USING (a);

-- @case: invalid_for_update_non_locking
-- @dialects: sqlite,duckdb,clickhouse
-- @units: sql=;
SELECT 1 FOR UPDATE;

-- @case: invalid_postgresql_temp_policy
-- @dialects: postgresql
-- @units: sql=;
CREATE TEMP POLICY seed_invalid_pol ON seed_invalid_t;

-- @case: invalid_postgresql_alter_rename_multi
-- @dialects: postgresql
-- @units: sql=;
ALTER TABLE seed_invalid_t RENAME TO seed_invalid_u, ADD COLUMN c INT;

-- @case: invalid_sqlite_within_group
-- @dialects: sqlite,mysql
-- @units: sql=;
SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY a) FROM seed_invalid_t;

-- @case: invalid_cross_join_on
-- @dialects: postgresql,duckdb,clickhouse
-- @units: sql=;
SELECT t.a FROM t CROSS JOIN u ON t.a = u.a;

-- DDL
-- @case: mariadb_ddl_sequence
-- @dialects: postgresql,mariadb,duckdb
-- @units: sql=;
CREATE SEQUENCE seed_maria_seq
    START WITH 10
    INCREMENT BY 2
    MINVALUE 10
    MAXVALUE 100
    CYCLE;

-- @case: mariadb_ddl_fulltext_index
-- @dialects: mysql,mariadb
-- @units: sql=;,sql=;
CREATE TABLE seed_maria_docs (id INT PRIMARY KEY, body TEXT NOT NULL) ENGINE=InnoDB;
-- @unit
CREATE FULLTEXT INDEX seed_maria_docs_ft ON seed_maria_docs (body);

-- @case: mariadb_ddl_hash_index
-- @dialects: mysql,mariadb
-- @units: sql=;,sql=;
CREATE TABLE seed_maria_mem (id INT NOT NULL, v INT NOT NULL) ENGINE=MEMORY;
-- @unit
CREATE UNIQUE INDEX seed_maria_mem_uq USING HASH ON seed_maria_mem (id);

-- @case: mariadb_ddl_alter_algorithm
-- @dialects: mysql,mariadb
-- @units: sql=;,sql=;
CREATE TABLE seed_maria_alter (id INT PRIMARY KEY);
-- @unit
ALTER TABLE seed_maria_alter
    ADD COLUMN note VARCHAR(8) NULL,
    ALGORITHM=INPLACE;

-- DML
-- @case: mariadb_dml_insert_returning
-- @dialects: postgresql,mariadb,sqlite,duckdb
-- @units: sql=;,sql=;
CREATE TABLE seed_maria_ret (id INT PRIMARY KEY, label VARCHAR(32) NOT NULL);
-- @unit
INSERT INTO seed_maria_ret (id, label) VALUES (1, 'alpha'), (2, 'beta')
RETURNING id, label;

-- Window percentile
-- @case: mariadb_window_within_group_over
-- @dialects: mariadb
-- @units: sql=;,sql=;
CREATE TABLE seed_maria_wg (id INT PRIMARY KEY, v INT NOT NULL) ENGINE=InnoDB;
-- @unit
SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY v) OVER () AS p50 FROM seed_maria_wg;

-- Utility
-- @case: mariadb_prepare_from_expression
-- @dialects: mariadb
-- @units: sql=;,sql=;,sql=;
PREPARE seed_maria_stmt FROM CONCAT('SELECT ', 1 + 1);
-- @unit
EXECUTE seed_maria_stmt;
-- @unit
DEALLOCATE PREPARE seed_maria_stmt;

-- Client delimiter directives
-- @case: mariadb_routine_delimiter_slash
-- @dialects: mysql,mariadb
-- @units: directive=//,sql=//,directive=;
DELIMITER //
-- @unit
CREATE PROCEDURE seed_maria_proc_slash()
BEGIN
    SELECT COUNT(*) AS n FROM (SELECT 1 UNION ALL SELECT 2) AS s;
END//
-- @unit
DELIMITER ;

-- @case: mariadb_delimiter_unquoted_tail
-- @dialects: mysql,mariadb
-- @units: directive=$$,sql=$$,directive=;
DELIMITER $$
-- @unit
SELECT 1 AS a$$
-- @unit
DELIMITER ;

-- Lexical
-- @case: mariadb_utf8_introducer_cast
-- @dialects: mysql,mariadb
-- @units: sql=;
SELECT _utf8mb4'héllo' AS label,
       CAST(_utf8mb4'wörld' AS CHAR(16)) AS casted,
       CONCAT(_utf8mb4'héllo', '!') AS joined
ORDER BY label COLLATE utf8mb4_unicode_ci;

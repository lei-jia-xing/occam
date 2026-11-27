-- DDL
-- SECONDARY_ENGINE is a MySQL 8.0+ table option (syntax error in MariaDB;
-- COLLATE=utf8mb4_0900_ai_ci alone differs only in the runtime collation catalog).
-- @case: mysql_ddl_table_index_charset
-- @dialects: mysql
-- @units: sql=;
CREATE TABLE seed_mysql_items (
    id INT NOT NULL AUTO_INCREMENT,
    name VARCHAR(64) NOT NULL,
    body TEXT,
    price DECIMAL(10, 2) NOT NULL DEFAULT 0.00,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (id),
    UNIQUE KEY seed_mysql_items_name_uq (name),
    KEY seed_mysql_items_name_prefix (name(10)),
    FULLTEXT KEY seed_mysql_items_body_ft (body)
) ENGINE=InnoDB
  DEFAULT CHARSET=utf8mb4
  COLLATE=utf8mb4_0900_ai_ci
  AUTO_INCREMENT=100
  SECONDARY_ENGINE=NULL;

-- @case: mysql_ddl_trigger_before_insert
-- @dialects: mysql,mariadb
-- @units: sql=;,sql=;
CREATE TABLE seed_my_trg (id INT PRIMARY KEY, n INT NOT NULL DEFAULT 0);
-- @unit
CREATE TRIGGER seed_my_trg_bi
BEFORE INSERT ON seed_my_trg
FOR EACH ROW SET NEW.n = NEW.n + 1;

-- DML
-- @case: mysql_dml_insert_duplicate_key
-- @dialects: mysql,mariadb
-- @units: sql=;,sql=;
CREATE TABLE seed_my_upsert (
    id INT PRIMARY KEY,
    label VARCHAR(32) NOT NULL,
    hits INT NOT NULL DEFAULT 0
);
-- @unit
INSERT INTO seed_my_upsert (id, label, hits) VALUES
    (1, 'alpha', 0),
    (2, 'beta', 0)
ON DUPLICATE KEY UPDATE
    label = VALUES(label),
    hits = hits + 1;

-- @case: mysql_dml_multi_table_update
-- @dialects: mysql,mariadb
-- @units: sql=;,sql=;,sql=;
CREATE TABLE seed_my_users (id INT PRIMARY KEY, name VARCHAR(32) NOT NULL);
-- @unit
CREATE TABLE seed_my_orders (id INT PRIMARY KEY, user_id INT NOT NULL, total INT NOT NULL);
-- @unit
UPDATE seed_my_users u
JOIN seed_my_orders o ON o.user_id = u.id
SET u.name = CONCAT(u.name, '!')
WHERE o.total > 10;

-- @case: mysql_dml_insert_values_row
-- @dialects: mysql
-- @units: sql=;,sql=;
CREATE TABLE seed_my_row (id INT PRIMARY KEY, label VARCHAR(16) NOT NULL);
-- @unit
INSERT INTO seed_my_row (id, label) VALUES ROW(1, 'a'), ROW(2, 'b');

-- Client delimiter directives
-- @case: mysql_routine_delimiter_dollar
-- @dialects: mysql,mariadb
-- @units: directive=$$,sql=$$,directive=;
DELIMITER $$
-- @unit
CREATE PROCEDURE seed_my_proc_dollar()
BEGIN
    DECLARE steps INT DEFAULT 0;
    SET steps = steps + 1;
    SELECT steps AS step_count;
END$$
-- @unit
DELIMITER ;

-- @case: mysql_routine_delimiter_slash
-- @dialects: mysql,mariadb
-- @units: directive=//,sql=//,directive=;
DELIMITER //
-- @unit
CREATE FUNCTION seed_my_fn_slash(x INT)
RETURNS INT
DETERMINISTIC
BEGIN
    RETURN x * 2;
END//
-- @unit
DELIMITER ;

-- @case: mysql_delimiter_unquoted_tail
-- @dialects: mysql,mariadb
-- @units: directive=$$,sql=$$,directive=;
DELIMITER $$
-- @unit
SELECT 1 AS a$$
-- @unit
DELIMITER ;

-- Lexical
-- @case: mysql_utf8_introducer_cast
-- @dialects: mysql,mariadb
-- @units: sql=;
SELECT _utf8mb4'héllo' AS label,
       CAST(_utf8mb4'wörld' AS CHAR(16)) AS casted,
       CONCAT(_utf8mb4'héllo', '!') AS joined
ORDER BY label COLLATE utf8mb4_unicode_ci;

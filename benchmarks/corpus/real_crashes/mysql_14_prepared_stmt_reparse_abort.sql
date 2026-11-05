-- Case ID: mysql_14
-- Target DBMS: MySQL (>= 8.0.21)
-- Bug Class: Assertion Failure (thd->lex == lex)
-- Defect Module: sql/sql_prepare.cc
-- Reference: MySQL-BUG-103112
-- Description: Assertion failure when re-preparing statement referencing a view
--              after underlying base table schema alteration.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE query_stats (
    stat_id INT PRIMARY KEY,
    query_digest VARCHAR(64),
    exec_count BIGINT
);

CREATE TABLE prepared_handles (
    handle_id INT PRIMARY KEY,
    stmt_name VARCHAR(64),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO query_stats (stat_id, query_digest, exec_count) VALUES
    (1, 'digest_001', 500),
    (2, 'digest_002', 1200);

INSERT INTO prepared_handles (handle_id, stmt_name) VALUES
    (10, 'stmt_find_user'),
    (20, 'stmt_order_lookup');

-- Target Reproduction Scenario: Table, View, and Prepared Statement Reparse
CREATE TABLE metric_entries (
    entry_id INT PRIMARY KEY,
    sensor_id INT NOT NULL,
    temperature DOUBLE NOT NULL,
    humidity DOUBLE NOT NULL
);

CREATE VIEW v_active_metrics AS
SELECT entry_id, sensor_id, temperature
FROM metric_entries
WHERE temperature > 0.0;

INSERT INTO metric_entries (entry_id, sensor_id, temperature, humidity) VALUES
    (1, 101, 22.5, 45.0),
    (2, 101, 24.1, 48.0),
    (3, 102, -5.0, 80.0);

-- Prepare statement referencing view
PREPARE stmt_check FROM 'SELECT entry_id, sensor_id FROM v_active_metrics WHERE sensor_id = ?';

SET @sid = 101;
EXECUTE stmt_check USING @sid;

-- Invalidate schema cache by altering base table
ALTER TABLE metric_entries ADD COLUMN pressure DOUBLE DEFAULT 1013.25;

-- Crashing Statement: Re-execution forcing re-parse and LEX mismatch assertion
EXECUTE stmt_check USING @sid;

DEALLOCATE PREPARE stmt_check;

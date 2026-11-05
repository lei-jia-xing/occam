-- Case ID: mysql_14
-- Target DBMS: MySQL (>= 8.0.21)
-- Bug Class: Assertion Failure (thd->lex == lex)
-- Defect Module: sql/sql_prepare.cc
-- Reference: MySQL-BUG-103112
-- Description: Assertion failure when re-preparing statement referencing a view
--              after underlying base table schema alteration.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




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
;


-- Prepare statement referencing view
PREPARE stmt_check FROM 'SELECT entry_id, sensor_id FROM v_active_metrics WHERE sensor_id = ?';

SET @sid = 101;
EXECUTE stmt_check USING @sid;

-- Invalidate schema cache by altering base table

-- Crashing Statement: Re-execution forcing re-parse and LEX mismatch assertion

-- Case ID: mysql_05
-- Target DBMS: MySQL (>= 8.0.20)
-- Bug Class: Assertion Failure / SIGABRT (btr_pcur_restore_position)
-- Defect Module: storage/innobase/row/row0ins.cc
-- Reference: MySQL-BUG-98380
-- Description: Multi-row INSERT ... ON DUPLICATE KEY UPDATE with multiple composite
--              unique indexes and auto-increment leading to B-tree cursor desync.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE audit_changelog (
    log_id INT PRIMARY KEY AUTO_INCREMENT,
    action_type VARCHAR(32),
    logged_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);



-- Target Reproduction Scenario: Table with PK and two conflicting UNIQUE keys


-- Crashing DML: Batch multi-row upsert triggering simultaneous collision across both distinct unique keys


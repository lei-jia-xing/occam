-- Case ID: mysql_12
-- Target DBMS: MySQL (>= 8.0.20)
-- Bug Class: SIGSEGV (Invalid pointer in HashJoinRowBuffer::StoreRow)
-- Defect Module: sql/hash_join_buffer.cc
-- Reference: MySQL-BUG-101902
-- Description: Hash join memory buffer spill crash when build table contains
--              variable-length nullable JSON and long string columns.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE billing_cycles (
    cycle_id INT PRIMARY KEY,
    cycle_name VARCHAR(32),
    days_in_cycle INT
);




-- Target Reproduction Scenario: Tables with large variable-width rows joined via HASH JOIN




-- Crashing Query: Hash join forcing disk spill buffer allocation on JSON/TEXT projections

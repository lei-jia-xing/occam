-- Case ID: mysql_09
-- Target DBMS: MySQL (>= 8.0.19)
-- Bug Class: SIGSEGV (Buffer overrun in Item_func_bit::val_int)
-- Defect Module: sql/item_func.cc
-- Reference: MySQL-BUG-101450
-- Description: CHECK constraint evaluation crash when validating bit fields BIT(N)
--              under multi-row INSERT with bitwise arithmetic expressions.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE user_preferences (
    pref_id INT PRIMARY KEY,
    user_id INT,
    theme VARCHAR(16) DEFAULT 'dark'
);




-- Target Reproduction Scenario: Table with BIT columns and bitwise CHECK constraint

-- Crashing DML: Multi-row insert evaluating bitwise CHECK constraint on boundary bits


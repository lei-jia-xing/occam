-- Case ID: mysql_04
-- Target DBMS: MySQL (>= 8.0.18)
-- Bug Class: Assertion Failure (result_field != nullptr)
-- Defect Module: sql/item_window_func.cc
-- Reference: MySQL-BUG-97886
-- Description: Window function LEAD/LAG with default value expression having type
--              mismatch against partition column causing assertion failure during evaluation.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE telemetry_units (
    unit_id INT PRIMARY KEY,
    firmware_ver VARCHAR(32),
    installed_at DATE
);




-- Target Reproduction Scenario: Stock tick pricing table with temporal window partitions


-- Crashing Query: Window LEAD function specifying string default for numeric price column

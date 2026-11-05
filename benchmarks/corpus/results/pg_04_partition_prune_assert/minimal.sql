-- Case ID: pg_04
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: Assertion Failure (bms_is_member in partition pruning)
-- Defect Module: backend/partitioning/partprune.c
-- Reference: PG-BUG-17101
-- Description: Run-time partition pruning assertion failure on composite range partition
--              key with DEFAULT partition when evaluating parameterized OR clauses.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE inventory_warehouses (
    warehouse_id INT PRIMARY KEY,
    location_city VARCHAR(64),
    capacity_sqft INT
);




-- Target Reproduction Scenario: Range partitioned table with DEFAULT partition


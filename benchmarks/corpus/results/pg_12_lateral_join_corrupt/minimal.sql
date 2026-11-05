-- Case ID: pg_12
-- Target DBMS: PostgreSQL (>= 13.0)
-- Bug Class: Assertion Failure (bms_is_subset(lateral_relids, relids))
-- Defect Module: backend/optimizer/plan/subselect.c
-- Reference: PG-BUG-17255 / SQLancer-PG-2021-11
-- Description: Corrupt join relids in lateral reference when flattening nested subqueries
--              with multiple left outer joins.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE device_telemetry (
    reading_id INT PRIMARY KEY,
    device_sn VARCHAR(32),
    reading_val DOUBLE PRECISION
);




-- Target Reproduction Scenario: Multi-table nested lateral join relid set mismatch




-- Crashing Query: Deep nested correlated lateral subqueries with outer joins

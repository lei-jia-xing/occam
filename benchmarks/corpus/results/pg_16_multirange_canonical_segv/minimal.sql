-- Case ID: pg_16
-- Target DBMS: PostgreSQL (>= 14.0)
-- Bug Class: SIGSEGV (Invalid function pointer in canonical multirange handler)
-- Defect Module: backend/utils/adt/multirangetypes.c
-- Reference: PG-BUG-17510
-- Description: Canonical function pointer dereference crash during multirange
--              aggregation and intersection on empty multiranges.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE scheduling_slots (
    slot_id INT PRIMARY KEY,
    slot_name VARCHAR(32),
    duration_min INT
);




-- Target Reproduction Scenario: Table with multirange columns and operations


-- Crashing Query: Multirange intersection and aggregate calculation across empty ranges

-- Case ID: pg_05
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Memory Corruption / SIGSEGV
-- Defect Module: backend/utils/adt/enum.c
-- Reference: PG-BUG-15822 / SQLancer-PG-2019-12
-- Description: enum_range() invoked over reordered enum type elements added via ALTER TYPE
--              causing array indexing bounds violation in enum_range_internal.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE order_status_codes (
    code_id INT PRIMARY KEY,
    description TEXT,
    is_terminal BOOLEAN
);




-- Target Reproduction Scenario: Altered enum type with reordered member positions




-- Crashing Query: Correlated subquery evaluating enum_range with altered ordering

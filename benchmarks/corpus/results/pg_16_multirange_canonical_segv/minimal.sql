-- Case ID: pg_16
-- Target DBMS: PostgreSQL (>= 14.0)
-- Bug Class: SIGSEGV (Invalid function pointer in canonical multirange handler)
-- Defect Module: backend/utils/adt/multirangetypes.c
-- Reference: PG-BUG-17510
-- Description: Canonical function pointer dereference crash during multirange
--              aggregation and intersection on empty multiranges.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Table with multirange columns and operations
CREATE TABLE staff_schedules (
    schedule_id INT PRIMARY KEY,
    employee_id INT NOT NULL,
    busy_intervals INT4MULTIRANGE NOT NULL
);

INSERT INTO staff_schedules (schedule_id, employee_id, busy_intervals) VALUES
    (1, 101, '{[1, 5), [10, 15)}'::int4multirange),
    (2, 101, '{[3, 8), [20, 25)}'::int4multirange),
    (3, 102, '{}'::int4multirange),
    (4, 102, '{[12, 18)}'::int4multirange);

-- Crashing Query: Multirange intersection and aggregate calculation across empty ranges
SELECT
    employee_id,
    range_merge(busy_intervals) AS combined_span,
    busy_intervals::int4multirange AS intersection_slice
FROM staff_schedules

;

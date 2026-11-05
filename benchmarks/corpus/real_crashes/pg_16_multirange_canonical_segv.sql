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

CREATE TABLE shift_rotations (
    rotation_id INT PRIMARY KEY,
    team_name VARCHAR(64)
);

INSERT INTO scheduling_slots (slot_id, slot_name, duration_min) VALUES
    (1, 'morning', 240),
    (2, 'evening', 240);

INSERT INTO shift_rotations (rotation_id, team_name) VALUES
    (10, 'Site Reliability Eng'),
    (20, 'Network Ops');

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
    busy_intervals * '{[2, 12)}'::int4multirange AS intersection_slice
FROM staff_schedules
WHERE NOT isempty(busy_intervals)
ORDER BY employee_id;

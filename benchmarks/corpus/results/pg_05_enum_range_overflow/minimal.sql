-- Case ID: pg_05
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Memory Corruption / SIGSEGV
-- Defect Module: backend/utils/adt/enum.c
-- Reference: PG-BUG-15822 / SQLancer-PG-2019-12
-- Description: enum_range() invoked over reordered enum type elements added via ALTER TYPE
--              causing array indexing bounds violation in enum_range_internal.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Altered enum type with reordered member positions
CREATE TYPE task_priority AS ENUM ('low', 'medium', 'high', 'critical');

ALTER TYPE task_priority ADD VALUE 'urgent' BEFORE 'critical';

CREATE TABLE dev_tasks (
    task_id INT PRIMARY KEY,
    title VARCHAR(128) NOT NULL,
    priority task_priority NOT NULL,
    owner_id INT
);

INSERT INTO dev_tasks (task_id, title, priority, owner_id) VALUES
    (1, 'Fix auth bypass', 'critical', 101),
    (2, 'Refactor CSS', 'low', 102),
    (3, 'Security patch kernel', 'urgent', 101),
    (4, 'Update readme', 'medium', 103);

-- Crashing Query: Correlated subquery evaluating enum_range with altered ordering
SELECT
    t.task_id,
    t.title,
    t.priority,
    (
        SELECT COUNT(*)
        FROM unnest(enum_range(NULL::task_priority, t.priority)) AS r(p)
       
    ) AS prior_count
FROM dev_tasks t
;

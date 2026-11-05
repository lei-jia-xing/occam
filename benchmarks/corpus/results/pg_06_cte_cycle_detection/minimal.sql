-- Case ID: pg_06
-- Target DBMS: PostgreSQL (>= 14.0)
-- Bug Class: Assertion Failure (cycle_data != NULL)
-- Defect Module: backend/executor/nodeRecursiveunion.c
-- Reference: PG-BUG-17345
-- Description: Recursive CTE cycle detection assertion failure when combining
--              CYCLE clause with set operations and outer joins.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE project_milestones (
    milestone_id INT PRIMARY KEY,
    name VARCHAR(64),
    target_qtr VARCHAR(8)
);




-- Target Reproduction Scenario: Cyclic graph traversal with CYCLE clause




-- Crashing Query: Recursive CTE with CYCLE detection merged with LEFT OUTER JOIN

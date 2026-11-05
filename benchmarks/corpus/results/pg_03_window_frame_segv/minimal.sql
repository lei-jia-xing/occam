-- Case ID: pg_03
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: SIGSEGV (Buffer underflow in tuple spool)
-- Defect Module: backend/executor/nodeWindowAgg.c
-- Reference: PG-BUG-16298 / SQUIRREL-PG-2021-33
-- Description: Window function frame boundary calculation with RANGE BETWEEN
--              CURRENT ROW AND UNBOUNDED FOLLOWING EXCLUDE CURRENT ROW on nullable expressions.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE department_stats (
    dept_id INT PRIMARY KEY,
    headcount INT,
    budget_allocated NUMERIC(14,2)
);




-- Target Reproduction Scenario


-- Crashing Query: Window aggregation with RANGE frame excluding current row over nullable sort key

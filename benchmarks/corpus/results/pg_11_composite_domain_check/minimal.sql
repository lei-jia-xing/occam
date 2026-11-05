-- Case ID: pg_11
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Type Confusion / Memory Corruption (ExecEvalCoerceViaIO)
-- Defect Module: backend/executor/execExpr.c
-- Reference: PG-BUG-17029
-- Description: Nested domain check constraint failure on composite type attributes leading
--              to executor type confusion when evaluating CoerceViaIO in UPDATE.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE billing_codes (
    code_id INT PRIMARY KEY,
    code_name VARCHAR(32),
    rate NUMERIC(6,2)
);




-- Target Reproduction Scenario: Domains defined over composite type fields




-- Crashing Query / DML: Updating composite attribute with cast domain coercion in subquery


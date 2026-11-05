-- Case ID: pg_13
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Plan Tree Corruption (SearchSysCacheAttNum)
-- Defect Module: backend/rewrite/rewriteHandler.c
-- Reference: PG-BUG-16550
-- Description: Planner crash during query rewrite when accessing generated column whose
--              stored expression involves array slicing after column alterations.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE session_cache (
    token_hash VARCHAR(64) PRIMARY KEY,
    issued_epoch BIGINT,
    ttl_seconds INT
);




-- Target Reproduction Scenario: Table with generated column referencing array expressions


-- Schema alteration modifying column sequence

-- Crashing Query: Rewriting query that projects slice generated column with array operators

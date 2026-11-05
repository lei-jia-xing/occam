-- Case ID: pg_14
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: SIGABRT (Assertion Failure !InTransactionState)
-- Defect Module: backend/commands/tablecmds.c
-- Reference: PG-BUG-16104
-- Description: Catalog cache invalidation crash during ON COMMIT DROP temp table
--              cleanup inside aborted subtransaction savepoints.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE temp_scratch_meta (
    scratch_id INT PRIMARY KEY,
    owner_user VARCHAR(32),
    created_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);




-- Target Reproduction Scenario: Transaction with nested savepoints and ON COMMIT DROP temp tables
 
-- Crashing Query: Querying surviving temporary relation after subtransaction abort


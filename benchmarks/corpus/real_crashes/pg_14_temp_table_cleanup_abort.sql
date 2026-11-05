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

CREATE TABLE perf_counters (
    counter_id INT PRIMARY KEY,
    name VARCHAR(64),
    val BIGINT
);

INSERT INTO temp_scratch_meta (scratch_id, owner_user) VALUES
    (1, 'worker_1'),
    (2, 'worker_2');

INSERT INTO perf_counters (counter_id, name, val) VALUES
    (10, 'queries_executed', 1420);

-- Target Reproduction Scenario: Transaction with nested savepoints and ON COMMIT DROP temp tables
BEGIN;

CREATE TEMP TABLE temp_batch_work (
    id INT,
    payload TEXT
) ON COMMIT DROP;

INSERT INTO temp_batch_work VALUES (1, 'payload_alpha'), (2, 'payload_beta');

SAVEPOINT sp1;

CREATE TEMP TABLE temp_sub_batch (
    sub_id INT PRIMARY KEY,
    parent_id INT
) ON COMMIT DROP;

INSERT INTO temp_sub_batch VALUES (101, 1);

-- Abort subtransaction to trigger cleanup invalidation
ROLLBACK TO SAVEPOINT sp1;

-- Crashing Query: Querying surviving temporary relation after subtransaction abort
SELECT * FROM temp_batch_work;

COMMIT;

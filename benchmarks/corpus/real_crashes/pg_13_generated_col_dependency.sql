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

CREATE TABLE access_tokens (
    token_id INT PRIMARY KEY,
    user_id INT,
    scope VARCHAR(32)
);

INSERT INTO session_cache (token_hash, issued_epoch, ttl_seconds) VALUES
    ('abc123hash', 1600000000, 3600),
    ('def456hash', 1600001000, 7200);

INSERT INTO access_tokens (token_id, user_id, scope) VALUES
    (1, 42, 'read:reports'),
    (2, 42, 'write:orders');

-- Target Reproduction Scenario: Table with generated column referencing array expressions
CREATE TABLE sensor_streams (
    stream_id INT PRIMARY KEY,
    device_id INT NOT NULL,
    raw_measurements INT[] NOT NULL,
    first_two INT[] GENERATED ALWAYS AS (raw_measurements[1:2]) STORED
);

INSERT INTO sensor_streams (stream_id, device_id, raw_measurements) VALUES
    (1, 101, ARRAY[10, 20, 30, 40]),
    (2, 101, ARRAY[50, 60]),
    (3, 102, ARRAY[99]);

-- Schema alteration modifying column sequence
ALTER TABLE sensor_streams ADD COLUMN note TEXT DEFAULT 'uncalibrated';
ALTER TABLE sensor_streams DROP COLUMN note;

-- Crashing Query: Rewriting query that projects slice generated column with array operators
SELECT
    stream_id,
    device_id,
    first_two,
    first_two[1] AS primary_val,
    array_length(first_two, 1) AS slice_len
FROM sensor_streams
WHERE first_two && ARRAY[10, 50];

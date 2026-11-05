-- Case ID: pg_02
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: SIGSEGV / Assertion Failure
-- Defect Module: backend/access/gin/ginfast.c
-- Reference: PG-BUG-15424
-- Description: GIN fastupdate pending list corruption during concurrent inserts with
--              fastupdate enabled, leading to segfault during ginInsertCleanup.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE cluster_nodes (
    node_id INT PRIMARY KEY,
    hostname VARCHAR(128) NOT NULL,
    ip_addr VARCHAR(45) NOT NULL,
    rack_zone VARCHAR(32)
);




-- Target Reproduction Scenario




-- Flush pending list and trigger index corruption scan

-- Crashing Query: Array containment query on corrupted fastupdate GIN index

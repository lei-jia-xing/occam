-- Case ID: pg_07
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: SIGABRT / Assertion Failure
-- Defect Module: backend/access/brin/brin_inclusion.c
-- Reference: PG-BUG-16611
-- Description: BRIN inclusion operator class crash during index summarization of
--              empty and NULL ranges.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE raw_telemetry_dump (
    dump_id BIGINT PRIMARY KEY,
    ingest_node VARCHAR(32),
    raw_bytes BYTEA
);




-- Target Reproduction Scenario: BRIN index on range inclusion operator class

    

-- Trigger index summarization across empty and null range pages

-- Crashing Query: Overlap operator query on BRIN range inclusion index

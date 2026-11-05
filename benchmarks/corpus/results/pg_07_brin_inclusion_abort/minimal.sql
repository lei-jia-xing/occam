-- Case ID: pg_07
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: SIGABRT / Assertion Failure
-- Defect Module: backend/access/brin/brin_inclusion.c
-- Reference: PG-BUG-16611
-- Description: BRIN inclusion operator class crash during index summarization of
--              empty and NULL ranges.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: BRIN index on range inclusion operator class
CREATE TABLE sensor_ranges (
    record_id INT PRIMARY KEY,
    station_id INT NOT NULL,
    operating_interval INT4RANGE
);

    WITH (pages_per_range = 1);


-- Trigger index summarization across empty and null range pages
SELECT brin_summarize_new_values('idx_sensor_ranges_brin');

-- Crashing Query: Overlap operator query on BRIN range inclusion index
SELECT record_id, station_id, operating_interval
FROM sensor_ranges
WHERE operating_interval && '[15, 25)'::int4range;

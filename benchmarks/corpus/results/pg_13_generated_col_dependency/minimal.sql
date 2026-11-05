-- Case ID: pg_13
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Plan Tree Corruption (SearchSysCacheAttNum)
-- Defect Module: backend/rewrite/rewriteHandler.c
-- Reference: PG-BUG-16550
-- Description: Planner crash during query rewrite when accessing generated column whose
--              stored expression involves array slicing after column alterations.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Table with generated column referencing array expressions
CREATE TABLE sensor_streams (
    stream_id INT PRIMARY KEY,
    device_id INT NOT NULL,
    raw_measurements INT[] NOT NULL,
    first_two INT[] GENERATED ALWAYS AS raw_measurements[1:2] STORED
);


-- Schema alteration modifying column sequence

-- Crashing Query: Rewriting query that projects slice generated column with array operators
SELECT
    stream_id,
    device_id,
    first_two,
    first_two[1] AS primary_val,
    array_length(first_two, 1) AS slice_len
FROM sensor_streams
;

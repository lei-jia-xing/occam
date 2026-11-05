-- Case ID: pg_10
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: SIGSEGV (Null pointer dereference in strip_tsvector)
-- Defect Module: backend/utils/adt/tsvector_op.c
-- Reference: PG-BUG-16812
-- Description: tsvector_strip() null pointer dereference on tsvector with empty word list
--              and position weights when combined with ts_stat or tsvector concatenation.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE article_revisions (
    rev_id INT PRIMARY KEY,
    author_id INT,
    change_summary VARCHAR(128)
);




-- Target Reproduction Scenario: Weighted tsvectors with stripped positions



-- Crashing Query: tsvector_strip within expression index match and subquery aggregation

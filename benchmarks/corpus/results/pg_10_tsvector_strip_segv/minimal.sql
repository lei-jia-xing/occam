-- Case ID: pg_10
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: SIGSEGV (Null pointer dereference in strip_tsvector)
-- Defect Module: backend/utils/adt/tsvector_op.c
-- Reference: PG-BUG-16812
-- Description: tsvector_strip() null pointer dereference on tsvector with empty word list
--              and position weights when combined with ts_stat or tsvector concatenation.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Weighted tsvectors with stripped positions
CREATE TABLE corpus_articles (
    doc_id INT PRIMARY KEY,
    title VARCHAR(256),
    tsv_content TSVECTOR NOT NULL
);


INSERT INTO corpus_articles (doc_id, title, tsv_content) VALUES
    (1, 'Database internals', to_tsvector('english', 'relational database query optimization execution')),
    (2, 'Empty placeholder', ''::tsvector),
    (3, 'Weighted search', setweight(to_tsvector('english', 'kernel'), 'A')),
    (4, 'Mixed empty weights', setweight(''::tsvector, 'B'));

-- Crashing Query: tsvector_strip within expression index match and subquery aggregation
SELECT
    doc_id,
    title,
    tsvector_strip(tsv_content) AS stripped_tsv,
    ts_rank(tsv_content, to_tsquery('english', 'database | crash')) AS rank_score
FROM corpus_articles

;

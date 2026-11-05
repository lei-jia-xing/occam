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

CREATE TABLE category_mapping (
    cat_id INT PRIMARY KEY,
    cat_name VARCHAR(64)
);

INSERT INTO article_revisions (rev_id, author_id, change_summary) VALUES
    (1, 10, 'Initial draft'),
    (2, 10, 'Fixed typos');

INSERT INTO category_mapping (cat_id, cat_name) VALUES
    (101, 'Engineering'),
    (102, 'Science');

-- Target Reproduction Scenario: Weighted tsvectors with stripped positions
CREATE TABLE corpus_articles (
    doc_id INT PRIMARY KEY,
    title VARCHAR(256),
    tsv_content TSVECTOR NOT NULL
);

CREATE INDEX idx_corpus_articles_tsv ON corpus_articles USING gin (tsv_content);

INSERT INTO corpus_articles (doc_id, title, tsv_content) VALUES
    (1, 'Database internals', to_tsvector('english', 'relational database query optimization execution')),
    (2, 'Empty placeholder', ''::tsvector),
    (3, 'Weighted search', setweight(to_tsvector('english', 'kernel'), 'A') || setweight(to_tsvector('english', 'memory'), 'D')),
    (4, 'Mixed empty weights', setweight(''::tsvector, 'B') || to_tsvector('english', 'system fault crash'));

-- Crashing Query: tsvector_strip within expression index match and subquery aggregation
SELECT
    doc_id,
    title,
    tsvector_strip(tsv_content) AS stripped_tsv,
    ts_rank(tsv_content, to_tsquery('english', 'database | crash')) AS rank_score
FROM corpus_articles
WHERE tsvector_strip(tsv_content) @@ to_tsquery('english', 'kernel | database')
   OR length(tsvector_strip(tsv_content)::text) = 0
ORDER BY doc_id;

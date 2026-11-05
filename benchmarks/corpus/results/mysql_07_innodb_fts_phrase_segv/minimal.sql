-- Case ID: mysql_07
-- Target DBMS: MySQL (>= 8.0.17)
-- Bug Class: SIGSEGV (Null pointer dereference in fts_phrase_search)
-- Defect Module: storage/innobase/fts/fts0fts.cc
-- Reference: MySQL-BUG-96452
-- Description: Full-text search phrase search crash on hyphenated tokens with stopword
--              filtering enabled in boolean mode.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Table with FULLTEXT index in InnoDB
CREATE TABLE knowledge_base (
    article_id INT PRIMARY KEY,
    author_id INT NOT NULL,
    title VARCHAR(128) NOT NULL,
    content_body TEXT NOT NULL,
    FULLTEXT INDEX idx_kb_content (title, content_body)
) ENGINE=InnoDB;


-- Crashing Query: Phrase search with hyphenated terms and boolean operators
SELECT
    article_id,
    title,
    MATCH(title, content_body) AGAINST('+"in-depth" +"deep-dive"' IN BOOLEAN MODE) AS score
FROM knowledge_base

;

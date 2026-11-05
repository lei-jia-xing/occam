-- Case ID: mysql_07
-- Target DBMS: MySQL (>= 8.0.17)
-- Bug Class: SIGSEGV (Null pointer dereference in fts_phrase_search)
-- Defect Module: storage/innobase/fts/fts0fts.cc
-- Reference: MySQL-BUG-96452
-- Description: Full-text search phrase search crash on hyphenated tokens with stopword
--              filtering enabled in boolean mode.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE author_directory (
    author_id INT PRIMARY KEY,
    author_name VARCHAR(64),
    joined_date DATE
);




-- Target Reproduction Scenario: Table with FULLTEXT index in InnoDB


-- Crashing Query: Phrase search with hyphenated terms and boolean operators

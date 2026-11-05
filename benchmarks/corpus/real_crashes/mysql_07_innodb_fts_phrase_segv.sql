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

CREATE TABLE doc_categories (
    cat_id INT PRIMARY KEY,
    cat_title VARCHAR(32)
);

INSERT INTO author_directory (author_id, author_name, joined_date) VALUES
    (1, 'John Doe', '2021-01-01'),
    (2, 'Jane Smith', '2022-03-15');

INSERT INTO doc_categories (cat_id, cat_title) VALUES
    (10, 'Reference Guide'),
    (20, 'Tutorial');

-- Target Reproduction Scenario: Table with FULLTEXT index in InnoDB
CREATE TABLE knowledge_base (
    article_id INT PRIMARY KEY,
    author_id INT NOT NULL,
    title VARCHAR(128) NOT NULL,
    content_body TEXT NOT NULL,
    FULLTEXT INDEX idx_kb_content (title, content_body)
) ENGINE=InnoDB;

INSERT INTO knowledge_base (article_id, author_id, title, content_body) VALUES
    (1, 1, 'C++ Memory Safety', 'Understanding RAII, smart-pointers, and modern memory management.'),
    (2, 1, 'In-Depth Kernel Guide', 'A deep-dive into non-blocking I/O and event-driven architecture.'),
    (3, 2, 'Troubleshooting Stopwords', 'How InnoDB fulltext handles common stop words like the-and-or.'),
    (4, 2, 'Compound Words in SQL', 'Testing hyphen-separated tokens like out-of-bounds in phrase searches.');

-- Crashing Query: Phrase search with hyphenated terms and boolean operators
SELECT
    article_id,
    title,
    MATCH(title, content_body) AGAINST('+"in-depth" +"deep-dive"' IN BOOLEAN MODE) AS score
FROM knowledge_base
WHERE MATCH(title, content_body) AGAINST('+"in-depth" +"deep-dive"' IN BOOLEAN MODE)
ORDER BY score DESC;

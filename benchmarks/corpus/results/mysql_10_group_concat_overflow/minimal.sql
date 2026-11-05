-- Case ID: mysql_10
-- Target DBMS: MySQL (>= 8.0.20)
-- Bug Class: Memory Corruption / SIGSEGV (Item_func_group_concat::add)
-- Defect Module: sql/item_sum.cc
-- Reference: MySQL-BUG-99882
-- Description: GROUP_CONCAT buffer reallocation overflow and crash when sorting by
--              BLOB fields with large group_concat_max_len settings.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Table with BLOB payloads and group aggregation
CREATE TABLE asset_meta (
    meta_id INT PRIMARY KEY,
    category VARCHAR(32) NOT NULL,
    blob_chunk BLOB NOT NULL
);

INSERT INTO asset_meta (meta_id, category, blob_chunk) VALUES
    (1, 'cat_alpha', REPEAT('A', 8192)),
    (2, 'cat_alpha', REPEAT('B', 8192)),
    (3, 'cat_alpha', REPEAT('C', 8192)),
    (4, 'cat_beta',  REPEAT('X', 4096)),
    (5, 'cat_beta',  REPEAT('Y', 4096));

-- Crashing Query: GROUP_CONCAT with ORDER BY blob_chunk over distinct grouping
SELECT
    category,
    LENGTH(GROUP_CONCAT(blob_chunk SEPARATOR '|')) AS total_len,
    COUNT(*) AS chunk_count
FROM asset_meta

;

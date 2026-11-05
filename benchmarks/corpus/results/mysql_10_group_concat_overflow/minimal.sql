-- Case ID: mysql_10
-- Target DBMS: MySQL (>= 8.0.20)
-- Bug Class: Memory Corruption / SIGSEGV (Item_func_group_concat::add)
-- Defect Module: sql/item_sum.cc
-- Reference: MySQL-BUG-99882
-- Description: GROUP_CONCAT buffer reallocation overflow and crash when sorting by
--              BLOB fields with large group_concat_max_len settings.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE media_assets (
    asset_id INT PRIMARY KEY,
    mime_type VARCHAR(64),
    file_size_kb INT
);




-- Target Reproduction Scenario: Table with BLOB payloads and group aggregation


-- Crashing Query: GROUP_CONCAT with ORDER BY blob_chunk over distinct grouping

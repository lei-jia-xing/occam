-- Case ID: mysql_03
-- Target DBMS: MySQL (>= 8.0.22)
-- Bug Class: SIGSEGV (Heap out-of-bounds read in Json_path_cache)
-- Defect Module: sql/item_json_func.cc
-- Reference: MySQL-BUG-101180
-- Description: JSON_EXTRACT with multiple wildcard selectors ($.*[*]) on deeply nested
--              heterogeneous JSON documents resulting in out-of-bounds memory access.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE client_sessions (
    session_id VARCHAR(64) PRIMARY KEY,
    ip_address VARCHAR(45),
    expires_at DATETIME
);




-- Target Reproduction Scenario: Table with complex JSON structure and multi-path extraction


-- Crashing Query: Deep wildcard extraction combined with JSON_LENGTH and JSON_CONTAINS_PATH

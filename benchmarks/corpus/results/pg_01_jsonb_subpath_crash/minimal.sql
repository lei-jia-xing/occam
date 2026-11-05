-- Case ID: pg_01
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: SIGSEGV (Null pointer dereference)
-- Defect Module: backend/utils/adt/jsonpath_exec.c
-- Reference: PG-BUG-16982 / SQLancer-PG-2020-09
-- Description: Nested JSONB path traversal null pointer dereference during
--              jsonb_path_query evaluation with filter predicate and jsonb_strip_nulls.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE sys_service_config (
    config_id INT PRIMARY KEY,
    module_name VARCHAR(64) NOT NULL,
    config_payload TEXT,
    is_active BOOLEAN DEFAULT TRUE
);




-- Target Reproduction Scenario




-- Crashing Query: Deep jsonpath filter navigation traversing null objects

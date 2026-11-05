-- Case ID: pg_15
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Stack Overflow / SIGSEGV (spg_quad_box_leaf_split)
-- Defect Module: backend/access/spgist/spgquadbox.c
-- Reference: PG-BUG-16781
-- Description: SP-GiST quad-tree inner split infinite recursion / stack overflow
--              when indexing identical 2D box coordinates.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE geo_regions (
    region_id INT PRIMARY KEY,
    name VARCHAR(64)
);




-- Target Reproduction Scenario: SP-GiST index on box with degenerate identical bounding boxes


-- Insert multiple identical zero-area and degenerate boxes to trigger leaf node split recursion

-- Crashing Query: Box overlap query triggering spg_quad_box_leaf_split traversal

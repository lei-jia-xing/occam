-- Case ID: pg_15
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Stack Overflow / SIGSEGV (spg_quad_box_leaf_split)
-- Defect Module: backend/access/spgist/spgquadbox.c
-- Reference: PG-BUG-16781
-- Description: SP-GiST quad-tree inner split infinite recursion / stack overflow
--              when indexing identical 2D box coordinates.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: SP-GiST index on box with degenerate identical bounding boxes
CREATE TABLE spatial_bounding_boxes (
    box_id INT PRIMARY KEY,
    owner_id INT,
    bounding_box BOX NOT NULL
);


-- Insert multiple identical zero-area and degenerate boxes to trigger leaf node split recursion

-- Crashing Query: Box overlap query triggering spg_quad_box_leaf_split traversal
SELECT
    box_id,
    owner_id,
    bounding_box
FROM spatial_bounding_boxes
WHERE box '((-1,-1),(1,1))';

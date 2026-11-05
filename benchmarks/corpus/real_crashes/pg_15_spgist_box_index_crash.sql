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

CREATE TABLE route_waypoints (
    point_id INT PRIMARY KEY,
    lat NUMERIC(9,6),
    lon NUMERIC(9,6)
);

INSERT INTO geo_regions (region_id, name) VALUES
    (1, 'North America'),
    (2, 'Western Europe');

INSERT INTO route_waypoints (point_id, lat, lon) VALUES
    (101, 37.774929, -122.419416),
    (102, 51.507351, -0.127758);

-- Target Reproduction Scenario: SP-GiST index on box with degenerate identical bounding boxes
CREATE TABLE spatial_bounding_boxes (
    box_id INT PRIMARY KEY,
    owner_id INT,
    bounding_box BOX NOT NULL
);

CREATE INDEX idx_spatial_boxes_spgist ON spatial_bounding_boxes USING spgist (bounding_box);

-- Insert multiple identical zero-area and degenerate boxes to trigger leaf node split recursion
INSERT INTO spatial_bounding_boxes (box_id, owner_id, bounding_box) VALUES
    (1, 10, box '((0,0),(0,0))'),
    (2, 10, box '((0,0),(0,0))'),
    (3, 10, box '((0,0),(0,0))'),
    (4, 10, box '((0,0),(0,0))'),
    (5, 10, box '((0,0),(0,0))'),
    (6, 10, box '((0,0),(0,0))'),
    (7, 20, box '((10,10),(20,20))'),
    (8, 20, box '((10,10),(20,20))');

-- Crashing Query: Box overlap query triggering spg_quad_box_leaf_split traversal
SELECT
    box_id,
    owner_id,
    bounding_box
FROM spatial_bounding_boxes
WHERE bounding_box && box '((-1,-1),(1,1))';

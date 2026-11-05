-- Case ID: mysql_06
-- Target DBMS: MySQL (>= 8.0.23)
-- Bug Class: SIGSEGV (Null pointer dereference in rtr_page_split_and_insert)
-- Defect Module: storage/innobase/gis/gis0rtree.cc
-- Reference: MySQL-BUG-102555
-- Description: R-tree spatial index corruption and segfault during page split with
--              degenerate zero-area polygon geometries.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE map_tiles (
    tile_id INT PRIMARY KEY,
    zoom_level INT,
    tile_url VARCHAR(256)
);




-- Target Reproduction Scenario: SPATIAL INDEX on GEOMETRY column

-- Insert valid polygons followed by degenerate polygons (identical vertices / zero MBR)

-- Crashing Query: Spatial MBR intersection query triggering R-tree search across split pages

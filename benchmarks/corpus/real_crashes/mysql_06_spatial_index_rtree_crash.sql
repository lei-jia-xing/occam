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

CREATE TABLE gps_tracks (
    track_id INT PRIMARY KEY,
    vehicle_id INT,
    recorded_date DATE
);

INSERT INTO map_tiles (tile_id, zoom_level, tile_url) VALUES
    (1, 14, 'https://tiles.cdn.internal/14/1/2.png'),
    (2, 14, 'https://tiles.cdn.internal/14/1/3.png');

INSERT INTO gps_tracks (track_id, vehicle_id, recorded_date) VALUES
    (101, 7, '2023-11-01'),
    (102, 8, '2023-11-02');

-- Target Reproduction Scenario: SPATIAL INDEX on GEOMETRY column
CREATE TABLE geofence_zones (
    zone_id INT PRIMARY KEY,
    zone_name VARCHAR(64) NOT NULL,
    boundary GEOMETRY NOT NULL /*!80003 SRID 0 */,
    SPATIAL INDEX idx_geofence_boundary (boundary)
) ENGINE=InnoDB;

-- Insert valid polygons followed by degenerate polygons (identical vertices / zero MBR)
INSERT INTO geofence_zones (zone_id, zone_name, boundary) VALUES
    (1, 'Terminal Zone', ST_GeomFromText('POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))')),
    (2, 'Depot Zone', ST_GeomFromText('POLYGON((20 20, 20 30, 30 30, 30 20, 20 20))')),
    (3, 'Degenerate Zero Area', ST_GeomFromText('POLYGON((5 5, 5 5, 5 5, 5 5, 5 5))')),
    (4, 'Collinear Segment', ST_GeomFromText('POLYGON((0 0, 0 5, 0 10, 0 5, 0 0))'));

-- Crashing Query: Spatial MBR intersection query triggering R-tree search across split pages
SELECT
    zone_id,
    zone_name,
    ST_AsText(boundary) AS boundary_wkt
FROM geofence_zones
WHERE MBRIntersects(boundary, ST_GeomFromText('POLYGON((4 4, 4 6, 6 6, 6 4, 4 4))'));

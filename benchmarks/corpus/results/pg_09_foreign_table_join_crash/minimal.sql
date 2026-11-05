-- Case ID: pg_09
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Plan Tree Corruption / SIGSEGV (reparameterize_path)
-- Defect Module: backend/optimizer/path/joinpath.c
-- Reference: PG-BUG-17488 / SQLancer-PG-2022-01
-- Description: Planner crash during path reparameterization for lateral joins involving
--              partitioned tables and outer join grouping sets.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE cluster_topology (
    topo_id INT PRIMARY KEY,
    region VARCHAR(32),
    is_primary BOOLEAN
);




-- Target Reproduction Scenario: Partitioned parent/child tables with grouping sets and lateral join


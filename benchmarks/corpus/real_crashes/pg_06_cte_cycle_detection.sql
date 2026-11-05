-- Case ID: pg_06
-- Target DBMS: PostgreSQL (>= 14.0)
-- Bug Class: Assertion Failure (cycle_data != NULL)
-- Defect Module: backend/executor/nodeRecursiveunion.c
-- Reference: PG-BUG-17345
-- Description: Recursive CTE cycle detection assertion failure when combining
--              CYCLE clause with set operations and outer joins.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE project_milestones (
    milestone_id INT PRIMARY KEY,
    name VARCHAR(64),
    target_qtr VARCHAR(8)
);

CREATE TABLE resource_tags (
    tag_id INT PRIMARY KEY,
    tag_label VARCHAR(32)
);

INSERT INTO project_milestones (milestone_id, name, target_qtr) VALUES
    (1, 'Alpha Release', 'Q1-2024'),
    (2, 'Beta Release', 'Q2-2024');

INSERT INTO resource_tags (tag_id, tag_label) VALUES
    (101, 'critical-path'),
    (102, 'experimental');

-- Target Reproduction Scenario: Cyclic graph traversal with CYCLE clause
CREATE TABLE graph_nodes (
    node_id INT PRIMARY KEY,
    label TEXT
);

CREATE TABLE graph_edges (
    source_id INT REFERENCES graph_nodes(node_id),
    target_id INT REFERENCES graph_nodes(node_id),
    cost INT,
    PRIMARY KEY (source_id, target_id)
);

INSERT INTO graph_nodes (node_id, label) VALUES
    (1, 'Node A'), (2, 'Node B'), (3, 'Node C'), (4, 'Node D');

INSERT INTO graph_edges (source_id, target_id, cost) VALUES
    (1, 2, 10),
    (2, 3, 20),
    (3, 1, 30),
    (3, 4, 15);

-- Crashing Query: Recursive CTE with CYCLE detection merged with LEFT OUTER JOIN
WITH RECURSIVE traverse(src, dst, depth, total_cost) AS (
    SELECT source_id, target_id, 1, cost
    FROM graph_edges
    WHERE source_id = 1
    UNION
    SELECT e.source_id, e.target_id, t.depth + 1, t.total_cost + e.cost
    FROM graph_edges e
    JOIN traverse t ON e.source_id = t.dst
    WHERE t.depth < 10
)
CYCLE dst SET is_cycle USING path_array
SELECT
    t.src,
    t.dst,
    t.depth,
    t.total_cost,
    t.is_cycle,
    gn.label AS destination_label
FROM traverse t
LEFT JOIN graph_nodes gn ON t.dst = gn.node_id
WHERE t.is_cycle = FALSE OR t.depth <= 3;

-- Case ID: pg_06
-- Target DBMS: PostgreSQL (>= 14.0)
-- Bug Class: Assertion Failure (cycle_data != NULL)
-- Defect Module: backend/executor/nodeRecursiveunion.c
-- Reference: PG-BUG-17345
-- Description: Recursive CTE cycle detection assertion failure when combining
--              CYCLE clause with set operations and outer joins.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Cyclic graph traversal with CYCLE clause

CREATE TABLE graph_edges (
    source_id INT REFERENCES graph_nodes(node_id),
    target_id INT REFERENCES graph_nodes(node_id),
    cost INT,
    PRIMARY KEY (source_id, target_id)
);



-- Crashing Query: Recursive CTE with CYCLE detection merged with LEFT OUTER JOIN
WITH RECURSIVE traverse(src, dst, depth, total_cost) AS (
    SELECT source_id, target_id, 1, cost
    FROM graph_edges
   
    UNION
    SELECT e.source_id, e.target_id, t.depth, t.total_cost
    FROM graph_edges e
    JOIN traverse t ON e.source_id
   
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
LEFT JOIN graph_nodes gn ON t.dst
;

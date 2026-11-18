-- Seed 04: Multi-Table Diamond DAG Dependency Structure
-- Tests: Diamond graph Def-Use chains (Root -> Left/Right -> Sink),
-- foreign key integrity, and cascaded multi-way joins.

CREATE TABLE cluster_root (
    root_id INT PRIMARY KEY,
    cluster_name VARCHAR(64) NOT NULL
);

CREATE TABLE cluster_node_left (
    left_id INT PRIMARY KEY,
    root_id INT NOT NULL REFERENCES cluster_root(root_id) ON DELETE CASCADE,
    left_score INT NOT NULL
);

CREATE TABLE cluster_node_right (
    right_id INT PRIMARY KEY,
    root_id INT NOT NULL REFERENCES cluster_root(root_id) ON DELETE CASCADE,
    right_weight NUMERIC(8, 2) NOT NULL
);

CREATE TABLE cluster_sink (
    sink_id INT PRIMARY KEY,
    left_id INT NOT NULL REFERENCES cluster_node_left(left_id),
    right_id INT NOT NULL REFERENCES cluster_node_right(right_id),
    verdict VARCHAR(32) NOT NULL
);

INSERT INTO cluster_root (root_id, cluster_name) VALUES
    (1, 'east-cluster'),
    (2, 'west-cluster');

INSERT INTO cluster_node_left (left_id, root_id, left_score) VALUES
    (10, 1, 95),
    (20, 2, 40);

INSERT INTO cluster_node_right (right_id, root_id, right_weight) VALUES
    (100, 1, 250.50),
    (200, 2, 80.00);

INSERT INTO cluster_sink (sink_id, left_id, right_id, verdict) VALUES
    (1001, 10, 100, 'passed'),
    (1002, 20, 200, 'quarantined');

SELECT
    s.sink_id,
    r.cluster_name,
    l.left_score,
    rg.right_weight,
    s.verdict
FROM cluster_sink s
JOIN cluster_node_left l ON s.left_id = l.left_id
JOIN cluster_node_right rg ON s.right_id = rg.right_id
JOIN cluster_root r ON l.root_id = r.root_id
WHERE l.left_score > 50 AND rg.right_weight > 100.0;

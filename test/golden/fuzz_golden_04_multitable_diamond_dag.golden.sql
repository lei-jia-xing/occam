-- Golden Scenario 04: Multi-Table Diamond DAG Dependency Stress

-- Focus: Preserving all four nodes of a diamond Def-Use relation while slicing away dead chains

-- Target: diamond_root, diamond_branch_left, diamond_branch_right, diamond_sink













































CREATE TABLE diamond_root (root_id INT PRIMARY KEY, cluster_name VARCHAR(64));

CREATE TABLE diamond_branch_left (left_id INT PRIMARY KEY, root_id INT REFERENCES diamond_root(root_id), left_score INT);

CREATE TABLE diamond_branch_right (right_id INT PRIMARY KEY, root_id INT REFERENCES diamond_root(root_id), right_weight NUMERIC(8,2));

CREATE TABLE diamond_sink (sink_id INT PRIMARY KEY, left_id INT REFERENCES diamond_branch_left(left_id), right_id INT REFERENCES diamond_branch_right(right_id), summary_note TEXT);

INSERT INTO diamond_root (root_id, cluster_name) VALUES (1, 'cluster_alpha'), (2, 'cluster_beta');

INSERT INTO diamond_branch_left (left_id, root_id, left_score) VALUES (10, 1, 85), (20, 2, 42);

INSERT INTO diamond_branch_right (right_id, root_id, right_weight) VALUES (100, 1, 150.50), (200, 2, 88.00);

INSERT INTO diamond_sink (sink_id, left_id, right_id, summary_note) VALUES (1001, 10, 100, 'node_alpha_verified'), (1002, 20, 200, 'node_beta_pending');

UPDATE diamond_branch_left SET left_score = 90;

SELECT
    s.sink_id,
    r.cluster_name,
    l.left_score,
    rg.right_weight
FROM diamond_sink s
JOIN diamond_branch_left l ON s.left_id
JOIN diamond_branch_right rg ON s.right_id
JOIN diamond_root r ON l.root_id
;

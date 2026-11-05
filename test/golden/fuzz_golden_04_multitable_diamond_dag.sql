-- Golden Scenario 04: Multi-Table Diamond DAG Dependency Stress

-- Focus: Preserving all four nodes of a diamond Def-Use relation while slicing away dead chains

-- Target: diamond_root, diamond_branch_left, diamond_branch_right, diamond_sink



CREATE TABLE dead_chain_x (x_id INT PRIMARY KEY, x_name TEXT);

CREATE TABLE dead_chain_y (y_id INT PRIMARY KEY, x_id INT REFERENCES dead_chain_x(x_id), y_val INT);

CREATE TABLE dead_chain_z (z_id INT PRIMARY KEY, y_id INT REFERENCES dead_chain_y(y_id), z_meta TEXT);

CREATE TABLE dead_audit_a (audit_id INT PRIMARY KEY, tag VARCHAR(32));

CREATE TABLE dead_audit_b (b_id INT PRIMARY KEY, audit_id INT REFERENCES dead_audit_a(audit_id));

CREATE TABLE dead_scratchpad (id INT PRIMARY KEY, note TEXT);

CREATE INDEX idx_dead_x_name ON dead_chain_x (x_name);

CREATE VIEW v_dead_chain AS SELECT x.x_id, y.y_val FROM dead_chain_x x JOIN dead_chain_y y ON x.x_id = y.x_id;

INSERT INTO dead_chain_x (x_id, x_name) VALUES (1, 'name_1');

INSERT INTO dead_chain_y (y_id, x_id, y_val) VALUES (101, 1, 10);

INSERT INTO dead_chain_z (z_id, y_id, z_meta) VALUES (1001, 101, 'meta_1');

INSERT INTO dead_chain_x (x_id, x_name) VALUES (2, 'name_2');

INSERT INTO dead_chain_y (y_id, x_id, y_val) VALUES (102, 2, 20);

INSERT INTO dead_chain_z (z_id, y_id, z_meta) VALUES (1002, 102, 'meta_2');

INSERT INTO dead_chain_x (x_id, x_name) VALUES (3, 'name_3');

INSERT INTO dead_chain_y (y_id, x_id, y_val) VALUES (103, 3, 30);

INSERT INTO dead_chain_z (z_id, y_id, z_meta) VALUES (1003, 103, 'meta_3');

INSERT INTO dead_chain_x (x_id, x_name) VALUES (4, 'name_4');

INSERT INTO dead_chain_y (y_id, x_id, y_val) VALUES (104, 4, 40);

INSERT INTO dead_chain_z (z_id, y_id, z_meta) VALUES (1004, 104, 'meta_4');

INSERT INTO dead_chain_x (x_id, x_name) VALUES (5, 'name_5');

INSERT INTO dead_chain_y (y_id, x_id, y_val) VALUES (105, 5, 50);

INSERT INTO dead_chain_z (z_id, y_id, z_meta) VALUES (1005, 105, 'meta_5');

INSERT INTO dead_chain_x (x_id, x_name) VALUES (6, 'name_6');

INSERT INTO dead_chain_y (y_id, x_id, y_val) VALUES (106, 6, 60);

INSERT INTO dead_chain_z (z_id, y_id, z_meta) VALUES (1006, 106, 'meta_6');

INSERT INTO dead_chain_x (x_id, x_name) VALUES (7, 'name_7');

INSERT INTO dead_chain_y (y_id, x_id, y_val) VALUES (107, 7, 70);

INSERT INTO dead_chain_z (z_id, y_id, z_meta) VALUES (1007, 107, 'meta_7');

INSERT INTO dead_chain_x (x_id, x_name) VALUES (8, 'name_8');

INSERT INTO dead_chain_y (y_id, x_id, y_val) VALUES (108, 8, 80);

INSERT INTO dead_chain_z (z_id, y_id, z_meta) VALUES (1008, 108, 'meta_8');

UPDATE dead_chain_y SET y_val = y_val + 5 WHERE y_id = 101;

DELETE FROM dead_scratchpad WHERE id = 1;

UPDATE dead_chain_y SET y_val = y_val + 5 WHERE y_id = 102;

DELETE FROM dead_scratchpad WHERE id = 2;

UPDATE dead_chain_y SET y_val = y_val + 5 WHERE y_id = 103;

DELETE FROM dead_scratchpad WHERE id = 3;

UPDATE dead_chain_y SET y_val = y_val + 5 WHERE y_id = 104;

DELETE FROM dead_scratchpad WHERE id = 4;

UPDATE dead_chain_y SET y_val = y_val + 5 WHERE y_id = 105;

DELETE FROM dead_scratchpad WHERE id = 5;

CREATE TABLE diamond_root (root_id INT PRIMARY KEY, cluster_name VARCHAR(64));

CREATE TABLE diamond_branch_left (left_id INT PRIMARY KEY, root_id INT REFERENCES diamond_root(root_id), left_score INT);

CREATE TABLE diamond_branch_right (right_id INT PRIMARY KEY, root_id INT REFERENCES diamond_root(root_id), right_weight NUMERIC(8,2));

CREATE TABLE diamond_sink (sink_id INT PRIMARY KEY, left_id INT REFERENCES diamond_branch_left(left_id), right_id INT REFERENCES diamond_branch_right(right_id), summary_note TEXT);

INSERT INTO diamond_root (root_id, cluster_name) VALUES (1, 'cluster_alpha'), (2, 'cluster_beta');

INSERT INTO diamond_branch_left (left_id, root_id, left_score) VALUES (10, 1, 85), (20, 2, 42);

INSERT INTO diamond_branch_right (right_id, root_id, right_weight) VALUES (100, 1, 150.50), (200, 2, 88.00);

INSERT INTO diamond_sink (sink_id, left_id, right_id, summary_note) VALUES (1001, 10, 100, 'node_alpha_verified'), (1002, 20, 200, 'node_beta_pending');

UPDATE diamond_branch_left SET left_score = 90 WHERE left_id = 10;

SELECT
    s.sink_id,
    r.cluster_name,
    l.left_score,
    rg.right_weight
FROM diamond_sink s
JOIN diamond_branch_left l ON s.left_id = l.left_id
JOIN diamond_branch_right rg ON s.right_id = rg.right_id
JOIN diamond_root r ON l.root_id = r.root_id
WHERE l.left_score > 50 AND rg.right_weight < 500.0;

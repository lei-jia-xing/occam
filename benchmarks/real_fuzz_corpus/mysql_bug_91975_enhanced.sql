-- Authentic MySQL Bug #91975 Enhanced with Multi-Table Fuzz Noise
-- [Noise Group 1: Auxiliary Tables]
CREATE TABLE app_config (k VARCHAR(32) PRIMARY KEY, v TEXT);
INSERT INTO app_config VALUES ('env', 'production'), ('version', '8.0');
CREATE TABLE temp_worker (wid INT PRIMARY KEY, task_name VARCHAR(64));
INSERT INTO temp_worker VALUES (1, 'indexer'), (2, 'cleaner');

-- [Crash Core: Target Base Table t1]
CREATE TABLE t1(c1 TIME, c2 CHAR(1), c3 INT(1), c4 VARCHAR(1) PRIMARY KEY, c5 TIME UNIQUE KEY, c6 NUMERIC(0,0) DEFAULT 3.141592);
INSERT INTO t1 VALUES ('12:00:00', 'a', 1, 'x', '12:01:00', 3);

-- [Global State Barrier]
SET @@global.super_read_only = TRUE;

-- [Noise Group 2: Intermediate Temporary Tables]
CREATE TEMPORARY TABLE t_noise(id INT PRIMARY KEY, label VARCHAR(20));
INSERT INTO t_noise VALUES (10, 'scratch');

-- [Crash Core: Temporary Shadow Table & Rename Collision]
CREATE TEMPORARY TABLE t1(i INT, b BLOB, PRIMARY KEY pk(i), INDEX sk(b(1)));
CREATE TEMPORARY TABLE t3(f INT);
RENAME TABLE t1 TO t3;

-- Authentic SQUIRREL Bug #99421 Enhanced with Multi-Table Fuzz Noise
-- [Noise Group 1: Users & System Catalog]
CREATE TABLE users (id INT PRIMARY KEY, name VARCHAR(50), active BOOLEAN DEFAULT TRUE);
INSERT INTO users VALUES (1, 'alice', TRUE), (2, 'bob', FALSE);
CREATE TABLE sessions (sid INT PRIMARY KEY, uid INT, token VARCHAR(64), FOREIGN KEY (uid) REFERENCES users(id));
INSERT INTO sessions VALUES (101, 1, 'tok_abc'), (102, 2, 'tok_xyz');
CREATE INDEX idx_sessions_uid ON sessions(uid);

-- [Crash Core: Target Table v0 with Virtual Index Dependencies]
CREATE TABLE v0 ( v1 INT, extra_col VARCHAR(32) DEFAULT 'noise' ) ;
INSERT INTO v0 ( v1, extra_col ) VALUES ( 44, 'seed' ) ;
UPDATE v0 SET v1 = 21 WHERE 62 > -128 = ( NOT ( v1 IS NULL ) ) ;
INSERT INTO v0 ( v1, extra_col ) VALUES ( 0, 'data1' ) , ( 82, 'data2' ) ;

-- [Noise Group 2: E-commerce Products & Audits]
CREATE TABLE products (pid INT PRIMARY KEY, price DECIMAL(10,2), stock INT);
INSERT INTO products VALUES (1, 99.99, 10), (2, 19.50, 50);
CREATE TABLE audit_log (log_id INT PRIMARY KEY, msg TEXT);
INSERT INTO audit_log VALUES (1, 'initial catalog loaded');

-- [Crash Trigger: Complex Multi-Column Expression Index]
CREATE INDEX v2 ON v0 ( ( v1 = 127 ) , ( v1 + v1 ) , v1 , ( v1 + - -1 ) , ( v1 > 26 ) ) ;

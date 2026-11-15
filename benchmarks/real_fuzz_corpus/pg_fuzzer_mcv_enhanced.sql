-- Enhanced multi-statement fuzz crash case based on authentic SQLancer bug
-- Incorporates schema and data redundancy in accordance with thesis Section 6.1:
-- "在已经较短的真实用例中插入无关的建表、插数、查询语句，并在相关语句里加入冗余列与表达式，形成已知核心的样例。"

-- [Redundancy Group 1: Users & Orders Subsystem]
CREATE TABLE app_users (user_id INT PRIMARY KEY, username VARCHAR(64) NOT NULL, is_active BOOLEAN DEFAULT TRUE);
INSERT INTO app_users VALUES (1, 'alice', TRUE), (2, 'bob', FALSE), (3, 'charlie', TRUE);
CREATE TABLE orders (order_id INT PRIMARY KEY, user_id INT, amount NUMERIC(10, 2), status VARCHAR(20));
INSERT INTO orders VALUES (101, 1, 99.50, 'completed'), (102, 1, 150.00, 'pending'), (103, 2, 45.00, 'cancelled');
CREATE VIEW active_users_summary AS SELECT user_id, COUNT(*) AS total_orders FROM orders GROUP BY user_id;
ALTER TABLE orders ADD COLUMN note TEXT DEFAULT 'standard delivery';
UPDATE app_users SET is_active = FALSE WHERE username = 'bob';
SELECT u.username, o.amount FROM app_users u LEFT JOIN orders o ON u.user_id = o.user_id WHERE u.is_active = TRUE;

-- [Redundancy Group 2: Sensor Telemetry & Indexing]
CREATE TABLE sensor_readings (device_id INT, temp_celsius FLOAT, recorded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP);
INSERT INTO sensor_readings (device_id, temp_celsius) VALUES (10, 23.4), (11, 24.1), (12, 19.8);
CREATE INDEX idx_sensor_temp ON sensor_readings(temp_celsius);
SELECT AVG(temp_celsius) FROM sensor_readings WHERE temp_celsius > 20.0;

-- [Redundancy Group 3: Inventory Subsystem & Mutation]
CREATE TABLE inventory_items (sku VARCHAR(32) PRIMARY KEY, stock_count INT CHECK (stock_count >= 0));
INSERT INTO inventory_items VALUES ('SKU-001', 50), ('SKU-002', 12);
DELETE FROM orders WHERE status = 'cancelled';

-- [Authentic Crash Core with Redundant Attributes & Intermediate Probing]
CREATE TABLE t0 (c0 boolean, c1 boolean, c2 boolean, c_extra INT DEFAULT 42);
INSERT INTO t0 VALUES (FALSE, FALSE, FALSE, 100);
INSERT INTO t0 VALUES (TRUE, FALSE, TRUE, 200);
CREATE STATISTICS s0 ON c0, c2 FROM t0;
SELECT COUNT(*) FROM app_users WHERE user_id IN (SELECT user_id FROM orders);
ANALYZE;

-- [Trigger Query]
SELECT * FROM t0 WHERE t0.c2 OR t0.c1 OR t0.c0;

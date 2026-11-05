-- Golden Scenario 01: High-Entropy CTEs & Subqueries Chaos Stress

-- Focus: Unreferenced tables pruning through complex CTE aliases & nested subqueries

-- Target: target_customers, target_orders



CREATE TABLE dead_dim_user (user_id INT PRIMARY KEY, username VARCHAR(64), email VARCHAR(128));

CREATE TABLE dead_dim_geo (geo_id INT PRIMARY KEY, country VARCHAR(32), city VARCHAR(32));

CREATE TABLE dead_fact_clicks (click_id INT PRIMARY KEY, user_id INT, url TEXT, clicked_at INT);

CREATE TABLE dead_fact_impressions (imp_id INT PRIMARY KEY, user_id INT, banner_id INT);

CREATE TABLE dead_sys_logs (log_id INT PRIMARY KEY, severity VARCHAR(16), message TEXT);

CREATE TABLE dead_metric_aggregates (metric_id INT PRIMARY KEY, metric_name VARCHAR(64), val NUMERIC(12,4));

CREATE TABLE dead_session_tokens (token_id INT PRIMARY KEY, token VARCHAR(64), expires_at INT);

CREATE TABLE dead_audit_trails (audit_id INT PRIMARY KEY, actor VARCHAR(64), action VARCHAR(32));

CREATE INDEX idx_dead_clicks_user ON dead_fact_clicks (user_id);

CREATE INDEX idx_dead_geo_country ON dead_dim_geo (country);

CREATE VIEW v_dead_user_clicks AS SELECT u.username, c.url FROM dead_dim_user u JOIN dead_fact_clicks c ON u.user_id = c.user_id;

CREATE VIEW v_dead_logs AS SELECT log_id, severity FROM dead_sys_logs WHERE severity = 'ERROR';

INSERT INTO dead_dim_user (user_id, username, email) VALUES (1, 'user_1', 'user_1@example.com');

INSERT INTO dead_fact_clicks (click_id, user_id, url, clicked_at) VALUES (101, 1, 'https://example.com/1', 1600000001);

INSERT INTO dead_sys_logs (log_id, severity, message) VALUES (1001, 'WARN', 'Warning log payload 1');

INSERT INTO dead_dim_user (user_id, username, email) VALUES (2, 'user_2', 'user_2@example.com');

INSERT INTO dead_fact_clicks (click_id, user_id, url, clicked_at) VALUES (102, 2, 'https://example.com/2', 1600000002);

INSERT INTO dead_sys_logs (log_id, severity, message) VALUES (1002, 'WARN', 'Warning log payload 2');

INSERT INTO dead_dim_user (user_id, username, email) VALUES (3, 'user_3', 'user_3@example.com');

INSERT INTO dead_fact_clicks (click_id, user_id, url, clicked_at) VALUES (103, 3, 'https://example.com/3', 1600000003);

INSERT INTO dead_sys_logs (log_id, severity, message) VALUES (1003, 'WARN', 'Warning log payload 3');

INSERT INTO dead_dim_user (user_id, username, email) VALUES (4, 'user_4', 'user_4@example.com');

INSERT INTO dead_fact_clicks (click_id, user_id, url, clicked_at) VALUES (104, 4, 'https://example.com/4', 1600000004);

INSERT INTO dead_sys_logs (log_id, severity, message) VALUES (1004, 'WARN', 'Warning log payload 4');

INSERT INTO dead_dim_user (user_id, username, email) VALUES (5, 'user_5', 'user_5@example.com');

INSERT INTO dead_fact_clicks (click_id, user_id, url, clicked_at) VALUES (105, 5, 'https://example.com/5', 1600000005);

INSERT INTO dead_sys_logs (log_id, severity, message) VALUES (1005, 'WARN', 'Warning log payload 5');

INSERT INTO dead_dim_user (user_id, username, email) VALUES (6, 'user_6', 'user_6@example.com');

INSERT INTO dead_fact_clicks (click_id, user_id, url, clicked_at) VALUES (106, 6, 'https://example.com/6', 1600000006);

INSERT INTO dead_sys_logs (log_id, severity, message) VALUES (1006, 'WARN', 'Warning log payload 6');

INSERT INTO dead_dim_user (user_id, username, email) VALUES (7, 'user_7', 'user_7@example.com');

INSERT INTO dead_fact_clicks (click_id, user_id, url, clicked_at) VALUES (107, 7, 'https://example.com/7', 1600000007);

INSERT INTO dead_sys_logs (log_id, severity, message) VALUES (1007, 'WARN', 'Warning log payload 7');

INSERT INTO dead_dim_user (user_id, username, email) VALUES (8, 'user_8', 'user_8@example.com');

INSERT INTO dead_fact_clicks (click_id, user_id, url, clicked_at) VALUES (108, 8, 'https://example.com/8', 1600000008);

INSERT INTO dead_sys_logs (log_id, severity, message) VALUES (1008, 'WARN', 'Warning log payload 8');

INSERT INTO dead_dim_user (user_id, username, email) VALUES (9, 'user_9', 'user_9@example.com');

INSERT INTO dead_fact_clicks (click_id, user_id, url, clicked_at) VALUES (109, 9, 'https://example.com/9', 1600000009);

INSERT INTO dead_sys_logs (log_id, severity, message) VALUES (1009, 'WARN', 'Warning log payload 9');

UPDATE dead_dim_user SET email = 'updated_1@example.com' WHERE user_id = 1;

DELETE FROM dead_fact_clicks WHERE click_id = 101;

UPDATE dead_dim_user SET email = 'updated_2@example.com' WHERE user_id = 2;

DELETE FROM dead_fact_clicks WHERE click_id = 102;

UPDATE dead_dim_user SET email = 'updated_3@example.com' WHERE user_id = 3;

DELETE FROM dead_fact_clicks WHERE click_id = 103;

UPDATE dead_dim_user SET email = 'updated_4@example.com' WHERE user_id = 4;

DELETE FROM dead_fact_clicks WHERE click_id = 104;

UPDATE dead_dim_user SET email = 'updated_5@example.com' WHERE user_id = 5;

DELETE FROM dead_fact_clicks WHERE click_id = 105;

CREATE TABLE target_customers (customer_id INT PRIMARY KEY, customer_name VARCHAR(64), tier VARCHAR(16));

CREATE TABLE target_orders (order_id INT PRIMARY KEY, customer_id INT REFERENCES target_customers(customer_id), order_total NUMERIC(10,2), order_status VARCHAR(24));

INSERT INTO target_customers (customer_id, customer_name, tier) VALUES (1, 'Alice Smith', 'VIP'), (2, 'Bob Jones', 'STANDARD'), (3, 'Charlie Brown', 'VIP');

INSERT INTO target_orders (order_id, customer_id, order_total, order_status) VALUES (101, 1, 1500.00, 'COMPLETED'), (102, 1, 850.00, 'COMPLETED'), (103, 2, 45.00, 'COMPLETED'), (104, 3, 2200.00, 'COMPLETED');

UPDATE target_orders SET order_total = 2500.00 WHERE order_id = 104;

WITH regional_summary AS (
    SELECT customer_id, SUM(order_total) AS total_spent, COUNT(*) AS order_cnt
    FROM target_orders
    WHERE order_status = 'COMPLETED'
    GROUP BY customer_id
),
vip_customers AS (
    SELECT c.customer_id, c.customer_name, rs.total_spent
    FROM target_customers c
    JOIN regional_summary rs ON c.customer_id = rs.customer_id
    WHERE c.tier = 'VIP' AND rs.total_spent > 1000.00
)
SELECT
    vc.customer_id,
    vc.customer_name,
    (SELECT MAX(order_total) FROM target_orders o WHERE o.customer_id = vc.customer_id) AS max_single_order
FROM vip_customers vc
WHERE vc.total_spent > (SELECT AVG(order_total) * 2 FROM target_orders);

-- Golden Scenario 01: High-Entropy CTEs & Subqueries Chaos Stress

-- Focus: Unreferenced tables pruning through complex CTE aliases & nested subqueries

-- Target: target_customers, target_orders




















































CREATE TABLE target_customers (customer_id INT PRIMARY KEY, customer_name VARCHAR(64), tier VARCHAR(16));

CREATE TABLE target_orders (order_id INT PRIMARY KEY, customer_id INT REFERENCES target_customers(customer_id), order_total NUMERIC(10,2), order_status VARCHAR(24));

INSERT INTO target_customers (customer_id, customer_name, tier) VALUES (1, 'Alice Smith', 'VIP'), (2, 'Bob Jones', 'STANDARD'), (3, 'Charlie Brown', 'VIP');

INSERT INTO target_orders (order_id, customer_id, order_total, order_status) VALUES (101, 1, 1500.00, 'COMPLETED'), (102, 1, 850.00, 'COMPLETED'), (103, 2, 45.00, 'COMPLETED'), (104, 3, 2200.00, 'COMPLETED');

UPDATE target_orders SET order_total = 2500.00;

WITH regional_summary AS (
    SELECT customer_id, SUM(order_total) AS total_spent, COUNT(*) AS order_cnt
    FROM target_orders
   
   
),
vip_customers AS (
    SELECT c.customer_id, c.customer_name, rs.total_spent
    FROM target_customers c
    JOIN regional_summary rs ON c.customer_id
   
)
SELECT
    vc.customer_id,
    vc.customer_name,
    (SELECT MAX(order_total) FROM target_orders o) AS max_single_order
FROM vip_customers vc
;

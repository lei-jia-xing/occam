-- Case ID: mysql_02
-- Target DBMS: MySQL (>= 8.0.21)
-- Bug Class: Assertion Failure (thd->is_error() || !table->is_derived())
-- Defect Module: sql/sql_resolver.cc
-- Reference: MySQL-BUG-99234 / SQLancer-MYSQL-2020-05
-- Description: Crash in derived table merge optimization when subquery contains GROUP BY
--              and outer query contains ORDER BY with LIMIT and JOIN.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Derived tables with aggregation merged under optimizer switch
CREATE TABLE customers (
    customer_id INT PRIMARY KEY,
    region_code VARCHAR(8) NOT NULL,
    tier VARCHAR(16) NOT NULL
);

CREATE TABLE orders (
    order_id INT PRIMARY KEY,
    customer_id INT NOT NULL,
    total_amount DECIMAL(10,2) NOT NULL,
    order_status VARCHAR(16) NOT NULL
);

INSERT INTO customers (customer_id, region_code, tier) VALUES
    (1, 'NA', 'gold'),
    (2, 'NA', 'silver'),
    (3, 'EU', 'gold'),
    (4, 'APAC', 'bronze');

INSERT INTO orders (order_id, customer_id, total_amount, order_status) VALUES
    (101, 1, 350.00, 'completed'),
    (102, 1, 120.00, 'completed'),
    (103, 2, 85.00, 'pending'),
    (104, 3, 490.00, 'completed'),
    (105, 4, 25.00, 'refunded');

-- Crashing Query: Derived merge with outer sort and correlated group by
SELECT
    dt.customer_id,
    dt.avg_spent,
    c.region_code
FROM (
    SELECT o.customer_id, AVG(o.total_amount) AS avg_spent
    FROM orders o
   
   
) dt
JOIN customers c ON dt.customer_id

;

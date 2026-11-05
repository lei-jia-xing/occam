-- Case ID: mysql_12
-- Target DBMS: MySQL (>= 8.0.20)
-- Bug Class: SIGSEGV (Invalid pointer in HashJoinRowBuffer::StoreRow)
-- Defect Module: sql/hash_join_buffer.cc
-- Reference: MySQL-BUG-101902
-- Description: Hash join memory buffer spill crash when build table contains
--              variable-length nullable JSON and long string columns.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Tables with large variable-width rows joined via HASH JOIN
CREATE TABLE customers_large (
    cust_id INT PRIMARY KEY,
    company_name VARCHAR(255) NOT NULL,
    profile_data JSON
);

CREATE TABLE orders_large (
    ord_id INT PRIMARY KEY,
    cust_id INT NOT NULL,
    notes TEXT,
    order_val DECIMAL(12,2)
);

INSERT INTO customers_large (cust_id, company_name, profile_data) VALUES
    (1, 'Global Logistics Inc', '{"tier": "enterprise", "features": ["api", "audit", "sso"]}'),
    (2, 'Metro Retail LLC', '{"tier": "starter", "features": []}'),
    (3, 'Peak Analytics Corp', NULL);

INSERT INTO orders_large (ord_id, cust_id, notes, order_val) VALUES
    (101, 1, REPEAT('Order confirmation notes. ', 20), 4500.00),
    (102, 1, REPEAT('Expedited shipping request. ', 15), 1200.00),
    (201, 2, 'Standard freight delivery.', 350.00),
    (301, 3, REPEAT('Custom packaging specifications. ', 25), 8900.00);

-- Crashing Query: Hash join forcing disk spill buffer allocation on JSON/TEXT projections
SELECT
    c.cust_id,
    c.company_name,
    JSON_EXTRACT(c.profile_data, '$.tier') AS cust_tier,
    o.ord_id,
    o.order_val,
    LEFT(o.notes, 50) AS snippet
FROM customers_large c
JOIN orders_large o ON c.cust_id

;

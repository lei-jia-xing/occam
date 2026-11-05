-- Case ID: mysql_08
-- Target DBMS: MySQL (>= 8.0.21)
-- Bug Class: Assertion Failure (!tab->materialized || tab->is_materialized())
-- Defect Module: sql/sql_optimizer.cc
-- Reference: MySQL-BUG-100800 / SQLancer-MYSQL-2020-09
-- Description: Assertion failure during subquery materialization when outer query
--              contains GROUP BY ... WITH ROLLUP and HAVING clauses.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE sales_targets (
    target_id INT PRIMARY KEY,
    fiscal_year INT,
    quarter_num INT,
    target_usd DECIMAL(12,2)
);

CREATE TABLE region_lookup (
    region_id INT PRIMARY KEY,
    region_name VARCHAR(32)
);

INSERT INTO sales_targets (target_id, fiscal_year, quarter_num, target_usd) VALUES
    (1, 2023, 1, 1000000.00),
    (2, 2023, 2, 1200000.00);

INSERT INTO region_lookup (region_id, region_name) VALUES
    (10, 'North America'),
    (20, 'Europe');

-- Target Reproduction Scenario: Invoices and line items with rollup aggregation
CREATE TABLE invoices (
    invoice_id INT PRIMARY KEY,
    region_id INT NOT NULL,
    sales_rep_id INT NOT NULL,
    invoice_amount DECIMAL(10,2) NOT NULL
);

CREATE TABLE invoice_discounts (
    discount_id INT PRIMARY KEY,
    region_id INT NOT NULL,
    min_qualifying_amount DECIMAL(10,2) NOT NULL
);

INSERT INTO invoices (invoice_id, region_id, sales_rep_id, invoice_amount) VALUES
    (1, 10, 101, 5000.00),
    (2, 10, 101, 7500.00),
    (3, 10, 102, 3200.00),
    (4, 20, 201, 12000.00),
    (5, 20, 202, 4500.00);

INSERT INTO invoice_discounts (discount_id, region_id, min_qualifying_amount) VALUES
    (1, 10, 4000.00),
    (2, 20, 10000.00);

-- Crashing Query: ROLLUP query with correlated materialized IN subquery in HAVING
SELECT
    i.region_id,
    i.sales_rep_id,
    SUM(i.invoice_amount) AS total_sales
FROM invoices i
WHERE i.region_id IN (
    SELECT d.region_id
    FROM invoice_discounts d
    WHERE d.min_qualifying_amount <= i.invoice_amount
)
GROUP BY i.region_id, i.sales_rep_id WITH ROLLUP
HAVING total_sales > 5000.00;

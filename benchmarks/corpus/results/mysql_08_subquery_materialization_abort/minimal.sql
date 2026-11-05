-- Case ID: mysql_08
-- Target DBMS: MySQL (>= 8.0.21)
-- Bug Class: Assertion Failure (!tab->materialized || tab->is_materialized())
-- Defect Module: sql/sql_optimizer.cc
-- Reference: MySQL-BUG-100800 / SQLancer-MYSQL-2020-09
-- Description: Assertion failure during subquery materialization when outer query
--              contains GROUP BY ... WITH ROLLUP and HAVING clauses.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




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



-- Crashing Query: ROLLUP query with correlated materialized IN subquery in HAVING

.sales_rep_id WITH ROLLUP
;

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




-- Target Reproduction Scenario: Invoices and line items with rollup aggregation




-- Crashing Query: ROLLUP query with correlated materialized IN subquery in HAVING

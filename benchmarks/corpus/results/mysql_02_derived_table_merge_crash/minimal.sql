-- Case ID: mysql_02
-- Target DBMS: MySQL (>= 8.0.21)
-- Bug Class: Assertion Failure (thd->is_error() || !table->is_derived())
-- Defect Module: sql/sql_resolver.cc
-- Reference: MySQL-BUG-99234 / SQLancer-MYSQL-2020-05
-- Description: Crash in derived table merge optimization when subquery contains GROUP BY
--              and outer query contains ORDER BY with LIMIT and JOIN.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE order_items_archive (
    archive_id INT PRIMARY KEY,
    order_id INT,
    archived_at DATETIME
);




-- Target Reproduction Scenario: Derived tables with aggregation merged under optimizer switch




-- Crashing Query: Derived merge with outer sort and correlated group by

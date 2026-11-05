-- Case ID: mysql_01
-- Target DBMS: MySQL (>= 8.0.19)
-- Bug Class: SIGSEGV (Use-after-free of Query_expression during CTE materialization)
-- Defect Module: sql/sql_lex.cc / sql/sql_cte.cc
-- Reference: MySQL-BUG-98512 / SQUIRREL-MYSQL-2020
-- Description: Memory corruption / SIGSEGV in CTE union materialization with window aggregates
--              when references are evaluated across multiple subqueries.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE sys_logs (
    log_id INT PRIMARY KEY,
    source VARCHAR(64),
    payload TEXT
);




-- Target Reproduction Scenario: Multi-branch CTE with UNION and window functions




-- Crashing Query: Materialized CTE union with window ranking joined back to base tables

-- Case ID: mysql_01
-- Target DBMS: MySQL (>= 8.0.19)
-- Bug Class: SIGSEGV (Use-after-free of Query_expression during CTE materialization)
-- Defect Module: sql/sql_lex.cc / sql/sql_cte.cc
-- Reference: MySQL-BUG-98512 / SQUIRREL-MYSQL-2020
-- Description: Memory corruption / SIGSEGV in CTE union materialization with window aggregates
--              when references are evaluated across multiple subqueries.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Multi-branch CTE with UNION and window functions
CREATE TABLE sales_pipeline (
    opportunity_id INT PRIMARY KEY,
    rep_id INT NOT NULL,
    deal_value DECIMAL(12,2) NOT NULL,
    stage VARCHAR(32) NOT NULL
);




-- Crashing Query: Materialized CTE union with window ranking joined back to base tables
WITH cte_pipeline AS (
    SELECT rep_id, deal_value, stage,
           ROW_NUMBER() OVER (PARTITION BY rep_id) AS rank_in_rep
    FROM sales_pipeline
   
    UNION
    SELECT rep_id, estimated_arr, 'prospect',
           1 AS rank_in_rep
    FROM crm_leads
)
SELECT
    cp.rep_id,
    cp.stage,
    SUM(cp.deal_value) AS aggregate_value,
    COUNT(*) AS cnt
FROM cte_pipeline cp


;

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

CREATE TABLE app_events (
    event_id INT PRIMARY KEY,
    event_type VARCHAR(32),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO sys_logs (log_id, source, payload) VALUES
    (1, 'httpd', 'status=200 path=/index.html'),
    (2, 'sshd', 'status=401 user=root');

INSERT INTO app_events (event_id, event_type) VALUES
    (101, 'user_login'),
    (102, 'password_change');

-- Target Reproduction Scenario: Multi-branch CTE with UNION and window functions
CREATE TABLE sales_pipeline (
    opportunity_id INT PRIMARY KEY,
    rep_id INT NOT NULL,
    deal_value DECIMAL(12,2) NOT NULL,
    stage VARCHAR(32) NOT NULL
);

CREATE TABLE crm_leads (
    lead_id INT PRIMARY KEY,
    rep_id INT NOT NULL,
    estimated_arr DECIMAL(12,2) NOT NULL
);

INSERT INTO sales_pipeline (opportunity_id, rep_id, deal_value, stage) VALUES
    (1, 10, 50000.00, 'closed_won'),
    (2, 10, 15000.00, 'in_progress'),
    (3, 20, 80000.00, 'closed_won'),
    (4, 20, 12000.00, 'discovery');

INSERT INTO crm_leads (lead_id, rep_id, estimated_arr) VALUES
    (101, 10, 25000.00),
    (102, 20, 45000.00);

-- Crashing Query: Materialized CTE union with window ranking joined back to base tables
WITH cte_pipeline AS (
    SELECT rep_id, deal_value, stage,
           ROW_NUMBER() OVER (PARTITION BY rep_id ORDER BY deal_value DESC) AS rank_in_rep
    FROM sales_pipeline
    WHERE stage = 'closed_won'
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
WHERE cp.rank_in_rep = 1
GROUP BY cp.rep_id, cp.stage
ORDER BY aggregate_value DESC;

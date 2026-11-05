-- Benchmark 02: MySQL Recursive Common Table Expression (CTE) & Window Functions Crash Scenario
-- Target DBMS: MySQL (>= 8.0)
-- Defect Type: Optimizer crash during recursive CTE query evaluation with multiple window function frames
-- Description: Recursive CTE navigating hierarchical organizational units, joined with staff and sales
--              tables, computing ROW_NUMBER, DENSE_RANK, SUM, and AVG window aggregates.
--              Includes multiple interleaved unrelated DML statements testing noise elimination.

-- ============================================================================
-- Setup: Target Hierarchy & Performance Schema Tables
-- ============================================================================

CREATE TABLE org_units (
    unit_id INT PRIMARY KEY,
    parent_unit_id INT,
    unit_name VARCHAR(64) NOT NULL,
    budget DECIMAL(12,2) NOT NULL
);

-- Interleaved Unrelated DDL 1
CREATE TABLE app_session_log (
    session_id INT PRIMARY KEY,
    ip_addr VARCHAR(45) NOT NULL,
    user_agent VARCHAR(128),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE staff_members (
    staff_id INT PRIMARY KEY,
    unit_id INT NOT NULL,
    full_name VARCHAR(64) NOT NULL,
    role VARCHAR(32) NOT NULL,
    salary DECIMAL(10,2) NOT NULL
);

-- Interleaved Unrelated DDL 2
CREATE TABLE inventory_stock (
    sku VARCHAR(32) PRIMARY KEY,
    bin_id VARCHAR(16) NOT NULL,
    count INT NOT NULL DEFAULT 0
);

CREATE TABLE sales_pipeline (
    deal_id INT PRIMARY KEY,
    staff_id INT NOT NULL,
    deal_value DECIMAL(12,2) NOT NULL,
    stage VARCHAR(32) NOT NULL,
    closed_at DATE NOT NULL
);

-- Interleaved Unrelated DDL 3
CREATE TABLE billing_invoices (
    invoice_id INT PRIMARY KEY,
    client_name VARCHAR(64) NOT NULL,
    total_amount DECIMAL(10,2) NOT NULL,
    is_settled BOOLEAN DEFAULT FALSE
);

-- ============================================================================
-- Data Population with Interleaved Unrelated DML
-- ============================================================================

INSERT INTO org_units (unit_id, parent_unit_id, unit_name, budget) VALUES
    (1, NULL, 'Global Enterprise', 10000000.00),
    (10, 1, 'Engineering Division', 5000000.00),
    (20, 1, 'Commercial Operations', 3500000.00),
    (101, 10, 'Cloud Infrastructure', 2500000.00),
    (102, 10, 'Data Platform', 2500000.00),
    (201, 20, 'Enterprise Sales', 2000000.00),
    (202, 20, 'Customer Success', 1500000.00);

-- Unrelated DML Interleaving 1
INSERT INTO app_session_log (session_id, ip_addr, user_agent) VALUES
    (10001, '192.168.1.10', 'Mozilla/5.0 Chrome/118'),
    (10002, '192.168.1.15', 'curl/7.88.1');

INSERT INTO staff_members (staff_id, unit_id, full_name, role, salary) VALUES
    (1001, 101, 'Martin Fowler', 'Principal Architect', 185000.00),
    (1002, 101, 'Linus Torvalds', 'Distinguished Engineer', 210000.00),
    (1003, 102, 'Grace Hopper', 'Data Director', 195000.00),
    (2001, 201, 'Alice Johnson', 'Senior Account Executive', 120000.00),
    (2002, 201, 'Bob Sterling', 'Account Executive', 95000.00),
    (2003, 202, 'Carol Danvers', 'Customer Success Manager', 90000.00);

-- Unrelated DML Interleaving 2
INSERT INTO inventory_stock (sku, bin_id, count) VALUES
    ('SKU-SRV-01', 'BAY-1', 40),
    ('SKU-CAB-09', 'BAY-4', 120);

UPDATE inventory_stock SET count = count - 5 WHERE sku = 'SKU-SRV-01';

INSERT INTO sales_pipeline (deal_id, staff_id, deal_value, stage, closed_at) VALUES
    (50001, 2001, 150000.00, 'WON', '2026-03-15'),
    (50002, 2001, 85000.00, 'WON', '2026-03-20'),
    (50003, 2002, 240000.00, 'WON', '2026-03-22'),
    (50004, 2002, 45000.00, 'LOST', '2026-03-25'),
    (50005, 2003, 30000.00, 'WON', '2026-03-28'),
    (50006, 2001, 500000.00, 'WON', '2026-04-01');

-- Unrelated DML Interleaving 3
INSERT INTO billing_invoices (invoice_id, client_name, total_amount, is_settled) VALUES
    (7001, 'Acme Corp', 150000.00, TRUE),
    (7002, 'Globex Inc', 85000.00, FALSE);

UPDATE billing_invoices SET is_settled = TRUE WHERE invoice_id = 7002;
DELETE FROM app_session_log WHERE session_id = 10001;

-- ============================================================================
-- Crashing Query: Recursive Common Table Expression with Window Aggregations
-- ============================================================================

WITH RECURSIVE org_hierarchy AS (
    SELECT
        unit_id,
        parent_unit_id,
        unit_name,
        budget,
        1 AS hierarchy_level,
        CAST(unit_name AS CHAR(1024)) AS full_path
    FROM org_units
    WHERE parent_unit_id IS NULL
    UNION ALL
    SELECT
        c.unit_id,
        c.parent_unit_id,
        c.unit_name,
        c.budget,
        p.hierarchy_level + 1,
        CONCAT(p.full_path, ' > ', c.unit_name)
    FROM org_units c
    INNER JOIN org_hierarchy p ON c.parent_unit_id = p.unit_id
)
SELECT
    h.unit_id,
    h.unit_name,
    h.hierarchy_level,
    s.full_name,
    d.deal_value,
    ROW_NUMBER() OVER (PARTITION BY h.unit_id ORDER BY d.deal_value DESC) AS deal_rank_in_unit,
    DENSE_RANK() OVER (ORDER BY h.hierarchy_level) AS level_rank,
    SUM(d.deal_value) OVER (PARTITION BY s.staff_id) AS rep_total_revenue,
    AVG(d.deal_value) OVER (PARTITION BY h.unit_id) AS unit_avg_deal
FROM org_hierarchy h
JOIN staff_members s ON h.unit_id = s.unit_id
JOIN sales_pipeline d ON s.staff_id = d.staff_id
WHERE h.hierarchy_level <= 4
  AND d.stage = 'WON'
ORDER BY h.hierarchy_level, deal_rank_in_unit;

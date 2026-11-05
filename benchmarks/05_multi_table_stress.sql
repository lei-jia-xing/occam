-- Benchmark 05: Multi-Table Dependency Stress & Transaction Fence Scenario
-- Target DBMS: PostgreSQL / MySQL / ANSI SQL
-- Defect Type: Complex multi-table dependency resolution failure across transactional barriers
-- Statement Count: Exactly 25 statements
-- Features: 5 tables, foreign keys, views, transaction blocks (BEGIN...COMMIT), edge-case expressions.

-- ============================================================================
-- Statements 1 to 5: 5 Schema Tables with Foreign Key Constraints
-- ============================================================================

-- Statement 1
CREATE TABLE departments (
    dept_id INT PRIMARY KEY,
    dept_code VARCHAR(16) UNIQUE NOT NULL,
    dept_name TEXT NOT NULL,
    annual_budget NUMERIC(14,2) NOT NULL
);

-- Statement 2
CREATE TABLE employees (
    emp_id INT PRIMARY KEY,
    dept_id INT NOT NULL REFERENCES departments(dept_id),
    full_name TEXT NOT NULL,
    salary NUMERIC(10,2) NOT NULL,
    commission_rate NUMERIC(4,3) DEFAULT 0.000,
    is_active BOOLEAN DEFAULT TRUE
);

-- Statement 3
CREATE TABLE projects (
    proj_id INT PRIMARY KEY,
    proj_code VARCHAR(32) UNIQUE NOT NULL,
    title TEXT NOT NULL,
    risk_rating INT DEFAULT 1,
    budget_cap NUMERIC(12,2) NOT NULL
);

-- Statement 4
CREATE TABLE task_assignments (
    task_id INT PRIMARY KEY,
    emp_id INT NOT NULL REFERENCES employees(emp_id),
    proj_id INT NOT NULL REFERENCES projects(proj_id),
    logged_hours NUMERIC(8,2) NOT NULL DEFAULT 0.00,
    billing_rate NUMERIC(8,2) NOT NULL
);

-- Statement 5
CREATE TABLE system_journal (
    journal_id INT PRIMARY KEY,
    target_entity VARCHAR(32) NOT NULL,
    entity_key INT NOT NULL,
    event_payload TEXT NOT NULL,
    severity_level INT DEFAULT 0
);

-- ============================================================================
-- Statements 6 & 7: 2 Views Over Interdependent Relations
-- ============================================================================

-- Statement 6
CREATE VIEW dept_burn_rate AS
SELECT
    d.dept_id,
    d.dept_name,
    d.annual_budget,
    COALESCE(SUM(e.salary * (1.0 + e.commission_rate)), 0.0) AS total_payroll
FROM departments d
LEFT JOIN employees e ON d.dept_id = e.dept_id
GROUP BY d.dept_id, d.dept_name, d.annual_budget;

-- Statement 7
CREATE VIEW project_cost_summary AS
SELECT
    p.proj_id,
    p.proj_code,
    p.risk_rating,
    p.budget_cap,
    COALESCE(SUM(t.logged_hours * t.billing_rate), 0.0) AS accrued_cost
FROM projects p
LEFT JOIN task_assignments t ON p.proj_id = t.proj_id
GROUP BY p.proj_id, p.proj_code, p.risk_rating, p.budget_cap;

-- ============================================================================
-- Statements 8 to 10: Initial Base Table Population
-- ============================================================================

-- Statement 8
INSERT INTO departments (dept_id, dept_code, dept_name, annual_budget) VALUES
    (1, 'DEP-ENG', 'Core Platform Engineering', 1200000.00),
    (2, 'DEP-SEC', 'Information Security', 600000.00),
    (3, 'DEP-OPS', 'Site Reliability Operations', 800000.00);

-- Statement 9
INSERT INTO employees (emp_id, dept_id, full_name, salary, commission_rate, is_active) VALUES
    (101, 1, 'Devin Ross', 140000.00, 0.050, TRUE),
    (102, 1, 'Elena Zhao', 155000.00, 0.000, TRUE),
    (103, 2, 'Marcus Vance', 130000.00, 0.025, TRUE),
    (104, 3, 'Sarah Connor', 125000.00, 0.000, FALSE);

-- Statement 10
INSERT INTO projects (proj_id, proj_code, title, risk_rating, budget_cap) VALUES
    (501, 'PRJ-TITAN', 'Distributed Consensus Engine', 5, 450000.00),
    (502, 'PRJ-AEGIS', 'Zero Trust Perimeter', 3, 200000.00),
    (503, 'PRJ-ORBIT', 'Multi-Region Replication', 4, 350000.00);

-- ============================================================================
-- Statements 11 to 14: Transaction Block 1 (Fenced Population)
-- ============================================================================

-- Statement 11
BEGIN;

-- Statement 12
INSERT INTO task_assignments (task_id, emp_id, proj_id, logged_hours, billing_rate) VALUES
    (1001, 101, 501, 160.00, 125.00),
    (1002, 102, 501, 120.00, 140.00),
    (1003, 103, 502, 80.00, 110.00);

-- Statement 13
INSERT INTO system_journal (journal_id, target_entity, entity_key, event_payload, severity_level) VALUES
    (9001, 'task_assignments', 1001, 'Allocated Devin to PRJ-TITAN', 1),
    (9002, 'task_assignments', 1002, 'Allocated Elena to PRJ-TITAN', 1);

-- Statement 14
COMMIT;

-- ============================================================================
-- Statements 15 to 17: Interleaved Updates & Journaling
-- ============================================================================

-- Statement 15
UPDATE employees SET salary = salary * 1.08 WHERE dept_id = 1 AND is_active = TRUE;

-- Statement 16
INSERT INTO task_assignments (task_id, emp_id, proj_id, logged_hours, billing_rate) VALUES
    (1004, 102, 503, 65.00, 145.00);

-- Statement 17
INSERT INTO system_journal (journal_id, target_entity, entity_key, event_payload, severity_level) VALUES
    (9003, 'task_assignments', 1004, 'Allocated Elena to PRJ-ORBIT', 1);

-- ============================================================================
-- Statements 18 to 22: Transaction Block 2 (Risk Escalation)
-- ============================================================================

-- Statement 18
BEGIN;

-- Statement 19
UPDATE projects SET risk_rating = risk_rating + 1 WHERE proj_id = 501;

-- Statement 20
UPDATE task_assignments SET logged_hours = logged_hours + 40.00 WHERE task_id = 1001;

-- Statement 21
INSERT INTO system_journal (journal_id, target_entity, entity_key, event_payload, severity_level) VALUES
    (9004, 'projects', 501, 'Escalated risk rating on PRJ-TITAN', 2);

-- Statement 22
COMMIT;

-- ============================================================================
-- Statements 23 to 25: Maintenance, Billing Adjustment & Final Crashing Query
-- ============================================================================

-- Statement 23
DELETE FROM system_journal WHERE severity_level = 0;

-- Statement 24
UPDATE task_assignments SET billing_rate = billing_rate * 1.05 WHERE logged_hours > 100.00;

-- Statement 25: Stress Query with Complex Edge-Case Arithmetic and Conditionals
SELECT
    d.dept_code,
    e.full_name,
    p.proj_code,
    t.logged_hours,
    t.billing_rate,
    (t.logged_hours * t.billing_rate) AS total_charge,
    (t.logged_hours * t.billing_rate) / NULLIF(d.annual_budget / 1000.0, 0.0) AS budget_impact_per_mille,
    COALESCE(e.salary / NULLIF(t.logged_hours, 0.0), 0.0) AS effective_hourly_cost,
    CASE
        WHEN (p.risk_rating * t.logged_hours) >= 800.0 THEN 'CRITICAL_EXPOSURE'
        WHEN (p.risk_rating * t.logged_hours) >= 400.0 THEN 'MODERATE_EXPOSURE'
        ELSE 'CONTROLLED'
    END AS risk_classification,
    (e.emp_id % 7 = 0 OR NOT (e.commission_rate IS NULL)) AS special_audit_flag
FROM departments d
JOIN employees e ON d.dept_id = e.dept_id
JOIN task_assignments t ON e.emp_id = t.emp_id
JOIN projects p ON t.proj_id = p.proj_id
WHERE e.is_active = TRUE
  AND (d.annual_budget > 500000.00 OR p.risk_rating >= 4)
  AND (t.logged_hours * t.billing_rate) > 5000.00
ORDER BY budget_impact_per_mille DESC, total_charge DESC;

-- Case ID: pg_03
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: SIGSEGV (Buffer underflow in tuple spool)
-- Defect Module: backend/executor/nodeWindowAgg.c
-- Reference: PG-BUG-16298 / SQUIRREL-PG-2021-33
-- Description: Window function frame boundary calculation with RANGE BETWEEN
--              CURRENT ROW AND UNBOUNDED FOLLOWING EXCLUDE CURRENT ROW on nullable expressions.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario
CREATE TABLE financial_ledger (
    entry_id INT PRIMARY KEY,
    account_id INT NOT NULL,
    amount NUMERIC(12,2) NOT NULL,
    effective_date DATE,
    reconciliation_code VARCHAR(16)
);

INSERT INTO financial_ledger (entry_id, account_id, amount, effective_date, reconciliation_code) VALUES
    (1, 101, 1500.00, '2023-01-10', 'REC-A'),
    (2, 101, -300.00, NULL, 'REC-B'),
    (3, 101, 450.50, '2023-01-15', NULL),
    (4, 101, -200.00, NULL, 'REC-C'),
    (5, 102, 2200.00, '2023-02-01', 'REC-A'),
    (6, 102, -800.00, '2023-02-05', 'REC-B');

-- Crashing Query: Window aggregation with RANGE frame excluding current row over nullable sort key
SELECT
    entry_id,
    account_id,
    effective_date,
    amount,
    SUM(amount) OVER (
        PARTITION BY account_id
       
        RANGE BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING
        EXCLUDE CURRENT ROW
    ) AS forward_rolling_sum,
    COUNT(*) OVER (
        PARTITION BY account_id
       
        RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
        EXCLUDE GROUP
    ) AS group_filtered_count
FROM financial_ledger;

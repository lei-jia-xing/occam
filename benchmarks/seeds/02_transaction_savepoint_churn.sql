-- Seed 02: Transaction Fences, Savepoints & Schema Mutation Churn
-- Tests: BEGIN/COMMIT transaction boundaries, SAVEPOINT, ROLLBACK TO SAVEPOINT,
-- and DDL schema alterations (ADD/DROP/RENAME COLUMN) inside/around transactions.

CREATE TABLE account_ledger (
    account_id INT PRIMARY KEY,
    owner_name VARCHAR(64) NOT NULL,
    balance NUMERIC(14, 2) NOT NULL,
    status VARCHAR(16) NOT NULL
);

INSERT INTO account_ledger (account_id, owner_name, balance, status) VALUES
    (101, 'Acme Corp', 50000.00, 'active'),
    (102, 'Beta LLC', 12500.00, 'active');

BEGIN;

UPDATE account_ledger SET balance = balance - 5000.00 WHERE account_id = 101;

SAVEPOINT transfer_checkpoint;

UPDATE account_ledger SET balance = balance + 5000.00 WHERE account_id = 102;

-- Partial rollback to verify reducer preserves fence states
ROLLBACK TO SAVEPOINT transfer_checkpoint;

UPDATE account_ledger SET balance = balance + 4800.00 WHERE account_id = 102;

COMMIT;

ALTER TABLE account_ledger ADD COLUMN risk_tier VARCHAR(8) DEFAULT 'standard';

UPDATE account_ledger SET risk_tier = 'premium' WHERE balance >= 40000.00;

SELECT account_id, owner_name, balance, risk_tier
FROM account_ledger
WHERE status = 'active' AND balance > 15000.00;

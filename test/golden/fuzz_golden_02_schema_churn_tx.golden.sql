-- Golden Scenario 02: Transactional Churn & Schema Mutation Stress

-- Focus: Preserving transaction fences, savepoint boundaries, and required ALTER TABLE columns

-- Target: tx_account_ledger




































BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (101, 'tx_scratch_1');
SAVEPOINT sp_batch_1;
UPDATE dead_churn_tbl SET val = val;
ROLLBACK TO SAVEPOINT sp_batch_1;
COMMIT;

BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (102, 'tx_scratch_2');
SAVEPOINT sp_batch_2;
UPDATE dead_churn_tbl SET val = val;
ROLLBACK TO SAVEPOINT sp_batch_2;
COMMIT;

BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (103, 'tx_scratch_3');
SAVEPOINT sp_batch_3;
UPDATE dead_churn_tbl SET val = val;
ROLLBACK TO SAVEPOINT sp_batch_3;
COMMIT;

BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (104, 'tx_scratch_4');
SAVEPOINT sp_batch_4;
UPDATE dead_churn_tbl SET val = val;
ROLLBACK TO SAVEPOINT sp_batch_4;
COMMIT;

BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (105, 'tx_scratch_5');
SAVEPOINT sp_batch_5;
UPDATE dead_churn_tbl SET val = val;
ROLLBACK TO SAVEPOINT sp_batch_5;
COMMIT;











CREATE TABLE tx_account_ledger (account_id INT PRIMARY KEY, balance NUMERIC(14,2) NOT NULL, status VARCHAR(20));

ALTER TABLE tx_account_ledger ADD COLUMN branch_code VARCHAR(16);


BEGIN;
INSERT INTO tx_account_ledger (account_id, balance, status, branch_code) VALUES (101, 5000.00, 'OPEN', 'BR_NORTH');
INSERT INTO tx_account_ledger (account_id, balance, status, branch_code) VALUES (102, 12000.00, 'OPEN', 'BR_SOUTH');
COMMIT;

BEGIN;
UPDATE tx_account_ledger SET balance = balance;
SAVEPOINT sp_tx_audit;
UPDATE tx_account_ledger SET status = 'FLAGGED';
ROLLBACK TO SAVEPOINT sp_tx_audit;
COMMIT;

SELECT account_id, balance, status, branch_code FROM tx_account_ledger WHERE balance < 10000.00;

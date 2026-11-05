-- Golden Scenario 02: Transactional Churn & Schema Mutation Stress

-- Focus: Preserving transaction fences, savepoint boundaries, and required ALTER TABLE columns

-- Target: tx_account_ledger



CREATE TEMP TABLE temp_stage_a (stage_id INT PRIMARY KEY, payload TEXT);

CREATE TABLE dead_churn_tbl (id INT PRIMARY KEY, tag VARCHAR(32), val INT);

CREATE TABLE dead_churn_extra (id INT PRIMARY KEY, extra_data TEXT);

CREATE INDEX idx_dead_churn_tag ON dead_churn_tbl (tag);

ALTER TABLE dead_churn_tbl ADD COLUMN extra_note TEXT;

ALTER TABLE dead_churn_tbl DROP COLUMN extra_note;

INSERT INTO temp_stage_a (stage_id, payload) VALUES (1, 'payload_init_1');

INSERT INTO dead_churn_tbl (id, tag, val) VALUES (1, 'tag_1', 100);

INSERT INTO dead_churn_extra (id, extra_data) VALUES (1, 'extra_payload_1');

INSERT INTO temp_stage_a (stage_id, payload) VALUES (2, 'payload_init_2');

INSERT INTO dead_churn_tbl (id, tag, val) VALUES (2, 'tag_2', 200);

INSERT INTO dead_churn_extra (id, extra_data) VALUES (2, 'extra_payload_2');

INSERT INTO temp_stage_a (stage_id, payload) VALUES (3, 'payload_init_3');

INSERT INTO dead_churn_tbl (id, tag, val) VALUES (3, 'tag_3', 300);

INSERT INTO dead_churn_extra (id, extra_data) VALUES (3, 'extra_payload_3');

INSERT INTO temp_stage_a (stage_id, payload) VALUES (4, 'payload_init_4');

INSERT INTO dead_churn_tbl (id, tag, val) VALUES (4, 'tag_4', 400);

INSERT INTO dead_churn_extra (id, extra_data) VALUES (4, 'extra_payload_4');

INSERT INTO temp_stage_a (stage_id, payload) VALUES (5, 'payload_init_5');

INSERT INTO dead_churn_tbl (id, tag, val) VALUES (5, 'tag_5', 500);

INSERT INTO dead_churn_extra (id, extra_data) VALUES (5, 'extra_payload_5');

INSERT INTO temp_stage_a (stage_id, payload) VALUES (6, 'payload_init_6');

INSERT INTO dead_churn_tbl (id, tag, val) VALUES (6, 'tag_6', 600);

INSERT INTO dead_churn_extra (id, extra_data) VALUES (6, 'extra_payload_6');

INSERT INTO temp_stage_a (stage_id, payload) VALUES (7, 'payload_init_7');

INSERT INTO dead_churn_tbl (id, tag, val) VALUES (7, 'tag_7', 700);

INSERT INTO dead_churn_extra (id, extra_data) VALUES (7, 'extra_payload_7');

INSERT INTO temp_stage_a (stage_id, payload) VALUES (8, 'payload_init_8');

INSERT INTO dead_churn_tbl (id, tag, val) VALUES (8, 'tag_8', 800);

INSERT INTO dead_churn_extra (id, extra_data) VALUES (8, 'extra_payload_8');

INSERT INTO temp_stage_a (stage_id, payload) VALUES (9, 'payload_init_9');

INSERT INTO dead_churn_tbl (id, tag, val) VALUES (9, 'tag_9', 900);

INSERT INTO dead_churn_extra (id, extra_data) VALUES (9, 'extra_payload_9');

BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (101, 'tx_scratch_1');
SAVEPOINT sp_batch_1;
UPDATE dead_churn_tbl SET val = val + 1 WHERE id = 1;
ROLLBACK TO SAVEPOINT sp_batch_1;
COMMIT;

BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (102, 'tx_scratch_2');
SAVEPOINT sp_batch_2;
UPDATE dead_churn_tbl SET val = val + 1 WHERE id = 2;
ROLLBACK TO SAVEPOINT sp_batch_2;
COMMIT;

BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (103, 'tx_scratch_3');
SAVEPOINT sp_batch_3;
UPDATE dead_churn_tbl SET val = val + 1 WHERE id = 3;
ROLLBACK TO SAVEPOINT sp_batch_3;
COMMIT;

BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (104, 'tx_scratch_4');
SAVEPOINT sp_batch_4;
UPDATE dead_churn_tbl SET val = val + 1 WHERE id = 4;
ROLLBACK TO SAVEPOINT sp_batch_4;
COMMIT;

BEGIN;
INSERT INTO temp_stage_a (stage_id, payload) VALUES (105, 'tx_scratch_5');
SAVEPOINT sp_batch_5;
UPDATE dead_churn_tbl SET val = val + 1 WHERE id = 5;
ROLLBACK TO SAVEPOINT sp_batch_5;
COMMIT;

UPDATE dead_churn_tbl SET tag = 'churn_1' WHERE id = 1;

DELETE FROM temp_stage_a WHERE stage_id = 1;

UPDATE dead_churn_tbl SET tag = 'churn_2' WHERE id = 2;

DELETE FROM temp_stage_a WHERE stage_id = 2;

UPDATE dead_churn_tbl SET tag = 'churn_3' WHERE id = 3;

DELETE FROM temp_stage_a WHERE stage_id = 3;

UPDATE dead_churn_tbl SET tag = 'churn_4' WHERE id = 4;

DELETE FROM temp_stage_a WHERE stage_id = 4;

UPDATE dead_churn_tbl SET tag = 'churn_5' WHERE id = 5;

DELETE FROM temp_stage_a WHERE stage_id = 5;

CREATE TABLE tx_account_ledger (account_id INT PRIMARY KEY, balance NUMERIC(14,2) NOT NULL, status VARCHAR(20));

ALTER TABLE tx_account_ledger ADD COLUMN branch_code VARCHAR(16);

CREATE INDEX idx_tx_ledger_branch ON tx_account_ledger (branch_code);

BEGIN;
INSERT INTO tx_account_ledger (account_id, balance, status, branch_code) VALUES (101, 5000.00, 'OPEN', 'BR_NORTH');
INSERT INTO tx_account_ledger (account_id, balance, status, branch_code) VALUES (102, 12000.00, 'OPEN', 'BR_SOUTH');
COMMIT;

BEGIN;
UPDATE tx_account_ledger SET balance = balance - 150.00 WHERE account_id = 101;
SAVEPOINT sp_tx_audit;
UPDATE tx_account_ledger SET status = 'FLAGGED' WHERE account_id = 102;
ROLLBACK TO SAVEPOINT sp_tx_audit;
COMMIT;

SELECT account_id, balance, status, branch_code FROM tx_account_ledger WHERE status = 'FLAGGED' OR balance < 10000.00;

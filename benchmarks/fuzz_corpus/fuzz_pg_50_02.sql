-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: POSTGRES

-- Statements: 50

-- AST Expression Depth: 12

-- Injected Defect Case: pg_03

-- Seed: 5012



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSONB
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 JSONB,
    col_3 VARCHAR(64),
    col_4 VARCHAR(64)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 BOOLEAN,
    col_3 INT,
    col_4 INT
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 VARCHAR(64),
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

CREATE TABLE financial_ledger (
    entry_id INT PRIMARY KEY,
    account_id INT NOT NULL,
    amount NUMERIC(12,2) NOT NULL,
    effective_date DATE,
    reconciliation_code VARCHAR(16)
);

UPDATE financial_ledger SET payload = '__fuzz_marker__' WHERE id > 85;

ALTER TABLE tbl_02 ADD COLUMN dyn_col_6 VARCHAR(32);

INSERT INTO tbl_05 (id, col_1, col_2, col_3, ref_tbl_02_id) VALUES (842887, 'chaos', '__fuzz_marker__', 'alpha_beta_gamma', 2147483647);

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (661931, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'escaped''quote', 'chaos', 'alpha_beta_gamma');
INSERT INTO financial_ledger (id, payload) VALUES (845353, 'escaped''quote');
COMMIT;

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, col_3, ref_tbl_02_id) VALUES (613509, 'alpha_beta_gamma', 'alpha_beta_gamma', '__fuzz_marker__', 2147483647);
INSERT INTO financial_ledger (id, payload) VALUES (344012, 'ref_val');
COMMIT;

INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_01_id) VALUES (484822, 0.0, 'escaped''quote', 100);

INSERT INTO financial_ledger (entry_id, account_id, amount, effective_date, reconciliation_code) VALUES
    (1, 101, 1500.00, '2023-01-10', 'REC-A'),
    (2, 101, -300.00, NULL, 'REC-B'),
    (3, 101, 450.50, '2023-01-15', NULL),
    (4, 101, -200.00, NULL, 'REC-C'),
    (5, 102, 2200.00, '2023-02-01', 'REC-A'),
    (6, 102, -800.00, '2023-02-05', 'REC-B');

INSERT INTO financial_ledger (id, payload) VALUES (74126, '');

UPDATE financial_ledger SET payload = '' WHERE id > 61;

SELECT
    ((t_ref_1.col_4 IS NOT NULL) / NULLIF((SELECT (SELECT FALSE FROM financial_ledger AS inn_5 LIMIT 1) FROM tbl_01 AS sub_2 LIMIT 1), 0)) AS c_6
FROM tbl_02 AS t_ref_1
WHERE (((SELECT (SELECT inn_8.col_2 FROM tbl_03 AS inn_8 LIMIT 1) FROM tbl_01 AS sub_7 LIMIT 1)::TEXT) IS NULL)
LIMIT 46;

ALTER TABLE tbl_03 ADD COLUMN dyn_col_5 VARCHAR(32);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (37825, TRUE, TRUE, 2147483647, -9223372036854775808);

ALTER TABLE tbl_05 DROP COLUMN ref_tbl_02_id;

INSERT INTO tbl_01 (id, col_1, col_2) VALUES (207484, 0, '{}');

UPDATE financial_ledger SET payload = 'alpha_beta_gamma' WHERE id > 38;

UPDATE tbl_04 SET col_2 = TRUE WHERE id > 94;

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (754847, 'ref_val', 'ref_val', 'escaped''quote');

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (138035, '', 'alpha_beta_gamma', '');

SELECT
    t_ref_9.payload AS c_10,
    FALSE AS c_11
FROM financial_ledger AS t_ref_9 LEFT JOIN tbl_03 AS j_ref_12 ON t_ref_9.id = j_ref_12.id
WHERE (UPPER((FALSE + ABS((UPPER((((t_ref_9.payload < (COALESCE(t_ref_9.id, t_ref_9.payload) >= ABS(NULL))) < (EXISTS (SELECT 1 FROM tbl_03 AS sub_13 WHERE sub_13.col_1)::NUMERIC(10,2))) * ABS(t_ref_9.id))) - COALESCE(LENGTH((CASE WHEN (NOT (UPPER(COALESCE(t_ref_9.payload, -2147483648)))) THEN (((FALSE * t_ref_9.payload)::TEXT::VARCHAR(32))::BOOLEAN) ELSE ('ref_val' <= (t_ref_9.id > ABS(TRUE))) END)), (t_ref_9.payload + (FALSE OR ((CASE WHEN 'alpha_beta_gamma' THEN (FALSE > TRUE) ELSE (SELECT (SELECT inn_15.col_2 FROM tbl_01 AS inn_15 LIMIT 1) FROM tbl_02 AS sub_14 LIMIT 1) END) AND TRUE)))))))) <> (((COALESCE((t_ref_9.id - (CASE WHEN (EXISTS (SELECT 1 FROM financial_ledger AS sub_16 WHERE FALSE) - (((t_ref_9.id - t_ref_9.payload)::TEXT::VARCHAR(32)) > (CASE WHEN EXISTS (SELECT 1 FROM tbl_02 AS sub_17 WHERE sub_17.col_4) THEN TRUE ELSE LOWER(t_ref_9.id) END))) THEN 'escaped''quote' ELSE TRUE END)), (((SELECT (SELECT NULL FROM financial_ledger AS sub_19 LIMIT 1) FROM financial_ledger AS sub_18 LIMIT 1) - ((1.0::NUMERIC(10,2))::TEXT::VARCHAR(32))) / NULLIF(((t_ref_9.payload IN (SELECT sub_20.col_3 FROM tbl_04 AS sub_20))::BOOLEAN), 0)))::TEXT) + t_ref_9.payload) <= t_ref_9.payload))
LIMIT 32;

CREATE INDEX idx_tbl_02_col_2_1 ON tbl_02 (col_2);

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 BOOLEAN,
    col_3 VARCHAR(64),
    col_4 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

UPDATE tbl_03 SET col_2 = 'escaped''quote' WHERE id > 51;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (912055, -1, FALSE, 'chaos', 'chaos', 2147483647);
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (374077, 0, TRUE, 'ref_val', 'chaos', 0);
COMMIT;

DELETE FROM tbl_03 WHERE id = 82407;

CREATE TABLE financial_ledger (
    entry_id INT PRIMARY KEY,
    account_id INT NOT NULL,
    amount NUMERIC(12,2) NOT NULL,
    effective_date DATE,
    reconciliation_code VARCHAR(16)
);

SELECT
    entry_id,
    account_id,
    effective_date,
    amount,
    SUM(amount) OVER (
        PARTITION BY account_id
        ORDER BY effective_date NULLS FIRST
        RANGE BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING
        EXCLUDE CURRENT ROW
    ) AS forward_rolling_sum,
    COUNT(*) OVER (
        PARTITION BY account_id
        ORDER BY effective_date NULLS FIRST
        RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
        EXCLUDE GROUP
    ) AS group_filtered_count
FROM financial_ledger;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (229481, 0, FALSE, '__fuzz_marker__', 'escaped''quote', -1);
INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_01_id, dyn_col_5) VALUES (619814, 1.0, 'chaos', 9223372036854775807, 'ref_val');
COMMIT;

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

CREATE INDEX idx_tbl_05_col_3_2 ON tbl_05 (col_3);

CREATE INDEX idx_tbl_01_id_3 ON tbl_01 (id);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (389489, '{}', '[]', '', 'escaped''quote', 'chaos');

DELETE FROM tbl_02 WHERE id = 11594;

CREATE INDEX idx_tbl_02_id_4 ON tbl_02 (id);

ALTER TABLE tbl_01 DROP COLUMN col_1;

CREATE INDEX idx_tbl_06_col_3_5 ON tbl_06 (col_3);

DELETE FROM tbl_01 WHERE id = 58903;

CREATE INDEX idx_tbl_04_col_4_6 ON tbl_04 (col_4);

UPDATE tbl_05 SET col_3 = '' WHERE id > 95;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT,
    col_3 INT,
    col_4 JSONB,
    ref_tbl_05_id INT,
    FOREIGN KEY (ref_tbl_05_id) REFERENCES tbl_05(id)
);

SELECT
    ((((SELECT (SELECT inn_23.col_1 FROM tbl_06 AS inn_23 LIMIT 1) FROM tbl_01 AS sub_22 LIMIT 1) + (SELECT sub_24.col_4 FROM tbl_07 AS sub_24 LIMIT 1))::BOOLEAN) + (EXISTS (SELECT 1 FROM tbl_02 AS sub_25 WHERE 'chaos')::BOOLEAN)) AS c_31
FROM tbl_01 AS t_ref_21
WHERE ((UPPER(ABS(((SELECT (SELECT inn_34.col_3 FROM tbl_06 AS inn_34 LIMIT 1) FROM tbl_06 AS sub_32 LIMIT 1) + (SELECT (SELECT 2147483647 FROM tbl_01 AS inn_36 LIMIT 1) FROM financial_ledger AS sub_35 LIMIT 1)))) OR t_ref_21.id) < NULL)
LIMIT 41;

INSERT INTO financial_ledger (id, payload) VALUES (35431, 'alpha_beta_gamma');

CREATE INDEX idx_tbl_06_id_7 ON tbl_06 (id);
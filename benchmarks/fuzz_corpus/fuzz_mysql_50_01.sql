-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: MYSQL

-- Statements: 50

-- AST Expression Depth: 10

-- Injected Defect Case: mysql_01

-- Seed: 5021



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 JSON,
    col_4 NUMERIC(10,2)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 INT,
    col_3 JSON
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSON,
    col_3 INT,
    col_4 BOOLEAN
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 NUMERIC(10,2),
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

CREATE TABLE sales_pipeline (
    opportunity_id INT PRIMARY KEY,
    rep_id INT NOT NULL,
    deal_value DECIMAL(12,2) NOT NULL,
    stage VARCHAR(32) NOT NULL
);

INSERT INTO tbl_05 (id, col_1, col_2, ref_tbl_03_id) VALUES (516989, '{}', 0.0000000001, 9223372036854775807);

CREATE INDEX idx_tbl_04_id_1 ON tbl_04 (id);

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT,
    col_3 JSON,
    col_4 BOOLEAN,
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

SELECT
    (CAST(CAST(((CASE WHEN (((CASE WHEN CAST(t_ref_1.payload AS DECIMAL(10,2)) THEN NULLIF('chaos', 9223372036854775807) ELSE t_ref_1.id END) - (t_ref_1.id + EXISTS (SELECT 1 FROM tbl_05 AS sub_2 WHERE sub_2.col_2))) * (((SELECT sub_3.col_4 FROM tbl_01 AS sub_3 LIMIT 1) <> t_ref_1.id) / NULLIF((SELECT (SELECT '__fuzz_marker__' FROM sales_pipeline AS inn_5 LIMIT 1) FROM tbl_06 AS sub_4 LIMIT 1), 0))) THEN CAST(CAST(t_ref_1.id AS CHAR) AS DECIMAL(10,2)) ELSE COALESCE(t_ref_1.payload, (SELECT (SELECT 1.0 FROM tbl_05 AS inn_8 LIMIT 1) FROM tbl_06 AS sub_6 LIMIT 1)) END) OR CAST(t_ref_1.id AS DECIMAL(10,2))) AS CHAR) AS DECIMAL(10,2)) IS NULL) AS c_9,
    FALSE AS c_10,
    EXISTS (SELECT 1 FROM tbl_03 AS sub_11 WHERE sub_11.col_2) AS c_16
FROM sales_pipeline AS t_ref_1
WHERE CAST(((42 - ((NOT (t_ref_1.id)) IS NOT NULL)) < (SELECT (SELECT CAST(CAST('escaped''quote' AS SIGNED) AS DECIMAL(10,2)) FROM tbl_02 AS sub_18 LIMIT 1) FROM tbl_05 AS sub_17 LIMIT 1)) AS CHAR)
LIMIT 36;

UPDATE tbl_03 SET col_1 = 'chaos' WHERE id > 79;

CREATE TABLE crm_leads (
    lead_id INT PRIMARY KEY,
    rep_id INT NOT NULL,
    estimated_arr DECIMAL(12,2) NOT NULL
);

UPDATE tbl_01 SET col_1 = -2147483648 WHERE id > 13;

SELECT
    ((((t_ref_19.id IN (SELECT sub_20.col_1 FROM tbl_05 AS sub_20)) - (t_ref_19.id > CAST(t_ref_19.id AS SIGNED))) < ((t_ref_19.col_2 = (((t_ref_19.ref_tbl_03_id + (CAST(t_ref_19.id AS CHAR) / NULLIF(t_ref_19.id, 0))) IS NOT NULL) - (0.0 AND t_ref_19.col_2))) AND NULLIF(t_ref_19.col_2, ((SELECT FALSE FROM sales_pipeline AS sub_21 LIMIT 1) + CAST(UPPER(((SELECT sub_22.col_1 FROM tbl_02 AS sub_22 LIMIT 1) / NULLIF((SELECT (SELECT -1.5 FROM tbl_05 AS inn_24 LIMIT 1) FROM tbl_03 AS sub_23 LIMIT 1), 0))) AS CHAR))))) <= t_ref_19.col_2) AS c_25,
    'escaped''quote' AS c_26
FROM tbl_05 AS t_ref_19 LEFT JOIN tbl_06 AS j_ref_27 ON t_ref_19.id = j_ref_27.id
WHERE (SELECT 0.0000000001 FROM sales_pipeline AS sub_28 LIMIT 1)
LIMIT 29;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT,
    col_3 NUMERIC(10,2),
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

CREATE INDEX idx_tbl_05_ref_tbl_03_id_2 ON tbl_05 (ref_tbl_03_id);

SELECT
    (SELECT (SELECT inn_33.col_2 FROM tbl_04 AS inn_33 LIMIT 1) FROM tbl_07 AS sub_30 LIMIT 1) AS c_34,
    (SELECT (SELECT NULL FROM sales_pipeline AS inn_37 LIMIT 1) FROM sales_pipeline AS sub_35 LIMIT 1) AS c_38,
    CAST(ABS(FALSE) AS DECIMAL(10,2)) AS c_39
FROM sales_pipeline AS t_ref_29
WHERE (TRUE AND FALSE)
LIMIT 11;

INSERT INTO sales_pipeline (opportunity_id, rep_id, deal_value, stage) VALUES
    (1, 10, 50000.00, 'closed_won'),
    (2, 10, 15000.00, 'in_progress'),
    (3, 20, 80000.00, 'closed_won'),
    (4, 20, 12000.00, 'discovery');

DELETE FROM tbl_04 WHERE id = 39705;

SELECT
    ABS((LENGTH(CAST(FALSE AS CHAR)) BETWEEN 0 AND 9223372036854775807)) AS c_41
FROM tbl_04 AS t_ref_40 LEFT JOIN tbl_01 AS j_ref_42 ON t_ref_40.id = j_ref_42.id
WHERE t_ref_40.col_1
LIMIT 5;

INSERT INTO tbl_03 (id, col_1, col_2) VALUES (919612, '__fuzz_marker__', FALSE);

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT,
    col_3 NUMERIC(10,2)
);

CREATE INDEX idx_tbl_08_col_3_3 ON tbl_08 (col_3);

INSERT INTO crm_leads (lead_id, rep_id, estimated_arr) VALUES
    (101, 10, 25000.00),
    (102, 20, 45000.00);

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_08 WHERE id IS NOT NULL;

UPDATE tbl_05 SET ref_tbl_03_id = -2147483648 WHERE id > 43;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 JSON,
    col_4 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

SELECT
    (SELECT (SELECT NULL FROM tbl_03 AS inn_45 LIMIT 1) FROM tbl_01 AS sub_44 LIMIT 1) AS c_46,
    (SELECT (SELECT TRUE FROM tbl_03 AS inn_51 LIMIT 1) FROM tbl_09 AS sub_47 LIMIT 1) AS c_52,
    (t_ref_43.ref_tbl_03_id IN (SELECT sub_53.id FROM tbl_09 AS sub_53)) AS c_59
FROM tbl_05 AS t_ref_43
WHERE NULL
LIMIT 23;

DELETE FROM tbl_09 WHERE id = 80328;

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_08 WHERE id IS NOT NULL;

UPDATE tbl_04 SET col_3 = -1 WHERE id > 64;

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSON,
    col_3 JSON,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

UPDATE tbl_03 SET col_1 = 'ref_val' WHERE id > 11;

INSERT INTO tbl_10 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (393880, -1.5, '[]', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -9223372036854775808);

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 NUMERIC(10,2),
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

UPDATE tbl_06 SET col_2 = -2147483648 WHERE id > 73;

UPDATE tbl_05 SET ref_tbl_03_id = 1 WHERE id > 52;

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_10 WHERE id IS NOT NULL;

SELECT
    ((COALESCE((NOT (t_ref_60.ref_tbl_04_id)), (SELECT sub_61.id FROM tbl_05 AS sub_61 LIMIT 1)) <= CAST(((SELECT (CASE WHEN ((SELECT sub_63.col_3 FROM tbl_09 AS sub_63 LIMIT 1) * sub_62.id) THEN ((sub_62.id < 9223372036854775807) IS NOT NULL) ELSE (CASE WHEN ('alpha_beta_gamma' + sub_62.id) THEN (sub_62.id >= sub_62.col_1) ELSE (SELECT (SELECT -1 FROM sales_pipeline AS inn_65 LIMIT 1) FROM tbl_11 AS sub_64 LIMIT 1) END) END) FROM tbl_02 AS sub_62 LIMIT 1) <= ((NOT (t_ref_60.col_2)) + ((COALESCE(('alpha_beta_gamma' + FALSE), (SELECT sub_66.ref_tbl_01_id FROM tbl_10 AS sub_66 LIMIT 1)) IS NOT NULL) OR t_ref_60.col_3))) AS DECIMAL(10,2))) / NULLIF(t_ref_60.col_1, 0)) AS c_67,
    (CAST('alpha_beta_gamma' AS DECIMAL(10,2)) >= (t_ref_60.id - (t_ref_60.ref_tbl_04_id * NULL))) AS c_68
FROM tbl_07 AS t_ref_60
WHERE NULLIF((((NOT (NULL)) - UPPER(CAST(t_ref_60.col_3 AS SIGNED))) + NULL), CAST((t_ref_60.ref_tbl_04_id IN (SELECT sub_69.ref_tbl_01_id FROM tbl_09 AS sub_69)) AS DECIMAL(10,2)))
LIMIT 35;

CREATE TABLE sales_pipeline (
    opportunity_id INT PRIMARY KEY,
    rep_id INT NOT NULL,
    deal_value DECIMAL(12,2) NOT NULL,
    stage VARCHAR(32) NOT NULL
);

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

ALTER TABLE tbl_02 ADD COLUMN dyn_col_5 VARCHAR(32);

ALTER TABLE tbl_02 DROP COLUMN dyn_col_5;

UPDATE tbl_04 SET col_2 = '{}' WHERE id > 69;

ALTER TABLE tbl_09 ADD COLUMN dyn_col_7 VARCHAR(32);

CREATE INDEX idx_tbl_01_id_4 ON tbl_01 (id);

SELECT
    NULLIF((((NULL - 0.0000000001) - NULL) OR ((('escaped''quote' + (SELECT (sub_72.col_3 IN (SELECT sub_73.col_1 FROM tbl_11 AS sub_73)) FROM tbl_07 AS sub_72 LIMIT 1)) >= ((t_ref_71.id >= (EXISTS (SELECT 1 FROM tbl_03 AS sub_74 WHERE sub_74.col_1) * ((FALSE <> t_ref_71.col_2) OR (SELECT sub_75.id FROM tbl_01 AS sub_75 LIMIT 1)))) * LENGTH(TRUE))) = (t_ref_71.id IS NULL))), (UPPER(t_ref_71.col_2) = NULLIF((t_ref_71.id IN (SELECT sub_76.col_4 FROM tbl_06 AS sub_76)), (SELECT (CAST(LENGTH((NOT (sub_80.ref_tbl_01_id))) AS DECIMAL(10,2)) AND ((SELECT (SELECT inn_82.payload FROM sales_pipeline AS inn_82 LIMIT 1) FROM tbl_01 AS sub_81 LIMIT 1) / NULLIF(FALSE, 0))) FROM tbl_09 AS sub_80 LIMIT 1)))) AS c_83,
    ((t_ref_71.col_2 AND ((TRUE IN (SELECT sub_84.id FROM tbl_03 AS sub_84)) BETWEEN 0 AND 9223372036854775807)) >= (t_ref_71.col_1 - ((CASE WHEN t_ref_71.col_1 THEN CAST((EXISTS (SELECT 1 FROM tbl_11 AS sub_87 WHERE sub_87.ref_tbl_02_id) >= (t_ref_71.col_1 IN (SELECT sub_88.col_1 FROM tbl_02 AS sub_88))) AS CHAR) ELSE ('chaos' IN (SELECT sub_89.ref_tbl_03_id FROM tbl_05 AS sub_89)) END) BETWEEN -2147483648 AND -2147483648))) AS c_90
FROM tbl_03 AS t_ref_71
WHERE ((SELECT ((CAST(sub_91.col_1 AS DECIMAL(10,2)) = sub_91.col_1) <= (SELECT (((TRUE * sub_92.col_1) = CAST(sub_92.col_2 AS SIGNED)) - CAST(sub_92.col_3 AS DECIMAL(10,2))) FROM tbl_10 AS sub_92 LIMIT 1)) FROM tbl_11 AS sub_91 LIMIT 1) OR (NULLIF(t_ref_71.id, COALESCE(COALESCE(t_ref_71.col_2, (SELECT ((sub_93.col_2 IS NULL) < CAST(sub_93.col_3 AS SIGNED)) FROM tbl_06 AS sub_93 LIMIT 1)), CAST((NOT ((CAST(COALESCE(t_ref_71.col_1, t_ref_71.col_1) AS SIGNED) <> ABS(EXISTS (SELECT 1 FROM tbl_04 AS sub_94 WHERE sub_94.col_3))))) AS DECIMAL(10,2)))) * ((0.0000000001 IN (SELECT sub_95.col_1 FROM tbl_11 AS sub_95)) / NULLIF(EXISTS (SELECT 1 FROM tbl_03 AS sub_97 WHERE sub_97.col_2), 0))))
LIMIT 6;

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT,
    col_3 NUMERIC(10,2),
    col_4 JSON,
    ref_tbl_10_id INT,
    FOREIGN KEY (ref_tbl_10_id) REFERENCES tbl_10(id)
);

INSERT INTO tbl_03 (id, col_1, col_2) VALUES (548153, 'ref_val', FALSE);
-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: MYSQL

-- Statements: 100

-- AST Expression Depth: 14

-- Injected Defect Case: mysql_02

-- Seed: 10021



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSON
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 VARCHAR(64),
    col_3 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 BOOLEAN,
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64),
    col_4 BOOLEAN
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSON
);

CREATE TABLE customers (
    customer_id INT PRIMARY KEY,
    region_code VARCHAR(8) NOT NULL,
    tier VARCHAR(16) NOT NULL
);

SELECT
    (SELECT sub_2.col_2 FROM tbl_04 AS sub_2 LIMIT 1) AS c_3,
    NULLIF(ABS(t_ref_1.col_1), (SELECT ((sub_4.ref_tbl_01_id * TRUE) / NULLIF((-1 IN (SELECT sub_5.ref_tbl_01_id FROM tbl_02 AS sub_5)), 0)) FROM tbl_02 AS sub_4 LIMIT 1)) AS c_6
FROM tbl_01 AS t_ref_1 LEFT JOIN tbl_05 AS j_ref_7 ON t_ref_1.id = j_ref_7.id
WHERE (CASE WHEN ('alpha_beta_gamma' - 1) THEN LENGTH(((t_ref_1.col_2 OR (t_ref_1.col_2 IN (SELECT sub_8.ref_tbl_02_id FROM tbl_03 AS sub_8))) - NULLIF((SELECT (SELECT TRUE FROM tbl_04 AS inn_10 LIMIT 1) FROM tbl_03 AS sub_9 LIMIT 1), UPPER((SELECT ((FALSE IN (SELECT sub_12.id FROM orders AS sub_12)) - (CASE WHEN sub_11.id THEN CAST(sub_11.payload AS SIGNED) ELSE (COALESCE('ref_val', (FALSE / NULLIF(CAST(sub_11.id AS SIGNED), 0))) BETWEEN 9223372036854775807 AND 1) END)) FROM orders AS sub_11 LIMIT 1))))) ELSE ((t_ref_1.col_2 BETWEEN -9223372036854775808 AND 100) / NULLIF(COALESCE((SELECT (SELECT inn_14.col_2 FROM tbl_01 AS inn_14 LIMIT 1) FROM orders AS sub_13 LIMIT 1), ABS((t_ref_1.col_2 AND COALESCE((((ABS((42 IN (SELECT sub_15.id FROM tbl_05 AS sub_15))) - ABS((SELECT (sub_16.ref_tbl_01_id / NULLIF(sub_16.ref_tbl_01_id, 0)) FROM tbl_02 AS sub_16 LIMIT 1))) <> LENGTH((t_ref_1.id - (-1 IN (SELECT sub_17.col_2 FROM tbl_03 AS sub_17))))) - LOWER(((CASE WHEN (ABS(COALESCE(0.0, t_ref_1.col_1)) + t_ref_1.id) THEN EXISTS (SELECT 1 FROM orders AS sub_18 WHERE TRUE) ELSE FALSE END) IS NOT NULL))), 'chaos')))), 0)) END)
LIMIT 33;

UPDATE tbl_02 SET ref_tbl_01_id = 1 WHERE id > 17;

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2)
);

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 BOOLEAN,
    ref_tbl_06_id INT,
    FOREIGN KEY (ref_tbl_06_id) REFERENCES tbl_06(id)
);

UPDATE tbl_02 SET ref_tbl_01_id = -9223372036854775808 WHERE id > 1;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (185976, '[]', 'chaos', 'chaos', -9223372036854775808);

DELETE FROM tbl_03 WHERE id = 99748;

DELETE FROM orders WHERE id = 57272;

CREATE INDEX idx_tbl_07_id_1 ON tbl_07 (id);

SELECT
    t_ref_19.col_1 AS c_20,
    EXISTS (SELECT 1 FROM tbl_06 AS sub_21 WHERE sub_21.id) AS c_45
FROM tbl_01 AS t_ref_19 LEFT JOIN tbl_04 AS j_ref_46 ON t_ref_19.id = j_ref_46.id
WHERE CAST(t_ref_19.col_2 AS SIGNED)
LIMIT 18;

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (80280, 3.14159265, 0.0, '', TRUE);

SELECT
    NULLIF(CAST(EXISTS (SELECT 1 FROM tbl_03 AS sub_48 WHERE sub_48.ref_tbl_02_id) AS CHAR), CAST(t_ref_47.id AS SIGNED)) AS c_53,
    NULLIF(t_ref_47.col_2, ((LOWER(COALESCE((CAST(LOWER(t_ref_47.col_2) AS DECIMAL(10,2)) OR CAST(9223372036854775807 AS SIGNED)), t_ref_47.col_1)) / NULLIF((((EXISTS (SELECT 1 FROM tbl_07 AS sub_54 WHERE sub_54.id) <= ('__fuzz_marker__' - t_ref_47.col_2)) OR EXISTS (SELECT 1 FROM orders AS sub_56 WHERE 0)) + t_ref_47.col_1), 0)) * (CASE WHEN CAST((CASE WHEN (SELECT (NOT (CAST(NULLIF(-1.5, (CASE WHEN (SELECT (SELECT inn_60.col_2 FROM tbl_01 AS inn_60 LIMIT 1) FROM tbl_06 AS sub_59 LIMIT 1) THEN sub_58.col_1 ELSE COALESCE(sub_58.col_2, (sub_58.col_2 IS NOT NULL)) END)) AS SIGNED))) FROM tbl_01 AS sub_58 LIMIT 1) THEN ((SELECT (SELECT inn_63.col_2 FROM tbl_06 AS inn_63 LIMIT 1) FROM tbl_03 AS sub_61 LIMIT 1) BETWEEN 1 AND 2147483647) ELSE ((((SELECT (SELECT NULL FROM tbl_07 AS inn_65 LIMIT 1) FROM tbl_02 AS sub_64 LIMIT 1) * ((t_ref_47.col_1 - (t_ref_47.col_2 - t_ref_47.col_2)) BETWEEN 100 AND 0)) = t_ref_47.col_2) + ABS(CAST((SELECT sub_66.id FROM orders AS sub_66 LIMIT 1) AS DECIMAL(10,2)))) END) AS CHAR) THEN LENGTH(((CAST(COALESCE((NOT (((LOWER((t_ref_47.col_2 > t_ref_47.col_2)) + t_ref_47.id) / NULLIF(((t_ref_47.id / NULLIF(t_ref_47.col_1, 0)) > ((100 BETWEEN 100 AND 42) <> t_ref_47.col_1)), 0)))), (CAST(t_ref_47.col_2 AS CHAR) <> ((CAST((SELECT (SELECT inn_68.id FROM tbl_02 AS inn_68 LIMIT 1) FROM tbl_06 AS sub_67 LIMIT 1) AS CHAR) * (TRUE = t_ref_47.col_2)) = CAST((EXISTS (SELECT 1 FROM orders AS sub_69 WHERE sub_69.payload) + (t_ref_47.id IS NULL)) AS SIGNED)))) AS CHAR) + CAST(((EXISTS (SELECT 1 FROM tbl_02 AS sub_70 WHERE 0.0) + (((CAST(t_ref_47.id AS SIGNED) AND (t_ref_47.col_2 IN (SELECT sub_71.col_2 FROM tbl_06 AS sub_71))) + (SELECT (SELECT inn_73.id FROM tbl_04 AS inn_73 LIMIT 1) FROM tbl_03 AS sub_72 LIMIT 1)) / NULLIF(t_ref_47.id, 0))) < (((COALESCE(NULL, (t_ref_47.col_2 + t_ref_47.col_1)) * (('ref_val' + 2147483647) <= (TRUE <= t_ref_47.id))) BETWEEN 100 AND -2147483648) >= TRUE)) AS SIGNED)) / NULLIF((CAST((SELECT (sub_74.col_2 IN (SELECT sub_75.id FROM tbl_07 AS sub_75)) FROM tbl_05 AS sub_74 LIMIT 1) AS DECIMAL(10,2)) > t_ref_47.id), 0))) ELSE (CAST(CAST(t_ref_47.col_1 AS CHAR) AS SIGNED) < CAST(EXISTS (SELECT 1 FROM tbl_02 AS sub_76 WHERE sub_76.col_2) AS SIGNED)) END))) AS c_79,
    t_ref_47.col_2 AS c_80
FROM tbl_05 AS t_ref_47
WHERE UPPER(UPPER(COALESCE((t_ref_47.col_2 + EXISTS (SELECT 1 FROM tbl_04 AS sub_81 WHERE sub_81.id)), ((CAST('' AS SIGNED) * (SELECT (SELECT inn_86.ref_tbl_06_id FROM tbl_07 AS inn_86 LIMIT 1) FROM tbl_04 AS sub_82 LIMIT 1)) - UPPER((t_ref_47.col_1 <= t_ref_47.id))))))
LIMIT 36;

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_06 WHERE id IS NOT NULL;

CREATE TABLE orders (
    order_id INT PRIMARY KEY,
    customer_id INT NOT NULL,
    total_amount DECIMAL(10,2) NOT NULL,
    order_status VARCHAR(16) NOT NULL
);

CREATE INDEX idx_tbl_07_ref_tbl_06_id_2 ON tbl_07 (ref_tbl_06_id);

UPDATE tbl_04 SET col_2 = 1e10 WHERE id > 70;

INSERT INTO orders (id, payload) VALUES (966218, 'chaos');

INSERT INTO tbl_02 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (74059, '[]', 'chaos', 'ref_val', 1);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (859178, 0.0000000001, -1e-5, '', FALSE);

INSERT INTO tbl_06 (id, col_1, col_2) VALUES (775211, 1e10, -1.5);

CREATE INDEX idx_tbl_01_col_2_3 ON tbl_01 (col_2);

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 JSON,
    col_3 VARCHAR(64)
);

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 BOOLEAN
);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (281057, '[]', '__fuzz_marker__', 'ref_val', 42);

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN,
    col_3 VARCHAR(64),
    col_4 BOOLEAN
);

SELECT
    t_ref_87.col_3 AS c_88
FROM tbl_08 AS t_ref_87
WHERE (UPPER(LOWER((CASE WHEN (ABS((t_ref_87.col_1 + (CAST(((t_ref_87.col_1 IN (SELECT sub_89.col_2 FROM tbl_03 AS sub_89)) + ((SELECT sub_90.col_3 FROM tbl_08 AS sub_90 LIMIT 1) * ((t_ref_87.col_3 BETWEEN 42 AND 1) * UPPER(t_ref_87.id)))) AS DECIMAL(10,2)) - CAST(UPPER((t_ref_87.col_3 IN (SELECT sub_91.ref_tbl_06_id FROM tbl_07 AS sub_91))) AS SIGNED)))) IS NOT NULL) THEN (CASE WHEN (SELECT (sub_92.col_1 IS NULL) FROM tbl_02 AS sub_92 LIMIT 1) THEN COALESCE((CAST(t_ref_87.col_1 AS CHAR) / NULLIF((EXISTS (SELECT 1 FROM tbl_06 AS sub_93 WHERE sub_93.col_2) + t_ref_87.col_3), 0)), UPPER((SELECT 999999999.99 FROM tbl_08 AS sub_94 LIMIT 1))) ELSE t_ref_87.col_2 END) ELSE ((t_ref_87.col_1 - (SELECT '' FROM tbl_10 AS sub_95 LIMIT 1)) * CAST((NOT (t_ref_87.id)) AS CHAR)) END))) OR ((TRUE + LOWER(t_ref_87.col_1)) AND ((SELECT (SELECT inn_104.col_1 FROM tbl_02 AS inn_104 LIMIT 1) FROM tbl_07 AS sub_96 LIMIT 1) / NULLIF(t_ref_87.col_2, 0))))
LIMIT 43;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (103309, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'chaos', 'alpha_beta_gamma', 42);

INSERT INTO customers (customer_id, region_code, tier) VALUES
    (1, 'NA', 'gold'),
    (2, 'NA', 'silver'),
    (3, 'EU', 'gold'),
    (4, 'APAC', 'bronze');

SELECT
    (SELECT (SELECT inn_107.col_1 FROM tbl_04 AS inn_107 LIMIT 1) FROM tbl_04 AS sub_106 LIMIT 1) AS c_108,
    NULLIF('chaos', (SELECT (SELECT 'ref_val' FROM tbl_02 AS inn_112 LIMIT 1) FROM tbl_08 AS sub_109 LIMIT 1)) AS c_113
FROM tbl_09 AS t_ref_105 LEFT JOIN tbl_10 AS j_ref_114 ON t_ref_105.id = j_ref_114.id
WHERE (((FALSE IN (SELECT sub_115.col_2 FROM tbl_10 AS sub_115)) <= (NOT (ABS(((SELECT LOWER((NOT (sub_116.id))) FROM tbl_04 AS sub_116 LIMIT 1) / NULLIF((SELECT (CASE WHEN ((SELECT (SELECT -1 FROM orders AS inn_119 LIMIT 1) FROM tbl_04 AS sub_118 LIMIT 1) - sub_117.id) THEN NULLIF((LOWER(COALESCE(CAST(sub_117.col_1 AS DECIMAL(10,2)), (sub_117.col_1 < -1.5))) AND (SELECT CAST(sub_120.id AS SIGNED) FROM tbl_07 AS sub_120 LIMIT 1)), COALESCE(((('chaos' * 'ref_val') <> CAST('' AS SIGNED)) + EXISTS (SELECT 1 FROM tbl_10 AS sub_121 WHERE sub_121.col_3)), (CASE WHEN (SELECT sub_122.col_1 FROM tbl_10 AS sub_122 LIMIT 1) THEN sub_117.col_2 ELSE (SELECT NULL FROM tbl_09 AS sub_123 LIMIT 1) END))) ELSE ((EXISTS (SELECT 1 FROM tbl_09 AS sub_124 WHERE sub_124.col_3) OR sub_117.col_1) - ((((sub_117.col_2 OR FALSE) + (SELECT (SELECT 'chaos' FROM tbl_04 AS inn_126 LIMIT 1) FROM tbl_08 AS sub_125 LIMIT 1)) * sub_117.col_1) * (sub_117.col_1 / NULLIF((EXISTS (SELECT 1 FROM tbl_05 AS sub_127 WHERE sub_127.col_2) - LOWER(sub_117.col_1)), 0)))) END) FROM tbl_06 AS sub_117 LIMIT 1), 0)))))) AND t_ref_105.id)
LIMIT 25;

INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_02_id) VALUES (900251, TRUE, TRUE, -2147483648);

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (954330, TRUE, 0.0000000001, TRUE);

INSERT INTO tbl_01 (id, col_1, col_2) VALUES (443869, '', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (146566, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'alpha_beta_gamma', 'escaped''quote', 0);
INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_06_id) VALUES (621359, 100, FALSE, 2147483647);
COMMIT;

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (132367, TRUE, -1e-5, FALSE);

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 JSON,
    col_4 JSON
);

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2)
);

DELETE FROM tbl_12 WHERE id = 35889;

ALTER TABLE tbl_04 DROP COLUMN col_4;

SELECT
    (t_ref_128.col_1 BETWEEN 0 AND 9223372036854775807) AS c_129,
    (t_ref_128.col_1 + (t_ref_128.col_1 AND ('__fuzz_marker__' > FALSE))) AS c_130,
    ((((((t_ref_128.col_2 IN (SELECT sub_131.id FROM tbl_05 AS sub_131)) / NULLIF(((NOT ((SELECT (SELECT inn_136.col_3 FROM tbl_04 AS inn_136 LIMIT 1) FROM tbl_06 AS sub_133 LIMIT 1))) = CAST((NULLIF(t_ref_128.col_2, (0 >= 'escaped''quote')) AND (((CASE WHEN (t_ref_128.col_1 IS NOT NULL) THEN (t_ref_128.col_2 - t_ref_128.id) ELSE (t_ref_128.id IN (SELECT sub_137.col_4 FROM tbl_10 AS sub_137)) END) <> t_ref_128.col_2) * ABS(CAST(t_ref_128.col_1 AS SIGNED)))) AS SIGNED)), 0)) / NULLIF(EXISTS (SELECT 1 FROM tbl_07 AS sub_138 WHERE -1e-5), 0)) / NULLIF(EXISTS (SELECT 1 FROM tbl_05 AS sub_139 WHERE TRUE), 0)) / NULLIF(CAST(((SELECT (ABS(((NOT (((sub_150.payload IN (SELECT sub_151.col_4 FROM tbl_11 AS sub_151)) <> (UPPER('__fuzz_marker__') / NULLIF(CAST(sub_150.id AS CHAR), 0))))) * sub_150.payload)) OR EXISTS (SELECT 1 FROM tbl_09 AS sub_152 WHERE TRUE)) FROM orders AS sub_150 LIMIT 1) / NULLIF((t_ref_128.id / NULLIF(EXISTS (SELECT 1 FROM tbl_07 AS sub_154 WHERE sub_154.id), 0)), 0)) AS DECIMAL(10,2)), 0)) BETWEEN -2147483648 AND -9223372036854775808) AS c_155
FROM tbl_05 AS t_ref_128 LEFT JOIN orders AS j_ref_156 ON t_ref_128.id = j_ref_156.id
WHERE LOWER(CAST(COALESCE((LOWER(((UPPER((NOT (CAST(FALSE AS CHAR)))) <> (SELECT COALESCE(LENGTH(((TRUE / NULLIF(sub_157.id, 0)) / NULLIF((SELECT TRUE FROM tbl_09 AS sub_158 LIMIT 1), 0))), EXISTS (SELECT 1 FROM tbl_12 AS sub_159 WHERE FALSE)) FROM orders AS sub_157 LIMIT 1)) - LENGTH(UPPER(LOWER('alpha_beta_gamma'))))) / NULLIF((SELECT CAST(UPPER((CAST(EXISTS (SELECT 1 FROM tbl_04 AS sub_161 WHERE NULL) AS DECIMAL(10,2)) * (SELECT (SELECT -1 FROM tbl_11 AS inn_164 LIMIT 1) FROM tbl_01 AS sub_163 LIMIT 1))) AS DECIMAL(10,2)) FROM tbl_06 AS sub_160 LIMIT 1), 0)), (SELECT (SELECT 'alpha_beta_gamma' FROM tbl_01 AS inn_170 LIMIT 1) FROM tbl_11 AS sub_165 LIMIT 1)) AS SIGNED))
LIMIT 2;

CREATE INDEX idx_tbl_04_col_1_4 ON tbl_04 (col_1);

INSERT INTO tbl_01 (id, col_1, col_2) VALUES (145250, 'ref_val', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

INSERT INTO orders (order_id, customer_id, total_amount, order_status) VALUES
    (101, 1, 350.00, 'completed'),
    (102, 1, 120.00, 'completed'),
    (103, 2, 85.00, 'pending'),
    (104, 3, 490.00, 'completed'),
    (105, 4, 25.00, 'refunded');

SELECT
    t_ref_171.col_3 AS c_172,
    (NULLIF(('' / NULLIF(((t_ref_171.col_2 IN (SELECT sub_173.col_1 FROM tbl_10 AS sub_173)) OR EXISTS (SELECT 1 FROM tbl_11 AS sub_180 WHERE sub_180.col_1)), 0)), FALSE) / NULLIF((t_ref_171.col_2 > ((((CASE WHEN ((t_ref_171.id * (((CAST((NOT (0.0000000001)) AS CHAR) * CAST((SELECT (SELECT inn_186.col_3 FROM tbl_02 AS inn_186 LIMIT 1) FROM tbl_04 AS sub_185 LIMIT 1) AS DECIMAL(10,2))) - (SELECT (SELECT inn_188.id FROM tbl_10 AS inn_188 LIMIT 1) FROM tbl_03 AS sub_187 LIMIT 1)) / NULLIF(((SELECT (SELECT inn_190.col_2 FROM tbl_12 AS inn_190 LIMIT 1) FROM tbl_10 AS sub_189 LIMIT 1) AND 1), 0))) * ((t_ref_171.col_1 IN (SELECT sub_191.id FROM tbl_12 AS sub_191)) OR EXISTS (SELECT 1 FROM tbl_01 AS sub_193 WHERE sub_193.col_1))) THEN t_ref_171.col_3 ELSE (CASE WHEN ((t_ref_171.col_2 / NULLIF(LOWER(((t_ref_171.col_3 > LOWER(t_ref_171.id)) IS NOT NULL)), 0)) BETWEEN 1 AND -2147483648) THEN t_ref_171.col_2 ELSE '' END) END) / NULLIF((t_ref_171.col_1 = EXISTS (SELECT 1 FROM tbl_11 AS sub_195 WHERE TRUE)), 0)) + (CASE WHEN TRUE THEN ((TRUE - LENGTH((t_ref_171.col_1 AND (CASE WHEN ((SELECT (SELECT 0 FROM tbl_04 AS inn_197 LIMIT 1) FROM tbl_03 AS sub_196 LIMIT 1) / NULLIF(((t_ref_171.id * NULL) * UPPER(t_ref_171.col_1)), 0)) THEN (NULL IN (SELECT sub_198.col_1 FROM tbl_07 AS sub_198)) ELSE t_ref_171.col_1 END)))) * (EXISTS (SELECT 1 FROM tbl_12 AS sub_199 WHERE sub_199.col_2) - (SELECT (SELECT FALSE FROM tbl_02 AS inn_203 LIMIT 1) FROM tbl_09 AS sub_201 LIMIT 1))) ELSE EXISTS (SELECT 1 FROM tbl_10 AS sub_204 WHERE sub_204.col_1) END)) > t_ref_171.col_1)), 0)) AS c_209
FROM tbl_04 AS t_ref_171
WHERE FALSE
LIMIT 35;

UPDATE orders SET payload = 'escaped''quote' WHERE id > 56;

BEGIN;
INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (482762, FALSE, 1e10, TRUE);
INSERT INTO tbl_01 (id, col_1, col_2) VALUES (956266, 'escaped''quote', '{}');
INSERT INTO orders (id, payload) VALUES (866418, 'chaos');
COMMIT;

BEGIN;
INSERT INTO tbl_11 (id, col_1, col_2, col_3, col_4) VALUES (514439, 1.0, 'alpha_beta_gamma', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{}');
INSERT INTO tbl_11 (id, col_1, col_2, col_3, col_4) VALUES (343687, 0.0, '__fuzz_marker__', '[]', '[]');
INSERT INTO tbl_02 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (557243, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'escaped''quote', 'ref_val', -2147483648);
COMMIT;

BEGIN;
INSERT INTO tbl_11 (id, col_1, col_2, col_3, col_4) VALUES (578410, 999999999.99, '__fuzz_marker__', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

INSERT INTO tbl_08 (id, col_1, col_2, col_3) VALUES (409034, TRUE, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'escaped''quote');

INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_02_id) VALUES (48017, FALSE, FALSE, 42);

BEGIN;
INSERT INTO orders (id, payload) VALUES (962126, 'alpha_beta_gamma');
COMMIT;

SELECT
    LOWER(CAST((t_ref_210.col_1 IS NULL) AS SIGNED)) AS c_211
FROM tbl_08 AS t_ref_210
WHERE (t_ref_210.col_2 IN (SELECT sub_212.col_2 FROM tbl_03 AS sub_212))
LIMIT 50;

DELETE FROM orders WHERE id = 5318;

BEGIN;
INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (553606, FALSE, 999999999.99, FALSE);
INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4) VALUES (793153, -1.5, TRUE, 'alpha_beta_gamma', FALSE);
COMMIT;

BEGIN;
INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (177682, TRUE, 1.0, FALSE);
COMMIT;

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64),
    ref_tbl_10_id INT,
    FOREIGN KEY (ref_tbl_10_id) REFERENCES tbl_10(id)
);

INSERT INTO tbl_08 (id, col_1, col_2, col_3) VALUES (574442, TRUE, '{}', 'alpha_beta_gamma');

INSERT INTO tbl_12 (id, col_1, col_2) VALUES (310416, 1e10, 3.14159265);

CREATE INDEX idx_tbl_09_col_1_5 ON tbl_09 (col_1);

INSERT INTO tbl_08 (id, col_1, col_2, col_3) VALUES (166738, FALSE, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'chaos');

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

SELECT
    t_ref_222.id AS c_223,
    t_ref_222.col_1 AS c_224,
    (LENGTH(t_ref_222.col_1) * NULL) AS c_225
FROM tbl_06 AS t_ref_222
WHERE (((t_ref_222.id BETWEEN -1 AND 100) AND ((NOT (COALESCE('ref_val', LENGTH((NULLIF(((CAST((EXISTS (SELECT 1 FROM tbl_01 AS sub_226 WHERE FALSE) + ('__fuzz_marker__' OR NULL)) AS SIGNED) IS NULL) * LENGTH('escaped''quote')), ((t_ref_222.col_2 - t_ref_222.col_2) * (LOWER((CASE WHEN (t_ref_222.col_2 - 'ref_val') THEN NULL ELSE t_ref_222.col_2 END)) + (CAST((SELECT TRUE FROM tbl_07 AS sub_227 LIMIT 1) AS CHAR) < (FALSE IN (SELECT sub_228.col_1 FROM tbl_02 AS sub_228)))))) + (SELECT (SELECT 'chaos' FROM tbl_13 AS inn_230 LIMIT 1) FROM tbl_07 AS sub_229 LIMIT 1)))))) > NULLIF(NULLIF((((SELECT (SELECT FALSE FROM tbl_03 AS inn_233 LIMIT 1) FROM tbl_07 AS sub_231 LIMIT 1) + t_ref_222.id) * (CASE WHEN ((FALSE IS NOT NULL) + EXISTS (SELECT 1 FROM tbl_11 AS sub_234 WHERE sub_234.col_3)) THEN (t_ref_222.id - CAST(t_ref_222.col_1 AS DECIMAL(10,2))) ELSE t_ref_222.id END)), (t_ref_222.id IN (SELECT sub_236.id FROM tbl_06 AS sub_236))), (SELECT (SELECT inn_242.ref_tbl_02_id FROM tbl_03 AS inn_242 LIMIT 1) FROM tbl_02 AS sub_238 LIMIT 1)))) > NULLIF((SELECT (CAST(LENGTH(('__fuzz_marker__' BETWEEN 1 AND 0)) AS CHAR) * 1) FROM tbl_09 AS sub_243 LIMIT 1), ((t_ref_222.col_2 * (SELECT ((sub_244.col_3 IN (SELECT sub_245.id FROM orders AS sub_245)) * sub_244.col_2) FROM tbl_11 AS sub_244 LIMIT 1)) / NULLIF((((-1e-5 < UPPER(TRUE)) / NULLIF(CAST((t_ref_222.col_1 - CAST(NULLIF(EXISTS (SELECT 1 FROM tbl_07 AS sub_248 WHERE sub_248.col_1), (CASE WHEN COALESCE(COALESCE((NULL BETWEEN 2147483647 AND 2147483647), (t_ref_222.col_2 IN (SELECT sub_250.id FROM tbl_10 AS sub_250))), ('' * ABS(FALSE))) THEN (((t_ref_222.id IN (SELECT sub_251.id FROM tbl_07 AS sub_251)) <= (CASE WHEN 1 THEN t_ref_222.id ELSE t_ref_222.col_1 END)) - t_ref_222.col_1) ELSE CAST(((SELECT (SELECT inn_253.col_1 FROM tbl_12 AS inn_253 LIMIT 1) FROM tbl_02 AS sub_252 LIMIT 1) < t_ref_222.col_2) AS SIGNED) END)) AS CHAR)) AS SIGNED), 0)) BETWEEN -1 AND 9223372036854775807), 0))))
LIMIT 38;

BEGIN;
INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (861035, FALSE, 1e10, TRUE);
COMMIT;

CREATE INDEX idx_tbl_12_col_2_6 ON tbl_12 (col_2);

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN,
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

UPDATE tbl_10 SET col_1 = 999999999.99 WHERE id > 11;

ALTER TABLE tbl_08 ADD COLUMN dyn_col_5 VARCHAR(32);

BEGIN;
INSERT INTO tbl_14 (id, col_1, col_2, ref_tbl_04_id) VALUES (987685, 0.0, FALSE, 42);
COMMIT;

INSERT INTO tbl_11 (id, col_1, col_2, col_3, col_4) VALUES (956162, 1.0, 'alpha_beta_gamma', '{}', '[]');

CREATE INDEX idx_tbl_06_col_2_7 ON tbl_06 (col_2);

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_06 WHERE id IS NOT NULL;

INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4) VALUES (790109, -1e-5, TRUE, 'chaos', TRUE);

INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_02_id) VALUES (361026, TRUE, FALSE, -1);

UPDATE tbl_07 SET col_2 = TRUE WHERE id > 83;

CREATE TABLE customers (
    customer_id INT PRIMARY KEY,
    region_code VARCHAR(8) NOT NULL,
    tier VARCHAR(16) NOT NULL
);

SELECT
    dt.customer_id,
    dt.avg_spent,
    c.region_code
FROM (
    SELECT o.customer_id, AVG(o.total_amount) AS avg_spent
    FROM orders o
    WHERE o.order_status = 'completed'
    GROUP BY o.customer_id
) dt
JOIN customers c ON dt.customer_id = c.customer_id
ORDER BY dt.avg_spent DESC
LIMIT 5;

DELETE FROM tbl_05 WHERE id = 85115;

INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (339368, -1e-5, 0.0, 'escaped''quote');

UPDATE tbl_08 SET col_2 = '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}' WHERE id > 0;

DELETE FROM tbl_10 WHERE id = 88684;

SELECT
    (EXISTS (SELECT 1 FROM tbl_09 AS sub_255 WHERE 1.0) >= t_ref_254.col_2) AS c_260,
    EXISTS (SELECT 1 FROM tbl_06 AS sub_261 WHERE FALSE) AS c_262,
    (NOT ((TRUE - 'chaos'))) AS c_263
FROM tbl_08 AS t_ref_254 LEFT JOIN tbl_13 AS j_ref_264 ON t_ref_254.id = j_ref_264.id
WHERE ((SELECT UPPER(((ABS((sub_265.id OR EXISTS (SELECT 1 FROM tbl_10 AS sub_266 WHERE sub_266.col_2))) / NULLIF(1.0, 0)) * (SELECT 'alpha_beta_gamma' FROM tbl_13 AS sub_268 LIMIT 1))) FROM tbl_12 AS sub_265 LIMIT 1) * t_ref_254.col_3)
LIMIT 1;

UPDATE tbl_03 SET col_2 = TRUE WHERE id > 75;

BEGIN;
INSERT INTO tbl_14 (id, col_1, col_2, ref_tbl_04_id) VALUES (175081, 0.0000000001, TRUE, 1);
INSERT INTO tbl_14 (id, col_1, col_2, ref_tbl_04_id) VALUES (734498, 1e10, TRUE, 2147483647);
COMMIT;

INSERT INTO orders (id, payload) VALUES (839482, 'ref_val');

UPDATE tbl_02 SET col_1 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 77;

SELECT
    (CASE WHEN (EXISTS (SELECT 1 FROM tbl_11 AS sub_270 WHERE sub_270.col_2) / NULLIF(LENGTH((EXISTS (SELECT 1 FROM tbl_11 AS sub_275 WHERE sub_275.id) > (t_ref_269.id > (LOWER((CAST(CAST((t_ref_269.id > 0.0000000001) AS DECIMAL(10,2)) AS SIGNED) BETWEEN 2147483647 AND -2147483648)) <= (SELECT UPPER((EXISTS (SELECT 1 FROM orders AS sub_280 WHERE NULL) - (sub_279.id > (EXISTS (SELECT 1 FROM tbl_11 AS sub_281 WHERE sub_281.col_2) IS NULL)))) FROM tbl_12 AS sub_279 LIMIT 1))))), 0)) THEN (NULLIF(NULLIF((((CASE WHEN (SELECT (SELECT inn_285.col_1 FROM tbl_04 AS inn_285 LIMIT 1) FROM tbl_08 AS sub_282 LIMIT 1) THEN t_ref_269.payload ELSE t_ref_269.id END) OR -1.5) - t_ref_269.id), t_ref_269.id), ((SELECT ((sub_286.col_3 IN (SELECT sub_287.ref_tbl_06_id FROM tbl_07 AS sub_287)) / NULLIF((sub_286.col_3 * sub_286.id), 0)) FROM tbl_10 AS sub_286 LIMIT 1) * EXISTS (SELECT 1 FROM tbl_10 AS sub_289 WHERE sub_289.col_2))) = t_ref_269.id) ELSE EXISTS (SELECT 1 FROM tbl_05 AS sub_290 WHERE sub_290.col_2) END) AS c_291,
    COALESCE('__fuzz_marker__', ((SELECT (FALSE AND (SELECT (SELECT inn_297.col_1 FROM tbl_05 AS inn_297 LIMIT 1) FROM orders AS sub_293 LIMIT 1)) FROM tbl_12 AS sub_292 LIMIT 1) BETWEEN 1 AND -9223372036854775808)) AS c_298,
    (CAST((NOT (LENGTH(((SELECT (SELECT FALSE FROM tbl_10 AS inn_303 LIMIT 1) FROM orders AS sub_299 LIMIT 1) >= (t_ref_269.payload - LOWER((-9223372036854775808 BETWEEN -1 AND -9223372036854775808))))))) AS CHAR) = (CASE WHEN (SELECT (SELECT 0.0000000001 FROM tbl_11 AS inn_309 LIMIT 1) FROM tbl_08 AS sub_304 LIMIT 1) THEN ABS(COALESCE(UPPER(EXISTS (SELECT 1 FROM tbl_12 AS sub_310 WHERE 1.0)), (CAST((t_ref_269.id * (t_ref_269.id BETWEEN 42 AND 1)) AS DECIMAL(10,2)) - t_ref_269.payload))) ELSE (CASE WHEN (NULL + (NULL + ((SELECT (SELECT inn_317.ref_tbl_06_id FROM tbl_07 AS inn_317 LIMIT 1) FROM tbl_03 AS sub_316 LIMIT 1) <= (CASE WHEN (CASE WHEN (SELECT (((sub_318.col_1 = sub_318.id) - LOWER(sub_318.id)) >= 1.0) FROM tbl_04 AS sub_318 LIMIT 1) THEN (COALESCE(LOWER(t_ref_269.payload), (((t_ref_269.payload - t_ref_269.id) <> (SELECT sub_319.col_2 FROM tbl_05 AS sub_319 LIMIT 1)) AND ((SELECT sub_320.col_2 FROM tbl_04 AS sub_320 LIMIT 1) - NULL))) - t_ref_269.id) ELSE t_ref_269.payload END) THEN CAST((((CASE WHEN t_ref_269.payload THEN (COALESCE(t_ref_269.id, t_ref_269.payload) = FALSE) ELSE (CASE WHEN t_ref_269.id THEN (SELECT (SELECT TRUE FROM tbl_02 AS inn_322 LIMIT 1) FROM tbl_08 AS sub_321 LIMIT 1) ELSE NULLIF(-9223372036854775808, NULL) END) END) * CAST(t_ref_269.payload AS DECIMAL(10,2))) OR (SELECT (SELECT inn_324.col_1 FROM tbl_06 AS inn_324 LIMIT 1) FROM tbl_03 AS sub_323 LIMIT 1)) AS SIGNED) ELSE (CAST(TRUE AS DECIMAL(10,2)) + (((SELECT sub_325.col_4 FROM tbl_10 AS sub_325 LIMIT 1) + (SELECT LOWER(sub_326.col_1) FROM tbl_01 AS sub_326 LIMIT 1)) * ABS('chaos'))) END)))) THEN LOWER((SELECT 0 FROM tbl_09 AS sub_327 LIMIT 1)) ELSE t_ref_269.id END) END)) AS c_328
FROM orders AS t_ref_269 LEFT JOIN tbl_06 AS j_ref_329 ON t_ref_269.id = j_ref_329.id
WHERE ABS((((SELECT (CASE WHEN (CAST((sub_330.col_2 + sub_330.col_2) AS DECIMAL(10,2)) / NULLIF(((NULLIF(LENGTH((100 * sub_330.col_2)), ((('alpha_beta_gamma' < sub_330.col_1) = (999999999.99 - sub_330.col_1)) <> (SELECT sub_331.id FROM tbl_14 AS sub_331 LIMIT 1))) >= (sub_330.col_2 IN (SELECT sub_332.id FROM tbl_13 AS sub_332))) * TRUE), 0)) THEN (CAST((NOT ((NULLIF(sub_330.col_1, (CASE WHEN sub_330.id THEN sub_330.col_2 ELSE (TRUE = sub_330.col_2) END)) AND CAST(sub_330.id AS SIGNED)))) AS DECIMAL(10,2)) + COALESCE(('escaped''quote' OR (CASE WHEN (SELECT (SELECT -1.5 FROM tbl_05 AS inn_335 LIMIT 1) FROM tbl_13 AS sub_334 LIMIT 1) THEN (((sub_330.col_1 IN (SELECT sub_336.id FROM tbl_02 AS sub_336)) >= (sub_330.id < sub_330.id)) < '') ELSE ((sub_330.col_3 IN (SELECT sub_337.payload FROM orders AS sub_337)) + (SELECT sub_338.id FROM tbl_09 AS sub_338 LIMIT 1)) END)), (CAST(LENGTH(-9223372036854775808) AS CHAR) - COALESCE((((sub_330.id IN (SELECT sub_339.col_3 FROM tbl_02 AS sub_339)) * (NULL - TRUE)) - sub_330.col_2), (FALSE IN (SELECT sub_340.id FROM tbl_08 AS sub_340)))))) ELSE ((sub_330.col_1 IN (SELECT sub_341.id FROM tbl_09 AS sub_341)) + EXISTS (SELECT 1 FROM tbl_07 AS sub_343 WHERE sub_343.id)) END) FROM tbl_11 AS sub_330 LIMIT 1) * (CASE WHEN (ABS(CAST(((0.0000000001 + ((CASE WHEN (t_ref_269.payload + (t_ref_269.id * t_ref_269.payload)) THEN ((NOT (t_ref_269.id)) * (t_ref_269.payload IS NULL)) ELSE (CASE WHEN LOWER(t_ref_269.id) THEN (t_ref_269.payload > t_ref_269.payload) ELSE t_ref_269.id END) END) <> (CAST(ABS(t_ref_269.payload) AS SIGNED) > CAST((t_ref_269.id * '') AS SIGNED)))) BETWEEN 0 AND -9223372036854775808) AS SIGNED)) / NULLIF(t_ref_269.id, 0)) THEN CAST('escaped''quote' AS DECIMAL(10,2)) ELSE CAST(((((CASE WHEN ((1.0 AND COALESCE(t_ref_269.payload, (NULL AND 'chaos'))) OR t_ref_269.id) THEN (t_ref_269.payload AND NULL) ELSE EXISTS (SELECT 1 FROM tbl_05 AS sub_344 WHERE sub_344.id) END) + (t_ref_269.payload - CAST((((t_ref_269.payload <= t_ref_269.payload) <> CAST(t_ref_269.payload AS SIGNED)) - t_ref_269.payload) AS DECIMAL(10,2)))) >= (CAST((((t_ref_269.id + (CASE WHEN 3.14159265 THEN t_ref_269.id ELSE t_ref_269.id END)) AND (CASE WHEN t_ref_269.payload THEN t_ref_269.id ELSE (t_ref_269.id > FALSE) END)) - ((t_ref_269.payload BETWEEN -2147483648 AND 9223372036854775807) - FALSE)) AS DECIMAL(10,2)) * ((((CAST(FALSE AS CHAR) / NULLIF((t_ref_269.id <> t_ref_269.payload), 0)) > -2147483648) / NULLIF(t_ref_269.payload, 0)) + ((CASE WHEN ((t_ref_269.payload - t_ref_269.id) * (t_ref_269.payload + t_ref_269.id)) THEN CAST(t_ref_269.id AS SIGNED) ELSE 0.0000000001 END) * (t_ref_269.payload IN (SELECT sub_345.col_1 FROM tbl_06 AS sub_345)))))) - (3.14159265 + t_ref_269.payload)) AS SIGNED) END)) < (3.14159265 IN (SELECT sub_346.col_3 FROM tbl_08 AS sub_346))))
LIMIT 32;

INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4) VALUES (586595, 1e10, FALSE, '', TRUE);

CREATE INDEX idx_orders_payload_8 ON orders (payload);

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_06 WHERE id IS NOT NULL;

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_05 WHERE id IS NOT NULL;

CREATE INDEX idx_tbl_04_col_1_9 ON tbl_04 (col_1);

CREATE INDEX idx_tbl_09_col_1_10 ON tbl_09 (col_1);

SELECT
    CAST(CAST((SELECT (CAST((sub_348.col_1 <> (SELECT (999999999.99 / NULLIF((((sub_349.col_2 + sub_349.col_1) - (100 + sub_349.col_3)) IS NOT NULL), 0)) FROM tbl_09 AS sub_349 LIMIT 1)) AS SIGNED) / NULLIF(((CAST((SELECT LENGTH(CAST((TRUE IN (SELECT sub_351.col_3 FROM tbl_10 AS sub_351)) AS DECIMAL(10,2))) FROM tbl_07 AS sub_350 LIMIT 1) AS CHAR) * ((CASE WHEN 1 THEN (((CASE WHEN sub_348.col_1 THEN NULL ELSE sub_348.col_1 END) - COALESCE((NULL / NULLIF(sub_348.id, 0)), 'alpha_beta_gamma')) <> (ABS(sub_348.id) < (CAST(FALSE AS DECIMAL(10,2)) < EXISTS (SELECT 1 FROM tbl_12 AS sub_352 WHERE sub_352.col_1)))) ELSE sub_348.id END) = CAST(CAST(CAST(CAST((CASE WHEN sub_348.col_1 THEN sub_348.id ELSE sub_348.col_1 END) AS CHAR) AS DECIMAL(10,2)) AS DECIMAL(10,2)) AS SIGNED))) - (SELECT (SELECT inn_354.id FROM tbl_13 AS inn_354 LIMIT 1) FROM tbl_12 AS sub_353 LIMIT 1)), 0)) FROM tbl_05 AS sub_348 LIMIT 1) AS SIGNED) AS CHAR) AS c_355,
    CAST((SELECT ((NULLIF(((SELECT (sub_357.id IN (SELECT sub_358.col_3 FROM tbl_02 AS sub_358)) FROM orders AS sub_357 LIMIT 1) <= ((LENGTH((sub_356.col_2 AND (CAST(sub_356.id AS CHAR) * ABS(sub_356.col_1)))) <= (CAST(((SELECT sub_359.col_1 FROM tbl_04 AS sub_359 LIMIT 1) - sub_356.id) AS CHAR) / NULLIF(NULLIF(UPPER((sub_356.id IS NOT NULL)), (CASE WHEN sub_356.col_2 THEN CAST('' AS CHAR) ELSE (NULL * sub_356.id) END)), 0))) <> (-1 >= sub_356.col_2))), (CASE WHEN (sub_356.col_1 IN (SELECT sub_360.col_1 FROM tbl_07 AS sub_360)) THEN CAST((sub_356.id IN (SELECT sub_361.id FROM tbl_05 AS sub_361)) AS CHAR) ELSE ((CASE WHEN sub_356.id THEN ((sub_356.col_1 - CAST((sub_356.col_1 AND sub_356.col_1) AS SIGNED)) * (LENGTH(EXISTS (SELECT 1 FROM tbl_13 AS sub_362 WHERE 'chaos')) - (CASE WHEN ('__fuzz_marker__' + sub_356.col_2) THEN LENGTH(TRUE) ELSE ('chaos' IN (SELECT sub_363.col_2 FROM tbl_04 AS sub_363)) END))) ELSE (9223372036854775807 >= CAST(NULL AS DECIMAL(10,2))) END) IS NOT NULL) END)) < (CAST('ref_val' AS CHAR) <> -2147483648)) / NULLIF(sub_356.id, 0)) FROM tbl_12 AS sub_356 LIMIT 1) AS DECIMAL(10,2)) AS c_364,
    t_ref_347.id AS c_365
FROM tbl_01 AS t_ref_347
WHERE CAST(t_ref_347.id AS SIGNED)
LIMIT 7;

INSERT INTO tbl_05 (id, col_1, col_2) VALUES (544602, '', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

INSERT INTO tbl_08 (id, col_1, col_2, col_3, dyn_col_5) VALUES (939363, FALSE, '[]', '__fuzz_marker__', 'escaped''quote');
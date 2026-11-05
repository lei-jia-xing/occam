-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: MYSQL

-- Statements: 100

-- AST Expression Depth: 15

-- Injected Defect Case: mysql_04

-- Seed: 10022



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 INT,
    col_3 JSON
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    col_3 NUMERIC(10,2)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSON,
    col_3 NUMERIC(10,2),
    col_4 INT
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT,
    col_3 VARCHAR(64),
    col_4 BOOLEAN
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 BOOLEAN,
    col_4 VARCHAR(64)
);

CREATE TABLE stock_ticks (
    tick_id INT PRIMARY KEY,
    symbol VARCHAR(8) NOT NULL,
    tick_time DATETIME NOT NULL,
    price_cents INT NOT NULL
);

UPDATE tbl_02 SET col_3 = 999999999.99 WHERE id > 72;

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (15407, -9223372036854775808, -9223372036854775808, 'alpha_beta_gamma', FALSE);

SELECT
    LENGTH(((SELECT (((sub_2.col_3 - (1 = (((CAST((999999999.99 IN (SELECT sub_3.col_2 FROM tbl_02 AS sub_3)) AS SIGNED) BETWEEN -1 AND 42) = (SELECT (SELECT inn_7.id FROM tbl_04 AS inn_7 LIMIT 1) FROM tbl_02 AS sub_4 LIMIT 1)) / NULLIF(CAST(NULLIF((sub_2.id IN (SELECT sub_8.id FROM stock_ticks AS sub_8)), (SELECT (SELECT inn_10.col_3 FROM tbl_01 AS inn_10 LIMIT 1) FROM tbl_05 AS sub_9 LIMIT 1)) AS DECIMAL(10,2)), 0)))) + -9223372036854775808) AND sub_2.id) FROM tbl_05 AS sub_2 LIMIT 1) / NULLIF(t_ref_1.col_3, 0))) AS c_11,
    ((SELECT (SELECT '' FROM tbl_01 AS inn_21 LIMIT 1) FROM tbl_04 AS sub_12 LIMIT 1) * (SELECT (SELECT FALSE FROM tbl_01 AS inn_23 LIMIT 1) FROM tbl_05 AS sub_22 LIMIT 1)) AS c_24
FROM tbl_03 AS t_ref_1
WHERE (((-9223372036854775808 - (SELECT (SELECT inn_30.id FROM stock_ticks AS inn_30 LIMIT 1) FROM tbl_03 AS sub_25 LIMIT 1)) - ('ref_val' * (UPPER((EXISTS (SELECT 1 FROM tbl_05 AS sub_31 WHERE sub_31.id) OR ((LENGTH(((NOT (((t_ref_1.col_4 + (NULL / NULLIF(t_ref_1.col_2, 0))) < FALSE))) AND ((t_ref_1.col_2 IN (SELECT sub_35.id FROM tbl_02 AS sub_35)) >= (CASE WHEN (SELECT (SELECT inn_39.col_1 FROM tbl_03 AS inn_39 LIMIT 1) FROM tbl_03 AS sub_38 LIMIT 1) THEN (SELECT (SELECT inn_41.payload FROM stock_ticks AS inn_41 LIMIT 1) FROM stock_ticks AS sub_40 LIMIT 1) ELSE COALESCE(9223372036854775807, (100 - t_ref_1.col_3)) END)))) - t_ref_1.col_2) - (CASE WHEN EXISTS (SELECT 1 FROM tbl_04 AS sub_42 WHERE sub_42.col_4) THEN (CASE WHEN t_ref_1.col_4 THEN (NOT ((LOWER(t_ref_1.col_2) BETWEEN -2147483648 AND -1))) ELSE COALESCE((NULL IN (SELECT sub_45.col_1 FROM tbl_05 AS sub_45)), LOWER((NOT ((t_ref_1.col_3 <= t_ref_1.col_3))))) END) ELSE (SELECT (SELECT inn_48.id FROM stock_ticks AS inn_48 LIMIT 1) FROM tbl_02 AS sub_47 LIMIT 1) END)))) - t_ref_1.id))) / NULLIF((CASE WHEN (SELECT EXISTS (SELECT 1 FROM stock_ticks AS sub_50 WHERE sub_50.payload) FROM tbl_04 AS sub_49 LIMIT 1) THEN (SELECT (sub_51.id IN (SELECT sub_52.col_2 FROM tbl_01 AS sub_52)) FROM tbl_01 AS sub_51 LIMIT 1) ELSE (t_ref_1.col_2 IN (SELECT sub_54.col_2 FROM tbl_01 AS sub_54)) END), 0))
LIMIT 28;

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 NUMERIC(10,2),
    col_3 INT
);

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (552091, '{}', -2147483648, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
COMMIT;

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (177595, 0, '{}', 3.14159265, 1);
COMMIT;

CREATE VIEW v_summary_1 AS SELECT id FROM stock_ticks WHERE id IS NOT NULL;

UPDATE tbl_01 SET col_3 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 32;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 INT,
    col_4 JSON
);

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (299682, 100, 0, 'alpha_beta_gamma', TRUE);
COMMIT;

INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (894233, '[]', 3.14159265, -1);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (347578, 0, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 0.0, 0);

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (198527, '[]', -1e-5, 9223372036854775807);
INSERT INTO stock_ticks (id, payload) VALUES (802966, '');
INSERT INTO tbl_02 (id, col_1, col_2, col_3) VALUES (364844, 'ref_val', 0, 1e10);
COMMIT;

DELETE FROM tbl_01 WHERE id = 32584;

UPDATE tbl_04 SET col_1 = 9223372036854775807 WHERE id > 68;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2, col_3, col_4) VALUES (286441, 'escaped''quote', '__fuzz_marker__', 100, '{}');
INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (918640, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -1.5, 0);
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (835597, -1, -2147483648, '', FALSE);
COMMIT;

CREATE INDEX idx_stock_ticks_id_1 ON stock_ticks (id);

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (870150, '[]', -1, '[]');
INSERT INTO stock_ticks (id, payload) VALUES (292793, 'chaos');
COMMIT;

UPDATE tbl_03 SET col_3 = 1e10 WHERE id > 26;

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (850984, 9223372036854775807, -1, '__fuzz_marker__', FALSE);

INSERT INTO stock_ticks (id, payload) VALUES (46083, 'chaos');

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

INSERT INTO stock_ticks (tick_id, symbol, tick_time, price_cents) VALUES
    (1, 'AAPL', '2023-10-01 09:30:00', 17500),
    (2, 'AAPL', '2023-10-01 09:30:05', 17520),
    (3, 'AAPL', '2023-10-01 09:30:10', 17490),
    (4, 'MSFT', '2023-10-01 09:30:00', 31500),
    (5, 'MSFT', '2023-10-01 09:30:05', 31550);

INSERT INTO tbl_02 (id, col_1, col_2, col_3) VALUES (473682, 'alpha_beta_gamma', 42, 1e10);

ALTER TABLE tbl_01 DROP COLUMN col_1;

SELECT
    ((((SELECT sub_56.col_2 FROM tbl_06 AS sub_56 LIMIT 1) / NULLIF((t_ref_55.id OR (SELECT (SELECT inn_58.col_3 FROM tbl_04 AS inn_58 LIMIT 1) FROM tbl_02 AS sub_57 LIMIT 1)), 0)) + ((CAST(CAST((COALESCE((NULL BETWEEN 42 AND -9223372036854775808), CAST(-9223372036854775808 AS DECIMAL(10,2))) IS NOT NULL) AS SIGNED) AS DECIMAL(10,2)) + (ABS((CASE WHEN CAST(EXISTS (SELECT 1 FROM tbl_06 AS sub_59 WHERE sub_59.col_3) AS SIGNED) THEN (((SELECT (NULLIF(sub_61.id, FALSE) - (SELECT NULL FROM tbl_07 AS sub_62 LIMIT 1)) FROM stock_ticks AS sub_61 LIMIT 1) > (CASE WHEN ((t_ref_55.col_2 + (SELECT sub_63.col_2 FROM tbl_01 AS sub_63 LIMIT 1)) < (t_ref_55.col_2 IN (SELECT sub_64.id FROM tbl_02 AS sub_64))) THEN CAST(LENGTH('chaos') AS DECIMAL(10,2)) ELSE ((COALESCE(t_ref_55.col_1, (t_ref_55.col_1 / NULLIF(t_ref_55.col_1, 0))) <= t_ref_55.col_1) - (t_ref_55.col_2 OR EXISTS (SELECT 1 FROM tbl_01 AS sub_65 WHERE sub_65.id))) END)) * (CASE WHEN CAST((CASE WHEN t_ref_55.col_1 THEN (1 * t_ref_55.id) ELSE CAST(((t_ref_55.col_1 * -2147483648) >= CAST(NULL AS CHAR)) AS SIGNED) END) AS CHAR) THEN (((t_ref_55.col_3 IN (SELECT sub_66.col_2 FROM tbl_03 AS sub_66)) / NULLIF((t_ref_55.col_1 IN (SELECT sub_68.col_1 FROM tbl_06 AS sub_68)), 0)) < (SELECT sub_71.id FROM tbl_04 AS sub_71 LIMIT 1)) ELSE CAST((t_ref_55.col_1 IN (SELECT sub_72.col_1 FROM tbl_02 AS sub_72)) AS CHAR) END)) ELSE (0.0000000001 + t_ref_55.col_1) END)) <> EXISTS (SELECT 1 FROM tbl_07 AS sub_74 WHERE TRUE))) >= FALSE)) / NULLIF((t_ref_55.col_3 OR FALSE), 0)) AS c_80,
    (CASE WHEN (EXISTS (SELECT 1 FROM tbl_03 AS sub_81 WHERE sub_81.col_3) / NULLIF((NULL - (CAST((t_ref_55.col_2 >= t_ref_55.id) AS CHAR) * (((SELECT CAST((CASE WHEN sub_100.col_2 THEN 999999999.99 ELSE (CASE WHEN (SELECT (SELECT inn_102.col_3 FROM tbl_07 AS inn_102 LIMIT 1) FROM tbl_02 AS sub_101 LIMIT 1) THEN (((sub_100.col_2 / NULLIF(1e10, 0)) + (TRUE >= sub_100.col_3)) * ((SELECT sub_103.col_2 FROM tbl_03 AS sub_103 LIMIT 1) > EXISTS (SELECT 1 FROM tbl_03 AS sub_104 WHERE sub_104.col_4))) ELSE CAST(((FALSE + sub_100.col_3) > (sub_100.col_2 = sub_100.col_3)) AS CHAR) END) END) AS SIGNED) FROM tbl_02 AS sub_100 LIMIT 1) AND (9223372036854775807 < (SELECT CAST(CAST(CAST(NULLIF(sub_105.payload, CAST(sub_105.payload AS DECIMAL(10,2))) AS CHAR) AS DECIMAL(10,2)) AS SIGNED) FROM stock_ticks AS sub_105 LIMIT 1))) + t_ref_55.id))), 0)) THEN (CAST(((((SELECT (SELECT 0.0000000001 FROM tbl_03 AS inn_109 LIMIT 1) FROM tbl_07 AS sub_106 LIMIT 1) + t_ref_55.col_3) <> (SELECT (SELECT inn_116.col_1 FROM tbl_02 AS inn_116 LIMIT 1) FROM tbl_02 AS sub_110 LIMIT 1)) / NULLIF(((SELECT EXISTS (SELECT 1 FROM tbl_05 AS sub_118 WHERE -1.5) FROM tbl_06 AS sub_117 LIMIT 1) <= LENGTH((CASE WHEN (t_ref_55.col_1 IN (SELECT sub_119.col_3 FROM tbl_04 AS sub_119)) THEN NULLIF(t_ref_55.col_3, t_ref_55.id) ELSE (t_ref_55.col_3 IN (SELECT sub_120.col_3 FROM tbl_04 AS sub_120)) END))), 0)) AS SIGNED) AND (SELECT ((LOWER((((sub_124.payload * CAST(EXISTS (SELECT 1 FROM tbl_07 AS sub_125 WHERE TRUE) AS SIGNED)) + ((SELECT sub_126.col_2 FROM tbl_04 AS sub_126 LIMIT 1) + (NOT ('')))) > EXISTS (SELECT 1 FROM tbl_04 AS sub_127 WHERE sub_127.id))) BETWEEN 0 AND -1) + sub_124.payload) FROM stock_ticks AS sub_124 LIMIT 1)) ELSE NULL END) AS c_128,
    (t_ref_55.col_3 IN (SELECT sub_129.col_1 FROM tbl_05 AS sub_129)) AS c_130
FROM tbl_06 AS t_ref_55 LEFT JOIN tbl_04 AS j_ref_131 ON t_ref_55.id = j_ref_131.id
WHERE 0
LIMIT 37;

UPDATE tbl_06 SET col_1 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 39;

INSERT INTO tbl_01 (id, col_2, col_3) VALUES (66268, -2147483648, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

CREATE INDEX idx_tbl_05_col_3_2 ON tbl_05 (col_3);

INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (616349, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 1e10, 1);

INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (142427, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 0.0000000001, 42);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (658476, 0, 100, 'chaos', FALSE);

ALTER TABLE tbl_05 DROP COLUMN col_4;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2, col_3, col_4) VALUES (274219, '', 'ref_val', 42, '[]');
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (835345, 9223372036854775807, '{}', 0.0000000001, 9223372036854775807);
COMMIT;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2, col_3, col_4) VALUES (471267, '__fuzz_marker__', 'alpha_beta_gamma', -1, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (331942, 1.0, 'escaped''quote', FALSE);
INSERT INTO tbl_07 (id, col_1, col_2, col_3, col_4) VALUES (458455, 'ref_val', 'chaos', 100, '[]');
COMMIT;

INSERT INTO tbl_02 (id, col_1, col_2, col_3) VALUES (246587, '', 0, 0.0000000001);

SELECT
    (EXISTS (SELECT 1 FROM tbl_01 AS sub_133 WHERE -1.5) < '__fuzz_marker__') AS c_140
FROM tbl_03 AS t_ref_132
WHERE (CASE WHEN t_ref_132.col_4 THEN t_ref_132.col_2 ELSE (('__fuzz_marker__' - TRUE) <> t_ref_132.col_2) END)
LIMIT 9;

SELECT
    ((SELECT (EXISTS (SELECT 1 FROM tbl_01 AS sub_143 WHERE sub_143.col_3) < CAST(LOWER(CAST('' AS SIGNED)) AS SIGNED)) FROM tbl_01 AS sub_142 LIMIT 1) * (CAST('alpha_beta_gamma' AS CHAR) IS NOT NULL)) AS c_144,
    ((LENGTH(ABS(CAST(t_ref_141.payload AS CHAR))) / NULLIF((t_ref_141.id * (COALESCE(CAST(t_ref_141.id AS CHAR), t_ref_141.payload) <> CAST((SELECT (sub_145.col_3 / NULLIF((sub_145.col_1 = (SELECT ((LENGTH(sub_146.col_2) IS NULL) * ((sub_146.id / NULLIF(sub_146.col_2, 0)) BETWEEN 1 AND 1)) FROM tbl_04 AS sub_146 LIMIT 1)), 0)) FROM tbl_02 AS sub_145 LIMIT 1) AS DECIMAL(10,2)))), 0)) + ((t_ref_141.id * (t_ref_141.payload IN (SELECT sub_147.col_2 FROM tbl_01 AS sub_147))) IS NOT NULL)) AS c_156
FROM stock_ticks AS t_ref_141
WHERE ('ref_val' OR NULL)
LIMIT 50;

DELETE FROM stock_ticks WHERE id = 22151;

INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (94614, '{}', 1e10, 100);

SELECT
    CAST(NULL AS CHAR) AS c_158,
    ((SELECT (SELECT inn_166.id FROM stock_ticks AS inn_166 LIMIT 1) FROM tbl_03 AS sub_159 LIMIT 1) * (t_ref_157.col_2 IN (SELECT sub_167.id FROM tbl_01 AS sub_167))) AS c_168
FROM tbl_03 AS t_ref_157 LEFT JOIN tbl_07 AS j_ref_169 ON t_ref_157.id = j_ref_169.id
WHERE CAST((CASE WHEN NULL THEN ((EXISTS (SELECT 1 FROM tbl_06 AS sub_170 WHERE 1e10) + (CASE WHEN LOWER(t_ref_157.col_3) THEN NULLIF(CAST(LOWER(COALESCE(t_ref_157.col_4, ((CAST((COALESCE('', t_ref_157.col_3) OR (t_ref_157.col_1 < t_ref_157.col_2)) AS SIGNED) / NULLIF(COALESCE(LENGTH(NULLIF(-2147483648, 100)), EXISTS (SELECT 1 FROM tbl_03 AS sub_174 WHERE sub_174.col_3)), 0)) + ((EXISTS (SELECT 1 FROM tbl_01 AS sub_175 WHERE sub_175.id) BETWEEN 2147483647 AND -9223372036854775808) < LENGTH((EXISTS (SELECT 1 FROM tbl_01 AS sub_176 WHERE sub_176.col_3) OR (t_ref_157.id + t_ref_157.col_2))))))) AS CHAR), (((SELECT CAST(sub_177.col_2 AS DECIMAL(10,2)) FROM tbl_01 AS sub_177 LIMIT 1) <= EXISTS (SELECT 1 FROM tbl_02 AS sub_178 WHERE sub_178.col_3)) / NULLIF((t_ref_157.col_1 = EXISTS (SELECT 1 FROM tbl_02 AS sub_180 WHERE sub_180.col_2)), 0))) ELSE CAST(NULLIF(EXISTS (SELECT 1 FROM stock_ticks AS sub_183 WHERE FALSE), (CASE WHEN TRUE THEN (FALSE IN (SELECT sub_184.col_3 FROM tbl_01 AS sub_184)) ELSE CAST(NULLIF((((EXISTS (SELECT 1 FROM tbl_02 AS sub_185 WHERE -1.5) <= (SELECT (SELECT inn_187.col_2 FROM tbl_01 AS inn_187 LIMIT 1) FROM tbl_05 AS sub_186 LIMIT 1)) - (COALESCE(t_ref_157.col_3, t_ref_157.col_4) + (t_ref_157.col_3 IS NULL))) + t_ref_157.col_2), ((CASE WHEN (EXISTS (SELECT 1 FROM tbl_02 AS sub_188 WHERE sub_188.col_1) / NULLIF(CAST(t_ref_157.col_4 AS DECIMAL(10,2)), 0)) THEN t_ref_157.col_2 ELSE (SELECT -2147483648 FROM tbl_03 AS sub_189 LIMIT 1) END) OR (NULLIF(TRUE, CAST(t_ref_157.col_1 AS CHAR)) <> -1.5))) AS SIGNED) END)) AS DECIMAL(10,2)) END)) / NULLIF(COALESCE((((CASE WHEN ABS((0 / NULLIF(t_ref_157.col_2, 0))) THEN EXISTS (SELECT 1 FROM tbl_06 AS sub_190 WHERE sub_190.col_1) ELSE (((t_ref_157.col_1 IN (SELECT sub_191.col_3 FROM tbl_04 AS sub_191)) + CAST((ABS(ABS(FALSE)) + (SELECT LENGTH('') FROM tbl_02 AS sub_192 LIMIT 1)) AS SIGNED)) >= t_ref_157.col_3) END) + ((CAST(EXISTS (SELECT 1 FROM tbl_07 AS sub_193 WHERE 'alpha_beta_gamma') AS SIGNED) * LOWER((CAST((SELECT (SELECT inn_196.col_2 FROM tbl_03 AS inn_196 LIMIT 1) FROM tbl_06 AS sub_195 LIMIT 1) AS DECIMAL(10,2)) <= (999999999.99 IN (SELECT sub_197.col_2 FROM tbl_03 AS sub_197))))) OR t_ref_157.col_2)) <= (t_ref_157.id >= ((UPPER(t_ref_157.col_1) < LENGTH(0)) IS NOT NULL))), (LENGTH(t_ref_157.col_3) + (SELECT (SELECT NULL FROM tbl_02 AS inn_203 LIMIT 1) FROM stock_ticks AS sub_199 LIMIT 1))), 0)) ELSE NULLIF(3.14159265, t_ref_157.col_3) END) AS SIGNED)
LIMIT 24;

CREATE INDEX idx_tbl_05_id_3 ON tbl_05 (id);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (338374, 0, '[]', 1e10, -1);

INSERT INTO tbl_02 (id, col_1, col_2, col_3) VALUES (367283, 'alpha_beta_gamma', -9223372036854775808, 1.0);

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (773416, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 1.0, -2147483648);
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (349337, 9223372036854775807, -2147483648, 'ref_val', FALSE);
COMMIT;

UPDATE tbl_04 SET col_3 = 'ref_val' WHERE id > 22;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2)
);

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, col_3) VALUES (634500, 'alpha_beta_gamma', -1, -1.5);
INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (802494, 999999999.99, 'ref_val', TRUE);
COMMIT;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

SELECT
    CAST((SELECT LOWER((CAST(NULL AS DECIMAL(10,2)) * sub_205.col_3)) FROM tbl_02 AS sub_205 LIMIT 1) AS CHAR) AS c_206,
    TRUE AS c_207,
    t_ref_204.col_3 AS c_208
FROM tbl_05 AS t_ref_204
WHERE ((CASE WHEN (FALSE IN (SELECT sub_209.col_2 FROM tbl_08 AS sub_209)) THEN (SELECT (SELECT inn_211.col_3 FROM tbl_04 AS inn_211 LIMIT 1) FROM tbl_08 AS sub_210 LIMIT 1) ELSE (SELECT CAST(EXISTS (SELECT 1 FROM tbl_01 AS sub_213 WHERE sub_213.id) AS CHAR) FROM tbl_03 AS sub_212 LIMIT 1) END) > EXISTS (SELECT 1 FROM tbl_02 AS sub_215 WHERE -1e-5))
LIMIT 27;

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 INT
);

INSERT INTO tbl_01 (id, col_2, col_3) VALUES (229273, 2147483647, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

DELETE FROM tbl_02 WHERE id = 76322;

SELECT
    ((LOWER((SELECT (SELECT inn_218.id FROM tbl_09 AS inn_218 LIMIT 1) FROM tbl_02 AS sub_217 LIMIT 1)) IS NULL) + ((t_ref_216.col_3 OR TRUE) * (CASE WHEN NULLIF(((-1 + LENGTH((COALESCE(t_ref_216.id, (t_ref_216.col_1 IN (SELECT sub_219.id FROM tbl_03 AS sub_219))) < 42))) - CAST(((999999999.99 > t_ref_216.col_2) / NULLIF(('ref_val' IN (SELECT sub_222.col_2 FROM tbl_01 AS sub_222)), 0)) AS DECIMAL(10,2))), (SELECT (SELECT inn_226.id FROM tbl_06 AS inn_226 LIMIT 1) FROM tbl_05 AS sub_225 LIMIT 1)) THEN ((EXISTS (SELECT 1 FROM tbl_06 AS sub_227 WHERE -1e-5) * ((t_ref_216.col_3 + (CASE WHEN t_ref_216.id THEN NULLIF(0, t_ref_216.col_1) ELSE ((SELECT (SELECT -1.5 FROM tbl_06 AS inn_234 LIMIT 1) FROM tbl_04 AS sub_230 LIMIT 1) AND CAST((SELECT (CASE WHEN (sub_235.col_1 >= NULL) THEN TRUE ELSE sub_235.id END) FROM tbl_10 AS sub_235 LIMIT 1) AS DECIMAL(10,2))) END)) = ((t_ref_216.col_3 IN (SELECT sub_236.col_3 FROM tbl_01 AS sub_236)) + (CASE WHEN (CAST(CAST((CASE WHEN NULLIF(NULLIF(t_ref_216.col_2, t_ref_216.id), (SELECT (SELECT TRUE FROM tbl_06 AS inn_238 LIMIT 1) FROM tbl_04 AS sub_237 LIMIT 1)) THEN t_ref_216.col_1 ELSE (NULL < t_ref_216.col_1) END) AS DECIMAL(10,2)) AS SIGNED) OR (CASE WHEN LENGTH(EXISTS (SELECT 1 FROM tbl_04 AS sub_239 WHERE sub_239.col_2)) THEN (SELECT (SELECT '' FROM tbl_05 AS inn_242 LIMIT 1) FROM tbl_01 AS sub_241 LIMIT 1) ELSE NULLIF((((t_ref_216.id BETWEEN 2147483647 AND 100) * (-1 IN (SELECT sub_243.col_2 FROM tbl_04 AS sub_243))) <> (CASE WHEN (t_ref_216.col_3 BETWEEN 100 AND 0) THEN (t_ref_216.col_3 BETWEEN 9223372036854775807 AND 0) ELSE (t_ref_216.col_2 / NULLIF(t_ref_216.col_2, 0)) END)), 'escaped''quote') END)) THEN (CASE WHEN LENGTH((CASE WHEN ((NOT ((t_ref_216.id OR t_ref_216.id))) + (t_ref_216.col_2 + COALESCE(t_ref_216.col_3, NULL))) THEN LOWER(COALESCE(LENGTH(t_ref_216.col_3), t_ref_216.col_3)) ELSE (t_ref_216.col_2 IN (SELECT sub_244.col_3 FROM tbl_02 AS sub_244)) END)) THEN 'alpha_beta_gamma' ELSE t_ref_216.col_1 END) ELSE CAST((LENGTH(((TRUE IN (SELECT sub_245.col_1 FROM tbl_05 AS sub_245)) OR LOWER((t_ref_216.col_1 < t_ref_216.col_2)))) > (t_ref_216.col_2 IN (SELECT sub_246.col_1 FROM tbl_03 AS sub_246))) AS DECIMAL(10,2)) END)))) * TRUE) ELSE (SELECT (SELECT inn_249.col_3 FROM tbl_03 AS inn_249 LIMIT 1) FROM tbl_05 AS sub_248 LIMIT 1) END))) AS c_250,
    (((ABS((SELECT (SELECT inn_252.col_2 FROM tbl_04 AS inn_252 LIMIT 1) FROM tbl_04 AS sub_251 LIMIT 1)) * (ABS((SELECT (SELECT TRUE FROM tbl_08 AS inn_255 LIMIT 1) FROM tbl_05 AS sub_253 LIMIT 1)) >= (ABS((100 IN (SELECT sub_256.col_3 FROM tbl_10 AS sub_256))) <> (UPPER(EXISTS (SELECT 1 FROM tbl_09 AS sub_258 WHERE TRUE)) <= (COALESCE(LOWER((((CAST(CAST('escaped''quote' AS SIGNED) AS SIGNED) + t_ref_216.id) / NULLIF((0.0 = (t_ref_216.col_2 IN (SELECT sub_261.id FROM tbl_04 AS sub_261))), 0)) OR ((CAST((t_ref_216.col_1 - FALSE) AS SIGNED) IS NOT NULL) / NULLIF(CAST((NOT (FALSE)) AS CHAR), 0)))), (t_ref_216.col_2 IN (SELECT sub_262.col_4 FROM tbl_04 AS sub_262))) + (CASE WHEN (SELECT (SELECT 2147483647 FROM tbl_10 AS inn_267 LIMIT 1) FROM tbl_07 AS sub_264 LIMIT 1) THEN t_ref_216.col_1 ELSE 42 END)))))) + (t_ref_216.col_1 + -1)) * (ABS((t_ref_216.col_3 IN (SELECT sub_268.id FROM stock_ticks AS sub_268))) - (t_ref_216.id = (CAST((SELECT ((ABS(EXISTS (SELECT 1 FROM tbl_10 AS sub_273 WHERE sub_273.col_2)) <> LENGTH(CAST(TRUE AS CHAR))) IS NULL) FROM tbl_10 AS sub_272 LIMIT 1) AS CHAR) / NULLIF((COALESCE(t_ref_216.id, (t_ref_216.id * (t_ref_216.col_1 - (((100 IN (SELECT sub_274.col_1 FROM tbl_06 AS sub_274)) <> ((100 IN (SELECT sub_275.col_1 FROM tbl_05 AS sub_275)) * EXISTS (SELECT 1 FROM tbl_09 AS sub_276 WHERE FALSE))) IS NULL)))) < ((CASE WHEN t_ref_216.col_2 THEN LOWER('') ELSE (SELECT (SELECT inn_279.id FROM tbl_02 AS inn_279 LIMIT 1) FROM tbl_09 AS sub_278 LIMIT 1) END) + (LOWER(CAST((42 AND (t_ref_216.col_1 = COALESCE(TRUE, 'chaos'))) AS CHAR)) >= LOWER(t_ref_216.id)))), 0))))) AS c_280
FROM tbl_10 AS t_ref_216 LEFT JOIN tbl_02 AS j_ref_281 ON t_ref_216.id = j_ref_281.id
WHERE ((SELECT (SELECT 'alpha_beta_gamma' FROM tbl_05 AS inn_283 LIMIT 1) FROM tbl_06 AS sub_282 LIMIT 1) OR (CASE WHEN t_ref_216.col_1 THEN (t_ref_216.col_1 IN (SELECT sub_284.id FROM tbl_09 AS sub_284)) ELSE CAST(t_ref_216.col_2 AS DECIMAL(10,2)) END))
LIMIT 41;

UPDATE tbl_10 SET col_3 = -9223372036854775808 WHERE id > 65;

UPDATE tbl_01 SET col_2 = -1 WHERE id > 28;

SELECT
    t_ref_289.col_3 AS c_290,
    (FALSE >= CAST(NULL AS SIGNED)) AS c_291
FROM tbl_07 AS t_ref_289
WHERE (CASE WHEN t_ref_289.col_2 THEN (t_ref_289.col_4 IN (SELECT sub_292.col_3 FROM tbl_02 AS sub_292)) ELSE COALESCE((SELECT (SELECT TRUE FROM tbl_02 AS inn_334 LIMIT 1) FROM stock_ticks AS sub_318 LIMIT 1), t_ref_289.id) END)
LIMIT 30;

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3) VALUES (634715, 1e10, 'ref_val', -1.5);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (403823, -1, '[]', 999999999.99, -9223372036854775808);
INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (555143, 1e10, 'chaos', FALSE);
COMMIT;

UPDATE tbl_07 SET col_3 = -2147483648 WHERE id > 20;

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (503550, -1e-5, '__fuzz_marker__', FALSE);

INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (821404, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 1e10, -1);

UPDATE tbl_10 SET col_1 = 1 WHERE id > 56;

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 INT,
    col_4 JSON,
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 INT,
    col_3 BOOLEAN
);

INSERT INTO stock_ticks (id, payload) VALUES (303651, 'escaped''quote');

CREATE INDEX idx_tbl_01_col_2_4 ON tbl_01 (col_2);

SELECT
    FALSE AS c_336,
    (COALESCE((t_ref_335.col_1 IN (SELECT sub_337.col_1 FROM tbl_09 AS sub_337)), ((((UPPER(CAST(1 AS DECIMAL(10,2))) / NULLIF(ABS((((t_ref_335.col_1 BETWEEN -9223372036854775808 AND 100) BETWEEN -1 AND 42) >= (NOT (t_ref_335.col_2)))), 0)) IS NOT NULL) + '') OR LOWER(NULLIF((CAST((CASE WHEN (1 - (SELECT (((sub_338.id / NULLIF(sub_338.payload, 0)) * (SELECT (SELECT inn_340.id FROM tbl_07 AS inn_340 LIMIT 1) FROM stock_ticks AS sub_339 LIMIT 1)) / NULLIF(CAST((sub_338.id / NULLIF(sub_338.payload, 0)) AS CHAR), 0)) FROM stock_ticks AS sub_338 LIMIT 1)) THEN (t_ref_335.col_1 IN (SELECT sub_341.col_1 FROM tbl_04 AS sub_341)) ELSE (CASE WHEN ((CASE WHEN (t_ref_335.id IN (SELECT sub_344.col_1 FROM tbl_05 AS sub_344)) THEN (((t_ref_335.col_1 + t_ref_335.id) IS NOT NULL) < ((SELECT sub_345.col_1 FROM tbl_06 AS sub_345 LIMIT 1) AND (t_ref_335.col_1 < t_ref_335.id))) ELSE t_ref_335.col_2 END) - (SELECT (EXISTS (SELECT 1 FROM tbl_05 AS sub_347 WHERE -9223372036854775808) - (sub_346.col_3 <= 999999999.99)) FROM tbl_04 AS sub_346 LIMIT 1)) THEN t_ref_335.id ELSE '' END) END) AS DECIMAL(10,2)) * (((((CASE WHEN t_ref_335.id THEN (NULL / NULLIF(((t_ref_335.col_2 IN (SELECT sub_348.col_2 FROM tbl_09 AS sub_348)) / NULLIF(t_ref_335.id, 0)), 0)) ELSE (LOWER(COALESCE(1e10, NULL)) / NULLIF(CAST((SELECT sub_349.col_4 FROM tbl_03 AS sub_349 LIMIT 1) AS CHAR), 0)) END) < LOWER(t_ref_335.col_3)) AND t_ref_335.col_1) + (((CAST(NULLIF(CAST(42 AS DECIMAL(10,2)), t_ref_335.col_3) AS SIGNED) OR 1e10) <> ((t_ref_335.id + (t_ref_335.col_3 IN (SELECT sub_350.id FROM tbl_05 AS sub_350))) >= t_ref_335.col_2)) + (SELECT (SELECT inn_352.col_3 FROM tbl_01 AS inn_352 LIMIT 1) FROM tbl_02 AS sub_351 LIMIT 1))) / NULLIF((SELECT (SELECT TRUE FROM tbl_02 AS inn_356 LIMIT 1) FROM tbl_11 AS sub_353 LIMIT 1), 0))), (((CASE WHEN ((NOT (t_ref_335.col_3)) OR t_ref_335.id) THEN ((t_ref_335.col_3 * t_ref_335.id) * (ABS(t_ref_335.col_3) AND FALSE)) ELSE ('escaped''quote' IN (SELECT sub_357.col_2 FROM tbl_06 AS sub_357)) END) AND (t_ref_335.col_2 - (ABS(((SELECT CAST(sub_358.col_1 AS DECIMAL(10,2)) FROM tbl_02 AS sub_358 LIMIT 1) OR TRUE)) * (CAST(CAST((SELECT (SELECT inn_360.col_1 FROM tbl_07 AS inn_360 LIMIT 1) FROM tbl_08 AS sub_359 LIMIT 1) AS CHAR) AS SIGNED) AND ((COALESCE((t_ref_335.col_2 * t_ref_335.col_2), LOWER(t_ref_335.col_2)) BETWEEN -1 AND 100) > (EXISTS (SELECT 1 FROM tbl_01 AS sub_361 WHERE NULL) < 1.0)))))) - ABS(CAST(t_ref_335.id AS CHAR))))))) < t_ref_335.col_3) AS c_362
FROM tbl_02 AS t_ref_335
WHERE (SELECT CAST(sub_363.col_2 AS SIGNED) FROM tbl_08 AS sub_363 LIMIT 1)
LIMIT 11;

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 INT,
    col_4 VARCHAR(64)
);

INSERT INTO tbl_11 (id, col_1, col_2, ref_tbl_03_id) VALUES (319735, -9223372036854775808, '__fuzz_marker__', -9223372036854775808);

UPDATE tbl_12 SET col_4 = '[]' WHERE id > 80;

BEGIN;
INSERT INTO tbl_01 (id, col_2, col_3) VALUES (950168, -9223372036854775808, '[]');
INSERT INTO tbl_13 (id, col_1, col_2, col_3) VALUES (598475, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 0, TRUE);
COMMIT;

CREATE TABLE stock_ticks (
    tick_id INT PRIMARY KEY,
    symbol VARCHAR(8) NOT NULL,
    tick_time DATETIME NOT NULL,
    price_cents INT NOT NULL
);

SELECT
    tick_id,
    symbol,
    tick_time,
    price_cents,
    LEAD(price_cents, 1, 0) OVER (
        PARTITION BY symbol
        ORDER BY tick_time
    ) AS next_price,
    LAG(price_cents, 1, NULL) OVER (
        PARTITION BY symbol
        ORDER BY tick_time
    ) AS prev_price
FROM stock_ticks
WHERE symbol IN ('AAPL', 'MSFT');

BEGIN;
INSERT INTO tbl_14 (id, col_1, col_2, col_3, col_4) VALUES (380599, 0.0, 1.0, 9223372036854775807, 'escaped''quote');
COMMIT;

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 BOOLEAN,
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

DELETE FROM tbl_01 WHERE id = 86879;

CREATE INDEX idx_tbl_12_col_3_5 ON tbl_12 (col_3);

INSERT INTO tbl_11 (id, col_1, col_2, ref_tbl_03_id) VALUES (692389, 1, 'ref_val', 9223372036854775807);

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON,
    ref_tbl_08_id INT,
    FOREIGN KEY (ref_tbl_08_id) REFERENCES tbl_08(id)
);

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (849857, -2147483648, 1, 'escaped''quote', TRUE);
INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (422528, 42, '', 2147483647);
COMMIT;

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_16 WHERE id IS NOT NULL;

INSERT INTO stock_ticks (id, payload) VALUES (595033, 'escaped''quote');

INSERT INTO tbl_02 (id, col_1, col_2, col_3) VALUES (55157, 'alpha_beta_gamma', 2147483647, -1.5);

CREATE TABLE tbl_17 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    ref_tbl_12_id INT,
    FOREIGN KEY (ref_tbl_12_id) REFERENCES tbl_12(id)
);

SELECT
    (COALESCE(t_ref_364.col_1, (CAST((t_ref_364.col_3 IN (SELECT sub_365.col_2 FROM tbl_10 AS sub_365)) AS DECIMAL(10,2)) BETWEEN 42 AND 42)) BETWEEN -9223372036854775808 AND 0) AS c_366
FROM tbl_13 AS t_ref_364
WHERE (SELECT (SELECT inn_369.col_2 FROM tbl_16 AS inn_369 LIMIT 1) FROM tbl_05 AS sub_367 LIMIT 1)
LIMIT 3;

CREATE TABLE tbl_18 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2),
    col_4 VARCHAR(64),
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

CREATE INDEX idx_tbl_17_id_6 ON tbl_17 (id);

CREATE INDEX idx_tbl_08_col_2_7 ON tbl_08 (col_2);

UPDATE tbl_05 SET col_1 = -1e-5 WHERE id > 1;

CREATE TABLE tbl_19 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 BOOLEAN,
    col_3 NUMERIC(10,2)
);

INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (442860, -2147483648, 'alpha_beta_gamma', 2147483647);

INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (369806, 2147483647, '', 9223372036854775807);

CREATE TABLE tbl_20 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    col_3 INT,
    col_4 INT
);
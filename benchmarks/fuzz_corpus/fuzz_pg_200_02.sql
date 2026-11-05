-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: POSTGRES

-- Statements: 200

-- AST Expression Depth: 20

-- Injected Defect Case: pg_12

-- Seed: 20012



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 BOOLEAN,
    col_3 BOOLEAN,
    col_4 VARCHAR(64)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    col_3 BOOLEAN,
    col_4 NUMERIC(10,2)
);

CREATE VIEW v_summary_1 AS SELECT id FROM alpha_tbl WHERE id IS NOT NULL;

CREATE TABLE alpha_tbl (
    a_id INT PRIMARY KEY,
    val_a INT
);

DELETE FROM tbl_01 WHERE id = 70481;

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64)
);

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 JSONB,
    col_4 JSONB
);

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (893899, 'ref_val', FALSE, '[]', '{}');
COMMIT;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSONB,
    col_3 NUMERIC(10,2),
    col_4 NUMERIC(10,2)
);

ALTER TABLE tbl_03 ADD COLUMN dyn_col_5 VARCHAR(32);

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_07 WHERE id IS NOT NULL;

CREATE INDEX idx_tbl_06_id_1 ON tbl_06 (id);

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (310556, 'ref_val', TRUE, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{}');
INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (272467, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 999999999.99, 'escaped''quote');
COMMIT;

SELECT
    ((0.0000000001 IN (SELECT sub_2.id FROM tbl_05 AS sub_2)) - (NOT (t_ref_1.col_2))) AS c_19,
    t_ref_1.col_2 AS c_20
FROM tbl_06 AS t_ref_1
WHERE t_ref_1.col_1
LIMIT 2;

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (81341, 0, FALSE, TRUE, '');
COMMIT;

INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_02_id, dyn_col_5) VALUES (463206, TRUE, 1e10, -2147483648, '__fuzz_marker__');

INSERT INTO tbl_02 (id, col_1, col_2) VALUES (91059, 'ref_val', 'ref_val');

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (176066, '{}', 999999999.99, '');

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (185976, 1, FALSE, TRUE, '__fuzz_marker__');

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2),
    col_3 INT,
    col_4 JSONB
);

DELETE FROM tbl_03 WHERE id = 71228;

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_07 WHERE id IS NOT NULL;

CREATE TABLE beta_tbl (
    b_id INT PRIMARY KEY,
    a_ref INT REFERENCES alpha_tbl(a_id),
    val_b INT
);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (787571, 'escaped''quote', 0, FALSE, -1e-5);

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSONB
);

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSONB,
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (405005, 'chaos', FALSE, '[]', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

SELECT
    ABS(t_ref_21.col_1) AS c_22,
    t_ref_21.col_1 AS c_23
FROM tbl_08 AS t_ref_21 LEFT JOIN tbl_04 AS j_ref_24 ON t_ref_21.id = j_ref_24.id
WHERE (((t_ref_21.col_2 - (t_ref_21.col_2 IN (SELECT sub_25.col_2 FROM tbl_09 AS sub_25)))::INT::TEXT) < (((LOWER((((SELECT (SELECT sub_40.col_3 FROM tbl_07 AS sub_40 LIMIT 1) FROM tbl_09 AS sub_39 LIMIT 1)::TEXT) * (((SELECT (SELECT inn_46.id FROM tbl_04 AS inn_46 LIMIT 1) FROM tbl_01 AS sub_41 LIMIT 1) * UPPER(((NOT ((CASE WHEN ((t_ref_21.col_4::BOOLEAN)::TEXT::VARCHAR(32)) THEN (NOT (t_ref_21.id)) ELSE (t_ref_21.col_3 AND ((t_ref_21.col_3 - NULL) * (SELECT (EXISTS (SELECT 1 FROM tbl_06 AS sub_48 WHERE 0.0000000001)::INT) FROM tbl_02 AS sub_47 LIMIT 1))) END)))::INT))) * ((((CASE WHEN (t_ref_21.id IN (SELECT sub_49.col_2 FROM tbl_04 AS sub_49)) THEN (((t_ref_21.col_4 IN (SELECT sub_50.id FROM tbl_02 AS sub_50)) * EXISTS (SELECT 1 FROM tbl_03 AS sub_52 WHERE 1.0)) = t_ref_21.col_2) ELSE ((LENGTH((SELECT (SELECT inn_57.col_1 FROM tbl_09 AS inn_57 LIMIT 1) FROM tbl_08 AS sub_55 LIMIT 1)) * (t_ref_21.col_4 + t_ref_21.col_1)) * (SELECT sub_58.col_3 FROM tbl_08 AS sub_58 LIMIT 1)) END) AND ((SELECT (SELECT inn_60.col_1 FROM tbl_09 AS inn_60 LIMIT 1) FROM tbl_03 AS sub_59 LIMIT 1) + EXISTS (SELECT 1 FROM tbl_05 AS sub_61 WHERE FALSE)))::INT::TEXT) * ((t_ref_21.col_4::NUMERIC(10,2)) IS NULL))))) + COALESCE((EXISTS (SELECT 1 FROM tbl_01 AS sub_62 WHERE 2147483647)::NUMERIC(10,2)), (LOWER(((t_ref_21.col_3 AND (((SELECT (SELECT (sub_64.col_2 > (SELECT NULLIF(sub_65.id, '__fuzz_marker__') FROM alpha_tbl AS sub_65 LIMIT 1)) FROM tbl_05 AS sub_64 LIMIT 1) FROM tbl_07 AS sub_63 LIMIT 1) > FALSE)::INT)) - COALESCE((t_ref_21.col_2 IN (SELECT sub_66.col_2 FROM tbl_03 AS sub_66)), (t_ref_21.col_2 - COALESCE('', ((ABS(((((('' / NULLIF('escaped''quote', 0))::INT) <> ((SELECT NULL FROM tbl_08 AS sub_67 LIMIT 1) >= EXISTS (SELECT 1 FROM tbl_04 AS sub_68 WHERE sub_68.id)))::INT::TEXT) * COALESCE(TRUE, (CASE WHEN LENGTH((t_ref_21.col_4::NUMERIC(10,2))) THEN 'alpha_beta_gamma' ELSE t_ref_21.col_3 END)))) + ((((1.0 = t_ref_21.col_3) * t_ref_21.col_1) IS NULL)::NUMERIC(10,2))) + ((COALESCE((((t_ref_21.col_4 IN (SELECT sub_69.id FROM tbl_09 AS sub_69)) <= (t_ref_21.col_3 IN (SELECT sub_70.col_3 FROM tbl_08 AS sub_70))) OR t_ref_21.col_3), (CASE WHEN LENGTH(((t_ref_21.col_4 * -1e-5)::NUMERIC(10,2))) THEN (CASE WHEN (EXISTS (SELECT 1 FROM tbl_09 AS sub_71 WHERE -1.5) AND (SELECT sub_72.id FROM tbl_09 AS sub_72 LIMIT 1)) THEN (SELECT (SELECT inn_74.col_2 FROM tbl_08 AS inn_74 LIMIT 1) FROM tbl_07 AS sub_73 LIMIT 1) ELSE LENGTH(t_ref_21.id) END) ELSE (CASE WHEN t_ref_21.id THEN (UPPER(t_ref_21.col_4) >= t_ref_21.col_1) ELSE ((t_ref_21.col_2 AND TRUE) < (t_ref_21.col_1 > t_ref_21.col_3)) END) END)) <= ABS(t_ref_21.id)) - (CASE WHEN (t_ref_21.id < ((t_ref_21.col_2 IN (SELECT sub_75.col_2 FROM tbl_10 AS sub_75))::NUMERIC(10,2))) THEN ((SELECT ((sub_77.col_4 <> -1) - (CASE WHEN sub_77.col_1 THEN sub_77.col_1 ELSE TRUE END)) FROM tbl_01 AS sub_77 LIMIT 1) = TRUE) ELSE ABS((('chaos' <> EXISTS (SELECT 1 FROM alpha_tbl AS sub_78 WHERE sub_78.payload)) + t_ref_21.id)) END)))))))) OR (-1 * t_ref_21.id)))) <> (SELECT (SELECT inn_80.col_3 FROM tbl_08 AS inn_80 LIMIT 1) FROM tbl_03 AS sub_79 LIMIT 1)) * COALESCE((t_ref_21.col_3 IN (SELECT sub_81.col_2 FROM tbl_09 AS sub_81)), ('ref_val'::INT::TEXT))))
LIMIT 32;

ALTER TABLE tbl_05 ADD COLUMN dyn_col_5 VARCHAR(32);

DELETE FROM tbl_07 WHERE id = 53193;

INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_04_id) VALUES (496933, 'chaos', '[]', -1);

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (731934, -2147483648, FALSE, FALSE, 'alpha_beta_gamma');

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (422582, 'escaped''quote', 42, FALSE, -1e-5);
COMMIT;

BEGIN;
INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_04_id) VALUES (939204, '', '{}', 9223372036854775807);
INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_04_id) VALUES (67970, '__fuzz_marker__', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 100);
COMMIT;

INSERT INTO tbl_07 (id, col_1, col_2, col_3, col_4) VALUES (922706, -9223372036854775808, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 1e10, 1.0);

ALTER TABLE tbl_05 DROP COLUMN col_2;

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_09 WHERE id IS NOT NULL;

DELETE FROM tbl_01 WHERE id = 34870;

ALTER TABLE tbl_01 ADD COLUMN dyn_col_6 VARCHAR(32);

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2, col_3, col_4) VALUES (898358, -2147483648, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 999999999.99, 1.0);
INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_02_id, dyn_col_5) VALUES (632511, FALSE, -1.5, 0, '__fuzz_marker__');
INSERT INTO tbl_05 (id, col_1, col_3, dyn_col_5) VALUES (474482, '{}', '', 'alpha_beta_gamma');
COMMIT;

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (772472, 'alpha_beta_gamma', -2147483648, FALSE, 0.0);

CREATE TABLE gamma_tbl (
    c_id INT PRIMARY KEY,
    b_ref INT REFERENCES beta_tbl(b_id),
    val_c INT
);

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2),
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

INSERT INTO tbl_02 (id, col_1, col_2) VALUES (985806, 'alpha_beta_gamma', 'alpha_beta_gamma');

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_07 WHERE id IS NOT NULL;

ALTER TABLE tbl_11 ADD COLUMN dyn_col_5 VARCHAR(32);

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (999354, 'ref_val', 999999999.99, 42, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

INSERT INTO alpha_tbl (id, payload) VALUES (789456, 'alpha_beta_gamma');

ALTER TABLE tbl_03 DROP COLUMN ref_tbl_02_id;

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2)
);

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 INT,
    col_4 INT,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

DELETE FROM tbl_05 WHERE id = 3750;

SELECT
    (SELECT (((sub_83.col_1 IS NULL) - (((COALESCE((SELECT LENGTH(EXISTS (SELECT 1 FROM tbl_11 AS sub_85 WHERE sub_85.ref_tbl_04_id)) FROM tbl_06 AS sub_84 LIMIT 1), (NOT (sub_83.col_1)))::INT) = sub_83.col_2)::TEXT::VARCHAR(32))) IS NULL) FROM tbl_09 AS sub_83 LIMIT 1) AS c_92
FROM tbl_04 AS t_ref_82 LEFT JOIN tbl_13 AS j_ref_93 ON t_ref_82.id = j_ref_93.id
WHERE (CASE WHEN 'escaped''quote' THEN (t_ref_82.col_1 + (NOT ((SELECT (SELECT 'alpha_beta_gamma' FROM tbl_13 AS inn_111 LIMIT 1) FROM tbl_09 AS sub_94 LIMIT 1)))) ELSE (SELECT (SELECT 2147483647 FROM alpha_tbl AS inn_114 LIMIT 1) FROM tbl_12 AS sub_112 LIMIT 1) END)
LIMIT 2;

UPDATE tbl_02 SET col_1 = 'ref_val' WHERE id > 49;

DELETE FROM tbl_02 WHERE id = 8022;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (232031, 'alpha_beta_gamma', TRUE, '[]', '[]');
COMMIT;

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT,
    ref_tbl_09_id INT,
    FOREIGN KEY (ref_tbl_09_id) REFERENCES tbl_09(id)
);

CREATE INDEX idx_tbl_11_col_2_2 ON tbl_11 (col_2);

INSERT INTO tbl_09 (id, col_1, col_2) VALUES (584696, -9223372036854775808, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 INT,
    col_3 NUMERIC(10,2),
    col_4 JSONB
);

INSERT INTO alpha_tbl (a_id, val_a) VALUES (1, 10), (2, 20);

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 INT
);

CREATE TABLE tbl_17 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 NUMERIC(10,2),
    ref_tbl_11_id INT,
    FOREIGN KEY (ref_tbl_11_id) REFERENCES tbl_11(id)
);

UPDATE tbl_14 SET col_1 = 0 WHERE id > 64;

SELECT
    t_ref_115.id AS c_116,
    t_ref_115.dyn_col_6 AS c_117,
    (NULLIF(LOWER(COALESCE(((((t_ref_115.col_3 IN (SELECT sub_118.ref_tbl_09_id FROM tbl_14 AS sub_118)) / NULLIF(((EXISTS (SELECT 1 FROM tbl_08 AS sub_129 WHERE sub_129.col_4) - 2147483647) * (((((EXISTS (SELECT 1 FROM tbl_11 AS sub_135 WHERE sub_135.ref_tbl_04_id) + ((SELECT (SELECT TRUE FROM tbl_05 AS inn_137 LIMIT 1) FROM tbl_07 AS sub_136 LIMIT 1) * (((((CASE WHEN 'ref_val' THEN '' ELSE t_ref_115.col_2 END) - ('alpha_beta_gamma' / NULLIF(t_ref_115.col_1, 0))) * t_ref_115.dyn_col_6) > (t_ref_115.col_2 <= EXISTS (SELECT 1 FROM tbl_13 AS sub_138 WHERE FALSE))) <= LENGTH((((CASE WHEN t_ref_115.id THEN t_ref_115.col_4 ELSE t_ref_115.id END) - t_ref_115.col_3)::INT))))) * LENGTH((SELECT (sub_139.col_2 / NULLIF(ABS((CASE WHEN (sub_139.col_1 IS NULL) THEN (NOT (NULL)) ELSE EXISTS (SELECT 1 FROM tbl_03 AS sub_140 WHERE sub_140.col_1) END)), 0)) FROM tbl_15 AS sub_139 LIMIT 1))) + t_ref_115.col_4) + (LENGTH((CASE WHEN ((-1e-5 IN (SELECT sub_141.id FROM tbl_15 AS sub_141)) + COALESCE((t_ref_115.col_3 < ((NULLIF(t_ref_115.dyn_col_6, t_ref_115.id)::TEXT::VARCHAR(32)) >= ('escaped''quote' > ABS(t_ref_115.col_4)))), ABS((t_ref_115.col_2 - ((t_ref_115.col_3 > NULL) >= NULL))))) THEN (((((t_ref_115.id <= (t_ref_115.col_4 - t_ref_115.id))::INT)::NUMERIC(10,2))::NUMERIC(10,2)) IS NOT NULL) ELSE 2147483647 END)) - (NOT (LENGTH((EXISTS (SELECT 1 FROM tbl_11 AS sub_143 WHERE TRUE) > EXISTS (SELECT 1 FROM tbl_14 AS sub_146 WHERE sub_146.ref_tbl_09_id))))))) IS NOT NULL)), 0)) OR (('alpha_beta_gamma' / NULLIF((CASE WHEN ((0 IN (SELECT sub_148.col_3 FROM tbl_01 AS sub_148)) + (((SELECT sub_152.col_3 FROM tbl_12 AS sub_152 LIMIT 1) / NULLIF(t_ref_115.dyn_col_6, 0)) / NULLIF((EXISTS (SELECT 1 FROM tbl_17 AS sub_153 WHERE sub_153.ref_tbl_11_id) AND ((NULLIF(t_ref_115.dyn_col_6, TRUE)::BOOLEAN) >= COALESCE((t_ref_115.id IN (SELECT sub_157.col_2 FROM tbl_04 AS sub_157)), NULLIF(((LOWER((100::INT))::TEXT) <= TRUE), (COALESCE(LENGTH(LOWER(t_ref_115.col_2)), 100) - EXISTS (SELECT 1 FROM tbl_03 AS sub_159 WHERE sub_159.col_2)))))), 0))) THEN t_ref_115.col_3 ELSE t_ref_115.col_1 END), 0)) / NULLIF((NULL IN (SELECT sub_160.id FROM alpha_tbl AS sub_160)), 0))) - t_ref_115.col_4), (CASE WHEN ((EXISTS (SELECT 1 FROM tbl_03 AS sub_162 WHERE sub_162.id) + t_ref_115.col_3)::TEXT::VARCHAR(32)) THEN (NOT (((t_ref_115.col_4 AND (-9223372036854775808 IN (SELECT sub_176.col_2 FROM tbl_10 AS sub_176))) < ABS(COALESCE(TRUE, (1.0 / NULLIF((t_ref_115.dyn_col_6 IN (SELECT sub_177.col_1 FROM tbl_10 AS sub_177)), 0))))))) ELSE (SELECT UPPER(sub_183.col_2) FROM tbl_17 AS sub_183 LIMIT 1) END))), (CASE WHEN (t_ref_115.col_4 IN (SELECT sub_184.dyn_col_5 FROM tbl_05 AS sub_184)) THEN EXISTS (SELECT 1 FROM tbl_12 AS sub_186 WHERE sub_186.col_2) ELSE ((((t_ref_115.col_2::TEXT) + t_ref_115.col_2) < t_ref_115.col_3) < ((CASE WHEN (CASE WHEN (CASE WHEN (t_ref_115.col_1 BETWEEN 100 AND 9223372036854775807) THEN (TRUE <= (SELECT (SELECT inn_217.dyn_col_5 FROM tbl_05 AS inn_217 LIMIT 1) FROM tbl_05 AS sub_216 LIMIT 1)) ELSE (COALESCE(((1.0 IS NULL)::NUMERIC(10,2)), '') <= t_ref_115.col_1) END) THEN t_ref_115.col_4 ELSE t_ref_115.col_3 END) THEN (NULL AND (LENGTH(t_ref_115.id) BETWEEN -2147483648 AND 2147483647)) ELSE LOWER(((SELECT (SELECT inn_222.id FROM tbl_12 AS inn_222 LIMIT 1) FROM tbl_01 AS sub_218 LIMIT 1) * (t_ref_115.col_2 <> ((((SELECT (SELECT inn_232.col_1 FROM tbl_09 AS inn_232 LIMIT 1) FROM tbl_03 AS sub_223 LIMIT 1) - NULLIF(((SELECT (SELECT TRUE FROM tbl_11 AS inn_234 LIMIT 1) FROM tbl_06 AS sub_233 LIMIT 1) = t_ref_115.id), NULL)) + COALESCE(((100 - (SELECT (SELECT inn_237.col_3 FROM tbl_05 AS inn_237 LIMIT 1) FROM tbl_13 AS sub_235 LIMIT 1)) BETWEEN -1 AND 2147483647), t_ref_115.col_3)) / NULLIF(FALSE, 0))))) END) OR t_ref_115.col_3)) END))::NUMERIC(10,2)) AS c_238
FROM tbl_01 AS t_ref_115 LEFT JOIN tbl_15 AS j_ref_239 ON t_ref_115.id = j_ref_239.id
WHERE t_ref_115.col_2
LIMIT 50;

INSERT INTO tbl_05 (id, col_1, col_3, dyn_col_5) VALUES (818041, '[]', '__fuzz_marker__', 'alpha_beta_gamma');

CREATE VIEW v_summary_6 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

SELECT
    (SELECT (SELECT inn_251.id FROM tbl_14 AS inn_251 LIMIT 1) FROM tbl_16 AS sub_241 LIMIT 1) AS c_252,
    (t_ref_240.col_1 * (EXISTS (SELECT 1 FROM tbl_02 AS sub_253 WHERE sub_253.id) + ((((t_ref_240.id::BOOLEAN) / NULLIF((SELECT ((CASE WHEN (SELECT (SELECT inn_292.col_1 FROM tbl_06 AS inn_292 LIMIT 1) FROM tbl_10 AS sub_285 LIMIT 1) THEN (sub_284.col_4 * (((sub_284.col_4 IN (SELECT sub_293.dyn_col_5 FROM tbl_11 AS sub_293)) - sub_284.col_2) < ABS((((EXISTS (SELECT 1 FROM tbl_02 AS sub_295 WHERE -1) * NULLIF((((NULL IN (SELECT sub_296.col_1 FROM tbl_09 AS sub_296)) OR sub_284.id)::INT::TEXT), (NULLIF((NOT (sub_284.col_2)), (-1e-5 >= TRUE))::NUMERIC(10,2)))) / NULLIF(sub_284.col_1, 0)) / NULLIF((SELECT (SELECT NULL FROM tbl_17 AS inn_298 LIMIT 1) FROM tbl_07 AS sub_297 LIMIT 1), 0))))) ELSE (EXISTS (SELECT 1 FROM tbl_08 AS sub_299 WHERE sub_299.id) / NULLIF(COALESCE(sub_284.col_1, sub_284.dyn_col_6), 0)) END) AND ABS(LOWER(sub_284.id))) FROM tbl_01 AS sub_284 LIMIT 1), 0)) - (SELECT ((EXISTS (SELECT 1 FROM tbl_12 AS sub_301 WHERE 'escaped''quote') - (SELECT ((LENGTH((sub_302.ref_tbl_09_id * (((((sub_302.ref_tbl_09_id <= 42) + COALESCE(sub_302.col_1, sub_302.id)) < LOWER((sub_302.ref_tbl_09_id IN (SELECT sub_303.col_2 FROM tbl_17 AS sub_303)))) / NULLIF((sub_302.id IN (SELECT sub_304.id FROM tbl_04 AS sub_304)), 0)) - sub_302.id))) + ((((((CASE WHEN EXISTS (SELECT 1 FROM tbl_14 AS sub_306 WHERE 3.14159265) THEN (-9223372036854775808::NUMERIC(10,2)) ELSE (NULL = NULL) END) / NULLIF(((sub_302.ref_tbl_09_id + NULL) < (SELECT (SELECT inn_308.id FROM tbl_07 AS inn_308 LIMIT 1) FROM tbl_16 AS sub_307 LIMIT 1)), 0)) > (sub_302.id IN (SELECT sub_309.id FROM tbl_09 AS sub_309))) OR 'chaos') * (((CASE WHEN LOWER(COALESCE(sub_302.col_1, NULL)) THEN EXISTS (SELECT 1 FROM tbl_14 AS sub_311 WHERE sub_311.ref_tbl_09_id) ELSE ((sub_302.ref_tbl_09_id AND FALSE) - sub_302.id) END) / NULLIF(sub_302.col_1, 0)) AND (EXISTS (SELECT 1 FROM tbl_01 AS sub_312 WHERE FALSE) = (SELECT (SELECT FALSE FROM tbl_16 AS inn_314 LIMIT 1) FROM tbl_02 AS sub_313 LIMIT 1)))) - (sub_302.col_2 < FALSE))) IS NOT NULL) FROM tbl_14 AS sub_302 LIMIT 1)) * (((FALSE + (((TRUE / NULLIF(((SELECT COALESCE((LENGTH(sub_315.col_1) <> EXISTS (SELECT 1 FROM tbl_14 AS sub_316 WHERE sub_316.ref_tbl_09_id)), LOWER(sub_315.col_4)) FROM tbl_04 AS sub_315 LIMIT 1) * sub_300.col_2), 0)) - ((NULL = ABS(sub_300.col_1)) / NULLIF((SELECT EXISTS (SELECT 1 FROM tbl_08 AS sub_318 WHERE sub_318.col_1) FROM tbl_16 AS sub_317 LIMIT 1), 0))) AND sub_300.col_1)) IS NOT NULL) <= (LOWER(((sub_300.col_2 IN (SELECT sub_320.col_4 FROM tbl_04 AS sub_320)) AND ((COALESCE(((LENGTH(((1 + sub_300.col_1) IS NULL)) <= 100) + LOWER((((sub_300.col_2 + sub_300.id) IS NULL) <> ((sub_300.id::BOOLEAN) * sub_300.col_1)))), (999999999.99 IN (SELECT sub_323.col_3 FROM tbl_13 AS sub_323))) * UPPER((sub_300.id IS NULL)))::TEXT))) < ('escaped''quote' < sub_300.col_2)))) FROM tbl_09 AS sub_300 LIMIT 1))::BOOLEAN))) AS c_324,
    t_ref_240.col_2 AS c_325
FROM tbl_06 AS t_ref_240
WHERE UPPER((SELECT ABS((UPPER(sub_326.col_3) <> (FALSE / NULLIF((UPPER(UPPER((((NOT ((TRUE < (((CASE WHEN ((sub_326.col_4 IN (SELECT sub_327.ref_tbl_01_id FROM tbl_13 AS sub_327)) IS NOT NULL) THEN sub_326.col_3 ELSE (sub_326.col_3 IN (SELECT sub_328.dyn_col_5 FROM tbl_03 AS sub_328)) END) AND sub_326.col_1)::INT::TEXT)))) <= (((((SELECT (SELECT inn_330.col_2 FROM tbl_10 AS inn_330 LIMIT 1) FROM tbl_15 AS sub_329 LIMIT 1) / NULLIF('escaped''quote', 0)) IS NULL) <> (NOT (((((sub_326.col_3 IN (SELECT sub_331.dyn_col_5 FROM tbl_05 AS sub_331)) - sub_326.col_3)::TEXT::VARCHAR(32)) * (((sub_326.col_3 <> sub_326.col_2)::TEXT) <= (ABS((SELECT (SELECT inn_333.ref_tbl_04_id FROM tbl_10 AS inn_333 LIMIT 1) FROM tbl_04 AS sub_332 LIMIT 1)) BETWEEN -2147483648 AND 42)))))) - (sub_326.col_3 IN (SELECT sub_334.col_4 FROM tbl_08 AS sub_334)))) >= ((EXISTS (SELECT 1 FROM tbl_05 AS sub_335 WHERE sub_335.col_3) OR ((sub_326.col_2::TEXT::VARCHAR(32)) OR ((NULL::TEXT::VARCHAR(32)) = (((42 < (UPPER(sub_326.id) / NULLIF((sub_326.col_2 - sub_326.col_2), 0))) <> LENGTH(((SELECT (SELECT inn_337.dyn_col_5 FROM tbl_03 AS inn_337 LIMIT 1) FROM tbl_08 AS sub_336 LIMIT 1) + sub_326.col_1))) < sub_326.col_1)))) + sub_326.col_2))))::TEXT::VARCHAR(32)), 0)))) FROM tbl_08 AS sub_326 LIMIT 1))
LIMIT 16;

INSERT INTO tbl_15 (id, col_1, col_2, col_3, col_4) VALUES (113745, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 42, -1.5, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

BEGIN;
INSERT INTO tbl_12 (id, col_1, col_2, col_3) VALUES (955587, 42, 999999999.99, 1e10);
COMMIT;

SELECT
    (TRUE * t_ref_338.payload) AS c_339,
    EXISTS (SELECT 1 FROM tbl_03 AS sub_340 WHERE sub_340.dyn_col_5) AS c_372
FROM alpha_tbl AS t_ref_338
WHERE 'escaped''quote'
LIMIT 42;

CREATE INDEX idx_tbl_02_col_2_3 ON tbl_02 (col_2);

SELECT
    (LENGTH((CASE WHEN NULLIF(((CASE WHEN (((SELECT (((SELECT (SELECT TRUE FROM tbl_08 AS inn_376 LIMIT 1) FROM tbl_08 AS sub_375 LIMIT 1) <= -1.5) OR (SELECT (SELECT inn_380.ref_tbl_04_id FROM tbl_11 AS inn_380 LIMIT 1) FROM tbl_07 AS sub_377 LIMIT 1)) FROM tbl_06 AS sub_374 LIMIT 1) <> (SELECT 'escaped''quote' FROM tbl_17 AS sub_381 LIMIT 1)) + COALESCE((t_ref_373.col_3 / NULLIF(NULL, 0)), ((SELECT (SELECT inn_383.ref_tbl_11_id FROM tbl_17 AS inn_383 LIMIT 1) FROM tbl_11 AS sub_382 LIMIT 1) >= ((CASE WHEN NULL THEN (((SELECT EXISTS (SELECT 1 FROM tbl_15 AS sub_385 WHERE sub_385.col_3) FROM tbl_11 AS sub_384 LIMIT 1)::TEXT) + (NULL IN (SELECT sub_386.dyn_col_5 FROM tbl_03 AS sub_386))) ELSE (NOT (TRUE)) END) IS NULL)))) THEN LOWER(((SELECT (SELECT inn_392.col_2 FROM tbl_11 AS inn_392 LIMIT 1) FROM tbl_14 AS sub_389 LIMIT 1) - (CASE WHEN (((EXISTS (SELECT 1 FROM tbl_01 AS sub_393 WHERE NULL) / NULLIF((SELECT (SELECT inn_397.ref_tbl_11_id FROM tbl_17 AS inn_397 LIMIT 1) FROM tbl_16 AS sub_394 LIMIT 1), 0)) * t_ref_373.col_3) BETWEEN 2147483647 AND 1) THEN COALESCE(EXISTS (SELECT 1 FROM tbl_13 AS sub_398 WHERE sub_398.col_1), (CASE WHEN ((t_ref_373.col_3 IN (SELECT sub_399.dyn_col_6 FROM tbl_01 AS sub_399)) - t_ref_373.col_2) THEN (t_ref_373.col_1 IN (SELECT sub_400.col_3 FROM tbl_13 AS sub_400)) ELSE (((t_ref_373.ref_tbl_01_id <= ((((TRUE BETWEEN 0 AND 42) - ((t_ref_373.col_1::TEXT)::INT::TEXT)) BETWEEN 42 AND 42) >= (NOT ((LENGTH(t_ref_373.id)::INT::TEXT)))))::INT::TEXT) > COALESCE((((SELECT (sub_401.col_3 / NULLIF((sub_401.col_1 <> sub_401.col_2), 0)) FROM tbl_13 AS sub_401 LIMIT 1)::INT) + (EXISTS (SELECT 1 FROM tbl_05 AS sub_402 WHERE sub_402.dyn_col_5)::BOOLEAN)), (t_ref_373.id IN (SELECT sub_404.col_4 FROM tbl_04 AS sub_404)))) END)) ELSE (SELECT (EXISTS (SELECT 1 FROM tbl_09 AS sub_407 WHERE sub_407.id) * (EXISTS (SELECT 1 FROM tbl_04 AS sub_410 WHERE sub_410.col_4) = (((NULLIF(sub_406.id, ((CASE WHEN FALSE THEN sub_406.payload ELSE sub_406.id END) <= ABS(sub_406.id)))::BOOLEAN)::BOOLEAN) - EXISTS (SELECT 1 FROM tbl_04 AS sub_411 WHERE -1.5)))) FROM alpha_tbl AS sub_406 LIMIT 1) END))) ELSE EXISTS (SELECT 1 FROM tbl_04 AS sub_412 WHERE sub_412.id) END) - COALESCE((t_ref_373.col_1 IN (SELECT sub_437.col_1 FROM tbl_16 AS sub_437)), t_ref_373.col_1)), UPPER(((((SELECT NULLIF(EXISTS (SELECT 1 FROM tbl_04 AS sub_448 WHERE sub_448.col_3), (CASE WHEN (sub_447.col_2 - (SELECT EXISTS (SELECT 1 FROM tbl_09 AS sub_452 WHERE -1e-5) FROM tbl_10 AS sub_451 LIMIT 1)) THEN (SELECT (SELECT inn_457.col_1 FROM tbl_15 AS inn_457 LIMIT 1) FROM tbl_17 AS sub_453 LIMIT 1) ELSE sub_447.col_2 END)) FROM tbl_17 AS sub_447 LIMIT 1) BETWEEN 100 AND -1)::TEXT)::NUMERIC(10,2)))) THEN 'ref_val' ELSE (t_ref_373.col_4 BETWEEN 42 AND -2147483648) END)) - LOWER(t_ref_373.col_2)) AS c_458,
    t_ref_373.col_4 AS c_459
FROM tbl_13 AS t_ref_373
WHERE (CASE WHEN ((CASE WHEN (CASE WHEN (SELECT (SELECT inn_473.col_1 FROM tbl_14 AS inn_473 LIMIT 1) FROM tbl_05 AS sub_460 LIMIT 1) THEN LOWER(LOWER((FALSE OR t_ref_373.ref_tbl_01_id))) ELSE ((t_ref_373.ref_tbl_01_id IN (SELECT sub_474.col_2 FROM tbl_03 AS sub_474)) + (t_ref_373.col_2 IN (SELECT sub_477.dyn_col_5 FROM tbl_11 AS sub_477))) END) THEN t_ref_373.col_3 ELSE (ABS(t_ref_373.col_3) * UPPER((SELECT (SELECT inn_499.col_4 FROM tbl_08 AS inn_499 LIMIT 1) FROM tbl_16 AS sub_489 LIMIT 1))) END) OR (SELECT (SELECT LENGTH(((ABS((sub_501.col_3 IN (SELECT sub_502.col_1 FROM tbl_09 AS sub_502))) BETWEEN 0 AND 42)::TEXT::VARCHAR(32))) FROM tbl_05 AS sub_501 LIMIT 1) FROM tbl_03 AS sub_500 LIMIT 1)) THEN ((SELECT (SELECT inn_509.payload FROM alpha_tbl AS inn_509 LIMIT 1) FROM tbl_09 AS sub_508 LIMIT 1) OR NULLIF((ABS(((NOT (FALSE)) = (t_ref_373.id + (SELECT (SELECT (SELECT inn_515.col_2 FROM tbl_08 AS inn_515 LIMIT 1) FROM tbl_14 AS sub_511 LIMIT 1) FROM tbl_06 AS sub_510 LIMIT 1)))) IS NOT NULL), t_ref_373.col_3)) ELSE ((t_ref_373.ref_tbl_01_id * (CASE WHEN EXISTS (SELECT 1 FROM tbl_16 AS sub_516 WHERE 0) THEN ((EXISTS (SELECT 1 FROM tbl_13 AS sub_521 WHERE sub_521.col_3) + t_ref_373.id) * 9223372036854775807) ELSE (FALSE::INT::TEXT) END)) >= ((((3.14159265::NUMERIC(10,2)) > (((SELECT (SELECT (SELECT NULL FROM tbl_14 AS inn_530 LIMIT 1) FROM tbl_09 AS sub_526 LIMIT 1) FROM tbl_10 AS sub_525 LIMIT 1) <= t_ref_373.col_2) = t_ref_373.col_1)) BETWEEN 0 AND 0) BETWEEN 9223372036854775807 AND 0)) END)
LIMIT 34;

UPDATE tbl_06 SET col_1 = '' WHERE id > 29;

UPDATE tbl_17 SET ref_tbl_11_id = 100 WHERE id > 91;

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2, dyn_col_5) VALUES (142984, TRUE, 0.0000000001, '__fuzz_marker__');
INSERT INTO tbl_02 (id, col_1, col_2) VALUES (619660, 'alpha_beta_gamma', '');
COMMIT;

DELETE FROM alpha_tbl WHERE id = 78786;

SELECT
    (NOT (t_ref_531.col_2)) AS c_532,
    COALESCE((NULLIF(((t_ref_531.col_4 IN (SELECT sub_533.id FROM tbl_10 AS sub_533))::TEXT), t_ref_531.col_2)::INT), ABS((NOT ((NOT (t_ref_531.col_4)))))) AS c_536
FROM tbl_04 AS t_ref_531 LEFT JOIN tbl_02 AS j_ref_537 ON t_ref_531.id = j_ref_537.id
WHERE 'alpha_beta_gamma'
LIMIT 19;

UPDATE tbl_12 SET col_1 = -2147483648 WHERE id > 46;

INSERT INTO beta_tbl (b_id, a_ref, val_b) VALUES (1, 1, 100), (2, 2, 200);

SELECT
    (((0.0000000001 IN (SELECT sub_539.col_2 FROM tbl_03 AS sub_539)) - ((CASE WHEN (t_ref_538.col_2 IN (SELECT sub_547.col_2 FROM tbl_12 AS sub_547)) THEN ((SELECT (SELECT inn_569.col_2 FROM tbl_11 AS inn_569 LIMIT 1) FROM alpha_tbl AS sub_568 LIMIT 1) = 999999999.99) ELSE 0.0 END) <= COALESCE((NULLIF(t_ref_538.id, (SELECT (SELECT inn_571.ref_tbl_11_id FROM tbl_17 AS inn_571 LIMIT 1) FROM tbl_05 AS sub_570 LIMIT 1)) < ('__fuzz_marker__' * t_ref_538.id)), ((((SELECT (SELECT inn_573.id FROM tbl_07 AS inn_573 LIMIT 1) FROM tbl_06 AS sub_572 LIMIT 1) OR t_ref_538.col_2) OR LOWER(((((((((SELECT (SELECT inn_575.col_1 FROM tbl_15 AS inn_575 LIMIT 1) FROM tbl_17 AS sub_574 LIMIT 1) <> (t_ref_538.col_2::NUMERIC(10,2))) - t_ref_538.dyn_col_5) + (t_ref_538.col_1 + (ABS(EXISTS (SELECT 1 FROM tbl_14 AS sub_576 WHERE 'chaos')) * FALSE))) * (SELECT 3.14159265 FROM alpha_tbl AS sub_578 LIMIT 1)) BETWEEN 100 AND -9223372036854775808)::NUMERIC(10,2)) IS NOT NULL))) / NULLIF(('chaos' BETWEEN 1 AND 42), 0))))) = COALESCE(TRUE, ((CASE WHEN t_ref_538.id THEN ((CASE WHEN 'chaos' THEN (LOWER(LOWER(LOWER((((((t_ref_538.dyn_col_5 <= (SELECT ((sub_579.col_1 / NULLIF(sub_579.col_2, 0)) + (NULL::BOOLEAN)) FROM tbl_02 AS sub_579 LIMIT 1)) - (((t_ref_538.col_1 = (NOT (EXISTS (SELECT 1 FROM tbl_14 AS sub_580 WHERE sub_580.id)))) - EXISTS (SELECT 1 FROM tbl_02 AS sub_581 WHERE NULL)) / NULLIF((SELECT (SELECT inn_584.col_2 FROM tbl_15 AS inn_584 LIMIT 1) FROM tbl_11 AS sub_582 LIMIT 1), 0))) IS NOT NULL) >= (((EXISTS (SELECT 1 FROM tbl_09 AS sub_585 WHERE sub_585.col_2) * (LOWER((t_ref_538.col_2 IN (SELECT sub_586.col_4 FROM tbl_15 AS sub_586))) + (NULL::NUMERIC(10,2)))) * NULL)::NUMERIC(10,2))) + (t_ref_538.id IN (SELECT sub_587.col_2 FROM tbl_11 AS sub_587)))))) AND FALSE) ELSE (t_ref_538.col_2 - (NULL::BOOLEAN)) END) - ((SELECT (sub_588.ref_tbl_04_id::NUMERIC(10,2)) FROM tbl_10 AS sub_588 LIMIT 1)::TEXT::VARCHAR(32))) ELSE 1e10 END) = (t_ref_538.col_2 + (LENGTH((SELECT ((CASE WHEN (SELECT (((CASE WHEN NULLIF(((sub_590.col_1 < (NULL IN (SELECT sub_591.col_4 FROM tbl_04 AS sub_591))) AND LOWER((sub_590.col_3 - ''))), (TRUE IN (SELECT sub_592.col_3 FROM tbl_12 AS sub_592))) THEN ((((sub_590.id + sub_590.col_4) * LOWER(sub_590.col_4))::TEXT) / NULLIF(ABS((SELECT sub_593.col_1 FROM tbl_01 AS sub_593 LIMIT 1)), 0)) ELSE NULLIF(sub_590.col_2, LOWER(sub_590.col_4)) END) BETWEEN -2147483648 AND 42) >= ABS((sub_590.id > (SELECT 0 FROM tbl_08 AS sub_594 LIMIT 1)))) FROM tbl_15 AS sub_590 LIMIT 1) THEN (SELECT (SELECT inn_600.dyn_col_5 FROM tbl_05 AS inn_600 LIMIT 1) FROM tbl_16 AS sub_595 LIMIT 1) ELSE sub_589.col_3 END) >= (((sub_589.col_4 IN (SELECT sub_601.col_3 FROM tbl_04 AS sub_601))::BOOLEAN) >= LENGTH(sub_589.col_1))) FROM tbl_06 AS sub_589 LIMIT 1)) OR TRUE))))) AS c_603,
    ((SELECT (SELECT -1.5 FROM tbl_04 AS inn_605 LIMIT 1) FROM tbl_12 AS sub_604 LIMIT 1) >= FALSE) AS c_606
FROM tbl_03 AS t_ref_538
WHERE ((((NULLIF(((SELECT (SELECT -2147483648 FROM tbl_04 AS inn_612 LIMIT 1) FROM tbl_17 AS sub_607 LIMIT 1) BETWEEN 9223372036854775807 AND 9223372036854775807), ((TRUE::NUMERIC(10,2)) + t_ref_538.dyn_col_5)) = t_ref_538.col_1)::TEXT::VARCHAR(32))::TEXT) / NULLIF((((((((SELECT LOWER(-9223372036854775808) FROM tbl_10 AS sub_613 LIMIT 1)::INT)::NUMERIC(10,2))::TEXT::VARCHAR(32)) * (((CASE WHEN (SELECT (SELECT inn_625.id FROM tbl_02 AS inn_625 LIMIT 1) FROM tbl_10 AS sub_614 LIMIT 1) THEN t_ref_538.col_2 ELSE (SELECT (SELECT inn_627.col_2 FROM tbl_14 AS inn_627 LIMIT 1) FROM tbl_12 AS sub_626 LIMIT 1) END) - ((SELECT (SELECT inn_632.id FROM alpha_tbl AS inn_632 LIMIT 1) FROM alpha_tbl AS sub_628 LIMIT 1)::TEXT))::INT)) AND ((t_ref_538.col_2 BETWEEN 2147483647 AND 42) AND t_ref_538.col_1)) * (SELECT (SELECT inn_634.col_1 FROM tbl_16 AS inn_634 LIMIT 1) FROM alpha_tbl AS sub_633 LIMIT 1)), 0))
LIMIT 35;

DELETE FROM tbl_06 WHERE id = 38069;

CREATE TABLE tbl_18 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    col_3 VARCHAR(64),
    ref_tbl_10_id INT,
    FOREIGN KEY (ref_tbl_10_id) REFERENCES tbl_10(id)
);

INSERT INTO tbl_02 (id, col_1, col_2) VALUES (61515, 'escaped''quote', 'ref_val');

SELECT
    ((NOT (((SELECT (SELECT (SELECT -2147483648 FROM tbl_09 AS inn_649 LIMIT 1) FROM tbl_03 AS sub_637 LIMIT 1) FROM tbl_15 AS sub_636 LIMIT 1)::TEXT))) / NULLIF((SELECT (SELECT TRUE FROM tbl_05 AS inn_703 LIMIT 1) FROM tbl_12 AS sub_650 LIMIT 1), 0)) AS c_704
FROM tbl_07 AS t_ref_635
WHERE ((t_ref_635.id::BOOLEAN) <= t_ref_635.col_4)
LIMIT 27;

CREATE TABLE tbl_19 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 VARCHAR(64),
    col_3 INT,
    col_4 BOOLEAN
);

INSERT INTO tbl_13 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (347247, 3.14159265, 'chaos', -1, -9223372036854775808, 100);

INSERT INTO tbl_03 (id, col_1, col_2, dyn_col_5) VALUES (543343, TRUE, 0.0, '');

SELECT
    (FALSE IN (SELECT sub_706.col_3 FROM tbl_18 AS sub_706)) AS c_708
FROM tbl_10 AS t_ref_705 LEFT JOIN tbl_12 AS j_ref_709 ON t_ref_705.id = j_ref_709.id
WHERE TRUE
LIMIT 42;

SELECT
    t_ref_710.id AS c_711,
    (NULL BETWEEN -1 AND 0) AS c_712,
    t_ref_710.col_3 AS c_713
FROM tbl_19 AS t_ref_710
WHERE ((t_ref_710.col_2 AND ABS(ABS((CASE WHEN (SELECT (CASE WHEN EXISTS (SELECT 1 FROM tbl_07 AS sub_715 WHERE sub_715.col_3) THEN (SELECT 3.14159265 FROM tbl_07 AS sub_717 LIMIT 1) ELSE (((sub_714.id BETWEEN -2147483648 AND 100) BETWEEN 9223372036854775807 AND -2147483648) <> 9223372036854775807) END) FROM tbl_05 AS sub_714 LIMIT 1) THEN (t_ref_710.col_1 IN (SELECT sub_718.id FROM tbl_16 AS sub_718)) ELSE ((LOWER(((t_ref_710.col_2 * ABS((SELECT (SELECT inn_726.col_1 FROM tbl_09 AS inn_726 LIMIT 1) FROM tbl_14 AS sub_723 LIMIT 1))) / NULLIF(LENGTH((t_ref_710.col_4 IN (SELECT sub_727.col_1 FROM tbl_05 AS sub_727))), 0))) - ((SELECT (LOWER(COALESCE((CASE WHEN (SELECT (SELECT (CASE WHEN 1e10 THEN sub_730.id ELSE sub_730.id END) FROM tbl_08 AS sub_730 LIMIT 1) FROM tbl_05 AS sub_729 LIMIT 1) THEN COALESCE(sub_728.col_2, (LENGTH((9223372036854775807 = ('escaped''quote' <= 'ref_val')))::TEXT)) ELSE sub_728.col_2 END), ((LOWER((NOT (((sub_728.col_2 IS NULL) * (sub_728.col_1 + -1e-5))))) BETWEEN -2147483648 AND -1) / NULLIF(((sub_728.col_2 > (sub_728.col_2 / NULLIF(sub_728.id, 0))) BETWEEN -9223372036854775808 AND 42), 0))))::INT::TEXT) FROM tbl_01 AS sub_728 LIMIT 1) - ((t_ref_710.col_4 IN (SELECT sub_731.col_2 FROM tbl_18 AS sub_731)) + t_ref_710.col_3))) >= ((t_ref_710.col_2 <> COALESCE(((LENGTH(FALSE) >= ((SELECT (((((0.0000000001 - 0) * (sub_732.col_1 / NULLIF(1e10, 0))) - ((sub_732.id / NULLIF(sub_732.col_1, 0)) <= (SELECT TRUE FROM tbl_14 AS sub_733 LIMIT 1))) / NULLIF((SELECT (sub_734.col_1 / NULLIF(sub_734.id, 0)) FROM tbl_16 AS sub_734 LIMIT 1), 0)) / NULLIF(ABS(EXISTS (SELECT 1 FROM tbl_03 AS sub_735 WHERE NULL)), 0)) FROM tbl_17 AS sub_732 LIMIT 1) > (t_ref_710.col_3 OR ((((SELECT sub_736.col_3 FROM tbl_18 AS sub_736 LIMIT 1) > t_ref_710.col_2) OR (COALESCE(((t_ref_710.col_4::BOOLEAN) - (t_ref_710.col_3 - t_ref_710.col_1)), t_ref_710.col_1)::INT)) * (ABS(UPPER(('escaped''quote' IN (SELECT sub_737.col_3 FROM tbl_18 AS sub_737)))) - FALSE))))) * COALESCE((LOWER((((CASE WHEN (-1e-5 + ((t_ref_710.col_4 * t_ref_710.id) - (42 = t_ref_710.col_2))) THEN (((FALSE + t_ref_710.col_2) / NULLIF((CASE WHEN FALSE THEN t_ref_710.col_2 ELSE t_ref_710.id END), 0)) / NULLIF(LOWER((t_ref_710.col_1 AND t_ref_710.col_3)), 0)) ELSE (((t_ref_710.id + '') + t_ref_710.col_2) = (('ref_val' / NULLIF(t_ref_710.col_2, 0)) OR COALESCE(NULL, TRUE))) END) * ((LENGTH('__fuzz_marker__')::BOOLEAN) BETWEEN -1 AND 2147483647))::INT::TEXT)) - NULLIF((EXISTS (SELECT 1 FROM tbl_04 AS sub_738 WHERE FALSE) < ((EXISTS (SELECT 1 FROM tbl_16 AS sub_739 WHERE sub_739.col_1) + ((SELECT (SELECT inn_741.id FROM tbl_03 AS inn_741 LIMIT 1) FROM tbl_04 AS sub_740 LIMIT 1) * (ABS(t_ref_710.col_3)::TEXT))) * (TRUE <= ((t_ref_710.col_1 + (0.0000000001 < t_ref_710.id)) <= (SELECT 0 FROM tbl_12 AS sub_742 LIMIT 1))))), EXISTS (SELECT 1 FROM tbl_19 AS sub_743 WHERE sub_743.col_4))), (SELECT (FALSE::TEXT::VARCHAR(32)) FROM tbl_08 AS sub_746 LIMIT 1))), t_ref_710.col_2)) = LOWER((SELECT (sub_747.col_3 + (CASE WHEN (sub_747.col_2 <= ((((CASE WHEN (SELECT sub_748.col_1 FROM tbl_12 AS sub_748 LIMIT 1) THEN (CASE WHEN (CASE WHEN sub_747.col_2 THEN 0.0 ELSE '__fuzz_marker__' END) THEN (''::TEXT) ELSE (sub_747.col_3::INT::TEXT) END) ELSE (sub_747.id IN (SELECT sub_749.dyn_col_5 FROM tbl_11 AS sub_749)) END)::NUMERIC(10,2)) <= TRUE) > COALESCE((NULLIF((SELECT sub_750.col_3 FROM tbl_05 AS sub_750 LIMIT 1), EXISTS (SELECT 1 FROM alpha_tbl AS sub_751 WHERE 0.0)) > sub_747.col_1), (TRUE / NULLIF(COALESCE('', sub_747.id), 0))))) THEN sub_747.col_2 ELSE (CASE WHEN sub_747.col_3 THEN (SELECT COALESCE(sub_752.id, (SELECT 'escaped''quote' FROM tbl_17 AS sub_753 LIMIT 1)) FROM tbl_11 AS sub_752 LIMIT 1) ELSE ((SELECT (SELECT inn_756.col_3 FROM tbl_12 AS inn_756 LIMIT 1) FROM alpha_tbl AS sub_754 LIMIT 1) BETWEEN 2147483647 AND 2147483647) END) END)) FROM tbl_19 AS sub_747 LIMIT 1)))) END)))) * EXISTS (SELECT 1 FROM tbl_05 AS sub_757 WHERE sub_757.col_3))
LIMIT 7;

SELECT
    ((CASE WHEN (((LENGTH(((SELECT (SELECT inn_760.id FROM alpha_tbl AS inn_760 LIMIT 1) FROM tbl_14 AS sub_759 LIMIT 1) - NULL))::TEXT::VARCHAR(32)) * UPPER('')) AND ((t_ref_758.col_1::INT) < (CASE WHEN (LOWER((t_ref_758.col_1 IN (SELECT sub_761.id FROM tbl_19 AS sub_761))) * (CASE WHEN (LOWER('chaos') = '') THEN (t_ref_758.dyn_col_5 IN (SELECT sub_762.col_1 FROM tbl_15 AS sub_762)) ELSE ABS(FALSE) END)) THEN (NULL > (t_ref_758.col_2 / NULLIF(EXISTS (SELECT 1 FROM tbl_11 AS sub_772 WHERE sub_772.id), 0))) ELSE COALESCE(NULLIF(EXISTS (SELECT 1 FROM tbl_08 AS sub_773 WHERE sub_773.id), (((((CASE WHEN (((CASE WHEN t_ref_758.col_1 THEN LOWER((SELECT (SELECT inn_775.col_4 FROM tbl_07 AS inn_775 LIMIT 1) FROM alpha_tbl AS sub_774 LIMIT 1)) ELSE (CASE WHEN ((t_ref_758.col_1 IN (SELECT sub_776.col_2 FROM tbl_06 AS sub_776)) < (SELECT sub_777.col_1 FROM tbl_05 AS sub_777 LIMIT 1)) THEN t_ref_758.dyn_col_5 ELSE (LENGTH(t_ref_758.dyn_col_5) > ((NULL AND t_ref_758.col_1) + t_ref_758.col_2)) END) END) < LOWER((t_ref_758.id / NULLIF((NOT ((SELECT sub_778.col_2 FROM tbl_10 AS sub_778 LIMIT 1))), 0)))) IS NOT NULL) THEN ABS((t_ref_758.col_1 IN (SELECT sub_779.col_2 FROM tbl_10 AS sub_779))) ELSE EXISTS (SELECT 1 FROM tbl_05 AS sub_780 WHERE sub_780.dyn_col_5) END) * ((t_ref_758.id::BOOLEAN) / NULLIF(((CASE WHEN ('ref_val' BETWEEN 1 AND -9223372036854775808) THEN (SELECT (SELECT inn_784.id FROM tbl_02 AS inn_784 LIMIT 1) FROM tbl_07 AS sub_783 LIMIT 1) ELSE TRUE END) - ((((((t_ref_758.id::TEXT)::BOOLEAN) IS NULL) * ((LENGTH(NULL) BETWEEN 2147483647 AND 9223372036854775807)::INT)) / NULLIF((t_ref_758.col_1 < (SELECT FALSE FROM tbl_12 AS sub_785 LIMIT 1)), 0)) < (CASE WHEN ((t_ref_758.col_2 IN (SELECT sub_786.col_1 FROM tbl_06 AS sub_786)) - LENGTH(((t_ref_758.dyn_col_5 / NULLIF(t_ref_758.col_2, 0)) >= ('chaos' < t_ref_758.id)))) THEN ((CASE WHEN t_ref_758.col_1 THEN ((SELECT (SELECT inn_788.id FROM tbl_03 AS inn_788 LIMIT 1) FROM tbl_01 AS sub_787 LIMIT 1)::INT::TEXT) ELSE t_ref_758.id END) OR (CASE WHEN (t_ref_758.col_1 IN (SELECT sub_789.col_3 FROM tbl_19 AS sub_789)) THEN ((SELECT sub_790.col_2 FROM tbl_14 AS sub_790 LIMIT 1) * (FALSE IS NOT NULL)) ELSE (SELECT (SELECT 'chaos' FROM tbl_09 AS inn_792 LIMIT 1) FROM tbl_08 AS sub_791 LIMIT 1) END)) ELSE FALSE END))), 0)))::INT) + ('alpha_beta_gamma' / NULLIF((t_ref_758.col_2::INT), 0)))::INT::TEXT)), (t_ref_758.dyn_col_5 + (SELECT (sub_793.col_4 IN (SELECT sub_794.col_2 FROM tbl_15 AS sub_794)) FROM tbl_07 AS sub_793 LIMIT 1))) END))) THEN ((((SELECT (((sub_795.col_1 + (EXISTS (SELECT 1 FROM tbl_17 AS sub_796 WHERE sub_796.col_2)::TEXT::VARCHAR(32)))::INT) / NULLIF((EXISTS (SELECT 1 FROM tbl_08 AS sub_798 WHERE sub_798.id)::INT), 0)) FROM tbl_09 AS sub_795 LIMIT 1) <> (t_ref_758.col_1 > (SELECT (SELECT 'ref_val' FROM tbl_04 AS inn_806 LIMIT 1) FROM tbl_06 AS sub_799 LIMIT 1))) > LENGTH(t_ref_758.col_2)) / NULLIF((NULLIF(1, (((((t_ref_758.col_2 OR (EXISTS (SELECT 1 FROM tbl_10 AS sub_807 WHERE TRUE)::NUMERIC(10,2))) AND ((SELECT (SELECT inn_816.dyn_col_5 FROM tbl_11 AS inn_816 LIMIT 1) FROM tbl_14 AS sub_808 LIMIT 1) * (SELECT sub_817.id FROM alpha_tbl AS sub_817 LIMIT 1))) <> t_ref_758.col_1) < t_ref_758.id)::BOOLEAN)) * LOWER((ABS((((t_ref_758.dyn_col_5 <= t_ref_758.id) / NULLIF(t_ref_758.col_1, 0))::BOOLEAN)) * ((CASE WHEN (CASE WHEN EXISTS (SELECT 1 FROM tbl_11 AS sub_818 WHERE -1e-5) THEN (CASE WHEN (t_ref_758.col_2 / NULLIF((EXISTS (SELECT 1 FROM tbl_07 AS sub_834 WHERE '__fuzz_marker__')::INT::TEXT), 0)) THEN ((CASE WHEN (NOT ((((t_ref_758.dyn_col_5 = ((LENGTH('escaped''quote') * (t_ref_758.col_2 + t_ref_758.id)) * EXISTS (SELECT 1 FROM tbl_11 AS sub_835 WHERE NULL))) / NULLIF((((t_ref_758.dyn_col_5 * (t_ref_758.col_1 * 2147483647)) / NULLIF(t_ref_758.id, 0)) / NULLIF(ABS(NULLIF((t_ref_758.col_1 + t_ref_758.col_2), (t_ref_758.id + FALSE))), 0)), 0))::TEXT))) THEN ((CASE WHEN (((t_ref_758.col_1 BETWEEN 42 AND 42) + t_ref_758.col_2) - (t_ref_758.id <= EXISTS (SELECT 1 FROM tbl_05 AS sub_836 WHERE FALSE))) THEN (t_ref_758.col_1 IN (SELECT sub_837.id FROM tbl_06 AS sub_837)) ELSE (t_ref_758.id * ((((CASE WHEN NULL THEN t_ref_758.col_2 ELSE t_ref_758.id END) <> (t_ref_758.dyn_col_5 < t_ref_758.col_2)) = (SELECT (SELECT FALSE FROM tbl_18 AS inn_839 LIMIT 1) FROM tbl_03 AS sub_838 LIMIT 1)) OR (((t_ref_758.dyn_col_5::INT::TEXT) / NULLIF(TRUE, 0)) < ((t_ref_758.col_1 / NULLIF(t_ref_758.col_2, 0)) IS NULL)))) END) * FALSE) ELSE UPPER(LOWER(COALESCE((t_ref_758.col_2 IN (SELECT sub_840.col_3 FROM tbl_15 AS sub_840)), (t_ref_758.id::NUMERIC(10,2))))) END) - t_ref_758.dyn_col_5) ELSE (('escaped''quote' IN (SELECT sub_841.col_1 FROM tbl_04 AS sub_841))::BOOLEAN) END) ELSE (COALESCE(((((CASE WHEN (((NOT (EXISTS (SELECT 1 FROM tbl_06 AS sub_845 WHERE NULL))) - (NULL + (NULL::TEXT::VARCHAR(32))))::BOOLEAN) THEN NULL ELSE (CASE WHEN EXISTS (SELECT 1 FROM tbl_03 AS sub_846 WHERE sub_846.dyn_col_5) THEN (SELECT (SELECT (SELECT NULL FROM tbl_18 AS inn_851 LIMIT 1) FROM tbl_15 AS sub_850 LIMIT 1) FROM tbl_09 AS sub_849 LIMIT 1) ELSE (EXISTS (SELECT 1 FROM tbl_19 AS sub_852 WHERE sub_852.id)::INT::TEXT) END) END) BETWEEN 1 AND -1) + (NOT (FALSE))) <= ((t_ref_758.id IN (SELECT sub_853.col_1 FROM tbl_14 AS sub_853)) OR FALSE)), FALSE) / NULLIF(t_ref_758.col_1, 0)) END) THEN (((((CASE WHEN ((CASE WHEN (LOWER(UPPER((FALSE - -2147483648))) - ((-2147483648 - LOWER(t_ref_758.col_2)) = t_ref_758.col_2)) THEN ((t_ref_758.id::INT::TEXT)::INT::TEXT) ELSE (SELECT (SELECT inn_855.ref_tbl_10_id FROM tbl_18 AS inn_855 LIMIT 1) FROM tbl_14 AS sub_854 LIMIT 1) END)::INT) THEN (LOWER(EXISTS (SELECT 1 FROM alpha_tbl AS sub_856 WHERE TRUE)) OR EXISTS (SELECT 1 FROM tbl_15 AS sub_858 WHERE sub_858.id)) ELSE TRUE END)::BOOLEAN) + (CASE WHEN (NULLIF((EXISTS (SELECT 1 FROM tbl_12 AS sub_860 WHERE '')::TEXT::VARCHAR(32)), (0 > t_ref_758.col_2)) IS NOT NULL) THEN COALESCE(((UPPER((((SELECT (SELECT inn_863.payload FROM alpha_tbl AS inn_863 LIMIT 1) FROM tbl_05 AS sub_862 LIMIT 1) AND (t_ref_758.col_2 + t_ref_758.id))::TEXT::VARCHAR(32)))::BOOLEAN) - (NULLIF(((CASE WHEN (-1e-5 AND t_ref_758.dyn_col_5) THEN (t_ref_758.col_2 IS NULL) ELSE EXISTS (SELECT 1 FROM tbl_12 AS sub_864 WHERE 'chaos') END) - t_ref_758.dyn_col_5), ((NOT ((NOT (FALSE)))) / NULLIF((('alpha_beta_gamma' = -1) * ''), 0))) + LENGTH(((NULLIF(NULL, t_ref_758.id) / NULLIF(t_ref_758.col_2, 0)) * TRUE)))), ((CASE WHEN (CASE WHEN (SELECT (NOT (-1)) FROM tbl_16 AS sub_865 LIMIT 1) THEN (CASE WHEN (SELECT sub_866.col_1 FROM tbl_03 AS sub_866 LIMIT 1) THEN UPPER((FALSE::TEXT::VARCHAR(32))) ELSE ((3.14159265 AND t_ref_758.col_1) = FALSE) END) ELSE t_ref_758.id END) THEN (NULLIF(((t_ref_758.col_2 / NULLIF(t_ref_758.dyn_col_5, 0))::BOOLEAN), ((SELECT sub_867.dyn_col_6 FROM tbl_01 AS sub_867 LIMIT 1) > UPPER(0.0000000001))) * (NOT (((TRUE::INT) AND (NOT (t_ref_758.dyn_col_5)))))) ELSE ABS((t_ref_758.id IN (SELECT sub_868.col_1 FROM tbl_07 AS sub_868))) END) <> (t_ref_758.dyn_col_5 IN (SELECT sub_869.col_2 FROM tbl_08 AS sub_869)))) ELSE t_ref_758.id END)) = (LOWER(((SELECT (SELECT LENGTH(EXISTS (SELECT 1 FROM tbl_15 AS sub_872 WHERE NULL)) FROM tbl_17 AS sub_871 LIMIT 1) FROM tbl_09 AS sub_870 LIMIT 1) = t_ref_758.col_1)) < (-1.5 AND ((t_ref_758.id IS NULL) * t_ref_758.col_2))))::INT) ELSE t_ref_758.col_1 END)::TEXT)))), 0)) ELSE NULLIF(t_ref_758.id, ((t_ref_758.col_2 IN (SELECT sub_873.col_1 FROM tbl_11 AS sub_873)) IS NOT NULL)) END) BETWEEN 100 AND 100) AS c_882
FROM tbl_03 AS t_ref_758
WHERE t_ref_758.col_2
LIMIT 19;

INSERT INTO tbl_17 (id, col_1, col_2, ref_tbl_11_id) VALUES (89657, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 3.14159265, 42);

INSERT INTO tbl_14 (id, col_1, col_2, ref_tbl_09_id) VALUES (286182, -9223372036854775808, 9223372036854775807, 9223372036854775807);

UPDATE tbl_19 SET col_2 = 'escaped''quote' WHERE id > 89;

SELECT
    (t_ref_883.id::INT) AS c_884,
    ((SELECT ((SELECT NULL FROM tbl_11 AS sub_886 LIMIT 1) = ABS(((NULLIF(sub_885.col_2, (EXISTS (SELECT 1 FROM tbl_06 AS sub_887 WHERE sub_887.col_3)::INT::TEXT)) = sub_885.dyn_col_5) + ((SELECT LENGTH(((CASE WHEN ((SELECT LOWER((LOWER((sub_890.col_1 <= sub_890.col_1)) - (sub_890.id::TEXT))) FROM tbl_15 AS sub_890 LIMIT 1) + (SELECT NULLIF((SELECT (sub_892.col_2::TEXT::VARCHAR(32)) FROM tbl_12 AS sub_892 LIMIT 1), sub_891.id) FROM tbl_17 AS sub_891 LIMIT 1)) THEN sub_889.id ELSE (sub_889.col_2 IN (SELECT sub_893.col_1 FROM tbl_03 AS sub_893)) END) + COALESCE(UPPER((SELECT (SELECT FALSE FROM tbl_06 AS inn_897 LIMIT 1) FROM alpha_tbl AS sub_896 LIMIT 1)), ((NULL >= sub_889.col_2) > sub_889.id)))) FROM tbl_16 AS sub_889 LIMIT 1) / NULLIF(sub_885.col_1, 0))))) FROM tbl_03 AS sub_885 LIMIT 1) > t_ref_883.id) AS c_898
FROM tbl_03 AS t_ref_883
WHERE UPPER(t_ref_883.col_2)
LIMIT 22;

ALTER TABLE tbl_11 ADD COLUMN dyn_col_6 VARCHAR(32);

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_3, dyn_col_5) VALUES (711632, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '__fuzz_marker__', '__fuzz_marker__');
COMMIT;

DELETE FROM tbl_19 WHERE id = 66552;

INSERT INTO gamma_tbl (c_id, b_ref, val_c) VALUES (1, 1, 1000), (2, 2, 2000);

CREATE VIEW v_summary_7 AS SELECT id FROM tbl_15 WHERE id IS NOT NULL;

INSERT INTO tbl_03 (id, col_1, col_2, dyn_col_5) VALUES (376864, TRUE, -1.5, '');

INSERT INTO tbl_15 (id, col_1, col_2, col_3, col_4) VALUES (395331, '[]', 100, -1.5, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

SELECT
    FALSE AS c_900,
    (SELECT sub_901.id FROM tbl_08 AS sub_901 LIMIT 1) AS c_902,
    (t_ref_899.col_2 IN (SELECT sub_903.col_1 FROM tbl_09 AS sub_903)) AS c_936
FROM tbl_19 AS t_ref_899 LEFT JOIN tbl_13 AS j_ref_937 ON t_ref_899.id = j_ref_937.id
WHERE (CASE WHEN LENGTH((SELECT sub_938.col_1 FROM tbl_01 AS sub_938 LIMIT 1)) THEN (CASE WHEN EXISTS (SELECT 1 FROM tbl_08 AS sub_939 WHERE 'escaped''quote') THEN t_ref_899.col_1 ELSE (t_ref_899.col_3 IN (SELECT sub_948.id FROM tbl_05 AS sub_948)) END) ELSE (NULLIF((((LENGTH((42 IN (SELECT sub_949.id FROM tbl_12 AS sub_949))) + EXISTS (SELECT 1 FROM tbl_17 AS sub_951 WHERE 42))::INT)::BOOLEAN), t_ref_899.col_2) <= ((ABS((FALSE IS NOT NULL))::TEXT) * (SELECT (SELECT inn_998.col_1 FROM tbl_18 AS inn_998 LIMIT 1) FROM tbl_08 AS sub_958 LIMIT 1))) END)
LIMIT 31;

CREATE INDEX idx_tbl_04_col_3_4 ON tbl_04 (col_3);

CREATE TABLE tbl_20 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2),
    col_4 NUMERIC(10,2),
    ref_tbl_14_id INT,
    FOREIGN KEY (ref_tbl_14_id) REFERENCES tbl_14(id)
);

CREATE INDEX idx_tbl_13_col_4_5 ON tbl_13 (col_4);

DELETE FROM tbl_08 WHERE id = 76371;

BEGIN;
INSERT INTO tbl_14 (id, col_1, col_2, ref_tbl_09_id) VALUES (223284, 9223372036854775807, -1, -1);
INSERT INTO tbl_07 (id, col_1, col_2, col_3, col_4) VALUES (58411, -1, '[]', 999999999.99, -1e-5);
INSERT INTO tbl_18 (id, col_1, col_2, col_3, ref_tbl_10_id) VALUES (780715, '', 2147483647, '', -1);
COMMIT;

CREATE TABLE tbl_21 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 VARCHAR(64),
    col_3 BOOLEAN
);

SELECT
    1 AS c_1000,
    (t_ref_999.col_1 + (((CASE WHEN UPPER('escaped''quote') THEN -2147483648 ELSE (((CASE WHEN (TRUE::TEXT::VARCHAR(32)) THEN (SELECT (SELECT inn_1008.payload FROM alpha_tbl AS inn_1008 LIMIT 1) FROM tbl_14 AS sub_1001 LIMIT 1) ELSE t_ref_999.col_1 END)::INT::TEXT)::BOOLEAN) END) - t_ref_999.col_3) <= UPPER(FALSE))) AS c_1009,
    ((t_ref_999.col_1 IN (SELECT sub_1010.col_1 FROM tbl_20 AS sub_1010))::INT::TEXT) AS c_1011
FROM tbl_12 AS t_ref_999 LEFT JOIN tbl_01 AS j_ref_1012 ON t_ref_999.id = j_ref_1012.id
WHERE t_ref_999.col_2
LIMIT 29;

UPDATE tbl_06 SET col_1 = '' WHERE id > 100;

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (669757, 'chaos', 0.0000000001, -9223372036854775808, '{}');
COMMIT;

SELECT
    UPPER((TRUE - (SELECT (EXISTS (SELECT 1 FROM tbl_19 AS sub_1015 WHERE sub_1015.col_2) < (COALESCE(((((SELECT (sub_1018.col_2 = (ABS((((sub_1018.col_2 IN (SELECT sub_1019.col_2 FROM tbl_01 AS sub_1019)) - (SELECT sub_1020.col_2 FROM tbl_02 AS sub_1020 LIMIT 1)) AND ((CASE WHEN UPPER(0.0) THEN (NOT (999999999.99)) ELSE (sub_1018.col_1 = NULL) END) + UPPER((CASE WHEN FALSE THEN sub_1018.col_1 ELSE sub_1018.col_1 END))))) + (((CASE WHEN (EXISTS (SELECT 1 FROM tbl_11 AS sub_1021 WHERE sub_1021.id) >= TRUE) THEN (NOT ((0 / NULLIF(TRUE, 0)))) ELSE (1.0 + (sub_1018.dyn_col_5 / NULLIF(1e10, 0))) END) > (SELECT (SELECT NULL FROM tbl_02 AS inn_1023 LIMIT 1) FROM tbl_02 AS sub_1022 LIMIT 1)) / NULLIF(UPPER(((SELECT sub_1024.col_3 FROM tbl_06 AS sub_1024 LIMIT 1) <= sub_1018.col_1)), 0)))) FROM tbl_03 AS sub_1018 LIMIT 1)::TEXT)::TEXT) / NULLIF(COALESCE((((sub_1014.col_1 - (FALSE / NULLIF((CASE WHEN (((EXISTS (SELECT 1 FROM tbl_14 AS sub_1025 WHERE sub_1025.col_2)::INT::TEXT) / NULLIF((CASE WHEN (('' AND sub_1014.col_1) - (SELECT sub_1026.id FROM tbl_14 AS sub_1026 LIMIT 1)) THEN (sub_1014.col_1::TEXT) ELSE (sub_1014.col_2 + (sub_1014.id::TEXT)) END), 0))::BOOLEAN) THEN 100 ELSE sub_1014.col_2 END), 0))) - (sub_1014.id / NULLIF(sub_1014.col_2, 0))) >= EXISTS (SELECT 1 FROM tbl_03 AS sub_1027 WHERE 100)), (((ABS(COALESCE(((FALSE + ((CASE WHEN COALESCE(sub_1014.id, sub_1014.id) THEN (NULL::TEXT) ELSE (sub_1014.col_1 AND sub_1014.col_2) END) <= ((SELECT (SELECT inn_1029.col_1 FROM tbl_20 AS inn_1029 LIMIT 1) FROM tbl_01 AS sub_1028 LIMIT 1) / NULLIF(-9223372036854775808, 0)))) / NULLIF((0.0 IN (SELECT sub_1030.col_4 FROM tbl_08 AS sub_1030)), 0)), (NULL + (sub_1014.col_2 IS NULL)))) * EXISTS (SELECT 1 FROM tbl_01 AS sub_1031 WHERE -1.5)) - ((UPPER((sub_1014.col_2 IN (SELECT sub_1038.id FROM tbl_07 AS sub_1038))) * (FALSE + sub_1014.col_1)) > sub_1014.id)) > (EXISTS (SELECT 1 FROM tbl_18 AS sub_1039 WHERE sub_1039.col_2) OR (1e10 / NULLIF(EXISTS (SELECT 1 FROM tbl_12 AS sub_1040 WHERE sub_1040.col_3), 0))))), 0)), (CASE WHEN sub_1014.id THEN (SELECT (SELECT inn_1043.ref_tbl_11_id FROM tbl_17 AS inn_1043 LIMIT 1) FROM tbl_04 AS sub_1042 LIMIT 1) ELSE TRUE END))::NUMERIC(10,2))) FROM tbl_09 AS sub_1014 LIMIT 1))) AS c_1044,
    (t_ref_1013.col_1 - (NULLIF((SELECT (SELECT inn_1051.id FROM tbl_05 AS inn_1051 LIMIT 1) FROM tbl_14 AS sub_1045 LIMIT 1), t_ref_1013.id) AND t_ref_1013.dyn_col_6)) AS c_1052,
    EXISTS (SELECT 1 FROM tbl_18 AS sub_1053 WHERE sub_1053.id) AS c_1055
FROM tbl_01 AS t_ref_1013 LEFT JOIN tbl_13 AS j_ref_1056 ON t_ref_1013.id = j_ref_1056.id
WHERE ((NOT ((CASE WHEN (SELECT ((ABS((NOT (sub_1057.id))) - TRUE) > -9223372036854775808) FROM alpha_tbl AS sub_1057 LIMIT 1) THEN (((SELECT (SELECT NULL FROM tbl_20 AS inn_1066 LIMIT 1) FROM tbl_10 AS sub_1058 LIMIT 1) IS NOT NULL) / NULLIF(t_ref_1013.col_2, 0)) ELSE t_ref_1013.col_4 END)))::INT::TEXT)
LIMIT 14;

CREATE TABLE tbl_22 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 VARCHAR(64),
    col_4 BOOLEAN
);

DELETE FROM tbl_11 WHERE id = 31543;

ALTER TABLE tbl_21 DROP COLUMN col_1;

BEGIN;
INSERT INTO tbl_13 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (129148, 1e10, 'escaped''quote', -9223372036854775808, -2147483648, 9223372036854775807);
COMMIT;

BEGIN;
INSERT INTO tbl_13 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (194008, -1e-5, 'alpha_beta_gamma', 0, 42, -2147483648);
INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_04_id) VALUES (79060, 'alpha_beta_gamma', '[]', -1);
INSERT INTO tbl_12 (id, col_1, col_2, col_3) VALUES (474451, 1, 0.0, 3.14159265);
COMMIT;

UPDATE tbl_16 SET col_1 = '[]' WHERE id > 18;

SELECT
    t_ref_1067.id AS c_1068
FROM tbl_05 AS t_ref_1067 LEFT JOIN tbl_20 AS j_ref_1069 ON t_ref_1067.id = j_ref_1069.id
WHERE (SELECT ((CASE WHEN EXISTS (SELECT 1 FROM alpha_tbl AS sub_1071 WHERE sub_1071.payload) THEN ((UPPER((COALESCE('__fuzz_marker__', (UPPER(((sub_1070.payload IN (SELECT sub_1095.col_3 FROM tbl_05 AS sub_1095)) - sub_1070.id))::TEXT::VARCHAR(32))) < ((sub_1070.id IN (SELECT sub_1097.col_2 FROM tbl_02 AS sub_1097)) OR FALSE))) * (FALSE AND NULLIF((sub_1070.payload * (sub_1070.id IN (SELECT sub_1099.id FROM tbl_07 AS sub_1099))), NULLIF((SELECT (SELECT inn_1101.id FROM tbl_11 AS inn_1101 LIMIT 1) FROM alpha_tbl AS sub_1100 LIMIT 1), sub_1070.id)))) * '__fuzz_marker__') ELSE (CASE WHEN -1e-5 THEN ((((sub_1070.id IN (SELECT sub_1102.id FROM tbl_11 AS sub_1102)) <> (-1.5 IN (SELECT sub_1103.id FROM tbl_08 AS sub_1103))) / NULLIF(((LENGTH((SELECT (NULLIF((NOT (EXISTS (SELECT 1 FROM tbl_07 AS sub_1105 WHERE sub_1105.col_3))), sub_1104.col_3) AND sub_1104.id) FROM tbl_05 AS sub_1104 LIMIT 1)) * (CASE WHEN (FALSE IN (SELECT sub_1107.col_1 FROM tbl_06 AS sub_1107)) THEN ((((CASE WHEN (sub_1070.payload * ((SELECT (sub_1110.id BETWEEN -9223372036854775808 AND -1) FROM alpha_tbl AS sub_1110 LIMIT 1)::INT::TEXT)) THEN ((sub_1070.payload IN (SELECT sub_1111.id FROM tbl_15 AS sub_1111)) = ((CASE WHEN (1.0 OR NULL) THEN ((sub_1070.id * sub_1070.payload) > LOWER(sub_1070.payload)) ELSE TRUE END) * (NOT (((SELECT (SELECT 3.14159265 FROM tbl_14 AS inn_1113 LIMIT 1) FROM tbl_14 AS sub_1112 LIMIT 1) + FALSE))))) ELSE (NOT (((NOT (LENGTH((sub_1070.id / NULLIF(sub_1070.id, 0))))) OR (SELECT NULLIF('alpha_beta_gamma', NULL) FROM tbl_20 AS sub_1114 LIMIT 1)))) END) IS NULL) IS NULL) AND (LENGTH((CASE WHEN (NOT (EXISTS (SELECT 1 FROM tbl_15 AS sub_1115 WHERE sub_1115.col_2))) THEN (((((sub_1070.payload / NULLIF(sub_1070.id, 0)) + sub_1070.payload) = ((sub_1070.id::TEXT) < '__fuzz_marker__')) - (SELECT (SELECT inn_1117.id FROM tbl_03 AS inn_1117 LIMIT 1) FROM tbl_22 AS sub_1116 LIMIT 1))::INT) ELSE LOWER((sub_1070.id BETWEEN -9223372036854775808 AND -2147483648)) END)) * ((SELECT (SELECT inn_1119.id FROM tbl_08 AS inn_1119 LIMIT 1) FROM tbl_16 AS sub_1118 LIMIT 1) / NULLIF(EXISTS (SELECT 1 FROM tbl_02 AS sub_1120 WHERE sub_1120.col_1), 0)))) ELSE sub_1070.id END)) IS NOT NULL), 0)) <> (sub_1070.id < (1e10 IN (SELECT sub_1123.id FROM tbl_19 AS sub_1123)))) ELSE (sub_1070.payload AND (NULLIF((CASE WHEN (SELECT LOWER(sub_1124.col_2) FROM tbl_20 AS sub_1124 LIMIT 1) THEN sub_1070.id ELSE (((TRUE IN (SELECT sub_1125.id FROM tbl_11 AS sub_1125))::BOOLEAN)::INT) END), sub_1070.id) - (sub_1070.payload * ((sub_1070.id IN (SELECT sub_1127.id FROM tbl_10 AS sub_1127)) + sub_1070.payload)))) END) END) = UPPER(COALESCE(((((sub_1070.id IN (SELECT sub_1130.col_2 FROM tbl_13 AS sub_1130)) IS NULL)::INT::TEXT) AND sub_1070.payload), ((sub_1070.payload IN (SELECT sub_1134.id FROM tbl_19 AS sub_1134)) / NULLIF((CASE WHEN EXISTS (SELECT 1 FROM tbl_19 AS sub_1156 WHERE FALSE) THEN (sub_1070.payload <> ((-1e-5::INT) * (ABS((ABS(-1e-5)::TEXT::VARCHAR(32)))::INT::TEXT))) ELSE (sub_1070.id IS NULL) END), 0))))) FROM alpha_tbl AS sub_1070 LIMIT 1)
LIMIT 16;

SELECT
    ABS(UPPER((1.0 IN (SELECT sub_1162.col_1 FROM tbl_22 AS sub_1162)))) AS c_1164
FROM tbl_08 AS t_ref_1161 LEFT JOIN tbl_04 AS j_ref_1165 ON t_ref_1161.id = j_ref_1165.id
WHERE (SELECT (SELECT inn_1167.id FROM tbl_12 AS inn_1167 LIMIT 1) FROM tbl_18 AS sub_1166 LIMIT 1)
LIMIT 14;

CREATE INDEX idx_tbl_11_col_1_6 ON tbl_11 (col_1);

INSERT INTO tbl_02 (id, col_1, col_2) VALUES (450005, 'chaos', 'alpha_beta_gamma');

UPDATE tbl_21 SET col_3 = FALSE WHERE id > 26;

INSERT INTO tbl_14 (id, col_1, col_2, ref_tbl_09_id) VALUES (713980, 2147483647, 0, 9223372036854775807);

INSERT INTO tbl_12 (id, col_1, col_2, col_3) VALUES (268629, -9223372036854775808, 0.0, 1.0);

CREATE VIEW v_summary_8 AS SELECT id FROM tbl_16 WHERE id IS NOT NULL;

UPDATE tbl_09 SET col_1 = 9223372036854775807 WHERE id > 57;

INSERT INTO tbl_16 (id, col_1, col_2) VALUES (774499, '[]', 0);

UPDATE tbl_18 SET ref_tbl_10_id = -1 WHERE id > 99;

SELECT
    ((t_ref_1168.col_1::TEXT::VARCHAR(32)) <> (t_ref_1168.col_2 OR (((t_ref_1168.id IN (SELECT sub_1169.col_2 FROM tbl_06 AS sub_1169))::TEXT) <> LENGTH(EXISTS (SELECT 1 FROM tbl_12 AS sub_1170 WHERE sub_1170.col_1))))) AS c_1171
FROM tbl_09 AS t_ref_1168 LEFT JOIN tbl_21 AS j_ref_1172 ON t_ref_1168.id = j_ref_1172.id
WHERE (EXISTS (SELECT 1 FROM tbl_12 AS sub_1173 WHERE FALSE)::BOOLEAN)
LIMIT 21;

UPDATE tbl_17 SET col_2 = -1e-5 WHERE id > 89;

CREATE TABLE tbl_23 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 NUMERIC(10,2)
);

INSERT INTO tbl_14 (id, col_1, col_2, ref_tbl_09_id) VALUES (188524, 42, 0, 0);

CREATE INDEX idx_alpha_tbl_id_7 ON alpha_tbl (id);

SELECT
    (((((SELECT NULL FROM alpha_tbl AS sub_1177 LIMIT 1) > (SELECT (SELECT NULL FROM tbl_23 AS inn_1179 LIMIT 1) FROM tbl_09 AS sub_1178 LIMIT 1)) AND t_ref_1176.id) IS NULL) * ((EXISTS (SELECT 1 FROM tbl_03 AS sub_1180 WHERE 'alpha_beta_gamma') * t_ref_1176.col_3) AND t_ref_1176.col_2)) AS c_1187
FROM tbl_07 AS t_ref_1176
WHERE t_ref_1176.col_3
LIMIT 39;

BEGIN;
INSERT INTO tbl_17 (id, col_1, col_2, ref_tbl_11_id) VALUES (966121, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 999999999.99, 100);
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (658394, '__fuzz_marker__', 0, TRUE, 999999999.99);
INSERT INTO tbl_15 (id, col_1, col_2, col_3, col_4) VALUES (935445, '{}', -1, 0.0000000001, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

UPDATE tbl_02 SET col_2 = 'ref_val' WHERE id > 30;

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (441391, '__fuzz_marker__', 1e10, 42, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

CREATE INDEX idx_tbl_15_col_3_8 ON tbl_15 (col_3);

CREATE TABLE alpha_tbl (
    a_id INT PRIMARY KEY,
    val_a INT
);

SELECT *
FROM alpha_tbl a
LEFT JOIN (
    beta_tbl b
    LEFT JOIN LATERAL (
        SELECT g.c_id, (g.val_c + a.val_a + b.val_b) AS computed_sum
        FROM gamma_tbl g
        WHERE g.b_ref = b.b_id
          AND g.val_c > a.val_a
    ) lat ON TRUE
) ON a.a_id = b.a_ref
WHERE a.val_a > 5;

UPDATE tbl_14 SET col_2 = -1 WHERE id > 67;

CREATE TABLE tbl_24 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 JSONB,
    col_3 JSONB,
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

CREATE TABLE tbl_25 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2)
);

CREATE VIEW v_summary_9 AS SELECT id FROM tbl_21 WHERE id IS NOT NULL;

CREATE VIEW v_summary_10 AS SELECT id FROM tbl_10 WHERE id IS NOT NULL;

UPDATE tbl_09 SET col_1 = -9223372036854775808 WHERE id > 18;

BEGIN;
INSERT INTO tbl_24 (id, col_1, col_2, col_3, ref_tbl_03_id) VALUES (127302, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '[]', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -2147483648);
COMMIT;

INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_04_id) VALUES (233549, '__fuzz_marker__', '{}', 100);

CREATE VIEW v_summary_11 AS SELECT id FROM tbl_12 WHERE id IS NOT NULL;

UPDATE tbl_18 SET col_2 = 1 WHERE id > 5;

BEGIN;
INSERT INTO tbl_16 (id, col_1, col_2) VALUES (769910, '{}', -2147483648);
INSERT INTO tbl_15 (id, col_1, col_2, col_3, col_4) VALUES (769474, '{}', 42, 1e10, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

CREATE INDEX idx_tbl_10_col_2_9 ON tbl_10 (col_2);

CREATE INDEX idx_tbl_10_col_1_10 ON tbl_10 (col_1);

SELECT
    ((UPPER(((('ref_val' / NULLIF((((t_ref_1188.col_3 >= (EXISTS (SELECT 1 FROM tbl_19 AS sub_1189 WHERE sub_1189.col_4) - ((t_ref_1188.col_3 - EXISTS (SELECT 1 FROM tbl_14 AS sub_1192 WHERE -9223372036854775808)) - t_ref_1188.col_1))) < NULL) - EXISTS (SELECT 1 FROM tbl_10 AS sub_1197 WHERE NULL)), 0)) > ((t_ref_1188.col_1 * (((NULLIF(t_ref_1188.id, ((NOT (NULLIF((((SELECT (SELECT NULL FROM tbl_12 AS inn_1209 LIMIT 1) FROM tbl_25 AS sub_1208 LIMIT 1) + NULL) OR EXISTS (SELECT 1 FROM tbl_18 AS sub_1210 WHERE sub_1210.id)), EXISTS (SELECT 1 FROM tbl_15 AS sub_1211 WHERE sub_1211.col_3)))) / NULLIF(TRUE, 0))) < (NULLIF(COALESCE((ABS(LENGTH((((t_ref_1188.col_3 IN (SELECT sub_1212.dyn_col_5 FROM tbl_05 AS sub_1212)) AND (t_ref_1188.col_2::BOOLEAN)) + (SELECT (SELECT inn_1214.col_3 FROM tbl_18 AS inn_1214 LIMIT 1) FROM tbl_16 AS sub_1213 LIMIT 1)))) / NULLIF(((ABS(t_ref_1188.col_4) / NULLIF(t_ref_1188.col_4, 0)) < (3.14159265 AND EXISTS (SELECT 1 FROM tbl_19 AS sub_1215 WHERE FALSE))), 0)), (ABS(((SELECT NULL FROM tbl_06 AS sub_1216 LIMIT 1) < ((FALSE / NULLIF(TRUE, 0))::NUMERIC(10,2)))) BETWEEN 2147483647 AND -2147483648)), t_ref_1188.id) + (SELECT (SELECT inn_1220.col_2 FROM tbl_01 AS inn_1220 LIMIT 1) FROM tbl_17 AS sub_1217 LIMIT 1))) * t_ref_1188.id) >= ((t_ref_1188.col_2::NUMERIC(10,2))::TEXT))) = (((NULLIF((('alpha_beta_gamma'::BOOLEAN) = (UPPER(FALSE) AND t_ref_1188.col_4)), (NOT (t_ref_1188.col_2))) + (((t_ref_1188.col_3::TEXT::VARCHAR(32)) > (SELECT (SELECT inn_1231.ref_tbl_11_id FROM tbl_17 AS inn_1231 LIMIT 1) FROM tbl_04 AS sub_1221 LIMIT 1)) = (CASE WHEN '' THEN t_ref_1188.col_3 ELSE t_ref_1188.col_2 END))) < t_ref_1188.col_2)::INT))) / NULLIF((((LOWER(((SELECT (LOWER(((((9223372036854775807 * ((TRUE IS NULL) / NULLIF(LOWER(sub_1232.col_2), 0)))::INT) >= sub_1232.col_2) BETWEEN -9223372036854775808 AND -2147483648)) < sub_1232.col_1) FROM tbl_02 AS sub_1232 LIMIT 1)::BOOLEAN)) - (-1.5 IN (SELECT sub_1233.col_4 FROM tbl_13 AS sub_1233))) / NULLIF(t_ref_1188.id, 0))::INT), 0))) + ((((SELECT (SELECT inn_1239.col_2 FROM tbl_06 AS inn_1239 LIMIT 1) FROM tbl_07 AS sub_1238 LIMIT 1)::INT::TEXT) OR 3.14159265) = (t_ref_1188.col_1 = (CASE WHEN 'alpha_beta_gamma' THEN (('__fuzz_marker__' IN (SELECT sub_1240.col_2 FROM tbl_14 AS sub_1240)) + UPPER((NULLIF((t_ref_1188.id AND ABS(0.0000000001)), ((NOT (((SELECT (CASE WHEN (((sub_1257.id <= (sub_1257.id - 0.0000000001)) AND LOWER(sub_1257.id))::NUMERIC(10,2)) THEN '__fuzz_marker__' ELSE (SELECT (SELECT '__fuzz_marker__' FROM tbl_14 AS inn_1259 LIMIT 1) FROM tbl_03 AS sub_1258 LIMIT 1) END) FROM tbl_02 AS sub_1257 LIMIT 1) < (((NOT (t_ref_1188.id)) - t_ref_1188.col_1) IS NOT NULL)))) + (SELECT (SELECT inn_1267.id FROM tbl_14 AS inn_1267 LIMIT 1) FROM tbl_08 AS sub_1260 LIMIT 1))) * t_ref_1188.col_2))) ELSE LOWER('__fuzz_marker__') END)))) >= UPPER((((((t_ref_1188.col_4 IN (SELECT sub_1268.col_4 FROM tbl_01 AS sub_1268))::TEXT)::BOOLEAN) >= ((CASE WHEN t_ref_1188.col_3 THEN UPPER('alpha_beta_gamma') ELSE (ABS((t_ref_1188.col_3 IN (SELECT sub_1269.col_3 FROM tbl_13 AS sub_1269)))::INT) END) - 0.0))::BOOLEAN))) AS c_1270,
    (((SELECT (SELECT NULL FROM tbl_01 AS inn_1283 LIMIT 1) FROM tbl_23 AS sub_1271 LIMIT 1) BETWEEN 1 AND 0) >= FALSE) AS c_1284,
    LENGTH(t_ref_1188.col_2) AS c_1285
FROM tbl_19 AS t_ref_1188
WHERE (NULL IN (SELECT sub_1286.col_4 FROM tbl_22 AS sub_1286))
LIMIT 29;

INSERT INTO tbl_05 (id, col_1, col_3, dyn_col_5) VALUES (225532, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'chaos', 'chaos');

CREATE INDEX idx_tbl_17_id_11 ON tbl_17 (id);

UPDATE tbl_07 SET col_3 = 0.0 WHERE id > 39;

UPDATE tbl_04 SET col_1 = 'chaos' WHERE id > 72;

INSERT INTO tbl_17 (id, col_1, col_2, ref_tbl_11_id) VALUES (17497, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 3.14159265, -1);

ALTER TABLE tbl_08 ADD COLUMN dyn_col_6 VARCHAR(32);

INSERT INTO tbl_13 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (313139, 0.0, 'escaped''quote', 0, 9223372036854775807, 0);

BEGIN;
INSERT INTO tbl_16 (id, col_1, col_2) VALUES (620460, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -1);
INSERT INTO tbl_18 (id, col_1, col_2, col_3, ref_tbl_10_id) VALUES (796450, 'chaos', -9223372036854775808, '__fuzz_marker__', 9223372036854775807);
COMMIT;

SELECT
    ABS(LOWER((SELECT (SELECT 2147483647 FROM tbl_03 AS inn_1414 LIMIT 1) FROM tbl_20 AS sub_1337 LIMIT 1))) AS c_1415,
    COALESCE(t_ref_1336.col_4, ((SELECT (SELECT inn_1472.ref_tbl_09_id FROM tbl_14 AS inn_1472 LIMIT 1) FROM tbl_13 AS sub_1416 LIMIT 1) - (SELECT (((((UPPER(((sub_1473.col_1 IN (SELECT sub_1474.col_2 FROM tbl_06 AS sub_1474)) - ((SELECT (SELECT inn_1485.col_2 FROM tbl_14 AS inn_1485 LIMIT 1) FROM tbl_23 AS sub_1480 LIMIT 1) / NULLIF((sub_1473.col_3 + EXISTS (SELECT 1 FROM tbl_03 AS sub_1486 WHERE -1)), 0)))) < ((((CASE WHEN (((NOT ((((TRUE IS NOT NULL) - (sub_1473.col_1 + sub_1473.col_2)) / NULLIF((SELECT (SELECT NULL FROM tbl_09 AS inn_1489 LIMIT 1) FROM tbl_07 AS sub_1488 LIMIT 1), 0)))) IS NOT NULL) / NULLIF((LENGTH((NULL IN (SELECT sub_1490.id FROM tbl_08 AS sub_1490)))::INT), 0)) THEN (sub_1473.id * (sub_1473.col_1 - (SELECT TRUE FROM tbl_11 AS sub_1491 LIMIT 1))) ELSE ((((sub_1473.id IS NOT NULL) <> (sub_1473.col_2 = ((SELECT (SELECT inn_1493.id FROM tbl_07 AS inn_1493 LIMIT 1) FROM alpha_tbl AS sub_1492 LIMIT 1) / NULLIF(sub_1473.col_3, 0))))::NUMERIC(10,2)) * (sub_1473.dyn_col_6 + ((CASE WHEN sub_1473.col_2 THEN ((sub_1473.col_4 / NULLIF(sub_1473.col_1, 0)) <= (sub_1473.col_2 - sub_1473.dyn_col_6)) ELSE (SELECT (SELECT inn_1495.col_2 FROM tbl_22 AS inn_1495 LIMIT 1) FROM tbl_13 AS sub_1494 LIMIT 1) END) AND (CASE WHEN (CASE WHEN EXISTS (SELECT 1 FROM tbl_20 AS sub_1496 WHERE 1.0) THEN (NULL::INT::TEXT) ELSE (sub_1473.col_3::TEXT) END) THEN sub_1473.col_1 ELSE NULLIF(LOWER(sub_1473.col_1), LENGTH(sub_1473.id)) END)))) END) * LOWER(((CASE WHEN (CASE WHEN ((LOWER(sub_1473.id) > (sub_1473.col_3 + sub_1473.col_2)) * ((2147483647 BETWEEN 100 AND 9223372036854775807) <= (CASE WHEN sub_1473.col_2 THEN sub_1473.dyn_col_6 ELSE 0.0000000001 END))) THEN sub_1473.col_4 ELSE (FALSE BETWEEN 1 AND 1) END) THEN ABS((((sub_1473.col_2 - sub_1473.col_1) + EXISTS (SELECT 1 FROM tbl_12 AS sub_1497 WHERE NULL)) <> (SELECT (SELECT TRUE FROM tbl_17 AS inn_1499 LIMIT 1) FROM tbl_01 AS sub_1498 LIMIT 1))) ELSE (CASE WHEN (sub_1473.col_4 + ((SELECT (SELECT inn_1501.id FROM tbl_06 AS inn_1501 LIMIT 1) FROM tbl_07 AS sub_1500 LIMIT 1) * (sub_1473.id::BOOLEAN))) THEN (sub_1473.col_1 IN (SELECT sub_1502.col_3 FROM tbl_06 AS sub_1502)) ELSE NULL END) END)::TEXT::VARCHAR(32)))) >= FALSE) * UPPER(((SELECT (-9223372036854775808 - (SELECT (SELECT inn_1507.id FROM tbl_09 AS inn_1507 LIMIT 1) FROM tbl_06 AS sub_1506 LIMIT 1)) FROM tbl_22 AS sub_1505 LIMIT 1) >= (CASE WHEN EXISTS (SELECT 1 FROM tbl_23 AS sub_1508 WHERE sub_1508.col_1) THEN (CASE WHEN (CASE WHEN (((SELECT (SELECT inn_1511.col_3 FROM tbl_20 AS inn_1511 LIMIT 1) FROM tbl_16 AS sub_1510 LIMIT 1)::INT)::BOOLEAN) THEN ((SELECT (SELECT FALSE FROM tbl_02 AS inn_1513 LIMIT 1) FROM tbl_01 AS sub_1512 LIMIT 1) / NULLIF((((SELECT (SELECT inn_1515.col_4 FROM tbl_04 AS inn_1515 LIMIT 1) FROM tbl_08 AS sub_1514 LIMIT 1) <= TRUE)::INT::TEXT), 0)) ELSE sub_1473.col_3 END) THEN ((NULLIF((SELECT sub_1516.col_2 FROM tbl_09 AS sub_1516 LIMIT 1), (EXISTS (SELECT 1 FROM tbl_23 AS sub_1517 WHERE sub_1517.id) * (sub_1473.col_3 = sub_1473.id)))::INT)::BOOLEAN) ELSE sub_1473.col_2 END) ELSE sub_1473.dyn_col_6 END))))) * sub_1473.id) < (FALSE / NULLIF((sub_1473.dyn_col_6 AND ((SELECT (SELECT inn_1519.col_1 FROM tbl_09 AS inn_1519 LIMIT 1) FROM tbl_23 AS sub_1518 LIMIT 1) - COALESCE((TRUE - sub_1473.col_4), NULL))), 0)))::INT::TEXT) AND ((SELECT (SELECT 1.0 FROM tbl_17 AS inn_1521 LIMIT 1) FROM tbl_07 AS sub_1520 LIMIT 1) + (((FALSE + ((UPPER(sub_1473.col_3) * ((CASE WHEN (SELECT (LOWER(((CASE WHEN TRUE THEN sub_1522.col_1 ELSE sub_1522.id END) <= sub_1522.col_2)) / NULLIF((((sub_1522.col_2 * NULL) AND sub_1522.col_1) <> sub_1522.dyn_col_5), 0)) FROM tbl_03 AS sub_1522 LIMIT 1) THEN ((SELECT (SELECT inn_1524.col_2 FROM tbl_12 AS inn_1524 LIMIT 1) FROM tbl_06 AS sub_1523 LIMIT 1) / NULLIF((LOWER(ABS(sub_1473.col_2)) AND (CASE WHEN (((SELECT TRUE FROM tbl_16 AS sub_1525 LIMIT 1) IS NULL) <= (CASE WHEN NULLIF(sub_1473.col_2, sub_1473.dyn_col_6) THEN (9223372036854775807 AND sub_1473.col_2) ELSE (100 / NULLIF(-1, 0)) END)) THEN (SELECT (SELECT 3.14159265 FROM tbl_07 AS inn_1527 LIMIT 1) FROM tbl_20 AS sub_1526 LIMIT 1) ELSE sub_1473.dyn_col_6 END)), 0)) ELSE (UPPER(NULLIF(((EXISTS (SELECT 1 FROM tbl_05 AS sub_1528 WHERE sub_1528.col_1)::TEXT)::INT), (CASE WHEN (sub_1473.col_2 IN (SELECT sub_1529.col_2 FROM tbl_23 AS sub_1529)) THEN (sub_1473.col_1 IN (SELECT sub_1530.id FROM tbl_12 AS sub_1530)) ELSE (EXISTS (SELECT 1 FROM tbl_22 AS sub_1531 WHERE sub_1531.col_4) < sub_1473.col_1) END))) AND sub_1473.col_1) END) OR LOWER((sub_1473.col_2 * TRUE)))) >= (COALESCE(sub_1473.col_1, (1e10 IN (SELECT sub_1532.col_3 FROM tbl_21 AS sub_1532))) - (sub_1473.col_1::INT::TEXT)))) - (COALESCE(sub_1473.col_1, (FALSE IN (SELECT sub_1535.col_3 FROM tbl_06 AS sub_1535))) IS NOT NULL)) / NULLIF(sub_1473.col_2, 0)))) FROM tbl_01 AS sub_1473 LIMIT 1))) AS c_1536
FROM tbl_15 AS t_ref_1336
WHERE (SELECT (SELECT inn_1539.id FROM tbl_23 AS inn_1539 LIMIT 1) FROM tbl_23 AS sub_1537 LIMIT 1)
LIMIT 3;

CREATE INDEX idx_tbl_05_dyn_col_5_12 ON tbl_05 (dyn_col_5);

UPDATE tbl_17 SET col_1 = '[]' WHERE id > 1;

INSERT INTO tbl_24 (id, col_1, col_2, col_3, ref_tbl_03_id) VALUES (740719, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '[]', 1);

UPDATE tbl_12 SET col_3 = 1.0 WHERE id > 16;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (293260, 'escaped''quote', TRUE, '[]', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
INSERT INTO tbl_02 (id, col_1, col_2) VALUES (186047, '__fuzz_marker__', 'chaos');
COMMIT;

CREATE TABLE tbl_26 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2),
    col_4 BOOLEAN
);

SELECT
    t_ref_1540.id AS c_1541,
    FALSE AS c_1542,
    (NOT ((((SELECT sub_1543.col_3 FROM tbl_15 AS sub_1543 LIMIT 1) < 0.0000000001) * (((SELECT sub_1544.ref_tbl_10_id FROM tbl_18 AS sub_1544 LIMIT 1) AND (SELECT ((NULL AND EXISTS (SELECT 1 FROM tbl_03 AS sub_1546 WHERE FALSE))::INT::TEXT) FROM tbl_09 AS sub_1545 LIMIT 1)) + NULL)))) AS c_1547
FROM tbl_17 AS t_ref_1540
WHERE (TRUE + ((((CASE WHEN t_ref_1540.ref_tbl_11_id THEN (((NULLIF(t_ref_1540.col_2, ((SELECT sub_1548.id FROM tbl_10 AS sub_1548 LIMIT 1)::INT)) + (t_ref_1540.id IN (SELECT sub_1549.dyn_col_5 FROM tbl_05 AS sub_1549))) BETWEEN -2147483648 AND -1) + (UPPER(t_ref_1540.id) >= (((NOT ((NULL >= (t_ref_1540.col_1::TEXT)))) - (SELECT (SELECT FALSE FROM tbl_16 AS inn_1580 LIMIT 1) FROM tbl_11 AS sub_1572 LIMIT 1)) / NULLIF((CASE WHEN ((t_ref_1540.ref_tbl_11_id IN (SELECT sub_1581.col_1 FROM tbl_09 AS sub_1581))::TEXT) THEN (((t_ref_1540.col_1 = ((CASE WHEN FALSE THEN t_ref_1540.col_2 ELSE t_ref_1540.id END) OR (((SELECT LENGTH((sub_1587.col_4 - sub_1587.col_1)) FROM tbl_26 AS sub_1587 LIMIT 1) <> EXISTS (SELECT 1 FROM tbl_19 AS sub_1588 WHERE sub_1588.col_2)) BETWEEN 9223372036854775807 AND 2147483647))) - (SELECT (SELECT inn_1590.dyn_col_5 FROM tbl_05 AS inn_1590 LIMIT 1) FROM tbl_20 AS sub_1589 LIMIT 1))::INT) ELSE (EXISTS (SELECT 1 FROM tbl_12 AS sub_1591 WHERE sub_1591.col_2)::TEXT::VARCHAR(32)) END), 0)))) ELSE (t_ref_1540.col_1::NUMERIC(10,2)) END) + FALSE) + LENGTH((SELECT (SELECT inn_1597.col_2 FROM tbl_17 AS inn_1597 LIMIT 1) FROM tbl_04 AS sub_1596 LIMIT 1))) + (((UPPER(LENGTH((TRUE::BOOLEAN)))::TEXT::VARCHAR(32))::TEXT::VARCHAR(32))::BOOLEAN)))
LIMIT 41;

BEGIN;
INSERT INTO tbl_25 (id, col_1, col_2) VALUES (32545, 0.0000000001, 1.0);
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (686923, 'chaos', TRUE, '{}', '[]');
COMMIT;

SELECT
    t_ref_1598.col_2 AS c_1599,
    t_ref_1598.col_4 AS c_1600,
    ((t_ref_1598.col_2 IN (SELECT sub_1601.col_1 FROM tbl_12 AS sub_1601)) + (NOT ((UPPER(ABS(((SELECT (((SELECT (SELECT inn_1607.col_3 FROM tbl_26 AS inn_1607 LIMIT 1) FROM tbl_21 AS sub_1606 LIMIT 1)::INT) AND ((-1 IN (SELECT sub_1608.id FROM tbl_23 AS sub_1608))::NUMERIC(10,2))) FROM tbl_19 AS sub_1605 LIMIT 1) + LOWER(t_ref_1598.col_1))))::TEXT::VARCHAR(32))))) AS c_1611
FROM tbl_07 AS t_ref_1598 LEFT JOIN tbl_18 AS j_ref_1612 ON t_ref_1598.id = j_ref_1612.id
WHERE NULLIF(((t_ref_1598.col_4 < (NULL + (FALSE IN (SELECT sub_1613.col_1 FROM tbl_08 AS sub_1613)))) + ((LOWER((LOWER((((((((t_ref_1598.col_4 * (SELECT (((sub_1614.col_2 = sub_1614.col_1)::NUMERIC(10,2)) OR sub_1614.col_2) FROM tbl_16 AS sub_1614 LIMIT 1))::INT) / NULLIF(t_ref_1598.col_4, 0)) > ((1 OR t_ref_1598.col_3) + (CASE WHEN t_ref_1598.id THEN ('alpha_beta_gamma' IN (SELECT sub_1615.id FROM tbl_17 AS sub_1615)) ELSE t_ref_1598.col_4 END))) > t_ref_1598.id)::NUMERIC(10,2)) / NULLIF(t_ref_1598.id, 0))) >= (SELECT (FALSE <> (SELECT (SELECT inn_1619.col_3 FROM tbl_04 AS inn_1619 LIMIT 1) FROM tbl_22 AS sub_1618 LIMIT 1)) FROM tbl_14 AS sub_1617 LIMIT 1)))::NUMERIC(10,2)) + (SELECT (0 / NULLIF((NOT (sub_1620.col_1)), 0)) FROM tbl_20 AS sub_1620 LIMIT 1))), (SELECT (SELECT inn_1639.ref_tbl_10_id FROM tbl_18 AS inn_1639 LIMIT 1) FROM tbl_11 AS sub_1621 LIMIT 1))
LIMIT 41;

CREATE TABLE tbl_27 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    col_3 JSONB,
    col_4 INT,
    ref_tbl_23_id INT,
    FOREIGN KEY (ref_tbl_23_id) REFERENCES tbl_23(id)
);

CREATE TABLE tbl_28 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 NUMERIC(10,2),
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (86815, 'escaped''quote', 1.0, 2147483647, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'escaped''quote');

CREATE TABLE tbl_29 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN
);

INSERT INTO tbl_23 (id, col_1, col_2) VALUES (754169, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 3.14159265);

SELECT
    (LOWER(TRUE) >= t_ref_1640.col_1) AS c_1641,
    (-2147483648 + t_ref_1640.col_1) AS c_1642
FROM tbl_01 AS t_ref_1640
WHERE (3.14159265 * LENGTH(t_ref_1640.col_3))
LIMIT 47;

CREATE TABLE tbl_30 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSONB,
    col_3 VARCHAR(64),
    ref_tbl_10_id INT,
    FOREIGN KEY (ref_tbl_10_id) REFERENCES tbl_10(id)
);

SELECT
    (NOT (((((EXISTS (SELECT 1 FROM tbl_07 AS sub_1644 WHERE TRUE) <= 0.0) > t_ref_1643.id) = (NULL IN (SELECT sub_1647.id FROM tbl_07 AS sub_1647))) / NULLIF((((t_ref_1643.col_2 BETWEEN 1 AND -1) > FALSE) - (EXISTS (SELECT 1 FROM tbl_06 AS sub_1651 WHERE sub_1651.col_2) > ((EXISTS (SELECT 1 FROM tbl_02 AS sub_1675 WHERE sub_1675.col_1) = 100)::TEXT))), 0)))) AS c_1677
FROM tbl_23 AS t_ref_1643
WHERE ((t_ref_1643.id IN (SELECT sub_1678.id FROM tbl_21 AS sub_1678)) + (NULLIF(EXISTS (SELECT 1 FROM tbl_01 AS sub_1698 WHERE 42), COALESCE(ABS(t_ref_1643.col_1), LENGTH((t_ref_1643.col_2 IN (SELECT sub_1712.id FROM tbl_01 AS sub_1712))))) * EXISTS (SELECT 1 FROM tbl_04 AS sub_1713 WHERE sub_1713.col_2)))
LIMIT 13;

BEGIN;
INSERT INTO tbl_20 (id, col_1, col_2, col_3, col_4, ref_tbl_14_id) VALUES (311132, 100, -1.5, 1e10, 0.0000000001, -2147483648);
INSERT INTO tbl_13 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (41485, 3.14159265, '', 2147483647, 9223372036854775807, -2147483648);
COMMIT;

DELETE FROM tbl_23 WHERE id = 4930;

SELECT
    t_ref_1714.id AS c_1715,
    ((ABS((CASE WHEN ((NOT ((SELECT sub_1716.col_3 FROM tbl_26 AS sub_1716 LIMIT 1))) - ((CASE WHEN ((((t_ref_1714.col_1 IN (SELECT sub_1717.ref_tbl_10_id FROM tbl_30 AS sub_1717))::TEXT::VARCHAR(32)) BETWEEN 42 AND 9223372036854775807) = (((t_ref_1714.id IN (SELECT sub_1718.col_3 FROM tbl_19 AS sub_1718)) <> LOWER(t_ref_1714.col_1)) * (COALESCE(ABS(((t_ref_1714.col_2 + (CASE WHEN ((EXISTS (SELECT 1 FROM tbl_18 AS sub_1720 WHERE sub_1720.col_1) - t_ref_1714.col_1) + NULL) THEN (((UPPER(-2147483648) * (CASE WHEN t_ref_1714.col_1 THEN 0.0 ELSE -1 END)) - (-1.5 + (t_ref_1714.id * t_ref_1714.col_2))) > (('chaos' OR (t_ref_1714.col_1 IN (SELECT sub_1721.col_2 FROM tbl_23 AS sub_1721))) - (CASE WHEN (TRUE - t_ref_1714.col_1) THEN (FALSE + t_ref_1714.col_2) ELSE (t_ref_1714.col_1 / NULLIF('chaos', 0)) END))) ELSE COALESCE((((CASE WHEN t_ref_1714.col_2 THEN t_ref_1714.id ELSE t_ref_1714.col_1 END)::TEXT::VARCHAR(32)) < t_ref_1714.id), (SELECT (SELECT inn_1723.id FROM tbl_06 AS inn_1723 LIMIT 1) FROM tbl_02 AS sub_1722 LIMIT 1)) END))::NUMERIC(10,2))), (t_ref_1714.col_1 >= ((LOWER(t_ref_1714.id) * (TRUE <= ((((t_ref_1714.col_1::INT::TEXT) < (t_ref_1714.col_2 = t_ref_1714.id)) AND ((t_ref_1714.id / NULLIF(t_ref_1714.col_2, 0)) > (t_ref_1714.id - t_ref_1714.col_2)))::TEXT))) + FALSE)))::TEXT))) THEN UPPER((SELECT EXISTS (SELECT 1 FROM tbl_09 AS sub_1725 WHERE 3.14159265) FROM tbl_29 AS sub_1724 LIMIT 1)) ELSE COALESCE((t_ref_1714.col_1 IN (SELECT sub_1728.col_2 FROM tbl_25 AS sub_1728)), t_ref_1714.col_1) END)::NUMERIC(10,2))) THEN t_ref_1714.col_2 ELSE (t_ref_1714.col_1 IN (SELECT sub_1729.col_1 FROM tbl_30 AS sub_1729)) END))::INT::TEXT) - t_ref_1714.col_1) AS c_1756,
    ('__fuzz_marker__' - LENGTH((LENGTH((((SELECT LENGTH(TRUE) FROM tbl_11 AS sub_1757 LIMIT 1) / NULLIF(NULL, 0)) <> (CASE WHEN (((((t_ref_1714.id IN (SELECT sub_1758.col_1 FROM tbl_25 AS sub_1758)) * t_ref_1714.col_1) BETWEEN 0 AND -9223372036854775808) AND ((UPPER((((LOWER((-2147483648 * FALSE))::TEXT) - ((((CASE WHEN EXISTS (SELECT 1 FROM tbl_17 AS sub_1766 WHERE sub_1766.id) THEN t_ref_1714.col_1 ELSE (NULL / NULLIF((t_ref_1714.col_2 <= 1), 0)) END) + ((t_ref_1714.col_1 IS NOT NULL) - (NULLIF(TRUE, t_ref_1714.col_1) OR (t_ref_1714.col_1::BOOLEAN)))) >= EXISTS (SELECT 1 FROM tbl_17 AS sub_1767 WHERE FALSE)) - ((t_ref_1714.col_1 IN (SELECT sub_1769.col_3 FROM tbl_15 AS sub_1769)) OR (t_ref_1714.col_2 - t_ref_1714.id)))) OR t_ref_1714.col_2)) < NULL) + ((t_ref_1714.col_1 IN (SELECT sub_1770.id FROM tbl_18 AS sub_1770)) >= LENGTH((NOT (COALESCE(((TRUE < COALESCE((((t_ref_1714.id - t_ref_1714.col_1)::INT) - ((t_ref_1714.col_2 * t_ref_1714.col_2) * COALESCE(t_ref_1714.col_2, 1))), ((EXISTS (SELECT 1 FROM tbl_05 AS sub_1778 WHERE sub_1778.dyn_col_5) / NULLIF((t_ref_1714.col_2 IN (SELECT sub_1779.col_2 FROM tbl_19 AS sub_1779)), 0)) < NULLIF((SELECT TRUE FROM tbl_04 AS sub_1780 LIMIT 1), (t_ref_1714.col_2 AND NULL)))))::NUMERIC(10,2)), 1))))))) <= (EXISTS (SELECT 1 FROM tbl_22 AS sub_1781 WHERE sub_1781.col_1) IS NULL)) THEN ABS((((NOT (NULL)) / NULLIF(t_ref_1714.col_1, 0))::TEXT::VARCHAR(32))) ELSE (t_ref_1714.col_2::NUMERIC(10,2)) END))) >= (NOT (t_ref_1714.col_1))))) AS c_1784
FROM tbl_02 AS t_ref_1714
WHERE (SELECT (((SELECT (SELECT inn_1787.col_2 FROM tbl_13 AS inn_1787 LIMIT 1) FROM tbl_05 AS sub_1786 LIMIT 1)::NUMERIC(10,2)) / NULLIF(ABS((NULL / NULLIF(((((sub_1785.col_3 * (NOT (ABS((CASE WHEN sub_1785.col_1 THEN ((CASE WHEN (((FALSE::TEXT)::TEXT::VARCHAR(32)) - ((sub_1785.col_4 IN (SELECT sub_1788.id FROM tbl_21 AS sub_1788))::TEXT)) THEN ((sub_1785.id >= (CASE WHEN LENGTH(NULLIF(sub_1785.col_4, 0)) THEN (SELECT (SELECT FALSE FROM tbl_24 AS inn_1790 LIMIT 1) FROM tbl_23 AS sub_1789 LIMIT 1) ELSE LOWER((sub_1785.col_2 = sub_1785.id)) END)) AND ((SELECT (-2147483648 OR sub_1791.id) FROM tbl_22 AS sub_1791 LIMIT 1) + 100)) ELSE sub_1785.col_4 END) / NULLIF((((CASE WHEN ((sub_1785.col_2::BOOLEAN)::NUMERIC(10,2)) THEN TRUE ELSE (((-1e-5 + sub_1785.col_1) + sub_1785.col_2) + (SELECT (SELECT NULL FROM tbl_06 AS inn_1793 LIMIT 1) FROM tbl_06 AS sub_1792 LIMIT 1)) END) - (sub_1785.id > ((NOT ((sub_1785.col_3 + NULL))) + 0.0)))::INT), 0)) ELSE (COALESCE((SELECT sub_1794.col_2 FROM tbl_25 AS sub_1794 LIMIT 1), (NOT ((UPPER((sub_1785.col_4::NUMERIC(10,2))) < (COALESCE(UPPER(sub_1785.col_1), (sub_1785.col_3 / NULLIF(FALSE, 0))) OR (SELECT (SELECT inn_1796.id FROM tbl_14 AS inn_1796 LIMIT 1) FROM tbl_11 AS sub_1795 LIMIT 1)))))) BETWEEN 1 AND 42) END)))))::TEXT::VARCHAR(32))::NUMERIC(10,2)) * 'alpha_beta_gamma'), 0))), 0)) FROM tbl_19 AS sub_1785 LIMIT 1)
LIMIT 6;

UPDATE tbl_15 SET col_2 = 2147483647 WHERE id > 67;

UPDATE tbl_02 SET col_2 = 'ref_val' WHERE id > 9;

CREATE TABLE tbl_31 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 BOOLEAN,
    col_3 BOOLEAN,
    col_4 INT
);

UPDATE tbl_15 SET col_2 = 0 WHERE id > 88;

SELECT
    t_ref_1797.col_1 AS c_1798,
    (SELECT (SELECT inn_1800.dyn_col_5 FROM tbl_05 AS inn_1800 LIMIT 1) FROM tbl_05 AS sub_1799 LIMIT 1) AS c_1801,
    (LOWER((1e10 IN (SELECT sub_1802.id FROM tbl_03 AS sub_1802))) <> t_ref_1797.col_2) AS c_1803
FROM tbl_03 AS t_ref_1797 LEFT JOIN tbl_19 AS j_ref_1804 ON t_ref_1797.id = j_ref_1804.id
WHERE t_ref_1797.dyn_col_5
LIMIT 15;

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (574665, 0, FALSE, TRUE, '__fuzz_marker__', 'alpha_beta_gamma');
COMMIT;

SELECT
    (SELECT (SELECT inn_1842.col_2 FROM tbl_29 AS inn_1842 LIMIT 1) FROM tbl_24 AS sub_1806 LIMIT 1) AS c_1843
FROM tbl_04 AS t_ref_1805
WHERE (t_ref_1805.col_3 = ((t_ref_1805.col_1 IN (SELECT sub_1844.col_2 FROM tbl_16 AS sub_1844)) IS NOT NULL))
LIMIT 45;

INSERT INTO tbl_20 (id, col_1, col_2, col_3, col_4, ref_tbl_14_id) VALUES (78135, 0, -1e-5, -1e-5, 1.0, -9223372036854775808);

BEGIN;
INSERT INTO tbl_26 (id, col_1, col_2, col_3, col_4) VALUES (182941, -9223372036854775808, 'ref_val', 3.14159265, TRUE);
INSERT INTO tbl_17 (id, col_1, col_2, ref_tbl_11_id) VALUES (114024, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 1.0, 2147483647);
COMMIT;

SELECT
    ((SELECT (SELECT TRUE FROM tbl_05 AS inn_1863 LIMIT 1) FROM tbl_31 AS sub_1846 LIMIT 1)::TEXT::VARCHAR(32)) AS c_1864,
    t_ref_1845.col_2 AS c_1865,
    t_ref_1845.id AS c_1866
FROM tbl_09 AS t_ref_1845 LEFT JOIN tbl_26 AS j_ref_1867 ON t_ref_1845.id = j_ref_1867.id
WHERE (t_ref_1845.col_1 IN (SELECT sub_1868.payload FROM alpha_tbl AS sub_1868))
LIMIT 13;

CREATE VIEW v_summary_12 AS SELECT id FROM tbl_31 WHERE id IS NOT NULL;

UPDATE tbl_21 SET col_3 = FALSE WHERE id > 52;

INSERT INTO tbl_26 (id, col_1, col_2, col_3, col_4) VALUES (925157, 0, 'chaos', 0.0000000001, TRUE);
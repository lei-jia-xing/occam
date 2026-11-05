-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: MYSQL

-- Statements: 100

-- AST Expression Depth: 16

-- Injected Defect Case: mysql_05

-- Seed: 10023



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 BOOLEAN,
    col_3 BOOLEAN,
    col_4 NUMERIC(10,2)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSON,
    col_3 INT,
    col_4 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    col_3 BOOLEAN,
    col_4 JSON
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN
);

CREATE TABLE warehouse_inventory (
    item_id INT AUTO_INCREMENT PRIMARY KEY,
    sku_code VARCHAR(32) NOT NULL,
    warehouse_id INT NOT NULL,
    bin_location VARCHAR(32) NOT NULL,
    stock_qty INT DEFAULT 0,
    CONSTRAINT uk_sku_wh UNIQUE (sku_code, warehouse_id),
    CONSTRAINT uk_bin UNIQUE (warehouse_id, bin_location)
) ENGINE=InnoDB;

SELECT
    (LENGTH(t_ref_1.id) * LENGTH(t_ref_1.col_1)) AS c_2
FROM tbl_03 AS t_ref_1
WHERE (CASE WHEN NULLIF(LOWER(FALSE), CAST((SELECT CAST(((sub_3.col_1 IN (SELECT sub_4.payload FROM warehouse_inventory AS sub_4)) / NULLIF((CAST(COALESCE((NOT (ABS(LOWER((EXISTS (SELECT 1 FROM tbl_02 AS sub_9 WHERE sub_9.col_4) - sub_3.id))))), ((CAST((COALESCE(1, sub_3.col_1) OR ('ref_val' * sub_3.col_2)) AS DECIMAL(10,2)) < sub_3.col_2) + (sub_3.col_1 / NULLIF(sub_3.id, 0)))) AS DECIMAL(10,2)) - (CAST(NULLIF((CASE WHEN (CASE WHEN sub_3.col_2 THEN (LENGTH(sub_3.col_1) * (sub_3.col_2 * sub_3.id)) ELSE ((SELECT sub_10.id FROM tbl_04 AS sub_10 LIMIT 1) / NULLIF(LENGTH(0.0), 0)) END) THEN (sub_3.col_2 IN (SELECT sub_11.col_1 FROM tbl_04 AS sub_11)) ELSE (SELECT LOWER(0) FROM tbl_05 AS sub_12 LIMIT 1) END), (sub_3.col_1 < (ABS((-1e-5 + FALSE)) IS NOT NULL))) AS DECIMAL(10,2)) * ABS((SELECT 0 FROM tbl_05 AS sub_13 LIMIT 1)))), 0)) AS DECIMAL(10,2)) FROM tbl_04 AS sub_3 LIMIT 1) AS SIGNED)) THEN COALESCE(NULL, (CASE WHEN (CAST((SELECT (SELECT inn_19.col_3 FROM tbl_01 AS inn_19 LIMIT 1) FROM tbl_02 AS sub_14 LIMIT 1) AS SIGNED) BETWEEN 1 AND -9223372036854775808) THEN 1e10 ELSE ((t_ref_1.col_4 IN (SELECT sub_20.col_2 FROM tbl_04 AS sub_20)) / NULLIF(CAST(((999999999.99 <= (TRUE + (t_ref_1.col_4 AND (t_ref_1.col_2 + LOWER((t_ref_1.id / NULLIF(CAST((SELECT sub_21.col_1 FROM tbl_02 AS sub_21 LIMIT 1) AS SIGNED), 0))))))) / NULLIF((SELECT (SELECT inn_23.col_3 FROM tbl_01 AS inn_23 LIMIT 1) FROM tbl_05 AS sub_22 LIMIT 1), 0)) AS DECIMAL(10,2)), 0)) END)) ELSE (((((t_ref_1.col_3 + ((LOWER(t_ref_1.col_1) * (((UPPER(UPPER((t_ref_1.col_4 + (t_ref_1.col_2 - t_ref_1.col_4)))) = (t_ref_1.col_2 + ABS(CAST(EXISTS (SELECT 1 FROM tbl_04 AS sub_24 WHERE TRUE) AS CHAR)))) <= ((t_ref_1.col_3 IN (SELECT sub_25.col_2 FROM tbl_05 AS sub_25)) = ((((t_ref_1.col_4 - t_ref_1.col_4) * t_ref_1.col_1) IS NULL) <> LENGTH(NULLIF(ABS(t_ref_1.col_2), (t_ref_1.col_2 > NULL)))))) OR (CASE WHEN (t_ref_1.id IS NOT NULL) THEN CAST(FALSE AS CHAR) ELSE 1 END))) BETWEEN 2147483647 AND -1)) <> (NULLIF((0.0 = (SELECT (SELECT inn_28.id FROM tbl_04 AS inn_28 LIMIT 1) FROM tbl_04 AS sub_27 LIMIT 1)), (SELECT (SELECT FALSE FROM tbl_05 AS inn_36 LIMIT 1) FROM tbl_04 AS sub_29 LIMIT 1)) <= LOWER(LENGTH(LENGTH(t_ref_1.col_2))))) - (CAST(((CASE WHEN (SELECT (NULLIF((EXISTS (SELECT 1 FROM tbl_02 AS sub_38 WHERE 1) / NULLIF((CASE WHEN (CASE WHEN sub_37.col_4 THEN sub_37.col_3 ELSE sub_37.col_3 END) THEN (sub_37.col_3 * FALSE) ELSE (NOT (sub_37.col_2)) END), 0)), CAST(CAST((SELECT FALSE FROM warehouse_inventory AS sub_39 LIMIT 1) AS SIGNED) AS DECIMAL(10,2))) - (LOWER(3.14159265) BETWEEN 42 AND -2147483648)) FROM tbl_01 AS sub_37 LIMIT 1) THEN CAST(t_ref_1.id AS SIGNED) ELSE (t_ref_1.id IN (SELECT sub_40.id FROM tbl_04 AS sub_40)) END) + NULLIF(((t_ref_1.id IN (SELECT sub_41.col_1 FROM tbl_05 AS sub_41)) >= (t_ref_1.col_1 <> ABS(NULLIF(((SELECT sub_43.id FROM warehouse_inventory AS sub_43 LIMIT 1) <= ABS((FALSE * -9223372036854775808))), CAST((SELECT (SELECT inn_45.payload FROM warehouse_inventory AS inn_45 LIMIT 1) FROM tbl_04 AS sub_44 LIMIT 1) AS DECIMAL(10,2)))))), CAST(FALSE AS SIGNED))) AS DECIMAL(10,2)) > (NULL OR (TRUE IN (SELECT sub_46.col_2 FROM tbl_04 AS sub_46))))) + (CAST(1 AS CHAR) * ((CASE WHEN LOWER(((t_ref_1.col_3 OR CAST(UPPER((NOT ((CASE WHEN 0 THEN (CASE WHEN t_ref_1.col_4 THEN t_ref_1.col_4 ELSE CAST(t_ref_1.col_2 AS CHAR) END) ELSE (t_ref_1.col_3 - (SELECT sub_49.col_3 FROM tbl_03 AS sub_49 LIMIT 1)) END)))) AS CHAR)) IS NULL)) THEN (NULL IN (SELECT sub_50.col_2 FROM tbl_04 AS sub_50)) ELSE ABS(CAST(NULLIF(t_ref_1.col_1, (1e10 IN (SELECT sub_51.col_2 FROM tbl_03 AS sub_51))) AS DECIMAL(10,2))) END) / NULLIF((NULL AND (((CAST(FALSE AS DECIMAL(10,2)) IS NOT NULL) - (-2147483648 + NULLIF(t_ref_1.id, CAST(((((t_ref_1.col_3 + TRUE) IS NULL) * t_ref_1.col_1) - ((CASE WHEN (NOT (t_ref_1.col_3)) THEN (t_ref_1.col_1 + TRUE) ELSE (SELECT sub_53.col_4 FROM tbl_03 AS sub_53 LIMIT 1) END) * ((TRUE < t_ref_1.col_2) * (NOT (t_ref_1.col_4))))) AS SIGNED)))) / NULLIF((SELECT CAST(UPPER((LENGTH((SELECT sub_55.id FROM warehouse_inventory AS sub_55 LIMIT 1)) = ((sub_54.col_2 > FALSE) * (SELECT (SELECT inn_57.col_2 FROM tbl_02 AS inn_57 LIMIT 1) FROM tbl_03 AS sub_56 LIMIT 1)))) AS SIGNED) FROM tbl_01 AS sub_54 LIMIT 1), 0))), 0)))) - t_ref_1.col_1) END)
LIMIT 2;

SELECT
    (CASE WHEN COALESCE((SELECT sub_59.col_1 FROM tbl_04 AS sub_59 LIMIT 1), EXISTS (SELECT 1 FROM tbl_04 AS sub_60 WHERE 'alpha_beta_gamma')) THEN (ABS(t_ref_58.col_2) <> ((CASE WHEN (CAST(('alpha_beta_gamma' IN (SELECT sub_71.col_2 FROM tbl_05 AS sub_71)) AS DECIMAL(10,2)) / NULLIF(CAST((UPPER((t_ref_58.col_1 = ((NULLIF(CAST(NULLIF(-1.5, NULL) AS SIGNED), EXISTS (SELECT 1 FROM tbl_05 AS sub_73 WHERE 1)) + 0.0) < ((EXISTS (SELECT 1 FROM tbl_03 AS sub_74 WHERE sub_74.id) AND (t_ref_58.col_1 + t_ref_58.id)) >= (SELECT (SELECT TRUE FROM tbl_05 AS inn_76 LIMIT 1) FROM tbl_04 AS sub_75 LIMIT 1))))) <> (SELECT (SELECT inn_80.col_1 FROM tbl_02 AS inn_80 LIMIT 1) FROM tbl_02 AS sub_77 LIMIT 1)) AS SIGNED), 0)) THEN 'ref_val' ELSE CAST(CAST(t_ref_58.col_1 AS CHAR) AS CHAR) END) OR CAST('__fuzz_marker__' AS SIGNED))) ELSE ('__fuzz_marker__' IS NOT NULL) END) AS c_81
FROM tbl_05 AS t_ref_58
WHERE UPPER(TRUE)
LIMIT 11;

UPDATE tbl_05 SET col_1 = 1e10 WHERE id > 95;

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (267269, -1e-5, TRUE);
INSERT INTO tbl_04 (id, col_1, col_2) VALUES (824723, '__fuzz_marker__', 9223372036854775807);
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (405049, 'escaped''quote', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -1, 'alpha_beta_gamma', 2147483647);
COMMIT;

DELETE FROM warehouse_inventory WHERE id = 2871;

INSERT INTO warehouse_inventory (id, payload) VALUES (939957, 'chaos');

SELECT
    ((CASE WHEN t_ref_82.ref_tbl_01_id THEN (SELECT (SELECT inn_84.id FROM tbl_04 AS inn_84 LIMIT 1) FROM tbl_04 AS sub_83 LIMIT 1) ELSE (SELECT (SELECT NULL FROM tbl_05 AS inn_86 LIMIT 1) FROM tbl_05 AS sub_85 LIMIT 1) END) / NULLIF(ABS(COALESCE((t_ref_82.id AND (UPPER((t_ref_82.col_1 <= t_ref_82.col_1)) + t_ref_82.col_3)), CAST(t_ref_82.col_4 AS CHAR))), 0)) AS c_87
FROM tbl_02 AS t_ref_82 LEFT JOIN tbl_04 AS j_ref_88 ON t_ref_82.id = j_ref_88.id
WHERE ((CAST(((SELECT (SELECT inn_90.col_3 FROM tbl_01 AS inn_90 LIMIT 1) FROM tbl_02 AS sub_89 LIMIT 1) / NULLIF(t_ref_82.col_4, 0)) AS DECIMAL(10,2)) AND (CASE WHEN t_ref_82.col_3 THEN ((t_ref_82.col_3 > (CASE WHEN EXISTS (SELECT 1 FROM tbl_04 AS sub_91 WHERE sub_91.col_1) THEN (CASE WHEN (CASE WHEN NULL THEN t_ref_82.col_4 ELSE CAST(((CAST(-2147483648 AS DECIMAL(10,2)) = COALESCE((NOT (((t_ref_82.col_2 <= t_ref_82.col_2) - (t_ref_82.col_2 <= t_ref_82.col_4)))), COALESCE((LENGTH(t_ref_82.id) * UPPER(FALSE)), ((t_ref_82.col_3 OR TRUE) OR t_ref_82.col_1)))) <= (FALSE + t_ref_82.id)) AS DECIMAL(10,2)) END) THEN t_ref_82.ref_tbl_01_id ELSE t_ref_82.id END) ELSE t_ref_82.col_3 END)) - (CAST(t_ref_82.col_3 AS DECIMAL(10,2)) - t_ref_82.id)) ELSE (NOT ((CAST(EXISTS (SELECT 1 FROM warehouse_inventory AS sub_95 WHERE 'chaos') AS DECIMAL(10,2)) * CAST(NULL AS DECIMAL(10,2))))) END)) - t_ref_82.col_2)
LIMIT 29;

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (812772, 'ref_val', 0, TRUE, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (875996, '', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 100, '__fuzz_marker__', 0);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (905037, 'chaos', 1, FALSE, '{}');
COMMIT;

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSON,
    col_3 INT,
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

CREATE INDEX idx_tbl_03_id_1 ON tbl_03 (id);

INSERT INTO warehouse_inventory (id, payload) VALUES (809460, 'chaos');

CREATE VIEW v_summary_1 AS SELECT id FROM warehouse_inventory WHERE id IS NOT NULL;

UPDATE tbl_01 SET col_4 = 0.0 WHERE id > 3;

SELECT
    ((t_ref_96.col_1 IN (SELECT sub_97.col_1 FROM tbl_05 AS sub_97)) <= CAST(('ref_val' <> (SELECT (SELECT inn_99.col_2 FROM tbl_02 AS inn_99 LIMIT 1) FROM tbl_06 AS sub_98 LIMIT 1)) AS CHAR)) AS c_100,
    (CASE WHEN UPPER((NOT ('__fuzz_marker__'))) THEN CAST((COALESCE(((SELECT (sub_101.id * UPPER((SELECT (SELECT NULL FROM tbl_04 AS inn_105 LIMIT 1) FROM tbl_03 AS sub_102 LIMIT 1))) FROM tbl_03 AS sub_101 LIMIT 1) / NULLIF((TRUE * t_ref_96.ref_tbl_01_id), 0)), (((CAST(CAST(((CAST(ABS(LENGTH((t_ref_96.ref_tbl_01_id = -1e-5))) AS SIGNED) BETWEEN -9223372036854775808 AND 100) / NULLIF(t_ref_96.ref_tbl_01_id, 0)) AS CHAR) AS DECIMAL(10,2)) + CAST(t_ref_96.col_3 AS SIGNED)) * t_ref_96.id) <= (SELECT (SELECT inn_107.id FROM warehouse_inventory AS inn_107 LIMIT 1) FROM tbl_04 AS sub_106 LIMIT 1))) = (SELECT (SELECT 1.0 FROM tbl_06 AS inn_109 LIMIT 1) FROM tbl_05 AS sub_108 LIMIT 1)) AS SIGNED) ELSE (SELECT (SELECT inn_113.col_2 FROM tbl_03 AS inn_113 LIMIT 1) FROM warehouse_inventory AS sub_110 LIMIT 1) END) AS c_114
FROM tbl_02 AS t_ref_96
WHERE (100 - t_ref_96.col_2)
LIMIT 33;

INSERT INTO warehouse_inventory (sku_code, warehouse_id, bin_location, stock_qty) VALUES
    ('SKU-A', 1, 'BIN-101', 50),
    ('SKU-B', 1, 'BIN-102', 30),
    ('SKU-C', 2, 'BIN-201', 100);

ALTER TABLE tbl_05 ADD COLUMN dyn_col_4 VARCHAR(32);

INSERT INTO tbl_05 (id, col_1, col_2, dyn_col_4) VALUES (842359, -1e-5, FALSE, 'escaped''quote');

SELECT
    COALESCE(t_ref_115.id, (CAST(LENGTH(((CAST((UPPER(LENGTH((t_ref_115.id * (SELECT ((CASE WHEN TRUE THEN sub_116.col_4 ELSE '' END) <= NULLIF(sub_116.col_3, sub_116.col_1)) FROM tbl_03 AS sub_116 LIMIT 1)))) + t_ref_115.id) AS DECIMAL(10,2)) * ('' >= (SELECT (SELECT inn_119.payload FROM warehouse_inventory AS inn_119 LIMIT 1) FROM tbl_03 AS sub_117 LIMIT 1))) IS NULL)) AS SIGNED) IS NULL)) AS c_120,
    (CAST(t_ref_115.payload AS CHAR) * t_ref_115.id) AS c_121,
    ((CAST((t_ref_115.payload / NULLIF(t_ref_115.id, 0)) AS DECIMAL(10,2)) - (NULL BETWEEN 1 AND 2147483647)) OR ((((FALSE IN (SELECT sub_122.col_3 FROM tbl_01 AS sub_122)) <= TRUE) / NULLIF(t_ref_115.payload, 0)) = 'escaped''quote')) AS c_123
FROM warehouse_inventory AS t_ref_115
WHERE (TRUE IN (SELECT sub_124.ref_tbl_03_id FROM tbl_06 AS sub_124))
LIMIT 20;

UPDATE tbl_02 SET col_4 = 'ref_val' WHERE id > 31;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

DELETE FROM warehouse_inventory WHERE id = 61667;

SELECT
    t_ref_126.col_3 AS c_127,
    EXISTS (SELECT 1 FROM tbl_05 AS sub_128 WHERE sub_128.id) AS c_140,
    LENGTH(t_ref_126.col_2) AS c_141
FROM tbl_06 AS t_ref_126
WHERE ((((CAST((NULLIF(LOWER((LOWER(CAST(((UPPER(LENGTH(t_ref_126.id)) / NULLIF(COALESCE(LENGTH(t_ref_126.col_1), NULLIF(t_ref_126.col_2, 3.14159265)), 0)) - (NOT (t_ref_126.ref_tbl_03_id))) AS CHAR)) BETWEEN 9223372036854775807 AND -9223372036854775808)), t_ref_126.col_2) AND (UPPER(((NULLIF((SELECT CAST(COALESCE(sub_142.col_1, 9223372036854775807) AS SIGNED) FROM tbl_04 AS sub_142 LIMIT 1), NULLIF((LENGTH((t_ref_126.ref_tbl_03_id < t_ref_126.id)) - ((t_ref_126.ref_tbl_03_id > t_ref_126.col_1) = t_ref_126.col_2)), (t_ref_126.col_1 + EXISTS (SELECT 1 FROM tbl_07 AS sub_143 WHERE TRUE)))) - (CASE WHEN t_ref_126.ref_tbl_03_id THEN ((TRUE - (SELECT sub_144.col_3 FROM tbl_03 AS sub_144 LIMIT 1)) = (SELECT (SELECT inn_146.id FROM warehouse_inventory AS inn_146 LIMIT 1) FROM tbl_02 AS sub_145 LIMIT 1)) ELSE (NULL AND (((-9223372036854775808 * t_ref_126.id) + (t_ref_126.col_2 OR TRUE)) IS NOT NULL)) END)) - (SELECT (SELECT inn_149.col_4 FROM tbl_01 AS inn_149 LIMIT 1) FROM tbl_03 AS sub_147 LIMIT 1))) OR t_ref_126.ref_tbl_03_id)) AS CHAR) * (NULLIF(CAST((('escaped''quote' IN (SELECT sub_150.id FROM warehouse_inventory AS sub_150)) IS NULL) AS SIGNED), EXISTS (SELECT 1 FROM tbl_01 AS sub_156 WHERE sub_156.col_3)) = EXISTS (SELECT 1 FROM tbl_04 AS sub_159 WHERE NULL))) = (t_ref_126.col_3 > CAST(t_ref_126.col_2 AS DECIMAL(10,2)))) / NULLIF((SELECT ((CAST(sub_164.id AS SIGNED) > ABS('__fuzz_marker__')) >= ((EXISTS (SELECT 1 FROM tbl_03 AS sub_165 WHERE TRUE) OR (SELECT (SELECT inn_173.ref_tbl_01_id FROM tbl_07 AS inn_173 LIMIT 1) FROM warehouse_inventory AS sub_167 LIMIT 1)) + (CASE WHEN LOWER((NOT ((CASE WHEN CAST(sub_164.ref_tbl_01_id AS DECIMAL(10,2)) THEN TRUE ELSE NULL END)))) THEN ((CASE WHEN (ABS((sub_164.ref_tbl_01_id AND UPPER((ABS(sub_164.ref_tbl_01_id) < (sub_164.col_2 * 'alpha_beta_gamma'))))) AND (NOT (COALESCE((FALSE = (SELECT (SELECT 1.0 FROM tbl_02 AS inn_175 LIMIT 1) FROM tbl_04 AS sub_174 LIMIT 1)), sub_164.id)))) THEN (SELECT (SELECT FALSE FROM tbl_05 AS inn_179 LIMIT 1) FROM tbl_01 AS sub_176 LIMIT 1) ELSE (((((SELECT (SELECT -1 FROM tbl_07 AS inn_181 LIMIT 1) FROM tbl_07 AS sub_180 LIMIT 1) OR EXISTS (SELECT 1 FROM tbl_06 AS sub_182 WHERE sub_182.col_1)) + ((42 * (NULL >= sub_164.ref_tbl_01_id)) + ((sub_164.ref_tbl_01_id - 0.0000000001) < (sub_164.ref_tbl_01_id < sub_164.id)))) - sub_164.id) - NULL) END) - sub_164.col_2) ELSE sub_164.col_1 END))) FROM tbl_07 AS sub_164 LIMIT 1), 0)) <= ((t_ref_126.col_1 / NULLIF(((NOT ((CASE WHEN EXISTS (SELECT 1 FROM tbl_06 AS sub_183 WHERE 9223372036854775807) THEN TRUE ELSE t_ref_126.col_2 END))) > (t_ref_126.col_2 / NULLIF(COALESCE(t_ref_126.id, CAST((-9223372036854775808 / NULLIF(t_ref_126.col_2, 0)) AS CHAR)), 0))), 0)) + t_ref_126.ref_tbl_03_id))
LIMIT 23;

ALTER TABLE tbl_01 DROP COLUMN col_3;

INSERT INTO tbl_04 (id, col_1, col_2) VALUES (242952, '', 2147483647);

SELECT
    ((CAST(CAST(t_ref_190.id AS DECIMAL(10,2)) AS SIGNED) AND LENGTH(((LENGTH(t_ref_190.id) * t_ref_190.payload) + (CASE WHEN (COALESCE((CAST(LOWER((SELECT sub_191.col_1 FROM tbl_05 AS sub_191 LIMIT 1)) AS SIGNED) * (((((((SELECT (SELECT NULL FROM tbl_05 AS inn_193 LIMIT 1) FROM warehouse_inventory AS sub_192 LIMIT 1) * CAST(t_ref_190.payload AS SIGNED)) / NULLIF(EXISTS (SELECT 1 FROM tbl_01 AS sub_194 WHERE TRUE), 0)) + ((2147483647 + (t_ref_190.id < t_ref_190.payload)) / NULLIF(CAST(LENGTH(t_ref_190.payload) AS CHAR), 0))) AND (t_ref_190.payload < NULLIF((t_ref_190.id <= UPPER(t_ref_190.id)), NULL))) <> t_ref_190.payload) <= (t_ref_190.payload > t_ref_190.payload))), UPPER(TRUE)) * (SELECT (SELECT TRUE FROM tbl_06 AS inn_196 LIMIT 1) FROM tbl_04 AS sub_195 LIMIT 1)) THEN (t_ref_190.id IN (SELECT sub_197.ref_tbl_01_id FROM tbl_02 AS sub_197)) ELSE (t_ref_190.payload IN (SELECT sub_200.col_2 FROM tbl_02 AS sub_200)) END)))) / NULLIF(((EXISTS (SELECT 1 FROM tbl_04 AS sub_204 WHERE 'alpha_beta_gamma') < TRUE) * UPPER(t_ref_190.id)), 0)) AS c_205,
    (COALESCE(CAST((CAST(t_ref_190.id AS DECIMAL(10,2)) - ((NULLIF(t_ref_190.payload, t_ref_190.payload) < (t_ref_190.id - 0.0)) < LOWER(((SELECT FALSE FROM tbl_04 AS sub_206 LIMIT 1) IS NULL)))) AS CHAR), ((CASE WHEN (t_ref_190.id + ((t_ref_190.payload OR t_ref_190.payload) <= LOWER(((t_ref_190.id IN (SELECT sub_207.id FROM tbl_04 AS sub_207)) / NULLIF(t_ref_190.payload, 0))))) THEN ((SELECT (NULL IN (SELECT sub_211.col_4 FROM tbl_02 AS sub_211)) FROM tbl_06 AS sub_210 LIMIT 1) <> (SELECT (CASE WHEN FALSE THEN (((sub_214.id IS NOT NULL) <= 'alpha_beta_gamma') + (CAST((sub_214.id IS NULL) AS DECIMAL(10,2)) / NULLIF(ABS(sub_214.payload), 0))) ELSE sub_214.payload END) FROM warehouse_inventory AS sub_214 LIMIT 1)) ELSE t_ref_190.payload END) - t_ref_190.payload)) - NULL) AS c_215
FROM warehouse_inventory AS t_ref_190
WHERE (CASE WHEN (CAST((CASE WHEN (TRUE - (CASE WHEN (t_ref_190.payload IN (SELECT sub_216.ref_tbl_03_id FROM tbl_06 AS sub_216)) THEN ((SELECT (SELECT (SELECT inn_224.col_2 FROM tbl_06 AS inn_224 LIMIT 1) FROM tbl_07 AS sub_223 LIMIT 1) FROM tbl_01 AS sub_222 LIMIT 1) + t_ref_190.id) ELSE (((CASE WHEN (SELECT ((sub_225.id / NULLIF(((sub_225.col_1 + sub_225.id) * (CASE WHEN sub_225.col_3 THEN sub_225.col_1 ELSE 'ref_val' END)), 0)) - CAST(((sub_225.col_1 AND sub_225.col_2) - NULL) AS SIGNED)) FROM tbl_06 AS sub_225 LIMIT 1) THEN ((COALESCE((CAST((1 BETWEEN 1 AND -9223372036854775808) AS DECIMAL(10,2)) * LENGTH(CAST(FALSE AS CHAR))), (((CASE WHEN t_ref_190.id THEN t_ref_190.payload ELSE -9223372036854775808 END) * ABS(t_ref_190.id)) BETWEEN 9223372036854775807 AND -9223372036854775808)) / NULLIF((SELECT (SELECT inn_229.col_1 FROM tbl_04 AS inn_229 LIMIT 1) FROM tbl_03 AS sub_226 LIMIT 1), 0)) AND COALESCE(((SELECT (sub_230.col_4 * sub_230.col_3) FROM tbl_02 AS sub_230 LIMIT 1) - LOWER(t_ref_190.payload)), EXISTS (SELECT 1 FROM tbl_02 AS sub_231 WHERE FALSE))) ELSE t_ref_190.payload END) - LENGTH((SELECT sub_232.payload FROM warehouse_inventory AS sub_232 LIMIT 1))) IS NOT NULL) END)) THEN t_ref_190.id ELSE CAST(CAST((((LOWER(((SELECT (CAST(NULL AS SIGNED) + LENGTH(NULL)) FROM tbl_03 AS sub_233 LIMIT 1) * -9223372036854775808)) > (((0 * CAST((COALESCE(t_ref_190.payload, t_ref_190.id) <= (t_ref_190.id * t_ref_190.id)) AS DECIMAL(10,2))) * (('alpha_beta_gamma' OR t_ref_190.payload) + (SELECT EXISTS (SELECT 1 FROM tbl_06 AS sub_235 WHERE sub_235.col_3) FROM tbl_06 AS sub_234 LIMIT 1))) / NULLIF((('' - (SELECT (SELECT (SELECT inn_238.col_2 FROM tbl_04 AS inn_238 LIMIT 1) FROM tbl_02 AS sub_237 LIMIT 1) FROM tbl_03 AS sub_236 LIMIT 1)) + (EXISTS (SELECT 1 FROM tbl_03 AS sub_239 WHERE sub_239.col_3) * (t_ref_190.payload OR ((t_ref_190.id < TRUE) - LENGTH(t_ref_190.id))))), 0))) < (SELECT UPPER(((NULLIF(CAST(sub_240.col_2 AS SIGNED), CAST(sub_240.col_1 AS DECIMAL(10,2))) * sub_240.col_1) + CAST((sub_240.col_4 - (SELECT sub_241.col_2 FROM tbl_01 AS sub_241 LIMIT 1)) AS SIGNED))) FROM tbl_01 AS sub_240 LIMIT 1)) / NULLIF(ABS(EXISTS (SELECT 1 FROM tbl_06 AS sub_242 WHERE sub_242.col_1)), 0)) AS CHAR) AS SIGNED) END) AS DECIMAL(10,2)) + ((CAST((TRUE IN (SELECT sub_245.col_2 FROM tbl_03 AS sub_245)) AS DECIMAL(10,2)) = t_ref_190.id) <= (ABS(NULL) + 1e10))) THEN TRUE ELSE t_ref_190.id END)
LIMIT 25;

CREATE INDEX idx_tbl_07_id_2 ON tbl_07 (id);

CREATE INDEX idx_tbl_05_col_2_3 ON tbl_05 (col_2);

SELECT
    t_ref_246.col_1 AS c_247
FROM tbl_05 AS t_ref_246
WHERE ((CAST((TRUE <> (t_ref_246.id * LENGTH((ABS(ABS(t_ref_246.col_2)) * CAST(t_ref_246.id AS SIGNED))))) AS DECIMAL(10,2)) + CAST((t_ref_246.col_2 IN (SELECT sub_248.id FROM tbl_03 AS sub_248)) AS DECIMAL(10,2))) >= CAST(EXISTS (SELECT 1 FROM tbl_03 AS sub_257 WHERE sub_257.col_1) AS CHAR))
LIMIT 22;

INSERT INTO tbl_01 (id, col_1, col_2, col_4) VALUES (900647, -2147483648, TRUE, 1.0);

INSERT INTO warehouse_inventory (sku_code, warehouse_id, bin_location, stock_qty) VALUES
    ('SKU-A', 1, 'BIN-102', 15),
    ('SKU-B', 1, 'BIN-101', 25)
ON DUPLICATE KEY UPDATE
    stock_qty = warehouse_inventory.stock_qty + VALUES(stock_qty);

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_01_id) VALUES (775483, 'alpha_beta_gamma', FALSE, 9223372036854775807);
INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_01_id) VALUES (293497, '', TRUE, -9223372036854775808);
COMMIT;

CREATE INDEX idx_tbl_07_col_1_4 ON tbl_07 (col_1);

ALTER TABLE tbl_01 ADD COLUMN dyn_col_5 VARCHAR(32);

UPDATE tbl_01 SET col_1 = -1 WHERE id > 68;

UPDATE tbl_04 SET col_2 = 2147483647 WHERE id > 92;

CREATE VIEW v_summary_2 AS SELECT id FROM warehouse_inventory WHERE id IS NOT NULL;

UPDATE warehouse_inventory SET payload = '' WHERE id > 68;

SELECT
    t_ref_269.id AS c_270,
    (((SELECT sub_271.ref_tbl_03_id FROM tbl_06 AS sub_271 LIMIT 1) <> (((t_ref_269.col_2 < CAST((CAST(t_ref_269.col_1 AS CHAR) * (LENGTH(((3.14159265 / NULLIF(ABS(EXISTS (SELECT 1 FROM warehouse_inventory AS sub_272 WHERE NULL)), 0)) OR (CASE WHEN (t_ref_269.col_2 + CAST((FALSE + t_ref_269.id) AS DECIMAL(10,2))) THEN ((SELECT (SELECT (SELECT FALSE FROM tbl_04 AS inn_275 LIMIT 1) FROM tbl_06 AS sub_274 LIMIT 1) FROM warehouse_inventory AS sub_273 LIMIT 1) - (t_ref_269.dyn_col_4 <= (ABS(t_ref_269.id) = (NOT (t_ref_269.id))))) ELSE CAST((LENGTH((SELECT (SELECT inn_277.ref_tbl_03_id FROM tbl_06 AS inn_277 LIMIT 1) FROM tbl_04 AS sub_276 LIMIT 1)) = TRUE) AS DECIMAL(10,2)) END))) < (((t_ref_269.dyn_col_4 + t_ref_269.id) / NULLIF((t_ref_269.col_2 / NULLIF(t_ref_269.col_1, 0)), 0)) >= LOWER(t_ref_269.dyn_col_4)))) AS CHAR)) OR LENGTH((TRUE < 'ref_val'))) BETWEEN 2147483647 AND 2147483647)) >= ('ref_val' BETWEEN 9223372036854775807 AND 9223372036854775807)) AS c_278,
    (2147483647 IN (SELECT sub_279.col_1 FROM tbl_07 AS sub_279)) AS c_280
FROM tbl_05 AS t_ref_269 LEFT JOIN tbl_01 AS j_ref_281 ON t_ref_269.id = j_ref_281.id
WHERE (CAST((CAST((CAST((100 IN (SELECT sub_282.id FROM warehouse_inventory AS sub_282)) AS CHAR) / NULLIF((SELECT CAST(COALESCE(LOWER((sub_287.col_4 * sub_287.col_4)), NULLIF(LOWER(sub_287.col_3), CAST(sub_287.col_2 AS DECIMAL(10,2)))) AS CHAR) FROM tbl_03 AS sub_287 LIMIT 1), 0)) AS DECIMAL(10,2)) * t_ref_269.col_2) AS DECIMAL(10,2)) <= LENGTH((CASE WHEN ((CASE WHEN t_ref_269.id THEN (EXISTS (SELECT 1 FROM tbl_04 AS sub_288 WHERE sub_288.col_1) - t_ref_269.id) ELSE (UPPER((CASE WHEN NULLIF((((CASE WHEN FALSE THEN NULLIF((CASE WHEN (0.0 > t_ref_269.dyn_col_4) THEN 100 ELSE CAST(t_ref_269.col_1 AS SIGNED) END), (FALSE > t_ref_269.col_1)) ELSE COALESCE(FALSE, t_ref_269.col_1) END) / NULLIF(((SELECT EXISTS (SELECT 1 FROM tbl_07 AS sub_290 WHERE sub_290.col_1) FROM tbl_01 AS sub_289 LIMIT 1) >= (SELECT (SELECT NULL FROM tbl_03 AS inn_292 LIMIT 1) FROM tbl_04 AS sub_291 LIMIT 1)), 0)) + t_ref_269.dyn_col_4), t_ref_269.col_2) THEN (9223372036854775807 <= t_ref_269.col_1) ELSE t_ref_269.col_1 END)) IS NOT NULL) END) * (NOT (((t_ref_269.id >= (t_ref_269.dyn_col_4 IN (SELECT sub_293.id FROM tbl_03 AS sub_293))) IS NULL)))) THEN (SELECT (SELECT 1 FROM tbl_01 AS inn_296 LIMIT 1) FROM tbl_06 AS sub_294 LIMIT 1) ELSE ((((-1.5 IN (SELECT sub_297.col_1 FROM tbl_05 AS sub_297)) <= EXISTS (SELECT 1 FROM tbl_05 AS sub_300 WHERE sub_300.id)) OR (SELECT sub_301.dyn_col_4 FROM tbl_05 AS sub_301 LIMIT 1)) * EXISTS (SELECT 1 FROM tbl_04 AS sub_302 WHERE sub_302.col_2)) END)))
LIMIT 28;

INSERT INTO tbl_05 (id, col_1, col_2, dyn_col_4) VALUES (756863, 1.0, TRUE, 'ref_val');

SELECT
    CAST((NULL - CAST(((SELECT ((NULL BETWEEN -2147483648 AND -2147483648) >= (sub_305.col_1 - (sub_305.id IN (SELECT sub_306.col_3 FROM tbl_03 AS sub_306)))) FROM tbl_07 AS sub_305 LIMIT 1) * t_ref_304.id) AS CHAR)) AS DECIMAL(10,2)) AS c_309,
    COALESCE(NULLIF((((t_ref_304.col_2 IN (SELECT sub_310.id FROM tbl_04 AS sub_310)) / NULLIF(LENGTH((COALESCE((SELECT (SELECT inn_312.id FROM tbl_06 AS inn_312 LIMIT 1) FROM tbl_04 AS sub_311 LIMIT 1), t_ref_304.col_1) * (LENGTH(t_ref_304.col_1) / NULLIF(EXISTS (SELECT 1 FROM warehouse_inventory AS sub_313 WHERE sub_313.payload), 0)))), 0)) + t_ref_304.id), (t_ref_304.id * EXISTS (SELECT 1 FROM tbl_03 AS sub_315 WHERE sub_315.col_3))), COALESCE(((COALESCE(EXISTS (SELECT 1 FROM tbl_04 AS sub_328 WHERE sub_328.col_2), CAST('escaped''quote' AS SIGNED)) * (t_ref_304.col_2 AND (TRUE + CAST(CAST((t_ref_304.id / NULLIF((SELECT (SELECT FALSE FROM tbl_06 AS inn_331 LIMIT 1) FROM tbl_06 AS sub_330 LIMIT 1), 0)) AS SIGNED) AS CHAR)))) / NULLIF(CAST(ABS(CAST((CAST(((CAST(t_ref_304.id AS DECIMAL(10,2)) BETWEEN -2147483648 AND 42) <= (t_ref_304.col_1 < (t_ref_304.id IN (SELECT sub_332.col_2 FROM tbl_01 AS sub_332)))) AS CHAR) < (((NOT (UPPER(EXISTS (SELECT 1 FROM tbl_02 AS sub_334 WHERE 1)))) / NULLIF((LOWER(((SELECT (SELECT inn_338.id FROM tbl_04 AS inn_338 LIMIT 1) FROM warehouse_inventory AS sub_336 LIMIT 1) + (TRUE + (NOT ((t_ref_304.id / NULLIF(t_ref_304.col_2, 0))))))) - (CASE WHEN t_ref_304.id THEN (EXISTS (SELECT 1 FROM tbl_02 AS sub_339 WHERE sub_339.col_3) = t_ref_304.col_1) ELSE (t_ref_304.id - t_ref_304.col_2) END)), 0)) / NULLIF((CASE WHEN (SELECT (SELECT (SELECT inn_342.col_2 FROM tbl_07 AS inn_342 LIMIT 1) FROM tbl_05 AS sub_341 LIMIT 1) FROM tbl_01 AS sub_340 LIMIT 1) THEN (t_ref_304.id * ((t_ref_304.id / NULLIF(CAST(((SELECT 1.0 FROM tbl_04 AS sub_343 LIMIT 1) / NULLIF(t_ref_304.id, 0)) AS DECIMAL(10,2)), 0)) BETWEEN 2147483647 AND 42)) ELSE (SELECT (SELECT inn_345.col_3 FROM tbl_02 AS inn_345 LIMIT 1) FROM tbl_01 AS sub_344 LIMIT 1) END), 0))) AS CHAR)) AS SIGNED), 0)), ((COALESCE(COALESCE(CAST((t_ref_304.id < CAST((SELECT (SELECT inn_347.col_1 FROM tbl_04 AS inn_347 LIMIT 1) FROM tbl_05 AS sub_346 LIMIT 1) AS CHAR)) AS CHAR), ABS(TRUE)), ((CAST(((t_ref_304.col_2 IN (SELECT sub_348.col_2 FROM tbl_02 AS sub_348)) < LOWER((NULLIF((SELECT (SELECT inn_353.col_2 FROM tbl_04 AS inn_353 LIMIT 1) FROM tbl_02 AS sub_352 LIMIT 1), CAST(NULLIF(t_ref_304.col_2, EXISTS (SELECT 1 FROM tbl_03 AS sub_354 WHERE sub_354.col_3)) AS SIGNED)) / NULLIF((CASE WHEN CAST(CAST((-9223372036854775808 IS NULL) AS DECIMAL(10,2)) AS CHAR) THEN (LENGTH(TRUE) OR (EXISTS (SELECT 1 FROM tbl_02 AS sub_355 WHERE 2147483647) * -1)) ELSE (t_ref_304.col_2 + CAST((CASE WHEN TRUE THEN t_ref_304.col_2 ELSE 3.14159265 END) AS DECIMAL(10,2))) END), 0)))) AS CHAR) * NULLIF(EXISTS (SELECT 1 FROM tbl_01 AS sub_356 WHERE sub_356.id), EXISTS (SELECT 1 FROM tbl_03 AS sub_357 WHERE NULL))) = (((t_ref_304.id < (SELECT (SELECT 0.0 FROM tbl_04 AS inn_360 LIMIT 1) FROM tbl_07 AS sub_359 LIMIT 1)) AND ((t_ref_304.col_2 IN (SELECT sub_361.col_4 FROM tbl_02 AS sub_361)) = t_ref_304.col_2)) <= CAST(('' AND (t_ref_304.id IN (SELECT sub_366.col_3 FROM tbl_06 AS sub_366))) AS SIGNED)))) - (t_ref_304.col_1 + t_ref_304.id)) OR t_ref_304.col_1))) AS c_372,
    t_ref_304.col_1 AS c_373
FROM tbl_04 AS t_ref_304
WHERE t_ref_304.id
LIMIT 31;

CREATE INDEX idx_tbl_03_col_2_5 ON tbl_03 (col_2);

INSERT INTO warehouse_inventory (id, payload) VALUES (71616, '__fuzz_marker__');

UPDATE tbl_04 SET col_2 = 42 WHERE id > 37;

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, dyn_col_4) VALUES (318169, 999999999.99, FALSE, 'alpha_beta_gamma');
COMMIT;

UPDATE tbl_01 SET dyn_col_5 = 'alpha_beta_gamma' WHERE id > 6;

UPDATE tbl_01 SET col_4 = -1.5 WHERE id > 59;

DELETE FROM tbl_04 WHERE id = 92872;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSON
);

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, ref_tbl_03_id) VALUES (873537, 999999999.99, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -9223372036854775808, -1);
COMMIT;

ALTER TABLE tbl_08 DROP COLUMN col_1;

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_05 WHERE id IS NOT NULL;

SELECT
    t_ref_374.col_3 AS c_375,
    NULL AS c_376,
    (NULLIF(t_ref_374.col_1, (CASE WHEN ((LOWER('') * (t_ref_374.col_4 * (CAST((NULLIF(CAST(((((2147483647 IS NULL) * (CASE WHEN '' THEN t_ref_374.col_4 ELSE t_ref_374.col_4 END)) < (NOT (LENGTH(t_ref_374.col_3)))) IS NULL) AS CHAR), (COALESCE(CAST(EXISTS (SELECT 1 FROM tbl_06 AS sub_377 WHERE -1e-5) AS CHAR), (CASE WHEN t_ref_374.id THEN ('alpha_beta_gamma' / NULLIF(t_ref_374.ref_tbl_01_id, 0)) ELSE LOWER(CAST(TRUE AS DECIMAL(10,2))) END)) >= UPPER((LENGTH((t_ref_374.id <> 'alpha_beta_gamma')) * NULLIF((1.0 IS NULL), (1 * t_ref_374.col_4)))))) = UPPER(t_ref_374.col_3)) AS DECIMAL(10,2)) BETWEEN 0 AND 1))) + (ABS(EXISTS (SELECT 1 FROM tbl_06 AS sub_378 WHERE sub_378.col_3)) / NULLIF(t_ref_374.ref_tbl_01_id, 0))) THEN t_ref_374.col_2 ELSE FALSE END)) * (COALESCE((t_ref_374.col_3 IN (SELECT sub_379.col_1 FROM tbl_07 AS sub_379)), (CASE WHEN ((LOWER(TRUE) / NULLIF((NOT ((((t_ref_374.col_3 IN (SELECT sub_380.col_2 FROM tbl_01 AS sub_380)) < ABS(((NOT (CAST(NULLIF((TRUE <> TRUE), NULLIF(1.0, t_ref_374.col_1)) AS SIGNED))) = (CAST((LENGTH(TRUE) / NULLIF((1.0 > t_ref_374.col_1), 0)) AS DECIMAL(10,2)) <= (((t_ref_374.id * FALSE) / NULLIF(t_ref_374.col_2, 0)) - COALESCE((SELECT NULL FROM tbl_01 AS sub_382 LIMIT 1), (TRUE BETWEEN 1 AND 2147483647))))))) <= (CAST((CASE WHEN ((((t_ref_374.col_1 IN (SELECT sub_383.ref_tbl_01_id FROM tbl_02 AS sub_383)) < t_ref_374.col_2) * ((t_ref_374.col_3 * t_ref_374.col_3) <> '')) + ((t_ref_374.id IN (SELECT sub_384.col_1 FROM tbl_06 AS sub_384)) > (t_ref_374.id IN (SELECT sub_385.col_2 FROM tbl_08 AS sub_385)))) THEN LENGTH((SELECT LENGTH(sub_386.col_2) FROM tbl_05 AS sub_386 LIMIT 1)) ELSE LOWER(((LOWER('escaped''quote') + LOWER(t_ref_374.col_1)) + ((t_ref_374.col_3 OR t_ref_374.col_3) >= CAST(FALSE AS CHAR)))) END) AS SIGNED) - LENGTH(((t_ref_374.ref_tbl_01_id * ((('ref_val' + 100) + CAST(t_ref_374.col_3 AS DECIMAL(10,2))) <= ABS((SELECT sub_387.col_1 FROM tbl_01 AS sub_387 LIMIT 1)))) OR ((CASE WHEN (t_ref_374.id * (NULL / NULLIF(1.0, 0))) THEN CAST((FALSE > t_ref_374.col_2) AS SIGNED) ELSE CAST(LENGTH(t_ref_374.id) AS SIGNED) END) / NULLIF(t_ref_374.col_1, 0)))))))), 0)) BETWEEN 1 AND 42) THEN COALESCE(((NULL * t_ref_374.col_4) * t_ref_374.col_3), (t_ref_374.col_2 <= LOWER(UPPER(((LENGTH((CAST(COALESCE((t_ref_374.col_1 / NULLIF(TRUE, 0)), LENGTH(t_ref_374.id)) AS CHAR) <> t_ref_374.ref_tbl_01_id)) / NULLIF(((ABS(1.0) - t_ref_374.col_1) - ((NOT (t_ref_374.col_2)) - CAST(t_ref_374.id AS DECIMAL(10,2)))), 0)) > t_ref_374.col_3))))) ELSE ((LOWER(t_ref_374.ref_tbl_01_id) BETWEEN 42 AND 100) <> LOWER(CAST(LOWER(TRUE) AS CHAR))) END)) > ('escaped''quote' IN (SELECT sub_388.ref_tbl_01_id FROM tbl_02 AS sub_388)))) AS c_410
FROM tbl_02 AS t_ref_374 LEFT JOIN tbl_04 AS j_ref_411 ON t_ref_374.id = j_ref_411.id
WHERE t_ref_374.col_2
LIMIT 22;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2),
    col_4 BOOLEAN
);

INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_5) VALUES (693233, 9223372036854775807, FALSE, -1.5, 'chaos');

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, ref_tbl_03_id) VALUES (348663, 3.14159265, '[]', 2147483647, 1);
INSERT INTO tbl_08 (id, col_2) VALUES (939028, '{}');
INSERT INTO tbl_04 (id, col_1, col_2) VALUES (72539, '', 2147483647);
COMMIT;

UPDATE tbl_02 SET ref_tbl_01_id = 2147483647 WHERE id > 92;

ALTER TABLE tbl_01 DROP COLUMN dyn_col_5;

BEGIN;
INSERT INTO warehouse_inventory (id, payload) VALUES (399110, '__fuzz_marker__');
INSERT INTO tbl_04 (id, col_1, col_2) VALUES (307406, '', 100);
COMMIT;

SELECT
    LOWER(CAST(LOWER(CAST((CAST(t_ref_412.col_3 AS SIGNED) >= (SELECT ((-1e-5 / NULLIF(UPPER((CAST(CAST(((sub_413.id + sub_413.col_1) = CAST(sub_413.dyn_col_4 AS CHAR)) AS DECIMAL(10,2)) AS SIGNED) < CAST(EXISTS (SELECT 1 FROM tbl_06 AS sub_414 WHERE sub_414.id) AS SIGNED))), 0)) - sub_413.col_1) FROM tbl_05 AS sub_413 LIMIT 1)) AS DECIMAL(10,2))) AS CHAR)) AS c_415,
    LENGTH(EXISTS (SELECT 1 FROM tbl_08 AS sub_416 WHERE TRUE)) AS c_417,
    (SELECT (CASE WHEN (0 * (SELECT (SELECT inn_420.col_3 FROM tbl_02 AS inn_420 LIMIT 1) FROM tbl_04 AS sub_419 LIMIT 1)) THEN (EXISTS (SELECT 1 FROM tbl_04 AS sub_421 WHERE TRUE) / NULLIF(sub_418.col_2, 0)) ELSE (CAST(((ABS(CAST(((((SELECT (SELECT '' FROM warehouse_inventory AS sub_436 LIMIT 1) FROM tbl_07 AS sub_435 LIMIT 1) * (SELECT (SELECT NULL FROM warehouse_inventory AS inn_438 LIMIT 1) FROM tbl_08 AS sub_437 LIMIT 1)) / NULLIF(NULLIF((LOWER((sub_418.col_2 * sub_418.col_3)) / NULLIF(EXISTS (SELECT 1 FROM tbl_07 AS sub_439 WHERE 'escaped''quote'), 0)), sub_418.col_3), 0)) OR sub_418.col_4) AS DECIMAL(10,2))) / NULLIF(999999999.99, 0)) AND ((SELECT (SELECT inn_441.col_1 FROM tbl_07 AS inn_441 LIMIT 1) FROM tbl_02 AS sub_440 LIMIT 1) IS NULL)) AS SIGNED) <= 'chaos') END) FROM tbl_09 AS sub_418 LIMIT 1) AS c_442
FROM tbl_03 AS t_ref_412
WHERE (UPPER(((ABS((SELECT ((CAST((CASE WHEN sub_443.col_3 THEN (CAST(((sub_443.col_3 > NULL) < NULL) AS CHAR) BETWEEN -9223372036854775808 AND 9223372036854775807) ELSE (CASE WHEN (SELECT (sub_444.col_1 OR sub_444.ref_tbl_01_id) FROM tbl_07 AS sub_444 LIMIT 1) THEN sub_443.col_1 ELSE (SELECT CAST(sub_445.col_4 AS DECIMAL(10,2)) FROM tbl_01 AS sub_445 LIMIT 1) END) END) AS DECIMAL(10,2)) IS NULL) * ((SELECT TRUE FROM tbl_03 AS sub_446 LIMIT 1) - (CASE WHEN FALSE THEN EXISTS (SELECT 1 FROM tbl_02 AS sub_447 WHERE 0) ELSE (SELECT (SELECT TRUE FROM tbl_07 AS inn_450 LIMIT 1) FROM tbl_06 AS sub_449 LIMIT 1) END))) FROM tbl_03 AS sub_443 LIMIT 1)) * t_ref_412.id) * LOWER(CAST((SELECT (CASE WHEN CAST(CAST(CAST((sub_451.payload AND LENGTH(CAST((sub_451.id > 1e10) AS DECIMAL(10,2)))) AS CHAR) AS SIGNED) AS SIGNED) THEN ABS((LENGTH((TRUE OR 100)) = sub_451.payload)) ELSE sub_451.id END) FROM warehouse_inventory AS sub_451 LIMIT 1) AS SIGNED)))) <= (FALSE AND CAST(CAST((NOT ((ABS((((t_ref_412.col_2 BETWEEN 0 AND 2147483647) * EXISTS (SELECT 1 FROM tbl_06 AS sub_452 WHERE 'chaos')) BETWEEN -1 AND 42)) - NULL))) AS DECIMAL(10,2)) AS DECIMAL(10,2))))
LIMIT 33;

UPDATE tbl_07 SET col_2 = FALSE WHERE id > 29;

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_01_id) VALUES (616325, 'alpha_beta_gamma', TRUE, 0);

INSERT INTO tbl_04 (id, col_1, col_2) VALUES (27310, 'alpha_beta_gamma', -9223372036854775808);

CREATE TABLE warehouse_inventory (
    item_id INT AUTO_INCREMENT PRIMARY KEY,
    sku_code VARCHAR(32) NOT NULL,
    warehouse_id INT NOT NULL,
    bin_location VARCHAR(32) NOT NULL,
    stock_qty INT DEFAULT 0,
    CONSTRAINT uk_sku_wh UNIQUE (sku_code, warehouse_id),
    CONSTRAINT uk_bin UNIQUE (warehouse_id, bin_location)
) ENGINE=InnoDB;

SELECT item_id, sku_code, warehouse_id, bin_location, stock_qty
FROM warehouse_inventory
ORDER BY item_id;

SELECT
    CAST((t_ref_453.id + (CAST(((t_ref_453.id IN (SELECT sub_454.ref_tbl_01_id FROM tbl_07 AS sub_454)) - (((t_ref_453.payload + TRUE) / NULLIF(((NULL AND EXISTS (SELECT 1 FROM warehouse_inventory AS sub_455 WHERE sub_455.payload)) + 100), 0)) + (SELECT (SELECT inn_461.payload FROM warehouse_inventory AS inn_461 LIMIT 1) FROM tbl_08 AS sub_457 LIMIT 1))) AS SIGNED) = ABS(((CAST(TRUE AS CHAR) / NULLIF(t_ref_453.id, 0)) < (SELECT (SELECT inn_463.ref_tbl_03_id FROM tbl_06 AS inn_463 LIMIT 1) FROM tbl_01 AS sub_462 LIMIT 1))))) AS SIGNED) AS c_464
FROM warehouse_inventory AS t_ref_453 LEFT JOIN tbl_04 AS j_ref_465 ON t_ref_453.id = j_ref_465.id
WHERE (t_ref_453.payload IN (SELECT sub_466.col_2 FROM tbl_04 AS sub_466))
LIMIT 23;

ALTER TABLE tbl_03 ADD COLUMN dyn_col_6 VARCHAR(32);

INSERT INTO warehouse_inventory (id, payload) VALUES (883789, 'ref_val');

BEGIN;
INSERT INTO warehouse_inventory (id, payload) VALUES (722612, 'ref_val');
INSERT INTO tbl_05 (id, col_1, col_2, dyn_col_4) VALUES (447031, 0.0, TRUE, '');
COMMIT;

ALTER TABLE tbl_05 DROP COLUMN col_1;

BEGIN;
INSERT INTO tbl_08 (id, col_2) VALUES (585027, '[]');
INSERT INTO tbl_08 (id, col_2) VALUES (555644, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
COMMIT;

SELECT
    TRUE AS c_469,
    EXISTS (SELECT 1 FROM tbl_06 AS sub_470 WHERE sub_470.id) AS c_487,
    t_ref_468.col_2 AS c_488
FROM tbl_07 AS t_ref_468
WHERE (CAST((CASE WHEN EXISTS (SELECT 1 FROM warehouse_inventory AS sub_489 WHERE FALSE) THEN ((SELECT CAST(((sub_497.dyn_col_4 - ((CASE WHEN sub_497.id THEN sub_497.dyn_col_4 ELSE CAST(-1 AS CHAR) END) - 1)) BETWEEN 2147483647 AND 2147483647) AS CHAR) FROM tbl_05 AS sub_497 LIMIT 1) / NULLIF(t_ref_468.col_1, 0)) ELSE ((SELECT (SELECT inn_499.ref_tbl_01_id FROM tbl_02 AS inn_499 LIMIT 1) FROM warehouse_inventory AS sub_498 LIMIT 1) / NULLIF((NOT (t_ref_468.id)), 0)) END) AS DECIMAL(10,2)) >= ((NULLIF((COALESCE((LOWER(COALESCE(((CAST(0.0 AS DECIMAL(10,2)) - (CAST(((t_ref_468.col_2 = t_ref_468.col_2) <= (SELECT (SELECT NULL FROM warehouse_inventory AS inn_501 LIMIT 1) FROM tbl_04 AS sub_500 LIMIT 1)) AS DECIMAL(10,2)) < ((NULLIF(t_ref_468.col_1, t_ref_468.id) / NULLIF(('__fuzz_marker__' = t_ref_468.col_2), 0)) OR t_ref_468.col_1))) <> EXISTS (SELECT 1 FROM tbl_08 AS sub_502 WHERE -1e-5)), (SELECT sub_503.col_4 FROM tbl_01 AS sub_503 LIMIT 1))) = TRUE), COALESCE(EXISTS (SELECT 1 FROM tbl_01 AS sub_504 WHERE 'alpha_beta_gamma'), UPPER((t_ref_468.ref_tbl_01_id IN (SELECT sub_505.col_4 FROM tbl_09 AS sub_505))))) * (CAST(COALESCE(FALSE, CAST((CAST(EXISTS (SELECT 1 FROM tbl_07 AS sub_507 WHERE NULL) AS DECIMAL(10,2)) OR CAST(('alpha_beta_gamma' IN (SELECT sub_509.col_4 FROM tbl_01 AS sub_509)) AS DECIMAL(10,2))) AS DECIMAL(10,2))) AS CHAR) / NULLIF(CAST(CAST((NOT (t_ref_468.ref_tbl_01_id)) AS DECIMAL(10,2)) AS SIGNED), 0))), t_ref_468.col_1) >= ((((CAST(FALSE AS SIGNED) - ((t_ref_468.col_2 - CAST((NULLIF((t_ref_468.id AND (t_ref_468.id IN (SELECT sub_510.col_4 FROM tbl_09 AS sub_510))), (CASE WHEN (LENGTH(FALSE) + ABS(t_ref_468.col_1)) THEN CAST(t_ref_468.ref_tbl_01_id AS SIGNED) ELSE CAST(('ref_val' IN (SELECT sub_511.col_1 FROM tbl_03 AS sub_511)) AS CHAR) END)) / NULLIF((SELECT -1 FROM tbl_04 AS sub_512 LIMIT 1), 0)) AS SIGNED)) - ((t_ref_468.ref_tbl_01_id <> (CAST(NULL AS SIGNED) <= (LOWER(t_ref_468.id) <= CAST(((t_ref_468.id OR t_ref_468.col_2) + TRUE) AS SIGNED)))) / NULLIF((EXISTS (SELECT 1 FROM tbl_02 AS sub_513 WHERE sub_513.col_4) * ((SELECT LENGTH(999999999.99) FROM tbl_05 AS sub_514 LIMIT 1) > ((CAST(CAST(-2147483648 AS CHAR) AS DECIMAL(10,2)) OR (EXISTS (SELECT 1 FROM tbl_03 AS sub_515 WHERE TRUE) IS NULL)) / NULLIF(t_ref_468.col_1, 0)))), 0)))) / NULLIF(((SELECT CAST(CAST((CASE WHEN ABS(sub_516.id) THEN (NULL - ((SELECT (SELECT inn_518.col_1 FROM tbl_06 AS inn_518 LIMIT 1) FROM tbl_05 AS sub_517 LIMIT 1) IS NULL)) ELSE CAST(ABS((sub_516.col_2 IN (SELECT sub_519.id FROM tbl_02 AS sub_519))) AS CHAR) END) AS CHAR) AS SIGNED) FROM tbl_04 AS sub_516 LIMIT 1) = ((((((t_ref_468.col_2 - ABS(t_ref_468.ref_tbl_01_id)) = (SELECT (sub_520.col_1 - sub_520.col_4) FROM tbl_09 AS sub_520 LIMIT 1)) IS NULL) - (SELECT FALSE FROM tbl_07 AS sub_521 LIMIT 1)) = (LOWER(t_ref_468.id) IS NOT NULL)) * (((SELECT (SELECT inn_524.payload FROM warehouse_inventory AS inn_524 LIMIT 1) FROM tbl_02 AS sub_522 LIMIT 1) < TRUE) + (CASE WHEN LENGTH(FALSE) THEN t_ref_468.ref_tbl_01_id ELSE (t_ref_468.col_2 + ((SELECT (SELECT inn_526.dyn_col_6 FROM tbl_03 AS inn_526 LIMIT 1) FROM tbl_09 AS sub_525 LIMIT 1) * FALSE)) END)))), 0)) * (t_ref_468.col_2 >= ((EXISTS (SELECT 1 FROM tbl_03 AS sub_527 WHERE sub_527.col_3) + (SELECT (SELECT inn_530.col_2 FROM tbl_05 AS inn_530 LIMIT 1) FROM tbl_07 AS sub_528 LIMIT 1)) = ABS((9223372036854775807 IN (SELECT sub_531.col_3 FROM tbl_09 AS sub_531)))))) + ((SELECT (SELECT inn_544.payload FROM warehouse_inventory AS inn_544 LIMIT 1) FROM tbl_07 AS sub_536 LIMIT 1) / NULLIF((LENGTH(((SELECT (SELECT inn_546.col_4 FROM tbl_09 AS inn_546 LIMIT 1) FROM warehouse_inventory AS sub_545 LIMIT 1) = (SELECT (SELECT inn_548.col_2 FROM tbl_04 AS inn_548 LIMIT 1) FROM tbl_07 AS sub_547 LIMIT 1))) AND ABS(('ref_val' BETWEEN 0 AND 2147483647))), 0)))) IS NULL))
LIMIT 24;

CREATE INDEX idx_tbl_04_id_6 ON tbl_04 (id);

BEGIN;
INSERT INTO tbl_09 (id, col_1, col_2, col_3, col_4) VALUES (946466, 9223372036854775807, 1.0, 3.14159265, TRUE);
INSERT INTO tbl_05 (id, col_2, dyn_col_4) VALUES (429387, FALSE, 'escaped''quote');
COMMIT;

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_07 WHERE id IS NOT NULL;

SELECT
    -9223372036854775808 AS c_550,
    (((CASE WHEN t_ref_549.dyn_col_4 THEN CAST(EXISTS (SELECT 1 FROM tbl_08 AS sub_551 WHERE 'alpha_beta_gamma') AS DECIMAL(10,2)) ELSE t_ref_549.dyn_col_4 END) > 1e10) BETWEEN -9223372036854775808 AND 2147483647) AS c_555,
    (SELECT (SELECT (SELECT TRUE FROM tbl_01 AS inn_558 LIMIT 1) FROM tbl_08 AS sub_557 LIMIT 1) FROM tbl_01 AS sub_556 LIMIT 1) AS c_559
FROM tbl_05 AS t_ref_549 LEFT JOIN tbl_01 AS j_ref_560 ON t_ref_549.id = j_ref_560.id
WHERE (((FALSE IN (SELECT sub_561.col_4 FROM tbl_01 AS sub_561)) IS NOT NULL) < CAST(('chaos' / NULLIF(((t_ref_549.col_2 / NULLIF((t_ref_549.dyn_col_4 * t_ref_549.dyn_col_4), 0)) <= CAST(EXISTS (SELECT 1 FROM warehouse_inventory AS sub_563 WHERE sub_563.id) AS SIGNED)), 0)) AS CHAR))
LIMIT 22;

CREATE INDEX idx_tbl_04_col_2_7 ON tbl_04 (col_2);

CREATE INDEX idx_tbl_01_col_1_8 ON tbl_01 (col_1);

SELECT
    CAST(((CASE WHEN t_ref_565.id THEN NULLIF((-9223372036854775808 <> t_ref_565.col_3), (TRUE - (42 - CAST(EXISTS (SELECT 1 FROM tbl_01 AS sub_566 WHERE sub_566.col_4) AS CHAR)))) ELSE EXISTS (SELECT 1 FROM tbl_06 AS sub_569 WHERE sub_569.col_3) END) * t_ref_565.col_2) AS CHAR) AS c_572,
    ((NULLIF(t_ref_565.id, LENGTH(('chaos' * ((TRUE - UPPER(((((ABS(UPPER(ABS(t_ref_565.col_4))) IS NOT NULL) / NULLIF((SELECT (SELECT inn_575.col_2 FROM tbl_04 AS inn_575 LIMIT 1) FROM warehouse_inventory AS sub_573 LIMIT 1), 0)) + UPPER(UPPER(((FALSE IN (SELECT sub_576.col_2 FROM tbl_02 AS sub_576)) AND (CAST(t_ref_565.col_3 AS CHAR) BETWEEN 42 AND 9223372036854775807))))) - t_ref_565.id))) BETWEEN 9223372036854775807 AND -1)))) > EXISTS (SELECT 1 FROM tbl_07 AS sub_577 WHERE sub_577.col_2)) BETWEEN -1 AND -9223372036854775808) AS c_578,
    t_ref_565.col_3 AS c_579
FROM tbl_09 AS t_ref_565
WHERE (SELECT (SELECT inn_585.col_2 FROM tbl_04 AS inn_585 LIMIT 1) FROM tbl_08 AS sub_580 LIMIT 1)
LIMIT 36;

DELETE FROM tbl_03 WHERE id = 4428;

CREATE INDEX idx_warehouse_inventory_payload_9 ON warehouse_inventory (payload);

INSERT INTO tbl_01 (id, col_1, col_2, col_4) VALUES (975583, 1, FALSE, 0.0);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (331174, 'alpha_beta_gamma', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 9223372036854775807, 'alpha_beta_gamma', 100);

SELECT
    LOWER((SELECT sub_587.col_2 FROM tbl_08 AS sub_587 LIMIT 1)) AS c_588
FROM tbl_06 AS t_ref_586
WHERE (SELECT (NOT ((((sub_589.col_4 BETWEEN 0 AND 42) - (CAST(((UPPER(NULLIF(ABS(sub_589.col_4), (NOT (CAST(((sub_589.col_4 + sub_589.col_3) * ('chaos' + TRUE)) AS CHAR))))) * EXISTS (SELECT 1 FROM warehouse_inventory AS sub_590 WHERE sub_590.payload)) / NULLIF(42, 0)) AS SIGNED) BETWEEN -1 AND -2147483648)) <= ((EXISTS (SELECT 1 FROM tbl_02 AS sub_591 WHERE sub_591.col_4) <= TRUE) OR (CAST(CAST(CAST(CAST(sub_589.id AS SIGNED) AS SIGNED) AS CHAR) AS DECIMAL(10,2)) IS NOT NULL))))) FROM tbl_03 AS sub_589 LIMIT 1)
LIMIT 29;

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN,
    col_3 JSON
);

INSERT INTO warehouse_inventory (id, payload) VALUES (786120, 'escaped''quote');

CREATE VIEW v_summary_6 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

UPDATE tbl_07 SET col_1 = 'escaped''quote' WHERE id > 21;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, ref_tbl_03_id) VALUES (292696, 0.0000000001, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 2147483647, -2147483648);

INSERT INTO tbl_06 (id, col_1, col_2, col_3, ref_tbl_03_id) VALUES (906968, 3.14159265, '{}', 100, 2147483647);

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_01_id) VALUES (958419, 'ref_val', TRUE, 1);
COMMIT;

UPDATE tbl_02 SET col_3 = 2147483647 WHERE id > 92;

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (86073, 'escaped''quote', 2147483647, FALSE, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'ref_val');

SELECT
    CAST((LOWER((t_ref_593.id IN (SELECT sub_594.id FROM tbl_06 AS sub_594))) BETWEEN 1 AND -2147483648) AS SIGNED) AS c_604
FROM tbl_07 AS t_ref_593 LEFT JOIN tbl_09 AS j_ref_605 ON t_ref_593.id = j_ref_605.id
WHERE NULL
LIMIT 6;
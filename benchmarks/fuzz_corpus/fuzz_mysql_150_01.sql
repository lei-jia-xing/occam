-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: MYSQL

-- Statements: 150

-- AST Expression Depth: 16

-- Injected Defect Case: mysql_06

-- Seed: 15021



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 INT,
    col_3 JSON
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 VARCHAR(64),
    col_4 INT,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 INT,
    col_3 VARCHAR(64)
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 JSON
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT,
    col_3 BOOLEAN,
    col_4 JSON,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE geofence_zones (
    zone_id INT PRIMARY KEY,
    zone_name VARCHAR(64) NOT NULL,
    boundary GEOMETRY NOT NULL /*!80003 SRID 0 */,
    SPATIAL INDEX idx_geofence_boundary (boundary)
) ENGINE=InnoDB;

CREATE INDEX idx_tbl_04_col_3_1 ON tbl_04 (col_3);

BEGIN;
INSERT INTO geofence_zones (id, payload) VALUES (661395, '');
INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (329631, '{}', -1, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

UPDATE tbl_01 SET col_2 = 100 WHERE id > 47;

SELECT
    CAST(NULL AS DECIMAL(10,2)) AS c_2,
    t_ref_1.id AS c_3
FROM tbl_03 AS t_ref_1
WHERE ABS((t_ref_1.col_1 IN (SELECT sub_4.col_2 FROM tbl_03 AS sub_4)))
LIMIT 41;

INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (721742, '{}', 42, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (410179, 9223372036854775807, 42, TRUE, '{}', 1);

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_05 WHERE id IS NOT NULL;

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_05 WHERE id IS NOT NULL;

INSERT INTO geofence_zones (id, payload) VALUES (8906, 'chaos');

UPDATE tbl_03 SET col_1 = '{}' WHERE id > 16;

INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (231401, '[]', 1, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

SELECT
    UPPER(ABS(((NOT (UPPER(t_ref_14.col_1))) <= ((t_ref_14.col_3 <= (SELECT (EXISTS (SELECT 1 FROM tbl_01 AS sub_16 WHERE NULL) * CAST(sub_15.id AS SIGNED)) FROM tbl_01 AS sub_15 LIMIT 1)) + t_ref_14.col_2)))) AS c_17,
    (t_ref_14.col_1 IS NULL) AS c_18,
    NULLIF(((((((UPPER(t_ref_14.id) - (t_ref_14.col_3 IN (SELECT sub_19.col_3 FROM tbl_01 AS sub_19))) * (NOT (t_ref_14.col_3))) = (CASE WHEN CAST((((SELECT (SELECT 'ref_val' FROM tbl_02 AS inn_27 LIMIT 1) FROM tbl_02 AS sub_24 LIMIT 1) OR CAST(t_ref_14.id AS CHAR)) AND (SELECT (SELECT inn_29.id FROM tbl_05 AS inn_29 LIMIT 1) FROM tbl_05 AS sub_28 LIMIT 1)) AS DECIMAL(10,2)) THEN CAST(COALESCE((t_ref_14.col_2 IN (SELECT sub_30.ref_tbl_01_id FROM tbl_02 AS sub_30)), (CASE WHEN (('__fuzz_marker__' IS NOT NULL) <= (SELECT sub_31.id FROM tbl_04 AS sub_31 LIMIT 1)) THEN (CAST((t_ref_14.col_2 OR UPPER(CAST(TRUE AS DECIMAL(10,2)))) AS CHAR) BETWEEN -1 AND 2147483647) ELSE ((SELECT (SELECT 1 FROM tbl_04 AS inn_34 LIMIT 1) FROM geofence_zones AS sub_32 LIMIT 1) IS NOT NULL) END)) AS CHAR) ELSE (SELECT ((EXISTS (SELECT 1 FROM tbl_05 AS sub_36 WHERE sub_36.col_2) - (CAST(((sub_35.col_3 IN (SELECT sub_37.col_2 FROM tbl_04 AS sub_37)) / NULLIF((CAST(sub_35.id AS CHAR) AND (NOT (sub_35.col_2))), 0)) AS CHAR) <= ((CASE WHEN sub_35.col_1 THEN (ABS(sub_35.col_2) <> (sub_35.col_3 > NULL)) ELSE ((sub_35.col_3 / NULLIF(sub_35.col_2, 0)) - (sub_35.col_3 <> sub_35.col_2)) END) > sub_35.col_3))) <= sub_35.col_1) FROM tbl_01 AS sub_35 LIMIT 1) END)) - ((FALSE IN (SELECT sub_38.col_3 FROM tbl_03 AS sub_38)) - ((EXISTS (SELECT 1 FROM tbl_05 AS sub_41 WHERE sub_41.col_3) = ((CASE WHEN ((((t_ref_14.col_2 AND (UPPER(t_ref_14.col_3) OR t_ref_14.col_2)) BETWEEN 2147483647 AND -9223372036854775808) > COALESCE(((CAST(t_ref_14.id AS SIGNED) <> (FALSE - TRUE)) <> EXISTS (SELECT 1 FROM tbl_05 AS sub_42 WHERE '')), (t_ref_14.col_2 + ''))) + ((SELECT (SELECT inn_46.payload FROM geofence_zones AS inn_46 LIMIT 1) FROM tbl_04 AS sub_43 LIMIT 1) BETWEEN -9223372036854775808 AND -1)) THEN LOWER(((CASE WHEN (((t_ref_14.col_2 > t_ref_14.col_1) AND ABS(t_ref_14.col_1)) BETWEEN 100 AND 0) THEN t_ref_14.col_1 ELSE (t_ref_14.col_1 BETWEEN 1 AND 0) END) = FALSE)) ELSE NULL END) + TRUE)) >= (t_ref_14.col_3 - CAST(COALESCE((CASE WHEN CAST(t_ref_14.col_3 AS SIGNED) THEN (CASE WHEN (CAST((SELECT sub_47.col_1 FROM tbl_04 AS sub_47 LIMIT 1) AS CHAR) AND t_ref_14.col_2) THEN COALESCE(UPPER(COALESCE((t_ref_14.col_3 + t_ref_14.col_3), EXISTS (SELECT 1 FROM tbl_03 AS sub_48 WHERE sub_48.col_1))), (EXISTS (SELECT 1 FROM tbl_04 AS sub_49 WHERE sub_49.col_1) - LOWER((SELECT (SELECT inn_51.id FROM tbl_05 AS inn_51 LIMIT 1) FROM tbl_02 AS sub_50 LIMIT 1)))) ELSE NULLIF(UPPER(((t_ref_14.col_2 <> t_ref_14.col_2) / NULLIF((t_ref_14.col_3 >= 'chaos'), 0))), EXISTS (SELECT 1 FROM tbl_02 AS sub_52 WHERE sub_52.col_3)) END) ELSE CAST((((FALSE IN (SELECT sub_53.col_1 FROM tbl_03 AS sub_53)) * ABS(LOWER(t_ref_14.col_1))) - ABS(ABS((TRUE + FALSE)))) AS CHAR) END), EXISTS (SELECT 1 FROM tbl_02 AS sub_54 WHERE TRUE)) AS SIGNED))))) / NULLIF(t_ref_14.col_2, 0)) * (NULLIF(((COALESCE(LENGTH((-2147483648 IN (SELECT sub_56.col_3 FROM tbl_04 AS sub_56))), CAST(FALSE AS DECIMAL(10,2))) OR TRUE) = ABS((((-9223372036854775808 <> (NOT ((t_ref_14.col_1 <= t_ref_14.id)))) + ((t_ref_14.id * UPPER(UPPER((TRUE / NULLIF(((t_ref_14.col_1 * t_ref_14.col_2) / NULLIF(CAST(t_ref_14.col_1 AS DECIMAL(10,2)), 0)), 0))))) / NULLIF((999999999.99 - t_ref_14.col_1), 0))) * LOWER((SELECT (NULLIF(sub_57.col_4, (UPPER(sub_57.col_2) * CAST((sub_57.col_4 > sub_57.col_2) AS SIGNED))) <> CAST((100 OR FALSE) AS SIGNED)) FROM tbl_02 AS sub_57 LIMIT 1))))), UPPER((CASE WHEN COALESCE(((SELECT ((sub_58.col_3 IN (SELECT sub_59.id FROM tbl_04 AS sub_59)) < sub_58.id) FROM tbl_04 AS sub_58 LIMIT 1) + (t_ref_14.col_2 + COALESCE(LENGTH(t_ref_14.id), LOWER((COALESCE(t_ref_14.id, (CASE WHEN NULL THEN (NULL <> TRUE) ELSE (CASE WHEN TRUE THEN NULL ELSE FALSE END) END)) <> (((NULL / NULLIF(t_ref_14.col_2, 0)) OR (t_ref_14.col_2 + t_ref_14.col_2)) BETWEEN 1 AND 9223372036854775807)))))), (CASE WHEN EXISTS (SELECT 1 FROM tbl_01 AS sub_60 WHERE sub_60.col_2) THEN (2147483647 IN (SELECT sub_62.col_1 FROM tbl_04 AS sub_62)) ELSE LOWER((EXISTS (SELECT 1 FROM geofence_zones AS sub_63 WHERE sub_63.id) = ((SELECT (SELECT inn_65.payload FROM geofence_zones AS inn_65 LIMIT 1) FROM tbl_02 AS sub_64 LIMIT 1) BETWEEN -2147483648 AND -1))) END)) THEN CAST((ABS(((SELECT LOWER(sub_66.ref_tbl_01_id) FROM tbl_02 AS sub_66 LIMIT 1) * 'alpha_beta_gamma')) - (UPPER(((CASE WHEN (999999999.99 IN (SELECT sub_67.col_3 FROM tbl_03 AS sub_67)) THEN (FALSE / NULLIF((ABS(t_ref_14.id) IS NULL), 0)) ELSE (CASE WHEN CAST((SELECT (SELECT inn_69.col_3 FROM tbl_05 AS inn_69 LIMIT 1) FROM geofence_zones AS sub_68 LIMIT 1) AS DECIMAL(10,2)) THEN (TRUE IN (SELECT sub_70.id FROM tbl_01 AS sub_70)) ELSE EXISTS (SELECT 1 FROM tbl_02 AS sub_71 WHERE 0.0000000001) END) END) AND (SELECT NULLIF(sub_72.col_3, (sub_72.col_2 IN (SELECT sub_73.payload FROM geofence_zones AS sub_73))) FROM tbl_02 AS sub_72 LIMIT 1))) >= (NULLIF(t_ref_14.col_2, (SELECT (SELECT inn_75.payload FROM geofence_zones AS inn_75 LIMIT 1) FROM geofence_zones AS sub_74 LIMIT 1)) / NULLIF(CAST(((CAST(COALESCE(t_ref_14.id, t_ref_14.col_2) AS DECIMAL(10,2)) - CAST(TRUE AS SIGNED)) <= ((t_ref_14.col_3 / NULLIF((CASE WHEN FALSE THEN 0.0000000001 ELSE NULL END), 0)) AND (t_ref_14.col_1 <> ABS(t_ref_14.col_3)))) AS CHAR), 0)))) AS CHAR) ELSE (SELECT (SELECT inn_85.id FROM tbl_02 AS inn_85 LIMIT 1) FROM tbl_04 AS sub_76 LIMIT 1) END))) * COALESCE((SELECT (SELECT inn_87.id FROM tbl_03 AS inn_87 LIMIT 1) FROM tbl_02 AS sub_86 LIMIT 1), t_ref_14.col_2))), 9223372036854775807) AS c_88
FROM tbl_04 AS t_ref_14 LEFT JOIN tbl_05 AS j_ref_89 ON t_ref_14.id = j_ref_89.id
WHERE (EXISTS (SELECT 1 FROM tbl_04 AS sub_90 WHERE 2147483647) * (COALESCE((t_ref_14.col_2 - (CAST((TRUE IN (SELECT sub_95.col_2 FROM tbl_01 AS sub_95)) AS SIGNED) * t_ref_14.col_2)), COALESCE((t_ref_14.col_2 IN (SELECT sub_103.id FROM tbl_02 AS sub_103)), EXISTS (SELECT 1 FROM tbl_01 AS sub_104 WHERE sub_104.col_1))) - EXISTS (SELECT 1 FROM tbl_01 AS sub_115 WHERE sub_115.id)))
LIMIT 6;

INSERT INTO tbl_03 (id, col_1, col_2, col_3) VALUES (124070, '[]', -2147483648, 'ref_val');

INSERT INTO geofence_zones (id, payload) VALUES (203444, 'alpha_beta_gamma');

INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (542950, 2147483647, 1, TRUE, '[]', 0);

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 BOOLEAN,
    col_3 INT,
    col_4 NUMERIC(10,2)
);

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    ref_tbl_05_id INT,
    FOREIGN KEY (ref_tbl_05_id) REFERENCES tbl_05(id)
);

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (973692, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 1, '[]');
INSERT INTO tbl_03 (id, col_1, col_2, col_3) VALUES (484881, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 0, 'alpha_beta_gamma');
COMMIT;

CREATE VIEW v_summary_3 AS SELECT id FROM geofence_zones WHERE id IS NOT NULL;

DELETE FROM tbl_05 WHERE id = 33935;

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_07 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO geofence_zones (id, payload) VALUES (982012, 'chaos');
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (296546, '{}', FALSE, 1, 0.0);
INSERT INTO tbl_03 (id, col_1, col_2, col_3) VALUES (69403, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 9223372036854775807, '__fuzz_marker__');
COMMIT;

SELECT
    (NULL IS NULL) AS c_119,
    (CAST((CAST(FALSE AS DECIMAL(10,2)) BETWEEN -9223372036854775808 AND -1) AS SIGNED) IS NOT NULL) AS c_120
FROM tbl_05 AS t_ref_118
WHERE (('escaped''quote' IN (SELECT sub_121.id FROM tbl_04 AS sub_121)) * (SELECT (CAST((sub_122.id * (SELECT (SELECT TRUE FROM tbl_06 AS inn_126 LIMIT 1) FROM tbl_07 AS sub_123 LIMIT 1)) AS DECIMAL(10,2)) / NULLIF(NULLIF((SELECT (SELECT inn_130.id FROM tbl_06 AS inn_130 LIMIT 1) FROM geofence_zones AS sub_127 LIMIT 1), sub_122.col_1), 0)) FROM tbl_04 AS sub_122 LIMIT 1))
LIMIT 25;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (845831, 0.0, 'ref_val', 'ref_val', 1, 0);

SELECT
    ((CAST((SELECT 0.0 FROM tbl_06 AS sub_132 LIMIT 1) AS SIGNED) BETWEEN 42 AND 0) * NULL) AS c_133,
    t_ref_131.col_3 AS c_134,
    (t_ref_131.col_1 - t_ref_131.col_1) AS c_135
FROM tbl_04 AS t_ref_131 LEFT JOIN tbl_01 AS j_ref_136 ON t_ref_131.id = j_ref_136.id
WHERE (NOT (CAST((EXISTS (SELECT 1 FROM tbl_06 AS sub_137 WHERE sub_137.id) IS NULL) AS SIGNED)))
LIMIT 19;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSON
);

INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_05_id) VALUES (351124, 3.14159265, '__fuzz_marker__', -9223372036854775808);

SELECT
    UPPER(t_ref_138.id) AS c_139,
    LENGTH(t_ref_138.col_1) AS c_140,
    LOWER(UPPER((t_ref_138.col_3 < t_ref_138.col_2))) AS c_141
FROM tbl_04 AS t_ref_138
WHERE ((SELECT (SELECT inn_143.col_1 FROM tbl_08 AS inn_143 LIMIT 1) FROM tbl_03 AS sub_142 LIMIT 1) IS NULL)
LIMIT 50;

ALTER TABLE tbl_05 DROP COLUMN col_3;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

INSERT INTO geofence_zones (zone_id, zone_name, boundary) VALUES
    (1, 'Terminal Zone', ST_GeomFromText('POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))')),
    (2, 'Depot Zone', ST_GeomFromText('POLYGON((20 20, 20 30, 30 30, 30 20, 20 20))')),
    (3, 'Degenerate Zero Area', ST_GeomFromText('POLYGON((5 5, 5 5, 5 5, 5 5, 5 5))')),
    (4, 'Collinear Segment', ST_GeomFromText('POLYGON((0 0, 0 5, 0 10, 0 5, 0 0))'));

SELECT
    ABS(((CASE WHEN (CASE WHEN (SELECT ABS((EXISTS (SELECT 1 FROM tbl_06 AS sub_146 WHERE sub_146.id) * FALSE)) FROM geofence_zones AS sub_145 LIMIT 1) THEN ((ABS((CASE WHEN CAST(CAST(TRUE AS CHAR) AS SIGNED) THEN t_ref_144.id ELSE (SELECT (SELECT inn_150.col_3 FROM tbl_03 AS inn_150 LIMIT 1) FROM tbl_05 AS sub_148 LIMIT 1) END)) - (((FALSE = (CASE WHEN (NOT ((t_ref_144.col_2 * NULL))) THEN (SELECT (SELECT inn_153.col_2 FROM tbl_07 AS inn_153 LIMIT 1) FROM geofence_zones AS sub_151 LIMIT 1) ELSE t_ref_144.col_3 END)) - (ABS(((SELECT (SELECT 'alpha_beta_gamma' FROM tbl_03 AS inn_155 LIMIT 1) FROM geofence_zones AS sub_154 LIMIT 1) BETWEEN -9223372036854775808 AND 9223372036854775807)) + t_ref_144.col_3)) + ((SELECT UPPER((sub_156.id AND (CASE WHEN ((sub_156.payload < NULL) / NULLIF(CAST(sub_156.payload AS DECIMAL(10,2)), 0)) THEN (sub_156.id IN (SELECT sub_157.col_2 FROM tbl_09 AS sub_157)) ELSE (CASE WHEN (1 < sub_156.id) THEN (NOT ('chaos')) ELSE sub_156.id END) END))) FROM geofence_zones AS sub_156 LIMIT 1) BETWEEN 0 AND -1))) - NULLIF((t_ref_144.col_2 + CAST((CAST(CAST((SELECT (SELECT FALSE FROM tbl_01 AS inn_160 LIMIT 1) FROM tbl_06 AS sub_158 LIMIT 1) AS DECIMAL(10,2)) AS CHAR) AND ((SELECT (SELECT inn_162.col_1 FROM tbl_06 AS inn_162 LIMIT 1) FROM tbl_07 AS sub_161 LIMIT 1) = FALSE)) AS CHAR)), UPPER(t_ref_144.id))) ELSE (9223372036854775807 * ((((t_ref_144.id <= ((((SELECT sub_163.col_1 FROM tbl_01 AS sub_163 LIMIT 1) * CAST(CAST((t_ref_144.col_3 = 0) AS CHAR) AS SIGNED)) + CAST((t_ref_144.col_3 + LOWER(0.0)) AS DECIMAL(10,2))) > ((t_ref_144.col_2 / NULLIF(-1, 0)) / NULLIF(t_ref_144.col_1, 0)))) IS NULL) < t_ref_144.col_3) <= (CASE WHEN (t_ref_144.col_3 >= UPPER(CAST(ABS(CAST(LENGTH(((NULL * t_ref_144.col_2) = EXISTS (SELECT 1 FROM geofence_zones AS sub_164 WHERE sub_164.payload))) AS SIGNED)) AS SIGNED))) THEN t_ref_144.col_3 ELSE (t_ref_144.col_3 <= 1.0) END))) END) THEN t_ref_144.col_2 ELSE ((EXISTS (SELECT 1 FROM geofence_zones AS sub_165 WHERE -1) - COALESCE((SELECT 'ref_val' FROM tbl_07 AS sub_166 LIMIT 1), t_ref_144.col_2)) + LENGTH((SELECT (SELECT inn_168.col_1 FROM tbl_08 AS inn_168 LIMIT 1) FROM tbl_09 AS sub_167 LIMIT 1))) END) BETWEEN 9223372036854775807 AND 9223372036854775807)) AS c_169,
    2147483647 AS c_170,
    CAST(CAST(CAST((SELECT (SELECT (SELECT CAST((LOWER((CAST((sub_173.col_1 > -1) AS SIGNED) <= CAST(LENGTH(sub_173.col_3) AS DECIMAL(10,2)))) OR ((ABS((SELECT FALSE FROM tbl_02 AS sub_174 LIMIT 1)) > (sub_173.col_1 / NULLIF((sub_173.id + TRUE), 0))) * UPPER((FALSE / NULLIF((SELECT sub_175.id FROM tbl_05 AS sub_175 LIMIT 1), 0))))) AS DECIMAL(10,2)) FROM tbl_01 AS sub_173 LIMIT 1) FROM tbl_01 AS sub_172 LIMIT 1) FROM tbl_04 AS sub_171 LIMIT 1) AS DECIMAL(10,2)) AS CHAR) AS SIGNED) AS c_176
FROM tbl_01 AS t_ref_144
WHERE COALESCE((CASE WHEN (NOT ((CASE WHEN (t_ref_144.col_2 IN (SELECT sub_177.id FROM tbl_01 AS sub_177)) THEN (SELECT (SELECT inn_191.col_1 FROM tbl_05 AS inn_191 LIMIT 1) FROM tbl_07 AS sub_187 LIMIT 1) ELSE (EXISTS (SELECT 1 FROM tbl_01 AS sub_192 WHERE TRUE) BETWEEN -1 AND -2147483648) END))) THEN NULLIF((UPPER(LOWER(t_ref_144.col_1)) / NULLIF(LENGTH((t_ref_144.id / NULLIF(((LENGTH(((t_ref_144.col_2 IN (SELECT sub_193.col_1 FROM tbl_01 AS sub_193)) / NULLIF((UPPER(t_ref_144.col_1) + t_ref_144.col_2), 0))) / NULLIF(t_ref_144.id, 0)) / NULLIF(EXISTS (SELECT 1 FROM tbl_03 AS sub_194 WHERE sub_194.col_3), 0)), 0))), 0)), (ABS(((SELECT sub_195.id FROM tbl_05 AS sub_195 LIMIT 1) OR (((SELECT ((sub_196.col_4 IN (SELECT sub_197.col_4 FROM tbl_06 AS sub_197)) <> 1) FROM tbl_05 AS sub_196 LIMIT 1) / NULLIF(t_ref_144.col_2, 0)) BETWEEN 9223372036854775807 AND -2147483648))) = (CAST((FALSE IN (SELECT sub_198.id FROM tbl_07 AS sub_198)) AS CHAR) - (COALESCE((((COALESCE((t_ref_144.col_3 < ((CASE WHEN t_ref_144.col_1 THEN t_ref_144.col_2 ELSE (t_ref_144.col_2 IN (SELECT sub_199.ref_tbl_05_id FROM tbl_07 AS sub_199)) END) > CAST(LENGTH(NULL) AS DECIMAL(10,2)))), ((SELECT (SELECT NULL FROM tbl_07 AS inn_201 LIMIT 1) FROM tbl_03 AS sub_200 LIMIT 1) <> ABS('alpha_beta_gamma'))) OR t_ref_144.col_2) + (TRUE IN (SELECT sub_202.col_3 FROM tbl_02 AS sub_202))) + t_ref_144.col_1), (EXISTS (SELECT 1 FROM tbl_02 AS sub_206 WHERE sub_206.ref_tbl_01_id) + NULLIF((CASE WHEN t_ref_144.col_3 THEN CAST((EXISTS (SELECT 1 FROM tbl_01 AS sub_209 WHERE -9223372036854775808) AND t_ref_144.col_3) AS DECIMAL(10,2)) ELSE (t_ref_144.id IN (SELECT sub_211.col_1 FROM tbl_04 AS sub_211)) END), (TRUE > (SELECT (SELECT inn_214.col_2 FROM tbl_03 AS inn_214 LIMIT 1) FROM tbl_07 AS sub_213 LIMIT 1))))) IS NOT NULL)))) ELSE (t_ref_144.col_3 = LOWER(t_ref_144.id)) END), NULL)
LIMIT 37;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (479032, -1.5, '', '__fuzz_marker__', -2147483648, 9223372036854775807);

SELECT
    NULL AS c_216,
    CAST(((t_ref_215.col_1 IN (SELECT sub_217.id FROM tbl_05 AS sub_217)) > (SELECT (FALSE * sub_220.ref_tbl_01_id) FROM tbl_05 AS sub_220 LIMIT 1)) AS SIGNED) AS c_221,
    ((t_ref_215.id IN (SELECT sub_222.col_2 FROM tbl_02 AS sub_222)) >= COALESCE(NULLIF((CASE WHEN t_ref_215.col_1 THEN ABS((SELECT sub_239.ref_tbl_01_id FROM tbl_02 AS sub_239 LIMIT 1)) ELSE (t_ref_215.col_2 * (SELECT UPPER(((SELECT (SELECT inn_242.id FROM tbl_05 AS inn_242 LIMIT 1) FROM tbl_01 AS sub_241 LIMIT 1) = CAST((SELECT ((SELECT sub_244.ref_tbl_05_id FROM tbl_07 AS sub_244 LIMIT 1) OR CAST(sub_243.col_2 AS SIGNED)) FROM tbl_03 AS sub_243 LIMIT 1) AS CHAR))) FROM tbl_07 AS sub_240 LIMIT 1)) END), EXISTS (SELECT 1 FROM geofence_zones AS sub_245 WHERE sub_245.id)), (CASE WHEN t_ref_215.col_2 THEN CAST((CASE WHEN ((CASE WHEN LOWER(3.14159265) THEN EXISTS (SELECT 1 FROM tbl_02 AS sub_254 WHERE sub_254.col_4) ELSE LOWER(NULLIF(((SELECT sub_258.col_2 FROM tbl_05 AS sub_258 LIMIT 1) / NULLIF(EXISTS (SELECT 1 FROM tbl_06 AS sub_259 WHERE sub_259.id), 0)), t_ref_215.col_1)) END) AND (SELECT (CASE WHEN (SELECT sub_261.id FROM tbl_08 AS sub_261 LIMIT 1) THEN (UPPER(sub_260.col_2) OR (CASE WHEN CAST(EXISTS (SELECT 1 FROM tbl_05 AS sub_262 WHERE TRUE) AS CHAR) THEN (NOT ((LENGTH((SELECT NULL FROM tbl_08 AS sub_264 LIMIT 1)) - EXISTS (SELECT 1 FROM tbl_04 AS sub_265 WHERE sub_265.id)))) ELSE sub_260.col_1 END)) ELSE CAST(((EXISTS (SELECT 1 FROM tbl_08 AS sub_266 WHERE sub_266.col_2) - (EXISTS (SELECT 1 FROM tbl_09 AS sub_267 WHERE sub_267.ref_tbl_02_id) < ((-2147483648 BETWEEN -1 AND -2147483648) OR (sub_260.col_3 AND FALSE)))) > sub_260.col_2) AS CHAR) END) FROM tbl_01 AS sub_260 LIMIT 1)) THEN ((((t_ref_215.col_2 OR t_ref_215.col_1) * ((LOWER((t_ref_215.col_1 * CAST(((SELECT sub_268.id FROM geofence_zones AS sub_268 LIMIT 1) * t_ref_215.col_2) AS DECIMAL(10,2)))) + (CAST(FALSE AS SIGNED) * (NULLIF((TRUE < EXISTS (SELECT 1 FROM tbl_02 AS sub_269 WHERE -1)), (SELECT (SELECT inn_271.col_2 FROM tbl_03 AS inn_271 LIMIT 1) FROM geofence_zones AS sub_270 LIMIT 1)) * CAST(CAST(TRUE AS SIGNED) AS DECIMAL(10,2))))) + t_ref_215.id)) * (CASE WHEN t_ref_215.col_2 THEN UPPER(t_ref_215.col_3) ELSE (t_ref_215.col_2 IN (SELECT sub_272.id FROM tbl_08 AS sub_272)) END)) AND (((t_ref_215.col_3 / NULLIF(EXISTS (SELECT 1 FROM tbl_03 AS sub_276 WHERE sub_276.id), 0)) * '') < (CAST(CAST(COALESCE((((('chaos' AND t_ref_215.col_2) / NULLIF(t_ref_215.col_3, 0)) - CAST((SELECT sub_277.col_2 FROM tbl_01 AS sub_277 LIMIT 1) AS DECIMAL(10,2))) * (SELECT ABS(sub_278.col_3) FROM tbl_02 AS sub_278 LIMIT 1)), (CASE WHEN t_ref_215.id THEN (-2147483648 AND t_ref_215.col_2) ELSE (t_ref_215.id + (CASE WHEN CAST(t_ref_215.col_3 AS DECIMAL(10,2)) THEN (SELECT sub_279.col_3 FROM tbl_06 AS sub_279 LIMIT 1) ELSE (-1.5 OR t_ref_215.col_1) END)) END)) AS DECIMAL(10,2)) AS CHAR) <> t_ref_215.col_1))) ELSE (t_ref_215.col_1 = (SELECT (SELECT -9223372036854775808 FROM tbl_04 AS inn_283 LIMIT 1) FROM tbl_08 AS sub_280 LIMIT 1)) END) AS CHAR) ELSE (t_ref_215.id * t_ref_215.col_3) END))) AS c_284
FROM tbl_04 AS t_ref_215
WHERE ((CAST((TRUE - t_ref_215.id) AS DECIMAL(10,2)) + ((ABS(((SELECT (TRUE OR sub_285.col_4) FROM tbl_05 AS sub_285 LIMIT 1) BETWEEN 100 AND 9223372036854775807)) BETWEEN 1 AND 1) / NULLIF(t_ref_215.col_2, 0))) <> (t_ref_215.col_3 AND t_ref_215.id))
LIMIT 19;

INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_05_id) VALUES (576953, 0.0, 'escaped''quote', -1);

DELETE FROM tbl_07 WHERE id = 3645;

UPDATE tbl_01 SET col_1 = '{}' WHERE id > 66;

BEGIN;
INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_02_id) VALUES (1374, FALSE, 'chaos', 0);
COMMIT;

INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_05_id) VALUES (144641, -1.5, '', 9223372036854775807);

INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (204452, '{}', 100, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

UPDATE tbl_08 SET col_2 = '{}' WHERE id > 3;

DELETE FROM tbl_02 WHERE id = 93962;

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 JSON,
    col_3 INT,
    col_4 INT
);

DELETE FROM geofence_zones WHERE id = 12539;

SELECT
    CAST((t_ref_286.col_2 > EXISTS (SELECT 1 FROM tbl_01 AS sub_287 WHERE sub_287.col_2)) AS DECIMAL(10,2)) AS c_288,
    ((CAST(CAST(t_ref_286.col_1 AS DECIMAL(10,2)) AS CHAR) <= ((((UPPER(((NOT (t_ref_286.col_4)) > (CASE WHEN t_ref_286.id THEN t_ref_286.col_4 ELSE CAST((LENGTH((CASE WHEN UPPER(COALESCE(t_ref_286.col_4, t_ref_286.col_1)) THEN t_ref_286.col_4 ELSE CAST((NOT (t_ref_286.col_2)) AS CHAR) END)) < t_ref_286.id) AS DECIMAL(10,2)) END))) * (CAST(((UPPER((SELECT sub_289.id FROM tbl_06 AS sub_289 LIMIT 1)) <> t_ref_286.ref_tbl_01_id) BETWEEN 42 AND 100) AS DECIMAL(10,2)) < (-1e-5 IN (SELECT sub_290.ref_tbl_02_id FROM tbl_09 AS sub_290)))) / NULLIF(t_ref_286.ref_tbl_01_id, 0)) = (((UPPER(t_ref_286.col_1) IS NOT NULL) < CAST((((NOT ((CASE WHEN t_ref_286.col_1 THEN (SELECT (SELECT sub_292.id FROM tbl_04 AS sub_292 LIMIT 1) FROM tbl_02 AS sub_291 LIMIT 1) ELSE ((CASE WHEN CAST((t_ref_286.ref_tbl_01_id IN (SELECT sub_293.col_2 FROM tbl_08 AS sub_293)) AS CHAR) THEN (SELECT NULL FROM tbl_05 AS sub_294 LIMIT 1) ELSE EXISTS (SELECT 1 FROM geofence_zones AS sub_295 WHERE sub_295.id) END) <> (NULLIF((-1.5 + 'ref_val'), (CASE WHEN t_ref_286.col_2 THEN t_ref_286.col_2 ELSE TRUE END)) > t_ref_286.col_4)) END))) <= ('__fuzz_marker__' - CAST((CASE WHEN FALSE THEN t_ref_286.col_1 ELSE t_ref_286.ref_tbl_01_id END) AS DECIMAL(10,2)))) + UPPER((SELECT (SELECT inn_297.col_1 FROM tbl_09 AS inn_297 LIMIT 1) FROM tbl_02 AS sub_296 LIMIT 1))) AS DECIMAL(10,2))) + ((CAST((CASE WHEN (CASE WHEN ((SELECT ((SELECT (SELECT inn_300.ref_tbl_01_id FROM tbl_02 AS inn_300 LIMIT 1) FROM tbl_02 AS sub_299 LIMIT 1) / NULLIF(2147483647, 0)) FROM tbl_06 AS sub_298 LIMIT 1) >= NULLIF(EXISTS (SELECT 1 FROM tbl_01 AS sub_301 WHERE sub_301.id), (SELECT (SELECT inn_303.col_2 FROM tbl_01 AS inn_303 LIMIT 1) FROM tbl_02 AS sub_302 LIMIT 1))) THEN TRUE ELSE 'escaped''quote' END) THEN t_ref_286.col_1 ELSE (t_ref_286.col_1 < (CAST((SELECT (SELECT inn_305.col_1 FROM tbl_06 AS inn_305 LIMIT 1) FROM tbl_02 AS sub_304 LIMIT 1) AS DECIMAL(10,2)) + (t_ref_286.ref_tbl_01_id IN (SELECT sub_306.col_2 FROM tbl_04 AS sub_306)))) END) AS SIGNED) + (SELECT (SELECT 42 FROM tbl_02 AS inn_313 LIMIT 1) FROM tbl_08 AS sub_307 LIMIT 1)) / NULLIF(t_ref_286.col_4, 0)))) = t_ref_286.col_4)) * ((NOT (t_ref_286.col_1)) > ((CASE WHEN (FALSE IN (SELECT sub_314.col_3 FROM tbl_06 AS sub_314)) THEN ((((SELECT EXISTS (SELECT 1 FROM tbl_10 AS sub_316 WHERE 1.0) FROM tbl_01 AS sub_315 LIMIT 1) * EXISTS (SELECT 1 FROM tbl_06 AS sub_317 WHERE NULL)) / NULLIF(TRUE, 0)) >= t_ref_286.col_4) ELSE (CAST(UPPER((SELECT (SELECT 2147483647 FROM tbl_02 AS inn_320 LIMIT 1) FROM tbl_09 AS sub_319 LIMIT 1)) AS CHAR) - (-9223372036854775808 - 'escaped''quote')) END) < UPPER(NULLIF(t_ref_286.id, (CAST(CAST(((ABS(EXISTS (SELECT 1 FROM tbl_05 AS sub_321 WHERE NULL)) * CAST(((NOT (EXISTS (SELECT 1 FROM tbl_08 AS sub_322 WHERE sub_322.id))) / NULLIF((((NOT ('__fuzz_marker__')) / NULLIF(COALESCE(t_ref_286.ref_tbl_01_id, t_ref_286.ref_tbl_01_id), 0)) - t_ref_286.col_2), 0)) AS CHAR)) + ((t_ref_286.id IN (SELECT sub_323.col_1 FROM tbl_01 AS sub_323)) * t_ref_286.col_2)) AS CHAR) AS SIGNED) <= (CASE WHEN ('__fuzz_marker__' IN (SELECT sub_324.col_2 FROM tbl_09 AS sub_324)) THEN NULL ELSE t_ref_286.id END))))))) AS c_325
FROM tbl_05 AS t_ref_286
WHERE (SELECT (SELECT 'chaos' FROM geofence_zones AS inn_336 LIMIT 1) FROM tbl_05 AS sub_326 LIMIT 1)
LIMIT 21;

DELETE FROM tbl_08 WHERE id = 22126;

UPDATE tbl_09 SET col_1 = TRUE WHERE id > 85;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (490902, '[]', TRUE, -2147483648, 0.0000000001);

UPDATE tbl_03 SET col_3 = 'chaos' WHERE id > 57;

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2),
    col_3 BOOLEAN,
    ref_tbl_10_id INT,
    FOREIGN KEY (ref_tbl_10_id) REFERENCES tbl_10(id)
);

INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (112867, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -2147483648, '[]');

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_05 WHERE id IS NOT NULL;

UPDATE tbl_02 SET col_1 = 3.14159265 WHERE id > 4;

INSERT INTO tbl_05 (id, col_1, col_2, col_4, ref_tbl_01_id) VALUES (529522, 42, 9223372036854775807, '{}', 100);

ALTER TABLE tbl_06 DROP COLUMN col_4;

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (671128, '[]', -2147483648, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
INSERT INTO tbl_05 (id, col_1, col_2, col_4, ref_tbl_01_id) VALUES (402241, -1, 100, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 100);
INSERT INTO tbl_11 (id, col_1, col_2, col_3, ref_tbl_10_id) VALUES (371718, 'escaped''quote', -1e-5, FALSE, 0);
COMMIT;

UPDATE tbl_02 SET col_1 = 1.0 WHERE id > 11;

UPDATE tbl_04 SET col_1 = 999999999.99 WHERE id > 20;

SELECT
    t_ref_337.col_1 AS c_338,
    (((t_ref_337.ref_tbl_02_id IN (SELECT sub_339.id FROM geofence_zones AS sub_339)) - (t_ref_337.ref_tbl_02_id = NULLIF((LENGTH(t_ref_337.ref_tbl_02_id) * NULLIF((t_ref_337.ref_tbl_02_id IN (SELECT sub_349.col_1 FROM tbl_05 AS sub_349)), ((((t_ref_337.col_1 / NULLIF(TRUE, 0)) <= (((t_ref_337.col_2 < 0.0) / NULLIF((EXISTS (SELECT 1 FROM tbl_01 AS sub_350 WHERE 9223372036854775807) * t_ref_337.col_1), 0)) - (((CAST((t_ref_337.col_1 IS NOT NULL) AS CHAR) + t_ref_337.id) * FALSE) < (LENGTH(((SELECT (SELECT inn_352.col_3 FROM tbl_11 AS inn_352 LIMIT 1) FROM geofence_zones AS sub_351 LIMIT 1) - (SELECT 1e10 FROM tbl_01 AS sub_353 LIMIT 1))) >= (SELECT (SELECT inn_355.id FROM geofence_zones AS inn_355 LIMIT 1) FROM tbl_06 AS sub_354 LIMIT 1))))) IS NOT NULL) / NULLIF(FALSE, 0)))), (COALESCE(((SELECT (SELECT 1e10 FROM tbl_07 AS inn_360 LIMIT 1) FROM tbl_05 AS sub_356 LIMIT 1) + NULL), (EXISTS (SELECT 1 FROM tbl_09 AS sub_361 WHERE 1.0) <> (((999999999.99 OR t_ref_337.id) / NULLIF(CAST(9223372036854775807 AS DECIMAL(10,2)), 0)) - (SELECT (SELECT 9223372036854775807 FROM tbl_09 AS inn_366 LIMIT 1) FROM tbl_11 AS sub_362 LIMIT 1)))) BETWEEN 1 AND -1)))) IS NOT NULL) AS c_367
FROM tbl_09 AS t_ref_337 LEFT JOIN geofence_zones AS j_ref_368 ON t_ref_337.id = j_ref_368.id
WHERE ((ABS((t_ref_337.col_1 / NULLIF(EXISTS (SELECT 1 FROM tbl_10 AS sub_369 WHERE sub_369.col_3), 0))) / NULLIF(CAST(ABS(ABS((SELECT LOWER(0.0) FROM tbl_05 AS sub_370 LIMIT 1))) AS SIGNED), 0)) BETWEEN 9223372036854775807 AND 42)
LIMIT 8;

SELECT
    t_ref_371.ref_tbl_01_id AS c_372,
    (CASE WHEN LOWER((SELECT (EXISTS (SELECT 1 FROM tbl_01 AS sub_374 WHERE sub_374.col_1) - (SELECT EXISTS (SELECT 1 FROM tbl_05 AS sub_377 WHERE 0.0000000001) FROM tbl_09 AS sub_376 LIMIT 1)) FROM tbl_09 AS sub_373 LIMIT 1)) THEN (SELECT (SELECT inn_383.col_3 FROM tbl_11 AS inn_383 LIMIT 1) FROM geofence_zones AS sub_378 LIMIT 1) ELSE COALESCE(EXISTS (SELECT 1 FROM tbl_02 AS sub_384 WHERE '__fuzz_marker__'), ((CASE WHEN (LENGTH((SELECT (SELECT inn_392.col_2 FROM tbl_07 AS inn_392 LIMIT 1) FROM tbl_01 AS sub_387 LIMIT 1)) * ((CAST(LENGTH((NOT ((SELECT ((CASE WHEN (SELECT NULL FROM tbl_11 AS sub_394 LIMIT 1) THEN (sub_393.col_4 / NULLIF(sub_393.col_4, 0)) ELSE (sub_393.id - sub_393.col_2) END) / NULLIF(EXISTS (SELECT 1 FROM tbl_06 AS sub_395 WHERE sub_395.id), 0)) FROM tbl_10 AS sub_393 LIMIT 1)))) AS CHAR) AND (t_ref_371.col_3 IS NULL)) - CAST(COALESCE((t_ref_371.ref_tbl_01_id * t_ref_371.col_1), ((t_ref_371.col_1 = (((t_ref_371.ref_tbl_01_id / NULLIF(((t_ref_371.col_4 * FALSE) + (t_ref_371.id BETWEEN 42 AND -9223372036854775808)), 0)) < CAST((ABS(t_ref_371.ref_tbl_01_id) + (TRUE >= t_ref_371.id)) AS DECIMAL(10,2))) / NULLIF(-9223372036854775808, 0))) * '__fuzz_marker__')) AS DECIMAL(10,2)))) THEN NULL ELSE (CASE WHEN ((SELECT (CASE WHEN (NOT ((sub_396.col_2 = (NOT (sub_396.col_2))))) THEN (((EXISTS (SELECT 1 FROM tbl_01 AS sub_397 WHERE sub_397.col_2) / NULLIF(ABS(sub_396.col_1), 0)) - CAST(((SELECT (SELECT inn_399.col_2 FROM tbl_05 AS inn_399 LIMIT 1) FROM tbl_08 AS sub_398 LIMIT 1) < (sub_396.id IN (SELECT sub_400.col_2 FROM tbl_02 AS sub_400))) AS DECIMAL(10,2))) + (((CAST(NULL AS SIGNED) * LOWER(CAST(sub_396.col_1 AS CHAR))) + CAST(EXISTS (SELECT 1 FROM tbl_05 AS sub_401 WHERE NULL) AS CHAR)) > UPPER(LENGTH(EXISTS (SELECT 1 FROM tbl_07 AS sub_402 WHERE sub_402.id))))) ELSE ((-1.5 IN (SELECT sub_403.id FROM tbl_07 AS sub_403)) * (CASE WHEN sub_396.col_3 THEN (sub_396.col_2 >= EXISTS (SELECT 1 FROM tbl_07 AS sub_404 WHERE sub_404.col_1)) ELSE (SELECT ((SELECT 999999999.99 FROM tbl_05 AS sub_406 LIMIT 1) <= sub_405.col_1) FROM tbl_04 AS sub_405 LIMIT 1) END)) END) FROM tbl_06 AS sub_396 LIMIT 1) <= t_ref_371.id) THEN (CAST((SELECT (SELECT inn_410.col_1 FROM tbl_06 AS inn_410 LIMIT 1) FROM tbl_01 AS sub_407 LIMIT 1) AS CHAR) <> COALESCE(t_ref_371.ref_tbl_01_id, (CAST((((SELECT (SELECT 'chaos' FROM tbl_04 AS inn_413 LIMIT 1) FROM tbl_11 AS sub_411 LIMIT 1) = NULL) - ((FALSE IN (SELECT sub_414.col_2 FROM tbl_10 AS sub_414)) BETWEEN 2147483647 AND 2147483647)) AS SIGNED) OR (LENGTH(((CAST(t_ref_371.col_1 AS CHAR) AND FALSE) / NULLIF(LENGTH((t_ref_371.id BETWEEN -1 AND 42)), 0))) > (TRUE <> (SELECT COALESCE((SELECT sub_416.col_2 FROM tbl_08 AS sub_416 LIMIT 1), CAST((sub_415.ref_tbl_01_id <> sub_415.col_2) AS CHAR)) FROM tbl_05 AS sub_415 LIMIT 1)))))) ELSE CAST(((CASE WHEN NULLIF(LOWER(CAST(LOWER(EXISTS (SELECT 1 FROM tbl_08 AS sub_417 WHERE 'chaos')) AS SIGNED)), (SELECT (SELECT inn_420.col_3 FROM tbl_03 AS inn_420 LIMIT 1) FROM tbl_05 AS sub_418 LIMIT 1)) THEN (((SELECT (SELECT NULL FROM tbl_04 AS inn_423 LIMIT 1) FROM tbl_09 AS sub_421 LIMIT 1) - ABS(CAST(CAST(COALESCE((2147483647 IN (SELECT sub_424.payload FROM geofence_zones AS sub_424)), (CASE WHEN t_ref_371.col_3 THEN t_ref_371.col_2 ELSE t_ref_371.col_1 END)) AS DECIMAL(10,2)) AS DECIMAL(10,2)))) > EXISTS (SELECT 1 FROM tbl_07 AS sub_425 WHERE FALSE)) ELSE (COALESCE((t_ref_371.col_3 IN (SELECT sub_427.col_3 FROM tbl_04 AS sub_427)), (SELECT EXISTS (SELECT 1 FROM geofence_zones AS sub_429 WHERE 1.0) FROM tbl_07 AS sub_428 LIMIT 1)) + (((NULLIF((t_ref_371.col_2 + (42 = TRUE)), ((NULL - t_ref_371.ref_tbl_01_id) + (SELECT (SELECT inn_431.col_3 FROM tbl_03 AS inn_431 LIMIT 1) FROM tbl_11 AS sub_430 LIMIT 1))) - CAST(t_ref_371.ref_tbl_01_id AS DECIMAL(10,2))) + (SELECT CAST(FALSE AS DECIMAL(10,2)) FROM tbl_01 AS sub_432 LIMIT 1)) <> (t_ref_371.id OR (CAST((t_ref_371.col_2 IN (SELECT sub_433.col_3 FROM tbl_10 AS sub_433)) AS SIGNED) * t_ref_371.col_2)))) END) + COALESCE((NOT (t_ref_371.col_4)), (CASE WHEN FALSE THEN ((EXISTS (SELECT 1 FROM tbl_05 AS sub_434 WHERE sub_434.id) + (CAST((SELECT (SELECT 9223372036854775807 FROM geofence_zones AS inn_437 LIMIT 1) FROM tbl_05 AS sub_436 LIMIT 1) AS CHAR) > t_ref_371.id)) BETWEEN -1 AND 2147483647) ELSE (1.0 AND (LOWER((t_ref_371.col_4 > ((t_ref_371.ref_tbl_01_id < '__fuzz_marker__') < t_ref_371.col_1))) * EXISTS (SELECT 1 FROM tbl_11 AS sub_438 WHERE sub_438.col_3))) END))) AS CHAR) END) END) OR -2147483648)) END) AS c_441,
    (3.14159265 IN (SELECT sub_442.col_1 FROM tbl_09 AS sub_442)) AS c_443
FROM tbl_02 AS t_ref_371
WHERE t_ref_371.col_2
LIMIT 28;

CREATE INDEX idx_tbl_02_col_1_2 ON tbl_02 (col_1);

INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (908286, '[]', 1, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

CREATE VIEW v_summary_6 AS SELECT id FROM tbl_01 WHERE id IS NOT NULL;

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2)
);

INSERT INTO geofence_zones (id, payload) VALUES (190319, 'chaos');

SELECT
    (SELECT (SELECT inn_450.payload FROM geofence_zones AS inn_450 LIMIT 1) FROM tbl_01 AS sub_445 LIMIT 1) AS c_451,
    CAST((CAST((NOT (3.14159265)) AS SIGNED) >= t_ref_444.id) AS CHAR) AS c_452,
    (CASE WHEN (t_ref_444.col_3 IN (SELECT sub_453.col_1 FROM tbl_10 AS sub_453)) THEN (((9223372036854775807 IN (SELECT sub_476.col_3 FROM tbl_06 AS sub_476)) * ((((CASE WHEN UPPER((SELECT (SELECT 3.14159265 FROM tbl_01 AS inn_480 LIMIT 1) FROM tbl_03 AS sub_477 LIMIT 1)) THEN t_ref_444.col_2 ELSE t_ref_444.col_2 END) + (((t_ref_444.id OR ((UPPER(((SELECT -1 FROM geofence_zones AS sub_481 LIMIT 1) - (CASE WHEN (SELECT (SELECT 3.14159265 FROM tbl_04 AS inn_483 LIMIT 1) FROM tbl_09 AS sub_482 LIMIT 1) THEN t_ref_444.col_2 ELSE (t_ref_444.col_3 + '') END))) / NULLIF(((((t_ref_444.col_2 OR t_ref_444.col_2) - t_ref_444.col_2) IS NOT NULL) OR (FALSE IN (SELECT sub_484.col_2 FROM tbl_03 AS sub_484))), 0)) + NULL)) / NULLIF((EXISTS (SELECT 1 FROM tbl_07 AS sub_486 WHERE sub_486.id) / NULLIF(((UPPER(('chaos' OR (t_ref_444.col_1 BETWEEN 1 AND 9223372036854775807))) = LENGTH(LOWER(t_ref_444.col_2))) - t_ref_444.col_1), 0)), 0)) >= (EXISTS (SELECT 1 FROM tbl_09 AS sub_487 WHERE sub_487.id) - (((SELECT (SELECT inn_489.col_2 FROM tbl_01 AS inn_489 LIMIT 1) FROM tbl_08 AS sub_488 LIMIT 1) >= t_ref_444.id) * (((CASE WHEN t_ref_444.col_3 THEN t_ref_444.col_2 ELSE ((EXISTS (SELECT 1 FROM tbl_09 AS sub_490 WHERE sub_490.col_1) * FALSE) > CAST(ABS(NULL) AS CHAR)) END) - EXISTS (SELECT 1 FROM tbl_04 AS sub_491 WHERE sub_491.col_3)) * ((CAST((NOT ((t_ref_444.col_1 + t_ref_444.col_3))) AS CHAR) * t_ref_444.col_4) = 3.14159265)))))) / NULLIF((t_ref_444.col_3 - (t_ref_444.col_1 IN (SELECT sub_492.ref_tbl_01_id FROM tbl_05 AS sub_492))), 0)) BETWEEN 100 AND 9223372036854775807)) + ((t_ref_444.col_2 IN (SELECT sub_495.id FROM tbl_01 AS sub_495)) + NULLIF(EXISTS (SELECT 1 FROM tbl_03 AS sub_496 WHERE TRUE), t_ref_444.col_4))) ELSE (t_ref_444.col_3 IN (SELECT sub_497.col_1 FROM tbl_12 AS sub_497)) END) AS c_506
FROM tbl_10 AS t_ref_444
WHERE CAST(t_ref_444.col_1 AS DECIMAL(10,2))
LIMIT 19;

UPDATE tbl_03 SET col_1 = '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}' WHERE id > 1;

UPDATE tbl_11 SET col_3 = FALSE WHERE id > 74;

CREATE VIEW v_summary_7 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

DELETE FROM geofence_zones WHERE id = 96247;

CREATE VIEW v_summary_8 AS SELECT id FROM tbl_09 WHERE id IS NOT NULL;

UPDATE geofence_zones SET payload = 'alpha_beta_gamma' WHERE id > 25;

INSERT INTO tbl_11 (id, col_1, col_2, col_3, ref_tbl_10_id) VALUES (79189, '', -1e-5, FALSE, -1);

INSERT INTO tbl_08 (id, col_1, col_2) VALUES (696212, 3.14159265, '[]');

BEGIN;
INSERT INTO geofence_zones (id, payload) VALUES (577570, '__fuzz_marker__');
INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4) VALUES (614623, FALSE, '[]', -2147483648, 2147483647);
INSERT INTO geofence_zones (id, payload) VALUES (332164, 'alpha_beta_gamma');
COMMIT;

CREATE INDEX idx_tbl_03_col_3_3 ON tbl_03 (col_3);

INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (73671, '[]', TRUE, 100);

CREATE INDEX idx_tbl_12_col_2_4 ON tbl_12 (col_2);

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2) VALUES (511176, 0.0, '{}');
INSERT INTO tbl_12 (id, col_1, col_2) VALUES (431234, 1, 3.14159265);
INSERT INTO tbl_08 (id, col_1, col_2) VALUES (171934, -1e-5, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN
);

DELETE FROM tbl_04 WHERE id = 45383;

ALTER TABLE tbl_03 DROP COLUMN col_2;

DELETE FROM tbl_08 WHERE id = 18104;

INSERT INTO tbl_12 (id, col_1, col_2) VALUES (602173, 100, 1.0);

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (133797, -1e-5, '', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4) VALUES (79825, FALSE, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 9223372036854775807, 100);
INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (817707, -1.5, 'alpha_beta_gamma', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (176058, -1.5, 'alpha_beta_gamma', '{}');

ALTER TABLE tbl_06 ADD COLUMN dyn_col_5 VARCHAR(32);

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSON,
    col_3 VARCHAR(64),
    ref_tbl_05_id INT,
    FOREIGN KEY (ref_tbl_05_id) REFERENCES tbl_05(id)
);

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN
);

SELECT
    (EXISTS (SELECT 1 FROM tbl_13 AS sub_508 WHERE 2147483647) = t_ref_507.col_1) AS c_509,
    ((t_ref_507.id BETWEEN -1 AND -9223372036854775808) * ((SELECT (SELECT inn_519.col_2 FROM tbl_02 AS inn_519 LIMIT 1) FROM tbl_14 AS sub_510 LIMIT 1) + ((NOT (t_ref_507.id)) * NULL))) AS c_520,
    NULLIF((SELECT (SELECT TRUE FROM tbl_14 AS inn_522 LIMIT 1) FROM tbl_07 AS sub_521 LIMIT 1), t_ref_507.id) AS c_523
FROM tbl_04 AS t_ref_507 LEFT JOIN tbl_11 AS j_ref_524 ON t_ref_507.id = j_ref_524.id
WHERE (t_ref_507.col_3 <= (t_ref_507.col_3 IN (SELECT sub_525.col_2 FROM tbl_13 AS sub_525)))
LIMIT 19;

INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_02_id) VALUES (554035, TRUE, 'chaos', 1);

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN,
    ref_tbl_06_id INT,
    FOREIGN KEY (ref_tbl_06_id) REFERENCES tbl_06(id)
);

CREATE TABLE tbl_17 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSON
);

SELECT
    t_ref_527.col_1 AS c_528,
    t_ref_527.id AS c_529,
    EXISTS (SELECT 1 FROM tbl_13 AS sub_530 WHERE sub_530.col_2) AS c_561
FROM tbl_17 AS t_ref_527 LEFT JOIN tbl_03 AS j_ref_562 ON t_ref_527.id = j_ref_562.id
WHERE (t_ref_527.col_2 <= ((SELECT (SELECT inn_569.col_1 FROM tbl_17 AS inn_569 LIMIT 1) FROM tbl_07 AS sub_563 LIMIT 1) > TRUE))
LIMIT 6;

BEGIN;
INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_02_id) VALUES (743808, FALSE, 'chaos', -2147483648);
INSERT INTO tbl_14 (id, col_1, col_2, col_3, ref_tbl_05_id) VALUES (734701, 'escaped''quote', '[]', '', -1);
INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (261885, 0.0000000001, 'chaos', '{}');
COMMIT;

SELECT
    t_ref_570.col_1 AS c_571,
    (((t_ref_570.col_2 IN (SELECT sub_572.col_2 FROM tbl_10 AS sub_572)) AND CAST(((t_ref_570.id IN (SELECT sub_573.col_2 FROM tbl_15 AS sub_573)) * (t_ref_570.ref_tbl_05_id IN (SELECT sub_574.ref_tbl_01_id FROM tbl_05 AS sub_574))) AS CHAR)) + COALESCE((SELECT (SELECT inn_578.ref_tbl_06_id FROM tbl_16 AS inn_578 LIMIT 1) FROM tbl_09 AS sub_575 LIMIT 1), (SELECT (SELECT inn_586.id FROM tbl_10 AS inn_586 LIMIT 1) FROM tbl_06 AS sub_579 LIMIT 1))) AS c_587,
    (TRUE IN (SELECT sub_588.ref_tbl_06_id FROM tbl_16 AS sub_588)) AS c_619
FROM tbl_14 AS t_ref_570
WHERE (((CASE WHEN t_ref_570.ref_tbl_05_id THEN t_ref_570.col_2 ELSE (t_ref_570.col_1 / NULLIF(CAST(LENGTH((SELECT (SELECT inn_624.col_2 FROM tbl_15 AS inn_624 LIMIT 1) FROM tbl_10 AS sub_620 LIMIT 1)) AS DECIMAL(10,2)), 0)) END) + UPPER(CAST(t_ref_570.col_2 AS SIGNED))) * TRUE)
LIMIT 39;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (708400, -1.5, '__fuzz_marker__', 'escaped''quote', 42, 42);

CREATE TABLE geofence_zones (
    zone_id INT PRIMARY KEY,
    zone_name VARCHAR(64) NOT NULL,
    boundary GEOMETRY NOT NULL /*!80003 SRID 0 */,
    SPATIAL INDEX idx_geofence_boundary (boundary)
) ENGINE=InnoDB;

SELECT
    zone_id,
    zone_name,
    ST_AsText(boundary) AS boundary_wkt
FROM geofence_zones
WHERE MBRIntersects(boundary, ST_GeomFromText('POLYGON((4 4, 4 6, 6 6, 6 4, 4 4))'));

UPDATE tbl_04 SET col_1 = 1e10 WHERE id > 37;

CREATE TABLE tbl_18 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2),
    col_4 JSON,
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

INSERT INTO tbl_11 (id, col_1, col_2, col_3, ref_tbl_10_id) VALUES (362031, '', 3.14159265, FALSE, -2147483648);

DELETE FROM tbl_07 WHERE id = 97706;

INSERT INTO tbl_14 (id, col_1, col_2, col_3, ref_tbl_05_id) VALUES (17645, '__fuzz_marker__', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'escaped''quote', 0);

INSERT INTO tbl_17 (id, col_1, col_2) VALUES (421317, -1, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

CREATE TABLE tbl_19 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 VARCHAR(64),
    col_3 JSON,
    col_4 INT
);

DELETE FROM tbl_19 WHERE id = 37903;

CREATE TABLE tbl_20 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 INT,
    col_3 JSON,
    col_4 INT
);

DELETE FROM geofence_zones WHERE id = 93240;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, dyn_col_5) VALUES (528938, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', TRUE, 9223372036854775807, 'escaped''quote');

BEGIN;
INSERT INTO geofence_zones (id, payload) VALUES (456762, 'escaped''quote');
COMMIT;

CREATE TABLE tbl_21 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2),
    ref_tbl_09_id INT,
    FOREIGN KEY (ref_tbl_09_id) REFERENCES tbl_09(id)
);

INSERT INTO tbl_17 (id, col_1, col_2) VALUES (872215, -9223372036854775808, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

SELECT
    COALESCE((t_ref_625.ref_tbl_01_id < (t_ref_625.id IN (SELECT sub_626.id FROM tbl_12 AS sub_626))), (SELECT (sub_627.id BETWEEN 2147483647 AND 2147483647) FROM tbl_08 AS sub_627 LIMIT 1)) AS c_628,
    CAST(t_ref_625.id AS DECIMAL(10,2)) AS c_629
FROM tbl_02 AS t_ref_625
WHERE NULLIF((SELECT NULLIF((SELECT (SELECT 'chaos' FROM tbl_13 AS inn_632 LIMIT 1) FROM tbl_11 AS sub_631 LIMIT 1), (sub_630.ref_tbl_02_id = sub_630.col_2)) FROM tbl_09 AS sub_630 LIMIT 1), (SELECT (SELECT inn_640.col_1 FROM tbl_03 AS inn_640 LIMIT 1) FROM tbl_19 AS sub_633 LIMIT 1))
LIMIT 20;

INSERT INTO tbl_18 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (693102, FALSE, 'alpha_beta_gamma', 0.0, '{}', -9223372036854775808);

INSERT INTO tbl_06 (id, col_1, col_2, col_3, dyn_col_5) VALUES (531950, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', TRUE, 2147483647, 'ref_val');

INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4) VALUES (276284, FALSE, '[]', 42, 9223372036854775807);

SELECT
    ((CASE WHEN TRUE THEN LOWER(LENGTH((TRUE / NULLIF(NULLIF(t_ref_641.id, (SELECT (SELECT inn_643.col_1 FROM tbl_17 AS inn_643 LIMIT 1) FROM tbl_09 AS sub_642 LIMIT 1)), 0)))) ELSE (CASE WHEN ((((NOT (((t_ref_641.col_1 >= (100 IN (SELECT sub_644.id FROM tbl_16 AS sub_644))) > (NULLIF(t_ref_641.id, t_ref_641.col_2) + LENGTH(ABS(t_ref_641.ref_tbl_05_id)))))) + CAST((SELECT 'alpha_beta_gamma' FROM tbl_06 AS sub_645 LIMIT 1) AS SIGNED)) >= (((SELECT (SELECT FALSE FROM tbl_11 AS inn_647 LIMIT 1) FROM tbl_13 AS sub_646 LIMIT 1) IS NULL) <= (SELECT (SELECT 1.0 FROM tbl_06 AS inn_649 LIMIT 1) FROM tbl_21 AS sub_648 LIMIT 1))) IS NOT NULL) THEN t_ref_641.col_1 ELSE ((UPPER((CASE WHEN EXISTS (SELECT 1 FROM tbl_11 AS sub_650 WHERE sub_650.ref_tbl_10_id) THEN LOWER(ABS((CAST(t_ref_641.id AS DECIMAL(10,2)) < (t_ref_641.id IN (SELECT sub_655.col_1 FROM tbl_07 AS sub_655))))) ELSE COALESCE(((SELECT (NOT (LENGTH(sub_656.col_1))) FROM tbl_03 AS sub_656 LIMIT 1) AND CAST(-2147483648 AS DECIMAL(10,2))), EXISTS (SELECT 1 FROM tbl_06 AS sub_657 WHERE FALSE)) END)) >= NULLIF((COALESCE(((CASE WHEN t_ref_641.id THEN (t_ref_641.col_2 IS NOT NULL) ELSE (ABS((ABS(CAST(t_ref_641.col_2 AS SIGNED)) <= t_ref_641.ref_tbl_05_id)) / NULLIF(((NOT (((SELECT (SELECT inn_661.col_2 FROM tbl_12 AS inn_661 LIMIT 1) FROM tbl_15 AS sub_660 LIMIT 1) + FALSE))) BETWEEN 2147483647 AND 1), 0)) END) < t_ref_641.id), LENGTH(t_ref_641.ref_tbl_05_id)) - NULL), CAST((t_ref_641.col_1 = UPPER(((CASE WHEN LENGTH(CAST(((CASE WHEN FALSE THEN t_ref_641.col_1 ELSE t_ref_641.id END) BETWEEN -2147483648 AND 100) AS SIGNED)) THEN ('alpha_beta_gamma' IN (SELECT sub_662.id FROM tbl_21 AS sub_662)) ELSE (SELECT (SELECT sub_664.id FROM tbl_14 AS sub_664 LIMIT 1) FROM tbl_12 AS sub_663 LIMIT 1) END) OR (SELECT (SELECT inn_667.col_1 FROM tbl_01 AS inn_667 LIMIT 1) FROM tbl_02 AS sub_665 LIMIT 1)))) AS SIGNED))) BETWEEN 9223372036854775807 AND 9223372036854775807) END) END) > (TRUE = ABS(-9223372036854775808))) AS c_668
FROM tbl_07 AS t_ref_641
WHERE (CAST((SELECT (1 + CAST(((sub_669.col_3 / NULLIF(LOWER((SELECT ((SELECT (SELECT inn_672.col_1 FROM tbl_03 AS inn_672 LIMIT 1) FROM tbl_09 AS sub_671 LIMIT 1) * (sub_670.col_4 + NULLIF(TRUE, ('escaped''quote' OR 1)))) FROM tbl_18 AS sub_670 LIMIT 1)), 0)) < 3.14159265) AS CHAR)) FROM tbl_04 AS sub_669 LIMIT 1) AS SIGNED) <= (((SELECT (TRUE IN (SELECT sub_674.ref_tbl_09_id FROM tbl_21 AS sub_674)) FROM geofence_zones AS sub_673 LIMIT 1) <> (t_ref_641.col_1 / NULLIF((SELECT (SELECT '__fuzz_marker__' FROM tbl_01 AS inn_692 LIMIT 1) FROM tbl_03 AS sub_676 LIMIT 1), 0))) >= FALSE))
LIMIT 43;

CREATE TABLE tbl_22 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 INT,
    col_4 BOOLEAN,
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

ALTER TABLE tbl_08 DROP COLUMN col_1;

UPDATE tbl_08 SET col_2 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 45;

CREATE INDEX idx_tbl_14_col_1_5 ON tbl_14 (col_1);

INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (166811, -1e-5, 'chaos', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

UPDATE tbl_20 SET col_1 = '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}' WHERE id > 97;

CREATE TABLE tbl_23 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 BOOLEAN,
    ref_tbl_16_id INT,
    FOREIGN KEY (ref_tbl_16_id) REFERENCES tbl_16(id)
);

BEGIN;
INSERT INTO tbl_11 (id, col_1, col_2, col_3, ref_tbl_10_id) VALUES (979412, 'chaos', -1e-5, TRUE, 2147483647);
INSERT INTO tbl_05 (id, col_1, col_2, col_4, ref_tbl_01_id) VALUES (159721, 42, 9223372036854775807, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -2147483648);
COMMIT;

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (454966, 1e10, 'ref_val', '', -2147483648, 42);
COMMIT;

INSERT INTO tbl_11 (id, col_1, col_2, col_3, ref_tbl_10_id) VALUES (771986, 'ref_val', 0.0000000001, FALSE, 42);

SELECT
    t_ref_693.col_2 AS c_694
FROM tbl_11 AS t_ref_693
WHERE LENGTH((EXISTS (SELECT 1 FROM tbl_12 AS sub_695 WHERE sub_695.col_1) IS NOT NULL))
LIMIT 20;

CREATE INDEX idx_tbl_22_col_2_6 ON tbl_22 (col_2);

CREATE TABLE tbl_24 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 INT,
    col_4 VARCHAR(64),
    ref_tbl_13_id INT,
    FOREIGN KEY (ref_tbl_13_id) REFERENCES tbl_13(id)
);

INSERT INTO tbl_20 (id, col_1, col_2, col_3, col_4) VALUES (279060, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 0, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 42);

UPDATE tbl_13 SET col_2 = FALSE WHERE id > 22;

DELETE FROM tbl_09 WHERE id = 48286;

CREATE TABLE tbl_25 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2)
);

CREATE INDEX idx_tbl_07_col_2_7 ON tbl_07 (col_2);

UPDATE tbl_13 SET col_2 = FALSE WHERE id > 45;

INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_05_id) VALUES (14018, 0.0000000001, '', -2147483648);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (262899, 0.0000000001, 'alpha_beta_gamma', 'alpha_beta_gamma', 1, 0);

UPDATE tbl_16 SET ref_tbl_06_id = 1 WHERE id > 45;

CREATE VIEW v_summary_9 AS SELECT id FROM tbl_11 WHERE id IS NOT NULL;

UPDATE tbl_09 SET col_1 = TRUE WHERE id > 44;

SELECT
    ((CASE WHEN (UPPER(t_ref_700.col_1) * (COALESCE(((SELECT (SELECT inn_710.id FROM tbl_17 AS inn_710 LIMIT 1) FROM tbl_17 AS sub_701 LIMIT 1) - CAST((-1 - CAST(t_ref_700.id AS CHAR)) AS SIGNED)), t_ref_700.col_3) / NULLIF(EXISTS (SELECT 1 FROM tbl_09 AS sub_711 WHERE sub_711.ref_tbl_02_id), 0))) THEN (SELECT (CAST(LENGTH(((sub_712.col_2 IN (SELECT sub_713.col_1 FROM tbl_14 AS sub_713)) < ((SELECT ((SELECT sub_715.id FROM tbl_18 AS sub_715 LIMIT 1) < (SELECT (SELECT TRUE FROM tbl_20 AS inn_717 LIMIT 1) FROM tbl_06 AS sub_716 LIMIT 1)) FROM tbl_19 AS sub_714 LIMIT 1) - TRUE))) AS DECIMAL(10,2)) / NULLIF((SELECT sub_718.id FROM tbl_09 AS sub_718 LIMIT 1), 0)) FROM tbl_23 AS sub_712 LIMIT 1) ELSE (LOWER((t_ref_700.id <> (SELECT ((sub_719.ref_tbl_16_id OR ((SELECT NULLIF(((sub_720.id = sub_720.dyn_col_5) + sub_720.id), CAST((sub_720.id >= sub_720.col_2) AS CHAR)) FROM tbl_06 AS sub_720 LIMIT 1) - EXISTS (SELECT 1 FROM tbl_19 AS sub_721 WHERE sub_721.id))) BETWEEN 0 AND 42) FROM tbl_23 AS sub_719 LIMIT 1))) <= (SELECT (9223372036854775807 - (EXISTS (SELECT 1 FROM tbl_15 AS sub_724 WHERE sub_724.col_2) - (sub_723.col_1 / NULLIF(EXISTS (SELECT 1 FROM tbl_18 AS sub_727 WHERE sub_727.col_1), 0)))) FROM tbl_14 AS sub_723 LIMIT 1)) END) IS NOT NULL) AS c_732,
    EXISTS (SELECT 1 FROM tbl_18 AS sub_733 WHERE NULL) AS c_739,
    LOWER((t_ref_700.col_3 IN (SELECT sub_740.col_2 FROM tbl_23 AS sub_740))) AS c_747
FROM tbl_21 AS t_ref_700
WHERE ((3.14159265 IN (SELECT sub_748.col_1 FROM tbl_22 AS sub_748)) + (SELECT (SELECT inn_762.col_4 FROM tbl_19 AS inn_762 LIMIT 1) FROM tbl_24 AS sub_750 LIMIT 1))
LIMIT 35;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (149671, 999999999.99, 'chaos', '__fuzz_marker__', 100, 9223372036854775807);
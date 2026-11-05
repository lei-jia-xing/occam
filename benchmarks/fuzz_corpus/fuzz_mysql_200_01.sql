-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: MYSQL

-- Statements: 200

-- AST Expression Depth: 18

-- Injected Defect Case: mysql_10

-- Seed: 20021



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSON,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2),
    col_3 INT,
    col_4 JSON
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 INT,
    col_4 VARCHAR(64)
);

DELETE FROM tbl_01 WHERE id = 33738;

CREATE TABLE asset_meta (
    meta_id INT PRIMARY KEY,
    category VARCHAR(32) NOT NULL,
    blob_chunk BLOB NOT NULL
);

INSERT INTO asset_meta (id, payload) VALUES (690510, 'chaos');

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

UPDATE asset_meta SET payload = 'alpha_beta_gamma' WHERE id > 1;

UPDATE tbl_03 SET col_2 = 0.0 WHERE id > 89;

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (515498, '', 'alpha_beta_gamma', 0, 'ref_val');

INSERT INTO asset_meta (id, payload) VALUES (777859, 'chaos');

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (257895, '__fuzz_marker__', -1.5, 0, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

CREATE INDEX idx_asset_meta_id_1 ON asset_meta (id);

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (828986, 'chaos', '', 42, '__fuzz_marker__');

UPDATE tbl_04 SET col_1 = 'ref_val' WHERE id > 47;

ALTER TABLE tbl_02 DROP COLUMN col_2;

ALTER TABLE tbl_01 DROP COLUMN col_2;

INSERT INTO asset_meta (id, payload) VALUES (21982, 'chaos');

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (718684, '__fuzz_marker__', 'alpha_beta_gamma', 100, 'alpha_beta_gamma');

CREATE INDEX idx_tbl_02_id_2 ON tbl_02 (id);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSON,
    col_3 INT
);

SELECT
    (-1e-5 OR '__fuzz_marker__') AS c_2,
    ((EXISTS (SELECT 1 FROM tbl_04 AS sub_3 WHERE '') / NULLIF((CAST(((NOT ((t_ref_1.col_4 / NULLIF(((LOWER(UPPER((SELECT ((((sub_47.col_3 <> sub_47.col_4) < (42 < sub_47.id)) - CAST((sub_47.col_4 - sub_47.id) AS DECIMAL(10,2))) - NULL) FROM tbl_04 AS sub_47 LIMIT 1))) - t_ref_1.col_1) <> (SELECT EXISTS (SELECT 1 FROM tbl_03 AS sub_49 WHERE sub_49.id) FROM asset_meta AS sub_48 LIMIT 1)), 0)))) < t_ref_1.col_4) AS CHAR) / NULLIF(EXISTS (SELECT 1 FROM tbl_01 AS sub_50 WHERE TRUE), 0)), 0)) <> (NOT (((SELECT LOWER(CAST((((EXISTS (SELECT 1 FROM tbl_03 AS sub_77 WHERE sub_77.id) - (EXISTS (SELECT 1 FROM asset_meta AS sub_78 WHERE sub_78.id) * (SELECT ((SELECT (SELECT (SELECT NULL FROM tbl_04 AS inn_83 LIMIT 1) FROM tbl_03 AS sub_82 LIMIT 1) FROM tbl_03 AS sub_81 LIMIT 1) <= (SELECT ABS(sub_84.col_3) FROM tbl_01 AS sub_84 LIMIT 1)) FROM tbl_03 AS sub_80 LIMIT 1))) - COALESCE(sub_76.id, (((SELECT (sub_85.ref_tbl_01_id OR ((sub_85.col_1 BETWEEN 2147483647 AND 0) - (sub_85.col_1 BETWEEN 100 AND -9223372036854775808))) FROM tbl_02 AS sub_85 LIMIT 1) >= EXISTS (SELECT 1 FROM tbl_01 AS sub_86 WHERE sub_86.id)) * (sub_76.col_3 IN (SELECT sub_88.id FROM asset_meta AS sub_88))))) <> (SELECT (SELECT inn_91.col_1 FROM tbl_02 AS inn_91 LIMIT 1) FROM tbl_05 AS sub_89 LIMIT 1)) AS DECIMAL(10,2))) FROM tbl_05 AS sub_76 LIMIT 1) <= CAST(EXISTS (SELECT 1 FROM asset_meta AS sub_92 WHERE 0) AS DECIMAL(10,2)))))) AS c_101
FROM tbl_04 AS t_ref_1
WHERE FALSE
LIMIT 45;

UPDATE tbl_02 SET col_1 = 100 WHERE id > 84;

ALTER TABLE tbl_02 ADD COLUMN dyn_col_4 VARCHAR(32);

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    col_3 BOOLEAN,
    col_4 INT
);

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_01 WHERE id IS NOT NULL;

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (376326, 0.0000000001, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 9223372036854775807);

BEGIN;
INSERT INTO asset_meta (id, payload) VALUES (804468, 'alpha_beta_gamma');
COMMIT;

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

SELECT
    CAST(('escaped''quote' AND LOWER(((SELECT (sub_103.id + CAST(sub_103.id AS CHAR)) FROM tbl_05 AS sub_103 LIMIT 1) + CAST((((t_ref_102.id IN (SELECT sub_104.col_1 FROM tbl_04 AS sub_104)) > ((CAST((CAST(EXISTS (SELECT 1 FROM tbl_02 AS sub_105 WHERE FALSE) AS CHAR) < CAST((((t_ref_102.id = (('escaped''quote' - t_ref_102.col_3) * (t_ref_102.col_3 BETWEEN 100 AND 2147483647))) < t_ref_102.col_3) AND (t_ref_102.col_1 AND (1e10 IN (SELECT sub_107.col_1 FROM tbl_06 AS sub_107)))) AS DECIMAL(10,2))) AS DECIMAL(10,2)) >= LOWER((((t_ref_102.id IN (SELECT sub_108.col_4 FROM tbl_04 AS sub_108)) - -1e-5) * t_ref_102.id))) / NULLIF((SELECT ((sub_109.col_2 IN (SELECT sub_110.dyn_col_4 FROM tbl_02 AS sub_110)) BETWEEN 9223372036854775807 AND 100) FROM tbl_03 AS sub_109 LIMIT 1), 0))) <= CAST(EXISTS (SELECT 1 FROM tbl_04 AS sub_112 WHERE sub_112.col_4) AS CHAR)) AS DECIMAL(10,2))))) AS DECIMAL(10,2)) AS c_119,
    UPPER(CAST(EXISTS (SELECT 1 FROM tbl_05 AS sub_120 WHERE 42) AS SIGNED)) AS c_129,
    CAST((CASE WHEN ABS(((SELECT (SELECT inn_137.col_3 FROM tbl_03 AS inn_137 LIMIT 1) FROM tbl_03 AS sub_130 LIMIT 1) AND (LOWER(EXISTS (SELECT 1 FROM tbl_01 AS sub_138 WHERE sub_138.col_1)) AND (((ABS((((LENGTH(((t_ref_102.col_3 IN (SELECT sub_139.col_1 FROM tbl_05 AS sub_139)) - (SELECT (SELECT inn_141.dyn_col_4 FROM tbl_02 AS inn_141 LIMIT 1) FROM tbl_01 AS sub_140 LIMIT 1))) <= LOWER(t_ref_102.col_3)) * (((t_ref_102.col_3 IN (SELECT sub_142.col_4 FROM tbl_03 AS sub_142)) <= (t_ref_102.col_3 IN (SELECT sub_143.col_1 FROM tbl_04 AS sub_143))) BETWEEN -1 AND 42)) >= EXISTS (SELECT 1 FROM asset_meta AS sub_145 WHERE NULL))) + FALSE) < (TRUE IN (SELECT sub_146.col_3 FROM tbl_03 AS sub_146))) + EXISTS (SELECT 1 FROM tbl_05 AS sub_148 WHERE FALSE))))) THEN (CAST((ABS(NULL) * ((SELECT (((sub_149.col_2 - FALSE) < LENGTH(((sub_149.col_1 BETWEEN -9223372036854775808 AND -1) - sub_149.col_2))) * ((SELECT CAST((SELECT COALESCE(FALSE, (sub_151.id BETWEEN 9223372036854775807 AND 42)) FROM tbl_05 AS sub_151 LIMIT 1) AS SIGNED) FROM tbl_02 AS sub_150 LIMIT 1) / NULLIF(TRUE, 0))) FROM tbl_05 AS sub_149 LIMIT 1) = (t_ref_102.col_1 IN (SELECT sub_152.id FROM asset_meta AS sub_152)))) AS CHAR) + (LENGTH((((CAST(t_ref_102.col_3 AS CHAR) + UPPER(t_ref_102.col_3)) IS NULL) / NULLIF((t_ref_102.col_3 > (CAST((LENGTH(((SELECT UPPER(CAST((SELECT sub_166.id FROM tbl_02 AS sub_166 LIMIT 1) AS SIGNED)) FROM tbl_06 AS sub_165 LIMIT 1) OR 3.14159265)) - ((-9223372036854775808 IN (SELECT sub_167.col_1 FROM tbl_01 AS sub_167)) * t_ref_102.id)) AS SIGNED) + CAST((t_ref_102.col_1 IN (SELECT sub_169.id FROM tbl_06 AS sub_169)) AS DECIMAL(10,2)))), 0))) / NULLIF((SELECT (ABS(((EXISTS (SELECT 1 FROM tbl_02 AS sub_172 WHERE sub_172.ref_tbl_01_id) - (UPPER(LENGTH(UPPER(COALESCE(NULLIF((sub_171.col_4 / NULLIF(1.0, 0)), (sub_171.id + sub_171.col_1)), ((sub_171.col_2 BETWEEN -9223372036854775808 AND 1) + (1 / NULLIF(FALSE, 0))))))) / NULLIF(((((sub_171.col_4 IN (SELECT sub_173.id FROM asset_meta AS sub_173)) OR COALESCE(sub_171.id, CAST((SELECT sub_174.id FROM tbl_03 AS sub_174 LIMIT 1) AS CHAR))) <= ((TRUE >= ((FALSE IN (SELECT sub_175.id FROM asset_meta AS sub_175)) BETWEEN 1 AND 1)) <= (SELECT COALESCE(sub_176.col_1, sub_176.ref_tbl_01_id) FROM tbl_02 AS sub_176 LIMIT 1))) * ((NULL - LOWER(42)) <= COALESCE((SELECT (SELECT 'alpha_beta_gamma' FROM asset_meta AS inn_179 LIMIT 1) FROM asset_meta AS sub_177 LIMIT 1), (sub_171.col_2 <> ((FALSE + FALSE) * COALESCE(sub_171.id, sub_171.col_3)))))), 0))) >= COALESCE((CASE WHEN (2147483647 < NULLIF(UPPER((((sub_171.id = sub_171.id) = sub_171.id) <> sub_171.id)), (sub_171.col_1 IN (SELECT sub_180.id FROM tbl_04 AS sub_180)))) THEN LOWER(EXISTS (SELECT 1 FROM tbl_04 AS sub_182 WHERE sub_182.col_4)) ELSE ((SELECT LENGTH(sub_186.payload) FROM asset_meta AS sub_186 LIMIT 1) AND ((3.14159265 IN (SELECT sub_187.col_3 FROM tbl_03 AS sub_187)) * TRUE)) END), -1.5))) <> (sub_171.col_3 - CAST((((sub_171.col_4 IN (SELECT sub_189.id FROM tbl_04 AS sub_189)) - ((1.0 - sub_171.col_2) IS NULL)) AND sub_171.col_2) AS CHAR))) FROM tbl_03 AS sub_171 LIMIT 1), 0))) ELSE ((SELECT CAST((FALSE > 'escaped''quote') AS DECIMAL(10,2)) FROM tbl_04 AS sub_190 LIMIT 1) OR COALESCE((CASE WHEN (LOWER((((t_ref_102.id <= (SELECT (TRUE BETWEEN 0 AND 0) FROM tbl_05 AS sub_191 LIMIT 1)) >= LOWER(((t_ref_102.id IN (SELECT sub_192.payload FROM asset_meta AS sub_192)) + NULLIF((((((t_ref_102.id * 1.0) > (t_ref_102.col_1 + -9223372036854775808)) * ((SELECT (SELECT inn_194.col_2 FROM tbl_05 AS inn_194 LIMIT 1) FROM asset_meta AS sub_193 LIMIT 1) = (t_ref_102.col_3 + NULL))) + EXISTS (SELECT 1 FROM tbl_01 AS sub_195 WHERE 'chaos')) + COALESCE(((SELECT sub_198.id FROM tbl_01 AS sub_198 LIMIT 1) + CAST(('alpha_beta_gamma' + -1e-5) AS SIGNED)), ((100 IN (SELECT sub_199.col_2 FROM tbl_03 AS sub_199)) >= ((t_ref_102.col_1 + t_ref_102.col_1) - EXISTS (SELECT 1 FROM tbl_06 AS sub_200 WHERE sub_200.col_3))))), ((CASE WHEN (EXISTS (SELECT 1 FROM tbl_05 AS sub_201 WHERE sub_201.id) + (CAST(t_ref_102.id AS SIGNED) < (t_ref_102.col_1 * t_ref_102.col_1))) THEN 3.14159265 ELSE (SELECT (SELECT inn_203.id FROM tbl_02 AS inn_203 LIMIT 1) FROM tbl_04 AS sub_202 LIMIT 1) END) - EXISTS (SELECT 1 FROM tbl_01 AS sub_204 WHERE sub_204.col_1)))))) + (((NOT (((('escaped''quote' > t_ref_102.col_1) IS NOT NULL) OR EXISTS (SELECT 1 FROM tbl_05 AS sub_205 WHERE 'ref_val')))) >= (CASE WHEN ((SELECT (SELECT inn_210.col_1 FROM tbl_06 AS inn_210 LIMIT 1) FROM tbl_01 AS sub_208 LIMIT 1) <= ((SELECT sub_211.col_2 FROM tbl_06 AS sub_211 LIMIT 1) / NULLIF(CAST(CAST(((t_ref_102.id >= t_ref_102.col_3) - CAST(t_ref_102.col_1 AS SIGNED)) AS DECIMAL(10,2)) AS SIGNED), 0))) THEN ((SELECT NULLIF(LOWER(sub_212.col_2), (NULL AND (sub_212.id * sub_212.col_4))) FROM tbl_04 AS sub_212 LIMIT 1) = (SELECT sub_213.id FROM tbl_06 AS sub_213 LIMIT 1)) ELSE (t_ref_102.col_1 / NULLIF((CASE WHEN EXISTS (SELECT 1 FROM tbl_03 AS sub_214 WHERE TRUE) THEN t_ref_102.id ELSE (t_ref_102.col_3 IN (SELECT sub_215.col_1 FROM tbl_02 AS sub_215)) END), 0)) END)) <= (t_ref_102.col_1 * CAST(CAST((CASE WHEN ((t_ref_102.id OR (SELECT (SELECT inn_217.dyn_col_4 FROM tbl_02 AS inn_217 LIMIT 1) FROM tbl_03 AS sub_216 LIMIT 1)) BETWEEN 9223372036854775807 AND 1) THEN COALESCE(CAST((SELECT (SELECT inn_219.col_3 FROM tbl_05 AS inn_219 LIMIT 1) FROM tbl_03 AS sub_218 LIMIT 1) AS SIGNED), ((SELECT sub_220.id FROM tbl_02 AS sub_220 LIMIT 1) - (('ref_val' IS NULL) - UPPER(t_ref_102.col_3)))) ELSE t_ref_102.col_3 END) AS CHAR) AS SIGNED))))) <= (NOT ((LOWER((((SELECT ((NOT (((sub_221.id IS NULL) * CAST(sub_221.col_3 AS SIGNED)))) + (CASE WHEN sub_221.col_4 THEN sub_221.col_4 ELSE (SELECT (SELECT 'alpha_beta_gamma' FROM tbl_05 AS inn_223 LIMIT 1) FROM tbl_03 AS sub_222 LIMIT 1) END)) FROM tbl_04 AS sub_221 LIMIT 1) / NULLIF(((LENGTH(ABS(CAST((NULL / NULLIF(t_ref_102.col_1, 0)) AS DECIMAL(10,2)))) IS NULL) IS NOT NULL), 0)) / NULLIF(CAST((SELECT CAST(LOWER('escaped''quote') AS CHAR) FROM tbl_01 AS sub_224 LIMIT 1) AS CHAR), 0))) * (UPPER(t_ref_102.col_3) <= t_ref_102.col_1))))) THEN ((42 IN (SELECT sub_225.col_2 FROM tbl_06 AS sub_225)) IS NULL) ELSE EXISTS (SELECT 1 FROM tbl_04 AS sub_228 WHERE sub_228.id) END), EXISTS (SELECT 1 FROM tbl_06 AS sub_230 WHERE FALSE))) END) AS CHAR) AS c_231
FROM tbl_01 AS t_ref_102 LEFT JOIN tbl_05 AS j_ref_232 ON t_ref_102.id = j_ref_232.id
WHERE (t_ref_102.id IN (SELECT sub_233.id FROM asset_meta AS sub_233))
LIMIT 37;

UPDATE tbl_04 SET col_1 = 'escaped''quote' WHERE id > 82;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSON,
    col_3 NUMERIC(10,2)
);

SELECT
    t_ref_243.id AS c_244,
    (t_ref_243.ref_tbl_01_id * (EXISTS (SELECT 1 FROM asset_meta AS sub_245 WHERE TRUE) > t_ref_243.dyn_col_4)) AS c_267,
    CAST(((CASE WHEN t_ref_243.id THEN UPPER(CAST(((SELECT (((SELECT (sub_269.col_2 - ((CASE WHEN ((sub_269.col_2 AND sub_269.col_4) <= sub_269.id) THEN sub_269.id ELSE ((sub_269.col_2 IN (SELECT sub_270.payload FROM asset_meta AS sub_270)) = sub_269.col_3) END) + FALSE)) FROM tbl_06 AS sub_269 LIMIT 1) BETWEEN 100 AND 2147483647) < (CASE WHEN (CASE WHEN (CASE WHEN (((SELECT (CASE WHEN 9223372036854775807 THEN sub_271.id ELSE 9223372036854775807 END) FROM tbl_06 AS sub_271 LIMIT 1) AND sub_268.col_3) + 100) THEN (((ABS(sub_268.col_1) <> ((SELECT (SELECT 0 FROM tbl_06 AS inn_273 LIMIT 1) FROM tbl_02 AS sub_272 LIMIT 1) * sub_268.col_3)) OR CAST(((sub_268.id BETWEEN -2147483648 AND 42) BETWEEN -1 AND 1) AS SIGNED)) / NULLIF(CAST((CAST(COALESCE(NULL, sub_268.id) AS DECIMAL(10,2)) BETWEEN 0 AND 2147483647) AS DECIMAL(10,2)), 0)) ELSE ((CASE WHEN (SELECT (SELECT inn_275.col_2 FROM tbl_03 AS inn_275 LIMIT 1) FROM tbl_03 AS sub_274 LIMIT 1) THEN (UPPER((sub_268.col_2 IN (SELECT sub_276.payload FROM asset_meta AS sub_276))) / NULLIF((SELECT sub_277.id FROM tbl_05 AS sub_277 LIMIT 1), 0)) ELSE (CASE WHEN sub_268.id THEN CAST(CAST(TRUE AS SIGNED) AS SIGNED) ELSE (42 - (-1 IN (SELECT sub_278.col_2 FROM tbl_07 AS sub_278))) END) END) * EXISTS (SELECT 1 FROM tbl_03 AS sub_279 WHERE FALSE)) END) THEN (((CASE WHEN (sub_268.col_3 - NULLIF(NULLIF(TRUE, sub_268.col_2), CAST(sub_268.col_3 AS DECIMAL(10,2)))) THEN (((sub_268.id > sub_268.col_1) * 0) - (SELECT sub_280.dyn_col_4 FROM tbl_02 AS sub_280 LIMIT 1)) ELSE (SELECT (TRUE / NULLIF(2147483647, 0)) FROM tbl_05 AS sub_281 LIMIT 1) END) - ABS(TRUE)) AND ((sub_268.col_1 IN (SELECT sub_282.col_2 FROM tbl_03 AS sub_282)) IS NULL)) ELSE (SELECT (SELECT inn_285.col_4 FROM tbl_06 AS inn_285 LIMIT 1) FROM tbl_04 AS sub_283 LIMIT 1) END) THEN ((SELECT LENGTH((CAST((sub_286.col_1 + sub_286.col_2) AS DECIMAL(10,2)) * EXISTS (SELECT 1 FROM asset_meta AS sub_287 WHERE sub_287.payload))) FROM tbl_05 AS sub_286 LIMIT 1) * NULLIF((((CAST(-1.5 AS SIGNED) - ((sub_268.col_3 BETWEEN -9223372036854775808 AND -2147483648) AND (SELECT sub_288.ref_tbl_01_id FROM tbl_02 AS sub_288 LIMIT 1))) / NULLIF((SELECT (SELECT 0.0 FROM tbl_04 AS inn_290 LIMIT 1) FROM tbl_04 AS sub_289 LIMIT 1), 0)) * NULLIF(ABS(((SELECT (SELECT inn_292.id FROM tbl_01 AS inn_292 LIMIT 1) FROM tbl_07 AS sub_291 LIMIT 1) OR (sub_268.col_2 IN (SELECT sub_293.id FROM tbl_04 AS sub_293)))), UPPER((sub_268.col_3 + (sub_268.col_2 < sub_268.col_3))))), sub_268.col_3)) ELSE (((sub_268.col_3 BETWEEN 1 AND 0) >= sub_268.id) > (NOT ((SELECT (SELECT inn_296.col_2 FROM tbl_03 AS inn_296 LIMIT 1) FROM tbl_06 AS sub_294 LIMIT 1)))) END)) FROM tbl_05 AS sub_268 LIMIT 1) = (((SELECT ((NOT (COALESCE((UPPER(LOWER(9223372036854775807)) BETWEEN 42 AND -9223372036854775808), (((LENGTH(42) > ('escaped''quote' / NULLIF(100, 0))) < sub_297.id) + sub_297.id)))) <> COALESCE(100, (NOT (((sub_297.payload IN (SELECT sub_298.col_4 FROM tbl_03 AS sub_298)) OR (SELECT (SELECT 1e10 FROM tbl_03 AS inn_300 LIMIT 1) FROM tbl_03 AS sub_299 LIMIT 1)))))) FROM asset_meta AS sub_297 LIMIT 1) OR 2147483647) * ABS(t_ref_243.ref_tbl_01_id))) AS DECIMAL(10,2))) ELSE (t_ref_243.dyn_col_4 <> ((CAST(((SELECT (SELECT inn_306.col_3 FROM tbl_01 AS inn_306 LIMIT 1) FROM tbl_04 AS sub_301 LIMIT 1) = (LOWER(ABS((((LOWER(LENGTH((1 <= 'ref_val'))) < t_ref_243.ref_tbl_01_id) * COALESCE((NOT (t_ref_243.ref_tbl_01_id)), (CASE WHEN ABS((t_ref_243.col_1 >= NULL)) THEN (COALESCE(FALSE, t_ref_243.ref_tbl_01_id) AND (t_ref_243.dyn_col_4 <> t_ref_243.ref_tbl_01_id)) ELSE CAST('alpha_beta_gamma' AS SIGNED) END))) - LOWER(t_ref_243.dyn_col_4)))) < (t_ref_243.col_1 IN (SELECT sub_307.col_3 FROM tbl_03 AS sub_307)))) AS CHAR) AND ((SELECT (SELECT ((sub_309.dyn_col_4 - sub_309.col_1) - EXISTS (SELECT 1 FROM tbl_03 AS sub_310 WHERE NULL)) FROM tbl_02 AS sub_309 LIMIT 1) FROM tbl_07 AS sub_308 LIMIT 1) + (((SELECT ((SELECT (SELECT TRUE FROM tbl_03 AS inn_315 LIMIT 1) FROM tbl_06 AS sub_313 LIMIT 1) - (SELECT CAST(((sub_316.col_1 BETWEEN -1 AND 1) < (sub_316.col_3 / NULLIF(TRUE, 0))) AS CHAR) FROM tbl_01 AS sub_316 LIMIT 1)) FROM tbl_04 AS sub_312 LIMIT 1) / NULLIF((SELECT (SELECT TRUE FROM tbl_02 AS inn_322 LIMIT 1) FROM tbl_02 AS sub_317 LIMIT 1), 0)) - CAST(((SELECT (SELECT inn_324.payload FROM asset_meta AS inn_324 LIMIT 1) FROM tbl_07 AS sub_323 LIMIT 1) / NULLIF((SELECT (SELECT inn_327.id FROM tbl_05 AS inn_327 LIMIT 1) FROM tbl_05 AS sub_325 LIMIT 1), 0)) AS DECIMAL(10,2))))) / NULLIF(TRUE, 0))) END) IS NOT NULL) AS DECIMAL(10,2)) AS c_328
FROM tbl_02 AS t_ref_243
WHERE (LOWER(CAST(CAST(t_ref_243.ref_tbl_01_id AS DECIMAL(10,2)) AS DECIMAL(10,2))) * ((EXISTS (SELECT 1 FROM tbl_07 AS sub_329 WHERE sub_329.col_3) BETWEEN 2147483647 AND 9223372036854775807) * (t_ref_243.id IN (SELECT sub_330.col_3 FROM tbl_03 AS sub_330))))
LIMIT 30;

INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (800556, 100, 0, 'chaos');

ALTER TABLE tbl_01 DROP COLUMN col_1;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (964647, FALSE, '__fuzz_marker__', TRUE, -1);
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (304920, FALSE, '__fuzz_marker__', TRUE, 2147483647);
COMMIT;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    col_3 BOOLEAN,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE VIEW v_summary_6 AS SELECT id FROM tbl_08 WHERE id IS NOT NULL;

INSERT INTO asset_meta (id, payload) VALUES (919107, 'chaos');

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (323177, 'chaos', -2147483648, FALSE, 1);
INSERT INTO asset_meta (id, payload) VALUES (845807, 'escaped''quote');
INSERT INTO tbl_01 (id, col_3) VALUES (659498, 1.0);
COMMIT;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

SELECT
    t_ref_346.col_4 AS c_347
FROM tbl_06 AS t_ref_346 LEFT JOIN tbl_04 AS j_ref_348 ON t_ref_346.id = j_ref_348.id
WHERE t_ref_346.id
LIMIT 17;

BEGIN;
INSERT INTO tbl_01 (id, col_3) VALUES (614206, -1.5);
COMMIT;

INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_03_id) VALUES (507660, 1.0, 'escaped''quote', 0);

INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (755717, -9223372036854775808, 100, '__fuzz_marker__');

ALTER TABLE tbl_07 DROP COLUMN col_2;

SELECT
    ('alpha_beta_gamma' <> (CAST((t_ref_349.col_1 IN (SELECT sub_350.id FROM tbl_03 AS sub_350)) AS SIGNED) * ((CAST(t_ref_349.col_1 AS DECIMAL(10,2)) >= ((CASE WHEN TRUE THEN (t_ref_349.id IN (SELECT sub_353.col_3 FROM tbl_07 AS sub_353)) ELSE LOWER(LENGTH(ABS(t_ref_349.id))) END) OR NULLIF((CASE WHEN (t_ref_349.col_3 IS NOT NULL) THEN t_ref_349.col_1 ELSE (t_ref_349.col_3 - ((LOWER((t_ref_349.id * (CAST(t_ref_349.id AS SIGNED) + (t_ref_349.col_3 IN (SELECT sub_354.col_3 FROM tbl_07 AS sub_354))))) <= CAST((SELECT (SELECT inn_357.col_1 FROM tbl_07 AS inn_357 LIMIT 1) FROM tbl_09 AS sub_355 LIMIT 1) AS DECIMAL(10,2))) / NULLIF(((t_ref_349.col_1 - ABS(EXISTS (SELECT 1 FROM tbl_06 AS sub_358 WHERE 1))) BETWEEN -9223372036854775808 AND 100), 0))) END), ((NOT (((t_ref_349.col_1 IN (SELECT sub_359.col_4 FROM tbl_06 AS sub_359)) <= LOWER(((CAST((SELECT (SELECT inn_362.id FROM tbl_01 AS inn_362 LIMIT 1) FROM tbl_02 AS sub_360 LIMIT 1) AS SIGNED) + (t_ref_349.id IN (SELECT sub_363.id FROM asset_meta AS sub_363))) BETWEEN 42 AND 9223372036854775807))))) * t_ref_349.col_1)))) + CAST(t_ref_349.col_1 AS SIGNED)))) AS c_364,
    (('chaos' >= (CASE WHEN ((t_ref_349.col_3 AND (TRUE * FALSE)) < t_ref_349.col_3) THEN UPPER(((UPPER((t_ref_349.col_1 OR CAST((0.0 IN (SELECT sub_365.id FROM tbl_05 AS sub_365)) AS SIGNED))) + ((COALESCE(CAST(-1 AS SIGNED), ((SELECT (SELECT inn_367.col_1 FROM tbl_07 AS inn_367 LIMIT 1) FROM tbl_05 AS sub_366 LIMIT 1) - (NULL + t_ref_349.col_3))) / NULLIF(((SELECT ((((CAST((sub_368.col_3 < sub_368.col_4) AS SIGNED) > (sub_368.col_1 IN (SELECT sub_369.col_1 FROM tbl_06 AS sub_369))) = (NOT ((CAST(sub_368.col_4 AS SIGNED) / NULLIF(3.14159265, 0))))) >= CAST((ABS((SELECT (SELECT TRUE FROM tbl_02 AS inn_371 LIMIT 1) FROM tbl_09 AS sub_370 LIMIT 1)) * CAST((SELECT sub_372.id FROM asset_meta AS sub_372 LIMIT 1) AS SIGNED)) AS CHAR)) OR ((LOWER((LOWER(sub_368.col_4) >= (-2147483648 - sub_368.col_3))) <> (SELECT (SELECT inn_374.col_1 FROM tbl_05 AS inn_374 LIMIT 1) FROM tbl_04 AS sub_373 LIMIT 1)) IS NOT NULL)) FROM tbl_03 AS sub_368 LIMIT 1) + NULL), 0)) OR (SELECT (SELECT inn_380.col_4 FROM tbl_04 AS inn_380 LIMIT 1) FROM tbl_09 AS sub_375 LIMIT 1))) + (CAST(t_ref_349.col_3 AS CHAR) / NULLIF(((SELECT (SELECT (SELECT inn_383.ref_tbl_03_id FROM tbl_09 AS inn_383 LIMIT 1) FROM tbl_06 AS sub_382 LIMIT 1) FROM tbl_07 AS sub_381 LIMIT 1) + (t_ref_349.id + EXISTS (SELECT 1 FROM tbl_08 AS sub_384 WHERE sub_384.ref_tbl_01_id))), 0)))) ELSE (EXISTS (SELECT 1 FROM tbl_04 AS sub_385 WHERE FALSE) / NULLIF(CAST((0.0000000001 AND t_ref_349.col_1) AS DECIMAL(10,2)), 0)) END)) + (t_ref_349.col_1 IN (SELECT sub_386.col_1 FROM tbl_06 AS sub_386))) AS c_387
FROM tbl_07 AS t_ref_349 LEFT JOIN tbl_02 AS j_ref_388 ON t_ref_349.id = j_ref_388.id
WHERE (NULL OR ('alpha_beta_gamma' IN (SELECT sub_389.col_1 FROM tbl_05 AS sub_389)))
LIMIT 50;

INSERT INTO asset_meta (meta_id, category, blob_chunk) VALUES
    (1, 'cat_alpha', REPEAT('A', 8192)),
    (2, 'cat_alpha', REPEAT('B', 8192)),
    (3, 'cat_alpha', REPEAT('C', 8192)),
    (4, 'cat_beta',  REPEAT('X', 4096)),
    (5, 'cat_beta',  REPEAT('Y', 4096));

CREATE VIEW v_summary_7 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (188508, 3.14159265, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -9223372036854775808);

UPDATE tbl_07 SET col_3 = 3.14159265 WHERE id > 51;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (210057, '', 'chaos', -9223372036854775808, 'ref_val');
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (181595, '__fuzz_marker__', 'alpha_beta_gamma', 0, 'escaped''quote');
COMMIT;

INSERT INTO tbl_01 (id, col_3) VALUES (829171, 999999999.99);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (925856, 'chaos', 1.0, 9223372036854775807, '{}');

ALTER TABLE tbl_09 DROP COLUMN col_2;

ALTER TABLE tbl_01 ADD COLUMN dyn_col_3 VARCHAR(32);

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (136847, TRUE, 'escaped''quote', TRUE, 1);
INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (909772, 1.0, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -2147483648);
INSERT INTO tbl_07 (id, col_1, col_3) VALUES (157909, 0.0, 999999999.99);
COMMIT;

SELECT
    (t_ref_395.id IN (SELECT sub_396.col_1 FROM tbl_08 AS sub_396)) AS c_397,
    (CAST(t_ref_395.dyn_col_3 AS DECIMAL(10,2)) OR ((CAST((TRUE IN (SELECT sub_398.col_1 FROM tbl_02 AS sub_398)) AS DECIMAL(10,2)) + (SELECT (SELECT inn_401.col_4 FROM tbl_04 AS inn_401 LIMIT 1) FROM tbl_08 AS sub_400 LIMIT 1)) / NULLIF((CASE WHEN CAST(UPPER(EXISTS (SELECT 1 FROM tbl_06 AS sub_402 WHERE sub_402.col_3)) AS SIGNED) THEN EXISTS (SELECT 1 FROM tbl_03 AS sub_403 WHERE 1e10) ELSE CAST(TRUE AS CHAR) END), 0))) AS c_421,
    ABS((EXISTS (SELECT 1 FROM tbl_01 AS sub_422 WHERE TRUE) + LENGTH(CAST(NULL AS DECIMAL(10,2))))) AS c_440
FROM tbl_01 AS t_ref_395 LEFT JOIN asset_meta AS j_ref_441 ON t_ref_395.id = j_ref_441.id
WHERE (CAST(999999999.99 AS CHAR) OR (((SELECT sub_442.col_1 FROM tbl_04 AS sub_442 LIMIT 1) - (SELECT CAST(CAST(ABS(CAST((CASE WHEN ((SELECT (SELECT FALSE FROM tbl_08 AS inn_445 LIMIT 1) FROM tbl_04 AS sub_444 LIMIT 1) - (((SELECT NULLIF((sub_446.col_1 - sub_446.col_4), -1) FROM tbl_03 AS sub_446 LIMIT 1) * (COALESCE((LOWER(sub_443.col_2) <= (FALSE / NULLIF(sub_443.col_3, 0))), ((sub_443.id = sub_443.col_1) + (FALSE + sub_443.col_2))) - NULLIF(EXISTS (SELECT 1 FROM tbl_03 AS sub_447 WHERE 3.14159265), CAST(sub_443.id AS CHAR)))) = (((NULLIF(EXISTS (SELECT 1 FROM tbl_09 AS sub_448 WHERE FALSE), (TRUE IN (SELECT sub_449.col_3 FROM tbl_01 AS sub_449))) + FALSE) + (SELECT (SELECT (SELECT inn_452.col_2 FROM tbl_04 AS inn_452 LIMIT 1) FROM tbl_02 AS sub_451 LIMIT 1) FROM tbl_03 AS sub_450 LIMIT 1)) / NULLIF((sub_443.col_1 > (sub_443.col_2 * EXISTS (SELECT 1 FROM tbl_03 AS sub_453 WHERE sub_453.col_3))), 0)))) THEN CAST(LOWER(TRUE) AS SIGNED) ELSE EXISTS (SELECT 1 FROM tbl_04 AS sub_454 WHERE sub_454.col_4) END) AS SIGNED)) AS SIGNED) AS SIGNED) FROM tbl_05 AS sub_443 LIMIT 1)) + t_ref_395.col_3))
LIMIT 47;

DELETE FROM tbl_03 WHERE id = 5302;

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (314499, 0.0000000001, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 0);

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 INT,
    col_4 JSON
);

SELECT
    (CAST(((SELECT (SELECT TRUE FROM tbl_10 AS inn_468 LIMIT 1) FROM tbl_08 AS sub_458 LIMIT 1) = (ABS((NOT (NULL))) * NULLIF(NULLIF((SELECT (SELECT (SELECT inn_472.col_2 FROM tbl_04 AS inn_472 LIMIT 1) FROM tbl_05 AS sub_470 LIMIT 1) FROM tbl_04 AS sub_469 LIMIT 1), (t_ref_457.id - (SELECT (SELECT FALSE FROM tbl_05 AS inn_474 LIMIT 1) FROM tbl_02 AS sub_473 LIMIT 1))), CAST((SELECT (SELECT 1.0 FROM tbl_05 AS inn_476 LIMIT 1) FROM tbl_02 AS sub_475 LIMIT 1) AS DECIMAL(10,2))))) AS SIGNED) + ((((((ABS(((((CASE WHEN ((SELECT (SELECT inn_478.col_3 FROM tbl_04 AS inn_478 LIMIT 1) FROM tbl_03 AS sub_477 LIMIT 1) / NULLIF(t_ref_457.col_2, 0)) THEN t_ref_457.col_1 ELSE (SELECT EXISTS (SELECT 1 FROM tbl_05 AS sub_480 WHERE 'alpha_beta_gamma') FROM tbl_08 AS sub_479 LIMIT 1) END) <> (TRUE = (NOT ((SELECT ((sub_481.id + sub_481.col_4) + CAST(sub_481.col_2 AS CHAR)) FROM tbl_10 AS sub_481 LIMIT 1))))) AND CAST(FALSE AS DECIMAL(10,2))) * t_ref_457.col_1)) * 0.0) - CAST(EXISTS (SELECT 1 FROM tbl_07 AS sub_482 WHERE TRUE) AS SIGNED)) = NULLIF(t_ref_457.col_2, t_ref_457.col_1)) AND ((CAST(t_ref_457.col_2 AS SIGNED) / NULLIF((SELECT (SELECT inn_492.payload FROM asset_meta AS inn_492 LIMIT 1) FROM tbl_10 AS sub_486 LIMIT 1), 0)) - t_ref_457.col_3)) BETWEEN -9223372036854775808 AND -2147483648) - (NOT ((SELECT (((EXISTS (SELECT 1 FROM tbl_07 AS sub_494 WHERE sub_494.id) < ((CAST((LOWER(CAST(NULL AS CHAR)) - sub_493.col_2) AS SIGNED) - (EXISTS (SELECT 1 FROM asset_meta AS sub_495 WHERE sub_495.payload) / NULLIF(CAST(CAST(COALESCE(CAST((CASE WHEN NULL THEN LOWER(FALSE) ELSE (sub_493.col_1 * 0.0) END) AS SIGNED), (sub_493.id = CAST(999999999.99 AS DECIMAL(10,2)))) AS DECIMAL(10,2)) AS CHAR), 0))) OR CAST(FALSE AS DECIMAL(10,2)))) = CAST(EXISTS (SELECT 1 FROM tbl_04 AS sub_499 WHERE FALSE) AS DECIMAL(10,2))) >= (sub_493.col_2 IN (SELECT sub_502.id FROM tbl_09 AS sub_502))) FROM tbl_06 AS sub_493 LIMIT 1))))) AS c_503,
    (-1e-5 IN (SELECT sub_504.col_4 FROM tbl_06 AS sub_504)) AS c_516,
    ((((t_ref_457.id IN (SELECT sub_517.col_1 FROM tbl_05 AS sub_517)) OR NULLIF((LENGTH(-1.5) AND EXISTS (SELECT 1 FROM tbl_04 AS sub_518 WHERE sub_518.col_4)), t_ref_457.id)) + (SELECT (sub_522.col_1 IN (SELECT sub_523.ref_tbl_03_id FROM tbl_09 AS sub_523)) FROM tbl_07 AS sub_522 LIMIT 1)) - CAST((((SELECT (SELECT inn_525.col_2 FROM tbl_04 AS inn_525 LIMIT 1) FROM tbl_07 AS sub_524 LIMIT 1) >= (((((EXISTS (SELECT 1 FROM tbl_02 AS sub_526 WHERE FALSE) AND EXISTS (SELECT 1 FROM tbl_06 AS sub_530 WHERE NULL)) * t_ref_457.col_3) - (SELECT (SELECT inn_534.col_4 FROM tbl_06 AS inn_534 LIMIT 1) FROM tbl_03 AS sub_533 LIMIT 1)) IS NOT NULL) - NULL)) * (CASE WHEN (FALSE <> (LOWER(UPPER((UPPER((SELECT (SELECT -1.5 FROM tbl_10 AS inn_536 LIMIT 1) FROM tbl_06 AS sub_535 LIMIT 1)) > ((t_ref_457.col_2 OR t_ref_457.ref_tbl_01_id) = (SELECT (CASE WHEN (NOT (ABS((NOT (sub_537.col_2))))) THEN LENGTH((1e10 - NULL)) ELSE sub_537.id END) FROM tbl_10 AS sub_537 LIMIT 1))))) BETWEEN 42 AND 1)) THEN (SELECT ((UPPER(sub_538.col_4) - sub_538.col_3) * sub_538.col_4) FROM tbl_03 AS sub_538 LIMIT 1) ELSE ((SELECT (SELECT inn_544.col_1 FROM tbl_04 AS inn_544 LIMIT 1) FROM tbl_03 AS sub_539 LIMIT 1) / NULLIF(CAST(((t_ref_457.ref_tbl_01_id IN (SELECT sub_545.col_1 FROM tbl_02 AS sub_545)) * ABS(-9223372036854775808)) AS SIGNED), 0)) END)) AS CHAR)) AS c_559
FROM tbl_08 AS t_ref_457 LEFT JOIN tbl_10 AS j_ref_560 ON t_ref_457.id = j_ref_560.id
WHERE ((CASE WHEN FALSE THEN LENGTH(((EXISTS (SELECT 1 FROM tbl_07 AS sub_561 WHERE 'escaped''quote') AND (NULLIF(((CAST((SELECT (SELECT inn_565.col_3 FROM tbl_06 AS inn_565 LIMIT 1) FROM asset_meta AS sub_564 LIMIT 1) AS SIGNED) * (SELECT (EXISTS (SELECT 1 FROM tbl_04 AS sub_567 WHERE sub_567.col_1) > EXISTS (SELECT 1 FROM tbl_07 AS sub_570 WHERE sub_570.id)) FROM tbl_06 AS sub_566 LIMIT 1)) * (TRUE * t_ref_457.col_1)), NULLIF((t_ref_457.col_1 IN (SELECT sub_571.payload FROM asset_meta AS sub_571)), ((SELECT LOWER(EXISTS (SELECT 1 FROM tbl_01 AS sub_573 WHERE FALSE)) FROM tbl_01 AS sub_572 LIMIT 1) - ((((SELECT (SELECT TRUE FROM tbl_03 AS inn_576 LIMIT 1) FROM tbl_05 AS sub_574 LIMIT 1) AND (t_ref_457.col_2 >= t_ref_457.ref_tbl_01_id)) < TRUE) <= EXISTS (SELECT 1 FROM tbl_07 AS sub_577 WHERE FALSE))))) * (t_ref_457.col_3 - EXISTS (SELECT 1 FROM tbl_01 AS sub_583 WHERE sub_583.col_3)))) - (LOWER(((CAST(CAST(((t_ref_457.id * t_ref_457.col_1) + 1e10) AS CHAR) AS CHAR) <= (SELECT (SELECT inn_598.payload FROM asset_meta AS inn_598 LIMIT 1) FROM tbl_01 AS sub_592 LIMIT 1)) AND CAST(t_ref_457.col_1 AS SIGNED))) * NULLIF(t_ref_457.col_2, LOWER((t_ref_457.col_1 * EXISTS (SELECT 1 FROM tbl_10 AS sub_599 WHERE sub_599.col_3))))))) ELSE t_ref_457.col_1 END) + (LENGTH(CAST((LOWER((t_ref_457.id IN (SELECT sub_600.col_1 FROM tbl_04 AS sub_600))) - CAST((ABS(t_ref_457.id) - CAST(CAST(LOWER((SELECT (SELECT '' FROM tbl_04 AS inn_602 LIMIT 1) FROM tbl_10 AS sub_601 LIMIT 1)) AS CHAR) AS CHAR)) AS CHAR)) AS DECIMAL(10,2))) <= (UPPER(EXISTS (SELECT 1 FROM tbl_05 AS sub_603 WHERE NULL)) * TRUE)))
LIMIT 46;

SELECT
    ((CAST((SELECT (SELECT inn_625.col_1 FROM tbl_02 AS inn_625 LIMIT 1) FROM tbl_09 AS sub_621 LIMIT 1) AS CHAR) * (((CASE WHEN (SELECT ((CASE WHEN CAST((sub_626.col_3 AND (SELECT (sub_627.id * EXISTS (SELECT 1 FROM tbl_06 AS sub_628 WHERE 'alpha_beta_gamma')) FROM tbl_10 AS sub_627 LIMIT 1)) AS SIGNED) THEN 'chaos' ELSE (CASE WHEN FALSE THEN EXISTS (SELECT 1 FROM asset_meta AS sub_629 WHERE 0.0) ELSE UPPER((LOWER((SELECT ((SELECT sub_631.col_3 FROM tbl_04 AS sub_631 LIMIT 1) / NULLIF(-1, 0)) FROM asset_meta AS sub_630 LIMIT 1)) / NULLIF(NULL, 0))) END) END) + (SELECT (sub_632.col_1 - (sub_632.col_1 / NULLIF(NULL, 0))) FROM tbl_09 AS sub_632 LIMIT 1)) FROM tbl_08 AS sub_626 LIMIT 1) THEN ('ref_val' IN (SELECT sub_633.col_1 FROM tbl_10 AS sub_633)) ELSE ((((CAST(t_ref_620.ref_tbl_01_id AS DECIMAL(10,2)) / NULLIF(((CASE WHEN (SELECT (SELECT inn_639.dyn_col_3 FROM tbl_01 AS inn_639 LIMIT 1) FROM tbl_09 AS sub_637 LIMIT 1) THEN CAST('chaos' AS CHAR) ELSE (t_ref_620.col_2 - FALSE) END) + 'alpha_beta_gamma'), 0)) <> t_ref_620.col_3) OR (SELECT (SELECT '__fuzz_marker__' FROM tbl_01 AS inn_644 LIMIT 1) FROM tbl_06 AS sub_640 LIMIT 1)) - ((CASE WHEN t_ref_620.col_2 THEN ((SELECT (SELECT NULL FROM tbl_08 AS inn_646 LIMIT 1) FROM tbl_05 AS sub_645 LIMIT 1) >= t_ref_620.col_2) ELSE (SELECT (CASE WHEN ABS(((CASE WHEN (sub_647.col_3 IN (SELECT sub_648.id FROM asset_meta AS sub_648)) THEN (((sub_647.id * 'escaped''quote') / NULLIF((sub_647.col_3 BETWEEN -9223372036854775808 AND 2147483647), 0)) / NULLIF((NOT ((sub_647.col_1 IS NULL))), 0)) ELSE ABS(sub_647.col_3) END) - (SELECT (SELECT inn_650.col_2 FROM tbl_04 AS inn_650 LIMIT 1) FROM tbl_07 AS sub_649 LIMIT 1))) THEN sub_647.col_1 ELSE ((0.0 IN (SELECT sub_651.col_1 FROM tbl_10 AS sub_651)) * (LOWER((COALESCE(LENGTH(TRUE), (3.14159265 > sub_647.col_3)) BETWEEN -9223372036854775808 AND 42)) + ABS(CAST((SELECT (SELECT inn_654.col_3 FROM tbl_01 AS inn_654 LIMIT 1) FROM tbl_04 AS sub_653 LIMIT 1) AS CHAR)))) END) FROM tbl_07 AS sub_647 LIMIT 1) END) <> EXISTS (SELECT 1 FROM tbl_02 AS sub_655 WHERE 'chaos'))) END) + LOWER((t_ref_620.ref_tbl_01_id IN (SELECT sub_659.col_3 FROM tbl_10 AS sub_659)))) + t_ref_620.col_2)) / NULLIF(((SELECT (sub_660.col_2 BETWEEN -1 AND 42) FROM tbl_03 AS sub_660 LIMIT 1) + LENGTH(UPPER(t_ref_620.id))), 0)) AS c_661
FROM tbl_08 AS t_ref_620 LEFT JOIN tbl_09 AS j_ref_662 ON t_ref_620.id = j_ref_662.id
WHERE ((-1e-5 + t_ref_620.id) <> (((('__fuzz_marker__' / NULLIF(EXISTS (SELECT 1 FROM tbl_02 AS sub_663 WHERE sub_663.col_1), 0)) < CAST((CAST(('escaped''quote' IS NULL) AS SIGNED) > CAST((SELECT (NOT ((LENGTH(sub_664.id) * CAST(1e10 AS DECIMAL(10,2))))) FROM tbl_10 AS sub_664 LIMIT 1) AS DECIMAL(10,2))) AS CHAR)) = TRUE) > (NOT (CAST(((COALESCE(((COALESCE(UPPER(UPPER((NOT (CAST((NULLIF((t_ref_620.col_2 AND 100), (t_ref_620.ref_tbl_01_id <= -2147483648)) * ((t_ref_620.id - FALSE) OR (t_ref_620.col_2 / NULLIF('', 0)))) AS SIGNED))))), '__fuzz_marker__') = (NOT ((SELECT (SELECT TRUE FROM tbl_09 AS inn_668 LIMIT 1) FROM tbl_07 AS sub_665 LIMIT 1)))) + FALSE), (t_ref_620.col_1 + (CASE WHEN (SELECT (SELECT inn_672.id FROM tbl_06 AS inn_672 LIMIT 1) FROM tbl_09 AS sub_669 LIMIT 1) THEN FALSE ELSE ((CASE WHEN (SELECT (SELECT inn_674.col_4 FROM tbl_10 AS inn_674 LIMIT 1) FROM tbl_10 AS sub_673 LIMIT 1) THEN ((LENGTH((('ref_val' IN (SELECT sub_675.col_4 FROM tbl_10 AS sub_675)) > NULLIF(EXISTS (SELECT 1 FROM tbl_07 AS sub_676 WHERE sub_676.id), (t_ref_620.ref_tbl_01_id <= t_ref_620.ref_tbl_01_id)))) / NULLIF((((SELECT FALSE FROM tbl_06 AS sub_677 LIMIT 1) - (COALESCE(t_ref_620.ref_tbl_01_id, t_ref_620.col_1) + (t_ref_620.col_3 / NULLIF(TRUE, 0)))) - ((t_ref_620.id IN (SELECT sub_678.id FROM tbl_07 AS sub_678)) / NULLIF(COALESCE((TRUE * t_ref_620.id), ABS(t_ref_620.col_3)), 0))), 0)) * ((100 IN (SELECT sub_679.col_1 FROM tbl_06 AS sub_679)) + t_ref_620.col_3)) ELSE (CASE WHEN (t_ref_620.col_3 IN (SELECT sub_681.id FROM tbl_05 AS sub_681)) THEN ((EXISTS (SELECT 1 FROM tbl_08 AS sub_683 WHERE sub_683.col_3) * t_ref_620.col_3) >= (SELECT (CASE WHEN sub_686.id THEN (sub_686.id = sub_686.col_3) ELSE sub_686.col_1 END) FROM tbl_07 AS sub_686 LIMIT 1)) ELSE (t_ref_620.col_3 = EXISTS (SELECT 1 FROM tbl_04 AS sub_687 WHERE sub_687.col_2)) END) END) * COALESCE(CAST((CASE WHEN (SELECT (SELECT (SELECT 'ref_val' FROM tbl_07 AS inn_690 LIMIT 1) FROM tbl_10 AS sub_689 LIMIT 1) FROM tbl_02 AS sub_688 LIMIT 1) THEN ((NOT ((ABS(t_ref_620.col_2) <= (t_ref_620.col_1 AND t_ref_620.col_2)))) IS NULL) ELSE ABS(UPPER(((NOT (t_ref_620.col_1)) BETWEEN 100 AND -9223372036854775808))) END) AS CHAR), t_ref_620.ref_tbl_01_id)) END))) / NULLIF(((SELECT sub_691.id FROM tbl_06 AS sub_691 LIMIT 1) >= t_ref_620.col_3), 0)) - t_ref_620.col_2) AS DECIMAL(10,2))))))
LIMIT 13;

DELETE FROM tbl_03 WHERE id = 92722;

INSERT INTO tbl_01 (id, col_3, dyn_col_3) VALUES (805443, -1.5, 'chaos');

SELECT
    (3.14159265 OR CAST(t_ref_692.col_3 AS SIGNED)) AS c_693,
    (((SELECT ((SELECT (SELECT TRUE FROM tbl_02 AS inn_704 LIMIT 1) FROM tbl_03 AS sub_695 LIMIT 1) BETWEEN -1 AND -2147483648) FROM asset_meta AS sub_694 LIMIT 1) * NULL) * LOWER((SELECT (SELECT -1e-5 FROM tbl_06 AS inn_710 LIMIT 1) FROM tbl_08 AS sub_705 LIMIT 1))) AS c_711
FROM tbl_05 AS t_ref_692 LEFT JOIN asset_meta AS j_ref_712 ON t_ref_692.id = j_ref_712.id
WHERE (t_ref_692.id = (9223372036854775807 - ((((SELECT LOWER((1e10 <> (((sub_713.col_3 IS NOT NULL) = ((sub_713.id * 0) - CAST((sub_713.col_3 <= (SELECT (SELECT inn_715.col_1 FROM tbl_05 AS inn_715 LIMIT 1) FROM tbl_03 AS sub_714 LIMIT 1)) AS CHAR))) - (CASE WHEN (LENGTH(CAST(EXISTS (SELECT 1 FROM tbl_08 AS sub_716 WHERE FALSE) AS SIGNED)) >= FALSE) THEN (SELECT COALESCE(-1.5, (SELECT (SELECT 42 FROM tbl_01 AS inn_719 LIMIT 1) FROM tbl_07 AS sub_718 LIMIT 1)) FROM tbl_07 AS sub_717 LIMIT 1) ELSE (SELECT (SELECT 0.0000000001 FROM tbl_04 AS inn_721 LIMIT 1) FROM asset_meta AS sub_720 LIMIT 1) END)))) FROM tbl_04 AS sub_713 LIMIT 1) OR 'ref_val') OR (((COALESCE(((SELECT (SELECT -1 FROM tbl_05 AS inn_726 LIMIT 1) FROM tbl_05 AS sub_722 LIMIT 1) - t_ref_692.col_2), (TRUE * (SELECT (SELECT inn_728.col_1 FROM tbl_04 AS inn_728 LIMIT 1) FROM tbl_03 AS sub_727 LIMIT 1))) * (SELECT (SELECT 0.0000000001 FROM tbl_05 AS inn_733 LIMIT 1) FROM tbl_04 AS sub_729 LIMIT 1)) BETWEEN 42 AND 1) BETWEEN 9223372036854775807 AND 42)) >= UPPER((SELECT (SELECT 'chaos' FROM tbl_10 AS inn_744 LIMIT 1) FROM tbl_02 AS sub_734 LIMIT 1)))))
LIMIT 46;

SELECT
    100 AS c_746,
    LENGTH((((((EXISTS (SELECT 1 FROM tbl_07 AS sub_747 WHERE sub_747.col_1) IS NOT NULL) / NULLIF(-1e-5, 0)) <= ((((SELECT (sub_755.dyn_col_3 IN (SELECT sub_756.payload FROM asset_meta AS sub_756)) FROM tbl_01 AS sub_755 LIMIT 1) - (EXISTS (SELECT 1 FROM tbl_04 AS sub_760 WHERE sub_760.col_1) + (SELECT (sub_765.col_2 IN (SELECT sub_766.col_2 FROM tbl_10 AS sub_766)) FROM tbl_05 AS sub_765 LIMIT 1))) - ((t_ref_745.col_1 IN (SELECT sub_767.col_3 FROM tbl_01 AS sub_767)) >= LENGTH((t_ref_745.col_2 IN (SELECT sub_780.id FROM tbl_02 AS sub_780))))) <> (t_ref_745.col_2 >= t_ref_745.col_1))) AND COALESCE(CAST((CASE WHEN CAST(((t_ref_745.col_3 IN (SELECT sub_782.col_1 FROM tbl_04 AS sub_782)) / NULLIF(t_ref_745.col_1, 0)) AS SIGNED) THEN -1.5 ELSE (t_ref_745.col_2 IN (SELECT sub_795.id FROM asset_meta AS sub_795)) END) AS DECIMAL(10,2)), (((((t_ref_745.id AND (LENGTH(t_ref_745.col_3) + (CASE WHEN (((((t_ref_745.col_1 IN (SELECT sub_797.ref_tbl_03_id FROM tbl_09 AS sub_797)) < ABS(EXISTS (SELECT 1 FROM tbl_01 AS sub_798 WHERE sub_798.dyn_col_3))) BETWEEN -1 AND 1) / NULLIF((CASE WHEN CAST(t_ref_745.id AS SIGNED) THEN NULLIF(LOWER(FALSE), (CAST(TRUE AS DECIMAL(10,2)) <= (SELECT sub_799.col_4 FROM tbl_06 AS sub_799 LIMIT 1))) ELSE (SELECT (sub_800.payload OR sub_800.id) FROM asset_meta AS sub_800 LIMIT 1) END), 0)) - (((LOWER((t_ref_745.col_2 IN (SELECT sub_801.col_2 FROM tbl_10 AS sub_801))) BETWEEN 0 AND 2147483647) OR (LOWER(LENGTH('alpha_beta_gamma')) AND ((999999999.99 + '') < (SELECT '' FROM tbl_09 AS sub_802 LIMIT 1)))) <> CAST(LENGTH(((SELECT (SELECT 'chaos' FROM tbl_10 AS inn_804 LIMIT 1) FROM tbl_06 AS sub_803 LIMIT 1) + CAST(t_ref_745.col_2 AS CHAR))) AS SIGNED))) THEN TRUE ELSE CAST((((EXISTS (SELECT 1 FROM tbl_07 AS sub_805 WHERE sub_805.id) / NULLIF((ABS(t_ref_745.col_3) BETWEEN -2147483648 AND 0), 0)) <> ((t_ref_745.col_1 < (t_ref_745.id - t_ref_745.col_3)) + CAST((t_ref_745.col_2 - t_ref_745.col_3) AS CHAR))) / NULLIF(2147483647, 0)) AS CHAR) END))) < UPPER((TRUE IN (SELECT sub_806.col_1 FROM tbl_02 AS sub_806)))) / NULLIF((((FALSE < t_ref_745.col_3) + t_ref_745.col_2) OR 3.14159265), 0)) * ('escaped''quote' * ((CASE WHEN ((t_ref_745.col_3 BETWEEN -1 AND 0) AND t_ref_745.col_3) THEN t_ref_745.col_3 ELSE (t_ref_745.col_3 AND (LENGTH(EXISTS (SELECT 1 FROM tbl_04 AS sub_812 WHERE sub_812.col_2)) + ((t_ref_745.id - 0) + (CAST((SELECT (SELECT (SELECT inn_815.payload FROM asset_meta AS inn_815 LIMIT 1) FROM tbl_04 AS sub_814 LIMIT 1) FROM tbl_04 AS sub_813 LIMIT 1) AS SIGNED) <= CAST(NULLIF(FALSE, ((FALSE IN (SELECT sub_816.col_1 FROM tbl_04 AS sub_816)) - ('ref_val' * t_ref_745.col_1))) AS SIGNED))))) END) - (((CASE WHEN ((CAST(LOWER(EXISTS (SELECT 1 FROM tbl_03 AS sub_817 WHERE 1.0)) AS SIGNED) IS NULL) - (CASE WHEN (CAST(CAST((t_ref_745.col_3 < t_ref_745.col_1) AS CHAR) AS CHAR) - CAST((SELECT 3.14159265 FROM tbl_06 AS sub_818 LIMIT 1) AS SIGNED)) THEN t_ref_745.col_2 ELSE (((SELECT (SELECT inn_820.id FROM tbl_04 AS inn_820 LIMIT 1) FROM asset_meta AS sub_819 LIMIT 1) <> (t_ref_745.col_2 = (1.0 < t_ref_745.col_3))) + 0) END)) THEN ((SELECT (SELECT inn_822.id FROM asset_meta AS inn_822 LIMIT 1) FROM tbl_10 AS sub_821 LIMIT 1) BETWEEN -1 AND 0) ELSE EXISTS (SELECT 1 FROM tbl_03 AS sub_823 WHERE sub_823.col_4) END) BETWEEN 100 AND 2147483647) - CAST(((CASE WHEN NULL THEN LOWER(('' BETWEEN -9223372036854775808 AND 1)) ELSE EXISTS (SELECT 1 FROM tbl_06 AS sub_826 WHERE 'chaos') END) IS NOT NULL) AS SIGNED))))) + ((CAST(((-9223372036854775808 / NULLIF(NULLIF(((FALSE - t_ref_745.id) < UPPER(t_ref_745.col_2)), EXISTS (SELECT 1 FROM tbl_09 AS sub_828 WHERE sub_828.ref_tbl_03_id)), 0)) / NULLIF((NOT ((FALSE IN (SELECT sub_829.col_3 FROM tbl_06 AS sub_829)))), 0)) AS CHAR) - CAST(((TRUE > (CAST(((TRUE > -9223372036854775808) <> ('__fuzz_marker__' = t_ref_745.col_3)) AS DECIMAL(10,2)) OR -2147483648)) + ((t_ref_745.col_3 = ((CASE WHEN FALSE THEN TRUE ELSE (t_ref_745.col_3 IN (SELECT sub_831.col_2 FROM tbl_10 AS sub_831)) END) / NULLIF(UPPER((CAST(0.0000000001 AS SIGNED) < t_ref_745.id)), 0))) * t_ref_745.col_1)) AS CHAR)) > (((((t_ref_745.col_3 < ((t_ref_745.col_3 IN (SELECT sub_832.id FROM tbl_04 AS sub_832)) OR t_ref_745.col_2)) AND ABS(((t_ref_745.col_1 IN (SELECT sub_835.col_4 FROM tbl_06 AS sub_835)) - (NULLIF(CAST((CAST(t_ref_745.id AS DECIMAL(10,2)) - (t_ref_745.col_2 BETWEEN -9223372036854775808 AND 1)) AS SIGNED), LOWER((t_ref_745.id = t_ref_745.col_1))) + 42)))) IS NULL) / NULLIF(t_ref_745.col_1, 0)) + t_ref_745.col_3))))) / NULLIF((SELECT sub_836.col_1 FROM tbl_09 AS sub_836 LIMIT 1), 0))) AS c_837,
    UPPER(UPPER(COALESCE(t_ref_745.col_3, (CASE WHEN (LOWER((EXISTS (SELECT 1 FROM tbl_06 AS sub_838 WHERE sub_838.col_1) - ((SELECT (sub_842.col_1 IN (SELECT sub_843.id FROM tbl_02 AS sub_843)) FROM tbl_09 AS sub_842 LIMIT 1) BETWEEN -1 AND 42))) - (NOT (CAST((NULLIF(LOWER(CAST(t_ref_745.col_2 AS SIGNED)), ((SELECT (SELECT inn_848.col_1 FROM tbl_09 AS inn_848 LIMIT 1) FROM tbl_06 AS sub_844 LIMIT 1) / NULLIF((LENGTH((LENGTH(EXISTS (SELECT 1 FROM tbl_08 AS sub_849 WHERE sub_849.id)) = t_ref_745.col_3)) + t_ref_745.col_2), 0))) / NULLIF((CASE WHEN LENGTH(NULLIF(CAST(t_ref_745.id AS DECIMAL(10,2)), LOWER((NULLIF(t_ref_745.col_1, LOWER((CAST(t_ref_745.col_2 AS CHAR) <= CAST(t_ref_745.col_2 AS CHAR)))) - (LENGTH(((NULL < t_ref_745.id) <= (SELECT (SELECT inn_851.col_1 FROM tbl_08 AS inn_851 LIMIT 1) FROM tbl_08 AS sub_850 LIMIT 1))) / NULLIF(('chaos' IS NOT NULL), 0)))))) THEN ABS((('escaped''quote' * t_ref_745.col_1) >= (SELECT (SELECT inn_853.col_1 FROM tbl_05 AS inn_853 LIMIT 1) FROM tbl_06 AS sub_852 LIMIT 1))) ELSE NULLIF(FALSE, ABS((SELECT (NOT (CAST(CAST((SELECT (SELECT inn_856.id FROM asset_meta AS inn_856 LIMIT 1) FROM tbl_10 AS sub_855 LIMIT 1) AS DECIMAL(10,2)) AS CHAR))) FROM tbl_04 AS sub_854 LIMIT 1))) END), 0)) AS DECIMAL(10,2))))) THEN CAST(((NOT (CAST(EXISTS (SELECT 1 FROM tbl_02 AS sub_857 WHERE sub_857.id) AS SIGNED))) AND CAST(t_ref_745.col_2 AS SIGNED)) AS DECIMAL(10,2)) ELSE LOWER(CAST(COALESCE(LENGTH((EXISTS (SELECT 1 FROM tbl_05 AS sub_858 WHERE sub_858.col_3) * (t_ref_745.col_3 IN (SELECT sub_860.col_2 FROM tbl_06 AS sub_860)))), LENGTH(CAST(EXISTS (SELECT 1 FROM tbl_05 AS sub_861 WHERE sub_861.id) AS DECIMAL(10,2)))) AS CHAR)) END)))) AS c_862
FROM tbl_05 AS t_ref_745
WHERE ((SELECT (SELECT inn_870.id FROM tbl_07 AS inn_870 LIMIT 1) FROM tbl_03 AS sub_863 LIMIT 1) * COALESCE((CASE WHEN EXISTS (SELECT 1 FROM tbl_04 AS sub_871 WHERE sub_871.col_4) THEN ((t_ref_745.col_1 IN (SELECT sub_874.ref_tbl_03_id FROM tbl_09 AS sub_874)) * (2147483647 - (EXISTS (SELECT 1 FROM tbl_03 AS sub_886 WHERE sub_886.col_3) + TRUE))) ELSE (CASE WHEN LOWER(t_ref_745.id) THEN (t_ref_745.col_1 IN (SELECT sub_887.col_3 FROM tbl_07 AS sub_887)) ELSE t_ref_745.col_1 END) END), LENGTH(((t_ref_745.col_1 IN (SELECT sub_888.col_1 FROM tbl_02 AS sub_888)) <= NULLIF((CASE WHEN ((SELECT ((((((SELECT (3.14159265 - sub_890.col_1) FROM tbl_05 AS sub_890 LIMIT 1) IS NOT NULL) < CAST(TRUE AS DECIMAL(10,2))) <> (sub_889.id IN (SELECT sub_891.ref_tbl_01_id FROM tbl_02 AS sub_891))) <= 'ref_val') < TRUE) FROM tbl_09 AS sub_889 LIMIT 1) - t_ref_745.col_2) THEN ((t_ref_745.col_3 IS NULL) * (SELECT (SELECT inn_897.id FROM tbl_10 AS inn_897 LIMIT 1) FROM tbl_06 AS sub_894 LIMIT 1)) ELSE (NULLIF((COALESCE(t_ref_745.col_1, (LOWER(((CAST((LOWER(t_ref_745.col_1) > (t_ref_745.col_1 > 1e10)) AS DECIMAL(10,2)) + EXISTS (SELECT 1 FROM tbl_09 AS sub_898 WHERE sub_898.col_1)) * (NULLIF(CAST(t_ref_745.id AS CHAR), (CAST('escaped''quote' AS DECIMAL(10,2)) >= (SELECT (SELECT FALSE FROM tbl_08 AS inn_900 LIMIT 1) FROM tbl_06 AS sub_899 LIMIT 1))) = (CASE WHEN (t_ref_745.id IS NULL) THEN ((SELECT TRUE FROM tbl_04 AS sub_901 LIMIT 1) IS NULL) ELSE t_ref_745.id END)))) < COALESCE((EXISTS (SELECT 1 FROM tbl_03 AS sub_902 WHERE sub_902.id) OR (t_ref_745.id IN (SELECT sub_903.col_4 FROM tbl_03 AS sub_903))), ((t_ref_745.id IN (SELECT sub_904.id FROM tbl_09 AS sub_904)) / NULLIF(LENGTH(((CASE WHEN (t_ref_745.col_3 * t_ref_745.col_2) THEN (FALSE * t_ref_745.col_2) ELSE (t_ref_745.col_3 >= t_ref_745.col_1) END) * t_ref_745.col_2)), 0))))) BETWEEN 100 AND 100), ABS((SELECT (SELECT inn_911.dyn_col_3 FROM tbl_01 AS inn_911 LIMIT 1) FROM tbl_07 AS sub_905 LIMIT 1))) - NULL) END), ABS(t_ref_745.col_2))))))
LIMIT 17;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (817624, FALSE, '__fuzz_marker__', FALSE, 100);
INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (858384, 999999999.99, '[]', 0);
COMMIT;

INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4) VALUES (422296, -9223372036854775808, '__fuzz_marker__', 2147483647, '[]');

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (888018, 'alpha_beta_gamma', 'chaos', -9223372036854775808, 'ref_val');

BEGIN;
INSERT INTO tbl_09 (id, col_1, ref_tbl_03_id) VALUES (150718, 3.14159265, 100);
INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4) VALUES (262212, -9223372036854775808, '', 0, '[]');
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (199209, 'chaos', 3.14159265, -1, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
COMMIT;

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (560880, 1.0, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 2147483647);
INSERT INTO tbl_09 (id, col_1, ref_tbl_03_id) VALUES (983810, -1.5, 100);
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (14127, '', 'alpha_beta_gamma', -9223372036854775808, 'alpha_beta_gamma');
COMMIT;

CREATE VIEW v_summary_8 AS SELECT id FROM tbl_07 WHERE id IS NOT NULL;

DELETE FROM tbl_07 WHERE id = 99587;

BEGIN;
INSERT INTO tbl_01 (id, col_3, dyn_col_3) VALUES (142710, 1.0, 'escaped''quote');
COMMIT;

INSERT INTO asset_meta (id, payload) VALUES (196900, 'chaos');

DELETE FROM tbl_03 WHERE id = 2660;

SELECT
    t_ref_912.id AS c_913,
    (t_ref_912.payload * (-1e-5 > t_ref_912.id)) AS c_914,
    FALSE AS c_915
FROM asset_meta AS t_ref_912
WHERE ((t_ref_912.id * (t_ref_912.id - COALESCE(CAST(UPPER((CASE WHEN t_ref_912.payload THEN (CAST((SELECT (SELECT 9223372036854775807 FROM tbl_03 AS inn_919 LIMIT 1) FROM asset_meta AS sub_916 LIMIT 1) AS DECIMAL(10,2)) - EXISTS (SELECT 1 FROM tbl_09 AS sub_920 WHERE sub_920.ref_tbl_03_id)) ELSE FALSE END)) AS DECIMAL(10,2)), (ABS((t_ref_912.id IN (SELECT sub_924.payload FROM asset_meta AS sub_924))) / NULLIF(999999999.99, 0))))) / NULLIF(0.0000000001, 0))
LIMIT 3;

SELECT
    (1e10 BETWEEN -2147483648 AND 2147483647) AS c_937
FROM tbl_07 AS t_ref_936
WHERE (((SELECT sub_938.col_2 FROM tbl_06 AS sub_938 LIMIT 1) BETWEEN 9223372036854775807 AND 42) * (SELECT (SELECT 3.14159265 FROM tbl_03 AS inn_940 LIMIT 1) FROM tbl_06 AS sub_939 LIMIT 1))
LIMIT 26;

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON
);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (436960, '__fuzz_marker__', 'ref_val', 2147483647, '__fuzz_marker__');

CREATE INDEX idx_tbl_08_ref_tbl_01_id_3 ON tbl_08 (ref_tbl_01_id);

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (859300, 3.14159265, '[]', -1);

INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4) VALUES (88347, 42, 'escaped''quote', -2147483648, '{}');

SELECT
    LOWER(LENGTH(t_ref_941.col_3)) AS c_942,
    ((FALSE IS NULL) * (SELECT 999999999.99 FROM asset_meta AS sub_943 LIMIT 1)) AS c_944,
    t_ref_941.col_1 AS c_945
FROM tbl_05 AS t_ref_941 LEFT JOIN tbl_09 AS j_ref_946 ON t_ref_941.id = j_ref_946.id
WHERE (SELECT (SELECT inn_950.id FROM tbl_09 AS inn_950 LIMIT 1) FROM tbl_11 AS sub_947 LIMIT 1)
LIMIT 45;

ALTER TABLE tbl_08 DROP COLUMN col_3;

INSERT INTO tbl_01 (id, col_3, dyn_col_3) VALUES (203699, 3.14159265, '');

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (486128, 1e10, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 9223372036854775807);

SELECT
    (CASE WHEN (t_ref_951.col_4 < '__fuzz_marker__') THEN COALESCE(0.0, (SELECT (sub_952.col_1 IS NOT NULL) FROM tbl_07 AS sub_952 LIMIT 1)) ELSE t_ref_951.col_2 END) AS c_953
FROM tbl_06 AS t_ref_951 LEFT JOIN tbl_10 AS j_ref_954 ON t_ref_951.id = j_ref_954.id
WHERE t_ref_951.col_3
LIMIT 9;

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 BOOLEAN,
    ref_tbl_09_id INT,
    FOREIGN KEY (ref_tbl_09_id) REFERENCES tbl_09(id)
);

UPDATE tbl_04 SET col_2 = '__fuzz_marker__' WHERE id > 78;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (833112, FALSE, 'ref_val', FALSE, 9223372036854775807);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (841026, '', 'ref_val', -1, 'escaped''quote');

UPDATE tbl_03 SET col_4 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 16;

CREATE VIEW v_summary_9 AS SELECT id FROM tbl_06 WHERE id IS NOT NULL;

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSON,
    ref_tbl_08_id INT,
    FOREIGN KEY (ref_tbl_08_id) REFERENCES tbl_08(id)
);

INSERT INTO tbl_11 (id, col_1, col_2) VALUES (518884, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

DELETE FROM tbl_03 WHERE id = 58211;

UPDATE tbl_06 SET col_2 = 'chaos' WHERE id > 67;

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (835443, '', '__fuzz_marker__', 1, 'chaos');

DELETE FROM tbl_07 WHERE id = 75108;

CREATE INDEX idx_tbl_10_col_1_4 ON tbl_10 (col_1);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (325239, 'ref_val', 0.0, 2147483647, '{}');

INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (622039, 9223372036854775807, 42, 'escaped''quote');

ALTER TABLE tbl_10 ADD COLUMN dyn_col_6 VARCHAR(32);

INSERT INTO tbl_13 (id, col_1, col_2, ref_tbl_08_id) VALUES (279229, -1, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 9223372036854775807);

INSERT INTO tbl_11 (id, col_1, col_2) VALUES (482115, '{}', '{}');

SELECT
    TRUE AS c_956,
    (SELECT 999999999.99 FROM tbl_12 AS sub_957 LIMIT 1) AS c_958,
    ABS((ABS(t_ref_955.col_1) * ((CAST(((SELECT CAST(NULL AS SIGNED) FROM tbl_08 AS sub_959 LIMIT 1) - CAST(EXISTS (SELECT 1 FROM tbl_13 AS sub_960 WHERE sub_960.col_2) AS CHAR)) AS DECIMAL(10,2)) >= ABS((CASE WHEN (((CAST(COALESCE((-1.5 * NULLIF((((SELECT sub_967.col_3 FROM tbl_07 AS sub_967 LIMIT 1) + EXISTS (SELECT 1 FROM tbl_02 AS sub_968 WHERE 0)) <= (EXISTS (SELECT 1 FROM tbl_08 AS sub_969 WHERE sub_969.ref_tbl_01_id) * -2147483648)), (t_ref_955.col_1 < CAST(UPPER(CAST(t_ref_955.col_1 AS DECIMAL(10,2))) AS CHAR)))), t_ref_955.id) AS DECIMAL(10,2)) IS NOT NULL) + (CASE WHEN (CAST((CASE WHEN COALESCE((CAST((SELECT 999999999.99 FROM tbl_10 AS sub_970 LIMIT 1) AS DECIMAL(10,2)) >= (FALSE IN (SELECT sub_971.id FROM tbl_10 AS sub_971))), LOWER(LENGTH(CAST(('ref_val' - -1e-5) AS DECIMAL(10,2))))) THEN LENGTH(((CAST(('__fuzz_marker__' BETWEEN 9223372036854775807 AND 2147483647) AS SIGNED) / NULLIF(((CASE WHEN t_ref_955.ref_tbl_03_id THEN t_ref_955.id ELSE 1e10 END) - (NOT (100))), 0)) < t_ref_955.col_1)) ELSE UPPER(CAST((SELECT LENGTH(sub_972.ref_tbl_09_id) FROM tbl_12 AS sub_972 LIMIT 1) AS DECIMAL(10,2))) END) AS DECIMAL(10,2)) OR (UPPER((TRUE + COALESCE((SELECT (SELECT inn_975.col_1 FROM tbl_05 AS inn_975 LIMIT 1) FROM tbl_06 AS sub_973 LIMIT 1), (CAST((SELECT (SELECT 'escaped''quote' FROM tbl_02 AS inn_977 LIMIT 1) FROM asset_meta AS sub_976 LIMIT 1) AS SIGNED) >= CAST((TRUE - t_ref_955.ref_tbl_03_id) AS DECIMAL(10,2)))))) * (((SELECT 3.14159265 FROM tbl_06 AS sub_978 LIMIT 1) + (t_ref_955.id IN (SELECT sub_979.id FROM tbl_12 AS sub_979))) < (((t_ref_955.id IN (SELECT sub_980.col_1 FROM tbl_05 AS sub_980)) AND EXISTS (SELECT 1 FROM tbl_09 AS sub_981 WHERE sub_981.id)) >= ((((CASE WHEN t_ref_955.ref_tbl_03_id THEN t_ref_955.ref_tbl_03_id ELSE 1.0 END) - (CASE WHEN t_ref_955.id THEN t_ref_955.id ELSE t_ref_955.col_1 END)) * (SELECT (SELECT 2147483647 FROM tbl_05 AS inn_983 LIMIT 1) FROM tbl_07 AS sub_982 LIMIT 1)) + TRUE))))) THEN NULL ELSE (CASE WHEN ((NULLIF(CAST(t_ref_955.id AS SIGNED), NULLIF(EXISTS (SELECT 1 FROM tbl_01 AS sub_984 WHERE sub_984.id), t_ref_955.ref_tbl_03_id)) AND t_ref_955.col_1) <= NULL) THEN (UPPER((TRUE IN (SELECT sub_985.col_3 FROM tbl_03 AS sub_985))) > (((SELECT COALESCE(CAST(-1.5 AS DECIMAL(10,2)), UPPER(sub_988.col_1)) FROM tbl_05 AS sub_988 LIMIT 1) - t_ref_955.id) - (SELECT (((sub_989.col_1 BETWEEN 100 AND 0) <> UPPER(sub_989.dyn_col_4)) = (SELECT (SELECT TRUE FROM tbl_04 AS inn_991 LIMIT 1) FROM tbl_07 AS sub_990 LIMIT 1)) FROM tbl_02 AS sub_989 LIMIT 1))) ELSE EXISTS (SELECT 1 FROM tbl_11 AS sub_992 WHERE sub_992.id) END) END)) <> (NULL IN (SELECT sub_997.col_4 FROM tbl_06 AS sub_997))) THEN EXISTS (SELECT 1 FROM tbl_10 AS sub_1001 WHERE NULL) ELSE (CASE WHEN t_ref_955.id THEN (SELECT CAST(((LENGTH(NULLIF(NULLIF(CAST((TRUE AND FALSE) AS CHAR), sub_1002.id), sub_1002.col_4)) - sub_1002.col_2) - sub_1002.col_4) AS SIGNED) FROM tbl_04 AS sub_1002 LIMIT 1) ELSE ((((((t_ref_955.ref_tbl_03_id IN (SELECT sub_1003.ref_tbl_03_id FROM tbl_09 AS sub_1003)) * (ABS(FALSE) >= (SELECT sub_1004.col_3 FROM tbl_06 AS sub_1004 LIMIT 1))) - (TRUE IN (SELECT sub_1005.col_1 FROM tbl_07 AS sub_1005))) - (SELECT sub_1006.col_1 FROM tbl_09 AS sub_1006 LIMIT 1)) >= t_ref_955.ref_tbl_03_id) * (FALSE + (CAST((t_ref_955.id + t_ref_955.ref_tbl_03_id) AS SIGNED) OR 9223372036854775807))) END) END))) / NULLIF(((CASE WHEN CAST(LOWER(CAST(t_ref_955.id AS DECIMAL(10,2))) AS CHAR) THEN LENGTH((EXISTS (SELECT 1 FROM tbl_06 AS sub_1007 WHERE sub_1007.id) OR (((((ABS(LENGTH((t_ref_955.col_1 * (t_ref_955.id IN (SELECT sub_1012.col_3 FROM tbl_07 AS sub_1012))))) AND (EXISTS (SELECT 1 FROM tbl_12 AS sub_1013 WHERE TRUE) / NULLIF(ABS(NULL), 0))) >= (3.14159265 IN (SELECT sub_1014.id FROM tbl_07 AS sub_1014))) < 'chaos') + (((SELECT (SELECT inn_1016.col_2 FROM tbl_05 AS inn_1016 LIMIT 1) FROM tbl_01 AS sub_1015 LIMIT 1) / NULLIF((t_ref_955.ref_tbl_03_id / NULLIF(t_ref_955.ref_tbl_03_id, 0)), 0)) <> (EXISTS (SELECT 1 FROM tbl_06 AS sub_1017 WHERE sub_1017.col_1) / NULLIF('alpha_beta_gamma', 0)))) + (CASE WHEN LENGTH(COALESCE((NOT ((t_ref_955.col_1 IN (SELECT sub_1018.col_2 FROM tbl_13 AS sub_1018)))), (SELECT (SELECT TRUE FROM tbl_07 AS inn_1023 LIMIT 1) FROM tbl_10 AS sub_1020 LIMIT 1))) THEN ((SELECT CAST(sub_1024.payload AS CHAR) FROM asset_meta AS sub_1024 LIMIT 1) < EXISTS (SELECT 1 FROM tbl_10 AS sub_1025 WHERE sub_1025.id)) ELSE EXISTS (SELECT 1 FROM tbl_04 AS sub_1026 WHERE sub_1026.col_1) END)))) ELSE LOWER(LOWER((CAST(((SELECT (sub_1029.ref_tbl_03_id IN (SELECT sub_1030.col_1 FROM tbl_11 AS sub_1030)) FROM tbl_09 AS sub_1029 LIMIT 1) IS NULL) AS DECIMAL(10,2)) + CAST(LENGTH(NULL) AS SIGNED)))) END) + CAST((t_ref_955.ref_tbl_03_id / NULLIF((((t_ref_955.ref_tbl_03_id + (SELECT COALESCE((SELECT (sub_1035.id IN (SELECT sub_1036.id FROM tbl_11 AS sub_1036)) FROM tbl_05 AS sub_1035 LIMIT 1), (((SELECT (SELECT inn_1038.id FROM tbl_11 AS inn_1038 LIMIT 1) FROM tbl_06 AS sub_1037 LIMIT 1) / NULLIF((SELECT (SELECT FALSE FROM tbl_04 AS inn_1040 LIMIT 1) FROM tbl_13 AS sub_1039 LIMIT 1), 0)) = NULL)) FROM tbl_13 AS sub_1034 LIMIT 1)) - ((SELECT (TRUE = (((LOWER((NULL + sub_1041.col_2)) - sub_1041.col_1) OR (SELECT (SELECT inn_1043.col_2 FROM tbl_13 AS inn_1043 LIMIT 1) FROM tbl_06 AS sub_1042 LIMIT 1)) IS NULL)) FROM tbl_12 AS sub_1041 LIMIT 1) BETWEEN 9223372036854775807 AND 42)) BETWEEN 1 AND 100), 0)) AS SIGNED)), 0)))) AS c_1044
FROM tbl_09 AS t_ref_955
WHERE TRUE
LIMIT 40;

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN,
    col_3 VARCHAR(64),
    col_4 VARCHAR(64)
);

CREATE VIEW v_summary_10 AS SELECT id FROM tbl_12 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (219914, 2147483647, -2147483648, '__fuzz_marker__');
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (996654, '__fuzz_marker__', 0.0000000001, 9223372036854775807, '[]');
COMMIT;

SELECT
    t_ref_1045.col_2 AS c_1046,
    t_ref_1045.id AS c_1047
FROM tbl_08 AS t_ref_1045
WHERE (((t_ref_1045.ref_tbl_01_id * CAST(UPPER((t_ref_1045.col_1 IN (SELECT sub_1048.id FROM tbl_06 AS sub_1048))) AS SIGNED)) / NULLIF((t_ref_1045.id / NULLIF(((CASE WHEN ((CAST(((NOT (LENGTH(UPPER((SELECT (SELECT 'alpha_beta_gamma' FROM tbl_13 AS inn_1064 LIMIT 1) FROM tbl_02 AS sub_1060 LIMIT 1))))) > t_ref_1045.id) AS CHAR) <= FALSE) + EXISTS (SELECT 1 FROM tbl_03 AS sub_1065 WHERE FALSE)) THEN (0 IN (SELECT sub_1073.id FROM tbl_07 AS sub_1073)) ELSE t_ref_1045.col_2 END) <= (EXISTS (SELECT 1 FROM tbl_09 AS sub_1082 WHERE sub_1082.id) <> 'chaos')), 0)), 0)) / NULLIF(LOWER((LOWER(((t_ref_1045.col_1 IN (SELECT sub_1092.col_3 FROM tbl_10 AS sub_1092)) + t_ref_1045.col_1)) <> COALESCE(LOWER((t_ref_1045.col_2 < t_ref_1045.ref_tbl_01_id)), t_ref_1045.col_1))), 0))
LIMIT 19;

SELECT
    t_ref_1093.col_3 AS c_1094,
    t_ref_1093.col_2 AS c_1095,
    (((ABS(((t_ref_1093.ref_tbl_09_id * t_ref_1093.col_1) <> t_ref_1093.col_1)) BETWEEN -2147483648 AND -9223372036854775808) - ((SELECT (SELECT inn_1098.col_3 FROM tbl_12 AS inn_1098 LIMIT 1) FROM tbl_06 AS sub_1096 LIMIT 1) AND ((CASE WHEN ((LOWER((EXISTS (SELECT 1 FROM tbl_04 AS sub_1099 WHERE sub_1099.col_4) / NULLIF((FALSE IN (SELECT sub_1102.col_3 FROM tbl_01 AS sub_1102)), 0))) OR (NULL / NULLIF(ABS((EXISTS (SELECT 1 FROM tbl_07 AS sub_1103 WHERE sub_1103.id) * FALSE)), 0))) + t_ref_1093.col_3) THEN (t_ref_1093.col_2 IN (SELECT sub_1104.payload FROM asset_meta AS sub_1104)) ELSE (SELECT (((LOWER(CAST((((CASE WHEN sub_1105.id THEN CAST((sub_1105.col_3 IN (SELECT sub_1106.id FROM tbl_07 AS sub_1106)) AS DECIMAL(10,2)) ELSE ((SELECT (SELECT 'ref_val' FROM tbl_06 AS inn_1108 LIMIT 1) FROM tbl_14 AS sub_1107 LIMIT 1) / NULLIF((sub_1105.col_2 / NULLIF(sub_1105.col_2, 0)), 0)) END) OR sub_1105.id) + ABS((sub_1105.col_2 IN (SELECT sub_1109.col_1 FROM tbl_03 AS sub_1109)))) AS DECIMAL(10,2))) + ((SELECT (SELECT (SELECT TRUE FROM tbl_12 AS sub_1112 LIMIT 1) FROM tbl_14 AS sub_1111 LIMIT 1) FROM tbl_12 AS sub_1110 LIMIT 1) - (sub_1105.col_1 / NULLIF(CAST((CASE WHEN 9223372036854775807 THEN sub_1105.col_2 ELSE sub_1105.col_2 END) AS CHAR), 0)))) / NULLIF(sub_1105.col_4, 0)) * CAST(COALESCE((((sub_1105.col_3 < (SELECT (sub_1113.id IN (SELECT sub_1114.dyn_col_3 FROM tbl_01 AS sub_1114)) FROM tbl_09 AS sub_1113 LIMIT 1)) / NULLIF(CAST(sub_1105.col_1 AS DECIMAL(10,2)), 0)) / NULLIF((UPPER(COALESCE(FALSE, ((SELECT sub_1115.col_2 FROM tbl_05 AS sub_1115 LIMIT 1) IS NOT NULL))) AND (CAST((CASE WHEN sub_1105.col_2 THEN (EXISTS (SELECT 1 FROM tbl_10 AS sub_1116 WHERE sub_1116.col_2) - CAST(sub_1105.id AS DECIMAL(10,2))) ELSE (SELECT (SELECT inn_1118.col_3 FROM tbl_12 AS inn_1118 LIMIT 1) FROM tbl_06 AS sub_1117 LIMIT 1) END) AS SIGNED) * sub_1105.col_4)), 0)), sub_1105.col_1) AS DECIMAL(10,2))) FROM tbl_04 AS sub_1105 LIMIT 1) END) * TRUE))) + (CASE WHEN ((999999999.99 IS NOT NULL) IS NULL) THEN (((t_ref_1093.id IN (SELECT sub_1119.col_1 FROM tbl_12 AS sub_1119)) - (t_ref_1093.id IN (SELECT sub_1120.col_1 FROM tbl_09 AS sub_1120))) - (((t_ref_1093.col_1 AND CAST((t_ref_1093.ref_tbl_09_id <> ((SELECT (sub_1124.col_1 >= sub_1124.ref_tbl_01_id) FROM tbl_08 AS sub_1124 LIMIT 1) * ((SELECT (SELECT NULL FROM tbl_07 AS inn_1130 LIMIT 1) FROM tbl_05 AS sub_1125 LIMIT 1) <= ABS((CASE WHEN t_ref_1093.col_3 THEN NULL ELSE COALESCE(EXISTS (SELECT 1 FROM tbl_01 AS sub_1131 WHERE sub_1131.dyn_col_3), LENGTH((SELECT sub_1132.col_3 FROM tbl_04 AS sub_1132 LIMIT 1))) END))))) AS DECIMAL(10,2))) + UPPER(LOWER(CAST((t_ref_1093.ref_tbl_09_id IN (SELECT sub_1133.dyn_col_6 FROM tbl_10 AS sub_1133)) AS DECIMAL(10,2))))) - FALSE)) ELSE t_ref_1093.ref_tbl_09_id END)) AS c_1137
FROM tbl_12 AS t_ref_1093
WHERE t_ref_1093.col_1
LIMIT 1;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (806816, TRUE, '', TRUE, 1);

DELETE FROM tbl_03 WHERE id = 67428;

SELECT
    UPPER((t_ref_1138.col_3 IN (SELECT sub_1139.id FROM tbl_08 AS sub_1139))) AS c_1142,
    (t_ref_1138.id IN (SELECT sub_1143.col_3 FROM tbl_14 AS sub_1143)) AS c_1157
FROM tbl_01 AS t_ref_1138
WHERE (t_ref_1138.col_3 IN (SELECT sub_1158.col_4 FROM tbl_14 AS sub_1158))
LIMIT 5;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_3) VALUES (187596, 3.14159265, 0.0);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (311681, '__fuzz_marker__', 0.0000000001, -1, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (419435, 'ref_val', -1.5, 100, '{}');
COMMIT;

SELECT
    t_ref_1159.col_4 AS c_1160,
    CAST((t_ref_1159.col_4 BETWEEN 1 AND 1) AS DECIMAL(10,2)) AS c_1161,
    (ABS('__fuzz_marker__') >= (FALSE - 1e10)) AS c_1162
FROM tbl_03 AS t_ref_1159
WHERE ((((CASE WHEN 1e10 THEN LENGTH(CAST(t_ref_1159.col_2 AS SIGNED)) ELSE (NULL IN (SELECT sub_1163.ref_tbl_03_id FROM tbl_09 AS sub_1163)) END) + (SELECT (SELECT inn_1165.col_2 FROM tbl_08 AS inn_1165 LIMIT 1) FROM tbl_10 AS sub_1164 LIMIT 1)) * (EXISTS (SELECT 1 FROM asset_meta AS sub_1166 WHERE sub_1166.payload) <= ((SELECT (SELECT (((UPPER((SELECT ((CAST(sub_1169.col_3 AS DECIMAL(10,2)) - CAST(sub_1169.id AS DECIMAL(10,2))) <> ((SELECT (SELECT inn_1171.col_3 FROM tbl_07 AS inn_1171 LIMIT 1) FROM tbl_08 AS sub_1170 LIMIT 1) + CAST(sub_1169.id AS DECIMAL(10,2)))) FROM tbl_14 AS sub_1169 LIMIT 1)) * CAST((((NOT (LOWER(('' IS NULL)))) BETWEEN -9223372036854775808 AND 1) <= 2147483647) AS SIGNED)) - sub_1168.col_3) * EXISTS (SELECT 1 FROM tbl_13 AS sub_1172 WHERE 1e10)) FROM tbl_07 AS sub_1168 LIMIT 1) FROM tbl_03 AS sub_1167 LIMIT 1) * t_ref_1159.col_2))) * (NULLIF(1, (CASE WHEN (EXISTS (SELECT 1 FROM tbl_01 AS sub_1177 WHERE sub_1177.col_3) + (NULL - ABS(CAST((NOT (COALESCE((0.0 + t_ref_1159.col_2), (CASE WHEN (NULLIF((((t_ref_1159.col_3 IN (SELECT sub_1185.col_1 FROM tbl_13 AS sub_1185)) <> 100) / NULLIF(CAST(((t_ref_1159.col_1 - t_ref_1159.col_2) + LENGTH(t_ref_1159.col_3)) AS DECIMAL(10,2)), 0)), CAST((t_ref_1159.col_3 * (CAST(9223372036854775807 AS DECIMAL(10,2)) AND (t_ref_1159.col_3 + t_ref_1159.col_4))) AS SIGNED)) > 999999999.99) THEN (SELECT (SELECT inn_1188.col_1 FROM tbl_02 AS inn_1188 LIMIT 1) FROM asset_meta AS sub_1186 LIMIT 1) ELSE t_ref_1159.col_3 END)))) AS CHAR)))) THEN NULLIF(ABS(CAST((CAST((CASE WHEN (t_ref_1159.col_4 = (t_ref_1159.col_3 IN (SELECT sub_1189.ref_tbl_08_id FROM tbl_13 AS sub_1189))) THEN (CASE WHEN ((TRUE - (FALSE IN (SELECT sub_1191.col_3 FROM tbl_04 AS sub_1191))) BETWEEN -9223372036854775808 AND 2147483647) THEN (CAST((FALSE * ((NOT ((SELECT sub_1192.id FROM tbl_02 AS sub_1192 LIMIT 1))) > ((CASE WHEN t_ref_1159.col_2 THEN t_ref_1159.col_3 ELSE t_ref_1159.col_3 END) AND (t_ref_1159.id OR t_ref_1159.col_4)))) AS CHAR) * (t_ref_1159.id >= (t_ref_1159.col_1 / NULLIF(CAST(((SELECT sub_1193.ref_tbl_01_id FROM tbl_08 AS sub_1193 LIMIT 1) > (NOT (t_ref_1159.col_2))) AS DECIMAL(10,2)), 0)))) ELSE NULLIF(UPPER((SELECT (SELECT inn_1195.id FROM tbl_08 AS inn_1195 LIMIT 1) FROM tbl_06 AS sub_1194 LIMIT 1)), ((((t_ref_1159.col_2 / NULLIF(-9223372036854775808, 0)) / NULLIF(CAST(9223372036854775807 AS DECIMAL(10,2)), 0)) BETWEEN 0 AND -1) >= t_ref_1159.col_2)) END) ELSE (EXISTS (SELECT 1 FROM tbl_08 AS sub_1196 WHERE '__fuzz_marker__') - -2147483648) END) AS CHAR) / NULLIF((-1.5 + t_ref_1159.col_1), 0)) AS SIGNED)), NULLIF((ABS((t_ref_1159.col_4 IN (SELECT sub_1198.id FROM tbl_06 AS sub_1198))) AND LOWER(EXISTS (SELECT 1 FROM tbl_09 AS sub_1199 WHERE 'escaped''quote'))), (3.14159265 <> LOWER((t_ref_1159.col_3 * (((EXISTS (SELECT 1 FROM tbl_12 AS sub_1200 WHERE sub_1200.id) * ((SELECT (SELECT NULL FROM tbl_14 AS inn_1202 LIMIT 1) FROM tbl_05 AS sub_1201 LIMIT 1) * CAST((FALSE * (SELECT (SELECT inn_1204.id FROM tbl_07 AS inn_1204 LIMIT 1) FROM tbl_13 AS sub_1203 LIMIT 1)) AS CHAR))) - UPPER(((UPPER(t_ref_1159.col_4) / NULLIF((t_ref_1159.col_3 BETWEEN -2147483648 AND 0), 0)) / NULLIF(t_ref_1159.col_3, 0)))) BETWEEN -1 AND -2147483648)))))) ELSE 'escaped''quote' END)) + (NOT (CAST(t_ref_1159.id AS SIGNED)))))
LIMIT 4;

SELECT
    ((EXISTS (SELECT 1 FROM tbl_03 AS sub_1206 WHERE TRUE) <> (COALESCE(t_ref_1205.col_1, (NOT (t_ref_1205.col_3))) > CAST(t_ref_1205.id AS CHAR))) > ((t_ref_1205.id / NULLIF(CAST(((t_ref_1205.id AND FALSE) + t_ref_1205.col_4) AS DECIMAL(10,2)), 0)) / NULLIF(UPPER(NULL), 0))) AS c_1207,
    CAST((SELECT (sub_1208.col_1 IN (SELECT sub_1209.col_1 FROM tbl_03 AS sub_1209)) FROM tbl_12 AS sub_1208 LIMIT 1) AS DECIMAL(10,2)) AS c_1210
FROM tbl_06 AS t_ref_1205 LEFT JOIN tbl_04 AS j_ref_1211 ON t_ref_1205.id = j_ref_1211.id
WHERE ((EXISTS (SELECT 1 FROM tbl_12 AS sub_1212 WHERE sub_1212.ref_tbl_09_id) / NULLIF(LOWER(CAST((SELECT ((CASE WHEN FALSE THEN ((TRUE - (CAST(sub_1232.col_2 AS DECIMAL(10,2)) > (SELECT (SELECT inn_1235.col_1 FROM tbl_08 AS inn_1235 LIMIT 1) FROM tbl_08 AS sub_1233 LIMIT 1))) * ((CASE WHEN LOWER(sub_1232.id) THEN (sub_1232.col_3 - (EXISTS (SELECT 1 FROM tbl_07 AS sub_1236 WHERE NULL) / NULLIF(sub_1232.col_1, 0))) ELSE ((SELECT (SELECT inn_1239.col_4 FROM tbl_10 AS inn_1239 LIMIT 1) FROM tbl_09 AS sub_1238 LIMIT 1) + (SELECT (NULLIF((sub_1240.col_3 + 3.14159265), ('chaos' * sub_1240.col_1)) IS NOT NULL) FROM tbl_12 AS sub_1240 LIMIT 1)) END) = (((SELECT (SELECT inn_1242.id FROM tbl_13 AS inn_1242 LIMIT 1) FROM tbl_06 AS sub_1241 LIMIT 1) >= ((TRUE OR LENGTH(((sub_1232.col_3 - 100) BETWEEN 100 AND -1))) <> UPPER(CAST(CAST(sub_1232.col_1 AS SIGNED) AS SIGNED)))) <> LENGTH(sub_1232.id)))) ELSE sub_1232.col_1 END) BETWEEN 2147483647 AND -1) FROM tbl_05 AS sub_1232 LIMIT 1) AS CHAR)), 0)) - LENGTH(1.0))
LIMIT 40;

INSERT INTO tbl_07 (id, col_1, col_3) VALUES (947681, 1e10, -1e-5);

INSERT INTO asset_meta (id, payload) VALUES (839926, '__fuzz_marker__');

UPDATE tbl_08 SET col_2 = 2147483647 WHERE id > 48;

BEGIN;
INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (149033, 0, 2147483647, '__fuzz_marker__');
INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (389868, 100, -1, 'escaped''quote');
INSERT INTO tbl_11 (id, col_1, col_2) VALUES (187000, '{}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

SELECT
    (t_ref_1243.col_3 IN (SELECT sub_1244.id FROM tbl_04 AS sub_1244)) AS c_1260
FROM tbl_14 AS t_ref_1243
WHERE t_ref_1243.id
LIMIT 3;

INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (852023, -2147483648, 100, '');

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSON,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

SELECT
    ((t_ref_1261.col_2 / NULLIF(CAST((SELECT (SELECT -1e-5 FROM asset_meta AS inn_1264 LIMIT 1) FROM tbl_10 AS sub_1262 LIMIT 1) AS SIGNED), 0)) + t_ref_1261.col_3) AS c_1265,
    ((SELECT sub_1266.id FROM asset_meta AS sub_1266 LIMIT 1) BETWEEN 9223372036854775807 AND -2147483648) AS c_1267
FROM tbl_03 AS t_ref_1261
WHERE ((CASE WHEN (UPPER(((t_ref_1261.id BETWEEN 42 AND 42) IS NULL)) < ((CAST((t_ref_1261.col_4 <= (CASE WHEN t_ref_1261.col_1 THEN (EXISTS (SELECT 1 FROM tbl_14 AS sub_1268 WHERE FALSE) - t_ref_1261.col_1) ELSE (SELECT ((sub_1270.col_1 / NULLIF(sub_1270.ref_tbl_01_id, 0)) / NULLIF((((UPPER(EXISTS (SELECT 1 FROM tbl_11 AS sub_1271 WHERE sub_1271.id)) - COALESCE(((sub_1270.col_1 IN (SELECT sub_1272.col_2 FROM tbl_04 AS sub_1272)) - (SELECT sub_1273.col_1 FROM tbl_07 AS sub_1273 LIMIT 1)), EXISTS (SELECT 1 FROM tbl_01 AS sub_1274 WHERE sub_1274.col_3))) IS NOT NULL) - (1e10 BETWEEN 100 AND 9223372036854775807)), 0)) FROM tbl_08 AS sub_1270 LIMIT 1) END)) AS DECIMAL(10,2)) = EXISTS (SELECT 1 FROM tbl_07 AS sub_1275 WHERE 'alpha_beta_gamma')) + t_ref_1261.id)) THEN (CAST(EXISTS (SELECT 1 FROM tbl_09 AS sub_1278 WHERE sub_1278.ref_tbl_03_id) AS SIGNED) BETWEEN 1 AND 2147483647) ELSE (CASE WHEN ((NOT (((CAST(t_ref_1261.col_4 AS SIGNED) = ((t_ref_1261.id IN (SELECT sub_1279.id FROM tbl_09 AS sub_1279)) * ABS(('' <> (SELECT (SELECT inn_1281.col_3 FROM tbl_07 AS inn_1281 LIMIT 1) FROM tbl_15 AS sub_1280 LIMIT 1))))) AND (((SELECT sub_1282.id FROM tbl_07 AS sub_1282 LIMIT 1) <> t_ref_1261.col_2) - (CAST(CAST((SELECT (((sub_1283.col_1 * COALESCE(('__fuzz_marker__' >= sub_1283.col_3), (TRUE / NULLIF(sub_1283.col_2, 0)))) * (EXISTS (SELECT 1 FROM tbl_07 AS sub_1284 WHERE sub_1284.col_3) < (CASE WHEN (sub_1283.col_1 / NULLIF(TRUE, 0)) THEN (SELECT sub_1285.ref_tbl_01_id FROM tbl_15 AS sub_1285 LIMIT 1) ELSE TRUE END))) * sub_1283.col_1) FROM tbl_05 AS sub_1283 LIMIT 1) AS DECIMAL(10,2)) AS CHAR) * ((t_ref_1261.col_1 - t_ref_1261.col_4) BETWEEN 100 AND -1)))))) / NULLIF(t_ref_1261.col_2, 0)) THEN ((EXISTS (SELECT 1 FROM tbl_07 AS sub_1286 WHERE sub_1286.col_3) <= CAST((NOT (CAST(LENGTH(ABS(((SELECT UPPER(LOWER(CAST(LENGTH(sub_1287.col_2) AS CHAR))) FROM tbl_14 AS sub_1287 LIMIT 1) - EXISTS (SELECT 1 FROM asset_meta AS sub_1288 WHERE sub_1288.id)))) AS SIGNED))) AS CHAR)) + CAST(('escaped''quote' IN (SELECT sub_1289.id FROM tbl_08 AS sub_1289)) AS DECIMAL(10,2))) ELSE (t_ref_1261.col_1 AND (((CAST((1e10 / NULLIF(t_ref_1261.col_2, 0)) AS CHAR) BETWEEN 9223372036854775807 AND 9223372036854775807) - -1) >= ((LENGTH(t_ref_1261.id) IS NULL) > ((NULLIF(((NULL / NULLIF(((CASE WHEN ((t_ref_1261.col_2 IS NULL) + t_ref_1261.id) THEN (SELECT (sub_1294.col_3 IN (SELECT sub_1295.col_1 FROM tbl_14 AS sub_1295)) FROM tbl_01 AS sub_1294 LIMIT 1) ELSE t_ref_1261.col_2 END) + ((CAST(((SELECT sub_1296.id FROM tbl_14 AS sub_1296 LIMIT 1) <= CAST(t_ref_1261.col_1 AS CHAR)) AS SIGNED) <= (((SELECT FALSE FROM tbl_04 AS sub_1297 LIMIT 1) - (-9223372036854775808 / NULLIF(t_ref_1261.col_4, 0))) - COALESCE(NULLIF(0.0, t_ref_1261.col_3), (t_ref_1261.col_3 IS NOT NULL)))) IS NOT NULL)), 0)) / NULLIF(LOWER(t_ref_1261.id), 0)), (SELECT sub_1298.col_1 FROM tbl_09 AS sub_1298 LIMIT 1)) - (t_ref_1261.col_2 IN (SELECT sub_1299.id FROM tbl_02 AS sub_1299))) IS NOT NULL)))) END) END) <> (CASE WHEN EXISTS (SELECT 1 FROM tbl_11 AS sub_1306 WHERE sub_1306.col_1) THEN ((9223372036854775807 + t_ref_1261.col_3) > LENGTH(ABS(NULLIF(((t_ref_1261.col_1 AND (SELECT (SELECT (SELECT inn_1318.id FROM tbl_07 AS inn_1318 LIMIT 1) FROM tbl_13 AS sub_1317 LIMIT 1) FROM tbl_11 AS sub_1316 LIMIT 1)) AND (SELECT (SELECT '__fuzz_marker__' FROM tbl_01 AS inn_1321 LIMIT 1) FROM tbl_05 AS sub_1319 LIMIT 1)), (SELECT sub_1322.col_2 FROM tbl_11 AS sub_1322 LIMIT 1))))) ELSE (CASE WHEN (CASE WHEN EXISTS (SELECT 1 FROM tbl_13 AS sub_1323 WHERE sub_1323.id) THEN (CASE WHEN t_ref_1261.col_2 THEN LENGTH(((COALESCE((NOT ((((TRUE IN (SELECT sub_1330.id FROM tbl_02 AS sub_1330)) * FALSE) - (t_ref_1261.col_4 * (CASE WHEN EXISTS (SELECT 1 FROM tbl_04 AS sub_1331 WHERE sub_1331.col_1) THEN (SELECT LENGTH(sub_1332.id) FROM tbl_02 AS sub_1332 LIMIT 1) ELSE (t_ref_1261.col_2 IN (SELECT sub_1333.id FROM tbl_07 AS sub_1333)) END))))), FALSE) > (NULL * ABS(((((t_ref_1261.col_4 * TRUE) < (t_ref_1261.col_1 / NULLIF(((FALSE OR LOWER(t_ref_1261.col_3)) / NULLIF(EXISTS (SELECT 1 FROM tbl_02 AS sub_1334 WHERE sub_1334.dyn_col_4), 0)), 0))) OR (CASE WHEN (CASE WHEN t_ref_1261.col_4 THEN ((t_ref_1261.col_1 = (t_ref_1261.col_3 - t_ref_1261.col_3)) + t_ref_1261.col_1) ELSE CAST(NULL AS CHAR) END) THEN ((((t_ref_1261.col_1 - -1.5) <= t_ref_1261.col_2) + (SELECT (SELECT inn_1336.id FROM tbl_01 AS inn_1336 LIMIT 1) FROM tbl_12 AS sub_1335 LIMIT 1)) / NULLIF(t_ref_1261.col_4, 0)) ELSE COALESCE(1e10, EXISTS (SELECT 1 FROM tbl_04 AS sub_1337 WHERE sub_1337.id)) END)) * (CASE WHEN ((t_ref_1261.id / NULLIF((FALSE IN (SELECT sub_1340.col_1 FROM tbl_03 AS sub_1340)), 0)) * t_ref_1261.col_2) THEN TRUE ELSE ABS(((NOT (CAST(t_ref_1261.col_4 AS CHAR))) * t_ref_1261.col_4)) END))))) - (SELECT (SELECT NULL FROM tbl_06 AS inn_1345 LIMIT 1) FROM tbl_13 AS sub_1341 LIMIT 1))) ELSE LENGTH(CAST(t_ref_1261.id AS CHAR)) END) ELSE (NULL / NULLIF(UPPER(NULLIF((('__fuzz_marker__' IN (SELECT sub_1346.ref_tbl_01_id FROM tbl_02 AS sub_1346)) AND (t_ref_1261.col_3 IN (SELECT sub_1350.id FROM tbl_09 AS sub_1350))), (EXISTS (SELECT 1 FROM tbl_07 AS sub_1351 WHERE sub_1351.id) OR 9223372036854775807))), 0)) END) THEN t_ref_1261.col_1 ELSE UPPER(((SELECT UPPER(sub_1352.id) FROM tbl_09 AS sub_1352 LIMIT 1) - CAST(NULL AS DECIMAL(10,2)))) END) END))
LIMIT 4;

ALTER TABLE tbl_14 ADD COLUMN dyn_col_6 VARCHAR(32);

ALTER TABLE tbl_12 ADD COLUMN dyn_col_6 VARCHAR(32);

BEGIN;
INSERT INTO tbl_12 (id, col_1, col_2, col_3, ref_tbl_09_id, dyn_col_6) VALUES (871336, '__fuzz_marker__', FALSE, FALSE, 42, 'ref_val');
COMMIT;

ALTER TABLE tbl_11 ADD COLUMN dyn_col_4 VARCHAR(32);

CREATE INDEX idx_tbl_11_col_2_5 ON tbl_11 (col_2);

DELETE FROM tbl_03 WHERE id = 26896;

UPDATE tbl_13 SET col_2 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 73;

DELETE FROM tbl_01 WHERE id = 93090;

DELETE FROM tbl_14 WHERE id = 68252;

INSERT INTO tbl_12 (id, col_1, col_2, col_3, ref_tbl_09_id, dyn_col_6) VALUES (23634, 'chaos', FALSE, TRUE, -1, 'ref_val');

BEGIN;
INSERT INTO asset_meta (id, payload) VALUES (34606, 'chaos');
COMMIT;

UPDATE tbl_06 SET col_3 = TRUE WHERE id > 93;

INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (487268, 0, 'alpha_beta_gamma', 42, '{}', 'chaos');

CREATE INDEX idx_tbl_07_col_1_6 ON tbl_07 (col_1);

CREATE TABLE asset_meta (
    meta_id INT PRIMARY KEY,
    category VARCHAR(32) NOT NULL,
    blob_chunk BLOB NOT NULL
);

SELECT
    category,
    LENGTH(GROUP_CONCAT(blob_chunk ORDER BY blob_chunk SEPARATOR '|')) AS total_len,
    COUNT(*) AS chunk_count
FROM asset_meta
GROUP BY category
HAVING total_len > 10000;

BEGIN;
INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (595297, 9223372036854775807, 'alpha_beta_gamma', 2147483647, '[]', '');
INSERT INTO tbl_14 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (253479, 999999999.99, TRUE, 'escaped''quote', 'ref_val', 'chaos');
COMMIT;

ALTER TABLE tbl_03 ADD COLUMN dyn_col_6 VARCHAR(32);

SELECT
    t_ref_1353.col_3 AS c_1354,
    ((t_ref_1353.id IN (SELECT sub_1355.col_2 FROM tbl_10 AS sub_1355)) / NULLIF((SELECT ((TRUE OR NULLIF(NULLIF(((SELECT (SELECT 9223372036854775807 FROM tbl_09 AS inn_1368 LIMIT 1) FROM asset_meta AS sub_1367 LIMIT 1) - LENGTH((sub_1366.id / NULLIF(EXISTS (SELECT 1 FROM tbl_08 AS sub_1369 WHERE NULL), 0)))), ((CASE WHEN (FALSE IS NULL) THEN ABS(ABS(CAST((CAST(sub_1366.ref_tbl_08_id AS SIGNED) AND sub_1366.ref_tbl_08_id) AS CHAR))) ELSE sub_1366.col_2 END) > ((((SELECT EXISTS (SELECT 1 FROM tbl_12 AS sub_1371 WHERE TRUE) FROM asset_meta AS sub_1370 LIMIT 1) >= ((sub_1366.col_1 IN (SELECT sub_1372.id FROM tbl_08 AS sub_1372)) BETWEEN 42 AND 9223372036854775807)) BETWEEN 100 AND 1) AND (NULLIF((SELECT (SELECT NULL FROM tbl_14 AS inn_1378 LIMIT 1) FROM tbl_03 AS sub_1373 LIMIT 1), CAST(CAST(COALESCE((sub_1366.col_2 IN (SELECT sub_1379.col_1 FROM tbl_07 AS sub_1379)), (ABS(ABS(sub_1366.col_2)) + sub_1366.ref_tbl_08_id)) AS DECIMAL(10,2)) AS DECIMAL(10,2))) + -1e-5)))), ABS(sub_1366.ref_tbl_08_id))) <> 'escaped''quote') FROM tbl_13 AS sub_1366 LIMIT 1), 0)) AS c_1380
FROM tbl_05 AS t_ref_1353
WHERE NULLIF((CASE WHEN (SELECT (SELECT 'escaped''quote' FROM tbl_05 AS inn_1383 LIMIT 1) FROM tbl_05 AS sub_1381 LIMIT 1) THEN (SELECT (SELECT -1 FROM asset_meta AS inn_1394 LIMIT 1) FROM tbl_12 AS sub_1384 LIMIT 1) ELSE (SELECT (SELECT inn_1403.col_1 FROM tbl_13 AS inn_1403 LIMIT 1) FROM tbl_08 AS sub_1395 LIMIT 1) END), ((CAST((t_ref_1353.col_2 <= NULLIF((CAST((((EXISTS (SELECT 1 FROM tbl_04 AS sub_1404 WHERE sub_1404.col_4) AND LENGTH((((ABS(CAST(t_ref_1353.col_3 AS CHAR)) / NULLIF(EXISTS (SELECT 1 FROM asset_meta AS sub_1405 WHERE TRUE), 0)) IS NOT NULL) / NULLIF((SELECT (SELECT inn_1408.col_1 FROM tbl_06 AS inn_1408 LIMIT 1) FROM tbl_05 AS sub_1406 LIMIT 1), 0)))) <> (COALESCE((SELECT '__fuzz_marker__' FROM tbl_13 AS sub_1409 LIMIT 1), (EXISTS (SELECT 1 FROM tbl_07 AS sub_1410 WHERE TRUE) - (SELECT sub_1411.col_3 FROM tbl_10 AS sub_1411 LIMIT 1))) >= (ABS((((t_ref_1353.col_2 AND (NOT (t_ref_1353.id))) = t_ref_1353.id) + EXISTS (SELECT 1 FROM tbl_04 AS sub_1412 WHERE sub_1412.id))) IS NULL))) + TRUE) AS DECIMAL(10,2)) <> LENGTH(((SELECT (SELECT inn_1415.col_2 FROM tbl_12 AS inn_1415 LIMIT 1) FROM tbl_14 AS sub_1414 LIMIT 1) <= EXISTS (SELECT 1 FROM tbl_04 AS sub_1416 WHERE sub_1416.col_1)))), CAST(t_ref_1353.col_3 AS SIGNED))) AS SIGNED) * (t_ref_1353.col_2 IN (SELECT sub_1418.col_1 FROM tbl_09 AS sub_1418))) * NULL))
LIMIT 35;

CREATE INDEX idx_tbl_06_col_4_7 ON tbl_06 (col_4);

INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (300493, FALSE, 'alpha_beta_gamma', TRUE, 42);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (771829, '', -1e-5, 2147483647, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'alpha_beta_gamma');

ALTER TABLE tbl_13 DROP COLUMN ref_tbl_08_id;

UPDATE tbl_03 SET col_4 = '[]' WHERE id > 32;

BEGIN;
INSERT INTO tbl_12 (id, col_1, col_2, col_3, ref_tbl_09_id, dyn_col_6) VALUES (190151, 'ref_val', FALSE, TRUE, 2147483647, '');
INSERT INTO tbl_10 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (579630, 100, 'ref_val', 0, '[]', 'chaos');
INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (856505, -1, 100, '__fuzz_marker__');
COMMIT;

DELETE FROM tbl_07 WHERE id = 53337;

SELECT
    t_ref_1424.col_1 AS c_1425,
    t_ref_1424.col_3 AS c_1426
FROM tbl_04 AS t_ref_1424
WHERE (t_ref_1424.col_4 IN (SELECT sub_1427.col_1 FROM tbl_09 AS sub_1427))
LIMIT 41;

INSERT INTO tbl_08 (id, col_1, col_2, ref_tbl_01_id) VALUES (437214, 'alpha_beta_gamma', 1, -2147483648);

ALTER TABLE tbl_12 ADD COLUMN dyn_col_7 VARCHAR(32);

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2)
);

UPDATE tbl_03 SET dyn_col_6 = 'alpha_beta_gamma' WHERE id > 56;

ALTER TABLE tbl_03 ADD COLUMN dyn_col_7 VARCHAR(32);

SELECT
    (t_ref_1428.col_1 IN (SELECT sub_1429.col_4 FROM tbl_10 AS sub_1429)) AS c_1430
FROM tbl_04 AS t_ref_1428
WHERE COALESCE((NULL IN (SELECT sub_1431.col_3 FROM tbl_10 AS sub_1431)), CAST(FALSE AS CHAR))
LIMIT 6;

CREATE TABLE tbl_17 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2),
    col_4 INT
);

CREATE INDEX idx_tbl_09_id_8 ON tbl_09 (id);

CREATE TABLE tbl_18 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 JSON,
    col_3 INT,
    col_4 INT,
    ref_tbl_14_id INT,
    FOREIGN KEY (ref_tbl_14_id) REFERENCES tbl_14(id)
);

INSERT INTO asset_meta (id, payload) VALUES (347451, 'ref_val');

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_3) VALUES (950511, 1e10, 0.0000000001);
INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (451611, -1e-5, '{}', 1);
COMMIT;

SELECT
    (NOT (UPPER((SELECT (SELECT inn_1453.col_4 FROM tbl_03 AS inn_1453 LIMIT 1) FROM tbl_14 AS sub_1445 LIMIT 1)))) AS c_1454,
    t_ref_1444.dyn_col_7 AS c_1455,
    LENGTH((t_ref_1444.ref_tbl_09_id * EXISTS (SELECT 1 FROM tbl_16 AS sub_1456 WHERE sub_1456.id))) AS c_1475
FROM tbl_12 AS t_ref_1444 LEFT JOIN tbl_17 AS j_ref_1476 ON t_ref_1444.id = j_ref_1476.id
WHERE EXISTS (SELECT 1 FROM tbl_03 AS sub_1477 WHERE sub_1477.col_2)
LIMIT 29;

CREATE TABLE tbl_19 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 INT,
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

DELETE FROM tbl_14 WHERE id = 15554;

ALTER TABLE tbl_10 DROP COLUMN col_3;

SELECT
    FALSE AS c_1479
FROM tbl_05 AS t_ref_1478
WHERE TRUE
LIMIT 17;

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (291425, -1e-5, '{}', 100);

BEGIN;
INSERT INTO tbl_12 (id, col_1, col_2, col_3, ref_tbl_09_id, dyn_col_6, dyn_col_7) VALUES (24306, 'alpha_beta_gamma', TRUE, FALSE, -2147483648, '__fuzz_marker__', '__fuzz_marker__');
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4, dyn_col_6, dyn_col_7) VALUES (928532, 'escaped''quote', -1.5, 42, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'escaped''quote', 'chaos');
COMMIT;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_3) VALUES (157917, -1.5, -1.5);
COMMIT;

SELECT
    (ABS(EXISTS (SELECT 1 FROM asset_meta AS sub_1481 WHERE sub_1481.payload)) / NULLIF(CAST((t_ref_1480.id IS NULL) AS SIGNED), 0)) AS c_1494
FROM tbl_18 AS t_ref_1480 LEFT JOIN tbl_02 AS j_ref_1495 ON t_ref_1480.id = j_ref_1495.id
WHERE (CAST((CASE WHEN (SELECT (SELECT NULL FROM tbl_14 AS inn_1504 LIMIT 1) FROM tbl_08 AS sub_1496 LIMIT 1) THEN ((t_ref_1480.col_3 IS NOT NULL) + (SELECT (sub_1505.id <> sub_1505.col_2) FROM tbl_13 AS sub_1505 LIMIT 1)) ELSE (SELECT sub_1506.id FROM tbl_03 AS sub_1506 LIMIT 1) END) AS CHAR) BETWEEN 1 AND -9223372036854775808)
LIMIT 37;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_3) VALUES (401651, 1.0, -1.5);
INSERT INTO tbl_10 (id, col_1, col_2, col_4, dyn_col_6) VALUES (202399, 1, '__fuzz_marker__', '{}', 'ref_val');
INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (958989, 100, 2147483647, '');
COMMIT;

UPDATE tbl_01 SET dyn_col_3 = 'alpha_beta_gamma' WHERE id > 66;

UPDATE tbl_02 SET dyn_col_4 = 'chaos' WHERE id > 54;

UPDATE tbl_15 SET col_1 = -9223372036854775808 WHERE id > 36;

ALTER TABLE tbl_07 ADD COLUMN dyn_col_4 VARCHAR(32);

CREATE VIEW v_summary_11 AS SELECT id FROM tbl_19 WHERE id IS NOT NULL;

CREATE VIEW v_summary_12 AS SELECT id FROM tbl_10 WHERE id IS NOT NULL;

ALTER TABLE tbl_17 DROP COLUMN col_2;

DELETE FROM tbl_13 WHERE id = 75435;

CREATE VIEW v_summary_13 AS SELECT id FROM tbl_15 WHERE id IS NOT NULL;

CREATE VIEW v_summary_14 AS SELECT id FROM tbl_12 WHERE id IS NOT NULL;

CREATE VIEW v_summary_15 AS SELECT id FROM tbl_06 WHERE id IS NOT NULL;

CREATE VIEW v_summary_16 AS SELECT id FROM tbl_14 WHERE id IS NOT NULL;

CREATE TABLE tbl_20 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT
);

CREATE TABLE tbl_21 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 INT,
    col_4 NUMERIC(10,2),
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

INSERT INTO tbl_02 (id, col_1, ref_tbl_01_id, dyn_col_4) VALUES (103587, 2147483647, 100, 'alpha_beta_gamma');

SELECT
    (t_ref_1507.col_1 IN (SELECT sub_1508.col_2 FROM tbl_21 AS sub_1508)) AS c_1509,
    (SELECT EXISTS (SELECT 1 FROM tbl_11 AS sub_1511 WHERE TRUE) FROM tbl_02 AS sub_1510 LIMIT 1) AS c_1512
FROM tbl_18 AS t_ref_1507
WHERE t_ref_1507.col_2
LIMIT 48;

SELECT
    CAST(t_ref_1513.ref_tbl_01_id AS DECIMAL(10,2)) AS c_1514
FROM tbl_15 AS t_ref_1513
WHERE t_ref_1513.col_1
LIMIT 36;

CREATE TABLE tbl_22 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 INT,
    col_3 BOOLEAN,
    ref_tbl_09_id INT,
    FOREIGN KEY (ref_tbl_09_id) REFERENCES tbl_09(id)
);

CREATE TABLE tbl_23 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64)
);

UPDATE tbl_20 SET col_1 = TRUE WHERE id > 55;

ALTER TABLE tbl_01 ADD COLUMN dyn_col_4 VARCHAR(32);

CREATE TABLE tbl_24 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 NUMERIC(10,2)
);
-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: MYSQL

-- Statements: 200

-- AST Expression Depth: 20

-- Injected Defect Case: mysql_12

-- Seed: 20022



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON,
    col_3 INT,
    col_4 NUMERIC(10,2)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSON,
    col_3 BOOLEAN,
    col_4 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON,
    col_3 BOOLEAN,
    col_4 JSON
);

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (853677, '[]', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 100, 1e10);
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (671358, 'alpha_beta_gamma', '[]', TRUE, 'ref_val', -1);
COMMIT;

CREATE TABLE customers_large (
    cust_id INT PRIMARY KEY,
    company_name VARCHAR(255) NOT NULL,
    profile_data JSON
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 VARCHAR(64),
    col_4 INT
);

INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (277604, 'chaos', '', 'escaped''quote', -2147483648);

DELETE FROM tbl_05 WHERE id = 16165;

INSERT INTO tbl_03 (id, col_1, col_2) VALUES (329858, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (275985, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -1, 1e10);
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (14115, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '[]', TRUE, '{}');
COMMIT;

ALTER TABLE tbl_04 DROP COLUMN col_3;

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (375045, 'alpha_beta_gamma', '{}', TRUE, '', 9223372036854775807);
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (53481, '', '{}', FALSE, 'escaped''quote', 9223372036854775807);
COMMIT;

ALTER TABLE tbl_03 ADD COLUMN dyn_col_4 VARCHAR(32);

SELECT
    (CAST((((CASE WHEN (((NULLIF(CAST(EXISTS (SELECT 1 FROM customers_large AS sub_2 WHERE -2147483648) AS SIGNED), (t_ref_1.col_4 BETWEEN -1 AND 0)) / NULLIF((SELECT (CASE WHEN sub_8.col_4 THEN (SELECT (SELECT inn_11.id FROM tbl_05 AS inn_11 LIMIT 1) FROM tbl_01 AS sub_9 LIMIT 1) ELSE LOWER(sub_8.col_4) END) FROM tbl_05 AS sub_8 LIMIT 1), 0)) / NULLIF(((((CASE WHEN (t_ref_1.col_4 = FALSE) THEN (SELECT sub_12.id FROM customers_large AS sub_12 LIMIT 1) ELSE t_ref_1.col_2 END) < t_ref_1.col_4) - (SELECT (SELECT inn_19.col_3 FROM tbl_01 AS inn_19 LIMIT 1) FROM tbl_05 AS sub_13 LIMIT 1)) / NULLIF((SELECT (SELECT FALSE FROM tbl_04 AS inn_23 LIMIT 1) FROM tbl_04 AS sub_20 LIMIT 1), 0)), 0)) = LOWER(NULL)) THEN ABS(NULL) ELSE CAST((t_ref_1.col_3 - 0.0000000001) AS SIGNED) END) IS NOT NULL) * t_ref_1.col_3) AS CHAR) BETWEEN -1 AND 2147483647) AS c_24
FROM tbl_05 AS t_ref_1 LEFT JOIN tbl_02 AS j_ref_25 ON t_ref_1.id = j_ref_25.id
WHERE CAST((CAST(COALESCE((t_ref_1.col_4 * (CASE WHEN (SELECT (SELECT FALSE FROM tbl_05 AS inn_27 LIMIT 1) FROM tbl_05 AS sub_26 LIMIT 1) THEN ((t_ref_1.col_1 IN (SELECT sub_28.col_1 FROM tbl_02 AS sub_28)) - (LOWER(((ABS(LOWER(('__fuzz_marker__' / NULLIF((t_ref_1.col_2 / NULLIF((NULLIF(((t_ref_1.col_2 + t_ref_1.col_1) - EXISTS (SELECT 1 FROM tbl_01 AS sub_30 WHERE sub_30.col_1)), FALSE) * ABS((t_ref_1.col_3 + (NULL <= t_ref_1.col_3)))), 0)), 0)))) * EXISTS (SELECT 1 FROM customers_large AS sub_31 WHERE sub_31.id)) / NULLIF((SELECT (SELECT inn_33.col_3 FROM tbl_05 AS inn_33 LIMIT 1) FROM tbl_01 AS sub_32 LIMIT 1), 0))) <> LENGTH(NULL))) ELSE ((SELECT (SELECT 1 FROM customers_large AS inn_38 LIMIT 1) FROM tbl_05 AS sub_34 LIMIT 1) IS NULL) END)), ((SELECT ((CASE WHEN ABS((COALESCE(EXISTS (SELECT 1 FROM tbl_03 AS sub_40 WHERE sub_40.col_1), EXISTS (SELECT 1 FROM customers_large AS sub_41 WHERE sub_41.id)) - COALESCE(((((CAST(((999999999.99 - 'ref_val') >= (sub_39.col_1 >= sub_39.col_1)) AS DECIMAL(10,2)) OR sub_39.col_4) <> sub_39.col_2) + (sub_39.id IN (SELECT sub_45.col_1 FROM tbl_02 AS sub_45))) * ('alpha_beta_gamma' IN (SELECT sub_47.id FROM tbl_04 AS sub_47))), (ABS(sub_39.col_2) > sub_39.col_1)))) THEN EXISTS (SELECT 1 FROM tbl_05 AS sub_48 WHERE 'escaped''quote') ELSE -9223372036854775808 END) BETWEEN 9223372036854775807 AND 1) FROM tbl_04 AS sub_39 LIMIT 1) <> ((t_ref_1.col_3 IN (SELECT sub_49.id FROM tbl_03 AS sub_49)) BETWEEN 0 AND 1))) AS CHAR) BETWEEN -1 AND 42) AS SIGNED)
LIMIT 45;

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (348532, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 2147483647, 0.0);
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (906421, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '[]', 9223372036854775807, -1e-5);
COMMIT;

ALTER TABLE tbl_03 ADD COLUMN dyn_col_5 VARCHAR(32);

UPDATE tbl_04 SET col_2 = '{}' WHERE id > 86;

INSERT INTO tbl_03 (id, col_1, col_2, dyn_col_4, dyn_col_5) VALUES (589699, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'ref_val', 'ref_val');

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 INT,
    col_3 BOOLEAN,
    col_4 INT,
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_06 WHERE id IS NOT NULL;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 BOOLEAN,
    ref_tbl_05_id INT,
    FOREIGN KEY (ref_tbl_05_id) REFERENCES tbl_05(id)
);

INSERT INTO tbl_04 (id, col_1, col_2, col_4) VALUES (198527, '[]', '[]', '[]');

UPDATE tbl_02 SET col_2 = '[]' WHERE id > 67;

SELECT
    (t_ref_50.ref_tbl_02_id + t_ref_50.col_2) AS c_51
FROM tbl_06 AS t_ref_50 LEFT JOIN tbl_05 AS j_ref_52 ON t_ref_50.id = j_ref_52.id
WHERE t_ref_50.col_4
LIMIT 16;

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_05 WHERE id IS NOT NULL;

SELECT
    EXISTS (SELECT 1 FROM tbl_04 AS sub_54 WHERE sub_54.id) AS c_55,
    ((CAST(t_ref_53.col_1 AS CHAR) > TRUE) > (SELECT (SELECT inn_75.id FROM tbl_04 AS inn_75 LIMIT 1) FROM tbl_06 AS sub_56 LIMIT 1)) AS c_76,
    (t_ref_53.col_2 IN (SELECT sub_77.col_3 FROM tbl_06 AS sub_77)) AS c_81
FROM tbl_04 AS t_ref_53
WHERE NULLIF((ABS((LOWER((CASE WHEN (NOT (CAST((SELECT sub_82.ref_tbl_02_id FROM tbl_06 AS sub_82 LIMIT 1) AS SIGNED))) THEN EXISTS (SELECT 1 FROM customers_large AS sub_83 WHERE sub_83.payload) ELSE EXISTS (SELECT 1 FROM tbl_06 AS sub_85 WHERE sub_85.col_2) END)) * ((NOT (CAST(CAST(((t_ref_53.col_2 IN (SELECT sub_87.id FROM tbl_04 AS sub_87)) IS NOT NULL) AS SIGNED) AS SIGNED))) + t_ref_53.col_2))) / NULLIF(EXISTS (SELECT 1 FROM tbl_07 AS sub_91 WHERE 3.14159265), 0)), (999999999.99 >= LOWER((-1 IN (SELECT sub_105.col_4 FROM tbl_05 AS sub_105)))))
LIMIT 13;

DELETE FROM tbl_06 WHERE id = 8298;

UPDATE customers_large SET payload = 'ref_val' WHERE id > 91;

DELETE FROM tbl_05 WHERE id = 81129;

CREATE INDEX idx_tbl_01_id_1 ON tbl_01 (id);

CREATE TABLE orders_large (
    ord_id INT PRIMARY KEY,
    cust_id INT NOT NULL,
    notes TEXT,
    order_val DECIMAL(12,2)
);

DELETE FROM tbl_06 WHERE id = 31977;

SELECT
    t_ref_148.payload AS c_149,
    ((CAST(t_ref_148.id AS DECIMAL(10,2)) + (SELECT (SELECT FALSE FROM tbl_06 AS inn_155 LIMIT 1) FROM tbl_01 AS sub_150 LIMIT 1)) IS NOT NULL) AS c_156,
    t_ref_148.id AS c_157
FROM customers_large AS t_ref_148
WHERE (SELECT (SELECT inn_162.col_1 FROM tbl_06 AS inn_162 LIMIT 1) FROM tbl_06 AS sub_158 LIMIT 1)
LIMIT 27;

DELETE FROM tbl_01 WHERE id = 47313;

CREATE INDEX idx_tbl_03_dyn_col_5_2 ON tbl_03 (dyn_col_5);

SELECT
    ((CAST(CAST(ABS((SELECT (CAST(((SELECT (sub_165.col_4 BETWEEN 1 AND -1) FROM tbl_02 AS sub_165 LIMIT 1) >= sub_164.payload) AS DECIMAL(10,2)) OR sub_164.id) FROM customers_large AS sub_164 LIMIT 1)) AS CHAR) AS DECIMAL(10,2)) IS NOT NULL) = ((('chaos' IN (SELECT sub_166.payload FROM customers_large AS sub_166)) / NULLIF(COALESCE(((SELECT sub_167.col_2 FROM tbl_05 AS sub_167 LIMIT 1) + LOWER(t_ref_163.dyn_col_5)), (t_ref_163.id IN (SELECT sub_168.id FROM tbl_07 AS sub_168))), 0)) / NULLIF(EXISTS (SELECT 1 FROM tbl_06 AS sub_198 WHERE 'escaped''quote'), 0))) AS c_199
FROM tbl_03 AS t_ref_163 LEFT JOIN tbl_04 AS j_ref_200 ON t_ref_163.id = j_ref_200.id
WHERE LENGTH(t_ref_163.col_2)
LIMIT 22;

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (346329, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 9223372036854775807, 0.0);

DELETE FROM customers_large WHERE id = 68801;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_4) VALUES (487996, '{}', '{}', '[]');
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (979702, '[]', 0, TRUE, 9223372036854775807, 0);
COMMIT;

DELETE FROM tbl_01 WHERE id = 43326;

CREATE INDEX idx_customers_large_payload_3 ON customers_large (payload);

CREATE INDEX idx_tbl_07_ref_tbl_05_id_4 ON tbl_07 (ref_tbl_05_id);

UPDATE tbl_06 SET col_4 = 1 WHERE id > 81;

INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_05_id) VALUES (912471, '{}', FALSE, -9223372036854775808);

SELECT
    (NULL >= COALESCE(COALESCE((EXISTS (SELECT 1 FROM tbl_05 AS sub_202 WHERE sub_202.col_1) < t_ref_201.id), LOWER(UPPER((NOT (CAST((FALSE IN (SELECT sub_203.col_2 FROM tbl_05 AS sub_203)) AS DECIMAL(10,2))))))), t_ref_201.id)) AS c_213,
    (NULLIF((t_ref_201.col_1 > NULLIF((LOWER(t_ref_201.col_2) < (((CASE WHEN NULL THEN ((((SELECT (SELECT inn_217.id FROM customers_large AS inn_217 LIMIT 1) FROM tbl_04 AS sub_214 LIMIT 1) > EXISTS (SELECT 1 FROM tbl_04 AS sub_218 WHERE sub_218.col_2)) + ((LOWER(EXISTS (SELECT 1 FROM tbl_05 AS sub_219 WHERE sub_219.col_4)) - t_ref_201.col_3) * (((SELECT (SELECT inn_223.id FROM customers_large AS inn_223 LIMIT 1) FROM tbl_06 AS sub_221 LIMIT 1) <> 0) + (SELECT (((SELECT (sub_225.col_1 IN (SELECT sub_226.id FROM tbl_02 AS sub_226)) FROM tbl_03 AS sub_225 LIMIT 1) IS NULL) < (sub_224.id - sub_224.id)) FROM customers_large AS sub_224 LIMIT 1)))) BETWEEN 1 AND 1) ELSE (((t_ref_201.id - (t_ref_201.ref_tbl_01_id / NULLIF(FALSE, 0))) / NULLIF((ABS(((UPPER(EXISTS (SELECT 1 FROM tbl_05 AS sub_227 WHERE FALSE)) / NULLIF((ABS(CAST(1.0 AS CHAR)) AND ((SELECT (SELECT 'ref_val' FROM tbl_05 AS inn_229 LIMIT 1) FROM tbl_06 AS sub_228 LIMIT 1) + t_ref_201.col_4)), 0)) < UPPER((TRUE IN (SELECT sub_230.col_4 FROM tbl_04 AS sub_230))))) / NULLIF((SELECT (SELECT inn_232.col_1 FROM tbl_05 AS inn_232 LIMIT 1) FROM tbl_03 AS sub_231 LIMIT 1), 0)), 0)) - (((SELECT ABS(EXISTS (SELECT 1 FROM tbl_06 AS sub_234 WHERE sub_234.ref_tbl_02_id)) FROM tbl_05 AS sub_233 LIMIT 1) BETWEEN 100 AND 42) < CAST((NULLIF(NULLIF(t_ref_201.col_3, ((SELECT sub_235.col_1 FROM tbl_04 AS sub_235 LIMIT 1) AND (t_ref_201.col_4 BETWEEN 100 AND 0))), t_ref_201.col_2) <> (CASE WHEN (SELECT (SELECT inn_237.dyn_col_4 FROM tbl_03 AS inn_237 LIMIT 1) FROM tbl_03 AS sub_236 LIMIT 1) THEN ((CAST(ABS(EXISTS (SELECT 1 FROM tbl_05 AS sub_238 WHERE FALSE)) AS DECIMAL(10,2)) > ((t_ref_201.id <= CAST(CAST(TRUE AS DECIMAL(10,2)) AS CHAR)) - (t_ref_201.col_4 + t_ref_201.ref_tbl_01_id))) <= ((NULL > ((CASE WHEN t_ref_201.col_4 THEN (t_ref_201.col_1 * t_ref_201.col_4) ELSE CAST(t_ref_201.col_3 AS SIGNED) END) + COALESCE(FALSE, COALESCE(t_ref_201.col_1, t_ref_201.col_2)))) - '')) ELSE ((CASE WHEN NULL THEN (NOT (COALESCE(COALESCE((CASE WHEN 1e10 THEN -1 ELSE t_ref_201.col_1 END), (SELECT (SELECT inn_240.id FROM tbl_01 AS inn_240 LIMIT 1) FROM tbl_01 AS sub_239 LIMIT 1)), t_ref_201.col_3))) ELSE t_ref_201.ref_tbl_01_id END) * ((t_ref_201.id - CAST((COALESCE(1e10, FALSE) = (t_ref_201.col_4 BETWEEN 9223372036854775807 AND -2147483648)) AS CHAR)) >= t_ref_201.col_1)) END)) AS CHAR))) END) BETWEEN -2147483648 AND 0) AND (t_ref_201.col_4 IN (SELECT sub_241.col_1 FROM tbl_03 AS sub_241)))), NULLIF(((NOT ((((t_ref_201.col_1 IN (SELECT sub_246.col_3 FROM tbl_05 AS sub_246)) + (CAST((CAST(EXISTS (SELECT 1 FROM tbl_01 AS sub_253 WHERE sub_253.id) AS SIGNED) - (-1e-5 IN (SELECT sub_254.payload FROM customers_large AS sub_254))) AS SIGNED) + (CASE WHEN ABS(EXISTS (SELECT 1 FROM tbl_05 AS sub_257 WHERE -9223372036854775808)) THEN CAST(t_ref_201.col_1 AS CHAR) ELSE CAST((NOT (t_ref_201.col_1)) AS DECIMAL(10,2)) END))) - EXISTS (SELECT 1 FROM customers_large AS sub_262 WHERE sub_262.id)))) * t_ref_201.col_3), (SELECT (sub_264.col_1 IN (SELECT sub_265.id FROM tbl_03 AS sub_265)) FROM tbl_05 AS sub_264 LIMIT 1)))), NULL) OR ((ABS(NULL) / NULLIF((t_ref_201.ref_tbl_01_id IN (SELECT sub_266.col_3 FROM tbl_06 AS sub_266)), 0)) IS NULL)) AS c_267
FROM tbl_02 AS t_ref_201
WHERE 100
LIMIT 7;

UPDATE tbl_07 SET ref_tbl_05_id = 0 WHERE id > 85;

SELECT
    (SELECT ((COALESCE(sub_269.id, 1) / NULLIF((COALESCE(UPPER((NOT ((SELECT (EXISTS (SELECT 1 FROM tbl_03 AS sub_271 WHERE sub_271.col_2) > COALESCE(sub_270.col_4, sub_270.col_4)) FROM tbl_01 AS sub_270 LIMIT 1)))), ABS(NULLIF((CASE WHEN COALESCE(((SELECT (SELECT sub_273.dyn_col_4 FROM tbl_03 AS sub_273 LIMIT 1) FROM tbl_07 AS sub_272 LIMIT 1) / NULLIF(CAST(((CAST(((CASE WHEN (sub_269.ref_tbl_05_id + CAST(3.14159265 AS DECIMAL(10,2))) THEN 'ref_val' ELSE ((TRUE AND sub_269.id) - (NULL + sub_269.col_2)) END) OR CAST((CAST(1e10 AS CHAR) + ABS(sub_269.ref_tbl_05_id)) AS DECIMAL(10,2))) AS SIGNED) <= ((sub_269.col_1 - (LOWER(EXISTS (SELECT 1 FROM tbl_03 AS sub_274 WHERE NULL)) - sub_269.col_1)) / NULLIF(UPPER((CASE WHEN 1.0 THEN (CAST(sub_269.ref_tbl_05_id AS CHAR) <> (sub_269.id < sub_269.id)) ELSE NULLIF((sub_269.id - sub_269.col_1), sub_269.col_2) END)), 0))) BETWEEN 100 AND 100) AS DECIMAL(10,2)), 0)), NULLIF((SELECT (SELECT inn_276.col_2 FROM tbl_07 AS inn_276 LIMIT 1) FROM tbl_05 AS sub_275 LIMIT 1), LENGTH(UPPER(ABS(sub_269.ref_tbl_05_id))))) THEN LOWER('chaos') ELSE ((sub_269.col_2 - LENGTH((SELECT (sub_277.col_4 IN (SELECT sub_278.col_2 FROM tbl_06 AS sub_278)) FROM tbl_01 AS sub_277 LIMIT 1))) < CAST(EXISTS (SELECT 1 FROM tbl_03 AS sub_279 WHERE sub_279.dyn_col_4) AS DECIMAL(10,2))) END), (CASE WHEN (sub_269.col_2 IN (SELECT sub_282.col_3 FROM tbl_05 AS sub_282)) THEN (((ABS((SELECT (SELECT TRUE FROM tbl_05 AS inn_284 LIMIT 1) FROM tbl_01 AS sub_283 LIMIT 1)) / NULLIF((CAST(sub_269.id AS SIGNED) >= (ABS(sub_269.ref_tbl_05_id) / NULLIF((ABS(COALESCE(((sub_269.id < NULL) BETWEEN -1 AND 1), ((CASE WHEN -9223372036854775808 THEN sub_269.col_1 ELSE FALSE END) * LENGTH(-1e-5)))) <= UPPER(UPPER(CAST(UPPER(sub_269.col_2) AS DECIMAL(10,2))))), 0))), 0)) * LOWER(100)) <> (sub_269.col_1 IN (SELECT sub_285.id FROM tbl_07 AS sub_285))) ELSE CAST(LOWER(UPPER((TRUE + -1.5))) AS SIGNED) END)))) * (CASE WHEN (SELECT sub_291.id FROM tbl_05 AS sub_291 LIMIT 1) THEN (NOT ((EXISTS (SELECT 1 FROM customers_large AS sub_292 WHERE sub_292.payload) * (LOWER(CAST(CAST(((ABS(CAST((((FALSE IN (SELECT sub_293.col_2 FROM tbl_01 AS sub_293)) >= (sub_269.col_2 <> 'ref_val')) - sub_269.ref_tbl_05_id) AS DECIMAL(10,2))) + (EXISTS (SELECT 1 FROM tbl_03 AS sub_294 WHERE sub_294.id) / NULLIF(((TRUE IN (SELECT sub_297.dyn_col_5 FROM tbl_03 AS sub_297)) BETWEEN 9223372036854775807 AND 42), 0))) / NULLIF((sub_269.id IN (SELECT sub_298.col_4 FROM tbl_01 AS sub_298)), 0)) AS SIGNED) AS CHAR)) OR ((((999999999.99 / NULLIF(((sub_269.id - (sub_269.col_2 < ((SELECT (SELECT -1 FROM tbl_02 AS inn_301 LIMIT 1) FROM tbl_01 AS sub_300 LIMIT 1) OR ((sub_269.ref_tbl_05_id - sub_269.ref_tbl_05_id) <> (sub_269.ref_tbl_05_id = FALSE))))) * sub_269.id), 0)) < ((CAST((SELECT (SELECT inn_304.col_3 FROM tbl_05 AS inn_304 LIMIT 1) FROM tbl_05 AS sub_302 LIMIT 1) AS DECIMAL(10,2)) > (SELECT (SELECT inn_307.dyn_col_4 FROM tbl_03 AS inn_307 LIMIT 1) FROM tbl_04 AS sub_305 LIMIT 1)) IS NOT NULL)) / NULLIF(((LENGTH(sub_269.col_2) OR (EXISTS (SELECT 1 FROM tbl_02 AS sub_308 WHERE sub_308.id) + 0.0)) < EXISTS (SELECT 1 FROM customers_large AS sub_309 WHERE -1)), 0)) >= (SELECT (SELECT inn_321.col_1 FROM tbl_02 AS inn_321 LIMIT 1) FROM tbl_07 AS sub_314 LIMIT 1)))))) ELSE 'alpha_beta_gamma' END)), 0)) + sub_269.id) FROM tbl_07 AS sub_269 LIMIT 1) AS c_322,
    (9223372036854775807 - (LOWER(COALESCE(UPPER(t_ref_268.col_2), EXISTS (SELECT 1 FROM tbl_05 AS sub_323 WHERE sub_323.col_4))) < COALESCE((SELECT (SELECT FALSE FROM tbl_02 AS inn_327 LIMIT 1) FROM tbl_06 AS sub_326 LIMIT 1), (-1.5 + UPPER(ABS((UPPER((NOT (UPPER(t_ref_268.col_1)))) + t_ref_268.col_1))))))) AS c_328,
    (t_ref_268.col_3 + UPPER((NULLIF(CAST((UPPER((COALESCE((((SELECT (sub_329.id + ((COALESCE(((sub_329.col_1 IN (SELECT sub_330.col_1 FROM tbl_05 AS sub_330)) < (sub_329.col_1 = 'alpha_beta_gamma')), UPPER((sub_329.col_1 / NULLIF(1.0, 0)))) <= (sub_329.col_4 IN (SELECT sub_331.col_4 FROM tbl_01 AS sub_331))) > (CAST((CAST(FALSE AS CHAR) IS NOT NULL) AS SIGNED) IS NULL))) FROM tbl_04 AS sub_329 LIMIT 1) + (1.0 IN (SELECT sub_332.col_4 FROM tbl_06 AS sub_332))) <= 999999999.99), ((SELECT sub_335.col_3 FROM tbl_06 AS sub_335 LIMIT 1) - NULLIF((CAST(((t_ref_268.col_4 IN (SELECT sub_336.col_2 FROM tbl_01 AS sub_336)) AND FALSE) AS DECIMAL(10,2)) OR ((t_ref_268.col_3 IN (SELECT sub_337.ref_tbl_01_id FROM tbl_02 AS sub_337)) + 42)), t_ref_268.col_4))) + COALESCE((EXISTS (SELECT 1 FROM tbl_06 AS sub_340 WHERE '__fuzz_marker__') / NULLIF((NOT (((CAST(NULLIF(((NOT ((LOWER(t_ref_268.col_3) / NULLIF(t_ref_268.id, 0)))) / NULLIF(UPPER(1), 0)), (SELECT CAST((sub_344.col_2 * sub_344.col_3) AS CHAR) FROM tbl_05 AS sub_344 LIMIT 1)) AS CHAR) < ((NOT (LOWER((((CASE WHEN t_ref_268.id THEN t_ref_268.col_3 ELSE t_ref_268.id END) OR EXISTS (SELECT 1 FROM tbl_05 AS sub_345 WHERE sub_345.col_4)) / NULLIF((t_ref_268.col_3 <> (SELECT TRUE FROM tbl_04 AS sub_346 LIMIT 1)), 0))))) - EXISTS (SELECT 1 FROM tbl_07 AS sub_347 WHERE FALSE))) - (((SELECT CAST((LENGTH(FALSE) AND COALESCE(sub_349.id, '')) AS SIGNED) FROM customers_large AS sub_349 LIMIT 1) BETWEEN 9223372036854775807 AND -1) - EXISTS (SELECT 1 FROM customers_large AS sub_350 WHERE FALSE))))), 0)), (t_ref_268.col_3 * NULL)))) <= FALSE) AS SIGNED), 'alpha_beta_gamma') BETWEEN 0 AND -2147483648))) AS c_351
FROM tbl_01 AS t_ref_268 LEFT JOIN tbl_05 AS j_ref_352 ON t_ref_268.id = j_ref_352.id
WHERE t_ref_268.col_1
LIMIT 45;

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2, dyn_col_4, dyn_col_5) VALUES (591403, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{}', '__fuzz_marker__', 'ref_val');
COMMIT;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (335598, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{}', 2147483647, 0.0000000001);

UPDATE tbl_01 SET col_2 = '{}' WHERE id > 18;

DELETE FROM tbl_04 WHERE id = 62715;

INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (635265, 'chaos', 'chaos', 'chaos', 1);

BEGIN;
INSERT INTO customers_large (id, payload) VALUES (650166, 'ref_val');
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (661187, '[]', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -9223372036854775808, 1e10);
INSERT INTO customers_large (id, payload) VALUES (345553, '');
COMMIT;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSON,
    col_3 VARCHAR(64),
    col_4 INT,
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

INSERT INTO customers_large (cust_id, company_name, profile_data) VALUES
    (1, 'Global Logistics Inc', '{"tier": "enterprise", "features": ["api", "audit", "sso"]}'),
    (2, 'Metro Retail LLC', '{"tier": "starter", "features": []}'),
    (3, 'Peak Analytics Corp', NULL);

ALTER TABLE tbl_07 ADD COLUMN dyn_col_5 VARCHAR(32);

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (112364, '[]', 100, FALSE, 1, -2147483648);
INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (903355, '__fuzz_marker__', '', '__fuzz_marker__', -1);
INSERT INTO tbl_04 (id, col_1, col_2, col_4) VALUES (70018, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

DELETE FROM tbl_01 WHERE id = 50842;

CREATE INDEX idx_tbl_07_col_1_5 ON tbl_07 (col_1);

INSERT INTO tbl_04 (id, col_1, col_2, col_4) VALUES (460383, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id) VALUES (426817, '__fuzz_marker__', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'alpha_beta_gamma', 42, 0);
INSERT INTO tbl_04 (id, col_1, col_2, col_4) VALUES (164741, '{}', '[]', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
INSERT INTO tbl_04 (id, col_1, col_2, col_4) VALUES (377700, '{}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{}');
COMMIT;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (63023, '{}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 1, -1.5);

INSERT INTO tbl_04 (id, col_1, col_2, col_4) VALUES (346913, '[]', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '[]');

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id) VALUES (15526, 'alpha_beta_gamma', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '__fuzz_marker__', -1, -2147483648);

UPDATE tbl_02 SET col_4 = 'chaos' WHERE id > 20;

UPDATE customers_large SET payload = '' WHERE id > 10;

CREATE INDEX idx_tbl_02_col_1_6 ON tbl_02 (col_1);

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (742146, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{}', 9223372036854775807, -1.5);

SELECT
    (UPPER((-1 IN (SELECT sub_354.col_4 FROM tbl_06 AS sub_354))) AND ((((LENGTH(LOWER((COALESCE((SELECT NULLIF(ABS(sub_355.col_1), NULL) FROM tbl_05 AS sub_355 LIMIT 1), (t_ref_353.id * (COALESCE((NULLIF(((((CASE WHEN t_ref_353.dyn_col_5 THEN (TRUE = TRUE) ELSE FALSE END) - t_ref_353.id) < ((CAST(3.14159265 AS CHAR) >= (t_ref_353.col_1 IN (SELECT sub_356.col_2 FROM tbl_03 AS sub_356))) * (SELECT (SELECT TRUE FROM tbl_02 AS inn_358 LIMIT 1) FROM tbl_02 AS sub_357 LIMIT 1))) OR CAST((t_ref_353.col_1 / NULLIF(t_ref_353.id, 0)) AS SIGNED)), LENGTH((NOT (FALSE)))) = FALSE), ((CAST(NULLIF(((SELECT TRUE FROM customers_large AS sub_359 LIMIT 1) / NULLIF(CAST((SELECT (SELECT 0.0 FROM tbl_03 AS inn_361 LIMIT 1) FROM tbl_04 AS sub_360 LIMIT 1) AS CHAR), 0)), COALESCE((SELECT sub_362.col_1 FROM tbl_06 AS sub_362 LIMIT 1), LENGTH(EXISTS (SELECT 1 FROM tbl_08 AS sub_363 WHERE FALSE)))) AS SIGNED) AND ((CASE WHEN t_ref_353.dyn_col_4 THEN FALSE ELSE UPPER((NULL <> NULL)) END) * UPPER(LENGTH((EXISTS (SELECT 1 FROM tbl_07 AS sub_364 WHERE sub_364.col_1) <= LENGTH(t_ref_353.col_2)))))) - t_ref_353.id)) * 1))) = (LOWER(t_ref_353.col_1) / NULLIF((t_ref_353.dyn_col_4 + 42), 0))))) OR t_ref_353.col_2) * (t_ref_353.dyn_col_4 IN (SELECT sub_365.ref_tbl_03_id FROM tbl_08 AS sub_365))) + ((CASE WHEN ((SELECT (SELECT inn_378.col_3 FROM tbl_05 AS inn_378 LIMIT 1) FROM tbl_05 AS sub_374 LIMIT 1) BETWEEN 100 AND 100) THEN (ABS(((CAST('alpha_beta_gamma' AS CHAR) > EXISTS (SELECT 1 FROM tbl_03 AS sub_379 WHERE sub_379.col_1)) = (CAST((NOT ((((NULLIF((NULLIF(LENGTH(ABS(NULL)), (SELECT '' FROM tbl_07 AS sub_380 LIMIT 1)) - UPPER(EXISTS (SELECT 1 FROM tbl_06 AS sub_381 WHERE sub_381.col_2))), (t_ref_353.col_2 IN (SELECT sub_382.col_4 FROM tbl_02 AS sub_382))) <= ((t_ref_353.dyn_col_4 - (LENGTH('chaos') <= CAST((t_ref_353.col_2 <= TRUE) AS CHAR))) > (t_ref_353.id IN (SELECT sub_383.col_4 FROM tbl_04 AS sub_383)))) > (t_ref_353.id IN (SELECT sub_385.dyn_col_5 FROM tbl_07 AS sub_385))) - ((SELECT (SELECT 'alpha_beta_gamma' FROM customers_large AS inn_390 LIMIT 1) FROM tbl_07 AS sub_388 LIMIT 1) + (SELECT (SELECT inn_393.dyn_col_4 FROM tbl_03 AS inn_393 LIMIT 1) FROM tbl_04 AS sub_391 LIMIT 1))))) AS CHAR) * CAST((CASE WHEN (t_ref_353.dyn_col_5 * LOWER((t_ref_353.col_1 / NULLIF((((((t_ref_353.col_2 / NULLIF(0.0000000001, 0)) <= (SELECT (SELECT inn_395.col_4 FROM tbl_01 AS inn_395 LIMIT 1) FROM tbl_02 AS sub_394 LIMIT 1)) > t_ref_353.col_2) AND (t_ref_353.col_2 AND (t_ref_353.dyn_col_4 IN (SELECT sub_396.ref_tbl_05_id FROM tbl_07 AS sub_396)))) <= (NOT ((CAST(NULL AS SIGNED) + (CASE WHEN CAST(t_ref_353.id AS CHAR) THEN CAST(t_ref_353.col_1 AS DECIMAL(10,2)) ELSE (t_ref_353.dyn_col_5 IN (SELECT sub_397.id FROM tbl_06 AS sub_397)) END))))), 0)))) THEN ((TRUE IS NULL) + (CASE WHEN (SELECT (SELECT inn_399.col_1 FROM tbl_07 AS inn_399 LIMIT 1) FROM tbl_08 AS sub_398 LIMIT 1) THEN (ABS(LOWER(((SELECT (SELECT inn_401.ref_tbl_02_id FROM tbl_06 AS inn_401 LIMIT 1) FROM customers_large AS sub_400 LIMIT 1) < t_ref_353.col_2))) / NULLIF((CASE WHEN ((CASE WHEN (CASE WHEN EXISTS (SELECT 1 FROM tbl_08 AS sub_402 WHERE sub_402.col_3) THEN EXISTS (SELECT 1 FROM customers_large AS sub_403 WHERE sub_403.payload) ELSE ('escaped''quote' IN (SELECT sub_404.col_1 FROM tbl_03 AS sub_404)) END) THEN ((FALSE * t_ref_353.dyn_col_5) - (t_ref_353.dyn_col_4 AND t_ref_353.id)) ELSE (CAST(FALSE AS DECIMAL(10,2)) / NULLIF((t_ref_353.col_1 IN (SELECT sub_405.payload FROM customers_large AS sub_405)), 0)) END) < (SELECT NULLIF(999999999.99, sub_406.col_1) FROM tbl_04 AS sub_406 LIMIT 1)) THEN (SELECT (('escaped''quote' IN (SELECT sub_408.id FROM tbl_06 AS sub_408)) > (sub_407.col_1 OR sub_407.col_2)) FROM tbl_05 AS sub_407 LIMIT 1) ELSE (SELECT (SELECT inn_411.col_2 FROM tbl_06 AS inn_411 LIMIT 1) FROM customers_large AS sub_409 LIMIT 1) END), 0)) ELSE t_ref_353.col_1 END)) ELSE t_ref_353.col_1 END) AS DECIMAL(10,2))))) <> t_ref_353.col_2) ELSE ((t_ref_353.col_2 IN (SELECT sub_412.id FROM tbl_08 AS sub_412)) IS NOT NULL) END) / NULLIF(t_ref_353.col_1, 0))) <= (CAST(t_ref_353.dyn_col_4 AS CHAR) AND (NOT (t_ref_353.col_1))))) AS c_415,
    (CAST(ABS(((t_ref_353.id <> (((t_ref_353.col_2 IN (SELECT sub_416.id FROM tbl_04 AS sub_416)) * (SELECT (SELECT inn_426.id FROM tbl_02 AS inn_426 LIMIT 1) FROM tbl_04 AS sub_420 LIMIT 1)) * t_ref_353.col_2)) + (CASE WHEN (CASE WHEN TRUE THEN (SELECT (NULL - (SELECT (sub_428.id IN (SELECT sub_429.col_4 FROM tbl_02 AS sub_429)) FROM customers_large AS sub_428 LIMIT 1)) FROM tbl_04 AS sub_427 LIMIT 1) ELSE (t_ref_353.dyn_col_5 IN (SELECT sub_433.payload FROM customers_large AS sub_433)) END) THEN t_ref_353.dyn_col_4 ELSE (SELECT (SELECT inn_443.ref_tbl_01_id FROM tbl_02 AS inn_443 LIMIT 1) FROM tbl_03 AS sub_434 LIMIT 1) END))) AS DECIMAL(10,2)) - t_ref_353.col_1) AS c_444,
    t_ref_353.dyn_col_4 AS c_445
FROM tbl_03 AS t_ref_353 LEFT JOIN tbl_06 AS j_ref_446 ON t_ref_353.id = j_ref_446.id
WHERE (EXISTS (SELECT 1 FROM customers_large AS sub_447 WHERE FALSE) * (SELECT (SELECT inn_543.payload FROM customers_large AS inn_543 LIMIT 1) FROM tbl_05 AS sub_460 LIMIT 1))
LIMIT 39;

DELETE FROM tbl_08 WHERE id = 90407;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_4) VALUES (909806, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{}');
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (663795, '{}', 2147483647, TRUE, 9223372036854775807, -1);
COMMIT;

SELECT
    t_ref_544.col_1 AS c_545,
    t_ref_544.col_2 AS c_546,
    ((t_ref_544.col_1 - (TRUE BETWEEN -9223372036854775808 AND 2147483647)) AND t_ref_544.id) AS c_547
FROM tbl_04 AS t_ref_544 LEFT JOIN tbl_08 AS j_ref_548 ON t_ref_544.id = j_ref_548.id
WHERE '__fuzz_marker__'
LIMIT 33;

CREATE VIEW v_summary_5 AS SELECT id FROM customers_large WHERE id IS NOT NULL;

CREATE INDEX idx_tbl_06_col_3_7 ON tbl_06 (col_3);

SELECT
    (NULL BETWEEN 2147483647 AND -1) AS c_550,
    (SELECT (CAST(CAST((ABS((sub_551.col_3 + ((NULL OR ((SELECT (SELECT (TRUE IN (SELECT sub_554.id FROM tbl_05 AS sub_554)) FROM tbl_04 AS sub_553 LIMIT 1) FROM tbl_01 AS sub_552 LIMIT 1) IS NOT NULL)) + FALSE))) - ((TRUE IS NOT NULL) OR sub_551.col_4)) AS CHAR) AS CHAR) / NULLIF(0.0, 0)) FROM tbl_02 AS sub_551 LIMIT 1) AS c_556
FROM tbl_08 AS t_ref_549 LEFT JOIN customers_large AS j_ref_557 ON t_ref_549.id = j_ref_557.id
WHERE 'ref_val'
LIMIT 4;

INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_05_id, dyn_col_5) VALUES (264345, '{}', FALSE, 9223372036854775807, '__fuzz_marker__');

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 JSON,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id) VALUES (288877, 'ref_val', '[]', '__fuzz_marker__', 2147483647, 0);

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSON,
    col_3 NUMERIC(10,2)
);

INSERT INTO tbl_04 (id, col_1, col_2, col_4) VALUES (831812, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '[]', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

INSERT INTO orders_large (ord_id, cust_id, notes, order_val) VALUES
    (101, 1, REPEAT('Order confirmation notes. ', 20), 4500.00),
    (102, 1, REPEAT('Expedited shipping request. ', 15), 1200.00),
    (201, 2, 'Standard freight delivery.', 350.00),
    (301, 3, REPEAT('Custom packaging specifications. ', 25), 8900.00);

SELECT
    (((t_ref_558.ref_tbl_01_id AND (t_ref_558.id / NULLIF((FALSE < (SELECT (SELECT inn_594.id FROM tbl_01 AS inn_594 LIMIT 1) FROM tbl_09 AS sub_559 LIMIT 1)), 0))) <= t_ref_558.col_2) IS NULL) AS c_595,
    (t_ref_558.col_4 IN (SELECT sub_596.col_4 FROM tbl_04 AS sub_596)) AS c_605,
    (9223372036854775807 IN (SELECT sub_606.id FROM tbl_09 AS sub_606)) AS c_618
FROM tbl_02 AS t_ref_558 LEFT JOIN tbl_06 AS j_ref_619 ON t_ref_558.id = j_ref_619.id
WHERE LOWER((CAST(LENGTH((SELECT CAST(sub_620.ref_tbl_03_id AS CHAR) FROM tbl_08 AS sub_620 LIMIT 1)) AS SIGNED) IS NOT NULL))
LIMIT 26;

UPDATE customers_large SET payload = 'escaped''quote' WHERE id > 36;

SELECT
    CAST(t_ref_621.id AS DECIMAL(10,2)) AS c_622,
    COALESCE(t_ref_621.col_4, -9223372036854775808) AS c_623,
    (t_ref_621.col_4 / NULLIF(((SELECT (SELECT inn_625.dyn_col_5 FROM tbl_03 AS inn_625 LIMIT 1) FROM tbl_04 AS sub_624 LIMIT 1) BETWEEN -9223372036854775808 AND 42), 0)) AS c_626
FROM tbl_08 AS t_ref_621
WHERE 9223372036854775807
LIMIT 14;

CREATE INDEX idx_tbl_01_col_2_8 ON tbl_01 (col_2);

SELECT
    (LOWER(t_ref_627.col_3) * ABS((t_ref_627.col_4 * t_ref_627.col_3))) AS c_628
FROM tbl_01 AS t_ref_627
WHERE (t_ref_627.col_2 IN (SELECT sub_629.col_4 FROM tbl_04 AS sub_629))
LIMIT 28;

INSERT INTO customers_large (id, payload) VALUES (457685, '__fuzz_marker__');

CREATE INDEX idx_tbl_04_id_9 ON tbl_04 (id);

CREATE INDEX idx_tbl_03_id_10 ON tbl_03 (id);

SELECT
    '__fuzz_marker__' AS c_632,
    ABS(NULLIF((CASE WHEN (SELECT ((3.14159265 OR (-1.5 IN (SELECT sub_634.col_1 FROM tbl_07 AS sub_634))) - sub_633.ref_tbl_01_id) FROM tbl_02 AS sub_633 LIMIT 1) THEN NULLIF((1e10 IN (SELECT sub_635.col_4 FROM tbl_02 AS sub_635)), COALESCE((CASE WHEN (CAST(t_ref_631.col_1 AS DECIMAL(10,2)) + (CAST((((SELECT (SELECT inn_638.payload FROM customers_large AS inn_638 LIMIT 1) FROM tbl_08 AS sub_637 LIMIT 1) IS NULL) * (LENGTH(((LENGTH((NOT (CAST(NULL AS DECIMAL(10,2))))) - (t_ref_631.col_3 IN (SELECT sub_639.col_4 FROM tbl_01 AS sub_639))) <= EXISTS (SELECT 1 FROM tbl_06 AS sub_640 WHERE sub_640.col_3))) < ((('chaos' / NULLIF(((NOT ((t_ref_631.id IN (SELECT sub_643.col_4 FROM tbl_02 AS sub_643)))) > (CAST(CAST((NULL >= t_ref_631.id) AS DECIMAL(10,2)) AS CHAR) * (42 * t_ref_631.col_1))), 0)) < ((LENGTH((((SELECT TRUE FROM tbl_03 AS sub_645 LIMIT 1) + (SELECT (SELECT 100 FROM tbl_07 AS inn_647 LIMIT 1) FROM customers_large AS sub_646 LIMIT 1)) > (NOT ((TRUE * t_ref_631.col_1))))) + CAST(((TRUE BETWEEN 2147483647 AND 100) < (COALESCE(t_ref_631.col_1, t_ref_631.col_1) - t_ref_631.col_1)) AS DECIMAL(10,2))) AND (COALESCE(t_ref_631.col_3, t_ref_631.col_2) * ((SELECT (SELECT FALSE FROM tbl_04 AS inn_649 LIMIT 1) FROM tbl_01 AS sub_648 LIMIT 1) <= ABS((UPPER(t_ref_631.col_3) - t_ref_631.id)))))) <> (t_ref_631.col_2 = (SELECT CAST(NULL AS SIGNED) FROM tbl_06 AS sub_650 LIMIT 1))))) AS CHAR) / NULLIF((t_ref_631.col_4 IN (SELECT sub_651.id FROM tbl_06 AS sub_651)), 0))) THEN t_ref_631.id ELSE EXISTS (SELECT 1 FROM tbl_08 AS sub_654 WHERE sub_654.ref_tbl_03_id) END), COALESCE(NULL, (t_ref_631.col_4 + (TRUE IN (SELECT sub_660.col_4 FROM tbl_01 AS sub_660)))))) ELSE (CASE WHEN (SELECT (NOT ((((COALESCE((LOWER(CAST((EXISTS (SELECT 1 FROM tbl_10 AS sub_665 WHERE sub_665.col_1) / NULLIF((sub_664.col_1 IN (SELECT sub_666.id FROM tbl_02 AS sub_666)), 0)) AS SIGNED)) >= (((SELECT (SELECT 'escaped''quote' FROM customers_large AS inn_671 LIMIT 1) FROM tbl_01 AS sub_668 LIMIT 1) * -9223372036854775808) - (SELECT (42 AND (999999999.99 IN (SELECT sub_673.col_1 FROM tbl_03 AS sub_673))) FROM tbl_01 AS sub_672 LIMIT 1))), sub_664.col_2) BETWEEN 0 AND 42) = (sub_664.col_4 IN (SELECT sub_675.col_3 FROM tbl_10 AS sub_675))) + (CASE WHEN (((EXISTS (SELECT 1 FROM customers_large AS sub_676 WHERE sub_676.id) * (((SELECT (SELECT (FALSE > sub_678.payload) FROM customers_large AS sub_678 LIMIT 1) FROM tbl_03 AS sub_677 LIMIT 1) / NULLIF((((SELECT (NULL = sub_679.col_2) FROM tbl_04 AS sub_679 LIMIT 1) + ((SELECT (SELECT inn_681.col_1 FROM tbl_07 AS inn_681 LIMIT 1) FROM tbl_05 AS sub_680 LIMIT 1) * ((TRUE <= TRUE) >= (SELECT (SELECT inn_683.col_1 FROM tbl_10 AS inn_683 LIMIT 1) FROM tbl_07 AS sub_682 LIMIT 1)))) <> (((sub_664.id / NULLIF((SELECT (SELECT inn_685.col_2 FROM tbl_06 AS inn_685 LIMIT 1) FROM tbl_08 AS sub_684 LIMIT 1), 0)) IS NULL) OR sub_664.col_4)), 0)) * sub_664.col_1)) * '__fuzz_marker__') + (ABS((SELECT (NULLIF(CAST((('ref_val' AND -9223372036854775808) / NULLIF(sub_686.id, 0)) AS CHAR), EXISTS (SELECT 1 FROM tbl_07 AS sub_687 WHERE sub_687.col_2)) >= CAST(100 AS SIGNED)) FROM customers_large AS sub_686 LIMIT 1)) IS NOT NULL)) THEN sub_664.id ELSE CAST(LOWER(NULLIF((sub_664.col_4 IN (SELECT sub_688.id FROM customers_large AS sub_688)), sub_664.id)) AS DECIMAL(10,2)) END)))) FROM tbl_04 AS sub_664 LIMIT 1) THEN NULL ELSE (EXISTS (SELECT 1 FROM tbl_04 AS sub_691 WHERE TRUE) < ('' * NULLIF(LENGTH((LENGTH(LENGTH((SELECT LOWER('escaped''quote') FROM customers_large AS sub_711 LIMIT 1))) + t_ref_631.col_3)), ABS(UPPER(t_ref_631.col_2))))) END) END), (SELECT ((CASE WHEN (sub_712.col_3 - sub_712.col_3) THEN NULLIF(CAST((EXISTS (SELECT 1 FROM tbl_04 AS sub_713 WHERE sub_713.col_2) AND LENGTH((sub_712.col_4 <> (NOT ((sub_712.col_3 - -1e-5)))))) AS CHAR), (LOWER((NULL IN (SELECT sub_716.col_3 FROM tbl_06 AS sub_716))) * (LOWER((CASE WHEN sub_712.col_2 THEN ((NULL IN (SELECT sub_722.id FROM tbl_10 AS sub_722)) <= ((sub_712.col_1 + ((CASE WHEN FALSE THEN (CASE WHEN ('__fuzz_marker__' <= EXISTS (SELECT 1 FROM tbl_05 AS sub_723 WHERE 1.0)) THEN (CAST(NULL AS DECIMAL(10,2)) - CAST(FALSE AS DECIMAL(10,2))) ELSE TRUE END) ELSE (LENGTH(NULLIF(sub_712.id, sub_712.col_1)) AND sub_712.col_4) END) <> (CASE WHEN ((SELECT (SELECT inn_725.col_1 FROM tbl_08 AS inn_725 LIMIT 1) FROM tbl_02 AS sub_724 LIMIT 1) - CAST((sub_712.id IN (SELECT sub_726.id FROM tbl_03 AS sub_726)) AS CHAR)) THEN TRUE ELSE FALSE END))) * (NULL / NULLIF(sub_712.col_2, 0)))) ELSE ((sub_712.col_4 * sub_712.col_1) + (((CASE WHEN (LOWER(((42 IS NOT NULL) IS NULL)) / NULLIF((sub_712.col_1 IN (SELECT sub_727.col_4 FROM tbl_05 AS sub_727)), 0)) THEN CAST(FALSE AS SIGNED) ELSE sub_712.id END) >= (EXISTS (SELECT 1 FROM customers_large AS sub_728 WHERE 'escaped''quote') OR CAST((((SELECT TRUE FROM tbl_01 AS sub_729 LIMIT 1) * (sub_712.col_4 AND sub_712.col_2)) > EXISTS (SELECT 1 FROM tbl_09 AS sub_730 WHERE sub_730.id)) AS SIGNED))) BETWEEN 1 AND -9223372036854775808)) END)) <= (sub_712.col_1 + (CASE WHEN CAST((CASE WHEN ((SELECT (SELECT inn_734.col_4 FROM tbl_02 AS inn_734 LIMIT 1) FROM tbl_04 AS sub_731 LIMIT 1) <= EXISTS (SELECT 1 FROM tbl_04 AS sub_735 WHERE sub_735.col_4)) THEN (sub_712.col_3 IN (SELECT sub_737.col_3 FROM tbl_10 AS sub_737)) ELSE (SELECT (TRUE AND COALESCE((SELECT sub_740.payload FROM customers_large AS sub_740 LIMIT 1), sub_739.id)) FROM customers_large AS sub_739 LIMIT 1) END) AS CHAR) THEN (CAST((ABS(((SELECT (sub_741.id <> TRUE) FROM customers_large AS sub_741 LIMIT 1) * (TRUE IN (SELECT sub_742.col_3 FROM tbl_10 AS sub_742)))) + TRUE) AS CHAR) AND sub_712.id) ELSE ((CASE WHEN CAST(((SELECT (SELECT inn_745.col_3 FROM tbl_06 AS inn_745 LIMIT 1) FROM tbl_06 AS sub_743 LIMIT 1) + ABS((NULLIF(COALESCE(sub_712.id, FALSE), (sub_712.col_1 IN (SELECT sub_746.col_2 FROM tbl_01 AS sub_746))) / NULLIF((CASE WHEN UPPER(sub_712.ref_tbl_02_id) THEN (NULL / NULLIF('alpha_beta_gamma', 0)) ELSE 'alpha_beta_gamma' END), 0)))) AS DECIMAL(10,2)) THEN CAST((NOT ((CASE WHEN sub_712.id THEN sub_712.id ELSE sub_712.id END))) AS DECIMAL(10,2)) ELSE COALESCE(((SELECT ((CASE WHEN FALSE THEN sub_747.col_1 ELSE sub_747.id END) - sub_747.col_3) FROM tbl_05 AS sub_747 LIMIT 1) - (CAST(999999999.99 AS DECIMAL(10,2)) >= COALESCE(NULLIF((-2147483648 IN (SELECT sub_748.ref_tbl_01_id FROM tbl_02 AS sub_748)), EXISTS (SELECT 1 FROM tbl_07 AS sub_749 WHERE '__fuzz_marker__')), (sub_712.ref_tbl_02_id IN (SELECT sub_750.payload FROM customers_large AS sub_750))))), ((SELECT (SELECT inn_752.col_3 FROM tbl_01 AS inn_752 LIMIT 1) FROM tbl_04 AS sub_751 LIMIT 1) >= (SELECT (SELECT inn_754.col_3 FROM tbl_10 AS inn_754 LIMIT 1) FROM tbl_08 AS sub_753 LIMIT 1))) END) / NULLIF((sub_712.col_3 IN (SELECT sub_755.col_1 FROM tbl_10 AS sub_755)), 0)) END))))) ELSE sub_712.id END) IS NOT NULL) FROM tbl_06 AS sub_712 LIMIT 1))) AS c_760
FROM tbl_01 AS t_ref_631 LEFT JOIN tbl_03 AS j_ref_761 ON t_ref_631.id = j_ref_761.id
WHERE ((((t_ref_631.col_2 AND ((NULL + t_ref_631.col_2) - TRUE)) <> (t_ref_631.col_2 + LOWER(CAST(CAST(0 AS CHAR) AS CHAR)))) * (SELECT (SELECT inn_763.col_4 FROM tbl_04 AS inn_763 LIMIT 1) FROM tbl_09 AS sub_762 LIMIT 1)) > (NULL IS NOT NULL))
LIMIT 46;

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 INT,
    col_3 BOOLEAN,
    col_4 JSON
);

SELECT
    EXISTS (SELECT 1 FROM tbl_10 AS sub_765 WHERE 1e10) AS c_814
FROM tbl_10 AS t_ref_764
WHERE NULLIF(t_ref_764.col_3, (SELECT CAST((LOWER((LENGTH((NOT (CAST(CAST(sub_815.col_4 AS SIGNED) AS DECIMAL(10,2))))) AND (SELECT (SELECT '' FROM tbl_08 AS inn_817 LIMIT 1) FROM tbl_06 AS sub_816 LIMIT 1))) / NULLIF(COALESCE((CAST(sub_815.col_4 AS CHAR) / NULLIF((UPPER(sub_815.col_4) * sub_815.ref_tbl_02_id), 0)), (COALESCE(EXISTS (SELECT 1 FROM tbl_06 AS sub_818 WHERE TRUE), ((CASE WHEN ((TRUE <= ABS(sub_815.col_3)) / NULLIF(UPPER(NULLIF(EXISTS (SELECT 1 FROM tbl_02 AS sub_819 WHERE sub_819.col_1), EXISTS (SELECT 1 FROM tbl_10 AS sub_820 WHERE sub_820.col_3))), 0)) THEN TRUE ELSE (sub_815.id IN (SELECT sub_823.col_2 FROM tbl_01 AS sub_823)) END) - (EXISTS (SELECT 1 FROM tbl_06 AS sub_824 WHERE sub_824.col_3) AND (((SELECT (SELECT inn_827.col_2 FROM tbl_01 AS inn_827 LIMIT 1) FROM tbl_02 AS sub_826 LIMIT 1) OR ((((SELECT ((SELECT (SELECT NULL FROM tbl_03 AS inn_830 LIMIT 1) FROM tbl_05 AS sub_829 LIMIT 1) / NULLIF(CAST(sub_828.col_2 AS SIGNED), 0)) FROM tbl_11 AS sub_828 LIMIT 1) >= (((SELECT (SELECT inn_832.col_1 FROM tbl_05 AS inn_832 LIMIT 1) FROM tbl_10 AS sub_831 LIMIT 1) <= CAST((SELECT sub_833.col_3 FROM tbl_05 AS sub_833 LIMIT 1) AS DECIMAL(10,2))) - 9223372036854775807)) OR (FALSE / NULLIF((sub_815.col_1 + (sub_815.col_4 - (SELECT sub_834.payload FROM customers_large AS sub_834 LIMIT 1))), 0))) * (CAST(CAST((3.14159265 / NULLIF((sub_815.col_2 IN (SELECT sub_835.col_2 FROM tbl_02 AS sub_835)), 0)) AS CHAR) AS CHAR) - (-1e-5 IN (SELECT sub_836.col_3 FROM tbl_01 AS sub_836))))) > EXISTS (SELECT 1 FROM tbl_03 AS sub_839 WHERE sub_839.dyn_col_4))))) = ((CAST(sub_815.id AS DECIMAL(10,2)) > sub_815.col_4) BETWEEN -1 AND -1))), 0)) AS DECIMAL(10,2)) FROM tbl_06 AS sub_815 LIMIT 1))
LIMIT 25;

CREATE INDEX idx_tbl_03_dyn_col_4_11 ON tbl_03 (dyn_col_4);

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 NUMERIC(10,2)
);

CREATE INDEX idx_tbl_08_col_1_12 ON tbl_08 (col_1);

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (174787, '{}', 100, TRUE, -2147483648, -9223372036854775808);
INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_05_id, dyn_col_5) VALUES (673204, '[]', FALSE, 100, 'chaos');
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (674666, '{}', '{}', 100, 1.0);
COMMIT;

UPDATE tbl_07 SET col_2 = FALSE WHERE id > 49;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (669300, '', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', TRUE, 'ref_val', 0);

SELECT
    EXISTS (SELECT 1 FROM tbl_12 AS sub_846 WHERE sub_846.id) AS c_847,
    LENGTH((EXISTS (SELECT 1 FROM tbl_08 AS sub_848 WHERE sub_848.ref_tbl_03_id) / NULLIF((((SELECT COALESCE(ABS(LENGTH(1e10)), ((SELECT (SELECT (TRUE BETWEEN -1 AND 1) FROM tbl_10 AS sub_857 LIMIT 1) FROM tbl_06 AS sub_856 LIMIT 1) OR FALSE)) FROM tbl_02 AS sub_855 LIMIT 1) OR (NOT (((SELECT (CAST(2147483647 AS DECIMAL(10,2)) + EXISTS (SELECT 1 FROM tbl_01 AS sub_859 WHERE sub_859.col_2)) FROM tbl_09 AS sub_858 LIMIT 1) / NULLIF(CAST((CAST(LENGTH((SELECT (SELECT 'alpha_beta_gamma' FROM tbl_07 AS inn_864 LIMIT 1) FROM tbl_11 AS sub_863 LIMIT 1)) AS SIGNED) OR TRUE) AS SIGNED), 0))))) - ABS((NULL > (LENGTH((SELECT EXISTS (SELECT 1 FROM tbl_11 AS sub_866 WHERE sub_866.col_1) FROM tbl_08 AS sub_865 LIMIT 1)) > NULL)))), 0))) AS c_869
FROM tbl_06 AS t_ref_845
WHERE (CAST((CASE WHEN UPPER(t_ref_845.ref_tbl_02_id) THEN FALSE ELSE 0 END) AS DECIMAL(10,2)) / NULLIF('chaos', 0))
LIMIT 14;

ALTER TABLE tbl_07 DROP COLUMN dyn_col_5;

SELECT
    (SELECT (SELECT inn_883.col_1 FROM tbl_09 AS inn_883 LIMIT 1) FROM tbl_08 AS sub_871 LIMIT 1) AS c_884
FROM tbl_12 AS t_ref_870
WHERE (CASE WHEN TRUE THEN ((CAST((EXISTS (SELECT 1 FROM tbl_08 AS sub_885 WHERE FALSE) + FALSE) AS DECIMAL(10,2)) BETWEEN 0 AND 100) - t_ref_870.col_2) ELSE LENGTH((LOWER((t_ref_870.col_1 IS NOT NULL)) + ((t_ref_870.col_2 IN (SELECT sub_887.col_4 FROM tbl_08 AS sub_887)) OR ABS(((CASE WHEN (CAST(t_ref_870.id AS DECIMAL(10,2)) - (SELECT (CAST(sub_892.id AS CHAR) + (SELECT (SELECT inn_896.dyn_col_4 FROM tbl_03 AS inn_896 LIMIT 1) FROM tbl_11 AS sub_893 LIMIT 1)) FROM tbl_07 AS sub_892 LIMIT 1)) THEN (CASE WHEN (t_ref_870.id / NULLIF(t_ref_870.col_1, 0)) THEN (CASE WHEN ((SELECT (sub_897.ref_tbl_02_id IS NULL) FROM tbl_06 AS sub_897 LIMIT 1) / NULLIF(((SELECT (SELECT inn_899.col_1 FROM tbl_10 AS inn_899 LIMIT 1) FROM tbl_08 AS sub_898 LIMIT 1) - (COALESCE((SELECT (sub_900.dyn_col_5 * ((SELECT sub_901.col_2 FROM tbl_09 AS sub_901 LIMIT 1) / NULLIF(((NOT (NULL)) * (NULL = sub_900.id)), 0))) FROM tbl_03 AS sub_900 LIMIT 1), NULLIF((SELECT (sub_902.col_3 IN (SELECT sub_903.ref_tbl_01_id FROM tbl_09 AS sub_903)) FROM tbl_09 AS sub_902 LIMIT 1), EXISTS (SELECT 1 FROM tbl_07 AS sub_904 WHERE sub_904.id))) + CAST((SELECT (SELECT inn_908.col_1 FROM tbl_12 AS inn_908 LIMIT 1) FROM tbl_08 AS sub_905 LIMIT 1) AS SIGNED))), 0)) THEN CAST(ABS(UPPER(t_ref_870.col_2)) AS CHAR) ELSE CAST(CAST((((SELECT (SELECT TRUE FROM tbl_05 AS inn_911 LIMIT 1) FROM tbl_04 AS sub_909 LIMIT 1) / NULLIF(((((SELECT sub_912.col_1 FROM tbl_01 AS sub_912 LIMIT 1) + COALESCE((COALESCE(t_ref_870.col_1, t_ref_870.col_2) AND t_ref_870.col_2), (TRUE + 'alpha_beta_gamma'))) * CAST(CAST((t_ref_870.col_2 IN (SELECT sub_913.col_4 FROM tbl_02 AS sub_913)) AS CHAR) AS DECIMAL(10,2))) - t_ref_870.col_2), 0)) - ((EXISTS (SELECT 1 FROM tbl_10 AS sub_914 WHERE sub_914.id) / NULLIF((SELECT sub_916.col_4 FROM tbl_11 AS sub_916 LIMIT 1), 0)) / NULLIF('alpha_beta_gamma', 0))) AS SIGNED) AS DECIMAL(10,2)) END) ELSE -1.5 END) ELSE (t_ref_870.col_2 >= ((SELECT (SELECT TRUE FROM tbl_08 AS inn_918 LIMIT 1) FROM tbl_06 AS sub_917 LIMIT 1) * TRUE)) END) * (CASE WHEN ((CAST(0.0000000001 AS DECIMAL(10,2)) <= (((SELECT (SELECT inn_922.col_1 FROM tbl_11 AS inn_922 LIMIT 1) FROM tbl_03 AS sub_919 LIMIT 1) + CAST((t_ref_870.col_1 IN (SELECT sub_923.col_4 FROM tbl_08 AS sub_923)) AS CHAR)) + (NOT (-1e-5)))) = ((SELECT CAST((9223372036854775807 IS NOT NULL) AS CHAR) FROM customers_large AS sub_924 LIMIT 1) > TRUE)) THEN (CASE WHEN t_ref_870.col_2 THEN (t_ref_870.col_1 IN (SELECT sub_925.col_2 FROM tbl_01 AS sub_925)) ELSE ((t_ref_870.col_1 / NULLIF(((EXISTS (SELECT 1 FROM tbl_10 AS sub_936 WHERE sub_936.col_1) - (SELECT ((sub_941.id IS NOT NULL) / NULLIF((EXISTS (SELECT 1 FROM tbl_06 AS sub_942 WHERE TRUE) - TRUE), 0)) FROM tbl_01 AS sub_941 LIMIT 1)) AND LOWER(CAST((t_ref_870.col_1 IN (SELECT sub_943.id FROM tbl_12 AS sub_943)) AS SIGNED))), 0)) / NULLIF((NULL * (SELECT (SELECT inn_951.col_4 FROM tbl_06 AS inn_951 LIMIT 1) FROM tbl_11 AS sub_944 LIMIT 1)), 0)) END) ELSE TRUE END)))))) END)
LIMIT 3;

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON,
    col_3 NUMERIC(10,2)
);

CREATE INDEX idx_tbl_12_col_1_13 ON tbl_12 (col_1);

DELETE FROM tbl_12 WHERE id = 22425;

SELECT
    (CASE WHEN COALESCE(CAST((CAST(((t_ref_952.col_4 / NULLIF(t_ref_952.col_4, 0)) / NULLIF((SELECT (SELECT inn_954.col_2 FROM tbl_07 AS inn_954 LIMIT 1) FROM tbl_07 AS sub_953 LIMIT 1), 0)) AS CHAR) BETWEEN 1 AND 42) AS SIGNED), ABS(CAST((t_ref_952.col_3 AND ((UPPER(EXISTS (SELECT 1 FROM tbl_09 AS sub_955 WHERE NULL)) - t_ref_952.col_1) / NULLIF(EXISTS (SELECT 1 FROM tbl_09 AS sub_966 WHERE NULL), 0))) AS SIGNED))) THEN t_ref_952.id ELSE EXISTS (SELECT 1 FROM tbl_04 AS sub_967 WHERE sub_967.col_4) END) AS c_968
FROM tbl_06 AS t_ref_952
WHERE EXISTS (SELECT 1 FROM tbl_08 AS sub_969 WHERE sub_969.col_3)
LIMIT 1;

SELECT
    t_ref_974.col_2 AS c_975,
    NULLIF(t_ref_974.col_1, CAST((CASE WHEN (CASE WHEN (SELECT (CAST(sub_976.id AS SIGNED) > CAST(COALESCE(UPPER(EXISTS (SELECT 1 FROM tbl_02 AS sub_977 WHERE 'escaped''quote')), sub_976.id) AS CHAR)) FROM tbl_13 AS sub_976 LIMIT 1) THEN (((NULL - t_ref_974.col_3) > (t_ref_974.col_4 IN (SELECT sub_978.col_1 FROM tbl_12 AS sub_978))) + ((((EXISTS (SELECT 1 FROM tbl_03 AS sub_988 WHERE sub_988.dyn_col_4) + EXISTS (SELECT 1 FROM tbl_05 AS sub_989 WHERE sub_989.col_4)) BETWEEN 1 AND 9223372036854775807) < (t_ref_974.col_1 IN (SELECT sub_990.col_1 FROM tbl_09 AS sub_990))) + NULLIF((SELECT (SELECT (SELECT inn_993.col_2 FROM tbl_09 AS inn_993 LIMIT 1) FROM tbl_09 AS sub_992 LIMIT 1) FROM tbl_06 AS sub_991 LIMIT 1), 1.0))) ELSE CAST((EXISTS (SELECT 1 FROM tbl_02 AS sub_994 WHERE 1) <> ((SELECT ((CASE WHEN ((CASE WHEN (((CASE WHEN CAST((ABS(sub_1022.col_2) * (SELECT sub_1023.id FROM tbl_13 AS sub_1023 LIMIT 1)) AS SIGNED) THEN ((sub_1022.col_3 IS NOT NULL) * (SELECT 42 FROM customers_large AS sub_1024 LIMIT 1)) ELSE (CAST(LENGTH(sub_1022.id) AS SIGNED) * EXISTS (SELECT 1 FROM tbl_06 AS sub_1025 WHERE 1.0)) END) = sub_1022.col_3) + (3.14159265 IN (SELECT sub_1026.col_2 FROM tbl_08 AS sub_1026))) THEN (TRUE / NULLIF(ABS((CAST((sub_1022.id IN (SELECT sub_1027.col_3 FROM tbl_10 AS sub_1027)) AS CHAR) <= NULL)), 0)) ELSE (ABS(((SELECT NULL FROM customers_large AS sub_1028 LIMIT 1) >= LOWER(sub_1022.col_3))) * LENGTH((SELECT (TRUE BETWEEN -1 AND -1) FROM tbl_13 AS sub_1029 LIMIT 1))) END) <= CAST((COALESCE(TRUE, (TRUE + sub_1022.col_3)) - COALESCE(FALSE, (SELECT (SELECT inn_1031.col_1 FROM tbl_12 AS inn_1031 LIMIT 1) FROM tbl_07 AS sub_1030 LIMIT 1))) AS CHAR)) THEN (CASE WHEN EXISTS (SELECT 1 FROM customers_large AS sub_1032 WHERE sub_1032.payload) THEN (CASE WHEN EXISTS (SELECT 1 FROM tbl_10 AS sub_1033 WHERE sub_1033.col_2) THEN (LENGTH((sub_1022.col_1 IN (SELECT sub_1034.id FROM tbl_13 AS sub_1034))) / NULLIF(sub_1022.id, 0)) ELSE (CAST((((sub_1022.col_1 IN (SELECT sub_1035.ref_tbl_05_id FROM tbl_07 AS sub_1035)) + EXISTS (SELECT 1 FROM tbl_13 AS sub_1036 WHERE sub_1036.col_1)) - (SELECT (SELECT inn_1038.col_1 FROM tbl_11 AS inn_1038 LIMIT 1) FROM tbl_01 AS sub_1037 LIMIT 1)) AS CHAR) = EXISTS (SELECT 1 FROM customers_large AS sub_1039 WHERE sub_1039.payload)) END) ELSE sub_1022.col_2 END) ELSE (2147483647 IN (SELECT sub_1040.col_1 FROM tbl_10 AS sub_1040)) END) - (NULLIF(COALESCE(COALESCE(COALESCE((((sub_1022.col_1 - sub_1022.id) * NULLIF('alpha_beta_gamma', (SELECT TRUE FROM tbl_07 AS sub_1044 LIMIT 1))) / NULLIF(EXISTS (SELECT 1 FROM tbl_01 AS sub_1045 WHERE sub_1045.col_4), 0)), (sub_1022.id / NULLIF((SELECT (CASE WHEN sub_1046.col_4 THEN sub_1046.ref_tbl_02_id ELSE sub_1046.col_1 END) FROM tbl_06 AS sub_1046 LIMIT 1), 0))), (CASE WHEN ABS(NULL) THEN (SELECT (SELECT 9223372036854775807 FROM tbl_12 AS inn_1049 LIMIT 1) FROM tbl_12 AS sub_1047 LIMIT 1) ELSE ((SELECT 42 FROM tbl_11 AS sub_1050 LIMIT 1) * ((CASE WHEN -1.5 THEN sub_1022.col_2 ELSE (FALSE <> sub_1022.col_2) END) < (CASE WHEN ('chaos' IN (SELECT sub_1051.col_1 FROM tbl_09 AS sub_1051)) THEN (CASE WHEN sub_1022.id THEN sub_1022.col_1 ELSE sub_1022.col_1 END) ELSE (sub_1022.id - FALSE) END))) END)), (((((CASE WHEN sub_1022.col_2 THEN (NULL IN (SELECT sub_1052.col_3 FROM tbl_02 AS sub_1052)) ELSE CAST(sub_1022.col_3 AS SIGNED) END) / NULLIF((ABS(sub_1022.col_2) AND CAST(sub_1022.col_1 AS CHAR)), 0)) <> (SELECT (SELECT inn_1054.col_4 FROM tbl_11 AS inn_1054 LIMIT 1) FROM tbl_06 AS sub_1053 LIMIT 1)) OR LENGTH((sub_1022.col_3 IS NULL))) + (NOT (CAST(-1e-5 AS DECIMAL(10,2)))))), sub_1022.id) / NULLIF((ABS((SELECT (SELECT 0.0 FROM tbl_11 AS inn_1060 LIMIT 1) FROM tbl_11 AS sub_1055 LIMIT 1)) / NULLIF(CAST((COALESCE(FALSE, ((SELECT (sub_1061.col_3 + '') FROM tbl_05 AS sub_1061 LIMIT 1) AND ((LOWER(sub_1022.id) IS NULL) OR 999999999.99))) BETWEEN 9223372036854775807 AND 2147483647) AS SIGNED), 0)), 0))) FROM tbl_10 AS sub_1022 LIMIT 1) * t_ref_974.col_4)) AS CHAR) END) THEN COALESCE((NOT (t_ref_974.id)), EXISTS (SELECT 1 FROM tbl_04 AS sub_1062 WHERE sub_1062.col_4)) ELSE t_ref_974.col_3 END) AS CHAR)) AS c_1063,
    ((LOWER((((EXISTS (SELECT 1 FROM tbl_11 AS sub_1064 WHERE 9223372036854775807) = (NULLIF(1.0, t_ref_974.col_3) + CAST(t_ref_974.col_4 AS DECIMAL(10,2)))) BETWEEN 2147483647 AND -1) + (t_ref_974.col_4 IN (SELECT sub_1073.id FROM tbl_04 AS sub_1073)))) BETWEEN 1 AND 100) < COALESCE((t_ref_974.col_4 <= (EXISTS (SELECT 1 FROM tbl_09 AS sub_1074 WHERE 100) / NULLIF((EXISTS (SELECT 1 FROM tbl_10 AS sub_1075 WHERE sub_1075.col_1) / NULLIF((NOT ((t_ref_974.ref_tbl_02_id IN (SELECT sub_1087.col_4 FROM tbl_01 AS sub_1087)))), 0)), 0))), LOWER((SELECT -1.5 FROM tbl_02 AS sub_1088 LIMIT 1)))) AS c_1089
FROM tbl_06 AS t_ref_974
WHERE (((NULL * CAST(LENGTH((NULLIF(9223372036854775807, CAST(FALSE AS SIGNED)) - CAST((42 IN (SELECT sub_1090.col_3 FROM tbl_01 AS sub_1090)) AS CHAR))) AS CHAR)) * t_ref_974.col_2) * ((SELECT (SELECT inn_1095.dyn_col_4 FROM tbl_03 AS inn_1095 LIMIT 1) FROM tbl_03 AS sub_1094 LIMIT 1) * CAST(((SELECT sub_1096.col_2 FROM tbl_04 AS sub_1096 LIMIT 1) / NULLIF((SELECT (SELECT inn_1098.col_1 FROM tbl_07 AS inn_1098 LIMIT 1) FROM tbl_02 AS sub_1097 LIMIT 1), 0)) AS DECIMAL(10,2))))
LIMIT 38;

INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (264152, 'ref_val', 'chaos', '__fuzz_marker__', 42);

CREATE VIEW v_summary_6 AS SELECT id FROM tbl_10 WHERE id IS NOT NULL;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (237359, 'ref_val', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', TRUE, '', -2147483648);

BEGIN;
INSERT INTO tbl_09 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (119973, 'escaped''quote', FALSE, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 42);
COMMIT;

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64),
    ref_tbl_12_id INT,
    FOREIGN KEY (ref_tbl_12_id) REFERENCES tbl_12(id)
);

SELECT
    (NOT (t_ref_1099.id)) AS c_1100
FROM tbl_03 AS t_ref_1099
WHERE (TRUE IN (SELECT sub_1101.id FROM tbl_01 AS sub_1101))
LIMIT 19;

SELECT
    CAST(t_ref_1105.id AS DECIMAL(10,2)) AS c_1106,
    ((CAST(t_ref_1105.col_3 AS CHAR) / NULLIF(((NOT (LENGTH(('' <> CAST(FALSE AS CHAR))))) - LENGTH(t_ref_1105.col_1)), 0)) * t_ref_1105.col_3) AS c_1107
FROM tbl_10 AS t_ref_1105
WHERE (9223372036854775807 IN (SELECT sub_1108.col_3 FROM tbl_08 AS sub_1108))
LIMIT 2;

SELECT
    FALSE AS c_1110
FROM tbl_10 AS t_ref_1109 LEFT JOIN tbl_01 AS j_ref_1111 ON t_ref_1109.id = j_ref_1111.id
WHERE ABS(t_ref_1109.col_3)
LIMIT 7;

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN
);

DELETE FROM tbl_12 WHERE id = 43346;

SELECT
    t_ref_1112.col_1 AS c_1113
FROM tbl_13 AS t_ref_1112 LEFT JOIN tbl_05 AS j_ref_1114 ON t_ref_1112.id = j_ref_1114.id
WHERE UPPER(CAST(((CAST((CASE WHEN (LOWER(NULL) * FALSE) THEN ((NULL * (SELECT (SELECT inn_1120.col_4 FROM tbl_02 AS inn_1120 LIMIT 1) FROM tbl_05 AS sub_1115 LIMIT 1)) + ((t_ref_1112.col_2 >= (CASE WHEN (((CASE WHEN UPPER(CAST(((CASE WHEN t_ref_1112.col_1 THEN (t_ref_1112.col_3 IN (SELECT sub_1121.col_2 FROM tbl_02 AS sub_1121)) ELSE ((CASE WHEN NULL THEN t_ref_1112.col_2 ELSE FALSE END) / NULLIF('__fuzz_marker__', 0)) END) - (EXISTS (SELECT 1 FROM tbl_11 AS sub_1122 WHERE sub_1122.col_3) <= ABS(t_ref_1112.col_2))) AS CHAR)) THEN LOWER((((NOT ((ABS(NULL) IS NULL))) < (((1e10 * t_ref_1112.col_1) * CAST(t_ref_1112.col_2 AS DECIMAL(10,2))) <> (ABS(999999999.99) IS NOT NULL))) AND CAST(t_ref_1112.col_2 AS SIGNED))) ELSE t_ref_1112.col_1 END) = t_ref_1112.id) IS NOT NULL) THEN COALESCE((SELECT (SELECT inn_1124.dyn_col_5 FROM tbl_03 AS inn_1124 LIMIT 1) FROM tbl_04 AS sub_1123 LIMIT 1), t_ref_1112.col_1) ELSE (ABS(LENGTH((t_ref_1112.col_3 IN (SELECT sub_1125.col_2 FROM tbl_06 AS sub_1125)))) <> (t_ref_1112.col_1 IN (SELECT sub_1126.col_2 FROM tbl_15 AS sub_1126))) END)) OR (NOT (LENGTH((SELECT NULL FROM tbl_01 AS sub_1128 LIMIT 1)))))) ELSE 0.0 END) AS CHAR) + CAST(t_ref_1112.col_2 AS CHAR)) AND CAST((SELECT ((sub_1129.col_1 / NULLIF((CASE WHEN CAST(LOWER(((sub_1129.col_1 / NULLIF(CAST((((NOT (TRUE)) > (CAST(sub_1129.ref_tbl_05_id AS DECIMAL(10,2)) > COALESCE((sub_1129.ref_tbl_05_id - TRUE), (SELECT (SELECT inn_1131.col_1 FROM tbl_12 AS inn_1131 LIMIT 1) FROM customers_large AS sub_1130 LIMIT 1)))) >= FALSE) AS DECIMAL(10,2)), 0)) > (SELECT (SELECT NULL FROM tbl_01 AS inn_1136 LIMIT 1) FROM tbl_04 AS sub_1132 LIMIT 1))) AS DECIMAL(10,2)) THEN (SELECT (SELECT inn_1141.col_4 FROM tbl_04 AS inn_1141 LIMIT 1) FROM tbl_11 AS sub_1137 LIMIT 1) ELSE 0.0000000001 END), 0)) / NULLIF(((sub_1129.ref_tbl_05_id IN (SELECT sub_1142.ref_tbl_02_id FROM tbl_06 AS sub_1142)) * (NOT (((SELECT (SELECT inn_1149.col_1 FROM tbl_07 AS inn_1149 LIMIT 1) FROM tbl_12 AS sub_1145 LIMIT 1) = ((SELECT (SELECT inn_1155.col_3 FROM tbl_08 AS inn_1155 LIMIT 1) FROM tbl_07 AS sub_1150 LIMIT 1) IS NOT NULL))))), 0)) FROM tbl_07 AS sub_1129 LIMIT 1) AS CHAR)) AS SIGNED))
LIMIT 9;

UPDATE tbl_01 SET col_2 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 50;

INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (844990, 3.14159265, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -1.5);

ALTER TABLE tbl_14 ADD COLUMN dyn_col_6 VARCHAR(32);

INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (307596, 'ref_val', 'alpha_beta_gamma', 'alpha_beta_gamma', -2147483648);

ALTER TABLE tbl_07 ADD COLUMN dyn_col_5 VARCHAR(32);

SELECT
    ((SELECT (SELECT NULL FROM tbl_07 AS inn_1161 LIMIT 1) FROM tbl_05 AS sub_1157 LIMIT 1) + (CASE WHEN (NOT (((CAST((SELECT (SELECT inn_1170.id FROM tbl_15 AS inn_1170 LIMIT 1) FROM tbl_13 AS sub_1162 LIMIT 1) AS SIGNED) * (((NOT ((t_ref_1156.ref_tbl_02_id * ((FALSE IN (SELECT sub_1171.col_1 FROM tbl_03 AS sub_1171)) <> CAST(t_ref_1156.id AS DECIMAL(10,2)))))) BETWEEN 9223372036854775807 AND 100) + t_ref_1156.id)) > ((TRUE * (t_ref_1156.col_2 IN (SELECT sub_1172.col_2 FROM tbl_05 AS sub_1172))) > ((t_ref_1156.col_1 > (LOWER(((((SELECT ((sub_1174.id <= (NULL IN (SELECT sub_1175.col_1 FROM tbl_06 AS sub_1175))) AND ((sub_1174.col_2 + (sub_1174.col_2 IN (SELECT sub_1176.col_3 FROM tbl_02 AS sub_1176))) + ((sub_1174.col_1 / NULLIF((NULL + sub_1174.id), 0)) / NULLIF(sub_1174.id, 0)))) FROM tbl_12 AS sub_1174 LIMIT 1) - ((CAST(((t_ref_1156.col_4 BETWEEN -1 AND -1) * ((NOT (TRUE)) > t_ref_1156.col_1)) AS CHAR) - LOWER((999999999.99 = ((t_ref_1156.col_4 <= (TRUE - t_ref_1156.id)) OR CAST((NULL * t_ref_1156.ref_tbl_02_id) AS DECIMAL(10,2)))))) + (CASE WHEN EXISTS (SELECT 1 FROM tbl_12 AS sub_1177 WHERE sub_1177.col_2) THEN ((SELECT (SELECT inn_1179.col_2 FROM tbl_05 AS inn_1179 LIMIT 1) FROM tbl_04 AS sub_1178 LIMIT 1) > LENGTH(((LOWER(t_ref_1156.id) < ('escaped''quote' >= 3.14159265)) * NULLIF((t_ref_1156.ref_tbl_02_id = t_ref_1156.col_1), (NOT (t_ref_1156.col_4)))))) ELSE CAST((NOT ((CASE WHEN (SELECT sub_1180.col_2 FROM tbl_15 AS sub_1180 LIMIT 1) THEN TRUE ELSE (SELECT (SELECT inn_1182.ref_tbl_01_id FROM tbl_02 AS inn_1182 LIMIT 1) FROM tbl_08 AS sub_1181 LIMIT 1) END))) AS DECIMAL(10,2)) END))) + ((CAST(t_ref_1156.ref_tbl_02_id AS SIGNED) * (SELECT EXISTS (SELECT 1 FROM tbl_12 AS sub_1184 WHERE sub_1184.id) FROM tbl_06 AS sub_1183 LIMIT 1)) * CAST((NULLIF(t_ref_1156.col_2, t_ref_1156.col_3) OR (SELECT UPPER((FALSE > NULL)) FROM tbl_03 AS sub_1185 LIMIT 1)) AS CHAR))) = CAST('chaos' AS CHAR))) AND ((t_ref_1156.col_2 + (t_ref_1156.col_3 IN (SELECT sub_1186.col_4 FROM tbl_05 AS sub_1186))) <= ((t_ref_1156.col_3 / NULLIF((LOWER(CAST(t_ref_1156.ref_tbl_02_id AS SIGNED)) IS NULL), 0)) IS NOT NULL)))) <= (t_ref_1156.ref_tbl_02_id IN (SELECT sub_1187.id FROM tbl_08 AS sub_1187))))))) THEN FALSE ELSE 'ref_val' END)) AS c_1197
FROM tbl_06 AS t_ref_1156 LEFT JOIN tbl_04 AS j_ref_1198 ON t_ref_1156.id = j_ref_1198.id
WHERE (CASE WHEN (t_ref_1156.col_1 - t_ref_1156.col_1) THEN t_ref_1156.col_4 ELSE -1.5 END)
LIMIT 32;

DELETE FROM customers_large WHERE id = 15708;

SELECT
    (NOT ((((t_ref_1199.col_2 OR (CASE WHEN CAST(((NOT ((NOT (t_ref_1199.id)))) = 'alpha_beta_gamma') AS CHAR) THEN (CASE WHEN (t_ref_1199.id IN (SELECT sub_1200.col_2 FROM tbl_03 AS sub_1200)) THEN EXISTS (SELECT 1 FROM tbl_07 AS sub_1201 WHERE FALSE) ELSE CAST(EXISTS (SELECT 1 FROM tbl_09 AS sub_1223 WHERE sub_1223.col_3) AS SIGNED) END) ELSE (CASE WHEN (t_ref_1199.col_1 IN (SELECT sub_1224.id FROM tbl_15 AS sub_1224)) THEN (COALESCE((t_ref_1199.col_1 > (FALSE + NULLIF(LENGTH((((ABS(t_ref_1199.col_2) = (EXISTS (SELECT 1 FROM tbl_13 AS sub_1232 WHERE sub_1232.col_3) - (FALSE > ((SELECT (SELECT inn_1234.col_2 FROM tbl_14 AS inn_1234 LIMIT 1) FROM tbl_07 AS sub_1233 LIMIT 1) / NULLIF((t_ref_1199.col_1 + -1.5), 0))))) >= NULLIF(NULL, t_ref_1199.col_1)) * ((CASE WHEN (t_ref_1199.col_2 IN (SELECT sub_1235.col_1 FROM tbl_09 AS sub_1235)) THEN (CAST((t_ref_1199.id IN (SELECT sub_1236.id FROM tbl_09 AS sub_1236)) AS DECIMAL(10,2)) <> ((CASE WHEN t_ref_1199.col_2 THEN t_ref_1199.col_1 ELSE 'escaped''quote' END) * (SELECT (SELECT inn_1238.id FROM tbl_11 AS inn_1238 LIMIT 1) FROM tbl_01 AS sub_1237 LIMIT 1))) ELSE (COALESCE(NULLIF((1.0 = t_ref_1199.col_1), LENGTH(t_ref_1199.col_2)), t_ref_1199.col_2) / NULLIF(t_ref_1199.col_2, 0)) END) <> CAST((((CAST(t_ref_1199.col_2 AS SIGNED) AND LOWER('alpha_beta_gamma')) + ((t_ref_1199.col_2 IN (SELECT sub_1239.col_3 FROM tbl_09 AS sub_1239)) - LOWER(t_ref_1199.id))) <= EXISTS (SELECT 1 FROM customers_large AS sub_1240 WHERE sub_1240.payload)) AS CHAR)))), t_ref_1199.col_2))), (NOT (ABS(t_ref_1199.id)))) BETWEEN 100 AND 9223372036854775807) ELSE t_ref_1199.col_2 END) END)) <> ((SELECT COALESCE((((ABS((SELECT (SELECT inn_1245.col_3 FROM tbl_05 AS inn_1245 LIMIT 1) FROM customers_large AS sub_1242 LIMIT 1)) <= CAST(((sub_1241.col_3 - LOWER(sub_1241.id)) * ((SELECT (((NOT (EXISTS (SELECT 1 FROM tbl_12 AS sub_1247 WHERE sub_1247.id))) IS NOT NULL) OR FALSE) FROM tbl_06 AS sub_1246 LIMIT 1) / NULLIF((SELECT (SELECT inn_1249.payload FROM customers_large AS inn_1249 LIMIT 1) FROM tbl_09 AS sub_1248 LIMIT 1), 0))) AS CHAR)) - sub_1241.col_3) / NULLIF(CAST((((sub_1241.id IN (SELECT sub_1250.col_2 FROM tbl_13 AS sub_1250)) AND (sub_1241.col_3 - EXISTS (SELECT 1 FROM tbl_04 AS sub_1253 WHERE sub_1253.col_4))) > NULLIF(((sub_1241.id BETWEEN 100 AND -9223372036854775808) - (NULLIF(sub_1241.col_1, ((LOWER((SELECT (SELECT 'chaos' FROM tbl_09 AS inn_1255 LIMIT 1) FROM tbl_01 AS sub_1254 LIMIT 1)) + (sub_1241.col_1 / NULLIF((NOT (CAST(sub_1241.col_1 AS CHAR))), 0))) < NULLIF(EXISTS (SELECT 1 FROM tbl_08 AS sub_1256 WHERE sub_1256.col_1), (CASE WHEN ((-9223372036854775808 < 1) <= 'escaped''quote') THEN CAST(('chaos' / NULLIF(sub_1241.col_1, 0)) AS CHAR) ELSE sub_1241.col_1 END)))) / NULLIF((sub_1241.col_1 / NULLIF(sub_1241.col_3, 0)), 0))), ((sub_1241.col_3 BETWEEN 100 AND -1) + (2147483647 >= sub_1241.col_2)))) AS SIGNED), 0)), (NULLIF(sub_1241.id, sub_1241.col_2) AND (ABS(EXISTS (SELECT 1 FROM tbl_09 AS sub_1257 WHERE sub_1257.id)) - sub_1241.col_2))) FROM tbl_10 AS sub_1241 LIMIT 1) = ((SELECT (SELECT inn_1259.col_1 FROM tbl_12 AS inn_1259 LIMIT 1) FROM tbl_13 AS sub_1258 LIMIT 1) <= (t_ref_1199.id IN (SELECT sub_1260.id FROM tbl_14 AS sub_1260))))) BETWEEN 2147483647 AND 42))) AS c_1270
FROM tbl_12 AS t_ref_1199
WHERE COALESCE(t_ref_1199.id, ((SELECT (SELECT FALSE FROM tbl_14 AS inn_1294 LIMIT 1) FROM customers_large AS sub_1271 LIMIT 1) BETWEEN 9223372036854775807 AND -9223372036854775808))
LIMIT 7;

INSERT INTO tbl_12 (id, col_1, col_2) VALUES (794283, '[]', 3.14159265);

SELECT
    ((((SELECT sub_1296.col_2 FROM tbl_12 AS sub_1296 LIMIT 1) + CAST((SELECT (SELECT inn_1298.col_2 FROM tbl_10 AS inn_1298 LIMIT 1) FROM tbl_09 AS sub_1297 LIMIT 1) AS SIGNED)) + (t_ref_1295.col_2 + (LENGTH((SELECT CAST(ABS((((LOWER(((SELECT (SELECT FALSE FROM tbl_11 AS sub_1301 LIMIT 1) FROM customers_large AS sub_1300 LIMIT 1) = ABS((EXISTS (SELECT 1 FROM tbl_02 AS sub_1302 WHERE -1e-5) - ((SELECT NULL FROM tbl_03 AS sub_1303 LIMIT 1) * (sub_1299.id + sub_1299.id)))))) - TRUE) BETWEEN 42 AND 1) AND COALESCE(CAST((NOT ((sub_1299.col_3 * (CAST((sub_1299.col_3 > (CASE WHEN UPPER(sub_1299.col_3) THEN (sub_1299.col_2 * sub_1299.ref_tbl_01_id) ELSE (SELECT (SELECT NULL FROM tbl_05 AS inn_1305 LIMIT 1) FROM tbl_10 AS sub_1304 LIMIT 1) END)) AS CHAR) / NULLIF(CAST(((sub_1299.id IN (SELECT sub_1306.col_1 FROM tbl_10 AS sub_1306)) < (CASE WHEN CAST('' AS DECIMAL(10,2)) THEN (sub_1299.ref_tbl_01_id > TRUE) ELSE (NULL AND sub_1299.col_1) END)) AS DECIMAL(10,2)), 0))))) AS CHAR), (((3.14159265 IN (SELECT sub_1307.id FROM customers_large AS sub_1307)) + ((((LENGTH(LENGTH(sub_1299.col_2)) / NULLIF(sub_1299.ref_tbl_01_id, 0)) - (TRUE IN (SELECT sub_1310.dyn_col_4 FROM tbl_03 AS sub_1310))) <= (((LENGTH(sub_1299.ref_tbl_01_id) + (NULL - sub_1299.id)) * CAST((NULL AND 0.0) AS SIGNED)) < EXISTS (SELECT 1 FROM tbl_08 AS sub_1311 WHERE sub_1311.col_2))) <> (SELECT (SELECT inn_1314.col_1 FROM tbl_06 AS inn_1314 LIMIT 1) FROM tbl_15 AS sub_1312 LIMIT 1))) - ((999999999.99 IN (SELECT sub_1315.ref_tbl_01_id FROM tbl_09 AS sub_1315)) BETWEEN 42 AND 100))))) AS SIGNED) FROM tbl_09 AS sub_1299 LIMIT 1)) BETWEEN 0 AND -1))) + (((t_ref_1295.col_2 * CAST(t_ref_1295.col_1 AS SIGNED)) - ((((SELECT (CASE WHEN (FALSE = (CAST(CAST((sub_1316.col_2 <> FALSE) AS SIGNED) AS DECIMAL(10,2)) AND NULL)) THEN (CASE WHEN sub_1316.id THEN (UPPER((sub_1316.col_4 IN (SELECT sub_1317.id FROM tbl_15 AS sub_1317))) * ((SELECT (((2147483647 <> ABS((-9223372036854775808 = sub_1319.col_1))) - EXISTS (SELECT 1 FROM tbl_08 AS sub_1320 WHERE sub_1320.col_4)) / NULLIF(((((sub_1319.col_3 - sub_1319.col_1) * sub_1319.col_1) OR sub_1319.id) <> '__fuzz_marker__'), 0)) FROM tbl_10 AS sub_1319 LIMIT 1) - EXISTS (SELECT 1 FROM tbl_14 AS sub_1321 WHERE sub_1321.col_3))) ELSE ((NOT (((((0 IN (SELECT sub_1324.id FROM tbl_12 AS sub_1324)) * sub_1316.col_2) >= CAST('alpha_beta_gamma' AS SIGNED)) * (sub_1316.col_4 BETWEEN 0 AND -1)))) BETWEEN 42 AND 1) END) ELSE ((LENGTH((((NULL <> (('alpha_beta_gamma' IS NOT NULL) * (sub_1316.col_2 IN (SELECT sub_1325.ref_tbl_01_id FROM tbl_02 AS sub_1325)))) < (sub_1316.id / NULLIF(sub_1316.id, 0))) / NULLIF((TRUE - (SELECT ((CAST(sub_1327.col_1 AS CHAR) <> TRUE) - ABS((FALSE + sub_1327.col_2))) FROM tbl_12 AS sub_1327 LIMIT 1)), 0))) / NULLIF(LOWER((FALSE + ((CASE WHEN ((((SELECT sub_1328.col_4 FROM tbl_05 AS sub_1328 LIMIT 1) OR (CASE WHEN sub_1316.col_2 THEN 1 ELSE sub_1316.id END)) + (NOT (CAST(0.0000000001 AS SIGNED)))) * NULLIF((ABS(sub_1316.col_4) / NULLIF(sub_1316.col_4, 0)), (-1.5 IN (SELECT sub_1329.col_4 FROM tbl_11 AS sub_1329)))) THEN sub_1316.col_4 ELSE (sub_1316.col_4 / NULLIF(sub_1316.id, 0)) END) / NULLIF(2147483647, 0)))), 0)) - sub_1316.col_2) END) FROM tbl_04 AS sub_1316 LIMIT 1) * t_ref_1295.col_2) = (SELECT NULLIF((0.0 IN (SELECT sub_1331.id FROM tbl_04 AS sub_1331)), (sub_1330.dyn_col_4 = CAST(((CASE WHEN NULLIF(sub_1330.dyn_col_5, (SELECT sub_1336.col_1 FROM tbl_03 AS sub_1336 LIMIT 1)) THEN (sub_1330.dyn_col_5 <= sub_1330.dyn_col_5) ELSE sub_1330.col_1 END) <= (sub_1330.id + sub_1330.col_2)) AS DECIMAL(10,2)))) FROM tbl_03 AS sub_1330 LIMIT 1)) = t_ref_1295.col_2)) > (((((SELECT (CAST(EXISTS (SELECT 1 FROM tbl_10 AS sub_1338 WHERE sub_1338.id) AS SIGNED) >= sub_1337.id) FROM tbl_01 AS sub_1337 LIMIT 1) >= ('__fuzz_marker__' > t_ref_1295.col_1)) - (LENGTH(CAST((1.0 * (CAST((t_ref_1295.col_1 IN (SELECT sub_1339.col_2 FROM tbl_05 AS sub_1339)) AS SIGNED) >= COALESCE(NULLIF(((SELECT (SELECT NULL FROM tbl_14 AS inn_1347 LIMIT 1) FROM tbl_03 AS sub_1340 LIMIT 1) * COALESCE(((SELECT (SELECT inn_1349.dyn_col_5 FROM tbl_07 AS inn_1349 LIMIT 1) FROM tbl_08 AS sub_1348 LIMIT 1) < t_ref_1295.id), ((t_ref_1295.col_1 IN (SELECT sub_1350.col_1 FROM tbl_12 AS sub_1350)) = (t_ref_1295.id > (NULLIF((FALSE <= t_ref_1295.id), (TRUE + t_ref_1295.col_2)) * LOWER((SELECT sub_1352.id FROM tbl_04 AS sub_1352 LIMIT 1))))))), NULLIF((SELECT (SELECT inn_1354.col_1 FROM tbl_12 AS inn_1354 LIMIT 1) FROM tbl_01 AS sub_1353 LIMIT 1), t_ref_1295.id)), FALSE))) AS SIGNED)) * (SELECT sub_1355.id FROM tbl_15 AS sub_1355 LIMIT 1))) AND CAST((NULL IN (SELECT sub_1356.col_1 FROM tbl_09 AS sub_1356)) AS CHAR)) / NULLIF(EXISTS (SELECT 1 FROM tbl_01 AS sub_1357 WHERE sub_1357.id), 0)))) AS c_1367,
    (LOWER(CAST(EXISTS (SELECT 1 FROM tbl_10 AS sub_1368 WHERE 'ref_val') AS DECIMAL(10,2))) - 0.0000000001) AS c_1369
FROM tbl_15 AS t_ref_1295
WHERE CAST(LENGTH(CAST(((NULLIF((SELECT (SELECT inn_1371.col_2 FROM tbl_01 AS inn_1371 LIMIT 1) FROM tbl_13 AS sub_1370 LIMIT 1), TRUE) OR LENGTH(((t_ref_1295.col_1 / NULLIF(CAST(t_ref_1295.id AS CHAR), 0)) / NULLIF(((SELECT (SELECT -1e-5 FROM tbl_03 AS inn_1376 LIMIT 1) FROM tbl_05 AS sub_1372 LIMIT 1) * (EXISTS (SELECT 1 FROM tbl_01 AS sub_1377 WHERE -1e-5) IS NULL)), 0)))) / NULLIF(TRUE, 0)) AS CHAR)) AS SIGNED)
LIMIT 9;

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id) VALUES (565887, 'alpha_beta_gamma', '{}', '', -9223372036854775808, -9223372036854775808);
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id) VALUES (480115, 'chaos', '[]', '', 0, 100);
INSERT INTO tbl_13 (id, col_1, col_2, col_3) VALUES (925988, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -1.5);
COMMIT;

INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (889386, -1.5, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 3.14159265);

INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (286148, 0.0000000001, '{}', 3.14159265);

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (368726, '', '__fuzz_marker__', 'alpha_beta_gamma', 42);
COMMIT;

CREATE INDEX idx_tbl_12_col_2_14 ON tbl_12 (col_2);

ALTER TABLE tbl_04 ADD COLUMN dyn_col_5 VARCHAR(32);

SELECT
    (SELECT (CAST(((CASE WHEN ((((sub_1385.id - UPPER(FALSE)) + TRUE) > (CASE WHEN sub_1385.col_1 THEN (sub_1385.id <> sub_1385.col_2) ELSE (CAST(CAST((SELECT (SELECT inn_1388.col_2 FROM tbl_10 AS inn_1388 LIMIT 1) FROM tbl_15 AS sub_1386 LIMIT 1) AS SIGNED) AS CHAR) - (SELECT (SELECT inn_1393.ref_tbl_02_id FROM tbl_06 AS inn_1393 LIMIT 1) FROM tbl_05 AS sub_1389 LIMIT 1)) END)) > (SELECT (sub_1394.col_1 + CAST(CAST((sub_1394.col_2 > ABS(sub_1394.col_1)) AS CHAR) AS SIGNED)) FROM tbl_15 AS sub_1394 LIMIT 1)) THEN sub_1385.col_3 ELSE sub_1385.col_3 END) - FALSE) AS SIGNED) < ((SELECT ((ABS((SELECT (SELECT (SELECT 'alpha_beta_gamma' FROM tbl_15 AS inn_1398 LIMIT 1) FROM tbl_07 AS sub_1397 LIMIT 1) FROM tbl_05 AS sub_1396 LIMIT 1)) >= UPPER(sub_1395.id)) * (SELECT (SELECT TRUE FROM tbl_08 AS inn_1407 LIMIT 1) FROM tbl_02 AS sub_1399 LIMIT 1)) FROM customers_large AS sub_1395 LIMIT 1) + (LENGTH((UPPER(CAST('ref_val' AS DECIMAL(10,2))) - sub_1385.col_1)) * ((LENGTH((NULL IN (SELECT sub_1408.col_3 FROM tbl_14 AS sub_1408))) - sub_1385.col_1) / NULLIF(-1.5, 0))))) FROM tbl_10 AS sub_1385 LIMIT 1) AS c_1411
FROM tbl_15 AS t_ref_1384 LEFT JOIN tbl_03 AS j_ref_1412 ON t_ref_1384.id = j_ref_1412.id
WHERE t_ref_1384.id
LIMIT 7;

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id) VALUES (782492, '__fuzz_marker__', '{}', '__fuzz_marker__', -1, -1);
INSERT INTO tbl_14 (id, col_1, col_2, col_3, ref_tbl_12_id, dyn_col_6) VALUES (513668, TRUE, 3.14159265, 'alpha_beta_gamma', -1, 'chaos');
COMMIT;

DELETE FROM tbl_01 WHERE id = 99123;

INSERT INTO customers_large (id, payload) VALUES (235493, 'escaped''quote');

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id) VALUES (289639, 'escaped''quote', '[]', 'alpha_beta_gamma', 1, -9223372036854775808);
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (64681, '[]', '{}', -9223372036854775808, 0.0);
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (157733, '{}', -1, TRUE, -2147483648, -9223372036854775808);
COMMIT;

UPDATE tbl_05 SET col_2 = 'ref_val' WHERE id > 55;

CREATE INDEX idx_tbl_04_col_2_15 ON tbl_04 (col_2);

CREATE TABLE customers_large (
    cust_id INT PRIMARY KEY,
    company_name VARCHAR(255) NOT NULL,
    profile_data JSON
);

SELECT
    c.cust_id,
    c.company_name,
    JSON_EXTRACT(c.profile_data, '$.tier') AS cust_tier,
    o.ord_id,
    o.order_val,
    LEFT(o.notes, 50) AS snippet
FROM customers_large c
JOIN orders_large o ON c.cust_id = o.cust_id
WHERE o.order_val > 1000.00
ORDER BY o.order_val DESC;

UPDATE tbl_10 SET col_3 = 1.0 WHERE id > 78;

SELECT
    ((t_ref_1413.col_2 BETWEEN -2147483648 AND 42) - (CASE WHEN ('alpha_beta_gamma' <> NULLIF(((t_ref_1413.col_1 * (((SELECT (((SELECT (SELECT 'ref_val' FROM tbl_04 AS inn_1417 LIMIT 1) FROM tbl_11 AS sub_1415 LIMIT 1) + (sub_1414.ref_tbl_01_id * sub_1414.ref_tbl_01_id)) <= (0 = (sub_1414.ref_tbl_01_id BETWEEN -9223372036854775808 AND 42))) FROM tbl_02 AS sub_1414 LIMIT 1) <= (NOT (t_ref_1413.col_3))) - LENGTH((t_ref_1413.col_2 > LOWER(LOWER(NULLIF(NULL, ((CAST(t_ref_1413.col_3 AS DECIMAL(10,2)) > (((SELECT (SELECT 3.14159265 FROM tbl_13 AS inn_1419 LIMIT 1) FROM tbl_02 AS sub_1418 LIMIT 1) >= (((t_ref_1413.col_3 - t_ref_1413.col_2) - t_ref_1413.col_1) * LENGTH((t_ref_1413.col_3 < t_ref_1413.id)))) = t_ref_1413.col_2)) * ((1 * t_ref_1413.col_1) < NULLIF(UPPER(((COALESCE(t_ref_1413.col_3, 999999999.99) * CAST(TRUE AS DECIMAL(10,2))) + (LENGTH(1.0) BETWEEN 42 AND 100))), (SELECT (-2147483648 BETWEEN -1 AND 2147483647) FROM tbl_08 AS sub_1420 LIMIT 1))))))))))) - ((t_ref_1413.id - ABS((COALESCE(t_ref_1413.col_3, t_ref_1413.col_3) AND (EXISTS (SELECT 1 FROM tbl_15 AS sub_1421 WHERE sub_1421.col_2) / NULLIF(((t_ref_1413.col_2 - ((t_ref_1413.col_1 <= -9223372036854775808) BETWEEN 1 AND 42)) < (t_ref_1413.id > ((t_ref_1413.col_3 IN (SELECT sub_1424.id FROM tbl_15 AS sub_1424)) BETWEEN -9223372036854775808 AND 100))), 0))))) - (((LOWER(((1e10 IN (SELECT sub_1425.payload FROM customers_large AS sub_1425)) IS NOT NULL)) BETWEEN -2147483648 AND -9223372036854775808) BETWEEN 1 AND -2147483648) * EXISTS (SELECT 1 FROM tbl_03 AS sub_1426 WHERE sub_1426.col_2)))), ABS((TRUE < NULL)))) THEN UPPER(((SELECT (SELECT (SELECT FALSE FROM tbl_14 AS inn_1434 LIMIT 1) FROM tbl_04 AS sub_1433 LIMIT 1) FROM tbl_02 AS sub_1432 LIMIT 1) - 1)) ELSE t_ref_1413.col_1 END)) AS c_1435
FROM tbl_13 AS t_ref_1413
WHERE -9223372036854775808
LIMIT 28;

SELECT
    (t_ref_1436.col_1 > NULL) AS c_1437
FROM tbl_05 AS t_ref_1436 LEFT JOIN tbl_08 AS j_ref_1438 ON t_ref_1436.id = j_ref_1438.id
WHERE t_ref_1436.col_2
LIMIT 13;

SELECT
    ((CASE WHEN t_ref_1439.id THEN ((SELECT sub_1440.col_1 FROM tbl_14 AS sub_1440 LIMIT 1) OR (t_ref_1439.id >= EXISTS (SELECT 1 FROM tbl_08 AS sub_1441 WHERE 1.0))) ELSE EXISTS (SELECT 1 FROM tbl_05 AS sub_1451 WHERE sub_1451.col_3) END) - t_ref_1439.payload) AS c_1453,
    'alpha_beta_gamma' AS c_1454
FROM customers_large AS t_ref_1439
WHERE TRUE
LIMIT 45;

CREATE INDEX idx_tbl_10_col_1_16 ON tbl_10 (col_1);

SELECT
    (((ABS(TRUE) OR CAST(ABS((999999999.99 IN (SELECT sub_1456.col_2 FROM tbl_02 AS sub_1456))) AS SIGNED)) IS NULL) >= (LOWER((SELECT ((CASE WHEN sub_1457.col_3 THEN (CASE WHEN (SELECT (CASE WHEN (ABS(sub_1458.dyn_col_5) >= (NULLIF(sub_1458.col_4, (SELECT COALESCE((sub_1459.col_1 OR CAST(sub_1459.col_2 AS SIGNED)), sub_1459.col_3) FROM tbl_06 AS sub_1459 LIMIT 1)) - LOWER(CAST((sub_1458.id - UPPER((sub_1458.col_1 IN (SELECT sub_1460.col_1 FROM tbl_04 AS sub_1460)))) AS SIGNED)))) THEN (CAST(NULL AS CHAR) / NULLIF(sub_1458.dyn_col_5, 0)) ELSE ((SELECT (SELECT inn_1462.dyn_col_5 FROM tbl_04 AS inn_1462 LIMIT 1) FROM tbl_04 AS sub_1461 LIMIT 1) - 'alpha_beta_gamma') END) FROM tbl_04 AS sub_1458 LIMIT 1) THEN (EXISTS (SELECT 1 FROM tbl_03 AS sub_1463 WHERE sub_1463.dyn_col_5) / NULLIF((((sub_1457.col_3 IN (SELECT sub_1468.col_1 FROM tbl_11 AS sub_1468)) / NULLIF((LOWER(COALESCE((FALSE IN (SELECT sub_1471.col_3 FROM tbl_02 AS sub_1471)), 'chaos')) / NULLIF(EXISTS (SELECT 1 FROM tbl_05 AS sub_1472 WHERE NULL), 0)), 0)) < ((-2147483648 IN (SELECT sub_1473.col_3 FROM tbl_13 AS sub_1473)) / NULLIF((SELECT (SELECT inn_1485.col_2 FROM tbl_12 AS inn_1485 LIMIT 1) FROM tbl_07 AS sub_1474 LIMIT 1), 0))), 0)) ELSE (((((sub_1457.ref_tbl_02_id <> (NULL IN (SELECT sub_1486.col_2 FROM tbl_13 AS sub_1486))) <= (CASE WHEN FALSE THEN sub_1457.col_4 ELSE COALESCE(sub_1457.ref_tbl_02_id, ('escaped''quote' IN (SELECT sub_1488.dyn_col_4 FROM tbl_03 AS sub_1488))) END)) >= (sub_1457.id / NULLIF((SELECT CAST(EXISTS (SELECT 1 FROM tbl_14 AS sub_1490 WHERE sub_1490.dyn_col_6) AS DECIMAL(10,2)) FROM tbl_11 AS sub_1489 LIMIT 1), 0))) < (sub_1457.id BETWEEN -1 AND -9223372036854775808)) >= ((TRUE IN (SELECT sub_1491.ref_tbl_02_id FROM tbl_06 AS sub_1491)) + (SELECT (SELECT inn_1494.col_2 FROM tbl_12 AS inn_1494 LIMIT 1) FROM tbl_09 AS sub_1492 LIMIT 1))) END) ELSE CAST(NULL AS DECIMAL(10,2)) END) OR CAST((EXISTS (SELECT 1 FROM tbl_09 AS sub_1495 WHERE sub_1495.col_2) * (((sub_1457.col_4 <= (CASE WHEN ((CASE WHEN (FALSE IS NULL) THEN sub_1457.col_3 ELSE (SELECT CAST(CAST(COALESCE(sub_1505.col_2, 0.0000000001) AS DECIMAL(10,2)) AS CHAR) FROM tbl_07 AS sub_1505 LIMIT 1) END) / NULLIF((((CAST(((sub_1457.col_1 > TRUE) > sub_1457.col_4) AS CHAR) <> ((sub_1457.col_2 * NULLIF(sub_1457.col_1, TRUE)) + (CAST(sub_1457.col_4 AS DECIMAL(10,2)) - NULLIF(sub_1457.ref_tbl_02_id, sub_1457.ref_tbl_02_id)))) AND '__fuzz_marker__') <> 'escaped''quote'), 0)) THEN COALESCE(CAST((CASE WHEN sub_1457.id THEN ((SELECT (SELECT NULL FROM tbl_08 AS inn_1507 LIMIT 1) FROM tbl_09 AS sub_1506 LIMIT 1) + (SELECT (SELECT TRUE FROM tbl_03 AS inn_1509 LIMIT 1) FROM tbl_05 AS sub_1508 LIMIT 1)) ELSE CAST((sub_1457.col_2 IS NOT NULL) AS CHAR) END) AS DECIMAL(10,2)), sub_1457.id) ELSE (((CASE WHEN LOWER(CAST(sub_1457.id AS DECIMAL(10,2))) THEN UPPER((sub_1457.col_2 * (SELECT 'escaped''quote' FROM tbl_01 AS sub_1510 LIMIT 1))) ELSE ((LOWER(sub_1457.ref_tbl_02_id) - sub_1457.ref_tbl_02_id) / NULLIF((sub_1457.col_4 IN (SELECT sub_1511.col_1 FROM tbl_15 AS sub_1511)), 0)) END) IS NOT NULL) > FALSE) END)) = CAST((((UPPER(EXISTS (SELECT 1 FROM tbl_02 AS sub_1513 WHERE sub_1513.col_3)) * sub_1457.col_4) - sub_1457.id) / NULLIF(sub_1457.col_3, 0)) AS CHAR)) + (CASE WHEN (SELECT (sub_1514.col_1 IN (SELECT sub_1515.col_4 FROM tbl_02 AS sub_1515)) FROM tbl_09 AS sub_1514 LIMIT 1) THEN (SELECT (SELECT NULL FROM tbl_06 AS inn_1519 LIMIT 1) FROM customers_large AS sub_1516 LIMIT 1) ELSE sub_1457.id END))) AS DECIMAL(10,2))) FROM tbl_06 AS sub_1457 LIMIT 1)) > (ABS((SELECT (SELECT FALSE FROM tbl_03 AS inn_1528 LIMIT 1) FROM tbl_01 AS sub_1520 LIMIT 1)) * ABS(NULL)))) AS c_1529,
    t_ref_1455.col_1 AS c_1530
FROM tbl_07 AS t_ref_1455 LEFT JOIN tbl_01 AS j_ref_1531 ON t_ref_1455.id = j_ref_1531.id
WHERE t_ref_1455.id
LIMIT 28;

SELECT
    (999999999.99 IS NULL) AS c_1533,
    ((SELECT COALESCE((SELECT 0 FROM tbl_03 AS sub_1535 LIMIT 1), EXISTS (SELECT 1 FROM tbl_15 AS sub_1536 WHERE sub_1536.id)) FROM tbl_12 AS sub_1534 LIMIT 1) * t_ref_1532.col_4) AS c_1537,
    ((EXISTS (SELECT 1 FROM tbl_06 AS sub_1538 WHERE TRUE) / NULLIF(9223372036854775807, 0)) OR CAST(COALESCE((SELECT CAST((((SELECT (SELECT inn_1548.id FROM tbl_06 AS inn_1548 LIMIT 1) FROM tbl_03 AS sub_1547 LIMIT 1) > ((CAST(sub_1546.id AS CHAR) - 'escaped''quote') IS NOT NULL)) > TRUE) AS CHAR) FROM tbl_06 AS sub_1546 LIMIT 1), ((SELECT (SELECT inn_1559.col_2 FROM tbl_06 AS inn_1559 LIMIT 1) FROM tbl_09 AS sub_1549 LIMIT 1) <= (SELECT (SELECT inn_1563.id FROM tbl_15 AS inn_1563 LIMIT 1) FROM tbl_12 AS sub_1560 LIMIT 1))) AS SIGNED)) AS c_1564
FROM tbl_04 AS t_ref_1532
WHERE (SELECT ((ABS(100) <= (SELECT (SELECT inn_1578.col_1 FROM tbl_05 AS inn_1578 LIMIT 1) FROM tbl_09 AS sub_1566 LIMIT 1)) <= sub_1565.id) FROM tbl_12 AS sub_1565 LIMIT 1)
LIMIT 3;

UPDATE tbl_04 SET col_1 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 90;

ALTER TABLE tbl_06 DROP COLUMN col_4;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (888972, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 0, 3.14159265);

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON
);

BEGIN;
INSERT INTO tbl_15 (id, col_1, col_2) VALUES (834649, 'alpha_beta_gamma', TRUE);
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (714694, '[]', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 2147483647, 1e10);
INSERT INTO tbl_07 (id, col_1, col_2, ref_tbl_05_id, dyn_col_5) VALUES (454388, '[]', TRUE, -2147483648, 'alpha_beta_gamma');
COMMIT;

SELECT
    (t_ref_1579.col_4 IN (SELECT sub_1580.col_1 FROM tbl_12 AS sub_1580)) AS c_1581,
    ((NOT ((SELECT (SELECT inn_1603.col_3 FROM tbl_10 AS inn_1603 LIMIT 1) FROM tbl_05 AS sub_1582 LIMIT 1))) >= (SELECT (CAST(sub_1604.col_2 AS DECIMAL(10,2)) > FALSE) FROM tbl_02 AS sub_1604 LIMIT 1)) AS c_1605
FROM tbl_02 AS t_ref_1579 LEFT JOIN tbl_06 AS j_ref_1606 ON t_ref_1579.id = j_ref_1606.id
WHERE LOWER((t_ref_1579.col_1 IN (SELECT sub_1607.col_4 FROM tbl_08 AS sub_1607)))
LIMIT 21;

CREATE TABLE tbl_17 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2)
);

CREATE TABLE tbl_18 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT,
    col_3 NUMERIC(10,2),
    ref_tbl_16_id INT,
    FOREIGN KEY (ref_tbl_16_id) REFERENCES tbl_16(id)
);

DELETE FROM tbl_06 WHERE id = 64454;

CREATE TABLE tbl_19 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 JSON,
    col_3 NUMERIC(10,2)
);

SELECT
    CAST((t_ref_1610.id IN (SELECT sub_1611.col_1 FROM tbl_05 AS sub_1611)) AS CHAR) AS c_1637
FROM tbl_18 AS t_ref_1610
WHERE -9223372036854775808
LIMIT 9;

INSERT INTO tbl_19 (id, col_1, col_2, col_3) VALUES (150527, FALSE, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 1.0);

ALTER TABLE tbl_15 DROP COLUMN col_2;

INSERT INTO tbl_15 (id, col_1) VALUES (773464, '__fuzz_marker__');

INSERT INTO customers_large (id, payload) VALUES (297989, '');

CREATE INDEX idx_tbl_10_col_3_17 ON tbl_10 (col_3);

INSERT INTO tbl_13 (id, col_1, col_2, col_3) VALUES (591261, '{}', '[]', 3.14159265);

CREATE TABLE tbl_20 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 BOOLEAN,
    col_3 NUMERIC(10,2),
    ref_tbl_06_id INT,
    FOREIGN KEY (ref_tbl_06_id) REFERENCES tbl_06(id)
);

CREATE VIEW v_summary_7 AS SELECT id FROM tbl_08 WHERE id IS NOT NULL;

UPDATE tbl_03 SET col_2 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 77;

ALTER TABLE tbl_11 ADD COLUMN dyn_col_6 VARCHAR(32);

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (557895, 'escaped''quote', 'alpha_beta_gamma', '', 42);
INSERT INTO tbl_14 (id, col_1, col_2, col_3, ref_tbl_12_id, dyn_col_6) VALUES (855101, TRUE, 999999999.99, '__fuzz_marker__', 9223372036854775807, 'ref_val');
COMMIT;

BEGIN;
INSERT INTO tbl_11 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (508485, -1.5, -2147483648, FALSE, '{}', 'alpha_beta_gamma');
INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (700474, 0.0000000001, '[]', 999999999.99);
INSERT INTO tbl_11 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (600107, 1e10, 2147483647, TRUE, '{}', 'ref_val');
COMMIT;

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id) VALUES (427352, 'alpha_beta_gamma', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'chaos', -1, -9223372036854775808);
INSERT INTO tbl_13 (id, col_1, col_2, col_3) VALUES (266206, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{}', 0.0);
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id) VALUES (232699, 'chaos', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '__fuzz_marker__', -2147483648, 1);
COMMIT;

CREATE TABLE tbl_21 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 INT,
    col_3 INT,
    col_4 NUMERIC(10,2)
);

DELETE FROM tbl_13 WHERE id = 28422;

UPDATE tbl_02 SET ref_tbl_01_id = -9223372036854775808 WHERE id > 97;

UPDATE tbl_06 SET col_2 = -1 WHERE id > 34;

INSERT INTO tbl_11 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (276363, 3.14159265, 42, TRUE, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'ref_val');

ALTER TABLE tbl_12 ADD COLUMN dyn_col_4 VARCHAR(32);

SELECT
    ((t_ref_1638.dyn_col_5 / NULLIF(NULLIF((CAST((((SELECT ((('' - (SELECT (SELECT inn_1641.col_2 FROM tbl_21 AS inn_1641 LIMIT 1) FROM tbl_08 AS sub_1640 LIMIT 1)) < sub_1639.col_3) + (1 - (sub_1639.col_2 IN (SELECT sub_1642.col_3 FROM tbl_06 AS sub_1642)))) FROM tbl_11 AS sub_1639 LIMIT 1) + (t_ref_1638.ref_tbl_05_id + (TRUE IS NULL))) - (SELECT (NOT (EXISTS (SELECT 1 FROM tbl_10 AS sub_1644 WHERE 1))) FROM tbl_13 AS sub_1643 LIMIT 1)) AS SIGNED) OR (CASE WHEN (t_ref_1638.col_1 / NULLIF((NOT (CAST(ABS(t_ref_1638.col_1) AS SIGNED))), 0)) THEN (t_ref_1638.ref_tbl_05_id IN (SELECT sub_1645.dyn_col_5 FROM tbl_04 AS sub_1645)) ELSE t_ref_1638.dyn_col_5 END)), (((SELECT (SELECT TRUE FROM tbl_01 AS inn_1647 LIMIT 1) FROM tbl_11 AS sub_1646 LIMIT 1) - t_ref_1638.col_1) * (SELECT (SELECT 2147483647 FROM customers_large AS inn_1649 LIMIT 1) FROM tbl_21 AS sub_1648 LIMIT 1))), 0)) < t_ref_1638.col_2) AS c_1650,
    999999999.99 AS c_1651,
    CAST(((t_ref_1638.ref_tbl_05_id >= EXISTS (SELECT 1 FROM tbl_20 AS sub_1652 WHERE sub_1652.ref_tbl_06_id)) IS NULL) AS CHAR) AS c_1680
FROM tbl_07 AS t_ref_1638
WHERE (t_ref_1638.col_2 >= CAST(LENGTH(t_ref_1638.col_1) AS CHAR))
LIMIT 7;

BEGIN;
INSERT INTO tbl_17 (id, col_1, col_2) VALUES (904574, FALSE, 999999999.99);
INSERT INTO tbl_14 (id, col_1, col_2, col_3, ref_tbl_12_id, dyn_col_6) VALUES (548833, TRUE, 3.14159265, 'escaped''quote', 1, 'ref_val');
INSERT INTO tbl_09 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (841495, 'ref_val', FALSE, '[]', -2147483648);
COMMIT;

CREATE VIEW v_summary_8 AS SELECT id FROM tbl_19 WHERE id IS NOT NULL;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (52436, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -2147483648, 1.0);

CREATE VIEW v_summary_9 AS SELECT id FROM tbl_18 WHERE id IS NOT NULL;

CREATE VIEW v_summary_10 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

INSERT INTO tbl_21 (id, col_1, col_2, col_3, col_4) VALUES (629342, 3.14159265, 1, 0, 1e10);

INSERT INTO tbl_09 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (70250, '__fuzz_marker__', TRUE, '{}', 0);

UPDATE tbl_21 SET col_4 = 999999999.99 WHERE id > 91;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, ref_tbl_02_id) VALUES (665149, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 9223372036854775807, FALSE, 100);

CREATE TABLE tbl_22 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 JSON,
    col_4 VARCHAR(64),
    ref_tbl_07_id INT,
    FOREIGN KEY (ref_tbl_07_id) REFERENCES tbl_07(id)
);

CREATE INDEX idx_tbl_21_id_18 ON tbl_21 (id);

CREATE TABLE tbl_23 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 INT,
    col_4 INT
);

DELETE FROM tbl_11 WHERE id = 44734;

SELECT
    ((t_ref_1681.col_3 <> CAST((NULL IN (SELECT sub_1682.col_2 FROM tbl_07 AS sub_1682)) AS DECIMAL(10,2))) * t_ref_1681.id) AS c_1726,
    1.0 AS c_1727
FROM tbl_20 AS t_ref_1681
WHERE (((FALSE IN (SELECT sub_1728.col_1 FROM tbl_15 AS sub_1728)) + ((TRUE AND (FALSE IN (SELECT sub_1730.id FROM tbl_21 AS sub_1730))) * (NULLIF((t_ref_1681.id + CAST(((TRUE IN (SELECT sub_1790.dyn_col_4 FROM tbl_12 AS sub_1790)) + (CAST(t_ref_1681.id AS SIGNED) < CAST(t_ref_1681.ref_tbl_06_id AS SIGNED))) AS CHAR)), CAST(((CASE WHEN ((ABS(CAST(COALESCE(((SELECT (sub_1793.id / NULLIF((CASE WHEN ABS(-1e-5) THEN ('ref_val' IS NULL) ELSE 0 END), 0)) FROM tbl_09 AS sub_1793 LIMIT 1) > (CASE WHEN (SELECT (SELECT inn_1796.col_1 FROM tbl_19 AS inn_1796 LIMIT 1) FROM tbl_11 AS sub_1794 LIMIT 1) THEN NULLIF(((CASE WHEN t_ref_1681.id THEN NULL ELSE CAST(t_ref_1681.col_3 AS SIGNED) END) > (COALESCE(t_ref_1681.id, FALSE) < t_ref_1681.id)), t_ref_1681.ref_tbl_06_id) ELSE (((CASE WHEN (t_ref_1681.col_1 / NULLIF(t_ref_1681.id, 0)) THEN (t_ref_1681.col_3 <> t_ref_1681.id) ELSE (t_ref_1681.col_3 <= t_ref_1681.ref_tbl_06_id) END) >= NULLIF(LENGTH(t_ref_1681.id), FALSE)) > LOWER(((TRUE / NULLIF(t_ref_1681.col_3, 0)) OR (NOT (t_ref_1681.col_2))))) END)), (t_ref_1681.col_2 AND (CAST((t_ref_1681.col_3 IN (SELECT sub_1797.col_2 FROM tbl_17 AS sub_1797)) AS DECIMAL(10,2)) > CAST((NOT (('chaos' + ABS(t_ref_1681.col_3)))) AS CHAR)))) AS SIGNED)) / NULLIF(NULL, 0)) >= -1e-5) THEN UPPER(((((t_ref_1681.col_1 IN (SELECT sub_1798.id FROM tbl_21 AS sub_1798)) - (((CASE WHEN ((NOT (UPPER(NULL))) < EXISTS (SELECT 1 FROM tbl_15 AS sub_1801 WHERE sub_1801.id)) THEN LENGTH(t_ref_1681.col_3) ELSE ABS(CAST(t_ref_1681.col_1 AS SIGNED)) END) / NULLIF(t_ref_1681.col_2, 0)) > (SELECT (sub_1802.id - CAST(EXISTS (SELECT 1 FROM tbl_14 AS sub_1803 WHERE sub_1803.col_1) AS CHAR)) FROM tbl_04 AS sub_1802 LIMIT 1))) <= CAST((CASE WHEN (CASE WHEN (CAST((CASE WHEN LENGTH((t_ref_1681.ref_tbl_06_id >= FALSE)) THEN t_ref_1681.ref_tbl_06_id ELSE CAST(LENGTH(t_ref_1681.id) AS DECIMAL(10,2)) END) AS SIGNED) * UPPER(CAST((SELECT sub_1804.col_3 FROM tbl_02 AS sub_1804 LIMIT 1) AS SIGNED))) THEN '__fuzz_marker__' ELSE TRUE END) THEN 'ref_val' ELSE t_ref_1681.col_1 END) AS CHAR)) + (CAST(((SELECT sub_1805.col_2 FROM tbl_11 AS sub_1805 LIMIT 1) < t_ref_1681.id) AS SIGNED) - COALESCE(((FALSE + t_ref_1681.col_1) - UPPER((t_ref_1681.col_1 = (SELECT (SELECT inn_1807.col_2 FROM tbl_10 AS inn_1807 LIMIT 1) FROM tbl_16 AS sub_1806 LIMIT 1)))), NULL)))) ELSE CAST(CAST(COALESCE(t_ref_1681.col_1, (t_ref_1681.col_2 >= ((SELECT EXISTS (SELECT 1 FROM tbl_05 AS sub_1809 WHERE sub_1809.col_3) FROM tbl_17 AS sub_1808 LIMIT 1) + TRUE))) AS SIGNED) AS CHAR) END) = -1.5) AS SIGNED)) + ((CASE WHEN t_ref_1681.col_2 THEN (t_ref_1681.col_1 > t_ref_1681.col_1) ELSE (t_ref_1681.col_1 = CAST(UPPER(CAST((NOT (t_ref_1681.id)) AS SIGNED)) AS DECIMAL(10,2))) END) * ABS((0.0000000001 <= t_ref_1681.ref_tbl_06_id)))))) * (t_ref_1681.id BETWEEN 42 AND 0))
LIMIT 40;

CREATE TABLE tbl_24 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 JSON,
    ref_tbl_10_id INT,
    FOREIGN KEY (ref_tbl_10_id) REFERENCES tbl_10(id)
);

UPDATE tbl_14 SET col_1 = TRUE WHERE id > 59;

CREATE INDEX idx_tbl_22_col_3_19 ON tbl_22 (col_3);
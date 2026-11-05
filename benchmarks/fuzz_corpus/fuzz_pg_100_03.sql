-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: POSTGRES

-- Statements: 100

-- AST Expression Depth: 16

-- Injected Defect Case: pg_06

-- Seed: 10013



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 INT,
    col_3 BOOLEAN,
    col_4 BOOLEAN,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 NUMERIC(10,2),
    col_3 INT,
    col_4 BOOLEAN,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 VARCHAR(64),
    col_3 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

UPDATE tbl_04 SET col_3 = 'escaped''quote' WHERE id > 54;

CREATE TABLE graph_nodes (
    node_id INT PRIMARY KEY,
    label TEXT
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSONB
);

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (543023, 'ref_val', '[]');
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (684062, '', '[]');
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (92105, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 1, TRUE, FALSE, 1);
COMMIT;

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (691073, '[]', 1e10, 1, FALSE, -1);

INSERT INTO graph_edges (id, payload) VALUES (157563, 'ref_val');

INSERT INTO tbl_04 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (709401, '{}', '', 'escaped''quote', 0);

DELETE FROM tbl_03 WHERE id = 60608;

INSERT INTO tbl_05 (id, col_1, col_2) VALUES (66993, 'ref_val', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (15921, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 2147483647, FALSE, FALSE, -1);

SELECT
    ((SELECT (SELECT 999999999.99 FROM tbl_04 AS inn_16 LIMIT 1) FROM tbl_05 AS sub_2 LIMIT 1) = (NULLIF(t_ref_1.payload, 1e10) > t_ref_1.payload)) AS c_17,
    (SELECT (SELECT inn_19.payload FROM graph_edges AS inn_19 LIMIT 1) FROM tbl_01 AS sub_18 LIMIT 1) AS c_20
FROM graph_edges AS t_ref_1
WHERE (TRUE IN (SELECT sub_21.id FROM tbl_04 AS sub_21))
LIMIT 42;

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2) VALUES (952777, FALSE, 0.0);
COMMIT;

UPDATE tbl_01 SET col_1 = FALSE WHERE id > 41;

SELECT
    ((t_ref_38.col_1 IN (SELECT sub_39.col_1 FROM tbl_03 AS sub_39)) OR FALSE) AS c_43,
    ((COALESCE((t_ref_38.col_2 - ((LENGTH((CASE WHEN (t_ref_38.ref_tbl_01_id / NULLIF(((CASE WHEN ((TRUE * EXISTS (SELECT 1 FROM tbl_01 AS sub_44 WHERE sub_44.id)) = t_ref_38.col_3) THEN t_ref_38.id ELSE (SELECT (sub_45.col_2 * (TRUE IN (SELECT sub_46.col_2 FROM tbl_01 AS sub_46))) FROM tbl_04 AS sub_45 LIMIT 1) END) > EXISTS (SELECT 1 FROM tbl_01 AS sub_47 WHERE FALSE)), 0)) THEN t_ref_38.id ELSE t_ref_38.col_1 END)) + (((t_ref_38.col_3 IN (SELECT sub_49.col_2 FROM tbl_05 AS sub_49)) + EXISTS (SELECT 1 FROM graph_edges AS sub_52 WHERE sub_52.id)) AND t_ref_38.col_1))::BOOLEAN)), '__fuzz_marker__')::INT) / NULLIF((t_ref_38.ref_tbl_01_id < (t_ref_38.id::BOOLEAN)), 0)) AS c_53,
    ((SELECT (SELECT inn_67.col_3 FROM tbl_03 AS inn_67 LIMIT 1) FROM graph_edges AS sub_54 LIMIT 1) / NULLIF(t_ref_38.col_4, 0)) AS c_68
FROM tbl_03 AS t_ref_38 LEFT JOIN tbl_01 AS j_ref_69 ON t_ref_38.id = j_ref_69.id
WHERE (t_ref_38.ref_tbl_01_id IN (SELECT sub_70.payload FROM graph_edges AS sub_70))
LIMIT 4;

CREATE TABLE graph_edges (
    source_id INT REFERENCES graph_nodes(node_id),
    target_id INT REFERENCES graph_nodes(node_id),
    cost INT,
    PRIMARY KEY (source_id, target_id)
);

SELECT
    (('alpha_beta_gamma' IN (SELECT sub_84.col_1 FROM tbl_01 AS sub_84)) = NULL) AS c_104
FROM tbl_02 AS t_ref_83 LEFT JOIN tbl_04 AS j_ref_105 ON t_ref_83.id = j_ref_105.id
WHERE (t_ref_83.col_3 - FALSE)
LIMIT 40;

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (813975, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -9223372036854775808, FALSE, FALSE, 0);
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (262761, 'ref_val', '[]');
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (161718, 'escaped''quote', '[]');
COMMIT;

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 JSONB,
    col_3 JSONB,
    col_4 JSONB,
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

SELECT
    (SELECT (SELECT inn_122.id FROM graph_edges AS inn_122 LIMIT 1) FROM tbl_05 AS sub_107 LIMIT 1) AS c_123,
    FALSE AS c_124,
    (t_ref_106.ref_tbl_04_id + (t_ref_106.col_2 - t_ref_106.col_4)) AS c_125
FROM tbl_06 AS t_ref_106 LEFT JOIN tbl_05 AS j_ref_126 ON t_ref_106.id = j_ref_126.id
WHERE (t_ref_106.ref_tbl_04_id * 0.0)
LIMIT 40;

CREATE INDEX idx_tbl_03_ref_tbl_01_id_1 ON tbl_03 (ref_tbl_01_id);

CREATE INDEX idx_tbl_01_col_1_2 ON tbl_01 (col_1);

INSERT INTO graph_edges (id, payload) VALUES (214003, '__fuzz_marker__');

UPDATE tbl_03 SET col_3 = 0 WHERE id > 60;

SELECT
    (CASE WHEN (t_ref_127.ref_tbl_04_id::INT::TEXT) THEN FALSE ELSE t_ref_127.col_2 END) AS c_128
FROM tbl_06 AS t_ref_127 LEFT JOIN tbl_01 AS j_ref_129 ON t_ref_127.id = j_ref_129.id
WHERE (CASE WHEN t_ref_127.col_3 THEN t_ref_127.col_1 ELSE ((NULLIF(EXISTS (SELECT 1 FROM tbl_02 AS sub_130 WHERE sub_130.ref_tbl_01_id), (ABS(((t_ref_127.col_2::NUMERIC(10,2)) BETWEEN 100 AND 0)) OR ((NOT ((SELECT (SELECT inn_132.id FROM graph_edges AS inn_132 LIMIT 1) FROM tbl_05 AS sub_131 LIMIT 1))) < t_ref_127.col_4))) IS NOT NULL) / NULLIF(t_ref_127.col_4, 0)) END)
LIMIT 4;

SELECT
    COALESCE(1, (t_ref_133.col_2 AND (t_ref_133.col_2 + (FALSE / NULLIF('', 0))))) AS c_134
FROM tbl_03 AS t_ref_133 LEFT JOIN tbl_02 AS j_ref_135 ON t_ref_133.id = j_ref_135.id
WHERE ABS((TRUE <= (t_ref_133.ref_tbl_01_id IN (SELECT sub_136.col_1 FROM tbl_02 AS sub_136))))
LIMIT 47;

SELECT
    t_ref_159.col_2 AS c_160,
    LENGTH((CASE WHEN (CASE WHEN FALSE THEN (((((SELECT (((CASE WHEN ABS(sub_161.col_1) THEN ((sub_161.col_3::TEXT) * sub_161.col_3) ELSE EXISTS (SELECT 1 FROM tbl_05 AS sub_162 WHERE sub_162.col_2) END) - LOWER(((CASE WHEN (sub_161.col_1 + sub_161.col_2) THEN sub_161.col_4 ELSE (sub_161.col_3 IN (SELECT sub_163.id FROM tbl_06 AS sub_163)) END) > ((-1 >= sub_161.id) - (1.0 >= sub_161.ref_tbl_01_id))))) + sub_161.ref_tbl_01_id) FROM tbl_03 AS sub_161 LIMIT 1) + ((t_ref_159.col_2::INT) * (SELECT (SELECT (SELECT inn_166.id FROM tbl_02 AS inn_166 LIMIT 1) FROM tbl_06 AS sub_165 LIMIT 1) FROM tbl_03 AS sub_164 LIMIT 1))) <> LOWER(TRUE)) / NULLIF(UPPER((SELECT sub_167.col_2 FROM tbl_03 AS sub_167 LIMIT 1)), 0)) AND (CASE WHEN (LOWER((-1e-5 IN (SELECT sub_168.col_1 FROM tbl_01 AS sub_168))) < (FALSE IN (SELECT sub_178.ref_tbl_04_id FROM tbl_06 AS sub_178))) THEN t_ref_159.col_1 ELSE (('alpha_beta_gamma' AND t_ref_159.id) <= ((CASE WHEN (LENGTH((t_ref_159.col_2::TEXT)) * t_ref_159.id) THEN LOWER(COALESCE(t_ref_159.col_2, (NOT ((SELECT ((sub_184.payload BETWEEN 9223372036854775807 AND 2147483647) OR (sub_184.payload::INT)) FROM graph_edges AS sub_184 LIMIT 1))))) ELSE ((EXISTS (SELECT 1 FROM tbl_05 AS sub_185 WHERE sub_185.col_2) <= UPPER(NULL))::NUMERIC(10,2)) END) * ((NULLIF(t_ref_159.col_1, t_ref_159.id) - (t_ref_159.col_2 IN (SELECT sub_186.col_1 FROM tbl_05 AS sub_186)))::NUMERIC(10,2)))) END)) ELSE ((FALSE IN (SELECT sub_187.col_2 FROM tbl_05 AS sub_187))::TEXT::VARCHAR(32)) END) THEN (CASE WHEN (t_ref_159.col_2 / NULLIF(NULL, 0)) THEN t_ref_159.id ELSE ABS(UPPER(((SELECT (SELECT FALSE FROM tbl_02 AS inn_198 LIMIT 1) FROM tbl_01 AS sub_189 LIMIT 1) / NULLIF(t_ref_159.col_1, 0)))) END) ELSE (NULLIF(UPPER(ABS((NOT ('ref_val')))), t_ref_159.id) BETWEEN 2147483647 AND 42) END)) AS c_199
FROM tbl_01 AS t_ref_159 LEFT JOIN tbl_05 AS j_ref_200 ON t_ref_159.id = j_ref_200.id
WHERE (SELECT (SELECT NULL FROM graph_edges AS inn_202 LIMIT 1) FROM tbl_06 AS sub_201 LIMIT 1)
LIMIT 50;

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (990595, 'ref_val', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (863107, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 0, FALSE, TRUE, -9223372036854775808);
COMMIT;

INSERT INTO graph_nodes (node_id, label) VALUES
    (1, 'Node A'), (2, 'Node B'), (3, 'Node C'), (4, 'Node D');

INSERT INTO tbl_04 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (370487, '[]', '', 'chaos', 100);

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 VARCHAR(64)
);

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 BOOLEAN
);

INSERT INTO graph_edges (id, payload) VALUES (691317, 'escaped''quote');

CREATE INDEX idx_tbl_07_id_3 ON tbl_07 (id);

CREATE INDEX idx_tbl_08_col_2_4 ON tbl_08 (col_2);

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    col_3 BOOLEAN,
    ref_tbl_08_id INT,
    FOREIGN KEY (ref_tbl_08_id) REFERENCES tbl_08(id)
);

ALTER TABLE tbl_03 DROP COLUMN col_4;

SELECT
    ((NULLIF((CASE WHEN ABS(UPPER(((SELECT (SELECT inn_205.col_3 FROM tbl_03 AS inn_205 LIMIT 1) FROM graph_edges AS sub_204 LIMIT 1) / NULLIF(((NULLIF((CASE WHEN ((((t_ref_203.id::NUMERIC(10,2))::NUMERIC(10,2)) + ((SELECT FALSE FROM tbl_07 AS sub_206 LIMIT 1)::INT)) + TRUE) THEN (t_ref_203.id::INT) ELSE 'alpha_beta_gamma' END), (((t_ref_203.col_1 IN (SELECT sub_207.col_2 FROM tbl_06 AS sub_207)) / NULLIF(t_ref_203.id, 0)) BETWEEN 42 AND 0)) >= ((COALESCE(((t_ref_203.col_1 IN (SELECT sub_208.col_1 FROM tbl_01 AS sub_208)) - ((1.0 = t_ref_203.id) BETWEEN 100 AND 2147483647)), (t_ref_203.id IN (SELECT sub_209.col_2 FROM tbl_08 AS sub_209))) OR LOWER(t_ref_203.col_2))::BOOLEAN)) / NULLIF((((((SELECT (SELECT inn_213.col_2 FROM tbl_05 AS inn_213 LIMIT 1) FROM tbl_03 AS sub_212 LIMIT 1) + t_ref_203.col_1) < (t_ref_203.col_2 + (((SELECT (SELECT inn_215.id FROM tbl_08 AS inn_215 LIMIT 1) FROM tbl_07 AS sub_214 LIMIT 1) > ABS(TRUE)) AND ((SELECT NULL FROM tbl_01 AS sub_216 LIMIT 1)::INT)))) + t_ref_203.col_1) IS NULL), 0)), 0)))) THEN (((SELECT (SELECT inn_218.col_2 FROM tbl_06 AS inn_218 LIMIT 1) FROM tbl_08 AS sub_217 LIMIT 1) + (SELECT sub_219.payload FROM graph_edges AS sub_219 LIMIT 1)) < (t_ref_203.col_1 IN (SELECT sub_220.id FROM graph_edges AS sub_220))) ELSE UPPER(EXISTS (SELECT 1 FROM tbl_05 AS sub_223 WHERE sub_223.id)) END), ((ABS((SELECT (SELECT inn_228.id FROM tbl_01 AS inn_228 LIMIT 1) FROM tbl_01 AS sub_225 LIMIT 1)) <= 'alpha_beta_gamma') / NULLIF((LOWER(0.0000000001) - ((SELECT (SELECT inn_231.id FROM tbl_05 AS inn_231 LIMIT 1) FROM tbl_08 AS sub_229 LIMIT 1) < ((CASE WHEN ((SELECT EXISTS (SELECT 1 FROM tbl_09 AS sub_233 WHERE sub_233.col_2) FROM tbl_06 AS sub_232 LIMIT 1) / NULLIF(((CASE WHEN t_ref_203.col_1 THEN (NOT (t_ref_203.col_1)) ELSE ((CASE WHEN (0.0000000001 + (SELECT sub_234.col_1 FROM tbl_03 AS sub_234 LIMIT 1)) THEN ((t_ref_203.id = t_ref_203.col_1)::INT::TEXT) ELSE EXISTS (SELECT 1 FROM tbl_06 AS sub_235 WHERE sub_235.col_3) END) * EXISTS (SELECT 1 FROM tbl_05 AS sub_236 WHERE sub_236.col_1)) END)::BOOLEAN), 0)) THEN ((((ABS(t_ref_203.col_1)::TEXT) / NULLIF(NULL, 0))::INT::TEXT) + (COALESCE(UPPER(0.0), (t_ref_203.col_2 IN (SELECT sub_238.col_3 FROM tbl_03 AS sub_238))) > t_ref_203.col_1)) ELSE ((SELECT (SELECT inn_240.col_2 FROM tbl_05 AS inn_240 LIMIT 1) FROM tbl_06 AS sub_239 LIMIT 1)::TEXT::VARCHAR(32)) END) BETWEEN -2147483648 AND 2147483647))), 0))) * (EXISTS (SELECT 1 FROM tbl_01 AS sub_241 WHERE sub_241.id) * t_ref_203.col_1)) + ((((CASE WHEN ((COALESCE(((SELECT (SELECT FALSE FROM tbl_01 AS inn_247 LIMIT 1) FROM graph_edges AS sub_246 LIMIT 1) / NULLIF((SELECT (SELECT 9223372036854775807 FROM tbl_06 AS inn_250 LIMIT 1) FROM tbl_02 AS sub_248 LIMIT 1), 0)), ('' IN (SELECT sub_251.col_1 FROM tbl_05 AS sub_251))) - ((((-1e-5 * (t_ref_203.col_2::INT::TEXT)) + (SELECT EXISTS (SELECT 1 FROM tbl_06 AS sub_261 WHERE -1e-5) FROM tbl_05 AS sub_260 LIMIT 1)) * (CASE WHEN '__fuzz_marker__' THEN ((NULLIF((SELECT (SELECT inn_264.col_1 FROM tbl_05 AS inn_264 LIMIT 1) FROM tbl_03 AS sub_263 LIMIT 1), LENGTH((TRUE + t_ref_203.id))) / NULLIF((t_ref_203.id > COALESCE((SELECT sub_265.payload FROM graph_edges AS sub_265 LIMIT 1), (SELECT sub_266.col_2 FROM tbl_04 AS sub_266 LIMIT 1))), 0))::INT) ELSE NULL END)) * ((SELECT (SELECT ABS(LOWER('chaos')) FROM tbl_05 AS sub_268 LIMIT 1) FROM tbl_08 AS sub_267 LIMIT 1) * (SELECT (SELECT inn_272.id FROM tbl_08 AS inn_272 LIMIT 1) FROM tbl_07 AS sub_269 LIMIT 1)))) + 'escaped''quote') THEN (ABS(((((((SELECT (sub_273.id + sub_273.col_1) FROM tbl_01 AS sub_273 LIMIT 1) IS NULL) IS NOT NULL) IS NOT NULL)::TEXT)::TEXT::VARCHAR(32))) / NULLIF(NULLIF(t_ref_203.id, ((CASE WHEN (SELECT (SELECT inn_275.col_1 FROM tbl_08 AS inn_275 LIMIT 1) FROM tbl_06 AS sub_274 LIMIT 1) THEN (((CASE WHEN ((SELECT sub_276.col_1 FROM tbl_03 AS sub_276 LIMIT 1) < t_ref_203.id) THEN (NULLIF((t_ref_203.col_1 AND 1e10), (SELECT sub_277.col_2 FROM tbl_06 AS sub_277 LIMIT 1)) / NULLIF(((t_ref_203.col_1 IN (SELECT sub_278.id FROM graph_edges AS sub_278)) + t_ref_203.col_1), 0)) ELSE COALESCE(LOWER((-9223372036854775808::TEXT)), (t_ref_203.col_2::INT)) END)::BOOLEAN) <= (SELECT (SELECT inn_280.col_1 FROM tbl_05 AS inn_280 LIMIT 1) FROM tbl_07 AS sub_279 LIMIT 1)) ELSE ((t_ref_203.col_1 - (CASE WHEN (NOT (LOWER((FALSE IN (SELECT sub_281.col_1 FROM tbl_03 AS sub_281))))) THEN (UPPER(NULL) >= t_ref_203.id) ELSE (NULLIF((t_ref_203.id OR t_ref_203.col_2), COALESCE(TRUE, NULL)) * (t_ref_203.col_1 BETWEEN 100 AND 1)) END)) IS NOT NULL) END) <= (t_ref_203.id - t_ref_203.col_1))), 0)) ELSE ((((SELECT UPPER(LENGTH((SELECT (SELECT inn_284.col_2 FROM tbl_04 AS inn_284 LIMIT 1) FROM graph_edges AS sub_283 LIMIT 1))) FROM tbl_02 AS sub_282 LIMIT 1) > (CASE WHEN (((t_ref_203.col_2 OR ((t_ref_203.col_2 - (t_ref_203.col_2 >= t_ref_203.col_2)) - ((NULL IN (SELECT sub_285.payload FROM graph_edges AS sub_285)) <> (SELECT (SELECT FALSE FROM tbl_02 AS inn_287 LIMIT 1) FROM tbl_08 AS sub_286 LIMIT 1)))) + NULLIF((-1.5 / NULLIF(((NULL::INT::TEXT) * t_ref_203.id), 0)), (t_ref_203.id OR ((SELECT sub_288.ref_tbl_01_id FROM tbl_03 AS sub_288 LIMIT 1) < (CASE WHEN t_ref_203.col_1 THEN t_ref_203.id ELSE 1 END)))))::TEXT) THEN (EXISTS (SELECT 1 FROM tbl_09 AS sub_289 WHERE 1e10) IS NULL) ELSE ((t_ref_203.col_2 IN (SELECT sub_292.id FROM graph_edges AS sub_292))::INT) END))::NUMERIC(10,2)) - ((NULL <> LENGTH((UPPER(EXISTS (SELECT 1 FROM tbl_05 AS sub_293 WHERE FALSE)) <> ((((SELECT 'ref_val' FROM tbl_09 AS sub_294 LIMIT 1) - ((t_ref_203.id OR TRUE) + (NULL + 0.0000000001))) IS NULL) AND LENGTH(3.14159265))))) AND 3.14159265)) END) BETWEEN -1 AND -2147483648) >= t_ref_203.col_1) = (SELECT (SELECT inn_296.col_2 FROM tbl_05 AS inn_296 LIMIT 1) FROM tbl_07 AS sub_295 LIMIT 1))) AS c_297,
    t_ref_203.id AS c_298
FROM tbl_07 AS t_ref_203 LEFT JOIN graph_edges AS j_ref_299 ON t_ref_203.id = j_ref_299.id
WHERE ABS(t_ref_203.col_2)
LIMIT 40;

DELETE FROM tbl_07 WHERE id = 69984;

INSERT INTO graph_edges (source_id, target_id, cost) VALUES
    (1, 2, 10),
    (2, 3, 20),
    (3, 1, 30),
    (3, 4, 15);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (972473, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 0.0000000001, -9223372036854775808, 1);

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 BOOLEAN,
    col_3 JSONB
);

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (94626, '{}', 42, FALSE, FALSE, 1);
COMMIT;

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 INT
);

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 INT,
    col_3 VARCHAR(64),
    col_4 VARCHAR(64),
    ref_tbl_06_id INT,
    FOREIGN KEY (ref_tbl_06_id) REFERENCES tbl_06(id)
);

ALTER TABLE tbl_02 DROP COLUMN col_4;

CREATE INDEX idx_tbl_09_col_2_5 ON tbl_09 (col_2);

CREATE INDEX idx_tbl_11_col_3_6 ON tbl_11 (col_3);

BEGIN;
INSERT INTO tbl_12 (id, col_1, col_2, col_3, col_4, ref_tbl_06_id) VALUES (281585, '{}', 100, '', 'ref_val', 100);
INSERT INTO graph_edges (id, payload) VALUES (30751, '');
COMMIT;

SELECT
    (t_ref_300.col_1 IN (SELECT sub_301.col_3 FROM tbl_12 AS sub_301)) AS c_310,
    1.0 AS c_311,
    (NULLIF(((0.0 > (((UPPER(0) + LENGTH(UPPER(t_ref_300.ref_tbl_01_id))) * (((t_ref_300.col_2::NUMERIC(10,2)) + (((((CASE WHEN ((t_ref_300.ref_tbl_01_id::BOOLEAN) / NULLIF((SELECT (SELECT inn_313.id FROM tbl_11 AS inn_313 LIMIT 1) FROM tbl_12 AS sub_312 LIMIT 1), 0)) THEN t_ref_300.col_3 ELSE (SELECT (SELECT -1e-5 FROM tbl_12 AS inn_315 LIMIT 1) FROM tbl_02 AS sub_314 LIMIT 1) END)::INT) >= NULL) = (t_ref_300.col_3 IN (SELECT sub_316.id FROM graph_edges AS sub_316))) OR t_ref_300.id))::BOOLEAN)) + ((t_ref_300.col_2 > (SELECT (SELECT TRUE FROM tbl_10 AS inn_321 LIMIT 1) FROM tbl_10 AS sub_318 LIMIT 1))::TEXT::VARCHAR(32))))::INT), (ABS(LOWER((t_ref_300.col_3 < (EXISTS (SELECT 1 FROM tbl_03 AS sub_322 WHERE sub_322.col_2)::NUMERIC(10,2))))) - (CASE WHEN t_ref_300.id THEN (((LOWER((((((TRUE / NULLIF(t_ref_300.id, 0)) / NULLIF(t_ref_300.col_1, 0))::TEXT) + t_ref_300.id) - (((CASE WHEN NULL THEN NULLIF(t_ref_300.col_3, (SELECT sub_323.col_1 FROM tbl_05 AS sub_323 LIMIT 1)) ELSE (NULLIF((SELECT (SELECT inn_325.col_3 FROM tbl_02 AS inn_325 LIMIT 1) FROM tbl_07 AS sub_324 LIMIT 1), t_ref_300.col_3) + FALSE) END) AND (((SELECT (SELECT inn_327.ref_tbl_08_id FROM tbl_09 AS inn_327 LIMIT 1) FROM tbl_09 AS sub_326 LIMIT 1)::TEXT) AND (1e10 + ((TRUE + t_ref_300.col_2) / NULLIF((SELECT sub_328.col_1 FROM tbl_11 AS sub_328 LIMIT 1), 0))))) / NULLIF((COALESCE(COALESCE((UPPER(t_ref_300.id) > t_ref_300.col_3), ((CASE WHEN t_ref_300.col_1 THEN 1 ELSE -1 END) / NULLIF((t_ref_300.col_1 - t_ref_300.id), 0))), (COALESCE((t_ref_300.id IN (SELECT sub_329.id FROM tbl_04 AS sub_329)), LOWER(t_ref_300.col_3))::NUMERIC(10,2)))::NUMERIC(10,2)), 0)))) + (-1 * t_ref_300.col_2)) - NULL)::TEXT) ELSE (t_ref_300.id::INT::TEXT) END))) / NULLIF(((t_ref_300.col_2 + (SELECT LOWER((ABS((SELECT LENGTH((sub_331.col_1 - (SELECT (TRUE IS NOT NULL) FROM tbl_09 AS sub_332 LIMIT 1))) FROM tbl_03 AS sub_331 LIMIT 1)) / NULLIF(COALESCE((sub_330.id IS NOT NULL), (CASE WHEN (0.0000000001 - -1.5) THEN NULLIF((EXISTS (SELECT 1 FROM tbl_01 AS sub_333 WHERE 'alpha_beta_gamma') > (((SELECT sub_335.id FROM tbl_05 AS sub_335 LIMIT 1) / NULLIF(((sub_330.col_1::NUMERIC(10,2)) * (sub_330.ref_tbl_04_id OR sub_330.col_1)), 0)) = ((sub_330.col_3 IN (SELECT sub_336.ref_tbl_01_id FROM tbl_04 AS sub_336)) - EXISTS (SELECT 1 FROM tbl_04 AS sub_337 WHERE sub_337.id)))), sub_330.ref_tbl_04_id) ELSE LENGTH(sub_330.col_3) END)), 0))) FROM tbl_06 AS sub_330 LIMIT 1)) >= ((((t_ref_300.id >= (1.0::INT))::TEXT::VARCHAR(32)) BETWEEN 9223372036854775807 AND -1) AND EXISTS (SELECT 1 FROM tbl_11 AS sub_338 WHERE sub_338.id))), 0)) AS c_339
FROM tbl_03 AS t_ref_300
WHERE ((SELECT (SELECT inn_343.col_2 FROM tbl_06 AS inn_343 LIMIT 1) FROM tbl_07 AS sub_340 LIMIT 1) - (SELECT (((sub_344.id - ((sub_344.ref_tbl_01_id AND NULLIF(sub_344.col_2, UPPER(((sub_344.col_3 IN (SELECT sub_345.col_3 FROM tbl_09 AS sub_345)) - (SELECT NULL FROM tbl_12 AS sub_348 LIMIT 1))))) <= sub_344.col_3)) + ((sub_344.col_2::NUMERIC(10,2)) + 3.14159265))::BOOLEAN) FROM tbl_03 AS sub_344 LIMIT 1))
LIMIT 16;

ALTER TABLE tbl_11 ADD COLUMN dyn_col_5 VARCHAR(32);

INSERT INTO tbl_12 (id, col_1, col_2, col_3, col_4, ref_tbl_06_id) VALUES (984385, '{}', -9223372036854775808, 'ref_val', '', -9223372036854775808);

INSERT INTO tbl_11 (id, col_1, col_2, col_3, dyn_col_5) VALUES (904017, 3.14159265, 'chaos', 100, 'escaped''quote');

CREATE INDEX idx_graph_edges_payload_7 ON graph_edges (payload);

INSERT INTO graph_edges (id, payload) VALUES (561556, 'escaped''quote');

INSERT INTO tbl_09 (id, col_1, col_2, col_3, ref_tbl_08_id) VALUES (813886, TRUE, 'chaos', TRUE, 2147483647);

ALTER TABLE tbl_12 ADD COLUMN dyn_col_7 VARCHAR(32);

ALTER TABLE tbl_05 ADD COLUMN dyn_col_4 VARCHAR(32);

CREATE INDEX idx_tbl_06_ref_tbl_04_id_8 ON tbl_06 (ref_tbl_04_id);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (527900, '{}', 42, TRUE, 42);

DELETE FROM tbl_01 WHERE id = 31656;

INSERT INTO tbl_12 (id, col_1, col_2, col_3, col_4, ref_tbl_06_id, dyn_col_7) VALUES (187889, '[]', 0, '', 'escaped''quote', 2147483647, 'chaos');

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_06 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2) VALUES (672935, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '');
INSERT INTO tbl_08 (id, col_1, col_2) VALUES (381060, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', TRUE);
COMMIT;

DELETE FROM tbl_07 WHERE id = 46652;

CREATE INDEX idx_tbl_02_ref_tbl_01_id_9 ON tbl_02 (ref_tbl_01_id);

ALTER TABLE tbl_12 DROP COLUMN ref_tbl_06_id;

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2) VALUES (550996, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', FALSE);
INSERT INTO graph_edges (id, payload) VALUES (237866, '');
INSERT INTO graph_edges (id, payload) VALUES (651017, 'alpha_beta_gamma');
COMMIT;

DELETE FROM tbl_03 WHERE id = 38576;

CREATE TABLE graph_nodes (
    node_id INT PRIMARY KEY,
    label TEXT
);

WITH RECURSIVE traverse(src, dst, depth, total_cost) AS (
    SELECT source_id, target_id, 1, cost
    FROM graph_edges
    WHERE source_id = 1
    UNION
    SELECT e.source_id, e.target_id, t.depth + 1, t.total_cost + e.cost
    FROM graph_edges e
    JOIN traverse t ON e.source_id = t.dst
    WHERE t.depth < 10
)
CYCLE dst SET is_cycle USING path_array
SELECT
    t.src,
    t.dst,
    t.depth,
    t.total_cost,
    t.is_cycle,
    gn.label AS destination_label
FROM traverse t
LEFT JOIN graph_nodes gn ON t.dst = gn.node_id
WHERE t.is_cycle = FALSE OR t.depth <= 3;

DELETE FROM tbl_11 WHERE id = 5102;

BEGIN;
INSERT INTO graph_edges (id, payload) VALUES (293354, 'ref_val');
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_04_id) VALUES (758425, '{}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 100);
COMMIT;

INSERT INTO tbl_03 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (12604, '[]', 1.0, 0, 1);

SELECT
    (((EXISTS (SELECT 1 FROM tbl_11 AS sub_350 WHERE sub_350.id) - ((42 IN (SELECT sub_353.ref_tbl_08_id FROM tbl_09 AS sub_353)) / NULLIF(((1e10 IN (SELECT sub_356.id FROM graph_edges AS sub_356)) + (('chaos' IN (SELECT sub_358.payload FROM graph_edges AS sub_358)) IS NOT NULL)), 0))) <> (SELECT (CASE WHEN (SELECT (SELECT inn_362.ref_tbl_01_id FROM tbl_02 AS inn_362 LIMIT 1) FROM tbl_06 AS sub_361 LIMIT 1) THEN ((SELECT (SELECT NULL FROM tbl_08 AS inn_366 LIMIT 1) FROM tbl_08 AS sub_363 LIMIT 1) - (UPPER((SELECT (COALESCE(sub_367.col_3, (sub_367.id IN (SELECT sub_368.id FROM tbl_07 AS sub_368))) - EXISTS (SELECT 1 FROM tbl_04 AS sub_369 WHERE sub_369.ref_tbl_01_id)) FROM tbl_04 AS sub_367 LIMIT 1)) < LOWER(COALESCE((UPPER(COALESCE(LENGTH((('chaos' - TRUE) / NULLIF(UPPER(-1.5), 0))), (('escaped''quote' / NULLIF(sub_360.col_2, 0)) IS NULL)))::TEXT::VARCHAR(32)), sub_360.id)))) ELSE ((SELECT (SELECT inn_377.col_2 FROM tbl_12 AS inn_377 LIMIT 1) FROM tbl_02 AS sub_370 LIMIT 1) <= (sub_360.col_2 BETWEEN 42 AND 0)) END) FROM tbl_04 AS sub_360 LIMIT 1)) - (EXISTS (SELECT 1 FROM tbl_07 AS sub_378 WHERE -2147483648) IS NULL)) AS c_387,
    ((SELECT (SELECT inn_391.col_2 FROM tbl_05 AS inn_391 LIMIT 1) FROM tbl_02 AS sub_388 LIMIT 1) > (t_ref_349.id IN (SELECT sub_392.col_2 FROM tbl_05 AS sub_392))) AS c_394
FROM tbl_08 AS t_ref_349
WHERE (CASE WHEN (SELECT (SELECT -1e-5 FROM tbl_06 AS inn_425 LIMIT 1) FROM tbl_09 AS sub_395 LIMIT 1) THEN (SELECT (SELECT inn_440.id FROM tbl_08 AS inn_440 LIMIT 1) FROM tbl_08 AS sub_426 LIMIT 1) ELSE ((CASE WHEN ((NOT (FALSE)) * (t_ref_349.col_1::INT)) THEN (-1 > t_ref_349.col_1) ELSE ((0.0000000001 IN (SELECT sub_441.col_3 FROM tbl_12 AS sub_441)) < FALSE) END)::INT) END)
LIMIT 16;

CREATE INDEX idx_graph_edges_payload_10 ON graph_edges (payload);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (423430, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'escaped''quote', 'escaped''quote', 42);

CREATE INDEX idx_tbl_04_col_1_11 ON tbl_04 (col_1);

SELECT
    (COALESCE((t_ref_444.col_2 IN (SELECT sub_445.col_3 FROM tbl_06 AS sub_445)), (t_ref_444.id::INT)) = (((t_ref_444.col_2 OR NULL) / NULLIF((NOT ((((SELECT (SELECT inn_447.id FROM graph_edges AS inn_447 LIMIT 1) FROM tbl_04 AS sub_446 LIMIT 1) >= (NOT (t_ref_444.col_2))) * UPPER((CASE WHEN (t_ref_444.col_2 IN (SELECT sub_448.col_2 FROM tbl_07 AS sub_448)) THEN (NULLIF((NULLIF((((NOT ((FALSE / NULLIF(t_ref_444.col_1, 0))))::INT)::TEXT), (ABS((SELECT sub_451.ref_tbl_08_id FROM tbl_09 AS sub_451 LIMIT 1)) * t_ref_444.col_1)) / NULLIF(t_ref_444.id, 0)), EXISTS (SELECT 1 FROM tbl_06 AS sub_452 WHERE sub_452.col_1)) OR (t_ref_444.id IN (SELECT sub_453.col_2 FROM tbl_02 AS sub_453))) ELSE LENGTH(('escaped''quote' >= ((TRUE BETWEEN -9223372036854775808 AND -9223372036854775808)::INT::TEXT))) END))))), 0)) * (LENGTH((LENGTH(TRUE) AND (SELECT sub_455.col_2 FROM tbl_05 AS sub_455 LIMIT 1))) + FALSE))) AS c_456,
    TRUE AS c_457
FROM tbl_01 AS t_ref_444
WHERE EXISTS (SELECT 1 FROM tbl_07 AS sub_458 WHERE FALSE)
LIMIT 12;

UPDATE tbl_05 SET dyn_col_4 = 'alpha_beta_gamma' WHERE id > 4;

ALTER TABLE tbl_09 ADD COLUMN dyn_col_6 VARCHAR(32);

INSERT INTO tbl_08 (id, col_1, col_2) VALUES (53428, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', FALSE);

DELETE FROM tbl_04 WHERE id = 94547;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4, ref_tbl_04_id) VALUES (5401, '[]', '{}', '{}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 1);
INSERT INTO tbl_12 (id, col_1, col_2, col_3, col_4, dyn_col_7) VALUES (771498, '[]', 100, 'ref_val', 'alpha_beta_gamma', 'ref_val');
INSERT INTO tbl_07 (id, col_1, col_2) VALUES (61828, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '');
COMMIT;

INSERT INTO tbl_11 (id, col_1, col_2, col_3, dyn_col_5) VALUES (267500, 3.14159265, 'alpha_beta_gamma', 100, '__fuzz_marker__');

UPDATE tbl_04 SET col_3 = 'alpha_beta_gamma' WHERE id > 59;

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_09 WHERE id IS NOT NULL;

INSERT INTO tbl_12 (id, col_1, col_2, col_3, col_4, dyn_col_7) VALUES (311356, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 100, '', '__fuzz_marker__', 'escaped''quote');

INSERT INTO tbl_12 (id, col_1, col_2, col_3, col_4, dyn_col_7) VALUES (272110, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 42, 'alpha_beta_gamma', '', 'alpha_beta_gamma');

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN,
    ref_tbl_07_id INT,
    FOREIGN KEY (ref_tbl_07_id) REFERENCES tbl_07(id)
);

BEGIN;
INSERT INTO tbl_11 (id, col_1, col_2, col_3, dyn_col_5) VALUES (588718, 0.0, 'escaped''quote', -9223372036854775808, 'chaos');
INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (100248, FALSE, TRUE, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
COMMIT;

INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (367460, TRUE, TRUE, '[]');

UPDATE tbl_12 SET dyn_col_7 = '__fuzz_marker__' WHERE id > 97;

UPDATE tbl_02 SET col_2 = -9223372036854775808 WHERE id > 0;
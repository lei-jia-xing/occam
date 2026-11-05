-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: POSTGRES

-- Statements: 200

-- AST Expression Depth: 18

-- Injected Defect Case: pg_10

-- Seed: 20011



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 BOOLEAN,
    col_3 BOOLEAN,
    col_4 NUMERIC(10,2)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 VARCHAR(64),
    col_3 JSONB,
    col_4 VARCHAR(64),
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

CREATE INDEX idx_corpus_articles_id_1 ON corpus_articles (id);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (662394, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', FALSE, TRUE, 999999999.99);

CREATE TABLE corpus_articles (
    doc_id INT PRIMARY KEY,
    title VARCHAR(256),
    tsv_content TSVECTOR NOT NULL
);

SELECT
    UPPER((((LOWER(t_ref_1.col_1) / NULLIF(ABS(NULL), 0)) <= t_ref_1.col_1) - COALESCE((t_ref_1.col_3 IN (SELECT sub_2.col_3 FROM tbl_02 AS sub_2)), (1 IN (SELECT sub_4.id FROM tbl_01 AS sub_4))))) AS c_21
FROM tbl_01 AS t_ref_1
WHERE TRUE
LIMIT 6;

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (754585, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'ref_val', 42);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (969746, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'chaos', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'ref_val', -1);
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (508565, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', FALSE, TRUE, 1.0);
COMMIT;

BEGIN;
INSERT INTO corpus_articles (id, payload) VALUES (437484, 'ref_val');
INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (913915, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '__fuzz_marker__', -1e-5);
COMMIT;

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64)
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2)
);

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64),
    col_4 INT
);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (119734, '{}', '__fuzz_marker__', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'ref_val', -1);

CREATE INDEX idx_corpus_articles_payload_2 ON corpus_articles (payload);

DELETE FROM tbl_03 WHERE id = 46693;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (936486, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', FALSE, TRUE, 0.0000000001);

UPDATE tbl_02 SET col_4 = -1.5 WHERE id > 6;

SELECT
    (t_ref_22.col_2 IN (SELECT sub_23.col_2 FROM tbl_06 AS sub_23)) AS c_24
FROM tbl_03 AS t_ref_22 LEFT JOIN tbl_06 AS j_ref_25 ON t_ref_22.id = j_ref_25.id
WHERE (t_ref_22.id IN (SELECT sub_26.id FROM corpus_articles AS sub_26))
LIMIT 42;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2) VALUES (327928, TRUE, 'escaped''quote');
INSERT INTO corpus_articles (id, payload) VALUES (245745, 'chaos');
COMMIT;

SELECT
    (100 > ((((t_ref_48.col_1 IN (SELECT sub_49.id FROM corpus_articles AS sub_49)) / NULLIF((SELECT (SELECT 1e10 FROM tbl_05 AS inn_52 LIMIT 1) FROM tbl_03 AS sub_51 LIMIT 1), 0))::TEXT)::BOOLEAN)) AS c_53,
    t_ref_48.col_4 AS c_54,
    ('ref_val' * t_ref_48.col_3) AS c_55
FROM tbl_02 AS t_ref_48
WHERE t_ref_48.id
LIMIT 25;

SELECT
    ((SELECT (SELECT inn_73.id FROM tbl_03 AS inn_73 LIMIT 1) FROM tbl_06 AS sub_57 LIMIT 1)::INT) AS c_74,
    ((ABS((t_ref_56.payload = ((t_ref_56.id BETWEEN 1 AND 42) < (CASE WHEN LOWER(((SELECT TRUE FROM tbl_02 AS sub_75 LIMIT 1) < ((((SELECT (EXISTS (SELECT 1 FROM tbl_06 AS sub_77 WHERE FALSE)::TEXT::VARCHAR(32)) FROM tbl_01 AS sub_76 LIMIT 1)::TEXT::VARCHAR(32)) * t_ref_56.id) OR ABS(ABS(((ABS(FALSE) OR (SELECT (sub_80.id::TEXT::VARCHAR(32)) FROM corpus_articles AS sub_80 LIMIT 1)) < FALSE)))))) THEN t_ref_56.payload ELSE (NOT (t_ref_56.payload)) END)))) * (t_ref_56.id::BOOLEAN)) = (COALESCE(EXISTS (SELECT 1 FROM corpus_articles AS sub_81 WHERE sub_81.id), t_ref_56.payload) * (UPPER((SELECT (SELECT inn_93.col_4 FROM tbl_06 AS inn_93 LIMIT 1) FROM tbl_06 AS sub_82 LIMIT 1)) + t_ref_56.id))) AS c_94,
    FALSE AS c_95
FROM corpus_articles AS t_ref_56 LEFT JOIN tbl_05 AS j_ref_96 ON t_ref_56.id = j_ref_96.id
WHERE t_ref_56.id
LIMIT 14;

ALTER TABLE tbl_03 DROP COLUMN ref_tbl_02_id;

DELETE FROM tbl_06 WHERE id = 77993;

CREATE INDEX idx_corpus_articles_payload_3 ON corpus_articles (payload);

UPDATE tbl_05 SET col_1 = 'alpha_beta_gamma' WHERE id > 63;

CREATE INDEX idx_tbl_03_id_4 ON tbl_03 (id);

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (282381, '[]', '', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'chaos');
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (126856, FALSE, -1e-5, '__fuzz_marker__', 9223372036854775807);
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (663826, TRUE, 0.0000000001, 'alpha_beta_gamma', 0);
COMMIT;

ALTER TABLE tbl_04 ADD COLUMN dyn_col_4 VARCHAR(32);

UPDATE tbl_03 SET col_1 = '[]' WHERE id > 30;

SELECT
    t_ref_97.col_3 AS c_98
FROM tbl_01 AS t_ref_97 LEFT JOIN tbl_06 AS j_ref_99 ON t_ref_97.id = j_ref_99.id
WHERE (t_ref_97.col_1 < NULLIF(LOWER(((t_ref_97.id::TEXT) + UPPER(NULLIF((((NOT ((TRUE = t_ref_97.col_2)))::INT) BETWEEN 1 AND 9223372036854775807), LOWER(((SELECT TRUE FROM tbl_03 AS sub_100 LIMIT 1) * NULLIF((t_ref_97.id = t_ref_97.col_1), t_ref_97.col_2))))))), (ABS((CASE WHEN (SELECT (LENGTH((NOT (NULLIF(((EXISTS (SELECT 1 FROM tbl_01 AS sub_102 WHERE NULL)::TEXT::VARCHAR(32)) <= ((NULLIF((COALESCE(sub_101.col_4, sub_101.col_3) + 3.14159265), sub_101.col_1)::INT::TEXT)::INT::TEXT)), ((NULL * (SELECT (SELECT inn_104.col_3 FROM tbl_01 AS inn_104 LIMIT 1) FROM tbl_05 AS sub_103 LIMIT 1)) IS NULL))))) < ((CASE WHEN ABS((sub_101.id::INT)) THEN 'escaped''quote' ELSE (((CASE WHEN (sub_101.col_3 IN (SELECT sub_105.col_3 FROM tbl_06 AS sub_105)) THEN COALESCE(sub_101.col_3, ((sub_101.id = TRUE) * sub_101.col_3)) ELSE UPPER(-2147483648) END) * (UPPER(((FALSE / NULLIF((SELECT (SELECT 'ref_val' FROM corpus_articles AS inn_107 LIMIT 1) FROM corpus_articles AS sub_106 LIMIT 1), 0)) < EXISTS (SELECT 1 FROM corpus_articles AS sub_108 WHERE sub_108.payload)))::TEXT::VARCHAR(32))) * sub_101.col_4) END) * (CASE WHEN EXISTS (SELECT 1 FROM tbl_01 AS sub_109 WHERE sub_109.col_1) THEN EXISTS (SELECT 1 FROM tbl_04 AS sub_114 WHERE 2147483647) ELSE LENGTH(((LOWER((ABS(ABS(NULL)) * ((CASE WHEN sub_101.col_4 THEN FALSE ELSE sub_101.col_3 END)::TEXT::VARCHAR(32)))) - ABS((((sub_101.col_2 + TRUE) > (FALSE + sub_101.col_2)) BETWEEN -1 AND -9223372036854775808)))::NUMERIC(10,2))) END))) FROM tbl_03 AS sub_101 LIMIT 1) THEN (CASE WHEN ((UPPER((((CASE WHEN ((SELECT 'chaos' FROM corpus_articles AS sub_117 LIMIT 1)::NUMERIC(10,2)) THEN ((((SELECT (SELECT inn_119.col_3 FROM tbl_03 AS inn_119 LIMIT 1) FROM tbl_06 AS sub_118 LIMIT 1) + LOWER(EXISTS (SELECT 1 FROM tbl_04 AS sub_120 WHERE TRUE))) - (((SELECT sub_121.id FROM tbl_01 AS sub_121 LIMIT 1) IS NULL) <> ABS(COALESCE(t_ref_97.col_2, t_ref_97.col_2)))) >= (FALSE IN (SELECT sub_122.col_2 FROM tbl_03 AS sub_122))) ELSE EXISTS (SELECT 1 FROM tbl_06 AS sub_123 WHERE sub_123.id) END) + (NOT (LOWER((TRUE - ((SELECT (SELECT inn_126.id FROM tbl_02 AS inn_126 LIMIT 1) FROM tbl_04 AS sub_125 LIMIT 1) = t_ref_97.col_1)))))) > UPPER((CASE WHEN EXISTS (SELECT 1 FROM tbl_01 AS sub_127 WHERE 42) THEN ((UPPER((1.0 IN (SELECT sub_129.col_3 FROM tbl_01 AS sub_129))) > (NULLIF(t_ref_97.col_2, (CASE WHEN t_ref_97.col_1 THEN t_ref_97.col_2 ELSE t_ref_97.col_1 END)) = 'ref_val')) IS NULL) ELSE (SELECT (SELECT inn_131.col_2 FROM tbl_05 AS inn_131 LIMIT 1) FROM corpus_articles AS sub_130 LIMIT 1) END)))) - t_ref_97.col_2) / NULLIF((NULL IN (SELECT sub_132.col_3 FROM tbl_01 AS sub_132)), 0)) THEN (t_ref_97.col_3 = ((UPPER(t_ref_97.col_1) AND (t_ref_97.col_2 IS NOT NULL)) + (CASE WHEN LOWER((0.0000000001 * t_ref_97.col_1)) THEN t_ref_97.col_1 ELSE ABS(((t_ref_97.col_3 + ('chaos' + t_ref_97.col_2))::INT)) END))) ELSE t_ref_97.col_2 END) ELSE (t_ref_97.col_3 IN (SELECT sub_134.col_1 FROM tbl_06 AS sub_134)) END)) IS NULL)))
LIMIT 3;

UPDATE tbl_01 SET col_1 = '{}' WHERE id > 70;

CREATE VIEW v_summary_2 AS SELECT id FROM corpus_articles WHERE id IS NOT NULL;

SELECT
    LOWER((LENGTH((('ref_val' / NULLIF((1 - (-1e-5::BOOLEAN)), 0)) <> EXISTS (SELECT 1 FROM tbl_06 AS sub_147 WHERE sub_147.col_3)))::INT)) AS c_148,
    t_ref_146.id AS c_149
FROM tbl_02 AS t_ref_146 LEFT JOIN tbl_03 AS j_ref_150 ON t_ref_146.id = j_ref_150.id
WHERE FALSE
LIMIT 19;

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (74929, 'escaped''quote', 999999999.99);
COMMIT;

CREATE INDEX idx_corpus_articles_tsv ON corpus_articles USING gin (tsv_content);

UPDATE tbl_02 SET col_3 = FALSE WHERE id > 81;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE INDEX idx_tbl_04_id_5 ON tbl_04 (id);

CREATE INDEX idx_tbl_03_col_1_6 ON tbl_03 (col_1);

SELECT
    t_ref_151.col_1 AS c_152,
    (TRUE OR ((SELECT (3.14159265 BETWEEN 1 AND 2147483647) FROM tbl_06 AS sub_153 LIMIT 1) IS NOT NULL)) AS c_154
FROM tbl_05 AS t_ref_151 LEFT JOIN tbl_04 AS j_ref_155 ON t_ref_151.id = j_ref_155.id
WHERE ((((NOT ((t_ref_151.id IN (SELECT sub_156.id FROM tbl_02 AS sub_156)))) IS NOT NULL)::INT::TEXT)::TEXT)
LIMIT 2;

SELECT
    t_ref_157.id AS c_158
FROM tbl_03 AS t_ref_157
WHERE 0.0000000001
LIMIT 49;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (43945, '{}', TRUE, TRUE, 0.0000000001);

INSERT INTO corpus_articles (id, payload) VALUES (607587, '');

DELETE FROM tbl_07 WHERE id = 27286;

UPDATE tbl_06 SET col_1 = TRUE WHERE id > 84;

UPDATE tbl_05 SET col_1 = 'chaos' WHERE id > 44;

UPDATE tbl_01 SET col_1 = '[]' WHERE id > 92;

SELECT
    (((t_ref_159.col_1 > (CASE WHEN t_ref_159.id THEN t_ref_159.col_1 ELSE NULLIF(t_ref_159.id, FALSE) END)) + (EXISTS (SELECT 1 FROM tbl_07 AS sub_160 WHERE NULL) = (((CASE WHEN ((NOT (COALESCE((SELECT (SELECT 2147483647 FROM tbl_01 AS inn_162 LIMIT 1) FROM tbl_02 AS sub_161 LIMIT 1), (SELECT UPPER(((SELECT (CASE WHEN sub_164.col_1 THEN sub_164.col_4 ELSE (sub_164.col_2::TEXT::VARCHAR(32)) END) FROM tbl_03 AS sub_164 LIMIT 1) BETWEEN -1 AND 100)) FROM corpus_articles AS sub_163 LIMIT 1))))::INT::TEXT) THEN (t_ref_159.id = NULLIF((LOWER(FALSE) - (SELECT ((NOT ((CASE WHEN (LOWER(sub_165.id)::BOOLEAN) THEN (((sub_165.id < FALSE) - (CASE WHEN sub_165.col_1 THEN -1 ELSE TRUE END)) - (ABS(0)::NUMERIC(10,2))) ELSE ABS(EXISTS (SELECT 1 FROM tbl_04 AS sub_166 WHERE FALSE)) END))) / NULLIF(sub_165.col_1, 0)) FROM tbl_05 AS sub_165 LIMIT 1)), ((NOT ((ABS(NULL) AND ((((3.14159265 / NULLIF(((FALSE::TEXT::VARCHAR(32)) IS NOT NULL), 0)) >= (UPPER(COALESCE(t_ref_159.col_1, NULL)) IS NULL)) <> (t_ref_159.col_1::NUMERIC(10,2))) * EXISTS (SELECT 1 FROM tbl_01 AS sub_167 WHERE sub_167.col_3))))) + 1.0))) ELSE (LOWER(((t_ref_159.col_2 + (NOT (t_ref_159.col_1))) - ((EXISTS (SELECT 1 FROM tbl_01 AS sub_168 WHERE 0.0) > (NULL <= (((SELECT (999999999.99::INT) FROM tbl_02 AS sub_170 LIMIT 1) * ((EXISTS (SELECT 1 FROM tbl_03 AS sub_171 WHERE TRUE) = COALESCE(t_ref_159.id, t_ref_159.col_2)) BETWEEN 42 AND -2147483648)) - (NOT ((t_ref_159.col_2::TEXT::VARCHAR(32))))))) - t_ref_159.col_2))) AND t_ref_159.col_1) END) - (EXISTS (SELECT 1 FROM corpus_articles AS sub_172 WHERE sub_172.id) - t_ref_159.id)) * t_ref_159.col_1))) / NULLIF((LENGTH(t_ref_159.col_1)::INT::TEXT), 0)) AS c_174,
    (NULLIF(((t_ref_159.id IN (SELECT sub_175.id FROM tbl_03 AS sub_175)) <= t_ref_159.col_1), NULL) - ((SELECT (LENGTH((SELECT (SELECT inn_206.col_2 FROM tbl_07 AS inn_206 LIMIT 1) FROM tbl_05 AS sub_199 LIMIT 1)) + (FALSE BETWEEN -9223372036854775808 AND 0)) FROM tbl_07 AS sub_198 LIMIT 1)::INT::TEXT)) AS c_207,
    (((t_ref_159.col_1 IN (SELECT sub_208.col_3 FROM tbl_02 AS sub_208)) < ((SELECT (SELECT inn_216.id FROM tbl_01 AS inn_216 LIMIT 1) FROM corpus_articles AS sub_215 LIMIT 1) / NULLIF((ABS((t_ref_159.col_1 + (SELECT (SELECT inn_227.col_2 FROM tbl_02 AS inn_227 LIMIT 1) FROM tbl_01 AS sub_217 LIMIT 1))) - (((((2147483647 IN (SELECT sub_228.col_2 FROM tbl_05 AS sub_228)) AND (((ABS((NULLIF((((t_ref_159.col_2::TEXT)::INT) < (LOWER(2147483647)::TEXT::VARCHAR(32))), (SELECT sub_240.col_1 FROM tbl_03 AS sub_240 LIMIT 1)) / NULLIF(FALSE, 0))) >= ((CASE WHEN LOWER((SELECT (SELECT inn_242.col_3 FROM tbl_01 AS inn_242 LIMIT 1) FROM tbl_05 AS sub_241 LIMIT 1)) THEN LENGTH(((t_ref_159.id IN (SELECT sub_243.payload FROM corpus_articles AS sub_243)) / NULLIF(ABS(0.0), 0))) ELSE (t_ref_159.id / NULLIF(((NULL * (SELECT (SELECT inn_245.col_1 FROM tbl_04 AS inn_245 LIMIT 1) FROM tbl_01 AS sub_244 LIMIT 1))::TEXT), 0)) END) < (((((t_ref_159.id::BOOLEAN) BETWEEN 100 AND -9223372036854775808) / NULLIF((EXISTS (SELECT 1 FROM tbl_06 AS sub_246 WHERE sub_246.col_4) * (CASE WHEN t_ref_159.id THEN 0.0 ELSE t_ref_159.id END)), 0)) > (FALSE > (CASE WHEN (SELECT 42 FROM tbl_06 AS sub_247 LIMIT 1) THEN (t_ref_159.id = t_ref_159.id) ELSE t_ref_159.col_1 END)))::TEXT))) <= ((((((CASE WHEN LENGTH(t_ref_159.col_2) THEN (TRUE > t_ref_159.col_2) ELSE (t_ref_159.col_1 - -1e-5) END) < ((t_ref_159.col_2 + t_ref_159.col_2) + EXISTS (SELECT 1 FROM tbl_01 AS sub_248 WHERE sub_248.col_3))) + (SELECT (SELECT inn_251.col_1 FROM tbl_03 AS inn_251 LIMIT 1) FROM tbl_07 AS sub_249 LIMIT 1)) / NULLIF(TRUE, 0)) < (t_ref_159.col_2::TEXT::VARCHAR(32))) + (SELECT EXISTS (SELECT 1 FROM tbl_04 AS sub_253 WHERE sub_253.id) FROM tbl_07 AS sub_252 LIMIT 1))) * ((UPPER((SELECT (SELECT inn_256.col_3 FROM tbl_03 AS inn_256 LIMIT 1) FROM tbl_05 AS sub_255 LIMIT 1))::INT::TEXT) = (SELECT (SELECT inn_258.id FROM tbl_05 AS inn_258 LIMIT 1) FROM tbl_05 AS sub_257 LIMIT 1))))::INT)::TEXT) - (((((SELECT (SELECT inn_260.id FROM tbl_05 AS inn_260 LIMIT 1) FROM tbl_04 AS sub_259 LIMIT 1) > ('' - ABS((EXISTS (SELECT 1 FROM corpus_articles AS sub_261 WHERE TRUE) IS NULL))))::INT) * (1 / NULLIF(TRUE, 0))) - ((UPPER((t_ref_159.col_1 + (CASE WHEN (((SELECT (SELECT 2147483647 FROM tbl_01 AS inn_264 LIMIT 1) FROM tbl_04 AS sub_262 LIMIT 1)::INT) <> ((CASE WHEN (((SELECT sub_265.payload FROM corpus_articles AS sub_265 LIMIT 1) >= (t_ref_159.id - t_ref_159.col_1)) >= LOWER((999999999.99 + t_ref_159.col_1))) THEN (((t_ref_159.id + t_ref_159.col_1)::NUMERIC(10,2)) = (SELECT (SELECT TRUE FROM corpus_articles AS inn_267 LIMIT 1) FROM tbl_02 AS sub_266 LIMIT 1)) ELSE COALESCE((t_ref_159.id + t_ref_159.col_1), (t_ref_159.col_1::INT)) END) IS NULL)) THEN ((CASE WHEN (SELECT sub_268.col_4 FROM tbl_06 AS sub_268 LIMIT 1) THEN (t_ref_159.col_2 IN (SELECT sub_269.col_1 FROM tbl_03 AS sub_269)) ELSE ((((t_ref_159.col_2 BETWEEN 9223372036854775807 AND -1) * 'alpha_beta_gamma')::TEXT::VARCHAR(32)) + t_ref_159.id) END) >= LENGTH(LENGTH(t_ref_159.col_1))) ELSE t_ref_159.col_1 END)))::TEXT) - (SELECT (SELECT inn_272.id FROM tbl_04 AS inn_272 LIMIT 1) FROM tbl_06 AS sub_271 LIMIT 1))))), 0))) / NULLIF(EXISTS (SELECT 1 FROM tbl_01 AS sub_273 WHERE sub_273.id), 0)) AS c_296
FROM tbl_05 AS t_ref_159
WHERE UPPER((LOWER(t_ref_159.col_1)::TEXT))
LIMIT 40;

UPDATE corpus_articles SET payload = 'alpha_beta_gamma' WHERE id > 40;

DELETE FROM tbl_06 WHERE id = 20032;

SELECT
    NULL AS c_298,
    COALESCE(t_ref_297.col_3, (TRUE IN (SELECT sub_299.id FROM tbl_03 AS sub_299))) AS c_324,
    (LOWER(NULLIF(t_ref_297.col_2, ((NULL IN (SELECT sub_325.col_2 FROM tbl_01 AS sub_325)) < ((SELECT sub_332.id FROM tbl_06 AS sub_332 LIMIT 1) - (t_ref_297.col_1 / NULLIF(UPPER(EXISTS (SELECT 1 FROM corpus_articles AS sub_333 WHERE sub_333.payload)), 0)))))) * t_ref_297.col_3) AS c_336
FROM tbl_01 AS t_ref_297 LEFT JOIN tbl_02 AS j_ref_337 ON t_ref_297.id = j_ref_337.id
WHERE (CASE WHEN (CASE WHEN (0.0000000001 IN (SELECT sub_338.id FROM tbl_03 AS sub_338)) THEN (t_ref_297.col_2 IN (SELECT sub_354.col_4 FROM tbl_02 AS sub_354)) ELSE ((SELECT (SELECT inn_366.id FROM tbl_05 AS inn_366 LIMIT 1) FROM tbl_07 AS sub_357 LIMIT 1) * (EXISTS (SELECT 1 FROM tbl_03 AS sub_367 WHERE TRUE) >= (SELECT (SELECT inn_377.col_2 FROM tbl_01 AS inn_377 LIMIT 1) FROM tbl_02 AS sub_376 LIMIT 1))) END) THEN t_ref_297.id ELSE t_ref_297.col_2 END)
LIMIT 48;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 BOOLEAN,
    col_3 INT,
    col_4 NUMERIC(10,2)
);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (861122, '[]', TRUE, FALSE, 1e10);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (337547, '{}', TRUE, TRUE, 1e10);

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, dyn_col_4) VALUES (236738, FALSE, 'alpha_beta_gamma', '__fuzz_marker__');
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (114655, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'ref_val', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'chaos');
INSERT INTO tbl_04 (id, col_1, col_2, dyn_col_4) VALUES (952901, FALSE, 'ref_val', '');
COMMIT;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 VARCHAR(64),
    ref_tbl_08_id INT,
    FOREIGN KEY (ref_tbl_08_id) REFERENCES tbl_08(id)
);

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_09 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (870112, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '__fuzz_marker__', -1.5);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (751138, '[]', 'ref_val', '[]', 'alpha_beta_gamma');
INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (458844, '{}', 'alpha_beta_gamma', -9223372036854775808);
COMMIT;

SELECT
    (((999999999.99 * (NOT (t_ref_378.id)))::INT) <= (((COALESCE(0.0, NULL) AND EXISTS (SELECT 1 FROM tbl_03 AS sub_379 WHERE 'alpha_beta_gamma')) <> (t_ref_378.ref_tbl_01_id IN (SELECT sub_380.col_1 FROM tbl_08 AS sub_380))) IS NOT NULL)) AS c_405
FROM tbl_07 AS t_ref_378
WHERE t_ref_378.col_1
LIMIT 11;

SELECT
    (((t_ref_406.col_1 IN (SELECT sub_407.col_1 FROM tbl_02 AS sub_407)) <= 'escaped''quote') + t_ref_406.col_3) AS c_408,
    (t_ref_406.id IN (SELECT sub_409.col_4 FROM tbl_02 AS sub_409)) AS c_410,
    ((FALSE / NULLIF(((((NULL IN (SELECT sub_411.col_1 FROM tbl_04 AS sub_411)) >= 100) + ((CASE WHEN ((UPPER(t_ref_406.col_1)::TEXT) <> (t_ref_406.col_4 IN (SELECT sub_423.ref_tbl_01_id FROM tbl_07 AS sub_423))) THEN ((NOT ((EXISTS (SELECT 1 FROM tbl_09 AS sub_424 WHERE 'alpha_beta_gamma') AND ((42 IN (SELECT sub_428.col_3 FROM tbl_07 AS sub_428)) OR EXISTS (SELECT 1 FROM tbl_05 AS sub_431 WHERE sub_431.id))))) > (t_ref_406.id IN (SELECT sub_432.ref_tbl_01_id FROM tbl_07 AS sub_432))) ELSE (SELECT (SELECT inn_442.id FROM tbl_03 AS inn_442 LIMIT 1) FROM tbl_01 AS sub_435 LIMIT 1) END) - ((((LENGTH((-1 IN (SELECT sub_443.id FROM corpus_articles AS sub_443))) / NULLIF(t_ref_406.col_3, 0)) BETWEEN -1 AND 9223372036854775807) / NULLIF(('alpha_beta_gamma' + ((t_ref_406.col_1 <> ((NOT ((EXISTS (SELECT 1 FROM tbl_08 AS sub_447 WHERE -9223372036854775808)::BOOLEAN))) - (CASE WHEN (((COALESCE(TRUE, (t_ref_406.col_1 AND t_ref_406.col_2))::TEXT::VARCHAR(32)) OR ((EXISTS (SELECT 1 FROM tbl_08 AS sub_449 WHERE sub_449.id) AND (CASE WHEN t_ref_406.col_2 THEN 0 ELSE t_ref_406.col_2 END)) * t_ref_406.col_2)) BETWEEN 9223372036854775807 AND 2147483647) THEN ((t_ref_406.col_4::INT) <= (EXISTS (SELECT 1 FROM tbl_04 AS sub_450 WHERE TRUE) IS NOT NULL)) ELSE (NOT (COALESCE(((t_ref_406.col_1 IS NULL) / NULLIF((SELECT (SELECT 100 FROM tbl_06 AS inn_452 LIMIT 1) FROM tbl_04 AS sub_451 LIMIT 1), 0)), UPPER((CASE WHEN t_ref_406.id THEN t_ref_406.col_3 ELSE COALESCE(-2147483648, FALSE) END))))) END)))::NUMERIC(10,2))), 0)) <> t_ref_406.id))) + UPPER((t_ref_406.id IN (SELECT sub_453.id FROM tbl_04 AS sub_453)))), 0))::TEXT::VARCHAR(32)) AS c_454
FROM tbl_06 AS t_ref_406 LEFT JOIN tbl_08 AS j_ref_455 ON t_ref_406.id = j_ref_455.id
WHERE EXISTS (SELECT 1 FROM tbl_04 AS sub_456 WHERE sub_456.dyn_col_4)
LIMIT 46;

UPDATE tbl_08 SET col_4 = 999999999.99 WHERE id > 78;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (711593, TRUE, -1.5, '', 9223372036854775807);

INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (364767, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '__fuzz_marker__', 2147483647);

UPDATE tbl_03 SET col_1 = '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}' WHERE id > 38;

INSERT INTO corpus_articles (doc_id, title, tsv_content) VALUES
    (1, 'Database internals', to_tsvector('english', 'relational database query optimization execution')),
    (2, 'Empty placeholder', ''::tsvector),
    (3, 'Weighted search', setweight(to_tsvector('english', 'kernel'), 'A') || setweight(to_tsvector('english', 'memory'), 'D')),
    (4, 'Mixed empty weights', setweight(''::tsvector, 'B') || to_tsvector('english', 'system fault crash'));

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (575778, -9223372036854775808, TRUE, 0, -1e-5);
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (612520, 'alpha_beta_gamma', 0.0);
COMMIT;

CREATE INDEX idx_tbl_04_id_7 ON tbl_04 (id);

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (998371, -9223372036854775808, TRUE, -2147483648, 999999999.99);
COMMIT;

SELECT
    ((((((SELECT sub_459.col_2 FROM tbl_05 AS sub_459 LIMIT 1) - t_ref_458.col_3) + t_ref_458.col_1) IS NOT NULL) * (((NULL IN (SELECT sub_460.col_1 FROM tbl_07 AS sub_460))::BOOLEAN)::BOOLEAN))::INT) AS c_461,
    NULLIF((CASE WHEN (LENGTH((((EXISTS (SELECT 1 FROM tbl_09 AS sub_462 WHERE sub_462.col_1)::TEXT::VARCHAR(32)) - (-9223372036854775808 IN (SELECT sub_466.col_4 FROM tbl_02 AS sub_466))) / NULLIF(COALESCE(t_ref_458.id, NULL), 0))) < ((SELECT (SELECT ABS(((CASE WHEN '' THEN (((EXISTS (SELECT 1 FROM tbl_02 AS sub_474 WHERE sub_474.col_1) > (NULLIF((SELECT (SELECT inn_476.col_1 FROM tbl_05 AS inn_476 LIMIT 1) FROM tbl_03 AS sub_475 LIMIT 1), ('escaped''quote' > sub_473.col_4)) + (NOT ((sub_473.col_1 > '__fuzz_marker__'))))) / NULLIF((sub_473.col_3 - 1.0), 0)) * (NULLIF((EXISTS (SELECT 1 FROM tbl_07 AS sub_477 WHERE sub_477.col_1) >= sub_473.col_1), (100 IN (SELECT sub_478.col_4 FROM tbl_02 AS sub_478)))::INT)) ELSE ((CASE WHEN EXISTS (SELECT 1 FROM tbl_07 AS sub_479 WHERE sub_479.col_3) THEN -1e-5 ELSE ((((sub_473.col_2 - -1)::BOOLEAN)::NUMERIC(10,2)) > (sub_473.col_3::INT::TEXT)) END) / NULLIF((LENGTH((((9223372036854775807 >= sub_473.col_3) + sub_473.id)::INT)) BETWEEN 0 AND 2147483647), 0)) END)::BOOLEAN)) FROM tbl_02 AS sub_473 LIMIT 1) FROM tbl_06 AS sub_472 LIMIT 1) * (NULL IN (SELECT sub_480.id FROM corpus_articles AS sub_480)))) THEN (FALSE OR NULLIF((100::TEXT::VARCHAR(32)), t_ref_458.id)) ELSE (SELECT (SELECT inn_493.col_1 FROM tbl_09 AS inn_493 LIMIT 1) FROM tbl_09 AS sub_489 LIMIT 1) END), UPPER((t_ref_458.col_1 IN (SELECT sub_494.id FROM corpus_articles AS sub_494)))) AS c_495,
    2147483647 AS c_496
FROM tbl_01 AS t_ref_458
WHERE -1
LIMIT 3;

SELECT
    (t_ref_497.col_4 OR t_ref_497.col_3) AS c_498,
    (((CASE WHEN ((SELECT (SELECT inn_505.col_2 FROM tbl_07 AS inn_505 LIMIT 1) FROM tbl_07 AS sub_499 LIMIT 1)::NUMERIC(10,2)) THEN t_ref_497.col_3 ELSE t_ref_497.id END) + (UPPER((SELECT (SELECT TRUE FROM tbl_07 AS inn_517 LIMIT 1) FROM tbl_02 AS sub_506 LIMIT 1)) >= EXISTS (SELECT 1 FROM corpus_articles AS sub_518 WHERE sub_518.payload))) < ((((SELECT ABS((sub_531.col_2 IN (SELECT sub_532.col_1 FROM tbl_01 AS sub_532))) FROM tbl_08 AS sub_531 LIMIT 1) * TRUE) * (t_ref_497.col_3 BETWEEN -1 AND 9223372036854775807)) + ((t_ref_497.col_3 / NULLIF(((CASE WHEN LENGTH((t_ref_497.id IN (SELECT sub_537.col_3 FROM tbl_02 AS sub_537))) THEN (t_ref_497.col_3 * EXISTS (SELECT 1 FROM tbl_05 AS sub_538 WHERE sub_538.col_2)) ELSE (((TRUE IN (SELECT sub_539.id FROM tbl_02 AS sub_539)) / NULLIF((t_ref_497.col_4 * t_ref_497.col_3), 0)) + (((((t_ref_497.id * t_ref_497.col_3)::TEXT) OR FALSE) / NULLIF(t_ref_497.col_3, 0)) - (SELECT (SELECT TRUE FROM tbl_09 AS inn_548 LIMIT 1) FROM tbl_02 AS sub_540 LIMIT 1))) END) IS NULL), 0)) / NULLIF(NULL, 0)))) AS c_549
FROM tbl_06 AS t_ref_497
WHERE t_ref_497.col_1
LIMIT 17;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (944604, TRUE, 0.0000000001, 'chaos', 2147483647);

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (535815, 42, FALSE, 42, 3.14159265);

ALTER TABLE tbl_04 DROP COLUMN col_1;

INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (914704, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'alpha_beta_gamma', 100);

BEGIN;
INSERT INTO corpus_articles (id, payload) VALUES (875454, '__fuzz_marker__');
COMMIT;

INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (418277, '{}', '', 9223372036854775807);

SELECT
    EXISTS (SELECT 1 FROM tbl_01 AS sub_551 WHERE sub_551.id) AS c_577,
    (LENGTH(((((UPPER(LOWER(((((CASE WHEN TRUE THEN (((SELECT (SELECT TRUE FROM tbl_02 AS inn_579 LIMIT 1) FROM tbl_06 AS sub_578 LIMIT 1) OR (((SELECT (SELECT 0.0 FROM tbl_06 AS inn_581 LIMIT 1) FROM tbl_01 AS sub_580 LIMIT 1) BETWEEN -1 AND -1)::TEXT::VARCHAR(32))) >= NULL) ELSE (t_ref_550.col_3 >= t_ref_550.col_1) END) AND (((t_ref_550.col_3 > (SELECT (SELECT inn_583.col_3 FROM tbl_06 AS inn_583 LIMIT 1) FROM tbl_07 AS sub_582 LIMIT 1)) <> NULLIF(NULLIF(NULL, t_ref_550.col_3), (CASE WHEN NULLIF((t_ref_550.col_2::INT::TEXT), (NULL OR 'alpha_beta_gamma')) THEN t_ref_550.col_2 ELSE (CASE WHEN t_ref_550.col_4 THEN COALESCE(t_ref_550.col_3, t_ref_550.col_3) ELSE (TRUE::TEXT::VARCHAR(32)) END) END))) BETWEEN -1 AND 0)) OR t_ref_550.id) < (CASE WHEN (SELECT (sub_584.col_4 / NULLIF(COALESCE(UPPER(((sub_584.col_3 OR sub_584.id)::TEXT)), sub_584.id), 0)) FROM tbl_02 AS sub_584 LIMIT 1) THEN TRUE ELSE t_ref_550.col_1 END)))) * ((TRUE < ABS(t_ref_550.col_3)) OR ((1.0 IN (SELECT sub_585.col_3 FROM tbl_01 AS sub_585)) / NULLIF(TRUE, 0)))) = (((NULLIF(((t_ref_550.col_1 * (UPPER((((t_ref_550.col_1 OR (t_ref_550.col_2 - t_ref_550.id))::BOOLEAN) * LENGTH(t_ref_550.col_3))) > t_ref_550.col_2)) - 2147483647), (FALSE IN (SELECT sub_589.col_3 FROM tbl_03 AS sub_589)))::INT) * ((t_ref_550.col_3 / NULLIF(FALSE, 0)) + ((((((FALSE - (LENGTH((2147483647 / NULLIF(9223372036854775807, 0))) * TRUE)) - FALSE) AND (t_ref_550.col_1 IN (SELECT sub_590.ref_tbl_08_id FROM tbl_09 AS sub_590))) >= 'alpha_beta_gamma') < ((EXISTS (SELECT 1 FROM tbl_08 AS sub_595 WHERE TRUE) >= (SELECT (sub_598.id <= ((9223372036854775807 - TRUE) - sub_598.payload)) FROM corpus_articles AS sub_598 LIMIT 1))::TEXT)) / NULLIF(NULLIF((t_ref_550.col_1 + ABS(EXISTS (SELECT 1 FROM tbl_01 AS sub_599 WHERE sub_599.col_1))), t_ref_550.id), 0)))) IS NULL)) IS NOT NULL) = t_ref_550.col_1)) >= (SELECT (CASE WHEN (((2147483647::TEXT::VARCHAR(32)) - ABS((NOT ((((9223372036854775807 IN (SELECT sub_601.col_4 FROM tbl_03 AS sub_601)) * sub_600.id) - (0 IN (SELECT sub_607.col_1 FROM tbl_02 AS sub_607))))))) > (SELECT sub_608.id FROM tbl_03 AS sub_608 LIMIT 1)) THEN (sub_600.col_4 <= FALSE) ELSE ((NULLIF(sub_600.col_3, (CASE WHEN EXISTS (SELECT 1 FROM tbl_08 AS sub_609 WHERE -9223372036854775808) THEN sub_600.col_1 ELSE ((sub_600.col_1 < (SELECT ('__fuzz_marker__' <= (LOWER(NULL) / NULLIF(sub_612.col_3, 0))) FROM tbl_01 AS sub_612 LIMIT 1)) * LENGTH(('alpha_beta_gamma' - LOWER((sub_600.col_4 + ((0::NUMERIC(10,2)) + (sub_600.col_2 IN (SELECT sub_613.col_2 FROM tbl_03 AS sub_613)))))))) END)) = (((NOT ((sub_600.col_1 IN (SELECT sub_614.col_3 FROM tbl_03 AS sub_614)))) * LOWER((SELECT sub_620.col_2 FROM tbl_05 AS sub_620 LIMIT 1)))::INT::TEXT)) / NULLIF((sub_600.col_2 BETWEEN 1 AND 9223372036854775807), 0)) END) FROM tbl_03 AS sub_600 LIMIT 1)) AS c_621,
    (((CASE WHEN TRUE THEN ((t_ref_550.col_2 IN (SELECT sub_622.id FROM tbl_01 AS sub_622)) <= (NULLIF(t_ref_550.col_2, (((((t_ref_550.col_1 IN (SELECT sub_623.id FROM tbl_09 AS sub_623)) OR NULL) / NULLIF((EXISTS (SELECT 1 FROM tbl_01 AS sub_624 WHERE FALSE) OR (t_ref_550.col_3 * ((((t_ref_550.col_2 IN (SELECT sub_625.ref_tbl_08_id FROM tbl_09 AS sub_625)) - LENGTH((t_ref_550.col_2::INT))) IS NULL)::TEXT))), 0)) + (t_ref_550.col_4 IN (SELECT sub_626.dyn_col_4 FROM tbl_04 AS sub_626))) * (SELECT (SELECT 1 FROM tbl_01 AS inn_631 LIMIT 1) FROM tbl_01 AS sub_630 LIMIT 1))) - ((SELECT (SELECT inn_638.id FROM tbl_09 AS inn_638 LIMIT 1) FROM tbl_08 AS sub_632 LIMIT 1) >= (LOWER(NULL)::TEXT)))) ELSE (LENGTH(((SELECT (((CASE WHEN (EXISTS (SELECT 1 FROM tbl_04 AS sub_640 WHERE 'ref_val') <> ((((CASE WHEN (NULL + NULL) THEN (CASE WHEN 1 THEN sub_639.id ELSE sub_639.col_4 END) ELSE FALSE END) * (SELECT (SELECT inn_644.col_3 FROM tbl_03 AS inn_644 LIMIT 1) FROM tbl_01 AS sub_643 LIMIT 1)) - sub_639.col_2)::BOOLEAN)) THEN (sub_639.col_2 IN (SELECT sub_645.id FROM tbl_09 AS sub_645)) ELSE sub_639.col_4 END) = NULLIF((((LOWER((sub_639.col_4 IN (SELECT sub_646.id FROM tbl_05 AS sub_646))) + (CASE WHEN ((-1e-5 BETWEEN 100 AND 2147483647) - LENGTH(sub_639.col_1)) THEN ((CASE WHEN 1.0 THEN FALSE ELSE -2147483648 END) / NULLIF(UPPER(-1e-5), 0)) ELSE (EXISTS (SELECT 1 FROM corpus_articles AS sub_647 WHERE sub_647.id)::BOOLEAN) END))::TEXT) + sub_639.col_3), ''))::INT) FROM tbl_03 AS sub_639 LIMIT 1) * UPPER(t_ref_550.col_4)))::INT) END)::BOOLEAN) * ((LOWER((NOT (NULL))) + (((TRUE::NUMERIC(10,2)) - NULLIF(((EXISTS (SELECT 1 FROM tbl_06 AS sub_648 WHERE 'ref_val') / NULLIF(t_ref_550.col_1, 0)) * NULLIF(9223372036854775807, (CASE WHEN (SELECT (SELECT inn_652.ref_tbl_08_id FROM tbl_09 AS inn_652 LIMIT 1) FROM tbl_07 AS sub_650 LIMIT 1) THEN ((t_ref_550.col_2 / NULLIF((SELECT ((NOT ((LENGTH(sub_653.payload)::INT))) <= (LOWER((CASE WHEN sub_653.id THEN FALSE ELSE sub_653.payload END)) / NULLIF((TRUE::TEXT), 0))) FROM corpus_articles AS sub_653 LIMIT 1), 0)) * 0.0) ELSE t_ref_550.id END))), t_ref_550.col_1)) - (ABS(((TRUE BETWEEN 0 AND -2147483648)::TEXT::VARCHAR(32))) * EXISTS (SELECT 1 FROM tbl_06 AS sub_654 WHERE sub_654.col_4)))) >= (t_ref_550.col_1 IN (SELECT sub_655.col_1 FROM tbl_09 AS sub_655)))) AS c_656
FROM tbl_02 AS t_ref_550 LEFT JOIN tbl_06 AS j_ref_657 ON t_ref_550.id = j_ref_657.id
WHERE FALSE
LIMIT 24;

ALTER TABLE tbl_03 DROP COLUMN col_3;

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 BOOLEAN,
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (435493, 9223372036854775807, TRUE, -9223372036854775808, 1.0);

UPDATE tbl_03 SET col_1 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 29;

SELECT
    t_ref_658.id AS c_659
FROM tbl_07 AS t_ref_658
WHERE (CASE WHEN ((t_ref_658.col_1 + (UPPER(t_ref_658.col_2) - (((SELECT (EXISTS (SELECT 1 FROM tbl_08 AS sub_661 WHERE sub_661.col_4)::BOOLEAN) FROM corpus_articles AS sub_660 LIMIT 1) <> t_ref_658.col_3) OR t_ref_658.col_1))) - (((SELECT ((sub_662.id IN (SELECT sub_663.id FROM tbl_04 AS sub_663)) / NULLIF(sub_662.col_2, 0)) FROM tbl_05 AS sub_662 LIMIT 1) - (COALESCE((NULL + (((((t_ref_658.col_2 * ((((CASE WHEN (SELECT NULL FROM tbl_05 AS sub_673 LIMIT 1) THEN (SELECT FALSE FROM corpus_articles AS sub_674 LIMIT 1) ELSE NULLIF(1.0, 2147483647) END) - FALSE)::BOOLEAN) / NULLIF((SELECT 1 FROM tbl_08 AS sub_675 LIMIT 1), 0))) < t_ref_658.col_1) + (ABS((CASE WHEN t_ref_658.col_3 THEN '__fuzz_marker__' ELSE NULLIF(((SELECT NULL FROM tbl_01 AS sub_676 LIMIT 1) + (LENGTH(t_ref_658.col_3)::TEXT::VARCHAR(32))), (0.0000000001 IN (SELECT sub_677.col_4 FROM tbl_08 AS sub_677))) END)) - NULL)) / NULLIF(((LENGTH(EXISTS (SELECT 1 FROM tbl_10 AS sub_679 WHERE sub_679.id)) OR (t_ref_658.col_2 IN (SELECT sub_680.ref_tbl_04_id FROM tbl_10 AS sub_680))) IS NOT NULL), 0))::TEXT::VARCHAR(32))), ((FALSE::TEXT)::INT::TEXT)) * ((((((('ref_val'::TEXT::VARCHAR(32)) * ((SELECT (SELECT inn_686.id FROM tbl_10 AS inn_686 LIMIT 1) FROM tbl_09 AS sub_684 LIMIT 1) - (CASE WHEN LENGTH(t_ref_658.col_2) THEN t_ref_658.col_2 ELSE (LOWER(((t_ref_658.col_3 + t_ref_658.id)::INT::TEXT)) = ('chaos' - 999999999.99)) END))) - LOWER((CASE WHEN ((t_ref_658.ref_tbl_01_id - t_ref_658.col_1) + UPPER(999999999.99)) THEN LOWER(LENGTH((((SELECT sub_687.id FROM tbl_04 AS sub_687 LIMIT 1)::INT::TEXT)::INT))) ELSE LOWER(((t_ref_658.col_3 / NULLIF(((t_ref_658.id / NULLIF('', 0)) IS NULL), 0)) <= t_ref_658.col_1)) END))) = t_ref_658.col_3) OR (LOWER('chaos') + t_ref_658.col_1)) * (COALESCE((EXISTS (SELECT 1 FROM corpus_articles AS sub_688 WHERE TRUE)::TEXT), ('__fuzz_marker__' * COALESCE(LENGTH((((t_ref_658.col_3 <= t_ref_658.col_3) - LENGTH(((SELECT (SELECT FALSE FROM tbl_10 AS inn_691 LIMIT 1) FROM tbl_07 AS sub_690 LIMIT 1) >= (t_ref_658.id IS NOT NULL)))) + (COALESCE(NULL, t_ref_658.col_2) <> (SELECT (SELECT NULL FROM tbl_01 AS inn_693 LIMIT 1) FROM tbl_05 AS sub_692 LIMIT 1)))), t_ref_658.col_2))) - t_ref_658.id)) + (((t_ref_658.col_1 > ((SELECT (SELECT inn_697.col_1 FROM tbl_06 AS inn_697 LIMIT 1) FROM tbl_01 AS sub_694 LIMIT 1) * (((UPPER(t_ref_658.col_1) / NULLIF((SELECT (SELECT (SELECT inn_700.col_3 FROM tbl_08 AS inn_700 LIMIT 1) FROM tbl_08 AS sub_699 LIMIT 1) FROM tbl_04 AS sub_698 LIMIT 1), 0))::TEXT) * (EXISTS (SELECT 1 FROM tbl_02 AS sub_701 WHERE sub_701.col_3) = t_ref_658.col_3)))) + (t_ref_658.ref_tbl_01_id >= ((-1 IN (SELECT sub_703.id FROM tbl_01 AS sub_703)) = EXISTS (SELECT 1 FROM tbl_09 AS sub_705 WHERE sub_705.id)))) / NULLIF(((((SELECT (sub_706.ref_tbl_08_id + LOWER((FALSE / NULLIF(9223372036854775807, 0)))) FROM tbl_09 AS sub_706 LIMIT 1) / NULLIF(t_ref_658.col_1, 0)) + (t_ref_658.col_3 IN (SELECT sub_707.col_3 FROM tbl_07 AS sub_707))) * (SELECT (SELECT inn_711.col_1 FROM tbl_01 AS inn_711 LIMIT 1) FROM tbl_01 AS sub_710 LIMIT 1)), 0)))))::INT)) THEN (NULL::INT) ELSE NULL END)
LIMIT 19;

ALTER TABLE tbl_01 ADD COLUMN dyn_col_5 VARCHAR(32);

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    col_3 JSONB,
    ref_tbl_06_id INT,
    FOREIGN KEY (ref_tbl_06_id) REFERENCES tbl_06(id)
);

SELECT
    NULL AS c_713,
    (1::INT::TEXT) AS c_714
FROM tbl_09 AS t_ref_712 LEFT JOIN tbl_06 AS j_ref_715 ON t_ref_712.id = j_ref_715.id
WHERE (-9223372036854775808 AND EXISTS (SELECT 1 FROM tbl_03 AS sub_716 WHERE sub_716.col_1))
LIMIT 33;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (965600, FALSE, 1.0, '__fuzz_marker__', 100);

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 INT,
    col_3 JSONB
);

UPDATE tbl_03 SET col_4 = 'ref_val' WHERE id > 60;

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSONB,
    col_3 BOOLEAN
);

CREATE INDEX idx_tbl_10_col_1_8 ON tbl_10 (col_1);

SELECT
    (t_ref_728.id OR FALSE) AS c_729,
    LENGTH(((SELECT (SELECT inn_738.col_2 FROM tbl_13 AS inn_738 LIMIT 1) FROM corpus_articles AS sub_730 LIMIT 1) + FALSE)) AS c_739,
    (SELECT (SELECT inn_741.ref_tbl_08_id FROM tbl_09 AS inn_741 LIMIT 1) FROM tbl_11 AS sub_740 LIMIT 1) AS c_742
FROM tbl_05 AS t_ref_728 LEFT JOIN tbl_12 AS j_ref_743 ON t_ref_728.id = j_ref_743.id
WHERE NULLIF(-9223372036854775808, (CASE WHEN TRUE THEN ((t_ref_728.col_2 * (UPPER(NULL) / NULLIF(((t_ref_728.col_1 IN (SELECT sub_744.col_4 FROM tbl_08 AS sub_744))::INT), 0))) IS NOT NULL) ELSE TRUE END))
LIMIT 30;

INSERT INTO tbl_03 (id, col_1, col_2, col_4) VALUES (864909, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '', '');

UPDATE tbl_12 SET col_2 = 2147483647 WHERE id > 80;

SELECT
    (SELECT UPPER(((100 - NULL) IS NOT NULL)) FROM tbl_01 AS sub_749 LIMIT 1) AS c_750,
    EXISTS (SELECT 1 FROM corpus_articles AS sub_751 WHERE sub_751.payload) AS c_752
FROM tbl_01 AS t_ref_748 LEFT JOIN tbl_07 AS j_ref_753 ON t_ref_748.id = j_ref_753.id
WHERE NULLIF(((SELECT ((sub_754.col_1::NUMERIC(10,2)) > (sub_754.ref_tbl_04_id = (NOT ((sub_754.col_2::NUMERIC(10,2)))))) FROM tbl_10 AS sub_754 LIMIT 1) < EXISTS (SELECT 1 FROM tbl_12 AS sub_755 WHERE sub_755.col_2)), FALSE)
LIMIT 41;

DELETE FROM tbl_06 WHERE id = 62298;

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (351840, 'chaos', 0.0000000001);
COMMIT;

CREATE INDEX idx_tbl_06_col_1_9 ON tbl_06 (col_1);

INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (609737, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'chaos', 2147483647);

INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (586008, '[]', 'ref_val', 9223372036854775807);

CREATE INDEX idx_corpus_articles_payload_10 ON corpus_articles (payload);

UPDATE corpus_articles SET payload = '__fuzz_marker__' WHERE id > 9;

UPDATE tbl_13 SET col_2 = '[]' WHERE id > 18;

BEGIN;
INSERT INTO corpus_articles (id, payload) VALUES (496006, 'ref_val');
COMMIT;

CREATE INDEX idx_tbl_09_id_11 ON tbl_09 (id);

CREATE INDEX idx_tbl_08_col_4_12 ON tbl_08 (col_4);

CREATE INDEX idx_tbl_13_col_1_13 ON tbl_13 (col_1);

DELETE FROM tbl_11 WHERE id = 41778;

INSERT INTO tbl_03 (id, col_1, col_2, col_4) VALUES (141833, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'alpha_beta_gamma', '');

SELECT
    (NOT (FALSE)) AS c_769,
    (((((t_ref_768.id - t_ref_768.id) >= t_ref_768.id) / NULLIF(EXISTS (SELECT 1 FROM tbl_11 AS sub_770 WHERE sub_770.id), 0))::BOOLEAN) = (NOT (('__fuzz_marker__'::TEXT)))) AS c_774
FROM corpus_articles AS t_ref_768
WHERE ((SELECT (SELECT TRUE FROM tbl_07 AS inn_777 LIMIT 1) FROM tbl_03 AS sub_775 LIMIT 1) = ((SELECT (SELECT TRUE FROM tbl_04 AS inn_779 LIMIT 1) FROM tbl_09 AS sub_778 LIMIT 1)::BOOLEAN))
LIMIT 23;

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 BOOLEAN,
    col_4 VARCHAR(64)
);

DELETE FROM tbl_08 WHERE id = 85411;

DELETE FROM tbl_03 WHERE id = 34666;

CREATE INDEX idx_tbl_03_col_1_14 ON tbl_03 (col_1);

SELECT
    t_ref_780.id AS c_781,
    (ABS((FALSE + ((t_ref_780.id * ('' <> ((t_ref_780.col_3 OR (SELECT (CASE WHEN ABS(NULL) THEN (FALSE IN (SELECT sub_783.col_3 FROM tbl_01 AS sub_783)) ELSE sub_782.col_1 END) FROM tbl_02 AS sub_782 LIMIT 1)) / NULLIF(((ABS(FALSE)::INT) <> t_ref_780.col_1), 0)))) / NULLIF(UPPER(NULLIF((EXISTS (SELECT 1 FROM tbl_04 AS sub_784 WHERE -1e-5) > ((t_ref_780.col_3 + (((((NULL + t_ref_780.col_2) <= ((0.0::NUMERIC(10,2)) / NULLIF(ABS(1e10), 0))) / NULLIF(ABS(((NOT ((FALSE - EXISTS (SELECT 1 FROM tbl_10 AS sub_786 WHERE FALSE)))) AND t_ref_780.col_2)), 0))::TEXT)::TEXT::VARCHAR(32))) > (t_ref_780.col_1 IN (SELECT sub_787.col_4 FROM tbl_06 AS sub_787)))), UPPER((UPPER(t_ref_780.id) = t_ref_780.col_1)))), 0)))) <> (SELECT (SELECT inn_790.col_2 FROM tbl_10 AS inn_790 LIMIT 1) FROM tbl_04 AS sub_789 LIMIT 1)) AS c_791
FROM tbl_12 AS t_ref_780
WHERE (SELECT (SELECT inn_815.col_4 FROM tbl_08 AS inn_815 LIMIT 1) FROM tbl_14 AS sub_792 LIMIT 1)
LIMIT 37;

CREATE INDEX idx_tbl_11_id_15 ON tbl_11 (id);

INSERT INTO tbl_01 (id, col_1, col_2, col_3, dyn_col_5) VALUES (5591, '{}', 'ref_val', 1e10, 'chaos');

SELECT
    ABS(t_ref_816.id) AS c_817,
    (CASE WHEN (t_ref_816.ref_tbl_08_id::INT) THEN LOWER(t_ref_816.col_1) ELSE (CASE WHEN ((SELECT (SELECT inn_819.ref_tbl_01_id FROM tbl_07 AS inn_819 LIMIT 1) FROM tbl_14 AS sub_818 LIMIT 1)::NUMERIC(10,2)) THEN (t_ref_816.id IN (SELECT sub_820.col_1 FROM tbl_14 AS sub_820)) ELSE 'alpha_beta_gamma' END) END) AS c_822,
    (CASE WHEN (((((((CASE WHEN (t_ref_816.col_1 IN (SELECT sub_823.id FROM tbl_06 AS sub_823)) THEN NULLIF((((1.0 - UPPER(t_ref_816.ref_tbl_08_id)) / NULLIF(t_ref_816.col_1, 0)) >= t_ref_816.col_2), LENGTH((CASE WHEN (COALESCE((((EXISTS (SELECT 1 FROM tbl_05 AS sub_827 WHERE 'alpha_beta_gamma') - (t_ref_816.col_1 - NULL)) BETWEEN 2147483647 AND 2147483647)::BOOLEAN), (SELECT ABS(('chaos' BETWEEN -2147483648 AND 0)) FROM tbl_12 AS sub_828 LIMIT 1)) * ((((t_ref_816.ref_tbl_08_id / NULLIF(t_ref_816.col_1, 0)) AND (ABS(t_ref_816.col_2) + (t_ref_816.id::INT::TEXT)))::TEXT::VARCHAR(32)) BETWEEN -9223372036854775808 AND 42)) THEN ((CASE WHEN ((FALSE IN (SELECT sub_829.payload FROM corpus_articles AS sub_829))::NUMERIC(10,2)) THEN LOWER((((TRUE::BOOLEAN)::TEXT)::BOOLEAN)) ELSE (SELECT (SELECT inn_832.id FROM tbl_14 AS inn_832 LIMIT 1) FROM corpus_articles AS sub_830 LIMIT 1) END)::TEXT) ELSE (SELECT (SELECT NULL FROM tbl_01 AS inn_834 LIMIT 1) FROM tbl_05 AS sub_833 LIMIT 1) END))) ELSE TRUE END) OR ((SELECT (SELECT inn_836.col_3 FROM tbl_11 AS inn_836 LIMIT 1) FROM tbl_05 AS sub_835 LIMIT 1)::BOOLEAN)) / NULLIF((CASE WHEN t_ref_816.col_1 THEN (CASE WHEN ((0::TEXT) IS NOT NULL) THEN 0.0 ELSE (SELECT sub_837.col_4 FROM tbl_03 AS sub_837 LIMIT 1) END) ELSE (SELECT (sub_838.col_4 - 3.14159265) FROM tbl_03 AS sub_838 LIMIT 1) END), 0)) + 0.0) + t_ref_816.ref_tbl_08_id) OR ((EXISTS (SELECT 1 FROM tbl_14 AS sub_839 WHERE sub_839.col_4) / NULLIF(((SELECT (((((sub_840.col_2 IS NULL) <> -9223372036854775808) * sub_840.col_4) * ((SELECT (SELECT inn_842.id FROM tbl_14 AS inn_842 LIMIT 1) FROM tbl_04 AS sub_841 LIMIT 1) + sub_840.col_4)) = (sub_840.col_3::INT)) FROM tbl_06 AS sub_840 LIMIT 1) / NULLIF(t_ref_816.id, 0)), 0)) * UPPER(((EXISTS (SELECT 1 FROM corpus_articles AS sub_843 WHERE 1e10) OR t_ref_816.col_1) * (UPPER(((SELECT (((sub_846.col_2::TEXT) / NULLIF((COALESCE(COALESCE(EXISTS (SELECT 1 FROM tbl_09 AS sub_847 WHERE 1), (CASE WHEN sub_846.col_3 THEN FALSE ELSE sub_846.col_3 END)), ((TRUE::INT::TEXT) AND sub_846.col_4)) - ((('alpha_beta_gamma' * NULL)::INT) * (EXISTS (SELECT 1 FROM tbl_05 AS sub_848 WHERE sub_848.col_2) + LENGTH(sub_846.id)))), 0)) BETWEEN -9223372036854775808 AND 100) FROM tbl_08 AS sub_846 LIMIT 1)::INT::TEXT)) + t_ref_816.ref_tbl_08_id))))) BETWEEN 100 AND 100) THEN t_ref_816.col_1 ELSE ((((SELECT (SELECT (SELECT inn_851.col_1 FROM tbl_13 AS inn_851 LIMIT 1) FROM tbl_05 AS sub_850 LIMIT 1) FROM tbl_08 AS sub_849 LIMIT 1)::INT::TEXT) OR ((SELECT (SELECT inn_872.col_2 FROM tbl_08 AS inn_872 LIMIT 1) FROM tbl_04 AS sub_852 LIMIT 1) + t_ref_816.id)) - (SELECT sub_873.col_4 FROM tbl_02 AS sub_873 LIMIT 1)) END) AS c_874
FROM tbl_09 AS t_ref_816
WHERE t_ref_816.ref_tbl_08_id
LIMIT 33;

UPDATE tbl_02 SET col_2 = FALSE WHERE id > 3;

ALTER TABLE tbl_02 DROP COLUMN col_2;

INSERT INTO tbl_02 (id, col_1, col_3, col_4) VALUES (918389, '{}', TRUE, -1e-5);

INSERT INTO tbl_11 (id, col_1, col_2, col_3, ref_tbl_06_id) VALUES (258892, TRUE, 'alpha_beta_gamma', '[]', -1);

INSERT INTO tbl_13 (id, col_1, col_2, col_3) VALUES (627802, 3.14159265, '[]', FALSE);

UPDATE tbl_05 SET col_1 = 'ref_val' WHERE id > 9;

CREATE INDEX idx_tbl_01_col_3_16 ON tbl_01 (col_3);

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    ref_tbl_13_id INT,
    FOREIGN KEY (ref_tbl_13_id) REFERENCES tbl_13(id)
);

SELECT
    (SELECT (SELECT inn_877.col_1 FROM tbl_08 AS inn_877 LIMIT 1) FROM tbl_13 AS sub_876 LIMIT 1) AS c_878
FROM tbl_05 AS t_ref_875
WHERE (((UPPER(LOWER((NULL IS NULL)))::BOOLEAN) / NULLIF(LOWER((NULL - -1)), 0))::INT)
LIMIT 22;

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 JSONB,
    col_4 INT
);

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (171886, -1, FALSE, -9223372036854775808, 999999999.99);

CREATE TABLE tbl_17 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 NUMERIC(10,2),
    col_4 VARCHAR(64)
);

SELECT
    'ref_val' AS c_880,
    ((((SELECT (SELECT 'chaos' FROM tbl_11 AS inn_896 LIMIT 1) FROM tbl_03 AS sub_881 LIMIT 1) <> ((t_ref_879.col_1 IN (SELECT sub_897.ref_tbl_04_id FROM tbl_10 AS sub_897)) AND t_ref_879.id)) >= ((EXISTS (SELECT 1 FROM tbl_03 AS sub_902 WHERE TRUE) <= (LOWER(t_ref_879.col_2)::NUMERIC(10,2))) - ((SELECT (SELECT inn_908.col_1 FROM tbl_12 AS inn_908 LIMIT 1) FROM tbl_11 AS sub_907 LIMIT 1) < TRUE))) < (NOT ((LENGTH(((CASE WHEN t_ref_879.col_3 THEN t_ref_879.col_2 ELSE t_ref_879.col_4 END) - (SELECT (SELECT inn_910.ref_tbl_04_id FROM tbl_10 AS inn_910 LIMIT 1) FROM tbl_17 AS sub_909 LIMIT 1))) <= 'escaped''quote')))) AS c_911
FROM tbl_16 AS t_ref_879
WHERE UPPER(EXISTS (SELECT 1 FROM tbl_15 AS sub_912 WHERE sub_912.col_2))
LIMIT 10;

CREATE TABLE corpus_articles (
    doc_id INT PRIMARY KEY,
    title VARCHAR(256),
    tsv_content TSVECTOR NOT NULL
);

SELECT
    doc_id,
    title,
    tsvector_strip(tsv_content) AS stripped_tsv,
    ts_rank(tsv_content, to_tsquery('english', 'database | crash')) AS rank_score
FROM corpus_articles
WHERE tsvector_strip(tsv_content) @@ to_tsquery('english', 'kernel | database')
   OR length(tsvector_strip(tsv_content)::text) = 0
ORDER BY doc_id;

INSERT INTO tbl_05 (id, col_1, col_2) VALUES (551915, 'escaped''quote', 999999999.99);

UPDATE tbl_03 SET col_2 = 'alpha_beta_gamma' WHERE id > 40;

CREATE INDEX idx_tbl_14_col_2_17 ON tbl_14 (col_2);

CREATE INDEX idx_tbl_03_id_18 ON tbl_03 (id);

BEGIN;
INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (808458, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '__fuzz_marker__', 100);
INSERT INTO tbl_14 (id, col_1, col_2, col_3, col_4) VALUES (947303, -2147483648, 3.14159265, TRUE, 'escaped''quote');
COMMIT;

CREATE TABLE tbl_18 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 BOOLEAN,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

INSERT INTO corpus_articles (id, payload) VALUES (10509, '__fuzz_marker__');

CREATE TABLE tbl_19 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 INT
);

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, col_4) VALUES (745797, FALSE, 0.0000000001, '__fuzz_marker__', 1);
COMMIT;

ALTER TABLE tbl_06 DROP COLUMN col_3;

BEGIN;
INSERT INTO tbl_19 (id, col_1, col_2, col_3) VALUES (322034, 1, 'alpha_beta_gamma', 100);
INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_04_id) VALUES (336781, TRUE, FALSE, 2147483647);
INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_04_id) VALUES (723912, FALSE, TRUE, -2147483648);
COMMIT;

INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_04_id) VALUES (889550, TRUE, FALSE, -1);

ALTER TABLE tbl_05 DROP COLUMN col_2;

INSERT INTO tbl_05 (id, col_1) VALUES (69565, 'escaped''quote');

ALTER TABLE tbl_01 ADD COLUMN dyn_col_6 VARCHAR(32);

SELECT
    (t_ref_940.col_1 IN (SELECT sub_941.col_2 FROM tbl_07 AS sub_941)) AS c_946
FROM tbl_03 AS t_ref_940
WHERE ((SELECT (sub_947.id >= (LENGTH((('ref_val' AND ((sub_947.col_2::INT::TEXT)::BOOLEAN)) + (sub_947.col_2::INT::TEXT)))::TEXT)) FROM tbl_13 AS sub_947 LIMIT 1) / NULLIF(t_ref_940.col_2, 0))
LIMIT 3;

CREATE INDEX idx_tbl_17_col_2_19 ON tbl_17 (col_2);

INSERT INTO tbl_16 (id, col_1, col_2, col_3, col_4) VALUES (602752, 'chaos', 'escaped''quote', '{}', -1);

INSERT INTO tbl_19 (id, col_1, col_2, col_3) VALUES (738550, 100, 'chaos', -9223372036854775808);

CREATE TABLE tbl_20 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2),
    col_4 INT,
    ref_tbl_07_id INT,
    FOREIGN KEY (ref_tbl_07_id) REFERENCES tbl_07(id)
);

BEGIN;
INSERT INTO corpus_articles (id, payload) VALUES (156545, 'chaos');
COMMIT;

UPDATE tbl_18 SET ref_tbl_01_id = 2147483647 WHERE id > 33;

UPDATE tbl_03 SET col_1 = '{}' WHERE id > 89;

INSERT INTO tbl_07 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (236220, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -1.5, '', 2147483647);

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_17 WHERE id IS NOT NULL;

ALTER TABLE tbl_17 ADD COLUMN dyn_col_6 VARCHAR(32);

ALTER TABLE tbl_03 DROP COLUMN col_4;

INSERT INTO tbl_06 (id, col_1, col_2, col_4) VALUES (257116, FALSE, 1.0, -2147483648);

UPDATE tbl_19 SET col_2 = 'chaos' WHERE id > 19;

UPDATE tbl_14 SET col_3 = TRUE WHERE id > 83;

SELECT
    ((SELECT sub_949.id FROM tbl_20 AS sub_949 LIMIT 1) * (((LENGTH((UPPER(t_ref_948.col_1) BETWEEN -1 AND -1))::TEXT::VARCHAR(32)) / NULLIF(((CASE WHEN t_ref_948.col_2 THEN (((((SELECT (SELECT inn_951.ref_tbl_06_id FROM tbl_11 AS inn_951 LIMIT 1) FROM tbl_13 AS sub_950 LIMIT 1)::BOOLEAN)::NUMERIC(10,2)) - (t_ref_948.id IN (SELECT sub_952.col_2 FROM tbl_04 AS sub_952))) / NULLIF(EXISTS (SELECT 1 FROM tbl_18 AS sub_962 WHERE sub_962.ref_tbl_01_id), 0)) ELSE (SELECT EXISTS (SELECT 1 FROM tbl_03 AS sub_964 WHERE sub_964.col_2) FROM tbl_05 AS sub_963 LIMIT 1) END)::NUMERIC(10,2)), 0))::NUMERIC(10,2))) AS c_965,
    (SELECT (SELECT 0.0 FROM tbl_03 AS inn_967 LIMIT 1) FROM tbl_12 AS sub_966 LIMIT 1) AS c_968,
    ((NULL IN (SELECT sub_969.col_2 FROM tbl_13 AS sub_969)) OR EXISTS (SELECT 1 FROM tbl_05 AS sub_970 WHERE 100)) AS c_972
FROM tbl_06 AS t_ref_948
WHERE (((SELECT (SELECT (SELECT NULL FROM tbl_18 AS inn_981 LIMIT 1) FROM tbl_13 AS sub_974 LIMIT 1) FROM tbl_06 AS sub_973 LIMIT 1) <> ((LENGTH(t_ref_948.col_4) BETWEEN 100 AND 42) + (NOT (EXISTS (SELECT 1 FROM tbl_09 AS sub_982 WHERE sub_982.id)))))::NUMERIC(10,2))
LIMIT 1;

SELECT
    (CASE WHEN (CASE WHEN (((CASE WHEN (t_ref_989.id = ((ABS((SELECT (sub_990.col_1 - UPPER(((CASE WHEN ABS(LENGTH(sub_990.id)) THEN ((sub_990.id::BOOLEAN) - EXISTS (SELECT 1 FROM tbl_02 AS sub_991 WHERE sub_991.col_4)) ELSE NULLIF(0, (NOT (sub_990.col_3))) END) * (sub_990.col_3 < (FALSE::TEXT))))) FROM tbl_02 AS sub_990 LIMIT 1)) > (((t_ref_989.ref_tbl_07_id::NUMERIC(10,2)) * (COALESCE(((((COALESCE(TRUE, t_ref_989.col_4)::INT) * ((NULL > t_ref_989.col_4) + t_ref_989.ref_tbl_07_id)) / NULLIF(COALESCE((EXISTS (SELECT 1 FROM tbl_18 AS sub_992 WHERE sub_992.col_1) >= t_ref_989.col_1), ((-1.5::TEXT) / NULLIF(1, 0))), 0)) + t_ref_989.col_3), UPPER(EXISTS (SELECT 1 FROM tbl_06 AS sub_993 WHERE -2147483648))) * NULL)) / NULLIF(1, 0)))::INT)) THEN ((('escaped''quote'::INT::TEXT) * (LOWER(NULL) * t_ref_989.ref_tbl_07_id))::TEXT::VARCHAR(32)) ELSE (t_ref_989.col_4::NUMERIC(10,2)) END)::INT) IS NOT NULL) THEN t_ref_989.ref_tbl_07_id ELSE ((COALESCE((t_ref_989.col_1 - t_ref_989.id), TRUE) IS NULL) AND UPPER(((t_ref_989.ref_tbl_07_id IN (SELECT sub_994.col_1 FROM tbl_12 AS sub_994)) AND NULLIF(t_ref_989.col_2, 'ref_val')))) END) THEN 1e10 ELSE (NULL - (CASE WHEN EXISTS (SELECT 1 FROM tbl_09 AS sub_999 WHERE sub_999.id) THEN ('__fuzz_marker__' IN (SELECT sub_1004.id FROM tbl_14 AS sub_1004)) ELSE UPPER(((SELECT (ABS((CASE WHEN (('escaped''quote' <= ((sub_1017.id / NULLIF((sub_1017.col_3 IN (SELECT sub_1018.col_3 FROM tbl_13 AS sub_1018)), 0)) - UPPER(sub_1017.col_2))) * LOWER(((FALSE IN (SELECT sub_1019.col_1 FROM tbl_05 AS sub_1019)) + ((((CASE WHEN sub_1017.col_1 THEN sub_1017.col_1 ELSE sub_1017.col_1 END) * sub_1017.col_1) AND (NULL::TEXT::VARCHAR(32))) + (sub_1017.col_4 / NULLIF(sub_1017.id, 0)))))) THEN (((sub_1017.col_2::BOOLEAN) - UPPER(NULLIF(LENGTH(sub_1017.col_4), (sub_1017.col_3 <> ((SELECT (SELECT inn_1021.ref_tbl_01_id FROM tbl_07 AS inn_1021 LIMIT 1) FROM tbl_03 AS sub_1020 LIMIT 1) > FALSE))))) AND (SELECT ((EXISTS (SELECT 1 FROM tbl_09 AS sub_1023 WHERE sub_1023.col_2) <> sub_1022.col_1) >= (COALESCE(LOWER(sub_1022.id), (sub_1022.col_4 AND sub_1022.col_4)) BETWEEN 100 AND 0)) FROM tbl_02 AS sub_1022 LIMIT 1)) ELSE ((SELECT (SELECT inn_1029.col_3 FROM tbl_02 AS inn_1029 LIMIT 1) FROM tbl_13 AS sub_1024 LIMIT 1) BETWEEN 1 AND -2147483648) END)) OR 'alpha_beta_gamma') FROM tbl_16 AS sub_1017 LIMIT 1) = (((LENGTH(NULL) <> t_ref_989.col_2) AND ((1e10 IN (SELECT sub_1030.col_3 FROM tbl_17 AS sub_1030)) / NULLIF(COALESCE(((SELECT (SELECT NULL FROM tbl_06 AS inn_1033 LIMIT 1) FROM tbl_01 AS sub_1032 LIMIT 1) > (((ABS(((ABS('chaos') * t_ref_989.col_2) - LOWER((t_ref_989.id / NULLIF(NULL, 0))))) >= (CASE WHEN (((CASE WHEN t_ref_989.col_3 THEN t_ref_989.id ELSE t_ref_989.col_3 END) <> (t_ref_989.ref_tbl_07_id AND t_ref_989.col_1)) - t_ref_989.col_3) THEN t_ref_989.col_4 ELSE (COALESCE(t_ref_989.ref_tbl_07_id, t_ref_989.col_3) - (COALESCE(t_ref_989.col_2, t_ref_989.col_4) AND FALSE)) END)) + (((-1.5::INT::TEXT) - (((CASE WHEN TRUE THEN 'ref_val' ELSE t_ref_989.ref_tbl_07_id END)::TEXT::VARCHAR(32)) < (SELECT TRUE FROM tbl_01 AS sub_1034 LIMIT 1)))::BOOLEAN)) - UPPER(t_ref_989.id))), ((t_ref_989.ref_tbl_07_id / NULLIF((((SELECT (SELECT FALSE FROM tbl_01 AS inn_1036 LIMIT 1) FROM tbl_11 AS sub_1035 LIMIT 1) OR (t_ref_989.col_2 / NULLIF(((SELECT -1e-5 FROM tbl_08 AS sub_1037 LIMIT 1) OR (t_ref_989.col_1::NUMERIC(10,2))), 0))) + LOWER(LOWER((((CASE WHEN 'ref_val' THEN t_ref_989.col_1 ELSE 1.0 END) > ABS(t_ref_989.col_3))::BOOLEAN)))), 0)) AND ((SELECT (SELECT (SELECT NULL FROM tbl_10 AS inn_1040 LIMIT 1) FROM corpus_articles AS sub_1039 LIMIT 1) FROM tbl_14 AS sub_1038 LIMIT 1) = (EXISTS (SELECT 1 FROM tbl_11 AS sub_1041 WHERE sub_1041.col_1) / NULLIF((t_ref_989.col_4 OR ABS(t_ref_989.id)), 0))))), 0))) - (t_ref_989.col_1 IS NOT NULL)))) END)) END) AS c_1044
FROM tbl_20 AS t_ref_989 LEFT JOIN tbl_09 AS j_ref_1045 ON t_ref_989.id = j_ref_1045.id
WHERE ((NOT ((TRUE * NULLIF(((TRUE IN (SELECT sub_1046.ref_tbl_13_id FROM tbl_15 AS sub_1046))::TEXT::VARCHAR(32)), t_ref_989.col_4)))) >= ((t_ref_989.col_4 IN (SELECT sub_1053.dyn_col_5 FROM tbl_01 AS sub_1053)) * t_ref_989.col_2))
LIMIT 47;

INSERT INTO tbl_16 (id, col_1, col_2, col_3, col_4) VALUES (873221, 'chaos', 'alpha_beta_gamma', '[]', -9223372036854775808);

CREATE INDEX idx_tbl_13_col_3_20 ON tbl_13 (col_3);

INSERT INTO tbl_02 (id, col_1, col_3, col_4) VALUES (89988, '{}', FALSE, 1.0);

CREATE INDEX idx_tbl_08_col_1_21 ON tbl_08 (col_1);

UPDATE corpus_articles SET payload = 'chaos' WHERE id > 50;

SELECT
    ABS(NULLIF(((0.0000000001::INT::TEXT)::INT::TEXT), (EXISTS (SELECT 1 FROM tbl_05 AS sub_1055 WHERE sub_1055.col_1) BETWEEN -1 AND 2147483647))) AS c_1056,
    FALSE AS c_1057
FROM tbl_07 AS t_ref_1054
WHERE (((EXISTS (SELECT 1 FROM tbl_12 AS sub_1058 WHERE sub_1058.col_3) / NULLIF((SELECT (SELECT inn_1068.col_2 FROM tbl_10 AS inn_1068 LIMIT 1) FROM tbl_18 AS sub_1059 LIMIT 1), 0))::INT::TEXT) + t_ref_1054.id)
LIMIT 25;

INSERT INTO tbl_03 (id, col_1, col_2) VALUES (771087, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '__fuzz_marker__');

CREATE INDEX idx_tbl_02_col_1_22 ON tbl_02 (col_1);

INSERT INTO tbl_17 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (342625, 'chaos', TRUE, 3.14159265, '', '__fuzz_marker__');

CREATE INDEX idx_tbl_20_ref_tbl_07_id_23 ON tbl_20 (ref_tbl_07_id);

UPDATE tbl_06 SET col_2 = 0.0 WHERE id > 0;

INSERT INTO corpus_articles (id, payload) VALUES (240972, 'chaos');

SELECT
    (CASE WHEN t_ref_1069.col_1 THEN ((EXISTS (SELECT 1 FROM tbl_20 AS sub_1070 WHERE sub_1070.col_1) * LOWER((FALSE <> (EXISTS (SELECT 1 FROM corpus_articles AS sub_1078 WHERE sub_1078.payload) OR 1e10)))) + ((((ABS(((LOWER(((t_ref_1069.col_4 IN (SELECT sub_1079.id FROM tbl_16 AS sub_1079)) + t_ref_1069.col_1)) > ABS(t_ref_1069.col_1)) / NULLIF(((SELECT (SELECT NULL FROM tbl_18 AS inn_1084 LIMIT 1) FROM tbl_08 AS sub_1082 LIMIT 1) = (t_ref_1069.col_3 / NULLIF(COALESCE(t_ref_1069.id, t_ref_1069.id), 0))), 0))) * (((EXISTS (SELECT 1 FROM tbl_13 AS sub_1085 WHERE sub_1085.col_1) / NULLIF((SELECT sub_1086.col_3 FROM tbl_18 AS sub_1086 LIMIT 1), 0)) <= ((SELECT sub_1087.col_1 FROM tbl_05 AS sub_1087 LIMIT 1)::TEXT::VARCHAR(32))) <> t_ref_1069.id)) * (NOT (COALESCE(t_ref_1069.id, (SELECT ((NOT (NULL)) - NULLIF(sub_1088.payload, TRUE)) FROM corpus_articles AS sub_1088 LIMIT 1))))) = (LENGTH(((SELECT UPPER((NOT ((sub_1089.id IN (SELECT sub_1090.col_3 FROM tbl_16 AS sub_1090))))) FROM tbl_16 AS sub_1089 LIMIT 1) * (((((((((SELECT (SELECT inn_1093.ref_tbl_01_id FROM tbl_18 AS inn_1093 LIMIT 1) FROM tbl_01 AS sub_1092 LIMIT 1) < ((100 >= t_ref_1069.id) * (t_ref_1069.col_3 IN (SELECT sub_1094.id FROM tbl_12 AS sub_1094)))) - NULLIF(NULLIF((t_ref_1069.col_1::TEXT), (t_ref_1069.col_4 IN (SELECT sub_1095.id FROM tbl_06 AS sub_1095))), (t_ref_1069.col_3 IN (SELECT sub_1096.ref_tbl_01_id FROM tbl_07 AS sub_1096)))) - ((('chaos' AND (t_ref_1069.col_4 IN (SELECT sub_1097.id FROM tbl_12 AS sub_1097))) * (COALESCE(t_ref_1069.col_4, NULL) IS NULL))::BOOLEAN)) * ((SELECT (SELECT 2147483647 FROM tbl_04 AS inn_1099 LIMIT 1) FROM tbl_06 AS sub_1098 LIMIT 1) - NULL)) - 'chaos') + (ABS((999999999.99::INT))::TEXT)) OR t_ref_1069.col_4) <> ABS(t_ref_1069.col_1)))) * 0)) OR (CASE WHEN (UPPER((0.0000000001 * t_ref_1069.id)) <= EXISTS (SELECT 1 FROM corpus_articles AS sub_1100 WHERE sub_1100.payload)) THEN (t_ref_1069.col_4 + NULL) ELSE (NOT (EXISTS (SELECT 1 FROM tbl_19 AS sub_1102 WHERE 0.0))) END))) ELSE t_ref_1069.id END) AS c_1114,
    t_ref_1069.col_4 AS c_1115
FROM tbl_02 AS t_ref_1069 LEFT JOIN corpus_articles AS j_ref_1116 ON t_ref_1069.id = j_ref_1116.id
WHERE t_ref_1069.col_1
LIMIT 37;

CREATE TABLE tbl_21 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 JSONB,
    col_3 VARCHAR(64)
);

INSERT INTO tbl_13 (id, col_1, col_2, col_3) VALUES (719031, 999999999.99, '[]', FALSE);

UPDATE tbl_02 SET col_1 = '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}' WHERE id > 53;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_4) VALUES (921312, FALSE, 0.0, 2147483647);
COMMIT;

CREATE TABLE tbl_22 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64)
);

INSERT INTO tbl_14 (id, col_1, col_2, col_3, col_4) VALUES (670149, 2147483647, -1.5, TRUE, 'ref_val');

SELECT
    1e10 AS c_1118
FROM tbl_14 AS t_ref_1117
WHERE t_ref_1117.col_4
LIMIT 20;

CREATE INDEX idx_tbl_21_id_24 ON tbl_21 (id);

UPDATE tbl_15 SET col_1 = FALSE WHERE id > 41;

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_3, col_4) VALUES (732143, '[]', FALSE, 0.0);
INSERT INTO tbl_06 (id, col_1, col_2, col_4) VALUES (257283, TRUE, -1.5, 9223372036854775807);
INSERT INTO tbl_21 (id, col_1, col_2, col_3) VALUES (149284, '{}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'ref_val');
COMMIT;

SELECT
    LENGTH((ABS(999999999.99) AND COALESCE((((t_ref_1119.ref_tbl_04_id / NULLIF(ABS((t_ref_1119.id::NUMERIC(10,2))), 0)) + (100 - (((SELECT (sub_1120.col_1 - (SELECT (SELECT inn_1122.col_3 FROM tbl_07 AS inn_1122 LIMIT 1) FROM tbl_09 AS sub_1121 LIMIT 1)) FROM tbl_13 AS sub_1120 LIMIT 1) - t_ref_1119.col_1) IS NOT NULL))) BETWEEN -1 AND -1), (SELECT (SELECT inn_1124.id FROM tbl_01 AS inn_1124 LIMIT 1) FROM tbl_17 AS sub_1123 LIMIT 1)))) AS c_1125,
    (COALESCE((COALESCE(EXISTS (SELECT 1 FROM corpus_articles AS sub_1126 WHERE sub_1126.id), EXISTS (SELECT 1 FROM tbl_15 AS sub_1140 WHERE FALSE)) / NULLIF((t_ref_1119.ref_tbl_04_id OR LOWER(((SELECT -1e-5 FROM tbl_13 AS sub_1148 LIMIT 1)::INT::TEXT))), 0)), (((SELECT COALESCE((LOWER(((SELECT (FALSE IN (SELECT sub_1151.id FROM tbl_09 AS sub_1151)) FROM tbl_13 AS sub_1150 LIMIT 1) OR NULL)) - sub_1149.col_2), (SELECT (SELECT inn_1158.id FROM tbl_05 AS inn_1158 LIMIT 1) FROM tbl_12 AS sub_1152 LIMIT 1)) FROM tbl_09 AS sub_1149 LIMIT 1) IS NULL) / NULLIF(COALESCE(UPPER((SELECT (SELECT inn_1160.col_1 FROM tbl_18 AS inn_1160 LIMIT 1) FROM tbl_01 AS sub_1159 LIMIT 1)), t_ref_1119.col_1), 0)))::NUMERIC(10,2)) AS c_1161,
    (SELECT (SELECT FALSE FROM tbl_14 AS inn_1185 LIMIT 1) FROM tbl_20 AS sub_1162 LIMIT 1) AS c_1186
FROM tbl_10 AS t_ref_1119
WHERE t_ref_1119.col_1
LIMIT 3;

SELECT
    TRUE AS c_1188,
    t_ref_1187.col_2 AS c_1189,
    (SELECT EXISTS (SELECT 1 FROM tbl_13 AS sub_1191 WHERE 3.14159265) FROM tbl_12 AS sub_1190 LIMIT 1) AS c_1202
FROM tbl_15 AS t_ref_1187 LEFT JOIN tbl_07 AS j_ref_1203 ON t_ref_1187.id = j_ref_1203.id
WHERE 0
LIMIT 20;

UPDATE tbl_07 SET col_3 = '__fuzz_marker__' WHERE id > 14;

SELECT
    LENGTH(EXISTS (SELECT 1 FROM tbl_22 AS sub_1205 WHERE TRUE)) AS c_1206,
    ((EXISTS (SELECT 1 FROM tbl_16 AS sub_1207 WHERE FALSE) + t_ref_1204.col_1) / NULLIF(((t_ref_1204.col_1::BOOLEAN) + t_ref_1204.id), 0)) AS c_1212
FROM tbl_03 AS t_ref_1204
WHERE (CASE WHEN t_ref_1204.col_2 THEN NULLIF(((EXISTS (SELECT 1 FROM tbl_22 AS sub_1213 WHERE sub_1213.id) / NULLIF((CASE WHEN ((CASE WHEN ('__fuzz_marker__' IN (SELECT sub_1214.id FROM tbl_16 AS sub_1214)) THEN ((t_ref_1204.col_2 IN (SELECT sub_1216.col_1 FROM tbl_22 AS sub_1216))::INT::TEXT) ELSE ((SELECT (sub_1221.col_2 = COALESCE(COALESCE(((((FALSE <= TRUE)::INT::TEXT) + sub_1221.col_1) * NULLIF((sub_1221.ref_tbl_07_id <= sub_1221.id), NULLIF(COALESCE(FALSE, NULL), sub_1221.col_2))), NULL), (FALSE IN (SELECT sub_1222.col_3 FROM tbl_22 AS sub_1222)))) FROM tbl_20 AS sub_1221 LIMIT 1) * EXISTS (SELECT 1 FROM tbl_12 AS sub_1224 WHERE sub_1224.col_3)) END) AND FALSE) THEN (EXISTS (SELECT 1 FROM tbl_20 AS sub_1227 WHERE sub_1227.id) / NULLIF((UPPER((t_ref_1204.id / NULLIF(NULLIF((EXISTS (SELECT 1 FROM tbl_15 AS sub_1236 WHERE sub_1236.ref_tbl_13_id) - (1 IN (SELECT sub_1238.id FROM tbl_05 AS sub_1238))), EXISTS (SELECT 1 FROM tbl_05 AS sub_1240 WHERE sub_1240.id)), 0))) + (EXISTS (SELECT 1 FROM tbl_21 AS sub_1242 WHERE sub_1242.id) = -9223372036854775808)), 0)) ELSE (CASE WHEN ABS(COALESCE((t_ref_1204.col_2 IN (SELECT sub_1243.col_2 FROM tbl_21 AS sub_1243)), t_ref_1204.col_1)) THEN ('__fuzz_marker__' = t_ref_1204.id) ELSE NULLIF((((t_ref_1204.id IN (SELECT sub_1253.id FROM tbl_19 AS sub_1253)) - EXISTS (SELECT 1 FROM tbl_08 AS sub_1256 WHERE 'escaped''quote')) > ((((EXISTS (SELECT 1 FROM tbl_03 AS sub_1258 WHERE FALSE)::TEXT::VARCHAR(32)) / NULLIF(NULL, 0)) - ABS(t_ref_1204.col_2)) > t_ref_1204.id)), (CASE WHEN ABS((t_ref_1204.id::INT::TEXT)) THEN (LENGTH(((SELECT (SELECT inn_1262.id FROM tbl_14 AS inn_1262 LIMIT 1) FROM tbl_13 AS sub_1261 LIMIT 1) / NULLIF(NULL, 0))) = (SELECT (sub_1263.id * (sub_1263.col_2 IN (SELECT sub_1264.ref_tbl_08_id FROM tbl_09 AS sub_1264))) FROM tbl_09 AS sub_1263 LIMIT 1)) ELSE t_ref_1204.col_2 END)) END) END), 0)) - FALSE), ((((SELECT COALESCE(((sub_1265.col_2 < ((LOWER(9223372036854775807)::INT) = (((ABS(1) - COALESCE(TRUE, (ABS(sub_1265.ref_tbl_08_id)::INT))) <> sub_1265.col_2) OR (sub_1265.col_2 BETWEEN -2147483648 AND 0)))) + (CASE WHEN (COALESCE((1e10::BOOLEAN), ABS(((sub_1265.ref_tbl_08_id * EXISTS (SELECT 1 FROM tbl_02 AS sub_1266 WHERE sub_1266.col_3)) * LENGTH((ABS(sub_1265.id) / NULLIF(sub_1265.ref_tbl_08_id, 0)))))) / NULLIF(COALESCE(sub_1265.id, NULL), 0)) THEN (((LOWER(sub_1265.ref_tbl_08_id) / NULLIF((COALESCE(ABS(LOWER(sub_1265.ref_tbl_08_id)), (('escaped''quote' * sub_1265.ref_tbl_08_id) - sub_1265.col_2))::BOOLEAN), 0)) * (CASE WHEN (SELECT (SELECT inn_1269.col_4 FROM tbl_16 AS inn_1269 LIMIT 1) FROM tbl_19 AS sub_1267 LIMIT 1) THEN sub_1265.ref_tbl_08_id ELSE sub_1265.ref_tbl_08_id END))::BOOLEAN) ELSE EXISTS (SELECT 1 FROM tbl_13 AS sub_1270 WHERE sub_1270.col_2) END)), FALSE) FROM tbl_09 AS sub_1265 LIMIT 1) IS NULL)::INT) * t_ref_1204.col_1)) ELSE (FALSE::NUMERIC(10,2)) END)
LIMIT 7;

SELECT
    t_ref_1271.id AS c_1272,
    LOWER(ABS(((NOT (((((t_ref_1271.id - ((LOWER(t_ref_1271.col_1) * (((CASE WHEN (((SELECT (SELECT inn_1274.col_1 FROM tbl_05 AS inn_1274 LIMIT 1) FROM tbl_12 AS sub_1273 LIMIT 1)::INT::TEXT) - (t_ref_1271.id IN (SELECT sub_1275.col_3 FROM tbl_21 AS sub_1275))) THEN (t_ref_1271.col_1 + (CASE WHEN (SELECT sub_1276.id FROM tbl_03 AS sub_1276 LIMIT 1) THEN t_ref_1271.col_2 ELSE NULLIF(t_ref_1271.ref_tbl_08_id, (t_ref_1271.ref_tbl_08_id::BOOLEAN)) END)) ELSE t_ref_1271.ref_tbl_08_id END) + (((((FALSE + t_ref_1271.id) - (t_ref_1271.col_1 / NULLIF(t_ref_1271.col_1, 0))) OR (-1e-5::TEXT)) - ((t_ref_1271.id OR (1.0 - t_ref_1271.col_2)) <= ((CASE WHEN t_ref_1271.id THEN t_ref_1271.ref_tbl_08_id ELSE TRUE END) - ('ref_val' + t_ref_1271.col_2)))) OR ((CASE WHEN (CASE WHEN EXISTS (SELECT 1 FROM corpus_articles AS sub_1277 WHERE sub_1277.payload) THEN ('' + t_ref_1271.ref_tbl_08_id) ELSE NULLIF(NULL, t_ref_1271.id) END) THEN ((t_ref_1271.id::NUMERIC(10,2))::INT) ELSE FALSE END) * (SELECT (SELECT inn_1279.col_1 FROM tbl_16 AS inn_1279 LIMIT 1) FROM tbl_08 AS sub_1278 LIMIT 1)))) IS NULL)) * NULLIF((t_ref_1271.ref_tbl_08_id < (42 IS NOT NULL)), ABS(NULL)))) / NULLIF(((t_ref_1271.col_2 IN (SELECT sub_1280.col_4 FROM tbl_06 AS sub_1280)) = LENGTH(EXISTS (SELECT 1 FROM tbl_07 AS sub_1285 WHERE 100))), 0)) - TRUE) BETWEEN 100 AND 42))) - UPPER((t_ref_1271.col_1 - t_ref_1271.id))))) AS c_1288
FROM tbl_09 AS t_ref_1271 LEFT JOIN tbl_20 AS j_ref_1289 ON t_ref_1271.id = j_ref_1289.id
WHERE ((SELECT (SELECT inn_1318.id FROM tbl_14 AS inn_1318 LIMIT 1) FROM corpus_articles AS sub_1290 LIMIT 1) <= t_ref_1271.ref_tbl_08_id)
LIMIT 41;

SELECT
    t_ref_1319.col_3 AS c_1320,
    ((((SELECT (SELECT 999999999.99 FROM tbl_17 AS inn_1334 LIMIT 1) FROM tbl_03 AS sub_1321 LIMIT 1) OR NULL) <> ((((CASE WHEN (1.0 AND ((1.0 IN (SELECT sub_1335.col_3 FROM tbl_01 AS sub_1335)) AND (t_ref_1319.col_1::TEXT::VARCHAR(32)))) THEN ((((t_ref_1319.col_2::NUMERIC(10,2)) OR t_ref_1319.col_4) < (LOWER((LOWER(((SELECT sub_1336.col_2 FROM tbl_11 AS sub_1336 LIMIT 1) / NULLIF(((t_ref_1319.col_4 * (SELECT sub_1337.col_2 FROM tbl_19 AS sub_1337 LIMIT 1)) <= (t_ref_1319.id <= ((SELECT (SELECT inn_1339.col_2 FROM tbl_11 AS inn_1339 LIMIT 1) FROM tbl_17 AS sub_1338 LIMIT 1) + t_ref_1319.id))), 0))) < ((t_ref_1319.ref_tbl_07_id AND EXISTS (SELECT 1 FROM tbl_05 AS sub_1340 WHERE sub_1340.col_1)) - ((CASE WHEN ('alpha_beta_gamma' IN (SELECT sub_1343.col_1 FROM tbl_10 AS sub_1343)) THEN (LENGTH((t_ref_1319.ref_tbl_07_id <= 0.0000000001))::TEXT) ELSE (SELECT -2147483648 FROM tbl_22 AS sub_1345 LIMIT 1) END) / NULLIF(((0.0000000001::TEXT::VARCHAR(32)) - LENGTH(EXISTS (SELECT 1 FROM tbl_15 AS sub_1346 WHERE sub_1346.col_2))), 0))))) / NULLIF((SELECT (SELECT 'alpha_beta_gamma' FROM tbl_19 AS inn_1351 LIMIT 1) FROM tbl_12 AS sub_1347 LIMIT 1), 0))) / NULLIF(EXISTS (SELECT 1 FROM tbl_17 AS sub_1352 WHERE sub_1352.col_2), 0)) ELSE ((FALSE IN (SELECT sub_1356.col_2 FROM tbl_07 AS sub_1356)) + (((NULL * t_ref_1319.col_3) - (((SELECT NULLIF('', sub_1359.col_3) FROM tbl_11 AS sub_1359 LIMIT 1)::INT) - 1)) / NULLIF(TRUE, 0))) END) <= (t_ref_1319.id - ((((EXISTS (SELECT 1 FROM tbl_03 AS sub_1360 WHERE FALSE) < ABS(t_ref_1319.col_3)) > (CASE WHEN ((((NULLIF((LENGTH(t_ref_1319.col_2) * (t_ref_1319.id OR t_ref_1319.col_3)), (t_ref_1319.col_3 IN (SELECT sub_1361.col_4 FROM tbl_02 AS sub_1361))) / NULLIF((((t_ref_1319.col_2 * TRUE) >= (t_ref_1319.col_2 / NULLIF('escaped''quote', 0))) <= (NULLIF(t_ref_1319.col_4, t_ref_1319.col_3)::NUMERIC(10,2))), 0))::BOOLEAN) - ABS(1.0)) IS NULL) THEN (SELECT (SELECT inn_1364.col_1 FROM tbl_05 AS inn_1364 LIMIT 1) FROM tbl_03 AS sub_1362 LIMIT 1) ELSE (t_ref_1319.col_3 / NULLIF(0, 0)) END))::BOOLEAN) + 0.0)))::INT::TEXT) < LENGTH(''))) < LENGTH(t_ref_1319.ref_tbl_07_id)) AS c_1365
FROM tbl_20 AS t_ref_1319
WHERE (((NOT (EXISTS (SELECT 1 FROM tbl_19 AS sub_1366 WHERE sub_1366.col_2))) <> FALSE) >= (((LENGTH((SELECT (SELECT 'ref_val' FROM tbl_18 AS inn_1382 LIMIT 1) FROM tbl_14 AS sub_1368 LIMIT 1)) - FALSE)::TEXT)::INT::TEXT))
LIMIT 49;

UPDATE tbl_04 SET col_2 = '' WHERE id > 0;

CREATE TABLE tbl_23 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64)
);

INSERT INTO corpus_articles (id, payload) VALUES (21293, 'alpha_beta_gamma');

CREATE INDEX idx_tbl_22_col_1_25 ON tbl_22 (col_1);

CREATE TABLE tbl_24 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 JSONB,
    col_4 VARCHAR(64)
);
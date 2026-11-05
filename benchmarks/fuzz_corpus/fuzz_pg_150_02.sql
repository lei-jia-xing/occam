-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: POSTGRES

-- Statements: 150

-- AST Expression Depth: 18

-- Injected Defect Case: pg_07

-- Seed: 15012



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSONB,
    col_3 VARCHAR(64)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 JSONB,
    col_3 NUMERIC(10,2),
    col_4 JSONB
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 BOOLEAN
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2)
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 BOOLEAN,
    col_3 VARCHAR(64),
    col_4 JSONB,
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

CREATE TABLE sensor_ranges (
    record_id INT PRIMARY KEY,
    station_id INT NOT NULL,
    operating_interval INT4RANGE
);

SELECT
    (((((((t_ref_1.id = t_ref_1.id) BETWEEN -1 AND 100) - ((TRUE + 100) + t_ref_1.payload)) > t_ref_1.id) * (t_ref_1.id + t_ref_1.id)) - '') >= ((t_ref_1.payload + t_ref_1.id) AND ('alpha_beta_gamma' AND (EXISTS (SELECT 1 FROM tbl_02 AS sub_2 WHERE sub_2.col_1) AND t_ref_1.id)))) AS c_8,
    LENGTH((UPPER(UPPER('')) - LENGTH((-2147483648 / NULLIF((FALSE::BOOLEAN), 0))))) AS c_9
FROM sensor_ranges AS t_ref_1
WHERE ((SELECT ((sub_10.col_2 IN (SELECT sub_11.id FROM tbl_01 AS sub_11)) + NULL) FROM tbl_02 AS sub_10 LIMIT 1)::TEXT)
LIMIT 45;

SELECT
    ((t_ref_17.col_1 IN (SELECT sub_18.col_1 FROM tbl_02 AS sub_18))::TEXT) AS c_19
FROM tbl_02 AS t_ref_17
WHERE ((SELECT COALESCE((sub_20.payload * TRUE), (UPPER((SELECT (SELECT 'chaos' FROM tbl_04 AS inn_25 LIMIT 1) FROM tbl_04 AS sub_21 LIMIT 1))::INT::TEXT)) FROM sensor_ranges AS sub_20 LIMIT 1) > (SELECT (SELECT 'escaped''quote' FROM tbl_04 AS inn_28 LIMIT 1) FROM tbl_01 AS sub_26 LIMIT 1))
LIMIT 43;

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    col_3 VARCHAR(64),
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT
);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (373740, TRUE, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 1e10, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

UPDATE tbl_07 SET col_2 = 0 WHERE id > 61;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (947219, -9223372036854775808, '__fuzz_marker__', -1e-5);
INSERT INTO tbl_03 (id, col_1, col_2) VALUES (5576, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', TRUE);
INSERT INTO sensor_ranges (id, payload) VALUES (3550, '');
COMMIT;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT
);

INSERT INTO tbl_03 (id, col_1, col_2) VALUES (851659, '[]', TRUE);

INSERT INTO sensor_ranges (id, payload) VALUES (5627, '__fuzz_marker__');

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 JSONB
);

SELECT
    (t_ref_29.col_1::NUMERIC(10,2)) AS c_30,
    ((SELECT (SELECT inn_33.col_1 FROM tbl_02 AS inn_33 LIMIT 1) FROM sensor_ranges AS sub_31 LIMIT 1) OR LENGTH((NOT (t_ref_29.id)))) AS c_34,
    (t_ref_29.col_2 + ((UPPER(ABS(EXISTS (SELECT 1 FROM tbl_07 AS sub_35 WHERE sub_35.id)))::TEXT::VARCHAR(32)) * TRUE)) AS c_42
FROM tbl_09 AS t_ref_29
WHERE (CASE WHEN ((((t_ref_29.col_3 > COALESCE((t_ref_29.col_2 IN (SELECT sub_43.col_2 FROM tbl_04 AS sub_43)), FALSE)) <= ((FALSE >= (('' <> t_ref_29.col_3) / NULLIF((NOT (t_ref_29.col_2)), 0))) + (NULLIF((NULLIF((SELECT (UPPER(EXISTS (SELECT 1 FROM tbl_09 AS sub_46 WHERE -9223372036854775808)) AND (((SELECT (sub_48.col_4 * sub_48.col_2) FROM tbl_02 AS sub_48 LIMIT 1)::TEXT)::TEXT)) FROM sensor_ranges AS sub_45 LIMIT 1), (ABS(t_ref_29.col_1) / NULLIF((NULL IN (SELECT sub_49.col_1 FROM tbl_01 AS sub_49)), 0))) * ABS(t_ref_29.id)), ((UPPER((((((TRUE <> (CASE WHEN (TRUE < t_ref_29.col_2) THEN EXISTS (SELECT 1 FROM tbl_05 AS sub_50 WHERE sub_50.col_4) ELSE LENGTH(t_ref_29.col_2) END)) + (t_ref_29.col_2 IN (SELECT sub_51.payload FROM sensor_ranges AS sub_51))) - 999999999.99) <= (UPPER(t_ref_29.col_1)::TEXT::VARCHAR(32))) - (((SELECT TRUE FROM sensor_ranges AS sub_52 LIMIT 1) - ((((t_ref_29.id AND t_ref_29.id) / NULLIF(TRUE, 0)) AND ABS(t_ref_29.col_3)) / NULLIF(EXISTS (SELECT 1 FROM tbl_02 AS sub_53 WHERE sub_53.col_3), 0))) <= (NULL IN (SELECT sub_54.id FROM tbl_06 AS sub_54)))))::INT) > ((((EXISTS (SELECT 1 FROM tbl_01 AS sub_56 WHERE sub_56.col_3) OR (SELECT (SELECT NULL FROM tbl_06 AS inn_59 LIMIT 1) FROM tbl_08 AS sub_57 LIMIT 1))::NUMERIC(10,2)) + (t_ref_29.col_1::INT::TEXT))::INT::TEXT)))::NUMERIC(10,2)))) <> UPPER((FALSE IN (SELECT sub_60.col_1 FROM tbl_06 AS sub_60)))) + (((0.0000000001::TEXT::VARCHAR(32))::BOOLEAN) - (SELECT (2147483647 >= ((sub_65.id <> (LOWER((EXISTS (SELECT 1 FROM tbl_02 AS sub_66 WHERE NULL) OR (3.14159265 IN (SELECT sub_68.col_2 FROM tbl_09 AS sub_68)))) * (sub_65.col_1 BETWEEN 42 AND 2147483647))) <= NULL)) FROM tbl_03 AS sub_65 LIMIT 1))) THEN t_ref_29.col_3 ELSE 0.0 END)
LIMIT 13;

SELECT
    (t_ref_70.col_3 IS NULL) AS c_71,
    ((SELECT (SELECT inn_78.col_2 FROM tbl_01 AS inn_78 LIMIT 1) FROM tbl_07 AS sub_72 LIMIT 1) < t_ref_70.col_2) AS c_79
FROM tbl_02 AS t_ref_70
WHERE ((999999999.99 / NULLIF(NULL, 0))::NUMERIC(10,2))
LIMIT 2;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2) VALUES (803008, 0, -9223372036854775808);
INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (339439, 0, 'chaos', 3.14159265);
INSERT INTO tbl_07 (id, col_1, col_2) VALUES (508023, 42, 100);
COMMIT;

CREATE INDEX idx_sensor_ranges_id_1 ON sensor_ranges (id);

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    col_3 NUMERIC(10,2),
    ref_tbl_05_id INT,
    FOREIGN KEY (ref_tbl_05_id) REFERENCES tbl_05(id)
);

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (561990, 'chaos', 'chaos', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

CREATE INDEX idx_sensor_ranges_brin ON sensor_ranges
    USING brin (operating_interval range_inclusion_ops)
    WITH (pages_per_range = 1);

INSERT INTO tbl_08 (id, col_1, col_2) VALUES (59399, -2147483648, 1);

SELECT
    EXISTS (SELECT 1 FROM tbl_02 AS sub_81 WHERE sub_81.col_2) AS c_83,
    ((((((t_ref_80.ref_tbl_02_id IN (SELECT sub_84.col_2 FROM tbl_01 AS sub_84))::TEXT) / NULLIF(t_ref_80.col_1, 0)) BETWEEN -9223372036854775808 AND 9223372036854775807) AND (LENGTH((EXISTS (SELECT 1 FROM sensor_ranges AS sub_88 WHERE sub_88.payload) IS NOT NULL)) >= (SELECT (SELECT inn_99.col_1 FROM tbl_03 AS inn_99 LIMIT 1) FROM tbl_04 AS sub_90 LIMIT 1))) BETWEEN -9223372036854775808 AND 9223372036854775807) AS c_100,
    (((((-2147483648::INT) BETWEEN 1 AND -2147483648)::INT) + (NOT (((0.0000000001 + (SELECT (SELECT inn_124.id FROM tbl_06 AS inn_124 LIMIT 1) FROM tbl_04 AS sub_101 LIMIT 1)) < ((COALESCE(((LOWER(((NOT (((((CASE WHEN (TRUE = t_ref_80.col_2) THEN (t_ref_80.col_3::BOOLEAN) ELSE (NULL IN (SELECT sub_125.col_4 FROM tbl_05 AS sub_125)) END) * (t_ref_80.col_4 - t_ref_80.ref_tbl_02_id)) / NULLIF((ABS((NOT (t_ref_80.ref_tbl_02_id))) <> ((NULL IN (SELECT sub_126.payload FROM sensor_ranges AS sub_126))::TEXT)), 0))::INT))) * ((((t_ref_80.id OR (LENGTH('chaos') + FALSE)) AND t_ref_80.col_2) = ((SELECT (SELECT sub_128.col_1 FROM tbl_04 AS sub_128 LIMIT 1) FROM tbl_03 AS sub_127 LIMIT 1) > t_ref_80.col_4)) + (t_ref_80.col_1 - EXISTS (SELECT 1 FROM tbl_01 AS sub_129 WHERE sub_129.col_2))))) * (EXISTS (SELECT 1 FROM tbl_05 AS sub_130 WHERE sub_130.id) + (NOT (t_ref_80.col_4)))) - ((((((((NOT (t_ref_80.ref_tbl_02_id)) + (SELECT (SELECT 100 FROM tbl_10 AS inn_132 LIMIT 1) FROM tbl_05 AS sub_131 LIMIT 1)) > 'ref_val') / NULLIF((t_ref_80.col_2::BOOLEAN), 0)) / NULLIF(((SELECT (SELECT 'chaos' FROM tbl_05 AS inn_134 LIMIT 1) FROM tbl_09 AS sub_133 LIMIT 1) >= LENGTH(TRUE)), 0))::TEXT)::NUMERIC(10,2)) AND t_ref_80.col_3)), t_ref_80.id)::INT) * ((t_ref_80.col_2 < (((t_ref_80.id <= COALESCE((TRUE AND NULLIF((LOWER(((t_ref_80.col_4::BOOLEAN)::BOOLEAN)) - (COALESCE(t_ref_80.col_4, (NULL IN (SELECT sub_135.id FROM tbl_02 AS sub_135))) < (NOT ((SELECT (SELECT TRUE FROM tbl_02 AS inn_137 LIMIT 1) FROM sensor_ranges AS sub_136 LIMIT 1))))), (-1e-5 IN (SELECT sub_138.id FROM sensor_ranges AS sub_138)))), (SELECT sub_140.col_3 FROM tbl_06 AS sub_140 LIMIT 1))) * (LENGTH((CASE WHEN 2147483647 THEN ((((CASE WHEN COALESCE(FALSE, t_ref_80.col_4) THEN (NOT (t_ref_80.col_1)) ELSE (t_ref_80.ref_tbl_02_id * t_ref_80.ref_tbl_02_id) END) OR t_ref_80.ref_tbl_02_id) + EXISTS (SELECT 1 FROM tbl_03 AS sub_141 WHERE 0)) > ABS('')) ELSE ((ABS(t_ref_80.ref_tbl_02_id)::TEXT::VARCHAR(32)) / NULLIF(LENGTH((2147483647 / NULLIF(t_ref_80.id, 0))), 0)) END))::NUMERIC(10,2))) BETWEEN -1 AND 42)) * (NULLIF((NOT (t_ref_80.col_3)), t_ref_80.col_4) OR FALSE))))))) / NULLIF((TRUE - t_ref_80.col_1), 0)) AS c_142
FROM tbl_05 AS t_ref_80
WHERE (ABS(((100::BOOLEAN) + (CASE WHEN ((EXISTS (SELECT 1 FROM tbl_07 AS sub_143 WHERE sub_143.col_2) + t_ref_80.col_4) < ((CASE WHEN ((((SELECT sub_146.col_1 FROM tbl_03 AS sub_146 LIMIT 1) + t_ref_80.col_4) / NULLIF(t_ref_80.col_2, 0)) > ABS((t_ref_80.col_1 IN (SELECT sub_147.col_2 FROM tbl_04 AS sub_147)))) THEN NULLIF((SELECT ((LENGTH(LENGTH((LOWER((FALSE = sub_148.ref_tbl_03_id)) >= (NULLIF(TRUE, sub_148.ref_tbl_03_id)::TEXT)))) IS NULL)::INT::TEXT) FROM tbl_06 AS sub_148 LIMIT 1), ((SELECT (((((NULLIF(sub_149.col_1, sub_149.col_1) IS NOT NULL) + NULLIF(LOWER(sub_149.col_2), 100))::INT::TEXT) + (SELECT UPPER(sub_150.id) FROM tbl_08 AS sub_150 LIMIT 1)) = sub_149.col_1) FROM tbl_04 AS sub_149 LIMIT 1)::INT)) ELSE LOWER(t_ref_80.col_2) END) + ((CASE WHEN (((NULLIF(((SELECT sub_151.col_1 FROM tbl_06 AS sub_151 LIMIT 1) - TRUE), ('chaos' < (t_ref_80.col_1::BOOLEAN))) / NULLIF((SELECT (SELECT -1e-5 FROM tbl_01 AS inn_153 LIMIT 1) FROM sensor_ranges AS sub_152 LIMIT 1), 0)) AND ((NULLIF(NULLIF(((EXISTS (SELECT 1 FROM tbl_01 AS sub_154 WHERE 0.0) > COALESCE(t_ref_80.ref_tbl_02_id, NULL)) - EXISTS (SELECT 1 FROM tbl_01 AS sub_155 WHERE -1.5)), UPPER(1e10)), t_ref_80.col_2) - (-1e-5 < t_ref_80.col_2)) = LENGTH(NULLIF(TRUE, ((((t_ref_80.col_3 IS NOT NULL) / NULLIF(EXISTS (SELECT 1 FROM tbl_04 AS sub_156 WHERE FALSE), 0)) + (NULLIF(t_ref_80.col_2, t_ref_80.col_3)::TEXT::VARCHAR(32))) * ABS((SELECT sub_157.id FROM tbl_09 AS sub_157 LIMIT 1)))))))::TEXT::VARCHAR(32)) THEN t_ref_80.col_1 ELSE LOWER(t_ref_80.id) END) / NULLIF(((SELECT sub_158.id FROM tbl_08 AS sub_158 LIMIT 1) * (LOWER(((SELECT (SELECT 1 FROM tbl_07 AS inn_162 LIMIT 1) FROM tbl_05 AS sub_159 LIMIT 1)::TEXT)) < ((NOT (((SELECT ((0.0 / NULLIF((FALSE * sub_163.col_2), 0)) IS NULL) FROM tbl_01 AS sub_163 LIMIT 1) / NULLIF(EXISTS (SELECT 1 FROM tbl_04 AS sub_164 WHERE sub_164.col_2), 0))))::TEXT))), 0)))) THEN UPPER(((-2147483648 * (CASE WHEN TRUE THEN t_ref_80.col_2 ELSE COALESCE('chaos', (EXISTS (SELECT 1 FROM tbl_05 AS sub_165 WHERE sub_165.col_4) * UPPER(LENGTH(((EXISTS (SELECT 1 FROM tbl_06 AS sub_166 WHERE sub_166.id)::TEXT)::INT))))) END)) - ((((LENGTH(t_ref_80.col_4) <= t_ref_80.id)::TEXT)::BOOLEAN) IS NULL))) ELSE ((t_ref_80.id::INT) - UPPER(t_ref_80.ref_tbl_02_id)) END))) = ((CASE WHEN (((SELECT sub_167.col_1 FROM tbl_10 AS sub_167 LIMIT 1) > (((t_ref_80.col_4 * t_ref_80.col_4) > (((CASE WHEN EXISTS (SELECT 1 FROM tbl_10 AS sub_168 WHERE 'chaos') THEN LOWER((9223372036854775807 IN (SELECT sub_170.col_1 FROM tbl_01 AS sub_170))) ELSE NULLIF(((t_ref_80.col_1::INT) OR (SELECT (SELECT '__fuzz_marker__' FROM tbl_05 AS inn_175 LIMIT 1) FROM tbl_01 AS sub_174 LIMIT 1)), (((((LENGTH(t_ref_80.col_2) >= EXISTS (SELECT 1 FROM tbl_05 AS sub_176 WHERE FALSE))::TEXT) + ('escaped''quote' IS NULL)) <= EXISTS (SELECT 1 FROM tbl_03 AS sub_177 WHERE sub_177.col_1)) BETWEEN 1 AND 0)) END) IS NULL) + (((t_ref_80.id - (((((NULL IN (SELECT sub_178.col_3 FROM tbl_10 AS sub_178)) / NULLIF((EXISTS (SELECT 1 FROM tbl_04 AS sub_179 WHERE NULL)::NUMERIC(10,2)), 0))::BOOLEAN) * (((t_ref_80.ref_tbl_02_id < UPPER(t_ref_80.col_2)) / NULLIF(((''::INT::TEXT) / NULLIF((NOT (t_ref_80.ref_tbl_02_id)), 0)), 0)) * ('__fuzz_marker__' IN (SELECT sub_180.payload FROM sensor_ranges AS sub_180)))) > (((NULLIF(t_ref_80.col_1, LENGTH(t_ref_80.ref_tbl_02_id))::BOOLEAN) / NULLIF(EXISTS (SELECT 1 FROM tbl_08 AS sub_181 WHERE sub_181.col_1), 0)) - (-9223372036854775808 + (t_ref_80.ref_tbl_02_id BETWEEN 9223372036854775807 AND 100))))) + (SELECT (SELECT inn_184.col_3 FROM tbl_04 AS inn_184 LIMIT 1) FROM tbl_10 AS sub_182 LIMIT 1)) IS NULL))) * (LOWER(FALSE) + ((t_ref_80.ref_tbl_02_id IN (SELECT sub_185.id FROM tbl_02 AS sub_185)) + (SELECT (SELECT inn_192.id FROM tbl_09 AS inn_192 LIMIT 1) FROM tbl_05 AS sub_187 LIMIT 1))))) * ((CASE WHEN t_ref_80.col_3 THEN EXISTS (SELECT 1 FROM sensor_ranges AS sub_193 WHERE FALSE) ELSE NULL END) - (SELECT TRUE FROM tbl_01 AS sub_208 LIMIT 1))) THEN (((CASE WHEN ((CASE WHEN (CASE WHEN (t_ref_80.col_3 <> (t_ref_80.col_2 < (LENGTH(NULLIF(LENGTH(FALSE), ((CASE WHEN (t_ref_80.id::NUMERIC(10,2)) THEN (t_ref_80.ref_tbl_02_id IN (SELECT sub_209.col_1 FROM tbl_04 AS sub_209)) ELSE LOWER(0) END)::BOOLEAN))) + (LOWER((NULLIF((t_ref_80.col_1 IN (SELECT sub_210.col_1 FROM tbl_07 AS sub_210)), (SELECT NULL FROM tbl_09 AS sub_211 LIMIT 1)) * (t_ref_80.col_2 BETWEEN 1 AND -2147483648))) >= (SELECT 0.0000000001 FROM tbl_04 AS sub_212 LIMIT 1))))) THEN (t_ref_80.col_4 / NULLIF((t_ref_80.col_1 * (CASE WHEN LOWER(t_ref_80.id) THEN ABS(COALESCE((((SELECT (SELECT TRUE FROM tbl_01 AS inn_214 LIMIT 1) FROM tbl_03 AS sub_213 LIMIT 1) IS NULL)::INT::TEXT), (SELECT sub_215.col_1 FROM tbl_01 AS sub_215 LIMIT 1))) ELSE ((1e10 < ((NOT (t_ref_80.col_1)) IS NOT NULL)) * (SELECT (SELECT inn_218.payload FROM sensor_ranges AS inn_218 LIMIT 1) FROM tbl_03 AS sub_216 LIMIT 1)) END)), 0)) ELSE t_ref_80.col_4 END) THEN ((NOT (COALESCE((NULL * (((CASE WHEN (t_ref_80.col_3 IN (SELECT sub_219.col_2 FROM tbl_03 AS sub_219)) THEN t_ref_80.col_3 ELSE (t_ref_80.col_2 BETWEEN 1 AND 1) END) + (t_ref_80.col_1 IN (SELECT sub_220.col_1 FROM tbl_04 AS sub_220))) + (ABS(((TRUE * t_ref_80.ref_tbl_02_id) <> (CASE WHEN -9223372036854775808 THEN t_ref_80.ref_tbl_02_id ELSE t_ref_80.col_3 END)))::NUMERIC(10,2)))), (((SELECT (SELECT inn_222.col_1 FROM tbl_10 AS inn_222 LIMIT 1) FROM tbl_09 AS sub_221 LIMIT 1) AND (-1.5 IN (SELECT sub_223.col_2 FROM tbl_08 AS sub_223))) + ABS(((UPPER((t_ref_80.col_4 IN (SELECT sub_224.col_2 FROM tbl_07 AS sub_224))) AND (SELECT (SELECT inn_226.id FROM tbl_08 AS inn_226 LIMIT 1) FROM tbl_10 AS sub_225 LIMIT 1))::INT::TEXT)))))) * (UPPER(LOWER((((SELECT (SELECT FALSE FROM tbl_08 AS inn_229 LIMIT 1) FROM tbl_01 AS sub_227 LIMIT 1)::TEXT::VARCHAR(32))::NUMERIC(10,2)))) <> (CASE WHEN ((FALSE IN (SELECT sub_230.id FROM tbl_05 AS sub_230))::INT::TEXT) THEN (SELECT (SELECT inn_232.col_1 FROM tbl_07 AS inn_232 LIMIT 1) FROM tbl_09 AS sub_231 LIMIT 1) ELSE (CASE WHEN ABS(((SELECT (SELECT inn_234.payload FROM sensor_ranges AS inn_234 LIMIT 1) FROM tbl_07 AS sub_233 LIMIT 1) + (((SELECT (SELECT inn_236.id FROM sensor_ranges AS inn_236 LIMIT 1) FROM tbl_07 AS sub_235 LIMIT 1) / NULLIF(t_ref_80.col_3, 0)) > ((SELECT 'ref_val' FROM tbl_04 AS sub_237 LIMIT 1) + (t_ref_80.col_1::TEXT))))) THEN ((t_ref_80.id < ((SELECT TRUE FROM tbl_10 AS sub_238 LIMIT 1) > (NULLIF(t_ref_80.id, 'chaos') * (SELECT (SELECT inn_240.col_4 FROM tbl_02 AS inn_240 LIMIT 1) FROM tbl_10 AS sub_239 LIMIT 1)))) <> ((((NULL::NUMERIC(10,2)) / NULLIF(EXISTS (SELECT 1 FROM tbl_07 AS sub_241 WHERE sub_241.col_2), 0)) OR COALESCE((t_ref_80.col_4 IS NOT NULL), t_ref_80.col_2)) <= t_ref_80.col_2)) ELSE TRUE END) END))) ELSE ((SELECT (SELECT inn_245.payload FROM sensor_ranges AS inn_245 LIMIT 1) FROM tbl_09 AS sub_242 LIMIT 1) < (t_ref_80.col_2 + ((SELECT ((EXISTS (SELECT 1 FROM tbl_03 AS sub_247 WHERE sub_247.id) <> (SELECT -1.5 FROM tbl_06 AS sub_248 LIMIT 1)) > (NULL IN (SELECT sub_249.id FROM tbl_08 AS sub_249))) FROM tbl_05 AS sub_246 LIMIT 1) < COALESCE((t_ref_80.col_2::TEXT), (NULLIF((t_ref_80.col_2 - EXISTS (SELECT 1 FROM tbl_08 AS sub_250 WHERE sub_250.col_2)), (((t_ref_80.ref_tbl_02_id + t_ref_80.ref_tbl_02_id)::INT::TEXT) + t_ref_80.ref_tbl_02_id)) OR ABS((t_ref_80.col_1 IN (SELECT sub_251.col_1 FROM tbl_02 AS sub_251)))))))) END) AND ABS(((((t_ref_80.col_4 / NULLIF((TRUE / NULLIF((((CASE WHEN (t_ref_80.col_2 < 'alpha_beta_gamma') THEN (t_ref_80.col_4::NUMERIC(10,2)) ELSE (t_ref_80.id::TEXT::VARCHAR(32)) END)::TEXT) * t_ref_80.col_3), 0)), 0))::NUMERIC(10,2)) - (CASE WHEN (SELECT sub_252.id FROM tbl_05 AS sub_252 LIMIT 1) THEN t_ref_80.col_3 ELSE LENGTH((t_ref_80.col_4 IN (SELECT sub_253.col_1 FROM tbl_02 AS sub_253))) END)) * (SELECT (EXISTS (SELECT 1 FROM tbl_10 AS sub_256 WHERE sub_256.col_3) * sub_255.col_1) FROM tbl_02 AS sub_255 LIMIT 1)))) THEN (9223372036854775807 IN (SELECT sub_257.col_2 FROM tbl_02 AS sub_257)) ELSE ((SELECT (SELECT NULL FROM tbl_09 AS inn_268 LIMIT 1) FROM tbl_06 AS sub_262 LIMIT 1) <= ((SELECT sub_269.col_4 FROM tbl_05 AS sub_269 LIMIT 1) / NULLIF(LENGTH((SELECT (SELECT inn_272.col_2 FROM tbl_03 AS inn_272 LIMIT 1) FROM tbl_03 AS sub_270 LIMIT 1)), 0))) END) + (((SELECT (CASE WHEN (((EXISTS (SELECT 1 FROM tbl_05 AS sub_274 WHERE sub_274.col_3) + (sub_273.ref_tbl_03_id - (CASE WHEN (sub_273.col_1 + (sub_273.col_1 < NULL)) THEN LENGTH((SELECT (SELECT inn_276.col_2 FROM tbl_02 AS inn_276 LIMIT 1) FROM tbl_02 AS sub_275 LIMIT 1)) ELSE ('alpha_beta_gamma' BETWEEN 42 AND 2147483647) END))) / NULLIF(((sub_273.ref_tbl_03_id >= (((sub_273.col_2 BETWEEN 100 AND -9223372036854775808) - sub_273.col_1) + ((sub_273.id = sub_273.id) + (CASE WHEN 2147483647 THEN sub_273.col_2 ELSE FALSE END)))) / NULLIF(LOWER(sub_273.id), 0)), 0)) AND (sub_273.col_1 <> (CASE WHEN (((3.14159265 - (SELECT (SELECT TRUE FROM tbl_10 AS inn_278 LIMIT 1) FROM tbl_06 AS sub_277 LIMIT 1)) <= NULLIF((sub_273.col_1 + sub_273.ref_tbl_03_id), (sub_273.col_3 IN (SELECT sub_279.col_2 FROM tbl_08 AS sub_279)))) AND (((SELECT (SELECT inn_281.col_1 FROM tbl_09 AS inn_281 LIMIT 1) FROM sensor_ranges AS sub_280 LIMIT 1) = sub_273.id)::TEXT::VARCHAR(32))) THEN (SELECT COALESCE(EXISTS (SELECT 1 FROM tbl_04 AS sub_283 WHERE sub_283.col_1), (sub_282.col_1 < sub_282.id)) FROM tbl_10 AS sub_282 LIMIT 1) ELSE (SELECT (SELECT inn_285.id FROM tbl_01 AS inn_285 LIMIT 1) FROM tbl_01 AS sub_284 LIMIT 1) END))) THEN EXISTS (SELECT 1 FROM tbl_07 AS sub_286 WHERE sub_286.col_2) ELSE ((1.0 <= ((SELECT (SELECT inn_290.col_2 FROM tbl_03 AS inn_290 LIMIT 1) FROM tbl_04 AS sub_289 LIMIT 1) * sub_273.id)) - (LENGTH(sub_273.ref_tbl_03_id) AND (CASE WHEN sub_273.id THEN TRUE ELSE (((TRUE >= (sub_273.col_3 * sub_273.col_1)) * (sub_273.ref_tbl_03_id - EXISTS (SELECT 1 FROM tbl_08 AS sub_291 WHERE sub_291.col_2))) IS NOT NULL) END))) END) FROM tbl_06 AS sub_273 LIMIT 1) BETWEEN 2147483647 AND 42) + (SELECT (SELECT inn_293.col_1 FROM tbl_08 AS inn_293 LIMIT 1) FROM tbl_08 AS sub_292 LIMIT 1)))::TEXT) ELSE (t_ref_80.col_3 - -9223372036854775808) END) >= (ABS(EXISTS (SELECT 1 FROM tbl_05 AS sub_294 WHERE 'escaped''quote')) - (t_ref_80.id IN (SELECT sub_302.id FROM tbl_08 AS sub_302)))))
LIMIT 15;

SELECT
    ((NULLIF((t_ref_303.id IN (SELECT sub_304.col_3 FROM tbl_06 AS sub_304)), ((CASE WHEN t_ref_303.col_2 THEN COALESCE((t_ref_303.col_3 / NULLIF(((CASE WHEN ((((ABS(t_ref_303.col_1)::TEXT::VARCHAR(32))::TEXT) < (LENGTH(t_ref_303.col_2) * (NULLIF((FALSE IN (SELECT sub_305.col_3 FROM tbl_02 AS sub_305)), t_ref_303.col_1) * ((((NOT (t_ref_303.col_1)) - 'ref_val') AND (t_ref_303.id IN (SELECT sub_306.col_2 FROM tbl_07 AS sub_306))) * ((t_ref_303.col_1 BETWEEN -1 AND 2147483647)::TEXT::VARCHAR(32)))))) BETWEEN 1 AND 9223372036854775807) THEN (3.14159265 / NULLIF(LENGTH(((t_ref_303.col_3 >= t_ref_303.col_2) < t_ref_303.col_2)), 0)) ELSE (SELECT (SELECT NULL FROM tbl_04 AS inn_308 LIMIT 1) FROM tbl_05 AS sub_307 LIMIT 1) END) - (((((EXISTS (SELECT 1 FROM tbl_09 AS sub_309 WHERE sub_309.col_3) * (COALESCE(((FALSE::INT) / NULLIF(NULL, 0)), ((t_ref_303.col_3 BETWEEN -9223372036854775808 AND -2147483648)::INT))::TEXT::VARCHAR(32))) >= t_ref_303.col_2)::BOOLEAN) - (((((t_ref_303.col_3 IN (SELECT sub_310.col_2 FROM tbl_03 AS sub_310)) - (t_ref_303.col_1 + EXISTS (SELECT 1 FROM sensor_ranges AS sub_311 WHERE sub_311.payload)))::NUMERIC(10,2)) <> ((((t_ref_303.col_2::TEXT::VARCHAR(32)) - ((t_ref_303.col_3 > 0.0000000001) / NULLIF((t_ref_303.col_3 * -2147483648), 0))) IS NULL) + ((((SELECT (SELECT inn_313.id FROM tbl_08 AS inn_313 LIMIT 1) FROM tbl_05 AS sub_312 LIMIT 1) <= (TRUE / NULLIF('ref_val', 0))) / NULLIF(t_ref_303.id, 0)) * ((EXISTS (SELECT 1 FROM tbl_04 AS sub_314 WHERE sub_314.col_3)::INT::TEXT) IS NULL)))) * (t_ref_303.id IN (SELECT sub_315.col_2 FROM tbl_09 AS sub_315)))) AND t_ref_303.col_1)), 0)), 1.0) ELSE (LENGTH(LOWER(t_ref_303.col_1)) / NULLIF(((LOWER(((CASE WHEN (t_ref_303.col_2 IN (SELECT sub_316.id FROM sensor_ranges AS sub_316)) THEN ((-1 - LENGTH(EXISTS (SELECT 1 FROM tbl_04 AS sub_319 WHERE sub_319.col_1))) = (((((t_ref_303.id::INT) < (t_ref_303.col_2 AND NULL))::NUMERIC(10,2)) OR ((t_ref_303.col_3 BETWEEN -1 AND 100)::TEXT::VARCHAR(32)))::NUMERIC(10,2))) ELSE EXISTS (SELECT 1 FROM tbl_06 AS sub_320 WHERE sub_320.id) END) AND 'alpha_beta_gamma'))::TEXT::VARCHAR(32))::INT), 0)) END)::INT::TEXT)) / NULLIF(EXISTS (SELECT 1 FROM tbl_02 AS sub_321 WHERE sub_321.col_1), 0)) * ((SELECT (SELECT FALSE FROM tbl_05 AS inn_325 LIMIT 1) FROM tbl_01 AS sub_324 LIMIT 1)::BOOLEAN)) AS c_326,
    (SELECT (SELECT inn_340.payload FROM sensor_ranges AS inn_340 LIMIT 1) FROM tbl_06 AS sub_327 LIMIT 1) AS c_341
FROM tbl_01 AS t_ref_303
WHERE (SELECT NULL FROM tbl_03 AS sub_342 LIMIT 1)
LIMIT 34;

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

UPDATE tbl_08 SET col_1 = -2147483648 WHERE id > 32;

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (254264, FALSE, '{}', 1e10, '{}');
COMMIT;

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_01 WHERE id IS NOT NULL;

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (148941, 'alpha_beta_gamma', '', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

SELECT
    (((t_ref_343.col_2 + COALESCE((SELECT (SELECT inn_362.id FROM tbl_09 AS inn_362 LIMIT 1) FROM tbl_03 AS sub_344 LIMIT 1), (CASE WHEN ((('ref_val' >= t_ref_343.col_1)::TEXT::VARCHAR(32)) / NULLIF((SELECT (SELECT inn_369.col_1 FROM tbl_08 AS inn_369 LIMIT 1) FROM tbl_02 AS sub_363 LIMIT 1), 0)) THEN ((t_ref_343.col_1 IN (SELECT sub_370.col_1 FROM tbl_05 AS sub_370)) - t_ref_343.id) ELSE (LOWER(EXISTS (SELECT 1 FROM tbl_10 AS sub_371 WHERE sub_371.id)) - EXISTS (SELECT 1 FROM tbl_05 AS sub_377 WHERE sub_377.id)) END)))::TEXT) - (t_ref_343.col_1 OR t_ref_343.id)) AS c_384,
    LOWER((EXISTS (SELECT 1 FROM tbl_09 AS sub_385 WHERE -2147483648)::INT::TEXT)) AS c_389,
    ((t_ref_343.col_1 / NULLIF((UPPER(((FALSE > ((t_ref_343.col_2 * ((EXISTS (SELECT 1 FROM tbl_02 AS sub_390 WHERE FALSE) = t_ref_343.id) = (UPPER(((SELECT (SELECT inn_395.col_3 FROM tbl_04 AS inn_395 LIMIT 1) FROM sensor_ranges AS sub_392 LIMIT 1) / NULLIF(((SELECT (SELECT -1.5 FROM tbl_07 AS inn_398 LIMIT 1) FROM tbl_01 AS sub_396 LIMIT 1) / NULLIF(((t_ref_343.id / NULLIF((SELECT (SELECT inn_400.id FROM tbl_10 AS inn_400 LIMIT 1) FROM sensor_ranges AS sub_399 LIMIT 1), 0))::NUMERIC(10,2)), 0)), 0))) + (SELECT LOWER((SELECT ('escaped''quote' = NULL) FROM tbl_09 AS sub_402 LIMIT 1)) FROM sensor_ranges AS sub_401 LIMIT 1)))) * ((SELECT -1.5 FROM tbl_02 AS sub_403 LIMIT 1) + ((SELECT NULL FROM sensor_ranges AS sub_404 LIMIT 1)::TEXT)))) + 1)) + (NULL::INT)), 0)) AND ((LENGTH((100 <> ((SELECT (LOWER(sub_405.id)::TEXT::VARCHAR(32)) FROM tbl_06 AS sub_405 LIMIT 1) * TRUE))) / NULLIF(((NOT ((9223372036854775807 IN (SELECT sub_406.ref_tbl_03_id FROM tbl_06 AS sub_406))))::TEXT), 0)) / NULLIF(t_ref_343.col_3, 0))) AS c_414
FROM tbl_09 AS t_ref_343 LEFT JOIN tbl_02 AS j_ref_415 ON t_ref_343.id = j_ref_415.id
WHERE ((FALSE = (((t_ref_343.id IN (SELECT sub_416.col_2 FROM tbl_04 AS sub_416)) < t_ref_343.col_1)::BOOLEAN))::NUMERIC(10,2))
LIMIT 2;

ALTER TABLE tbl_06 DROP COLUMN col_2;

ALTER TABLE tbl_07 ADD COLUMN dyn_col_4 VARCHAR(32);

INSERT INTO tbl_08 (id, col_1, col_2) VALUES (608264, -9223372036854775808, 0);

INSERT INTO tbl_06 (id, col_1, col_3, ref_tbl_03_id) VALUES (762023, 'escaped''quote', 'ref_val', 9223372036854775807);

SELECT
    (((t_ref_419.id IN (SELECT sub_420.col_2 FROM tbl_01 AS sub_420))::INT::TEXT) - LOWER(t_ref_419.col_2)) AS c_425,
    LOWER(((((((LOWER(t_ref_419.col_1) < (CASE WHEN t_ref_419.id THEN UPPER(t_ref_419.col_2) ELSE t_ref_419.id END))::INT) * (SELECT (SELECT FALSE FROM tbl_04 AS inn_427 LIMIT 1) FROM tbl_10 AS sub_426 LIMIT 1))::TEXT::VARCHAR(32))::TEXT) = (((EXISTS (SELECT 1 FROM sensor_ranges AS sub_428 WHERE sub_428.payload) / NULLIF((ABS((CASE WHEN (t_ref_419.col_2::INT) THEN 'chaos' ELSE t_ref_419.id END))::INT::TEXT), 0)) <> (((SELECT (SELECT inn_434.dyn_col_4 FROM tbl_07 AS inn_434 LIMIT 1) FROM tbl_10 AS sub_433 LIMIT 1) <= (t_ref_419.col_2 + ((LENGTH((NULLIF((NOT ((((SELECT (SELECT 'ref_val' FROM tbl_04 AS inn_436 LIMIT 1) FROM sensor_ranges AS sub_435 LIMIT 1)::INT) >= t_ref_419.col_1))), EXISTS (SELECT 1 FROM tbl_01 AS sub_437 WHERE '')) - ((t_ref_419.id < t_ref_419.col_2)::TEXT))) AND (t_ref_419.col_1 * (EXISTS (SELECT 1 FROM tbl_01 AS sub_438 WHERE sub_438.col_1) + FALSE))) <= ((SELECT (sub_439.payload IN (SELECT sub_440.col_1 FROM tbl_07 AS sub_440)) FROM sensor_ranges AS sub_439 LIMIT 1) - FALSE)))) BETWEEN 1 AND -9223372036854775808)) * (t_ref_419.col_1 IN (SELECT sub_441.col_1 FROM tbl_03 AS sub_441))))) AS c_446
FROM tbl_08 AS t_ref_419 LEFT JOIN tbl_03 AS j_ref_447 ON t_ref_419.id = j_ref_447.id
WHERE ('alpha_beta_gamma' < t_ref_419.col_2)
LIMIT 32;

ALTER TABLE tbl_10 ADD COLUMN dyn_col_6 VARCHAR(32);

SELECT
    NULLIF((t_ref_448.col_2 <= (NOT ((t_ref_448.ref_tbl_02_id = t_ref_448.col_3)))), (t_ref_448.id IN (SELECT sub_449.col_1 FROM tbl_08 AS sub_449))) AS c_450,
    ((SELECT TRUE FROM tbl_02 AS sub_451 LIMIT 1)::TEXT::VARCHAR(32)) AS c_452
FROM tbl_05 AS t_ref_448 LEFT JOIN tbl_06 AS j_ref_453 ON t_ref_448.id = j_ref_453.id
WHERE (CASE WHEN t_ref_448.col_1 THEN 'ref_val' ELSE t_ref_448.col_3 END)
LIMIT 50;

INSERT INTO sensor_ranges (record_id, station_id, operating_interval) VALUES
    (1, 101, '[10, 20)'::int4range),
    (2, 101, 'empty'::int4range),
    (3, 102, NULL),
    (4, 102, '[50, 100)'::int4range),
    (5, 103, 'empty'::int4range),
    (6, 103, '[200, 300)'::int4range);

INSERT INTO tbl_07 (id, col_1, col_2, dyn_col_4) VALUES (3472, -9223372036854775808, 2147483647, '__fuzz_marker__');

ALTER TABLE tbl_02 DROP COLUMN col_2;

ALTER TABLE tbl_03 DROP COLUMN col_1;

SELECT
    (UPPER(t_ref_454.col_3) >= UPPER(t_ref_454.col_4)) AS c_455,
    COALESCE(FALSE, (t_ref_454.col_4 AND ('chaos' / NULLIF((NOT (((t_ref_454.col_4 + 1.0) - ABS((COALESCE(t_ref_454.col_3, ((((0.0000000001 / NULLIF((100 AND (UPPER(((FALSE - t_ref_454.col_3) / NULLIF((t_ref_454.col_4::BOOLEAN), 0))) > (CASE WHEN ((CASE WHEN 'chaos' THEN 0.0000000001 ELSE TRUE END) / NULLIF((t_ref_454.col_1 + t_ref_454.col_1), 0)) THEN ((NOT (NULL)) + ('ref_val'::INT::TEXT)) ELSE NULL END))), 0))::INT) <= UPPER(t_ref_454.col_3)) <= t_ref_454.col_3))::TEXT))))), 0)))) AS c_456
FROM tbl_02 AS t_ref_454 LEFT JOIN tbl_10 AS j_ref_457 ON t_ref_454.id = j_ref_457.id
WHERE ((ABS(t_ref_454.id) + (-1e-5 >= (((LOWER((t_ref_454.col_1 / NULLIF((t_ref_454.id * LENGTH(LENGTH((CASE WHEN t_ref_454.id THEN 3.14159265 ELSE (t_ref_454.col_1 <> ((CASE WHEN (NULLIF('alpha_beta_gamma', 'alpha_beta_gamma') / NULLIF((SELECT (SELECT inn_459.col_1 FROM tbl_02 AS inn_459 LIMIT 1) FROM tbl_04 AS sub_458 LIMIT 1), 0)) THEN ABS((FALSE > t_ref_454.col_4)) ELSE t_ref_454.col_1 END)::TEXT::VARCHAR(32))) END)))), 0))) BETWEEN 42 AND 100)::INT::TEXT)::TEXT))) * NULLIF((NOT (((SELECT (((sub_460.id >= (sub_460.id IN (SELECT sub_461.ref_tbl_03_id FROM tbl_06 AS sub_461)))::TEXT)::INT::TEXT) FROM tbl_09 AS sub_460 LIMIT 1)::INT))), (SELECT (((SELECT (CASE WHEN (CASE WHEN LOWER(sub_463.col_1) THEN ABS(sub_463.col_2) ELSE (1.0 / NULLIF(sub_463.col_3, 0)) END) THEN EXISTS (SELECT 1 FROM tbl_09 AS sub_464 WHERE sub_464.col_1) ELSE sub_463.ref_tbl_02_id END) FROM tbl_05 AS sub_463 LIMIT 1) > LOWER(EXISTS (SELECT 1 FROM tbl_05 AS sub_465 WHERE sub_465.col_3)))::BOOLEAN) FROM tbl_07 AS sub_462 LIMIT 1)))
LIMIT 2;

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

INSERT INTO tbl_10 (id, col_1, col_2, col_3, ref_tbl_05_id, dyn_col_6) VALUES (449513, 'escaped''quote', 2147483647, 1.0, -9223372036854775808, 'alpha_beta_gamma');

SELECT
    ((SELECT sub_467.col_1 FROM tbl_10 AS sub_467 LIMIT 1)::NUMERIC(10,2)) AS c_468,
    (TRUE IN (SELECT sub_469.id FROM tbl_04 AS sub_469)) AS c_472,
    (SELECT (SELECT (((CASE WHEN ((LENGTH(sub_474.col_1) / NULLIF((CASE WHEN COALESCE(TRUE, (CASE WHEN (sub_474.col_1 >= ((ABS((sub_474.id BETWEEN 1 AND 1)) * (CASE WHEN (sub_474.id::TEXT::VARCHAR(32)) THEN (0 BETWEEN 100 AND 0) ELSE (sub_474.col_1 < sub_474.col_1) END)) <= (EXISTS (SELECT 1 FROM tbl_02 AS sub_475 WHERE sub_475.col_1) - (sub_474.col_2 IN (SELECT sub_476.col_2 FROM tbl_03 AS sub_476))))) THEN (((SELECT sub_477.col_2 FROM tbl_04 AS sub_477 LIMIT 1) < (((-2147483648 < sub_474.col_2) / NULLIF(sub_474.col_1, 0)) - ABS(sub_474.col_1))) - (sub_474.col_2 AND ABS(EXISTS (SELECT 1 FROM sensor_ranges AS sub_478 WHERE sub_478.id)))) ELSE (NOT (NULLIF((sub_474.id IN (SELECT sub_479.col_3 FROM tbl_10 AS sub_479)), -1))) END)) THEN NULL ELSE sub_474.col_1 END), 0)) AND (LOWER((CASE WHEN ((NOT (LOWER((SELECT (sub_480.id - 9223372036854775807) FROM tbl_05 AS sub_480 LIMIT 1)))) - EXISTS (SELECT 1 FROM tbl_08 AS sub_481 WHERE NULL)) THEN ((SELECT sub_483.id FROM sensor_ranges AS sub_483 LIMIT 1) - (sub_474.col_1 OR (LOWER(((FALSE - TRUE)::INT)) + (sub_474.col_2 - (NULLIF(sub_474.id, NULL) + (sub_474.col_1 - sub_474.col_2)))))) ELSE LOWER(((sub_474.col_2 * (((SELECT (SELECT inn_485.col_1 FROM tbl_07 AS inn_485 LIMIT 1) FROM sensor_ranges AS sub_484 LIMIT 1) - (TRUE - sub_474.col_1)) < ((NOT (NULL)) * (sub_474.col_2 * 1e10)))) + sub_474.col_2)) END)) <= sub_474.col_1)) THEN sub_474.col_2 ELSE (((SELECT sub_486.col_1 FROM tbl_09 AS sub_486 LIMIT 1)::BOOLEAN)::TEXT::VARCHAR(32)) END)::NUMERIC(10,2)) / NULLIF((NULLIF((SELECT (SELECT inn_488.id FROM tbl_01 AS inn_488 LIMIT 1) FROM tbl_06 AS sub_487 LIMIT 1), sub_474.col_1) BETWEEN 0 AND 9223372036854775807), 0)) FROM tbl_08 AS sub_474 LIMIT 1) FROM tbl_07 AS sub_473 LIMIT 1) AS c_489
FROM tbl_05 AS t_ref_466 LEFT JOIN tbl_02 AS j_ref_490 ON t_ref_466.id = j_ref_490.id
WHERE (NOT ((3.14159265 * (((t_ref_466.col_2 IN (SELECT sub_491.col_4 FROM tbl_02 AS sub_491)) >= t_ref_466.ref_tbl_02_id) + EXISTS (SELECT 1 FROM tbl_06 AS sub_492 WHERE NULL)))))
LIMIT 20;

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 INT,
    col_3 JSONB
);

SELECT
    ABS(EXISTS (SELECT 1 FROM tbl_02 AS sub_504 WHERE TRUE)) AS c_505,
    EXISTS (SELECT 1 FROM tbl_11 AS sub_506 WHERE sub_506.col_1) AS c_510,
    (UPPER((EXISTS (SELECT 1 FROM tbl_10 AS sub_511 WHERE sub_511.col_2)::TEXT)) - TRUE) AS c_532
FROM sensor_ranges AS t_ref_503
WHERE t_ref_503.payload
LIMIT 33;

BEGIN;
INSERT INTO tbl_10 (id, col_1, col_2, col_3, ref_tbl_05_id, dyn_col_6) VALUES (975496, 'ref_val', 1, 0.0, -2147483648, 'alpha_beta_gamma');
INSERT INTO tbl_10 (id, col_1, col_2, col_3, ref_tbl_05_id, dyn_col_6) VALUES (101290, 'escaped''quote', 9223372036854775807, -1.5, 100, 'alpha_beta_gamma');
COMMIT;

ALTER TABLE tbl_03 ADD COLUMN dyn_col_3 VARCHAR(32);

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (192078, FALSE, TRUE, 'chaos', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -9223372036854775808);
INSERT INTO tbl_07 (id, col_1, col_2, dyn_col_4) VALUES (597714, 42, 0, 'escaped''quote');
COMMIT;

BEGIN;
INSERT INTO sensor_ranges (id, payload) VALUES (777818, 'alpha_beta_gamma');
COMMIT;

SELECT
    (SELECT (SELECT (NULL * NULLIF(NULL, sub_535.id)) FROM tbl_07 AS sub_535 LIMIT 1) FROM tbl_05 AS sub_534 LIMIT 1) AS c_536,
    ((CASE WHEN ((t_ref_533.ref_tbl_03_id::NUMERIC(10,2)) * t_ref_533.col_1) THEN -1.5 ELSE (SELECT (FALSE BETWEEN -1 AND 9223372036854775807) FROM sensor_ranges AS sub_537 LIMIT 1) END)::INT) AS c_538
FROM tbl_06 AS t_ref_533
WHERE ((((t_ref_533.id / NULLIF(((((((EXISTS (SELECT 1 FROM tbl_01 AS sub_539 WHERE sub_539.col_2) - t_ref_533.ref_tbl_03_id)::TEXT::VARCHAR(32)) OR (t_ref_533.id + (t_ref_533.ref_tbl_03_id::TEXT)))::NUMERIC(10,2)) + TRUE) / NULLIF(t_ref_533.id, 0)), 0)) IS NOT NULL) / NULLIF(t_ref_533.ref_tbl_03_id, 0)) <> ((NULLIF(t_ref_533.col_1, ((EXISTS (SELECT 1 FROM tbl_04 AS sub_540 WHERE sub_540.id) > ABS(t_ref_533.id)) / NULLIF(((EXISTS (SELECT 1 FROM tbl_10 AS sub_543 WHERE sub_543.col_1) BETWEEN -2147483648 AND -2147483648)::BOOLEAN), 0))) <= (FALSE IN (SELECT sub_545.dyn_col_3 FROM tbl_03 AS sub_545))) IS NOT NULL))
LIMIT 34;

INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (353291, -1.5, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'ref_val');

INSERT INTO tbl_03 (id, col_2, dyn_col_3) VALUES (626494, TRUE, 'alpha_beta_gamma');

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_3, col_4) VALUES (468915, FALSE, 999999999.99, '{}');
INSERT INTO tbl_08 (id, col_1, col_2) VALUES (467790, 0, 2147483647);
INSERT INTO tbl_07 (id, col_1, col_2, dyn_col_4) VALUES (12680, -2147483648, 100, 'chaos');
COMMIT;

SELECT brin_summarize_new_values('idx_sensor_ranges_brin');

INSERT INTO tbl_08 (id, col_1, col_2) VALUES (704783, -2147483648, 2147483647);

UPDATE tbl_04 SET col_3 = 1.0 WHERE id > 31;

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (385573, '__fuzz_marker__', '__fuzz_marker__', '{}');

CREATE INDEX idx_tbl_09_col_1_2 ON tbl_09 (col_1);

UPDATE tbl_10 SET col_2 = 1 WHERE id > 13;

SELECT
    (SELECT (SELECT FALSE FROM tbl_10 AS inn_553 LIMIT 1) FROM tbl_04 AS sub_552 LIMIT 1) AS c_554,
    ((SELECT FALSE FROM tbl_06 AS sub_555 LIMIT 1) >= (CASE WHEN ((CASE WHEN (EXISTS (SELECT 1 FROM tbl_03 AS sub_556 WHERE sub_556.id) <> (3.14159265 IN (SELECT sub_557.id FROM tbl_09 AS sub_557))) THEN LENGTH(TRUE) ELSE t_ref_551.id END) <= UPPER(LOWER(t_ref_551.payload))) THEN (LOWER((SELECT sub_567.id FROM tbl_10 AS sub_567 LIMIT 1)) + (CASE WHEN t_ref_551.payload THEN (CASE WHEN ((LOWER(((((CASE WHEN TRUE THEN '__fuzz_marker__' ELSE EXISTS (SELECT 1 FROM tbl_07 AS sub_568 WHERE NULL) END) / NULLIF(t_ref_551.id, 0)) IS NOT NULL) <> FALSE)) * TRUE) + EXISTS (SELECT 1 FROM tbl_09 AS sub_569 WHERE sub_569.col_3)) THEN TRUE ELSE (SELECT (CASE WHEN EXISTS (SELECT 1 FROM tbl_08 AS sub_571 WHERE FALSE) THEN (-2147483648::TEXT::VARCHAR(32)) ELSE COALESCE(((((((TRUE IN (SELECT sub_573.col_1 FROM tbl_07 AS sub_573)) + NULLIF((UPPER(-1.5) + sub_570.id), ((sub_570.col_3 IN (SELECT sub_574.payload FROM sensor_ranges AS sub_574)) AND 'alpha_beta_gamma'))) >= (((SELECT (SELECT NULL FROM tbl_01 AS inn_576 LIMIT 1) FROM tbl_07 AS sub_575 LIMIT 1) + ((sub_570.id * sub_570.col_1) = sub_570.id)) = EXISTS (SELECT 1 FROM tbl_07 AS sub_577 WHERE sub_577.id))) IS NULL)::INT) / NULLIF(((100 IN (SELECT sub_578.col_2 FROM tbl_01 AS sub_578)) > NULLIF((FALSE BETWEEN 2147483647 AND 100), (0.0 IN (SELECT sub_580.col_2 FROM tbl_04 AS sub_580)))), 0)), 1.0) END) FROM tbl_06 AS sub_570 LIMIT 1) END) ELSE t_ref_551.id END)) ELSE (SELECT (SELECT inn_613.col_2 FROM tbl_03 AS inn_613 LIMIT 1) FROM tbl_11 AS sub_581 LIMIT 1) END)) AS c_614,
    (((SELECT (SELECT inn_616.id FROM tbl_06 AS inn_616 LIMIT 1) FROM tbl_05 AS sub_615 LIMIT 1)::TEXT::VARCHAR(32)) + t_ref_551.payload) AS c_617
FROM sensor_ranges AS t_ref_551 LEFT JOIN tbl_03 AS j_ref_618 ON t_ref_551.id = j_ref_618.id
WHERE (SELECT sub_619.col_1 FROM tbl_05 AS sub_619 LIMIT 1)
LIMIT 14;

INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (327846, -2147483648, 'chaos', -1e-5);

CREATE INDEX idx_tbl_11_col_1_3 ON tbl_11 (col_1);

DELETE FROM tbl_03 WHERE id = 92420;

SELECT
    (UPPER(((CASE WHEN FALSE THEN ((SELECT (SELECT (SELECT TRUE FROM tbl_08 AS inn_634 LIMIT 1) FROM tbl_06 AS sub_622 LIMIT 1) FROM tbl_08 AS sub_621 LIMIT 1) - (SELECT (CASE WHEN (((SELECT (SELECT 1.0 FROM tbl_08 AS inn_641 LIMIT 1) FROM tbl_05 AS sub_636 LIMIT 1) * (sub_635.col_2 IN (SELECT sub_642.id FROM tbl_02 AS sub_642))) * sub_635.id) THEN sub_635.id ELSE ((((CASE WHEN (EXISTS (SELECT 1 FROM tbl_10 AS sub_644 WHERE sub_644.id) < LENGTH(((SELECT (SELECT 'alpha_beta_gamma' FROM tbl_06 AS inn_646 LIMIT 1) FROM tbl_07 AS sub_645 LIMIT 1) < LENGTH(sub_635.id)))) THEN (CASE WHEN ((sub_635.col_3::NUMERIC(10,2)) - (CASE WHEN ((sub_635.col_2 - 'escaped''quote')::TEXT::VARCHAR(32)) THEN (NOT (ABS(sub_635.col_2))) ELSE LOWER(UPPER('chaos')) END)) THEN (TRUE - sub_635.col_3) ELSE sub_635.col_1 END) ELSE (SELECT (SELECT FALSE FROM tbl_05 AS inn_648 LIMIT 1) FROM tbl_01 AS sub_647 LIMIT 1) END) = (SELECT (SELECT TRUE FROM tbl_02 AS inn_650 LIMIT 1) FROM tbl_04 AS sub_649 LIMIT 1))::BOOLEAN) BETWEEN 42 AND 100) END) FROM tbl_01 AS sub_635 LIMIT 1)) ELSE (SELECT (SELECT TRUE FROM tbl_01 AS inn_652 LIMIT 1) FROM tbl_03 AS sub_651 LIMIT 1) END) IS NOT NULL)) + (LENGTH(EXISTS (SELECT 1 FROM tbl_04 AS sub_653 WHERE sub_653.col_3)) / NULLIF(((TRUE IS NULL) - (SELECT (SELECT inn_661.id FROM sensor_ranges AS inn_661 LIMIT 1) FROM tbl_03 AS sub_654 LIMIT 1)), 0))) AS c_662,
    (t_ref_620.ref_tbl_03_id IN (SELECT sub_663.ref_tbl_03_id FROM tbl_06 AS sub_663)) AS c_674
FROM tbl_06 AS t_ref_620
WHERE (t_ref_620.col_3 IN (SELECT sub_675.col_1 FROM tbl_02 AS sub_675))
LIMIT 11;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2, dyn_col_4) VALUES (478201, -1, 2147483647, '__fuzz_marker__');
INSERT INTO sensor_ranges (id, payload) VALUES (704521, '__fuzz_marker__');
COMMIT;

CREATE INDEX idx_tbl_11_col_3_4 ON tbl_11 (col_3);

INSERT INTO tbl_11 (id, col_1, col_2, col_3) VALUES (347714, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 0, '{}');

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_3, ref_tbl_03_id) VALUES (3275, 'alpha_beta_gamma', 'ref_val', 42);
COMMIT;

UPDATE tbl_04 SET col_3 = 3.14159265 WHERE id > 77;

SELECT
    t_ref_707.col_3 AS c_708,
    (EXISTS (SELECT 1 FROM tbl_04 AS sub_709 WHERE FALSE) AND (NOT ((SELECT sub_712.id FROM tbl_01 AS sub_712 LIMIT 1)))) AS c_713
FROM tbl_01 AS t_ref_707 LEFT JOIN tbl_08 AS j_ref_714 ON t_ref_707.id = j_ref_714.id
WHERE EXISTS (SELECT 1 FROM tbl_04 AS sub_715 WHERE FALSE)
LIMIT 38;

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2)
);

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64)
);

BEGIN;
INSERT INTO tbl_13 (id, col_1, col_2, col_3) VALUES (580552, TRUE, 3.14159265, 'alpha_beta_gamma');
COMMIT;

INSERT INTO sensor_ranges (id, payload) VALUES (452383, 'escaped''quote');

SELECT
    t_ref_730.col_3 AS c_731,
    NULL AS c_732
FROM tbl_05 AS t_ref_730
WHERE (t_ref_730.id - 'ref_val')
LIMIT 46;

SELECT
    t_ref_733.col_2 AS c_734,
    (((SELECT (SELECT inn_741.id FROM tbl_06 AS inn_741 LIMIT 1) FROM tbl_06 AS sub_735 LIMIT 1) IS NOT NULL)::INT) AS c_742
FROM tbl_12 AS t_ref_733 LEFT JOIN tbl_10 AS j_ref_743 ON t_ref_733.id = j_ref_743.id
WHERE (((SELECT UPPER(LOWER(sub_744.payload)) FROM sensor_ranges AS sub_744 LIMIT 1) AND ((SELECT (SELECT inn_746.id FROM tbl_06 AS inn_746 LIMIT 1) FROM tbl_07 AS sub_745 LIMIT 1) / NULLIF((EXISTS (SELECT 1 FROM tbl_11 AS sub_747 WHERE sub_747.col_3) < (SELECT UPPER(NULLIF(((UPPER(sub_748.id) * (CASE WHEN sub_748.dyn_col_3 THEN TRUE ELSE 'escaped''quote' END))::BOOLEAN), (LOWER(UPPER((NULL - ABS(sub_748.col_2)))) + EXISTS (SELECT 1 FROM tbl_11 AS sub_749 WHERE NULL)))) FROM tbl_03 AS sub_748 LIMIT 1)), 0))) >= ((t_ref_733.id::NUMERIC(10,2))::INT))
LIMIT 12;

CREATE INDEX idx_tbl_04_id_5 ON tbl_04 (id);

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSONB
);

DELETE FROM sensor_ranges WHERE id = 38995;

UPDATE tbl_06 SET col_3 = '__fuzz_marker__' WHERE id > 39;

INSERT INTO tbl_12 (id, col_1, col_2, col_3) VALUES (996595, FALSE, 1.0, -1e-5);

SELECT
    t_ref_750.id AS c_751,
    (t_ref_750.col_1 * (t_ref_750.col_2::BOOLEAN)) AS c_752,
    (999999999.99 / NULLIF(((LENGTH((CASE WHEN (SELECT (NOT ((LENGTH(sub_753.id) / NULLIF('', 0)))) FROM tbl_01 AS sub_753 LIMIT 1) THEN ((EXISTS (SELECT 1 FROM tbl_01 AS sub_754 WHERE sub_754.col_3)::INT)::TEXT::VARCHAR(32)) ELSE (ABS((t_ref_750.id::NUMERIC(10,2))) - (((SELECT (SELECT 9223372036854775807 FROM tbl_10 AS inn_765 LIMIT 1) FROM tbl_01 AS sub_762 LIMIT 1) IS NOT NULL) * (3.14159265 = (((((t_ref_750.col_2 IN (SELECT sub_766.col_3 FROM tbl_09 AS sub_766)) / NULLIF((((((NULL <= 0) <> t_ref_750.id)::INT) - 1) - t_ref_750.col_1), 0)) <= (SELECT (NULL AND (sub_767.col_1 / NULLIF(UPPER(NULLIF(sub_767.col_3, sub_767.col_1)), 0))) FROM tbl_05 AS sub_767 LIMIT 1)) / NULLIF(t_ref_750.id, 0))::NUMERIC(10,2))))) END))::NUMERIC(10,2))::TEXT), 0)) AS c_768
FROM tbl_14 AS t_ref_750 LEFT JOIN tbl_02 AS j_ref_769 ON t_ref_750.id = j_ref_769.id
WHERE t_ref_750.col_2
LIMIT 11;

INSERT INTO tbl_07 (id, col_1, col_2, dyn_col_4) VALUES (263437, 42, 1, 'alpha_beta_gamma');

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (897713, FALSE, TRUE, '', '[]', 2147483647);
COMMIT;

INSERT INTO tbl_06 (id, col_1, col_3, ref_tbl_03_id) VALUES (426498, 'ref_val', 'ref_val', 1);

DELETE FROM tbl_09 WHERE id = 73869;

INSERT INTO tbl_02 (id, col_1, col_3, col_4) VALUES (894380, FALSE, 1e10, '{}');

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT,
    col_3 VARCHAR(64)
);

INSERT INTO tbl_12 (id, col_1, col_2, col_3) VALUES (506261, TRUE, 0.0000000001, 0.0);

DELETE FROM tbl_12 WHERE id = 60837;

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (409135, 'alpha_beta_gamma', 'chaos', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

INSERT INTO tbl_10 (id, col_1, col_2, col_3, ref_tbl_05_id, dyn_col_6) VALUES (116270, 'ref_val', 1, -1.5, 0, 'chaos');

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT,
    col_3 VARCHAR(64),
    col_4 VARCHAR(64),
    ref_tbl_15_id INT,
    FOREIGN KEY (ref_tbl_15_id) REFERENCES tbl_15(id)
);

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_10 WHERE id IS NOT NULL;

INSERT INTO tbl_15 (id, col_1, col_2, col_3) VALUES (375334, -9223372036854775808, 1, 'alpha_beta_gamma');

INSERT INTO tbl_08 (id, col_1, col_2) VALUES (462444, 0, -9223372036854775808);

CREATE TABLE sensor_ranges (
    record_id INT PRIMARY KEY,
    station_id INT NOT NULL,
    operating_interval INT4RANGE
);

SELECT record_id, station_id, operating_interval
FROM sensor_ranges
WHERE operating_interval && '[15, 25)'::int4range;

SELECT
    (CASE WHEN (CASE WHEN (((t_ref_770.ref_tbl_02_id / NULLIF(FALSE, 0)) = t_ref_770.id) - (((SELECT (SELECT inn_772.col_3 FROM tbl_11 AS inn_772 LIMIT 1) FROM tbl_11 AS sub_771 LIMIT 1) <> (SELECT ((SELECT (SELECT 1e10 FROM tbl_02 AS inn_775 LIMIT 1) FROM tbl_01 AS sub_774 LIMIT 1)::NUMERIC(10,2)) FROM sensor_ranges AS sub_773 LIMIT 1)) - TRUE)) THEN ((SELECT (SELECT FALSE FROM tbl_08 AS inn_781 LIMIT 1) FROM tbl_03 AS sub_776 LIMIT 1) OR 0.0) ELSE (SELECT COALESCE(((NOT ((LENGTH((COALESCE(((CASE WHEN ABS(((EXISTS (SELECT 1 FROM tbl_02 AS sub_783 WHERE sub_783.col_1) - 1e10)::INT::TEXT)) THEN (SELECT (SELECT inn_786.dyn_col_4 FROM tbl_07 AS inn_786 LIMIT 1) FROM tbl_11 AS sub_784 LIMIT 1) ELSE ((SELECT (SELECT inn_790.ref_tbl_03_id FROM tbl_06 AS inn_790 LIMIT 1) FROM tbl_07 AS sub_787 LIMIT 1)::INT::TEXT) END) - (NULLIF((SELECT (CASE WHEN sub_791.dyn_col_4 THEN sub_791.col_2 ELSE TRUE END) FROM tbl_07 AS sub_791 LIMIT 1), (ABS((sub_782.col_2 * TRUE)) <= (SELECT sub_792.col_1 FROM tbl_04 AS sub_792 LIMIT 1))) = ((SELECT (SELECT 3.14159265 FROM sensor_ranges AS inn_794 LIMIT 1) FROM tbl_07 AS sub_793 LIMIT 1) / NULLIF((0.0000000001 BETWEEN -2147483648 AND 42), 0)))), LENGTH(sub_782.id)) / NULLIF((sub_782.col_2::INT::TEXT), 0))) <> (((sub_782.id IN (SELECT sub_795.id FROM tbl_06 AS sub_795)) OR ((((sub_782.id IN (SELECT sub_796.id FROM tbl_02 AS sub_796)) / NULLIF(sub_782.col_2, 0)) IS NOT NULL) * LENGTH(sub_782.dyn_col_3)))::BOOLEAN)))) * (SELECT (((SELECT (SELECT inn_804.col_3 FROM tbl_11 AS inn_804 LIMIT 1) FROM tbl_10 AS sub_800 LIMIT 1) <= sub_799.col_3) - (NULL IN (SELECT sub_805.col_1 FROM tbl_08 AS sub_805))) FROM tbl_16 AS sub_799 LIMIT 1)), (CASE WHEN (((sub_782.dyn_col_3 > (SELECT (SELECT inn_807.col_4 FROM tbl_16 AS inn_807 LIMIT 1) FROM tbl_01 AS sub_806 LIMIT 1)) / NULLIF(((EXISTS (SELECT 1 FROM tbl_13 AS sub_808 WHERE '__fuzz_marker__')::NUMERIC(10,2)) IS NOT NULL), 0)) <> sub_782.dyn_col_3) THEN (sub_782.col_2 > -1.5) ELSE ((((sub_782.col_2::INT::TEXT)::INT::TEXT) < COALESCE((sub_782.dyn_col_3 BETWEEN 0 AND 42), sub_782.id)) - NULLIF((SELECT (SELECT inn_813.col_1 FROM tbl_01 AS inn_813 LIMIT 1) FROM tbl_01 AS sub_812 LIMIT 1), EXISTS (SELECT 1 FROM tbl_13 AS sub_814 WHERE 0.0000000001))) END)) FROM tbl_03 AS sub_782 LIMIT 1) END) THEN ABS((-2147483648 IN (SELECT sub_819.col_1 FROM tbl_08 AS sub_819))) ELSE (((t_ref_770.ref_tbl_02_id / NULLIF(LOWER((((((NULLIF((CASE WHEN (SELECT (SELECT 'escaped''quote' FROM sensor_ranges AS inn_825 LIMIT 1) FROM tbl_05 AS sub_824 LIMIT 1) THEN (LOWER((SELECT (sub_826.col_1 - FALSE) FROM tbl_01 AS sub_826 LIMIT 1)) BETWEEN -9223372036854775808 AND -2147483648) ELSE (CASE WHEN t_ref_770.col_1 THEN ((SELECT (sub_827.id IN (SELECT sub_828.col_3 FROM tbl_12 AS sub_828)) FROM tbl_15 AS sub_827 LIMIT 1) / NULLIF((ABS(t_ref_770.col_1) - ((TRUE / NULLIF(t_ref_770.col_3, 0))::INT)), 0)) ELSE t_ref_770.col_4 END) END), t_ref_770.ref_tbl_02_id)::INT::TEXT)::NUMERIC(10,2)) / NULLIF(t_ref_770.ref_tbl_02_id, 0)) = (FALSE - LENGTH(FALSE))) AND (t_ref_770.col_2 = ABS((((((EXISTS (SELECT 1 FROM tbl_01 AS sub_829 WHERE sub_829.col_3) <> ((CASE WHEN (LENGTH(t_ref_770.ref_tbl_02_id) AND (CASE WHEN t_ref_770.col_2 THEN TRUE ELSE 1.0 END)) THEN EXISTS (SELECT 1 FROM tbl_15 AS sub_830 WHERE sub_830.id) ELSE (t_ref_770.col_4 IN (SELECT sub_831.id FROM tbl_15 AS sub_831)) END) - t_ref_770.col_4))::NUMERIC(10,2))::NUMERIC(10,2)) <= (t_ref_770.col_2 = ((((SELECT (SELECT inn_834.col_3 FROM tbl_06 AS inn_834 LIMIT 1) FROM tbl_12 AS sub_832 LIMIT 1)::INT) < (t_ref_770.id IN (SELECT sub_835.col_3 FROM tbl_05 AS sub_835))) * (t_ref_770.col_2 + ((CASE WHEN t_ref_770.ref_tbl_02_id THEN NULLIF(LOWER(t_ref_770.ref_tbl_02_id), t_ref_770.col_4) ELSE (t_ref_770.col_4::INT::TEXT) END) * (LOWER(t_ref_770.col_3) / NULLIF(((t_ref_770.id + NULL) / NULLIF((t_ref_770.ref_tbl_02_id::BOOLEAN), 0)), 0))))))) / NULLIF(ABS((t_ref_770.ref_tbl_02_id IN (SELECT sub_836.id FROM tbl_13 AS sub_836))), 0)))))), 0)) <= t_ref_770.col_4) IS NULL) END) AS c_840,
    t_ref_770.col_2 AS c_841,
    UPPER(t_ref_770.col_1) AS c_842
FROM tbl_05 AS t_ref_770
WHERE (LOWER((SELECT (SELECT inn_844.id FROM tbl_06 AS inn_844 LIMIT 1) FROM tbl_16 AS sub_843 LIMIT 1)) AND LOWER((((TRUE AND (t_ref_770.id OR (0.0000000001 <= LOWER((NOT (t_ref_770.ref_tbl_02_id))))))::BOOLEAN) + LOWER(t_ref_770.col_4))))
LIMIT 14;

INSERT INTO tbl_02 (id, col_1, col_3, col_4) VALUES (216184, TRUE, 1.0, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

SELECT
    (FALSE * ((SELECT EXISTS (SELECT 1 FROM tbl_05 AS sub_847 WHERE sub_847.col_2) FROM tbl_16 AS sub_846 LIMIT 1)::BOOLEAN)) AS c_860,
    ((CASE WHEN ((((SELECT (SELECT 1 FROM tbl_01 AS inn_871 LIMIT 1) FROM tbl_05 AS sub_861 LIMIT 1) - ((((SELECT ((sub_872.col_3 / NULLIF((SELECT (SELECT TRUE FROM tbl_15 AS inn_876 LIMIT 1) FROM tbl_06 AS sub_873 LIMIT 1), 0)) AND (((UPPER(((sub_872.col_2 OR -1) OR LENGTH(sub_872.col_3))) IS NULL) BETWEEN -2147483648 AND 1)::TEXT)) FROM tbl_11 AS sub_872 LIMIT 1) = t_ref_845.id)::BOOLEAN) - COALESCE((EXISTS (SELECT 1 FROM tbl_12 AS sub_877 WHERE TRUE) / NULLIF(t_ref_845.ref_tbl_03_id, 0)), (((FALSE IN (SELECT sub_878.col_1 FROM tbl_08 AS sub_878)) OR ((((t_ref_845.id <= ((t_ref_845.id IN (SELECT sub_879.col_3 FROM tbl_05 AS sub_879))::INT))::TEXT) AND (SELECT sub_880.id FROM tbl_07 AS sub_880 LIMIT 1))::TEXT)) + t_ref_845.col_3))))::NUMERIC(10,2))::NUMERIC(10,2)) THEN ((COALESCE(((COALESCE(t_ref_845.id, TRUE)::INT)::TEXT::VARCHAR(32)), ((t_ref_845.ref_tbl_03_id <> ((SELECT (SELECT NULL FROM tbl_06 AS inn_882 LIMIT 1) FROM tbl_16 AS sub_881 LIMIT 1) + ((0 - (t_ref_845.id IN (SELECT sub_883.col_2 FROM tbl_12 AS sub_883))) = t_ref_845.col_3))) BETWEEN -2147483648 AND -1)) * (NULL IN (SELECT sub_884.col_1 FROM tbl_05 AS sub_884))) / NULLIF((LOWER((('__fuzz_marker__' IS NULL) <> -1e-5))::BOOLEAN), 0)) ELSE (NOT (((NOT (ABS((t_ref_845.id IN (SELECT sub_885.id FROM sensor_ranges AS sub_885))))) > t_ref_845.ref_tbl_03_id))) END)::NUMERIC(10,2)) AS c_886,
    ((COALESCE(((CASE WHEN (t_ref_845.id IN (SELECT sub_887.col_1 FROM tbl_14 AS sub_887)) THEN (0.0000000001 - (SELECT (CASE WHEN NULLIF(EXISTS (SELECT 1 FROM tbl_10 AS sub_889 WHERE NULL), ((0 / NULLIF((SELECT (sub_890.col_4 + (SELECT sub_891.col_2 FROM tbl_03 AS sub_891 LIMIT 1)) FROM tbl_16 AS sub_890 LIMIT 1), 0)) * (LENGTH(TRUE) / NULLIF(((sub_888.id::BOOLEAN) AND sub_888.ref_tbl_03_id), 0)))) THEN (SELECT (SELECT inn_893.col_1 FROM tbl_04 AS inn_893 LIMIT 1) FROM tbl_09 AS sub_892 LIMIT 1) ELSE sub_888.col_1 END) FROM tbl_06 AS sub_888 LIMIT 1)) ELSE ((CASE WHEN (t_ref_845.col_1 IN (SELECT sub_894.id FROM tbl_06 AS sub_894)) THEN t_ref_845.col_3 ELSE (1.0 IN (SELECT sub_897.col_2 FROM tbl_13 AS sub_897)) END) / NULLIF(LOWER(LOWER(((NOT ((SELECT (SELECT NULL FROM tbl_05 AS inn_917 LIMIT 1) FROM tbl_12 AS sub_916 LIMIT 1))) IS NULL))), 0)) END) <= COALESCE(((SELECT (((((EXISTS (SELECT 1 FROM tbl_11 AS sub_919 WHERE sub_919.col_2) < (((SELECT sub_921.payload FROM sensor_ranges AS sub_921 LIMIT 1) * sub_918.id) <> sub_918.ref_tbl_03_id)) AND sub_918.col_1) / NULLIF((SELECT ((SELECT (SELECT NULL FROM tbl_11 AS inn_924 LIMIT 1) FROM tbl_04 AS sub_923 LIMIT 1) + ((EXISTS (SELECT 1 FROM tbl_05 AS sub_925 WHERE '__fuzz_marker__') / NULLIF((sub_922.ref_tbl_02_id::TEXT::VARCHAR(32)), 0)) + (NOT ('chaos')))) FROM tbl_05 AS sub_922 LIMIT 1), 0)) / NULLIF((UPPER(LOWER((((sub_918.id IN (SELECT sub_926.ref_tbl_15_id FROM tbl_16 AS sub_926)) + (SELECT sub_927.col_1 FROM tbl_04 AS sub_927 LIMIT 1)) OR ((('ref_val' OR sub_918.ref_tbl_03_id)::TEXT) BETWEEN 2147483647 AND 1)))) - EXISTS (SELECT 1 FROM tbl_09 AS sub_928 WHERE NULL)), 0)) + FALSE) FROM tbl_06 AS sub_918 LIMIT 1) + (t_ref_845.ref_tbl_03_id * ((LENGTH(((t_ref_845.col_3 IN (SELECT sub_929.col_1 FROM tbl_13 AS sub_929)) - ((SELECT ABS(3.14159265) FROM tbl_13 AS sub_930 LIMIT 1) - t_ref_845.id))) >= (-9223372036854775808 BETWEEN -9223372036854775808 AND -2147483648))::BOOLEAN))), (NULLIF(-9223372036854775808, (((-9223372036854775808::NUMERIC(10,2)) >= (((((LOWER(((t_ref_845.ref_tbl_03_id + t_ref_845.col_1) IS NOT NULL))::TEXT) * EXISTS (SELECT 1 FROM tbl_07 AS sub_931 WHERE sub_931.dyn_col_4))::TEXT::VARCHAR(32)) - (CASE WHEN ((((t_ref_845.ref_tbl_03_id - (SELECT sub_932.col_1 FROM tbl_08 AS sub_932 LIMIT 1)) > NULL) BETWEEN 0 AND 0) - LENGTH((CASE WHEN t_ref_845.id THEN ABS(-1.5) ELSE (('chaos' - t_ref_845.col_3)::BOOLEAN) END))) THEN (SELECT (SELECT 0 FROM tbl_01 AS inn_935 LIMIT 1) FROM tbl_12 AS sub_933 LIMIT 1) ELSE LOWER(('escaped''quote' IN (SELECT sub_936.payload FROM sensor_ranges AS sub_936))) END))::INT))::BOOLEAN))::NUMERIC(10,2)))), NULLIF(((SELECT (SELECT TRUE FROM tbl_12 AS inn_952 LIMIT 1) FROM tbl_05 AS sub_938 LIMIT 1) = (((t_ref_845.ref_tbl_03_id AND (t_ref_845.ref_tbl_03_id / NULLIF(t_ref_845.col_3, 0)))::INT::TEXT) - ((SELECT (SELECT inn_954.dyn_col_3 FROM tbl_03 AS inn_954 LIMIT 1) FROM tbl_16 AS sub_953 LIMIT 1) AND (t_ref_845.col_1 IN (SELECT sub_955.col_2 FROM tbl_12 AS sub_955))))), ((CASE WHEN EXISTS (SELECT 1 FROM tbl_14 AS sub_959 WHERE 'ref_val') THEN (SELECT (((sub_960.col_2 - (SELECT (SELECT (SELECT inn_964.id FROM sensor_ranges AS inn_964 LIMIT 1) FROM tbl_02 AS sub_962 LIMIT 1) FROM tbl_02 AS sub_961 LIMIT 1)) >= NULL) + EXISTS (SELECT 1 FROM tbl_07 AS sub_965 WHERE sub_965.id)) FROM tbl_05 AS sub_960 LIMIT 1) ELSE (t_ref_845.id <= t_ref_845.id) END) < (SELECT (SELECT inn_969.id FROM tbl_04 AS inn_969 LIMIT 1) FROM tbl_03 AS sub_968 LIMIT 1)))) > t_ref_845.col_1) / NULLIF((FALSE + (CASE WHEN (SELECT (SELECT (SELECT NULL FROM tbl_14 AS inn_982 LIMIT 1) FROM tbl_14 AS sub_971 LIMIT 1) FROM tbl_06 AS sub_970 LIMIT 1) THEN NULL ELSE EXISTS (SELECT 1 FROM tbl_03 AS sub_983 WHERE sub_983.col_2) END)), 0)) AS c_986
FROM tbl_06 AS t_ref_845
WHERE ((t_ref_845.col_1 IN (SELECT sub_987.payload FROM sensor_ranges AS sub_987))::TEXT::VARCHAR(32))
LIMIT 23;

UPDATE tbl_15 SET col_3 = '' WHERE id > 40;

INSERT INTO tbl_13 (id, col_1, col_2, col_3) VALUES (827397, TRUE, 3.14159265, '__fuzz_marker__');

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (21275, 999999999.99, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'alpha_beta_gamma');
INSERT INTO tbl_15 (id, col_1, col_2, col_3) VALUES (816503, 9223372036854775807, 100, '');
COMMIT;

SELECT
    (CASE WHEN (((t_ref_988.id::NUMERIC(10,2)) / NULLIF(UPPER((SELECT (-1e-5 * (((SELECT (sub_990.col_1 - (sub_990.id IN (SELECT sub_991.col_1 FROM tbl_05 AS sub_991))) FROM tbl_15 AS sub_990 LIMIT 1) + 2147483647)::TEXT)) FROM tbl_15 AS sub_989 LIMIT 1)), 0)) OR NULL) THEN (ABS(((SELECT (SELECT inn_995.id FROM sensor_ranges AS inn_995 LIMIT 1) FROM tbl_15 AS sub_992 LIMIT 1)::INT::TEXT)) - ((CASE WHEN t_ref_988.payload THEN (NULLIF(t_ref_988.payload, '')::BOOLEAN) ELSE (CASE WHEN EXISTS (SELECT 1 FROM tbl_07 AS sub_996 WHERE sub_996.col_1) THEN ((((EXISTS (SELECT 1 FROM tbl_08 AS sub_1001 WHERE sub_1001.id) - (NOT ((CASE WHEN UPPER(((t_ref_988.payload + (((t_ref_988.id + t_ref_988.id) >= t_ref_988.id) - (ABS(t_ref_988.payload) <= (SELECT (SELECT 100 FROM tbl_03 AS inn_1004 LIMIT 1) FROM tbl_02 AS sub_1003 LIMIT 1)))) IS NULL)) THEN (SELECT (SELECT inn_1006.id FROM tbl_16 AS inn_1006 LIMIT 1) FROM sensor_ranges AS sub_1005 LIMIT 1) ELSE ABS(((((CASE WHEN t_ref_988.id THEN (t_ref_988.id IN (SELECT sub_1007.col_3 FROM tbl_06 AS sub_1007)) ELSE (FALSE >= FALSE) END) - (SELECT sub_1008.col_1 FROM tbl_01 AS sub_1008 LIMIT 1)) < 'escaped''quote')::NUMERIC(10,2))) END)))) < ((LOWER((((0.0000000001 IN (SELECT sub_1009.col_4 FROM tbl_02 AS sub_1009)) AND ABS(((SELECT (SELECT inn_1011.id FROM tbl_06 AS inn_1011 LIMIT 1) FROM tbl_14 AS sub_1010 LIMIT 1) BETWEEN 0 AND -2147483648))) BETWEEN 0 AND -2147483648))::BOOLEAN)::INT)) + ((FALSE <= (t_ref_988.payload OR UPPER((FALSE * EXISTS (SELECT 1 FROM tbl_05 AS sub_1012 WHERE sub_1012.col_4))))) * NULLIF((SELECT (SELECT -1 FROM tbl_05 AS inn_1014 LIMIT 1) FROM tbl_06 AS sub_1013 LIMIT 1), ((SELECT (SELECT inn_1017.col_1 FROM tbl_04 AS inn_1017 LIMIT 1) FROM tbl_11 AS sub_1015 LIMIT 1) * (SELECT (COALESCE(NULLIF(((CASE WHEN sub_1018.col_3 THEN sub_1018.id ELSE 'ref_val' END) = (sub_1018.id / NULLIF(sub_1018.col_2, 0))), sub_1018.col_3), (sub_1018.col_2 * ((NOT (sub_1018.col_3)) / NULLIF(sub_1018.col_2, 0)))) AND (((SELECT (SELECT inn_1020.col_2 FROM tbl_13 AS inn_1020 LIMIT 1) FROM sensor_ranges AS sub_1019 LIMIT 1) + (NOT ((sub_1018.col_1::BOOLEAN)))) * (SELECT sub_1021.col_2 FROM tbl_15 AS sub_1021 LIMIT 1))) FROM tbl_11 AS sub_1018 LIMIT 1))))) IS NOT NULL) ELSE t_ref_988.payload END) END) IS NOT NULL)) ELSE LENGTH(COALESCE((t_ref_988.payload + TRUE), EXISTS (SELECT 1 FROM tbl_04 AS sub_1022 WHERE -2147483648))) END) AS c_1042
FROM sensor_ranges AS t_ref_988 LEFT JOIN tbl_13 AS j_ref_1043 ON t_ref_988.id = j_ref_1043.id
WHERE EXISTS (SELECT 1 FROM tbl_05 AS sub_1044 WHERE 1.0)
LIMIT 10;

INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (301538, 3.14159265, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'chaos');

UPDATE tbl_03 SET col_2 = TRUE WHERE id > 91;

CREATE INDEX idx_tbl_04_id_6 ON tbl_04 (id);

INSERT INTO tbl_14 (id, col_1, col_2) VALUES (18927, 'escaped''quote', '{}');

CREATE TABLE tbl_17 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64)
);

INSERT INTO tbl_11 (id, col_1, col_2, col_3) VALUES (397636, '[]', 42, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (327258, '__fuzz_marker__', 'ref_val', '{}');

DELETE FROM tbl_11 WHERE id = 15288;

SELECT
    (SELECT ((SELECT (SELECT inn_1123.col_2 FROM tbl_07 AS inn_1123 LIMIT 1) FROM tbl_11 AS sub_1122 LIMIT 1) / NULLIF((((UPPER((CASE WHEN sub_1121.col_2 THEN (sub_1121.col_2::INT) ELSE LENGTH(sub_1121.col_2) END)) - (1::INT)) <= (((CASE WHEN (CASE WHEN (CASE WHEN (CASE WHEN (SELECT (SELECT inn_1125.dyn_col_6 FROM tbl_10 AS inn_1125 LIMIT 1) FROM tbl_14 AS sub_1124 LIMIT 1) THEN (NULLIF((CASE WHEN ((sub_1121.id IN (SELECT sub_1126.col_3 FROM tbl_15 AS sub_1126)) * sub_1121.col_1) THEN ABS((ABS(sub_1121.col_1)::INT)) ELSE (CASE WHEN (CASE WHEN (sub_1121.col_1 >= sub_1121.id) THEN (sub_1121.id IN (SELECT sub_1127.col_2 FROM tbl_17 AS sub_1127)) ELSE EXISTS (SELECT 1 FROM tbl_17 AS sub_1128 WHERE sub_1128.id) END) THEN ((sub_1121.col_2 + sub_1121.col_1)::TEXT) ELSE ((SELECT sub_1129.col_1 FROM tbl_08 AS sub_1129 LIMIT 1) AND TRUE) END) END), ((LENGTH((sub_1121.col_1 - sub_1121.col_2)) / NULLIF(LENGTH((sub_1121.id > sub_1121.col_2)), 0)) / NULLIF(LENGTH(((100 OR sub_1121.col_2) OR (sub_1121.col_2 * sub_1121.col_1))), 0))) - EXISTS (SELECT 1 FROM tbl_04 AS sub_1130 WHERE sub_1130.col_3)) ELSE (CASE WHEN (TRUE IN (SELECT sub_1131.col_1 FROM tbl_08 AS sub_1131)) THEN UPPER((CASE WHEN 2147483647 THEN LOWER((-1e-5::BOOLEAN)) ELSE NULLIF(LENGTH(sub_1121.col_2), UPPER((sub_1121.col_1::NUMERIC(10,2)))) END)) ELSE LENGTH((((sub_1121.col_2 IN (SELECT sub_1132.col_2 FROM tbl_11 AS sub_1132))::INT::TEXT) + sub_1121.col_2)) END) END) THEN COALESCE((EXISTS (SELECT 1 FROM tbl_17 AS sub_1133 WHERE sub_1133.col_1) * (((((sub_1121.id + sub_1121.id) BETWEEN 42 AND -2147483648)::NUMERIC(10,2))::INT::TEXT)::NUMERIC(10,2))), sub_1121.col_1) ELSE (EXISTS (SELECT 1 FROM tbl_09 AS sub_1136 WHERE sub_1136.col_2)::TEXT::VARCHAR(32)) END) THEN ((EXISTS (SELECT 1 FROM tbl_07 AS sub_1137 WHERE sub_1137.col_2) > ((NULLIF(sub_1121.id, (ABS((sub_1121.col_1 - sub_1121.id)) BETWEEN 0 AND 2147483647))::BOOLEAN) / NULLIF(NULL, 0))) / NULLIF(sub_1121.col_2, 0)) ELSE sub_1121.col_1 END) THEN (NULL >= ((TRUE * (EXISTS (SELECT 1 FROM sensor_ranges AS sub_1138 WHERE sub_1138.id) / NULLIF((TRUE IN (SELECT sub_1141.col_2 FROM tbl_03 AS sub_1141)), 0))) * sub_1121.col_1)) ELSE ABS((SELECT (sub_1142.col_1 IN (SELECT sub_1143.id FROM tbl_08 AS sub_1143)) FROM tbl_16 AS sub_1142 LIMIT 1)) END) >= (sub_1121.id::TEXT)) > (NULL >= ((CASE WHEN (SELECT (SELECT inn_1150.payload FROM sensor_ranges AS inn_1150 LIMIT 1) FROM tbl_16 AS sub_1146 LIMIT 1) THEN (NOT ((NULL IN (SELECT sub_1151.id FROM tbl_04 AS sub_1151)))) ELSE ((CASE WHEN FALSE THEN (sub_1121.id::INT::TEXT) ELSE (SELECT ((1.0 > '__fuzz_marker__')::INT::TEXT) FROM tbl_10 AS sub_1154 LIMIT 1) END) + sub_1121.col_1) END) < (((SELECT (SELECT 0.0000000001 FROM tbl_14 AS inn_1159 LIMIT 1) FROM tbl_03 AS sub_1155 LIMIT 1) OR NULLIF((sub_1121.id IN (SELECT sub_1160.col_1 FROM tbl_17 AS sub_1160)), (sub_1121.col_1 IN (SELECT sub_1161.col_1 FROM tbl_08 AS sub_1161))))::TEXT::VARCHAR(32)))))) >= (ABS(((FALSE::BOOLEAN) BETWEEN 0 AND 42)) <> UPPER((SELECT ((TRUE + (SELECT (SELECT inn_1167.id FROM tbl_12 AS inn_1167 LIMIT 1) FROM tbl_10 AS sub_1164 LIMIT 1))::TEXT::VARCHAR(32)) FROM tbl_02 AS sub_1163 LIMIT 1)))), 0)) FROM tbl_17 AS sub_1121 LIMIT 1) AS c_1168,
    EXISTS (SELECT 1 FROM tbl_17 AS sub_1169 WHERE FALSE) AS c_1206,
    (t_ref_1120.id::INT) AS c_1207
FROM tbl_07 AS t_ref_1120 LEFT JOIN tbl_10 AS j_ref_1208 ON t_ref_1120.id = j_ref_1208.id
WHERE (((((((t_ref_1120.col_1 IN (SELECT sub_1209.col_3 FROM tbl_04 AS sub_1209)) >= (LENGTH((((SELECT (SELECT inn_1211.col_3 FROM tbl_02 AS inn_1211 LIMIT 1) FROM tbl_07 AS sub_1210 LIMIT 1) * t_ref_1120.dyn_col_4) / NULLIF((t_ref_1120.col_1 IN (SELECT sub_1212.col_1 FROM tbl_04 AS sub_1212)), 0))) * t_ref_1120.id)) AND COALESCE(t_ref_1120.id, ((SELECT (SELECT NULL FROM tbl_09 AS inn_1220 LIMIT 1) FROM tbl_16 AS sub_1213 LIMIT 1) - (FALSE / NULLIF(((SELECT (SELECT inn_1225.col_1 FROM tbl_11 AS inn_1225 LIMIT 1) FROM tbl_12 AS sub_1221 LIMIT 1)::INT), 0))))) - ((((LENGTH(TRUE) <> (((ABS(LOWER(((((t_ref_1120.dyn_col_4 / NULLIF(TRUE, 0)) < COALESCE(NULL, NULL)) + ((CASE WHEN t_ref_1120.col_1 THEN NULL ELSE t_ref_1120.dyn_col_4 END) - ABS(t_ref_1120.id))) * t_ref_1120.col_2)))::BOOLEAN) / NULLIF(t_ref_1120.id, 0)) * ((((CASE WHEN (t_ref_1120.col_1 / NULLIF(((UPPER(t_ref_1120.id) / NULLIF((t_ref_1120.id - t_ref_1120.dyn_col_4), 0))::BOOLEAN), 0)) THEN (CASE WHEN LOWER(((NULL::NUMERIC(10,2))::INT::TEXT)) THEN TRUE ELSE (SELECT FALSE FROM tbl_11 AS sub_1226 LIMIT 1) END) ELSE (CASE WHEN ((SELECT sub_1227.col_1 FROM tbl_08 AS sub_1227 LIMIT 1) <> ABS(UPPER(t_ref_1120.col_2))) THEN (NOT ((SELECT (SELECT inn_1229.id FROM sensor_ranges AS inn_1229 LIMIT 1) FROM tbl_10 AS sub_1228 LIMIT 1))) ELSE (CASE WHEN (LOWER(t_ref_1120.col_2) <> (2147483647 <> t_ref_1120.id)) THEN FALSE ELSE ((t_ref_1120.col_2 <> t_ref_1120.dyn_col_4) <> t_ref_1120.dyn_col_4) END) END) END)::NUMERIC(10,2)) < t_ref_1120.id) + (((42::INT::TEXT)::INT) / NULLIF((NULL IN (SELECT sub_1230.id FROM tbl_15 AS sub_1230)), 0))))) IS NOT NULL) <> EXISTS (SELECT 1 FROM tbl_16 AS sub_1232 WHERE sub_1232.col_3)) <= (SELECT (SELECT inn_1245.col_1 FROM tbl_14 AS inn_1245 LIMIT 1) FROM tbl_13 AS sub_1236 LIMIT 1))) + (SELECT (SELECT inn_1251.col_1 FROM tbl_17 AS inn_1251 LIMIT 1) FROM tbl_10 AS sub_1246 LIMIT 1)) = (SELECT (NULLIF(FALSE, sub_1252.dyn_col_3) <> (UPPER((LENGTH((sub_1252.col_2 IN (SELECT sub_1253.id FROM tbl_05 AS sub_1253))) < (sub_1252.id - ((SELECT UPPER((((CASE WHEN (FALSE AND TRUE) THEN (SELECT TRUE FROM tbl_10 AS sub_1257 LIMIT 1) ELSE sub_1256.col_1 END) BETWEEN 100 AND -9223372036854775808) - ((sub_1256.col_1 IN (SELECT sub_1258.col_1 FROM tbl_17 AS sub_1258)) * sub_1256.col_2))) FROM tbl_04 AS sub_1256 LIMIT 1)::TEXT)))) OR (CASE WHEN (SELECT (SELECT inn_1263.col_2 FROM tbl_08 AS inn_1263 LIMIT 1) FROM tbl_11 AS sub_1259 LIMIT 1) THEN (TRUE AND (((NOT ((EXISTS (SELECT 1 FROM tbl_13 AS sub_1264 WHERE sub_1264.col_2) / NULLIF(NULLIF((((SELECT (SELECT inn_1267.col_1 FROM tbl_01 AS inn_1267 LIMIT 1) FROM tbl_04 AS sub_1266 LIMIT 1)::INT::TEXT) BETWEEN 42 AND 100), FALSE), 0))))::TEXT::VARCHAR(32)) IS NULL)) ELSE 'chaos' END))) FROM tbl_03 AS sub_1252 LIMIT 1)) BETWEEN 9223372036854775807 AND 0)
LIMIT 34;

DELETE FROM tbl_03 WHERE id = 62089;

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3) VALUES (508991, 999999999.99, '[]', 'ref_val');
COMMIT;

DELETE FROM sensor_ranges WHERE id = 97937;

INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (454389, -1, 'escaped''quote', 1e10);

UPDATE tbl_17 SET col_2 = 'ref_val' WHERE id > 10;

CREATE INDEX idx_tbl_06_ref_tbl_03_id_7 ON tbl_06 (ref_tbl_03_id);

INSERT INTO tbl_08 (id, col_1, col_2) VALUES (495001, -1, 42);

CREATE INDEX idx_tbl_17_col_2_8 ON tbl_17 (col_2);

DELETE FROM tbl_16 WHERE id = 88325;

CREATE VIEW v_summary_6 AS SELECT id FROM tbl_07 WHERE id IS NOT NULL;

DELETE FROM tbl_01 WHERE id = 32413;

INSERT INTO tbl_06 (id, col_1, col_3, ref_tbl_03_id) VALUES (326875, '', '__fuzz_marker__', 2147483647);

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3) VALUES (92001, -1, 'escaped''quote', -1.5);
COMMIT;

CREATE VIEW v_summary_7 AS SELECT id FROM tbl_17 WHERE id IS NOT NULL;

UPDATE tbl_09 SET col_1 = 'ref_val' WHERE id > 94;

INSERT INTO tbl_10 (id, col_1, col_2, col_3, ref_tbl_05_id, dyn_col_6) VALUES (300819, 'chaos', -9223372036854775808, -1.5, 2147483647, 'alpha_beta_gamma');

CREATE TABLE tbl_18 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 JSONB
);

SELECT
    LOWER((((SELECT (SELECT inn_1275.col_1 FROM tbl_12 AS inn_1275 LIMIT 1) FROM tbl_17 AS sub_1269 LIMIT 1) + t_ref_1268.id) / NULLIF((SELECT ((sub_1276.id::NUMERIC(10,2)) / NULLIF(sub_1276.payload, 0)) FROM sensor_ranges AS sub_1276 LIMIT 1), 0))) AS c_1277,
    (SELECT (SELECT inn_1302.col_4 FROM tbl_05 AS inn_1302 LIMIT 1) FROM tbl_03 AS sub_1278 LIMIT 1) AS c_1303,
    FALSE AS c_1304
FROM tbl_01 AS t_ref_1268 LEFT JOIN tbl_14 AS j_ref_1305 ON t_ref_1268.id = j_ref_1305.id
WHERE LENGTH(LENGTH((t_ref_1268.col_2::INT::TEXT)))
LIMIT 16;

CREATE TABLE tbl_19 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 BOOLEAN,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

BEGIN;
INSERT INTO tbl_15 (id, col_1, col_2, col_3) VALUES (220424, -2147483648, -2147483648, 'escaped''quote');
COMMIT;

CREATE VIEW v_summary_8 AS SELECT id FROM sensor_ranges WHERE id IS NOT NULL;

SELECT
    (UPPER(1)::INT) AS c_1307,
    'escaped''quote' AS c_1308,
    (((CASE WHEN EXISTS (SELECT 1 FROM tbl_04 AS sub_1309 WHERE sub_1309.col_2) THEN (((CASE WHEN (-1.5::NUMERIC(10,2)) THEN (t_ref_1306.id::BOOLEAN) ELSE (SELECT 'alpha_beta_gamma' FROM tbl_18 AS sub_1312 LIMIT 1) END) - -9223372036854775808) = (CASE WHEN (CASE WHEN (CASE WHEN (SELECT ABS((SELECT (SELECT NULL FROM tbl_18 AS inn_1317 LIMIT 1) FROM tbl_13 AS sub_1314 LIMIT 1)) FROM tbl_18 AS sub_1313 LIMIT 1) THEN t_ref_1306.col_1 ELSE EXISTS (SELECT 1 FROM tbl_15 AS sub_1318 WHERE sub_1318.col_2) END) THEN (NOT (t_ref_1306.col_1)) ELSE ((t_ref_1306.col_1 + (CASE WHEN (t_ref_1306.id BETWEEN 0 AND 0) THEN ABS((t_ref_1306.col_1 IN (SELECT sub_1323.id FROM tbl_13 AS sub_1323))) ELSE (NOT (ABS(t_ref_1306.col_1))) END)) * ((((t_ref_1306.id::TEXT::VARCHAR(32))::TEXT::VARCHAR(32)) BETWEEN 0 AND -1)::TEXT::VARCHAR(32))) END) THEN EXISTS (SELECT 1 FROM tbl_19 AS sub_1324 WHERE FALSE) ELSE (t_ref_1306.id::NUMERIC(10,2)) END)) ELSE (t_ref_1306.col_1 IN (SELECT sub_1339.id FROM tbl_03 AS sub_1339)) END) / NULLIF(t_ref_1306.col_2, 0)) OR LOWER(t_ref_1306.col_2)) AS c_1340
FROM tbl_08 AS t_ref_1306
WHERE (COALESCE(((t_ref_1306.id IN (SELECT sub_1341.col_2 FROM tbl_14 AS sub_1341)) - (EXISTS (SELECT 1 FROM tbl_05 AS sub_1342 WHERE sub_1342.id)::INT::TEXT)), t_ref_1306.col_1) <> FALSE)
LIMIT 48;

INSERT INTO tbl_07 (id, col_1, col_2, dyn_col_4) VALUES (184822, 9223372036854775807, 2147483647, 'chaos');

CREATE TABLE tbl_20 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2),
    col_4 VARCHAR(64),
    ref_tbl_19_id INT,
    FOREIGN KEY (ref_tbl_19_id) REFERENCES tbl_19(id)
);

CREATE VIEW v_summary_9 AS SELECT id FROM tbl_19 WHERE id IS NOT NULL;

INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (568980, TRUE, TRUE, 'ref_val', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 2147483647);

CREATE INDEX idx_tbl_11_col_1_9 ON tbl_11 (col_1);

BEGIN;
INSERT INTO tbl_16 (id, col_1, col_2, col_3, col_4, ref_tbl_15_id) VALUES (683101, 42, 100, '__fuzz_marker__', '', 9223372036854775807);
INSERT INTO tbl_15 (id, col_1, col_2, col_3) VALUES (215238, 100, 1, 'alpha_beta_gamma');
INSERT INTO tbl_02 (id, col_1, col_3, col_4) VALUES (426838, FALSE, 1e10, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
COMMIT;

CREATE TABLE tbl_21 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    ref_tbl_14_id INT,
    FOREIGN KEY (ref_tbl_14_id) REFERENCES tbl_14(id)
);

INSERT INTO tbl_20 (id, col_1, col_2, col_3, col_4, ref_tbl_19_id) VALUES (46383, 1, 'escaped''quote', 0.0000000001, '', 0);
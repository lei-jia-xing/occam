-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: POSTGRES

-- Statements: 100

-- AST Expression Depth: 14

-- Injected Defect Case: pg_02

-- Seed: 10011



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT,
    col_3 INT,
    col_4 NUMERIC(10,2)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT,
    col_3 VARCHAR(64),
    col_4 INT
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT,
    col_3 JSONB,
    col_4 INT
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 JSONB,
    col_4 VARCHAR(64),
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE log_batches (
    batch_id INT PRIMARY KEY,
    source_service VARCHAR(64) NOT NULL,
    tag_ids INT[] NOT NULL
);

ALTER TABLE tbl_01 ADD COLUMN dyn_col_6 VARCHAR(32);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (832756, TRUE, 1, 'escaped''quote', -2147483648);

INSERT INTO log_batches (id, payload) VALUES (556829, 'escaped''quote');

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (695728, 3.14159265, 'chaos', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '', -2147483648);
COMMIT;

BEGIN;
INSERT INTO log_batches (id, payload) VALUES (264413, 'chaos');
COMMIT;

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_01 WHERE id IS NOT NULL;

SELECT
    (t_ref_1.col_1 < LENGTH(t_ref_1.col_2)) AS c_2,
    t_ref_1.col_2 AS c_3,
    (t_ref_1.col_2::INT) AS c_4
FROM tbl_05 AS t_ref_1
WHERE (((((EXISTS (SELECT 1 FROM log_batches AS sub_5 WHERE sub_5.id)::BOOLEAN) BETWEEN 100 AND 100) BETWEEN -1 AND 42)::NUMERIC(10,2)) = (ABS(((SELECT NULLIF((sub_10.col_2 IN (SELECT sub_11.col_3 FROM tbl_02 AS sub_11)), (SELECT (SELECT inn_16.ref_tbl_02_id FROM tbl_04 AS inn_16 LIMIT 1) FROM tbl_03 AS sub_13 LIMIT 1)) FROM tbl_04 AS sub_10 LIMIT 1) / NULLIF(LENGTH(NULL), 0)))::TEXT::VARCHAR(32)))
LIMIT 14;

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 INT,
    col_4 JSONB
);

CREATE INDEX idx_log_batches_tags_gin ON log_batches USING gin (tag_ids) WITH (fastupdate = on);

ALTER TABLE tbl_01 DROP COLUMN col_3;

SELECT
    (NOT ((SELECT sub_18.ref_tbl_01_id FROM tbl_05 AS sub_18 LIMIT 1))) AS c_19
FROM tbl_03 AS t_ref_17
WHERE (SELECT (NULLIF(((((sub_20.col_3 >= NULLIF((SELECT (sub_21.dyn_col_6 - NULL) FROM tbl_01 AS sub_21 LIMIT 1), (((EXISTS (SELECT 1 FROM tbl_01 AS sub_22 WHERE sub_22.id) IS NULL) / NULLIF((SELECT (SELECT inn_24.ref_tbl_02_id FROM tbl_04 AS inn_24 LIMIT 1) FROM tbl_06 AS sub_23 LIMIT 1), 0)) + (NULLIF((CASE WHEN sub_20.col_3 THEN TRUE ELSE sub_20.col_1 END), (SELECT (SELECT inn_26.id FROM tbl_05 AS inn_26 LIMIT 1) FROM tbl_05 AS sub_25 LIMIT 1)) / NULLIF((''::INT::TEXT), 0))))) OR (((SELECT (SELECT inn_28.col_3 FROM tbl_02 AS inn_28 LIMIT 1) FROM tbl_04 AS sub_27 LIMIT 1) <> (sub_20.id IS NULL)) * (FALSE IN (SELECT sub_29.dyn_col_6 FROM tbl_01 AS sub_29)))) - ((sub_20.col_1 IN (SELECT sub_30.col_4 FROM tbl_01 AS sub_30))::TEXT::VARCHAR(32)))::NUMERIC(10,2)), ((sub_20.col_1 <> (LENGTH((((NULLIF(LENGTH(TRUE), (CASE WHEN sub_20.id THEN sub_20.id ELSE sub_20.col_1 END)) > ((sub_20.col_2 / NULLIF(sub_20.col_2, 0)) < sub_20.col_2)) = NULL)::NUMERIC(10,2)))::TEXT::VARCHAR(32)))::NUMERIC(10,2)))::INT::TEXT) FROM tbl_04 AS sub_20 LIMIT 1)
LIMIT 12;

SELECT
    ((SELECT (SELECT 1 FROM tbl_06 AS inn_41 LIMIT 1) FROM tbl_01 AS sub_35 LIMIT 1) > (t_ref_34.col_3 + FALSE)) AS c_42
FROM tbl_06 AS t_ref_34
WHERE (((UPPER((((CASE WHEN (EXISTS (SELECT 1 FROM tbl_03 AS sub_43 WHERE sub_43.col_4) * (SELECT sub_46.col_1 FROM tbl_05 AS sub_46 LIMIT 1)) THEN (COALESCE(TRUE, t_ref_34.col_3) <= ((t_ref_34.col_2 / NULLIF((t_ref_34.col_3 < (NOT (t_ref_34.col_2))), 0)) <> (SELECT EXISTS (SELECT 1 FROM tbl_06 AS sub_48 WHERE sub_48.col_2) FROM tbl_04 AS sub_47 LIMIT 1))) ELSE (SELECT ABS((3.14159265 > sub_49.id)) FROM log_batches AS sub_49 LIMIT 1) END)::INT)::INT::TEXT)) / NULLIF(UPPER(t_ref_34.col_3), 0)) IS NOT NULL) / NULLIF((SELECT (NOT (((((SELECT TRUE FROM log_batches AS sub_51 LIMIT 1) = (SELECT (SELECT 100 FROM tbl_02 AS inn_56 LIMIT 1) FROM log_batches AS sub_52 LIMIT 1)) + ((NOT (((sub_50.col_3 * ((NULLIF(sub_50.col_1, 1) OR sub_50.col_3) + TRUE))::INT))) * (SELECT (sub_57.id IS NULL) FROM tbl_05 AS sub_57 LIMIT 1))) * (SELECT (SELECT NULL FROM tbl_01 AS inn_60 LIMIT 1) FROM tbl_05 AS sub_58 LIMIT 1)))) FROM tbl_06 AS sub_50 LIMIT 1), 0))
LIMIT 38;

SELECT
    t_ref_61.col_4 AS c_62,
    (SELECT sub_63.payload FROM log_batches AS sub_63 LIMIT 1) AS c_64,
    NULLIF((SELECT -1e-5 FROM tbl_05 AS sub_65 LIMIT 1), t_ref_61.col_1) AS c_66
FROM tbl_02 AS t_ref_61
WHERE -2147483648
LIMIT 43;

UPDATE tbl_03 SET col_2 = 0 WHERE id > 77;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (796914, 3.14159265, 'ref_val', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'chaos', 0);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (56405, TRUE, -2147483648, '[]', 42);
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (258127, FALSE, 42, 'ref_val', -2147483648);
COMMIT;

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (81583, TRUE, -9223372036854775808, '[]', 100);
INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_6) VALUES (558011, -9223372036854775808, 9223372036854775807, 1.0, '');
COMMIT;

CREATE INDEX idx_tbl_04_col_2_1 ON tbl_04 (col_2);

INSERT INTO log_batches (batch_id, source_service, tag_ids) VALUES
    (1, 'ingress-proxy', ARRAY[101, 102, 103, 104]),
    (2, 'ingress-proxy', ARRAY[102, 105, 106]),
    (3, 'auth-gateway', ARRAY[201, 202, 101]),
    (4, 'database-pool', ARRAY[301, 302, 303, 304, 305]);

CREATE INDEX idx_tbl_05_ref_tbl_01_id_2 ON tbl_05 (ref_tbl_01_id);

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 JSONB
);

SELECT
    (((SELECT (SELECT inn_78.id FROM tbl_03 AS inn_78 LIMIT 1) FROM tbl_01 AS sub_68 LIMIT 1) + (EXISTS (SELECT 1 FROM tbl_01 AS sub_79 WHERE sub_79.col_2)::TEXT::VARCHAR(32))) + (UPPER((CASE WHEN t_ref_67.col_1 THEN (CASE WHEN (((FALSE = EXISTS (SELECT 1 FROM tbl_04 AS sub_86 WHERE sub_86.ref_tbl_02_id)) / NULLIF((((t_ref_67.col_1 IN (SELECT sub_90.id FROM tbl_02 AS sub_90)) > LENGTH((t_ref_67.col_3 * (('alpha_beta_gamma'::NUMERIC(10,2))::INT))))::BOOLEAN), 0))::TEXT::VARCHAR(32)) THEN (CASE WHEN (SELECT (SELECT inn_94.id FROM log_batches AS inn_94 LIMIT 1) FROM tbl_05 AS sub_92 LIMIT 1) THEN (SELECT (SELECT inn_97.col_1 FROM tbl_05 AS inn_97 LIMIT 1) FROM log_batches AS sub_95 LIMIT 1) ELSE (t_ref_67.col_1 + ('alpha_beta_gamma' * (t_ref_67.col_1 IN (SELECT sub_98.col_2 FROM tbl_01 AS sub_98)))) END) ELSE FALSE END) ELSE ((t_ref_67.col_2 IN (SELECT sub_99.col_1 FROM tbl_05 AS sub_99)) * (SELECT ((2147483647 * (EXISTS (SELECT 1 FROM tbl_02 AS sub_105 WHERE sub_105.col_4) + NULLIF((TRUE::NUMERIC(10,2)), (sub_104.col_4 IN (SELECT sub_106.dyn_col_6 FROM tbl_01 AS sub_106))))) + (((EXISTS (SELECT 1 FROM tbl_04 AS sub_107 WHERE TRUE) <= NULL) / NULLIF(sub_104.id, 0))::INT)) FROM tbl_03 AS sub_104 LIMIT 1)) END))::NUMERIC(10,2))) AS c_108
FROM tbl_07 AS t_ref_67
WHERE LOWER((SELECT (SELECT (SELECT '__fuzz_marker__' FROM tbl_07 AS inn_111 LIMIT 1) FROM tbl_04 AS sub_110 LIMIT 1) FROM tbl_06 AS sub_109 LIMIT 1))
LIMIT 39;

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (234110, FALSE, 100, '{}', 2147483647);

SELECT
    ((CASE WHEN (SELECT (SELECT inn_114.col_3 FROM tbl_02 AS inn_114 LIMIT 1) FROM log_batches AS sub_113 LIMIT 1) THEN t_ref_112.col_4 ELSE ((t_ref_112.col_3 IS NULL)::TEXT) END) - t_ref_112.col_3) AS c_115,
    (((SELECT (SELECT inn_119.col_1 FROM tbl_07 AS inn_119 LIMIT 1) FROM tbl_03 AS sub_116 LIMIT 1) / NULLIF((TRUE::BOOLEAN), 0)) > ((ABS(t_ref_112.id)::INT::TEXT) OR ((NULLIF(TRUE, t_ref_112.col_2)::INT)::INT))) AS c_120
FROM tbl_02 AS t_ref_112
WHERE (((FALSE IN (SELECT sub_121.col_3 FROM tbl_02 AS sub_121))::TEXT::VARCHAR(32)) / NULLIF(((NULL <= EXISTS (SELECT 1 FROM tbl_07 AS sub_122 WHERE TRUE)) + (t_ref_112.id > 42)), 0))
LIMIT 6;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (486857, TRUE, -1, 'escaped''quote', 1);

CREATE INDEX idx_log_batches_payload_3 ON log_batches (payload);

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_05 WHERE id IS NOT NULL;

INSERT INTO log_batches (batch_id, source_service, tag_ids)
SELECT g, 'load-generator', ARRAY[g % 50, (g * 3) % 50, (g * 7) % 50]
FROM generate_series(5, 50) g;

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_6) VALUES (814814, 9223372036854775807, 100, -1e-5, 'escaped''quote');
COMMIT;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2)
);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (335462, 0.0, '', '{}', 'alpha_beta_gamma', -1);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (563696, TRUE, 42, '[]', 2147483647);

DELETE FROM tbl_06 WHERE id = 34606;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN,
    ref_tbl_08_id INT,
    FOREIGN KEY (ref_tbl_08_id) REFERENCES tbl_08(id)
);

CREATE INDEX idx_tbl_08_id_4 ON tbl_08 (id);

ALTER TABLE tbl_06 DROP COLUMN col_3;

SELECT gin_clean_pending_list('idx_log_batches_tags_gin');

UPDATE tbl_08 SET col_3 = 3.14159265 WHERE id > 32;

INSERT INTO tbl_05 (id, col_1, col_2, ref_tbl_01_id) VALUES (385273, 1, -2147483648, 2147483647);

UPDATE tbl_02 SET col_1 = TRUE WHERE id > 3;

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, ref_tbl_01_id) VALUES (722559, 2147483647, 1, 100);
INSERT INTO tbl_05 (id, col_1, col_2, ref_tbl_01_id) VALUES (948101, 2147483647, 100, 42);
COMMIT;

ALTER TABLE tbl_08 DROP COLUMN col_1;

DELETE FROM tbl_06 WHERE id = 9753;

INSERT INTO log_batches (id, payload) VALUES (489098, 'alpha_beta_gamma');

INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_6) VALUES (702306, -2147483648, 0, 3.14159265, 'chaos');

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_08 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_6) VALUES (867500, 9223372036854775807, -2147483648, 3.14159265, 'chaos');
COMMIT;

INSERT INTO tbl_05 (id, col_1, col_2, ref_tbl_01_id) VALUES (660651, 1, -2147483648, 2147483647);

INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (591508, 1e10, TRUE, 1);

INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (206567, 0.0000000001, FALSE, 2147483647);

UPDATE tbl_07 SET col_3 = '{}' WHERE id > 73;

SELECT
    (t_ref_123.col_4 / NULLIF(((CASE WHEN (FALSE IN (SELECT sub_124.id FROM tbl_09 AS sub_124)) THEN (CASE WHEN (((((((NULL + (SELECT (SELECT inn_128.id FROM tbl_03 AS inn_128 LIMIT 1) FROM tbl_07 AS sub_127 LIMIT 1)) = t_ref_123.col_2) + (SELECT (SELECT 'ref_val' FROM log_batches AS inn_130 LIMIT 1) FROM tbl_08 AS sub_129 LIMIT 1)) * TRUE) / NULLIF((CASE WHEN (SELECT ((sub_131.col_3 IN (SELECT sub_132.id FROM log_batches AS sub_132)) <> ((SELECT (SELECT inn_134.col_3 FROM tbl_03 AS inn_134 LIMIT 1) FROM tbl_03 AS sub_133 LIMIT 1) + EXISTS (SELECT 1 FROM tbl_03 AS sub_135 WHERE TRUE))) FROM tbl_07 AS sub_131 LIMIT 1) THEN (LENGTH(t_ref_123.id) AND (t_ref_123.col_4 IN (SELECT sub_136.col_2 FROM tbl_01 AS sub_136))) ELSE t_ref_123.col_2 END), 0))::INT::TEXT) / NULLIF(EXISTS (SELECT 1 FROM tbl_07 AS sub_137 WHERE sub_137.col_3), 0)) THEN (((SELECT (SELECT inn_146.col_2 FROM tbl_08 AS inn_146 LIMIT 1) FROM tbl_01 AS sub_143 LIMIT 1) - 9223372036854775807) + (SELECT (SELECT inn_149.col_4 FROM tbl_04 AS inn_149 LIMIT 1) FROM tbl_05 AS sub_147 LIMIT 1)) ELSE (NOT ((-1.5 + t_ref_123.col_1))) END) ELSE COALESCE((((t_ref_123.col_3 + (t_ref_123.col_3 IN (SELECT sub_150.col_2 FROM tbl_07 AS sub_150)))::NUMERIC(10,2)) - ((9223372036854775807 / NULLIF((0.0 IN (SELECT sub_154.id FROM tbl_03 AS sub_154)), 0)) / NULLIF(LENGTH(t_ref_123.id), 0))), (((('chaos' / NULLIF((NULLIF(((t_ref_123.col_4 * (TRUE / NULLIF(t_ref_123.col_4, 0)))::INT), EXISTS (SELECT 1 FROM tbl_08 AS sub_155 WHERE sub_155.id)) <> ((CASE WHEN ((t_ref_123.col_3 IN (SELECT sub_156.id FROM tbl_08 AS sub_156))::INT) THEN (CASE WHEN t_ref_123.col_1 THEN (FALSE <> FALSE) ELSE (CASE WHEN t_ref_123.id THEN NULL ELSE TRUE END) END) ELSE ((t_ref_123.col_2 IN (SELECT sub_157.col_1 FROM tbl_01 AS sub_157))::INT) END) - (((SELECT sub_158.col_2 FROM tbl_05 AS sub_158 LIMIT 1) BETWEEN 2147483647 AND -9223372036854775808) - t_ref_123.col_1))), 0)) = LOWER(LOWER((SELECT (SELECT 'chaos' FROM tbl_05 AS inn_160 LIMIT 1) FROM tbl_08 AS sub_159 LIMIT 1))))::INT) IS NOT NULL)) END) OR (EXISTS (SELECT 1 FROM tbl_06 AS sub_161 WHERE sub_161.col_4) > t_ref_123.id)), 0)) AS c_162,
    t_ref_123.col_4 AS c_163,
    (t_ref_123.col_4 / NULLIF(t_ref_123.col_2, 0)) AS c_164
FROM tbl_02 AS t_ref_123 LEFT JOIN tbl_07 AS j_ref_165 ON t_ref_123.id = j_ref_165.id
WHERE FALSE
LIMIT 14;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (54272, 999999999.99, 'chaos', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '', 9223372036854775807);
INSERT INTO tbl_08 (id, col_2, col_3) VALUES (902946, 'ref_val', 3.14159265);
COMMIT;

SELECT
    (CASE WHEN (NULL <= (EXISTS (SELECT 1 FROM tbl_06 AS sub_167 WHERE sub_167.col_4) * LENGTH(ABS(t_ref_166.col_4)))) THEN (t_ref_166.col_2 + (('ref_val' + (NOT ((((SELECT (SELECT inn_176.col_1 FROM tbl_06 AS inn_176 LIMIT 1) FROM tbl_06 AS sub_172 LIMIT 1)::NUMERIC(10,2)) + ((((t_ref_166.col_2 * LOWER(t_ref_166.col_1)) * (COALESCE(((t_ref_166.col_2 + FALSE) * (t_ref_166.ref_tbl_02_id::TEXT)), ABS((TRUE + FALSE))) / NULLIF(ABS(((SELECT (SELECT inn_178.ref_tbl_01_id FROM tbl_05 AS inn_178 LIMIT 1) FROM tbl_07 AS sub_177 LIMIT 1) * EXISTS (SELECT 1 FROM log_batches AS sub_179 WHERE sub_179.id))), 0))) BETWEEN -1 AND 0) > TRUE))))) / NULLIF((t_ref_166.ref_tbl_02_id * t_ref_166.col_2), 0))) ELSE COALESCE(LOWER(((SELECT (SELECT 'ref_val' FROM tbl_08 AS inn_190 LIMIT 1) FROM tbl_02 AS sub_180 LIMIT 1) + ((CASE WHEN (LOWER((t_ref_166.col_3::TEXT::VARCHAR(32)))::BOOLEAN) THEN (UPPER(t_ref_166.col_2)::NUMERIC(10,2)) ELSE COALESCE(((t_ref_166.ref_tbl_02_id::TEXT::VARCHAR(32)) / NULLIF(t_ref_166.col_2, 0)), ((t_ref_166.ref_tbl_02_id + ((CASE WHEN ((SELECT (SELECT inn_192.col_3 FROM tbl_08 AS inn_192 LIMIT 1) FROM tbl_07 AS sub_191 LIMIT 1) - (SELECT (SELECT inn_194.col_2 FROM tbl_05 AS inn_194 LIMIT 1) FROM tbl_05 AS sub_193 LIMIT 1)) THEN ((CASE WHEN t_ref_166.col_3 THEN t_ref_166.col_3 ELSE t_ref_166.col_4 END)::TEXT::VARCHAR(32)) ELSE (UPPER(t_ref_166.col_1) = (t_ref_166.col_4::INT)) END)::BOOLEAN)) <= FALSE)) END) * t_ref_166.col_3))), t_ref_166.col_2) END) AS c_195,
    (((((999999999.99 <> 'alpha_beta_gamma')::NUMERIC(10,2)) * t_ref_166.col_2) - NULLIF((t_ref_166.col_3 - EXISTS (SELECT 1 FROM tbl_04 AS sub_196 WHERE 3.14159265)), (((LENGTH((((t_ref_166.col_4 < (((SELECT sub_201.col_2 FROM tbl_03 AS sub_201 LIMIT 1) - (t_ref_166.ref_tbl_02_id * 'escaped''quote')) - (t_ref_166.col_1 IN (SELECT sub_202.id FROM tbl_03 AS sub_202)))) IS NULL) - EXISTS (SELECT 1 FROM tbl_03 AS sub_203 WHERE sub_203.col_4))) OR (SELECT (SELECT NULL FROM tbl_06 AS inn_208 LIMIT 1) FROM tbl_06 AS sub_205 LIMIT 1))::INT::TEXT) > (SELECT (SELECT inn_215.col_2 FROM tbl_02 AS inn_215 LIMIT 1) FROM log_batches AS sub_209 LIMIT 1)))) - t_ref_166.id) AS c_216,
    ((ABS(-2147483648) - (EXISTS (SELECT 1 FROM log_batches AS sub_217 WHERE 100) - (((LENGTH(LOWER(((t_ref_166.col_4 > ((SELECT sub_218.col_4 FROM tbl_02 AS sub_218 LIMIT 1)::INT)) AND (((NOT (NULL)) / NULLIF((t_ref_166.col_3 * (t_ref_166.ref_tbl_02_id OR t_ref_166.col_3)), 0))::INT)))) > (t_ref_166.col_4 IN (SELECT sub_219.col_4 FROM tbl_04 AS sub_219)))::TEXT) * (t_ref_166.col_4::NUMERIC(10,2))))) + NULL) AS c_222
FROM tbl_04 AS t_ref_166 LEFT JOIN tbl_08 AS j_ref_223 ON t_ref_166.id = j_ref_223.id
WHERE (SELECT (SELECT inn_239.id FROM tbl_08 AS inn_239 LIMIT 1) FROM tbl_05 AS sub_224 LIMIT 1)
LIMIT 18;

UPDATE tbl_08 SET col_2 = 'alpha_beta_gamma' WHERE id > 78;

INSERT INTO tbl_06 (id, col_1, col_2, col_4) VALUES (246347, 9223372036854775807, 1e10, '{}');

SELECT
    (SELECT ('__fuzz_marker__' - NULLIF(((((sub_241.id > (((LENGTH((-1.5 > -1e-5)) / NULLIF(((sub_241.col_1 IN (SELECT sub_242.col_2 FROM tbl_06 AS sub_242)) <> FALSE), 0)) OR 100) + (SELECT (SELECT inn_244.payload FROM log_batches AS inn_244 LIMIT 1) FROM tbl_01 AS sub_243 LIMIT 1)))::INT) AND sub_241.id)::BOOLEAN), (NULL >= LENGTH((sub_241.ref_tbl_08_id < ((FALSE IN (SELECT sub_245.col_3 FROM tbl_07 AS sub_245)) >= (sub_241.id > -1.5))))))) FROM tbl_09 AS sub_241 LIMIT 1) AS c_249,
    NULLIF(TRUE, UPPER(((LENGTH(LOWER((LOWER(t_ref_240.col_2)::NUMERIC(10,2)))) + t_ref_240.ref_tbl_01_id)::BOOLEAN))) AS c_250,
    TRUE AS c_251
FROM tbl_05 AS t_ref_240
WHERE (LENGTH(UPPER(COALESCE(t_ref_240.ref_tbl_01_id, (t_ref_240.col_1 = ((t_ref_240.ref_tbl_01_id::BOOLEAN) AND (((((NOT (NULLIF((t_ref_240.ref_tbl_01_id::TEXT), (SELECT NULL FROM tbl_08 AS sub_252 LIMIT 1)))) * ((CASE WHEN COALESCE(t_ref_240.col_2, 'ref_val') THEN t_ref_240.id ELSE (t_ref_240.col_2 * t_ref_240.id) END) * (SELECT (SELECT inn_254.id FROM tbl_02 AS inn_254 LIMIT 1) FROM tbl_06 AS sub_253 LIMIT 1))) = TRUE)::INT::TEXT) - ('ref_val' IN (SELECT sub_255.col_2 FROM tbl_08 AS sub_255)))))))) < t_ref_240.id)
LIMIT 33;

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 JSONB,
    ref_tbl_05_id INT,
    FOREIGN KEY (ref_tbl_05_id) REFERENCES tbl_05(id)
);

CREATE INDEX idx_tbl_08_col_2_5 ON tbl_08 (col_2);

SELECT
    LENGTH((COALESCE(t_ref_256.col_2, (NOT (((SELECT (SELECT inn_262.col_1 FROM tbl_07 AS inn_262 LIMIT 1) FROM tbl_06 AS sub_257 LIMIT 1) > -1))))::TEXT)) AS c_263,
    (LOWER((((SELECT ((sub_264.col_2 * LENGTH(EXISTS (SELECT 1 FROM tbl_06 AS sub_265 WHERE sub_265.col_1))) < (sub_264.col_1::TEXT::VARCHAR(32))) FROM tbl_05 AS sub_264 LIMIT 1)::TEXT) / NULLIF((FALSE - t_ref_256.id), 0))) <= (((((COALESCE(((CASE WHEN t_ref_256.id THEN t_ref_256.col_2 ELSE ((((SELECT sub_266.col_3 FROM tbl_08 AS sub_266 LIMIT 1) + (t_ref_256.col_1 BETWEEN 2147483647 AND -2147483648)) + (('__fuzz_marker__' + t_ref_256.col_1) * 'alpha_beta_gamma')) >= (NULLIF(t_ref_256.col_3, t_ref_256.col_1) + t_ref_256.col_3)) END) <= t_ref_256.col_2), EXISTS (SELECT 1 FROM tbl_01 AS sub_267 WHERE sub_267.id)) > UPPER(t_ref_256.id)) OR t_ref_256.col_2)::INT)::BOOLEAN) BETWEEN -9223372036854775808 AND -9223372036854775808)) AS c_269
FROM tbl_07 AS t_ref_256
WHERE (((SELECT (SELECT inn_281.col_2 FROM tbl_03 AS inn_281 LIMIT 1) FROM tbl_03 AS sub_270 LIMIT 1)::TEXT) > (((CASE WHEN ((TRUE AND UPPER((TRUE::INT::TEXT))) + TRUE) THEN t_ref_256.col_3 ELSE UPPER((NOT (((SELECT (SELECT inn_284.ref_tbl_01_id FROM tbl_05 AS inn_284 LIMIT 1) FROM tbl_02 AS sub_282 LIMIT 1) * EXISTS (SELECT 1 FROM tbl_01 AS sub_285 WHERE sub_285.id))))) END) AND (42 * ((((SELECT (SELECT 0.0000000001 FROM tbl_08 AS inn_288 LIMIT 1) FROM tbl_02 AS sub_286 LIMIT 1) / NULLIF(t_ref_256.col_1, 0))::BOOLEAN) AND (t_ref_256.col_3 IN (SELECT sub_289.col_2 FROM tbl_03 AS sub_289))))) BETWEEN -1 AND 42))
LIMIT 48;

SELECT
    ((t_ref_290.id BETWEEN 100 AND 100) + LOWER((t_ref_290.id BETWEEN 1 AND -1))) AS c_291
FROM log_batches AS t_ref_290
WHERE (COALESCE(((t_ref_290.id::TEXT) <> COALESCE((SELECT (SELECT inn_293.id FROM tbl_05 AS inn_293 LIMIT 1) FROM tbl_07 AS sub_292 LIMIT 1), (ABS(COALESCE(t_ref_290.id, (LENGTH((t_ref_290.payload OR (t_ref_290.payload / NULLIF(t_ref_290.payload, 0)))) * (((FALSE + (SELECT 42 FROM tbl_05 AS sub_294 LIMIT 1)) - t_ref_290.id) * t_ref_290.id)))) = (SELECT (SELECT inn_296.col_3 FROM tbl_03 AS inn_296 LIMIT 1) FROM tbl_03 AS sub_295 LIMIT 1)))), ((UPPER((CASE WHEN ABS((t_ref_290.payload - (t_ref_290.payload::INT))) THEN ((((FALSE - (-1 IN (SELECT sub_297.col_2 FROM tbl_05 AS sub_297))) / NULLIF((COALESCE(t_ref_290.payload, t_ref_290.payload) / NULLIF((NOT (((FALSE::INT::TEXT) IS NOT NULL))), 0)), 0)) BETWEEN 1 AND -2147483648) / NULLIF((t_ref_290.id::INT), 0)) ELSE (t_ref_290.payload * (('alpha_beta_gamma' IN (SELECT sub_298.col_4 FROM tbl_06 AS sub_298))::NUMERIC(10,2))) END)) AND (LENGTH((t_ref_290.id::TEXT)) / NULLIF('escaped''quote', 0))) < ((SELECT (SELECT TRUE FROM tbl_10 AS inn_301 LIMIT 1) FROM tbl_05 AS sub_299 LIMIT 1) > (CASE WHEN ((SELECT (SELECT inn_303.col_1 FROM tbl_05 AS inn_303 LIMIT 1) FROM tbl_06 AS sub_302 LIMIT 1) BETWEEN 1 AND -9223372036854775808) THEN t_ref_290.payload ELSE ((CASE WHEN (t_ref_290.id > (t_ref_290.payload IN (SELECT sub_304.col_1 FROM tbl_05 AS sub_304))) THEN (FALSE::TEXT) ELSE (((COALESCE(COALESCE((t_ref_290.payload >= NULL), COALESCE('chaos', t_ref_290.id)), (SELECT sub_306.col_1 FROM tbl_05 AS sub_306 LIMIT 1)) OR COALESCE(t_ref_290.payload, ((SELECT (SELECT inn_308.col_3 FROM tbl_07 AS inn_308 LIMIT 1) FROM tbl_04 AS sub_307 LIMIT 1) OR (CASE WHEN t_ref_290.id THEN t_ref_290.payload ELSE TRUE END)))) * (SELECT (sub_309.col_2 IN (SELECT sub_310.col_4 FROM tbl_06 AS sub_310)) FROM tbl_09 AS sub_309 LIMIT 1)) <= LOWER(9223372036854775807)) END) * EXISTS (SELECT 1 FROM tbl_07 AS sub_311 WHERE 'ref_val')) END)))) - (((SELECT (SELECT inn_314.payload FROM log_batches AS inn_314 LIMIT 1) FROM tbl_02 AS sub_313 LIMIT 1) < EXISTS (SELECT 1 FROM tbl_02 AS sub_315 WHERE sub_315.col_2)) < (SELECT (SELECT inn_324.col_3 FROM tbl_04 AS inn_324 LIMIT 1) FROM tbl_03 AS sub_317 LIMIT 1)))
LIMIT 47;

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT,
    ref_tbl_06_id INT,
    FOREIGN KEY (ref_tbl_06_id) REFERENCES tbl_06(id)
);

ALTER TABLE tbl_06 DROP COLUMN col_2;

CREATE TABLE log_batches (
    batch_id INT PRIMARY KEY,
    source_service VARCHAR(64) NOT NULL,
    tag_ids INT[] NOT NULL
);

SELECT batch_id, source_service
FROM log_batches
WHERE tag_ids @> ARRAY[101, 102]
ORDER BY batch_id;

DELETE FROM tbl_05 WHERE id = 50245;

INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (400491, TRUE, 9223372036854775807, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -9223372036854775808);

INSERT INTO tbl_07 (id, col_1, col_2, col_3) VALUES (526893, 2147483647, '', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

BEGIN;
INSERT INTO tbl_11 (id, col_1, col_2, ref_tbl_06_id) VALUES (758584, 42, 2147483647, 1);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (613699, FALSE, 100, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 1);
COMMIT;

INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_05_id) VALUES (196124, '{}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 100);

SELECT
    EXISTS (SELECT 1 FROM tbl_11 AS sub_326 WHERE sub_326.id) AS c_327,
    (SELECT (sub_328.col_2 IN (SELECT sub_329.ref_tbl_05_id FROM tbl_10 AS sub_329)) FROM tbl_10 AS sub_328 LIMIT 1) AS c_338,
    ((TRUE <= (CASE WHEN (((SELECT (ABS(((sub_339.col_2 < LENGTH((NOT (sub_339.col_1)))) + LENGTH(((FALSE::INT::TEXT) AND (sub_339.ref_tbl_01_id::TEXT::VARCHAR(32)))))) * sub_339.id) FROM tbl_05 AS sub_339 LIMIT 1)::TEXT) + LENGTH('')) THEN (t_ref_325.col_2 AND t_ref_325.col_3) ELSE (t_ref_325.col_2 IN (SELECT sub_340.id FROM tbl_11 AS sub_340)) END))::BOOLEAN) AS c_349
FROM tbl_03 AS t_ref_325 LEFT JOIN tbl_11 AS j_ref_350 ON t_ref_325.id = j_ref_350.id
WHERE ((((t_ref_325.col_3::TEXT::VARCHAR(32)) > ((t_ref_325.col_4::TEXT)::BOOLEAN)) + t_ref_325.col_2) + (SELECT (SELECT inn_371.col_3 FROM tbl_08 AS inn_371 LIMIT 1) FROM tbl_11 AS sub_351 LIMIT 1))
LIMIT 21;

UPDATE tbl_09 SET col_1 = 3.14159265 WHERE id > 17;

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64)
);

UPDATE tbl_07 SET col_2 = 'alpha_beta_gamma' WHERE id > 67;

INSERT INTO tbl_08 (id, col_2, col_3) VALUES (675134, '', 3.14159265);

INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_6) VALUES (868822, -1, 2147483647, 3.14159265, 'chaos');

SELECT
    (SELECT (SELECT (SELECT FALSE FROM tbl_02 AS inn_377 LIMIT 1) FROM tbl_05 AS sub_374 LIMIT 1) FROM tbl_05 AS sub_373 LIMIT 1) AS c_378,
    ((NOT (((((CASE WHEN (t_ref_372.id >= (SELECT (SELECT NULL FROM tbl_09 AS inn_382 LIMIT 1) FROM tbl_08 AS sub_379 LIMIT 1)) THEN (((EXISTS (SELECT 1 FROM tbl_08 AS sub_383 WHERE sub_383.col_3) / NULLIF((((SELECT (SELECT inn_385.col_1 FROM tbl_12 AS inn_385 LIMIT 1) FROM tbl_10 AS sub_384 LIMIT 1) * ABS(LOWER(t_ref_372.col_1))) + t_ref_372.col_4), 0))::BOOLEAN) <> COALESCE(((NULL IN (SELECT sub_386.col_2 FROM tbl_10 AS sub_386)) - (t_ref_372.id::NUMERIC(10,2))), -1.5)) ELSE t_ref_372.id END)::INT) + t_ref_372.col_4) BETWEEN 100 AND 9223372036854775807)))::BOOLEAN) AS c_387
FROM tbl_06 AS t_ref_372
WHERE ABS(EXISTS (SELECT 1 FROM log_batches AS sub_388 WHERE 2147483647))
LIMIT 47;

INSERT INTO log_batches (id, payload) VALUES (524536, 'alpha_beta_gamma');

BEGIN;
INSERT INTO tbl_08 (id, col_2, col_3) VALUES (512887, 'alpha_beta_gamma', 0.0000000001);
INSERT INTO tbl_09 (id, col_1, col_2, ref_tbl_08_id) VALUES (234126, 0.0000000001, TRUE, -2147483648);
INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_6) VALUES (764756, 0, -9223372036854775808, 1e10, 'escaped''quote');
COMMIT;

INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_6) VALUES (216763, 1, -9223372036854775808, -1.5, 'escaped''quote');

SELECT
    (LENGTH((UPPER((LENGTH((LOWER(UPPER(LOWER('chaos'))) + (t_ref_397.col_1 / NULLIF(t_ref_397.col_4, 0)))) + EXISTS (SELECT 1 FROM tbl_02 AS sub_398 WHERE sub_398.id))) BETWEEN -1 AND 100)) * (UPPER(0.0) * UPPER((SELECT TRUE FROM tbl_12 AS sub_402 LIMIT 1)))) AS c_403,
    (((SELECT (SELECT TRUE FROM tbl_08 AS inn_410 LIMIT 1) FROM tbl_07 AS sub_404 LIMIT 1) - (TRUE::TEXT)) IS NOT NULL) AS c_411
FROM tbl_06 AS t_ref_397
WHERE t_ref_397.col_4
LIMIT 24;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (768135, TRUE, 100, 'escaped''quote', 9223372036854775807);

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_6) VALUES (915025, -2147483648, 9223372036854775807, -1e-5, 'ref_val');
INSERT INTO tbl_12 (id, col_1, col_2) VALUES (599981, 42, 'chaos');
COMMIT;

INSERT INTO tbl_06 (id, col_1, col_4) VALUES (848165, -9223372036854775808, '{}');

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 JSONB
);

INSERT INTO tbl_06 (id, col_1, col_4) VALUES (170071, 2147483647, '[]');

INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_6) VALUES (964771, -9223372036854775808, -2147483648, 1e10, '__fuzz_marker__');

INSERT INTO tbl_12 (id, col_1, col_2) VALUES (779337, 2147483647, 'chaos');

SELECT
    t_ref_412.col_4 AS c_413,
    FALSE AS c_414,
    1 AS c_415
FROM tbl_06 AS t_ref_412
WHERE UPPER((CASE WHEN (FALSE - ((NOT (LENGTH((t_ref_412.col_4 <= (0 IN (SELECT sub_416.col_1 FROM tbl_05 AS sub_416)))))) * (LENGTH(1e10) > NULL))) THEN t_ref_412.col_1 ELSE (CASE WHEN t_ref_412.col_4 THEN (SELECT (SELECT inn_418.col_1 FROM tbl_06 AS inn_418 LIMIT 1) FROM tbl_05 AS sub_417 LIMIT 1) ELSE -9223372036854775808 END) END))
LIMIT 44;

SELECT
    EXISTS (SELECT 1 FROM tbl_10 AS sub_420 WHERE sub_420.col_1) AS c_425
FROM log_batches AS t_ref_419 LEFT JOIN tbl_02 AS j_ref_426 ON t_ref_419.id = j_ref_426.id
WHERE (t_ref_419.payload::INT::TEXT)
LIMIT 42;

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2),
    col_4 BOOLEAN,
    ref_tbl_13_id INT,
    FOREIGN KEY (ref_tbl_13_id) REFERENCES tbl_13(id)
);

CREATE INDEX idx_tbl_06_col_4_6 ON tbl_06 (col_4);

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (207494, FALSE, -9223372036854775808, '[]', 100);
INSERT INTO tbl_08 (id, col_2, col_3) VALUES (846600, 'escaped''quote', -1.5);
COMMIT;

UPDATE tbl_07 SET col_2 = 'alpha_beta_gamma' WHERE id > 59;

INSERT INTO tbl_10 (id, col_1, col_2, ref_tbl_05_id) VALUES (201408, '[]', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -9223372036854775808);